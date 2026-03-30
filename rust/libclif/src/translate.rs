//! CIR → Cranelift IR translator.
//!
//! Takes a parsed CirModule and produces compiled native code via Cranelift.

use crate::cir::*;
use cranelift_codegen::ir::types as cl_types;
use cranelift_codegen::ir::{self, AbiParam, InstBuilder, Value};
use cranelift_codegen::isa::{CallConv, TargetIsa};
use cranelift_codegen::settings;
use cranelift_codegen::Context;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_module::{default_libcall_names, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;

/// Translate a CIR module to a native object file.
pub fn translate_module(
    module: &CirModule,
    isa: std::sync::Arc<dyn TargetIsa>,
) -> Result<Vec<u8>, String> {
    let obj_builder = ObjectBuilder::new(isa, "cot_module", default_libcall_names())
        .map_err(|e| format!("ObjectBuilder: {e}"))?;
    let mut obj_module = ObjectModule::new(obj_builder);

    let mut func_ctx = FunctionBuilderContext::new();

    // Module-level function declaration table.
    // Defined functions are declared in the first pass with their true signatures.
    // External callees are declared lazily at call sites with types from the CIR instruction.
    let mut module_func_ids: HashMap<String, cranelift_module::FuncId> = HashMap::new();

    // First pass: declare all defined functions so callers can reference them
    let mut func_ids_vec: Vec<cranelift_module::FuncId> = Vec::new();
    let mut func_names_vec: Vec<String> = Vec::new();
    for cir_func in &module.functions {
        let raw_name = module.strings.get(cir_func.name_offset);
        let name = if raw_name == "main" { "_cot_main".to_string() } else { raw_name.to_string() };
        // Skip duplicate definitions (e.g., async function wrappers with same name)
        let func_id = if let Some(&existing_id) = module_func_ids.get(&name) {
            existing_id
        } else {
            let sig = build_signature(cir_func, &module.strings);
            let fid = obj_module
                .declare_function(&name, Linkage::Export, &sig)
                .map_err(|e| format!("declare {name}: {e}"))?;
            module_func_ids.insert(name.clone(), fid);
            fid
        };
        func_ids_vec.push(func_id);
        func_names_vec.push(name);
    }

    let mut defined_func_ids: std::collections::HashSet<cranelift_module::FuncId> = std::collections::HashSet::new();
    for (fi, cir_func) in module.functions.iter().enumerate() {
        let name = &func_names_vec[fi];
        let func_id = func_ids_vec[fi];

        // Skip duplicate function definitions (same FuncId already defined)
        if !defined_func_ids.insert(func_id) {
            continue;
        }

        // Build Cranelift signature from CIR
        let sig = build_signature(cir_func, &module.strings);

        // Build function body
        let mut func =
            ir::Function::with_name_signature(ir::UserFuncName::user(0, func_id.as_u32()), sig);

        translate_function(cir_func, module, &mut func, &mut func_ctx, &mut obj_module, &mut module_func_ids)?;

        // Compile
        let mut ctx = Context::for_function(func);

        // Debug: dump CLIF IR if CIR_TRACE is set
        if std::env::var("CIR_TRACE").is_ok() {
            eprintln!("=== CLIF IR for '{name}' ===");
            eprintln!("{}", ctx.func.display());
        }

        // Run verifier for better error messages
        if let Err(errors) = cranelift_codegen::verify_function(&ctx.func, obj_module.isa()) {
            eprintln!("=== Verifier errors for '{name}' ===");
            eprintln!("{errors}");
            return Err(format!("verify {name}: {errors}"));
        }

        if let Err(e) = obj_module.define_function(func_id, &mut ctx) {
            eprintln!("=== CLIF IR for failed function '{name}' ===");
            eprintln!("{}", ctx.func.display());
            eprintln!("=== Error detail ===");
            eprintln!("{e:#}");
            return Err(format!("compile {name}: {e:#}"));
        }
    }

    let product = obj_module.finish();
    product.emit().map_err(|e| format!("emit: {e}"))
}

fn build_signature(cir_func: &CirFunction, _strings: &StringHeap) -> ir::Signature {
    let mut sig = ir::Signature::new(CallConv::SystemV);

    for &pt in &cir_func.param_types {
        sig.params.push(AbiParam::new(cir_type_to_clif(pt)));
    }
    for &rt in &cir_func.return_types {
        sig.returns.push(AbiParam::new(cir_type_to_clif(rt)));
    }

    sig
}

fn cir_type_to_clif(type_idx: u32) -> ir::types::Type {
    match type_idx {
        TYPE_BOOL => cl_types::I8,
        TYPE_I8 => cl_types::I8,
        TYPE_I16 => cl_types::I16,
        TYPE_I32 => cl_types::I32,
        TYPE_I64 => cl_types::I64,
        TYPE_U8 => cl_types::I8,
        TYPE_U16 => cl_types::I16,
        TYPE_U32 => cl_types::I32,
        TYPE_U64 => cl_types::I64,
        TYPE_F32 => cl_types::F32,
        TYPE_F64 => cl_types::F64,
        TYPE_VOID | TYPE_NORETURN => cl_types::INVALID,
        // Pointers, strings, slices are all I64 in Cot's model
        _ => cl_types::I64,
    }
}

fn translate_function(
    cir_func: &CirFunction,
    module: &CirModule,
    func: &mut ir::Function,
    func_ctx: &mut FunctionBuilderContext,
    obj_module: &mut ObjectModule,
    module_func_ids: &mut HashMap<String, cranelift_module::FuncId>,
) -> Result<(), String> {
    let mut builder = FunctionBuilder::new(func, func_ctx);

    // Create blocks
    let mut block_map: HashMap<u32, ir::Block> = HashMap::new();
    for cir_block in &cir_func.blocks {
        let block = builder.create_block();
        block_map.insert(cir_block.id, block);
    }

    // Value map: CIR result ID → Cranelift Value (same-block fast path)
    let mut value_map: HashMap<u32, Value> = HashMap::new();
    // Variable map: CIR result ID → Cranelift Variable (cross-block SSA)
    let mut var_map: HashMap<u32, Variable> = HashMap::new();
    let mut next_var: u32 = 0;

    // Per-function FuncRef cache (module FuncId → this function's FuncRef)
    let mut func_refs: HashMap<String, ir::FuncRef> = HashMap::new();

    // Pre-create stack slots from STACK_SLOT_DECL instructions
    let mut stack_slot_map: HashMap<u32, ir::StackSlot> = HashMap::new();
    // Pre-create global values from GLOBAL_VALUE_SYMBOL / GLOBAL_VALUE_IADD instructions
    let mut gv_map: HashMap<u32, ir::GlobalValue> = HashMap::new();
    for cir_block in &cir_func.blocks {
        for inst in &cir_block.instructions {
            if inst.opcode == OP_STACK_SLOT_DECL {
                let slot_index = inst.words[0];
                let size = inst.words[1];
                let _alignment = inst.words[2];
                let ss = builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    size,
                    0,
                ));
                stack_slot_map.insert(slot_index, ss);
            } else if inst.opcode == OP_GLOBAL_VALUE_SYMBOL {
                let gv_index = inst.words[0];
                let name_offset = inst.words[1];
                let offset_lo = inst.words[2] as i64;
                let offset_hi = inst.words[3] as i64;
                let colocated = inst.words[4] != 0;
                let offset = (offset_hi << 32) | (offset_lo & 0xFFFFFFFF);

                let sym_name = module.strings.get(name_offset);
                if sym_name == "__vmctx" {
                    // VM context — skip for now
                    continue;
                }

                // Declare data symbol in the module
                let data_id = obj_module
                    .declare_data(sym_name, if colocated { Linkage::Local } else { Linkage::Import }, false, false)
                    .unwrap_or_else(|_| {
                        // Already declared — get existing
                        obj_module.declare_data(sym_name, Linkage::Import, false, false).unwrap()
                    });
                let gv = obj_module.declare_data_in_func(data_id, builder.func);

                // If there's an offset, wrap in iadd_imm
                let final_gv = if offset != 0 {
                    builder.func.create_global_value(ir::GlobalValueData::IAddImm {
                        base: gv,
                        offset: ir::immediates::Imm64::new(offset),
                        global_type: cl_types::I64,
                    })
                } else {
                    gv
                };
                gv_map.insert(gv_index, final_gv);
            } else if inst.opcode == OP_GLOBAL_VALUE_IADD {
                let gv_index = inst.words[0];
                let base_gv_index = inst.words[1];
                let offset_lo = inst.words[2] as i64;
                let offset_hi = inst.words[3] as i64;
                let offset = (offset_hi << 32) | (offset_lo & 0xFFFFFFFF);

                if let Some(&base_gv) = gv_map.get(&base_gv_index) {
                    let gv = builder.func.create_global_value(ir::GlobalValueData::IAddImm {
                        base: base_gv,
                        offset: ir::immediates::Imm64::new(offset),
                        global_type: cl_types::I64,
                    });
                    gv_map.insert(gv_index, gv);
                }
            }
        }
    }

    for cir_block in &cir_func.blocks {
        let block = block_map[&cir_block.id];
        builder.switch_to_block(block);

        // Entry block gets function params
        if cir_block.kind == 0x05 {
            builder.append_block_params_for_function_params(block);
        } else {
            // Non-entry blocks: add block params for phi values
            // Count OP_ARG instructions in this block to determine param count
            for inst in &cir_block.instructions {
                if inst.opcode == OP_ARG {
                    let type_idx = inst.words[1];
                    let clif_type = cir_type_to_clif(type_idx);
                    builder.append_block_param(block, clif_type);
                }
            }
        }

        for inst in &cir_block.instructions {
            translate_instruction(
                inst,
                block,
                &mut builder,
                &mut value_map,
                &mut var_map,
                &mut next_var,
                &block_map,
                &stack_slot_map,
                &gv_map,
                module,
                obj_module,
                module_func_ids,
                &mut func_refs,
            )?;
        }

        // Seal block
        builder.seal_block(block);
    }

    builder.finalize();
    Ok(())
}

/// Define a CIR value — stores in value_map AND declares a Cranelift Variable for cross-block use.
fn def_value(
    result_id: u32,
    val: Value,
    builder: &mut FunctionBuilder,
    value_map: &mut HashMap<u32, Value>,
    var_map: &mut HashMap<u32, Variable>,
    next_var: &mut u32,
) {
    value_map.insert(result_id, val);
    let var = if let Some(&v) = var_map.get(&result_id) {
        v
    } else {
        let v = Variable::from_u32(*next_var);
        *next_var += 1;
        let ty = builder.func.dfg.value_type(val);
        builder.declare_var(v, ty);
        var_map.insert(result_id, v);
        v
    };
    builder.def_var(var, val);
}

/// Use a CIR value — checks value_map first (fast), falls back to Cranelift's use_var (cross-block).
fn use_value(
    val_id: u32,
    builder: &mut FunctionBuilder,
    value_map: &HashMap<u32, Value>,
    var_map: &HashMap<u32, Variable>,
) -> Result<Value, String> {
    // Fast path: value in same block
    if let Some(&val) = value_map.get(&val_id) {
        return Ok(val);
    }
    // Cross-block: use Cranelift's SSA machinery
    if let Some(&var) = var_map.get(&val_id) {
        return Ok(builder.use_var(var));
    }
    Err(format!("unknown value v{val_id}"))
}

fn translate_instruction(
    inst: &CirInst,
    block: ir::Block,
    builder: &mut FunctionBuilder,
    value_map: &mut HashMap<u32, Value>,
    var_map: &mut HashMap<u32, Variable>,
    next_var: &mut u32,
    block_map: &HashMap<u32, ir::Block>,
    stack_slot_map: &HashMap<u32, ir::StackSlot>,
    gv_map: &HashMap<u32, ir::GlobalValue>,
    module: &CirModule,
    obj_module: &mut ObjectModule,
    module_func_ids: &mut HashMap<String, cranelift_module::FuncId>,
    func_refs: &mut HashMap<String, ir::FuncRef>,
) -> Result<(), String> {
    match inst.opcode {
        // -- Stack slot declarations (handled in pre-pass) --
        OP_STACK_SLOT_DECL => { /* already handled */ }

        // -- Arguments --
        OP_ARG => {
            let result_id = inst.words[0];
            let arg_index = inst.words[2] as usize;
            // Get the params for the current block (works for entry + non-entry blocks)
            let params = builder.block_params(block);
            if arg_index < params.len() {
                def_value(result_id, params[arg_index], builder, value_map, var_map, next_var);
            }
        }

        // -- Constants --
        OP_CONST_INT => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let clif_type = cir_type_to_clif(type_idx);
            let imm = if inst.words.len() >= 4 {
                // 64-bit immediate: combine two u32 words
                ((inst.words[3] as u64) << 32 | (inst.words[2] as u64)) as i64
            } else {
                // 32-bit immediate: zero-extend, not sign-extend
                // (u32 values like 3000000000 must not become negative)
                inst.words[2] as u64 as i64
            };
            let val = builder.ins().iconst(clif_type, imm);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        OP_CONST_BOOL => {
            let result_id = inst.words[0];
            let imm = inst.words[2] as i64;
            let val = builder.ins().iconst(cl_types::I8, imm);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        OP_CONST_FLOAT => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let bits = (inst.words[3] as u64) << 32 | (inst.words[2] as u64);
            let val = if type_idx == TYPE_F32 {
                builder.ins().f32const(f32::from_bits(bits as u32))
            } else {
                builder.ins().f64const(f64::from_bits(bits))
            };
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Function Address (for function pointers) --
        // Format: (result_id, type_idx, name_offset, param_count, [param_types...], return_count, [return_types...])
        OP_FUNC_ADDR => {
            let result_id = inst.words[0];
            let _type_idx = inst.words[1];
            let name_offset = inst.words[2];
            let func_name = module.strings.get(name_offset).to_string();

            // Read the callee's full signature from the CIR instruction
            let mut fpos = 3;
            let param_count = inst.words[fpos] as usize;
            fpos += 1;
            let mut fa_param_types = Vec::with_capacity(param_count);
            for _ in 0..param_count {
                fa_param_types.push(cir_type_to_clif(inst.words[fpos]));
                fpos += 1;
            }
            let return_count = inst.words[fpos] as usize;
            fpos += 1;
            let mut fa_return_types = Vec::with_capacity(return_count);
            for _ in 0..return_count {
                fa_return_types.push(cir_type_to_clif(inst.words[fpos]));
                fpos += 1;
            }

            // Get or declare the function in the module with its real signature
            let func_ref = if let Some(&fr) = func_refs.get(&func_name) {
                fr
            } else {
                let callee_id = if let Some(&fid) = module_func_ids.get(&func_name) {
                    fid
                } else {
                    let mut sig = ir::Signature::new(CallConv::SystemV);
                    for &pt in &fa_param_types {
                        sig.params.push(AbiParam::new(pt));
                    }
                    for &rt in &fa_return_types {
                        if rt != cl_types::INVALID {
                            sig.returns.push(AbiParam::new(rt));
                        }
                    }
                    let fid = obj_module
                        .declare_function(&func_name, Linkage::Import, &sig)
                        .map_err(|e| format!("declare func_addr {func_name}: {e}"))?;
                    module_func_ids.insert(func_name.clone(), fid);
                    fid
                };
                let callee_ref = obj_module.declare_func_in_func(callee_id, builder.func);
                func_refs.insert(func_name.clone(), callee_ref);
                callee_ref
            };

            let val = builder.ins().func_addr(cl_types::I64, func_ref);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Integer Arithmetic --
        OP_ADD => {
            let (result_id, _type_idx, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().iadd(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_SUB => {
            let (result_id, _type_idx, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().isub(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_MUL => {
            let (result_id, _type_idx, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().imul(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_DIV => {
            let (result_id, _type_idx, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().sdiv(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Bitwise --
        OP_AND => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().band(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_OR => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().bor(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_XOR => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().bxor(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_SHL => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().ishl(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_SHR => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().ushr(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_SAR => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().sshr(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Comparison --
        OP_EQ => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::Equal, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_NE => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::NotEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_LT => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::SignedLessThan, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_LE => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::SignedLessThanOrEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_GT => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThan, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_GE => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::SignedGreaterThanOrEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Float Arithmetic --
        OP_ADD_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fadd(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_SUB_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fsub(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_MUL_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fmul(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_DIV_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fdiv(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Float binary --
        OP_FMIN => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fmin(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_FMAX => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fmax(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_FCOPYSIGN => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fcopysign(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Copy --
        0x0091 => {
            // OP_COPY
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            if let Ok(val) = use_value(val_id, builder, value_map, var_map) {
                def_value(result_id, val, builder, value_map, var_map, next_var);
            }
        }

        // -- Bitcast --
        0x0049 => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let target_type = cir_type_to_clif(type_idx);
            if let Ok(val) = use_value(val_id, builder, value_map, var_map) {
                let val_ty = builder.func.dfg.value_type(val);
                let result = if val_ty == target_type {
                    val
                } else {
                    builder.ins().bitcast(target_type, ir::MemFlags::new(), val)
                };
                def_value(result_id, result, builder, value_map, var_map, next_var);
            }
        }

        // -- Return --
        OP_RET => {
            let sig_ret_count = builder.func.signature.returns.len();
            if sig_ret_count == 0 {
                // Void function — ignore the return value, just return
                builder.ins().return_(&[]);
            } else {
                // Read all return values from the CIR instruction
                let mut ret_vals = Vec::with_capacity(sig_ret_count);
                for i in 0..inst.words.len().min(sig_ret_count) {
                    let val_id = inst.words[i];
                    let mut val = use_value(val_id, builder, value_map, var_map)
                        .map_err(|e| format!("RET[{i}]: {e}"))?;
                    // Coerce return value type if needed
                    let ret_ty = builder.func.signature.returns[i].value_type;
                    let val_ty = builder.func.dfg.value_type(val);
                    if val_ty != ret_ty && val_ty.is_int() && ret_ty.is_int() {
                        if val_ty.bits() > ret_ty.bits() {
                            val = builder.ins().ireduce(ret_ty, val);
                        } else {
                            val = builder.ins().uextend(ret_ty, val);
                        }
                    }
                    ret_vals.push(val);
                }
                // Pad remaining return values if CIR has fewer than signature
                for i in ret_vals.len()..sig_ret_count {
                    let pad_ty = builder.func.signature.returns[i].value_type;
                    let pad = builder.ins().iconst(pad_ty, 0);
                    ret_vals.push(pad);
                }
                builder.ins().return_(&ret_vals);
            }
        }
        OP_RET_VOID => {
            builder.ins().return_(&[]);
        }

        // -- Static Call --
        // Format: (result_count, [result_id, result_type]..., name_offset, arg_count, [arg_type, arg_value_id]...)
        OP_STATIC_CALL => {
            let result_count = inst.words[0] as usize;
            let mut pos = 1;
            let mut result_ids = Vec::with_capacity(result_count);
            let mut result_types = Vec::with_capacity(result_count);
            for _ in 0..result_count {
                result_ids.push(inst.words[pos]);
                result_types.push(inst.words[pos + 1]);
                pos += 2;
            }
            let name_offset = inst.words[pos];
            pos += 1;
            let arg_count = inst.words[pos] as usize;
            pos += 1;

            // Read arg types and value IDs (paired)
            let mut arg_param_types = Vec::with_capacity(arg_count);
            let mut arg_value_ids = Vec::with_capacity(arg_count);
            for _ in 0..arg_count {
                arg_param_types.push(cir_type_to_clif(inst.words[pos]));
                arg_value_ids.push(inst.words[pos + 1]);
                pos += 2;
            }

            let func_name = module.strings.get(name_offset).to_string();

            // Get or declare the function reference
            let func_ref = if let Some(&fr) = func_refs.get(&func_name) {
                fr
            } else {
                let callee_id = if let Some(&fid) = module_func_ids.get(&func_name) {
                    fid
                } else {
                    // Build signature from the actual types encoded in the CIR instruction
                    let mut sig = ir::Signature::new(CallConv::SystemV);
                    for &pt in &arg_param_types {
                        sig.params.push(AbiParam::new(pt));
                    }
                    for &rt in &result_types {
                        let ret = cir_type_to_clif(rt);
                        if ret != cl_types::INVALID {
                            sig.returns.push(AbiParam::new(ret));
                        }
                    }
                    let sig_ref = builder.import_signature(sig);
                    let fid = obj_module
                        .declare_function(&func_name, Linkage::Import, &builder.func.stencil.dfg.signatures[sig_ref])
                        .map_err(|e| format!("declare callee {func_name}: {e}"))?;
                    module_func_ids.insert(func_name.clone(), fid);
                    fid
                };
                let callee_ref = obj_module.declare_func_in_func(callee_id, builder.func);
                func_refs.insert(func_name.clone(), callee_ref);
                callee_ref
            };

            // Collect args, adjusting each to the callee's expected param type from CIR
            let mut args = Vec::with_capacity(arg_count);
            for (i, &arg_id) in arg_value_ids.iter().enumerate() {
                let val = use_value(arg_id, builder, value_map, var_map)
                    .map_err(|e| format!("CALL {func_name}: {e}"))?;
                args.push(adjust_arg_type(builder, val, arg_param_types[i]));
            }

            let call = builder.ins().call(func_ref, &args);
            let cranelift_results: Vec<Value> = builder.inst_results(call).to_vec();
            for (i, &rid) in result_ids.iter().enumerate() {
                if i < cranelift_results.len() {
                    def_value(rid, cranelift_results[i], builder, value_map, var_map, next_var);
                }
            }
        }

        // -- Stack / Local Address --
        OP_LOCAL_ADDR => {
            let result_id = inst.words[0];
            let _type_idx = inst.words[1];
            let slot_index = inst.words[2];
            // Look up the pre-created stack slot, or create a fallback
            let ss = if let Some(&ss) = stack_slot_map.get(&slot_index) {
                ss
            } else {
                // Fallback: create an 8-byte slot (shouldn't happen if serializer is correct)
                builder.create_sized_stack_slot(ir::StackSlotData::new(
                    ir::StackSlotKind::ExplicitSlot,
                    8,
                    0,
                ))
            };
            let addr = builder.ins().stack_addr(cl_types::I64, ss, 0);
            def_value(result_id, addr, builder, value_map, var_map, next_var);
        }

        // -- Indirect Call --
        // Format: (result_count, [result_id, result_type]..., callee_value, arg_count, [arg_type, arg_value_id]...)
        OP_CALL => {
            let result_count = inst.words[0] as usize;
            let mut pos = 1;
            let mut result_ids = Vec::with_capacity(result_count);
            let mut result_types_vec = Vec::with_capacity(result_count);
            for _ in 0..result_count {
                result_ids.push(inst.words[pos]);
                result_types_vec.push(inst.words[pos + 1]);
                pos += 2;
            }
            let callee_id = inst.words[pos];
            pos += 1;
            let arg_count = inst.words[pos] as usize;
            pos += 1;

            // Read arg types and value IDs (paired)
            let mut arg_param_types = Vec::with_capacity(arg_count);
            let mut arg_value_ids = Vec::with_capacity(arg_count);
            for _ in 0..arg_count {
                arg_param_types.push(cir_type_to_clif(inst.words[pos]));
                arg_value_ids.push(inst.words[pos + 1]);
                pos += 2;
            }

            let callee = use_value(callee_id, builder, value_map, var_map)
                .map_err(|e| format!("CALL_INDIRECT callee: {e}"))?;

            // Build signature from actual types encoded in CIR
            let mut sig = ir::Signature::new(CallConv::SystemV);
            for &pt in &arg_param_types {
                sig.params.push(AbiParam::new(pt));
            }
            for &rt in &result_types_vec {
                let ret = cir_type_to_clif(rt);
                if ret != cl_types::INVALID {
                    sig.returns.push(AbiParam::new(ret));
                }
            }
            let sig_ref = builder.import_signature(sig);

            let mut args = Vec::with_capacity(arg_count);
            for (i, &arg_id) in arg_value_ids.iter().enumerate() {
                let val = use_value(arg_id, builder, value_map, var_map)
                    .map_err(|e| format!("CALL_INDIRECT arg: {e}"))?;
                args.push(adjust_arg_type(builder, val, arg_param_types[i]));
            }

            let call = builder.ins().call_indirect(sig_ref, callee, &args);
            let cranelift_results: Vec<Value> = builder.inst_results(call).to_vec();
            for (i, &rid) in result_ids.iter().enumerate() {
                if i < cranelift_results.len() {
                    def_value(rid, cranelift_results[i], builder, value_map, var_map, next_var);
                }
            }
        }

        // -- Global Value --
        OP_GLOBAL_VALUE => {
            let result_id = inst.words[0];
            let _type_idx = inst.words[1];
            let gv_index = inst.words[2];
            if let Some(&gv) = gv_map.get(&gv_index) {
                let addr = builder.ins().global_value(cl_types::I64, gv);
                def_value(result_id, addr, builder, value_map, var_map, next_var);
            } else {
                // Fallback: emit iconst 0 (will crash at runtime but won't fail compilation)
                let val = builder.ins().iconst(cl_types::I64, 0);
                def_value(result_id, val, builder, value_map, var_map, next_var);
            }
        }
        OP_GLOBAL_VALUE_SYMBOL => { /* handled in pre-pass */ }
        OP_GLOBAL_VALUE_IADD => { /* handled in pre-pass */ }

        // -- Memory --
        OP_LOAD => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let addr_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let addr = use_value(addr_id, builder, value_map, var_map)
                .map_err(|e| format!("LOAD: {e}"))?;
            let val = builder.ins().load(clif_type, ir::MemFlags::new(), addr, 0);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        OP_STORE => {
            let addr_id = inst.words[1];
            let val_id = inst.words[2];
            let addr = use_value(addr_id, builder, value_map, var_map)
                .map_err(|e| format!("STORE addr: {e}"))?;
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("STORE val: {e}"))?;
            builder.ins().store(ir::MemFlags::new(), val, addr, 0);
        }

        // -- Trap --
        OP_TRAP => {
            builder.ins().trap(ir::TrapCode::unwrap_user(1));
        }
        OP_COND_TRAP => {
            let cond_id = inst.words[0];
            let cond = use_value(cond_id, builder, value_map, var_map)
                .map_err(|e| format!("COND_TRAP: {e}"))?;
            builder.ins().trapnz(cond, ir::TrapCode::unwrap_user(1));
        }

        // -- Jump --
        OP_JUMP => {
            let target_block_id = inst.words[0];
            let arg_count = inst.words[1] as usize;
            let target = block_map
                .get(&target_block_id)
                .copied()
                .ok_or_else(|| format!("JUMP: unknown block {target_block_id}"))?;
            let mut args = Vec::with_capacity(arg_count);
            for i in 0..arg_count {
                let arg_id = inst.words[2 + i];
                let val = use_value(arg_id, builder, value_map, var_map)
                    .map_err(|e| format!("JUMP: {e}"))?;
                args.push(val);
            }
            builder.ins().jump(target, &args);
        }

        // -- Conditional Branch --
        OP_BRIF => {
            let cond_id = inst.words[0];
            let then_block_id = inst.words[1];
            let else_block_id = inst.words[2];
            let then_arg_count = inst.words[3] as usize;

            let cond = use_value(cond_id, builder, value_map, var_map)
                .map_err(|e| format!("BRIF cond: {e}"))?;
            let then_block = block_map
                .get(&then_block_id)
                .copied()
                .ok_or_else(|| format!("BRIF: unknown then block {then_block_id}"))?;
            let else_block = block_map
                .get(&else_block_id)
                .copied()
                .ok_or_else(|| format!("BRIF: unknown else block {else_block_id}"))?;

            // Collect then args
            let mut then_args = Vec::with_capacity(then_arg_count);
            for i in 0..then_arg_count {
                let arg_id = inst.words[4 + i];
                let val = use_value(arg_id, builder, value_map, var_map)
                    .map_err(|e| format!("BRIF then: {e}"))?;
                then_args.push(val);
            }

            let else_arg_count_idx = 4 + then_arg_count;
            let else_arg_count = inst.words[else_arg_count_idx] as usize;
            let mut else_args = Vec::with_capacity(else_arg_count);
            for i in 0..else_arg_count {
                let arg_id = inst.words[else_arg_count_idx + 1 + i];
                let val = use_value(arg_id, builder, value_map, var_map)
                    .map_err(|e| format!("BRIF else: {e}"))?;
                else_args.push(val);
            }

            builder.ins().brif(cond, then_block, &then_args, else_block, &else_args);
        }

        // -- Branch Table --
        OP_BR_TABLE => {
            let index_id = inst.words[0];
            let entry_count = inst.words[1] as usize;
            let index_val = use_value(index_id, builder, value_map, var_map)
                .map_err(|e| format!("BR_TABLE: {e}"))?;

            // Parse entries: first is default, rest are indexed
            let mut pos = 2;
            let mut jt_data = Vec::with_capacity(entry_count);
            for _ in 0..entry_count {
                let block_id = inst.words[pos];
                let arg_count = inst.words[pos + 1] as usize;
                pos += 2;
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    let arg_id = inst.words[pos];
                    let val = use_value(arg_id, builder, value_map, var_map)
                        .map_err(|e| format!("BR_TABLE arg: {e}"))?;
                    args.push(val);
                    pos += 1;
                }
                let block = block_map.get(&block_id).copied()
                    .ok_or_else(|| format!("BR_TABLE: unknown block {block_id}"))?;
                jt_data.push((block, args));
            }

            if !jt_data.is_empty() {
                // First entry is default, rest are indexed
                let (default_block, _default_args) = &jt_data[0];
                // Build BlockCall entries for the jump table
                let default_bc = ir::BlockCall::new(*default_block, &[], &mut builder.func.dfg.value_lists);
                let mut entries = Vec::with_capacity(jt_data.len() - 1);
                for (block, _args) in &jt_data[1..] {
                    let bc = ir::BlockCall::new(*block, &[], &mut builder.func.dfg.value_lists);
                    entries.push(bc);
                }
                let jt = builder.create_jump_table(ir::JumpTableData::new(default_bc, &entries));
                builder.ins().br_table(index_val, jt);
            }
        }

        // -- Conditional Select --
        OP_COND_SELECT => {
            let result_id = inst.words[0];
            let _type_idx = inst.words[1];
            let cond_id = inst.words[2];
            let true_id = inst.words[3];
            let false_id = inst.words[4];
            let cond = use_value(cond_id, builder, value_map, var_map)
                .map_err(|e| format!("SELECT cond: {e}"))?;
            let true_val = use_value(true_id, builder, value_map, var_map)
                .map_err(|e| format!("SELECT true: {e}"))?;
            let false_val = use_value(false_id, builder, value_map, var_map)
                .map_err(|e| format!("SELECT false: {e}"))?;
            let (true_val, false_val) = coerce_types(builder, true_val, false_val);
            let val = builder.ins().select(cond, true_val, false_val);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Unsigned comparisons --
        OP_ULT => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThan, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_ULE => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::UnsignedLessThanOrEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_UGT => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThan, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_UGE => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let (lhs, rhs) = coerce_types(builder, lhs, rhs);
            let val = builder.ins().icmp(ir::condcodes::IntCC::UnsignedGreaterThanOrEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Float comparisons --
        OP_EQ_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fcmp(ir::condcodes::FloatCC::Equal, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_NE_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fcmp(ir::condcodes::FloatCC::NotEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_LT_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fcmp(ir::condcodes::FloatCC::LessThan, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_LE_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fcmp(ir::condcodes::FloatCC::LessThanOrEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_GT_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fcmp(ir::condcodes::FloatCC::GreaterThan, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_GE_F => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().fcmp(ir::condcodes::FloatCC::GreaterThanOrEqual, lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Unsigned div/mod --
        OP_UDIV => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().udiv(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_UMOD => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().urem(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }
        OP_MOD => {
            let (result_id, _, lhs, rhs) = binary_operands(inst, builder, value_map, var_map)?;
            let val = builder.ins().srem(lhs, rhs);
            def_value(result_id, val, builder, value_map, var_map, next_var);
        }

        // -- Negation --
        OP_NEG => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("NEG: {e}"))?;
            let neg = builder.ins().ineg(val);
            def_value(result_id, neg, builder, value_map, var_map, next_var);
        }
        OP_FABS => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("FABS: {e}"))?;
            let r = builder.ins().fabs(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_SQRT => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("SQRT: {e}"))?;
            let r = builder.ins().sqrt(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_CEIL => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("CEIL: {e}"))?;
            let r = builder.ins().ceil(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_FLOOR => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("FLOOR: {e}"))?;
            let r = builder.ins().floor(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_TRUNC_F => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("TRUNC_F: {e}"))?;
            let r = builder.ins().trunc(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_NEAREST => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("NEAREST: {e}"))?;
            let r = builder.ins().nearest(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_NEG_F => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("NEG_F: {e}"))?;
            let neg = builder.ins().fneg(val);
            def_value(result_id, neg, builder, value_map, var_map, next_var);
        }
        // -- Type conversions --
        OP_UEXTEND => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("UEXTEND: {e}"))?;
            let val_ty = builder.func.dfg.value_type(val);
            let result = if val_ty == clif_type {
                val
            } else if val_ty.bits() < clif_type.bits() {
                builder.ins().uextend(clif_type, val)
            } else {
                builder.ins().ireduce(clif_type, val)
            };
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_SEXTEND => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("SEXTEND: {e}"))?;
            let val_ty = builder.func.dfg.value_type(val);
            let result = if val_ty == clif_type {
                val
            } else if val_ty.bits() < clif_type.bits() {
                builder.ins().sextend(clif_type, val)
            } else {
                builder.ins().ireduce(clif_type, val)
            };
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_IREDUCE => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("IREDUCE: {e}"))?;
            let val_ty = builder.func.dfg.value_type(val);
            let result = if val_ty == clif_type {
                val // already the right type, no-op
            } else if val_ty.bits() > clif_type.bits() {
                builder.ins().ireduce(clif_type, val)
            } else {
                builder.ins().uextend(clif_type, val) // widening, not narrowing
            };
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_FCVT_FROM_SINT => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("FCVT_FROM_SINT: {e}"))?;
            let result = builder.ins().fcvt_from_sint(clif_type, val);
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_FCVT_TO_SINT_SAT => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("FCVT_TO_SINT_SAT: {e}"))?;
            let result = builder.ins().fcvt_to_sint(clif_type, val);
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_FCVT_FROM_UINT => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("FCVT_FROM_UINT: {e}"))?;
            let result = builder.ins().fcvt_from_uint(clif_type, val);
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_FCVT_TO_UINT => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("FCVT_TO_UINT: {e}"))?;
            let result = builder.ins().fcvt_to_uint(clif_type, val);
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_FPROMOTE => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("FPROMOTE: {e}"))?;
            let val_ty = builder.func.dfg.value_type(val);
            let result = if val_ty == clif_type {
                val // identity cast — no-op
            } else {
                builder.ins().fpromote(clif_type, val)
            };
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }
        OP_FDEMOTE => {
            let result_id = inst.words[0];
            let type_idx = inst.words[1];
            let val_id = inst.words[2];
            let clif_type = cir_type_to_clif(type_idx);
            let val = use_value(val_id, builder, value_map, var_map)
                .map_err(|e| format!("FDEMOTE: {e}"))?;
            let val_ty = builder.func.dfg.value_type(val);
            let result = if val_ty == clif_type {
                val // identity cast — no-op
            } else {
                builder.ins().fdemote(clif_type, val)
            };
            def_value(result_id, result, builder, value_map, var_map, next_var);
        }

        OP_NOT => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("NOT: {e}"))?;
            let r = builder.ins().bnot(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_POPCNT => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("POPCNT: {e}"))?;
            let r = builder.ins().popcnt(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_CLZ => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("CLZ: {e}"))?;
            let r = builder.ins().clz(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }
        OP_CTZ => {
            let result_id = inst.words[0];
            let val_id = inst.words[2];
            let val = use_value(val_id, builder, value_map, var_map).map_err(|e| format!("CTZ: {e}"))?;
            let r = builder.ins().ctz(val);
            def_value(result_id, r, builder, value_map, var_map, next_var);
        }

        _ => {
            // Unknown opcode — skip for now, will be filled in as needed
        }
    }

    Ok(())
}

fn binary_operands(
    inst: &CirInst,
    builder: &mut FunctionBuilder,
    value_map: &HashMap<u32, Value>,
    var_map: &HashMap<u32, Variable>,
) -> Result<(u32, u32, Value, Value), String> {
    let result_id = inst.words[0];
    let type_idx = inst.words[1];
    let lhs_id = inst.words[2];
    let rhs_id = inst.words[3];
    let lhs = use_value(lhs_id, builder, value_map, var_map)
        .map_err(|e| format!("op 0x{:04X}: lhs {e}", inst.opcode))?;
    let rhs = use_value(rhs_id, builder, value_map, var_map)
        .map_err(|e| format!("op 0x{:04X}: rhs {e}", inst.opcode))?;
    Ok((result_id, type_idx, lhs, rhs))
}

/// Ensure two values have the same type by inserting uextend/ireduce as needed.
/// The Zig CLIF IR is loosely typed — Cranelift requires strict type matching.
/// Adjust an argument value to match the callee's expected parameter type.
/// Mirrors cg_clif's adjust_arg_for_abi pattern: ireduce for wider→narrower,
/// uextend for narrower→wider, fpromote/fdemote for floats.
/// The expected type comes from the callee's actual CLIF signature (encoded in CIR).
fn adjust_arg_type(
    builder: &mut FunctionBuilder,
    val: Value,
    expected_ty: ir::types::Type,
) -> Value {
    let val_ty = builder.func.dfg.value_type(val);
    if val_ty == expected_ty {
        return val;
    }
    if val_ty.is_int() && expected_ty.is_int() {
        if val_ty.bits() > expected_ty.bits() {
            return builder.ins().ireduce(expected_ty, val);
        } else {
            return builder.ins().uextend(expected_ty, val);
        }
    }
    if val_ty.is_float() && expected_ty.is_float() {
        if val_ty == expected_ty { return val; }
        if val_ty.bits() < expected_ty.bits() {
            return builder.ins().fpromote(expected_ty, val);
        } else {
            return builder.ins().fdemote(expected_ty, val);
        }
    }
    val
}

fn coerce_types(
    builder: &mut FunctionBuilder,
    lhs: Value,
    rhs: Value,
) -> (Value, Value) {
    let lhs_ty = builder.func.dfg.value_type(lhs);
    let rhs_ty = builder.func.dfg.value_type(rhs);
    if lhs_ty == rhs_ty {
        return (lhs, rhs);
    }
    // Widen the narrower operand to match the wider one
    let (wider, narrower_is_lhs) = if lhs_ty.bits() > rhs_ty.bits() {
        (lhs_ty, false)
    } else {
        (rhs_ty, true)
    };
    if narrower_is_lhs {
        (builder.ins().uextend(wider, lhs), rhs)
    } else {
        (lhs, builder.ins().uextend(wider, rhs))
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use cranelift_codegen::settings::Configurable;

    fn get_native_isa() -> std::sync::Arc<dyn TargetIsa> {
        let mut flag_builder = settings::builder();
        flag_builder.set("opt_level", "speed").unwrap();
        flag_builder.set("is_pic", "true").unwrap();
        let flags = settings::Flags::new(flag_builder);
        cranelift_native::builder()
            .unwrap()
            .finish(flags)
            .unwrap()
    }

    #[test]
    fn test_translate_add_function() {
        let mut writer = CirWriter::new(10);
        let name_off = writer.intern_string("_test_add");
        writer.write_string_heap();

        writer.begin_func_defs();
        writer.begin_func(name_off, &[TYPE_I64, TYPE_I64], &[TYPE_I64], 1, 0x02);
        writer.begin_block(0, 0x05, &[], &[]);
        writer.emit(OP_ARG, &[0, TYPE_I64, 0]);
        writer.emit(OP_ARG, &[1, TYPE_I64, 1]);
        writer.emit(OP_ADD, &[2, TYPE_I64, 0, 1]);
        writer.emit(OP_RET, &[2]);
        writer.end_block();
        writer.end_func();

        let bytes = writer.finish();
        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();

        let isa = get_native_isa();
        let obj_bytes = translate_module(&module, isa).unwrap();

        // Should produce a valid object file
        assert!(obj_bytes.len() > 0);
        let is_macho = obj_bytes[..4] == [0xCF, 0xFA, 0xED, 0xFE];
        let is_elf = obj_bytes[..4] == [0x7F, 0x45, 0x4C, 0x46];
        assert!(is_macho || is_elf, "should be valid object file");
    }

    #[test]
    fn test_translate_with_brif() {
        // fn test_brif(x: i64) -> i64 { if (x > 0) return 1 else return 0 }
        let mut writer = CirWriter::new(10);
        let name = writer.intern_string("_test_brif");
        writer.write_string_heap();

        writer.begin_func_defs();
        writer.begin_func(name, &[TYPE_I64], &[TYPE_I64], 3, 0x02);

        // Block 0 (entry): arg, compare, brif
        writer.begin_block(0, 0x05, &[], &[]);
        writer.emit(OP_ARG, &[0, TYPE_I64, 0]); // v0 = arg 0
        writer.emit(OP_CONST_INT, &[1, TYPE_I64, 0]); // v1 = 0
        writer.emit(OP_GT, &[2, TYPE_I64, 0, 1]); // v2 = v0 > v1
        writer.emit(OP_BRIF, &[2, 1, 2, 0, 0]); // brif v2, block1, block2, 0 then args, 0 else args
        writer.end_block();

        // Block 1 (then): return 1
        writer.begin_block(1, 0x00, &[], &[]);
        writer.emit(OP_CONST_INT, &[3, TYPE_I64, 1]); // v3 = 1
        writer.emit(OP_RET, &[3]);
        writer.end_block();

        // Block 2 (else): return 0
        writer.begin_block(2, 0x00, &[], &[]);
        writer.emit(OP_CONST_INT, &[4, TYPE_I64, 0]); // v4 = 0
        writer.emit(OP_RET, &[4]);
        writer.end_block();

        writer.end_func();

        let bytes = writer.finish();
        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();

        let isa = get_native_isa();
        let obj_bytes = translate_module(&module, isa).unwrap();
        assert!(obj_bytes.len() > 0);
    }

    #[test]
    fn test_translate_with_jump() {
        // fn test_jump() -> i64 { jump to block1; block1: return 42 }
        let mut writer = CirWriter::new(10);
        let name = writer.intern_string("_test_jump");
        writer.write_string_heap();

        writer.begin_func_defs();
        writer.begin_func(name, &[], &[TYPE_I64], 2, 0x02);

        // Block 0 (entry): jump to block 1
        writer.begin_block(0, 0x05, &[], &[]);
        writer.emit(OP_JUMP, &[1, 0]); // jump block1, 0 args
        writer.end_block();

        // Block 1: return 42
        writer.begin_block(1, 0x00, &[], &[]);
        writer.emit(OP_CONST_INT, &[0, TYPE_I64, 42]);
        writer.emit(OP_RET, &[0]);
        writer.end_block();

        writer.end_func();

        let bytes = writer.finish();
        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();

        let isa = get_native_isa();
        let obj_bytes = translate_module(&module, isa).unwrap();
        assert!(obj_bytes.len() > 0);
    }

    #[test]
    fn test_translate_with_stack_slots() {
        // fn test_stack() -> i64 { var x = 42; return x }
        let mut writer = CirWriter::new(10);
        let name = writer.intern_string("_test_stack");
        writer.write_string_heap();

        writer.begin_func_defs();
        writer.begin_func(name, &[], &[TYPE_I64], 1, 0x02);

        writer.begin_block(0, 0x05, &[], &[]);
        // Declare stack slot 0: 8 bytes, 8 alignment
        writer.emit(OP_STACK_SLOT_DECL, &[0, 8, 8]);
        // v0 = 42
        writer.emit(OP_CONST_INT, &[0, TYPE_I64, 42]);
        // addr = stack_addr(slot 0)
        writer.emit(OP_LOCAL_ADDR, &[1, TYPE_I64, 0]);
        // store v0 to addr
        writer.emit(OP_STORE, &[TYPE_I64, 1, 0]);
        // load from addr
        writer.emit(OP_LOAD, &[2, TYPE_I64, 1]);
        // return loaded value
        writer.emit(OP_RET, &[2]);
        writer.end_block();

        writer.end_func();

        let bytes = writer.finish();
        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();

        let isa = get_native_isa();
        let obj_bytes = translate_module(&module, isa).unwrap();
        assert!(obj_bytes.len() > 0);
    }

    #[test]
    fn test_translate_with_call() {
        let mut writer = CirWriter::new(10);
        let add_name = writer.intern_string("_helper_add");
        let main_name = writer.intern_string("_test_main");
        writer.write_string_heap();

        writer.begin_func_defs();

        // fn _helper_add(a, b) -> i64 { return a + b }
        writer.begin_func(add_name, &[TYPE_I64, TYPE_I64], &[TYPE_I64], 1, 0x00);
        writer.begin_block(0, 0x05, &[], &[]);
        writer.emit(OP_ARG, &[0, TYPE_I64, 0]);
        writer.emit(OP_ARG, &[1, TYPE_I64, 1]);
        writer.emit(OP_ADD, &[2, TYPE_I64, 0, 1]);
        writer.emit(OP_RET, &[2]);
        writer.end_block();
        writer.end_func();

        // fn _test_main() -> i64 { return _helper_add(10, 32) }
        writer.begin_func(main_name, &[], &[TYPE_I64], 1, 0x02);
        writer.begin_block(0, 0x05, &[], &[]);
        writer.emit(OP_CONST_INT, &[0, TYPE_I64, 10]);
        writer.emit(OP_CONST_INT, &[1, TYPE_I64, 32]);
        writer.emit(OP_STATIC_CALL, &[1, 2, TYPE_I64, add_name, 2, 0, 1]);
        writer.emit(OP_RET, &[2]);
        writer.end_block();
        writer.end_func();

        let bytes = writer.finish();
        let mut reader = CirReader::new(&bytes);
        let module = reader.read_module().unwrap();

        let isa = get_native_isa();
        let obj_bytes = translate_module(&module, isa).unwrap();
        assert!(obj_bytes.len() > 0);
    }
}
