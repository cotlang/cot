//! CIR -> Cranelift IR translator.
//!
//! Takes a parsed CirModule and produces compiled native code via the
//! hand-ported Cranelift backend.
//!
//! This is a 1:1 port of rust/libclif/src/translate.rs.

const std = @import("std");
const Allocator = std.mem.Allocator;

// CIR reader types
const cir = @import("cir_read.zig");
const CirModule = cir.CirModule;
const CirFunction = cir.CirFunction;
const CirBlock = cir.CirBlock;
const CirInst = cir.CirInst;

// CLIF IR types
const clif = @import("clif_ir/mod.zig");
const Type = clif.Type;
const Value = clif.Value;
const Block = clif.Block;
const Inst = clif.Inst;
const StackSlot = clif.StackSlot;
const StackSlotData = clif.StackSlotData;
const StackSlotKind = clif.StackSlotKind;
const FuncRef = clif.FuncRef;
const SigRef = clif.SigRef;
const GlobalValue = clif.GlobalValue;
const GlobalValueData = clif.GlobalValueData;
const IntCC = clif.IntCC;
const FloatCC = clif.FloatCC;
const TrapCode = clif.TrapCode;
const Function = clif.Function;
const Signature = clif.Signature;
const AbiParam = clif.AbiParam;
const CallConv = clif.CallConv;
const ExternalName = clif.ExternalName;
const GvExternalName = @import("clif_ir/globalvalue.zig").ExternalName;
const ExtFuncData = clif.ExtFuncData;
const MemFlags = clif.MemFlags;
const JumpTable = clif.JumpTable;
const JumpTableData = clif.JumpTableData;
const BlockCall = clif.BlockCall;

// Frontend builder
const frontend = @import("frontend/frontend.zig");
const FunctionBuilder = frontend.FunctionBuilder;
const FunctionBuilderContext = frontend.FunctionBuilderContext;
const Variable = frontend.Variable;

// Compiler
const compile_mod = @import("compile.zig");
const CompiledCode = compile_mod.CompiledCode;
const TargetIsa = compile_mod.TargetIsa;
const ControlPlane = compile_mod.ControlPlane;

// Object module
const object_module = @import("object_module.zig");
const ObjectModule = object_module.ObjectModule;
const FuncId = object_module.FuncId;
const DataId = object_module.DataId;
const Linkage = object_module.Linkage;

// ============================================================================
// CIR Type Constants (must match cir.rs / cir_write.zig)
// ============================================================================

const TYPE_INVALID: u32 = 0;
const TYPE_BOOL: u32 = 1;
const TYPE_I8: u32 = 2;
const TYPE_I16: u32 = 3;
const TYPE_I32: u32 = 4;
const TYPE_I64: u32 = 5;
const TYPE_U8: u32 = 6;
const TYPE_U16: u32 = 7;
const TYPE_U32: u32 = 8;
const TYPE_U64: u32 = 9;
const TYPE_F32: u32 = 10;
const TYPE_F64: u32 = 11;
const TYPE_VOID: u32 = 12;
const TYPE_STRING: u32 = 17;
const TYPE_NORETURN: u32 = 22;

// ============================================================================
// CIR Opcode Constants (must match cir.rs / cir_write.zig)
// ============================================================================

// Constants
const OP_CONST_BOOL: u16 = 0x0000;
const OP_CONST_INT: u16 = 0x0001;
const OP_CONST_FLOAT: u16 = 0x0002;

// Integer arithmetic
const OP_ADD: u16 = 0x0010;
const OP_SUB: u16 = 0x0011;
const OP_MUL: u16 = 0x0012;
const OP_DIV: u16 = 0x0013;
const OP_UDIV: u16 = 0x0014;
const OP_MOD: u16 = 0x0015;
const OP_UMOD: u16 = 0x0016;
const OP_NEG: u16 = 0x0017;

// Bitwise
const OP_AND: u16 = 0x0020;
const OP_OR: u16 = 0x0021;
const OP_XOR: u16 = 0x0022;
const OP_SHL: u16 = 0x0023;
const OP_SHR: u16 = 0x0024;
const OP_SAR: u16 = 0x0025;
const OP_NOT: u16 = 0x0026;
const OP_POPCNT: u16 = 0x0028;
const OP_CLZ: u16 = 0x0029;
const OP_CTZ: u16 = 0x002A;

// Comparison
const OP_EQ: u16 = 0x0030;
const OP_NE: u16 = 0x0031;
const OP_LT: u16 = 0x0032;
const OP_LE: u16 = 0x0033;
const OP_GT: u16 = 0x0034;
const OP_GE: u16 = 0x0035;
const OP_ULT: u16 = 0x0036;
const OP_ULE: u16 = 0x0037;
const OP_UGT: u16 = 0x0038;
const OP_UGE: u16 = 0x0039;

// Type conversions
const OP_UEXTEND: u16 = 0x0040;
const OP_SEXTEND: u16 = 0x0041;
const OP_IREDUCE: u16 = 0x0042;
const OP_FCVT_FROM_SINT: u16 = 0x0043;
const OP_FCVT_TO_SINT_SAT: u16 = 0x0044;
const OP_FCVT_FROM_UINT: u16 = 0x0045;
const OP_FPROMOTE: u16 = 0x0046;
const OP_FDEMOTE: u16 = 0x0047;
const OP_FCVT_TO_UINT: u16 = 0x0048;

// Float arithmetic
const OP_ADD_F: u16 = 0x0050;
const OP_SUB_F: u16 = 0x0051;
const OP_MUL_F: u16 = 0x0052;
const OP_DIV_F: u16 = 0x0053;
const OP_NEG_F: u16 = 0x0054;
const OP_FABS: u16 = 0x0055;
const OP_SQRT: u16 = 0x0056;
const OP_CEIL: u16 = 0x0057;
const OP_FLOOR: u16 = 0x0058;
const OP_TRUNC_F: u16 = 0x0059;
const OP_NEAREST: u16 = 0x005A;
const OP_FMIN: u16 = 0x005B;
const OP_FMAX: u16 = 0x005C;
const OP_FCOPYSIGN: u16 = 0x005D;

// Float comparison
const OP_EQ_F: u16 = 0x0060;
const OP_NE_F: u16 = 0x0061;
const OP_LT_F: u16 = 0x0062;
const OP_LE_F: u16 = 0x0063;
const OP_GT_F: u16 = 0x0064;
const OP_GE_F: u16 = 0x0065;

// Memory
const OP_LOAD: u16 = 0x0070;
const OP_STORE: u16 = 0x0071;
const OP_LOCAL_ADDR: u16 = 0x0080;
const OP_GLOBAL_VALUE: u16 = 0x0081;

// SSA / control
const OP_COPY: u16 = 0x0091;
const OP_ARG: u16 = 0x0092;
const OP_STATIC_CALL: u16 = 0x0093;
const OP_CALL: u16 = 0x0094;
const OP_FUNC_ADDR: u16 = 0x0095;
const OP_RET: u16 = 0x0097;
const OP_RET_VOID: u16 = 0x0098;
const OP_COND_SELECT: u16 = 0x0099;

// Control flow
const OP_JUMP: u16 = 0x00A0;
const OP_BRIF: u16 = 0x00A1;
const OP_TRAP: u16 = 0x00A2;
const OP_COND_TRAP: u16 = 0x00A3;
const OP_BR_TABLE: u16 = 0x00A4;

// Stack / global value declarations
const OP_STACK_SLOT_DECL: u16 = 0x00B0;
const OP_GLOBAL_VALUE_SYMBOL: u16 = 0x00B1;
const OP_GLOBAL_VALUE_IADD: u16 = 0x00B2;

// ============================================================================
// Translation Error
// ============================================================================

pub const TranslateError = error{
    UnknownBlock,
    UnknownValue,
    CompileError,
    ObjectError,
    OutOfMemory,
    InvalidFuncRef,
    InvalidSignature,
    UsedBeforeDeclared,
    TypeMismatch,
    DefinedBeforeDeclared,
    Overflow,
};

// ============================================================================
// Public API
// ============================================================================

/// Translate a CIR module to a native object file.
///
/// Port of translate_module() in translate.rs.
pub fn translateModule(allocator: Allocator, module: *const CirModule, isa: TargetIsa) TranslateError![]u8 {
    var obj_module = ObjectModule.init(allocator);
    defer obj_module.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();

    // Module-level function declaration table.
    // Maps function name -> FuncId in the object module.
    var module_func_ids = std.StringHashMapUnmanaged(FuncId){};
    defer module_func_ids.deinit(allocator);

    // First pass: declare all defined functions so callers can reference them.
    var func_ids_vec = std.ArrayListUnmanaged(FuncId){};
    defer func_ids_vec.deinit(allocator);
    var func_names_vec = std.ArrayListUnmanaged([]const u8){};
    defer func_names_vec.deinit(allocator);

    for (module.functions) |*cir_func| {
        const raw_name = module.getString(cir_func.name_offset);
        const name = if (std.mem.eql(u8, raw_name, "main")) "_cot_main" else raw_name;

        // Skip duplicate definitions (e.g., async function wrappers with same name)
        const gop = try module_func_ids.getOrPut(allocator, name);
        const func_id = if (gop.found_existing) gop.value_ptr.* else blk: {
            const fid = try obj_module.declareFunction(name, .Export);
            gop.value_ptr.* = fid;
            break :blk fid;
        };
        try func_ids_vec.append(allocator, func_id);
        try func_names_vec.append(allocator, name);
    }

    // Track which FuncIds have already been defined (skip duplicates).
    var defined_func_ids = std.AutoHashMapUnmanaged(u32, void){};
    defer defined_func_ids.deinit(allocator);

    for (module.functions, 0..) |*cir_func, fi| {
        const name = func_names_vec.items[fi];
        const func_id = func_ids_vec.items[fi];

        // Skip duplicate function definitions (same FuncId already defined)
        const def_gop = try defined_func_ids.getOrPut(allocator, func_id.index);
        if (def_gop.found_existing) continue;

        // Build Cranelift signature from CIR
        const sig = buildSignature(allocator, cir_func) catch Signature.init(.system_v);

        // Build function body
        var func = Function.init(allocator);
        defer func.deinit();
        func.signature = sig;

        try translateFunction(
            allocator,
            cir_func,
            module,
            &func,
            &func_ctx,
            &obj_module,
            &module_func_ids,
        );

        // Debug: dump CLIF IR if CIR_TRACE is set
        if (std.posix.getenv("CIR_TRACE")) |_| {
            std.debug.print("=== CLIF IR for '{s}' ===\n", .{name});
        }

        // Compile
        var ctrl_plane = ControlPlane.init();
        var compiled = compile_mod.compile(allocator, &func, isa, &ctrl_plane) catch {
            std.debug.print("=== Compile error for '{s}' ===\n", .{name});
            return TranslateError.CompileError;
        };
        defer compiled.deinit();

        obj_module.defineFunction(func_id, &compiled) catch {
            std.debug.print("=== Define error for '{s}' ===\n", .{name});
            return TranslateError.ObjectError;
        };

        // Note: sig ownership is transferred to func.signature, and func.deinit() frees it.
    }

    // Finish and emit the object file bytes.
    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);
    obj_module.finish(output.writer(allocator)) catch return TranslateError.ObjectError;

    return try output.toOwnedSlice(allocator);
}

// ============================================================================
// Signature Building
// ============================================================================

fn buildSignature(allocator: Allocator, cir_func: *const CirFunction) !Signature {
    var sig = Signature.init(.system_v);

    for (cir_func.param_types) |pt| {
        try sig.params.append(allocator, AbiParam.init(cirTypeToClif(pt)));
    }
    for (cir_func.return_types) |rt| {
        try sig.returns.append(allocator, AbiParam.init(cirTypeToClif(rt)));
    }

    return sig;
}

fn cirTypeToClif(type_idx: u32) Type {
    return switch (type_idx) {
        TYPE_BOOL => Type.I8,
        TYPE_I8 => Type.I8,
        TYPE_I16 => Type.I16,
        TYPE_I32 => Type.I32,
        TYPE_I64 => Type.I64,
        TYPE_U8 => Type.I8,
        TYPE_U16 => Type.I16,
        TYPE_U32 => Type.I32,
        TYPE_U64 => Type.I64,
        TYPE_F32 => Type.F32,
        TYPE_F64 => Type.F64,
        TYPE_VOID, TYPE_NORETURN => Type.INVALID,
        // Pointers, strings, slices are all I64 in Cot's model
        else => Type.I64,
    };
}

// ============================================================================
// Function Translation
// ============================================================================

/// Translation context passed through instruction translation.
const TranslateCtx = struct {
    allocator: Allocator,
    builder: *FunctionBuilder,
    value_map: *std.AutoHashMapUnmanaged(u32, Value),
    var_map: *std.AutoHashMapUnmanaged(u32, Variable),
    next_var: *u32,
    block_map: *std.AutoHashMapUnmanaged(u32, Block),
    stack_slot_map: *std.AutoHashMapUnmanaged(u32, StackSlot),
    gv_map: *std.AutoHashMapUnmanaged(u32, GlobalValue),
    module: *const CirModule,
    obj_module: *ObjectModule,
    module_func_ids: *std.StringHashMapUnmanaged(FuncId),
    func_refs: *std.StringHashMapUnmanaged(FuncRef),
};

fn translateFunction(
    allocator: Allocator,
    cir_func: *const CirFunction,
    module: *const CirModule,
    func: *Function,
    func_ctx: *FunctionBuilderContext,
    obj_module: *ObjectModule,
    module_func_ids: *std.StringHashMapUnmanaged(FuncId),
) TranslateError!void {
    var builder = FunctionBuilder.init(func, func_ctx);

    // Create blocks
    var block_map = std.AutoHashMapUnmanaged(u32, Block){};
    defer block_map.deinit(allocator);
    for (cir_func.blocks) |*cir_block| {
        const block = try builder.createBlock();
        try block_map.put(allocator, cir_block.id, block);
    }

    // Value map: CIR result ID -> Cranelift Value (same-block fast path)
    var value_map = std.AutoHashMapUnmanaged(u32, Value){};
    defer value_map.deinit(allocator);
    // Variable map: CIR result ID -> Cranelift Variable (cross-block SSA)
    var var_map = std.AutoHashMapUnmanaged(u32, Variable){};
    defer var_map.deinit(allocator);
    var next_var: u32 = 0;

    // Per-function FuncRef cache (function name -> this function's FuncRef)
    var func_refs = std.StringHashMapUnmanaged(FuncRef){};
    defer func_refs.deinit(allocator);

    // Pre-create stack slots from STACK_SLOT_DECL instructions
    var stack_slot_map = std.AutoHashMapUnmanaged(u32, StackSlot){};
    defer stack_slot_map.deinit(allocator);
    // Pre-create global values from GLOBAL_VALUE_SYMBOL / GLOBAL_VALUE_IADD instructions
    var gv_map = std.AutoHashMapUnmanaged(u32, GlobalValue){};
    defer gv_map.deinit(allocator);

    for (cir_func.blocks) |*cir_block| {
        for (cir_block.instructions) |*inst| {
            if (inst.opcode == OP_STACK_SLOT_DECL) {
                const slot_index = inst.words[0];
                const size = inst.words[1];
                const ss = try builder.createSizedStackSlot(StackSlotData.init(
                    .explicit_slot,
                    size,
                    0,
                ));
                try stack_slot_map.put(allocator, slot_index, ss);
            } else if (inst.opcode == OP_GLOBAL_VALUE_SYMBOL) {
                const gv_index = inst.words[0];
                const name_offset = inst.words[1];
                const offset_lo = @as(i64, @bitCast(@as(u64, inst.words[2])));
                const offset_hi = @as(i64, @bitCast(@as(u64, inst.words[3])));
                const colocated = inst.words[4] != 0;
                const offset = (offset_hi << 32) | (offset_lo & 0xFFFFFFFF);

                const sym_name = module.getString(name_offset);
                if (std.mem.eql(u8, sym_name, "__vmctx")) {
                    // VM context -- skip for now
                    continue;
                }

                // Declare data symbol in the module
                const data_id = obj_module.declareData(
                    sym_name,
                    if (colocated) Linkage.Local else Linkage.Import,
                    false,
                ) catch continue;

                // Create a global value with a symbol reference.
                // The symbol is identified by data_id index.
                const gv = try func.createGlobalValue(.{ .symbol = .{
                    .name = GvExternalName.initUser(0, data_id.index),
                    .offset = 0,
                    .colocated = colocated,
                    .tls = false,
                } });

                // If there's an offset, wrap in iadd_imm
                const final_gv = if (offset != 0) blk: {
                    break :blk try func.createGlobalValue(.{ .iadd_imm = .{
                        .base = gv,
                        .offset = offset,
                        .global_type = Type.I64,
                    } });
                } else gv;

                try gv_map.put(allocator, gv_index, final_gv);
            } else if (inst.opcode == OP_GLOBAL_VALUE_IADD) {
                const gv_index = inst.words[0];
                const base_gv_index = inst.words[1];
                const offset_lo = @as(i64, @bitCast(@as(u64, inst.words[2])));
                const offset_hi = @as(i64, @bitCast(@as(u64, inst.words[3])));
                const offset = (offset_hi << 32) | (offset_lo & 0xFFFFFFFF);

                if (gv_map.get(base_gv_index)) |base_gv| {
                    const gv = try func.createGlobalValue(.{ .iadd_imm = .{
                        .base = base_gv,
                        .offset = offset,
                        .global_type = Type.I64,
                    } });
                    try gv_map.put(allocator, gv_index, gv);
                }
            }
        }
    }

    var ctx = TranslateCtx{
        .allocator = allocator,
        .builder = &builder,
        .value_map = &value_map,
        .var_map = &var_map,
        .next_var = &next_var,
        .block_map = &block_map,
        .stack_slot_map = &stack_slot_map,
        .gv_map = &gv_map,
        .module = module,
        .obj_module = obj_module,
        .module_func_ids = module_func_ids,
        .func_refs = &func_refs,
    };

    for (cir_func.blocks) |*cir_block| {
        const block = block_map.get(cir_block.id) orelse return TranslateError.UnknownBlock;
        builder.switchToBlock(block);

        // Entry block gets function params
        if (cir_block.kind == 0x05) {
            try builder.appendBlockParamsForFunctionParams(block);
        } else {
            // Non-entry blocks: add block params for phi values
            // Count OP_ARG instructions in this block to determine param count
            for (cir_block.instructions) |*inst| {
                if (inst.opcode == OP_ARG) {
                    const type_idx = inst.words[1];
                    const clif_type = cirTypeToClif(type_idx);
                    _ = try builder.appendBlockParam(block, clif_type);
                }
            }
        }

        for (cir_block.instructions) |*inst| {
            try translateInstruction(inst, block, &ctx);
        }

        // Seal block
        try builder.sealBlock(block);
    }

    builder.finalize();
}

// ============================================================================
// Value Helpers
// ============================================================================

/// Define a CIR value -- stores in value_map AND declares a Cranelift Variable for cross-block use.
fn defValue(
    result_id: u32,
    val: Value,
    ctx: *TranslateCtx,
) TranslateError!void {
    try ctx.value_map.put(ctx.allocator, result_id, val);
    const variable = if (ctx.var_map.get(result_id)) |v| v else blk: {
        const ty = ctx.builder.func.dfg.valueType(val);
        const v = try ctx.builder.declareVar(ty);
        try ctx.var_map.put(ctx.allocator, result_id, v);
        break :blk v;
    };
    try ctx.builder.defVar(variable, val);
}

/// Use a CIR value -- checks value_map first (fast), falls back to Cranelift's use_var (cross-block).
fn useValue(
    val_id: u32,
    ctx: *TranslateCtx,
) TranslateError!Value {
    // Fast path: value in same block
    if (ctx.value_map.get(val_id)) |val| {
        return val;
    }
    // Cross-block: use Cranelift's SSA machinery
    if (ctx.var_map.get(val_id)) |variable| {
        return ctx.builder.useVar(variable);
    }
    return TranslateError.UnknownValue;
}

/// Ensure two values have the same type by inserting uextend/ireduce as needed.
fn coerceTypes(ctx: *TranslateCtx, lhs: Value, rhs: Value) TranslateError!struct { Value, Value } {
    const lhs_ty = ctx.builder.func.dfg.valueType(lhs);
    const rhs_ty = ctx.builder.func.dfg.valueType(rhs);
    if (lhs_ty.eql(rhs_ty)) {
        return .{ lhs, rhs };
    }
    // Widen the narrower operand to match the wider one
    if (lhs_ty.bits() > rhs_ty.bits()) {
        return .{ lhs, try ctx.builder.ins().uextend(lhs_ty, rhs) };
    } else {
        return .{ try ctx.builder.ins().uextend(rhs_ty, lhs), rhs };
    }
}

/// Adjust an argument value to match the callee's expected parameter type.
fn adjustArgType(ctx: *TranslateCtx, val: Value, expected_ty: Type) TranslateError!Value {
    const val_ty = ctx.builder.func.dfg.valueType(val);
    if (val_ty.eql(expected_ty)) {
        return val;
    }
    if (val_ty.isInt() and expected_ty.isInt()) {
        if (val_ty.bits() > expected_ty.bits()) {
            return ctx.builder.ins().ireduce(expected_ty, val);
        } else {
            return ctx.builder.ins().uextend(expected_ty, val);
        }
    }
    if (val_ty.isFloat() and expected_ty.isFloat()) {
        if (val_ty.eql(expected_ty)) return val;
        if (val_ty.bits() < expected_ty.bits()) {
            return ctx.builder.ins().fpromote(expected_ty, val);
        } else {
            return ctx.builder.ins().fdemote(expected_ty, val);
        }
    }
    return val;
}

/// Extract binary operands from CIR instruction.
fn binaryOperands(inst: *const CirInst, ctx: *TranslateCtx) TranslateError!struct { u32, u32, Value, Value } {
    const result_id = inst.words[0];
    const type_idx = inst.words[1];
    const lhs_id = inst.words[2];
    const rhs_id = inst.words[3];
    const lhs = try useValue(lhs_id, ctx);
    const rhs = try useValue(rhs_id, ctx);
    return .{ result_id, type_idx, lhs, rhs };
}

// ============================================================================
// Instruction Translation
// ============================================================================

fn translateInstruction(
    inst: *const CirInst,
    block: Block,
    ctx: *TranslateCtx,
) TranslateError!void {
    _ = block;
    const builder = ctx.builder;
    switch (inst.opcode) {
        // -- Stack slot declarations (handled in pre-pass) --
        OP_STACK_SLOT_DECL => {},

        // -- Arguments --
        OP_ARG => {
            const result_id = inst.words[0];
            const arg_index: usize = @intCast(inst.words[2]);
            // Get the params for the current block (works for entry + non-entry blocks)
            const cur_block = builder.currentBlock() orelse return;
            const params = builder.blockParams(cur_block);
            if (arg_index < params.len) {
                try defValue(result_id, params[arg_index], ctx);
            }
        },

        // -- Constants --
        OP_CONST_INT => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const clif_type = cirTypeToClif(type_idx);
            const imm: i64 = if (inst.words.len >= 4)
                // 64-bit immediate
                (@as(i64, @bitCast(@as(u64, inst.words[3]))) << 32) | @as(i64, @bitCast(@as(u64, inst.words[2])))
            else
                @as(i64, @as(i32, @bitCast(inst.words[2])));
            const val = try builder.ins().iconst(clif_type, imm);
            try defValue(result_id, val, ctx);
        },

        OP_CONST_BOOL => {
            const result_id = inst.words[0];
            const imm: i64 = @intCast(inst.words[2]);
            const val = try builder.ins().iconst(Type.I8, imm);
            try defValue(result_id, val, ctx);
        },

        OP_CONST_FLOAT => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const bits: u64 = (@as(u64, inst.words[3]) << 32) | @as(u64, inst.words[2]);
            const val = if (type_idx == TYPE_F32)
                try builder.ins().f32const(@bitCast(@as(u32, @truncate(bits))))
            else
                try builder.ins().f64const(@bitCast(bits));
            try defValue(result_id, val, ctx);
        },

        // -- Function Address (for function pointers) --
        OP_FUNC_ADDR => {
            const result_id = inst.words[0];
            const name_offset = inst.words[2];
            const func_name = ctx.module.getString(name_offset);

            // Read the callee's full signature from the CIR instruction
            var fpos: usize = 3;
            const param_count: usize = @intCast(inst.words[fpos]);
            fpos += 1;

            var fa_param_types = std.ArrayListUnmanaged(Type){};
            defer fa_param_types.deinit(ctx.allocator);
            for (0..param_count) |_| {
                try fa_param_types.append(ctx.allocator, cirTypeToClif(inst.words[fpos]));
                fpos += 1;
            }

            const return_count: usize = @intCast(inst.words[fpos]);
            fpos += 1;
            var fa_return_types = std.ArrayListUnmanaged(Type){};
            defer fa_return_types.deinit(ctx.allocator);
            for (0..return_count) |_| {
                try fa_return_types.append(ctx.allocator, cirTypeToClif(inst.words[fpos]));
                fpos += 1;
            }

            // Get or declare the function reference
            const func_ref = if (ctx.func_refs.get(func_name)) |fr| fr else blk: {
                const callee_id = if (ctx.module_func_ids.get(func_name)) |fid| fid else inner: {
                    var sig = Signature.init(.system_v);
                    for (fa_param_types.items) |pt| {
                        try sig.params.append(ctx.allocator, AbiParam.init(pt));
                    }
                    for (fa_return_types.items) |rt| {
                        if (!rt.eql(Type.INVALID)) {
                            try sig.returns.append(ctx.allocator, AbiParam.init(rt));
                        }
                    }
                    const fid = try ctx.obj_module.declareFunction(func_name, .Import);
                    try ctx.module_func_ids.put(ctx.allocator, func_name, fid);
                    break :inner fid;
                };

                // Import function into the current CLIF function
                // Build the real signature from CIR types
                var fa_sig = Signature.init(.system_v);
                for (fa_param_types.items) |pt| {
                    try fa_sig.params.append(ctx.allocator, AbiParam.init(pt));
                }
                for (fa_return_types.items) |rt| {
                    if (!rt.eql(Type.INVALID)) {
                        try fa_sig.returns.append(ctx.allocator, AbiParam.init(rt));
                    }
                }
                const sig_ref = try builder.importSignature(fa_sig);
                const fr = try builder.importFunction(ExtFuncData.init(
                    ExternalName.initUser(0, callee_id.index),
                    sig_ref,
                    true,
                ));
                try ctx.func_refs.put(ctx.allocator, func_name, fr);
                break :blk fr;
            };

            const val = try builder.ins().funcAddr(Type.I64, func_ref);
            try defValue(result_id, val, ctx);
        },

        // -- Integer Arithmetic --
        OP_ADD => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().iadd(coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_SUB => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().isub(coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_MUL => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().imul(coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_DIV => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().sdiv(coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },

        // -- Bitwise --
        OP_AND => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().band(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_OR => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().bor(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_XOR => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().bxor(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_SHL => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().ishl(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_SHR => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().ushr(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_SAR => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().sshr(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },

        // -- Comparison --
        OP_EQ => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.eq, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_NE => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.ne, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_LT => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.slt, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_LE => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.sle, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_GT => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.sgt, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_GE => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.sge, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },

        // -- Unsigned comparisons --
        OP_ULT => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.ult, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_ULE => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.ule, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_UGT => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.ugt, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },
        OP_UGE => {
            const ops = try binaryOperands(inst, ctx);
            const coerced = try coerceTypes(ctx, ops[2], ops[3]);
            const val = try builder.ins().icmp(.uge, coerced[0], coerced[1]);
            try defValue(ops[0], val, ctx);
        },

        // -- Float Arithmetic --
        OP_ADD_F => {
            const ops = try binaryOperands(inst, ctx);
            const ty = builder.func.dfg.valueType(ops[2]);
            const val = try builder.ins().fadd(ty, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_SUB_F => {
            const ops = try binaryOperands(inst, ctx);
            const ty = builder.func.dfg.valueType(ops[2]);
            const val = try builder.ins().fsub(ty, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_MUL_F => {
            const ops = try binaryOperands(inst, ctx);
            const ty = builder.func.dfg.valueType(ops[2]);
            const val = try builder.ins().fmul(ty, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_DIV_F => {
            const ops = try binaryOperands(inst, ctx);
            const ty = builder.func.dfg.valueType(ops[2]);
            const val = try builder.ins().fdiv(ty, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },

        // -- Float binary --
        OP_FMIN => {
            const ops = try binaryOperands(inst, ctx);
            const ty = builder.func.dfg.valueType(ops[2]);
            const val = try builder.ins().fmin(ty, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_FMAX => {
            const ops = try binaryOperands(inst, ctx);
            const ty = builder.func.dfg.valueType(ops[2]);
            const val = try builder.ins().fmax(ty, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_FCOPYSIGN => {
            const ops = try binaryOperands(inst, ctx);
            const ty = builder.func.dfg.valueType(ops[2]);
            const val = try builder.ins().fcopysign(ty, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },

        // -- Float comparisons --
        OP_EQ_F => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().fcmp(.eq, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_NE_F => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().fcmp(.ne, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_LT_F => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().fcmp(.lt, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_LE_F => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().fcmp(.le, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_GT_F => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().fcmp(.gt, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_GE_F => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().fcmp(.ge, ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },

        // -- Copy --
        OP_COPY => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            if (useValue(val_id, ctx)) |val| {
                try defValue(result_id, val, ctx);
            } else |_| {}
        },

        // -- Return --
        OP_RET => {
            const sig_ret_count = builder.func.signature.returns.items.len;
            if (sig_ret_count == 0) {
                // Void function -- ignore the return value, just return
                _ = try builder.ins().return_(&.{});
            } else {
                const val_id = inst.words[0];
                var val = try useValue(val_id, ctx);
                // Coerce return value type if needed
                const ret_ty = builder.func.signature.returns.items[0].value_type;
                const val_ty = builder.func.dfg.valueType(val);
                if (!val_ty.eql(ret_ty) and val_ty.isInt() and ret_ty.isInt()) {
                    if (val_ty.bits() > ret_ty.bits()) {
                        val = try builder.ins().ireduce(ret_ty, val);
                    } else {
                        val = try builder.ins().uextend(ret_ty, val);
                    }
                }
                // Pad return values to match signature (Zig CLIF may return fewer than declared)
                if (sig_ret_count == 1) {
                    _ = try builder.ins().return_(&.{val});
                } else {
                    var ret_vals = std.ArrayListUnmanaged(Value){};
                    defer ret_vals.deinit(ctx.allocator);
                    try ret_vals.append(ctx.allocator, val);
                    for (1..sig_ret_count) |i| {
                        const pad_ty = builder.func.signature.returns.items[i].value_type;
                        const pad = try builder.ins().iconst(pad_ty, 0);
                        try ret_vals.append(ctx.allocator, pad);
                    }
                    _ = try builder.ins().return_(ret_vals.items);
                }
            }
        },
        OP_RET_VOID => {
            _ = try builder.ins().return_(&.{});
        },

        // -- Static Call --
        OP_STATIC_CALL => {
            const result_count: usize = @intCast(inst.words[0]);
            var pos: usize = 1;

            var result_ids = std.ArrayListUnmanaged(u32){};
            defer result_ids.deinit(ctx.allocator);
            var result_types = std.ArrayListUnmanaged(u32){};
            defer result_types.deinit(ctx.allocator);
            for (0..result_count) |_| {
                try result_ids.append(ctx.allocator, inst.words[pos]);
                try result_types.append(ctx.allocator, inst.words[pos + 1]);
                pos += 2;
            }
            const name_offset = inst.words[pos];
            pos += 1;
            const arg_count: usize = @intCast(inst.words[pos]);
            pos += 1;

            // Read arg types and value IDs (paired)
            var arg_param_types = std.ArrayListUnmanaged(Type){};
            defer arg_param_types.deinit(ctx.allocator);
            var arg_value_ids = std.ArrayListUnmanaged(u32){};
            defer arg_value_ids.deinit(ctx.allocator);
            for (0..arg_count) |_| {
                try arg_param_types.append(ctx.allocator, cirTypeToClif(inst.words[pos]));
                try arg_value_ids.append(ctx.allocator, inst.words[pos + 1]);
                pos += 2;
            }

            const func_name = ctx.module.getString(name_offset);

            // Get or declare the function reference
            const func_ref = if (ctx.func_refs.get(func_name)) |fr| fr else blk: {
                const callee_id = if (ctx.module_func_ids.get(func_name)) |fid| fid else inner: {
                    // Build signature from the actual types encoded in the CIR instruction
                    var sig = Signature.init(.system_v);
                    for (arg_param_types.items) |pt| {
                        try sig.params.append(ctx.allocator, AbiParam.init(pt));
                    }
                    for (result_types.items) |rt| {
                        const ret = cirTypeToClif(rt);
                        if (!ret.eql(Type.INVALID)) {
                            try sig.returns.append(ctx.allocator, AbiParam.init(ret));
                        }
                    }
                    const fid = try ctx.obj_module.declareFunction(func_name, .Import);
                    try ctx.module_func_ids.put(ctx.allocator, func_name, fid);
                    break :inner fid;
                };

                // Import into current function
                var sig2 = Signature.init(.system_v);
                for (arg_param_types.items) |pt| {
                    try sig2.params.append(ctx.allocator, AbiParam.init(pt));
                }
                for (result_types.items) |rt| {
                    const ret = cirTypeToClif(rt);
                    if (!ret.eql(Type.INVALID)) {
                        try sig2.returns.append(ctx.allocator, AbiParam.init(ret));
                    }
                }
                const sig_ref = try builder.importSignature(sig2);
                const fr = try builder.importFunction(ExtFuncData.init(
                    ExternalName.initUser(0, callee_id.index),
                    sig_ref,
                    true,
                ));
                try ctx.func_refs.put(ctx.allocator, func_name, fr);
                break :blk fr;
            };

            // Collect args, adjusting each to the callee's expected param type from CIR
            var args = std.ArrayListUnmanaged(Value){};
            defer args.deinit(ctx.allocator);
            for (arg_value_ids.items, 0..) |arg_id, i| {
                const val = try useValue(arg_id, ctx);
                try args.append(ctx.allocator, try adjustArgType(ctx, val, arg_param_types.items[i]));
            }

            const call_result = try builder.ins().call(func_ref, args.items);
            const cranelift_results = call_result.results;
            for (result_ids.items, 0..) |rid, i| {
                if (i < cranelift_results.len) {
                    try defValue(rid, cranelift_results[i], ctx);
                }
            }
        },

        // -- Stack / Local Address --
        OP_LOCAL_ADDR => {
            const result_id = inst.words[0];
            const slot_index = inst.words[2];
            // Look up the pre-created stack slot, or create a fallback
            const ss = if (ctx.stack_slot_map.get(slot_index)) |s| s else blk: {
                // Fallback: create an 8-byte slot (shouldn't happen if serializer is correct)
                break :blk try builder.createSizedStackSlot(StackSlotData.init(
                    .explicit_slot,
                    8,
                    0,
                ));
            };
            const addr = try builder.ins().stackAddr(Type.I64, ss, 0);
            try defValue(result_id, addr, ctx);
        },

        // -- Indirect Call --
        OP_CALL => {
            const result_count: usize = @intCast(inst.words[0]);
            var pos: usize = 1;

            var result_ids = std.ArrayListUnmanaged(u32){};
            defer result_ids.deinit(ctx.allocator);
            var result_types_vec = std.ArrayListUnmanaged(u32){};
            defer result_types_vec.deinit(ctx.allocator);
            for (0..result_count) |_| {
                try result_ids.append(ctx.allocator, inst.words[pos]);
                try result_types_vec.append(ctx.allocator, inst.words[pos + 1]);
                pos += 2;
            }
            const callee_id_val = inst.words[pos];
            pos += 1;
            const arg_count: usize = @intCast(inst.words[pos]);
            pos += 1;

            // Read arg types and value IDs (paired)
            var arg_param_types = std.ArrayListUnmanaged(Type){};
            defer arg_param_types.deinit(ctx.allocator);
            var arg_value_ids = std.ArrayListUnmanaged(u32){};
            defer arg_value_ids.deinit(ctx.allocator);
            for (0..arg_count) |_| {
                try arg_param_types.append(ctx.allocator, cirTypeToClif(inst.words[pos]));
                try arg_value_ids.append(ctx.allocator, inst.words[pos + 1]);
                pos += 2;
            }

            const callee = try useValue(callee_id_val, ctx);

            // Build signature from actual types encoded in CIR
            // Note: sig ownership transfers to builder.importSignature, do not deinit here.
            var sig = Signature.init(.system_v);
            for (arg_param_types.items) |pt| {
                try sig.params.append(ctx.allocator, AbiParam.init(pt));
            }
            for (result_types_vec.items) |rt| {
                const ret = cirTypeToClif(rt);
                if (!ret.eql(Type.INVALID)) {
                    try sig.returns.append(ctx.allocator, AbiParam.init(ret));
                }
            }
            const sig_ref = try builder.importSignature(sig);

            var args = std.ArrayListUnmanaged(Value){};
            defer args.deinit(ctx.allocator);
            for (arg_value_ids.items, 0..) |arg_id, i| {
                const val = try useValue(arg_id, ctx);
                try args.append(ctx.allocator, try adjustArgType(ctx, val, arg_param_types.items[i]));
            }

            const call_result = try builder.ins().callIndirect(sig_ref, callee, args.items);
            const cranelift_results = call_result.results;
            for (result_ids.items, 0..) |rid, i| {
                if (i < cranelift_results.len) {
                    try defValue(rid, cranelift_results[i], ctx);
                }
            }
        },

        // -- Global Value --
        OP_GLOBAL_VALUE => {
            const result_id = inst.words[0];
            const gv_index = inst.words[2];
            if (ctx.gv_map.get(gv_index)) |gv| {
                const addr = try builder.ins().globalValue(Type.I64, gv);
                try defValue(result_id, addr, ctx);
            } else {
                // Fallback: emit iconst 0 (will crash at runtime but won't fail compilation)
                const val = try builder.ins().iconst(Type.I64, 0);
                try defValue(result_id, val, ctx);
            }
        },
        OP_GLOBAL_VALUE_SYMBOL => {}, // handled in pre-pass
        OP_GLOBAL_VALUE_IADD => {}, // handled in pre-pass

        // -- Memory --
        OP_LOAD => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const addr_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const addr = try useValue(addr_id, ctx);
            const val = try builder.ins().load(clif_type, MemFlags.DEFAULT, addr, 0);
            try defValue(result_id, val, ctx);
        },

        OP_STORE => {
            const addr_id = inst.words[1];
            const val_id = inst.words[2];
            const addr = try useValue(addr_id, ctx);
            const val = try useValue(val_id, ctx);
            _ = try builder.ins().store(MemFlags.DEFAULT, val, addr, 0);
        },

        // -- Trap --
        OP_TRAP => {
            _ = try builder.ins().trap(.user1);
        },
        OP_COND_TRAP => {
            const cond_id = inst.words[0];
            const cond = try useValue(cond_id, ctx);
            _ = try builder.ins().trapnz(cond, .user1);
        },

        // -- Jump --
        OP_JUMP => {
            const target_block_id = inst.words[0];
            const arg_count: usize = @intCast(inst.words[1]);
            const target = ctx.block_map.get(target_block_id) orelse return TranslateError.UnknownBlock;

            var args = std.ArrayListUnmanaged(Value){};
            defer args.deinit(ctx.allocator);
            for (0..arg_count) |i| {
                const arg_id = inst.words[2 + i];
                const val = try useValue(arg_id, ctx);
                try args.append(ctx.allocator, val);
            }
            _ = try builder.ins().jump(target, args.items);
        },

        // -- Conditional Branch --
        OP_BRIF => {
            const cond_id = inst.words[0];
            const then_block_id = inst.words[1];
            const else_block_id = inst.words[2];
            const then_arg_count: usize = @intCast(inst.words[3]);

            const cond = try useValue(cond_id, ctx);
            const then_block = ctx.block_map.get(then_block_id) orelse return TranslateError.UnknownBlock;
            const else_block = ctx.block_map.get(else_block_id) orelse return TranslateError.UnknownBlock;

            // Collect then args
            var then_args = std.ArrayListUnmanaged(Value){};
            defer then_args.deinit(ctx.allocator);
            for (0..then_arg_count) |i| {
                const arg_id = inst.words[4 + i];
                const val = try useValue(arg_id, ctx);
                try then_args.append(ctx.allocator, val);
            }

            const else_arg_count_idx = 4 + then_arg_count;
            const else_arg_count: usize = @intCast(inst.words[else_arg_count_idx]);
            var else_args = std.ArrayListUnmanaged(Value){};
            defer else_args.deinit(ctx.allocator);
            for (0..else_arg_count) |i| {
                const arg_id = inst.words[else_arg_count_idx + 1 + i];
                const val = try useValue(arg_id, ctx);
                try else_args.append(ctx.allocator, val);
            }

            _ = try builder.ins().brif(cond, then_block, then_args.items, else_block, else_args.items);
        },

        // -- Branch Table --
        OP_BR_TABLE => {
            const index_id = inst.words[0];
            const entry_count: usize = @intCast(inst.words[1]);
            const index_val = try useValue(index_id, ctx);

            // Parse entries: first is default, rest are indexed
            var wpos: usize = 2;
            const JtEntry = struct { blk: Block, args: []const Value };
            var jt_data = std.ArrayListUnmanaged(JtEntry){};
            defer {
                for (jt_data.items) |entry| {
                    ctx.allocator.free(entry.args);
                }
                jt_data.deinit(ctx.allocator);
            }

            for (0..entry_count) |_| {
                const block_id = inst.words[wpos];
                const arg_count: usize = @intCast(inst.words[wpos + 1]);
                wpos += 2;
                var entry_args = std.ArrayListUnmanaged(Value){};
                for (0..arg_count) |_| {
                    const arg_id = inst.words[wpos];
                    const val = try useValue(arg_id, ctx);
                    try entry_args.append(ctx.allocator, val);
                    wpos += 1;
                }
                const blk = ctx.block_map.get(block_id) orelse return TranslateError.UnknownBlock;
                try jt_data.append(ctx.allocator, .{
                    .blk = blk,
                    .args = try entry_args.toOwnedSlice(ctx.allocator),
                });
            }

            if (jt_data.items.len > 0) {
                // First entry is default, rest are indexed
                const default_block = jt_data.items[0].blk;
                var targets = std.ArrayListUnmanaged(Block){};
                defer targets.deinit(ctx.allocator);
                for (jt_data.items[1..]) |entry| {
                    try targets.append(ctx.allocator, entry.blk);
                }
                const jt = try builder.createJumpTable(default_block, targets.items);
                _ = try builder.ins().brTable(index_val, jt);
            }
        },

        // -- Conditional Select --
        OP_COND_SELECT => {
            const result_id = inst.words[0];
            const cond_id = inst.words[2];
            const true_id = inst.words[3];
            const false_id = inst.words[4];
            const cond = try useValue(cond_id, ctx);
            var true_val = try useValue(true_id, ctx);
            var false_val = try useValue(false_id, ctx);
            const coerced = try coerceTypes(ctx, true_val, false_val);
            true_val = coerced[0];
            false_val = coerced[1];
            const ty = builder.func.dfg.valueType(true_val);
            const val = try builder.ins().select(ty, cond, true_val, false_val);
            try defValue(result_id, val, ctx);
        },

        // -- Unsigned div/mod --
        OP_UDIV => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().udiv(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_UMOD => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().urem(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },
        OP_MOD => {
            const ops = try binaryOperands(inst, ctx);
            const val = try builder.ins().srem(ops[2], ops[3]);
            try defValue(ops[0], val, ctx);
        },

        // -- Negation --
        OP_NEG => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const neg = try builder.ins().ineg(val);
            try defValue(result_id, neg, ctx);
        },
        OP_NEG_F => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const ty = builder.func.dfg.valueType(val);
            const neg = try builder.ins().fneg(ty, val);
            try defValue(result_id, neg, ctx);
        },

        // -- Float unary --
        OP_FABS => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const ty = builder.func.dfg.valueType(val);
            const r = try builder.ins().fabs(ty, val);
            try defValue(result_id, r, ctx);
        },
        OP_SQRT => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const ty = builder.func.dfg.valueType(val);
            const r = try builder.ins().sqrt(ty, val);
            try defValue(result_id, r, ctx);
        },
        OP_CEIL => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const ty = builder.func.dfg.valueType(val);
            const r = try builder.ins().ceil(ty, val);
            try defValue(result_id, r, ctx);
        },
        OP_FLOOR => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const ty = builder.func.dfg.valueType(val);
            const r = try builder.ins().floor(ty, val);
            try defValue(result_id, r, ctx);
        },
        OP_TRUNC_F => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const ty = builder.func.dfg.valueType(val);
            const r = try builder.ins().ftrunc(ty, val);
            try defValue(result_id, r, ctx);
        },
        OP_NEAREST => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const ty = builder.func.dfg.valueType(val);
            const r = try builder.ins().nearest(ty, val);
            try defValue(result_id, r, ctx);
        },

        // -- Type conversions --
        OP_UEXTEND => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const val_ty = builder.func.dfg.valueType(val);
            const result = if (val_ty.eql(clif_type))
                val
            else if (val_ty.bits() < clif_type.bits())
                try builder.ins().uextend(clif_type, val)
            else
                try builder.ins().ireduce(clif_type, val);
            try defValue(result_id, result, ctx);
        },
        OP_SEXTEND => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const val_ty = builder.func.dfg.valueType(val);
            const result = if (val_ty.eql(clif_type))
                val
            else if (val_ty.bits() < clif_type.bits())
                try builder.ins().sextend(clif_type, val)
            else
                try builder.ins().ireduce(clif_type, val);
            try defValue(result_id, result, ctx);
        },
        OP_IREDUCE => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const val_ty = builder.func.dfg.valueType(val);
            const result = if (val_ty.eql(clif_type))
                val // already the right type, no-op
            else if (val_ty.bits() > clif_type.bits())
                try builder.ins().ireduce(clif_type, val)
            else
                try builder.ins().uextend(clif_type, val); // widening, not narrowing
            try defValue(result_id, result, ctx);
        },
        OP_FCVT_FROM_SINT => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const result = try builder.ins().fcvtFromSint(clif_type, val);
            try defValue(result_id, result, ctx);
        },
        OP_FCVT_TO_SINT_SAT => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const result = try builder.ins().fcvtToSint(clif_type, val);
            try defValue(result_id, result, ctx);
        },
        OP_FCVT_FROM_UINT => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const result = try builder.ins().fcvtFromUint(clif_type, val);
            try defValue(result_id, result, ctx);
        },
        OP_FCVT_TO_UINT => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const result = try builder.ins().fcvtToUint(clif_type, val);
            try defValue(result_id, result, ctx);
        },
        OP_FPROMOTE => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const val_ty = builder.func.dfg.valueType(val);
            const result = if (val_ty.eql(clif_type))
                val // identity cast -- no-op
            else
                try builder.ins().fpromote(clif_type, val);
            try defValue(result_id, result, ctx);
        },
        OP_FDEMOTE => {
            const result_id = inst.words[0];
            const type_idx = inst.words[1];
            const val_id = inst.words[2];
            const clif_type = cirTypeToClif(type_idx);
            const val = try useValue(val_id, ctx);
            const val_ty = builder.func.dfg.valueType(val);
            const result = if (val_ty.eql(clif_type))
                val // identity cast -- no-op
            else
                try builder.ins().fdemote(clif_type, val);
            try defValue(result_id, result, ctx);
        },

        // -- Bitwise NOT --
        OP_NOT => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const r = try builder.ins().bnot(val);
            try defValue(result_id, r, ctx);
        },
        OP_POPCNT => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const r = try builder.ins().popcnt(val);
            try defValue(result_id, r, ctx);
        },
        OP_CLZ => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const r = try builder.ins().clz(val);
            try defValue(result_id, r, ctx);
        },
        OP_CTZ => {
            const result_id = inst.words[0];
            const val_id = inst.words[2];
            const val = try useValue(val_id, ctx);
            const r = try builder.ins().ctz(val);
            try defValue(result_id, r, ctx);
        },

        else => {
            // Unknown opcode -- skip for now, will be filled in as needed
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

test "cir_type_to_clif" {
    const testing = std.testing;
    try testing.expect(cirTypeToClif(TYPE_BOOL).eql(Type.I8));
    try testing.expect(cirTypeToClif(TYPE_I32).eql(Type.I32));
    try testing.expect(cirTypeToClif(TYPE_I64).eql(Type.I64));
    try testing.expect(cirTypeToClif(TYPE_F32).eql(Type.F32));
    try testing.expect(cirTypeToClif(TYPE_F64).eql(Type.F64));
    try testing.expect(cirTypeToClif(TYPE_VOID).eql(Type.INVALID));
    try testing.expect(cirTypeToClif(TYPE_STRING).eql(Type.I64));
}
