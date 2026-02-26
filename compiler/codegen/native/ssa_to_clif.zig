//! SSA → CLIF Translator — Direct native backend.
//!
//! Translates SSA functions directly to CLIF IR, bypassing Wasm entirely.
//! This is the core of the direct native path: SSA → CLIF → compile.zig → native.
//!
//! The translator walks SSA blocks in layout order, dispatching on each value's op
//! to emit corresponding CLIF instructions via FunctionBuilder.
//!
//! Reference patterns copied from:
//! - translator.zig (Wasm → CLIF) for builder setup and instruction emission
//! - lower_wasm.zig for SSA op dispatch structure

const std = @import("std");
const Allocator = std.mem.Allocator;

// SSA types
const ssa_mod = @import("../../ssa/func.zig");
const ssa_block = @import("../../ssa/block.zig");
const ssa_value = @import("../../ssa/value.zig");
const Op = @import("../../ssa/op.zig").Op;

// Frontend types
const types_mod = @import("../../frontend/types.zig");
const TypeIndex = types_mod.TypeIndex;
const TypeRegistry = types_mod.TypeRegistry;

// CLIF types
const clif = @import("../../ir/clif/mod.zig");
const GlobalValueData = @import("../../ir/clif/globalvalue.zig").GlobalValueData;
const gv_ExternalName = @import("../../ir/clif/globalvalue.zig").ExternalName;

// Frontend builder (Cranelift-style)
const frontend_mod = @import("frontend/mod.zig");
const FunctionBuilder = frontend_mod.FunctionBuilder;
const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;

const debug = @import("../../pipeline_debug.zig");

// ============================================================================
// Public API
// ============================================================================

// IR types (for param/return info)
const ir_mod = @import("../../frontend/ir.zig");

/// Translate an SSA function to CLIF IR.
///
/// This builds a complete CLIF Function from the SSA representation, using
/// the FunctionBuilder for proper SSA construction (block sealing, etc.).
///
/// The caller provides a pre-initialized clif.Function (with signature already set
/// if calling conventions matter). The translator fills it with blocks and instructions.
///
/// `ir_params` and `ir_return_type` are from the IR Func — these directly provide
/// the function signature since ssa_func.type_idx may not be a registered func type.
/// `func_index_map` maps function names to their indices in the object file,
/// so call relocations point to the correct target function.
/// `ir_funcs` is the full list of IR functions — used to look up callee signatures
/// when emitting calls (cg_clif pattern: get_function_ref uses callee's actual sig).
/// `string_data_symbol_idx` is the external name index for the string data section symbol.
/// When non-null, string_make ops use globalValue to get the base address of string data.
/// Reference: cg_clif constant.rs:152-157 — symbol_value for data section references.
/// `ctxt_symbol_idx` is the external name index for the closure context global variable.
/// Mirrors Wasm global 1 (CTXT). Used by closure_call to pass context, and by closure
/// body functions (via wasm_global_get) to read captured variables.
pub fn translate(
    ssa_func: *const ssa_mod.Func,
    clif_func: *clif.Function,
    type_reg: *const TypeRegistry,
    ir_params: []const ir_mod.Local,
    ir_return_type: TypeIndex,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    ir_funcs: []const ir_mod.Func,
    allocator: Allocator,
    string_data_symbol_idx: ?u32,
    ctxt_symbol_idx: u32,
    global_symbol_map: *const std.StringHashMapUnmanaged(u32),
) !void {
    var translator = SsaToClifTranslator.init(allocator, ssa_func, clif_func, type_reg, ir_params, ir_return_type, func_index_map, ir_funcs, string_data_symbol_idx, ctxt_symbol_idx, global_symbol_map);
    defer translator.deinit();
    try translator.run();
}

// ============================================================================
// SsaToClifTranslator
// ============================================================================

const SsaToClifTranslator = struct {
    allocator: Allocator,
    ssa_func: *const ssa_mod.Func,
    clif_func: *clif.Function,
    type_reg: *const TypeRegistry,
    builder: FunctionBuilder,
    func_ctx: FunctionBuilderContext,

    /// IR function parameter info — used for building CLIF signature.
    ir_params: []const ir_mod.Local,
    ir_return_type: TypeIndex,

    /// Function name → object file index map for call relocations.
    func_index_map: *const std.StringHashMapUnmanaged(u32),

    /// All IR functions — used to look up callee signatures for calls.
    /// Reference: cg_clif's get_function_ref uses callee's actual fn_abi.
    ir_funcs: []const ir_mod.Func,

    /// SSA value ID → CLIF value
    value_map: std.AutoHashMapUnmanaged(ssa_value.ID, clif.Value),

    /// SSA block ID → CLIF block
    block_map: std.AutoHashMapUnmanaged(ssa_value.ID, clif.Block),

    /// SSA block ID → list of phi CLIF block params (in order)
    /// Used when emitting terminators to pass phi args to successors.
    block_phi_params: std.AutoHashMapUnmanaged(ssa_value.ID, std.ArrayListUnmanaged(PhiInfo)),

    /// External function references for calls.
    func_refs: std.StringHashMapUnmanaged(clif.FuncRef),

    /// Stack slots for local variables.
    /// Indexed by slot offset (from local_addr aux_int), NOT by local_idx.
    stack_slot_map: std.AutoHashMapUnmanaged(u32, clif.StackSlot),

    /// Secondary value map for compound types (strings, slices).
    /// Maps string_make/slice_make SSA IDs to their length/extra component.
    /// Reference: cg_clif value_and_place.rs CValueInner::ByValPair(val1, val2).
    compound_extra_map: std.AutoHashMapUnmanaged(ssa_value.ID, clif.Value),

    /// External name index for string data section symbol.
    /// Used by string_make to get the base address of string literals.
    /// Reference: cg_clif constant.rs:152 — declare_data_in_func.
    string_data_symbol_idx: ?u32,

    /// External name index for CTXT global variable (closure context pointer).
    /// Mirrors Wasm global 1 (CTXT). Used by closure_call to store context before
    /// calling, and by wasm_global_get(1) in closure bodies to read context.
    ctxt_symbol_idx: u32,

    /// Tracks SSA value IDs that are string data offsets (args[0] of string_make).
    /// When a `copy` from one of these is used as a pointer, the string data base
    /// address is added. This is needed because rewritedec decomposes string_make
    /// into raw offset copies, losing the base address computation.
    string_offset_ids: std.AutoHashMapUnmanaged(ssa_value.ID, void),

    /// Global variable name → external name index for data section symbols.
    /// Used by global_addr op to get the address of module-level variables.
    global_symbol_map: *const std.StringHashMapUnmanaged(u32),

    /// Set when a value-level terminator (e.g. wasm_unreachable/trap) is emitted
    /// mid-block. The main loop checks this to skip the remaining values and the
    /// normal block terminator.
    block_terminated: bool,

    const Self = @This();

    const PhiInfo = struct {
        clif_param: clif.Value,
        ssa_value_id: ssa_value.ID,
    };

    fn init(
        allocator: Allocator,
        ssa_func: *const ssa_mod.Func,
        clif_func: *clif.Function,
        type_reg: *const TypeRegistry,
        ir_params: []const ir_mod.Local,
        ir_return_type: TypeIndex,
        func_index_map: *const std.StringHashMapUnmanaged(u32),
        ir_funcs: []const ir_mod.Func,
        string_data_symbol_idx: ?u32,
        ctxt_symbol_idx: u32,
        global_symbol_map: *const std.StringHashMapUnmanaged(u32),
    ) Self {
        var func_ctx = FunctionBuilderContext.init(allocator);
        var builder = FunctionBuilder.init(clif_func, &func_ctx);
        _ = &builder;
        return .{
            .allocator = allocator,
            .ssa_func = ssa_func,
            .clif_func = clif_func,
            .type_reg = type_reg,
            .ir_params = ir_params,
            .ir_return_type = ir_return_type,
            .func_index_map = func_index_map,
            .ir_funcs = ir_funcs,
            .builder = undefined, // set below
            .func_ctx = func_ctx,
            .value_map = .{},
            .block_map = .{},
            .block_phi_params = .{},
            .func_refs = .{},
            .stack_slot_map = .{},
            .compound_extra_map = .{},
            .string_data_symbol_idx = string_data_symbol_idx,
            .ctxt_symbol_idx = ctxt_symbol_idx,
            .string_offset_ids = .{},
            .global_symbol_map = global_symbol_map,
            .block_terminated = false,
        };
    }

    fn deinit(self: *Self) void {
        self.value_map.deinit(self.allocator);
        self.block_map.deinit(self.allocator);
        var phi_iter = self.block_phi_params.valueIterator();
        while (phi_iter.next()) |list| {
            var l = list.*;
            l.deinit(self.allocator);
        }
        self.block_phi_params.deinit(self.allocator);
        self.func_refs.deinit(self.allocator);
        self.stack_slot_map.deinit(self.allocator);
        self.compound_extra_map.deinit(self.allocator);
        self.string_offset_ids.deinit(self.allocator);
        self.func_ctx.deinit();
    }

    // ========================================================================
    // Main translation loop
    // ========================================================================

    fn run(self: *Self) !void {
        // Re-initialize builder after func_ctx is at its final address
        self.builder = FunctionBuilder.init(self.clif_func, &self.func_ctx);

        const ssa_func = self.ssa_func;
        debug.log(.codegen, "ssa_to_clif: translating '{s}' ({d} blocks)", .{
            ssa_func.name, ssa_func.blocks.items.len,
        });

        // Build CLIF signature from SSA func type (standard C ABI, no vmctx)
        try self.buildSignature();

        // Create stack slots for local variables
        try self.createStackSlots();

        // Phase 1: Pre-create CLIF blocks for all SSA blocks
        for (ssa_func.blocks.items) |ssa_block_ptr| {
            const clif_block = try self.builder.createBlock();
            try self.block_map.put(self.allocator, ssa_block_ptr.id, clif_block);
        }

        // Phase 1b: Pre-scan for string_make ops to track string offset IDs.
        // Needed because rewritedec creates copies of string offsets BEFORE the
        // string_make that identifies them. Without this pre-scan, the copy handler
        // wouldn't know to add the string data base address.
        // IMPORTANT: Only register const_64/const_int args — NOT loads or other ops.
        // When string_make is fed loaded values (e.g., from struct field access),
        // those are already absolute pointers. Adding base again would double it.
        if (self.string_data_symbol_idx != null) {
            for (ssa_func.blocks.items) |ssa_block_ptr| {
                for (ssa_block_ptr.values.items) |val| {
                    if (val.op == .string_make and val.args.len >= 2) {
                        const arg0_op = val.args[0].op;
                        if (arg0_op == .const_64 or arg0_op == .const_int) {
                            try self.string_offset_ids.put(self.allocator, val.args[0].id, {});
                        }
                    }
                }
            }
        }

        // Phase 2: Set up entry block — append function params
        const entry_ssa = ssa_func.entry orelse return error.NoEntryBlock;
        const entry_clif = self.block_map.get(entry_ssa.id) orelse return error.BlockNotFound;

        self.builder.switchToBlock(entry_clif);
        try self.builder.appendBlockParamsForFunctionParams(entry_clif);
        try self.builder.ensureInsertedBlock();
        try self.builder.sealBlock(entry_clif);

        // Phase 3: Pre-scan all blocks for phi values and add block params
        for (ssa_func.blocks.items) |ssa_block_ptr| {
            const clif_block = self.block_map.get(ssa_block_ptr.id) orelse continue;
            for (ssa_block_ptr.values.items) |v| {
                if (v.op == .phi) {
                    const clif_ty = self.ssaTypeToClifType(v.type_idx);
                    const param = try self.builder.appendBlockParam(clif_block, clif_ty);
                    // Record phi info for this block
                    const gop = try self.block_phi_params.getOrPut(self.allocator, ssa_block_ptr.id);
                    if (!gop.found_existing) gop.value_ptr.* = .{};
                    try gop.value_ptr.append(self.allocator, .{
                        .clif_param = param,
                        .ssa_value_id = v.id,
                    });
                    // Map phi's SSA value to the block param
                    try self.value_map.put(self.allocator, v.id, param);
                }
            }
        }

        // Phase 4: Translate each block
        for (ssa_func.blocks.items, 0..) |ssa_block_ptr, block_idx| {
            const clif_block = self.block_map.get(ssa_block_ptr.id) orelse continue;

            // Skip re-switching to entry (already there for first block)
            if (block_idx > 0 or ssa_block_ptr != entry_ssa) {
                self.builder.switchToBlock(clif_block);
                try self.builder.ensureInsertedBlock();
            }

            // Translate all values in this block
            self.block_terminated = false;
            for (ssa_block_ptr.values.items) |v| {
                try self.translateValue(v, ssa_block_ptr);
                if (self.block_terminated) break;
            }

            // Emit terminator based on block kind (skip if value-level terminator already emitted)
            if (!self.block_terminated) {
                try self.emitTerminator(ssa_block_ptr, block_idx);
            }
        }

        // Phase 5: Seal all remaining blocks and finalize
        try self.builder.sealAllBlocks();
        self.builder.finalize();

        debug.log(.codegen, "ssa_to_clif: done '{s}'", .{ssa_func.name});
    }

    // ========================================================================
    // Signature building
    // ========================================================================

    fn buildSignature(self: *Self) !void {
        // Build signature from IR params, decomposing compound types the same
        // way the SSA builder does (ssa_builder.zig:77-168).
        // Standard C ABI, NO vmctx.
        var total_clif_params: usize = 0;
        for (self.ir_params) |param| {
            const ptype = self.type_reg.get(param.type_idx);
            const is_string_or_slice = param.type_idx == TypeRegistry.STRING or ptype == .slice;
            const type_size = self.type_reg.sizeOf(param.type_idx);
            const is_large_struct = (ptype == .struct_type or ptype == .union_type or ptype == .tuple) and type_size > 8;

            if (is_string_or_slice) {
                // Decomposed to (ptr, len) — 2 x I64
                try self.clif_func.signature.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
                try self.clif_func.signature.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
                total_clif_params += 2;
            } else if (is_large_struct) {
                // Decomposed to N x I64 words
                const num_slots = @max(1, (type_size + 7) / 8);
                for (0..num_slots) |_| {
                    try self.clif_func.signature.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
                }
                total_clif_params += num_slots;
            } else {
                // Single register param
                try self.clif_func.signature.addParam(
                    self.allocator,
                    clif.AbiParam.init(self.ssaTypeToClifType(param.type_idx)),
                );
                total_clif_params += 1;
            }
        }
        if (self.ir_return_type != TypeRegistry.VOID) {
            try self.clif_func.signature.addReturn(
                self.allocator,
                clif.AbiParam.init(self.ssaTypeToClifType(self.ir_return_type)),
            );
        }

        debug.log(.codegen, "ssa_to_clif: signature: {d} ir params -> {d} clif params, return={d}", .{
            self.ir_params.len, total_clif_params, self.ir_return_type,
        });
    }

    // ========================================================================
    // Stack slots for local variables
    // ========================================================================

    fn createStackSlots(self: *Self) !void {
        // local_sizes is indexed by IR local_idx, but local_addr aux_int is a
        // slot offset (computed as sum of ceil(size/8) for preceding locals).
        // Build a map: slot_offset → CLIF stack slot.
        // Minimum slot size is 8 bytes because SSA values are I64-width
        // (e.g., const_bool produces I64, store writes 8 bytes).
        // Without this, types like UNTYPED_BOOL (sizeOf=0) create 0-byte slots
        // that overlap with adjacent slots.
        var next_slot_offset: u32 = 0;
        for (self.ssa_func.local_sizes) |size| {
            const actual_size = @max(size, 8);
            const align_shift: u8 = if (actual_size <= 1) 0 else if (actual_size <= 2) 1 else if (actual_size <= 4) 2 else 3;
            const slot = try self.builder.createSizedStackSlot(
                clif.StackSlotData.explicit(actual_size, align_shift),
            );
            try self.stack_slot_map.put(self.allocator, next_slot_offset, slot);
            const num_8byte_slots = @max(1, (size + 7) / 8);
            next_slot_offset += num_8byte_slots;
        }
    }

    // ========================================================================
    // Value translation — the main dispatch
    // ========================================================================

    fn translateValue(self: *Self, v: *const ssa_value.Value, _: *const ssa_block.Block) !void {
        const ins = self.builder.ins();

        switch (v.op) {
            // ============================================================
            // Constants
            // ============================================================
            .const_int, .const_64 => {
                const result = try ins.iconst(clif.Type.I64, v.aux_int);
                try self.putValue(v.id, result);
            },
            .const_32 => {
                const result = try ins.iconst(clif.Type.I32, v.aux_int);
                try self.putValue(v.id, result);
            },
            .const_16 => {
                const result = try ins.iconst(clif.Type.I16, v.aux_int);
                try self.putValue(v.id, result);
            },
            .const_8 => {
                const result = try ins.iconst(clif.Type.I8, v.aux_int);
                try self.putValue(v.id, result);
            },
            .const_bool => {
                const imm: i64 = if (v.aux_int != 0) 1 else 0;
                const result = try ins.iconst(clif.Type.I64, imm);
                try self.putValue(v.id, result);
            },
            .const_nil => {
                const result = try ins.iconst(clif.Type.I64, 0);
                try self.putValue(v.id, result);
            },
            .const_float => {
                const float_val: f64 = @bitCast(v.aux_int);
                const result = try ins.f64const(float_val);
                try self.putValue(v.id, result);
            },

            // ============================================================
            // Integer Arithmetic
            // ============================================================
            .add, .add64 => try self.emitBinary(v, .iadd),
            .sub, .sub64 => try self.emitBinary(v, .isub),
            .mul, .mul64 => try self.emitBinary(v, .imul),
            .div => try self.emitBinary(v, .sdiv),
            .udiv => try self.emitBinary(v, .udiv),
            .mod => try self.emitBinary(v, .srem),
            .umod => try self.emitBinary(v, .urem),
            .neg => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.ineg(arg);
                try self.putValue(v.id, result);
            },

            // 32-bit arithmetic
            .add32 => try self.emitBinary32(v, .iadd),
            .sub32 => try self.emitBinary32(v, .isub),
            .mul32 => try self.emitBinary32(v, .imul),

            // ============================================================
            // Bitwise operations
            // ============================================================
            .and_ => try self.emitBinary(v, .band),
            .or_ => try self.emitBinary(v, .bor),
            .xor => try self.emitBinary(v, .bxor),
            .shl => try self.emitBinary(v, .ishl),
            .shr => try self.emitBinary(v, .ushr),
            .sar => try self.emitBinary(v, .sshr),
            .not => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.bnot(arg);
                try self.putValue(v.id, result);
            },
            .bool_not => {
                // bool_not: arg == 0
                const arg = self.getClif(v.args[0]);
                const zero = try ins.iconst(clif.Type.I64, 0);
                const cmp = try ins.icmp(.eq, arg, zero);
                // icmp returns I8; extend to I64 so store/load round-trips correctly
                const result = try ins.uextend(clif.Type.I64, cmp);
                try self.putValue(v.id, result);
            },

            // Sized bitwise
            .and32 => try self.emitBinary32(v, .band),
            .or32 => try self.emitBinary32(v, .bor),
            .xor32 => try self.emitBinary32(v, .bxor),
            .shl32 => try self.emitBinary32(v, .ishl),
            .shr32 => try self.emitBinary32(v, .ushr),
            .sar32 => try self.emitBinary32(v, .sshr),
            .and64 => try self.emitBinary(v, .band),
            .or64 => try self.emitBinary(v, .bor),
            .xor64 => try self.emitBinary(v, .bxor),
            .shl64 => try self.emitBinary(v, .ishl),
            .shr64 => try self.emitBinary(v, .ushr),
            .sar64 => try self.emitBinary(v, .sshr),

            // Bit count
            .clz64 => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.clz(arg);
                try self.putValue(v.id, result);
            },
            .ctz64 => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.ctz(arg);
                try self.putValue(v.id, result);
            },
            .popcnt64 => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.popcnt(arg);
                try self.putValue(v.id, result);
            },
            .clz32 => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.clz(arg);
                try self.putValue(v.id, result);
            },
            .ctz32 => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.ctz(arg);
                try self.putValue(v.id, result);
            },
            .popcnt32 => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.popcnt(arg);
                try self.putValue(v.id, result);
            },

            // ============================================================
            // Float operations
            // ============================================================
            .add64f => try self.emitFloatBinary(v, .fadd, clif.Type.F64),
            .sub64f => try self.emitFloatBinary(v, .fsub, clif.Type.F64),
            .mul64f => try self.emitFloatBinary(v, .fmul, clif.Type.F64),
            .div64f => try self.emitFloatBinary(v, .fdiv, clif.Type.F64),
            .add32f => try self.emitFloatBinary(v, .fadd, clif.Type.F32),
            .sub32f => try self.emitFloatBinary(v, .fsub, clif.Type.F32),
            .mul32f => try self.emitFloatBinary(v, .fmul, clif.Type.F32),
            .div32f => try self.emitFloatBinary(v, .fdiv, clif.Type.F32),
            .neg64f => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.fneg(clif.Type.F64, arg);
                try self.putValue(v.id, result);
            },
            .neg32f => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.fneg(clif.Type.F32, arg);
                try self.putValue(v.id, result);
            },
            .sqrt64f => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.sqrt(clif.Type.F64, arg);
                try self.putValue(v.id, result);
            },
            .sqrt32f => {
                const arg = self.getClif(v.args[0]);
                const result = try ins.sqrt(clif.Type.F32, arg);
                try self.putValue(v.id, result);
            },

            // ============================================================
            // Comparisons
            // ============================================================
            .eq, .eq64 => try self.emitCompare(v, .eq),
            .ne, .ne64 => try self.emitCompare(v, .ne),
            .lt, .lt64 => try self.emitCompare(v, .slt),
            .le, .le64 => try self.emitCompare(v, .sle),
            .gt, .gt64 => try self.emitCompare(v, .sgt),
            .ge, .ge64 => try self.emitCompare(v, .sge),
            .ult => try self.emitCompare(v, .ult),
            .ule => try self.emitCompare(v, .ule),
            .ugt => try self.emitCompare(v, .ugt),
            .uge => try self.emitCompare(v, .uge),
            .eq32 => try self.emitCompare(v, .eq),
            .ne32 => try self.emitCompare(v, .ne),
            .lt32 => try self.emitCompare(v, .slt),
            .le32 => try self.emitCompare(v, .sle),
            .gt32 => try self.emitCompare(v, .sgt),
            .ge32 => try self.emitCompare(v, .sge),

            // Float comparisons
            .eq64f => try self.emitFloatCompare(v, .eq),
            .ne64f => try self.emitFloatCompare(v, .ne),
            .lt64f => try self.emitFloatCompare(v, .lt),
            .le64f => try self.emitFloatCompare(v, .le),
            .gt64f => try self.emitFloatCompare(v, .gt),
            .ge64f => try self.emitFloatCompare(v, .ge),
            .eq32f => try self.emitFloatCompare(v, .eq),
            .ne32f => try self.emitFloatCompare(v, .ne),
            .lt32f => try self.emitFloatCompare(v, .lt),
            .le32f => try self.emitFloatCompare(v, .le),
            .gt32f => try self.emitFloatCompare(v, .gt),
            .ge32f => try self.emitFloatCompare(v, .ge),

            // ============================================================
            // Type conversions
            // ============================================================
            .sign_ext8to16 => try self.emitConvert(v, .sextend, clif.Type.I16),
            .sign_ext8to32 => try self.emitConvert(v, .sextend, clif.Type.I32),
            .sign_ext8to64 => try self.emitConvert(v, .sextend, clif.Type.I64),
            .sign_ext16to32 => try self.emitConvert(v, .sextend, clif.Type.I32),
            .sign_ext16to64 => try self.emitConvert(v, .sextend, clif.Type.I64),
            .sign_ext32to64 => try self.emitConvert(v, .sextend, clif.Type.I64),
            .zero_ext8to16 => try self.emitConvert(v, .uextend, clif.Type.I16),
            .zero_ext8to32 => try self.emitConvert(v, .uextend, clif.Type.I32),
            .zero_ext8to64 => try self.emitConvert(v, .uextend, clif.Type.I64),
            .zero_ext16to32 => try self.emitConvert(v, .uextend, clif.Type.I32),
            .zero_ext16to64 => try self.emitConvert(v, .uextend, clif.Type.I64),
            .zero_ext32to64 => try self.emitConvert(v, .uextend, clif.Type.I64),
            .trunc16to8 => try self.emitConvert(v, .ireduce, clif.Type.I8),
            .trunc32to8 => try self.emitConvert(v, .ireduce, clif.Type.I8),
            .trunc32to16 => try self.emitConvert(v, .ireduce, clif.Type.I16),
            .trunc64to8 => try self.emitConvert(v, .ireduce, clif.Type.I8),
            .trunc64to16 => try self.emitConvert(v, .ireduce, clif.Type.I16),
            .trunc64to32 => try self.emitConvert(v, .ireduce, clif.Type.I32),

            // Float conversions
            .cvt64to64f => try self.emitConvert(v, .fcvt_from_sint, clif.Type.F64),
            .cvt32to64f => try self.emitConvert(v, .fcvt_from_sint, clif.Type.F64),
            .cvt64to32f => try self.emitConvert(v, .fcvt_from_sint, clif.Type.F32),
            .cvt32to32f => try self.emitConvert(v, .fcvt_from_sint, clif.Type.F32),
            .cvt64fto64 => try self.emitConvert(v, .fcvt_to_sint, clif.Type.I64),
            .cvt64fto32 => try self.emitConvert(v, .fcvt_to_sint, clif.Type.I32),
            .cvt32fto64 => try self.emitConvert(v, .fcvt_to_sint, clif.Type.I64),
            .cvt32fto32 => try self.emitConvert(v, .fcvt_to_sint, clif.Type.I32),
            .cvt32fto64f => try self.emitConvert(v, .fpromote, clif.Type.F64),
            .cvt64fto32f => try self.emitConvert(v, .fdemote, clif.Type.F32),

            // Generic convert — dispatches based on source/target type.
            // Reference: gen.zig:1086-1131 (Wasm codegen handles convert with type_ref)
            .convert => {
                try self.emitGenericConvert(v);
            },

            // ============================================================
            // Memory operations
            // Reference: rustc_codegen_cranelift/src/pointer.rs:109-127
            // Pointer.load()/store() pass the offset directly to CLIF
            // load/store instructions. The ARM64/x64 backends honor this.
            // ============================================================
            .load, .load64 => {
                // Generic load — use I64 for integer types (all SSA values are I64-width;
                // width-specific loads handle narrow types), but use F32/F64 for floats.
                const addr = self.getClif(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const ty = self.ssaTypeToClifType(v.type_idx);
                const load_ty = if (ty.repr == clif.Type.F32.repr or ty.repr == clif.Type.F64.repr)
                    ty
                else
                    clif.Type.I64;
                const result = try ins.load(load_ty, clif.MemFlags.DEFAULT, addr, offset);
                try self.putValue(v.id, result);
            },
            .load32 => {
                const addr = self.getClif(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const loaded = try ins.load(clif.Type.I32, clif.MemFlags.DEFAULT, addr, offset);
                const result = try ins.uextend(clif.Type.I64, loaded);
                try self.putValue(v.id, result);
            },
            .load32s => {
                const addr = self.getClif(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const loaded = try ins.load(clif.Type.I32, clif.MemFlags.DEFAULT, addr, offset);
                const result = try ins.sextend(clif.Type.I64, loaded);
                try self.putValue(v.id, result);
            },
            .load16 => {
                const addr = self.getClif(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const loaded = try ins.load(clif.Type.I16, clif.MemFlags.DEFAULT, addr, offset);
                const result = try ins.uextend(clif.Type.I64, loaded);
                try self.putValue(v.id, result);
            },
            .load16s => {
                const addr = self.getClif(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const loaded = try ins.load(clif.Type.I16, clif.MemFlags.DEFAULT, addr, offset);
                const result = try ins.sextend(clif.Type.I64, loaded);
                try self.putValue(v.id, result);
            },
            .load8 => {
                const addr = self.getClif(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const loaded = try ins.load(clif.Type.I8, clif.MemFlags.DEFAULT, addr, offset);
                const result = try ins.uextend(clif.Type.I64, loaded);
                try self.putValue(v.id, result);
            },
            .load8s => {
                const addr = self.getClif(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const loaded = try ins.load(clif.Type.I8, clif.MemFlags.DEFAULT, addr, offset);
                const result = try ins.sextend(clif.Type.I64, loaded);
                try self.putValue(v.id, result);
            },
            .store, .store64 => {
                // SSA convention: args[0] = address, args[1] = value
                const addr = self.getClif(v.args[0]);
                const val = self.getClif(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                _ = try ins.store(clif.MemFlags.DEFAULT, val, addr, offset);
            },
            .store32 => {
                const addr = self.getClif(v.args[0]);
                const val = self.getClif(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                const truncated = try ins.ireduce(clif.Type.I32, val);
                _ = try ins.store(clif.MemFlags.DEFAULT, truncated, addr, offset);
            },
            .store16 => {
                const addr = self.getClif(v.args[0]);
                const val = self.getClif(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                const truncated = try ins.ireduce(clif.Type.I16, val);
                _ = try ins.store(clif.MemFlags.DEFAULT, truncated, addr, offset);
            },
            .store8 => {
                const addr = self.getClif(v.args[0]);
                const val = self.getClif(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                const truncated = try ins.ireduce(clif.Type.I8, val);
                _ = try ins.store(clif.MemFlags.DEFAULT, truncated, addr, offset);
            },

            // ============================================================
            // Address computation — native pointers, no heap_base
            // ============================================================
            .off_ptr => {
                const base = self.getClif(v.args[0]);
                const offset = v.aux_int;
                const result = try ins.iaddImm(base, offset);
                try self.putValue(v.id, result);
            },
            .add_ptr => {
                const base = self.getClif(v.args[0]);
                const index = self.getClif(v.args[1]);
                const result = try ins.iadd(base, index);
                try self.putValue(v.id, result);
            },
            .sub_ptr => {
                const base = self.getClif(v.args[0]);
                const index = self.getClif(v.args[1]);
                const result = try ins.isub(base, index);
                try self.putValue(v.id, result);
            },
            .local_addr => {
                const slot_offset: u32 = @intCast(v.aux_int);
                const stack_slot = self.stack_slot_map.get(slot_offset) orelse {
                    debug.log(.codegen, "ssa_to_clif: local_addr slot offset {d} not found (have {d} slots)", .{
                        slot_offset, self.stack_slot_map.count(),
                    });
                    return error.StackSlotNotFound;
                };
                const result = try ins.stackAddr(clif.Type.I64, stack_slot, 0);
                try self.putValue(v.id, result);
            },
            .global_addr => {
                // Get address of a module-level global variable.
                // Each global is a data section symbol, accessed via globalValue.
                const global_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                if (global_name) |name| {
                    if (self.global_symbol_map.get(name)) |sym_idx| {
                        const gv = try self.clif_func.createGlobalValue(GlobalValueData{
                            .symbol = .{
                                .name = gv_ExternalName.initUser(0, sym_idx),
                                .offset = 0,
                                .colocated = true,
                                .tls = false,
                            },
                        });
                        const addr = try ins.globalValue(clif.Type.I64, gv);
                        try self.putValue(v.id, addr);
                    } else {
                        debug.log(.codegen, "ssa_to_clif: global_addr '{s}' not found in symbol map", .{name});
                        const addr = try ins.iconst(clif.Type.I64, 0);
                        try self.putValue(v.id, addr);
                    }
                } else {
                    const addr = try ins.iconst(clif.Type.I64, 0);
                    try self.putValue(v.id, addr);
                }
            },
            .addr => {
                // Get function address as pointer value.
                // Wasm uses table indices; native uses actual addresses via func_addr CLIF op.
                // Reference: cg_clif base.rs — func_addr for taking address of a function.
                const fn_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                if (fn_name) |name| {
                    const func_ref = try self.getOrCreateFuncRef(name, 0, 0);
                    const result = try ins.funcAddr(clif.Type.I64, func_ref);
                    try self.putValue(v.id, result);
                } else {
                    debug.log(.codegen, "ssa_to_clif: addr op with no function name (v{d})", .{v.id});
                    const result = try ins.iconst(clif.Type.I64, 0);
                    try self.putValue(v.id, result);
                }
            },

            // ============================================================
            // Control flow / structural ops
            // ============================================================
            .arg => {
                // Map to entry block param at the corresponding index
                const entry_ssa = self.ssa_func.entry orelse return error.NoEntryBlock;
                const entry_clif = self.block_map.get(entry_ssa.id) orelse return error.BlockNotFound;
                const params = self.builder.blockParams(entry_clif);
                const arg_idx: usize = @intCast(v.aux_int);
                if (arg_idx < params.len) {
                    try self.putValue(v.id, params[arg_idx]);
                } else {
                    // Arg out of range — emit zero
                    const result = try ins.iconst(clif.Type.I64, 0);
                    try self.putValue(v.id, result);
                }
            },
            .copy => {
                // Copy: use the same CLIF value as the source.
                // Special case: if source is a string data offset (tracked by string_make),
                // add the string data base address to produce an actual pointer.
                // This is needed because rewritedec decomposes string_make into raw offset
                // copies, losing the globalValue base address computation.
                if (v.args.len > 0) {
                    const src = self.getClif(v.args[0]);
                    if (self.string_data_symbol_idx != null and self.string_offset_ids.contains(v.args[0].id)) {
                        const sym_idx = self.string_data_symbol_idx.?;
                        const gv = try self.clif_func.createGlobalValue(GlobalValueData{
                            .symbol = .{
                                .name = gv_ExternalName.initUser(0, sym_idx),
                                .offset = 0,
                                .colocated = true,
                                .tls = false,
                            },
                        });
                        const base = try ins.globalValue(clif.Type.I64, gv);
                        const ptr = try ins.iadd(base, src);
                        try self.putValue(v.id, ptr);
                    } else {
                        try self.putValue(v.id, src);
                    }
                }
            },
            .phi => {
                // Already handled in Phase 3 (block params)
            },
            .cond_select => {
                const cond = self.getClif(v.args[0]);
                const if_true = self.getClif(v.args[1]);
                const if_false = self.getClif(v.args[2]);
                const ty = self.ssaTypeToClifType(v.type_idx);
                const result = try ins.select(ty, cond, if_true, if_false);
                try self.putValue(v.id, result);
            },

            // ============================================================
            // Function calls
            // ============================================================
            .static_call, .call => {
                try self.emitCall(v);
            },
            .closure_call, .inter_call => {
                try self.emitIndirectCall(v);
            },

            // select0/select1 handled after call results
            .select0 => {
                // First result of preceding call — should be mapped already
                if (v.args.len > 0) {
                    if (self.value_map.get(v.args[0].id)) |call_result| {
                        try self.putValue(v.id, call_result);
                    }
                }
            },
            .select1 => {
                // Second result — not yet implemented for multi-return
                // For now, produce zero placeholder
                const result = try ins.iconst(clif.Type.I64, 0);
                try self.putValue(v.id, result);
            },
            .make_tuple => {
                // Bundle values — just map to first arg for now
                if (v.args.len > 0) {
                    const first = self.getClif(v.args[0]);
                    try self.putValue(v.id, first);
                }
            },

            // ============================================================
            // ARC ops — emit as calls to runtime functions
            // Reference: cg_clif abi/mod.rs:183-201 lib_call pattern
            // ============================================================
            .retain => {
                const arg = self.getClif(v.args[0]);
                const func_ref = try self.getOrCreateFuncRef("retain", 1, 1);
                const call_result = try ins.call(func_ref, &[_]clif.Value{arg});
                if (call_result.results.len > 0) {
                    try self.putValue(v.id, call_result.results[0]);
                }
            },
            .release => {
                const arg = self.getClif(v.args[0]);
                const func_ref = try self.getOrCreateFuncRef("release", 1, 0);
                _ = try ins.call(func_ref, &[_]clif.Value{arg});
            },

            // ============================================================
            // Nil checks
            // ============================================================
            .nil_check => {
                if (v.args.len > 0) {
                    const arg = self.getClif(v.args[0]);
                    _ = try ins.trapz(arg, .user1); // user1 = null reference trap
                }
            },
            .is_non_nil => {
                const arg = self.getClif(v.args[0]);
                const zero = try ins.iconst(clif.Type.I64, 0);
                const result = try ins.icmp(.ne, arg, zero);
                try self.putValue(v.id, result);
            },
            .is_nil => {
                const arg = self.getClif(v.args[0]);
                const zero = try ins.iconst(clif.Type.I64, 0);
                const result = try ins.icmp(.eq, arg, zero);
                try self.putValue(v.id, result);
            },

            // ============================================================
            // Memory state / ops we skip
            // ============================================================
            .init_mem, .var_def, .var_live, .var_kill, .fwd_ref => {
                // No CLIF equivalent — skip
            },

            // ============================================================
            // Move/zero for large types
            // ============================================================
            .move => {
                // memcpy — inline load/store pairs with direct offsets
                // SSA convention: args[0] = dest addr, args[1] = src addr, aux = byte size
                // Reference: rustc_codegen_cranelift Pointer.load/store use offset directly
                if (v.args.len >= 2) {
                    const dst = self.getClif(v.args[0]);
                    const src = self.getClif(v.args[1]);
                    const size: u32 = @intCast(v.aux_int);

                    var offset: i32 = 0;
                    const end: i32 = @intCast(size);
                    while (offset + 8 <= end) {
                        const val = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, src, offset);
                        _ = try ins.store(clif.MemFlags.DEFAULT, val, dst, offset);
                        offset += 8;
                    }
                    while (offset + 4 <= end) {
                        const val = try ins.load(clif.Type.I32, clif.MemFlags.DEFAULT, src, offset);
                        _ = try ins.store(clif.MemFlags.DEFAULT, val, dst, offset);
                        offset += 4;
                    }
                    while (offset + 1 <= end) {
                        const val = try ins.load(clif.Type.I8, clif.MemFlags.DEFAULT, src, offset);
                        _ = try ins.store(clif.MemFlags.DEFAULT, val, dst, offset);
                        offset += 1;
                    }
                }
                // move doesn't produce a value; map to dest for any downstream refs
                if (v.args.len > 0) {
                    const dst = self.getClif(v.args[0]);
                    try self.putValue(v.id, dst);
                }
            },
            .zero => {
                // Zero memory — store zeros inline with direct offsets
                if (v.args.len > 0) {
                    const addr = self.getClif(v.args[0]);
                    const size: u32 = @intCast(v.aux_int);
                    const zero64 = try ins.iconst(clif.Type.I64, 0);
                    const zero8 = try ins.iconst(clif.Type.I8, 0);

                    var offset: i32 = 0;
                    const end: i32 = @intCast(size);
                    while (offset + 8 <= end) {
                        _ = try ins.store(clif.MemFlags.DEFAULT, zero64, addr, offset);
                        offset += 8;
                    }
                    while (offset + 1 <= end) {
                        _ = try ins.store(clif.MemFlags.DEFAULT, zero8, addr, offset);
                        offset += 1;
                    }
                    try self.putValue(v.id, addr);
                } else {
                    const result = try ins.iconst(clif.Type.I64, 0);
                    try self.putValue(v.id, result);
                }
            },

            // ============================================================
            // String/slice construction — cg_clif ByValPair pattern
            // Reference: cg_clif constant.rs:223-227 — CValue::by_val_pair(ptr, len)
            // Reference: cg_clif value_and_place.rs:91-97 — ByValPair
            // ============================================================
            .string_make => {
                // string_make(offset, len) → compute actual pointer from string data section
                // After rewritegeneric: args[0] = const_64 with data section byte offset
                //                       args[1] = const_64 with string length
                // Track the offset arg so copies of it get the base address applied.
                // IMPORTANT: Only add base when arg[0] is a raw offset (const_64/const_int).
                // When string_make is fed loaded values (e.g., from struct field access),
                // those are already absolute pointers — don't add base again.
                if (v.args.len >= 2) {
                    const arg0_op = v.args[0].op;
                    const is_raw_offset = arg0_op == .const_64 or arg0_op == .const_int;
                    if (is_raw_offset) {
                        try self.string_offset_ids.put(self.allocator, v.args[0].id, {});
                    }
                    const ptr_val = self.getClif(v.args[0]);
                    const len_val = self.getClif(v.args[1]);

                    if (is_raw_offset) {
                        if (self.string_data_symbol_idx) |sym_idx| {
                            const gv = try self.clif_func.createGlobalValue(GlobalValueData{
                                .symbol = .{
                                    .name = gv_ExternalName.initUser(0, sym_idx),
                                    .offset = 0,
                                    .colocated = true,
                                    .tls = false,
                                },
                            });
                            const base = try ins.globalValue(clif.Type.I64, gv);
                            const ptr = try ins.iadd(base, ptr_val);
                            try self.putValue(v.id, ptr);
                        } else {
                            try self.putValue(v.id, ptr_val);
                        }
                    } else {
                        // Args are already absolute pointers (e.g., loaded from struct)
                        try self.putValue(v.id, ptr_val);
                    }
                    try self.compound_extra_map.put(self.allocator, v.id, len_val);
                }
            },
            .slice_make => {
                // slice_make(ptr, len, cap) — map to first arg (ptr)
                // Store len in compound_extra_map for decomposition
                if (v.args.len >= 2) {
                    const ptr_val = self.getClif(v.args[0]);
                    const len_val = self.getClif(v.args[1]);
                    try self.putValue(v.id, ptr_val);
                    try self.compound_extra_map.put(self.allocator, v.id, len_val);
                }
            },

            // ============================================================
            // String ops — field access on string values
            // ============================================================
            .string_ptr => {
                // After rewritedec, these should be decomposed to copies.
                // But if they remain, load from memory layout (ptr, len).
                if (v.args.len > 0) {
                    const arg = self.getClif(v.args[0]);
                    const result = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, arg, 0);
                    try self.putValue(v.id, result);
                }
            },
            .string_len => {
                if (v.args.len > 0) {
                    const arg = self.getClif(v.args[0]);
                    const result = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, arg, 8);
                    try self.putValue(v.id, result);
                }
            },

            // ============================================================
            // Compound value component extraction (strings, slices)
            // These extract the ptr/len components of compound types
            // from either compound_extra_map (ByValPair) or memory loads.
            // ============================================================
            .slice_ptr => {
                // Extract pointer component from compound value (first of pair)
                if (v.args.len > 0) {
                    const src_id = v.args[0].id;
                    if (self.value_map.get(src_id)) |primary| {
                        try self.putValue(v.id, primary);
                    } else {
                        // Fallback: load from memory
                        const arg = self.getClif(v.args[0]);
                        const result = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, arg, 0);
                        try self.putValue(v.id, result);
                    }
                }
            },
            .slice_len, .slice_cap => {
                // Extract length/cap component from compound value (second of pair)
                if (v.args.len > 0) {
                    const src_id = v.args[0].id;
                    if (self.compound_extra_map.get(src_id)) |secondary| {
                        try self.putValue(v.id, secondary);
                    } else {
                        // Fallback: load from memory at offset 8
                        const arg = self.getClif(v.args[0]);
                        const result = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, arg, 8);
                        try self.putValue(v.id, result);
                    }
                }
            },

            // ============================================================
            // Wasm globals — used by closure bodies to read CTXT
            // ============================================================
            .wasm_global_get => {
                // In native, Wasm global 1 = CTXT (closure context pointer).
                // Load from the _cot_ctxt data section variable.
                const global_idx = v.aux_int;
                if (global_idx == 1) {
                    // CTXT global: load from _cot_ctxt symbol
                    const ctxt_addr = try self.getCtxtAddr();
                    const result = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ctxt_addr, 0);
                    try self.putValue(v.id, result);
                } else {
                    debug.log(.codegen, "ssa_to_clif: unhandled wasm_global_get index {d}", .{global_idx});
                    const result = try ins.iconst(clif.Type.I64, 0);
                    try self.putValue(v.id, result);
                }
            },

            .wasm_unreachable => {
                // When unreachable is used as a switch arm value (e.g. else => unreachable),
                // the SSA inlines it as a value in the cond_select chain. We can't emit a
                // trap here because that would terminate the block before the select runs.
                // Instead, produce a dummy value — the select will never choose it at runtime.
                // If this block's KIND is exit (standalone unreachable), the terminator handles it.
                const result = try ins.iconst(clif.Type.I64, 0);
                try self.putValue(v.id, result);
            },

            else => {
                // Unhandled op — emit zero placeholder and log
                debug.log(.codegen, "ssa_to_clif: unhandled op {s} (v{d})", .{ @tagName(v.op), v.id });
                const result = try ins.iconst(clif.Type.I64, 0);
                try self.putValue(v.id, result);
            },
        }
    }

    // ========================================================================
    // Terminator emission
    // ========================================================================

    fn emitTerminator(self: *Self, ssa_blk: *const ssa_block.Block, block_idx: usize) !void {
        const ins = self.builder.ins();

        switch (ssa_blk.kind) {
            .ret => {
                // Return: controls[0] is the return value (if any)
                // Reference: cg_clif value_and_place.rs — ByValPair for compound returns
                if (ssa_blk.controls[0]) |ctrl| {
                    if ((ctrl.op == .string_make or ctrl.op == .slice_make) and ctrl.args.len >= 2) {
                        // Compound return: return both components (ptr, len)
                        // Reference: gen.zig:217-223 — Wasm path pushes both components
                        const ptr = self.getClif(ctrl); // Adjusted pointer (from value_map)
                        const len = self.getClif(ctrl.args[1]); // Length component
                        _ = try ins.return_(&[_]clif.Value{ ptr, len });
                    } else if (self.compound_extra_map.get(ctrl.id)) |_| {
                        // Value was previously identified as compound (e.g., passed through phi)
                        const ptr = self.getClif(ctrl);
                        if (self.compound_extra_map.get(ctrl.id)) |len| {
                            _ = try ins.return_(&[_]clif.Value{ ptr, len });
                        } else {
                            _ = try ins.return_(&[_]clif.Value{ptr});
                        }
                    } else {
                        const val = self.getClif(ctrl);
                        _ = try ins.return_(&[_]clif.Value{val});
                    }
                } else {
                    _ = try ins.return_(&[_]clif.Value{});
                }
            },
            .plain, .defer_, .first => {
                // Unconditional jump to single successor
                if (ssa_blk.succs.len > 0) {
                    const target_ssa = ssa_blk.succs[0].b;
                    const target_clif = self.block_map.get(target_ssa.id) orelse return error.BlockNotFound;
                    const phi_args = try self.gatherPhiArgs(ssa_blk, target_ssa);
                    _ = try ins.jump(target_clif, phi_args);
                } else {
                    // No explicit successor — fall through to next layout block if available.
                    // SSA uses implicit fallthrough for plain blocks (e.g., blocks containing
                    // wasm_unreachable as a value, where the next block has the actual logic).
                    const blocks = self.ssa_func.blocks.items;
                    if (block_idx + 1 < blocks.len) {
                        // Find the next non-empty block in layout order
                        var next_idx = block_idx + 1;
                        while (next_idx < blocks.len and blocks[next_idx].values.items.len == 0 and blocks[next_idx].kind == .plain and blocks[next_idx].succs.len == 0) {
                            next_idx += 1;
                        }
                        if (next_idx < blocks.len) {
                            const next_ssa = blocks[next_idx];
                            const next_clif = self.block_map.get(next_ssa.id) orelse return error.BlockNotFound;
                            _ = try ins.jump(next_clif, &.{});
                        } else {
                            _ = try ins.trap(.unreachable_code_reached);
                        }
                    } else {
                        _ = try ins.trap(.unreachable_code_reached);
                    }
                }
            },
            .if_ => {
                // Conditional branch: controls[0] is the condition
                const cond = if (ssa_blk.controls[0]) |ctrl| self.getClif(ctrl) else return error.MissingControl;
                if (ssa_blk.succs.len >= 2) {
                    const then_ssa = ssa_blk.succs[0].b;
                    const else_ssa = ssa_blk.succs[1].b;
                    const then_clif = self.block_map.get(then_ssa.id) orelse return error.BlockNotFound;
                    const else_clif = self.block_map.get(else_ssa.id) orelse return error.BlockNotFound;
                    const then_args = try self.gatherPhiArgs(ssa_blk, then_ssa);
                    const else_args = try self.gatherPhiArgs(ssa_blk, else_ssa);
                    _ = try ins.brif(cond, then_clif, then_args, else_clif, else_args);
                } else {
                    _ = try ins.trap(.unreachable_code_reached);
                }
            },
            .exit => {
                _ = try ins.trap(.unreachable_code_reached);
            },
            .jump_table => {
                // Branch table: controls[0] is the selector
                const selector = if (ssa_blk.controls[0]) |ctrl| self.getClif(ctrl) else return error.MissingControl;
                if (ssa_blk.succs.len > 0) {
                    // First successor is default, rest are targets
                    const default_ssa = ssa_blk.succs[0].b;
                    const default_clif = self.block_map.get(default_ssa.id) orelse return error.BlockNotFound;

                    var targets = std.ArrayListUnmanaged(clif.Block){};
                    defer targets.deinit(self.allocator);
                    for (ssa_blk.succs[1..]) |edge| {
                        const t = self.block_map.get(edge.b.id) orelse return error.BlockNotFound;
                        try targets.append(self.allocator, t);
                    }

                    const jt = try self.builder.createJumpTable(default_clif, targets.items);
                    _ = try ins.brTable(selector, jt);
                } else {
                    _ = try ins.trap(.unreachable_code_reached);
                }
            },
            else => {
                // Architecture-specific block kinds shouldn't appear at this stage
                debug.log(.codegen, "ssa_to_clif: unhandled block kind {s}", .{@tagName(ssa_blk.kind)});
                _ = try ins.trap(.unreachable_code_reached);
            },
        }
    }

    // ========================================================================
    // Phi resolution — gather args for successor block params
    // ========================================================================

    fn gatherPhiArgs(self: *Self, from_block: *const ssa_block.Block, to_block: *const ssa_block.Block) ![]const clif.Value {
        const phi_infos = self.block_phi_params.get(to_block.id) orelse return &[_]clif.Value{};

        var args = std.ArrayListUnmanaged(clif.Value){};
        defer args.deinit(self.allocator);

        for (phi_infos.items) |phi_info| {
            // Find the phi value in the target block
            var found = false;
            for (to_block.values.items) |phi_v| {
                if (phi_v.id == phi_info.ssa_value_id and phi_v.op == .phi) {
                    // Find which arg of this phi corresponds to from_block
                    const pred_idx = self.findPredIndex(to_block, from_block);
                    if (pred_idx) |idx| {
                        if (idx < phi_v.args.len) {
                            const arg_val = self.getClif(phi_v.args[idx]);
                            try args.append(self.allocator, arg_val);
                            found = true;
                        }
                    }
                    break;
                }
            }
            if (!found) {
                // Phi arg not found — use zero
                const zero = try self.builder.ins().iconst(clif.Type.I64, 0);
                try args.append(self.allocator, zero);
            }
        }

        // Return a stable slice by allocating
        const result = try self.allocator.alloc(clif.Value, args.items.len);
        @memcpy(result, args.items);
        return result;
    }

    fn findPredIndex(self: *Self, block: *const ssa_block.Block, pred: *const ssa_block.Block) ?usize {
        _ = self;
        for (block.preds, 0..) |edge, i| {
            if (edge.b.id == pred.id) return i;
        }
        return null;
    }

    // ========================================================================
    // Call emission
    // ========================================================================

    fn emitCall(self: *Self, v: *const ssa_value.Value) !void {
        const func_name = switch (v.aux) {
            .string => |s| s,
            .call => |c| c.fn_name,
            else => "unknown",
        };

        // Gather call arguments
        var call_args = std.ArrayListUnmanaged(clif.Value){};
        defer call_args.deinit(self.allocator);
        for (v.args) |arg| {
            const clif_arg = self.getClif(arg);
            try call_args.append(self.allocator, clif_arg);
        }

        // Create/get function reference
        const n_params = v.args.len;
        const has_return = v.type_idx != TypeRegistry.VOID and v.type_idx != TypeRegistry.SSA_MEM;
        const n_returns: usize = if (has_return) 1 else 0;
        const func_ref = try self.getOrCreateFuncRef(func_name, n_params, n_returns);

        const call_result = try self.builder.ins().call(func_ref, call_args.items);
        if (call_result.results.len > 0) {
            try self.putValue(v.id, call_result.results[0]);
            // For compound return types (string, slice), store second result in compound_extra_map
            if (call_result.results.len > 1) {
                try self.compound_extra_map.put(self.allocator, v.id, call_result.results[1]);
            }
        }
    }

    fn emitIndirectCall(self: *Self, v: *const ssa_value.Value) !void {
        // closure_call: args[0]=callee, args[1]=context_ptr, args[2..]=function args
        // inter_call:   args[0]=callee, args[1..]=function args
        if (v.args.len == 0) return;
        const callee = self.getClif(v.args[0]);

        // For closure_call: store context pointer to _cot_ctxt global (mirrors Wasm CTXT global)
        // The closure body reads it via wasm_global_get(1).
        const actual_args_start: usize = if (v.op == .closure_call) blk: {
            if (v.args.len > 1) {
                const context = self.getClif(v.args[1]);
                const ctxt_addr = try self.getCtxtAddr();
                _ = try self.builder.ins().store(clif.MemFlags.DEFAULT, context, ctxt_addr, 0);
            }
            break :blk 2;
        } else 1;

        var call_args = std.ArrayListUnmanaged(clif.Value){};
        defer call_args.deinit(self.allocator);
        if (actual_args_start < v.args.len) {
            for (v.args[actual_args_start..]) |arg| {
                const clif_arg = self.getClif(arg);
                try call_args.append(self.allocator, clif_arg);
            }
        }

        // Build signature for indirect call
        var sig = clif.Signature.init(.system_v);
        for (call_args.items) |_| {
            try sig.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
        }
        if (v.type_idx != TypeRegistry.VOID and v.type_idx != TypeRegistry.SSA_MEM) {
            try sig.addReturn(self.allocator, clif.AbiParam.init(clif.Type.I64));
        }

        const sig_ref = try self.builder.importSignature(sig);
        const call_result = try self.builder.ins().callIndirect(sig_ref, callee, call_args.items);
        if (call_result.results.len > 0) {
            try self.putValue(v.id, call_result.results[0]);
        }
    }

    fn getOrCreateFuncRef(self: *Self, name: []const u8, n_params: usize, n_returns: usize) !clif.FuncRef {
        if (self.func_refs.get(name)) |ref| return ref;

        // Build signature from callee's actual IR type info.
        // Reference: cg_clif get_function_ref uses callee's fn_abi, not caller's args.
        var sig = clif.Signature.init(.system_v);

        // Try to find callee's IR func for proper type-aware signature
        const callee_ir = self.findIrFunc(name);
        if (callee_ir) |ir_func| {
            // Build signature from callee's actual param/return types
            for (ir_func.params) |param| {
                const ptype = self.type_reg.get(param.type_idx);
                const is_string_or_slice = param.type_idx == TypeRegistry.STRING or ptype == .slice;
                const type_size = self.type_reg.sizeOf(param.type_idx);
                const is_large_struct = (ptype == .struct_type or ptype == .union_type or ptype == .tuple) and type_size > 8;

                if (is_string_or_slice) {
                    try sig.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
                    try sig.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
                } else if (is_large_struct) {
                    const num_slots = @max(1, (type_size + 7) / 8);
                    for (0..num_slots) |_| {
                        try sig.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
                    }
                } else {
                    try sig.addParam(self.allocator, clif.AbiParam.init(self.ssaTypeToClifType(param.type_idx)));
                }
            }
            if (ir_func.return_type != TypeRegistry.VOID) {
                const ret_ptype = self.type_reg.get(ir_func.return_type);
                const is_string_or_slice_ret = ir_func.return_type == TypeRegistry.STRING or ret_ptype == .slice;
                if (is_string_or_slice_ret) {
                    // Compound return: (ptr, len)
                    try sig.addReturn(self.allocator, clif.AbiParam.init(clif.Type.I64));
                    try sig.addReturn(self.allocator, clif.AbiParam.init(clif.Type.I64));
                } else {
                    try sig.addReturn(self.allocator, clif.AbiParam.init(self.ssaTypeToClifType(ir_func.return_type)));
                }
            }
        } else {
            // External/runtime function — fall back to n_params × I64
            for (0..n_params) |_| {
                try sig.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
            }
            for (0..n_returns) |_| {
                try sig.addReturn(self.allocator, clif.AbiParam.init(clif.Type.I64));
            }
        }

        const sig_ref = try self.builder.importSignature(sig);
        // Look up the target function's index in the object file
        const func_idx = self.func_index_map.get(name) orelse 0;
        const func_ref = try self.builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = func_idx } },
            .signature = sig_ref,
            .colocated = true, // Same object file
        });
        try self.func_refs.put(self.allocator, name, func_ref);
        return func_ref;
    }

    fn findIrFunc(self: *const Self, name: []const u8) ?*const ir_mod.Func {
        for (self.ir_funcs) |*f| {
            if (std.mem.eql(u8, f.name, name)) return f;
        }
        return null;
    }

    /// Get the address of the _cot_ctxt global variable as a CLIF Value.
    /// Creates a GlobalValue for the symbol and emits a global_value instruction.
    /// Used by closure_call (store context) and wasm_global_get(1) (load context).
    fn getCtxtAddr(self: *Self) !clif.Value {
        const gv = try self.clif_func.createGlobalValue(GlobalValueData{
            .symbol = .{
                .name = gv_ExternalName.initUser(0, self.ctxt_symbol_idx),
                .offset = 0,
                .colocated = true,
                .tls = false,
            },
        });
        return try self.builder.ins().globalValue(clif.Type.I64, gv);
    }

    // ========================================================================
    // Helper: emit binary op (I64)
    // ========================================================================

    fn emitBinary(self: *Self, v: *const ssa_value.Value, comptime opcode: clif.Opcode) !void {
        const lhs = self.getClif(v.args[0]);
        const rhs = self.getClif(v.args[1]);
        const ins = self.builder.ins();
        const result = switch (opcode) {
            .iadd => try ins.iadd(lhs, rhs),
            .isub => try ins.isub(lhs, rhs),
            .imul => try ins.imul(lhs, rhs),
            .sdiv => try ins.sdiv(lhs, rhs),
            .udiv => try ins.udiv(lhs, rhs),
            .srem => try ins.srem(lhs, rhs),
            .urem => try ins.urem(lhs, rhs),
            .band => try ins.band(lhs, rhs),
            .bor => try ins.bor(lhs, rhs),
            .bxor => try ins.bxor(lhs, rhs),
            .ishl => try ins.ishl(lhs, rhs),
            .ushr => try ins.ushr(lhs, rhs),
            .sshr => try ins.sshr(lhs, rhs),
            else => unreachable,
        };
        try self.putValue(v.id, result);
    }

    fn emitBinary32(self: *Self, v: *const ssa_value.Value, comptime opcode: clif.Opcode) !void {
        // For 32-bit ops, the values are still I64 in SSA, so we truncate, operate, extend
        const lhs = self.getClif(v.args[0]);
        const rhs = self.getClif(v.args[1]);
        const ins = self.builder.ins();
        const lhs32 = try ins.ireduce(clif.Type.I32, lhs);
        const rhs32 = try ins.ireduce(clif.Type.I32, rhs);
        const result32 = switch (opcode) {
            .iadd => try ins.iadd(lhs32, rhs32),
            .isub => try ins.isub(lhs32, rhs32),
            .imul => try ins.imul(lhs32, rhs32),
            .band => try ins.band(lhs32, rhs32),
            .bor => try ins.bor(lhs32, rhs32),
            .bxor => try ins.bxor(lhs32, rhs32),
            .ishl => try ins.ishl(lhs32, rhs32),
            .ushr => try ins.ushr(lhs32, rhs32),
            .sshr => try ins.sshr(lhs32, rhs32),
            else => unreachable,
        };
        const result = try ins.uextend(clif.Type.I64, result32);
        try self.putValue(v.id, result);
    }

    fn emitFloatBinary(self: *Self, v: *const ssa_value.Value, comptime opcode: clif.Opcode, ty: clif.Type) !void {
        const lhs = self.getClif(v.args[0]);
        const rhs = self.getClif(v.args[1]);
        const ins = self.builder.ins();
        const result = switch (opcode) {
            .fadd => try ins.fadd(ty, lhs, rhs),
            .fsub => try ins.fsub(ty, lhs, rhs),
            .fmul => try ins.fmul(ty, lhs, rhs),
            .fdiv => try ins.fdiv(ty, lhs, rhs),
            else => unreachable,
        };
        try self.putValue(v.id, result);
    }

    // ========================================================================
    // Helper: emit comparison
    // ========================================================================

    fn emitCompare(self: *Self, v: *const ssa_value.Value, cond: clif.IntCC) !void {
        const lhs = self.getClif(v.args[0]);
        const rhs = self.getClif(v.args[1]);
        const result = try self.builder.ins().icmp(cond, lhs, rhs);
        try self.putValue(v.id, result);
    }

    fn emitFloatCompare(self: *Self, v: *const ssa_value.Value, cond: clif.FloatCC) !void {
        const lhs = self.getClif(v.args[0]);
        const rhs = self.getClif(v.args[1]);
        const result = try self.builder.ins().fcmp(cond, lhs, rhs);
        try self.putValue(v.id, result);
    }

    // ========================================================================
    // Helper: emit conversion
    // ========================================================================

    fn emitConvert(self: *Self, v: *const ssa_value.Value, comptime opcode: clif.Opcode, target_ty: clif.Type) !void {
        const arg = self.getClif(v.args[0]);
        const ins = self.builder.ins();
        const result = switch (opcode) {
            .sextend => try ins.sextend(target_ty, arg),
            .uextend => try ins.uextend(target_ty, arg),
            .ireduce => try ins.ireduce(target_ty, arg),
            .fcvt_from_sint => try ins.fcvtFromSint(target_ty, arg),
            .fcvt_to_sint => try ins.fcvtToSint(target_ty, arg),
            .fpromote => try ins.fpromote(target_ty, arg),
            .fdemote => try ins.fdemote(target_ty, arg),
            else => unreachable,
        };
        try self.putValue(v.id, result);
    }

    // ========================================================================
    // Helper: generic convert (int↔float, int↔int)
    // Reference: gen.zig:1086-1131 for Wasm codegen's type dispatch
    // ========================================================================

    fn emitGenericConvert(self: *Self, v: *const ssa_value.Value) !void {
        const arg = self.getClif(v.args[0]);
        const from_type = v.aux.type_ref;
        const to_type = v.type_idx;
        const ins = self.builder.ins();

        const from_is_float = (from_type == TypeRegistry.F32 or from_type == TypeRegistry.F64 or from_type == TypeRegistry.UNTYPED_FLOAT);
        const to_is_float = (to_type == TypeRegistry.F32 or to_type == TypeRegistry.F64 or to_type == TypeRegistry.UNTYPED_FLOAT);
        const to_clif = self.ssaTypeToClifType(to_type);

        if (from_is_float and !to_is_float) {
            // float → int: fcvt_to_sint
            const result = try ins.fcvtToSint(to_clif, arg);
            try self.putValue(v.id, result);
        } else if (!from_is_float and to_is_float) {
            // int → float: fcvt_from_sint
            const result = try ins.fcvtFromSint(to_clif, arg);
            try self.putValue(v.id, result);
        } else if (from_is_float and to_is_float) {
            // float → float: promote or demote
            if (to_type == TypeRegistry.F32) {
                const result = try ins.fdemote(clif.Type.F32, arg);
                try self.putValue(v.id, result);
            } else {
                const result = try ins.fpromote(clif.Type.F64, arg);
                try self.putValue(v.id, result);
            }
        } else {
            // int → int: extend or reduce based on sizes
            const from_clif = self.ssaTypeToClifType(from_type);
            const from_bits = from_clif.bits();
            const to_bits = to_clif.bits();
            if (from_bits < to_bits) {
                const result = try ins.sextend(to_clif, arg);
                try self.putValue(v.id, result);
            } else if (from_bits > to_bits) {
                const result = try ins.ireduce(to_clif, arg);
                try self.putValue(v.id, result);
            } else {
                // Same size — no conversion needed
                try self.putValue(v.id, arg);
            }
        }
    }

    // ========================================================================
    // Value mapping helpers
    // ========================================================================

    fn getClif(self: *const Self, ssa_val: *const ssa_value.Value) clif.Value {
        return self.value_map.get(ssa_val.id) orelse {
            // Value not yet translated — this can happen for forward references.
            // Return a sentinel value; the caller should handle this.
            debug.log(.codegen, "ssa_to_clif: missing value v{d} (op={s})", .{ ssa_val.id, @tagName(ssa_val.op) });
            return clif.Value.fromIndex(0);
        };
    }

    fn putValue(self: *Self, id: ssa_value.ID, val: clif.Value) !void {
        try self.value_map.put(self.allocator, id, val);
    }

    // ========================================================================
    // Type mapping: SSA TypeIndex → CLIF Type
    // ========================================================================

    fn ssaTypeToClifType(self: *const Self, idx: TypeIndex) clif.Type {
        if (idx == TypeRegistry.F32) return clif.Type.F32;
        if (idx == TypeRegistry.F64 or idx == TypeRegistry.UNTYPED_FLOAT) return clif.Type.F64;
        if (idx == TypeRegistry.BOOL or idx == TypeRegistry.UNTYPED_BOOL) return clif.Type.I8;
        if (idx == TypeRegistry.I8 or idx == TypeRegistry.U8) return clif.Type.I8;
        if (idx == TypeRegistry.I16 or idx == TypeRegistry.U16) return clif.Type.I16;
        if (idx == TypeRegistry.I32 or idx == TypeRegistry.U32) return clif.Type.I32;

        // Check composite types that might be float
        const t = self.type_reg.get(idx);
        switch (t) {
            .basic => |k| {
                if (k.isFloat()) return clif.Type.F64;
                if (k == .i32_type or k == .u32_type) return clif.Type.I32;
                if (k == .i16_type or k == .u16_type) return clif.Type.I16;
                if (k == .i8_type or k == .u8_type or k == .bool_type) return clif.Type.I8;
            },
            else => {},
        }

        // Default: everything else is a 64-bit integer (pointers, i64, etc.)
        return clif.Type.I64;
    }
};
