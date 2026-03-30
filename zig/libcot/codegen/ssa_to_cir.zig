//! SSA → CIR Direct Translator
//!
//! Translates SSA functions directly to CIR binary bytes, bypassing CLIF IR entirely.
//! This replaces two files:
//!   1. codegen/native/ssa_to_clif.zig (SSA → CLIF IR)
//!   2. codegen/native/cir_write.zig   (CLIF IR → CIR bytes)
//!
//! For each SSA op, the corresponding CIR opcode is emitted directly into a byte buffer.
//! The output is a complete CIR binary blob ready for the Rust Cranelift translator.
//!
//! CIR format: SPIR-V style, 32-bit word-aligned instructions.
//! See claude/CIR_FORMAT_SPEC.md for full specification.

const std = @import("std");
const Allocator = std.mem.Allocator;

// SSA types
const ssa_mod = @import("../ssa/func.zig");
const ssa_block = @import("../ssa/block.zig");
const ssa_value = @import("../ssa/value.zig");
const Op = @import("../ssa/op.zig").Op;

// Frontend types
const types_mod = @import("../frontend/types.zig");
const TypeIndex = types_mod.TypeIndex;
const TypeRegistry = types_mod.TypeRegistry;
const ir_mod = @import("../frontend/ir.zig");

const debug = @import("../pipeline_debug.zig");

// ============================================================================
// CIR Constants (must match rust/libclif/src/cir.rs)
// ============================================================================

const MAGIC: u32 = 0x00434952; // "CIR\0"
const VERSION_1_0: u32 = 0x00010000;

const SECTION_STRING_HEAP: u16 = 0x01;
const SECTION_FUNC_DEFS: u16 = 0x06;

// Structure markers
const FUNC_BEGIN: u16 = 0xFF00;
const FUNC_END: u16 = 0xFF01;
const BLOCK_BEGIN: u16 = 0xFF02;
const BLOCK_END: u16 = 0xFF03;

// Opcodes — constants
const OP_CONST_BOOL: u16 = 0x0000;
const OP_CONST_INT: u16 = 0x0001;
const OP_CONST_FLOAT: u16 = 0x0002;

// Opcodes — integer arithmetic
const OP_ADD: u16 = 0x0010;
const OP_SUB: u16 = 0x0011;
const OP_MUL: u16 = 0x0012;
const OP_DIV: u16 = 0x0013;
const OP_UDIV: u16 = 0x0014;
const OP_MOD: u16 = 0x0015;
const OP_UMOD: u16 = 0x0016;
const OP_INEG: u16 = 0x0017;

// Opcodes — bitwise
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

// Opcodes — integer comparison
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

// Opcodes — type conversion
const OP_UEXTEND: u16 = 0x0040;
const OP_SEXTEND: u16 = 0x0041;
const OP_IREDUCE: u16 = 0x0042;
const OP_FCVT_FROM_SINT: u16 = 0x0043;
const OP_FCVT_TO_SINT: u16 = 0x0044;
const OP_FCVT_FROM_UINT: u16 = 0x0045;
const OP_FPROMOTE: u16 = 0x0046;
const OP_FDEMOTE: u16 = 0x0047;
const OP_FCVT_TO_UINT: u16 = 0x0048;
const OP_BITCAST: u16 = 0x0049;

// Opcodes — float arithmetic
const OP_ADD_F: u16 = 0x0050;
const OP_SUB_F: u16 = 0x0051;
const OP_MUL_F: u16 = 0x0052;
const OP_DIV_F: u16 = 0x0053;
const OP_NEG_F: u16 = 0x0054;
const OP_FABS: u16 = 0x0055;
const OP_SQRT: u16 = 0x0056;
const OP_CEIL: u16 = 0x0057;
const OP_FLOOR: u16 = 0x0058;
const OP_TRUNC: u16 = 0x0059;
const OP_NEAREST: u16 = 0x005A;
const OP_FMIN: u16 = 0x005B;
const OP_FMAX: u16 = 0x005C;
const OP_FCOPYSIGN: u16 = 0x005D;

// Opcodes — float comparison
const OP_EQ_F: u16 = 0x0060;
const OP_NE_F: u16 = 0x0061;
const OP_LT_F: u16 = 0x0062;
const OP_LE_F: u16 = 0x0063;
const OP_GT_F: u16 = 0x0064;
const OP_GE_F: u16 = 0x0065;

// Opcodes — memory
const OP_LOAD: u16 = 0x0070;
const OP_STORE: u16 = 0x0071;
const OP_LOCAL_ADDR: u16 = 0x0080;
const OP_GLOBAL_VALUE: u16 = 0x0081;

// Opcodes — control flow (SSA)
const OP_COPY: u16 = 0x0091;
const OP_ARG: u16 = 0x0092;
const OP_STATIC_CALL: u16 = 0x0093;
const OP_CALL_INDIRECT: u16 = 0x0094;
const OP_FUNC_ADDR: u16 = 0x0095;
const OP_RET: u16 = 0x0097;
const OP_RET_VOID: u16 = 0x0098;
const OP_COND_SELECT: u16 = 0x0099;

// Opcodes — control flow (CFG)
const OP_JUMP: u16 = 0x00A0;
const OP_BRIF: u16 = 0x00A1;
const OP_TRAP: u16 = 0x00A2;
const OP_COND_TRAP: u16 = 0x00A3;
const OP_BR_TABLE: u16 = 0x00A4;

// Opcodes — declarations
const OP_STACK_SLOT_DECL: u16 = 0x00B0;
const OP_GLOBAL_VALUE_SYMBOL: u16 = 0x00B1;
const OP_GLOBAL_VALUE_IADD: u16 = 0x00B2;

// CIR type indices (match pre-registered types in spec)
const CIR_INVALID: u32 = 0;
const CIR_BOOL: u32 = 1;
const CIR_I8: u32 = 2;
const CIR_I16: u32 = 3;
const CIR_I32: u32 = 4;
const CIR_I64: u32 = 5;
const CIR_U8: u32 = 6;
const CIR_U16: u32 = 7;
const CIR_U32: u32 = 8;
const CIR_U64: u32 = 9;
const CIR_F32: u32 = 10;
const CIR_F64: u32 = 11;
const CIR_VOID: u32 = 12;

// ============================================================================
// Public API
// ============================================================================

/// Translate a collection of SSA functions directly to CIR binary bytes.
///
/// This function:
/// 1. For each SSA function, emits CIR instructions for each SSA value
/// 2. Returns the complete CIR binary blob
///
/// The caller is responsible for running SSA passes before calling this.
pub fn translateModule(
    allocator: Allocator,
    funcs: []const ir_mod.Func,
    ssa_funcs: []const *ssa_mod.Func,
    type_reg: *const TypeRegistry,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    string_data_symbol_idx: ?u32,
    ctxt_symbol_idx: u32,
    global_symbol_map: *const std.StringHashMapUnmanaged(u32),
) ![]const u8 {
    var writer = CirWriter.init(allocator);
    defer writer.deinit();

    // Build reverse map: func_index → name
    var reverse_map = std.AutoHashMapUnmanaged(u32, []const u8){};
    defer reverse_map.deinit(allocator);
    {
        var it = func_index_map.iterator();
        while (it.next()) |entry| {
            try reverse_map.put(allocator, entry.value_ptr.*, entry.key_ptr.*);
        }
    }

    for (ssa_funcs, funcs) |ssa_func, ir_func| {
        var translator = SsaToCirTranslator.init(
            allocator,
            ssa_func,
            type_reg,
            ir_func.params,
            ir_func.return_type,
            func_index_map,
            funcs,
            string_data_symbol_idx,
            ctxt_symbol_idx,
            global_symbol_map,
            &reverse_map,
        );
        defer translator.deinit();
        try translator.run(&writer);
    }

    return writer.finish();
}

/// Translate a single SSA function to CIR bytes, appending to an existing writer.
pub fn translateFunc(
    allocator: Allocator,
    ssa_func: *const ssa_mod.Func,
    writer: *CirWriter,
    type_reg: *const TypeRegistry,
    ir_params: []const ir_mod.Local,
    ir_return_type: TypeIndex,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    ir_funcs: []const ir_mod.Func,
    string_data_symbol_idx: ?u32,
    ctxt_symbol_idx: u32,
    global_symbol_map: *const std.StringHashMapUnmanaged(u32),
    reverse_map: *const std.AutoHashMapUnmanaged(u32, []const u8),
) !void {
    var translator = SsaToCirTranslator.init(
        allocator,
        ssa_func,
        type_reg,
        ir_params,
        ir_return_type,
        func_index_map,
        ir_funcs,
        string_data_symbol_idx,
        ctxt_symbol_idx,
        global_symbol_map,
        reverse_map,
    );
    defer translator.deinit();
    try translator.run(writer);
}

// ============================================================================
// CIR Binary Writer
// ============================================================================

pub const CirWriter = struct {
    /// Function definition words — written during serialization.
    /// String heap is assembled separately and prepended at finish() time.
    words: std.ArrayListUnmanaged(u32),
    string_heap: std.ArrayListUnmanaged(u8),
    string_offsets: std.StringHashMapUnmanaged(u32),
    allocator: Allocator,

    pub fn init(allocator: Allocator) CirWriter {
        return .{
            .words = .{},
            .string_heap = .{},
            .string_offsets = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *CirWriter) void {
        self.words.deinit(self.allocator);
        self.string_heap.deinit(self.allocator);
        self.string_offsets.deinit(self.allocator);
    }

    /// Intern a string, returning its offset in the string heap.
    pub fn internString(self: *CirWriter, s: []const u8) u32 {
        if (self.string_offsets.get(s)) |off| return off;

        const offset: u32 = @intCast(self.string_heap.items.len);
        const len: u32 = @intCast(s.len);

        // Write length + bytes + padding
        self.string_heap.appendSlice(self.allocator, std.mem.asBytes(&len)) catch return 0;
        self.string_heap.appendSlice(self.allocator, s) catch return 0;
        while (self.string_heap.items.len % 4 != 0) {
            self.string_heap.append(self.allocator, 0) catch {};
        }

        self.string_offsets.put(self.allocator, s, offset) catch {};
        return offset;
    }

    fn emitRaw(self: *CirWriter, opcode: u16, operands: []const u32) void {
        const word_count: u32 = @intCast(1 + operands.len);
        self.words.append(self.allocator, (word_count << 16) | @as(u32, opcode)) catch {};
        for (operands) |op| self.words.append(self.allocator, op) catch {};
    }

    pub fn emit(self: *CirWriter, opcode: u16, operands: []const u32) void {
        self.emitRaw(opcode, operands);
    }

    pub fn beginFuncWithSig(self: *CirWriter, name_offset: u32, param_types: []const u32, return_types: []const u32, block_count: u32, flags: u32) void {
        var operands_buf: [128]u32 = undefined;
        operands_buf[0] = name_offset;
        operands_buf[1] = @intCast(param_types.len);
        operands_buf[2] = @intCast(return_types.len);
        operands_buf[3] = block_count;
        operands_buf[4] = flags;
        var pos: usize = 5;
        for (param_types) |pt| {
            operands_buf[pos] = pt;
            pos += 1;
        }
        for (return_types) |rt| {
            operands_buf[pos] = rt;
            pos += 1;
        }
        self.emitRaw(FUNC_BEGIN, operands_buf[0..pos]);
    }

    pub fn endFunc(self: *CirWriter) void {
        self.words.append(self.allocator, (@as(u32, 1) << 16) | @as(u32, FUNC_END)) catch {};
    }

    pub fn beginBlock(self: *CirWriter, id: u32, kind: u8, successors: []const u32, predecessors: []const u32) void {
        const word_count: u32 = @intCast(5 + successors.len + predecessors.len);
        self.words.append(self.allocator, (word_count << 16) | @as(u32, BLOCK_BEGIN)) catch {};
        self.words.append(self.allocator, id) catch {};
        self.words.append(self.allocator, kind) catch {};
        self.words.append(self.allocator, @intCast(successors.len)) catch {};
        for (successors) |s| self.words.append(self.allocator, s) catch {};
        self.words.append(self.allocator, @intCast(predecessors.len)) catch {};
        for (predecessors) |p| self.words.append(self.allocator, p) catch {};
    }

    pub fn endBlock(self: *CirWriter) void {
        self.words.append(self.allocator, (@as(u32, 1) << 16) | @as(u32, BLOCK_END)) catch {};
    }

    /// Finalize and return the CIR bytes.
    /// Assembles: [header] [string_heap_section] [func_defs_section]
    pub fn finish(self: *CirWriter) []const u8 {
        var out = std.ArrayListUnmanaged(u32){};

        // Header (5 words)
        out.append(self.allocator, MAGIC) catch {};
        out.append(self.allocator, VERSION_1_0) catch {};
        out.append(self.allocator, 0) catch {}; // generator
        out.append(self.allocator, 0) catch {}; // bound
        out.append(self.allocator, 0) catch {}; // reserved

        // String heap section
        const heap_bytes = self.string_heap.items.len;
        const heap_words = (heap_bytes + 3) / 4;
        const heap_section_wc: u32 = @intCast(2 + heap_words);
        out.append(self.allocator, (heap_section_wc << 16) | @as(u32, SECTION_STRING_HEAP)) catch {};
        out.append(self.allocator, @intCast(heap_bytes)) catch {};
        {
            var i: usize = 0;
            while (i < heap_bytes) : (i += 4) {
                var word: [4]u8 = .{ 0, 0, 0, 0 };
                const remaining = @min(4, heap_bytes - i);
                @memcpy(word[0..remaining], self.string_heap.items[i..][0..remaining]);
                out.append(self.allocator, std.mem.bytesAsValue(u32, &word).*) catch {};
            }
        }

        // Func defs section (word count 0 = read until end)
        out.append(self.allocator, @as(u32, SECTION_FUNC_DEFS)) catch {};
        out.appendSlice(self.allocator, self.words.items) catch {};

        return std.mem.sliceAsBytes(out.items);
    }
};

// ============================================================================
// SSA → CIR Translator
// ============================================================================

const SsaToCirTranslator = struct {
    allocator: Allocator,
    ssa_func: *const ssa_mod.Func,
    type_reg: *const TypeRegistry,

    /// IR function parameter info — used for building CIR signature.
    ir_params: []const ir_mod.Local,
    ir_return_type: TypeIndex,

    /// Function name → object file index map for call relocations.
    func_index_map: *const std.StringHashMapUnmanaged(u32),

    /// All IR functions — used to look up callee signatures for calls.
    ir_funcs: []const ir_mod.Func,

    /// Reverse map: func_index → name
    reverse_map: *const std.AutoHashMapUnmanaged(u32, []const u8),

    /// SSA value ID → CIR result ID (u32).
    /// For most values, result_id == SSA value ID. But for compound types
    /// (string, slice, optional), the primary component maps here.
    value_map: std.AutoHashMapUnmanaged(ssa_value.ID, u32),

    /// Secondary value map for compound types (strings, slices, optionals).
    /// Maps string_make/slice_make/opt_make SSA IDs to their length/extra component result ID.
    compound_extra_map: std.AutoHashMapUnmanaged(ssa_value.ID, u32),

    /// Tracks SSA value IDs that are string data offsets (args[0] of string_make).
    string_offset_ids: std.AutoHashMapUnmanaged(ssa_value.ID, void),

    /// External name index for string data section symbol.
    string_data_symbol_idx: ?u32,

    /// External name index for CTXT global variable (closure context pointer).
    ctxt_symbol_idx: u32,

    /// Global variable name → external name index for data section symbols.
    global_symbol_map: *const std.StringHashMapUnmanaged(u32),

    /// Stack slot offset → CIR slot index. Tracks unique stack slot declarations.
    stack_slot_map: std.AutoHashMapUnmanaged(u32, u32),

    /// Counter for unique synthetic value IDs (for decomposing offsets, etc.)
    next_synthetic_id: u32,

    /// Global value counter for this function.
    next_gv_id: u32,

    /// Set when a value-level terminator is emitted mid-block.
    block_terminated: bool,

    const Self = @This();

    fn init(
        allocator: Allocator,
        ssa_func: *const ssa_mod.Func,
        type_reg: *const TypeRegistry,
        ir_params: []const ir_mod.Local,
        ir_return_type: TypeIndex,
        func_index_map: *const std.StringHashMapUnmanaged(u32),
        ir_funcs: []const ir_mod.Func,
        string_data_symbol_idx: ?u32,
        ctxt_symbol_idx: u32,
        global_symbol_map: *const std.StringHashMapUnmanaged(u32),
        reverse_map: *const std.AutoHashMapUnmanaged(u32, []const u8),
    ) Self {
        return .{
            .allocator = allocator,
            .ssa_func = ssa_func,
            .type_reg = type_reg,
            .ir_params = ir_params,
            .ir_return_type = ir_return_type,
            .func_index_map = func_index_map,
            .ir_funcs = ir_funcs,
            .reverse_map = reverse_map,
            .value_map = .{},
            .compound_extra_map = .{},
            .string_offset_ids = .{},
            .string_data_symbol_idx = string_data_symbol_idx,
            .ctxt_symbol_idx = ctxt_symbol_idx,
            .global_symbol_map = global_symbol_map,
            .stack_slot_map = .{},
            .next_synthetic_id = 0xC0000000,
            .next_gv_id = 0,
            .block_terminated = false,
        };
    }

    fn deinit(self: *Self) void {
        self.value_map.deinit(self.allocator);
        self.compound_extra_map.deinit(self.allocator);
        self.string_offset_ids.deinit(self.allocator);
        self.stack_slot_map.deinit(self.allocator);
    }

    fn nextSyntheticId(self: *Self) u32 {
        const id = self.next_synthetic_id;
        self.next_synthetic_id += 1;
        return id;
    }

    fn nextGvId(self: *Self) u32 {
        const id = self.next_gv_id;
        self.next_gv_id += 1;
        return id;
    }

    // ========================================================================
    // Main translation loop
    // ========================================================================

    fn run(self: *Self, writer: *CirWriter) !void {
        const ssa_func = self.ssa_func;
        debug.log(.codegen, "ssa_to_cir: translating '{s}' ({d} blocks)", .{
            ssa_func.name, ssa_func.blocks.items.len,
        });

        // Reset per-function state
        self.next_synthetic_id = 0xC0000000;
        self.next_gv_id = 0;

        // Build function signature
        var param_types = std.ArrayListUnmanaged(u32){};
        defer param_types.deinit(self.allocator);
        var return_types = std.ArrayListUnmanaged(u32){};
        defer return_types.deinit(self.allocator);
        try self.buildSignatureTypes(&param_types, &return_types);

        // Determine function name for CIR
        const func_name = if (std.mem.eql(u8, ssa_func.name, "main")) "_cot_main" else ssa_func.name;
        const name_offset = writer.internString(func_name);
        const flags: u32 = if (ssa_func.is_export) 2 else 0; // bit 1 = export

        writer.beginFuncWithSig(
            name_offset,
            param_types.items,
            return_types.items,
            @intCast(ssa_func.blocks.items.len),
            flags,
        );

        // Pre-scan for string_make ops to track string offset IDs.
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

        // Create stack slots and emit declarations
        try self.createStackSlots(writer);

        // Emit global value declarations
        try self.emitGlobalValueDecls(writer);

        // Translate each block
        for (ssa_func.blocks.items, 0..) |ssa_block_ptr, block_idx| {
            const kind: u8 = if (block_idx == 0) 0x05 else 0x00; // entry or plain
            writer.beginBlock(@intCast(block_idx), kind, &.{}, &.{});

            // Entry block: emit stack slot declarations + OP_ARGs for params
            if (block_idx == 0) {
                try self.emitStackSlotDecls(writer);
                try self.emitParamArgs(writer);
            }

            // Translate all values in this block
            self.block_terminated = false;
            for (ssa_block_ptr.values.items) |v| {
                try self.translateValue(v, writer);
                if (self.block_terminated) break;
            }

            // Emit terminator
            if (!self.block_terminated) {
                try self.emitTerminator(ssa_block_ptr, @intCast(block_idx), writer);
            }

            writer.endBlock();
        }

        writer.endFunc();

        debug.log(.codegen, "ssa_to_cir: done '{s}'", .{ssa_func.name});
    }

    // ========================================================================
    // Signature building
    // ========================================================================

    fn buildSignatureTypes(self: *Self, param_types: *std.ArrayListUnmanaged(u32), return_types: *std.ArrayListUnmanaged(u32)) !void {
        for (self.ir_params) |param| {
            const ptype = self.type_reg.get(param.type_idx);
            const is_string_or_slice = param.type_idx == TypeRegistry.STRING or ptype == .slice;
            const type_size = self.type_reg.sizeOf(param.type_idx);
            const is_opt_ptr = ptype == .optional and blk: {
                const ei = self.type_reg.get(ptype.optional.elem);
                break :blk ei == .pointer and ei.pointer.managed;
            };
            const is_compound_opt = ptype == .optional and self.type_reg.get(ptype.optional.elem) != .pointer and !is_opt_ptr;
            const is_large_struct = (ptype == .struct_type or ptype == .union_type or ptype == .tuple or is_compound_opt) and type_size > 8;

            if (is_opt_ptr or is_string_or_slice) {
                try param_types.append(self.allocator, CIR_I64);
                try param_types.append(self.allocator, CIR_I64);
            } else if (is_large_struct) {
                const num_slots = @max(1, (type_size + 7) / 8);
                for (0..num_slots) |_| {
                    try param_types.append(self.allocator, CIR_I64);
                }
            } else {
                try param_types.append(self.allocator, self.ssaTypeToCirType(param.type_idx));
            }
        }

        if (self.ir_return_type != TypeRegistry.VOID) {
            const ret_type = self.type_reg.get(self.ir_return_type);
            const ret_is_string_or_slice = self.ir_return_type == TypeRegistry.STRING or ret_type == .slice;
            const ret_is_opt_ptr = ret_type == .optional and blk: {
                const ei = self.type_reg.get(ret_type.optional.elem);
                break :blk ei == .pointer and ei.pointer.managed;
            };
            if (ret_is_string_or_slice or ret_is_opt_ptr) {
                try return_types.append(self.allocator, CIR_I64);
                try return_types.append(self.allocator, CIR_I64);
            } else {
                try return_types.append(self.allocator, self.ssaTypeToCirType(self.ir_return_type));
            }
        }
    }

    // ========================================================================
    // Stack slots
    // ========================================================================

    fn createStackSlots(self: *Self, writer: *CirWriter) !void {
        _ = writer;
        const has_precomputed = self.ssa_func.local_slot_offsets.len == self.ssa_func.local_sizes.len and
            self.ssa_func.local_slot_offsets.len > 0;

        // First pass: compute max size needed at each unique slot_offset
        var slot_max_sizes = std.AutoHashMapUnmanaged(u32, u32){};
        defer slot_max_sizes.deinit(self.allocator);

        if (has_precomputed) {
            for (self.ssa_func.local_sizes, self.ssa_func.local_slot_offsets) |size, offset| {
                const actual_size = @max(size, 8);
                const entry = try slot_max_sizes.getOrPut(self.allocator, offset);
                if (entry.found_existing) {
                    entry.value_ptr.* = @max(entry.value_ptr.*, actual_size);
                } else {
                    entry.value_ptr.* = actual_size;
                }
            }
        } else {
            var next_off: u32 = 0;
            for (self.ssa_func.local_sizes) |size| {
                const actual_size = @max(size, 8);
                try slot_max_sizes.put(self.allocator, next_off, actual_size);
                next_off += @max(1, (size + 7) / 8);
            }
        }

        // Second pass: assign sequential slot indices
        if (has_precomputed) {
            var next_slot_idx: u32 = 0;
            for (self.ssa_func.local_slot_offsets) |offset| {
                if (!self.stack_slot_map.contains(offset)) {
                    try self.stack_slot_map.put(self.allocator, offset, next_slot_idx);
                    next_slot_idx += 1;
                }
            }
        } else {
            var next_slot_offset: u32 = 0;
            var next_slot_idx: u32 = 0;
            for (self.ssa_func.local_sizes) |size| {
                try self.stack_slot_map.put(self.allocator, next_slot_offset, next_slot_idx);
                next_slot_idx += 1;
                next_slot_offset += @max(1, (size + 7) / 8);
            }
        }
    }

    fn emitStackSlotDecls(self: *Self, writer: *CirWriter) !void {
        const has_precomputed = self.ssa_func.local_slot_offsets.len == self.ssa_func.local_sizes.len and
            self.ssa_func.local_slot_offsets.len > 0;

        // Collect unique (offset → size, slot_idx) and emit in slot_idx order
        var emitted = std.AutoHashMapUnmanaged(u32, void){};
        defer emitted.deinit(self.allocator);

        if (has_precomputed) {
            for (self.ssa_func.local_sizes, self.ssa_func.local_slot_offsets) |size, offset| {
                if (emitted.contains(offset)) continue;
                try emitted.put(self.allocator, offset, {});
                const slot_idx = self.stack_slot_map.get(offset) orelse continue;
                const actual_size = @max(size, 8);
                const alignment: u32 = if (actual_size <= 1) 1 else if (actual_size <= 2) 2 else if (actual_size <= 4) 4 else 8;
                writer.emit(OP_STACK_SLOT_DECL, &.{ slot_idx, actual_size, alignment });
            }
        } else {
            var next_slot_offset: u32 = 0;
            for (self.ssa_func.local_sizes) |size| {
                const slot_idx = self.stack_slot_map.get(next_slot_offset) orelse continue;
                const actual_size = @max(size, 8);
                const alignment: u32 = if (actual_size <= 1) 1 else if (actual_size <= 2) 2 else if (actual_size <= 4) 4 else 8;
                writer.emit(OP_STACK_SLOT_DECL, &.{ slot_idx, actual_size, alignment });
                next_slot_offset += @max(1, (size + 7) / 8);
            }
        }
    }

    // ========================================================================
    // Global value declarations
    // ========================================================================

    fn emitGlobalValueDecls(self: *Self, writer: *CirWriter) !void {
        // String data symbol
        if (self.string_data_symbol_idx) |sym_idx| {
            const gv_id = self.nextGvId();
            const name = self.reverse_map.get(sym_idx) orelse "__string_data";
            const name_off = writer.internString(name);
            writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 }); // colocated=1
        }

        // CTXT symbol
        {
            const gv_id = self.nextGvId();
            const name = self.reverse_map.get(self.ctxt_symbol_idx) orelse "_cot_ctxt";
            const name_off = writer.internString(name);
            writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 }); // colocated=1
        }
    }

    // ========================================================================
    // Parameter args
    // ========================================================================

    fn emitParamArgs(self: *Self, writer: *CirWriter) !void {
        // Emit OP_ARG for each CLIF-level parameter (compound types decomposed)
        var param_idx: u32 = 0;
        for (self.ir_params) |param| {
            const ptype = self.type_reg.get(param.type_idx);
            const is_string_or_slice = param.type_idx == TypeRegistry.STRING or ptype == .slice;
            const type_size = self.type_reg.sizeOf(param.type_idx);
            const is_opt_ptr = ptype == .optional and blk: {
                const ei = self.type_reg.get(ptype.optional.elem);
                break :blk ei == .pointer and ei.pointer.managed;
            };
            const is_compound_opt = ptype == .optional and self.type_reg.get(ptype.optional.elem) != .pointer and !is_opt_ptr;
            const is_large_struct = (ptype == .struct_type or ptype == .union_type or ptype == .tuple or is_compound_opt) and type_size > 8;

            if (is_opt_ptr or is_string_or_slice) {
                const id1 = self.nextSyntheticId();
                const id2 = self.nextSyntheticId();
                writer.emit(OP_ARG, &.{ id1, CIR_I64, param_idx });
                writer.emit(OP_ARG, &.{ id2, CIR_I64, param_idx + 1 });
                param_idx += 2;
            } else if (is_large_struct) {
                const num_slots = @max(1, (type_size + 7) / 8);
                for (0..num_slots) |_| {
                    const id = self.nextSyntheticId();
                    writer.emit(OP_ARG, &.{ id, CIR_I64, param_idx });
                    param_idx += 1;
                }
            } else {
                const id = self.nextSyntheticId();
                writer.emit(OP_ARG, &.{ id, self.ssaTypeToCirType(param.type_idx), param_idx });
                param_idx += 1;
            }
        }
    }

    // ========================================================================
    // Value translation — main dispatch
    // ========================================================================

    fn translateValue(self: *Self, v: *const ssa_value.Value, writer: *CirWriter) !void {
        const result_id: u32 = v.id;

        switch (v.op) {
            // ============================================================
            // Constants
            // ============================================================
            .const_int, .const_64 => {
                self.emitConstInt(writer, result_id, CIR_I64, v.aux_int);
                try self.putValue(v.id, result_id);
            },
            .const_32 => {
                self.emitConstInt(writer, result_id, CIR_I32, v.aux_int);
                try self.putValue(v.id, result_id);
            },
            .const_16 => {
                self.emitConstInt(writer, result_id, CIR_I16, v.aux_int);
                try self.putValue(v.id, result_id);
            },
            .const_8 => {
                self.emitConstInt(writer, result_id, CIR_I8, v.aux_int);
                try self.putValue(v.id, result_id);
            },
            .const_bool => {
                const imm: i64 = if (v.aux_int != 0) 1 else 0;
                self.emitConstInt(writer, result_id, CIR_I64, imm);
                try self.putValue(v.id, result_id);
            },
            .const_nil => {
                self.emitConstInt(writer, result_id, CIR_I64, 0);
                try self.putValue(v.id, result_id);
            },
            .const_float => {
                const bits: u64 = @bitCast(v.aux_int);
                writer.emit(OP_CONST_FLOAT, &.{ result_id, CIR_F64, @truncate(bits), @truncate(bits >> 32) });
                try self.putValue(v.id, result_id);
            },

            // ============================================================
            // Integer Arithmetic
            // ============================================================
            .add, .add64 => try self.emitBinary(v, OP_ADD, CIR_I64, writer),
            .sub, .sub64 => try self.emitBinary(v, OP_SUB, CIR_I64, writer),
            .mul, .mul64 => try self.emitBinary(v, OP_MUL, CIR_I64, writer),
            .div => try self.emitBinary(v, OP_DIV, CIR_I64, writer),
            .udiv => try self.emitBinary(v, OP_UDIV, CIR_I64, writer),
            .mod => try self.emitBinary(v, OP_MOD, CIR_I64, writer),
            .umod => try self.emitBinary(v, OP_UMOD, CIR_I64, writer),
            .neg => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_INEG, &.{ result_id, CIR_I64, arg });
                try self.putValue(v.id, result_id);
            },

            // 32-bit arithmetic — truncate, operate, extend
            .add32 => try self.emitBinary32(v, OP_ADD, writer),
            .sub32 => try self.emitBinary32(v, OP_SUB, writer),
            .mul32 => try self.emitBinary32(v, OP_MUL, writer),

            // ============================================================
            // Bitwise operations
            // ============================================================
            .and_ => try self.emitBinary(v, OP_AND, CIR_I64, writer),
            .or_ => try self.emitBinary(v, OP_OR, CIR_I64, writer),
            .xor => try self.emitBinary(v, OP_XOR, CIR_I64, writer),
            .shl => try self.emitBinary(v, OP_SHL, CIR_I64, writer),
            .shr => try self.emitBinary(v, OP_SHR, CIR_I64, writer),
            .sar => try self.emitBinary(v, OP_SAR, CIR_I64, writer),
            .not => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_NOT, &.{ result_id, CIR_I64, arg });
                try self.putValue(v.id, result_id);
            },
            .bool_not => {
                // bool_not: arg == 0 → icmp eq + uextend
                const arg = self.getCirId(v.args[0]);
                const zero_id = self.nextSyntheticId();
                self.emitConstInt(writer, zero_id, CIR_I64, 0);
                const cmp_id = self.nextSyntheticId();
                writer.emit(OP_EQ, &.{ cmp_id, CIR_I64, arg, zero_id });
                writer.emit(OP_UEXTEND, &.{ result_id, CIR_I64, cmp_id });
                try self.putValue(v.id, result_id);
            },

            // Sized bitwise
            .and32 => try self.emitBinary32(v, OP_AND, writer),
            .or32 => try self.emitBinary32(v, OP_OR, writer),
            .xor32 => try self.emitBinary32(v, OP_XOR, writer),
            .shl32 => try self.emitBinary32(v, OP_SHL, writer),
            .shr32 => try self.emitBinary32(v, OP_SHR, writer),
            .sar32 => try self.emitBinary32(v, OP_SAR, writer),
            .and64 => try self.emitBinary(v, OP_AND, CIR_I64, writer),
            .or64 => try self.emitBinary(v, OP_OR, CIR_I64, writer),
            .xor64 => try self.emitBinary(v, OP_XOR, CIR_I64, writer),
            .shl64 => try self.emitBinary(v, OP_SHL, CIR_I64, writer),
            .shr64 => try self.emitBinary(v, OP_SHR, CIR_I64, writer),
            .sar64 => try self.emitBinary(v, OP_SAR, CIR_I64, writer),

            // Bit count
            .clz64, .clz32 => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_CLZ, &.{ result_id, CIR_I64, arg });
                try self.putValue(v.id, result_id);
            },
            .ctz64, .ctz32 => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_CTZ, &.{ result_id, CIR_I64, arg });
                try self.putValue(v.id, result_id);
            },
            .popcnt64, .popcnt32 => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_POPCNT, &.{ result_id, CIR_I64, arg });
                try self.putValue(v.id, result_id);
            },

            // ============================================================
            // Float operations
            // ============================================================
            .add64f => try self.emitBinary(v, OP_ADD_F, CIR_F64, writer),
            .sub64f => try self.emitBinary(v, OP_SUB_F, CIR_F64, writer),
            .mul64f => try self.emitBinary(v, OP_MUL_F, CIR_F64, writer),
            .div64f => try self.emitBinary(v, OP_DIV_F, CIR_F64, writer),
            .add32f => try self.emitBinary(v, OP_ADD_F, CIR_F32, writer),
            .sub32f => try self.emitBinary(v, OP_SUB_F, CIR_F32, writer),
            .mul32f => try self.emitBinary(v, OP_MUL_F, CIR_F32, writer),
            .div32f => try self.emitBinary(v, OP_DIV_F, CIR_F32, writer),
            .neg64f => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_NEG_F, &.{ result_id, CIR_F64, arg });
                try self.putValue(v.id, result_id);
            },
            .neg32f => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_NEG_F, &.{ result_id, CIR_F32, arg });
                try self.putValue(v.id, result_id);
            },
            .sqrt64f => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_SQRT, &.{ result_id, CIR_F64, arg });
                try self.putValue(v.id, result_id);
            },
            .sqrt32f => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_SQRT, &.{ result_id, CIR_F32, arg });
                try self.putValue(v.id, result_id);
            },

            // Wasm float builtins — unary
            .wasm_f64_ceil => try self.emitFloatUnary(v, OP_CEIL, CIR_F64, writer),
            .wasm_f64_floor => try self.emitFloatUnary(v, OP_FLOOR, CIR_F64, writer),
            .wasm_f64_trunc => try self.emitFloatUnary(v, OP_TRUNC, CIR_F64, writer),
            .wasm_f64_nearest => try self.emitFloatUnary(v, OP_NEAREST, CIR_F64, writer),
            .wasm_f64_abs => try self.emitFloatUnary(v, OP_FABS, CIR_F64, writer),
            .wasm_f64_neg => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_NEG_F, &.{ result_id, CIR_F64, arg });
                try self.putValue(v.id, result_id);
            },
            .wasm_f64_sqrt => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_SQRT, &.{ result_id, CIR_F64, arg });
                try self.putValue(v.id, result_id);
            },
            .wasm_f32_ceil => try self.emitFloatUnary(v, OP_CEIL, CIR_F32, writer),
            .wasm_f32_floor => try self.emitFloatUnary(v, OP_FLOOR, CIR_F32, writer),
            .wasm_f32_trunc => try self.emitFloatUnary(v, OP_TRUNC, CIR_F32, writer),
            .wasm_f32_nearest => try self.emitFloatUnary(v, OP_NEAREST, CIR_F32, writer),
            .wasm_f32_abs => try self.emitFloatUnary(v, OP_FABS, CIR_F32, writer),
            .wasm_f32_neg => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_NEG_F, &.{ result_id, CIR_F32, arg });
                try self.putValue(v.id, result_id);
            },
            .wasm_f32_sqrt => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_SQRT, &.{ result_id, CIR_F32, arg });
                try self.putValue(v.id, result_id);
            },

            // Wasm reinterpret (bitcast between int and float)
            .wasm_i64_reinterpret_f64 => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_BITCAST, &.{ result_id, CIR_I64, arg });
                try self.putValue(v.id, result_id);
            },
            .wasm_f64_reinterpret_i64 => {
                const arg = self.getCirId(v.args[0]);
                writer.emit(OP_BITCAST, &.{ result_id, CIR_F64, arg });
                try self.putValue(v.id, result_id);
            },

            // Wasm float builtins — binary (min, max, copysign)
            .wasm_f64_min => try self.emitBinary(v, OP_FMIN, CIR_F64, writer),
            .wasm_f64_max => try self.emitBinary(v, OP_FMAX, CIR_F64, writer),
            .wasm_f64_copysign => try self.emitBinary(v, OP_FCOPYSIGN, CIR_F64, writer),
            .wasm_f32_min => try self.emitBinary(v, OP_FMIN, CIR_F32, writer),
            .wasm_f32_max => try self.emitBinary(v, OP_FMAX, CIR_F32, writer),
            .wasm_f32_copysign => try self.emitBinary(v, OP_FCOPYSIGN, CIR_F32, writer),

            // ============================================================
            // Comparisons
            // ============================================================
            .eq, .eq64 => try self.emitCompare(v, OP_EQ, CIR_I64, writer),
            .ne, .ne64 => try self.emitCompare(v, OP_NE, CIR_I64, writer),
            .lt, .lt64 => try self.emitCompare(v, OP_LT, CIR_I64, writer),
            .le, .le64 => try self.emitCompare(v, OP_LE, CIR_I64, writer),
            .gt, .gt64 => try self.emitCompare(v, OP_GT, CIR_I64, writer),
            .ge, .ge64 => try self.emitCompare(v, OP_GE, CIR_I64, writer),
            .ult => try self.emitCompare(v, OP_ULT, CIR_I64, writer),
            .ule => try self.emitCompare(v, OP_ULE, CIR_I64, writer),
            .ugt => try self.emitCompare(v, OP_UGT, CIR_I64, writer),
            .uge => try self.emitCompare(v, OP_UGE, CIR_I64, writer),
            .eq32 => try self.emitCompare(v, OP_EQ, CIR_I32, writer),
            .ne32 => try self.emitCompare(v, OP_NE, CIR_I32, writer),
            .lt32 => try self.emitCompare(v, OP_LT, CIR_I32, writer),
            .le32 => try self.emitCompare(v, OP_LE, CIR_I32, writer),
            .gt32 => try self.emitCompare(v, OP_GT, CIR_I32, writer),
            .ge32 => try self.emitCompare(v, OP_GE, CIR_I32, writer),

            // Float comparisons
            .eq64f => try self.emitCompare(v, OP_EQ_F, CIR_F64, writer),
            .ne64f => try self.emitCompare(v, OP_NE_F, CIR_F64, writer),
            .lt64f => try self.emitCompare(v, OP_LT_F, CIR_F64, writer),
            .le64f => try self.emitCompare(v, OP_LE_F, CIR_F64, writer),
            .gt64f => try self.emitCompare(v, OP_GT_F, CIR_F64, writer),
            .ge64f => try self.emitCompare(v, OP_GE_F, CIR_F64, writer),
            .eq32f => try self.emitCompare(v, OP_EQ_F, CIR_F32, writer),
            .ne32f => try self.emitCompare(v, OP_NE_F, CIR_F32, writer),
            .lt32f => try self.emitCompare(v, OP_LT_F, CIR_F32, writer),
            .le32f => try self.emitCompare(v, OP_LE_F, CIR_F32, writer),
            .gt32f => try self.emitCompare(v, OP_GT_F, CIR_F32, writer),
            .ge32f => try self.emitCompare(v, OP_GE_F, CIR_F32, writer),

            // ============================================================
            // Type conversions
            // ============================================================
            .sign_ext8to16 => try self.emitConvert(v, OP_SEXTEND, CIR_I16, writer),
            .sign_ext8to32 => try self.emitConvert(v, OP_SEXTEND, CIR_I32, writer),
            .sign_ext8to64 => try self.emitConvert(v, OP_SEXTEND, CIR_I64, writer),
            .sign_ext16to32 => try self.emitConvert(v, OP_SEXTEND, CIR_I32, writer),
            .sign_ext16to64 => try self.emitConvert(v, OP_SEXTEND, CIR_I64, writer),
            .sign_ext32to64 => try self.emitConvert(v, OP_SEXTEND, CIR_I64, writer),
            .zero_ext8to16 => try self.emitConvert(v, OP_UEXTEND, CIR_I16, writer),
            .zero_ext8to32 => try self.emitConvert(v, OP_UEXTEND, CIR_I32, writer),
            .zero_ext8to64 => try self.emitConvert(v, OP_UEXTEND, CIR_I64, writer),
            .zero_ext16to32 => try self.emitConvert(v, OP_UEXTEND, CIR_I32, writer),
            .zero_ext16to64 => try self.emitConvert(v, OP_UEXTEND, CIR_I64, writer),
            .zero_ext32to64 => try self.emitConvert(v, OP_UEXTEND, CIR_I64, writer),
            .trunc16to8 => try self.emitConvert(v, OP_IREDUCE, CIR_I8, writer),
            .trunc32to8 => try self.emitConvert(v, OP_IREDUCE, CIR_I8, writer),
            .trunc32to16 => try self.emitConvert(v, OP_IREDUCE, CIR_I16, writer),
            .trunc64to8 => try self.emitConvert(v, OP_IREDUCE, CIR_I8, writer),
            .trunc64to16 => try self.emitConvert(v, OP_IREDUCE, CIR_I16, writer),
            .trunc64to32 => try self.emitConvert(v, OP_IREDUCE, CIR_I32, writer),

            // Float conversions
            .cvt64to64f => try self.emitConvert(v, OP_FCVT_FROM_SINT, CIR_F64, writer),
            .cvt32to64f => try self.emitConvert(v, OP_FCVT_FROM_SINT, CIR_F64, writer),
            .cvt64to32f => try self.emitConvert(v, OP_FCVT_FROM_SINT, CIR_F32, writer),
            .cvt32to32f => try self.emitConvert(v, OP_FCVT_FROM_SINT, CIR_F32, writer),
            .cvt64fto64 => try self.emitConvert(v, OP_FCVT_TO_SINT, CIR_I64, writer),
            .cvt64fto32 => try self.emitConvert(v, OP_FCVT_TO_SINT, CIR_I32, writer),
            .cvt32fto64 => try self.emitConvert(v, OP_FCVT_TO_SINT, CIR_I64, writer),
            .cvt32fto32 => try self.emitConvert(v, OP_FCVT_TO_SINT, CIR_I32, writer),
            .cvt32fto64f => try self.emitConvert(v, OP_FPROMOTE, CIR_F64, writer),
            .cvt64fto32f => try self.emitConvert(v, OP_FDEMOTE, CIR_F32, writer),

            // Generic convert
            .convert => try self.emitGenericConvert(v, writer),

            // ============================================================
            // Memory operations
            // ============================================================
            .load, .load64 => {
                const addr = self.getCirId(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const ty = self.ssaTypeToCirType(v.type_idx);
                if (ty == CIR_F32 or ty == CIR_F64) {
                    self.emitLoadWithOffset(writer, result_id, ty, addr, offset);
                } else if (ty != CIR_I64 and ty != CIR_VOID) {
                    // Narrow integer: load, then zero-extend to I64
                    const load_id = self.nextSyntheticId();
                    self.emitLoadWithOffset(writer, load_id, ty, addr, offset);
                    writer.emit(OP_UEXTEND, &.{ result_id, CIR_I64, load_id });
                } else {
                    self.emitLoadWithOffset(writer, result_id, CIR_I64, addr, offset);
                }
                try self.putValue(v.id, result_id);
            },
            .load32 => {
                const addr = self.getCirId(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const load_id = self.nextSyntheticId();
                self.emitLoadWithOffset(writer, load_id, CIR_I32, addr, offset);
                writer.emit(OP_UEXTEND, &.{ result_id, CIR_I64, load_id });
                try self.putValue(v.id, result_id);
            },
            .load32s => {
                const addr = self.getCirId(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const load_id = self.nextSyntheticId();
                self.emitLoadWithOffset(writer, load_id, CIR_I32, addr, offset);
                writer.emit(OP_SEXTEND, &.{ result_id, CIR_I64, load_id });
                try self.putValue(v.id, result_id);
            },
            .load16 => {
                const addr = self.getCirId(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const load_id = self.nextSyntheticId();
                self.emitLoadWithOffset(writer, load_id, CIR_I16, addr, offset);
                writer.emit(OP_UEXTEND, &.{ result_id, CIR_I64, load_id });
                try self.putValue(v.id, result_id);
            },
            .load16s => {
                const addr = self.getCirId(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const load_id = self.nextSyntheticId();
                self.emitLoadWithOffset(writer, load_id, CIR_I16, addr, offset);
                writer.emit(OP_SEXTEND, &.{ result_id, CIR_I64, load_id });
                try self.putValue(v.id, result_id);
            },
            .load8 => {
                const addr = self.getCirId(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const load_id = self.nextSyntheticId();
                self.emitLoadWithOffset(writer, load_id, CIR_I8, addr, offset);
                writer.emit(OP_UEXTEND, &.{ result_id, CIR_I64, load_id });
                try self.putValue(v.id, result_id);
            },
            .load8s => {
                const addr = self.getCirId(v.args[0]);
                const offset: i32 = @intCast(v.aux_int);
                const load_id = self.nextSyntheticId();
                self.emitLoadWithOffset(writer, load_id, CIR_I8, addr, offset);
                writer.emit(OP_SEXTEND, &.{ result_id, CIR_I64, load_id });
                try self.putValue(v.id, result_id);
            },
            .store, .store64 => {
                // SSA convention: args[0] = address, args[1] = value
                const addr = self.getCirId(v.args[0]);
                const val = self.getCirId(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                self.emitStoreWithOffset(writer, CIR_I64, addr, val, offset);
            },
            .store32 => {
                const addr = self.getCirId(v.args[0]);
                const val = self.getCirId(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                // Truncate to I32 first
                const trunc_id = self.nextSyntheticId();
                writer.emit(OP_IREDUCE, &.{ trunc_id, CIR_I32, val });
                self.emitStoreWithOffset(writer, CIR_I32, addr, trunc_id, offset);
            },
            .store16 => {
                const addr = self.getCirId(v.args[0]);
                const val = self.getCirId(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                const trunc_id = self.nextSyntheticId();
                writer.emit(OP_IREDUCE, &.{ trunc_id, CIR_I16, val });
                self.emitStoreWithOffset(writer, CIR_I16, addr, trunc_id, offset);
            },
            .store8 => {
                const addr = self.getCirId(v.args[0]);
                const val = self.getCirId(v.args[1]);
                const offset: i32 = @intCast(v.aux_int);
                const trunc_id = self.nextSyntheticId();
                writer.emit(OP_IREDUCE, &.{ trunc_id, CIR_I8, val });
                self.emitStoreWithOffset(writer, CIR_I8, addr, trunc_id, offset);
            },

            // ============================================================
            // Atomic operations
            // ============================================================
            .atomic_load64 => {
                // Emit as regular load for CIR (Cranelift will handle atomics)
                const addr = self.getCirId(v.args[0]);
                writer.emit(OP_LOAD, &.{ result_id, CIR_I64, addr });
                try self.putValue(v.id, result_id);
            },
            .atomic_store64 => {
                const addr = self.getCirId(v.args[0]);
                const val = self.getCirId(v.args[1]);
                writer.emit(OP_STORE, &.{ CIR_I64, addr, val });
            },
            .atomic_add64, .atomic_cas64, .atomic_exchange64 => {
                // These need runtime function calls — emit as static calls
                const func_name = switch (v.op) {
                    .atomic_add64 => "__atomic_add_i64",
                    .atomic_cas64 => "__atomic_cas_i64",
                    .atomic_exchange64 => "__atomic_xchg_i64",
                    else => unreachable,
                };
                try self.emitCallByName(v, func_name, writer);
            },

            // ============================================================
            // Address computation
            // ============================================================
            .off_ptr => {
                const base = self.getCirId(v.args[0]);
                const offset = v.aux_int;
                if (offset == 0) {
                    writer.emit(OP_COPY, &.{ result_id, CIR_I64, base });
                } else {
                    const off_id = self.nextSyntheticId();
                    self.emitConstInt(writer, off_id, CIR_I64, offset);
                    writer.emit(OP_ADD, &.{ result_id, CIR_I64, base, off_id });
                }
                try self.putValue(v.id, result_id);
            },
            .add_ptr => {
                const base = self.getCirId(v.args[0]);
                const index = self.getCirId(v.args[1]);
                writer.emit(OP_ADD, &.{ result_id, CIR_I64, base, index });
                try self.putValue(v.id, result_id);
            },
            .sub_ptr => {
                const base = self.getCirId(v.args[0]);
                const index = self.getCirId(v.args[1]);
                writer.emit(OP_SUB, &.{ result_id, CIR_I64, base, index });
                try self.putValue(v.id, result_id);
            },
            .local_addr => {
                const slot_offset: u32 = @intCast(v.aux_int);
                const slot_idx = self.stack_slot_map.get(slot_offset) orelse {
                    debug.log(.codegen, "ssa_to_cir: local_addr slot offset {d} not found", .{slot_offset});
                    self.emitConstInt(writer, result_id, CIR_I64, 0);
                    try self.putValue(v.id, result_id);
                    return;
                };
                writer.emit(OP_LOCAL_ADDR, &.{ result_id, CIR_I64, slot_idx });
                try self.putValue(v.id, result_id);
            },
            .global_addr => {
                const global_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                if (global_name) |name| {
                    if (self.global_symbol_map.get(name)) |sym_idx| {
                        const gv_id = self.nextGvId();
                        const name_off = writer.internString(name);
                        writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 });
                        writer.emit(OP_GLOBAL_VALUE, &.{ result_id, CIR_I64, gv_id });
                        try self.putValue(v.id, result_id);
                    } else {
                        debug.log(.codegen, "ssa_to_cir: global_addr '{s}' not found", .{name});
                        self.emitConstInt(writer, result_id, CIR_I64, 0);
                        try self.putValue(v.id, result_id);
                    }
                } else {
                    self.emitConstInt(writer, result_id, CIR_I64, 0);
                    try self.putValue(v.id, result_id);
                }
            },
            .addr => {
                // Get function address as pointer value.
                const fn_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                if (fn_name) |name| {
                    try self.emitFuncAddr(v, name, writer);
                } else {
                    debug.log(.codegen, "ssa_to_cir: addr op with no function name (v{d})", .{v.id});
                    self.emitConstInt(writer, result_id, CIR_I64, 0);
                    try self.putValue(v.id, result_id);
                }
            },
            .metadata_addr => {
                const type_name: ?[]const u8 = switch (v.aux) {
                    .string => |s| s,
                    else => null,
                };
                if (type_name) |tname| {
                    const dtor_name = try std.fmt.allocPrint(self.allocator, "{s}_deinit", .{tname});
                    defer self.allocator.free(dtor_name);
                    if (self.func_index_map.get(dtor_name) != null) {
                        try self.emitFuncAddr(v, dtor_name, writer);
                    } else {
                        self.emitConstInt(writer, result_id, CIR_I64, 0);
                        try self.putValue(v.id, result_id);
                    }
                } else {
                    self.emitConstInt(writer, result_id, CIR_I64, 0);
                    try self.putValue(v.id, result_id);
                }
            },

            // ============================================================
            // Control flow / structural ops
            // ============================================================
            .arg => {
                const arg_idx: u32 = @intCast(v.aux_int);
                writer.emit(OP_ARG, &.{ result_id, CIR_I64, arg_idx });
                try self.putValue(v.id, result_id);
            },
            .copy => {
                if (v.args.len > 0 and v.type_idx != TypeRegistry.SSA_MEM) {
                    const src = self.getCirId(v.args[0]);
                    if (self.string_data_symbol_idx != null and self.string_offset_ids.contains(v.args[0].id)) {
                        // String offset copy: add base address
                        // Emit global_value for string data symbol + iadd
                        const gv_id = self.nextGvId();
                        const sym_idx = self.string_data_symbol_idx.?;
                        const sym_name = self.reverse_map.get(sym_idx) orelse "__string_data";
                        const name_off = writer.internString(sym_name);
                        writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 });
                        const base_id = self.nextSyntheticId();
                        writer.emit(OP_GLOBAL_VALUE, &.{ base_id, CIR_I64, gv_id });
                        writer.emit(OP_ADD, &.{ result_id, CIR_I64, base_id, src });
                    } else {
                        writer.emit(OP_COPY, &.{ result_id, CIR_I64, src });
                    }
                    try self.putValue(v.id, result_id);
                    // Propagate compound extra if source is compound
                    if (self.compound_extra_map.get(v.args[0].id)) |extra| {
                        try self.compound_extra_map.put(self.allocator, v.id, extra);
                    }
                }
            },
            .phi => {
                // Phi values: use OP_ARG with a synthetic param index.
                // The Rust translator's SSA construction handles phis via block params.
                if (v.type_idx != TypeRegistry.SSA_MEM) {
                    // Phis are resolved via block params on the Rust side.
                    // Emit as OP_ARG to declare a block parameter.
                    const ty = self.ssaTypeToCirType(v.type_idx);
                    // Use a high param index to distinguish from entry params
                    writer.emit(OP_ARG, &.{ result_id, ty, 0 });
                    try self.putValue(v.id, result_id);
                }
            },
            .cond_select => {
                const cond = self.getCirId(v.args[0]);
                const if_true = self.getCirId(v.args[1]);
                const if_false = self.getCirId(v.args[2]);
                writer.emit(OP_COND_SELECT, &.{ result_id, CIR_I64, cond, if_true, if_false });
                try self.putValue(v.id, result_id);
            },

            // ============================================================
            // Function calls
            // ============================================================
            .static_call, .call => try self.emitCall(v, writer),
            .closure_call, .inter_call => try self.emitIndirectCall(v, writer),

            // select0/select1 — multi-return component extraction
            .select0 => {
                if (v.args.len > 0) {
                    if (self.value_map.get(v.args[0].id)) |call_result| {
                        try self.putValue(v.id, call_result);
                    }
                }
            },
            .select1 => {
                if (v.args.len > 0) {
                    if (self.compound_extra_map.get(v.args[0].id)) |secondary| {
                        try self.putValue(v.id, secondary);
                    } else {
                        self.emitConstInt(writer, result_id, CIR_I64, 0);
                        try self.putValue(v.id, result_id);
                    }
                }
            },
            .make_tuple => {
                if (v.args.len > 0) {
                    const first = self.getCirId(v.args[0]);
                    try self.putValue(v.id, first);
                }
            },

            // ============================================================
            // ARC ops — emit as calls to runtime functions
            // ============================================================
            .retain => {
                const arg = self.getCirId(v.args[0]);
                try self.emitRuntimeCall(result_id, "retain", &.{arg}, CIR_I64, writer);
                try self.putValue(v.id, result_id);
            },
            .release => {
                const arg = self.getCirId(v.args[0]);
                try self.emitRuntimeCall(self.nextSyntheticId(), "release", &.{arg}, CIR_VOID, writer);
            },

            // ============================================================
            // Nil checks
            // ============================================================
            .nil_check => {
                if (v.args.len > 0) {
                    const arg = self.getCirId(v.args[0]);
                    // trapz = trap if arg == 0 → brif(arg == 0, trap_block, continue)
                    // Simplify: emit cond_trap with inverted condition
                    // Actually, use OP_COND_TRAP: trap if condition is nonzero
                    // We want: trap if arg == 0, so: is_zero = (arg == 0), cond_trap(is_zero)
                    const zero_id = self.nextSyntheticId();
                    self.emitConstInt(writer, zero_id, CIR_I64, 0);
                    const cmp_id = self.nextSyntheticId();
                    writer.emit(OP_EQ, &.{ cmp_id, CIR_I64, arg, zero_id });
                    writer.emit(OP_COND_TRAP, &.{cmp_id});
                }
            },
            .is_non_nil => {
                const arg = self.getCirId(v.args[0]);
                const zero_id = self.nextSyntheticId();
                self.emitConstInt(writer, zero_id, CIR_I64, 0);
                writer.emit(OP_NE, &.{ result_id, CIR_I64, arg, zero_id });
                try self.putValue(v.id, result_id);
            },
            .is_nil => {
                const arg = self.getCirId(v.args[0]);
                const zero_id = self.nextSyntheticId();
                self.emitConstInt(writer, zero_id, CIR_I64, 0);
                writer.emit(OP_EQ, &.{ result_id, CIR_I64, arg, zero_id });
                try self.putValue(v.id, result_id);
            },

            // ============================================================
            // Memory state / ops we skip
            // ============================================================
            .init_mem, .var_def, .var_live, .var_kill, .fwd_ref => {},

            // ============================================================
            // Move/zero for large types
            // ============================================================
            .move => {
                if (v.args.len >= 2) {
                    const dst = self.getCirId(v.args[0]);
                    const src = self.getCirId(v.args[1]);
                    const size: u32 = @intCast(v.aux_int);
                    const end: i32 = @intCast(size);

                    // Phase 1: Load all values into synthetic temporaries
                    var load_ids: [256]u32 = undefined;
                    var load_types: [256]u32 = undefined;
                    var load_count: u32 = 0;
                    var offset: i32 = 0;

                    while (offset + 8 <= end) {
                        const lid = self.nextSyntheticId();
                        self.emitLoadWithOffset(writer, lid, CIR_I64, src, offset);
                        load_ids[load_count] = lid;
                        load_types[load_count] = CIR_I64;
                        load_count += 1;
                        offset += 8;
                    }
                    while (offset + 4 <= end) {
                        const lid = self.nextSyntheticId();
                        self.emitLoadWithOffset(writer, lid, CIR_I32, src, offset);
                        load_ids[load_count] = lid;
                        load_types[load_count] = CIR_I32;
                        load_count += 1;
                        offset += 4;
                    }
                    while (offset + 1 <= end) {
                        const lid = self.nextSyntheticId();
                        self.emitLoadWithOffset(writer, lid, CIR_I8, src, offset);
                        load_ids[load_count] = lid;
                        load_types[load_count] = CIR_I8;
                        load_count += 1;
                        offset += 1;
                    }

                    // Phase 2: Store all values from temporaries
                    var store_idx: u32 = 0;
                    offset = 0;
                    while (offset + 8 <= end) {
                        self.emitStoreWithOffset(writer, CIR_I64, dst, load_ids[store_idx], offset);
                        store_idx += 1;
                        offset += 8;
                    }
                    while (offset + 4 <= end) {
                        self.emitStoreWithOffset(writer, CIR_I32, dst, load_ids[store_idx], offset);
                        store_idx += 1;
                        offset += 4;
                    }
                    while (offset + 1 <= end) {
                        self.emitStoreWithOffset(writer, CIR_I8, dst, load_ids[store_idx], offset);
                        store_idx += 1;
                        offset += 1;
                    }
                }
                if (v.args.len > 0) {
                    const dst = self.getCirId(v.args[0]);
                    try self.putValue(v.id, dst);
                }
            },
            .zero => {
                if (v.args.len > 0) {
                    const addr = self.getCirId(v.args[0]);
                    const size: u32 = @intCast(v.aux_int);
                    const zero64_id = self.nextSyntheticId();
                    self.emitConstInt(writer, zero64_id, CIR_I64, 0);
                    const zero8_id = self.nextSyntheticId();
                    self.emitConstInt(writer, zero8_id, CIR_I8, 0);

                    var offset: i32 = 0;
                    const end: i32 = @intCast(size);
                    while (offset + 8 <= end) {
                        self.emitStoreWithOffset(writer, CIR_I64, addr, zero64_id, offset);
                        offset += 8;
                    }
                    while (offset + 1 <= end) {
                        self.emitStoreWithOffset(writer, CIR_I8, addr, zero8_id, offset);
                        offset += 1;
                    }
                    try self.putValue(v.id, addr);
                } else {
                    self.emitConstInt(writer, result_id, CIR_I64, 0);
                    try self.putValue(v.id, result_id);
                }
            },

            // ============================================================
            // String/slice/optional construction — compound types
            // ============================================================
            .string_make => {
                if (v.args.len >= 2) {
                    const arg0_op = v.args[0].op;
                    const is_raw_offset = arg0_op == .const_64 or arg0_op == .const_int;
                    if (is_raw_offset) {
                        try self.string_offset_ids.put(self.allocator, v.args[0].id, {});
                    }
                    const ptr_val = self.getCirId(v.args[0]);
                    const len_val = self.getCirId(v.args[1]);

                    if (is_raw_offset) {
                        if (self.string_data_symbol_idx) |sym_idx| {
                            // Add string data base address
                            const gv_id = self.nextGvId();
                            const sym_name = self.reverse_map.get(sym_idx) orelse "__string_data";
                            const name_off = writer.internString(sym_name);
                            writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 });
                            const base_id = self.nextSyntheticId();
                            writer.emit(OP_GLOBAL_VALUE, &.{ base_id, CIR_I64, gv_id });
                            writer.emit(OP_ADD, &.{ result_id, CIR_I64, base_id, ptr_val });
                        } else {
                            writer.emit(OP_COPY, &.{ result_id, CIR_I64, ptr_val });
                        }
                    } else {
                        writer.emit(OP_COPY, &.{ result_id, CIR_I64, ptr_val });
                    }
                    try self.putValue(v.id, result_id);
                    try self.compound_extra_map.put(self.allocator, v.id, len_val);
                }
            },
            .slice_make => {
                if (v.args.len >= 2) {
                    const ptr_val = self.getCirId(v.args[0]);
                    const len_val = self.getCirId(v.args[1]);
                    writer.emit(OP_COPY, &.{ result_id, CIR_I64, ptr_val });
                    try self.putValue(v.id, result_id);
                    try self.compound_extra_map.put(self.allocator, v.id, len_val);
                }
            },
            .opt_make => {
                if (v.args.len >= 2) {
                    const tag_val = self.getCirId(v.args[0]);
                    const data_val = self.getCirId(v.args[1]);
                    writer.emit(OP_COPY, &.{ result_id, CIR_I64, tag_val });
                    try self.putValue(v.id, result_id);
                    try self.compound_extra_map.put(self.allocator, v.id, data_val);
                }
            },
            .opt_tag => {
                if (v.args.len > 0) {
                    const src_id = v.args[0].id;
                    if (self.value_map.get(src_id)) |primary| {
                        try self.putValue(v.id, primary);
                    } else {
                        const arg = self.getCirId(v.args[0]);
                        self.emitLoadWithOffset(writer, result_id, CIR_I64, arg, 0);
                        try self.putValue(v.id, result_id);
                    }
                }
            },
            .opt_data => {
                if (v.args.len > 0) {
                    const src_id = v.args[0].id;
                    if (self.compound_extra_map.get(src_id)) |secondary| {
                        try self.putValue(v.id, secondary);
                    } else {
                        const arg = self.getCirId(v.args[0]);
                        self.emitLoadWithOffset(writer, result_id, CIR_I64, arg, 8);
                        try self.putValue(v.id, result_id);
                    }
                }
            },

            // ============================================================
            // String ops — field access
            // ============================================================
            .string_ptr => {
                if (v.args.len > 0) {
                    const arg = self.getCirId(v.args[0]);
                    self.emitLoadWithOffset(writer, result_id, CIR_I64, arg, 0);
                    try self.putValue(v.id, result_id);
                }
            },
            .string_len => {
                if (v.args.len > 0) {
                    const arg = self.getCirId(v.args[0]);
                    self.emitLoadWithOffset(writer, result_id, CIR_I64, arg, 8);
                    try self.putValue(v.id, result_id);
                }
            },

            // ============================================================
            // Compound value component extraction
            // ============================================================
            .slice_ptr => {
                if (v.args.len > 0) {
                    const src_id = v.args[0].id;
                    if (self.value_map.get(src_id)) |primary| {
                        try self.putValue(v.id, primary);
                    } else {
                        const arg = self.getCirId(v.args[0]);
                        self.emitLoadWithOffset(writer, result_id, CIR_I64, arg, 0);
                        try self.putValue(v.id, result_id);
                    }
                }
            },
            .slice_len, .slice_cap => {
                if (v.args.len > 0) {
                    const src_id = v.args[0].id;
                    if (self.compound_extra_map.get(src_id)) |secondary| {
                        try self.putValue(v.id, secondary);
                    } else {
                        const arg = self.getCirId(v.args[0]);
                        self.emitLoadWithOffset(writer, result_id, CIR_I64, arg, 8);
                        try self.putValue(v.id, result_id);
                    }
                }
            },

            // ============================================================
            // Wasm globals — used by closure bodies to read CTXT
            // ============================================================
            .wasm_global_get => {
                const global_idx = v.aux_int;
                if (global_idx == 1) {
                    // CTXT global: emit global_value for _cot_ctxt + load
                    const gv_id = self.nextGvId();
                    const name_off = writer.internString("_cot_ctxt");
                    writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 });
                    const addr_id = self.nextSyntheticId();
                    writer.emit(OP_GLOBAL_VALUE, &.{ addr_id, CIR_I64, gv_id });
                    writer.emit(OP_LOAD, &.{ result_id, CIR_I64, addr_id });
                    try self.putValue(v.id, result_id);
                } else {
                    debug.log(.codegen, "ssa_to_cir: unhandled wasm_global_get index {d}", .{global_idx});
                    self.emitConstInt(writer, result_id, CIR_I64, 0);
                    try self.putValue(v.id, result_id);
                }
            },

            .wasm_unreachable => {
                // Produce a dummy value — select will never choose it at runtime.
                self.emitConstInt(writer, result_id, CIR_I64, 0);
                try self.putValue(v.id, result_id);
            },

            else => {
                debug.log(.codegen, "ssa_to_cir: unhandled op {s} (v{d})", .{ @tagName(v.op), v.id });
                self.emitConstInt(writer, result_id, CIR_I64, 0);
                try self.putValue(v.id, result_id);
            },
        }
    }

    // ========================================================================
    // Terminator emission
    // ========================================================================

    fn emitTerminator(self: *Self, ssa_blk: *const ssa_block.Block, block_idx: u32, writer: *CirWriter) !void {
        switch (ssa_blk.kind) {
            .ret => {
                if (ssa_blk.controls[0]) |ctrl| {
                    if ((ctrl.op == .string_make or ctrl.op == .slice_make or ctrl.op == .opt_make) and ctrl.args.len >= 2) {
                        // Compound return: return both components
                        const ptr = self.getCirId(ctrl);
                        const len = if (self.compound_extra_map.get(ctrl.id)) |extra| extra else self.getCirId(ctrl.args[1]);
                        writer.emit(OP_RET, &.{ ptr, len });
                    } else if (self.compound_extra_map.get(ctrl.id)) |len_val| {
                        const ptr = self.getCirId(ctrl);
                        writer.emit(OP_RET, &.{ ptr, len_val });
                    } else {
                        const val = self.getCirId(ctrl);
                        writer.emit(OP_RET, &.{val});
                    }
                } else {
                    writer.emit(OP_RET_VOID, &.{});
                }
            },
            .plain, .defer_, .first => {
                if (ssa_blk.succs.len > 0) {
                    const target_ssa = ssa_blk.succs[0].b;
                    const target_idx = self.findBlockIndex(target_ssa);
                    // Collect phi args for successor
                    var phi_args = std.ArrayListUnmanaged(u32){};
                    defer phi_args.deinit(self.allocator);
                    try self.collectPhiArgs(ssa_blk, target_ssa, &phi_args);
                    var operands_buf: [64]u32 = undefined;
                    operands_buf[0] = target_idx;
                    operands_buf[1] = @intCast(phi_args.items.len);
                    for (phi_args.items, 0..) |a, i| {
                        operands_buf[2 + i] = a;
                    }
                    writer.emit(OP_JUMP, operands_buf[0 .. 2 + phi_args.items.len]);
                } else {
                    // No successors — check for trap blocks
                    var is_trap_block = false;
                    if (ssa_blk.preds.len > 0) {
                        for (ssa_blk.values.items) |v| {
                            if (v.op == .wasm_unreachable) {
                                is_trap_block = true;
                                break;
                            }
                        }
                    }
                    if (is_trap_block) {
                        writer.emit(OP_TRAP, &.{});
                    } else {
                        // Fall through to next layout block
                        const blocks = self.ssa_func.blocks.items;
                        if (block_idx + 1 < blocks.len) {
                            var next_idx = block_idx + 1;
                            while (next_idx < blocks.len and blocks[next_idx].values.items.len == 0 and blocks[next_idx].kind == .plain and blocks[next_idx].succs.len == 0) {
                                next_idx += 1;
                            }
                            if (next_idx < blocks.len) {
                                writer.emit(OP_JUMP, &.{ next_idx, 0 });
                            } else {
                                writer.emit(OP_TRAP, &.{});
                            }
                        } else {
                            writer.emit(OP_TRAP, &.{});
                        }
                    }
                }
            },
            .if_ => {
                const cond = if (ssa_blk.controls[0]) |ctrl| self.getCirId(ctrl) else {
                    writer.emit(OP_TRAP, &.{});
                    return;
                };
                if (ssa_blk.succs.len >= 2) {
                    const then_ssa = ssa_blk.succs[0].b;
                    const else_ssa = ssa_blk.succs[1].b;
                    const then_idx = self.findBlockIndex(then_ssa);
                    const else_idx = self.findBlockIndex(else_ssa);

                    var then_phi_args = std.ArrayListUnmanaged(u32){};
                    defer then_phi_args.deinit(self.allocator);
                    var else_phi_args = std.ArrayListUnmanaged(u32){};
                    defer else_phi_args.deinit(self.allocator);
                    try self.collectPhiArgs(ssa_blk, then_ssa, &then_phi_args);
                    try self.collectPhiArgs(ssa_blk, else_ssa, &else_phi_args);

                    var operands_buf: [64]u32 = undefined;
                    operands_buf[0] = cond;
                    operands_buf[1] = then_idx;
                    operands_buf[2] = else_idx;
                    operands_buf[3] = @intCast(then_phi_args.items.len);
                    var pos: usize = 4;
                    for (then_phi_args.items) |a| {
                        operands_buf[pos] = a;
                        pos += 1;
                    }
                    operands_buf[pos] = @intCast(else_phi_args.items.len);
                    pos += 1;
                    for (else_phi_args.items) |a| {
                        operands_buf[pos] = a;
                        pos += 1;
                    }
                    writer.emit(OP_BRIF, operands_buf[0..pos]);
                } else {
                    writer.emit(OP_TRAP, &.{});
                }
            },
            .exit => {
                writer.emit(OP_TRAP, &.{});
            },
            .jump_table => {
                const selector = if (ssa_blk.controls[0]) |ctrl| self.getCirId(ctrl) else {
                    writer.emit(OP_TRAP, &.{});
                    return;
                };
                if (ssa_blk.succs.len > 0) {
                    var operands_buf: [256]u32 = undefined;
                    operands_buf[0] = selector;
                    operands_buf[1] = @intCast(ssa_blk.succs.len);
                    var pos: usize = 2;
                    for (ssa_blk.succs) |edge| {
                        const target_idx = self.findBlockIndex(edge.b);
                        operands_buf[pos] = target_idx;
                        operands_buf[pos + 1] = 0; // arg_count = 0
                        pos += 2;
                    }
                    writer.emit(OP_BR_TABLE, operands_buf[0..pos]);
                } else {
                    writer.emit(OP_TRAP, &.{});
                }
            },
            else => {
                debug.log(.codegen, "ssa_to_cir: unhandled block kind {s}", .{@tagName(ssa_blk.kind)});
                writer.emit(OP_TRAP, &.{});
            },
        }
    }

    // ========================================================================
    // Phi resolution
    // ========================================================================

    fn collectPhiArgs(self: *Self, from_block: *const ssa_block.Block, to_block: *const ssa_block.Block, phi_args: *std.ArrayListUnmanaged(u32)) !void {
        const pred_idx = self.findPredIndex(to_block, from_block) orelse return;
        for (to_block.values.items) |v| {
            if (v.op == .phi and v.type_idx != TypeRegistry.SSA_MEM) {
                if (pred_idx < v.args.len) {
                    const arg_val = self.getCirId(v.args[pred_idx]);
                    try phi_args.append(self.allocator, arg_val);
                }
            }
        }
    }

    fn findPredIndex(_: *const Self, block: *const ssa_block.Block, pred: *const ssa_block.Block) ?usize {
        for (block.preds, 0..) |edge, i| {
            if (edge.b.id == pred.id) return i;
        }
        return null;
    }

    fn findBlockIndex(self: *const Self, target: *const ssa_block.Block) u32 {
        for (self.ssa_func.blocks.items, 0..) |b, i| {
            if (b.id == target.id) return @intCast(i);
        }
        return 0;
    }

    // ========================================================================
    // Call emission
    // ========================================================================

    fn emitCall(self: *Self, v: *const ssa_value.Value, writer: *CirWriter) !void {
        const func_name = switch (v.aux) {
            .string => |s| s,
            .call => |c| c.fn_name,
            else => "unknown",
        };

        // Gather call arguments — skip the memory arg (always last position).
        const has_mem = v.memoryArg() != null;
        const data_args = if (has_mem and v.args.len > 0) v.args[0 .. v.args.len - 1] else v.args;

        // Collect CIR value IDs for args
        var arg_ids: [128]u32 = undefined;
        const arg_count = @min(data_args.len, 128);
        for (data_args[0..arg_count], 0..) |arg, i| {
            arg_ids[i] = self.getCirId(arg);
        }

        // Look up callee's IR func for parameter types
        const callee_ir = self.findIrFunc(func_name);
        const has_return = v.type_idx != TypeRegistry.VOID and v.type_idx != TypeRegistry.SSA_MEM;

        // Build CIR call: (result_count, [result_id, result_type]..., name_offset, arg_count, [arg_type, arg_value_id]...)
        const result_id: u32 = v.id;
        const name_off = writer.internString(func_name);
        var operands_buf: [256]u32 = undefined;

        // Determine result info
        const result_count: u32 = if (has_return) blk: {
            // Check if compound return
            if (callee_ir) |ir_func| {
                const ret_ptype = self.type_reg.get(ir_func.return_type);
                const is_compound = ir_func.return_type == TypeRegistry.STRING or ret_ptype == .slice or
                    (ret_ptype == .optional and brk: {
                    const ei = self.type_reg.get(ret_ptype.optional.elem);
                    break :brk ei == .pointer and ei.pointer.managed;
                });
                if (is_compound) break :blk 2;
            }
            break :blk 1;
        } else 0;

        operands_buf[0] = result_count;
        var pos: usize = 1;

        if (result_count >= 1) {
            operands_buf[pos] = result_id;
            operands_buf[pos + 1] = CIR_I64;
            pos += 2;
        }
        if (result_count >= 2) {
            const extra_result_id = self.nextSyntheticId();
            operands_buf[pos] = extra_result_id;
            operands_buf[pos + 1] = CIR_I64;
            pos += 2;
            // Store extra result ID for compound_extra_map
            self.compound_extra_map.put(self.allocator, v.id, extra_result_id) catch {};
        }

        operands_buf[pos] = name_off;
        pos += 1;
        operands_buf[pos] = @intCast(arg_count);
        pos += 1;

        // Emit arg types and values
        for (0..arg_count) |i| {
            // Get callee's expected parameter type
            const param_type: u32 = if (callee_ir) |ir_func| blk: {
                if (i < ir_func.params.len) {
                    break :blk self.ssaTypeToCirType(ir_func.params[i].type_idx);
                }
                break :blk CIR_I64;
            } else CIR_I64;
            operands_buf[pos] = param_type;
            operands_buf[pos + 1] = arg_ids[i];
            pos += 2;
        }

        writer.emit(OP_STATIC_CALL, operands_buf[0..pos]);
        if (has_return) {
            try self.putValue(v.id, result_id);
        }
    }

    fn emitIndirectCall(self: *Self, v: *const ssa_value.Value, writer: *CirWriter) !void {
        if (v.args.len == 0) return;
        const callee = self.getCirId(v.args[0]);
        const result_id: u32 = v.id;

        // For closure_call: store context pointer to _cot_ctxt global
        const actual_args_start: usize = if (v.op == .closure_call) blk: {
            if (v.args.len > 1) {
                const context = self.getCirId(v.args[1]);
                // Store to CTXT
                const gv_id = self.nextGvId();
                const name_off = writer.internString("_cot_ctxt");
                writer.emit(OP_GLOBAL_VALUE_SYMBOL, &.{ gv_id, name_off, 0, 0, 1 });
                const ctxt_addr = self.nextSyntheticId();
                writer.emit(OP_GLOBAL_VALUE, &.{ ctxt_addr, CIR_I64, gv_id });
                writer.emit(OP_STORE, &.{ CIR_I64, ctxt_addr, context });
            }
            break :blk 2;
        } else 1;

        // Skip last arg (memory state) by position
        const indirect_has_mem = v.memoryArg() != null;
        const indirect_end = if (indirect_has_mem and v.args.len > 0) v.args.len - 1 else v.args.len;

        // Collect actual call args
        var arg_ids: [128]u32 = undefined;
        var actual_arg_count: usize = 0;
        if (actual_args_start < indirect_end) {
            for (v.args[actual_args_start..indirect_end]) |arg| {
                arg_ids[actual_arg_count] = self.getCirId(arg);
                actual_arg_count += 1;
            }
        }

        const has_return = v.type_idx != TypeRegistry.VOID and v.type_idx != TypeRegistry.SSA_MEM;

        // Format: (result_count, [result_id, result_type]..., callee_value, arg_count, [arg_type, arg_value_id]...)
        var operands_buf: [256]u32 = undefined;
        const result_count: u32 = if (has_return) 1 else 0;
        operands_buf[0] = result_count;
        var pos: usize = 1;
        if (result_count > 0) {
            operands_buf[pos] = result_id;
            operands_buf[pos + 1] = CIR_I64;
            pos += 2;
        }
        operands_buf[pos] = callee;
        pos += 1;
        operands_buf[pos] = @intCast(actual_arg_count);
        pos += 1;
        for (0..actual_arg_count) |i| {
            operands_buf[pos] = CIR_I64; // param type
            operands_buf[pos + 1] = arg_ids[i];
            pos += 2;
        }
        writer.emit(OP_CALL_INDIRECT, operands_buf[0..pos]);

        if (has_return) {
            try self.putValue(v.id, result_id);
        }
    }

    fn emitCallByName(self: *Self, v: *const ssa_value.Value, func_name: []const u8, writer: *CirWriter) !void {
        const result_id: u32 = v.id;
        const name_off = writer.internString(func_name);
        const has_return = v.type_idx != TypeRegistry.VOID and v.type_idx != TypeRegistry.SSA_MEM;

        var operands_buf: [64]u32 = undefined;
        operands_buf[0] = if (has_return) 1 else 0;
        var pos: usize = 1;
        if (has_return) {
            operands_buf[pos] = result_id;
            operands_buf[pos + 1] = CIR_I64;
            pos += 2;
        }
        operands_buf[pos] = name_off;
        pos += 1;
        const arg_count = v.args.len;
        operands_buf[pos] = @intCast(arg_count);
        pos += 1;
        for (v.args) |arg| {
            operands_buf[pos] = CIR_I64;
            operands_buf[pos + 1] = self.getCirId(arg);
            pos += 2;
        }
        writer.emit(OP_STATIC_CALL, operands_buf[0..pos]);
        if (has_return) {
            try self.putValue(v.id, result_id);
        }
    }

    fn emitRuntimeCall(self: *Self, result_id: u32, name: []const u8, args: []const u32, ret_type: u32, writer: *CirWriter) !void {
        const name_off = writer.internString(name);
        const has_return = ret_type != CIR_VOID;
        var operands_buf: [64]u32 = undefined;
        operands_buf[0] = if (has_return) 1 else 0;
        var pos: usize = 1;
        if (has_return) {
            operands_buf[pos] = result_id;
            operands_buf[pos + 1] = CIR_I64;
            pos += 2;
        }
        operands_buf[pos] = name_off;
        pos += 1;
        operands_buf[pos] = @intCast(args.len);
        pos += 1;
        for (args) |a| {
            operands_buf[pos] = CIR_I64;
            operands_buf[pos + 1] = a;
            pos += 2;
        }
        writer.emit(OP_STATIC_CALL, operands_buf[0..pos]);
    }

    fn emitFuncAddr(self: *Self, v: *const ssa_value.Value, name: []const u8, writer: *CirWriter) !void {
        const result_id: u32 = v.id;
        const name_off = writer.internString(name);
        const callee_ir = self.findIrFunc(name);

        // Format: (result_id, type_idx, name_offset, param_count, [param_types...], return_count, [return_types...])
        var operands_buf: [64]u32 = undefined;
        operands_buf[0] = result_id;
        operands_buf[1] = CIR_I64;
        operands_buf[2] = name_off;
        var pos: usize = 3;
        if (callee_ir) |ir_func| {
            operands_buf[pos] = @intCast(ir_func.params.len);
            pos += 1;
            for (ir_func.params) |p| {
                operands_buf[pos] = self.ssaTypeToCirType(p.type_idx);
                pos += 1;
            }
            const has_ret = ir_func.return_type != TypeRegistry.VOID;
            operands_buf[pos] = if (has_ret) 1 else 0;
            pos += 1;
            if (has_ret) {
                operands_buf[pos] = self.ssaTypeToCirType(ir_func.return_type);
                pos += 1;
            }
        } else {
            operands_buf[pos] = 0; // param_count
            pos += 1;
            operands_buf[pos] = 0; // return_count
            pos += 1;
        }
        writer.emit(OP_FUNC_ADDR, operands_buf[0..pos]);
        try self.putValue(v.id, result_id);
    }

    fn findIrFunc(self: *const Self, name: []const u8) ?*const ir_mod.Func {
        for (self.ir_funcs) |*f| {
            if (std.mem.eql(u8, f.name, name)) return f;
        }
        return null;
    }

    // ========================================================================
    // Generic convert
    // ========================================================================

    fn emitGenericConvert(self: *Self, v: *const ssa_value.Value, writer: *CirWriter) !void {
        const result_id: u32 = v.id;
        const arg = self.getCirId(v.args[0]);
        const from_type = v.aux.type_ref;
        const to_type = v.type_idx;

        const from_is_float = (from_type == TypeRegistry.F32 or from_type == TypeRegistry.F64 or from_type == TypeRegistry.UNTYPED_FLOAT);
        const to_is_float = (to_type == TypeRegistry.F32 or to_type == TypeRegistry.F64 or to_type == TypeRegistry.UNTYPED_FLOAT);
        const to_cir = self.ssaTypeToCirType(to_type);

        if (from_is_float and !to_is_float) {
            writer.emit(OP_FCVT_TO_SINT, &.{ result_id, to_cir, arg });
        } else if (!from_is_float and to_is_float) {
            writer.emit(OP_FCVT_FROM_SINT, &.{ result_id, to_cir, arg });
        } else if (from_is_float and to_is_float) {
            if (to_type == TypeRegistry.F32) {
                writer.emit(OP_FDEMOTE, &.{ result_id, CIR_F32, arg });
            } else {
                writer.emit(OP_FPROMOTE, &.{ result_id, CIR_F64, arg });
            }
        } else {
            // int → int
            const from_cir = self.ssaTypeToCirType(from_type);
            const from_bits = cirTypeBits(from_cir);
            const to_bits = cirTypeBits(to_cir);
            if (from_bits < to_bits) {
                const from_info = self.type_reg.get(from_type);
                const is_unsigned = switch (from_info) {
                    .basic => |k| k.isUnsigned(),
                    .enum_type => |e| blk: {
                        const backing = self.type_reg.get(e.backing_type);
                        break :blk backing == .basic and backing.basic.isUnsigned();
                    },
                    else => false,
                };
                if (is_unsigned) {
                    writer.emit(OP_UEXTEND, &.{ result_id, to_cir, arg });
                } else {
                    writer.emit(OP_SEXTEND, &.{ result_id, to_cir, arg });
                }
            } else if (from_bits > to_bits) {
                writer.emit(OP_IREDUCE, &.{ result_id, to_cir, arg });
            } else {
                writer.emit(OP_COPY, &.{ result_id, to_cir, arg });
            }
        }
        try self.putValue(v.id, result_id);
    }

    // ========================================================================
    // Helper: emit binary op
    // ========================================================================

    fn emitBinary(self: *Self, v: *const ssa_value.Value, opcode: u16, ty: u32, writer: *CirWriter) !void {
        const lhs = self.getCirId(v.args[0]);
        const rhs = self.getCirId(v.args[1]);
        writer.emit(opcode, &.{ v.id, ty, lhs, rhs });
        try self.putValue(v.id, v.id);
    }

    fn emitBinary32(self: *Self, v: *const ssa_value.Value, opcode: u16, writer: *CirWriter) !void {
        const lhs = self.getCirId(v.args[0]);
        const rhs = self.getCirId(v.args[1]);
        // Truncate to I32, operate, extend back to I64
        const lhs32 = self.nextSyntheticId();
        const rhs32 = self.nextSyntheticId();
        writer.emit(OP_IREDUCE, &.{ lhs32, CIR_I32, lhs });
        writer.emit(OP_IREDUCE, &.{ rhs32, CIR_I32, rhs });
        const result32 = self.nextSyntheticId();
        writer.emit(opcode, &.{ result32, CIR_I32, lhs32, rhs32 });
        writer.emit(OP_UEXTEND, &.{ v.id, CIR_I64, result32 });
        try self.putValue(v.id, v.id);
    }

    fn emitFloatUnary(self: *Self, v: *const ssa_value.Value, opcode: u16, ty: u32, writer: *CirWriter) !void {
        const arg = self.getCirId(v.args[0]);
        writer.emit(opcode, &.{ v.id, ty, arg });
        try self.putValue(v.id, v.id);
    }

    // ========================================================================
    // Helper: emit comparison
    // ========================================================================

    fn emitCompare(self: *Self, v: *const ssa_value.Value, opcode: u16, operand_ty: u32, writer: *CirWriter) !void {
        const lhs = self.getCirId(v.args[0]);
        const rhs = self.getCirId(v.args[1]);
        writer.emit(opcode, &.{ v.id, operand_ty, lhs, rhs });
        try self.putValue(v.id, v.id);
    }

    // ========================================================================
    // Helper: emit conversion
    // ========================================================================

    fn emitConvert(self: *Self, v: *const ssa_value.Value, opcode: u16, target_ty: u32, writer: *CirWriter) !void {
        const arg = self.getCirId(v.args[0]);
        writer.emit(opcode, &.{ v.id, target_ty, arg });
        try self.putValue(v.id, v.id);
    }

    // ========================================================================
    // Helper: emit const int
    // ========================================================================

    fn emitConstInt(self: *Self, writer: *CirWriter, result_id: u32, ty: u32, imm: i64) void {
        _ = self;
        const imm_lo: u32 = @truncate(@as(u64, @bitCast(imm)));
        const imm_hi: u32 = @truncate(@as(u64, @bitCast(imm)) >> 32);
        if (imm_hi != 0) {
            writer.emit(OP_CONST_INT, &.{ result_id, ty, imm_lo, imm_hi });
        } else {
            writer.emit(OP_CONST_INT, &.{ result_id, ty, imm_lo });
        }
    }

    // ========================================================================
    // Helper: emit load/store with offset decomposition
    // The CIR format only supports zero-offset loads/stores.
    // Non-zero offsets are decomposed into: const + add + load/store.
    // ========================================================================

    fn emitLoadWithOffset(self: *Self, writer: *CirWriter, result_id: u32, ty: u32, addr: u32, offset: i32) void {
        if (offset != 0) {
            const off_id = self.nextSyntheticId();
            const adj_id = self.nextSyntheticId();
            self.emitConstInt(writer, off_id, CIR_I64, @as(i64, offset));
            writer.emit(OP_ADD, &.{ adj_id, CIR_I64, addr, off_id });
            writer.emit(OP_LOAD, &.{ result_id, ty, adj_id });
        } else {
            writer.emit(OP_LOAD, &.{ result_id, ty, addr });
        }
    }

    fn emitStoreWithOffset(self: *Self, writer: *CirWriter, val_type: u32, addr: u32, val: u32, offset: i32) void {
        if (offset != 0) {
            const off_id = self.nextSyntheticId();
            const adj_id = self.nextSyntheticId();
            self.emitConstInt(writer, off_id, CIR_I64, @as(i64, offset));
            writer.emit(OP_ADD, &.{ adj_id, CIR_I64, addr, off_id });
            writer.emit(OP_STORE, &.{ val_type, adj_id, val });
        } else {
            writer.emit(OP_STORE, &.{ val_type, addr, val });
        }
    }

    // ========================================================================
    // Value mapping helpers
    // ========================================================================

    fn getCirId(self: *const Self, ssa_val: *const ssa_value.Value) u32 {
        return self.value_map.get(ssa_val.id) orelse {
            debug.log(.codegen, "ssa_to_cir: missing value v{d} (op={s}) in func {s}", .{ ssa_val.id, @tagName(ssa_val.op), self.ssa_func.name });
            return ssa_val.id; // Return the SSA ID as fallback
        };
    }

    fn putValue(self: *Self, id: ssa_value.ID, cir_id: u32) !void {
        try self.value_map.put(self.allocator, id, cir_id);
    }

    // ========================================================================
    // Type mapping: SSA TypeIndex → CIR type index
    // ========================================================================

    fn ssaTypeToCirType(self: *const Self, idx: TypeIndex) u32 {
        if (idx == TypeRegistry.F32) return CIR_F32;
        if (idx == TypeRegistry.F64 or idx == TypeRegistry.UNTYPED_FLOAT) return CIR_F64;
        if (idx == TypeRegistry.BOOL or idx == TypeRegistry.UNTYPED_BOOL) return CIR_I8;
        if (idx == TypeRegistry.I8 or idx == TypeRegistry.U8) return CIR_I8;
        if (idx == TypeRegistry.I16 or idx == TypeRegistry.U16) return CIR_I16;
        if (idx == TypeRegistry.I32 or idx == TypeRegistry.U32) return CIR_I32;

        const t = self.type_reg.get(idx);
        switch (t) {
            .basic => |k| {
                if (k.isFloat()) return CIR_F64;
                if (k == .i32_type or k == .u32_type) return CIR_I32;
                if (k == .i16_type or k == .u16_type) return CIR_I16;
                if (k == .i8_type or k == .u8_type or k == .bool_type) return CIR_I8;
            },
            .enum_type => |e| return self.ssaTypeToCirType(e.backing_type),
            else => {},
        }

        return CIR_I64; // Default: everything else is 64-bit
    }
};

fn cirTypeBits(ty: u32) u32 {
    return switch (ty) {
        CIR_I8, CIR_U8, CIR_BOOL => 8,
        CIR_I16, CIR_U16 => 16,
        CIR_I32, CIR_U32, CIR_F32 => 32,
        CIR_I64, CIR_U64, CIR_F64 => 64,
        else => 64,
    };
}
