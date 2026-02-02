//! WebAssembly bytecode decoder.
//!
//! Port of wasmparser's operator iteration pattern.
//! Decodes raw Wasm bytecode into WasmOperator tagged unions.
//!
//! Reference: wasmparser crate's BinaryReader and Operator enum

const std = @import("std");
const wasm = @import("../../wasm_opcodes.zig");
const func_translator = @import("func_translator.zig");

pub const WasmOperator = func_translator.WasmOperator;
pub const BlockData = func_translator.BlockData;

// ============================================================================
// MemArg - Memory instruction argument
// ============================================================================

pub const MemArg = struct {
    /// Alignment (log2 of byte alignment)
    align_: u32,
    /// Memory offset
    offset: u32,
};

// ============================================================================
// Extended WasmOperator with all Wasm instructions
// ============================================================================

/// Extended operator enum covering all Wasm 1.0 instructions.
/// This extends func_translator.WasmOperator with missing instructions.
pub const WasmOp = union(enum) {
    // ========================================================================
    // Control flow
    // ========================================================================
    block: BlockData,
    loop: BlockData,
    if_op: BlockData,
    else_op,
    end,
    br: u32,
    br_if: u32,
    br_table: BrTableData,
    return_op,
    unreachable_op,
    nop,

    // ========================================================================
    // Call instructions
    // ========================================================================
    call: u32,
    call_indirect: CallIndirectData,

    // ========================================================================
    // Variables
    // ========================================================================
    local_get: u32,
    local_set: u32,
    local_tee: u32,
    global_get: u32,
    global_set: u32,

    // ========================================================================
    // Constants
    // ========================================================================
    i32_const: i32,
    i64_const: i64,
    f32_const: f32,
    f64_const: f64,

    // ========================================================================
    // Memory instructions
    // ========================================================================
    i32_load: MemArg,
    i64_load: MemArg,
    f32_load: MemArg,
    f64_load: MemArg,
    i32_load8_s: MemArg,
    i32_load8_u: MemArg,
    i32_load16_s: MemArg,
    i32_load16_u: MemArg,
    i64_load8_s: MemArg,
    i64_load8_u: MemArg,
    i64_load16_s: MemArg,
    i64_load16_u: MemArg,
    i64_load32_s: MemArg,
    i64_load32_u: MemArg,
    i32_store: MemArg,
    i64_store: MemArg,
    f32_store: MemArg,
    f64_store: MemArg,
    i32_store8: MemArg,
    i32_store16: MemArg,
    i64_store8: MemArg,
    i64_store16: MemArg,
    i64_store32: MemArg,
    memory_size: u32,
    memory_grow: u32,

    // ========================================================================
    // Arithmetic i32
    // ========================================================================
    i32_add,
    i32_sub,
    i32_mul,
    i32_div_s,
    i32_div_u,
    i32_rem_s,
    i32_rem_u,
    i32_and,
    i32_or,
    i32_xor,
    i32_shl,
    i32_shr_s,
    i32_shr_u,
    i32_rotl,
    i32_rotr,
    i32_clz,
    i32_ctz,
    i32_popcnt,

    // ========================================================================
    // Arithmetic i64
    // ========================================================================
    i64_add,
    i64_sub,
    i64_mul,
    i64_div_s,
    i64_div_u,
    i64_rem_s,
    i64_rem_u,
    i64_and,
    i64_or,
    i64_xor,
    i64_shl,
    i64_shr_s,
    i64_shr_u,
    i64_rotl,
    i64_rotr,
    i64_clz,
    i64_ctz,
    i64_popcnt,

    // ========================================================================
    // Arithmetic f32
    // ========================================================================
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_min,
    f32_max,
    f32_copysign,
    f32_abs,
    f32_neg,
    f32_ceil,
    f32_floor,
    f32_trunc,
    f32_nearest,
    f32_sqrt,

    // ========================================================================
    // Arithmetic f64
    // ========================================================================
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_min,
    f64_max,
    f64_copysign,
    f64_abs,
    f64_neg,
    f64_ceil,
    f64_floor,
    f64_trunc,
    f64_nearest,
    f64_sqrt,

    // ========================================================================
    // Comparison i32
    // ========================================================================
    i32_eqz,
    i32_eq,
    i32_ne,
    i32_lt_s,
    i32_lt_u,
    i32_gt_s,
    i32_gt_u,
    i32_le_s,
    i32_le_u,
    i32_ge_s,
    i32_ge_u,

    // ========================================================================
    // Comparison i64
    // ========================================================================
    i64_eqz,
    i64_eq,
    i64_ne,
    i64_lt_s,
    i64_lt_u,
    i64_gt_s,
    i64_gt_u,
    i64_le_s,
    i64_le_u,
    i64_ge_s,
    i64_ge_u,

    // ========================================================================
    // Comparison f32
    // ========================================================================
    f32_eq,
    f32_ne,
    f32_lt,
    f32_gt,
    f32_le,
    f32_ge,

    // ========================================================================
    // Comparison f64
    // ========================================================================
    f64_eq,
    f64_ne,
    f64_lt,
    f64_gt,
    f64_le,
    f64_ge,

    // ========================================================================
    // Conversions
    // ========================================================================
    i32_wrap_i64,
    i64_extend_i32_s,
    i64_extend_i32_u,
    i32_trunc_f32_s,
    i32_trunc_f32_u,
    i32_trunc_f64_s,
    i32_trunc_f64_u,
    i64_trunc_f32_s,
    i64_trunc_f32_u,
    i64_trunc_f64_s,
    i64_trunc_f64_u,
    f32_convert_i32_s,
    f32_convert_i32_u,
    f32_convert_i64_s,
    f32_convert_i64_u,
    f64_convert_i32_s,
    f64_convert_i32_u,
    f64_convert_i64_s,
    f64_convert_i64_u,
    f32_demote_f64,
    f64_promote_f32,
    i32_reinterpret_f32,
    i64_reinterpret_f64,
    f32_reinterpret_i32,
    f64_reinterpret_i64,

    // ========================================================================
    // Sign extension
    // ========================================================================
    i32_extend8_s,
    i32_extend16_s,
    i64_extend8_s,
    i64_extend16_s,
    i64_extend32_s,

    // ========================================================================
    // Parametric
    // ========================================================================
    drop,
    select,

    // ========================================================================
    // Convert to basic WasmOperator (for func_translator compatibility)
    // ========================================================================

    pub fn toBasicOperator(self: WasmOp) ?WasmOperator {
        return switch (self) {
            // Control flow
            .block => |d| WasmOperator{ .block = d },
            .loop => |d| WasmOperator{ .loop = d },
            .if_op => |d| WasmOperator{ .if_op = d },
            .else_op => .else_op,
            .end => .end,
            .br => |d| WasmOperator{ .br = d },
            .br_if => |d| WasmOperator{ .br_if = d },
            .return_op => .return_op,
            .unreachable_op => .unreachable_op,
            .nop => .nop,

            // Variables
            .local_get => |d| WasmOperator{ .local_get = d },
            .local_set => |d| WasmOperator{ .local_set = d },
            .local_tee => |d| WasmOperator{ .local_tee = d },

            // Constants
            .i32_const => |d| WasmOperator{ .i32_const = d },
            .i64_const => |d| WasmOperator{ .i64_const = d },

            // Arithmetic i32
            .i32_add => .i32_add,
            .i32_sub => .i32_sub,
            .i32_mul => .i32_mul,
            .i32_div_s => .i32_div_s,
            .i32_div_u => .i32_div_u,
            .i32_rem_s => .i32_rem_s,
            .i32_rem_u => .i32_rem_u,
            .i32_and => .i32_and,
            .i32_or => .i32_or,
            .i32_xor => .i32_xor,
            .i32_shl => .i32_shl,
            .i32_shr_s => .i32_shr_s,
            .i32_shr_u => .i32_shr_u,

            // Comparison i32
            .i32_eqz => .i32_eqz,
            .i32_eq => .i32_eq,
            .i32_ne => .i32_ne,
            .i32_lt_s => .i32_lt_s,
            .i32_lt_u => .i32_lt_u,
            .i32_gt_s => .i32_gt_s,
            .i32_gt_u => .i32_gt_u,
            .i32_le_s => .i32_le_s,
            .i32_le_u => .i32_le_u,
            .i32_ge_s => .i32_ge_s,
            .i32_ge_u => .i32_ge_u,

            // Conversions
            .i32_wrap_i64 => .i32_wrap_i64,
            .i64_extend_i32_s => .i64_extend_i32_s,
            .i64_extend_i32_u => .i64_extend_i32_u,

            // Parametric
            .drop => .drop,
            .select => .select,

            // Not yet supported in basic translator
            else => null,
        };
    }
};

pub const BrTableData = struct {
    targets: []const u32,
    default: u32,
};

pub const CallIndirectData = struct {
    type_index: u32,
    table_index: u32,
};

// ============================================================================
// Decoder
// ============================================================================

pub const DecodeError = error{
    UnexpectedEnd,
    InvalidOpcode,
    InvalidBlockType,
    OutOfMemory,
};

/// WebAssembly bytecode decoder.
/// Decodes a function body into a sequence of operators.
pub const Decoder = struct {
    allocator: std.mem.Allocator,
    bytes: []const u8,
    pos: usize,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, bytes: []const u8) Self {
        return .{
            .allocator = allocator,
            .bytes = bytes,
            .pos = 0,
        };
    }

    /// Decode all operators from the bytecode.
    /// Returns owned slice that must be freed by caller.
    pub fn decodeAll(self: *Self) ![]WasmOp {
        var ops = std.ArrayListUnmanaged(WasmOp){};
        errdefer ops.deinit(self.allocator);

        while (self.pos < self.bytes.len) {
            const op = try self.decodeOne();
            try ops.append(self.allocator, op);

            // Stop after final end
            if (op == .end and self.isAtEnd()) break;
        }

        return ops.toOwnedSlice(self.allocator);
    }

    fn isAtEnd(self: *const Self) bool {
        return self.pos >= self.bytes.len;
    }

    /// Decode a single operator.
    pub fn decodeOne(self: *Self) !WasmOp {
        const opcode = self.readByte() orelse return DecodeError.UnexpectedEnd;

        return switch (opcode) {
            // Control flow
            wasm.Op.unreachable_op => .unreachable_op,
            wasm.Op.nop => .nop,
            wasm.Op.block => WasmOp{ .block = try self.readBlockType() },
            wasm.Op.loop => WasmOp{ .loop = try self.readBlockType() },
            wasm.Op.if_op => WasmOp{ .if_op = try self.readBlockType() },
            wasm.Op.else_op => .else_op,
            wasm.Op.end => .end,
            wasm.Op.br => WasmOp{ .br = self.readU32() },
            wasm.Op.br_if => WasmOp{ .br_if = self.readU32() },
            wasm.Op.br_table => try self.readBrTable(),
            wasm.Op.return_op => .return_op,
            wasm.Op.call => WasmOp{ .call = self.readU32() },
            wasm.Op.call_indirect => WasmOp{ .call_indirect = .{
                .type_index = self.readU32(),
                .table_index = self.readU32(),
            } },

            // Parametric
            wasm.Op.drop => .drop,
            wasm.Op.select => .select,

            // Variables
            wasm.Op.local_get => WasmOp{ .local_get = self.readU32() },
            wasm.Op.local_set => WasmOp{ .local_set = self.readU32() },
            wasm.Op.local_tee => WasmOp{ .local_tee = self.readU32() },
            wasm.Op.global_get => WasmOp{ .global_get = self.readU32() },
            wasm.Op.global_set => WasmOp{ .global_set = self.readU32() },

            // Memory - loads
            wasm.Op.i32_load => WasmOp{ .i32_load = self.readMemArg() },
            wasm.Op.i64_load => WasmOp{ .i64_load = self.readMemArg() },
            wasm.Op.f32_load => WasmOp{ .f32_load = self.readMemArg() },
            wasm.Op.f64_load => WasmOp{ .f64_load = self.readMemArg() },
            wasm.Op.i32_load8_s => WasmOp{ .i32_load8_s = self.readMemArg() },
            wasm.Op.i32_load8_u => WasmOp{ .i32_load8_u = self.readMemArg() },
            wasm.Op.i32_load16_s => WasmOp{ .i32_load16_s = self.readMemArg() },
            wasm.Op.i32_load16_u => WasmOp{ .i32_load16_u = self.readMemArg() },
            wasm.Op.i64_load8_s => WasmOp{ .i64_load8_s = self.readMemArg() },
            wasm.Op.i64_load8_u => WasmOp{ .i64_load8_u = self.readMemArg() },
            wasm.Op.i64_load16_s => WasmOp{ .i64_load16_s = self.readMemArg() },
            wasm.Op.i64_load16_u => WasmOp{ .i64_load16_u = self.readMemArg() },
            wasm.Op.i64_load32_s => WasmOp{ .i64_load32_s = self.readMemArg() },
            wasm.Op.i64_load32_u => WasmOp{ .i64_load32_u = self.readMemArg() },

            // Memory - stores
            wasm.Op.i32_store => WasmOp{ .i32_store = self.readMemArg() },
            wasm.Op.i64_store => WasmOp{ .i64_store = self.readMemArg() },
            wasm.Op.f32_store => WasmOp{ .f32_store = self.readMemArg() },
            wasm.Op.f64_store => WasmOp{ .f64_store = self.readMemArg() },
            wasm.Op.i32_store8 => WasmOp{ .i32_store8 = self.readMemArg() },
            wasm.Op.i32_store16 => WasmOp{ .i32_store16 = self.readMemArg() },
            wasm.Op.i64_store8 => WasmOp{ .i64_store8 = self.readMemArg() },
            wasm.Op.i64_store16 => WasmOp{ .i64_store16 = self.readMemArg() },
            wasm.Op.i64_store32 => WasmOp{ .i64_store32 = self.readMemArg() },

            // Memory size/grow
            wasm.Op.memory_size => blk: {
                const mem_idx = self.readU32();
                break :blk WasmOp{ .memory_size = mem_idx };
            },
            wasm.Op.memory_grow => blk: {
                const mem_idx = self.readU32();
                break :blk WasmOp{ .memory_grow = mem_idx };
            },

            // Constants
            wasm.Op.i32_const => WasmOp{ .i32_const = self.readI32() },
            wasm.Op.i64_const => WasmOp{ .i64_const = self.readI64() },
            wasm.Op.f32_const => WasmOp{ .f32_const = self.readF32() },
            wasm.Op.f64_const => WasmOp{ .f64_const = self.readF64() },

            // i32 comparison
            wasm.Op.i32_eqz => .i32_eqz,
            wasm.Op.i32_eq => .i32_eq,
            wasm.Op.i32_ne => .i32_ne,
            wasm.Op.i32_lt_s => .i32_lt_s,
            wasm.Op.i32_lt_u => .i32_lt_u,
            wasm.Op.i32_gt_s => .i32_gt_s,
            wasm.Op.i32_gt_u => .i32_gt_u,
            wasm.Op.i32_le_s => .i32_le_s,
            wasm.Op.i32_le_u => .i32_le_u,
            wasm.Op.i32_ge_s => .i32_ge_s,
            wasm.Op.i32_ge_u => .i32_ge_u,

            // i64 comparison
            wasm.Op.i64_eqz => .i64_eqz,
            wasm.Op.i64_eq => .i64_eq,
            wasm.Op.i64_ne => .i64_ne,
            wasm.Op.i64_lt_s => .i64_lt_s,
            wasm.Op.i64_lt_u => .i64_lt_u,
            wasm.Op.i64_gt_s => .i64_gt_s,
            wasm.Op.i64_gt_u => .i64_gt_u,
            wasm.Op.i64_le_s => .i64_le_s,
            wasm.Op.i64_le_u => .i64_le_u,
            wasm.Op.i64_ge_s => .i64_ge_s,
            wasm.Op.i64_ge_u => .i64_ge_u,

            // f32 comparison
            wasm.Op.f32_eq => .f32_eq,
            wasm.Op.f32_ne => .f32_ne,
            wasm.Op.f32_lt => .f32_lt,
            wasm.Op.f32_gt => .f32_gt,
            wasm.Op.f32_le => .f32_le,
            wasm.Op.f32_ge => .f32_ge,

            // f64 comparison
            wasm.Op.f64_eq => .f64_eq,
            wasm.Op.f64_ne => .f64_ne,
            wasm.Op.f64_lt => .f64_lt,
            wasm.Op.f64_gt => .f64_gt,
            wasm.Op.f64_le => .f64_le,
            wasm.Op.f64_ge => .f64_ge,

            // i32 arithmetic
            wasm.Op.i32_clz => .i32_clz,
            wasm.Op.i32_ctz => .i32_ctz,
            wasm.Op.i32_popcnt => .i32_popcnt,
            wasm.Op.i32_add => .i32_add,
            wasm.Op.i32_sub => .i32_sub,
            wasm.Op.i32_mul => .i32_mul,
            wasm.Op.i32_div_s => .i32_div_s,
            wasm.Op.i32_div_u => .i32_div_u,
            wasm.Op.i32_rem_s => .i32_rem_s,
            wasm.Op.i32_rem_u => .i32_rem_u,
            wasm.Op.i32_and => .i32_and,
            wasm.Op.i32_or => .i32_or,
            wasm.Op.i32_xor => .i32_xor,
            wasm.Op.i32_shl => .i32_shl,
            wasm.Op.i32_shr_s => .i32_shr_s,
            wasm.Op.i32_shr_u => .i32_shr_u,
            wasm.Op.i32_rotl => .i32_rotl,
            wasm.Op.i32_rotr => .i32_rotr,

            // i64 arithmetic
            wasm.Op.i64_clz => .i64_clz,
            wasm.Op.i64_ctz => .i64_ctz,
            wasm.Op.i64_popcnt => .i64_popcnt,
            wasm.Op.i64_add => .i64_add,
            wasm.Op.i64_sub => .i64_sub,
            wasm.Op.i64_mul => .i64_mul,
            wasm.Op.i64_div_s => .i64_div_s,
            wasm.Op.i64_div_u => .i64_div_u,
            wasm.Op.i64_rem_s => .i64_rem_s,
            wasm.Op.i64_rem_u => .i64_rem_u,
            wasm.Op.i64_and => .i64_and,
            wasm.Op.i64_or => .i64_or,
            wasm.Op.i64_xor => .i64_xor,
            wasm.Op.i64_shl => .i64_shl,
            wasm.Op.i64_shr_s => .i64_shr_s,
            wasm.Op.i64_shr_u => .i64_shr_u,
            wasm.Op.i64_rotl => .i64_rotl,
            wasm.Op.i64_rotr => .i64_rotr,

            // f32 arithmetic
            wasm.Op.f32_abs => .f32_abs,
            wasm.Op.f32_neg => .f32_neg,
            wasm.Op.f32_ceil => .f32_ceil,
            wasm.Op.f32_floor => .f32_floor,
            wasm.Op.f32_trunc => .f32_trunc,
            wasm.Op.f32_nearest => .f32_nearest,
            wasm.Op.f32_sqrt => .f32_sqrt,
            wasm.Op.f32_add => .f32_add,
            wasm.Op.f32_sub => .f32_sub,
            wasm.Op.f32_mul => .f32_mul,
            wasm.Op.f32_div => .f32_div,
            wasm.Op.f32_min => .f32_min,
            wasm.Op.f32_max => .f32_max,
            wasm.Op.f32_copysign => .f32_copysign,

            // f64 arithmetic
            wasm.Op.f64_abs => .f64_abs,
            wasm.Op.f64_neg => .f64_neg,
            wasm.Op.f64_ceil => .f64_ceil,
            wasm.Op.f64_floor => .f64_floor,
            wasm.Op.f64_trunc => .f64_trunc,
            wasm.Op.f64_nearest => .f64_nearest,
            wasm.Op.f64_sqrt => .f64_sqrt,
            wasm.Op.f64_add => .f64_add,
            wasm.Op.f64_sub => .f64_sub,
            wasm.Op.f64_mul => .f64_mul,
            wasm.Op.f64_div => .f64_div,
            wasm.Op.f64_min => .f64_min,
            wasm.Op.f64_max => .f64_max,
            wasm.Op.f64_copysign => .f64_copysign,

            // Conversions
            wasm.Op.i32_wrap_i64 => .i32_wrap_i64,
            wasm.Op.i32_trunc_f32_s => .i32_trunc_f32_s,
            wasm.Op.i32_trunc_f32_u => .i32_trunc_f32_u,
            wasm.Op.i32_trunc_f64_s => .i32_trunc_f64_s,
            wasm.Op.i32_trunc_f64_u => .i32_trunc_f64_u,
            wasm.Op.i64_extend_i32_s => .i64_extend_i32_s,
            wasm.Op.i64_extend_i32_u => .i64_extend_i32_u,
            wasm.Op.i64_trunc_f32_s => .i64_trunc_f32_s,
            wasm.Op.i64_trunc_f32_u => .i64_trunc_f32_u,
            wasm.Op.i64_trunc_f64_s => .i64_trunc_f64_s,
            wasm.Op.i64_trunc_f64_u => .i64_trunc_f64_u,
            wasm.Op.f32_convert_i32_s => .f32_convert_i32_s,
            wasm.Op.f32_convert_i32_u => .f32_convert_i32_u,
            wasm.Op.f32_convert_i64_s => .f32_convert_i64_s,
            wasm.Op.f32_convert_i64_u => .f32_convert_i64_u,
            wasm.Op.f32_demote_f64 => .f32_demote_f64,
            wasm.Op.f64_convert_i32_s => .f64_convert_i32_s,
            wasm.Op.f64_convert_i32_u => .f64_convert_i32_u,
            wasm.Op.f64_convert_i64_s => .f64_convert_i64_s,
            wasm.Op.f64_convert_i64_u => .f64_convert_i64_u,
            wasm.Op.f64_promote_f32 => .f64_promote_f32,
            wasm.Op.i32_reinterpret_f32 => .i32_reinterpret_f32,
            wasm.Op.i64_reinterpret_f64 => .i64_reinterpret_f64,
            wasm.Op.f32_reinterpret_i32 => .f32_reinterpret_i32,
            wasm.Op.f64_reinterpret_i64 => .f64_reinterpret_i64,

            // Sign extension
            wasm.Op.i32_extend8_s => .i32_extend8_s,
            wasm.Op.i32_extend16_s => .i32_extend16_s,
            wasm.Op.i64_extend8_s => .i64_extend8_s,
            wasm.Op.i64_extend16_s => .i64_extend16_s,
            wasm.Op.i64_extend32_s => .i64_extend32_s,

            else => return DecodeError.InvalidOpcode,
        };
    }

    // ========================================================================
    // Reading primitives
    // ========================================================================

    fn readByte(self: *Self) ?u8 {
        if (self.pos >= self.bytes.len) return null;
        const b = self.bytes[self.pos];
        self.pos += 1;
        return b;
    }

    fn readU32(self: *Self) u32 {
        return @intCast(self.readULEB128());
    }

    fn readI32(self: *Self) i32 {
        return @intCast(self.readSLEB128());
    }

    fn readI64(self: *Self) i64 {
        return self.readSLEB128();
    }

    fn readF32(self: *Self) f32 {
        if (self.pos + 4 > self.bytes.len) return 0;
        const bytes = self.bytes[self.pos..][0..4];
        self.pos += 4;
        return @bitCast(std.mem.readInt(u32, bytes, .little));
    }

    fn readF64(self: *Self) f64 {
        if (self.pos + 8 > self.bytes.len) return 0;
        const bytes = self.bytes[self.pos..][0..8];
        self.pos += 8;
        return @bitCast(std.mem.readInt(u64, bytes, .little));
    }

    fn readULEB128(self: *Self) u64 {
        var result: u64 = 0;
        var shift: u6 = 0;
        while (true) {
            const byte = self.readByte() orelse return result;
            result |= @as(u64, byte & 0x7F) << shift;
            if (byte & 0x80 == 0) return result;
            shift +|= 7;
        }
    }

    fn readSLEB128(self: *Self) i64 {
        var result: i64 = 0;
        var shift: u6 = 0;
        var byte: u8 = 0;
        while (true) {
            byte = self.readByte() orelse return result;
            const low7: u7 = @truncate(byte);
            result |= @as(i64, low7) << shift;
            shift +|= 7;
            if (byte & 0x80 == 0) break;
        }
        // Sign extend
        if (shift < 64 and (byte & 0x40) != 0) {
            result |= ~@as(i64, 0) << shift;
        }
        return result;
    }

    fn readMemArg(self: *Self) MemArg {
        return .{
            .align_ = self.readU32(),
            .offset = self.readU32(),
        };
    }

    fn readBlockType(self: *Self) !BlockData {
        const byte = self.readByte() orelse return DecodeError.UnexpectedEnd;

        // Block type encoding:
        // 0x40 = empty (no params, no results)
        // 0x7F = i32 result
        // 0x7E = i64 result
        // 0x7D = f32 result
        // 0x7C = f64 result
        // Otherwise it's a signed LEB128 type index (for multi-value)

        return switch (byte) {
            0x40 => BlockData{ .params = 0, .results = 0 },
            0x7F, 0x7E, 0x7D, 0x7C => BlockData{ .params = 0, .results = 1 },
            else => {
                // For negative values (type indices), we'd need multi-value support
                // For now, treat as single result
                return BlockData{ .params = 0, .results = 1 };
            },
        };
    }

    fn readBrTable(self: *Self) !WasmOp {
        const count = self.readU32();
        var targets = try self.allocator.alloc(u32, count);
        for (0..count) |i| {
            targets[i] = self.readU32();
        }
        const default = self.readU32();
        return WasmOp{ .br_table = .{ .targets = targets, .default = default } };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "decode i32.const 42" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // i32.const 42, end
    const bytes = [_]u8{ 0x41, 0x2A, 0x0B };
    var decoder = Decoder.init(allocator, &bytes);

    const ops = try decoder.decodeAll();
    defer allocator.free(ops);

    try testing.expectEqual(@as(usize, 2), ops.len);
    try testing.expectEqual(WasmOp{ .i32_const = 42 }, ops[0]);
    try testing.expectEqual(WasmOp.end, ops[1]);
}

test "decode local.get 0, local.get 1, i32.add, end" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bytes = [_]u8{
        0x20, 0x00, // local.get 0
        0x20, 0x01, // local.get 1
        0x6A, // i32.add
        0x0B, // end
    };
    var decoder = Decoder.init(allocator, &bytes);

    const ops = try decoder.decodeAll();
    defer allocator.free(ops);

    try testing.expectEqual(@as(usize, 4), ops.len);
    try testing.expectEqual(WasmOp{ .local_get = 0 }, ops[0]);
    try testing.expectEqual(WasmOp{ .local_get = 1 }, ops[1]);
    try testing.expectEqual(WasmOp.i32_add, ops[2]);
    try testing.expectEqual(WasmOp.end, ops[3]);
}

test "decode block with i32 result" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bytes = [_]u8{
        0x02, 0x7F, // block (result i32)
        0x41, 0x01, // i32.const 1
        0x0B, // end block
        0x0B, // end function
    };
    var decoder = Decoder.init(allocator, &bytes);

    const ops = try decoder.decodeAll();
    defer allocator.free(ops);

    try testing.expectEqual(@as(usize, 4), ops.len);
    try testing.expect(ops[0] == .block);
    try testing.expectEqual(@as(usize, 0), ops[0].block.params);
    try testing.expectEqual(@as(usize, 1), ops[0].block.results);
}

test "decode if-else" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bytes = [_]u8{
        0x04, 0x7F, // if (result i32)
        0x41, 0x01, // i32.const 1
        0x05, // else
        0x41, 0x00, // i32.const 0
        0x0B, // end
    };
    var decoder = Decoder.init(allocator, &bytes);

    const ops = try decoder.decodeAll();
    defer allocator.free(ops);

    try testing.expectEqual(@as(usize, 5), ops.len);
    try testing.expect(ops[0] == .if_op);
    try testing.expectEqual(WasmOp{ .i32_const = 1 }, ops[1]);
    try testing.expect(ops[2] == .else_op);
    try testing.expectEqual(WasmOp{ .i32_const = 0 }, ops[3]);
    try testing.expect(ops[4] == .end);
}

test "decode memory load/store" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bytes = [_]u8{
        0x28, 0x02, 0x00, // i32.load align=2 offset=0
        0x36, 0x02, 0x04, // i32.store align=2 offset=4
        0x0B, // end
    };
    var decoder = Decoder.init(allocator, &bytes);

    const ops = try decoder.decodeAll();
    defer allocator.free(ops);

    try testing.expectEqual(@as(usize, 3), ops.len);
    try testing.expect(ops[0] == .i32_load);
    try testing.expectEqual(@as(u32, 2), ops[0].i32_load.align_);
    try testing.expectEqual(@as(u32, 0), ops[0].i32_load.offset);
    try testing.expect(ops[1] == .i32_store);
    try testing.expectEqual(@as(u32, 2), ops[1].i32_store.align_);
    try testing.expectEqual(@as(u32, 4), ops[1].i32_store.offset);
}

test "decode loop with br" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bytes = [_]u8{
        0x03, 0x40, // loop (empty)
        0x0C, 0x00, // br 0 (back to loop)
        0x0B, // end loop
        0x0B, // end function
    };
    var decoder = Decoder.init(allocator, &bytes);

    const ops = try decoder.decodeAll();
    defer allocator.free(ops);

    try testing.expectEqual(@as(usize, 4), ops.len);
    try testing.expect(ops[0] == .loop);
    try testing.expectEqual(WasmOp{ .br = 0 }, ops[1]);
}

test "toBasicOperator conversion" {
    const testing = std.testing;

    const op1: WasmOp = .{ .i32_const = 42 };
    const basic1 = op1.toBasicOperator();
    try testing.expect(basic1 != null);
    try testing.expectEqual(WasmOperator{ .i32_const = 42 }, basic1.?);

    const op2: WasmOp = .i32_add;
    const basic2 = op2.toBasicOperator();
    try testing.expect(basic2 != null);
    try testing.expectEqual(WasmOperator.i32_add, basic2.?);

    // Memory ops return null (not in basic operator)
    const op3: WasmOp = .{ .i32_load = .{ .align_ = 2, .offset = 0 } };
    const basic3 = op3.toBasicOperator();
    try testing.expect(basic3 == null);
}
