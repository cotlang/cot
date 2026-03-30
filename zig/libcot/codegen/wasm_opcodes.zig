//! WebAssembly opcode constants.
//! Reference: https://webassembly.github.io/spec/core/binary/instructions.html

const std = @import("std");

// ============================================================================
// Value Types
// ============================================================================

pub const ValType = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
    funcref = 0x70,
    externref = 0x6F,
};

// ============================================================================
// Section IDs
// ============================================================================

pub const Section = enum(u8) {
    custom = 0,
    type = 1,
    import = 2,
    function = 3,
    table = 4,
    memory = 5,
    global = 6,
    @"export" = 7,
    start = 8,
    element = 9,
    code = 10,
    data = 11,
    data_count = 12,
};

// ============================================================================
// Export Kinds
// ============================================================================

pub const ExportKind = enum(u8) {
    func = 0,
    table = 1,
    memory = 2,
    global = 3,
};

// ============================================================================
// Control Instructions
// ============================================================================

pub const Op = struct {
    // Control flow
    pub const unreachable_op: u8 = 0x00;
    pub const nop: u8 = 0x01;
    pub const block: u8 = 0x02;
    pub const loop: u8 = 0x03;
    pub const if_op: u8 = 0x04;
    pub const else_op: u8 = 0x05;
    pub const end: u8 = 0x0B;
    pub const br: u8 = 0x0C;
    pub const br_if: u8 = 0x0D;
    pub const br_table: u8 = 0x0E;
    pub const return_op: u8 = 0x0F;
    pub const call: u8 = 0x10;
    pub const call_indirect: u8 = 0x11;
    pub const return_call: u8 = 0x12;

    // Parametric instructions
    pub const drop: u8 = 0x1A;
    pub const select: u8 = 0x1B;

    // Variable instructions
    pub const local_get: u8 = 0x20;
    pub const local_set: u8 = 0x21;
    pub const local_tee: u8 = 0x22;
    pub const global_get: u8 = 0x23;
    pub const global_set: u8 = 0x24;

    // Memory instructions
    pub const i32_load: u8 = 0x28;
    pub const i64_load: u8 = 0x29;
    pub const f32_load: u8 = 0x2A;
    pub const f64_load: u8 = 0x2B;
    pub const i32_load8_s: u8 = 0x2C;
    pub const i32_load8_u: u8 = 0x2D;
    pub const i32_load16_s: u8 = 0x2E;
    pub const i32_load16_u: u8 = 0x2F;
    pub const i64_load8_s: u8 = 0x30;
    pub const i64_load8_u: u8 = 0x31;
    pub const i64_load16_s: u8 = 0x32;
    pub const i64_load16_u: u8 = 0x33;
    pub const i64_load32_s: u8 = 0x34;
    pub const i64_load32_u: u8 = 0x35;
    pub const i32_store: u8 = 0x36;
    pub const i64_store: u8 = 0x37;
    pub const f32_store: u8 = 0x38;
    pub const f64_store: u8 = 0x39;
    pub const i32_store8: u8 = 0x3A;
    pub const i32_store16: u8 = 0x3B;
    pub const i64_store8: u8 = 0x3C;
    pub const i64_store16: u8 = 0x3D;
    pub const i64_store32: u8 = 0x3E;
    pub const memory_size: u8 = 0x3F;
    pub const memory_grow: u8 = 0x40;

    // Numeric instructions - constants
    pub const i32_const: u8 = 0x41;
    pub const i64_const: u8 = 0x42;
    pub const f32_const: u8 = 0x43;
    pub const f64_const: u8 = 0x44;

    // Numeric instructions - i32 comparison
    pub const i32_eqz: u8 = 0x45;
    pub const i32_eq: u8 = 0x46;
    pub const i32_ne: u8 = 0x47;
    pub const i32_lt_s: u8 = 0x48;
    pub const i32_lt_u: u8 = 0x49;
    pub const i32_gt_s: u8 = 0x4A;
    pub const i32_gt_u: u8 = 0x4B;
    pub const i32_le_s: u8 = 0x4C;
    pub const i32_le_u: u8 = 0x4D;
    pub const i32_ge_s: u8 = 0x4E;
    pub const i32_ge_u: u8 = 0x4F;

    // Numeric instructions - i64 comparison
    pub const i64_eqz: u8 = 0x50;
    pub const i64_eq: u8 = 0x51;
    pub const i64_ne: u8 = 0x52;
    pub const i64_lt_s: u8 = 0x53;
    pub const i64_lt_u: u8 = 0x54;
    pub const i64_gt_s: u8 = 0x55;
    pub const i64_gt_u: u8 = 0x56;
    pub const i64_le_s: u8 = 0x57;
    pub const i64_le_u: u8 = 0x58;
    pub const i64_ge_s: u8 = 0x59;
    pub const i64_ge_u: u8 = 0x5A;

    // Numeric instructions - f32 comparison
    pub const f32_eq: u8 = 0x5B;
    pub const f32_ne: u8 = 0x5C;
    pub const f32_lt: u8 = 0x5D;
    pub const f32_gt: u8 = 0x5E;
    pub const f32_le: u8 = 0x5F;
    pub const f32_ge: u8 = 0x60;

    // Numeric instructions - f64 comparison
    pub const f64_eq: u8 = 0x61;
    pub const f64_ne: u8 = 0x62;
    pub const f64_lt: u8 = 0x63;
    pub const f64_gt: u8 = 0x64;
    pub const f64_le: u8 = 0x65;
    pub const f64_ge: u8 = 0x66;

    // Numeric instructions - i32 arithmetic
    pub const i32_clz: u8 = 0x67;
    pub const i32_ctz: u8 = 0x68;
    pub const i32_popcnt: u8 = 0x69;
    pub const i32_add: u8 = 0x6A;
    pub const i32_sub: u8 = 0x6B;
    pub const i32_mul: u8 = 0x6C;
    pub const i32_div_s: u8 = 0x6D;
    pub const i32_div_u: u8 = 0x6E;
    pub const i32_rem_s: u8 = 0x6F;
    pub const i32_rem_u: u8 = 0x70;
    pub const i32_and: u8 = 0x71;
    pub const i32_or: u8 = 0x72;
    pub const i32_xor: u8 = 0x73;
    pub const i32_shl: u8 = 0x74;
    pub const i32_shr_s: u8 = 0x75;
    pub const i32_shr_u: u8 = 0x76;
    pub const i32_rotl: u8 = 0x77;
    pub const i32_rotr: u8 = 0x78;

    // Numeric instructions - i64 arithmetic
    pub const i64_clz: u8 = 0x79;
    pub const i64_ctz: u8 = 0x7A;
    pub const i64_popcnt: u8 = 0x7B;
    pub const i64_add: u8 = 0x7C;
    pub const i64_sub: u8 = 0x7D;
    pub const i64_mul: u8 = 0x7E;
    pub const i64_div_s: u8 = 0x7F;
    pub const i64_div_u: u8 = 0x80;
    pub const i64_rem_s: u8 = 0x81;
    pub const i64_rem_u: u8 = 0x82;
    pub const i64_and: u8 = 0x83;
    pub const i64_or: u8 = 0x84;
    pub const i64_xor: u8 = 0x85;
    pub const i64_shl: u8 = 0x86;
    pub const i64_shr_s: u8 = 0x87;
    pub const i64_shr_u: u8 = 0x88;
    pub const i64_rotl: u8 = 0x89;
    pub const i64_rotr: u8 = 0x8A;

    // Numeric instructions - f32 arithmetic
    pub const f32_abs: u8 = 0x8B;
    pub const f32_neg: u8 = 0x8C;
    pub const f32_ceil: u8 = 0x8D;
    pub const f32_floor: u8 = 0x8E;
    pub const f32_trunc: u8 = 0x8F;
    pub const f32_nearest: u8 = 0x90;
    pub const f32_sqrt: u8 = 0x91;
    pub const f32_add: u8 = 0x92;
    pub const f32_sub: u8 = 0x93;
    pub const f32_mul: u8 = 0x94;
    pub const f32_div: u8 = 0x95;
    pub const f32_min: u8 = 0x96;
    pub const f32_max: u8 = 0x97;
    pub const f32_copysign: u8 = 0x98;

    // Numeric instructions - f64 arithmetic
    pub const f64_abs: u8 = 0x99;
    pub const f64_neg: u8 = 0x9A;
    pub const f64_ceil: u8 = 0x9B;
    pub const f64_floor: u8 = 0x9C;
    pub const f64_trunc: u8 = 0x9D;
    pub const f64_nearest: u8 = 0x9E;
    pub const f64_sqrt: u8 = 0x9F;
    pub const f64_add: u8 = 0xA0;
    pub const f64_sub: u8 = 0xA1;
    pub const f64_mul: u8 = 0xA2;
    pub const f64_div: u8 = 0xA3;
    pub const f64_min: u8 = 0xA4;
    pub const f64_max: u8 = 0xA5;
    pub const f64_copysign: u8 = 0xA6;

    // Numeric instructions - conversions
    pub const i32_wrap_i64: u8 = 0xA7;
    pub const i32_trunc_f32_s: u8 = 0xA8;
    pub const i32_trunc_f32_u: u8 = 0xA9;
    pub const i32_trunc_f64_s: u8 = 0xAA;
    pub const i32_trunc_f64_u: u8 = 0xAB;
    pub const i64_extend_i32_s: u8 = 0xAC;
    pub const i64_extend_i32_u: u8 = 0xAD;
    pub const i64_trunc_f32_s: u8 = 0xAE;
    pub const i64_trunc_f32_u: u8 = 0xAF;
    pub const i64_trunc_f64_s: u8 = 0xB0;
    pub const i64_trunc_f64_u: u8 = 0xB1;
    pub const f32_convert_i32_s: u8 = 0xB2;
    pub const f32_convert_i32_u: u8 = 0xB3;
    pub const f32_convert_i64_s: u8 = 0xB4;
    pub const f32_convert_i64_u: u8 = 0xB5;
    pub const f32_demote_f64: u8 = 0xB6;
    pub const f64_convert_i32_s: u8 = 0xB7;
    pub const f64_convert_i32_u: u8 = 0xB8;
    pub const f64_convert_i64_s: u8 = 0xB9;
    pub const f64_convert_i64_u: u8 = 0xBA;
    pub const f64_promote_f32: u8 = 0xBB;
    pub const i32_reinterpret_f32: u8 = 0xBC;
    pub const i64_reinterpret_f64: u8 = 0xBD;
    pub const f32_reinterpret_i32: u8 = 0xBE;
    pub const f64_reinterpret_i64: u8 = 0xBF;

    // Sign extension (Wasm 1.0+)
    pub const i32_extend8_s: u8 = 0xC0;
    pub const i32_extend16_s: u8 = 0xC1;
    pub const i64_extend8_s: u8 = 0xC2;
    pub const i64_extend16_s: u8 = 0xC3;
    pub const i64_extend32_s: u8 = 0xC4;
};

// ============================================================================
// Block Types
// ============================================================================

/// Block type for void blocks (no result)
pub const BLOCK_VOID: u8 = 0x40;

pub const BlockType = union(enum) {
    empty: void, // BLOCK_VOID
    val_type: ValType,
    type_idx: u32,

    pub fn encode(self: BlockType) u8 {
        return switch (self) {
            .empty => BLOCK_VOID,
            .val_type => |v| @intFromEnum(v),
            .type_idx => unreachable, // requires s33 LEB128
        };
    }
};

// ============================================================================
// Module Encoding Constants
// ============================================================================

pub const MAGIC: [4]u8 = .{ 0x00, 0x61, 0x73, 0x6D }; // \0asm
pub const VERSION: [4]u8 = .{ 0x01, 0x00, 0x00, 0x00 }; // version 1

pub const FUNC_TYPE_TAG: u8 = 0x60;

/// Limits encoding (Wasm spec §5.3.5)
pub const LIMITS_NO_MAX: u8 = 0x00;
pub const LIMITS_WITH_MAX: u8 = 0x01;

/// Global mutability (Wasm spec §5.3.6)
pub const GLOBAL_IMMUTABLE: u8 = 0x00;
pub const GLOBAL_MUTABLE: u8 = 0x01;

/// Import/export descriptor kinds (Wasm spec §5.4.1/§5.4.2)
pub const IMPORT_KIND_FUNC: u8 = 0x00;

/// FC-prefixed opcode prefix (bulk memory, saturating truncation)
pub const FC_PREFIX: u8 = 0xFC;
pub const FC_MEMORY_COPY: u8 = 0x0A;

/// Reserved memory index (always 0 for single-memory modules)
pub const MEMORY_IDX_ZERO: u8 = 0x00;

/// Reserved table index (always 0 for single-table modules)
pub const TABLE_IDX_ZERO: u8 = 0x00;

/// Reference type opcodes (Wasm reference types proposal)
pub const REF_NULL: u8 = 0xD0;
pub const REF_FUNC: u8 = 0xD2;

// ============================================================================
// Tests
// ============================================================================

test "magic and version" {
    try std.testing.expectEqualSlices(u8, &MAGIC, "\x00asm");
    try std.testing.expectEqual(@as(u8, 1), VERSION[0]);
}

test "value types" {
    try std.testing.expectEqual(@as(u8, 0x7F), @intFromEnum(ValType.i32));
    try std.testing.expectEqual(@as(u8, 0x7E), @intFromEnum(ValType.i64));
    try std.testing.expectEqual(@as(u8, 0x7D), @intFromEnum(ValType.f32));
    try std.testing.expectEqual(@as(u8, 0x7C), @intFromEnum(ValType.f64));
}

test "section IDs" {
    try std.testing.expectEqual(@as(u8, 1), @intFromEnum(Section.type));
    try std.testing.expectEqual(@as(u8, 3), @intFromEnum(Section.function));
    try std.testing.expectEqual(@as(u8, 7), @intFromEnum(Section.@"export"));
    try std.testing.expectEqual(@as(u8, 10), @intFromEnum(Section.code));
}

test "basic opcodes" {
    try std.testing.expectEqual(@as(u8, 0x41), Op.i32_const);
    try std.testing.expectEqual(@as(u8, 0x42), Op.i64_const);
    try std.testing.expectEqual(@as(u8, 0x6A), Op.i32_add);
    try std.testing.expectEqual(@as(u8, 0x7C), Op.i64_add);
    try std.testing.expectEqual(@as(u8, 0x20), Op.local_get);
    try std.testing.expectEqual(@as(u8, 0x0F), Op.return_op);
    try std.testing.expectEqual(@as(u8, 0x0B), Op.end);
}

test "block type encoding" {
    const empty: BlockType = .empty;
    const i32_block = BlockType{ .val_type = .i32 };
    const i64_block = BlockType{ .val_type = .i64 };

    try std.testing.expectEqual(@as(u8, 0x40), empty.encode());
    try std.testing.expectEqual(@as(u8, 0x7F), i32_block.encode());
    try std.testing.expectEqual(@as(u8, 0x7E), i64_block.encode());
}
