//! Opcode Specification - Single Source of Truth
//!
//! This file defines all bytecode opcodes in a structured format.
//! Other modules derive their behavior from this specification:
//!   - opcodes.zig: Enum and operandLength()
//!   - vm_opcodes.zig: Handler operand decoding
//!   - emit_instruction.zig: Emitter byte encoding
//!   - disasm.zig: Disassembler decoding
//!
//! When adding or modifying opcodes, ONLY change this file.
//! Everything else is derived at comptime.

const std = @import("std");

/// Operand types with their bit widths
pub const OperandType = enum(u8) {
    /// 4-bit register (high nibble of byte)
    reg4_hi,
    /// 4-bit register (low nibble of byte)
    reg4_lo,
    /// 4-bit padding (low nibble, always 0)
    pad4,
    /// 4-bit flags (low nibble)
    flags4,
    /// 8-bit immediate value
    imm8,
    /// 8-bit slot index
    slot8,
    /// 8-bit padding (always 0)
    pad8,
    /// 16-bit immediate value
    imm16,
    /// 16-bit slot index
    slot16,
    /// 16-bit constant/function index
    idx16,
    /// 32-bit immediate value
    imm32,

    pub fn bits(self: OperandType) u8 {
        return switch (self) {
            .reg4_hi, .reg4_lo, .pad4, .flags4 => 4,
            .imm8, .slot8, .pad8 => 8,
            .imm16, .slot16, .idx16 => 16,
            .imm32 => 32,
        };
    }
};

/// Opcode specification
pub const OpcodeSpec = struct {
    /// Opcode name (matches enum field name)
    name: []const u8,
    /// Opcode value (0x00-0xFF)
    value: u8,
    /// Operand format - sequence of operand types
    /// MUST be byte-aligned (total bits divisible by 8)
    operands: []const OperandType,
    /// Brief description
    desc: []const u8,

    /// Calculate operand bytes from spec
    pub fn operandBytes(self: OpcodeSpec) u8 {
        var total_bits: u16 = 0;
        for (self.operands) |op| {
            total_bits += op.bits();
        }
        std.debug.assert(total_bits % 8 == 0); // Must be byte-aligned
        return @intCast(total_bits / 8);
    }

    /// Total instruction size including opcode byte
    pub fn totalBytes(self: OpcodeSpec) u8 {
        return 1 + self.operandBytes();
    }
};

// Short aliases for cleaner spec definitions
const r_hi = OperandType.reg4_hi;
const r_lo = OperandType.reg4_lo;
const p4 = OperandType.pad4;
const f4 = OperandType.flags4;
const i8_ = OperandType.imm8;
const s8 = OperandType.slot8;
const p8 = OperandType.pad8;
const i16_ = OperandType.imm16;
const s16 = OperandType.slot16;
const x16 = OperandType.idx16;
const i32_ = OperandType.imm32;

/// The authoritative opcode specification
/// Format: { name, value, operands, description }
pub const opcodes = [_]OpcodeSpec{
    // ============================================
    // Core Operations (0x00-0x0F)
    // ============================================
    .{ .name = "nop", .value = 0x00, .operands = &.{}, .desc = "no operation" },
    .{ .name = "halt", .value = 0x01, .operands = &.{}, .desc = "halt execution" },

    // ============================================
    // Register Moves & Immediates (0x10-0x1F)
    // ============================================
    .{ .name = "mov", .value = 0x10, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = rs" },
    .{ .name = "movi", .value = 0x11, .operands = &.{ r_hi, p4, i8_ }, .desc = "rd = sign_extend(imm8)" },
    .{ .name = "movi16", .value = 0x12, .operands = &.{ r_hi, p4, i16_ }, .desc = "rd = sign_extend(imm16)" },
    .{ .name = "movi32", .value = 0x13, .operands = &.{ r_hi, p4, i32_ }, .desc = "rd = sign_extend(imm32)" },
    .{ .name = "load_const", .value = 0x14, .operands = &.{ r_hi, p4, x16 }, .desc = "rd = constants[idx]" },
    .{ .name = "load_null", .value = 0x15, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = null" },
    .{ .name = "load_true", .value = 0x16, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = true" },
    .{ .name = "load_false", .value = 0x17, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = false" },

    // ============================================
    // Local & Global Variables (0x20-0x2F)
    // ============================================
    .{ .name = "load_local", .value = 0x20, .operands = &.{ r_hi, p4, s8 }, .desc = "rd = locals[slot]" },
    .{ .name = "store_local", .value = 0x21, .operands = &.{ r_hi, p4, s8 }, .desc = "locals[slot] = rs" },
    .{ .name = "load_local16", .value = 0x22, .operands = &.{ r_hi, p4, s16 }, .desc = "rd = locals[slot] (16-bit)" },
    .{ .name = "store_local16", .value = 0x23, .operands = &.{ r_hi, p4, s16 }, .desc = "locals[slot] = rs (16-bit)" },
    .{ .name = "load_global", .value = 0x24, .operands = &.{ r_hi, p4, x16 }, .desc = "rd = globals[idx]" },
    .{ .name = "store_global", .value = 0x25, .operands = &.{ r_hi, p4, x16 }, .desc = "globals[idx] = rs" },

    // ============================================
    // Arithmetic (0x30-0x3F)
    // ============================================
    .{ .name = "add", .value = 0x30, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 + rs2" },
    .{ .name = "sub", .value = 0x31, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 - rs2" },
    .{ .name = "mul", .value = 0x32, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 * rs2" },
    .{ .name = "div", .value = 0x33, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 / rs2" },
    .{ .name = "mod", .value = 0x34, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 % rs2" },
    .{ .name = "neg", .value = 0x35, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = -rs" },
    .{ .name = "addi", .value = 0x36, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = rs + imm8" },
    .{ .name = "subi", .value = 0x37, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = rs - imm8" },
    .{ .name = "muli", .value = 0x38, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = rs * imm8" },
    .{ .name = "incr", .value = 0x39, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = rd + 1" },
    .{ .name = "decr", .value = 0x3A, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = rd - 1" },
    .{ .name = "add_dec", .value = 0x3B, .operands = &.{ r_hi, r_lo, r_hi, f4 }, .desc = "rd = rs1 + rs2 (decimal)" },
    .{ .name = "sub_dec", .value = 0x3C, .operands = &.{ r_hi, r_lo, r_hi, f4 }, .desc = "rd = rs1 - rs2 (decimal)" },
    .{ .name = "mul_dec", .value = 0x3D, .operands = &.{ r_hi, r_lo, r_hi, f4 }, .desc = "rd = rs1 * rs2 (decimal)" },
    .{ .name = "div_dec", .value = 0x3E, .operands = &.{ r_hi, r_lo, r_hi, f4 }, .desc = "rd = rs1 / rs2 (decimal)" },
    // 0x3F reserved

    // ============================================
    // Comparison (0x40-0x4F)
    // ============================================
    .{ .name = "cmp_eq", .value = 0x40, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 == rs2" },
    .{ .name = "cmp_ne", .value = 0x41, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 != rs2" },
    .{ .name = "cmp_lt", .value = 0x42, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 < rs2" },
    .{ .name = "cmp_le", .value = 0x43, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 <= rs2" },
    .{ .name = "cmp_gt", .value = 0x44, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 > rs2" },
    .{ .name = "cmp_ge", .value = 0x45, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 >= rs2" },
    .{ .name = "cmp_str_eq", .value = 0x46, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 == rs2 (str)" },
    .{ .name = "cmp_str_lt", .value = 0x47, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 < rs2 (str)" },
    .{ .name = "cmp_str_ne", .value = 0x48, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 != rs2 (str)" },
    .{ .name = "cmp_str_le", .value = 0x49, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 <= rs2 (str)" },
    .{ .name = "cmp_str_gt", .value = 0x4A, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 > rs2 (str)" },
    .{ .name = "cmp_str_ge", .value = 0x4B, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 >= rs2 (str)" },

    // ============================================
    // Logical & Bitwise (0x50-0x5F)
    // ============================================
    .{ .name = "log_and", .value = 0x50, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 and rs2" },
    .{ .name = "log_or", .value = 0x51, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 or rs2" },
    .{ .name = "log_not", .value = 0x52, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = not rs" },
    .{ .name = "bit_and", .value = 0x53, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 & rs2" },
    .{ .name = "bit_or", .value = 0x54, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 | rs2" },
    .{ .name = "bit_xor", .value = 0x55, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 ^ rs2" },
    .{ .name = "bit_not", .value = 0x56, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = ~rs" },
    .{ .name = "is_null", .value = 0x57, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = rs == null" },
    .{ .name = "select", .value = 0x58, .operands = &.{ r_hi, r_lo, r_hi, r_lo }, .desc = "rd = cond ? rs1 : rs2" },
    .{ .name = "ptr_offset", .value = 0x59, .operands = &.{ r_hi, r_lo, i16_ }, .desc = "rd = rs + offset (ptr)" },
    .{ .name = "shl", .value = 0x5A, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 << rs2" },
    .{ .name = "shr", .value = 0x5B, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 >> rs2" },
    .{ .name = "is_type", .value = 0x5C, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = typeof(rs) == type" },

    // ============================================
    // Control Flow - Branches (0x60-0x6F)
    // ============================================
    .{ .name = "jmp", .value = 0x60, .operands = &.{ p8, i16_ }, .desc = "ip += offset" },
    .{ .name = "jmp32", .value = 0x61, .operands = &.{ p8, i32_ }, .desc = "ip += offset (32-bit)" },
    .{ .name = "jz", .value = 0x62, .operands = &.{ r_hi, p4, i16_ }, .desc = "if rs == 0: ip += offset" },
    .{ .name = "jnz", .value = 0x63, .operands = &.{ r_hi, p4, i16_ }, .desc = "if rs != 0: ip += offset" },
    .{ .name = "jeq", .value = 0x64, .operands = &.{ r_hi, r_lo, i16_ }, .desc = "if rs1 == rs2: ip += offset" },
    .{ .name = "jne", .value = 0x65, .operands = &.{ r_hi, r_lo, i16_ }, .desc = "if rs1 != rs2: ip += offset" },
    .{ .name = "jlt", .value = 0x66, .operands = &.{ r_hi, r_lo, i16_ }, .desc = "if rs1 < rs2: ip += offset" },
    .{ .name = "jge", .value = 0x67, .operands = &.{ r_hi, r_lo, i16_ }, .desc = "if rs1 >= rs2: ip += offset" },
    .{ .name = "loop_start", .value = 0x68, .operands = &.{}, .desc = "loop start marker" },
    .{ .name = "loop_end", .value = 0x69, .operands = &.{}, .desc = "loop end marker" },
    .{ .name = "set_error_handler", .value = 0x6A, .operands = &.{ p8, i16_ }, .desc = "set error handler offset" },
    .{ .name = "clear_error_handler", .value = 0x6B, .operands = &.{}, .desc = "clear error handler" },
    .{ .name = "throw", .value = 0x6C, .operands = &.{ r_hi, p4, p8 }, .desc = "throw exception" },

    // ============================================
    // Control Flow - Calls (0x70-0x7F)
    // Format: call [argc:4|stack_argc:4] [idx:16] = 3 bytes
    // ============================================
    .{ .name = "call", .value = 0x70, .operands = &.{ r_hi, r_lo, x16 }, .desc = "call function" },
    .{ .name = "call_external", .value = 0x71, .operands = &.{ r_hi, p4, x16 }, .desc = "call external function" },
    .{ .name = "call_native", .value = 0x72, .operands = &.{ r_hi, p4, x16 }, .desc = "call native function" },
    .{ .name = "call_indirect", .value = 0x73, .operands = &.{ r_hi, r_lo, p8 }, .desc = "call via register" },
    .{ .name = "call_dynamic", .value = 0x74, .operands = &.{ r_hi, p4, x16 }, .desc = "call by name" },
    .{ .name = "ret", .value = 0x75, .operands = &.{ i8_, p8 }, .desc = "return (count=overflow slots)" },
    .{ .name = "ret_val", .value = 0x76, .operands = &.{ r_hi, p4, p8 }, .desc = "return with value" },
    .{ .name = "push_arg", .value = 0x77, .operands = &.{s16}, .desc = "push slot to arg stack" },
    .{ .name = "push_arg_reg", .value = 0x78, .operands = &.{ r_hi, p4, p8 }, .desc = "push register to arg stack" },
    .{ .name = "pop_arg", .value = 0x79, .operands = &.{s16}, .desc = "pop arg stack to slot" },

    // ============================================
    // Record/Field Operations (0x80-0x8F)
    // ============================================
    .{ .name = "new_record", .value = 0x80, .operands = &.{ r_hi, p4, x16 }, .desc = "rd = new record" },
    .{ .name = "free_record", .value = 0x81, .operands = &.{ r_hi, p4, p8 }, .desc = "free record" },
    .{ .name = "load_field", .value = 0x82, .operands = &.{ r_hi, r_lo, x16 }, .desc = "rd = rs.field[idx]" },
    .{ .name = "store_field", .value = 0x83, .operands = &.{ r_hi, r_lo, x16 }, .desc = "rd.field[idx] = rs" },
    .{ .name = "load_field_fast", .value = 0x84, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = rs.field[offset]" },
    .{ .name = "store_field_fast", .value = 0x85, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd.field[offset] = rs" },
    .{ .name = "load_record_buf", .value = 0x86, .operands = &.{ r_hi, f4, x16, s16 }, .desc = "serialize record" },
    .{ .name = "store_record_buf", .value = 0x87, .operands = &.{ r_hi, f4, x16, s16 }, .desc = "deserialize record" },
    .{ .name = "clear_record", .value = 0x88, .operands = &.{ r_hi, p4, p8 }, .desc = "clear record fields" },
    .{ .name = "alloc_buffer", .value = 0x89, .operands = &.{ s8, x16 }, .desc = "allocate buffer" },

    // ============================================
    // Sum Type (Variant) Operations (0x8A-0x8F)
    // ============================================
    .{ .name = "variant_construct", .value = 0x8A, .operands = &.{ r_hi, r_lo, x16 }, .desc = "construct variant" },
    .{ .name = "variant_get_tag", .value = 0x8B, .operands = &.{ r_hi, r_lo, p8 }, .desc = "get variant tag" },
    .{ .name = "variant_get_payload", .value = 0x8C, .operands = &.{ r_hi, r_lo, x16 }, .desc = "get variant payload" },

    // ============================================
    // String Operations (0x90-0x9F)
    // ============================================
    .{ .name = "str_concat", .value = 0x90, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 + rs2" },
    .{ .name = "str_len", .value = 0x91, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = len(rs)" },
    .{ .name = "str_index", .value = 0x92, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs[idx]" },
    .{ .name = "str_slice", .value = 0x93, .operands = &.{ r_hi, r_lo, r_hi, r_lo }, .desc = "rd = rs[start:len]" },
    .{ .name = "str_slice_store", .value = 0x94, .operands = &.{ r_hi, r_lo, r_hi, r_lo }, .desc = "rd[start:len] = val" },
    .{ .name = "str_trim", .value = 0x95, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = trim(rs)" },
    .{ .name = "str_upper", .value = 0x96, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = upper(rs)" },
    .{ .name = "str_lower", .value = 0x97, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = lower(rs)" },
    .{ .name = "str_find", .value = 0x98, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = find(rs1, rs2)" },
    .{ .name = "str_replace", .value = 0x99, .operands = &.{ r_hi, r_lo, r_hi, r_lo }, .desc = "rd = replace(rs, old, new)" },
    .{ .name = "str_setchar", .value = 0x9A, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd[idx] = char" },

    // ============================================
    // Type Conversion (0xA0-0xAF)
    // ============================================
    .{ .name = "to_int", .value = 0xA0, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = int(rs)" },
    .{ .name = "to_str", .value = 0xA1, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = str(rs)" },
    .{ .name = "to_bool", .value = 0xA2, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = bool(rs)" },
    .{ .name = "to_dec", .value = 0xA3, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = decimal(rs, prec)" },
    .{ .name = "to_char", .value = 0xA4, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = char(rs)" },
    .{ .name = "to_fixed_string", .value = 0xA5, .operands = &.{ r_hi, r_lo, x16 }, .desc = "rd = fixed_string(rs, size)" },
    .{ .name = "format_decimal", .value = 0xA6, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = format_decimal(rs, width)" },
    .{ .name = "parse_decimal", .value = 0xA7, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = parse_decimal(rs)" },

    // ============================================
    // Array Operations (0xB0-0xBF)
    // ============================================
    .{ .name = "array_load", .value = 0xB0, .operands = &.{ i8_, s16 }, .desc = "r0 = arr[idx]" },
    .{ .name = "array_store", .value = 0xB1, .operands = &.{ r_hi, r_lo, s16 }, .desc = "arr[idx] = val" },
    .{ .name = "array_len", .value = 0xB2, .operands = &.{ i8_, s16 }, .desc = "rd = len(arr)" },
    .{ .name = "list_len", .value = 0xB3, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = list.len()" },
    .{ .name = "list_clear", .value = 0xB4, .operands = &.{ r_hi, p4, p8 }, .desc = "list.clear()" },
    .{ .name = "list_push_struct", .value = 0xB5, .operands = &.{ r_hi, p4, i8_, s16 }, .desc = "list.push(struct)" },
    .{ .name = "list_get_struct", .value = 0xB6, .operands = &.{ r_hi, r_lo, i8_, i8_, p8 }, .desc = "get struct from list" },
    .{ .name = "list_pop_struct", .value = 0xB7, .operands = &.{ r_hi, p4, i8_, s16 }, .desc = "list.pop() -> struct" },
    .{ .name = "list_set_struct", .value = 0xB8, .operands = &.{ r_hi, r_lo, i8_, s16 }, .desc = "list[idx] = struct" },
    .{ .name = "map_set_struct", .value = 0xB9, .operands = &.{ r_hi, r_lo, i8_, s16 }, .desc = "map[key] = struct" },
    .{ .name = "map_get_struct", .value = 0xBA, .operands = &.{ r_hi, r_lo, i8_, i8_, p8 }, .desc = "get struct from map" },
    .{ .name = "array_slice", .value = 0xBB, .operands = &.{ r_hi, f4, r_hi, r_lo, s8, s8 }, .desc = "rd = arr[start..end]" },
    .{ .name = "array_load_opt", .value = 0xBC, .operands = &.{ i8_, s16, i16_ }, .desc = "r0 = arr[idx] or null" },
    .{ .name = "list_to_slice", .value = 0xBD, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = list.to_slice()" },

    // ============================================
    // Built-in Functions (0xC0-0xCF)
    // ============================================
    .{ .name = "fn_abs", .value = 0xC0, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = abs(rs)" },
    .{ .name = "fn_sqrt", .value = 0xC1, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = sqrt(rs)" },
    .{ .name = "fn_sin", .value = 0xC2, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = sin(rs)" },
    .{ .name = "fn_cos", .value = 0xC3, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = cos(rs)" },
    .{ .name = "fn_tan", .value = 0xC4, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = tan(rs)" },
    .{ .name = "fn_log", .value = 0xC5, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = log(rs)" },
    .{ .name = "fn_log10", .value = 0xC6, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = log10(rs)" },
    .{ .name = "fn_exp", .value = 0xC7, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = exp(rs)" },
    .{ .name = "fn_round", .value = 0xC8, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "rd = round(rs, prec)" },
    .{ .name = "fn_trunc", .value = 0xC9, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = trunc(rs)" },
    .{ .name = "fn_date", .value = 0xCA, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = current date" },
    .{ .name = "fn_time", .value = 0xCB, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = current time" },
    .{ .name = "fn_size", .value = 0xCC, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = size(rs)" },
    .{ .name = "fn_instr", .value = 0xCD, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = instr(rs1, rs2)" },
    .{ .name = "fn_mem", .value = 0xCE, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = available memory" },
    .{ .name = "fn_error", .value = 0xCF, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = last error" },

    // ============================================
    // Terminal I/O (0xD0-0xDF)
    // Format: [rs:4|argc:4] [0] = 2 bytes
    // ============================================
    .{ .name = "print", .value = 0xD0, .operands = &.{ r_hi, r_lo, p8 }, .desc = "print without newline" },
    .{ .name = "println", .value = 0xD1, .operands = &.{ r_hi, r_lo, p8 }, .desc = "print with newline" },
    .{ .name = "readln", .value = 0xD2, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = read line" },
    .{ .name = "readkey", .value = 0xD3, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = read key" },
    .{ .name = "log", .value = 0xD4, .operands = &.{ r_hi, r_lo, p8 }, .desc = "log to stderr" },

    // ============================================
    // Map Operations (0xD5-0xDF)
    // ============================================
    .{ .name = "map_new", .value = 0xD5, .operands = &.{ r_hi, f4, p8 }, .desc = "rd = new map" },
    .{ .name = "map_set", .value = 0xD6, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "map[key] = val" },
    .{ .name = "map_get", .value = 0xD7, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = map[key]" },
    .{ .name = "map_delete", .value = 0xD8, .operands = &.{ r_hi, r_lo, p8 }, .desc = "delete map[key]" },
    .{ .name = "map_has", .value = 0xD9, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = map.has(key)" },
    .{ .name = "map_len", .value = 0xDA, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = map.len()" },
    .{ .name = "map_clear", .value = 0xDB, .operands = &.{ r_hi, p4, p8 }, .desc = "map.clear()" },
    .{ .name = "map_keys", .value = 0xDC, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = map.keys()" },
    .{ .name = "map_values", .value = 0xDD, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = map.values()" },
    .{ .name = "map_get_at", .value = 0xDE, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = map[pos]" },
    .{ .name = "map_set_at", .value = 0xDF, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "map[pos] = val" },

    // ============================================
    // Quickened/Specialized Opcodes (0xE0-0xEF)
    // ============================================
    .{ .name = "add_int", .value = 0xE0, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 + rs2 (int)" },
    .{ .name = "sub_int", .value = 0xE1, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 - rs2 (int)" },
    .{ .name = "mul_int", .value = 0xE2, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 * rs2 (int)" },
    .{ .name = "div_int", .value = 0xE3, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 / rs2 (int)" },
    .{ .name = "cmp_lt_int", .value = 0xE4, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 < rs2 (int)" },
    .{ .name = "cmp_le_int", .value = 0xE5, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 <= rs2 (int)" },
    .{ .name = "cmp_gt_int", .value = 0xE6, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 > rs2 (int)" },
    .{ .name = "cmp_ge_int", .value = 0xE7, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 >= rs2 (int)" },
    .{ .name = "cmp_eq_int", .value = 0xE8, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 == rs2 (int)" },
    .{ .name = "cmp_ne_int", .value = 0xE9, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = rs1 != rs2 (int)" },
    .{ .name = "incr_int", .value = 0xEA, .operands = &.{ r_hi, p4, p8 }, .desc = "rd++ (int)" },
    .{ .name = "decr_int", .value = 0xEB, .operands = &.{ r_hi, p4, p8 }, .desc = "rd-- (int)" },
    .{ .name = "fn_datetime", .value = 0xEC, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = datetime string" },

    // ============================================
    // List Operations (0xED-0xEF)
    // ============================================
    .{ .name = "list_new", .value = 0xED, .operands = &.{ r_hi, p4, p8 }, .desc = "rd = new list" },
    .{ .name = "list_push", .value = 0xEE, .operands = &.{ r_hi, r_lo, p8 }, .desc = "list.push(val)" },
    .{ .name = "list_pop", .value = 0xEF, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = list.pop()" },

    // ============================================
    // Debug & Meta (0xF0-0xFF)
    // ============================================
    .{ .name = "debug_break", .value = 0xF0, .operands = &.{}, .desc = "debugger breakpoint" },
    .{ .name = "debug_line", .value = 0xF1, .operands = &.{ p8, i16_ }, .desc = "set source line" },
    .{ .name = "assert", .value = 0xF2, .operands = &.{ r_hi, p4, p8 }, .desc = "assert rs is true" },
    .{ .name = "weak_ref", .value = 0xF3, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = weak(rs)" },
    .{ .name = "weak_load", .value = 0xF4, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = *weak_rs" },

    // ARC Operations (0xF5-0xF7)
    .{ .name = "arc_retain", .value = 0xF5, .operands = &.{ r_hi, p4, p8 }, .desc = "retain rs" },
    .{ .name = "arc_release", .value = 0xF6, .operands = &.{ r_hi, p4, p8 }, .desc = "release rs" },
    .{ .name = "arc_move", .value = 0xF7, .operands = &.{ r_hi, r_lo, p8 }, .desc = "rd = move(rs)" },

    // Closure Operations (0xF8-0xF9)
    .{ .name = "make_closure", .value = 0xF8, .operands = &.{ r_hi, r_lo, x16 }, .desc = "create closure" },
    .{ .name = "call_closure", .value = 0xF9, .operands = &.{ r_hi, r_lo, i8_ }, .desc = "call closure" },

    // Trait Object Operations (0xFA-0xFB)
    .{ .name = "make_trait_object", .value = 0xFA, .operands = &.{ r_hi, r_lo, x16 }, .desc = "create trait object" },
    .{ .name = "call_trait_method", .value = 0xFB, .operands = &.{ r_hi, r_lo, i8_, i8_ }, .desc = "call trait method" },

    // Map key at (0xFC)
    .{ .name = "map_key_at", .value = 0xFC, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = map.key_at(idx)" },

    // List operations (0xFD, 0xFF)
    .{ .name = "list_get", .value = 0xFD, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "rd = list[idx]" },
    .{ .name = "extended", .value = 0xFE, .operands = &.{i8_}, .desc = "extended opcode prefix" },
    .{ .name = "list_set", .value = 0xFF, .operands = &.{ r_hi, r_lo, r_hi, p4 }, .desc = "list[idx] = val" },
};

// =============================================================================
// Comptime Verification
// =============================================================================

comptime {
    @setEvalBranchQuota(100000);

    // Verify all operand formats are byte-aligned
    for (opcodes) |spec| {
        var total_bits: u16 = 0;
        for (spec.operands) |op| {
            total_bits += op.bits();
        }
        if (total_bits % 8 != 0) {
            @compileError("Opcode '" ++ spec.name ++ "' has non-byte-aligned operands: " ++
                std.fmt.comptimePrint("{}", .{total_bits}) ++ " bits");
        }
    }

    // Verify no duplicate opcode values
    for (opcodes, 0..) |spec1, i| {
        for (opcodes[i + 1 ..]) |spec2| {
            if (spec1.value == spec2.value) {
                @compileError("Duplicate opcode value 0x" ++
                    std.fmt.comptimePrint("{x}", .{spec1.value}) ++
                    " for '" ++ spec1.name ++ "' and '" ++ spec2.name ++ "'");
            }
        }
    }
}

// =============================================================================
// Comptime-Derived Functions
// =============================================================================

/// Get operand byte count for an opcode value
/// This matches the original Opcode.operandSize() function
pub fn operandSize(opcode: u8) u8 {
    for (opcodes) |spec| {
        if (spec.value == opcode) {
            return spec.operandBytes();
        }
    }
    return 0; // Unknown opcode
}

/// Get the spec for an opcode by value
pub fn getSpecByValue(opcode: u8) ?OpcodeSpec {
    for (opcodes) |spec| {
        if (spec.value == opcode) {
            return spec;
        }
    }
    return null;
}

/// Get the spec for an opcode by name
pub fn getSpecByName(name: []const u8) ?OpcodeSpec {
    for (opcodes) |spec| {
        if (std.mem.eql(u8, spec.name, name)) {
            return spec;
        }
    }
    return null;
}

// =============================================================================
// Tests
// =============================================================================

test "all opcodes byte-aligned" {
    for (opcodes) |spec| {
        const bytes = spec.operandBytes();
        _ = bytes; // Comptime verification already done, this is just for coverage
    }
}

test "operandSize matches spec" {
    // Test a few key opcodes
    try std.testing.expectEqual(@as(u8, 0), operandSize(0x00)); // nop
    try std.testing.expectEqual(@as(u8, 2), operandSize(0x10)); // mov
    try std.testing.expectEqual(@as(u8, 3), operandSize(0x12)); // movi16
    try std.testing.expectEqual(@as(u8, 5), operandSize(0x13)); // movi32
    try std.testing.expectEqual(@as(u8, 2), operandSize(0x77)); // push_arg
    try std.testing.expectEqual(@as(u8, 2), operandSize(0x79)); // pop_arg
    try std.testing.expectEqual(@as(u8, 2), operandSize(0x75)); // ret
}

test "no duplicate opcode values" {
    var seen: [256]bool = [_]bool{false} ** 256;
    for (opcodes) |spec| {
        try std.testing.expect(!seen[spec.value]);
        seen[spec.value] = true;
    }
}
