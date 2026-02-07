//! WebAssembly constants - Instructions and Registers
//!
//! Go reference: cmd/internal/obj/wasm/a.out.go
//!
//! This file defines:
//! - Wasm instruction opcodes (matching Go's A* constants)
//! - Virtual register numbers (matching Go's REG_* constants)
//! - Value types

const std = @import("std");

// ============================================================================
// Value Types (Wasm spec)
// ============================================================================

pub const ValType = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
    funcref = 0x70,
};

// ============================================================================
// Registers (Go reference: a.out.go lines 263-341)
// ============================================================================
//
// Go's Wasm port uses these virtual registers:
// - Globals 0-7: SP, CTXT, g, RET0-3, PAUSE
// - Locals: PC_B (param), SP cache, R0-R15, F0-F31
//
// We simplify for Cot (no goroutines yet):
// - Global 0: SP (stack pointer into linear memory)
// - Locals: function parameters, then computed locals

pub const Reg = enum(i16) {
    // Globals (Go: lines 268-277)
    sp = 0, // Stack pointer (i32) - global 0
    ctxt = 1, // Closure context (i64) - global 1 (for future)
    g = 2, // Goroutine pointer (i64) - global 2 (for future)
    ret0 = 3, // Return value 0 (i64) - global 3
    ret1 = 4, // Return value 1 (i64) - global 4
    ret2 = 5, // Return value 2 (i64) - global 5
    ret3 = 6, // Return value 3 (i64) - global 6
    pause = 7, // Pause flag (i32) - global 7 (for future)

    // i64 locals (Go: lines 279-295)
    r0 = 16,
    r1 = 17,
    r2 = 18,
    r3 = 19,
    r4 = 20,
    r5 = 21,
    r6 = 22,
    r7 = 23,
    r8 = 24,
    r9 = 25,
    r10 = 26,
    r11 = 27,
    r12 = 28,
    r13 = 29,
    r14 = 30,
    r15 = 31,

    // f32 locals (Go: lines 297-313)
    f0 = 32,
    f1 = 33,
    f2 = 34,
    f3 = 35,
    f4 = 36,
    f5 = 37,
    f6 = 38,
    f7 = 39,
    f8 = 40,
    f9 = 41,
    f10 = 42,
    f11 = 43,
    f12 = 44,
    f13 = 45,
    f14 = 46,
    f15 = 47,

    // f64 locals (Go: lines 315-331)
    f16 = 48,
    f17 = 49,
    f18 = 50,
    f19 = 51,
    f20 = 52,
    f21 = 53,
    f22 = 54,
    f23 = 55,
    f24 = 56,
    f25 = 57,
    f26 = 58,
    f27 = 59,
    f28 = 60,
    f29 = 61,
    f30 = 62,
    f31 = 63,

    // Special (Go: line 333)
    pc_b = 64, // Block resume index (function parameter)

    none = -1,

    pub fn isGlobal(self: Reg) bool {
        return @intFromEnum(self) >= 0 and @intFromEnum(self) <= 7;
    }

    pub fn isI64(self: Reg) bool {
        const v = @intFromEnum(self);
        return v >= 16 and v <= 31;
    }

    pub fn isF32(self: Reg) bool {
        const v = @intFromEnum(self);
        return v >= 32 and v <= 47;
    }

    pub fn isF64(self: Reg) bool {
        const v = @intFromEnum(self);
        return v >= 48 and v <= 63;
    }

    pub fn valType(self: Reg) ValType {
        if (self == .sp or self == .pause) return .i32;
        if (self.isF32()) return .f32;
        if (self.isF64()) return .f64;
        return .i64;
    }
};

pub const REG_SP = Reg.sp;
pub const REG_CTXT = Reg.ctxt;
pub const REG_R0 = Reg.r0;
pub const REG_F0 = Reg.f0;
pub const REG_F16 = Reg.f16;
pub const REG_PC_B = Reg.pc_b;

// ============================================================================
// Instructions (Go reference: a.out.go lines 20-260)
// ============================================================================
//
// Go uses iota-based constants that map to Wasm opcodes.
// We define both the assembler instruction (As) and the raw opcode.

/// Assembler instruction type (like Go's obj.As)
pub const As = enum(u16) {
    // High-level pseudo-instructions (Go: lines 21-24)
    get, // Get register value onto stack
    set, // Set register from stack
    tee, // Tee register (set and keep on stack)
    not, // Alias for I32Eqz

    // Control flow (Go: lines 30-45, opcodes 0x00-0x11)
    @"unreachable", // 0x00
    nop, // 0x01
    block, // 0x02
    loop, // 0x03
    @"if", // 0x04
    @"else", // 0x05
    end, // 0x0B
    br, // 0x0C
    br_if, // 0x0D
    br_table, // 0x0E
    @"return", // 0x0F
    call, // 0x10
    call_indirect, // 0x11

    // Parametric (Go: lines 47-48, opcodes 0x1A-0x1B)
    drop, // 0x1A
    select, // 0x1B

    // Variable (Go: lines 50-54, opcodes 0x20-0x24)
    local_get, // 0x20
    local_set, // 0x21
    local_tee, // 0x22
    global_get, // 0x23
    global_set, // 0x24

    // Memory load (Go: lines 56-69, opcodes 0x28-0x35)
    i32_load, // 0x28
    i64_load, // 0x29
    f32_load, // 0x2A
    f64_load, // 0x2B
    i32_load8_s, // 0x2C
    i32_load8_u, // 0x2D
    i32_load16_s, // 0x2E
    i32_load16_u, // 0x2F
    i64_load8_s, // 0x30
    i64_load8_u, // 0x31
    i64_load16_s, // 0x32
    i64_load16_u, // 0x33
    i64_load32_s, // 0x34
    i64_load32_u, // 0x35

    // Memory store (Go: lines 70-78, opcodes 0x36-0x3E)
    i32_store, // 0x36
    i64_store, // 0x37
    f32_store, // 0x38
    f64_store, // 0x39
    i32_store8, // 0x3A
    i32_store16, // 0x3B
    i64_store8, // 0x3C
    i64_store16, // 0x3D
    i64_store32, // 0x3E

    // Memory (Go: lines 79-80, opcodes 0x3F-0x40)
    memory_size, // 0x3F
    memory_grow, // 0x40

    // Constants (Go: lines 82-85, opcodes 0x41-0x44)
    i32_const, // 0x41
    i64_const, // 0x42
    f32_const, // 0x43
    f64_const, // 0x44

    // i32 comparison (Go: lines 87-97, opcodes 0x45-0x4F)
    i32_eqz, // 0x45
    i32_eq, // 0x46
    i32_ne, // 0x47
    i32_lt_s, // 0x48
    i32_lt_u, // 0x49
    i32_gt_s, // 0x4A
    i32_gt_u, // 0x4B
    i32_le_s, // 0x4C
    i32_le_u, // 0x4D
    i32_ge_s, // 0x4E
    i32_ge_u, // 0x4F

    // i64 comparison (Go: lines 99-109, opcodes 0x50-0x5A)
    i64_eqz, // 0x50
    i64_eq, // 0x51
    i64_ne, // 0x52
    i64_lt_s, // 0x53
    i64_lt_u, // 0x54
    i64_gt_s, // 0x55
    i64_gt_u, // 0x56
    i64_le_s, // 0x57
    i64_le_u, // 0x58
    i64_ge_s, // 0x59
    i64_ge_u, // 0x5A

    // f32 comparison (Go: lines 111-116, opcodes 0x5B-0x60)
    f32_eq, // 0x5B
    f32_ne, // 0x5C
    f32_lt, // 0x5D
    f32_gt, // 0x5E
    f32_le, // 0x5F
    f32_ge, // 0x60

    // f64 comparison (Go: lines 118-123, opcodes 0x61-0x66)
    f64_eq, // 0x61
    f64_ne, // 0x62
    f64_lt, // 0x63
    f64_gt, // 0x64
    f64_le, // 0x65
    f64_ge, // 0x66

    // i32 arithmetic (Go: lines 125-142, opcodes 0x67-0x78)
    i32_clz, // 0x67
    i32_ctz, // 0x68
    i32_popcnt, // 0x69
    i32_add, // 0x6A
    i32_sub, // 0x6B
    i32_mul, // 0x6C
    i32_div_s, // 0x6D
    i32_div_u, // 0x6E
    i32_rem_s, // 0x6F
    i32_rem_u, // 0x70
    i32_and, // 0x71
    i32_or, // 0x72
    i32_xor, // 0x73
    i32_shl, // 0x74
    i32_shr_s, // 0x75
    i32_shr_u, // 0x76
    i32_rotl, // 0x77
    i32_rotr, // 0x78

    // i64 arithmetic (Go: lines 144-161, opcodes 0x79-0x8A)
    i64_clz, // 0x79
    i64_ctz, // 0x7A
    i64_popcnt, // 0x7B
    i64_add, // 0x7C
    i64_sub, // 0x7D
    i64_mul, // 0x7E
    i64_div_s, // 0x7F
    i64_div_u, // 0x80
    i64_rem_s, // 0x81
    i64_rem_u, // 0x82
    i64_and, // 0x83
    i64_or, // 0x84
    i64_xor, // 0x85
    i64_shl, // 0x86
    i64_shr_s, // 0x87
    i64_shr_u, // 0x88
    i64_rotl, // 0x89
    i64_rotr, // 0x8A

    // f32 arithmetic (Go: lines 163-176, opcodes 0x8B-0x98)
    f32_abs, // 0x8B
    f32_neg, // 0x8C
    f32_ceil, // 0x8D
    f32_floor, // 0x8E
    f32_trunc, // 0x8F
    f32_nearest, // 0x90
    f32_sqrt, // 0x91
    f32_add, // 0x92
    f32_sub, // 0x93
    f32_mul, // 0x94
    f32_div, // 0x95
    f32_min, // 0x96
    f32_max, // 0x97
    f32_copysign, // 0x98

    // f64 arithmetic (Go: lines 178-191, opcodes 0x99-0xA6)
    f64_abs, // 0x99
    f64_neg, // 0x9A
    f64_ceil, // 0x9B
    f64_floor, // 0x9C
    f64_trunc, // 0x9D
    f64_nearest, // 0x9E
    f64_sqrt, // 0x9F
    f64_add, // 0xA0
    f64_sub, // 0xA1
    f64_mul, // 0xA2
    f64_div, // 0xA3
    f64_min, // 0xA4
    f64_max, // 0xA5
    f64_copysign, // 0xA6

    // Conversions (Go: lines 193-222, opcodes 0xA7-0xC4)
    i32_wrap_i64, // 0xA7
    i32_trunc_f32_s, // 0xA8
    i32_trunc_f32_u, // 0xA9
    i32_trunc_f64_s, // 0xAA
    i32_trunc_f64_u, // 0xAB
    i64_extend_i32_s, // 0xAC
    i64_extend_i32_u, // 0xAD
    i64_trunc_f32_s, // 0xAE
    i64_trunc_f32_u, // 0xAF
    i64_trunc_f64_s, // 0xB0
    i64_trunc_f64_u, // 0xB1
    f32_convert_i32_s, // 0xB2
    f32_convert_i32_u, // 0xB3
    f32_convert_i64_s, // 0xB4
    f32_convert_i64_u, // 0xB5
    f32_demote_f64, // 0xB6
    f64_convert_i32_s, // 0xB7
    f64_convert_i32_u, // 0xB8
    f64_convert_i64_s, // 0xB9
    f64_convert_i64_u, // 0xBA
    f64_promote_f32, // 0xBB
    i32_reinterpret_f32, // 0xBC
    i64_reinterpret_f64, // 0xBD
    f32_reinterpret_i32, // 0xBE
    f64_reinterpret_i64, // 0xBF

    // Sign extension (Go: lines 218-222, opcodes 0xC0-0xC4)
    i32_extend8_s, // 0xC0
    i32_extend16_s, // 0xC1
    i64_extend8_s, // 0xC2
    i64_extend16_s, // 0xC3
    i64_extend32_s, // 0xC4

    // Saturating truncation (Go: lines 224-231, opcodes 0xFC 0x00-0x07)
    i32_trunc_sat_f32_s, // 0xFC 0x00
    i32_trunc_sat_f32_u, // 0xFC 0x01
    i32_trunc_sat_f64_s, // 0xFC 0x02
    i32_trunc_sat_f64_u, // 0xFC 0x03
    i64_trunc_sat_f32_s, // 0xFC 0x04
    i64_trunc_sat_f32_u, // 0xFC 0x05
    i64_trunc_sat_f64_s, // 0xFC 0x06
    i64_trunc_sat_f64_u, // 0xFC 0x07

    // Bulk memory (Go: lines 233-242, opcodes 0xFC 0x08-0x11)
    memory_init, // 0xFC 0x08
    data_drop, // 0xFC 0x09
    memory_copy, // 0xFC 0x0A
    memory_fill, // 0xFC 0x0B
    table_init, // 0xFC 0x0C
    elem_drop, // 0xFC 0x0D
    table_copy, // 0xFC 0x0E
    table_grow, // 0xFC 0x0F
    table_size, // 0xFC 0x10
    table_fill, // 0xFC 0x11

    // Pseudo-instructions (Go: lines 246-260)
    jmp, // Pseudo-jump (Go: obj.AJMP) - transformed by preprocess
    aret, // Pseudo-return (Go: obj.ARET) - transformed by preprocess
    resume_point, // Mark a resume point
    call_no_resume, // Call without resume point
    ret_unwind, // Return and unwind stack

    // Move pseudo-instructions (Go: lines 254-257)
    mov_b, // Move byte
    mov_h, // Move halfword
    mov_w, // Move word
    mov_d, // Move doubleword

    // Misc
    word, // Raw data
    text, // Function start
    func_data, // Function metadata
    pc_data, // PC metadata
    undef, // Undefined (trap)

    /// Convert to Wasm opcode byte(s)
    pub fn opcode(self: As) ?u8 {
        return switch (self) {
            .@"unreachable" => 0x00,
            .nop => 0x01,
            .block => 0x02,
            .loop => 0x03,
            .@"if" => 0x04,
            .@"else" => 0x05,
            .end => 0x0B,
            .br => 0x0C,
            .br_if => 0x0D,
            .br_table => 0x0E,
            .@"return" => 0x0F,
            .call => 0x10,
            .call_indirect => 0x11,
            .drop => 0x1A,
            .select => 0x1B,
            .local_get => 0x20,
            .local_set => 0x21,
            .local_tee => 0x22,
            .global_get => 0x23,
            .global_set => 0x24,
            .i32_load => 0x28,
            .i64_load => 0x29,
            .f32_load => 0x2A,
            .f64_load => 0x2B,
            .i32_load8_s => 0x2C,
            .i32_load8_u => 0x2D,
            .i32_load16_s => 0x2E,
            .i32_load16_u => 0x2F,
            .i64_load8_s => 0x30,
            .i64_load8_u => 0x31,
            .i64_load16_s => 0x32,
            .i64_load16_u => 0x33,
            .i64_load32_s => 0x34,
            .i64_load32_u => 0x35,
            .i32_store => 0x36,
            .i64_store => 0x37,
            .f32_store => 0x38,
            .f64_store => 0x39,
            .i32_store8 => 0x3A,
            .i32_store16 => 0x3B,
            .i64_store8 => 0x3C,
            .i64_store16 => 0x3D,
            .i64_store32 => 0x3E,
            .memory_size => 0x3F,
            .memory_grow => 0x40,
            .i32_const => 0x41,
            .i64_const => 0x42,
            .f32_const => 0x43,
            .f64_const => 0x44,
            .i32_eqz => 0x45,
            .i32_eq => 0x46,
            .i32_ne => 0x47,
            .i32_lt_s => 0x48,
            .i32_lt_u => 0x49,
            .i32_gt_s => 0x4A,
            .i32_gt_u => 0x4B,
            .i32_le_s => 0x4C,
            .i32_le_u => 0x4D,
            .i32_ge_s => 0x4E,
            .i32_ge_u => 0x4F,
            .i64_eqz => 0x50,
            .i64_eq => 0x51,
            .i64_ne => 0x52,
            .i64_lt_s => 0x53,
            .i64_lt_u => 0x54,
            .i64_gt_s => 0x55,
            .i64_gt_u => 0x56,
            .i64_le_s => 0x57,
            .i64_le_u => 0x58,
            .i64_ge_s => 0x59,
            .i64_ge_u => 0x5A,
            .f32_eq => 0x5B,
            .f32_ne => 0x5C,
            .f32_lt => 0x5D,
            .f32_gt => 0x5E,
            .f32_le => 0x5F,
            .f32_ge => 0x60,
            .f64_eq => 0x61,
            .f64_ne => 0x62,
            .f64_lt => 0x63,
            .f64_gt => 0x64,
            .f64_le => 0x65,
            .f64_ge => 0x66,
            .i32_clz => 0x67,
            .i32_ctz => 0x68,
            .i32_popcnt => 0x69,
            .i32_add => 0x6A,
            .i32_sub => 0x6B,
            .i32_mul => 0x6C,
            .i32_div_s => 0x6D,
            .i32_div_u => 0x6E,
            .i32_rem_s => 0x6F,
            .i32_rem_u => 0x70,
            .i32_and => 0x71,
            .i32_or => 0x72,
            .i32_xor => 0x73,
            .i32_shl => 0x74,
            .i32_shr_s => 0x75,
            .i32_shr_u => 0x76,
            .i32_rotl => 0x77,
            .i32_rotr => 0x78,
            .i64_clz => 0x79,
            .i64_ctz => 0x7A,
            .i64_popcnt => 0x7B,
            .i64_add => 0x7C,
            .i64_sub => 0x7D,
            .i64_mul => 0x7E,
            .i64_div_s => 0x7F,
            .i64_div_u => 0x80,
            .i64_rem_s => 0x81,
            .i64_rem_u => 0x82,
            .i64_and => 0x83,
            .i64_or => 0x84,
            .i64_xor => 0x85,
            .i64_shl => 0x86,
            .i64_shr_s => 0x87,
            .i64_shr_u => 0x88,
            .i64_rotl => 0x89,
            .i64_rotr => 0x8A,
            .f32_abs => 0x8B,
            .f32_neg => 0x8C,
            .f32_ceil => 0x8D,
            .f32_floor => 0x8E,
            .f32_trunc => 0x8F,
            .f32_nearest => 0x90,
            .f32_sqrt => 0x91,
            .f32_add => 0x92,
            .f32_sub => 0x93,
            .f32_mul => 0x94,
            .f32_div => 0x95,
            .f32_min => 0x96,
            .f32_max => 0x97,
            .f32_copysign => 0x98,
            .f64_abs => 0x99,
            .f64_neg => 0x9A,
            .f64_ceil => 0x9B,
            .f64_floor => 0x9C,
            .f64_trunc => 0x9D,
            .f64_nearest => 0x9E,
            .f64_sqrt => 0x9F,
            .f64_add => 0xA0,
            .f64_sub => 0xA1,
            .f64_mul => 0xA2,
            .f64_div => 0xA3,
            .f64_min => 0xA4,
            .f64_max => 0xA5,
            .f64_copysign => 0xA6,
            .i32_wrap_i64 => 0xA7,
            .i32_trunc_f32_s => 0xA8,
            .i32_trunc_f32_u => 0xA9,
            .i32_trunc_f64_s => 0xAA,
            .i32_trunc_f64_u => 0xAB,
            .i64_extend_i32_s => 0xAC,
            .i64_extend_i32_u => 0xAD,
            .i64_trunc_f32_s => 0xAE,
            .i64_trunc_f32_u => 0xAF,
            .i64_trunc_f64_s => 0xB0,
            .i64_trunc_f64_u => 0xB1,
            .f32_convert_i32_s => 0xB2,
            .f32_convert_i32_u => 0xB3,
            .f32_convert_i64_s => 0xB4,
            .f32_convert_i64_u => 0xB5,
            .f32_demote_f64 => 0xB6,
            .f64_convert_i32_s => 0xB7,
            .f64_convert_i32_u => 0xB8,
            .f64_convert_i64_s => 0xB9,
            .f64_convert_i64_u => 0xBA,
            .f64_promote_f32 => 0xBB,
            .i32_reinterpret_f32 => 0xBC,
            .i64_reinterpret_f64 => 0xBD,
            .f32_reinterpret_i32 => 0xBE,
            .f64_reinterpret_i64 => 0xBF,
            .i32_extend8_s => 0xC0,
            .i32_extend16_s => 0xC1,
            .i64_extend8_s => 0xC2,
            .i64_extend16_s => 0xC3,
            .i64_extend32_s => 0xC4,
            else => null, // Pseudo-instructions or FC-prefixed
        };
    }

    /// Returns true if this is an FC-prefixed instruction
    pub fn isFcPrefixed(self: As) bool {
        return switch (self) {
            .i32_trunc_sat_f32_s,
            .i32_trunc_sat_f32_u,
            .i32_trunc_sat_f64_s,
            .i32_trunc_sat_f64_u,
            .i64_trunc_sat_f32_s,
            .i64_trunc_sat_f32_u,
            .i64_trunc_sat_f64_s,
            .i64_trunc_sat_f64_u,
            .memory_init,
            .data_drop,
            .memory_copy,
            .memory_fill,
            .table_init,
            .elem_drop,
            .table_copy,
            .table_grow,
            .table_size,
            .table_fill,
            => true,
            else => false,
        };
    }

    /// Get the FC-prefixed opcode byte
    pub fn fcOpcode(self: As) ?u8 {
        return switch (self) {
            .i32_trunc_sat_f32_s => 0x00,
            .i32_trunc_sat_f32_u => 0x01,
            .i32_trunc_sat_f64_s => 0x02,
            .i32_trunc_sat_f64_u => 0x03,
            .i64_trunc_sat_f32_s => 0x04,
            .i64_trunc_sat_f32_u => 0x05,
            .i64_trunc_sat_f64_s => 0x06,
            .i64_trunc_sat_f64_u => 0x07,
            .memory_init => 0x08,
            .data_drop => 0x09,
            .memory_copy => 0x0A,
            .memory_fill => 0x0B,
            .table_init => 0x0C,
            .elem_drop => 0x0D,
            .table_copy => 0x0E,
            .table_grow => 0x0F,
            .table_size => 0x10,
            .table_fill => 0x11,
            else => null,
        };
    }
};

// ============================================================================
// Section IDs (Wasm spec)
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
// Export kinds
// ============================================================================

pub const ExportKind = enum(u8) {
    func = 0,
    table = 1,
    memory = 2,
    global = 3,
};

// ============================================================================
// Constants
// ============================================================================

pub const WASM_MAGIC: [4]u8 = .{ 0x00, 0x61, 0x73, 0x6D }; // \0asm
pub const WASM_VERSION: [4]u8 = .{ 0x01, 0x00, 0x00, 0x00 }; // version 1
pub const FUNC_TYPE_TAG: u8 = 0x60;

// Go's funcValueOffset (asm.go line 45)
// Offset between PC_F value and WebAssembly function index
pub const FUNC_VALUE_OFFSET: u32 = 0x1000;

/// Block type for void blocks (no result)
pub const BLOCK_VOID: u8 = 0x40;

/// Limits encoding (Wasm spec §5.3.5)
pub const LIMITS_NO_MAX: u8 = 0x00;
pub const LIMITS_WITH_MAX: u8 = 0x01;

/// Global mutability (Wasm spec §5.3.6)
pub const GLOBAL_IMMUTABLE: u8 = 0x00;
pub const GLOBAL_MUTABLE: u8 = 0x01;

/// Import descriptor kinds (Wasm spec §5.4.1)
pub const IMPORT_KIND_FUNC: u8 = 0x00;

/// FC-prefixed opcode prefix (bulk memory, saturating truncation)
pub const FC_PREFIX: u8 = 0xFC;

/// Reserved memory index (always 0 for single-memory modules)
pub const MEMORY_IDX_ZERO: u8 = 0x00;

/// Reserved table index (always 0 for single-table modules)
pub const TABLE_IDX_ZERO: u8 = 0x00;

/// Wasm linear memory page size
pub const WASM_PAGE_SIZE: u32 = 65536;

/// Default stack size (64KB = 1 Wasm page)
pub const STACK_SIZE: u32 = WASM_PAGE_SIZE;

// ============================================================================
// Tests
// ============================================================================

test "opcode values match Wasm spec" {
    try std.testing.expectEqual(@as(u8, 0x00), As.@"unreachable".opcode().?);
    try std.testing.expectEqual(@as(u8, 0x0B), As.end.opcode().?);
    try std.testing.expectEqual(@as(u8, 0x10), As.call.opcode().?);
    try std.testing.expectEqual(@as(u8, 0x20), As.local_get.opcode().?);
    try std.testing.expectEqual(@as(u8, 0x41), As.i32_const.opcode().?);
    try std.testing.expectEqual(@as(u8, 0x42), As.i64_const.opcode().?);
    try std.testing.expectEqual(@as(u8, 0x7C), As.i64_add.opcode().?);
}

test "register types" {
    try std.testing.expectEqual(ValType.i32, Reg.sp.valType());
    try std.testing.expectEqual(ValType.i64, Reg.r0.valType());
    try std.testing.expectEqual(ValType.f32, Reg.f0.valType());
    try std.testing.expectEqual(ValType.f64, Reg.f16.valType());
}

test "global vs local registers" {
    try std.testing.expect(Reg.sp.isGlobal());
    try std.testing.expect(Reg.ctxt.isGlobal());
    try std.testing.expect(!Reg.r0.isGlobal());
    try std.testing.expect(!Reg.f0.isGlobal());
}
