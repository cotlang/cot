//! WebAssembly constants — instructions, registers, opcodes, and section IDs.

const std = @import("std");

pub const ValType = enum(u8) {
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
    funcref = 0x70,
};

pub const WasmType = struct {
    val: ValType = .i64,
    gc_ref: ?u32 = null,

    pub fn fromVal(v: ValType) WasmType {
        return .{ .val = v };
    }

    pub fn gcRefNull(type_idx: u32) WasmType {
        return .{ .gc_ref = type_idx };
    }

    pub fn eql(a: WasmType, b: WasmType) bool {
        if (a.gc_ref) |ar| {
            if (b.gc_ref) |br| return ar == br;
            return false;
        }
        if (b.gc_ref != null) return false;
        return a.val == b.val;
    }
};

pub const Reg = enum(i16) {
    sp = 0,
    ctxt = 1,
    g = 2,
    ret0 = 3,
    ret1 = 4,
    ret2 = 5,
    ret3 = 6,
    pause = 7,

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

    pc_b = 64,

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

pub const As = enum(u16) {
    get,
    set,
    tee,
    not,

    @"unreachable",
    nop,
    block,
    loop,
    @"if",
    @"else",
    end,
    br,
    br_if,
    br_table,
    @"return",
    call,
    call_indirect,
    return_call,

    drop,
    select,

    local_get,
    local_set,
    local_tee,
    global_get,
    global_set,

    i32_load,
    i64_load,
    f32_load,
    f64_load,
    i32_load8_s,
    i32_load8_u,
    i32_load16_s,
    i32_load16_u,
    i64_load8_s,
    i64_load8_u,
    i64_load16_s,
    i64_load16_u,
    i64_load32_s,
    i64_load32_u,

    i32_store,
    i64_store,
    f32_store,
    f64_store,
    i32_store8,
    i32_store16,
    i64_store8,
    i64_store16,
    i64_store32,

    memory_size,
    memory_grow,

    i32_const,
    i64_const,
    f32_const,
    f64_const,

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

    f32_eq,
    f32_ne,
    f32_lt,
    f32_gt,
    f32_le,
    f32_ge,

    f64_eq,
    f64_ne,
    f64_lt,
    f64_gt,
    f64_le,
    f64_ge,

    i32_clz,
    i32_ctz,
    i32_popcnt,
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

    i64_clz,
    i64_ctz,
    i64_popcnt,
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

    f32_abs,
    f32_neg,
    f32_ceil,
    f32_floor,
    f32_trunc,
    f32_nearest,
    f32_sqrt,
    f32_add,
    f32_sub,
    f32_mul,
    f32_div,
    f32_min,
    f32_max,
    f32_copysign,

    f64_abs,
    f64_neg,
    f64_ceil,
    f64_floor,
    f64_trunc,
    f64_nearest,
    f64_sqrt,
    f64_add,
    f64_sub,
    f64_mul,
    f64_div,
    f64_min,
    f64_max,
    f64_copysign,

    i32_wrap_i64,
    i32_trunc_f32_s,
    i32_trunc_f32_u,
    i32_trunc_f64_s,
    i32_trunc_f64_u,
    i64_extend_i32_s,
    i64_extend_i32_u,
    i64_trunc_f32_s,
    i64_trunc_f32_u,
    i64_trunc_f64_s,
    i64_trunc_f64_u,
    f32_convert_i32_s,
    f32_convert_i32_u,
    f32_convert_i64_s,
    f32_convert_i64_u,
    f32_demote_f64,
    f64_convert_i32_s,
    f64_convert_i32_u,
    f64_convert_i64_s,
    f64_convert_i64_u,
    f64_promote_f32,
    i32_reinterpret_f32,
    i64_reinterpret_f64,
    f32_reinterpret_i32,
    f64_reinterpret_i64,

    i32_extend8_s,
    i32_extend16_s,
    i64_extend8_s,
    i64_extend16_s,
    i64_extend32_s,

    i32_trunc_sat_f32_s,
    i32_trunc_sat_f32_u,
    i32_trunc_sat_f64_s,
    i32_trunc_sat_f64_u,
    i64_trunc_sat_f32_s,
    i64_trunc_sat_f32_u,
    i64_trunc_sat_f64_s,
    i64_trunc_sat_f64_u,

    memory_init,
    data_drop,
    memory_copy,
    memory_fill,
    table_init,
    elem_drop,
    table_copy,
    table_grow,
    table_size,
    table_fill,

    jmp,
    aret,
    resume_point,
    call_no_resume,
    ret_unwind,

    mov_b,
    mov_h,
    mov_w,
    mov_d,

    gc_struct_new,
    gc_struct_get,
    gc_struct_set,

    gc_array_new,
    gc_array_new_default,
    gc_array_new_fixed,
    gc_array_get,
    gc_array_set,
    gc_array_len,
    gc_array_new_data,
    gc_array_copy,

    ref_test,
    ref_cast,
    br_on_cast,
    br_on_cast_fail,
    ref_null,
    ref_is_null,
    ref_func,
    ref_eq,
    ref_as_not_null,
    br_on_null,
    br_on_non_null,
    call_ref,
    return_call_ref,

    word,
    text,
    func_data,
    pc_data,
    undef,

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
            .return_call => 0x12,
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
            .ref_is_null => 0xD1,
            .ref_func => 0xD2,
            .ref_eq => 0xD3,
            .ref_as_not_null => 0xD4,
            .call_ref => 0x14,
            .return_call_ref => 0x15,
            else => null,
        };
    }

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

    pub fn isGcPrefixed(self: As) bool {
        return switch (self) {
            .gc_struct_new, .gc_struct_get, .gc_struct_set,
            .gc_array_new, .gc_array_new_default, .gc_array_new_fixed, .gc_array_new_data,
            .gc_array_get, .gc_array_set, .gc_array_len, .gc_array_copy,
            .ref_test, .ref_cast, .br_on_cast, .br_on_cast_fail,
            => true,
            else => false,
        };
    }

    pub fn gcOpcode(self: As) ?u8 {
        return switch (self) {
            .gc_struct_new => GC_STRUCT_NEW,
            .gc_struct_get => GC_STRUCT_GET,
            .gc_struct_set => GC_STRUCT_SET,
            .gc_array_new => GC_ARRAY_NEW,
            .gc_array_new_default => GC_ARRAY_NEW_DEFAULT,
            .gc_array_new_fixed => GC_ARRAY_NEW_FIXED,
            .gc_array_new_data => GC_ARRAY_NEW_DATA,
            .gc_array_get => GC_ARRAY_GET,
            .gc_array_set => GC_ARRAY_SET,
            .gc_array_len => GC_ARRAY_LEN,
            .gc_array_copy => GC_ARRAY_COPY,
            .ref_test => GC_REF_TEST,
            .ref_cast => GC_REF_CAST,
            .br_on_cast => GC_BR_ON_CAST,
            .br_on_cast_fail => GC_BR_ON_CAST_FAIL,
            else => null,
        };
    }

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

pub const ExportKind = enum(u8) {
    func = 0,
    table = 1,
    memory = 2,
    global = 3,
};

pub const WASM_MAGIC: [4]u8 = .{ 0x00, 0x61, 0x73, 0x6D };
pub const WASM_VERSION: [4]u8 = .{ 0x01, 0x00, 0x00, 0x00 };
pub const FUNC_TYPE_TAG: u8 = 0x60;

pub const FUNC_VALUE_OFFSET: u32 = 0x1000;

pub const BLOCK_VOID: u8 = 0x40;

pub const LIMITS_NO_MAX: u8 = 0x00;
pub const LIMITS_WITH_MAX: u8 = 0x01;

pub const GLOBAL_IMMUTABLE: u8 = 0x00;
pub const GLOBAL_MUTABLE: u8 = 0x01;

pub const IMPORT_KIND_FUNC: u8 = 0x00;

pub const FC_PREFIX: u8 = 0xFC;

pub const MEMORY_IDX_ZERO: u8 = 0x00;

pub const TABLE_IDX_ZERO: u8 = 0x00;

pub const WASM_PAGE_SIZE: u32 = 65536;

pub const STACK_SIZE: u32 = 128 * WASM_PAGE_SIZE;

pub const GC_PREFIX: u8 = 0xFB;

pub const GC_STRUCT_NEW: u8 = 0x00;
pub const GC_STRUCT_NEW_DEFAULT: u8 = 0x01;
pub const GC_STRUCT_GET: u8 = 0x02;
pub const GC_STRUCT_GET_S: u8 = 0x03;
pub const GC_STRUCT_GET_U: u8 = 0x04;
pub const GC_STRUCT_SET: u8 = 0x05;

pub const GC_ARRAY_NEW: u8 = 0x06;
pub const GC_ARRAY_NEW_DEFAULT: u8 = 0x07;
pub const GC_ARRAY_NEW_FIXED: u8 = 0x08;
pub const GC_ARRAY_NEW_DATA: u8 = 0x09;
pub const GC_ARRAY_GET: u8 = 0x0B;
pub const GC_ARRAY_GET_S: u8 = 0x0C;
pub const GC_ARRAY_GET_U: u8 = 0x0D;
pub const GC_ARRAY_SET: u8 = 0x0E;
pub const GC_ARRAY_LEN: u8 = 0x0F;
pub const GC_ARRAY_COPY: u8 = 0x11;

pub const GC_REF_TEST: u8 = 0x14;
pub const GC_REF_TEST_NULL: u8 = 0x15;
pub const GC_REF_CAST: u8 = 0x16;
pub const GC_REF_CAST_NULL: u8 = 0x17;
pub const GC_BR_ON_CAST: u8 = 0x18;
pub const GC_BR_ON_CAST_FAIL: u8 = 0x19;

pub const GC_REF_FUNC: u8 = 0xD2;
pub const GC_CALL_REF: u8 = 0x14;
pub const GC_RETURN_CALL_REF: u8 = 0x15;

pub const REF_NULL: u8 = 0xD0;
pub const REF_IS_NULL: u8 = 0xD1;
pub const REF_FUNC: u8 = 0xD2;
pub const REF_EQ: u8 = 0xD3;
pub const REF_AS_NOT_NULL: u8 = 0xD4;
pub const BR_ON_NULL: u8 = 0xD5;
pub const BR_ON_NON_NULL: u8 = 0xD6;

pub const CALL_REF: u8 = 0x14;
pub const RETURN_CALL_REF: u8 = 0x15;

pub const GC_STRUCT_TYPE: u8 = 0x5F;
pub const GC_ARRAY_TYPE: u8 = 0x5E;
pub const GC_FIELD_MUT: u8 = 0x01;
pub const GC_FIELD_IMMUT: u8 = 0x00;
pub const GC_REF_TYPE: u8 = 0x64;
pub const GC_REF_TYPE_NULL: u8 = 0x63;
pub const GC_REC_TYPE: u8 = 0x4E;
pub const GC_SUB_TYPE: u8 = 0x50;
pub const GC_SUB_TYPE_FINAL: u8 = 0x4F;

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
