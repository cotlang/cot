//! SSA Operation definitions - what computation each Value performs.

const std = @import("std");

/// Register mask - each bit represents a register.
pub const RegMask = u64;

/// SSA operation type.
pub const Op = enum(u16) {
    // === Invalid/Placeholder ===
    invalid,

    // === Memory State ===
    init_mem,

    // === Constants ===
    const_bool, const_int, const_float, const_nil, const_string, const_ptr,
    const_8, const_16, const_32, const_64,

    // === Integer Arithmetic (Generic) ===
    add, sub, mul, div, udiv, mod, umod, neg,

    // === Integer Arithmetic (Sized) ===
    add8, sub8, mul8, add16, sub16, mul16, add32, sub32, mul32, add64, sub64, mul64,
    hmul32, hmul32u, hmul64, hmul64u,
    divmod32, divmod64, divmodu32, divmodu64,

    // === Bitwise (Generic) ===
    and_, or_, xor, shl, shr, sar, not,

    // === Bitwise (Sized) ===
    and8, and16, and32, and64, or8, or16, or32, or64, xor8, xor16, xor32, xor64,
    shl8, shl16, shl32, shl64, shr8, shr16, shr32, shr64, sar8, sar16, sar32, sar64,
    com8, com16, com32, com64,
    ctz32, ctz64, clz32, clz64, popcnt32, popcnt64,

    // === Comparisons ===
    eq, ne, lt, le, gt, ge, ult, ule, ugt, uge,
    eq8, eq16, eq32, eq64, ne8, ne16, ne32, ne64,
    lt32, lt64, le32, le64, gt32, gt64, ge32, ge64,

    // === Type Conversions ===
    sign_ext8to16, sign_ext8to32, sign_ext8to64, sign_ext16to32, sign_ext16to64, sign_ext32to64,
    zero_ext8to16, zero_ext8to32, zero_ext8to64, zero_ext16to32, zero_ext16to64, zero_ext32to64,
    trunc16to8, trunc32to8, trunc32to16, trunc64to8, trunc64to16, trunc64to32,
    convert,
    cvt32to32f, cvt32to64f, cvt64to32f, cvt64to64f,
    cvt32fto32, cvt32fto64, cvt64fto32, cvt64fto64,
    cvt32fto64f, cvt64fto32f,

    // === Float Operations ===
    add32f, sub32f, mul32f, div32f, neg32f, sqrt32f,
    add64f, sub64f, mul64f, div64f, neg64f, sqrt64f,

    // === Memory Operations ===
    load, store,
    load8, load16, load32, load64, store8, store16, store32, store64,
    load8s, load16s, load32s,
    addr, local_addr, global_addr, off_ptr, add_ptr, sub_ptr,
    store_wb, move, zero,
    var_def, var_live, var_kill,
    sp, store_sp, // Stack pointer operations for Wasm->native AOT

    // === Control Flow ===
    phi, copy, fwd_ref, arg,
    select0, select1, make_tuple, select_n, cond_select,
    string_len, string_ptr, string_make, slice_len, slice_ptr, slice_make, string_concat,

    // === Function Calls ===
    call, tail_call, static_call, closure_call, inter_call,

    // === Safety Checks ===
    nil_check, is_non_nil, is_nil, bounds_check, slice_bounds,

    // === ARC (Reference Counting) ===
    retain, release,

    // === Atomics ===
    atomic_load32, atomic_load64, atomic_store32, atomic_store64,
    atomic_add32, atomic_add64, atomic_cas32, atomic_cas64, atomic_exchange32, atomic_exchange64,

    // === Register Allocation ===
    store_reg, load_reg,

    // === ARM64-Specific ===
    arm64_add, arm64_adds, arm64_sub, arm64_subs, arm64_mul, arm64_sdiv, arm64_udiv,
    arm64_madd, arm64_msub, arm64_smulh, arm64_umulh,
    arm64_and, arm64_orr, arm64_eor, arm64_bic, arm64_orn, arm64_eon, arm64_mvn,
    arm64_lsl, arm64_lsr, arm64_asr, arm64_ror, arm64_lslimm, arm64_lsrimm, arm64_asrimm,
    arm64_cmp, arm64_cmn, arm64_tst,
    arm64_movd, arm64_movw, arm64_movz, arm64_movn, arm64_movk,
    arm64_ldr, arm64_ldrw, arm64_ldrh, arm64_ldrb, arm64_ldrsw, arm64_ldrsh, arm64_ldrsb, arm64_ldp,
    arm64_str, arm64_strw, arm64_strh, arm64_strb, arm64_stp,
    arm64_adrp, arm64_add_imm, arm64_sub_imm,
    arm64_bl, arm64_blr, arm64_br, arm64_ret, arm64_b, arm64_bcond,
    arm64_csel, arm64_csinc, arm64_csinv, arm64_csneg, arm64_cset,
    arm64_clz, arm64_rbit, arm64_rev,
    arm64_sxtb, arm64_sxth, arm64_sxtw, arm64_uxtb, arm64_uxth,
    arm64_fadd, arm64_fsub, arm64_fmul, arm64_fdiv, arm64_fneg, arm64_fsqrt,
    arm64_fmov, arm64_fcvtzs, arm64_scvtf, arm64_fcmp,

    // === AMD64-Specific ===
    amd64_addq, amd64_addl, amd64_subq, amd64_subl, amd64_imulq, amd64_imull,
    amd64_idivq, amd64_idivl, amd64_divq, amd64_divl,
    amd64_andq, amd64_andl, amd64_orq, amd64_orl, amd64_xorq, amd64_xorl,
    amd64_shlq, amd64_shll, amd64_shrq, amd64_shrl, amd64_sarq, amd64_sarl,
    amd64_notq, amd64_notl, amd64_negq, amd64_negl,
    amd64_cmpq, amd64_cmpl, amd64_testq, amd64_testl,
    amd64_movq, amd64_movl, amd64_movb, amd64_movw,
    amd64_movabs, amd64_movzx, amd64_movsx,
    amd64_leaq, amd64_leal,
    amd64_loadq, amd64_loadl, amd64_loadw, amd64_loadb,
    amd64_storeq, amd64_storel, amd64_storew, amd64_storeb,
    amd64_call, amd64_ret, amd64_jmp, amd64_jcc,
    amd64_setcc, amd64_cmov,
    amd64_pushq, amd64_popq,
    amd64_addsd, amd64_subsd, amd64_mulsd, amd64_divsd,
    amd64_movsd, amd64_cvtsi2sd, amd64_cvttsd2si,

    // === Wasm-Specific ===
    // Constants
    wasm_i64_const, wasm_i32_const, wasm_f64_const, wasm_f32_const,

    // Integer arithmetic (i64)
    wasm_i64_add, wasm_i64_sub, wasm_i64_mul, wasm_i64_div_s, wasm_i64_div_u,
    wasm_i64_rem_s, wasm_i64_rem_u,

    // Integer arithmetic (i32)
    wasm_i32_add, wasm_i32_sub, wasm_i32_mul, wasm_i32_div_s, wasm_i32_div_u,
    wasm_i32_rem_s, wasm_i32_rem_u,

    // Integer bitwise (i64)
    wasm_i64_and, wasm_i64_or, wasm_i64_xor, wasm_i64_shl, wasm_i64_shr_s, wasm_i64_shr_u,
    wasm_i64_clz, wasm_i64_ctz, wasm_i64_popcnt, wasm_i64_rotl, wasm_i64_rotr,

    // Integer bitwise (i32)
    wasm_i32_and, wasm_i32_or, wasm_i32_xor, wasm_i32_shl, wasm_i32_shr_s, wasm_i32_shr_u,
    wasm_i32_clz, wasm_i32_ctz, wasm_i32_popcnt, wasm_i32_rotl, wasm_i32_rotr,

    // Integer comparison (i64)
    wasm_i64_eqz, wasm_i64_eq, wasm_i64_ne,
    wasm_i64_lt_s, wasm_i64_lt_u, wasm_i64_gt_s, wasm_i64_gt_u,
    wasm_i64_le_s, wasm_i64_le_u, wasm_i64_ge_s, wasm_i64_ge_u,

    // Integer comparison (i32)
    wasm_i32_eqz, wasm_i32_eq, wasm_i32_ne,
    wasm_i32_lt_s, wasm_i32_lt_u, wasm_i32_gt_s, wasm_i32_gt_u,
    wasm_i32_le_s, wasm_i32_le_u, wasm_i32_ge_s, wasm_i32_ge_u,

    // Float arithmetic (f64)
    wasm_f64_add, wasm_f64_sub, wasm_f64_mul, wasm_f64_div,
    wasm_f64_neg, wasm_f64_abs, wasm_f64_sqrt, wasm_f64_ceil, wasm_f64_floor, wasm_f64_trunc, wasm_f64_nearest,
    wasm_f64_min, wasm_f64_max, wasm_f64_copysign,

    // Float arithmetic (f32)
    wasm_f32_add, wasm_f32_sub, wasm_f32_mul, wasm_f32_div,
    wasm_f32_neg, wasm_f32_abs, wasm_f32_sqrt, wasm_f32_ceil, wasm_f32_floor, wasm_f32_trunc, wasm_f32_nearest,
    wasm_f32_min, wasm_f32_max, wasm_f32_copysign,

    // Float comparison (f64)
    wasm_f64_eq, wasm_f64_ne, wasm_f64_lt, wasm_f64_gt, wasm_f64_le, wasm_f64_ge,

    // Float comparison (f32)
    wasm_f32_eq, wasm_f32_ne, wasm_f32_lt, wasm_f32_gt, wasm_f32_le, wasm_f32_ge,

    // Conversions
    wasm_i64_trunc_f64_s, wasm_i64_trunc_f64_u, wasm_i64_trunc_f32_s, wasm_i64_trunc_f32_u,
    wasm_i32_trunc_f64_s, wasm_i32_trunc_f64_u, wasm_i32_trunc_f32_s, wasm_i32_trunc_f32_u,
    wasm_f64_convert_i64_s, wasm_f64_convert_i64_u, wasm_f64_convert_i32_s, wasm_f64_convert_i32_u,
    wasm_f32_convert_i64_s, wasm_f32_convert_i64_u, wasm_f32_convert_i32_s, wasm_f32_convert_i32_u,
    wasm_i64_extend_i32_s, wasm_i64_extend_i32_u,
    wasm_i32_wrap_i64,
    wasm_f64_promote_f32, wasm_f32_demote_f64,
    wasm_i64_reinterpret_f64, wasm_f64_reinterpret_i64,
    wasm_i32_reinterpret_f32, wasm_f32_reinterpret_i32,

    // Memory (i64)
    wasm_i64_load, wasm_i64_store,
    wasm_i64_load8_s, wasm_i64_load8_u, wasm_i64_load16_s, wasm_i64_load16_u,
    wasm_i64_load32_s, wasm_i64_load32_u,
    wasm_i64_store8, wasm_i64_store16, wasm_i64_store32,

    // Memory (i32)
    wasm_i32_load, wasm_i32_store,
    wasm_i32_load8_s, wasm_i32_load8_u, wasm_i32_load16_s, wasm_i32_load16_u,
    wasm_i32_store8, wasm_i32_store16,

    // Memory (float)
    wasm_f64_load, wasm_f64_store,
    wasm_f32_load, wasm_f32_store,

    // Variables
    wasm_local_get, wasm_local_set, wasm_local_tee,
    wasm_global_get, wasm_global_set,

    // Control flow
    wasm_call, wasm_call_indirect,
    wasm_drop, wasm_select,
    wasm_unreachable, wasm_nop,
    wasm_return,

    // Lowered operations (like Go's OpWasmLowered*)
    wasm_lowered_move, wasm_lowered_zero,
    wasm_lowered_nil_check,
    wasm_lowered_static_call, wasm_lowered_closure_call, wasm_lowered_inter_call,

    // ARC runtime calls
    wasm_lowered_retain, wasm_lowered_release,

    pub fn info(self: Op) OpInfo { return op_info_table[@intFromEnum(self)]; }
    pub fn isCall(self: Op) bool { return self.info().call; }
    pub fn name(self: Op) []const u8 { return self.info().name; }
    pub fn isGeneric(self: Op) bool { return self.info().generic; }
    pub fn isCommutative(self: Op) bool { return self.info().commutative; }
    pub fn hasSideEffects(self: Op) bool { return self.info().has_side_effects; }
    pub fn isRematerializable(self: Op) bool { return self.info().rematerializable; }
    pub fn readsMemory(self: Op) bool { return self.info().reads_memory; }
    pub fn writesMemory(self: Op) bool { return self.info().writes_memory; }
};

pub const OpInfo = struct {
    name: []const u8 = "",
    reg: RegInfo = .{},
    aux_type: AuxType = .none,
    arg_len: i8 = 0,
    generic: bool = true,
    rematerializable: bool = false,
    commutative: bool = false,
    result_in_arg0: bool = false,
    clobber_flags: bool = false,
    call: bool = false,
    has_side_effects: bool = false,
    reads_memory: bool = false,
    writes_memory: bool = false,
    nil_check: bool = false,
    fault_on_nil_arg0: bool = false,
    uses_flags: bool = false,
};

pub const RegInfo = struct {
    inputs: []const InputInfo = &.{},
    outputs: []const OutputInfo = &.{},
    clobbers: RegMask = 0,
};

pub const InputInfo = struct {
    idx: u8,
    regs: RegMask = 0,
};

pub const OutputInfo = struct {
    idx: u8,
    regs: RegMask = 0,
};

pub const AuxType = enum {
    none, bool_, int8, int16, int32, int64, float32, float64,
    string, symbol, symbol_off, symbol_val_off, call, type_ref, cond, arch,
};

// Register masks
const GP_REGS: RegMask = 0x7FFFFFFF;
const CALLER_SAVED: RegMask = 0x0007FFFF;

const op_info_table = blk: {
    var table: [@typeInfo(Op).@"enum".fields.len]OpInfo = undefined;
    for (&table) |*e| e.* = .{};

    // Invalid
    table[@intFromEnum(Op.invalid)] = .{ .name = "Invalid" };

    // Memory
    table[@intFromEnum(Op.init_mem)] = .{ .name = "InitMem", .rematerializable = true };

    // Constants (all rematerializable)
    table[@intFromEnum(Op.const_bool)] = .{ .name = "ConstBool", .aux_type = .int64, .rematerializable = true };
    table[@intFromEnum(Op.const_int)] = .{ .name = "ConstInt", .aux_type = .int64, .rematerializable = true };
    table[@intFromEnum(Op.const_float)] = .{ .name = "ConstFloat", .aux_type = .float64, .rematerializable = true };
    table[@intFromEnum(Op.const_nil)] = .{ .name = "ConstNil", .rematerializable = true };
    table[@intFromEnum(Op.const_string)] = .{ .name = "ConstString", .aux_type = .string, .rematerializable = true };
    table[@intFromEnum(Op.const_ptr)] = .{ .name = "ConstPtr", .aux_type = .int64, .rematerializable = true };
    for ([_]Op{ .const_8, .const_16, .const_32, .const_64 }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .aux_type = .int64, .rematerializable = true };
    }

    // Arithmetic (2 args, generic)
    for ([_]Op{ .add, .mul }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .sub, .div, .udiv, .mod, .umod }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2 };
    }
    table[@intFromEnum(Op.neg)] = .{ .name = "Neg", .arg_len = 1 };

    // Sized arithmetic
    for ([_]Op{ .add8, .add16, .add32, .add64, .mul8, .mul16, .mul32, .mul64 }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .sub8, .sub16, .sub32, .sub64 }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2 };
    }

    // Bitwise
    for ([_]Op{ .and_, .or_, .xor }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .shl, .shr, .sar }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2 };
    }
    table[@intFromEnum(Op.not)] = .{ .name = "Not", .arg_len = 1 };

    // Comparisons (all produce bool, 2 args)
    for ([_]Op{ .eq, .ne }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .lt, .le, .gt, .ge, .ult, .ule, .ugt, .uge }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2 };
    }

    // Conversions
    table[@intFromEnum(Op.convert)] = .{ .name = "Convert", .arg_len = 1, .aux_type = .type_ref };

    // Memory ops
    table[@intFromEnum(Op.load)] = .{ .name = "Load", .arg_len = 2, .reads_memory = true };
    table[@intFromEnum(Op.store)] = .{ .name = "Store", .arg_len = 3, .writes_memory = true, .has_side_effects = true };
    for ([_]Op{ .load8, .load16, .load32, .load64 }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 2, .reads_memory = true };
    }
    for ([_]Op{ .store8, .store16, .store32, .store64 }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = 3, .writes_memory = true, .has_side_effects = true };
    }

    // Address computation
    table[@intFromEnum(Op.addr)] = .{ .name = "Addr", .aux_type = .symbol };
    table[@intFromEnum(Op.local_addr)] = .{ .name = "LocalAddr", .aux_type = .int64 };
    table[@intFromEnum(Op.global_addr)] = .{ .name = "GlobalAddr", .aux_type = .symbol };
    table[@intFromEnum(Op.off_ptr)] = .{ .name = "OffPtr", .arg_len = 1, .aux_type = .int64 };
    table[@intFromEnum(Op.add_ptr)] = .{ .name = "AddPtr", .arg_len = 2 };
    table[@intFromEnum(Op.sub_ptr)] = .{ .name = "SubPtr", .arg_len = 2 };

    // Control flow
    table[@intFromEnum(Op.phi)] = .{ .name = "Phi", .arg_len = -1 };
    table[@intFromEnum(Op.copy)] = .{ .name = "Copy", .arg_len = 1 };
    table[@intFromEnum(Op.fwd_ref)] = .{ .name = "FwdRef", .aux_type = .int64 };
    table[@intFromEnum(Op.arg)] = .{ .name = "Arg", .aux_type = .int64, .rematerializable = true };
    table[@intFromEnum(Op.cond_select)] = .{ .name = "CondSelect", .arg_len = 3 };

    // Tuple ops
    table[@intFromEnum(Op.select0)] = .{ .name = "Select0", .arg_len = 1 };
    table[@intFromEnum(Op.select1)] = .{ .name = "Select1", .arg_len = 1 };
    table[@intFromEnum(Op.select_n)] = .{ .name = "SelectN", .arg_len = 1, .aux_type = .int64 };
    table[@intFromEnum(Op.make_tuple)] = .{ .name = "MakeTuple", .arg_len = -1 };

    // String/slice
    table[@intFromEnum(Op.string_len)] = .{ .name = "StringLen", .arg_len = 1 };
    table[@intFromEnum(Op.string_ptr)] = .{ .name = "StringPtr", .arg_len = 1 };
    table[@intFromEnum(Op.string_make)] = .{ .name = "StringMake", .arg_len = 2 };
    table[@intFromEnum(Op.slice_len)] = .{ .name = "SliceLen", .arg_len = 1 };
    table[@intFromEnum(Op.slice_ptr)] = .{ .name = "SlicePtr", .arg_len = 1 };
    table[@intFromEnum(Op.slice_make)] = .{ .name = "SliceMake", .arg_len = 2 };
    table[@intFromEnum(Op.string_concat)] = .{ .name = "StringConcat", .arg_len = 2, .has_side_effects = true, .call = true };

    // Calls
    for ([_]Op{ .call, .tail_call, .static_call, .closure_call, .inter_call }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .arg_len = -1, .call = true, .has_side_effects = true, .aux_type = .call };
    }

    // Safety checks
    table[@intFromEnum(Op.nil_check)] = .{ .name = "NilCheck", .arg_len = 2, .nil_check = true, .has_side_effects = true };
    table[@intFromEnum(Op.is_non_nil)] = .{ .name = "IsNonNil", .arg_len = 1 };
    table[@intFromEnum(Op.is_nil)] = .{ .name = "IsNil", .arg_len = 1 };
    table[@intFromEnum(Op.bounds_check)] = .{ .name = "BoundsCheck", .arg_len = 3, .has_side_effects = true };

    // Register allocation
    table[@intFromEnum(Op.store_reg)] = .{ .name = "StoreReg", .arg_len = 1 };
    table[@intFromEnum(Op.load_reg)] = .{ .name = "LoadReg", .arg_len = 0 };

    // ARC ops (generic - 1 arg: pointer to ARC object)
    table[@intFromEnum(Op.retain)] = .{ .name = "Retain", .arg_len = 1, .has_side_effects = true };
    table[@intFromEnum(Op.release)] = .{ .name = "Release", .arg_len = 1, .has_side_effects = true };

    // Move
    table[@intFromEnum(Op.move)] = .{ .name = "Move", .arg_len = 3, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };

    // ARM64 ops (machine-specific, not generic)
    for ([_]Op{ .arm64_add, .arm64_adds, .arm64_sub, .arm64_subs, .arm64_mul, .arm64_sdiv, .arm64_udiv, .arm64_madd, .arm64_msub }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 2 };
    }
    for ([_]Op{ .arm64_and, .arm64_orr, .arm64_eor, .arm64_bic, .arm64_lsl, .arm64_lsr, .arm64_asr }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 2 };
    }
    table[@intFromEnum(Op.arm64_cmp)] = .{ .name = "ARM64Cmp", .generic = false, .arg_len = 2, .clobber_flags = true };
    table[@intFromEnum(Op.arm64_movd)] = .{ .name = "ARM64MovD", .generic = false, .arg_len = 1 };
    table[@intFromEnum(Op.arm64_movz)] = .{ .name = "ARM64MovZ", .generic = false, .aux_type = .int64 };
    for ([_]Op{ .arm64_ldr, .arm64_ldrw, .arm64_ldrh, .arm64_ldrb }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    }
    for ([_]Op{ .arm64_str, .arm64_strw, .arm64_strh, .arm64_strb }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };
    }
    table[@intFromEnum(Op.arm64_bl)] = .{ .name = "ARM64BL", .generic = false, .call = true, .has_side_effects = true };
    table[@intFromEnum(Op.arm64_blr)] = .{ .name = "ARM64BLR", .generic = false, .arg_len = 1, .call = true, .has_side_effects = true };
    table[@intFromEnum(Op.arm64_ret)] = .{ .name = "ARM64Ret", .generic = false, .has_side_effects = true };
    table[@intFromEnum(Op.arm64_csel)] = .{ .name = "ARM64CSel", .generic = false, .arg_len = 2, .uses_flags = true };
    table[@intFromEnum(Op.arm64_add_imm)] = .{ .name = "ARM64AddImm", .generic = false, .arg_len = 1, .aux_type = .int64 };
    table[@intFromEnum(Op.arm64_adrp)] = .{ .name = "ARM64ADRP", .generic = false, .aux_type = .symbol };

    // AMD64 ops (machine-specific)
    for ([_]Op{ .amd64_addq, .amd64_addl, .amd64_subq, .amd64_subl, .amd64_imulq, .amd64_imull }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 2 };
    }
    for ([_]Op{ .amd64_andq, .amd64_andl, .amd64_orq, .amd64_orl, .amd64_xorq, .amd64_xorl }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 2 };
    }
    table[@intFromEnum(Op.amd64_cmpq)] = .{ .name = "AMD64CmpQ", .generic = false, .arg_len = 2, .clobber_flags = true };
    table[@intFromEnum(Op.amd64_movq)] = .{ .name = "AMD64MovQ", .generic = false, .arg_len = 1 };
    table[@intFromEnum(Op.amd64_movabs)] = .{ .name = "AMD64MovAbs", .generic = false, .aux_type = .int64 };
    for ([_]Op{ .amd64_loadq, .amd64_loadl, .amd64_loadw, .amd64_loadb }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    }
    for ([_]Op{ .amd64_storeq, .amd64_storel, .amd64_storew, .amd64_storeb }) |op| {
        table[@intFromEnum(op)] = .{ .name = @tagName(op), .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };
    }
    table[@intFromEnum(Op.amd64_call)] = .{ .name = "AMD64Call", .generic = false, .call = true, .has_side_effects = true };
    table[@intFromEnum(Op.amd64_ret)] = .{ .name = "AMD64Ret", .generic = false, .has_side_effects = true };
    table[@intFromEnum(Op.amd64_leaq)] = .{ .name = "AMD64LeaQ", .generic = false, .arg_len = 1, .aux_type = .int64 };

    // ========================================================================
    // Wasm ops (machine-specific, not generic)
    // ========================================================================

    // Constants
    table[@intFromEnum(Op.wasm_i64_const)] = .{ .name = "WasmI64Const", .generic = false, .aux_type = .int64, .rematerializable = true };
    table[@intFromEnum(Op.wasm_i32_const)] = .{ .name = "WasmI32Const", .generic = false, .aux_type = .int32, .rematerializable = true };
    table[@intFromEnum(Op.wasm_f64_const)] = .{ .name = "WasmF64Const", .generic = false, .aux_type = .float64, .rematerializable = true };
    table[@intFromEnum(Op.wasm_f32_const)] = .{ .name = "WasmF32Const", .generic = false, .aux_type = .float32, .rematerializable = true };

    // Integer arithmetic i64 (commutative: add, mul)
    for ([_]Op{ .wasm_i64_add, .wasm_i64_mul }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_i64_sub, .wasm_i64_div_s, .wasm_i64_div_u, .wasm_i64_rem_s, .wasm_i64_rem_u }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }

    // Integer arithmetic i32 (commutative: add, mul)
    for ([_]Op{ .wasm_i32_add, .wasm_i32_mul }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_i32_sub, .wasm_i32_div_s, .wasm_i32_div_u, .wasm_i32_rem_s, .wasm_i32_rem_u }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }

    // Integer bitwise i64 (commutative: and, or, xor)
    for ([_]Op{ .wasm_i64_and, .wasm_i64_or, .wasm_i64_xor }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_i64_shl, .wasm_i64_shr_s, .wasm_i64_shr_u, .wasm_i64_rotl, .wasm_i64_rotr }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }
    for ([_]Op{ .wasm_i64_clz, .wasm_i64_ctz, .wasm_i64_popcnt }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 1 };
    }

    // Integer bitwise i32 (commutative: and, or, xor)
    for ([_]Op{ .wasm_i32_and, .wasm_i32_or, .wasm_i32_xor }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_i32_shl, .wasm_i32_shr_s, .wasm_i32_shr_u, .wasm_i32_rotl, .wasm_i32_rotr }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }
    for ([_]Op{ .wasm_i32_clz, .wasm_i32_ctz, .wasm_i32_popcnt }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 1 };
    }

    // Integer comparison i64
    table[@intFromEnum(Op.wasm_i64_eqz)] = .{ .name = "WasmI64Eqz", .generic = false, .arg_len = 1 };
    for ([_]Op{ .wasm_i64_eq, .wasm_i64_ne }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_i64_lt_s, .wasm_i64_lt_u, .wasm_i64_gt_s, .wasm_i64_gt_u, .wasm_i64_le_s, .wasm_i64_le_u, .wasm_i64_ge_s, .wasm_i64_ge_u }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }

    // Integer comparison i32
    table[@intFromEnum(Op.wasm_i32_eqz)] = .{ .name = "WasmI32Eqz", .generic = false, .arg_len = 1 };
    for ([_]Op{ .wasm_i32_eq, .wasm_i32_ne }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_i32_lt_s, .wasm_i32_lt_u, .wasm_i32_gt_s, .wasm_i32_gt_u, .wasm_i32_le_s, .wasm_i32_le_u, .wasm_i32_ge_s, .wasm_i32_ge_u }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }

    // Float arithmetic f64 (commutative: add, mul, min, max)
    for ([_]Op{ .wasm_f64_add, .wasm_f64_mul, .wasm_f64_min, .wasm_f64_max }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_f64_sub, .wasm_f64_div, .wasm_f64_copysign }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }
    for ([_]Op{ .wasm_f64_neg, .wasm_f64_abs, .wasm_f64_sqrt, .wasm_f64_ceil, .wasm_f64_floor, .wasm_f64_trunc, .wasm_f64_nearest }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 1 };
    }

    // Float arithmetic f32 (commutative: add, mul, min, max)
    for ([_]Op{ .wasm_f32_add, .wasm_f32_mul, .wasm_f32_min, .wasm_f32_max }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_f32_sub, .wasm_f32_div, .wasm_f32_copysign }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }
    for ([_]Op{ .wasm_f32_neg, .wasm_f32_abs, .wasm_f32_sqrt, .wasm_f32_ceil, .wasm_f32_floor, .wasm_f32_trunc, .wasm_f32_nearest }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 1 };
    }

    // Float comparison f64 (eq/ne commutative)
    for ([_]Op{ .wasm_f64_eq, .wasm_f64_ne }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_f64_lt, .wasm_f64_gt, .wasm_f64_le, .wasm_f64_ge }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }

    // Float comparison f32 (eq/ne commutative)
    for ([_]Op{ .wasm_f32_eq, .wasm_f32_ne }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .commutative = true };
    }
    for ([_]Op{ .wasm_f32_lt, .wasm_f32_gt, .wasm_f32_le, .wasm_f32_ge }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2 };
    }

    // Conversions (all 1 arg)
    for ([_]Op{
        .wasm_i64_trunc_f64_s,    .wasm_i64_trunc_f64_u,    .wasm_i64_trunc_f32_s,    .wasm_i64_trunc_f32_u,
        .wasm_i32_trunc_f64_s,    .wasm_i32_trunc_f64_u,    .wasm_i32_trunc_f32_s,    .wasm_i32_trunc_f32_u,
        .wasm_f64_convert_i64_s,  .wasm_f64_convert_i64_u,  .wasm_f64_convert_i32_s,  .wasm_f64_convert_i32_u,
        .wasm_f32_convert_i64_s,  .wasm_f32_convert_i64_u,  .wasm_f32_convert_i32_s,  .wasm_f32_convert_i32_u,
        .wasm_i64_extend_i32_s,   .wasm_i64_extend_i32_u,
        .wasm_i32_wrap_i64,
        .wasm_f64_promote_f32,    .wasm_f32_demote_f64,
        .wasm_i64_reinterpret_f64, .wasm_f64_reinterpret_i64,
        .wasm_i32_reinterpret_f32, .wasm_f32_reinterpret_i32,
    }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 1 };
    }

    // Memory i64
    table[@intFromEnum(Op.wasm_i64_load)] = .{ .name = "WasmI64Load", .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    table[@intFromEnum(Op.wasm_i64_store)] = .{ .name = "WasmI64Store", .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };
    for ([_]Op{ .wasm_i64_load8_s, .wasm_i64_load8_u, .wasm_i64_load16_s, .wasm_i64_load16_u, .wasm_i64_load32_s, .wasm_i64_load32_u }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    }
    for ([_]Op{ .wasm_i64_store8, .wasm_i64_store16, .wasm_i64_store32 }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };
    }

    // Memory i32
    table[@intFromEnum(Op.wasm_i32_load)] = .{ .name = "WasmI32Load", .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    table[@intFromEnum(Op.wasm_i32_store)] = .{ .name = "WasmI32Store", .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };
    for ([_]Op{ .wasm_i32_load8_s, .wasm_i32_load8_u, .wasm_i32_load16_s, .wasm_i32_load16_u }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    }
    for ([_]Op{ .wasm_i32_store8, .wasm_i32_store16 }) |o| {
        table[@intFromEnum(o)] = .{ .name = @tagName(o), .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };
    }

    // Memory float
    table[@intFromEnum(Op.wasm_f64_load)] = .{ .name = "WasmF64Load", .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    table[@intFromEnum(Op.wasm_f64_store)] = .{ .name = "WasmF64Store", .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_f32_load)] = .{ .name = "WasmF32Load", .generic = false, .arg_len = 1, .aux_type = .int64, .reads_memory = true };
    table[@intFromEnum(Op.wasm_f32_store)] = .{ .name = "WasmF32Store", .generic = false, .arg_len = 2, .aux_type = .int64, .writes_memory = true, .has_side_effects = true };

    // Variables
    table[@intFromEnum(Op.wasm_local_get)] = .{ .name = "WasmLocalGet", .generic = false, .aux_type = .int64, .rematerializable = true };
    table[@intFromEnum(Op.wasm_local_set)] = .{ .name = "WasmLocalSet", .generic = false, .arg_len = 1, .aux_type = .int64, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_local_tee)] = .{ .name = "WasmLocalTee", .generic = false, .arg_len = 1, .aux_type = .int64, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_global_get)] = .{ .name = "WasmGlobalGet", .generic = false, .aux_type = .int64 };
    table[@intFromEnum(Op.wasm_global_set)] = .{ .name = "WasmGlobalSet", .generic = false, .arg_len = 1, .aux_type = .int64, .has_side_effects = true };

    // Control flow
    table[@intFromEnum(Op.wasm_call)] = .{ .name = "WasmCall", .generic = false, .arg_len = -1, .aux_type = .call, .call = true, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_call_indirect)] = .{ .name = "WasmCallIndirect", .generic = false, .arg_len = -1, .aux_type = .call, .call = true, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_drop)] = .{ .name = "WasmDrop", .generic = false, .arg_len = 1, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_select)] = .{ .name = "WasmSelect", .generic = false, .arg_len = 3 };
    table[@intFromEnum(Op.wasm_unreachable)] = .{ .name = "WasmUnreachable", .generic = false, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_nop)] = .{ .name = "WasmNop", .generic = false };
    table[@intFromEnum(Op.wasm_return)] = .{ .name = "WasmReturn", .generic = false, .has_side_effects = true };

    // Lowered operations
    table[@intFromEnum(Op.wasm_lowered_move)] = .{ .name = "WasmLoweredMove", .generic = false, .arg_len = 1 };
    table[@intFromEnum(Op.wasm_lowered_zero)] = .{ .name = "WasmLoweredZero", .generic = false, .aux_type = .int64, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_lowered_nil_check)] = .{ .name = "WasmLoweredNilCheck", .generic = false, .arg_len = 1, .nil_check = true, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_lowered_static_call)] = .{ .name = "WasmLoweredStaticCall", .generic = false, .arg_len = -1, .aux_type = .call, .call = true, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_lowered_closure_call)] = .{ .name = "WasmLoweredClosureCall", .generic = false, .arg_len = -1, .aux_type = .call, .call = true, .has_side_effects = true };
    table[@intFromEnum(Op.wasm_lowered_inter_call)] = .{ .name = "WasmLoweredInterCall", .generic = false, .arg_len = -1, .aux_type = .call, .call = true, .has_side_effects = true };

    // Wasm ARC ops (lowered - call runtime functions)
    table[@intFromEnum(Op.wasm_lowered_retain)] = .{ .name = "WasmLoweredRetain", .generic = false, .arg_len = 1, .has_side_effects = true, .call = true };
    table[@intFromEnum(Op.wasm_lowered_release)] = .{ .name = "WasmLoweredRelease", .generic = false, .arg_len = 1, .has_side_effects = true, .call = true };

    break :blk table;
};

// ============================================================================
// Tests
// ============================================================================

test "Op info lookup" {
    const add_info = Op.add.info();
    try std.testing.expectEqualStrings("add", add_info.name);
    try std.testing.expect(add_info.commutative);
    try std.testing.expectEqual(@as(i8, 2), add_info.arg_len);
}

test "constant ops are rematerializable" {
    try std.testing.expect(Op.const_int.isRematerializable());
    try std.testing.expect(Op.const_bool.isRematerializable());
    try std.testing.expect(Op.const_float.isRematerializable());
}

test "call ops have call flag" {
    try std.testing.expect(Op.call.isCall());
    try std.testing.expect(Op.static_call.isCall());
    try std.testing.expect(!Op.add.isCall());
}

test "memory ops" {
    try std.testing.expect(Op.load.readsMemory());
    try std.testing.expect(Op.store.writesMemory());
    try std.testing.expect(Op.store.hasSideEffects());
}

test "generic vs machine ops" {
    try std.testing.expect(Op.add.isGeneric());
    try std.testing.expect(!Op.arm64_add.isGeneric());
    try std.testing.expect(!Op.amd64_addq.isGeneric());
}

test "wasm ops are not generic" {
    try std.testing.expect(!Op.wasm_i64_add.isGeneric());
    try std.testing.expect(!Op.wasm_i64_const.isGeneric());
    try std.testing.expect(!Op.wasm_f64_mul.isGeneric());
    try std.testing.expect(!Op.wasm_call.isGeneric());
}

test "wasm constants are rematerializable" {
    try std.testing.expect(Op.wasm_i64_const.isRematerializable());
    try std.testing.expect(Op.wasm_i32_const.isRematerializable());
    try std.testing.expect(Op.wasm_f64_const.isRematerializable());
    try std.testing.expect(Op.wasm_f32_const.isRematerializable());
    try std.testing.expect(Op.wasm_local_get.isRematerializable());
}

test "wasm arithmetic ops have correct properties" {
    // Commutative ops
    try std.testing.expect(Op.wasm_i64_add.isCommutative());
    try std.testing.expect(Op.wasm_i64_mul.isCommutative());
    try std.testing.expect(Op.wasm_f64_add.isCommutative());
    // Non-commutative ops
    try std.testing.expect(!Op.wasm_i64_sub.isCommutative());
    try std.testing.expect(!Op.wasm_i64_div_s.isCommutative());
    try std.testing.expect(!Op.wasm_f64_div.isCommutative());
}

test "wasm memory ops have correct properties" {
    try std.testing.expect(Op.wasm_i64_load.readsMemory());
    try std.testing.expect(!Op.wasm_i64_load.writesMemory());
    try std.testing.expect(Op.wasm_i64_store.writesMemory());
    try std.testing.expect(Op.wasm_i64_store.hasSideEffects());
}

test "wasm call ops" {
    try std.testing.expect(Op.wasm_call.isCall());
    try std.testing.expect(Op.wasm_call.hasSideEffects());
    try std.testing.expect(Op.wasm_lowered_static_call.isCall());
}
