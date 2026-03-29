//! Wasm Lowering Pass - Convert generic SSA ops to Wasm-specific ops.
//!
//! This pass transforms architecture-independent ops (add, sub, load) into
//! Wasm-specific ops (wasm_i64_add, wasm_i64_sub, wasm_i64_load).
//!
//! After this pass, all remaining ops should be either:
//! - Wasm-specific (wasm_*)
//! - Control flow (phi, copy)
//! - Structural (arg, select0, select1, make_tuple)

const std = @import("std");
const Value = @import("../value.zig").Value;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const TypeIndex = foundation.types.TypeIndex;
const foundation = @import("foundation");
const TypeRegistry = foundation.types.TypeRegistry;
const debug = foundation.debug;

pub fn lower(f: *Func) !void {
    var total_values: usize = 0;
    for (f.blocks.items) |b| total_values += b.values.items.len;
    debug.log(.codegen, "=== lower_wasm pass for '{s}' ({d} blocks, {d} values) ===", .{
        f.name, f.blocks.items.len, total_values,
    });

    var lowered: usize = 0;
    var unchanged: usize = 0;
    var unlowered_ops: [16]Op = undefined;
    var unlowered_count: usize = 0;

    for (f.blocks.items) |block| {
        for (block.values.items) |v| {
            const old_op = v.op;
            if (lowerValue(v)) {
                lowered += 1;
                debug.log(.codegen, "  lower v{d}: {s} -> {s}", .{ v.id, @tagName(old_op), @tagName(v.op) });
            } else {
                unchanged += 1;
                if (!isExpectedUnlowered(old_op) and unlowered_count < 16) {
                    var found = false;
                    for (unlowered_ops[0..unlowered_count]) |existing| {
                        if (existing == old_op) { found = true; break; }
                    }
                    if (!found) {
                        unlowered_ops[unlowered_count] = old_op;
                        unlowered_count += 1;
                    }
                }
            }
        }
    }

    debug.log(.codegen, "=== lower_wasm complete for '{s}': lowered {d}, unchanged {d} ===", .{
        f.name, lowered, unchanged,
    });
    if (unlowered_count > 0) {
        for (unlowered_ops[0..unlowered_count]) |op| {
            debug.log(.codegen, "  WARNING: op '{s}' not lowered (expected?)", .{@tagName(op)});
        }
    }

    stripMemoryThreading(f);

    optimizeTailCalls(f);
}

fn stripMemoryThreading(f: *Func) void {
    debug.log(.codegen, "stripMemoryThreading: '{s}'", .{f.name});

    for (f.blocks.items) |block| {
        var keep: usize = 0;
        for (block.values.items) |v| {
            const is_mem_value = v.op == .init_mem or v.type_idx == TypeRegistry.SSA_MEM;
            if (is_mem_value and (v.op == .init_mem or v.op == .phi or v.op == .copy or v.op == .fwd_ref)) {
                continue;
            }
            block.values.items[keep] = v;
            keep += 1;
        }
        block.values.shrinkRetainingCapacity(keep);
    }

    for (f.blocks.items) |block| {
        for (block.values.items) |v| {
            if (v.args.len == 0) continue;
            const info = v.op.info();
            if (info.arg_len >= 0) {
                const expected: usize = @intCast(info.arg_len);
                if (v.args.len > expected) {
                    debug.log(.codegen, "  strip args: v{d} {s} {d} -> {d}", .{ v.id, @tagName(v.op), v.args.len, expected });
                    v.args = v.args[0..expected];
                }
                if (info.writes_memory and v.type_idx == TypeRegistry.SSA_MEM) {
                    v.type_idx = TypeRegistry.VOID;
                }
                continue;
            }
            const last = v.args[v.args.len - 1];
            if (last.type_idx == TypeRegistry.SSA_MEM or last.op == .init_mem or
                (last.type_idx == TypeRegistry.VOID and last.writesMemory()))
            {
                debug.log(.codegen, "  strip call: v{d} {s} {d} -> {d}", .{ v.id, @tagName(v.op), v.args.len, v.args.len - 1 });
                v.args = v.args[0 .. v.args.len - 1];
            }
        }
    }
}

fn lowerValue(v: *Value) bool {
    const new_op: ?Op = switch (v.op) {
        .const_int, .const_64 => .wasm_i64_const,
        .const_32 => .wasm_i32_const,
        .const_float => .wasm_f64_const,

        .add, .add64 => .wasm_i64_add,
        .sub, .sub64 => .wasm_i64_sub,
        .mul, .mul64 => .wasm_i64_mul,
        .div => .wasm_i64_div_s,
        .udiv => .wasm_i64_div_u,
        .mod => .wasm_i64_rem_s,
        .umod => .wasm_i64_rem_u,
        .neg => null,

        .add32 => .wasm_i32_add,
        .sub32 => .wasm_i32_sub,
        .mul32 => .wasm_i32_mul,

        .and_, .and64 => .wasm_i64_and,
        .or_, .or64 => .wasm_i64_or,
        .xor, .xor64 => .wasm_i64_xor,
        .shl, .shl64 => .wasm_i64_shl,
        .shr, .shr64 => .wasm_i64_shr_u,
        .sar, .sar64 => .wasm_i64_shr_s,
        .not => null,
        .bool_not => .wasm_i64_eqz,

        .and32 => .wasm_i32_and,
        .or32 => .wasm_i32_or,
        .xor32 => .wasm_i32_xor,
        .shl32 => .wasm_i32_shl,
        .shr32 => .wasm_i32_shr_u,
        .sar32 => .wasm_i32_shr_s,

        .clz64 => .wasm_i64_clz,
        .ctz64 => .wasm_i64_ctz,
        .popcnt64 => .wasm_i64_popcnt,
        .clz32 => .wasm_i32_clz,
        .ctz32 => .wasm_i32_ctz,
        .popcnt32 => .wasm_i32_popcnt,

        .eq, .eq64 => .wasm_i64_eq,
        .ne, .ne64 => .wasm_i64_ne,
        .lt, .lt64 => .wasm_i64_lt_s,
        .le, .le64 => .wasm_i64_le_s,
        .gt, .gt64 => .wasm_i64_gt_s,
        .ge, .ge64 => .wasm_i64_ge_s,
        .ult => .wasm_i64_lt_u,
        .ule => .wasm_i64_le_u,
        .ugt => .wasm_i64_gt_u,
        .uge => .wasm_i64_ge_u,

        .eq32 => .wasm_i32_eq,
        .ne32 => .wasm_i32_ne,
        .lt32 => .wasm_i32_lt_s,
        .le32 => .wasm_i32_le_s,
        .gt32 => .wasm_i32_gt_s,
        .ge32 => .wasm_i32_ge_s,

        .add64f => .wasm_f64_add,
        .sub64f => .wasm_f64_sub,
        .mul64f => .wasm_f64_mul,
        .div64f => .wasm_f64_div,
        .neg64f => .wasm_f64_neg,
        .sqrt64f => .wasm_f64_sqrt,

        .add32f => .wasm_f32_add,
        .sub32f => .wasm_f32_sub,
        .mul32f => .wasm_f32_mul,
        .div32f => .wasm_f32_div,
        .neg32f => .wasm_f32_neg,
        .sqrt32f => .wasm_f32_sqrt,

        .eq64f => .wasm_f64_eq,
        .ne64f => .wasm_f64_ne,
        .lt64f => .wasm_f64_lt,
        .le64f => .wasm_f64_le,
        .gt64f => .wasm_f64_gt,
        .ge64f => .wasm_f64_ge,

        .eq32f => .wasm_f32_eq,
        .ne32f => .wasm_f32_ne,
        .lt32f => .wasm_f32_lt,
        .le32f => .wasm_f32_le,
        .gt32f => .wasm_f32_gt,
        .ge32f => .wasm_f32_ge,

        .sign_ext32to64 => .wasm_i64_extend_i32_s,
        .zero_ext32to64 => .wasm_i64_extend_i32_u,
        .trunc64to32 => .wasm_i32_wrap_i64,
        .cvt64to64f => .wasm_f64_convert_i64_s,
        .cvt32to64f => .wasm_f64_convert_i32_s,
        .cvt64fto64 => .wasm_i64_trunc_f64_s,
        .cvt64fto32 => .wasm_i32_trunc_f64_s,
        .cvt32fto64f => .wasm_f64_promote_f32,
        .cvt64fto32f => .wasm_f32_demote_f64,
        .cvt64to32f => .wasm_f32_convert_i64_s,
        .cvt32fto64 => .wasm_i64_trunc_f32_s,

        .load, .load64 => if (isFloatType(v.type_idx)) .wasm_f64_load else .wasm_i64_load,
        .load32 => .wasm_i64_load32_u,
        .load32s => .wasm_i64_load32_s,
        .load16 => .wasm_i64_load16_u,
        .load16s => .wasm_i64_load16_s,
        .load8 => .wasm_i64_load8_u,
        .load8s => .wasm_i64_load8_s,

        .store, .store64 => if (v.args.len >= 2 and isFloatType(v.args[1].type_idx)) .wasm_f64_store else .wasm_i64_store,
        .store32 => .wasm_i64_store32,
        .store16 => .wasm_i64_store16,
        .store8 => .wasm_i64_store8,

        .static_call => .wasm_lowered_static_call,
        .call => .wasm_call,
        .closure_call => .wasm_lowered_closure_call,
        .inter_call => .wasm_lowered_inter_call,

        .nil_check => .wasm_lowered_nil_check,

        .move => .wasm_lowered_move,
        .zero => .wasm_lowered_zero,

        .retain, .release => .wasm_nop,

        .phi, .copy, .fwd_ref, .arg,
        .select0, .select1, .select_n, .make_tuple, .cond_select,
        .init_mem, .invalid,
        .const_bool, .const_nil, .const_string, .const_ptr,
        .string_make, .string_concat,
        .slice_make,
        .opt_make,
        .addr, .local_addr, .global_addr, .metadata_addr, .off_ptr, .add_ptr, .sub_ptr,
        .var_def, .var_live, .var_kill,
        .is_non_nil, .is_nil, .bounds_check, .slice_bounds,
        .store_reg, .load_reg,
        .store_wb,
        => null,

        .slice_ptr, .slice_len, .slice_cap, .string_ptr, .string_len,
        .opt_tag, .opt_data,
        => null,

        .wasm_i64_const, .wasm_i32_const, .wasm_f64_const, .wasm_f32_const,
        .wasm_i64_add, .wasm_i64_sub, .wasm_i64_mul, .wasm_i64_div_s, .wasm_i64_div_u,
        .wasm_i64_rem_s, .wasm_i64_rem_u,
        .wasm_i32_add, .wasm_i32_sub, .wasm_i32_mul, .wasm_i32_div_s, .wasm_i32_div_u,
        .wasm_i32_rem_s, .wasm_i32_rem_u,
        .wasm_i64_and, .wasm_i64_or, .wasm_i64_xor, .wasm_i64_shl, .wasm_i64_shr_s, .wasm_i64_shr_u,
        .wasm_i64_clz, .wasm_i64_ctz, .wasm_i64_popcnt, .wasm_i64_rotl, .wasm_i64_rotr,
        .wasm_i32_and, .wasm_i32_or, .wasm_i32_xor, .wasm_i32_shl, .wasm_i32_shr_s, .wasm_i32_shr_u,
        .wasm_i32_clz, .wasm_i32_ctz, .wasm_i32_popcnt, .wasm_i32_rotl, .wasm_i32_rotr,
        .wasm_i64_eqz, .wasm_i64_eq, .wasm_i64_ne,
        .wasm_i64_lt_s, .wasm_i64_lt_u, .wasm_i64_gt_s, .wasm_i64_gt_u,
        .wasm_i64_le_s, .wasm_i64_le_u, .wasm_i64_ge_s, .wasm_i64_ge_u,
        .wasm_i32_eqz, .wasm_i32_eq, .wasm_i32_ne,
        .wasm_i32_lt_s, .wasm_i32_lt_u, .wasm_i32_gt_s, .wasm_i32_gt_u,
        .wasm_i32_le_s, .wasm_i32_le_u, .wasm_i32_ge_s, .wasm_i32_ge_u,
        .wasm_f64_add, .wasm_f64_sub, .wasm_f64_mul, .wasm_f64_div,
        .wasm_f64_neg, .wasm_f64_abs, .wasm_f64_sqrt, .wasm_f64_ceil, .wasm_f64_floor,
        .wasm_f64_trunc, .wasm_f64_nearest, .wasm_f64_min, .wasm_f64_max, .wasm_f64_copysign,
        .wasm_f32_add, .wasm_f32_sub, .wasm_f32_mul, .wasm_f32_div,
        .wasm_f32_neg, .wasm_f32_abs, .wasm_f32_sqrt, .wasm_f32_ceil, .wasm_f32_floor,
        .wasm_f32_trunc, .wasm_f32_nearest, .wasm_f32_min, .wasm_f32_max, .wasm_f32_copysign,
        .wasm_f64_eq, .wasm_f64_ne, .wasm_f64_lt, .wasm_f64_gt, .wasm_f64_le, .wasm_f64_ge,
        .wasm_f32_eq, .wasm_f32_ne, .wasm_f32_lt, .wasm_f32_gt, .wasm_f32_le, .wasm_f32_ge,
        .wasm_i64_trunc_f64_s, .wasm_i64_trunc_f64_u, .wasm_i64_trunc_f32_s, .wasm_i64_trunc_f32_u,
        .wasm_i32_trunc_f64_s, .wasm_i32_trunc_f64_u, .wasm_i32_trunc_f32_s, .wasm_i32_trunc_f32_u,
        .wasm_f64_convert_i64_s, .wasm_f64_convert_i64_u, .wasm_f64_convert_i32_s, .wasm_f64_convert_i32_u,
        .wasm_f32_convert_i64_s, .wasm_f32_convert_i64_u, .wasm_f32_convert_i32_s, .wasm_f32_convert_i32_u,
        .wasm_i64_extend_i32_s, .wasm_i64_extend_i32_u, .wasm_i32_wrap_i64,
        .wasm_f64_promote_f32, .wasm_f32_demote_f64,
        .wasm_i64_reinterpret_f64, .wasm_f64_reinterpret_i64,
        .wasm_i32_reinterpret_f32, .wasm_f32_reinterpret_i32,
        .wasm_i64_load, .wasm_i64_store,
        .wasm_i64_load8_s, .wasm_i64_load8_u, .wasm_i64_load16_s, .wasm_i64_load16_u,
        .wasm_i64_load32_s, .wasm_i64_load32_u,
        .wasm_i64_store8, .wasm_i64_store16, .wasm_i64_store32,
        .wasm_i32_load, .wasm_i32_store,
        .wasm_i32_load8_s, .wasm_i32_load8_u, .wasm_i32_load16_s, .wasm_i32_load16_u,
        .wasm_i32_store8, .wasm_i32_store16,
        .wasm_f64_load, .wasm_f64_store, .wasm_f32_load, .wasm_f32_store,
        .wasm_local_get, .wasm_local_set, .wasm_local_tee,
        .wasm_global_get, .wasm_global_set,
        .wasm_call, .wasm_call_indirect, .wasm_return_call, .wasm_drop, .wasm_select,
        .wasm_unreachable, .wasm_nop, .wasm_return,
        .wasm_lowered_move, .wasm_lowered_zero, .wasm_lowered_nil_check,
        .wasm_lowered_static_call, .wasm_lowered_closure_call, .wasm_lowered_inter_call,
        .wasm_gc_struct_new, .wasm_gc_struct_get, .wasm_gc_struct_set,
        .wasm_gc_array_new, .wasm_gc_array_new_default, .wasm_gc_array_new_fixed, .wasm_gc_array_new_data,
        .wasm_gc_array_get, .wasm_gc_array_set, .wasm_gc_array_len, .wasm_gc_array_copy,
        .wasm_gc_ref_test, .wasm_gc_ref_cast, .wasm_gc_br_on_cast,
        .wasm_gc_ref_null, .wasm_gc_ref_is_null, .wasm_gc_ref_eq,
        .wasm_gc_ref_func, .wasm_gc_call_ref, .wasm_gc_return_call_ref,
        .sp, .store_sp,
        => null,

        .arm64_add, .arm64_adds, .arm64_sub, .arm64_subs, .arm64_mul, .arm64_sdiv, .arm64_udiv,
        .arm64_madd, .arm64_msub, .arm64_smulh, .arm64_umulh,
        .arm64_and, .arm64_orr, .arm64_eor, .arm64_bic, .arm64_orn, .arm64_eon, .arm64_mvn,
        .arm64_lsl, .arm64_lsr, .arm64_asr, .arm64_ror, .arm64_lslimm, .arm64_lsrimm, .arm64_asrimm,
        .arm64_cmp, .arm64_cmn, .arm64_tst,
        .arm64_movd, .arm64_movw, .arm64_movz, .arm64_movn, .arm64_movk,
        .arm64_ldr, .arm64_ldrw, .arm64_ldrh, .arm64_ldrb, .arm64_ldrsw, .arm64_ldrsh, .arm64_ldrsb, .arm64_ldp,
        .arm64_str, .arm64_strw, .arm64_strh, .arm64_strb, .arm64_stp,
        .arm64_adrp, .arm64_add_imm, .arm64_sub_imm,
        .arm64_bl, .arm64_blr, .arm64_br, .arm64_ret, .arm64_b, .arm64_bcond,
        .arm64_csel, .arm64_csinc, .arm64_csinv, .arm64_csneg, .arm64_cset,
        .arm64_clz, .arm64_rbit, .arm64_rev,
        .arm64_sxtb, .arm64_sxth, .arm64_sxtw, .arm64_uxtb, .arm64_uxth,
        .arm64_fadd, .arm64_fsub, .arm64_fmul, .arm64_fdiv, .arm64_fneg, .arm64_fsqrt,
        .arm64_fmov, .arm64_fcvtzs, .arm64_scvtf, .arm64_fcmp,
        .amd64_addq, .amd64_addl, .amd64_subq, .amd64_subl, .amd64_imulq, .amd64_imull,
        .amd64_idivq, .amd64_idivl, .amd64_divq, .amd64_divl,
        .amd64_andq, .amd64_andl, .amd64_orq, .amd64_orl, .amd64_xorq, .amd64_xorl,
        .amd64_shlq, .amd64_shll, .amd64_shrq, .amd64_shrl, .amd64_sarq, .amd64_sarl,
        .amd64_notq, .amd64_notl, .amd64_negq, .amd64_negl,
        .amd64_cmpq, .amd64_cmpl, .amd64_testq, .amd64_testl,
        .amd64_movq, .amd64_movl, .amd64_movb, .amd64_movw,
        .amd64_movabs, .amd64_movzx, .amd64_movsx,
        .amd64_leaq, .amd64_leal,
        .amd64_loadq, .amd64_loadl, .amd64_loadw, .amd64_loadb,
        .amd64_storeq, .amd64_storel, .amd64_storew, .amd64_storeb,
        .amd64_call, .amd64_ret, .amd64_jmp, .amd64_jcc,
        .amd64_setcc, .amd64_cmov,
        .amd64_pushq, .amd64_popq,
        .amd64_addsd, .amd64_subsd, .amd64_mulsd, .amd64_divsd,
        .amd64_movsd, .amd64_cvtsi2sd, .amd64_cvttsd2si,
        => null,

        .const_8, .const_16 => .wasm_i32_const,
        .add8, .add16 => .wasm_i32_add,
        .sub8, .sub16 => .wasm_i32_sub,
        .mul8, .mul16 => .wasm_i32_mul,
        .and8, .and16 => .wasm_i32_and,
        .or8, .or16 => .wasm_i32_or,
        .xor8, .xor16 => .wasm_i32_xor,
        .shl8, .shl16 => .wasm_i32_shl,
        .shr8, .shr16 => .wasm_i32_shr_u,
        .sar8, .sar16 => .wasm_i32_shr_s,
        .eq8, .eq16 => .wasm_i32_eq,
        .ne8, .ne16 => .wasm_i32_ne,
        .com8, .com16, .com32, .com64 => null,
        .sign_ext8to16, .sign_ext8to32, .sign_ext8to64,
        .sign_ext16to32, .sign_ext16to64,
        .zero_ext8to16, .zero_ext8to32, .zero_ext8to64,
        .zero_ext16to32, .zero_ext16to64,
        .trunc16to8, .trunc32to8, .trunc32to16, .trunc64to8, .trunc64to16,
        => null,
        .hmul32, .hmul32u, .hmul64, .hmul64u,
        .divmod32, .divmod64, .divmodu32, .divmodu64,
        => null,
        .convert => null,
        .cvt32to32f, .cvt32fto32 => null,
        .tail_call => .wasm_return_call,
        .atomic_load32, .atomic_load64, .atomic_store32, .atomic_store64,
        .atomic_add32, .atomic_add64, .atomic_cas32, .atomic_cas64,
        .atomic_exchange32, .atomic_exchange64,
        => null,
    };

    if (new_op) |op| {
        v.op = op;
        return true;
    }
    return false;
}

fn optimizeTailCalls(f: *Func) void {
    _ = f;
}

fn isFloatType(type_idx: TypeIndex) bool {
    return type_idx == TypeRegistry.F64 or type_idx == TypeRegistry.F32 or type_idx == TypeRegistry.UNTYPED_FLOAT;
}

fn isExpectedUnlowered(op: Op) bool {
    return switch (op) {
        .wasm_i64_add, .wasm_i64_sub, .wasm_i64_mul, .wasm_i64_div_s, .wasm_i64_div_u,
        .wasm_i64_rem_s, .wasm_i64_rem_u, .wasm_i64_and, .wasm_i64_or, .wasm_i64_xor,
        .wasm_i64_shl, .wasm_i64_shr_s, .wasm_i64_shr_u, .wasm_i64_const,
        .wasm_i64_eq, .wasm_i64_ne, .wasm_i64_lt_s, .wasm_i64_le_s,
        .wasm_i64_gt_s, .wasm_i64_ge_s, .wasm_i64_lt_u, .wasm_i64_le_u,
        .wasm_i64_gt_u, .wasm_i64_ge_u, .wasm_i64_eqz,
        .wasm_i32_const, .wasm_i32_add, .wasm_i32_sub, .wasm_i32_mul,
        .wasm_f64_add, .wasm_f64_sub, .wasm_f64_mul, .wasm_f64_div,
        .wasm_f32_add, .wasm_f32_sub, .wasm_f32_mul, .wasm_f32_div,
        .wasm_i64_load, .wasm_i64_store, .wasm_f64_load, .wasm_f64_store,
        .wasm_lowered_static_call, .wasm_lowered_closure_call, .wasm_lowered_inter_call,
        .wasm_lowered_move, .wasm_lowered_zero, .wasm_lowered_nil_check,
        .wasm_call, .wasm_call_indirect, .wasm_return_call,
        .wasm_nop, .wasm_unreachable, .wasm_return, .wasm_drop, .wasm_select,
        .wasm_gc_struct_new, .wasm_gc_struct_get, .wasm_gc_struct_set,
        .wasm_gc_array_new, .wasm_gc_array_get, .wasm_gc_array_set,
        .wasm_gc_ref_null, .wasm_gc_ref_is_null, .wasm_gc_ref_eq,
        .phi, .copy, .fwd_ref, .arg, .select0, .select1, .select_n,
        .make_tuple, .cond_select, .init_mem, .invalid,
        .const_bool, .const_nil, .const_string, .const_ptr,
        .string_make, .slice_make, .opt_make,
        .addr, .local_addr, .global_addr, .metadata_addr,
        .off_ptr, .add_ptr, .sub_ptr,
        .sp, .store_sp,
        => true,
        else => false,
    };
}

const testing = std.testing;

test "lower empty function" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "empty");
    defer f.deinit();

    _ = try f.newBlock(.first);
    try lower(&f);
}

test "lower const_int to wasm_i64_const" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "const");
    defer f.deinit();

    const b = try f.newBlock(.first);
    const v = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v.aux_int = 42;
    try b.addValue(allocator, v);

    try lower(&f);

    try testing.expectEqual(Op.wasm_i64_const, b.values.items[0].op);
    try testing.expectEqual(@as(i64, 42), b.values.items[0].aux_int);
}

test "lower add to wasm_i64_add" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "add");
    defer f.deinit();

    const b = try f.newBlock(.first);

    const c1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    c1.aux_int = 1;
    try b.addValue(allocator, c1);

    const c2 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    c2.aux_int = 2;
    try b.addValue(allocator, c2);

    const add = try f.newValue(.add, @enumFromInt(0), b, .{});
    add.addArg(c1);
    add.addArg(c2);
    try b.addValue(allocator, add);

    try lower(&f);

    try testing.expectEqual(Op.wasm_i64_const, b.values.items[0].op);
    try testing.expectEqual(Op.wasm_i64_const, b.values.items[1].op);
    try testing.expectEqual(Op.wasm_i64_add, b.values.items[2].op);
}

test "lower comparisons" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "cmp");
    defer f.deinit();

    const b = try f.newBlock(.first);

    const c1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, c1);
    const c2 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, c2);

    const lt = try f.newValue(.lt, @enumFromInt(0), b, .{});
    lt.addArg(c1);
    lt.addArg(c2);
    try b.addValue(allocator, lt);

    const ult_v = try f.newValue(.ult, @enumFromInt(0), b, .{});
    ult_v.addArg(c1);
    ult_v.addArg(c2);
    try b.addValue(allocator, ult_v);

    try lower(&f);

    try testing.expectEqual(Op.wasm_i64_lt_s, b.values.items[2].op);
    try testing.expectEqual(Op.wasm_i64_lt_u, b.values.items[3].op);
}

test "lower float arithmetic" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "float");
    defer f.deinit();

    const b = try f.newBlock(.first);

    const c1 = try f.newValue(.const_float, @enumFromInt(0), b, .{});
    try b.addValue(allocator, c1);
    const c2 = try f.newValue(.const_float, @enumFromInt(0), b, .{});
    try b.addValue(allocator, c2);

    const add = try f.newValue(.add64f, @enumFromInt(0), b, .{});
    add.addArg(c1);
    add.addArg(c2);
    try b.addValue(allocator, add);

    try lower(&f);

    try testing.expectEqual(Op.wasm_f64_const, b.values.items[0].op);
    try testing.expectEqual(Op.wasm_f64_add, b.values.items[2].op);
}

test "lower memory operations" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "mem");
    defer f.deinit();

    const b = try f.newBlock(.first);

    const addr_v = try f.newValue(.const_ptr, @enumFromInt(0), b, .{});
    try b.addValue(allocator, addr_v);

    const load_v = try f.newValue(.load, @enumFromInt(0), b, .{});
    load_v.addArg(addr_v);
    try b.addValue(allocator, load_v);

    const store_v = try f.newValue(.store, @enumFromInt(0), b, .{});
    store_v.addArg(addr_v);
    store_v.addArg(load_v);
    try b.addValue(allocator, store_v);

    try lower(&f);

    try testing.expectEqual(Op.wasm_i64_load, b.values.items[1].op);
    try testing.expectEqual(Op.wasm_i64_store, b.values.items[2].op);
}

test "lower static_call to wasm_lowered_static_call" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "call");
    defer f.deinit();

    const b = try f.newBlock(.first);

    const call_v = try f.newValue(.static_call, @enumFromInt(0), b, .{});
    try b.addValue(allocator, call_v);

    try lower(&f);

    try testing.expectEqual(Op.wasm_lowered_static_call, b.values.items[0].op);
}

test "phi and arg unchanged" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "control");
    defer f.deinit();

    const b = try f.newBlock(.first);

    const arg_v = try f.newValue(.arg, @enumFromInt(0), b, .{});
    arg_v.aux_int = 0;
    try b.addValue(allocator, arg_v);

    const phi_v = try f.newValue(.phi, @enumFromInt(0), b, .{});
    try b.addValue(allocator, phi_v);

    try lower(&f);

    try testing.expectEqual(Op.arg, b.values.items[0].op);
    try testing.expectEqual(Op.phi, b.values.items[1].op);
}

test "wasm ops remain unchanged" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "already_wasm");
    defer f.deinit();

    const b = try f.newBlock(.first);

    const v = try f.newValue(.wasm_i64_add, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v);

    try lower(&f);

    try testing.expectEqual(Op.wasm_i64_add, b.values.items[0].op);
}
