//! Signal Handler Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates signal handling functions as CLIF IR for native binaries.
//! Installs handlers for SIGILL, SIGSEGV, SIGBUS, SIGFPE, SIGABRT using
//! SA_SIGINFO to get full crash context (faulting address, registers).
//!
//! Output format (Go-style, reference: runtime/signal_unix.go:845-886):
//!   SIGSEGV: segmentation fault
//!   PC=0x100146da8 sigcode=1 addr=0x0000000000000020
//!   x0  0x0000000000000000
//!   x1  0x000000010050a800
//!   ...
//!   [backtrace]
//!
//! Reference: Go runtime/signal_unix.go (fatalsignal, sighandler)
//! Reference: Go runtime/signal_arm64.go (dumpregs)
//! Reference: Go runtime/signal_darwin_arm64.go (sigctxt, regs64 access)
//! Reference: Go runtime/defs_darwin_arm64.go (struct layouts)

const std = @import("std");
const Allocator = std.mem.Allocator;

const clif = @import("../../ir/clif/mod.zig");
const frontend_mod = @import("frontend/mod.zig");
const FunctionBuilder = frontend_mod.FunctionBuilder;
const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;
const native_compile = @import("compile.zig");
const arc_native = @import("arc_native.zig");
const RuntimeFunc = arc_native.RuntimeFunc;

const target_mod = @import("../../frontend/target.zig");

// macOS ARM64 siginfo_t offsets (defs_darwin_arm64.go:136-147)
const SI_CODE_OFFSET: i32 = 8; // si_code: i32
const SI_ADDR_OFFSET: i32 = 24; // si_addr: *byte (i64)

// macOS ARM64 ucontext_t offsets (defs_darwin_arm64.go:194-200)
const UC_MCONTEXT_OFFSET: i32 = 48; // uc_mcontext: *mcontext64 (pointer)

// macOS ARM64 mcontext64 offsets (defs_darwin_arm64.go:189-192)
// mcontext64 = { es: exceptionstate64(16), ss: regs64(272), ns: neonstate64 }
const REGS_OFFSET: i32 = 16; // skip exceptionstate64

// macOS ARM64 regs64 offsets within mcontext.ss (defs_darwin_arm64.go:175-182)
// x[0..28]: 29 × u64 at offset 0..232
// fp (x29): offset 232
// lr (x30): offset 240
// sp (x31): offset 248
// pc:       offset 256
const FP_OFFSET: i32 = 232;
const LR_OFFSET: i32 = 240;
const SP_OFFSET: i32 = 248;
const PC_OFFSET: i32 = 256;

// macOS sigaction flags (defs_darwin_arm64.go:28-31)
const SA_ONSTACK: i32 = 0x1;
const SA_SIGINFO: i32 = 0x40;

/// Generate signal handler runtime functions.
/// Returns four functions:
///   1. __cot_signal_handler(sig, info, ucontext) — Go-style crash diagnostics
///   2. __cot_install_signals() — sigaction with SA_SIGINFO | SA_ONSTACK
///   3. __cot_print_backtrace() — libc backtrace() + backtrace_symbols_fd()
///   4. __cot_print_hex(value) — stack-based hex printer (safe during heap corruption)
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    target_os: target_mod.Os,
) !std.ArrayListUnmanaged(RuntimeFunc) {
    var result = std.ArrayListUnmanaged(RuntimeFunc){};
    errdefer {
        for (result.items) |*rf| rf.compiled.deinit();
        result.deinit(allocator);
    }

    try result.append(allocator, .{
        .name = "__cot_print_hex",
        .compiled = try generatePrintHex(allocator, isa, ctrl_plane, func_index_map),
    });

    try result.append(allocator, .{
        .name = "__cot_signal_handler",
        .compiled = try generateSignalHandler(allocator, isa, ctrl_plane, func_index_map),
    });

    try result.append(allocator, .{
        .name = "__cot_install_signals",
        .compiled = try generateInstallSignals(allocator, isa, ctrl_plane, func_index_map, target_os),
    });

    try result.append(allocator, .{
        .name = "__cot_print_backtrace",
        .compiled = try generatePrintBacktrace(allocator, isa, ctrl_plane, func_index_map),
    });

    return result;
}

/// Helper: import write(fd, buf, len) -> ssize_t
fn importWrite(allocator: Allocator, builder: *FunctionBuilder, func_index_map: *const std.StringHashMapUnmanaged(u32)) !clif.FuncRef {
    const idx = func_index_map.get("write") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    return try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = idx } },
        .signature = sig_ref,
        .colocated = false,
    });
}

/// Helper: write a string literal to stderr using a stack slot
fn writeStringToStderr(
    ins: anytype,
    _: *FunctionBuilder,
    clif_func: *clif.Function,
    allocator: Allocator,
    write_func: clif.FuncRef,
    msg: []const u8,
) !void {
    const slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = @intCast(msg.len),
        .align_shift = 0,
    });
    const addr = try ins.stackAddr(clif.Type.I64, slot, 0);
    for (msg, 0..) |byte, offset| {
        const ch = try ins.iconst(clif.Type.I8, @intCast(byte));
        _ = try ins.store(clif.MemFlags.DEFAULT, ch, addr, @intCast(offset));
    }
    const fd = try ins.iconst(clif.Type.I64, 2);
    const len = try ins.iconst(clif.Type.I64, @intCast(msg.len));
    _ = try ins.call(write_func, &[_]clif.Value{ fd, addr, len });
}

// ============================================================================
// __cot_print_hex(value: i64) -> void
// Stack-based hex printer. No malloc — safe during heap corruption.
// Reference: Go runtime/print.go hex() function.
// ============================================================================
fn generatePrintHex(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const value = builder.blockParams(block_entry)[0];
    const write_func = try importWrite(allocator, &builder, func_index_map);

    // Buffer: "0x" + 16 hex digits = 18 bytes
    const hex_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 18,
        .align_shift = 0,
    });
    const buf = try ins.stackAddr(clif.Type.I64, hex_slot, 0);

    // Store "0x" prefix
    const ch_0 = try ins.iconst(clif.Type.I8, '0');
    _ = try ins.store(clif.MemFlags.DEFAULT, ch_0, buf, 0);
    const ch_x = try ins.iconst(clif.Type.I8, 'x');
    _ = try ins.store(clif.MemFlags.DEFAULT, ch_x, buf, 1);

    // Convert to hex: digit[i] = (value >> (60 - i*4)) & 0xF
    // Unrolled 16 iterations (comptime in Zig, generates 16 shift+mask+store sequences)
    const v_0xf = try ins.iconst(clif.Type.I64, 0xF);
    for (0..16) |i| {
        const shift_amt = @as(i64, @intCast(60 - @as(i32, @intCast(i)) * 4));
        const v_shift = try ins.iconst(clif.Type.I64, shift_amt);
        const shifted = try ins.ushr(value, v_shift);
        const nibble = try ins.band(shifted, v_0xf);

        // nibble < 10 ? '0' + nibble : 'a' + nibble - 10
        const v_10 = try ins.iconst(clif.Type.I64, 10);
        const is_digit = try ins.icmp(.ult, nibble, v_10);
        const v_0 = try ins.iconst(clif.Type.I64, '0');
        const v_a = try ins.iconst(clif.Type.I64, 'a' - 10);
        const base = try ins.select(clif.Type.I64, is_digit, v_0, v_a);
        const ch = try ins.iadd(nibble, base);
        const ch8 = try ins.ireduce(clif.Type.I8, ch);
        _ = try ins.store(clif.MemFlags.DEFAULT, ch8, buf, @intCast(2 + i));
    }

    // write(2, buf, 18)
    const fd = try ins.iconst(clif.Type.I64, 2);
    const len = try ins.iconst(clif.Type.I64, 18);
    _ = try ins.call(write_func, &[_]clif.Value{ fd, buf, len });
    _ = try ins.return_(&.{});

    try builder.sealAllBlocks();
    builder.finalize();
    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// __cot_signal_handler(sig: i64, info: i64, ucontext: i64) -> void
//
// Go-style crash diagnostics. Reference: runtime/signal_unix.go:845-886
// (fatalsignal), runtime/signal_arm64.go:16-51 (dumpregs).
//
// Output:
//   SIGSEGV: segmentation fault
//   PC=0x100146da8 sigcode=1 addr=0x0000000000000020
//   x0  0x...  x1  0x...  x2  0x...  x3  0x...
//   ...
//   [backtrace]
// ============================================================================
fn generateSignalHandler(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // SA_SIGINFO signature: (sig: i64, info: i64, ucontext: i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const sig = params[0];
    const info = params[1];
    const ucontext = params[2];

    // Import functions
    const write_func = try importWrite(allocator, &builder, func_index_map);

    const hex_idx = func_index_map.get("__cot_print_hex") orelse 0;
    var hex_sig = clif.Signature.init(.system_v);
    try hex_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    const hex_sig_ref = try builder.importSignature(hex_sig);
    const hex_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = hex_idx } },
        .signature = hex_sig_ref,
        .colocated = true,
    });

    const bt_idx = func_index_map.get("__cot_print_backtrace") orelse 0;
    const bt_sig = clif.Signature.init(.system_v);
    const bt_sig_ref = try builder.importSignature(bt_sig);
    const bt_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = bt_idx } },
        .signature = bt_sig_ref,
        .colocated = true,
    });

    const exit_idx = func_index_map.get("_exit") orelse 0;
    var exit_sig = clif.Signature.init(.system_v);
    try exit_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    const exit_sig_ref = try builder.importSignature(exit_sig);
    const exit_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = exit_idx } },
        .signature = exit_sig_ref,
        .colocated = false,
    });

    // ---- Step 1: Print signal name ----
    // Go fatalsignal (signal_unix.go:846-850): sigtable[sig].name
    // We use cascading comparisons since we only handle 5 signals.
    const block_sigill = try builder.createBlock();
    const block_sigabrt = try builder.createBlock();
    const block_sigfpe = try builder.createBlock();
    const block_sigbus = try builder.createBlock();
    const block_sigsegv = try builder.createBlock();
    const block_sigother = try builder.createBlock();
    const block_after_name = try builder.createBlock();

    const v_4 = try ins.iconst(clif.Type.I64, 4);
    const is_ill = try ins.icmp(.eq, sig, v_4);
    _ = try ins.brif(is_ill, block_sigill, &.{}, block_sigabrt, &.{});

    // SIGILL
    builder.switchToBlock(block_sigill);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        try writeStringToStderr(i, &builder, &clif_func, allocator, write_func, "SIGILL: illegal instruction\n");
        _ = try i.jump(block_after_name, &.{});
    }

    // SIGABRT check
    builder.switchToBlock(block_sigabrt);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const v_6 = try i.iconst(clif.Type.I64, 6);
        const is_abrt = try i.icmp(.eq, sig, v_6);
        const block_abrt_msg = try builder.createBlock();
        _ = try i.brif(is_abrt, block_abrt_msg, &.{}, block_sigfpe, &.{});

        builder.switchToBlock(block_abrt_msg);
        try builder.ensureInsertedBlock();
        const bi = builder.ins();
        try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "SIGABRT: abort\n");
        _ = try bi.jump(block_after_name, &.{});
    }

    // SIGFPE check
    builder.switchToBlock(block_sigfpe);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const v_8 = try i.iconst(clif.Type.I64, 8);
        const is_fpe = try i.icmp(.eq, sig, v_8);
        const block_fpe_msg = try builder.createBlock();
        _ = try i.brif(is_fpe, block_fpe_msg, &.{}, block_sigbus, &.{});

        builder.switchToBlock(block_fpe_msg);
        try builder.ensureInsertedBlock();
        const bi = builder.ins();
        try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "SIGFPE: floating point exception\n");
        _ = try bi.jump(block_after_name, &.{});
    }

    // SIGBUS check
    builder.switchToBlock(block_sigbus);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const v_10 = try i.iconst(clif.Type.I64, 10);
        const is_bus = try i.icmp(.eq, sig, v_10);
        const block_bus_msg = try builder.createBlock();
        _ = try i.brif(is_bus, block_bus_msg, &.{}, block_sigsegv, &.{});

        builder.switchToBlock(block_bus_msg);
        try builder.ensureInsertedBlock();
        const bi = builder.ins();
        try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "SIGBUS: bus error\n");
        _ = try bi.jump(block_after_name, &.{});
    }

    // SIGSEGV check
    builder.switchToBlock(block_sigsegv);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const v_11 = try i.iconst(clif.Type.I64, 11);
        const is_segv = try i.icmp(.eq, sig, v_11);
        const block_segv_msg = try builder.createBlock();
        _ = try i.brif(is_segv, block_segv_msg, &.{}, block_sigother, &.{});

        builder.switchToBlock(block_segv_msg);
        try builder.ensureInsertedBlock();
        const bi = builder.ins();
        try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "SIGSEGV: segmentation fault\n");
        _ = try bi.jump(block_after_name, &.{});
    }

    // Default: "Signal N\n"
    builder.switchToBlock(block_sigother);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        try writeStringToStderr(i, &builder, &clif_func, allocator, write_func, "Signal ");
        // Print signal number as decimal (1-2 digits)
        const v_10d = try i.iconst(clif.Type.I64, 10);
        const tens = try i.udiv(sig, v_10d);
        const ones = try i.urem(sig, v_10d);
        const v_0x30 = try i.iconst(clif.Type.I64, '0');
        const v_zero = try i.iconst(clif.Type.I64, 0);
        const has_tens = try i.icmp(.ne, tens, v_zero);

        const num_slot = try clif_func.createStackSlot(allocator, .{ .kind = .explicit_slot, .size = 3, .align_shift = 0 });
        const num_addr = try i.stackAddr(clif.Type.I64, num_slot, 0);

        const block_two_digit = try builder.createBlock();
        const block_one_digit = try builder.createBlock();

        _ = try i.brif(has_tens, block_two_digit, &.{}, block_one_digit, &.{});

        builder.switchToBlock(block_two_digit);
        try builder.ensureInsertedBlock();
        {
            const bi = builder.ins();
            const t = try bi.iadd(tens, v_0x30);
            _ = try bi.store(clif.MemFlags.DEFAULT, try bi.ireduce(clif.Type.I8, t), num_addr, 0);
            const o = try bi.iadd(ones, v_0x30);
            _ = try bi.store(clif.MemFlags.DEFAULT, try bi.ireduce(clif.Type.I8, o), num_addr, 1);
            _ = try bi.store(clif.MemFlags.DEFAULT, try bi.iconst(clif.Type.I8, '\n'), num_addr, 2);
            const fd = try bi.iconst(clif.Type.I64, 2);
            _ = try bi.call(write_func, &[_]clif.Value{ fd, num_addr, try bi.iconst(clif.Type.I64, 3) });
            _ = try bi.jump(block_after_name, &.{});
        }

        builder.switchToBlock(block_one_digit);
        try builder.ensureInsertedBlock();
        {
            const bi = builder.ins();
            const o = try bi.iadd(ones, v_0x30);
            _ = try bi.store(clif.MemFlags.DEFAULT, try bi.ireduce(clif.Type.I8, o), num_addr, 0);
            _ = try bi.store(clif.MemFlags.DEFAULT, try bi.iconst(clif.Type.I8, '\n'), num_addr, 1);
            const fd = try bi.iconst(clif.Type.I64, 2);
            _ = try bi.call(write_func, &[_]clif.Value{ fd, num_addr, try bi.iconst(clif.Type.I64, 2) });
            _ = try bi.jump(block_after_name, &.{});
        }
    }

    // ---- Step 2: Print PC + sigcode + addr ----
    // Go fatalsignal (signal_unix.go:855-859):
    //   print("PC=", hex(c.sigpc()), " m=", mp.id, " sigcode=", c.sigcode())
    //   if sig == SIGSEGV || sig == SIGBUS { print(" addr=", hex(c.fault())) }
    builder.switchToBlock(block_after_name);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();

        // Load mcontext pointer from ucontext
        // Go: (*ucontext)(c.ctxt).uc_mcontext.ss (signal_darwin_arm64.go:16)
        const mctx_ptr = try i.load(clif.Type.I64, clif.MemFlags.DEFAULT, ucontext, UC_MCONTEXT_OFFSET);
        // regs_base = mcontext + 16 (skip exceptionstate64)
        const v_regs_off = try i.iconst(clif.Type.I64, REGS_OFFSET);
        const regs_base = try i.iadd(mctx_ptr, v_regs_off);

        // Load PC from regs64 (offset 256 from regs_base)
        const pc = try i.load(clif.Type.I64, clif.MemFlags.DEFAULT, regs_base, PC_OFFSET);

        // Load si_code from siginfo_t
        const si_code_i32 = try i.load(clif.Type.I32, clif.MemFlags.DEFAULT, info, SI_CODE_OFFSET);
        const si_code = try i.sextend(clif.Type.I64, si_code_i32);

        // Load si_addr (fault address) from siginfo_t
        const fault_addr = try i.load(clif.Type.I64, clif.MemFlags.DEFAULT, info, SI_ADDR_OFFSET);

        // Print "PC="
        try writeStringToStderr(i, &builder, &clif_func, allocator, write_func, "PC=");
        _ = try i.call(hex_func, &[_]clif.Value{pc});

        // Print " sigcode="
        try writeStringToStderr(i, &builder, &clif_func, allocator, write_func, " sigcode=");
        _ = try i.call(hex_func, &[_]clif.Value{si_code});

        // For SIGSEGV(11) / SIGBUS(10): print " addr="
        // Go fatalsignal (signal_unix.go:856-858)
        const v_10 = try i.iconst(clif.Type.I64, 10);
        const v_11 = try i.iconst(clif.Type.I64, 11);
        const is_bus = try i.icmp(.eq, sig, v_10);
        const is_segv = try i.icmp(.eq, sig, v_11);
        const needs_addr = try i.bor(is_bus, is_segv);

        const block_print_addr = try builder.createBlock();
        const block_after_addr = try builder.createBlock();
        _ = try i.brif(needs_addr, block_print_addr, &.{}, block_after_addr, &.{});

        builder.switchToBlock(block_print_addr);
        try builder.ensureInsertedBlock();
        {
            const bi = builder.ins();
            try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, " addr=");
            _ = try bi.call(hex_func, &[_]clif.Value{fault_addr});
            _ = try bi.jump(block_after_addr, &.{});
        }

        builder.switchToBlock(block_after_addr);
        try builder.ensureInsertedBlock();
        {
            const bi = builder.ins();
            try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "\n");

            // ---- Step 3: Register dump ----
            // Go dumpregs (signal_arm64.go:16-51): prints x0-x28, fp, lr, sp, pc, fault
            // We print x0-x7 (args), x16 (indirect call), fp, lr, sp, pc, fault
            const reg_names = [_][]const u8{ "x0  ", "x1  ", "x2  ", "x3  ", "x4  ", "x5  ", "x6  ", "x7  ", "x16 " };
            const reg_offsets = [_]i32{ 0, 8, 16, 24, 32, 40, 48, 56, 128 }; // x[i]*8

            for (reg_names, reg_offsets) |name, offset| {
                try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, name);
                const val = try bi.load(clif.Type.I64, clif.MemFlags.DEFAULT, regs_base, offset);
                _ = try bi.call(hex_func, &[_]clif.Value{val});
                try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "\n");
            }

            // fp, lr, sp, pc, fault
            const special_names = [_][]const u8{ "fp  ", "lr  ", "sp  ", "pc  ", "fault " };
            const special_offsets = [_]i32{ FP_OFFSET, LR_OFFSET, SP_OFFSET, PC_OFFSET };

            for (special_names[0..4], special_offsets) |name, offset| {
                try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, name);
                const val = try bi.load(clif.Type.I64, clif.MemFlags.DEFAULT, regs_base, offset);
                _ = try bi.call(hex_func, &[_]clif.Value{val});
                try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "\n");
            }
            // fault address (from siginfo, not regs)
            try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "fault ");
            _ = try bi.call(hex_func, &[_]clif.Value{fault_addr});
            try writeStringToStderr(bi, &builder, &clif_func, allocator, write_func, "\n");

            // ---- Step 4: Backtrace ----
            _ = try bi.call(bt_func, &[_]clif.Value{});

            // ---- Step 5: Exit ----
            const v_128 = try bi.iconst(clif.Type.I64, 128);
            const exit_code = try bi.iadd(sig, v_128);
            _ = try bi.call(exit_func, &[_]clif.Value{exit_code});
            _ = try bi.trap(.unreachable_code_reached);
        }
    }

    try builder.sealAllBlocks();
    builder.finalize();
    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// __cot_install_signals() -> void
//
// Sets up alternate signal stack (sigaltstack) then installs SA_SIGINFO
// signal handlers for SIGILL(4), SIGABRT(6), SIGFPE(8), SIGBUS(10), SIGSEGV(11).
//
// Reference: Go runtime/signal_unix.go:signalstack
// ============================================================================
fn generateInstallSignals(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    target_os: target_mod.Os,
) !native_compile.CompiledCode {
    _ = target_os;

    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // ---- Step 1: Allocate alternate signal stack via malloc ----
    const malloc_idx = func_index_map.get("malloc") orelse func_index_map.get("_malloc") orelse 0;
    var malloc_sig = clif.Signature.init(.system_v);
    try malloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try malloc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const malloc_sig_ref = try builder.importSignature(malloc_sig);
    const malloc_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = malloc_idx } },
        .signature = malloc_sig_ref,
        .colocated = false,
    });

    const SIGSTKSZ: i64 = 65536;
    const v_stksz = try ins.iconst(clif.Type.I64, SIGSTKSZ);
    const malloc_result = try ins.call(malloc_func, &[_]clif.Value{v_stksz});
    const alt_stack_buf = malloc_result.results[0];

    // ---- Step 2: Call sigaltstack ----
    // macOS ARM64 stack_t: { ss_sp(8), ss_size(8), ss_flags(4), pad(4) } = 24 bytes
    // Note: macOS has ss_size BEFORE ss_flags (differs from Linux)
    const ss_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 24,
        .align_shift = 3,
    });
    const ss_addr = try ins.stackAddr(clif.Type.I64, ss_slot, 0);
    _ = try ins.store(clif.MemFlags.DEFAULT, alt_stack_buf, ss_addr, 0); // ss_sp
    _ = try ins.store(clif.MemFlags.DEFAULT, v_stksz, ss_addr, 8); // ss_size
    const v_zero_flags = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.store(clif.MemFlags.DEFAULT, v_zero_flags, ss_addr, 16); // ss_flags = 0

    const sigaltstack_idx = func_index_map.get("sigaltstack") orelse 0;
    var sigaltstack_sig = clif.Signature.init(.system_v);
    try sigaltstack_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sigaltstack_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sigaltstack_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sigaltstack_sig_ref = try builder.importSignature(sigaltstack_sig);
    const sigaltstack_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = sigaltstack_idx } },
        .signature = sigaltstack_sig_ref,
        .colocated = false,
    });
    const v_null = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(sigaltstack_func, &[_]clif.Value{ ss_addr, v_null });

    // ---- Step 3: Install SA_SIGINFO handlers via sigaction ----
    // macOS ARM64 struct __sigaction (defs_darwin_arm64.go:123-128):
    //   __sigaction_u: [8]byte  (handler pointer, offset 0)
    //   sa_tramp:      ptr      (offset 8, set to 0)
    //   sa_mask:       u32      (offset 16)
    //   sa_flags:      i32      (offset 20)
    // Total: 24 bytes
    const sa_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 24,
        .align_shift = 3,
    });

    // Get address of __cot_signal_handler (now 3-arg: sig, info, ucontext)
    const handler_idx = func_index_map.get("__cot_signal_handler") orelse 0;
    var handler_sig = clif.Signature.init(.system_v);
    try handler_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // sig
    try handler_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // info
    try handler_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // ucontext
    const handler_sig_ref = try builder.importSignature(handler_sig);
    const handler_func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = handler_idx } },
        .signature = handler_sig_ref,
        .colocated = false,
    });
    const handler_addr = try ins.funcAddr(clif.Type.I64, handler_func_ref);

    // Import sigaction(signum, act, oldact) -> int
    const sigaction_idx = func_index_map.get("sigaction") orelse 0;
    var sigaction_sig = clif.Signature.init(.system_v);
    try sigaction_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sigaction_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sigaction_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sigaction_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sigaction_sig_ref = try builder.importSignature(sigaction_sig);
    const sigaction_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = sigaction_idx } },
        .signature = sigaction_sig_ref,
        .colocated = false,
    });

    // Fill sigaction struct
    const sa_addr = try ins.stackAddr(clif.Type.I64, sa_slot, 0);
    _ = try ins.store(clif.MemFlags.DEFAULT, handler_addr, sa_addr, 0); // __sigaction_u (handler)
    _ = try ins.store(clif.MemFlags.DEFAULT, v_null, sa_addr, 8); // sa_tramp = null
    const v_zero_mask = try ins.iconst(clif.Type.I32, 0);
    _ = try ins.store(clif.MemFlags.DEFAULT, v_zero_mask, sa_addr, 16); // sa_mask = 0
    const v_flags = try ins.iconst(clif.Type.I32, SA_ONSTACK | SA_SIGINFO);
    _ = try ins.store(clif.MemFlags.DEFAULT, v_flags, sa_addr, 20); // sa_flags

    // Install for each signal
    const signals = [_]i64{ 4, 6, 8, 10, 11 };
    for (signals) |signum| {
        const v_sig = try ins.iconst(clif.Type.I64, signum);
        _ = try ins.call(sigaction_func, &[_]clif.Value{ v_sig, sa_addr, v_null });
    }

    _ = try ins.return_(&.{});

    try builder.sealAllBlocks();
    builder.finalize();
    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// __cot_print_backtrace() -> void
//
// Calls libc backtrace() + backtrace_symbols_fd() to print stack trace.
// Reference: macOS/Linux backtrace(3)
// ============================================================================
fn generatePrintBacktrace(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const bt_idx = func_index_map.get("backtrace") orelse 0;
    var bt_sig = clif.Signature.init(.system_v);
    try bt_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try bt_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try bt_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const bt_sig_ref = try builder.importSignature(bt_sig);
    const bt_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = bt_idx } },
        .signature = bt_sig_ref,
        .colocated = false,
    });

    const btsf_idx = func_index_map.get("backtrace_symbols_fd") orelse 0;
    var btsf_sig = clif.Signature.init(.system_v);
    try btsf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try btsf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try btsf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    const btsf_sig_ref = try builder.importSignature(btsf_sig);
    const btsf_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = btsf_idx } },
        .signature = btsf_sig_ref,
        .colocated = false,
    });

    const bt_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 512,
        .align_shift = 3,
    });

    const ins = builder.ins();
    const bt_buf = try ins.stackAddr(clif.Type.I64, bt_slot, 0);
    const v_64 = try ins.iconst(clif.Type.I64, 64);
    const bt_result = try ins.call(bt_func, &[_]clif.Value{ bt_buf, v_64 });
    const count = bt_result.results[0];

    // Skip first 2 frames (signal handler internals)
    const v_16 = try ins.iconst(clif.Type.I64, 16);
    const adjusted_buf = try ins.iadd(bt_buf, v_16);
    const v_2 = try ins.iconst(clif.Type.I64, 2);
    const adjusted_count = try ins.isub(count, v_2);

    const v_fd = try ins.iconst(clif.Type.I64, 2);
    _ = try ins.call(btsf_func, &[_]clif.Value{ adjusted_buf, adjusted_count, v_fd });

    _ = try ins.return_(&.{});

    try builder.sealAllBlocks();
    builder.finalize();
    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
