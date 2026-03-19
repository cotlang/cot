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

// GlobalValue support for source map data symbols
const globalvalue_mod = @import("../../ir/clif/globalvalue.zig");
const gv_ExternalName = globalvalue_mod.ExternalName;
const GlobalValueData = globalvalue_mod.GlobalValueData;

// macOS ARM64 Dl_info struct offsets (from <dlfcn.h>)
// typedef struct { const char *dli_fname; void *dli_fbase; const char *dli_sname; void *dli_saddr; } Dl_info;
const DL_INFO_SIZE: i32 = 32;
const DLI_SNAME_OFFSET: i32 = 16; // const char *dli_sname
const DLI_SADDR_OFFSET: i32 = 24; // void *dli_saddr

/// Generate signal handler runtime functions.
/// Returns five functions:
///   1. __cot_print_hex(value) — stack-based hex printer (safe during heap corruption)
///   2. __cot_signal_handler(sig, info, ucontext) — Go-style crash diagnostics
///   3. __cot_install_signals() — sigaction with SA_SIGINFO | SA_ONSTACK
///   4. __cot_print_backtrace() — libc backtrace() + backtrace_symbols_fd()
///   5. __cot_print_source_loc(pc) — resolve PC to file:line via runtime source map
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    target_os: target_mod.Os,
    pctab_idx: u32,
    functab_idx: u32,
    functab_count_idx: u32,
    filetab_idx: u32,
    funcnames_idx: u32,
) !std.ArrayListUnmanaged(RuntimeFunc) {
    _ = funcnames_idx; // reserved for future name lookup
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

    try result.append(allocator, .{
        .name = "__cot_print_source_loc",
        .compiled = try generatePrintSourceLoc(allocator, isa, ctrl_plane, func_index_map, pctab_idx, functab_idx, functab_count_idx, filetab_idx),
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

            // ---- Step 3.5: Source location for crash PC ----
            // Call __cot_print_source_loc(pc) to print "  at file.cot:42\n"
            {
                const srcloc_idx = func_index_map.get("__cot_print_source_loc") orelse 0;
                var srcloc_sig = clif.Signature.init(.system_v);
                try srcloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
                const srcloc_sig_ref = try builder.importSignature(srcloc_sig);
                const srcloc_func = try builder.importFunction(.{
                    .name = .{ .user = .{ .namespace = 0, .index = srcloc_idx } },
                    .signature = srcloc_sig_ref,
                    .colocated = true,
                });
                // Reload PC from regs
                const crash_pc = try bi.load(clif.Type.I64, clif.MemFlags.DEFAULT, regs_base, PC_OFFSET);
                _ = try bi.call(srcloc_func, &[_]clif.Value{crash_pc});
            }

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

// ============================================================================
// __cot_print_source_loc(pc: i64) -> void
//
// Resolves a program counter to source file:line using Go-style pctab.
//
// Algorithm (reference: Go runtime/symtab.go pcvalue/step/readvarint):
// 1. dladdr(pc) → dli_sname (function name), dli_saddr (function base)
// 2. offset_in_func = pc - dli_saddr
// 3. FNV-1a hash of dli_sname → target_hash
// 4. Linear scan _cot_functab for entry where name_hash == target_hash
// 5. Decode pctab varint stream at _cot_pctab[pctab_off]:
//    - Read value delta (unsigned varint), zigzag decode → line delta
//    - Read PC delta (unsigned varint) → accumulate cur_pc
//    - If cur_pc > offset_in_func → found line
// 6. Print "  at <filename>:<line>\n"
// ============================================================================
fn generatePrintSourceLoc(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    pctab_idx: u32,
    functab_idx: u32,
    functab_count_idx: u32,
    filetab_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // pc

    // Stack slot for Dl_info struct (32 bytes, 8-byte aligned)
    const dl_info_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = @intCast(DL_INFO_SIZE),
        .align_shift = 3,
    });

    // Pre-create ALL blocks before emitting any instructions
    const b_entry = try builder.createBlock();
    const b_dladdr_ok = try builder.createBlock();

    // FNV hash loop: [ptr, hash, offset]
    const b_hash_loop = try builder.createBlock();
    _ = try builder.appendBlockParam(b_hash_loop, clif.Type.I64); // char_ptr
    _ = try builder.appendBlockParam(b_hash_loop, clif.Type.I64); // hash (I64 for consistency)
    _ = try builder.appendBlockParam(b_hash_loop, clif.Type.I64); // target_offset

    const b_hash_step = try builder.createBlock();
    _ = try builder.appendBlockParam(b_hash_step, clif.Type.I64); // char_ptr
    _ = try builder.appendBlockParam(b_hash_step, clif.Type.I64); // hash
    _ = try builder.appendBlockParam(b_hash_step, clif.Type.I64); // target_offset

    // Hash done → start functab scan: [hash, offset]
    const b_hash_done = try builder.createBlock();
    _ = try builder.appendBlockParam(b_hash_done, clif.Type.I64); // final_hash
    _ = try builder.appendBlockParam(b_hash_done, clif.Type.I64); // target_offset

    // Functab scan: [cur, end, hash, offset]
    const b_ft_scan = try builder.createBlock();
    _ = try builder.appendBlockParam(b_ft_scan, clif.Type.I64); // cur_ptr
    _ = try builder.appendBlockParam(b_ft_scan, clif.Type.I64); // end_ptr
    _ = try builder.appendBlockParam(b_ft_scan, clif.Type.I64); // target_hash
    _ = try builder.appendBlockParam(b_ft_scan, clif.Type.I64); // target_offset

    const b_ft_check = try builder.createBlock();
    _ = try builder.appendBlockParam(b_ft_check, clif.Type.I64); // cur_ptr
    _ = try builder.appendBlockParam(b_ft_check, clif.Type.I64); // end_ptr
    _ = try builder.appendBlockParam(b_ft_check, clif.Type.I64); // target_hash
    _ = try builder.appendBlockParam(b_ft_check, clif.Type.I64); // target_offset

    // Found functab entry → start pctab decode: [pctab_off, offset]
    const b_ft_found = try builder.createBlock();
    _ = try builder.appendBlockParam(b_ft_found, clif.Type.I64); // pctab_off
    _ = try builder.appendBlockParam(b_ft_found, clif.Type.I64); // target_offset

    // Pctab walk step: [ptr, cur_pc, cur_line, target_off, first]
    const b_pc_step = try builder.createBlock();
    _ = try builder.appendBlockParam(b_pc_step, clif.Type.I64); // ptr into pctab
    _ = try builder.appendBlockParam(b_pc_step, clif.Type.I64); // cur_pc
    _ = try builder.appendBlockParam(b_pc_step, clif.Type.I64); // cur_line
    _ = try builder.appendBlockParam(b_pc_step, clif.Type.I64); // target_off
    _ = try builder.appendBlockParam(b_pc_step, clif.Type.I64); // first (1=first iter, 0=not)

    // readvarint for value delta: [ptr, accum, shift, cur_pc, cur_line, target_off, first]
    const b_rv1 = try builder.createBlock();
    _ = try builder.appendBlockParam(b_rv1, clif.Type.I64); // ptr
    _ = try builder.appendBlockParam(b_rv1, clif.Type.I64); // accum
    _ = try builder.appendBlockParam(b_rv1, clif.Type.I64); // shift
    _ = try builder.appendBlockParam(b_rv1, clif.Type.I64); // cur_pc
    _ = try builder.appendBlockParam(b_rv1, clif.Type.I64); // cur_line
    _ = try builder.appendBlockParam(b_rv1, clif.Type.I64); // target_off
    _ = try builder.appendBlockParam(b_rv1, clif.Type.I64); // first

    // Value decoded: [ptr, raw_val, cur_pc, cur_line, target_off, first]
    const b_rv1_done = try builder.createBlock();
    _ = try builder.appendBlockParam(b_rv1_done, clif.Type.I64); // ptr (advanced past varint)
    _ = try builder.appendBlockParam(b_rv1_done, clif.Type.I64); // raw_val
    _ = try builder.appendBlockParam(b_rv1_done, clif.Type.I64); // cur_pc
    _ = try builder.appendBlockParam(b_rv1_done, clif.Type.I64); // cur_line
    _ = try builder.appendBlockParam(b_rv1_done, clif.Type.I64); // target_off
    _ = try builder.appendBlockParam(b_rv1_done, clif.Type.I64); // first

    // readvarint for PC delta: [ptr, accum, shift, cur_pc, new_line, target_off]
    const b_rv2 = try builder.createBlock();
    _ = try builder.appendBlockParam(b_rv2, clif.Type.I64); // ptr
    _ = try builder.appendBlockParam(b_rv2, clif.Type.I64); // accum
    _ = try builder.appendBlockParam(b_rv2, clif.Type.I64); // shift
    _ = try builder.appendBlockParam(b_rv2, clif.Type.I64); // cur_pc
    _ = try builder.appendBlockParam(b_rv2, clif.Type.I64); // new_line
    _ = try builder.appendBlockParam(b_rv2, clif.Type.I64); // target_off

    // PC decoded, check target: [ptr, pc_delta, cur_pc, new_line, target_off]
    const b_rv2_done = try builder.createBlock();
    _ = try builder.appendBlockParam(b_rv2_done, clif.Type.I64); // ptr
    _ = try builder.appendBlockParam(b_rv2_done, clif.Type.I64); // pc_delta
    _ = try builder.appendBlockParam(b_rv2_done, clif.Type.I64); // cur_pc
    _ = try builder.appendBlockParam(b_rv2_done, clif.Type.I64); // new_line
    _ = try builder.appendBlockParam(b_rv2_done, clif.Type.I64); // target_off

    // Found line → print: [line_num]
    const b_found = try builder.createBlock();
    _ = try builder.appendBlockParam(b_found, clif.Type.I64); // line_number

    const b_return = try builder.createBlock();

    // Import dladdr
    const dladdr_idx = func_index_map.get("dladdr") orelse 0;
    var dladdr_sig = clif.Signature.init(.system_v);
    try dladdr_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try dladdr_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try dladdr_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const dladdr_sig_ref = try builder.importSignature(dladdr_sig);
    const dladdr_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = dladdr_idx } },
        .signature = dladdr_sig_ref,
        .colocated = false,
    });
    const write_fn = try importWrite(allocator, &builder, func_index_map);

    // Global values for pctab/functab data symbols
    const gv_pctab = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{ .name = gv_ExternalName.initUser(0, pctab_idx), .offset = 0, .colocated = true, .tls = false },
    });
    const gv_functab = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{ .name = gv_ExternalName.initUser(0, functab_idx), .offset = 0, .colocated = true, .tls = false },
    });
    const gv_functab_count = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{ .name = gv_ExternalName.initUser(0, functab_count_idx), .offset = 0, .colocated = true, .tls = false },
    });
    const gv_filetab = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{ .name = gv_ExternalName.initUser(0, filetab_idx), .offset = 0, .colocated = true, .tls = false },
    });

    // === b_entry: call dladdr(pc, &dl_info), check result ===
    builder.switchToBlock(b_entry);
    try builder.appendBlockParamsForFunctionParams(b_entry);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const pc_val = builder.blockParams(b_entry)[0];
        const dl_addr = try i.stackAddr(clif.Type.I64, dl_info_slot, 0);
        const dl_result = try i.call(dladdr_func, &[_]clif.Value{ pc_val, dl_addr });
        const dl_ok = dl_result.results[0];
        const v0 = try i.iconst(clif.Type.I64, 0);
        const failed = try i.icmp(.eq, dl_ok, v0);
        _ = try i.brif(failed, b_return, &.{}, b_dladdr_ok, &.{});
    }

    // === b_dladdr_ok: extract dli_sname + dli_saddr, compute offset, start hash ===
    builder.switchToBlock(b_dladdr_ok);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const pc_val = builder.blockParams(b_entry)[0];
        const dl_addr = try i.stackAddr(clif.Type.I64, dl_info_slot, 0);
        const dli_sname = try i.load(clif.Type.I64, clif.MemFlags.DEFAULT, dl_addr, DLI_SNAME_OFFSET);
        const dli_saddr = try i.load(clif.Type.I64, clif.MemFlags.DEFAULT, dl_addr, DLI_SADDR_OFFSET);
        const v0 = try i.iconst(clif.Type.I64, 0);
        const sname_null = try i.icmp(.eq, dli_sname, v0);
        const target_offset = try i.isub(pc_val, dli_saddr); // I64
        const fnv_basis = try i.iconst(clif.Type.I64, @as(i64, 0x811c9dc5));
        // On macOS, dladdr symbol names have a '_' prefix — skip it to match func_names hash.
        // Check if first byte is '_' and advance pointer by 1 if so.
        const first_byte = try i.load(clif.Type.I8, clif.MemFlags.DEFAULT, dli_sname, 0);
        const first_i64 = try i.uextend(clif.Type.I64, first_byte);
        const v_underscore = try i.iconst(clif.Type.I64, '_');
        const is_underscore = try i.icmp(.eq, first_i64, v_underscore);
        const v1 = try i.iconst(clif.Type.I64, 1);
        const skip_amt = try i.select(clif.Type.I64, is_underscore, v1, v0);
        const hash_start = try i.iadd(dli_sname, skip_amt);
        _ = try i.brif(sname_null, b_return, &.{}, b_hash_loop, &[_]clif.Value{ hash_start, fnv_basis, target_offset });
    }

    // === b_hash_loop: check for null terminator ===
    builder.switchToBlock(b_hash_loop);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const params = builder.blockParams(b_hash_loop);
        const ptr = params[0];
        const hash = params[1];
        const tgt_off = params[2];
        const byte = try i.load(clif.Type.I8, clif.MemFlags.DEFAULT, ptr, 0);
        const byte_i64 = try i.uextend(clif.Type.I64, byte);
        const v0 = try i.iconst(clif.Type.I64, 0);
        const is_null = try i.icmp(.eq, byte_i64, v0);
        _ = try i.brif(is_null, b_hash_done, &[_]clif.Value{ hash, tgt_off }, b_hash_step, &[_]clif.Value{ ptr, hash, tgt_off });
    }

    // === b_hash_step: h = (h ^ c) * 0x01000193; ptr++ ===
    builder.switchToBlock(b_hash_step);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const params = builder.blockParams(b_hash_step);
        const ptr = params[0];
        const hash = params[1];
        const tgt_off = params[2];
        const byte = try i.load(clif.Type.I8, clif.MemFlags.DEFAULT, ptr, 0);
        const byte_i64 = try i.uextend(clif.Type.I64, byte);
        const h_xor = try i.bxor(hash, byte_i64);
        // Mask to 32 bits before multiply to match FNV-1a u32 behavior
        const v_mask = try i.iconst(clif.Type.I64, 0xFFFFFFFF);
        const h_xor_masked = try i.band(h_xor, v_mask);
        const v_prime = try i.iconst(clif.Type.I64, 0x01000193);
        const h_mul = try i.imul(h_xor_masked, v_prime);
        const h_new = try i.band(h_mul, v_mask);
        const v1 = try i.iconst(clif.Type.I64, 1);
        const ptr_next = try i.iadd(ptr, v1);
        _ = try i.jump(b_hash_loop, &[_]clif.Value{ ptr_next, h_new, tgt_off });
    }

    // === b_hash_done: set up functab scan ===
    builder.switchToBlock(b_hash_done);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const params = builder.blockParams(b_hash_done);
        const target_hash = params[0];
        const target_offset = params[1];
        const count_addr = try i.globalValue(clif.Type.I64, gv_functab_count);
        const count_i32 = try i.load(clif.Type.I32, clif.MemFlags.DEFAULT, count_addr, 0);
        const v0_i32 = try i.iconst(clif.Type.I32, 0);
        const no_entries = try i.icmp(.eq, count_i32, v0_i32);
        const functab_base = try i.globalValue(clif.Type.I64, gv_functab);
        const count_i64 = try i.uextend(clif.Type.I64, count_i32);
        const v16 = try i.iconst(clif.Type.I64, 16);
        const total = try i.imul(count_i64, v16);
        const end_ptr = try i.iadd(functab_base, total);
        _ = try i.brif(no_entries, b_return, &.{}, b_ft_scan, &[_]clif.Value{ functab_base, end_ptr, target_hash, target_offset });
    }

    // === b_ft_scan: check bounds ===
    builder.switchToBlock(b_ft_scan);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_ft_scan);
        // Save block params to locals before emitting instructions (pool may reallocate)
        const ft_cur = p[0];
        const ft_end = p[1];
        const ft_hash = p[2];
        const ft_off = p[3];
        const past_end = try i.icmp(.uge, ft_cur, ft_end);
        _ = try i.brif(past_end, b_return, &.{}, b_ft_check, &[_]clif.Value{ ft_cur, ft_end, ft_hash, ft_off });
    }

    // === b_ft_check: compare hash ===
    builder.switchToBlock(b_ft_check);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_ft_check);
        // Save block params to locals before emitting instructions (pool may reallocate)
        const ftc_cur = p[0];
        const ftc_end = p[1];
        const ftc_hash = p[2];
        const ftc_off = p[3];
        // Load name_hash (u32) from functab entry at offset 0
        const entry_hash_i32 = try i.load(clif.Type.I32, clif.MemFlags.DEFAULT, ftc_cur, 0);
        const entry_hash = try i.uextend(clif.Type.I64, entry_hash_i32);
        const hash_match = try i.icmp(.eq, entry_hash, ftc_hash);

        // If match, load pctab_off and jump to b_ft_found
        // If not, advance to next entry
        const b_ft_next = try builder.createBlock();
        _ = try builder.appendBlockParam(b_ft_next, clif.Type.I64);
        _ = try builder.appendBlockParam(b_ft_next, clif.Type.I64);
        _ = try builder.appendBlockParam(b_ft_next, clif.Type.I64);
        _ = try builder.appendBlockParam(b_ft_next, clif.Type.I64);

        const b_ft_match = try builder.createBlock();
        _ = try builder.appendBlockParam(b_ft_match, clif.Type.I64);
        _ = try builder.appendBlockParam(b_ft_match, clif.Type.I64);

        // Load pctab_off (u32) from functab entry at offset 4
        const pctab_off_i32 = try i.load(clif.Type.I32, clif.MemFlags.DEFAULT, ftc_cur, 4);
        const pctab_off = try i.uextend(clif.Type.I64, pctab_off_i32);

        _ = try i.brif(hash_match, b_ft_match, &[_]clif.Value{ pctab_off, ftc_off }, b_ft_next, &[_]clif.Value{ ftc_cur, ftc_end, ftc_hash, ftc_off });

        // b_ft_match: jump to b_ft_found
        builder.switchToBlock(b_ft_match);
        try builder.ensureInsertedBlock();
        {
            const mi = builder.ins();
            const mp = builder.blockParams(b_ft_match);
            const m0 = mp[0];
            const m1 = mp[1];
            _ = try mi.jump(b_ft_found, &[_]clif.Value{ m0, m1 });
        }

        // b_ft_next: advance by 16 bytes
        builder.switchToBlock(b_ft_next);
        try builder.ensureInsertedBlock();
        {
            const ni = builder.ins();
            const np = builder.blockParams(b_ft_next);
            const n0 = np[0];
            const n1 = np[1];
            const n2 = np[2];
            const n3 = np[3];
            const v16 = try ni.iconst(clif.Type.I64, 16);
            const next = try ni.iadd(n0, v16);
            _ = try ni.jump(b_ft_scan, &[_]clif.Value{ next, n1, n2, n3 });
        }
    }

    // === b_ft_found: start pctab decode ===
    builder.switchToBlock(b_ft_found);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_ft_found);
        const pctab_off = p[0];
        const target_off = p[1];
        // Compute pctab pointer: _cot_pctab + pctab_off
        const pctab_base = try i.globalValue(clif.Type.I64, gv_pctab);
        const pctab_ptr = try i.iadd(pctab_base, pctab_off);
        // Initial values: cur_pc=0, cur_line=-1 (Go pcln.go line 37), first=1
        const v0 = try i.iconst(clif.Type.I64, 0);
        // Store -1 as i64 for cur_line (will be interpreted as signed)
        const v_neg1 = try i.iconst(clif.Type.I64, -1);
        const v1 = try i.iconst(clif.Type.I64, 1);
        _ = try i.jump(b_pc_step, &[_]clif.Value{ pctab_ptr, v0, v_neg1, target_off, v1 });
    }

    // === b_pc_step: start reading value delta varint ===
    builder.switchToBlock(b_pc_step);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_pc_step);
        // Save block params to locals before emitting instructions (pool may reallocate)
        const ps_ptr = p[0];
        const ps_cur_pc = p[1];
        const ps_cur_line = p[2];
        const ps_target_off = p[3];
        const ps_first = p[4];
        const v0 = try i.iconst(clif.Type.I64, 0);
        // Start readvarint: accum=0, shift=0
        _ = try i.jump(b_rv1, &[_]clif.Value{ ps_ptr, v0, v0, ps_cur_pc, ps_cur_line, ps_target_off, ps_first });
    }

    // === b_rv1: readvarint loop for value delta ===
    // Go runtime/symtab.go readvarint: load byte, accum |= (b&0x7F)<<shift, if b&0x80==0 break
    builder.switchToBlock(b_rv1);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_rv1);
        // Save block params to locals before emitting instructions (pool may reallocate)
        const rv1_ptr = p[0];
        const rv1_accum = p[1];
        const rv1_shift = p[2];
        const rv1_cur_pc = p[3];
        const rv1_cur_line = p[4];
        const rv1_target_off = p[5];
        const rv1_first = p[6];
        const byte = try i.load(clif.Type.I8, clif.MemFlags.DEFAULT, rv1_ptr, 0);
        const byte_i64 = try i.uextend(clif.Type.I64, byte);
        const v_0x7f = try i.iconst(clif.Type.I64, 0x7F);
        const data_bits = try i.band(byte_i64, v_0x7f);
        // shifted = data_bits << (shift & 31)
        const v_31 = try i.iconst(clif.Type.I64, 31);
        const shift_masked = try i.band(rv1_shift, v_31);
        const shifted = try i.ishl(data_bits, shift_masked);
        const new_accum = try i.bor(rv1_accum, shifted);
        // has_more = (byte & 0x80) != 0
        const v_0x80 = try i.iconst(clif.Type.I64, 0x80);
        const hi_bit = try i.band(byte_i64, v_0x80);
        const v0 = try i.iconst(clif.Type.I64, 0);
        const has_more = try i.icmp(.ne, hi_bit, v0);
        // ptr + 1
        const v1 = try i.iconst(clif.Type.I64, 1);
        const ptr_next = try i.iadd(rv1_ptr, v1);
        // shift + 7
        const v7 = try i.iconst(clif.Type.I64, 7);
        const new_shift = try i.iadd(rv1_shift, v7);
        _ = try i.brif(has_more, b_rv1, &[_]clif.Value{ ptr_next, new_accum, new_shift, rv1_cur_pc, rv1_cur_line, rv1_target_off, rv1_first }, b_rv1_done, &[_]clif.Value{ ptr_next, new_accum, rv1_cur_pc, rv1_cur_line, rv1_target_off, rv1_first });
    }

    // === b_rv1_done: value varint decoded ===
    // Check terminator: raw_value == 0 && !first → done (no match found)
    // Zigzag decode: delta = -(raw & 1) ^ (raw >> 1)
    // new_line = cur_line + delta
    builder.switchToBlock(b_rv1_done);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_rv1_done);
        // Save block params to locals before emitting instructions (pool may reallocate)
        const rd_ptr = p[0];
        const rd_raw_val = p[1];
        const rd_cur_pc = p[2];
        const rd_cur_line = p[3];
        const rd_target_off = p[4];
        const rd_first = p[5];

        // Check terminator: raw_val == 0 && first == 0
        const v0 = try i.iconst(clif.Type.I64, 0);
        const is_zero = try i.icmp(.eq, rd_raw_val, v0);
        const not_first = try i.icmp(.eq, rd_first, v0);
        // If zero and not first → stream terminated, no match
        const is_term = try i.band(is_zero, not_first);
        const b_rv1_continue = try builder.createBlock();
        _ = try builder.appendBlockParam(b_rv1_continue, clif.Type.I64); // ptr
        _ = try builder.appendBlockParam(b_rv1_continue, clif.Type.I64); // raw_val
        _ = try builder.appendBlockParam(b_rv1_continue, clif.Type.I64); // cur_pc
        _ = try builder.appendBlockParam(b_rv1_continue, clif.Type.I64); // cur_line
        _ = try builder.appendBlockParam(b_rv1_continue, clif.Type.I64); // target_off
        _ = try i.brif(is_term, b_return, &.{}, b_rv1_continue, &[_]clif.Value{ rd_ptr, rd_raw_val, rd_cur_pc, rd_cur_line, rd_target_off });

        // b_rv1_continue: zigzag decode and start reading PC delta
        builder.switchToBlock(b_rv1_continue);
        try builder.ensureInsertedBlock();
        {
            const ci = builder.ins();
            const cp = builder.blockParams(b_rv1_continue);
            // Save block params to locals before emitting instructions (pool may reallocate)
            const c_ptr = cp[0];
            const c_raw_val = cp[1];
            const c_cur_pc = cp[2];
            const c_cur_line = cp[3];
            const c_target_off = cp[4];
            // Zigzag decode: delta = -(raw & 1) ^ (raw >> 1)
            // In Go: val = int32(-(v & 1) ^ (v >> 1))
            const v1 = try ci.iconst(clif.Type.I64, 1);
            const raw_and_1 = try ci.band(c_raw_val, v1);
            const neg_bit = try ci.ineg(raw_and_1); // -(raw & 1)
            const raw_shr_1 = try ci.ushr(c_raw_val, v1); // raw >> 1
            const delta = try ci.bxor(neg_bit, raw_shr_1);
            // new_line = cur_line + delta
            const new_line = try ci.iadd(c_cur_line, delta);
            // Start readvarint for PC delta: accum=0, shift=0
            const v0_2 = try ci.iconst(clif.Type.I64, 0);
            _ = try ci.jump(b_rv2, &[_]clif.Value{ c_ptr, v0_2, v0_2, c_cur_pc, new_line, c_target_off });
        }
    }

    // === b_rv2: readvarint loop for PC delta ===
    builder.switchToBlock(b_rv2);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_rv2);
        // Save block params to locals before emitting instructions (pool may reallocate)
        const rv2_ptr = p[0];
        const rv2_accum = p[1];
        const rv2_shift = p[2];
        const rv2_cur_pc = p[3];
        const rv2_new_line = p[4];
        const rv2_target_off = p[5];
        const byte = try i.load(clif.Type.I8, clif.MemFlags.DEFAULT, rv2_ptr, 0);
        const byte_i64 = try i.uextend(clif.Type.I64, byte);
        const v_0x7f = try i.iconst(clif.Type.I64, 0x7F);
        const data_bits = try i.band(byte_i64, v_0x7f);
        const v_31 = try i.iconst(clif.Type.I64, 31);
        const shift_masked = try i.band(rv2_shift, v_31);
        const shifted = try i.ishl(data_bits, shift_masked);
        const new_accum = try i.bor(rv2_accum, shifted);
        const v_0x80 = try i.iconst(clif.Type.I64, 0x80);
        const hi_bit = try i.band(byte_i64, v_0x80);
        const v0 = try i.iconst(clif.Type.I64, 0);
        const has_more = try i.icmp(.ne, hi_bit, v0);
        const v1 = try i.iconst(clif.Type.I64, 1);
        const ptr_next = try i.iadd(rv2_ptr, v1);
        const v7 = try i.iconst(clif.Type.I64, 7);
        const new_shift = try i.iadd(rv2_shift, v7);
        _ = try i.brif(has_more, b_rv2, &[_]clif.Value{ ptr_next, new_accum, new_shift, rv2_cur_pc, rv2_new_line, rv2_target_off }, b_rv2_done, &[_]clif.Value{ ptr_next, new_accum, rv2_cur_pc, rv2_new_line, rv2_target_off });
    }

    // === b_rv2_done: PC delta decoded, check if cur_pc > target_off ===
    builder.switchToBlock(b_rv2_done);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(b_rv2_done);
        // Save block params to locals before emitting instructions (pool may reallocate)
        const r2d_ptr = p[0];
        const r2d_pc_delta = p[1];
        const r2d_cur_pc = p[2];
        const r2d_new_line = p[3];
        const r2d_target_off = p[4];
        // cur_pc += pc_delta
        const new_pc = try i.iadd(r2d_cur_pc, r2d_pc_delta);
        // If new_pc > target_off → found
        const found_it = try i.icmp(.ugt, new_pc, r2d_target_off);
        const v0 = try i.iconst(clif.Type.I64, 0);
        _ = try i.brif(found_it, b_found, &[_]clif.Value{r2d_new_line}, b_pc_step, &[_]clif.Value{ r2d_ptr, new_pc, r2d_new_line, r2d_target_off, v0 });
    }

    // === b_found: print "  at <file>:<line>\n" ===
    builder.switchToBlock(b_found);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const line_num = builder.blockParams(b_found)[0]; // I64

        try writeStringToStderr(i, &builder, &clif_func, allocator, write_fn, "  at ");

        // Print file name via strlen + write
        const file_addr = try i.globalValue(clif.Type.I64, gv_filetab);
        const strlen_idx = func_index_map.get("strlen") orelse 0;
        var strlen_sig = clif.Signature.init(.system_v);
        try strlen_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try strlen_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const strlen_sig_ref = try builder.importSignature(strlen_sig);
        const strlen_func = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = strlen_idx } },
            .signature = strlen_sig_ref,
            .colocated = false,
        });
        const file_len_result = try i.call(strlen_func, &[_]clif.Value{file_addr});
        const file_len = file_len_result.results[0];
        const v_fd = try i.iconst(clif.Type.I64, 2);
        _ = try i.call(write_fn, &[_]clif.Value{ v_fd, file_addr, file_len });

        try writeStringToStderr(i, &builder, &clif_func, allocator, write_fn, ":");

        // Print line number as decimal (unrolled, up to 5 digits)
        const dec_slot = try clif_func.createStackSlot(allocator, .{ .kind = .explicit_slot, .size = 8, .align_shift = 0 });
        const dec_buf = try i.stackAddr(clif.Type.I64, dec_slot, 0);
        const v_10 = try i.iconst(clif.Type.I64, 10);
        const v_100 = try i.iconst(clif.Type.I64, 100);
        const v_1000 = try i.iconst(clif.Type.I64, 1000);
        const v_10000 = try i.iconst(clif.Type.I64, 10000);
        const v_0x30 = try i.iconst(clif.Type.I64, '0');
        const v_0_i64 = try i.iconst(clif.Type.I64, 0);
        const v_1 = try i.iconst(clif.Type.I64, 1);
        const d4 = try i.udiv(line_num, v_10000);
        const r4 = try i.urem(line_num, v_10000);
        const d3 = try i.udiv(r4, v_1000);
        const r3 = try i.urem(r4, v_1000);
        const d2 = try i.udiv(r3, v_100);
        const r2 = try i.urem(r3, v_100);
        const d1 = try i.udiv(r2, v_10);
        const d0 = try i.urem(r2, v_10);
        _ = try i.store(clif.MemFlags.DEFAULT, try i.ireduce(clif.Type.I8, try i.iadd(d4, v_0x30)), dec_buf, 0);
        _ = try i.store(clif.MemFlags.DEFAULT, try i.ireduce(clif.Type.I8, try i.iadd(d3, v_0x30)), dec_buf, 1);
        _ = try i.store(clif.MemFlags.DEFAULT, try i.ireduce(clif.Type.I8, try i.iadd(d2, v_0x30)), dec_buf, 2);
        _ = try i.store(clif.MemFlags.DEFAULT, try i.ireduce(clif.Type.I8, try i.iadd(d1, v_0x30)), dec_buf, 3);
        _ = try i.store(clif.MemFlags.DEFAULT, try i.ireduce(clif.Type.I8, try i.iadd(d0, v_0x30)), dec_buf, 4);
        const has_d4 = try i.icmp(.ne, d4, v_0_i64);
        const has_d3 = try i.icmp(.ne, d3, v_0_i64);
        const has_d2 = try i.icmp(.ne, d2, v_0_i64);
        const has_d1 = try i.icmp(.ne, d1, v_0_i64);
        const s4 = try i.iconst(clif.Type.I64, 4);
        const s3 = try i.select(clif.Type.I64, has_d1, try i.iconst(clif.Type.I64, 3), s4);
        const s2 = try i.select(clif.Type.I64, has_d2, try i.iconst(clif.Type.I64, 2), s3);
        const s1 = try i.select(clif.Type.I64, has_d3, v_1, s2);
        const s0 = try i.select(clif.Type.I64, has_d4, v_0_i64, s1);
        const v_5 = try i.iconst(clif.Type.I64, 5);
        const dec_len = try i.isub(v_5, s0);
        const dec_start = try i.iadd(dec_buf, s0);
        _ = try i.call(write_fn, &[_]clif.Value{ v_fd, dec_start, dec_len });

        try writeStringToStderr(i, &builder, &clif_func, allocator, write_fn, "\n");
        _ = try i.jump(b_return, &.{});
    }

    // === b_return ===
    builder.switchToBlock(b_return);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        _ = try i.return_(&.{});
    }

    try builder.sealAllBlocks();
    builder.finalize();
    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
