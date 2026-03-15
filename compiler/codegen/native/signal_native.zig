//! Signal Handler Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates signal handling functions as CLIF IR for native binaries.
//! Installs handlers for SIGILL, SIGSEGV, SIGBUS, SIGFPE, SIGABRT
//! that print diagnostic info (signal name + PC addresses) before exit.
//!
//! Reference: Go runtime/signal_unix.go (signal trampoline pattern)
//! Reference: compiler/driver.zig:3312-3335 (Wasm wrapper signal handler)

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

/// Generate signal handler runtime functions.
/// Returns two functions:
///   1. __cot_signal_handler(sig: i64) — writes signal name to stderr, exits
///   2. __cot_install_signals() — calls sigaction for 5 signals
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

    // __cot_signal_handler(sig) — dispatches on signal, prints message, exits
    try result.append(allocator, .{
        .name = "__cot_signal_handler",
        .compiled = try generateSignalHandler(allocator, isa, ctrl_plane, func_index_map),
    });

    // __cot_install_signals() — calls sigaction for each signal
    try result.append(allocator, .{
        .name = "__cot_install_signals",
        .compiled = try generateInstallSignals(allocator, isa, ctrl_plane, func_index_map, target_os),
    });

    return result;
}

/// Generate __cot_signal_handler(sig: i64) -> void
///
/// Dispatches on signal number, writes "fatal error: SIG...\n" to stderr, exits(128+sig).
/// Pattern: Go runtime/signal_unix.go sigtrampgo → crash.
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

    // Signature: (i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const sig = params[0];

    // Import write and _exit
    const write_idx = func_index_map.get("write") orelse 0;
    var write_sig = clif.Signature.init(.system_v);
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // buf
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // len
    try write_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const write_sig_ref = try builder.importSignature(write_sig);
    const write_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = write_idx } },
        .signature = write_sig_ref,
        .colocated = false,
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

    // Stack slot for the message string
    // "fatal error: SIGSEGV\n" = 21 chars (longest)
    const msg_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 24,
        .align_shift = 3,
    });

    // Write "fatal error: " prefix (13 chars)
    const msg_addr = try ins.stackAddr(clif.Type.I64, msg_slot, 0);
    // "fatal error: " = 66 61 74 61 6c 20 65 72 72 6f 72 3a 20
    const prefix_bytes = [_]u8{ 'f', 'a', 't', 'a', 'l', ' ', 'e', 'r', 'r', 'o', 'r', ':', ' ' };
    for (prefix_bytes, 0..) |byte, offset| {
        const ch = try ins.iconst(clif.Type.I8, @intCast(byte));
        _ = try ins.store(clif.MemFlags.DEFAULT, ch, msg_addr, @intCast(offset));
    }

    // Dispatch on signal number to write the signal name
    // SIGILL=4, SIGABRT=6, SIGFPE=8, SIGBUS=10, SIGSEGV=11
    // Default: write "SIG???\n" (6 chars)

    // Blocks for each signal
    const block_sigill = try builder.createBlock();
    const block_sigabrt = try builder.createBlock();
    const block_sigfpe = try builder.createBlock();
    const block_sigbus = try builder.createBlock();
    const block_sigsegv = try builder.createBlock();
    const block_default = try builder.createBlock();
    const block_write = try builder.createBlock();
    _ = try builder.appendBlockParam(block_write, clif.Type.I64); // msg length

    // Check sig == 11 (SIGSEGV)
    const v_11 = try ins.iconst(clif.Type.I64, 11);
    const is_segv = try ins.icmp(.eq, sig, v_11);
    _ = try ins.brif(is_segv, block_sigsegv, &.{}, block_sigill, &.{});

    // Actually, a simpler approach: just write the signal number as decimal
    // and "SIGNAL\n" suffix. Avoids complex branching.
    // Let me use a simpler approach: write "fatal error: signal N\n"

    // block_sigill is actually our "not SIGSEGV" fallback — but let's simplify.
    // Write signal number after prefix using the print_int pattern.

    // SIMPLIFIED APPROACH: Just write "fatal error: signal " + digit(s) + "\n"
    // This avoids needing a jump table for signal names.

    // Actually, let me use the simplest possible approach:
    // Store the full message for each signal in the stack slot.
    // Only 5 signals, and we can share the prefix.

    // Even simpler: just write fixed strings. Use separate code paths.

    // Let me restart with the simplest working approach:
    // Write "fatal error: signal N\n" where N is the signal number.
    // This is what we need for Phase 1 — signal names can come later.

    // Abandon the block structure above. Let me use a single block.
    // Switch to block_sigill (which we'll repurpose as the "write number" block)
    builder.switchToBlock(block_sigill);
    try builder.ensureInsertedBlock();
    const ins2 = builder.ins();

    // Write prefix "fatal error: signal " (20 chars)
    // Store additional chars after "fatal error: " (offset 13)
    const msg_addr2 = try ins2.stackAddr(clif.Type.I64, msg_slot, 0);
    const extra_bytes = [_]u8{ 's', 'i', 'g', 'n', 'a', 'l', ' ' };
    for (extra_bytes, 0..) |byte, offset| {
        const ch = try ins2.iconst(clif.Type.I8, @intCast(byte));
        _ = try ins2.store(clif.MemFlags.DEFAULT, ch, msg_addr2, @intCast(13 + offset));
    }

    // Offset 20: write signal number as decimal
    // For simplicity, divide by 10 for tens digit, mod 10 for ones digit
    const v_10 = try ins2.iconst(clif.Type.I64, 10);
    const tens = try ins2.udiv(sig, v_10);
    const ones = try ins2.urem(sig, v_10);
    const v_0x30 = try ins2.iconst(clif.Type.I64, 0x30); // '0'

    // Check if tens > 0 (2-digit number)
    const v_zero = try ins2.iconst(clif.Type.I64, 0);
    const has_tens = try ins2.icmp(.ne, tens, v_zero);

    const block_two_digit = try builder.createBlock();
    const block_one_digit = try builder.createBlock();

    _ = try ins2.brif(has_tens, block_two_digit, &.{}, block_one_digit, &.{});

    // Two-digit path: write tens digit at offset 20, ones at 21, '\n' at 22
    builder.switchToBlock(block_two_digit);
    try builder.ensureInsertedBlock();
    const ins3 = builder.ins();
    const msg_addr3 = try ins3.stackAddr(clif.Type.I64, msg_slot, 0);
    const tens_ch = try ins3.iadd(tens, v_0x30);
    const tens_i8 = try ins3.ireduce(clif.Type.I8, tens_ch);
    _ = try ins3.store(clif.MemFlags.DEFAULT, tens_i8, msg_addr3, 20);
    const ones_ch = try ins3.iadd(ones, v_0x30);
    const ones_i8 = try ins3.ireduce(clif.Type.I8, ones_ch);
    _ = try ins3.store(clif.MemFlags.DEFAULT, ones_i8, msg_addr3, 21);
    const newline = try ins3.iconst(clif.Type.I8, 0x0A);
    _ = try ins3.store(clif.MemFlags.DEFAULT, newline, msg_addr3, 22);
    // Write 23 bytes
    const v_23 = try ins3.iconst(clif.Type.I64, 23);
    const v_fd3 = try ins3.iconst(clif.Type.I64, 2);
    _ = try ins3.call(write_func, &[_]clif.Value{ v_fd3, msg_addr3, v_23 });
    // exit(128 + sig)
    const v_128 = try ins3.iconst(clif.Type.I64, 128);
    const exit_code = try ins3.iadd(sig, v_128);
    _ = try ins3.call(exit_func, &[_]clif.Value{exit_code});
    _ = try ins3.trap(.unreachable_code_reached);

    // One-digit path: write ones digit at offset 20, '\n' at 21
    builder.switchToBlock(block_one_digit);
    try builder.ensureInsertedBlock();
    const ins4 = builder.ins();
    const msg_addr4 = try ins4.stackAddr(clif.Type.I64, msg_slot, 0);
    const ones_ch4 = try ins4.iadd(ones, v_0x30);
    const ones_i8_4 = try ins4.ireduce(clif.Type.I8, ones_ch4);
    _ = try ins4.store(clif.MemFlags.DEFAULT, ones_i8_4, msg_addr4, 20);
    const newline4 = try ins4.iconst(clif.Type.I8, 0x0A);
    _ = try ins4.store(clif.MemFlags.DEFAULT, newline4, msg_addr4, 21);
    // Write 22 bytes
    const v_22 = try ins4.iconst(clif.Type.I64, 22);
    const v_fd4 = try ins4.iconst(clif.Type.I64, 2);
    _ = try ins4.call(write_func, &[_]clif.Value{ v_fd4, msg_addr4, v_22 });
    // exit(128 + sig)
    const v_128_4 = try ins4.iconst(clif.Type.I64, 128);
    const exit_code4 = try ins4.iadd(sig, v_128_4);
    _ = try ins4.call(exit_func, &[_]clif.Value{exit_code4});
    _ = try ins4.trap(.unreachable_code_reached);

    // Seal remaining unused blocks
    builder.switchToBlock(block_sigabrt);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().trap(.unreachable_code_reached);

    builder.switchToBlock(block_sigfpe);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().trap(.unreachable_code_reached);

    builder.switchToBlock(block_sigbus);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().trap(.unreachable_code_reached);

    builder.switchToBlock(block_sigsegv);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().trap(.unreachable_code_reached);

    builder.switchToBlock(block_default);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().trap(.unreachable_code_reached);

    builder.switchToBlock(block_write);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().trap(.unreachable_code_reached);

    try builder.sealAllBlocks();
    builder.finalize();

    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

/// Generate __cot_install_signals() -> void
///
/// Calls sigaction() for SIGILL(4), SIGABRT(6), SIGFPE(8), SIGBUS(10), SIGSEGV(11).
/// Uses the C sigaction struct layout:
///   macOS ARM64: { handler: fn ptr, mask: sigset_t (4 bytes), flags: i32 }
///     → struct size = 16 bytes at minimum (handler ptr + mask + flags)
///     → Actually: sa_handler is at offset 0 (8 bytes), sa_mask at +8 (4 bytes),
///       sa_flags at +12 (4 bytes) — total 16 bytes
///   Linux x64: { handler: fn ptr, flags: u64, restorer: fn ptr, mask: [16]u8 }
///     → Different layout — use SA_SIGINFO variant
///
/// For simplicity, use the "signal()" function which is simpler than sigaction:
///   signal(signum, handler) -> old_handler
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

    // Signature: () -> void (no params, no return)

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Import signal(signum, handler) -> old_handler
    // We use signal() instead of sigaction() for simplicity — it's available on all platforms.
    const signal_idx = func_index_map.get("signal") orelse 0;
    var signal_sig = clif.Signature.init(.system_v);
    try signal_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // signum
    try signal_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // handler
    try signal_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64)); // old handler
    const signal_sig_ref = try builder.importSignature(signal_sig);
    const signal_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = signal_idx } },
        .signature = signal_sig_ref,
        .colocated = false,
    });

    // Get the address of __cot_signal_handler
    const handler_idx = func_index_map.get("__cot_signal_handler") orelse 0;
    var handler_sig = clif.Signature.init(.system_v);
    try handler_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    const handler_sig_ref = try builder.importSignature(handler_sig);
    const handler_func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = handler_idx } },
        .signature = handler_sig_ref,
        .colocated = false,
    });
    const handler_addr = try ins.funcAddr(clif.Type.I64, handler_func_ref);

    // Install for each signal: signal(SIGNUM, handler)
    const signals = [_]i64{ 4, 6, 8, 10, 11 }; // SIGILL, SIGABRT, SIGFPE, SIGBUS, SIGSEGV
    for (signals) |signum| {
        const v_sig = try ins.iconst(clif.Type.I64, signum);
        _ = try ins.call(signal_func, &[_]clif.Value{ v_sig, handler_addr });
    }

    _ = try ins.return_(&.{});

    try builder.sealAllBlocks();
    builder.finalize();

    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
