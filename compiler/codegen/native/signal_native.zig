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
/// Returns three functions:
///   1. __cot_signal_handler(sig: i64) — writes signal name to stderr, exits
///   2. __cot_install_signals() — calls sigaction for 5 signals
///   3. __cot_print_backtrace() — calls libc backtrace(), prints PCs as hex
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

    // __cot_print_backtrace() — calls libc backtrace() + prints hex PCs
    try result.append(allocator, .{
        .name = "__cot_print_backtrace",
        .compiled = try generatePrintBacktrace(allocator, isa, ctrl_plane, func_index_map),
    });

    return result;
}

/// Generate __cot_signal_handler(sig: i64) -> void
///
/// Writes "fatal error: signal N\n" to stderr, exits(128+sig).
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
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try write_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
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

    // Stack slot for message: "fatal error: signal NN\n" (max 23 bytes)
    const msg_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 24,
        .align_shift = 3,
    });

    // Store "fatal error: signal " prefix (20 chars)
    const msg_addr = try ins.stackAddr(clif.Type.I64, msg_slot, 0);
    const prefix = "fatal error: signal ";
    for (prefix, 0..) |byte, offset| {
        const ch = try ins.iconst(clif.Type.I8, @intCast(byte));
        _ = try ins.store(clif.MemFlags.DEFAULT, ch, msg_addr, @intCast(offset));
    }

    // Convert signal number to 1-2 digit ASCII
    const v_10 = try ins.iconst(clif.Type.I64, 10);
    const tens = try ins.udiv(sig, v_10);
    const ones = try ins.urem(sig, v_10);
    const v_0x30 = try ins.iconst(clif.Type.I64, 0x30);
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const has_tens = try ins.icmp(.ne, tens, v_zero);

    const block_two = try builder.createBlock();
    const block_one = try builder.createBlock();

    _ = try ins.brif(has_tens, block_two, &.{}, block_one, &.{});

    // Two-digit: tens at 20, ones at 21, '\n' at 22, write 23 bytes
    builder.switchToBlock(block_two);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const addr = try i.stackAddr(clif.Type.I64, msg_slot, 0);
        const t_ch = try i.iadd(tens, v_0x30);
        const t_i8 = try i.ireduce(clif.Type.I8, t_ch);
        _ = try i.store(clif.MemFlags.DEFAULT, t_i8, addr, 20);
        const o_ch = try i.iadd(ones, v_0x30);
        const o_i8 = try i.ireduce(clif.Type.I8, o_ch);
        _ = try i.store(clif.MemFlags.DEFAULT, o_i8, addr, 21);
        const nl = try i.iconst(clif.Type.I8, 0x0A);
        _ = try i.store(clif.MemFlags.DEFAULT, nl, addr, 22);
        const fd = try i.iconst(clif.Type.I64, 2);
        const len = try i.iconst(clif.Type.I64, 23);
        _ = try i.call(write_func, &[_]clif.Value{ fd, addr, len });
        const v128 = try i.iconst(clif.Type.I64, 128);
        const ec = try i.iadd(sig, v128);
        _ = try i.call(exit_func, &[_]clif.Value{ec});
        _ = try i.trap(.unreachable_code_reached);
    }

    // One-digit: ones at 20, '\n' at 21, write 22 bytes
    builder.switchToBlock(block_one);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const addr = try i.stackAddr(clif.Type.I64, msg_slot, 0);
        const o_ch = try i.iadd(ones, v_0x30);
        const o_i8 = try i.ireduce(clif.Type.I8, o_ch);
        _ = try i.store(clif.MemFlags.DEFAULT, o_i8, addr, 20);
        const nl = try i.iconst(clif.Type.I8, 0x0A);
        _ = try i.store(clif.MemFlags.DEFAULT, nl, addr, 21);
        const fd = try i.iconst(clif.Type.I64, 2);
        const len = try i.iconst(clif.Type.I64, 22);
        _ = try i.call(write_func, &[_]clif.Value{ fd, addr, len });
        const v128 = try i.iconst(clif.Type.I64, 128);
        const ec = try i.iadd(sig, v128);
        _ = try i.call(exit_func, &[_]clif.Value{ec});
        _ = try i.trap(.unreachable_code_reached);
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

/// Generate __cot_install_signals() -> void
///
/// Calls signal() for SIGILL(4), SIGABRT(6), SIGFPE(8), SIGBUS(10), SIGSEGV(11).
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

    // Import signal(signum, handler) -> old_handler
    const signal_idx = func_index_map.get("signal") orelse 0;
    var signal_sig = clif.Signature.init(.system_v);
    try signal_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try signal_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try signal_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const signal_sig_ref = try builder.importSignature(signal_sig);
    const signal_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = signal_idx } },
        .signature = signal_sig_ref,
        .colocated = false,
    });

    // Get address of __cot_signal_handler
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

    // Install for each signal
    const signals = [_]i64{ 4, 6, 8, 10, 11 };
    for (signals) |signum| {
        const v_sig = try ins.iconst(clif.Type.I64, signum);
        _ = try ins.call(signal_func, &[_]clif.Value{ v_sig, handler_addr });
    }

    _ = try ins.return_(&.{});

    try builder.sealAllBlocks();
    builder.finalize();

    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

/// Generate __cot_print_backtrace() -> void
///
/// Calls libc backtrace() to capture up to 64 return addresses, then calls
/// backtrace_symbols_fd() to print resolved symbol names to stderr.
/// This produces output like:
///   0   selfcot  0x100146da8 _SSABuilder_convertStoreLocalField + 284
///   1   selfcot  0x100140108 _SSABuilder_convertNode + 1180
///
/// Reference: macOS/Linux backtrace(3), backtrace_symbols_fd(3)
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

    // Signature: () -> void (no params)

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    // Import backtrace(buf, size) -> int
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

    // Import backtrace_symbols_fd(buf, size, fd) -> void
    const btsf_idx = func_index_map.get("backtrace_symbols_fd") orelse 0;
    var btsf_sig = clif.Signature.init(.system_v);
    try btsf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // buf
    try btsf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // size
    try btsf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    const btsf_sig_ref = try builder.importSignature(btsf_sig);
    const btsf_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = btsf_idx } },
        .signature = btsf_sig_ref,
        .colocated = false,
    });

    // Stack slot for backtrace buffer: 64 pointers × 8 bytes = 512 bytes
    const bt_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 512,
        .align_shift = 3,
    });

    const ins = builder.ins();

    // Call backtrace(buf, 64)
    const bt_buf = try ins.stackAddr(clif.Type.I64, bt_slot, 0);
    const v_64 = try ins.iconst(clif.Type.I64, 64);
    const bt_result = try ins.call(bt_func, &[_]clif.Value{ bt_buf, v_64 });
    const count = bt_result.results[0];

    // Skip first 2 frames (__cot_print_backtrace + @trap/@panic lowering)
    // Adjust buffer pointer and count
    const v_16 = try ins.iconst(clif.Type.I64, 16); // 2 * 8 bytes
    const adjusted_buf = try ins.iadd(bt_buf, v_16);
    const v_2 = try ins.iconst(clif.Type.I64, 2);
    const adjusted_count = try ins.isub(count, v_2);

    // Call backtrace_symbols_fd(adjusted_buf, adjusted_count, 2)
    // fd=2 is stderr
    const v_fd = try ins.iconst(clif.Type.I64, 2);
    _ = try ins.call(btsf_func, &[_]clif.Value{ adjusted_buf, adjusted_count, v_fd });

    _ = try ins.return_(&.{});

    try builder.sealAllBlocks();
    builder.finalize();

    return try native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
