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

    // Import write, _exit, and __cot_print_backtrace
    // Go pattern: signal handler calls traceback before exit (runtime/signal_unix.go)
    const bt_print_idx = func_index_map.get("__cot_print_backtrace") orelse 0;
    const bt_print_sig = clif.Signature.init(.system_v);
    const bt_print_sig_ref = try builder.importSignature(bt_print_sig);
    const bt_print_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = bt_print_idx } },
        .signature = bt_print_sig_ref,
        .colocated = true,
    });

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
        _ = try i.call(bt_print_func, &[_]clif.Value{});
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
        _ = try i.call(bt_print_func, &[_]clif.Value{});
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
/// Sets up alternate signal stack (sigaltstack) then installs signal handlers
/// using sigaction with SA_ONSTACK for SIGILL(4), SIGABRT(6), SIGFPE(8),
/// SIGBUS(10), SIGSEGV(11).
///
/// The alternate stack allows the signal handler to run even during stack
/// overflow, where the main stack's guard page has been hit and there's no
/// space for a normal signal handler frame.
///
/// Reference: Go runtime/signal_unix.go:signalstack — uses sigaltstack for
/// crash recovery during goroutine stack overflow.
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
    // Go runtime: signalstack allocates _g_.m.gsignal stack for signal handling.
    // We use malloc(SIGSTKSZ=65536) for the alternate stack buffer.
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

    const SIGSTKSZ: i64 = 65536; // 64KB alternate stack
    const v_stksz = try ins.iconst(clif.Type.I64, SIGSTKSZ);
    const malloc_result = try ins.call(malloc_func, &[_]clif.Value{v_stksz});
    const alt_stack_buf = malloc_result.results[0];

    // ---- Step 2: Call sigaltstack to register the alternate stack ----
    // struct stack_t { void *ss_sp; int ss_flags; size_t ss_size; }
    // On ARM64 macOS: ss_sp at offset 0 (8 bytes), ss_flags at 8 (4 bytes, padded to 8), ss_size at 16 (8 bytes)
    // Total: 24 bytes
    const ss_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 24,
        .align_shift = 3,
    });
    const ss_addr = try ins.stackAddr(clif.Type.I64, ss_slot, 0);
    _ = try ins.store(clif.MemFlags.DEFAULT, alt_stack_buf, ss_addr, 0); // ss_sp
    const v_zero_flags = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.store(clif.MemFlags.DEFAULT, v_zero_flags, ss_addr, 8); // ss_flags = 0
    _ = try ins.store(clif.MemFlags.DEFAULT, v_stksz, ss_addr, 16); // ss_size

    const sigaltstack_idx = func_index_map.get("sigaltstack") orelse 0;
    var sigaltstack_sig = clif.Signature.init(.system_v);
    try sigaltstack_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // new stack_t*
    try sigaltstack_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // old stack_t* (null)
    try sigaltstack_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sigaltstack_sig_ref = try builder.importSignature(sigaltstack_sig);
    const sigaltstack_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = sigaltstack_idx } },
        .signature = sigaltstack_sig_ref,
        .colocated = false,
    });
    const v_null = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(sigaltstack_func, &[_]clif.Value{ ss_addr, v_null });

    // ---- Step 3: Install handlers via sigaction with SA_ONSTACK ----
    // struct sigaction { void (*sa_handler)(int); sigset_t sa_mask; int sa_flags; }
    // macOS ARM64: sa_handler at 0 (8 bytes), sa_mask at 8 (4 bytes), sa_flags at 12 (4 bytes)
    // Total: 16 bytes (we allocate 16)
    const sa_slot = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 16,
        .align_shift = 3,
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

    // Import sigaction(signum, act, oldact) -> int
    const sigaction_idx = func_index_map.get("sigaction") orelse 0;
    var sigaction_sig = clif.Signature.init(.system_v);
    try sigaction_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // signum
    try sigaction_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // act*
    try sigaction_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // oldact*
    try sigaction_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sigaction_sig_ref = try builder.importSignature(sigaction_sig);
    const sigaction_func = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = sigaction_idx } },
        .signature = sigaction_sig_ref,
        .colocated = false,
    });

    // SA_ONSTACK = 0x1 on macOS, 0x08000000 on Linux
    // For now use macOS value. TODO: conditional on target_os.
    const SA_ONSTACK: i64 = 0x1;

    // Fill sigaction struct: handler + (sa_mask | sa_flags<<32) as combined i64
    // macOS ARM64 struct __sigaction: sa_handler(8) + sa_mask(4) + sa_flags(4) = 16
    const sa_addr = try ins.stackAddr(clif.Type.I64, sa_slot, 0);
    _ = try ins.store(clif.MemFlags.DEFAULT, handler_addr, sa_addr, 0); // sa_handler
    // Pack sa_mask(0) in low 32 bits + sa_flags(SA_ONSTACK) in high 32 bits
    const v_mask_flags = try ins.iconst(clif.Type.I64, SA_ONSTACK << 32);
    _ = try ins.store(clif.MemFlags.DEFAULT, v_mask_flags, sa_addr, 8); // sa_mask + sa_flags

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
