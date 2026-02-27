//! I/O Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates thin I/O wrapper functions as CLIF IR, compiled through native_compile.compile().
//! Each function forwards to the corresponding libc call.
//!
//! Reference: compiler/codegen/wasi_runtime.zig (Wasm I/O path)
//! Reference: cg_clif abi/mod.rs:183-201 (lib_call pattern for external calls)

const std = @import("std");
const Allocator = std.mem.Allocator;

const clif = @import("../../ir/clif/mod.zig");
const frontend_mod = @import("frontend/mod.zig");
const FunctionBuilder = frontend_mod.FunctionBuilder;
const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;
const native_compile = @import("compile.zig");
const arc_native = @import("arc_native.zig");
const RuntimeFunc = arc_native.RuntimeFunc;

const GlobalValueData = @import("../../ir/clif/globalvalue.zig").GlobalValueData;
const gv_ExternalName = @import("../../ir/clif/globalvalue.zig").ExternalName;

const debug = @import("../../pipeline_debug.zig");

/// Generate all I/O runtime functions as compiled native code.
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    argc_symbol_idx: u32,
    argv_symbol_idx: u32,
    envp_symbol_idx: u32,
    lib_mode: bool,
) !std.ArrayListUnmanaged(RuntimeFunc) {
    var result = std.ArrayListUnmanaged(RuntimeFunc){};
    errdefer {
        for (result.items) |*rf| rf.compiled.deinit();
        result.deinit(allocator);
    }

    // fd_write(fd, ptr, len) → i64  (calls libc write)
    try result.append(allocator, .{
        .name = "fd_write",
        .compiled = try generateForward3(allocator, isa, ctrl_plane, func_index_map, "write", true),
    });

    // fd_read(fd, buf, len) → i64  (calls libc read)
    try result.append(allocator, .{
        .name = "fd_read",
        .compiled = try generateForward3(allocator, isa, ctrl_plane, func_index_map, "read", true),
    });

    // fd_close(fd) → i64  (calls libc close, returns -errno on error)
    try result.append(allocator, .{
        .name = "fd_close",
        .compiled = try generateForward1WithErrno(allocator, isa, ctrl_plane, func_index_map, "close"),
    });

    // exit(code) → void  (calls libc _exit)
    try result.append(allocator, .{
        .name = "exit",
        .compiled = try generateExit(allocator, isa, ctrl_plane, func_index_map),
    });

    // fd_seek(fd, offset, whence) → i64  (calls libc lseek)
    try result.append(allocator, .{
        .name = "fd_seek",
        .compiled = try generateForward3(allocator, isa, ctrl_plane, func_index_map, "lseek", true),
    });

    // memset_zero(ptr, size) → void  (calls libc memset(ptr, 0, size))
    try result.append(allocator, .{
        .name = "memset_zero",
        .compiled = try generateMemsetZero(allocator, isa, ctrl_plane, func_index_map),
    });

    // fd_open(path_ptr, path_len, flags) → i64  (null-terminates path, calls libc open)
    try result.append(allocator, .{
        .name = "fd_open",
        .compiled = try generateFdOpen(allocator, isa, ctrl_plane, func_index_map),
    });

    // time() → i64  (nanoseconds since epoch, calls libc gettimeofday)
    try result.append(allocator, .{
        .name = "time",
        .compiled = try generateTime(allocator, isa, ctrl_plane, func_index_map),
    });

    // random(buf, len) → i64  (calls getentropy in 256-byte chunks)
    try result.append(allocator, .{
        .name = "random",
        .compiled = try generateRandom(allocator, isa, ctrl_plane, func_index_map),
    });

    // growslice(old_ptr, old_len, new_cap, elem_size) → new_ptr
    // Go reference: runtime/slice.go growslice (lines 178-287)
    try result.append(allocator, .{
        .name = "growslice",
        .compiled = try generateGrowSlice(allocator, isa, ctrl_plane, func_index_map),
    });

    // nextslicecap(newLen, oldCap) → newCap
    // Go reference: runtime/slice.go nextslicecap (lines 326-358)
    try result.append(allocator, .{
        .name = "nextslicecap",
        .compiled = try generateNextSliceCap(allocator, isa, ctrl_plane),
    });

    // args_count() → i64  (load argc from _cot_argc global)
    try result.append(allocator, .{
        .name = "args_count",
        .compiled = try generateArgsCount(allocator, isa, ctrl_plane, argc_symbol_idx),
    });

    // arg_len(n) → i64  (load argv[n] pointer, call strlen)
    try result.append(allocator, .{
        .name = "arg_len",
        .compiled = try generateArgLen(allocator, isa, ctrl_plane, func_index_map, argv_symbol_idx),
    });

    // arg_ptr(n) → i64  (load argv[n] pointer)
    try result.append(allocator, .{
        .name = "arg_ptr",
        .compiled = try generateArgPtr(allocator, isa, ctrl_plane, argv_symbol_idx),
    });

    // environ_count() → i64  (count non-null entries in envp array)
    try result.append(allocator, .{
        .name = "environ_count",
        .compiled = try generateEnvironCount(allocator, isa, ctrl_plane, envp_symbol_idx, lib_mode),
    });

    // environ_len(n) → i64  (strlen of envp[n])
    try result.append(allocator, .{
        .name = "environ_len",
        .compiled = try generateEnvironLen(allocator, isa, ctrl_plane, func_index_map, envp_symbol_idx, lib_mode),
    });

    // environ_ptr(n) → i64  (pointer to envp[n])
    try result.append(allocator, .{
        .name = "environ_ptr",
        .compiled = try generateEnvironPtr(allocator, isa, ctrl_plane, envp_symbol_idx, lib_mode),
    });

    // --- Network runtime ---

    // net_socket(family, type, proto) → i64  (calls libc socket)
    try result.append(allocator, .{
        .name = "net_socket",
        .compiled = try generateForward3(allocator, isa, ctrl_plane, func_index_map, "socket", true),
    });
    // net_bind(fd, addr, addrlen) → i64  (calls libc bind)
    try result.append(allocator, .{
        .name = "net_bind",
        .compiled = try generateForward3(allocator, isa, ctrl_plane, func_index_map, "bind", true),
    });
    // net_listen(fd, backlog) → i64  (calls libc listen)
    try result.append(allocator, .{
        .name = "net_listen",
        .compiled = try generateForward2(allocator, isa, ctrl_plane, func_index_map, "listen"),
    });
    // net_accept(fd) → i64  (calls accept(fd, NULL, NULL))
    try result.append(allocator, .{
        .name = "net_accept",
        .compiled = try generateNetAccept(allocator, isa, ctrl_plane, func_index_map),
    });
    // net_connect(fd, addr, addrlen) → i64  (calls libc connect)
    try result.append(allocator, .{
        .name = "net_connect",
        .compiled = try generateForward3(allocator, isa, ctrl_plane, func_index_map, "connect", true),
    });
    // net_set_reuse_addr(fd) → i64  (calls setsockopt)
    try result.append(allocator, .{
        .name = "net_set_reuse_addr",
        .compiled = try generateNetSetReuseAddr(allocator, isa, ctrl_plane, func_index_map),
    });
    // set_nonblocking(fd) → i64  (calls fcntl)
    try result.append(allocator, .{
        .name = "set_nonblocking",
        .compiled = try generateSetNonblocking(allocator, isa, ctrl_plane, func_index_map),
    });

    // --- Event loop runtime (kqueue on macOS) ---

    // kqueue_create() → i64  (calls libc kqueue)
    try result.append(allocator, .{
        .name = "kqueue_create",
        .compiled = try generateForward0(allocator, isa, ctrl_plane, func_index_map, "kqueue"),
    });
    // kevent_add(kq, fd, events) → i64
    try result.append(allocator, .{
        .name = "kevent_add",
        .compiled = try generateKeventAdd(allocator, isa, ctrl_plane, func_index_map),
    });
    // kevent_del(kq, fd, events) → i64
    try result.append(allocator, .{
        .name = "kevent_del",
        .compiled = try generateKeventDel(allocator, isa, ctrl_plane, func_index_map),
    });
    // kevent_wait(kq, events_buf, timeout) → i64
    try result.append(allocator, .{
        .name = "kevent_wait",
        .compiled = try generateKeventWait(allocator, isa, ctrl_plane, func_index_map),
    });

    // Epoll stubs (return -1 on macOS)
    try result.append(allocator, .{ .name = "epoll_create", .compiled = try generateReturnsNeg1(allocator, isa, ctrl_plane) });
    try result.append(allocator, .{ .name = "epoll_add", .compiled = try generateReturnsNeg1_3(allocator, isa, ctrl_plane) });
    try result.append(allocator, .{ .name = "epoll_del", .compiled = try generateReturnsNeg1_2(allocator, isa, ctrl_plane) });
    try result.append(allocator, .{ .name = "epoll_wait", .compiled = try generateReturnsNeg1_3(allocator, isa, ctrl_plane) });

    // --- Process runtime ---
    // fork/dup2/execve are libc-only (ABI-compatible, no wrapper needed).
    // waitpid/pipe need wrappers (WEXITSTATUS extraction, fd packing).

    // waitpid(pid) → i64  (calls waitpid, extracts WEXITSTATUS)
    // Named "cot_waitpid" to avoid linker collision with libc waitpid
    try result.append(allocator, .{
        .name = "cot_waitpid",
        .compiled = try generateWaitpid(allocator, isa, ctrl_plane, func_index_map),
    });
    // pipe() → i64  (calls pipe, packs fds into i64: read | (write << 32))
    // Named "cot_pipe" to avoid linker collision with libc pipe
    try result.append(allocator, .{
        .name = "cot_pipe",
        .compiled = try generatePipe(allocator, isa, ctrl_plane, func_index_map),
    });

    // isatty(fd) → i64: no wrapper needed, libc isatty is ABI-compatible.
    // User code calls the libc symbol directly via external reference.

    // --- Terminal PTY runtime ---

    // openpty() → i64 (calls libc openpty, packs fds: master | (slave << 32))
    try result.append(allocator, .{
        .name = "cot_openpty",
        .compiled = try generateOpenpty(allocator, isa, ctrl_plane, func_index_map),
    });
    // setsid() → i64: no wrapper needed, libc setsid is ABI-compatible.
    // ioctl_winsize(fd, rows, cols) → i64 (builds winsize struct, calls ioctl)
    try result.append(allocator, .{
        .name = "cot_ioctl_winsize",
        .compiled = try generateIoctlWinsize(allocator, isa, ctrl_plane, func_index_map),
    });

    return result;
}

// ============================================================================
// Generic forwarding: 3-param function → 3-param libc call
// Used for: fd_write→write, fd_read→read, fd_seek→lseek
// ============================================================================

fn generateForward3(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    libc_name: []const u8,
    has_return: bool,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64, i64, i64) -> i64 (or void)
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    if (has_return) {
        try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    }

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);

    // Call libc function with same arguments
    const libc_idx = func_index_map.get(libc_name) orelse 0;
    var libc_sig = clif.Signature.init(.system_v);
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    if (has_return) {
        try libc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    }
    const sig_ref = try builder.importSignature(libc_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = libc_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const call_result = try ins.call(func_ref, &[_]clif.Value{ params[0], params[1], params[2] });

    if (has_return) {
        _ = try ins.return_(&[_]clif.Value{call_result.results[0]});
    } else {
        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic forwarding: 1-param function → 1-param libc call
// Used for: fd_close→close
// ============================================================================

fn generateForward1(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    libc_name: []const u8,
    has_return: bool,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    if (has_return) {
        try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    }

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);

    const libc_idx = func_index_map.get(libc_name) orelse 0;
    var libc_sig = clif.Signature.init(.system_v);
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    if (has_return) {
        try libc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    }
    const sig_ref = try builder.importSignature(libc_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = libc_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const call_result = try ins.call(func_ref, &[_]clif.Value{params[0]});

    if (has_return) {
        _ = try ins.return_(&[_]clif.Value{call_result.results[0]});
    } else {
        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic forwarding with errno: 1-param function → libc call → -errno on error
// On success (result >= 0): returns result.
// On error (result < 0): calls __error() to get errno, returns -errno.
// Used for: fd_close→close
// ============================================================================

fn generateForward1WithErrno(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    libc_name: []const u8,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_success = try builder.createBlock();
    const block_error = try builder.createBlock();

    // -- entry: call libc function, branch on result --
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    var ins = builder.ins();
    const params = builder.blockParams(block_entry);

    const libc_idx = func_index_map.get(libc_name) orelse 0;
    var libc_sig = clif.Signature.init(.system_v);
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(libc_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = libc_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const call_result = try ins.call(func_ref, &[_]clif.Value{params[0]});
    const result = call_result.results[0];

    // Check if result < 0 (error)
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const is_error = try ins.icmp(.slt, result, v_zero);
    _ = try ins.brif(is_error, block_error, &[_]clif.Value{}, block_success, &[_]clif.Value{result});

    // -- success: return result as-is --
    builder.switchToBlock(block_success);
    _ = try builder.appendBlockParam(block_success, clif.Type.I64);
    try builder.ensureInsertedBlock();
    ins = builder.ins();
    const success_val = builder.blockParams(block_success)[0];
    _ = try ins.return_(&[_]clif.Value{success_val});

    // -- error: call __error() to get errno, return -errno --
    builder.switchToBlock(block_error);
    try builder.ensureInsertedBlock();
    ins = builder.ins();

    // __error() → int* (pointer to errno)
    const error_idx = func_index_map.get("__error") orelse 0;
    var error_sig = clif.Signature.init(.system_v);
    try error_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const error_sig_ref = try builder.importSignature(error_sig);
    const error_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = error_idx } },
        .signature = error_sig_ref,
        .colocated = false,
    });
    const error_result = try ins.call(error_ref, &[_]clif.Value{});
    const errno_ptr = error_result.results[0];

    // Load errno (int = i32), sign-extend to i64, negate
    const errno_i32 = try ins.load(clif.Type.I32, clif.MemFlags.DEFAULT, errno_ptr, 0);
    const errno_i64 = try ins.sextend(clif.Type.I64, errno_i32);
    const neg_errno = try ins.ineg(errno_i64);
    _ = try ins.return_(&[_]clif.Value{neg_errno});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// exit(code: i64) → void
// Calls libc _exit(code). Does not return.
// Reference: wasi_runtime.zig exit function
// ============================================================================

fn generateExit(
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
    const code = builder.blockParams(block_entry)[0];

    // Call _exit(code) — does not return
    const exit_idx = func_index_map.get("_exit") orelse 0;
    var exit_sig = clif.Signature.init(.system_v);
    try exit_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(exit_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = exit_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    _ = try ins.call(func_ref, &[_]clif.Value{code});

    // _exit doesn't return, but CLIF needs a terminator
    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// memset_zero(ptr: i64, size: i64) → void
// Calls libc memset(ptr, 0, size).
// Reference: arc.zig memset_zero
// ============================================================================

fn generateMemsetZero(
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

    // Signature: (ptr: i64, size: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const ptr = params[0];
    const size = params[1];

    // Call memset(ptr, 0, size) — libc signature: memset(void*, int, size_t) → void*
    const memset_idx = func_index_map.get("memset") orelse 0;
    var libc_sig = clif.Signature.init(.system_v);
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // 0 as i64
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64)); // memset returns ptr
    const sig_ref = try builder.importSignature(libc_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memset_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(func_ref, &[_]clif.Value{ ptr, v_zero, size });
    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// fd_open(path_ptr: i64, path_len: i64, flags: i64) → i64
// Null-terminates the path string, then calls libc open(path, flags, 0666).
// Reference: wasi_runtime.zig generateWasiPathOpenShim
// ============================================================================

fn generateFdOpen(
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

    // Signature: (path_ptr: i64, path_len: i64, flags: i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const insb = builder.ins();
    const params = builder.blockParams(block_entry);
    const path_ptr = params[0];
    const path_len = params[1];
    const flags = params[2];

    // Allocate stack buffer for null-terminated path (1024 bytes = PATH_MAX)
    const path_slot = try builder.createSizedStackSlot(
        clif.StackSlotData.explicit(1024, 3), // 1024 bytes, 8-byte aligned
    );
    const buf_addr = try insb.stackAddr(clif.Type.I64, path_slot, 0);

    // Call memcpy(buf_addr, path_ptr, path_len) to copy path to stack
    const memcpy_idx = func_index_map.get("memcpy") orelse 0;
    var memcpy_sig = clif.Signature.init(.system_v);
    try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memcpy_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const memcpy_sig_ref = try builder.importSignature(memcpy_sig);
    const memcpy_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memcpy_idx } },
        .signature = memcpy_sig_ref,
        .colocated = false,
    });
    _ = try insb.call(memcpy_ref, &[_]clif.Value{ buf_addr, path_ptr, path_len });

    // Null-terminate: store 0 at buf_addr + path_len
    const null_addr = try insb.iadd(buf_addr, path_len);
    const v_zero_byte = try insb.iconst(clif.Type.I8, 0);
    _ = try insb.store(.{}, v_zero_byte, null_addr, 0);

    // Call __open(buf_addr, flags, 0o666) → fd
    // IMPORTANT: Use __open (non-variadic) instead of open (variadic).
    // On Apple ARM64, variadic arguments go on the stack, not in registers.
    // Our CLIF code passes all args in registers, so open() reads garbage mode
    // from the stack. __open is the internal non-variadic syscall wrapper.
    const open_idx = func_index_map.get("__open") orelse 0;
    var open_sig = clif.Signature.init(.system_v);
    try open_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // path
    try open_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // flags
    try open_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // mode
    try open_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const open_sig_ref = try builder.importSignature(open_sig);
    const open_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = open_idx } },
        .signature = open_sig_ref,
        .colocated = false,
    });
    const mode = try insb.iconst(clif.Type.I64, 0o666);
    const call_result = try insb.call(open_ref, &[_]clif.Value{ buf_addr, flags, mode });
    const open_result = call_result.results[0];

    // Check if result < 0 (error) — convert to -errno
    const block_success = try builder.createBlock();
    const block_error = try builder.createBlock();

    const v_zero_check = try insb.iconst(clif.Type.I64, 0);
    const is_error = try insb.icmp(.slt, open_result, v_zero_check);
    _ = try insb.brif(is_error, block_error, &[_]clif.Value{}, block_success, &[_]clif.Value{open_result});

    // success: return fd
    builder.switchToBlock(block_success);
    _ = try builder.appendBlockParam(block_success, clif.Type.I64);
    try builder.ensureInsertedBlock();
    var ins_ok = builder.ins();
    const fd_val = builder.blockParams(block_success)[0];
    _ = try ins_ok.return_(&[_]clif.Value{fd_val});

    // error: call __error(), load errno, return -errno
    builder.switchToBlock(block_error);
    try builder.ensureInsertedBlock();
    var ins_err = builder.ins();

    const error_idx = func_index_map.get("__error") orelse 0;
    var error_sig = clif.Signature.init(.system_v);
    try error_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const error_sig_ref = try builder.importSignature(error_sig);
    const error_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = error_idx } },
        .signature = error_sig_ref,
        .colocated = false,
    });
    const error_result = try ins_err.call(error_ref, &[_]clif.Value{});
    const errno_ptr = error_result.results[0];
    const errno_i32 = try ins_err.load(clif.Type.I32, clif.MemFlags.DEFAULT, errno_ptr, 0);
    const errno_i64 = try ins_err.sextend(clif.Type.I64, errno_i32);
    const neg_errno = try ins_err.ineg(errno_i64);
    _ = try ins_err.return_(&[_]clif.Value{neg_errno});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// time() → i64
// Returns nanoseconds since epoch. Calls libc gettimeofday(&tv, NULL).
// Computes: tv_sec * 1_000_000_000 + tv_usec * 1_000
// Reference: wasi_runtime.zig generateWasiTimeShim
// ============================================================================

fn generateTime(
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

    // Signature: () → i64
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Stack slot for struct timeval { tv_sec: i64, tv_usec: i64 } = 16 bytes
    const tv_slot = try builder.createSizedStackSlot(
        clif.StackSlotData.explicit(16, 3), // 16 bytes, 8-byte aligned
    );
    const tv_addr = try ins.stackAddr(clif.Type.I64, tv_slot, 0);

    // Call gettimeofday(&tv, NULL)
    const gtod_idx = func_index_map.get("gettimeofday") orelse 0;
    var gtod_sig = clif.Signature.init(.system_v);
    try gtod_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // tv
    try gtod_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // tz (NULL)
    try gtod_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(gtod_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = gtod_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const v_null = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(func_ref, &[_]clif.Value{ tv_addr, v_null });

    // Load tv_sec and tv_usec from stack
    // tv_sec is always 8 bytes (time_t = long on both macOS and Linux).
    // tv_usec is 4 bytes on macOS (suseconds_t = int32_t) but 8 bytes on Linux
    // (suseconds_t = long). Load as I32 and extend — safe on both platforms since
    // microseconds max at 999999 (fits in 20 bits).
    const tv_sec = try ins.stackLoad(clif.Type.I64, tv_slot, 0);
    const tv_usec_i32 = try ins.stackLoad(clif.Type.I32, tv_slot, 8);
    const tv_usec = try ins.uextend(clif.Type.I64, tv_usec_i32);

    // Compute: tv_sec * 1_000_000_000 + tv_usec * 1_000
    const billion = try ins.iconst(clif.Type.I64, 1_000_000_000);
    const thousand = try ins.iconst(clif.Type.I64, 1_000);
    const sec_ns = try ins.imul(tv_sec, billion);
    const usec_ns = try ins.imul(tv_usec, thousand);
    const total = try ins.iadd(sec_ns, usec_ns);

    _ = try ins.return_(&[_]clif.Value{total});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic forwarding: 2-param function → 2-param libc call
// Used for: random→getentropy
// ============================================================================

fn generateForward2(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    libc_name: []const u8,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64, i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);

    const libc_idx = func_index_map.get(libc_name) orelse 0;
    var libc_sig = clif.Signature.init(.system_v);
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(libc_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = libc_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const call_result = try ins.call(func_ref, &[_]clif.Value{ params[0], params[1] });

    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// nextslicecap(newLen: i64, oldCap: i64) → i64
// Go reference: runtime/slice.go nextslicecap (lines 326-358)
//
// Pure computation — no libc calls.
// Algorithm:
//   if newLen > 2*oldCap → return newLen
//   if oldCap < 256 → return 2*oldCap
//   loop: newcap += (newcap + 768) >> 2 until newcap >= newLen
//   if newcap <= 0 → return newLen (overflow)
//   return newcap
// ============================================================================

fn generateNextSliceCap(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (newLen: i64, oldCap: i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    // Create all blocks upfront
    const block_entry = try builder.createBlock();
    const block_check_thresh = try builder.createBlock();
    const block_loop = try builder.createBlock();
    const block_check_overflow = try builder.createBlock();
    const block_ret = try builder.createBlock();

    // Block params — SSA values passed between blocks
    // ret_block: (result)
    const ret_result = try builder.appendBlockParam(block_ret, clif.Type.I64);
    // check_thresh: (newLen, doublecap, oldCap)
    const ct_newLen = try builder.appendBlockParam(block_check_thresh, clif.Type.I64);
    const ct_doublecap = try builder.appendBlockParam(block_check_thresh, clif.Type.I64);
    const ct_oldCap = try builder.appendBlockParam(block_check_thresh, clif.Type.I64);
    // loop: (newLen, newcap)
    const lp_newLen = try builder.appendBlockParam(block_loop, clif.Type.I64);
    const lp_newcap = try builder.appendBlockParam(block_loop, clif.Type.I64);
    // check_overflow: (newLen, final_cap)
    const co_newLen = try builder.appendBlockParam(block_check_overflow, clif.Type.I64);
    const co_final_cap = try builder.appendBlockParam(block_check_overflow, clif.Type.I64);

    // === block_entry(newLen, oldCap) ===
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const params = builder.blockParams(block_entry);
        const newLen = params[0];
        const oldCap = params[1];

        const doublecap = try ins.iadd(oldCap, oldCap);
        const cond1 = try ins.icmp(.sgt, newLen, doublecap);
        _ = try ins.brif(cond1, block_ret, &[_]clif.Value{newLen}, block_check_thresh, &[_]clif.Value{ newLen, doublecap, oldCap });
    }

    // === block_check_thresh(newLen, doublecap, oldCap) ===
    builder.switchToBlock(block_check_thresh);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const threshold = try ins.iconst(clif.Type.I64, 256);
        const cond2 = try ins.icmp(.slt, ct_oldCap, threshold);
        _ = try ins.brif(cond2, block_ret, &[_]clif.Value{ct_doublecap}, block_loop, &[_]clif.Value{ ct_newLen, ct_oldCap });
    }

    // === block_loop(newLen, newcap) ===
    builder.switchToBlock(block_loop);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        // newcap += (newcap + 3*256) >> 2
        const c768 = try ins.iconst(clif.Type.I64, 768);
        const sum = try ins.iadd(lp_newcap, c768);
        const c2 = try ins.iconst(clif.Type.I64, 2);
        const shifted = try ins.ushr(sum, c2);
        const newcap2 = try ins.iadd(lp_newcap, shifted);
        // if uint(newcap) >= uint(newLen) → done
        const cond3 = try ins.icmp(.uge, newcap2, lp_newLen);
        _ = try ins.brif(cond3, block_check_overflow, &[_]clif.Value{ lp_newLen, newcap2 }, block_loop, &[_]clif.Value{ lp_newLen, newcap2 });
    }

    // === block_check_overflow(newLen, final_cap) ===
    builder.switchToBlock(block_check_overflow);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const zero = try ins.iconst(clif.Type.I64, 0);
        const cond4 = try ins.icmp(.sle, co_final_cap, zero);
        _ = try ins.brif(cond4, block_ret, &[_]clif.Value{co_newLen}, block_ret, &[_]clif.Value{co_final_cap});
    }

    // === block_ret(result) ===
    builder.switchToBlock(block_ret);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        _ = try ins.return_(&[_]clif.Value{ret_result});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// growslice(old_ptr: i64, old_len: i64, new_cap: i64, elem_size: i64) → i64
// Go reference: runtime/slice.go growslice (lines 178-287)
//
// Allocates new buffer via malloc, copies old data via memcpy, frees old buffer.
// Returns new_ptr. Caller already knows new_cap.
// ============================================================================

fn generateGrowSlice(
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

    // Signature: (old_ptr: i64, old_len: i64, new_cap: i64, elem_size: i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const old_ptr = params[0];
    const old_len = params[1];
    const new_cap = params[2];
    const elem_size = params[3];

    // alloc_size = new_cap * elem_size
    const alloc_size = try ins.imul(new_cap, elem_size);

    // new_ptr = malloc(alloc_size)
    const malloc_idx = func_index_map.get("malloc") orelse 0;
    var malloc_sig = clif.Signature.init(.system_v);
    try malloc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try malloc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const malloc_sig_ref = try builder.importSignature(malloc_sig);
    const malloc_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = malloc_idx } },
        .signature = malloc_sig_ref,
        .colocated = false,
    });
    const malloc_result = try ins.call(malloc_ref, &[_]clif.Value{alloc_size});
    const new_ptr = malloc_result.results[0];

    // old_size = old_len * elem_size
    const old_size = try ins.imul(old_len, elem_size);

    // memcpy(new_ptr, old_ptr, old_size)
    const memcpy_idx = func_index_map.get("memcpy") orelse 0;
    var memcpy_sig = clif.Signature.init(.system_v);
    try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memcpy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memcpy_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const memcpy_sig_ref = try builder.importSignature(memcpy_sig);
    const memcpy_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memcpy_idx } },
        .signature = memcpy_sig_ref,
        .colocated = false,
    });
    _ = try ins.call(memcpy_ref, &[_]clif.Value{ new_ptr, old_ptr, old_size });

    // NOTE: Do NOT free(old_ptr) — the old buffer may be stack-allocated
    // (e.g., `var arr = [1, 2, 3]` is on the stack). We can't distinguish
    // stack vs heap pointers, so we skip free (matching Wasm bump allocator).

    _ = try ins.return_(&[_]clif.Value{new_ptr});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// random(buf: i64, len: i64) → i64: getentropy in 256-byte chunks
// getentropy has a 256-byte limit. For larger requests, loop in chunks.
// Reference: wasi_runtime.zig random (which also needs chunking)
// ============================================================================

fn generateRandom(
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

    // Signature: (buf: i64, len: i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    // Blocks: entry → loop(buf, remaining) → done
    const block_entry = try builder.createBlock();
    const block_loop = try builder.createBlock();
    const block_call = try builder.createBlock();
    const block_done = try builder.createBlock();

    // -- entry --
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    var ins = builder.ins();
    const entry_params = builder.blockParams(block_entry);
    const buf = entry_params[0];
    const len = entry_params[1];
    _ = try ins.jump(block_loop, &[_]clif.Value{ buf, len });

    // -- loop: check if remaining > 0 --
    builder.switchToBlock(block_loop);
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64); // buf_ptr
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64); // remaining
    try builder.ensureInsertedBlock();
    ins = builder.ins();

    const cur_buf = builder.blockParams(block_loop)[0];
    const remaining = builder.blockParams(block_loop)[1];
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const has_more = try ins.icmp(.sgt, remaining, v_zero);
    _ = try ins.brif(has_more, block_call, &[_]clif.Value{ cur_buf, remaining }, block_done, &[_]clif.Value{});

    // -- call: compute chunk_size = min(remaining, 256), call getentropy --
    builder.switchToBlock(block_call);
    _ = try builder.appendBlockParam(block_call, clif.Type.I64); // buf_ptr
    _ = try builder.appendBlockParam(block_call, clif.Type.I64); // remaining
    try builder.ensureInsertedBlock();
    ins = builder.ins();

    const call_buf = builder.blockParams(block_call)[0];
    const call_remaining = builder.blockParams(block_call)[1];
    const v_256 = try ins.iconst(clif.Type.I64, 256);

    // chunk_size = min(remaining, 256): if remaining < 256 then remaining else 256
    const is_small = try ins.icmp(.slt, call_remaining, v_256);
    const chunk_size = try ins.select(clif.Type.I64, is_small, call_remaining, v_256);

    // Call getentropy(call_buf, chunk_size)
    const getentropy_idx = func_index_map.get("getentropy") orelse 0;
    var ge_sig = clif.Signature.init(.system_v);
    try ge_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ge_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ge_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const ge_sig_ref = try builder.importSignature(ge_sig);
    const ge_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = getentropy_idx } },
        .signature = ge_sig_ref,
        .colocated = false,
    });
    _ = try ins.call(ge_ref, &[_]clif.Value{ call_buf, chunk_size });

    // Advance: buf += chunk_size, remaining -= chunk_size
    const next_buf = try ins.iadd(call_buf, chunk_size);
    const next_remaining = try ins.isub(call_remaining, chunk_size);
    _ = try ins.jump(block_loop, &[_]clif.Value{ next_buf, next_remaining });

    // -- done: return 0 --
    builder.switchToBlock(block_done);
    try builder.ensureInsertedBlock();
    ins = builder.ins();
    const v_zero_ret = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.return_(&[_]clif.Value{v_zero_ret});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// args_count() → i64: load argc from _cot_argc global
// Reference: wasi_runtime.zig generateWasiArgsCountShim
// ============================================================================

fn generateArgsCount(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    argc_symbol_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: () → i64
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Load argc from _cot_argc global data symbol
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, argc_symbol_idx),
            .offset = 0,
            .colocated = true,
            .tls = false,
        },
    });
    const argc_addr = try ins.globalValue(clif.Type.I64, gv);
    const argc = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, argc_addr, 0);

    _ = try ins.return_(&[_]clif.Value{argc});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// arg_len(n: i64) → i64: load argv[n] pointer, call strlen
// Reference: wasi_runtime.zig generateWasiArgLenShim
// ============================================================================

fn generateArgLen(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    argv_symbol_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const entry_params = builder.blockParams(block_entry);
    const n = entry_params[0];
    const ins = builder.ins();

    // Load argv pointer from _cot_argv global
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, argv_symbol_idx),
            .offset = 0,
            .colocated = true,
            .tls = false,
        },
    });
    const argv_addr = try ins.globalValue(clif.Type.I64, gv);
    const argv = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, argv_addr, 0);

    // Compute argv[n]: argv is char**, so argv + n * 8 (64-bit pointers)
    const eight = try ins.iconst(clif.Type.I64, 8);
    const offset = try ins.imul(n, eight);
    const arg_slot = try ins.iadd(argv, offset);
    const arg_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, arg_slot, 0);

    // Call strlen(arg_ptr) → length
    const strlen_idx = func_index_map.get("strlen") orelse 0;
    var strlen_sig = clif.Signature.init(.system_v);
    try strlen_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try strlen_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(strlen_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = strlen_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const call_result = try ins.call(func_ref, &[_]clif.Value{arg_ptr});
    const len = call_result.results[0];

    _ = try ins.return_(&[_]clif.Value{len});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// arg_ptr(n: i64) → i64: load argv[n] pointer
// Reference: wasi_runtime.zig generateWasiArgPtrShim
// ============================================================================

fn generateArgPtr(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    argv_symbol_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const entry_params = builder.blockParams(block_entry);
    const n = entry_params[0];
    const ins = builder.ins();

    // Load argv pointer from _cot_argv global
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, argv_symbol_idx),
            .offset = 0,
            .colocated = true,
            .tls = false,
        },
    });
    const argv_addr = try ins.globalValue(clif.Type.I64, gv);
    const argv = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, argv_addr, 0);

    // Compute argv[n]: argv is char**, so argv + n * 8 (64-bit pointers)
    const eight = try ins.iconst(clif.Type.I64, 8);
    const offset = try ins.imul(n, eight);
    const arg_slot = try ins.iadd(argv, offset);
    const arg_ptr_val = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, arg_slot, 0);

    _ = try ins.return_(&[_]clif.Value{arg_ptr_val});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// environ_count() → i64: count non-null entries in envp array
// Reference: wasi_runtime.zig generateWasiEnvironCountShim
// ============================================================================

fn generateEnvironCount(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    envp_symbol_idx: u32,
    lib_mode: bool,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: () → i64
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    // Blocks: entry → loop_header(count) → loop_body → done(count)
    const block_entry = try builder.createBlock();
    const block_loop = try builder.createBlock();
    const block_done = try builder.createBlock();

    // -- entry block --
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();
    var ins = builder.ins();

    // Load envp pointer.
    // Executable mode: from _cot_envp (colocated local, written by _main wrapper)
    // Lib mode: from _environ (POSIX libc global, external → GOT access)
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, envp_symbol_idx),
            .offset = 0,
            .colocated = !lib_mode,
            .tls = false,
        },
    });
    const envp_addr = try ins.globalValue(clif.Type.I64, gv);
    const envp = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, envp_addr, 0);
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.jump(block_loop, &[_]clif.Value{v_zero});

    // -- loop block: count envp entries until NULL --
    builder.switchToBlock(block_loop);
    _ = try builder.appendBlockParam(block_loop, clif.Type.I64); // count
    try builder.ensureInsertedBlock();
    ins = builder.ins();

    const count = builder.blockParams(block_loop)[0];
    const v_eight = try ins.iconst(clif.Type.I64, 8);
    const byte_offset = try ins.imul(count, v_eight);
    const slot_addr = try ins.iadd(envp, byte_offset);
    const entry_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, slot_addr, 0);
    const v_null = try ins.iconst(clif.Type.I64, 0);
    const is_null = try ins.icmp(.eq, entry_ptr, v_null);
    const v_one = try ins.iconst(clif.Type.I64, 1);
    const next_count = try ins.iadd(count, v_one);
    _ = try ins.brif(is_null, block_done, &[_]clif.Value{count}, block_loop, &[_]clif.Value{next_count});

    // -- done block: return count --
    builder.switchToBlock(block_done);
    _ = try builder.appendBlockParam(block_done, clif.Type.I64); // final count
    try builder.ensureInsertedBlock();
    ins = builder.ins();

    const final_count = builder.blockParams(block_done)[0];
    _ = try ins.return_(&[_]clif.Value{final_count});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// environ_len(n: i64) → i64: strlen of envp[n]
// Reference: wasi_runtime.zig generateWasiEnvironLenShim
// ============================================================================

fn generateEnvironLen(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    envp_symbol_idx: u32,
    lib_mode: bool,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const entry_params = builder.blockParams(block_entry);
    const n = entry_params[0];
    const ins = builder.ins();

    // Load envp pointer (colocated for executable _cot_envp, GOT for lib mode _environ)
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, envp_symbol_idx),
            .offset = 0,
            .colocated = !lib_mode,
            .tls = false,
        },
    });
    const envp_addr = try ins.globalValue(clif.Type.I64, gv);
    const envp = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, envp_addr, 0);

    // Compute envp[n]: envp + n * 8
    const eight = try ins.iconst(clif.Type.I64, 8);
    const offset = try ins.imul(n, eight);
    const slot_addr = try ins.iadd(envp, offset);
    const env_str = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, slot_addr, 0);

    // Call strlen(env_str)
    const strlen_idx = func_index_map.get("strlen") orelse 0;
    var strlen_sig = clif.Signature.init(.system_v);
    try strlen_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try strlen_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(strlen_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = strlen_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const call_result = try ins.call(func_ref, &[_]clif.Value{env_str});
    const len = call_result.results[0];

    _ = try ins.return_(&[_]clif.Value{len});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// environ_ptr(n: i64) → i64: pointer to envp[n] string
// Reference: wasi_runtime.zig generateWasiEnvironPtrShim
// ============================================================================

fn generateEnvironPtr(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    envp_symbol_idx: u32,
    lib_mode: bool,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (i64) → i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const entry_params = builder.blockParams(block_entry);
    const n = entry_params[0];
    const ins = builder.ins();

    // Load envp pointer (colocated for executable _cot_envp, GOT for lib mode _environ)
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, envp_symbol_idx),
            .offset = 0,
            .colocated = !lib_mode,
            .tls = false,
        },
    });
    const envp_addr = try ins.globalValue(clif.Type.I64, gv);
    const envp = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, envp_addr, 0);

    // Compute envp[n]: envp + n * 8
    const eight = try ins.iconst(clif.Type.I64, 8);
    const offset = try ins.imul(n, eight);
    const slot_addr = try ins.iadd(envp, offset);
    const env_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, slot_addr, 0);

    _ = try ins.return_(&[_]clif.Value{env_ptr});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic forwarding: 0-param function → 0-param libc call
// Used for: fork→fork, kqueue_create→kqueue
// ============================================================================

fn generateForward0(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    libc_name: []const u8,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: () → i64
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    const libc_idx = func_index_map.get(libc_name) orelse 0;
    var libc_sig = clif.Signature.init(.system_v);
    try libc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(libc_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = libc_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const call_result = try ins.call(func_ref, &[_]clif.Value{});
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Stub generators: return -1 (for unsupported platform features like epoll on macOS)
// ============================================================================

fn generateReturnsNeg1(allocator: Allocator, isa: native_compile.TargetIsa, ctrl_plane: *native_compile.ControlPlane) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const block = try builder.createBlock();
    builder.switchToBlock(block);
    try builder.ensureInsertedBlock();
    const ins = builder.ins();
    const v = try ins.iconst(clif.Type.I64, -1);
    _ = try ins.return_(&[_]clif.Value{v});
    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

fn generateReturnsNeg1_2(allocator: Allocator, isa: native_compile.TargetIsa, ctrl_plane: *native_compile.ControlPlane) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const block = try builder.createBlock();
    builder.switchToBlock(block);
    try builder.appendBlockParamsForFunctionParams(block);
    try builder.ensureInsertedBlock();
    const ins = builder.ins();
    const v = try ins.iconst(clif.Type.I64, -1);
    _ = try ins.return_(&[_]clif.Value{v});
    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

fn generateReturnsNeg1_3(allocator: Allocator, isa: native_compile.TargetIsa, ctrl_plane: *native_compile.ControlPlane) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();
    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const block = try builder.createBlock();
    builder.switchToBlock(block);
    try builder.appendBlockParamsForFunctionParams(block);
    try builder.ensureInsertedBlock();
    const ins = builder.ins();
    const v = try ins.iconst(clif.Type.I64, -1);
    _ = try ins.return_(&[_]clif.Value{v});
    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// net_accept(fd) → i64: calls accept(fd, NULL, NULL)
// ============================================================================

fn generateNetAccept(
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
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const fd = builder.blockParams(block_entry)[0];

    const accept_idx = func_index_map.get("accept") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // addr (NULL)
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // addrlen (NULL)
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = accept_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const v_null = try ins.iconst(clif.Type.I64, 0);
    const call_result = try ins.call(func_ref, &[_]clif.Value{ fd, v_null, v_null });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// net_set_reuse_addr(fd) → i64: setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &1, 4)
// macOS: SOL_SOCKET=0xFFFF, SO_REUSEADDR=0x0004
// ============================================================================

fn generateNetSetReuseAddr(
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
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const fd = builder.blockParams(block_entry)[0];

    // Stack slot for optval (i32 = 1)
    const val_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(4, 2));
    const val_addr = try ins.stackAddr(clif.Type.I64, val_slot, 0);
    const v_one = try ins.iconst(clif.Type.I32, 1);
    _ = try ins.store(.{}, v_one, val_addr, 0);

    // setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &val, 4)
    const sso_idx = func_index_map.get("setsockopt") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // level
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // optname
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // optval
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // optlen
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = sso_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const sol_socket = try ins.iconst(clif.Type.I64, 0xFFFF); // SOL_SOCKET on macOS
    const so_reuse = try ins.iconst(clif.Type.I64, 0x0004); // SO_REUSEADDR
    const optlen = try ins.iconst(clif.Type.I64, 4);
    const call_result = try ins.call(func_ref, &[_]clif.Value{ fd, sol_socket, so_reuse, val_addr, optlen });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// set_nonblocking(fd) → i64: fcntl(fd, F_SETFL, O_NONBLOCK)
// macOS: F_SETFL=4, O_NONBLOCK=0x0004
// ============================================================================

fn generateSetNonblocking(
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
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const fd = builder.blockParams(block_entry)[0];

    // fcntl(fd, F_SETFL, O_NONBLOCK) — variadic: int fcntl(int fd, int cmd, ...)
    // Apple ARM64: pad X2-X7 to force variadic arg onto stack.
    const fcntl_idx = func_index_map.get("fcntl") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // cmd
    const is_aarch64 = isa == .aarch64;
    if (is_aarch64) {
        for (0..6) |_| try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    }
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // flags
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = fcntl_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const f_setfl = try ins.iconst(clif.Type.I64, 4);
    const o_nonblock = try ins.iconst(clif.Type.I64, 0x0004);
    const call_result = if (is_aarch64) blk: {
        const pad = try ins.iconst(clif.Type.I64, 0);
        break :blk try ins.call(func_ref, &[_]clif.Value{ fd, f_setfl, pad, pad, pad, pad, pad, pad, o_nonblock });
    } else try ins.call(func_ref, &[_]clif.Value{ fd, f_setfl, o_nonblock });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// kevent_add(kq, fd, events) → i64: register fd for events with EV_ADD
// Builds a struct kevent on stack, calls kevent(kq, &changelist, 1, NULL, 0, NULL)
// macOS struct kevent: { ident(8), filter(2), flags(2), fflags(4), data(8), udata(8) } = 32 bytes
// ============================================================================

fn generateKeventAdd(
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

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // kq
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // events (filter)
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const kq = params[0];
    const fd = params[1];
    const events = params[2];

    // Stack slot for struct kevent (32 bytes, 8-byte aligned)
    const kev_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(32, 3));
    const kev_addr = try ins.stackAddr(clif.Type.I64, kev_slot, 0);

    // Zero-fill the struct first
    const memset_idx = func_index_map.get("memset") orelse 0;
    var ms_sig = clif.Signature.init(.system_v);
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const ms_sig_ref = try builder.importSignature(ms_sig);
    const ms_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memset_idx } },
        .signature = ms_sig_ref,
        .colocated = false,
    });
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const v_32 = try ins.iconst(clif.Type.I64, 32);
    _ = try ins.call(ms_ref, &[_]clif.Value{ kev_addr, v_zero, v_32 });

    // Fill kevent: ident = fd (offset 0, 8 bytes)
    _ = try ins.store(.{}, fd, kev_addr, 0);
    // filter = events (offset 8, i16 = 2 bytes)
    const events_i16 = try ins.ireduce(clif.Type.I16, events);
    _ = try ins.store(.{}, events_i16, kev_addr, 8);
    // flags = EV_ADD (0x0001) (offset 10, i16 = 2 bytes)
    const ev_add = try ins.iconst(clif.Type.I16, 0x0001);
    _ = try ins.store(.{}, ev_add, kev_addr, 10);

    // Call kevent(kq, &changelist, 1, NULL, 0, NULL)
    const kev_idx = func_index_map.get("kevent") orelse 0;
    var kev_sig = clif.Signature.init(.system_v);
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // kq
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // changelist
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // nchanges
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // eventlist
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // nevents
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // timeout
    try kev_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const kev_sig_ref = try builder.importSignature(kev_sig);
    const kev_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = kev_idx } },
        .signature = kev_sig_ref,
        .colocated = false,
    });
    const v_one = try ins.iconst(clif.Type.I64, 1);
    const v_null = try ins.iconst(clif.Type.I64, 0);
    const call_result = try ins.call(kev_ref, &[_]clif.Value{ kq, kev_addr, v_one, v_null, v_null, v_null });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// kevent_del(kq, fd, events) → i64: remove fd from kqueue with EV_DELETE
// ============================================================================

fn generateKeventDel(
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
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const kq = params[0];
    const fd = params[1];
    const events = params[2];

    // Build kevent struct on stack (32 bytes)
    const kev_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(32, 3));
    const kev_addr = try ins.stackAddr(clif.Type.I64, kev_slot, 0);

    // Zero-fill
    const memset_idx = func_index_map.get("memset") orelse 0;
    var ms_sig = clif.Signature.init(.system_v);
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const ms_sig_ref = try builder.importSignature(ms_sig);
    const ms_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memset_idx } },
        .signature = ms_sig_ref,
        .colocated = false,
    });
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const v_32 = try ins.iconst(clif.Type.I64, 32);
    _ = try ins.call(ms_ref, &[_]clif.Value{ kev_addr, v_zero, v_32 });

    // ident = fd, filter = events, flags = EV_DELETE (0x0002)
    _ = try ins.store(.{}, fd, kev_addr, 0);
    const events_i16 = try ins.ireduce(clif.Type.I16, events);
    _ = try ins.store(.{}, events_i16, kev_addr, 8);
    const ev_delete = try ins.iconst(clif.Type.I16, 0x0002);
    _ = try ins.store(.{}, ev_delete, kev_addr, 10);

    // kevent(kq, &changelist, 1, NULL, 0, NULL)
    const kev_idx = func_index_map.get("kevent") orelse 0;
    var kev_sig = clif.Signature.init(.system_v);
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const kev_sig_ref = try builder.importSignature(kev_sig);
    const kev_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = kev_idx } },
        .signature = kev_sig_ref,
        .colocated = false,
    });
    const v_one = try ins.iconst(clif.Type.I64, 1);
    const v_null = try ins.iconst(clif.Type.I64, 0);
    const call_result = try ins.call(kev_ref, &[_]clif.Value{ kq, kev_addr, v_one, v_null, v_null, v_null });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// kevent_wait(kq, events_buf, timeout_ms) → i64: wait for events
// Calls kevent(kq, NULL, 0, events_buf, MAX_EVENTS, &timeout)
// timeout_ms < 0 means infinite (NULL timeout pointer)
// ============================================================================

fn generateKeventWait(
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

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // kq
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // events_buf
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // timeout_ms
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const kq = params[0];
    const events_buf = params[1];
    const timeout_ms = params[2];

    // Stack slot for struct timespec { tv_sec: i64, tv_nsec: i64 } = 16 bytes
    const ts_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(16, 3));
    const ts_addr = try ins.stackAddr(clif.Type.I64, ts_slot, 0);

    // Convert timeout_ms to timespec: sec = ms / 1000, nsec = (ms % 1000) * 1_000_000
    const v_1000 = try ins.iconst(clif.Type.I64, 1000);
    const sec = try ins.sdiv(timeout_ms, v_1000);
    const rem = try ins.srem(timeout_ms, v_1000);
    const v_1000000 = try ins.iconst(clif.Type.I64, 1_000_000);
    const nsec = try ins.imul(rem, v_1000000);
    _ = try ins.stackStore(sec, ts_slot, 0);
    _ = try ins.stackStore(nsec, ts_slot, 8);

    // Use ts_addr as timeout (for simplicity, always pass timeout)
    // kevent(kq, NULL, 0, events_buf, 64, &timeout)
    const kev_idx = func_index_map.get("kevent") orelse 0;
    var kev_sig = clif.Signature.init(.system_v);
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try kev_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const kev_sig_ref = try builder.importSignature(kev_sig);
    const kev_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = kev_idx } },
        .signature = kev_sig_ref,
        .colocated = false,
    });
    const v_null = try ins.iconst(clif.Type.I64, 0);
    const v_max_events = try ins.iconst(clif.Type.I64, 64);
    const call_result = try ins.call(kev_ref, &[_]clif.Value{ kq, v_null, v_null, events_buf, v_max_events, ts_addr });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// waitpid(pid) → i64: calls libc waitpid(pid, &status, 0), returns WEXITSTATUS
// WEXITSTATUS(status) = (status >> 8) & 0xFF
// ============================================================================

fn generateWaitpid(
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
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const pid = builder.blockParams(block_entry)[0];

    // Stack slot for int status (4 bytes)
    const status_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(4, 2));
    const status_addr = try ins.stackAddr(clif.Type.I64, status_slot, 0);

    // waitpid(pid, &status, 0) — look up "c_waitpid" to avoid collision with this wrapper
    const wp_idx = func_index_map.get("c_waitpid") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // pid
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // &status
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // options
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = wp_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(func_ref, &[_]clif.Value{ pid, status_addr, v_zero });

    // Load status (i32), extract WEXITSTATUS: (status >> 8) & 0xFF
    const status_i32 = try ins.stackLoad(clif.Type.I32, status_slot, 0);
    const status_i64 = try ins.sextend(clif.Type.I64, status_i32);
    const v_8 = try ins.iconst(clif.Type.I64, 8);
    const shifted = try ins.ushr(status_i64, v_8);
    const v_ff = try ins.iconst(clif.Type.I64, 0xFF);
    const exit_status = try ins.band(shifted, v_ff);
    _ = try ins.return_(&[_]clif.Value{exit_status});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// pipe() → i64: calls libc pipe(fds), returns read_fd | (write_fd << 32)
// ============================================================================

fn generatePipe(
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

    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Stack slot for int fds[2] (8 bytes)
    const fds_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(8, 2));
    const fds_addr = try ins.stackAddr(clif.Type.I64, fds_slot, 0);

    // pipe(fds) — look up "c_pipe" to avoid collision with this wrapper
    const pipe_idx = func_index_map.get("c_pipe") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = pipe_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    _ = try ins.call(func_ref, &[_]clif.Value{fds_addr});

    // Load read_fd (fds[0], i32) and write_fd (fds[1], i32)
    const read_fd_i32 = try ins.stackLoad(clif.Type.I32, fds_slot, 0);
    const write_fd_i32 = try ins.stackLoad(clif.Type.I32, fds_slot, 4);

    // Pack: read_fd | (write_fd << 32)
    const read_fd = try ins.uextend(clif.Type.I64, read_fd_i32);
    const write_fd = try ins.uextend(clif.Type.I64, write_fd_i32);
    const v_32 = try ins.iconst(clif.Type.I64, 32);
    const write_shifted = try ins.ishl(write_fd, v_32);
    const result = try ins.bor(read_fd, write_shifted);
    _ = try ins.return_(&[_]clif.Value{result});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// openpty() → i64: calls libc openpty(&master, &slave, NULL, NULL, NULL)
// Returns master_fd | (slave_fd << 32)
// ============================================================================

fn generateOpenpty(
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

    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Stack slots for int master_fd and int slave_fd (4 bytes each)
    const master_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(4, 2));
    const slave_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(4, 2));
    const master_addr = try ins.stackAddr(clif.Type.I64, master_slot, 0);
    const slave_addr = try ins.stackAddr(clif.Type.I64, slave_slot, 0);

    // openpty(&master, &slave, NULL, NULL, NULL)
    const pty_idx = func_index_map.get("c_openpty") orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // &master
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // &slave
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // name (NULL)
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // termp (NULL)
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // winp (NULL)
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = pty_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const v_null = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(func_ref, &[_]clif.Value{ master_addr, slave_addr, v_null, v_null, v_null });

    // Load master_fd (i32) and slave_fd (i32), pack as master | (slave << 32)
    const master_i32 = try ins.stackLoad(clif.Type.I32, master_slot, 0);
    const slave_i32 = try ins.stackLoad(clif.Type.I32, slave_slot, 0);
    const master_fd = try ins.uextend(clif.Type.I64, master_i32);
    const slave_fd = try ins.uextend(clif.Type.I64, slave_i32);
    const v_32 = try ins.iconst(clif.Type.I64, 32);
    const slave_shifted = try ins.ishl(slave_fd, v_32);
    const packed_fds = try ins.bor(master_fd, slave_shifted);
    _ = try ins.return_(&[_]clif.Value{packed_fds});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// ioctl_winsize(fd, rows, cols) → i64: ioctl(fd, TIOCSWINSZ, &ws)
// macOS: TIOCSWINSZ = 0x80087467
// struct winsize: { ws_row(u16), ws_col(u16), ws_xpixel(u16), ws_ypixel(u16) } = 8 bytes
// ============================================================================

fn generateIoctlWinsize(
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

    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // rows
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // cols
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const fd = params[0];
    const rows = params[1];
    const cols = params[2];

    // Stack slot for struct winsize (8 bytes, 2-byte aligned)
    const ws_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(8, 1));
    const ws_addr = try ins.stackAddr(clif.Type.I64, ws_slot, 0);

    // Zero-fill the struct
    const memset_idx = func_index_map.get("memset") orelse 0;
    var ms_sig = clif.Signature.init(.system_v);
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try ms_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const ms_sig_ref = try builder.importSignature(ms_sig);
    const ms_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memset_idx } },
        .signature = ms_sig_ref,
        .colocated = false,
    });
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    const v_8 = try ins.iconst(clif.Type.I64, 8);
    _ = try ins.call(ms_ref, &[_]clif.Value{ ws_addr, v_zero, v_8 });

    // ws_row = rows (offset 0, u16)
    const rows_u16 = try ins.ireduce(clif.Type.I16, rows);
    _ = try ins.store(.{}, rows_u16, ws_addr, 0);
    // ws_col = cols (offset 2, u16)
    const cols_u16 = try ins.ireduce(clif.Type.I16, cols);
    _ = try ins.store(.{}, cols_u16, ws_addr, 2);

    // ioctl(fd, TIOCSWINSZ, &ws) — variadic: int ioctl(int fd, unsigned long request, ...)
    // Apple ARM64: variadic args must go on the stack, not in registers.
    // Pad X2-X7 with dummy zeros to force the real variadic arg onto the stack.
    // Port of rustc_codegen_cranelift adjust_call_for_c_variadic().
    const ioctl_idx = func_index_map.get("ioctl") orelse 0;
    var ioctl_sig = clif.Signature.init(.system_v);
    try ioctl_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // fd
    try ioctl_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // request
    const is_aarch64 = isa == .aarch64;
    if (is_aarch64) {
        for (0..6) |_| try ioctl_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    }
    try ioctl_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // argp
    try ioctl_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const ioctl_sig_ref = try builder.importSignature(ioctl_sig);
    const ioctl_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = ioctl_idx } },
        .signature = ioctl_sig_ref,
        .colocated = false,
    });
    const tiocswinsz = try ins.iconst(clif.Type.I64, 0x80087467); // macOS TIOCSWINSZ
    const call_result = if (is_aarch64) blk: {
        const pad = try ins.iconst(clif.Type.I64, 0);
        break :blk try ins.call(ioctl_ref, &[_]clif.Value{ fd, tiocswinsz, pad, pad, pad, pad, pad, pad, ws_addr });
    } else try ins.call(ioctl_ref, &[_]clif.Value{ fd, tiocswinsz, ws_addr });
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();
    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
