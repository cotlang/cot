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

const debug = @import("../../pipeline_debug.zig");

/// Generate all I/O runtime functions as compiled native code.
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
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

    // fd_close(fd) → i64  (calls libc close)
    try result.append(allocator, .{
        .name = "fd_close",
        .compiled = try generateForward1(allocator, isa, ctrl_plane, func_index_map, "close", true),
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

    // isatty(fd) → i64: no wrapper needed, libc isatty is ABI-compatible.
    // User code calls the libc symbol directly via external reference.

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
