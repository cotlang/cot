//! Threading Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates thin threading wrapper functions as CLIF IR, compiled through native_compile.compile().
//! Each function forwards to the corresponding pthread/libc call.
//!
//! Reference: compiler/codegen/native/io_native.zig (forwarding pattern)
//! Reference: pthread API (POSIX threads)

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

/// Generate all threading runtime functions as compiled native code.
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

    // --- Thread creation & joining ---

    // thread_spawn(fn_ptr, arg) → handle
    // Calls pthread_create(&slot, NULL, fn_ptr, arg), returns thread handle.
    try result.append(allocator, .{
        .name = "thread_spawn",
        .compiled = try generateThreadSpawn(allocator, isa, ctrl_plane, func_index_map),
    });

    // thread_join(handle) → void
    // Calls pthread_join(handle, NULL).
    try result.append(allocator, .{
        .name = "thread_join",
        .compiled = try generateThreadJoin(allocator, isa, ctrl_plane, func_index_map),
    });

    // thread_detach(handle) → void
    // Calls pthread_detach(handle).
    try result.append(allocator, .{
        .name = "thread_detach",
        .compiled = try generateThreadDetach(allocator, isa, ctrl_plane, func_index_map),
    });

    // --- Mutex ---

    // mutex_init() → ptr
    // malloc(64) + memset(0) + pthread_mutex_init(ptr, NULL) → return ptr
    try result.append(allocator, .{
        .name = "mutex_init",
        .compiled = try generateMutexInit(allocator, isa, ctrl_plane, func_index_map),
    });

    // mutex_lock(ptr) → void
    try result.append(allocator, .{
        .name = "mutex_lock",
        .compiled = try generateForward1Void(allocator, isa, ctrl_plane, func_index_map, "pthread_mutex_lock"),
    });

    // mutex_unlock(ptr) → void
    try result.append(allocator, .{
        .name = "mutex_unlock",
        .compiled = try generateForward1Void(allocator, isa, ctrl_plane, func_index_map, "pthread_mutex_unlock"),
    });

    // mutex_trylock(ptr) → i64 (0 = success)
    try result.append(allocator, .{
        .name = "mutex_trylock",
        .compiled = try generateForward1Ret(allocator, isa, ctrl_plane, func_index_map, "pthread_mutex_trylock"),
    });

    // mutex_destroy(ptr) → void
    // pthread_mutex_destroy(ptr) + free(ptr)
    try result.append(allocator, .{
        .name = "mutex_destroy",
        .compiled = try generateDestroyAndFree(allocator, isa, ctrl_plane, func_index_map, "pthread_mutex_destroy"),
    });

    // --- Condition Variables ---

    // cond_init() → ptr
    // malloc(48) + memset(0) + pthread_cond_init(ptr, NULL) → return ptr
    try result.append(allocator, .{
        .name = "cond_init",
        .compiled = try generateCondInit(allocator, isa, ctrl_plane, func_index_map),
    });

    // cond_wait(cond, mutex) → void
    try result.append(allocator, .{
        .name = "cond_wait",
        .compiled = try generateForward2Void(allocator, isa, ctrl_plane, func_index_map, "pthread_cond_wait"),
    });

    // cond_signal(cond) → void
    try result.append(allocator, .{
        .name = "cond_signal",
        .compiled = try generateForward1Void(allocator, isa, ctrl_plane, func_index_map, "pthread_cond_signal"),
    });

    // cond_broadcast(cond) → void
    try result.append(allocator, .{
        .name = "cond_broadcast",
        .compiled = try generateForward1Void(allocator, isa, ctrl_plane, func_index_map, "pthread_cond_broadcast"),
    });

    // cond_destroy(cond) → void
    // pthread_cond_destroy(cond) + free(cond)
    try result.append(allocator, .{
        .name = "cond_destroy",
        .compiled = try generateDestroyAndFree(allocator, isa, ctrl_plane, func_index_map, "pthread_cond_destroy"),
    });

    return result;
}

// ============================================================================
// thread_spawn(fn_ptr: i64, arg: i64) → i64 (thread handle)
//
// Stack-allocates pthread_t (8 bytes), calls pthread_create(&slot, NULL, fn_ptr, arg),
// then loads the pthread_t value from the stack slot to return as the handle.
// Pattern: io_native.zig generatePipe (stack slot + call + load)
// ============================================================================

fn generateThreadSpawn(
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

    // Signature: (fn_ptr: i64, arg: i64) → i64 (handle)
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const fn_ptr = params[0];
    const arg = params[1];

    // Stack slot for pthread_t (8 bytes, 8-byte aligned)
    const thread_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(8, 3)); // 2^3 = 8 alignment
    const thread_addr = try ins.stackAddr(clif.Type.I64, thread_slot, 0);

    // Call pthread_create(&thread_slot, NULL, fn_ptr, arg)
    const create_idx = func_index_map.get("pthread_create") orelse 0;
    var create_sig = clif.Signature.init(.system_v);
    try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // pthread_t*
    try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // attr (NULL)
    try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // start_routine
    try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // arg
    try create_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64)); // int result
    const sig_ref = try builder.importSignature(create_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = create_idx } },
        .signature = sig_ref,
        .colocated = false,
    });

    const v_null = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(func_ref, &[_]clif.Value{ thread_addr, v_null, fn_ptr, arg });

    // Load the pthread_t handle from the stack slot
    const handle = try ins.stackLoad(clif.Type.I64, thread_slot, 0);
    _ = try ins.return_(&[_]clif.Value{handle});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// thread_join(handle: i64) → void
// Calls pthread_join(handle, NULL).
// ============================================================================

fn generateThreadJoin(
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

    // Signature: (handle: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const handle = params[0];

    // Call pthread_join(handle, NULL)
    const join_idx = func_index_map.get("pthread_join") orelse 0;
    var join_sig = clif.Signature.init(.system_v);
    try join_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // pthread_t
    try join_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // retval (NULL)
    try join_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64)); // int result
    const sig_ref = try builder.importSignature(join_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = join_idx } },
        .signature = sig_ref,
        .colocated = false,
    });

    const v_null = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(func_ref, &[_]clif.Value{ handle, v_null });
    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// thread_detach(handle: i64) → void
// Calls pthread_detach(handle).
// ============================================================================

fn generateThreadDetach(
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

    // Signature: (handle: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const handle = params[0];

    // Call pthread_detach(handle)
    const detach_idx = func_index_map.get("pthread_detach") orelse 0;
    var detach_sig = clif.Signature.init(.system_v);
    try detach_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try detach_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(detach_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = detach_idx } },
        .signature = sig_ref,
        .colocated = false,
    });

    _ = try ins.call(func_ref, &[_]clif.Value{handle});
    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// mutex_init() → ptr
// malloc(64) + memset(ptr, 0, 64) + pthread_mutex_init(ptr, NULL) → return ptr
// 64 bytes covers both macOS (64 bytes) and Linux (40 bytes).
// ============================================================================

fn generateMutexInit(
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

    // Signature: () → i64 (ptr to mutex)
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Step 1: malloc(64)
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
    const v_64 = try ins.iconst(clif.Type.I64, 64);
    const malloc_result = try ins.call(malloc_ref, &[_]clif.Value{v_64});
    const ptr = malloc_result.results[0];

    // Step 2: memset(ptr, 0, 64)
    const memset_idx = func_index_map.get("memset") orelse 0;
    var memset_sig = clif.Signature.init(.system_v);
    try memset_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memset_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memset_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memset_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const memset_sig_ref = try builder.importSignature(memset_sig);
    const memset_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memset_idx } },
        .signature = memset_sig_ref,
        .colocated = false,
    });
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(memset_ref, &[_]clif.Value{ ptr, v_zero, v_64 });

    // Step 3: pthread_mutex_init(ptr, NULL)
    const init_idx = func_index_map.get("pthread_mutex_init") orelse 0;
    var init_sig = clif.Signature.init(.system_v);
    try init_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try init_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try init_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const init_sig_ref = try builder.importSignature(init_sig);
    const init_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = init_idx } },
        .signature = init_sig_ref,
        .colocated = false,
    });
    _ = try ins.call(init_ref, &[_]clif.Value{ ptr, v_zero });

    _ = try ins.return_(&[_]clif.Value{ptr});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// cond_init() → ptr
// malloc(48) + memset(ptr, 0, 48) + pthread_cond_init(ptr, NULL) → return ptr
// 48 bytes covers both macOS and Linux.
// ============================================================================

fn generateCondInit(
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

    // Signature: () → i64 (ptr to cond)
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    // Step 1: malloc(48)
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
    const v_48 = try ins.iconst(clif.Type.I64, 48);
    const malloc_result = try ins.call(malloc_ref, &[_]clif.Value{v_48});
    const ptr = malloc_result.results[0];

    // Step 2: memset(ptr, 0, 48)
    const memset_idx = func_index_map.get("memset") orelse 0;
    var memset_sig = clif.Signature.init(.system_v);
    try memset_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memset_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memset_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try memset_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const memset_sig_ref = try builder.importSignature(memset_sig);
    const memset_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = memset_idx } },
        .signature = memset_sig_ref,
        .colocated = false,
    });
    const v_zero = try ins.iconst(clif.Type.I64, 0);
    _ = try ins.call(memset_ref, &[_]clif.Value{ ptr, v_zero, v_48 });

    // Step 3: pthread_cond_init(ptr, NULL)
    const init_idx = func_index_map.get("pthread_cond_init") orelse 0;
    var init_sig = clif.Signature.init(.system_v);
    try init_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try init_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try init_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const init_sig_ref = try builder.importSignature(init_sig);
    const init_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = init_idx } },
        .signature = init_sig_ref,
        .colocated = false,
    });
    _ = try ins.call(init_ref, &[_]clif.Value{ ptr, v_zero });

    _ = try ins.return_(&[_]clif.Value{ptr});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic: forward 1-param function to libc call, no return value
// Used for: mutex_lock, mutex_unlock, cond_signal, cond_broadcast
// ============================================================================

fn generateForward1Void(
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

    // Signature: (i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);

    const libc_idx = func_index_map.get(libc_name) orelse 0;
    var libc_sig = clif.Signature.init(.system_v);
    try libc_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try libc_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64)); // pthread funcs return int
    const sig_ref = try builder.importSignature(libc_sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = libc_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    _ = try ins.call(func_ref, &[_]clif.Value{params[0]});
    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic: forward 1-param function to libc call, with return value
// Used for: mutex_trylock
// ============================================================================

fn generateForward1Ret(
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
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
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
    _ = try ins.return_(&[_]clif.Value{call_result.results[0]});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic: forward 2-param function to libc call, no return value
// Used for: cond_wait → pthread_cond_wait(cond, mutex)
// ============================================================================

fn generateForward2Void(
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

    // Signature: (i64, i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

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
    _ = try ins.call(func_ref, &[_]clif.Value{ params[0], params[1] });
    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Generic: destroy + free pattern
// Calls destroy_func(ptr) then free(ptr). Used for mutex_destroy, cond_destroy.
// ============================================================================

fn generateDestroyAndFree(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    destroy_func_name: []const u8,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (ptr: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const ptr = params[0];

    // Step 1: destroy_func(ptr)
    const destroy_idx = func_index_map.get(destroy_func_name) orelse 0;
    var destroy_sig = clif.Signature.init(.system_v);
    try destroy_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try destroy_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const destroy_sig_ref = try builder.importSignature(destroy_sig);
    const destroy_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = destroy_idx } },
        .signature = destroy_sig_ref,
        .colocated = false,
    });
    _ = try ins.call(destroy_ref, &[_]clif.Value{ptr});

    // Step 2: free(ptr)
    const free_idx = func_index_map.get("free") orelse 0;
    var free_sig = clif.Signature.init(.system_v);
    try free_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    const free_sig_ref = try builder.importSignature(free_sig);
    const free_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = free_idx } },
        .signature = free_sig_ref,
        .colocated = false,
    });
    _ = try ins.call(free_ref, &[_]clif.Value{ptr});

    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
