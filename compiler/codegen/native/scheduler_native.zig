//! Work-Stealing Scheduler Runtime — CLIF IR Generation
//!
//! Generates scheduler functions as CLIF IR for the `spawn { }` construct.
//! MVP: global queue only (no per-worker local queues, no work-stealing).
//! All spawns push to a mutex-protected global queue, all workers pop from it.
//!
//! Data structures (heap-allocated):
//!
//!   Scheduler (global singleton, 88 bytes):
//!     offset 0:   initialized  (i64) — 0 or 1
//!     offset 8:   num_workers  (i64)
//!     offset 16:  workers_ptr  (i64) — pointer to array of thread handles
//!     offset 24:  queue_head   (i64) — linked list head (Task*)
//!     offset 32:  queue_tail   (i64) — linked list tail (Task*)
//!     offset 40:  queue_size   (i64)
//!     offset 48:  queue_mutex  (i64) — mutex ptr
//!     offset 56:  active_tasks (i64) — atomic outstanding task count
//!     offset 64:  shutdown     (i64) — atomic shutdown flag
//!     offset 72:  idle_mutex   (i64) — mutex ptr for idle waiting
//!     offset 80:  idle_cond    (i64) — condition var ptr
//!
//!   Task (24 bytes, heap-allocated per spawn):
//!     offset 0:   fn_ptr   (i64) — function pointer to spawn body
//!     offset 8:   env_ptr  (i64) — capture environment pointer (0 if none)
//!     offset 16:  next     (i64) — linked list next pointer
//!
//! Reference: Go runtime/proc.go (GMP model — goroutine scheduling)
//! Reference: Rayon rustc_thread_pool (simpler steal-one pattern)
//! Reference: thread_native.zig (CLIF IR generation pattern)
//! Reference: io_native.zig (global data symbol access pattern)

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

// Scheduler struct offsets
const SCHED_INITIALIZED: i32 = 0;
const SCHED_NUM_WORKERS: i32 = 8;
const SCHED_WORKERS_PTR: i32 = 16;
const SCHED_QUEUE_HEAD: i32 = 24;
const SCHED_QUEUE_TAIL: i32 = 32;
const SCHED_QUEUE_SIZE: i32 = 40;
const SCHED_QUEUE_MUTEX: i32 = 48;
const SCHED_ACTIVE_TASKS: i32 = 56;
const SCHED_SHUTDOWN: i32 = 64;
const SCHED_IDLE_MUTEX: i32 = 72;
const SCHED_IDLE_COND: i32 = 80;
const SCHED_SIZE: i64 = 88;

// Task struct offsets
const TASK_FN_PTR: i32 = 0;
const TASK_ENV_PTR: i32 = 8;
const TASK_NEXT: i32 = 16;
const TASK_SIZE: i64 = 24;

// macOS sysconf constant
const SC_NPROCESSORS_ONLN: i64 = 58;

/// Generate all scheduler runtime functions as compiled native code.
/// sched_symbol_idx: external name index for the _cot_sched_ptr BSS symbol.
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    sched_symbol_idx: u32,
) !std.ArrayListUnmanaged(RuntimeFunc) {
    var result = std.ArrayListUnmanaged(RuntimeFunc){};
    errdefer {
        for (result.items) |*rf| rf.compiled.deinit();
        result.deinit(allocator);
    }

    // Exported functions
    try result.append(allocator, .{
        .name = "sched_spawn",
        .compiled = try generateSchedSpawn(allocator, isa, ctrl_plane, func_index_map, sched_symbol_idx),
    });
    try result.append(allocator, .{
        .name = "sched_shutdown",
        .compiled = try generateSchedShutdown(allocator, isa, ctrl_plane, func_index_map, sched_symbol_idx),
    });
    try result.append(allocator, .{
        .name = "sched_get_num_workers",
        .compiled = try generateSchedGetNumWorkers(allocator, isa, ctrl_plane, sched_symbol_idx),
    });

    // Internal functions
    try result.append(allocator, .{
        .name = "sched_init",
        .compiled = try generateSchedInit(allocator, isa, ctrl_plane, func_index_map, sched_symbol_idx),
    });
    try result.append(allocator, .{
        .name = "sched_worker_spawn",
        .compiled = try generateWorkerSpawn(allocator, isa, ctrl_plane, func_index_map),
    });
    try result.append(allocator, .{
        .name = "sched_worker_loop",
        .compiled = try generateWorkerLoop(allocator, isa, ctrl_plane, func_index_map, sched_symbol_idx),
    });
    try result.append(allocator, .{
        .name = "sched_join_workers",
        .compiled = try generateJoinWorkers(allocator, isa, ctrl_plane, func_index_map),
    });

    return result;
}

/// Helper: load the scheduler pointer from _cot_sched_ptr BSS symbol.
fn loadSchedPtr(clif_func: *clif.Function, builder: *FunctionBuilder, sched_symbol_idx: u32) !clif.Value {
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, sched_symbol_idx),
            .offset = 0,
            .colocated = true,
            .tls = false,
        },
    });
    const ins = builder.ins();
    const sched_global_addr = try ins.globalValue(clif.Type.I64, gv);
    return ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_global_addr, 0);
}

/// Helper: store scheduler pointer to the _cot_sched_ptr BSS symbol.
fn storeSchedPtr(clif_func: *clif.Function, builder: *FunctionBuilder, sched_symbol_idx: u32, value: clif.Value) !void {
    const gv = try clif_func.createGlobalValue(GlobalValueData{
        .symbol = .{
            .name = gv_ExternalName.initUser(0, sched_symbol_idx),
            .offset = 0,
            .colocated = true,
            .tls = false,
        },
    });
    const ins = builder.ins();
    const sched_global_addr = try ins.globalValue(clif.Type.I64, gv);
    _ = try ins.store(clif.MemFlags.DEFAULT, value, sched_global_addr, 0);
}

// ============================================================================
// sched_spawn(fn_ptr: i64, env_ptr: i64) -> void
//
// If scheduler not initialized, call sched_init().
// Allocate Task, push to global queue, signal one idle worker.
// Reference: Go newproc() (proc.go:5295)
// ============================================================================

fn generateSchedSpawn(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    sched_symbol_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (fn_ptr: i64, env_ptr: i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_do_init = try builder.createBlock();
    const block_post_init = try builder.createBlock();

    // --- Entry: load global scheduler ptr, check initialized ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = try loadSchedPtr(&clif_func, &builder, sched_symbol_idx);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const needs_init = try ins.icmp(.eq, sched_ptr, v_zero);
        _ = try ins.brif(needs_init, block_do_init, &.{}, block_post_init, &[_]clif.Value{sched_ptr});
    }

    // --- Call sched_init, reload pointer ---
    builder.switchToBlock(block_do_init);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const init_idx = func_index_map.get("sched_init") orelse 0;
        const init_sig = clif.Signature.init(.system_v);
        const init_sig_ref = try builder.importSignature(init_sig);
        const init_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = init_idx } },
            .signature = init_sig_ref,
            .colocated = true,
        });
        _ = try ins.call(init_ref, &[_]clif.Value{});
        // Reload sched_ptr after init
        const sched_ptr = try loadSchedPtr(&clif_func, &builder, sched_symbol_idx);
        _ = try ins.jump(block_post_init, &[_]clif.Value{sched_ptr});
    }

    // --- Post-init: allocate task, push to queue, signal ---
    _ = try builder.appendBlockParam(block_post_init, clif.Type.I64); // sched_ptr
    builder.switchToBlock(block_post_init);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const params = builder.blockParams(block_entry);
        const fn_ptr = params[0];
        const env_ptr = params[1];
        const sched_ptr = builder.blockParams(block_post_init)[0];

        // malloc(TASK_SIZE) for the task
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
        const v_task_size = try ins.iconst(clif.Type.I64, TASK_SIZE);
        const malloc_result = try ins.call(malloc_ref, &[_]clif.Value{v_task_size});
        const task_ptr = malloc_result.results[0];

        // Store fn_ptr, env_ptr, next=0
        _ = try ins.store(clif.MemFlags.DEFAULT, fn_ptr, task_ptr, TASK_FN_PTR);
        _ = try ins.store(clif.MemFlags.DEFAULT, env_ptr, task_ptr, TASK_ENV_PTR);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.store(clif.MemFlags.DEFAULT, v_zero, task_ptr, TASK_NEXT);

        // Increment active_tasks atomically
        const v_active_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_ACTIVE_TASKS));
        const active_addr = try ins.iadd(sched_ptr, v_active_off);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        _ = try ins.atomicRmwAdd(clif.Type.I64, active_addr, v_one);

        // Lock queue mutex
        const queue_mutex = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_MUTEX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_lock", queue_mutex);

        // Enqueue: push to tail of linked list
        const tail = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_TAIL);
        const has_tail = try ins.icmp(.ne, tail, v_zero);
        const tail_next_addr = try ins.iadd(tail, try ins.iconst(clif.Type.I64, @as(i64, TASK_NEXT)));
        const v_head_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_QUEUE_HEAD));
        const head_addr = try ins.iadd(sched_ptr, v_head_off);
        const store_addr = try ins.select(clif.Type.I64, has_tail, tail_next_addr, head_addr);
        _ = try ins.store(clif.MemFlags.DEFAULT, task_ptr, store_addr, 0);
        // Update tail = task
        _ = try ins.store(clif.MemFlags.DEFAULT, task_ptr, sched_ptr, SCHED_QUEUE_TAIL);
        // Increment size
        const old_size = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_SIZE);
        const new_size = try ins.iadd(old_size, v_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_size, sched_ptr, SCHED_QUEUE_SIZE);

        // Unlock queue mutex
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", queue_mutex);

        // Signal one idle worker
        const idle_cond = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_IDLE_COND);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_cond_signal", idle_cond);

        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// sched_init() -> void  [internal]
//
// Query CPU count, allocate scheduler, spawn worker threads.
// ============================================================================

fn generateSchedInit(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    sched_symbol_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();

        // Get CPU count via sysconf(_SC_NPROCESSORS_ONLN)
        const sysconf_idx = func_index_map.get("sysconf") orelse 0;
        var sysconf_sig = clif.Signature.init(.system_v);
        try sysconf_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try sysconf_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const sysconf_sig_ref = try builder.importSignature(sysconf_sig);
        const sysconf_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = sysconf_idx } },
            .signature = sysconf_sig_ref,
            .colocated = false,
        });
        const v_sc = try ins.iconst(clif.Type.I64, SC_NPROCESSORS_ONLN);
        const cpu_result = try ins.call(sysconf_ref, &[_]clif.Value{v_sc});
        const num_cpus = cpu_result.results[0];

        // malloc(SCHED_SIZE)
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
        const v_sched_size = try ins.iconst(clif.Type.I64, SCHED_SIZE);
        const sched_result = try ins.call(malloc_ref, &[_]clif.Value{v_sched_size});
        const sched_ptr = sched_result.results[0];

        // Zero-init
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try callLibc3(allocator, &builder, func_index_map, "memset", sched_ptr, v_zero, v_sched_size);

        // Store num_workers
        _ = try ins.store(clif.MemFlags.DEFAULT, num_cpus, sched_ptr, SCHED_NUM_WORKERS);

        // Store scheduler pointer in global BSS symbol
        try storeSchedPtr(&clif_func, &builder, sched_symbol_idx, sched_ptr);

        // Create queue mutex via mutex_init()
        const queue_mutex = try callRuntimeNoArgs(allocator, &builder, func_index_map, "mutex_init");
        _ = try ins.store(clif.MemFlags.DEFAULT, queue_mutex, sched_ptr, SCHED_QUEUE_MUTEX);

        // Create idle mutex and condvar
        const idle_mutex = try callRuntimeNoArgs(allocator, &builder, func_index_map, "mutex_init");
        _ = try ins.store(clif.MemFlags.DEFAULT, idle_mutex, sched_ptr, SCHED_IDLE_MUTEX);

        const idle_cond = try callRuntimeNoArgs(allocator, &builder, func_index_map, "cond_init");
        _ = try ins.store(clif.MemFlags.DEFAULT, idle_cond, sched_ptr, SCHED_IDLE_COND);

        // Allocate workers array: malloc(num_cpus * 8)
        const v_eight = try ins.iconst(clif.Type.I64, 8);
        const workers_size = try ins.imul(num_cpus, v_eight);
        const workers_result = try ins.call(malloc_ref, &[_]clif.Value{workers_size});
        _ = try ins.store(clif.MemFlags.DEFAULT, workers_result.results[0], sched_ptr, SCHED_WORKERS_PTR);

        // Spawn worker threads via sched_worker_spawn(sched_ptr, num_workers)
        const ws_idx = func_index_map.get("sched_worker_spawn") orelse 0;
        var ws_sig = clif.Signature.init(.system_v);
        try ws_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try ws_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const ws_sig_ref = try builder.importSignature(ws_sig);
        const ws_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = ws_idx } },
            .signature = ws_sig_ref,
            .colocated = true,
        });
        _ = try ins.call(ws_ref, &[_]clif.Value{ sched_ptr, num_cpus });

        // Set initialized = 1
        const v_one = try ins.iconst(clif.Type.I64, 1);
        _ = try ins.store(clif.MemFlags.DEFAULT, v_one, sched_ptr, SCHED_INITIALIZED);

        // Register sched_shutdown as atexit handler so it runs at process exit
        const shutdown_idx = func_index_map.get("sched_shutdown") orelse 0;
        const shutdown_sig = clif.Signature.init(.system_v);
        const shutdown_sig_ref = try builder.importSignature(shutdown_sig);
        const shutdown_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = shutdown_idx } },
            .signature = shutdown_sig_ref,
            .colocated = true,
        });
        const shutdown_addr = try ins.funcAddr(clif.Type.I64, shutdown_ref);
        _ = try callLibc1(allocator, &builder, func_index_map, "atexit", shutdown_addr);

        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// sched_worker_spawn(sched_ptr: i64, num_workers: i64) -> void  [internal]
//
// Loop: spawn num_workers threads, each running sched_worker_loop.
// ============================================================================

fn generateWorkerSpawn(
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

    // Signature: (sched_ptr: i64, num_workers: i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_check = try builder.createBlock();
    const block_body = try builder.createBlock();
    const block_done = try builder.createBlock();

    // --- Entry ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const v_zero = try builder.ins().iconst(clif.Type.I64, 0);
        _ = try builder.ins().jump(block_check, &[_]clif.Value{v_zero});
    }

    // --- Check: i < num_workers? ---
    _ = try builder.appendBlockParam(block_check, clif.Type.I64); // i
    builder.switchToBlock(block_check);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const num_workers = builder.blockParams(block_entry)[1];
        const i = builder.blockParams(block_check)[0];
        const done = try ins.icmp(.sge, i, num_workers);
        _ = try ins.brif(done, block_done, &.{}, block_body, &[_]clif.Value{i});
    }

    // --- Body: spawn thread i ---
    _ = try builder.appendBlockParam(block_body, clif.Type.I64); // i
    builder.switchToBlock(block_body);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_entry)[0];
        const i = builder.blockParams(block_body)[0];

        // Stack slot for pthread_t (8 bytes)
        const thread_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(8, 3));
        const thread_addr = try ins.stackAddr(clif.Type.I64, thread_slot, 0);

        // Get sched_worker_loop function address
        const loop_idx = func_index_map.get("sched_worker_loop") orelse 0;
        var loop_sig = clif.Signature.init(.system_v);
        try loop_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try loop_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const loop_sig_ref = try builder.importSignature(loop_sig);
        const loop_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = loop_idx } },
            .signature = loop_sig_ref,
            .colocated = true,
        });
        const fn_ptr = try ins.funcAddr(clif.Type.I64, loop_ref);

        // pthread_create(&thread_slot, NULL, fn_ptr, sched_ptr)
        const create_idx = func_index_map.get("pthread_create") orelse 0;
        var create_sig = clif.Signature.init(.system_v);
        try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try create_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        try create_sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
        const create_sig_ref = try builder.importSignature(create_sig);
        const create_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = create_idx } },
            .signature = create_sig_ref,
            .colocated = false,
        });
        const v_null = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.call(create_ref, &[_]clif.Value{ thread_addr, v_null, fn_ptr, sched_ptr });

        // Store thread handle: workers[i] = *thread_slot
        const handle = try ins.stackLoad(clif.Type.I64, thread_slot, 0);
        const workers_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_WORKERS_PTR);
        const v_eight = try ins.iconst(clif.Type.I64, 8);
        const offset = try ins.imul(i, v_eight);
        const slot_addr = try ins.iadd(workers_ptr, offset);
        _ = try ins.store(clif.MemFlags.DEFAULT, handle, slot_addr, 0);

        // i++, loop back
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const next_i = try ins.iadd(i, v_one);
        _ = try ins.jump(block_check, &[_]clif.Value{next_i});
    }

    // --- Done ---
    builder.switchToBlock(block_done);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// sched_worker_loop(sched_ptr: i64) -> i64  [internal, pthread start_routine]
//
// Main worker loop: pop task from global queue, execute, repeat until shutdown.
// Reference: Go schedule() + findRunnable() (proc.go:4135, 3389)
// ============================================================================

fn generateWorkerLoop(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    sched_symbol_idx: u32,
) !native_compile.CompiledCode {
    _ = sched_symbol_idx; // Worker receives sched_ptr as argument, doesn't need global

    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (sched_ptr: i64) -> i64 (pthread start_routine returns void*)
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_loop = try builder.createBlock();
    const block_try_dequeue = try builder.createBlock();
    const block_got_task = try builder.createBlock();
    const block_no_task = try builder.createBlock();
    const block_exit = try builder.createBlock();

    // --- Entry ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().jump(block_loop, &.{});

    // --- Loop: check shutdown ---
    builder.switchToBlock(block_loop);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_entry)[0];
        const v_shutdown_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_SHUTDOWN));
        const shutdown_addr = try ins.iadd(sched_ptr, v_shutdown_off);
        const shutdown = try ins.atomicLoad(clif.Type.I64, shutdown_addr);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_shutdown = try ins.icmp(.ne, shutdown, v_zero);
        _ = try ins.brif(is_shutdown, block_exit, &.{}, block_try_dequeue, &.{});
    }

    // --- Try dequeue: lock, pop head ---
    builder.switchToBlock(block_try_dequeue);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_entry)[0];

        const queue_mutex = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_MUTEX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_lock", queue_mutex);

        const head = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_HEAD);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const has_task = try ins.icmp(.ne, head, v_zero);
        _ = try ins.brif(has_task, block_got_task, &[_]clif.Value{ head, queue_mutex }, block_no_task, &[_]clif.Value{queue_mutex});
    }

    // --- Got task: dequeue, unlock, execute ---
    _ = try builder.appendBlockParam(block_got_task, clif.Type.I64); // task_ptr
    _ = try builder.appendBlockParam(block_got_task, clif.Type.I64); // queue_mutex
    builder.switchToBlock(block_got_task);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_entry)[0];
        const task_ptr = builder.blockParams(block_got_task)[0];
        const queue_mutex = builder.blockParams(block_got_task)[1];

        // Update head = task.next
        const next = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, task_ptr, TASK_NEXT);
        _ = try ins.store(clif.MemFlags.DEFAULT, next, sched_ptr, SCHED_QUEUE_HEAD);
        // If next == 0, clear tail too
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const next_is_null = try ins.icmp(.eq, next, v_zero);
        const current_tail = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_TAIL);
        const new_tail = try ins.select(clif.Type.I64, next_is_null, v_zero, current_tail);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_tail, sched_ptr, SCHED_QUEUE_TAIL);

        // Unlock
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", queue_mutex);

        // Execute: call_indirect(fn_ptr, env_ptr)
        const fn_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, task_ptr, TASK_FN_PTR);
        const env_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, task_ptr, TASK_ENV_PTR);
        var task_sig = clif.Signature.init(.system_v);
        try task_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const task_sig_ref = try builder.importSignature(task_sig);
        _ = try ins.callIndirect(task_sig_ref, fn_ptr, &[_]clif.Value{env_ptr});

        // Free task
        const free_idx = func_index_map.get("free") orelse 0;
        var free_sig = clif.Signature.init(.system_v);
        try free_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const free_sig_ref = try builder.importSignature(free_sig);
        const free_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = free_idx } },
            .signature = free_sig_ref,
            .colocated = false,
        });
        _ = try ins.call(free_ref, &[_]clif.Value{task_ptr});

        // Decrement active_tasks
        const v_active_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_ACTIVE_TASKS));
        const active_addr = try ins.iadd(sched_ptr, v_active_off);
        const v_neg_one = try ins.iconst(clif.Type.I64, -1);
        _ = try ins.atomicRmwAdd(clif.Type.I64, active_addr, v_neg_one);

        // Signal condvar (in case shutdown is waiting)
        const idle_cond = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_IDLE_COND);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_cond_broadcast", idle_cond);

        _ = try ins.jump(block_loop, &.{});
    }

    // --- No task: unlock, sleep briefly, retry ---
    _ = try builder.appendBlockParam(block_no_task, clif.Type.I64); // queue_mutex
    builder.switchToBlock(block_no_task);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const queue_mutex = builder.blockParams(block_no_task)[0];

        // Unlock queue mutex
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", queue_mutex);

        // Brief sleep to avoid busy-spinning (100 microseconds)
        const v_sleep = try ins.iconst(clif.Type.I64, 100);
        _ = try callLibc1(allocator, &builder, func_index_map, "usleep", v_sleep);

        _ = try ins.jump(block_loop, &.{});
    }

    // --- Exit: return 0 ---
    builder.switchToBlock(block_exit);
    try builder.ensureInsertedBlock();
    {
        const v_zero = try builder.ins().iconst(clif.Type.I64, 0);
        _ = try builder.ins().return_(&[_]clif.Value{v_zero});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// sched_shutdown() -> void
// ============================================================================

fn generateSchedShutdown(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    sched_symbol_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    const block_entry = try builder.createBlock();
    const block_return = try builder.createBlock();
    const block_wait_loop = try builder.createBlock();
    const block_join = try builder.createBlock();

    // --- Entry: check if initialized ---
    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = try loadSchedPtr(&clif_func, &builder, sched_symbol_idx);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, sched_ptr, v_zero);
        _ = try ins.brif(is_null, block_return, &.{}, block_wait_loop, &[_]clif.Value{sched_ptr});
    }

    // --- Return (no-op) ---
    builder.switchToBlock(block_return);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    // --- Wait loop: set shutdown, spin until active_tasks == 0 ---
    _ = try builder.appendBlockParam(block_wait_loop, clif.Type.I64); // sched_ptr
    builder.switchToBlock(block_wait_loop);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_wait_loop)[0];

        // Set shutdown = 1 (atomic store)
        const v_shutdown_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_SHUTDOWN));
        const shutdown_addr = try ins.iadd(sched_ptr, v_shutdown_off);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        _ = try ins.atomicStore(v_one, shutdown_addr);

        // Brief sleep to let workers notice the shutdown flag
        const v_sleep = try ins.iconst(clif.Type.I64, 1000);
        _ = try callLibc1(allocator, &builder, func_index_map, "usleep", v_sleep);

        // Check active_tasks
        const v_active_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_ACTIVE_TASKS));
        const active_addr = try ins.iadd(sched_ptr, v_active_off);
        const active = try ins.atomicLoad(clif.Type.I64, active_addr);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const all_done = try ins.icmp(.eq, active, v_zero);
        _ = try ins.brif(all_done, block_join, &[_]clif.Value{sched_ptr}, block_wait_loop, &[_]clif.Value{sched_ptr});
    }

    // --- Join all workers ---
    _ = try builder.appendBlockParam(block_join, clif.Type.I64); // sched_ptr
    builder.switchToBlock(block_join);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_join)[0];

        // Join workers
        const join_idx = func_index_map.get("sched_join_workers") orelse 0;
        var join_sig = clif.Signature.init(.system_v);
        try join_sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
        const join_sig_ref = try builder.importSignature(join_sig);
        const join_ref = try builder.importFunction(.{
            .name = .{ .user = .{ .namespace = 0, .index = join_idx } },
            .signature = join_sig_ref,
            .colocated = true,
        });
        _ = try ins.call(join_ref, &[_]clif.Value{sched_ptr});

        _ = try ins.return_(&[_]clif.Value{});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// sched_join_workers(sched_ptr: i64) -> void  [internal]
// ============================================================================

fn generateJoinWorkers(
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

    // Signature: (sched_ptr: i64) -> void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_check = try builder.createBlock();
    const block_body = try builder.createBlock();
    const block_done = try builder.createBlock();

    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const v_zero = try builder.ins().iconst(clif.Type.I64, 0);
        _ = try builder.ins().jump(block_check, &[_]clif.Value{v_zero});
    }

    _ = try builder.appendBlockParam(block_check, clif.Type.I64); // i
    builder.switchToBlock(block_check);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_entry)[0];
        const i = builder.blockParams(block_check)[0];
        const num_workers = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_NUM_WORKERS);
        const done = try ins.icmp(.sge, i, num_workers);
        _ = try ins.brif(done, block_done, &.{}, block_body, &[_]clif.Value{i});
    }

    _ = try builder.appendBlockParam(block_body, clif.Type.I64); // i
    builder.switchToBlock(block_body);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_entry)[0];
        const i = builder.blockParams(block_body)[0];

        const workers_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_WORKERS_PTR);
        const v_eight = try ins.iconst(clif.Type.I64, 8);
        const offset = try ins.imul(i, v_eight);
        const thread_addr = try ins.iadd(workers_ptr, offset);
        const thread_handle = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, thread_addr, 0);

        _ = try callLibc2(allocator, &builder, func_index_map, "pthread_join", thread_handle, try ins.iconst(clif.Type.I64, 0));

        const v_one = try ins.iconst(clif.Type.I64, 1);
        const next_i = try ins.iadd(i, v_one);
        _ = try ins.jump(block_check, &[_]clif.Value{next_i});
    }

    builder.switchToBlock(block_done);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// sched_get_num_workers() -> i64
// ============================================================================

fn generateSchedGetNumWorkers(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    sched_symbol_idx: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: () -> i64
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_return_zero = try builder.createBlock();
    const block_return_count = try builder.createBlock();

    builder.switchToBlock(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = try loadSchedPtr(&clif_func, &builder, sched_symbol_idx);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_null = try ins.icmp(.eq, sched_ptr, v_zero);
        _ = try ins.brif(is_null, block_return_zero, &.{}, block_return_count, &[_]clif.Value{sched_ptr});
    }

    builder.switchToBlock(block_return_zero);
    try builder.ensureInsertedBlock();
    {
        const v_zero = try builder.ins().iconst(clif.Type.I64, 0);
        _ = try builder.ins().return_(&[_]clif.Value{v_zero});
    }

    _ = try builder.appendBlockParam(block_return_count, clif.Type.I64);
    builder.switchToBlock(block_return_count);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const sched_ptr = builder.blockParams(block_return_count)[0];
        const num_workers = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_NUM_WORKERS);
        _ = try ins.return_(&[_]clif.Value{num_workers});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Helper: call a libc function with 1 arg, returns i64
// ============================================================================

fn callLibc1(
    allocator: Allocator,
    builder: *FunctionBuilder,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    name: []const u8,
    arg: clif.Value,
) !clif.Value {
    const idx = func_index_map.get(name) orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const result = try builder.ins().call(func_ref, &[_]clif.Value{arg});
    return result.results[0];
}

fn callLibc2(
    allocator: Allocator,
    builder: *FunctionBuilder,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    name: []const u8,
    arg1: clif.Value,
    arg2: clif.Value,
) !clif.Value {
    const idx = func_index_map.get(name) orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const result = try builder.ins().call(func_ref, &[_]clif.Value{ arg1, arg2 });
    return result.results[0];
}

fn callLibc3(
    allocator: Allocator,
    builder: *FunctionBuilder,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    name: []const u8,
    arg1: clif.Value,
    arg2: clif.Value,
    arg3: clif.Value,
) !clif.Value {
    const idx = func_index_map.get(name) orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = idx } },
        .signature = sig_ref,
        .colocated = false,
    });
    const result = try builder.ins().call(func_ref, &[_]clif.Value{ arg1, arg2, arg3 });
    return result.results[0];
}

fn callRuntimeNoArgs(
    allocator: Allocator,
    builder: *FunctionBuilder,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    name: []const u8,
) !clif.Value {
    const idx = func_index_map.get(name) orelse 0;
    var sig = clif.Signature.init(.system_v);
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    const func_ref = try builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = idx } },
        .signature = sig_ref,
        .colocated = true,
    });
    const result = try builder.ins().call(func_ref, &[_]clif.Value{});
    return result.results[0];
}
