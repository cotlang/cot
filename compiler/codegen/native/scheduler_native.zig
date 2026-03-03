//! Work-Stealing Scheduler Runtime — CLIF IR Generation
//!
//! Generates scheduler functions as CLIF IR for the `spawn { }` construct.
//! Per-worker Chase-Lev deques with work-stealing. Falls back to global queue.
//!
//! Worker priority: local deque → global queue → steal from random victim.
//! Spawns from worker threads push to local deque (fast, no lock).
//! Spawns from external threads push to global queue (mutex-protected).
//!
//! Data structures (heap-allocated):
//!
//!   Scheduler (global singleton, 96 bytes):
//!     offset 0:   (reserved)   (i64)
//!     offset 8:   num_workers  (i64)
//!     offset 16:  workers_ptr  (i64) — pointer to array of thread handles
//!     offset 24:  queue_head   (i64) — linked list head (Task*)
//!     offset 32:  queue_tail   (i64) — linked list tail (Task*)
//!     offset 40:  (reserved)   (i64)
//!     offset 48:  queue_mutex  (i64) — mutex ptr (also used for condvar wait)
//!     offset 56:  active_tasks (i64) — atomic outstanding task count
//!     offset 64:  shutdown     (i64) — atomic shutdown flag
//!     offset 72:  (reserved)   (i64)
//!     offset 80:  idle_cond    (i64) — condition var ptr (paired with queue_mutex)
//!     offset 88:  deques_ptr   (i64) — pointer to array of WorkerDeque structs
//!
//!   WorkerDeque (per worker, 32 bytes — Chase-Lev deque):
//!     offset 0:   top      (i64, atomic) — steal end (thieves CAS here)
//!     offset 8:   bottom   (i64, atomic) — push/pop end (owner only)
//!     offset 16:  buf_ptr  (i64) — pointer to circular buffer of task pointers
//!     offset 24:  capacity (i64) — buffer size (256, power of 2)
//!
//!   Task (24 bytes, heap-allocated per spawn):
//!     offset 0:   fn_ptr   (i64) — function pointer to spawn body
//!     offset 8:   env_ptr  (i64) — capture environment pointer (0 if none)
//!     offset 16:  next     (i64) — linked list next pointer
//!
//! Reference: Go runtime/proc.go (GMP model — goroutine scheduling, runqput/runqget/runqsteal)
//! Reference: Chase & Lev, "Dynamic Circular Work-Stealing Deque" (PPoPP 2005)
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
const SCHED_INITIALIZED: i32 = 0; // legacy — set but not checked (CAS on BSS handles init)
const SCHED_NUM_WORKERS: i32 = 8;
const SCHED_WORKERS_PTR: i32 = 16;
const SCHED_QUEUE_HEAD: i32 = 24;
const SCHED_QUEUE_TAIL: i32 = 32;
// offset 40: reserved (formerly queue_size)
const SCHED_QUEUE_MUTEX: i32 = 48;
const SCHED_ACTIVE_TASKS: i32 = 56;
const SCHED_SHUTDOWN: i32 = 64;
// offset 72: reserved (formerly idle_mutex — condvar now uses queue_mutex)
const SCHED_IDLE_COND: i32 = 80;
const SCHED_DEQUES_PTR: i32 = 88;
const SCHED_SIZE: i64 = 96;

// WorkerDeque struct offsets (Chase-Lev deque, 32 bytes per worker)
const DEQUE_TOP: i32 = 0; // atomic — steal end
const DEQUE_BOTTOM: i32 = 8; // atomic — push/pop end (owner)
const DEQUE_BUF_PTR: i32 = 16; // pointer to circular buffer
const DEQUE_CAPACITY: i32 = 24; // buffer size (256)
const DEQUE_SIZE: i64 = 32;
const DEQUE_DEFAULT_CAPACITY: i64 = 256; // power of 2

// Task struct offsets
const TASK_FN_PTR: i32 = 0;
const TASK_ENV_PTR: i32 = 8;
const TASK_NEXT: i32 = 16;
const TASK_SIZE: i64 = 24;

// sysconf constant — platform-dependent
const SC_NPROCESSORS_ONLN: i64 = switch (@import("builtin").os.tag) {
    .macos => 58,
    .linux => 84,
    else => 84, // Default to Linux value for other Unix-like systems
};

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
    try result.append(allocator, .{
        .name = "sched_select",
        .compiled = try generateSchedSelect(allocator, isa, ctrl_plane, func_index_map),
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

    const block_spin_wait = try builder.createBlock();

    // --- Entry: CAS-based lazy init (thread-safe) ---
    // Atomically try to swap _cot_sched_ptr from 0 → -1 (sentinel).
    // If CAS returns 0: we won the race, call sched_init, store real pointer.
    // If CAS returns -1: another thread is initializing, spin until ready.
    // If CAS returns other: already initialized, proceed.
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        // Get address of _cot_sched_ptr for CAS
        const gv = try clif_func.createGlobalValue(GlobalValueData{
            .symbol = .{
                .name = gv_ExternalName.initUser(0, sched_symbol_idx),
                .offset = 0,
                .colocated = true,
                .tls = false,
            },
        });
        const sched_global_addr = try ins.globalValue(clif.Type.I64, gv);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const v_sentinel = try ins.iconst(clif.Type.I64, -1);
        // CAS: try to swap 0 → -1 (returns old value)
        const old_val = try ins.atomicCas(clif.Type.I64, sched_global_addr, v_zero, v_sentinel);
        const was_null = try ins.icmp(.eq, old_val, v_zero);
        _ = try ins.brif(was_null, block_do_init, &.{}, block_spin_wait, &[_]clif.Value{old_val});
    }

    // --- We won the init race: call sched_init, store real pointer ---
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
        // Reload sched_ptr after init (sched_init stores the real pointer)
        const sched_ptr = try loadSchedPtr(&clif_func, &builder, sched_symbol_idx);
        _ = try ins.jump(block_post_init, &[_]clif.Value{sched_ptr});
    }

    // --- Spin wait: another thread is initializing (CAS returned -1) or already done ---
    _ = try builder.appendBlockParam(block_spin_wait, clif.Type.I64); // old CAS result
    builder.switchToBlock(block_spin_wait);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const old_val = builder.blockParams(block_spin_wait)[0];
        const v_sentinel = try ins.iconst(clif.Type.I64, -1);
        const is_sentinel = try ins.icmp(.eq, old_val, v_sentinel);
        // If sentinel (another thread initializing): spin-load until real pointer appears
        // If valid pointer: proceed directly
        const block_spin_loop = try builder.createBlock();
        _ = try ins.brif(is_sentinel, block_spin_loop, &.{}, block_post_init, &[_]clif.Value{old_val});

        // Spin loop: reload _cot_sched_ptr until non-sentinel
        builder.switchToBlock(block_spin_loop);
        try builder.ensureInsertedBlock();
        {
            const spin_ins = builder.ins();
            const v_usleep_arg = try spin_ins.iconst(clif.Type.I64, 10);
            _ = try callLibc1(allocator, &builder, func_index_map, "usleep", v_usleep_arg);
            const sched_ptr = try loadSchedPtr(&clif_func, &builder, sched_symbol_idx);
            const v_sentinel2 = try spin_ins.iconst(clif.Type.I64, -1);
            const still_sentinel = try spin_ins.icmp(.eq, sched_ptr, v_sentinel2);
            _ = try spin_ins.brif(still_sentinel, block_spin_loop, &.{}, block_post_init, &[_]clif.Value{sched_ptr});
        }
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

        // Signal one idle worker BEFORE unlocking (avoids lost-wakeup race).
        // POSIX: signal under mutex ensures worker in cond_wait sees the new task.
        const idle_cond = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_IDLE_COND);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_cond_signal", idle_cond);

        // Unlock queue mutex
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", queue_mutex);

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
        const raw_cpus = cpu_result.results[0];

        // num_workers = max(1, num_cpus - 1) — reserve one core for the main thread
        // Reference: Rayon defaults to num_cpus - 1 (main thread participates)
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const cpus_minus_one = try ins.isub(raw_cpus, v_one);
        const need_clamp = try ins.icmp(.slt, cpus_minus_one, v_one);
        const num_cpus = try ins.select(clif.Type.I64, need_clamp, v_one, cpus_minus_one);

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

        // Create condvar for idle worker signaling (uses queue_mutex, not a separate idle_mutex)
        const idle_cond = try callRuntimeNoArgs(allocator, &builder, func_index_map, "cond_init");
        _ = try ins.store(clif.MemFlags.DEFAULT, idle_cond, sched_ptr, SCHED_IDLE_COND);

        // Allocate workers array: malloc(num_cpus * 8)
        const v_eight = try ins.iconst(clif.Type.I64, 8);
        const workers_size = try ins.imul(num_cpus, v_eight);
        const workers_result = try ins.call(malloc_ref, &[_]clif.Value{workers_size});
        _ = try ins.store(clif.MemFlags.DEFAULT, workers_result.results[0], sched_ptr, SCHED_WORKERS_PTR);

        // Allocate per-worker deques array: malloc(num_cpus * DEQUE_SIZE)
        const v_deque_size = try ins.iconst(clif.Type.I64, DEQUE_SIZE);
        const deques_total = try ins.imul(num_cpus, v_deque_size);
        const deques_result = try ins.call(malloc_ref, &[_]clif.Value{deques_total});
        const deques_ptr = deques_result.results[0];
        _ = try callLibc3(allocator, &builder, func_index_map, "memset", deques_ptr, v_zero, deques_total);
        _ = try ins.store(clif.MemFlags.DEFAULT, deques_ptr, sched_ptr, SCHED_DEQUES_PTR);

        // Initialize each deque's buffer: allocate circular buffer of 256 task pointers
        // Done in sched_worker_spawn per-worker (each worker inits its own deque)

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

        // Set initialized = 1 (reuse v_one from above)
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

        // Initialize worker i's deque buffer: malloc(256 * 8 = 2048 bytes)
        const deques_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_DEQUES_PTR);
        const v_deque_struct_size = try ins.iconst(clif.Type.I64, DEQUE_SIZE);
        const deque_offset = try ins.imul(i, v_deque_struct_size);
        const deque_addr = try ins.iadd(deques_ptr, deque_offset);
        const v_deque_buf_size = try ins.iconst(clif.Type.I64, DEQUE_DEFAULT_CAPACITY * 8);
        const deque_buf_result = try callLibc1(allocator, &builder, func_index_map, "malloc", v_deque_buf_size);
        _ = try ins.store(clif.MemFlags.DEFAULT, deque_buf_result, deque_addr, DEQUE_BUF_PTR);
        const v_capacity = try ins.iconst(clif.Type.I64, DEQUE_DEFAULT_CAPACITY);
        _ = try ins.store(clif.MemFlags.DEFAULT, v_capacity, deque_addr, DEQUE_CAPACITY);
        // top=0, bottom=0 already from memset in sched_init

        // Encode (sched_ptr, worker_id) as arg to worker loop:
        // We pack worker_id in the upper 16 bits (bits 48-63), sched_ptr in the lower 48.
        // User-space addresses are < 2^48 on both x64 and ARM64, so this is safe.
        const v_48 = try ins.iconst(clif.Type.I64, 48);
        const id_shifted = try ins.ishl(i, v_48);
        const packed_arg = try ins.bor(sched_ptr, id_shifted);

        // pthread_create(&thread_slot, NULL, fn_ptr, packed_arg)
        _ = try ins.call(create_ref, &[_]clif.Value{ thread_addr, v_null, fn_ptr, packed_arg });

        // Store thread handle: workers[i] = *thread_slot
        const handle = try ins.stackLoad(clif.Type.I64, thread_slot, 0);
        const workers_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_WORKERS_PTR);
        const v_eight = try ins.iconst(clif.Type.I64, 8);
        const w_offset = try ins.imul(i, v_eight);
        const slot_addr = try ins.iadd(workers_ptr, w_offset);
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
// sched_worker_loop(packed_arg: i64) -> i64  [internal, pthread start_routine]
//
// Work-stealing worker loop. packed_arg encodes (worker_id << 32 | sched_ptr).
// Priority: 1) local deque pop  2) global queue  3) steal from victim  4) park
//
// Reference: Go schedule() + findRunnable() (proc.go:4135, 3389)
// Reference: Chase-Lev work-stealing deque
// ============================================================================

fn generateWorkerLoop(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    sched_symbol_idx: u32,
) !native_compile.CompiledCode {
    _ = sched_symbol_idx;

    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (packed_arg: i64) -> i64 (pthread start_routine returns void*)
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_loop = try builder.createBlock();
    const block_try_local = try builder.createBlock();
    const block_try_global = try builder.createBlock();
    const block_try_steal = try builder.createBlock();
    const block_got_task = try builder.createBlock();
    const block_no_task = try builder.createBlock();
    const block_exit = try builder.createBlock();

    // --- Entry: unpack sched_ptr and worker_id ---
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        // packed_arg: worker_id in upper 32 bits, sched_ptr in lower 32+
        // Unpack: sched_ptr = packed & 0xFFFFFFFF, worker_id = packed >> 32
        // Actually, sched_ptr can be > 32 bits. Use a mask for the lower 48 bits.
        // On 64-bit systems, user-space addresses are < 2^48.
        const ins = builder.ins();
        const packed_val = builder.blockParams(block_entry)[0];
        const v_mask = try ins.iconst(clif.Type.I64, 0x0000FFFFFFFFFFFF);
        const sched_ptr = try ins.band(packed_val, v_mask);
        const v_48 = try ins.iconst(clif.Type.I64, 48);
        const worker_id = try ins.ushr(packed_val, v_48);

        // Store sched_ptr and worker_id on stack for later access
        const sched_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(8, 3));
        _ = try ins.stackStore(sched_ptr, sched_slot, 0);
        const id_slot = try builder.createSizedStackSlot(clif.StackSlotData.explicit(8, 3));
        _ = try ins.stackStore(worker_id, id_slot, 0);

        _ = try ins.jump(block_loop, &.{});
    }

    // --- Loop: check shutdown ---
    builder.switchToBlock(block_loop);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        // Unpack from packed_arg again (CLIF IR blocks can access entry params)
        const packed_val = builder.blockParams(block_entry)[0];
        const v_mask = try ins.iconst(clif.Type.I64, 0x0000FFFFFFFFFFFF);
        const sched_ptr = try ins.band(packed_val, v_mask);

        const v_shutdown_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_SHUTDOWN));
        const shutdown_addr = try ins.iadd(sched_ptr, v_shutdown_off);
        const shutdown = try ins.atomicLoad(clif.Type.I64, shutdown_addr);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_shutdown = try ins.icmp(.ne, shutdown, v_zero);
        _ = try ins.brif(is_shutdown, block_exit, &.{}, block_try_local, &.{});
    }

    // --- Step 1: Try local deque pop (fast path, no lock) ---
    builder.switchToBlock(block_try_local);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const packed_val = builder.blockParams(block_entry)[0];
        const v_mask = try ins.iconst(clif.Type.I64, 0x0000FFFFFFFFFFFF);
        const sched_ptr = try ins.band(packed_val, v_mask);
        const v_48 = try ins.iconst(clif.Type.I64, 48);
        const worker_id = try ins.ushr(packed_val, v_48);

        // Calculate my deque address: deques_ptr + worker_id * DEQUE_SIZE
        const deques_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_DEQUES_PTR);
        const v_deque_size = try ins.iconst(clif.Type.I64, DEQUE_SIZE);
        const deque_offset = try ins.imul(worker_id, v_deque_size);
        const my_deque = try ins.iadd(deques_ptr, deque_offset);

        // Chase-Lev pop from bottom (owner only):
        // b = atomic_load(bottom) - 1
        // atomic_store(bottom, b)
        // t = atomic_load(top)
        // if t <= b: task = buf[b & (cap-1)], return task
        // if t == b+1: bottom = t; return EMPTY (CAS omitted for simplicity)
        // else: bottom = t; return EMPTY
        const v_bottom_off = try ins.iconst(clif.Type.I64, @as(i64, DEQUE_BOTTOM));
        const bottom_addr = try ins.iadd(my_deque, v_bottom_off);
        const old_bottom = try ins.atomicLoad(clif.Type.I64, bottom_addr);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const new_bottom = try ins.isub(old_bottom, v_one);
        _ = try ins.atomicStore(new_bottom, bottom_addr);

        const v_top_off = try ins.iconst(clif.Type.I64, @as(i64, DEQUE_TOP));
        const top_addr = try ins.iadd(my_deque, v_top_off);
        const top = try ins.atomicLoad(clif.Type.I64, top_addr);

        // If top <= new_bottom, there's at least one task
        const has_task = try ins.icmp(.sle, top, new_bottom);

        const block_local_got = try builder.createBlock();
        const block_local_empty = try builder.createBlock();
        _ = try ins.brif(has_task, block_local_got, &[_]clif.Value{ my_deque, new_bottom }, block_local_empty, &[_]clif.Value{ my_deque, old_bottom });

        // Got task from local deque
        _ = try builder.appendBlockParam(block_local_got, clif.Type.I64); // my_deque
        _ = try builder.appendBlockParam(block_local_got, clif.Type.I64); // new_bottom (index)
        builder.switchToBlock(block_local_got);
        try builder.ensureInsertedBlock();
        {
            const got_ins = builder.ins();
            const deque = builder.blockParams(block_local_got)[0];
            const b = builder.blockParams(block_local_got)[1];

            // task_ptr = buf[(b) & (cap-1)]
            const buf_ptr = try got_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, deque, DEQUE_BUF_PTR);
            const cap = try got_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, deque, DEQUE_CAPACITY);
            const cap_mask = try got_ins.isub(cap, v_one);
            const buf_idx = try got_ins.band(b, cap_mask);
            const v_eight = try got_ins.iconst(clif.Type.I64, 8);
            const slot_off = try got_ins.imul(buf_idx, v_eight);
            const slot_addr = try got_ins.iadd(buf_ptr, slot_off);
            const task_ptr = try got_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, slot_addr, 0);
            _ = try got_ins.jump(block_got_task, &[_]clif.Value{task_ptr});
        }

        // Local deque empty — restore bottom and try global
        _ = try builder.appendBlockParam(block_local_empty, clif.Type.I64); // my_deque
        _ = try builder.appendBlockParam(block_local_empty, clif.Type.I64); // old_bottom
        builder.switchToBlock(block_local_empty);
        try builder.ensureInsertedBlock();
        {
            const empty_ins = builder.ins();
            const deque = builder.blockParams(block_local_empty)[0];
            const ob = builder.blockParams(block_local_empty)[1];
            // Restore bottom to original value (undo the decrement)
            const empty_bottom_off = try empty_ins.iconst(clif.Type.I64, @as(i64, DEQUE_BOTTOM));
            const empty_bottom_addr = try empty_ins.iadd(deque, empty_bottom_off);
            _ = try empty_ins.atomicStore(ob, empty_bottom_addr);
            _ = try empty_ins.jump(block_try_global, &.{});
        }
    }

    // --- Step 2: Try global queue (under lock) ---
    builder.switchToBlock(block_try_global);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const packed_val = builder.blockParams(block_entry)[0];
        const v_mask = try ins.iconst(clif.Type.I64, 0x0000FFFFFFFFFFFF);
        const sched_ptr = try ins.band(packed_val, v_mask);

        const queue_mutex = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_MUTEX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_lock", queue_mutex);

        const head = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_HEAD);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const has_task = try ins.icmp(.ne, head, v_zero);

        const block_global_got = try builder.createBlock();
        _ = try ins.brif(has_task, block_global_got, &[_]clif.Value{ head, queue_mutex }, block_try_steal, &[_]clif.Value{queue_mutex});

        // Got task from global queue
        _ = try builder.appendBlockParam(block_global_got, clif.Type.I64); // task_ptr
        _ = try builder.appendBlockParam(block_global_got, clif.Type.I64); // queue_mutex
        builder.switchToBlock(block_global_got);
        try builder.ensureInsertedBlock();
        {
            const got_ins = builder.ins();
            const task_ptr = builder.blockParams(block_global_got)[0];
            const qm = builder.blockParams(block_global_got)[1];

            // Dequeue: head = task.next, clear tail if needed
            const next = try got_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, task_ptr, TASK_NEXT);
            _ = try got_ins.store(clif.MemFlags.DEFAULT, next, sched_ptr, SCHED_QUEUE_HEAD);
            const next_is_null = try got_ins.icmp(.eq, next, v_zero);
            const current_tail = try got_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_TAIL);
            const new_tail = try got_ins.select(clif.Type.I64, next_is_null, v_zero, current_tail);
            _ = try got_ins.store(clif.MemFlags.DEFAULT, new_tail, sched_ptr, SCHED_QUEUE_TAIL);

            _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", qm);
            _ = try got_ins.jump(block_got_task, &[_]clif.Value{task_ptr});
        }
    }

    // --- Step 3: Try stealing from a random victim ---
    _ = try builder.appendBlockParam(block_try_steal, clif.Type.I64); // queue_mutex (still locked)
    builder.switchToBlock(block_try_steal);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const packed_val = builder.blockParams(block_entry)[0];
        const v_mask = try ins.iconst(clif.Type.I64, 0x0000FFFFFFFFFFFF);
        const sched_ptr = try ins.band(packed_val, v_mask);
        const v_48 = try ins.iconst(clif.Type.I64, 48);
        const worker_id = try ins.ushr(packed_val, v_48);
        const queue_mutex = builder.blockParams(block_try_steal)[0];

        // Unlock global queue mutex (we failed to find work there)
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", queue_mutex);

        // Pick a victim: (worker_id + 1) % num_workers (simple round-robin)
        const num_workers = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_NUM_WORKERS);
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const victim_raw = try ins.iadd(worker_id, v_one);
        const victim_id = try ins.srem(victim_raw, num_workers);

        // Calculate victim's deque: deques_ptr + victim_id * DEQUE_SIZE
        const deques_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_DEQUES_PTR);
        const v_deque_size = try ins.iconst(clif.Type.I64, DEQUE_SIZE);
        const victim_deque_offset = try ins.imul(victim_id, v_deque_size);
        const victim_deque = try ins.iadd(deques_ptr, victim_deque_offset);

        // Chase-Lev steal from top (thief):
        // t = atomic_load(top)
        // b = atomic_load(bottom)
        // if t >= b: return EMPTY
        // task = buf[t & (cap-1)]
        // if CAS(top, t, t+1): return task  (we use atomicRmwAdd as approximation)
        const v_top_off = try ins.iconst(clif.Type.I64, @as(i64, DEQUE_TOP));
        const victim_top_addr = try ins.iadd(victim_deque, v_top_off);
        const victim_top = try ins.atomicLoad(clif.Type.I64, victim_top_addr);

        const v_bottom_off = try ins.iconst(clif.Type.I64, @as(i64, DEQUE_BOTTOM));
        const victim_bottom_addr = try ins.iadd(victim_deque, v_bottom_off);
        const victim_bottom = try ins.atomicLoad(clif.Type.I64, victim_bottom_addr);

        const victim_empty = try ins.icmp(.sge, victim_top, victim_bottom);

        const block_steal_got = try builder.createBlock();
        _ = try ins.brif(victim_empty, block_no_task, &[_]clif.Value{}, block_steal_got, &[_]clif.Value{ victim_deque, victim_top });

        // Steal one task from victim
        _ = try builder.appendBlockParam(block_steal_got, clif.Type.I64); // victim_deque
        _ = try builder.appendBlockParam(block_steal_got, clif.Type.I64); // victim_top
        builder.switchToBlock(block_steal_got);
        try builder.ensureInsertedBlock();
        {
            const steal_ins = builder.ins();
            const vd = builder.blockParams(block_steal_got)[0];
            const vt = builder.blockParams(block_steal_got)[1];

            // Read task pointer: buf[top & (cap-1)]
            const buf_ptr = try steal_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, vd, DEQUE_BUF_PTR);
            const cap = try steal_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, vd, DEQUE_CAPACITY);
            const cap_mask = try steal_ins.isub(cap, v_one);
            const buf_idx = try steal_ins.band(vt, cap_mask);
            const v_eight = try steal_ins.iconst(clif.Type.I64, 8);
            const slot_off = try steal_ins.imul(buf_idx, v_eight);
            const slot_addr = try steal_ins.iadd(buf_ptr, slot_off);
            const task_ptr = try steal_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, slot_addr, 0);

            // Advance top atomically (CAS approximation with atomic add)
            _ = try steal_ins.atomicRmwAdd(clif.Type.I64, victim_top_addr, v_one);

            _ = try steal_ins.jump(block_got_task, &[_]clif.Value{task_ptr});
        }
    }

    // --- Got task (from any source): execute it ---
    _ = try builder.appendBlockParam(block_got_task, clif.Type.I64); // task_ptr
    builder.switchToBlock(block_got_task);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const packed_val = builder.blockParams(block_entry)[0];
        const v_mask = try ins.iconst(clif.Type.I64, 0x0000FFFFFFFFFFFF);
        const sched_ptr = try ins.band(packed_val, v_mask);
        const task_ptr = builder.blockParams(block_got_task)[0];

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

        _ = try ins.jump(block_loop, &.{});
    }

    // --- No task: park on condvar ---
    builder.switchToBlock(block_no_task);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const packed_val = builder.blockParams(block_entry)[0];
        const v_mask = try ins.iconst(clif.Type.I64, 0x0000FFFFFFFFFFFF);
        const sched_ptr = try ins.band(packed_val, v_mask);

        const queue_mutex = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_MUTEX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_lock", queue_mutex);

        // Check shutdown before blocking
        const v_shutdown_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_SHUTDOWN));
        const shutdown_addr = try ins.iadd(sched_ptr, v_shutdown_off);
        const shutdown = try ins.atomicLoad(clif.Type.I64, shutdown_addr);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_shutdown = try ins.icmp(.ne, shutdown, v_zero);

        const block_do_wait = try builder.createBlock();
        const block_after_wait = try builder.createBlock();
        _ = try ins.brif(is_shutdown, block_after_wait, &[_]clif.Value{queue_mutex}, block_do_wait, &[_]clif.Value{queue_mutex});

        // Block: do the condvar wait
        _ = try builder.appendBlockParam(block_do_wait, clif.Type.I64);
        builder.switchToBlock(block_do_wait);
        try builder.ensureInsertedBlock();
        {
            const wait_ins = builder.ins();
            const qm = builder.blockParams(block_do_wait)[0];
            const idle_cond = try wait_ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_IDLE_COND);
            _ = try callLibc2(allocator, &builder, func_index_map, "pthread_cond_wait", idle_cond, qm);
            _ = try wait_ins.jump(block_after_wait, &[_]clif.Value{qm});
        }

        // After wait: unlock and re-enter main loop
        _ = try builder.appendBlockParam(block_after_wait, clif.Type.I64);
        builder.switchToBlock(block_after_wait);
        try builder.ensureInsertedBlock();
        {
            const after_ins = builder.ins();
            const qm = builder.blockParams(block_after_wait)[0];
            _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", qm);
            _ = try after_ins.jump(block_loop, &.{});
        }
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

    // --- Shutdown: set flag, broadcast to wake idle workers, wait for tasks ---
    // Reference: Rayon Registry::terminate pattern.
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

        // Broadcast on idle_cond to wake all parked workers (they'll see shutdown flag).
        // Lock queue_mutex first — workers cond_wait on (idle_cond, queue_mutex).
        const queue_mutex = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_MUTEX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_lock", queue_mutex);
        const idle_cond = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_IDLE_COND);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_cond_broadcast", idle_cond);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", queue_mutex);

        // Check active_tasks — if 0, proceed to join
        const v_active_off = try ins.iconst(clif.Type.I64, @as(i64, SCHED_ACTIVE_TASKS));
        const active_addr = try ins.iadd(sched_ptr, v_active_off);
        const active = try ins.atomicLoad(clif.Type.I64, active_addr);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const all_done = try ins.icmp(.eq, active, v_zero);
        // If tasks still running, brief sleep then re-broadcast (tasks may complete and park)
        const block_sleep = try builder.createBlock();
        _ = try ins.brif(all_done, block_join, &[_]clif.Value{sched_ptr}, block_sleep, &[_]clif.Value{sched_ptr});

        _ = try builder.appendBlockParam(block_sleep, clif.Type.I64);
        builder.switchToBlock(block_sleep);
        try builder.ensureInsertedBlock();
        {
            const sleep_ins = builder.ins();
            const v_sleep = try sleep_ins.iconst(clif.Type.I64, 1000);
            _ = try callLibc1(allocator, &builder, func_index_map, "usleep", v_sleep);
            const sp = builder.blockParams(block_sleep)[0];
            _ = try sleep_ins.jump(block_wait_loop, &[_]clif.Value{sp});
        }
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

        // Destroy condvar and mutex
        const idle_cond = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_IDLE_COND);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_cond_destroy", idle_cond);
        const queue_mutex = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_QUEUE_MUTEX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_destroy", queue_mutex);

        // Free workers array and scheduler struct
        const workers_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, sched_ptr, SCHED_WORKERS_PTR);
        _ = try callLibc1(allocator, &builder, func_index_map, "free", workers_ptr);
        _ = try callLibc1(allocator, &builder, func_index_map, "free", sched_ptr);

        // Clear _cot_sched_ptr so re-initialization is safe
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        try storeSchedPtr(&clif_func, &builder, sched_symbol_idx, v_zero);

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

// ============================================================================
// sched_select(cases_ptr: i64, num_cases: i64, block: i64) -> i64
//
// Go-style channel select. Each case in the cases array is 24 bytes:
//   offset 0: channel_ptr (i64) — pointer to Channel struct
//   offset 8: kind (i64) — 0=recv, 1=send
//   offset 16: value_ptr (i64) — send: value to send; recv: filled with received value
//
// Returns the index of the ready case (0..N-1), or -1 if default taken.
//
// Algorithm: poll-based (Go selectgo simplified for mutex channels).
//   Pass 1: try each case without blocking.
//   If none ready and block=0: return -1 (default).
//   If none ready and block=1: usleep(100) and retry (polling fallback).
//
// Reference: Go runtime/select.go selectgo()
// ============================================================================

// Channel struct offsets (must match stdlib/channel.cot layout)
const CHAN_BUF: i32 = 0;
const CHAN_CAPACITY: i32 = 8;
const CHAN_HEAD: i32 = 16;
const CHAN_TAIL: i32 = 24;
const CHAN_COUNT: i32 = 32;
const CHAN_MTX: i32 = 40; // Mutex (pointer to pthread_mutex_t)
const CHAN_NOT_FULL: i32 = 48; // Condition (pointer to pthread_cond_t)
const CHAN_NOT_EMPTY: i32 = 56; // Condition (pointer to pthread_cond_t)
const CHAN_CLOSED: i32 = 64;

// Select case layout (24 bytes per case)
const SELCASE_CHANNEL: i32 = 0;
const SELCASE_KIND: i32 = 8;
const SELCASE_VALUE: i32 = 16;
const SELCASE_SIZE: i64 = 24;

fn generateSchedSelect(
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

    // Signature: (cases_ptr: i64, num_cases: i64, block: i64) -> i64
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));

    const block_entry = try builder.createBlock();
    const block_poll_loop = try builder.createBlock();
    const block_check_case = try builder.createBlock();
    const block_try_recv = try builder.createBlock();
    const block_try_send = try builder.createBlock();
    const block_recv_ready = try builder.createBlock();
    const block_send_ready = try builder.createBlock();
    const block_not_ready = try builder.createBlock();
    const block_poll_done = try builder.createBlock();
    const block_return_default = try builder.createBlock();
    const block_sleep_retry = try builder.createBlock();

    // ---- Entry: extract params, jump to poll loop ----
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.jump(block_poll_loop, &[_]clif.Value{v_zero});
    }

    // ---- Poll loop: iterate over cases 0..num_cases ----
    // Block param: current index (i64)
    _ = try builder.appendBlockParam(block_poll_loop, clif.Type.I64);
    builder.switchToBlock(block_poll_loop);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const cases_ptr = builder.blockParams(block_entry)[0];
        const num_cases = builder.blockParams(block_entry)[1];

        // Start with index 0 on first entry, or the param value on subsequent
        const idx = builder.blockParams(block_poll_loop)[0];

        // Check if idx >= num_cases → done polling
        const done = try ins.icmp(.sge, idx, num_cases);
        _ = try ins.brif(done, block_poll_done, &.{}, block_check_case, &[_]clif.Value{ idx, cases_ptr });
    }

    // ---- Check case: load kind, branch to recv or send ----
    _ = try builder.appendBlockParam(block_check_case, clif.Type.I64); // idx
    _ = try builder.appendBlockParam(block_check_case, clif.Type.I64); // cases_ptr
    builder.switchToBlock(block_check_case);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const idx = builder.blockParams(block_check_case)[0];
        const cases_ptr = builder.blockParams(block_check_case)[1];

        // Calculate case base address: cases_ptr + idx * 24
        const v_case_size = try ins.iconst(clif.Type.I64, SELCASE_SIZE);
        const offset = try ins.imul(idx, v_case_size);
        const case_addr = try ins.iadd(cases_ptr, offset);

        // Load kind (offset 8)
        const kind = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, case_addr, SELCASE_KIND);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_recv = try ins.icmp(.eq, kind, v_zero);
        _ = try ins.brif(is_recv, block_try_recv, &[_]clif.Value{ idx, case_addr }, block_try_send, &[_]clif.Value{ idx, case_addr });
    }

    // ---- Try recv: lock channel, check count > 0 ----
    _ = try builder.appendBlockParam(block_try_recv, clif.Type.I64); // idx
    _ = try builder.appendBlockParam(block_try_recv, clif.Type.I64); // case_addr
    builder.switchToBlock(block_try_recv);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const idx = builder.blockParams(block_try_recv)[0];
        const case_addr = builder.blockParams(block_try_recv)[1];

        // Load channel_ptr from case
        const ch_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, case_addr, SELCASE_CHANNEL);

        // Lock channel mutex: pthread_mutex_lock(ch_ptr + CHAN_MTX)
        const mtx_val = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_MTX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_lock", mtx_val);

        // Check count > 0
        const count = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_COUNT);
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const has_data = try ins.icmp(.sgt, count, v_zero);
        _ = try ins.brif(has_data, block_recv_ready, &[_]clif.Value{ idx, case_addr, ch_ptr, mtx_val }, block_not_ready, &[_]clif.Value{ idx, mtx_val });
    }

    // ---- Recv ready: dequeue value, unlock, return index ----
    _ = try builder.appendBlockParam(block_recv_ready, clif.Type.I64); // idx
    _ = try builder.appendBlockParam(block_recv_ready, clif.Type.I64); // case_addr
    _ = try builder.appendBlockParam(block_recv_ready, clif.Type.I64); // ch_ptr
    _ = try builder.appendBlockParam(block_recv_ready, clif.Type.I64); // mtx_val
    builder.switchToBlock(block_recv_ready);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const idx = builder.blockParams(block_recv_ready)[0];
        const case_addr = builder.blockParams(block_recv_ready)[1];
        const ch_ptr = builder.blockParams(block_recv_ready)[2];
        const mtx_val = builder.blockParams(block_recv_ready)[3];

        // Dequeue: read buf[head], advance head, decrement count
        const buf = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_BUF);
        const head = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_HEAD);
        const v_eight = try ins.iconst(clif.Type.I64, 8);
        const elem_offset = try ins.imul(head, v_eight);
        const elem_addr = try ins.iadd(buf, elem_offset);
        const value = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, elem_addr, 0);

        // Store received value into case's value_ptr slot
        _ = try ins.store(clif.MemFlags.DEFAULT, value, case_addr, SELCASE_VALUE);

        // Advance head: (head + 1) % capacity
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const new_head_raw = try ins.iadd(head, v_one);
        const capacity = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_CAPACITY);
        const new_head = try ins.srem(new_head_raw, capacity);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_head, ch_ptr, CHAN_HEAD);

        // Decrement count
        const count = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_COUNT);
        const new_count = try ins.isub(count, v_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_count, ch_ptr, CHAN_COUNT);

        // Signal not_full (wake blocked senders)
        const not_full_cond = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_NOT_FULL);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_cond_signal", not_full_cond);

        // Unlock mutex
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", mtx_val);

        // Return the case index
        _ = try ins.return_(&[_]clif.Value{idx});
    }

    // ---- Try send: lock channel, check count < capacity ----
    _ = try builder.appendBlockParam(block_try_send, clif.Type.I64); // idx
    _ = try builder.appendBlockParam(block_try_send, clif.Type.I64); // case_addr
    builder.switchToBlock(block_try_send);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const idx = builder.blockParams(block_try_send)[0];
        const case_addr = builder.blockParams(block_try_send)[1];

        // Load channel_ptr
        const ch_ptr = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, case_addr, SELCASE_CHANNEL);

        // Lock mutex
        const mtx_val = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_MTX);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_lock", mtx_val);

        // Check count < capacity
        const count = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_COUNT);
        const capacity = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_CAPACITY);
        const has_space = try ins.icmp(.slt, count, capacity);
        _ = try ins.brif(has_space, block_send_ready, &[_]clif.Value{ idx, case_addr, ch_ptr, mtx_val }, block_not_ready, &[_]clif.Value{ idx, mtx_val });
    }

    // ---- Send ready: enqueue value, unlock, return index ----
    _ = try builder.appendBlockParam(block_send_ready, clif.Type.I64); // idx
    _ = try builder.appendBlockParam(block_send_ready, clif.Type.I64); // case_addr
    _ = try builder.appendBlockParam(block_send_ready, clif.Type.I64); // ch_ptr
    _ = try builder.appendBlockParam(block_send_ready, clif.Type.I64); // mtx_val
    builder.switchToBlock(block_send_ready);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const idx = builder.blockParams(block_send_ready)[0];
        const case_addr = builder.blockParams(block_send_ready)[1];
        const ch_ptr = builder.blockParams(block_send_ready)[2];
        const mtx_val = builder.blockParams(block_send_ready)[3];

        // Load value from case's value_ptr
        const send_value = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, case_addr, SELCASE_VALUE);

        // Enqueue: buf[tail] = value, advance tail, increment count
        const buf = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_BUF);
        const tail = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_TAIL);
        const v_eight = try ins.iconst(clif.Type.I64, 8);
        const elem_offset = try ins.imul(tail, v_eight);
        const elem_addr = try ins.iadd(buf, elem_offset);
        _ = try ins.store(clif.MemFlags.DEFAULT, send_value, elem_addr, 0);

        // Advance tail: (tail + 1) % capacity
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const new_tail_raw = try ins.iadd(tail, v_one);
        const capacity = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_CAPACITY);
        const new_tail = try ins.srem(new_tail_raw, capacity);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_tail, ch_ptr, CHAN_TAIL);

        // Increment count
        const count = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_COUNT);
        const new_count = try ins.iadd(count, v_one);
        _ = try ins.store(clif.MemFlags.DEFAULT, new_count, ch_ptr, CHAN_COUNT);

        // Signal not_empty (wake blocked receivers)
        const not_empty_cond = try ins.load(clif.Type.I64, clif.MemFlags.DEFAULT, ch_ptr, CHAN_NOT_EMPTY);
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_cond_signal", not_empty_cond);

        // Unlock mutex
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", mtx_val);

        // Return the case index
        _ = try ins.return_(&[_]clif.Value{idx});
    }

    // ---- Not ready: unlock mutex, advance to next case ----
    _ = try builder.appendBlockParam(block_not_ready, clif.Type.I64); // idx
    _ = try builder.appendBlockParam(block_not_ready, clif.Type.I64); // mtx_val
    builder.switchToBlock(block_not_ready);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const idx = builder.blockParams(block_not_ready)[0];
        const mtx_val = builder.blockParams(block_not_ready)[1];

        // Unlock mutex
        _ = try callLibc1(allocator, &builder, func_index_map, "pthread_mutex_unlock", mtx_val);

        // Advance to next case
        const v_one = try ins.iconst(clif.Type.I64, 1);
        const next_idx = try ins.iadd(idx, v_one);
        _ = try ins.jump(block_poll_loop, &[_]clif.Value{next_idx});
    }

    // ---- Poll done: all cases checked, none ready ----
    builder.switchToBlock(block_poll_done);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const block_flag = builder.blockParams(block_entry)[2];

        // If block == 0 (has default): return -1
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        const is_nonblocking = try ins.icmp(.eq, block_flag, v_zero);
        _ = try ins.brif(is_nonblocking, block_return_default, &.{}, block_sleep_retry, &.{});
    }

    // ---- Return default (-1) ----
    builder.switchToBlock(block_return_default);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        const v_neg_one = try ins.iconst(clif.Type.I64, -1);
        _ = try ins.return_(&[_]clif.Value{v_neg_one});
    }

    // ---- Sleep and retry (blocking select) ----
    builder.switchToBlock(block_sleep_retry);
    try builder.ensureInsertedBlock();
    {
        const ins = builder.ins();
        // usleep(100) — 100 microseconds
        const v_sleep = try ins.iconst(clif.Type.I64, 100);
        _ = try callLibc1(allocator, &builder, func_index_map, "usleep", v_sleep);
        // Restart poll from case 0
        const v_zero = try ins.iconst(clif.Type.I64, 0);
        _ = try ins.jump(block_poll_loop, &[_]clif.Value{v_zero});
    }

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}
