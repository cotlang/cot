# Cot Threading Primitives — Compiler Implementation Design

A phased execution plan for adding OS threading to Cot, designed so Cotty can adopt Ghostty's multi-threaded architecture. Each phase is independently useful and builds on the previous one.

**Audience:** The Cot compiler developer (you). Every section includes exact file paths, line references, code patterns, and instruction-level details sufficient to implement without further design.

---

## Table of Contents

1. [What Ghostty Uses](#what-ghostty-uses)
2. [What the Compiler Already Has](#what-the-compiler-already-has)
3. [Phase 1: Thread Creation & Joining](#phase-1-thread-creation--joining)
4. [Phase 2: Mutex](#phase-2-mutex)
5. [Phase 3: Condition Variables](#phase-3-condition-variables)
6. [Phase 4: Atomic Operations](#phase-4-atomic-operations)
7. [Phase 5: Channel — Pure Cot](#phase-5-channel--pure-cot)
8. [Cotty Adoption Plan](#cotty-adoption-plan)
9. [File Change Summary](#file-change-summary)
10. [Wasm Considerations](#wasm-considerations)

---

## What Ghostty Uses

From auditing `~/cot-land/cotty/references/ghostty/src/`:

### Thread Spawning (`Surface.zig:697-710`)
```zig
self.renderer_thr = try std.Thread.spawn(.{}, rendererpkg.Thread.threadMain, .{&self.renderer_thread});
self.io_thr = try std.Thread.spawn(.{}, termio.Thread.threadMain, .{&self.io_thread, &self.io});
```
Shutdown pattern (`Surface.zig:777-791`): notify stop async → `thread.join()`.

### Mutex (`Surface.zig:560`, `termio/Thread.zig:150-151`)
```zig
const mutex = try alloc.create(std.Thread.Mutex);
mutex.* = .{};

// IO thread locks when writing terminal state:
io.renderer_state.mutex.lock();
defer io.renderer_state.mutex.unlock();
```

### Condition Variables (`datastruct/blocking_queue.zig`)
```zig
self.cond_not_full.wait(&self.mutex);       // Block until space available
self.cond_not_full.timedWait(&self.mutex, ns) catch return 0;
self.cond_not_full.signal();                // Wake one waiter
```

### BlockingQueue (`datastruct/blocking_queue.zig`)
SPSC fixed-capacity ring buffer with mutex + two condition variables (not_full, not_empty). Operations:
- `push(value, timeout)` — blocks if full (supports instant/forever/timed)
- `pop()` — non-blocking, returns `?T`
- `drain()` — returns iterator that holds the lock, drains all items

### Architecture — 3 Threads Per Surface
| Thread | Purpose | Event Loop |
|--------|---------|------------|
| **UI (main)** | Window events, keyboard/mouse input | AppKit/GTK main loop |
| **IO** | PTY read/write, VT parsing, terminal state mutation | `xev.Loop` |
| **Renderer** | Sample terminal state, build GPU commands, VSync | `xev.Loop` |

Communication: UI → IO via `Mailbox` (BlockingQueue), IO → Renderer via `renderer_wakeup` async notify, Renderer → App via `app_mailbox` (BlockingQueue).

Shared state protection: Single `std.Thread.Mutex` on `renderer_state` — IO thread locks when mutating terminal grid, renderer locks when reading for frame.

---

## What the Compiler Already Has

### SSA Atomic Ops — Defined but No Codegen
`compiler/ssa/op.zig:82-84`:
```zig
// === Atomics ===
atomic_load32, atomic_load64, atomic_store32, atomic_store64,
atomic_add32, atomic_add64, atomic_cas32, atomic_cas64, atomic_exchange32, atomic_exchange64,
```
These exist in the `Op` enum but have **no OpInfo entries** in `op_info_table` (initialized at line 275) and **no codegen** in `ssa_to_clif.zig`.

### AArch64 Atomic Machine Instructions — Already Implemented
`compiler/codegen/native/isa/aarch64/inst/mod.zig` already defines:
- `atomic_rmw` — single-instruction RMW (LSE atomics: `ldadd`, `ldclr`, `ldeor`, `ldset`, `swp`)
- `atomic_rmw_loop` — `ldaxr` → ALU → `stlxr` → `cbnz` retry loop (pre-LSE fallback)
- `atomic_cas` — single-instruction CAS (LSE: `cas`)
- `atomic_cas_loop` — `ldaxr` → `cmp` → `stlxr` → `cbnz` loop
- `ldaxr`, `stlxr` — load-acquire exclusive, store-release exclusive
- `ldar`, `stlr` — load-acquire, store-release

`isa/aarch64/emit.zig:1874-2097` has full binary encoding for all of these.

**What's missing:** The CLIF IR → AArch64 lowering path. The machine instructions exist but nothing maps CLIF ops to them yet.

### Runtime Function Pattern
`compiler/driver.zig:1076-1140` — `runtime_func_names` array registers function names → indices. `generateNativeCodeDirect()` (line 1257+) calls `arc_native.generate()`, `io_native.generate()`, `print_native.generate()`, `test_native.generate()` to produce CLIF IR wrappers around libc calls.

### Builtin Pattern
`compiler/frontend/ast.zig:157-232` — `BuiltinKind` enum + string map. Type-checked in `checker.zig:checkBuiltinCall()` (line 1866+). Lowered to SSA in `lower.zig:lowerBuiltinCall()` (line 7098+).

### CLIF IR Opcodes
`compiler/ir/clif/instructions.zig:296-548` — No atomic opcodes currently defined. Memory ops: `load`, `store`, `stack_load`, `stack_store`, `stack_addr`.

---

## Phase 1: Thread Creation & Joining

**Goal:** Spawn OS threads from Cot code. Minimum viable threading.

### API Design

```cot
import "std/thread"

fn worker(arg: i64) void {
    // runs on new thread
    println(arg)
}

fn main() void {
    var t = Thread.spawn(worker, 42)
    t.join()  // wait for completion
}
```

### Runtime Functions

| Function | Signature | Wraps |
|----------|-----------|-------|
| `thread_spawn` | `(fn_ptr: i64, arg: i64) → i64` | `pthread_create` |
| `thread_join` | `(handle: i64) → void` | `pthread_join` |
| `thread_detach` | `(handle: i64) → void` | `pthread_detach` |

### Compiler Changes

#### 1. `compiler/driver.zig` — Register runtime function names

Add to `runtime_func_names` array (after the `cot_ioctl_winsize` entries, around line 1103):

```zig
// Thread runtime (thread_native.generate order)
"thread_spawn",  "thread_join",  "thread_detach",
// libc symbols for thread ops
"pthread_create", "pthread_join", "pthread_detach",
```

Add to the generate chain in `generateNativeCodeDirect()` (around line 1290, after test runtime):

```zig
const thread_native = @import("codegen/native/thread_native.zig");
// ... at top of file with other imports

// Thread runtime: thread_spawn, thread_join, thread_detach
var thread_funcs = try thread_native.generate(self.allocator, isa, &ctrl_plane, &func_index_map);
defer thread_funcs.deinit(self.allocator);
for (thread_funcs.items) |rf| {
    try compiled_funcs.append(self.allocator, rf.compiled);
    try func_names.append(self.allocator, rf.name);
}
```

#### 2. `compiler/codegen/native/thread_native.zig` — NEW FILE

Generates CLIF IR wrappers around pthread functions. Follow `io_native.zig` patterns exactly.

**`thread_spawn(fn_ptr, arg) → handle`:**

This is the most complex wrapper. `pthread_create` has signature:
```c
int pthread_create(pthread_t *thread, const pthread_attr_t *attr, void *(*start)(void*), void *arg)
```

The CLIF IR must:
1. Stack-allocate 8 bytes for `pthread_t` (it's a pointer-sized opaque type)
2. Call `pthread_create(&thread_t, NULL, fn_ptr, arg)`
3. Load the `pthread_t` value from the stack and return it as i64

```
thread_spawn(fn_ptr: i64, arg: i64) → i64:
    thread_slot = stack_alloc(8)           // space for pthread_t
    addr = stack_addr(thread_slot)         // &thread_t
    null = iconst(0)                       // NULL attrs
    call pthread_create(addr, null, fn_ptr, arg)
    handle = load(addr)                    // read pthread_t
    return handle
```

**`thread_join(handle) → void`:**

Simple forward — `pthread_join(handle, NULL)`:
```
thread_join(handle: i64) → void:
    null = iconst(0)
    call pthread_join(handle, null)
    return
```

**`thread_detach(handle) → void`:**

Direct forward — `pthread_detach(handle)`:
```
thread_detach(handle: i64) → void:
    call pthread_detach(handle)
    return
```

**Implementation pattern** — follow `generateForward3` from `io_native.zig:255-313`:

```zig
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !std.ArrayListUnmanaged(RuntimeFunc) {
    var result = std.ArrayListUnmanaged(RuntimeFunc){};

    // thread_spawn: allocate pthread_t on stack, call pthread_create, return handle
    try result.append(allocator, .{
        .name = "thread_spawn",
        .compiled = try generateThreadSpawn(allocator, isa, ctrl_plane, func_index_map),
    });

    // thread_join: call pthread_join(handle, NULL)
    try result.append(allocator, .{
        .name = "thread_join",
        .compiled = try generateThreadJoin(allocator, isa, ctrl_plane, func_index_map),
    });

    // thread_detach: call pthread_detach(handle)
    try result.append(allocator, .{
        .name = "thread_detach",
        .compiled = try generateThreadDetach(allocator, isa, ctrl_plane, func_index_map),
    });

    return result;
}
```

For `generateThreadSpawn`, the key CLIF IR pattern for stack allocation:
```zig
// Allocate stack slot for pthread_t (8 bytes, aligned to 8)
const slot = try builder.createStackSlot(.{ .size = 8, .alignment = 8 });
const ins = builder.ins();

// Get address of stack slot
const addr = try ins.stack_addr(slot);

// Build pthread_create call: pthread_create(&thread, NULL, fn_ptr, arg)
const null_val = try ins.iconst(clif.Type.I64, 0);
const pthread_create_idx = func_index_map.get("pthread_create") orelse 0;
var sig = clif.Signature.init(.system_v);
try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // &thread
try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // attr (NULL)
try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // start_routine
try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64)); // arg
try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64)); // int result
const sig_ref = try builder.importSignature(sig);
const func_ref = try builder.importFunction(.{
    .name = .{ .user = .{ .namespace = 0, .index = pthread_create_idx } },
    .signature = sig_ref,
    .colocated = false,
});
_ = try ins.call(func_ref, &[_]clif.Value{ addr, null_val, params[0], params[1] });

// Load the thread handle from stack
const handle = try ins.stack_load(clif.Type.I64, slot, 0);
_ = try ins.return_(&[_]clif.Value{handle});
```

#### 3. Thread entry function ABI

**Critical detail:** `pthread_create` expects `void *(*start_routine)(void*)` — takes `void*` arg, returns `void*`. But Cot functions are `fn(i64) void` — takes i64, returns void.

On native targets (macOS ARM64, Linux x64), pointers and i64 are the same size and use the same register (x0/rdi). So `fn(i64) void` is ABI-compatible with `void *(*)(void*)` as long as:
- The arg is passed as i64 (same register as void*)
- The return value is ignored (pthread discards it)

This means **no trampoline is needed** — `thread_spawn` can pass the Cot function pointer directly to `pthread_create`. The Cot function receives `arg` in x0 (ARM64) or rdi (x64), same as if called normally.

### Stdlib

`stdlib/thread.cot`:
```cot
import "std/sys"

/// OS thread handle
struct Thread {
    handle: i64,
}

impl Thread {
    /// Spawn a new OS thread running `func(arg)`.
    /// The function must have signature `fn(i64) void`.
    static fn spawn(func: fn(i64) void, arg: i64) Thread {
        var handle = thread_spawn(@ptrToInt(func), arg)
        return Thread { handle: handle }
    }

    /// Block until the thread completes.
    fn join() void {
        thread_join(handle)
    }

    /// Detach the thread (it will clean up on exit).
    fn detach() void {
        thread_detach(handle)
    }
}

// FFI declarations for runtime functions
extern fn thread_spawn(fn_ptr: i64, arg: i64) i64
extern fn thread_join(handle: i64) void
extern fn thread_detach(handle: i64) void
```

### Verification

```cot
import "std/thread"

fn worker(arg: i64) void {
    println(arg)  // prints 42 from worker thread
}

test "spawn and join" {
    var t = Thread.spawn(worker, 42)
    t.join()
}
```

### Cotty Use

Spawn a background PTY reader thread:
```cot
fn ptyReaderLoop(arg: i64) void {
    var state = @intToPtr(*TerminalState, arg)
    while (state.running) {
        var n = fd_read(state.pty_fd, state.read_buf_ptr, state.read_buf_len)
        if (n > 0) {
            // feed bytes to VT parser
            state.feedBytes(n)
        }
    }
}

// In terminal init:
var reader = Thread.spawn(ptyReaderLoop, @ptrToInt(state))
```

---

## Phase 2: Mutex

**Goal:** Protect shared state between threads. Required before Cotty can have a background IO thread.

### API Design

```cot
import "std/thread"

fn main() void {
    var mutex = Mutex.init()
    defer mutex.destroy()

    mutex.lock()
    // critical section — only one thread can be here
    mutex.unlock()

    if (mutex.tryLock()) {
        // got the lock
        mutex.unlock()
    }
}
```

### Runtime Functions

| Function | Signature | Wraps |
|----------|-----------|-------|
| `mutex_init` | `() → i64` | `malloc(size) + pthread_mutex_init` |
| `mutex_lock` | `(ptr: i64) → void` | `pthread_mutex_lock` |
| `mutex_unlock` | `(ptr: i64) → void` | `pthread_mutex_unlock` |
| `mutex_trylock` | `(ptr: i64) → i64` | `pthread_mutex_trylock` |
| `mutex_destroy` | `(ptr: i64) → void` | `pthread_mutex_destroy + free` |

### Compiler Changes

#### 1. `compiler/driver.zig`

Add to `runtime_func_names`:
```zig
// Mutex runtime (thread_native.generate order)
"mutex_init", "mutex_lock", "mutex_unlock", "mutex_trylock", "mutex_destroy",
// libc symbols
"pthread_mutex_init", "pthread_mutex_lock", "pthread_mutex_unlock",
"pthread_mutex_trylock", "pthread_mutex_destroy",
```

#### 2. `compiler/codegen/native/thread_native.zig` — Extend

**`mutex_init() → ptr`:**

`pthread_mutex_t` is an opaque struct. Sizes by platform:
- macOS ARM64: 64 bytes (`__darwin_pthread_mutex_t` = `_opaque_pthread_mutex_t` with 56-byte `__opaque` + 8-byte `__sig`)
- Linux x64: 40 bytes (`__pthread_mutex_s`)

The CLIF IR must:
1. Call `malloc(64)` to allocate the mutex (use 64 for both platforms — safe overallocation on Linux)
2. Call `memset(ptr, 0, 64)` to zero-initialize (equivalent to `PTHREAD_MUTEX_INITIALIZER`)
3. Call `pthread_mutex_init(ptr, NULL)` with default attributes
4. Return the pointer

```
mutex_init() → i64:
    size = iconst(64)
    ptr = call malloc(size)
    zero = iconst(0)
    size64 = iconst(64)
    call memset(ptr, zero, size64)        // zero-init = PTHREAD_MUTEX_INITIALIZER
    null = iconst(0)
    call pthread_mutex_init(ptr, null)    // default attrs
    return ptr
```

**`mutex_lock(ptr) → void`:**
Direct forward to `pthread_mutex_lock(ptr)`.

**`mutex_unlock(ptr) → void`:**
Direct forward to `pthread_mutex_unlock(ptr)`.

**`mutex_trylock(ptr) → i64`:**
Forward to `pthread_mutex_trylock(ptr)` which returns 0 on success, `EBUSY` on contention.

**`mutex_destroy(ptr) → void`:**
```
mutex_destroy(ptr: i64) → void:
    call pthread_mutex_destroy(ptr)
    call free(ptr)
    return
```

### Stdlib

Extend `stdlib/thread.cot`:
```cot
/// Mutual exclusion lock backed by pthread_mutex.
struct Mutex {
    ptr: i64,
}

impl Mutex {
    /// Create a new mutex. Must call destroy() when done.
    static fn init() Mutex {
        return Mutex { ptr: mutex_init() }
    }

    /// Acquire the lock. Blocks until available.
    fn lock() void {
        mutex_lock(ptr)
    }

    /// Release the lock.
    fn unlock() void {
        mutex_unlock(ptr)
    }

    /// Try to acquire the lock without blocking.
    /// Returns true if acquired, false if already held.
    fn tryLock() bool {
        return mutex_trylock(ptr) == 0
    }

    /// Destroy the mutex and free memory.
    fn destroy() void {
        mutex_destroy(ptr)
    }
}

extern fn mutex_init() i64
extern fn mutex_lock(ptr: i64) void
extern fn mutex_unlock(ptr: i64) void
extern fn mutex_trylock(ptr: i64) i64
extern fn mutex_destroy(ptr: i64) void
```

### Verification

```cot
import "std/thread"

var counter: i64 = 0
var mtx = Mutex.init()

fn increment(arg: i64) void {
    var i: i64 = 0
    while (i < 1000) {
        mtx.lock()
        counter = counter + 1
        mtx.unlock()
        i = i + 1
    }
}

test "mutex protects shared counter" {
    var t1 = Thread.spawn(increment, 0)
    var t2 = Thread.spawn(increment, 0)
    t1.join()
    t2.join()
    @assertEq(counter, 2000)
    mtx.destroy()
}
```

### Cotty Use

Protect terminal grid between IO thread (writer) and renderer (reader):
```cot
struct TerminalState {
    grid: Grid,
    mutex: Mutex,
    // ...
}

// IO thread:
fn feedBytes(state: *TerminalState, bytes: *u8, len: i64) void {
    state.mutex.lock()
    // parse VT sequences, update grid
    state.mutex.unlock()
}

// Renderer (main thread):
fn renderFrame(state: *TerminalState) void {
    state.mutex.lock()
    // read grid cells, build CellData array
    state.mutex.unlock()
}
```

---

## Phase 3: Condition Variables

**Goal:** Enable blocking queues (producer waits when full, consumer waits when empty).

### API Design

```cot
import "std/thread"

var mutex = Mutex.init()
var cond = Condition.init()
var ready = false

fn producer(arg: i64) void {
    // produce data...
    mutex.lock()
    ready = true
    cond.signal()   // wake one waiting consumer
    mutex.unlock()
}

fn consumer(arg: i64) void {
    mutex.lock()
    while (!ready) {
        cond.wait(mutex)  // atomically unlock + sleep + relock
    }
    // consume data...
    mutex.unlock()
}
```

### Runtime Functions

| Function | Signature | Wraps |
|----------|-----------|-------|
| `cond_init` | `() → i64` | `malloc(size) + pthread_cond_init` |
| `cond_wait` | `(cond: i64, mutex: i64) → void` | `pthread_cond_wait` |
| `cond_signal` | `(cond: i64) → void` | `pthread_cond_signal` |
| `cond_broadcast` | `(cond: i64) → void` | `pthread_cond_broadcast` |
| `cond_destroy` | `(cond: i64) → void` | `pthread_cond_destroy + free` |

### Compiler Changes

#### 1. `compiler/driver.zig`

Add to `runtime_func_names`:
```zig
// Condition variable runtime
"cond_init", "cond_wait", "cond_signal", "cond_broadcast", "cond_destroy",
// libc symbols
"pthread_cond_init", "pthread_cond_wait", "pthread_cond_signal",
"pthread_cond_broadcast", "pthread_cond_destroy",
```

#### 2. `compiler/codegen/native/thread_native.zig` — Extend

**`cond_init() → ptr`:**

`pthread_cond_t` sizes:
- macOS ARM64: 48 bytes (`_opaque_pthread_cond_t` = 40-byte `__opaque` + 8-byte `__sig`)
- Linux x64: 48 bytes (`__pthread_cond_s`)

```
cond_init() → i64:
    size = iconst(48)
    ptr = call malloc(size)
    zero = iconst(0)
    call memset(ptr, zero, size)         // zero-init
    null = iconst(0)
    call pthread_cond_init(ptr, null)    // default attrs
    return ptr
```

**`cond_wait(cond, mutex) → void`:**
Direct forward to `pthread_cond_wait(cond, mutex)` — 2-arg wrapper.

**`cond_signal(cond) → void`:**
Direct forward to `pthread_cond_signal(cond)` — 1-arg wrapper.

**`cond_broadcast(cond) → void`:**
Direct forward to `pthread_cond_broadcast(cond)` — 1-arg wrapper.

**`cond_destroy(cond) → void`:**
```
cond_destroy(cond: i64) → void:
    call pthread_cond_destroy(cond)
    call free(cond)
    return
```

### Stdlib

Extend `stdlib/thread.cot`:
```cot
/// Condition variable backed by pthread_cond.
struct Condition {
    ptr: i64,
}

impl Condition {
    /// Create a new condition variable. Must call destroy() when done.
    static fn init() Condition {
        return Condition { ptr: cond_init() }
    }

    /// Atomically unlock the mutex, sleep until signaled, then relock.
    /// Must be called while holding the mutex.
    fn wait(mutex: Mutex) void {
        cond_wait(ptr, mutex.ptr)
    }

    /// Wake one thread waiting on this condition.
    fn signal() void {
        cond_signal(ptr)
    }

    /// Wake all threads waiting on this condition.
    fn broadcast() void {
        cond_broadcast(ptr)
    }

    /// Destroy the condition variable and free memory.
    fn destroy() void {
        cond_destroy(ptr)
    }
}

extern fn cond_init() i64
extern fn cond_wait(cond: i64, mutex: i64) void
extern fn cond_signal(cond: i64) void
extern fn cond_broadcast(cond: i64) void
extern fn cond_destroy(cond: i64) void
```

### Verification

```cot
import "std/thread"

var mutex = Mutex.init()
var cond = Condition.init()
var value: i64 = 0

fn producer(arg: i64) void {
    mutex.lock()
    value = 42
    cond.signal()
    mutex.unlock()
}

fn consumer(arg: i64) void {
    mutex.lock()
    while (value == 0) {
        cond.wait(mutex)
    }
    @assertEq(value, 42)
    mutex.unlock()
}

test "condition variable signal" {
    var c = Thread.spawn(consumer, 0)
    var p = Thread.spawn(producer, 0)
    p.join()
    c.join()
    mutex.destroy()
    cond.destroy()
}
```

### Cotty Use

Enable the blocking message queue between app thread and IO thread — same pattern as Ghostty's `Mailbox`. See Phase 5 for the full `Channel(T)` built on top of these primitives.

---

## Phase 4: Atomic Operations

**Goal:** Lock-free dirty flags, atomic counters, wait-free single-writer patterns. Also required for atomic ARC refcounting in multi-threaded programs.

### API Design — Builtins

```cot
var flag: i64 = 0

fn ioThread(arg: i64) void {
    // ... process data ...
    @atomicStore(&flag, 1)    // signal "data ready"
}

fn renderer(arg: i64) void {
    if (@atomicLoad(&flag) != 0) {
        @atomicStore(&flag, 0)
        renderFrame()
    }
}
```

### Builtins

| Builtin | Signature | Returns | SSA Op |
|---------|-----------|---------|--------|
| `@atomicLoad` | `(ptr: *i64) → i64` | Value at ptr | `atomic_load64` |
| `@atomicStore` | `(ptr: *i64, val: i64) → void` | — | `atomic_store64` |
| `@atomicAdd` | `(ptr: *i64, val: i64) → i64` | Previous value | `atomic_add64` |
| `@atomicCAS` | `(ptr: *i64, expected: i64, new: i64) → i64` | Actual value | `atomic_cas64` |
| `@atomicExchange` | `(ptr: *i64, val: i64) → i64` | Previous value | `atomic_exchange64` |

**Note:** Only `i64` variants are needed initially. The `i32` variants (`atomic_load32`, etc.) exist in the SSA op enum but can be wired up later if needed.

### Compiler Changes

This is the most involved phase — it touches frontend (parsing, type-checking, lowering) and backend (SSA → CLIF → machine code).

#### 1. `compiler/frontend/ast.zig` — Add Builtin Kinds

Add to `BuiltinKind` enum (after `type_info`, around line 230):
```zig
// Atomic operations
atomic_load,
atomic_store,
atomic_add,
atomic_cas,
atomic_exchange,
```

Add to the string map (inside the `StaticStringMap` initialization):
```zig
.{ "atomicLoad", .atomic_load },
.{ "atomicStore", .atomic_store },
.{ "atomicAdd", .atomic_add },
.{ "atomicCAS", .atomic_cas },
.{ "atomicExchange", .atomic_exchange },
```

#### 2. `compiler/frontend/checker.zig` — Type-Check

Add cases to `checkBuiltinCall()` (around line 1970):

```zig
.atomic_load => {
    // @atomicLoad(ptr: *i64) → i64
    if (bc.args.len != 1) {
        self.err.errorWithCode(bc.span.start, .e300, "@atomicLoad requires 1 argument");
        return invalid_type;
    }
    const arg_type = try self.checkExpr(bc.args[0]);
    const arg_info = self.types.get(arg_type);
    if (arg_info != .pointer or arg_info.pointer.elem != TypeRegistry.I64) {
        self.err.errorWithCode(bc.span.start, .e300, "@atomicLoad argument must be *i64");
        return invalid_type;
    }
    return TypeRegistry.I64;
},

.atomic_store => {
    // @atomicStore(ptr: *i64, val: i64) → void
    if (bc.args.len != 2) {
        self.err.errorWithCode(bc.span.start, .e300, "@atomicStore requires 2 arguments");
        return invalid_type;
    }
    const ptr_type = try self.checkExpr(bc.args[0]);
    const ptr_info = self.types.get(ptr_type);
    if (ptr_info != .pointer or ptr_info.pointer.elem != TypeRegistry.I64) {
        self.err.errorWithCode(bc.span.start, .e300, "@atomicStore first argument must be *i64");
        return invalid_type;
    }
    const val_type = try self.checkExpr(bc.args[1]);
    if (val_type != TypeRegistry.I64) {
        self.err.errorWithCode(bc.span.start, .e300, "@atomicStore second argument must be i64");
        return invalid_type;
    }
    return TypeRegistry.VOID;
},

.atomic_add, .atomic_exchange => {
    // @atomicAdd(ptr: *i64, val: i64) → i64  (returns previous)
    // @atomicExchange(ptr: *i64, val: i64) → i64  (returns previous)
    if (bc.args.len != 2) {
        self.err.errorWithCode(bc.span.start, .e300, "requires 2 arguments");
        return invalid_type;
    }
    const ptr_type = try self.checkExpr(bc.args[0]);
    const ptr_info = self.types.get(ptr_type);
    if (ptr_info != .pointer or ptr_info.pointer.elem != TypeRegistry.I64) {
        self.err.errorWithCode(bc.span.start, .e300, "first argument must be *i64");
        return invalid_type;
    }
    _ = try self.checkExpr(bc.args[1]);
    return TypeRegistry.I64;
},

.atomic_cas => {
    // @atomicCAS(ptr: *i64, expected: i64, new: i64) → i64  (returns actual)
    if (bc.args.len != 3) {
        self.err.errorWithCode(bc.span.start, .e300, "@atomicCAS requires 3 arguments");
        return invalid_type;
    }
    const ptr_type = try self.checkExpr(bc.args[0]);
    const ptr_info = self.types.get(ptr_type);
    if (ptr_info != .pointer or ptr_info.pointer.elem != TypeRegistry.I64) {
        self.err.errorWithCode(bc.span.start, .e300, "@atomicCAS first argument must be *i64");
        return invalid_type;
    }
    _ = try self.checkExpr(bc.args[1]);
    _ = try self.checkExpr(bc.args[2]);
    return TypeRegistry.I64;
},
```

#### 3. `compiler/frontend/lower.zig` — Lower to SSA

Add cases to `lowerBuiltinCall()` (around line 7200):

```zig
.atomic_load => {
    const ptr = try self.lowerExprNode(bc.args[0]);
    return try fb.emit(ir.Node.init(.{
        .unary = .{ .operand = ptr, .op = .atomic_load64 },
    }, TypeRegistry.I64, bc.span));
},

.atomic_store => {
    const ptr = try self.lowerExprNode(bc.args[0]);
    const val = try self.lowerExprNode(bc.args[1]);
    return try fb.emit(ir.Node.init(.{
        .binary = .{ .lhs = ptr, .rhs = val, .op = .atomic_store64 },
    }, TypeRegistry.VOID, bc.span));
},

.atomic_add => {
    const ptr = try self.lowerExprNode(bc.args[0]);
    const val = try self.lowerExprNode(bc.args[1]);
    return try fb.emit(ir.Node.init(.{
        .binary = .{ .lhs = ptr, .rhs = val, .op = .atomic_add64 },
    }, TypeRegistry.I64, bc.span));
},

.atomic_cas => {
    const ptr = try self.lowerExprNode(bc.args[0]);
    const expected = try self.lowerExprNode(bc.args[1]);
    const new_val = try self.lowerExprNode(bc.args[2]);
    // Need a ternary IR node — or use args array
    return try fb.emit(ir.Node.init(.{
        .ternary = .{ .a = ptr, .b = expected, .c = new_val, .op = .atomic_cas64 },
    }, TypeRegistry.I64, bc.span));
},

.atomic_exchange => {
    const ptr = try self.lowerExprNode(bc.args[0]);
    const val = try self.lowerExprNode(bc.args[1]);
    return try fb.emit(ir.Node.init(.{
        .binary = .{ .lhs = ptr, .rhs = val, .op = .atomic_exchange64 },
    }, TypeRegistry.I64, bc.span));
},
```

**Note:** The exact IR node shapes (`.unary`, `.binary`, `.ternary`) depend on how the Cot IR defines multi-operand nodes. If no `.ternary` exists for CAS, you may need to add one or use a variadic args pattern. Check `compiler/ir/` for available node types.

#### 4. `compiler/ssa/op.zig` — Add OpInfo Entries

The SSA ops are already defined in the enum (lines 82-84). Add their metadata to `op_info_table` (around line 398, near other memory ops):

```zig
// Atomic operations
table[@intFromEnum(Op.atomic_load64)] = .{
    .name = "AtomicLoad64",
    .arg_len = 1,        // ptr
    .reads_memory = true,
    .has_side_effects = true,  // memory barrier semantics
};
table[@intFromEnum(Op.atomic_store64)] = .{
    .name = "AtomicStore64",
    .arg_len = 2,        // ptr, val
    .writes_memory = true,
    .has_side_effects = true,
};
table[@intFromEnum(Op.atomic_add64)] = .{
    .name = "AtomicAdd64",
    .arg_len = 2,        // ptr, val
    .reads_memory = true,
    .writes_memory = true,
    .has_side_effects = true,
};
table[@intFromEnum(Op.atomic_cas64)] = .{
    .name = "AtomicCas64",
    .arg_len = 3,        // ptr, expected, new
    .reads_memory = true,
    .writes_memory = true,
    .has_side_effects = true,
};
table[@intFromEnum(Op.atomic_exchange64)] = .{
    .name = "AtomicExchange64",
    .arg_len = 2,        // ptr, val
    .reads_memory = true,
    .writes_memory = true,
    .has_side_effects = true,
};
```

**All atomics have `has_side_effects = true`** to prevent reordering by optimization passes. They act as memory barriers.

#### 5. `compiler/codegen/native/ssa_to_clif.zig` — Translate to CLIF

**Option A: Add atomic CLIF opcodes** — add `atomic_load`, `atomic_store`, `atomic_rmw`, `atomic_cas` to `instructions.zig:Opcode` enum, then handle them in the ISA lowerers.

**Option B: Bypass CLIF entirely** — translate SSA atomic ops directly to machine instructions during lowering, similar to how `atomic_rmw_loop` in AArch64 is a compound machine instruction.

**Recommended: Option A** — cleaner pipeline. Add CLIF opcodes first:

In `compiler/ir/clif/instructions.zig`, add to `Opcode` enum (after `store`):
```zig
// Atomic memory operations
atomic_load,       // Load with acquire semantics
atomic_store,      // Store with release semantics
atomic_rmw,        // Atomic read-modify-write (add, exchange)
atomic_cas,        // Atomic compare-and-swap
```

In `ssa_to_clif.zig:translateValue()` (line 384+), add cases:

```zig
.atomic_load64 => {
    const addr = self.getClif(v.args[0]);
    // Emit CLIF atomic_load — will be lowered to ldaxr/ldar (ARM64) or mov (x64, all loads are acquire on x64-TSO)
    const result = try ins.atomic_load(clif.Type.I64, clif.MemFlags.DEFAULT, addr, 0);
    try self.putValue(v.id, result);
},

.atomic_store64 => {
    const addr = self.getClif(v.args[0]);
    const val = self.getClif(v.args[1]);
    // Emit CLIF atomic_store — lowered to stlr (ARM64) or mov+mfence (x64)
    _ = try ins.atomic_store(clif.MemFlags.DEFAULT, val, addr, 0);
},

.atomic_add64 => {
    const addr = self.getClif(v.args[0]);
    const val = self.getClif(v.args[1]);
    // Emit CLIF atomic_rmw with Add op — lowered to ldadd (ARM64 LSE) or lock xadd (x64)
    const result = try ins.atomic_rmw(clif.Type.I64, .add, addr, val);
    try self.putValue(v.id, result);
},

.atomic_cas64 => {
    const addr = self.getClif(v.args[0]);
    const expected = self.getClif(v.args[1]);
    const new_val = self.getClif(v.args[2]);
    // Emit CLIF atomic_cas — lowered to cas (ARM64 LSE) or lock cmpxchg (x64)
    const result = try ins.atomic_cas(clif.Type.I64, addr, expected, new_val);
    try self.putValue(v.id, result);
},

.atomic_exchange64 => {
    const addr = self.getClif(v.args[0]);
    const val = self.getClif(v.args[1]);
    // Emit CLIF atomic_rmw with Exchange op — lowered to swp (ARM64 LSE) or xchg (x64)
    const result = try ins.atomic_rmw(clif.Type.I64, .exchange, addr, val);
    try self.putValue(v.id, result);
},
```

#### 6. ISA Lowering — AArch64

`compiler/codegen/native/isa/aarch64/lower.zig` — add cases in the main `lower()` switch:

```zig
.atomic_load => {
    // Lower to ldar (load-acquire) — sequential consistency
    const addr = ctx.putInputInRegs(ir_inst, 0);
    const addr_reg = addr.onlyReg() orelse return null;
    const dst = ctx.allocTmp(clif.Type.I64) catch return null;
    const dst_reg = dst.onlyReg() orelse return null;
    // ldar x_dst, [x_addr]
    ctx.emit(Inst{ .ldar = .{
        .ty = .size64,
        .rt = dst_reg,
        .rn = addr_reg.toReg(),
    } }) catch return null;
    var output = InstOutput{};
    output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
    return output;
},

.atomic_store => {
    // Lower to stlr (store-release) — sequential consistency
    const val = ctx.putInputInRegs(ir_inst, 0);
    const addr = ctx.putInputInRegs(ir_inst, 1);
    val_reg = val.onlyReg() orelse return null;
    addr_reg = addr.onlyReg() orelse return null;
    // stlr x_val, [x_addr]
    ctx.emit(Inst{ .stlr = .{
        .ty = .size64,
        .rt = val_reg.toReg(),
        .rn = addr_reg.toReg(),
    } }) catch return null;
    return InstOutput{};
},

.atomic_rmw => {
    // Lower to atomic_rmw (LSE) or atomic_rmw_loop (pre-LSE)
    // The existing AArch64 machine instructions handle both paths
    const addr = ctx.putInputInRegs(ir_inst, 0);
    const val = ctx.putInputInRegs(ir_inst, 1);
    const addr_reg = addr.onlyReg() orelse return null;
    const val_reg = val.onlyReg() orelse return null;
    const dst = ctx.allocTmp(clif.Type.I64) catch return null;
    const dst_reg = dst.onlyReg() orelse return null;
    const rmw_op = ctx.data(ir_inst).getAtomicRmwOp();

    // Prefer LSE atomics (available on Apple Silicon and ARMv8.1+)
    ctx.emit(Inst{ .atomic_rmw = .{
        .ty = .size64,
        .op = rmw_op,  // .add, .swp, etc.
        .rs = val_reg.toReg(),
        .rt = dst_reg,
        .rn = addr_reg.toReg(),
    } }) catch return null;
    var output = InstOutput{};
    output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
    return output;
},

.atomic_cas => {
    // Lower to cas (LSE) or atomic_cas_loop
    const addr = ctx.putInputInRegs(ir_inst, 0);
    const expected = ctx.putInputInRegs(ir_inst, 1);
    const new_val = ctx.putInputInRegs(ir_inst, 2);
    const addr_reg = addr.onlyReg() orelse return null;
    const expected_reg = expected.onlyReg() orelse return null;
    const new_reg = new_val.onlyReg() orelse return null;
    const dst = ctx.allocTmp(clif.Type.I64) catch return null;
    const dst_reg = dst.onlyReg() orelse return null;

    // cas x_expected, x_new, [x_addr]
    // Result: x_expected is overwritten with actual value
    ctx.emit(Inst{ .atomic_cas = .{
        .ty = .size64,
        .rd = expected_reg.toReg(),
        .rt = new_reg.toReg(),
        .rn = addr_reg.toReg(),
    } }) catch return null;
    var output = InstOutput{};
    output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
    return output;
},
```

**ARM64 atomic instruction encoding** — already implemented in `emit.zig:1874-2097`:
- `ldadd` → `encAtomicRmw(ty, .add, rs, rt, rn)` — atomically: `old = *rn; *rn += rs; rt = old`
- `swp` → `encAtomicRmw(ty, .swp, rs, rt, rn)` — atomically: `old = *rn; *rn = rs; rt = old`
- `cas` → `encCas(ty, rd, rt, rn)` — atomically: `if (*rn == rd) *rn = rt; rd = *rn`
- `ldar` → load-acquire, `stlr` → store-release
- `ldaxr`/`stlxr` — load/store exclusive for loop-based fallback

#### 7. ISA Lowering — x86-64

`compiler/codegen/native/isa/x64/lower.zig`:

x86-64 has TSO (Total Store Order), so:
- **Atomic load** = regular `mov` (all loads are acquire on x64)
- **Atomic store** = regular `mov` + `mfence` (or use `xchg` which has implicit lock)
- **Atomic add** = `lock xadd`
- **Atomic CAS** = `lock cmpxchg`
- **Atomic exchange** = `xchg` (implicit lock prefix)

x64 machine instruction types to add to `isa/x64/inst/mod.zig`:

```zig
// Atomic instructions
lock_xadd: struct { size: OperandSize, src: Gpr, dst: SyntheticAmode },
lock_cmpxchg: struct { size: OperandSize, expected: Gpr, new: Gpr, addr: SyntheticAmode },
xchg: struct { size: OperandSize, src: Gpr, dst: SyntheticAmode },
mfence: void,
```

x64 encoding in `isa/x64/inst/emit.zig`:

```zig
.lock_xadd => |op| {
    // F0 0F C1 /r  — LOCK XADD r/m64, r64
    try sink.put1(0xF0);  // LOCK prefix
    // REX.W prefix for 64-bit
    const rex = RexPrefix.twoOp(op.src.hwEnc(), 0, true, false);
    try rex.encode(sink);
    try sink.put1(0x0F);
    try sink.put1(0xC1);
    try emitModrmSibDisp(sink, op.src.hwEnc(), op.dst.amode, 0, null);
},

.lock_cmpxchg => |op| {
    // F0 0F B1 /r  — LOCK CMPXCHG r/m64, r64
    // Compares RAX with r/m64. If equal, ZF is set and r64 is stored to r/m.
    // If not equal, ZF is cleared and r/m64 is loaded into RAX.
    try sink.put1(0xF0);  // LOCK prefix
    const rex = RexPrefix.twoOp(op.new.hwEnc(), 0, true, false);
    try rex.encode(sink);
    try sink.put1(0x0F);
    try sink.put1(0xB1);
    try emitModrmSibDisp(sink, op.new.hwEnc(), op.addr.amode, 0, null);
},

.xchg => |op| {
    // 87 /r  — XCHG r/m64, r64  (implicit LOCK, full barrier)
    const rex = RexPrefix.twoOp(op.src.hwEnc(), 0, true, false);
    try rex.encode(sink);
    try sink.put1(0x87);
    try emitModrmSibDisp(sink, op.src.hwEnc(), op.dst.amode, 0, null);
},

.mfence => {
    // 0F AE F0  — MFENCE
    try sink.put1(0x0F);
    try sink.put1(0xAE);
    try sink.put1(0xF0);
},
```

### Verification

```cot
var counter: i64 = 0

fn atomicIncrement(arg: i64) void {
    var i: i64 = 0
    while (i < 1000) {
        @atomicAdd(&counter, 1)
        i = i + 1
    }
}

test "atomic add from two threads" {
    var t1 = Thread.spawn(atomicIncrement, 0)
    var t2 = Thread.spawn(atomicIncrement, 0)
    t1.join()
    t2.join()
    @assertEq(@atomicLoad(&counter), 2000)
}
```

### Cotty Use

Lock-free dirty flag for render throttling:
```cot
var dirty: i64 = 0

// IO thread sets dirty after terminal state update
fn onPtyData() void {
    // ... update terminal grid (under mutex) ...
    @atomicStore(&dirty, 1)
}

// Renderer checks dirty on VSync
fn onVSync() void {
    if (@atomicLoad(&dirty) != 0) {
        @atomicStore(&dirty, 0)
        renderFrame()
    }
}
```

---

## Phase 5: Channel — Pure Cot

**Goal:** Typed blocking queue for inter-thread message passing. Built entirely in Cot using Phase 2 (Mutex) and Phase 3 (Condition) — **no compiler changes needed**.

### API Design

```cot
import "std/channel"

fn producer(ch_ptr: i64) void {
    var ch = @intToPtr(*Channel(i64), ch_ptr)
    var i: i64 = 0
    while (i < 100) {
        ch.send(i)
        i = i + 1
    }
    ch.close()
}

fn consumer(ch_ptr: i64) void {
    var ch = @intToPtr(*Channel(i64), ch_ptr)
    while (true) {
        var val = ch.recv()
        if (ch.isClosed() and val == 0) {
            break
        }
        println(val)
    }
}
```

### Implementation

`stdlib/channel.cot`:

```cot
import "std/thread"

/// Fixed-capacity blocking channel for inter-thread communication.
/// Follows Ghostty's BlockingQueue pattern: SPSC ring buffer + mutex + conditions.
struct Channel(T) {
    buf: List(T),       // ring buffer storage
    capacity: i64,
    head: i64,          // next read position
    tail: i64,          // next write position
    count: i64,         // number of items in buffer
    mutex: Mutex,
    not_full: Condition,
    not_empty: Condition,
    closed: bool,
}

impl Channel(T) {
    /// Create a channel with given capacity.
    static fn init(capacity: i64) Channel(T) {
        var buf = List(T).initCapacity(capacity)
        // Pre-fill list to capacity so we can index into it
        var i: i64 = 0
        while (i < capacity) {
            buf.append(undefined)
            i = i + 1
        }
        return Channel(T) {
            buf: buf,
            capacity: capacity,
            head: 0,
            tail: 0,
            count: 0,
            mutex: Mutex.init(),
            not_full: Condition.init(),
            not_empty: Condition.init(),
            closed: false,
        }
    }

    /// Send a value. Blocks if the channel is full.
    fn send(val: T) void {
        mutex.lock()
        while (count == capacity) {
            not_full.wait(mutex)
        }
        buf.set(tail, val)
        tail = (tail + 1) % capacity
        count = count + 1
        not_empty.signal()
        mutex.unlock()
    }

    /// Try to send without blocking. Returns true if sent.
    fn trySend(val: T) bool {
        mutex.lock()
        if (count == capacity) {
            mutex.unlock()
            return false
        }
        buf.set(tail, val)
        tail = (tail + 1) % capacity
        count = count + 1
        not_empty.signal()
        mutex.unlock()
        return true
    }

    /// Receive a value. Blocks if the channel is empty.
    /// Returns the value. If channel is closed and empty, behavior is undefined
    /// (check isClosed() + isEmpty() before calling).
    fn recv() T {
        mutex.lock()
        while (count == 0 and !closed) {
            not_empty.wait(mutex)
        }
        if (count == 0) {
            // closed and empty
            mutex.unlock()
            return undefined
        }
        var val = buf.get(head)
        head = (head + 1) % capacity
        count = count - 1
        not_full.signal()
        mutex.unlock()
        return val
    }

    /// Try to receive without blocking. Returns the value or undefined.
    fn tryRecv() ?T {
        mutex.lock()
        if (count == 0) {
            mutex.unlock()
            return null
        }
        var val = buf.get(head)
        head = (head + 1) % capacity
        count = count - 1
        not_full.signal()
        mutex.unlock()
        return val
    }

    /// Close the channel. Wakes all waiting receivers.
    fn close() void {
        mutex.lock()
        closed = true
        not_empty.broadcast()  // wake all waiting receivers
        mutex.unlock()
    }

    /// Check if the channel has been closed.
    fn isClosed() bool {
        return closed
    }

    /// Check if the channel is empty.
    fn isEmpty() bool {
        return count == 0
    }

    /// Destroy the channel. Must not be in use by any thread.
    fn destroy() void {
        mutex.destroy()
        not_full.destroy()
        not_empty.destroy()
        buf.deinit()
    }
}
```

### Comparison to Ghostty's BlockingQueue

| Feature | Ghostty `BlockingQueue` | Cot `Channel(T)` |
|---------|------------------------|-------------------|
| Storage | Fixed array `[N]T` | `List(T)` pre-filled to capacity |
| Synchronization | `std.Thread.Mutex` + `std.Thread.Condition` | `Mutex` + `Condition` (Phase 2+3) |
| Push blocking | `timeout: instant/forever/ns` | `send()` blocks forever, `trySend()` instant |
| Pop | Non-blocking `?T` | `recv()` blocks, `tryRecv()` non-blocking |
| Drain | Lock-holding iterator | Not yet (can add later) |
| Close | N/A | `close()` + `broadcast()` |
| Capacity | Compile-time `comptime N` | Runtime `capacity: i64` |

### Verification

```cot
import "std/thread"
import "std/channel"

var ch = Channel(i64).init(16)

fn producer(arg: i64) void {
    var i: i64 = 0
    while (i < 100) {
        ch.send(i)
        i = i + 1
    }
}

fn consumer(arg: i64) void {
    var sum: i64 = 0
    var i: i64 = 0
    while (i < 100) {
        sum = sum + ch.recv()
        i = i + 1
    }
    @assertEq(sum, 4950)  // sum of 0..99
}

test "channel producer-consumer" {
    var p = Thread.spawn(producer, 0)
    var c = Thread.spawn(consumer, 0)
    p.join()
    c.join()
    ch.destroy()
}
```

### Cotty Use

Replace the current single-threaded message passing with a real inter-thread mailbox:

```cot
import "std/channel"
import "std/thread"

// Message types from UI → IO thread (like Ghostty's termio.Message)
enum IOMessage {
    WriteBytes,     // keyboard input to send to PTY
    Resize,         // terminal resize
    Close,          // shutdown
}

struct IOMessageData {
    kind: IOMessage,
    data_ptr: i64,
    data_len: i64,
}

// Global mailbox — 64-slot ring buffer
var io_mailbox = Channel(IOMessageData).init(64)

// UI thread sends:
fn onKeyDown(bytes: *u8, len: i64) void {
    io_mailbox.send(IOMessageData {
        kind: IOMessage.WriteBytes,
        data_ptr: @ptrToInt(bytes),
        data_len: len,
    })
}

// IO thread drains:
fn ioThreadLoop(arg: i64) void {
    while (true) {
        var msg = io_mailbox.recv()
        switch (msg.kind) {
            .WriteBytes => {
                fd_write(pty_fd, msg.data_ptr, msg.data_len)
            },
            .Resize => {
                // ioctl TIOCSWINSZ
            },
            .Close => {
                break
            },
        }
    }
}
```

---

## Cotty Adoption Plan

Each phase unlocks a specific architectural improvement:

### After Phase 1 (Threads)
Spawn a background PTY reader thread. Currently Cotty reads PTY on main thread via DispatchSource, which blocks rendering during heavy output (`cat large_file`). With threads:
- Background thread does blocking `read()` on PTY fd
- Feeds bytes through VT parser
- Signals main thread when done

**Impact:** Unblocks UI during heavy shell output.

### After Phase 2 (Mutex)
Protect `TerminalState.grid` with a mutex:
- IO thread locks while parsing VT sequences and mutating grid cells
- Renderer (main thread) locks while reading grid for Metal draw call
- Same pattern as Ghostty's `renderer_state.mutex`

**Impact:** Safe concurrent access to terminal grid.

### After Phase 3 (Condition Variables)
Build a blocking message queue (manually, or wait for Phase 5 Channel):
- UI thread sends keyboard/resize events to IO thread's mailbox
- IO thread blocks on empty mailbox (no busy-waiting)
- Same pattern as Ghostty's `Mailbox = BlockingQueue(Message, 64)`

**Impact:** Efficient inter-thread communication without polling.

### After Phase 4 (Atomics)
Lock-free dirty flag for render throttling:
- IO thread sets `@atomicStore(&dirty, 1)` after grid mutation
- VSync callback checks `@atomicLoad(&dirty)` — only renders when state changed
- No mutex needed for the flag itself

Also enables atomic ARC refcounting for multi-threaded safety (ref `CONCURRENCY_DESIGN.md` Phase 3).

**Impact:** Efficient VSync rendering, no unnecessary frames.

### After Phase 5 (Channel)
Replace raw mutex+condition queue with typed `Channel(T)`:
- `Channel(IOMessage)` for UI → IO
- `Channel(RenderMessage)` for IO → Renderer (resize, font change, etc.)
- Type-safe, impossible to send wrong message type

**Impact:** Clean, type-safe message passing — matches Ghostty's architecture.

---

## File Change Summary

### Phase 1: Thread Creation
| File | Change | Effort |
|------|--------|--------|
| `compiler/driver.zig` | Add 6 names to `runtime_func_names`, import + call `thread_native` | Small |
| `compiler/codegen/native/thread_native.zig` | **NEW** — 3 runtime functions (spawn, join, detach) | Medium |
| `stdlib/thread.cot` | **NEW** — `Thread` struct with `spawn`, `join`, `detach` | Small |

### Phase 2: Mutex
| File | Change | Effort |
|------|--------|--------|
| `compiler/driver.zig` | Add 10 names to `runtime_func_names` | Small |
| `compiler/codegen/native/thread_native.zig` | Add 5 functions (init, lock, unlock, trylock, destroy) | Medium |
| `stdlib/thread.cot` | Add `Mutex` struct | Small |

### Phase 3: Condition Variables
| File | Change | Effort |
|------|--------|--------|
| `compiler/driver.zig` | Add 10 names to `runtime_func_names` | Small |
| `compiler/codegen/native/thread_native.zig` | Add 5 functions (init, wait, signal, broadcast, destroy) | Medium |
| `stdlib/thread.cot` | Add `Condition` struct | Small |

### Phase 4: Atomic Operations
| File | Change | Effort |
|------|--------|--------|
| `compiler/frontend/ast.zig` | Add 5 `BuiltinKind` variants + string map entries | Small |
| `compiler/frontend/checker.zig` | Add 5 type-check cases in `checkBuiltinCall()` | Small |
| `compiler/frontend/lower.zig` | Add 5 lowering cases in `lowerBuiltinCall()` | Small |
| `compiler/ssa/op.zig` | Add 5+ `OpInfo` entries to `op_info_table` | Small |
| `compiler/codegen/native/ssa_to_clif.zig` | Add 5 cases in `translateValue()` | Medium |
| `compiler/ir/clif/instructions.zig` | Add 4 atomic opcodes to `Opcode` enum | Small |
| `compiler/codegen/native/isa/aarch64/lower.zig` | Add 4 CLIF → machine instruction cases | Medium |
| `compiler/codegen/native/isa/x64/lower.zig` | Add 4 CLIF → machine instruction cases | Medium |
| `compiler/codegen/native/isa/x64/inst/mod.zig` | Add atomic machine instruction types | Medium |
| `compiler/codegen/native/isa/x64/inst/emit.zig` | Add atomic instruction encoding | Medium |

### Phase 5: Channel (Pure Cot — No Compiler Changes)
| File | Change | Effort |
|------|--------|--------|
| `stdlib/channel.cot` | **NEW** — `Channel(T)` generic struct | Medium |

---

## Wasm Considerations

### Phase 1-3 (Threads, Mutex, Condition)
Wasm does not have pthreads. Options:
- **Stub out** — runtime functions return -1 or no-op. `Thread.spawn` on Wasm is a compile error or no-op.
- **Wasm threads proposal** — uses `SharedArrayBuffer` + Web Workers. Future option, not broadly available.
- **Recommended:** For now, make threading native-only. Add a compile-time guard:

```cot
import "std/thread"

fn main() void {
    if (@targetOs() == "wasm32") {
        @compileError("threading not supported on wasm target")
    }
    var t = Thread.spawn(worker, 0)
    t.join()
}
```

In `thread_native.zig`, skip generating wrappers when `isa.target == .wasm32` — the stubs in `runtime_func_names` will resolve to no-ops.

### Phase 4 (Atomics)
Wasm has native atomic instructions (`i64.atomic.load`, `i64.atomic.store`, `i64.atomic.rmw.add`, `i64.atomic.rmw.cmpxchg`) when threads are enabled. These can be emitted directly from `ssa_to_wasm.zig` if/when Wasm threads support is added. For now, on single-threaded Wasm, atomics can be lowered to regular loads/stores (no other threads to race with).

### Phase 5 (Channel)
Pure Cot — works on any target where Mutex and Condition work. On Wasm, Channel would be single-threaded (synchronous send/recv), which is fine for message-passing within an event loop.

---

## Relationship to CONCURRENCY_DESIGN.md

The existing `CONCURRENCY_DESIGN.md` describes a Go-style `spawn` + channels model with a work-stealing scheduler. This threading design is **Phase 3 of that roadmap** — low-level primitives for library authors.

Key differences:
- **This document:** OS threads (`pthread_create`), manual mutex/condition, explicit thread management. For Cotty's Ghostty-like architecture where you need exactly N named threads with specific roles.
- **CONCURRENCY_DESIGN.md Phase 2:** Lightweight `spawn {}` tasks scheduled onto a thread pool. Higher-level, automatic parallelism. For general-purpose concurrent Cot programs.

Both share the same compiler foundation:
- Phase 1-3 here (threads, mutex, cond) are prerequisites for the work-stealing scheduler in `CONCURRENCY_DESIGN.md`
- Phase 4 here (atomics) is needed for atomic ARC refcounting in `CONCURRENCY_DESIGN.md`
- Phase 5 here (Channel) is a simpler version of the channels in `CONCURRENCY_DESIGN.md`

**Implementation order:** This document first (OS-level primitives), then `CONCURRENCY_DESIGN.md` Phase 2 builds on top.
