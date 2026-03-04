# Concurrency Design

Cot's concurrency roadmap, building on the existing async/await foundation.

**Status:** Phase 1 (async/await) **DONE**. Phase 3 (primitives) **DONE** — Thread, Mutex, Condition, RwLock, WaitGroup, Atomic(T), Channel(T) all implemented and tested (27 tests). Phase 2 (spawn + work-stealing) **DONE** — spawn with work-stealing scheduler, `select` statement, Channel tryRecv/trySend/len, atomic weak refs all complete (0.3.5).

---

## Current State (0.3.4)

Cot has single-threaded async I/O via `async fn` / `await`:

- **Native target:** Eager evaluation + OS event loop (kqueue on macOS, epoll on Linux)
- **Wasm target:** Stackless state machine transformation (Rust coroutine pattern)
- **9 builtins:** kqueue/epoll/fcntl, async accept/read/write/connect
- **`std/async`:** Platform-abstracted async I/O wrappers

Additionally, OS-level threading primitives are available (Phase 3):

- **`std/thread`:** Thread, Mutex, Condition, RwLock, WaitGroup, Atomic(T)
- **`std/channel`:** Channel(T) with init/send/recv/close (mutex+condvar based)
- **Atomic builtins:** `@atomicLoad`, `@atomicStore`, `@atomicAdd`, `@atomicCAS`, `@atomicExchange`
- **Spawn MVP:** `spawn {}` blocks with global queue scheduler (no per-worker local queues yet)
- **Atomic ARC:** Strong/unowned refcounts use atomic operations (weak refs still non-atomic)

---

## Three-Phase Approach

```
Phase 1: async/await (DONE)
  Single-threaded I/O concurrency. Event loop (kqueue/epoll). No parallelism.

Phase 2: spawn + work-stealing scheduler (DONE — 0.3.5)
  Work-stealing scheduler with per-worker Chase-Lev deques. select statement.
  Channel tryRecv/trySend/len. Atomic weak references.

Phase 3: Low-level primitives (DONE — 0.3.x)
  Thread, Mutex, Condition, RwLock, WaitGroup, Atomic(T), Channel(T),
  atomic builtins. Spawn MVP with global queue. Atomic ARC (strong/unowned).
  27 E2E tests pass. See archive/threading-design.md.
```

---

## Phase 2: spawn + Channels

### Syntax

```cot
fn main() void {
    var ch = Channel(i64).new(0)  // unbuffered

    spawn {
        var i: i64 = 0
        while (i < 100) {
            ch.send(i * 2)
            i = i + 1
        }
        ch.close()
    }

    // Receive until closed
    while (ch.recv()) |value| {
        println(value)
    }
}
```

### Select Statement

```cot
fn main() void {
    var ch1 = Channel(i64).new(0)
    var ch2 = Channel(string).new(0)

    select {
        value from ch1 => {
            println(value)
        }
        msg from ch2 => {
            println(msg)
        }
    }
}
```

### Runtime Architecture

```
┌──────────────────────────────────────────────────────────┐
│  Cot Runtime (linked into native binary)                 │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  ┌───────────┐ ┌───────────┐ ┌───────────┐              │
│  │ Worker 0  │ │ Worker 1  │ │ Worker N  │   N = cores  │
│  │ (OS thread)│ │ (OS thread)│ │ (OS thread)│             │
│  │ [local Q] │ │ [local Q] │ │ [local Q] │              │
│  └─────┬─────┘ └─────┬─────┘ └─────┬─────┘              │
│        └──────────────┼──────────────┘                   │
│                       │                                  │
│          ┌────────────▼────────────┐                     │
│          │     Global Queue        │                     │
│          └────────────┬────────────┘                     │
│                       │                                  │
│    ┌──────────────────┼──────────────────┐               │
│    ▼                  ▼                  ▼               │
│  Shared Heap     Channel Layer      Atomic ARC           │
│  (linear mem)    [Ch1][Ch2]...      Refcounts            │
│                                                          │
└──────────────────────────────────────────────────────────┘
```

Each worker is an OS thread running a work-stealing loop. Spawned tasks are functions with captured state, scheduled onto workers.

### Work-Stealing Scheduler

The scheduler follows Go's GMP model and Tokio's work-stealing pattern:

1. **`spawn { ... }`** creates a task (function pointer + captured environment), pushes to global queue
2. Each worker pops from its local queue first (fast path, no contention)
3. If empty, tries the global queue (one lock)
4. If empty, steals from another worker's local queue (deque steal)

This is implemented as a Cot runtime module linked into every concurrent binary. The scheduler code itself is Zig (in `compiler/codegen/`), compiled into the runtime like `arc.zig` and `wasi_runtime.zig`.

```
scheduler.zig (new):
  - ThreadPool: N workers (1 per core), global MPMC queue
  - Worker: OS thread + local SPSC deque + steal-half from peers
  - Task: { fn_ptr, env_ptr, stack_ptr }
  - spawn(): push task to global queue, wake idle worker
```

### Implementation in the Pipeline

Spawn compiles through the existing pipeline with minimal changes:

1. **Frontend:** `spawn { body }` parsed as a spawn expression. The body becomes an anonymous function.
2. **Checker:** Validates captured variables. Mutable captures require `var` (no implicit sharing).
3. **Lowerer:** Emits a closure-like capture (reuses closure infrastructure) + a call to `@spawn(fn_ptr, env_ptr)`.
4. **Codegen:** `@spawn` is a runtime function. On Wasm target, it's a no-op stub (Wasm is single-threaded). On native, it calls the work-stealing scheduler.

This follows the same pattern as networking functions: Wasm stubs + native CLIF IR implementations.

### Channel Implementation

Channels are heap-allocated objects managed by ARC:

```
Channel memory layout (linear memory):
  offset 0:  ARC header (8 bytes)
  offset 8:  capacity (i64)
  offset 16: buffer_ptr (i64) — ring buffer
  offset 24: head (i64)
  offset 32: tail (i64)
  offset 40: mutex (platform mutex)
  offset 56: not_empty (condition var)
  offset 72: not_full (condition var)
  offset 88: closed (i64, 0 or 1)
```

Operations:
- `send(value)`: lock, wait if full, push to ring buffer, signal not_empty
- `recv()`: lock, wait if empty (return null if closed), pop from ring buffer, signal not_full
- `close()`: set closed flag, broadcast to all waiters

On Wasm target, channels work synchronously (single-threaded — send blocks until recv in same event loop tick, or errors). On native, they use OS mutexes and condition variables.

### Atomic ARC — DONE (strong/unowned/weak)

Native ARC (`compiler/codegen/native/arc_native.zig`) uses atomic refcount operations for all reference types, matching Swift's `HeapObject.cpp` pattern:

- `retain`: `atomicRmwAdd(&refcount, STRONG_RC_ONE)` — relaxed ordering (Swift pattern)
- `release`: `atomicRmwAdd(&refcount, -STRONG_RC_ONE)` — ARM64 `ldaddal` (acquire-release), x64 `lock xadd` (seq-cst)
- `unowned_retain` / `unowned_release`: same atomic pattern with `UNOWNED_RC_ONE`
- `weakRetain` / `weakRelease` / `weakFormReference`: `atomicRmwAdd` for weak refcount (0.3.5)

**Wasm target:** No change needed. Wasm is single-threaded.

### Wasm Considerations

Wasm doesn't have OS threads (yet). The Wasm threads proposal adds shared memory and atomics but isn't universally supported. Cot's approach:

- **`--target=native`**: Full parallelism via OS threads, work-stealing scheduler
- **`--target=wasm32`**: `spawn` compiles but runs sequentially (same thread). Channels work as synchronous queues. This matches Go's GOMAXPROCS=1 behavior.
- **`--target=wasm32-threads`** (future): When Wasm threads are broadly available, spawn uses SharedArrayBuffer + Web Workers

### Deliverables

- [x] `spawn` keyword parsing and AST node (contextual keyword)
- [x] Closure-style environment capture for spawn blocks (`lowerSpawnExpr` in lower.zig)
- [x] `sched_spawn` runtime function (CLIF IR in `scheduler_native.zig`)
- [x] `Channel(T)` in `std/channel` — init/send/recv/close/tryRecv/trySend/len (mutex+condvar based)
- [x] `select` statement parsing, type-checking, and lowering (simple path + general N-case)
- [x] `sched_select` runtime function (CLIF IR, poll-based channel select)
- [x] Scheduler runtime: work-stealing with per-worker Chase-Lev deques (Go `runqsteal` pattern)
- [x] Atomic ARC for native target (strong/unowned/weak — all atomic)
- [x] Tests: `test/e2e/spawn.cot` — 6 tests, `test/e2e/select.cot` — 9 tests

---

## Phase 3: Low-Level Primitives — COMPLETE

**Implemented in 0.3.x.** See `archive/threading-design.md` for full design, implementation audit, and reference faithfulness proof.

Exposed through `std/thread` and `std/channel`. Compiler builtins for atomics (`@atomicLoad`, `@atomicStore`, `@atomicAdd`, `@atomicCAS`, `@atomicExchange`).

### Synchronization Types

```cot
import "std/thread"

fn main() void {
    var mutex = Mutex.init()
    var counter = Atomic(i64).init(0)

    spawn {
        mutex.lock()
        defer mutex.unlock()
        // critical section
    }

    spawn {
        counter.fetchAdd(1)
    }
}
```

### Types

| Type | Purpose | Status |
|------|---------|--------|
| `Thread` | OS thread spawn/join/detach | DONE |
| `Mutex` | Mutual exclusion lock | DONE |
| `Condition` | Condition variable (wait/signal/broadcast) | DONE |
| `Channel(T)` | Blocking queue for inter-thread messaging | DONE |
| `RwLock` | Reader-writer lock (multiple readers OR one writer) | DONE |
| `WaitGroup` | Wait for N tasks to complete | DONE |
| `Atomic(T)` | Lock-free atomic operations on primitive types | DONE |

### Implementation

Low-level primitives (Thread, Mutex, Condition) are backed by OS facilities on native, generated as CLIF IR in `thread_native.zig`. Higher-level types (RwLock, WaitGroup, Atomic(T), Channel(T)) are pure Cot built on top of the low-level primitives:

- Thread → `pthread_create`/`pthread_join`/`pthread_detach`
- Mutex → `pthread_mutex_init`/`lock`/`unlock`/`trylock`/`destroy`
- Condition → `pthread_cond_init`/`wait`/`signal`/`broadcast`/`destroy`
- Atomics → compiler intrinsics (ARM64 `ldaxr`/`stlxr`, x64 `lock` prefix)
- RwLock → Mutex + 2 Conditions + reader/writer counters (writer-preferring)
- WaitGroup → Mutex + Condition + atomic counter (Go pattern)
- Atomic(T) → wraps `@atomicLoad`/`@atomicStore`/`@atomicAdd`/`@atomicCAS`/`@atomicExchange`
- Channel(T) → Mutex + 2 Conditions + ring buffer (Go pattern)

On Wasm, these are no-ops or single-threaded stubs (same as spawn).

### Deliverables

- [x] `Thread` with `spawn()`, `join()`, `detach()` — `std/thread`
- [x] `Mutex` type with `init()`, `lock()`, `unlock()`, `tryLock()`, `destroy()` — `std/thread`
- [x] `Condition` with `init()`, `wait()`, `signal()`, `broadcast()`, `destroy()` — `std/thread`
- [x] `Channel(T)` with `init()`, `send()`, `recv()`, `close()`, `destroy()` — `std/channel`
- [x] Atomic builtins: `@atomicLoad`, `@atomicStore`, `@atomicAdd`, `@atomicCAS`, `@atomicExchange`
- [x] `RwLock` with `readLock()`, `readUnlock()`, `writeLock()`, `writeUnlock()`, `destroy()` — `std/thread` (writer-preferring, built on Mutex + Condition, Go sync.RWMutex pattern)
- [x] `WaitGroup` with `add()`, `done()`, `wait()`, `destroy()` — `std/thread` (Go sync.WaitGroup pattern)
- [x] `Atomic(T)` with `init()`, `load()`, `store()`, `fetchAdd()`, `compareAndSwap()`, `exchange()` — `std/thread` (wraps atomic builtins)

---

## Design Decisions

### Why Not Green Threads?

Green threads (goroutines, Erlang processes) require stack switching — either via OS `ucontext` or custom assembly. Wasm has no stack switching (the stack-switching proposal exists but isn't shipped).

Cot's approach: OS threads + work-stealing. This is simpler, works on native today, and avoids the Wasm stack-switching dependency. If Wasm stack switching ships, green threads become possible as a future optimization.

### Why Message-Passing Default?

Channels as the primary concurrency primitive (with low-level locks available for experts) follows Go's proven model: "Don't communicate by sharing memory; share memory by communicating."

This is easier to reason about, prevents data races by default, and works naturally with ARC (values sent through channels transfer ownership).

### Why Not async/await for Parallelism?

async/await is for I/O concurrency (waiting on network/disk). spawn + channels is for CPU parallelism (using multiple cores). They complement each other:

```cot
// I/O concurrency: async/await
async fn handleRequest(conn: Connection) void {
    const body = try await conn.read()
    const result = processBody(body)  // CPU work, runs on current thread
    try await conn.write(result)
}

// CPU parallelism: spawn + channels
fn processInParallel(items: List(Item)) List(Result) {
    var results = Channel(Result).new(items.len())
    for (items) |item| {
        spawn { results.send(processItem(item)) }
    }
    // collect results...
}
```

---

## References

| Concept | Reference |
|---------|-----------|
| Work-stealing scheduler | Go runtime (`runtime/proc.go`), Tokio (`tokio/src/runtime/`) |
| Channel design | Go channels (`runtime/chan.go`) |
| Atomic ARC | Swift (`stdlib/public/runtime/HeapObject.cpp`) |
| Select statement | Go select (`runtime/select.go`) |

*Extracted from pre-0.1 design docs (`~/cotlang/roadmap/03-feature-roadmap.md`), refactored for Cot 0.3+ dual-backend architecture (native + Wasm).*
