# Concurrency Design

Cot's concurrency roadmap, building on the existing async/await foundation.

**Status:** Phase 1 (async/await) complete. Phase 3 (OS-level primitives) **DONE** — see `threading-design.md`. Phase 2 (spawn + work-stealing) planned.

---

## Current State (0.4)

Cot has single-threaded async I/O via `async fn` / `await`:

- **Native target:** Eager evaluation + OS event loop (kqueue on macOS, epoll on Linux)
- **Wasm target:** Stackless state machine transformation (Rust coroutine pattern)
- **9 builtins:** kqueue/epoll/fcntl, async accept/read/write/connect
- **`std/async`:** Platform-abstracted async I/O wrappers

This handles I/O-bound concurrency (HTTP servers, database queries). What's missing is CPU parallelism.

---

## Three-Phase Approach

```
Phase 1: async/await (DONE)
  Single-threaded I/O concurrency. Event loop. No parallelism.

Phase 2: spawn + channels (0.5)
  Go-style lightweight tasks. True parallelism across CPU cores.
  Message-passing via channels. No shared mutable state.

Phase 3: Low-level primitives (DONE — 0.3.x)
  Thread, Mutex, Condition, Atomics, Channel(T). See threading-design.md.
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

### Atomic ARC

Current ARC (`compiler/codegen/arc.zig`) uses non-atomic refcount operations. For multi-threaded programs:

**Wasm target:** No change needed. Wasm is single-threaded (shared memory + atomics exist in Wasm threads proposal, but not yet in Cot's target).

**Native target:** Replace refcount increment/decrement with atomic operations:
- `retain`: `@atomicRmw(.Add, &header.refcount, 1, .monotonic)`
- `release`: `@atomicRmw(.Sub, &header.refcount, 1, .release)` + acquire fence on free

This matches Swift's ARC implementation. The Zig code in `arc.zig` already compiles to native — switching to atomics is a targeted change to the retain/release functions.

### Wasm Considerations

Wasm doesn't have OS threads (yet). The Wasm threads proposal adds shared memory and atomics but isn't universally supported. Cot's approach:

- **`--target=native`**: Full parallelism via OS threads, work-stealing scheduler
- **`--target=wasm32`**: `spawn` compiles but runs sequentially (same thread). Channels work as synchronous queues. This matches Go's GOMAXPROCS=1 behavior.
- **`--target=wasm32-threads`** (future): When Wasm threads are broadly available, spawn uses SharedArrayBuffer + Web Workers

### Deliverables

- [ ] `spawn` keyword parsing and AST node
- [ ] Closure-style environment capture for spawn blocks
- [ ] `@spawn` runtime builtin (Wasm stub + native override)
- [ ] `Channel(T)` type with `new`, `send`, `recv`, `close`
- [ ] `select` statement parsing and lowering
- [ ] Work-stealing scheduler in `compiler/codegen/scheduler.zig`
- [ ] Atomic ARC for native target
- [ ] Tests: parallel computation, channel ping-pong, producer-consumer

---

## Phase 3: Low-Level Primitives — DONE

**Implemented in 0.3.x.** See `threading-design.md` for full design, implementation audit, and reference faithfulness proof.

Exposed through `std/thread` and `std/channel`. Compiler builtins for atomics (`@atomicLoad`, `@atomicStore`, `@atomicAdd`, `@atomicCAS`, `@atomicExchange`).

### Synchronization Types

```cot
import "std/sync"

fn main() void {
    var mutex = Mutex.new()
    var counter = Atomic(i64).new(0)

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

| Type | Purpose |
|------|---------|
| `Mutex` | Mutual exclusion lock |
| `RwLock` | Reader-writer lock (multiple readers OR one writer) |
| `Atomic(T)` | Lock-free atomic operations on primitive types |
| `WaitGroup` | Wait for N tasks to complete |
| `Once` | Run initialization exactly once |

### Implementation

All synchronization primitives are backed by OS facilities on native:
- Mutex → `pthread_mutex` (POSIX) or `os_unfair_lock` (macOS)
- Condition → `pthread_cond`
- Atomics → compiler intrinsics (ARM64 `ldaxr`/`stlxr`, x64 `lock` prefix)

On Wasm, these are no-ops or single-threaded stubs (same as spawn).

### Deliverables

- [x] `Thread` with `spawn()`, `join()`, `detach()` — `std/thread`
- [x] `Mutex` type with `init()`, `lock()`, `unlock()`, `tryLock()`, `destroy()` — `std/thread`
- [x] `Condition` with `init()`, `wait()`, `signal()`, `broadcast()`, `destroy()` — `std/thread`
- [x] `Channel(T)` with `init()`, `send()`, `recv()`, `close()`, `destroy()` — `std/channel`
- [x] Atomic builtins: `@atomicLoad`, `@atomicStore`, `@atomicAdd`, `@atomicCAS`, `@atomicExchange`
- [ ] `RwLock` with `readLock()`, `writeLock()` — future
- [ ] `WaitGroup` with `add`, `done`, `wait` — future
- [ ] `Atomic(T)` wrapper struct — future (builtins work directly for now)

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
