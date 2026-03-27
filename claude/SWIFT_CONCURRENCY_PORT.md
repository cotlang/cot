# Cot Concurrency Rewrite: Swift 1:1 Port

**Date:** 2026-03-26 (design), 2026-03-27 (ALL IMPLEMENTED)
**Author:** John Cottrell / Claude
**Status:** IMPLEMENTED — All 13 phases complete, 409 native + 36 Wasm concurrency tests pass, selfcot ported.
**Scope:** Complete replacement of Cot's Go-influenced concurrency model with Swift's structured concurrency

---

## Executive Summary

Cot's current concurrency model is a loosely-assembled collection of Go patterns (work-stealing scheduler, channels, select) with async/await syntax bolted on. It was built quickly, is barely used in production code, and has known bugs (channel ring buffer corruption, incomplete Wasm multi-await state machine).

Swift's concurrency model (introduced Swift 5.5, hardened through Swift 6) is a complete, cohesive system designed to eliminate data races at compile time. Since Cot already ports Swift's ARC and VWT systems 1:1, adopting Swift's concurrency model creates a unified memory + concurrency architecture rather than a Go/Swift Frankenstein.

**This document specifies the complete removal of Go-style concurrency and a ground-up port of Swift's concurrency stack.**

---

## Table of Contents

1. [What Gets Deleted](#1-what-gets-deleted)
2. [Architecture Overview](#2-architecture-overview)
3. [Phase 1: Task Runtime](#3-phase-1-task-runtime)
4. [Phase 2: async/await Rewrite](#4-phase-2-asyncawait-rewrite)
5. [Phase 3: Actors](#5-phase-3-actors)
6. [Phase 4: Sendable](#6-phase-4-sendable)
7. [Phase 5: Structured Concurrency (TaskGroup)](#7-phase-5-structured-concurrency-taskgroup)
8. [Phase 6: AsyncSequence & AsyncStream](#8-phase-6-asyncsequence--asyncstream)
9. [Phase 7: Global Actors & @MainActor](#9-phase-7-global-actors--mainactor)
10. [Phase 8: Task Cancellation & Task Locals](#10-phase-8-task-cancellation--task-locals)
11. [Phase 9: Wasm Target](#11-phase-9-wasm-target)
12. [Phase 10: Continuations (Sync-to-Async Bridge)](#12-phase-10-continuations-sync-to-async-bridge)
13. [Cot Syntax Mapping](#13-cot-syntax-mapping)
14. [ARC Integration](#14-arc-integration)
15. [Implementation Order & Dependencies](#15-implementation-order--dependencies)
16. [Reference File Map](#16-reference-file-map)

---

## 1. What Gets Deleted

### Compiler Code (~2,500 lines)

| File | Lines | What | Replacement |
|------|-------|------|-------------|
| `compiler/codegen/native/scheduler_native.zig` | ~1,400 | Chase-Lev work-stealing scheduler | Cooperative executor |
| `compiler/codegen/native/thread_native.zig` | ~800 | Raw pthread wrappers (thread_spawn, mutex, cond, rwlock, waitgroup) | Actor serial executor + Task runtime |
| `compiler/frontend/lower.zig` (async sections) | ~400 | Eager async, spawn lowering, select stub | Swift-style task creation + actor hop |

### Standard Library (~615 lines)

| File | Lines | What | Replacement |
|------|-------|------|-------------|
| `stdlib/async.cot` | ~230 | Event loop (kqueue/epoll), async I/O wrappers | Executor-integrated I/O |
| `stdlib/thread.cot` | ~250 | Thread, Mutex, Condition, RwLock, WaitGroup, Atomic(T) | Internal to executor; Atomic stays as stdlib |
| `stdlib/channel.cot` | ~135 | Channel(T) SPSC ring buffer | New Channel(T) with backpressure + AsyncStream for callback bridging |

### Tests (~1,060 lines)

| File | Lines | Status |
|------|-------|--------|
| `test/e2e/async.cot` | 198 | Rewrite for new async model |
| `test/e2e/threading.cot` | 382 | Delete (raw threading replaced by actors) |
| `test/e2e/spawn.cot` | 104 | Rewrite as Task {} |
| `test/e2e/select.cot` | 132 | Delete (replaced by Channel + `for await in`) |
| `test/e2e/thread_basic.cot` | 28 | Delete |
| `test/e2e/event_loop.cot` | 189 | Rewrite for executor model |
| `test/e2e/browser_async.cot` | 29 | Rewrite |

### Parser/AST Changes

| Token/Node | Action |
|------------|--------|
| `kw_spawn` | **Delete** — replaced by `Task {}` |
| `kw_select` | **Delete** — replaced by `for await in` on Channel/AsyncSequence |
| `SpawnExpr` | **Delete** — replaced by TaskExpr |
| `SelectExpr` | **Delete** |
| `kw_async` | **Keep** — same keyword, new semantics |
| `kw_await` | **Keep** — same keyword, new semantics |
| `kw_actor` | **Add** |
| `kw_nonisolated` | **Add** |
| `kw_sending` | **Add** |
| `kw_isolated` | **Add** |

### What Stays

| Component | Reason |
|-----------|--------|
| `@atomicLoad/Store/Add/CAS/Exchange` | Universal primitive, Swift has equivalent |
| `Atomic(T)` in stdlib | Useful low-level building block |
| `stdlib/process.cot` | Subprocess management, orthogonal to concurrency model |
| async/await keywords | Same syntax, new runtime semantics |
| Wasm state machine approach | Correct for Wasm target, just needs executor integration |

---

## 2. Architecture Overview

### Swift's Concurrency Stack (what we're porting)

```
┌─────────────────────────────────────────────────┐
│  User Code                                       │
│  async fn, await, actor, Task {}, TaskGroup      │
├─────────────────────────────────────────────────┤
│  Sendable Checking (compile-time)                │
│  Actor isolation enforcement                     │
│  Data race prevention                            │
├─────────────────────────────────────────────────┤
│  Task Runtime                                    │
│  Task struct, priority, cancellation, locals     │
├─────────────────────────────────────────────────┤
│  Executor Layer                                  │
│  SerialExecutor (actors), TaskExecutor (pools)   │
│  Cooperative thread pool                         │
├─────────────────────────────────────────────────┤
│  Platform Layer                                  │
│  Native: pthread + cooperative pool              │
│  Wasm: single-thread event loop + state machine  │
│  Browser: requestAnimationFrame + microtask      │
├─────────────────────────────────────────────────┤
│  ARC + VWT (already ported)                      │
│  Memory management for task/actor objects         │
└─────────────────────────────────────────────────┘
```

### Key Design Decisions

**1. No function coloring.** Swift requires `await` at call sites but does NOT require callers to be `async`. Cot follows this — `await` blocks in sync contexts (like Rust's `block_on`). This is a deliberate divergence from Swift where synchronous functions cannot call async functions without `Task {}`. In Cot, we keep the current behavior: `await` in sync context blocks the current thread until the result is ready.

**2. Actors are reference types with ARC.** An actor is a heap-allocated object with a serial executor queue. ARC manages its lifetime. When the last reference drops, the actor's deinit runs (on its executor). This matches Swift's `Actor: AnyObject` constraint exactly.

**3. Sendable is a trait (protocol).** Value types (structs with all-Sendable fields, enums) are implicitly Sendable. Actors are implicitly Sendable. Closures crossing actor boundaries must capture only Sendable values. The compiler enforces this at type-check time.

**4. The cooperative executor is the only executor.** Swift supports custom executors (GCD, etc). Cot v1 will ship with a single cooperative thread pool executor. Custom executor support can be added later without breaking user code.

**5. Wasm is single-threaded.** On Wasm, the executor is an event loop. Actors run on the same thread (isolation is still enforced at compile time). `Task.sleep` yields to the event loop. No pthreads on Wasm.

---

## 3. Phase 1: Task Runtime

### Swift Reference
- `references/swift/include/swift/ABI/Task.h` — Task ABI (Job, AsyncTask, AsyncContext)
- `references/swift/stdlib/public/Concurrency/Task.swift` — Task API
- `references/swift/stdlib/public/Concurrency/TaskCancellation.swift` — Cancellation

### Cot Task Struct

```cot
// Internal runtime struct — not directly exposed to users
struct Task(Success) {
    // Heap-allocated task object
    handle: RawPtr          // pointer to TaskObject in heap

    // User API
    fn cancel()
    fn isCancelled() bool
    async fn value() Success  // await the result
}
```

### TaskObject Layout (heap-allocated)

```
Offset  Field               Size    Description
0       metadata            8       TypeMetadata pointer (for ARC)
8       refcount            8       ARC reference count
16      flags               4       JobFlags: priority(3 bits), isAsync, isCancelled, statusMask
20      id                  4       Unique task ID
24      resumeFn            8       TaskContinuationFunction pointer
32      context             8       AsyncContext pointer (suspension state)
40      result              N       Result storage (size depends on Success type)
40+N    parent              8       Parent task pointer (for structured concurrency)
48+N    childHead           8       First child task (linked list)
56+N    statusRecords       8       Status record chain (cancellation handlers, etc.)
```

This matches Swift's `Job` (lines 73-195 of Task.h) + `AsyncTask` extensions.

### Task Priority

```cot
enum TaskPriority {
    high,           // User-interactive
    medium,         // Default
    low,            // Background
    userInitiated,  // Explicit user action
    utility,        // Long-running
    background,     // Not time-sensitive
}
```

Swift reference: `JobFlags.getPriority()` in Task.h line 160.

### Task Creation

```cot
// Unstructured task (inherits actor context)
let task = Task {
    await someAsyncWork()
    return 42
}

// Detached task (no inherited context, no inherited priority)
let task = Task.detached {
    await someAsyncWork()
    return 42
}

// With priority
let task = Task(priority: .high) {
    await someAsyncWork()
}
```

Swift reference: `Task.swift` lines 141-151. Task is `struct Task<Success: Sendable, Failure: Error>`.

### Runtime Functions (native)

These replace `scheduler_native.zig` and `thread_native.zig`:

| Function | Purpose | Swift Reference |
|----------|---------|-----------------|
| `task_create(fn_ptr, context, flags) → TaskPtr` | Allocate + enqueue task | `swift_task_create` |
| `task_destroy(task_ptr)` | Deallocate task | `swift_task_destroy` |
| `task_switch(task_ptr, executor)` | Suspend current, switch to executor | `swift_task_switch` |
| `task_cancel(task_ptr)` | Set cancellation flag | `swift_task_cancel` |
| `task_get_current() → TaskPtr` | Get current task pointer | `swift_task_getCurrent` |
| `task_future_wait(task_ptr) → result` | Block until task completes | `swift_taskFutureWait` |
| `task_enqueue_global(job_ptr)` | Enqueue on global concurrent executor | `swift_task_enqueueGlobal` |
| `task_enqueue_main(job_ptr)` | Enqueue on main executor | `swift_task_enqueueMainExecutor` |

---

## 4. Phase 2: async/await Rewrite

### Current Cot (Go-influenced)

```cot
// Current: eager evaluation — body runs immediately, result stored in future
async fn fetch(url: string) string {
    return httpGet(url)
}
let result = await fetch("https://example.com")
```

### New Cot (Swift-influenced)

```cot
// New: cooperative suspension — body runs on executor, suspends at await points
async fn fetch(url: string) string {
    let response = await httpGet(url)  // suspension point → yields to executor
    return response
}
let result = await fetch("https://example.com")  // caller suspends until result ready
```

**The syntax is identical.** The difference is entirely in how the compiler lowers async functions.

### Async Function Lowering (Native)

Swift's approach: each async function gets an **AsyncContext** (frame) that lives on the heap, not the stack. Suspension saves the program counter into the context and returns to the executor. Resumption loads the context and jumps to the saved PC.

```
AsyncContext layout:
  Offset 0: parent_context      (8 bytes) — caller's AsyncContext
  Offset 8: resume_fn           (8 bytes) — where to resume after await
  Offset 16: flags              (8 bytes) — error flag, etc.
  Offset 24: ...locals...       (N bytes) — spilled locals alive across await
```

Swift reference: `GenCoro.cpp` (coroutine frame layout), `GenConcurrency.cpp` (task/executor integration).

### Lowering Steps

1. **Split at await points.** Each `await` becomes a split between two continuation functions:
   - `fn_name.0(context)` — code before first await
   - `fn_name.1(context)` — code after first await, before second await
   - `fn_name.N(context)` — code after Nth await

2. **Allocate AsyncContext on task allocator.** The task has a slab allocator for frames (avoids malloc per call).

3. **At each await:**
   - Store live locals into AsyncContext
   - Set `context.resume_fn = fn_name.{N+1}`
   - Call `task_switch(current_task, callee_executor)` — this returns to the executor
   - Executor runs other tasks until the awaited value is ready
   - Executor calls `context.resume_fn(context)` — execution continues after await

4. **At function completion:**
   - Store result into parent's expected location
   - Pop AsyncContext
   - Call parent's `resume_fn`

### Async Function Lowering (Wasm)

Wasm keeps the state machine approach (since Wasm can't do stack switching), but integrates it with the executor:

1. Each async function becomes a state machine (poll function)
2. The event loop (executor) calls `poll()` repeatedly
3. When poll returns `Pending`, the event loop runs other tasks
4. When poll returns `Ready(result)`, the parent continuation resumes

This is identical to Rust's `Future::poll()` model, which Cot already partially implements.

Swift reference: Swift on Wasm is not yet official, but the state machine approach aligns with how SwiftWasm (community fork) handles it.

---

## 5. Phase 3: Actors

### Swift Reference
- `references/swift/include/swift/ABI/Actor.h` — Actor ABI (DefaultActor, PrivateData)
- `references/swift/stdlib/public/Concurrency/Actor.swift` — Actor protocol
- `references/swift/include/swift/ABI/Executor.h` — SerialExecutorRef
- `references/swift/lib/Sema/TypeCheckConcurrency.cpp` — Actor isolation checking

### What is an Actor?

An actor is a **reference type** (like a class) whose mutable state is **isolated** — only accessible from within the actor's serial executor context. Cross-actor access requires `await`, which suspends the caller and enqueues the work on the actor's executor.

```cot
actor BankAccount {
    var balance: int = 0

    fn deposit(amount: int) {
        balance += amount   // safe: inside actor isolation
    }

    fn getBalance() int {
        return balance      // safe: inside actor isolation
    }
}

// Outside the actor:
let account = BankAccount()
await account.deposit(100)          // crosses isolation boundary → requires await
let bal = await account.getBalance() // crosses isolation boundary → requires await
```

### Actor Object Layout

```
Offset  Field               Size    Description
0       metadata            8       TypeMetadata pointer (for ARC)
8       refcount            8       ARC reference count
16      privateData         N×8     Actor private data (executor state)
16+N×8  ...user fields...           User-declared stored properties
```

Swift reference: `Actor.h` lines 33-46. `DefaultActor` has `PrivateData[NumWords_DefaultActor]` for executor state (job queue, lock, status).

The `PrivateData` region contains:
- **Job queue head** — linked list of pending jobs to run on this actor
- **Status flags** — idle, running, scheduled
- **Lock** — lightweight spinlock for queue manipulation

### Serial Executor

Each actor has a **serial executor** — a single-threaded execution context. Jobs enqueued on the executor run one at a time, in order. This is what provides data race safety without explicit locking.

```
SerialExecutorRef:
  Offset 0: identity        (8 bytes) — pointer to actor object (for default actors)
  Offset 8: implementation  (8 bytes) — witness table pointer (for custom executors) + kind bits
```

Swift reference: `Executor.h` lines 58-200.

For default actors, the executor is built into the actor itself. When a task needs to run on an actor:

1. The task enqueues itself as a Job on the actor's queue
2. If the actor is idle, it transitions to "scheduled" and the job is submitted to the global concurrent executor (thread pool)
3. The thread pool picks up the job, sets the actor as "running", and executes the job
4. After the job completes, the actor processes the next job in its queue, or transitions back to idle

### Actor Isolation Rules (Compiler-Enforced)

The checker enforces these rules at type-check time:

1. **Actor-isolated properties** can only be accessed from within the actor (or via `await` from outside)
2. **Actor-isolated methods** can only be called synchronously from within the actor (or via `await` from outside)
3. **`nonisolated` members** can be accessed from anywhere without `await` (must not access mutable state)
4. **Closures capturing actor state** must be `@Sendable` if they escape the actor's isolation domain
5. **Cross-actor calls** pass only `Sendable` values

```cot
actor Counter {
    var count: int = 0

    fn increment() {          // isolated: can access `count`
        count += 1
    }

    nonisolated fn id() int { // nonisolated: cannot access `count`
        return 42             // can be called without await
    }
}
```

Swift reference: `TypeCheckConcurrency.cpp` — this is 330KB of isolation checking logic. The core algorithm:
- Each declaration has an **isolation** (actor-isolated, global-actor-isolated, nonisolated)
- Each expression has a **required isolation** based on what it accesses
- If the expression's required isolation ≠ current context's isolation → insert `await` (async hop) or reject (if non-async context)

### Actor Reentrancy — NON-REENTRANT BY DEFAULT

**Cot diverges from Swift here.** Swift actors are reentrant by default (SE-0306) — when an actor method hits `await`, other messages can execute. This is the #1 developer complaint in Swift concurrency (see Appendix D.4).

Cot actors are **non-reentrant by default**. When an actor method hits `await`, the actor does NOT process other messages — it suspends until the await completes, then resumes. This prevents state invalidation across suspension points.

```cot
actor ImageCache {
    var cache: Map(string, Image) = .{}

    fn getImage(url: string) async Image {
        if (cache.has(url)) { return cache.get(url) }
        let image = await downloadImage(url)
        // SAFE: no other work ran on this actor while we awaited
        cache.set(url, image)
        return image
    }
}
```

Opt into reentrancy with `@reentrant` when deliberate interleaving is needed:

```cot
actor WorkQueue {
    @reentrant
    fn process(item: Item) async Result {
        // Other messages CAN execute during this await
        return await heavyComputation(item)
    }
}
```

The deadlock risk from non-reentrancy is manageable: actor A awaiting actor B while B awaits A deadlocks. This is detectable at runtime (cycle in the wait graph) and can be reported with a clear error. Reentrancy bugs (stale state after await) are silent and insidious.

### Runtime Functions for Actors

| Function | Purpose | Swift Reference |
|----------|---------|-----------------|
| `actor_initialize(actor_ptr)` | Init actor executor state | `swift_defaultActor_initialize` |
| `actor_destroy(actor_ptr)` | Destroy actor executor | `swift_defaultActor_destroy` |
| `actor_enqueue(actor_ptr, job_ptr)` | Enqueue job on actor | `swift_defaultActor_enqueue` |
| `actor_resign(actor_ptr)` | Actor done processing, check for more work | internal |

---

## 6. Phase 4: Sendable

### Swift Reference
- `references/swift/lib/Sema/TypeCheckConcurrency.cpp` — Sendable conformance checking

### What is Sendable?

`Sendable` is a marker trait that indicates a type is safe to transfer across concurrency domains (actor boundaries, task boundaries). The compiler checks Sendable conformance at compile time — no runtime cost.

```cot
// Implicitly Sendable:
struct Point { x: int, y: int }              // all fields are Sendable value types
enum Color { red, green, blue }              // simple enum, no associated data
// Actors are implicitly Sendable (they ARE the isolation boundary)

// NOT Sendable (compiler error if sent across actors):
struct UnsafeState { ptr: RawPtr }           // raw pointers are not Sendable
// Mutable reference types are not Sendable (unless they are actors)
```

### Sendable Rules

1. **Value types** (structs, enums) are Sendable if ALL their stored properties/associated values are Sendable
2. **Actors** are always Sendable
3. **Immutable reference types** (classes with only `let` properties of Sendable types) are Sendable
4. **Functions/closures** are Sendable if they capture only Sendable values (`@Sendable fn`)
5. **Generic types** are Sendable if their type parameters are constrained to Sendable
6. **Built-in types**: `int`, `i32`, `i64`, `f64`, `bool`, `string` (if COW/immutable) are Sendable
7. **Collections**: `List(T)` is Sendable if `T` is Sendable (COW semantics make this safe)
8. **Optional**: `?T` is Sendable if `T` is Sendable
9. **Error unions**: `E!T` is Sendable if both `E` and `T` are Sendable

### Compiler Implementation

In `checker.zig`, add `isSendable(type: TypeIndex) bool`:

```
fn isSendable(ty: TypeIndex) bool {
    switch (typeKind(ty)) {
        .int, .i32, .i64, .f64, .f32, .bool => return true,      // primitives
        .string => return true,                                     // COW string
        .struct_type => return allFieldsSendable(ty),
        .enum_type => return allVariantsSendable(ty),
        .actor_type => return true,                                 // actors always Sendable
        .optional => return isSendable(innerType(ty)),
        .error_union => return isSendable(errorType(ty)) and isSendable(payloadType(ty)),
        .list => return isSendable(elementType(ty)),
        .map => return isSendable(keyType(ty)) and isSendable(valueType(ty)),
        .fn_type => return closure.captures.all(isSendable),       // @Sendable closure check
        .pointer => return false,                                   // raw pointers not Sendable
        .existential => return false,                               // unless constrained
        else => return false,
    }
}
```

### Sendable Checking Points

The compiler inserts Sendable checks at:

1. **Task creation**: `Task { ... }` — captured values must be Sendable
2. **Cross-actor calls**: arguments passed to actor methods must be Sendable
3. **Cross-actor returns**: values returned from actor methods must be Sendable
4. **AsyncStream.yield**: values sent through streams must be Sendable
5. **`sending` parameter annotation**: function parameters marked `sending` must receive Sendable values

---

## 7. Phase 5: Structured Concurrency (TaskGroup)

### Swift Reference
- `references/swift/stdlib/public/Concurrency/TaskGroup.swift` — TaskGroup API and implementation

### TaskGroup

A `TaskGroup` creates a scope for launching dynamic numbers of child tasks. The group **always waits for all children to complete** before returning — no orphaned tasks.

```cot
// Cot syntax — matches Swift semantics
let results = await withTaskGroup(of: int) { group in
    for i in 0..10 {
        group.addTask {
            return await computeSlice(i)
        }
    }

    var sum = 0
    for await result in group {
        sum += result
    }
    return sum
}
```

### TaskGroup Rules

1. **Child tasks cannot outlive the group.** `withTaskGroup` blocks until ALL children finish, even cancelled ones.
2. **Cancelling the group** cancels all children. Children cooperatively check `Task.isCancelled`.
3. **Error propagation (nuanced)**:
   - An individual child error returned via `group.next()` does NOT auto-cancel siblings — the caller decides how to handle it
   - But throwing OUT of the `withThrowingTaskGroup` body DOES cancel all children, wait for them all to complete, then rethrow
   - This distinction matters: `next()` gives control, exiting the scope gives safety
4. **Results are collected via `for await in`** or manual `group.next()` calls.

### async let (Structured Child Task)

```cot
async fn loadDashboard() Dashboard {
    async let profile = fetchProfile()
    async let posts = fetchPosts()
    async let notifications = fetchNotifications()

    // All three run concurrently; we await them together
    return Dashboard(
        profile: await profile,
        posts: await posts,
        notifications: await notifications,
    )
}
```

`async let` creates an implicit child task. The value is awaited when first used. If the enclosing scope exits before the value is awaited, the child task is cancelled.

Swift reference: `AsyncLet.swift` and `AsyncLet.cpp` in stdlib/public/Concurrency/.

### Runtime Functions

| Function | Purpose | Swift Reference |
|----------|---------|-----------------|
| `taskGroup_create() → GroupPtr` | Allocate task group | `Builtin.createTaskGroup` |
| `taskGroup_destroy(group_ptr)` | Deallocate task group | `Builtin.destroyTaskGroup` |
| `taskGroup_addTask(group_ptr, fn_ptr, context)` | Launch child task | `TaskGroup.addTask` |
| `taskGroup_next(group_ptr) → ?Result` | Await next completed child | `TaskGroup.next()` |
| `taskGroup_cancelAll(group_ptr)` | Cancel all children | `TaskGroup.cancelAll()` |
| `asyncLet_start(fn_ptr, context) → ChildTaskPtr` | Start async let child | `Builtin.startAsyncLet` |
| `asyncLet_get(child_ptr) → result` | Await async let result | `Builtin.endAsyncLet` |

---

## 8. Phase 6: AsyncSequence & AsyncStream

### Swift Reference
- `references/swift/stdlib/public/Concurrency/AsyncSequence.swift` — Protocol definition
- `references/swift/stdlib/public/Concurrency/AsyncStream.swift` — Stream implementation

### AsyncSequence (trait)

```cot
trait AsyncSequence(Element) {
    type Iterator: AsyncIterator(Element)
    fn makeAsyncIterator() Iterator
}

trait AsyncIterator(Element) {
    async fn next() ?Element  // null = sequence finished
}
```

Usage with `for await`:

```cot
for await line in fileStream.lines() {
    print(line)
}

// Desugars to:
var iter = fileStream.lines().makeAsyncIterator()
while let line = await iter.next() {
    print(line)
}
```

### Channel (symmetric, with backpressure)

`Channel(T)` is the primary concurrency communication primitive. Both endpoints can send and recv. Send suspends when the buffer is full. Recv suspends when the buffer is empty. ARC manages lifetime — both endpoints retain the channel, last to drop deallocates.

This is Go's channel design with Cot's ARC lifetime management. Swift's `AsyncStream` lacks backpressure (see Appendix D.5), which makes it unsuitable as a general-purpose channel.

```cot
let ch = Channel(int, capacity: 10)

// Producer
Task {
    for i in 0..100 {
        await ch.send(i)    // suspends if buffer full — backpressure
    }
    ch.close()
}

// Consumer
for await val in ch {
    print(val)
}
```

Channel conforms to `AsyncSequence` — usable with `for await in` and all async sequence operators.

```cot
struct Channel(T) {
    // Creation
    static fn init(capacity: int) Channel(T)   // buffered
    static fn init() Channel(T)                 // unbuffered (rendezvous)

    // Async operations
    async fn send(value: T)             // suspends if full, error if closed
    async fn recv() ?T                  // suspends if empty, null if closed+drained

    // Control
    fn close()                          // signal no more sends; recv drains remaining
    fn isClosed() bool
}
```

### AsyncStream (callback bridge, no backpressure)

`AsyncStream` bridges synchronous callback-based APIs into async/await. The producer side (`Continuation`) is synchronous — it can be called from delegate methods, NotificationCenter callbacks, etc. without requiring an async context.

**Use AsyncStream when:** the producer is a sync callback you don't control.
**Use Channel when:** both producer and consumer are async code you control.

```cot
// Wrapping a callback-based API:
let events = AsyncStream(of: Event) { continuation in
    notificationCenter.observe("didUpdate") { event in
        continuation.yield(event)    // sync call — no await needed
    }
    continuation.onTermination = { _ in
        notificationCenter.removeObserver("didUpdate")
    }
}

for await event in events {
    await handleEvent(event)
}
```

```cot
struct AsyncStream(Element) {
    struct Continuation {
        fn yield(value: Element) YieldResult  // sync — never suspends
        fn finish()                            // signal end of stream

        enum BufferingPolicy {
            unbounded,                         // buffer everything (default)
            bufferingOldest(int),             // keep oldest N, drop new
            bufferingNewest(int),             // keep newest N, drop old
        }
    }
}
```

**AsyncStream has NO backpressure.** When the buffer fills, elements are silently dropped per the buffering policy. This is acceptable for the callback-bridging use case (you can't suspend a sync callback) but wrong for general producer/consumer patterns. Use `Channel` for those.

### Async Sequence Operators

These are standard library functions, implemented as concrete types wrapping an upstream AsyncSequence:

| Operator | Description | Returns |
|----------|-------------|---------|
| `.map(fn)` | Transform elements | `AsyncMapSequence` |
| `.filter(fn)` | Filter elements | `AsyncFilterSequence` |
| `.compactMap(fn)` | Map + filter nil | `AsyncCompactMapSequence` |
| `.flatMap(fn)` | Map to sequences, flatten | `AsyncFlatMapSequence` |
| `.prefix(n)` | Take first N | `AsyncPrefixSequence` |
| `.drop(n)` | Skip first N | `AsyncDropFirstSequence` |
| `.reduce(init, fn)` | Reduce to single value | `async fn → T` |
| `.contains(value)` | Check membership | `async fn → bool` |
| `.first(where: fn)` | Find first match | `async fn → ?T` |

---

## 9. Phase 7: Global Actors & @MainActor

### Swift Reference
- `references/swift/stdlib/public/Concurrency/GlobalActor.swift` — GlobalActor protocol
- `references/swift/stdlib/public/Concurrency/MainActor.swift` — MainActor implementation

### Global Actors

A **global actor** is a singleton actor that provides a shared isolation domain. Any declaration annotated with the global actor runs on that actor's executor.

```cot
@globalActor
actor DatabaseActor {
    static let shared = DatabaseActor()
}

@DatabaseActor
fn saveToDB(data: string) {
    // This function is guaranteed to run on DatabaseActor's executor
    // No concurrent access to DB — safe without locks
    db.write(data)
}

// From outside:
await saveToDB("hello")  // hops to DatabaseActor
```

### @MainActor

The most important global actor. Guarantees code runs on the main thread (critical for UI).

```cot
@MainActor
fn updateUI(text: string) {
    label.setText(text)  // safe: runs on main thread
}

// From a background task:
Task {
    let data = await fetchData()     // runs on global executor
    await updateUI(data.toString())  // hops to main thread
}
```

Swift reference: `MainActor.swift` — `@globalActor public final actor MainActor: GlobalActor`.

### Implementation

Global actors are implemented as:
1. A singleton actor instance (`shared` property)
2. A serial executor (same as regular actors)
3. Compiler-inserted executor hops at call sites

The `@MainActor` executor is special — on native platforms, it's the main thread's run loop. On Wasm, it's the browser's event loop (requestAnimationFrame / microtask queue).

---

## 10. Phase 8: Task Cancellation & Task Locals

### Task Cancellation

Swift reference: `TaskCancellation.swift`.

Cancellation is **cooperative** — setting the cancellation flag doesn't stop the task. The task must check for cancellation and respond appropriately.

```cot
// Check cancellation
if Task.isCancelled {
    return partial_result  // or throw CancellationError
}

// Throw on cancellation
try Task.checkCancellation()  // throws CancellationError if cancelled

// Cancellation handler
let result = await withTaskCancellationHandler(
    operation: {
        return await longRunningWork()
    },
    onCancel: {
        // Called immediately when task is cancelled
        // May run concurrently with operation
        workHandle.cancel()
    }
)
```

### Task Locals

Swift reference: `TaskLocal.swift`.

Task-local values are implicitly inherited by child tasks. They provide a way to pass context (trace IDs, auth tokens) down the task tree without parameter threading.

```cot
@TaskLocal
static var traceID: ?string = null

// Bind a value for a scope
await $traceID.withValue("req-123") {
    await handleRequest()  // traceID is "req-123" here
    // Any child tasks also see traceID = "req-123"
}
```

### Runtime Functions

| Function | Purpose |
|----------|---------|
| `task_cancel(task_ptr)` | Set cancellation flag |
| `task_isCancelled(task_ptr) → bool` | Check cancellation flag |
| `task_addCancellationHandler(handler_fn) → record` | Register cancellation callback |
| `task_removeCancellationHandler(record)` | Deregister cancellation callback |
| `task_localGet(key_ptr) → value_ptr` | Read task-local value |
| `task_localBind(key_ptr, value_ptr)` | Push task-local binding |
| `task_localUnbind(key_ptr)` | Pop task-local binding |

---

## 11. Phase 9: Wasm Target

### Single-Threaded Executor

Wasm has no threads (SharedArrayBuffer exists but is limited). The Wasm target uses a **single-threaded cooperative executor** — an event loop that polls tasks.

```
Wasm Executor Loop:
  1. Pop next ready task from queue
  2. Run task until it suspends (await) or completes
  3. If task suspended: re-enqueue when dependency resolves
  4. If task completed: wake parent/continuation
  5. If queue empty: return to host (JS/wasmtime)
  6. Host calls back into executor when I/O completes
```

### Actor Isolation on Wasm

Actors on Wasm are **compile-time only**. Since everything runs on one thread, there's no actual concurrency — but the isolation checking still prevents data races in the source code. This means:
- Code that compiles for Wasm will also be safe on native (where there IS real concurrency)
- Cross-actor calls still require `await` (the compiler enforces it)
- The `await` on Wasm yields to the event loop (not truly concurrent, but allows interleaving)

### State Machine Transformation

Async functions on Wasm are transformed to state machines (same as current approach, refined):

```cot
async fn fetchTwo() (string, string) {
    let a = await fetch("one")     // suspension point 0
    let b = await fetch("two")     // suspension point 1
    return (a, b)
}

// Compiles to:
struct fetchTwo_State {
    pc: int,         // program counter (0, 1, 2)
    a: string,       // live across suspension
    result: (string, string),
}

fn fetchTwo_poll(state: *fetchTwo_State) PollResult {
    switch state.pc {
        0 => {
            // Start fetch("one"), register callback
            state.pc = 1
            return .pending
        },
        1 => {
            state.a = getResult()  // fetch("one") completed
            // Start fetch("two"), register callback
            state.pc = 2
            return .pending
        },
        2 => {
            let b = getResult()    // fetch("two") completed
            state.result = (state.a, b)
            return .ready
        },
    }
}
```

### Browser Integration

For `--target=js`, the executor integrates with the browser event loop:

```javascript
// Generated JS glue
async function runCotExecutor(instance) {
    while (instance.exports._executor_hasWork()) {
        instance.exports._executor_runOne();
        await new Promise(resolve => setTimeout(resolve, 0)); // yield to browser
    }
}
```

`Task.sleep` on browser → `setTimeout`. Async I/O → `fetch()` wrapped as task wakeup.

---

## 12. Phase 10: Continuations (Sync-to-Async Bridge)

### Swift Reference
- `references/swift/stdlib/public/Concurrency/CheckedContinuation.swift`

### Continuations

Continuations bridge callback-based APIs into async/await. This is critical for integrating with platform APIs.

```cot
// Wrap a callback-based API:
async fn readFile(path: string) !string {
    return await withCheckedContinuation { continuation in
        fs.readFile(path) { result, error in
            if error != null {
                continuation.resume(throwing: error)
            } else {
                continuation.resume(returning: result)
            }
        }
    }
}
```

### Checked vs Unsafe Continuations

- **`withCheckedContinuation`**: Runtime checks that `resume` is called exactly once. Traps on double-resume or leak.
- **`withUnsafeContinuation`**: No runtime checks. For performance-critical code.

Swift's `CheckedContinuationCanary` (lines 22-88 of CheckedContinuation.swift) uses atomic exchange to detect double-resume and deinit logging to detect leaked continuations. We port this pattern exactly.

---

## 13. Cot Syntax Mapping

### Complete Syntax Reference

```cot
// ═══════════════════════════════════════════
// ASYNC FUNCTIONS
// ═══════════════════════════════════════════
async fn fetch(url: string) !string {
    let response = await httpGet(url)
    return response.body
}

// ═══════════════════════════════════════════
// TASKS (unstructured concurrency)
// ═══════════════════════════════════════════
let task = Task {
    return await fetch("https://api.example.com")
}
let result = await task.value

let detached = Task.detached(priority: .low) {
    return await heavyComputation()
}

// ═══════════════════════════════════════════
// ACTORS
// ═══════════════════════════════════════════
actor ChatRoom {
    var messages: List(string) = []
    var users: List(User) = []

    fn post(message: string, from: User) {
        messages.append("[${from.name}]: ${message}")
        // notifyAll is isolated — safe to access users
        for user in users {
            user.notify(message)
        }
    }

    fn join(user: User) {
        users.append(user)
    }

    nonisolated fn roomName() string {
        return "General"  // no mutable state access — no await needed
    }
}

let room = ChatRoom()
await room.post("hello", from: currentUser)
let name = room.roomName()  // no await needed — nonisolated

// ═══════════════════════════════════════════
// SENDABLE
// ═══════════════════════════════════════════
struct Message: Sendable {
    let text: string
    let timestamp: int
}

// Compiler error — RawPtr is not Sendable:
// struct BadMessage: Sendable {
//     let ptr: RawPtr   // ERROR: stored property 'ptr' of Sendable type has non-Sendable type 'RawPtr'
// }

// ═══════════════════════════════════════════
// TASK GROUPS (structured concurrency)
// ═══════════════════════════════════════════
let images = await withTaskGroup(of: Image) { group in
    for url in imageURLs {
        group.addTask {
            return await downloadImage(url)
        }
    }

    var results: List(Image) = []
    for await image in group {
        results.append(image)
    }
    return results
}

// ═══════════════════════════════════════════
// ASYNC LET (structured concurrency, shorthand)
// ═══════════════════════════════════════════
async fn loadPage() Page {
    async let header = fetchHeader()
    async let body = fetchBody()
    async let footer = fetchFooter()
    return Page(
        header: await header,
        body: await body,
        footer: await footer,
    )
}

// ═══════════════════════════════════════════
// ASYNC SEQUENCES & STREAMS
// ═══════════════════════════════════════════
// Channel with backpressure (primary concurrency primitive):
let numbers = Channel(int, capacity: 100)
Task {
    for i in 0..100 { await numbers.send(i) }
    numbers.close()
}
for await n in numbers { print(n) }

// AsyncStream for callback bridging (no backpressure):
let events = AsyncStream(of: int) { cont in
    for i in 0..100 {
        cont.yield(i)
    }
    cont.finish()
}

for await n in numbers {
    print(n)
}

// Async sequence operators:
for await even in numbers.filter({ n in n % 2 == 0 }).prefix(10) {
    print(even)
}

// ═══════════════════════════════════════════
// GLOBAL ACTORS
// ═══════════════════════════════════════════
@globalActor
actor UIActor {
    static let shared = UIActor()
}

@UIActor
fn updateLabel(text: string) {
    label.text = text  // guaranteed to run on UIActor
}

@MainActor
fn onButtonTap() {
    Task {
        let data = await fetchData()
        await updateLabel(data.summary)  // hops to UIActor
    }
}

// ═══════════════════════════════════════════
// CANCELLATION
// ═══════════════════════════════════════════
async fn processItems(items: List(Item)) !List(Result) {
    var results: List(Result) = []
    for item in items {
        try Task.checkCancellation()  // throws CancellationError
        results.append(await process(item))
    }
    return results
}

let task = Task { try await processItems(items) }
// Later:
task.cancel()  // sets flag, processItems will throw at next checkCancellation

// ═══════════════════════════════════════════
// TASK LOCALS
// ═══════════════════════════════════════════
@TaskLocal
static var requestID: ?string = null

await $requestID.withValue("req-abc-123") {
    await handleRequest()  // requestID visible to all child tasks
}

// ═══════════════════════════════════════════
// CONTINUATIONS (callback bridge)
// ═══════════════════════════════════════════
async fn connectWebSocket(url: string) !WebSocket {
    return try await withCheckedThrowingContinuation { cont in
        ws.connect(url, onSuccess: { socket in
            cont.resume(returning: socket)
        }, onError: { err in
            cont.resume(throwing: err)
        })
    }
}
```

---

## 14. ARC Integration

### How Swift's Concurrency Interacts with ARC

This is the key reason to use Swift's concurrency model — it was **designed** to work with ARC.

#### Task Lifetime

Tasks are ARC-managed objects. `Task {}` returns a `Task<Success>` struct containing a `Builtin.NativeObject` — a refcounted pointer. The task keeps itself alive while running (it holds a reference to itself). When the task completes and no external references remain, ARC deallocates it.

Swift reference: `Task.swift` line 145: `internal let _task: Builtin.NativeObject`.

#### Actor Lifetime

Actors are ARC-managed heap objects (they conform to `AnyObject`). When the last reference to an actor drops, its `deinit` runs — ON the actor's executor (to guarantee isolation during cleanup).

Swift reference: `MainActor.swift` lines 174-195 — `_deinitOnExecutorMainActorBackDeploy` ensures deinit runs on the correct executor.

#### Sendable and ARC

The Sendable check prevents ARC reference count races:
- Sending a non-Sendable reference type across actors would create shared mutable state → data race on the refcount
- Sendable checking at compile time prevents this entirely
- Value types (structs) are always safe to send because they're copied (COW for collections)

#### VWT Integration

Task-local values and actor private data use VWT for type-erased storage:
- Task locals store values as opaque bytes with VWT for copy/destroy
- TaskGroup result collection uses VWT to copy results from child task storage to parent
- AsyncStream buffers use VWT to manage element lifecycle

### What This Means for Cot

Since Cot already has:
- ARC (retain/release, unified cleanup stack)
- VWT (value witness tables for copy/destroy/assign)
- TypeMetadata (vwt_ptr, size, stride, type_idx)

The concurrency runtime plugs in naturally:
- Tasks are ARC objects → `retain(task)` / `release(task)` just works
- Actors are ARC objects → same
- Sendable checking prevents ARC races → no new runtime work needed
- VWT handles type-erased storage in task groups / streams → already done

---

## 15. Implementation Order & Dependencies

### Dependency Graph

```
Phase 0: Delete Go concurrency (clean slate)
    │
Phase 1: Task Runtime ──────────────────────┐
    │                                        │
Phase 2: async/await Rewrite ───────────────┤
    │                                        │
Phase 4: Sendable (BEFORE actors)           │
    │                                        │
Phase 3: Actors ────────────────────────────┤
    │                           │            │
Phase 5: TaskGroup ─────────────┤            │
    │                           │            │
Phase 6: Channel + AsyncSequence┤            │
    │                           │            │
Phase 7: Global Actors          │            │
    │                           │            │
Phase 8: Cancellation + Locals  │            │
    │                           │            │
Phase 9: Wasm Target ───────────┘            │
    │                                        │
Phase 10: Continuations ─────────────────────┘
```

**Key ordering change:** Sendable (Phase 4) comes BEFORE Actors (Phase 3). Without Sendable checking, actors compile but aren't safe — non-Sendable types can cross isolation boundaries unchecked.

### Recommended Order

| Phase | Effort | Depends On | Deliverable |
|-------|--------|------------|-------------|
| **0. Delete Go concurrency** | 1 day | None | ~5,000 lines removed, clean foundation |
| **1. Task Runtime** | 2-3 days | ARC (done) | Task create/destroy/enqueue, cooperative executor |
| **2. async/await** | 3-4 days | Phase 1 | Wasm state machine + native continuation passing |
| **4. Sendable** | 2-3 days | None (checker pass) | Sendable trait, compile-time checking at boundaries |
| **3. Actors** | 4-5 days | Phase 2, 4 | `actor` keyword, serial executor, isolation checking |
| **5. TaskGroup** | 3-4 days | Phase 2, 4 | `withTaskGroup`, `async let`, child task management |
| **6. Channel + AsyncSequence** | 2-3 days | Phase 4, 5 | Channel(T) with backpressure, AsyncStream for bridging, `for await in` |
| **7. Global Actors** | 1-2 days | Phase 3 | `@globalActor`, `@MainActor` |
| **8. Cancellation** | 1-2 days | Phase 1 | Cancellation API, handlers, Task.checkCancellation |
| **9. Wasm Target** | 2-3 days | Phase 2 | Single-threaded event loop executor, browser integration |
| **10. Continuations** | 1-2 days | Phase 2 | withCheckedContinuation, callback bridge |

**Total estimated effort: ~5-7 weeks** (see Appendix D.8 for rationale)

### Deletion Order

1. **First**: Delete `stdlib/channel.cot`, `stdlib/thread.cot` (except Atomic), `stdlib/async.cot`
2. **Second**: Delete `scheduler_native.zig`, `thread_native.zig`
3. **Third**: Delete spawn/select lowering in `lower.zig`
4. **Fourth**: Remove `kw_spawn`, `kw_select`, `SpawnExpr`, `SelectExpr` from parser/AST
5. **Fifth**: Delete old test files, write new ones per phase

---

## 16. Reference File Map

### Swift Source → Cot Implementation

| Swift Source | Purpose | Cot Target |
|-------------|---------|------------|
| `include/swift/ABI/Task.h` | Task/Job struct layout | `compiler/frontend/types.zig` (TaskObject type) |
| `include/swift/ABI/Actor.h` | Actor struct layout | `compiler/frontend/types.zig` (ActorObject type) |
| `include/swift/ABI/Executor.h` | Executor ref layout | `compiler/frontend/types.zig` (SerialExecutorRef) |
| `stdlib/public/Concurrency/Task.swift` | Task API | `stdlib/task.cot` |
| `stdlib/public/Concurrency/Actor.swift` | Actor protocol | `compiler/frontend/checker.zig` (Actor trait) |
| `stdlib/public/Concurrency/Executor.swift` | Executor protocol | `stdlib/executor.cot` |
| `stdlib/public/Concurrency/GlobalActor.swift` | GlobalActor protocol | `compiler/frontend/checker.zig` |
| `stdlib/public/Concurrency/MainActor.swift` | MainActor singleton | `stdlib/main_actor.cot` |
| `stdlib/public/Concurrency/TaskGroup.swift` | TaskGroup API | `stdlib/task_group.cot` |
| `stdlib/public/Concurrency/AsyncSequence.swift` | AsyncSequence protocol | `stdlib/async_sequence.cot` |
| `stdlib/public/Concurrency/AsyncStream.swift` | AsyncStream (callback bridge) | `stdlib/async_stream.cot` |
| `stdlib/public/Concurrency/TaskCancellation.swift` | Cancellation API | `stdlib/task.cot` |
| `stdlib/public/Concurrency/TaskLocal.swift` | Task-local values | `stdlib/task_local.cot` |
| `stdlib/public/Concurrency/CheckedContinuation.swift` | Continuation bridge | `stdlib/continuation.cot` |
| `stdlib/public/Concurrency/CooperativeExecutor.swift` | Cooperative executor | `compiler/codegen/native/executor_native.zig` |
| `lib/Sema/TypeCheckConcurrency.cpp` | Sendable/isolation checking | `compiler/frontend/checker.zig` |
| `lib/IRGen/GenConcurrency.cpp` | Executor type info, task creation | `compiler/frontend/lower.zig` |
| `lib/IRGen/GenCoro.cpp` | Coroutine frame layout, splitting | `compiler/frontend/lower.zig` |
| `lib/SILGen/SILGenConcurrency.cpp` | SIL for async/actor | `compiler/frontend/lower.zig` |

---

## Appendix A: Swift Concurrency Lessons Learned

These are known pain points from Swift's concurrency rollout (2021-2026) that Cot should learn from:

### 1. Strict Sendable checking was too disruptive at launch
Swift shipped Sendable as a warning first, then made it an error in Swift 6. Cot should ship with Sendable as errors from day one (we have no legacy code to break).

### 2. Actor reentrancy confuses developers
Developers assume actor methods are atomic. They're not — state can change across `await` points. **Cot makes actors non-reentrant by default** with `@reentrant` opt-in (see Section 5 and Appendix D.4).

### 3. MainActor inference was too aggressive
Swift 6 made UI-related code implicitly `@MainActor`, breaking tons of existing code. Cot should require explicit annotation.

### 4. Global actors are overused
Developers annotate everything with `@MainActor` instead of properly isolating data. This serialises work unnecessarily. Cot should document best practices early.

### 5. Continuations are easy to misuse
Forgetting to call `resume` leaks the continuation and deadlocks the caller. `CheckedContinuation` catches this at runtime but not at compile time. Cot should make `CheckedContinuation` the only option (no unsafe variant).

### 6. AsyncSequence operator types are unwieldy
`AsyncMapSequence<AsyncFilterSequence<AsyncStream<Int>>>` is painful. Cot should use existential types (`any AsyncSequence(int)`) to erase these.

---

## Appendix B: Comparison with Go (What We're Replacing)

| Aspect | Go (current Cot) | Swift (new Cot) |
|--------|-------------------|-----------------|
| Concurrency primitive | goroutine (lightweight thread) | Task (structured unit of work) |
| Communication | channels (CSP model) | Channel (with backpressure), AsyncStream (bridging), actor methods |
| Synchronisation | mutex, waitgroup | Actor isolation (compile-time) |
| Data race prevention | Race detector (runtime) | Sendable checking (compile-time) |
| Function coloring | No (all functions can suspend) | Yes (async functions explicit) |
| Cancellation | context.Context (manual threading) | Task.isCancelled (automatic propagation) |
| Memory management | GC | ARC (already ported) |
| Scheduling | M:N green threads | Cooperative thread pool |
| Error handling | Multiple returns (value, error) | Error unions (already in Cot) |

The fundamental improvement: **Go detects data races at runtime (and only in test mode). Swift prevents them at compile time.** This matches Cot's philosophy of catching errors early.

---

## Appendix C: Deep Audit Corrections (2026-03-26)

**This appendix contains corrections and deep implementation details gathered from reading the actual Swift source code in `references/swift/` and web research. Future Claude sessions MUST read this appendix — it corrects oversimplifications in the main document.**

---

### C.1: Task Object — Corrected Layout

The main document's TaskObject layout is oversimplified. The real layout is a C++ inheritance chain: `HeapObject → Job → AsyncTask`, plus optional trailing fragments, all in a **single contiguous allocation**.

#### Job Layout (64 bytes on 64-bit, `static_assert` confirmed)

```
Offset  0: HeapObject.metadata         8B   isa pointer (TypeMetadata*)
Offset  8: HeapObject.refCounts        8B   atomic refcount
Offset 16: SchedulerPrivate[0]         8B   next-waiting-task linked list pointer
Offset 24: SchedulerPrivate[1]         8B   dispatch linkage
Offset 32: JobFlags Flags              4B   packed uint32 (see bit layout below)
Offset 36: uint32_t Id                 4B   LOWER 32 bits of task ID
Offset 40: voucher_t Voucher           8B   Darwin-only, NULL on other platforms
Offset 48: void* Reserved              8B   future use
Offset 56: ResumeTask fn ptr           8B   TaskContinuationFunction*
```

Alignment: `alignas(2 * alignof(void*))` = 16 bytes.

#### AsyncTask extends Job

```
Offset 64: AsyncContext* ResumeContext  8B   current continuation context (signed ptr)
Offset 72: void* Reserved64            8B   padding (64-bit only)
Offset 80: PrivateStorage              ~88+ bytes (see below)
```

#### PrivateStorage (inside AsyncTask, starts at offset 80)

```
Offset  0: ExclusivityAccessSet[2]    16B   exclusivity runtime state
Offset 16: ActiveTaskStatus           16B   atomic state (flags + status record head)
Offset 32: TaskAllocator              20B   slab allocator state (2 ptrs + 2 u32)
Offset 52: [4B padding]
Offset 56: TaskLocal::Storage          8B   task-local value chain head
Offset 64: uint32_t Id                 4B   UPPER 32 bits of task ID
Offset 68: [4B padding]
Offset 72: JobPriority BasePriority    8B   original priority (before escalation)
Offset 80: TaskDependencyStatusRecord* 8B   what this task is blocked on
Offset 88: RecursiveMutex statusLock   var  platform-dependent mutex
```

**CRITICAL: Task ID is split** — lower 32 bits in `Job::Id` (offset 36), upper 32 bits in `PrivateStorage::Id` (offset 80+64). Combined for the full 64-bit task ID.

#### Trailing Fragments (appended after sizeof(AsyncTask), in strict order)

```
[AsyncTask base ~168+ bytes]
[ChildFragment?]         — if isChildTask: 2 ptrs (Parent + NextChild) = 16B
[GroupChildFragment?]    — if isGroupChildTask: 1 ptr (Group) = 8B
[FutureFragment?]        — if isFuture: waitQueue(8) + resultType(8) + error(8) + padding + result storage
[Context Prefix]         — FutureAsyncContextPrefix(32B) or AsyncContextPrefix(24B)
[Initial AsyncContext]   — Parent(8) + ResumeParent(8) + spilled locals
[Initial Slab Space]     — 512 bytes for the task's slab allocator
```

**Everything is ONE malloc.** The `amountToAllocate = headerSize + initialContextSize + initialSlabSize`. This is critical for performance — no per-frame mallocs.

#### JobFlags Bit Layout (uint32_t)

```
Bits [7:0]    Kind (8 bits)          JobKind enum (Task=0, NullaryContinuation, etc.)
Bits [15:8]   Priority (8 bits)      JobPriority (0x09=background, 0x11=utility, 0x15=default, 0x19=userInitiated)
Bits [23:16]  Reserved (8 bits)      generic job flags
Bit  24       Task_IsChildTask
Bit  25       Task_IsFuture
Bit  26       Task_IsGroupChildTask
Bit  28       Task_IsAsyncLetTask
Bit  29       Task_HasInitialTaskExecutorPreference
Bit  30       Task_HasInitialTaskName
```

#### ActiveTaskStatus Flags (uint32_t, inside the 16-byte atomic)

```
Bits [7:0]    PriorityMask           current max priority (may be escalated above base)
Bit  8        IsCancelled
Bit  9        IsStatusRecordLocked   status record chain lock held
Bit  10       IsEscalated            priority was boosted
Bit  11       IsRunning              (non-escalation platforms only)
Bit  12       IsEnqueued             task is in an executor queue
Bit  13       IsComplete
Bit  14       HasTaskDependency
Bit  15       HasTaskExecutorPreference
```

#### Task Slab Allocator

- **Slab capacity**: ~984 bytes (1024 - header overhead)
- **Initial slab**: 512 bytes, inline with the task allocation
- **Allocation**: Bump pointer, 16-byte aligned
- **Deallocation**: LIFO only (stack discipline). `swift_task_dealloc` moves bump pointer back
- **Can be disabled**: `SWIFT_CONCURRENCY_ENABLE_TASK_SLAB_ALLOCATOR=0` falls back to malloc/free
- **async let tasks**: Allocated from parent's slab allocator, with "immortal" refcounts (no ARC)

#### Future Wait Queue (lock-free)

`FutureFragment::waitQueue` is `atomic<WaitQueueItem>` using **pointer tagging**:

```
Bits [1:0]  Status (0=Executing, 1=Success, 2=Error)
Bits [63:2] AsyncTask* (head of waiting task linked list, via SchedulerPrivate[0])
```

**Waiting**: CAS the current task into the wait queue head. If status is already Success/Error, return immediately.

**Completion**: Atomic exchange to Success/Error with null pointer. Walk the old linked list and wake each waiter via `vw_initializeWithCopy` (copies result using VWT) + enqueue on executor.

---

### C.2: Actor — Corrected Implementation

#### DefaultActor Size

`NumWords_DefaultActor = 12`. Total size: `sizeof(HeapObject) + 12 * sizeof(void*) = 16 + 96 = 112 bytes` on 64-bit.

The 12 words of PrivateData are overlaid by `DefaultActorImpl`:

**Header (front of PrivateData):**
- `bool isDistributedRemoteActor` (1B + padding)
- `ActiveActorStatus` (16B, 16-byte aligned) — atomic state word

**Footer (end of PrivateData):**
- `PriorityQueue<Job*>` — multi-bucket priority queue for pending jobs

#### Two-Level Job Queue (NOT a simple MPSC)

**Level 1 — Lock-free atomic LIFO linked list (ingress):**
- Producers atomically prepend jobs using CAS on `ActiveActorStatus.FirstJob`
- Uses `SchedulerPrivate[0]` as the intrusive next pointer
- Jobs arrive in LIFO order (most recent first)

**Level 2 — Priority-bucketed FIFO queue (drain-side only):**
- Drain thread calls `processIncomingQueue()` which atomically claims ALL jobs from Level 1 (CAS to NULL)
- Reverses them to FIFO order
- Inserts into `PriorityQueue` by priority bucket
- `drainOne()` dequeues the highest-priority job
- O(1) enqueue and dequeue (bucketed linked list, not a heap)

#### Actor State Machine

```
States: Idle (0x0), Scheduled (0x1), Running (0x2), Zombie (0x3)

Transitions:
  Idle → Running       tryLock fast path (CAS, no global pool involvement)
  Idle → Scheduled     first job enqueued → creates ProcessOutOfLineJob on global pool
  Scheduled → Running  drain thread acquires via CAS
  Running → Idle       unlock, no more jobs
  Running → Scheduled  unlock, jobs remain → reschedule ProcessOutOfLineJob
  Idle → Deallocated   last ARC release while idle → immediate dealloc
  Running → Zombie     last ARC release while running → deferred dealloc
  Zombie → Deallocated unlock detects zombie → deallocateUnconditional()
```

**CRITICAL: The "lock" is a CAS, not a mutex.** `tryLock` either succeeds immediately (CAS Idle→Running) or fails. No blocking. The only exception is `SWIFT_CONCURRENCY_ACTORS_AS_LOCKS` mode (task-to-thread, e.g. WASI).

**CRITICAL: The Zombie state exists** because the last `swift_release` can happen while a job is executing on the actor. The actor stays alive until `unlock()` finishes.

#### Executor Hop Fast Path (`swift_task_switchImpl`)

1. Compare current executor identity vs new executor identity
2. If same → tail-call resume directly (no switch)
3. If different, **fast path**: `canGiveUpThread` AND `tryAssumeThread` (CAS new actor Idle→Running) → swap thread from old actor to new actor without going through global pool
4. If fast path fails → **slow path**: enqueue task as Job on target actor's queue, return to drain loop

**CRITICAL: The drain loop can change actors mid-loop.** After `runJobInEstablishedExecutorContext`, the thread-local tracking info may point to a DIFFERENT actor (because `swift_task_switchImpl` swapped it during a fast-path hop).

#### MainActor is NOT a Default Actor

`MainActor` has a custom executor with a witness table (Implementation ≠ 0). It goes through the slow path for all hops. Its `enqueue` calls `_enqueueOnMain` which dispatches to the main thread/GCD/event loop.

#### Actor Reentrancy — Detailed Semantics

When an actor method hits `await`:
1. The task suspends (continuation saved)
2. Control returns to the drain loop — **the actor lock is NOT released**
3. The drain thread keeps draining and runs the NEXT job in the queue
4. This is reentrancy: another task's work runs on the same actor while the first is suspended
5. When the awaited value is ready, the first task's continuation is re-enqueued on the actor

**State can change across await points.** This is by design (prevents deadlocks) but is a common source of bugs.

#### Actor Deinit

Three paths:
1. **Already on correct executor**: run deinit inline
2. **Actor is idle** (`tryLock` succeeds): run deinit inline, unlock
3. **Actor is busy** (`tryLock` fails): wrap deinit in `IsolatedDeinitJob`, enqueue on actor — runs later

#### `nonisolated` — Purely Compile-Time

No runtime marker. The compiler simply omits the `hop_to_executor` instruction. The function runs on whatever executor the caller is on. `unownedExecutor` on Actor is itself `nonisolated` so the runtime can read it from any context.

---

### C.3: Sendable — Corrected Rules

#### Implicit Sendable Conformance (exact rules from TypeCheckConcurrency.cpp:7488)

1. **Actors** — always Sendable (handled separately)
2. **Global-actor-isolated types** — implicitly Sendable (the actor IS the safety)
3. **Non-public structs and enums** — if ALL stored properties/associated values are Sendable
4. **Public structs/enums** — must **explicitly** declare `: Sendable` (or be `@frozen`)
5. **Non-final classes** — **NEVER** conform to Sendable (compile error)
6. **Final classes** — only if no mutable stored properties (unless actor-isolated) and no non-Sendable superclass
7. **`~Sendable`** — explicitly suppresses implicit conformance
8. **Protocols** — never get implicit Sendable

#### @Sendable Closures

- Only by-value captures (never by reference)
- All captured values must be Sendable
- **@Sendable implies nonisolated** — cannot access actor-isolated state from enclosing context
- Non-@Sendable closures inherit isolation from parent context

#### Actor Isolation Inference (17-level priority chain)

1. Defer body → inherits enclosing isolation
2. Isolated self → ActorInstance
3. Isolated parameter → forActorInstanceParameter
4. Explicit attributes (`@nonisolated`, `@MainActor`, etc.)
5. `Main.main()` → implicitly @MainActor
6. Accessors → inherit from storage declaration
7. Local functions → inherit from enclosing if not @Sendable
8. Top-level globals → @MainActor in strict mode
9. Property wrappers → inherit from wrapper type
10. Dynamic replacements → inherit from replaced declaration
11. Protocol witnesses → inherit from protocol requirement
12. Superclass → inherit from superclass isolation
13. Conformances → inherit from protocol conformance
14. Extension isolation → inherit from extension attributes
15. Enclosing nominal type → inherit from type
16. `@IBAction` → implies @MainActor(unsafe)
17. Default → nonisolated

#### The Core Algorithm: `ActorReferenceResult::forReference()` (~170 lines)

This is THE function to port. It decides when `await` is required:

1. Compute declaration isolation (what is being accessed)
2. Compute context isolation (where the access is from)
3. If same actor/global actor → `SameConcurrencyDomain` (no await)
4. If declaration is nonisolated + async + context is actor-isolated → `ExitsActorToNonisolated` (await)
5. If cross-actor but accessible without hop (immutable Sendable `let`) → `EntersActor` (Sendable check, maybe no await)
6. Otherwise → `AsyncPromotion` (await required)

#### Recommendation: Implement Swift 6 Region-Based Isolation from Day One

**Swift 5 Sendable was too restrictive** — massive false positives, 4 years to fix. Cot should skip directly to Swift 6 regions.

**Region-based isolation (SE-0414)**: Every non-Sendable value belongs to an "isolation region." Regions merge on aliasing. A non-Sendable value can cross an actor boundary if its region is "disconnected" (not reachable from actor-isolated state) AND the caller doesn't use any value in that region afterwards.

**`sending` parameter (SE-0430)**: Argument must be in a disconnected region at call site. Caller cannot use it (or anything in its region) after the call. Essential for practical APIs like `CheckedContinuation.resume(returning: sending T)`.

**Region isolation is a forward dataflow analysis that maps cleanly to SSA** — each SSA value already has a single definition point. Cot's SSA infrastructure is well-suited for this.

---

### C.4: Cooperative Executor — Corrected Details

#### Linux Uses libdispatch

Surprising finding: Swift ships its own port of libdispatch (`swift-corelibs-libdispatch`) on ALL non-Apple platforms. Linux, Android, FreeBSD, OpenBSD, Windows all use `DispatchMainExecutor` + `DispatchGlobalTaskExecutor`.

The cooperative executor is **only** used for:
- Embedded Swift
- `SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY` (single-threaded, e.g. WASI)
- Platforms with `SWIFT_THREADING_NONE`

#### Thread Pool Size

`DISPATCH_QUEUE_WIDTH_MAX_LOGICAL_CPUS = -3` tells libdispatch to cap at CPU count. On an 8-core M1 = 8 threads. Not configurable from Swift. `LIBDISPATCH_COOPERATIVE_POOL_STRICT=1` limits to 1 thread for debugging.

#### No Work Stealing in Swift's Executor Layer

Swift enqueues jobs to priority-bucketed queues. libdispatch may use work stealing internally, but Swift's executor is a simple enqueue model.

#### Task.sleep — Lock-Free State Machine (5 states)

```
States: notStarted(0x0), activeContinuation(ptr), finished(0x1), cancelled(0x2), cancelledBeforeStarted(0x3)

Flow:
1. Allocate atomic state token
2. Install cancellation handler
3. Create UnsafeThrowingContinuation
4. CAS notStarted → activeContinuation
5. Create timer task with delay, enqueue on executor
6. Timer fires → CAS activeContinuation → finished → resume continuation
7. If cancelled → CAS activeContinuation → cancelled → resume with CancellationError
```

#### Recommendation: Start with CooperativeExecutor

~350 lines of Swift. It's exactly what Swift uses for embedded/WASI. Perfect for Cot's Wasm-first approach. Upgrade to thread pool executor for native later.

---

### C.5: TaskGroup — Corrected Internals

#### TaskGroup Does NOT Store Tasks

It stores only:
- **Atomic status word** (64 bits): `[1:cancelled][1:waiting][31:ready count][31:pending count]`
- **Wait queue** (atomic pointer): at most ONE waiting task (the parent)
- **Ready queue** (`std::queue<std::deque<ReadyQueueItem>>`): FIFO of completed results
- **Mutex**: protects the ready queue (TODO: lockless via mpsc)
- **ResultTypeInfo**: VWT for copying results

Child tasks are tracked as `ChildTaskStatusRecord` in the **parent task's** status record chain, not in the group.

#### next() Returns Completion Order (not submission order)

Results come back in the order tasks COMPLETE, not the order they were added. The ready queue is FIFO — first completed = first returned.

**Poll protocol in `AccumulatingTaskGroup::poll()`:**
1. Set waiting bit atomically
2. If pending=0 → return Empty (nil)
3. If ready queue has items → dequeue front, copy result via VWT, return immediately
4. If no ready + pending > 0 → store waiting task, suspend

**Offer protocol (when child completes):**
1. Lock, increment ready count
2. If waiting task exists → fill result directly, resume task on generic executor
3. If no waiter → retain task, enqueue into readyQueue for later `next()` consumption

#### async let Is Stack-Allocated (with heap fallback)

The compiler preallocates a fixed space for `AsyncLet` on the parent task's stack. The runtime uses this space if the child context fits; otherwise falls back to heap via parent's slab allocator. The child has **immortal refcounts** (no ARC overhead — lifetime structurally guaranteed).

At scope exit, `swift_asyncLet_finish` is called even if never explicitly awaited — this implicitly awaits then destroys. The child CANNOT outlive the scope.

#### AsyncStream Uses Deque + Platform Lock (not ring buffer)

- Internal buffer: `_Deque<Element>` (double-ended queue from stdlib)
- Locking: platform lock (`os_unfair_lock` on macOS) in tail-allocated storage
- **No backpressure**: producer never blocks. `yield()` return value is informational only (`.enqueued(remaining:)` or `.dropped(Element)`)
- **Continuation-based**: if consumer is waiting when yield() is called, the value goes directly to the continuation (bypasses buffer)
- **Single consumer only**: the throwing variant `fatalError`s if two tasks try to consume concurrently

#### DiscardingTaskGroup

For fire-and-forget work (server handlers, side effects). Discards successful results immediately — only stores the first error. Uses 62 bits for pending count (vs 31 for accumulating). If any child fails, cancels ALL siblings.

---

### C.6: Async Function Lowering — Corrected Approach

#### DO NOT Use LLVM Coroutines

Swift uses `@llvm.coro.id.async` + `@llvm.coro.suspend.async` + LLVM's `CoroSplit` pass. This is deeply tied to LLVM and inapplicable to Cot (which targets Wasm directly without LLVM).

#### Use Manual State Machine Splitting (Kotlin CPS Pattern)

**Recommended for Cot.** Transform each async function into a state machine at the SSA level:

1. Each `await` point becomes a state transition
2. Values live across await points are spilled into a heap-allocated frame
3. The function becomes a poll/resume function with `switch (state.phase)` dispatch
4. **br_table** for state dispatch — Cot already has this infrastructure from Go's dispatch loop

This matches:
- **Kotlin/Wasm**: CPS transform before codegen
- **Rust**: `Future::poll()` state machine (though Rust does this at MIR level)
- **Cot's current partial implementation**: already has single-await state machines on Wasm

#### AsyncContext Layout (from Swift, simplified for Cot)

```
Offset 0: parent         (8B) — pointer to caller's async frame (linked list)
Offset 8: resumeFn       (8B) — continuation function pointer
Offset 16: ...spilled locals... (variable) — values alive across await points
```

The parent pointer chain enables async stack traces. Each async call allocates a new frame via bump-pointer allocator; each return deallocates (LIFO).

#### Wasm Stack Switching Status (2026)

**Not ready for production.** Phase 2 in WebAssembly CG, needs new champion, uncertain timeline. V8 has experimental support behind flag. Wasmtime has tracking issue, x86_64 Linux only.

**Cot must use state machines for now.** When stack switching ships (~2028+?), the runtime can be swapped without changing language semantics.

#### Performance Note

Swift async overhead: ~2-5x per call vs sync for non-suspending calls (context allocation + indirect return). On Wasm: slightly worse because `musttail` is unavailable (stack grows temporarily during async return). Manual state machines are predictable — no hidden allocations unless actually suspending.

#### CheckedContinuation Double-Resume Detection

Uses `Builtin.atomicrmw_xchg_seqcst_Word` to atomically swap the continuation pointer with NULL:
- If old value was non-null → first resume, proceed
- If old value was null → double resume, **fatalError**
- In deinit, if pointer still non-null → leaked continuation, log warning

Cot should implement this identically using `@atomicExchange`.

---

### C.7: Cot-Specific Architectural Decisions

Based on all 6 audits, these are the recommended decisions for Cot:

#### 1. Single-Threaded First, Multi-Threaded Later

Start with the CooperativeExecutor (single-threaded run loop). This is:
- What Swift uses for embedded/WASI
- Perfect for Cot's Wasm-first approach
- Simpler to implement (no atomics needed for executor)
- Actor isolation still enforced at compile time (so code is safe when multi-threading is added later)

#### 2. Skip Priority Escalation

Only matters with multi-threaded execution + actor contention. Add when native thread pool is implemented. Use the simpler `ActiveTaskStatus` layout (no dispatch_lock_t, no 128-bit atomics).

#### 3. Simplified Actor Implementation for Wasm

On Wasm (single-threaded), actors don't need:
- Lock-free MPSC ingress queue (no concurrent producers)
- Priority-bucketed drain queue (single thread, FIFO is fine)
- CAS-based state machine (no contention)
- Zombie state (no concurrent dealloc)

They DO need:
- Compile-time isolation checking (the whole point)
- A simple FIFO job queue (for reentrancy: when an actor method awaits, other enqueued work runs)
- The `await` requirement at cross-actor boundaries

#### 4. Region Isolation Maps to SSA

Implement region-based isolation as a forward dataflow pass over the SSA graph:
- Each SSA value starts in a "disconnected" region
- Accessing actor state assigns the region to that actor
- Passing a value to a function merges regions of aliasing values
- At actor boundaries: check that transferred values are in disconnected regions
- After transfer: mark the region as "consumed" (dead)

#### 5. VWT Integration Points

- **TaskGroup result collection**: `vw_initializeWithCopy` from child's result storage to parent's buffer
- **AsyncStream buffer**: VWT for element copy/destroy in the Deque
- **Task-local values**: VWT for type-erased storage
- **Future completion**: `vw_initializeWithCopy` from FutureFragment to each waiter's result pointer
- **Actor stored properties**: VWT for copy/destroy during actor lifecycle

All of these already work via Cot's existing VWT infrastructure.

---

## Appendix D: Full Audit & Recommendations (2026-03-26 Session 2)

**This appendix contains findings from three parallel audits: Swift source code, Cot's current async implementation, and web research on production Swift concurrency. Future Claude sessions MUST read this.**

---

### D.1: Current Cot Async Status — BROKEN, Safe to Delete

The existing Go-style concurrency code is **half-built and non-functional**:

| Component | Status | Lines | Details |
|-----------|--------|-------|---------|
| Parser (spawn, select, async, await) | Parses correctly | ~200 | AST nodes for SpawnExpr/SelectExpr created via inline structs |
| Checker (async) | Minimal | ~50 | Returns Future(T) type but Future not in Type union |
| Lowerer (spawn/select/await) | Partially works | ~1,200 | Calls non-existent runtime functions |
| Runtime (scheduler, threads) | **Missing entirely** | 0 | `sched_spawn`, `sched_select` declared but never implemented |
| Stdlib (async.cot, channel.cot, thread.cot) | Partially written | ~615 | No test coverage, known channel ring buffer bug |
| Tests | **Deleted** | ~1,062 | 7 test files existed but were deleted as part of Go concurrency cleanup |
| Selfcot (self/build/lower.cot) | Ported from Zig | ~850 | Same broken calls to missing runtime |

**Verdict:** Clean deletion. Nothing works end-to-end. No user code depends on it. The only reusable piece is `detectCaptures()` in the lowerer (used by closures too).

---

### D.2: Swift Cooperative Executor — NOT a Thread Pool

The document's "cooperative thread pool" description is misleading. The actual Swift implementation:

**Production Swift (Darwin):** Delegates to libdispatch's cooperative dispatch queue. One thread per CPU core (hard cap). This is NOT available on non-Darwin platforms.

**CooperativeGlobalExecutor.cpp (what we'd port):** A **single-threaded** FIFO event loop:
- Priority queue with 5 buckets (dispatch QoS levels)
- Delayed job queue (sorted by deadline)
- `swift_task_enqueueMainExecutor` = same as global (no distinction in cooperative mode)
- NO background worker threads — everything runs inline in the calling thread
- When no work: sleep until next delayed job deadline

**Implication for Cot:** The cooperative executor is already what we want for Wasm. For native, we wrap it in a thread pool (one executor per thread, work-stealing between them). The executor itself stays single-threaded per worker.

---

### D.3: Compilation Strategy — State Machines Everywhere, Optimize Later

The web research revealed two async lowering approaches:

**Swift's approach:** Heap-allocated continuation frames (AsyncContext). Every `await` allocates. ~400-800 bytes per task creation. ~50-100ns overhead per async call.

**Rust's approach:** Compile async functions to state machines with known size at compile time. Zero heap allocation. The `Future` IS the state machine.

**Recommendation for Cot:** Use **state machines for both Wasm and native** initially. One lowering strategy, one code path, fewer bugs. This is critical for self-hosting — selfcot would need to implement both paths otherwise, doubling the surface area.

State machines work on native too (Rust proves this at scale). The performance difference vs heap-allocated continuations is negligible for most workloads, and state machines are actually *faster* (zero alloc per call).

AsyncContext-style continuations can be added later as a **native-only optimization** if profiling shows the state machine approach has limitations (e.g., deeply recursive async call chains where the state machine struct becomes very large). But start simple — one path, one implementation, one thing to debug.

---

### D.4: Actor Reentrancy — CHANGE FROM SWIFT DEFAULT

Swift actors are reentrant by default (SE-0306). This is the #1 developer complaint:

```cot
actor BankAccount {
    var balance = 100
    fn withdraw(amount: int) async bool {
        if (balance < amount) { return false }
        await sendNotification()     // ANOTHER withdraw can execute HERE
        balance -= amount            // balance might now be negative!
        return true
    }
}
```

**Real-world problems:**
- State invalidation across await points (checked value becomes stale)
- Message ordering surprises (two sequential awaits may interleave)
- Database/file corruption from interleaved writes

**Recommendation:** Make Cot actors **non-reentrant by default**. Add `@reentrant` opt-in for advanced use cases. The deadlock risk from non-reentrancy is manageable with timeouts; reentrancy bugs are far more insidious and harder to debug.

---

### D.5: AsyncStream Lacks Backpressure — Use Go Channels Instead

Swift's `AsyncStream` has NO backpressure. When the buffer fills:
- `bufferingOldest(N)`: silently drops NEW elements
- `bufferingNewest(N)`: silently drops OLD elements
- `unbounded`: unbounded memory growth

The producer CANNOT suspend waiting for space. `yield()` returns `.dropped` but there's no await-based flow control.

**Recommendation:** Don't port AsyncStream's design. Use Go-style buffered channels with proper backpressure:

```cot
let ch = Channel(int, capacity: 10)
await ch.send(value)    // suspends if buffer full
let v = await ch.recv() // suspends if buffer empty
```

This is the one place where Go's design is strictly better than Swift's. The `AsyncSequence` trait is still valuable — `Channel` can conform to it for `for await` syntax.

---

### D.6: Sendable Checking — Cot's ARC+COW Makes This Easier

Swift's Sendable adoption was painful because of retrofitting. Cot has advantages:

1. **COW collections are inherently Sendable.** `List(T)` copies on mutation, so sending across actors is always safe (just retain the shared buffer).
2. **No legacy code to break.** Ship Sendable as errors from day 1.
3. **ARC + unique ownership.** If refcount == 1, a value can be MOVED across actors (no copy needed). This is `sending` parameter semantics for free.
4. **No raw pointers in user code.** Cot's `RawPtr` is `distinct i64` — only used in stdlib internals. User code naturally produces Sendable types.

**Recommendation:** Most Cot types should be **implicitly Sendable** without annotation. Only reject at boundaries when a type contains: raw pointers, mutable reference types (non-actor), or closures capturing mutable state.

---

### D.7: Swift Source ABI Details — Corrections to Main Document

From the Swift source audit:

**DefaultActor private data is exactly 12 words (96 bytes on 64-bit).** Not dynamically sized. Contains: status state machine, job queue head, lock — all inline.

**Executor kind bits are packed in the low 3 bits of the witness table pointer.** Must mask before dereferencing: `wtable = Implementation & ~(alignof(void*) - 1)`. Three kinds: Ordinary (00), ComplexEquality (01), Immediate (10).

**JobFlags at fixed memory offset.** Schedulers (including libdispatch) read job priority directly from memory at `Job + 16` without a function call. This is an ABI contract — do NOT change the layout.

**Task groups do NOT auto-cancel on child error.** Only throwing from the `body` closure cascades cancellation. Individual child errors from `next()` do NOT cancel siblings. The caller controls the lifecycle.

---

### D.8: Revised Effort Estimates

Given Claude's porting velocity (200K+ lines in 10 weeks) and the existing infrastructure:

| Phase | Effort | Rationale |
|-------|--------|-----------|
| **0. Delete Go concurrency** | 1 day | ~5,000 lines, no dependencies |
| **1. Task Runtime** | 2-3 days | TaskObject is just an ARC heap object, executor is a priority queue |
| **2. async/await (Wasm)** | 3-4 days | State machine infra partially exists, needs executor integration |
| **2b. async/await (Native)** | 3-4 days | AsyncContext + continuation passing, new codegen path |
| **3. Actors** | 4-5 days | Serial executor queue + checker isolation rules |
| **4. Sendable** | 2-3 days | Mostly a checker pass, COW makes most types implicit |
| **5. TaskGroup + async let** | 3-4 days | Child task tracking, structured cancellation |
| **6. AsyncSequence + Channel** | 2-3 days | Trait + Go-style channel with backpressure |
| **7. Global Actors** | 1-2 days | Singleton pattern on top of Phase 3 |
| **8. Cancellation + Locals** | 1-2 days | Flag checking + task-local value stack |
| **9. Wasm executor** | 2-3 days | Single-threaded event loop, browser glue |
| **10. Continuations** | 1-2 days | Checked continuation bridge |
| **Total** | ~25-35 days | ~5-7 weeks |

---

### D.9: Implementation Order — Revised

```
Phase 0: Delete Go concurrency (FIRST — clean slate)
    │
Phase 1: Task Runtime (ARC heap object + priority queue executor)
    │
Phase 2: async/await lowering (Wasm state machine + native continuation)
    ├── This is the hardest phase — get a basic await working E2E
    │
Phase 4: Sendable checking (DO THIS BEFORE ACTORS)
    │   Sendable is a pure checker pass, no runtime. Having it ready
    │   means actor isolation is safe from the start.
    │
Phase 3: Actors (serial executor + isolation checking)
    │   Requires Sendable to be meaningful
    │
Phase 5: TaskGroup + async let
    │
Phase 6: Channel (Go-style with backpressure) + AsyncSequence trait
    │
Phase 7-10: Global actors, cancellation, locals, continuations
    │   These build incrementally on the foundation
    │
Phase 9: Wasm executor integration (can be done anytime after Phase 2)
```

Key change from original: **Sendable before Actors.** Without Sendable, actors compile but aren't actually safe — you can send non-Sendable types across boundaries. Get the checking in place first.

---

### D.10: What to Preserve from Current Code

| Component | Action | Reason |
|-----------|--------|--------|
| `detectCaptures()` in lower.zig | **Keep** | Used by closures AND will be needed for Task {} capture checking |
| `async`/`await` keywords in parser | **Keep** | Same syntax, new semantics |
| `kw_spawn`, `SpawnExpr` in parser | **Delete** | Replaced by `Task {}` |
| `kw_select`, `SelectExpr` in parser | **Delete** | Replaced by `for await in` |
| `stdlib/thread.cot` Atomic(T) | **Keep** | Universal primitive, actors need it internally |
| `stdlib/thread.cot` Mutex/Condition/RwLock | **Delete** | Internal to executor, not user-facing |
| `stdlib/channel.cot` | **Delete** | Replaced by new Channel with backpressure |
| `stdlib/async.cot` event loop | **Delete** | Replaced by executor |
| `scheduler_native.zig` | **Delete** | Replaced by cooperative executor |
| `thread_native.zig` | **Delete** | Replaced by task runtime |
| Wasm state machine infrastructure | **Refactor** | Core approach is correct, needs executor integration |
