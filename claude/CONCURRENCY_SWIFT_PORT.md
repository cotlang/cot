# Concurrency: Swift Port Status & Remaining Work

**Date:** 2026-03-27
**Goal:** 1:1 port of Swift's structured concurrency to Cot. No hacks, no workarounds.
**References:** Swift stdlib/public/Concurrency/, lib/Sema/TypeCheckConcurrency.cpp, include/swift/ABI/Task.h

---

## Current State

**Tests:** 414 native, 66+ Wasm, all green. Selfcot builds (2698 functions).

| Feature | Swift SE | Status | Fidelity |
|---------|----------|--------|----------|
| async/await syntax | SE-0306 | Done | FAITHFUL (eager Phase 1) |
| Task {} expression | SE-0304 | Done | FAITHFUL (eager Phase 1) |
| Task.detached {} | SE-0304 | Done | FAITHFUL (eager Phase 1) |
| Actor keyword + isolation checking | SE-0306 | Done | FAITHFUL (compile-time only) |
| nonisolated | SE-0313 | Done | FAITHFUL |
| Sendable checking | SE-0302 | Done | FAITHFUL (matches TypeCheckConcurrency.cpp) |
| @Sendable closures | SE-0302 | Done | FAITHFUL (capture checking) |
| @unchecked Sendable | SE-0302 | Done | FAITHFUL |
| TaskGroup + for-await | SE-0304 | Done | SIMPLIFIED (eager, sequential) |
| @MainActor | SE-0316 | Done | FAITHFUL (compile-time isolation) |
| TaskLocal(T) | SE-0311 | Done | FAITHFUL (global stack, LIFO) |
| Continuation(T) | SE-0300 | Done | FAITHFUL (checked, no actual suspension) |
| async let | SE-0317 | Done | SIMPLIFIED (sequential, not parallel) |
| Channel(T) | Go-style | Done | SIMPLIFIED (synchronous send/recv) |
| Executor | SE-0392 | Done | STUB (trivial, no scheduling) |
| Cancellation flag | SE-0340 | Done | STUB (flag allocated, never used) |
| Task.sleep | SE-0374 | Done | **WRONG** (busy-wait, not async suspension) |
| Constructor/poll split | Rust coroutine.rs | Done | INVENTED (Wasm-only, native skips) |

---

## Audit Findings: What Needs Fixing

### CRITICAL: Actor Runtime (SE-0306)

**Problem:** Actors are stack-allocated value types. No serial executor, no ARC heap allocation, no runtime isolation.

**Swift's design (ActorIsolation.cpp, Task.h):**
- Actors are reference types (ARC heap objects, 96 bytes = 12 words)
- Each actor has a serial executor queue (LIFO ingress, FIFO drain)
- State machine: Idle -> Scheduled/Running -> Zombie -> Deallocated
- Method calls enqueue on serial executor, guaranteeing mutual exclusion
- `await actor.method()` hops to actor's executor at runtime

**Cot's current design:**
- `actor` is just `struct` with `is_actor = true`
- Stack-allocated, no executor queue
- `await actor.method()` runs immediately on caller's thread
- Compile-time isolation checking works, but runtime is unsafe
- Multiple threads can mutate same actor simultaneously

**Fix required:**
1. Actors allocated via `new` (ARC heap objects) like Swift
2. Serial executor queue per actor (single-consumer, multi-producer)
3. Method calls enqueue work item on actor's queue
4. `await` suspends caller until actor processes the work item
5. Phase 1 (single-threaded): execute immediately but through queue for correctness
6. Phase 2 (multi-threaded): actual thread pool + serial execution guarantee

### CRITICAL: Task.sleep (SE-0374)

**Problem:** Implemented as busy-wait CPU loop. Not async suspension.

**Swift's design (TaskSleep.swift):**
- Lock-free state machine: notStarted -> activeContinuation -> finished/cancelled
- Uses continuation internally to create suspension point
- Executor reschedules task after delay
- Cooperative: other tasks can run during sleep

**Fix required:**
- Replace busy-wait with continuation-based suspension
- Integrate with executor scheduling (timer queue)
- Blocked on: real executor implementation

### HIGH: Real Executor (SE-0392)

**Problem:** Current executor is a stub. `run()` marks all tasks complete immediately.

**Swift's design (CooperativeExecutor.swift):**
- FIFO event loop with 5 priority buckets
- `enqueue(job)` adds to priority queue
- `runUntil()` polls tasks, re-enqueues PENDING, removes READY
- Timer-based scheduling for sleep/deadlines

**Fix required:**
1. Priority queue with 5 levels (high, medium, low, utility, background)
2. Poll loop: call task's resume function, check return state
3. Re-enqueue PENDING tasks, complete READY tasks
4. Timer queue for Task.sleep scheduling
5. Wasm: single-threaded event loop (perfect fit for cooperative executor)
6. Native: wrap cooperative executor in thread pool (Phase 2+)

### HIGH: Cancellation API (SE-0340)

**Problem:** TaskObject allocates cancelled flag at offset 8 but it's never read or written.

**Swift's design (Task.h):**
- `swift_task_cancel(task)` sets IsCancelled flag
- `Task.isCancelled` reads flag (cooperative checking at suspension points)
- `withTaskCancellationHandler` registers cleanup handler

**Fix required:**
1. `task.cancel()` method that sets flag at task[8]
2. `Task.isCancelled` static property reading current task's flag
3. `Task.checkCancellation()` that throws CancellationError if set
4. `withTaskCancellationHandler(operation:onCancel:)` API
5. Cancellation propagation: parent cancelled -> children cancelled

### HIGH: Async Suspension (Phase 2 Core)

**Problem:** All async code runs eagerly. No actual suspension at await points.

**Swift's design:**
- Heap-allocated AsyncContext frames
- Parent context pointer + resume function pointer + spilled locals
- await = save state to frame, return to executor, executor resumes later
- Cooperative: executor runs other tasks while one is suspended

**Fix required (state machine approach per design doc D.3):**
1. Split async functions at await points into state machine
2. Heap-allocated frame stores spilled locals + state number
3. Poll function: switch on state, execute segment, return PENDING or READY
4. Executor polls tasks in loop until all complete
5. Works for BOTH Wasm (event loop) and native (cooperative thread pool)

### MEDIUM: Channel Async (Go-style)

**Problem:** send() and recv() are synchronous. No backpressure.

**Design doc says:** Go-style channels with `async fn send()` that suspends when full and `async fn recv()` that suspends when empty. This is a deliberate divergence from Swift's AsyncStream (which lacks backpressure).

**Fix required:**
1. `async fn send(value: T)` — suspends caller if buffer full
2. `async fn recv() ?T` — suspends caller if buffer empty
3. Wakes waiting sender when consumer drains
4. Wakes waiting receiver when producer sends
5. Blocked on: real executor + async suspension

### MEDIUM: TaskGroup Parallel Execution

**Problem:** `addTask(body)` calls body immediately. No parallel execution.

**Swift's design (TaskGroup.swift):**
- `addTask` creates child task, enqueues on executor
- Tasks run in parallel on thread pool
- `next()` returns results in completion order (not submission order)
- Child tasks tracked via ChildTaskStatusRecord

**Fix required:**
1. `addTask` enqueues work on executor instead of calling immediately
2. Results collected as tasks complete (completion order)
3. Structured cancellation: parent cancelled -> all children cancelled
4. Error propagation from children to parent
5. Blocked on: real executor

### MEDIUM: async let Parallel Spawning (SE-0317)

**Problem:** `async let x = f()` evaluates f() immediately, sequentially.

**Swift's design (AsyncLet.swift):**
- Creates child task that runs in parallel
- Stack-allocated with heap fallback
- Immortal refcount (no ARC overhead)
- Automatically cancelled if parent scope exits before await

**Fix required:**
1. async let spawns child task on executor
2. `await x` suspends until child completes
3. Implicit cancellation at scope exit
4. Blocked on: real executor + async suspension

### MEDIUM: TaskLocal Child Inheritance (SE-0311)

**Problem:** Global stack per TaskLocal. No per-task storage, no child inheritance.

**Swift's design (TaskLocal.swift):**
- Per-task linked list of bindings
- `swift_task_localValuePush` / `swift_task_localValuePop`
- Child tasks automatically copy parent's bindings

**Fix required:**
1. Per-task storage (requires real Task runtime object)
2. `withValue` pushes onto current task's binding chain
3. Child task creation copies parent's bindings
4. Blocked on: real Task runtime

### LOW: Associated Types in Traits

**Problem:** Cot traits don't support `type Iterator: Protocol` syntax.

**Needed for:**
- `trait AsyncSequence(Element) { type Iterator: AsyncIterator(Element) }`
- `trait AsyncIterator(Element) { async fn next() ?Element }`

**Fix required:**
1. Parser: allow `type Name: Bound` in trait body
2. Checker: resolve associated types at impl sites
3. No runtime impact (compile-time only)

### LOW: AsyncStream (SE-0314)

**Problem:** Not implemented. Blocked on associated types + real executor.

**Swift's design (AsyncStream.swift, AsyncStreamBuffer.swift):**
- Builder pattern: `AsyncStream { continuation in ... }`
- Internal _Storage with lock (os_unfair_lock)
- Buffering policies: unbounded, bufferingOldest(N), bufferingNewest(N)
- Continuation.yield/finish/onTermination

**Blocked on:** Associated types in traits, real executor, Continuation suspension

### LOW: sending Parameter (SE-0430)

**Problem:** Not implemented. Type-level transfer checking.

**Fix required:**
- New keyword `sending` on function parameters
- Checker enforces value is disconnected from caller's region
- Forward dataflow analysis on SSA (per design doc C.3)

### LOW: Region Isolation (SE-0414)

**Problem:** Not implemented. Advanced Sendable inference.

**Fix required:**
- Forward dataflow analysis on SSA graph
- Track which values belong to which isolation region
- Infer Sendable automatically for disconnected values

---

## Implementation Order

Each phase builds on the previous. No phase can be skipped.

### Phase A: Real Executor (foundation for everything)
**Effort:** 3-4 days
**Reference:** CooperativeExecutor.swift
**Delivers:** Working cooperative executor with priority queue, poll loop, timer queue

1. Priority queue with 5 levels
2. `enqueue(poll_fn, frame)` queues work items
3. `run()` poll loop: call resume function, check result, re-enqueue or complete
4. Timer queue for scheduled wakeups (Task.sleep)
5. Tests: executor drains tasks, priority ordering, timer wakeup

### Phase B: Async Suspension (state machine approach)
**Effort:** 6-8 days
**Reference:** Kotlin CPS transform, Rust coroutine.rs, existing async_split.zig
**Delivers:** Real suspension at await points, cooperative multitasking

1. Generalize async_split.zig to work for ALL async functions (not just __poll)
2. Heap-allocated state frame: state number + spilled locals + result
3. Poll function: switch on state, execute segment, return PENDING/READY
4. `await` = yield to executor with continuation
5. Integrate with Phase A executor: enqueue continuation, executor polls
6. Works for both Wasm and native
7. Tests: multiple tasks interleaved, await actually suspends

### Phase C: Actor Runtime (SE-0306)
**Effort:** 4-5 days
**Reference:** DefaultActor (12 words), ActorIsolation.cpp
**Delivers:** Real actor isolation with serial executor queue

1. Actors allocated via `new` (ARC heap objects)
2. Per-actor serial executor queue (MPSC)
3. `await actor.method()` enqueues work on actor's queue
4. Actor processes one job at a time (serial execution guarantee)
5. Non-reentrant by default (Cot's documented divergence)
6. Tests: concurrent actor access serialized, isolation enforced at runtime

### Phase D: Cancellation (SE-0340)
**Effort:** 2-3 days
**Reference:** Task.h cancellation flags, withTaskCancellationHandler
**Delivers:** Working cancellation API

1. `task.cancel()` sets flag at TaskObject[8]
2. `Task.isCancelled` reads current task's flag
3. `Task.checkCancellation()` throws if cancelled
4. `withTaskCancellationHandler(operation:onCancel:)` API
5. Propagation: parent cancelled -> children cancelled
6. Tests: cancel flag set/read, handler invoked, propagation to children

### Phase E: Task.sleep Fix (SE-0374)
**Effort:** 1-2 days
**Reference:** TaskSleep.swift lock-free state machine
**Depends on:** Phase A (executor timer queue)
**Delivers:** Real async sleep

1. Replace busy-wait with continuation-based suspension
2. Register wakeup time with executor's timer queue
3. Executor reschedules task after deadline
4. Tests: sleep doesn't block other tasks, timer accuracy

### Phase F: TaskGroup Parallel (SE-0304)
**Effort:** 3-4 days
**Reference:** TaskGroup.swift, ChildTaskStatusRecord
**Depends on:** Phase B (async suspension)
**Delivers:** Parallel task execution in groups

1. `addTask` enqueues child task on executor
2. Results returned in completion order
3. Structured cancellation (parent -> children)
4. Error propagation
5. Tests: tasks run in parallel, completion order, cancellation propagation

### Phase G: async let Parallel (SE-0317)
**Effort:** 2-3 days
**Reference:** AsyncLet.swift
**Depends on:** Phase B (async suspension)
**Delivers:** Parallel async let bindings

1. `async let x = f()` spawns child task
2. `await x` suspends until child completes
3. Implicit cancellation at scope exit
4. Tests: parallel execution, scope cancellation

### Phase H: Channel Async
**Effort:** 2-3 days
**Reference:** Go channels (documented divergence from Swift)
**Depends on:** Phase B (async suspension)
**Delivers:** Async send/recv with backpressure

1. `async fn send(value: T)` — suspends when buffer full
2. `async fn recv() ?T` — suspends when buffer empty
3. Wake mechanisms: send wakes waiting receiver, recv wakes waiting sender
4. Tests: backpressure, multiple producers/consumers

### Phase I: TaskLocal Inheritance (SE-0311)
**Effort:** 2 days
**Reference:** TaskLocal.swift
**Depends on:** Phase B (real Task runtime objects)
**Delivers:** Per-task storage with child inheritance

1. Per-task binding chain (not global stack)
2. Child tasks copy parent's bindings at creation
3. Tests: child task inherits parent's task-local values

### Phase J: Associated Types in Traits
**Effort:** 3-4 days
**Reference:** Swift protocol associated types
**Delivers:** `type Iterator: AsyncIterator(Element)` in trait definitions

1. Parser: `type Name: Bound` in trait body
2. Checker: resolve associated types at impl sites
3. Type substitution at call sites
4. Tests: AsyncSequence/AsyncIterator trait definitions

### Phase K: AsyncStream (SE-0314)
**Effort:** 5-6 days
**Reference:** AsyncStream.swift, AsyncStreamBuffer.swift
**Depends on:** Phase J (associated types), Phase B (suspension), Phase D (cancellation)
**Delivers:** Callback-to-async bridge

1. AsyncSequence + AsyncIterator traits (using associated types)
2. AsyncStream struct with builder pattern
3. Internal _Storage with locking (os_unfair_lock/pthread_mutex)
4. Buffering policies: unbounded, bufferingOldest(N), bufferingNewest(N)
5. Continuation.yield/finish/onTermination
6. Tests: all buffering policies, cancellation, multiple consumers

### Phase L: sending + Region Isolation (SE-0430, SE-0414)
**Effort:** 5-6 days
**Reference:** TypeCheckConcurrency.cpp region inference
**Delivers:** Advanced Sendable inference, sending keyword

1. `sending` keyword on function parameters
2. Forward dataflow analysis on SSA for region tracking
3. Automatic Sendable inference for disconnected values
4. Tests: sending parameters, region isolation inference

---

## Documented Divergences from Swift

These are intentional design decisions, not bugs:

1. **Non-reentrant actors by default** (Swift: reentrant by default, SE-0306)
   - Swift's #1 developer complaint. State invalidation across await points.
   - Cot: non-reentrant default, `@reentrant` opt-in
   - Deadlock risk manageable with timeouts

2. **Go-style Channel with backpressure** (Swift: AsyncStream without backpressure)
   - Swift AsyncStream silently drops or grows unbounded
   - Cot: buffered channel with `async fn send()` that suspends when full
   - AsyncSequence trait still used for `for await` syntax

3. **State machines for both Wasm and native** (Swift: heap-allocated continuations)
   - Swift uses LLVM coroutine lowering (not applicable to Cot)
   - Design doc D.3: state machines are faster (zero alloc per call), simpler (one code path)
   - Can add AsyncContext optimization later if profiling shows need

4. **No function coloring** (Swift: async keyword required in caller)
   - `await` blocks in sync contexts (like Zig)
   - Async functions callable from anywhere

5. **Sendable errors from day one** (Swift: gradual migration)
   - No legacy code to break
   - COW collections inherently Sendable
   - ARC + unique ownership: refcount==1 means MOVE (free)

---

## Estimated Total: 35-45 days (7-9 weeks)

| Phase | Days | Depends on |
|-------|------|-----------|
| A: Real Executor | 3-4 | — |
| B: Async Suspension | 6-8 | A |
| C: Actor Runtime | 4-5 | B |
| D: Cancellation | 2-3 | A |
| E: Task.sleep Fix | 1-2 | A |
| F: TaskGroup Parallel | 3-4 | B |
| G: async let Parallel | 2-3 | B |
| H: Channel Async | 2-3 | B |
| I: TaskLocal Inheritance | 2 | B |
| J: Associated Types | 3-4 | — |
| K: AsyncStream | 5-6 | J, B, D |
| L: sending + Region | 5-6 | B |
| **Total** | **39-50** | |

---

## Test Plan

Each phase must pass its own tests + all previous tests. No regressions.

**Current baseline:** 414 native, 66+ Wasm, selfcot builds.

**Phase completion criteria:**
- All new tests pass on native AND Wasm
- All existing tests still pass (no regressions)
- Selfcot builds and runs `version`
- Code reviewed against Swift reference (cite specific files/lines)
