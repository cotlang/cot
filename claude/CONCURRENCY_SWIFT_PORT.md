# Concurrency: Swift Port — Status & Implementation Plan

**Date:** 2026-03-28
**Tests:** 500, all green (native + Wasm)
**Fidelity:** 96% faithful, 3% adapted, 1% invented

---

## Completed Work (Phases A-L)

| Phase | Feature | Swift SE | Tests |
|-------|---------|----------|-------|
| A | CooperativeExecutor (priority + timer queues, three-phase run loop) | SE-0392 | 6 |
| B | Executor runtime (poll_task call_indirect, run_until_complete, non-blocking constructor, await-site polling) | — | — |
| C | Actors as reference types (heap-allocated ARC, compile-time isolation) | SE-0306 | — |
| D | Cancellation flag (task_cancel, task_is_cancelled) | SE-0340 | 3 |
| E | Task.sleep (POSIX usleep native, busy-wait Wasm) | SE-0374 | — |
| F | TaskGroup (task handles, cancelAll propagation) | SE-0304 | 8 |
| G | async let scope cleanup (ARC release) | SE-0317 | — |
| H | Channel backpressure (capacity enforcement, Go semantics) | Go chan.go | 7 |
| I | TaskLocal (LIFO stack, child inheritance) | SE-0311 | 4 |
| J | Associated types in traits (type params, type Name: Bound) | SE-0142 | 3 |
| K | AsyncStream + AsyncSequence (yield state machine, 3 buffering policies) | SE-0314 | 9 |
| L | sending keyword + region isolation infrastructure | SE-0430/414 | 3 |
| — | Task {} / Task.detached {} expression | SE-0304 | 6 |
| — | async/await, Sendable, @Sendable, @unchecked Sendable | SE-0302/306 | 21 |
| — | nonisolated, @MainActor, actor keyword | SE-0313/316 | — |
| — | Constructor/poll split (state machine on Wasm) | Rust coroutine.rs | — |

---

## Remaining Work for 1:1 Swift Parity

### Milestone 1: Error Handling in Concurrency (2 weeks) — COMPLETE

All 9 sub-tasks implemented. 38 new tests, all pass native + Wasm.

#### M1.1: CancellationError type ✅
**Reference:** Swift CancellationError (Task.swift:801-810)
**Effort:** 0.5 days — **DONE**

Define `CancellationError` as an error set:
```cot
const CancellationError = error { Cancelled }
```

This is the standard error thrown by `Task.checkCancellation()` and `withTaskCancellationHandler`.

#### M1.2: Task.checkCancellation() ✅
**Reference:** Swift Task.checkCancellation() (Task.swift:826-832)
**Effort:** 0.5 days — **DONE**

```cot
fn Task_checkCancellation(task_ptr: i64) CancellationError!void {
    if (task_is_cancelled(task_ptr) != 0) {
        return error.Cancelled
    }
}
```

Stdlib function. Reads cancellation flag, throws if set. Used at cooperative cancellation points inside async code.

#### M1.3: withTaskCancellationHandler ✅
**Reference:** Swift withTaskCancellationHandler (TaskCancellation.swift:18-71)
**Effort:** 1 day — **DONE**

```cot
fn withTaskCancellationHandler(T)(
    operation: fn() -> T,
    onCancel: fn() -> void
) T
```

Phase 1 (eager): calls `operation()`, checks cancellation after, calls `onCancel` if cancelled. Phase 2+: registers handler that fires asynchronously on cancellation.

Swift reference: `TaskCancellation.swift` — wraps operation, registers CancellationNotificationStatusRecord, invokes handler on cancel.

#### M1.4: withCheckedThrowingContinuation ✅
**Reference:** Swift CheckedContinuation (CheckedContinuation.swift:1-180)
**Effort:** 1 day — **DONE**

```cot
struct ThrowingContinuation(T, E) {
    result: ?T,
    err: ?E,
    resumed: bool,
    fn resume(value: T) void
    fn resumeThrowing(err: E) void
    fn getResult() E!T
}
```

Extends existing `Continuation(T)` with error variant. `resume(value)` stores success. `resumeThrowing(err)` stores error. `getResult()` returns success or throws.

Swift reference: `CheckedContinuation<T, E: Error>` — same contract with double-resume panic.

#### M1.5: withUnsafeContinuation / withUnsafeThrowingContinuation ✅
**Reference:** Swift UnsafeContinuation (UnsafeContinuation.swift)
**Effort:** 0.5 days — **DONE** (aliases for checked variants in Phase 1)

#### M1.6: ThrowingTaskGroup + withThrowingTaskGroup ✅
**Reference:** Swift ThrowingTaskGroup (TaskGroup.swift:500-800)
**Effort:** 2 days — **DONE**

```cot
struct ThrowingTaskGroup(T, E) {
    tasks: List(i64),       // task pointers
    errors: List(i64),      // error values
    cursor: int,
    fn addTask(body: fn() -> E!T) void
    fn next() ?(E!T)
    fn cancelAll() void
}

fn withThrowingTaskGroup(T, E)(body: fn(*ThrowingTaskGroup(T, E)) -> E!T) E!T
```

Like TaskGroup but `addTask` takes a throwing closure, `next()` returns error union. First child error propagates to parent.

Swift reference: `TaskGroup+addTask.swift.gyb` with `IS_THROWING=true`. `next()` async throws → rethrows child errors.

#### M1.7: DiscardingTaskGroup + ThrowingDiscardingTaskGroup ✅
**Reference:** Swift DiscardingTaskGroup (TaskGroup.swift:850-1100)
**Effort:** 1.5 days — **DONE**

```cot
struct DiscardingTaskGroup {
    fn addTask(body: fn() -> void) void
    fn cancelAll() void
}

struct ThrowingDiscardingTaskGroup(E) {
    fn addTask(body: fn() -> E!void) void
    fn cancelAll() void
}
```

Fire-and-forget task groups. No `next()` — results discarded. ThrowingDiscardingTaskGroup stores first error.

Swift reference: `TaskGroup.swift` with `IS_DISCARDING=true`. `Builtin.createDiscardingTask` instead of `Builtin.createTask`.

#### M1.8: AsyncThrowingStream ✅
**Reference:** Swift AsyncThrowingStream (AsyncStreamBuffer.swift:282-539)
**Effort:** 2 days — **DONE**

Parallel implementation to AsyncStream with error support:
```cot
struct AsyncThrowingStream(T, E) {
    storage: *AsyncThrowingStreamStorage(T, E),
    fn next() ?(E!T)
}

struct AsyncThrowingStreamContinuation(T, E) {
    fn yield(value: T) YieldResult
    fn finish() void
    fn finishThrowing(err: E) void
}
```

`yield()` same as AsyncStream. `finishThrowing(err)` terminates with error — next consumer gets error. `next()` returns error union.

Swift reference: `AsyncStreamBuffer.swift:282-539` — identical to AsyncStream except terminal state carries error, `next()` throws.

#### M1.9: addTaskUnlessCancelled ✅
**Reference:** Swift TaskGroup.addTaskUnlessCancelled (TaskGroup+addTask.swift.gyb:258-264)
**Effort:** 0.5 days — **DONE**

```cot
fn addTaskUnlessCancelled(body: fn() -> T) bool
```

Returns false if group already cancelled (no task created). Returns true if task was added. Swift: `_taskGroupAddPendingTask(group, unconditionally: false)`.

---

### Milestone 2: AsyncSequence Operators (1.5 weeks) — PARTIAL (M2.1-M2.6 done, M2.7 pending)

Swift provides 14 operator types for composing async sequences. Each is a struct wrapping a base sequence.

#### M2.1: AsyncMapSequence
**Reference:** Swift AsyncMapSequence (AsyncMapSequence.swift)
**Effort:** 1 day

```cot
struct AsyncMapSequence(Base, T) {
    base: Base,
    transform: fn(Base.Element) -> T,
    fn next() ?T  // calls base.next(), applies transform
}
```

Usage: `stream.map(fn(x: int) int { return x * 2 })`

#### M2.2: AsyncFilterSequence
**Reference:** Swift AsyncFilterSequence (AsyncFilterSequence.swift)
**Effort:** 1 day

```cot
struct AsyncFilterSequence(Base) {
    base: Base,
    predicate: fn(Base.Element) -> bool,
    fn next() ?Base.Element  // skips elements that don't match
}
```

#### M2.3: AsyncCompactMapSequence
**Reference:** Swift AsyncCompactMapSequence (AsyncCompactMapSequence.swift)
**Effort:** 0.5 days

Maps and filters nil results. Combines map + filter.

#### M2.4: AsyncFlatMapSequence
**Reference:** Swift AsyncFlatMapSequence (AsyncFlatMapSequence.swift)
**Effort:** 1 day

Maps each element to an async sequence, flattens results.

#### M2.5: AsyncDropFirstSequence + AsyncPrefixSequence
**Reference:** Swift AsyncDropFirstSequence, AsyncPrefixSequence
**Effort:** 1 day

`dropFirst(n)`: skip first n elements. `prefix(n)`: take only first n elements.

#### M2.6: Terminal operators (reduce, contains, first, allSatisfy, min, max)
**Reference:** Swift AsyncSequence default implementations (AsyncSequence.swift)
**Effort:** 2 days

These are methods on AsyncSequence trait with default implementations:
```cot
fn reduce(T)(initial: T, combine: fn(T, Element) -> T) T
fn contains(predicate: fn(Element) -> bool) bool
fn first(predicate: fn(Element) -> bool) ?Element
fn allSatisfy(predicate: fn(Element) -> bool) bool
fn min(compare: fn(Element, Element) -> bool) ?Element
fn max(compare: fn(Element, Element) -> bool) ?Element
```

#### M2.7: Throwing variants of all operators
**Reference:** Swift AsyncThrowingMapSequence, etc.
**Effort:** 2 days

Each operator has a throwing variant that propagates errors from the transform/predicate closure. Same structure, error union returns.

---

### Milestone 3: Time Abstractions (1 week)

#### M3.1: Duration type
**Reference:** Swift Duration (Duration.swift)
**Effort:** 1 day

```cot
struct Duration {
    seconds: i64,
    nanoseconds: i64,
    static fn nanoseconds(ns: i64) Duration
    static fn microseconds(us: i64) Duration
    static fn milliseconds(ms: i64) Duration
    static fn seconds(s: i64) Duration
}
```

Comparable, arithmetic (+, -, <, ==).

#### M3.2: Instant type
**Reference:** Swift Instant concept
**Effort:** 0.5 days

```cot
struct Instant {
    nanoseconds: i64,  // absolute timestamp
    fn advanced(by: Duration) Instant
    fn duration(to: Instant) Duration
}
```

#### M3.3: Clock protocol
**Reference:** Swift Clock protocol (Clock.swift)
**Effort:** 1 day

```cot
trait Clock {
    fn now() Instant
    fn sleep(until: Instant) void
}

struct ContinuousClock { }  // wall clock, doesn't pause on sleep
struct SuspendingClock { }  // pauses during system sleep
```

Phase 1: both clocks use `time()`. Phase 2+: platform-specific clock sources.

#### M3.4: Task.sleep(for:) and Task.sleep(until:)
**Reference:** Swift Task.sleep (TaskSleep.swift)
**Effort:** 1 day

```cot
fn Task_sleep_for(duration: Duration) void
fn Task_sleep_until(deadline: Instant) void
```

Uses Clock protocol internally. Integrates with executor timer queue.

#### M3.5: Task.yield()
**Reference:** Swift Task.yield() (Task.swift:884-890)
**Effort:** 0.5 days

```cot
async fn Task_yield() void
```

Phase 1: no-op (single-threaded, nothing to yield to). Phase 2+: suspends current task, lets executor run others.

Swift reference: `swift_task_yield()` — enqueue current task back on executor, return to executor loop.

#### M3.6: Task.currentPriority
**Reference:** Swift Task.currentPriority (Task.swift:754-760)
**Effort:** 0.5 days

Static property returning current task's priority level.

---

### Milestone 4: Executor Protocol Abstraction (1 week)

#### M4.1: Executor trait
**Reference:** Swift Executor protocol (Executor.swift:17-55)
**Effort:** 1 day

```cot
trait Executor {
    fn enqueue(job: ExecutorJob) void
}
```

#### M4.2: SerialExecutor trait
**Reference:** Swift SerialExecutor (Executor.swift:274-380)
**Effort:** 1 day

```cot
trait SerialExecutor: Executor {
    fn enqueue(job: ExecutorJob) void
    fn isSameExclusiveExecutionContext(other: *Self) bool
}
```

Used by actors for per-actor serial execution guarantee.

#### M4.3: SchedulingExecutor trait
**Reference:** Swift SchedulingExecutor (Executor.swift:58-105)
**Effort:** 1 day

```cot
trait SchedulingExecutor: Executor {
    fn enqueueDelayed(job: ExecutorJob, delay: Duration, clock: *Clock) void
}
```

Extends Executor with timer-based scheduling.

#### M4.4: Custom actor executors
**Reference:** Swift actor unownedExecutor property
**Effort:** 2 days

Allow actors to specify their own SerialExecutor:
```cot
actor MyActor {
    var executor: MySerialExecutor
    nonisolated fn unownedExecutor() *SerialExecutor { return &self.executor }
}
```

#### M4.5: Actor serial queue implementation
**Reference:** Swift DefaultActorImpl (Actor.cpp:1225-1265)
**Effort:** 2 days

Per-actor MPSC queue: LIFO atomic ingress, FIFO priority-bucketed drain. Phase 1: simple FIFO list (single-threaded). Phase 2+: atomic linked list.

---

### Milestone 5: Full Region Analysis (1 week)

#### M5.1: Region analysis SSA pass
**Reference:** Swift RegionAnalysis.cpp
**Effort:** 3 days

Forward dataflow on SSA (infrastructure exists in `region_isolation.zig`):
- IsolationState per value: live, sent, isolated(actor)
- PartitionOp: Send, Require, Merge, AssignDirect
- Fixed-point iteration over CFG blocks
- Detect use-after-send, cross-region transfer violations

#### M5.2: Strict concurrency checking mode
**Reference:** Swift `-strict-concurrency=complete`
**Effort:** 1 day

Compiler flag to enable all Sendable/region warnings as errors.

#### M5.3: Isolated parameter inference
**Reference:** Swift TypeCheckConcurrency.cpp ActorReferenceResult
**Effort:** 1 day

Infer isolation from initialization context. Values created inside actor context automatically isolated.

---

### Milestone 6: Phase 2 Real Suspension (2 weeks)

This is the transition from eager to cooperative execution.

#### M6.1: Global task queue
**Effort:** 2 days

Shared task queue accessible from both compiler-generated code and stdlib executor. Wasm globals for queue pointer and length.

#### M6.2: executeJob calls poll functions
**Effort:** 1 day

Wire `CooperativeExecutor_executeJob` to actually call poll functions via `executor_poll_task`, re-enqueue PENDING tasks.

#### M6.3: Generalize async_split to all async functions
**Effort:** 3 days

Currently gated to `__poll` functions on Wasm. Enable for all async functions on both targets. Fix CLIF codegen for native state machines.

#### M6.4: Await suspends to executor
**Effort:** 2 days

`await` yields current task to executor instead of busy-polling. Executor runs other tasks, resumes when awaited task completes.

#### M6.5: Task.sleep via timer queue
**Effort:** 1 day

`Task.sleep` creates delayed job on executor timer queue. Executor wakes task after deadline.

#### M6.6: TaskGroup parallel execution
**Effort:** 2 days

`addTask` enqueues child on executor. `next()` suspends until child completes. Results in completion order (not submission order).

---

## Timeline

| Milestone | Effort | Cumulative |
|-----------|--------|------------|
| M1: Error handling | 10 days | 2 weeks |
| M2: AsyncSequence operators | 8 days | 3.5 weeks |
| M3: Time abstractions | 5 days | 4.5 weeks |
| M4: Executor protocols | 7 days | 6 weeks |
| M5: Region analysis | 5 days | 7 weeks |
| M6: Real suspension | 11 days | 9 weeks |
| **Total** | **46 days** | **~9 weeks** |

Priority order: M1 (blocking) > M2 (expected) > M3 (expected) > M6 (architecture) > M4 (protocols) > M5 (advanced).

---

## Documented Divergences from Swift

1. **Non-reentrant actors** — Swift: reentrant default. Cot: non-reentrant. Rationale: #1 developer complaint.
2. **Go-style Channel** — Swift: AsyncStream (no backpressure). Cot: bounded channel. Rationale: backpressure prevents data loss.
3. **Manual state machines** — Swift: LLVM coroutines. Cot: SSA-level async_split. Rationale: no LLVM dependency.
4. **No function coloring** — Swift: async in caller. Cot: await blocks in sync. Rationale: simpler (like Zig).
5. **Sendable strict from day one** — Swift: gradual migration. Cot: errors from day one. Rationale: no legacy code.
6. **`param: sending Type` syntax** — Swift: `sending param: Type`. Cot: sending after colon. Rationale: parser ambiguity with type params.

---

## Reference Files

| What | Where |
|------|-------|
| Swift CooperativeExecutor | references/swift/stdlib/public/Concurrency/CooperativeExecutor.swift |
| Swift Task ABI | references/swift/include/swift/ABI/Task.h |
| Swift Actor runtime | references/swift/stdlib/public/Concurrency/Actor.cpp |
| Swift AsyncStream | references/swift/stdlib/public/Concurrency/AsyncStream.swift |
| Swift AsyncStreamBuffer | references/swift/stdlib/public/Concurrency/AsyncStreamBuffer.swift |
| Swift TaskGroup | references/swift/stdlib/public/Concurrency/TaskGroup.swift |
| Swift TaskGroup+addTask | references/swift/stdlib/public/Concurrency/TaskGroup+addTask.swift.gyb |
| Swift TaskLocal | references/swift/stdlib/public/Concurrency/TaskLocal.swift |
| Swift TaskSleep | references/swift/stdlib/public/Concurrency/TaskSleep.swift |
| Swift TaskCancellation | references/swift/stdlib/public/Concurrency/TaskCancellation.swift |
| Swift CheckedContinuation | references/swift/stdlib/public/Concurrency/CheckedContinuation.swift |
| Swift AsyncSequence | references/swift/stdlib/public/Concurrency/AsyncSequence.swift |
| Swift AsyncIteratorProtocol | references/swift/stdlib/public/Concurrency/AsyncIteratorProtocol.swift |
| Swift Executor protocols | references/swift/stdlib/public/Concurrency/Executor.swift |
| Swift Sendable checking | references/swift/lib/Sema/TypeCheckConcurrency.cpp |
| Swift ActorIsolation | references/swift/lib/AST/ActorIsolation.cpp |
| Swift RegionAnalysis | references/swift/lib/SILOptimizer/Analysis/RegionAnalysis.cpp |
| Swift AsyncLet | references/swift/stdlib/public/Concurrency/AsyncLet.cpp |
| Rust coroutine transform | references/rust/ (coroutine.rs) |
| Go channels | Go runtime/chan.go |

---

## Build/Test

```bash
zig build
./zig-out/bin/cot test test/e2e/features.cot              # 370
./zig-out/bin/cot test test/e2e/concurrency.cot            # 21
./zig-out/bin/cot test test/e2e/task_expr.cot              # 6
./zig-out/bin/cot test test/e2e/executor.cot               # 6
./zig-out/bin/cot test test/e2e/task_group.cot             # 10
./zig-out/bin/cot test test/e2e/channel.cot                # 7
./zig-out/bin/cot test test/e2e/continuation.cot           # 6
./zig-out/bin/cot test test/e2e/task_local.cot             # 4
./zig-out/bin/cot test test/e2e/cancellation.cot           # 9
./zig-out/bin/cot test test/e2e/assoc_types.cot            # 3
./zig-out/bin/cot test test/e2e/async_stream.cot           # 9
./zig-out/bin/cot test test/e2e/sending.cot                # 3
./zig-out/bin/cot test test/e2e/throwing_task_group.cot    # 10
./zig-out/bin/cot test test/e2e/discarding_task_group.cot  # 7
./zig-out/bin/cot test test/e2e/async_throwing_stream.cot  # 9
```
