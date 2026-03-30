# Concurrency — Status, Architecture, and Gap 9 Plan

**Date:** 2026-03-30 (updated)
**Tests:** 618, all green (native + Wasm)
**Parity:** 46/50 Swift features at 1:1. 3 Phase 1 (simplified internals). 1 deliberate divergence (Channel).
**Remaining:** Gap 9 (native state machines) — **UNBLOCKED** by real Cranelift backend (rust/libclif).

---

## What's Done

All concurrency features are implemented and tested across M1-M6 milestones plus Gap closures 1-8. The implementation faithfully ports Swift's concurrency model using Rust's poll-based state machine pattern (no LLVM coroutines).

### Feature Summary (50 features)

| Category | Features | Parity |
|----------|----------|--------|
| Cooperative executor | PriorityQueue, TaskPriority, CooperativeExecutor 3-phase run loop, dual timer queues, snapshot drain | 1:1 Swift |
| Executor protocols | Executor, SerialExecutor, SchedulingExecutor, RunLoopExecutor | 1:1 Swift |
| Task lifecycle | Task create/cancel/isCancelled/checkCancellation, withTaskCancellationHandler | 1:1 Swift |
| Task scheduling | yield, sleep(nanoseconds/for/until), currentPriority | Phase 1 |
| Task groups | TaskGroup, ThrowingTaskGroup, DiscardingTaskGroup, ThrowingDiscardingTaskGroup, withTaskGroup, withThrowingTaskGroup, addTaskUnlessCancelled | 1:1 Swift |
| Streams | AsyncStream (3 buffering policies), AsyncThrowingStream | 1:1 Swift |
| Sequences | 13 terminal ops + 8 lazy wrappers (map, filter, compactMap, dropFirst, prefix, dropWhile, prefixWhile, flatMap) | 1:1 Swift |
| Continuations | Continuation(T), ThrowingContinuation(T) | 1:1 Swift |
| TaskLocal | get, withValue, LIFO stack, child inheritance | 1:1 Swift |
| Time | Duration, Instant, ContinuousClock, SuspendingClock | 1:1 Swift |
| Actors | actor keyword, nonisolated, @MainActor, ActorExecutor (per-actor serial queue) | 1:1 Swift |
| Sendable | @Sendable closures, @unchecked Sendable, sending parameter (SE-0430) | 1:1 Swift |
| Region analysis | Partition (element→region map), PartitionOp algebra, fixed-point dataflow | 1:1 Swift RegionAnalysis.cpp |
| Structured concurrency | async let, Task {}, Task.detached {} | 1:1 Swift |
| Async closures | `async fn() -> T` closure type, parser + checker + lowerer | 1:1 Swift |
| Cooperative await (Wasm) | Await sites call run_until_task_done, auto-import executor | 1:1 Rust block_on |
| Channel | Bounded channel with backpressure | Deliberate divergence (Go-style) |
| Associated types | Type params in trait definitions | 1:1 Swift |
| Strict concurrency | --strict-concurrency CLI flag | 1:1 Swift |

### Implementation Files

**Stdlib (19 concurrency files):**
priority_queue.cot, task_queue.cot, executor.cot, executor_traits.cot, cooperative.cot, async_sequences.cot, actor_executor.cot, task_group.cot, throwing_task_group.cot, discarding_task_group.cot, async_stream.cot, async_throwing_stream.cot, async_ops.cot, cancellation.cot, continuation.cot, channel.cot, task_local.cot, duration.cot, instant.cot, clock.cot, time.cot

**Compiler (8 concurrency-specific files):**
checker.zig (actor isolation, Sendable, sending, async closure types), lower.zig (async fn lowering, cooperative await, constructor/poll split, async closures), parser.zig (async fn() closure syntax), ast.zig (is_async on ClosureExpr, hasAsyncFunctions), async_split.zig (SSA state machine transform), region_isolation.zig (Partition dataflow), driver.zig (auto-import executor for async files), executor_runtime.zig (Wasm runtime poll bridge)

**Tests (24 files, 618 tests):**
See `claude/CONCURRENCY_SWIFT_PORT.md` for the complete test inventory.

---

## What Remains: Gap 9 — Native State Machines

### Problem

Native async functions use eager evaluation. The constructor/poll split and async_split state machine only work on Wasm. On native, async function bodies run inline — no suspension, no cooperative scheduling.

### Why This Is Now Unblocked (v0.4.1)

The blocker was: the hand-ported Cranelift backend (`zig/libclif`) couldn't handle `Switch::emit()` or cross-block SSA from `async_split`'s dispatch blocks.

**Real Cranelift (`rust/libclif`) solves this.** The `Variable` system in `cranelift_frontend` automatically handles values that cross block boundaries. `Switch::emit()` generates efficient `br_table` or binary search dispatch. This is the exact code path that Rust's `cg_clif` uses for async.

The async_split pass produces a normal function with a switch on `self.state`. Real Cranelift compiles it like any other function. No LLVM coroutines needed.

### Implementation Plan

See `claude/RELEASE_0_4_1.md` Step 3 for the detailed phased plan.

**Phase 9.1: Verify async_split output compiles through CIR** (2 days)
- Enable async_split for native target (currently Wasm-only gate in driver.zig)
- The state machine is a normal function with switch dispatch → CIR handles `OP_BR_TABLE`
- Test with `test/e2e/cooperative_await.cot`

**Phase 9.2: Frame allocation and local save/restore** (3 days)
- Heap-allocated frame: state discriminant + saved locals + return value
- Constructor allocates frame, stores args, returns frame pointer as task
- Poll receives frame pointer and dispatches
- `async_split.zig` already does this for Wasm — replicate for native

**Phase 9.3: Cooperative scheduling on native** (2 days)
- `lower.zig`: generate `run_until_task_done` calls at native await sites
- `driver.zig`: auto-import `std/executor` + `std/task_queue` for async files on native
- Executor stdlib code is target-independent — already passes tests

**Phase 9.4: Test parity** (2 days)
- All 24 concurrency test files (618 tests) must pass on native

### Future: LLVM Backend (libllvm)

When an LLVM backend is added (`--backend=llvm`), it can use LLVM's native coroutine intrinsics (`llvm.coro.begin/suspend/end`) for the state machine transform instead of doing it at the SSA level. This is how Swift works. The CIR async operations would map directly to LLVM coroutine calls.

### Swift/Rust/Kotlin References

| What | Where |
|------|-------|
| Rust coroutine transform | `references/rust/compiler/rustc_mir_transform/src/coroutine.rs` |
| Rust Cranelift SwitchInt | `references/rust/compiler/rustc_codegen_cranelift/src/base.rs:451-495` |
| Cranelift Switch::emit | `references/wasmtime/cranelift/frontend/src/switch.rs:267-278` |
| Cranelift br_table ARM64 | `references/wasmtime/cranelift/codegen/src/isa/aarch64/inst/emit.rs:3156-3238` |
| Kotlin suspend lowering | `references/kotlin/compiler/ir/backend.common/src/.../AbstractSuspendFunctionsLowering.kt` |
| Kotlin JS state machine | `references/kotlin/compiler/ir/backend.js/src/.../StateMachineBuilder.kt` |
| Kotlin coroutine runtime | `references/kotlin/kotlin-native/runtime/src/.../ContinuationImpl.kt` |
| Swift CooperativeExecutor | `references/swift/stdlib/public/Concurrency/CooperativeExecutor.swift` |
| Swift Task ABI | `references/swift/include/swift/ABI/Task.h` |
| Swift RegionAnalysis | `references/swift/lib/SILOptimizer/Analysis/RegionAnalysis.cpp` |

### Deliberate Divergences from Swift

| # | Divergence | Swift | Cot | Rationale |
|---|-----------|-------|-----|-----------|
| 1 | Non-reentrant actors | Reentrant by default | Non-reentrant | #1 developer complaint about Swift actors |
| 2 | Go-style Channel | AsyncStream (no backpressure) | Bounded channel with send/recv | Backpressure prevents data loss |
| 3 | Manual state machines | LLVM coroutines | SSA-level async_split (Rust pattern) | No LLVM dependency |
| 4 | No function coloring | `async` in signature | `await` blocks in sync context | Simpler (like Zig) |
| 5 | Strict Sendable from day one | Gradual migration | Compile errors from day one | No legacy code to migrate |
| 6 | `param: sending Type` syntax | `sending param: Type` | Sending after colon | Parser ambiguity with type params |

---

## Known Compiler Bugs (concurrency-related)

1. **Native function ordering fragility** — adding async functions to a test file can cause unrelated tests to fail. Pre-existing native codegen issue (function count affects correctness). Workaround: put async tests in separate files.
2. **Two globals of same generic type on Wasm** — `var a: List(T); var b: List(T)` share one Wasm global slot. Workaround: heap-allocated singleton.
3. **Checker sent_variables leaks across test blocks** — variables named identically in different test blocks share the sent state. Workaround: use unique variable names across tests in the same file.
