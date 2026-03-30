# Concurrency — Status, Architecture, and Gap 9 Plan

**Date:** 2026-03-28
**Tests:** 618, all green (native + Wasm)
**Parity:** 46/50 Swift features at 1:1. 3 Phase 1 (simplified internals). 1 deliberate divergence (Channel).
**Remaining:** Gap 9 (native state machines) — deferred to post-restructuring.

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

### Why It's Deferred to Post-Restructuring

The `src/` restructuring introduces three independent libraries with C ABI boundaries:

```
libcot (frontend) → CIR → libcir (IR + passes) → libclif (native backend)
```

Gap 9 should be implemented within this architecture for three reasons:

**1. CIR has explicit async primitives.**
`cir.h` defines `cir_build_await`, `cir_build_async_suspend`, `cir_build_async_resume`, `cir_build_task_create`. Native async dispatch should be expressed as CIR operations, not as ad-hoc SSA pass transformations. The current `async_split.zig` generates raw SSA ops (`.arg`, `.off_ptr`, `.load`, `.eq`, `.if_`) that must be handled by each backend — CIR abstracts this properly.

**2. libclif enables backend swapping.**
The `clif.h` contract allows multiple native backends:
- `libclif-zig` (current Cranelift port)
- `libclif-rs` (Rust Cranelift crate — direct access to `Switch::emit()` which handles coroutine dispatch natively)
- `libllvm-c` (LLVM backend)

The native state machine problem (Cot's CLIF port can't handle dispatch across blocks) is trivially solved by `libclif-rs` — real Cranelift's `Switch::emit()` generates correct binary search trees and `br_table` sequences. No need to debug SSA dominance issues in the Zig port.

**3. The restructuring changes where async_split lives.**
Currently `compiler/ssa/passes/async_split.zig`. In the new structure, the Wasm state machine belongs in `libcir` (since it transforms CIR), while native dispatch belongs in `libclif` (since it's target-specific). Implementing Gap 9 now would put the code in the wrong place.

### Root Cause Analysis (from deep audit)

The native dispatch failure was traced through three reference implementations:

**Rust (coroutine.rs → rustc_codegen_cranelift → Cranelift):**
- MIR `SwitchInt` on discriminant → `cranelift_frontend::Switch::emit()` → binary search tree + `br_table`
- The discriminant is loaded ONCE as a scalar Value, compared inline — no block parameters for the discriminant
- Cranelift's SSA builder (`def_var`/`use_var`) handles cross-block value flow automatically

**Kotlin (AbstractSuspendFunctionsLowering → StateMachineBuilder):**
- Suspension points → state graph with DFS topological sort → `when(this.label)` dispatch
- Each local live across suspension points → coroutine class field (spilled/restored)
- No LLVM coroutines — pure IR-level state machine (exactly like Cot needs)
- `$completion: Continuation<T>` parameter added to every suspend function
- `COROUTINE_SUSPENDED` sentinel value returned when task suspends

**Cot's specific blocker (async_split.zig → ssa_to_clif.zig):**
- async_split creates NEW if/else check blocks with `.arg` ops in each
- `.arg` handler in `ssa_to_clif.zig` looks up entry block params by index
- After async_split changes `f.entry` to the first check block, the entry HAS the params
- BUT: subsequent check blocks' `.arg` ops also reference entry params — these values cross block boundaries without phi nodes
- This is an SSA dominance issue: the CLIF builder rejects values from non-dominating blocks
- Fix requires either: (a) phi-based frame pointer flow, (b) Cranelift Variable system for cross-block values, or (c) use real Cranelift via `libclif-rs`

### Implementation Plan (for when src/ is ready)

**Phase 1: CIR async primitives (libcir)**
1. `cir_build_async_suspend(state_index)` — save state discriminant + spill locals to frame, return PENDING
2. `cir_build_async_resume(state_index)` — dispatch entry: load discriminant, branch to resume point, restore locals
3. `cir_build_await(type, future)` — poll future, if PENDING suspend, if READY extract result
4. Wasm emit: translate to current constructor/poll + async_split pattern (already working)
5. Native emit: translate to Cranelift Switch dispatch (via libclif)

**Phase 2: libclif native dispatch**
Option A (recommended): Use `libclif-rs` (Rust Cranelift crate). `Switch::emit()` handles coroutine dispatch out of the box. Zero custom code needed.
Option B: Fix `libclif-zig`. Implement Cranelift Variable system for frame pointer flow across dispatch blocks. Port `Switch::emit()` binary search tree + `br_table` from `wasmtime/cranelift/frontend/src/switch.rs`.
Option C: Use LLVM via `libllvm-c` for optimized builds. LLVM has native coroutine support.

**Phase 3: Native cooperative scheduling**
1. Native await site calls poll function by name (direct call, not call_indirect)
2. Between polls, process other tasks from global queue (same as Wasm cooperative path)
3. Reuse `run_until_task_done` pattern — compile executor.cot for native target

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
