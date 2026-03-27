# Concurrency Implementation Execution Plan

**Date:** 2026-03-27 (updated — selfcot port complete)
**Design doc:** `claude/SWIFT_CONCURRENCY_PORT.md` (1,971 lines, audited)
**Status:** Phases 0-10 ALL IMPLEMENTED in Zig compiler + selfcot. 27/27 concurrency tests + 370/370 features pass.

---

## Completed Phases

| Phase | What | Tests | Key Commit |
|-------|------|-------|------------|
| 0 | Delete 6,156 lines Go concurrency | 370/370 preserved | 5 commits |
| 1 | Task type, async/await, ARC lifecycle | 4 tests | `fb13cb6` |
| 1+ | Block-scoped fn declarations (IR builder nesting) | works | `8b3837e` |
| 2 | async_split.zig SSA pass (683 lines, analysis + transform) | analysis runs (gated) | `7b54e95` |
| 3 | Actor keyword, implicit self, cross-actor isolation, nonisolated (SE-0306, SE-0313) | 3 tests | `cbd57aa` `ad8f39c` `168b9be` |
| 4 | Sendable isSendable() checker (TypeCheckConcurrency.cpp:7488) | rejects fn params | `a7cca55` |
| 5 | TaskGroup stdlib + for-await-in desugaring | 2 tests | `22939ed` `05802a8` |
| 6 | Channel(T) with backpressure (Go-style) | 3 tests | `abc61d4` |
| 7 | @MainActor global actor isolation (SE-0316) | 1 test | `78474b5` |
| 8 | Task cancellation flag (16-byte TaskObject) | 1 test | `16d33fd` |
| 9 | Executor (cooperative, SE-0392) | 1 test | `5d3ac28` |
| 10 | Continuation(T) (CheckedContinuation, SE-0300) | 2 tests | `44d31fd` |
| - | Selfcot port: parser+checker+types+IR+lowerer (677 lines) | all pass | `af9697e` |
| - | 8 additional concurrency tests | 27 total | `63e69b5` |
| - | Fix @attr parsing regression (was intercepting @assertEq) | fixed | `a504640` |

## Selfcot Port Status — COMPLETE

All concurrency features ported to `self/` (1:1 with Zig compiler):

| Component | File | What |
|-----------|------|------|
| Parser | `self/parse/parser.cot` | parseActorDecl, parseNonisolatedFn, parseAttrDecl (@MainActor/@inlinable/@globalActor), for-await-in desugaring, block-scoped concurrency decls, current_impl_is_actor, pending_global_actor |
| Checker | `self/check/checker.cot` | actor_types Map, current_actor_type, current_global_actor, in_await, global_actor_fns Map, checkAwaitExpr, isCrossActorCallExpr, isGlobalActorCallExpr, getCallReceiverActorName, isSendable, cross-actor enforcement in checkCall, global-actor enforcement, Task(T) return wrapping |
| Types | `self/check/types.cot` | TaskType struct, Type.task variant, TAG_TASK, makeTask, MethodInfo.is_nonisolated, task in sizeOf/alignOf/isTrivial/equal/typeToString |
| IR | `self/build/ir.cot` | FuncBuilder.async_task_local, FuncBuilder.async_result_type, Builder.saved_funcs stack (push on startFunc, pop on endFunc) |
| Lowerer | `self/build/lower.cot` | lowerAsyncFnEager (alloc TaskObject, set async fields, lower body), lowerAwaitExpr (cross-actor/global-actor passthrough or load+release), isGlobalActorCall, isCrossActorCall, async return path in lowerReturn (store at task_ptr[0]) |
| Tokens | `self/parse/token.cot` | kw_actor, kw_nonisolated (added in prior session) |
| AST | `self/parse/ast.cot` | FnDecl.is_nonisolated/is_inlinable/global_actor, StructDecl.is_actor (added in prior session) |

## Swift 1:1 Audit Status

| Feature | Swift Reference | Cot Status | Faithful? |
|---------|----------------|------------|-----------|
| async fn | Task.swift:143 | Returns Task(T), ARC heap object | Yes |
| await | GenFunc.cpp suspension points | Loads result from task, releases via ARC | Yes |
| Task ARC lifecycle | HeapObject.cpp | alloc(metadata=0, size=16) + release | Yes |
| Sendable | TypeCheckConcurrency.cpp:7488 | isSendable() recursive type walk | Yes |
| actor keyword | SE-0306 | Parsed as struct_decl with is_actor=true | Yes |
| Actor implicit self | Actor protocol (AnyObject) | Parser + checker inject self, always pointer | Yes |
| Cross-actor await | TypeCheckConcurrency.cpp:8253 forReference() | Error without await, skip for nonisolated | Yes |
| hop_to_executor | SILGenApply.cpp:6155, Actor.cpp:2448 | Phase 1: no-op (single-threaded, correct) | Yes |
| nonisolated | SE-0313 | FnDecl.is_nonisolated, MethodInfo.is_nonisolated | Yes |
| TaskGroup | TaskGroup.swift, TaskGroup.cpp | stdlib struct with addTask/next/isEmpty/cancelAll | Yes |
| for-await-in | TypeCheckStmt.cpp:3443-3706 DesugarForEachStmt | Desugared to while-optional in parser | Yes |
| OptionalSomePattern | TypeCheckStmt.cpp:3609 | while (seq.next()) \|val\| { body } | Yes |
| Channel(T) | Go runtime/chan.go | stdlib struct with send/recv/next/close | Yes (documented divergence: backpressure) |
| @MainActor | SE-0316, GlobalActor.swift:44 | FnDecl.global_actor, isolation check in checkCall | Yes |
| @globalActor | TypeCheckConcurrency.cpp:317-351 | pending_global_actor parser, global_actor_fns checker | Yes |
| Same-actor call | TypeCheckConcurrency.cpp:2338 | Same @MainActor → no await needed | Yes |
| Task cancellation | TaskCancellation.swift:193, Task.h | 16-byte TaskObject with cancelled at offset 8 | Yes |
| Executor | CooperativeExecutor.swift:291-336 | stdlib with enqueue/run/stop | Yes (Phase 1 stub) |
| Continuation | CheckedContinuation.swift, SE-0300 | stdlib with resume/getResult, double-resume panic | Yes |

## Phase 2: State Machine Splitting — INFRASTRUCTURE COMPLETE

The SSA pass `async_split.zig` (683 lines) is fully implemented:
- **Analysis**: identifies suspension points (calls returning Task type) ✓
- **Local liveness**: tracks locals live across suspension boundaries ✓
- **State struct layout**: computes frame size with spill slots ✓
- **Block splitting**: splits at await into pre-await + resume blocks ✓
- **Dispatch entry**: jump_table block routing to resume blocks ✓
- **Spill/restore**: state stores before PENDING return ✓
- **Value rewriting**: constants re-emitted, args re-emitted, local_addr re-emitted, memory state skipped ✓

**Gated because:** Eager evaluation produces correct results for all current tests.
The state machine is only needed for REAL SUSPENSION (yielding to executor).

**To enable:**
1. Constructor/poll function split in `lowerAsyncFnEager`
2. Poll loop in `lowerAwaitExpr` (call fn_poll until READY)
3. Remove the `if (true or ...)` gate in `async_split.zig`
4. Test with `combined()` (2 suspend points)

## Remaining Work

| Feature | Status | Notes |
|---------|--------|-------|
| Enable state machine transform | Gated | Need constructor/poll split in lowerer |
| @TaskLocal (SE-0311) | Not started | Needs @attribute parser syntax + $var.withValue scoping |
| @Sendable closures (SE-0302) | Not started | Closure capture checking |
| @unchecked Sendable | Not started | Escape hatch for unverifiable Sendable |
| TaskGroup.addTask closure API | Not started | Currently takes pre-computed value, should take fn() -> T |

## Test Summary (27 tests)

```
test/e2e/concurrency.cot  — 15 tests:
  - basic async await returns value
  - async fn with parameters
  - async fn with computation
  - actor with methods requires await
  - nonisolated actor method callable without await
  - global actor MainActor isolation
  - task cancellation flag
  - async fn returning string
  - nested async calls
  - actor field access requires await
  - multiple awaits in sequence
  - async fn returning zero
  - actor with multiple fields
  - async fn chain of three
  - async fn with loop

test/e2e/task_group.cot   — 5 tests:
  - task group addTask and next
  - task group for-await iteration
  - empty task group next returns null
  - task group single element
  - for-await over empty group does nothing

test/e2e/channel.cot      — 5 tests:
  - channel send and recv
  - channel for-await iteration
  - channel close prevents send
  - channel recv from empty returns null
  - channel multiple send recv cycles

test/e2e/continuation.cot — 2 tests:
  - continuation resume and get result
  - continuation tracks resumed state

test/e2e/executor.cot     — 1 test:
  - executor enqueue and run (Phase 1 stub — no actual scheduling)

Total: 27/27 concurrency + 370/370 features = ALL PASS
```

## Key Bugs Fixed During Implementation

1. **Task ARC crash**: alloc() takes (metadata, size) not just (size). Missing metadata arg corrupted ARC header → SIGSEGV in release().
2. **Async fn state corruption**: lowerFnDecl's async early-return skipped state restoration → SIGKILL on subsequent tests.
3. **List/Map builtin resolution**: Generic structs referencing List(T)/Map(K,V) failed. Added fallback in resolveGenericInstance.
4. **for-await null check**: Desugared to while-optional which uses correct SRET compound handling.
5. **Method call type_name for collections**: Added `.list => "List"`, `.map => "Map"` to type_name switch.
6. **SSA EntryLivein errors**: Aggressive value rewriting — re-emit constants, args, local_addrs; skip memory state.
7. **@attr parsing regression**: parseStmt was intercepting `@assertEq` as attribute. Fixed to only match MainActor/globalActor/inlinable.

## Build/Test

```bash
zig build
./zig-out/bin/cot test test/e2e/concurrency.cot    # 15/15
./zig-out/bin/cot test test/e2e/task_group.cot     # 5/5
./zig-out/bin/cot test test/e2e/channel.cot        # 5/5
./zig-out/bin/cot test test/e2e/continuation.cot   # 2/2
./zig-out/bin/cot test test/e2e/executor.cot       # 1/1
./zig-out/bin/cot test test/e2e/features.cot       # 370/370
COT_DEBUG=async_split ./zig-out/bin/cot test test/e2e/concurrency.cot  # SSA analysis

# Selfcot
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot
/tmp/selfcot version  # cot 0.4.0 (self-hosted)
```

## Reference Files

| What | Where |
|------|-------|
| Swift Task ABI | `references/swift/include/swift/ABI/Task.h` |
| Swift Actor ABI | `references/swift/include/swift/ABI/Actor.h` |
| Swift for-await desugaring | `references/swift/lib/Sema/TypeCheckStmt.cpp:3443` |
| Swift Sendable checking | `references/swift/lib/Sema/TypeCheckConcurrency.cpp:7488` |
| Swift actor isolation | `references/swift/lib/Sema/TypeCheckConcurrency.cpp:8253` |
| Swift hop_to_executor | `references/swift/lib/SILGen/SILGenApply.cpp:6155` |
| Swift TaskGroup | `references/swift/stdlib/public/Concurrency/TaskGroup.swift` |
| Swift CheckedContinuation | `references/swift/stdlib/public/Concurrency/CheckedContinuation.swift` |
| Swift CooperativeExecutor | `references/swift/stdlib/public/Concurrency/CooperativeExecutor.swift` |
| Swift TaskLocal | `references/swift/stdlib/public/Concurrency/TaskLocal.swift` |
| Rust coroutine transform | `references/rust/` (coroutine.rs) |
| Cot concurrency stdlib | `stdlib/task_group.cot`, `stdlib/channel.cot`, `stdlib/continuation.cot`, `stdlib/executor.cot` |
| Cot concurrency tests | `test/e2e/concurrency.cot`, `test/e2e/task_group.cot`, `test/e2e/channel.cot`, `test/e2e/continuation.cot`, `test/e2e/executor.cot` |
| async_split SSA pass | `compiler/ssa/passes/async_split.zig` (683 lines) |
| Pipeline debug | `compiler/pipeline_debug.zig` (`.async_split` phase) |
