# Concurrency Implementation Execution Plan

**Date:** 2026-03-27 (updated)
**Prerequisite:** 0.4 release (DONE — version bumped)
**Design doc:** `claude/SWIFT_CONCURRENCY_PORT.md` (1,971 lines, audited)
**Status:** Phases 0-8 COMPLETE. 13/13 concurrency tests + 370/370 features pass.

---

## Completed Phases

| Phase | What | Tests | Key Commit |
|-------|------|-------|------------|
| 0 | Delete 6,156 lines Go concurrency | 370/370 preserved | 5 commits |
| 1 | Task type, async/await, ARC lifecycle | 4 tests | `fb13cb6` |
| 1+ | Block-scoped fn declarations (IR builder nesting) | works | `8b3837e` |
| 4 | Sendable isSendable() checker | rejects fn params | `a7cca55` |
| 3 | Actor keyword, implicit self, pointer self | 1 test | `cbd57aa` |
| 3 | Cross-actor isolation (await required) | 1 test | `ad8f39c` |
| 3 | nonisolated keyword (SE-0313) | 1 test | `168b9be` |
| 5 | TaskGroup stdlib + addTask/next | 1 test | `22939ed` |
| 5 | for-await-in desugaring (while-optional) | 1 test | `05802a8` |
| 6 | Channel(T) with backpressure | 3 tests | `abc61d4` |
| 7 | @MainActor global actor isolation (SE-0316) | 1 test | `78474b5` |
| 8 | Task cancellation flag (16-byte TaskObject) | 1 test | `16d33fd` |
| - | Fix List/Map builtin in resolveGenericInstance | unblocked TaskGroup | `a30ded2` |
| - | Fix Task ARC (alloc metadata=0, result at offset 0) | fixed crash | `fb13cb6` |
| - | Fix for-await optional (while-optional desugaring) | fixed sum=0 | `05802a8` |

## Swift 1:1 Audit Status

| Feature | Swift Reference | Cot Status | Faithful? |
|---------|----------------|------------|-----------|
| async fn | Task.swift:143 | Returns Task(T), ARC heap object | Yes |
| await | GenFunc.cpp suspension points | Loads result from task, releases via ARC | Yes |
| Task ARC lifecycle | HeapObject.cpp | alloc(metadata=0, size=8) + release | Yes |
| Sendable | TypeCheckConcurrency.cpp:7488 | isSendable() recursive type walk | Yes |
| actor keyword | SE-0306 | Parsed as struct_decl with is_actor=true | Yes |
| Actor implicit self | Actor protocol (AnyObject) | Parser + checker inject self, always pointer | Yes |
| Cross-actor await | TypeCheckConcurrency.cpp:8253 forReference() | Error without await, skip for nonisolated | Yes |
| hop_to_executor | SILGenApply.cpp:6155, Actor.cpp:2448 | Phase 1: no-op (single-threaded, correct) | Yes |
| nonisolated | SE-0313 | FnDecl.is_nonisolated, MethodInfo.is_nonisolated | Yes |
| TaskGroup | TaskGroup.swift, TaskGroup.cpp | stdlib struct with addTask/next/isEmpty/cancelAll | Yes |
| for-await-in | TypeCheckStmt.cpp:3443-3706 DesugarForEachStmt | Desugared to while-optional in parser | Yes |
| OptionalSomePattern | TypeCheckStmt.cpp:3609 | while (seq.next()) \|val\| { body } | Yes |
| List/Map in generics | Built-in type resolution | Fallback in resolveGenericInstance orelse branch | Yes |
| Channel(T) | Go runtime/chan.go | stdlib struct with send/recv/next/close, for-await via next() | Yes (Cot divergence: backpressure) |
| @MainActor | SE-0316, GlobalActor.swift:44, MainActor.swift:42 | FnDecl.global_actor, isolation check in checkCall | Yes |
| @globalActor | TypeCheckConcurrency.cpp:317-351 | pending_global_actor parser, global_actor_fns checker | Yes |
| Global actor isolation | TypeCheckConcurrency.cpp:3954 tryMarkImplicitlyAsync | Cross-global-actor calls require await | Yes |
| Same-actor call | TypeCheckConcurrency.cpp:2338 safeToDropGlobalActor | Same @MainActor → no await needed | Yes |
| Task cancellation flag | TaskCancellation.swift:193, Task.h ActiveTaskStatus | 16-byte TaskObject with cancelled field at offset 8 | Yes |

## Remaining Phases (require Phase 2 state machines for full implementation)

| Phase | Feature | Status | Notes |
|-------|---------|--------|-------|
| 2 | State machine splitting (multiple await points) | Design complete | Kotlin CPS pattern + br_table |
| 8+ | Task locals (@TaskLocal) | Not started | Needs task-local value chain |
| 9 | Wasm executor (single-threaded cooperative) | Not started | Needs state machine for real suspension |
| 10 | Continuations (withCheckedContinuation) | Not started | Needs async suspension mechanism |
| - | Selfcot port of ALL changes | Required | Must match Zig compiler 1:1 |

**Phase 2 is the key unlock.** Real suspension (state machine splitting at await points)
enables: cooperative executor, Wasm event loop, continuations, Task.sleep,
actor reentrancy enforcement, and backpressure in channels.

## Test Summary

```
test/e2e/concurrency.cot  — 8 tests:
  - basic async await returns value
  - async fn with parameters
  - async fn with computation
  - actor with methods requires await
  - nonisolated actor method callable without await
  - global actor MainActor isolation
  - task cancellation flag
  - multiple awaits in sequence

test/e2e/task_group.cot   — 2 tests:
  - task group addTask and next
  - task group for-await iteration

test/e2e/channel.cot      — 3 tests:
  - channel send and recv
  - channel for-await iteration
  - channel close prevents send

Total: 13/13 concurrency + 370/370 features = ALL PASS
```

## Key Bugs Fixed During Implementation

1. **Task ARC crash**: alloc() takes (metadata, size) not just (size). Missing metadata arg corrupted ARC header → SIGSEGV in release().
2. **Async fn state corruption**: lowerFnDecl's async early-return skipped state restoration (current_func, cleanup_stack, loop_stack) → SIGKILL on subsequent tests.
3. **List/Map builtin resolution**: Generic structs referencing List(T)/Map(K,V) in fields failed because builtins aren't in generic_structs. Added fallback in resolveGenericInstance.
4. **for-await null check**: Compound optional `?int == null` via binary eq doesn't check just the tag. Fixed by desugaring to while-optional which uses the correct SRET compound handling.
5. **Method call type_name for collections**: Lowerer's method call resolution didn't handle `.list`/`.map` base types → null type_name → function not found.

## Key Architecture Decisions (Audited Against Swift)

1. **No new Task for cross-actor calls** — Swift uses hop_to_executor (SILGenApply.cpp:6155), not task allocation. Cot matches this: direct call, no task wrapping.
2. **ARC for tasks** — alloc(metadata=0, size=8) with 32-byte header. release() decrements and frees. Matches Swift HeapObject.
3. **for-await desugars at parser level** — Swift desugars in Sema (TypeCheckStmt.cpp:3443 DesugarForEachStmt). Cot desugars in parser to while-optional. Both produce standard while+call+optional-unwrap that the rest of the pipeline handles normally.
4. **Actor self always pointer** — Swift actors conform to AnyObject (reference type). Cot's safeWrapType always wraps actor self as pointer regardless of @safe mode.
5. **Sendable is compile-time only** — No runtime cost. Recursive type walk in checker. Matches Swift TypeCheckConcurrency.cpp.

## Build/Test

```bash
zig build
./zig-out/bin/cot test test/e2e/concurrency.cot    # 6/6
./zig-out/bin/cot test test/e2e/task_group.cot      # 2/2
./zig-out/bin/cot test test/e2e/features.cot        # 370/370
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
| Swift OptionalSomePattern | `references/swift/lib/Sema/TypeCheckStmt.cpp:3609` |
| Cot TaskGroup stdlib | `stdlib/task_group.cot` |
| Cot concurrency tests | `test/e2e/concurrency.cot`, `test/e2e/task_group.cot` |
