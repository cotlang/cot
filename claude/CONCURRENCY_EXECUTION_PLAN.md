# Concurrency Implementation Execution Plan

**Date:** 2026-03-27
**Design doc:** `claude/SWIFT_CONCURRENCY_PORT.md` (1,971 lines, audited)
**Status:** Core concurrency complete. 406 tests pass native. Wasm has 6 known failures.

---

## Current Test Status

### Native — ALL PASS

| Suite | Tests | Status |
|-------|-------|--------|
| features.cot | 370 | 370/370 |
| concurrency.cot | 18 | 18/18 |
| task_group.cot | 7 | 7/7 |
| channel.cot | 5 | 5/5 |
| continuation.cot | 2 | 2/2 |
| executor.cot | 1 | 1/1 |
| task_local.cot | 3 | 3/3 |
| **Total** | **406** | **406/406** |

### Wasm — 6 FAILURES (tech debt)

| Failure | Root Cause | Fix Required |
|---------|-----------|--------------|
| task_group for-await iteration (expected 0, got 15) | Wasm test runner reads error union tag incorrectly — test computes correct value 15 but runner misinterprets return | Fix `generateTestRunner` error union handling for Wasm: the `eu_ptr` load reads from wrong offset when function has for-await loops with accumulator variables |
| task_group closure captures (expected 0, got 306) | Same root cause — correct value 306, wrong test runner interpretation | Same fix as above |
| channel for-await iteration (expected 0, got 60) | Same root cause — correct value 60 | Same fix |
| features.cot compilation error | `values remaining on stack at end of block` — the Wasm store codegen fix (off_ptr/local_addr setReg) doesn't cover all VOID-typed ops that produce stack values. Some op in the 370-test file still leaves values on the Wasm stack. | Audit ALL ops with VOID type that call `ssaGenValueOnStack` — any that push to the Wasm stack need `setReg`. Likely candidates: `global_addr`, `ptr_add`, `elem_ptr`. |
| test/cases/arrays.cot Wasm | Compilation error — same stack-remaining class | Same fix as features.cot |
| test/cases/loops.cot Wasm | Compilation error — same class | Same fix |
| test/cases/strings.cot Wasm | Compilation error — same class | Same fix |

**Summary:** 3 failures are a single Wasm test runner bug (error union return interpretation). 4 failures are a single Wasm codegen bug (VOID-typed ops missing setReg). Two fixes would resolve all 7.

---

## Completed Phases

| Phase | What | Tests | Key Commit |
|-------|------|-------|------------|
| 0 | Delete 6,156 lines Go concurrency | 370/370 preserved | 5 commits |
| 1 | Task type, async/await, ARC lifecycle | 4 tests | `fb13cb6` |
| 1+ | Block-scoped fn declarations (IR builder nesting) | works | `8b3837e` |
| 2 | async_split.zig SSA pass (683 lines) + constructor/poll split | gated (Wasm dispatch bug) | `7b54e95` `250aeef` `7755583` |
| 3 | Actor keyword, implicit self, cross-actor isolation, nonisolated (SE-0306, SE-0313) | 3 tests | `cbd57aa` `ad8f39c` `168b9be` |
| 4 | Sendable isSendable() + @Sendable closures (SE-0302) + @unchecked Sendable | capture checking works | `a7cca55` `da70878` `a9808ef` |
| 5 | TaskGroup stdlib + closure API + for-await-in desugaring (SE-0304) | 7 tests | `22939ed` `29e2553` |
| 6 | Channel(T) with backpressure (Go-style) | 5 tests | `abc61d4` |
| 7 | @MainActor global actor isolation (SE-0316) | 1 test | `78474b5` |
| 8 | Task cancellation flag (16-byte TaskObject) | 1 test | `16d33fd` |
| 9 | Executor (cooperative, SE-0392) | 1 test | `5d3ac28` |
| 10 | Continuation(T) (CheckedContinuation, SE-0300) | 2 tests | `44d31fd` |
| 11 | TaskLocal(T) (SE-0311) | 3 tests | `5e6cf8d` |
| - | Selfcot port: parser+checker+types+IR+lowerer (677 lines) | all pass | `af9697e` |
| - | Fix Wasm store codegen after memory threading | 18/18 wasm concurrency | `08050b1` |

## Selfcot Port Status

**Phases 0-10 ported.** Phases 11 (@Sendable, @unchecked Sendable, TaskLocal, constructor/poll) NOT yet ported to self/.

| Component | File | Ported? |
|-----------|------|---------|
| Parser | `self/parse/parser.cot` | Phases 0-10 ✓, @Sendable/constructor NOT ported |
| Checker | `self/check/checker.cot` | Phases 0-10 ✓, @Sendable capture check NOT ported |
| Types | `self/check/types.cot` | Phases 0-10 ✓ |
| IR | `self/build/ir.cot` | Phases 0-10 ✓, is_async_poll NOT ported |
| Lowerer | `self/build/lower.cot` | Phases 0-10 ✓, constructor/poll NOT ported |
| AST | `self/parse/ast.cot` | Phases 0-10 ✓, UncheckedSendable/is_sendable NOT ported |

---

## Swift 1:1 Audit Status

| Feature | Swift Reference | Cot Status | Faithful? |
|---------|----------------|------------|-----------|
| async fn | Task.swift:143 | Returns Task(T), ARC heap object | Yes |
| await | GenFunc.cpp | Loads result from task, releases via ARC | Yes |
| Task ARC lifecycle | HeapObject.cpp | alloc(metadata=0, size=16) + release | Yes |
| Sendable | TypeCheckConcurrency.cpp:7488 | isSendable() recursive type walk | Yes |
| @Sendable closures | TypeCheckConcurrency.cpp:2971 | Capture analysis + Sendable check | Yes |
| @unchecked Sendable | TypeCheckConcurrency.cpp:7488 | Bypass recursive check | Yes |
| actor keyword | SE-0306 | Parsed as struct_decl with is_actor=true | Yes |
| Actor implicit self | Actor protocol (AnyObject) | Parser + checker inject self, always pointer | Yes |
| Cross-actor await | TypeCheckConcurrency.cpp:8253 | Error without await, skip for nonisolated | Yes |
| hop_to_executor | SILGenApply.cpp:6155 | Phase 1: no-op (single-threaded) | Yes |
| nonisolated | SE-0313 | FnDecl.is_nonisolated, MethodInfo.is_nonisolated | Yes |
| TaskGroup | TaskGroup.swift | addTask(fn() -> T) closure API | Yes |
| for-await-in | TypeCheckStmt.cpp:3443 | Desugared to while-optional in parser | Yes |
| Channel(T) | Go runtime/chan.go | send/recv/next/close | Yes (Go divergence) |
| @MainActor | SE-0316 | global_actor isolation check | Yes |
| Task cancellation | Task.h | 16-byte TaskObject + cancelled flag | Yes |
| Executor | CooperativeExecutor.swift | enqueue/run/stop | Yes (Phase 1 stub) |
| Continuation | CheckedContinuation.swift | resume/getResult, double-resume panic | Yes |
| TaskLocal | TaskLocal.swift | withValue/get, List-based stack | Yes (simplified) |
| State machine | Rust coroutine.rs | Constructor/poll split, if/else dispatch | Partial (gated) |
| async let | SE-0317, AsyncLet.swift | **NOT IMPLEMENTED** | — |
| AsyncSequence protocol | AsyncSequence.swift | **NOT IMPLEMENTED** (using duck typing) | — |
| Task.sleep | TaskSleep.swift | **NOT IMPLEMENTED** | — |
| sending parameter | SE-0430 | **NOT IMPLEMENTED** | — |
| Region isolation | SE-0414, RegionIsolation.cpp | **NOT IMPLEMENTED** | — |

---

## Next Phases — Detailed Execution Plan

### Phase A: Fix Wasm Tech Debt (PRIORITY)

**Two bugs block Wasm parity. Fix both before new features.**

#### A.1: Wasm test runner error union return bug

**Symptom:** for-await tests compute correct values but report "expected: 0, received: N".

**Root cause:** The test runner's `generateTestRunner` in `lower.zig` calls each test function and reads the error union tag at `eu_ptr[0]`. For tests with for-await accumulator loops, the error union pointer or offset is wrong in Wasm codegen.

**Fix approach:**
1. Build the failing test to Wasm with `COT_SSA='*'` and examine the test function's SSA
2. Compare the error union return path for a simple test (works) vs a for-await test (fails)
3. The difference is likely the for-await loop creating extra locals that shift the error union local's stack offset
4. Fix the error union local addressing in the Wasm codegen

**Test:** `task_group for-await iteration`, `channel for-await iteration`, `task_group closure captures` should all pass.

#### A.2: Wasm VOID-typed ops missing setReg

**Symptom:** `values remaining on stack at end of block` in features.cot, arrays.cot, loops.cot, strings.cot on Wasm.

**Root cause:** The fix in commit `08050b1` added `setReg` for `off_ptr` and `local_addr` with VOID type, but other ops that produce Wasm stack values also have VOID type and need the same treatment.

**Fix approach:**
1. Build features.cot to Wasm with `COT_DEBUG=codegen`, find the function that fails
2. Look at the SSA values that reach `ssaGenValue` — any op that calls `ssaGenValueOnStack` and pushes a value but has `type_idx == VOID` needs `setReg`
3. Likely candidates: `global_addr`, `ptr_add`, `elem_ptr`, `addr_add`
4. Add them to the VOID setReg check alongside `off_ptr` and `local_addr`

**Test:** features.cot, arrays.cot, loops.cot, strings.cot should all compile on Wasm.

---

### Phase B: async let (SE-0317)

**Swift parallel bindings — launch child task, await result at use site.**

```cot
async let x = fetchData()
async let y = fetchOther()
let result = await x + await y  // both run concurrently
```

**Swift reference:** `AsyncLet.swift`, `AsyncLet.cpp`, `AsyncLet.h`

**Implementation:**

1. **Parser** — Parse `async let x = expr` as a new AST node `AsyncLetDecl { name, expr, span }`
2. **Checker** — Type-check the expression, infer type T, the binding has type Task(T)
3. **Lowerer** — Desugar to:
   ```
   // async let x = fetchData()
   var __async_let_x = fetchData()  // call immediately (eager, Phase 1)

   // await x
   load result from __async_let_x task ptr, release task
   ```
   Phase 1 (eager): child task runs to completion immediately. The `await x` just loads the result.
   Phase 2 (real): `_asyncLetStart` launches child task, `_asyncLet_get` suspends until ready.

4. **Scope cleanup** — At scope exit, if async let hasn't been awaited, implicitly await + release. Swift uses `_asyncLet_finish` for this.

5. **Tests:**
   ```cot
   test "async let basic" {
       async fn getValue() int { return 42 }
       async let x = getValue()
       @assertEq(await x, 42)
   }

   test "async let parallel" {
       async fn getA() int { return 10 }
       async fn getB() int { return 20 }
       async let a = getA()
       async let b = getB()
       @assertEq(await a + await b, 30)
   }
   ```

**Swift references:**
- `AsyncLet.swift` — `_asyncLetStart`, `_asyncLet_get`, `_asyncLet_finish`
- `AsyncLet.h` — ABI struct, preallocation from parent stack
- SE-0317 proposal

---

### Phase C: Task.sleep

**Suspend current task for a duration.**

```cot
async fn delayed() int {
    await Task.sleep(1_000_000_000)  // 1 second in nanoseconds
    return 42
}
```

**Swift reference:** `TaskSleep.swift` — continuation-based, uses executor scheduling.

**Implementation:**

1. **Stdlib** — Add `Task.sleep(nanoseconds: i64)` as an async function
2. **Native** — Use `nanosleep` syscall (or platform equivalent)
3. **Wasm** — Phase 1: busy-wait (no real timer). Phase 2: integrate with event loop executor.
4. **Cancellation** — Check task cancellation flag before and after sleep. If cancelled, return early.

**Tests:**
```cot
test "task sleep zero" {
    await Task.sleep(0)  // should return immediately
}
```

---

### Phase D: AsyncSequence Protocol

**Formalize the iteration protocol for async streams.**

Currently, `for await val in seq { }` desugars to `while (seq.next()) |val| { }` via duck typing. Swift uses a formal `AsyncSequence` protocol with `makeAsyncIterator()` + `AsyncIterator.next()`.

**Swift reference:** `AsyncSequence.swift` — protocol with `Element`, `Failure` associated types, `makeAsyncIterator()` method.

**Implementation:**

1. **Trait** — Define `AsyncSequence` trait:
   ```cot
   trait AsyncSequence {
       fn next(self: *Self) ?Self.Element
   }
   ```
   (Simplified from Swift — Cot uses duck typing for `next()` already, this formalizes it.)

2. **Conformance** — `impl AsyncSequence for TaskGroup(T)`, `impl AsyncSequence for Channel(T)`

3. **Stdlib methods** — Port Swift's `reduce`, `contains`, `first(where:)`, `allSatisfy` as trait methods.

4. **Tests:**
   ```cot
   test "async sequence reduce" {
       var group = TaskGroup(int).init()
       group.addTask(fn() int { return 1 })
       group.addTask(fn() int { return 2 })
       group.addTask(fn() int { return 3 })
       const sum = group.reduce(0, fn(acc: int, val: int) int { return acc + val })
       @assertEq(sum, 6)
   }
   ```

---

### Phase E: Selfcot Port of New Features

**Port Phases 4 (@Sendable), 5 (@unchecked), 11 (TaskLocal), 2 (constructor/poll) to self/.**

| Feature | File | What to Port |
|---------|------|-------------|
| @Sendable closure | `self/parse/parser.cot` | Parse `@Sendable fn(...)` |
| @Sendable closure | `self/check/checker.cot` | `checkSendableCaptures()` |
| @Sendable closure | `self/parse/ast.cot` | `ClosureExpr.is_sendable` |
| @unchecked Sendable | `self/parse/parser.cot` | Parse `impl @unchecked Sendable for Type {}` |
| @unchecked Sendable | `self/check/checker.cot` | `unchecked_sendable_types` map |
| @unchecked Sendable | `self/parse/ast.cot` | `UncheckedSendable` decl |
| Constructor/poll | `self/build/lower.cot` | `lowerAsyncConstructorPoll` |
| Constructor/poll | `self/build/ir.cot` | `is_async_poll` flag |

---

### Phase F: State Machine Wasm Dispatch Fix

**Ungate async_split.zig for real suspension.**

**Current bug:** The if/else dispatch chain in async_split loads `frame[state_offset]` correctly, but the state written by the spill code doesn't propagate to the next poll call. The issue is likely that the Wasm codegen's store to `frame[8]` (state) uses a local that isn't flushed, or the `off_ptr` for the state address is optimized away.

**Fix approach:**
1. Build the multi-await test with `COT_SSA='combined__poll'`
2. Compare the SSA before and after async_split
3. Trace the state store through lower_wasm → Wasm gen → disassembly
4. Verify the `i64.store` for state actually writes to the correct memory address
5. Add a dedicated test: 3-await chain where each suspend point has live locals

**When done:** Remove `if (true or ...)` gate in `async_split.zig:189`. All multi-await functions will use state machine on Wasm.

---

## Priority Order

| Priority | Phase | Effort | Why |
|----------|-------|--------|-----|
| 1 | A (Wasm tech debt) | 1-2 days | Unblocks Wasm parity, clears known failures |
| 2 | B (async let) | 2-3 days | Core language feature, parallel bindings |
| 3 | C (Task.sleep) | 1 day | Required for real async programs |
| 4 | D (AsyncSequence) | 2 days | Formalizes existing duck-typed protocol |
| 5 | E (selfcot port) | 1-2 days | Maintains self-hosting parity |
| 6 | F (state machine fix) | 2-3 days | Enables real suspension on Wasm |

---

## Build/Test

```bash
zig build
./zig-out/bin/cot test test/e2e/concurrency.cot    # 18/18
./zig-out/bin/cot test test/e2e/task_group.cot     # 7/7
./zig-out/bin/cot test test/e2e/channel.cot        # 5/5
./zig-out/bin/cot test test/e2e/continuation.cot   # 2/2
./zig-out/bin/cot test test/e2e/executor.cot       # 1/1
./zig-out/bin/cot test test/e2e/task_local.cot     # 3/3
./zig-out/bin/cot test test/e2e/features.cot       # 370/370
zig build test                                      # compiler internals

# Wasm (18/18 concurrency, but task_group/channel for-await fail)
./zig-out/bin/cot test test/e2e/concurrency.cot --target=wasm

# Selfcot
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot
/tmp/selfcot version  # cot 0.4.0 (self-hosted)
```

## Reference Files

| What | Where |
|------|-------|
| Swift Task ABI | `references/swift/include/swift/ABI/Task.h` |
| Swift Actor ABI | `references/swift/include/swift/ABI/Actor.h` |
| Swift AsyncLet | `references/swift/stdlib/public/Concurrency/AsyncLet.swift` |
| Swift AsyncSequence | `references/swift/stdlib/public/Concurrency/AsyncSequence.swift` |
| Swift TaskSleep | `references/swift/stdlib/public/Concurrency/TaskSleep.swift` |
| Swift TaskLocal | `references/swift/stdlib/public/Concurrency/TaskLocal.swift` |
| Swift Sendable checking | `references/swift/lib/Sema/TypeCheckConcurrency.cpp:7488` |
| Swift @Sendable closures | `references/swift/lib/Sema/TypeCheckConcurrency.cpp:2971` |
| Swift region isolation | `references/swift/lib/SILOptimizer/Utils/RegionIsolation.cpp` |
| Rust coroutine transform | `references/rust/` (coroutine.rs) |
| Cot async_split SSA pass | `compiler/ssa/passes/async_split.zig` (683 lines) |
| Cot constructor/poll | `compiler/frontend/lower.zig` (lowerAsyncConstructorPoll) |
