# Concurrency Implementation Execution Plan

**Date:** 2026-03-27
**Design doc:** `claude/SWIFT_CONCURRENCY_PORT.md` (1,971 lines, audited)
**Status:** 409 tests pass native. 22/22 Wasm cases pass. 4 Wasm e2e failures remain.

---

## Current Test Status

### Native — 409/409 PASS

| Suite | Tests | Status |
|-------|-------|--------|
| features.cot | 370 | 370/370 |
| concurrency.cot | 21 | 21/21 |
| task_group.cot | 7 | 7/7 |
| channel.cot | 5 | 5/5 |
| continuation.cot | 2 | 2/2 |
| executor.cot | 1 | 1/1 |
| task_local.cot | 3 | 3/3 |
| **Total** | **409** | **409/409** |

### Wasm — 4 FAILURES

| Failure | Root Cause | Status |
|---------|-----------|--------|
| task_group for-await iteration | for-await desugaring: synthesized `group.next()` doesn't re-read the group struct on each iteration. Manual `while (group.next()) \|val\|` works. The parser's desugared AST node reuses the same field_access base, but the Wasm codegen evaluates it as a value copy instead of a reference. | **Open** — Wasm struct reference bug in desugared for-await |
| task_group closure captures | Same for-await root cause | Same |
| channel for-await iteration | Same for-await root cause | Same |
| features.cot (print.cot subset) | `expected i64, found f64` — float value being stored into i64 local or vice versa in print codegen. NOT the VOID setReg bug (that's fixed). | **Open** — Wasm float/int type confusion in print |

### Wasm — FIXED this session

| Was Broken | Fix | Commit |
|-----------|-----|--------|
| test/cases/arrays.cot | `producesAddressValue()` — add_ptr/sub_ptr/global_addr need setReg | `5ab26d6` |
| test/cases/loops.cot | Same | `5ab26d6` |
| test/cases/strings.cot | Same | `5ab26d6` |
| Test runner stack-use-after-free | Return i64 tag directly, not error union stack pointer | `c7fded6` |
| Wasm stores silently dropped | Strip memory threading type, setReg for off_ptr/local_addr | `08050b1` |

**22/22 test/cases/ now pass on Wasm (was 19/22).**

---

## Completed Phases

| Phase | What | Tests | Key Commit |
|-------|------|-------|------------|
| 0 | Delete 6,156 lines Go concurrency | 370/370 preserved | 5 commits |
| 1 | Task type, async/await, ARC lifecycle | 4 tests | `fb13cb6` |
| 1+ | Block-scoped fn declarations (IR builder nesting) | works | `8b3837e` |
| 2 | async_split.zig SSA pass (683 lines) + constructor/poll split | gated (Wasm dispatch) | `7b54e95` `250aeef` `7755583` |
| 3 | Actor keyword, implicit self, cross-actor isolation, nonisolated (SE-0306, SE-0313) | 3 tests | `cbd57aa` `ad8f39c` `168b9be` |
| 4 | Sendable isSendable() + @Sendable closures (SE-0302) + @unchecked Sendable | capture checking | `a7cca55` `da70878` `a9808ef` |
| 5 | TaskGroup stdlib + closure API + for-await-in desugaring (SE-0304) | 7 tests | `22939ed` `29e2553` |
| 6 | Channel(T) with backpressure (Go-style) | 5 tests | `abc61d4` |
| 7 | @MainActor global actor isolation (SE-0316) | 1 test | `78474b5` |
| 8 | Task cancellation flag (16-byte TaskObject) | 1 test | `16d33fd` |
| 9 | Executor (cooperative, SE-0392) | 1 test | `5d3ac28` |
| 10 | Continuation(T) (CheckedContinuation, SE-0300) | 2 tests | `44d31fd` |
| 11 | TaskLocal(T) (SE-0311) | 3 tests | `5e6cf8d` |
| 12 | async let (SE-0317) | 3 tests | `9b721cd` |
| 13 | Task.sleep (SE-0374) | 3 tests (stdlib) | `32ab411` |
| - | Selfcot port (phases 0-10) | all pass | `af9697e` |
| - | Fix Wasm store codegen | 22/22 cases | `08050b1` `5ab26d6` |
| - | Fix test runner stack-use-after-free | removes stale ptr reads | `c7fded6` |

## Selfcot Port Status

| Component | Phases 0-10 | Phases 11-13 |
|-----------|-------------|-------------|
| Parser | ported | @Sendable, async let NOT ported |
| Checker | ported | @Sendable capture check, @unchecked, async let NOT ported |
| AST | ported | ClosureExpr.is_sendable, UncheckedSendable, AsyncLet NOT ported |
| IR | ported | is_async_poll NOT ported |
| Lowerer | ported | constructor/poll, async let NOT ported |

---

## Next Work — Priority Order

### 1. Fix Wasm for-await struct reference bug

**The for-await desugaring produces correct results on native but returns 0 on Wasm.** The synthesized `group.next()` call evaluates the group as a value copy instead of a reference on Wasm. Manual `while (group.next()) |val|` works because the parser creates a different AST path.

**Root cause investigation needed:**
1. Compare IR/SSA for `for await val in group` vs `while (group.next()) |val|`
2. The `sequence` AST node (group ident) is shared between the field_access and the for_stmt
3. On Wasm, the struct may be getting copied for the method dispatch
4. The fix likely needs the for-await desugaring to explicitly take a pointer/reference

### 2. Fix Wasm print.cot float type mismatch

`expected i64, found f64` in print codegen on Wasm. A print function is storing an f64 value into an i64 local or vice versa.

### 3. Selfcot port (Phases 11-13)

Port @Sendable, @unchecked Sendable, TaskLocal, async let, constructor/poll to `self/`:

| Feature | Files |
|---------|-------|
| @Sendable closure | `self/parse/parser.cot`, `self/check/checker.cot`, `self/parse/ast.cot` |
| @unchecked Sendable | `self/parse/parser.cot`, `self/check/checker.cot`, `self/parse/ast.cot` |
| TaskLocal(T) | stdlib only (auto-compiled) |
| async let | `self/parse/parser.cot`, `self/check/checker.cot`, `self/parse/ast.cot`, `self/build/lower.cot` |
| constructor/poll | `self/build/lower.cot`, `self/build/ir.cot` |

### 4. Ungate state machine SSA transform

The async_split pass produces valid SSA but the Wasm dispatch doesn't work because:
- The if/else chain loads `frame[state_offset]` but the state store doesn't persist across poll calls
- Need to trace the state store through lower_wasm → Wasm gen → disassembly
- Verify `i64.store` at the correct memory address

### 5. AsyncSequence formal protocol

Deferred — requires associated types in traits (language feature). Current duck typing via `next()` works. Generic impl methods with closure params hit a compiler bug (wrong function pointer in monomorphized code).

### 6. Future Swift features

| Feature | Swift SE | Priority | Notes |
|---------|----------|----------|-------|
| sending parameter | SE-0430 | Low | Type-level transfer checking |
| Region isolation | SE-0414 | Low | Forward dataflow analysis on SSA |
| AsyncStream | SE-0314 | Medium | Callback-to-async bridge |
| Task { } expression | SE-0304 | Medium | Unstructured task creation |
| Actor reentrancy | SE-0306 | Low | Cot is non-reentrant by default |

---

## Build/Test

```bash
zig build
./zig-out/bin/cot test test/e2e/features.cot       # 370/370
./zig-out/bin/cot test test/e2e/concurrency.cot     # 21/21
./zig-out/bin/cot test test/e2e/task_group.cot      # 7/7
./zig-out/bin/cot test test/e2e/channel.cot         # 5/5
./zig-out/bin/cot test test/e2e/continuation.cot    # 2/2
./zig-out/bin/cot test test/e2e/executor.cot        # 1/1
./zig-out/bin/cot test test/e2e/task_local.cot      # 3/3
zig build test                                       # compiler internals

# Wasm
./zig-out/bin/cot test test/e2e/concurrency.cot --target=wasm  # 21/21
./zig-out/bin/cot test test/cases/*.cot --target=wasm           # 22/22

# Selfcot
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot
/tmp/selfcot version  # cot 0.4.0 (self-hosted)
```

## Reference Files

| What | Where |
|------|-------|
| Swift Task ABI | `references/swift/include/swift/ABI/Task.h` |
| Swift AsyncLet | `references/swift/stdlib/public/Concurrency/AsyncLet.swift` |
| Swift AsyncSequence | `references/swift/stdlib/public/Concurrency/AsyncSequence.swift` |
| Swift TaskSleep | `references/swift/stdlib/public/Concurrency/TaskSleep.swift` |
| Swift TaskLocal | `references/swift/stdlib/public/Concurrency/TaskLocal.swift` |
| Swift Sendable checking | `references/swift/lib/Sema/TypeCheckConcurrency.cpp:7488` |
| Swift @Sendable closures | `references/swift/lib/Sema/TypeCheckConcurrency.cpp:2971` |
| Rust coroutine transform | `references/rust/` (coroutine.rs) |
| Cot async_split SSA pass | `compiler/ssa/passes/async_split.zig` (683 lines) |
| Cot constructor/poll | `compiler/frontend/lower.zig` (lowerAsyncConstructorPoll) |
