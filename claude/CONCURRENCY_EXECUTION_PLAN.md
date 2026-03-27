# Concurrency Implementation Execution Plan

**Date:** 2026-03-27 (final update)
**Design doc:** `claude/SWIFT_CONCURRENCY_PORT.md` (1,971 lines, audited)
**Status:** ALL PASS. 409 native, 38/38 Wasm concurrency, 22/22 Wasm cases. Zero failures.

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

### Wasm — ALL CONCURRENCY PASS

| Suite | Tests | Status |
|-------|-------|--------|
| concurrency.cot | 21 | 21/21 |
| task_group.cot | 7 | 7/7 |
| channel.cot | 5 | 5/5 |
| continuation.cot | 2 | 2/2 |
| task_local.cot | 3 | 3/3 |
| test/cases/ (22 files) | 22 | 22/22 |
| **Total Wasm** | **60** | **60/60** |

**Remaining Wasm issue:** features.cot test mode fails (print.cot `expected i64, found f64` — float/int type confusion in print codegen). Build mode works. Not a concurrency bug.

### Wasm Linker Bugs Fixed (2026-03-27 session 2)

| Bug | Root Cause | Fix |
|-----|-----------|-----|
| Type section 0xAA corruption | `addType` used SSA `param_count` but `params[]` only filled for IR `wasm_param_idx` entries | Fill remaining param slots with i64 for async poll state pointer |
| CFG pred/succ invariant | `splitBlockAtSuspend` transferred succs without updating pred back-pointers | Patch pred entries in successor blocks |
| async_split transforms non-poll fns | Test fns with 2+ `async let` treated as state machines | Only transform `__poll` functions |

### Wasm Bugs Fixed This Session

| Bug | Root Cause | Fix | Commit |
|-----|-----------|-----|--------|
| Stores silently dropped | Memory threading left SSA_MEM type on store ops | Reset to VOID after strip | `08050b1` |
| VOID-typed ops leave values on stack | off_ptr/local_addr/add_ptr etc. need setReg | `producesAddressValue()` method on Op | `5ab26d6` |
| Test runner reads stale stack memory | Error union returned as stack pointer | Return i64 tag directly | `c7fded6` |
| for-await loop body gets wrong sum | `inferBinaryType` returns VOID for `untyped + i64` | Resolve VOID/untyped to I64 using right operand | `acac253` |
| Duplicate Wasm export names | async let test reuses function names | Rename functions | `acac253` |
| test/cases/ arrays, loops, strings | Same as VOID-typed ops | Same fix | `5ab26d6` |

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

## Selfcot Port Status — ALL PORTED

| Component | Status | Commit |
|-----------|--------|--------|
| Parser | All features ported (phases 0-13) | `af9697e` `2c54c63` |
| Checker | All features ported (phases 0-13) | `af9697e` `2c54c63` |
| AST | All features ported (phases 0-13) | `af9697e` `2c54c63` |
| IR | All features ported (is_async_poll) | `2c54c63` |
| Lowerer | All features ported (constructor/poll, async let) | `2c54c63` |
| Selfcot builds | 2696 functions, 478 VWT types | verified |

---

## Remaining Work

### Done ✓ (completed this session)
- ~~Fix Wasm for-await~~ → `inferBinaryType` VOID→I64 resolution (`acac253`)
- ~~Fix Wasm VOID-typed ops~~ → `producesAddressValue()` (`5ab26d6`)
- ~~Fix test runner~~ → return i64 tag directly (`c7fded6`)
- ~~Selfcot port~~ → all features ported (`2c54c63`)

### Fixed (2026-03-27 session 2)

1. **Wasm linker truncation (was the blocker)** — NOT a truncated file. 21KB binary was written correctly. Real bug: type section had uninitialized `0xAA` byte in function type params. Root cause: `addType(params[0..param_count])` used SSA arg count but `params[]` was only filled for `wasm_param_idx` entries from IR params. Async poll functions have extra SSA args (state pointer) not in `ir_func.params`. Fixed by filling remaining slots with i64.

2. **CFG edge invariant in async_split** — `splitBlockAtSuspend` transferred successor edges to resume block but didn't update predecessor back-pointers in successor blocks. Fixed by iterating successors and patching pred entries.

3. **async_split scope too broad** — Non-poll functions with 2+ await points (like test functions using `async let`) were incorrectly transformed into state machines, changing their signature and breaking callers. Fixed by only transforming `__poll` functions.

### Open

1. **Wasm print.cot float type mismatch** — `expected i64, found f64` in test mode. Build mode works. Not a concurrency bug.

2. **AsyncSequence formal protocol** — Deferred. Requires associated types in traits. Duck typing via `next()` works. Generic impl methods with closure params hit a compiler bug.

### Future Swift Features

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
