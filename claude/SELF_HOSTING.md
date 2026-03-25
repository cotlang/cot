# Cot Self-Hosting: Status, Blockers, and Path to 0.4

**Updated:** 2026-03-25
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: selfcot2.wasm Builds (1.9MB), Zig Compiler at 370/370

**Audited 2026-03-25** — Zig compiler: 370/370 features pass, 22/22 cases pass (Phase 8 VWT complete).
Selfcot: builds, all test cases pass, selfcot2.wasm builds. Next: selfcot2 runtime bugs.

**Build chain:**
1. `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` → native binary (Success)
2. `/tmp/selfcot test test/cases/*.cot` → **22/22 test files pass** (132 tests)
3. `/tmp/selfcot build self/main.cot -o /tmp/selfcot2.wasm` → **1.9MB Wasm** (Success)
4. `wasmtime compile /tmp/selfcot2.wasm` → fails: duplicate export name (Zig compiler bug, see below)

**All 42 files compile individually to valid Wasm via selfcot:**
- `self/parse/` — token, scanner, parser, ast, source (5 files)
- `self/check/` — checker, types, errors, comptime (4 files)
- `self/build/` — ir, lower, builder, arc, ssa (5 files)
- `self/optimize/` — copyelim, cse, deadcode, decompose, layout, rewrite, rewritedec, schedule (8 files)
- `self/emit/wasm/` — gen, assemble, link, preprocess, types, constants, builder, prog, mem, print, wasi, test, bench, slice, driver, passes, passes_dec, lower (18 files)
- `self/main.cot`, `self/test_tiny.cot` (2 files)

**~46,250 lines across 42 files.**

---

## Comparison: Zig Compiler vs Selfcot Wasm Output

Both the Zig compiler and selfcot compile the same source (`self/main.cot`) to Wasm. Here's how the outputs compare:

| Metric | Zig compiler | Selfcot | Match? |
|--------|-------------|---------|--------|
| Function count | 5,409 | 5,409 | **Exact match** |
| Binary size | 2.44 MB | 1.90 MB | Selfcot 22% smaller |
| Unique function names | 3,517 | 3,515 | 3,203 shared (91%) |
| Duplicate exports | 2 (`resolveComptimeFieldAccess`, `evalComptimeValueTag`) | Same 2 | **Both have same bug** |
| Validation | Fails (duplicate export) | Fails (same reason) | Same error |

**Name differences are cosmetic:** The ~300 names that differ are generic instantiations with different type indices (e.g., `List(1185)` vs `List(1198)`) — the functions are the same, just numbered differently due to type registry ordering. The `__cot_init_file_N` functions differ in numbering (Zig starts at stdlib offset, selfcot at 0) — same functions, different indices.

**The size difference (500KB)** needs investigation. Possible causes:
- Selfcot may generate simpler code for some patterns (e.g., short-circuit fix reduced IR)
- Missing data segments or string literals
- Different constant folding behavior

**The duplicate export bug is in the Zig compiler's lowerer** — both compilers produce the same duplicates, confirming selfcot correctly mirrors the Zig behavior (including its bugs).

---

## Current Blocker: Wasm Validation Error (i32/i64 type mismatch)

`wasmtime compile /tmp/selfcot2.wasm` fails at function 130 (`List_ensureUnique`):
```
type mismatch: expected i32, found i64
```

**Root cause:** WASI import `fd_write` expects `(i32, i32, i32, i32) -> i32` but the Wasm codegen passes i64 values without wrapping to i32. This is in `List_ensureUnique`'s COW copy path which calls `alloc` (which internally uses `fd_write`).

**This is a pre-existing Wasm codegen issue** — NOT caused by Phase 8 VWT changes. The i32/i64 mismatch occurs whenever a shared generic body calls runtime functions that use WASI imports.

**Fix required in:** `compiler/codegen/wasm/wasm_gen.zig` — ensure all WASI import call sites wrap i64 values to i32.

**Progress (2026-03-25):** Fixed silent func_idx=0 fallback, added memcmp/retain/release to Wasm runtime. Zero unresolved function warnings now.

**MILESTONE (2026-03-25):** selfcot2.wasm VALIDATES and RUNS.
```
wasmtime run selfcot2.wasm version → cot 0.3.7 (self-hosted)
wasmtime run selfcot2.wasm build self/test_tiny.cot → starts compiling, crashes at Map hash
```

**Current blocker:** Runtime codegen bug — Map string hash receives corrupt string (ptr=0, len=0) during checker's type declaration pass. The Wasm binary runs but produces incorrect data for string operations in some codepaths.

---

## Guarded Bugs (Not Root-Caused)

### 1. Null func_addr in buildGenericCacheKey
- **Location:** `self/emit/wasm/gen.cot:1802-1808`
- **Symptom:** SSA `addr` value has `SsaAux.str` with `ptr=0, len=1`
- **Only occurs for:** `Checker_buildGenericCacheKey` (1 instance total)
- **Guard:** Emits `i64.const 0` instead of crashing (SIGSEGV)
- **Impact:** Function pointer for `buildGenericCacheKey` will be 0 at runtime — crash if called via `call_indirect`
- **Root cause:** Unknown — IR or SSA builder creates a phantom func_addr node with corrupt string data

### 2. Negative value in writeULEB128
- **Location:** `self/emit/wasm/assemble.cot:27`
- **Symptom:** `writeULEB128` called with value -1 → infinite loop (unsigned `>>` never reaches 0)
- **Source:** `Lowerer_emitErrorVoidSuccessReturn` has an unresolved branch offset
- **Guard:** Clamps to 0
- **Impact:** One branch instruction has wrong target — likely a cold error path
- **Root cause:** `pass6ResolveBranches` in preprocess.cot fails to resolve a branch for this specific function

---

## Resolved Bugs (2026-03-21 → 2026-03-23)

### Bug 11: Exponential IR Blowup in Short-Circuit Lowering — FIXED (2026-03-23)
**Root cause:** `lowerShortCircuit()` in `self/build/lower.cot` re-lowered the left operand that `lowerBinary()` had already lowered. For chains of N `or`/`and` operators, this caused **2^N exponential IR node duplication**.

**Evidence (before → after fix):**
| Function | Before | After | Zig reference |
|----------|--------|-------|---------------|
| `Parser_synchronize` (13 `or` terms) | 53,251 nodes | <500 | <500 |
| `GenState_ssaGenValue` (huge switch) | 107,032 nodes | <500 | 724 |
| `Checker_isNumericType` (12 `or` terms) | 26,615 nodes | <500 | <500 |
| `GenState_allocateLocals` | 27,909 nodes | 735 | 764 |

**Fix:** Changed `lowerShortCircuit(bin)` → `lowerShortCircuit(left, bin)`, passing the already-lowered left operand. Matches Zig's `lowerShortCircuit(fb, left, bin)` signature exactly.

**Impact:** selfcot2.wasm build went from **hanging indefinitely** (preprocess stuck on 194K-instruction function) to completing in ~15 seconds.

### Bug 10: Union switch infinite loop — FIXED (2026-03-22)
**Root cause:** Missing `else` handling in union switch dispatch. Ported from Zig `lower.zig:7935-7955`.

### Bug 9: Deadcode pass corrupting block indices — FIXED (2026-03-22)
**Root cause:** Deadcode pass removed unreachable blocks, invalidating edge references. Rewrote to match `Zig deadcode.zig` structure exactly.

### Bug 8: lower_wasm not lowering control values — FIXED (2026-03-23)
**Root cause:** Control values on blocks weren't being lowered by the `lower_wasm` pass, leaving high-level SSA ops that the Wasm codegen didn't understand.

### Bug 7: ARC retain/release emitted on Wasm — FIXED (2026-03-21)
**Root cause:** selfcot's lowerer emitted `retain`/`release` calls on Wasm target, but Zig compiler skips them (Wasm = bump allocator). In `Map_remove`, `@arcRelease(string)` decomposed into (ptr, len) but `release` takes 1 i64 → extra value on stack → validation error.

### Bug 5: `dealloc()` on `alloc_raw()` memory — FIXED (2026-03-19)
**Root cause:** `editDistance()` allocated with `alloc_raw()` (no ARC header) but freed with `dealloc()`. Now **impossible to reintroduce** — `alloc_raw` returns `RawPtr` (distinct type), `dealloc` takes `i64`.

### Bug 6: Map states read as `*u8` instead of `*i64` — FIXED (2026-03-19)
**Root cause:** `findSimilarType()` iterated Map internal arrays reading states as single bytes instead of 8-byte i64 values.

### Bug 2: Multi-Param Function Call Type Mismatch — FIXED
**Root cause:** `alloc(0, ...)` used for raw buffers. Fix: `alloc(0, ...) → alloc_raw(...)`.

### ARC ?*T SSA Mismatch — FIXED
**Root cause:** `?*T` was 16 bytes in struct fields but 8 bytes in SSA.

### Ad-Hoc ARC Dispatch — FIXED
All inline retain/release replaced with centralized `emitCopyValue`/`emitDestroyValue`.

### VWT Migration — COMPLETE (2026-03-25)
**VWT dispatch fully connected.** `emitCopyValue` and `emitDestroyValue` now call VWT witness
functions (`__vwt_initializeWithCopy_{type}`, `__vwt_destroy_{type}`) for ALL non-trivial types.
Old inline ARC code deleted (~450 lines). Dict/stenciling infrastructure deleted from `self/`
(~620 lines). VWT witness emission gated on native target (wasm skips — no ARC).
- 482 VWT types emitted, 135 unique witnesses, 2,558 total functions
- Selfcot builds in ~10.9s (was 8.7s pre-VWT dispatch — ~1s overhead from witness functions)
- `emitVWTWrapper`, `isGenericReturnType`, `emitOptionalFieldRetain` deleted
- `computeGenericBaseName` kept (used by active VWT path)
- Checker monomorphization kept (still needed for type checking — VWT changes codegen only)

### Blockers A, B, C from 2026-03-20 — ALL RESOLVED
- **Blocker A** (SIGSEGV in lowerGenericFnInstance for ir, ssa, builder) — resolved
- **Blocker B** (Scope import resolution for checker, lower) — resolved
- **Blocker C** (Enum method resolution in token.cot tests) — only affects test blocks, not builds

---

## Assessment: How Close Is Self-Hosting?

**Structurally complete.** The selfcot compiler produces a Wasm binary with the exact same 5,409 functions as the Zig compiler. The binary is well-formed enough that `wasm-objdump` can parse and analyze it. All 22 test files (132 tests) pass through selfcot.

**Remaining work estimate:** ~5-15 runtime codegen bugs, comparable to the 22 test files that needed fixing (arrays, chars, loops, optional, strings, structs — all resolved in 2 sessions).

**Evidence for optimism:**
- Function counts match exactly (5,409 = 5,409)
- 91% of function names match (differences are type index numbering)
- All 22/22 test files pass — basic codegen is correct
- The duplicate export bug is in the Zig compiler, not selfcot

**Evidence for caution:**
- 2 guarded bugs produce wrong code silently
- 500KB size difference needs investigation
- Zero runtime testing of selfcot2.wasm (blocked by duplicate export)
- Every pipeline stage (scanner, parser, checker, lowerer, SSA, codegen, linker) is untested as Wasm-compiled code beyond trivial paths
- The arg parsing bug from the previous milestone may still exist

**Critical path:**
1. Fix i32/i64 type mismatch in Wasm codegen for WASI imports → selfcot2.wasm validates
2. Fix duplicate export names if still present → clean validation
3. Test selfcot2.wasm runtime → fix 5-15 codegen bugs as they surface
4. `wasmtime run selfcot2.wasm build self/test_tiny.cot` → first bootstrap test
5. `wasmtime run selfcot2.wasm build self/main.cot -o selfcot3.wasm` → full bootstrap = 0.4

---

## Debugging Methodology

**What works (proven 2026-03-23):**
1. Add debug prints to **both** Zig compiler and selfcot — compare per-function IR/SSA node counts
2. Binary narrowing — println at each pipeline stage to find the exact hang/crash point
3. Side-by-side line-by-line comparison — when selfcot diverges from Zig, the divergence IS the bug

**What doesn't work:**
- Manually searching for bugs by reading code (5 hours wasted in previous session)
- Guessing root causes without data
- Running the same failing command repeatedly hoping for different results

**Key insight:** The Zig compiler compiling the same source (`self/main.cot`) is the ground truth. Any difference in output is a bug in selfcot. Compare numbers, not code.

---

## Path to 0.4

### Phase 1: All Files Compile to Valid Wasm — COMPLETE
- [x] All 42 self/ files compile individually to valid Wasm
- [x] Whole-program build produces selfcot2.wasm (1.9MB)
- [x] 22/22 test files pass through selfcot (132 tests)

### Phase 2: selfcot2.wasm Can Compile Files (current)
- [x] `/tmp/selfcot build self/main.cot -o /tmp/selfcot2.wasm` succeeds
- [ ] Fix duplicate export in Zig compiler (blocks validation of both Zig and selfcot Wasm output)
- [ ] Root-cause guarded bugs (null func_addr, negative ULEB128)
- [ ] Fix arg parsing bug (3+ args) in selfcot2.wasm
- [ ] Debug and fix runtime codegen bugs (~5-15 expected)
- [ ] `wasmtime selfcot2.wasm build self/test_tiny.cot -o /tmp/out.wasm` succeeds
- [ ] `wasmtime selfcot2.wasm build self/main.cot -o /tmp/selfcot3.wasm` succeeds (full bootstrap)

### Phase 3: Release Polish
- [ ] Homebrew tap + x86_64-macos binary
- [ ] VS Code marketplace extension
- [ ] Error messages polish pass

---

## Key Files

| File | Purpose |
|------|---------|
| `self/main.cot` | Selfcot entry point + multi-file pipeline |
| `self/build/lower.cot` | IR lowering (~9,700 lines, most complex file) |
| `self/check/checker.cot` | Type checker (~6,600 lines) |
| `self/emit/wasm/gen.cot` | Wasm code generator (~2,700 lines) |
| `self/emit/wasm/driver.cot` | Wasm codegen driver (SSA passes + assembly) |
| `self/emit/wasm/preprocess.cot` | Instruction preprocessing (dispatch loop, branch resolution) |
| `self/emit/wasm/assemble.cot` | Wasm bytecode assembly (LEB128 encoding) |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (reference implementation) |
| `compiler/frontend/ssa_builder.zig` | SSA builder (reference) |
| `compiler/driver.zig` | Zig codegen driver (reference for selfcot driver.cot) |
