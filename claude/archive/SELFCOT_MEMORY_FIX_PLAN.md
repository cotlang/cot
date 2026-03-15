# Selfcot Memory Fix — Execution Plan (REVISED)

**Date:** 2026-03-15
**Status:** Root cause identified — the lowerer hangs on source.cot, NOT the SSA/codegen pipeline

---

## What the Previous Audits Got Wrong

All three audit documents (MEMORY_LEAK_DEBUG.md, SELFCOT_MEMORY_AUDIT.md, COW_EXECUTION_PLAN.md) blamed the SSA/codegen pipeline for the 6.5GB memory usage. They were wrong.

**Instrumentation proves the lowerer (Phase 3 IR generation) is the bottleneck:**

```
=== Phase 3: lowering 7 files ===
  lowering [0]: stdlib/sys.cot       → done, 0 funcs
  lowering [1]: stdlib/list.cot      → done, 0 funcs
  lowering [2]: stdlib/string.cot    → done, 8 funcs
  lowering [3]: self/frontend/token.cot → done, 15 funcs
  lowering [4]: self/frontend/source.cot → HANGS (6.5GB, never finishes)
```

`lowerOneFile(source.cot)` never returns. The process consumes ~400MB/s during this call. The SSA pipeline and codegen are never reached.

**source.cot is 315 lines** with simple structs (Pos, Position, Span, Source) and 19 tests. Nothing that should cause infinite allocation. The problem is in the selfcot lowerer (`self/frontend/lowerer.cot`) hitting a pathological case — likely related to generic monomorphization or string interpolation lowering.

**For comparison:**
- `selfcot check self/frontend/scanner.cot` — 9MB, <0.01s (Phase 1+2 only — no lowering)
- `selfcot build self/test_tiny.cot` — 3MB, <0.01s (no imports, trivial lowering)
- `selfcot build self/test_import_only.cot` (imports list.cot) — 4MB, <0.01s (succeeds)

The memory crisis is entirely in one function call: `lowerOneFile(source.cot)`.

---

## What free() Calls Fixed (And Didn't Fix)

free() methods were added to: SsaValue, SsaBlock, SsaFunc, SSABuilder, GenState, ProgBuilder. Called in driver.cot after each function's codegen. Also freed: schedule.cot/layout.cot/rewritegeneric.cot temporaries, registerFuncType params/results.

**Impact on scanner.cot: ZERO.** 4.3GB@10s before → 4.3GB@10s after. Because the memory is consumed in the lowerer, which runs before any of these free() paths execute.

**Impact on test_import_only.cot: Verified working.** The free() calls are correct and don't cause use-after-free (after fixing the GenState.free() to not free builder.progs/gc_ref_locals which are aliased by the returned Symbol).

The free() calls are still valuable — they'll matter once the lowerer bug is fixed and codegen actually runs. But they're irrelevant to the current crisis.

---

## Next Step: Diagnose the Lowerer Hang

The selfcot lowerer is in `self/frontend/lowerer.cot`. It's a port of the Zig `compiler/frontend/lower.zig`. The hang on source.cot suggests either:

1. **Infinite loop in generic monomorphization.** source.cot uses `List(int)` (via `line_offsets: List(int)` in the Source struct). The lowerer may be infinitely re-monomorphizing List methods.

2. **String interpolation blowup.** source.cot has `"${self.filename}:${self.line}:${self.column}"` in `Position.toString()`. String interpolation lowering may generate exponential IR.

3. **Method resolution loop.** The lowerer resolves method calls by constructing qualified names. If the shared `lowered_generics` map fails to dedup, methods get re-lowered infinitely.

**To diagnose:** Add `eprintln` inside `lowerOneFile` to trace which function/declaration is being processed when it hangs. Narrow to the specific function in source.cot causing the blowup.

---

## Summary of Changes Made (keep these)

| File | Change | Why |
|------|--------|-----|
| `self/frontend/ssa.cot` | Added `free()` to SsaValue, SsaBlock, SsaFunc | Needed once codegen runs |
| `self/frontend/ssa_builder.cot` | Added `free()` to SSABuilder | Needed once codegen runs |
| `self/codegen/wasm/wasm_gen.cot` | Added `free()` to GenState (excludes aliased fields) | Needed once codegen runs |
| `self/codegen/wasm/prog.cot` | Added `free()` to ProgBuilder | Needed once codegen runs |
| `self/codegen/wasm/driver.cot` | Call ssa_func.free(), builder.free() after codegen; free params/results in registerFuncType | Needed once codegen runs |
| `self/ssa/passes/rewritegeneric.cot` | Free to_rewrite list | Needed once codegen runs |
| `self/ssa/passes/schedule.cot` | Free 9 per-block temporaries | Needed once codegen runs |
| `self/ssa/passes/layout.cot` | Free 6 per-function temporaries | Needed once codegen runs |
