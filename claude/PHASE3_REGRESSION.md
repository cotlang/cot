# Phase 3-5 Selfcot Regression

**Date:** 2026-03-19
**Status:** Investigating
**Context:** TypeLowering Phase 3-5 changes (commit 87c6179) regressed selfcot from 8/13 to 0/13 files

## Reproduction

```bash
# After Phase 1-2 commit (6d05e56): 8/13 selfcot files compile
# After Phase 3-5 commit (87c6179): 0/13 selfcot files compile

/tmp/selfcot build self/parse/source.cot -o /tmp/source.wasm
# SIGSEGV in Map_rehash during Scope_define → collectFnDecl
```

## Crash Details

```
SIGSEGV in Map(17;1556)_rehash + 1024
Called from: Map_set → Scope_define → Checker_defineSymbol → Checker_collectFnDecl
```

Map rehash crashes during stdlib checking. The Map was created via `.{}` (zero init). Growing from capacity 0 should work. The crash is in the selfcot binary's compiled code — the rehash function itself is corrupted.

## What Changed

Phase 3: Field assignment sites replaced inline `.pointer.managed` check + `managedFromLowered` + conditional retain/release with `emitCopyValue`/`emitDestroyValue` + ownership check.

Phase 4: `emitFieldReleases` and `emitPendingAutoDeinits` replaced per-type inline release with centralized `emitDestroyValue` iteration.

Phase 5: Return value retain simplified (uses inline pattern for managed types).

## Why 83/83 Tests Pass But Selfcot Breaks

The test suite exercises ARC with simple types (i64, string, pointer). Selfcot has complex nested types (Map(string, *Symbol) inside Scope with ?*Scope parent). The Phase 3-5 changes generate DIFFERENT IR for the selfcot binary's ARC operations — the IR is semantically equivalent for simple cases but produces different machine code for complex ones.

## Specific Concern: emitDestroyValue in Auto-Deinit

The `emitPendingAutoDeinits` now uses `emitDestroyValue` which checks `isTrivial`. For Scope:
- `parent: ?*Scope` → NOT trivial → emitDestroyValue emits unwrap-then-release
- `symbols: Map(...)` → trivial (struct_type → true) → skipped

This SHOULD be identical to the original code which only released managed pointers. But the generated IR may have subtle differences (different block names, different SSA value ordering) that affect the native codegen.

## Investigation Plan

1. Compare disassembly of `Scope_define` between old selfcot (working, from Phase 1-2) and new selfcot (broken, from Phase 3-5)
2. Check if the auto-deinit for Scope generates different code
3. Write a minimal test that reproduces the Map_rehash crash
4. Trace the exact codegen difference using the pipeline debug tools
