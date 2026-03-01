# Compiler Bugs Found While Building cot.land Package Registry

**Project**: `~/cot-land/pkg` — package registry written in Cot
**Found**: 2026-03-01

---

## 1. Module-Level `var`/`const` String Values Are Empty at Runtime — FIXED

**Priority**: HIGH — affects any module that uses module-level string constants
**Status**: FIXED (2026-03-01)

### Root Cause

`lowerGlobalVarDecl` in `compiler/frontend/lower.zig` used the init expression for type inference, then created a zero-initialized global without emitting any store code. Integer/float `const` declarations worked because the checker const-folds them (inlined at use sites), but `var` declarations and string `const` declarations were silently zeroed.

### Fix

Go init function pattern: per-file `__cot_init_file_N` functions containing `global_store` operations, with a master `__cot_init_globals` that calls them all. Called at the start of every entry point (main, test runner, bench runner). Works correctly for both single-file and multi-file builds.

- `compiler/frontend/lower.zig` — `GlobalInit` struct, `pending_global_inits` list, `generateGlobalInitsNamed()`, entry point injection
- `compiler/driver.zig` — per-file init functions + master `__cot_init_globals` generation

---

## 2. Module-Level `var` Integer Values Have Garbage Upper Bits — FIXED

**Priority**: MEDIUM
**Status**: FIXED (2026-03-01) — same root cause as Bug #1

---

## 3. String Interpolation — WORKS (was misdiagnosed)

**Priority**: N/A
**Status**: WORKS — the interpolation issues were caused by Bug #1/#2 (uninitialized globals), not by interpolation itself.

---

## Summary

| Bug | Severity | Status | Root Cause |
|-----|----------|--------|------------|
| Module-level string globals empty | HIGH | FIXED | Missing global init stores → `__cot_init_globals` |
| Module-level int globals garbage bits | MEDIUM | FIXED | Same root cause |
| String interpolation broken | N/A | WORKS | Was misdiagnosed — caused by uninitialized globals |
