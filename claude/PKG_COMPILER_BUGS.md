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

## 4. Flat Symbol Namespace — Import Name Collisions

**Priority**: HIGH — blocks use of any two stdlib modules with same-named functions
**Status**: FIXED

### Description

In multi-file builds (`cot build`), all exported symbols from all transitively imported modules share a single flat namespace. If two modules export a function with the same name and signature, one silently shadows the other.

### Example

`std/json` exports `parse(input: string) i64` and `std/semver` exports `parse(s: string) i64`. When both are transitively imported (even in separate files), `std/semver`'s `parse` shadows `std/json`'s `parse` globally. JSON parsing silently fails (returns 0) because the semver parser is called instead.

### Impact

Cannot use `std/semver` and `std/json` in the same project. Any two stdlib modules with same-named functions will collide. Workaround: manual reimplementation of one module's functionality to avoid the import.

### Suggested Fix

Per-module namespacing or qualified imports (e.g., `semver.parse()` vs `json.parse()`).

---

## 5. Optional Struct Return (`?Struct`) Corrupts Field Values — FIXED

**Priority**: HIGH — blocks any function returning `?Struct`
**Status**: FIXED (2026-03-02)

### Root Cause

In `compiler/frontend/ssa_builder.zig` `convertStoreLocal`, when unwrapping a compound optional `?Struct`, `convertFieldLocal` returns a VOID-typed address (an `off_ptr`). The code checked the target local's type to decide whether to use `OpMove` (bulk copy) or scalar store, but the condition was `local_size > 8` — excluding single-field structs (exactly 8 bytes). For these, the raw memory address was stored as the scalar value instead of being dereferenced via `OpMove`.

### Fix

Remove the `local_size > 8` check from the VOID-typed address fallback path in `convertStoreLocal`, `convertStoreGlobal`, and `convertStoreLocalField`. All struct/tuple/union types need `OpMove` when the source is a VOID-typed address, regardless of size.

---

## 6. Heap-Allocated Strings Corrupted in Multi-Field Structs in List — FIXED

**Priority**: HIGH — blocks reading 2+ dependencies from SQLite
**Status**: FIXED (2026-03-02) — same root cause as Bug #5
**Found**: 2026-03-02

### Root Cause

Same as Bug #5. `convertStoreLocal` in `ssa_builder.zig` skipped `OpMove` for struct types exactly 8 bytes (single-field structs), storing the raw memory address instead of dereferencing it. This affected `?Dependency` returns where `Dependency` (2 string fields = 16 bytes) was extracted from compound optionals — the struct field stores via `convertStoreLocalField` had the same `local_size > 8` guard.

The symptom appeared as SQLite-specific because `columnString` returns heap-allocated strings stored into `Dependency` struct fields, then appended to a `List(Dependency)`. The struct field store corruption only manifested when the struct was part of a compound optional unwrap path.

### Fix

Remove `local_size > 8` check from `convertStoreLocal`, `convertStoreGlobal`, and `convertStoreLocalField` — all struct/tuple/union types need `OpMove` when the source is a VOID-typed address, regardless of size. Same commit as Bug #5 fix.

---

## Summary

| Bug | Severity | Status | Root Cause |
|-----|----------|--------|------------|
| Module-level string globals empty | HIGH | FIXED | Missing global init stores → `__cot_init_globals` |
| Module-level int globals garbage bits | MEDIUM | FIXED | Same root cause |
| String interpolation broken | N/A | WORKS | Was misdiagnosed — caused by uninitialized globals |
| Flat symbol namespace collisions | HIGH | FIXED | Go LinkFuncName: module-qualified IR names (`module.funcName`) |
| Optional struct return corrupts fields | HIGH | FIXED | `convertStoreLocal` skipped `OpMove` for single-field structs (8 bytes) |
| Heap strings corrupted in struct+List | HIGH | FIXED | Same as Bug #5 — `convertStoreLocal/Global/Field` skipped `OpMove` for 8-byte structs |
