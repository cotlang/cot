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

## 6. Heap-Allocated Strings Corrupted by `@arcRetain` in List Append — CANNOT REPRODUCE

**Priority**: HIGH — blocks storing `columnString` results in multi-field structs inside Lists
**Status**: CANNOT REPRODUCE (2026-03-02)
**Found**: 2026-03-02

### Investigation

Thorough testing with synthetic `alloc(0, n)` + `memcpy` + `@string(buf, n)` strings in multi-field structs in Lists — all pass. The original root cause analysis was incorrect:

1. **`@arcRetain` does NOT walk struct fields.** `couldBeARC(Entry)` returns false for struct types. `@arcRetain(value)` in `List.append` is a no-op for structs and strings.
2. **`alloc(0, n)` DOES create proper ARC-managed buffers.** `generateAlloc` in `arc_native.zig` allocates `header_size + user_size`, writes a 32-byte ARC header (with `ARC_HEAP_MAGIC`), and returns `ptr + HEADER_SIZE`.
3. **Likely already fixed** by the ARC_HEAP_MAGIC guard commit (which made retain/release on non-heap pointers into safe no-ops).

The bug may have been caused by the `?Struct` corruption in Bug #5 (the pkg registry likely used optional struct returns from SQLite query functions), or by a since-fixed ARC pointer corruption issue. If the issue recurs with actual SQLite, it may be a C FFI calling convention issue rather than ARC.

---

## Summary

| Bug | Severity | Status | Root Cause |
|-----|----------|--------|------------|
| Module-level string globals empty | HIGH | FIXED | Missing global init stores → `__cot_init_globals` |
| Module-level int globals garbage bits | MEDIUM | FIXED | Same root cause |
| String interpolation broken | N/A | WORKS | Was misdiagnosed — caused by uninitialized globals |
| Flat symbol namespace collisions | HIGH | FIXED | Go LinkFuncName: module-qualified IR names (`module.funcName`) |
| Optional struct return corrupts fields | HIGH | FIXED | `convertStoreLocal` skipped `OpMove` for single-field structs (8 bytes) |
| Heap strings corrupted by @arcRetain | HIGH | CANNOT REPRODUCE | Original analysis was wrong; likely fixed by ARC_HEAP_MAGIC or Bug #5 fix |
