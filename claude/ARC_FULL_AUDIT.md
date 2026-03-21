# ARC Full Audit: Cot vs Swift — 12 Gaps Found

**Date:** 2026-03-21
**Audited by:** Line-by-line comparison against Swift SILGen
**Status:** Implementation plan ready

---

## Summary

Cot's ARC implementation has the right architecture (ManagedValue, CleanupStack, emitCopyValue/emitDestroyValue) ported from Swift's SILGen. But 12 gaps remain where the Zig compiler diverges from Swift's behavior.

---

## Gaps by Priority

### P0 — Causes crashes/corruption in real code

**Gap 2: `isTrivial` says all structs are trivial**
- File: `types.zig:462` — `.struct_type => true`
- Swift: `LoadableStructTypeLowering` extends `NonTrivialLoadableTypeLowering` — structs with class refs are non-trivial
- Fix: Recursive field check, same as `couldBeARC`
- Status: PENDING

**Gap 1: `couldBeARC` missing tuple/union/error_union/list/map/slice**
- File: `types.zig:489-501` — only checks `.pointer`, `.optional`, `.struct_type`
- Swift: `isTrivial()` is recursive across ALL aggregate types
- Fix: Add checks for all aggregate kinds
- Status: PENDING

### P1 — Causes memory leaks or UAF in specific patterns

**Gap 5: `emitCopyValue`/`emitDestroyValue` missing union types**
- File: `lower.zig:4464-4535` — no `.union_type` case
- Swift: `LoadableEnumTypeLowering` handles tag-conditional retain/release
- Fix: Add tag-based conditional copy/destroy for union payloads
- Status: PENDING

**Gap 6: `emitCopyValue`/`emitDestroyValue` missing tuple types**
- File: `lower.zig:4464-4535` — no `.tuple` case
- Swift: `LoadableTupleTypeLowering` does element-wise copy/destroy
- Fix: Add element-wise copy/destroy for tuples
- Status: PENDING

**Gap 9: `?*T` local var init doesn't register ARC cleanup**
- File: `lower.zig:2390-2420` — compound optional path skips cleanup
- Swift: Optional values with non-trivial payloads get cleanups
- Fix: Push cleanup after storing compound optional with ARC payload
- Status: PENDING

**Gap 10: Destructured tuple bindings skip ARC**
- File: `lower.zig:2469-2510` — no ARC in `lowerDestructureStmt`
- Swift: Destructuring copies each element and registers cleanups
- Fix: Check `couldBeARC` for each extracted element
- Status: PENDING

**Gap 4: SRET return path doesn't retain ARC fields**
- File: `lower.zig:1833-1879` — word-by-word copy without field retain
- Swift: `ensurePlusOne()` before forwarding to indirect result
- Fix: Emit `emitCopyValue` for non-trivial fields in SRET path
- Status: PENDING

### P2 — Latent / narrow trigger

**Gap 7: Struct field assign missing ARC when struct has no cleanup**
- File: `lower.zig:3318-3338` — guarded by `hasCleanupForLocal`
- Swift: Field assign always does retain/release based on field type
- Fix: Remove the cleanup guard, use field type only
- Status: PENDING

**Gap 12: Nested struct destroy skips inner structs**
- Fixed automatically by Gap 2 (`isTrivial` fix)
- Status: BLOCKED on Gap 2

### P3 — Design gaps / future

**Gap 3: No ARC cleanup for function parameters**
- Currently mitigated by field-assign workaround
- Cot uses +0 parameter convention implicitly
- Status: DEFERRED — document convention

**Gap 8: `managedFromLowered`/`lowerExprManaged` only check new/call**
- Future expressions producing +1 must be manually added
- Status: DEFERRED — document invariant

**Gap 11: Wasm skips all ARC**
- Known limitation — requires Wasm ARC runtime
- Status: DEFERRED — separate project

---

## Implementation Plan

### Phase 1: Fix type predicates (Gaps 1, 2)

**Fix `isTrivial` for structs** (`types.zig:462`):
```zig
.struct_type => |s| {
    for (s.fields) |field| {
        if (!self.isTrivial(field.type_idx)) return false;
    }
    return true;
},
```

**Extend `couldBeARC`** (`types.zig:489`):
Add tuple, union, error_union, list, map, slice, array checks.

**Test:** Run 360 feature tests + Cotty. These are type-level changes that affect ALL ARC decisions.

### Phase 2: Add union/tuple copy/destroy (Gaps 5, 6)

**Union copy/destroy** (`lower.zig`):
- Store to temp, load tag, conditional branch per variant with ARC payload
- Retain/release the payload based on tag match
- Reference: Swift `LoadableEnumTypeLowering`

**Tuple copy/destroy** (`lower.zig`):
- Store to temp, iterate elements, recursively copy/destroy non-trivial elements
- Reference: Swift `LoadableTupleTypeLowering`

### Phase 3: Fix cleanup registration (Gaps 9, 10)

**`?*T` local cleanup** (`lower.zig:2390`):
- After storing compound optional, push cleanup if `couldBeARC(type_idx)`

**Destructured tuple cleanup** (`lower.zig:2469`):
- After extracting each element, check `couldBeARC` and push cleanup

### Phase 4: Fix SRET and field assign (Gaps 4, 7)

**SRET return retain** (`lower.zig:1833`):
- Before word-by-word SRET copy, emit `emitCopyValue` for the whole value
- Or retain individual non-trivial fields

**Field assign without cleanup guard** (`lower.zig:3318`):
- Remove `hasCleanupForLocal` guard, use `couldBeARC(field.type_idx)` only

### Phase 5: Document conventions (Gaps 3, 8)

- Document +0 parameter convention
- Document +1 producers list (new_expr, call)
- Add to CLAUDE.md or ARC reference doc

---

## Key Files

| File | Lines | What to change |
|------|-------|---------------|
| `types.zig` | 462, 489 | `isTrivial` for structs, `couldBeARC` for all aggregates |
| `lower.zig` | 4464-4535 | `emitCopyValue` — add union + tuple |
| `lower.zig` | 4540-4631 | `emitDestroyValue` — add union + tuple |
| `lower.zig` | 2390-2420 | `?*T` local cleanup registration |
| `lower.zig` | 2469-2510 | Destructured tuple ARC |
| `lower.zig` | 1833-1879 | SRET return retain |
| `lower.zig` | 3318-3338 | Field assign cleanup guard |

## Swift References

| Swift File | Lines | What it shows |
|------------|-------|---------------|
| `TypeLowering.cpp` | 1455-1519 | Tuple/Struct/Enum type lowering — recursive trivial check |
| `TypeLowering.h` | 166 | `isTrivial()` interface |
| `SILGenStmt.cpp` | 848-909 | `emitReturnExpr` — `ensurePlusOne` before return |
| `SILGenApply.cpp` | 6257-6296 | Call result ownership from `ResultConvention` |
| `ManagedValue.cpp` | 289-299 | `ensurePlusOne` → `copy` → retain |
| `SILFunctionType.cpp` | 1625-1670 | `DestructureResults` — convention from lowered type |
