# ARC Full Audit: Cot vs Swift — 12 Gaps Found

**Date:** 2026-03-21
**Audited by:** Line-by-line comparison against Swift SILGen
**Status:** Complete. 10 gaps fixed, 2 documented (convention + WasmGC by design).

---

## Summary

Cot's ARC implementation has the right architecture (ManagedValue, CleanupStack, emitCopyValue/emitDestroyValue) ported from Swift's SILGen. But 12 gaps remain where the Zig compiler diverges from Swift's behavior.

---

## Gaps by Priority

### P0 — Causes crashes/corruption in real code

**Gap 2: `isTrivial` says all structs are trivial — FIXED**
- Fix: Recursive field check for structs and unions
- Commit: 4211427

**Gap 1: `couldBeARC` missing tuple/union/error_union/list/map/slice — FIXED**
- Fix: Extended for all aggregate types (union, tuple, error_union, list, map, future, array, slice, distinct)
- Commit: 4211427

**Gap 13 (NEW): Callee-side retain used IR type instead of declared type — FIXED**
- `lowerReturn` now uses `fb.return_type` for `emitCopyValue`, matching Swift's `ensurePlusOne`
- Commit: 4211427

### P1 — Causes memory leaks or UAF in specific patterns

**Gap 5: `emitCopyValue`/`emitDestroyValue` missing union types — FIXED**
- Tag-conditional retain/release for variants with ARC payloads
- Commit: cbe0627

**Gap 6: `emitCopyValue`/`emitDestroyValue` missing tuple types — FIXED**
- Element-wise copy/destroy, destroy in reverse order (LIFO)
- Commit: cbe0627

**Gap 9: `?*T` local var init doesn't register ARC cleanup — FIXED**
- Compound optional locals with managed payloads now get cleanup
- Commit: 735ef69

**Gap 10: Destructured tuple bindings skip ARC — FIXED**
- Destructured elements get ARC copy + cleanup for managed types
- Commit: 735ef69

**Gap 4: SRET return path doesn't retain ARC fields — FIXED**
- SRET copies now retain ARC fields before word-by-word copy
- Commit: 0f0577f

### P2 — Latent / narrow trigger

**Gap 7: Struct field assign missing ARC when struct has no cleanup — FIXED**
- Removed `hasCleanupForLocal` guard, uses field type only
- Commit: 0f0577f

**Gap 12: Nested struct destroy skips inner structs — FIXED**
- Fixed automatically by Gap 2 (`isTrivial` fix)
- Commit: 4211427

### P3 — Design gaps / future

**Gap 3: No ARC cleanup for function parameters — DOCUMENTED**
- Cot uses +0 (borrowed/guaranteed) parameter convention implicitly
- All parameters are borrowed — callee does NOT release at scope exit
- Swift equivalent: `@guaranteed` parameter convention
- Field assignment on parameters correctly does retain/release (Gap 7 fix)
- Status: Convention documented, no code change needed

**Gap 8: `managedFromLowered`/`lowerExprManaged` only check new/call — DOCUMENTED**
- +1 producers in Cot: `.new_expr` and `.call` returning non-trivial types
- Future expression kinds that produce +1 must be added to both functions
- Swift equivalent: `ResultConvention::Owned` on function types
- Status: Invariant documented

**Gap 11: Wasm skips all ARC — BY DESIGN (not a gap)**
- Wasm target uses WasmGC (garbage collected by runtime), not ARC
- Reference: Kotlin/Wasm compiler (`references/kotlin/wasm/`)
- All `if (self.target.isWasm()) return;` guards are correct
- Status: NOT APPLICABLE

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
