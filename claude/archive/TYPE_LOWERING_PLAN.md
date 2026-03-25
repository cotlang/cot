# TypeLowering Implementation Plan: Centralize ARC Dispatch

**Date:** 2026-03-19
**Reference:** Swift TypeLowering.cpp:1286-1574
**Goal:** Replace 77 ad-hoc ARC sites with centralized `emitCopyValue`/`emitDestroyValue` dispatch

---

## Current State

77 ARC-specific sites in lower.zig with inline type checks. The centralized `emitCopyValue` and `emitDestroyValue` functions exist but are NOT called from most sites. Each site manually checks `.pointer.managed`, `.optional`, `couldBeARC()`, etc.

## The Problem

Each ad-hoc site independently decides how to handle each type category. When a new type category is added (e.g., recursive struct fields), ALL 77 sites need updating. Missing one site causes silent memory corruption (as demonstrated by the `?*T` scope bug).

## Swift's Solution

One function per operation (`emitCopyValue`, `emitDestroyValue`) that handles ALL type categories. Call sites don't check types — they call the function and trust it.

---

## Implementation: 5 Phases

### Phase 1: Make emitCopyValue/emitDestroyValue Complete

The existing functions handle: trivial, managed pointer, optional managed pointer.

Add handling for:
- **Struct with non-trivial fields** — iterate fields, recursively call emitCopyValue/emitDestroyValue on each non-trivial field
- **String** — no ARC needed (confirm and document)
- **Error union** — check tag, copy/destroy elem if success

### Phase 2: Replace Struct Init Field Retain Sites (6 sites)

Currently 6 sites in `lowerNewExpr`, `lowerStructInitExpr`, `lowerStructInit` that manually check `.pointer.managed` and `.optional` for field init retain.

Replace each with:
```zig
if (!self.type_reg.isTrivial(struct_field.type_idx) and !self.target.isWasm()) {
    _ = try self.emitCopyValue(fb, value_node, struct_field.type_idx, span);
}
```

**Sites:**
- lowerStructInit field init (~line 2804)
- lowerStructInit default field (~line 2866)
- lowerStructInitExpr field init (~line 5980)
- lowerStructInitExpr default field (~line 5982)
- lowerNewExpr field init (~line 6344)
- lowerNewExpr default field (~line 6418)

### Phase 3: Replace Assignment Retain/Release Sites (15 sites)

Currently 15 sites in `lowerAssign` and `lowerFieldAssign` that manually implement retain-before-release.

Replace the pattern:
```zig
// OLD: inline type checking
if (.pointer and .pointer.managed) {
    old = load(); retain(new); store(new); release(old);
}
```

With:
```zig
// NEW: centralized dispatch
if (!self.type_reg.isTrivial(type_idx) and !self.target.isWasm()) {
    const old = load();
    _ = try self.emitCopyValue(fb, new_value, type_idx, span); // retain new
    store(new_value);
    try self.emitDestroyValue(fb, old, type_idx, span); // release old
}
```

**Sites:**
- lowerAssign ident — optional managed (~line 3187)
- lowerAssign ident — direct managed (~line 3215)
- lowerAssign deref — managed (~line 3322)
- lowerFieldAssign pointer base — managed (~line 3403)
- lowerFieldAssign local base — managed (~line 3448)
- lowerFieldAssign global base — managed (~line 3471)
- lowerFieldAssign nested — managed (~line 3507)
- lowerIndexAssign — managed element (~line 3691)

### Phase 4: Replace Cleanup Emission Sites (3 sites)

Currently 3 sites in `emitCleanupsImpl` and `emitFieldReleases` that manually handle release.

Replace with `emitDestroyValue` calls:

**Sites:**
- emitCleanupsImpl release path (~line 2259)
- emitFieldReleases direct pointer (~line 2103)
- emitFieldReleases optional pointer (~line 2112)
- emitPendingAutoDeinits direct pointer (~line 2163)
- emitPendingAutoDeinits optional pointer (~line 2171)

### Phase 5: Replace Return Value Retain Sites (2 sites)

Currently 2 sites in `lowerReturn` that manually handle return value retain.

Replace with `emitCopyValue` for +0 return values:

**Sites:**
- lowerReturn direct managed (~line 1902)
- lowerReturn optional managed (~line 1918)

---

## Testing Strategy

Each phase gets its own test:

1. **Phase 1 test:** Struct with mixed trivial/non-trivial fields — verify copy and destroy
2. **Phase 2 test:** `new Outer { inner: managed_ptr }` — verify field retain (EXISTING: return_managed_field.cot)
3. **Phase 3 test:** `x = makeBox(42)` reassignment — verify retain-before-release (EXISTING: assign_double_eval.cot)
4. **Phase 4 test:** Scope exit releases managed fields — verify cleanup (EXISTING: arc.cot)
5. **Phase 5 test:** Return managed field from method — verify return retain (EXISTING: return_nested_struct.cot)
6. **Selfcot test:** Build selfcot, compile 8+ frontend files
7. **Full suite:** 83/83 pass

---

## Execution Order

Phase 1 first — complete the centralized functions. Then phases 2-5 can proceed in any order since each replaces ad-hoc code with calls to the centralized functions. Run tests after each phase.

## Risk

The main risk is changing working code. Each phase must be verified against the existing test suite AND selfcot before proceeding to the next. If a phase breaks tests, stop and investigate before continuing.
