# ARC Parity Audit: Cot vs Swift

**Date:** 2026-03-19
**Purpose:** Full gap analysis of Cot's ARC implementation against Swift SILGen.
**Conclusion:** Cot implements ~40% of Swift's ARC. 6 critical gaps identified.

---

## Swift's Architecture

Swift's ARC has three layers:

1. **TypeLowering** (TypeLowering.cpp, 5,638 lines) — type-based dispatch for copy/destroy. Every type has a lowering class that knows how to copy and destroy it recursively.

2. **ManagedValue** (ManagedValue.cpp, 329 lines) — per-value ownership tracking. Every value is +1 (owned, has cleanup) or +0 (borrowed, no cleanup). Conversions between +1 and +0 are explicit.

3. **CleanupStack** (Cleanup.cpp, 560 lines) — scope-based LIFO destruction. Each non-trivial value registers a cleanup that fires at scope exit.

### Swift's Key Principle: Recursive Field Copying

When copying a struct, Swift **destructures it into fields** and **recursively copies each non-trivial field**:

```cpp
// TypeLowering.cpp:1396-1422 — LoadableAggTypeLowering::emitLoweredCopyValue
asImpl().destructureAggregate(
    B, loc, aggValue, false,
    [&](unsigned childIndex, SILValue childValue,
        const TypeLowering &childLowering) {
      if (!childLowering.isTrivial())
        childValue = childLowering.emitLoweredCopyChildValue(
            B, loc, childValue, style);  // RECURSIVE RETAIN
      loweredChildValues.push_back(childValue);
    });
```

This means copying `struct { a: *Node, b: Map(K,V), c: ?*Scope }` would:
- retain `a` (managed pointer)
- recursively copy `b` (aggregate with managed backing buffers)
- conditionally retain inner pointer of `c` (optional managed pointer)

### Swift's Store [assign] Pattern

```cpp
// TypeLowering.cpp:1213-1216 — emitStore for non-trivial types
SILValue old = B.createLoad(loc, addr, LoadOwnershipQualifier::Unqualified);
B.createStore(loc, value, addr, StoreOwnershipQualifier::Unqualified);
B.emitDestroyValueOperation(loc, old);
```

Load old, store new, destroy old. Always in this order. Retain of new value is implicit — the stored value must already be +1.

### Swift's Optional Payload Handling

```cpp
// TypeLowering.cpp:1603+ — LoadableEnumTypeLowering::emitCopyValue
// Switches on enum tag, copies non-trivial payload
switch_enum %optional {
  case .some(%payload): copy_value %payload  // RETAIN inner pointer
  case .none: /* no-op */
}
```

---

## Gap Analysis: 6 Critical Gaps

### Gap 1: No Recursive Struct Field Copy (CRITICAL — ROOT CAUSE OF SELFCOT BUG)

**Swift:** `LoadableAggTypeLowering::destructureAggregate` walks all fields, retains each non-trivial one.

**Cot:** `lowerNewExpr` field init (lower.zig:6244) only checks top-level `.pointer` fields. Misses:
- `?*T` optional pointer fields
- Nested structs containing managed pointers
- Slices/strings containing heap pointers (partially handled separately)

**Impact:** `new Scope { parent: old_scope }` doesn't retain `old_scope` through the `?*Scope` field. When the source is released, the pointed-to scope is freed while the new Scope's `parent` still references it.

**Fix:** Implement Swift's destructureAggregate pattern — for each non-trivial field in a struct init, emit the appropriate retain:
- `.pointer` (managed) → `retain(value)`
- `.optional` containing managed pointer → unwrap tag, conditionally `retain(inner_ptr)`
- `.struct_type` containing non-trivial fields → recursive

### Gap 2: No Type Lowering Hierarchy

**Swift:** 8+ TypeLowering subclasses:
- `TrivialTypeLowering` — i32, f64, etc.
- `ReferenceTypeLowering` — class instances (single retain/release)
- `LoadableStructTypeLowering` — structs with non-trivial fields
- `LoadableEnumTypeLowering` — enums/optionals with non-trivial payloads
- `AddressOnlyTypeLowering` — types that can't be loaded into registers

**Cot:** Flat `isTrivial()` check. No per-type-category copy/destroy dispatch.

**Impact:** Every ARC operation in Cot is ad-hoc — each call site manually checks type categories and emits retain/release. This leads to inconsistencies (some paths retain, others don't).

**Fix:** Create a `TypeLowering` module that given a type index returns:
- `isTrivial() bool` — no ARC needed
- `emitCopy(value) → retained_value` — type-appropriate copy (retain/recursive)
- `emitDestroy(value)` — type-appropriate destroy (release/recursive)

### Gap 3: No Per-Field Cleanup Registration

**Swift:** Each non-trivial variable gets a cleanup registered at declaration:
```cpp
// SILGenProlog.cpp — for each +1 parameter:
if (!lowering.isTrivial())
    enterDestroyCleanup(val);
```

**Cot:** Registers ONE cleanup per struct (`scope_destroy` calling `deinit`). Does NOT register individual cleanups for each non-trivial field.

**Impact:** When a struct goes out of scope, its fields are not individually released. The struct's `deinit` (if any) runs, but fields without explicit release in deinit are leaked.

**Fix:** At variable declaration, if the type is a struct with non-trivial fields, register a cleanup that recursively destroys each non-trivial field (matching `emitDestroyValue`).

### Gap 4: Incomplete Ownership Forwarding on Return

**Swift:** `ManagedValue::forward()` disables cleanup and transfers ownership to the caller. For aggregates, this means all field cleanups are disabled.

**Cot:** `cleanup_stack.disableForLocal(local_idx)` — only disables if the return expr is a simple identifier. Doesn't handle:
- Aggregate returns (returning a struct with multiple non-trivial fields)
- Field access returns (returning `self.scope` — must retain the field)
- Nested expressions (returning `process(x)` where x needs forwarding)

**Fix:** Match Swift's `emitReturnExpr` pattern: evaluate RHS to ManagedValue, forward ALL managed values (not just simple idents).

### Gap 5: Double Evaluation in Assignment

**Swift:** `emitAssignToLValue` evaluates RHS exactly once via move-only `RValue`:
```cpp
// SILGenLValue.cpp:5965
RValue srcValue = std::move(src).getAsRValue(*this);
```
`RValue` copy constructor is deleted. After move, source is marked `Used`.

**Cot:** `lowerAssign` evaluates `assign.value` twice for managed pointer locals:
1. Line 3165: `lowerExprNode(assign.value)` — first evaluation
2. Line 3215: `lowerExprManaged(assign.value)` — second evaluation (FIXED in recent commit but needs Swift audit verification)

Multiple other `lowerExprManaged(assign.value)` call sites remain (lines 3187, 3322, 3403, 3448, 3471, 3507, 3652).

**Fix:** ALL assignment paths should use the already-lowered `value_node` from line 3165. Determine ownership from expression kind, never re-lower.

### Gap 6: No Aggregate Type Expansion

**Swift:** `TypeExpansionKind::DirectChildren` triggers field-by-field processing for ALL aggregate operations (copy, destroy, store, load).

**Cot:** No equivalent. Each operation handles aggregates differently (or not at all).

**Fix:** Long-term — implement TypeExpansionKind equivalent. Short-term — ensure each operation site handles non-trivial fields correctly.

---

## Priority Fix Order

| Priority | Gap | Description | Effort | Unblocks |
|----------|-----|-------------|--------|----------|
| **P0** | Gap 1 | ?*T field retain in struct init (unwrap-then-retain) | Small | Selfcot scope bug |
| **P1** | Gap 5 | Remaining double-eval sites in lowerAssign | Small | Correctness |
| **P2** | Gap 4 | Return value ownership forwarding | Medium | Complex returns |
| **P3** | Gap 3 | Per-field cleanup registration | Medium | Field-level cleanup |
| **P4** | Gap 2 | TypeLowering hierarchy | Large | Systematic ARC |
| **P5** | Gap 6 | Aggregate type expansion | Large | Full parity |

### Immediate Fix: Gap 1 (Unblocks Selfcot)

The `?*T` unwrap-then-retain pattern for struct field init:

```
// For field type ?*T where T is managed:
// 1. Store the compound optional value (tag + pointer)
store compound_optional at field_addr

// 2. Load tag, conditionally retain inner pointer
tag = load_i64(field_addr)
if (tag != 0) {
    inner_ptr = load_i64(field_addr + 8)
    retain(inner_ptr)
}
```

**Reference:** Swift `LoadableEnumTypeLowering::emitCopyValue` (TypeLowering.cpp:1603+)

Apply to:
- `lowerNewExpr` field init (lower.zig:~6244)
- `lowerStructInitExpr` field init (lower.zig:~5860)
- `lowerStructInit` field init (lower.zig:~2752)

---

## Swift Reference Files

| File | Lines | Purpose |
|------|-------|---------|
| `references/swift/lib/SIL/IR/TypeLowering.cpp` | 5,638 | emitCopyValue, emitDestroyValue, struct/enum lowering |
| `references/swift/lib/SILGen/ManagedValue.cpp` | 329 | copy(), forward(), ensurePlusOne() |
| `references/swift/lib/SILGen/Cleanup.cpp` | 560 | CleanupManager, scope-based destruction |
| `references/swift/lib/SILGen/SILGenLValue.cpp` | 5,993 | emitAssignToLValue, emitLoad, emitSemanticStore |
| `references/swift/lib/SILGen/SILGenProlog.cpp` | 1,774 | Parameter ownership, cleanup registration |
| `references/swift/lib/SILGen/SILGenExpr.cpp` | 7,793 | Expression ownership (+1/+0), struct literal init |
| `references/swift/include/swift/SIL/SILGenManagedValue.h` | 347 | ManagedValue class, +1/+0 semantics |

## Cot Reference Files

| File | Lines | Purpose |
|------|-------|---------|
| `compiler/frontend/lower.zig` | ~12,000 | Ad-hoc ARC insertion during lowering |
| `compiler/frontend/arc_insertion.zig` | 444 | CleanupStack, ManagedValue data structures |
| `compiler/codegen/native/arc_native.zig` | ~2,100 | Native ARC runtime (alloc/retain/release) |
| `compiler/codegen/arc.zig` | 1,642 | Wasm ARC runtime |
| `compiler/frontend/types.zig` | ~800 | Type classification (isTrivial, couldBeARC) |
