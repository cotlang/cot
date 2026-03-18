# ARC Architecture Audit: Cot vs Swift

**Date:** 2026-03-18
**Purpose:** Identify fundamental design gaps in Cot's ARC implementation compared to Swift's proven architecture. Every individual crash we've fixed traces back to these architectural gaps.

---

## Root Cause: ManagedValue Defined But Never Used

Cot's `arc_insertion.zig` defines `ManagedValue` (line 258) and `Scope` (line 307), matching Swift's `ManagedValue.h` and `Cleanup.h`. But the lowerer (`lower.zig`) never uses them. Instead, it works with raw `ir.NodeIndex` values and guesses ownership by pattern-matching AST node types:

```zig
// This pattern is repeated at 6+ sites in lower.zig:
const is_owned = if (value_expr) |e| (e == .new_expr or e == .call) else false;
```

Swift's equivalent: every expression returns a `ManagedValue` that carries `(SILValue, +1_or_+0, CleanupHandle)`. No guessing needed.

**This single gap causes ALL the ARC bugs we've encountered.**

---

## Gap 1: No Per-Value Ownership Tracking (CRITICAL)

**Swift:** `ManagedValue` (ManagedValue.h:40-456) pairs every value with ownership status (+1/+0) and an optional cleanup handle. Every expression-lowering function returns `ManagedValue`.

**Cot:** Expression lowering returns raw `ir.NodeIndex`. Ownership is inferred by checking AST expression type at every use site. Complex expressions (if-expr, switch, orelse) produce values whose ownership is ambiguous.

**Fix:** Integrate `ManagedValue` into the lowering pipeline. Every `lowerExprNode` returns `ManagedValue` instead of `NodeIndex`.

---

## Gap 2: Type Classification Inconsistency (HIGH)

**Swift:** `TypeLowering.cpp` classifies types as trivial (no ARC) or non-trivial. Single source of truth.

**Cot:** Two inconsistent checks:
- `isTrivial(pointer) = true` (types.zig:431) — says pointers are trivial
- `couldBeARC(managed_pointer) = true` (types.zig:470) — says managed pointers need ARC
- `needsARC = !isTrivial` returns FALSE for managed pointers

**Fix:** `isTrivial` should return `false` for `pointer` types where `.managed == true`. Unify `needsARC` and `couldBeARC`.

---

## Gap 3: No Move/Borrow Semantics (HIGH)

**Swift:** Three operations: `copy_value` (retain), `move_value` (transfer), `borrow` (temporary). `ManagedValue.forward()` implements move by disabling cleanup.

**Cot:** Only retain + release. The `disableForLocal` at return only works for ident expressions. Returning `self.field`, `arr[i]`, or complex expressions doesn't disable cleanup → double-free or leak.

**Fix:** Return should operate on `ManagedValue`. If +1, forward cleanup. If +0, retain to produce +1.

---

## Gap 4: Struct Copy/Destroy Doesn't Walk Fields (CRITICAL)

**Swift:** `LoadableAggTypeLowering` (TypeLowering.cpp:1286) iterates fields on copy/destroy. `retain_value` recursively retains all non-trivial fields. `destroy_value` recursively releases them.

**Cot:** Struct copy is bitwise memcpy. Struct destroy only calls `deinit()` if one exists (via `scope_destroy`). Structs without explicit `deinit()` NEVER release their managed pointer fields.

**Fix:** Auto-synthesize destructors for all structs with non-trivial fields (already planned in `emitPendingAutoDeinits` but depends on checker detection).

---

## Gap 5: No Parameter Ownership Convention (MEDIUM)

**Swift:** Parameters are `@guaranteed` (borrowed, +0) by default. Callees that need to keep a reference must retain. Parameters don't get cleanup entries.

**Cot:** Parameters have no cleanup entry (implicitly +0). But the `hasCleanupForLocal` check incorrectly uses this to decide whether to retain — it returns false for parameters, causing values loaded from parameters to skip retain when stored in longer-lived locations.

**Fix:** Define convention: all params are +0. When storing a param value into a field or returned struct, always retain.

---

## Gap 6: Optional ARC Excluded From Init/Assignment (HIGH)

**Swift:** Optional destruction checks tag, conditionally destroys payload via `LoadableEnumTypeLowering`. Optional init/assignment follows the same pattern as all non-trivial types.

**Cot:** Optional destruction IS implemented (cleanup unwrap at lower.zig:2223). But optional vars are EXCLUDED from ARC registration at init (line 2517: `!= .optional`) and skipped in assignment. Optional reassignment (`x = new_value`) leaks the old value.

**Fix:** Remove the optional exclusion. Handle optional init/assignment with tag-aware retain/release (unwrap old, retain new, store new, conditionally release old).

---

## Gap 7: Wasm Has Zero ARC (DESIGN)

All ARC guarded by `!self.target.isWasm()`. Wasm uses bump allocator — nothing is ever freed. This is a known trade-off for 0.3 but must be addressed for production use.

---

## Gap 8: ARC Runtime Matches Swift (OK)

`retain`/`release` in `arc_native.zig` follow Swift's `_swift_retain_`/`_swift_release_` pattern. Magic validation, null check, range check, immortal check, atomic decrement — all correct. The runtime is sound; the issue is in WHEN retain/release are emitted.

---

## Fix Priority for Self-Hosting

| Priority | Gap | Impact | Status |
|----------|-----|--------|--------|
| 1 | Fix `isTrivial` for managed pointers (#2) | Immediate | DONE (59d31e0) |
| 2 | Remove optional exclusion from ARC (#6) | Fixes optional leaks/crashes | DONE (89c6ff1) |
| 3 | Auto-synthesize destructors for ARC structs (#4) | Fixes field leaks | DONE (ac39faa) |
| 4 | Integrate ManagedValue into lowering (#1) | Eliminates all heuristic bugs | DONE (9b3b1a1) |
| 5 | Define parameter ownership convention (#5) | Prevents stored-param crashes | DONE (via ManagedValue) |
| 6 | Implement move semantics for return (#3) | Fixes non-ident return bugs | DONE (fd36a74) |

**The ManagedValue integration (#4) is the correct long-term fix.** It eliminates ALL heuristic-based ownership guessing. But it's a major refactor — every `lowerExprNode` call changes signature.

For self-hosting in the short term, fixes #1-#3 (type classification, optional ARC, auto-deinit) would resolve the remaining crashes without the full ManagedValue refactor.

---

## Reference Files

| Swift | Cot | Purpose |
|-------|-----|---------|
| `ManagedValue.h:40-456` | `arc_insertion.zig:258` (unused) | Per-value ownership tracking |
| `Cleanup.h:85-317` | `arc_insertion.zig:1-200` | Cleanup stack |
| `TypeLowering.cpp:1286` | `lower.zig:2441-2570` | Type-aware copy/destroy |
| `SILGenExpr.cpp:70-109` | `lower.zig` (ad-hoc) | Expression ownership |
| `HeapObject.cpp:548-551` | `arc_native.zig:380-530` | Runtime retain/release |
