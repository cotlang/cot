# Bug: lowerAssign Double-Evaluates RHS for ARC Managed Locals

**Date:** 2026-03-18
**Status:** Diagnosed, fix needs Swift audit verification
**Reference:** Swift SILGen `emitAssignToLValue` (SILGenLValue.cpp:5941-5993)

---

## Bug

`lowerAssign` in `compiler/frontend/lower.zig:3153-3220` evaluates `assign.value` twice for managed pointer locals:

1. **Line 3165:** `const value_node = try self.lowerExprNode(assign.value)` — first evaluation
2. **Line 3215:** `const managed = try self.lowerExprManaged(assign.value)` — second evaluation

`lowerExprManaged` calls `lowerExprNode` internally (line 4541), causing the RHS expression to be lowered twice. For side-effecting expressions like `newScope()`, this generates two function calls and two heap allocations. The first result is leaked.

## Reproduction

```cot
@safe
struct Scope {
    parent: ?*Scope,
    value: i64,
}
fn newScope(parent: ?*Scope) *Scope {
    return new Scope { parent: parent, value: 0 }
}
struct Checker {
    scope: *Scope,
}
impl Checker {
    fn pushScope() void {
        self.scope = newScope(self.scope)  // newScope called TWICE
    }
}
```

**Evidence:** Disassembly of selfcot's `Checker_pushScope` shows two `bl newScope` calls.

## Swift Reference

Swift prevents double-evaluation through **move-only `RValue` semantics**:

```cpp
// SILGenLValue.cpp:5965 — RHS evaluated once, consumed by move
RValue srcValue = std::move(src).getAsRValue(*this);

// RValue.h:100-101 — Copy is deleted
RValue(const RValue &) = delete;
RValue &operator=(const RValue &) = delete;
```

**Swift's assignment sequence** (TypeLowering.cpp:1213-1216):
1. Evaluate RHS → `RValue` (consumed, cannot be reused)
2. `ensurePlusOne()` → `ManagedValue +1`
3. `assignInto()` → `emitSemanticStore()`
4. Load old value from address
5. Store new value
6. Release old value

The `RValue` can only be consumed ONCE. After move, source is marked `Used`.

## Fix (Per Swift Pattern)

The ARC managed path at line 3210-3220 should use the already-lowered `value_node` from line 3165, not re-evaluate via `lowerExprManaged`. Determine ownership from the expression kind (call/new = +1, ident/field = +0).

```zig
// BEFORE (double evaluation):
const value_node = try self.lowerExprNode(assign.value);     // eval 1
const managed = try self.lowerExprManaged(assign.value);     // eval 2

// AFTER (single evaluation, Swift pattern):
const value_node = try self.lowerExprNode(assign.value);     // eval 1 only
const is_owned = (expr == .new_expr or expr == .call);
const managed = if (is_owned)
    ManagedValue.forOwned(value_node, ...)
else
    ManagedValue.forTrivial(value_node, ...);
```

## Impact

- Memory leak: first `newScope()` result is never released
- With old ARC refcount (2 logical), leaked objects survived longer, masking bugs
- With corrected refcount (1 logical), leaked objects may be freed incorrectly
- This is NOT the root cause of the selfcot scope corruption (that persists after this fix)
