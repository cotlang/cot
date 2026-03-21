# ARC Swift Parity — Deep Investigation

**Date:** 2026-03-21
**Status:** In Progress
**Root cause:** `@arcRetain` is a no-op for structs containing managed pointers

---

## The Bug

Cotty's `Workspace.selected_index` returns garbage (`12884901891`) after `addTerminalTab()`. The `Tab` struct has `surface: *Surface` (managed pointer). When `List(Tab).append(tab)` copies the tab into the list, `@arcRetain(tab)` does nothing because `couldBeARC(Tab)` returns `false`. But `Tab_deinit` at scope exit releases `tab.surface`. One release, no retain = use-after-free → heap corruption.

## Why Swift Doesn't Have This Bug

Swift's `Array.append` calls `initializeWithCopy` which recursively retains every class reference field inside a struct value. Cot's `@arcRetain` only handles direct pointers.

---

## Phase 1: Fix `couldBeARC` for structs with managed fields — COMPLETE

**File:** `compiler/frontend/types.zig:487-491`

`couldBeARC` currently returns `true` only for managed pointers and optional managed pointers. It returns `false` for struct types, even if they contain managed pointer fields.

**Fix:** Add a check: if the type is `.struct_type`, scan its fields. If any field has `couldBeARC(field.type_idx) == true`, return `true`.

**Swift reference:** `TypeLowering.h` — `isTrivial()` returns false for structs containing class references. The retain/release path is taken for all non-trivial types.

**Impact:** `@arcRetain(tab)` in `List.append` will trigger `emitCopyValue` which recursively retains each managed field. `Tab_deinit` at scope exit releases them symmetrically.

---

## Phase 2: Make `emitCopyValue` recursive for structs — ALREADY IMPLEMENTED

**File:** `compiler/frontend/lower.zig:4441+`

Once `couldBeARC(Tab)` returns `true`, `emitCopyValue(tab)` will be called. Currently `emitCopyValue` calls `retain(value)` — but `retain` expects a heap pointer with an ARC header. A `Tab` struct on the stack has no ARC header.

**Fix:** `emitCopyValue` for struct types must iterate fields and recursively call `emitCopyValue` on each managed field, extracting field values via loads. This matches Swift's `initializeWithCopy` which is memberwise.

**Swift reference:** `SILGenExpr.cpp` — `emitRValueForDecl` + `ManagedValue::copy` does memberwise retain for aggregate types.

---

## Phase 3: Make `emitDestroyValue` recursive for structs — ALREADY IMPLEMENTED

**File:** `compiler/frontend/lower.zig:4517+`

Symmetrically, `emitDestroyValue(tab)` must iterate struct fields and release each managed field. This may already work if `Tab_deinit` (auto-synthesized) handles it, but verify there's no double-release when both `emitDestroyValue` AND `scope_destroy` fire.

**Swift reference:** `TypeLowering.h` — `emitDestroyValue` for aggregate types calls `destroy_value` on each non-trivial field.

**Risk:** If both `emitDestroyValue` (from ARC cleanup) AND `scope_destroy` (from auto-deinit) fire for the same local, fields get released twice. Must ensure only ONE cleanup path exists per local.

---

## Phase 4: Fix `storeCompoundOptFieldPtr` missing retain — PENDING

**File:** `compiler/frontend/lower.zig:376-410`

When `self.focused_surface = s` assigns a `?*Surface` (optional managed pointer), the `storeCompoundOptFieldPtr` function stores the tag and value but emits NO retain or release. Swift's `store [assign]` for optional class references does `retain new → store → release old`.

**Fix:** In `storeCompoundOptFieldPtr`, if the optional wraps a managed pointer:
1. Load old value
2. Retain new value (`emitCopyValue`)
3. Store new value
4. Release old value (`emitDestroyValue`)

---

## Phase 5: Reproduce with minimal test case — COMPLETE (passes with fix)

Create a test that fails on native but passes on Wasm (Wasm has no ARC):

```cot
struct Payload { value: i64 }
struct Inner { ptr: *Payload }
struct Container { items: List(Inner), selected: i64 }

fn createAndUse() void {
    var c = Container { items: .{}, selected: -1 }
    var p = new Payload { value: 42 }
    c.items.append(Inner { ptr: p })
    c.selected = 0
    @assertEq(c.selected, 0)
    @assertEq(c.items.get(0).ptr.value, 42)
}

test "struct with managed ptr in list" {
    createAndUse()
}
```

---

## Phase 6: Verify refcount accounting end-to-end — PENDING

After Phases 1-4, trace the full lifecycle of a `*Surface` through:
1. `Surface.initTerminal()` — `new Surface { ... }` creates with refcount 1
2. `addTerminalSurface()` — `self.surfaces.append(s)` — should retain (+1 = 2)
3. `addTerminalTab()` — `Tab { surface: s }` → `self.tabs.append(tab)` — should retain (+1 = 3)
4. `addTerminalTab()` exit — `tab` local deinit releases `tab.surface` (-1 = 2)
5. `addTerminalTab()` exit — `s` local cleanup releases `s` (-1 = 1)
6. Final state: Surface refcount = 1 (owned by `surfaces` list entry via `tabs` list)

Verify each step produces the correct retain/release count.

---

## Implementation Order

```
Phase 1 (couldBeARC)        ← core fix, enables all others
Phase 2 (emitCopyValue)     ← makes retain work for structs
Phase 3 (emitDestroyValue)  ← verify no double-release
Phase 5 (test case)         ← verify fix works
Phase 4 (optional fields)   ← separate issue, fix after core
Phase 6 (refcount audit)    ← final verification
```

## Key Files

| File | What to change |
|------|---------------|
| `compiler/frontend/types.zig:487` | `couldBeARC` — add struct field scan |
| `compiler/frontend/lower.zig:4441` | `emitCopyValue` — recursive for structs |
| `compiler/frontend/lower.zig:4517` | `emitDestroyValue` — verify recursive |
| `compiler/frontend/lower.zig:376` | `storeCompoundOptFieldPtr` — add retain/release |
| `stdlib/list.cot:50` | `@arcRetain(value)` — automatically fixed by Phase 1 |
