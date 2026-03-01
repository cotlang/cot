# Dylib Mode: Startup Crash — Wrong Branch Targets

## Status: FIXED (Mar 2, 2026)

## Bug Summary

Cotty crashes on launch with `EXC_BAD_INSTRUCTION` in `cotty_terminal_lock` → `std.string.charAt`. The compiled code branches to the wrong function because **method call names are resolved with the wrong module prefix** in `lower.zig`, causing `func_index_map` lookup misses in `ssa_to_clif.zig`, which silently falls back to function index 0.

## Root Cause (Confirmed)

**File:** `compiler/frontend/lower.zig`, function `resolveCallName`

When code in module `ffi` calls `terminal_mutex.lock()` (a method defined in `std/thread.cot`), the lowerer:
1. Gets `MethodInfo.func_name = "Mutex_lock"` from the TypeRegistry
2. Calls `resolveCallName("Mutex_lock")`
3. `resolveCallName` does `self.chk.scope.lookup("Mutex_lock")` in the **ffi** scope
4. The lookup **fails** — method symbols aren't imported into the calling module's scope
5. Falls through to `self.qualifyName("Mutex_lock")` which returns `"ffi.Mutex_lock"`
6. But the actual function is defined as `"std.thread.Mutex_lock"` in `func_index_map`

Then in **`compiler/codegen/native/ssa_to_clif.zig`**:
```zig
const func_idx = self.func_index_map.get(name) orelse 0;
```
`func_index_map.get("ffi.Mutex_lock")` returns `null`, so it silently uses index 0 — which is `std.string.charAt`. The `bl` instruction branches to the completely wrong function.

## The Fix (Applied)

### Fix 1: `resolveMethodName` in `lower.zig`

Added `resolveMethodName(type_name, func_name)` helper that uses a two-strategy approach:

1. **Strategy 1**: Look up the TYPE name in scope to find its defining module. Works for user-defined types (structs, enums) imported from other modules (e.g., `Mutex` → `std.thread`).

2. **Strategy 2**: If type lookup fails (e.g., primitive types like `i64`), look up the FUNCTION synth name in scope. Works for trait impls on primitive types (e.g., `i64_hash` from `impl Hashable for i64` in `std/map.cot`).

All method call sites updated from `resolveCallName(method_info.func_name)` to `resolveMethodName(type_name, method_info.func_name)`:
- `lowerMethodCall` — regular method dispatch
- `maybeRegisterScopeDestroy` — deinit method for scope cleanup
- `lowerNewExpr` — init() constructor sugar

### Fix 2: `orelse 0` → compile error in `ssa_to_clif.zig`

Changed the silent fallback to a fatal error so this class of bug is immediately visible:
```zig
const func_idx = self.func_index_map.get(name) orelse {
    std.debug.print("FATAL: func_index_map miss for '{s}' — no such function in IR\n", .{name});
    return error.FunctionNotFound;
};
```

### Test coverage

Added `test/cases/module_qualify/main.cot` test "cross-module method calls resolve correctly" — calls methods on a struct imported from another module.
