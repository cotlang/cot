# Dylib Mode: Startup Crash — Wrong Branch Targets

## Status: FIXED (Mar 2, 2026)

## Bug Summary

Cotty crashes on launch with `EXC_BAD_INSTRUCTION` in `cotty_terminal_lock` → `std.string.charAt`. The compiled code branches to the wrong function because **method call names are resolved with the wrong module prefix** in `lower.zig`, causing `func_index_map` lookup misses in `ssa_to_clif.zig`, which silently falls back to function index 0.

## Root Cause

**File:** `compiler/frontend/lower.zig`, function `resolveCallName`

When code in module `ffi` calls `terminal_mutex.lock()` (a method defined in `std/thread.cot`), the lowerer:
1. Gets `MethodInfo.func_name = "Mutex_lock"` from the TypeRegistry
2. Calls `resolveCallName("Mutex_lock")`
3. `resolveCallName` does `self.chk.scope.lookup("Mutex_lock")` in the **ffi** scope
4. The lookup **fails** — method symbols aren't imported into the calling module's scope
5. Falls through to `self.qualifyName("Mutex_lock")` which returns `"ffi.Mutex_lock"`
6. But the actual function is defined as `"std.thread.Mutex_lock"` in `func_index_map`

Compounded by transitive imports: `ffi.cot` imports `surface.cot` which imports `std/thread.cot`. The `Thread` type is accessible via struct field types, but its **symbol** is not in `ffi`'s scope because `createFileScope` only imports direct-dependency symbols.

## The Fix

### Go pattern: store defining module with the method

Go's `LinkFuncName` stores the defining package path with each method. We follow the same pattern:

1. **`MethodInfo.source_tree`** (`types.zig`): New field `source_tree: ?*const anyopaque` records the AST pointer of the file that defined the method. Set at all `registerMethod` call sites in `checker.zig`.

2. **`resolveMethodName`** (`lower.zig`): Uses `method_info.source_tree` for direct `tree_module_map` lookup — works regardless of scope visibility (direct or transitive imports). Falls back to type name scope lookup, then function synth name scope lookup.

3. **`orelse 0` → fatal error** (`ssa_to_clif.zig`): The silent fallback to function index 0 is now a compile error with a descriptive message.

### Files changed

| File | Change |
|------|--------|
| `compiler/frontend/types.zig` | `MethodInfo.source_tree` field |
| `compiler/frontend/checker.zig` | Set `.source_tree = self.tree` at all 6 `registerMethod` sites |
| `compiler/frontend/lower.zig` | `resolveMethodName` uses `source_tree` as primary strategy |
| `compiler/codegen/native/ssa_to_clif.zig` | `orelse 0` → `return error.FunctionNotFound` |

### Test coverage

`test/cases/module_qualify/`:
- "module-qualified function names prevent collision" — same-named functions in different modules
- "cross-module new calls correct destructor" — ARC destructor from imported type
- "cross-module method calls resolve correctly" — direct import method calls
- "transitive import method calls resolve correctly" — method calls on types from transitive imports
