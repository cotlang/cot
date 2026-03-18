# Shape Stenciling Audit: Cot vs Go's Dictionary-Based Generics

**Date:** 2026-03-18
**Status:** Stenciling disabled (line 8856: `if (false and ...`). 6 gaps identified.

---

## Architecture Comparison

### Go's Approach (cmd/compile/internal/noder/reader.go)

Go passes a **single dictionary pointer** (`*[N]uintptr`) as the first argument to every shaped function. The dictionary contains:
- Type parameter method expression function pointers (line 1448)
- Subdictionaries for nested generic calls (line 1458)
- Runtime type descriptors (line 1469)

Every concrete instantiation gets a **wrapper function** that:
1. Takes the original concrete params (no dict in signature)
2. Prepends `&dict` (address of its static dictionary global)
3. Tail-calls the shaped function (reader.go lines 1377-1396)

Call sites call the concrete name → wrapper → shaped function with dict.

### Cot's Approach (compiler/frontend/lower.zig)

Cot passes **individual function pointer parameters** (`__dict_0`, `__dict_1`) before regular params. No single dictionary struct. Inside the body, entries are read via `emitLoadLocal(params[di])` (lines 4954, 9397).

---

## 6 Identified Gaps

### Gap 1: `lowerCall` never passes dict args (CRASH-CAUSING)

**Go** (reader.go:2389-2393): `exprCall` prepends dict for instanced function calls.
**Cot** (lower.zig:8134-8211): `lowerCall` calls `emitCall(concrete_name, args)` without consulting `dict_arg_names`.

**Impact:** Function body expects `(dict_0, dict_1, self, key...)` but receives `(self, key...)`. self interpreted as dict_0 → indirect call on data pointer → SIGSEGV.

### Gap 2: `shape_aliases` never resolved at call sites

**Go**: Wrappers handle aliasing transparently.
**Cot** (lower.zig:8888): `shape_aliases.put()` is called but the map is never consumed during call emission. Aliased functions are never lowered → linker error or wrong dispatch.

### Gap 3: No wrapper functions for aliased instances

**Go** (reader.go:1377-1396): Creates thin wrapper per concrete instantiation that tail-calls shaped function with its specific dictionary.
**Cot** (lower.zig:8894): Aliased instances `return` without lowering. No wrapper generated. Call sites targeting aliased names have no function to call.

### Gap 4: Dict lookup uses wrong key in lowerMethodCall

**Cot** (lower.zig:9431): `dict_arg_names.get(method_info.func_name)` — but after shape aliasing, the actual call target is `method_link_name`. Should use the resolved name.

### Gap 5: No subdictionary support

**Go** (reader.go:1458-1467): Dictionary includes subdictionaries for nested generic calls.
**Cot**: No equivalent. Nested generic calls inside stenciled body use hardcoded concrete names.

### Gap 6: Dict dispatch doesn't match by type_param_idx

**Cot** (lower.zig:4952-4953): Matches only by `(kind, op)`, ignoring `type_param_idx`. Two type params with same operation would select wrong entry.

---

## Fix Plan: Go-Style Wrapper Functions (Option A)

The cleanest fix, matching Go's proven architecture:

### Step 1: Generate wrapper functions for aliased instances

In the alias branch (line 8886-8894), instead of `return`, generate a wrapper:

```
fn Map(17;1556)_getOrNull(self, key_ptr, key_len, ...) {
    hash_fn = funcaddr("__cot_dict_string_hash")
    eq_fn = funcaddr("__cot_dict_string_eq")
    return Map(17;5)_getOrNull(hash_fn, eq_fn, self, key_ptr, key_len, ...)
}
```

The wrapper:
- Has the original concrete signature (no dict params)
- Computes dict args from the concrete type's dict helpers
- Tail-calls the canonical stencil function

### Step 2: Generate wrapper for the canonical (first) instance too

The canonical instance has dict params in its body. But callers call it by its concrete name without dict args. Generate a wrapper for it too:

```
// Canonical stencil (internal, with dict params):
fn __stencil_Map_getOrNull(dict_0, dict_1, self, key_ptr, ...) { ... }

// Concrete wrapper (called by user code):
fn Map(17;5)_getOrNull(self, key_ptr, key_len, ...) {
    hash_fn = funcaddr("__cot_dict_i64_hash")  // or whatever K=string needs
    eq_fn = funcaddr("__cot_dict_i64_eq")
    return __stencil_Map_getOrNull(hash_fn, eq_fn, self, key_ptr, ...)
}
```

### Step 3: Remove `dict_arg_names` lookup from call sites

With wrappers handling dict injection, `lowerCall` and `lowerMethodCall` don't need to prepend dict args. They call the concrete name → wrapper → stencil.

### Step 4: Fix type_param_idx matching

In dict dispatch (line 4952), add `type_param_idx` check:
```zig
if (entry.kind == .binary_op and entry.binary_op == op and entry.type_param_idx == target_type_param) {
```

### Step 5: Add subdictionary support (future)

For nested generic calls inside stenciled bodies, pass subdictionaries via the dictionary parameter. This is needed for chains like `List(T).sort` calling `T.compare`.

---

## Effort Estimate

| Step | Description | Effort |
|------|-------------|--------|
| 1 | Wrapper for aliased instances | 2 hours |
| 2 | Wrapper for canonical instance | 1 hour |
| 3 | Remove call-site dict passing | 30 min |
| 4 | Fix type_param_idx matching | 15 min |
| 5 | Subdictionary support | 4 hours (future) |
| **Total** | | **~4 hours** (steps 1-4) |

---

## Reference Files

| File | Lines | Content |
|------|-------|---------|
| `references/go/src/cmd/compile/internal/noder/reader.go` | 157-210 | Go dictionary structure |
| Same | 1377-1396 | Go wrapper function generation |
| Same | 2355-2396 | Go call site dict passing |
| Same | 4034-4063 | Go shaped function signature |
| `compiler/frontend/lower.zig` | 8451-8510 | Cot stencilability analysis |
| Same | 8848-8898 | Cot stencil result handling |
| Same | 8924-8934 | Cot dict param injection (body) |
| Same | 9132-9432 | Cot lowerMethodCall |
| Same | 8007-8211 | Cot lowerCall |
| Same | 4942-4970 | Cot dict dispatch (body) |
