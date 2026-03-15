# Copy-on-Write Execution Plan for List(T) and Map(K,V)

**Goal:** Enable automatic scope_destroy cleanup for List/Map without corrupting aliased backing buffers.

**Reference:** Swift's Array/Dictionary use ARC on their backing buffers. Assignment increments refcount. deinit/free decrements. Buffer freed when refcount hits 0.

**Status:** Plan ready for implementation.

---

## Background

The selfcot currently uses 4.1GB / 201s to compile scanner.cot (the Zig compiler does it in 25MB / 0.09s). The frontend is efficient (check-only: 9MB / 0.01s). The 4.1GB is entirely from per-function codegen data leaking because there's no automatic collection cleanup.

Previous attempts to add automatic cleanup (scope_destroy calling free/deinit) crashed because copied Lists share backing buffers. `var x: List(int) = .{}; x = other.field` makes x alias other.field's backing buffer. Calling free() at scope exit frees other.field's data.

The fix: use ARC retain/release on backing buffers. Copies increment refcount. Free/deinit decrements refcount. Buffer only freed when last reference drops.

---

## Implementation Steps

### Step 1: Change List.free() to use release() instead of dealloc()

**File:** `stdlib/list.cot`, line 106

```diff
- dealloc(self.items)
+ release(self.items)
```

`release()` is already available as an extern fn in `std/sys`. It decrements the ARC header refcount and only frees when it hits 0. Since `alloc(0, bytes)` creates the buffer with refcount=1, a single `release()` with no prior `retain()` has the same effect as `dealloc()` — backward compatible.

Also add `fn deinit` that calls `self.free()`:
```cot
fn deinit() void {
    self.free()
}
```

This enables scope_destroy to auto-call deinit at scope exit.

### Step 2: Change Map.free() to use release() instead of dealloc()

**File:** `stdlib/map.cot`, lines 423-425

```diff
- dealloc(self.keys)
- dealloc(self.values)
- dealloc(self.states)
+ release(self.keys)
+ release(self.values)
+ release(self.states)
```

Map has THREE backing arrays. All need the same treatment. Also add `fn deinit`.

### Step 3: Change Map.rehash() to use release() instead of dealloc()

**File:** `stdlib/map.cot`, lines 143-147

```diff
- dealloc(old_keys)
- dealloc(old_values)
- dealloc(old_states)
+ release(old_keys)
+ release(old_values)
+ release(old_states)
```

When rehashing, old arrays' refcounts should be decremented, not unconditionally freed. If another Map alias holds a reference, the old arrays stay alive.

### Step 4: Add retain() calls during struct copy in the compiler

**File:** `compiler/frontend/lower.zig`, in `lowerVarStmt()` (around line 2437)

When a variable of struct type is assigned from another variable (copy), AND the struct has a `deinit` or `free` method, emit retain() calls on internal pointer fields.

The approach: after the `emitStoreLocal` that performs the memcpy, check if:
1. The type is a struct with a `deinit()` or `free()` method
2. The source is a copy (identifier, field access, index — not `.{}` or `new` or call)

If both conditions are met, emit `retain()` calls on each i64 field that could be a heap pointer (items/keys/values/states fields).

**Implementation detail:** Rather than a generic "retain all fields" approach, use the simpler pattern of calling `retain()` on the LOCAL's address fields after the copy. For List(T), retain `self.items`. For Map(K,V), retain `self.keys`, `self.values`, `self.states`.

This can be done by emitting a call to a new runtime function `_struct_retain_fields` OR by inlining retain calls for each known field offset.

**Simplest approach:** Teach the lowerer to emit individual `retain()` calls:
```zig
// After struct copy for a type with deinit/free:
if (struct has deinit or free method) {
    // For each field at offset that is an i64 (potential pointer):
    for (struct_type.fields) |field| {
        if (field.type_idx == .list or field.type_idx == .map or ...) {
            const field_val = emitFieldLocal(local_idx, field_idx, field.offset, I64, span);
            emitCall("retain", &[_]{field_val}, false, I64, span);
        }
    }
}
```

### Step 5: Handle struct copy in assignment statements

**File:** `compiler/frontend/lower.zig`, in `lowerAssign()` (around line 3107)

Same logic as Step 4, but for assignment `x = y` where x already exists. Before the assignment:
1. Call `release()` on x's old backing buffer fields
2. Perform the store (memcpy)
3. Call `retain()` on x's new backing buffer fields

This mirrors Swift's `swift_assignWithCopy` which releases old, retains new, then copies.

### Step 6: Handle switch capture bindings

**File:** `compiler/frontend/lower.zig`, in switch union extraction (around line 7353)

When extracting a union payload into a capture variable (`switch (x) { Variant |v| => ... }`), the capture is a copy. If the payload type has deinit/free, emit retain() on its backing buffer fields after the chunk-by-chunk copy.

### Step 7: Handle function parameters

When a function receives a struct by value (copy from caller), the parameter copy needs retain(). When the function returns, the parameter scope cleanup calls release().

**File:** `compiler/frontend/lower.zig`, in parameter setup.

### Step 8: Fix the checker generic method bug

**File:** `compiler/frontend/checker.zig`, line 3758

The `deinit` method on List(T) causes checker errors because generic impl methods are registered in `global_scope` which pollutes the namespace. Fix: register in the declaring file's scope (impl_info.scope), not global_scope.

Note: Previous attempt to change this caused other failures. Need more careful investigation — the lowerer may depend on finding generic methods in global_scope. Solution: register in BOTH scopes, but ensure the file-scope registration doesn't create name conflicts.

### Step 9: Re-enable scope_destroy for List/Map

Once Steps 1-8 are complete:
1. List has `fn deinit` which calls `self.free()` which calls `release(self.items)`
2. Map has `fn deinit` which calls `self.free()` which calls `release()` on all arrays
3. Copies call `retain()` on backing buffers
4. scope_destroy calls `deinit()` at scope exit → decrements refcount → safe

**File:** `compiler/frontend/lower.zig`, `maybeRegisterScopeDestroy()`
- Already checks for `deinit()` method on structs
- Will automatically pick up List/Map deinit once the checker bug is fixed

---

## Risk Assessment

### Low risk (Steps 1-3):
Changing `dealloc → release` in List/Map is backward compatible. For non-aliased buffers (refcount=1), `release()` behaves identically to `dealloc()`. No observable change until retain() is also called.

### Medium risk (Steps 4-7):
Emitting retain() calls during struct copies. Risk: over-retaining (memory leak) or under-retaining (use-after-free). Mitigated by testing with the selfcot as the primary test case.

### High risk (Step 8):
Checker generic method scope registration. Previous attempts caused cascading errors. Needs careful investigation of how the lowerer resolves generic method names.

---

## Verification Plan

After each step:
```bash
zig build test                                    # Compiler internals
cot build self/main.cot -o /tmp/selfcot           # Selfcot builds
/tmp/selfcot build self/test_tiny.cot -o /tmp/t   # Selfcot runs
/usr/bin/time -l /tmp/selfcot build self/frontend/scanner.cot -o /tmp/s  # Memory measurement
```

**Target:** scanner.cot build < 200MB RSS (from current 4.1GB).

---

## Alternative: Arena Allocator (Simpler, Less Elegant)

If COW proves too complex, the arena allocator (`stdlib/arena.cot`) can be used as a simpler solution:

1. Create an Arena per function in `driver.cot:generateAndAddFunc`
2. Pass the Arena to SSABuilder, SSA passes, and generateFunc
3. All per-function allocations use `arena.create()` instead of `alloc()`
4. After codegen, call `arena.deinit()` to free everything at once

This matches the Zig compiler's pattern (per-function ArenaAllocator) and avoids the complexity of COW. But it requires modifying List/Map to accept an optional arena parameter, which changes the API.

**Recommendation:** Try COW first (Steps 1-3 are safe and easy). If Step 4+ proves too complex, fall back to the arena approach.

---

## Dependency Graph

```
Step 1 (list.cot release) ─── no deps
Step 2 (map.cot release) ──── no deps
Step 3 (map.cot rehash) ───── no deps
                                        ├──→ Step 9 (re-enable scope_destroy)
Step 4 (copy retain) ──────── depends on understanding struct layout
Step 5 (assign retain) ────── depends on Step 4
Step 6 (switch retain) ────── depends on Step 4
Step 7 (param retain) ─────── depends on Step 4
Step 8 (checker fix) ──────── independent, needed for deinit on generics
```

Steps 1-3 can be done immediately and are safe. Steps 4-7 are the core implementation.
Step 8 is the blocker for re-enabling scope_destroy with `deinit`.
