# ARC Allocation Separation: `alloc` vs `alloc_raw`

**Date:** 2026-03-18
**Problem:** `alloc()` adds ARC headers to ALL allocations (both ARC-managed objects and raw memory). When ARC cleanup encounters pointers to raw memory (List/Map backing buffers), it modifies refcounts and potentially frees the buffers, corrupting heap data.
**Root cause of:** All remaining selfcot crashes (Map corruption in FuncBuilder, etc.)

---

## Current State

One allocation function: `alloc(metadata, size)` in `arc_native.zig`
- Always adds 32-byte ARC header (magic, alloc_size, metadata, refcount=1)
- Used by BOTH:
  - `new T { ... }` → ARC-managed heap object (retain/release should work)
  - `List.ensureCapacity` / `Map.ensureCapacity` → raw backing buffer (retain/release must NOT touch)
- `retain`/`release` check the magic header at `ptr - 32` to verify it's an ARC object
- **Problem:** Raw buffers from `alloc()` HAVE the magic header, so retain/release think they're ARC objects

## Swift Reference

Swift separates:
- `swift_allocObject(metadata, size, align)` → HeapObject with refcount (HeapObject.cpp:52)
- `swift_slowAlloc(size, align)` → raw memory via malloc (Heap.cpp)
- `malloc()` → libc raw memory

Only `swift_allocObject` results have ARC headers. `retain`/`release` only operate on HeapObject pointers. Raw buffers from `swift_slowAlloc`/`malloc` have NO ARC header — retain/release on them hits `isValidPointerForNativeRetain` null/range check and skips.

## Implementation Plan

### Step 1: Add `alloc_raw(size)` function (30 min)

**File:** `compiler/codegen/native/arc_native.zig`

Add a new runtime function `alloc_raw(size) → ptr` that calls `malloc(size)` directly WITHOUT adding an ARC header. Returns the raw malloc pointer.

```
alloc_raw(size):
    ptr = malloc(size)
    return ptr
```

No magic, no refcount, no metadata. Just malloc.

Also add `realloc_raw(ptr, new_size) → ptr` that calls `realloc(ptr, new_size)` directly.

And `dealloc_raw(ptr)` that calls `free(ptr)` directly.

### Step 2: Register new runtime functions (15 min)

**File:** `compiler/driver.zig`

Add `alloc_raw`, `realloc_raw`, `dealloc_raw` to:
- `runtime_func_names` array (line ~1339)
- `func_index_map` registration
- External function declarations for the linker

### Step 3: Switch stdlib List/Map to `alloc_raw` (30 min)

**Files:** `stdlib/list.cot`, `stdlib/map.cot`

Change all backing buffer allocations from `alloc(0, size)` to `alloc_raw(size)`.
Change all backing buffer reallocations from `realloc(ptr, size)` to `realloc_raw(ptr, size)`.
Change all backing buffer frees from `dealloc(ptr)` to `dealloc_raw(ptr)`.

List.cot changes:
- `ensureCapacity`: `alloc(0, bytes)` → `alloc_raw(bytes)`, `realloc(self.items, bytes)` → `realloc_raw(self.items, bytes)`
- `free`: `dealloc(self.items)` → `dealloc_raw(self.items)`
- `sort`: `alloc(0, n * @sizeOf(T))` → `alloc_raw(n * @sizeOf(T))`, `dealloc(tmp)` → `dealloc_raw(tmp)`

Map.cot changes:
- `ensureCapacity/rehash`: all `alloc(0, ...)` → `alloc_raw(...)`, `dealloc(...)` → `dealloc_raw(...)`
- `free`: `dealloc(self.keys)` etc → `dealloc_raw(...)`

### Step 4: Switch selfcot manual allocations to `alloc_raw` (15 min)

**File:** `self/main.cot`

`checkAndStoreChecker` uses `alloc(0, @sizeOf(Checker))` for manual heap allocation of checkers. This should use `alloc_raw` since these are manually managed (not ARC).

Lines ~1011-1014:
```cot
const errs_heap = alloc_raw(@sizeOf(ErrorReporter))
const chk_heap = alloc_raw(@sizeOf(Checker))
```

### Step 5: Verify `new T` still uses `alloc` with ARC header (verify only)

**File:** `compiler/frontend/lower.zig`

The `new` expression (lowerNewExpr) should continue using `alloc(metadata, size)` which adds the ARC header. Only `new` creates ARC-managed objects. Verify this path is unchanged.

### Step 6: Add `alloc_raw` to Wasm runtime (15 min)

**File:** `compiler/codegen/wasm/arc.zig`

Add `alloc_raw` as a simple bump-allocator wrapper (same as current `alloc` on Wasm, minus the header overhead). Wasm doesn't use ARC, so `alloc_raw` is just `malloc`.

### Step 7: Extern declarations in stdlib (10 min)

**File:** `stdlib/sys.cot`

Add `extern fn alloc_raw(size: i64) i64` and `extern fn realloc_raw(ptr: i64, size: i64) i64` and `extern fn dealloc_raw(ptr: i64) void`.

---

## Expected Result

After this change:
- `new T { ... }` → calls `alloc()` → ARC header, magic, refcount=1 → retain/release work
- `List.ensureCapacity` → calls `alloc_raw()` → NO ARC header → retain/release skip (magic check fails)
- `Map.ensureCapacity` → calls `alloc_raw()` → NO ARC header → retain/release skip

The selfcot's FuncBuilder.local_map backing buffers won't have ARC headers. When ARC cleanup runs and encounters these pointers, the magic check fails and they're skipped. No more heap data corruption.

## Risk Assessment

- **Low risk:** List/Map backing buffers are never retained/released intentionally. They're managed manually via ensureCapacity/free.
- **Breaking change:** Any user code that calls `alloc(0, size)` for raw memory will continue to get ARC headers (unchanged). Only stdlib code is modified to use `alloc_raw`.
- **Wasm path:** Unchanged semantics (Wasm has no ARC).

## Total Effort: ~2 hours
