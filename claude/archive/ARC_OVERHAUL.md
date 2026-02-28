# ARC Overhaul — Full Execution Plan

**Date**: February 27, 2026
**Goal**: Production-quality memory management — Swift ARC parity for native, WasmGC for Wasm
**Reference**: `references/swift/stdlib/public/runtime/HeapObject.cpp`, `references/swift/lib/SILGen/`

---

## Completed Phases Summary

| Phase | Description | Status |
|-------|-------------|--------|
| Phase 0 | WasmGC replaces Wasm ARC | **DONE** |
| Phase 1 | Managed pointer flag on `*T` | **DONE** (commit `73f4c0b`) |
| Phase 2 | Swift InlineRefCounts bit layout | **DONE** (commit `ee9f763`) |
| Phase 3 | Weak references via side tables | **DONE** (commit `d99b7bc`) |
| Phase 4 | Ownership correctness + destructor auto-release | **DONE** (commit `33a98a4`) |
| Phase 5 | Collection element ARC (List/Map retain/release) | **DONE** (commit `07332b7`) |

See `ARC_AUDIT.md` for detailed verification of all completed phases.

---

## Architecture: Split Memory Models

| Target | Memory Model | Rationale |
|--------|-------------|-----------|
| **Native** (default) | **Swift ARC** — Phases 0-5 complete | Deterministic, predictable, no GC pauses |
| **Wasm** (`--target=wasm32`) | **WasmGC** — browser GC manages structs | Smaller binaries, no ARC runtime overhead |

---

## What's Implemented (native ARC, after all phases)
- 24-byte heap header: `[alloc_size:i64][metadata:i64][refcount:i64][user_data...]`
- `alloc`, `retain`, `release`, `dealloc`, `realloc` via CLIF IR in `arc_native.zig`
- Swift InlineRefCounts bit layout (strong + unowned + weak)
- Side tables for weak references with zeroing semantics
- `managed` flag on pointer types — `couldBeARC()` is precise
- Ownership correctness — auto-generated destructor field releases
- Collection element ARC — `@arcRetain`/`@arcRelease` in List and Map methods
- Swift-faithful `set()` ordering: load old → retain new → store → release old
- Cleanup stack LIFO ordering (faithful Swift CleanupManager port)
- ManagedValue/forward pattern, immortal refcount, null-safe retain/release
- Type deduplication for compound type constructors

## What's Still Missing (vs Swift)

| Gap | Priority | Blocked by |
|-----|----------|------------|
| Non-atomic refcount (no thread safety) | Medium | Concurrency roadmap (0.6) |
| ARC optimization pass (retain/release elision) | Low | Correctness-first |
| `unowned` keyword (traps on access after dealloc) | Low | — |

---

## Phase 0: Remove Wasm ARC (Replace with WasmGC) — DONE

**Status**: Complete. `--target=wasm32` now produces WasmGC output. Wasm structs are GC-managed. `cot_retain`/`cot_release` are dead code on Wasm (ARC only runs on native). `arc.zig` still provides `cot_alloc`/`cot_dealloc`/`cot_realloc` for linear memory (string buffers, List backing arrays).

### 0.1 — Define WasmGC Type Mappings

Map Cot types to WasmGC type section entries:

```
Cot struct Foo { x: i64, y: f64 }  →  (type $Foo (struct (field i64) (field f64)))
Cot new Foo { ... }                →  struct.new $Foo
Cot foo.x                          →  struct.get $Foo 0
Cot foo.x = val                    →  struct.set $Foo 0
Cot [N]T (arrays)                  →  (type $arr_T (array (mut T)))
Cot string data                    →  (type $bytes (array (mut i8)))
```

**Files to modify**:
- `compiler/codegen/wasm/link.zig` — Add WasmGC type section generation (composite types: struct, array)
- `compiler/codegen/wasm/assemble.zig` — Encode `0xFB` prefix opcodes (`struct.new`, `struct.get`, `struct.set`, `array.new`, etc.)
- `compiler/codegen/wasm_opcodes.zig` — Add WasmGC opcode constants

**New opcodes to add** (all under `0xFB` prefix):
| Instruction | Encoding | Usage |
|-------------|----------|-------|
| `struct.new x` | `0xFB 0x00` | Replace `alloc()` + field stores |
| `struct.get x y` | `0xFB 0x02` | Replace `load(ptr + offset)` |
| `struct.set x y` | `0xFB 0x05` | Replace `store(ptr + offset, val)` |
| `array.new x` | `0xFB 0x06` | Replace `alloc()` for arrays/slices |
| `array.get x` | `0xFB 0x0B` | Replace `load(base + idx * stride)` |
| `array.set x` | `0xFB 0x0E` | Replace `store(base + idx * stride, val)` |
| `array.len` | `0xFB 0x0F` | Replace manual length tracking |
| `ref.cast` | `0xFB 0x16` | Type casting |

### 0.2 — Remove Wasm ARC Runtime

**Delete or gut these files/sections**:

| File | Lines | Action |
|------|-------|--------|
| `compiler/codegen/arc.zig` | 1,559 | **DELETE** — entire Wasm ARC runtime (alloc, retain, release, dealloc, realloc, freelist, string ops) |
| `compiler/codegen/wasm/gen.zig` | ~100 | **MODIFY** — remove `wasm_lowered_retain`/`wasm_lowered_release` emission, replace `alloc` calls with `struct.new` |
| `compiler/codegen/wasm/link.zig` | ~200 | **MODIFY** — remove ARC function registration (alloc, dealloc, retain, release, realloc), add WasmGC type section |
| `compiler/ssa/passes/lower_wasm.zig` | ~30 | **MODIFY** — remove `retain→wasm_lowered_retain` and `release→wasm_lowered_release` lowering rules |
| `compiler/ssa/op.zig` | ~10 | **MODIFY** — remove `wasm_lowered_retain` and `wasm_lowered_release` op definitions |
| `compiler/driver.zig` | ~100 | **MODIFY** — remove Wasm ARC func_indices registration, add WasmGC type index mapping |
| `compiler/codegen/slice_runtime.zig` | varies | **MODIFY** — `growslice` needs to use WasmGC arrays instead of `realloc` |

### 0.3 — Modify Frontend for Target-Conditional ARC

The frontend (`lower.zig`, `arc_insertion.zig`) currently inserts retain/release unconditionally. For Wasm targets, ARC insertion must be skipped entirely — the GC handles it.

**Changes to `lower.zig`**:
- `lowerLocalVarDecl`: Skip ARC cleanup push when `target == .wasm32`
- `emitCleanups`: Skip release emission for Wasm target
- `lowerAssign` (deref/field/index): Skip release-old/retain-new for Wasm target
- `new T { ... }`: Emit `struct.new` instead of `alloc` + field stores for Wasm target

**Changes to `arc_insertion.zig`**:
- `CleanupStack`: Add `target` field, skip `.release` cleanups for Wasm
- Keep `.defer_expr`, `.errdefer_expr`, `.scope_destroy` cleanups (those are not ARC — they're language semantics)

### 0.4 — String and Collection Representation on WasmGC

Strings and collections need WasmGC-native representations:

```
// String: pointer + length → WasmGC array ref + length
Cot string  →  (struct (field (ref $bytes)) (field i32))  // ref to byte array + length

// List(T): items + count + capacity → WasmGC array ref + count
Cot List(T) →  (struct (field (ref $arr_T)) (field i32))  // ref to T array + count

// Map(K,V): needs WasmGC arrays for buckets + entries
```

**Key difference**: WasmGC arrays are GC-managed, so no `realloc` — grow by creating a new, larger array and copying. The GC reclaims the old one.

### 0.5 — Testing & Validation

- All existing Wasm tests (`cot test <file> --target=wasm32`) must pass with WasmGC
- Verify no retain/release calls appear in generated Wasm modules
- Verify WasmGC struct types appear in the type section
- Verify `wasmtime` can run WasmGC modules (wasmtime supports WasmGC since v14)
- Benchmark: compare binary size (expect ~2-5KB reduction from removed ARC runtime)

---

## Phase 1: Type-System ARC Distinction (The Foundation)

**The root cause of all ARC bugs**: `*T` can be either ARC-managed or raw, and the compiler can't tell. Fix this at the type level.

### 1.1 — Add `managed` Flag to Pointer Types

**File**: `compiler/frontend/types.zig`

Add a `managed` boolean to the pointer type:

```zig
// Current:
pointer: struct { elem: TypeIndex, is_const: bool }

// New:
pointer: struct { elem: TypeIndex, is_const: bool, managed: bool }
```

- `new T { ... }` returns `*T` with `managed = true`
- `alloc()`, `@intToPtr()`, pointer arithmetic return `managed = false`
- Functions inherit from their return type annotation
- `couldBeARC()` checks `managed` flag instead of guessing from pointee type

### 1.2 — Propagate `managed` Through the Type System

**File**: `compiler/frontend/checker.zig`

- `new T { ... }` expression: result type is `*T` with `managed = true`
- Function return type inference: if the function body returns a `new` expression, infer `managed = true` for the return type
- Variable assignment: `var x = new Foo{}` → x has type `*Foo(managed=true)`
- Function parameter passing: `managed` propagates through call sites
- Generic instantiation: `List(*Foo)` where `*Foo` is managed → element type carries `managed`

### 1.3 — Fix `couldBeARC()` to Use the Flag

**File**: `compiler/frontend/types.zig`

```zig
pub fn couldBeARC(self: *const TypeRegistry, idx: TypeIndex) bool {
    const t = self.get(idx);
    if (t == .pointer) {
        return t.pointer.managed;  // Only managed pointers get ARC
    }
    if (t == .optional) {
        return self.couldBeARC(t.optional.elem);
    }
    return false;
}
```

This eliminates the entire class of bugs where raw pointers get ARC'd.

### 1.4 — Ownership Convention on Function Types

**File**: `compiler/frontend/types.zig`, `compiler/frontend/checker.zig`

Add return ownership to function types (Swift's `ResultConvention`):

```zig
// In function type:
return_convention: enum { owned, guaranteed, unowned } = .owned
```

- Functions returning `new T` or calling allocating functions → `.owned` (+1, caller must release)
- Getters, accessors, field reads → `.guaranteed` (+0, caller must NOT release)
- Default for user functions returning `*T(managed=true)` → `.owned`

**Inference rules** (checker determines convention from function body):
1. Return expression is `new T { ... }` → `.owned`
2. Return expression is a field access → `.guaranteed`
3. Return expression is a parameter → `.guaranteed`
4. Return expression is a call → inherit callee's convention
5. Default → `.owned` (safe — may leak but won't corrupt)

### 1.5 — Update `lowerLocalVarDecl` to Use Conventions

**File**: `compiler/frontend/lower.zig`

```zig
// Current (broken):
const is_owned = if (value_expr) |e| (e == .new_expr or e == .call) else false;

// New (correct):
const is_owned = blk: {
    if (!type_reg.couldBeARC(type_idx)) break :blk false;
    if (value_expr) |e| {
        if (e == .new_expr) break :blk true;
        if (e == .call) {
            // Check callee's return convention
            const callee_type = getCalleeType(e);
            break :blk callee_type.return_convention == .owned;
        }
    }
    break :blk false;
};
```

### 1.6 — Tests

Add to `test/e2e/arc.cot`:
- Factory function returning `*T` (owned) → auto-released
- Getter returning `*T` (guaranteed) → NOT auto-released
- Raw pointer from `@intToPtr` → NOT arc'd
- Mixed managed/raw in same scope
- Generic function with managed type parameter

---

## Phase 2: Three Reference Counts (Swift Parity)

**Reference**: `references/swift/stdlib/public/SwiftShims/swift/shims/RefCount.h`

Swift's InlineRefCounts packs three counters into 64 bits. Cot should match this.

### 2.1 — Redesign Heap Object Header

**File**: `compiler/codegen/native/arc_native.zig`

```
Current header (24 bytes):
[alloc_size: i64][metadata: i64][refcount: i64][user_data...]

New header (24 bytes — same size, more information):
[alloc_size: i64][metadata: i64][refcounts: i64][user_data...]

refcounts bit layout (64 bits, matching Swift's InlineRefCounts):
  Bit  0:       UseSlowRC (1 bit)  — 1 = side table mode, 0 = inline mode
  Bits 1-31:    UnownedRC (31 bits) — unowned reference count (extra count, +1 = 1 logical)
  Bit  32:      IsDeiniting (1 bit) — object is being destroyed
  Bits 33-62:   StrongRC (30 bits)  — strong reference count (extra count, +1 = 1 logical)
  Bit  63:      Immortal (1 bit)    — object is immortal (string constants)
```

**Why match Swift's layout**: Exact bit compatibility means we can copy Swift's inline RC manipulation code exactly. No invention, no bugs.

### 2.2 — Implement Inline Refcount Operations

**File**: `compiler/codegen/native/arc_native.zig`

Replace the current simple `load/increment/store` with bit-field operations:

```
retain(obj):
  refcounts = atomic_load(obj - 8)
  if (refcounts >> 63) return obj          // Immortal check (bit 63)
  if (refcounts & 1) goto slow_path        // UseSlowRC → side table
  new_rc = refcounts + (1 << 33)           // Increment StrongRC field
  atomic_store(obj - 8, new_rc)
  return obj

release(obj):
  refcounts = atomic_load(obj - 8)
  if (refcounts >> 63) return              // Immortal
  if (refcounts & 1) goto slow_path        // Side table
  new_rc = refcounts - (1 << 33)           // Decrement StrongRC field
  strong_count = (new_rc >> 33) & 0x3FFFFFFF
  if (strong_count == 0):
    new_rc = new_rc | (1 << 32)            // Set IsDeiniting flag
    atomic_store(obj - 8, new_rc)
    call destructor(obj)                    // metadata->destroy(obj)
    unowned_release(obj)                    // Decrement unowned RC
  else:
    atomic_store(obj - 8, new_rc)
```

### 2.3 — Implement Unowned References

**New runtime functions** (add to `arc_native.zig`):

```
unowned_retain(obj):   // Increment UnownedRC (bits 1-31)
  refcounts = atomic_load(obj - 8)
  new_rc = refcounts + (1 << 1)   // +1 to UnownedRC field
  atomic_store(obj - 8, new_rc)

unowned_release(obj):  // Decrement UnownedRC, free memory if 0
  refcounts = atomic_load(obj - 8)
  new_rc = refcounts - (1 << 1)
  unowned_count = (new_rc >> 1) & 0x7FFFFFFF
  if (unowned_count == 0):
    // No more unowned refs → free the memory
    weak_release(obj)  // Decrement weak RC, free side table if 0
  else:
    atomic_store(obj - 8, new_rc)

unowned_load_strong(obj):  // Convert unowned→strong (traps if deiniting)
  refcounts = atomic_load(obj - 8)
  if (refcounts >> 32) & 1:  // IsDeiniting
    trap("access to unowned reference after deallocation")
  retain(obj)
  return obj
```

**New Cot syntax**: `unowned var x = strong_ref`
- Parser: recognize `unowned` keyword before `var`/`const`
- Checker: validate pointee is managed type
- Lowerer: emit `unowned_retain` instead of `retain`, `unowned_release` instead of `release`

### 2.4 — Update `alloc()` to Initialize All Three Counters

```
alloc(metadata, size):
  raw = malloc(size + 24)
  store(raw + 0, alloc_size)
  store(raw + 8, metadata)
  // Initialize: Immortal=0, StrongRC=0(+1=1 logical), IsDeiniting=0, UnownedRC=0(+1=1), UseSlowRC=0
  // StrongRC: 0 in extra-count field = 1 logical (0 << 33)
  // UnownedRC: 0 in extra-count field = 1 logical (0 << 1)
  store(raw + 16, 0x0000000000000000)  // All zero = strong=1, unowned=1, weak=1 (implied)
  return raw + 24
```

### 2.5 — Object Lifecycle State Machine

Implement Swift's 8-state lifecycle:

| State | Meaning | Allowed Operations |
|-------|---------|-------------------|
| LIVE | Normal operation | retain, release, unowned_retain, weak_init |
| DEINITING | Strong RC hit 0, destructor running | unowned_load_strong → trap, weak_load → nil |
| DEINITED | Destructor finished, unowned refs remain | unowned_release only |
| FREED | Memory freed, weak refs remain (side table alive) | weak_load → nil, weak_release |
| DEAD | Everything gone | N/A |

The IsDeiniting bit (bit 32) drives state transitions. Combined with UseSlowRC (bit 0), the runtime can determine the exact state.

### 2.6 — Tests

Add to `test/e2e/arc.cot`:
- `unowned var` basic usage
- `unowned` trap on access after dealloc (test with expected panic)
- Object lifecycle: strong release → deinit runs → unowned still valid → unowned release → freed
- Immortal objects skip all RC operations
- Multiple strong refs → last release triggers deinit

---

## Phase 3: Weak References with Side Tables

**Reference**: `references/swift/stdlib/public/runtime/WeakReference.h`, `references/swift/stdlib/public/SwiftShims/swift/shims/RefCount.h`

This is the most complex phase. Swift uses side table entries for weak references, with zeroing semantics.

### 3.1 — Side Table Entry Structure

**New file**: `compiler/codegen/native/side_table.zig` (or inline in `arc_native.zig`)

```
HeapObjectSideTableEntry:
  [object_ptr: i64]     — back-pointer to the heap object
  [refcounts: i64]      — strong + unowned + weak RC (same bit layout as inline)
  [weak_count: i64]     — number of weak references (separate counter for side table lifetime)
```

When a weak reference is created for an object that doesn't have a side table yet:
1. Allocate a `HeapObjectSideTableEntry` via `malloc(24)`
2. Copy the current inline refcounts to the side table
3. Set the object's refcounts field to `(side_table_ptr | 0x1)` — UseSlowRC bit marks side table mode
4. Weak variable stores a pointer to the side table entry (not the object)

### 3.2 — Weak Reference Runtime Functions

**New functions in `arc_native.zig`**:

```
weak_init(weak_ref_addr, obj):
  // Create or reuse side table for obj
  refcounts = load(obj - 8)
  if (refcounts & 1):  // Already has side table
    side_table = refcounts & ~0x1
  else:
    side_table = malloc(24)
    store(side_table + 0, obj)           // back-pointer
    store(side_table + 8, refcounts)     // copy inline RC
    store(side_table + 16, 1)            // weak_count = 1
    store(obj - 8, side_table | 0x1)     // Switch to slow RC
  // Store side table pointer in weak_ref variable
  store(weak_ref_addr, side_table)

weak_load_strong(weak_ref_addr) -> obj_or_null:
  side_table = load(weak_ref_addr)
  if (side_table == 0) return 0          // Already nil
  obj = load(side_table + 0)             // Back-pointer
  refcounts = load(side_table + 8)
  is_deiniting = (refcounts >> 32) & 1
  if (is_deiniting):
    return 0                              // Object dead → return nil (ZEROING)
  // Object alive → acquire strong reference
  retain(obj)
  return obj

weak_release(weak_ref_addr):
  side_table = load(weak_ref_addr)
  if (side_table == 0) return
  weak_count = load(side_table + 16)
  weak_count -= 1
  if (weak_count == 0):
    free(side_table)                      // Last weak ref → free side table
  else:
    store(side_table + 16, weak_count)
  store(weak_ref_addr, 0)                 // Clear weak ref
```

### 3.3 — Zeroing Semantics

When strong RC hits 0 and the object has a side table (UseSlowRC=1):
1. Set IsDeiniting in side table's refcounts
2. Run destructor
3. Zero the object back-pointer in the side table: `store(side_table + 0, 0)`
4. This causes all subsequent `weak_load_strong` calls to return nil

This is **zeroing** — weak references automatically become nil after the object is destroyed, without any explicit notification.

### 3.4 — Update `weak var` Semantics

**Current behavior** (broken): `weak var w = obj` just stores the raw pointer, no retain, no safety.

**New behavior** (Swift-like):
- `weak var w = obj` → emit `weak_init(&w, obj)` — creates side table entry
- `w.field` → emit `weak_load_strong(&w)`, null-check result, access field
- Scope exit → emit `weak_release(&w)` — decrement weak count
- `weak var` type is always optional: `weak var w: ?*Foo = obj` (can be nil after dealloc)

**Files to modify**:
- `compiler/frontend/checker.zig` — `weak var` must be optional type
- `compiler/frontend/lower.zig` — emit `weak_init`/`weak_load_strong`/`weak_release`
- `compiler/frontend/arc_insertion.zig` — add `CleanupKind.weak_release`

### 3.5 — Slow Path for Side Table Mode

When `UseSlowRC` (bit 0) is set in the inline refcounts, all RC operations redirect to the side table:

```
retain(obj):
  refcounts = load(obj - 8)
  if (refcounts & 1):  // Side table mode
    side_table = refcounts & ~0x1
    side_refcounts = load(side_table + 8)
    side_refcounts += (1 << 33)  // Increment StrongRC in side table
    store(side_table + 8, side_refcounts)
    return obj
  // ... inline fast path ...

release(obj):
  refcounts = load(obj - 8)
  if (refcounts & 1):  // Side table mode
    side_table = refcounts & ~0x1
    side_refcounts = load(side_table + 8)
    side_refcounts -= (1 << 33)
    strong = (side_refcounts >> 33) & 0x3FFFFFFF
    if (strong == 0):
      side_refcounts |= (1 << 32)  // IsDeiniting
      store(side_table + 8, side_refcounts)
      store(side_table + 0, 0)     // Zero back-pointer (zeroing weak refs)
      call destructor(obj)
      unowned_release(obj)
    else:
      store(side_table + 8, side_refcounts)
    return
  // ... inline fast path ...
```

### 3.6 — Tests

Add to `test/e2e/arc.cot`:
- Weak reference zeroing: create weak, release strong, check weak is nil
- Weak reference cycle breaking: parent↔child with weak back-reference
- Side table creation on first weak ref
- Multiple weak refs to same object
- `weak var` is optional — requires `if (w) |val| { }` to access
- Weak ref after scope exit → nil

---

## Phase 4: Collection Element ARC

**Problem**: `List(*Foo).free()` frees the backing buffer but doesn't release elements. `list.set(i, new_val)` overwrites without releasing the old value.

### 4.1 — ARC-Aware Generic Instantiation

**File**: `compiler/frontend/lower.zig`, `compiler/frontend/checker.zig`

When `List(T)` is instantiated with `T = *Foo(managed=true)`:
- The monomorphized `List_pFoo` type carries metadata: "element type is ARC-managed"
- The lowerer checks this during method emission

### 4.2 — Collection Methods That Need ARC

| Method | Current | Correct |
|--------|---------|---------|
| `List.free()` | `dealloc(items)` | Release each element, then `dealloc(items)` |
| `List.set(i, val)` | `items[i] = val` | `release(items[i])`, then `items[i] = val` |
| `List.clear()` | `count = 0` | Release each element, then `count = 0` |
| `List.pop()` | Return and decrement count | Return element (caller gets +1), decrement count |
| `List.remove(i)` | Remove and shift | `release(items[i])`, shift, decrement count |
| `List.append(val)` | Grow + store | Store val (already +1 from caller) |
| `Map.set(k, v)` | Overwrite entry | Release old value (and old key if different), store new |
| `Map.free()` | `dealloc(buckets)` | Release all keys + values, then `dealloc` |
| `Map.remove(k)` | Remove entry | Release key + value of removed entry |

### 4.3 — Implementation Strategy

**Option**: Emit ARC calls at the call site during monomorphization, not inside the collection method.

When the lowerer sees `list.set(i, val)` and knows the list's element type is ARC-managed:
```
// Before: just emits call to List_set(list, i, val)
// After:
old_val = List_get(list, i)
release(old_val)
List_set(list, i, val)
```

This avoids modifying stdlib collection source — the compiler injects ARC around collection operations based on the element type.

**Alternative**: Add `release_element` callback in collection types, set during monomorphization. This is more like Swift's value witness tables but simpler.

### 4.4 — Tests

Add to `test/e2e/arc.cot`:
- `List(*Node)` — append, set (old released), free (all released)
- `Map(string, *Node)` — set (old value released), remove (released), free (all released)
- Nested: `List(*List(*Node))` — recursive element release on free

---

## Phase 5: ARC Optimization Pass

**Reference**: `references/swift/lib/SILOptimizer/ARC/`

After ARC is correct, optimize it. Swift's ARC optimizer eliminates redundant retain/release pairs.

### 5.1 — Retain/Release Pairing

**New SSA pass**: `compiler/ssa/passes/arc_optimize.zig`

Find and eliminate balanced retain/release pairs:
```
// Before optimization:
retain(x)
use(x)
release(x)

// After: remove retain + release (net effect is zero)
use(x)
```

Rules:
- `retain(x)` followed by `release(x)` with no intervening store/call that could alias x → eliminate both
- `retain(x)` at function entry + `release(x)` at function exit when x is a parameter → eliminate both (parameter is already +1)

### 5.2 — Copy Elimination

When a `+0` value is copied and the copy is released before the original goes out of scope:
```
// Before:
y = retain(x)   // +1 copy
use(y)
release(y)       // -1 copy

// After: use x directly (it's still alive)
use(x)
```

### 5.3 — Move Semantics

When a value is consumed (last use), convert retain+release to a move:
```
// Before:
y = retain(x)
release(x)      // x no longer used

// After: just move (no RC operations)
y = x           // ownership transferred
```

### 5.4 — Tests

- Verify optimization doesn't change observable behavior
- Benchmark: measure retain/release call count before vs after optimization
- Stress test: deeply nested scopes with many ARC values

---

## Phase 6: Atomic Reference Counting (Thread Safety)

**Reference**: Swift's `std::atomic` operations in `RefCount.h`
**Dependency**: Cot's concurrency roadmap (v0.6 — `spawn`, channels, work-stealing)

### 6.1 — Atomic Inline Refcount Operations

Replace `load/iadd/store` with atomic compare-and-swap:

```
retain(obj):  // Thread-safe
  loop:
    old = atomic_load_relaxed(obj - 8)
    new = old + (1 << 33)
    if atomic_cas(obj - 8, old, new): break
  return obj

release(obj):  // Thread-safe
  loop:
    old = atomic_load_relaxed(obj - 8)
    new = old - (1 << 33)
    if atomic_cas(obj - 8, old, new): break
  strong = (new >> 33) & 0x3FFFFFFF
  if (strong == 0):
    atomic_fence_acquire()
    // ... deinit + dealloc path ...
```

### 6.2 — Non-Atomic Fast Path

For single-threaded code (no `spawn` in the module), use non-atomic operations (Swift's `swift_nonatomic_retain`/`swift_nonatomic_release`):

```
// Compiler detects: no spawn/channel usage in this compilation unit
// → use non-atomic RC (faster, no CAS overhead)
retain_nonatomic(obj):
  rc = load(obj - 8)
  store(obj - 8, rc + (1 << 33))
  return obj
```

### 6.3 — CLIF IR Atomic Operations

Add atomic load/store/CAS instructions to the CLIF IR:
- `atomic_load` → ARM64 `ldar`, x64 `mov` (x64 loads are atomic on aligned addresses)
- `atomic_store` → ARM64 `stlr`, x64 `mov` (+ `mfence` for release semantics)
- `atomic_cas` → ARM64 `ldaxr`/`stlxr` loop, x64 `lock cmpxchg`

---

## Phase 7: Destructor Improvements

### 7.1 — Cascading Destructors

When a struct has fields that are ARC-managed, the destructor must release those fields:

```cot
struct Tree {
    left: *Tree
    right: *Tree
    value: i64
}

// Compiler-generated destructor:
fn __Tree_deinit(self: *Tree) {
    release(self.left)    // Release left child
    release(self.right)   // Release right child
    // value is i64 (trivial) — no action
}
```

**File**: `compiler/frontend/lower.zig` — auto-generate destructors for structs with managed fields, even if user doesn't define `deinit`.

### 7.2 — User-Defined deinit + Auto Fields

If the user defines `deinit`, the compiler appends field releases after the user's code:

```cot
impl Tree {
    fn deinit(self: *Tree) {
        // User code runs first
        log("Tree destroyed")
    }
    // Compiler appends: release(self.left), release(self.right)
}
```

### 7.3 — Tests

- Struct with managed fields → auto-generated destructor releases fields
- User deinit + managed fields → user code + auto releases
- Recursive struct (tree) → cascading deallocation
- Struct with no managed fields → no destructor generated

---

## Implementation Order & Dependencies

```
Phase 0: Remove Wasm ARC / Add WasmGC
  ↓
Phase 1: Type-System ARC Distinction ← BLOCKS everything else
  ↓
Phase 2: Three Reference Counts ← requires Phase 1 (managed flag)
  ↓
Phase 3: Weak References + Side Tables ← requires Phase 2 (three RCs)
  ↓
Phase 4: Collection Element ARC ← requires Phase 1 (managed flag in generics)
  ↓
Phase 5: ARC Optimization ← requires Phases 1-4 (correct ARC first, optimize later)
  ↓
Phase 6: Atomic RC ← requires Phase 2 (bit layout), blocked by concurrency roadmap
  ↓
Phase 7: Destructor Improvements ← requires Phase 1 (managed flag for field detection)
```

**Can parallelize**:
- Phase 0 (Wasm) is independent of Phases 1-7 (native)
- Phase 4 (collections) can start after Phase 1
- Phase 7 (destructors) can start after Phase 1

---

## File Change Summary

### New Files
| File | Purpose |
|------|---------|
| `compiler/codegen/native/side_table.zig` | Side table entry allocation and management (Phase 3) |
| `compiler/ssa/passes/arc_optimize.zig` | ARC optimization pass — retain/release elision (Phase 5) |

### Major Modifications
| File | Changes |
|------|---------|
| `compiler/frontend/types.zig` | Add `managed` flag to pointer type, fix `couldBeARC()`, add return conventions |
| `compiler/frontend/checker.zig` | Infer `managed` flag, infer return ownership convention |
| `compiler/frontend/lower.zig` | Use conventions instead of heuristics, target-conditional ARC, collection ARC insertion |
| `compiler/frontend/arc_insertion.zig` | Add `weak_release` cleanup kind, target field, `unowned` support |
| `compiler/codegen/native/arc_native.zig` | Three RCs (bit-packed), side table mode, weak/unowned functions, atomic CAS |
| `compiler/codegen/wasm/gen.zig` | Replace retain/release with WasmGC struct ops |
| `compiler/codegen/wasm/link.zig` | Remove ARC function emission, add WasmGC type section |
| `compiler/ssa/passes/lower_wasm.zig` | Remove ARC lowering for Wasm target |
| `compiler/ssa/op.zig` | Remove `wasm_lowered_retain`/`wasm_lowered_release` |
| `compiler/driver.zig` | Remove Wasm ARC func_indices, add WasmGC type mapping |

### Deleted Files
| File | Reason |
|------|--------|
| `compiler/codegen/arc.zig` (1,559 lines) | Entire Wasm ARC runtime — replaced by WasmGC |

---

## Verification Strategy

After each phase:
1. `zig build test` — compiler internals pass
2. `cot test test/e2e/features.cot` — all ~341 feature tests pass (native)
3. `cot test test/e2e/features.cot --target=wasm32` — all pass (Wasm, with WasmGC after Phase 0)
4. `cot test test/e2e/arc.cot` — targeted ARC tests pass
5. `./test/run_all.sh` — full suite passes (~1,623 tests across 67 files)
6. No memory corruption under stress (run ARC tests in a loop 1000x)

---

## Swift Reference File Index

| Feature | Swift File | Cot Target |
|---------|-----------|------------|
| HeapObject layout | `references/swift/stdlib/public/SwiftShims/swift/shims/HeapObject.h` | `arc_native.zig` header layout |
| InlineRefCounts bit layout | `references/swift/stdlib/public/SwiftShims/swift/shims/RefCount.h` | `arc_native.zig` refcount operations |
| swift_retain/release | `references/swift/stdlib/public/runtime/HeapObject.cpp:474-552` | `arc_native.zig` retain/release |
| swift_allocObject | `references/swift/stdlib/public/runtime/HeapObject.cpp:424-440` | `arc_native.zig` alloc |
| WeakReference | `references/swift/stdlib/public/runtime/WeakReference.h` | `side_table.zig` |
| Side table entry | `references/swift/stdlib/public/SwiftShims/swift/shims/RefCount.h` (HeapObjectSideTableEntry) | `side_table.zig` |
| ManagedValue | `references/swift/lib/SILGen/ManagedValue.h:59-95` | `arc_insertion.zig` |
| Cleanup stack | `references/swift/lib/SILGen/Cleanup.h:85-317` | `arc_insertion.zig` |
| ARC insertion | `references/swift/lib/SILGen/SILGenExpr.cpp:70-109` | `lower.zig` |
| Ownership conventions | `references/swift/include/swift/AST/Types.h` (ResultConvention) | `types.zig` |
| ARC optimization | `references/swift/lib/SILOptimizer/ARC/` | `arc_optimize.zig` |
