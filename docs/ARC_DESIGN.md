# ARC Design — Cot's Swift-Based Reference Counting

**Last updated:** February 7, 2026
**Status:** Production-grade. 852 tests pass. Freelist allocator, destructor dispatch, builtins all working.

---

## Table of Contents

1. [Swift Reference Sources](#swift-reference-sources)
2. [Architecture Overview](#architecture-overview)
3. [Object Header Layout](#object-header-layout)
4. [Runtime Functions — Detailed Comparison with Swift](#runtime-functions)
5. [ARC Insertion (Compiler Frontend)](#arc-insertion)
6. [Memory Allocator](#memory-allocator)
7. [Native AOT Considerations](#native-aot-considerations)
8. [Builtins](#builtins)
9. [Test Coverage](#test-coverage)
10. [Known Issues & Future Work](#known-issues--future-work)
11. [File Map](#file-map)

---

## Swift Reference Sources

| File | Purpose |
|------|---------|
| `~/learning/swift/stdlib/public/SwiftShims/swift/shims/HeapObject.h` | HeapObject struct: metadata + InlineRefCounts = 16 bytes |
| `~/learning/swift/stdlib/public/SwiftShims/swift/shims/RefCount.h` | RefCount bit layout: strong(30), unowned(31), flags, side table |
| `~/learning/swift/stdlib/public/runtime/HeapObject.cpp` | `swift_allocObject` (247-276), `swift_retain` (474-545), `swift_release` (548-596), `_swift_release_dealloc` (835-837), `swift_deallocObject` (967-1070) |
| `~/learning/swift/stdlib/public/runtime/Heap.cpp` | `swift_slowAlloc` (85-96), `swift_slowDealloc` (151-161) |
| `~/learning/swift/include/swift/Runtime/RuntimeFunctions.def` | Runtime function declarations |
| `~/learning/swift/include/swift/SIL/SILNodes.def` | SIL instructions: strong_retain, strong_release, copy_value, destroy_value |

---

## Architecture Overview

### Swift's Deallocation Chain (What We Port)

```
swift_release(object)                        // HeapObject.cpp:548
  → refCounts.decrementAndMaybeDeinit(1)     // Atomic CAS, memory_order_release
  → if refcount == 0:
    → _swift_release_dealloc(object)         // HeapObject.cpp:835
      → object->metadata->destroy(object)   // Type-specific destructor
    → swift_deallocObject(object, size)      // HeapObject.cpp:967
      → if canBeFreedNow():
        → swift_slowDealloc(ptr, size)       // Heap.cpp:151
          → free(ptr)                        // System allocator
```

### Cot's Equivalent Chain

```
cot_release(obj)                             // arc.zig:553
  → decrement refcount (non-atomic)
  → if refcount == 0:
    → if metadata_ptr != 0:                  // Guard against null metadata
      → destructor_ptr = load(metadata + 8)
      → if destructor_ptr != 0:
        → call_indirect destructor(obj)      // Wasm function table dispatch
    → cot_dealloc(obj)                       // arc.zig:363
      → push to freelist                     // Zig WasmAllocator pattern
```

### Key Design Decisions

| Decision | Swift | Cot | Rationale |
|----------|-------|-----|-----------|
| Atomics | Atomic CAS with memory ordering | Non-atomic loads/stores | Wasm is single-threaded |
| Deallocation | System `free()` via `swift_slowDealloc` | Freelist push (Zig WasmAllocator pattern) | Wasm has no system allocator |
| Weak/Unowned | Side table with 3 refcount types | Strong refcount only | Not yet needed |
| Metadata | 8-byte pointer to type metadata struct | 4-byte Wasm table index | Wasm tables are 32-bit indexed |
| Header size | 16 bytes (metadata + refcounts) | 16 bytes (total_size + metadata + refcount) | Same size, different layout |

---

## Object Header Layout

### Swift (64-bit)

```
┌────────────────────────────────────────────┐
│ HeapObject (16 bytes)                      │
├────────────────────────────────────────────┤
│ +0: HeapMetadata *metadata     (8 bytes)   │  → Type info, destructor pointer
│ +8: InlineRefCounts refCounts  (8 bytes)   │  → Packed: strong(30) + unowned(31) + flags
├────────────────────────────────────────────┤
│ +16: user data starts here                 │
└────────────────────────────────────────────┘
```

Swift's `InlineRefCounts` bit layout (64-bit):
```
Bit  0:      PureSwiftDealloc
Bits 1-31:   UnownedRefCount (31 bits, initial=1)
Bit  32:     IsDeiniting
Bits 33-62:  StrongExtraRefCount (30 bits, actual = field + 1)
Bit  63:     UseSlowRC / SideTableMark
```

### Cot (Wasm32)

```
┌────────────────────────────────────────────┐
│ CotHeapObject (16 bytes)                   │
├────────────────────────────────────────────┤
│ +0:  total_size: i32  (block size)         │  → Used by freelist allocator for first-fit
│ +4:  metadata:   i32  (destructor table)   │  → Wasm function table index, 0 = no destructor
│ +8:  refcount:   i64  (strong count)       │  → Simple counter, 1 = single owner
├────────────────────────────────────────────┤
│ +16: user data starts here                 │
└────────────────────────────────────────────┘
```

### Constants (`arc.zig`)

```zig
pub const HEAP_OBJECT_HEADER_SIZE: u32 = 16;
pub const SIZE_OFFSET: u32 = 0;          // total_size field
pub const METADATA_OFFSET: u32 = 4;      // metadata/destructor table index
pub const REFCOUNT_OFFSET: u32 = 8;      // strong refcount
pub const USER_DATA_OFFSET: u32 = 16;    // payload starts here
pub const FREELIST_NEXT_OFFSET: u32 = 4; // = METADATA_OFFSET (reuses metadata slot in freed blocks)
pub const DESTRUCTOR_OFFSET: u32 = 8;    // destructor fn index within metadata table
pub const IMMORTAL_REFCOUNT: i64 = 0x7FFFFFFFFFFFFFFF;
pub const INITIAL_REFCOUNT: i64 = 1;
pub const HEAP_START: u32 = 0x10000;     // 64KB reserved for stack
```

### Why `total_size` at Offset 0?

Swift uses system `malloc` which tracks block size internally. Cot's Wasm runtime has no system allocator,
so we need the block size in the header for:
1. **Freelist first-fit**: `cot_alloc` checks if a freed block is large enough to reuse
2. **Realloc shrink**: `cot_realloc` compares old vs new size to decide if in-place shrink works
3. **Dealloc correctness**: `FREELIST_NEXT_OFFSET` uses `METADATA_OFFSET` (4), NOT `SIZE_OFFSET` (0),
   so block size is preserved when a block is freed

---

## Runtime Functions

### Function Registration Order

Functions are registered in `arc.zig:addToLinker()`. Order matters for native AOT element table filtering.

| Index | Function | Signature | Swift Equivalent |
|-------|----------|-----------|------------------|
| 0 | `cot_alloc` | `(i64, i64) → i64` | `swift_allocObject` |
| 1 | `cot_retain` | `(i64) → i64` | `swift_retain` |
| 2 | `cot_dealloc` | `(i64) → void` | `swift_deallocObject` → `swift_slowDealloc` |
| 3 | `cot_release` | `(i64) → void` | `swift_release` |
| 4 | `cot_realloc` | `(i64, i64) → i64` | No Swift equivalent |
| 5 | `cot_string_concat` | `(i64, i64, i64, i64) → i64` | — |
| 6 | `cot_memset_zero` | `(i64, i64) → void` | — |

Globals (after SP at index 0, CTXT at index 1):
- Global 2: `heap_ptr` (i32, mutable, init = HEAP_START)
- Global 3: `freelist_head` (i32, mutable, init = 0)

### `cot_alloc` — Allocation (`arc.zig:272-359`)

**Swift equivalent:** `swift_allocObject` (HeapObject.cpp:247-276)

```
cot_alloc(metadata_ptr: i64, size: i64) → i64:
    total_size = (size + 16 + 7) & ~7           // Header + payload, 8-byte aligned

    // Phase 1: Check freelist head (Zig WasmAllocator pattern)
    if (freelist_head != 0):
        block_size = i32.load(freelist_head + SIZE_OFFSET)
        if (block_size >= total_size):
            ptr = freelist_head
            freelist_head = i32.load(ptr + FREELIST_NEXT_OFFSET)  // Unlink
            found = 1

    // Phase 2: Bump allocation fallback
    if (!found):
        ptr = heap_ptr
        heap_ptr += total_size

    // Initialize header
    i32.store(ptr + SIZE_OFFSET, total_size)          // For freelist reuse
    i32.store(ptr + METADATA_OFFSET, metadata_ptr)    // Destructor table index
    i64.store(ptr + REFCOUNT_OFFSET, 1)               // Initial refcount = 1

    return ptr + USER_DATA_OFFSET
```

**Comparison with Swift:**

| Aspect | Swift `swift_allocObject` | Cot `cot_alloc` | Match? |
|--------|--------------------------|------------------|--------|
| Memory source | `swift_slowAlloc` → `malloc` | Freelist head check → bump fallback | Adapted |
| Metadata init | `HeapObject(metadata)` constructor | `i32.store(ptr + 4, metadata)` | Yes |
| Refcount init | `StrongExtraRefCount = 0` (means 1) | `i64.store(ptr + 8, 1)` | Yes |
| Alignment | Respects `requiredAlignmentMask` | Always 8-byte | Simplified |
| OOM handling | Aborts via `swift_abortAllocationFailure` | None (relies on Wasm trap) | Gap |
| Returns | Pointer to HeapObject (header start) | `ptr + USER_DATA_OFFSET` (user data) | Different convention |

### `cot_retain` — Strong Retain (`arc.zig:493-545`)

**Swift equivalent:** `swift_retain` (HeapObject.cpp:474-545)

```
cot_retain(obj: i64) → i64:
    if (obj == 0) return 0                              // Null check
    header_ptr = obj - USER_DATA_OFFSET
    old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
    if (old_count >= IMMORTAL_REFCOUNT) return obj      // Immortal check
    i64.store(header_ptr + REFCOUNT_OFFSET, old_count + 1)
    return obj                                          // Return for tail call optimization
```

**Comparison with Swift:**

| Aspect | Swift `swift_retain` | Cot `cot_retain` | Match? |
|--------|---------------------|-------------------|--------|
| Null check | `isValidPointerForNativeRetain` | `obj == 0` | Yes |
| Immortal check | Bottom 32 bits all set | `>= 0x7FFFFFFFFFFFFFFF` | Yes |
| Increment | Atomic CAS loop on `StrongExtraRefCount` | Non-atomic `i64.store(old + 1)` | Adapted (single-threaded) |
| Return value | Original pointer (spare bits preserved) | Original pointer | Yes |
| Side table | Slow path for side table entries | N/A | Intentional |

### `cot_release` — Strong Release (`arc.zig:553-640`)

**Swift equivalent:** `swift_release` (HeapObject.cpp:548-596) + `_swift_release_dealloc` (835-837)

```
cot_release(obj: i64) → void:
    if (obj == 0) return                                // Null check
    header_ptr = obj - USER_DATA_OFFSET
    old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
    if (old_count >= IMMORTAL_REFCOUNT) return          // Immortal check
    new_count = old_count - 1
    i64.store(header_ptr + REFCOUNT_OFFSET, new_count)

    if (new_count == 0):
        // Load metadata (destructor table index)
        metadata_ptr = i32.load(header_ptr + METADATA_OFFSET)

        if (metadata_ptr != 0):                         // CRITICAL: guard before load
            destructor_ptr = i32.load(metadata_ptr + DESTRUCTOR_OFFSET)
            if (destructor_ptr != 0):
                call_indirect destructor_type(obj)      // metadata->destroy(object)

        cot_dealloc(obj)                                // swift_deallocObject equivalent
```

**Comparison with Swift:**

| Aspect | Swift `swift_release` | Cot `cot_release` | Match? |
|--------|----------------------|---------------------|--------|
| Null check | `isValidPointerForNativeRetain` | `obj == 0` | Yes |
| Immortal check | Bottom 32 bits check | `>= IMMORTAL_REFCOUNT` | Yes |
| Decrement | Atomic CAS with `memory_order_release` | Non-atomic `i64.store(old - 1)` | Adapted |
| IsDeiniting flag | Set before calling destroy | Not needed (no concurrent access) | Simplified |
| Destructor call | `metadata->destroy(object)` | `call_indirect destructor_type(obj)` | Yes |
| Memory free | `swift_deallocObject` → `swift_slowDealloc` → `free()` | `cot_dealloc` → freelist push | Adapted |
| canBeFreedNow | Checks unowned/weak RC | N/A (no unowned/weak) | Intentional |
| metadata_ptr guard | Always valid (Swift guarantees metadata) | `if (metadata_ptr != 0)` | Extra safety |

**Critical bug fix (Feb 7, 2026):** The `metadata_ptr != 0` guard is essential. Without it,
when `metadata_ptr = 0` (no destructor), the code loads from linear memory address `0 + 8 = 8`,
which may be non-zero on native AOT. This caused SIGILL crashes on native because the
`call_indirect` dispatch chain's unreachable trap was hit.

### `cot_dealloc` — Deallocation (`arc.zig:363-395`)

**Swift equivalent:** `swift_deallocObject` (HeapObject.cpp:967-1070) → `swift_slowDealloc` (Heap.cpp:151-161)

```
cot_dealloc(obj: i64) → void:
    if (obj == 0) return
    header_ptr = obj - USER_DATA_OFFSET
    // Push onto freelist: freed_block.next = freelist_head; freelist_head = freed_block
    i32.store(header_ptr + FREELIST_NEXT_OFFSET, freelist_head)
    freelist_head = header_ptr
```

**Comparison with Swift:**

| Aspect | Swift `swift_deallocObject` | Cot `cot_dealloc` | Match? |
|--------|---------------------------|---------------------|--------|
| Free method | `free()` / `AlignedFree()` | Push to freelist | Adapted |
| Unowned/weak check | `canBeFreedNow()` → may defer to DEINITED state | N/A | Simplified |
| Debug clobber | `memset_pattern8` in debug builds | Not implemented | Gap |
| Null check | No (called internally only) | `obj == 0` guard | Extra safety |

**Why FREELIST_NEXT_OFFSET = METADATA_OFFSET (4)?**
Originally `FREELIST_NEXT_OFFSET` was aliased to `SIZE_OFFSET` (0), which clobbered the block's
`total_size` when the freed block was pushed to the freelist. This made first-fit allocation
impossible since `cot_alloc` couldn't read the freed block's size. Fixed Feb 7, 2026 by storing
the next pointer in the metadata slot (offset 4), which is irrelevant in freed blocks.

### `cot_realloc` — Reallocation (`arc.zig:401-489`)

**No Swift equivalent.** ARC objects have fixed sizes in Swift. This is a Cot extension for
growable containers, ported from C's `realloc` semantics.

```
cot_realloc(obj: i64, new_size: i64) → i64:
    if (obj == 0) return cot_alloc(0, new_size)      // null → fresh alloc

    header_ptr = obj - USER_DATA_OFFSET
    old_total = i32.load(header_ptr + SIZE_OFFSET)
    new_total = (new_size + HEADER_SIZE + 7) & ~7

    if (new_total <= old_total):
        i32.store(header_ptr + SIZE_OFFSET, new_total)  // Shrink in-place
        return obj

    // Grow: alloc new, copy payload, dealloc old
    new_obj = cot_alloc(0, new_size)
    memory.copy(new_obj, obj, old_total - HEADER_SIZE)
    cot_dealloc(obj)
    return new_obj
```

**Note:** On grow, metadata is passed as 0 since `cot_realloc` is only used by the `@realloc`
builtin (raw allocations). ARC objects have fixed sizes and never need realloc.

---

## ARC Insertion

### Ownership Model

Cot follows Swift's +0/+1 ownership convention:

| Expression | Ownership | Action | Swift SIL Equivalent |
|------------|-----------|--------|---------------------|
| `new Foo { x: 1 }` | +1 (owned) | Register cleanup only | `alloc_ref` |
| `createFoo()` (call returning ptr) | +1 (owned) | Register cleanup only | Function return |
| `let q = p` (copy from local) | +0 (borrowed) | `cot_retain(p)` + register cleanup | `copy_value` |
| `p.field` (field access) | +0 (borrowed) | `cot_retain` if base has cleanup | `copy_value` |
| Scope exit | — | `cot_release` for all active cleanups (LIFO) | `destroy_value` |
| `return p` | — | Disable cleanup (forward ownership to caller) | Consumed by return |
| `p = q` (reassignment) | — | `cot_release(old_p)` + `cot_retain(q)` | `strong_release` + `copy_value` |

### Type Predicate: `couldBeARC()` (`types.zig:325-332`)

Only `*StructType` (pointer-to-struct) triggers ARC. Raw pointers (`*i64`), value types, and
primitives are trivial.

```zig
pub fn couldBeARC(self: *const TypeRegistry, idx: TypeIndex) bool {
    const t = self.get(idx);
    if (t != .pointer) return false;
    return self.get(t.pointer.elem) == .struct_type;
}
```

**Swift equivalent:** `TypeLowering.isTrivial()` — only class types (heap-allocated) are non-trivial.

### Cleanup Stack (`arc_insertion.zig`)

Follows Swift's `CleanupManager` pattern — a unified LIFO stack for both ARC releases and defers.

```zig
pub const CleanupKind = enum {
    release,      // Emit cot_release(value) on scope exit
    end_borrow,   // Placeholder for future borrow checking
    defer_expr,   // Emit deferred expression on scope exit
};

pub const Cleanup = struct {
    kind: CleanupKind,
    value: NodeIndex,        // IR node to release, or AST node for defer
    type_idx: TypeIndex,
    state: CleanupState,     // dormant, dead, active
    local_idx: ?ir.LocalIdx, // Track which local this cleanup belongs to
};
```

**Key operations:**

| Operation | Method | When Used |
|-----------|--------|-----------|
| Register cleanup | `push(cleanup)` | After `let p = new Foo {}` or `let q = p` |
| Disable on return | `disableForLocal(local_idx)` | `return p` — forward ownership, skip release |
| Update on reassign | `updateValueForLocal(local_idx, new_value)` | `p = q` — update cleanup to release new value |
| Scope exit | `emitCleanups(target_depth)` | End of block — emit all cleanups down to depth, pop |
| Break/continue | `emitCleanupsNoPop(target_depth)` | Loop exit — emit cleanups but DON'T pop (other paths need them) |
| Defer + return | Capture into `__ret_tmp`, emit cleanups, load tmp | Zig semantics: return value captured before defers run |

### `baseHasCleanup()` (`lower.zig:411-423`)

Walks expression chains to determine if a value needs ARC. For `a.b.c`, checks if `a` has a cleanup.

```zig
fn baseHasCleanup(self: *Lowerer, base_idx: NodeIndex) bool {
    // For ident: check cleanup_stack.hasCleanupForLocal
    // For field_access: recurse on base
    // For index: recurse on base
}
```

### Assignment Patterns

**Local reassignment** (`lower.zig:786-795`):
```
p = q  →  release(old_p); retained_q = retain(q); store(p, retained_q)
```

**Deref assignment** (`lower.zig:812-816`):
```
*ptr = new_val  →  release(old_*ptr); store(*ptr, new_val)
```

**Field assignment** (`lower.zig:850-865`):
```
obj.field = new_val  →  release(old_field); retained = retain(new_val); store(obj.field, retained)
```

**Index assignment** (`lower.zig:898-915`):
```
arr[i] = new_val  →  release(old_arr[i]); retained = retain(new_val); store(arr[i], retained)
```

---

## Memory Allocator

### Linear Memory Layout (Wasm)

```
┌─────────────────────────────────────────────────────┐
│ 0x0000 - 0x0FFF:  Reserved (null guard page)        │
├─────────────────────────────────────────────────────┤
│ 0x1000 - 0xFFFF:  Data segments + stack             │
├─────────────────────────────────────────────────────┤
│ 0x10000 (HEAP_START):  Heap grows upward →          │
│                                                     │
│   [Header 16B][Payload]  [Header 16B][Payload]  ... │
│                                                     │
│                         ← heap_ptr points here      │
├─────────────────────────────────────────────────────┤
│ Unused (available for memory.grow)                  │
└─────────────────────────────────────────────────────┘
```

### Freelist Design (Zig WasmAllocator Pattern)

Freed blocks form a singly-linked list through the `FREELIST_NEXT_OFFSET` (offset 4, reusing the
metadata slot which is irrelevant in freed blocks).

```
freelist_head → [Block A] → [Block B] → [Block C] → 0 (empty)
                  ↑ next       ↑ next       ↑ next
                at offset 4  at offset 4  at offset 4

Block layout (freed):
+0: total_size (i32) — PRESERVED for first-fit checking
+4: next_ptr (i32)   — points to next freed block (or 0)
+8: (garbage)        — refcount no longer meaningful
+16: (garbage)       — user data no longer meaningful
```

**Allocation strategy:**
1. Check freelist HEAD — if head block's `total_size >= requested`, unlink and reuse
2. If no match (or empty freelist), bump allocate from `heap_ptr`

**Deallocation strategy:**
1. Push freed block onto freelist HEAD (LIFO)

This is O(1) for both alloc and dealloc. Head-only checking means larger freed blocks at
the back of the list may go unused, but for ARC objects (similar sizes), this works well.

### Native AOT Memory (`driver.zig`)

```
vmctx (16MB pre-allocated, zero-initialized):
+0x00000: Globals area (SP, CTXT, heap_ptr, freelist_head)
+0x10000: Stack pointer initial value
+0x20000: heap_data (heap_base ptr, heap_bound)
+0x40000: Linear memory starts here (12MB available)
          heap_ptr starts at vmctx + 0x40000 + HEAP_START
```

The native AOT wrapper (`_main`) initializes:
- `heap_base = vmctx + 0x40000`
- `heap_bound = 0x1000000 - 0x40000` (≈12MB)
- Memory is zero-initialized (important for the `metadata_ptr != 0` guard)

---

## Native AOT Considerations

### `call_indirect` for Destructors

On Wasm, `call_indirect` dispatches through a function table. On native AOT, this becomes an
if-else chain over element table entries, filtered by `type_index` (function signature).

```
// Native AOT translation of call_indirect:
if (callee_index == elem[0] && func_type[elem[0]] == destructor_type):
    call func_elem_0(obj)
elif (callee_index == elem[1] && func_type[elem[1]] == destructor_type):
    call func_elem_1(obj)
else:
    unreachable  // trap
```

**Critical lesson (Feb 6-7, 2026):** The element table contains ALL user functions, not just
function pointers. `cot_release` has the same signature `(i64) → void` as destructors, so it
appears in the dispatch chain. Without the `metadata_ptr != 0` guard, release could recursively
call itself or hit the unreachable trap.

### Element Table Filtering

`translateCallIndirect` in `translator.zig` filters element entries by `type_index`:
- Only entries where `func_to_type[func_idx] == type_index` get dispatch branches
- This prevents calling functions with mismatched signatures (e.g., `main: () → i64` from
  a destructor dispatch expecting `(i64) → void`)

---

## Builtins

### `@alloc(size: i64) → i64`

Raw allocation. Returns pointer to user data (after 16-byte header). Metadata = 0 (no destructor).

```cot
let buf = @alloc(1024)   // Allocate 1024 bytes
// buf points to user data, header at buf - 16
```

**Lowered to:** `cot_alloc(0, size)`

### `@dealloc(ptr: i64) → void`

Raw deallocation. Pushes the block to freelist. Does NOT call destructors.

```cot
@dealloc(buf)   // Return memory to freelist
```

**Lowered to:** `cot_dealloc(ptr)`

### `@realloc(ptr: i64, new_size: i64) → i64`

Resize allocation. If new size fits in current block, shrinks in-place. Otherwise alloc+copy+dealloc.

```cot
let buf2 = @realloc(buf, 2048)   // Grow to 2048 bytes
```

**Lowered to:** `cot_realloc(ptr, new_size)`

### Pipeline

```
Scanner:  '@' → .at token
Parser:   @ident(args) → BuiltinCall { name, type_arg, args[2], span }
Checker:  @alloc → i64, @dealloc → void, @realloc → i64
Lowerer:  emitCall("cot_alloc"/"cot_dealloc"/"cot_realloc", args)
```

---

## Test Coverage

### Wasm E2E Tests (9 ARC tests)

| Test | What it Verifies |
|------|-----------------|
| `ARC function returning new` | +1 ownership across function boundary |
| `ARC copy from local` | +0 copy with retain |
| `ARC reassignment` | Release old, retain new |
| `ARC return forwarding` | Disable cleanup on return (no double-free) |
| `ARC dealloc after release` | Destructor + dealloc chain |
| `ARC dealloc multiple objects` | Multiple alloc/release cycles |
| `builtin alloc and dealloc` | Raw @alloc/@dealloc |
| `builtin realloc` | @realloc grow path |
| `freelist reuse cycle` | Alloc-dealloc-alloc reuses freed memory |

### Native E2E Tests (9 ARC tests — mirror Wasm)

Same tests as Wasm, verified via process exit code on native ARM64 binary.

### Unit Tests (`arc.zig`)

| Test | What it Verifies |
|------|-----------------|
| `memory layout constants` | Header size = 16, offsets correct |
| `generateRetainBody produces valid bytecode` | Retain body ends with `0x0b` |
| `generateReleaseBody produces valid bytecode` | Release body ends with `0x0b` |
| `addToLinker creates functions` | 7 functions, 2 globals registered |

### Missing Coverage

| Gap | Priority | Notes |
|-----|----------|-------|
| Destructor dispatch with actual destructors | HIGH | No test has a struct with a deinit |
| Immortal objects | MEDIUM | No test exercises the immortal refcount path |
| Nested ARC structs | MEDIUM | Struct containing `*OtherStruct` field |
| ARC across closures | MEDIUM | Closure capturing ARC-managed object |
| OOM / memory exhaustion | LOW | No test pushes allocator to limits |

---

## Known Issues & Future Work

### Issues (Ordered by Risk)

#### 1. No `memory.grow` in `cot_alloc` (MEDIUM risk)

The allocator does not call `memory.grow` when bump allocation exceeds current linear memory.
On Wasm, this causes a trap. On native, the 16MB vmctx is pre-allocated but there's no growth
path if exceeded.

**Fix:** Add `memory.grow` call when `heap_ptr + total_size > memory.size * 65536`. Reference:
Go's `sbrk` in `runtime/mem_wasm.go:16-45`.

#### 2. No OOM handling (MEDIUM risk)

`cot_alloc` doesn't check for allocation failure. Swift aborts via `swift_abortAllocationFailure`.

**Fix:** After `memory.grow`, check return value. If -1, call `unreachable` (Wasm trap) or
return a sentinel value.

#### 3. `string_concat` bypasses ARC headers (LOW risk)

`generateStringConcatBody` allocates raw memory via bump allocation without headers. These
buffers can't be freed or tracked by the freelist.

**Fix:** When strings become ARC objects, use `cot_alloc` instead of raw bump allocation.

#### 4. Legacy API inconsistency (LOW risk)

`generateLegacyReleaseFunction` (used by old unit tests) doesn't have destructor dispatch
or the `metadata_ptr != 0` guard. Only called by `wasm.Module`-based tests, not production.

**Fix:** Eventually remove legacy API when all tests migrate to `Linker` API.

#### 5. Freelist only checks HEAD (LOW risk, design limitation)

If the freelist head block is too small but a subsequent block fits, it falls through to bump
allocation, wasting the larger block.

**Fix:** Walk the freelist (first-fit) or use size-class bins (Zig WasmAllocator pattern with
per-size-class freelists for O(1) alloc).

### Future Work (Swift Features Not Yet Ported)

| Feature | Swift Reference | Priority | Notes |
|---------|----------------|----------|-------|
| Weak references | `SideTableRefCounts`, `HeapObjectSideTableEntry` | HIGH | Needed for delegate patterns, UI bindings |
| Unowned references | `UnownedRefCount` (bits 1-31) | MEDIUM | Needed for parent-child without cycles |
| Cycle detection | Not in Swift (programmer responsibility) | LOW | Could add runtime cycle detector |
| `IsDeiniting` flag | Bit 32 in `InlineRefCounts` | LOW | Prevents retain during deinit |
| Atomic refcount ops | `compare_exchange_weak` with memory ordering | LOW | Only needed if Wasm threads added |
| Debug clobber | `memset_pattern8` on freed memory | LOW | Helps catch use-after-free |
| Side table overflow | Side table allocated when refcount bits overflow | VERY LOW | 30-bit strong RC is sufficient for now |
| Size-class bins | Zig WasmAllocator's `frees[size_class_count]` | MEDIUM | Better memory reuse for varied object sizes |

---

## File Map

| File | Lines | Purpose |
|------|-------|---------|
| `compiler/codegen/arc.zig` | 1098 | Runtime function bytecode generation (alloc, retain, release, dealloc, realloc) |
| `compiler/codegen/arc_insertion.zig` | 413 | CleanupStack, CleanupHandle, ManagedValue, Cleanup types |
| `compiler/frontend/lower.zig` | ~400 lines (scattered) | ARC insertion during IR lowering: `lowerLocalVarDecl`, `emitCleanups`, `emitCleanupsNoPop`, `baseHasCleanup`, reassignment |
| `compiler/frontend/types.zig` | 8 | `couldBeARC()` type predicate |
| `compiler/frontend/checker.zig` | 12 | `@alloc`/`@dealloc`/`@realloc` type checking |
| `compiler/frontend/parser.zig` | 25 | `@alloc`/`@dealloc`/`@realloc` parsing |
| `compiler/driver.zig` | ~90 | Runtime setup, vmctx initialization, element table |
| `compiler/codegen/wasm_e2e_test.zig` | ~100 | 9 Wasm ARC E2E tests |
| `compiler/codegen/native_e2e_test.zig` | ~90 | 9 Native ARC E2E tests |

### Key Locations for Debugging

| If you're debugging... | Start here |
|----------------------|------------|
| Header layout mismatch | `arc.zig:22-48` (constants) + `lower.zig:1814` (HEAP_HEADER_SIZE) |
| Refcount wrong | `arc.zig:493-545` (retain) + `arc.zig:553-640` (release) |
| Destructor not called | `arc.zig:616-631` (metadata_ptr guard + destructor dispatch) |
| Memory not freed | `arc.zig:363-395` (dealloc) + `arc.zig:295-333` (freelist check in alloc) |
| Retain/release not emitted | `lower.zig:562-590` (lowerLocalVarDecl) + `lower.zig:439-471` (emitCleanups) |
| Double free | `lower.zig:376` (disableForLocal on return) + `arc_insertion.zig:150` (disable) |
| Native crash in release | Check `metadata_ptr != 0` guard + element table filtering in `translator.zig:1414-1530` |
| Freelist corruption | `arc.zig:40` (FREELIST_NEXT_OFFSET must be METADATA_OFFSET, not SIZE_OFFSET) |

---

## Appendix: Swift's Complete Object Lifecycle

For reference, Swift's full object state machine (not all states are implemented in Cot):

```
LIVE (no side table)
  ├─ Strong RC > 0, Unowned RC = 1
  ├─ Weak ref created → allocate side table → LIVE (with side table)
  └─ Strong RC → 0 → DEINITING

DEINITING
  ├─ deinit() running (IsDeiniting = true)
  ├─ Strong operations are no-ops
  └─ canBeFreedNow()?
     ├─ Yes (Unowned RC = 1) → DEAD (swift_slowDealloc)
     └─ No → DEINITED (swift_unownedRelease)

DEINITED
  ├─ deinit complete, unowned refs outstanding
  └─ Unowned RC → 0 → FREED or DEAD

FREED (with side table only)
  ├─ Object freed, weak refs outstanding
  └─ Weak RC → 0 → DEAD (side table freed)

DEAD
  └─ Object and side table gone
```

**Cot currently implements:** LIVE → refcount=0 → destroy → DEAD (immediate free, no intermediate states).
