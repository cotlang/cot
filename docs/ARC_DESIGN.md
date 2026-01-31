# ARC Design (Based on Swift)

## Swift Reference Sources

| File | Purpose |
|------|---------|
| `~/learning/swift/stdlib/public/SwiftShims/swift/shims/HeapObject.h` | HeapObject struct definition |
| `~/learning/swift/stdlib/public/SwiftShims/swift/shims/RefCount.h` | RefCount bit layout and operations |
| `~/learning/swift/stdlib/public/runtime/HeapObject.cpp` | swift_retain/release implementation |
| `~/learning/swift/include/swift/Runtime/RuntimeFunctions.def` | Runtime function declarations |
| `~/learning/swift/include/swift/SIL/SILNodes.def` | SIL instructions: strong_retain, strong_release |

---

## Swift's Object Layout (64-bit)

```c
struct HeapObject {
    HeapMetadata const *metadata;  // +0: type metadata pointer (8 bytes)
    InlineRefCounts refCounts;     // +8: reference counts (8 bytes)
};
// Size: 16 bytes (SWIFT_ABI_HEAP_OBJECT_HEADER_SIZE_64)
```

Swift's embedded mode (closer to Wasm constraints):
```c
struct HeapObject {
    HeapMetadata const *metadata;  // +0: type metadata pointer
    uintptr_t embeddedRefcount;    // +8: simple refcount
};
```

---

## Cot's Object Layout (Wasm32)

Following Swift's embedded mode pattern for simplicity:

```
┌──────────────────────────────────────────┐
│ CotHeapObject (12 bytes on wasm32)       │
├──────────────────────────────────────────┤
│ +0: metadata: i32   (type info pointer)  │
│ +4: refcount: i64   (reference count)    │
├──────────────────────────────────────────┤
│ +12: user data starts here               │
└──────────────────────────────────────────┘
```

### Why 64-bit refcount on 32-bit wasm?
- Matches Swift's InlineRefCounts size
- Leaves room for future flags (weak, unowned bits)
- Prevents overflow without extra checking
- 8-byte alignment for user data

### Immortal Objects
Following Swift's pattern:
```c
// Swift's embedded immortal (from HeapObject.h)
static const uintptr_t EmbeddedImmortalRefCount = 0x7fffffffffffffff; // i64 max / 2
```

Immortal objects never deallocate. Used for:
- Global constants
- Static strings
- Singleton objects

---

## Memory Layout Constants

```zig
pub const HEAP_OBJECT_HEADER_SIZE: u32 = 12;  // metadata(4) + refcount(8)
pub const METADATA_OFFSET: u32 = 0;
pub const REFCOUNT_OFFSET: u32 = 4;
pub const USER_DATA_OFFSET: u32 = 12;

pub const IMMORTAL_REFCOUNT: i64 = 0x7FFFFFFFFFFFFFFF;  // Max positive i64
pub const INITIAL_REFCOUNT: i64 = 1;
```

---

## Runtime Functions

### cot_alloc(metadata: i32, size: i32) -> i32

Allocates a new heap object with initial refcount of 1.

```
fn cot_alloc(metadata: i32, size: i32) -> i32 {
    total_size = HEAP_OBJECT_HEADER_SIZE + size
    ptr = heap_bump_alloc(total_size)

    // Initialize header
    i32.store(ptr + METADATA_OFFSET, metadata)
    i64.store(ptr + REFCOUNT_OFFSET, INITIAL_REFCOUNT)

    // Return pointer to user data
    return ptr + USER_DATA_OFFSET
}
```

### cot_retain(obj: i32) -> i32

Increments the reference count. Returns the object pointer (for tail call optimization).

```
fn cot_retain(obj: i32) -> i32 {
    // Null check (Swift pattern)
    if obj == 0:
        return 0

    header_ptr = obj - USER_DATA_OFFSET
    refcount_ptr = header_ptr + REFCOUNT_OFFSET

    old_count = i64.load(refcount_ptr)

    // Immortal check (Swift pattern)
    if old_count >= IMMORTAL_REFCOUNT:
        return obj

    new_count = old_count + 1
    i64.store(refcount_ptr, new_count)
    return obj
}
```

### cot_release(obj: i32) -> void

Decrements the reference count. Frees the object if count reaches zero.

```
fn cot_release(obj: i32) {
    // Null check
    if obj == 0:
        return

    header_ptr = obj - USER_DATA_OFFSET
    refcount_ptr = header_ptr + REFCOUNT_OFFSET
    metadata_ptr = header_ptr + METADATA_OFFSET

    old_count = i64.load(refcount_ptr)

    // Immortal check
    if old_count >= IMMORTAL_REFCOUNT:
        return

    new_count = old_count - 1
    i64.store(refcount_ptr, new_count)

    if new_count == 0:
        // Load metadata to get destructor
        metadata = i32.load(metadata_ptr)

        // Call destructor if present (via metadata lookup)
        deinit_fn = metadata_get_deinit(metadata)
        if deinit_fn != 0:
            call_indirect(deinit_fn, obj)

        // Free the allocation
        heap_free(header_ptr)
}
```

### cot_retain_count(obj: i32) -> i64

Returns the current reference count (for debugging/testing).

```
fn cot_retain_count(obj: i32) -> i64 {
    if obj == 0:
        return 0

    header_ptr = obj - USER_DATA_OFFSET
    refcount_ptr = header_ptr + REFCOUNT_OFFSET
    return i64.load(refcount_ptr)
}
```

### cot_is_uniquely_referenced(obj: i32) -> i32

Returns 1 if refcount is exactly 1 (for copy-on-write optimization).

```
fn cot_is_uniquely_referenced(obj: i32) -> i32 {
    if obj == 0:
        return 0

    count = cot_retain_count(obj)
    return count == 1 ? 1 : 0
}
```

---

## Compiler Integration

### SSA Operations

```zig
// op.zig - Generic ARC ops
retain,   // 1 arg: object pointer, has_side_effects=true
release,  // 1 arg: object pointer, has_side_effects=true

// op.zig - Wasm-lowered ARC ops
wasm_lowered_retain,   // Calls cot_retain
wasm_lowered_release,  // Calls cot_release
```

### Lowering Pass (lower_wasm.zig)

```zig
.retain => val.op = .wasm_lowered_retain,
.release => val.op = .wasm_lowered_release,
```

### Code Generation (wasm_gen.zig)

```zig
.wasm_lowered_retain => {
    try self.getValue32(v.args[0]);  // Push object pointer
    try self.code.emitCall(self.runtime_retain_idx);
    try self.code.emitDrop();  // Discard return value
},

.wasm_lowered_release => {
    try self.getValue32(v.args[0]);  // Push object pointer
    try self.code.emitCall(self.runtime_release_idx);
},
```

---

## Metadata System (Future)

For M15, we use a simple metadata value (type ID). Future milestones will expand:

```
TypeMetadata {
    kind: i32,           // Type kind (class, struct, etc.)
    size: i32,           // Instance size
    alignment: i32,      // Required alignment
    deinit: i32,         // Destructor function index (0 if none)
    // ... more fields
}
```

---

## Memory Allocator (Heap Management)

For M15, we use a simple bump allocator. Future milestones will add:
- Free list for reuse
- Size classes for efficiency
- Compaction (optional)

### Bump Allocator Layout

```
Linear Memory:
┌─────────────────────────────────────────────────────────┐
│ 0x0000 - 0x0FFF: Reserved (null guard)                  │
├─────────────────────────────────────────────────────────┤
│ 0x1000 - 0x1FFF: Globals (SP, heap_ptr, etc.)           │
├─────────────────────────────────────────────────────────┤
│ 0x2000 - 0x?????: Stack (grows down from SP)            │
├─────────────────────────────────────────────────────────┤
│ HEAP_START - END: Heap (grows up from heap_ptr)         │
└─────────────────────────────────────────────────────────┘
```

### heap_bump_alloc(size: i32) -> i32

```
global heap_ptr: i32 = HEAP_START

fn heap_bump_alloc(size: i32) -> i32 {
    // Align to 8 bytes
    aligned_size = (size + 7) & ~7

    ptr = heap_ptr
    heap_ptr = heap_ptr + aligned_size

    // Check for OOM (would need memory.grow)
    if heap_ptr > memory_size:
        memory.grow(1)  // Grow by 1 page (64KB)

    return ptr
}
```

### heap_free(ptr: i32)

For M15, this is a no-op (bump allocator doesn't free). Future milestones will implement proper freeing.

---

## Test Cases

### 1. Basic Retain/Release Cycle
```cot
fn test_basic_arc() {
    let obj = Object.new()  // refcount = 1
    retain(obj)             // refcount = 2
    release(obj)            // refcount = 1
    release(obj)            // refcount = 0, freed
}
```

### 2. Null Pointer Safety
```cot
fn test_null_safety() {
    retain(null)   // No-op, returns null
    release(null)  // No-op
}
```

### 3. Immortal Objects
```cot
fn test_immortal() {
    let str = "hello"  // Static string, immortal
    retain(str)        // No-op (immortal)
    release(str)       // No-op (immortal)
    // String is never freed
}
```

### 4. Uniquely Referenced Check
```cot
fn test_unique() {
    let obj = Object.new()
    assert(is_uniquely_referenced(obj))  // true
    let alias = obj
    retain(alias)
    assert(!is_uniquely_referenced(obj)) // false
    release(alias)
    assert(is_uniquely_referenced(obj))  // true again
}
```

---

## Implementation Checklist for M15

1. [x] Add `retain`, `release` ops to op.zig
2. [x] Add `wasm_lowered_retain`, `wasm_lowered_release` ops
3. [x] Create compiler/codegen/arc.zig with Wasm function generation
4. [x] Add memory layout constants (HEAP_OBJECT_HEADER_SIZE, offsets)
5. [x] Implement cot_alloc, cot_retain, cot_release in Wasm
6. [x] Add runtime functions to Wasm module during linking (wasm_e2e_test.zig)
7. [x] Update lower_wasm.zig to lower retain/release
8. [x] Update wasm_gen.zig to emit calls to runtime functions
9. [x] Add comprehensive tests (arc.zig, wasm_gen.zig integration tests)
10. [x] Update documentation

---

## Future Milestones

### M15+ Enhancements
- Weak references (side table pattern from Swift)
- Unowned references
- Cycle detection/collection
- Thread-safe atomics (when Wasm threads available)
- Destructor chaining
- Move semantics optimization

### References for Future Work
- Swift's `SideTableRefCounts` for weak references
- Swift's `HeapObjectSideTableEntry` for overflow handling
- Swift's `_swift_release_dealloc` for deinitialization sequence
