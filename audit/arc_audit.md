# ARC Implementation Audit

## Reference
- Primary: `~/learning/swift/stdlib/public/runtime/HeapObject.cpp`
- Secondary: `~/learning/swift/stdlib/public/SwiftShims/swift/shims/RefCount.h`

## Summary

**Status: CORRECTLY COPIES Swift's proven patterns**

The ARC implementation follows Swift's embedded ARC design with appropriate adaptations for Wasm32.

---

## Memory Layout Constants

| Constant | Cot Value | Swift Reference | Status |
|----------|-----------|-----------------|--------|
| HEAP_OBJECT_HEADER_SIZE | 12 | 16 (64-bit), 12 (32-bit) | ✅ Correct |
| METADATA_OFFSET | 0 | 0 | ✅ Matches |
| REFCOUNT_OFFSET | 4 | 8 (64-bit), 4 (32-bit) | ✅ Correct |
| USER_DATA_OFFSET | 12 | 16 (64-bit), 12 (32-bit) | ✅ Correct |
| IMMORTAL_REFCOUNT | 0x7FFFFFFFFFFFFFFF | EmbeddedImmortalRefCount | ✅ Matches |
| INITIAL_REFCOUNT | 1 | 1 | ✅ Matches |

---

## Function-by-Function Audit

### 1. generateRetainBody() → swift_retain

**Cot (arc.zig lines 117-170):**
```zig
// if (obj == 0) return 0
// header_ptr = obj - USER_DATA_OFFSET
// old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
// if (old_count >= IMMORTAL_REFCOUNT) return obj
// i64.store(header_ptr + REFCOUNT_OFFSET, old_count + 1)
// return obj
```

**Swift (HeapObject.cpp swift_retain):**
```cpp
HeapObject *swift::swift_retain(HeapObject *object) {
  SWIFT_RT_TRACK_INVOCATION(object, swift_retain);
  if (isValidPointerForNativeRetain(object))
    object->refCounts.increment(1);
  return object;
}
```

**RefCount.h increment():**
```cpp
void increment(uint32_t inc = 1) {
  auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_RELAXED);
  if (oldbits.isImmortal(true)) return;
  refCounts.store(oldbits.incrementStrongExtraRefCount(inc));
}
```

**Assessment:** ✅ COPIES Swift pattern
- Null check → return early
- Immortal check → return without increment
- Increment → store new value
- Return object (tail call optimization)

---

### 2. generateReleaseBody() → swift_release

**Cot (arc.zig lines 173-237):**
```zig
// if (obj == 0) return
// header_ptr = obj - USER_DATA_OFFSET
// old_count = i64.load(header_ptr + REFCOUNT_OFFSET)
// if (old_count >= IMMORTAL_REFCOUNT) return
// new_count = old_count - 1
// i64.store(header_ptr + REFCOUNT_OFFSET, new_count)
// if (new_count == 0) { /* TODO: free */ }
```

**Swift (HeapObject.cpp swift_release):**
```cpp
void swift::swift_release(HeapObject *object) {
  if (isValidPointerForNativeRetain(object))
    object->refCounts.decrementAndMaybeDeinit(1);
}
```

**RefCount.h decrementAndMaybeDeinit():**
```cpp
bool decrementShouldDeinit(uint32_t dec) {
  auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_RELAXED);
  if (oldbits.isImmortal(true)) return false;
  auto newbits = oldbits.decrementStrongExtraRefCount(dec);
  refCounts.store(newbits);
  return newbits.getStrongExtraRefCount() == 0;
}
```

**Assessment:** ✅ COPIES Swift pattern
- Null check → return early
- Immortal check → return without decrement
- Decrement → store new value
- Zero check → placeholder for deallocation

**Note:** Deallocation deferred to future milestone (documented in code).

---

### 3. generateLegacy*Functions

These follow the same Swift patterns but use i32 pointers for the old wasm.Module API.

| Function | Swift Reference | Status |
|----------|-----------------|--------|
| generateLegacyAllocFunction | swift_allocObject | ✅ Copies allocation pattern |
| generateLegacyRetainFunction | swift_retain | ✅ Copies retain pattern |
| generateLegacyReleaseFunction | swift_release | ✅ Copies release pattern |
| generateLegacyRetainCountFunction | getStrongExtraRefCount | ✅ Copies read-only query |
| generateLegacyIsUniqueFunction | isUniquelyReferenced | ✅ Copies COW check (count == 1) |

---

## Deviations from Swift (Intentional)

| Aspect | Swift | Cot | Reason |
|--------|-------|-----|--------|
| Atomic operations | CAS loops | Simple load/store | Wasm is single-threaded |
| Memory freeing | Free list + destructor | TODO placeholder | Deferred to M16+ |
| Side tables | Weak/unowned refs | Not implemented | Out of scope for M15 |
| Thread safety | Full atomic support | None | Wasm constraint |

---

## Concerns

1. **No atomics** - Acceptable for single-threaded Wasm, but should add comment documenting this assumption.

2. **Incomplete free path** - Release marks objects dead but doesn't free. Expected for M15, needs M16+ work.

3. **No destructor invocation** - Metadata lookup for destructors not implemented. Marked TODO.

---

## Conclusion

**PASSES AUDIT** - The implementation correctly copies Swift's ARC patterns, with appropriate simplifications for Wasm32. No invented logic detected.
