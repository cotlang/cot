# ARC Implementation Audit: Cot vs Swift

**Date**: February 18, 2026
**Status**: ALL 4 critical gaps FIXED (Feb 18, 2026)

## Reference Files

| Cot File | Swift Reference | Purpose |
|----------|----------------|---------|
| `compiler/codegen/wasm/arc.zig` | `references/swift/stdlib/public/runtime/HeapObject.cpp` | ARC runtime (retain/release/dealloc) |
| `compiler/frontend/arc_insertion.zig` | `references/swift/lib/SILGen/Cleanup.h` | Cleanup stack, managed values |
| `compiler/frontend/lower.zig` | `references/swift/lib/SILGen/SILGenExpr.cpp` | ARC insertion during lowering |

---

## Critical Gap 1: No Weak References

**Problem**: Cot has no `weak` or `unowned` references. Reference cycles (parent↔child, delegate patterns, closures capturing `self`) are permanent memory leaks with zero mitigation.

**Swift solution**: Side table allocation for objects with weak references. `WeakReference` with atomic zeroing on dealloc. Three reference count kinds: strong, unowned, weak.

**Fix plan**:
1. Add `weak` keyword to parser/checker
2. Add side table support in `arc.zig` runtime
3. `cot_weak_retain` / `cot_weak_release` / `cot_weak_load` runtime functions
4. Zeroing: when strong refcount hits 0, zero all weak refs via side table
5. `weak` variables compile to side table entries, loads check for zero

**Impact**: Without this, any bidirectional reference pattern leaks forever.

---

## Critical Gap 2: Collections Don't Release Contained Elements

**Problem**: `List(*Foo).free()` frees the backing buffer but does NOT release the pointed-to elements. `list.set(i, new_val)` doesn't release the old value it overwrites. Same for Map/Set. `list.clear()` just sets count=0 without releasing anything.

**Swift solution**: Collections use value witness tables to know how to destroy element types. Array.deinit releases all elements. Array subscript set releases old, retains new.

**Fix plan**:
1. Add `cot_release` calls in `List.free()` for each element when T is ARC-managed
2. Add `cot_release` for old value in `List.set()` before overwriting
3. Same pattern for `Map` and `Set` (keys and values)
4. `clear()` must release all elements before zeroing count
5. `orderedRemove()`/`swapRemove()` must release removed elements (or return them for caller to manage)

**Challenge**: Generic collections need to know at monomorphization time whether T is ARC-managed. The lowerer already has `couldBeARC()` — need to thread this into collection method instantiation.

---

## Critical Gap 3: Narrow `couldBeARC` Heuristic

**Problem**: `couldBeARC()` only returns true for `*StructType`. All stdlib collections store heap pointers as raw `i64` (`items: i64`), which is invisible to ARC. The ARC system can't see through the indirection.

**Swift solution**: Value witness tables carry destroy/copy/move operations for every type. Generic code calls through the witness table, so even type-erased values get proper ARC treatment.

**Fix plan**:
1. Track ARC-managed types through generic instantiation — when `List(T)` is instantiated with `T = *MyStruct`, mark the instantiated methods as containing ARC elements
2. In `lowerMethodCall` for collection methods, check if the generic type parameter is ARC-managed
3. Emit retain/release for elements at call sites where needed (insert, set, remove)
4. Alternative: make collections store a `needs_arc: bool` flag computed at monomorphization time

**Root cause**: Cot's i64-everything approach means pointer types lose their identity when stored in struct fields. The type system knows `T = *MyStruct` at generic instantiation, but the lowered code just sees `i64`.

---

## Critical Gap 4: Fragile Ownership Heuristic

**Problem**: `is_owned = new_expr or call` assumes ALL function calls return +1 (owned) values. A getter returning a borrowed reference gets treated as +1 → double-release when the cleanup fires.

**Swift solution**: 4 ownership kinds (Owned, Guaranteed, Unowned, None). Function signatures carry ownership convention for parameters and return values. SILGen tracks ownership precisely.

**Fix plan**:
1. Add return ownership convention to function types: `+1` (caller owns, default for new/allocating calls) vs `+0` (caller borrows)
2. Field getters and accessors return `+0` — no cleanup registered
3. `new`, constructors, and functions that allocate return `+1`
4. When assigning a `+0` value to a variable, emit `cot_retain` (already partially done)
5. Mark functions in IR with ownership convention

**Key insight from Swift**: The convention is on the FUNCTION TYPE, not guessed from the call site. `fn getItems() []i64` returns +0 (borrowed). `fn createList() *List` returns +1 (owned). The caller doesn't guess — the callee declares.

---

## What Cot Does Well (keep these)

- **Cleanup stack LIFO ordering** — correct, matches Swift
- **ManagedValue/forward pattern** — faithful Swift port, ownership transfer on return works
- **Error path handling** — errdefer + cleanup on error paths, well-implemented
- **Destructor dispatch** — metadata + table-based call_indirect, functional
- **Immortal refcount** — prevents wasteful ops on string constants
- **Null checks** — retain/release safely handle null pointers
- **Scope destroy** — auto-calls deinit for stack-allocated structs

---

## Implementation Order

**Dependencies**:
- Gap 3 (ARC detection) must come before Gap 2 (collection ARC) — collections need ARC to see their elements
- Gap 4 (ownership) is independent but improves correctness of all ARC operations
- Gap 1 (weak refs) is independent, most complex

**Order**:
1. **Gap 4: Ownership formalization** — fixes double-free risk, improves all subsequent work
2. **Gap 3: Widen ARC detection** — makes ARC visible for collection elements
3. **Gap 2: Collection element ARC** — uses widened detection to release elements properly
4. **Gap 1: Weak references** — most complex, independent feature

---

## Verification

After each fix:
1. `zig build test` — compiler internals pass
2. `cot test test/e2e/features.cot` — all features pass (native)
3. `cot test test/e2e/features.cot --target=wasm32` — all features pass (wasm)
4. `./test/run_all.sh` — 60/60 test files pass
5. New ARC-specific test file: `test/e2e/arc.cot` — targeted tests for each fix
