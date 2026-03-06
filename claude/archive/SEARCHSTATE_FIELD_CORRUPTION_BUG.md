# Bug: Struct Field Corruption After Repeated Method Calls on Heap-Allocated *Struct

## Status: FIXED (March 2026)

**Root cause**: `getLoadOp`/`getStoreOp` in `ssa_builder.zig` did not handle `enum_type` — enum(u8) fields used 8-byte `.load`/`.store` instead of 1-byte `.load8`/`.store8`. The 8-byte load from a sub-word enum field read adjacent field data, corrupting comparisons.
**Fix**: Added `enum_type` delegation to backing type in both `getLoadOp` and `getStoreOp`.
**Discovered in**: Cotty `libcotty/src/search.cot`
**Original description**: Any heap-allocated struct (`new T { ... }`) with interleaved List fields and scalar fields, when methods that mutate List fields are called 2+ times directly on the `*T` pointer

---

## 1. The Bug in One Sentence

Calling a method that mutates a `List` field on a heap-allocated `*StructType` corrupts the scalar field immediately after the List in the struct layout, starting from the **2nd method call**.

---

## 2. Affected Struct Layout

```cot
struct SearchState {
    active: bool,                // offset 0:  8 bytes (bool is i64)
    query: List(u8),             // offset 8:  24 bytes (items: i64, count: i64, capacity: i64)
    query_cursor: int,           // offset 32: 8 bytes  ← THIS FIELD GETS CORRUPTED
    matches: List(SearchMatch),  // offset 40: 24 bytes
    current_match: int,          // offset 64: 8 bytes
    replace_active: bool,        // offset 72: 8 bytes
    replace_text: List(u8),      // offset 80: 24 bytes
    replace_cursor: int,         // offset 104: 8 bytes
    focus: SearchFocus,          // offset 112: 8 bytes (enum(u8) but likely padded to i64)
    case_sensitive: bool,        // offset 120: 8 bytes
    dirty: bool,                 // offset 128: 8 bytes
}
```

The pattern: `List` field at offset 8 (`query`), followed by scalar `int` field at offset 32 (`query_cursor`). The scalar field gets corrupted after the 2nd call to any method that mutates the List via `self.query.append(...)`.

---

## 3. Reproduction

### Minimal test case (FAILS):

```cot
test "search insert two chars" {
    var s = SearchState.init()   // returns *SearchState via `new`
    s.insertChar('a')            // 1st call: query_cursor = 1 ✓
    s.insertChar('b')            // 2nd call: query_cursor should be 2, but ISN'T
    @assertEq(s.query.count, 2)      // ✓ passes — List mutation works
    @assertEq(s.query_cursor, 2)     // ✗ FAILS — query_cursor is corrupted
    @assertEq(s.dirty, true)         // ✓ passes — bool at end of struct is fine
}
```

### Expected vs actual behavior:

| After call | `query.count` | `query_cursor` | `dirty` |
|-----------|---------------|----------------|---------|
| 1st `insertChar('a')` | 1 ✓ | 1 ✓ | true ✓ |
| 2nd `insertChar('b')` | 2 ✓ | **corrupted** ✗ | true ✓ |

### Key observations:

1. **1st method call works perfectly** — all fields are correct
2. **2nd+ method calls corrupt `query_cursor`** — the scalar field immediately after `query: List(u8)` in the struct layout
3. **List fields themselves are fine** — `query.count` and `query.items` are correct after both calls
4. **Bool fields at the end of the struct are fine** — `dirty` persists correctly
5. **The Replace branch works for 2+ calls** — `replace_text.append()` + `replace_cursor += 1` both work correctly when `focus == SearchFocus.Replace`. This strongly suggests the corruption is specific to the first List→scalar field boundary in the struct layout
6. **The bug does NOT occur when called through a parent struct** — `Surface.searchOpen()` calls `self.search.insertChar()` (where `search` is a field on Surface), and this works correctly for any number of calls. All 386+ Cotty tests pass through this path.

### What does NOT reproduce the bug:

- Calling methods on stack-allocated SearchState (untested — `init()` returns `*SearchState` via `new`)
- Calling methods through a parent struct's field (`self.search.insertChar()` from Surface methods)
- Calling methods on the Replace branch (different List + scalar field pair)
- Reading/writing scalar fields directly (`s.query_cursor = 5` works fine)
- Non-mutating methods (e.g., `moveCursorLeft`, `overlayRows`) work fine

---

## 4. Hypothesis: Codegen Writes Back Stale `self` Copy

The most likely root cause: when a method calls `self.query.append(x)`, the compiler:

1. Loads `self` (the `*SearchState` pointer) into a register
2. Computes the address of `self.query` (offset 8)
3. Calls `List.append()` on that address — this may **reallocate** the List's backing storage
4. After the List method returns, the compiler writes back other fields from a **stale copy** of the struct that was loaded in step 1

If `List.append()` triggers a reallocation, and the compiler cached the entire struct in registers/stack before the call, the writeback in step 4 would overwrite `query_cursor` with its pre-call value (0 instead of the incremented value from the previous iteration).

This would explain why:
- 1st call works (no stale data yet, `query_cursor` starts at 0 and becomes 1)
- 2nd call fails (`query_cursor` gets written back as the value from before the method body executes — either 0 or 1 instead of 2)
- Replace branch works (different struct offset region, possibly different register allocation)
- Parent struct access works (different codegen path for `self.field.method()` vs direct `ptr.method()`)

### Alternative hypothesis: ARC-related

The `new` allocation returns an ARC-managed pointer. If ARC operations (retain/release) around method dispatch cause the struct to be copied/moved, the field at the List→scalar boundary could be corrupted by an ARC bookkeeping write that overlaps with the scalar field offset.

---

## 5. Workaround

Access SearchState methods through a parent struct (Surface) rather than directly:

```cot
// WORKS — called from Surface method
fn searchOpen() void {
    self.search.open()
    // ... self.search.insertChar() works correctly for N calls
}

// BROKEN — direct unit test on *SearchState
test "insert two chars" {
    var s = SearchState.init()
    s.insertChar('a')
    s.insertChar('b')
    // s.query_cursor is corrupted
}
```

This is NOT a code workaround (per Cotty CLAUDE.md rules) — the SearchState code is correct. The test limitations are documented, and the real code path through Surface works correctly.

---

## 6. Files

| File | Relevance |
|------|-----------|
| `cot-land/cotty/libcotty/src/search.cot` | Affected struct + methods + tests |
| `cot-land/cotty/libcotty/src/surface.cot` | Working path (Surface.searchOpen → self.search.insertChar) |
| Compiler: `src/codegen/` or `src/backends/` | Likely location of fix — method dispatch on `*StructType` with nested struct fields |

---

## 7. Suggested Investigation

1. Add a test to the compiler test suite with a minimal struct: `struct S { list: List(u8), counter: int }` with a method that calls `self.list.append(x)` then `self.counter += 1`. Verify `counter` after 2 calls.
2. Dump the generated native code (or WASM) for the method — check if `self.counter += 1` reads from a stale copy of `self` that was loaded before the `List.append` call.
3. Compare codegen for direct `ptr.method()` vs `parent.field.method()` — the latter works, the former doesn't.
4. Check if the bug reproduces with non-ARC allocation (stack-allocated struct passed by pointer).
