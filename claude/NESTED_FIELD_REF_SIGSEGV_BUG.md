# Bug: Passing `&self.nested.field` to Free Functions Causes SIGSEGV

## Status: FIXED (March 2026)

**Root cause**: `lowerAddrOf` in `lower.zig` only handled `.field_access` when the base was `.ident` (1-level). For nested field access (`&x.y.z`), it fell through to return `ir.null_node`, causing SIGSEGV.
**Fix**: Replaced the `.field_access` handling to use `resolveStructFieldAddr` recursively for any nesting depth. Regression tests added to `test/e2e/memory.cot` (3 tests: 1-level, 2-level, via ptr).
**Note**: Fix works on native. Wasm has different struct addr semantics (GC refs) so nested field addr tests are native-only.
**Discovered in**: Cotty `libcotty/src/surface.cot` calling functions from `movement.cot`
**Original description**: Any method on a large heap-allocated struct that passes `&self.field.subfield` as a pointer argument to a free function

---

## 1. The Bug in One Sentence

When a method on a heap-allocated `*Surface` (or any large `*StructType`) passes `&self.document.buffer` (a reference to a nested struct field) as an argument to an imported free function, the program crashes with SIGSEGV.

---

## 2. Affected Pattern

```cot
// movement.cot — free function taking *Buffer
fn moveWordForward(buf: *Buffer, offset: int) int {
    const len = buf.len()          // ← crashes here
    // ...
}

// surface.cot — method on *Surface
fn handleWordForward() void {
    // THIS CRASHES:
    const new_pos = moveWordForward(&self.document.buffer, self.cursor.offset)
    //                               ^^^^^^^^^^^^^^^^^^^^^^^^
    //                               nested field ref passed to free function
}
```

The `&self.document.buffer` expression should produce a valid `*Buffer` pointer (the address of the `buffer` field within the `document` field within `self`). Instead, the pointer appears to be invalid — dereferencing it in the free function causes SIGSEGV.

---

## 3. What Works vs What Breaks

### WORKS — direct access in the same method:
```cot
fn handleWordForward() void {
    // Inlined logic — accessing self.document.buffer directly
    const len = self.document.buffer.len()      // ✓ works
    const ch = self.document.buffer.charAt(pos) // ✓ works
}
```

### WORKS — free functions in their own tests:
```cot
test "moveWordForward" {
    var buf = Buffer.initWithContent("hello world")
    const result = moveWordForward(&buf, 0)  // ✓ works
    @assertEq(result, 5)
}
```

### BREAKS — method passing nested field ref to free function:
```cot
fn handleWordForward() void {
    const new_pos = moveWordForward(&self.document.buffer, self.cursor.offset)
    // ✗ SIGSEGV
}
```

### All affected functions (from movement.cot):
| Function | Signature |
|----------|-----------|
| `movePosLeft` | `fn movePosLeft(buf: *Buffer, offset: int) int` |
| `movePosRight` | `fn movePosRight(buf: *Buffer, offset: int) int` |
| `moveUp` | `fn moveUp(buf: *Buffer, offset: int, goal_col: int) int` |
| `moveDown` | `fn moveDown(buf: *Buffer, offset: int, goal_col: int) int` |
| `moveLineStart` | `fn moveLineStart(buf: *Buffer, offset: int) int` |
| `moveLineEnd` | `fn moveLineEnd(buf: *Buffer, offset: int) int` |
| `moveWordForward` | `fn moveWordForward(buf: *Buffer, offset: int) int` |
| `moveWordBackward` | `fn moveWordBackward(buf: *Buffer, offset: int) int` |
| `moveWordEnd` | `fn moveWordEnd(buf: *Buffer, offset: int) int` |

---

## 4. Struct Layout Context

```cot
struct Surface {
    kind: SurfaceKind,           // offset 0
    document: Document,          // offset 8 — contains buffer, history, selection, etc.
    cursor: Cursor,              // further offset
    // ... 20+ more fields
    // Total struct size: ~300+ bytes
}

struct Document {
    buffer: Buffer,              // offset 0 within Document — gap buffer (items, gap_start, gap_end, data_count)
    history: History,            // offset 32+
    selection: Selection,        // further
    // ...
}
```

So `&self.document.buffer` is `self + offsetof(Surface, document) + offsetof(Document, buffer)`. For a heap-allocated `*Surface` (via `new`), `self` is a valid heap pointer, and the computed address should be valid.

---

## 5. Hypothesis: Incorrect Address Computation for Nested Field References

The compiler likely miscomputes the address of `&self.document.buffer` when:
1. `self` is a heap-allocated `*StructType` obtained via `new` (ARC-managed)
2. The struct is large (300+ bytes)
3. The nested access is 2+ levels deep (`self` → `document` → `buffer`)
4. The result is passed as an argument to a **cross-module** (imported) free function

Possible codegen issues:
- **Offset miscalculation**: The compiler may compute `&self.document` (one level) instead of `&self.document.buffer` (two levels), producing a pointer to `Document` rather than `Buffer`
- **ARC interference**: The compiler may insert ARC retain/release around the nested field access that corrupts the pointer value
- **Register clobbering**: The address may be computed correctly but then overwritten by calling convention setup for the cross-module function call

The fact that **direct access within the same method works** (`self.document.buffer.len()`) but **passing the address to a free function fails** suggests the issue is specifically in how nested field addresses are materialized as function arguments.

---

## 6. Current Workaround

All movement functions have been **inlined** into Surface methods in `surface.cot`. Instead of calling `moveWordForward(&self.document.buffer, offset)`, the Surface method directly accesses `self.document.buffer.charAt(...)` etc. This is documented in the codebase as a compiler bug workaround, not a design choice.

```cot
// surface.cot — inlined version (WORKS)
fn moveOffsetWordForward(offset: int) int {
    const buf_len = self.document.buffer.len()
    if (offset >= buf_len) { return buf_len }
    var pos = offset
    const start_cat = charCategory(self.document.buffer.charAt(pos))
    // ... rest of logic uses self.document.buffer directly
}
```

The free functions in `movement.cot` still exist and pass their own unit tests — they just can't be called from Surface methods.

---

## 7. Files

| File | Relevance |
|------|-----------|
| `cot-land/cotty/libcotty/src/surface.cot` | Affected call sites (inlined workaround) |
| `cot-land/cotty/libcotty/src/movement.cot` | Free functions that can't be called from Surface |
| Compiler: codegen for `&self.field.subfield` as function argument | Root cause |

---

## 8. Suggested Investigation

1. **Minimal repro**: Create a 2-file test: `a.cot` has `struct Big { inner: Inner }` with `Inner` containing a `List` field, and a method that passes `&self.inner` to a function in `b.cot`. Verify SIGSEGV.
2. **Dump generated assembly/WASM**: Compare the address computed for `&self.inner.list` when used inline vs when passed as an argument to an imported function.
3. **Test with 1-level nesting**: Does `&self.buffer` (direct field, not nested) work when passed to a free function? This would isolate whether the bug is about nesting depth or about passing any field ref to a cross-module function.
4. **Test with stack-allocated struct**: Does the bug reproduce with a stack-allocated `Big` struct, or only with heap-allocated `*Big` from `new`?
5. **Check ARC on field refs**: Verify that `&self.document.buffer` does not accidentally trigger ARC retain/release on the `Document` or `Buffer` — field addresses should be raw pointer arithmetic, not ARC-managed.
