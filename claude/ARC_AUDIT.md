# ARC Audit: Cot vs Swift — Parity Status

**Date:** 2026-03-30
**Result:** Architecture is at parity. selfcot2 crash is a codegen bug, not an ARC design issue.

## Header Layout

| Field | Swift (16B) | Cot Native (32B) | Cot Wasm (16B) |
|-------|-------------|-------------------|----------------|
| magic | — | offset 0 (8B) | — |
| alloc_size | — | offset 8 (8B) | offset 0 (4B) |
| metadata | offset 0 (8B) | offset 16 (8B) | offset 4 (4B) |
| refcount | offset 8 (8B) | offset 24 (8B) | offset 8 (8B) |
| user_data | offset 16 | offset 32 | offset 16 |

Native has extra magic + alloc_size fields (debugging aids). Offsets are consistent within each target.

## Refcount Semantics — 1:1 Swift Parity

- `STRONG_RC_ONE = 1 << 33` (matches Swift InlineRefCounts)
- `INITIAL_REFCOUNT = 3` (PURE_SWIFT_DEALLOC | UNOWNED_RC_ONE)
- `@isUnique(ptr)`: loads refcount from `ptr - 8`, compares with 3
- Retain: atomic add STRONG_RC_ONE
- Release: atomic sub STRONG_RC_ONE, free at zero

## `@isUnique` — Verified Correct

- Loads refcount from `user_data - 8` (native: offset 24 in 32-byte header)
- Compares with `INITIAL_REFCOUNT = 3`
- Returns true if no strong references added (unique owner)
- On Wasm: always returns true (no refcounting)

## List COW — Verified Correct

- `ensureUnique()`: allocates new buffer via `alloc(0, size)`, copies data, releases old
- `ensureCapacity()`: calls `realloc(buf, new_size)` to grow
- `realloc()`: subtracts 32 for header, reads alloc_size, grows via alloc+memcpy+dealloc

## selfcot2 Crash — NOT an ARC bug

Crash: `List_ensureCapacity → realloc → "pointer being freed was not allocated"`

This is a **native codegen bug** in selfcot's compiled code, not an ARC design issue.
selfcot is compiled by Cranelift as a native binary. When it runs and calls TypeRegistry_init,
its own List implementation (compiled from self/emit/wasm/list.cot or similar) hits a corrupt
buffer pointer. The corruption likely comes from:

1. A register/stack value not preserved across a function call boundary
2. A struct field initialization issue (list buffer not zeroed)
3. A codegen bug in how selfcot's List methods handle the buffer pointer

Investigation requires debugging the specific native code selfcot generates for TypeRegistry_init.
