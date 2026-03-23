# Packed Struct Bitfields — COMPLETE

**Date:** 2026-03-23
**Status:** 100% implemented

---

## All Features Implemented

### Type System (types.zig)
- `parseBitWidth()` — resolves u1-u64 AND i1-i64 to smallest standard type
- `lookupByName()` — maps iN to I8/I16/I32/I64 (signed), uN to U8/U16/U32/U64
- `StructField.bit_offset` and `bit_width` fields
- `StructType.backing_int` field — TypeIndex of backing integer (U8/U16/U32/U64)

### Checker (checker.zig)
- Bitfield detection: non-standard widths (not 8/16/32/64) set bit_width
- Bit-packed layout: recomputes all field offsets in bits
- Backing integer selection from total bit width (≤8→U8, ≤16→U16, ≤32→U32, ≤64→U64)
- Error for total bit width > 64

### Lowerer (lower.zig)
1. **Struct init (statement)** — shift+OR chain with value masking for signed fields
2. **Struct init (expression)** — same bitfield path in `lowerStructInitExpr`
3. **Field read** — shift right + mask extraction from backing integer
4. **Signed field read** — sign extension via `(val << (64-width)) >> (64-width)` using arithmetic shift
5. **Field write** — read-modify-write: load → clear bits → OR new value → store
   - Handles local, pointer, and global base paths
6. **@bitCast** — packed struct ↔ integer is no-op (same representation)
7. **Pointer-to-bitfield** — compile error for non-byte-aligned fields
8. **Nested packed struct** — field loads use backing_int type for proper integer typing
   - `effective_field_type` mechanism: packed struct fields loaded as their backing int
   - Chained access (`outer.inner.a`) works via recursive expression evaluation

### Tests (10 new tests in features.cot, all pass native + Wasm)
- Bitfield read (u32 + u3 + u29)
- Bitfield write (middle field, preserves neighbors)
- Bitfield write first field
- Signed bitfield init + read (i3)
- Signed bitfield write
- Cross-byte field (u6 + u10 in u16)
- @bitCast packed → integer
- @bitCast roundtrip (packed → int → packed)
- Nested packed struct chained access
- Nested packed struct via variable

### Known Limitation
- **Pointer-to-non-byte-aligned-bitfield** emits compile error (matches C behavior).
  Full Zig-style PackedOffset pointers would require adding bit_offset metadata to
  the pointer type representation — not needed for practical use cases.

---

## Key Code Locations

| Feature | File | Lines |
|---------|------|-------|
| Signed/unsigned bit width parsing | `types.zig` | `parseBitWidth()`, `lookupByName()` |
| Bit layout computation | `checker.zig` | `buildStructTypeWithLayout()` |
| Bitfield struct init (stmt) | `lower.zig` | `lowerStructInit()` backing_int path |
| Bitfield struct init (expr) | `lower.zig` | `lowerStructInitExpr()` backing_int path |
| Bitfield read + sign extend | `lower.zig` | `lowerFieldAccess()` bitfield path |
| Bitfield write (RMW) | `lower.zig` | `lowerFieldAssign()` backing_int path |
| @bitCast packed ↔ int | `lower.zig` | `.bit_cast` handler |
| Pointer-to-bitfield error | `lower.zig` | `lowerAddrOf()` field_access path |
| Nested packed field load | `lower.zig` | `effective_field_type` in `lowerFieldAccess()` |
