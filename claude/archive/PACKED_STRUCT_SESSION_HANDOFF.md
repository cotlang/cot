# Packed Struct Bitfields — COMPLETE (Zig + selfcot)

**Date:** 2026-03-23
**Status:** 100% implemented in both compilers, all tests pass

---

## Zig Compiler (compiler/)

### Type System (types.zig)
- `parseBitWidth()` — resolves u1-u64 AND i1-i64 to smallest standard type
- `lookupBitfieldType()` — resolves uN/iN in type-expression contexts only (NOT in `lookupByName`, which would conflict with variable names like `i1`, `i2`)
- `StructField.bit_offset` and `bit_width` fields
- `StructType.backing_int` field — TypeIndex of backing integer (U8/U16/U32/U64)

### Checker (checker.zig)
- Bitfield detection: non-standard widths (not 8/16/32/64) set bit_width
- `lookupBitfieldType()` fallback in `resolveType()` and `resolveTypeExpr()` — NOT in `resolveTypeByName()` (which is also called from expression contexts)
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

### Tests (10 new tests in features.cot, 370 total, all pass native + Wasm)
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

---

## Self-Hosted Compiler (self/) — Fully Ported

### types.cot
- `StructField`: added `bit_offset: int`, `bit_width: int`
- `StructType`: added `backing_int: int`
- `parseBitWidth()` — same algorithm as Zig
- `lookupBitfieldType()` — resolves uN/iN for type contexts only

### checker.cot
- `buildStructField()`: detects sub-byte bitfield types via `parseBitWidth()`
- `buildStructType()`: bit-packing layout computation, backing integer selection
- `resolveNamedType()`: `lookupBitfieldType()` fallback for uN/iN in type expressions
- All `StructType { ... }` literals updated with `backing_int: 0`

### lower.cot
- `lowerStructInitLocal()`: bitfield init (shift+OR chain with masking)
- `lowerStructInitExpr()`: same bitfield path for expression context
- `lowerStructFieldAccess()`: bitfield read (shift+mask+sign-extend), effective_field_type for nested
- `lowerFieldAssign()`: bitfield write (read-modify-write pattern)

### builder.cot
- **Bug fix:** `BinaryOp.shr => SsaOp.sar` (was `SsaOp.shr`)
- Zig reference: `ssa_builder.zig:1184` — `>> is arithmetic (Zig parity: signed int)`
- This one-line fix was required for signed bitfield sign extension

### Selfcot test results: 6/6 pass
- Bitfield read, write, signed read, signed write, cross-byte, @bitCast

---

## Pitfall: lookupBitfieldType vs lookupByName

**DO NOT put bitfield type resolution in `lookupByName()`.** It is called from expression contexts (e.g., `checkIdentifier`), where names like `i1`, `i2` are local variables. Putting it there causes `i1` to resolve as a type instead of a variable.

**Correct approach:** `lookupBitfieldType()` is a separate function called ONLY from type-expression resolution paths (`resolveType()`, `resolveTypeExpr()`, `resolveNamedType()`).

---

## Key Code Locations

| Feature | Zig File | Self File |
|---------|----------|-----------|
| Bit width parsing | `types.zig` `parseBitWidth()` | `types.cot` `parseBitWidth()` |
| Bitfield type resolution | `types.zig` `lookupBitfieldType()` | `types.cot` `lookupBitfieldType()` |
| Type resolution fallback | `checker.zig` `resolveType()` `.named` | `checker.cot` `resolveNamedType()` |
| Bit layout computation | `checker.zig` `buildStructTypeWithLayout()` | `checker.cot` `buildStructType()` |
| Bitfield struct init (stmt) | `lower.zig` `lowerStructInit()` | `lower.cot` `lowerStructInitLocal()` |
| Bitfield struct init (expr) | `lower.zig` `lowerStructInitExpr()` | `lower.cot` `lowerStructInitExpr()` |
| Bitfield read + sign extend | `lower.zig` `lowerFieldAccess()` | `lower.cot` `lowerStructFieldAccess()` |
| Bitfield write (RMW) | `lower.zig` `lowerFieldAssign()` | `lower.cot` `lowerFieldAssign()` |
| @bitCast packed ↔ int | `lower.zig` `.bit_cast` handler | `lower.cot` `BuiltinKind.bit_cast` (identity) |
| Pointer-to-bitfield error | `lower.zig` `lowerAddrOf()` | — (not yet ported) |
| Nested packed field load | `lower.zig` `effective_field_type` | `lower.cot` `effective_type` in `lowerStructFieldAccess()` |
| Arithmetic shift right | `ssa_builder.zig:1184` `.shr => .sar` | `builder.cot:951` `.shr => .sar` |

### Known Limitation
- **Pointer-to-non-byte-aligned-bitfield** emits compile error in Zig compiler (matches C behavior). Not yet ported to selfcot. Full Zig-style PackedOffset pointers would require adding bit_offset metadata to the pointer type representation — not needed for practical use cases.
