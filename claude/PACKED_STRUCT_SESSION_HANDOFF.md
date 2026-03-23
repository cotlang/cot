# Packed Struct Bitfields — Session Handoff

**Date:** 2026-03-23
**Status:** 40% implemented, needs full rewrite from Zig reference

---

## What Was Done This Session

### Working (keep):
- `parseBitWidth()` in types.zig — resolves u1-u63 to smallest standard type
- `StructField.bit_offset` and `bit_width` fields added to types.zig
- `StructType.backing_int` field added to types.zig
- Checker detects bitfields and computes bit positions correctly
- `@sizeOf` returns correct backing integer size

### Working but incomplete (rewrite with Zig reference):
- Struct init: shift+OR chain builds backing integer — correct pattern but needs sign handling
- Field read: shift+mask extracts unsigned value — missing sign extension

### MISSING (must implement):
1. **Field write codegen** — read-modify-write with mask
2. **Signed bitfield extraction** — arithmetic right shift for iN types
3. **PackedOffset on pointer types** — Zig InternPool.zig:2104-2112
4. **Pointer-to-bitfield** — `&packed_struct.bitfield` must track bit offset
5. **Nested packed struct access** — chained PackedOffset
6. **@bitCast between packed struct and integer** — no-op reinterpret

---

## Zig Reference Files (READ BEFORE IMPLEMENTING)

| What | File | Lines | Key Concept |
|------|------|-------|-------------|
| PackedOffset struct | `references/zig/src/InternPool.zig` | 2104-2112 | `{ host_size: u16, bit_offset: u16 }` |
| Backing int type | `references/zig/src/InternPool.zig` | 6146 | `backing_int_ty` on packed struct |
| Field layout | `references/zig/src/Type.zig` | 3550-3578 | `packedStructFieldPtrInfo()` |
| Bit size | `references/zig/src/Type.zig` | 1719-1731 | `bitSizeInner()` for packed |
| Pointer semantics | `references/zig/src/Sema.zig` | 6254, 27514-27518 | Field pointer with packed_offset |
| Validation | `references/zig/src/Sema.zig` | 19176-19218 | host_size validation |

---

## Current Test Status

```cot
// PASSES:
packed struct Ref { id: u32, type_id: u3, head: u29 }
var r = Ref { .id = 42, .type_id = 2, .head = 0 }
@assertEq(r.id, 42)      // ✓ read works
@assertEq(r.type_id, 2)  // ✓ read works
@assertEq(@sizeOf(Ref), 8) // ✓ size correct

// WILL BREAK (not tested yet):
r.type_id = 5             // ✗ field write — NOT IMPLEMENTED
var p = &r.id             // ✗ pointer to bitfield — NOT IMPLEMENTED
const x: i3 = -2          // ✗ signed bitfield read — NOT IMPLEMENTED
```

---

## Implementation Plan (For Next Session)

### Step 1: Study Zig's codegen for packed field access
- `grep -rn 'packed_offset\|host_size\|structFieldPtr.*packed' references/zig/src/codegen.zig`
- Understand how Zig generates the read-modify-write pattern

### Step 2: Add PackedOffset to Cot's IR
- Add to pointer type representation
- Thread through field access lowering

### Step 3: Implement field write (read-modify-write)
```
old = load backing_int
cleared = old & ~(mask << bit_offset)
new = cleared | ((value & mask) << bit_offset)
store backing_int = new
```

### Step 4: Implement signed bitfield extraction
```
unsigned = (backing >> bit_offset) & mask
if type is signed and (unsigned >> (bit_width - 1)) & 1:
    signed = unsigned | (~0 << bit_width)  // sign extend
```

### Step 5: Tests
- Field write test
- Signed bitfield test (i3, i5)
- Pointer-to-bitfield test (&r.type_id)
- @bitCast test
- Cross-byte field test (u6 + u10 in u16)

---

## Other Session Work (All Committed + Pushed)

This session accomplished:
1. COT_SSA HTML visualizer (Go GOSSAFUNC port) — 8 phases complete
2. Pipeline logging in both Zig compiler + selfcot — 15 files instrumented
3. Per-pass timing + post-pass verification
4. Compiler bugs fixed: anonymous enum literal args, enum == .variant, @tagName + interpolation
5. self/debug.cot production debug infrastructure
6. JS bridge: extern fn → Wasm imports → generated JS with stubs
7. Cotty Browser plan + web/ directory structure
8. cot-js repo initialized with zig-js reference
9. Packed struct bitfields (partial — needs rewrite)
10. Wasm codegen docs: WASM_CODEGEN_REFERENCE.md, WASM_ASYNC_ARCHITECTURE.md
