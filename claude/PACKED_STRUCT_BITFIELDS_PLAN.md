# Packed Struct Bitfields — Execution Plan

**Date:** 2026-03-23
**Status:** Planning
**Goal:** Add sub-byte bitfield support to Cot's `packed struct`
**Prerequisite for:** cot-js library (NaN-encoded Ref type), FFI/C interop, network protocols

---

## Current State

Cot already has `packed struct` with **byte-level packing** (no inter-field padding):

```cot
packed struct PackedSmall { a: u8, b: u32, c: u8 }
// Layout: a at offset 0 (1 byte), b at offset 1 (4 bytes), c at offset 5 (1 byte)
// Total: 6 bytes (no alignment padding)
```

**What works:** `packed struct` keyword, parser, checker layout, field access, 3 passing tests.
**What's missing:** Sub-byte fields (bitfields) — fields smaller than 8 bits.

---

## Target Feature

```cot
packed struct(u64) Ref {
    id: u32,          // bits 0-31
    type_id: u3,      // bits 32-34
    head: u29,        // bits 35-63
}

// Usage:
var r = Ref { id: 42, type_id: 2, head: 0x1FFC0000 }
println(r.id)       // → 42
println(r.type_id)  // → 2
println(@sizeOf(Ref))  // → 8 (backed by u64)

// Bitcast to/from backing integer:
const raw: u64 = @bitCast(u64, r)
const ref2: Ref = @bitCast(Ref, raw)
```

---

## Reference Implementation: Zig

**Files:**
- `references/zig/src/Type.zig` lines 1719-1731, 3550-3578
- `references/zig/src/InternPool.zig` lines 2081-2112, 4015-4080, 6146

**Zig's design:**
1. Each packed struct has a **backing integer type** (u8, u16, u32, u64, u128) determined by total bit width
2. Each field has a `PackedOffset = packed struct(u32) { host_size: u16, bit_offset: u16 }`
3. Field access: load backing integer → shift right by bit_offset → mask to field width
4. Field write: load backing integer → clear bits → insert new value → store

**Runtime codegen for reading `ref.type_id` (3 bits at offset 32):**
```
load u64 from &ref
i64.const 32
i64.shr_u          // shift right by bit_offset
i64.const 7        // mask = (1 << 3) - 1
i64.and             // extract 3 bits
```

**Runtime codegen for writing `ref.type_id = 5`:**
```
load u64 from &ref
i64.const 0xFFFFFFFF8FFFFFFFF  // clear mask: ~(7 << 32)
i64.and                         // clear old bits
i64.const 5
i64.const 32
i64.shl                         // shift new value to position
i64.or                          // insert new bits
store u64 to &ref
```

---

## Implementation Plan

### Phase 1: Type System — Bit-Level Field Layout
**Files:** `compiler/frontend/types.zig`, `compiler/frontend/checker.zig`

- [ ] **1.1** Add `bit_offset` and `bit_width` to `StructField`
  ```zig
  pub const StructField = struct {
      name: []const u8,
      type_idx: TypeIndex,
      offset: u32,          // byte offset (existing)
      bit_offset: u8 = 0,   // NEW: bit position within the byte-aligned word (0-63)
      bit_width: u8 = 0,    // NEW: field width in bits (0 = full type size)
      default_value: NodeIndex = null_node,
  };
  ```

- [ ] **1.2** Add `backing_int` to `StructType`
  ```zig
  pub const StructType = struct {
      ...
      backing_int: TypeIndex = invalid_type,  // NEW: for packed(uN) structs
  };
  ```

- [ ] **1.3** Update `buildStructTypeWithLayout` in checker.zig
  - When `layout == .packed` and any field type has bit_width < 8:
    - Compute total bit width across all fields
    - Assign `bit_offset` to each field (cumulative)
    - Set `backing_int` to smallest uN that fits
    - Set struct `size` = sizeOf(backing_int)
  - When `layout == .packed` and all fields are byte-aligned:
    - Keep existing byte-packed behavior (backward compatible)

- [ ] **1.4** Support `packed struct(u64)` syntax
  - Parser: accept optional `(uN)` after `packed struct`
  - Checker: validate total bit width fits in specified backing type
  - If no `(uN)`: auto-select backing type from total bit width

### Phase 2: Parser — Bitfield Type Syntax
**Files:** `compiler/frontend/parser.zig`, `compiler/frontend/checker.zig`

- [ ] **2.1** Accept sub-byte integer types: `u1`, `u2`, `u3`, ..., `u7`
  - Parser already handles `u8`, `u16`, `u32`, `u64`
  - Extend to accept any `uN` where 1 ≤ N ≤ 64
  - Store as BasicKind or as a sized integer type with explicit bit width

- [ ] **2.2** Accept `packed struct(uN)` syntax
  - Parse `(u32)` or `(u64)` after `packed struct` keyword
  - Store backing type in AST StructDecl

- [ ] **2.3** Validate bitfield constraints
  - Total bit width must fit in backing integer
  - Individual field bit width must match type (u3 = 3 bits, u29 = 29 bits)
  - No pointers or strings in bitfield structs (must be integer types only)

### Phase 3: Lowering — Bit-Level Field Access
**Files:** `compiler/frontend/lower.zig`

- [ ] **3.1** Detect bitfield access in `lowerFieldAccess`
  - If `field.bit_width > 0` and `field.bit_width < type_size_bits`:
    - Emit bit extraction code (shift + mask) instead of byte load
  - If `field.bit_width == 0`: existing byte-level access (backward compatible)

- [ ] **3.2** Emit read codegen
  ```
  // Read field at bit_offset B with width W from backing type of size S bytes:
  load<uS>(base_addr)           // load entire backing integer
  shr_u(bit_offset)             // shift right by B
  and((1 << W) - 1)             // mask to W bits
  ```

- [ ] **3.3** Emit write codegen
  ```
  // Write value V to field at bit_offset B with width W:
  load<uS>(base_addr)           // load entire backing integer
  and(~(((1 << W) - 1) << B))  // clear field bits
  or(V << B)                    // insert new value
  store<uS>(base_addr)          // write back
  ```

- [ ] **3.4** Emit struct literal init for bitfields
  - Build the backing integer from all field values:
    ```
    result = (field0 << 0) | (field1 << bit_offset_1) | (field2 << bit_offset_2) | ...
    ```

### Phase 4: SSA / Wasm Codegen
**Files:** `compiler/ssa/passes/lower_wasm.zig`, `compiler/codegen/wasm/gen.zig`

- [ ] **4.1** No new SSA ops needed
  - Bitfield access decomposes to existing ops: `load`, `shr`, `and`, `shl`, `or`, `store`
  - The lowerer (Phase 3) emits these existing ops

- [ ] **4.2** Verify Wasm codegen handles the patterns
  - `i64.shr_u`, `i64.and`, `i64.shl`, `i64.or` are all existing Wasm ops
  - No new Wasm instructions needed

### Phase 5: @bitCast Support
**Files:** `compiler/frontend/checker.zig`, `compiler/frontend/lower.zig`

- [ ] **5.1** Support `@bitCast(TargetType, value)` between packed structs and integers
  - `@bitCast(u64, ref)` → reinterpret the backing integer as u64
  - `@bitCast(Ref, raw_u64)` → reinterpret u64 as the packed struct
  - For packed structs, this is a no-op (the struct IS the integer)

- [ ] **5.2** Support `@bitCast` between f64 and u64
  - Needed for NaN-encoded refs: `@bitCast(f64, ref_u64)` checks if it's a number
  - Lowers to `wasm_f64_reinterpret_i64` / `wasm_i64_reinterpret_f64`

### Phase 6: Selfcot Port
**Files:** `self/check/checker.cot`, `self/build/lower.cot`

- [ ] **6.1** Port bit-level layout computation to selfcot checker
- [ ] **6.2** Port bit-level field access codegen to selfcot lowerer
- [ ] **6.3** Port @bitCast to selfcot

### Phase 7: Tests
**Files:** `test/e2e/features.cot`, `test/cases/structs.cot`

- [ ] **7.1** Basic bitfield struct
  ```cot
  packed struct(u8) Flags { read: u1, write: u1, exec: u1, reserved: u5 }
  test "bitfield u8" {
      var f = Flags { read: 1, write: 1, exec: 0, reserved: 0 }
      @assertEq(f.read, 1)
      @assertEq(f.write, 1)
      @assertEq(f.exec, 0)
      @assertEq(@sizeOf(Flags), 1)
  }
  ```

- [ ] **7.2** NaN-encoded ref (the motivating use case)
  ```cot
  packed struct(u64) Ref {
      id: u32,
      type_id: u3,
      head: u29,
  }
  test "NaN ref" {
      const NAN_HEAD: u29 = 0x1FFC0000
      var r = Ref { id: 42, type_id: 2, head: NAN_HEAD }
      @assertEq(r.id, 42)
      @assertEq(r.type_id, 2)
      @assertEq(@sizeOf(Ref), 8)
  }
  ```

- [ ] **7.3** @bitCast between packed struct and integer
  ```cot
  test "bitcast ref to u64" {
      var r = Ref { id: 1, type_id: 1, head: NAN_HEAD }
      const raw = @bitCast(u64, r)
      const r2 = @bitCast(Ref, raw)
      @assertEq(r2.id, 1)
      @assertEq(r2.type_id, 1)
  }
  ```

- [ ] **7.4** Cross-byte bitfields
  ```cot
  packed struct(u16) CrossByte { low: u6, high: u10 }
  test "cross byte" {
      var c = CrossByte { low: 63, high: 1023 }
      @assertEq(c.low, 63)
      @assertEq(c.high, 1023)
  }
  ```

- [ ] **7.5** Backward compatibility — existing packed struct tests still pass
  ```cot
  packed struct PackedSmall { a: u8, b: u32, c: u8 }
  // Should still be 6 bytes, not reinterpreted as bitfield
  ```

- [ ] **7.6** Wasm target tests
  - All above tests pass with `--target=wasm`

- [ ] **7.7** Native target tests
  - All above tests pass on native

---

## Effort Estimate

| Phase | Files Changed | Estimated Lines | Complexity |
|-------|--------------|-----------------|-----------|
| 1. Type system | types.zig, checker.zig | ~100 | Medium |
| 2. Parser | parser.zig, checker.zig | ~50 | Low |
| 3. Lowering | lower.zig | ~150 | High (bit manipulation codegen) |
| 4. SSA/Wasm | (verify only) | ~0 | Low |
| 5. @bitCast | checker.zig, lower.zig | ~50 | Medium |
| 6. Selfcot | checker.cot, lower.cot | ~200 | Medium |
| 7. Tests | features.cot, structs.cot | ~100 | Low |
| **Total** | | **~650 lines** | |

---

## Implementation Order

```
Phase 1 (type system)    ← Foundation: bit_offset/bit_width in StructField
Phase 2 (parser)         ← Syntax: packed struct(u64), u3/u5/u29 types
Phase 3 (lowering)       ← Codegen: shift/mask for field access
Phase 5 (@bitCast)       ← Utility: convert between packed struct and integer
Phase 7 (tests)          ← Verify everything works
Phase 4 (verify)         ← Confirm SSA/Wasm handles the patterns
Phase 6 (selfcot)        ← Port to self-hosted compiler
```

---

## Backward Compatibility

**Existing packed structs are NOT affected.** The rule:
- If ALL fields are byte-sized (u8, u16, u32, u64, i8, i16, i32, i64): use byte-packed layout (existing behavior)
- If ANY field has sub-byte width (u1-u7, u9-u15, etc.): use bit-packed layout (new behavior)
- `packed struct(uN)` syntax explicitly selects bit-packed with specific backing type

This ensures all existing tests pass without modification.

---

## Key Design Decision: Zig's Approach

We follow Zig's design because:
1. Cot's compiler is ported from Zig — the patterns map naturally
2. `PackedOffset(bit_offset, host_size)` is mathematically precise
3. Single backing integer means all field access is load→shift→mask (simple codegen)
4. `@bitCast` between packed struct and integer is a no-op (zero cost)
5. Go doesn't have bitfields (we're going beyond Go here)
6. Rust's `#[repr(packed)]` doesn't support sub-byte fields (we're going beyond Rust too)

This puts Cot at parity with Zig for struct layout — the most advanced of the reference languages.
