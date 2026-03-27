# Execution Plan: Fix Shared Generic Body Codegen (Port Swift IRGen 1:1)

**Date:** 2026-03-25
**Status:** PLAN — not yet implemented
**Blocks:** Phase 1 of self-hosting (selfcot can't compile files due to Map_get trap)
**Root cause:** SSA builder decomposes T-typed values using first instantiation's concrete type instead of treating them as opaque/address-only (Swift pattern)

## The Swift Rule (GenOpaque.cpp)

> In a shared generic body, ALL values whose type is a type parameter (T, K, V, etc.)
> are **address-only**. They are NEVER loaded into registers, NEVER decomposed into
> components (ptr+len for strings, tag+payload for optionals). All operations go
> through VWT witnesses or memcpy with runtime size from metadata.

Cot violates this rule. The SSA builder sees `type_idx == STRING` (from the first
instantiation) and decomposes values into ptr+len — even in shared bodies where
the actual runtime type could be `int` (8 bytes, not 24).

## Current Architecture

```
lower.zig          →  IR (ir.zig)  →  SSA builder (ssa_builder.zig)  →  SSA  →  CLIF  →  ARM64
                                       ↑ THIS IS WHERE THE BUG IS
Tracks T-indirect:     No concept      Decomposes based on
- indirect_t_params    of "opaque"     compile-time type_idx
- type_substitution    locals
- __metadata_T params
```

The lowerer (lower.zig) KNOWS which values are T-typed. The SSA builder does NOT.
The IR layer between them has no way to convey "this local is address-only."

## Target Architecture (Swift Pattern)

```
lower.zig          →  IR (ir.zig)  →  SSA builder (ssa_builder.zig)  →  SSA  →  CLIF  →  ARM64
                       ↑ NEW            ↑ FIXED
Tracks T-indirect:     Local.is_        Checks is_address_only
- indirect_t_params    address_only     BEFORE decomposing.
- type_substitution    = true           If true: return address,
- __metadata_T params  for T-typed      use memcpy/VWT witnesses.
                       locals
```

## Execution Steps

### Step 1: Add `is_address_only` flag to `ir.Local`

**File:** `compiler/frontend/ir.zig`
**What:** Add `is_address_only: bool = false` to the `Local` struct.

```zig
pub const Local = struct {
    name: []const u8,
    type_idx: TypeIndex,
    mutable: bool,
    is_param: bool = false,
    param_idx: ParamIdx = 0,
    size: u32 = 8,
    alignment: u32 = 8,
    offset: i32 = 0,
    is_address_only: bool = false,  // NEW: Swift address-only convention
    overlap_group: u16 = 0,
    overlap_arm: u16 = 0,
    // ...
};
```

**Why:** This is the bridge between lower.zig (which knows T-typed locals) and
the SSA builder (which needs to know to skip decomposition). Swift's equivalent
is `TypeLowering::isAddressOnly()` in `TypeInfo.h`.

**Verification:** `zig build` compiles. No behavioral change yet.

### Step 2: Mark T-typed locals as address-only in lower.zig

**File:** `compiler/frontend/lower.zig`

**2a: Mark T-indirect param locals**

In the shared body setup (~line 10198), after adding `__ptr_{name}` params:
```zig
if (type_param_name) |tp_name| {
    const ptr_name = try std.fmt.allocPrint(self.allocator, "__ptr_{s}", .{param.name});
    _ = try fb.addParam(ptr_name, TypeRegistry.I64, 8);
    try self.indirect_t_params.put(param.name, tp_name);
    // The param itself is not added, but the scalar shadow will be.
}
```

After scalar shadows are created (~line 10222), mark them:
```zig
// Phase 8.5: Create scalar shadow locals for T-indirect params.
for (indirect_t_params) {
    const shadow_local = fb.lookupLocal(param_name);
    if (shadow_local) |idx| {
        fb.markAddressOnly(idx);  // NEW
    }
}
```

**2b: Mark `var k = key` locals (early_init_tp path)**

In `lowerLocalVarDecl` (~line 2506), after the Phase 8.5 early memcpy:
```zig
if (early_init_tp) |tp_name| {
    // ... existing memcpy code ...
    fb.markLocalAddressOnly(local_idx);  // NEW
    return;
}
```

**2c: Mark method receiver locals when base is T-typed**

When lowering `k.hash()` where `k` is a T-typed local, the local should
already be marked from step 2b. No additional marking needed.

**2d: Mark locals created from T-typed pointer dereferences**

`var rk = @intToPtr(*K, addr).*` creates a local from a T-typed deref.
In the lowerer, after `lowerExprNode` for the deref, check if the result
type matches a type substitution and mark the target local.

**Verification:** Debug print local flags. All T-typed locals show `is_address_only=true`.

### Step 3: SSA builder — skip decomposition for address-only locals

**File:** `compiler/frontend/ssa_builder.zig`

**3a: `convertLoadLocal` — return address for address-only locals**

Current code (~line 678) checks type and decomposes strings/slices/compounds.
Add check BEFORE any decomposition:

```zig
fn convertLoadLocal(self: *SSABuilder, local_idx: ir.LocalIdx, type_idx: TypeIndex, cur: *Block) !*Value {
    // NEW: Swift GenOpaque.cpp — address-only types always returned by address
    if (local_idx < self.ir_func.locals.len and self.ir_func.locals[local_idx].is_address_only) {
        const addr_val = try self.emitLocalAddr(local_idx, type_idx, cur);
        return addr_val;
    }

    // ... existing decomposition code unchanged ...
}
```

**Why:** In Swift, loading an address-only value returns its ADDRESS, not a decomposed
value. The consumer (a VWT witness call or memcpy) operates on the address directly.

**3b: `convertStoreLocal` — use memcpy for address-only locals**

Current code (~line 781) decomposes strings/slices into separate stores.
Add check BEFORE decomposition:

```zig
fn convertStoreLocal(self: *SSABuilder, local_idx: ir.LocalIdx, value_idx: ir.NodeIndex, cur: *Block) !*Value {
    const value = try self.convertNode(value_idx) orelse return error.MissingValue;

    // NEW: Swift GenOpaque.cpp — address-only stores use memcpy
    if (local_idx < self.ir_func.locals.len and self.ir_func.locals[local_idx].is_address_only) {
        // The value should be an address (from convertLoadLocal on another address-only).
        // Store via memcpy: memcpy(local_addr, value_addr, local.size)
        const dst = try self.emitLocalAddr(local_idx, TypeRegistry.VOID, cur);
        const size_val = try self.func.newValue(.const_int, TypeRegistry.I64, cur, self.cur_pos);
        size_val.aux_int = @intCast(self.ir_func.locals[local_idx].size);
        try cur.addValue(self.allocator, size_val);
        // Emit memcpy call
        const mc = try self.func.newValue(.call, TypeRegistry.VOID, cur, self.cur_pos);
        mc.aux = .{ .string = "memcpy" };
        mc.addArg2(dst, value);
        try mc.addArgAlloc(size_val, self.allocator);
        try cur.addValue(self.allocator, mc);
        self.assign(local_idx, value);
        return value;
    }

    // ... existing decomposition code unchanged ...
}
```

**Why:** Swift never decomposes address-only stores. It uses `initializeWithCopy`
witness (which is memcpy for trivial types, memcpy+retain for ARC types).

**3c: `convertPtrLoadValue` — use memcpy for address-only dereferences**

Current code (~line 2014) decomposes STRING dereferences into ptr+len loads.
Add check BEFORE decomposition:

```zig
fn convertPtrLoadValue(self: *SSABuilder, ptr_idx: ir.NodeIndex, type_idx: TypeIndex, cur: *Block) !*Value {
    const ptr_val = try self.convertNode(ptr_idx) orelse return error.MissingValue;

    // NEW: Check if this dereference result is stored to an address-only local.
    // If the node's type matches an address-only local's type in the current
    // shared body context, treat it as opaque.
    // For now, check if type_idx matches any address-only local's type.
    if (self.isAddressOnlyType(type_idx)) {
        // Return the pointer value directly — the consumer will use it as an address.
        // Wrap in .copy to distinguish from the raw pointer local's address.
        const copy_val = try self.func.newValue(.copy, type_idx, cur, self.cur_pos);
        copy_val.addArg(ptr_val);
        try cur.addValue(self.allocator, copy_val);
        return copy_val;
    }

    // ... existing decomposition code unchanged ...
}
```

**3d: `convertPtrStoreValue` — use memcpy for address-only stores through pointers**

Current code (~line 2101) decomposes STRING stores into separate ptr+len stores.
Add check BEFORE decomposition:

```zig
fn convertPtrStoreValue(self: *SSABuilder, p: ir.PtrStoreValue, cur: *Block) !*Value {
    const ptr_val = try self.convertNode(p.ptr) orelse return error.MissingValue;
    const value = try self.convertNode(p.value) orelse return error.MissingValue;

    // NEW: If the value being stored is address-only, use memcpy
    if (self.isAddressOnlyType(value.type_idx)) {
        // value is an address (from convertLoadLocal/convertPtrLoadValue).
        // memcpy(dest_ptr, src_addr, size)
        const size_val = try self.func.newValue(.const_int, TypeRegistry.I64, cur, self.cur_pos);
        size_val.aux_int = @intCast(self.type_registry.sizeOf(value.type_idx));
        try cur.addValue(self.allocator, size_val);
        const mc = try self.func.newValue(.call, TypeRegistry.VOID, cur, self.cur_pos);
        mc.aux = .{ .string = "memcpy" };
        mc.addArg2(ptr_val, value);
        try mc.addArgAlloc(size_val, self.allocator);
        try cur.addValue(self.allocator, mc);
        return mc;
    }

    // ... existing decomposition code unchanged ...
}
```

**3e: Add `isAddressOnlyType` helper to SSABuilder**

```zig
/// Check if a type should be treated as address-only in this function.
/// True when any local with this type is marked is_address_only.
fn isAddressOnlyType(self: *SSABuilder, type_idx: TypeIndex) bool {
    for (self.ir_func.locals) |local| {
        if (local.is_address_only and local.type_idx == type_idx) return true;
    }
    return false;
}
```

**Verification:** The failing test `Map(string,int) + Map(int,int)` passes on native.

### Step 4: Fix memcpy sizes to use runtime metadata (not compile-time)

Steps 3b-3d use `local.size` (compile-time size from first instantiation) for memcpy.
This is wrong — for K=string, size=24, but for K=int at runtime, size=8. The memcpy
should use the RUNTIME size from `__metadata_K[1]`.

**However,** for Step 3 to work initially, compile-time sizes are acceptable because:
- memcpy(dst, src, 24) when actual size is 8 → copies 16 extra bytes (harmless if
  the local is allocated with 24 bytes, which it is from the first instantiation)
- The KEY insight: the local IS 24 bytes in the stack frame. Reading/writing 24 bytes
  is correct for the stack layout. The extra bytes are unused padding.

**For correctness with ARC types (Step 5),** runtime sizes matter because VWT
`initializeWithCopy` does retain on inner refs, and the witness needs the real metadata.

**This step is deferred to Step 5.** Step 3's compile-time memcpy is sufficient
to fix the Map_get trap.

### Step 5: Use VWT witnesses instead of raw memcpy for ARC types

**File:** `compiler/frontend/ssa_builder.zig`

Replace the `memcpy` calls from Step 3 with VWT `initializeWithCopy` dispatch:

```
// Load VWT pointer from metadata
vwt = load(metadata[0])
if (vwt == 0) {
    // Trivial type: plain memcpy
    size = load(metadata[1])
    memcpy(dst, src, size)
} else {
    // Non-trivial: call initializeWithCopy witness
    iwc = load(vwt + 2*8)  // VWT[2] = initializeWithCopy
    call_indirect(iwc, dst, src, metadata)
}
```

This matches the existing VWT dispatch pattern in lower.zig (~line 3492-3517).

**Verification:** Tests with ARC types (strings stored in Map values) don't leak or crash.

### Step 6: Fix equality comparison for address-only types

**File:** `compiler/frontend/lower.zig`

The Phase 8.8 equality comparison (lines 6019-6159) currently only triggers when
one operand is an IDENT in `indirect_t_params`. It should ALSO trigger when
either operand's type matches a type substitution value (i.e., the type is T).

Change the detection from:
```zig
const lhs_is_t = if (lhs_expr) |e| (if (e == .ident) self.indirect_t_params.contains(e.ident.name) else false) else false;
```

To:
```zig
const lhs_is_t = if (self.type_substitution) |sub| blk: {
    var it = sub.valueIterator();
    while (it.next()) |v| {
        if (v.* == self.inferExprType(bin.left)) break :blk true;
    }
    break :blk false;
} else false;
```

This catches `self.keyPtr(idx).* == key` where the LHS is a deref (not an ident)
but its TYPE is T (matches a type substitution).

**Verification:** Map equality comparisons use runtime dispatch for ALL T-typed operands.

### Step 7: Fix method calls for address-only receivers

**File:** `compiler/frontend/lower.zig`

The Phase 8.8 hash dispatch (lines 9418-9433) checks `type_substitution` to
detect T-typed receivers. This should ALSO apply to other protocol methods
(e.g., `eq()` if we add `Equatable` support, `compare()`, etc.).

For now, the hash dispatch is sufficient. But the detection should be generalized:

```zig
// In the method call path:
if (self.type_substitution != null) {
    if (self.type_substitution.?.get(receiver_type_param)) |_| {
        // This is a protocol method call on a T-typed receiver.
        // Dispatch through metadata witness table.
    }
}
```

**Verification:** All protocol method calls in shared bodies use runtime dispatch.

## Verification Plan

After all steps:

1. `zig build` — compiler builds
2. `./zig-out/bin/cot test test/e2e/features.cot` — 370/370 pass
3. `./test/run_all.sh` — 82/82 pass
4. Failing test passes on native:
   ```cot
   struct H { m1: Map(string, int), m2: Map(int, int) }
   test "bug" {
       var h = H { .m1 = .{}, .m2 = .{} }
       h.m1.set("x", 1)
       h.m2.set(0, 0); h.m2.set(1, 2); h.m2.set(2, 4); h.m2.set(3, 6)
       @assertEq(h.m2.get(0), 0)
   }
   ```
5. `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` — selfcot builds
6. `/tmp/selfcot test test/cases/arithmetic.cot` — selfcot can compile files
7. Wasm target still works: `./zig-out/bin/cot test test/e2e/features.cot --target=wasm`

## File Change Summary

| File | Changes |
|------|---------|
| `compiler/frontend/ir.zig` | Add `is_address_only: bool` to `Local` struct |
| `compiler/frontend/lower.zig` | Mark T-typed locals, fix equality detection |
| `compiler/frontend/ssa_builder.zig` | Check `is_address_only` in 4 convert functions, add helper |

## Risk Assessment

- **Step 1-2:** Zero risk — only adds a flag, no behavioral change
- **Step 3:** Medium risk — changes SSA generation for address-only locals. Must verify ALL 82 tests pass since the flag affects code generation for any program that uses generics.
- **Step 4:** Low risk — deferred, compile-time sizes work for now
- **Step 5:** Low risk — VWT dispatch already exists in lower.zig, just needs connection
- **Step 6-7:** Low risk — extends existing Phase 8.8 detection

## Notes

- This plan does NOT change the Wasm target — Wasm monomorphizes and doesn't use shared bodies.
- The `is_address_only` flag is per-local, not per-type. A type might be address-only in one function (shared body) but loadable in another (monomorphized).
- The compile-time `local.size` from the first instantiation is SAFE for stack allocation because the stack frame is sized for the largest instantiation. Extra bytes are padding.
