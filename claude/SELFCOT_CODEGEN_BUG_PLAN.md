# Selfcot Codegen Bug — Execution Plan

**Date:** 2026-03-16
**Bug:** selfcot crashes in `emitMove` during SSA build of `TypeRegistry_makePointer`
**Blocking:** Wasm self-hosting (types.cot, ir.cot, scanner.cot, and all larger files)

---

## Evidence Summary

### Crash Location
```
source.cot:64: trap
  SSABuilder_emitMove + 900
  SSABuilder_convertStoreLocalField + 2032
  SSABuilder_convertNode + 4304
  SSABuilder_build + 3112
  generateAndAddFunc + 1528
```

### Key Facts
1. `@panic` guards INSIDE `emitMove` don't fire — crash is in a callee (List.get/set/append)
2. The crash is `List(int).get: idx=0, count=0` — an empty list accessed at index 0
3. The `source.cot:64` is a monomorphization artifact — wrong file:line
4. The `test_complex.cot` repro (simpler version of the same pattern) **passes**
5. The real `TypeRegistry_makePointer` has a 16-variant `Type` union (much larger struct)
6. Zig compiler native path compiles and runs the test_complex repro correctly
7. The Zig compiler's Wasm target also compiles types.cot correctly

### The Failing Pattern
```cot
fn makePointer(elem: int) int {
    const pointee = self.get(elem)              // returns Type union (~128 bytes)
    const is_managed = pointee.tag == TAG_STRUCT // union tag access
    for i in FIRST_USER_TYPE..self.types.count {
        const t = self.get(i)                   // returns Type union via SRET
        if (t.tag == TAG_POINTER) {
            const p = switch (t) { Type.pointer |p| => p, else => PointerType{...} }
            if (p.elem == elem and p.managed == is_managed) { return i }
        }
    }
    return self.add(Type.pointer(PointerType { elem: elem, managed: is_managed }))
}
```

### What the Simpler Repro Does Differently
The test_complex.cot version uses a `Type` union with only 4 variants (basic, pointer, optional, slice) instead of 16. The selfcot's `Type` union has 16 variants including StructType (5 fields), EnumType (4 fields), UnionType (4 fields), FuncType (3 fields), TupleType (2 fields), ErrorSetType (3 fields). The largest variant is likely StructType or EnumType at ~40 bytes. With 16 variants + tag, the union is probably ~48 bytes.

**Hypothesis:** The Zig compiler's native SRET codegen for large unions (>48 bytes, >6 i64 slots) corrupts a field during the struct return copy. This only manifests when the union has enough variants to push the size past a threshold.

---

## Debugging Plan (per TROUBLESHOOTING.md)

### Step 1: Identify the Failing Stage

**Stage: Native codegen (SSA → CLIF → ARM64 → binary)**

The bug is in the compiled selfcot binary. selfcot is compiled by the Zig `cot` compiler's native backend. The crash occurs when selfcot runs — not during compilation. This means the Zig compiler generates incorrect ARM64 code for one of selfcot's functions.

Specifically: the `TypeRegistry.get()` method returns a large `Type` union via SRET. The SRET mechanism (hidden `__sret` pointer parameter, callee writes result to caller's stack) may corrupt a field during the copy.

**Our code:** `compiler/codegen/native/ssa_to_clif.zig` (SSA → CLIF translation)
**Reference:** `references/rust/compiler/rustc_codegen_cranelift/src/base.rs` (cg_clif)

### Step 2: Reproduce with Increasing Union Size

Create a test that isolates the union size threshold:

```cot
// 4 variants — PASSES
union Type4 { a: int, b: PointerType, c: int, d: int }

// 8 variants — test this
union Type8 { a: int, b: PointerType, c: int, d: int, e: int, f: int, g: int, h: int }

// 16 variants — FAILS (matches selfcot)
union Type16 { a: int, b: PointerType, c: int, d: int, e: int, f: int, g: int, h: int,
               i: StructInfo, j: int, k: int, l: int, m: int, n: int, o: int, p: int }
```

For each size, create a `Registry` with `get(idx: int) TypeN` and `makePointer(elem: int) int` following the same pattern. Find the exact union size where it breaks.

### Step 3: Check SRET Codegen for Large Unions

Once we have the threshold, compare the generated ARM64 code between the working and failing cases:

```bash
# Compile the test to native binary
cot build test_union_8.cot -o /tmp/test8
cot build test_union_16.cot -o /tmp/test16

# Disassemble the get() function in each
objdump -d /tmp/test8 | grep -A 50 "Registry_get"
objdump -d /tmp/test16 | grep -A 50 "Registry_get"
```

Compare the SRET handling:
- How is `__sret` pointer passed? (first parameter)
- How is the union copied to the SRET buffer?
- Is the `memcpy`/`move` size correct?
- Are all union fields copied (not just the active variant)?

### Step 4: Compare with Zig Reference (ssa_to_clif.zig)

**Our file:** `compiler/codegen/native/ssa_to_clif.zig`
**Reference:** `references/rust/compiler/rustc_codegen_cranelift/src/base.rs`

Key areas to compare:
1. **SRET return handling** — how does cg_clif handle functions that return large structs/unions?
   - Look for `return_layout`, `PassMode::ByRef`, `write_return_value`
   - Compare with our `translateOp(.ret)` for SRET returns

2. **OpMove codegen** — how does cg_clif emit bulk copies for large values?
   - Look for `memcpy`, `memmove`, `copy_from_slice` patterns
   - Compare with our `translateOp(.move)` / `translateOp(.wasm_lowered_move)`

3. **Union field access** — how does cg_clif handle extracting a variant from a union?
   - Look for `downcast`, `variant_index`, `read_discriminant`
   - Compare with our `translateOp(.union_payload)` / `translateOp(.union_tag)`

4. **Stack slot allocation** — does cg_clif allocate stack slots for SRET buffers?
   - Look for `alloc_local`, `StackSlot`, `layout.size`
   - Compare with our stack slot allocation for large return values

### Step 5: Check the Specific Bug Pattern

The crash is `List(int).get: idx=0, count=0`. This means a `List` struct's `count` field is 0 when it shouldn't be. The `List` is embedded in an `SsaValue.args` or similar structure.

The most likely cause: when `self.func.getValue(src)` returns a 136-byte `SsaValue` via SRET, the returned copy's `args` field (which is `List(int)` = 3 × i64 = 24 bytes within the struct) is corrupted. Specifically, `args.count` becomes 0 even though the original value in the list has `args.count > 0`.

To verify: add a print BEFORE and AFTER `getValue` in `emitMove`:
```cot
const val_v = self.func.getValue(src)
// If val_v.args.count != expected, the SRET corrupted the field
```

But we already tried this and the @panic didn't fire. This means either:
- The crash is NOT in `getValue` — it's in a subsequent call
- The `getValue` return IS correct but a later operation invalidates it

### Step 6: Check List Backing Array Invalidation

When `self.func.newValue(...)` is called (line 2164 in emitMove), it appends to `self.func.values`. If the values list reallocates:
- The old backing array is freed
- All previous `getValue` copies still hold valid data (they're copies, not pointers)
- BUT: the copies' `args.items` pointers point to separately allocated arrays — these are NOT affected by the values list reallocation

So reallocation of `self.func.values` should NOT corrupt existing SsaValue copies. Unless the `List(SsaValue).append` implementation has a bug when `SsaValue` is 136 bytes (requires SRET for the copy).

### Step 7: Minimal Zig Compiler Test

Create a test case that exercises the Zig compiler's native codegen for this pattern WITHOUT going through selfcot:

```cot
struct BigValue {
    id: int,
    args_items: int,
    args_count: int,
    args_capacity: int,
    // ... fill to 136 bytes
}

struct ValueList {
    items: int,  // raw pointer
    count: int,
    capacity: int,
}

fn getValue(list: *ValueList, idx: int) BigValue {
    // Manual load from backing array
    const ptr = @intToPtr(*BigValue, list.items + idx * @sizeOf(BigValue))
    return ptr.*
}

test "SRET 136-byte struct" {
    // Create a list, add items, retrieve and verify all fields
}
```

If this test fails on native but passes on Wasm, the bug is confirmed in the Zig compiler's SRET codegen for large structs.

---

## Fix Strategy

### If the bug is SRET for large unions/structs:

**Location:** `compiler/codegen/native/ssa_to_clif.zig` — `translateRet` or `translateCall` (SRET paths)

**Reference:** `references/rust/compiler/rustc_codegen_cranelift/src/abi/mod.rs` — `return_place`, `adjust_call_arguments` for SRET

**Common SRET bugs:**
1. Wrong size passed to memcpy/move (uses variant size instead of union size)
2. Wrong alignment for the SRET buffer
3. Missing fields in the copy (copies only 8 bytes instead of full struct)
4. SRET pointer invalidated by stack frame changes

### If the bug is List.append for large-element lists:

**Location:** `stdlib/list.cot` — `append`, `ensureCapacity`, `realloc`

**Check:** When `List(SsaValue)` (element size 136) appends, does `ensureCapacity` correctly compute `count * @sizeOf(SsaValue)` for the realloc size? If `@sizeOf(SsaValue)` overflows or is wrong, the backing array is too small.

### If the bug is value-copy semantics:

**Location:** `compiler/codegen/native/ssa_to_clif.zig` — how struct copies are emitted

**Check:** When a 136-byte struct is loaded from memory (getValue returns ptr.*), does the codegen emit a full 136-byte copy or only partial? On ARM64, copies >32 bytes should use a `memcpy` call, not individual LDP/STP pairs. Verify the threshold.

---

## Execution Order

1. **Create union-size sweep test** (30 min) — find the exact variant count where the crash starts
2. **Create minimal Zig compiler SRET test** (30 min) — isolate whether it's SRET or something else
3. **Compare ssa_to_clif.zig SRET handling with cg_clif** (1-2 hours) — line-by-line per TROUBLESHOOTING.md
4. **Apply fix** (30 min-2 hours depending on root cause)
5. **Verify selfcot types.cot builds** (5 min)
6. **Continue self-hosting: build remaining files** (ongoing)

---

## What NOT to Do

- Do NOT add workarounds in selfcot code (splitting functions, avoiding patterns)
- Do NOT skip the failing function (it's needed for self-hosting)
- Do NOT guess at fixes — find the exact codegen difference per TROUBLESHOOTING.md Step 4
- Do NOT increase test_complex.cot's variant count without systematic measurement
