# Bug: `convertGlobalStore` Uses `.store` (8 bytes) for Struct-Type Globals — Must Use `.move` (N bytes)

**Status**: FIXED — `convertGlobalStore` now uses size-aware `.move` for structs > 8 bytes, including VOID-typed `off_ptr` values from pointer field dereferences
**Priority**: High — all 16 basic colors render as black/garbage

---

## 1. The Bug in One Sentence

`convertGlobalStore` (ssa_builder.zig:745-755) always emits a single `.store` op regardless of type size. For struct types > 8 bytes, this writes only 8 bytes (the source address) instead of copying the full struct value. The fix is to add the same size-aware `.move` branching that `convertStoreLocal` (line 651) and `convertPtrStoreValue` (line 1479) already use.

---

## 2. Buggy Code — `compiler/frontend/ssa_builder.zig:745-755`

```zig
fn convertGlobalStore(self: *SSABuilder, name: []const u8, value_idx: ir.NodeIndex, cur: *Block) !*Value {
    const value = try self.convertNode(value_idx) orelse return error.MissingValue;
    const addr_val = try self.func.newValue(.global_addr, TypeRegistry.VOID, cur, self.cur_pos);
    addr_val.aux = .{ .string = name };
    try cur.addValue(self.allocator, addr_val);

    const store_val = try self.func.newValue(.store, TypeRegistry.VOID, cur, self.cur_pos);  // ← ALWAYS .store
    store_val.addArg2(addr_val, value);
    try cur.addValue(self.allocator, store_val);
    return value;
}
```

**Problem**: No type size check. `.store` emits a single 8-byte write (ssa_to_clif.zig:728-734). For `List(i64)` (24 bytes: items pointer + count + capacity), only the first 8 bytes are written — and the "value" is the source address, not the dereferenced content.

---

## 3. Reference Implementation #1 — `convertStoreLocal` (same file, lines 651-698)

This is the **primary reference** to copy from. It handles the identical problem for local variables:

```zig
// ssa_builder.zig:651-698 (abridged — key logic only)
const addr_val = try self.emitLocalAddr(local_idx, TypeRegistry.VOID, cur);
const type_size = self.type_registry.sizeOf(value.type_idx);
const is_compound_opt = value_type == .optional and blk: {
    const elem_info = self.type_registry.get(value_type.optional.elem);
    break :blk elem_info != .pointer;
};
var is_large_struct = ((value_type == .struct_type or value_type == .tuple or value_type == .union_type) and type_size > 8) or is_compound_opt;

// Also check VOID-typed addresses from convertFieldValue
if (!is_large_struct and value.op == .off_ptr and value.type_idx == TypeRegistry.VOID) {
    const local_type_idx = self.ir_func.locals[local_idx].type_idx;
    const local_type = self.type_registry.get(local_type_idx);
    const local_size = self.ir_func.locals[local_idx].size;
    const local_is_compound_opt = local_type == .optional and blk: {
        const elem_info = self.type_registry.get(local_type.optional.elem);
        break :blk elem_info != .pointer;
    };
    if (((local_type == .struct_type or local_type == .tuple or local_type == .union_type) and local_size > 8) or local_is_compound_opt) {
        is_large_struct = true;
    }
}

if (is_large_struct) {
    const src_addr = value;
    const move_val = try self.func.newValue(.move, TypeRegistry.VOID, cur, self.cur_pos);
    move_val.addArg2(addr_val, src_addr);     // args[0]=dest, args[1]=src
    const move_size = if (type_size > 0) type_size else self.ir_func.locals[local_idx].size;
    move_val.aux_int = @intCast(move_size);   // byte count
    try cur.addValue(self.allocator, move_val);
    self.assign(local_idx, value);
    return value;
}

// Scalar path — single .store (only for types ≤ 8 bytes)
const store_val = try self.func.newValue(.store, TypeRegistry.VOID, cur, self.cur_pos);
store_val.addArg2(addr_val, value);
try cur.addValue(self.allocator, store_val);
```

**Key pattern**: Check type size → if > 8 bytes or compound optional → emit `.move` with `aux_int = type_size` → else emit `.store`.

---

## 4. Reference Implementation #2 — `convertPtrStoreValue` (same file, lines 1479-1563)

Shows the same pattern for pointer dereference stores, **plus** string/slice decomposition:

```zig
// ssa_builder.zig:1486-1550 (abridged)

// String/slice: field-by-field decomposition (ptr@0, len@8, cap@16)
const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
if (is_string_or_slice) {
    // ... decompose into ptr/len/cap stores at offsets 0/8/16 ...
    return len_store;
}

// Large struct: bulk .move
const is_large_struct = (value_type == .struct_type or value_type == .tuple or value_type == .union_type) and type_size > 8;
if (is_large_struct) {
    const src_addr = value;
    const move_val = try self.func.newValue(.move, TypeRegistry.VOID, cur, self.cur_pos);
    move_val.addArg2(ptr_val, src_addr);
    move_val.aux_int = @intCast(type_size);
    try cur.addValue(self.allocator, move_val);
    return move_val;
}
```

---

## 5. The Fix

Replace `convertGlobalStore` (lines 745-755) with this size-aware version. The fix copies the pattern from `convertStoreLocal` and `convertPtrStoreValue`:

```zig
fn convertGlobalStore(self: *SSABuilder, name: []const u8, value_idx: ir.NodeIndex, cur: *Block) !*Value {
    const value = try self.convertNode(value_idx) orelse return error.MissingValue;
    const addr_val = try self.func.newValue(.global_addr, TypeRegistry.VOID, cur, self.cur_pos);
    addr_val.aux = .{ .string = name };
    try cur.addValue(self.allocator, addr_val);

    const value_type = self.type_registry.get(value.type_idx);
    const type_size = self.type_registry.sizeOf(value.type_idx);

    // String/slice: field-by-field decomposition (same as convertPtrStoreValue lines 1486-1536)
    const is_string_or_slice = value.type_idx == TypeRegistry.STRING or value_type == .slice;
    if (is_string_or_slice) {
        var ptr_component: *Value = undefined;
        var len_component: *Value = undefined;

        if ((value.op == .string_make or value.op == .slice_make) and value.args.len >= 2) {
            ptr_component = value.args[0];
            len_component = value.args[1];
        } else {
            ptr_component = try self.func.newValue(.slice_ptr, TypeRegistry.I64, cur, self.cur_pos);
            ptr_component.addArg(value);
            try cur.addValue(self.allocator, ptr_component);

            len_component = try self.func.newValue(.slice_len, TypeRegistry.I64, cur, self.cur_pos);
            len_component.addArg(value);
            try cur.addValue(self.allocator, len_component);
        }

        const ptr_store = try self.func.newValue(.store, TypeRegistry.VOID, cur, self.cur_pos);
        ptr_store.addArg2(addr_val, ptr_component);
        try cur.addValue(self.allocator, ptr_store);

        const len_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
        len_addr.aux_int = 8;
        len_addr.addArg(addr_val);
        try cur.addValue(self.allocator, len_addr);

        const len_store = try self.func.newValue(.store, TypeRegistry.VOID, cur, self.cur_pos);
        len_store.addArg2(len_addr, len_component);
        try cur.addValue(self.allocator, len_store);

        if (value.op == .slice_make and value.args.len >= 3) {
            const cap_addr = try self.func.newValue(.off_ptr, TypeRegistry.VOID, cur, self.cur_pos);
            cap_addr.aux_int = 16;
            cap_addr.addArg(addr_val);
            try cur.addValue(self.allocator, cap_addr);

            const cap_store = try self.func.newValue(.store, TypeRegistry.VOID, cur, self.cur_pos);
            cap_store.addArg2(cap_addr, value.args[2]);
            try cur.addValue(self.allocator, cap_store);
        }

        return value;
    }

    // Compound optional (non-pointer element): bulk copy (same as convertStoreLocal line 653)
    const is_compound_opt = value_type == .optional and blk: {
        const elem_info = self.type_registry.get(value_type.optional.elem);
        break :blk elem_info != .pointer;
    };

    // Large struct/tuple/union: bulk .move copy (same as convertStoreLocal line 676)
    const is_large_struct = ((value_type == .struct_type or value_type == .tuple or value_type == .union_type) and type_size > 8) or is_compound_opt;

    if (is_large_struct) {
        const src_addr = value;
        const move_val = try self.func.newValue(.move, TypeRegistry.VOID, cur, self.cur_pos);
        move_val.addArg2(addr_val, src_addr);
        move_val.aux_int = @intCast(type_size);
        try cur.addValue(self.allocator, move_val);
        return value;
    }

    // Scalar path — single 8-byte .store (types ≤ 8 bytes)
    const store_val = try self.func.newValue(.store, TypeRegistry.VOID, cur, self.cur_pos);
    store_val.addArg2(addr_val, value);
    try cur.addValue(self.allocator, store_val);
    return value;
}
```

**Note**: The global store version is simpler than `convertStoreLocal` because there's no `self.assign()` tracking for globals and no VOID-typed `off_ptr` fallback path (globals have known types).

---

## 6. CLIF/ARM64 Codegen Trace

### Current (broken) — `.store` op

SSA: `store(global_addr("THEME_PALETTE"), value)` → ssa_to_clif.zig:728-734:
```
ins.store(val, addr, 0)  →  single CLIF store
```
ARM64:
```asm
STR x0, [x1]    ; writes 8 bytes — only items pointer, loses count+capacity
```

### Fixed — `.move` op

SSA: `move(global_addr("THEME_PALETTE"), value_addr, aux_int=24)` → ssa_to_clif.zig:1013-1045:
```
Loop: load 8 bytes from src+offset, store 8 bytes to dst+offset, offset += 8
Repeat for size=24: 3 iterations
```
ARM64:
```asm
LDR x2, [x1, #0]      ; load items ptr
STR x2, [x0, #0]      ; store items ptr
LDR x2, [x1, #8]      ; load count
STR x2, [x0, #8]      ; store count
LDR x2, [x1, #16]     ; load capacity
STR x2, [x0, #16]     ; store capacity
```

---

## 7. Minimal Reproduction

```cot
// test_global_struct.cot
import "std/list"

struct Pair {
    a: i64,
    b: i64,
    c: i64,
}

var GLOBAL_PAIR: Pair = undefined

fn setPair(p: *Pair) void {
    GLOBAL_PAIR = p.*    // Bug: stores &p.a (8 bytes) instead of copying 24 bytes
}

fn getA() i64 {
    return GLOBAL_PAIR.a  // Returns garbage — reads from address stored in first 8 bytes
}

fn getB() i64 {
    return GLOBAL_PAIR.b  // Returns 0 — global memory was never written past byte 8
}

test "global struct assignment copies all fields" {
    var p = Pair { a: 111, b: 222, c: 333 }
    setPair(&p)
    assert(getA() == 111)  // May pass (first 8 bytes happen to overlap)
    assert(getB() == 222)  // FAILS — b was never written
    assert(GLOBAL_PAIR.c == 333)  // FAILS — c was never written
}
```

---

## 8. Test Evidence from Cotty

C test harness at `/tmp/test_cotty_palette.c` calls Cotty FFI functions that exercise `THEME_PALETTE = cfg.palette` (a `List(i64)` — 24 bytes):

```
=== ANSI Basic Colors via ESC[3Xm ===
  Color 0 (ESC[30m): fg=(4347831960, 48, 64)   ← List struct metadata (items_ptr, count, capacity)
  Color 1 (ESC[31m): fg=(1, 0, 0)              ← reading past struct into App.focused_surface
  Color 2 (ESC[32m): fg=(0, 0, 0)              ← zeroed memory
  ...
  24-bit (ESC[38;2;255;128;0m): fg=(255, 128, 0) ← works (no palette lookup)
  SGR reset (ESC[0m): fg=(217, 217, 217)         ← works (scalar globals)
```

The values `(4347831960, 48, 64)` are the 3 fields of the `List(i64)` struct itself — the global contains the *address* of the palette list, so reading `GLOBAL.items` returns the list's `items` pointer (a heap address), not the first color value.

---

## 9. Global Data Section — Size Is Correct

`compiler/driver.zig:1332-1348` already allocates the right size for global data:
```zig
const global_size = @max(g.size, 8);
const zero_data = try self.allocator.alloc(u8, global_size);
```

The data section entry for a `List(i64)` global is 24 bytes. The storage is correctly sized — the bug is purely in the SSA store op selection (`.store` vs `.move`).

---

## 10. Secondary Issue: `convertGlobalRef` (lines 701-743)

Global struct **reads** may also have an issue. For non-slice types, it falls through to a single `.load` (line 739):

```zig
// ssa_builder.zig:739-742
const load_val = try self.func.newValue(.load, type_idx, cur, self.cur_pos);
load_val.addArg(addr_val);
try cur.addValue(self.allocator, load_val);
return load_val;
```

For a 24-byte struct, a single `.load` only reads 8 bytes. However, struct init expressions like `palette: THEME_PALETTE` may work because the compiler resolves them via field-level access paths (ADRP+ADD to global address, then field-by-field LDR/STR), bypassing the single-load path.

Slices are already handled correctly (lines 707-736) with field-by-field decomposition. Struct types need similar treatment — either return the global address as a pointer and let downstream field accesses work, or decompose into field loads.

**This is lower priority** — the store bug is the immediate blocker. But if after fixing the store, reads of global structs return partial data, this is why.

---

## 11. Verification Steps

After applying the fix:

```bash
# 1. Build the Cot compiler
cd ~/cotlang/cot
zig build

# 2. Run compiler tests to ensure no regressions
zig build test

# 3. Build Cotty dylib with the fixed compiler
cd ~/cot-land/cotty
cot build src/ffi.cot --lib -o /tmp/test_cotty.dylib

# 4. Verify disassembly shows multi-word copy in initThemeDefaults
objdump -d /tmp/test_cotty.dylib | grep -A30 '<_initThemeDefaults>:'
# EXPECTED: 3× LDR/STR pairs for 24-byte palette copy
# BROKEN:   single STR (address, not value)

# 5. Run the C palette test
cc -o /tmp/test_cotty_palette /tmp/test_cotty_palette.c -L/tmp -ltest_cotty -Wl,-rpath,/tmp
/tmp/test_cotty_palette
# EXPECTED: Color 1 = (244, 0, 95), Color 2 = (152, 224, 36), etc.
# BROKEN:   Color 0 = (4347831960, 48, 64), Color 1 = (1, 0, 0), etc.

# 6. Run Cot unit tests
cot test src/config.cot
cot test src/terminal.cot
```

---

## 12. Summary of Files to Modify

| File | What to Change |
|------|---------------|
| `compiler/frontend/ssa_builder.zig:745-755` | Replace `convertGlobalStore` with size-aware version (see Section 5) |

One function, ~70 lines of new code replacing ~10 lines, copying an established pattern from two other functions in the same file.

---

## 13. Supersedes

This report supersedes `DYLIB_GLOBAL_VARS_BUG.md`, which incorrectly blamed "mutable global variable writes don't persist." Scalar global writes DO persist. The bug is specific to struct-type global assignment codegen.
