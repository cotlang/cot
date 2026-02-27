# Bug: Struct Return Codegen — Pointer vs Value

## Status: DIAGNOSED, NOT YET FIXED

Blocked by: ARC overhaul (claude/ARC_OVERHAUL.md) — fix after that lands.

## Summary

Functions returning struct types via `return ptr.*` return the **pointer** instead of loading and returning the struct **value**. This causes ARC to release the wrong memory, leading to use-after-free crashes.

## Symptom

`selfcot check` crashes with SIGBUS (signal 10) during multi-file checking when the symbols list exceeds 64 entries. The crash is in `realloc` → `memmove` because the ARC heap header at `items - 24` has been zeroed.

## Root Cause Chain

1. **`Checker.getSymbol(idx)`** calls `self.symbols.get(idx)` which does `return ptr.*`
2. **Codegen bug:** The generated ARM64 code for `getSymbol` computes `items + idx * 56` and returns it as x0 — a **pointer into the list buffer**, NOT the dereferenced Symbol value
3. **ARC releases the pointer:** When the caller's `sym` local goes out of scope, ARC tries to release `sym.name.ptr` (first field of Symbol). But `sym` IS the pointer, so ARC releases the list buffer pointer directly
4. **Free zeroes the header:** `release(items)` → refcount hits 0 → `dealloc(items)` → `free(items - 24)` → macOS memsets the freed block to zero
5. **Later realloc crashes:** When the symbols list needs to grow, `realloc` reads `alloc_size` from the zeroed header, computes `copy_size = 0 - 24 = huge`, and `memmove` crashes

## Proof (lldb watchpoint)

```
Watchpoint 1 hit:
old value: 3608
new value: 0

* thread #1, stop reason = watchpoint 1
  frame #0: _platform_memset + 112       ← macOS free zeroing memory
  frame #1: mfm_free + 304
  frame #2: dealloc + 52                 ← Cot ARC dealloc
  frame #3: release + 204                ← Cot ARC release
  frame #4: Checker_checkFnDeclBody + 1400  ← ARC cleanup of "struct" local
  frame #5: Checker_checkDecl + 812
  frame #6: Checker_checkFile + 784
```

## Disassembly Evidence

`Checker_getSymbol` returns a pointer, not a value:
```asm
ldr    x17, [x17]      ; x17 = self.symbols.items
mov    x1, #0x38       ; 56 = sizeof(Symbol)
mul    x0, x0, x1      ; idx * 56
add    x0, x17, x0     ; x0 = items + idx*56 = POINTER (not value!)
ret                     ; returns pointer in x0
```

No loads from the computed address — should load 7 chunks (Symbol = 56 bytes).

## Fix Location

**File: `compiler/codegen/native/ssa_to_clif.zig`** — three places need struct return decomposition:

### 1. `buildSignature()` (line ~343)
Currently adds a single I64 return for any non-void type. Needs to add N × I64 returns for struct types > 8 bytes (same pattern as parameter decomposition at lines 327-333).

### 2. `getOrCreateFuncRef()` (line ~1501)
Same issue — call-site signature only adds 1 return for structs. Needs N × I64.

### 3. `emitTerminator()` `.ret` case (line ~1227)
Currently returns single value. For struct returns, needs to:
- Detect struct return type
- Load N chunks from the pointer at offsets 0, 8, 16, ...
- Return all N values

### 4. `emitCall()` (line ~1420)
Currently stores only `results[0]`. For struct returns, needs to:
- Receive N return values
- Allocate stack slot for the struct
- Store all N values to the stack slot
- Map SSA value to stack slot address

## Reference Pattern

The **parameter** side already handles struct decomposition correctly:
```zig
// Lines 327-333 in buildSignature
if (is_large_struct) {
    const num_slots = @max(1, (type_size + 7) / 8);
    for (0..num_slots) |_| {
        try self.clif_func.signature.addParam(self.allocator, clif.AbiParam.init(clif.Type.I64));
    }
}
```

Apply the same pattern to **returns**.

## Reproduction

```bash
# Build selfcot
cot build self/main.cot -o /tmp/selfcot

# This crashes (6 functions + sys.cot's 53 = 65 symbols, triggers realloc)
cat > /tmp/crash.cot << 'EOF'
import "std/sys"
fn f1(n: i64) i64 { return n }
fn f2(n: i64) i64 { return n }
fn f3(n: i64) i64 { return n }
fn f4(n: i64) i64 { return n }
fn f5(n: i64) i64 { return n }
fn f6(n: i64) i64 { return n }
EOF
/tmp/selfcot check /tmp/crash.cot  # exit 138 (SIGBUS)

# This works (5 functions = 63 symbols, fits in capacity 64, no realloc)
# Remove fn f6 → works fine
```

## Why 64 is the threshold

- sys.cot has 53 extern fn declarations → 53 symbols
- Each fn in the test adds 2 symbols (fn name + parameter) = 12 for 6 functions
- Total: 53 + 12 = 65, exceeds List capacity 64 (grew: 8→16→32→64)
- 65th append triggers `ensureCapacity(65)` → `realloc` → reads zeroed header → crash
- With 5 functions: 53 + 10 = 63, fits in capacity 64, no realloc needed → no crash
