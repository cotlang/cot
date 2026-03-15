# Selfcot Memory & Performance Audit

**Date:** 2026-03-15
**Status:** Fixes implemented, blocked on native codegen crash (SIGILL in Checker struct return)
**Symptom:** selfcot uses 4.3GB+ RAM (documented) / 100GB (observed) to compile scanner.cot (774 lines). The Zig-compiled `cot` compiler handles the same file in 64MB / <1s.

---

## Executive Summary

The selfcot memory/performance crisis has **three independent root causes** that compound multiplicatively:

1. **ARC gap:** `couldBeARC()` returns false for strings, List, Map, and all non-pointer types. `@arcRetain`/`@arcRelease` in stdlib are no-ops. No automatic cleanup at scope exit for these types.
2. **No arena allocator:** The Zig compiler uses per-function `ArenaAllocator` — all SSA temps freed at once. Selfcot uses individual `alloc`/`dealloc` with 32-byte ARC headers per allocation.
3. **Value-copy overhead:** Every `getValue`/`setValue` copies 136-byte SsaValue structs. Go uses `*Value` pointers (8 bytes, zero-copy mutation).

These are not bugs in the selfcot code. They are **design gaps in the Cot language and compiler** that were masked by the Zig compiler's arena allocator but surface catastrophically when the language compiles itself.

---

## Part 1: The ARC Gap

### What `couldBeARC` actually does

```zig
// types.zig:470-475
pub fn couldBeARC(self: *const TypeRegistry, idx: TypeIndex) bool {
    const t = self.get(idx);
    if (t == .pointer) return t.pointer.managed;
    if (t == .optional) return self.couldBeARC(t.optional.elem);
    return false;  // ← strings, structs, List, Map all hit this
}
```

Only **managed pointers** (`*T`) and optionals wrapping them (`?*T`) are ARC-managed. Everything else returns false.

### Consequence: @arcRetain/@arcRelease are no-ops

In `lower.zig:10056-10077`, the `@arcRetain(value)` builtin checks `couldBeARC(arg_type)`. If false, no code is emitted. The retain/release calls in `list.cot` and `map.cot` do **nothing** for:

| Type | `couldBeARC()` | `@arcRetain` effect |
|------|----------------|---------------------|
| `string` (= `[]u8`) | **false** (slice, not pointer) | **no-op** |
| `int`, `bool`, `f64` | **false** (primitive) | **no-op** |
| `SsaValue` (struct) | **false** (struct, not pointer) | **no-op** |
| `List(int)` (struct) | **false** (struct) | **no-op** |
| `Map(string, int)` (struct) | **false** (struct) | **no-op** |
| `*MyStruct` (managed ptr) | **true** | emits `retain()` call |
| `?*MyStruct` | **true** | emits `retain()` call |

### Consequence: No automatic cleanup at scope exit

The compiler registers cleanup at scope exit in two ways:

1. **ARC release cleanup** (`lower.zig:2457`): Only for `couldBeARC(type_idx)` types.
2. **scope_destroy** (`lower.zig:2026`): Only for structs with `deinit()` method OR structs in `pending_auto_deinits` (which requires at least one `couldBeARC` field).

Since List/Map have fields `items: i64, count: i64, capacity: i64` (all primitives), they have **no ARC fields** and are never added to `pending_auto_deinits`. Result:

| Variable declaration | Cleanup at scope exit? |
|---------------------|----------------------|
| `var m: Map(string, int) = .{}` | **NONE** — backing arrays leak |
| `var l: List(int) = .{}` | **NONE** — backing array leaks |
| `var s = copyString("hello")` | **NONE** — string buffer leaks |
| `var p = new MyStruct{}` | **YES** — ARC release (if `*MyStruct` is managed) |

### What reference languages do

**Go:** No GC-managed collections — all memory is GC'd uniformly. No retain/release needed.

**Swift:** Value types with `deinit` — Swift's `Array<T>` is a value type backed by a reference-counted buffer. When an Array goes out of scope, the buffer's refcount is decremented. This happens because Swift's type system knows `Array` contains a heap pointer and generates appropriate copy/destroy witnesses.

**Rust:** RAII via `Drop` trait — `Vec<T>` implements `Drop`, which is called automatically when the variable goes out of scope. No runtime checks needed.

**Zig:** `defer list.deinit(allocator)` — explicit but guaranteed cleanup via `defer`.

### Fix required

The Cot compiler needs to recognize that `List(T)` and `Map(K,V)` contain heap-allocated backing arrays that must be freed at scope exit. Two approaches:

**Fix implemented: Extend `maybeRegisterScopeDestroy` to recognize `free()` methods**

Modified `lower.zig:maybeRegisterScopeDestroy` to check for both `deinit()` and `free()` methods.
List(T) and Map(K,V) already have `free()` — the compiler now auto-calls it at scope exit.

Note: Adding `fn deinit` to generic structs like `List(T)` triggers a Zig compiler bug
(checker.zig:3758 — generic impl methods registered in `global_scope` instead of file scope,
causing cascading "undefined identifier" errors in importing files). The `free()` fallback
avoids this bug entirely.

String functions `indexOf`, `contains`, `count` in `stdlib/string.cot` were renamed to
`strIndexOf`, `strContains`, `strCount` to avoid name conflicts with List/Map methods
(another manifestation of the same checker namespace pollution bug).

---

## Part 2: No Arena Allocator

### How the Zig compiler manages memory (64MB)

```zig
// driver.zig:5713-5715 (wasm path), 1447-1449 (native path)
for (ir_funcs) |ir_func| {
    var func_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer func_arena.deinit();  // ← ALL SSA temps freed here
    const func_alloc = func_arena.allocator();

    var ssa_builder = try SSABuilder.init(func_alloc, ir_func, ...);
    var ssa_func = ssa_builder.build();
    // ... 6 SSA passes, all using func_alloc ...
    const body = try wasm.generateFunc(self.allocator, ssa_func, ...);
    linker.addFunc(body);  // Only bytecode persists
    // func_arena.deinit() frees EVERYTHING
}
```

**Key insight:** Each function's SSA data is allocated in a throwaway arena. After codegen extracts the bytecode, the entire arena is freed at once. Peak memory = size of ONE function's SSA data (~64MB for large functions).

### How selfcot manages memory (4.3GB+)

```cot
// self/codegen/wasm/driver.cot:342-387
fn generateAllFunctions(...) void {
    var i = 0
    while (i < ir.funcs.count) {
        generateAndAddFunc(...)  // Each function allocated individually
        i += 1
    }
}

fn generateAndAddFunc(...) void {
    var builder = SSABuilder.init(ir_func, ir.globals, types)
    var ssa_func = builder.build()
    // ... 6 SSA passes ...
    var sym = generateFunc(ssa_func, ...)
    preprocess(&sym)
    const body = assemble(&sym)
    linker.addFunc(...)
    ssa_func.free()   // Manual free — must enumerate all nested structures
    builder.free()    // Manual free — must enumerate all internal maps
}
```

**Problems:**
1. Every `alloc(0, bytes)` adds 32-byte ARC header overhead (magic, size, metadata, refcount)
2. `ssa_func.free()` must manually iterate all values and free their args lists, all blocks and free their lists — any missed path leaks
3. `GenState` (5 Maps/Lists) is **never freed** — no `free()` method exists
4. `registerFuncType` creates `params: List(int)` and `results: List(int)` — **never freed**
5. `stringToData` creates `List(int)` per string literal — **never freed** (passed to linker which doesn't free it)
6. `rewritegeneric.cot` creates `to_rewrite: List(int)` per block — **never freed**
7. Even when items ARE freed, the individual `malloc`/`free` calls fragment the heap, preventing the OS from reclaiming pages

### Leaked allocations inventory

| Location | What leaks | Per-function? | Estimated size |
|----------|-----------|---------------|----------------|
| `wasm_gen.cot:94-119` | GenState: 5 Maps/Lists | Yes | ~2KB × N funcs |
| `driver.cot:392-398` | `params` and `results` Lists | Yes | ~128B × N funcs |
| `driver.cot:324-335` | `stringToData` return Lists | Per string | ~200B × N strings |
| `rewritegeneric.cot:26` | `to_rewrite` List | Per block | ~64B × N blocks |
| `driver.cot:92-108` | Global Maps (func_indices, etc.) | Once | ~50KB |
| `driver.cot:108-114` | `null_data` List | Once | ~64B |

For scanner.cot (~200 generated functions, ~1000 blocks, ~500 strings):
- GenState: 200 × 2KB = 400KB
- params/results: 200 × 128B = 25KB
- stringToData: 500 × 200B = 100KB
- to_rewrite: 1000 × 64B = 64KB
- **Total explicit leaks: ~600KB**

This does NOT explain 4.3GB. The remaining ~4.3GB comes from:
1. **Fragmentation:** malloc metadata + ARC headers + fragmented free-lists
2. **Temporary copies:** Every `getValue`/`setValue` creates stack copies via SRET — these are stack-allocated and freed automatically, but the sheer volume causes cache pressure and TLB thrashing
3. **String buffers:** Every dynamically created string (`copyString`, `++` concatenation) allocates via `alloc(0, len)` with refcount=1. Since `@arcRelease` is a no-op for strings, these buffers are **never freed**

### Fix required

**Short-term: Add missing `free()` calls**
1. Add `GenState.free()` method and call it in `driver.cot` after codegen
2. Free `params`/`results` in `registerFuncType`
3. Free `to_rewrite` in `rewritegeneric.cot`
4. Free `stringToData` return values after linker copies data

**Medium-term: Add `deinit` to List and Map** (see Part 1)

**Long-term: Implement arena allocator in Cot**
- Port Go's `Cache`/`Reset` pattern or Zig's `ArenaAllocator`
- Per-function arena that frees all SSA data at once
- Eliminates individual alloc/dealloc overhead and fragmentation
- Target: match Zig compiler's 64MB baseline

---

## Part 3: Value-Copy Overhead

### The getValue/setValue problem

SsaValue is ~136 bytes (11 fields including embedded `List(int)` for args). Every access copies the entire struct:

```cot
var val = func.getValue(idx)   // 136-byte copy out of list
val.aux_int = 42               // Modify local copy
func.setValue(idx, val)         // 136-byte copy back into list
```

In the native binary, SRET (struct return) uses stack slots — the 136-byte copy goes to a stack local, not heap. So this is a **performance** issue, not a **memory** issue. But it compounds with the other problems:

### Call frequency analysis

| File | getValue calls | setValue calls | Total |
|------|---------------|---------------|-------|
| `ssa_builder.cot` | 108 | 80 | 188 |
| `wasm_gen.cot` | 351 | 0 | 351 |
| `schedule.cot` | 7 | 0 | 7 |
| SSA passes (other) | ~20 | ~10 | ~30 |
| **Per function total** | ~486 | ~90 | **~576** |

For 200 functions: 576 × 200 = **115,200 calls**, each copying 136 bytes = **15MB of memcpy**.

### How Go avoids this

Go's SSA uses `*Value` pointers throughout:
```go
v := f.newValue(OpAdd, types.Int)  // Returns *Value (8 bytes)
v.Args = append(v.Args, arg1)      // Direct mutation via pointer
// No copy ever happens
```

### Fix required

**Option A: Add `getValuePtr` returning `*SsaValue`**
```cot
fn getValuePtr(idx: int) *SsaValue {
    return @intToPtr(*SsaValue, self.items + idx * @sizeOf(SsaValue))
}
```
This requires Cot to support returning pointers into container backing arrays safely. Currently risky because `List.append` can reallocate the backing array, invalidating the pointer.

**Option B: Store `List(*SsaValue)` instead of `List(SsaValue)`**
Heap-allocate each SsaValue individually and store pointers. Eliminates copies but adds allocation overhead. Mitigated by arena allocator.

**Option C: Skip ARC for non-ARC types in List.get/set** (already done — @arcRetain is a no-op)
The compiler already optimizes this away. No further optimization possible at this level.

**Option D: Reduce SsaValue size**
Currently 136 bytes with aux_float (8B), pos_line/pos_col (16B), in_cache (8B padded). Removing rarely-used fields or using bit-packing could reduce to ~80 bytes, cutting copy overhead by 40%.

---

## Part 4: Priority Fixes

### P0 — Blocking (fixes 4.3GB → <500MB)

1. **Add `fn deinit` to `List(T)` and `Map(K,V)` in stdlib**
   - `fn deinit() void { self.free() }`
   - Enables compiler's `scope_destroy` mechanism to auto-free at scope exit
   - Fixes ALL implicit backing array leaks throughout the compiler
   - **Risk:** Double-free if code also calls `.free()` explicitly — audit all `.free()` call sites

2. **Add `GenState.free()` in `wasm_gen.cot`**
   ```cot
   fn free() void {
       self.value_to_local.free()
       self.branches.free()
       self.bstart.free()
       self.gc_ref_locals.free()
       self.compound_len_locals.free()
       self.builder.free()
   }
   ```
   Call after `generateFunc` returns in `driver.cot`.

3. **Free `params`/`results` in `registerFuncType` (driver.cot:392-398)**
   ```cot
   fn registerFuncType(...) int {
       var params: List(int) = .{}
       buildFuncParams(&params, ...)
       var results: List(int) = .{}
       buildFuncResults(&results, ...)
       const type_idx = linker.addType(params, results)
       params.free()
       results.free()
       return type_idx
   }
   ```

### P1 — Important (fixes 500MB → <200MB)

4. **Free `to_rewrite` in `rewritegeneric.cot`**
5. **Free `stringToData` return values** in `collectAllStringLiterals`
6. **Free global maps** at end of `generateWasmCode` (func_indices, func_table_indices, string_offsets)

### P2 — Performance (fixes 240s → ~60s target)

7. **Add `getValuePtr` / `getBlockPtr` to SsaFunc** — eliminate 136-byte copies
8. **Pre-size Lists** — `ensureCapacity` based on IR function size to avoid repeated realloc
9. **Reduce SsaValue size** — pack rarely-used fields, remove padding

### P3 — Architecture (fixes ~60s → ~10s target)

10. **Implement per-function arena allocator** — Cot equivalent of `ArenaAllocator`
11. **Add compiler optimization passes** — inlining, constant folding, dead code elimination for the native binary

---

## Part 5: Verification Plan

After each fix, measure with:
```bash
# Build selfcot
cot build self/main.cot -o /tmp/selfcot

# Measure single file
/usr/bin/time -l /tmp/selfcot build self/frontend/scanner.cot -o /tmp/out.wasm

# Baseline (Zig compiler)
/usr/bin/time -l cot build self/frontend/scanner.cot --target=wasm32 -o /tmp/out_zig.wasm
```

Track: peak RSS, wall time, and output correctness (compare .wasm files).

**Current baseline:** selfcot crashes (SIGSEGV in Map.contains) before reaching codegen. Fix the native codegen crash first (large struct return bug in Cranelift backend), then measure memory.

---

## Appendix: File Reference

| File | Role | Key lines |
|------|------|-----------|
| `compiler/frontend/types.zig` | `couldBeARC()`, `isTrivial()`, `Shape` | 425-475, 668-747 |
| `compiler/frontend/lower.zig` | ARC insertion, scope_destroy, cleanup stack | 2020-2490, 10051-10077 |
| `compiler/driver.zig` | Per-function arena (Zig path) | 1447-1449, 5713-5715 |
| `compiler/codegen/native/arc_native.zig` | Native alloc/dealloc/retain/release | 230-960 |
| `stdlib/list.cot` | List(T) — no deinit, @arcRetain no-op | 15-556 |
| `stdlib/map.cot` | Map(K,V) — no deinit, @arcRetain no-op | 63-562 |
| `self/codegen/wasm/driver.cot` | Selfcot pipeline, missing frees | 74-590 |
| `self/codegen/wasm/wasm_gen.cot` | GenState — no free method | 43-119 |
| `self/frontend/ssa.cot` | SsaValue (136 bytes), SsaFunc.free() | 214-408 |
| `self/ssa/passes/rewritegeneric.cot` | to_rewrite leak | line 26 |
