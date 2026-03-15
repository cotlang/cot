# Selfcot Memory Leak: Complete Debug Guide

## The Problem

`selfcot build self/frontend/scanner.cot` (774 lines) uses **4.3GB RAM** and takes **225 seconds**.
`cot build self/frontend/scanner.cot` (same file, Zig-compiled compiler) uses **64MB** and takes **<1 second**.

That's **67x more memory** for the same compilation. The selfcot binary produces the same output (a .wasm file) through the same pipeline code.

## What Has Been Proven

### NOT the cause:
1. **ARC is working** — tested 100K string concats, 19MB. Strings are freed correctly.
2. **SSA data structures are small** — simulated 1738 functions × 100 values with full pipeline temporaries (Maps, Lists), result: **98MB even when leaked**.
3. **List.free() works** — tested alloc+free cycles, memory returns correctly.
4. **The SSA pipeline logic is not the bottleneck** — the same Cot code compiled by the Zig compiler uses 64MB.

### IS the cause:
The selfcot **binary is unoptimized** — compiled by the Zig `cot` compiler with no inlining, no CSE, no constant folding. Every `List.get()`, `Map.set()`, `getValue()` is a full function call with stack frame setup. The binary does **74GB of cumulative allocation** (measured via peak memory footprint) for a 9K-line file, vs a few hundred MB for the Zig-compiled version.

## BUT — 5.5MB per line is still insane

Even accounting for unoptimized code, 5.5MB per line of source is too much. There must be specific hot paths that allocate disproportionately. Here's exactly where to look:

## Where to Debug

### Step 1: Instrument the Zig compiler's alloc/dealloc

The selfcot binary calls `malloc`/`free` via the native runtime's `alloc`/`dealloc` functions in `compiler/codegen/native/arc_native.zig`.

**Add counters to the native runtime:**
- In `generateAlloc()` (line 230 of `arc_native.zig`): the generated CLIF IR calls `malloc`. Add a global counter that increments on each call and sums the requested sizes.
- In `generateDealloc()` (line 310): add a counter for frees.
- In `generateRealloc()` (line 786): add a counter for reallocs + bytes.
- Expose these counters via new runtime functions `get_alloc_count()`, `get_alloc_bytes()`, etc.

Then call these from the self-hosted driver between phases to see where allocation explodes.

### Step 2: Add per-function memory tracking to the self-hosted driver

File: `self/codegen/wasm/driver.cot`, function `generateAndAddFunc` (line ~355)

The pipeline per function is:
```
SSABuilder.init → builder.build() → 6 SSA passes → generateFunc → preprocess → assemble → linker.addFunc
```

Add stderr prints between each step showing a function counter. Use `write(2, ...)` and `eprint_int()`. The key is to print the **function number** so you can correlate with `/usr/bin/time -l` RSS growth.

**IMPORTANT**: `self/` uses `@safe` mode via `self/cot.json`. Do NOT add `@safe` to files. `self` parameter is auto-injected in struct methods.

### Step 3: The likely hot paths

Based on the architecture, these are the most likely memory hogs **per function**:

#### 3a. `List(SsaValue)` growth in SSA builder
- File: `self/frontend/ssa.cot`, `SsaFunc.newValue()` (line ~444)
- `SsaValue` is a struct with `args: List(int)` — each value allocates a List backing array
- With 100+ values per function × 1738 functions, that's 170K+ `List(int)` allocations
- Each `List(int)` starts at capacity 8 (64 bytes) — 170K × 64 = ~11MB. Should be small.
- **BUT**: `List(SsaValue).append()` copies the entire SsaValue struct on growth. SsaValue is ~88 bytes. When the list grows from 100→200, it copies 100 × 88 = 8.8KB, then the old array (8.8KB) should be freed via `realloc`. If `realloc` doesn't free the old array (bump allocator?), this compounds.

#### 3b. Map allocations in SSA passes
- File: `self/ssa/passes/schedule.cot`, `scheduleBlock()` — creates 9 Maps/Lists PER BLOCK
- File: `self/ssa/passes/layout.cot`, `layout()` — creates 6 Maps/Lists per function
- These are created and NEVER freed (no `.free()` calls in the original code)
- With 5 blocks per function × 1738 functions × 9 allocations = 78K leaked Maps
- Each Map starts at capacity 16 entries × 24 bytes = 384 bytes. 78K × 384 = ~30MB.
- **This was partially fixed** — `.free()` calls were added but may not be in the current binary.

#### 3c. GenState in wasm_gen.cot
- File: `self/codegen/wasm/wasm_gen.cot`, `generateFunc()` (line ~2284)
- Creates `GenState` with: `value_to_local: Map(int,int)`, `branches: List(Branch)`, `bstart: Map(int,int)`, `compound_len_locals: Map(int,int)`, `builder: ProgBuilder` (contains `List(Prog)`)
- `ProgBuilder.progs` grows to ~300 entries per function × ~56 bytes per Prog = 16KB
- value_to_local: ~100 entries × 24 bytes = 2.4KB
- GenState itself is NOT freed after `generate()` returns — **all backing arrays leak**
- With 1738 functions: 1738 × (16KB + 2.4KB + ...) = ~32MB. Still not enough for 4.3GB.

#### 3d. The Prog chain and preprocess
- File: `self/codegen/wasm/preprocess.cot` — `pass6ResolveBranches` creates `blockDepths: List(int)` with one entry per Prog (~300 entries). NEVER freed in original code.
- `pass1CountResumePoints` creates `tableIdxs: List(int)`. Returned but original may not be freed.
- 1738 functions × 300 entries × 8 bytes = ~4MB. Small.

#### 3e. The REAL suspect: `realloc` doesn't free old memory

The native runtime's `realloc` (in `arc_native.zig` line 786):
```
alloc new → memcpy old data → dealloc old → return new
```

But `List.ensureCapacity()` in `stdlib/list.cot` (line 41) calls:
```
self.items = realloc(self.items, bytes)
```

The runtime `realloc` calls `malloc(new_size)`, `memcpy`, `free(old - HEADER_SIZE)`. If `free` doesn't return pages to the OS (macOS malloc behavior with fragmented heap), RSS keeps growing.

**Test this theory**: Build a Cot program that does 1 million `realloc` calls in a loop and measure RSS. If RSS grows linearly, `realloc` is the problem.

#### 3f. Stack frame overhead from no inlining

The selfcot binary has NO inlining. Every `List.get()` call:
1. Push 6 callee-saved registers (48 bytes)
2. Allocate local frame (~32 bytes)
3. Push args
4. Call
5. Pop everything

With ~10M function calls during compilation, the peak live stack could be significant. But stack is limited to 8MB by default, so this caps at 8MB — not the 4.3GB.

#### 3g. ARC retain/release on SsaValue.aux (union with string)

`SsaValue` has `aux: SsaAux` which is a union containing `str: string`. When `List(SsaValue).append()` copies an SsaValue, it calls `@arcRetain` on the `aux` field. If `aux` is `SsaAux.none` (tag=0), `@arcRetain` should be a no-op. But if the ARC code doesn't check the union tag correctly, it might try to retain a garbage pointer.

**This could cause unbounded memory growth** — every `List(SsaValue)` append retains a garbage pointer, incrementing a refcount on random memory, preventing any dealloc from actually freeing.

**Test**: Check if `@arcRetain` on a union with tag `none` does the right thing. Add a test that creates 100K SsaValues, appends them to a List, and checks memory.

## How to Measure

### Quick approach (no code changes):
```bash
# Compare Zig compiler vs selfcot for the same file
/usr/bin/time -l cot build self/frontend/scanner.cot -o /tmp/out1
/usr/bin/time -l /tmp/selfcot build self/frontend/scanner.cot -o /tmp/out2
```

### Per-phase measurement:
Add `write(2, ...)` + `eprint_int()` prints to `self/codegen/wasm/driver.cot` in `generateAllFunctions()`:
```cot
// Before the function loop:
write(2, @ptrOf("[phase] before codegen\n"), 22)

// Inside the loop, every 100 functions:
if (i % 100 == 0) {
    write(2, @ptrOf("[fn] "), 5)
    eprint_int(i)
    write(2, @ptrOf("/"), 1)
    eprint_int(ir.funcs.count)
    write(2, @ptrOf("\n"), 1)
}
```

Then run with `/usr/bin/time -l` and observe RSS growth. Also run `top -pid <PID>` in another terminal to see RSS in real-time.

### Allocation counting approach:
Create `stdlib/alloc_debug.cot` that wraps `alloc`/`dealloc`/`realloc` with global counters:
```cot
import "std/sys"

var g_alloc_count: int = 0
var g_alloc_bytes: int = 0

fn debug_alloc(metadata: int, size: int) int {
    g_alloc_count += 1
    g_alloc_bytes += size
    return alloc(metadata, size)
}
```

Then change `stdlib/list.cot` and `stdlib/map.cot` to use `debug_alloc` instead of `alloc`. This will show exactly how many allocations happen and how many bytes.

**But**: This requires modifying stdlib which is a submodule. Alternative: add the counters to `self/codegen/wasm/driver.cot` and manually track around the hot paths.

## File Map

| File | What it does | Relevance |
|------|-------------|-----------|
| `self/codegen/wasm/driver.cot` | Orchestrates per-function pipeline | **Add debug prints here** |
| `self/frontend/ssa.cot` | SsaFunc, SsaValue, SsaBlock | Value/block allocation |
| `self/frontend/ssa_builder.cot` | Builds SSA from IR | Creates all values/blocks |
| `self/ssa/passes/schedule.cot` | Value scheduling | Creates 9 temp allocs per block |
| `self/ssa/passes/layout.cot` | Block ordering | Creates 6 temp allocs |
| `self/codegen/wasm/wasm_gen.cot` | SSA → Prog chain | GenState maps/lists |
| `self/codegen/wasm/preprocess.cot` | Dispatch loop insertion | Temp lists |
| `self/codegen/wasm/assemble.cot` | Prog → Wasm bytes | Output list |
| `stdlib/list.cot` | List(T) — calls alloc/realloc/dealloc | All allocations go through here |
| `stdlib/map.cot` | Map(K,V) — calls alloc/realloc | Hash table allocations |
| `compiler/codegen/native/arc_native.zig` | Native alloc/dealloc/realloc runtime | Where malloc/free are called |

## Most Likely Root Cause (Best Guess)

**Theory: ARC retain on union fields in `List(SsaValue)` is retaining garbage pointers.**

`SsaValue` contains `aux: SsaAux` which is a `union { none, str: string, type_ref: int }`. When `List(SsaValue).append()` runs, `stdlib/list.cot:49` calls `@arcRetain(value)`. For a struct containing a union with a string variant, the ARC system may try to retain the string even when the union tag is `none` — because at the codegen level, the string pointer field exists at the same memory offset regardless of the tag.

If this is happening:
- Every `List(SsaValue).append()` call increments a refcount on a garbage pointer
- The garbage pointer prevents pages from being freed
- With 170K+ SsaValue appends, this accumulates to gigabytes

**How to test**: Create a simple Cot program that does `List(SsaValue).append()` 10K times and measure RSS. Compare with a struct that has no union field. If the union version uses much more memory, ARC on unions is the bug.

## What Was Already Done (In Current Commit d2e94dd)

1. Per-function ArenaAllocator in Zig compiler — **works**, 187MB wasm
2. SSA optimization passes (deadcode, copyelim, phielim, CSE) — **written and tested**, wired into Zig wasm path
3. Leak fixes in self-hosted passes — `.free()` calls added to schedule, layout, rewritegeneric, preprocess
4. `ptr.* = struct` compiler bug fixed — checker + lowerer in both Zig and self-hosted
5. Go Cache pattern attempted but **reverted** due to SIGBUS crash (SsaValue struct was restructured, broke runtime)

## What the Next Agent Should Do

1. **Don't touch self/frontend/ssa.cot structure** — the user reverted it to the working version with `List(int)` args and `SsaAux` union. Leave it alone.
2. **Add simple print-based debugging** to `self/codegen/wasm/driver.cot` — print function counter every N functions to correlate with RSS growth observed via `top` or `time -l`.
3. **Test ARC on unions** — write a standalone `.cot` test that creates structs with union fields, appends to a List, and measures if memory grows unboundedly.
4. **If ARC is the bug** — fix it in `compiler/codegen/native/arc_native.zig` (the `retain` function needs to check if the value is actually an ARC-managed pointer before incrementing refcount).
5. **If ARC is not the bug** — instrument `stdlib/list.cot` `ensureCapacity` to count realloc calls and bytes, find which List type is growing most.
