# Selfcot Performance Analysis

## The Problem

Compiling `self/main.cot` (42K lines, 38 files, 1738 generated functions):

| Compiler | Time | Peak RSS | Notes |
|----------|------|----------|-------|
| `cot build` (Zig-compiled) | **4.5s** | 903MB | Native backend, Cranelift |
| `selfcot build` (self-compiled) | **240s** | 4.8GB+ | Same pipeline, unoptimized binary |

**53x slower, 5x more memory.** Both compile the same source through the same pipeline (parse → check → IR → SSA → passes → Wasm codegen → link). The difference is entirely in the quality of the compiled binary.

## Root Causes

### 1. No Optimization Passes (biggest factor, ~10-20x)

The Cot native backend emits correct but completely unoptimized ARM64 code:
- No constant folding
- No dead code elimination
- No common subexpression elimination
- No loop-invariant code motion
- No strength reduction

Every expression is computed from scratch every time. A loop like `while (i < list.count)` reloads `list.count` from memory on every iteration instead of hoisting it to a register.

**Reference:** Go's compiler was similarly slow before adding SSA optimizations. Their SSA passes (deadcode, copyelim, phielim, nilcheck) each gave measurable speedups.

### 2. No Inlining (~5-10x)

Every function call is a real call — stack frame setup, argument passing, return. Small helpers like `SsaValue.getArg(idx)`, `SsaFunc.getValue(idx)`, `List(T).get(idx)` are called millions of times during compilation. Each call costs ~10-20 cycles of overhead that inlining would eliminate.

The SSA pipeline does roughly:
- 1738 functions × ~100 values/function × ~5 getValue/setValue calls per value = **~870K function calls** just for value access
- Plus args access, block access, pass iterations: easily **10M+ function calls** total

With inlining, most of these become direct memory loads — 1-2 cycles instead of 10-20.

### 3. Value-Type Copying (~3-5x)

`SsaValue` is ~144 bytes. Every `func.getValue(idx)` copies 144 bytes from the list backing array to a local variable. Every `func.setValue(idx, val)` copies 144 bytes back. Go avoids this entirely by using pointers (`*Value`) — getValue returns a pointer, not a copy.

In the SSA pipeline:
- `getValue` + modify + `setValue` = 288 bytes copied per modification
- With ~100 values per function × ~5 modifications × 1738 functions = **~250MB of unnecessary memcpy**

The fix would be to use pointer-based access (`getValuePtr` returning `*SsaValue`), but Cot's `List(T).get()` returns by value, not by pointer. This is a language-level design decision that affects performance.

### 4. ARC Overhead on String Fields (~2-3x)

`SsaValue` has `aux_str: string`. In Cot, `string` is ARC-managed. Every `List(SsaValue).get()` calls `@arcRetain` on the string. Every `List(SsaValue).set()` calls `@arcRetain` on the new value and `@arcRelease` on the old value. These are atomic reference count operations.

With ~870K getValue/setValue calls, that's **~1.7M atomic operations** just for SsaValue string fields. On ARM64, atomic operations involve memory barriers that stall the pipeline.

Most SSA values don't even use `aux_str` (it's empty string `""`). The retain/release is wasted work.

### 5. No Register Allocation Quality

The Cot backend uses a simple register allocator. The Zig-compiled binary uses Cranelift's production-quality regalloc2 (linear scan + backtracking). Poor register allocation means more spills to stack, more loads/stores, more cache pressure.

## Memory and Performance Are Related

The memory issue (4.8GB vs 903MB) directly causes slowdowns:

1. **Cache pressure:** 4.8GB working set doesn't fit in L3 cache (~32MB on M-series). Every list access is likely a cache miss — 50-100 cycles instead of 3-4 cycles.

2. **TLB pressure:** 4.8GB requires thousands of page table entries. TLB misses add 10-30 cycles per memory access.

3. **malloc fragmentation:** Millions of small allocations fragment the heap. `malloc`/`free` themselves become slow (lock contention, freelist traversal).

4. **Realloc copies:** When `List(SsaValue)` grows past capacity, `realloc` copies the entire backing array. With 144-byte elements, growing from 1000→2000 copies 144KB. This happens repeatedly as lists grow.

**Fixing memory will improve performance.** If we get RSS from 4.8GB to ~200MB, the working set fits in cache, and we'd expect a 2-5x speedup from better locality alone.

## Fix Priority (ordered by impact)

### Phase 1: Memory (blocks performance too)
1. **Fix `ptr.* = struct` compiler bug** — enables direct cache array access, eliminates List overhead for SSA values
2. **Go Cache pattern** — pre-allocated value/block arrays, zero per-function allocation
3. **Free all temporaries** — SSA pass Maps/Lists, GenState, Prog chains
4. **Target: <200MB RSS** — fits in cache, major perf improvement for free

### Phase 2: Eliminate Unnecessary Work
5. **Pointer-based value access** — `getValuePtr() *SsaValue` instead of copy-return `getValue() SsaValue`
6. **Skip ARC for non-ARC fields** — don't retain/release empty strings in SsaValue
7. **Pre-size Lists** — `ensureCapacity` based on IR function size, avoid repeated realloc
8. **Target: ~60s** (4x improvement from memory + copy elimination)

### Phase 3: Compiler Optimizations
9. **Inlining** — inline small functions (<20 instructions), especially List.get/set, SsaValue accessors
10. **Constant folding** — evaluate constant expressions at compile time
11. **Dead code elimination** — remove unused values/computations
12. **Register allocation improvements** — reduce spills
13. **Target: ~10-15s** (matching Go's self-compilation time at similar maturity)

### Phase 4: Advanced
14. **Loop optimizations** — hoist invariants, unroll small loops
15. **Escape analysis** — stack-allocate values that don't escape
16. **PGO** — profile-guided optimization using selfcot's own profile
17. **Target: <5s** (matching Zig-compiled cot)

## Benchmarks to Track

```bash
# Full self-compilation (the canonical benchmark)
/usr/bin/time -l /tmp/selfcot build self/main.cot -o /tmp/selfcot2

# Type-check only (no codegen — isolates frontend perf)
/usr/bin/time -l /tmp/selfcot check self/main.cot

# Single large file (isolates per-function overhead)
/usr/bin/time -l /tmp/selfcot build self/frontend/ssa_builder.cot -o /dev/null
```

## Reference Points

| Compiler | Self-compile time | Lines | Ratio |
|----------|------------------|-------|-------|
| Go 1.4 (first self-hosted) | ~60s | 500K | 120μs/line |
| Go 1.22 (current) | ~15s | 2M+ | 7.5μs/line |
| Zig (stage2) | ~90s | 300K | 300μs/line |
| Rust (rustc) | ~300s | 600K+ | 500μs/line |
| **Cot selfcot (current)** | **240s** | **42K** | **5700μs/line** |
| **Cot cot (Zig-compiled)** | **4.5s** | **42K** | **107μs/line** |

Selfcot is 53x slower than the Zig-compiled version and 760x slower per-line than Go. The gap is expected for an unoptimized first self-hosted compiler, but it confirms that optimization work will have massive payoff.
