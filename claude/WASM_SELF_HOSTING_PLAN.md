# Wasm Self-Hosting Plan

**Date:** 2026-03-16
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm binary that can compile itself.
**Metric:** `wasmtime /tmp/selfcot.wasm build self/test_tiny.cot -o /tmp/out.wasm` succeeds.

---

## Current State (Verified 2026-03-16)

**What works:**
| Test | Result | RSS | Time |
|------|--------|-----|------|
| `selfcot check self/main.cot` | PASS (38 files) | 9MB | 0.01s |
| `selfcot build self/test_tiny.cot` | PASS (valid Wasm) | 3MB | <0.01s |
| `selfcot build self/test_import_only.cot` | PASS (imports List) | 4MB | <0.01s |
| `selfcot build self/frontend/token.cot` | PASS (448 lines) | 5MB | <0.01s |
| Free fn with struct-by-value param | PASS | — | — |
| Method with self only (no struct param) | PASS | — | — |

**What fails:**
| Test | Result | Cause |
|------|--------|-------|
| `selfcot build self/frontend/source.cot` | assertion failed | Span_merge: method + struct-by-value param |
| `selfcot build self/frontend/scanner.cot` | assertion failed | Same root cause (imports source.cot) |
| `selfcot build self/main.cot` | assertion failed | Same root cause (transitively imports source.cot) |

**Minimal repro of the blocker:**
```cot
struct Span {
    start: int,
    end: int,

    fn getOtherStart(other: Span) Span {  // self ptr + struct-by-value = CRASH
        return other.start
    }
}
```
Free function `fn getStart(s: Span) int` works. Method `fn sum() int` (self only) works. The bug is specifically: **self pointer (param 0) + struct-by-value (param 1+) = incorrect SSA**.

---

## Completeness Assessment

| Component | Lines | Status |
|-----------|-------|--------|
| Frontend (scanner, parser, checker, lower, ssa_builder, types, ir, ast) | 28,355 | 100% complete |
| Wasm codegen (wasm_gen, driver, preprocess, assemble, link) | 4,341 | 100% complete |
| Runtime (mem, print, wasi, test, bench, slice) | 3,720 | 100% complete |
| SSA passes (8 passes: copyelim, cse, deadcode, decompose, layout, rewritedec, rewritegeneric, schedule) | 2,636 | 100% complete |
| Support (code_builder, wasm_types, prog, constants, ssa_passes shims) | 2,008 | 100% complete |
| Entry point (main.cot) | 1,222 | 100% complete |
| **Total** | **43,364** | **Code complete, 1 bug blocking** |

---

## Steps to Self-Hosting

### Step 1: Fix method + struct-by-value parameter bug

**The only code bug blocking self-hosting.**

The SSA builder's `initLargeStructParam` decomposes compound params (>8 bytes) into multiple i64 arg registers. When the function is a method, `self` is param 0 (a pointer, 1 register). The compound param is param 1+ (decomposed into N registers). The `phys_reg_idx` counter tracks which register each param maps to.

**Hypothesis:** The `phys_reg_idx` is getting out of sync when self is a pointer param followed by a large struct param. The SSA builder assigns register indices incorrectly, causing later field access to read from wrong memory.

**Fix approach (per TROUBLESHOOTING.md):**
1. Compare `self/frontend/ssa_builder.cot` param init (lines 200-260) against `compiler/frontend/ssa_builder.zig` (lines 150-240) line by line
2. Check that `phys_reg_idx` advances correctly for self pointer params
3. Check that `local_slot_offsets` are computed correctly when self has size=0 vs size=8
4. Check field_local offset computation for the second param

**Verification:** The minimal repro above must pass.

### Step 2: Fix verify() assertion for functions with phis + struct params

After Step 1, `verify()` may still fail because `insertPhis` produces phis with arg counts that don't match predecessor counts. This is likely a consequence of Step 1's incorrect param handling — fixing the root cause may resolve this.

**Verification:** `selfcot build self/frontend/source.cot` succeeds.

### Step 3: Build scanner.cot end-to-end

Once source.cot builds, scanner.cot should build (it imports source.cot). Test:
```bash
/tmp/selfcot build self/frontend/scanner.cot -o /tmp/scanner.wasm
# Compare against Zig compiler output
cot build self/frontend/scanner.cot --target=wasm32 -o /tmp/scanner_zig.wasm
```

If the Wasm outputs differ, compare function by function to find codegen differences.

**Verification:** scanner.cot produces a valid .wasm file.

### Step 4: Build progressively larger files

Test each file individually to find any remaining bugs:
```bash
for f in token source errors scanner types ir ast ssa ssa_builder parser checker lower; do
    /tmp/selfcot build self/frontend/$f.cot -o /tmp/$f.wasm 2>&1
    echo "$f: $?"
done
```

Fix bugs as they appear. Each file exercises different language features:
- `types.cot`: complex unions, large switch statements, recursive type functions
- `checker.cot`: largest file (5,907 lines), heavy Map usage, generic resolution
- `lower.cot`: largest file (9,177 lines), 100+ conversion functions, generic lowering
- `parser.cot`: deeply nested expression parsing, operator precedence

### Step 5: Build main.cot (full self-compilation)

```bash
/tmp/selfcot build self/main.cot -o /tmp/selfcot_gen1.wasm
```

This compiles all 42 files through the selfcot pipeline to produce a Wasm binary.

**Verification:** The command completes without errors. Output is a valid Wasm module.

### Step 6: Test the generated Wasm binary

```bash
# Run the Wasm selfcot on test_tiny
wasmtime /tmp/selfcot_gen1.wasm -- build self/test_tiny.cot -o /tmp/gen1_tiny.wasm

# Compare output
diff <(xxd /tmp/tiny.wasm) <(xxd /tmp/gen1_tiny.wasm)
```

The generated Wasm selfcot should produce identical output to the native selfcot.

### Step 7: Full bootstrap (gen2)

```bash
# Gen2: Wasm selfcot compiles itself
wasmtime /tmp/selfcot_gen1.wasm -- build self/main.cot -o /tmp/selfcot_gen2.wasm

# Gen1 and Gen2 should produce identical output (fixed point)
diff <(xxd /tmp/selfcot_gen1.wasm) <(xxd /tmp/selfcot_gen2.wasm)
```

If gen1 == gen2, self-hosting is complete. The compiler is a fixed point.

---

## Risk Assessment

| Step | Risk | Rationale |
|------|------|-----------|
| 1 (param bug) | LOW | Well-isolated, minimal repro exists, reference code available |
| 2 (verify) | LOW | Likely resolved by Step 1 |
| 3 (scanner) | LOW | Small file, exercises basic features |
| 4 (all files) | MEDIUM | Large files may reveal new edge cases in codegen |
| 5 (main.cot) | MEDIUM | Multi-file compilation, generic monomorphization at scale |
| 6 (wasmtime) | MEDIUM | WASI compatibility, file I/O, argument parsing in Wasm |
| 7 (bootstrap) | LOW | If gen1 works, gen2 is mechanical |

**Critical path:** Step 1 → Step 5. Steps 2-4 are incremental validation. Steps 6-7 are the actual self-hosting proof.

---

## What NOT to Do

- Do NOT add arena allocators, CoW, or deinit to stdlib (P2/P3 optimizations, not blockers)
- Do NOT modify the Zig compiler — all remaining bugs are in selfcot's Cot code
- Do NOT skip verify() permanently — it catches real bugs
- Do NOT write more planning documents — this is the last one
