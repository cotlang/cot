# Self-Hosting Bootstrap: Audit & Path to Completion

**Date:** 2026-03-20
**Status:** 37/40 files compile individually. 3 optimizer files have API mismatch bugs.

---

## Full Audit: self/ vs Zig Compiler

### Compilation Status (audited 2026-03-20)

| File | Lines | Compiles | Notes |
|------|------:|:--------:|-------|
| **parse/** | | | |
| token.cot | 451 | OK | Complete |
| source.cot | 315 | OK | Complete |
| scanner.cot | 774 | OK | Complete |
| ast.cot | 1,533 | OK | Complete |
| parser.cot | 3,260 | OK | Complete |
| **check/** | | | |
| errors.cot | 545 | OK | Complete |
| types.cot | 1,609 | OK | Complete |
| checker.cot | 5,981 | OK | 1 TODO line 5633 (generic type resolution in free functions) |
| **build/** | | | |
| ir.cot | 1,467 | OK | Complete |
| ssa.cot | 625 | OK | Complete |
| builder.cot | 2,364 | OK | Complete |
| lower.cot | 9,201 | OK | 1 TODO line 811 (evalComptimeValue) |
| arc.cot | 443 | OK | Complete |
| **optimize/** | | | |
| copyelim.cot | ~100 | **FAIL** | Uses `args_len`/`getArg()` — should be `args.count`/`args.get()` |
| cse.cot | ~200 | **FAIL** | Same API mismatch |
| deadcode.cot | ~100 | **FAIL** | Same API mismatch |
| decompose.cot | ~200 | OK | Uses correct `args.count`/`args.get()` |
| layout.cot | ~150 | OK | Complete |
| rewrite.cot | ~300 | OK | Complete |
| rewritedec.cot | ~250 | OK | Complete |
| schedule.cot | ~200 | OK | Complete |
| **emit/wasm/** | | | |
| assemble.cot | ~400 | OK | Complete |
| bench.cot | ~100 | OK | Complete |
| builder.cot | ~200 | OK | Complete |
| constants.cot | ~100 | OK | Complete |
| driver.cot | ~400 | OK | Missing 3 optimizer pass calls (see below) |
| gen.cot | ~800 | OK | Complete |
| link.cot | ~300 | OK | Complete |
| lower.cot | ~500 | OK | 1 TODO line 246 (return_call translation) |
| mem.cot | ~200 | OK | Complete |
| passes_dec.cot | ~200 | OK | Complete |
| passes.cot | ~300 | OK | Complete |
| preprocess.cot | ~150 | OK | Complete |
| print.cot | ~200 | OK | Complete |
| prog.cot | ~150 | OK | Complete |
| slice.cot | ~150 | OK | Complete |
| test.cot | ~200 | OK | Complete |
| types.cot | ~200 | OK | Complete |
| wasi.cot | ~300 | OK | Complete |
| **main.cot** | ~1,100 | OK | Missing imports for copyelim/cse/deadcode |

**Total: ~43,600 lines across 40 files. 37 compile, 3 fail.**

---

## Bugs Found

### Bug 1: Optimizer API Mismatch (3 files)

**Files:** `copyelim.cot`, `cse.cot`, `deadcode.cot`

**Root cause:** These files were written against an older `SsaValue` interface that had `args_len` field and `getArg()` method. The actual struct in `self/build/ssa.cot` uses `args: List(int)` with `.count` and `.get()`.

**Fix (find-and-replace in each file):**
- `val.args_len` → `val.args.count`
- `val.getArg(ai)` → `val.args.get(ai)`
- `a.args_len` → `a.args.count`
- `b.args_len` → `b.args.count`
- `a.getArg(ai)` → `a.args.get(ai)`
- `b.getArg(ai)` → `b.args.get(ai)`
- `result.args_len` → `result.args.count`

Special case in `copyelim.cot` line 63: `val.args_len = 0` — needs a list clear: `val.args.clear()` or `val.args.count = 0`

### Bug 2: Missing Optimizer Pass Invocations

**File:** `self/emit/wasm/driver.cot` lines 358-363

**Current pass pipeline:**
```
passRewriteGeneric → decompose → passRewriteDec → schedule → layout → lower
```

**Expected pipeline (from Zig `driver.zig:6158-6167`):**
```
copyelim → passRewriteGeneric → decompose → passRewriteDec → copyelim → cse → deadcode → schedule → layout → lower
```

**Missing:** `copyelim` (called twice), `cse`, `deadcode`

### Bug 3: Missing Imports in main.cot

`self/main.cot` does not import `optimize/copyelim`, `optimize/cse`, `optimize/deadcode`. These need to be added for the optimizer passes to be available.

---

## TODOs in Code

| File | Line | Description |
|------|------|-------------|
| `checker.cot` | 5633 | Fix selfcot's type resolution for generic types in free functions |
| `lower.cot` | 811 | evalComptimeValue when comptime evaluation is fully ported |
| `emit/wasm/lower.cot` | 246 | Fix native return_call translation before enabling |

None of these block compilation or basic correctness.

---

## Steps to Bootstrap Loop

### Step 1: Fix 3 optimizer files
- Fix `args_len`/`getArg()` → `args.count`/`args.get()` in copyelim, cse, deadcode
- Verify all 40/40 files compile individually

### Step 2: Add missing optimizer imports and pass calls
- Add imports to `main.cot`: `import "optimize/copyelim"`, `import "optimize/cse"`, `import "optimize/deadcode"`
- Add pass calls to `emit/wasm/driver.cot` in correct order

### Step 3: Full build test
```bash
# Build selfcot (native) from Zig compiler
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot

# Selfcot compiles test_tiny.cot to Wasm
/tmp/selfcot build self/test_tiny.cot -o /tmp/tiny.wasm
wasmtime /tmp/tiny.wasm
```

If `test_tiny.cot` output is correct, basic compilation works.

### Step 4: Fix Wasm runtime output
Currently selfcot's Wasm output doesn't print (fd_write not connected to WASI). Debug:
```bash
# Compare Wasm sections
wasm-objdump -x /tmp/tiny_selfcot.wasm | head -50
wasm-objdump -x /tmp/tiny_zig.wasm | head -50
```

Look for missing WASI imports (`fd_write`, `proc_exit`) or wrong function indices.

### Step 5: Selfcot compiles itself
```bash
# Selfcot (native) → selfcot2 (Wasm)
/tmp/selfcot build self/main.cot -o /tmp/selfcot2.wasm

# Selfcot2 (Wasm) compiles test_tiny
wasmtime /tmp/selfcot2.wasm build self/test_tiny.cot -o /tmp/tiny2.wasm
wasmtime /tmp/tiny2.wasm
```

### Step 6: Bootstrap loop closes
```bash
# Selfcot2 (Wasm) → selfcot3 (Wasm)
wasmtime /tmp/selfcot2.wasm build self/main.cot -o /tmp/selfcot3.wasm

# Fixed point: selfcot2 == selfcot3
diff /tmp/selfcot2.wasm /tmp/selfcot3.wasm && echo "BOOTSTRAP COMPLETE"
```

If `selfcot2.wasm` and `selfcot3.wasm` are identical, the bootstrap loop has closed. Check into repo as `bootstrap/cot.wasm`.

### Step 7: Release
- Check `bootstrap/cot.wasm` into repo
- Update `cot.json` with bootstrap instructions
- Fresh clone bootstraps with only `wasmtime`
- Tag as Cot 0.4

---

## Architecture Reference

```
Zig Compiler (compiler/*.zig)
    ↓ zig build
selfcot (native binary)
    ↓ selfcot build self/main.cot -o selfcot2.wasm
selfcot2.wasm (Wasm binary)
    ↓ wasmtime selfcot2.wasm build self/main.cot -o selfcot3.wasm
selfcot3.wasm (Wasm binary)
    ↓ diff selfcot2.wasm selfcot3.wasm → IDENTICAL = bootstrap closed
```

## Key Files

| File | Purpose |
|------|---------|
| `self/main.cot` | Selfcot entry point — multi-file compile pipeline |
| `self/emit/wasm/driver.cot` | Wasm codegen driver — SSA pass pipeline + emission |
| `self/build/ssa.cot` | SsaValue struct — reference for correct API (`args: List(int)`) |
| `self/test_tiny.cot` | Minimal test file for smoke-testing selfcot output |
| `compiler/driver.zig` | Zig compiler reference — pass pipeline order (lines 6158-6167) |
