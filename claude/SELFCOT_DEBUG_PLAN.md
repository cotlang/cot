# Selfcot Pipeline Debugger — Execution Plan

**Date:** 2026-03-23
**Status:** Planning
**Goal:** Port the entire Zig compiler debug infrastructure to selfcot, 1:1

---

## Why This Matters

Selfcot currently has **zero** debug infrastructure. When a selfcot-compiled binary generates wrong Wasm:
1. You can't see what any pass did
2. You can't trace a value through the pipeline
3. You can't compare selfcot's decisions against the Zig compiler
4. You add `eprintln()` statements, rebuild (8 seconds), test, repeat 20x

After this work: `COT_DEBUG=all /tmp/selfcot build file.cot` shows the ENTIRE pipeline with per-pass, per-function, per-value detail — identical format to the Zig compiler.

---

## Current State

| Component | Zig Compiler | Selfcot |
|-----------|-------------|---------|
| Debug infrastructure | `pipeline_debug.zig` (166 lines) | **Nothing** |
| Phase filtering | `COT_DEBUG=phase1,phase2` | **Nothing** |
| Function tracing | `COT_TRACE=funcname` | **Nothing** |
| SSA pass logging | 57 `debug.log()` calls | **0 calls** |
| Wasm codegen logging | 32 `debug.log()` calls | **0 calls** |
| Frontend logging | 9 `debug.log()` calls | **0 calls** |
| **Total** | **95 debug.log() calls** | **0** |

---

## File Mapping: Zig → Selfcot

### Infrastructure
| Zig File | Selfcot File | Status |
|----------|-------------|--------|
| `compiler/pipeline_debug.zig` | `self/debug.cot` (NEW) | Not started |

### Frontend
| Zig File | Debug Calls | Selfcot File | Lines |
|----------|------------|-------------|-------|
| `compiler/frontend/checker.zig` | 5 | `self/check/checker.cot` | 4,538 |
| `compiler/frontend/lower.zig` | 3 | `self/build/lower.cot` | 6,760 |
| `compiler/frontend/ssa_builder.zig` | 1 | `self/build/ssa_builder.cot` | 2,841 |

### SSA Passes
| Zig File | Debug Calls | Selfcot File | Lines |
|----------|------------|-------------|-------|
| `compiler/ssa/passes/copyelim.zig` | 2 | `self/optimize/copyelim.cot` | ~200 |
| `compiler/ssa/passes/cse.zig` | 2 | `self/optimize/cse.cot` | ~300 |
| `compiler/ssa/passes/deadcode.zig` | 4 | `self/optimize/deadcode.cot` | ~300 |
| `compiler/ssa/passes/decompose.zig` | 5 | `self/optimize/decompose.cot` | ~400 |
| `compiler/ssa/passes/layout.zig` | 2 | `self/optimize/layout.cot` | ~200 |
| `compiler/ssa/passes/lower_wasm.zig` | 4 | `self/emit/wasm/lower.cot` | ~400 |
| `compiler/ssa/passes/rewritedec.zig` | 29 | `self/optimize/rewrite.cot` | ~800 |
| `compiler/ssa/passes/rewritegeneric.zig` | 5 | `self/optimize/rewrite.cot` | (same file) |
| `compiler/ssa/passes/schedule.zig` | 4 | `self/optimize/schedule.cot` | ~300 |

### Wasm Codegen
| Zig File | Debug Calls | Selfcot File | Lines |
|----------|------------|-------------|-------|
| `compiler/codegen/wasm/gen.zig` | 11 | `self/emit/wasm/gen.cot` | 2,726 |
| `compiler/codegen/wasm/wasm.zig` | 8 | `self/emit/wasm/wasm.cot` | ~600 |
| `compiler/codegen/wasm/preprocess.zig` | 3 | `self/emit/wasm/preprocess.cot` | ~700 |
| `compiler/codegen/wasm/assemble.zig` | 5 | `self/emit/wasm/assemble.cot` | 918 |
| `compiler/codegen/wasm/link.zig` | 1 | `self/emit/wasm/link.cot` | ~800 |

### Driver
| Zig File | Debug Calls | Selfcot File | Lines |
|----------|------------|-------------|-------|
| `compiler/driver.zig` | ~10 | `self/emit/wasm/driver.cot` | 664 |
| `compiler/main.zig` | 2 | `self/main.cot` | 1,239 |

---

## Implementation Tasks

### Task 1: Create `self/debug.cot` — Debug Infrastructure
**Port of:** `compiler/pipeline_debug.zig` (166 lines)
**Priority:** P0 — everything depends on this

```cot
// Phase enum
const Phase = enum {
    parse, check, lower, ssa, deadcode, copyelim,
    phielim, schedule, regalloc, codegen, strings, abi,
}

// Global state
var g_phases_enabled: [12]bool = [false; 12]
var g_all_enabled: bool = false
var g_trace_func: string = ""
var g_initialized: bool = false

// Initialize from COT_DEBUG and COT_TRACE env vars
fn initGlobal() void { ... }

// Check if a phase is enabled
fn isEnabled(phase: Phase) bool { ... }

// Log a message if phase is enabled
fn log(phase: Phase, msg: string) void { ... }

// Check if tracing a specific function
fn shouldTrace(func_name: string) bool { ... }
```

**Key considerations:**
- [ ] Must use `std/os` for env var access (`env()` or `environCount`/`environPtr`)
- [ ] Must work when compiled to Wasm (selfcot2.wasm running under wasmtime)
- [ ] Zero cost when disabled — just boolean checks
- [ ] Format: `[phase] message` matching Zig compiler output exactly

### Task 2: Wire `debug.initGlobal()` into `self/main.cot`
**Port of:** `compiler/main.zig` line 126

- [ ] Add `import "debug"` to main.cot
- [ ] Call `debug.initGlobal()` at start of main function (before arg parsing)
- [ ] Add top-level pipeline markers:
  ```
  debug.log(.parse, "parsing '${path}'")
  debug.log(.check, "checking '${path}'")
  debug.log(.lower, "lowering '${path}'")
  debug.log(.codegen, "generating wasm for '${path}'")
  ```

### Task 3: Port Checker Logging (5 calls)
**Port of:** `compiler/frontend/checker.zig` debug.log calls
**Target:** `self/check/checker.cot`

- [ ] Import debug module
- [ ] Add check entry: `debug.log(.check, "=== Type checking file (${decls.len()} declarations, safe=${safe_mode}) ===")`
- [ ] Add pass markers: `debug.log(.check, "  pass 1: type declarations collected")`
- [ ] Add per-function: `debug.log(.check, "  check fn '${name}' (${params.len()} params)")`
- [ ] Add check exit: `debug.log(.check, "=== Type checking complete (${types.count} types registered) ===")`

### Task 4: Port Lowerer Logging (3 calls)
**Port of:** `compiler/frontend/lower.zig` debug.log calls
**Target:** `self/build/lower.cot`

- [ ] Import debug module
- [ ] Add lower entry: `debug.log(.lower, "=== Lowering AST to IR (${decls} declarations) ===")`
- [ ] Add per-function: `debug.log(.lower, "  lower fn '${name}' (${params} params)")`
- [ ] Add lower exit: `debug.log(.lower, "=== Lowering complete: ${funcs} IR functions generated ===")`

### Task 5: Port SSA Builder Logging (1 call)
**Port of:** `compiler/frontend/ssa_builder.zig` debug.log call
**Target:** `self/build/ssa_builder.cot` (or wherever SSA is built)

- [ ] Add SSA summary: `debug.log(.ssa, "=== SSA built for '${name}': ${blocks} blocks, ${values} values, ${locals} locals ===")`

### Task 6: Port Copyelim Logging (2 calls)
**Port of:** `compiler/ssa/passes/copyelim.zig`
**Target:** `self/optimize/copyelim.cot`

- [ ] Entry: `debug.log(.copyelim, "=== Copyelim pass for '${name}' (${values} values, ${copies} copies, ${phis} phis) ===")`
- [ ] Exit: `debug.log(.copyelim, "=== Copyelim complete for '${name}': ${rewrites} control rewrites ===")`

### Task 7: Port CSE Logging (2 calls)
**Port of:** `compiler/ssa/passes/cse.zig`
**Target:** `self/optimize/cse.cot`

- [ ] Entry with counts
- [ ] Exit: `debug.log(.codegen, "=== CSE complete for '${name}': ${rewrites} rewrites ===")`

### Task 8: Port Deadcode Logging (4 calls)
**Port of:** `compiler/ssa/passes/deadcode.zig`
**Target:** `self/optimize/deadcode.cot`

- [ ] Entry with block/value counts
- [ ] Per unreachable block: `debug.log(.deadcode, "  block b${id} unreachable (${kind})")`
- [ ] Reachable/unreachable summary
- [ ] Exit with remaining counts

### Task 9: Port Decompose Logging (5 calls)
**Port of:** `compiler/ssa/passes/decompose.zig`
**Target:** `self/optimize/decompose.cot`

- [ ] Entry with block/value/phi counts
- [ ] Per-decomposition: `debug.log(.codegen, "  v${id}: decomposing slice phi")`
- [ ] Exit with decomposed count

### Task 10: Port Schedule Logging (4 calls)
**Port of:** `compiler/ssa/passes/schedule.zig`
**Target:** `self/optimize/schedule.cot`

- [ ] Entry with block/value counts
- [ ] Per-block: `debug.log(.schedule, "  block b${id}: ${count} values")`
- [ ] Exit

### Task 11: Port Layout Logging (2 calls)
**Port of:** `compiler/ssa/passes/layout.zig`
**Target:** `self/optimize/layout.cot`

- [ ] Entry with block count and entry block
- [ ] Exit with laid out count

### Task 12: Port Rewrite Logging (29 calls — LARGEST)
**Port of:** `compiler/ssa/passes/rewritedec.zig` + `rewritegeneric.zig`
**Target:** `self/optimize/rewrite.cot`

This is the most important module — 29 log calls covering every value transformation:
- [ ] Entry/exit markers with iteration counts
- [ ] slice_ptr rewrites (6 patterns)
- [ ] slice_len rewrites (4 patterns)
- [ ] string_ptr/string_len rewrites (6 patterns)
- [ ] opt_tag/opt_data rewrites (4 patterns)
- [ ] const_string → string_make rewrites (3 patterns)
- [ ] const_nil decompositions (2 patterns)
- [ ] Error union rewrites (2 patterns)
- [ ] Each with `debug.log(.codegen, "  v${id}: ${old} -> ${new}")` format

### Task 13: Port Lower Wasm Logging (4 calls)
**Port of:** `compiler/ssa/passes/lower_wasm.zig`
**Target:** `self/emit/wasm/lower.cot`

- [ ] Entry with block/value counts
- [ ] Per-value lowering: `debug.log(.codegen, "  lower v${id}: ${old_op} -> ${new_op}")`
- [ ] Unlowered op warnings
- [ ] Exit with lowered/unchanged counts

### Task 14: Port Gen Logging (11 calls)
**Port of:** `compiler/codegen/wasm/gen.zig`
**Target:** `self/emit/wasm/gen.cot`

- [ ] Function entry: name, block count
- [ ] Frame size and locals allocated
- [ ] Per-block: id, kind, value count, succ count
- [ ] Generated instruction and branch counts
- [ ] Stack depth validation warnings
- [ ] Unhandled op warnings
- [ ] Compound type tracking (string_len/opt_data without compound local)

### Task 15: Port Preprocess Logging (3 calls)
**Port of:** `compiler/codegen/wasm/preprocess.zig`
**Target:** `self/emit/wasm/preprocess.cot`

- [ ] Function entry with frame size
- [ ] Resume points, PC count, table entries
- [ ] Completion with branch count, dispatch loop status

### Task 16: Port Assemble Logging (5 calls)
**Port of:** `compiler/codegen/wasm/assemble.zig`
**Target:** `self/emit/wasm/assemble.cot`

- [ ] Function entry
- [ ] Local counts (params, i64, f64)
- [ ] Assembled byte count
- [ ] Error conditions (string not found, bounds violation)

### Task 17: Port Link Logging (1 call)
**Port of:** `compiler/codegen/wasm/link.zig`
**Target:** `self/emit/wasm/link.cot`

- [ ] Module summary: types, imports, funcs, globals, data segments
- [ ] Per-section byte counts

### Task 18: Port Driver Logging (~10 calls)
**Port of:** `compiler/driver.zig` generateWasmCode section
**Target:** `self/emit/wasm/driver.cot`

- [ ] Function count
- [ ] String literal registration
- [ ] Per-function SSA pass markers (matching Zig compiler's pass loop)
- [ ] COT_SSA HTML generation hooks (future — after debug logging works)

### Task 19: Verify Output Matches Zig Compiler

- [ ] Compile same file with both compilers using `COT_DEBUG=all`
- [ ] Diff the output — format and content should be identical
- [ ] Fix any discrepancies until output matches 1:1

---

## Implementation Order

```
Task 1  (debug.cot)       ← Foundation: everything depends on this
Task 2  (main.cot wire)   ← Initialize at startup
Task 18 (driver.cot)      ← Pass loop markers
Task 6-12 (optimize/)     ← SSA pass logging (7 files)
Task 13 (lower.cot)       ← Wasm lowering
Task 14-17 (emit/wasm/)   ← Wasm codegen logging (4 files)
Task 3-5 (check/build/)   ← Frontend logging (3 files)
Task 19 (verify)          ← Compare output with Zig compiler
```

**Priority justification:** The optimization passes and Wasm codegen are where selfcot bugs live. Frontend logging is lower priority because frontend bugs manifest as type errors (which are visible without debug logging).

---

## Success Criteria

1. `COT_DEBUG=all /tmp/selfcot build test/cases/arithmetic.cot` produces verbose output matching Zig compiler format
2. Every SSA pass reports entry stats, per-value transforms, exit stats
3. Every Wasm codegen stage reports function metrics, instruction counts, error conditions
4. `COT_DEBUG=codegen` shows only Wasm codegen, `COT_DEBUG=deadcode` shows only deadcode pass
5. Output is identical between Zig compiler and selfcot for the same input file
6. Zero performance impact when `COT_DEBUG` is not set

---

## Cot Syntax Considerations

Selfcot is compiled in `@safe` mode (`self/cot.json`). Key differences from Zig:

- **String interpolation:** `"v${v.id}: ${op_name}"` instead of `std.fmt.bufPrint`
- **Auto-pointer params:** `fn log(phase: Phase, msg: string)` — Phase is auto-pointer in @safe
- **No explicit `self`:** Methods auto-inject self
- **`+` is `++`:** String concat uses `+` in @safe mode (desugars to `++`)
- **Enum access:** `Phase.codegen` not `.codegen`

The debug module must use these patterns — it's Cot code, not Zig.
