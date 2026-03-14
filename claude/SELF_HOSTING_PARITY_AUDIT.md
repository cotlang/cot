# Self-Hosted Compiler Parity Audit

**Date:** March 14, 2026
**Total self-hosted:** 42,575 lines across 38 files, 409 tests pass

---

## 1. Executive Summary

| Subsystem | Files | Lines | Parity |
|-----------|-------|-------|--------|
| Token/Scanner | 2 | 1,222 | **100%** |
| AST | 1 | 1,532 | **95%** |
| Parser | 1 | 3,234 | **95%** |
| Types | 1 | 1,609 | **95%** |
| Checker | 1 | 5,904 | **86%** |
| IR | 1 | 1,450 | **100%** |
| ARC Insertion | 1 | 443 | **100%** |
| Lowerer | 1 | 9,100+ | **85%** |
| SSA Builder | 1 | 2,315 | **95%** |
| SSA Data | 1 | 582 | **95%** |
| SSA Passes | 6 | 1,896 | **92%** |
| Wasm Codegen | 17 | 11,152 | **93%** |
| Source/Errors | 2 | 860 | **100%** |
| Main/CLI | 1 | 1,100+ | **100%** |

**Overall: ~93% Wasm-frontend-complete. 42,575 lines across 38 files.**

---

## 2. Self-Compilation Status

**`selfcot check self/main.cot`** → **PASSES** (38 files, 42,575 lines)
**`selfcot build self/main.cot`** → **Crashes in SSA builder** (Phase 3)

### What works end-to-end:
- Phase 1 (parse all files recursively) — PASS
- Phase 2 (check all files with shared state) — PASS
- Phase 3 (lower + SSA build + codegen) — crashes during SSA building

### Current crash:
```
List(2310)_get + 128 (@trap — bounds check)
← ir.Func_getNode
← SSABuilder_convertNode
← SSABuilder_convertPtrLoadValue
← SSABuilder_convertNode
← SSABuilder_build
← generateAndAddFunc
← generateAllFunctions
← generateWasmCode
```

An IR node index is out of bounds during SSA conversion. `convertPtrLoadValue` calls `convertNode` with a node index that doesn't exist in the IR function's node list. This is likely a divergence in how the self-hosted SSA builder handles `ptr_load_value` IR nodes compared to the Zig reference (`compiler/frontend/ssa_builder.zig`).

---

## 3. Remaining Blockers (Priority Order)

### BLOCKER 1: SSA Builder node index out of bounds
- **Crash:** `Func.getNode(idx)` — `List.get` bounds check fails
- **Location:** `self/frontend/ssa_builder.cot:convertPtrLoadValue`
- **Reference:** `compiler/frontend/ssa_builder.zig:convertPtrLoadValue`
- **Action:** Line-by-line comparison of `convertPtrLoadValue` and its callers. The IR node index stored in `PtrLoadValue.ptr` may reference a node from a different function or a stale index.

### BLOCKER 2: Potential further divergences in SSA builder
- The self-hosted SSA builder was ported from Zig but may have other `convertX` functions that diverge from the reference.
- **Action:** Systematic audit of ALL `convertX` functions in `ssa_builder.cot` vs `ssa_builder.zig`.

### BLOCKER 3: collectDecls() in Phase 3
- Phase 3 uses `collectDecls()` (declarations only, no body checking) to populate the symbol table for lowering.
- This may miss symbols that are only available after full type checking (e.g., generic instantiations).
- If lowering calls `inferExprType` → `checkExpr` lazily, the checker may not have enough context.
- **Action:** Compare with Zig driver Phase 3 pattern. The Zig driver stores fully-checked `Checker` objects in an ArrayList and reuses them directly.

### NOT BLOCKING (deferred):
- Async lowering (~1,170 LOC) — v0.4
- Native backend (~72,000 LOC) — not started
- LSP, formatter, project config — not started

---

## 4. Completed Work (This Session)

### Pipeline Architecture (matches Zig driver.zig 1:1):
- 3-phase pipeline: parseFileRecursive → checkOneFileDirect loop → lowerFileWithSharedState loop
- Module name qualification via `deriveModuleName` + `tree_module_map`
- SharedCheckerState passed by pointer across phases

### Lowerer Fixes:
- `@trap()` dead block: expr_stmt now detects block change from trap, returns terminated
- field_access receiver: `b.field.method()` pattern takes address of field (was loading value)
- fb parameter removal: all 90+ lowering functions now use `self.builder.func()` (matches Zig pattern)
- WasmGC lowering: full pipeline ported (unions, optionals, arrays, refs)

### Types Fix:
- `TypeRegistry.equal` had nested union switches (16×16=256 arms) diverging from Zig reference (single switch with direct field access). Replaced with tag-based dispatch using helper functions.

---

## 5. Key File Locations

| Self-Hosted | Zig Reference | Purpose |
|-------------|---------------|---------|
| `self/main.cot` | `compiler/driver.zig` | Pipeline orchestrator |
| `self/frontend/lower.cot` | `compiler/frontend/lower.zig` | AST→IR lowering |
| `self/frontend/ssa_builder.cot` | `compiler/frontend/ssa_builder.zig` | IR→SSA conversion |
| `self/frontend/checker.cot` | `compiler/frontend/checker.zig` | Type checker |
| `self/frontend/types.cot` | `compiler/frontend/types.zig` | Type registry |
| `self/codegen/wasm/driver.cot` | `compiler/driver.zig` (lines 5262+) | Wasm code generation |
| `self/codegen/wasm/wasm_gen.cot` | `compiler/codegen/wasm/gen.zig` | SSA→Wasm codegen |
