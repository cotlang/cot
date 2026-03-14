# Self-Hosting Strategy: Zig Compiler → Cot Compiler

**Date:** Mar 14, 2026
**Goal:** Port the Cot compiler from Zig to Cot, achieving full self-hosting.

---

## Current State

| Subsystem | Zig Lines | Cot Lines | Status |
|-----------|-----------|-----------|--------|
| **Frontend** (scanner, parser, checker, lower, SSA builder, IR, types, AST, errors, arc_insertion) | 24,188 | 25,800+ | **~100%** |
| SSA passes (6 passes) | 4,557 | 1,896 | **92%** |
| Wasm codegen (all modules) | 9,993 | 11,152 | **93%** |
| Driver/CLI | 8,884 | 1,100+ | **partial** |
| Native codegen | 70,934 | — | 0% |
| LSP | 4,973 | — | 0% |
| Formatter | 1,234 | — | 0% |
| **Total** | **~139K** | **~42.6K** | **31%** |

**MILESTONE (Mar 13):** `selfcot check self/main.cot` passes — 38 files, 42,575 lines.
**MILESTONE (Mar 14):** Self-compilation reaches SSA builder phase (Phase 3).

---

## Self-Compilation Progress

`selfcot build self/main.cot` executes:
1. ✅ Phase 1: Parse all 38 files recursively
2. ✅ Phase 2: Check all files with shared state
3. ❌ Phase 3: Lower + SSA build — crashes in `SSABuilder.convertPtrLoadValue` (node index out of bounds)

### Fixes applied (Mar 14):
1. Module name qualification (`deriveModuleName` + `tree_module_map`)
2. @trap() dead block detection in expr_stmt
3. 3-phase pipeline (parse all → check all → lower all)
4. field_access receiver in lowerMethodCall
5. fb parameter removal (90+ functions, matches Zig `self.current_func` pattern)
6. TypeRegistry.equal nested union switch elimination
7. Re-applied correct 3-phase architecture (collectDecls for Phase 3)

### Next fix needed:
SSA builder `convertPtrLoadValue` diverges from Zig reference. Line-by-line comparison required.

---

## Architecture

The Zig driver uses a flat 3-phase pattern in one function:
```
Phase 1: parseFileRecursive → parsed_files list
Phase 2: for pf in parsed_files: check(pf) with shared state
Phase 3: for pf in parsed_files: lower(pf) with shared state
```

The self-hosted driver now matches this 1:1:
```
Phase 1: parseFileRecursive → parsed_files list
Phase 2: for pf in parsed_files: checkOneFileDirect(pf, shared)
Phase 3: for pf in parsed_files: lowerFileWithSharedState(pf, shared)
```

Phase 3 uses `collectDecls()` instead of full `checkFile()` — only populates the symbol table for `lookupSymbol` during lowering, without re-resolving types.
