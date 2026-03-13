# Self-Hosted Compiler Parity Audit

**Date:** March 13, 2026 (full re-audit)
**Scope:** File-by-file comparison of `self/` vs `compiler/` (Zig reference)
**Total self-hosted:** 39,945 lines across 36 files, 398 tests pass

---

## 1. Executive Summary

| Subsystem | Files | Lines | Parity | Critical Gaps |
|-----------|-------|-------|--------|---------------|
| Token/Scanner | 2 | 1,217 | **97%** | Missing `kw_spawn`, `kw_select` tokens |
| AST | 1 | 1,532 | **95%** | None (SelectExpr already present) |
| Parser | 1 | 3,158 | **87%** | Spawn/select token defs block integration |
| Types | 1 | 1,609 | **85%** | Shape stenciling struct (~80 LOC) |
| Checker | 1 | 5,501 | **82%** | `buildStructTypeWithLayout`, `evalComptimeValue` |
| IR | 1 | 1,356 | **100%** | None |
| ARC Insertion | 1 | 443 | **100%** | None |
| Lowerer | 1 | 8,527 | **76%** | WasmGC helpers, async (~1,170L) |
| SSA Builder | 1 | 2,130 | **95%** | None |
| SSA Data | 1 | 577 | **95%** | None |
| SSA Passes | 6 | 1,896 | **84%** | lower_wasm (64%), rewritedec (80%) |
| Wasm Codegen | 16 | 10,541 | **85%** | constants.cot (18%), wasm_gen edge cases |
| Source/Errors | 2 | 860 | **100%** | None |
| Main/CLI | 1 | 788 | **100%** | None |

**Overall: ~85% feature-complete. 39,945 lines across 36 files.**

**Remaining gaps by priority:**
1. **HIGH**: Token keywords (5 LOC), lower_wasm pass gaps (~205 LOC), rewritedec gaps (~130 LOC)
2. **MEDIUM**: Shape stenciling (~80 LOC), buildStructTypeWithLayout (~42 LOC), constants.cot (~500 LOC)
3. **LOW**: evalComptimeValue (~300 LOC), WasmGC helpers (~107 LOC)
4. **DEFERRED**: async lowering (~1,170 LOC — v0.4)

---

## 2. Token & Scanner — 97%

**Files:** `token.cot` (443 lines), `scanner.cot` (774 lines)
**Reference:** `token.zig` (333 lines), `scanner.zig` (527 lines)

All token variants and scanner methods present. Scanner fully functional for all lexing.

**Gap:** Token enum missing `kw_spawn` and `kw_select` (added to Zig compiler in Mar 2026 concurrency work). 5 LOC to add.

---

## 3. AST — 95%

**File:** `ast.cot` (1,532 lines)
**Reference:** `ast.zig` (696 lines)

All 13 declaration types, 29+ expression types, 8+ statement types present.
All 59 builtins mapped. SelectExpr/SelectCase already present.
Cot is 2x lines due to explicit struct definitions and inline tests.

---

## 4. Parser — 87%

**File:** `parser.cot` (3,158 lines, 66 functions)
**Reference:** `parser.zig` (2,340 lines, 55 functions)

All critical parsing logic implemented. Cot has 11 extra helper methods for
safe mode (`@safe` colon syntax, field shorthand) and concurrency (select/spawn parsing).

**Gap:** Spawn/select parsing exists but token definitions missing (blocks integration tests).
Minor: `unexpectedToken()` less detailed than Zig version.

---

## 5. Types — 85%

**File:** `types.cot` (1,609 lines)
**Reference:** `types.zig` (941 lines)

All 13 type structures, TypeRegistry with all methods, 10 type constructors,
all type queries (sizeOf, alignOf, isPointer, isArray, etc.).

**Gap:** Shape stenciling struct missing (~80 LOC). Shape is needed for 3-tier
generic optimization (same-shape types share code). All other type infrastructure complete.

---

## 6. Checker — 82%

**File:** `checker.cot` (5,501 lines)
**Reference:** `checker.zig` (4,350 lines)

### 6.1 Complete
- All expression checking (binary, unary, call, index, field access, etc.)
- All statement checking (if, while, for, block, return, etc.)
- Type resolution (`resolveTypeExpr`)
- Generic instantiation (`instantiateGenericFunc`, `instantiateGenericImplMethods`)
- Comptime evaluation (evalConstExpr, evalConstFloat, evalConstBinary, evalConstUnary, etc.)
- Comptime block/assign/inline-for (inlined into evalComptimeStmts)
- Lint checks W001-W005 (unused vars/params, shadowing, unreachable, empty block)
- Method resolution (delegates to types.lookupMethod)
- RLS (Result Location Semantics) via expected_type save/restore
- collectNestedDecl — switch on Decl union (matches Zig pattern)
- Multi-file scope management (loadSharedState/syncToShared)

### 6.2 Missing

| Function | LOC | Priority | Description |
|----------|-----|----------|-------------|
| `buildStructTypeWithLayout()` | ~42 | **HIGH** | Field offset calculation for packed/extern/auto layout |
| `evalComptimeValue()` | ~300 | LOW | Rich comptime value union (array/struct construction) |
| `checkBenchDecl()` | ~26 | MEDIUM | Benchmark declaration checking |
| `checkStmtsWithReachability()` | ~59 | LOW | Full type pre-pass + reachability |
| Typo suggestions | ~60 | LOW | editDistSuggest, errWithSuggestion |

---

## 7. Lowerer — 76%

**File:** `lower.cot` (8,527 lines)
**Reference:** `lower.zig` (11,164 lines)

### 7.1 Complete
- All 28 expression types lowered
- All 10+ statement types lowered
- All 59 builtin intrinsics dispatched
- Method call + generic lowering with shape stenciling
- Struct/array/slice/union init lowering
- Cleanup/defer/ARC infrastructure (switch-based dispatch)
- `maybeRegisterScopeDestroy()` — auto-deinit registration
- Auto-deinit generation (emitPendingAutoDeinits)
- Test/bench runner generation
- Module qualification and import resolution
- Global variable init generation
- Async spawn/select/await (native fiber pattern)

### 7.2 Missing

| Function | LOC | Priority | Description |
|----------|-----|----------|-------------|
| `baseHasCleanup()`/`hasDeferCleanups()` | ~15 | HIGH | Cleanup stack validation |
| WasmGC helpers | ~107 | MEDIUM | gcChunkIndex, gcFieldChunks, emitGcDefaultValue, emitGcStructNewExpanded |
| `buildDictArgNames()` | ~30 | LOW | Dict helper argument builder |
| `emitComptimeArray()` | ~41 | LOW | Comptime array value emission |
| Async state machine (Wasm) | ~150 | DEFERRED | WasmGC async state machine generation |
| Full async lowering | ~1,170 | DEFERRED | Zig async ported but not to Cot (v0.4) |

---

## 8. SSA Data & Builder — 95%

**Files:** `ssa.cot` (577 lines), `ssa_builder.cot` (2,130 lines)
**Reference:** `ssa.zig` (1,523 lines combined), `ssa_builder.zig` (2,242 lines)

All 220+ SSA operations, phi handling (Braun et al. 2013), all type conversions,
call handling, memory ops, dict-stenciling. enum_type delegation fixed in getLoadOp/getStoreOp.

---

## 9. SSA Passes — 84%

| Pass | Cot LOC | Zig LOC | Parity | Gaps |
|------|---------|---------|--------|------|
| decompose.cot | 316 | 273 | **100%** | None |
| rewritegeneric.cot | 171 | 154 | **100%** | None |
| layout.cot | 259 | 291 | **95%** | Minor peephole optimizations |
| schedule.cot | 255 | 264 | **95%** | Slight scheduling variation |
| rewritedec.cot | 524 | 654 | **80%** | ~130 LOC defer/control flow edge cases |
| lower_wasm.cot | 371 | 576 | **64%** | ~205 LOC float dispatch, complement/hmul/divmod |

**Priority gaps:** lower_wasm.cot complement/hmul/divmod lowerings, rewritedec control flow edge cases.

---

## 10. Wasm Codegen — 85%

**14+ files, 10,541 total lines**

| Component | LOC | Parity | Notes |
|-----------|-----|--------|-------|
| wasm_gen.cot | 1,898 | **85%** | Switch-based dispatch, ~90 op handlers, emitBinaryI32 |
| code_builder.cot | 699 | **90%** | ~100 emit methods for all Wasm instructions |
| assemble.cot | 806 | **90%** | LEB128, alignment, opcode emission |
| link.cot | 896 | **95%** | Module imports/exports, type dedup |
| preprocess.cot | 551 | **79%** | String/metadata preprocessing |
| constants.cot | 152 | **18%** | Float literal pooling, metadata tables missing |
| driver.cot | 601 | **90%** | Pipeline orchestration |
| prog.cot | 405 | **95%** | Prog/ProgBuilder data structures |
| wasm_types.cot | 599 | **95%** | Wasm type wrappers |
| Runtimes (6 files) | 4,170 | **95%** | mem, print, test, bench, wasi, slice |

**Critical gap:** `constants.cot` at 18% — missing float literal IEEE 754 handling,
constant pool deduplication, metadata table generation. ~500 LOC to reach parity.

---

## 11. Remaining Work (Prioritized)

### Immediate (HIGH — enables dogfooding)
1. Token: add `kw_spawn`, `kw_select` (~5 LOC)
2. Lowerer: `baseHasCleanup()`/`hasDeferCleanups()` (~15 LOC)
3. SSA pass lower_wasm: complement/hmul/divmod lowerings (~100 LOC)
4. SSA pass rewritedec: defer/control flow edge cases (~130 LOC)

### Short-term (MEDIUM — improves completeness)
5. Types: Shape stenciling struct (~80 LOC)
6. Checker: `buildStructTypeWithLayout()` (~42 LOC)
7. Checker: `checkBenchDecl()` (~26 LOC)
8. Constants: float literal pooling + metadata (~500 LOC)

### Deferred (LOW/v0.4)
9. Checker: `evalComptimeValue()` rich union (~300 LOC)
10. Lowerer: WasmGC helpers (~107 LOC)
11. Lowerer: async lowering (~1,170 LOC)

**Total actionable: ~400 LOC** (items 1-7)
**Total with constants: ~900 LOC** (items 1-8)
**Grand total including deferred: ~2,475 LOC** (all items)
