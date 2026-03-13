# Self-Hosted Compiler Parity Audit

**Date:** March 13, 2026 (updated — Phase A-C complete)
**Scope:** File-by-file comparison of `self/` vs `compiler/` (Zig reference)
**Total self-hosted:** 39,615 lines across 30 files, 398 tests pass

---

## 1. Executive Summary

| Subsystem | Files | Lines | Parity | Critical Gaps |
|-----------|-------|-------|--------|---------------|
| Token/Scanner | 2 | 1,217 | **99%** | None |
| AST | 1 | 1,532 | **99%** | None |
| Parser | 1 | 3,158 | **95%** | `unexpectedToken()` error quality |
| Types | 1 | 1,609 | **99%** | None (Shape is intentional addition) |
| Checker | 1 | 5,445 | **~80%** | Comptime mutation, lint, checkBlock RLS |
| IR | 1 | 1,356 | **100%** | None |
| ARC Insertion | 1 | 443 | **100%** | None |
| Lowerer | 1 | 8,447 | **~78%** | WasmGC helpers, stenciling, async (~1,170L) |
| SSA Builder | 1 | 2,120 | **~95%** | enum_type getLoadOp/getStoreOp fixed |
| SSA Data | 1 | 577 | **~95%** | All ops added (complement, hmul, divmod, conversions) |
| SSA Passes | 6 | 1,906 | **~90%** | Float conversions DONE, tail calls stubbed |
| Wasm Codegen | 14 | 10,413 | **~90%** | All ~90 op handlers + ~100 emit methods added |

**Overall: ~90% feature-complete. Remaining gaps: checker comptime mutation, async lowering.**

---

## 2. Token & Scanner — 99% Complete

**Files:** `token.cot` (443 lines), `scanner.cot` (774 lines)
**Reference:** `token.zig`, `scanner.zig`
**Status:** COMPLETE PARITY

All token variants present (operators, keywords, literals, punctuation). All scanner methods implemented (identifiers, numbers with hex/octal/binary, strings with interpolation, operators, doc comments, whitespace/comment skipping).

**No action needed.**

---

## 3. AST — 99% Complete

**File:** `ast.cot` (1,532 lines)
**Reference:** `ast.zig`
**Status:** COMPLETE PARITY

All 13 declaration types, 29 expression types, 8+ statement types present. All 59 builtins mapped.

**No action needed.**

---

## 4. Parser — 95% Complete

**File:** `parser.cot` (3,158 lines, 63 functions)
**Reference:** `parser.zig` (2,340 lines, 52 functions)
**Status:** NEAR-COMPLETE

All critical parsing logic implemented — all declaration, expression, statement, and type constructs.
Cot extracts more functions for modularity (parseReturnStmt, parseBreakStmt, parseFieldDef, etc.).

**Minor gap:** `unexpectedToken()` — Zig has detailed "expected X, got Y" with token name translation.

---

## 5. Types — 99% Complete

**File:** `types.cot` (1,609 lines)
**Reference:** `types.zig`
**Status:** COMPLETE PARITY

All 13 type structures, TypeRegistry with all methods. Shape struct is intentional addition for stenciling.

**No action needed.**

---

## 6. Checker — ~75% Complete (37 Missing Functions)

**File:** `checker.cot` (5,445 lines)
**Reference:** `checker.zig` (~8,500 lines)

### 6.1 Critical Missing

| # | Function | Zig Line | LOC | Description |
|---|----------|----------|-----|-------------|
| 1 | `lookupMethod()` | 648 | ~30 | Method resolution — returns MethodInfo |
| 2 | `resolveMethodCall()` | 1905 | ~25 | Method call type checking |
| 3 | `checkFnDeclWithName()` | 748 | ~50 | Unified function declaration checking |
| 4 | `evalComptimeValue()` | 836 | ~300 | Full comptime value evaluator |
| 5 | `evalComptimeBlock()` | 1124 | ~60 | Comptime block evaluation |
| 6 | `evalComptimeAssign()` | 1182 | ~50 | Comptime assignment |
| 7 | `evalComptimeInlineFor()` | 1250 | ~80 | Inline for in comptime |
| 8 | `checkBlock()` | 2926 | ~40 | Block expr type inference with RLS |
| 9 | `runLintChecks()` | 270 | ~30 | Unused variable/import checks |
| 10 | `defineInFileScope()` | 250 | ~15 | Symbol definition with AST tracking |

### 6.2 Helper Functions Missing

| # | Function | LOC | Description |
|---|----------|-----|-------------|
| 11 | `checkVarDecl()` | ~30 | Variable declaration checking |
| 12 | `checkBenchDecl()` | ~15 | Benchmark declaration |
| 13 | `checkBuiltinLen()` | ~10 | `len()` builtin |
| 14 | `checkBuiltinAppend()` | ~10 | `append()` builtin |
| 15 | `checkContinueExpr()` | ~15 | Continue validation |
| 16 | `resolveTypeByName()` | ~20 | Type name resolution |
| 17 | `registerMethod()` | ~15 | Method registration |
| 18 | `reportRedefined()` | ~10 | Redefinition error |
| 19 | `evalConstFloat()` | ~15 | Float constant eval |
| 20 | `isFloatType()` | ~5 | Float type predicate |
| 21-24 | Small helpers | ~40 | isZeroInitLit, isUndefinedLit, getComptimeIdentName, evalComptimeArrayType |

---

## 7. Lowerer — ~78% Complete

**File:** `lower.cot` (8,447 lines)
**Reference:** `lower.zig` (11,164 lines)

### 7.1 Completed (this session)
- `resolveStructFieldAddr()` — recursive nested field addresses (`&s.f.inner`)
- `lowerDeferredNode()` — deferred cleanup lowering
- `isDivOp()` — division/modulo operator check
- `findLabeledBlock()` — labeled block lookup in stack
- `resolveGenericTypeName()` — generic type name mangling + lookup
- `resolveTypeArgNode()` — type argument resolution with substitution

### 7.2 Previously completed
- `init()` / `deinit()` — decomposed into initLowererScalars/Lists/Maps helpers
- `lowerShortCircuit()` — split into lowerShortCircuitAnd/Or
- Dispatch — all 30 expression types and 13 statement types, 50+ builtins

### 7.3 Still Missing
- `maybeRegisterScopeDestroy()` (~15 LOC) — auto-deinit for structs with free()
- `baseHasCleanup()` / `hasDeferCleanups()` (~20 LOC) — cleanup stack validation
- WasmGC helpers (~75 LOC): gcChunkIndex, gcFieldChunks, emitGcDefaultValue, emitGcStructNewExpanded
- Stenciling (~45 LOC): isTypeParamType, isStencilable, buildDictArgNames
- Async (~1,170 LOC): resolveAsyncPollName + full async lowering (ported in Zig, not yet in Cot)

---

## 8. SSA Data — ~95% Complete

**File:** `ssa.cot` (577 lines)

All ~20 previously missing SsaOp variants have been added:
complement (com8/16/32/64), high multiply (hmul32/32u/64/64u),
divmod (divmod32/64/divmodu32/64), int↔float conversions (all 10),
stack ptr (sp/store_sp), register (store_reg/load_reg).

**No further action needed.**

---

## 9. SSA Builder — ~95% Complete

**File:** `ssa_builder.cot` (2,120 lines)
**Reference:** `ssa_builder.zig` (~2,242 lines)

Previous audit estimated 60% — re-audit reveals 95%. All major features implemented:
- Phi handling (insertPhis, reorderPhis, defvar/lookupVarOutgoing)
- Type conversions (convertLoadLocal, convertStoreLocal, convertFieldLocal, etc.)
- Call handling (convertCall, addCallArg, convertCallIndirect, convertClosureCall)
- Memory ops (emitMove, emitStore, emitOffPtr, emitBinOp, emitSelect3)
- All index/slice/ptr/union conversion variants
- Dict-stenciling (BONUS: newer than Zig reference)

### Fixed (this session)
- `getLoadOp()` / `getStoreOp()`: Added enum_type delegation to backing type
  (prevents 8-byte load on 1-byte enum(u8) fields, which corrupts adjacent fields)

---

## 10. SSA Passes — ~90% Complete

### lower_wasm.cot (371 vs 576 lines)

All float conversion mappings added (cvt*to*f → wasm ops), plus cross-size conversions
(cvt32fto64f → promote, cvt64fto32f → demote). optimizeTailCalls stub added (disabled — causes SIGILL on native).

### Other passes — adequately ported.

**Remaining:** complement/hmul/divmod lowerings (rarely generated), atomics (future).

---

## 11. Wasm Codegen — ~90% Complete

**File:** `wasm_gen.cot` (1,714 lines) — was 1,296
**File:** `code_builder.cot` (699 lines) — was 567
**Reference:** `gen.zig` (1,521 lines)

### Completed (this session)
All ~90 previously missing op handlers added to ssaGenValueOnStack:
- i32 arithmetic (add/sub/mul/div_s/u/rem_s/u) with wrap-operate-extend pattern
- i32 bitwise (and/or/xor/shl/shr_s/shr_u/rotl/rotr/clz/ctz/popcnt)
- i32 comparisons (eqz/eq/ne/lt_s/lt_u/gt_s/gt_u/le_s/le_u/ge_s/ge_u)
- f32 full coverage (add/sub/mul/div/neg/abs/sqrt/ceil/floor/trunc/nearest/min/max/copysign + comparisons)
- f64 extras (abs/sqrt/ceil/floor/trunc/nearest/min/max/copysign)
- All 25+ type conversions (trunc/convert/extend/wrap/promote/demote/reinterpret)
- i32/i64 memory (load8/16/32 signed/unsigned, store8/16/32)
- f32/f64 memory (load/store)
- Control (call_indirect, select, nop)
- Lowered (zero via memory.fill, nil_check)
- Added emitBinaryI32() helper with wrap-extend pattern matching Zig gen.zig
- ~100 new emit methods in code_builder.cot

**Note:** i32 arithmetic/f32 ops are NOT handled in Zig gen.zig (fall through to else debug log).
Self-hosted version handles them proactively for forward compatibility.

### Still Missing
- Some edge-case ops that gen.zig also doesn't handle (extremely rare code paths)

---

## 12. Implementation Roadmap

### Phase A: SSA Foundation — COMPLETE
- A1: All ~20 missing SsaOp variants added to ssa.cot
- A2: All float conversion mappings added to lower_wasm.cot

### Phase B: Wasm Codegen — COMPLETE
- ~90 op handlers added to wasm_gen.cot
- ~100 emit methods added to code_builder.cot
- emitBinaryI32 helper with wrap-extend pattern

### Phase C: Lowerer — COMPLETE
- resolveStructFieldAddr, lowerDeferredNode, isDivOp, findLabeledBlock
- resolveGenericTypeName, resolveTypeArgNode
- (init/deinit, shortCircuit were already done)

### Phase D: Checker (~300 LOC) — IN PROGRESS
- D1: lookupMethod() — EXISTS (delegates to types.lookupMethod)
- D2: resolveMethodCall() — EXISTS as resolveMethodFromFieldAccess
- D3: checkFnDeclWithName() — EXISTS as checkFnDeclBody (~95% parity)
- D4: Comptime mutation (evalComptimeAssign, evalComptimeInlineFor) — MISSING
- D5: checkBlock() with RLS — basic version exists, RLS/labels missing
- D6: runLintChecks() — MISSING (nice-to-have)

### Phase E: SSA Builder — MOSTLY COMPLETE
- Re-audit reveals ~95% parity (was incorrectly estimated at 60%)
- Fixed enum_type delegation in getLoadOp/getStoreOp
- All phi handling, type conversions, call handling, memory ops implemented

### Phase F: WasmGC & Stenciling (~200 LOC) — LOW PRIORITY

### Remaining Work
- Checker: comptime mutation (evalComptimeAssign/InlineFor ~130 LOC)
- Checker: runLintChecks (~30 LOC)
- Checker: checkBlock RLS/labeled block improvements (~40 LOC)
- Lowerer: cleanup helpers (maybeRegisterScopeDestroy ~15 LOC)
- Lowerer: WasmGC helpers (~75 LOC)
- Lowerer: async lowering (~1,170 LOC — largest gap, deferred)

**Total remaining: ~460 LOC** (excluding async, which is ~1,170 LOC)
**Current: 39,615 lines — ~90% parity**
