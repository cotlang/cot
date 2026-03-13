# Self-Hosted Compiler Parity Audit

**Date:** March 13, 2026 (full re-audit)
**Scope:** File-by-file comparison of `self/` vs `compiler/` (Zig reference)
**Total self-hosted:** 40,231 lines across 36 files, 398 tests pass

---

## 1. Executive Summary

| Subsystem | Files | Lines | Parity | Critical Gaps |
|-----------|-------|-------|--------|---------------|
| Token/Scanner | 2 | 1,217 | **100%** | None (`kw_spawn`, `kw_select` added) |
| AST | 1 | 1,532 | **95%** | None (SelectExpr already present) |
| Parser | 1 | 3,158 | **95%** | Minor: less detailed error messages |
| Types | 1 | 1,609 | **95%** | Shape stenciling complete |
| Checker | 1 | 5,546 | **86%** | `evalComptimeValue` rich union |
| IR | 1 | 1,356 | **100%** | None |
| ARC Insertion | 1 | 443 | **100%** | None |
| Lowerer | 1 | 8,750+ | **80%** | WasmGC helpers, async (~1,170L) |
| SSA Builder | 1 | 2,130 | **95%** | None |
| SSA Data | 1 | 577 | **95%** | None |
| SSA Passes | 6 | 1,896 | **92%** | lower_wasm ~90% (LOC gap is explicit null arms), rewritedec 100% |
| Wasm Codegen | 16 | 10,541 | **88%** | wasm_types.cot vs constants.zig ~72%, wasm_gen edge cases |
| Source/Errors | 2 | 860 | **100%** | None |
| Main/CLI | 1 | 788 | **100%** | None |

**Overall: ~90% feature-complete. 40,231 lines across 36 files.**

**Remaining gaps by priority:**
1. **HIGH**: Token keywords (5 LOC), lower_wasm pass gaps (~205 LOC), rewritedec gaps (~130 LOC)
2. **MEDIUM**: Shape stenciling (~80 LOC), buildStructTypeWithLayout (~42 LOC), constants.cot (~500 LOC)
3. **LOW**: evalComptimeValue (~300 LOC), WasmGC helpers (~107 LOC)
4. **DEFERRED**: async lowering (~1,170 LOC — v0.4)

---

## 2. Token & Scanner — 100%

**Files:** `token.cot` (445 lines), `scanner.cot` (774 lines)
**Reference:** `token.zig` (333 lines), `scanner.zig` (527 lines)

All token variants and scanner methods present. Scanner fully functional for all lexing.
`kw_spawn` and `kw_select` added. No remaining gaps.

---

## 3. AST — 95%

**File:** `ast.cot` (1,532 lines)
**Reference:** `ast.zig` (696 lines)

All 13 declaration types, 29+ expression types, 8+ statement types present.
All 59 builtins mapped. SelectExpr/SelectCase already present.
Cot is 2x lines due to explicit struct definitions and inline tests.

---

## 4. Parser — 95%

**File:** `parser.cot` (3,158 lines, 66 functions)
**Reference:** `parser.zig` (2,340 lines, 55 functions)

All critical parsing logic implemented. Cot has 11 extra helper methods for
safe mode (`@safe` colon syntax, field shorthand) and concurrency (select/spawn parsing).
Spawn/select tokens now defined and parser integration complete.

**Gap:** Minor: `unexpectedToken()` less detailed than Zig version.

---

## 5. Types — 95%

**File:** `types.cot` (1,609 lines)
**Reference:** `types.zig` (941 lines)

All 13 type structures, TypeRegistry with all methods, 10 type constructors,
all type queries (sizeOf, alignOf, isPointer, isArray, etc.).
Shape stenciling complete (Shape, ArcKind, RegClass, fromType, eql, key).
3 tests cover basic types, pointers/slices, and key format.

---

## 6. Checker — 86%

**File:** `checker.cot` (5,546 lines)
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
- **checkStmtsWithReachability** — full type pre-pass for block-scoped decls + reachability

### 6.2 Missing

| Function | LOC | Priority | Description |
|----------|-----|----------|-------------|
| `evalComptimeValue()` | ~300 | LOW | Rich comptime value union (array/struct construction) |
| Typo suggestions | ~60 | LOW | editDistSuggest, errWithSuggestion |

**Previously missing, now complete:**
- `buildStructTypeWithLayout()` — correct packed/extern/auto field offset calculation
- `checkBenchDecl()` — handled by checkTestDecl (supports both test_decl and bench_decl)
- `checkStmtsWithReachability()` — type decl pre-pass + struct method body checking

---

## 7. Lowerer — 80%

**File:** `lower.cot` (8,750+ lines)
**Reference:** `lower.zig` (11,164 lines)

### 7.1 Complete
- All 28 expression types lowered
- All 10+ statement types lowered
- All 59 builtin intrinsics dispatched
- Method call + generic lowering with shape stenciling
- **Shape stenciling in lowerGenericFnInstanceInner** — Tier 1 (shape alias) + Tier 2 (dict-stenciled)
- **buildDictArgNames** — maps concrete generic names to comma-separated helper fn names
- **Dict dispatch in binary ops** — indirect call through fn-ptr params for type-param binary ops
- **Dict dispatch in method calls** — indirect call through fn-ptr params for type-param method calls
- **dictOpName fixed** — correctly maps Token values (not BinaryOp), matching Zig reference
- **generateDictHelpers** — takes type_args list (multi-type-param support), not single type
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
| WasmGC helpers | ~107 | MEDIUM | gcChunkIndex, gcFieldChunks, emitGcDefaultValue, emitGcStructNewExpanded |
| `emitComptimeArray()` | ~41 | LOW | Comptime array value emission |
| `resolveComptimeFieldAccess()` | ~27 | LOW | Comptime field access (.name, .fields, .len) |
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
| rewritedec.cot | 524 | 654 | **100%** | All patterns complete (Cot uses combined extractStringComponent helper) |
| lower_wasm.cot | 371 | 576 | **~90%** | LOC gap is explicit `=> null` arms in Zig; all functional mappings present |

**Note:** rewritedec.cot was re-audited and found to be functionally complete.
lower_wasm.cot LOC gap is Zig's explicit null arms for ops that don't need lowering —
Cot's `else => SsaOp.invalid` achieves the same result. Cot's `isSliceType()` is
actually MORE correct (includes F64/F32 checks missing from Zig).

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
| constants.cot | 152 | **100%** | LEB128/alignment/opcode encoding utilities (NOT Zig's constants.zig) |
| driver.cot | 601 | **90%** | Pipeline orchestration |
| prog.cot | 405 | **95%** | Prog/ProgBuilder data structures |
| wasm_types.cot | 599 | **95%** | Wasm type wrappers |
| Runtimes (6 files) | 4,170 | **95%** | mem, print, test, bench, wasi, slice |

**Note:** `constants.cot` is LEB128/alignment encoding (parity with assemble.zig utilities).
Zig's `constants.zig` (opcodes/registers) maps to Cot's `wasm_types.cot` (599 vs 829 lines, ~72%).
The LOC gap is unused opcode variants not yet needed by the self-hosted compiler.

---

## 11. Remaining Work (Prioritized)

### Immediate (HIGH — enables dogfooding)
All HIGH items completed:
- Token: `kw_spawn`, `kw_select` added
- Lowerer: `baseHasCleanup()`/`hasDeferCleanups()` added
- Lowerer: `maybeRegisterScopeDestroy()` added
- Checker: `buildStructTypeWithLayout()` fixed (correct packed/extern/auto layout)
- Checker: `evalConstFloat()` added
- SSA passes: rewritedec confirmed 100% complete
- **Types: Shape stenciling struct** — COMPLETE (Shape, ArcKind, RegClass, fromType, eql, key)
- **Lowerer: shape stenciling in lowerGenericFnInstanceInner** — COMPLETE (Tier 1+2)
- **Lowerer: buildDictArgNames** — COMPLETE (comma-separated helper names)
- **Lowerer: dict dispatch in binary ops and method calls** — COMPLETE
- **Lowerer: dictOpName fixed** — uses Token values (was incorrectly using BinaryOp)
- **SSA builder: dict_arg_names parsing** — updated to comma-separated format

### Short-term (MEDIUM — improves completeness)
1. wasm_types.cot: additional opcode variants (~230 LOC, as-needed)
2. Lowerer: `emitComptimeArray()` (~41 LOC) + `resolveComptimeFieldAccess()` (~27 LOC)

### Deferred (LOW/v0.4)
3. Checker: `evalComptimeValue()` rich union (~300 LOC)
4. Lowerer: WasmGC helpers (~107 LOC)
5. Lowerer: async lowering (~1,170 LOC)

**Total remaining actionable: ~298 LOC** (items 1-2)
**Grand total including deferred: ~1,875 LOC** (all items)
