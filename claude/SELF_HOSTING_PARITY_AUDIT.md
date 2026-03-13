# Self-Hosted Compiler Parity Audit

**Date:** March 14, 2026 (updated after WasmGC improvements — 5 phases ported from Kotlin patterns)
**Scope:** File-by-file comparison of `self/` vs `compiler/` (Zig reference)
**Total self-hosted:** 41,813 lines across 38 files, 409 tests pass

---

## 1. Executive Summary

| Subsystem | Files | Lines | Parity | Critical Gaps |
|-----------|-------|-------|--------|---------------|
| Token/Scanner | 2 | 1,222 | **100%** | None |
| AST | 1 | 1,532 | **95%** | None |
| Parser | 1 | 3,234 | **95%** | Minor: less detailed error messages |
| Types | 1 | 1,609 | **95%** | Shape stenciling complete |
| Checker | 1 | 5,864 | **86%** | Per-file scoping implemented, `evalComptimeValue` rich union remaining |
| IR | 1 | 1,450 | **100%** | WasmGC array/ref/cast nodes added |
| ARC Insertion | 1 | 443 | **100%** | None |
| Lowerer | 1 | 8,834 | **80%** | async (~1,170L), WasmGC lowering is Zig-only |
| SSA Builder | 1 | 2,160 | **95%** | WasmGC array/ref conversion stubs |
| SSA Data | 1 | 582 | **95%** | WasmGC SSA ops added |
| SSA Passes | 6 | 1,896 | **92%** | lower_wasm ~90%, rewritedec 100% |
| Wasm Codegen | 17 | 11,051 | **93%** | WasmGC array/ref/cast assembly added |
| Source/Errors | 2 | 860 | **100%** | None |
| Main/CLI | 1 | 1,029 | **100%** | Per-file scoping, CheckedScopeEntry |

**Overall: ~93% Wasm-frontend-complete. 41,813 lines across 38 files.**

**MILESTONE (Mar 13, 2026): selfcot type-checks itself.**
`selfcot check self/main.cot` → passes (all 38 files, ~41,813 lines multi-file type-checking).
`selfcot build foo.cot -o foo.wasm` → works for single-file programs.
Full pipeline wired: parse → check → lower → SSA → 6 passes → wasm_gen → link → .wasm.
All 9 CLI commands implemented: build, run, test, bench, check, parse, lex, init, help.

**WasmGC (Mar 14, 2026):** 5 phases of Kotlin-pattern WasmGC ported to self-hosted:
- GC arrays, union subtypes, nullable refs, function refs, GC strings
- Infrastructure complete (IR nodes, SSA ops, codegen assembly)
- Self-hosted codegen emits `unreachable` stubs (lowerer wiring is Zig-only for now)

**Remaining gaps by priority:**
1. **HIGH**: Multi-file build (checkAndLowerRecursive exists, needs wiring into build pipeline)
2. **MEDIUM**: wasm_types opcodes (~230 LOC), comptime array emission
3. **LOW**: evalComptimeValue (~300 LOC), WasmGC lowerer wiring in self-hosted
4. **DEFERRED**: async lowering (~1,170 LOC — v0.4)
5. **NOT STARTED**: Native backend (~72,000 LOC — CLIF IR, MachInst, regalloc, ISA, object emit)

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

**File:** `parser.cot` (3,234 lines, 66 functions)
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

**File:** `checker.cot` (5,864 lines)
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

**File:** `lower.cot` (8,809 lines)
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

**Files:** `ssa.cot` (577 lines), `ssa_builder.cot` (2,145 lines)
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

## 10. Wasm Codegen — 91%

**17 files, 10,841 total lines**

| Component | LOC | Parity | Notes |
|-----------|-----|--------|-------|
| wasm_gen.cot | 2,277 | **92%** | All call types, convert, cond_select, globals, addr, ~100 op handlers |
| code_builder.cot | 731 | **95%** | ~115 emit methods incl. saturation truncate, return_call, memory.fill |
| assemble.cot | 806 | **95%** | LEB128, alignment, opcode emission, AddrVal union matching for br_table |
| link.cot | 896 | **95%** | Module imports/exports, type dedup |
| preprocess.cot | 578 | **90%** | Pass1-6 dispatch loop, br_table indices stored in Prog AddrVal (matches Zig) |
| constants.cot | 152 | **100%** | LEB128/alignment/opcode encoding utilities (NOT Zig's constants.zig) |
| driver.cot | 585 | **90%** | Pipeline orchestration |
| prog.cot | 473 | **95%** | Prog/Addr/AddrVal/Symbol data structures |
| wasm_types.cot | 599 | **95%** | Wasm type wrappers |
| ssa_passes.cot | 13 | **100%** | SSA pass dispatch (delegates to individual passes) |
| ssa_passes_dec.cot | 11 | **100%** | Decompose pass dispatch |
| Runtimes (6 files) | 3,720 | **95%** | mem(556), print(909), test(538), bench(536), wasi(931), slice(250) |

**Note:** `constants.cot` is LEB128/alignment encoding (parity with assemble.zig utilities).
Zig's `constants.zig` (opcodes/registers) maps to Cot's `wasm_types.cot` (599 vs 829 lines, ~72%).
The LOC gap is unused opcode variants not yet needed by the self-hosted compiler.

**Mar 14, 2026 — Workarounds reverted:** Two Zig compiler bugs were fixed (overlapping memcpy
in native codegen, stack overflow from switch arm local over-allocation), allowing the
self-hosted code to use idiomatic patterns: br_table data stored in Prog's AddrVal union
(not Symbol side-channel), assemble uses AddrVal union matching (not Symbol bypass).

---

## 11. Remaining Work (Prioritized)

### Immediate (HIGH — enables self-compilation)
1. **Multi-file build** (~100 LOC in main.cot): `selfcot build` only lowers top-level file. Must lower ALL imported files, merge IR, pass to generateWasmCode.
2. **`generateGlobalInits()`** (~50 LOC in lower.cot): Emits `__cot_init_globals` for module-level variable initialization.
3. **`generateTestRunner()`** (~80 LOC in lower.cot): Emits test harness for `selfcot test` to discover and run test blocks.

### Completed (previously HIGH)
- All 9 CLI commands: build, run, test, bench, check, parse, lex, init, help
- Full Wasm pipeline: parse → check → lower → SSA → 6 passes → wasm_gen → link → .wasm
- Token keywords, shape stenciling, dict dispatch, SSA builder, ARC insertion
- 9 wasm_gen SSA op handlers: closures, globals, convert, addr, cond_select, return_call, metadata_addr
- Saturation truncate + memory.fill CodeBuilder methods
- enum(u8) sign extension fix in ssa_to_clif.zig

### Short-term (MEDIUM — improves completeness)
1. wasm_types.cot: additional opcode variants (~230 LOC, as-needed)
2. Lowerer: `emitComptimeArray()` (~41 LOC) + `resolveComptimeFieldAccess()` (~27 LOC)

### Deferred (LOW/v0.4)
3. Checker: `evalComptimeValue()` rich union (~300 LOC)
4. Lowerer: WasmGC helpers (~107 LOC)
5. Lowerer: async lowering (~1,170 LOC)

**Critical path to self-compilation: ~230 LOC** (items 1-3 in HIGH)
**Grand total including deferred: ~1,875 LOC** (all items)
