# Cot 0.3 Rebuild Plan

---

## ⚠️ PIVOT NOTICE (January 2026)

**The project has pivoted from native codegen to Wasm-first architecture.**

- Rounds 1-4 (frontend, SSA, object files, pipeline) are **COMPLETE**
- Round 5 (native codegen) is **SKIPPED** - replaced with Wasm backend
- Native codegen will become the **AOT compiler**, not the main Cot compiler

See `../bootstrap-0.2/DESIGN.md` for the full Wasm architecture specification.

See `README.md` and `CLAUDE.md` for current project status.

---

## Historical Context (Rounds 1-4)

The approach below was used for the completed refactoring work.

## Approach

**This is a complete rebuild, not a refactor.**

For each file:
1. Read original from bootstrap-0.2
2. Identify essential vs verbose code
3. Rewrite from scratch - concise, elegant
4. Keep/expand tests (tests can grow - target 10x source)
5. Verify: `zig test src/path/file.zig`
6. Record: source/tests lines separately (source reduction is primary metric)

## Refactoring Strategy

**Codegen files saved for last.** The arm64.zig and amd64.zig files are:
- The largest (~7,535 lines combined)
- The most duplicated (~2,500 lines of shared infrastructure)
- The most critical (if codegen breaks, nothing works)

**Execution order:**
1. Complete all supporting infrastructure first (abi, debug, compile, passes, object files, pipeline)
2. Clean up arch-specific assemblers (arm64/asm.zig, amd64/asm.zig, amd64/regs.zig)
3. Then tackle main codegen with proper `CodeGenBase` abstraction design
4. Run full E2E tests after each phase to catch regressions early

## Build Order

Files must be built in dependency order. A file can only import files that already exist.

---

## Phase 1: Foundation

### 1. build.zig
- **Original**: 118 source ✅
- **New**: 32 source (73% reduction)
- **Method**: Remove extra test targets, keep core build/run/test

### 2. src/core/types.zig
- **Original**: 409 source / 139 tests ✅
- **New**: 159 source / 106 tests (61% source reduction, 9 tests)
- **Method**: Remove verbose docs, inline trivial helpers, keep all type definitions

### 3. src/core/errors.zig
- **Original**: 259 source / 32 tests ✅
- **New**: 164 source / 31 tests (37% source reduction, 3 tests)
- **Method**: Simplify error types, keep CompileError and VerifyError

### 4. src/core/target.zig
- **Original**: 110 source / 22 tests ✅
- **New**: 82 source / 21 tests (25% source reduction, 4 tests)
- **Method**: Simplify target enum, keep parse/name functions

### 5. src/core/testing.zig
- **Original**: 139 source / 31 tests ✅
- **New**: 94 source / 27 tests (32% source reduction, 2 tests)
- **Method**: Keep CountingAllocator, simplify interface

---

## Phase 2: Frontend Foundation

### 6. src/frontend/token.zig
- **Original**: 401 source / 64 tests ✅
- **New**: 225 source / 64 tests (44% source reduction, 8 tests)
- **Method**: Compact token enum, simplify keyword table

### 7. src/frontend/source.zig
- **Original**: 245 source / 91 tests ✅
- **New**: 157 source / 69 tests (36% source reduction, 9 tests)
- **Method**: Simplify Pos/Span, keep line/col calculation

### 8. src/frontend/errors.zig
- **Original**: 263 source / 83 tests ✅
- **New**: 154 source / 69 tests (41% source reduction, 7 tests)
- **Method**: Simplify ErrorReporter, keep error formatting

### 9. src/frontend/scanner.zig
- **Original**: 560 source / 193 tests ✅
- **New**: 303 source / 158 tests (46% source reduction, 11 tests)
- **Method**: Simplify state machine, compact character handling

### 10. src/frontend/ast.zig
- **Original**: 676 source / 88 tests ✅
- **New**: 278 source / 54 tests (59% source reduction, 9 tests)
- **Method**: Compact node definitions, simplify accessors

### 11. src/frontend/parser.zig
- **Original**: 1,689 source / 125 tests ✅
- **New**: 797 source / 84 tests (53% source reduction, 11 tests)
- **Method**: Simplify recursive descent, reduce error recovery verbosity

---

## Phase 3: Type System

### 12. src/frontend/types.zig
- **Original**: 802 source / 95 tests ✅
- **New**: 341 source / 53 tests (57% source reduction, 7 tests)
- **Method**: Simplify TypeRegistry, compact type creation

### 13. src/frontend/checker.zig
- **Original**: 2,100 source / 68 tests ✅
- **New**: 893 source / 43 tests (57% source reduction, 5 tests)
- **Method**: Simplify scope handling, compact type checking

---

## Phase 4: IR

### 14. src/frontend/ir.zig
- **Original**: 1,594 source / 158 tests ✅
- **New**: 459 source / 76 tests (71% source reduction, 7 tests)
- **Method**: Compact IR node definitions, simplify builders

### 15. src/frontend/lower.zig
- **Original**: 3,488 lines
- **Target**: ~1,800 lines
- **Method**: Extract common patterns, table-driven where possible
- **Keep**: All lowering logic
- **Remove**: Repetitive AST→IR patterns, verbose comments

### 16. src/frontend/ssa_builder.zig
- **Original**: 3,044 source ✅
- **New**: 1,087 source / 78 tests (64% source reduction, 3 tests)
- **Method**: Simplify variable tracking, compact phi handling
- **Keep**: FwdRef pattern, all SSA construction
- **Remove**: Verbose iteration, redundant null checks

---

## Phase 5: SSA Infrastructure

### 17. src/ssa/value.zig
- **Original**: 673 lines
- **Target**: ~400 lines
- **Method**: Compact Value struct, remove dead functions
- **Keep**: Value definition, arg handling, aux fields
- **Remove**: addArg3, addArgs (unused)

### 18. src/ssa/block.zig
- **Original**: 449 lines
- **Target**: ~280 lines
- **Method**: Simplify block handling, remove dead code
- **Keep**: Block struct, edge management
- **Remove**: addControl (unused)

### 19. src/ssa/func.zig
- **Original**: 650 lines
- **Target**: ~400 lines
- **Method**: Simplify function representation
- **Keep**: Func struct, block management
- **Remove**: Verbose iteration helpers

### 20. src/ssa/op.zig
- **Original**: 1,569 lines
- **Target**: ~1,000 lines
- **Method**: Compact op enum, simplify OpInfo
- **Keep**: All operations needed
- **Remove**: Unused ops, verbose comments

### 21. src/ssa/dom.zig
- **Original**: 296 source / 100 tests ✅
- **New**: 184 source / 71 tests (38% source reduction, 3 tests)
- **Method**: Simplify dominator computation, iterative stack-based DFS
- **Keep**: DomTree, computeDominators, dominanceFrontier

### 22. src/ssa/liveness.zig
- **Original**: 640 source / 308 tests ✅
- **New**: 295 source / 175 tests (54% source reduction, 8 tests)
- **Method**: Simplify LiveMap, compact fixed-point iteration
- **Keep**: LivenessResult, computeLiveness, nextCall tracking

### 23. src/ssa/regalloc.zig
- **Original**: 1,472 source / 1 test ✅
- **New**: 811 source / 101 tests (45% source reduction, 7 tests)
- **Method**: Simplified state tracking, compact allocation, extracted AMD64 div/mod helper
- **Keep**: Linear scan algorithm, spill handling, Go's 3-phase architecture

### 24. src/ssa/stackalloc.zig
- **Original**: 493 source / 1 test ✅
- **New**: 278 source / 79 tests (44% source reduction, 5 tests)
- **Method**: Simplified frame layout, compact interference graph building
- **Keep**: Stack allocation algorithm, interference-based slot reuse

### 25. src/ssa/abi.zig
- **Original**: 704 source / 2 tests ✅
- **New**: 319 source / 61 tests (55% source reduction, 8 tests)
- **Method**: Removed dead functions, verbose comments, debug dump method
- **Keep**: ARM64/AMD64 ABI logic, analyzeFunc, pre-built str_concat_abi

### 26. src/ssa/debug.zig
- **Original**: 645 source / 3 tests ✅
- **New**: 273 source / 79 tests (58% source reduction, 5 tests)
- **Method**: Removed HTML output, compacted dumpValue, simplified PhaseSnapshot
- **Keep**: Text output, DOT output, verification, phase comparison

### 27. src/ssa/test_helpers.zig
- **Original**: 200 source / 60 tests ✅
- **New**: 120 source / 44 tests (40% source reduction, 3 tests)
- **Method**: Compact TestFuncBuilder, inline validation
- **Keep**: TestFuncBuilder, DiamondCFG, LinearCFG, validation

### 28. src/ssa/compile.zig
- **Original**: 547 lines (mostly stubs!)
- **Target**: ~150 lines
- **Method**: Remove stubs, wire to real passes
- **Keep**: Pass infrastructure, compile() function
- **Remove**: All stub implementations (they do nothing)
- **Wire up**: Call real passes from ssa/passes/

---

## Phase 6: SSA Passes

### 29. src/ssa/passes/schedule.zig
- **Original**: 193 lines
- **Target**: ~120 lines
- **Method**: Keep algorithm, simplify
- **Keep**: Scheduling logic
- **Remove**: Verbose comments

### 30. src/ssa/passes/decompose.zig
- **Original**: 477 lines
- **Target**: ~300 lines
- **Method**: Simplify decomposition
- **Keep**: Large value handling
- **Remove**: Verbose iteration

### 31. src/ssa/passes/expand_calls.zig
- **Original**: 662 lines
- **Target**: ~400 lines
- **Method**: Simplify call expansion
- **Keep**: ABI handling for calls
- **Remove**: Verbose comments

### 32. src/ssa/passes/lower.zig
- **Original**: 322 lines
- **Target**: ~200 lines
- **Method**: Simplify lowering
- **Keep**: Generic→arch conversion
- **Wire up**: Actually USE this in pipeline

---

## Phase 7: Code Generation

### 33. src/codegen/generic.zig
- **Original**: 308 lines
- **Decision**: Delete or keep as reference?
- If keep: ~150 lines
- **Method**: It's only used in tests, not production

### 34. src/codegen/arm64.zig + src/codegen/amd64.zig
- **Original**: 3,589 + 3,946 = 7,535 lines
- **Target**: ~4,000 lines total (after consolidation)
- **Method**:
  1. Create CodeGenBase with shared fields/methods
  2. ARM64CodeGen and AMD64CodeGen extend base
  3. Only arch-specific emission differs
- **Keep**: All instruction emission logic
- **Remove**: Duplicated infrastructure (~2,500 lines)
- **Add**: Many more tests (currently 1 each!)

### 35. src/arm64/asm.zig
- **Original**: 989 lines
- **Target**: ~600 lines
- **Method**: Compact instruction encoding
- **Keep**: All encoders (already good test coverage: 29 tests)
- **Remove**: Verbose comments

### 36. src/amd64/asm.zig
- **Original**: 1,628 lines
- **Target**: ~900 lines
- **Method**: Compact instruction encoding
- **Keep**: All encoders
- **Add**: More tests (only 12 currently)

### 37. src/amd64/regs.zig
- **Original**: 218 lines
- **Target**: ~140 lines
- **Method**: Compact register definitions
- **Keep**: All register mappings

---

## Phase 8: Object Files

### 38. src/obj/macho.zig
- **Original**: 1,175 lines
- **Target**: ~700 lines
- **Method**: Simplify Mach-O generation
- **Keep**: All section handling, symbols, relocations
- **Remove**: Verbose comments

### 39. src/obj/elf.zig
- **Original**: 784 lines
- **Target**: ~500 lines
- **Method**: Simplify ELF generation
- **Keep**: All section handling
- **Remove**: Verbose comments

### 40. src/dwarf.zig
- **Original**: 475 lines
- **Target**: ~300 lines
- **Method**: Simplify DWARF generation
- **Keep**: Debug line info
- **Remove**: Unused DWARF features

---

## Phase 9: Pipeline

### 41. src/pipeline_debug.zig
- **Original**: 437 lines
- **Target**: ~250 lines
- **Method**: Simplify debug output
- **Keep**: COT_DEBUG parsing, phase logging
- **Remove**: Verbose formatting

### 42. src/driver.zig
- **Original**: 707 lines
- **Target**: ~350 lines
- **Method**:
  1. Single generateCode() instead of ARM64/AMD64 duplication
  2. Use compile.zig pass infrastructure
- **Keep**: File compilation, multi-file support
- **Remove**: Duplicated pipeline code (~200 lines)

### 43. src/main.zig
- **Original**: 526 lines
- **Target**: ~250 lines
- **Method**: Simplify CLI, reduce re-exports
- **Keep**: Arg parsing, module structure
- **Remove**: Verbose help text, redundant exports

---

## Summary

| Phase | Original | Target | Reduction |
|-------|----------|--------|-----------|
| Foundation (1-5) | 1,261 | ~627 | 50% |
| Frontend Foundation (6-11) | 4,478 | ~2,440 | 45% |
| Type System (12-13) | 3,063 | ~1,700 | 44% |
| IR (14-16) | 8,283 | ~4,300 | 48% |
| SSA Infra (17-28) | 7,903 | ~4,680 | 41% |
| SSA Passes (29-32) | 1,654 | ~1,020 | 38% |
| Codegen (33-37) | 10,668 | ~5,790 | 46% |
| Object Files (38-40) | 2,434 | ~1,500 | 38% |
| Pipeline (41-43) | 1,670 | ~850 | 49% |
| **Total** | **42,304** | **~22,907** | **46%** |

## Progress Tracking (SOURCE LINES ONLY)

| File | Original | New | Reduction |
|------|----------|-----|-----------|
| build.zig | 118 | 32 | 73% |
| core/types.zig | 409 | 159 | 61% |
| core/errors.zig | 259 | 164 | 37% |
| core/target.zig | 110 | 82 | 25% |
| core/testing.zig | 139 | 94 | 32% |
| frontend/token.zig | 401 | 225 | 44% |
| frontend/source.zig | 245 | 157 | 36% |
| frontend/errors.zig | 263 | 154 | 41% |
| frontend/scanner.zig | 560 | 303 | 46% |
| frontend/ast.zig | 676 | 278 | 59% |
| frontend/parser.zig | 1,689 | 797 | 53% |
| frontend/types.zig | 802 | 341 | 57% |
| frontend/checker.zig | 2,100 | 893 | 57% |
| frontend/ir.zig | 1,594 | 459 | 71% |
| frontend/lower.zig | 3,488 | 1,864 | 47% |
| ssa/op.zig | 1,569 | 366 | 77% |
| ssa/value.zig | 673 | 258 | 62% |
| ssa/block.zig | 449 | 228 | 49% |
| ssa/func.zig | 650 | 257 | 60% |
| frontend/ssa_builder.zig | 3,044 | 1,087 | 64% |
| ssa/dom.zig | 296 | 184 | 38% |
| ssa/liveness.zig | 640 | 295 | 54% |
| ssa/test_helpers.zig | 200 | 120 | 40% |
| ssa/regalloc.zig | 1,472 | 811 | 45% |
| ssa/stackalloc.zig | 493 | 278 | 44% |
| ssa/abi.zig | 704 | 319 | 55% |
| ssa/debug.zig | 645 | 273 | 58% |
| pipeline_debug.zig | 437 | 138 | 68% |
| ssa/compile.zig | 547 | 218 | 60% |
| ssa/passes/schedule.zig | 193 | 234 | -21%* |
| ssa/passes/decompose.zig | 477 | 285 | 40% |
| ssa/passes/expand_calls.zig | 662 | 256 | 61% |
| dwarf.zig | 475 | 363 | 24% |
| obj/macho.zig | 1,175 | 548 | 53% |
| obj/elf.zig | 784 | 529 | 33% |
| driver.zig | 707 | 302 | 57% |
| main.zig | 526 | 219 | 58% |
| **Total (37 files)** | **29,671** | **13,570** | **54%** |

*schedule.zig grew slightly due to added tests (4 tests vs 0)

Tests: 248 total tests across all modules

### Next (8 files remaining)

**Round 1: SSA Infrastructure** ✅ COMPLETE
- [x] src/ssa/abi.zig ✅
- [x] src/ssa/debug.zig ✅
- [x] src/pipeline_debug.zig ✅
- [x] src/ssa/compile.zig ✅

**Round 2: SSA Passes** ✅ COMPLETE (3 of 4 - lower.zig skipped, handled by codegen)
- [x] src/ssa/passes/schedule.zig ✅
- [x] src/ssa/passes/decompose.zig ✅
- [x] src/ssa/passes/expand_calls.zig ✅
- [ ] src/ssa/passes/lower.zig (~322 lines) - may skip, lowering done in codegen

**Round 3: Object Files** ✅ COMPLETE
- [x] src/dwarf.zig ✅ (475 → 363, 24% reduction, 5 new tests)
- [x] src/obj/macho.zig ✅ (1,175 → 548, 53% reduction, removed duplicate DWARF)
- [x] src/obj/elf.zig ✅ (784 → 529, 33% reduction, 1 new test)

**Round 4: Pipeline** ✅ COMPLETE
- [x] src/driver.zig ✅ (707 → 302, 57% reduction, unified generateCode)
- [x] src/main.zig ✅ (526 → 219, 58% reduction, flat exports)

**Round 5: Wasm Backend** ⚠️ PIVOTED
- [ ] src/codegen/wasm.zig - NEW: IR → Wasm emission
- Native codegen files moved to AOT phase (see DESIGN.md)

~~**Original Round 5: Native Codegen (SKIPPED - becomes AOT)**~~
~~- src/codegen/generic.zig, arm64/asm.zig, amd64/asm.zig, amd64/regs.zig~~
~~- src/codegen/arm64.zig + amd64.zig~~
~~These will be used in the AOT compiler (Wasm → Native), not the main Cot compiler.~~

### Test Coverage Notes
- **34 E2E tests** (e2e_test.zig) verify full pipeline: source → parse → check → lower → IR → SSA
- E2E tests cover: arithmetic/comparison/bitwise/unary ops, if/else, while loops, logical ops, function calls, recursion, structs, enums, pointers, arrays, fibonacci, gcd, div_rem, nested loops
- Error detection tests verify type errors, undefined variables/functions are caught
- lower.zig has 20 additional IR lowering tests
- ssa_builder.zig has 3 tests for builder initialization, block transitions, variable tracking
- dom.zig has 3 tests for dominator tree computation
- regalloc.zig has 7 tests for register allocation, diamond/linear CFG, register masks, ValState/RegState operations
- stackalloc.zig has 5 tests for stack allocation, locals, use block tracking, frame alignment
- abi.zig has 8 tests for ARM64/AMD64 register masks, ABI structures, param assignment, accessors
- debug.zig has 5 tests for text/dot format, verification, PhaseSnapshot comparison
- pipeline_debug.zig has 4 tests for debug phase parsing, all-enabled, empty string
- compile.zig has 3 tests for CompileResult, pass ordering, prepareForRegalloc
- schedule.zig has 4 tests for Score ordering, getScore, empty function, dependencies
- decompose.zig has 3 tests for empty function, non-string unchanged, iteration limit
- expand_calls.zig has 4 tests for no registry, empty function, simple call, MAX_SSA_SIZE
- dwarf.zig has 5 tests for LEB128 encoding, DwarfBuilder lifecycle, source info parsing
- obj/macho.zig has 6 tests for struct sizes, basic usage, string deduplication, relocation encoding
- obj/elf.zig has 8 tests for struct sizes, symbol/reloc encoding, basic usage, string deduplication
- driver.zig has 3 tests for init/target, test mode, path normalization
- main.zig has 1 test for runtime path lookup
