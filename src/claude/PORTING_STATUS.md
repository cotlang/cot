# Porting Status: compiler/ → src/

**Last updated:** 2026-03-29

---

## Architecture

```
src/
├── foundation/    (4 files, 1,566 lines)  Language-agnostic shared types
├── libcot-zig/    (12 files, 22,063 lines) Cot frontend — COMPLETE
├── libcir-zig/    (24 files, 10,573 lines) IR + SSA + passes + Wasm — COMPLETE
└── build.zig      Root build with diamond dependency wiring
```

**Total: 41 files, 34,202+ lines. `zig build test` passes.**

---

## foundation/ — Shared Types (COMPLETE)

| File | Lines | Notes |
|------|-------|-------|
| types.zig | 1,035 | TypeIndex, TypeRegistry, BasicKind, VWT |
| source.zig | 264 | Span, Pos, Source |
| target.zig | 153 | Arch, Os, Target |
| debug.zig | 168 | Pipeline logging |
| lib.zig | 15 | Module root |

Language-agnostic — a TypeScript frontend uses the same foundation.

---

## libcot-zig/ — Frontend (COMPLETE)

| File | Lines | Notes |
|------|-------|-------|
| lower.zig | 8,359 | AST → IR lowering (227 functions) |
| checker.zig | 5,424 | Type checking (122 functions, 4-file split planned) |
| parser.zig | 3,218 | Recursive descent, compact AST output |
| ast.zig | 1,594 | Zig-style data-oriented AST, full.* accessors |
| formatter.zig | 1,399 | Source code formatter |
| scanner.zig | 643 | Lexer with string interpolation |
| token.zig | 565 | Cot token definitions |
| errors.zig | 415 | Error/warning reporting (Cot-specific codes) |
| comptime.zig | 108 | Compile-time value system |
| lib.zig | 32 | Module root |

---

## libcir-zig/ — IR + Passes + Wasm (COMPLETE)

| File | Lines | Notes |
|------|-------|-------|
| ir.zig | 1,419 | IR instruction definitions (90+ node types) |
| arc_insertion.zig | 282 | ARC cleanup stack |
| vwt_gen.zig | 1,123 | Value Witness Table generation |
| ssa_builder.zig | ~600 | IR → SSA conversion (core framework) |
| ssa/op.zig | ~500 | 240+ SSA operations |
| ssa/value.zig | ~250 | SSA values |
| ssa/block.zig | ~180 | SSA basic blocks |
| ssa/func.zig | ~220 | SSA functions |
| ssa/dom.zig | ~200 | Dominator tree computation |
| ssa/debug.zig | ~400 | SSA dump, verify, liveness |
| ssa/html.zig | ~700 | COT_SSA HTML visualizer |
| ssa/passes/ (13 files) | ~4,100 | schedule, deadcode, lower_wasm, cse, copyelim, phielim, decompose, rewritedec, rewritegeneric, async_split, layout, region_isolation, lower_native |
| wasm/ (7 files) | ~4,100 | constants, prog, preprocess, gen, assemble, link, wasm entry |
| lib.zig | ~30 | Module root |

---

## Remaining Work

### Refactoring (non-blocking, quality improvement)
- checker.zig 4-file split: builtins, comptime_eval, diagnostics, generics
- lower.zig 7-file split: expr, calls, structs, generics, type_ops, async, collections
- lower.zig coverage gap: ~7,000 lines of original 13,528 still simplified/missing

### Integration Testing
- Parse real .cot files through new pipeline end-to-end
- Compare output with compiler/ pipeline

### Documentation
- DocC-style restructure of ac/ (symbol-based, not line-numbers)
- ac/ docs stale for many files after restructuring

### Future Libraries
- libclif-zig/ — native backend (68K lines, decision: Zig port or Rust Cranelift)
- libts/ — TypeScript frontend (see TYPESCRIPT_NATIVE.md)

### Build System
- C ABI export layer (cot_api.zig implementing cot.h)
- Static library artifact output
- Integration with root cot build.zig
