# Self-Hosted Compiler Audit (Feb 28, 2026)

Full fidelity audit of `self/` files against `compiler/frontend/` Zig reference.

**Overall: ~85% frontend fidelity** (11,153 lines across 9 files, backend 0% ported)

**Post-audit corrections (Feb 28):** Original audit was conservative — 5 of 10 Tier 1/2 gaps were already fixed:
- Gap 2 (`instantiateGenericImplMethods`): FIXED — fully implemented at checker.cot lines 3686-3775
- Gap 3 (cache key format): CORRECT — uses `;` separator matching Zig
- Gap 4 (STRING size): FIXED — `sizeOf` now special-cases STRING → 16 bytes; `basicTypeSize` already correct
- Gap 7 (closures): FIXED — fully implemented at checker.cot lines 3274-3333
- Gap 9 (~30 builtins missing): WRONG — only 5 atomics missing; all other builtins already handled

---

## Per-File Fidelity

| File | Lines | Zig Reference | Fidelity | Status |
|------|-------|---------------|----------|--------|
| `token.cot` | 436 | `token.zig` | **95%** | Near-perfect |
| `scanner.cot` | 736 | `scanner.zig` | **92%** | Excellent |
| `source.cot` | 111 | `source.zig` | **80%** | Good logic, deinit added |
| `errors.cot` | 203 | `errors.zig` | **80%** | All codes present, handler callback added |
| `ast.cot` | 1,259 | `ast.zig` | **78%** | All builtins/types incl. atomics |
| `parser.cot` | 2,691 | `parser.zig` | **92%** | Excellent, full grammar coverage |
| `types.cot` | 1,287 | `types.zig` | **90%** | All type kinds, STRING size fixed |
| `checker.cot` | 4,112 | `checker.zig` | **82%** | 3-pass works, generic_inst_by_name added |
| `main.cot` | 318 | `cli.zig`+`main.zig` | **15%** | 4/14 commands (expected, no backend) |

---

## Detailed Findings

### token.cot — 95%

All 99 token types, 55 keyword mappings, 36 operator tokens present and correct. All 7 helper methods ported (`toString`, `precedence`, `isLiteral`, `isOperator`, `isTypeKeyword`, `isAssignment`, `lookup`). Cot tests exceed Zig coverage (10 vs 8 tests).

**Minor gaps:**
- Keyword lookup uses manual switch instead of Zig's `StaticStringMap` (still O(1), different cache behavior)
- `isLiteral`/`isOperator`/`isKeyword` don't cache `@intFromEnum` result (negligible perf difference)

### scanner.cot — 92%

All scanning methods present: identifiers, numbers (hex/octal/binary/float/underscores), strings (interpolation with brace tracking, escape sequences), characters, operators (40+ types), comments (line/block/doc). Character classification helpers complete.

**Minor gaps:**
- String unterminated error reports on newline encounter (Cot) vs loop exit (Zig) — slightly different timing
- `scanOperator` split into two methods for readability (functionally identical)
- Uses raw `int` offsets instead of `Pos` struct (design choice, not a bug)

### source.cot — 80%

Binary search in `position()` is identical and correct. `ensureLineOffsets()` logic matches. All methods present: `at`, `slice`, `spanText`, `position`, `getLine`, `lineCount`. `deinit()` added to free line_offsets. Cot has 18 tests vs Zig's 9.

**Remaining gaps:**
- **No allocator field** — relies on implicit global allocator
- **Span struct uses flat `start_offset`/`end_offset` ints** instead of nested `Pos` structs — API breaking change
- **`slice()`/`getLine()` allocate new strings** vs Zig's zero-copy slice references
- `init()` takes 2 args (filename, content) vs Zig's 3 (allocator, filename, content)

### errors.cot — 80%

All 25 error codes (E100-E403) and 5 warning codes (W001-W005) with identical descriptions. Error factory functions present: `errorAt`, `errorWithCode`, `errorWithCodeAndNote`, `warnError`. `offsetToLineCol` and `getSourceLine` helpers ported correctly. Error handler callback added for LSP integration.

**Remaining gaps:**
- **Missing `errorAtSpan()`** factory — can't report errors over multi-token spans
- **No `firstError()` accessor** — first error fragmented into `has_first`/`first_msg`/`first_code`/`first_offset` fields instead of `?Error`
- **No ANSI colors** — no TTY detection, no red/yellow/blue error highlighting
- **Single caret `^` underline** — doesn't show span length (`^~~~` in Zig)
- Severity stored as `int` (0/1 magic numbers) instead of `Severity` enum

### ast.cot — 78%

All 54+ `BuiltinKind` variants present and correct, including 5 atomics. All 14 declaration types, 13 statement types present. Core expression types complete. `getImports()` ported. Cot has more helper methods than Zig (debugging/introspection functions).

**Gaps:**
- **Auxiliary nodes in Expr union** — `FieldDef`, `FieldInitNode`, `EnumVariantNode`, `UnionVariantNode`, `DestructureBindNode`, `SwitchCaseNode` are Expr variants in Cot but separate structs in Zig. Pollutes expression namespace.
- **TypeExpr uses `int kind + data1/data2`** instead of Zig's `TypeKind` tagged union — loses type safety
- **Missing optional field markers** — `ForStmt.label`, `ForStmt.index_binding`, `BreakStmt.label`, `ContinueStmt.label`, `WhileStmt.label` should be `?string`, not `string` with empty sentinel
- **BuiltinCall args**: Cot uses `List(int)` (dynamic) vs Zig's `[3]NodeIndex` (fixed-size) — different memory characteristics
- **NewExpr missing `is_constructor` field**
- **14+ struct naming divergences** (e.g., `IndexExpr` vs `Index`, `StructInitExpr` vs `StructInit`)
- **BlockExpr field**: `result_expr` vs Zig's `expr`
- `BuiltinKind` stored as `int` instead of enum type

### parser.cot — 92%

Full language grammar coverage. All expression types (literals, binary/unary ops, calls, field access, index/slice, struct init, new expr, array/tuple literals, if/switch/block expressions, closures, builtins, string interpolation, try/catch/await, error literals, comptime blocks, address-of/deref). All statement types (return, var/const, assignment, if/while/for, break/continue, defer/errdefer, destructure). All declaration types (fn, struct, enum, union, trait, impl, type alias, import, test, bench). Precedence climbing correct. Type expression parsing complete (optional, pointer, error union, slice, array, function, tuple, generic).

**Minor gaps:**
- Builtin dispatch uses manual ifs instead of exhaustive switch on `BuiltinKind` enum
- New expression disambiguation simpler than Zig (less sophisticated @safe mode peek-ahead)
- `parseOperand` not split for stack frame optimization (minor, Cot doesn't have same stack constraints)
- Error messages less context-aware (no `fmt.allocPrint` for dynamic error strings)

### types.cot — 90%

All 15 type kinds present (primitives, pointer, optional, error union, error set, slice, array, map, list, struct, enum, union, function, tuple, future). All factory methods (`makePointer`, `makeOptional`, `makeErrorUnion`, `makeSlice`, `makeArray`, `makeMap`, `makeList`, `makeTuple`, `makeFunc`, `makeFuture`). Type equality and assignability rules complete (identity, untyped conversions, T→?T, T→E!T, integer widening, slice↔array, enum→backing type). ARC predicates (`isTrivial`, `needsARC`, `couldBeARC`) verbatim. STRING sizeOf fixed (16 bytes, not 24). 50+ tests vs Zig's 21.

**Remaining gaps:**
- **Method registry O(n)** — flat `List(MethodEntry)` with linear scan vs Zig's `Map(name, List(MethodInfo))` with O(1) lookup
- Missing `alignOf` alias, `Type.isInvalid()`, `Type.underlying()`
- `lookupByName` returns `-1` sentinel instead of `?TypeIndex`
- No error returns (`!T`) on factory methods — can't detect allocation failures

### checker.cot — 82%

3-pass architecture faithful (collectTypeDecl → collectNonTypeDecl → checkDecl). Expression type checking covers: literals, identifiers, binary/unary ops, calls, field access, index/slice, struct init, new expr, array/tuple literals, if/switch/block expressions, builtins (all ~55 including atomics), string interpolation, try/catch, await, address-of/deref, error literals, closures. Statement checking complete. Declaration checking covers all forms. Edit-distance "did you mean" suggestions ported. Lint warnings (W001-W005) all present. @safe mode support complete. Multi-file SharedCheckerState works. `generic_inst_by_name` added for backend porting. `instantiateGenericImplMethods()` fully implemented with cross-file expr_types isolation.

**Remaining gaps:**
- **Comptime evaluation ~70%** — basic i64/float const-folding works. No array construction/mutation, no rich `ComptimeValue` tagged union, no `@typeInfo()` return values.
- **Trait bounds not validated** — generic functions with trait constraints accept invalid types silently.
- Missing `source_tree` tracking for cross-file redefinition checks
- Type substitution uses parallel lists instead of HashMap (less efficient)

### main.cot — 15%

4 of 14 commands: `parse`, `check`, `lex`, `help`/`version`. Multi-file import resolution works with circular import detection and stdlib path resolution. `isProjectSafe()` reads cot.json (naive string search, not JSON parsing).

**Missing commands:** build, run, test, bench, lint, fmt, doc, task, info, init, lsp, mcp. Expected — no backend exists. Version hardcoded `"0.3.3"` instead of reading `VERSION` file.

---

## Priority Gaps (Ordered)

### Tier 1 — Blocking Backend Port

| # | Gap | File | Status |
|---|-----|------|--------|
| 1 | No `generic_inst_by_name` in SharedCheckerState | checker.cot | **FIXED** — parallel arrays added |
| 2 | No `instantiateGenericImplMethods()` | checker.cot | **FIXED** — already implemented (lines 3686-3775) |
| 3 | Generic cache key format mismatch | checker.cot | **NOT A BUG** — uses `;` separator, matches Zig |
| 4 | STRING size mismatch (24 vs 16 bytes) | types.cot | **FIXED** — sizeOf special-cases STRING → 16 |
| 5 | No `deinit()` on Source | source.cot | **FIXED** — deinit() added |

### Tier 2 — Missing Language Features

| # | Gap | File | Status |
|---|-----|------|--------|
| 6 | Comptime evaluation ~70% | checker.cot | Open — no arrays, limited ComptimeValue |
| 7 | Closures stubbed | checker.cot | **FIXED** — fully implemented (lines 3274-3333) |
| 8 | Trait bounds not validated at instantiation | checker.cot | Open — basic trait checks work |
| 9 | ~30 builtins missing from checker | checker.cot | **FIXED** — only 5 atomics were missing, now added |
| 10 | No error handler callback | errors.cot | **FIXED** — callback field added |

### Tier 3 — Architectural Tech Debt

| # | Gap | File | Impact |
|---|-----|------|--------|
| 11 | Auxiliary nodes in Expr union | ast.cot | Polluted expression namespace |
| 12 | TypeExpr uses `int kind + data1/data2` | ast.cot | No type safety |
| 13 | Missing optional field markers on ForStmt/BreakStmt/etc. | ast.cot | Empty-string sentinels instead of `?string` |
| 14 | Method registry O(n) linear scan | types.cot | Performance degrades with many methods |
| 15 | No ANSI color in diagnostics | errors.cot | Poor developer UX |

---

## What's Excellent

- **Token/Scanner (95%/92%)**: Rock-solid lexing. Essentially done.
- **Parser (92%)**: Full language grammar. All expression, statement, declaration, type forms.
- **Type system core (90%)**: All type kinds, equality, assignability, ARC predicates, STRING size correct.
- **3-pass checking architecture**: Faithful to Zig's multi-pass pattern.
- **All builtins handled**: ~55 builtins including atomics, casts, math, reflection.
- **Closures**: Fully implemented with capture analysis.
- **Generic instantiation**: Full pipeline with cross-file isolation and cache key matching.
- **Test coverage**: Cot has MORE tests than Zig in every single file.
- **Multi-file import resolution**: Working with circular import detection.
- **Edit-distance suggestions**: "Did you mean X?" diagnostics ported.
- **Lint warnings**: All 5 warning codes active.

---

## Backend Status

The backend remains **0% ported** (12,095 lines):

| File | Lines | Purpose |
|------|-------|---------|
| `lower.zig` | 8,964 | AST → IR lowering |
| `ir.zig` | 606 | IR definitions |
| `ssa_builder.zig` | 2,017 | SSA construction |
| `arc_insertion.zig` | 444 | ARC retain/release insertion |
| codegen/* | ~15,000+ | Wasm + native code generation |

All Tier 1 gaps resolved. Backend porting can begin.
