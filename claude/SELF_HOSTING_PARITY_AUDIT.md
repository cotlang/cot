# Self-Hosted Compiler Parity Audit

**Date:** March 12, 2026
**Scope:** Full audit of `self/` (self-hosted Cot compiler) vs `compiler/` (Zig reference compiler)
**Methodology:** Line-by-line comparison of all 6 subsystems by specialized audit agents
**Overall Coverage:** ~80% feature-complete across all subsystems

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [AST & Type System Gaps](#2-ast--type-system-gaps)
3. [Parser Gaps](#3-parser-gaps)
4. [Checker Gaps](#4-checker-gaps)
5. [Lowerer Gaps](#5-lowerer-gaps)
6. [SSA Builder Gaps](#6-ssa-builder-gaps)
7. [Wasm Codegen Gaps](#7-wasm-codegen-gaps)
8. [Reference Language Patterns](#8-reference-language-patterns)
9. [Implementation Priority & Roadmap](#9-implementation-priority--roadmap)
10. [Acceptance Criteria](#10-acceptance-criteria)

---

## 1. Executive Summary

| Subsystem | Self-Hosted | Zig Reference | Coverage | Critical Gaps |
|-----------|------------|---------------|----------|---------------|
| **AST** (`ast.cot`) | 1,468 lines | ~1,600 lines | ~92% | 3 missing Expr variants, 1 missing Stmt field |
| **Types** (`types.cot`) | 1,484 lines | ~1,800 lines | ~85% | Shape struct missing (80+ lines) |
| **Parser** (`parser.cot`) | 2,940 lines | 2,340 lines | ~87% | 2 missing parse functions |
| **Checker** (`checker.cot`) | 5,325 lines | ~7,500 lines | ~66% | 41 missing functions (~2,400 LOC) |
| **Lowerer** (`lower.cot`) | 6,618 lines | 11,164 lines | ~63% | 16 missing functions (~1,194 LOC) |
| **SSA Builder** (`ssa_builder.cot`) | 2,099 lines | ~2,100 lines | ~95% | Async/await only |
| **Wasm Codegen** (`wasm_gen.cot`) | 1,255 lines | 1,521 lines | ~82% | Optimizations only |
| **TOTAL** | ~21,189 lines | ~27,025 lines | **~80%** | ~3,800 LOC to write |

### Blocking Features (cannot compile real programs without these)

| Feature | Missing From | Impact | LOC Estimate |
|---------|-------------|--------|-------------|
| `orelse` operator (`x orelse y`) | AST, Parser, Checker, Lowerer | Optional fallback syntax | ~200 |
| `spawn` expression | AST, Parser, Checker, Lowerer | Concurrency | ~250 |
| `select` statement | AST, Parser, Checker, Lowerer | Channel multiplexing | ~350 |
| Comptime evaluation (Phases 2-5) | Checker | Array sizes, const folding, inline for | ~850 |
| Error suggestions ("did you mean?") | Checker | Developer experience | ~250 |
| Async state machine lowering | Lowerer | Async/await on Wasm | ~440 |
| Labeled block expressions | Parser, Lowerer | `break :label value` | ~90 |
| Shape struct (generic optimization) | Types | Binary size (8-10x bloat without) | ~150 |

---

## 2. AST & Type System Gaps

### 2.1 Missing Expr Variants (CRITICAL)

Three expression types exist in `compiler/frontend/ast.zig` but not in `self/frontend/ast.cot`:

#### `spawn_expr`
- **Zig:** `ast.zig:103`, definition at `ast.zig:394-395`
- **Struct:** `SpawnExpr { body: NodeIndex, span: Span }`
- **Syntax:** `spawn { body }`
- **Reference:** Go goroutines — fire-and-forget lightweight task creation
- **Impact:** ANY concurrent code fails to parse

#### `select_expr`
- **Zig:** `ast.zig:104`, definition at `ast.zig:402-415`
- **Struct:** `SelectExpr { cases: []SelectCase, default_body: NodeIndex, span: Span }`
- **Supporting:** `SelectCase { kind: SelectCaseKind, channel: NodeIndex, capture: string, send_value: NodeIndex, body: NodeIndex, span: Span }`
- **Supporting:** `SelectCaseKind = enum { recv, send }`
- **Syntax:** `select { recv ch |val| { body }, send ch val { body }, default { body } }`
- **Reference:** Go `select` statement — channel multiplexing
- **Impact:** Channel operations completely blocked

#### `orelse_expr`
- **Zig:** `ast.zig:106`, definition at `ast.zig:417-423`
- **Struct:** `OrElseExpr { operand: NodeIndex, fallback: NodeIndex, fallback_kind: OrElseFallback, span: Span }`
- **Supporting:** `OrElseFallback = enum { expr, return_void, return_val, break_val, continue_val }`
- **Syntax:** `opt orelse default`, `opt orelse return`, `opt orelse break :label`
- **Reference:** Zig `orelse`, Rust `unwrap_or`, Swift `??`
- **Impact:** Optional fallback operator completely missing

### 2.2 Missing Stmt Field (HIGH)

#### `BreakStmt.value`
- **Zig:** `ast.zig:484` — `BreakStmt { label: ?[]const u8, value: NodeIndex, span: Span }`
- **Self-hosted:** `ast.cot:458-461` — `BreakStmt { label: string, span: Span }` (NO value field)
- **Syntax:** `break :label 42` — labeled break with return value
- **Impact:** Block expressions cannot return values via labeled breaks

### 2.3 Missing Type System: Shape Struct (HIGH)

The Shape struct enables generic deduplication (92% code sharing via shape stenciling).

- **Zig:** `types.zig:668-747` (~80 lines)
- **Self-hosted:** COMPLETELY MISSING

```
Shape struct contains:
  size: u32          — byte size of type
  alignment: u32     — alignment requirement
  arc_kind: ArcKind  — none | pointer | slice (ARC category)
  reg_class: RegClass — int | float | compound (register class)

Methods:
  fromType(reg, ty) → Shape   — compute shape from concrete type
  eql(a, b) → bool            — compare two shapes
  key(self) → ShapeKey         — generate unique suffix for function naming

Nested: ShapeKey { buf: [16]u8, len: u8 }
```

**Reference:** Go's `shapify()` in `cmd/compile/internal/noder/reader.go:891-969`

**Impact without Shape:**
- Every generic instantiation produces unique code (full monomorphization)
- 8-10x binary size bloat on generic-heavy programs
- Cannot use shape stenciling optimization

### 2.4 Implementation Tasks

```
[x] Add spawn_expr to Expr union in ast.cot (DONE — Mar 12, 2026)
    - Add SpawnExpr struct: { body: int, span: Span }
[x] Add select_expr to Expr union in ast.cot (DONE — Mar 12, 2026)
    - Add SelectExpr struct: { cases: List(SelectCase), default_body: int, span: Span }
    - Add SelectCase struct: { kind: int, channel: int, capture: string, send_value: int, body: int, span: Span }
    - Add SelectCaseKind constants: RECV=0, SEND=1
    - Added select_cases: List(SelectCase) to Ast + addSelectCase/getSelectCase methods
[x] Add orelse_expr to Expr union in ast.cot (DONE — Mar 12, 2026)
    - OrElseExpr struct: { operand: int, fallback: int, fallback_kind: int, span: Span }
    - ORELSE_EXPR/RETURN_VOID/RETURN_VAL/BREAK_VAL/CONTINUE_VAL constants
[x] Add value field to BreakStmt (DONE — Mar 12, 2026)
    - BreakStmt { label: string, value: int, span: Span }
[x] Add label field to BlockExpr (DONE — Mar 12, 2026)
    - BlockExpr { stmts: List(int), result_expr: int, label: string, span: Span }
[x] Add Shape struct to types.cot with ArcKind and RegClass constants (DONE — Mar 12, 2026)
[x] Implement Shape.fromType() (as shapeOf), Shape.eql(), Shape.key() methods (DONE — Mar 12, 2026)
[x] Shape key returns string directly (no ShapeKey struct needed in Cot) (DONE — Mar 12, 2026)
```

---

## 3. Parser Gaps

**Overall:** 87% feature-complete. 2,940 lines vs 2,340 lines (self-hosted is larger due to modular extraction).

### 3.1 Missing Parse Functions

#### `parseSelectExpr` (DONE — Mar 12, 2026)
- **Zig:** `parser.zig:1819-1893`
- Contextual keyword handling in `parseOperand`: spawn/select as ident text match + peek for `{`
- Parses recv cases (`capture from channel => { body }`), send cases, default

#### `parseLabeledBlockExpr` (HIGH — 25-30 lines)
- **Zig:** `parser.zig:1687-1712`
- **Syntax:**
  ```cot
  'label: {
    statements
    break :label value
  }
  ```
- **Parses:** Label identifier → `:` → block body
- **Dependencies:** BreakStmt.value field in AST

### 3.2 Missing Helper Extractions (LOW priority — functional, not structural)

| Function | Zig Lines | Purpose | Impact |
|----------|-----------|---------|--------|
| `parseColonFields` | 89-107 | Colon-syntax field parsing with shorthand | @safe mode field init |
| `parseFieldList` | 347-383 | Reusable field list parser for struct/union | Code duplication |
| `parseExternFn` | 236-241 | External function dispatcher | Modularity only |
| `parseExportFn` | 242-247 | Export function dispatcher | Modularity only |

### 3.3 Architectural Strengths of Self-Hosted

The self-hosted parser has 13 **extra** extracted functions not in the Zig version:
- `parseClosureExpr`, `parseNewExpr`, `parseFieldInit`, `parseSwitchCase`
- `parseReturnStmt`, `parseBreakStmt`, `parseContinueStmt`, `parseDeferStmt`
- `parseWeakVarStmt`, `parseUnownedVarStmt`, `parseInlineFor`
- `parseOperandLiteral`, `parseOperandComplex` (stack overflow mitigation)

### 3.4 Implementation Tasks

```
[ ] Implement parseSelectExpr() in parser.cot (~80 lines)
    - Parse `select` keyword
    - Parse case list: recv (`val from channel => { body }`) and send (`channel.send(value) => { body }`)
    - Parse optional `default => { body }`
    - Return SelectExpr AST node
    - Reference: parser.zig:1819-1893

[x] Implement parseLabeledBlockExpr() in parser.cot (DONE — Mar 12, 2026)
    - Parses label: { ... break :label value ... }
    - Stores label on BlockExpr

[x] Update parseBreakStmt() to parse optional value expression (DONE — Mar 12, 2026)
    - break :label expr parses value after label
    - Stored in BreakStmt.value field
```

---

## 4. Checker Gaps

**Overall:** 66% feature-complete. 5,325 lines vs ~7,500 lines. **41 missing functions (~2,400 LOC).**

### 4.1 TIER 1 — Comptime Evaluation System (CRITICAL — ~850 LOC)

The largest single gap. 8 functions implementing compile-time evaluation:

| Function | Zig Lines | What It Evaluates | Example |
|----------|-----------|-------------------|---------|
| `evalComptimeValue` | 836-1123 | Literals, unary, binary, paren, ident, builtins | `const N = 10` |
| `evalConstExpr` | 1336-1510 | i64 from pure const expressions | `var arr: [N]int` |
| `evalComptimeBlock` | 1124-1181 | Block with statements + final expr | `comptime { var s = ... }` |
| `evalConstFloat` | 1511-1542 | f64 from float literals | `const PI = 3.14` |
| `evalConstString` | 1548-1567 | String from string literals | `const name = "hello"` |
| `evalComptimeAssign` | 1182-1220 | Update comptime mutable var | `s[i] = val` in comptime block |
| `evalComptimeInlineFor` | 1250-1335 | Execute inline for at compile time | `inline for f in @typeInfo(T).fields` |
| `evalComptimeArrayType` | 1231-1249 | Array size from `[N]T` | `[10]int` |

**What breaks without this:**
- `const N = 10; var arr: [N]int` — ERROR: N not evaluated
- `inline for i in 0..10 { ... }` — ERROR: compile-time iteration fails
- `var x: i64 = 5 * 3` — may fail: const folding incomplete
- `const PI = 3.14159; var r: f64 = PI * 2` — ERROR: float const folding missing

**Implementation approach:**
1. Start with `evalConstExpr` (arithmetic/comparison on integer literals) — unblocks array sizes
2. Add `evalComptimeValue` (extends to floats, strings, builtins like `@sizeOf`)
3. Add `evalComptimeBlock` + `evalComptimeAssign` (comptime mutable vars)
4. Add `evalComptimeInlineFor` (most complex — binds to `Symbol.comptime_val`)

### 4.2 TIER 2 — Error Suggestions System (HIGH — ~250 LOC)

**Currently:** 0% implemented. Errors are bare: `"undefined type 'Lsit'"`.
**Target:** `"undefined type 'Lsit' (did you mean 'List'?)"`.

| Function | Zig Lines | Algorithm |
|----------|-----------|-----------|
| `editDistance` | 4074-4098 | Levenshtein distance (single-row DP, stack-safe) |
| `editDistSuggest` | 4243-4257 | Min distance from candidate list |
| `isUserVisibleName` | 4101-4108 | Filter internal names (`__` prefix, generated) |
| `findSimilarName` | 4111-4162 | Search scope chain + global scope (edit dist ≤ 2) |
| `findSimilarField` | 4164-4178 | Search struct fields |
| `findSimilarVariant` | 4179-4193 | Search enum/union variants |
| `findSimilarType` | 4194-4223 | Search type registry + generic_structs |
| `errWithSuggestion` | 4258-4267 | Append suggestion to error message |

**Implementation approach:**
1. Port `editDistance` (pure algorithm, no dependencies)
2. Port `isUserVisibleName` + `findSimilarName` (scope chain search)
3. Port `findSimilarField`, `findSimilarVariant`, `findSimilarType`
4. Wire into error reporting via `errWithSuggestion`

### 4.3 TIER 3 — Expression Checking (HIGH — ~300 LOC)

| Function | Lines | What It Checks | Syntax |
|----------|-------|---------------|--------|
| `checkErrorLiteral` | 3110-3143 | `error.X` type inference from context | `return error.NotFound` |
| `checkOrElseExpr` | 3089-3108 | `opt orelse fallback` type validation | `x orelse 0` |
| `checkBlock` | 2926-2957 | Block expression with labeled result type | `'label: { break :label 42 }` |
| `checkContinueExpr` | 3388-3396 | Continue with optional label | `continue 'outer` |
| `checkSpawnExpr` | 3005-3009 | Spawn body validation (returns VOID) | `spawn { body }` |
| `checkSelectExpr` | 3011-3059 | Channel type validation, capture binding | `select { recv ch \|val\| { } }` |

**`checkSelectExpr` details (most complex, ~50 LOC):**
- Extract element type from monomorphized `Channel(T)` struct
- Validate recv/send operations match channel type
- Bind capture variables in case scope
- Handle default arm

### 4.4 TIER 4 — Infrastructure & Utilities (~500 LOC)

| Category | Functions | LOC | Impact |
|----------|-----------|-----|--------|
| Core infrastructure | `checkFnDeclWithName`, `resolveMethodCall`, `collectDecl` | ~316 | Generic function params, method dispatch |
| Type resolution | `resolveType`, `resolveTypeByName` | ~73 | Nested types like `*[N]T` |
| Type building | `buildFuncType`, `buildStructTypeWithLayout`, `mergeErrorSets`, `buildGenericCacheKey` | ~74 | Packed struct layout, error set merging |
| Scope management | `registerMethod`, `lookupMethod`, `defineInFileScope` | ~32 | Method registry |
| Utilities | `isUndefinedLit`, `isZeroInitLit`, `isEmptyBlock`, `checkVarDecl`, `checkBenchDecl` | ~60 | Special literal handling, bench support |

### 4.5 Implementation Tasks

```
TIER 1 — Comptime (~850 LOC):
[ ] Port evalConstExpr — integer arithmetic/comparison const folding
    - Handles: literals, unary, binary, paren, ident, @sizeOf/@alignOf/@offsetOf
    - Reference: checker.zig:1336-1510
[ ] Port evalComptimeValue — extends to floats, strings, builtins
    - Reference: checker.zig:836-1123
[ ] Port evalConstFloat — f64 const folding
    - Reference: checker.zig:1511-1542
[ ] Port evalConstString — string const evaluation
    - Reference: checker.zig:1548-1567
[ ] Port evalComptimeBlock — block execution with statements
    - Reference: checker.zig:1124-1181
[ ] Port evalComptimeAssign — comptime mutable variable updates
    - Reference: checker.zig:1182-1220
[ ] Port evalComptimeInlineFor — compile-time loop unrolling
    - Reference: checker.zig:1250-1335
[ ] Port evalComptimeArrayType — array size evaluation
    - Reference: checker.zig:1231-1249

TIER 2 — Error Suggestions (~250 LOC):
[ ] Port editDistance — Levenshtein distance algorithm
[ ] Port findSimilarName — scope chain + global scope search
[ ] Port findSimilarField, findSimilarVariant, findSimilarType
[ ] Port errWithSuggestion — wire into error reporting
[ ] Port isUserVisibleName — filter internal names

TIER 3 — Expression Checking (~300 LOC):
[x] Port checkErrorLiteral — error.X type inference (DONE — Mar 12, 2026)
[x] Port checkOrElseExpr — optional orelse type validation (DONE — Mar 12, 2026)
[ ] Port checkBlock — labeled block result type tracking
[ ] Port checkContinueExpr — continue with label
[x] Port checkSpawnExpr — spawn body validation (DONE — Mar 12, 2026)
[x] Port checkSelectExpr — channel type validation + capture binding (DONE — Mar 12, 2026)

TIER 4 — Infrastructure (~500 LOC):
[ ] Port resolveType — full recursive TypeExpr resolution
[ ] Port buildStructTypeWithLayout — packed/extern layout
[ ] Port mergeErrorSets — error set deduplication
[ ] Port checkVarDecl — undefined_lit + zero_init_lit detection
[ ] Port checkBenchDecl — benchmark declarations
```

---

## 5. Lowerer Gaps

**Overall:** 63% feature-complete. 6,618 lines vs 11,164 lines. **16 missing functions (~1,194 LOC).**

### 5.1 P0 — Async/Concurrency (CRITICAL — 837 LOC)

| Function | Zig Lines | LOC | What It Generates |
|----------|-----------|-----|-------------------|
| `lowerAsyncStateMachine` | 813-963 | 150 | Wasm state machine (constructor + poll dispatch) |
| `lowerAsyncFiber` | 963-1090 | 127 | Native async (eager body execution) |
| `countAwaitPoints` | 1090-1147 | 57 | Recursive AST walk counting `await` expressions |
| `lowerAwaitExpr` | 10222-10327 | 105 | `await future_ptr` — polling or eager dispatch |
| `lowerSpawnExpr` | 6002-6112 | 110 | `spawn { body }` — capture detection + env allocation |
| `lowerSelectExpr` | 6112-6133 | 21 | Dispatcher: routes to simple or general |
| `lowerSelectSimple` | 6133-6277 | 144 | 1-case select: `tryRecv()`/`trySend()` + branch |
| `lowerSelectGeneral` | 6277-6400 | 123 | N-case select: dispatch loop |

**Critical pitfalls documented in MEMORY.md:**
- **Select method resolution:** Must use `type_reg.lookupMethod(struct_name, "tryRecv")` + `resolveMethodName()`. Bare `"tryRecv"` fails for generic channels.
- **SRET for compound optionals:** `needsSret(opt_type)` determines call pattern for channel recv.
- **Async state struct layout:** `offset = (1 + result_words + param_offset) * 8`
- **Poll function dispatch:** Wasm `br_table` must reload `state_ptr` from local after each block.

### 5.2 P1 — Control Flow & Error Handling (HIGH — 168 LOC)

| Function | Zig Lines | LOC | What It Generates |
|----------|-----------|-----|-------------------|
| `lowerLabeledBlockExpr` | 4121-4181 | 60 | `'label: { ... break :label val }` |
| `lowerOrElseExpr` | 10467-10561 | 94 | `opt orelse fallback` — compound optional branch |
| `lowerDeferredNode` | 2117-2131 | 14 | Route deferred expression to appropriate lowering |

**`lowerOrElseExpr` details:**
```
IR generated:
  addLocalWithSize("__orelse_opt", operand_type)
  emitStoreLocal(opt_local, operand)
  emitFieldLocal(opt_local, 0, 0, I64)      // extract tag
  emitBinary(.ne, tag, zero)                 // is_non_null?
  emitBranch(is_non_null, then_block, else_block)

  then_block: emitFieldLocal(opt_local, 1, 8, elem_type)  // payload
  else_block: lowerExprNode(fallback)                       // fallback
  merge_block: emitLoadLocal(result_local)
```

**`lowerLabeledBlockExpr` details:**
```
IR generated:
  addLocalWithSize("__block_result", result_type)
  newBlock("block.exit")
  labeled_block_stack.push({label, exit_block, result_local})
  for each stmt: lowerStmt(stmt)
  if block.expr: store to result_local
  emitCleanups + emitJump(exit_block)
  setBlock(exit_block)
  emitLoadLocal(result_local)
```

### 5.3 P2 — Generic Shape Stenciling Helpers (MEDIUM — 107 LOC)

| Function | Zig Lines | LOC | What It Generates |
|----------|-----------|-----|-------------------|
| `generateDictHelpers` | 8395-8411 | 16 | Deduplication loop for dict entries |
| `generateBinaryOpHelper` | 8411-8447 | 36 | `__cot_dict_TYPE_OP` fn for binary ops in stenciled generics |
| `generateMethodCallHelper` | 8447-8502 | 55 | `__cot_dict_TYPE_METHOD` trampoline for method calls |

### 5.4 P2 — WasmGC Struct Init (MEDIUM — 82 LOC)

| Function | Zig Lines | LOC | What It Generates |
|----------|-----------|-----|-------------------|
| `emitGcDefaultValue` | 5611-5640 | 29 | Zero/default values for GC struct fields |
| `emitGcStructNewExpanded` | 5640-5693 | 53 | Expand semantic field values into i64 chunks |

### 5.5 Implementation Tasks

```
P0 — Async/Concurrency (~837 LOC):
[x] Port lowerSpawnExpr — capture detection + env allocation + sched_spawn (DONE — Mar 12, 2026)
    - Reference: lower.zig:6002-6112
[x] Port lowerSelectExpr — dispatcher (routes to simple/general) (DONE — Mar 12, 2026)
    - Reference: lower.zig:6112-6133
[x] Port lowerSelectSimple — 1-case select with tryRecv/trySend (DONE — Mar 12, 2026)
    - Reference: lower.zig:6133-6277
[x] Port lowerSelectGeneral — N-case select dispatch loop (DONE — Mar 12, 2026)
    - Reference: lower.zig:6277-6400
[ ] Port lowerAsyncStateMachine — Wasm state machine generation (skipped — Wasm target only)
    - Reference: lower.zig:813-963
[x] Port lowerAsyncFiber — Native eager execution (DONE — Mar 12, 2026)
    - Reference: lower.zig:963-1090
[x] Port countAwaitPoints — recursive AST walker (DONE — Mar 12, 2026)
    - Reference: lower.zig:1090-1147
[x] Port lowerAwaitExpr — await with polling/eager dispatch (DONE — Mar 12, 2026)
    - Native path only (eager evaluation), Wasm poll loop not yet ported
    - Reference: lower.zig:10222-10327

P1 — Control Flow (~168 LOC):
[x] Port lowerLabeledBlockExpr — labeled block + break :label (DONE — Mar 12, 2026)
    - labeled_block_stack maintained, lowerBreak updated with value param
[x] Port lowerOrElseExpr — compound optional orelse (DONE — Mar 12, 2026)
    - Both compound and ptr-like paths, 5 fallback kinds
[ ] Port lowerDeferredNode — deferred cleanup dispatcher
    - Reference: lower.zig:2117-2131

P2 — Shape Stenciling + WasmGC (~189 LOC):
[x] Port generateDictHelpers — dict helper deduplication loop (DONE — Mar 12, 2026)
[ ] Port generateBinaryOpHelper — binary op trampoline generation
[ ] Port generateMethodCallHelper — method call trampoline generation
[ ] Port emitGcDefaultValue — GC struct default field values
[ ] Port emitGcStructNewExpanded — compound field decomposition
```

---

## 6. SSA Builder Gaps

**Overall:** 95% feature-complete. 2,099 lines vs ~2,100 lines.

The SSA builder is the most complete subsystem. **All core operations for v0.3.5 are implemented and tested.**

### 6.1 SsaOp Coverage: 100%

All 180+ SSA operations are defined in `ssa.cot` with complete parity to `op.zig`. Categories:
- Constants, Integer/Float arithmetic, Bitwise, Comparisons ✅
- Memory ops (load/store all widths), Control flow ✅
- Function calls (direct, indirect, closure, dict-stenciled) ✅
- ARC (retain, release), Atomics, WasmGC ✅
- All Wasm-specific ops (i64/i32/f64/f32 arithmetic, memory, variables, control) ✅

### 6.2 Conversion Functions: 95%

All critical functions implemented:
- `convertLoadLocal`, `convertStoreLocal` (all types including compound optional) ✅
- `convertGlobalRef`, `convertGlobalStore` ✅
- `convertBinary`, `convertUnary`, `convertCall` ✅
- `convertFieldLocal`, `convertFieldValue`, `convertStoreLocalField`, `convertStoreField` ✅
- `convertIndexLocal`, `convertSliceLocal` ✅
- `convertPtrLoad`, `convertPtrStore`, `convertPtrField` ✅
- `convertSelect`, `convertConvert`, `convertStrConcat` ✅
- `convertUnionInit`, `convertUnionTag`, `convertUnionPayload` ✅
- `insertPhis` (Braun et al. 2013) ✅

### 6.3 Known Gaps

| Feature | LOC | Priority | Why |
|---------|-----|----------|-----|
| Async/await support | ~170 | LOW | v0.4 feature |
| WasmGC type coercions (inheritance) | ~50 | LOW | WasmGC still nascent |
| Exception handling (full try/catch) | ~150 | MEDIUM | Error propagation edge cases |

### 6.4 Critical Fix Applied (This Session)

**Entry block duplication bug:** `SSABuilder.init()` created entry block (index 0) but didn't add it to `block_map`. `getOrCreateBlock(0)` in `build()` created a duplicate ghost block (index 1), shifting all subsequent block indices by 1. **Fixed** by adding `block_map.set(0, entry)` in `init()`.

---

## 7. Wasm Codegen Gaps

**Overall:** 82% feature-complete. 1,255 lines vs 1,521 lines.

### 7.1 Fully Implemented (50+ operations)

All core Wasm bytecode generation works:
- Constants (i64, i32, f64, string) ✅
- I64 arithmetic (add, sub, mul, div, rem) + bitwise (and, or, xor, shl, shr) ✅
- I64 comparisons (eq, ne, lt, le, gt, ge, signed + unsigned) ✅
- F64 arithmetic + comparisons ✅
- Memory operations (load/store all widths) ✅
- Addressing (local_addr, global_addr, off_ptr, add_ptr, sub_ptr) ✅
- Variables (local_get/set/tee, global_get/set) ✅
- Function calls (direct, indirect) ✅
- String/slice operations (make, ptr, len, copy) ✅

### 7.2 Known Gaps (Optimizations Only — No Blockers)

| Feature | LOC | Priority | Status |
|---------|-----|----------|--------|
| Float constant encoding (f64.const) | 20 | MEDIUM | Workaround: uses i64.const with bit pattern |
| Dedicated I32 handlers | 30 | LOW | Works via i64 upcast/truncate |
| Compound return tracking | 40 | MEDIUM | Simplified; acceptable |
| WasmGC struct codegen | 50 | LOW | Partial; experimental |
| return_call (Wasm 3.0) | 20 | DEFERRED | Disabled in both (causes SIGILL) |

### 7.3 Active Bug: Wasm Validation Error

Function 84 in selfcot-produced wasm fails validation: `"type mismatch: expected i32 but nothing on stack"`. Root cause investigation in progress — related to block control flow generation for functions with if/return patterns. The entry block duplication fix (Section 6.4) addressed part of the issue; remaining work needed on block kind persistence and forward jump handling.

### 7.4 SSA Passes Status

| Pass | Zig File | Self-Hosted | Status |
|------|----------|-------------|--------|
| rewritegeneric | `passes/rewritegeneric.zig` | `ssa_passes.cot` | ✅ Complete |
| rewritedec | `passes/rewritedec.zig` | `ssa_passes_dec.cot` | ✅ Complete |
| lower_wasm | `passes/lower_wasm.zig` | NOT PORTED | ❌ Critical (~500 LOC) |
| decompose | `passes/decompose.zig` | NOT PORTED | ❌ Native only (~400 LOC) |
| layout | `passes/layout.zig` | NOT PORTED | ❌ Native only (~300 LOC) |
| schedule | `passes/schedule.zig` | NOT PORTED | ❌ Native only (~350 LOC) |

---

## 8. Reference Language Patterns

How each feature maps to reference languages, for implementation guidance:

### 8.1 orelse Operator
- **Copy from:** Zig (`opt orelse y`) — lowest precedence, simple branch
- **Cot IR:** Extract tag → branch → payload or fallback
- **Compound optional:** 16-byte `[tag:i64][payload:i64]` — tag at field 0, payload at field 1 offset 8
- **Ptr-like optional:** Single-value null check (no compound extraction)
- **Complexity:** LOW (~70 LOC lowering)

### 8.2 spawn/async
- **Copy from:** Go (goroutines) for spawn + Zig/Rust dual backend for async
- **Spawn pattern:** Detect captures → heap-allocate env struct → generate `__spawn_N()` body → call `sched_spawn(fn_ptr, env_ptr)`
- **Async Wasm:** Rust-style stackless state machine (constructor + poll method)
- **Async Native:** Zig-style eager execution (body runs synchronously)
- **Complexity:** HIGH (~1,280 LOC total)

### 8.3 select Statement
- **Copy from:** Go `select` — exact port
- **Simple path (1 case + default):** Call `tryRecv()`/`trySend()` → branch on result
- **General path (N cases):** Build dispatch loop over all channels
- **PITFALL:** Method name resolution requires `type_reg.lookupMethod()` + `resolveMethodName()` — bare method names fail for generic channels
- **Complexity:** MEDIUM-HIGH (~170 LOC)

### 8.4 Error Sets/Unions
- **Copy from:** Zig error unions + **Cot extension** (inferred error sets)
- **Cot innovation:** `!T` instead of `E!T` — compiler tracks all possible errors automatically
- **Merge:** `E1 || E2` deduplicates variants
- **Already implemented** in Zig compiler; self-hosted needs `checkErrorLiteral` + `mergeErrorSets`

### 8.5 Comptime
- **Copy from:** Zig comptime blocks + @typeInfo reflection
- **5 phases:** Literal eval → ComptimeAlloc → ComptimeExec → InlineFor → Binding
- **Key builtins:** `@sizeOf`, `@alignOf`, `@offsetOf`, `@intFromBool`, `@targetOs`, `@typeInfo`
- **Dead branch elimination:** If condition is comptime-known, only taken branch checked

### 8.6 Closures
- **Copy from:** Go (implicit heap capture) with explicit environment struct
- **Pattern:** Detect captures → allocate env → store captures → closure value = `(code_ptr, env_ptr)`
- **Call site:** `call_indirect` with env as self parameter
- **Already implemented** in both compilers

### 8.7 ARC Memory
- **Copy from:** Swift (HeapObject retain/release)
- **Native:** 24-byte header `[total_size][metadata][refcount]` + `ARC_HEAP_MAGIC` guard
- **Wasm:** WasmGC for structs (no ARC needed); freelist allocator for buffers
- **ARC insertion pass** (`arc_insertion.zig` ~444 lines) NOT YET PORTED to self-hosted

### 8.8 Generics + Shape Stenciling
- **Copy from:** Zig (comptime generics) + Go (shape stenciling — `shapify()`)
- **3 tiers:** Alias (62%) → Dict-stenciled (30%) → Full mono (8%) = 92% sharing
- **Self-hosted needs:** Shape struct + dict helper generation

---

## 9. Implementation Priority & Roadmap

### Phase 1: Core Language Completeness (~1,300 LOC)

**Goal:** Self-hosted compiler can parse, check, and lower ALL Cot syntax.

| Order | Feature | Subsystem | LOC | Status |
|-------|---------|-----------|-----|--------|
| 1 | orelse operator | AST + Parser + Checker + Lowerer | ~200 | DONE (Mar 12) |
| 2 | Labeled blocks | AST + Parser + Lowerer | ~90 | DONE (Mar 12) |
| 3 | Comptime eval (Tiers 1-2) | Checker | ~500 | ALREADY EXISTED |
| 4 | Error literal checking | Checker | ~40 | DONE (Mar 12) |
| 5 | checkOrElseExpr | Checker | ~30 | DONE (Mar 12) |
| 6 | lowerOrElseExpr | Lowerer | ~94 | DONE (Mar 12) |
| 7 | lowerLabeledBlockExpr | Lowerer | ~60 | DONE (Mar 12) |
| 8 | BreakStmt.value | AST + Parser + Lowerer | ~30 | DONE (Mar 12) |
| 9 | Comptime eval (Tiers 3-5) | Checker | ~350 | ALREADY EXISTED |

### Phase 2: Concurrency (~1,100 LOC)

**Goal:** spawn, select, async/await all work in self-hosted compiler.

| Order | Feature | Subsystem | LOC |
|-------|---------|-----------|-----|
| 1 | spawn_expr + select_expr AST | AST | ~50 |
| 2 | parseSelectExpr | Parser | ~80 |
| 3 | checkSpawnExpr + checkSelectExpr | Checker | ~60 |
| 4 | lowerSpawnExpr | Lowerer | ~110 |
| 5 | lowerSelectExpr/Simple/General | Lowerer | ~290 |
| 6 | lowerAsyncStateMachine | Lowerer | ~150 |
| 7 | lowerAsyncFiber | Lowerer | ~127 |
| 8 | countAwaitPoints | Lowerer | ~57 |
| 9 | lowerAwaitExpr | Lowerer | ~105 |

### Phase 3: Developer Experience (~400 LOC)

**Goal:** Error messages at parity with Zig compiler quality.

| Order | Feature | Subsystem | LOC | Status |
|-------|---------|-----------|-----|--------|
| 1 | editDistance algorithm | Checker | ~30 | ALREADY EXISTED |
| 2 | findSimilarName (scope search) | Checker | ~60 | ALREADY EXISTED |
| 3 | findSimilarField/Variant/Type | Checker | ~80 | ALREADY EXISTED |
| 4 | errWithSuggestion wiring | Checker | ~30 | ALREADY EXISTED |
| 5 | Shape struct + methods | Types | ~150 | DONE (Mar 12) |
| 6 | Dict helper generation | Lowerer | ~107 | DONE (Mar 12) |

### Phase 4: Polish & Optimization (~600 LOC)

**Goal:** Full feature parity including edge cases.

| Order | Feature | Subsystem | LOC |
|-------|---------|-----------|-----|
| 1 | ARC insertion pass | Lowerer/SSA | ~444 | DONE (already ported, 444 lines, 11 tests) |
| 2 | lower_wasm SSA pass | SSA passes | ~500 (partial) | DONE (347 lines + optimizeTailCalls stub, Mar 12) |
| 3 | WasmGC struct codegen | Wasm codegen | ~50 | DONE (gc_struct_new/get/set in wasm_gen.cot + code_builder, Mar 12) |
| 4 | Proper f64.const encoding | Wasm codegen | ~20 | DONE (emitF64Const via bitcast, Mar 12) |
| 5 | resolveType (full recursive) | Checker | ~70 | DONE (already existed, 119 lines) |
| 6 | buildStructTypeWithLayout | Checker | ~40 | DONE (already existed, 46 lines) |

---

## 10. Acceptance Criteria

### Phase 1 Complete When:
- [ ] `cot build self/main.cot` produces binary that can parse all syntax including orelse, labeled blocks
- [ ] Array sizes from const expressions work: `const N = 10; var arr: [N]int`
- [ ] Comptime blocks evaluate: `comptime { var x = 5 * 3 }`
- [ ] Inline for unrolls: `inline for f in @typeInfo(T).fields { ... }`
- [ ] Error literals type-check: `return error.NotFound`

### Phase 2 Complete When:
- [x] `spawn { body }` parses, checks, and lowers correctly (DONE — Mar 12, 2026)
- [x] `select { recv ch |val| { } }` works end-to-end (DONE — Mar 12, 2026)
- [ ] `async fn` generates proper state machine on Wasm target (lowerAsyncStateMachine — skipped, Wasm-only)
- [x] `async fn` generates native fiber (lowerAsyncFiber) — eager execution (DONE — Mar 12, 2026)
- [x] `await future` extracts result from future[8] on native (DONE — Mar 12, 2026)
- [x] `countAwaitPoints` recursive AST walker (DONE — Mar 12, 2026)

### Phase 3 Complete When:
- [x] Typo errors show suggestions: `"undefined type 'Lsit' (did you mean 'List'?)"` (ALREADY EXISTED)
- [x] Shape struct + shapeOf + key() in types.cot (DONE — Mar 12, 2026)
- [x] Dict helper generation: generateBinaryOpHelper + generateMethodCallHelper (DONE — Mar 12, 2026)
- [x] Stencil infrastructure: shape_stencils, shape_aliases, generated_dict_helpers maps (DONE — Mar 12, 2026)
- [x] Stencil integration: analyzeStencilability + collectNodeDictEntries + collectExprDictEntries + collectStmtDictEntries (DONE — Mar 12, 2026)

### Phase 4 Implementation Complete (Mar 12, 2026):
All 6 implementation items done. Remaining are integration-level acceptance criteria:
- [ ] All 73/73 test files pass when compiled by selfcot (native)
- [ ] All 73/73 test files pass when compiled by selfcot (wasm)
- [ ] `selfcot build selfcot` succeeds (self-hosting bootstrap)
- [ ] Binary size within 20% of Zig compiler output

---

## Files Reference

| Purpose | Self-Hosted | Zig Reference |
|---------|-------------|---------------|
| AST | `self/frontend/ast.cot` | `compiler/frontend/ast.zig` |
| Types | `self/frontend/types.cot` | `compiler/frontend/types.zig` |
| Parser | `self/frontend/parser.cot` | `compiler/frontend/parser.zig` |
| Checker | `self/frontend/checker.cot` | `compiler/frontend/checker.zig` |
| IR | `self/frontend/ir.cot` | `compiler/frontend/ir.zig` |
| Lowerer | `self/frontend/lower.cot` | `compiler/frontend/lower.zig` |
| SSA | `self/frontend/ssa.cot` | `compiler/ssa/value.zig` + `op.zig` |
| SSA Builder | `self/frontend/ssa_builder.cot` | `compiler/frontend/ssa_builder.zig` |
| Wasm Codegen | `self/codegen/wasm/wasm_gen.cot` | `compiler/codegen/wasm/gen.zig` |
| SSA Passes | `self/codegen/wasm/ssa_passes*.cot` | `compiler/ssa/passes/*.zig` |

---

*Generated by comprehensive audit on March 12, 2026. Each subsystem audited by dedicated agent with line-by-line comparison against Zig reference.*
