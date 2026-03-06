# Execution Plan: Port `lower.zig` → `self/frontend/lower.cot`

## Overview

Port `compiler/frontend/lower.zig` (~9,559 lines, 79 functions) to `self/frontend/lower.cot`. This is the AST→IR lowering pass — the bridge between the type-checked AST and the IR representation. It's the largest single file in the backend and the critical dependency for enabling `build`, `run`, and `test` commands in the self-hosted compiler.

**Current status (Mar 6, 2026):** lower.cot is **~97% non-async parity** — 7,031 lines, 20 tests. Only ~223 lines of small helpers remain. See "Progress Update" below.

**Estimated final output**: ~8,000–10,000 lines of Cot (lower.zig is dense Zig; Cot's switch/capture verbosity adds ~15%, but @safe mode removes `&` noise)

**Dependencies satisfied**: ir.cot (1,329 lines, 24 tests) is complete. ast.cot, checker.cot, types.cot, scanner.cot, token.cot, source.cot, errors.cot all exist.

**Blocking language gaps**: None critical. Tagged union switch works. Optional types work. List/Map available. The self-hosted checker.cot (5,316 lines) already demonstrates all patterns needed.

---

## Architecture: How lower.zig Works

The Lowerer walks the AST (from checker) and emits IR nodes (into FuncBuilder). Key flow:

```
AST (from parser/checker) → Lowerer → IR Builder → Func/Block/Node graph
```

**Core loop**: `lowerDecl()` dispatches on AST node type → calls specialized lowering functions → each emits IR via `self.builder.func().emitXxx()`.

**State machine**: The Lowerer maintains:
- `builder: ir.Builder` — the IR being constructed
- `cleanup_stack` — deferred cleanup (ARC releases, defer bodies)
- `loop_stack` — break/continue targets
- `const_values` / `float_const_values` — compile-time constant folding
- `comptime_value_vars` — comptime variable tracking
- `local_map` — name→local index (handled by FuncBuilder in ir.cot)
- `test_names` / `bench_names` — test/bench registration
- `global_error_table` — error set variant→ID mapping
- `lowered_generics` — dedup map for generic instantiations

---

## Phased Execution Plan

### Phase 1: Foundation (Lowerer struct + init + decl dispatch)
**~800 lines | 2–3 sessions**

Create `self/frontend/lower.cot` with:

1. **Lowerer struct** — all 34 fields translated to Cot types:
   - `builder: Builder` (from ir.cot)
   - `tree: Ast` (from ast.cot)
   - `chk: Checker` (from checker.cot)
   - `type_reg: TypeRegistry` (from types.cot)
   - `cleanup_stack: List(CleanupEntry)`
   - `loop_stack: List(LoopInfo)`
   - `const_values: Map(int, int)`
   - `test_names: List(string)`, `bench_names: List(string)`
   - `lowered_generics: Map(string, bool)`
   - etc.

2. **Supporting types** (inside lower.cot):
   - `struct CleanupEntry { kind: int, local_idx: int, type_idx: int }`
   - `struct LoopInfo { cond_block: int, exit_block: int }`

3. **`init()` static function**

4. **`lowerDecl()` top-level dispatch** — switch on AST node types:
   - `fn_decl` → `lowerFnDecl()`
   - `var_decl` → `lowerVarDecl()`
   - `const_decl` → `lowerConstDecl()`
   - `struct_decl` → `lowerStructDecl()`
   - `enum_decl` → `lowerEnumDecl()`
   - `impl_decl` → `lowerImplDecl()`
   - `test_decl` → `lowerTestDecl()`
   - `bench_decl` → `lowerBenchDecl()`

5. **`lowerFnDecl()`** — the core: create FuncBuilder, add params, lower body, call `build()`.

6. **Basic tests**: Lower a simple function with params and return.

**Verification**: `cot test self/frontend/lower.cot` — unit tests on Lowerer init and simple fn lowering.

---

### Phase 2: Statements (if/while/for/assign/return/defer)
**~2,500 lines | 4–6 sessions**

7. **`lowerStmt()` dispatch** — the second-largest dispatcher:
   - `var_decl` → local variable creation
   - `assign` → `emitStoreLocal` / `emitStoreLocalField` / `emitPtrStore`
   - `return_stmt` → `emitRet` with cleanup
   - `if_stmt` → `emitBranch` + blocks
   - `while_stmt` → loop blocks + `emitBranch`
   - `for_stmt` → range/slice iteration (desugars to while)
   - `switch_stmt` → cascading branches or jump tables
   - `defer_stmt` → push to cleanup_stack
   - `break_stmt` / `continue_stmt` → `emitJump` to loop_stack targets
   - `block_stmt` → scope entry/exit + sequential lowering

8. **Cleanup stack**: `emitPendingCleanups()` — walk cleanup_stack in reverse, emit ARC releases and defer bodies before returns/breaks.

9. **Assignment lowering** — the most complex statement type:
   - Simple: `x = val` → `emitStoreLocal`
   - Field: `x.f = val` → `emitStoreLocalField` / `emitStoreField`
   - Index: `x[i] = val` → `emitStoreIndexLocal`
   - Deref: `ptr.* = val` → `emitPtrStore` / `emitPtrStoreValue`
   - Compound: `x += val` → load + binary + store

10. **Tests**: if/else branching, while loops, for-range, variable assignment, defer ordering.

**Verification**: `cot test self/frontend/lower.cot` — tests for each statement type.

---

### Phase 3: Expressions (literals, binary/unary, field access, indexing)
**~2,000 lines | 3–5 sessions**

11. **`lowerExpr()` dispatch** — routes to specialized expression lowerers:
    - Literals: int, float, bool, string, null → `emitConstInt/Float/Bool/Null/Slice`
    - Binary: `a + b` → `emitBinary`
    - Unary: `-x`, `!x` → `emitUnary`
    - Variable ref: `x` → `emitLoadLocal` / `emitGlobalRef`
    - Field access: `x.f` → `emitFieldLocal` / `emitFieldValue`
    - Index: `x[i]` → `emitIndexLocal` / `emitIndexValue`
    - Slice: `x[a..b]` → `emitSliceLocal` / `emitSliceValue`
    - Address-of: `&x` → `emitAddrLocal`
    - Deref: `ptr.*` → `emitPtrLoad` / `emitPtrLoadValue`

12. **`lowerBinaryExpr()`** — token-to-BinaryOp mapping + special cases:
    - Short-circuit `and`/`or` (branch-based, not simple binary)
    - String concat `++` → `emitCall("str_concat", ...)`
    - Comparison chains

13. **Struct init**: `Point { .x = 10, .y = 20 }` → series of `emitStoreLocalField`

14. **Array init**: `[3]int { 1, 2, 3 }` → index stores

15. **Tests**: Each expression type with expected IR output.

**Verification**: `cot test self/frontend/lower.cot`

---

### Phase 4: Complex Expressions (calls, if-expr, switch-expr, closures)
**~2,000 lines | 3–5 sessions**

16. **`lowerCallExpr()`** — function calls:
    - Regular: `foo(a, b)` → `emitCall`
    - Method: `x.method(a)` → synthesize name `Type_method`, prepend self arg
    - Indirect: `fn_ptr(a)` → `emitCallIndirect`
    - Closure: `closure(a)` → `emitClosureCall`
    - Builtin: `@intCast(x)` → `lowerBuiltinCall`

17. **`lowerIfExpr()`** — if-as-expression:
    - Simple: `emitSelect` (ternary)
    - Complex (compound types): block-based with merge

18. **`lowerSwitchExpr()`** — switch-as-expression:
    - Cascading branches
    - Enum dispatch
    - Optional capture in arms

19. **`lowerBuiltinCall()`** — 60+ builtins:
    - Type info: `@sizeOf`, `@alignOf`, `@typeOf`, `@typeInfo`, `@enumLen`
    - Casts: `@intCast`, `@floatCast`, `@ptrCast`, `@intToPtr`, `@ptrToInt`, `@bitCast`, `@truncate`
    - Introspection: `@hasField`, `@intFromEnum`, `@tagName`, `@errorName`
    - Math: `@abs`, `@ceil`, `@floor`, `@sqrt`, `@min`, `@max`
    - Memory: `@string`, `@arcRetain`, `@arcRelease`
    - Safety: `@assert`, `@assertEq`, `@trap`, `@panic`
    - Comptime: `@target`, `@targetOs`, `@targetArch`
    - Atomics: `@atomicLoad`, `@atomicStore`, `@atomicAdd`, `@atomicCAS`, `@atomicExchange`

20. **Closure lowering**: capture detection, environment struct creation.

21. **Tests**: Call expressions, if-expressions returning values, switch-expressions, builtin calls.

**Verification**: `cot test self/frontend/lower.cot`

---

### Phase 5: Error Handling + Generics + Memory
**~1,500 lines | 2–3 sessions**

22. **Error union lowering**:
    - `try expr` → unwrap or propagate
    - `catch |err|` → branch on error tag
    - Error set values → `global_error_table` lookup
    - `errdefer` → cleanup_stack with error-conditional flag

23. **Generic instantiation**:
    - `ensureGenericFnQueued()` — mark for deferred lowering
    - `lowerQueuedGenericFunctions()` — process after all decls
    - `lowered_generics` dedup map

24. **Memory/ARC**:
    - `new Foo { ... }` → alloc + init
    - Automatic retain/release insertion (via cleanup_stack)
    - Slice/list/map operations

25. **Compound optional handling**:
    - `?T` → 16-byte `[tag, payload]`
    - `storeCompoundOptArm`, `storeCompoundOptField`

26. **Tests**: try/catch, generic function instantiation, new expressions.

**Verification**: `cot test self/frontend/lower.cot`

---

### Phase 6: Integration + Test/Bench Runners
**~1,200 lines | 2–3 sessions**

27. **Test runner generation**: `generateTestRunner()` — emit main() that calls all test functions, tracks pass/fail counts.

28. **Bench runner generation**: `generateBenchRunner()` — emit main() with timing loops.

29. **Multi-file lowering**: Wire into `main.cot` command dispatch — add `build` and `test` commands.

30. **`lowerModule()`** — top-level entry: iterate all decls, lower each, process generic queue, generate runner.

31. **Integration tests**: Full programs lowered end-to-end:
    - Simple hello world
    - Function with if/else returning a value
    - Struct with methods
    - Generic function
    - Test block

**Verification**:
```bash
cot test self/frontend/lower.cot     # unit tests
cot test self/main.cot               # full self-hosted tests
```

---

## Zig→Cot Translation Patterns

| Zig Pattern | Cot Equivalent |
|-------------|----------------|
| `const decl = node.asDecl() orelse continue;` | `switch (self.ast.getNode(idx)) { Node.decl \|d\| => { ... }, else => {} }` |
| `if (decl == .fn_decl)` | `switch (d) { Decl.fn_decl \|f\| => { ... }, else => {} }` |
| `try self.builder.emit(...)` | `self.builder.func().emit(...)` (no try — Cot doesn't error on emit) |
| `var list = std.ArrayListUnmanaged(T){};` | `var list: List(T) = .{}` |
| `list.append(alloc, item)` | `list.append(item)` |
| `map.get(key) orelse ...` | `if (map.getOrNull(key)) \|v\| { ... } else { ... }` |
| `@as(u32, @intCast(x))` | Direct int usage (Cot has single `int` type) |
| `defer cleanup()` | `defer cleanup()` (same syntax) |
| `for (items) \|item\|` | `for (item in items)` |
| `self.allocator` | Not needed (Cot has GC/ARC) |

---

## Key Risk Areas

1. **Switch verbosity**: Zig's `if (decl == .fn_decl)` becomes multi-line switch/capture in Cot. ~30% of lower.zig is dispatch logic — this will expand. Mitigate by using helper methods that extract specific node types.

2. **Compound optional**: The 16-byte `[tag, payload]` pattern is intricate. Port the `storeCompoundOpt*` functions carefully — these were the source of 4 bugs in the Zig compiler.

3. **Cleanup stack ordering**: Reverse-order cleanup emission is critical for correctness. Test thoroughly.

4. **Generic instantiation**: The `lowered_generics` dedup + queue processing must match the Zig compiler exactly. This was a source of multi-file bugs.

5. **Builtin dispatch**: 60+ cases in `lowerBuiltinCall`. Consider splitting into sub-functions by category (type ops, math, memory, etc.) for maintainability.

---

## Execution Order (What to Do First)

**Start with Phase 1** — get the Lowerer struct compiling and a single function lowering. This proves the ir.cot API works from the lowerer's perspective.

**Then Phase 3 before Phase 2** — expressions are simpler than statements and are needed by statements (e.g., `if (expr)` needs expression lowering). Getting `lowerExpr()` working for literals and binary ops first makes statement testing easier.

**Then Phase 2** (statements), **Phase 4** (complex expressions), **Phase 5** (error/generics), **Phase 6** (integration).

Actual order: **1 → 3 → 2 → 4 → 5 → 6**

---

## Progress Update (Mar 6, 2026)

**7,031 lines / ~9,967 reference = ~71% ported. 20 tests. ~97% non-async parity.**

### All phases: DONE
- **Phases 1–6** (Foundation through Integration): DONE
- **Phases 0–9b** (Parity phases): DONE
- **Post-parity ports**: lowerArrayInit, lowerTupleInit, lowerUnionInit, lowerStringInit, lowerSliceInit, lowerCompoundOrelse, storeCatchCompound, lowerArrayConcat, lowerSliceConcat, findLabeledLoop, createFuncValue — all DONE

### Remaining gaps:

| Category | Functions | Ref Lines | Status |
|----------|-----------|-----------|--------|
| **Small helpers** | `resolveGenericTypeName` (17L), `resolveTypeArgNode` (33L), `resolveStructFieldAddr` (57L), `maybeRegisterScopeDestroy` (39L), `hasDeferCleanups` (13L), `baseHasCleanup` (16L), `emitComptimeArray` (41L), `isDivOp` (7L) | **~223** | **Ready to port** — straightforward |
| Async/concurrency | `lowerSpawnExpr`, `lowerAsyncStateMachine`, `lowerAsyncFiber`, `countAwaitPoints`, `lowerAwaitExpr` | ~1,170 | **Deferred** — not needed until self-hosted compiler compiles async code |
| WasmGC | `gcChunkIndex`, `gcFieldChunks`, `emitGcDefaultValue`, `emitGcStructNewExpanded` | ~138 | **SKIP** — native only |

**~223 lines of small helpers are the only gap for non-async native parity.** Async (~1,170L) and WasmGC (~138L) are deferrable.

---

## Success Criteria

- [x] `cot test self/frontend/lower.cot` — 20 tests pass
- [x] `cot build self/main.cot -o /tmp/selfcot` — builds successfully
- [ ] `/tmp/selfcot build test/e2e/hello.cot` — self-hosted compiler can compile a program (requires SSA port)
- [x] All existing self-hosted tests continue to pass (266 tests as of 0.3.5)

**Note**: ssa_builder.cot (2,099 lines, 7 tests) and ssa.cot (482 lines, 10 tests) are COMPLETE. arc_insertion.cot (~444 lines) is the remaining backend dependency.
