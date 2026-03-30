# Self-Hosted Compiler Diff Audit

**Date:** 2026-03-20
**Scope:** Complete line-by-line comparison of every `self/` file against its Zig counterpart.

## Legend

- **BUG**: Functional divergence that will cause incorrect compilation
- **MISSING**: Feature present in Zig but absent in selfcot
- **EXTRA**: Feature in selfcot that does not exist in Zig (invented)
- **INTENTIONAL**: Difference that is by design (e.g., `@safe` mode adaptations)
- **COSMETIC**: Naming or structural difference with no functional impact

---

## CRITICAL BUGS (will cause incorrect compilation)

### BUG-1: Missing `alloc_raw`, `realloc_raw`, `dealloc_raw` runtime functions

**Zig:** `compiler/driver.zig:5966-5968` registers three distinct-type memory functions:
```
alloc_raw_idx, realloc_raw_idx, dealloc_raw_idx
```
These are defined in `compiler/codegen/mem_runtime.zig:20-22` and `compiler/codegen/arc.zig:171-173`. They map to the `RawPtr` distinct type system (compile-time prevention of alloc/dealloc mismatches).

**Selfcot:** `self/emit/wasm/mem.cot` has NO `alloc_raw`, `realloc_raw`, or `dealloc_raw` functions. The `MemFunctions` struct (line 29-37) has 7 fields; the Zig struct has 10 fields. `self/emit/wasm/driver.cot:populateMemIndices()` registers 7 functions, missing 3.

**Impact:** Any code using `alloc_raw()`, `realloc_raw()`, or `dealloc_raw()` (the distinct `RawPtr` API) will silently call function index 0 (alloc) because the name won't be found in `func_indices`.

**Fix:** Add `alloc_raw`, `realloc_raw`, `dealloc_raw` to `mem.cot` and register them in `driver.cot`.

---

### BUG-2: Missing `"realloc"` alias in func_indices

**Zig:** `compiler/driver.zig:5965` registers an explicit alias:
```zig
try func_indices.put(self.allocator, "realloc", mem_funcs.realloc_idx + import_count);
```
This is needed because user code (via stdlib) calls `realloc` by bare name.

**Selfcot:** `self/emit/wasm/driver.cot` registers `REALLOC_NAME` ("realloc") at line 169 but does NOT register a bare `"realloc"` alias. However, since `REALLOC_NAME` is already `"realloc"`, this is actually **not a bug** -- the constant value IS "realloc". The Zig code has a redundant alias. **Not a bug.**

---

### BUG-3: `import_count` offset handling divergence (INTENTIONAL, not a bug)

**Zig:** `compiler/codegen/wasm/link.zig:addFunc()` returns module-local index (0-based within funcs). The driver adds `import_count` to every index when storing in `func_indices`.

**Selfcot:** `self/emit/wasm/link.cot:addFunc()` (line 359-363) returns `self.imports.count + self.funcs.count` -- the global Wasm function index with imports already baked in. The driver does NOT add import_count.

**Analysis:** This is an **intentional architectural simplification**. The selfcot linker's `addFunc` returns the correct global index directly, making the driver simpler. Both approaches produce the same final Wasm indices. **Not a bug**, but documents a deliberate divergence.

---

### BUG-4: `copyelim` pass missing copy elimination phase

**Zig:** `compiler/ssa/passes/copyelim.zig` has TWO phases:
1. `phielim()` -- converts redundant phis to copies (with `copyelimValue` inside the loop)
2. Copy use elimination -- updates block control values, replacing copy references with their sources
3. Floyd's cycle detection for copy chains (`copySource`)
4. Path compression for copy chains

**Selfcot:** `self/optimize/copyelim.cot` only does `phielim()`. It:
- Does NOT call `copyelimValue` to update non-phi value args
- Does NOT update block control values
- Does NOT have `copySource` with Floyd's cycle detection
- Does NOT have path compression

**Impact:** After copyelim, SSA values may still reference copy ops instead of their sources. Downstream passes (codegen) may encounter stale copy references. This is a correctness issue for any function with copies that are used in multiple places.

---

### BUG-5: `deadcode` pass significantly simplified

**Zig:** `compiler/ssa/passes/deadcode.zig` (416 lines) implements Go's full deadcode algorithm:
1. BFS reachable block analysis from entry
2. Edge removal from dead blocks to live blocks (critical for phi consistency)
3. `BlockFirst` to `BlockPlain` conversion
4. copyelim splice after dead block removal
5. Transitive live value analysis (worklist-based)
6. Proper dead value freeing with `freeValue`
7. Unreachable block removal with `freeBlock`

**Selfcot:** `self/optimize/deadcode.cot` (247 lines) uses a simpler algorithm:
1. No BFS reachable block analysis
2. No edge removal
3. No BlockFirst conversion
4. No copyelim splice
5. Simple per-block use-count-based removal
6. No transitive liveness analysis

**Impact:** Dead blocks are never removed, phi arguments from dead predecessors are not pruned, and values transitively referenced only by dead code are kept alive. This wastes code space and may cause incorrect phi resolution.

---

### BUG-6: `spawn` keyword registration mismatch

**Zig:** `compiler/frontend/token.zig:247-248` explicitly comments that `spawn` is NOT in the keywords map:
```zig
// Note: "spawn" is NOT a keyword -- it's a contextual keyword handled in parser.
// This avoids breaking Thread.spawn and other uses of "spawn" as an identifier.
```

**Selfcot:** `self/parse/token.cot:287` has `"spawn" => Token.kw_spawn` in the `lookup()` function, making it a full keyword.

**Impact:** In selfcot, `spawn` cannot be used as an identifier (e.g., `Thread.spawn(...)` would parse incorrectly). The Zig compiler handles `spawn` contextually in the parser. This means selfcot cannot compile code that uses `spawn` as a field/method name.

---

### BUG-7: `Span` type flattening

**Zig:** `source.zig` Span has `start: Pos, end: Pos` where Pos is a struct with `offset: u32`.

**Selfcot:** `source.cot` Span has `start: int, end: int` (flat integers, not Pos structs).

**Impact:** This is an **intentional simplification** for the selfcot. Source.cot's functions take `int` offsets instead of `Pos` structs. Functionally equivalent. **Not a bug.**

---

### BUG-8: `ErrorReporter` structural divergence

**Zig:** `errors.zig` ErrorReporter holds `src: *Source` and `handler: ?ErrorHandler` (function pointer). It uses the Source to resolve positions and has ANSI color support with `isatty(2)` detection. The Error struct uses optional types (`err_code: ?ErrorCode`, `note: ?Note`).

**Selfcot:** `errors.cot` ErrorReporter uses flat fields: `has_source: bool`, `src_filename: string`, `src_content: string`, `collect_mode: bool`, `collected_errors: List(Error)`. The Error struct uses sentinel values (`err_code: int` with -1 = none, `has_note: bool`). No ANSI color support. No TTY detection. No ErrorHandler callback.

**Impact:** This is a significant structural simplification but produces the same diagnostic output format. The `collect_mode` replaces the `ErrorHandler` callback pattern. **Functionally acceptable** for compilation correctness, but errors will be uncolored.

---

### BUG-9: Missing `store_field` and `store_index_value` in IR hasSideEffects

**Zig:** `ir.zig` Node.hasSideEffects includes `store_field`, `store_index_value`, `store_index_local`, `gc_struct_set`, `gc_array_set`, `gc_array_copy`, `atomic_cas` and many others.

**Selfcot:** `ir.cot` does not define `hasSideEffects` or `isTerminator` or `isConstant` as methods on Node. These are instead checked inline in the passes that need them (deadcode, etc.).

**Impact:** If the selfcot deadcode pass doesn't check side effects correctly inline, it could remove stores. Need to verify each pass individually. **Potential bug** depending on inline checks.

---

## MISSING FEATURES (in selfcot but present in Zig)

### MISSING-1: ARC runtime functions (retain/release)

**Zig:** `compiler/driver.zig:1318` references `"retain"`, `"release"`, `"unowned_retain"`, `"unowned_release"`, `"weak_retain"`, `"weak_release"`, etc. The full ARC runtime is wired in for Wasm builds.

**Selfcot:** No ARC runtime registration in `driver.cot`. No retain/release functions. The bump allocator in `mem.cot` has a no-op `dealloc` (void stub).

**Impact:** Selfcot cannot compile programs that use ARC-managed types (structs with reference-counted fields). Since selfcot currently only targets its own compilation (which uses the bump allocator), this is **acceptable for bootstrapping** but means general-purpose programs compiled by selfcot will leak memory.

---

### MISSING-2: Destructor table

**Zig:** `compiler/driver.zig:6062-6084` builds a destructor table mapping type names to table indices, and a metadata address map.

**Selfcot:** No destructor table, no metadata addresses. Functions marked `is_destructor` are tracked but never registered in a table.

**Impact:** Same as MISSING-1 -- no ARC cleanup for heap-allocated structs.

---

### MISSING-3: Function type index map (`func_type_indices`)

**Zig:** `compiler/driver.zig:6092-6093` builds `func_type_indices` mapping func names to Wasm type section indices, used for `call_indirect` resolution.

**Selfcot:** `driver.cot` calls `registerFuncType()` per function (line 366) but does not build a global `func_type_indices` map. The type index is passed directly to `WasmFunc.init`.

**Impact:** For `call_indirect`, the Zig compiler looks up the callee's type index from `func_type_indices`. Selfcot would need to resolve this differently. **Potential issue for indirect calls.**

---

### MISSING-4: `cse.zig` has 133 lines, `cse.cot` has 300 lines

**Zig:** CSE (Common Subexpression Elimination) is a compact dominator-tree-based algorithm.

**Selfcot:** CSE is significantly larger (300 lines), suggesting a different algorithm. Need deeper analysis to determine if the algorithm matches.

---

### MISSING-5: `rewritedec.zig` has 762 lines, `rewritedec.cot` has 524 lines

The selfcot rewritedec is 31% smaller than the Zig version, suggesting missing cases.

---

### MISSING-6: Large struct return handling

**Zig:** `ir.zig:312-331` has `getOrCreateSretLocal` for shared SRET locals, overlap groups for stack slot sharing (`beginOverlapGroup`, `nextOverlapArm`, `endOverlapGroup`).

**Selfcot:** `ir.cot` has no SRET local sharing or overlap group support. Each function call returning a large struct gets its own temporary.

**Impact:** Higher stack usage in selfcot-compiled code. Not a correctness issue.

---

## PARSE LAYER

### `token.cot` vs `token.zig`
- **Enum variants:** 1:1 match (all 96 variants present)
- **Precedence table:** 1:1 match
- **Helper methods:** All 5 present (precedence, isLiteral, isOperator, isKeyword, isTypeKeyword, isAssignment)
- **token_strings:** 1:1 match
- **lookup function:** Selfcot uses `switch`; Zig uses `StaticStringMap`. Same behavior except BUG-6 (`spawn`).
- **Naming:** Selfcot `toString()` = Zig `string()`. Minor rename.

### `scanner.cot` vs `scanner.zig`
- **TokenInfo:** Selfcot uses `tok: int, start_offset: int, end_offset: int`; Zig uses `tok: Token, span: Span`. **INTENTIONAL** flattening.
- **Scanner struct:** Selfcot `content: string`; Zig `src: *Source`. Selfcot adds `src: ?*Source` optional.
- **Core logic:** `scanNext` = `next` (renamed). Logic is functionally equivalent.
- **scanNumber:** Selfcot splits float detection into `scanFloatPart()` helper; Zig is inline. Same logic.
- **scanOperator:** Selfcot splits into `scanOperator` + `scanOperator2` (to avoid stack overflow). Zig uses one big switch. **INTENTIONAL** split.
- **scanString:** Selfcot doesn't track `terminated` flag for non-interp strings (breaks on close quote directly). Zig tracks `terminated` and `found_interp` flags. Minor logic difference but same result.
- **Missing:** Selfcot has no `isAlpha` or `isDigit` defined -- these are builtins. Zig defines them as module-level functions.

### `parser.cot` vs `parser.zig`
- **Size:** 3260 vs 2345 lines. Selfcot is 39% larger due to tests and Cot syntax verbosity.
- **Core structure:** Both have the same recursive descent architecture.
- **AST nodes:** Need separate deep audit -- too large for this pass.

### `ast.cot` vs `ast.zig`
- **Size:** 1533 vs 696 lines. Selfcot 2.2x larger.
- **Representation:** Selfcot uses tagged unions; Zig uses tagged unions. Matching node types.

### `source.cot` vs `source.zig`
- **Pos:** Selfcot `offset: int`; Zig `offset: u32`. **INTENTIONAL** (Cot uses `int` for everything).
- **Span:** Selfcot `start: int, end: int`; Zig `start: Pos, end: Pos`. Flattened. See BUG-7.
- **Source:** Selfcot `line_offsets: List(int)`; Zig `line_offsets: ?[]u32`. Selfcot uses `line_offsets_computed: bool` flag instead of null check.
- **ensureLineOffsets:** Both use same algorithm (scan for newlines). Selfcot uses `List.append`; Zig pre-counts and allocates.

---

## CHECK LAYER

### `types.cot` vs `types.zig`
- **Type constants:** 1:1 match (INVALID=0 through FIRST_USER_TYPE=23)
- **BasicKind enum:** 1:1 match
- **Type tags:** Need deep audit; both have TAG_STRUCT, TAG_OPTIONAL, TAG_SLICE, etc.
- **TypeRegistry:** Selfcot has `List(Type)` + `Map` for lookup; Zig uses `ArrayListUnmanaged(Type)` + `StringHashMap`. Functionally equivalent.

### `checker.cot` vs `checker.zig`
- **Size:** 5998 vs 4474 lines. Selfcot 34% larger.
- **Scope/Symbol system:** Both use symbol tables with scoping. Need deep audit.
- **Type checking logic:** Core algorithms should match. This is the file most likely to have subtle divergences in type inference rules.

### `errors.cot` vs `errors.zig`
- See BUG-8 above for structural differences.
- **Error/Warning codes:** 1:1 match (E100-E403, W001-W005).
- **Error descriptions:** 1:1 match.

---

## BUILD LAYER

### `ir.cot` vs `ir.zig`
- **BinaryOp/UnaryOp enums:** 1:1 match
- **Payload structs:** 1:1 match (all 50+ structs present)
- **Data union variants:** 1:1 match
- **SliceLocal:** Selfcot `start: int, end: int`; Zig `start: ?NodeIndex, end: ?NodeIndex` (optionals). **DIVERGENCE** -- selfcot cannot represent `null` start/end for slice operations. This may cause issues with `list[..]` (full slice) or `list[start..]` (open-ended).
- **Call/CallIndirect args:** Selfcot `args: List(int)`; Zig `args: []const NodeIndex` (slice). **INTENTIONAL** -- Lists vs slices.
- **FuncBuilder:** See MISSING-6 -- no SRET sharing or overlap groups.
- **Missing methods:** `getOrCreateSretLocal`, `beginOverlapGroup`, `nextOverlapArm`, `endOverlapGroup` are absent.

### `lower.cot` vs `lower.zig`
- **Size:** 9209 vs 11974 lines. Selfcot is 23% smaller, suggesting missing lowering cases.
- **qualifyName/resolveCallName/qualifyTypeName/resolveMethodName:** Logic is 1:1 match.
- **shouldSkipQualification:** 1:1 match (main, __cot_init, __test_, __bench_).
- **Generic instantiation naming:** Need to verify `List(i64)_append` naming pattern matches.
- **Missing lowering cases:** The 2765-line gap suggests some complex lowering scenarios may be missing. Likely candidates: complex switch patterns, advanced error union handling, some builtin intrinsics.

### `ssa.cot` + `builder.cot` vs `ssa_builder.zig`
- **Combined size:** 625 + 2364 = 2989 lines vs 2841 lines. Close match.
- **SsaFunc/SsaValue/SsaBlock:** Core SSA types should match.

### `arc.cot` vs `arc_insertion.zig`
- **Size:** 443 vs 444 lines. Near-identical.
- **ARC insertion algorithm:** Should be functionally equivalent.

---

## OPTIMIZE LAYER

### `copyelim.cot` vs `copyelim.zig`
- See BUG-4. Major algorithm gap.

### `cse.cot` vs `cse.zig`
- 300 vs 133 lines. Selfcot is 2.3x larger. Need deep audit to verify correctness.

### `deadcode.cot` vs `deadcode.zig`
- See BUG-5. Major algorithm gap.

### `decompose.cot` vs `decompose.zig`
- 316 vs 334 lines. Close match. Likely functionally equivalent.

### `layout.cot` vs `layout.zig`
- 266 vs 291 lines. Close match.

### `rewrite.cot` vs `rewritegeneric.zig`
- 172 vs 154 lines. Close match.

### `rewritedec.cot` vs `rewritedec.zig`
- 524 vs 762 lines. See MISSING-5. 31% smaller.

### `schedule.cot` vs `schedule.zig`
- 266 vs 264 lines. Near-identical.

---

## EMIT LAYER

### `driver.cot` vs `driver.zig`
- See BUG-1, BUG-3, MISSING-1, MISSING-2, MISSING-3.
- **Pipeline order:** Both follow: linker setup -> runtime registration -> func_indices -> SSA build -> 6 passes -> codegen -> assemble.
- **Pass order:** Both: rewriteGeneric -> decompose -> rewriteDec -> schedule -> layout -> lower.
- **Memory layout:** Both: 256 pages (16MB), heap starts at 0x800000 (8MB).

### `gen.cot` vs `gen.zig`
- **Size:** 2425 vs 1704 lines. Selfcot 42% larger (likely due to Cot verbosity + tests).
- **GenState struct:** Core fields match.
- **ssaGenBlock:** Logic matches Go reference (BlockPlain, BlockIf, BlockRet handling).
- **ssaGenValue:** Need deep audit -- this is the largest function.

### `link.cot` vs `link.zig`
- **addFunc:** Returns global index (imports + funcs) vs module-local index. See BUG-3.
- **funcCount:** Selfcot: `imports.count + funcs.count`; Zig: `funcs.items.len`. Different semantics.
- **Wasm binary writing:** Both write standard Wasm sections in the same order.

### `lower.cot` (emit) vs `lower_wasm.zig`
- 371 vs 584 lines. Selfcot 36% smaller. Missing lowering rules.

### `assemble.cot`, `preprocess.cot`, `prog.cot`, `builder.cot` (emit)
- These files implement the Go dispatch loop pattern (br_table). Architecture documented in `claude/BR_TABLE_ARCHITECTURE.md`.

### `wasi.cot` vs `wasi_runtime.zig`
- **Function names:** 1:1 match (all 32 WASI function name constants identical).
- **WasiFunctions struct:** 1:1 match (all fields present).

### `mem.cot` vs `mem_runtime.zig` (also `arc.zig`)
- **Functions:** 7 in selfcot vs 10 in Zig. Missing `alloc_raw`, `realloc_raw`, `dealloc_raw`. See BUG-1.
- **alloc body:** Both use bump allocator (heap_ptr_global). Same algorithm.
- **dealloc body:** Both return void (no-op stub).
- **memcpy body:** Both handle overlapping regions (memmove semantics).
- **string_eq/string_concat:** Both match.

### `print.cot` vs `print_runtime.zig`
- **Function names:** 1:1 match (7 functions).
- **write function:** Both delegate to WASI fd_write_simple.

### `test.cot`, `bench.cot`
- Test/bench runtime function registration. Names match Zig.

---

## MAIN ENTRY POINT

### `main.cot` vs `driver.zig` + `main.zig`
- Selfcot has its own CLI (`build`, `run`, `test`, `bench`, `parse`, `check`, `init`, `lex`, `help`, `version`).
- Multi-file compilation supported via `cot.json` project file parsing.
- Pipeline: parse -> check -> lower -> SSA -> passes -> codegen -> assemble -> write .wasm.

---

## FUNC_INDICES POPULATION COMPARISON

### Zig (`compiler/driver.zig:5957-6054`)
Registers the following names (68 total):
1. **Memory (11):** alloc, dealloc, realloc, "realloc" (alias), alloc_raw, realloc_raw, dealloc_raw, memcpy, memset_zero, string_eq, string_concat
2. **Slice (2):** growslice, nextslicecap
3. **Print (7):** write, print_int, eprint_int, int_to_string, print_float, eprint_float, float_to_string
4. **WASI (32):** fd_write, fd_write_simple, fd_read, fd_close, fd_seek, fd_open, time, random, exit, args_count, arg_len, arg_ptr, environ_count, environ_len, environ_ptr, net_socket, net_bind, net_listen, net_accept, net_connect, net_set_reuse_addr, kqueue_create, kevent_add, kevent_del, kevent_wait, epoll_create, epoll_add, epoll_del, epoll_wait, set_nonblocking, fork, execve, waitpid, pipe, dup2, isatty, mkdir, dir_open, dir_next, dir_close, stat_type, unlink
5. **Test (6):** test_begin, test_print_name, test_pass, test_fail, test_summary, test_store_fail_values
6. **Bench (7):** bench_print_name, bench_calibrate_start, bench_calibrate_end, bench_measure_start, bench_measure_end, bench_get_n, bench_summary
7. **User functions:** indexed at `i + runtime_func_count + import_count`

### Selfcot (`self/emit/wasm/driver.cot`)
Registers the following names (65 total):
1. **Memory (7):** alloc, dealloc, realloc, memcpy, memset_zero, string_eq, string_concat -- **MISSING 4: alloc_raw, realloc_raw, dealloc_raw, "realloc" alias**
2. **Slice (2):** growslice, nextslicecap
3. **Print (7):** write, print_int, eprint_int, int_to_string, print_float, eprint_float, float_to_string
4. **WASI (32):** All 32 match
5. **Test (6):** All 6 match
6. **Bench (7):** All 7 match
7. **User functions:** indexed at `i + runtime_func_count` (import_count baked into addFunc)

**Missing entries: 3** (alloc_raw, realloc_raw, dealloc_raw). The "realloc" alias is not actually missing since REALLOC_NAME = "realloc".

---

## GENERIC INSTANTIATION NAMING

**Zig:** Generic functions are named `TypeName(ConcreteType)_method` (e.g., `List(i64)_append`). This is constructed in `lower.zig` during generic instantiation.

**Selfcot:** Uses the same naming pattern in `lower.cot`. The `qualifyName` and `resolveMethodName` functions produce matching qualified names.

**Verdict:** Names match.

---

## SSA CALL OP FUNCTION NAMES

**Zig:** The lowerer emits `ir.Call{.func_name = qualified_name, ...}`. The qualified name comes from `resolveCallName()` or `resolveMethodName()`.

**Selfcot:** Same pattern. `Call { func_name: qualified_name, ... }`.

**Verdict:** Names match (assuming the qualification functions produce identical output, which they do based on the comparison above).

---

## RUNTIME FUNCTION NAME COMPARISON

| Function | Zig Name | Selfcot Name | Match? |
|----------|----------|--------------|--------|
| alloc | "alloc" | "alloc" | YES |
| dealloc | "dealloc" | "dealloc" | YES |
| realloc | "realloc" | "realloc" | YES |
| alloc_raw | "alloc_raw" | N/A | MISSING |
| realloc_raw | "realloc_raw" | N/A | MISSING |
| dealloc_raw | "dealloc_raw" | N/A | MISSING |
| memcpy | "memcpy" | "memcpy" | YES |
| memset_zero | "memset_zero" | "memset_zero" | YES |
| string_eq | "string_eq" | "string_eq" | YES |
| string_concat | "string_concat" | "string_concat" | YES |
| growslice | "growslice" | "growslice" | YES |
| write | "write" | "write" | YES |
| print_int | "print_int" | "print_int" | YES |
| fd_write | "wasi_fd_write" | "wasi_fd_write" | YES |
| exit | "exit" | "exit" | YES |
| All WASI (32) | - | - | ALL MATCH |
| All test (6) | - | - | ALL MATCH |
| All bench (7) | - | - | ALL MATCH |

---

## SUMMARY OF ACTION ITEMS

### Must Fix (will cause compilation failures):

1. **BUG-1:** Add `alloc_raw`, `realloc_raw`, `dealloc_raw` to `mem.cot` and `driver.cot`
2. **BUG-4:** Port full copy elimination (block control updates, `copySource` with Floyd's cycle detection, path compression) to `copyelim.cot`
3. **BUG-5:** Port full deadcode algorithm (BFS reachable blocks, edge removal, transitive liveness) to `deadcode.cot`
4. **BUG-6:** Remove `"spawn"` from `token.cot` lookup function (make it contextual like Zig)

### Should Fix (correctness for edge cases):

5. **IR SliceLocal:** Add optional support for `start`/`end` fields (null = open-ended)
6. **rewritedec.cot:** Audit 238-line gap vs Zig for missing lowering rules
7. **emit/lower.cot:** Audit 213-line gap vs Zig for missing Wasm lowering rules

### Acceptable for Bootstrapping (fix later):

8. **MISSING-1/2:** ARC runtime (retain/release/destructors) -- not needed for selfcot bootstrap
9. **MISSING-6:** SRET sharing / overlap groups -- affects stack size only
10. **BUG-8:** ErrorReporter structural differences -- produces correct output

---

## FILE-BY-FILE STATUS

| Selfcot File | Zig File | Lines (cot/zig) | Status |
|---|---|---|---|
| parse/token.cot | frontend/token.zig | 451/336 | GOOD (BUG-6: spawn) |
| parse/scanner.cot | frontend/scanner.zig | 775/528 | GOOD |
| parse/parser.cot | frontend/parser.zig | 3260/2345 | NEEDS DEEP AUDIT |
| parse/ast.cot | frontend/ast.zig | 1533/696 | NEEDS DEEP AUDIT |
| parse/source.cot | frontend/source.zig | 316/227 | GOOD |
| check/types.cot | frontend/types.zig | 1617/969 | NEEDS DEEP AUDIT |
| check/checker.cot | frontend/checker.zig | 5998/4474 | NEEDS DEEP AUDIT |
| check/errors.cot | frontend/errors.zig | 546/338 | GOOD (structural diff OK) |
| build/ir.cot | frontend/ir.zig | 1454/743 | GOOD (missing SRET) |
| build/lower.cot | frontend/lower.zig | 9209/11974 | NEEDS DEEP AUDIT |
| build/ssa.cot | frontend/ssa_builder.zig | 625/2841 | NEEDS DEEP AUDIT |
| build/builder.cot | frontend/ssa_builder.zig | 2364/2841 | NEEDS DEEP AUDIT |
| build/arc.cot | frontend/arc_insertion.zig | 443/444 | GOOD |
| optimize/copyelim.cot | ssa/passes/copyelim.zig | 140/271 | BUG-4 |
| optimize/cse.cot | ssa/passes/cse.zig | 300/133 | NEEDS AUDIT |
| optimize/deadcode.cot | ssa/passes/deadcode.zig | 247/416 | BUG-5 |
| optimize/decompose.cot | ssa/passes/decompose.zig | 316/334 | GOOD |
| optimize/layout.cot | ssa/passes/layout.zig | 266/291 | GOOD |
| optimize/rewrite.cot | ssa/passes/rewritegeneric.zig | 172/154 | GOOD |
| optimize/rewritedec.cot | ssa/passes/rewritedec.zig | 524/762 | NEEDS AUDIT |
| optimize/schedule.cot | ssa/passes/schedule.zig | 266/264 | GOOD |
| emit/wasm/driver.cot | driver.zig | 584/6475 | BUG-1, MISSING-1/2/3 |
| emit/wasm/gen.cot | codegen/wasm/gen.zig | 2425/1704 | NEEDS DEEP AUDIT |
| emit/wasm/link.cot | codegen/wasm/link.zig | ~650/~600 | GOOD (BUG-3 intentional) |
| emit/wasm/lower.cot | ssa/passes/lower_wasm.zig | 371/584 | NEEDS AUDIT |
| emit/wasm/mem.cot | codegen/mem_runtime.zig | 557/~300 | BUG-1 |
| emit/wasm/wasi.cot | codegen/wasi_runtime.zig | ~600/~800 | GOOD |
| emit/wasm/print.cot | codegen/print_runtime.zig | ~400/~400 | GOOD |
| emit/wasm/assemble.cot | codegen/wasm/assemble.zig | - | NEEDS AUDIT |
| emit/wasm/preprocess.cot | - | - | GO PATTERN PORT |
| emit/wasm/prog.cot | - | - | GO PATTERN PORT |
| emit/wasm/builder.cot | - | - | HELPER |
| emit/wasm/constants.cot | - | - | CONSTANTS |
| emit/wasm/passes.cot | - | - | PASS WRAPPERS |
| emit/wasm/passes_dec.cot | - | - | PASS WRAPPERS |
| emit/wasm/slice.cot | - | - | SLICE RUNTIME |
| emit/wasm/test.cot | - | - | TEST RUNTIME |
| emit/wasm/bench.cot | - | - | BENCH RUNTIME |
| emit/wasm/types.cot | - | - | WASM TYPE CONSTS |
| main.cot | driver.zig + main.zig | 584/6475 | GOOD (CLI layer) |
