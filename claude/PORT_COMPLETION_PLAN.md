# Port Completion Plan: Fix Every Incomplete File

**Date:** 2026-03-29
**Status:** Execution plan — every file audited, every gap cataloged
**Method:** Line-by-line diff against compiler/ reference. No invention. No stubs.

---

## Root Cause

Previous porting sessions reduced files by 20-40%, dropping cases and leaving TODO stubs.
The lowerer has a literal `// TODO: iterate block statements` that prevents `{ return 42 }` from being lowered. This is the tip of the iceberg — every file smaller than the reference has dropped cases.

## Execution Rules

1. For each file: `diff` the reference function-by-function
2. Port every missing case exactly from the reference
3. Zero TODOs, zero stubs, zero "simplified" logic
4. Adapt only for known API differences (compact AST, `pointer.flags.is_managed`, TypeIndex enum)
5. Run `zig build test` after each file
6. Run `COT_DEBUG=all` driver test to verify pipeline output matches reference

---

## Files to Fix (ordered by impact)

### Priority 0: Blocking — Must fix to produce valid .wasm

#### 1. lower.zig (13,528 → 8,354 = -5,174 lines, 38% missing)
**Reference:** `compiler/frontend/lower.zig`
**Known stubs:**
- Line 890-898: `block_one/block_two/block` — empty, no iteration
- Line 977: `async let` — TODO
- Line 1839: `emitDestroyValue` — TODO
- Line 1919: "Expression lowering — stubs for submodule functions"
- Line 4146: "Type resolution helpers — stubs"
- Line 8123: WasmGC union switch — stub
**Action:** Function-by-function diff against reference. The compact AST means node access patterns differ (`tree.nodeTag(idx)` vs `node.tag`, `tree.fnDeclData(idx)` vs `decl.fn_decl`), but the LOGIC must be 1:1.
**Verification:** `COT_DEBUG=all` output for `fn main() i64 { return 42 }` must match reference: 1 block, kind=ret, 3 values after passes.

#### 2. ssa_builder.zig (2,908 → 1,540 = -1,368 lines, 47% missing)
**Reference:** `compiler/frontend/ssa_builder.zig`
**Known gaps:**
- `convertNode` is missing WasmGC op handlers (currently returns null)
- `convertStoreLocal` simplified — missing WasmGC GC ref store path
- `convertLoadLocal` simplified — missing WasmGC GC ref load path
- `addCallArg` — missing string/slice decomposition for call arguments
- `convertGlobalStore` simplified — missing string/slice/struct bulk store paths
- `reportNoCurrentBlock` — missing from port
**Action:** Diff every convert* function. The init() is already correct (verified).
**Verification:** SSA dump for multi-block function must match reference block count and value ops.

### Priority 1: Required for correct codegen

#### 3. preprocess.zig (710 → 528 = -182 lines, 26% missing)
**Reference:** `compiler/codegen/wasm/preprocess.zig`
**Action:** Diff dispatch loop generation, resume point handling.

#### 4. assemble.zig (762 → 583 = -179 lines, 23% missing)
**Reference:** `compiler/codegen/wasm/assemble.zig`
**Action:** Diff instruction encoding, LEB128 paths.

#### 5. link.zig (865 → 694 = -171 lines, 20% missing)
**Reference:** `compiler/codegen/wasm/link.zig`
**Action:** Diff section emission (data, element, export).

#### 6. lower_wasm.zig (723 → 596 = -127 lines, 18% missing)
**Reference:** `compiler/ssa/passes/lower_wasm.zig`
**Action:** Diff op lowering table, memory threading strip.

#### 7. gen.zig (1,788 → 1,450 = -338 lines, 19% missing)
**Reference:** `compiler/codegen/wasm/gen.zig`
**NOTE:** Agent audit found this is functionally complete — difference is comments only.
**Action:** Verify, no changes expected.

### Priority 2: Required for full feature coverage

#### 8. vwt_gen.zig (1,378 → 1,123 = -255 lines, 18% missing)
**Reference:** `compiler/frontend/vwt_gen.zig`
**Action:** Diff witness function generation for each type category.

#### 9. async_split.zig (727 → 539 = -188 lines, 26% missing)
**Reference:** `compiler/ssa/passes/async_split.zig`
**Action:** Diff state machine splitting logic.

#### 10. rewritedec.zig (768 → 613 = -155 lines, 20% missing)
**Reference:** `compiler/ssa/passes/rewritedec.zig`
**Action:** Diff destructor rewriting patterns.

#### 11. deadcode.zig (506 → 431 = -75 lines, 15% missing)
**Reference:** `compiler/ssa/passes/deadcode.zig`
**Action:** Diff unreachable block removal, value liveness.

#### 12. decompose.zig (347 → 271 = -76 lines, 22% missing)
**Reference:** `compiler/ssa/passes/decompose.zig`
**Action:** Diff compound type decomposition.

#### 13. region_isolation.zig (438 → 350 = -88 lines, 20% missing)
**Reference:** `compiler/ssa/passes/region_isolation.zig`
**Action:** Diff dataflow analysis, partition state.

#### 14. constants.zig (904 → 794 = -110 lines, 12% missing)
**Reference:** `compiler/codegen/wasm/constants.zig`
**Action:** Diff opcode definitions, register constants.

#### 15. prog.zig (270 → 231 = -39 lines, 14% missing)
**Reference:** `compiler/codegen/wasm/prog.zig`
**Action:** Diff Prog chain node types.

### Priority 3: Non-blocking (debug/visualization)

#### 16. html.zig (1,079 → 628 = -451 lines, 42% missing)
**Reference:** `compiler/ssa/html.zig`
**Action:** Diff HTML visualization generation. Not blocking correctness.

---

## Verification Protocol

After fixing each file:
1. `cd src && zig build test` — all library tests pass
2. `COT_DEBUG=all zig build test-driver` — pipeline output matches reference
3. For lower.zig specifically: `wasmtime /tmp/cot_test_output.wasm` must exit 0

After all P0 files:
- Gate test: `fn main() i64 { return 42 }` → valid .wasm → wasmtime runs → exit 0

After all P1 files:
- Gate test: `fn main() { print(42) }` → valid .wasm → wasmtime prints "42"
- Gate test: all `test/cases/*.cot` files pass via driver

---

## Total Work

- P0: ~6,542 missing lines across 2 files (lower.zig, ssa_builder.zig)
- P1: ~659 missing lines across 4 files
- P2: ~987 missing lines across 7 files
- P3: ~451 missing lines across 1 file
- **Total: ~8,639 lines to port from reference**

All of it is mechanical — the reference code exists line-for-line. The only adaptation is compact AST access patterns in lower.zig.
