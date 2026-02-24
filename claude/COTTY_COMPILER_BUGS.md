# Cotty Dogfooding — Compiler Bugs Status

Bugs found while building Cotty (cot-land/cotty) with `"safe": true` in cot.json.

---

## Bug 1: @assertEq Fails for Small Types (enum, bool) — FIXED

**File changed:** `compiler/frontend/lower.zig` line ~3334
**Fix:** Changed char literal type from U8 to I64 so enum comparisons use consistent types.
**Audit:** Follows Cot's documented Wasm-first type model (`docs/syntax.md`: char = i64).
**Status:** Fixed and verified. All enum @assertEq tests pass.

---

## Bug 2: Union Switch Fails to Match No-Payload Variants — FIXED

**File changed:** `compiler/frontend/lower.zig` — `resolveUnionVariantIndex`
**Fix:** Extended pattern matching to handle the AST node type used for no-payload union variants in switch arms.
**Audit:** Follows field-access resolution pattern already used for payload variants.
**Status:** Fixed and verified. Union switch patterns now match correctly.

---

## Bug 3: Bool Store via Pointer Fails When Function Has String Parameter — FIXED

**Root cause:** SSA schedule pass (`compiler/ssa/passes/schedule.zig`) was missing IR-level
sized load/store ops (`load8, load16, load32, load64, store8, store16, store32, store64,
load8s, load16s, load32s`) from the memory ordering chain. These ops exist at schedule time
(before `lower_wasm` converts them to Wasm-level ops like `wasm_i64_load8_u`). Without them,
`load8` (reading cfg.flag for the assert) could be scheduled before `static_call`
(doSetString), reading a stale value.

**Files changed:**
- `compiler/ssa/passes/schedule.zig`: Added IR-level sized ops to store chain (store8..store64)
  and load dependency (load8..load64, load8s..load32s).

**Audit:** Go's schedule.go chains ALL memory operations via `MemoryArg()`. Cot doesn't have
explicit memory SSA, so IR-level sized ops must be listed explicitly. The pattern (stores/calls
chain via last_mem, loads get edge from last_mem but don't update it) follows Go's asymmetric
load→store ordering.

**Status:** Fixed and verified. config.cot 8/8 tests pass (was 7/8). 66/66 test suite files pass.

---

## Bug 4: Malformed Wasm Bytecode on app.cot/main.cot — FIXED

**Two sub-bugs found and fixed:**

### Bug 4a: Union switch mixed void/non-void arms

**File changed:** `compiler/frontend/lower.zig` — `lowerUnionSwitch`
**Fix:** Per-arm void checking: before emitting `emitStoreLocal(result_local, ...)` for each
switch arm, check `inferExprType(case.body) != TypeRegistry.VOID`. Void arms (like `@panic()`
or void function calls) skip the result store. Also route ALL union switches through
`lowerUnionSwitch` (not just those with captures) because the generic switch path compares
full union values which fails for multi-word types.
**Audit:** Follows Zig's switch-as-expression pattern where void/diverging arms are handled
individually. The previous "force VOID" hack was removed as it was too aggressive — it
forced entire switches to be void even when only one arm was void/diverging.

### Bug 4b: Struct deref in var declaration triggers wrong call arg decomposition

**File changed:** `compiler/frontend/lower.zig` — `lowerLocalVarDecl`
**Root cause:** The `is_compound_copy` path for `var value = ptr.*` (struct type from
deref/field_access/index) lowered the deref expression, then passed it to
`emitCall("memcpy")`. But `convertPtrLoadValue` in the SSA builder retypes the pointer
value as the struct type, causing `addCallArg` to decompose it into N field loads (one
per 8-byte chunk). This produced wrong memcpy arguments: `memcpy(dst, field0, field1, size)`
instead of `memcpy(dst, ptr, size)`, leaving extra values on the Wasm stack.
**Fix:** Removed the `is_compound_copy` special path. The normal `emitStoreLocal` path handles
large struct copies correctly via `convertStoreLocal` → `OpMove` → `memory.copy`.
**Audit:** Go's SSA uses OpMove for struct copies at IR level; memcpy emerges during lowering.

**Also fixed:** `arg_len` metadata for `move` (3→2) and `wasm_lowered_move` (1→2) in
`compiler/ssa/op.zig`.

**Status:** Fixed and verified. app.cot 10/10 tests pass on wasm32, main.cot 1/1 passes
on wasm32. Native has a separate issue (ARM64 arg register overflow for functions with >16
integer args, not related to this bug).

---

## Bug 5: Optional String Memory Corruption — FIXED

**Files changed:**
- `compiler/frontend/types.zig` line 301: `sizeOf` for optionals was hardcoded to 16.
  Changed to dynamic calculation: `8 + payload_size` (matching the error_union pattern
  already in the same function).
- `compiler/frontend/lower.zig`: 10 sites with hardcoded `16` for optional locals changed
  to use `self.type_reg.sizeOf(...)`.

**Root cause:** `?string` = [tag:8][ptr:8][len:8] = 24 bytes, but `sizeOf` returned 16.
The string length field at offset 16-23 was beyond the 16-byte local allocation.
**Audit:** Follows the error_union pattern already in `sizeOf`. Go uses tag+payload with
dynamic sizing.
**Status:** Fixed and verified. surface.cot 45/45 tests pass.

---

## Overall Status

| Bug | Status | Category | Audit |
|-----|--------|----------|-------|
| Bug 1 | FIXED | Frontend (lower.zig) | Follows Wasm type model |
| Bug 2 | FIXED | Frontend (lower.zig) | Follows existing pattern |
| Bug 3 | FIXED | SSA (schedule.zig) | Follows Go schedule.go principle |
| Bug 4 | FIXED | Frontend + SSA (lower.zig, op.zig) | Follows Go OpMove + Zig switch pattern |
| Bug 5 | FIXED | Frontend (types.zig, lower.zig) | Follows existing error_union pattern |

### Cotty Per-File Status (Feb 25, 2026)

| File | Lines | Type-check | Tests (native) | Tests (wasm32) | Notes |
|------|-------|-----------|----------------|----------------|-------|
| buffer.cot | 340 | OK | 16/16 pass | 16/16 pass | — |
| config.cot | 176 | OK | 8/8 pass | 8/8 pass | Bug 3 FIXED |
| cursor.cot | 149 | OK | 11/11 pass | 11/11 pass | — |
| input.cot | 195 | OK | 13/13 pass | 13/13 pass | — |
| message.cot | 64 | OK | 5/5 pass | 5/5 pass | — |
| surface.cot | 160 | OK | 45/45 pass | 45/45 pass | — |
| app.cot | 201 | OK | ARM64 arg overflow | 10/10 pass | Bug 4 fixed, native has separate ARM64 issue |
| main.cot | 93 | OK | ARM64 arg overflow | 1/1 pass | Bug 4 fixed, native has separate ARM64 issue |
| **Total** | **1,378** | **8/8** | **98/109** | **109/109** | — |

### Changes Not Yet Committed

- `compiler/ssa/passes/schedule.zig` — Bug 3 (add IR-level sized ops to memory ordering chain)
- `compiler/ssa/op.zig` — Bug 4b (fix arg_len metadata for move/wasm_lowered_move)
- `compiler/frontend/lower.zig` — Bug 1 (char literal I64), Bug 2 (union switch no-payload),
  Bug 4a (per-arm void checking + all unions use lowerUnionSwitch), Bug 4b (remove
  is_compound_copy path), Bug 5 (dynamic optional size)
- `compiler/frontend/types.zig` — Bug 5 (sizeOf for optionals)
- `test/e2e/features.cot` — 2 new tests for Bug 2

### Verification Commands

```bash
zig build test                                          # Compiler internals
cot test test/e2e/features.cot                          # 341 features native
cot test test/e2e/features.cot --target=wasm32          # 341 features wasm
cot test self/main.cot                                  # Self-hosted 142 tests
./test/run_all.sh                                       # Full suite 66/66 files

# Cotty individual files (all except app.cot/main.cot native)
cot test ~/cot-land/cotty/src/config.cot                # 8 tests (Bug 3 fixed!)
cot test ~/cot-land/cotty/src/surface.cot               # 45 tests
cot test ~/cot-land/cotty/src/buffer.cot                # 16 tests
cot test ~/cot-land/cotty/src/input.cot                 # 13 tests
cot test ~/cot-land/cotty/src/cursor.cot                # 11 tests
cot test ~/cot-land/cotty/src/message.cot               # 5 tests
```

### Remaining Issue

**ARM64 arg register overflow** — app.cot/main.cot crash on native due to functions with
>16 integer args hitting an assertion in `genArgSetup`. This is a separate native backend
limitation affecting only the app.cot integration (test harness with many params), not
related to the Wasm codegen bugs fixed here.
