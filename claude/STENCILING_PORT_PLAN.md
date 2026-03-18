# Shape Stenciling: Delete Invented Code, Port Go's 3-Tier Model

**Date:** 2026-03-18
**Status:** Implementation plan
**Reference:** `claude/STENCILING_PARITY_AUDIT.md` for full gap analysis

---

## What Go Actually Does

Go's generic code generation has NO body analysis step. The process is:

### Writer Phase (during export/encoding)

1. Start encoding the generic function body with a `writerDict`
2. As expressions are encoded, dictionary entries are added **on demand**:
   - **Method call on type param receiver** (`t.Hash()`) → `typeParamMethodExprs` entry
   - **Nested generic function call** (`G[T](x)`) → `subdicts` entry
   - **Type assertion/conversion involving derived type** → `rtypes` / `itabs` entry
3. **Binary ops (`==`, `<`, `+`) do NOT create dictionary entries.** The shaped type's operators handle these directly. When `T` is shaped to `go.shape.int`, the `==` on shape.int works for all ints.
4. Dictionary entries are deduplicated by checking if they already exist before appending
5. After encoding, the dict's entry counts are known statically

### Reader Phase (during import/instantiation)

1. Read the dictionary definition (entry counts, types, method selectors)
2. **Shapify** all type arguments (collapse to shape types)
3. Build a runtime dictionary global variable (`__dict.pkg.Name[T1,T2]`) with:
   - Method function pointers resolved for concrete types
   - Subdictionary pointers
   - Runtime type descriptors
   - Interface table pointers
4. Generate shaped function with dict param added to signature
5. Generate wrapper function (original signature → tail-call shaped with dict)

### Key Insight: Binary Ops

**Go does NOT use dictionary dispatch for binary operators.** When compiling `a > b` where a, b are type param T shaped to `go.shape.int`:
- The comparison uses the shape type's native operator
- `go.shape.int` supports `>` directly
- No indirect call through dictionary needed
- This works because all types with the same shape have identical operator behavior at the machine level

This is fundamentally different from Cot's approach which tries to create dict helpers like `__cot_dict_i64_gt`.

---

## What Cot Currently Has (Invented, Must Delete)

### Functions to Delete

| Function | Lines | Why Delete |
|----------|-------|------------|
| `analyzeStencilability` | 8458-8493 | Does not exist in Go. Body walk approach is wrong. |
| `collectNodeDictEntries` | 8510-8524 | Body walk helper — invented |
| `collectExprDictEntries` | 8527-8650 | Body walk helper — invented |
| `collectStmtDictEntries` | 8651-8700 (approx) | Body walk helper — invented |
| `typeParamIndex` | 8495-8502 | Matches resolved types — Go uses derived flag |
| `isTypeParamType` | 8504-8506 | Wrapper for typeParamIndex — invented |
| `generateBinaryOpHelper` | 9177-9209 | Go doesn't use dict dispatch for binary ops |
| `dictHelperName` (binary_op branch) | 9105-9112 | Go doesn't use dict dispatch for binary ops |
| `dictOpName` | 9131+ | Names for binary op helpers — invented |

### Data Structures to Delete

| Structure | Why Delete |
|-----------|------------|
| `StencilResult` enum (shape_only/dict_stencil/not_stencilable) | Go has no tiering — every generic gets the same treatment |
| `shape_analysis_cache` | Caches invented body walk results |
| `DictEntry.binary_op` variant | Go doesn't use dict dispatch for binary ops |

### Data Structures to Keep (But Modify)

| Structure | Keep Because | Modify |
|-----------|-------------|--------|
| `DictEntry.method_call` | Go uses `typeParamMethodExprs` for method calls on T | Change detection method (see below) |
| `shape_stencils` | Maps shape keys to canonical stencil names | Keep |
| `shape_aliases` | **Delete** — Go uses wrappers for everything, not SSA aliases | Replace with wrappers |
| `dict_arg_names` | Maps concrete name to dict helper names | Keep |
| `generateDictWrapper` | Go generates wrapper functions | Keep, improve |
| `generateMethodCallHelper` | Go has method trampolines in dict | Keep |
| Dict dispatch in `lowerMethodCall` | Go dispatches method calls through dict | Keep |

### Code to Keep (Correct Patterns)

| Code | Why Keep |
|------|----------|
| Shape struct + fromType + key | Shape key construction (similar to Go's shapify) |
| generateDictWrapper | Wrapper generation (matches Go's callShaped) |
| generateMethodCallHelper | Method trampolines (matches Go's typeParamMethodExprs) |
| Dict dispatch for method calls in lowerMethodCall | Indirect method dispatch through dict params |
| Stencil body lowering with dict params prepended | Similar to Go's shapeSig |

---

## Port Plan: Step by Step

### Step 1: Delete Binary Op Dict Dispatch

Go does NOT use dictionary dispatch for binary operators (`==`, `<`, `+`, etc.). Shape types handle these directly. The machine instructions for `i64 >` and `u64 >` differ (signed vs unsigned), but types with the same shape use the same instruction.

**Delete:**
- `DictEntry.binary_op` variant
- `generateBinaryOpHelper`
- `dictOpName`
- Binary op dict dispatch in `lowerBinaryExpr` (lines 4944-4964)
- `dictHelperName` binary_op branch

**Keep:**
- `DictEntry.method_call` variant
- Method call dict dispatch in `lowerMethodCall`

**Risk:** `max(T)(a, b)` with `a > b` currently uses dict dispatch for `>`. After this change, it will use the direct binary op instruction. For same-shaped types (e.g., i64 and u64), this means both use the SAME comparison instruction — which may be wrong if one is signed and the other unsigned.

**Mitigation:** Cot's shape key includes the `kind` (integer vs float). Signed and unsigned could be different kinds, preventing them from sharing a stencil. Alternatively, if i64 and u64 have the same shape, the stencil body uses the first instantiation's comparison, which would be wrong for the second. This is the same tradeoff Go makes — Go's `go.shape.int` uses signed comparison, and all ints shaped to it must be compatible.

**Cot-specific concern:** Cot distinguishes i64 (signed) and u64 (unsigned) more explicitly than Go. We may need to ensure `Shape.fromType` gives different shapes for signed vs unsigned integers. Check the current Shape implementation.

### Step 2: Delete `analyzeStencilability` and Tiering

Replace the body-walk analysis with Go's model: every generic function with type args gets stenciled.

**Before:**
```zig
stencil_result = analyzeStencilability(f.body, inst_info.type_args);
if (stencil_result.isStencilable()) { ... }
```

**After:**
```zig
// Go model: every generic gets shaped + dict + wrapper
// Dictionary entries determined during body lowering (not before)
const has_type_params = inst_info.type_args.len > 0;
if (has_type_params) { ... stenciling ... }
```

Delete `StencilResult`, `shape_analysis_cache`, and all the `analyzeStencilability` helpers.

### Step 3: Determine Dict Entries During Body Lowering

Port Go's on-demand dictionary entry creation. Instead of pre-analyzing the body, add dict entries AS the body is lowered:

**Method calls on type param receivers:** When `lowerMethodCall` encounters a call on a receiver whose type is derived from a type parameter, add a `typeParamMethodExpr` entry to the current dict.

**How to detect "derived from type param":** Check the AST. For a method call `receiver.method()`:
1. Get the receiver's AST expression
2. If the receiver's declared type (from checker's `expr_types`) was resolved through a type parameter substitution, it's derived
3. Concretely: check if `type_substitution` maps any type param name to the receiver's type, AND the receiver's AST type annotation references that type param name

This replaces `typeParamIndex(tpt, resolved_type)` with AST-based provenance checking.

**Simpler alternative for Cot V1:** Since Cot's lowerer already has `type_substitution` active during body lowering, we can check:
- Is the receiver's type in the VALUES of `type_substitution`?
- AND is the receiver's AST type expression an identifier that's in the KEYS of `type_substitution`?

The second check is the provenance: `self.capacity` has type i64, but its AST type is `i64` (literal in struct definition), not `V` (type param name). So it wouldn't match. But `key` with declared type `K` where K→string WOULD match.

### Step 4: All Stencils Get Wrappers

Delete `shape_aliases` from SSA builder. Every stenciled function gets a wrapper. No more SSA-level call redirection.

**Before (shape_only):**
```zig
// SSA builder redirects call
shape_aliases.put(concrete_name, stencil_name);
// No wrapper generated
```

**After (Go model):**
```zig
// Always generate wrapper
generateWrapper(inst_info, stencil_name, f);
// No shape_aliases needed
```

Remove `shape_aliases` from:
- `Lowerer` struct fields
- `ssa_builder.zig` `convertCall`
- `driver.zig` sharing between lowerers

### Step 5: Verify Against Selfcot

After all changes:
1. `cot test test/e2e/features.cot` — all tests pass
2. `cot test test/e2e/features.cot --target=wasm32` — Wasm passes
3. `cot test test/e2e/shape_stencil.cot` — stencil tests pass
4. Build selfcot: `cot build self/main.cot -o /tmp/selfcot`
5. Test selfcot compilation: all 5 previously-passing files still pass
6. Run `./test/run_all.sh` — no regressions

---

## Detailed Code Changes

### lower.zig Changes

**Delete (approximately 300 lines):**
```
- analyzeStencilability (~35 lines)
- collectNodeDictEntries (~15 lines)
- collectExprDictEntries (~120 lines)
- collectStmtDictEntries (~50 lines)
- typeParamIndex (~7 lines)
- isTypeParamType (~3 lines)
- generateBinaryOpHelper (~32 lines)
- dictOpName (~20 lines)
- StencilResult enum and isStencilable (~10 lines)
- Binary op dict dispatch in lowerBinaryExpr (~20 lines)
- DictEntry.binary_op handling throughout
```

**Modify:**
```
- DictEntry: remove binary_op variant, keep method_call
- Stenciling block (~line 8855): remove analysis, always stencil
- Dict entry detection: move from pre-analysis to during-lowering
- shape_aliases: remove entirely, use wrappers for all
- generateDictWrapper: keep, apply to all stencils (not just dict_stencil)
```

### ssa_builder.zig Changes

**Delete:**
```
- shape_aliases field
- shape_aliases resolution in convertCall (line 1109)
- dict_arg_names field (dict injection removed from SSA builder earlier)
```

### driver.zig Changes

**Delete:**
```
- shape_aliases sharing between lowerers (~15 references)
- Keep dict_arg_names sharing (wrappers still need it)
```

### types.zig Changes

**Verify:**
```
- Shape.fromType gives different shapes for signed vs unsigned ints
- If not, add signedness to shape kind
```

---

## Testing Plan

### New Test Cases Needed

1. **Binary op on T without dict dispatch:** `fn max(T)(a: T, b: T) T { if (a > b) return a; return b }` — must work with direct comparison, not dict dispatch
2. **Method call on T with dict dispatch:** `fn callHash(T)(x: T) i64 { return x.hash() }` — method call should still go through dict
3. **Mixed ops:** Function with both binary ops (direct) and method calls (dict) on T
4. **Selfcot Map:** The primary regression test — Map.getOrNull must work correctly

### Regression Tests

- `test/e2e/features.cot` (native + wasm)
- `test/e2e/shape_stencil.cot` (update tests that rely on binary op dict dispatch)
- `./test/run_all.sh`
- Selfcot: token, source, errors, ast, arc_insertion
