# Shape Stenciling: Cot vs Go Audit & Port Plan

**Date:** 2026-03-18
**Status:** Stenciling disabled (`analyzeStencilability` returns `not_stencilable`). Invented code must be deleted and replaced with Go's actual model.

---

## 🚨 `analyzeStencilability` Does Not Exist in Go

**Cot invented `analyzeStencilability` — a function that walks the AST body to classify generic functions into tiers (shape_only / dict_stencil / not_stencilable). This function does not exist in Go. It is the root cause of all stenciling bugs.**

Go does NOT analyze function bodies to decide whether to stencil. Go's approach:

1. **EVERY** generic function gets shaped + dictionary + wrapper treatment. No classification.
2. Dictionary entries are added **on demand during body encoding** (`writer.go`), not by a separate body walk.
3. **Binary ops (`==`, `<`, `+`) do NOT create dictionary entries.** Shape types handle these directly.
4. Only **method calls on type param receivers** (`t.Hash()`) and **nested generic calls** create dictionary entries.
5. The `derived` flag propagates compositionally through the type system — a type is derived iff it structurally contains a type parameter.

### What Cot Invented (Not In Go)

| Cot Construct | Go Equivalent |
|---------------|---------------|
| `analyzeStencilability()` | **Does not exist** — every generic gets dict+shape+wrapper |
| `StencilResult { shape_only, dict_stencil, not_stencilable }` | **Does not exist** — no tiering |
| `collectExprDictEntries()` / `collectNodeDictEntries()` | **Does not exist** — dict entries added during body encoding |
| `typeParamIndex(tpt, resolved_type)` | **Does not exist** — Go uses `w.derived` flag on types |
| `isTypeParamType()` | **Does not exist** |
| `shape_analysis_cache` | **Does not exist** — no per-body analysis to cache |
| `generateBinaryOpHelper` / `__cot_dict_i64_gt` | **Does not exist** — binary ops use shape type's native operators |
| `DictEntry.binary_op` | **Does not exist** — no dict dispatch for binary ops |

---

## Go's Architecture (Reference)

### Three-Tier System

1. **Shaped functions**: Shared code parameterized by shape. Receives `*[N]uintptr` dictionary as first param (after receiver for methods). One shaped function per unique shape.
2. **Runtime dictionaries**: Global rodata variables (`__dict.pkg.Name[T1,T2]`) containing function pointers, sub-dictionaries, runtime types, and itabs. One per concrete instantiation.
3. **Wrapper functions**: Non-shaped, original-signature functions that construct/load the dictionary and tail-call the shaped variant.

### Key Files
- `noder/writer.go` — `writerDict`, `typIdx` (derived type tracking), on-demand dictionary entry collection
- `noder/reader.go` — `readerDict`, `shapify`, `objDictIdx`, `callShaped`, `shapeSig`

### How Dictionary Entries Are Added (writer.go)

Dictionary entries are created **during body encoding**, not before:

- **`methodExpr()`** (writer.go:2211): When encoding a method call on a type param receiver, calls `dict.typeParamMethodExprIdx()` → appends to `dict.typeParamMethodExprs`
- **`funcInst()`** (writer.go:748): When encoding a call to another generic function with derived type args, calls `dict.subdictIdx()` → appends to `dict.subdicts`
- **`itab()`** (writer.go:2469): When encoding a type conversion involving a derived type, calls `dict.itabIdx()` → appends to `dict.itabs`
- **Binary ops**: `Operation` (writer.go:1968) encodes `exprBinaryOp` with `implicitConvExpr` for operands. **No dictionary entry created.** The shape type handles the operator.

Each `*Idx()` function deduplicates by checking if the entry already exists before appending.

### Why Binary Ops Don't Need Dictionary Dispatch

When compiling `a > b` where a, b are type param T shaped to `go.shape.int`:
- The comparison uses the shape type's native instruction
- `go.shape.int` supports `>` directly via the shape's underlying type
- All types with the same shape have identical operator behavior at the machine level
- No indirect call needed — the shaped function body has a direct `GT` instruction

### Derived Type Tracking (`typIdx` in writer.go)

Go tracks **derived types** compositionally:

```
TypeParam T         → derived = true
*T                  → derived = true (pointer to derived)
[]T                 → derived = true (slice of derived)
struct { x T }      → derived = true (field is derived)
struct { x int }    → derived = false (no type param involvement)
int                 → derived = false (concrete, even if T=int)
```

A type is derived **only** if its definition structurally contains a type parameter. The field type `int` is NEVER derived, even if `T=int` for some instantiation. This is tracked via `w.derived` flag during recursive type encoding.

### Dictionary Layout (reader.go)

```
[0..N)   typeParamMethodExprs  — function pointers for method calls on T
[N..M)   subdicts              — pointers to nested generic function dicts
[M..O)   rtypes                — *runtime._type descriptors
[O..P)   itabs                 — *runtime.itab for interface conversions
```

Passed as single `*[P]uintptr` pointer. Shaped function reads entries at known word offsets.

### Wrapper Generation (`callShaped` in reader.go:1359)

```go
var args ir.Nodes
if r.methodSym != nil {
    args.Append(params[0])  // receiver first
    params = params[1:]
}
args.Append(typecheck.Expr(ir.NewAddrExpr(pos, r.p.dictNameOf(r.dict))))  // &dict
args.Append(params...)  // remaining params
r.syntheticTailCall(pos, shapedFn, args)
```

### Shaped Function Signature (`shapeSig` in reader.go:4034)

```go
params[0] = types.NewField(fn.Pos(), dictParamName, types.NewPtr(dict.varType()))
for i, param := range sig.Params() {
    params[1+i] = param  // shift all params by 1
}
```

Dictionary is first normal parameter (after receiver for methods).

---

## Current Cot Code (What to Keep vs Delete)

### Delete (~300 lines)

| Code | Why |
|------|-----|
| `analyzeStencilability` (lower.zig:8458) | Invented body walk |
| `collectNodeDictEntries` / `collectExprDictEntries` / `collectStmtDictEntries` | Invented body walk helpers |
| `typeParamIndex` / `isTypeParamType` | Matches resolved types instead of tracking provenance |
| `StencilResult` enum | Invented tiering (shape_only/dict_stencil/not_stencilable) |
| `shape_analysis_cache` | Caches invented analysis results |
| `generateBinaryOpHelper` | Go doesn't dict-dispatch binary ops |
| `dictOpName` | Names for invented binary op helpers |
| `DictEntry.binary_op` variant | Go doesn't dict-dispatch binary ops |
| Binary op dict dispatch in `lowerBinaryExpr` (line ~4944) | Invented — shape types handle operators |
| `shape_aliases` (lowerer + ssa_builder + driver) | Go uses wrappers, not SSA alias redirection |

### Keep (Correct Patterns, Modify)

| Code | Go Equivalent | Modify |
|------|---------------|--------|
| `DictEntry.method_call` | `writerDict.typeParamMethodExprs` | Change detection to on-demand during lowering |
| `shape_stencils` map | Shape key → canonical stencil | Keep |
| `dict_arg_names` map | Concrete name → dict helper names | Keep |
| `generateDictWrapper` | `callShaped` | Apply to ALL stencils, not just dict_stencil |
| `generateMethodCallHelper` | `typeParamMethodExprs` entries | Keep |
| Dict dispatch in `lowerMethodCall` (line ~9485) | Dictionary word offset dispatch | Keep |
| `Shape` struct + `fromType` + `key` | `shapify()` + `LinkString` | Consider improving precision |
| Stencil body lowering with dict params | `shapeSig` shaped function | Keep |

---

## Port Plan: Step by Step

### Step 1: Delete Binary Op Dict Dispatch

Go does NOT use dictionary dispatch for binary operators. Shape types handle these directly.

**Delete:**
- `DictEntry.binary_op` variant and all handling
- `generateBinaryOpHelper`
- `dictOpName`
- Binary op dict dispatch in `lowerBinaryExpr` (lines 4944-4964)
- `dictHelperName` binary_op branch

**Signed/unsigned concern:** `max(i64)` and `max(u64)` sharing a stencil would use the same comparison instruction. Ensure `Shape.fromType` gives different shapes for signed vs unsigned integers (different `kind` values). Go handles this — `go.shape.int` vs `go.shape.uint` are different shapes.

### Step 2: Delete `analyzeStencilability` and Tiering

Replace body-walk analysis with Go's model: every generic with type args gets stenciled.

**Before:**
```zig
stencil_result = analyzeStencilability(f.body, inst_info.type_args);
if (stencil_result.isStencilable()) { ... }
```

**After:**
```zig
// Go model: every generic gets shaped + wrapper
// Dict entries determined during body lowering, not before
if (inst_info.type_args.len > 0) { ... always stencil ... }
```

Delete `StencilResult`, `shape_analysis_cache`, and all `analyzeStencilability` helpers.

### Step 3: Determine Dict Entries During Body Lowering

Port Go's on-demand dictionary entry creation. Instead of pre-analyzing the body, add dict entries AS the body is lowered.

**Method calls on type param receivers:** When `lowerMethodCall` encounters a call on a receiver whose type is derived from a type parameter, add a dict entry.

**How to detect "derived from type param":** Check the AST provenance:
1. Get the receiver's AST expression
2. Check if the receiver's declared type annotation references a type parameter name (key in `type_substitution`)
3. `self.capacity` has type i64 but its AST type is `i64` (literal) — NOT derived
4. `key` with declared type `K` where K is in `type_substitution` — IS derived

This replaces `typeParamIndex(tpt, resolved_type)` with AST-based provenance checking.

**Challenge:** Dict entries must be known BEFORE the function body is lowered (to set up dict params). Two approaches:
- **Two-pass:** Walk body once to collect dict entries, then lower with dict params. Simpler than `analyzeStencilability` because only method calls on derived receivers matter (no binary ops).
- **Go's approach:** Encode body, discover entries on demand, then re-encode with correct params. More complex but matches Go exactly.

For Cot V1, the two-pass approach is acceptable: a simple walk that only looks for method calls on derived-type receivers. This is NOT `analyzeStencilability` — it doesn't classify binary ops, doesn't use `typeParamIndex`, and only cares about method calls.

### Step 4: All Stencils Get Wrappers

Delete `shape_aliases` from SSA builder. Every stenciled function gets a wrapper.

Remove `shape_aliases` from:
- `Lowerer` struct fields
- `ssa_builder.zig` `convertCall` (line 1109)
- `driver.zig` sharing between lowerers (~15 references)

### Step 5: Verify Against Selfcot

1. `cot test test/e2e/features.cot` — all tests pass (native + wasm)
2. `cot test test/e2e/shape_stencil.cot` — update tests, all pass
3. `./test/run_all.sh` — no regressions
4. Build selfcot + test 5 frontend files: token, source, errors, ast, arc_insertion

---

## Architectural Differences (Acceptable Divergence)

These differ from Go but work correctly. Migrate when needed.

| Aspect | Go | Cot | When to Migrate |
|--------|----|----|-----------------|
| Dict structure | Single `*[N]uintptr` global rodata | Individual `i64` fn-ptr params | When subdicts/rtypes/itabs needed |
| Dict position | After receiver, before params | Before all params (including self) | When migrating to unified dict |
| Wrapper tail call | `syntheticTailCall` | Full call + return | When `return_call` (Wasm 3.0) available |
| Stack elision | `.SetWrapper(true)` | Not implemented | When debugger support matters |
| Shape key | `Type.Underlying().LinkString()` | `size + alignment + kind` | When shape precision matters |
| Pointer collapsing | `*T → *byte` for basic interface | Not implemented | When trait-based generics mature |

---

## Testing Plan

### Update shape_stencil.cot

Tests that rely on binary op dict dispatch need updating:
- `dict stencil: equality function shares across i64/u64` — will now use direct `==` (no dict)
- `dict stencil: less-than shares across i64/u64` — will now use direct `<`
- `dict stencil: addition/subtraction/multiplication` — will now use direct ops
- `dict stencil: max function with comparison` — direct comparison
- `dict stencil: clamp with two comparison ops` — direct comparisons

These tests should still PASS (same behavior) but will no longer go through dict dispatch.

### New Tests

1. **Method call on T with dict:** `fn callHash(T)(x: T) i64 { return x.hash() }` — must use dict dispatch
2. **Mixed:** Function with binary ops (direct) AND method calls (dict) on same T
3. **Selfcot Map:** `Map.getOrNull` — the primary regression test
