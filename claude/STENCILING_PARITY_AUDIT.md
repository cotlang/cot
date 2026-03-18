# Shape Stenciling Parity Audit: Cot vs Go

**Date:** 2026-03-18
**Purpose:** 1:1 comparison of Go's dictionary-based generics vs Cot's implementation. Every gap must be closed.

---

## Go's Architecture (Reference)

Go uses a **three-tier system** for generic code generation:

1. **Shaped functions**: Shared code parameterized by shape. Receives `*[N]uintptr` dictionary as first param (after receiver for methods). One shaped function per unique shape.
2. **Runtime dictionaries**: Global rodata variables (`__dict.pkg.Name[T1,T2]`) containing function pointers, sub-dictionaries, runtime types, and itabs. One per concrete instantiation.
3. **Wrapper functions**: Non-shaped, original-signature functions that construct/load the dictionary and tail-call the shaped variant. Marked `.SetWrapper(true)` for stack trace elision.

### Go's Key Files
- `noder/writer.go` — `writerDict`, `typIdx` (derived type tracking), dictionary entry collection
- `noder/reader.go` — `readerDict`, `shapify`, `objDictIdx`, `callShaped`, `shapeSig`

---

## Gap 1: Derived Type Tracking (CRITICAL — Root Cause of False Positives)

### Go's Approach (`typIdx` in writer.go)

Go tracks **derived types** — types that transitively contain type parameters. This is compositional:

```
TypeParam T         → derived = true
*T                  → derived = true (pointer to derived)
[]T                 → derived = true (slice of derived)
struct { x T }      → derived = true (field is derived)
struct { x int }    → derived = false (no type param involvement)
int                 → derived = false (concrete)
```

Go checks `w.derived` flag during recursive type encoding. A type is derived **only** if its definition structurally contains a type parameter. The field type `int` is NEVER derived, even if `T=int` for some instantiation.

### Cot's Approach (`typeParamIndex` in lower.zig:8495)

Cot checks whether an expression's **resolved type** matches a concrete type parameter value:

```zig
fn typeParamIndex(type_param_types: []const TypeIndex, ty: TypeIndex) ?u8 {
    for (type_param_types, 0..) |tpt, i| {
        if (ty == tpt) return @intCast(i);
    }
    return null;
}
```

This compares `ty == tpt` — resolved TypeIndex equality. For `Map(string, i64)` with `V=i64`:
- `self.capacity` has type i64
- `typeParamIndex([string_type, i64_type], i64)` → returns 1 (matches V!)
- `self.capacity == 0` gets flagged as V-dependent → **FALSE POSITIVE**

### Fix Required

Replace type-matching with **AST provenance tracking**. Port Go's `w.derived` pattern:

1. During type checking (or a separate analysis pass), mark each AST expression node as "derived from type param" or "concrete".
2. An expression is derived if:
   - It directly references a parameter declared with type `T` (where T is a type parameter name)
   - It dereferences a derived expression (`kp.*` where `kp: *K`)
   - It's a field access on a derived expression where the field type contains a type param
   - It's the return value of a function whose return type is derived
3. An expression is NOT derived if:
   - Its type is concrete regardless of accidental type equality (e.g., `self.capacity: i64` even when `V=i64`)
   - It's a literal
   - It's a field access on a concrete struct field

**Implementation approach**: Add a `derived_exprs: std.AutoHashMap(NodeIndex, u8)` to the checker or lowerer. During the type-checking pass with `type_substitution` active, mark expressions whose types trace through type parameter names. `collectExprDictEntries` then checks `derived_exprs.get(bin.left)` instead of `typeParamIndex(tpt, resolved_type)`.

---

## Gap 2: Dictionary Structure

### Go's Dictionary

Go's runtime dictionary is a global rodata array of `uintptr` words with four sections:

| Section | Content | Purpose |
|---------|---------|---------|
| typeParamMethodExprs | Function pointers to method implementations | Method calls on type params (e.g., `t.Hash()`) |
| subdicts | Pointers to nested dictionaries | Calling other generic functions from within generic code |
| rtypes | `*runtime._type` descriptors | Type assertions, reflect, interface conversions |
| itabs | `*runtime.itab` pointers | Concrete→interface conversions |

Dictionary layout (word offsets):
```
[0..N)   typeParamMethodExprs
[N..M)   subdicts
[M..O)   rtypes
[O..P)   itabs
```

Passed as `*[P]uintptr` — first parameter (after receiver for methods).

### Cot's Dictionary

Cot passes individual function pointer addresses as separate parameters:

```zig
// DictEntry = { kind: binary_op|method_call, op, method_name, type_param_idx }
// Each dict entry → one i64 parameter (funcaddr)
```

No unified dictionary structure. No subdicts, rtypes, or itabs.

### Fix Required

For V1, Cot's approach (individual fn-ptr params) is simpler and works for the current feature set (binary ops + method calls on T). However, for parity:

1. **subdicts**: Not needed until nested generic calls exist in stenciled bodies (Cot doesn't have this pattern yet)
2. **rtypes**: Not needed until type assertions on T or reflect (Cot doesn't have reflect yet)
3. **itabs**: Not needed until interface conversions on T (Cot has basic trait support)

**Decision**: Keep individual fn-ptr params for now. Migrate to unified dict struct when subdicts/rtypes/itabs become necessary. Document this as intentional divergence.

---

## Gap 3: Shaped Function Signature

### Go's Approach (`shapeSig` in reader.go:4034)

Go modifies the function signature:
1. Keep receiver as receiver
2. Add `*[N]uintptr` dictionary as **first normal parameter**
3. Shift all other parameters by 1

```go
params[0] = types.NewField(fn.Pos(), dictParamName, types.NewPtr(dict.varType()))
for i, param := range sig.Params() {
    params[1+i] = param
}
```

### Cot's Approach (lower.zig:8950-8962)

Cot prepends dict params **before all other params** (including self for methods):

```zig
if (stencil_result == .dict_stencil) {
    for (dict_entries, 0..) |_, di| {
        dict_params[di] = try fb.addParam("__dict_N", I64, 8);
    }
}
for (f.params) |param| { ... } // self + regular params after dict
```

### Comparison

| Aspect | Go | Cot |
|--------|----|----|
| Dict position | After receiver, before params | Before everything (including self) |
| Dict type | Single `*[N]uintptr` | N individual `i64` values |
| Receiver handling | Stays as receiver | Mixed in with params |

### Fix Required

Cot's approach works but differs from Go. The wrapper generation already accounts for this ordering. No change needed unless we migrate to unified dict struct (Gap 2).

---

## Gap 4: Wrapper Function Generation

### Go's Approach (`callShaped` in reader.go:1359)

Go generates wrapper functions that:
1. Have the **original signature** (no dict param)
2. Construct/load the runtime dictionary
3. Build args: `receiver, &dict, original_params...`
4. **Tail-call** the shaped function
5. Set `.SetWrapper(true)` for stack trace elision

```go
var args ir.Nodes
if r.methodSym != nil {
    args.Append(params[0])  // receiver
    params = params[1:]
}
args.Append(typecheck.Expr(ir.NewAddrExpr(pos, r.p.dictNameOf(r.dict))))  // &dict
args.Append(params...)  // remaining params
r.syntheticTailCall(pos, shapedFn, args)
```

### Cot's Approach (`generateDictWrapper` in lower.zig:9019)

Cot generates wrapper functions that:
1. Have the original signature
2. Emit `funcaddr` for each dict helper
3. Build args: `dict_0, dict_1, ..., sret?, original_params...`
4. Call the stencil function (not tail-call — full call + ret)

### Gaps

| Aspect | Go | Cot | Gap |
|--------|----|----|-----|
| Tail call | `syntheticTailCall` | Full call + return | Minor perf (not correctness) |
| Stack elision | `.SetWrapper(true)` | Not implemented | Minor DX |
| Dict construction | Load `&dict` global address | Emit individual `funcaddr` | Architectural (see Gap 2) |
| safeWrapType for generic params | N/A (Go has no @safe mode) | **Missing `is_substituted` check** | **BUG — FIXED in this session** |

The `is_substituted` check was missing in the wrapper — generic type params like `key: K` with K=string were being safeWrapType'd to `*string` in the wrapper but left as `string` (value) in the stencil body. Fixed by mirroring the stencil body's is_substituted logic.

---

## Gap 5: Shape Key Construction

### Go's Approach (`shapify` in reader.go:891)

Go uses `Type.Underlying().LinkString()` as the shape key, with special handling:
- Pointers to basic-interface-constrained types → all collapse to `*byte`
- Long type names → hash to bound symbol length
- Shape types live in `go.shape.*` package

### Cot's Approach (lower.zig:8869-8886)

Cot builds shape keys from `Shape.fromType()`:

```zig
const shape = types.Shape.fromType(self.type_reg, type_arg);
const sk = shape.key();
```

Where `Shape` is:
```zig
pub const Shape = struct {
    size: u32,
    alignment: u32,
    kind: enum { integer, float, pointer, composite },

    pub fn fromType(reg: *TypeRegistry, ty: TypeIndex) Shape { ... }
    pub fn key(self: Shape) ShapeKey { ... }
};
```

### Gaps

| Aspect | Go | Cot |
|--------|----|----|
| Shape basis | Underlying type's LinkString | Size + alignment + kind |
| Pointer collapsing | `*T → *byte` for basic interface | Not implemented |
| Shape package | `go.shape.*` namespace | No namespace |
| Hash for long names | Yes (`#65030`) | No |

### Fix Required

Cot's shape keys are simpler but may be too aggressive — two types with the same size/alignment/kind but different field layouts would share a stencil. Go's approach using LinkString is more precise. Consider using type structure hash instead of just size+alignment+kind.

However, for current usage this works correctly because shape_only stencils don't access fields differently, and dict_stencil bodies use dict dispatch for type-dependent ops.

---

## Gap 6: SSA Builder Alias Resolution

### Go's Approach

Go doesn't resolve aliases in an SSA builder — the wrapper/shaped function split is handled at the IR level. Call sites call the wrapper; the wrapper calls the shaped function. No alias map needed.

### Cot's Approach (ssa_builder.zig:1107-1124)

Cot has `shape_aliases` consulted in `convertCall`:

```zig
const resolved_name = if (self.shape_aliases) |aliases|
    aliases.get(func_name) orelse func_name
else func_name;
```

This redirects aliased calls to the canonical stencil. But for dict_stencil, this bypasses the wrapper (which handles dict injection).

### Current Status

Dict injection was removed from SSA builder (correct — wrappers handle it). Shape aliases are only populated for `shape_only` stencils (correct — dict_stencil uses wrappers).

### Remaining Issue

Shape_only aliases redirect calls in the SSA builder to a stencil whose function body was lowered for a DIFFERENT concrete instantiation. If the function body's behavior differs between concrete types (even without type-dependent ops), this can cause incorrect behavior.

**Root cause**: Cot's shape_only aliases redirect at the SSA level, but Go handles this at the IR level with actual wrapper functions. Even for shape_only, Go generates a wrapper that tail-calls the shaped function — there's no alias substitution.

**Fix**: Generate wrapper functions for shape_only stencils too (not just dict_stencil). Remove shape_aliases from SSA builder entirely. All stenciling goes through wrappers.

---

## Gap 7: Dict Dispatch in Function Body

### Go's Approach

Go's shaped functions access dictionary entries by **word offset** from the dictionary parameter:

```go
// Read method function pointer from dictionary
word := dict_param[dict.typeParamMethodExprsOffset() + idx]
fn_ptr := (*func())(word)
fn_ptr(args...)
```

The dictionary parameter is a single pointer. All entries are at known offsets.

### Cot's Approach (lower.zig:4944-4964, 9485-9544)

Cot's dict dispatch loads individual dict params by local index:

```zig
// Binary op dispatch
const fn_ptr = try fb.emitLoadLocal(params[di], TypeRegistry.I64, bin.span);
var call_args = [_]ir.NodeIndex{ left, right };
return try fb.emitCallIndirect(fn_ptr, &call_args, result_type, bin.span);

// Method call dispatch
const fn_ptr = try fb.emitLoadLocal(params[di], TypeRegistry.I64, call.span);
return try fb.emitCallIndirect(fn_ptr, args.items, return_type, call.span);
```

### Comparison

This is functionally equivalent but with different encoding:
- Go: single dict ptr + offset calculation
- Cot: multiple dict params as locals

Both work correctly when the analysis is correct.

---

## Gap 8: Multi-Instantiation Handling

### Go's Approach

For each concrete instantiation:
1. Generate shaped function (once per shape)
2. Generate runtime dictionary (once per concrete type)
3. Generate wrapper function (once per concrete type)
4. Wrapper tail-calls shaped with `&dict`

### Cot's Approach

For first instance of a shape (dict_stencil):
1. Generate stencil body under `__stencil_Name` with dict params
2. Generate wrapper under concrete name
3. `buildDictArgNames` maps concrete name → helper names

For subsequent same-shape instances (dict_stencil):
1. Generate dict helpers for new concrete types
2. Generate wrapper under new concrete name → calls same stencil
3. No `shape_aliases` entry (correct)

For shape_only:
1. First instance lowered under concrete name, stored in `shape_stencils`
2. Subsequent instances aliased via `shape_aliases` (SSA redirect)
3. **No wrapper generated** — calls redirect directly

### Gap

Shape_only should also use wrappers instead of SSA aliases (see Gap 6).

---

## Summary: Priority Fixes

| Priority | Gap | Description | Effort |
|----------|-----|-------------|--------|
| **P0** | Gap 1 | Derived type tracking (false positive fix) | Medium — add `derived_exprs` map, populate during analysis |
| **P1** | Gap 6 | Shape_only wrappers (remove SSA aliases) | Small — generate identity wrappers for shape_only |
| **P2** | Gap 5 | Shape key precision (LinkString vs size+kind) | Small — improve Shape.key() |
| **P3** | Gap 4 | Tail calls in wrappers | Small — use `return_call` when available |
| **P4** | Gap 2 | Unified dictionary struct | Large — architectural change, defer to when needed |
| **P5** | Gap 3 | Dict position (after receiver) | Small but requires coordinated change |
| **P6** | Gap 8 | Shape_only wrapper generation | Small — tied to Gap 6 |

### Immediate Action: Fix Gap 1

The `analyzeStencilability` false positive is the **only blocking bug**. All other gaps are either:
- Working correctly with a different-but-valid approach (Gaps 2, 3, 7)
- Performance optimizations (Gaps 4, 5)
- Correctness issues only triggered by shape_only (Gap 6, 8)

Once Gap 1 is fixed with proper derived type tracking, re-enable stenciling and test against selfcot.
