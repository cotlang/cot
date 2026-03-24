# VWT Swift Port Plan — COMPLETED (2026-03-25)

**Status: ALL 6 STEPS IMPLEMENTED AND VERIFIED.**
- Steps 1-4 completed 2026-03-24 (commit e3864a4)
- Steps 5-6 completed 2026-03-25 (VWT dispatch connected, witnesses emitted)
- Old inline ARC code deleted, stenciling infrastructure deleted (~1,275 lines removed)

## Swift Source Audit Summary

All patterns below are from `references/swift/lib/IRGen/` with line numbers.

### The 5 Rules (Non-Negotiable)

1. **Address-only types use indirect return** (GenCall.cpp:747-764)
   - Generic type parameter T is ALWAYS address-only
   - Convention stored in function TYPE, not computed at codegen
   - `ResultConvention::Indirect` → adds `i8*` parameter FIRST

2. **Parameter order is fixed** (GenCall.cpp:28-36)
   ```
   [indirect results] [user params] [metadata + witnesses] [self/context] [error]
   ```

3. **One body per generic definition** (GenProto.cpp:2928-2950)
   - `EmitPolymorphicParameters::emit()` binds metadata from params
   - Body accesses T's size/VWT through bound metadata
   - NO monomorphization (specialization is a separate optional pass)

4. **Metadata passed as extra params after user params** (GenProto.cpp:4312-4377)
   - `expandPolymorphicSignature()` appends metadata + witness table ptrs
   - One per unfulfilled type requirement

5. **VWT loaded from metadata at runtime** (GenOpaque.cpp:431-476)
   - `metadata → vwt_ptr → function_ptr → indirect_call`
   - Index adjusted for 64-bit (skip Flags+xiCount packed field)

---

## Implementation Steps

### Step 1: Add `has_indirect_result` to GenericInstInfo

Swift stores the calling convention in `SILFunctionType.ResultConvention`.
Cot equivalent: when the checker creates `GenericInstInfo`, check if the
return type is a type parameter. If so, mark `has_indirect_result = true`.

This is computed ONCE at type-checking time, stored in the type info,
and read by both the lowerer (for body generation) and call sites.

**File:** `compiler/frontend/checker.zig`
**Change:** In `instantiateGenericImplMethods()`, after resolving the
function signature, check if the return type AST node is a type parameter
name. Set `has_indirect_result = true` on the GenericInstInfo.

**File:** `compiler/frontend/checker.zig`
**Change:** Add `has_indirect_result: bool = false` to `GenericInstInfo`.

### Step 2: Generic function body uses indirect return

**File:** `compiler/frontend/lower.zig`
**Change:** In `lowerGenericFnInstanceVWT()`:

```
if (inst_info.has_indirect_result or self.needsSret(return_type)) {
    // Indirect return: add __sret param, function returns void
    uses_sret = true;
    wasm_return_type = VOID;
}
```

This ensures ALL instantiations of the same generic function use
the same return convention, enabling one shared body.

Emit under BASE name (`List_append`), not concrete name.
Skip if base already emitted.

### Step 3: Metadata params AFTER user params

**File:** `compiler/frontend/lower.zig`
**Change:** In `lowerGenericFnInstanceVWT()`, move metadata param
addition to AFTER regular params:

```
// 1. SRET param (if indirect result)
if (uses_sret) addParam("__sret", I64, 8);
// 2. Self param (@safe injection)
if (safe_mode && !static) addParam("self", ptr_type, 8);
// 3. User params
for (f.params) addParam(...);
// 4. Metadata params (AFTER user params — Swift convention)
for (param_names) addParam("__metadata_T", I64, 8);
```

### Step 4: Call sites use base name + append metadata

**File:** `compiler/frontend/lower.zig`
**Change:** In `lowerMethodCall()`:

For generic instance methods:
1. Resolve to BASE name: `computeGenericBaseName(method_info.func_name)`
2. Check `has_indirect_result` from GenericInstInfo
3. If indirect: allocate return buffer, pass as first arg
4. Build args: [sret?] [self] [user_args...] [metadata_T]
5. Call base name

The `has_indirect_result` flag is on GenericInstInfo — no AST inspection
needed at the call site. Just read the flag.

### Step 5: VWT dispatch in generic bodies

**File:** `compiler/frontend/lower.zig`
**Change:** In `emitCopyValue()` and `emitDestroyValue()`:

When the type being copied/destroyed is a generic type parameter (check
`self.type_substitution` — if the type came from substitution, it was
originally generic), use VWT dispatch:

```
// Load VWT from metadata parameter
const metadata_local = fb.getParam("__metadata_T");
const vwt_ptr = fb.emitPtrLoadValue(metadata_local, I64, span);
// Load destroy function pointer from VWT[1] (destroy is at index 1)
const fn_ptr = fb.emitPtrLoadValue(vwt_ptr + 8, I64, span);
// Call indirectly
fb.emitCallIndirect(fn_ptr, &[_]{value_addr, metadata_local}, VOID, span);
```

This is Step 5 — not needed for the initial sharing fix. Inline ARC
works fine for now since type substitution resolves T to concrete type.

### Step 6: Emit VWT witnesses on demand

**File:** `compiler/frontend/lower.zig` or `compiler/driver.zig`
**Change:** When `emitCopyValue` generates a VWT call (Step 5), check if
the witness function exists in the builder. If not, call
`VWTGenerator.emitWitnesses()` to create it inline.

This is lazy emission — matches Swift's `emitValueWitnessTable()` pattern.

---

## Execution Order — ALL COMPLETE

1. **Step 1** (checker.zig): ✅ `has_indirect_result` added to GenericInstInfo
2. **Step 2** (lower.zig): ✅ `lowerGenericFnInstanceVWT` uses `has_indirect_result`
3. **Step 3** (lower.zig): ✅ Metadata params after user params (Swift convention)
4. **Step 4** (lower.zig): ✅ Call sites use base name + indirect return + metadata
5. **Verify**: ✅ 22/22 tests pass, selfcot builds in ~10.9s
6. **Step 5** (lower.zig): ✅ VWT dispatch in emitCopyValue/emitDestroyValue — ALL non-trivial types go through `__vwt_initializeWithCopy_{type}` / `__vwt_destroy_{type}`
7. **Step 6** (driver.zig): ✅ `emitVWTWitnesses()` called in both single-file and multi-file paths, gated on native target (482 types, 135 unique witnesses)

---

## What NOT To Do

- Do NOT inspect AST nodes to determine calling convention (that's what I did wrong)
- Do NOT use `isGenericReturnType()` — the convention is stored, not derived
- Do NOT compute base names at call sites by string manipulation of concrete names
  when the GenericInstInfo already has the information
- Do NOT emit wrappers — one body per generic, period
- Do NOT disable VWT emission as a "workaround" for performance
- Do NOT use `git checkout --` to revert changes
