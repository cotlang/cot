# VWT Redo Plan: Correct Swift Port

## What Went Wrong

The original VWT migration built the infrastructure (witness functions, metadata globals,
TypeCategory dispatch) but botched the integration:

1. Deleted Go-style shape stenciling (function sharing) before implementing Swift's replacement
2. Tried "one base + thin wrappers" but hit SRET mismatch (different return conventions per T)
3. Fell back to full monomorphization without sharing = 8x regression (64s vs 8s)
4. Current state: worst of both worlds — no sharing AND no VWT dispatch

## Swift's Actual Architecture (from source audit)

### Parameter Order (GenCall.cpp line 28-36)
```
fn generic_function(
    indirect_return_ptr,    // SRET — ALWAYS for generic returns
    user_param1,
    user_param2,
    ...,
    type_metadata_T,        // *TypeMetadata for each type param
    witness_table_T_Proto,  // *WitnessTable for each conformance
)
```

### Key Rules
1. **ALL generic returns are indirect** — caller allocates buffer, passes pointer as first param
2. **One body per generic definition** — `List.append` exists once, works for all T
3. **Metadata after user params** — not before (I had this wrong)
4. **VWT accessed through metadata** — load function pointer from VWT table, call indirectly
5. **Specialization is OPTIONAL** — optimizer creates monomorphic copies for hot paths

### Why This Works
- `List<Int>.pop()` and `List<BigStruct>.pop()` have IDENTICAL calling conventions
- Both return through indirect pointer (no SRET mismatch)
- Both receive metadata in same position
- ONE compiled body serves all instantiations
- Function sharing is AUTOMATIC — no aliases, no wrappers needed

## The Fix

### Step 1: Generic Return Convention
All functions with generic return type (`fn pop() T`) MUST use indirect return:
- Caller allocates return buffer
- Passes buffer pointer as first param (before user params)
- Callee writes return value into buffer via pointer
- Callee returns void

This is already how `needsSret` works for large structs. The change: `needsSret` must
return true for ANY generic return type (where T is a type parameter), regardless of
the concrete type's size.

### Step 2: Generic Function Lowering
`lowerGenericFnInstanceVWT` changes:
- Emit ONE body under base name (`List_append`)
- ALL generic returns use indirect (step 1 ensures uniform convention)
- Metadata params AFTER user params (Swift convention)
- Body uses VWT witnesses for ARC on type params
- NO wrappers, NO aliases — call sites use base name directly

### Step 3: Call Site Changes
When calling a generic function with concrete T:
- Caller looks up `List_append` (base name, not `List(5)_append`)
- Passes metadata for T after user args
- For generic returns: allocates return buffer, passes as first arg

### Step 4: Emit VWT Witnesses On Demand
Swift pattern: `emitValueWitnessTable()` called during IRGen when metadata first needed.
Cot pattern: when `emitCopyValue` encounters a type that needs VWT dispatch, emit the
witness function inline into the builder if not already emitted.

### Step 5: Remove Dead Code
- Delete `computeGenericBaseName` (no longer needed after step 3)
- Delete `emitVWTWrapper` (no wrappers)
- Delete per-instantiation body lowering
- Clean up `lowerMethodCall` to resolve to base generic name

## What We Keep

- VWT witness function bodies (vwt_gen.zig) — correct, match Swift ABI
- TypeMetadata + ValueWitnessTable struct definitions — correct
- ValueWitnessFlags — correct
- TypeCategory dispatch — correct
- VWTEntry computation — correct

## Verification

After implementation:
- [ ] All 22 test/cases pass
- [ ] selfcot builds in ≤8s (matching pre-VWT baseline)
- [ ] selfcot binary size comparable to pre-VWT
- [ ] selfcot version works
- [ ] `grep -c "fn " lower.zig` shows fewer functions (sharing reduces count)
- [ ] Zero BUG messages in selfcot build
