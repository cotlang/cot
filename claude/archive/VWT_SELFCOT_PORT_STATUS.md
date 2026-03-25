# VWT Selfcot Port — Status

**Date:** 2026-03-25
**Status:** BLOCKED by Cot compiler bug

## Completed

- `indirect_t_params: Map(string, string)` field added to Lowerer struct
- `getSubstitutedTypeParamName()` added to checker.cot (returns type param name)
- T-indirect param detection in `lowerGenericFnInstanceVWT` — `__ptr_{name}` naming
- `indirect_t_params` cleared on function exit

## Blocked

**Compiler bug:** `fb.emitLoadLocal(idx, I64, span)` produces `error[E300]: type mismatch` when called inside a `for` loop that's nested inside an `if (self.builder.func()) |fb|` optional capture block, specifically within `lowerGenericFnInstanceVWT`.

The EXACT same `fb.emitLoadLocal` call pattern works in dozens of other locations in the same file, including other `for` loops inside `if |fb|` blocks. The issue is specific to this location.

**Reproduction:** Add any `fb.emitLoadLocal(...)` call inside the scalar shadow `for` loop at ~line 7837 of `self/build/lower.cot`. Compiling with `./zig-out/bin/cot build self/main.cot` produces `error[E300]: type mismatch`.

**Root cause hypothesis:** The checker's scope resolution for `fb` (captured from optional unwrap) breaks when combined with:
1. A new `for` loop
2. After the existing param `for` loop (line 7803)
3. After `indirect_t_params.has()` conditional

## Remaining After Bug Fix

1. Scalar shadow creation (the blocked code)
2. Call site T-indirect wrapping (free-fn + method)
3. T-indirect ptr forwarding for inner calls
4. @sizeOf(T) runtime load from metadata (currently compile-time constant)
5. Deref store path — memcpy with runtime size
6. SRET return path — memcpy for T-typed returns
7. @arcRetain noop for T-indirect (wasm has no ARC, so just skip)
