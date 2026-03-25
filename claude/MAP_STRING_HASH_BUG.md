# Bug: Map String Hash Corruption in Shared Generic Bodies

**Date:** 2026-03-25
**Status:** PARTIALLY FIXED — e2e tests pass, selfcot native binary still crashes
**Severity:** Critical — blocks Phase 1 of self-hosting (selfcot can't compile ANY file)
**Affects:** Both native and Wasm targets

## Symptom

```
BUG: Map string hash: ptr=0 len=6129499304
/Users/johnc/cotlang/cot/stdlib/map.cot:47: panic: corrupt string in Map hash
```

The `len` field is garbage (e.g., `6129499304`, `7651461227091285295`). The `ptr` may be 0 or a valid address. The string's 16-byte representation (ptr+len) is truncated to 8 bytes somewhere.

## Root Cause

In VWT shared generic bodies (Phase 8.5), T-typed parameters are passed as indirect pointers (`__ptr_{name}`, 8 bytes). A "scalar shadow" local is created at function entry by loading the first 8 bytes from the pointer. For T=string (16 bytes: ptr+len), the scalar shadow only captures the `ptr` part — the `len` is lost.

When the body does `var k = key` (copying the T value to a local), the code path matters:

### What Was Fixed (commit 313a97f)

The `lowerLocalVarDecl` function dispatches on type. For `STRING` type, it goes through `lowerStringInit` which copies the scalar shadow (8 bytes only). The fix added a T-indirect check BEFORE the type dispatch that uses memcpy from `__ptr_key` to copy the full runtime-sized value.

This fixed the e2e test cases (map.cot, big_stack_map.cot, etc.) because those use `Map(string, i64)` where the Zig compiler's lowerer has the fix.

### What's Still Broken

The selfcot NATIVE binary crashes with the same error. This means either:

1. **selfcot's own lowerer** (self/build/lower.cot) doesn't have the var-init memcpy fix (likely — we ported T-indirect params and call site wrapping but may not have ported the var-init path)
2. **Another code path** in the Zig compiler's lowerer that copies T values without memcpy (e.g., method call receivers, field access stores, return value copies)
3. **The native codegen** (CLIF→ARM64) handles the memcpy differently than expected

## Reproduction

```bash
# Rebuild selfcot with latest Zig compiler
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot

# Try to compile ANY file — crashes immediately
/tmp/selfcot test test/cases/arithmetic.cot
# → BUG: Map string hash: ptr=0 len=6129499304
# → crash at Map_set in self/check/types.cot:271

# Also crashes with:
/tmp/selfcot build self/test_tiny.cot -o /tmp/out.wasm
# → same crash
```

## Where the Crash Happens

`self/check/types.cot:271` — inside the type checker's `registerType` or similar. The checker stores type names in a `Map(string, int)`. When inserting a string key, `k.hash()` reads `ptr` and `len` from the string struct. If `len` is garbage, the hash function reads wild memory → crash.

## Investigation Steps

### 1. Check if selfcot's lower.cot has the var-init memcpy fix

The Zig compiler has this check at the TOP of `lowerLocalVarDecl` (before type-specific paths):
```zig
// Phase 8.5: T-indirect var init takes priority over type-specific paths.
if (early_init_tp) |tp_name| {
    // memcpy from __ptr_ to local address
    ...
}
```

Check if `self/build/lower.cot` has the equivalent. If not, port it.

### 2. Check other T-value copy paths

Search for ALL places where a T-typed value is copied in a shared generic body:
- `var k = key` (var init) — partially fixed
- `ptr.* = value` (deref assign) — fixed (memcpy)
- `return value` (SRET return) — fixed (memcpy for deref/field)
- Method call: `k.hash()` — the receiver `k` is a local. If `k` was properly initialized via memcpy, `.hash()` should work
- Function call args: `indexOf(value)` — fixed (T-indirect forwarding)
- Comparison: `a == b` — fixed (memcmp/string_eq runtime dispatch)

### 3. Use pipeline debugger

```bash
COT_DEBUG=lower ./zig-out/bin/cot build self/main.cot -o /tmp/selfcot 2>&1 | grep 'Phase 8.5.*var.*init.*memcpy'
```

This shows which var inits use memcpy. If `Map_set`'s `var k = key` doesn't appear, the fix isn't triggering for that specific function.

### 4. Check the specific function

The crash is in `Map_set` (shared body for `Map(K,V).set(key: K, value: V)`). In this body:
```cot
fn set(key: K, value: V) void {
    var k = key           // ← This must use memcpy from __ptr_key
    var h = k.hash()      // ← k must have full 16-byte string (ptr+len)
    ...
}
```

If `var k = key` copies only 8 bytes (scalar shadow), `k.hash()` reads garbage for len.

## Related Code

| File | What | Line |
|------|------|------|
| `compiler/frontend/lower.zig` | Zig compiler T-indirect var init fix | ~2506 (early_init_tp check) |
| `self/build/lower.cot` | selfcot var init (needs porting) | ~1063 (lowerLocalVarDecl equivalent) |
| `stdlib/map.cot:47` | Map hash panic (detection) | `@panic("corrupt string in Map hash")` |
| `stdlib/map.cot:226` | Map_set body with `var k = key` | Where the copy happens |

## Fix Strategy

1. Port the var-init memcpy check to `self/build/lower.cot`
2. Verify with pipeline debugger that memcpy fires for Map_set's `var k = key`
3. If selfcot still crashes after porting, there's another copy path missing — use USE WATCH debug to find where string values get truncated
