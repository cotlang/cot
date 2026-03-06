# features.cot SIGSEGV Crash — RESOLVED

## Root Cause

**Generic instantiation `expr_types` collision.** The checker's `expr_types` map uses AST `NodeIndex` as keys. During nested generic instantiation (e.g., `Map(i64, i64)` triggering `MapIterator(i64, i64)` in the same file), the method body checker wrote to `expr_types` using the generic template's node indices, which collided with the user file's node indices.

Specifically: node 142 in the user's AST was `mayFail`'s return type (resolved to error_union). Node 142 in `stdlib/map.cot`'s AST was a `builtin_call` (resolved to I64). During `MapIterator` impl method checking, the I64 overwrote the error union, causing `mayFail` to be compiled without error union wrapping — returning a raw value instead of a pointer to `[tag, payload]`.

## Why Cross-File Detection Failed

The existing fix saved/restored `expr_types` only when `impl_info.tree != host_tree` (cross-file). But during nested instantiation (`Map` → `MapIterator`, both in `map.cot`), `host_tree` was already `map.cot`'s tree (set by the parent `Map` instantiation), so `is_cross_file` was false. The save/restore was skipped.

## Fix (checker.zig)

Always save/restore `expr_types` during generic body checking, regardless of cross-file status:

1. **`instantiateGenericImplMethods`** (Pass 2): unconditionally save/restore `expr_types` around method body checking
2. **`instantiateGenericFunc`**: same — always save/restore around `checkFnDeclWithName`

This is safe because generic bodies use the declaring file's AST (after tree swap), so their node indices should never pollute the caller's `expr_types`.

## Verification

- 360/360 `features.cot` tests pass
- 73/73 test files pass (`./test/run_all.sh`)
- Compiler unit tests pass (`zig build test`)
