# Bug: Array Element Store Through Pointer Doesn't Write Back

**Date:** 2026-03-25
**Status:** Open
**Severity:** High — affects any function that mutates array elements via pointer parameter

## Reproduction

```cot
fn setFirst(arr: *[4]i64, val: i64) void {
    arr[0] = val  // writes to local copy, not caller's memory
}

test "mutation visible" {
    var arr: [4]i64 = undefined
    arr[0] = 99
    setFirst(&arr, 42)
    @assertEq(arr[0], 42)  // FAILS: arr[0] is still 99
}
```

## Behavior

- Direct mutation (`arr[0] = 42` in same scope) works correctly
- Mutation through `*[N]T` pointer parameter does NOT write back to caller
- Fails on both native and Wasm targets
- Wasm output confirms: `expected: 99, received: 42` — the store happens but isn't visible to caller

## Impact

Any function that takes `*[N]T` and mutates elements is broken:
- Sorting algorithms (insertion sort, selection sort, quicksort partition)
- In-place array transforms
- Any utility function that modifies array contents

## Likely Cause

The codegen for `arr[0] = val` when `arr` is a pointer parameter is probably computing the store address wrong — storing to the local's stack slot instead of dereferencing the pointer to get the caller's address.

## Reference

Zig compiler: `lower.zig` index assignment through pointers — check `lowerIndexAssign` for the pointer-to-array case.
