# Bug: Array Computed Index Through Pointer Produces Wrong Results

**Date:** 2026-03-25
**Status:** Open
**Severity:** High — blocks sorting, in-place algorithms through pointer params

## Context

Simple constant index store through `*[N]T` was fixed in commit `73303da`.
This bug is about **computed** (runtime) index expressions through pointers.

## Reproduction

```cot
fn shiftRight(arr: *[8]i64, idx: i64) void {
    arr[idx + 1] = arr[idx]  // BROKEN: writes to wrong location
}

test "computed index" {
    var arr: [8]i64 = undefined
    arr[0] = 10; arr[1] = 20; arr[2] = 30
    shiftRight(&arr, 1)
    @assertEq(arr[2], 20)  // FAILS: arr[2] != 20
}
```

## Behavior

- `arr[0] = val` through `*[N]T` works (constant index — fixed)
- `arr[idx + 1] = arr[idx]` through `*[N]T` writes to wrong location
- Fails on both native and Wasm targets
- Reading `arr[idx]` through pointer may also be wrong (needs testing)

## Impact

Blocks any algorithm that does in-place array manipulation with computed indices through pointers:
- Insertion sort (`arr[j + 1] = arr[j]` in inner loop)
- Selection sort (swap via computed indices)
- Quicksort partition
- Binary search with modification
- Any function taking `*[N]T` and using variable indices

## Likely Cause

Address computation for `arr[idx + 1]` when `arr` is `*[N]T`: the codegen likely computes `local_addr + (idx+1)*8` instead of `load(local_addr) + (idx+1)*8`. The pointer must be dereferenced first to get the actual array base address, then the element offset added.

Compare with the constant index fix — the same deref-first pattern needs to apply to computed indices.
