# Bug: SIGSEGV in List.append on Native Target

**Date:** 2026-03-25
**Status:** Open — pre-existing regression
**Severity:** Critical — crashes on List.append for some tests on native

## Reproduction

```cot
import "std/list"

test "crash" {
    var list: List(i64) = .{}
    list.append(42)  // SIGSEGV here
}
```

## Behavior

- SIGSEGV in `List_append` on native target
- Crash address suggests null/invalid pointer dereference during COW buffer allocation
- Also affects features.cot: `list_basic` and `list_growth` tests FAIL
- Other List tests (`list_pop`, `list_set`) still pass — may depend on test ordering/state
- Wasm target: type mismatch error (different codegen bug)
- std/map also affected (uses List internally)

## Impact

Blocks most real-world programs that use List or Map on native. Many existing e2e tests (sort.cot, map.cot) are also affected.

## Notes

- The crash is in the ARC-managed List buffer allocation path
- `list_pop` and `list_set` still pass, suggesting the crash depends on initial buffer allocation (first append to empty list)
- May be related to VWT (Value Witness Table) codegen for generic types
