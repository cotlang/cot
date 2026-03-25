# Bug: Generic Struct Method Returning ?T Produces Invalid Wasm

**Date:** 2026-03-25
**Status:** Open
**Severity:** High — blocks generic container patterns with optional return

## Reproduction

```cot
import "std/list"

struct Stack(T) {
    items: List(T),

    fn pop(self: *Stack(T)) ?T {
        if (self.items.len() == 0) { return null }
        const val = self.items.get(self.items.len() - 1)
        self.items.pop()
        return val
    }
}

test "pop" {
    var s: Stack(i64) = Stack(i64) { .items = .{} }
    s.push(42)
    if (s.pop()) |v| { @assertEq(v, 42) }
}
```

## Error

```
Error: failed to compile: wasm[0]::function[96]
WebAssembly translation error
Invalid input WebAssembly code at offset 10065: type mismatch: expected i32, found i64
```

## Behavior

- Wasm validation error: i32/i64 type mismatch in generated code
- The `?T` return type in a generic method generates wrong Wasm types when T=i64
- Optional i64 is a 2-word type (has_value: i32, value: i64), but codegen may be treating it as single i64
- Native target: SIGSEGV (different manifestation of same issue)

## Impact

Blocks the Stack/Queue/MinStack pattern — any generic container with a `pop() ?T` or `peek() ?T` method. This is a fundamental pattern in data structures.

## Likely Cause

When monomorphizing `?T` with T=i64, the optional return type should be `?i64` which is a compound type (2 words). The Wasm codegen may be emitting a single i64 return instead of the multi-value or SRET pattern needed for compound optionals.
