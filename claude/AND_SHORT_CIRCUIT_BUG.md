# Bug: `and`/`or` Do Not Short-Circuit with Slice Indexing — FIXED

## Status: FIXED (Mar 2, 2026)

**Root cause:** `lowerBinary` in `lower.zig` evaluated both operands unconditionally before creating the IR node. The right operand's side effects (slice bounds checks) were emitted eagerly into the current block, before the SSA builder's `convertLogicalOp` could defer them behind a branch.

**Fix:** Added `lowerShortCircuit()` in `lower.zig` (Go reference: `ssagen/ssa.go:3398-3442` OANDAND/OOROR expr path). Same deferred-evaluation pattern as `lowerCompoundOrelse` — evaluates left, creates conditional blocks, defers right-side lowering to the appropriate block. Bounds checks now only execute when the right side is actually evaluated. Tests: `test/e2e/features.cot` (5 tests).

---

## Original Report

## Summary

The `and` operator does not short-circuit when the second operand involves slice indexing (`s[i]`). The bounds check for the slice access is emitted eagerly, causing an out-of-bounds panic even when the first operand is `false`.

This contradicts the expected behavior of `and` as a short-circuit operator.

## Severity

**High** — This is a correctness bug that causes runtime panics in common guard patterns. Every `while (i < len and s[i] != ch)` idiom is affected. The workaround (nested `if` blocks) is verbose and non-obvious.

## Reproducer

```cot
test "and should short-circuit with slice indexing" {
    var s = ""
    // This panics: index [0] with length 0
    // Expected: false (s.len > 0 is false, so s[0] should never be evaluated)
    if (s.len > 0 and s[0] == 'v') {
        @assert(false)
    }
}
```

**Panic output:**
```
panic: index out of range [0] with length 0
```

## Affected Patterns

Any guard-then-index pattern using `and`:

```cot
// All of these panic when the guard is false:
while (i < s.len and s[i] != ' ') { ... }
if (s.len > 2 and s[1] == '=') { ... }
if (i + 3 < len and s[i] == '\r' and s[i+1] == '\n') { ... }
```

## Quirk: Function Calls DO Short-Circuit

`and` correctly short-circuits when the second operand is a function call:

```cot
// This works — charAt is never called when s.len == 0
if (s.len > 0 and charAt(s, 0) == 'v') { ... }
```

This suggests the issue is specific to how slice indexing bounds checks are lowered in codegen, not the `and` operator itself. The bounds check for `s[i]` appears to be emitted unconditionally before the `and` short-circuit branch.

## Current Workaround

Nest the checks manually:

```cot
// Instead of:  while (i < s.len and s[i] != ' ')
while (i < s.len) {
    if (s[i] == ' ') { break }
    // ...
    i += 1
}

// Instead of:  if (s.len > 0 and s[0] == 'v')
if (s.len > 0) {
    if (s[0] == 'v') {
        // ...
    }
}
```

## Likely Root Cause

In the SSA/codegen layer, slice indexing (`s[i]`) lowers to a bounds check + load. The bounds check is likely emitted at the point where the expression is encountered during IR generation, rather than being deferred behind the short-circuit branch of the `and` operator.

For function calls, the entire call (including any internal bounds checks) is deferred to the right branch of the short-circuit, which is why `charAt(s, i)` works correctly.

## Impact

This bug was discovered during a cot.land codebase audit. **Every file** that did string scanning had at least one instance of this pattern. All had to be restructured to nested `if` blocks. Affected files:

- `pkg/src/request.cot` (header parsing, query parsing)
- `pkg/src/response.cot` (status extraction)
- `pkg/cli/src/semver.cot` (version parsing, range matching)
- `pkg/src/package.cot` (name validation)
- `pkg/src/semver_check.cot` (digit checking)

## Expected Fix

The `and`/`or` operators should fully short-circuit regardless of operand type. Slice indexing bounds checks on the right side of `and` should only execute when the left side is `true`. This likely requires deferring slice bounds-check IR emission to after the short-circuit branch.
