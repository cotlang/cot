# Language Feature Gaps: Cot vs Zig for Self-Hosting

This document catalogs Zig language features used heavily in the Cot compiler (`compiler/`)
that Cot doesn't yet support. Counts are from the Zig compiler source (~10,500 lines of lower.zig alone).

**Last audited: Mar 6, 2026** — All language-level gaps are now resolved.

## Remaining Gaps

**No language-level gaps remain.** All language features needed for self-hosting parity are implemented.

### Stdlib Enhancements (Non-Language)

| Feature | Status | Notes |
|---------|--------|-------|
| **Map `getOrPut`** | **TODO** | Stdlib: add `Map.getOrPut()` returning result struct (~25 call sites) |
| **Map iterator** | **Partial** | `keys()`/`values()`/`keyAt(n)`/`valueAt(n)` exist; a proper `entries()` iterator would be cleaner (~26 uses) |

## Completed Features

| Feature | Completed | Notes |
|---------|-----------|-------|
| Labeled block expressions | Mar 6, 2026 | `blk: { break :blk val }` — result-local + merge-block pattern, type inference from break values |
| `orelse return/continue/break` | Mar 6, 2026 | Dedicated `orelse_expr` AST node, compound optional fixes on native+wasm |
| Parallel `for` | Pre-existing | `for i, item in list { }` — `index_binding` in ForStmt already works |
| `catch \|err\| expr` | Pre-existing | Catch accepts any expression as fallback (precedence 1 postfix) |
| `startsWith`/`endsWith` | Pre-existing | Already in `stdlib/string.cot` as module functions |
| `inline for` | Pre-existing | `inline for i in 0..5 { }` — already implemented |

## Previously Listed (Now Resolved)

These were originally listed as gaps but were found to already work during the Mar 6 audit:

- **Parallel `for`** (275 uses): Already works as `for i, item in collection { }`. Cot uses `i, item` syntax instead of Zig's `|item, i|` capture.
- **`catch |err| switch`** (72 uses): Already works. `catch` accepts any expression as fallback — `catch |e| switch (e) { ... }` compiles today.
- **String `startsWith`/`endsWith`** (58 uses): Already in stdlib/string.cot. Functions: `startsWith(s, prefix)`, `endsWith(s, suffix)`.

## Self-Hosting Status

Current self-hosted state: **25,004 lines, 266 tests, ~95% of Zig frontend ported**.
The remaining ~5% is primarily async/concurrency and shape stenciling — implementation
gaps, not language gaps.

With labeled block expressions now implemented, the self-hosted compiler has reached **~98%
readability parity** with the Zig reference.
