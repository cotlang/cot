# token.cot ↔ token.zig Parity Report

**Status: COMPLETE** (Feb 21, 2026)

`self/token.cot` has achieved full functional parity with `compiler/frontend/token.zig`. All language features needed for parity have been implemented and verified on both native and wasm targets.

---

## Parity Summary

| Aspect | token.zig | token.cot | Parity |
|--------|-----------|-----------|--------|
| Enum variants (68 total) | All present | All present | Identical |
| Backing type | `enum(u8)` | `enum(u8)` | Identical |
| Bitwise operator names | `@"and"`, `@"or"`, `not` | `@"and"`, `@"or"`, `@"not"` | Identical (`@"not"` needed in Cot since `not` is a keyword) |
| token_strings table | Comptime block + `@typeInfo` | Comptime block + `@typeInfo` | Identical pattern |
| `string()` / `toString()` | `pub fn string(self: Token) []const u8` | `fn toString() string` (`@safe` implicit self) | Renamed — `string` is a keyword in Cot |
| `precedence()` | Returns `u8` | Returns `u8` | Identical |
| `isLiteral()` | `@intFromEnum(self)` range check | Direct `self > Token.literal_beg` | Simplified — Cot enums are orderable directly |
| `isOperator()` | Same pattern | Same pattern | Identical logic |
| `isKeyword()` | Same pattern | Same pattern | Identical logic |
| `isTypeKeyword()` | Switch statement | Switch statement | Identical |
| `isAssignment()` | Switch statement | Switch statement | Identical |
| Keyword lookup | `StaticStringMap.initComptime` (O(1) hash) | String switch expression | Different impl, identical behavior |
| `.variant` shorthand | Yes | Yes | Identical |
| Value receiver | `self: Token` | `self: Token` (implicit via `@safe`) | Identical |
| Tests | 8 test blocks | 12 test blocks | Cot has more coverage |

---

## Accepted Differences (cosmetic only)

1. **Method name**: `string()` → `toString()` — Cot reserves `string` as a type keyword. No functional impact; any self-hosted scanner calling this method uses `toString()`.

2. **Range check simplification**: `isLiteral`/`isOperator`/`isKeyword` use direct enum comparison (`self > Token.literal_beg`) instead of `@intFromEnum` extraction. This is a Cot improvement — enums support ordering directly.

3. **Keyword lookup implementation**: Zig's `StaticStringMap.initComptime` is a compile-time perfect hash table (O(1)). Cot's string switch is O(n) linear comparison. Functionally identical for ~55 keywords.

4. **No `pub`**: Cot has no visibility modifiers. All functions are implicitly public.

5. **`@safe` implicit self**: token.cot uses `@safe` mode, so `impl Token` methods omit the explicit `self: Token` parameter — it's injected by the compiler. Zig has no equivalent; explicit `self` is always required.

---

## Language Features That Enabled Parity

All implemented as part of this parity effort:

| Feature | Compiler File | Description |
|---------|--------------|-------------|
| `enum(u8)` backing type | `parser.zig` | Zig-style `enum(T)` syntax in 3 parse sites |
| `@"keyword"` quoted identifiers | `scanner.zig` | Use keywords as enum variant names |
| String switch expressions | `lower.zig` | `switch (name) { "fn" => ..., else => ... }` |

Previously implemented features also used:
- `@safe` mode — implicit `self: Token` injection for `impl Token` methods
- `.variant` shorthand in switch arms
- `comptime { ... }` blocks with `inline for` and `@typeInfo`
- `@enumLen`, `@intFromEnum`, `@enumFromInt`

---

## Verification

```bash
cot test self/token.cot                    # 12 tests, native ✓
cot test self/token.cot --target=wasm32    # 12 tests, wasm ✓
cot test test/e2e/features.cot             # 293 tests (includes 9 new feature tests) ✓
zig build test                             # Compiler internals ✓
```
