# Self-Hosting Gap Analysis: Complete Audit of ~19K Lines

**Date**: Feb 18, 2026
**Scope**: Every Zig language feature used in the Cot→Wasm compiler path
**Files audited**: parser.zig (1908), checker.zig (3155), lower.zig (6937), types.zig (668), ir.zig (598), ast.zig (759), ssa_builder.zig (1627), gen.zig (1448), constants.zig (829), main.zig (~300)
**Method**: 5 parallel agents read every line, cataloged every Zig feature

---

## Executive Summary

Of ~120 distinct Zig patterns found across ~19K lines, Cot already handles the vast majority.
The remaining gaps fall into **3 categories**:

1. **Eliminated by ARC** (~20% of Zig code disappears): allocator threading, manual deinit, defer cleanup chains, errdefer for memory
2. **Already supported**: structs, methods, enums, unions, optionals, error unions, try/catch, defer, generics, traits, switch with captures, for-in with index, labeled blocks, inline for, slices, string interpolation, all Tier 1-2 builtins
3. **Actually missing**: ~8 features needed, detailed below

---

## What Disappears (No Cot Equivalent Needed)

| Zig Pattern | Why It Disappears | Count |
|---|---|---|
| `allocator: std.mem.Allocator` field/param | ARC handles allocation | ~200 uses |
| `defer list.deinit(allocator)` | ARC auto-cleanup | ~100 uses |
| `errdefer cleanup()` for memory | ARC | ~30 uses |
| `try list.append(allocator, val)` → `list.append(val)` | List manages its own memory | ~150 uses |
| `allocator.dupe(u8, string)` | Strings are ARC-managed | ~50 uses |
| `allocator.alloc(T, n)` → `@alloc(n)` | Builtin | ~20 uses |
| `std.heap.ArenaAllocator` | ARC replaces arenas | ~10 uses |
| `std.ArrayListUnmanaged(T)` → `List(T)` | Stdlib List | ~35 uses per file |
| `std.StringHashMap(V)` → `Map(string, V)` or `StringMap` | Stdlib Map/StringMap | ~20 uses per file |
| `std.AutoHashMap(K, V)` → `Map(K, V)` | Stdlib Map | ~10 uses per file |
| `HashMap.getOrPut()` → `if (!m.has(k)) { m.set(k, v) }` | Stdlib Map API | ~15 uses |
| `HashMap.valueIterator()` / `.iterator()` | for-in on collections | ~10 uses |

**Total eliminated**: ~650 lines of allocator plumbing across all files. This is the biggest simplification.

---

## What Already Works (Verified)

| Category | Zig Feature | Cot Equivalent | Confidence |
|---|---|---|---|
| **Structs** | Struct definition, fields, methods, defaults | Same | HIGH |
| **Enums** | `const E = enum { ... }`, methods via impl | Same | HIGH |
| **Unions** | `union(enum)`, switch captures `\|val\|` | Same | HIGH |
| **Optionals** | `?T`, `if (opt) \|val\|`, `orelse` | Same | HIGH |
| **Errors** | `error { ... }`, `!T`, `try`, `catch` | Same | HIGH |
| **Control** | if/else, while, for-in, switch, break/continue | Same | HIGH |
| **Labeled** | `blk: { break :blk val; }`, labeled loops | Same | HIGH |
| **Generics** | Monomorphization, type params | Same | HIGH |
| **Traits** | impl Trait for Type | Same | HIGH |
| **Defer** | `defer`, `errdefer` | Same | HIGH |
| **Inline for** | `inline for i in 0..N` | Same | HIGH |
| **Slices** | `[]T`, indexing, `.len` | Same | HIGH |
| **Strings** | Literals, `==`, interpolation, slicing | Same | HIGH |
| **Pointers** | `*T`, `*const T`, `?*T`, deref `.*` | Same | HIGH |
| **Builtins** | @intCast, @bitCast, @truncate, @sizeOf, @offsetOf, @min/@max, @tagName, @errorName, @intFromEnum, @enumFromInt, @intFromBool, @as, @alignCast, @constCast, @hasField, @TypeOf, @field, @memcpy, @panic, @trap | Same | HIGH |
| **For+index** | `for i, item in collection` | Same | HIGH |
| **Defaults** | Struct field defaults | Same | HIGH |
| **Nested types** | `struct Foo { const Bar = ... }` | Same | HIGH |
| **Comptime** | Dead branch elim, const eval | Same | HIGH |
| **Collections** | List(T), Map(K,V), StringMap | stdlib | HIGH |
| **String funcs** | indexOf, startsWith, endsWith, contains, trim, replace, split, parseInt | stdlib/string.cot | HIGH |
| **Formatting** | hex(), binary(), padLeft(), sprintf() | stdlib/fmt.cot | HIGH |
| **Mem funcs** | eql, indexOfScalar, startsWith, endsWith | stdlib/mem.cot | HIGH |
| **I/O** | Writer/Reader traits, BufferWriter | stdlib/io.cot | HIGH |
| **Debug** | assert(cond, msg), print | stdlib/debug.cot | HIGH |

---

## What's Actually Missing (8 Items)

### Gap 1: `parseFloat(s: string) f64` — EASY

**Where used**: lower.zig:683, gen.zig (float literal parsing)
**Zig**: `std.fmt.parseFloat(f64, str)`
**Current Cot**: `parseInt(s)` exists in stdlib/string.cot, but no `parseFloat`

**Fix**: Add `fn parseFloat(s: string) f64` to `stdlib/string.cot`. Parse sign, integer part, `.`, fractional part, `e`/`E` exponent. Convert to f64 via `int_part + frac_part * 10^-digits`. Or add `@parseFloat` builtin that delegates to Wasm.

**Effort**: ~40 lines of Cot

---

### Gap 2: `for (a, b) |ai, bi|` — Parallel Iteration — EASY (workaround)

**Where used**: types.zig:477/556 (comparing tuple element types), ssa_builder.zig:64 (parallel slice iteration)
**Zig**: `for (slice_a, slice_b) |a, b| { ... }` — zip iteration
**Current Cot**: Only `for item in collection` and `for i, item in collection`

**Fix**: NOT a language feature — use indexed while loop:
```cot
var i = 0
while (i < a.len()) {
    var ai = a.get(i)
    var bi = b.get(i)
    // ...
    i += 1
}
```
~5 uses total. Not worth a language feature. **No implementation needed.**

---

### Gap 3: `@intFromFloat(f64)` / `@floatFromInt(i64)` — EASY

**Where used**: checker.zig:801, lower.zig:683, gen.zig (converting between int/float)
**Zig**: `@intFromFloat(val)`, `@floatFromInt(val)`
**Current Cot**: Has `@intCast` and `@floatCast` but those are same-family. Need int↔float conversion.

**Fix**: Add two builtins through standard pipeline (ast→parser→checker→lower). @intFromFloat lowers to `wasm_i64_trunc_f64_s`. @floatFromInt lowers to `wasm_f64_convert_i64_s`.

**Effort**: ~30 lines of Zig across 4 files

---

### Gap 4: `@divTrunc(a, b)` / `@rem(a, b)` — EASY

**Where used**: checker.zig:665-666 (comptime const evaluation), lower.zig (arithmetic)
**Zig**: `@divTrunc(a, b)` — truncated division, `@rem(a, b)` — remainder
**Current Cot**: Has `/` and `%` operators, but no explicit truncation-mode builtins

**Fix**: May not need separate builtins — Cot's `/` already does truncated division (Wasm `i64.div_s`), and `%` does remainder (Wasm `i64.rem_s`). Verify behavior matches Zig's `@divTrunc`/`@rem`. If so, just use operators in self-hosted code. If needed, add as aliases.

**Effort**: Verify only (0 lines), or ~15 lines if builtins needed

---

### Gap 5: `std.mem.endsWith` on byte slices — EASY

**Where used**: lower.zig:295 (`endsWith(u8, name, "_deinit")`)
**Current Cot**: `string.endsWith(s, suffix)` works on strings. `mem.endsWith` works on raw pointers.

**Fix**: In self-hosted Cot, names ARE strings, so `endsWith(name, "_deinit")` works directly. The mem.cot version is for raw byte buffers. **No implementation needed** — string version covers all compiler uses.

---

### Gap 6: `anytype` / Duck-Typed Parameters — MEDIUM

**Where used**: checker.zig:3001 (`fn findSimilarVariant(name: []const u8, variants: anytype)`)
**Zig**: `anytype` parameter accepts any type, resolved at comptime
**Current Cot**: Has generics `fn foo(T)(a: T)` but not implicit anytype

**Fix**: Only 1 use in the entire compiler path. Replace with explicit generic:
```cot
fn findSimilarVariant(T)(name: string, variants: T) ?string
```
Or just write two specializations (one for enum variants, one for map keys). **No language feature needed.**

---

### Gap 7: `@memcmp(a, b, len)` Builtin — EASY

**Where used**: Implicit in `std.mem.eql(u8, a, b)` — byte-level comparison
**Current Cot**: `string ==` works for string comparison. `mem.eql()` works for raw pointers.

**Fix**: Already covered by `mem.eql()` in stdlib/mem.cot. For string comparisons in the self-hosted compiler, just use `==`. **No implementation needed.**

---

### Gap 8: `std.fmt.parseInt(T, str, radix)` with Radix — EASY

**Where used**: checker.zig:172, 653, 800, lower.zig:2846, gen.zig (parsing integer literals)
**Zig**: `std.fmt.parseInt(i64, "0xFF", 0)` — auto-detects radix from prefix, or uses explicit radix
**Current Cot**: `parseInt(s)` exists but doesn't handle radix/hex prefix

**Fix**: Extend `parseInt` in stdlib/string.cot to handle `0x` (hex), `0b` (binary), `0o` (octal) prefixes. ~20 extra lines.

**Effort**: ~20 lines of Cot

---

## True Implementation Needed

| Gap | Effort | Priority |
|---|---|---|
| `parseFloat(s)` | ~40 lines Cot | HIGH — needed for float literal parsing |
| `@intFromFloat` / `@floatFromInt` | ~30 lines Zig | HIGH — needed for type conversions |
| `parseInt` hex/binary prefix | ~20 lines Cot | HIGH — needed for integer literal parsing |
| `@divTrunc` / `@rem` | Verify only | LOW — likely already works via `/` and `%` |

**Total new code needed: ~90 lines**

Everything else either already exists in Cot or is eliminated by ARC.

---

## Confidence Assessment

| File | Lines | Portability | Blockers |
|---|---|---|---|
| constants.cot | 829 → ~700 | **READY** | None — enum definitions + constants |
| token.cot | 319 → ~300 | **READY** | None — enum + StringMap |
| scanner.cot | 494 → ~450 | **READY** | None — string processing |
| ast.cot | 759 → ~700 | **READY** | None — struct + union definitions |
| types.cot | 668 → ~500 | **READY after Gap 1** | parseFloat for float type handling |
| parser.cot | 1908 → ~1500 | **READY** | None — recursive descent |
| ir.cot | 598 → ~450 | **READY** | None — struct + enum definitions |
| checker.cot | 3155 → ~2500 | **READY after Gaps 1,3** | parseFloat, intFromFloat |
| lower.cot | 6937 → ~5500 | **READY after Gaps 1,3,8** | parseFloat, intFromFloat, parseInt radix |
| ssa.cot | 1627 → ~1300 | **READY** | None |
| wasm.cot | 1448 → ~1200 | **READY** | None — bytecode emission |
| main.cot | ~300 → ~300 | **READY** | None — CLI dispatch |

**Total: ~19K lines Zig → ~15K lines Cot** (20% reduction from ARC simplification)

---

## Recommendation

The self-hosted compiler is **ready to start** after implementing 3 small items:
1. `parseFloat(s)` in stdlib/string.cot (~40 lines)
2. `@intFromFloat` / `@floatFromInt` builtins (~30 lines Zig)
3. Extend `parseInt` for hex/binary/octal prefixes (~20 lines Cot)

These can be done in a single session. After that, begin porting in dependency order:
constants → token → scanner → ast → types → parser → ir → checker → lower → ssa → wasm → main
