# Next Port Plan: comptime.zig, types.zig, formatter.zig

**Date:** 2026-03-29
**Status:** Ready to execute
**Prerequisite:** ast.zig + parser.zig complete (3,218 + 1,594 lines, 57 tests)

---

## Port Order and Rationale

1. **comptime.zig** (64 lines) — standalone, zero deps, 30 minutes
2. **types.zig** (1,202 lines) — depends on ast only, unblocks checker/ir/lower/everything
3. **formatter.zig** (1,226 lines) — depends on ast/source/token only, useful tool

After these three, the unlock chain is:
- types + comptime → **checker.zig** (5,162 lines)
- types → **ir.zig** (816 lines)
- ir + types → **arc_insertion.zig** (456 lines), **vwt_gen.zig** (1,378 lines), **ssa_builder.zig** (2,908 lines)
- all above → **lower.zig** (13,528 lines)

---

## File 1: comptime.zig (64 lines → ~70 lines)

### Source Analysis

A single tagged union `ComptimeValue` with 8 variants (int, float, string, boolean, array, enum_field, type_info, undefined_val) and three accessor methods (asInt, asString, asFloat). Three nested structs (ComptimeArray, EnumFieldInfo, TypeInfo) and one nested enum (TypeInfoKind).

### Transformation Plan

**Cleanup only — no structural changes needed.** This file is already clean.

1. Remove reference comment (line 3-4: "Reference: Zig's InternPool.MutableValue...")
2. Add `//!` module doc
3. Expand one-liner methods to match CODE_STYLE.md
4. Add `///` doc comments to all public types and methods

**Zig patterns to borrow:** None — this file is too small and specific to benefit from data-oriented redesign. Zig's InternPool.MutableValue is a much more complex system (interned values with thread-safe dedup) that's overkill for Cot's current comptime needs. Keep the simple tagged union.

**Test plan:** 3-4 tests verifying asInt/asString/asFloat extraction and ComptimeArray construction.

### Chunk Plan (single chunk, ~70 lines)

1. Write cleaned comptime.zig to src/
2. Write ac/src/libcot-zig/comptime.md
3. Verify: `zig test src/libcot-zig/comptime.zig`

---

## File 2: types.zig (1,202 lines → ~1,100-1,200 lines)

### Source Analysis

Three major sections:
1. **Type definitions** (lines 1-140): TypeIndex, BasicKind enum (18 variants with methods), 16 composite type structs (PointerType through TaskType), the Type union (17 variants), MethodInfo
2. **TypeRegistry** (lines 156-850): Initialization with 23 pre-registered basic types, type construction (`make*` methods), deduplication, type queries (`isTrivial`, `needsARC`, `isAssignable`, `canCoerce`), name resolution
3. **Helper methods** (lines 850-1202): `typeName`, `resolveDistinct`, bit-width parsing, method registry, generic type support, ARC classification

### Transformation Plan

#### A. TypeIndex — adopt ast.zig's enum wrapper pattern

```zig
// Current:
pub const TypeIndex = u32;
pub const invalid_type: TypeIndex = std.math.maxInt(TypeIndex);

// New (matching ast.zig's Index pattern):
pub const TypeIndex = enum(u32) {
    invalid = std.math.maxInt(u32),
    _,
    pub fn unwrap(self: TypeIndex) ?u32 {
        return if (self == .invalid) null else @intFromEnum(self);
    }
};
```

**Why:** Type safety — can't accidentally pass a TypeIndex where a NodeIndex is expected. Consistent with the Index/OptionalIndex pattern already established in ast.zig.

**Impact:** Every `== invalid_type` check becomes `== .invalid`. Every `@intCast(i)` becomes `@enumFromInt(i)`. Mechanical transformation.

#### B. BasicKind — add comptime name/size arrays (like debug.zig transform)

The current BasicKind has 6 methods that each switch over all 18 variants. Apply the debug.zig pattern — use `@typeInfo` reflection or comptime arrays:

```zig
pub const BasicKind = enum(u8) {
    invalid, bool_type, i8_type, i16_type, ...

    const names = [_][]const u8{ "invalid", "bool", "i8", "i16", ... };
    const sizes = [_]u8{ 0, 1, 1, 2, ... };

    pub fn name(self: BasicKind) []const u8 {
        return names[@intFromEnum(self)];
    }
    pub fn size(self: BasicKind) u8 {
        return sizes[@intFromEnum(self)];
    }
};
```

**Why:** Eliminates 6 switch statements (108 arms total). Adding a new basic type is one line in the enum + one entry in each array. Same pattern that made debug.zig 30% shorter.

#### C. PointerType — pack flags (Zig pattern)

```zig
// Current:
pub const PointerType = struct { elem: TypeIndex, managed: bool = false };

// New (Zig packed flags pattern):
pub const PointerType = struct {
    elem: TypeIndex,
    flags: Flags = .{},

    pub const Flags = packed struct(u8) {
        is_managed: bool = false,
        is_const: bool = false,
        _padding: u6 = 0,
    };
};
```

**Why:** Future-proofs for const pointers, volatile pointers, alignment info. One u8 instead of separate bools. Matches the packed flags pattern used throughout ast.zig.

#### D. TypeRegistry — improve deduplication

The current `make*` methods do linear scans for dedup. Add a hash-based cache for the hot path (pointer/optional wrapping):

```zig
pub const TypeRegistry = struct {
    types: std.ArrayListUnmanaged(Type),
    allocator: std.mem.Allocator,
    name_map: std.StringHashMap(TypeIndex),
    method_registry: std.StringHashMap(std.ArrayListUnmanaged(MethodInfo)),
    // New: hash-based dedup for anonymous types
    pointer_cache: std.AutoHashMap(TypeIndex, TypeIndex),  // elem → pointer(elem)
    optional_cache: std.AutoHashMap(TypeIndex, TypeIndex),  // elem → optional(elem)
};
```

**Why:** makePointer/makeOptional are called thousands of times during compilation. Linear scan is O(n) per call where n = total types. Hash lookup is O(1). Zig's InternPool uses hash-based dedup for the same reason.

**Impact:** ~10 lines to add cache, ~5 lines per make* method to check cache first. No API change.

#### E. Pre-registered constants — use comptime enum

```zig
// Current: manual constants
pub const INVALID: TypeIndex = 0;
pub const BOOL: TypeIndex = 1;
// ...23 constants

// New: derive from BasicKind order
pub const WellKnown = enum(u32) {
    invalid = 0, bool_type = 1, i8_type = 2, ...
    string = 17, ssa_mem = 18, ...
    pub const first_user: u32 = 23;
};
```

**Why:** Eliminates the risk of numbering errors. The order is defined once. Currently there are 23 manual constants that must stay in sync with the init() function.

#### F. Remove reference comments, add doc comments

Delete all lines containing "Reference:", "Go:", "Swift:", "Zig:". Add `///` to every public type and method per CODE_STYLE.md.

#### G. StructField.default_value — use ast.OptionalIndex

```zig
// Current:
default_value: @import("ast.zig").NodeIndex = @import("ast.zig").null_node,

// New:
default_value: ast.OptionalIndex = .none,
```

**Why:** Uses the type-safe wrapper from the redesigned ast.zig. No more inline `@import`.

### Zig Patterns Borrowed

| Pattern | Source | What it does |
|---------|--------|-------------|
| enum(u32) TypeIndex | ast.zig Index | Type-safe index wrapper |
| Packed flags on PointerType | Zig PtrType | Future-proof flag storage |
| Comptime arrays for BasicKind | debug.zig transform | Eliminate repetitive switches |
| Hash-based dedup cache | Zig InternPool | O(1) type dedup instead of O(n) |

### What NOT to change

- The Type union itself — 17 variants is fine, no need for data-oriented redesign here
- ARC classification methods (isTrivial, needsARC, couldBeARC) — these are excellent
- Method registry — the StringHashMap approach works well
- Type coercion/assignability logic — battle-tested, don't touch

### Chunk Plan (4 chunks, ~200-300 lines each)

**Chunk 1:** TypeIndex, BasicKind, all 16 composite type structs (lines 1-140)
**Chunk 2:** Type union, MethodInfo, TypeRegistry struct + init + pre-registered types (lines 140-300)
**Chunk 3:** make* methods with dedup caches, type queries (lines 300-700)
**Chunk 4:** typeName, resolveDistinct, method registry, ARC classification, tests (lines 700-1200)

### Test Plan

- BasicKind: name(), size(), isNumeric(), isInteger(), isFloat(), isSigned()
- TypeRegistry: init with 23 pre-registered types, verify indices
- make*: makePointer, makeOptional, makeSlice — dedup verification
- isAssignable: basic coercions (i32 → i64, untyped_int → i64)
- isTrivial/needsARC: basic types vs pointer vs struct
- resolveDistinct: distinct(i64) → i64

---

## File 3: formatter.zig (1,226 lines → ~1,100-1,200 lines)

### Source Analysis

AST walker that emits formatted Cot source code. Two main parts:
1. **Formatter struct** with indent tracking, output buffer, comment interleaving
2. **Format methods**: formatDecl, formatExpr, formatStmt — one per AST node type

### Transformation Plan

#### A. Adapt to compact AST

The formatter currently uses direct field access (`decl.fn_decl.name`, `expr.call.args`). With the new AST, it needs to use `full.*` accessors (`tree.fnDeclData(node).name`).

This is the **first real consumer** of the new AST. Getting this right validates the `full.*` accessor API.

```zig
// Current:
fn formatFnDecl(self: *Formatter, d: ast.FnDecl) void {
    self.write(d.name);
    for (d.params) |p| { ... }
}

// New:
fn formatFnDecl(self: *Formatter, tree: *const Ast, node: Index) void {
    const d = tree.fnDeclData(node);
    self.write(d.name);
    for (tree.extraNodes(d.params)) |param_extra| { ... }
}
```

**Key difference:** The formatter needs the `Ast` reference to call accessors. Pass it as a field on the Formatter struct.

#### B. Comment interleaving — use token indices

The current formatter collects comments from source text by scanning. With pre-tokenized input, doc comments are `///` tokens in the token array. The formatter can iterate tokens between node spans to find comments.

```zig
// New: iterate token range to find comments
fn writeCommentsBefore(self: *Formatter, tree: *const Ast, token: TokenIndex) void {
    while (self.next_comment_token < token) {
        if (tree.tokenTag(self.next_comment_token) == .doc_comment) {
            self.writeIndent();
            self.write(tree.tokenSlice(self.next_comment_token));
            self.newline();
        }
        self.next_comment_token += 1;
    }
}
```

#### C. Zig formatter patterns

Zig's own formatter (`lib/std/zig/Ast/Render.zig`, 3,900 lines) uses:
- **Fixups** — planned modifications applied during rendering (add/remove semicolons, parentheses)
- **Whitespace handling** — tracks whether a newline is needed, defers whitespace decisions
- **Token-based rendering** — writes tokens by index rather than regenerating text

For Cot Phase 1, we don't need fixups. But the token-based rendering approach is worth adopting — instead of regenerating text from AST fields, write the original tokens with adjusted whitespace. This preserves user-chosen names and avoids re-encoding string literals.

### Chunk Plan (4 chunks)

**Chunk 1:** Formatter struct, indent tracking, output buffer, entry point (lines 1-200)
**Chunk 2:** formatDecl — fn, struct, enum, union, impl, trait, type alias (lines 200-500)
**Chunk 3:** formatExpr — all expression types (lines 500-850)
**Chunk 4:** formatStmt — all statement types, comment interleaving, tests (lines 850-1200)

### Test Plan

- Round-trip: parse source → format → verify output matches expected canonical form
- Indent: nested blocks, nested structs
- Comments: doc comments preserved in correct positions
- All declaration types formatted correctly
- Expression precedence: parentheses added/removed correctly

---

## Implementation Order

```
Day 1: comptime.zig (30 min) + types.zig chunks 1-2 (2-3 hours)
Day 2: types.zig chunks 3-4 (2-3 hours)
Day 3: formatter.zig chunks 1-4 (3-4 hours)

After each file: tests, ac/ documentation, commit.
```

After all three: **checker.zig** and **ir.zig** become unblocked.
