# Cot Code Style Guide

**Applies to:** All Zig files in `src/`
**Reference:** Zig standard library (`references/zig/lib/std/`), Zig compiler (`references/zig/src/Air.zig`)
**Examples:** `src/libcot-zig/token.zig`, `src/libcot-zig/scanner.zig`

This guide follows Zig's established conventions from the standard library and compiler, with minimal Cot-specific additions noted as such.

---

## File Layout

Every file follows this order:

```zig
//! Module documentation (what, where in pipeline, key design decisions).

const std = @import("std");         // stdlib first
const sibling = @import("foo.zig"); // sibling imports, alphabetical

const Type = sibling.Type;          // type aliases, alphabetical

/// Public type doc.
pub const MyType = struct {
    // fields, then methods
};

// Private helpers

fn helper() void {}

// Tests last

test "behavior description" {}
```

No section dividers. Structure comes from blank lines, doc comments, and the natural ordering of declarations. This matches the Zig standard library (mem.zig, fmt.zig, hash_map.zig — none use dividers).

**Cot-specific note:** The earlier `═══` section dividers have been removed to align with standard Zig practice. If you find a file still using them, remove them during cleanup.

---

## Documentation

### Module doc (`//!`)

Every file starts with `//!`. State what the module does, where it sits in the pipeline, and any non-obvious design decisions. Match the depth to the complexity — a 50-line utility gets 2 lines, a 2000-line core module gets a paragraph.

```zig
//! Lexical scanner for Cot.
//!
//! First stage of the compilation pipeline. Reads raw source text and
//! produces a stream of tokens. String interpolation (`"hello ${name}"`)
//! requires tracking brace depth across token boundaries.
```

### Type and enum doc (`///`)

Every public type gets a `///` doc comment. For complex types, include invariants, guarantees, and usage patterns. Follow Zig's `hash_map.zig` depth — don't be shy about multi-paragraph docs on important types.

```zig
/// General purpose hash map with open addressing and linear probing.
///
/// No order is guaranteed. Any modification invalidates live iterators.
/// Provides fast operations (lookup, insertion, deletion) with load
/// factors up to 80% for low memory usage.
///
/// For ordered iteration, prefer `ArrayHashMap`.
pub const HashMap = struct {
```

### Enum variant doc (`///`)

Document every non-trivial enum variant. This is the pattern from Zig's `Air.zig` — the most comprehensive documentation in the Zig codebase. Each variant explains what it does, its type invariants, and which data field it uses.

```zig
pub const Token = enum(u8) {
    /// End of file. Every token stream terminates with this.
    eof,

    /// Identifier or variable name (not a keyword).
    /// text field contains the original characters.
    ident,

    /// Integer literal: decimal (42), hex (0xFF), octal (0o77), binary (0b1010).
    /// Underscore separators allowed (1_000_000). text field contains raw digits.
    int_lit,

    /// Start of string interpolation: `"text ${`
    /// Scanner sets in_interp_string=true and tracks brace depth.
    string_interp_start,
};
```

For simple groupings where the meaning is obvious from the name, a brief comment or no comment is fine:

```zig
    add,        // +
    sub,        // -
    mul,        // *
```

### Function doc (`///`)

Every public function and every private function that's non-trivial (> 5 lines or has subtle behavior). Include:
- What it does (first line, brief)
- Preconditions/invariants (if any)
- Return value semantics
- Side effects

```zig
/// Consume and return the next token from the source.
///
/// Skips whitespace and comments first, then dispatches based on the
/// first character. Returns .eof when the source is exhausted.
///
/// For string interpolation, may return .string_interp_start mid-string
/// and resume via scanStringContinuation when the interpolation closes.
pub fn next(self: *Scanner) TokenInfo {
```

For trivial helpers, skip the doc:

```zig
fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}
```

### No reference comments

Never document where code was ported from. The code is proven — its origin is in git history.

```zig
// NO:
// Go: ssaGenValue lines 280-284
// Swift Phase 8.5: T-indirect convention

// YES:
// Skip memory args — they exist for SSA ordering only
```

---

## Naming

Follow Zig convention exactly:

| Kind | Convention | Example |
|------|-----------|---------|
| Functions | `camelCase` | `scanIdentifier`, `makeNumberToken` |
| Local variables | `snake_case` | `is_float`, `text_start`, `found_interp` |
| Struct fields | `snake_case` | `interp_brace_depth`, `src` |
| Types | `PascalCase` | `TokenInfo`, `Scanner`, `ErrorReporter` |
| Module constants | `snake_case` | `token_strings`, `keywords` |
| Sentinel constants | `SCREAMING_CASE` | `INVALID_ID`, `FIRST_USER_TYPE` |

### Descriptive names, no abbreviations

```zig
// NO:
const kw = token.lookup(text);
var f_is_static_n = false;
const saved_tree_m = self.tree;

// YES:
const keyword = token.lookup(text);
var is_static = false;
const saved_tree = self.tree;
```

Universally understood abbreviations are fine: `i`, `n`, `buf`, `ptr`, `len`, `idx`, `fn`, `err`, `pos`, `src`.

---

## Formatting

### Blank lines for structure

Use blank lines to separate logical groups within functions and between declarations. Two blank lines before a new "section" of related functions within a struct. This replaces explicit section dividers.

```zig
pub const Scanner = struct {
    src: *Source,
    pos: Pos,
    ch: ?u8,
    err: ?*ErrorReporter,
    in_interp_string: bool,
    interp_brace_depth: u32,

    pub fn init(src: *Source) Scanner {
        return initWithErrors(src, null);
    }

    pub fn initWithErrors(src: *Source, err: ?*ErrorReporter) Scanner {
        // ...
    }


    /// Consume and return the next token.
    pub fn next(self: *Scanner) TokenInfo {
        // ...
    }


    fn scanIdentifier(self: *Scanner, start: Pos) TokenInfo {
        // ...
    }

    fn scanNumber(self: *Scanner, start: Pos) TokenInfo {
        // ...
    }

    fn makeNumberToken(self: *Scanner, start: Pos, is_float: bool) TokenInfo {
        // ...
    }


    fn scanString(self: *Scanner, start: Pos) TokenInfo {
        // ...
    }

    fn scanStringContinuation(self: *Scanner, start: Pos) TokenInfo {
        // ...
    }
```

Two blank lines between groups (init methods, main entry point, number scanning, string scanning). One blank line between functions within a group.

### Enum layout

One value per line when variants have doc comments:

```zig
pub const Token = enum(u8) {
    /// End of file.
    eof,

    /// Identifier (variable or function name).
    ident,

    /// Integer literal.
    int_lit,
};
```

Compact grouping when variants are self-explanatory:

```zig
    add,        // +
    sub,        // -
    mul,        // *
    quo,        // /
    rem,        // %
    concat,     // ++
```

### Switch formatting

Simple mappings, one per line:

```zig
'(' => .lparen,
')' => .rparen,
'[' => .lbrack,
']' => .rbrack,
```

Multi-character operator resolution, dense is acceptable:

```zig
'+' => if (self.ch == '+') blk: { self.advance(); break :blk .concat; }
       else if (self.ch == '=') blk: { self.advance(); break :blk .add_assign; }
       else .add,
```

---

## Tests

### At the bottom, always

Tests are the last thing in every file. No exceptions.

### Name describes behavior, not function

The file name provides context — don't repeat it in the test name.

```zig
// NO (redundant):
test "scanner basics" {}
test "token string representation" {}

// YES:
test "basics" {}
test "string representation" {}
```

### Consolidate related assertions

```zig
test "category checks" {
    // Literals
    try std.testing.expect(Token.ident.isLiteral());
    try std.testing.expect(Token.int_lit.isLiteral());
    try std.testing.expect(!Token.add.isLiteral());

    // Operators
    try std.testing.expect(Token.add.isOperator());
    try std.testing.expect(!Token.ident.isOperator());
}
```

Use inline comments to sub-group within a test. Don't create a separate test for each assertion.

---

## Error Handling

### Name error-reporting methods clearly

```zig
fn reportError(self: *Scanner, pos: Pos, code: ErrorCode, msg: []const u8) void {
    if (self.err) |reporter| reporter.errorWithCode(pos, code, msg);
}
```

### Return null or sentinels, don't panic

Internal functions that can't proceed return `null` or a sentinel value. Reserve `@panic` for genuinely unreachable code paths.

---

## What NOT to Do

- No `TODO`, `FIXME`, `HACK` comments in ported code
- No commented-out code blocks
- No dead functions (if nothing calls it, delete it)
- No `std.debug.print` left from debugging
- No numeric suffixes from copy-paste (`v1`, `v2`, `saved_tree_m`)
- No reference comments citing other languages' source code
