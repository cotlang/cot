# Cot Code Style Guide

**Applies to:** All files in `src/` (ported or new)
**Reference examples:** `src/libcot-zig/token.zig`, `src/libcot-zig/scanner.zig`

---

## File Structure

Every file follows the same layout, top to bottom:

```zig
//! Module-level documentation.
//!
//! 3-5 lines explaining what this file does, where it sits in the pipeline,
//! and any non-obvious design decisions.

const std = @import("std");
const foo = @import("foo.zig");     // sibling imports, alphabetical

const Foo = foo.Foo;                // type aliases, alphabetical
const Bar = foo.Bar;

// ═══════════════════════════════════════════════════════════════
// Section Name
// ═══════════════════════════════════════════════════════════════

pub const MyStruct = struct {
    // fields, then methods
};

// ═══════════════════════════════════════════════════════════════
// Free functions
// ═══════════════════════════════════════════════════════════════

fn helper() void {}

// ═══════════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════════

test "description" {}
```

---

## Section Dividers

Use the double-line box drawing character for visual separation between major sections. Always three lines: blank, divider with title, blank.

```zig

// ═══════════════════════════════════════════════════════════════
// Section Name
// ═══════════════════════════════════════════════════════════════

```

Use these to separate: imports from types, types from methods, method groups from each other, methods from free functions, free functions from tests.

Within a struct, use them to group related methods:

```zig
pub const Scanner = struct {
    // fields

    // ═══════════════════════════════════════════════════════════════
    // Main entry point
    // ═══════════════════════════════════════════════════════════════

    pub fn next() TokenInfo {}

    // ═══════════════════════════════════════════════════════════════
    // Number literals
    // ═══════════════════════════════════════════════════════════════

    fn scanNumber() TokenInfo {}
    fn makeNumberToken() TokenInfo {}
};
```

---

## Documentation Comments

### Module doc (`//!`)

Every file starts with `//!` explaining the module. 3-5 lines minimum. State:
- What this module does
- Where it sits in the pipeline
- Any key design decisions

### Struct/enum doc (`///`)

Every public struct and enum gets a `///` doc comment explaining:
- What it represents
- Key fields and why they exist

```zig
/// Lexical scanner. Reads characters from a Source and produces tokens.
///
/// String interpolation state:
///   in_interp_string   — true inside `"...${...}..."` after `${`
///   interp_brace_depth — counts nested `{` inside interpolation
pub const Scanner = struct {
```

### Method doc (`///`)

Every public method and every non-trivial private method gets a `///` doc comment. For simple one-liner helpers, skip it.

```zig
/// Consume and return the next token from the source.
///
/// Skips whitespace and comments first, then dispatches based on
/// the first character:
///   letter/_  → identifier or keyword
///   digit     → number literal
///   "         → string literal
///   other     → operator or punctuation
pub fn next(self: *Scanner) TokenInfo {
```

### No reference comments

Never write where the code came from. The code stands on its own.

```zig
// BAD:
// Go: ssaGenValue lines 280-284
// Swift Phase 8.5: T-indirect convention
// Reference: Cranelift src/machinst/compile.rs

// GOOD:
// Skip memory args — they exist for SSA ordering only
// COW: deep copy buffer only when refcount > 1
```

---

## Naming

### Variables and functions: `snake_case`

```zig
fn scan_identifier() {}     // NO — Zig uses camelCase for functions
fn scanIdentifier() {}      // YES

var is_float = false;        // YES
var isFloat = false;         // also acceptable in Zig, but prefer snake for locals
```

Follow Zig convention: `camelCase` for functions, `snake_case` for local variables and fields.

### Types: `PascalCase`

```zig
pub const TokenInfo = struct {};   // YES
pub const Scanner = struct {};     // YES
```

### Constants: `snake_case` or `SCREAMING_CASE` for true constants

```zig
const max_tokens = 256;            // local constant
pub const INVALID_ID: u32 = 0;    // module-level sentinel
```

### Descriptive names, no abbreviations

```zig
// BAD:
const kw = token.lookup(text);
var f_is_static_n = false;
const saved_tree_m = self.tree;

// GOOD:
const keyword = token.lookup(text);
var is_static = false;
const saved_tree = self.tree;
```

Exception: universally understood abbreviations are fine: `i`, `n`, `buf`, `ptr`, `len`, `idx`, `fn`, `err`.

---

## Formatting

### One concept per line in enums

```zig
// BAD — packed:
kw_fn, kw_var, kw_let, kw_const, kw_struct, kw_impl, kw_trait, kw_where,

// GOOD — one per line for keywords with comments:
kw_fn,
kw_var,
kw_let,
kw_const,

// ACCEPTABLE — grouped by category on fewer lines:
add,        // +
sub,        // -
mul,        // *
```

Use your judgement. If each item needs a comment, one per line. If they're self-explanatory and in a logical group, multiple per line is fine.

### Operator switch formatting

Multi-character operator dispatch can stay dense — it's a lookup table, not logic:

```zig
'+' => if (self.ch == '+') blk: { self.advance(); break :blk .concat; }
       else if (self.ch == '=') blk: { self.advance(); break :blk .add_assign; }
       else .add,
```

But single-character operators get one per line:

```zig
'(' => .lparen,
')' => .rparen,
'[' => .lbrack,
']' => .rbrack,
```

### Blank lines

- One blank line between methods
- Two blank lines before section dividers
- No blank lines inside short functions (< 10 lines)
- Blank line to separate logical blocks within longer functions

---

## Tests

### Test names: describe the behavior, not the function

```zig
// BAD:
test "scanner basics" {}
test "scanner operators" {}

// GOOD:
test "basics" {}        // file is scanner.zig — no prefix needed
test "operators" {}
test "numbers" {}
```

### Consolidate related assertions

```zig
// BAD — one test per check:
test "isLiteral ident" { try expect(Token.ident.isLiteral()); }
test "isLiteral int" { try expect(Token.int_lit.isLiteral()); }
test "isLiteral not operator" { try expect(!Token.add.isLiteral()); }

// GOOD — one test per category:
test "category checks" {
    // Literals
    try expect(Token.ident.isLiteral());
    try expect(Token.int_lit.isLiteral());
    try expect(!Token.add.isLiteral());

    // Operators
    try expect(Token.add.isOperator());
    try expect(!Token.ident.isOperator());
}
```

### Tests go at the bottom of the file

Always the last section, after all implementation code.

---

## Error Handling

### Method naming: `reportError` not `errorAt`

```zig
fn reportError(self: *Scanner, pos: Pos, code: ErrorCode, msg: []const u8) void {
    if (self.err) |reporter| reporter.errorWithCode(pos, code, msg);
}
```

### Fail gracefully in internal functions

Internal functions that can't proceed should return `null` or a sentinel, not panic. Only `@panic` in truly unreachable code.

---

## What NOT to Do

- No `TODO`, `FIXME`, `HACK`, `XXX` comments in ported code
- No commented-out code blocks
- No dead functions (if nothing calls it, delete it)
- No Hungarian notation (`m_count`, `s_name`, `p_value`)
- No numeric suffixes from copy-paste (`v1`, `v2`, `saved_tree_m`)
- No `std.debug.print` left from debugging sessions
