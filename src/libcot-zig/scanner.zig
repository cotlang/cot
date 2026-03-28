//! Lexical scanner for Cot.
//!
//! The scanner is the first stage of the compilation pipeline. It reads raw
//! source text character by character and produces a stream of tokens —
//! meaningful units like keywords (`fn`, `if`), literals (`42`, `"hello"`),
//! operators (`+`, `->`, `{`), and identifiers (variable/function names).
//!
//! The parser calls `next()` repeatedly to consume tokens. The scanner never
//! backtracks — it reads forward only, one token at a time.
//!
//! String interpolation (`"hello ${name}"`) is handled via two state flags
//! that track whether we're inside an interpolation and how deep the brace
//! nesting goes, so the scanner knows when the interpolation ends and the
//! string resumes.

const std = @import("std");
const token = @import("token.zig");
const source = @import("source.zig");
const errors = @import("errors.zig");

const Token = token.Token;
const Pos = source.Pos;
const Span = source.Span;
const Source = source.Source;
const ErrorReporter = errors.ErrorReporter;
const ErrorCode = errors.ErrorCode;

/// A token with its source location and original text.
/// Every call to `Scanner.next()` returns one of these.
pub const TokenInfo = struct {
    tok: Token,
    span: Span,
    text: []const u8,
};

/// Lexical scanner. Reads characters from a Source and produces tokens.
///
/// State:
///   pos  — current byte position in source (also tracks line/column)
///   ch   — current character (null = end of file)
///
/// String interpolation state:
///   in_interp_string   — true when inside `"...${...}..."` and we've
///                         returned the `${` token but not yet the closing `}`
///   interp_brace_depth — counts nested `{` inside interpolation so we
///                         don't mistake an inner `}` for the end
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
        var s = Scanner{
            .src = src,
            .pos = Pos.zero,
            .ch = null,
            .err = err,
            .in_interp_string = false,
            .interp_brace_depth = 0,
        };
        s.ch = src.at(s.pos);
        return s;
    }

    fn reportError(self: *Scanner, pos: Pos, code: ErrorCode, msg: []const u8) void {
        if (self.err) |reporter| reporter.errorWithCode(pos, code, msg);
    }

    // Main entry point — called by the parser to get the next token

    /// Consume and return the next token from the source.
    ///
    /// Skips whitespace and comments first, then dispatches based on
    /// the first character:
    ///   letter/_  → identifier or keyword
    ///   digit     → number literal
    ///   "         → string literal
    ///   '         → character literal
    ///   ///       → doc comment (returned as token, not skipped)
    ///   other     → operator or punctuation
    pub fn next(self: *Scanner) TokenInfo {
        self.skipWhitespaceAndComments();
        const start = self.pos;

        if (self.ch == null)
            return .{ .tok = .eof, .span = Span.fromPos(start), .text = "" };

        if (self.ch == '/' and self.peek(1) == '/' and self.peek(2) == '/')
            return self.scanDocComment(start);

        const c = self.ch.?;
        if (isAlpha(c) or c == '_') return self.scanIdentifier(start);
        if (isDigit(c)) return self.scanNumber(start);
        if (c == '"') return self.scanString(start);
        if (c == '\'') return self.scanChar(start);
        return self.scanOperator(start);
    }

    // Whitespace and comment skipping

    /// Advance past whitespace, line comments (//), and block comments (/* */).
    /// Doc comments (///) are NOT skipped — they're returned as tokens by next().
    fn skipWhitespaceAndComments(self: *Scanner) void {
        while (self.ch) |c| {
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                self.advance();
            } else if (c == '/' and self.peek(1) == '/' and self.peek(2) != '/') {
                // Line comment — skip to end of line
                self.advance();
                self.advance();
                while (self.ch) |cc| {
                    if (cc == '\n') {
                        self.advance();
                        break;
                    }
                    self.advance();
                }
            } else if (c == '/' and self.peek(1) == '/' and self.peek(2) == '/') {
                // Doc comment — stop skipping, let next() return it as a token
                break;
            } else if (c == '/' and self.peek(1) == '*') {
                // Block comment — skip to closing */
                self.advance();
                self.advance();
                while (self.ch != null) {
                    if (self.ch == '*' and self.peek(1) == '/') {
                        self.advance();
                        self.advance();
                        break;
                    }
                    self.advance();
                }
            } else break;
        }
    }

    // Doc comments

    /// Scan a `///` doc comment line. Returns the text content after the prefix.
    /// Each line is a separate doc_comment token — the parser concatenates them.
    fn scanDocComment(self: *Scanner, start: Pos) TokenInfo {
        self.advance(); // /
        self.advance(); // /
        self.advance(); // /
        if (self.ch == ' ') self.advance(); // optional space after ///

        const text_start = self.pos.offset;
        while (self.ch) |cc| {
            if (cc == '\n') break;
            self.advance();
        }
        const text = self.src.content[text_start..self.pos.offset];
        if (self.ch == '\n') self.advance();

        return .{ .tok = .doc_comment, .span = Span.init(start, self.pos), .text = text };
    }

    // Identifiers and keywords

    /// Scan an identifier (variable name, function name) or keyword.
    /// After consuming all alphanumeric/underscore characters, looks up the
    /// text in the keyword table. If it matches a keyword (fn, if, return, etc.),
    /// returns that token type. Otherwise returns .ident with the raw text.
    fn scanIdentifier(self: *Scanner, start: Pos) TokenInfo {
        while (self.ch) |c| {
            if (isAlphaNumeric(c) or c == '_') self.advance() else break;
        }
        const text = self.src.content[start.offset..self.pos.offset];
        const keyword = token.lookup(text);
        if (keyword != .ident)
            return .{ .tok = keyword, .span = Span.init(start, self.pos), .text = "" };
        return .{ .tok = .ident, .span = Span.init(start, self.pos), .text = text };
    }

    // Number literals

    /// Scan a number literal. Handles:
    ///   Decimal:     42, 1_000_000
    ///   Hex:         0xFF, 0XFF
    ///   Octal:       0o77, 0O77
    ///   Binary:      0b1010, 0B1010
    ///   Float:       3.14, 1.0e10, 2.5E-3
    ///   Separators:  1_000_000 (underscores ignored, readability only)
    fn scanNumber(self: *Scanner, start: Pos) TokenInfo {
        var is_float = false;

        // Check for base prefixes: 0x, 0o, 0b
        if (self.ch == '0') {
            self.advance();
            if (self.ch) |c| {
                if (c == 'x' or c == 'X') {
                    self.advance();
                    while (self.ch) |cc| if (isHexDigit(cc) or cc == '_') self.advance() else break;
                    return self.makeNumberToken(start, false);
                } else if (c == 'o' or c == 'O') {
                    self.advance();
                    while (self.ch) |cc| if ((cc >= '0' and cc <= '7') or cc == '_') self.advance() else break;
                    return self.makeNumberToken(start, false);
                } else if (c == 'b' or c == 'B') {
                    self.advance();
                    while (self.ch) |cc| if (cc == '0' or cc == '1' or cc == '_') self.advance() else break;
                    return self.makeNumberToken(start, false);
                }
            }
        }

        // Decimal digits
        while (self.ch) |c| if (isDigit(c) or c == '_') self.advance() else break;

        // Fractional part: must be `.` followed by non-`.` (to distinguish 1..10 range from 1.0)
        if (self.ch == '.' and self.peek(1) != '.') {
            is_float = true;
            self.advance();
            while (self.ch) |c| if (isDigit(c) or c == '_') self.advance() else break;
        }

        // Scientific notation: 1e10, 2.5E-3
        if (self.ch) |c| {
            if (c == 'e' or c == 'E') {
                is_float = true;
                self.advance();
                if (self.ch == '+' or self.ch == '-') self.advance();
                while (self.ch) |cc| if (isDigit(cc) or cc == '_') self.advance() else break;
            }
        }

        return self.makeNumberToken(start, is_float);
    }

    fn makeNumberToken(self: *Scanner, start: Pos, is_float: bool) TokenInfo {
        return .{
            .tok = if (is_float) .float_lit else .int_lit,
            .span = Span.init(start, self.pos),
            .text = self.src.content[start.offset..self.pos.offset],
        };
    }

    // String literals and interpolation

    /// Scan a string literal starting with `"`.
    ///
    /// Three possible outcomes:
    ///   .string_lit         — plain string `"hello"`
    ///   .string_interp_start — start of interpolated string `"hello ${`
    ///   error               — unterminated string
    ///
    /// When `${` is found, sets interpolation state so the scanner knows
    /// to resume the string after the expression closes with `}`.
    fn scanString(self: *Scanner, start: Pos) TokenInfo {
        self.advance(); // opening "
        var terminated = false;
        var found_interp = false;

        while (self.ch) |c| {
            if (c == '"') {
                self.advance();
                terminated = true;
                break;
            } else if (c == '\\') {
                self.advance(); // backslash
                if (self.ch != null) self.advance(); // escaped character
            } else if (c == '$' and self.peek(1) == '{') {
                self.advance(); // $
                self.advance(); // {
                found_interp = true;
                self.in_interp_string = true;
                self.interp_brace_depth = 1;
                break;
            } else if (c == '\n') {
                break; // strings don't span lines
            } else {
                self.advance();
            }
        }

        if (!terminated and !found_interp)
            self.reportError(start, .e100, "string literal not terminated");

        const text = self.src.content[start.offset..self.pos.offset];
        if (found_interp)
            return .{ .tok = .string_interp_start, .span = Span.init(start, self.pos), .text = text };
        return .{ .tok = .string_lit, .span = Span.init(start, self.pos), .text = text };
    }

    /// Resume scanning a string after an interpolation expression closes.
    /// Called when `}` drops interp_brace_depth to 0 in scanOperator.
    ///
    /// Two possible outcomes:
    ///   .string_interp_mid — another interpolation follows `...${`
    ///   .string_interp_end — string finishes `..."`
    fn scanStringContinuation(self: *Scanner, start: Pos) TokenInfo {
        var terminated = false;
        var found_interp = false;

        while (self.ch) |c| {
            if (c == '"') {
                self.advance();
                terminated = true;
                self.in_interp_string = false;
                break;
            } else if (c == '\\') {
                self.advance();
                if (self.ch != null) self.advance();
            } else if (c == '$' and self.peek(1) == '{') {
                self.advance();
                self.advance();
                found_interp = true;
                self.interp_brace_depth = 1;
                break;
            } else if (c == '\n') {
                break;
            } else {
                self.advance();
            }
        }

        if (!terminated and !found_interp)
            self.reportError(start, .e100, "string literal not terminated");

        const text = self.src.content[start.offset..self.pos.offset];
        if (found_interp)
            return .{ .tok = .string_interp_mid, .span = Span.init(start, self.pos), .text = text };
        return .{ .tok = .string_interp_end, .span = Span.init(start, self.pos), .text = text };
    }

    // Character literals

    /// Scan a character literal: 'a', '\n', '\\'
    fn scanChar(self: *Scanner, start: Pos) TokenInfo {
        self.advance(); // opening '
        if (self.ch == '\\') {
            self.advance(); // backslash
            if (self.ch != null) self.advance(); // escaped char
        } else if (self.ch != null and self.ch != '\'') {
            self.advance(); // the character
        }
        var terminated = false;
        if (self.ch == '\'') {
            self.advance();
            terminated = true;
        }
        if (!terminated)
            self.reportError(start, .e101, "character literal not terminated");
        return .{
            .tok = .char_lit,
            .span = Span.init(start, self.pos),
            .text = self.src.content[start.offset..self.pos.offset],
        };
    }

    // Operators and punctuation

    /// Scan an operator or punctuation token.
    ///
    /// Most operators are 1-2 characters. Multi-character operators are
    /// resolved by peeking at the next character:
    ///   +  → add       ++ → concat     += → add_assign
    ///   =  → assign    == → eql        => → fat_arrow
    ///   .  → period    .. → range      .* → deref      .? → optional unwrap
    ///
    /// Also handles string interpolation brace tracking — `{` inside an
    /// interpolation increments depth, `}` decrements. When depth hits 0,
    /// the string resumes via scanStringContinuation.
    fn scanOperator(self: *Scanner, start: Pos) TokenInfo {
        const c = self.ch.?;
        self.advance();

        // String interpolation brace tracking
        if (c == '{' and self.in_interp_string) {
            self.interp_brace_depth += 1;
            return .{ .tok = .lbrace, .span = Span.init(start, self.pos), .text = "" };
        }
        if (c == '}' and self.in_interp_string) {
            self.interp_brace_depth -= 1;
            if (self.interp_brace_depth == 0) return self.scanStringContinuation(start);
            return .{ .tok = .rbrace, .span = Span.init(start, self.pos), .text = "" };
        }

        const tok: Token = switch (c) {
            '(' => .lparen,
            ')' => .rparen,
            '[' => .lbrack,
            ']' => .rbrack,
            '{' => .lbrace,
            '}' => .rbrace,
            ',' => .comma,
            ';' => .semicolon,
            ':' => .colon,
            '~' => .not,
            '@' => if (self.ch == '"') {
                // Quoted identifier: @"keyword" allows using reserved words as names
                self.advance();
                const text_start = self.pos.offset;
                var ident_terminated = false;
                while (self.ch) |cc| {
                    if (cc == '"') { ident_terminated = true; break; }
                    if (cc == '\n') break;
                    self.advance();
                }
                const ident_text = self.src.content[text_start..self.pos.offset];
                if (ident_terminated) {
                    self.advance();
                } else {
                    self.reportError(start, .e100, "quoted identifier not terminated");
                }
                return .{ .tok = .ident, .span = Span.init(start, self.pos), .text = ident_text };
            } else .at,
            '+' => if (self.ch == '+') blk: { self.advance(); break :blk .concat; } else if (self.ch == '=') blk: { self.advance(); break :blk .add_assign; } else .add,
            '-' => if (self.ch == '=') blk: { self.advance(); break :blk .sub_assign; } else if (self.ch == '>') blk: { self.advance(); break :blk .arrow; } else .sub,
            '*' => if (self.ch == '=') blk: { self.advance(); break :blk .mul_assign; } else .mul,
            '/' => if (self.ch == '=') blk: { self.advance(); break :blk .quo_assign; } else .quo,
            '%' => if (self.ch == '=') blk: { self.advance(); break :blk .rem_assign; } else .rem,
            '&' => if (self.ch == '&') blk: { self.advance(); break :blk .land; } else if (self.ch == '=') blk: { self.advance(); break :blk .and_assign; } else .@"and",
            '|' => if (self.ch == '|') blk: { self.advance(); break :blk .lor; } else if (self.ch == '=') blk: { self.advance(); break :blk .or_assign; } else .@"or",
            '^' => if (self.ch == '=') blk: { self.advance(); break :blk .xor_assign; } else .xor,
            '=' => if (self.ch == '=') blk: { self.advance(); break :blk .eql; } else if (self.ch == '>') blk: { self.advance(); break :blk .fat_arrow; } else .assign,
            '!' => if (self.ch == '=') blk: { self.advance(); break :blk .neq; } else .lnot,
            '<' => if (self.ch == '=') blk: { self.advance(); break :blk .leq; } else if (self.ch == '<') blk: { self.advance(); break :blk .shl; } else .lss,
            '>' => if (self.ch == '=') blk: { self.advance(); break :blk .geq; } else if (self.ch == '>') blk: { self.advance(); break :blk .shr; } else .gtr,
            '.' => if (self.ch == '.') blk: { self.advance(); break :blk .period_period; } else if (self.ch == '*') blk: { self.advance(); break :blk .period_star; } else if (self.ch == '?') blk: { self.advance(); break :blk .period_question; } else .period,
            '?' => if (self.ch == '.') blk: { self.advance(); break :blk .optional_chain; } else .question,
            else => .illegal,
        };

        if (tok == .illegal)
            self.reportError(start, .e104, "unexpected character");
        return .{ .tok = tok, .span = Span.init(start, self.pos), .text = "" };
    }

    // Character movement

    /// Advance the scanner by one character.
    fn advance(self: *Scanner) void {
        self.pos = self.pos.advance(1);
        self.ch = self.src.at(self.pos);
    }

    /// Peek at the character n positions ahead without consuming.
    fn peek(self: *Scanner, n: u32) ?u8 {
        return self.src.at(self.pos.advance(n));
    }
};

// Character classification helpers

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn isAlphaNumeric(c: u8) bool {
    return isAlpha(c) or isDigit(c);
}

// Tests

test "basics" {
    const content = "fn main() { return 42 }";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_fn, scanner.next().tok);
    var tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("main", tok.text);
    try std.testing.expectEqual(Token.lparen, scanner.next().tok);
    try std.testing.expectEqual(Token.rparen, scanner.next().tok);
    try std.testing.expectEqual(Token.lbrace, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_return, scanner.next().tok);
    tok = scanner.next();
    try std.testing.expectEqual(Token.int_lit, tok.tok);
    try std.testing.expectEqualStrings("42", tok.text);
    try std.testing.expectEqual(Token.rbrace, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "operators" {
    const content = "== != <= >= << >> .* .? ?. orelse";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.eql, scanner.next().tok);
    try std.testing.expectEqual(Token.neq, scanner.next().tok);
    try std.testing.expectEqual(Token.leq, scanner.next().tok);
    try std.testing.expectEqual(Token.geq, scanner.next().tok);
    try std.testing.expectEqual(Token.shl, scanner.next().tok);
    try std.testing.expectEqual(Token.shr, scanner.next().tok);
    try std.testing.expectEqual(Token.period_star, scanner.next().tok);
    try std.testing.expectEqual(Token.period_question, scanner.next().tok);
    try std.testing.expectEqual(Token.optional_chain, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_orelse, scanner.next().tok);
}

test "strings" {
    const content = "\"hello world\" \"with \\\"escape\\\"\"";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.string_lit, tok.tok);
    try std.testing.expectEqualStrings("\"hello world\"", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.string_lit, tok.tok);
}

test "numbers" {
    const content = "42 3.14 0xFF 0b1010 0o777 1_000_000";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.int_lit, tok.tok);
    try std.testing.expectEqualStrings("42", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.float_lit, tok.tok);
    try std.testing.expectEqualStrings("3.14", tok.text);
    tok = scanner.next();
    try std.testing.expectEqualStrings("0xFF", tok.text);
    tok = scanner.next();
    try std.testing.expectEqualStrings("0b1010", tok.text);
    tok = scanner.next();
    try std.testing.expectEqualStrings("0o777", tok.text);
    tok = scanner.next();
    try std.testing.expectEqualStrings("1_000_000", tok.text);
}

test "comments" {
    const content = "// line comment\nx /* block */ y";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("x", tok.text);
    tok = scanner.next();
    try std.testing.expectEqualStrings("y", tok.text);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "keywords" {
    const content = "fn var const if else while for return";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_fn, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_var, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_const, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_if, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_else, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_while, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_for, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_return, scanner.next().tok);
}

test "type keywords" {
    const content = "int float bool string i64 u8";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_int, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_float, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_bool, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_string, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_i64, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_u8, scanner.next().tok);
}

test "character literals" {
    const content = "'a' '\\n' '\\\\'";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.char_lit, tok.tok);
    try std.testing.expectEqualStrings("'a'", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.char_lit, tok.tok);
    tok = scanner.next();
    try std.testing.expectEqual(Token.char_lit, tok.tok);
}

test "concat operator" {
    const content = "a ++ b";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("a", tok.text);
    try std.testing.expectEqual(Token.concat, scanner.next().tok);
    tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("b", tok.text);
}

test "compound assignment" {
    const content = "+= -= *= /= %= &= |= ^=";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.add_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.sub_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.mul_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.quo_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.rem_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.and_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.or_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.xor_assign, scanner.next().tok);
}

test "arrows" {
    const content = "-> =>";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.arrow, scanner.next().tok);
    try std.testing.expectEqual(Token.fat_arrow, scanner.next().tok);
}
