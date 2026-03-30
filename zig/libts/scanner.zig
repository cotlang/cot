//! Lexical scanner for TypeScript/JavaScript.
//!
//! Ported from references/typescript-go/internal/scanner/scanner.go.
//! Follows the same patterns as zig/libcot/frontend/scanner.zig:
//!   - Scanner struct with src, pos, ch
//!   - next() → TokenInfo as the main entry point
//!   - Reuses Source/Pos/Span from libcot/frontend/source.zig
//!
//! TS/JS-specific features:
//!   - Template literals: backtick state machine (head/middle/tail/none)
//!   - Single-quoted strings
//!   - Regex literals (parser-driven rescan)
//!   - Private identifiers (#name)
//!   - BigInt literals (42n)
//!   - ===, !==, **, >>>, ??, ..., ++, --, #
//!   - Contextual keywords (interface, type, as, etc.)
//!   - Shebang (#!) support

const std = @import("std");
const token = @import("token.zig");
const source = @import("source.zig");

const Token = token.Token;
const Pos = source.Pos;
const Span = source.Span;
const Source = source.Source;

pub const TokenInfo = struct {
    tok: Token,
    span: Span,
    text: []const u8,
};

pub const Scanner = struct {
    src: *Source,
    pos: Pos,
    ch: ?u8,
    /// The token kind of the most recently returned token (for regex disambiguation).
    prev_token: Token,
    /// Whether we are inside a template literal expression `${...}`.
    in_template_expr: bool,
    /// Brace nesting depth within template expression (to match `}` correctly).
    template_brace_depth: u32,
    /// Stack of template nesting depths for nested templates.
    /// e.g. `a ${`b ${c}` }` — outer template pauses while inner runs.
    template_depth_stack: [16]u32,
    template_depth_count: u8,

    pub fn init(src: *Source) Scanner {
        var s = Scanner{
            .src = src,
            .pos = Pos.zero,
            .ch = null,
            .prev_token = .eof,
            .in_template_expr = false,
            .template_brace_depth = 0,
            .template_depth_stack = [_]u32{0} ** 16,
            .template_depth_count = 0,
        };
        s.ch = src.at(s.pos);
        // Handle shebang: #!/usr/bin/env node
        if (s.ch == '#' and s.peek(1) == '!') {
            s.skipShebang();
        }
        return s;
    }

    fn skipShebang(self: *Scanner) void {
        // Skip #! line
        while (self.ch) |c| {
            if (c == '\n') {
                self.advance();
                break;
            }
            self.advance();
        }
    }

    /// Main entry point: return the next token.
    pub fn next(self: *Scanner) TokenInfo {
        self.skipWhitespaceAndComments();
        const start = self.pos;

        if (self.ch == null) return self.makeToken(.eof, start, "");

        const c = self.ch.?;

        // Template literal continuation: after `}` closes a template expression
        if (c == '}' and self.in_template_expr and self.template_brace_depth == 0) {
            return self.scanTemplateContinuation(start);
        }

        if (isIdentStart(c)) return self.scanIdentifier(start);
        if (isDigit(c)) return self.scanNumber(start);
        if (c == '"' or c == '\'') return self.scanString(start);
        if (c == '`') return self.scanTemplateLiteral(start);
        return self.scanOperator(start);
    }

    /// Rescan the current `/` or `/=` token as a regex literal.
    /// Called by the parser when it determines the context expects a regex.
    pub fn reScanSlashAsRegex(self: *Scanner, slash_start: Pos) TokenInfo {
        // Reset position to the start of the slash token
        self.pos = slash_start;
        self.ch = self.src.at(self.pos);
        return self.scanRegex(slash_start);
    }

    fn makeToken(self: *Scanner, tok: Token, start: Pos, text: []const u8) TokenInfo {
        self.prev_token = tok;
        return .{ .tok = tok, .span = Span.init(start, self.pos), .text = text };
    }

    // ====================================================================
    // Whitespace and comments
    // ====================================================================

    fn skipWhitespaceAndComments(self: *Scanner) void {
        while (self.ch) |c| {
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r' or c == 0x0C) {
                self.advance();
            } else if (c == '/' and self.peek(1) == '/') {
                // Single-line comment
                self.advance();
                self.advance();
                while (self.ch) |cc| {
                    if (cc == '\n') {
                        self.advance();
                        break;
                    }
                    self.advance();
                }
            } else if (c == '/' and self.peek(1) == '*') {
                // Multi-line comment
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

    // ====================================================================
    // Identifiers and keywords
    // ====================================================================

    fn scanIdentifier(self: *Scanner, start: Pos) TokenInfo {
        while (self.ch) |c| {
            if (isIdentPart(c)) self.advance() else break;
        }
        const text = self.src.content[start.offset..self.pos.offset];
        const kw = token.lookup(text);
        if (kw != .ident) return self.makeToken(kw, start, text);
        return self.makeToken(.ident, start, text);
    }

    // ====================================================================
    // Numbers
    // ====================================================================

    fn scanNumber(self: *Scanner, start: Pos) TokenInfo {
        var is_float = false;
        var is_bigint = false;

        if (self.ch == '0') {
            self.advance();
            if (self.ch) |c| {
                if (c == 'x' or c == 'X') {
                    self.advance();
                    self.scanDigits(isHexDigit);
                    if (self.ch == 'n') {
                        self.advance();
                        is_bigint = true;
                    }
                    return self.makeNumberToken(start, is_float, is_bigint);
                } else if (c == 'o' or c == 'O') {
                    self.advance();
                    self.scanDigits(isOctalDigit);
                    if (self.ch == 'n') {
                        self.advance();
                        is_bigint = true;
                    }
                    return self.makeNumberToken(start, is_float, is_bigint);
                } else if (c == 'b' or c == 'B') {
                    self.advance();
                    self.scanDigits(isBinaryDigit);
                    if (self.ch == 'n') {
                        self.advance();
                        is_bigint = true;
                    }
                    return self.makeNumberToken(start, is_float, is_bigint);
                }
            }
        }

        // Decimal digits
        self.scanDigits(isDigit);

        // Fractional part
        if (self.ch == '.' and !isIdentStart(self.peek(1) orelse 0)) {
            // Check it's not a method call like 42.toString()
            const next_ch = self.peek(1);
            if (next_ch == null or isDigit(next_ch.?)) {
                is_float = true;
                self.advance(); // consume .
                self.scanDigits(isDigit);
            }
        }

        // Exponent
        if (self.ch) |c| {
            if (c == 'e' or c == 'E') {
                is_float = true;
                self.advance();
                if (self.ch == '+' or self.ch == '-') self.advance();
                self.scanDigits(isDigit);
            }
        }

        // BigInt suffix
        if (!is_float and self.ch == 'n') {
            self.advance();
            is_bigint = true;
        }

        return self.makeNumberToken(start, is_float, is_bigint);
    }

    fn scanDigits(self: *Scanner, predicate: *const fn (u8) bool) void {
        while (self.ch) |c| {
            if (predicate(c) or c == '_') self.advance() else break;
        }
    }

    fn makeNumberToken(self: *Scanner, start: Pos, is_float: bool, is_bigint: bool) TokenInfo {
        const text = self.src.content[start.offset..self.pos.offset];
        const tok: Token = if (is_bigint) .bigint_lit else if (is_float) .float_lit else .int_lit;
        return self.makeToken(tok, start, text);
    }

    // ====================================================================
    // Strings (single and double quoted)
    // ====================================================================

    fn scanString(self: *Scanner, start: Pos) TokenInfo {
        const quote = self.ch.?;
        self.advance(); // opening quote

        while (self.ch) |c| {
            if (c == quote) {
                self.advance();
                break;
            } else if (c == '\\') {
                self.advance(); // backslash
                if (self.ch != null) self.advance(); // escaped char
            } else if (c == '\n' or c == '\r') {
                // Unterminated string (newline in single/double quoted string)
                break;
            } else {
                self.advance();
            }
        }

        const text = self.src.content[start.offset..self.pos.offset];
        return self.makeToken(.string_lit, start, text);
    }

    // ====================================================================
    // Template literals (backtick strings with ${} expressions)
    // ====================================================================
    //
    // Template literal state machine (matches TypeScript-Go scanner):
    //   ` text `           → template_none
    //   ` text ${          → template_head, then scan expressions
    //   } text ${          → template_middle (after expression closes)
    //   } text `           → template_tail (final segment)
    //
    // Nesting is supported: `a ${ `b ${c}` }` works correctly via
    // template_depth_stack which saves/restores brace depth.

    fn scanTemplateLiteral(self: *Scanner, start: Pos) TokenInfo {
        self.advance(); // consume opening backtick
        return self.scanTemplateContent(start, true);
    }

    fn scanTemplateContinuation(self: *Scanner, start: Pos) TokenInfo {
        self.advance(); // consume closing }
        self.in_template_expr = false;
        // Restore outer template brace depth if nested
        if (self.template_depth_count > 0) {
            self.template_depth_count -= 1;
            self.template_brace_depth = self.template_depth_stack[self.template_depth_count];
            self.in_template_expr = true;
        }
        return self.scanTemplateContent(start, false);
    }

    fn scanTemplateContent(self: *Scanner, start: Pos, is_head: bool) TokenInfo {
        while (self.ch) |c| {
            if (c == '`') {
                self.advance(); // consume closing backtick
                const text = self.src.content[start.offset..self.pos.offset];
                const tok: Token = if (is_head) .template_none else .template_tail;
                return self.makeToken(tok, start, text);
            } else if (c == '$' and self.peek(1) == '{') {
                self.advance(); // $
                self.advance(); // {
                const text = self.src.content[start.offset..self.pos.offset];
                const tok: Token = if (is_head) .template_head else .template_middle;
                // Push current brace depth if we're already in a template expression
                if (self.in_template_expr) {
                    if (self.template_depth_count < 16) {
                        self.template_depth_stack[self.template_depth_count] = self.template_brace_depth;
                        self.template_depth_count += 1;
                    }
                }
                self.in_template_expr = true;
                self.template_brace_depth = 0;
                return self.makeToken(tok, start, text);
            } else if (c == '\\') {
                self.advance(); // backslash
                if (self.ch != null) self.advance(); // escaped char
            } else {
                self.advance();
            }
        }
        // Unterminated template literal
        const text = self.src.content[start.offset..self.pos.offset];
        const tok: Token = if (is_head) .template_none else .template_tail;
        return self.makeToken(tok, start, text);
    }

    // ====================================================================
    // Regex literals (/pattern/flags)
    // ====================================================================
    //
    // The scanner initially scans `/` as .quo (division). When the parser
    // determines from context that a regex is expected, it calls
    // reScanSlashAsRegex() which re-reads from the slash position.
    //
    // Reference: scanner.go ReScanSlashToken() lines 1046-1199.

    fn scanRegex(self: *Scanner, start: Pos) TokenInfo {
        self.advance(); // consume opening /
        var in_character_class = false;

        while (self.ch) |c| {
            if (c == '/' and !in_character_class) {
                self.advance(); // consume closing /
                // Scan flags
                while (self.ch) |fc| {
                    if (isIdentPart(fc)) self.advance() else break;
                }
                const text = self.src.content[start.offset..self.pos.offset];
                return self.makeToken(.regex_lit, start, text);
            } else if (c == '[') {
                in_character_class = true;
                self.advance();
            } else if (c == ']') {
                in_character_class = false;
                self.advance();
            } else if (c == '\\') {
                self.advance(); // backslash
                if (self.ch != null) self.advance(); // escaped char
            } else if (c == '\n' or c == '\r') {
                // Unterminated regex
                break;
            } else {
                self.advance();
            }
        }
        // Unterminated — return as illegal
        const text = self.src.content[start.offset..self.pos.offset];
        return self.makeToken(.illegal, start, text);
    }

    // ====================================================================
    // Operators and punctuation
    // ====================================================================

    fn scanOperator(self: *Scanner, start: Pos) TokenInfo {
        const c = self.ch.?;
        self.advance();

        // Track braces for template expression nesting
        if (c == '{' and self.in_template_expr) {
            self.template_brace_depth += 1;
            return self.makeToken(.lbrace, start, "");
        }
        if (c == '}' and self.in_template_expr) {
            if (self.template_brace_depth > 0) {
                self.template_brace_depth -= 1;
                return self.makeToken(.rbrace, start, "");
            }
            // template_brace_depth == 0 is handled in next() before we get here
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
            '@' => .at,
            '#' => blk: {
                // Private identifier: #name
                if (self.ch != null and isIdentStart(self.ch.?)) {
                    while (self.ch) |cc| {
                        if (isIdentPart(cc)) self.advance() else break;
                    }
                    const text = self.src.content[start.offset..self.pos.offset];
                    return self.makeToken(.private_ident, start, text);
                }
                break :blk .hash;
            },
            '+' => if (self.ch == '+') blk: {
                self.advance();
                break :blk .increment;
            } else if (self.ch == '=') blk: {
                self.advance();
                break :blk .add_assign;
            } else .add,
            '-' => if (self.ch == '-') blk: {
                self.advance();
                break :blk .decrement;
            } else if (self.ch == '=') blk: {
                self.advance();
                break :blk .sub_assign;
            } else .sub,
            '*' => if (self.ch == '*') blk: {
                self.advance();
                if (self.ch == '=') {
                    self.advance();
                    break :blk .power_assign;
                }
                break :blk .power;
            } else if (self.ch == '=') blk: {
                self.advance();
                break :blk .mul_assign;
            } else .mul,
            '/' => if (self.ch == '=') blk: {
                self.advance();
                break :blk .quo_assign;
            } else .quo,
            '%' => if (self.ch == '=') blk: {
                self.advance();
                break :blk .rem_assign;
            } else .rem,
            '&' => if (self.ch == '&') blk: {
                self.advance();
                if (self.ch == '=') {
                    self.advance();
                    break :blk .land_assign;
                }
                break :blk .land;
            } else if (self.ch == '=') blk: {
                self.advance();
                break :blk .and_assign;
            } else .@"and",
            '|' => if (self.ch == '|') blk: {
                self.advance();
                if (self.ch == '=') {
                    self.advance();
                    break :blk .lor_assign;
                }
                break :blk .lor;
            } else if (self.ch == '=') blk: {
                self.advance();
                break :blk .or_assign;
            } else .@"or",
            '^' => if (self.ch == '=') blk: {
                self.advance();
                break :blk .xor_assign;
            } else .xor,
            '=' => if (self.ch == '=') blk: {
                self.advance();
                if (self.ch == '=') {
                    self.advance();
                    break :blk .strict_eql;
                }
                break :blk .eql;
            } else if (self.ch == '>') blk: {
                self.advance();
                break :blk .arrow;
            } else .assign,
            '!' => if (self.ch == '=') blk: {
                self.advance();
                if (self.ch == '=') {
                    self.advance();
                    break :blk .strict_neq;
                }
                break :blk .neq;
            } else .lnot,
            '<' => if (self.ch == '=') blk: {
                self.advance();
                break :blk .leq;
            } else if (self.ch == '<') blk: {
                self.advance();
                if (self.ch == '=') {
                    self.advance();
                    break :blk .shl_assign;
                }
                break :blk .shl;
            } else .lss,
            '>' => if (self.ch == '=') blk: {
                self.advance();
                break :blk .geq;
            } else if (self.ch == '>') blk: {
                self.advance();
                if (self.ch == '>') {
                    self.advance();
                    if (self.ch == '=') {
                        self.advance();
                        break :blk .unsigned_shr_assign;
                    }
                    break :blk .unsigned_shr;
                }
                if (self.ch == '=') {
                    self.advance();
                    break :blk .shr_assign;
                }
                break :blk .shr;
            } else .gtr,
            '.' => if (self.ch != null and isDigit(self.ch.?)) blk: {
                // Numeric literal starting with dot: .123
                break :blk @as(Token, .period); // let scanNumber handle it
            } else if (self.ch == '.' and self.peek(1) == '.') blk: {
                self.advance(); // second .
                self.advance(); // third .
                break :blk .ellipsis;
            } else .period,
            '?' => if (self.ch == '.') blk: {
                // ?. but not ?.digit (that would be ternary + number)
                const next_ch = self.peek(1);
                if (next_ch == null or !isDigit(next_ch.?)) {
                    self.advance();
                    break :blk .optional_chain;
                }
                break :blk .question;
            } else if (self.ch == '?') blk: {
                self.advance();
                if (self.ch == '=') {
                    self.advance();
                    break :blk .nullish_assign;
                }
                break :blk .nullish_coalesce;
            } else .question,
            else => .illegal,
        };

        return self.makeToken(tok, start, "");
    }

    // ====================================================================
    // Character classification
    // ====================================================================

    fn advance(self: *Scanner) void {
        self.pos = self.pos.advance(1);
        self.ch = self.src.at(self.pos);
    }

    fn peek(self: *Scanner, n: u32) ?u8 {
        return self.src.at(self.pos.advance(n));
    }
};

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c == '$';
}

fn isIdentPart(c: u8) bool {
    return isIdentStart(c) or isDigit(c);
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isHexDigit(c: u8) bool {
    return isDigit(c) or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

fn isOctalDigit(c: u8) bool {
    return c >= '0' and c <= '7';
}

fn isBinaryDigit(c: u8) bool {
    return c == '0' or c == '1';
}

// ============================================================================
// Tests
// ============================================================================

test "scanner basics" {
    const content = "function main() { return 42; }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_function, scanner.next().tok);
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
    try std.testing.expectEqual(Token.semicolon, scanner.next().tok);
    try std.testing.expectEqual(Token.rbrace, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner TS operators" {
    const content = "=== !== ** >>> ?? ... ?. ++ -- **= ??= &&= ||=";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.strict_eql, scanner.next().tok);
    try std.testing.expectEqual(Token.strict_neq, scanner.next().tok);
    try std.testing.expectEqual(Token.power, scanner.next().tok);
    try std.testing.expectEqual(Token.unsigned_shr, scanner.next().tok);
    try std.testing.expectEqual(Token.nullish_coalesce, scanner.next().tok);
    try std.testing.expectEqual(Token.ellipsis, scanner.next().tok);
    try std.testing.expectEqual(Token.optional_chain, scanner.next().tok);
    try std.testing.expectEqual(Token.increment, scanner.next().tok);
    try std.testing.expectEqual(Token.decrement, scanner.next().tok);
    try std.testing.expectEqual(Token.power_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.nullish_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.land_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.lor_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner shift operators" {
    const content = "<< >> >>> <<= >>= >>>=";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.shl, scanner.next().tok);
    try std.testing.expectEqual(Token.shr, scanner.next().tok);
    try std.testing.expectEqual(Token.unsigned_shr, scanner.next().tok);
    try std.testing.expectEqual(Token.shl_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.shr_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.unsigned_shr_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner equality operators" {
    const content = "== != === !==";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.eql, scanner.next().tok);
    try std.testing.expectEqual(Token.neq, scanner.next().tok);
    try std.testing.expectEqual(Token.strict_eql, scanner.next().tok);
    try std.testing.expectEqual(Token.strict_neq, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner strings" {
    const content =
        \\"hello world" 'single quoted' "with \"escape\""
    ;
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.string_lit, tok.tok);
    try std.testing.expectEqualStrings("\"hello world\"", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.string_lit, tok.tok);
    try std.testing.expectEqualStrings("'single quoted'", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.string_lit, tok.tok);
}

test "scanner numbers" {
    const content = "42 3.14 0xFF 0b1010 0o777 1_000_000 1e10 42n 0xFFn";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.int_lit, tok.tok);
    try std.testing.expectEqualStrings("42", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.float_lit, tok.tok);
    try std.testing.expectEqualStrings("3.14", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.int_lit, tok.tok);
    try std.testing.expectEqualStrings("0xFF", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.int_lit, tok.tok);
    try std.testing.expectEqualStrings("0b1010", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.int_lit, tok.tok);
    try std.testing.expectEqualStrings("0o777", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.int_lit, tok.tok);
    try std.testing.expectEqualStrings("1_000_000", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.float_lit, tok.tok);
    try std.testing.expectEqualStrings("1e10", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.bigint_lit, tok.tok);
    try std.testing.expectEqualStrings("42n", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.bigint_lit, tok.tok);
    try std.testing.expectEqualStrings("0xFFn", tok.text);
}

test "scanner template literal no substitution" {
    const content = "`hello world`";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    const tok = scanner.next();
    try std.testing.expectEqual(Token.template_none, tok.tok);
    try std.testing.expectEqualStrings("`hello world`", tok.text);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner template literal with substitution" {
    const content = "`hello ${name}!`";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.template_head, tok.tok);
    try std.testing.expectEqualStrings("`hello ${", tok.text);

    tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("name", tok.text);

    tok = scanner.next();
    try std.testing.expectEqual(Token.template_tail, tok.tok);
    try std.testing.expectEqualStrings("}!`", tok.text);

    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner template literal multiple substitutions" {
    const content = "`${a} + ${b} = ${c}`";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.template_head, scanner.next().tok);
    try std.testing.expectEqual(Token.ident, scanner.next().tok); // a
    try std.testing.expectEqual(Token.template_middle, scanner.next().tok);
    try std.testing.expectEqual(Token.ident, scanner.next().tok); // b
    try std.testing.expectEqual(Token.template_middle, scanner.next().tok);
    try std.testing.expectEqual(Token.ident, scanner.next().tok); // c
    try std.testing.expectEqual(Token.template_tail, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner template with nested braces" {
    const content = "`${{ x: 1 }}`";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.template_head, scanner.next().tok);
    try std.testing.expectEqual(Token.lbrace, scanner.next().tok);
    try std.testing.expectEqual(Token.ident, scanner.next().tok); // x
    try std.testing.expectEqual(Token.colon, scanner.next().tok);
    try std.testing.expectEqual(Token.int_lit, scanner.next().tok); // 1
    try std.testing.expectEqual(Token.rbrace, scanner.next().tok);
    try std.testing.expectEqual(Token.template_tail, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner private identifier" {
    const content = "#name #count";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.private_ident, tok.tok);
    try std.testing.expectEqualStrings("#name", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.private_ident, tok.tok);
    try std.testing.expectEqualStrings("#count", tok.text);
}

test "scanner keywords" {
    const content = "function class interface type const let var async await";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_function, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_class, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_interface, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_type, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_const, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_let, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_var, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_async, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_await, scanner.next().tok);
}

test "scanner TS type keywords" {
    const content = "number string boolean any unknown never void";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_number, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_string, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_boolean, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_any, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_unknown, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_never, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_void, scanner.next().tok);
}

test "scanner comments" {
    const content = "// line comment\nx /* block */ y";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("x", tok.text);
    tok = scanner.next();
    try std.testing.expectEqualStrings("y", tok.text);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner arrow function" {
    const content = "(x) => x * 2";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.lparen, scanner.next().tok);
    try std.testing.expectEqual(Token.ident, scanner.next().tok);
    try std.testing.expectEqual(Token.rparen, scanner.next().tok);
    try std.testing.expectEqual(Token.arrow, scanner.next().tok);
    try std.testing.expectEqual(Token.ident, scanner.next().tok);
    try std.testing.expectEqual(Token.mul, scanner.next().tok);
    try std.testing.expectEqual(Token.int_lit, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner dollar identifier" {
    const content = "$scope $123 _private";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    var tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("$scope", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("$123", tok.text);
    tok = scanner.next();
    try std.testing.expectEqual(Token.ident, tok.tok);
    try std.testing.expectEqualStrings("_private", tok.text);
}

test "scanner regex rescan" {
    const content = "/pattern/gi";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    // First scan produces division
    const first = scanner.next();
    try std.testing.expectEqual(Token.quo, first.tok);

    // Parser calls rescan — back to the slash
    const regex_tok = scanner.reScanSlashAsRegex(first.span.start);
    try std.testing.expectEqual(Token.regex_lit, regex_tok.tok);
    try std.testing.expectEqualStrings("/pattern/gi", regex_tok.text);
}

test "scanner shebang" {
    const content = "#!/usr/bin/env node\nconst x = 1";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_const, scanner.next().tok);
    try std.testing.expectEqual(Token.ident, scanner.next().tok);
    try std.testing.expectEqual(Token.assign, scanner.next().tok);
    try std.testing.expectEqual(Token.int_lit, scanner.next().tok);
}

test "scanner class declaration" {
    const content = "class User extends Base implements Greetable { #name: string; static count = 0; }";
    var src = Source.init(std.testing.allocator, "test.ts", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.kw_class, scanner.next().tok);
    const tok1 = scanner.next();
    try std.testing.expectEqual(Token.ident, tok1.tok);
    try std.testing.expectEqualStrings("User", tok1.text);
    try std.testing.expectEqual(Token.kw_extends, scanner.next().tok);
    const tok2 = scanner.next();
    try std.testing.expectEqualStrings("Base", tok2.text);
    try std.testing.expectEqual(Token.kw_implements, scanner.next().tok);
    const tok3 = scanner.next();
    try std.testing.expectEqualStrings("Greetable", tok3.text);
    try std.testing.expectEqual(Token.lbrace, scanner.next().tok);
    const tok4 = scanner.next();
    try std.testing.expectEqual(Token.private_ident, tok4.tok);
    try std.testing.expectEqualStrings("#name", tok4.text);
    try std.testing.expectEqual(Token.colon, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_string, scanner.next().tok);
    try std.testing.expectEqual(Token.semicolon, scanner.next().tok);
    try std.testing.expectEqual(Token.kw_static, scanner.next().tok);
    const tok5 = scanner.next();
    try std.testing.expectEqualStrings("count", tok5.text);
    try std.testing.expectEqual(Token.assign, scanner.next().tok);
    try std.testing.expectEqual(Token.int_lit, scanner.next().tok);
    try std.testing.expectEqual(Token.semicolon, scanner.next().tok);
    try std.testing.expectEqual(Token.rbrace, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}

test "scanner compound assignment" {
    const content = "+= -= *= /= %= &= |= ^= <<= >>= >>>= **= ??= &&= ||=";
    var src = Source.init(std.testing.allocator, "test.ts", content);
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
    try std.testing.expectEqual(Token.shl_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.shr_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.unsigned_shr_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.power_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.nullish_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.land_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.lor_assign, scanner.next().tok);
    try std.testing.expectEqual(Token.eof, scanner.next().tok);
}
