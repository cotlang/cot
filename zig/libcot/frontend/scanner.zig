//! Lexical scanner for cot.

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

pub const TokenInfo = struct {
    tok: Token,
    span: Span,
    text: []const u8,
};

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

    fn errorAt(self: *Scanner, pos: Pos, err_code: ErrorCode, msg: []const u8) void {
        if (self.err) |reporter| reporter.errorWithCode(pos, err_code, msg);
    }

    pub fn next(self: *Scanner) TokenInfo {
        self.skipWhitespaceAndComments();
        const start = self.pos;

        if (self.ch == null) return .{ .tok = .eof, .span = Span.fromPos(start), .text = "" };

        // Doc comment: /// ... (Zig pattern — returns token, not skipped)
        if (self.ch == '/' and self.peek(1) == '/' and self.peek(2) == '/') {
            return self.scanDocComment(start);
        }

        const c = self.ch.?;
        if (isAlpha(c) or c == '_') return self.scanIdentifier(start);
        if (isDigit(c)) return self.scanNumber(start);
        if (c == '"') return self.scanString(start);
        if (c == '\'') return self.scanChar(start);
        return self.scanOperator(start);
    }

    /// Scan a doc comment line (/// ...). Returns the text after "/// " (trimmed).
    /// Multiple consecutive /// lines are returned as separate tokens — the parser
    /// concatenates them (Zig pattern: each line is a separate doc_comment token).
    fn scanDocComment(self: *Scanner, start: Pos) TokenInfo {
        // Skip the "///" prefix
        self.advance(); // /
        self.advance(); // /
        self.advance(); // /
        // Skip optional single space after ///
        if (self.ch == ' ') self.advance();
        const text_start = self.pos.offset;
        // Consume until end of line
        while (self.ch) |cc| {
            if (cc == '\n') {
                break;
            }
            self.advance();
        }
        const text = self.src.content[text_start..self.pos.offset];
        // Consume the newline so next call starts fresh
        if (self.ch == '\n') self.advance();
        return .{ .tok = .doc_comment, .span = Span.init(start, self.pos), .text = text };
    }

    fn skipWhitespaceAndComments(self: *Scanner) void {
        while (self.ch) |c| {
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                self.advance();
            } else if (c == '/' and self.peek(1) == '/' and self.peek(2) != '/') {
                // Regular comment (not doc comment) — skip
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
                // Doc comment — stop skipping so next() can return doc_comment token
                break;
            } else if (c == '/' and self.peek(1) == '*') {
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

    fn scanIdentifier(self: *Scanner, start: Pos) TokenInfo {
        while (self.ch) |c| {
            if (isAlphaNumeric(c) or c == '_') self.advance() else break;
        }
        const text = self.src.content[start.offset..self.pos.offset];
        const kw = token.lookup(text);
        if (kw != .ident) return .{ .tok = kw, .span = Span.init(start, self.pos), .text = "" };
        return .{ .tok = .ident, .span = Span.init(start, self.pos), .text = text };
    }

    fn scanNumber(self: *Scanner, start: Pos) TokenInfo {
        var is_float = false;

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

        while (self.ch) |c| if (isDigit(c) or c == '_') self.advance() else break;

        if (self.ch == '.' and self.peek(1) != '.') {
            is_float = true;
            self.advance();
            while (self.ch) |c| if (isDigit(c) or c == '_') self.advance() else break;
        }

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
                self.advance();
                if (self.ch != null) self.advance();
            } else if (c == '$' and self.peek(1) == '{') {
                self.advance();
                self.advance();
                found_interp = true;
                self.in_interp_string = true;
                self.interp_brace_depth = 1;
                break;
            } else if (c == '\n') {
                break;
            } else {
                self.advance();
            }
        }

        if (!terminated and !found_interp) self.errorAt(start, .e100, "string literal not terminated");
        const text = self.src.content[start.offset..self.pos.offset];
        if (found_interp) return .{ .tok = .string_interp_start, .span = Span.init(start, self.pos), .text = text };
        return .{ .tok = .string_lit, .span = Span.init(start, self.pos), .text = text };
    }

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

        if (!terminated and !found_interp) self.errorAt(start, .e100, "string literal not terminated");
        const text = self.src.content[start.offset..self.pos.offset];
        if (found_interp) return .{ .tok = .string_interp_mid, .span = Span.init(start, self.pos), .text = text };
        return .{ .tok = .string_interp_end, .span = Span.init(start, self.pos), .text = text };
    }

    fn scanChar(self: *Scanner, start: Pos) TokenInfo {
        self.advance(); // opening '
        if (self.ch == '\\') {
            self.advance();
            if (self.ch != null) self.advance();
        } else if (self.ch != null and self.ch != '\'') {
            self.advance();
        }
        var terminated = false;
        if (self.ch == '\'') {
            self.advance();
            terminated = true;
        }
        if (!terminated) self.errorAt(start, .e101, "character literal not terminated");
        return .{ .tok = .char_lit, .span = Span.init(start, self.pos), .text = self.src.content[start.offset..self.pos.offset] };
    }

    fn scanOperator(self: *Scanner, start: Pos) TokenInfo {
        const c = self.ch.?;
        self.advance();

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
            '(' => .lparen, ')' => .rparen, '[' => .lbrack, ']' => .rbrack,
            '{' => .lbrace, '}' => .rbrace, ',' => .comma, ';' => .semicolon,
            ':' => .colon, '~' => .not,
            '@' => if (self.ch == '"') {
                // @"..." quoted identifier (Zig pattern: use keywords as identifiers)
                self.advance(); // consume opening "
                const text_start = self.pos.offset;
                var terminated = false;
                while (self.ch) |cc| {
                    if (cc == '"') { terminated = true; break; }
                    if (cc == '\n') break; // Zig rejects newlines in quoted identifiers
                    self.advance();
                }
                const ident_text = self.src.content[text_start..self.pos.offset];
                if (terminated) {
                    self.advance(); // consume closing "
                } else {
                    self.errorAt(start, .e100, "quoted identifier not terminated");
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

        if (tok == .illegal) self.errorAt(start, .e104, "unexpected character");
        return .{ .tok = tok, .span = Span.init(start, self.pos), .text = "" };
    }

    fn advance(self: *Scanner) void {
        self.pos = self.pos.advance(1);
        self.ch = self.src.at(self.pos);
    }

    fn peek(self: *Scanner, n: u32) ?u8 {
        return self.src.at(self.pos.advance(n));
    }
};

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

// ============================================================================
// Tests
// ============================================================================

test "scanner basics" {
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

test "scanner operators" {
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

test "scanner strings" {
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

test "scanner numbers" {
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

test "scanner comments" {
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

test "scanner keywords" {
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

test "scanner type keywords" {
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

test "scanner character literals" {
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

test "scanner concat operator" {
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

test "scanner compound assignment" {
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

test "scanner arrows" {
    const content = "-> =>";
    var src = Source.init(std.testing.allocator, "test.cot", content);
    defer src.deinit();
    var scanner = Scanner.init(&src);

    try std.testing.expectEqual(Token.arrow, scanner.next().tok);
    try std.testing.expectEqual(Token.fat_arrow, scanner.next().tok);
}
