//! Token types for Cot Core Lexer
//!
//! This defines the token types for modern Cot syntax.
//! For DBL syntax, use the cot-dbl frontend package.

const std = @import("std");

/// A span of bytes in the source code
/// Used for precise error highlighting and IDE integration
pub const Span = struct {
    /// Byte offset where this span starts (inclusive)
    start: u32,
    /// Byte offset where this span ends (exclusive)
    end: u32,

    pub const zero: Span = .{ .start = 0, .end = 0 };

    /// Create a span from start to end
    pub fn init(start: usize, end: usize) Span {
        return .{
            .start = @intCast(@min(start, std.math.maxInt(u32))),
            .end = @intCast(@min(end, std.math.maxInt(u32))),
        };
    }

    /// Get the length of this span in bytes
    pub fn len(self: Span) u32 {
        return self.end - self.start;
    }

    /// Merge two spans into one that covers both
    pub fn merge(a: Span, b: Span) Span {
        return .{
            .start = @min(a.start, b.start),
            .end = @max(a.end, b.end),
        };
    }

    /// Extract the text this span covers from source
    pub fn slice(self: Span, source: []const u8) []const u8 {
        if (self.start >= source.len) return "";
        const end = @min(self.end, source.len);
        return source[self.start..end];
    }

    /// Check if this span contains a byte offset
    pub fn contains(self: Span, offset: u32) bool {
        return offset >= self.start and offset < self.end;
    }

    /// Format for debugging
    pub fn format(
        self: Span,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("[{d}..{d}]", .{ self.start, self.end });
    }
};

/// A token from the source code
pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: usize,
    column: usize,
    /// Byte span in source (for precise error highlighting)
    span: Span,

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}('{s}' @ {d}:{d} [{d}..{d}])", .{
            @tagName(self.type),
            self.lexeme,
            self.line,
            self.column,
            self.span.start,
            self.span.end,
        });
    }
};

/// Token types for modern Cot syntax
pub const TokenType = enum {
    // Literals
    identifier,
    string_literal,
    integer_literal,
    decimal_literal,

    // Single-character tokens
    lparen, // (
    rparen, // )
    lbracket, // [
    rbracket, // ]
    lbrace, // {
    rbrace, // }
    comma, // ,
    period, // .
    colon, // :
    semicolon, // ;
    plus, // +
    minus, // -
    star, // *
    slash, // /
    hash, // # (string concatenation)
    equals, // =
    at, // @
    percent, // %
    ampersand, // &
    pipe, // |
    caret, // ^
    tilde, // ~
    bang, // !
    question, // ?

    // Multi-character tokens
    arrow, // ->
    fat_arrow, // =>
    double_colon, // ::
    range, // ..
    range_inclusive, // ..=
    walrus, // :=
    plus_plus, // ++ (string concatenation, Zig-style)
    plus_equals, // +=
    minus_equals, // -=
    star_equals, // *=
    slash_equals, // /=

    // Comparison operators
    eq, // ==
    ne, // !=
    lt, // <
    le, // <=
    gt, // >
    ge, // >=

    // Logical operators
    amp_amp, // &&
    pipe_pipe, // ||

    // Keywords - declarations
    kw_fn,
    kw_struct,
    kw_union,
    kw_view,
    kw_enum,
    kw_const,
    kw_var,
    kw_type,
    kw_impl,
    kw_trait,
    kw_pub,
    kw_static,

    // Keywords - control flow
    kw_if,
    kw_else,
    kw_switch,
    kw_for,
    kw_in,
    kw_while,
    kw_loop,
    kw_break,
    kw_continue,
    kw_return,

    // Keywords - error handling
    kw_try,
    kw_catch,
    kw_throw,
    kw_finally,

    // Keywords - other
    kw_import,
    kw_as,
    kw_self,
    kw_true,
    kw_false,
    kw_nil,
    kw_and,
    kw_or,
    kw_not,

    // Keywords - async
    kw_async,
    kw_await,

    // Keywords - comptime
    kw_comptime,

    // Keywords - testing
    kw_test,

    // Special
    eof,
    invalid,
    newline,

    /// Check if this is a keyword token
    pub fn isKeyword(self: TokenType) bool {
        return switch (self) {
            .kw_fn,
            .kw_struct,
            .kw_union,
            .kw_view,
            .kw_enum,
            .kw_const,
            .kw_var,
            .kw_type,
            .kw_impl,
            .kw_trait,
            .kw_pub,
            .kw_static,
            .kw_if,
            .kw_else,
            .kw_switch,
            .kw_for,
            .kw_in,
            .kw_while,
            .kw_loop,
            .kw_break,
            .kw_continue,
            .kw_return,
            .kw_try,
            .kw_catch,
            .kw_throw,
            .kw_finally,
            .kw_import,
            .kw_as,
            .kw_self,
            .kw_true,
            .kw_false,
            .kw_nil,
            .kw_and,
            .kw_or,
            .kw_not,
            .kw_async,
            .kw_await,
            .kw_comptime,
            .kw_test,
            => true,
            else => false,
        };
    }

    /// Check if this is an operator token
    pub fn isOperator(self: TokenType) bool {
        return switch (self) {
            .plus,
            .plus_plus,
            .minus,
            .star,
            .slash,
            .equals,
            .eq,
            .ne,
            .lt,
            .le,
            .gt,
            .ge,
            .amp_amp,
            .pipe_pipe,
            .bang,
            => true,
            else => false,
        };
    }

    /// Check if this is a literal token
    pub fn isLiteral(self: TokenType) bool {
        return switch (self) {
            .string_literal,
            .integer_literal,
            .decimal_literal,
            => true,
            else => false,
        };
    }
};

test "token format" {
    const token = Token{
        .type = .kw_fn,
        .lexeme = "fn",
        .line = 1,
        .column = 1,
        .span = Span.init(0, 2),
    };
    var buf: [100]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try token.format("", .{}, fbs.writer());
    try std.testing.expectEqualStrings("kw_fn('fn' @ 1:1 [0..2])", fbs.getWritten());
}

test "span operations" {
    const span1 = Span.init(10, 20);
    const span2 = Span.init(15, 25);

    try std.testing.expectEqual(@as(u32, 10), span1.len());
    try std.testing.expect(span1.contains(15));
    try std.testing.expect(!span1.contains(25));

    const merged = Span.merge(span1, span2);
    try std.testing.expectEqual(@as(u32, 10), merged.start);
    try std.testing.expectEqual(@as(u32, 25), merged.end);

    const source = "Hello, World!";
    const slice_span = Span.init(0, 5);
    try std.testing.expectEqualStrings("Hello", slice_span.slice(source));
}
