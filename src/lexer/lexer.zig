//! Cot Core Lexer
//!
//! Tokenizes modern Cot source code.
//! For DBL syntax (.dbl files), use the cot-dbl frontend package.

const std = @import("std");
const token_mod = @import("token.zig");
const Token = token_mod.Token;
const TokenType = token_mod.TokenType;
const Span = token_mod.Span;

pub const Lexer = struct {
    source: []const u8,
    position: usize,
    line: usize,
    column: usize,

    const Self = @This();

    pub fn init(source: []const u8) Self {
        return .{
            .source = source,
            .position = 0,
            .line = 1,
            .column = 1,
        };
    }

    pub fn tokenize(self: *Self, allocator: std.mem.Allocator) ![]Token {
        var tokens: std.ArrayListAligned(Token, null) = .empty;
        errdefer tokens.deinit(allocator);

        while (self.position < self.source.len) {
            const token = try self.nextToken();
            try tokens.append(allocator, token);
            if (token.type == .eof) break;
        }

        if (tokens.items.len == 0 or tokens.items[tokens.items.len - 1].type != .eof) {
            try tokens.append(allocator, .{
                .type = .eof,
                .lexeme = "",
                .line = self.line,
                .column = self.column,
                .span = Span.init(self.position, self.position),
            });
        }

        return tokens.toOwnedSlice(allocator);
    }

    pub fn nextToken(self: *Self) !Token {
        self.skipWhitespaceAndComments();

        if (self.isAtEnd()) {
            return self.makeToken(.eof, "");
        }

        const start_pos = self.position;
        const start_col = self.column;
        const c = self.advance();

        // Single character tokens
        switch (c) {
            '(' => return self.makeToken(.lparen, self.source[start_pos..self.position]),
            ')' => return self.makeToken(.rparen, self.source[start_pos..self.position]),
            '[' => return self.makeToken(.lbracket, self.source[start_pos..self.position]),
            ']' => return self.makeToken(.rbracket, self.source[start_pos..self.position]),
            '{' => return self.makeToken(.lbrace, self.source[start_pos..self.position]),
            '}' => return self.makeToken(.rbrace, self.source[start_pos..self.position]),
            ',' => return self.makeToken(.comma, self.source[start_pos..self.position]),
            ';' => return self.makeToken(.semicolon, self.source[start_pos..self.position]),
            '#' => return self.makeToken(.hash, self.source[start_pos..self.position]),
            '@' => return self.makeToken(.at, self.source[start_pos..self.position]),
            '%' => return self.makeToken(.percent, self.source[start_pos..self.position]),
            '~' => return self.makeToken(.tilde, self.source[start_pos..self.position]),
            '?' => return self.makeToken(.question, self.source[start_pos..self.position]),
            else => {},
        }

        // Colon variants: : := ::
        if (c == ':') {
            if (self.match('=')) {
                return self.makeToken(.walrus, self.source[start_pos..self.position]);
            } else if (self.match(':')) {
                return self.makeToken(.double_colon, self.source[start_pos..self.position]);
            }
            return self.makeToken(.colon, self.source[start_pos..self.position]);
        }

        // Minus and arrow: - ->
        if (c == '-') {
            if (self.match('>')) {
                return self.makeToken(.arrow, self.source[start_pos..self.position]);
            } else if (self.match('=')) {
                return self.makeToken(.minus_equals, self.source[start_pos..self.position]);
            }
            return self.makeToken(.minus, self.source[start_pos..self.position]);
        }

        // Plus: + ++ +=
        if (c == '+') {
            if (self.match('+')) {
                return self.makeToken(.plus_plus, self.source[start_pos..self.position]);
            }
            if (self.match('=')) {
                return self.makeToken(.plus_equals, self.source[start_pos..self.position]);
            }
            return self.makeToken(.plus, self.source[start_pos..self.position]);
        }

        // Star: * *=
        if (c == '*') {
            if (self.match('=')) {
                return self.makeToken(.star_equals, self.source[start_pos..self.position]);
            }
            return self.makeToken(.star, self.source[start_pos..self.position]);
        }

        // Slash: / /= // /*
        if (c == '/') {
            if (self.match('/')) {
                // Line comment
                while (!self.isAtEnd() and self.peek() != '\n') {
                    _ = self.advance();
                }
                return self.nextToken();
            } else if (self.match('*')) {
                self.skipBlockComment();
                return self.nextToken();
            } else if (self.match('=')) {
                return self.makeToken(.slash_equals, self.source[start_pos..self.position]);
            }
            return self.makeToken(.slash, self.source[start_pos..self.position]);
        }

        // Equals: = == =>
        if (c == '=') {
            if (self.match('=')) {
                return self.makeToken(.eq, self.source[start_pos..self.position]);
            } else if (self.match('>')) {
                return self.makeToken(.fat_arrow, self.source[start_pos..self.position]);
            }
            return self.makeToken(.equals, self.source[start_pos..self.position]);
        }

        // Bang: ! !=
        if (c == '!') {
            if (self.match('=')) {
                return self.makeToken(.ne, self.source[start_pos..self.position]);
            }
            return self.makeToken(.bang, self.source[start_pos..self.position]);
        }

        // Less than: < <=
        if (c == '<') {
            if (self.match('=')) {
                return self.makeToken(.le, self.source[start_pos..self.position]);
            }
            return self.makeToken(.lt, self.source[start_pos..self.position]);
        }

        // Greater than: > >=
        if (c == '>') {
            if (self.match('=')) {
                return self.makeToken(.ge, self.source[start_pos..self.position]);
            }
            return self.makeToken(.gt, self.source[start_pos..self.position]);
        }

        // Ampersand: & &&
        if (c == '&') {
            if (self.match('&')) {
                return self.makeToken(.amp_amp, self.source[start_pos..self.position]);
            }
            return self.makeToken(.ampersand, self.source[start_pos..self.position]);
        }

        // Pipe: | ||
        if (c == '|') {
            if (self.match('|')) {
                return self.makeToken(.pipe_pipe, self.source[start_pos..self.position]);
            }
            return self.makeToken(.pipe, self.source[start_pos..self.position]);
        }

        // Caret: ^
        if (c == '^') {
            return self.makeToken(.caret, self.source[start_pos..self.position]);
        }

        // Dot: . .. ..=
        if (c == '.') {
            if (self.match('.')) {
                if (self.match('=')) {
                    return self.makeToken(.range_inclusive, self.source[start_pos..self.position]);
                }
                return self.makeToken(.range, self.source[start_pos..self.position]);
            }
            return self.makeToken(.period, self.source[start_pos..self.position]);
        }

        // String literals
        if (c == '"' or c == '\'') {
            return self.scanString(c, start_pos, start_col);
        }

        // Numbers
        if (std.ascii.isDigit(c)) {
            return self.scanNumber(start_pos, start_col);
        }

        // Identifiers and keywords
        if (std.ascii.isAlphabetic(c) or c == '_') {
            return self.scanIdentifier(start_pos, start_col);
        }

        return self.makeToken(.invalid, self.source[start_pos..self.position]);
    }

    fn scanString(self: *Self, quote: u8, start_pos: usize, start_col: usize) Token {
        while (!self.isAtEnd() and self.peek() != quote) {
            if (self.peek() == '\n') {
                self.line += 1;
                self.column = 0;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            return .{
                .type = .invalid,
                .lexeme = self.source[start_pos..self.position],
                .line = self.line,
                .column = start_col,
                .span = Span.init(start_pos, self.position),
            };
        }

        _ = self.advance(); // closing quote
        return .{
            .type = .string_literal,
            .lexeme = self.source[start_pos..self.position],
            .line = self.line,
            .column = start_col,
            .span = Span.init(start_pos, self.position),
        };
    }

    fn scanNumber(self: *Self, start_pos: usize, start_col: usize) Token {
        while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        // Decimal point
        if (!self.isAtEnd() and self.peek() == '.' and
            self.position + 1 < self.source.len and
            std.ascii.isDigit(self.source[self.position + 1]))
        {
            _ = self.advance();
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
            return .{
                .type = .decimal_literal,
                .lexeme = self.source[start_pos..self.position],
                .line = self.line,
                .column = start_col,
                .span = Span.init(start_pos, self.position),
            };
        }

        return .{
            .type = .integer_literal,
            .lexeme = self.source[start_pos..self.position],
            .line = self.line,
            .column = start_col,
            .span = Span.init(start_pos, self.position),
        };
    }

    fn scanIdentifier(self: *Self, start_pos: usize, start_col: usize) Token {
        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_')) {
            _ = self.advance();
        }

        const lexeme = self.source[start_pos..self.position];
        var lower_buf: [32]u8 = undefined;
        const lower_len = @min(lexeme.len, 32);
        const lower_lexeme = std.ascii.lowerString(lower_buf[0..lower_len], lexeme[0..lower_len]);

        const token_type = keywords.get(lower_lexeme) orelse .identifier;

        return .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
            .column = start_col,
            .span = Span.init(start_pos, self.position),
        };
    }

    fn skipWhitespaceAndComments(self: *Self) void {
        while (!self.isAtEnd()) {
            switch (self.peek()) {
                ' ', '\t', '\r' => _ = self.advance(),
                '\n' => {
                    _ = self.advance();
                    self.line += 1;
                    self.column = 1;
                },
                else => break,
            }
        }
    }

    fn skipBlockComment(self: *Self) void {
        var depth: usize = 1;
        while (!self.isAtEnd() and depth > 0) {
            if (self.peek() == '/' and self.position + 1 < self.source.len and self.source[self.position + 1] == '*') {
                _ = self.advance();
                _ = self.advance();
                depth += 1;
            } else if (self.peek() == '*' and self.position + 1 < self.source.len and self.source[self.position + 1] == '/') {
                _ = self.advance();
                _ = self.advance();
                depth -= 1;
            } else {
                if (self.peek() == '\n') {
                    self.line += 1;
                    self.column = 0;
                }
                _ = self.advance();
            }
        }
    }

    fn isAtEnd(self: *const Self) bool {
        return self.position >= self.source.len;
    }

    fn peek(self: *const Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.position];
    }

    fn advance(self: *Self) u8 {
        const c = self.source[self.position];
        self.position += 1;
        self.column += 1;
        return c;
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.position] != expected) return false;
        self.position += 1;
        self.column += 1;
        return true;
    }

    fn makeToken(self: *const Self, token_type: TokenType, lexeme: []const u8) Token {
        const end_pos = self.position;
        const start_pos = end_pos - lexeme.len;
        return .{
            .type = token_type,
            .lexeme = lexeme,
            .line = self.line,
            .column = self.column - lexeme.len,
            .span = Span.init(start_pos, end_pos),
        };
    }

    // Modern Cot keywords only
    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        // Declarations
        .{ "fn", .kw_fn },
        .{ "struct", .kw_struct },
        .{ "union", .kw_union },
        .{ "view", .kw_view },
        .{ "enum", .kw_enum },
        .{ "const", .kw_const },
        .{ "var", .kw_var },
        .{ "type", .kw_type },
        .{ "impl", .kw_impl },
        .{ "trait", .kw_trait },
        .{ "pub", .kw_pub },
        .{ "static", .kw_static },

        // Control flow
        .{ "if", .kw_if },
        .{ "else", .kw_else },
        .{ "switch", .kw_switch },
        .{ "for", .kw_for },
        .{ "in", .kw_in },
        .{ "while", .kw_while },
        .{ "loop", .kw_loop },
        .{ "break", .kw_break },
        .{ "continue", .kw_continue },
        .{ "return", .kw_return },

        // Error handling
        .{ "try", .kw_try },
        .{ "catch", .kw_catch },
        .{ "throw", .kw_throw },
        .{ "finally", .kw_finally },

        // Other
        .{ "import", .kw_import },
        .{ "as", .kw_as },
        .{ "self", .kw_self },
        .{ "true", .kw_true },
        .{ "false", .kw_false },
        .{ "nil", .kw_nil },
        .{ "and", .kw_and },
        .{ "or", .kw_or },
        .{ "not", .kw_not },

        // Async
        .{ "async", .kw_async },
        .{ "await", .kw_await },

        // Comptime
        .{ "comptime", .kw_comptime },

        // Testing
        .{ "test", .kw_test },
    });
};

test "lexer basic tokens" {
    var lexer = Lexer.init("fn main() { }");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.kw_fn, tokens[0].type);
    try std.testing.expectEqual(TokenType.identifier, tokens[1].type);
    try std.testing.expectEqual(TokenType.lparen, tokens[2].type);
    try std.testing.expectEqual(TokenType.rparen, tokens[3].type);
    try std.testing.expectEqual(TokenType.lbrace, tokens[4].type);
    try std.testing.expectEqual(TokenType.rbrace, tokens[5].type);
}

test "lexer operators" {
    var lexer = Lexer.init("== != < <= > >= && ||");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.eq, tokens[0].type);
    try std.testing.expectEqual(TokenType.ne, tokens[1].type);
    try std.testing.expectEqual(TokenType.lt, tokens[2].type);
    try std.testing.expectEqual(TokenType.le, tokens[3].type);
    try std.testing.expectEqual(TokenType.gt, tokens[4].type);
    try std.testing.expectEqual(TokenType.ge, tokens[5].type);
    try std.testing.expectEqual(TokenType.amp_amp, tokens[6].type);
    try std.testing.expectEqual(TokenType.pipe_pipe, tokens[7].type);
}

test "lexer range operators" {
    var lexer = Lexer.init("0..10 0..=10");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.integer_literal, tokens[0].type);
    try std.testing.expectEqual(TokenType.range, tokens[1].type);
    try std.testing.expectEqual(TokenType.integer_literal, tokens[2].type);
    try std.testing.expectEqual(TokenType.integer_literal, tokens[3].type);
    try std.testing.expectEqual(TokenType.range_inclusive, tokens[4].type);
}

test "lexer string literal" {
    var lexer = Lexer.init("\"Hello, World\"");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.string_literal, tokens[0].type);
    try std.testing.expectEqualStrings("\"Hello, World\"", tokens[0].lexeme);
}

test "lexer comments" {
    var lexer = Lexer.init("x // comment\ny /* block */ z");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.identifier, tokens[0].type);
    try std.testing.expectEqualStrings("x", tokens[0].lexeme);
    try std.testing.expectEqual(TokenType.identifier, tokens[1].type);
    try std.testing.expectEqualStrings("y", tokens[1].lexeme);
    try std.testing.expectEqual(TokenType.identifier, tokens[2].type);
    try std.testing.expectEqualStrings("z", tokens[2].lexeme);
}

test "lexer arrow and fat arrow" {
    var lexer = Lexer.init("-> =>");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    try std.testing.expectEqual(TokenType.arrow, tokens[0].type);
    try std.testing.expectEqual(TokenType.fat_arrow, tokens[1].type);
}

test "lexer spans" {
    // Test: "fn main() { }"
    //        0123456789...
    var lexer = Lexer.init("fn main() { }");
    const tokens = try lexer.tokenize(std.testing.allocator);
    defer std.testing.allocator.free(tokens);

    // "fn" at positions 0-2
    try std.testing.expectEqual(@as(u32, 0), tokens[0].span.start);
    try std.testing.expectEqual(@as(u32, 2), tokens[0].span.end);

    // "main" at positions 3-7
    try std.testing.expectEqual(@as(u32, 3), tokens[1].span.start);
    try std.testing.expectEqual(@as(u32, 7), tokens[1].span.end);

    // "(" at position 7-8
    try std.testing.expectEqual(@as(u32, 7), tokens[2].span.start);
    try std.testing.expectEqual(@as(u32, 8), tokens[2].span.end);

    // ")" at position 8-9
    try std.testing.expectEqual(@as(u32, 8), tokens[3].span.start);
    try std.testing.expectEqual(@as(u32, 9), tokens[3].span.end);

    // Verify span can extract original text
    const source = "fn main() { }";
    try std.testing.expectEqualStrings("fn", tokens[0].span.slice(source));
    try std.testing.expectEqualStrings("main", tokens[1].span.slice(source));
}
