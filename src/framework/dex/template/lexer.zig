//! Dex Template Lexer
//!
//! Tokenizes HTML templates with embedded Cot expressions.
//! Handles:
//! - HTML text content
//! - HTML tags and attributes
//! - Cot expressions: {expr}
//! - Directives: @if, @for, @raw
//! - Event bindings: @click="handler"

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Token types for template lexing
pub const TokenType = enum {
    // HTML structure
    text,              // Raw text content
    tag_open,          // <
    tag_close,         // >
    tag_self_close,    // />
    tag_end_open,      // </
    tag_name,          // div, span, button, etc.

    // Attributes
    attr_name,         // class, id, value, etc.
    attr_equals,       // =
    attr_value,        // "string value"

    // Cot expressions
    expr_open,         // {
    expr_close,        // }
    expr_content,      // The expression inside { }

    // Directives
    directive_if,      // @if
    directive_else,    // @else
    directive_for,     // @for
    directive_in,      // in (within @for)
    directive_raw,     // @raw
    directive_comment, // @//

    // Event bindings (special attributes)
    event_name,        // @click, @input, @submit, etc.

    // Control
    block_open,        // {
    block_close,       // }

    // Components
    component_name,    // PascalCase component names

    // Special
    eof,
    error_token,
};

/// A single token from the template
pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    line: u32,
    column: u32,
};

/// Template Lexer
pub const Lexer = struct {
    source: []const u8,
    pos: usize,
    line: u32,
    column: u32,
    tokens: std.ArrayListUnmanaged(Token),
    allocator: Allocator,

    // State machine
    in_tag: bool,
    in_expr: bool,

    const Self = @This();

    pub fn init(allocator: Allocator, source: []const u8) Self {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .tokens = .empty,
            .allocator = allocator,
            .in_tag = false,
            .in_expr = false,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit(self.allocator);
    }

    /// Tokenize the entire template
    pub fn tokenize(self: *Self) ![]const Token {
        while (!self.isAtEnd()) {
            try self.scanToken();
        }

        try self.addToken(.eof, "");
        return self.tokens.items;
    }

    fn scanToken(self: *Self) !void {
        if (self.in_tag) {
            try self.scanInsideTag();
        } else {
            try self.scanOutsideTag();
        }
    }

    /// Scan tokens when outside of an HTML tag
    fn scanOutsideTag(self: *Self) !void {
        const c = self.peek();

        // Check for Cot expression
        if (c == '{') {
            try self.scanExpression();
            return;
        }

        // Check for directive
        if (c == '@') {
            try self.scanDirective();
            return;
        }

        // Check for tag start
        if (c == '<') {
            self.in_tag = true;
            self.advance();

            if (self.peek() == '/') {
                self.advance();
                try self.addToken(.tag_end_open, "</");
            } else {
                try self.addToken(.tag_open, "<");
            }

            // Scan tag name
            try self.scanTagName();
            return;
        }

        // Otherwise, scan text content
        try self.scanText();
    }

    /// Scan tokens when inside an HTML tag
    fn scanInsideTag(self: *Self) !void {
        self.skipWhitespace();

        if (self.isAtEnd()) return;

        const c = self.peek();

        // Check for tag end
        if (c == '>') {
            self.advance();
            try self.addToken(.tag_close, ">");
            self.in_tag = false;
            return;
        }

        // Check for self-closing tag
        if (c == '/' and self.peekNext() == '>') {
            self.advance();
            self.advance();
            try self.addToken(.tag_self_close, "/>");
            self.in_tag = false;
            return;
        }

        // Check for event binding (@click, @input, etc.)
        if (c == '@') {
            try self.scanEventBinding();
            return;
        }

        // Check for expression in attribute position
        if (c == '{') {
            try self.scanExpression();
            return;
        }

        // Otherwise, scan attribute
        try self.scanAttribute();
    }

    /// Scan tag name (handles both HTML tags and components)
    fn scanTagName(self: *Self) !void {
        self.skipWhitespace();

        const start = self.pos;

        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '-' or self.peek() == '_')) {
            self.advance();
        }

        const name = self.source[start..self.pos];

        if (name.len == 0) return;

        // Check if it's a component (PascalCase)
        if (name.len > 0 and std.ascii.isUpper(name[0])) {
            try self.addToken(.component_name, name);
        } else {
            try self.addToken(.tag_name, name);
        }
    }

    /// Scan an HTML attribute
    fn scanAttribute(self: *Self) !void {
        const start = self.pos;

        // Scan attribute name
        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '-' or self.peek() == '_' or self.peek() == ':')) {
            self.advance();
        }

        const name = self.source[start..self.pos];
        if (name.len == 0) return;

        try self.addToken(.attr_name, name);

        self.skipWhitespace();

        // Check for equals
        if (self.peek() == '=') {
            self.advance();
            try self.addToken(.attr_equals, "=");

            self.skipWhitespace();

            // Scan attribute value
            try self.scanAttributeValue();
        }
    }

    /// Scan attribute value (handles strings and expressions)
    fn scanAttributeValue(self: *Self) !void {
        const c = self.peek();

        // Expression value: attr={expr}
        if (c == '{') {
            try self.scanExpression();
            return;
        }

        // Quoted string value
        if (c == '"' or c == '\'') {
            const quote = c;
            self.advance(); // Skip opening quote

            const start = self.pos;
            while (!self.isAtEnd() and self.peek() != quote) {
                self.advance();
            }

            const value = self.source[start..self.pos];
            try self.addToken(.attr_value, value);

            if (!self.isAtEnd()) {
                self.advance(); // Skip closing quote
            }
        }
    }

    /// Scan event binding (@click, @input, etc.)
    fn scanEventBinding(self: *Self) !void {
        self.advance(); // Skip @

        const start = self.pos;
        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '-' or self.peek() == '_')) {
            self.advance();
        }

        const name = self.source[start..self.pos];
        try self.addToken(.event_name, name);

        self.skipWhitespace();

        // Check for equals and value
        if (self.peek() == '=') {
            self.advance();
            try self.addToken(.attr_equals, "=");
            self.skipWhitespace();
            try self.scanAttributeValue();
        }
    }

    /// Scan a Cot expression {expr}
    fn scanExpression(self: *Self) !void {
        self.advance(); // Skip {
        try self.addToken(.expr_open, "{");

        const start = self.pos;
        var brace_depth: u32 = 1;

        while (!self.isAtEnd() and brace_depth > 0) {
            const c = self.peek();
            if (c == '{') {
                brace_depth += 1;
            } else if (c == '}') {
                brace_depth -= 1;
                if (brace_depth == 0) break;
            }
            self.advance();
        }

        const expr = self.source[start..self.pos];
        if (expr.len > 0) {
            try self.addToken(.expr_content, expr);
        }

        if (!self.isAtEnd() and self.peek() == '}') {
            self.advance();
            try self.addToken(.expr_close, "}");
        }
    }

    /// Scan a directive (@if, @for, @else, @raw)
    fn scanDirective(self: *Self) !void {
        self.advance(); // Skip @

        const start = self.pos;

        // Check for comment
        if (self.peek() == '/') {
            self.advance();
            if (self.peek() == '/') {
                self.advance();
                // Scan to end of line
                while (!self.isAtEnd() and self.peek() != '\n') {
                    self.advance();
                }
                try self.addToken(.directive_comment, self.source[start..self.pos]);
                return;
            }
        }

        // Scan directive name
        while (!self.isAtEnd() and std.ascii.isAlphanumeric(self.peek())) {
            self.advance();
        }

        const name = self.source[start..self.pos];

        if (std.mem.eql(u8, name, "if")) {
            try self.addToken(.directive_if, "@if");
        } else if (std.mem.eql(u8, name, "else")) {
            try self.addToken(.directive_else, "@else");
        } else if (std.mem.eql(u8, name, "for")) {
            try self.addToken(.directive_for, "@for");
        } else if (std.mem.eql(u8, name, "raw")) {
            try self.addToken(.directive_raw, "@raw");
        } else {
            // Unknown directive, treat as text
            try self.addToken(.text, self.source[start - 1 .. self.pos]);
        }
    }

    /// Scan plain text content
    fn scanText(self: *Self) !void {
        const start = self.pos;

        while (!self.isAtEnd()) {
            const c = self.peek();
            // Stop at special characters
            if (c == '<' or c == '{' or c == '@') break;
            self.advance();
        }

        const text = self.source[start..self.pos];
        if (text.len > 0) {
            try self.addToken(.text, text);
        }
    }

    // Helper methods

    fn peek(self: *const Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
    }

    fn peekNext(self: *const Self) u8 {
        if (self.pos + 1 >= self.source.len) return 0;
        return self.source[self.pos + 1];
    }

    fn advance(self: *Self) void {
        if (!self.isAtEnd()) {
            if (self.source[self.pos] == '\n') {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
            self.pos += 1;
        }
    }

    fn isAtEnd(self: *const Self) bool {
        return self.pos >= self.source.len;
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isAtEnd() and std.ascii.isWhitespace(self.peek())) {
            self.advance();
        }
    }

    fn addToken(self: *Self, token_type: TokenType, lexeme: []const u8) !void {
        try self.tokens.append(self.allocator, .{
            .token_type = token_type,
            .lexeme = lexeme,
            .line = self.line,
            .column = self.column,
        });
    }
};

// ============================================================================
// Tests
// ============================================================================

test "lexer basic html" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(allocator, "<div>Hello</div>");
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    try std.testing.expectEqual(TokenType.tag_open, tokens[0].token_type);
    try std.testing.expectEqual(TokenType.tag_name, tokens[1].token_type);
    try std.testing.expectEqualStrings("div", tokens[1].lexeme);
    try std.testing.expectEqual(TokenType.tag_close, tokens[2].token_type);
    try std.testing.expectEqual(TokenType.text, tokens[3].token_type);
    try std.testing.expectEqualStrings("Hello", tokens[3].lexeme);
}

test "lexer expression" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(allocator, "<h1>{user.name}</h1>");
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    // Find the expression tokens
    var found_expr = false;
    for (tokens) |token| {
        if (token.token_type == .expr_content) {
            try std.testing.expectEqualStrings("user.name", token.lexeme);
            found_expr = true;
        }
    }
    try std.testing.expect(found_expr);
}

test "lexer event binding" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(allocator,
        \\<button @click="increment">+</button>
    );
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    var found_event = false;
    for (tokens) |token| {
        if (token.token_type == .event_name) {
            try std.testing.expectEqualStrings("click", token.lexeme);
            found_event = true;
        }
    }
    try std.testing.expect(found_event);
}

test "lexer component" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(allocator, "<UserCard user={current_user} />");
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    try std.testing.expectEqual(TokenType.tag_open, tokens[0].token_type);
    try std.testing.expectEqual(TokenType.component_name, tokens[1].token_type);
    try std.testing.expectEqualStrings("UserCard", tokens[1].lexeme);
}

test "lexer directive if" {
    const allocator = std.testing.allocator;
    var lexer = Lexer.init(allocator, "@if condition { <div>Yes</div> }");
    defer lexer.deinit();

    const tokens = try lexer.tokenize();

    try std.testing.expectEqual(TokenType.directive_if, tokens[0].token_type);
}
