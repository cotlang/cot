//! Dex Component Parser
//!
//! Parses .dex component files into ComponentDef structures.
//! Component syntax follows Cot conventions - explicit, struct-like.
//!
//! Syntax:
//!   component Counter {
//!       state count: int = 0
//!       prop title: string
//!
//!       fn mount(self: *Self) void {
//!           // initialization
//!       }
//!
//!       fn render(self: *Self) Html {
//!           <div>
//!               <h1>{self.count}</h1>
//!               <button @click="increment">+</button>
//!           </div>
//!       }
//!
//!       fn increment(self: *Self) void {
//!           self.count += 1
//!       }
//!   }

const std = @import("std");
const Allocator = std.mem.Allocator;
const component = @import("component.zig");
const template_lexer = @import("template/lexer.zig");
const template_parser = @import("template/parser.zig");
const renderer = @import("template/renderer.zig");

/// Token types for component file parsing
pub const TokenType = enum {
    component,      // 'component' keyword
    state,          // 'state' keyword
    prop,           // 'prop' keyword
    fn_keyword,     // 'fn' keyword

    identifier,     // names
    colon,          // :
    equals,         // =
    comma,          // ,
    semicolon,      // ;

    lparen,         // (
    rparen,         // )
    lbrace,         // {
    rbrace,         // }
    lbracket,       // [
    rbracket,       // ]

    type_int,       // int
    type_string,    // string
    type_bool,      // bool
    type_float,     // float
    type_array,     // []

    number,         // 123, 3.14
    string,         // "hello"
    true_lit,       // true
    false_lit,      // false

    html_block,     // HTML template block

    star,           // *
    self_keyword,   // Self
    void_keyword,   // void

    eof,
    error_token,
};

/// Token for component parsing
pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    line: u32,
    column: u32,
};

/// State field definition from parsing
pub const ParsedState = struct {
    name: []const u8,
    type_name: []const u8,
    default_value: ?[]const u8,
};

/// Prop definition from parsing
pub const ParsedProp = struct {
    name: []const u8,
    type_name: []const u8,
    required: bool,
};

/// Method definition from parsing
pub const ParsedMethod = struct {
    name: []const u8,
    params: []const []const u8,
    return_type: ?[]const u8,
    body: []const u8,
    is_render: bool,
};

/// Parsed component definition
pub const ParsedComponent = struct {
    name: []const u8,
    state_fields: []const ParsedState,
    props: []const ParsedProp,
    methods: []const ParsedMethod,
    render_template: ?template_parser.Node,
};

/// Component file lexer
pub const ComponentLexer = struct {
    source: []const u8,
    pos: usize,
    line: u32,
    column: u32,
    tokens: std.ArrayListUnmanaged(Token),
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, source: []const u8) Self {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .tokens = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit(self.allocator);
    }

    pub fn tokenize(self: *Self) ![]const Token {
        while (!self.isAtEnd()) {
            self.skipWhitespace();
            if (self.isAtEnd()) break;
            try self.scanToken();
        }

        try self.addToken(.eof, "");
        return self.tokens.items;
    }

    fn scanToken(self: *Self) !void {
        const c = self.peek();

        // Single character tokens
        switch (c) {
            ':' => {
                self.advance();
                try self.addToken(.colon, ":");
                return;
            },
            '=' => {
                self.advance();
                try self.addToken(.equals, "=");
                return;
            },
            ',' => {
                self.advance();
                try self.addToken(.comma, ",");
                return;
            },
            ';' => {
                self.advance();
                try self.addToken(.semicolon, ";");
                return;
            },
            '(' => {
                self.advance();
                try self.addToken(.lparen, "(");
                return;
            },
            ')' => {
                self.advance();
                try self.addToken(.rparen, ")");
                return;
            },
            '{' => {
                self.advance();
                try self.addToken(.lbrace, "{");
                return;
            },
            '}' => {
                self.advance();
                try self.addToken(.rbrace, "}");
                return;
            },
            '[' => {
                self.advance();
                try self.addToken(.lbracket, "[");
                return;
            },
            ']' => {
                self.advance();
                try self.addToken(.rbracket, "]");
                return;
            },
            '*' => {
                self.advance();
                try self.addToken(.star, "*");
                return;
            },
            '<' => {
                // Might be HTML block - scan until closing tag
                try self.scanHtmlBlock();
                return;
            },
            '"' => {
                try self.scanString();
                return;
            },
            else => {},
        }

        // Numbers
        if (std.ascii.isDigit(c)) {
            try self.scanNumber();
            return;
        }

        // Identifiers and keywords
        if (std.ascii.isAlphabetic(c) or c == '_') {
            try self.scanIdentifier();
            return;
        }

        // Unknown character, skip
        self.advance();
    }

    fn scanIdentifier(self: *Self) !void {
        const start = self.pos;

        while (!self.isAtEnd() and (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_')) {
            self.advance();
        }

        const lexeme = self.source[start..self.pos];

        // Check for keywords
        const token_type: TokenType = if (std.mem.eql(u8, lexeme, "component"))
            .component
        else if (std.mem.eql(u8, lexeme, "state"))
            .state
        else if (std.mem.eql(u8, lexeme, "prop"))
            .prop
        else if (std.mem.eql(u8, lexeme, "fn"))
            .fn_keyword
        else if (std.mem.eql(u8, lexeme, "int"))
            .type_int
        else if (std.mem.eql(u8, lexeme, "string"))
            .type_string
        else if (std.mem.eql(u8, lexeme, "bool"))
            .type_bool
        else if (std.mem.eql(u8, lexeme, "float"))
            .type_float
        else if (std.mem.eql(u8, lexeme, "true"))
            .true_lit
        else if (std.mem.eql(u8, lexeme, "false"))
            .false_lit
        else if (std.mem.eql(u8, lexeme, "Self"))
            .self_keyword
        else if (std.mem.eql(u8, lexeme, "void"))
            .void_keyword
        else
            .identifier;

        try self.addToken(token_type, lexeme);
    }

    fn scanNumber(self: *Self) !void {
        const start = self.pos;

        while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
            self.advance();
        }

        // Check for decimal
        if (self.peek() == '.' and self.pos + 1 < self.source.len and
            std.ascii.isDigit(self.source[self.pos + 1]))
        {
            self.advance(); // .
            while (!self.isAtEnd() and std.ascii.isDigit(self.peek())) {
                self.advance();
            }
        }

        try self.addToken(.number, self.source[start..self.pos]);
    }

    fn scanString(self: *Self) !void {
        self.advance(); // Skip opening quote
        const start = self.pos;

        while (!self.isAtEnd() and self.peek() != '"') {
            if (self.peek() == '\\') self.advance(); // Skip escape
            self.advance();
        }

        const value = self.source[start..self.pos];

        if (!self.isAtEnd()) {
            self.advance(); // Skip closing quote
        }

        try self.addToken(.string, value);
    }

    fn scanHtmlBlock(self: *Self) !void {
        const start = self.pos;
        var depth: u32 = 0;

        while (!self.isAtEnd()) {
            if (self.peek() == '<') {
                depth += 1;
                self.advance();

                if (self.peek() == '/') {
                    depth -= 2; // Closing tag cancels opening
                }
            } else if (self.peek() == '>' and self.pos > start) {
                self.advance();
                if (depth <= 0) break;
            } else {
                self.advance();
            }
        }

        try self.addToken(.html_block, self.source[start..self.pos]);
    }

    fn peek(self: *const Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.pos];
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

        // Skip comments
        if (self.pos + 1 < self.source.len and
            self.source[self.pos] == '/' and self.source[self.pos + 1] == '/')
        {
            while (!self.isAtEnd() and self.peek() != '\n') {
                self.advance();
            }
            self.skipWhitespace();
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

/// Component file parser
pub const ComponentParser = struct {
    tokens: []const Token,
    pos: usize,
    allocator: Allocator,

    state_fields: std.ArrayListUnmanaged(ParsedState),
    props: std.ArrayListUnmanaged(ParsedProp),
    methods: std.ArrayListUnmanaged(ParsedMethod),

    const Self = @This();

    pub fn init(allocator: Allocator, tokens: []const Token) Self {
        return .{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
            .state_fields = .empty,
            .props = .empty,
            .methods = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.state_fields.deinit(self.allocator);
        self.props.deinit(self.allocator);
        self.methods.deinit(self.allocator);
    }

    pub fn parse(self: *Self) !ParsedComponent {
        // Expect: component Name { ... }
        _ = try self.expect(.component);
        const name = try self.expect(.identifier);
        _ = try self.expect(.lbrace);

        while (!self.isAtEnd() and self.peek().token_type != .rbrace) {
            const token = self.peek();

            switch (token.token_type) {
                .state => try self.parseState(),
                .prop => try self.parseProp(),
                .fn_keyword => try self.parseMethod(),
                else => self.advance(),
            }
        }

        _ = try self.expect(.rbrace);

        // Find render method and parse its template
        var render_template: ?template_parser.Node = null;
        for (self.methods.items) |method| {
            if (method.is_render and method.body.len > 0) {
                // Parse the HTML body as a template
                var lexer = template_lexer.Lexer.init(self.allocator, method.body);
                defer lexer.deinit();
                const tmpl_tokens = try lexer.tokenize();

                var parser = template_parser.Parser.init(self.allocator, tmpl_tokens);
                defer parser.deinit();
                render_template = try parser.parse();
                break;
            }
        }

        return ParsedComponent{
            .name = name.lexeme,
            .state_fields = try self.state_fields.toOwnedSlice(self.allocator),
            .props = try self.props.toOwnedSlice(self.allocator),
            .methods = try self.methods.toOwnedSlice(self.allocator),
            .render_template = render_template,
        };
    }

    fn parseState(self: *Self) !void {
        self.advance(); // state
        const name = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const type_name = try self.expectType();

        var default_value: ?[]const u8 = null;
        if (self.peek().token_type == .equals) {
            self.advance(); // =
            default_value = self.peek().lexeme;
            self.advance();
        }

        try self.state_fields.append(self.allocator, .{
            .name = name.lexeme,
            .type_name = type_name,
            .default_value = default_value,
        });
    }

    fn parseProp(self: *Self) !void {
        self.advance(); // prop
        const name = try self.expect(.identifier);
        _ = try self.expect(.colon);
        const type_name = try self.expectType();

        try self.props.append(self.allocator, .{
            .name = name.lexeme,
            .type_name = type_name,
            .required = true,
        });
    }

    fn parseMethod(self: *Self) !void {
        self.advance(); // fn
        const name = try self.expect(.identifier);

        _ = try self.expect(.lparen);
        // Skip parameters for now
        while (self.peek().token_type != .rparen and !self.isAtEnd()) {
            self.advance();
        }
        _ = try self.expect(.rparen);

        // Return type (optional)
        var return_type: ?[]const u8 = null;
        if (self.peek().token_type == .identifier or
            self.peek().token_type == .void_keyword or
            self.peek().token_type == .type_int)
        {
            return_type = self.peek().lexeme;
            self.advance();
        }

        // Method body
        _ = try self.expect(.lbrace);
        const body_start = self.pos;

        var depth: u32 = 1;
        while (depth > 0 and !self.isAtEnd()) {
            if (self.peek().token_type == .lbrace) depth += 1;
            if (self.peek().token_type == .rbrace) depth -= 1;
            if (depth > 0) self.advance();
        }

        const body_end = self.pos;

        // Build body string from token lexemes
        var body: []const u8 = "";
        if (body_start < self.tokens.len and body_end > body_start) {
            // Concatenate token lexemes to reconstruct body
            var body_buf: std.ArrayListUnmanaged(u8) = .empty;
            errdefer body_buf.deinit(self.allocator);

            for (self.tokens[body_start..body_end]) |token| {
                // Add space between tokens (except for special chars)
                if (body_buf.items.len > 0) {
                    const last_char = body_buf.items[body_buf.items.len - 1];
                    const needs_space = last_char != '.' and
                        last_char != '(' and
                        last_char != ')' and
                        token.lexeme.len > 0 and
                        token.lexeme[0] != '.' and
                        token.lexeme[0] != '(' and
                        token.lexeme[0] != ')' and
                        token.lexeme[0] != ',' and
                        token.lexeme[0] != ';';
                    if (needs_space) {
                        try body_buf.append(self.allocator, ' ');
                    }
                }
                try body_buf.appendSlice(self.allocator, token.lexeme);
            }

            body = try body_buf.toOwnedSlice(self.allocator);
        }

        _ = try self.expect(.rbrace);

        const is_render = std.mem.eql(u8, name.lexeme, "render");

        try self.methods.append(self.allocator, .{
            .name = name.lexeme,
            .params = &.{},
            .return_type = return_type,
            .body = body,
            .is_render = is_render,
        });
    }

    fn expectType(self: *Self) ![]const u8 {
        const token = self.peek();
        if (token.token_type == .type_int or
            token.token_type == .type_string or
            token.token_type == .type_bool or
            token.token_type == .type_float or
            token.token_type == .identifier)
        {
            self.advance();
            return token.lexeme;
        }
        return error.ExpectedType;
    }

    fn expect(self: *Self, expected: TokenType) !Token {
        const token = self.peek();
        if (token.token_type != expected) {
            return error.UnexpectedToken;
        }
        self.advance();
        return token;
    }

    fn peek(self: *const Self) Token {
        if (self.pos >= self.tokens.len) {
            return Token{ .token_type = .eof, .lexeme = "", .line = 0, .column = 0 };
        }
        return self.tokens[self.pos];
    }

    fn advance(self: *Self) void {
        if (self.pos < self.tokens.len) {
            self.pos += 1;
        }
    }

    fn isAtEnd(self: *const Self) bool {
        return self.pos >= self.tokens.len or self.peek().token_type == .eof;
    }
};

/// Helper to convert ParsedComponent to runtime ComponentDef
pub fn toComponentDef(allocator: Allocator, parsed: ParsedComponent) !component.ComponentDef {
    var state_fields: std.ArrayListUnmanaged(component.StateField) = .empty;
    errdefer state_fields.deinit(allocator);

    for (parsed.state_fields) |field| {
        const default_value: renderer.Value = if (field.default_value) |dv| blk: {
            if (std.fmt.parseInt(i64, dv, 10)) |n| {
                break :blk .{ .int_val = n };
            } else |_| {}
            if (std.mem.eql(u8, dv, "true")) break :blk .{ .bool_val = true };
            if (std.mem.eql(u8, dv, "false")) break :blk .{ .bool_val = false };
            break :blk .{ .string_val = dv };
        } else .{ .null_val = {} };

        try state_fields.append(allocator, .{
            .name = field.name,
            .default_value = default_value,
        });
    }

    var props: std.ArrayListUnmanaged(component.PropDef) = .empty;
    errdefer props.deinit(allocator);

    for (parsed.props) |prop| {
        try props.append(allocator, .{
            .name = prop.name,
            .required = prop.required,
        });
    }

    var event_handlers: std.StringHashMapUnmanaged([]const u8) = .empty;
    errdefer event_handlers.deinit(allocator);

    for (parsed.methods) |method| {
        if (!method.is_render) {
            try event_handlers.put(allocator, method.name, method.name);
        }
    }

    return component.ComponentDef{
        .name = parsed.name,
        .state_fields = try state_fields.toOwnedSlice(allocator),
        .props = try props.toOwnedSlice(allocator),
        .template = parsed.render_template orelse template_parser.Node{
            .node_type = .fragment,
            .data = .{ .fragment = {} },
            .children = &.{},
            .line = 0,
            .column = 0,
        },
        .event_handlers = event_handlers,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "parse simple component" {
    const allocator = std.testing.allocator;

    const source =
        \\component Counter {
        \\    state count: int = 0
        \\}
    ;

    var lexer = ComponentLexer.init(allocator, source);
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parser = ComponentParser.init(allocator, tokens);
    defer parser.deinit();

    const parsed = try parser.parse();

    try std.testing.expectEqualStrings("Counter", parsed.name);
    try std.testing.expectEqual(@as(usize, 1), parsed.state_fields.len);
    try std.testing.expectEqualStrings("count", parsed.state_fields[0].name);
}
