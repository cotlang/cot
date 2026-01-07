//! Dex Component Parser
//!
//! Parses .dx component/page files into ComponentDef structures.
//!
//! Supports two syntaxes:
//!
//! 1. Component syntax (explicit, struct-like):
//!   component Counter {
//!       state count: int = 0
//!       prop title: string
//!
//!       fn render(self: *Self) Html {
//!           <div>{self.count}</div>
//!       }
//!   }
//!
//! 2. Page syntax (YAML frontmatter + template):
//!   ---
//!   name: HomePage
//!   layout: default
//!   state:
//!     count: int = 0
//!   ---
//!   <template>
//!     <div>{{ count }}</div>
//!   </template>
//!   <script>
//!   fn increment(self) { self.count += 1 }
//!   </script>

const std = @import("std");
const Allocator = std.mem.Allocator;
const component = @import("component.zig");
const template_lexer = @import("template/lexer.zig");
const template_parser = @import("template/parser.zig");
const renderer = @import("template/renderer.zig");

// ============================================================================
// Error Handling
// ============================================================================

/// Parse error with full context for debugging
pub const ParseError = struct {
    kind: ErrorKind,
    message: []const u8,
    line: u32,
    column: u32,
    source_line: ?[]const u8,
    file_path: ?[]const u8,

    pub const ErrorKind = enum {
        unexpected_token,
        expected_type,
        invalid_frontmatter,
        unclosed_template,
        unclosed_script,
        invalid_state_definition,
        missing_component_name,
        unexpected_eof,
        invalid_syntax,
    };

    /// Format a user-friendly error message with source context
    pub fn format(self: *const ParseError, allocator: Allocator) ![]const u8 {
        var buf: std.ArrayListUnmanaged(u8) = .empty;
        errdefer buf.deinit(allocator);

        const writer = buf.writer(allocator);

        // File location
        if (self.file_path) |path| {
            try writer.print("{s}:", .{path});
        }
        try writer.print("{d}:{d}: ", .{ self.line, self.column });

        // Error kind and message
        try writer.print("error: {s}\n", .{self.message});

        // Source line with caret pointing to error
        if (self.source_line) |line| {
            try writer.print("  |\n", .{});
            try writer.print("{d:>3} | {s}\n", .{ self.line, line });
            try writer.print("  | ", .{});
            // Print spaces then caret
            var i: u32 = 1;
            while (i < self.column) : (i += 1) {
                try writer.writeByte(' ');
            }
            try writer.print("^\n", .{});
        }

        return buf.toOwnedSlice(allocator);
    }
};

/// Global error context for the current parse operation
/// Used to pass detailed error info up the call stack
pub const ParseErrorContext = struct {
    last_error: ?ParseError = null,
    source: []const u8 = "",
    file_path: ?[]const u8 = null,
    allocator: Allocator,

    pub fn init(allocator: Allocator) ParseErrorContext {
        return .{ .allocator = allocator };
    }

    pub fn setSource(self: *ParseErrorContext, source: []const u8) void {
        self.source = source;
    }

    pub fn setFilePath(self: *ParseErrorContext, path: []const u8) void {
        self.file_path = path;
    }

    /// Record an error with full context
    pub fn recordError(
        self: *ParseErrorContext,
        kind: ParseError.ErrorKind,
        message: []const u8,
        line: u32,
        column: u32,
    ) void {
        // Extract the source line for context
        const source_line = self.getSourceLine(line);

        self.last_error = .{
            .kind = kind,
            .message = message,
            .line = line,
            .column = column,
            .source_line = source_line,
            .file_path = self.file_path,
        };
    }

    /// Get a specific line from the source
    fn getSourceLine(self: *const ParseErrorContext, line_num: u32) ?[]const u8 {
        if (self.source.len == 0) return null;
        if (line_num == 0) return null;

        var current_line: u32 = 1;
        var line_start: usize = 0;

        for (self.source, 0..) |c, i| {
            if (current_line == line_num) {
                // Found the line start, now find the end
                var line_end = i;
                while (line_end < self.source.len and self.source[line_end] != '\n') {
                    line_end += 1;
                }
                return self.source[line_start..line_end];
            }
            if (c == '\n') {
                current_line += 1;
                line_start = i + 1;
            }
        }

        // Handle last line without trailing newline
        if (current_line == line_num and line_start < self.source.len) {
            return self.source[line_start..];
        }

        return null;
    }

    /// Get formatted error message
    pub fn getFormattedError(self: *const ParseErrorContext) !?[]const u8 {
        if (self.last_error) |err| {
            return try err.format(self.allocator);
        }
        return null;
    }
};

/// Token types for component file parsing
pub const TokenType = enum {
    // Keywords
    ui, // 'ui' keyword (component definition)
    let_keyword, // 'let' keyword (state variable)
    fn_keyword, // 'fn' keyword
    return_keyword, // 'return' keyword

    // Legacy keywords (for backwards compat)
    component, // 'component' keyword
    state, // 'state' keyword
    prop, // 'prop' keyword

    identifier, // names
    colon, // :
    equals, // =
    comma, // ,
    semicolon, // ;

    lparen, // (
    rparen, // )
    lbrace, // {
    rbrace, // }
    lbracket, // [
    rbracket, // ]

    type_int, // int
    type_string, // string
    type_bool, // bool
    type_float, // float
    type_array, // []

    number, // 123, 3.14
    string, // "hello"
    true_lit, // true
    false_lit, // false

    html_block, // HTML template block (JSX in return)

    star, // *
    self_keyword, // Self
    void_keyword, // void

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
        const token_type: TokenType = if (std.mem.eql(u8, lexeme, "ui"))
            .ui
        else if (std.mem.eql(u8, lexeme, "let"))
            .let_keyword
        else if (std.mem.eql(u8, lexeme, "return"))
            .return_keyword
        else if (std.mem.eql(u8, lexeme, "fn"))
            .fn_keyword
        else if (std.mem.eql(u8, lexeme, "component"))
            .component
        else if (std.mem.eql(u8, lexeme, "state"))
            .state
        else if (std.mem.eql(u8, lexeme, "prop"))
            .prop
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
    error_context: *ParseErrorContext,

    state_fields: std.ArrayListUnmanaged(ParsedState),
    props: std.ArrayListUnmanaged(ParsedProp),
    methods: std.ArrayListUnmanaged(ParsedMethod),

    const Self = @This();

    pub fn init(allocator: Allocator, tokens: []const Token, error_context: *ParseErrorContext) Self {
        return .{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
            .error_context = error_context,
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
        _ = try self.expect(.component, "expected 'component' keyword at start of component definition");
        const name = try self.expect(.identifier, "expected component name after 'component' keyword");
        _ = try self.expect(.lbrace, "expected '{' after component name");

        while (!self.isAtEnd() and self.peek().token_type != .rbrace) {
            const token = self.peek();

            switch (token.token_type) {
                .state => try self.parseState(),
                .prop => try self.parseProp(),
                .fn_keyword => try self.parseMethod(),
                else => self.advance(),
            }
        }

        _ = try self.expect(.rbrace, "expected '}' to close component definition");

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
        const name = try self.expect(.identifier, "expected field name after 'state'");
        _ = try self.expect(.colon, "expected ':' after state field name");
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
        const name = try self.expect(.identifier, "expected prop name after 'prop'");
        _ = try self.expect(.colon, "expected ':' after prop name");
        const type_name = try self.expectType();

        try self.props.append(self.allocator, .{
            .name = name.lexeme,
            .type_name = type_name,
            .required = true,
        });
    }

    fn parseMethod(self: *Self) !void {
        self.advance(); // fn
        const name = try self.expect(.identifier, "expected function name after 'fn'");

        _ = try self.expect(.lparen, "expected '(' after function name");
        // Skip parameters for now
        while (self.peek().token_type != .rparen and !self.isAtEnd()) {
            self.advance();
        }
        _ = try self.expect(.rparen, "expected ')' to close function parameters");

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
        _ = try self.expect(.lbrace, "expected '{' to start function body");
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

        _ = try self.expect(.rbrace, "expected '}' to close function body");

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
        // Record detailed error
        self.error_context.recordError(
            .expected_type,
            "expected type name (int, string, bool, float, or custom type)",
            token.line,
            token.column,
        );
        return error.ExpectedType;
    }

    fn expect(self: *Self, expected: TokenType, message: []const u8) !Token {
        const token = self.peek();
        if (token.token_type != expected) {
            // Record detailed error with context
            self.error_context.recordError(
                .unexpected_token,
                message,
                token.line,
                token.column,
            );
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

// ============================================================================
// Page Parser (YAML frontmatter + HTML template format)
// ============================================================================

/// Parses .dx page files in the format:
/// ---
/// name: PageName
/// layout: default
/// state:
///   count: int = 0
/// ---
/// <template>
///   <div>{{ count }}</div>
/// </template>
/// <script>
/// fn increment(self) { self.count += 1 }
/// </script>
pub const PageParser = struct {
    source: []const u8,
    pos: usize,
    line: u32,
    column: u32,
    allocator: Allocator,
    error_context: *ParseErrorContext,

    const Self = @This();

    pub fn init(allocator: Allocator, source: []const u8, error_context: *ParseErrorContext) Self {
        return .{
            .source = source,
            .pos = 0,
            .line = 1,
            .column = 1,
            .allocator = allocator,
            .error_context = error_context,
        };
    }

    /// Parse a page file into a ParsedComponent
    pub fn parse(self: *Self) !ParsedComponent {
        var state_fields: std.ArrayListUnmanaged(ParsedState) = .empty;
        errdefer state_fields.deinit(self.allocator);

        var methods: std.ArrayListUnmanaged(ParsedMethod) = .empty;
        errdefer methods.deinit(self.allocator);

        var name: []const u8 = "Page";
        var template_content: ?[]const u8 = null;

        // Check for YAML frontmatter (starts with ---)
        if (self.startsWith("---")) {
            self.skipN(3); // Skip opening ---
            self.skipLine(); // Skip rest of line

            // Parse frontmatter until closing ---
            const fm_start = self.pos;
            while (!self.isAtEnd()) {
                if (self.startsWith("---")) {
                    const frontmatter = self.source[fm_start..self.pos];
                    self.skipN(3);
                    self.skipLine();

                    // Parse frontmatter YAML
                    var fm_result = self.parseFrontmatter(frontmatter);
                    name = fm_result.name;
                    // Copy state fields from frontmatter
                    for (fm_result.state_fields.items) |field| {
                        try state_fields.append(self.allocator, field);
                    }
                    fm_result.state_fields.deinit(self.allocator);
                    break;
                }
                self.advanceChar();
            }
        }

        // Skip whitespace
        self.skipWhitespace();

        // Look for <template> section
        if (self.findTag("template")) |tmpl| {
            template_content = tmpl;
        } else {
            // No <template> tag - treat the rest as template content
            // This is a fallback for simple pages
            const rest = self.source[self.pos..];
            if (rest.len > 0) {
                template_content = rest;
            }
        }

        // Look for <script> section
        if (self.findTag("script")) |script_content| {
            // Parse script as Cot code for methods
            try self.parseScript(script_content, &methods);
        }

        // Parse template content
        var render_template: ?template_parser.Node = null;
        if (template_content) |content| {
            var lexer = template_lexer.Lexer.init(self.allocator, content);
            defer lexer.deinit();
            const tokens = lexer.tokenize() catch |err| {
                self.error_context.recordError(
                    .invalid_syntax,
                    "failed to tokenize template content",
                    self.line,
                    self.column,
                );
                return err;
            };

            var parser = template_parser.Parser.init(self.allocator, tokens);
            defer parser.deinit();
            render_template = parser.parse() catch |err| {
                self.error_context.recordError(
                    .invalid_syntax,
                    "failed to parse template content",
                    self.line,
                    self.column,
                );
                return err;
            };
        }

        return ParsedComponent{
            .name = name,
            .state_fields = try state_fields.toOwnedSlice(self.allocator),
            .props = &.{},
            .methods = try methods.toOwnedSlice(self.allocator),
            .render_template = render_template,
        };
    }

    /// Parse YAML frontmatter
    fn parseFrontmatter(self: *Self, content: []const u8) struct {
        name: []const u8,
        state_fields: std.ArrayListUnmanaged(ParsedState),
    } {
        var name: []const u8 = "Page";
        var state_fields: std.ArrayListUnmanaged(ParsedState) = .empty;
        var in_state_section = false;

        var lines = std.mem.splitScalar(u8, content, '\n');
        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");
            if (trimmed.len == 0) continue;

            // Check for state: section
            if (std.mem.startsWith(u8, trimmed, "state:")) {
                in_state_section = true;
                continue;
            }

            // Check if we're back to top-level (not indented)
            if (!std.mem.startsWith(u8, line, " ") and !std.mem.startsWith(u8, line, "\t")) {
                in_state_section = false;
            }

            if (in_state_section) {
                // Parse state field: "  fieldname: type = default"
                if (self.parseStateField(trimmed)) |field| {
                    state_fields.append(self.allocator, field) catch {};
                }
            } else if (std.mem.startsWith(u8, trimmed, "name:")) {
                // Parse name field
                const value = std.mem.trim(u8, trimmed["name:".len..], " \t");
                if (value.len > 0) {
                    name = value;
                }
            }
            // Skip layout, meta, and other fields for now
        }

        return .{ .name = name, .state_fields = state_fields };
    }

    /// Parse a state field from YAML: "fieldname: type = default"
    fn parseStateField(self: *Self, line: []const u8) ?ParsedState {
        _ = self;
        const trimmed = std.mem.trim(u8, line, " \t");

        // Find the colon separating name from type
        const colon_pos = std.mem.indexOf(u8, trimmed, ":") orelse return null;
        const field_name = std.mem.trim(u8, trimmed[0..colon_pos], " \t");
        if (field_name.len == 0) return null;

        var rest = std.mem.trim(u8, trimmed[colon_pos + 1 ..], " \t");

        // Check for default value (= ...)
        var type_name: []const u8 = undefined;
        var default_value: ?[]const u8 = null;

        if (std.mem.indexOf(u8, rest, "=")) |eq_pos| {
            type_name = std.mem.trim(u8, rest[0..eq_pos], " \t");
            default_value = std.mem.trim(u8, rest[eq_pos + 1 ..], " \t");
        } else {
            type_name = rest;
        }

        if (type_name.len == 0) return null;

        return ParsedState{
            .name = field_name,
            .type_name = type_name,
            .default_value = default_value,
        };
    }

    /// Find and extract content between <tag> and </tag>
    fn findTag(self: *Self, tag_name: []const u8) ?[]const u8 {
        const open_tag_start = blk: {
            var search_pos = self.pos;
            while (search_pos < self.source.len) {
                if (self.source[search_pos] == '<') {
                    // Check if this is the tag we want
                    const remaining = self.source[search_pos..];
                    const expected_open = std.fmt.allocPrint(self.allocator, "<{s}", .{tag_name}) catch return null;
                    defer self.allocator.free(expected_open);

                    if (std.mem.startsWith(u8, remaining, expected_open)) {
                        break :blk search_pos;
                    }
                }
                search_pos += 1;
            }
            return null;
        };

        // Find the end of the opening tag (>)
        var content_start = open_tag_start;
        while (content_start < self.source.len and self.source[content_start] != '>') {
            content_start += 1;
        }
        if (content_start >= self.source.len) return null;
        content_start += 1; // Skip the >

        // Find closing tag
        const close_tag = std.fmt.allocPrint(self.allocator, "</{s}>", .{tag_name}) catch return null;
        defer self.allocator.free(close_tag);

        const close_tag_pos = std.mem.indexOf(u8, self.source[content_start..], close_tag) orelse return null;
        const content_end = content_start + close_tag_pos;

        return self.source[content_start..content_end];
    }

    /// Parse <script> content as Cot code
    fn parseScript(self: *Self, content: []const u8, methods: *std.ArrayListUnmanaged(ParsedMethod)) !void {
        // Simple parsing: look for "fn name(...) { ... }"
        var pos: usize = 0;

        while (pos < content.len) {
            // Skip whitespace
            while (pos < content.len and std.ascii.isWhitespace(content[pos])) {
                pos += 1;
            }
            if (pos >= content.len) break;

            // Look for "fn"
            if (pos + 2 < content.len and std.mem.eql(u8, content[pos .. pos + 2], "fn")) {
                pos += 2;
                // Skip whitespace
                while (pos < content.len and std.ascii.isWhitespace(content[pos])) {
                    pos += 1;
                }

                // Get function name
                const name_start = pos;
                while (pos < content.len and (std.ascii.isAlphanumeric(content[pos]) or content[pos] == '_')) {
                    pos += 1;
                }
                const fn_name = content[name_start..pos];

                // Skip to opening brace
                while (pos < content.len and content[pos] != '{') {
                    pos += 1;
                }
                if (pos >= content.len) break;
                pos += 1; // Skip {

                // Find matching closing brace
                var depth: u32 = 1;
                const body_start = pos;
                while (pos < content.len and depth > 0) {
                    if (content[pos] == '{') depth += 1;
                    if (content[pos] == '}') depth -= 1;
                    if (depth > 0) pos += 1;
                }
                const body_end = pos;
                if (pos < content.len) pos += 1; // Skip closing }

                const body = std.mem.trim(u8, content[body_start..body_end], " \t\n\r");

                try methods.append(self.allocator, .{
                    .name = fn_name,
                    .params = &.{},
                    .return_type = null,
                    .body = body,
                    .is_render = std.mem.eql(u8, fn_name, "render"),
                });
            } else {
                pos += 1;
            }
        }
    }

    // Helper functions
    fn startsWith(self: *const Self, prefix: []const u8) bool {
        if (self.pos + prefix.len > self.source.len) return false;
        return std.mem.eql(u8, self.source[self.pos .. self.pos + prefix.len], prefix);
    }

    fn skipN(self: *Self, n: usize) void {
        var i: usize = 0;
        while (i < n and !self.isAtEnd()) : (i += 1) {
            self.advanceChar();
        }
    }

    fn skipLine(self: *Self) void {
        while (!self.isAtEnd() and self.source[self.pos] != '\n') {
            self.advanceChar();
        }
        if (!self.isAtEnd()) self.advanceChar(); // Skip the newline
    }

    fn skipWhitespace(self: *Self) void {
        while (!self.isAtEnd() and std.ascii.isWhitespace(self.source[self.pos])) {
            self.advanceChar();
        }
    }

    fn advanceChar(self: *Self) void {
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
};

/// Detect source format
pub fn isPageFormat(source: []const u8) bool {
    const trimmed = std.mem.trimLeft(u8, source, " \t\n\r");
    return std.mem.startsWith(u8, trimmed, "---");
}

pub fn isUiFormat(source: []const u8) bool {
    const trimmed = std.mem.trimLeft(u8, source, " \t\n\r");
    return std.mem.startsWith(u8, trimmed, "ui ");
}

/// Parse source - detects format automatically (ui, page, or legacy component)
pub fn parseSource(allocator: Allocator, source: []const u8, error_context: *ParseErrorContext) !ParsedComponent {
    error_context.setSource(source);

    if (isUiFormat(source)) {
        // New ui syntax: ui Name() { let x = 0; fn foo() {}; return (...) }
        return parseUiComponent(allocator, source, error_context);
    } else if (isPageFormat(source)) {
        // YAML frontmatter format (legacy)
        var parser = PageParser.init(allocator, source, error_context);
        return parser.parse();
    } else {
        // Legacy component format
        var lexer = ComponentLexer.init(allocator, source);
        defer lexer.deinit();
        const tokens = try lexer.tokenize();

        var parser = ComponentParser.init(allocator, tokens, error_context);
        defer parser.deinit();
        return parser.parse();
    }
}

/// Parse new ui component syntax
fn parseUiComponent(allocator: Allocator, source: []const u8, error_context: *ParseErrorContext) !ParsedComponent {
    var state_fields: std.ArrayListUnmanaged(ParsedState) = .empty;
    errdefer state_fields.deinit(allocator);

    var methods: std.ArrayListUnmanaged(ParsedMethod) = .empty;
    errdefer methods.deinit(allocator);

    var pos: usize = 0;
    var line: u32 = 1;

    // Helper to skip whitespace and track line numbers
    const skipWs = struct {
        fn skip(src: []const u8, p: *usize, l: *u32) void {
            while (p.* < src.len) {
                const c = src[p.*];
                if (c == ' ' or c == '\t' or c == '\r') {
                    p.* += 1;
                } else if (c == '\n') {
                    p.* += 1;
                    l.* += 1;
                } else if (c == '/' and p.* + 1 < src.len and src[p.* + 1] == '/') {
                    // Skip line comment
                    while (p.* < src.len and src[p.*] != '\n') p.* += 1;
                } else {
                    break;
                }
            }
        }
    }.skip;

    skipWs(source, &pos, &line);

    // Expect 'ui'
    if (!std.mem.startsWith(u8, source[pos..], "ui ")) {
        error_context.recordError(.unexpected_token, "expected 'ui' keyword", line, 1);
        return error.UnexpectedToken;
    }
    pos += 3;
    skipWs(source, &pos, &line);

    // Parse component name
    const name_start = pos;
    while (pos < source.len and (std.ascii.isAlphanumeric(source[pos]) or source[pos] == '_')) {
        pos += 1;
    }
    const name = source[name_start..pos];
    if (name.len == 0) {
        error_context.recordError(.missing_component_name, "expected component name after 'ui'", line, 1);
        return error.UnexpectedToken;
    }

    skipWs(source, &pos, &line);

    // Expect ()
    if (pos >= source.len or source[pos] != '(') {
        error_context.recordError(.unexpected_token, "expected '(' after component name", line, 1);
        return error.UnexpectedToken;
    }
    pos += 1;
    skipWs(source, &pos, &line);
    if (pos >= source.len or source[pos] != ')') {
        error_context.recordError(.unexpected_token, "expected ')' after '('", line, 1);
        return error.UnexpectedToken;
    }
    pos += 1;
    skipWs(source, &pos, &line);

    // Expect {
    if (pos >= source.len or source[pos] != '{') {
        error_context.recordError(.unexpected_token, "expected '{' to start component body", line, 1);
        return error.UnexpectedToken;
    }
    pos += 1;
    skipWs(source, &pos, &line);

    var render_template: ?template_parser.Node = null;

    // Parse body: let statements, fn statements, return statement
    while (pos < source.len and source[pos] != '}') {
        skipWs(source, &pos, &line);
        if (pos >= source.len) break;

        if (std.mem.startsWith(u8, source[pos..], "let ")) {
            // Parse let statement: let name = value
            pos += 4;
            skipWs(source, &pos, &line);

            const var_name_start = pos;
            while (pos < source.len and (std.ascii.isAlphanumeric(source[pos]) or source[pos] == '_')) {
                pos += 1;
            }
            const var_name = source[var_name_start..pos];

            skipWs(source, &pos, &line);

            // Expect =
            if (pos < source.len and source[pos] == '=') {
                pos += 1;
                skipWs(source, &pos, &line);

                // Parse value until newline or statement end
                const val_start = pos;
                while (pos < source.len and source[pos] != '\n' and source[pos] != '}') {
                    pos += 1;
                }
                const val = std.mem.trim(u8, source[val_start..pos], " \t\r");

                // Infer type from value
                var type_name: []const u8 = "int";
                if (val.len >= 2 and val[0] == '"') {
                    type_name = "string";
                } else if (std.mem.eql(u8, val, "true") or std.mem.eql(u8, val, "false")) {
                    type_name = "bool";
                }

                try state_fields.append(allocator, .{
                    .name = var_name,
                    .type_name = type_name,
                    .default_value = val,
                });
            }
        } else if (std.mem.startsWith(u8, source[pos..], "fn ")) {
            // Parse fn statement
            pos += 3;
            skipWs(source, &pos, &line);

            const fn_name_start = pos;
            while (pos < source.len and (std.ascii.isAlphanumeric(source[pos]) or source[pos] == '_')) {
                pos += 1;
            }
            const fn_name = source[fn_name_start..pos];

            // Skip to {
            while (pos < source.len and source[pos] != '{') {
                pos += 1;
            }
            if (pos < source.len) pos += 1; // skip {

            // Find matching }
            const body_start = pos;
            var depth: u32 = 1;
            while (pos < source.len and depth > 0) {
                if (source[pos] == '{') depth += 1;
                if (source[pos] == '}') depth -= 1;
                if (depth > 0) pos += 1;
            }
            const fn_body = source[body_start..pos];
            if (pos < source.len) pos += 1; // skip }

            try methods.append(allocator, .{
                .name = fn_name,
                .params = &.{},
                .return_type = null,
                .body = fn_body,
                .is_render = false,
            });
        } else if (std.mem.startsWith(u8, source[pos..], "return")) {
            // Parse return ( JSX )
            pos += 6;
            skipWs(source, &pos, &line);

            if (pos < source.len and source[pos] == '(') {
                pos += 1;

                // Find matching )
                const jsx_start = pos;
                var depth: u32 = 1;
                while (pos < source.len and depth > 0) {
                    if (source[pos] == '(') depth += 1;
                    if (source[pos] == ')') depth -= 1;
                    if (depth > 0) pos += 1;
                }
                const jsx_content = source[jsx_start..pos];
                if (pos < source.len) pos += 1; // skip )

                // Parse the JSX
                var lexer = template_lexer.Lexer.init(allocator, jsx_content);
                defer lexer.deinit();
                const tokens = lexer.tokenize() catch {
                    error_context.recordError(.invalid_syntax, "failed to tokenize JSX", line, 1);
                    return error.InvalidSyntax;
                };

                var parser = template_parser.Parser.init(allocator, tokens);
                defer parser.deinit();
                render_template = parser.parse() catch {
                    error_context.recordError(.invalid_syntax, "failed to parse JSX", line, 1);
                    return error.InvalidSyntax;
                };
            }
        } else {
            // Skip unknown content
            pos += 1;
        }
    }

    return ParsedComponent{
        .name = name,
        .state_fields = try state_fields.toOwnedSlice(allocator),
        .props = &.{},
        .methods = try methods.toOwnedSlice(allocator),
        .render_template = render_template,
    };
}

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

    var error_context = ParseErrorContext.init(allocator);
    error_context.setSource(source);

    var lexer = ComponentLexer.init(allocator, source);
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parser = ComponentParser.init(allocator, tokens, &error_context);
    defer parser.deinit();

    const parsed = try parser.parse();

    try std.testing.expectEqualStrings("Counter", parsed.name);
    try std.testing.expectEqual(@as(usize, 1), parsed.state_fields.len);
    try std.testing.expectEqualStrings("count", parsed.state_fields[0].name);
}

test "parse page format with frontmatter" {
    const allocator = std.testing.allocator;

    const source =
        \\---
        \\name: TestPage
        \\layout: default
        \\state:
        \\  count: int = 0
        \\---
        \\<template>
        \\  <div>Hello</div>
        \\</template>
    ;

    var error_context = ParseErrorContext.init(allocator);
    const parsed = try parseSource(allocator, source, &error_context);

    try std.testing.expectEqualStrings("TestPage", parsed.name);
}

test "isPageFormat detection" {
    try std.testing.expect(isPageFormat("---\nname: Test\n---"));
    try std.testing.expect(isPageFormat("  ---\nname: Test\n---"));
    try std.testing.expect(!isPageFormat("component Counter {"));
    try std.testing.expect(!isPageFormat("<div>Hello</div>"));
}

test "error context records detailed errors" {
    const allocator = std.testing.allocator;

    const source = "component { }"; // Missing name

    var error_context = ParseErrorContext.init(allocator);
    error_context.setSource(source);

    var lexer = ComponentLexer.init(allocator, source);
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parser = ComponentParser.init(allocator, tokens, &error_context);
    defer parser.deinit();

    // This should fail and record an error
    const result = parser.parse();
    try std.testing.expectError(error.UnexpectedToken, result);

    // Check that error context has details
    try std.testing.expect(error_context.last_error != null);
    if (error_context.last_error) |err| {
        try std.testing.expect(err.line > 0);
        try std.testing.expect(err.message.len > 0);
    }
}
