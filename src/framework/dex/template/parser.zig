//! Dex Template Parser
//!
//! Parses template tokens into an AST for rendering.
//! Supports HTML elements, Cot expressions, directives, and components.

const std = @import("std");
const Allocator = std.mem.Allocator;
const lexer = @import("lexer.zig");
const Token = lexer.Token;
const TokenType = lexer.TokenType;

/// AST Node types
pub const NodeType = enum {
    element,      // HTML element: <div>...</div>
    text,         // Plain text content
    expression,   // Cot expression: {expr}
    conditional,  // @if / @else
    loop,         // @for item in items
    component,    // <ComponentName ... />
    raw,          // @raw {content} - unescaped HTML
    fragment,     // Root container for multiple nodes
};

/// Attribute on an element or component
pub const Attribute = struct {
    name: []const u8,
    value: AttributeValue,
};

/// Attribute value can be static string or dynamic expression
pub const AttributeValue = union(enum) {
    static: []const u8,
    expression: []const u8,
};

/// Event binding (@click, @input, etc.)
pub const EventBinding = struct {
    event: []const u8,   // "click", "input", "submit"
    handler: []const u8, // Handler name or expression
};

/// AST Node
pub const Node = struct {
    node_type: NodeType,
    data: NodeData,
    children: []const Node,
    line: u32,
    column: u32,
};

/// Node-specific data
pub const NodeData = union(enum) {
    element: ElementData,
    text: TextData,
    expression: ExpressionData,
    conditional: ConditionalData,
    loop: LoopData,
    component: ComponentData,
    raw: RawData,
    fragment: void,
};

pub const ElementData = struct {
    tag: []const u8,
    attributes: []const Attribute,
    events: []const EventBinding,
    self_closing: bool,
};

pub const TextData = struct {
    content: []const u8,
};

pub const ExpressionData = struct {
    expr: []const u8,
};

pub const ConditionalData = struct {
    condition: []const u8,
    else_branch: ?*const Node,
};

pub const LoopData = struct {
    item_name: []const u8,
    index_name: ?[]const u8,
    iterable: []const u8,
};

pub const ComponentData = struct {
    name: []const u8,
    props: []const Attribute,
    events: []const EventBinding,
};

pub const RawData = struct {
    content: []const u8,
};

/// Template Parser
pub const Parser = struct {
    tokens: []const Token,
    pos: usize,
    allocator: Allocator,
    nodes: std.ArrayListUnmanaged(Node),
    errors: std.ArrayListUnmanaged(ParseError),

    const Self = @This();

    pub const ParseError = struct {
        message: []const u8,
        line: u32,
        column: u32,
    };

    pub fn init(allocator: Allocator, tokens: []const Token) Self {
        return .{
            .tokens = tokens,
            .pos = 0,
            .allocator = allocator,
            .nodes = .empty,
            .errors = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.nodes.deinit(self.allocator);
        self.errors.deinit(self.allocator);
    }

    /// Parse all tokens into an AST
    pub fn parse(self: *Self) !Node {
        var children: std.ArrayListUnmanaged(Node) = .empty;
        defer children.deinit(self.allocator);

        while (!self.isAtEnd()) {
            if (try self.parseNode()) |node| {
                try children.append(self.allocator, node);
            }
        }

        return Node{
            .node_type = .fragment,
            .data = .{ .fragment = {} },
            .children = try children.toOwnedSlice(self.allocator),
            .line = 1,
            .column = 1,
        };
    }

    /// Parse a single node
    fn parseNode(self: *Self) Allocator.Error!?Node {
        const token = self.peek();

        switch (token.token_type) {
            .text => return try self.parseText(),
            .tag_open => return try self.parseElement(),
            .tag_end_open => {
                // Unexpected closing tag at top level
                _ = self.advance();
                return null;
            },
            .expr_open => return try self.parseExpression(),
            .directive_if => return try self.parseConditional(),
            .directive_for => return try self.parseLoop(),
            .directive_raw => return try self.parseRaw(),
            .eof => return null,
            else => {
                // Skip unexpected tokens
                _ = self.advance();
                return null;
            },
        }
    }

    /// Parse text node
    fn parseText(self: *Self) Allocator.Error!Node {
        const token = self.advance();
        return Node{
            .node_type = .text,
            .data = .{ .text = .{ .content = token.lexeme } },
            .children = &.{},
            .line = token.line,
            .column = token.column,
        };
    }

    /// Parse HTML element or component
    fn parseElement(self: *Self) Allocator.Error!Node {
        const start_token = self.advance(); // <

        // Get tag/component name
        const name_token = self.advance();
        const is_component = name_token.token_type == .component_name;
        const name = name_token.lexeme;

        // Parse attributes and events
        var attributes: std.ArrayListUnmanaged(Attribute) = .empty;
        defer attributes.deinit(self.allocator);
        var events: std.ArrayListUnmanaged(EventBinding) = .empty;
        defer events.deinit(self.allocator);

        while (!self.isAtEnd()) {
            const t = self.peek();

            if (t.token_type == .tag_close or t.token_type == .tag_self_close) {
                break;
            }

            if (t.token_type == .attr_name) {
                const attr = try self.parseAttribute();
                try attributes.append(self.allocator, attr);
            } else if (t.token_type == .event_name) {
                const event = try self.parseEvent();
                try events.append(self.allocator, event);
            } else {
                _ = self.advance();
            }
        }

        // Check for self-closing
        const close_token = self.peek();
        const self_closing = close_token.token_type == .tag_self_close;
        _ = self.advance(); // consume > or />

        // Parse children if not self-closing
        var children: std.ArrayListUnmanaged(Node) = .empty;
        defer children.deinit(self.allocator);

        if (!self_closing) {
            while (!self.isAtEnd()) {
                // Check for closing tag
                if (self.peek().token_type == .tag_end_open) {
                    _ = self.advance(); // </
                    if (self.peek().token_type == .tag_name or
                        self.peek().token_type == .component_name)
                    {
                        const closing_name = self.advance().lexeme;
                        if (std.mem.eql(u8, closing_name, name)) {
                            // Matching close tag - consume >
                            if (self.peek().token_type == .tag_close) {
                                _ = self.advance();
                            }
                            break;
                        }
                    }
                }

                if (try self.parseNode()) |child| {
                    try children.append(self.allocator, child);
                }
            }
        }

        if (is_component) {
            return Node{
                .node_type = .component,
                .data = .{ .component = .{
                    .name = name,
                    .props = try attributes.toOwnedSlice(self.allocator),
                    .events = try events.toOwnedSlice(self.allocator),
                } },
                .children = try children.toOwnedSlice(self.allocator),
                .line = start_token.line,
                .column = start_token.column,
            };
        } else {
            return Node{
                .node_type = .element,
                .data = .{ .element = .{
                    .tag = name,
                    .attributes = try attributes.toOwnedSlice(self.allocator),
                    .events = try events.toOwnedSlice(self.allocator),
                    .self_closing = self_closing,
                } },
                .children = try children.toOwnedSlice(self.allocator),
                .line = start_token.line,
                .column = start_token.column,
            };
        }
    }

    /// Parse an attribute
    fn parseAttribute(self: *Self) !Attribute {
        const name = self.advance().lexeme;

        // Check for =
        if (self.peek().token_type == .attr_equals) {
            _ = self.advance(); // =

            const value_token = self.peek();
            if (value_token.token_type == .attr_value) {
                _ = self.advance();
                return Attribute{
                    .name = name,
                    .value = .{ .static = value_token.lexeme },
                };
            } else if (value_token.token_type == .expr_open) {
                // Expression value
                _ = self.advance(); // {
                const expr_token = self.advance(); // expression
                if (self.peek().token_type == .expr_close) {
                    _ = self.advance(); // }
                }
                return Attribute{
                    .name = name,
                    .value = .{ .expression = expr_token.lexeme },
                };
            }
        }

        // Boolean attribute (no value)
        return Attribute{
            .name = name,
            .value = .{ .static = "" },
        };
    }

    /// Parse an event binding
    fn parseEvent(self: *Self) !EventBinding {
        const event = self.advance().lexeme;

        var handler: []const u8 = "";

        if (self.peek().token_type == .attr_equals) {
            _ = self.advance(); // =

            if (self.peek().token_type == .attr_value) {
                handler = self.advance().lexeme;
            }
        }

        return EventBinding{
            .event = event,
            .handler = handler,
        };
    }

    /// Parse expression interpolation
    fn parseExpression(self: *Self) Allocator.Error!Node {
        const start = self.advance(); // {

        var expr: []const u8 = "";
        if (self.peek().token_type == .expr_content) {
            expr = self.advance().lexeme;
        }

        if (self.peek().token_type == .expr_close) {
            _ = self.advance(); // }
        }

        return Node{
            .node_type = .expression,
            .data = .{ .expression = .{ .expr = expr } },
            .children = &.{},
            .line = start.line,
            .column = start.column,
        };
    }

    /// Parse @if conditional
    fn parseConditional(self: *Self) Allocator.Error!Node {
        const start = self.advance(); // @if

        // Parse condition (until {)
        var condition: []const u8 = "";
        while (!self.isAtEnd() and self.peek().token_type != .block_open and
            self.peek().token_type != .expr_open)
        {
            if (self.peek().token_type == .text) {
                condition = std.mem.trim(u8, self.advance().lexeme, " \t\n");
            } else {
                _ = self.advance();
            }
        }

        // Parse body
        var children: std.ArrayListUnmanaged(Node) = .empty;
        defer children.deinit(self.allocator);

        if (self.peek().token_type == .block_open or self.peek().token_type == .expr_open) {
            _ = self.advance(); // {

            var brace_depth: u32 = 1;
            while (!self.isAtEnd() and brace_depth > 0) {
                const t = self.peek();
                if (t.token_type == .block_open or t.token_type == .expr_open) {
                    brace_depth += 1;
                } else if (t.token_type == .block_close or t.token_type == .expr_close) {
                    brace_depth -= 1;
                    if (brace_depth == 0) {
                        _ = self.advance();
                        break;
                    }
                }

                if (try self.parseNode()) |child| {
                    try children.append(self.allocator, child);
                }
            }
        }

        // TODO: Parse @else branch

        return Node{
            .node_type = .conditional,
            .data = .{ .conditional = .{
                .condition = condition,
                .else_branch = null,
            } },
            .children = try children.toOwnedSlice(self.allocator),
            .line = start.line,
            .column = start.column,
        };
    }

    /// Parse @for loop
    fn parseLoop(self: *Self) Allocator.Error!Node {
        const start = self.advance(); // @for

        // Parse: item in iterable or item, index in iterable
        var item_name: []const u8 = "";
        const index_name: ?[]const u8 = null;
        var iterable: []const u8 = "";

        // Simple parsing: look for text tokens with "in"
        while (!self.isAtEnd() and self.peek().token_type != .block_open and
            self.peek().token_type != .expr_open)
        {
            if (self.peek().token_type == .text) {
                const text = std.mem.trim(u8, self.advance().lexeme, " \t\n");
                if (std.mem.indexOf(u8, text, " in ")) |in_pos| {
                    item_name = text[0..in_pos];
                    iterable = text[in_pos + 4 ..];
                }
            } else {
                _ = self.advance();
            }
        }

        // Parse body
        var children: std.ArrayListUnmanaged(Node) = .empty;
        defer children.deinit(self.allocator);

        if (self.peek().token_type == .block_open or self.peek().token_type == .expr_open) {
            _ = self.advance(); // {

            var brace_depth: u32 = 1;
            while (!self.isAtEnd() and brace_depth > 0) {
                const t = self.peek();
                if (t.token_type == .block_open or t.token_type == .expr_open) {
                    brace_depth += 1;
                } else if (t.token_type == .block_close or t.token_type == .expr_close) {
                    brace_depth -= 1;
                    if (brace_depth == 0) {
                        _ = self.advance();
                        break;
                    }
                }

                if (try self.parseNode()) |child| {
                    try children.append(self.allocator, child);
                }
            }
        }

        return Node{
            .node_type = .loop,
            .data = .{ .loop = .{
                .item_name = item_name,
                .index_name = index_name,
                .iterable = iterable,
            } },
            .children = try children.toOwnedSlice(self.allocator),
            .line = start.line,
            .column = start.column,
        };
    }

    /// Parse @raw directive
    fn parseRaw(self: *Self) !Node {
        const start = self.advance(); // @raw

        var content: []const u8 = "";

        if (self.peek().token_type == .expr_open) {
            _ = self.advance(); // {
            if (self.peek().token_type == .expr_content) {
                content = self.advance().lexeme;
            }
            if (self.peek().token_type == .expr_close) {
                _ = self.advance(); // }
            }
        }

        return Node{
            .node_type = .raw,
            .data = .{ .raw = .{ .content = content } },
            .children = &.{},
            .line = start.line,
            .column = start.column,
        };
    }

    // Helper methods

    fn peek(self: *const Self) Token {
        if (self.pos >= self.tokens.len) {
            return Token{
                .token_type = .eof,
                .lexeme = "",
                .line = 0,
                .column = 0,
            };
        }
        return self.tokens[self.pos];
    }

    fn advance(self: *Self) Token {
        const token = self.peek();
        if (self.pos < self.tokens.len) {
            self.pos += 1;
        }
        return token;
    }

    fn isAtEnd(self: *const Self) bool {
        return self.pos >= self.tokens.len or self.peek().token_type == .eof;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "parser simple element" {
    const allocator = std.testing.allocator;

    var lex = lexer.Lexer.init(allocator, "<div>Hello</div>");
    defer lex.deinit();
    const tokens = try lex.tokenize();

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();
    const ast = try parser.parse();

    try std.testing.expectEqual(NodeType.fragment, ast.node_type);
    try std.testing.expectEqual(@as(usize, 1), ast.children.len);

    const div = ast.children[0];
    try std.testing.expectEqual(NodeType.element, div.node_type);
    try std.testing.expectEqualStrings("div", div.data.element.tag);
}

test "parser expression" {
    const allocator = std.testing.allocator;

    var lex = lexer.Lexer.init(allocator, "<h1>{title}</h1>");
    defer lex.deinit();
    const tokens = try lex.tokenize();

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();
    const ast = try parser.parse();

    const h1 = ast.children[0];
    try std.testing.expectEqual(@as(usize, 1), h1.children.len);

    const expr = h1.children[0];
    try std.testing.expectEqual(NodeType.expression, expr.node_type);
    try std.testing.expectEqualStrings("title", expr.data.expression.expr);
}

test "parser component" {
    const allocator = std.testing.allocator;

    var lex = lexer.Lexer.init(allocator, "<UserCard name={user.name} />");
    defer lex.deinit();
    const tokens = try lex.tokenize();

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();
    const ast = try parser.parse();

    const component = ast.children[0];
    try std.testing.expectEqual(NodeType.component, component.node_type);
    try std.testing.expectEqualStrings("UserCard", component.data.component.name);
}

test "parser event binding" {
    const allocator = std.testing.allocator;

    var lex = lexer.Lexer.init(allocator,
        \\<button @click="handleClick">Click</button>
    );
    defer lex.deinit();
    const tokens = try lex.tokenize();

    var parser = Parser.init(allocator, tokens);
    defer parser.deinit();
    const ast = try parser.parse();

    const button = ast.children[0];
    try std.testing.expectEqual(@as(usize, 1), button.data.element.events.len);
    try std.testing.expectEqualStrings("click", button.data.element.events[0].event);
    try std.testing.expectEqualStrings("handleClick", button.data.element.events[0].handler);
}
