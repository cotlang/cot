//! Dex Template Renderer
//!
//! Renders parsed template AST to HTML with expression evaluation.
//! Handles conditionals, loops, event bindings, and XSS prevention.

const std = @import("std");
const Allocator = std.mem.Allocator;
const parser = @import("parser.zig");
const Node = parser.Node;
const NodeType = parser.NodeType;
const Attribute = parser.Attribute;
const AttributeValue = parser.AttributeValue;
const EventBinding = parser.EventBinding;

/// Value type for template context
pub const Value = union(enum) {
    null_val: void,
    bool_val: bool,
    int_val: i64,
    float_val: f64,
    string_val: []const u8,
    array_val: []const Value,
    object_val: std.StringHashMapUnmanaged(Value),

    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .null_val => false,
            .bool_val => |b| b,
            .int_val => |i| i != 0,
            .float_val => |f| f != 0.0,
            .string_val => |s| s.len > 0,
            .array_val => |a| a.len > 0,
            .object_val => true,
        };
    }

    pub fn asString(self: Value, allocator: Allocator) ![]const u8 {
        return switch (self) {
            .null_val => "",
            .bool_val => |b| if (b) "true" else "false",
            .int_val => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
            .float_val => |f| try std.fmt.allocPrint(allocator, "{d}", .{f}),
            .string_val => |s| s,
            .array_val => "[array]",
            .object_val => "[object]",
        };
    }
};

/// Template rendering context
pub const Context = struct {
    values: std.StringHashMapUnmanaged(Value),
    allocator: Allocator,
    parent: ?*const Context,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .values = .empty,
            .allocator = allocator,
            .parent = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit(self.allocator);
    }

    pub fn set(self: *Self, key: []const u8, value: Value) !void {
        try self.values.put(self.allocator, key, value);
    }

    pub fn get(self: *const Self, key: []const u8) ?Value {
        if (self.values.get(key)) |v| {
            return v;
        }
        if (self.parent) |p| {
            return p.get(key);
        }
        return null;
    }

    /// Create a child context (for loops)
    pub fn child(self: *const Self, allocator: Allocator) Self {
        return .{
            .values = .empty,
            .allocator = allocator,
            .parent = self,
        };
    }
};

/// Template Renderer
pub const Renderer = struct {
    allocator: Allocator,
    output: std.ArrayListUnmanaged(u8),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .output = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.output.deinit(self.allocator);
    }

    /// Render a template AST with the given context
    pub fn render(self: *Self, node: Node, ctx: *const Context) ![]const u8 {
        try self.renderNode(node, ctx);
        return self.output.items;
    }

    fn renderNode(self: *Self, node: Node, ctx: *const Context) !void {
        switch (node.node_type) {
            .fragment => {
                for (node.children) |child| {
                    try self.renderNode(child, ctx);
                }
            },
            .text => {
                try self.output.appendSlice(self.allocator, node.data.text.content);
            },
            .expression => {
                const value = try self.evaluateExpr(node.data.expression.expr, ctx);
                const str = try value.asString(self.allocator);
                try self.appendEscaped(str);
            },
            .element => {
                try self.renderElement(node, ctx);
            },
            .component => {
                try self.renderComponent(node, ctx);
            },
            .conditional => {
                try self.renderConditional(node, ctx);
            },
            .loop => {
                try self.renderLoop(node, ctx);
            },
            .raw => {
                // Raw content - no escaping
                const value = try self.evaluateExpr(node.data.raw.content, ctx);
                const str = try value.asString(self.allocator);
                try self.output.appendSlice(self.allocator, str);
            },
        }
    }

    fn renderElement(self: *Self, node: Node, ctx: *const Context) !void {
        const elem = node.data.element;

        // Opening tag
        try self.output.append(self.allocator, '<');
        try self.output.appendSlice(self.allocator, elem.tag);

        // Attributes
        for (elem.attributes) |attr| {
            try self.output.append(self.allocator, ' ');
            try self.output.appendSlice(self.allocator, attr.name);
            try self.output.append(self.allocator, '=');
            try self.output.append(self.allocator, '"');

            switch (attr.value) {
                .static => |s| try self.appendEscaped(s),
                .expression => |expr| {
                    const value = try self.evaluateExpr(expr, ctx);
                    const str = try value.asString(self.allocator);
                    try self.appendEscaped(str);
                },
            }

            try self.output.append(self.allocator, '"');
        }

        // Event bindings (generate data attributes for client-side handling)
        for (elem.events) |event| {
            try self.output.appendSlice(self.allocator, " data-dex-");
            try self.output.appendSlice(self.allocator, event.event);
            try self.output.appendSlice(self.allocator, "=\"");
            try self.appendEscaped(event.handler);
            try self.output.append(self.allocator, '"');
        }

        if (elem.self_closing) {
            try self.output.appendSlice(self.allocator, " />");
        } else {
            try self.output.append(self.allocator, '>');

            // Children
            for (node.children) |child| {
                try self.renderNode(child, ctx);
            }

            // Closing tag
            try self.output.appendSlice(self.allocator, "</");
            try self.output.appendSlice(self.allocator, elem.tag);
            try self.output.append(self.allocator, '>');
        }
    }

    fn renderComponent(self: *Self, node: Node, ctx: *const Context) !void {
        const comp = node.data.component;

        // Components render as custom elements with data attributes
        try self.output.appendSlice(self.allocator, "<dex-");
        try self.output.appendSlice(self.allocator, toLowerKebab(comp.name));
        try self.output.appendSlice(self.allocator, " data-component=\"");
        try self.output.appendSlice(self.allocator, comp.name);
        try self.output.append(self.allocator, '"');

        // Props as data attributes
        for (comp.props) |prop| {
            try self.output.appendSlice(self.allocator, " data-prop-");
            try self.output.appendSlice(self.allocator, prop.name);
            try self.output.appendSlice(self.allocator, "=\"");

            switch (prop.value) {
                .static => |s| try self.appendEscaped(s),
                .expression => |expr| {
                    const value = try self.evaluateExpr(expr, ctx);
                    const str = try value.asString(self.allocator);
                    try self.appendEscaped(str);
                },
            }

            try self.output.append(self.allocator, '"');
        }

        // Event bindings
        for (comp.events) |event| {
            try self.output.appendSlice(self.allocator, " data-dex-");
            try self.output.appendSlice(self.allocator, event.event);
            try self.output.appendSlice(self.allocator, "=\"");
            try self.appendEscaped(event.handler);
            try self.output.append(self.allocator, '"');
        }

        if (node.children.len == 0) {
            try self.output.appendSlice(self.allocator, "></dex-");
            try self.output.appendSlice(self.allocator, toLowerKebab(comp.name));
            try self.output.append(self.allocator, '>');
        } else {
            try self.output.append(self.allocator, '>');
            for (node.children) |child| {
                try self.renderNode(child, ctx);
            }
            try self.output.appendSlice(self.allocator, "</dex-");
            try self.output.appendSlice(self.allocator, toLowerKebab(comp.name));
            try self.output.append(self.allocator, '>');
        }
    }

    fn renderConditional(self: *Self, node: Node, ctx: *const Context) !void {
        const cond = node.data.conditional;

        const value = try self.evaluateExpr(cond.condition, ctx);

        if (value.isTruthy()) {
            for (node.children) |child| {
                try self.renderNode(child, ctx);
            }
        } else if (cond.else_branch) |else_node| {
            try self.renderNode(else_node.*, ctx);
        }
    }

    fn renderLoop(self: *Self, node: Node, ctx: *const Context) !void {
        const loop_data = node.data.loop;

        const iterable_value = try self.evaluateExpr(loop_data.iterable, ctx);

        switch (iterable_value) {
            .array_val => |arr| {
                for (arr, 0..) |item, i| {
                    var child_ctx = ctx.child(self.allocator);
                    defer child_ctx.deinit();

                    try child_ctx.set(loop_data.item_name, item);
                    if (loop_data.index_name) |idx_name| {
                        try child_ctx.set(idx_name, .{ .int_val = @intCast(i) });
                    }

                    for (node.children) |child| {
                        try self.renderNode(child, &child_ctx);
                    }
                }
            },
            else => {
                // Not iterable, skip
            },
        }
    }

    /// Evaluate an expression in the current context
    fn evaluateExpr(self: *Self, expr: []const u8, ctx: *const Context) !Value {
        _ = self;

        // Simple expression evaluation
        // Handles: variable, object.property, object.property.nested

        const trimmed = std.mem.trim(u8, expr, " \t\n");
        if (trimmed.len == 0) {
            return .{ .null_val = {} };
        }

        // Check for string literal
        if (trimmed.len >= 2 and (trimmed[0] == '"' or trimmed[0] == '\'')) {
            const quote = trimmed[0];
            if (trimmed[trimmed.len - 1] == quote) {
                return .{ .string_val = trimmed[1 .. trimmed.len - 1] };
            }
        }

        // Check for number literal
        if (std.fmt.parseInt(i64, trimmed, 10)) |n| {
            return .{ .int_val = n };
        } else |_| {}

        // Check for boolean
        if (std.mem.eql(u8, trimmed, "true")) {
            return .{ .bool_val = true };
        }
        if (std.mem.eql(u8, trimmed, "false")) {
            return .{ .bool_val = false };
        }

        // Property access: a.b.c
        var parts = std.mem.splitScalar(u8, trimmed, '.');
        const first = parts.first();

        var value = ctx.get(first) orelse return .{ .null_val = {} };

        while (parts.next()) |prop| {
            switch (value) {
                .object_val => |obj| {
                    value = obj.get(prop) orelse return .{ .null_val = {} };
                },
                else => return .{ .null_val = {} },
            }
        }

        return value;
    }

    /// Append HTML-escaped string
    fn appendEscaped(self: *Self, str: []const u8) !void {
        for (str) |c| {
            switch (c) {
                '<' => try self.output.appendSlice(self.allocator, "&lt;"),
                '>' => try self.output.appendSlice(self.allocator, "&gt;"),
                '&' => try self.output.appendSlice(self.allocator, "&amp;"),
                '"' => try self.output.appendSlice(self.allocator, "&quot;"),
                '\'' => try self.output.appendSlice(self.allocator, "&#x27;"),
                else => try self.output.append(self.allocator, c),
            }
        }
    }
};

/// Convert PascalCase to lower-kebab-case
fn toLowerKebab(name: []const u8) []const u8 {
    // For now, just return the name in lowercase
    // TODO: proper kebab-case conversion
    return name;
}

// ============================================================================
// Tests
// ============================================================================

test "render simple text" {
    const allocator = std.testing.allocator;

    const lexer_mod = @import("lexer.zig");
    var lexer = lexer_mod.Lexer.init(allocator, "<p>Hello World</p>");
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parse = parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    const ast = try parse.parse();

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    var renderer = Renderer.init(allocator);
    defer renderer.deinit();

    const html = try renderer.render(ast, &ctx);
    try std.testing.expectEqualStrings("<p>Hello World</p>", html);
}

test "render expression" {
    const allocator = std.testing.allocator;

    const lexer_mod = @import("lexer.zig");
    var lexer = lexer_mod.Lexer.init(allocator, "<h1>{name}</h1>");
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parse = parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    const ast = try parse.parse();

    var ctx = Context.init(allocator);
    defer ctx.deinit();
    try ctx.set("name", .{ .string_val = "Dex" });

    var renderer = Renderer.init(allocator);
    defer renderer.deinit();

    const html = try renderer.render(ast, &ctx);
    try std.testing.expectEqualStrings("<h1>Dex</h1>", html);
}

test "render with html escaping" {
    const allocator = std.testing.allocator;

    const lexer_mod = @import("lexer.zig");
    var lexer = lexer_mod.Lexer.init(allocator, "<p>{content}</p>");
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parse = parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    const ast = try parse.parse();

    var ctx = Context.init(allocator);
    defer ctx.deinit();
    try ctx.set("content", .{ .string_val = "<script>alert('xss')</script>" });

    var renderer = Renderer.init(allocator);
    defer renderer.deinit();

    const html = try renderer.render(ast, &ctx);
    try std.testing.expectEqualStrings("<p>&lt;script&gt;alert(&#x27;xss&#x27;)&lt;/script&gt;</p>", html);
}

test "render event binding" {
    const allocator = std.testing.allocator;

    const lexer_mod = @import("lexer.zig");
    var lexer = lexer_mod.Lexer.init(allocator,
        \\<button @click="increment">+</button>
    );
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parse = parser.Parser.init(allocator, tokens);
    defer parse.deinit();
    const ast = try parse.parse();

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    var renderer = Renderer.init(allocator);
    defer renderer.deinit();

    const html = try renderer.render(ast, &ctx);
    try std.testing.expectEqualStrings("<button data-dex-click=\"increment\">+</button>", html);
}
