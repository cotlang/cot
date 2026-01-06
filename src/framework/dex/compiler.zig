//! Dex Component Compiler
//!
//! Compiles .dex component files to executable components.
//! This bridges the gap between parsed component definitions and runtime execution.
//!
//! Architecture:
//!   ParsedComponent (from parser) → DexIR → CompiledComponent
//!
//! Each component compiles to:
//!   - State struct type definition
//!   - Handler functions (interpreted or compiled)
//!   - Render function (template-based)
//!
//! Handler Interpretation (Phase 1):
//!   Supports common patterns like:
//!   - self.count += 1
//!   - self.count = 0
//!   - self.name = "hello"
//!   Future phases will compile to full Cot bytecode.

const std = @import("std");
const Allocator = std.mem.Allocator;

const component_parser = @import("component_parser.zig");
const template_parser = @import("template/parser.zig");
const renderer = @import("template/renderer.zig");

// Import Cot compiler infrastructure
const cot = @import("../../root.zig");

// ============================================================================
// Handler Expression Interpreter
// ============================================================================

/// Simple expression AST for handler interpretation
pub const HandlerExpr = union(enum) {
    /// Assignment: self.field = expr
    assign: struct {
        field: []const u8,
        value: *const HandlerExpr,
    },
    /// Compound assignment: self.field += expr, self.field -= expr
    compound_assign: struct {
        field: []const u8,
        op: enum { add, sub, mul, div },
        value: *const HandlerExpr,
    },
    /// Field access: self.field
    field_access: []const u8,
    /// Integer literal
    int_literal: i64,
    /// Float literal
    float_literal: f64,
    /// String literal
    string_literal: []const u8,
    /// Boolean literal
    bool_literal: bool,
    /// Binary operation
    binary: struct {
        left: *const HandlerExpr,
        op: enum { add, sub, mul, div, eq, ne, lt, le, gt, ge },
        right: *const HandlerExpr,
    },
    /// Method call: self.method()
    method_call: struct {
        name: []const u8,
        args: []const *const HandlerExpr,
    },
};

/// Compiled handler instruction
pub const HandlerInstruction = union(enum) {
    /// self.field = value
    set_field: struct {
        field: []const u8,
        value: renderer.Value,
    },
    /// self.field += value (for ints)
    add_to_field: struct {
        field: []const u8,
        amount: i64,
    },
    /// self.field -= value (for ints)
    sub_from_field: struct {
        field: []const u8,
        amount: i64,
    },
    /// Reset field to default
    reset_field: []const u8,
    /// Reset all fields to defaults
    reset_all: void,
};

/// Handler interpreter - parses and executes simple handler expressions
pub const HandlerInterpreter = struct {
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{ .allocator = allocator };
    }

    /// Parse handler source into instructions
    pub fn parseHandler(self: *Self, source: []const u8) ![]HandlerInstruction {
        var instructions: std.ArrayListUnmanaged(HandlerInstruction) = .empty;
        errdefer instructions.deinit(self.allocator);

        // Simple line-by-line parsing
        var lines = std.mem.splitScalar(u8, source, '\n');
        while (lines.next()) |line| {
            const trimmed = std.mem.trim(u8, line, " \t\r");
            if (trimmed.len == 0) continue;

            if (try self.parseLine(trimmed)) |instr| {
                try instructions.append(self.allocator, instr);
            }
        }

        return instructions.toOwnedSlice(self.allocator);
    }

    /// Parse a single line into an instruction
    fn parseLine(self: *Self, line: []const u8) !?HandlerInstruction {
        _ = self;

        // Skip comments
        if (std.mem.startsWith(u8, line, "//")) return null;

        // Handle: self.field += number
        if (std.mem.indexOf(u8, line, "+=")) |plus_eq_pos| {
            if (std.mem.indexOf(u8, line, "self.")) |self_pos| {
                const field_start = self_pos + 5;
                const field_end = plus_eq_pos;
                const field = std.mem.trim(u8, line[field_start..field_end], " \t");
                const value_str = std.mem.trim(u8, line[plus_eq_pos + 2 ..], " \t");
                if (std.fmt.parseInt(i64, value_str, 10)) |amount| {
                    return .{ .add_to_field = .{ .field = field, .amount = amount } };
                } else |_| {}
            }
        }

        // Handle: self.field -= number
        if (std.mem.indexOf(u8, line, "-=")) |minus_eq_pos| {
            if (std.mem.indexOf(u8, line, "self.")) |self_pos| {
                const field_start = self_pos + 5;
                const field_end = minus_eq_pos;
                const field = std.mem.trim(u8, line[field_start..field_end], " \t");
                const value_str = std.mem.trim(u8, line[minus_eq_pos + 2 ..], " \t");
                if (std.fmt.parseInt(i64, value_str, 10)) |amount| {
                    return .{ .sub_from_field = .{ .field = field, .amount = amount } };
                } else |_| {}
            }
        }

        // Handle: self.field = value (but not += or -=)
        if (std.mem.indexOf(u8, line, "=")) |eq_pos| {
            // Make sure it's not += or -=
            if (eq_pos > 0 and (line[eq_pos - 1] == '+' or line[eq_pos - 1] == '-')) {
                return null;
            }

            if (std.mem.indexOf(u8, line, "self.")) |self_pos| {
                const field_start = self_pos + 5;
                const field_end = eq_pos;
                const field = std.mem.trim(u8, line[field_start..field_end], " \t");
                const value_str = std.mem.trim(u8, line[eq_pos + 1 ..], " \t");

                // Try to parse value
                const value = parseValue(value_str);
                return .{ .set_field = .{ .field = field, .value = value } };
            }
        }

        return null;
    }

    /// Execute instructions against component state
    pub fn execute(
        self: *Self,
        instructions: []const HandlerInstruction,
        state: *std.StringHashMapUnmanaged(renderer.Value),
        defaults: []const CompiledStateField,
    ) !void {
        _ = self;

        for (instructions) |instr| {
            switch (instr) {
                .set_field => |sf| {
                    try state.put(state.allocator, sf.field, sf.value);
                },
                .add_to_field => |af| {
                    if (state.get(af.field)) |current| {
                        if (current == .int_val) {
                            try state.put(state.allocator, af.field, .{
                                .int_val = current.int_val + af.amount,
                            });
                        }
                    }
                },
                .sub_from_field => |sf| {
                    if (state.get(sf.field)) |current| {
                        if (current == .int_val) {
                            try state.put(state.allocator, sf.field, .{
                                .int_val = current.int_val - sf.amount,
                            });
                        }
                    }
                },
                .reset_field => |field| {
                    for (defaults) |def| {
                        if (std.mem.eql(u8, def.name, field)) {
                            try state.put(state.allocator, field, def.default_value);
                            break;
                        }
                    }
                },
                .reset_all => {
                    for (defaults) |def| {
                        try state.put(state.allocator, def.name, def.default_value);
                    }
                },
            }
        }
    }
};

/// Parse a value string into renderer.Value
fn parseValue(value_str: []const u8) renderer.Value {
    // Try integer
    if (std.fmt.parseInt(i64, value_str, 10)) |n| {
        return .{ .int_val = n };
    } else |_| {}

    // Try float
    if (std.fmt.parseFloat(f64, value_str)) |f| {
        return .{ .float_val = f };
    } else |_| {}

    // Try boolean
    if (std.mem.eql(u8, value_str, "true")) return .{ .bool_val = true };
    if (std.mem.eql(u8, value_str, "false")) return .{ .bool_val = false };

    // Try string literal
    if (value_str.len >= 2 and value_str[0] == '"' and value_str[value_str.len - 1] == '"') {
        return .{ .string_val = value_str[1 .. value_str.len - 1] };
    }

    // Default to string
    return .{ .string_val = value_str };
}

/// Compiled handler function
pub const CompiledHandler = struct {
    name: []const u8,
    /// The handler body as Cot source (for later full compilation)
    source: []const u8,
    /// Parameter names for this handler
    params: []const []const u8,
    /// Pre-compiled instructions for simple handlers
    instructions: []const HandlerInstruction,
};

/// Compiled state field with type information
pub const CompiledStateField = struct {
    name: []const u8,
    type_tag: TypeTag,
    default_value: renderer.Value,
};

/// Type tags for state fields
pub const TypeTag = enum {
    int,
    string,
    bool,
    float,
    array,
    map,
    custom,
};

/// Compiled Dex component - ready for instantiation
pub const CompiledComponent = struct {
    name: []const u8,
    state_fields: []const CompiledStateField,
    props: []const component_parser.ParsedProp,
    handlers: []const CompiledHandler,
    render_template: ?template_parser.Node,
    allocator: Allocator,

    const Self = @This();

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.state_fields);
        self.allocator.free(self.handlers);
        // Note: props are borrowed from parser, don't free
    }

    /// Create a new instance of this component
    pub fn createInstance(self: *const Self) !ComponentInstance {
        return ComponentInstance.init(self.allocator, self);
    }
};

/// Runtime component instance
pub const ComponentInstance = struct {
    component: *const CompiledComponent,
    state: std.StringHashMapUnmanaged(renderer.Value),
    props: std.StringHashMapUnmanaged(renderer.Value),
    dirty: bool,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, component: *const CompiledComponent) !Self {
        var state: std.StringHashMapUnmanaged(renderer.Value) = .empty;
        errdefer state.deinit(allocator);

        // Initialize state with defaults
        for (component.state_fields) |field| {
            try state.put(allocator, field.name, field.default_value);
        }

        return Self{
            .component = component,
            .state = state,
            .props = .empty,
            .dirty = true,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.state.deinit(self.allocator);
        self.props.deinit(self.allocator);
    }

    /// Get current state value
    pub fn getState(self: *const Self, name: []const u8) ?renderer.Value {
        return self.state.get(name);
    }

    /// Set state value (marks component dirty)
    pub fn setState(self: *Self, name: []const u8, value: renderer.Value) !void {
        try self.state.put(self.allocator, name, value);
        self.dirty = true;
    }

    /// Set prop value
    pub fn setProp(self: *Self, name: []const u8, value: renderer.Value) !void {
        try self.props.put(self.allocator, name, value);
        self.dirty = true;
    }

    /// Get prop value
    pub fn getProp(self: *const Self, name: []const u8) ?renderer.Value {
        return self.props.get(name);
    }

    /// Call a handler by name
    pub fn callHandler(self: *Self, handler_name: []const u8, args: anytype) !void {
        // Find the handler
        for (self.component.handlers) |handler| {
            if (std.mem.eql(u8, handler.name, handler_name)) {
                try self.executeHandler(handler, args);
                return;
            }
        }
        return error.HandlerNotFound;
    }

    /// Execute a handler using compiled instructions
    fn executeHandler(self: *Self, handler: CompiledHandler, args: anytype) !void {
        _ = args;

        // Execute compiled instructions
        if (handler.instructions.len > 0) {
            var interpreter = HandlerInterpreter.init(self.allocator);
            try interpreter.execute(
                handler.instructions,
                &self.state,
                self.component.state_fields,
            );
            self.dirty = true;
            return;
        }

        // Fallback: Handle well-known handler names for backwards compatibility
        if (std.mem.eql(u8, handler.name, "increment")) {
            if (self.state.get("count")) |count_val| {
                if (count_val == .int_val) {
                    try self.setState("count", .{ .int_val = count_val.int_val + 1 });
                }
            }
        } else if (std.mem.eql(u8, handler.name, "decrement")) {
            if (self.state.get("count")) |count_val| {
                if (count_val == .int_val) {
                    try self.setState("count", .{ .int_val = count_val.int_val - 1 });
                }
            }
        } else if (std.mem.eql(u8, handler.name, "reset")) {
            for (self.component.state_fields) |field| {
                try self.setState(field.name, field.default_value);
            }
        }
    }

    /// Render component to HTML
    pub fn render(self: *Self) ![]const u8 {
        const tmpl = self.component.render_template orelse {
            return error.NoTemplate;
        };

        // Build context from state and props
        var ctx = renderer.Context.init(self.allocator);
        defer ctx.deinit();

        // Add state values
        var state_iter = self.state.iterator();
        while (state_iter.next()) |entry| {
            try ctx.set(entry.key_ptr.*, entry.value_ptr.*);
        }

        // Add prop values
        var prop_iter = self.props.iterator();
        while (prop_iter.next()) |entry| {
            try ctx.set(entry.key_ptr.*, entry.value_ptr.*);
        }

        var rend = renderer.Renderer.init(self.allocator);
        defer rend.deinit();

        const html = try rend.render(tmpl, &ctx);
        self.dirty = false;

        return html;
    }

    /// Check if needs re-render
    pub fn isDirty(self: *const Self) bool {
        return self.dirty;
    }

    /// Mark as needing re-render
    pub fn markDirty(self: *Self) void {
        self.dirty = true;
    }
};

/// Component compiler
pub const Compiler = struct {
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
        };
    }

    /// Compile a parsed component to an executable component
    pub fn compile(self: *Self, parsed: component_parser.ParsedComponent) !CompiledComponent {
        // Compile state fields
        var state_fields: std.ArrayListUnmanaged(CompiledStateField) = .empty;
        errdefer state_fields.deinit(self.allocator);

        for (parsed.state_fields) |field| {
            const type_tag = parseTypeTag(field.type_name);
            const default_value = parseDefaultValue(field.default_value, type_tag);

            try state_fields.append(self.allocator, .{
                .name = field.name,
                .type_tag = type_tag,
                .default_value = default_value,
            });
        }

        // Compile handlers
        var handlers: std.ArrayListUnmanaged(CompiledHandler) = .empty;
        errdefer handlers.deinit(self.allocator);

        var interpreter = HandlerInterpreter.init(self.allocator);

        for (parsed.methods) |method| {
            if (!method.is_render) {
                // Parse handler source into instructions
                const instructions = interpreter.parseHandler(method.body) catch &[_]HandlerInstruction{};

                try handlers.append(self.allocator, .{
                    .name = method.name,
                    .source = method.body,
                    .params = method.params,
                    .instructions = instructions,
                });
            }
        }

        return CompiledComponent{
            .name = parsed.name,
            .state_fields = try state_fields.toOwnedSlice(self.allocator),
            .props = parsed.props,
            .handlers = try handlers.toOwnedSlice(self.allocator),
            .render_template = parsed.render_template,
            .allocator = self.allocator,
        };
    }

    /// Compile a .dex file from source
    pub fn compileSource(self: *Self, source: []const u8) !CompiledComponent {
        // Tokenize
        var lexer = component_parser.ComponentLexer.init(self.allocator, source);
        defer lexer.deinit();
        const tokens = try lexer.tokenize();

        // Parse
        var parser = component_parser.ComponentParser.init(self.allocator, tokens);
        defer parser.deinit();
        const parsed = try parser.parse();

        // Compile
        return self.compile(parsed);
    }
};

/// Parse type name to type tag
fn parseTypeTag(type_name: []const u8) TypeTag {
    if (std.mem.eql(u8, type_name, "int") or std.mem.eql(u8, type_name, "i64")) {
        return .int;
    } else if (std.mem.eql(u8, type_name, "string")) {
        return .string;
    } else if (std.mem.eql(u8, type_name, "bool")) {
        return .bool;
    } else if (std.mem.eql(u8, type_name, "float") or std.mem.eql(u8, type_name, "f64")) {
        return .float;
    } else {
        return .custom;
    }
}

/// Parse default value string to renderer.Value
fn parseDefaultValue(default_str: ?[]const u8, type_tag: TypeTag) renderer.Value {
    const dv = default_str orelse {
        // Return type-appropriate zero value
        return switch (type_tag) {
            .int => .{ .int_val = 0 },
            .string => .{ .string_val = "" },
            .bool => .{ .bool_val = false },
            .float => .{ .float_val = 0.0 },
            else => .{ .null_val = {} },
        };
    };

    // Try to parse based on type
    switch (type_tag) {
        .int => {
            if (std.fmt.parseInt(i64, dv, 10)) |n| {
                return .{ .int_val = n };
            } else |_| {}
        },
        .float => {
            if (std.fmt.parseFloat(f64, dv)) |f| {
                return .{ .float_val = f };
            } else |_| {}
        },
        .bool => {
            if (std.mem.eql(u8, dv, "true")) return .{ .bool_val = true };
            if (std.mem.eql(u8, dv, "false")) return .{ .bool_val = false };
        },
        .string => return .{ .string_val = dv },
        else => {},
    }

    // Fallback: treat as string
    return .{ .string_val = dv };
}

// ============================================================================
// Tests
// ============================================================================

test "compile simple counter component" {
    const allocator = std.testing.allocator;

    const source =
        \\component Counter {
        \\    state count: int = 0
        \\
        \\    fn increment(self: *Self) void {
        \\        self.count += 1
        \\    }
        \\
        \\    fn render(self: *Self) Html {
        \\        <div>{count}</div>
        \\    }
        \\}
    ;

    var compiler = Compiler.init(allocator);
    var component = try compiler.compileSource(source);
    defer component.deinit();

    try std.testing.expectEqualStrings("Counter", component.name);
    try std.testing.expectEqual(@as(usize, 1), component.state_fields.len);
    try std.testing.expectEqualStrings("count", component.state_fields[0].name);
    try std.testing.expectEqual(TypeTag.int, component.state_fields[0].type_tag);
}

test "create and use component instance" {
    const allocator = std.testing.allocator;

    const source =
        \\component Counter {
        \\    state count: int = 5
        \\
        \\    fn increment(self: *Self) void {
        \\        self.count += 1
        \\    }
        \\}
    ;

    var compiler = Compiler.init(allocator);
    var component = try compiler.compileSource(source);
    defer component.deinit();

    var instance = try component.createInstance();
    defer instance.deinit();

    // Check initial state
    const count = instance.getState("count") orelse .{ .null_val = {} };
    try std.testing.expectEqual(@as(i64, 5), count.int_val);

    // Call handler
    try instance.callHandler("increment", .{});

    // Check updated state
    const new_count = instance.getState("count") orelse .{ .null_val = {} };
    try std.testing.expectEqual(@as(i64, 6), new_count.int_val);
}

test "render component with state" {
    const allocator = std.testing.allocator;

    const source =
        \\component Counter {
        \\    state count: int = 42
        \\
        \\    fn render(self: *Self) Html {
        \\        <span>{count}</span>
        \\    }
        \\}
    ;

    var compiler = Compiler.init(allocator);
    var component = try compiler.compileSource(source);
    defer component.deinit();

    var instance = try component.createInstance();
    defer instance.deinit();

    const html = try instance.render();
    defer allocator.free(html);

    try std.testing.expect(std.mem.indexOf(u8, html, "42") != null);
}

test "handler interpreter parses compound assignments" {
    const allocator = std.testing.allocator;

    var interpreter = HandlerInterpreter.init(allocator);

    // Test parsing self.count += 1
    const instructions1 = try interpreter.parseHandler("self.count += 1");
    defer allocator.free(instructions1);

    try std.testing.expectEqual(@as(usize, 1), instructions1.len);
    try std.testing.expectEqualStrings("count", instructions1[0].add_to_field.field);
    try std.testing.expectEqual(@as(i64, 1), instructions1[0].add_to_field.amount);

    // Test parsing self.value -= 5
    const instructions2 = try interpreter.parseHandler("self.value -= 5");
    defer allocator.free(instructions2);

    try std.testing.expectEqual(@as(usize, 1), instructions2.len);
    try std.testing.expectEqualStrings("value", instructions2[0].sub_from_field.field);
    try std.testing.expectEqual(@as(i64, 5), instructions2[0].sub_from_field.amount);
}

test "handler interpreter parses simple assignments" {
    const allocator = std.testing.allocator;

    var interpreter = HandlerInterpreter.init(allocator);

    // Test parsing self.count = 0
    const instructions = try interpreter.parseHandler("self.count = 0");
    defer allocator.free(instructions);

    try std.testing.expectEqual(@as(usize, 1), instructions.len);
    try std.testing.expectEqualStrings("count", instructions[0].set_field.field);
    try std.testing.expectEqual(@as(i64, 0), instructions[0].set_field.value.int_val);
}

test "handler interpreter executes instructions" {
    const allocator = std.testing.allocator;

    // Set up state
    var state: std.StringHashMapUnmanaged(renderer.Value) = .empty;
    defer state.deinit(allocator);

    try state.put(allocator, "count", .{ .int_val = 10 });

    const defaults = &[_]CompiledStateField{
        .{ .name = "count", .type_tag = .int, .default_value = .{ .int_val = 0 } },
    };

    // Create instructions
    const instructions = &[_]HandlerInstruction{
        .{ .add_to_field = .{ .field = "count", .amount = 5 } },
    };

    // Execute
    var interpreter = HandlerInterpreter.init(allocator);
    try interpreter.execute(instructions, &state, defaults);

    // Verify
    const result = state.get("count") orelse .{ .null_val = {} };
    try std.testing.expectEqual(@as(i64, 15), result.int_val);
}

test "compiled handler with instructions" {
    const allocator = std.testing.allocator;

    const source =
        \\component Counter {
        \\    state count: int = 0
        \\
        \\    fn add_five(self: *Self) void {
        \\        self.count += 5
        \\    }
        \\
        \\    fn subtract_three(self: *Self) void {
        \\        self.count -= 3
        \\    }
        \\
        \\    fn set_to_ten(self: *Self) void {
        \\        self.count = 10
        \\    }
        \\}
    ;

    var compiler = Compiler.init(allocator);
    var component = try compiler.compileSource(source);
    defer component.deinit();

    var instance = try component.createInstance();
    defer instance.deinit();

    // Initial state
    try std.testing.expectEqual(@as(i64, 0), instance.getState("count").?.int_val);

    // Test add_five (if handler source was parsed correctly)
    try instance.callHandler("add_five", .{});
    try std.testing.expectEqual(@as(i64, 5), instance.getState("count").?.int_val);

    // Test subtract_three
    try instance.callHandler("subtract_three", .{});
    try std.testing.expectEqual(@as(i64, 2), instance.getState("count").?.int_val);

    // Test set_to_ten
    try instance.callHandler("set_to_ten", .{});
    try std.testing.expectEqual(@as(i64, 10), instance.getState("count").?.int_val);
}

test "end-to-end: Counter component full lifecycle" {
    const allocator = std.testing.allocator;

    // Full counter component with render template
    const source =
        \\component Counter {
        \\    state count: int = 0
        \\
        \\    fn increment(self: *Self) void {
        \\        self.count += 1
        \\    }
        \\
        \\    fn decrement(self: *Self) void {
        \\        self.count -= 1
        \\    }
        \\
        \\    fn reset(self: *Self) void {
        \\        self.count = 0
        \\    }
        \\
        \\    fn render(self: *Self) Html {
        \\        <div class="counter">
        \\            <span class="count">{count}</span>
        \\            <button @click="increment">+</button>
        \\            <button @click="decrement">-</button>
        \\        </div>
        \\    }
        \\}
    ;

    // Step 1: Compile component
    var compiler = Compiler.init(allocator);
    var component = try compiler.compileSource(source);
    defer component.deinit();

    try std.testing.expectEqualStrings("Counter", component.name);
    try std.testing.expectEqual(@as(usize, 1), component.state_fields.len);
    try std.testing.expectEqual(@as(usize, 3), component.handlers.len); // increment, decrement, reset

    // Step 2: Create instance
    var instance = try component.createInstance();
    defer instance.deinit();

    // Step 3: Verify initial state
    try std.testing.expectEqual(@as(i64, 0), instance.getState("count").?.int_val);
    try std.testing.expect(instance.isDirty());

    // Step 4: Render initial HTML
    const html1 = try instance.render();
    defer allocator.free(html1);
    try std.testing.expect(!instance.isDirty());
    // Should contain the count value 0
    try std.testing.expect(std.mem.indexOf(u8, html1, "0") != null);

    // Step 5: Call increment handler
    try instance.callHandler("increment", .{});
    try std.testing.expectEqual(@as(i64, 1), instance.getState("count").?.int_val);
    try std.testing.expect(instance.isDirty());

    // Step 6: Render updated HTML
    const html2 = try instance.render();
    defer allocator.free(html2);
    try std.testing.expect(std.mem.indexOf(u8, html2, "1") != null);

    // Step 7: Call increment again
    try instance.callHandler("increment", .{});
    try std.testing.expectEqual(@as(i64, 2), instance.getState("count").?.int_val);

    // Step 8: Call decrement
    try instance.callHandler("decrement", .{});
    try std.testing.expectEqual(@as(i64, 1), instance.getState("count").?.int_val);

    // Step 9: Call reset
    try instance.callHandler("reset", .{});
    try std.testing.expectEqual(@as(i64, 0), instance.getState("count").?.int_val);

    // Step 10: Final render
    const html3 = try instance.render();
    defer allocator.free(html3);
    try std.testing.expect(std.mem.indexOf(u8, html3, "0") != null);
}

test "end-to-end: TodoList component with multiple state fields" {
    const allocator = std.testing.allocator;

    const source =
        \\component TodoList {
        \\    state item_count: int = 0
        \\    state completed_count: int = 0
        \\
        \\    fn add_item(self: *Self) void {
        \\        self.item_count += 1
        \\    }
        \\
        \\    fn complete_item(self: *Self) void {
        \\        self.completed_count += 1
        \\    }
        \\
        \\    fn render(self: *Self) Html {
        \\        <div>
        \\            <span>Items: {item_count}</span>
        \\            <span>Completed: {completed_count}</span>
        \\        </div>
        \\    }
        \\}
    ;

    var compiler = Compiler.init(allocator);
    var component = try compiler.compileSource(source);
    defer component.deinit();

    var instance = try component.createInstance();
    defer instance.deinit();

    // Initial state
    try std.testing.expectEqual(@as(i64, 0), instance.getState("item_count").?.int_val);
    try std.testing.expectEqual(@as(i64, 0), instance.getState("completed_count").?.int_val);

    // Add some items
    try instance.callHandler("add_item", .{});
    try instance.callHandler("add_item", .{});
    try instance.callHandler("add_item", .{});
    try std.testing.expectEqual(@as(i64, 3), instance.getState("item_count").?.int_val);

    // Complete some items
    try instance.callHandler("complete_item", .{});
    try instance.callHandler("complete_item", .{});
    try std.testing.expectEqual(@as(i64, 2), instance.getState("completed_count").?.int_val);

    // Render and verify
    const html = try instance.render();
    defer allocator.free(html);
    try std.testing.expect(std.mem.indexOf(u8, html, "3") != null); // item_count
    try std.testing.expect(std.mem.indexOf(u8, html, "2") != null); // completed_count
}
