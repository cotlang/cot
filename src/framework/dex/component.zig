//! Dex Component Runtime
//!
//! Manages component instances, state, props, and lifecycle.
//! This is the core of the Dex framework.

const std = @import("std");
const Allocator = std.mem.Allocator;
const template = @import("template/parser.zig");
const renderer = @import("template/renderer.zig");

/// Component state field definition
pub const StateField = struct {
    name: []const u8,
    default_value: renderer.Value,
};

/// Component prop definition
pub const PropDef = struct {
    name: []const u8,
    required: bool,
};

/// Component definition (metadata about a component type)
pub const ComponentDef = struct {
    name: []const u8,
    state_fields: []const StateField,
    props: []const PropDef,
    template: template.Node,

    /// Event handlers - maps event names to handler function names
    event_handlers: std.StringHashMapUnmanaged([]const u8),
};

/// Component instance (a live instance of a component)
pub const ComponentInstance = struct {
    id: u64,
    def: *const ComponentDef,
    state: renderer.Context,
    props: renderer.Context,
    subscriptions: std.ArrayListUnmanaged([]const u8),
    socket_id: ?u64,
    mounted: bool,
    dirty: bool,
    last_html: []const u8,
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, id: u64, def: *const ComponentDef) !Self {
        var state = renderer.Context.init(allocator);
        errdefer state.deinit();

        // Initialize state with defaults
        for (def.state_fields) |field| {
            try state.set(field.name, field.default_value);
        }

        return Self{
            .id = id,
            .def = def,
            .state = state,
            .props = renderer.Context.init(allocator),
            .subscriptions = .empty,
            .socket_id = null,
            .mounted = false,
            .dirty = true,
            .last_html = "",
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.state.deinit();
        self.props.deinit();
        self.subscriptions.deinit(self.allocator);
    }

    /// Set a prop value
    pub fn setProp(self: *Self, name: []const u8, value: renderer.Value) !void {
        try self.props.set(name, value);
        self.dirty = true;
    }

    /// Set a state value
    pub fn setState(self: *Self, name: []const u8, value: renderer.Value) !void {
        try self.state.set(name, value);
        self.dirty = true;
    }

    /// Get state value
    pub fn getState(self: *const Self, name: []const u8) ?renderer.Value {
        return self.state.get(name);
    }

    /// Get prop value
    pub fn getProp(self: *const Self, name: []const u8) ?renderer.Value {
        return self.props.get(name);
    }

    /// Subscribe to a pubsub topic
    pub fn subscribe(self: *Self, topic: []const u8) !void {
        try self.subscriptions.append(self.allocator, topic);
    }

    /// Render the component to HTML
    pub fn render(self: *Self) ![]const u8 {
        // Create combined context with props and state
        // State overrides props for 'self' access
        var ctx = self.props.child(self.allocator);
        defer ctx.deinit();

        // Copy state values into context
        var state_iter = self.state.values.iterator();
        while (state_iter.next()) |entry| {
            try ctx.set(entry.key_ptr.*, entry.value_ptr.*);
        }

        var rend = renderer.Renderer.init(self.allocator);
        defer rend.deinit();

        const html = try rend.render(self.def.template, &ctx);

        self.last_html = html;
        self.dirty = false;

        return html;
    }

    /// Check if component needs re-render
    pub fn isDirty(self: *const Self) bool {
        return self.dirty;
    }

    /// Mark component as needing re-render
    pub fn markDirty(self: *Self) void {
        self.dirty = true;
    }
};

/// Event data passed to handlers
pub const Event = struct {
    event_type: []const u8,
    target_id: ?[]const u8,
    value: ?[]const u8,
    params: std.StringHashMapUnmanaged([]const u8),
};

/// Component Registry - manages all component instances
pub const Registry = struct {
    instances: std.AutoHashMapUnmanaged(u64, ComponentInstance),
    definitions: std.StringHashMapUnmanaged(ComponentDef),
    next_id: u64,
    allocator: Allocator,

    // Socket to component mapping
    socket_components: std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(u64)),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .instances = .empty,
            .definitions = .empty,
            .next_id = 1,
            .allocator = allocator,
            .socket_components = .empty,
        };
    }

    pub fn deinit(self: *Self) void {
        // Clean up all instances
        var iter = self.instances.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.instances.deinit(self.allocator);
        self.definitions.deinit(self.allocator);

        // Clean up socket mappings
        var socket_iter = self.socket_components.iterator();
        while (socket_iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.socket_components.deinit(self.allocator);
    }

    /// Register a component definition
    pub fn registerComponent(self: *Self, def: ComponentDef) !void {
        try self.definitions.put(self.allocator, def.name, def);
    }

    /// Create a new component instance
    pub fn createInstance(self: *Self, component_name: []const u8, socket_id: ?u64) !*ComponentInstance {
        const def = self.definitions.getPtr(component_name) orelse return error.ComponentNotFound;

        const id = self.next_id;
        self.next_id += 1;

        var instance = try ComponentInstance.init(self.allocator, id, def);
        instance.socket_id = socket_id;

        try self.instances.put(self.allocator, id, instance);

        // Track socket association
        if (socket_id) |sid| {
            const result = try self.socket_components.getOrPut(self.allocator, sid);
            if (!result.found_existing) {
                result.value_ptr.* = .empty;
            }
            try result.value_ptr.append(self.allocator, id);
        }

        return self.instances.getPtr(id).?;
    }

    /// Get a component instance by ID
    pub fn getInstance(self: *Self, id: u64) ?*ComponentInstance {
        return self.instances.getPtr(id);
    }

    /// Get all component instances for a socket
    pub fn getSocketInstances(self: *Self, socket_id: u64) []const u64 {
        if (self.socket_components.get(socket_id)) |list| {
            return list.items;
        }
        return &.{};
    }

    /// Clean up all components for a disconnected socket
    pub fn cleanupSocket(self: *Self, socket_id: u64) void {
        if (self.socket_components.getPtr(socket_id)) |list| {
            for (list.items) |instance_id| {
                if (self.instances.getPtr(instance_id)) |instance| {
                    instance.deinit();
                    _ = self.instances.remove(instance_id);
                }
            }
            list.deinit(self.allocator);
            _ = self.socket_components.remove(socket_id);
        }
    }

    /// Get all dirty components that need re-render
    pub fn getDirtyInstances(self: *Self) ![]u64 {
        var dirty: std.ArrayListUnmanaged(u64) = .empty;
        errdefer dirty.deinit(self.allocator);

        var iter = self.instances.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.isDirty()) {
                try dirty.append(self.allocator, entry.key_ptr.*);
            }
        }

        return dirty.toOwnedSlice(self.allocator);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "component instance creation" {
    const allocator = std.testing.allocator;

    const lexer_mod = @import("template/lexer.zig");
    const parser_mod = @import("template/parser.zig");

    // Parse a simple template
    var lexer = lexer_mod.Lexer.init(allocator, "<div>{count}</div>");
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parser = parser_mod.Parser.init(allocator, tokens);
    defer parser.deinit();
    const tmpl = try parser.parse();

    // Create component definition
    const def = ComponentDef{
        .name = "Counter",
        .state_fields = &.{
            StateField{ .name = "count", .default_value = .{ .int_val = 0 } },
        },
        .props = &.{},
        .template = tmpl,
        .event_handlers = .empty,
    };

    // Create registry and register component
    var registry = Registry.init(allocator);
    defer registry.deinit();

    try registry.registerComponent(def);

    // Create instance
    var instance = try registry.createInstance("Counter", null);

    // Check initial state
    const count = instance.getState("count") orelse .{ .null_val = {} };
    try std.testing.expectEqual(@as(i64, 0), count.int_val);

    // Update state
    try instance.setState("count", .{ .int_val = 5 });
    try std.testing.expect(instance.isDirty());

    // Render
    _ = try instance.render();
    try std.testing.expect(!instance.isDirty());
}

test "component registry socket cleanup" {
    const allocator = std.testing.allocator;

    const lexer_mod = @import("template/lexer.zig");
    const parser_mod = @import("template/parser.zig");

    var lexer = lexer_mod.Lexer.init(allocator, "<p>test</p>");
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parser = parser_mod.Parser.init(allocator, tokens);
    defer parser.deinit();
    const tmpl = try parser.parse();

    var registry = Registry.init(allocator);
    defer registry.deinit();

    try registry.registerComponent(.{
        .name = "Test",
        .state_fields = &.{},
        .props = &.{},
        .template = tmpl,
        .event_handlers = .empty,
    });

    // Create instances for socket 1
    _ = try registry.createInstance("Test", 1);
    _ = try registry.createInstance("Test", 1);

    try std.testing.expectEqual(@as(usize, 2), registry.getSocketInstances(1).len);

    // Cleanup socket
    registry.cleanupSocket(1);

    try std.testing.expectEqual(@as(usize, 0), registry.getSocketInstances(1).len);
}
