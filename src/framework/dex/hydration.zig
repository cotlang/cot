//! Dex Hydration System
//!
//! Handles serializing component state for client-side hydration.
//! When the page loads, the client JavaScript reads embedded state
//! and "hydrates" the DOM - attaching event listeners and syncing
//! with the server via WebSocket.
//!
//! Flow:
//! 1. Server renders component to HTML
//! 2. Server serializes state to JSON and embeds in page
//! 3. Client receives HTML, displays immediately
//! 4. Client JavaScript reads embedded state
//! 5. Client connects WebSocket and syncs with server
//! 6. Component becomes interactive

const std = @import("std");
const Allocator = std.mem.Allocator;
const renderer = @import("template/renderer.zig");
const component = @import("component.zig");

/// Hydration data for a single component
pub const ComponentHydrationData = struct {
    /// Component instance ID
    id: u64,
    /// Component type name (not owned)
    name: []const u8,
    /// Serialized state (owned if allocated)
    state: []const u8,
    /// Serialized props (owned if allocated)
    props: []const u8,
    /// PubSub subscriptions (not owned)
    subscriptions: []const []const u8,
    /// Whether state/props are owned and should be freed
    owns_strings: bool,
};

/// Hydration data for a page
pub const PageHydrationData = struct {
    /// WebSocket URL for reconnection
    ws_url: []const u8,
    /// All components on the page
    components: std.ArrayListUnmanaged(ComponentHydrationData),
    /// CSRF token (if applicable)
    csrf_token: ?[]const u8,
    allocator: Allocator,

    pub fn init(allocator: Allocator, ws_url: []const u8) PageHydrationData {
        return .{
            .ws_url = ws_url,
            .components = .empty,
            .csrf_token = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *PageHydrationData) void {
        for (self.components.items) |comp| {
            if (comp.owns_strings) {
                self.allocator.free(comp.state);
                self.allocator.free(comp.props);
            }
        }
        self.components.deinit(self.allocator);
    }

    pub fn addComponent(self: *PageHydrationData, data: ComponentHydrationData) !void {
        try self.components.append(self.allocator, data);
    }

    /// Generate the hydration script to embed in HTML
    pub fn toScript(self: *const PageHydrationData) ![]const u8 {
        var script: std.ArrayListUnmanaged(u8) = .empty;
        errdefer script.deinit(self.allocator);

        try script.appendSlice(self.allocator, "<script>\n");
        try script.appendSlice(self.allocator, "window.__DEX_HYDRATION__ = ");

        // Build JSON
        try script.append(self.allocator, '{');

        // WebSocket URL
        try script.appendSlice(self.allocator, "\"wsUrl\":\"");
        try script.appendSlice(self.allocator, self.ws_url);
        try script.appendSlice(self.allocator, "\",");

        // CSRF token
        if (self.csrf_token) |token| {
            try script.appendSlice(self.allocator, "\"csrfToken\":\"");
            try script.appendSlice(self.allocator, token);
            try script.appendSlice(self.allocator, "\",");
        }

        // Components array
        try script.appendSlice(self.allocator, "\"components\":[");

        for (self.components.items, 0..) |comp, i| {
            if (i > 0) try script.append(self.allocator, ',');

            try script.append(self.allocator, '{');

            // ID
            try script.appendSlice(self.allocator, "\"id\":");
            var id_buf: [20]u8 = undefined;
            const id_str = std.fmt.bufPrint(&id_buf, "{d}", .{comp.id}) catch "0";
            try script.appendSlice(self.allocator, id_str);
            try script.append(self.allocator, ',');

            // Name
            try script.appendSlice(self.allocator, "\"name\":\"");
            try script.appendSlice(self.allocator, comp.name);
            try script.appendSlice(self.allocator, "\",");

            // State (already JSON)
            try script.appendSlice(self.allocator, "\"state\":");
            try script.appendSlice(self.allocator, comp.state);
            try script.append(self.allocator, ',');

            // Props (already JSON)
            try script.appendSlice(self.allocator, "\"props\":");
            try script.appendSlice(self.allocator, comp.props);
            try script.append(self.allocator, ',');

            // Subscriptions
            try script.appendSlice(self.allocator, "\"subscriptions\":[");
            for (comp.subscriptions, 0..) |sub, j| {
                if (j > 0) try script.append(self.allocator, ',');
                try script.append(self.allocator, '"');
                try script.appendSlice(self.allocator, sub);
                try script.append(self.allocator, '"');
            }
            try script.appendSlice(self.allocator, "]");

            try script.append(self.allocator, '}');
        }

        try script.appendSlice(self.allocator, "]");
        try script.append(self.allocator, '}');
        try script.appendSlice(self.allocator, ";\n");

        // Auto-initialize
        try script.appendSlice(self.allocator,
            \\if (window.Dex) {
            \\  window.dex = window.Dex.hydrate(window.__DEX_HYDRATION__);
            \\}
            \\
        );
        try script.appendSlice(self.allocator, "</script>\n");

        return script.toOwnedSlice(self.allocator);
    }
};

/// Serialize a Value to JSON
pub fn serializeValue(allocator: Allocator, value: renderer.Value) ![]const u8 {
    var json: std.ArrayListUnmanaged(u8) = .empty;
    errdefer json.deinit(allocator);

    try writeValue(allocator, &json, value);

    return json.toOwnedSlice(allocator);
}

fn writeValue(allocator: Allocator, json: *std.ArrayListUnmanaged(u8), value: renderer.Value) !void {
    switch (value) {
        .null_val => try json.appendSlice(allocator, "null"),
        .bool_val => |b| try json.appendSlice(allocator, if (b) "true" else "false"),
        .int_val => |i| {
            var buf: [24]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{i}) catch "0";
            try json.appendSlice(allocator, str);
        },
        .float_val => |f| {
            var buf: [32]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{f}) catch "0";
            try json.appendSlice(allocator, str);
        },
        .string_val => |s| {
            try json.append(allocator, '"');
            try writeEscapedString(allocator, json, s);
            try json.append(allocator, '"');
        },
        .array_val => |arr| {
            try json.append(allocator, '[');
            for (arr, 0..) |item, i| {
                if (i > 0) try json.append(allocator, ',');
                try writeValue(allocator, json, item);
            }
            try json.append(allocator, ']');
        },
        .object_val => |obj| {
            try json.append(allocator, '{');
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) try json.append(allocator, ',');
                first = false;
                try json.append(allocator, '"');
                try json.appendSlice(allocator, entry.key_ptr.*);
                try json.appendSlice(allocator, "\":");
                try writeValue(allocator, json, entry.value_ptr.*);
            }
            try json.append(allocator, '}');
        },
    }
}

fn writeEscapedString(allocator: Allocator, json: *std.ArrayListUnmanaged(u8), s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try json.appendSlice(allocator, "\\\""),
            '\\' => try json.appendSlice(allocator, "\\\\"),
            '\n' => try json.appendSlice(allocator, "\\n"),
            '\r' => try json.appendSlice(allocator, "\\r"),
            '\t' => try json.appendSlice(allocator, "\\t"),
            else => {
                if (c < 0x20) {
                    // Control character - escape as unicode
                    var buf: [6]u8 = undefined;
                    const str = std.fmt.bufPrint(&buf, "\\u{x:0>4}", .{c}) catch continue;
                    try json.appendSlice(allocator, str);
                } else {
                    try json.append(allocator, c);
                }
            },
        }
    }
}

/// Serialize a Context (state or props) to JSON
pub fn serializeContext(allocator: Allocator, ctx: *const renderer.Context) ![]const u8 {
    var json: std.ArrayListUnmanaged(u8) = .empty;
    errdefer json.deinit(allocator);

    try json.append(allocator, '{');

    var first = true;
    var iter = ctx.values.iterator();
    while (iter.next()) |entry| {
        if (!first) try json.append(allocator, ',');
        first = false;

        try json.append(allocator, '"');
        try json.appendSlice(allocator, entry.key_ptr.*);
        try json.appendSlice(allocator, "\":");
        try writeValue(allocator, &json, entry.value_ptr.*);
    }

    try json.append(allocator, '}');
    return json.toOwnedSlice(allocator);
}

/// Create hydration data from a component instance
pub fn fromComponent(allocator: Allocator, instance: *const component.ComponentInstance) !ComponentHydrationData {
    const state_json = try serializeContext(allocator, &instance.state);
    errdefer allocator.free(state_json);

    const props_json = try serializeContext(allocator, &instance.props);
    errdefer allocator.free(props_json);

    return ComponentHydrationData{
        .id = instance.id,
        .name = instance.def.name,
        .state = state_json,
        .props = props_json,
        .subscriptions = instance.subscriptions.items,
        .owns_strings = true,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "serialize null value" {
    const allocator = std.testing.allocator;

    const json = try serializeValue(allocator, .{ .null_val = {} });
    defer allocator.free(json);

    try std.testing.expectEqualStrings("null", json);
}

test "serialize int value" {
    const allocator = std.testing.allocator;

    const json = try serializeValue(allocator, .{ .int_val = 42 });
    defer allocator.free(json);

    try std.testing.expectEqualStrings("42", json);
}

test "serialize string value" {
    const allocator = std.testing.allocator;

    const json = try serializeValue(allocator, .{ .string_val = "hello" });
    defer allocator.free(json);

    try std.testing.expectEqualStrings("\"hello\"", json);
}

test "serialize string with escapes" {
    const allocator = std.testing.allocator;

    const json = try serializeValue(allocator, .{ .string_val = "hello\nworld" });
    defer allocator.free(json);

    try std.testing.expectEqualStrings("\"hello\\nworld\"", json);
}

test "page hydration data to script" {
    const allocator = std.testing.allocator;

    var page = PageHydrationData.init(allocator, "/dex/websocket");
    defer page.deinit();

    try page.addComponent(.{
        .id = 1,
        .name = "Counter",
        .state = "{\"count\":0}",
        .props = "{}",
        .subscriptions = &.{},
        .owns_strings = false, // String literals, don't free
    });

    const script = try page.toScript();
    defer allocator.free(script);

    try std.testing.expect(std.mem.indexOf(u8, script, "__DEX_HYDRATION__") != null);
    try std.testing.expect(std.mem.indexOf(u8, script, "Counter") != null);
    try std.testing.expect(std.mem.indexOf(u8, script, "/dex/websocket") != null);
}
