//! Dex Live Handler
//!
//! Integrates Dex components with the HTTP router for live routes.
//! Handles initial page render, WebSocket upgrade, and real-time updates.
//!
//! Usage:
//!   const dex = @import("dex");
//!
//!   // Register live component route
//!   app.live("/counter", dex.Counter);
//!   app.live("/chat/:room_id", dex.ChatRoom);

const std = @import("std");
const Allocator = std.mem.Allocator;
const component = @import("component.zig");
const socket = @import("socket.zig");
const pubsub = @import("pubsub.zig");
const renderer = @import("template/renderer.zig");

/// HTML page template for live components
const PAGE_TEMPLATE =
    \\<!DOCTYPE html>
    \\<html>
    \\<head>
    \\    <meta charset="UTF-8">
    \\    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    \\    <meta name="dex-ws-url" content="{ws_url}">
    \\    <title>{title}</title>
    \\    {head}
    \\</head>
    \\<body>
    \\    <div id="dex-root" data-dex-id="{component_id}">
    \\        {content}
    \\    </div>
    \\    <script src="/assets/dex-runtime.js"></script>
    \\    <script src="https://unpkg.com/morphdom@2.7.0/dist/morphdom-umd.min.js"></script>
    \\</body>
    \\</html>
;

/// Live route definition
pub const LiveRoute = struct {
    pattern: []const u8,
    component_name: []const u8,
    layout: ?[]const u8,
};

/// Live handler context
pub const LiveContext = struct {
    registry: *component.Registry,
    socket_manager: *socket.Manager,
    broker: *pubsub.Broker,
    allocator: Allocator,

    const Self = @This();

    pub fn init(
        allocator: Allocator,
        registry: *component.Registry,
        socket_manager: *socket.Manager,
        broker: *pubsub.Broker,
    ) Self {
        return .{
            .registry = registry,
            .socket_manager = socket_manager,
            .broker = broker,
            .allocator = allocator,
        };
    }

    /// Handle initial HTTP request for a live route
    pub fn handleRequest(
        self: *Self,
        component_name: []const u8,
        params: std.StringHashMapUnmanaged([]const u8),
    ) ![]const u8 {
        // Create a temporary component instance for initial render
        const instance = try self.registry.createInstance(component_name, null);

        // Set props from route params
        var param_iter = params.iterator();
        while (param_iter.next()) |entry| {
            try instance.setProp(entry.key_ptr.*, .{ .string_val = entry.value_ptr.* });
        }

        // Render the component
        const content = try instance.render();

        // Build the full page
        return try self.buildPage(component_name, instance.id, content);
    }

    fn buildPage(self: *Self, title: []const u8, component_id: u64, content: []const u8) ![]const u8 {
        var result: std.ArrayListUnmanaged(u8) = .empty;
        errdefer result.deinit(self.allocator);

        // Parse template and substitute values
        var i: usize = 0;
        const template = PAGE_TEMPLATE;

        while (i < template.len) {
            if (template[i] == '{' and i + 1 < template.len) {
                // Find closing brace
                var j = i + 1;
                while (j < template.len and template[j] != '}') : (j += 1) {}

                if (j < template.len) {
                    const key = template[i + 1 .. j];

                    if (std.mem.eql(u8, key, "title")) {
                        try result.appendSlice(self.allocator, title);
                    } else if (std.mem.eql(u8, key, "ws_url")) {
                        try result.appendSlice(self.allocator, "/dex/websocket");
                    } else if (std.mem.eql(u8, key, "head")) {
                        // Additional head content (CSS, etc.)
                    } else if (std.mem.eql(u8, key, "component_id")) {
                        var buf: [32]u8 = undefined;
                        const id_str = std.fmt.bufPrint(&buf, "{d}", .{component_id}) catch "0";
                        try result.appendSlice(self.allocator, id_str);
                    } else if (std.mem.eql(u8, key, "content")) {
                        try result.appendSlice(self.allocator, content);
                    } else {
                        // Unknown key, skip
                    }

                    i = j + 1;
                    continue;
                }
            }

            try result.append(self.allocator, template[i]);
            i += 1;
        }

        return result.toOwnedSlice(self.allocator);
    }

    /// Handle WebSocket message
    pub fn handleWebSocket(self: *Self, socket_id: u64, message: []const u8) !?[]const u8 {
        return self.socket_manager.onMessage(socket_id, message);
    }

    /// Handle new WebSocket connection
    pub fn onWebSocketConnect(self: *Self) !u64 {
        return self.socket_manager.onConnect();
    }

    /// Handle WebSocket disconnect
    pub fn onWebSocketDisconnect(self: *Self, socket_id: u64) void {
        self.socket_manager.onDisconnect(socket_id);
    }

    /// Process pending broadcasts and return patches to send
    pub fn processBroadcasts(self: *Self) ![]PendingPatch {
        var patches: std.ArrayListUnmanaged(PendingPatch) = .empty;
        errdefer patches.deinit(self.allocator);

        if (!self.broker.hasPending()) {
            return patches.toOwnedSlice(self.allocator);
        }

        const messages = try self.broker.takePending();
        defer self.allocator.free(messages);

        for (messages) |msg| {
            // Get all subscribers for this topic
            const subscribers = self.broker.getSubscribers(msg.topic);

            for (subscribers) |comp_id| {
                if (self.registry.getInstance(comp_id)) |instance| {
                    // The component should handle the message and re-render
                    // For now, just mark it dirty
                    instance.markDirty();

                    // Re-render and generate patch
                    const old_html = instance.last_html;
                    const new_html = try instance.render();

                    if (!std.mem.eql(u8, old_html, new_html)) {
                        if (instance.socket_id) |sid| {
                            try patches.append(self.allocator, .{
                                .socket_id = sid,
                                .component_id = comp_id,
                                .html = new_html,
                            });
                        }
                    }
                }
            }
        }

        return patches.toOwnedSlice(self.allocator);
    }
};

/// Pending patch to send to a client
pub const PendingPatch = struct {
    socket_id: u64,
    component_id: u64,
    html: []const u8,

    pub fn toJson(self: *const PendingPatch, allocator: Allocator) ![]const u8 {
        // Escape HTML for JSON
        var escaped: std.ArrayListUnmanaged(u8) = .empty;
        defer escaped.deinit(allocator);

        for (self.html) |c| {
            switch (c) {
                '"' => try escaped.appendSlice(allocator, "\\\""),
                '\\' => try escaped.appendSlice(allocator, "\\\\"),
                '\n' => try escaped.appendSlice(allocator, "\\n"),
                '\r' => try escaped.appendSlice(allocator, "\\r"),
                '\t' => try escaped.appendSlice(allocator, "\\t"),
                else => try escaped.append(allocator, c),
            }
        }

        return std.fmt.allocPrint(
            allocator,
            "{{\"type\":\"patch\",\"component_id\":\"{d}\",\"html\":\"{s}\"}}",
            .{ self.component_id, escaped.items },
        );
    }
};

// ============================================================================
// Tests
// ============================================================================

test "live handler page generation" {
    const allocator = std.testing.allocator;

    const template_lexer = @import("template/lexer.zig");
    const template_parser = @import("template/parser.zig");

    var lexer = template_lexer.Lexer.init(allocator, "<div>Hello</div>");
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parser = template_parser.Parser.init(allocator, tokens);
    defer parser.deinit();
    const tmpl = try parser.parse();

    var registry = component.Registry.init(allocator);
    defer registry.deinit();

    try registry.registerComponent(.{
        .name = "Test",
        .state_fields = &.{},
        .props = &.{},
        .template = tmpl,
        .event_handlers = .empty,
    });

    var broker = pubsub.Broker.init(allocator);
    defer broker.deinit();

    var socket_mgr = socket.Manager.init(allocator, &registry);
    defer socket_mgr.deinit();

    var ctx = LiveContext.init(allocator, &registry, &socket_mgr, &broker);

    var params: std.StringHashMapUnmanaged([]const u8) = .empty;
    defer params.deinit(allocator);

    const page = try ctx.handleRequest("Test", params);
    defer allocator.free(page);

    // Verify page contains expected elements
    try std.testing.expect(std.mem.indexOf(u8, page, "<!DOCTYPE html>") != null);
    try std.testing.expect(std.mem.indexOf(u8, page, "data-dex-id=") != null);
    try std.testing.expect(std.mem.indexOf(u8, page, "<div>Hello</div>") != null);
    try std.testing.expect(std.mem.indexOf(u8, page, "dex-runtime.js") != null);
}
