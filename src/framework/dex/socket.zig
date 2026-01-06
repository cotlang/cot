//! Dex Socket Manager
//!
//! Manages WebSocket connections and routes events to components.
//! This is the bridge between the network layer and the component runtime.

const std = @import("std");
const Allocator = std.mem.Allocator;
const component = @import("component.zig");
const diff = @import("diff.zig");
const renderer = @import("template/renderer.zig");
const presence = @import("presence.zig");

/// Message types from client
pub const ClientMessageType = enum {
    join, // Join a component
    event, // User interaction event
    ping, // Heartbeat
    leave, // Leave a component
    presence_track, // Track presence in topic
    presence_untrack, // Untrack presence from topic
};

/// Parsed client message
pub const ClientMessage = struct {
    id: u64,
    msg_type: ClientMessageType,
    component_id: ?[]const u8,
    event_name: ?[]const u8,
    event_type: ?[]const u8,
    target_id: ?[]const u8,
    value: ?[]const u8,
    form_data: ?std.StringHashMapUnmanaged([]const u8),
    // Presence fields
    topic: ?[]const u8,
    presence_key: ?[]const u8,
    presence_meta: ?[]const u8, // JSON-encoded metadata
};

/// Socket session - represents a connected client
pub const Session = struct {
    socket_id: u64,
    components: std.ArrayListUnmanaged(u64), // Component instance IDs
    presence_topics: std.ArrayListUnmanaged([]const u8), // Topics this session tracks presence in
    presence_key: ?[]const u8, // Unique key for this session's presence
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator, socket_id: u64) Self {
        return .{
            .socket_id = socket_id,
            .components = .empty,
            .presence_topics = .empty,
            .presence_key = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.components.deinit(self.allocator);
        self.presence_topics.deinit(self.allocator);
        if (self.presence_key) |key| {
            self.allocator.free(key);
        }
    }

    pub fn addComponent(self: *Self, instance_id: u64) !void {
        try self.components.append(self.allocator, instance_id);
    }

    pub fn addPresenceTopic(self: *Self, topic: []const u8) !void {
        // Check if already tracking
        for (self.presence_topics.items) |t| {
            if (std.mem.eql(u8, t, topic)) return;
        }
        try self.presence_topics.append(self.allocator, topic);
    }

    pub fn removePresenceTopic(self: *Self, topic: []const u8) void {
        for (self.presence_topics.items, 0..) |t, i| {
            if (std.mem.eql(u8, t, topic)) {
                _ = self.presence_topics.swapRemove(i);
                return;
            }
        }
    }
};

/// Socket Manager - handles all WebSocket connections
pub const Manager = struct {
    sessions: std.AutoHashMapUnmanaged(u64, Session),
    registry: *component.Registry,
    presence_state: presence.PresenceState,
    allocator: Allocator,
    next_socket_id: u64,

    const Self = @This();

    pub fn init(allocator: Allocator, registry: *component.Registry) Self {
        return .{
            .sessions = .empty,
            .registry = registry,
            .presence_state = presence.PresenceState.init(allocator),
            .allocator = allocator,
            .next_socket_id = 1,
        };
    }

    pub fn deinit(self: *Self) void {
        var iter = self.sessions.iterator();
        while (iter.next()) |entry| {
            entry.value_ptr.deinit();
        }
        self.sessions.deinit(self.allocator);
        self.presence_state.deinit();
    }

    /// Handle new WebSocket connection
    pub fn onConnect(self: *Self) !u64 {
        const socket_id = self.next_socket_id;
        self.next_socket_id += 1;

        try self.sessions.put(self.allocator, socket_id, Session.init(self.allocator, socket_id));

        return socket_id;
    }

    /// Handle WebSocket disconnect
    pub fn onDisconnect(self: *Self, socket_id: u64) void {
        // Clean up all components for this socket
        self.registry.cleanupSocket(socket_id);

        // Clean up presence for this session
        if (self.sessions.getPtr(socket_id)) |session| {
            if (session.presence_key) |key| {
                // Untrack from all topics
                for (session.presence_topics.items) |topic| {
                    self.presence_state.untrack(topic, key);
                }
            }

            session.deinit();
            _ = self.sessions.remove(socket_id);
        }
    }

    /// Handle incoming message from client
    pub fn onMessage(self: *Self, socket_id: u64, raw_message: []const u8) !?[]const u8 {
        const msg = try parseClientMessage(self.allocator, raw_message);

        return switch (msg.msg_type) {
            .join => self.handleJoin(socket_id, msg),
            .event => self.handleEvent(socket_id, msg),
            .ping => self.handlePing(msg),
            .leave => self.handleLeave(socket_id, msg),
            .presence_track => self.handlePresenceTrack(socket_id, msg),
            .presence_untrack => self.handlePresenceUntrack(socket_id, msg),
        };
    }

    fn handleJoin(self: *Self, socket_id: u64, msg: ClientMessage) !?[]const u8 {
        const component_name = msg.component_id orelse return null;

        // Create component instance
        const instance = self.registry.createInstance(component_name, socket_id) catch |err| {
            return try formatError(self.allocator, msg.id, err);
        };

        // Add to session
        if (self.sessions.getPtr(socket_id)) |session| {
            try session.addComponent(instance.id);
        }

        // Initial render
        const html = try instance.render();

        // Send initial HTML
        return try formatJoinResponse(self.allocator, msg.id, instance.id, html);
    }

    fn handleEvent(self: *Self, socket_id: u64, msg: ClientMessage) !?[]const u8 {
        _ = socket_id;

        // Parse component ID from message
        const comp_id_str = msg.component_id orelse return null;
        const comp_id = std.fmt.parseInt(u64, comp_id_str, 10) catch return null;

        const instance = self.registry.getInstance(comp_id) orelse return null;

        // Get the event handler name
        const event_name = msg.event_name orelse return null;

        // Look up handler in component definition
        if (instance.def.event_handlers.get(event_name)) |handler_name| {
            // For now, we'll use a simple approach:
            // The handler name maps to a state update pattern
            // e.g., "increment" -> increment the "count" state

            // This is where you'd integrate with the Cot VM to execute
            // the actual handler function. For now, we'll demonstrate
            // with a simple pattern-based approach.
            _ = handler_name;
        }

        // Update state based on event value
        if (msg.value) |value| {
            // If there's a value, treat event_name as a state field to update
            if (std.fmt.parseInt(i64, value, 10)) |int_val| {
                try instance.setState(event_name, .{ .int_val = int_val });
            } else |_| {
                try instance.setState(event_name, .{ .string_val = value });
            }
        }

        // Re-render and diff
        const old_html = instance.last_html;
        const new_html = try instance.render();

        var differ = diff.Differ.init(self.allocator);
        defer differ.deinit();

        const patch_set = try differ.diff(comp_id_str, old_html, new_html);

        if (differ.hasChanges()) {
            return try patch_set.toJson(self.allocator);
        }

        return null;
    }

    fn handlePing(self: *Self, msg: ClientMessage) !?[]const u8 {
        return try std.fmt.allocPrint(self.allocator, "{{\"ref\":{d},\"type\":\"pong\"}}", .{msg.id});
    }

    fn handleLeave(self: *Self, socket_id: u64, msg: ClientMessage) !?[]const u8 {
        const comp_id_str = msg.component_id orelse return null;
        const comp_id = std.fmt.parseInt(u64, comp_id_str, 10) catch return null;

        // Remove from session
        if (self.sessions.getPtr(socket_id)) |session| {
            for (session.components.items, 0..) |id, i| {
                if (id == comp_id) {
                    _ = session.components.swapRemove(i);
                    break;
                }
            }
        }

        // Clean up component
        if (self.registry.getInstance(comp_id)) |instance| {
            instance.deinit();
            _ = self.registry.instances.remove(comp_id);
        }

        return try std.fmt.allocPrint(self.allocator, "{{\"ref\":{d},\"type\":\"left\"}}", .{msg.id});
    }

    fn handlePresenceTrack(self: *Self, socket_id: u64, msg: ClientMessage) !?[]const u8 {
        const topic = msg.topic orelse return null;
        const key = msg.presence_key orelse {
            // Generate key from socket_id if not provided
            var buf: [32]u8 = undefined;
            const generated = std.fmt.bufPrint(&buf, "socket_{d}", .{socket_id}) catch return null;

            // Track presence
            try self.presence_state.track(topic, generated, &.{});

            // Update session
            if (self.sessions.getPtr(socket_id)) |session| {
                if (session.presence_key == null) {
                    session.presence_key = try self.allocator.dupe(u8, generated);
                }
                try session.addPresenceTopic(topic);
            }

            return try std.fmt.allocPrint(self.allocator, "{{\"ref\":{d},\"type\":\"presence_tracked\",\"topic\":\"{s}\"}}", .{ msg.id, topic });
        };

        // Track presence with provided key
        try self.presence_state.track(topic, key, &.{});

        // Update session
        if (self.sessions.getPtr(socket_id)) |session| {
            if (session.presence_key == null) {
                session.presence_key = try self.allocator.dupe(u8, key);
            }
            try session.addPresenceTopic(topic);
        }

        return try std.fmt.allocPrint(self.allocator, "{{\"ref\":{d},\"type\":\"presence_tracked\",\"topic\":\"{s}\"}}", .{ msg.id, topic });
    }

    fn handlePresenceUntrack(self: *Self, socket_id: u64, msg: ClientMessage) !?[]const u8 {
        const topic = msg.topic orelse return null;

        if (self.sessions.getPtr(socket_id)) |session| {
            if (session.presence_key) |key| {
                self.presence_state.untrack(topic, key);
                session.removePresenceTopic(topic);
            }
        }

        return try std.fmt.allocPrint(self.allocator, "{{\"ref\":{d},\"type\":\"presence_untracked\",\"topic\":\"{s}\"}}", .{ msg.id, topic });
    }

    /// Get pending presence diffs to broadcast
    pub fn getPresenceDiffs(self: *Self) ![]presence.PresenceState.TopicDiff {
        return self.presence_state.takeDiffs();
    }

    /// Check if there are pending presence diffs
    pub fn hasPresenceDiffs(self: *const Self) bool {
        return self.presence_state.hasPendingDiffs();
    }

    /// Get presence count for a topic
    pub fn getPresenceCount(self: *const Self, topic: []const u8) usize {
        return self.presence_state.count(topic);
    }
};

/// Parse a client message from JSON
fn parseClientMessage(allocator: Allocator, raw: []const u8) !ClientMessage {
    _ = allocator;

    // Simple JSON parsing - in production use a proper JSON parser
    var msg = ClientMessage{
        .id = 0,
        .msg_type = .ping,
        .component_id = null,
        .event_name = null,
        .event_type = null,
        .target_id = null,
        .value = null,
        .form_data = null,
        .topic = null,
        .presence_key = null,
        .presence_meta = null,
    };

    // Extract message ID
    if (std.mem.indexOf(u8, raw, "\"id\":")) |pos| {
        const start = pos + 5;
        var end = start;
        while (end < raw.len and (raw[end] >= '0' and raw[end] <= '9')) : (end += 1) {}
        if (end > start) {
            msg.id = std.fmt.parseInt(u64, raw[start..end], 10) catch 0;
        }
    }

    // Extract type
    if (std.mem.indexOf(u8, raw, "\"type\":\"")) |pos| {
        const start = pos + 8;
        if (std.mem.indexOfPos(u8, raw, start, "\"")) |end| {
            const type_str = raw[start..end];
            if (std.mem.eql(u8, type_str, "join")) {
                msg.msg_type = .join;
            } else if (std.mem.eql(u8, type_str, "event")) {
                msg.msg_type = .event;
            } else if (std.mem.eql(u8, type_str, "ping")) {
                msg.msg_type = .ping;
            } else if (std.mem.eql(u8, type_str, "leave")) {
                msg.msg_type = .leave;
            } else if (std.mem.eql(u8, type_str, "presence:track")) {
                msg.msg_type = .presence_track;
            } else if (std.mem.eql(u8, type_str, "presence:untrack")) {
                msg.msg_type = .presence_untrack;
            }
        }
    }

    // Extract component_id
    if (std.mem.indexOf(u8, raw, "\"component_id\":\"")) |pos| {
        const start = pos + 16;
        if (std.mem.indexOfPos(u8, raw, start, "\"")) |end| {
            msg.component_id = raw[start..end];
        }
    }

    // Extract event
    if (std.mem.indexOf(u8, raw, "\"event\":\"")) |pos| {
        const start = pos + 9;
        if (std.mem.indexOfPos(u8, raw, start, "\"")) |end| {
            msg.event_name = raw[start..end];
        }
    }

    // Extract value
    if (std.mem.indexOf(u8, raw, "\"value\":\"")) |pos| {
        const start = pos + 9;
        if (std.mem.indexOfPos(u8, raw, start, "\"")) |end| {
            msg.value = raw[start..end];
        }
    }

    // Extract topic (for presence)
    if (std.mem.indexOf(u8, raw, "\"topic\":\"")) |pos| {
        const start = pos + 9;
        if (std.mem.indexOfPos(u8, raw, start, "\"")) |end| {
            msg.topic = raw[start..end];
        }
    }

    // Extract key (for presence)
    if (std.mem.indexOf(u8, raw, "\"key\":\"")) |pos| {
        const start = pos + 7;
        if (std.mem.indexOfPos(u8, raw, start, "\"")) |end| {
            msg.presence_key = raw[start..end];
        }
    }

    return msg;
}

fn formatError(allocator: Allocator, ref: u64, err: anyerror) ![]const u8 {
    return try std.fmt.allocPrint(allocator, "{{\"ref\":{d},\"type\":\"error\",\"message\":\"{s}\"}}", .{ ref, @errorName(err) });
}

fn formatJoinResponse(allocator: Allocator, ref: u64, instance_id: u64, html: []const u8) ![]const u8 {
    // Need to escape HTML for JSON
    var escaped: std.ArrayListUnmanaged(u8) = .empty;
    defer escaped.deinit(allocator);

    for (html) |c| {
        switch (c) {
            '"' => try escaped.appendSlice(allocator, "\\\""),
            '\\' => try escaped.appendSlice(allocator, "\\\\"),
            '\n' => try escaped.appendSlice(allocator, "\\n"),
            '\r' => try escaped.appendSlice(allocator, "\\r"),
            '\t' => try escaped.appendSlice(allocator, "\\t"),
            else => try escaped.append(allocator, c),
        }
    }

    return try std.fmt.allocPrint(
        allocator,
        "{{\"ref\":{d},\"type\":\"joined\",\"instance_id\":{d},\"html\":\"{s}\"}}",
        .{ ref, instance_id, escaped.items },
    );
}

// ============================================================================
// Tests
// ============================================================================

test "socket manager basic flow" {
    const allocator = std.testing.allocator;

    const lexer_mod = @import("template/lexer.zig");
    const parser_mod = @import("template/parser.zig");

    var lexer = lexer_mod.Lexer.init(allocator, "<div>Test</div>");
    defer lexer.deinit();
    const tokens = try lexer.tokenize();

    var parser = parser_mod.Parser.init(allocator, tokens);
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

    var manager = Manager.init(allocator, &registry);
    defer manager.deinit();

    // Connect
    const socket_id = try manager.onConnect();
    try std.testing.expectEqual(@as(u64, 1), socket_id);

    // Ping
    const pong = try manager.onMessage(socket_id, "{\"id\":1,\"type\":\"ping\"}");
    try std.testing.expect(pong != null);
    if (pong) |p| allocator.free(p);

    // Disconnect
    manager.onDisconnect(socket_id);
}
