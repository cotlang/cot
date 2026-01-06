//! Dex PubSub System
//!
//! Enables real-time multi-user features like chat, collaborative editing,
//! and live updates. Components can subscribe to topics and receive broadcasts.
//!
//! Usage in Cot:
//!   pub fn mount(self: *Self) void {
//!       self.subscribe("room:lobby");
//!   }
//!
//!   pub fn handle_info(self: *Self, event: []const u8, payload: Value) void {
//!       if (std.mem.eql(u8, event, "new_message")) {
//!           self.messages.append(payload);
//!       }
//!   }

const std = @import("std");
const Allocator = std.mem.Allocator;

/// A broadcast message
pub const Message = struct {
    topic: []const u8,
    event: []const u8,
    payload: []const u8,  // JSON-encoded payload
};

/// Subscriber callback function type
pub const SubscriberFn = *const fn (msg: Message) void;

/// Subscription entry
pub const Subscription = struct {
    topic: []const u8,
    component_id: u64,
};

/// PubSub broker
pub const Broker = struct {
    /// Topic -> list of component IDs
    subscriptions: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(u64)),
    /// Component ID -> list of topics (for cleanup)
    component_topics: std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged([]const u8)),
    /// Pending broadcasts (for async delivery)
    pending: std.ArrayListUnmanaged(Message),
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .subscriptions = .empty,
            .component_topics = .empty,
            .pending = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        // Clean up subscription lists
        var sub_iter = self.subscriptions.iterator();
        while (sub_iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.subscriptions.deinit(self.allocator);

        // Clean up component topic lists
        var comp_iter = self.component_topics.iterator();
        while (comp_iter.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.component_topics.deinit(self.allocator);

        self.pending.deinit(self.allocator);
    }

    /// Subscribe a component to a topic
    pub fn subscribe(self: *Self, topic: []const u8, component_id: u64) !void {
        // Add to topic subscribers
        const result = try self.subscriptions.getOrPut(self.allocator, topic);
        if (!result.found_existing) {
            result.value_ptr.* = .empty;
        }

        // Check if already subscribed
        for (result.value_ptr.items) |id| {
            if (id == component_id) return;
        }

        try result.value_ptr.append(self.allocator, component_id);

        // Track topic for component (for cleanup)
        const comp_result = try self.component_topics.getOrPut(self.allocator, component_id);
        if (!comp_result.found_existing) {
            comp_result.value_ptr.* = .empty;
        }
        try comp_result.value_ptr.append(self.allocator, topic);
    }

    /// Unsubscribe a component from a topic
    pub fn unsubscribe(self: *Self, topic: []const u8, component_id: u64) void {
        // Remove from topic subscribers
        if (self.subscriptions.getPtr(topic)) |list| {
            for (list.items, 0..) |id, i| {
                if (id == component_id) {
                    _ = list.swapRemove(i);
                    break;
                }
            }
        }

        // Remove topic from component tracking
        if (self.component_topics.getPtr(component_id)) |list| {
            for (list.items, 0..) |t, i| {
                if (std.mem.eql(u8, t, topic)) {
                    _ = list.swapRemove(i);
                    break;
                }
            }
        }
    }

    /// Unsubscribe a component from all topics (on disconnect)
    pub fn unsubscribeAll(self: *Self, component_id: u64) void {
        if (self.component_topics.get(component_id)) |topics| {
            for (topics.items) |topic| {
                if (self.subscriptions.getPtr(topic)) |list| {
                    for (list.items, 0..) |id, i| {
                        if (id == component_id) {
                            _ = list.swapRemove(i);
                            break;
                        }
                    }
                }
            }
        }

        if (self.component_topics.getPtr(component_id)) |list| {
            list.deinit(self.allocator);
            _ = self.component_topics.remove(component_id);
        }
    }

    /// Broadcast a message to all subscribers of a topic
    pub fn broadcast(self: *Self, topic: []const u8, event: []const u8, payload: []const u8) !void {
        try self.pending.append(self.allocator, Message{
            .topic = topic,
            .event = event,
            .payload = payload,
        });
    }

    /// Get subscribers for a topic
    pub fn getSubscribers(self: *const Self, topic: []const u8) []const u64 {
        if (self.subscriptions.get(topic)) |list| {
            return list.items;
        }
        return &.{};
    }

    /// Get and clear pending broadcasts
    pub fn takePending(self: *Self) ![]Message {
        const items = try self.pending.toOwnedSlice(self.allocator);
        return items;
    }

    /// Check if there are pending broadcasts
    pub fn hasPending(self: *const Self) bool {
        return self.pending.items.len > 0;
    }

    /// Check if a topic matches a pattern (supports wildcards)
    pub fn topicMatches(pattern: []const u8, topic: []const u8) bool {
        // Simple pattern matching:
        // "room:*" matches "room:123", "room:lobby"
        // "*" matches everything

        if (std.mem.eql(u8, pattern, "*")) return true;

        if (std.mem.endsWith(u8, pattern, ":*")) {
            const prefix = pattern[0 .. pattern.len - 1]; // Keep the ":"
            return std.mem.startsWith(u8, topic, prefix);
        }

        return std.mem.eql(u8, pattern, topic);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "pubsub subscribe and broadcast" {
    const allocator = std.testing.allocator;

    var broker = Broker.init(allocator);
    defer broker.deinit();

    // Subscribe
    try broker.subscribe("room:lobby", 1);
    try broker.subscribe("room:lobby", 2);

    // Check subscribers
    const subs = broker.getSubscribers("room:lobby");
    try std.testing.expectEqual(@as(usize, 2), subs.len);

    // Broadcast
    try broker.broadcast("room:lobby", "new_message", "{\"text\":\"Hello\"}");
    try std.testing.expect(broker.hasPending());

    // Take pending
    const pending = try broker.takePending();
    defer allocator.free(pending);

    try std.testing.expectEqual(@as(usize, 1), pending.len);
    try std.testing.expectEqualStrings("room:lobby", pending[0].topic);
    try std.testing.expectEqualStrings("new_message", pending[0].event);
}

test "pubsub unsubscribe" {
    const allocator = std.testing.allocator;

    var broker = Broker.init(allocator);
    defer broker.deinit();

    try broker.subscribe("room:lobby", 1);
    try broker.subscribe("room:lobby", 2);

    broker.unsubscribe("room:lobby", 1);

    const subs = broker.getSubscribers("room:lobby");
    try std.testing.expectEqual(@as(usize, 1), subs.len);
    try std.testing.expectEqual(@as(u64, 2), subs[0]);
}

test "pubsub unsubscribe all" {
    const allocator = std.testing.allocator;

    var broker = Broker.init(allocator);
    defer broker.deinit();

    try broker.subscribe("room:lobby", 1);
    try broker.subscribe("room:general", 1);

    broker.unsubscribeAll(1);

    try std.testing.expectEqual(@as(usize, 0), broker.getSubscribers("room:lobby").len);
    try std.testing.expectEqual(@as(usize, 0), broker.getSubscribers("room:general").len);
}

test "topic pattern matching" {
    try std.testing.expect(Broker.topicMatches("*", "anything"));
    try std.testing.expect(Broker.topicMatches("room:*", "room:123"));
    try std.testing.expect(Broker.topicMatches("room:*", "room:lobby"));
    try std.testing.expect(!Broker.topicMatches("room:*", "chat:lobby"));
    try std.testing.expect(Broker.topicMatches("room:lobby", "room:lobby"));
    try std.testing.expect(!Broker.topicMatches("room:lobby", "room:general"));
}
