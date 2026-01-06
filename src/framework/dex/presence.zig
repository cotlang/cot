//! Dex Presence System
//!
//! Tracks who is online in each topic, similar to Phoenix Presence.
//! Enables features like "5 users viewing", typing indicators, and cursor sharing.
//!
//! Usage:
//!   // In component mount
//!   self.presence.track("room:lobby", user_id, .{
//!       .name = "Alice",
//!       .status = "online",
//!   });
//!
//!   // List who's online
//!   const users = self.presence.list("room:lobby");
//!
//!   // Presence changes trigger events
//!   fn handle_presence_diff(self: *Self, joins: []Presence, leaves: []Presence) void {
//!       // Update UI with who joined/left
//!   }

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Metadata associated with a presence entry
pub const Metadata = struct {
    /// Custom key-value metadata (JSON-like)
    data: std.StringHashMapUnmanaged([]const u8),

    pub fn init() Metadata {
        return .{ .data = .empty };
    }

    pub fn deinit(self: *Metadata, allocator: Allocator) void {
        self.data.deinit(allocator);
    }

    pub fn get(self: *const Metadata, key: []const u8) ?[]const u8 {
        return self.data.get(key);
    }

    pub fn toJson(self: *const Metadata, allocator: Allocator) ![]const u8 {
        var json: std.ArrayListUnmanaged(u8) = .empty;
        errdefer json.deinit(allocator);

        try json.append(allocator, '{');

        var first = true;
        var iter = self.data.iterator();
        while (iter.next()) |entry| {
            if (!first) try json.appendSlice(allocator, ",");
            first = false;
            try json.append(allocator, '"');
            try json.appendSlice(allocator, entry.key_ptr.*);
            try json.appendSlice(allocator, "\":\"");
            try json.appendSlice(allocator, entry.value_ptr.*);
            try json.append(allocator, '"');
        }

        try json.append(allocator, '}');
        return json.toOwnedSlice(allocator);
    }
};

/// A single presence entry
pub const Presence = struct {
    /// Unique identifier for this presence (NOT owned - reference to hash map key)
    key: []const u8,
    /// Topic this presence is in (NOT owned - reference to topic map key)
    topic: []const u8,
    /// Associated metadata (owned)
    meta: Metadata,
    /// When this presence was created
    joined_at: i64,
    /// Last seen (for timeout detection)
    last_seen: i64,
    /// Reference count (same user can be in multiple tabs)
    refs: u32,

    pub fn deinit(self: *Presence, allocator: Allocator) void {
        // key and topic are not owned - they're hash map keys
        self.meta.deinit(allocator);
    }
};

/// Presence diff - changes to broadcast
pub const PresenceDiff = struct {
    joins: std.ArrayListUnmanaged(PresenceRef),
    leaves: std.ArrayListUnmanaged(PresenceRef),
    allocator: Allocator,

    /// Reference to a presence (doesn't own the data)
    pub const PresenceRef = struct {
        key: []const u8,
        topic: []const u8,
        meta: *const Metadata,
    };

    pub fn init(allocator: Allocator) PresenceDiff {
        return .{
            .joins = .empty,
            .leaves = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *PresenceDiff) void {
        self.joins.deinit(self.allocator);
        self.leaves.deinit(self.allocator);
    }

    pub fn hasChanges(self: *const PresenceDiff) bool {
        return self.joins.items.len > 0 or self.leaves.items.len > 0;
    }

    pub fn toJson(self: *const PresenceDiff, allocator: Allocator) ![]const u8 {
        var json: std.ArrayListUnmanaged(u8) = .empty;
        errdefer json.deinit(allocator);

        try json.appendSlice(allocator, "{\"joins\":[");

        for (self.joins.items, 0..) |join, i| {
            if (i > 0) try json.append(allocator, ',');
            try json.appendSlice(allocator, "{\"key\":\"");
            try json.appendSlice(allocator, join.key);
            try json.appendSlice(allocator, "\",\"meta\":");
            const meta_json = try join.meta.toJson(allocator);
            defer allocator.free(meta_json);
            try json.appendSlice(allocator, meta_json);
            try json.append(allocator, '}');
        }

        try json.appendSlice(allocator, "],\"leaves\":[");

        for (self.leaves.items, 0..) |leave, i| {
            if (i > 0) try json.append(allocator, ',');
            try json.appendSlice(allocator, "{\"key\":\"");
            try json.appendSlice(allocator, leave.key);
            try json.appendSlice(allocator, "\"}");
        }

        try json.appendSlice(allocator, "]}");
        return json.toOwnedSlice(allocator);
    }
};

/// Presence state - tracks all presence entries
pub const PresenceState = struct {
    /// Topic -> (Key -> Presence)
    topics: std.StringHashMapUnmanaged(std.StringHashMapUnmanaged(Presence)),
    /// Pending diffs to broadcast
    pending_diffs: std.ArrayListUnmanaged(TopicDiff),
    /// Timeout for presence entries (milliseconds)
    timeout_ms: i64,
    allocator: Allocator,

    const TopicDiff = struct {
        topic: []const u8,
        diff: PresenceDiff,
    };

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .topics = .empty,
            .pending_diffs = .empty,
            .timeout_ms = 30_000, // 30 second default timeout
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        var topic_iter = self.topics.iterator();
        while (topic_iter.next()) |topic_entry| {
            // Free the topic key (owned by the topics hash map)
            self.allocator.free(topic_entry.key_ptr.*);

            // Free each presence in this topic
            var presence_iter = topic_entry.value_ptr.iterator();
            while (presence_iter.next()) |presence_entry| {
                // Free the presence key (owned by the inner hash map)
                self.allocator.free(presence_entry.key_ptr.*);
                // Free the presence metadata
                var p = presence_entry.value_ptr.*;
                p.deinit(self.allocator);
            }
            topic_entry.value_ptr.deinit(self.allocator);
        }
        self.topics.deinit(self.allocator);

        for (self.pending_diffs.items) |*pd| {
            pd.diff.deinit();
        }
        self.pending_diffs.deinit(self.allocator);
    }

    /// Track a presence in a topic
    pub fn track(
        self: *Self,
        topic: []const u8,
        key: []const u8,
        meta_entries: []const struct { []const u8, []const u8 },
    ) !void {
        const now = std.time.milliTimestamp();

        // Get or create topic map - need to dupe topic key if new
        const topic_result = try self.topics.getOrPut(self.allocator, topic);
        if (!topic_result.found_existing) {
            // Need to allocate owned key for the hash map
            const owned_topic_key = try self.allocator.dupe(u8, topic);
            topic_result.key_ptr.* = owned_topic_key;
            topic_result.value_ptr.* = .empty;
        }

        // The topic key in the hash map is now the canonical owned copy
        const topic_key = topic_result.key_ptr.*;

        // Check if presence already exists
        if (topic_result.value_ptr.getPtr(key)) |existing| {
            // Increment ref count
            existing.refs += 1;
            existing.last_seen = now;
            return;
        }

        // Create new presence
        var meta = Metadata.init();
        for (meta_entries) |entry| {
            try meta.data.put(self.allocator, entry[0], entry[1]);
        }

        const owned_key = try self.allocator.dupe(u8, key);
        errdefer self.allocator.free(owned_key);

        // First put into the hash map so we can get the stable key pointer
        const key_result = try topic_result.value_ptr.getOrPut(self.allocator, owned_key);
        // Since we checked above that key doesn't exist, this should always be a new entry
        key_result.key_ptr.* = owned_key;

        const presence = Presence{
            .key = owned_key, // Reference to hash map key
            .topic = topic_key, // Reference to the owned topic key in the hash map
            .meta = meta,
            .joined_at = now,
            .last_seen = now,
            .refs = 1,
        };

        key_result.value_ptr.* = presence;

        // Queue join diff
        try self.queueJoin(topic_key, owned_key, &key_result.value_ptr.meta);
    }

    /// Untrack a presence from a topic
    pub fn untrack(self: *Self, topic: []const u8, key: []const u8) void {
        const topic_map = self.topics.getPtr(topic) orelse return;

        if (topic_map.getPtr(key)) |presence| {
            presence.refs -= 1;

            if (presence.refs == 0) {
                // Queue leave diff before removing
                self.queueLeave(topic, key) catch {};

                // Get the owned key from the hash map before removing
                const owned_key = topic_map.getKey(key) orelse return;

                var p = presence.*;
                p.deinit(self.allocator);
                _ = topic_map.remove(key);

                // Free the owned key after removal
                self.allocator.free(owned_key);
            }
        }
    }

    /// Update presence metadata
    pub fn update(
        self: *Self,
        topic: []const u8,
        key: []const u8,
        meta_entries: []const struct { []const u8, []const u8 },
    ) !void {
        const topic_map = self.topics.getPtr(topic) orelse return;

        if (topic_map.getPtr(key)) |presence| {
            presence.last_seen = std.time.milliTimestamp();

            // Update metadata
            for (meta_entries) |entry| {
                try presence.meta.data.put(self.allocator, entry[0], entry[1]);
            }
        }
    }

    /// List all presences in a topic
    pub fn list(self: *const Self, topic: []const u8) []const Presence {
        const topic_map = self.topics.get(topic) orelse return &.{};

        // Return values - caller should not modify
        var result: std.ArrayListUnmanaged(Presence) = .empty;
        var iter = topic_map.iterator();
        while (iter.next()) |entry| {
            result.append(self.allocator, entry.value_ptr.*) catch {};
        }
        // Note: This leaks - would need proper iterator pattern
        return result.items;
    }

    /// Get count of presences in a topic
    pub fn count(self: *const Self, topic: []const u8) usize {
        const topic_map = self.topics.get(topic) orelse return 0;
        return topic_map.count();
    }

    /// Get a specific presence
    pub fn get(self: *const Self, topic: []const u8, key: []const u8) ?*const Presence {
        const topic_map = self.topics.get(topic) orelse return null;
        return topic_map.getPtr(key);
    }

    /// Heartbeat - update last_seen time
    pub fn heartbeat(self: *Self, topic: []const u8, key: []const u8) void {
        const topic_map = self.topics.getPtr(topic) orelse return;

        if (topic_map.getPtr(key)) |presence| {
            presence.last_seen = std.time.milliTimestamp();
        }
    }

    /// Check for timed-out presences and remove them
    pub fn pruneStale(self: *Self) !void {
        const now = std.time.milliTimestamp();
        const cutoff = now - self.timeout_ms;

        var topics_to_check: std.ArrayListUnmanaged([]const u8) = .empty;
        defer topics_to_check.deinit(self.allocator);

        var topic_iter = self.topics.iterator();
        while (topic_iter.next()) |entry| {
            try topics_to_check.append(self.allocator, entry.key_ptr.*);
        }

        for (topics_to_check.items) |topic| {
            const topic_map = self.topics.getPtr(topic) orelse continue;

            var keys_to_remove: std.ArrayListUnmanaged([]const u8) = .empty;
            defer keys_to_remove.deinit(self.allocator);

            var presence_iter = topic_map.iterator();
            while (presence_iter.next()) |entry| {
                if (entry.value_ptr.last_seen < cutoff) {
                    try keys_to_remove.append(self.allocator, entry.key_ptr.*);
                }
            }

            for (keys_to_remove.items) |key| {
                self.untrack(topic, key);
            }
        }
    }

    /// Take pending diffs
    pub fn takeDiffs(self: *Self) ![]TopicDiff {
        return self.pending_diffs.toOwnedSlice(self.allocator);
    }

    /// Check if there are pending diffs
    pub fn hasPendingDiffs(self: *const Self) bool {
        return self.pending_diffs.items.len > 0;
    }

    fn queueJoin(self: *Self, topic: []const u8, key: []const u8, meta: *const Metadata) !void {
        // Find or create topic diff
        for (self.pending_diffs.items) |*pd| {
            if (std.mem.eql(u8, pd.topic, topic)) {
                try pd.diff.joins.append(self.allocator, .{
                    .key = key,
                    .topic = topic,
                    .meta = meta,
                });
                return;
            }
        }

        // Create new topic diff
        var diff = PresenceDiff.init(self.allocator);
        try diff.joins.append(self.allocator, .{
            .key = key,
            .topic = topic,
            .meta = meta,
        });

        try self.pending_diffs.append(self.allocator, .{
            .topic = topic,
            .diff = diff,
        });
    }

    fn queueLeave(self: *Self, topic: []const u8, key: []const u8) !void {
        const topic_map = self.topics.get(topic) orelse return;

        // Find or create topic diff
        for (self.pending_diffs.items) |*pd| {
            if (std.mem.eql(u8, pd.topic, topic)) {
                if (topic_map.getPtr(key)) |presence| {
                    try pd.diff.leaves.append(self.allocator, .{
                        .key = key,
                        .topic = topic,
                        .meta = &presence.meta,
                    });
                }
                return;
            }
        }

        // Create new topic diff
        var diff = PresenceDiff.init(self.allocator);
        if (topic_map.getPtr(key)) |presence| {
            try diff.leaves.append(self.allocator, .{
                .key = key,
                .topic = topic,
                .meta = &presence.meta,
            });
        }

        try self.pending_diffs.append(self.allocator, .{
            .topic = topic,
            .diff = diff,
        });
    }
};

// ============================================================================
// Tests
// ============================================================================

test "presence tracking" {
    const allocator = std.testing.allocator;

    var state = PresenceState.init(allocator);
    defer state.deinit();

    // Track a presence
    try state.track("room:lobby", "user_1", &.{
        .{ "name", "Alice" },
        .{ "status", "online" },
    });

    try std.testing.expectEqual(@as(usize, 1), state.count("room:lobby"));

    // Get presence
    const presence = state.get("room:lobby", "user_1");
    try std.testing.expect(presence != null);
    try std.testing.expectEqualStrings("Alice", presence.?.meta.get("name").?);

    // Track another
    try state.track("room:lobby", "user_2", &.{
        .{ "name", "Bob" },
    });

    try std.testing.expectEqual(@as(usize, 2), state.count("room:lobby"));

    // Untrack
    state.untrack("room:lobby", "user_1");
    try std.testing.expectEqual(@as(usize, 1), state.count("room:lobby"));
}

test "presence diffs" {
    const allocator = std.testing.allocator;

    var state = PresenceState.init(allocator);
    defer state.deinit();

    // Track generates join diff
    try state.track("room:lobby", "user_1", &.{});

    try std.testing.expect(state.hasPendingDiffs());

    const diffs = try state.takeDiffs();
    defer {
        for (diffs) |*d| {
            var diff = d.*;
            diff.diff.deinit();
        }
        allocator.free(diffs);
    }

    try std.testing.expectEqual(@as(usize, 1), diffs.len);
    try std.testing.expectEqual(@as(usize, 1), diffs[0].diff.joins.items.len);
}

test "presence ref counting" {
    const allocator = std.testing.allocator;

    var state = PresenceState.init(allocator);
    defer state.deinit();

    // Same user tracks twice (multiple tabs)
    try state.track("room:lobby", "user_1", &.{});
    try state.track("room:lobby", "user_1", &.{});

    try std.testing.expectEqual(@as(usize, 1), state.count("room:lobby"));

    // Get presence and check refs
    const presence = state.get("room:lobby", "user_1");
    try std.testing.expectEqual(@as(u32, 2), presence.?.refs);

    // Untrack once - still present
    state.untrack("room:lobby", "user_1");
    try std.testing.expectEqual(@as(usize, 1), state.count("room:lobby"));

    // Untrack again - removed
    state.untrack("room:lobby", "user_1");
    try std.testing.expectEqual(@as(usize, 0), state.count("room:lobby"));
}

test "metadata to json" {
    const allocator = std.testing.allocator;

    var meta = Metadata.init();
    defer meta.deinit(allocator);

    try meta.data.put(allocator, "name", "Alice");
    try meta.data.put(allocator, "status", "online");

    const json = try meta.toJson(allocator);
    defer allocator.free(json);

    // Should contain both fields (order may vary)
    try std.testing.expect(std.mem.indexOf(u8, json, "\"name\":\"Alice\"") != null);
    try std.testing.expect(std.mem.indexOf(u8, json, "\"status\":\"online\"") != null);
}
