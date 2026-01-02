//! Ordered Map Data Structure
//!
//! A hash map that maintains insertion order (access list).
//! This provides O(1) key lookups while preserving the order of entries
//! for iteration and positional access (needed for DBL NSPC_* compatibility).
//!
//! ## Architecture
//!
//! ```
//! ┌─────────────────┐     ┌──────────────────────┐
//! │   Hash Index    │     │     Entry Store      │
//! │ key -> entry_idx│     │ [Entry0, Entry1, ...]│
//! └─────────────────┘     └──────────────────────┘
//!          │                        ▲
//!          └────────────────────────┘
//!
//! ┌──────────────────────────────────────────────┐
//! │              Access Order List               │
//! │ [entry_idx_first, entry_idx_second, ...]     │
//! │ (maintains insertion/move order)             │
//! └──────────────────────────────────────────────┘
//! ```

const std = @import("std");
const Value = @import("value.zig").Value;

/// Ordered map with O(1) key lookup and preserved insertion order
pub const OrderedMap = struct {
    allocator: std.mem.Allocator,
    /// Entry storage - entries indexed by entry_idx
    entries: std.ArrayListUnmanaged(Entry),
    /// Hash index: key -> entry index (for O(1) lookup)
    index: std.StringHashMapUnmanaged(usize),
    /// Access order: position -> entry_idx (for positional access)
    access_order: std.ArrayListUnmanaged(usize),
    /// Map behavior flags
    flags: Flags,
    /// Count of non-deleted entries
    live_count: usize,

    const Self = @This();

    /// Entry in the map
    pub const Entry = struct {
        /// Key string (owned copy)
        key: []const u8,
        /// Value stored
        value: Value,
        /// True if this entry has been deleted (tombstone)
        deleted: bool = false,
    };

    /// Map behavior flags
    pub const Flags = packed struct {
        /// If true, key comparisons are case-sensitive
        case_sensitive: bool = true,
        /// If true, preserve spaces in keys
        preserve_spaces: bool = true,
        /// Reserved for future use
        _reserved: u6 = 0,
    };

    /// Sentinel value for deleted/invalid access positions
    pub const DELETED_SENTINEL: usize = std.math.maxInt(usize);

    /// Initialize an empty ordered map
    pub fn init(allocator: std.mem.Allocator, flags: Flags) Self {
        return .{
            .allocator = allocator,
            .entries = .empty,
            .index = .empty,
            .access_order = .empty,
            .flags = flags,
            .live_count = 0,
        };
    }

    /// Free all resources
    pub fn deinit(self: *Self) void {
        // Free owned key strings (keys are always owned by the map)
        for (self.entries.items) |entry| {
            if (!entry.deleted) {
                self.allocator.free(entry.key);
                // Note: We don't deinit values because they may use
                // a different allocator (VM's arena). Arena handles cleanup.
            }
        }
        self.entries.deinit(self.allocator);
        self.index.deinit(self.allocator);
        self.access_order.deinit(self.allocator);
    }

    /// Normalize a key according to flags
    fn normalizeKey(self: *Self, key: []const u8) ![]const u8 {
        var result = key;

        // Handle space preservation
        if (!self.flags.preserve_spaces) {
            result = std.mem.trim(u8, result, " ");
        }

        // Handle case sensitivity
        if (!self.flags.case_sensitive) {
            const lower = try self.allocator.alloc(u8, result.len);
            for (result, 0..) |c, i| {
                lower[i] = std.ascii.toLower(c);
            }
            return lower;
        }

        return result;
    }

    /// Set a key-value pair, returns access code (1-based position)
    /// If key exists, updates the value and returns existing access code
    pub fn set(self: *Self, key: []const u8, value: Value) !usize {
        const lookup_key = try self.normalizeKey(key);
        defer if (!self.flags.case_sensitive) self.allocator.free(lookup_key);

        // Check if key already exists
        if (self.index.get(lookup_key)) |entry_idx| {
            // Note: We don't deinit the old value because values may use
            // a different allocator (VM's arena). Arena handles cleanup.
            // Update existing entry
            self.entries.items[entry_idx].value = value;
            // Find and return the access code for this entry
            for (self.access_order.items, 0..) |idx, access_code| {
                if (idx == entry_idx) {
                    return access_code + 1; // 1-based
                }
            }
            unreachable; // Entry exists but not in access order - shouldn't happen
        }

        // New entry - make owned copy of key
        const owned_key = try self.allocator.dupe(u8, lookup_key);
        errdefer self.allocator.free(owned_key);

        const entry_idx = self.entries.items.len;
        try self.entries.append(self.allocator, .{
            .key = owned_key,
            .value = value,
            .deleted = false,
        });

        try self.index.put(self.allocator, owned_key, entry_idx);
        try self.access_order.append(self.allocator, entry_idx);
        self.live_count += 1;

        return self.access_order.items.len; // 1-based access code
    }

    /// Get value by key, returns null if not found
    pub fn get(self: *Self, key: []const u8) ?Value {
        const lookup_key = self.normalizeKey(key) catch return null;
        defer if (!self.flags.case_sensitive) self.allocator.free(lookup_key);

        const entry_idx = self.index.get(lookup_key) orelse return null;
        const entry = &self.entries.items[entry_idx];
        if (entry.deleted) return null;
        return entry.value;
    }

    /// Get entry at access code position (1-based), returns null if out of bounds or deleted
    pub fn getAt(self: *Self, access_code: usize) ?Entry {
        if (access_code == 0 or access_code > self.access_order.items.len) {
            return null;
        }
        const entry_idx = self.access_order.items[access_code - 1];
        if (entry_idx == DELETED_SENTINEL) return null;
        const entry = &self.entries.items[entry_idx];
        if (entry.deleted) return null;
        return entry.*;
    }

    /// Get entry index at access code (1-based), returns null if invalid
    fn getEntryIdx(self: *Self, access_code: usize) ?usize {
        if (access_code == 0 or access_code > self.access_order.items.len) {
            return null;
        }
        const entry_idx = self.access_order.items[access_code - 1];
        if (entry_idx == DELETED_SENTINEL) return null;
        return entry_idx;
    }

    /// Delete by key, returns true if deleted
    pub fn delete(self: *Self, key: []const u8) bool {
        const lookup_key = self.normalizeKey(key) catch return false;
        defer if (!self.flags.case_sensitive) self.allocator.free(lookup_key);

        const entry_idx = self.index.get(lookup_key) orelse return false;
        return self.deleteByIdx(entry_idx);
    }

    /// Delete by access code (1-based), returns true if deleted
    pub fn deleteAt(self: *Self, access_code: usize) bool {
        const entry_idx = self.getEntryIdx(access_code) orelse return false;
        // Mark the access order slot as deleted
        self.access_order.items[access_code - 1] = DELETED_SENTINEL;
        return self.deleteByIdx(entry_idx);
    }

    /// Internal: delete by entry index
    fn deleteByIdx(self: *Self, entry_idx: usize) bool {
        var entry = &self.entries.items[entry_idx];
        if (entry.deleted) return false;

        // Remove from hash index
        _ = self.index.remove(entry.key);

        // Free the key
        self.allocator.free(entry.key);
        entry.key = "";

        // Note: We don't call deinit on values because they may have been
        // allocated with a different allocator (e.g., VM's arena allocator).
        // The VM's arena will clean up all values when it exits.

        entry.deleted = true;
        self.live_count -= 1;

        return true;
    }

    /// Find key and return access code (1-based), or null if not found
    pub fn find(self: *Self, key: []const u8) ?usize {
        const lookup_key = self.normalizeKey(key) catch return null;
        defer if (!self.flags.case_sensitive) self.allocator.free(lookup_key);

        const entry_idx = self.index.get(lookup_key) orelse return null;
        const entry = &self.entries.items[entry_idx];
        if (entry.deleted) return null;

        // Find position in access order
        for (self.access_order.items, 0..) |idx, access_code| {
            if (idx == entry_idx) {
                return access_code + 1; // 1-based
            }
        }
        return null;
    }

    /// Check if key exists
    pub fn has(self: *Self, key: []const u8) bool {
        return self.get(key) != null;
    }

    /// Move entry from one position to another
    /// from and to are 1-based access codes
    pub fn move(self: *Self, from: usize, to: usize) bool {
        if (from == 0 or to == 0) return false;
        if (from > self.access_order.items.len) return false;

        const entry_idx = self.access_order.items[from - 1];
        if (entry_idx == DELETED_SENTINEL) return false;

        // Remove from old position
        _ = self.access_order.orderedRemove(from - 1);

        // Insert at new position (clamp to valid range)
        const insert_pos = @min(to - 1, self.access_order.items.len);
        self.access_order.insert(self.allocator, insert_pos, entry_idx) catch return false;

        return true;
    }

    /// Update value at access code position (1-based)
    pub fn putAt(self: *Self, access_code: usize, value: Value) bool {
        if (access_code == 0 or access_code > self.access_order.items.len) {
            return false;
        }
        const entry_idx = self.access_order.items[access_code - 1];
        if (entry_idx == DELETED_SENTINEL) return false;

        var entry = &self.entries.items[entry_idx];
        if (entry.deleted) return false;

        entry.value = value;
        return true;
    }

    /// Get number of live (non-deleted) entries
    pub fn len(self: *const Self) usize {
        return self.live_count;
    }

    /// Clear all entries
    pub fn clear(self: *Self) void {
        // Free all owned keys (keys are always owned by the map)
        for (self.entries.items) |entry| {
            if (!entry.deleted) {
                self.allocator.free(entry.key);
                // Note: We don't deinit values because they may use
                // a different allocator (VM's arena). Arena handles cleanup.
            }
        }
        self.entries.clearRetainingCapacity();
        self.index.clearRetainingCapacity();
        self.access_order.clearRetainingCapacity();
        self.live_count = 0;
    }

    /// Iterator over entries in access order
    pub const Iterator = struct {
        map: *const Self,
        position: usize,

        pub fn next(self: *Iterator) ?Entry {
            while (self.position < self.map.access_order.items.len) {
                const entry_idx = self.map.access_order.items[self.position];
                self.position += 1;

                if (entry_idx == DELETED_SENTINEL) continue;
                const entry = &self.map.entries.items[entry_idx];
                if (!entry.deleted) {
                    return entry.*;
                }
            }
            return null;
        }

        /// Reset iterator to beginning
        pub fn reset(self: *Iterator) void {
            self.position = 0;
        }
    };

    /// Get iterator over entries in access order
    pub fn iterator(self: *const Self) Iterator {
        return .{ .map = self, .position = 0 };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "OrderedMap basic operations" {
    const allocator = std.testing.allocator;

    var map = OrderedMap.init(allocator, .{});
    defer map.deinit();

    // Add entries
    const code1 = try map.set("key1", Value.initInt(100));
    const code2 = try map.set("key2", Value.initInt(200));
    const code3 = try map.set("key3", Value.initInt(300));

    try std.testing.expectEqual(@as(usize, 1), code1);
    try std.testing.expectEqual(@as(usize, 2), code2);
    try std.testing.expectEqual(@as(usize, 3), code3);

    // Get by key
    try std.testing.expectEqual(@as(i64, 100), map.get("key1").?.asInt());
    try std.testing.expectEqual(@as(i64, 200), map.get("key2").?.asInt());
    try std.testing.expectEqual(@as(i64, 300), map.get("key3").?.asInt());
    try std.testing.expect(map.get("nonexistent") == null);

    // Get by position
    try std.testing.expectEqual(@as(i64, 100), map.getAt(1).?.value.asInt());
    try std.testing.expectEqual(@as(i64, 200), map.getAt(2).?.value.asInt());
    try std.testing.expectEqual(@as(i64, 300), map.getAt(3).?.value.asInt());
    try std.testing.expect(map.getAt(0) == null);
    try std.testing.expect(map.getAt(4) == null);

    // Find by key
    try std.testing.expectEqual(@as(usize, 1), map.find("key1").?);
    try std.testing.expectEqual(@as(usize, 2), map.find("key2").?);
    try std.testing.expectEqual(@as(usize, 3), map.find("key3").?);
    try std.testing.expect(map.find("nonexistent") == null);

    // Length
    try std.testing.expectEqual(@as(usize, 3), map.len());
}

test "OrderedMap update existing" {
    const allocator = std.testing.allocator;

    var map = OrderedMap.init(allocator, .{});
    defer map.deinit();

    _ = try map.set("key", Value.initInt(100));
    try std.testing.expectEqual(@as(i64, 100), map.get("key").?.asInt());

    // Update same key
    const code = try map.set("key", Value.initInt(999));
    try std.testing.expectEqual(@as(usize, 1), code); // Same position
    try std.testing.expectEqual(@as(i64, 999), map.get("key").?.asInt());
    try std.testing.expectEqual(@as(usize, 1), map.len()); // Still one entry
}

test "OrderedMap delete" {
    const allocator = std.testing.allocator;

    var map = OrderedMap.init(allocator, .{});
    defer map.deinit();

    _ = try map.set("key1", Value.initInt(100));
    _ = try map.set("key2", Value.initInt(200));
    _ = try map.set("key3", Value.initInt(300));

    try std.testing.expectEqual(@as(usize, 3), map.len());

    // Delete by key
    try std.testing.expect(map.delete("key2"));
    try std.testing.expect(!map.delete("key2")); // Already deleted
    try std.testing.expect(map.get("key2") == null);
    try std.testing.expectEqual(@as(usize, 2), map.len());

    // Delete by position
    try std.testing.expect(map.deleteAt(1)); // key1
    try std.testing.expect(map.get("key1") == null);
    try std.testing.expectEqual(@as(usize, 1), map.len());
}

test "OrderedMap case insensitive" {
    const allocator = std.testing.allocator;

    var map = OrderedMap.init(allocator, .{ .case_sensitive = false });
    defer map.deinit();

    _ = try map.set("MyKey", Value.initInt(100));

    // Should find with different cases
    try std.testing.expectEqual(@as(i64, 100), map.get("mykey").?.asInt());
    try std.testing.expectEqual(@as(i64, 100), map.get("MYKEY").?.asInt());
    try std.testing.expectEqual(@as(i64, 100), map.get("MyKey").?.asInt());
}

test "OrderedMap iteration" {
    const allocator = std.testing.allocator;

    var map = OrderedMap.init(allocator, .{});
    defer map.deinit();

    _ = try map.set("a", Value.initInt(1));
    _ = try map.set("b", Value.initInt(2));
    _ = try map.set("c", Value.initInt(3));

    var iter = map.iterator();
    var sum: i64 = 0;
    var count: usize = 0;
    while (iter.next()) |entry| {
        sum += entry.value.asInt();
        count += 1;
    }

    try std.testing.expectEqual(@as(i64, 6), sum);
    try std.testing.expectEqual(@as(usize, 3), count);
}

test "OrderedMap putAt" {
    const allocator = std.testing.allocator;

    var map = OrderedMap.init(allocator, .{});
    defer map.deinit();

    _ = try map.set("key", Value.initInt(100));
    try std.testing.expectEqual(@as(i64, 100), map.getAt(1).?.value.asInt());

    // Update by position
    try std.testing.expect(map.putAt(1, Value.initInt(999)));
    try std.testing.expectEqual(@as(i64, 999), map.getAt(1).?.value.asInt());
    try std.testing.expectEqual(@as(i64, 999), map.get("key").?.asInt());

    // Invalid position
    try std.testing.expect(!map.putAt(0, Value.initInt(0)));
    try std.testing.expect(!map.putAt(99, Value.initInt(0)));
}

test "OrderedMap clear" {
    const allocator = std.testing.allocator;

    var map = OrderedMap.init(allocator, .{});
    defer map.deinit();

    _ = try map.set("a", Value.initInt(1));
    _ = try map.set("b", Value.initInt(2));
    try std.testing.expectEqual(@as(usize, 2), map.len());

    map.clear();
    try std.testing.expectEqual(@as(usize, 0), map.len());
    try std.testing.expect(map.get("a") == null);
}
