//! String Interner - Identifier Deduplication
//!
//! Provides efficient string interning for identifiers and keywords.
//! Interned strings are stored once and referenced by a compact index,
//! enabling O(1) equality comparison and reduced memory usage.

const std = @import("std");

/// A handle to an interned string - just an index
pub const StringId = enum(u32) {
    /// Sentinel value for "no string"
    null_id = std.math.maxInt(u32),
    _,

    /// Check if this is the null sentinel
    pub fn isNull(self: StringId) bool {
        return self == .null_id;
    }
};

/// String interner that deduplicates strings and provides fast equality checks
pub const StringInterner = struct {
    /// Stored strings (indexed by StringId)
    strings: std.ArrayListUnmanaged([]const u8),
    /// Map from string content to StringId for deduplication
    map: std.StringHashMapUnmanaged(StringId),
    /// Arena allocator that owns all string memory
    arena: std.heap.ArenaAllocator,

    const Self = @This();

    /// Initialize a new string interner
    pub fn init(backing_allocator: std.mem.Allocator) Self {
        return .{
            .strings = .{},
            .map = .{},
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    /// Clean up all resources
    pub fn deinit(self: *Self) void {
        self.strings.deinit(self.arena.allocator());
        self.map.deinit(self.arena.allocator());
        self.arena.deinit();
    }

    /// Intern a string, returning a deduplicated handle.
    /// If the string was already interned, returns the existing handle.
    /// The interner owns the memory - caller's slice can be freed after this.
    pub fn intern(self: *Self, str: []const u8) !StringId {
        const allocator = self.arena.allocator();

        // Check if already interned
        if (self.map.get(str)) |existing| {
            return existing;
        }

        // Copy string to arena (interner owns the memory)
        const owned = try allocator.dupe(u8, str);
        const id: StringId = @enumFromInt(@as(u32, @intCast(self.strings.items.len)));

        try self.strings.append(allocator, owned);
        try self.map.put(allocator, owned, id);

        return id;
    }

    /// Get the string for an interned handle
    pub fn get(self: *const Self, id: StringId) []const u8 {
        if (id.isNull()) return "";
        return self.strings.items[@intFromEnum(id)];
    }

    /// Compare two interned strings for equality (O(1) - just index comparison)
    pub fn eql(a: StringId, b: StringId) bool {
        return a == b;
    }

    /// Get the number of unique strings interned
    pub fn count(self: *const Self) usize {
        return self.strings.items.len;
    }

    /// Check if a string has been interned
    pub fn contains(self: *const Self, str: []const u8) bool {
        return self.map.contains(str);
    }

    /// Get the StringId for a string if it exists, without interning
    pub fn lookup(self: *const Self, str: []const u8) ?StringId {
        return self.map.get(str);
    }
};

// ============================================================
// Tests
// ============================================================

test "basic interning" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const id1 = try interner.intern("hello");
    const id2 = try interner.intern("world");
    const id3 = try interner.intern("hello"); // duplicate

    // Same string should return same ID
    try std.testing.expect(StringInterner.eql(id1, id3));
    try std.testing.expect(!StringInterner.eql(id1, id2));

    // Should be able to retrieve strings
    try std.testing.expectEqualStrings("hello", interner.get(id1));
    try std.testing.expectEqualStrings("world", interner.get(id2));

    // Count should only have 2 unique strings
    try std.testing.expectEqual(@as(usize, 2), interner.count());
}

test "empty string" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const id = try interner.intern("");
    try std.testing.expectEqualStrings("", interner.get(id));
}

test "null id" {
    try std.testing.expect(StringId.null_id.isNull());

    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    const id = try interner.intern("test");
    try std.testing.expect(!id.isNull());

    // Null ID returns empty string
    try std.testing.expectEqualStrings("", interner.get(.null_id));
}

test "lookup without interning" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    try std.testing.expect(!interner.contains("missing"));
    try std.testing.expect(interner.lookup("missing") == null);

    _ = try interner.intern("exists");
    try std.testing.expect(interner.contains("exists"));
    try std.testing.expect(interner.lookup("exists") != null);
}

test "many strings" {
    var interner = StringInterner.init(std.testing.allocator);
    defer interner.deinit();

    // Intern many strings
    var ids: [100]StringId = undefined;
    for (0..100) |i| {
        var buf: [32]u8 = undefined;
        const str = std.fmt.bufPrint(&buf, "string_{d}", .{i}) catch unreachable;
        ids[i] = try interner.intern(str);
    }

    // Verify all are retrievable and correct
    for (0..100) |i| {
        var buf: [32]u8 = undefined;
        const expected = std.fmt.bufPrint(&buf, "string_{d}", .{i}) catch unreachable;
        try std.testing.expectEqualStrings(expected, interner.get(ids[i]));
    }

    try std.testing.expectEqual(@as(usize, 100), interner.count());
}
