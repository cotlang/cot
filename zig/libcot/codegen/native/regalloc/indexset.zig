//! Index sets: sets of integers that represent indices into a space.
//!
//! Ported from regalloc2's `src/indexset.rs`.
//!
//! This module provides a sparse bit set with hybrid small/large mode.
//! When the number of distinct 64-bit words is small (<= 12), the set
//! uses inline storage. When it grows larger, it switches to a HashMap.

const std = @import("std");

//=============================================================================
// Constants
//=============================================================================

const SMALL_ELEMS: usize = 12;
const BITS_PER_WORD: usize = 64;
const INVALID: u32 = 0xffff_ffff;

//=============================================================================
// SetBitsIter - Iterate over set bits in a u64
//=============================================================================

/// Iterator over set bits in a 64-bit word.
pub const SetBitsIter = struct {
    bits: u64,

    pub fn init(bits: u64) SetBitsIter {
        return .{ .bits = bits };
    }

    /// Returns the next set bit index (0-63), or null if no more bits.
    pub fn next(self: *SetBitsIter) ?usize {
        if (self.bits == 0) {
            return null;
        }
        const bitidx = @ctz(self.bits);
        self.bits &= self.bits - 1; // Clear lowest set bit
        return bitidx;
    }
};

//=============================================================================
// AdaptiveMap - Hybrid small/large mode sparse map
//=============================================================================

/// A hybrid large/small-mode sparse mapping from integer indices (u32) to
/// 64-bit words (u64).
///
/// In small mode, stores up to SMALL_ELEMS entries inline.
/// In large mode, uses a HashMap for arbitrary capacity.
const AdaptiveMap = struct {
    const Self = @This();

    const Mode = union(enum) {
        small: struct {
            len: u32,
            keys: [SMALL_ELEMS]u32,
            values: [SMALL_ELEMS]u64,
        },
        large: std.AutoHashMap(u32, u64),
    };

    mode: Mode,

    /// Create a new empty AdaptiveMap in small mode.
    pub fn init() AdaptiveMap {
        return .{
            .mode = .{
                .small = .{
                    .len = 0,
                    .keys = [_]u32{INVALID} ** SMALL_ELEMS,
                    .values = [_]u64{0} ** SMALL_ELEMS,
                },
            },
        };
    }

    /// Free resources (only needed in large mode).
    pub fn deinit(self: *AdaptiveMap) void {
        switch (self.mode) {
            .small => {},
            .large => |*map| map.deinit(),
        }
    }

    /// Clone the map (requires allocator for large mode).
    pub fn clone(self: *const AdaptiveMap, allocator: std.mem.Allocator) !AdaptiveMap {
        switch (self.mode) {
            .small => |s| {
                return .{
                    .mode = .{
                        .small = .{
                            .len = s.len,
                            .keys = s.keys,
                            .values = s.values,
                        },
                    },
                };
            },
            .large => |*map| {
                var new_map = std.AutoHashMap(u32, u64).init(allocator);
                var iter = map.iterator();
                while (iter.next()) |entry| {
                    try new_map.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                return .{ .mode = .{ .large = new_map } };
            },
        }
    }

    /// Get or insert a value for the given key, returning a pointer to the value.
    /// If the key doesn't exist, inserts 0 and returns pointer to it.
    pub fn getOrInsert(self: *AdaptiveMap, allocator: std.mem.Allocator, key: u32) !*u64 {
        switch (self.mode) {
            .small => |*s| {
                // Check if key exists
                for (0..s.len) |i| {
                    if (s.keys[i] == key) {
                        return &s.values[i];
                    }
                }

                // Key not found - try to add it
                if (s.len < SMALL_ELEMS) {
                    // Room to add
                    const idx = s.len;
                    s.len += 1;
                    s.keys[idx] = key;
                    s.values[idx] = 0;
                    return &s.values[idx];
                }

                // Check if any existing value is zero - reuse that slot
                for (0..SMALL_ELEMS) |i| {
                    if (s.values[i] == 0) {
                        s.keys[i] = key;
                        return &s.values[i];
                    }
                }

                // Must expand to large mode
                var map = std.AutoHashMap(u32, u64).init(allocator);
                for (0..s.len) |i| {
                    try map.put(s.keys[i], s.values[i]);
                }
                try map.put(key, 0);
                self.mode = .{ .large = map };
                return self.mode.large.getPtr(key).?;
            },
            .large => |*map| {
                const result = try map.getOrPut(key);
                if (!result.found_existing) {
                    result.value_ptr.* = 0;
                }
                return result.value_ptr;
            },
        }
    }

    /// Get a mutable pointer to the value for the given key, if it exists.
    pub fn getMut(self: *AdaptiveMap, key: u32) ?*u64 {
        switch (self.mode) {
            .small => |*s| {
                for (0..s.len) |i| {
                    if (s.keys[i] == key) {
                        return &s.values[i];
                    }
                }
                return null;
            },
            .large => |*map| {
                return map.getPtr(key);
            },
        }
    }

    /// Get the value for the given key, if it exists.
    pub fn get(self: *const AdaptiveMap, key: u32) ?u64 {
        switch (self.mode) {
            .small => |s| {
                for (0..s.len) |i| {
                    if (s.keys[i] == key) {
                        return s.values[i];
                    }
                }
                return null;
            },
            .large => |*map| {
                return map.get(key);
            },
        }
    }

    /// Check if the map is empty (all values are zero).
    pub fn isEmpty(self: *const AdaptiveMap) bool {
        switch (self.mode) {
            .small => |s| {
                for (s.values[0..s.len]) |v| {
                    if (v != 0) return false;
                }
                return true;
            },
            .large => |*map| {
                var iter = map.valueIterator();
                while (iter.next()) |v| {
                    if (v.* != 0) return false;
                }
                return true;
            },
        }
    }

    /// Check if in small mode.
    pub fn isSmall(self: *const AdaptiveMap) bool {
        return switch (self.mode) {
            .small => true,
            .large => false,
        };
    }

    /// Iterator over (key, value) pairs.
    pub const Iterator = struct {
        map: *const AdaptiveMap,
        small_idx: usize,
        large_iter: ?std.AutoHashMap(u32, u64).Iterator,

        pub fn next(self: *Iterator) ?struct { key: u32, value: u64 } {
            switch (self.map.mode) {
                .small => |s| {
                    if (self.small_idx >= s.len) {
                        return null;
                    }
                    const idx = self.small_idx;
                    self.small_idx += 1;
                    return .{ .key = s.keys[idx], .value = s.values[idx] };
                },
                .large => {
                    if (self.large_iter) |*iter| {
                        if (iter.next()) |entry| {
                            return .{ .key = entry.key_ptr.*, .value = entry.value_ptr.* };
                        }
                    }
                    return null;
                },
            }
        }
    };

    pub fn iterator(self: *const AdaptiveMap) Iterator {
        return .{
            .map = self,
            .small_idx = 0,
            .large_iter = switch (self.mode) {
                .small => null,
                .large => |*map| map.iterator(),
            },
        };
    }
};

//=============================================================================
// IndexSet - Sparse bit set
//=============================================================================

/// A conceptually infinite-length set of indices that allows union
/// and efficient iteration over elements.
///
/// Uses AdaptiveMap internally with each u64 word holding 64 bits.
pub const IndexSet = struct {
    const Self = @This();

    elems: AdaptiveMap,
    allocator: std.mem.Allocator,
    // Cache for streaming access: (word_index, word_value)
    cache_key: u32,
    cache_value: u64,

    /// Create a new empty IndexSet.
    pub fn init(allocator: std.mem.Allocator) IndexSet {
        return .{
            .elems = AdaptiveMap.init(),
            .allocator = allocator,
            .cache_key = INVALID,
            .cache_value = 0,
        };
    }

    /// Free all resources.
    pub fn deinit(self: *IndexSet) void {
        self.elems.deinit();
    }

    /// Clone this set.
    pub fn clone(self: *const IndexSet) !IndexSet {
        return .{
            .elems = try self.elems.clone(self.allocator),
            .allocator = self.allocator,
            .cache_key = self.cache_key,
            .cache_value = self.cache_value,
        };
    }

    /// Assign from another set.
    pub fn assign(self: *IndexSet, other: *const IndexSet) !void {
        self.elems.deinit();
        self.elems = try other.elems.clone(self.allocator);
        self.cache_key = other.cache_key;
        self.cache_value = other.cache_value;
    }

    /// Get a mutable pointer to the word containing bit_index,
    /// inserting a zero word if needed.
    fn elem(self: *IndexSet, bit_index: usize) !*u64 {
        const word_index: u32 = @intCast(bit_index / BITS_PER_WORD);
        if (self.cache_key == word_index) {
            self.cache_key = INVALID;
            self.cache_value = 0;
        }
        return try self.elems.getOrInsert(self.allocator, word_index);
    }

    /// Get a mutable pointer to the word containing bit_index, if it exists.
    fn maybeElemMut(self: *IndexSet, bit_index: usize) ?*u64 {
        const word_index: u32 = @intCast(bit_index / BITS_PER_WORD);
        if (self.cache_key == word_index) {
            self.cache_key = INVALID;
            self.cache_value = 0;
        }
        return self.elems.getMut(word_index);
    }

    /// Get the word containing bit_index, if it exists.
    fn maybeElem(self: *const IndexSet, bit_index: usize) ?u64 {
        const word_index: u32 = @intCast(bit_index / BITS_PER_WORD);
        if (self.cache_key == word_index) {
            return self.cache_value;
        }
        return self.elems.get(word_index);
    }

    /// Set or clear a bit.
    pub fn set(self: *IndexSet, idx: usize, val: bool) !void {
        const bit: u6 = @intCast(idx % BITS_PER_WORD);
        if (val) {
            const word = try self.elem(idx);
            word.* |= @as(u64, 1) << bit;
        } else if (self.maybeElemMut(idx)) |word| {
            word.* &= ~(@as(u64, 1) << bit);
        }
    }

    /// Get the value of a bit.
    pub fn get(self: *const IndexSet, idx: usize) bool {
        const bit: u6 = @intCast(idx % BITS_PER_WORD);
        if (self.maybeElem(idx)) |word| {
            return (word & (@as(u64, 1) << bit)) != 0;
        }
        return false;
    }

    /// Add a bit (shorthand for set(idx, true)).
    pub fn add(self: *IndexSet, idx: usize) !void {
        try self.set(idx, true);
    }

    /// Remove a bit (shorthand for set(idx, false)).
    pub fn remove(self: *IndexSet, idx: usize) !void {
        try self.set(idx, false);
    }

    /// Union this set with another. Returns true if this set changed.
    pub fn unionWith(self: *IndexSet, other: *const IndexSet) !bool {
        var changed: u64 = 0;
        var map_iter = other.elems.iterator();
        while (map_iter.next()) |entry| {
            if (entry.value == 0) {
                continue;
            }
            const word_idx = entry.key;
            const self_word = try self.elems.getOrInsert(self.allocator, word_idx);
            changed |= entry.value & ~self_word.*;
            self_word.* |= entry.value;
        }
        // Invalidate cache
        self.cache_key = INVALID;
        self.cache_value = 0;
        return changed != 0;
    }

    /// Is the adaptive data structure in "small" mode?
    pub fn isSmall(self: *const IndexSet) bool {
        return self.elems.isSmall();
    }

    /// Is the set empty?
    pub fn isEmpty(self: *const IndexSet) bool {
        return self.elems.isEmpty();
    }

    /// Iterator over set bit indices.
    pub const Iterator = struct {
        map_iter: AdaptiveMap.Iterator,
        current_word_idx: u32,
        bits_iter: SetBitsIter,

        pub fn next(self: *Iterator) ?usize {
            while (true) {
                // Try to get next bit from current word
                if (self.bits_iter.next()) |bit| {
                    return BITS_PER_WORD * self.current_word_idx + bit;
                }
                // Move to next word
                if (self.map_iter.next()) |entry| {
                    if (entry.value == 0) {
                        continue; // Skip zero words
                    }
                    self.current_word_idx = entry.key;
                    self.bits_iter = SetBitsIter.init(entry.value);
                } else {
                    return null;
                }
            }
        }
    };

    /// Returns an iterator over all set bit indices.
    pub fn iter(self: *const IndexSet) Iterator {
        return .{
            .map_iter = self.elems.iterator(),
            .current_word_idx = 0,
            .bits_iter = SetBitsIter.init(0),
        };
    }

    /// Format for debug output.
    pub fn format(
        self: *const IndexSet,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.writeAll("{");
        var first = true;
        var it = self.iter();
        while (it.next()) |idx| {
            if (!first) try writer.writeAll(", ");
            first = false;
            try writer.print("{d}", .{idx});
        }
        try writer.writeAll("}");
    }
};

//=============================================================================
// Tests
//=============================================================================

test "SetBitsIter" {
    var iter = SetBitsIter.init(0b10110);
    try std.testing.expectEqual(@as(?usize, 1), iter.next());
    try std.testing.expectEqual(@as(?usize, 2), iter.next());
    try std.testing.expectEqual(@as(?usize, 4), iter.next());
    try std.testing.expectEqual(@as(?usize, null), iter.next());
}

test "IndexSet basic operations" {
    var set = IndexSet.init(std.testing.allocator);
    defer set.deinit();

    // Initially empty
    try std.testing.expect(set.isEmpty());
    try std.testing.expect(!set.get(5));

    // Set some bits
    try set.set(5, true);
    try set.set(100, true);
    try set.set(1000, true);

    try std.testing.expect(!set.isEmpty());
    try std.testing.expect(set.get(5));
    try std.testing.expect(set.get(100));
    try std.testing.expect(set.get(1000));
    try std.testing.expect(!set.get(6));
    try std.testing.expect(!set.get(99));

    // Clear a bit
    try set.set(100, false);
    try std.testing.expect(!set.get(100));
    try std.testing.expect(set.get(5));
}

test "IndexSet iteration" {
    var set = IndexSet.init(std.testing.allocator);
    defer set.deinit();

    // Set bits with pattern: every 17th bit up to 1024
    var sum: usize = 0;
    var i: usize = 0;
    while (i < 1024) : (i += 17) {
        try set.set(i, true);
        sum += i;
    }

    // Verify iteration
    var checksum: usize = 0;
    var iter = set.iter();
    while (iter.next()) |bit| {
        try std.testing.expect(bit % 17 == 0);
        checksum += bit;
    }
    try std.testing.expectEqual(sum, checksum);
}

test "IndexSet small mode preservation" {
    var set = IndexSet.init(std.testing.allocator);
    defer set.deinit();

    // Set 12 different words (max small-mode size)
    for (0..12) |word_i| {
        try set.set(64 * word_i, true);
    }
    try std.testing.expect(set.isSmall());

    // Clear a bit and set a bit in a different word
    // Should still be in small mode (reuses zero slot)
    try set.set(64 * 5, false);
    try set.set(64 * 100, true);
    try std.testing.expect(set.isSmall());
}

test "IndexSet union" {
    var set1 = IndexSet.init(std.testing.allocator);
    defer set1.deinit();

    var set2 = IndexSet.init(std.testing.allocator);
    defer set2.deinit();

    try set1.set(1, true);
    try set1.set(5, true);
    try set1.set(100, true);

    try set2.set(5, true);
    try set2.set(10, true);
    try set2.set(200, true);

    // Union should return true (changed)
    const changed = try set1.unionWith(&set2);
    try std.testing.expect(changed);

    // set1 should now have all bits
    try std.testing.expect(set1.get(1));
    try std.testing.expect(set1.get(5));
    try std.testing.expect(set1.get(10));
    try std.testing.expect(set1.get(100));
    try std.testing.expect(set1.get(200));

    // Union again should return false (no change)
    const changed2 = try set1.unionWith(&set2);
    try std.testing.expect(!changed2);
}

test "IndexSet expand to large mode" {
    var set = IndexSet.init(std.testing.allocator);
    defer set.deinit();

    // Set bits in 13 different words (exceeds small mode)
    for (0..13) |word_i| {
        try set.set(64 * word_i + 1, true); // +1 to ensure non-zero values
    }

    // Should have expanded to large mode
    try std.testing.expect(!set.isSmall());

    // All bits should still be set
    for (0..13) |word_i| {
        try std.testing.expect(set.get(64 * word_i + 1));
    }
}

test "AdaptiveMap basic" {
    var map = AdaptiveMap.init();
    defer map.deinit();

    // Initially empty
    try std.testing.expect(map.isEmpty());
    try std.testing.expect(map.get(5) == null);

    // Get or insert
    const ptr = try map.getOrInsert(std.testing.allocator, 5);
    ptr.* = 42;

    try std.testing.expect(!map.isEmpty());
    try std.testing.expectEqual(@as(?u64, 42), map.get(5));

    // Get existing
    const ptr2 = try map.getOrInsert(std.testing.allocator, 5);
    try std.testing.expectEqual(@as(u64, 42), ptr2.*);
}
