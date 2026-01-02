//! Segmented Pool
//!
//! A memory pool that provides stable pointers even when growing.
//! Follows the Ghostty pattern for managing collections of objects
//! where pointer stability is required (e.g., AST nodes, IR instructions).
//!
//! Unlike ArrayList, pointers to elements remain valid after growth
//! because the pool uses segmented storage (like std.SegmentedList).
//!
//! Usage:
//!   var pool: SegmentedPool(AstNode, 256) = .{};
//!   defer pool.deinit(allocator);
//!
//!   // Get a stable pointer
//!   const node = try pool.getGrow(allocator);
//!   node.* = .{ .kind = .binary_op, ... };
//!
//!   // Pointer remains valid even after more allocations
//!   _ = try pool.getGrow(allocator);
//!   // node is still valid!

const std = @import("std");

/// A pool that provides stable pointers to elements of type T.
/// Uses segmented storage so pointers remain valid after growth.
pub fn SegmentedPool(comptime T: type, comptime prealloc: usize) type {
    return struct {
        const Self = @This();

        /// Internal storage using SegmentedList for pointer stability
        list: std.SegmentedList(T, prealloc) = .{},
        /// Number of elements currently allocated
        len: usize = 0,
        /// Number of elements available in current segment
        available: usize = prealloc,
        /// Next index to allocate
        next_idx: usize = 0,

        /// Get an element from the pool, growing if necessary
        pub fn getGrow(self: *Self, allocator: std.mem.Allocator) !*T {
            if (self.available == 0) {
                try self.grow(allocator);
            }
            return self.get();
        }

        /// Get an element from the pool without growing
        /// Returns error if no elements available
        pub fn get(self: *Self) error{OutOfValues}!*T {
            if (self.available == 0) return error.OutOfValues;

            self.available -= 1;
            const idx = self.next_idx;
            self.next_idx += 1;
            self.len += 1;

            return self.list.at(idx);
        }

        /// Grow the pool by allocating another segment
        fn grow(self: *Self, allocator: std.mem.Allocator) !void {
            // SegmentedList grows automatically, we just need to ensure capacity
            try self.list.ensureTotalCapacity(allocator, self.next_idx + prealloc);
            self.available = prealloc;
        }

        /// Return an element to the pool (marks it as reusable)
        /// Note: This doesn't actually free memory, just allows reuse
        pub fn put(self: *Self, ptr: *T) void {
            _ = ptr;
            // In the simple version, we don't track free slots
            // A more sophisticated version could maintain a free list
            self.len -= 1;
        }

        /// Get the number of elements currently in use
        pub fn count(self: Self) usize {
            return self.len;
        }

        /// Get the total capacity (allocated slots)
        pub fn capacity(self: Self) usize {
            return self.next_idx + self.available;
        }

        /// Reset the pool (all elements become available)
        /// Does not free memory
        pub fn reset(self: *Self) void {
            self.len = 0;
            self.next_idx = 0;
            self.available = prealloc;
        }

        /// Free all allocated memory
        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.list.deinit(allocator);
            self.* = .{};
        }

        /// Iterate over all allocated elements
        pub fn iterator(self: *Self) Iterator {
            return .{
                .pool = self,
                .idx = 0,
            };
        }

        pub const Iterator = struct {
            pool: *Self,
            idx: usize,

            pub fn next(self: *Iterator) ?*T {
                if (self.idx >= self.pool.len) return null;
                const ptr = self.pool.list.at(self.idx);
                self.idx += 1;
                return ptr;
            }

            pub fn reset(self: *Iterator) void {
                self.idx = 0;
            }
        };
    };
}

// ============================================
// Inline Tests (Ghostty pattern)
// ============================================

test "segmented_pool: basic allocation" {
    const allocator = std.testing.allocator;
    var pool: SegmentedPool(u64, 4) = .{};
    defer pool.deinit(allocator);

    // Allocate some elements
    const a = try pool.getGrow(allocator);
    a.* = 100;

    const b = try pool.getGrow(allocator);
    b.* = 200;

    try std.testing.expectEqual(@as(usize, 2), pool.count());
    try std.testing.expectEqual(@as(u64, 100), a.*);
    try std.testing.expectEqual(@as(u64, 200), b.*);
}

test "segmented_pool: pointer stability across growth" {
    const allocator = std.testing.allocator;
    var pool: SegmentedPool(u64, 2) = .{};
    defer pool.deinit(allocator);

    // Fill first segment
    const a = try pool.getGrow(allocator);
    a.* = 1;
    const b = try pool.getGrow(allocator);
    b.* = 2;

    // This should trigger growth
    const c = try pool.getGrow(allocator);
    c.* = 3;
    const d = try pool.getGrow(allocator);
    d.* = 4;

    // Original pointers should still be valid
    try std.testing.expectEqual(@as(u64, 1), a.*);
    try std.testing.expectEqual(@as(u64, 2), b.*);
    try std.testing.expectEqual(@as(u64, 3), c.*);
    try std.testing.expectEqual(@as(u64, 4), d.*);
}

test "segmented_pool: get without grow" {
    var pool: SegmentedPool(u64, 2) = .{};

    // Should succeed for preallocated elements
    _ = try pool.get();
    _ = try pool.get();

    // Should fail when exhausted
    try std.testing.expectError(error.OutOfValues, pool.get());
}

test "segmented_pool: reset" {
    const allocator = std.testing.allocator;
    var pool: SegmentedPool(u64, 4) = .{};
    defer pool.deinit(allocator);

    _ = try pool.getGrow(allocator);
    _ = try pool.getGrow(allocator);
    try std.testing.expectEqual(@as(usize, 2), pool.count());

    pool.reset();
    try std.testing.expectEqual(@as(usize, 0), pool.count());
}

test "segmented_pool: iterator" {
    const allocator = std.testing.allocator;
    var pool: SegmentedPool(u64, 4) = .{};
    defer pool.deinit(allocator);

    const a = try pool.getGrow(allocator);
    a.* = 10;
    const b = try pool.getGrow(allocator);
    b.* = 20;
    const c = try pool.getGrow(allocator);
    c.* = 30;

    var sum: u64 = 0;
    var iter = pool.iterator();
    while (iter.next()) |ptr| {
        sum += ptr.*;
    }

    try std.testing.expectEqual(@as(u64, 60), sum);
}

test "segmented_pool: struct elements" {
    const TestStruct = struct {
        id: u32,
        value: f64,
        name: []const u8,
    };

    const allocator = std.testing.allocator;
    var pool: SegmentedPool(TestStruct, 8) = .{};
    defer pool.deinit(allocator);

    const item = try pool.getGrow(allocator);
    item.* = .{
        .id = 42,
        .value = 3.14,
        .name = "test",
    };

    try std.testing.expectEqual(@as(u32, 42), item.id);
    try std.testing.expectEqual(@as(f64, 3.14), item.value);
    try std.testing.expectEqualStrings("test", item.name);
}
