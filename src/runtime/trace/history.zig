//! Ring buffer for execution history
//!
//! Fixed-size circular buffer that efficiently stores the last N events.
//! Used for crash dumps and post-mortem debugging.
//!
//! The buffer overwrites oldest entries when full, providing O(1) push
//! and O(n) iteration from oldest to newest.

const std = @import("std");

/// Generic ring buffer for storing execution history
pub fn History(comptime T: type) type {
    return struct {
        const Self = @This();

        /// Storage for entries
        buffer: []T,

        /// Next write position (0 to capacity-1)
        head: usize = 0,

        /// Number of valid entries (0 to capacity)
        count: usize = 0,

        /// Allocator for buffer memory
        allocator: std.mem.Allocator,

        /// Whether buffer was allocated (vs empty slice)
        owns_buffer: bool = false,

        /// Initialize with given capacity
        pub fn init(allocator: std.mem.Allocator, initial_capacity: u16) Self {
            if (initial_capacity == 0) {
                return .{
                    .buffer = &[_]T{},
                    .allocator = allocator,
                    .owns_buffer = false,
                };
            }

            const buffer = allocator.alloc(T, initial_capacity) catch {
                return .{
                    .buffer = &[_]T{},
                    .allocator = allocator,
                    .owns_buffer = false,
                };
            };

            return .{
                .buffer = buffer,
                .allocator = allocator,
                .owns_buffer = true,
            };
        }

        /// Free buffer memory
        pub fn deinit(self: *Self) void {
            if (self.owns_buffer and self.buffer.len > 0) {
                self.allocator.free(self.buffer);
            }
            self.buffer = &[_]T{};
            self.owns_buffer = false;
            self.head = 0;
            self.count = 0;
        }

        /// Push an item, overwriting oldest if full
        pub fn push(self: *Self, item: T) void {
            if (self.buffer.len == 0) return;

            self.buffer[self.head] = item;
            self.head = (self.head + 1) % self.buffer.len;
            if (self.count < self.buffer.len) {
                self.count += 1;
            }
        }

        /// Number of valid entries
        pub fn len(self: *const Self) usize {
            return self.count;
        }

        /// Buffer capacity
        pub fn capacity(self: *const Self) usize {
            return self.buffer.len;
        }

        /// Check if buffer is empty
        pub fn isEmpty(self: *const Self) bool {
            return self.count == 0;
        }

        /// Check if buffer is full
        pub fn isFull(self: *const Self) bool {
            return self.count == self.buffer.len;
        }

        /// Get most recent entry (last pushed)
        pub fn last(self: *const Self) ?T {
            if (self.count == 0) return null;
            const idx = if (self.head == 0) self.buffer.len - 1 else self.head - 1;
            return self.buffer[idx];
        }

        /// Get entry at index (0 = oldest, count-1 = newest)
        pub fn get(self: *const Self, index: usize) ?T {
            if (index >= self.count) return null;

            const start = self.oldestIndex();
            const idx = (start + index) % self.buffer.len;
            return self.buffer[idx];
        }

        /// Index of oldest entry in buffer
        fn oldestIndex(self: *const Self) usize {
            if (self.count < self.buffer.len) {
                return 0;
            }
            return self.head; // head points to next write = oldest entry
        }

        /// Iterator from oldest to newest
        pub fn iterator(self: *const Self) Iterator {
            return .{
                .history = self,
                .pos = 0,
            };
        }

        /// Iterator from newest to oldest
        pub fn reverseIterator(self: *const Self) ReverseIterator {
            return .{
                .history = self,
                .remaining = self.count,
            };
        }

        pub const Iterator = struct {
            history: *const Self,
            pos: usize,

            /// Get next entry (oldest to newest order)
            pub fn next(self: *Iterator) ?T {
                if (self.pos >= self.history.count) return null;

                const item = self.history.get(self.pos);
                self.pos += 1;
                return item;
            }

            /// Reset iterator to beginning
            pub fn reset(self: *Iterator) void {
                self.pos = 0;
            }
        };

        pub const ReverseIterator = struct {
            history: *const Self,
            remaining: usize,

            /// Get next entry (newest to oldest order)
            pub fn next(self: *ReverseIterator) ?T {
                if (self.remaining == 0) return null;

                self.remaining -= 1;
                return self.history.get(self.remaining);
            }

            /// Reset iterator to beginning (newest)
            pub fn reset(self: *ReverseIterator) void {
                self.remaining = self.history.count;
            }
        };

        /// Clear all history
        pub fn clear(self: *Self) void {
            self.head = 0;
            self.count = 0;
        }

        /// Get slice of last N entries (may wrap, returns contiguous slice up to N)
        /// For full access, use iterator instead
        pub fn lastNContiguous(self: *const Self, n: usize) []const T {
            if (self.count == 0 or n == 0) return &[_]T{};

            const actual_n = @min(n, self.count);

            // If head is at the end portion, we might have a contiguous slice
            if (self.head >= actual_n) {
                // Newest entries are contiguous before head
                return self.buffer[self.head - actual_n .. self.head];
            } else if (self.count < self.buffer.len) {
                // Buffer not full, entries are at start
                const start = if (self.count > actual_n) self.count - actual_n else 0;
                return self.buffer[start..self.count];
            } else {
                // Wrapped - return what we can from the end
                return self.buffer[self.buffer.len - (actual_n - self.head) .. self.buffer.len];
            }
        }
    };
}

// ============================================================================
// Tests
// ============================================================================

test "History: basic push and iterate" {
    const allocator = std.testing.allocator;
    var h = History(u32).init(allocator, 4);
    defer h.deinit();

    h.push(1);
    h.push(2);
    h.push(3);

    try std.testing.expectEqual(@as(usize, 3), h.len());

    var iter = h.iterator();
    try std.testing.expectEqual(@as(?u32, 1), iter.next());
    try std.testing.expectEqual(@as(?u32, 2), iter.next());
    try std.testing.expectEqual(@as(?u32, 3), iter.next());
    try std.testing.expectEqual(@as(?u32, null), iter.next());
}

test "History: overflow wraps correctly" {
    const allocator = std.testing.allocator;
    var h = History(u32).init(allocator, 3);
    defer h.deinit();

    h.push(1);
    h.push(2);
    h.push(3);
    h.push(4); // Overwrites 1
    h.push(5); // Overwrites 2

    try std.testing.expectEqual(@as(usize, 3), h.len());
    try std.testing.expect(h.isFull());

    var iter = h.iterator();
    try std.testing.expectEqual(@as(?u32, 3), iter.next());
    try std.testing.expectEqual(@as(?u32, 4), iter.next());
    try std.testing.expectEqual(@as(?u32, 5), iter.next());
    try std.testing.expectEqual(@as(?u32, null), iter.next());
}

test "History: last() returns newest" {
    const allocator = std.testing.allocator;
    var h = History(u32).init(allocator, 4);
    defer h.deinit();

    try std.testing.expectEqual(@as(?u32, null), h.last());

    h.push(10);
    try std.testing.expectEqual(@as(?u32, 10), h.last());

    h.push(20);
    try std.testing.expectEqual(@as(?u32, 20), h.last());

    h.push(30);
    h.push(40);
    h.push(50); // Wraps
    try std.testing.expectEqual(@as(?u32, 50), h.last());
}

test "History: reverse iterator" {
    const allocator = std.testing.allocator;
    var h = History(u32).init(allocator, 4);
    defer h.deinit();

    h.push(1);
    h.push(2);
    h.push(3);

    var iter = h.reverseIterator();
    try std.testing.expectEqual(@as(?u32, 3), iter.next());
    try std.testing.expectEqual(@as(?u32, 2), iter.next());
    try std.testing.expectEqual(@as(?u32, 1), iter.next());
    try std.testing.expectEqual(@as(?u32, null), iter.next());
}

test "History: zero capacity" {
    const allocator = std.testing.allocator;
    var h = History(u32).init(allocator, 0);
    defer h.deinit();

    h.push(1); // Should not crash
    try std.testing.expectEqual(@as(usize, 0), h.len());
    try std.testing.expectEqual(@as(?u32, null), h.last());
}
