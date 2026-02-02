//! A basic Variable implementation.
//!
//! Port of cranelift-frontend/src/variable.rs
//!
//! Frontends can use any indexing scheme they see fit and
//! generate the appropriate Variable instances.
//!
//! Note: The Variable is used to index into densely allocated
//! arrays containing information about your mutable variables.
//! Thus, make sure that Variable's indexes are allocated contiguously and
//! starting at 0.

const std = @import("std");

/// An opaque reference to a variable.
///
/// Variables are used by FunctionBuilder to track mutable local variables
/// that are translated to SSA form automatically.
pub const Variable = struct {
    index: u32,

    const Self = @This();

    pub const RESERVED: Variable = .{ .index = std.math.maxInt(u32) };

    /// Create a new variable from an index.
    pub fn new(index: u32) Self {
        return .{ .index = index };
    }

    /// Create a variable from a u32 (alias for new).
    pub fn fromU32(index: u32) Self {
        return new(index);
    }

    /// Get the index of this variable.
    pub fn asU32(self: Self) u32 {
        return self.index;
    }

    /// Check equality.
    pub fn eql(self: Self, other: Self) bool {
        return self.index == other.index;
    }

    /// Check if this is a valid (non-reserved) variable.
    pub fn isValid(self: Self) bool {
        return self.index != RESERVED.index;
    }

    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("var{d}", .{self.index});
    }
};

// ============================================================================
// Tests
// ============================================================================

test "variable basics" {
    const testing = std.testing;

    const v0 = Variable.new(0);
    const v1 = Variable.new(1);
    const v0_copy = Variable.new(0);

    try testing.expect(v0.eql(v0_copy));
    try testing.expect(!v0.eql(v1));
    try testing.expectEqual(@as(u32, 0), v0.asU32());
    try testing.expectEqual(@as(u32, 1), v1.asU32());
    try testing.expect(v0.isValid());
    try testing.expect(!Variable.RESERVED.isValid());
}

test "variable formatting" {
    const testing = std.testing;
    var buf: [32]u8 = undefined;

    const v = Variable.new(42);
    var fbs = std.io.fixedBufferStream(&buf);
    try v.format("", .{}, fbs.writer());
    try testing.expectEqualStrings("var42", fbs.getWritten());
}
