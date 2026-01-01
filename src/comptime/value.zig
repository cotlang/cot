//! Compile-time value types
//!
//! Values that can be computed and stored at compile time.

const std = @import("std");

/// A value that exists at compile time
pub const Value = union(enum) {
    /// Boolean value (true/false)
    boolean: bool,

    /// Integer value
    integer: i64,

    /// Decimal value (stored as scaled integer)
    decimal: struct {
        value: i64,
        scale: u8, // decimal places
    },

    /// String/alpha value
    string: []const u8,

    /// Undefined/null
    undefined: void,

    /// Check if this value is truthy
    pub fn isTruthy(self: Value) bool {
        return switch (self) {
            .boolean => |b| b,
            .integer => |i| i != 0,
            .decimal => |d| d.value != 0,
            .string => |s| s.len > 0,
            .undefined => false,
        };
    }

    /// Convert to boolean
    pub fn toBool(self: Value) bool {
        return self.isTruthy();
    }

    /// Convert to integer
    pub fn toInt(self: Value) i64 {
        return switch (self) {
            .boolean => |b| if (b) @as(i64, 1) else @as(i64, 0),
            .integer => |i| i,
            .decimal => |d| @divTrunc(d.value, std.math.pow(i64, 10, d.scale)),
            .string => |s| std.fmt.parseInt(i64, s, 10) catch 0,
            .undefined => 0,
        };
    }

    /// Convert to string representation
    pub fn toString(self: Value, allocator: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .boolean => |b| if (b) "true" else "false",
            .integer => |i| try std.fmt.allocPrint(allocator, "{d}", .{i}),
            .decimal => |d| try std.fmt.allocPrint(allocator, "{d}", .{d.value}), // TODO: proper decimal formatting
            .string => |s| s,
            .undefined => "",
        };
    }

    /// Check equality
    pub fn eql(self: Value, other: Value) bool {
        return switch (self) {
            .boolean => |b| switch (other) {
                .boolean => |ob| b == ob,
                else => b == other.toBool(),
            },
            .integer => |i| switch (other) {
                .integer => |oi| i == oi,
                .boolean => |ob| i == (if (ob) @as(i64, 1) else @as(i64, 0)),
                else => false,
            },
            .decimal => |d| switch (other) {
                .decimal => |od| d.value == od.value and d.scale == od.scale,
                else => false,
            },
            .string => |s| switch (other) {
                .string => |os| std.mem.eql(u8, s, os),
                else => false,
            },
            .undefined => switch (other) {
                .undefined => true,
                else => false,
            },
        };
    }

    /// Compare values (for <, >, <=, >=)
    pub fn compare(self: Value, other: Value) std.math.Order {
        const a = self.toInt();
        const b = other.toInt();
        return std.math.order(a, b);
    }

    /// Create a boolean value
    pub fn fromBool(b: bool) Value {
        return .{ .boolean = b };
    }

    /// Create an integer value
    pub fn fromInt(i: i64) Value {
        return .{ .integer = i };
    }

    /// Create a string value
    pub fn fromString(s: []const u8) Value {
        return .{ .string = s };
    }
};

test "value truthiness" {
    try std.testing.expect(Value.fromBool(true).isTruthy());
    try std.testing.expect(!Value.fromBool(false).isTruthy());
    try std.testing.expect(Value.fromInt(1).isTruthy());
    try std.testing.expect(!Value.fromInt(0).isTruthy());
    try std.testing.expect(Value.fromString("hello").isTruthy());
    try std.testing.expect(!Value.fromString("").isTruthy());
}

test "value equality" {
    try std.testing.expect(Value.fromBool(true).eql(Value.fromBool(true)));
    try std.testing.expect(Value.fromInt(42).eql(Value.fromInt(42)));
    try std.testing.expect(Value.fromString("hello").eql(Value.fromString("hello")));
}
