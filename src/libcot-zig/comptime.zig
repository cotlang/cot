//! Compile-time value system for structured constants.
//!
//! Holds values computed during compile-time evaluation: integers, floats,
//! strings, booleans, arrays, enum field info, and type info. Used by the
//! checker for const folding and by the lowerer for generic monomorphization.

const std = @import("std");

/// A value known at compile time. The checker produces these during
/// constant expression evaluation; the lowerer consumes them for
/// specialization and code generation.
pub const ComptimeValue = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    boolean: bool,
    array: ComptimeArray,
    enum_field: EnumFieldInfo,
    type_info: TypeInfo,
    undefined_val,

    /// Compile-time array with element type name for typed init.
    pub const ComptimeArray = struct {
        elements: std.ArrayListUnmanaged(ComptimeValue),
        elem_type_name: []const u8,
    };

    /// Enum variant name and backing integer value.
    pub const EnumFieldInfo = struct {
        name: []const u8,
        value: i64,
    };

    /// Reflection data returned by @typeInfo.
    pub const TypeInfo = struct {
        kind: TypeInfoKind,
        name: []const u8,
        fields: std.ArrayListUnmanaged(ComptimeValue),
    };

    pub const TypeInfoKind = enum {
        enum_info,
    };

    /// Extract integer value. Returns 1/0 for booleans.
    pub fn asInt(self: ComptimeValue) ?i64 {
        return switch (self) {
            .int => |v| v,
            .boolean => |b| if (b) @as(i64, 1) else @as(i64, 0),
            else => null,
        };
    }

    /// Extract string value.
    pub fn asString(self: ComptimeValue) ?[]const u8 {
        return switch (self) {
            .string => |s| s,
            else => null,
        };
    }

    /// Extract float value.
    pub fn asFloat(self: ComptimeValue) ?f64 {
        return switch (self) {
            .float => |f| f,
            else => null,
        };
    }

    /// Construct an integer ComptimeValue.
    pub fn makeInt(v: i64) ComptimeValue {
        return .{ .int = v };
    }

    /// Construct a string ComptimeValue.
    pub fn makeString(s: []const u8) ComptimeValue {
        return .{ .string = s };
    }

    /// Construct a boolean ComptimeValue.
    pub fn makeBool(b: bool) ComptimeValue {
        return .{ .boolean = b };
    }
};

test "asInt extracts integers and booleans" {
    try std.testing.expectEqual(@as(?i64, 42), ComptimeValue.makeInt(42).asInt());
    try std.testing.expectEqual(@as(?i64, 1), (ComptimeValue{ .boolean = true }).asInt());
    try std.testing.expectEqual(@as(?i64, 0), (ComptimeValue{ .boolean = false }).asInt());
    try std.testing.expect((ComptimeValue{ .string = "hello" }).asInt() == null);
}

test "asString extracts strings" {
    try std.testing.expectEqualStrings("hello", ComptimeValue.makeString("hello").asString().?);
    try std.testing.expect(ComptimeValue.makeInt(1).asString() == null);
}

test "asFloat extracts floats" {
    try std.testing.expectEqual(@as(?f64, 3.14), (ComptimeValue{ .float = 3.14 }).asFloat());
    try std.testing.expect(ComptimeValue.makeInt(1).asFloat() == null);
}

test "undefined_val returns null for all accessors" {
    const undef: ComptimeValue = .undefined_val;
    try std.testing.expect(undef.asInt() == null);
    try std.testing.expect(undef.asString() == null);
    try std.testing.expect(undef.asFloat() == null);
}
