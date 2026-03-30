/// Comptime value system — rich structured values for compile-time evaluation.
///
/// Reference: Zig's InternPool.MutableValue holds comptime state during analysis.
/// Cot uses a simpler tagged union since we don't have ZIR.
///
/// This replaces the `?i64`-only comptime system with support for arrays, strings,
/// structs, and enum field info.
const std = @import("std");

pub const ComptimeValue = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    boolean: bool,
    array: ComptimeArray,
    enum_field: EnumFieldInfo,
    type_info: TypeInfo,
    undefined_val,

    pub const ComptimeArray = struct {
        elements: std.ArrayListUnmanaged(ComptimeValue),
        elem_type_name: []const u8,
    };

    pub const EnumFieldInfo = struct {
        name: []const u8,
        value: i64,
    };

    pub const TypeInfo = struct {
        kind: TypeInfoKind,
        name: []const u8,
        fields: std.ArrayListUnmanaged(ComptimeValue),
    };

    pub const TypeInfoKind = enum {
        enum_info,
    };

    /// Extract integer value (backward compat with evalConstExpr)
    pub fn asInt(self: ComptimeValue) ?i64 {
        return switch (self) {
            .int => |v| v,
            .boolean => |b| if (b) @as(i64, 1) else @as(i64, 0),
            else => null,
        };
    }

    /// Extract string value (backward compat with evalConstString)
    pub fn asString(self: ComptimeValue) ?[]const u8 {
        return switch (self) {
            .string => |s| s,
            else => null,
        };
    }

    /// Extract float value
    pub fn asFloat(self: ComptimeValue) ?f64 {
        return switch (self) {
            .float => |f| f,
            else => null,
        };
    }
};
