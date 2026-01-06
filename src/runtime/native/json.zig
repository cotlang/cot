//! JSON Parsing and Serialization
//!
//! Namespace: std.json
//! Functions: parse, stringify, stringify_pretty
//!
//! Example usage:
//!   import std.json
//!   var obj = std.json.parse('{"name": "alice", "age": 30}')
//!   var name = obj.get("name")
//!   var json_str = std.json.stringify(obj)

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

/// Register all JSON functions
pub fn register(registry: anytype) !void {
    // Namespaced names (std.json.*)
    try registry.registerNative("std.json.parse", json_parse);
    try registry.registerNative("std.json.stringify", json_stringify);
    try registry.registerNative("std.json.stringify_pretty", json_stringify_pretty);

    // Short names for convenience
    try registry.registerNative("json_parse", json_parse);
    try registry.registerNative("json_stringify", json_stringify);
}

/// Parse a JSON string into a Cot Map value
/// json_parse(json_string) -> Map or null on error
pub fn json_parse(ctx: *NativeContext) NativeError!?Value {
    const json_str = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    debug.print(.general, "json_parse: parsing '{s}'", .{json_str});

    // Parse the JSON
    const parsed = std.json.parseFromSlice(std.json.Value, ctx.allocator, json_str, .{}) catch |err| {
        debug.print(.general, "json_parse: parse error: {}", .{err});
        return Value.initNull();
    };
    defer parsed.deinit();

    // Convert JSON value to Cot Value
    return jsonToCotValue(ctx.allocator, parsed.value) catch |err| {
        debug.print(.general, "json_parse: conversion error: {}", .{err});
        return Value.initNull();
    };
}

/// Convert a JSON string to a pretty-printed JSON string (with formatting)
/// stringify_pretty(value, indent) -> string
pub fn json_stringify_pretty(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const indent_count = ctx.getArgInt(1) catch 2;

    // For now, just use non-pretty stringify (TODO: implement proper pretty printing)
    _ = indent_count;
    return json_stringify_impl(ctx.allocator, val);
}

/// Stringify a Cot value to JSON string
/// json_stringify(value) -> string
pub fn json_stringify(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;

    debug.print(.general, "json_stringify: converting value", .{});

    return json_stringify_impl(ctx.allocator, val);
}

fn json_stringify_impl(allocator: std.mem.Allocator, val: Value) NativeError!?Value {
    const tag = val.tag();

    var buf: [64]u8 = undefined;
    const result: []const u8 = switch (tag) {
        .null_val => "null",
        .boolean => if (val.asBool()) "true" else "false",
        .integer => {
            const str = std.fmt.bufPrint(&buf, "{d}", .{val.toInt()}) catch "0";
            const dup = allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
            return Value.initString(allocator, dup) catch return NativeError.OutOfMemory;
        },
        .implied_decimal => {
            if (val.asDecimal()) |d| {
                const scale: f64 = @floatFromInt(std.math.powi(u64, 10, @intCast(d.precision)) catch 1);
                const f: f64 = @as(f64, @floatFromInt(d.value)) / scale;
                const str = std.fmt.bufPrint(&buf, "{d}", .{f}) catch "0";
                const dup = allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
                return Value.initString(allocator, dup) catch return NativeError.OutOfMemory;
            }
            const dup = allocator.dupe(u8, "0") catch return NativeError.OutOfMemory;
            return Value.initString(allocator, dup) catch return NativeError.OutOfMemory;
        },
        .string, .fixed_string => {
            const str = val.toString();
            // If the string looks like JSON, return it as-is
            if (str.len > 0 and (str[0] == '{' or str[0] == '[')) {
                const dup = allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
                return Value.initString(allocator, dup) catch return NativeError.OutOfMemory;
            }
            // Otherwise, wrap in quotes and escape
            var list: std.ArrayListUnmanaged(u8) = .empty;
            defer list.deinit(allocator);

            list.append(allocator, '"') catch return NativeError.OutOfMemory;
            for (str) |c| {
                switch (c) {
                    '"' => list.appendSlice(allocator, "\\\"") catch return NativeError.OutOfMemory,
                    '\\' => list.appendSlice(allocator, "\\\\") catch return NativeError.OutOfMemory,
                    '\n' => list.appendSlice(allocator, "\\n") catch return NativeError.OutOfMemory,
                    '\r' => list.appendSlice(allocator, "\\r") catch return NativeError.OutOfMemory,
                    '\t' => list.appendSlice(allocator, "\\t") catch return NativeError.OutOfMemory,
                    else => list.append(allocator, c) catch return NativeError.OutOfMemory,
                }
            }
            list.append(allocator, '"') catch return NativeError.OutOfMemory;

            const escaped = list.toOwnedSlice(allocator) catch return NativeError.OutOfMemory;
            return Value.initString(allocator, escaped) catch return NativeError.OutOfMemory;
        },
        else => "null",
    };

    const dup = allocator.dupe(u8, result) catch return NativeError.OutOfMemory;
    return Value.initString(allocator, dup) catch return NativeError.OutOfMemory;
}

/// Convert a std.json.Value to a Cot Value
fn jsonToCotValue(allocator: std.mem.Allocator, json_val: std.json.Value) !Value {
    return switch (json_val) {
        .null => Value.initNull(),
        .bool => |b| Value.initBool(b),
        .integer => |i| Value.initInt(i),
        .float => |f| blk: {
            // Convert float to decimal with 6 decimal places precision
            const scaled: i64 = @intFromFloat(f * 1_000_000.0);
            break :blk try Value.initDecimal(allocator, scaled, 6);
        },
        .string => |s| Value.initString(allocator, s) catch return error.OutOfMemory,
        .array => |arr| {
            // For arrays, create a JSON array string representation
            var list: std.ArrayListUnmanaged(u8) = .empty;
            defer list.deinit(allocator);

            try list.appendSlice(allocator, "[");
            for (arr.items, 0..) |item, i| {
                if (i > 0) try list.appendSlice(allocator, ",");
                const item_str = try jsonValueToString(allocator, item);
                defer allocator.free(item_str);
                try list.appendSlice(allocator, item_str);
            }
            try list.appendSlice(allocator, "]");

            const result = try list.toOwnedSlice(allocator);
            return Value.initString(allocator, result) catch return error.OutOfMemory;
        },
        .object => |obj| {
            // Create a JSON object string representation
            var list: std.ArrayListUnmanaged(u8) = .empty;
            defer list.deinit(allocator);

            try list.appendSlice(allocator, "{");
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) try list.appendSlice(allocator, ",");
                first = false;

                // Add key
                try list.appendSlice(allocator, "\"");
                try list.appendSlice(allocator, entry.key_ptr.*);
                try list.appendSlice(allocator, "\":");

                // Add value
                const val_str = try jsonValueToString(allocator, entry.value_ptr.*);
                defer allocator.free(val_str);
                try list.appendSlice(allocator, val_str);
            }
            try list.appendSlice(allocator, "}");

            const result = try list.toOwnedSlice(allocator);
            return Value.initString(allocator, result) catch return error.OutOfMemory;
        },
        .number_string => |s| {
            // Try to parse as integer first, then as decimal
            if (std.fmt.parseInt(i64, s, 10)) |i| {
                return Value.initInt(i);
            } else |_| {
                const f = std.fmt.parseFloat(f64, s) catch 0.0;
                const scaled: i64 = @intFromFloat(f * 1_000_000.0);
                return try Value.initDecimal(allocator, scaled, 6);
            }
        },
    };
}

/// Convert a JSON value to its string representation
fn jsonValueToString(allocator: std.mem.Allocator, json_val: std.json.Value) ![]u8 {
    var list: std.ArrayListUnmanaged(u8) = .empty;
    errdefer list.deinit(allocator);

    switch (json_val) {
        .null => try list.appendSlice(allocator, "null"),
        .bool => |b| try list.appendSlice(allocator, if (b) "true" else "false"),
        .integer => |i| {
            var buf: [32]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{i}) catch "0";
            try list.appendSlice(allocator, str);
        },
        .float => |f| {
            var buf: [64]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "{d}", .{f}) catch "0";
            try list.appendSlice(allocator, str);
        },
        .string => |s| {
            try list.append(allocator, '"');
            for (s) |c| {
                switch (c) {
                    '"' => try list.appendSlice(allocator, "\\\""),
                    '\\' => try list.appendSlice(allocator, "\\\\"),
                    '\n' => try list.appendSlice(allocator, "\\n"),
                    '\r' => try list.appendSlice(allocator, "\\r"),
                    '\t' => try list.appendSlice(allocator, "\\t"),
                    else => try list.append(allocator, c),
                }
            }
            try list.append(allocator, '"');
        },
        .array => |arr| {
            try list.append(allocator, '[');
            for (arr.items, 0..) |item, i| {
                if (i > 0) try list.append(allocator, ',');
                const item_str = try jsonValueToString(allocator, item);
                defer allocator.free(item_str);
                try list.appendSlice(allocator, item_str);
            }
            try list.append(allocator, ']');
        },
        .object => |obj| {
            try list.append(allocator, '{');
            var first = true;
            var iter = obj.iterator();
            while (iter.next()) |entry| {
                if (!first) try list.append(allocator, ',');
                first = false;
                try list.append(allocator, '"');
                try list.appendSlice(allocator, entry.key_ptr.*);
                try list.appendSlice(allocator, "\":");
                const val_str = try jsonValueToString(allocator, entry.value_ptr.*);
                defer allocator.free(val_str);
                try list.appendSlice(allocator, val_str);
            }
            try list.append(allocator, '}');
        },
        .number_string => |s| try list.appendSlice(allocator, s),
    }

    return list.toOwnedSlice(allocator);
}

// Tests
test "json parse simple object" {
    const allocator = std.testing.allocator;
    var ctx = NativeContext{
        .allocator = allocator,
        .args = &[_]Value{},
    };

    // Create a test JSON string value
    const json_str = try Value.initString(allocator, "{\"name\":\"alice\"}");
    defer json_str.deinit(allocator);

    ctx.args = &[_]Value{json_str};

    const result = try json_parse(&ctx);
    try std.testing.expect(result != null);
}
