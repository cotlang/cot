//! Type conversion native functions
//!
//! string, integer, decimal, char, alpha, boolean

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;

/// Register all conversion functions
pub fn register(registry: anytype) !void {
    try registry.registerNative("string", string);
    try registry.registerNative("integer", integer);
    try registry.registerNative("decimal", decimal);
    try registry.registerNative("char", char);
    try registry.registerNative("alpha", alpha);
    try registry.registerNative("boolean", boolean);
}

/// STRING - Convert to string
pub fn string(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;

    debug.print(.vm, "string(): input tag={s}", .{@tagName(val.tag())});

    // Handle different value types
    switch (val.tag()) {
        .integer => {
            // Format integer as string
            var buf: [32]u8 = undefined;
            const int_val = val.asInt();
            const str = std.fmt.bufPrint(&buf, "{d}", .{int_val}) catch return NativeError.OutOfMemory;
            const result = ctx.allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
            return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
        },
        .decimal => {
            // Format decimal as string
            if (val.asDecimal()) |d| {
                var buf: [64]u8 = undefined;
                const precision = d.precision;
                if (precision > 0) {
                    // Format with decimal point
                    const scale: u64 = std.math.powi(u64, 10, @intCast(precision)) catch 1;
                    const abs_val: u64 = @abs(d.value);
                    const int_part = @divTrunc(abs_val, scale);
                    const frac_part = @mod(abs_val, scale);
                    const sign: []const u8 = if (d.value < 0) "-" else "";
                    const str = std.fmt.bufPrint(&buf, "{s}{d}.{d:0>[3]}", .{ sign, int_part, frac_part, precision }) catch return NativeError.OutOfMemory;
                    const result = ctx.allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
                    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
                } else {
                    // Integer decimal
                    const str = std.fmt.bufPrint(&buf, "{d}", .{d.value}) catch return NativeError.OutOfMemory;
                    const result = ctx.allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
                    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
                }
            }
            return Value.initFixedString(ctx.allocator, ctx.allocator.dupe(u8, "0") catch return NativeError.OutOfMemory) catch return NativeError.OutOfMemory;
        },
        .boolean => {
            const str = if (val.asBool()) "true" else "false";
            const result = ctx.allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
            return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
        },
        .fixed_string, .string => {
            const str = val.asString();
            const result = ctx.allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
            return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
        },
        else => {
            // Return empty string for unknown types
            const result = ctx.allocator.dupe(u8, "") catch return NativeError.OutOfMemory;
            return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
        },
    }
}

/// INTEGER - Convert to integer
pub fn integer(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const num = val.toInt();
    return Value.initInt(num);
}

/// DECIMAL - Convert to decimal
pub fn decimal(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const precision: u8 = if (ctx.args.len > 1)
        @intCast((ctx.getArgInt(1) catch 2))
    else
        2;
    const num = val.toInt();
    return Value.initDecimal(ctx.allocator, num, precision) catch return NativeError.OutOfMemory;
}

/// CHAR - Convert integer to character
pub fn char(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const code = val.toInt();
    const result = ctx.allocator.alloc(u8, 1) catch return NativeError.OutOfMemory;
    result[0] = @intCast(@mod(code, 256));
    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// ALPHA - Convert to alpha (string)
pub fn alpha(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const str = val.asString();
    const result = ctx.allocator.dupe(u8, str) catch return NativeError.OutOfMemory;
    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// BOOLEAN - Convert to boolean (as integer 0 or 1)
pub fn boolean(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const b = val.toBool();
    return Value.initInt(if (b) 1 else 0);
}
