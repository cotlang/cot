//! Math native functions
//!
//! abs, sqrt, sin, cos, tan, log, log10, exp, round, trunc

const std = @import("std");
const native = @import("native.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;

/// Register all math functions
pub fn register(registry: anytype) !void {
    try registry.registerNative("abs", abs);
    try registry.registerNative("sqrt", sqrt);
    try registry.registerNative("sin", sin);
    try registry.registerNative("cos", cos);
    try registry.registerNative("tan", tan);
    try registry.registerNative("log", log);
    try registry.registerNative("log10", log10);
    try registry.registerNative("exp", exp);
    try registry.registerNative("round", round);
    try registry.registerNative("trunc", trunc);
}

/// ABS - Absolute value
pub fn abs(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const num = val.toInt();
    return Value.initInt(if (num < 0) -num else num);
}

/// SQRT - Square root
pub fn sqrt(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const num = val.toInt();
    const f: f64 = @floatFromInt(num);
    const result = @sqrt(f);
    const scaled: i64 = @intFromFloat(result * 10000);
    return Value.initDecimal(ctx.allocator, scaled, 4) catch return NativeError.OutOfMemory;
}

/// SIN - Sine (input in degrees)
pub fn sin(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const deg = val.toInt();
    const rad: f64 = @as(f64, @floatFromInt(deg)) * std.math.pi / 180.0;
    const result = @sin(rad);
    const scaled: i64 = @intFromFloat(result * 10000);
    return Value.initDecimal(ctx.allocator, scaled, 4) catch return NativeError.OutOfMemory;
}

/// COS - Cosine (input in degrees)
pub fn cos(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const deg = val.toInt();
    const rad: f64 = @as(f64, @floatFromInt(deg)) * std.math.pi / 180.0;
    const result = @cos(rad);
    const scaled: i64 = @intFromFloat(result * 10000);
    return Value.initDecimal(ctx.allocator, scaled, 4) catch return NativeError.OutOfMemory;
}

/// TAN - Tangent (input in degrees)
pub fn tan(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const deg = val.toInt();
    const rad: f64 = @as(f64, @floatFromInt(deg)) * std.math.pi / 180.0;
    const result = @tan(rad);
    const scaled: i64 = @intFromFloat(result * 10000);
    return Value.initDecimal(ctx.allocator, scaled, 4) catch return NativeError.OutOfMemory;
}

/// LOG - Natural logarithm
pub fn log(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const num = val.toInt();
    if (num <= 0) return Value.initDecimal(ctx.allocator, 0, 4) catch return NativeError.OutOfMemory;
    const f: f64 = @floatFromInt(num);
    const result = @log(f);
    const scaled: i64 = @intFromFloat(result * 10000);
    return Value.initDecimal(ctx.allocator, scaled, 4) catch return NativeError.OutOfMemory;
}

/// LOG10 - Base-10 logarithm
pub fn log10(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const num = val.toInt();
    if (num <= 0) return Value.initDecimal(ctx.allocator, 0, 4) catch return NativeError.OutOfMemory;
    const f: f64 = @floatFromInt(num);
    const result = std.math.log10(f);
    const scaled: i64 = @intFromFloat(result * 10000);
    return Value.initDecimal(ctx.allocator, scaled, 4) catch return NativeError.OutOfMemory;
}

/// EXP - Exponential (e^x)
pub fn exp(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const num = val.toInt();
    const f: f64 = @floatFromInt(num);
    const result = @exp(f);
    const scaled: i64 = @intFromFloat(result * 10000);
    return Value.initDecimal(ctx.allocator, scaled, 4) catch return NativeError.OutOfMemory;
}

/// ROUND - Round to specified decimal places
pub fn round(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const precision: u8 = if (ctx.args.len > 1)
        @intCast((ctx.getArgInt(1) catch 0))
    else
        0;

    const num = val.toInt();
    if (precision == 0) {
        return Value.initInt(num);
    }
    return Value.initDecimal(ctx.allocator, num, precision) catch return NativeError.OutOfMemory;
}

/// TRUNC - Truncate to integer
pub fn trunc(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const num = val.toInt();
    return Value.initInt(num);
}
