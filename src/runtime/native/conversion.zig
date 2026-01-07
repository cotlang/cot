//! Type conversion native functions
//!
//! Namespace: std.convert
//! Functions: string, integer, decimal, char, alpha, boolean, ascii
//! Cast Functions: cast_alpha, cast_decimal, cast_integer (DBL ^a, ^d, ^i operators)
//!
//! In .cot files: requires `import std.convert` then call as `std.convert.string(x)`
//! In .dbl files: available directly as `string(x)` (DBL compatibility)

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;

/// Register all conversion functions with both namespaced and short names
pub fn register(registry: anytype) !void {
    // Namespaced names (std.convert.*)
    try registry.registerNative("std.convert.string", string);
    try registry.registerNative("std.convert.integer", integer);
    try registry.registerNative("std.convert.decimal", decimal);
    try registry.registerNative("std.convert.char", char);
    try registry.registerNative("std.convert.alpha", alpha);
    try registry.registerNative("std.convert.boolean", boolean);
    try registry.registerNative("std.convert.ascii", ascii);
    try registry.registerNative("std.convert.cast_alpha", cast_alpha);
    try registry.registerNative("std.convert.cast_decimal", cast_decimal);
    try registry.registerNative("std.convert.cast_integer", cast_integer);

    // Short names (DBL compatibility)
    try registry.registerNative("string", string);
    try registry.registerNative("str", string); // Common alias for string()
    try registry.registerNative("integer", integer);
    try registry.registerNative("decimal", decimal);
    try registry.registerNative("char", char);
    try registry.registerNative("alpha", alpha);
    try registry.registerNative("boolean", boolean);
    try registry.registerNative("ascii", ascii);
    try registry.registerNative("cast_alpha", cast_alpha);
    try registry.registerNative("cast_decimal", cast_decimal);
    try registry.registerNative("cast_integer", cast_integer);
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
        .implied_decimal => {
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

// ============================================================================
// Cast Functions (DBL ^a, ^d, ^i operators)
// These REINTERPRET bytes as a different type, unlike conversion functions
// ============================================================================

/// CAST_ALPHA (^A) - Pure type cast to alpha/string
/// This is a type-level cast, NOT ASCII code conversion (use %ascii for that)
/// Returns the string representation of the value (same output as %string)
pub fn cast_alpha(ctx: *NativeContext) NativeError!?Value {
    // Cast to alpha is semantically the same as string conversion
    // The difference is type-system level (treating as alpha type)
    return string(ctx);
}

/// CAST_DECIMAL (^D) - Reinterpret bytes as decimal
/// For strings: parse the string as a decimal number
/// For integers: treat as decimal with 0 precision
/// For decimals: return as-is
pub fn cast_decimal(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;

    switch (val.tag()) {
        .fixed_string, .string => {
            // Parse string as decimal - treat ASCII digits as numeric value
            const str = std.mem.trimRight(u8, val.asString(), " ");
            var value: i64 = 0;
            var precision: u8 = 0;
            var seen_dot = false;

            for (str) |c| {
                if (c >= '0' and c <= '9') {
                    value = value * 10 + (c - '0');
                    if (seen_dot) precision += 1;
                } else if (c == '.') {
                    seen_dot = true;
                } else if (c == '-' and value == 0) {
                    // Handle negative sign at start - we'll negate at end
                    continue;
                }
            }
            if (str.len > 0 and str[0] == '-') {
                value = -value;
            }
            return Value.initDecimal(ctx.allocator, value, precision) catch return NativeError.OutOfMemory;
        },
        .integer => {
            // Integer cast to decimal with 0 precision
            const int_val = val.asInt();
            return Value.initDecimal(ctx.allocator, int_val, 0) catch return NativeError.OutOfMemory;
        },
        .implied_decimal => {
            // Already decimal - return as-is
            if (val.asDecimal()) |d| {
                return Value.initDecimal(ctx.allocator, d.value, d.precision) catch return NativeError.OutOfMemory;
            }
            return Value.initDecimal(ctx.allocator, 0, 0) catch return NativeError.OutOfMemory;
        },
        else => {
            return Value.initDecimal(ctx.allocator, 0, 0) catch return NativeError.OutOfMemory;
        },
    }
}

/// CAST_INTEGER (^I) - Reinterpret bytes as integer
/// For strings: parse the string as an integer (ASCII digits to number)
/// For decimals: truncate to integer
/// For integers: return as-is
pub fn cast_integer(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;

    switch (val.tag()) {
        .fixed_string, .string => {
            // Parse string as integer - treat ASCII digits as numeric value
            const str = std.mem.trimRight(u8, val.asString(), " ");
            var value: i64 = 0;
            var negative = false;

            for (str, 0..) |c, i| {
                if (c >= '0' and c <= '9') {
                    value = value * 10 + (c - '0');
                } else if (c == '-' and i == 0) {
                    negative = true;
                } else if (c == '.') {
                    break; // Stop at decimal point
                }
            }
            if (negative) value = -value;
            return Value.initInt(value);
        },
        .implied_decimal => {
            // Truncate decimal to integer
            if (val.asDecimal()) |d| {
                const scale: i64 = std.math.powi(i64, 10, d.precision) catch 1;
                return Value.initInt(@divTrunc(d.value, scale));
            }
            return Value.initInt(0);
        },
        .integer => {
            // Already integer - return as-is
            return Value.initInt(val.asInt());
        },
        .float => {
            // Truncate float to integer
            const f = val.asFloat();
            return Value.initInt(@intFromFloat(f));
        },
        .boolean => {
            // Convert boolean to integer (true -> 1, false -> 0)
            return Value.initInt(if (val.asBool()) 1 else 0);
        },
        else => {
            // Unknown type - return 0
            return Value.initInt(0);
        },
    }
}

/// ASCII - Convert integer to ASCII character (DBL %ASCII)
/// Takes an integer and returns a single character string
pub fn ascii(ctx: *NativeContext) NativeError!?Value {
    const val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const code = val.toInt();
    const result = ctx.allocator.alloc(u8, 1) catch return NativeError.OutOfMemory;
    result[0] = @intCast(@mod(code, 256));
    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}
