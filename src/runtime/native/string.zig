//! String native functions
//!
//! instr, fill, copy, str_delete_last, upper, lower, ltrim, s_bld, s_parse

const std = @import("std");
const native = @import("native.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;

/// Register all string functions
pub fn register(registry: anytype) !void {
    try registry.registerNative("instr", instr);
    try registry.registerNative("fill", fill);
    try registry.registerNative("copy", copy);
    try registry.registerNative("str_delete_last", str_delete_last);
    try registry.registerNative("upper", upper);
    try registry.registerNative("lower", lower);
    try registry.registerNative("ltrim", ltrim);
    try registry.registerNative("s_bld", s_bld);
    try registry.registerNative("s_parse", s_parse);
}

/// INSTR - Find substring position
pub fn instr(ctx: *NativeContext) NativeError!?Value {
    if (ctx.args.len < 2) return NativeError.InvalidArgument;

    const start: usize = if (ctx.args.len > 2)
        @intCast(@max(1, ctx.getArgInt(2) catch 1) - 1)
    else
        0;

    const haystack = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const needle = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    if (start >= haystack.len) return Value.initInt(0);

    if (std.mem.indexOf(u8, haystack[start..], needle)) |pos| {
        return Value.initInt(@intCast(pos + start + 1)); // 1-based
    }
    return Value.initInt(0);
}

/// FILL - Fill string with character
pub fn fill(ctx: *NativeContext) NativeError!?Value {
    if (ctx.args.len < 2) return NativeError.InvalidArgument;

    const fill_char_val = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const size = ctx.getArgInt(1) catch return NativeError.InvalidArgument;
    if (size <= 0) return NativeError.InvalidArgument;

    const fill_char: u8 = if (fill_char_val.len > 0) fill_char_val[0] else ' ';
    const result = ctx.allocator.alloc(u8, @intCast(size)) catch return NativeError.OutOfMemory;
    @memset(result, fill_char);

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// COPY - Copy string
pub fn copy(ctx: *NativeContext) NativeError!?Value {
    if (ctx.args.len < 1) return NativeError.InvalidArgument;

    const src = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const result = ctx.allocator.dupe(u8, src) catch return NativeError.OutOfMemory;

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// STR_DELETE_LAST - Remove the last character from a string (for backspace)
pub fn str_delete_last(ctx: *NativeContext) NativeError!?Value {
    const str = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    // Trim trailing spaces first
    const trimmed = std.mem.trimRight(u8, str, " ");

    // Remove the last character (if any)
    if (trimmed.len > 0) {
        const result = ctx.allocator.dupe(u8, trimmed[0 .. trimmed.len - 1]) catch return NativeError.OutOfMemory;
        return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
    }

    // Empty string - return empty
    const result = ctx.allocator.dupe(u8, "") catch return NativeError.OutOfMemory;
    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// S_BLD - Build a string with substitution
/// Format string uses %s for strings, %d for integers, %f for floats
pub fn s_bld(ctx: *NativeContext) NativeError!?Value {
    if (ctx.args.len < 1) return NativeError.InvalidArgument;

    const format = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    // Estimate output size (format length + extra for substitutions)
    var estimated_size: usize = format.len;
    for (ctx.args[1..]) |arg| {
        if (arg.isString() or arg.isFixedString()) {
            estimated_size += arg.toString().len;
        } else {
            estimated_size += 32; // Reasonable estimate for numbers
        }
    }

    var result: std.ArrayListUnmanaged(u8) = .empty;
    defer result.deinit(ctx.allocator);
    result.ensureTotalCapacity(ctx.allocator, estimated_size) catch return NativeError.OutOfMemory;

    var arg_idx: usize = 1;
    var i: usize = 0;
    while (i < format.len) {
        if (format[i] == '%' and i + 1 < format.len) {
            const spec = format[i + 1];
            if (spec == '%') {
                // Escaped %
                result.append(ctx.allocator, '%') catch return NativeError.OutOfMemory;
                i += 2;
                continue;
            }
            if (arg_idx < ctx.args.len) {
                const arg = ctx.args[arg_idx];
                switch (spec) {
                    's' => {
                        // String
                        const str = arg.toString();
                        result.appendSlice(ctx.allocator, str) catch return NativeError.OutOfMemory;
                    },
                    'd', 'i' => {
                        // Integer
                        const val = arg.toInt();
                        var buf: [32]u8 = undefined;
                        const printed = std.fmt.bufPrint(&buf, "{d}", .{val}) catch return NativeError.OutOfMemory;
                        result.appendSlice(ctx.allocator, printed) catch return NativeError.OutOfMemory;
                    },
                    'f' => {
                        // Float - treat as integer for now (proper decimal support would need Value.toDecimal)
                        const val = arg.toInt();
                        var buf: [32]u8 = undefined;
                        const printed = std.fmt.bufPrint(&buf, "{d}.0", .{val}) catch return NativeError.OutOfMemory;
                        result.appendSlice(ctx.allocator, printed) catch return NativeError.OutOfMemory;
                    },
                    else => {
                        // Unknown specifier, output as-is
                        result.append(ctx.allocator, '%') catch return NativeError.OutOfMemory;
                        result.append(ctx.allocator, spec) catch return NativeError.OutOfMemory;
                    },
                }
                arg_idx += 1;
                i += 2;
            } else {
                // No more args, output literally
                result.append(ctx.allocator, format[i]) catch return NativeError.OutOfMemory;
                i += 1;
            }
        } else {
            result.append(ctx.allocator, format[i]) catch return NativeError.OutOfMemory;
            i += 1;
        }
    }

    const str = result.toOwnedSlice(ctx.allocator) catch return NativeError.OutOfMemory;
    return Value.initString(ctx.allocator, str) catch return NativeError.OutOfMemory;
}

/// S_PARSE - Parse a string into components
/// Returns the position of the first delimiter found, or 0 if not found
/// Result goes into second argument
pub fn s_parse(ctx: *NativeContext) NativeError!?Value {
    if (ctx.args.len < 3) return NativeError.InvalidArgument;

    const source = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const delimiter = ctx.getArgString(2) catch return NativeError.InvalidArgument;

    if (delimiter.len == 0) {
        return Value.initInt(0);
    }

    // Find delimiter in source
    if (std.mem.indexOf(u8, source, delimiter)) |pos| {
        // Return 1-based position
        return Value.initInt(@intCast(pos + 1));
    }
    return Value.initInt(0);
}

/// UPPER - Convert string to uppercase
pub fn upper(ctx: *NativeContext) NativeError!?Value {
    const str = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const result = ctx.allocator.alloc(u8, str.len) catch return NativeError.OutOfMemory;
    for (str, 0..) |c, i| {
        result[i] = std.ascii.toUpper(c);
    }

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// LOWER - Convert string to lowercase
pub fn lower(ctx: *NativeContext) NativeError!?Value {
    const str = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const result = ctx.allocator.alloc(u8, str.len) catch return NativeError.OutOfMemory;
    for (str, 0..) |c, i| {
        result[i] = std.ascii.toLower(c);
    }

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// LTRIM - Remove leading spaces
pub fn ltrim(ctx: *NativeContext) NativeError!?Value {
    const str = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const trimmed = std.mem.trimLeft(u8, str, " ");
    const result = ctx.allocator.dupe(u8, trimmed) catch return NativeError.OutOfMemory;

    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}
