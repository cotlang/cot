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
pub fn s_bld(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    // TODO: Implement string building
    return NativeError.NotImplemented;
}

/// S_PARSE - Parse a string
pub fn s_parse(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    // TODO: Implement string parsing
    return NativeError.NotImplemented;
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
