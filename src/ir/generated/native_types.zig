//! Auto-generated from spec/natives.toml
//! DO NOT EDIT MANUALLY - regenerate with: zig build gen-natives
//!
//! This file contains compile-time mappings for native function return types.

const std = @import("std");
const ir = @import("../ir.zig");

/// Maps qualified function names to their IR return types.
/// Used by the lowerer to determine return types for native function calls.
pub const FunctionReturnTypes = std.StaticStringMap(ir.Type).initComptime(.{
    .{ "assert", .void },
    .{ "print", .void },
    .{ "println", .void },
    .{ "std.convert.ascii", .i64 },
    .{ "std.convert.boolean", .bool },
    .{ "std.convert.char", .string },
    .{ "std.convert.decimal", .f64 },
    .{ "std.convert.integer", .i64 },
    .{ "std.convert.string", .string },
    .{ "std.debug.assert", .void },
    .{ "std.io.file.file_close", .void },
    .{ "std.io.file.file_eof", .bool },
    .{ "std.io.file.file_flush", .void },
    .{ "std.io.file.file_open", .i64 },
    .{ "std.io.file.file_readall", .string },
    .{ "std.io.file.file_readline", .string },
    .{ "std.io.file.file_write", .void },
    .{ "std.io.file.file_writeline", .void },
    .{ "std.io.io_log", .void },
    .{ "std.io.print", .void },
    .{ "std.io.println", .void },
    .{ "std.io.readln", .string },
    .{ "std.math.abs", .i64 },
    .{ "std.math.cos", .f64 },
    .{ "std.math.exp", .f64 },
    .{ "std.math.log", .f64 },
    .{ "std.math.log10", .f64 },
    .{ "std.math.round", .f64 },
    .{ "std.math.sin", .f64 },
    .{ "std.math.sqrt", .f64 },
    .{ "std.math.tan", .f64 },
    .{ "std.math.trunc", .i64 },
    .{ "std.string.copy", .string },
    .{ "std.string.fill", .string },
    .{ "std.string.instr", .i64 },
    .{ "std.string.lower", .string },
    .{ "std.string.ltrim", .string },
    .{ "std.string.s_bld", .string },
    .{ "std.string.s_parse", .i64 },
    .{ "std.string.upper", .string },
    .{ "std.sys.error", .i64 },
    .{ "std.sys.getlog", .string },
    .{ "std.sys.setlog", .void },
    .{ "std.time.date", .i64 },
    .{ "std.time.time", .i64 },
});

/// Functions available in prelude (no import required in .cot files).
pub const PreludeFunctions = [_][]const u8{
    "assert",
    "print",
    "println",
};

/// Get the return type for a function name.
/// Returns null if the function is not a known native.
pub fn getReturnType(name: []const u8) ?ir.Type {
    return FunctionReturnTypes.get(name);
}

/// Check if a function is in the prelude.
pub fn isPreludeFunction(name: []const u8) bool {
    for (PreludeFunctions) |f| {
        if (std.mem.eql(u8, f, name)) return true;
    }
    return false;
}

test "known functions" {
    try std.testing.expectEqual(ir.Type.void, getReturnType("std.io.print").?);
    try std.testing.expectEqual(ir.Type.void, getReturnType("print").?);
    try std.testing.expectEqual(ir.Type.i64, getReturnType("std.math.abs").?);
    try std.testing.expect(getReturnType("unknown_function") == null);
}

test "prelude functions" {
    try std.testing.expect(isPreludeFunction("print"));
    try std.testing.expect(isPreludeFunction("println"));
    try std.testing.expect(isPreludeFunction("assert"));
    try std.testing.expect(!isPreludeFunction("abs"));
}
