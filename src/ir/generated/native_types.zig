//! Auto-generated from spec/natives.toml
//! DO NOT EDIT MANUALLY - regenerate with: zig build gen-natives
//!
//! This file contains compile-time mappings for native function return types.

const std = @import("std");
const ir = @import("../ir.zig");

/// Maps qualified function names to their IR return types.
/// Used by the lowerer to determine return types for native function calls.
pub const FunctionReturnTypes = std.StaticStringMap(ir.Type).initComptime(.{
    .{ "abs", .i64 },
    .{ "append_file", .bool },
    .{ "ascii", .i64 },
    .{ "assert", .void },
    .{ "atrim", .string },
    .{ "boolean", .bool },
    .{ "char", .string },
    .{ "copy", .string },
    .{ "copy_file", .bool },
    .{ "cos", .f64 },
    .{ "cwd", .string },
    .{ "date", .i64 },
    .{ "datetime", .string },
    .{ "decimal", .f64 },
    .{ "error", .i64 },
    .{ "exists", .bool },
    .{ "exp", .f64 },
    .{ "file_close", .void },
    .{ "file_eof", .bool },
    .{ "file_flush", .void },
    .{ "file_open", .i64 },
    .{ "file_write", .void },
    .{ "file_writeline", .void },
    .{ "fill", .string },
    .{ "getlog", .string },
    .{ "hex_decode", .string },
    .{ "hex_encode", .string },
    .{ "hmac_sha256_hex", .string },
    .{ "instr", .i64 },
    .{ "integer", .i64 },
    .{ "io_log", .void },
    .{ "is_dir", .bool },
    .{ "is_file", .bool },
    .{ "len", .i64 },
    .{ "log", .f64 },
    .{ "log10", .f64 },
    .{ "lower", .string },
    .{ "ltrim", .string },
    .{ "md5_hex", .string },
    .{ "mkdir", .bool },
    .{ "mkdir_all", .bool },
    .{ "print", .void },
    .{ "println", .void },
    .{ "read_dir", .string },
    .{ "read_file", .string },
    .{ "readln", .string },
    .{ "remove", .bool },
    .{ "remove_all", .bool },
    .{ "rename", .bool },
    .{ "round", .f64 },
    .{ "s_bld", .string },
    .{ "s_parse", .i64 },
    .{ "setlog", .void },
    .{ "sha256", .string },
    .{ "sha256_hex", .string },
    .{ "sha512_hex", .string },
    .{ "sin", .f64 },
    .{ "size", .i64 },
    .{ "sql_begin", .void },
    .{ "sql_close", .void },
    .{ "sql_commit", .void },
    .{ "sql_exec", .i64 },
    .{ "sql_fetch", .string },
    .{ "sql_fetch_all", .string },
    .{ "sql_open", .i64 },
    .{ "sql_query", .i64 },
    .{ "sql_rollback", .void },
    .{ "sqrt", .f64 },
    .{ "std.convert.ascii", .i64 },
    .{ "std.convert.boolean", .bool },
    .{ "std.convert.char", .string },
    .{ "std.convert.decimal", .f64 },
    .{ "std.convert.integer", .i64 },
    .{ "std.convert.string", .string },
    .{ "std.crypto.hex_decode", .string },
    .{ "std.crypto.hex_encode", .string },
    .{ "std.crypto.hmac_sha256_hex", .string },
    .{ "std.crypto.md5_hex", .string },
    .{ "std.crypto.sha256", .string },
    .{ "std.crypto.sha256_hex", .string },
    .{ "std.crypto.sha512_hex", .string },
    .{ "std.debug.assert", .void },
    .{ "std.fs.append_file", .bool },
    .{ "std.fs.copy_file", .bool },
    .{ "std.fs.cwd", .string },
    .{ "std.fs.exists", .bool },
    .{ "std.fs.is_dir", .bool },
    .{ "std.fs.is_file", .bool },
    .{ "std.fs.mkdir", .bool },
    .{ "std.fs.mkdir_all", .bool },
    .{ "std.fs.read_dir", .string },
    .{ "std.fs.read_file", .string },
    .{ "std.fs.remove", .bool },
    .{ "std.fs.remove_all", .bool },
    .{ "std.fs.rename", .bool },
    .{ "std.fs.write_file", .bool },
    .{ "std.io.file.file_close", .void },
    .{ "std.io.file.file_eof", .bool },
    .{ "std.io.file.file_flush", .void },
    .{ "std.io.file.file_open", .i64 },
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
    .{ "std.sql.sql_begin", .void },
    .{ "std.sql.sql_close", .void },
    .{ "std.sql.sql_commit", .void },
    .{ "std.sql.sql_exec", .i64 },
    .{ "std.sql.sql_fetch", .string },
    .{ "std.sql.sql_fetch_all", .string },
    .{ "std.sql.sql_open", .i64 },
    .{ "std.sql.sql_query", .i64 },
    .{ "std.sql.sql_rollback", .void },
    .{ "std.string.atrim", .string },
    .{ "std.string.copy", .string },
    .{ "std.string.fill", .string },
    .{ "std.string.instr", .i64 },
    .{ "std.string.len", .i64 },
    .{ "std.string.lower", .string },
    .{ "std.string.ltrim", .string },
    .{ "std.string.s_bld", .string },
    .{ "std.string.s_parse", .i64 },
    .{ "std.string.size", .i64 },
    .{ "std.string.trim", .string },
    .{ "std.string.upper", .string },
    .{ "std.sys.error", .i64 },
    .{ "std.sys.getlog", .string },
    .{ "std.sys.setlog", .void },
    .{ "std.time.date", .i64 },
    .{ "std.time.datetime", .string },
    .{ "std.time.time", .i64 },
    .{ "string", .string },
    .{ "tan", .f64 },
    .{ "time", .i64 },
    .{ "trim", .string },
    .{ "trunc", .i64 },
    .{ "upper", .string },
    .{ "write_file", .bool },
});

/// Functions available in prelude (no import required in .cot files).
pub const PreludeFunctions = [_][]const u8{
    "assert",
    "atrim",
    "date",
    "datetime",
    "len",
    "print",
    "println",
    "size",
    "time",
    "trim",
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
