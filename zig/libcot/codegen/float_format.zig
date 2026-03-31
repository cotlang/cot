// float_format.zig — Non-variadic float formatting for compiled programs.
//
// Cranelift can't pass float args in variadic C calls (confirmed by
// rustc_codegen_cranelift: "Non int ty F64 for variadic call").
// This provides a fixed-ABI wrapper that CIR-generated code calls instead.
//
// Compiled as a separate .o and linked into every user program,
// same pattern as dwarf_reader.zig.

const std = @import("std");

export fn cot_format_float(buf: [*]u8, size: usize, value: f64) callconv(.c) i64 {
    var fbs = std.io.fixedBufferStream(buf[0..size]);
    std.fmt.format(fbs.writer(), "{d}", .{value}) catch return 0;
    return @intCast(fbs.pos);
}

export fn cot_write_float(fd: i64, value: f64) callconv(.c) void {
    var buf: [64]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    std.fmt.format(fbs.writer(), "{d}", .{value}) catch return;
    _ = std.posix.write(@intCast(fd), buf[0..fbs.pos]) catch {};
}
