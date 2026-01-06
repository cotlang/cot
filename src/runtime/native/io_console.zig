//! Console I/O Native Functions
//!
//! Namespace: std.io
//! Prelude: print, println (available without import in .cot files)
//!
//! Functions:
//!   print(args...)   - write to stdout, no newline
//!   println(args...) - write to stdout with newline
//!   readln()         - read line from stdin
//!   log(args...)     - write to stderr (debugging)
//!
//! DBL/.NET compatibility aliases:
//!   display(args...) - alias for println (DBL style)
//!
//! Performance note: print/println use register-based opcodes for fast paths.
//! Native functions are fallback for edge cases.

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

/// Register console I/O functions
pub fn register(registry: anytype) !void {
    // Namespaced names (std.io.*)
    try registry.registerNative("std.io.print", io_print);
    try registry.registerNative("std.io.println", io_println);
    try registry.registerNative("std.io.readln", io_readln);
    try registry.registerNative("std.io.log", io_log);

    // Prelude names (available without import in .cot files)
    // Also serves as DBL compatibility
    try registry.registerNative("print", io_print);
    try registry.registerNative("println", io_println);

    // Non-prelude short names (require import in .cot, available in .dbl)
    try registry.registerNative("readln", io_readln);
    try registry.registerNative("log", io_log);

    // Note: DBL `display` alias is registered by the DBL extension
}

/// print(args...) - Write args to stdout without newline
fn io_print(ctx: *NativeContext) NativeError!?Value {
    const stdout_file: std.fs.File = .stdout();
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    for (ctx.args) |val| {
        writeValue(stdout, val);
    }
    stdout.flush() catch {};
    return null;
}

/// println(args...) - Write args to stdout with newline
/// Public so DBL extension can alias it as `display`
pub fn io_println(ctx: *NativeContext) NativeError!?Value {
    const stdout_file: std.fs.File = .stdout();
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    for (ctx.args) |val| {
        writeValue(stdout, val);
    }
    stdout.writeByte('\n') catch {};
    stdout.flush() catch {};
    return null;
}

/// readln() - Read a line from stdin
fn io_readln(ctx: *NativeContext) NativeError!?Value {
    const stdin_file: std.fs.File = .stdin();
    var buf: [4096]u8 = undefined;
    var pos: usize = 0;

    // Read character by character until newline or EOF
    while (pos < buf.len) {
        const n = stdin_file.read(buf[pos .. pos + 1]) catch |err| {
            debug.print(.general, "readln error: {}", .{err});
            return Value.initString(ctx.allocator, "") catch return null;
        };
        if (n == 0) break; // EOF
        if (buf[pos] == '\n') break;
        pos += 1;
    }

    if (pos > 0) {
        // Trim trailing \r if present (Windows line endings)
        const trimmed = std.mem.trimRight(u8, buf[0..pos], "\r");
        return Value.initString(ctx.allocator, trimmed) catch return null;
    }
    return Value.initString(ctx.allocator, "") catch return null;
}

/// log(args...) - Write to stderr for debugging
fn io_log(ctx: *NativeContext) NativeError!?Value {
    const stderr_file: std.fs.File = .stderr();
    var stderr_buffer: [4096]u8 = undefined;
    var stderr_writer = stderr_file.writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    for (ctx.args) |val| {
        writeValue(stderr, val);
    }
    stderr.writeByte('\n') catch {};
    stderr.flush() catch {};
    return null;
}

/// Write a Value to a writer with appropriate formatting
fn writeValue(writer: anytype, val: Value) void {
    switch (val.tag()) {
        .null_val => writer.writeAll("null") catch {},
        .boolean => writer.print("{s}", .{if (val.asBool()) "true" else "false"}) catch {},
        .integer => writer.print("{d}", .{val.asInt()}) catch {},
        .implied_decimal => {
            if (val.asImpliedDecimal()) |dval| {
                const divisor = std.math.pow(i64, 10, dval.precision);
                const whole = @divTrunc(dval.value, divisor);
                const frac = @abs(@rem(dval.value, divisor));
                if (dval.precision > 0) {
                    // Print whole part and decimal point
                    writer.print("{d}.", .{whole}) catch {};
                    // Manually pad fractional part with leading zeros
                    // Count digits in frac to determine leading zeros needed
                    var frac_digits: u8 = 0;
                    var temp = frac;
                    while (temp > 0) : (temp = @divTrunc(temp, 10)) {
                        frac_digits += 1;
                    }
                    if (frac == 0) frac_digits = 1;
                    // Write leading zeros
                    var i: u8 = frac_digits;
                    while (i < dval.precision) : (i += 1) {
                        writer.writeByte('0') catch {};
                    }
                    writer.print("{d}", .{frac}) catch {};
                } else {
                    writer.print("{d}", .{whole}) catch {};
                }
            }
        },
        .fixed_decimal => {
            if (val.asFixedDecimal()) |dval| {
                writer.print("{d}", .{dval.value}) catch {};
            }
        },
        .fixed_string, .string => writer.print("{s}", .{std.mem.trimRight(u8, val.asString(), " \x00")}) catch {},
        .record_ref => writer.writeAll("<record>") catch {},
        .handle => writer.print("<handle:{d}>", .{val.asHandle() orelse 0}) catch {},
        .object => {
            if (val.isMap()) {
                if (val.asMap()) |m| {
                    writer.print("<map:{d}>", .{m.len()}) catch {};
                } else {
                    writer.writeAll("<map:null>") catch {};
                }
            } else {
                writer.print("<object:{d}>", .{val.objectTypeId() orelse 0}) catch {};
            }
        },
    }
}
