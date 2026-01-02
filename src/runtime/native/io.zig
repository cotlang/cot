//! I/O native functions
//!
//! Unified I/O that supports both text files and ISAM databases.
//! The channel type is determined at open time based on mode string.
//!
//! Text file functions: file_open, file_readline, file_writeline, file_close
//! Unified functions: open, close, reads, writes (route based on channel type)
//! Display: display, print

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const channels_mod = @import("../channels.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const NativeFn = native.NativeFn;
const Value = native.Value;
const ChannelType = channels_mod.ChannelType;

/// Register all I/O functions
pub fn register(registry: anytype) !void {
    // Console I/O
    try registry.registerNative("display", display);
    try registry.registerNative("print", print);
    try registry.registerNative("lpque", lpque);

    // File system
    try registry.registerNative("mkdir", mkdir);
    try registry.registerNative("rmdir", rmdir);
    try registry.registerNative("file_exists", file_exists);
    try registry.registerNative("dir_exists", dir_exists);

    // Modern text file API
    try registry.registerNative("file_open", file_open);
    try registry.registerNative("file_readline", file_readline);
    try registry.registerNative("file_writeline", file_writeline);
    try registry.registerNative("file_close", file_close);

    // Unified I/O (routes based on channel type)
    try registry.registerNative("io_open", io_open);
    try registry.registerNative("io_close", io_close);
    try registry.registerNative("io_reads", io_reads);
    try registry.registerNative("io_writes", io_writes);
}

/// DISPLAY - Output to console
pub fn display(ctx: *NativeContext) NativeError!?Value {
    var stdout_file = std.fs.File.stdout();
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    // Determine start index - if first arg looks like a channel (integer), skip it
    const start: usize = if (ctx.args.len > 1) blk: {
        const first_tag = ctx.args[0].tag();
        break :blk switch (first_tag) {
            .integer, .decimal => 1,
            else => 0,
        };
    } else 0;

    var i: usize = start;
    while (i < ctx.args.len) : (i += 1) {
        const val = ctx.args[i];
        switch (val.tag()) {
            .null_val => {},
            .boolean => stdout.print("{d}", .{@as(u8, if (val.asBool()) 1 else 0)}) catch {},
            .integer => stdout.print("{d}", .{val.asInt()}) catch {},
            .decimal => {
                if (val.asDecimal()) |dval| {
                    const divisor = std.math.pow(i64, 10, dval.precision);
                    const whole = @divTrunc(dval.value, divisor);
                    const frac = @abs(@rem(dval.value, divisor));
                    if (dval.precision > 0) {
                        stdout.print("{d}.{d}", .{ whole, frac }) catch {};
                    } else {
                        stdout.print("{d}", .{whole}) catch {};
                    }
                }
            },
            .fixed_string, .string => stdout.print("{s}", .{std.mem.trimRight(u8, val.asString(), " ")}) catch {},
            .record_ref => stdout.writeAll("<record>") catch {},
            .handle => stdout.print("<handle:{d}>", .{val.asHandle() orelse 0}) catch {},
            .object => {
                // Handle extension objects (Map is type_id 16)
                if (val.isMap()) {
                    if (val.asMap()) |m| {
                        stdout.print("<map:{d}>", .{m.len()}) catch {};
                    } else {
                        stdout.writeAll("<map:null>") catch {};
                    }
                } else {
                    stdout.print("<object:type={d}>", .{val.objectTypeId() orelse 0}) catch {};
                }
            },
        }
    }
    stdout.writeByte('\n') catch {};
    stdout.flush() catch {};
    return null;
}

/// PRINT - Print to printer
pub fn print(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    return null;
}

/// LPQUE - Queue to printer
pub fn lpque(ctx: *NativeContext) NativeError!?Value {
    _ = ctx;
    return null;
}

/// MKDIR - Create a directory
pub fn mkdir(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    std.fs.cwd().makePath(path) catch |err| {
        debug.print(.general, "mkdir failed for '{s}': {}", .{ path, err });
        return Value.initInt(-1);
    };

    debug.print(.general, "mkdir: created '{s}'", .{path});
    return Value.initInt(0);
}

/// RMDIR - Remove an empty directory
pub fn rmdir(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    std.fs.cwd().deleteDir(path) catch |err| {
        debug.print(.general, "rmdir failed for '{s}': {}", .{ path, err });
        return Value.initInt(-1);
    };

    debug.print(.general, "rmdir: removed '{s}'", .{path});
    return Value.initInt(0);
}

/// FILE_EXISTS - Check if a file exists
pub fn file_exists(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const exists = blk: {
        std.fs.cwd().access(path, .{}) catch break :blk false;
        break :blk true;
    };

    return Value.initInt(if (exists) 1 else 0);
}

/// DIR_EXISTS - Check if a directory exists
pub fn dir_exists(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const exists = blk: {
        var dir = std.fs.cwd().openDir(path, .{}) catch break :blk false;
        dir.close();
        break :blk true;
    };

    return Value.initInt(if (exists) 1 else 0);
}

// ============================================================================
// Modern Text File API
// ============================================================================

/// FILE_OPEN - Open a text file
/// Args: channel, filename, mode ("r", "w", "a")
pub fn file_open(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const filename = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const mode = ctx.getArgString(2) catch "r";

    // Convert simple mode to legacy-style mode
    const legacy_mode: []const u8 = switch (mode[0]) {
        'w', 'W' => "O:SEQ",
        'a', 'A' => "A:SEQ",
        else => "I:SEQ",
    };

    ch_mgr.open(channel_id, legacy_mode, filename) catch |err| {
        debug.print(.general, "file_open failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// FILE_READLINE - Read a line from text file
/// Args: channel
/// Returns: string or null at EOF
pub fn file_readline(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    const line = ch_mgr.readLine(channel_id) catch |err| {
        debug.print(.general, "file_readline failed: {}", .{err});
        return NativeError.EndOfFile;
    };

    if (line) |data| {
        return Value.initString(ctx.allocator, data) catch return NativeError.OutOfMemory;
    }
    return NativeError.EndOfFile;
}

/// FILE_WRITELINE - Write a line to text file
/// Args: channel, data
pub fn file_writeline(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    ch_mgr.writeLine(channel_id, data) catch |err| {
        debug.print(.general, "file_writeline failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// FILE_CLOSE - Close a text file
/// Args: channel
pub fn file_close(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    ch_mgr.close(channel_id);

    return Value.initInt(0);
}

// ============================================================================
// Unified I/O (routes based on channel type)
// ============================================================================

/// IO_OPEN - Open a channel (auto-detects text vs ISAM from mode)
/// Args: channel, mode, filename
/// Mode: I/O/U/A with optional :SEQ or :ISAM suffix
pub fn io_open(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const mode = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const filename = ctx.getArgString(2) catch return NativeError.InvalidArgument;

    ch_mgr.open(channel_id, mode, filename) catch |err| {
        debug.print(.general, "io_open failed: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// IO_CLOSE - Close a channel
/// Args: channel
pub fn io_close(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    ch_mgr.close(channel_id);

    return Value.initInt(0);
}

/// IO_READS - Sequential read (routes based on channel type)
/// Args: channel
/// For text: reads line
/// For ISAM: reads next record
pub fn io_reads(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    const ch_type = ch_mgr.getType(channel_id);
    switch (ch_type) {
        .text => {
            const line = ch_mgr.readLine(channel_id) catch |err| {
                debug.print(.general, "io_reads (text) failed: {}", .{err});
                return NativeError.EndOfFile;
            };

            if (line) |data| {
                return Value.initString(ctx.allocator, data) catch return NativeError.OutOfMemory;
            }
            return NativeError.EndOfFile;
        },
        .isam => {
            // Delegate to ISAM cursor manager
            const cursors = ctx.cursors orelse return NativeError.NotImplemented;
            const record = cursors.readNext(channel_id) catch |err| {
                debug.print(.general, "io_reads (isam) failed: {}", .{err});
                return NativeError.EndOfFile;
            };

            const result = ctx.allocator.dupe(u8, record) catch return NativeError.OutOfMemory;
            return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
        },
        .closed => return NativeError.FileError,
    }
}

/// IO_WRITES - Sequential write (routes based on channel type)
/// Args: channel, data
/// For text: writes line
/// For ISAM: writes record at current position
pub fn io_writes(ctx: *NativeContext) NativeError!?Value {
    const ch_mgr = ctx.channels orelse return NativeError.NotImplemented;

    const channel_id: u8 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    const ch_type = ch_mgr.getType(channel_id);
    switch (ch_type) {
        .text => {
            ch_mgr.writeLine(channel_id, data) catch |err| {
                debug.print(.general, "io_writes (text) failed: {}", .{err});
                return Value.initInt(-1);
            };
            return Value.initInt(0);
        },
        .isam => {
            // Delegate to ISAM cursor manager
            const cursors = ctx.cursors orelse return NativeError.NotImplemented;
            cursors.write(channel_id, data) catch |err| {
                debug.print(.general, "io_writes (isam) failed: {}", .{err});
                return Value.initInt(-1);
            };
            return Value.initInt(0);
        },
        .closed => return NativeError.FileError,
    }
}
