//! DBL I/O Native Functions
//!
//! Channel-based I/O operations for DBL compatibility:
//!   open(channel, mode, filename)
//!   close(channel)
//!   reads(channel, record)  - read line from text file
//!   writes(channel, data)   - write line to text file
//!   read(channel, record, key)  - read by key from ISAM
//!   store(channel, record)  - insert into ISAM
//!   write(channel, record)  - update current ISAM record
//!   delete(channel)  - delete current ISAM record
//!   find(channel, key_num, key)  - position ISAM cursor
//!
//! These wrap the core File.*/Db.* APIs with DBL channel semantics.

const std = @import("std");
const native = @import("../../native/native.zig");
const handles = @import("../../handles/handles.zig");
const debug = @import("../../debug.zig");
const cotdb = @import("cotdb");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;
const UnifiedHandleManager = handles.UnifiedHandleManager;
const TextFileMode = handles.TextFileMode;
const MatchMode = cotdb.MatchMode;

/// Register all DBL I/O functions
pub fn register(registry: anytype) !void {
    // DBL channel-based I/O (short names)
    try registry.registerNative("open", dbl_open);
    try registry.registerNative("close", dbl_close);
    try registry.registerNative("reads", dbl_reads);
    try registry.registerNative("writes", dbl_writes);
    try registry.registerNative("puts", dbl_puts);
    try registry.registerNative("read", dbl_read);
    try registry.registerNative("store", dbl_store);
    try registry.registerNative("write", dbl_write);
    try registry.registerNative("delete", dbl_delete);
    try registry.registerNative("find", dbl_find);
    try registry.registerNative("text_eof", dbl_eof);

    // Also register with db_ prefix (parser emits these)
    try registry.registerNative("db_open", dbl_open);
    try registry.registerNative("db_close", dbl_close);
    try registry.registerNative("db_read", dbl_read);
    try registry.registerNative("db_readnext", dbl_reads);
    try registry.registerNative("db_store", dbl_store);
    try registry.registerNative("db_write", dbl_write);
    try registry.registerNative("db_delete", dbl_delete);
    try registry.registerNative("db_find", dbl_find);
}

/// Determine if a file is ISAM based on mode and extension
fn isIsamFile(mode: []const u8, filename: []const u8) bool {
    // Check mode for ISAM indicators
    var upper_buf: [32]u8 = undefined;
    const len = @min(mode.len, 31);
    for (mode[0..len], 0..) |c, i| {
        upper_buf[i] = std.ascii.toUpper(c);
    }
    const upper = upper_buf[0..len];

    // U:ISAM, I:ISAM are explicitly ISAM
    if (std.mem.indexOf(u8, upper, "ISAM") != null) return true;

    // Output/Append modes are text-only
    if (std.mem.indexOf(u8, upper, "O") != null) return false;
    if (std.mem.indexOf(u8, upper, "A") != null) return false;
    if (std.mem.indexOf(u8, upper, "S") != null) return false; // Sequential

    // Check extension
    if (std.mem.endsWith(u8, filename, ".ism") or
        std.mem.endsWith(u8, filename, ".db") or
        std.mem.endsWith(u8, filename, ".ddf"))
    {
        return true;
    }

    // Default to text for Input mode if not explicit ISAM
    return false;
}

/// open(channel_id, mode, filename) - DBL channel-based open
pub fn dbl_open(ctx: *NativeContext) NativeError!?Value {
    debug.print(.general, "dbl_open called with {d} args", .{ctx.args.len});
    const mgr = ctx.handles orelse {
        debug.print(.general, "dbl_open: handles is null!", .{});
        return NativeError.NotImplemented;
    };

    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    // Handle both 2-arg and 3-arg forms:
    // 2-arg: open(channel_id, filename) - defaults to "I" mode
    // 3-arg: open(channel_id, mode, filename) - DBL's native form
    const mode = if (ctx.args.len >= 3)
        ctx.getArgString(1) catch "I"
    else
        "I"; // Default to Input mode

    const filename = if (ctx.args.len >= 3)
        ctx.getArgString(2) catch return NativeError.InvalidArgument
    else
        ctx.getArgString(1) catch return NativeError.InvalidArgument;

    debug.print(.general, "dbl_open: channel={d} mode='{s}' filename='{s}'", .{ channel_id, mode, filename });

    if (isIsamFile(mode, filename)) {
        // Open as ISAM
        mgr.openIsamAt(channel_id, filename) catch |err| {
            debug.print(.general, "dbl_open ISAM failed: {}", .{err});
            return Value.initInt(-1);
        };
    } else {
        // Open as text file
        const file_mode = TextFileMode.fromString(mode);
        mgr.openTextFileAt(channel_id, filename, file_mode) catch |err| {
            debug.print(.general, "dbl_open text failed: {}", .{err});
            return Value.initInt(-1);
        };
    }

    return Value.initInt(0);
}

/// close(channel_id) - DBL channel-based close
pub fn dbl_close(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    mgr.close(channel_id);
    return Value.initInt(0);
}

/// reads(channel_id) - Read line from text file
pub fn dbl_reads(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;

    // Check handle type
    switch (handle.*) {
        .text_file => {
            const line = handle.readLine(ctx.allocator) catch |err| {
                debug.print(.general, "dbl_reads error: {}", .{err});
                return NativeError.FileError;
            };

            if (line) |l| {
                return Value.initFixedString(ctx.allocator, l) catch return NativeError.OutOfMemory;
            } else {
                return NativeError.EndOfFile;
            }
        },
        .isam_cursor => {
            // For ISAM, reads is readNext
            const record = handle.isamReadNext() catch |err| {
                debug.print(.general, "dbl_reads ISAM error: {}", .{err});
                return NativeError.EndOfFile;
            };

            const result = ctx.allocator.dupe(u8, record) catch return NativeError.OutOfMemory;
            return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
        },
    }
}

/// writes(channel_id, data) - Write line to text file
pub fn dbl_writes(ctx: *NativeContext) NativeError!?Value {
    debug.print(.general, "dbl_writes called with {d} args", .{ctx.args.len});
    const mgr = ctx.handles orelse {
        debug.print(.general, "dbl_writes: handles is null!", .{});
        return NativeError.NotImplemented;
    };
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;

    handle.writeLine(data) catch |err| {
        debug.print(.general, "dbl_writes error: {}", .{err});
        return NativeError.FileError;
    };

    return Value.initInt(0);
}

/// puts(channel_id, data) - Write without newline
pub fn dbl_puts(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;

    handle.writeRaw(data) catch |err| {
        debug.print(.general, "dbl_puts error: {}", .{err});
        return NativeError.FileError;
    };

    return Value.initInt(0);
}

/// read(channel_id, key_value) or read(channel_id, key_num, key_value) - ISAM read by key
/// DBL syntax: read(channel, record, key) - but record is return value, so we get (channel, key)
pub fn dbl_read(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    // Handle both 2-arg (channel, key) and 3-arg (channel, key_num, key) forms
    var key_num: u8 = 0;
    var key_buf: [32]u8 = undefined;
    const key_value: []const u8 = blk: {
        if (ctx.args.len >= 3) {
            // 3-arg form: (channel, key_num, key_value)
            key_num = @intCast(ctx.getArgInt(1) catch 0);
            const str_val = ctx.getArgString(2) catch "";
            if (str_val.len > 0) break :blk str_val;
            const int_val = ctx.getArgInt(2) catch 0;
            const formatted = std.fmt.bufPrint(&key_buf, "{d:0>6}", .{int_val}) catch "";
            break :blk formatted;
        } else {
            // 2-arg form: (channel, key_value) - DBL default
            const str_val = ctx.getArgString(1) catch "";
            if (str_val.len > 0) break :blk str_val;
            const int_val = ctx.getArgInt(1) catch 0;
            const formatted = std.fmt.bufPrint(&key_buf, "{d:0>6}", .{int_val}) catch "";
            break :blk formatted;
        }
    };

    debug.print(.general, "dbl_read: channel={d} key_num={d} key='{s}'", .{ channel_id, key_num, key_value });

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;

    const record = handle.isamRead(key_num, key_value, .exact) catch |err| {
        debug.print(.general, "dbl_read error: {}", .{err});
        return NativeError.RecordNotFound;
    };

    const result = ctx.allocator.dupe(u8, record) catch return NativeError.OutOfMemory;
    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// store(channel_id, record) - ISAM insert
pub fn dbl_store(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const record = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    debug.print(.general, "dbl_store: channel={d} record.len={d}", .{ channel_id, record.len });

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse {
        debug.print(.general, "dbl_store: handle not found for channel {d}", .{channel_id});
        return NativeError.InvalidArgument;
    };

    handle.isamStore(record) catch |err| {
        debug.print(.general, "dbl_store error: {}", .{err});
        return Value.initInt(-1);
    };

    debug.print(.general, "dbl_store: success, returning 0", .{});
    return Value.initInt(0);
}

/// write(channel_id, record) - ISAM update current
pub fn dbl_write(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const record = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;

    handle.isamWrite(record) catch |err| {
        debug.print(.general, "dbl_write error: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// delete(channel_id) - ISAM delete current
pub fn dbl_delete(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;

    handle.isamDelete() catch |err| {
        debug.print(.general, "dbl_delete error: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// find(channel_id, key_num, key_value, mode) - ISAM position
pub fn dbl_find(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const key_num: u8 = @intCast(ctx.getArgInt(1) catch 0);
    const key_value = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    const mode_str = ctx.getArgString(3) catch "exact";

    const match_mode: MatchMode = blk: {
        var lower_buf: [16]u8 = undefined;
        const lower = std.ascii.lowerString(&lower_buf, mode_str);
        if (std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "ge") or
            std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "greater_equal"))
        {
            break :blk .greater_equal;
        } else if (std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "gt") or
            std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "greater"))
        {
            break :blk .greater;
        } else if (std.mem.eql(u8, lower[0..@min(mode_str.len, 16)], "partial")) {
            break :blk .partial;
        } else {
            break :blk .exact;
        }
    };

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;

    handle.isamFind(key_num, key_value, match_mode) catch |err| {
        debug.print(.general, "dbl_find error: {}", .{err});
        return Value.initInt(-1);
    };

    return Value.initInt(0);
}

/// text_eof(channel_id) - Check if at EOF
pub fn dbl_eof(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const channel_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (channel_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(channel_id) orelse return NativeError.InvalidArgument;
    return Value.initInt(if (handle.eof()) 1 else 0);
}
