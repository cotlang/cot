//! Cot Core File I/O API
//!
//! Namespace: std.file
//! Functions: open, close, readLine, readAll, writeLine, write, eof, flush
//!
//! Handle-based file operations:
//!   import std.file
//!   var f = std.file.open("data.txt", "r")
//!   var line = std.file.readLine(f)
//!   std.file.writeLine(f, "Hello")
//!   std.file.close(f)
//!
//! This module uses the UnifiedHandleManager for all I/O operations.
//! For DBL channel-based syntax, see src/runtime/extensions/dbl/native_io.zig.

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const handles = @import("../handles/handles.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;
const UnifiedHandleManager = handles.UnifiedHandleManager;
const TextFileMode = handles.TextFileMode;

/// Register all file functions with namespaced names
pub fn register(registry: anytype) !void {
    // Namespaced names (std.file.*)
    try registry.registerNative("std.file.open", file_open);
    try registry.registerNative("std.file.close", file_close);
    try registry.registerNative("std.file.readLine", file_readline);
    try registry.registerNative("std.file.readAll", file_readall);
    try registry.registerNative("std.file.writeLine", file_writeline);
    try registry.registerNative("std.file.write", file_write);
    try registry.registerNative("std.file.eof", file_eof);
    try registry.registerNative("std.file.flush", file_flush);

    // Legacy lowercase names (for backward compatibility)
    try registry.registerNative("file.open", file_open);
    try registry.registerNative("file.close", file_close);
    try registry.registerNative("file.readline", file_readline);
    try registry.registerNative("file.readall", file_readall);
    try registry.registerNative("file.writeline", file_writeline);
    try registry.registerNative("file.write", file_write);
    try registry.registerNative("file.eof", file_eof);
    try registry.registerNative("file.flush", file_flush);
}

/// File.open(path, mode) -> handle
/// Mode: "r" (read), "w" (write), "a" (append), "rw" (read/write)
pub fn file_open(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const mode_str = ctx.getArgString(1) catch "r";

    const mode = TextFileMode.fromString(mode_str);

    debug.print(.general, "File.open: path='{s}' mode={s}", .{ path, @tagName(mode) });

    const handle_id = mgr.openTextFile(path, mode) catch |err| {
        debug.print(.general, "File.open failed: {}", .{err});
        return NativeError.FileError;
    };

    debug.print(.general, "File.open: allocated handle {d}", .{handle_id});
    return Value.initInt(@intCast(handle_id));
}

/// File.close(handle) -> void
pub fn file_close(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (handle_id == 0) {
        return NativeError.InvalidArgument;
    }

    mgr.close(handle_id);
    debug.print(.general, "File.close: closed handle {d}", .{handle_id});

    return null;
}

/// File.readLine(handle) -> string or nil at EOF
pub fn file_readline(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (handle_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(handle_id) orelse return NativeError.InvalidArgument;

    const line = handle.readLine(ctx.allocator) catch |err| {
        debug.print(.general, "File.readLine error: {}", .{err});
        return NativeError.FileError;
    };

    if (line) |l| {
        // initFixedString takes ownership of the buffer, don't free it
        return Value.initFixedString(ctx.allocator, l) catch return NativeError.OutOfMemory;
    } else {
        return Value.initNull();
    }
}

/// File.readAll(handle) -> string
pub fn file_readall(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (handle_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(handle_id) orelse return NativeError.InvalidArgument;

    // Read all lines and concatenate
    var content: std.ArrayListUnmanaged(u8) = .empty;
    defer content.deinit(ctx.allocator);

    while (true) {
        const line = handle.readLine(ctx.allocator) catch |err| {
            debug.print(.general, "File.readAll error: {}", .{err});
            return NativeError.FileError;
        };

        if (line) |l| {
            defer ctx.allocator.free(l);
            content.appendSlice(ctx.allocator, l) catch return NativeError.OutOfMemory;
            content.append(ctx.allocator, '\n') catch return NativeError.OutOfMemory;
        } else {
            break;
        }
    }

    const result = content.toOwnedSlice(ctx.allocator) catch return NativeError.OutOfMemory;
    return Value.initFixedString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// File.writeLine(handle, data) -> void
/// Writes data followed by a newline
pub fn file_writeline(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    if (handle_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(handle_id) orelse return NativeError.InvalidArgument;

    handle.writeLine(data) catch |err| {
        debug.print(.general, "File.writeLine error: {}", .{err});
        return NativeError.FileError;
    };

    return null;
}

/// File.write(handle, data) -> void
/// Writes data without newline
pub fn file_write(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    if (handle_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(handle_id) orelse return NativeError.InvalidArgument;

    handle.writeRaw(data) catch |err| {
        debug.print(.general, "File.write error: {}", .{err});
        return NativeError.FileError;
    };

    return null;
}

/// File.eof(handle) -> bool
pub fn file_eof(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (handle_id == 0) {
        return NativeError.InvalidArgument;
    }

    const handle = mgr.get(handle_id) orelse return NativeError.InvalidArgument;
    return Value.initBool(handle.eof());
}

/// File.flush(handle) -> void
pub fn file_flush(ctx: *NativeContext) NativeError!?Value {
    const mgr = ctx.handles orelse return NativeError.NotImplemented;
    const handle_id: u32 = @intCast(ctx.getArgInt(0) catch return NativeError.InvalidArgument);

    if (handle_id == 0) {
        return NativeError.InvalidArgument;
    }

    // Verify handle exists
    _ = mgr.get(handle_id) orelse return NativeError.InvalidArgument;

    // Flush is a no-op since we write directly
    return null;
}
