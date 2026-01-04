//! Filesystem Utility Functions
//!
//! Directory operations:
//!   mkdir(path)       - create directory
//!   rmdir(path)       - remove empty directory
//!   file_exists(path) - check if file exists
//!   dir_exists(path)  - check if directory exists

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

/// Register filesystem utility functions
pub fn register(registry: anytype) !void {
    try registry.registerNative("mkdir", mkdir);
    try registry.registerNative("rmdir", rmdir);
    try registry.registerNative("file_exists", file_exists);
    try registry.registerNative("dir_exists", dir_exists);
}

/// mkdir(path) - Create a directory (and parent directories)
fn mkdir(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    std.fs.cwd().makePath(path) catch |err| {
        debug.print(.general, "mkdir failed for '{s}': {}", .{ path, err });
        return Value.initInt(-1);
    };

    debug.print(.general, "mkdir: created '{s}'", .{path});
    return Value.initInt(0);
}

/// rmdir(path) - Remove an empty directory
fn rmdir(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    std.fs.cwd().deleteDir(path) catch |err| {
        debug.print(.general, "rmdir failed for '{s}': {}", .{ path, err });
        return Value.initInt(-1);
    };

    debug.print(.general, "rmdir: removed '{s}'", .{path});
    return Value.initInt(0);
}

/// file_exists(path) - Check if a file exists (returns 1/0)
fn file_exists(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const exists = blk: {
        std.fs.cwd().access(path, .{}) catch break :blk false;
        break :blk true;
    };

    return Value.initInt(if (exists) 1 else 0);
}

/// dir_exists(path) - Check if a directory exists (returns 1/0)
fn dir_exists(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const exists = blk: {
        var dir = std.fs.cwd().openDir(path, .{}) catch break :blk false;
        dir.close();
        break :blk true;
    };

    return Value.initInt(if (exists) 1 else 0);
}
