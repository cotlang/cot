//! std.fs - Filesystem Functions
//!
//! Path-based convenience API for file and directory operations.
//! Follows Zig std.fs naming conventions.
//!
//! File Operations:
//!   read_file(path)           - read entire file contents
//!   write_file(path, content) - write/overwrite file
//!   append_file(path, content)- append to file
//!   copy_file(src, dst)       - copy file
//!
//! Path Operations:
//!   exists(path)              - check if path exists
//!   is_file(path)             - check if path is file
//!   is_dir(path)              - check if path is directory
//!   rename(old, new)          - rename/move file or directory
//!
//! Directory Operations:
//!   mkdir(path)               - create single directory
//!   mkdir_all(path)           - create directory tree (mkdir -p)
//!   remove(path)              - remove file or empty directory
//!   remove_all(path)          - remove recursively (rm -rf)
//!   read_dir(path)            - list directory contents
//!   cwd()                     - get current working directory

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

/// Register std.fs functions
pub fn register(registry: anytype) !void {
    // File operations
    try registry.registerNative("std.fs.read_file", fs_read_file);
    try registry.registerNative("std.fs.write_file", fs_write_file);
    try registry.registerNative("std.fs.write_bytes", fs_write_bytes);
    try registry.registerNative("std.fs.append_file", fs_append_file);
    try registry.registerNative("std.fs.copy_file", fs_copy_file);

    // Path checks
    try registry.registerNative("std.fs.exists", fs_exists);
    try registry.registerNative("std.fs.is_file", fs_is_file);
    try registry.registerNative("std.fs.is_dir", fs_is_dir);

    // Directory operations
    try registry.registerNative("std.fs.mkdir", fs_mkdir);
    try registry.registerNative("std.fs.mkdir_all", fs_mkdir_all);
    try registry.registerNative("std.fs.remove", fs_remove);
    try registry.registerNative("std.fs.remove_all", fs_remove_all);
    try registry.registerNative("std.fs.read_dir", fs_read_dir);
    try registry.registerNative("std.fs.rename", fs_rename);
    try registry.registerNative("std.fs.cwd", fs_cwd);

    // Short names for convenience
    try registry.registerNative("read_file", fs_read_file);
    try registry.registerNative("write_file", fs_write_file);
    try registry.registerNative("write_bytes", fs_write_bytes);
    try registry.registerNative("append_file", fs_append_file);
    try registry.registerNative("copy_file", fs_copy_file);
    try registry.registerNative("fs_exists", fs_exists);
    try registry.registerNative("fs_is_file", fs_is_file);
    try registry.registerNative("fs_is_dir", fs_is_dir);
    try registry.registerNative("fs_mkdir", fs_mkdir);
    try registry.registerNative("fs_mkdir_all", fs_mkdir_all);
    try registry.registerNative("fs_remove", fs_remove);
    try registry.registerNative("fs_remove_all", fs_remove_all);
    try registry.registerNative("fs_read_dir", fs_read_dir);
    try registry.registerNative("fs_rename", fs_rename);
    try registry.registerNative("fs_cwd", fs_cwd);
}

// =============================================================================
// File Operations
// =============================================================================

/// read_file(path) -> string
/// Read entire file contents as a string. Throws FileError on failure.
fn fs_read_file(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        debug.print(.general, "read_file: failed to open '{s}': {}", .{ path, err });
        return NativeError.FileError;
    };
    defer file.close();

    // Read entire file
    const content = file.readToEndAlloc(ctx.allocator, 100 * 1024 * 1024) catch |err| {
        debug.print(.general, "read_file: failed to read '{s}': {}", .{ path, err });
        return NativeError.FileError;
    };

    return Value.initString(ctx.allocator, content) catch return NativeError.OutOfMemory;
}

/// write_file(path, content) -> bool
/// Write string content to file (creates or overwrites). Returns true on success.
fn fs_write_file(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const content = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    const file = std.fs.cwd().createFile(path, .{}) catch |err| {
        debug.print(.general, "write_file: failed to create '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };
    defer file.close();

    file.writeAll(content) catch |err| {
        debug.print(.general, "write_file: failed to write '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };

    debug.print(.general, "write_file: wrote {} bytes to '{s}'", .{ content.len, path });
    return Value.initBool(true);
}

/// write_bytes(path, bytes: List<i64>) -> bool
/// Write binary bytes (as List<i64> with values 0-255) to file. Returns true on success.
/// Used for writing bytecode files.
fn fs_write_bytes(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    // Get the list of bytes
    const list_val = ctx.args[1];
    const list_ptr = list_val.asList() orelse return NativeError.InvalidArgument;

    // Get list length
    const len = list_ptr.len();

    // Allocate buffer for bytes
    const buf = ctx.allocator.alloc(u8, len) catch return NativeError.OutOfMemory;
    defer ctx.allocator.free(buf);

    // Convert i64 values to u8 bytes
    for (0..len) |i| {
        const val = list_ptr.get(i) orelse return NativeError.InvalidArgument;
        const int_val = val.asInt();
        if (int_val < 0 or int_val > 255) {
            debug.print(.general, "write_bytes: byte value {} out of range at index {}", .{ int_val, i });
            return NativeError.InvalidArgument;
        }
        buf[i] = @intCast(int_val);
    }

    // Write to file
    const file = std.fs.cwd().createFile(path, .{}) catch |err| {
        debug.print(.general, "write_bytes: failed to create '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };
    defer file.close();

    file.writeAll(buf) catch |err| {
        debug.print(.general, "write_bytes: failed to write '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };

    debug.print(.general, "write_bytes: wrote {} bytes to '{s}'", .{ len, path });
    return Value.initBool(true);
}

/// append_file(path, content) -> bool
/// Append string content to file (creates if not exists). Returns true on success.
fn fs_append_file(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const content = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    const file = std.fs.cwd().openFile(path, .{ .mode = .write_only }) catch |err| {
        // If file doesn't exist, create it
        if (err == error.FileNotFound) {
            const new_file = std.fs.cwd().createFile(path, .{}) catch |create_err| {
                debug.print(.general, "append_file: failed to create '{s}': {}", .{ path, create_err });
                return Value.initBool(false);
            };
            defer new_file.close();
            new_file.writeAll(content) catch |write_err| {
                debug.print(.general, "append_file: failed to write '{s}': {}", .{ path, write_err });
                return Value.initBool(false);
            };
            return Value.initBool(true);
        }
        debug.print(.general, "append_file: failed to open '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };
    defer file.close();

    // Seek to end and write
    file.seekFromEnd(0) catch |err| {
        debug.print(.general, "append_file: failed to seek '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };

    file.writeAll(content) catch |err| {
        debug.print(.general, "append_file: failed to append '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };

    return Value.initBool(true);
}

/// copy_file(src, dst) -> bool
/// Copy file from source to destination. Returns true on success.
fn fs_copy_file(ctx: *NativeContext) NativeError!?Value {
    const src = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const dst = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    std.fs.cwd().copyFile(src, std.fs.cwd(), dst, .{}) catch |err| {
        debug.print(.general, "copy_file: failed to copy '{s}' to '{s}': {}", .{ src, dst, err });
        return Value.initBool(false);
    };

    debug.print(.general, "copy_file: copied '{s}' to '{s}'", .{ src, dst });
    return Value.initBool(true);
}

// =============================================================================
// Path Checks
// =============================================================================

/// exists(path) -> bool
/// Check if file or directory exists.
fn fs_exists(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const exists = blk: {
        std.fs.cwd().access(path, .{}) catch break :blk false;
        break :blk true;
    };

    return Value.initBool(exists);
}

/// is_file(path) -> bool
/// Check if path is a regular file.
fn fs_is_file(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    const stat = std.fs.cwd().statFile(path) catch {
        return Value.initBool(false);
    };

    return Value.initBool(stat.kind == .file);
}

/// is_dir(path) -> bool
/// Check if path is a directory.
fn fs_is_dir(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    var dir = std.fs.cwd().openDir(path, .{}) catch {
        return Value.initBool(false);
    };
    dir.close();

    return Value.initBool(true);
}

// =============================================================================
// Directory Operations
// =============================================================================

/// mkdir(path) -> bool
/// Create single directory (fails if parent doesn't exist).
fn fs_mkdir(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    std.fs.cwd().makeDir(path) catch |err| {
        debug.print(.general, "mkdir: failed for '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };

    debug.print(.general, "mkdir: created '{s}'", .{path});
    return Value.initBool(true);
}

/// mkdir_all(path) -> bool
/// Create directory and all parent directories (mkdir -p).
fn fs_mkdir_all(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    std.fs.cwd().makePath(path) catch |err| {
        debug.print(.general, "mkdir_all: failed for '{s}': {}", .{ path, err });
        return Value.initBool(false);
    };

    debug.print(.general, "mkdir_all: created '{s}'", .{path});
    return Value.initBool(true);
}

/// remove(path) -> bool
/// Remove file or empty directory.
fn fs_remove(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    // Try to delete as file first
    std.fs.cwd().deleteFile(path) catch |file_err| {
        // If not a file, try as directory
        if (file_err == error.IsDir) {
            std.fs.cwd().deleteDir(path) catch |dir_err| {
                debug.print(.general, "remove: failed for '{s}': {}", .{ path, dir_err });
                return Value.initBool(false);
            };
            debug.print(.general, "remove: deleted directory '{s}'", .{path});
            return Value.initBool(true);
        }
        debug.print(.general, "remove: failed for '{s}': {}", .{ path, file_err });
        return Value.initBool(false);
    };

    debug.print(.general, "remove: deleted file '{s}'", .{path});
    return Value.initBool(true);
}

/// remove_all(path) -> bool
/// Remove file or directory recursively (rm -rf).
fn fs_remove_all(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    // Try to delete as file first
    std.fs.cwd().deleteFile(path) catch |file_err| {
        if (file_err == error.IsDir) {
            // Delete directory tree
            std.fs.cwd().deleteTree(path) catch |err| {
                debug.print(.general, "remove_all: failed for '{s}': {}", .{ path, err });
                return Value.initBool(false);
            };
            debug.print(.general, "remove_all: deleted tree '{s}'", .{path});
            return Value.initBool(true);
        }
        debug.print(.general, "remove_all: failed for '{s}': {}", .{ path, file_err });
        return Value.initBool(false);
    };

    debug.print(.general, "remove_all: deleted file '{s}'", .{path});
    return Value.initBool(true);
}

/// read_dir(path) -> string
/// List directory contents as newline-separated string of entry names.
/// Throws FileError on failure.
fn fs_read_dir(ctx: *NativeContext) NativeError!?Value {
    const path = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch |err| {
        debug.print(.general, "read_dir: failed to open '{s}': {}", .{ path, err });
        return NativeError.FileError;
    };
    defer dir.close();

    // Collect entries as newline-separated string
    var result: std.ArrayListUnmanaged(u8) = .empty;
    defer result.deinit(ctx.allocator);

    var iter = dir.iterate();
    var first = true;
    while (iter.next() catch null) |entry| {
        if (!first) {
            result.append(ctx.allocator, '\n') catch return NativeError.OutOfMemory;
        }
        first = false;
        result.appendSlice(ctx.allocator, entry.name) catch return NativeError.OutOfMemory;
    }

    const owned = result.toOwnedSlice(ctx.allocator) catch return NativeError.OutOfMemory;
    return Value.initString(ctx.allocator, owned) catch return NativeError.OutOfMemory;
}

/// rename(old_path, new_path) -> bool
/// Rename/move file or directory.
fn fs_rename(ctx: *NativeContext) NativeError!?Value {
    const old_path = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const new_path = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    std.fs.cwd().rename(old_path, new_path) catch |err| {
        debug.print(.general, "rename: failed '{s}' -> '{s}': {}", .{ old_path, new_path, err });
        return Value.initBool(false);
    };

    debug.print(.general, "rename: '{s}' -> '{s}'", .{ old_path, new_path });
    return Value.initBool(true);
}

/// cwd() -> string
/// Get current working directory. Throws FileError on failure.
fn fs_cwd(ctx: *NativeContext) NativeError!?Value {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = std.fs.cwd().realpath(".", &buf) catch |err| {
        debug.print(.general, "cwd: failed to get: {}", .{err});
        return NativeError.FileError;
    };

    const owned = ctx.allocator.dupe(u8, path) catch return NativeError.OutOfMemory;
    return Value.initString(ctx.allocator, owned) catch return NativeError.OutOfMemory;
}
