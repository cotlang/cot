//! Frontend Registry
//!
//! Manages external language frontends that can be dispatched via subprocess.
//! Frontends are discovered in:
//!   1. Same directory as cot executable
//!   2. ~/.cot/frontends/
//!   3. PATH
//!
//! Each frontend compiles its source to bytecode, which cot then executes.

const std = @import("std");

pub const Frontend = struct {
    name: []const u8,
    extensions: []const []const u8,
    executable: []const u8,
};

/// Built-in frontend mappings
const builtin_frontends = [_]Frontend{
    .{
        .name = "cot-dbl",
        .extensions = &[_][]const u8{ ".dbl", ".dbo" },
        .executable = "cot-dbl",
    },
};

/// Find the frontend for a given file extension
pub fn findFrontendForExtension(extension: []const u8) ?Frontend {
    for (builtin_frontends) |frontend| {
        for (frontend.extensions) |ext| {
            if (std.ascii.eqlIgnoreCase(extension, ext)) {
                return frontend;
            }
        }
    }
    return null;
}

/// Find the frontend executable path
/// Searches in order: same dir as cot, ~/.cot/frontends/, PATH
pub fn findExecutable(allocator: std.mem.Allocator, name: []const u8) ?[]const u8 {
    // 1. Check same directory as cot executable
    var self_exe_buf: [std.fs.max_path_bytes]u8 = undefined;
    if (std.fs.selfExeDirPath(&self_exe_buf)) |self_dir| {
        const path = std.fs.path.join(allocator, &[_][]const u8{ self_dir, name }) catch null;
        if (path) |p| {
            if (std.fs.cwd().access(p, .{})) |_| {
                return p;
            } else |_| {
                allocator.free(p);
            }
        }
    } else |_| {}

    // 2. Check ~/.cot/frontends/
    if (std.posix.getenv("HOME")) |home| {
        const path = std.fs.path.join(allocator, &[_][]const u8{ home, ".cot", "frontends", name }) catch null;
        if (path) |p| {
            if (std.fs.cwd().access(p, .{})) |_| {
                return p;
            } else |_| {
                allocator.free(p);
            }
        }
    }

    // 3. Check PATH - just return the name and let the OS find it
    // We'll verify it exists by trying to spawn it
    return allocator.dupe(u8, name) catch null;
}

/// Run a frontend on a source file
/// The frontend should compile and execute the file
pub fn runFrontend(allocator: std.mem.Allocator, frontend: Frontend, args: []const []const u8) !void {
    const exe_path = findExecutable(allocator, frontend.executable) orelse {
        return error.FrontendNotFound;
    };
    defer allocator.free(exe_path);

    // Build argv: [frontend_exe, original_args...]
    var argv: std.ArrayListAligned([]const u8, null) = .empty;
    defer argv.deinit(allocator);

    try argv.append(allocator, exe_path);
    for (args) |arg| {
        try argv.append(allocator, arg);
    }

    var child = std.process.Child.init(argv.items, allocator);
    child.spawn() catch |err| {
        if (err == error.FileNotFound) {
            return error.FrontendNotFound;
        }
        return err;
    };
    const result = try child.wait();

    // Propagate exit code
    if (result.Exited != 0) {
        std.process.exit(result.Exited);
    }
}

/// Get the file extension from a filename
pub fn getExtension(filename: []const u8) ?[]const u8 {
    const last_dot = std.mem.lastIndexOfScalar(u8, filename, '.') orelse return null;
    return filename[last_dot..];
}

test "find frontend for dbl" {
    const frontend = findFrontendForExtension(".dbl");
    try std.testing.expect(frontend != null);
    try std.testing.expectEqualStrings("cot-dbl", frontend.?.name);
}

test "get extension" {
    try std.testing.expectEqualStrings(".dbl", getExtension("test.dbl").?);
    try std.testing.expectEqualStrings(".cot", getExtension("path/to/file.cot").?);
    try std.testing.expect(getExtension("noextension") == null);
}
