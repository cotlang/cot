//! Runtime Selector
//!
//! Manages alternate runtime backends for executing Cot bytecode.
//! The default runtime is the built-in Zig VM, but alternate runtimes
//! can be selected via --runtime flag.
//!
//! Supported runtimes:
//!   zig (default) - Built-in Zig VM
//!   rs            - Rust runtime (cot-rs)
//!
//! The Rust runtime is discovered in order:
//!   1. Same directory as cot executable
//!   2. ~/.cot/bin/
//!   3. PATH

const std = @import("std");

pub const Runtime = enum {
    zig,
    rs,

    pub fn fromString(s: []const u8) ?Runtime {
        if (std.mem.eql(u8, s, "zig")) return .zig;
        if (std.mem.eql(u8, s, "rs")) return .rs;
        if (std.mem.eql(u8, s, "rust")) return .rs;
        return null;
    }
};

/// Find the cot-rs executable
/// Searches in order: same dir as cot, ~/.cot/bin/, PATH
pub fn findRustRuntime(allocator: std.mem.Allocator) ?[]const u8 {
    const name = "cot-rs";

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

    // 2. Check ~/.cot/bin/
    if (std.posix.getenv("HOME")) |home| {
        const path = std.fs.path.join(allocator, &[_][]const u8{ home, ".cot", "bin", name }) catch null;
        if (path) |p| {
            if (std.fs.cwd().access(p, .{})) |_| {
                return p;
            } else |_| {
                allocator.free(p);
            }
        }
    }

    // 3. Check PATH - return the name and let the OS find it
    return allocator.dupe(u8, name) catch null;
}

/// Run a bytecode file using the Rust runtime
pub fn runWithRustRuntime(allocator: std.mem.Allocator, bytecode_path: []const u8) !void {
    const exe_path = findRustRuntime(allocator) orelse {
        return error.RuntimeNotFound;
    };
    defer allocator.free(exe_path);

    // Build argv: [cot-rs, bytecode_path]
    var argv: std.ArrayListUnmanaged([]const u8) = .empty;
    defer argv.deinit(allocator);

    try argv.append(allocator, exe_path);
    try argv.append(allocator, bytecode_path);

    var child = std.process.Child.init(argv.items, allocator);

    // Inherit stdio for proper output forwarding
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    child.stdin_behavior = .Inherit;

    child.spawn() catch |err| {
        if (err == error.FileNotFound) {
            return error.RuntimeNotFound;
        }
        return err;
    };

    const result = try child.wait();

    // Propagate exit code or signal
    switch (result) {
        .Exited => |code| {
            if (code != 0) {
                std.process.exit(code);
            }
        },
        .Signal => |sig| {
            std.debug.print("Runtime terminated by signal: {d}\n", .{sig});
            std.process.exit(128 + @as(u8, @intCast(sig)));
        },
        .Stopped, .Unknown => {
            std.process.exit(1);
        },
    }
}

/// Parse --runtime=<name> from args, returns the runtime and remaining args
pub fn parseRuntimeArg(args: []const []const u8) struct { runtime: Runtime, remaining: []const []const u8 } {
    var runtime: Runtime = .zig; // default
    var start_idx: usize = 0;

    for (args, 0..) |arg, i| {
        if (std.mem.startsWith(u8, arg, "--runtime=")) {
            const runtime_name = arg[10..];
            if (Runtime.fromString(runtime_name)) |r| {
                runtime = r;
                start_idx = i + 1;
                break;
            }
        }
    }

    return .{
        .runtime = runtime,
        .remaining = args[start_idx..],
    };
}

test "runtime from string" {
    try std.testing.expect(Runtime.fromString("zig") == .zig);
    try std.testing.expect(Runtime.fromString("rs") == .rs);
    try std.testing.expect(Runtime.fromString("rust") == .rs);
    try std.testing.expect(Runtime.fromString("invalid") == null);
}
