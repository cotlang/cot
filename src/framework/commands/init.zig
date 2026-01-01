//! cot init command
//!
//! Initializes a new Cot workspace in the current directory.

const std = @import("std");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const Allocator = std.mem.Allocator;

pub const InitOptions = struct {
    name: ?[]const u8 = null,
    force: bool = false,
};

/// Initialize a new workspace in the current directory
pub fn run(allocator: Allocator, options: InitOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    // Check if cot.json already exists
    if (config.ConfigLoader.exists(cwd) and !options.force) {
        try stdout.print("Error: cot.json already exists in this directory.\n", .{});
        try stdout.print("Use --force to overwrite.\n", .{});
        return error.AlreadyExists;
    }

    // Determine project name
    const name = options.name orelse std.fs.path.basename(cwd);

    // Create workspace
    var loader = workspace.WorkspaceLoader.init(allocator);
    try loader.create(cwd, name);

    try stdout.print("\n", .{});
    try stdout.print("  Initialized Cot workspace: {s}\n", .{name});
    try stdout.print("\n", .{});
    try stdout.print("  Created:\n", .{});
    try stdout.print("    cot.json\n", .{});
    try stdout.print("    apps/\n", .{});
    try stdout.print("    packages/\n", .{});
    try stdout.print("\n", .{});
    try stdout.print("  Next steps:\n", .{});
    try stdout.print("    cot new my-app --template app      Create a new app\n", .{});
    try stdout.print("    cot new common --template library  Create a shared package\n", .{});
    try stdout.print("    cot dev                            Start development server\n", .{});
    try stdout.print("\n", .{});
}

/// Parse init command arguments
pub fn parseArgs(args: []const []const u8) InitOptions {
    var options = InitOptions{};
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--force") or std.mem.eql(u8, arg, "-f")) {
            options.force = true;
        } else if (std.mem.eql(u8, arg, "--name") or std.mem.eql(u8, arg, "-n")) {
            if (i + 1 < args.len) {
                i += 1;
                options.name = args[i];
            }
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            options.name = arg;
        }
    }

    return options;
}

pub fn printHelp() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(
        \\Usage: cot init [name] [options]
        \\
        \\Initialize a new Cot workspace in the current directory.
        \\
        \\Arguments:
        \\  name          Workspace name (defaults to directory name)
        \\
        \\Options:
        \\  -f, --force   Overwrite existing cot.json
        \\  -n, --name    Specify workspace name
        \\  --help        Show this help
        \\
        \\Examples:
        \\  cot init
        \\  cot init my-company
        \\  cot init --name my-company --force
        \\
    );
}
