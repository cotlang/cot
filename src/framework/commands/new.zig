//! cot new command
//!
//! Creates a new app or package in the workspace.

const std = @import("std");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const Allocator = std.mem.Allocator;

pub const Template = enum {
    app,
    library,
};

pub const Syntax = enum {
    cot,
    dbl,
};

pub const NewOptions = struct {
    name: ?[]const u8 = null,
    template: Template = .app,
    syntax: Syntax = .cot,
};

/// Create a new app or package
pub fn run(allocator: Allocator, options: NewOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    const name = options.name orelse {
        try stdout.print("Error: Please provide a name for the new project.\n", .{});
        try stdout.print("Usage: cot new <name> [--template app|library]\n", .{});
        return error.MissingName;
    };

    // Find workspace root
    var loader = workspace.WorkspaceLoader.init(allocator);
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    var config_loader = config.ConfigLoader.init(allocator);
    const workspace_root = try config_loader.findWorkspaceRoot(cwd) orelse {
        try stdout.print("Error: Not inside a Cot workspace.\n", .{});
        try stdout.print("Run 'cot init' first to create a workspace.\n", .{});
        return error.NoWorkspace;
    };
    defer allocator.free(workspace_root);

    switch (options.template) {
        .app => {
            try loader.createApp(workspace_root, name, options.syntax == .dbl);
            try stdout.print("\n", .{});
            try stdout.print("  Created app: {s}\n", .{name});
            try stdout.print("  Location: apps/{s}/\n", .{name});
            try stdout.print("\n", .{});
            try stdout.print("  Next steps:\n", .{});
            try stdout.print("    cot build {s}        Build the app\n", .{name});
            try stdout.print("    cot run {s}          Run the app\n", .{name});
            try stdout.print("\n", .{});
            try stdout.flush();
        },
        .library => {
            try loader.createPackage(workspace_root, name, options.syntax == .dbl);
            try stdout.print("\n", .{});
            try stdout.print("  Created package: @company/{s}\n", .{name});
            try stdout.print("\n", .{});
            try stdout.print("  Location: packages/{s}/\n", .{name});
            try stdout.print("\n", .{});
            try stdout.print("  Structure:\n", .{});
            try stdout.print("    packages/{s}/\n", .{name});
            try stdout.print("    ├── cot.json\n", .{});
            try stdout.print("    └── src/\n", .{});
            try stdout.print("        └── index.cot\n", .{});
            try stdout.print("\n", .{});
            try stdout.print("  Usage in apps:\n", .{});
            try stdout.print("    Add to dependencies in cot.json:\n", .{});
            try stdout.print("    \"@company/{s}\": \"workspace:*\"\n", .{name});
            try stdout.print("\n", .{});
        },
    }
}

/// Parse new command arguments
pub fn parseArgs(args: []const []const u8) NewOptions {
    var options = NewOptions{};
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--template") or std.mem.eql(u8, arg, "-t")) {
            if (i + 1 < args.len) {
                i += 1;
                if (std.mem.eql(u8, args[i], "app")) {
                    options.template = .app;
                } else if (std.mem.eql(u8, args[i], "library") or std.mem.eql(u8, args[i], "lib")) {
                    options.template = .library;
                }
            }
        } else if (std.mem.eql(u8, arg, "--app")) {
            options.template = .app;
        } else if (std.mem.eql(u8, arg, "--library") or std.mem.eql(u8, arg, "--lib")) {
            options.template = .library;
        } else if (std.mem.eql(u8, arg, "--dbl")) {
            options.syntax = .dbl;
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            if (options.name == null) {
                options.name = arg;
            }
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
        \\Usage: cot new <name> [options]
        \\
        \\Create a new app or package in the workspace.
        \\
        \\Arguments:
        \\  name              Name of the new app or package
        \\
        \\Options:
        \\  -t, --template    Template type: app (default) or library
        \\  --app             Shorthand for --template app
        \\  --library, --lib  Shorthand for --template library
        \\  --dbl             Use DBL syntax instead of modern Cot syntax
        \\  --help            Show this help
        \\
        \\Examples:
        \\  cot new inventory                    Create app with Cot syntax
        \\  cot new inventory --dbl              Create app with DBL syntax
        \\  cot new common --template library    Create a shared package
        \\  cot new utils --lib                  Create a shared package (shorthand)
        \\
    );
}
