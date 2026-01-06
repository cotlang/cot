//! Package Dependencies Command
//!
//! Shows the package dependency graph for a workspace.
//! Usage: cot deps [--json]

const std = @import("std");
const package = @import("../package.zig");

pub const DepsOptions = struct {
    /// Output format
    json: bool = false,
    /// Workspace root (defaults to current directory)
    workspace_root: ?[]const u8 = null,
};

/// Run the deps command
pub fn run(allocator: std.mem.Allocator, options: DepsOptions) !void {
    const workspace_root = options.workspace_root orelse ".";

    // Get absolute path
    const abs_root = try std.fs.cwd().realpathAlloc(allocator, workspace_root);
    defer allocator.free(abs_root);

    // Initialize package manager and discover packages
    var pm = try package.PackageManager.init(allocator, abs_root);
    defer pm.deinit();

    try pm.discoverPackages();

    // Scan source files to find dependencies (imports)
    try scanDependencies(allocator, &pm);

    // Compute build order
    pm.computeBuildOrder() catch |err| {
        if (err == error.CircularDependency) {
            try printStderr("Error: Circular dependency detected in packages\n", .{});
            return error.CircularDependency;
        }
        return err;
    };

    // Output results
    if (options.json) {
        try outputJson(allocator, &pm);
    } else {
        try outputText(&pm);
    }
}

/// Scan source files to find import dependencies
fn scanDependencies(allocator: std.mem.Allocator, pm: *package.PackageManager) !void {
    var it = pm.packages.iterator();
    while (it.next()) |entry| {
        const pkg = entry.value_ptr.*;

        for (pkg.source_files.items) |source_file| {
            try scanFileForImports(allocator, pm, pkg, source_file);
        }
    }
}

/// Scan a single file for import statements
fn scanFileForImports(
    allocator: std.mem.Allocator,
    pm: *package.PackageManager,
    pkg: *package.Package,
    file_path: []const u8,
) !void {
    const file = std.fs.cwd().openFile(file_path, .{}) catch return;
    defer file.close();

    const content = file.readToEndAlloc(allocator, 10 * 1024 * 1024) catch return;
    defer allocator.free(content);

    // Simple import scanning - look for `import "..."` patterns
    var i: usize = 0;
    while (i < content.len) {
        // Look for 'import' keyword
        if (i + 6 < content.len and std.mem.eql(u8, content[i..][0..6], "import")) {
            i += 6;

            // Skip whitespace
            while (i < content.len and (content[i] == ' ' or content[i] == '\t')) {
                i += 1;
            }

            // Check for quote
            if (i < content.len and content[i] == '"') {
                i += 1;
                const start = i;

                // Find end quote
                while (i < content.len and content[i] != '"') {
                    i += 1;
                }

                if (i < content.len) {
                    const import_path = content[start..i];

                    // Resolve import to package name
                    if (try pm.resolveImport(pkg.name, import_path)) |dep_name| {
                        // Don't add self-dependency
                        if (!std.mem.eql(u8, dep_name, pkg.name)) {
                            try pkg.addDependency(dep_name);
                        }
                    }
                }
            }
        }
        i += 1;
    }
}

/// Output package graph as JSON
fn outputJson(allocator: std.mem.Allocator, pm: *const package.PackageManager) !void {
    var buf: [8192]u8 = undefined;
    const stdout_file: std.fs.File = .stdout();
    var writer = stdout_file.writer(&buf);
    const stdout = &writer.interface;

    try stdout.writeAll("{\n  \"packages\": {\n");

    var first_pkg = true;
    var it = pm.packages.iterator();
    while (it.next()) |entry| {
        const pkg = entry.value_ptr.*;

        if (!first_pkg) try stdout.writeAll(",\n");
        first_pkg = false;

        try stdout.print("    \"{s}\": {{\n", .{pkg.name});
        try stdout.print("      \"path\": \"{s}\",\n", .{pkg.path});
        try stdout.print("      \"files\": {d},\n", .{pkg.source_files.items.len});
        try stdout.writeAll("      \"dependencies\": [");

        for (pkg.dependencies.items, 0..) |dep, i| {
            if (i > 0) try stdout.writeAll(", ");
            try stdout.print("\"{s}\"", .{dep});
        }
        try stdout.writeAll("]\n    }");
    }

    try stdout.writeAll("\n  },\n  \"build_order\": [");
    for (pm.build_order.items, 0..) |name, i| {
        if (i > 0) try stdout.writeAll(", ");
        try stdout.print("\"{s}\"", .{name});
    }
    try stdout.writeAll("]\n}\n");
    try stdout.flush();
    _ = allocator;
}

/// Output package graph as text
fn outputText(pm: *const package.PackageManager) !void {
    var buf: [8192]u8 = undefined;
    const stdout_file: std.fs.File = .stdout();
    var writer = stdout_file.writer(&buf);
    const stdout = &writer.interface;
    try pm.printDependencyGraph(stdout);
    try stdout.flush();
}

fn printStderr(comptime fmt: []const u8, args: anytype) !void {
    var buf: [4096]u8 = undefined;
    const stderr_file: std.fs.File = .stderr();
    var writer = stderr_file.writer(&buf);
    try writer.interface.print(fmt, args);
}
