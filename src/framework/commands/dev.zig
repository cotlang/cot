//! cot dev command
//!
//! Starts the development server with hot reload.

const std = @import("std");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const discovery = @import("../discovery.zig");
const http_server = @import("../http/server.zig");
const http_router = @import("../http/router.zig");
const Allocator = std.mem.Allocator;

// Import compiler for on-the-fly compilation
const cot = @import("cot");

pub const DevOptions = struct {
    port: u16 = 3000,
    host: []const u8 = "127.0.0.1",
    project: ?[]const u8 = null,
    filter: ?[]const u8 = null, // --filter=<project> like Turborepo
    show_help: bool = false,

    /// Get effective project filter
    pub fn getProjectFilter(self: DevOptions) ?[]const u8 {
        return self.filter orelse self.project;
    }
};

/// Run the dev server
pub fn run(allocator: Allocator, options: DevOptions) !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    if (options.show_help) {
        try printHelp();
        return;
    }

    // Find workspace
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);

    var config_loader = config.ConfigLoader.init(allocator);
    const workspace_root = try config_loader.findWorkspaceRoot(cwd) orelse {
        try stdout.print("Error: Not inside a Cot workspace.\n", .{});
        try stdout.print("Run 'cot init' first to create a workspace.\n", .{});
        return error.NoWorkspace;
    };
    defer allocator.free(workspace_root);

    // Load workspace
    var ws_loader = workspace.WorkspaceLoader.init(allocator);
    var ws = try ws_loader.load(workspace_root);
    defer ws.deinit();

    try stdout.print("\n", .{});
    try stdout.print("  Cot Development Server\n", .{});
    try stdout.print("  ──────────────────────\n", .{});
    try stdout.print("  Workspace: {s}\n", .{ws.name});

    // Discover API routes
    var file_router = http_router.FileRouter.init(allocator);
    defer file_router.deinit();

    // Discover from each app
    for (ws.apps.items) |app| {
        const api_dir = try std.fs.path.join(allocator, &.{ app.path, "api" });
        defer allocator.free(api_dir);

        try file_router.discover(api_dir);
    }

    try stdout.print("  API Routes: {d}\n", .{file_router.routes.items.len});

    // Create HTTP server
    var server = http_server.Server.init(allocator, .{
        .port = options.port,
        .host = options.host,
    });
    defer server.deinit();

    // Register discovered API routes
    for (file_router.routes.items) |route| {
        // For now, register a placeholder handler
        // In a full implementation, this would compile and execute the .cot file
        try server.get(route.route, createApiHandler(allocator, route.path));
        try server.post(route.route, createApiHandler(allocator, route.path));
    }

    // Add built-in routes
    try server.get("/", struct {
        fn handler(_: *http_server.Request, res: *http_server.Response) !void {
            try res.html(
                \\<!DOCTYPE html>
                \\<html>
                \\<head>
                \\  <title>Cot Dev Server</title>
                \\  <style>
                \\    body { font-family: system-ui; max-width: 800px; margin: 2rem auto; padding: 0 1rem; }
                \\    h1 { color: #2563eb; }
                \\    code { background: #f1f5f9; padding: 0.2rem 0.4rem; border-radius: 0.25rem; }
                \\  </style>
                \\</head>
                \\<body>
                \\  <h1>Cot Development Server</h1>
                \\  <p>Your Cot workspace is running!</p>
                \\  <h2>API Endpoints</h2>
                \\  <p>Place <code>.cot</code> files in the <code>api/</code> directory to create endpoints.</p>
                \\  <h2>Quick Links</h2>
                \\  <ul>
                \\    <li><a href="/api/health">Health Check</a></li>
                \\  </ul>
                \\</body>
                \\</html>
            );
        }
    }.handler);

    try server.get("/api/health", struct {
        fn handler(_: *http_server.Request, res: *http_server.Response) !void {
            try res.json("{\"status\": \"ok\", \"server\": \"cot\"}");
        }
    }.handler);

    // Print routes
    try stdout.print("\n  Routes:\n", .{});
    try stdout.print("    GET  /              Homepage\n", .{});
    try stdout.print("    GET  /api/health    Health check\n", .{});
    for (file_router.routes.items) |route| {
        try stdout.print("    *    {s}\n", .{route.route});
    }

    try stdout.print("\n", .{});

    // Start server
    try server.listen();
}

/// Create a handler for an API route file
fn createApiHandler(allocator: Allocator, cot_path: []const u8) http_server.HandlerFn {
    _ = allocator;
    _ = cot_path;
    // For now, return a placeholder handler
    // In a full implementation, this would:
    // 1. Compile the .cot file on first request (or use cached bytecode)
    // 2. Execute the appropriate handler function based on HTTP method
    // 3. Return the response
    return struct {
        fn handler(_: *http_server.Request, res: *http_server.Response) !void {
            try res.json("{\"message\": \"API endpoint not yet implemented\"}");
        }
    }.handler;
}

/// Parse dev command arguments
pub fn parseArgs(args: []const []const u8) DevOptions {
    var options = DevOptions{};
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            options.show_help = true;
        } else if (std.mem.eql(u8, arg, "--port") or std.mem.eql(u8, arg, "-p")) {
            if (i + 1 < args.len) {
                i += 1;
                options.port = std.fmt.parseInt(u16, args[i], 10) catch 3000;
            }
        } else if (std.mem.eql(u8, arg, "--host")) {
            if (i + 1 < args.len) {
                i += 1;
                options.host = args[i];
            }
        } else if (std.mem.startsWith(u8, arg, "--filter=")) {
            // Turborepo-style --filter=<project>
            options.filter = arg[9..];
        } else if (std.mem.eql(u8, arg, "--filter") or std.mem.eql(u8, arg, "-f")) {
            // --filter <project> (separate arg)
            if (i + 1 < args.len) {
                i += 1;
                options.filter = args[i];
            }
        } else if (!std.mem.startsWith(u8, arg, "-")) {
            if (options.project == null) {
                options.project = arg;
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
        \\Usage: cot dev [options]
        \\
        \\Start the development server with hot reload.
        \\
        \\Options:
        \\  -p, --port <port>    Port to listen on (default: 3000)
        \\  --host <host>        Host to bind to (default: 127.0.0.1)
        \\  -h, --help           Show this help
        \\
        \\Features:
        \\  - Automatic API route discovery from api/ directories
        \\  - Hot reload on file changes (coming soon)
        \\  - Built-in health check endpoint
        \\
        \\Examples:
        \\  cot dev                    Start on default port 3000
        \\  cot dev --port 8080        Start on port 8080
        \\  cot dev --host 0.0.0.0     Listen on all interfaces
        \\
    );
}
