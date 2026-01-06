//! cot dev command
//!
//! Starts the development server with hot reload.

const std = @import("std");
const config = @import("../config.zig");
const workspace = @import("../workspace.zig");
const discovery = @import("../discovery.zig");
const http_server = @import("../http/server.zig");
const http_router = @import("../http/router.zig");
const hotreload = @import("../http/hotreload.zig");
const dex_handler = @import("../http/dex_handler.zig");
const Allocator = std.mem.Allocator;

// Import compiler for on-the-fly compilation
const cot = @import("cot");

// Dex page handler - renders pages/index.dx
const DexPageHandler = struct {
    fn handler(ctx: *http_server.Context) !void {
        const alloc = ctx.allocator;
        const dex = @import("../dex/dex.zig");
        const page_path = "pages/index.dx";

        // Try to read pages/index.dx
        const source = readFile(alloc, page_path) catch {
            try ctx.html("<!DOCTYPE html><html><body><h1>Page not found</h1><p>Could not read pages/index.dx</p></body></html>");
            return;
        };
        defer alloc.free(source);

        // Create error context for detailed error reporting
        var error_context = dex.component_parser.ParseErrorContext.init(alloc);
        error_context.setSource(source);
        error_context.setFilePath(page_path);

        // Compile the component with error context
        var compiler = dex.compiler.Compiler.init(alloc);
        var component = compiler.compileSourceWithContext(source, &error_context) catch |err| {
            // Build detailed error message
            var error_html_buf: std.ArrayListUnmanaged(u8) = .empty;
            defer error_html_buf.deinit(alloc);

            const writer = error_html_buf.writer(alloc);
            try writer.print(
                \\<!DOCTYPE html>
                \\<html>
                \\<head>
                \\  <title>Parse Error</title>
                \\  <style>
                \\    body {{ font-family: system-ui; max-width: 800px; margin: 2rem auto; padding: 1rem; background: #1a1a2e; color: #eee; }}
                \\    h1 {{ color: #ff6b6b; }}
                \\    .error-box {{ background: #16213e; padding: 1rem; border-radius: 8px; border-left: 4px solid #ff6b6b; }}
                \\    .location {{ color: #a8a8a8; margin-bottom: 0.5rem; }}
                \\    .message {{ color: #ff6b6b; font-weight: bold; margin-bottom: 1rem; }}
                \\    pre {{ background: #0f0f1a; padding: 1rem; border-radius: 4px; overflow-x: auto; }}
                \\    .caret {{ color: #ff6b6b; font-weight: bold; }}
                \\  </style>
                \\</head>
                \\<body>
                \\  <h1>Parse Error</h1>
                \\  <div class="error-box">
                \\    <div class="location">File: {s}</div>
            , .{page_path});

            if (error_context.last_error) |parse_err| {
                try writer.print(
                    \\    <div class="location">Location: line {d}, column {d}</div>
                    \\    <div class="message">{s}</div>
                , .{ parse_err.line, parse_err.column, parse_err.message });

                if (parse_err.source_line) |line| {
                    try writer.print(
                        \\    <pre>
                    , .{});
                    try writer.print("{d:>3} | {s}\n", .{ parse_err.line, line });
                    try writer.print("    | ", .{});
                    var i: u32 = 1;
                    while (i < parse_err.column) : (i += 1) {
                        try writer.writeByte(' ');
                    }
                    try writer.print("<span class=\"caret\">^</span>\n", .{});
                    try writer.print("</pre>", .{});
                }
            } else {
                try writer.print(
                    \\    <div class="message">Error: {}</div>
                , .{err});
            }

            try writer.print(
                \\  </div>
                \\</body>
                \\</html>
            , .{});

            const error_html = try error_html_buf.toOwnedSlice(alloc);
            defer alloc.free(error_html);
            try ctx.html(error_html);
            return;
        };
        defer component.deinit();

        // Duplicate the name since it points into source memory which we'll free later
        const page_name = try alloc.dupe(u8, component.name);
        defer alloc.free(page_name);

        // Create instance
        var instance = component.createInstance() catch {
            try ctx.html("<!DOCTYPE html><html><body><h1>Error creating component</h1></body></html>");
            return;
        };
        defer instance.deinit();

        // Render content
        const content = instance.render() catch {
            try ctx.html("<!DOCTYPE html><html><body><h1>Error rendering page</h1></body></html>");
            return;
        };
        defer alloc.free(content);

        // Wrap in document shell
        // Note: hot_reload disabled for now - uses hardcoded port
        const html = dex.document.render(alloc, content, .{
            .title = page_name,
            .hot_reload = false,
        }) catch {
            try ctx.html("<!DOCTYPE html><html><body><h1>Error generating document</h1></body></html>");
            return;
        };
        defer alloc.free(html);

        try ctx.html(html);
    }

    fn readFile(alloc: Allocator, path: []const u8) ![]const u8 {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        return try file.readToEndAlloc(alloc, 1024 * 1024);
    }
};

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

/// WebSocket handler for Dex live events
fn dexEventHandler(ws: *http_server.WebSocket) !void {
    std.debug.print("Dex: WebSocket connected\n", .{});

    // Keep connection alive and handle messages
    while (ws.isOpen()) {
        // Read next message
        var msg = ws.readMessage() catch |err| {
            std.debug.print("Dex: WebSocket read error: {}\n", .{err});
            break;
        } orelse break;
        defer msg.deinit();

        // Parse the message
        const text = msg.text();
        std.debug.print("Dex: Received event: {s}\n", .{text});

        // For now, just acknowledge the event
        // TODO: Parse JSON, dispatch to component, re-render, send patch
        const response =
            \\{"type":"ack","message":"Event received"}
        ;
        ws.sendText(response) catch |err| {
            std.debug.print("Dex: WebSocket send error: {}\n", .{err});
            break;
        };
    }

    std.debug.print("Dex: WebSocket disconnected\n", .{});
}

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

    // Add middleware
    try server.use(http_server.middleware.logger());
    try server.use(http_server.middleware.cors());
    try server.use(http_server.static.serve(.{})); // Static file serving
    try server.useAfter(http_server.middleware.loggerAfter());

    // Set up error handler
    server.onError(struct {
        fn handler(ctx: *http_server.Context, err: anyerror) void {
            _ = ctx.status(500);
            ctx.json("{\"error\": \"Internal Server Error\"}") catch {};
            std.debug.print("Error: {}\n", .{err});
        }
    }.handler);

    // Register discovered API routes
    for (file_router.routes.items) |route| {
        // For now, register a placeholder handler
        // In a full implementation, this would compile and execute the .cot file
        try server.get(route.route, createApiHandler(allocator, route.path));
        try server.post(route.route, createApiHandler(allocator, route.path));
    }

    // Check if pages directory exists for Dex pages
    const pages_dir = blk: {
        // First, check if local pages/ exists (for running in app directory)
        std.fs.cwd().access("pages", .{}) catch {
            // Try each app's pages directory
            for (ws.apps.items) |app| {
                const pages_path = try std.fs.path.join(allocator, &.{ app.path, "pages" });
                defer allocator.free(pages_path);
                std.fs.cwd().access(pages_path, .{}) catch continue;
                break :blk try allocator.dupe(u8, pages_path);
            }
            // Default to workspace root pages
            break :blk try allocator.dupe(u8, "pages");
        };
        // Local pages/ exists - use it
        break :blk try allocator.dupe(u8, "pages");
    };
    defer allocator.free(pages_dir);

    // Check if pages/index.dx exists
    const index_dex_path = try std.fs.path.join(allocator, &.{ pages_dir, "index.dx" });
    defer allocator.free(index_dex_path);
    const has_index_page = blk: {
        std.fs.cwd().access(index_dex_path, .{}) catch break :blk false;
        break :blk true;
    };

    if (has_index_page) {
        try stdout.print("  Pages: {s}\n", .{pages_dir});
        // Use Dex page handler for homepage
        try server.get("/", DexPageHandler.handler);
    } else {
        // Default welcome page when no pages/index.dex exists
        try server.get("/", struct {
            fn handler(ctx: *http_server.Context) !void {
                try ctx.html(
                    \\<!DOCTYPE html>
                    \\<html>
                    \\<head>
                    \\  <title>Cot Dev Server</title>
                    \\  <style>
                    \\    body { font-family: system-ui; max-width: 800px; margin: 2rem auto; padding: 0 1rem; }
                    \\    h1 { color: #2563eb; }
                    \\    h2 { color: #1e40af; margin-top: 2rem; }
                    \\    code { background: #f1f5f9; padding: 0.2rem 0.4rem; border-radius: 0.25rem; }
                    \\    .status { color: #16a34a; font-weight: 500; }
                    \\    .dex-section { background: #eff6ff; padding: 1rem; border-radius: 0.5rem; margin: 1rem 0; }
                    \\  </style>
                    \\</head>
                    \\<body>
                    \\  <h1>Cot Development Server</h1>
                    \\  <p>Your Cot workspace is running! <span class="status">Hot reload enabled.</span></p>
                    \\  <div class="dex-section">
                    \\    <h2>Dex Components</h2>
                    \\    <p>Create real-time UI components with <code>.dex</code> files:</p>
                    \\    <pre><code>cot gen dex Counter</code></pre>
                    \\    <p>Create a page at <code>pages/index.dex</code> to replace this page.</p>
                    \\  </div>
                    \\  <h2>API Endpoints</h2>
                    \\  <p>Place <code>.cot</code> files in the <code>api/</code> directory to create endpoints.</p>
                    \\  <h2>Quick Links</h2>
                    \\  <ul>
                    \\    <li><a href="/api/health">Health Check</a></li>
                    \\    <li><a href="/api/version">Version Info</a></li>
                    \\  </ul>
                ++ hotreload.client_script ++
                    \\</body>
                    \\</html>
                );
            }
        }.handler);
    }

    // Use route group for API routes
    var api = server.group("/api");
    defer api.deinit();

    try api.get("/health", struct {
        fn handler(ctx: *http_server.Context) !void {
            try ctx.json("{\"status\": \"ok\", \"server\": \"cot\"}");
        }
    }.handler);

    try api.get("/version", struct {
        fn handler(ctx: *http_server.Context) !void {
            try ctx.json("{\"version\": \"0.1.0\", \"framework\": \"cot\"}");
        }
    }.handler);

    // Nested group for v1 API
    var v1 = api.group("/v1");
    defer v1.deinit();

    try v1.get("/status", struct {
        fn handler(ctx: *http_server.Context) !void {
            try ctx.json("{\"api\": \"v1\", \"status\": \"operational\"}");
        }
    }.handler);

    // TODO: WebSocket blocks the single-threaded server, disabling for now
    // try server.ws("/__dex_events", dexEventHandler);

    // Print routes
    try stdout.print("\n  Routes:\n", .{});
    try stdout.print("    GET  /              Homepage\n", .{});
    try stdout.print("    WS   /__dex_events  Dex events\n", .{});
    try stdout.print("    GET  /api/health    Health check\n", .{});
    try stdout.print("    GET  /api/version   Version info\n", .{});
    try stdout.print("    GET  /api/v1/status API v1 status\n", .{});
    for (file_router.routes.items) |route| {
        try stdout.print("    *    {s}\n", .{route.route});
    }

    try stdout.print("\n", .{});

    // Start server
    try server.listen();
}

/// Create a handler for an API route file
fn createApiHandler(allocator: Allocator, cot_path: []const u8) http_server.ContextHandlerFn {
    _ = allocator;
    _ = cot_path;
    // For now, return a placeholder handler
    // In a full implementation, this would:
    // 1. Compile the .cot file on first request (or use cached bytecode)
    // 2. Execute the appropriate handler function based on HTTP method
    // 3. Return the response
    return struct {
        fn handler(ctx: *http_server.Context) !void {
            try ctx.json("{\"message\": \"API endpoint not yet implemented\"}");
        }
    }.handler;
}

/// Create a handler for Dex pages
fn createDexPageHandler(allocator: Allocator, pages_dir: []const u8, port: u16) http_server.ContextHandlerFn {
    _ = allocator;
    _ = pages_dir;
    _ = port;
    // Simplified handler that loads and renders index.dex
    return struct {
        fn handler(ctx: *http_server.Context) !void {
            const alloc = ctx.allocator;

            // Try to read pages/index.dex
            const source = readFile(alloc, "pages/index.dx") catch {
                // Fallback to default page
                try ctx.html("<!DOCTYPE html><html><body><h1>Page not found</h1></body></html>");
                return;
            };
            defer alloc.free(source);

            // Compile and render
            const dex = @import("../dex/dex.zig");
            var compiler = dex.compiler.Compiler.init(alloc);
            var component = compiler.compileSource(source) catch {
                try ctx.html("<!DOCTYPE html><html><body><h1>Error compiling page</h1></body></html>");
                return;
            };
            defer component.deinit();

            var instance = component.createInstance() catch {
                try ctx.html("<!DOCTYPE html><html><body><h1>Error creating component</h1></body></html>");
                return;
            };
            defer instance.deinit();

            const content = instance.render() catch {
                try ctx.html("<!DOCTYPE html><html><body><h1>Error rendering page</h1></body></html>");
                return;
            };
            defer alloc.free(content);

            // Wrap in document
            const html = dex.document.render(alloc, content, .{
                .title = component.name,
                .hot_reload = false,
            }) catch {
                try ctx.html("<!DOCTYPE html><html><body><h1>Error generating document</h1></body></html>");
                return;
            };
            defer alloc.free(html);

            try ctx.html(html);
        }

        fn readFile(alloc: Allocator, path: []const u8) ![]const u8 {
            const file = try std.fs.cwd().openFile(path, .{});
            defer file.close();
            return try file.readToEndAlloc(alloc, 1024 * 1024);
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
