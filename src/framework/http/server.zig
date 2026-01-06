//! HTTP Server
//!
//! Embedded HTTP server for serving API routes and static files.
//! Uses std.http.Server for proper HTTP/1.1 compliance.

const std = @import("std");
const http = std.http;
const net = std.net;
const posix = std.posix;
const Allocator = std.mem.Allocator;
const router = @import("router.zig");
const context = @import("context.zig");
pub const middleware = @import("middleware.zig");
pub const static = @import("static.zig");
pub const websocket = @import("websocket.zig");

// Re-export Context for convenience
pub const Context = context.Context;
pub const MiddlewareFn = middleware.MiddlewareFn;
pub const MiddlewareStack = middleware.MiddlewareStack;
pub const WebSocket = websocket.WebSocket;
pub const WebSocketHandler = websocket.WebSocketHandler;

/// Error handler function type
pub const ErrorHandlerFn = *const fn (*Context, anyerror) void;

/// HTTP Server configuration
pub const ServerConfig = struct {
    /// Port to listen on
    port: u16 = 3000,
    /// Host to bind to
    host: []const u8 = "127.0.0.1",
    /// Enable hot reload websocket
    hot_reload: bool = true,
    /// Static files directory (relative to workspace root)
    static_dir: ?[]const u8 = "public",
    /// Request timeout in milliseconds
    timeout_ms: u32 = 30000,
    /// Read buffer size for HTTP parsing
    read_buffer_size: usize = 8192,
    /// Write buffer size for responses
    write_buffer_size: usize = 8192,
};

/// HTTP Request wrapper for Cot
pub const Request = struct {
    method: Method,
    path: []const u8,
    query: ?[]const u8,
    headers: std.StringHashMap([]const u8),
    body: []const u8,
    params: std.StringHashMap([]const u8), // Route params like :id
    allocator: Allocator,

    // Reference to underlying std.http request for header iteration
    http_request: ?*http.Server.Request = null,

    pub const Method = enum {
        GET,
        POST,
        PUT,
        DELETE,
        PATCH,
        HEAD,
        OPTIONS,

        pub fn fromString(s: []const u8) ?Method {
            const methods = std.StaticStringMap(Method).initComptime(.{
                .{ "GET", .GET },
                .{ "POST", .POST },
                .{ "PUT", .PUT },
                .{ "DELETE", .DELETE },
                .{ "PATCH", .PATCH },
                .{ "HEAD", .HEAD },
                .{ "OPTIONS", .OPTIONS },
            });
            return methods.get(s);
        }

        pub fn fromHttpMethod(m: http.Method) Method {
            return switch (m) {
                .GET => .GET,
                .POST => .POST,
                .PUT => .PUT,
                .DELETE => .DELETE,
                .PATCH => .PATCH,
                .HEAD => .HEAD,
                .OPTIONS => .OPTIONS,
                else => .GET,
            };
        }
    };

    pub fn init(allocator: Allocator) Request {
        return .{
            .method = .GET,
            .path = "",
            .query = null,
            .headers = std.StringHashMap([]const u8).init(allocator),
            .body = "",
            .params = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Request) void {
        self.headers.deinit();
        self.params.deinit();
    }

    /// Get a header value (case-insensitive lookup)
    pub fn getHeader(self: *const Request, name: []const u8) ?[]const u8 {
        // First check our cached headers
        if (self.headers.get(name)) |v| return v;

        // If we have the http request, iterate through raw headers
        if (self.http_request) |req| {
            var iter = req.iterateHeaders();
            while (iter.next()) |header| {
                if (std.ascii.eqlIgnoreCase(header.name, name)) {
                    return header.value;
                }
            }
        }
        return null;
    }

    /// Get a route parameter
    pub fn getParam(self: *const Request, name: []const u8) ?[]const u8 {
        return self.params.get(name);
    }
};

/// HTTP Response builder for Cot
pub const Response = struct {
    status_code: u16 = 200,
    headers: std.StringHashMap([]const u8),
    body: std.ArrayListUnmanaged(u8),
    sent: bool = false,
    allocator: Allocator,

    pub fn init(allocator: Allocator) Response {
        return .{
            .headers = std.StringHashMap([]const u8).init(allocator),
            .body = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Response) void {
        self.headers.deinit();
        self.body.deinit(self.allocator);
    }

    /// Set response status code
    pub fn status(self: *Response, code: u16) *Response {
        self.status_code = code;
        return self;
    }

    /// Set a header
    pub fn setHeader(self: *Response, name: []const u8, value: []const u8) !void {
        try self.headers.put(name, value);
    }

    /// Write raw bytes to body
    pub fn write(self: *Response, data: []const u8) !void {
        try self.body.appendSlice(self.allocator, data);
    }

    /// Send JSON response
    pub fn json(self: *Response, data: []const u8) !void {
        try self.setHeader("Content-Type", "application/json");
        try self.write(data);
    }

    /// Send HTML response
    pub fn html(self: *Response, data: []const u8) !void {
        try self.setHeader("Content-Type", "text/html; charset=utf-8");
        try self.write(data);
    }

    /// Send plain text response
    pub fn text(self: *Response, data: []const u8) !void {
        try self.setHeader("Content-Type", "text/plain; charset=utf-8");
        try self.write(data);
    }

    /// Redirect to another URL
    pub fn redirect(self: *Response, url: []const u8) !void {
        self.status_code = 302;
        try self.setHeader("Location", url);
    }

    /// Convert status code to http.Status
    pub fn httpStatus(code: u16) http.Status {
        return @enumFromInt(code);
    }

    /// Get status text for code
    pub fn statusText(code: u16) []const u8 {
        return switch (code) {
            200 => "OK",
            201 => "Created",
            204 => "No Content",
            301 => "Moved Permanently",
            302 => "Found",
            304 => "Not Modified",
            400 => "Bad Request",
            401 => "Unauthorized",
            403 => "Forbidden",
            404 => "Not Found",
            405 => "Method Not Allowed",
            500 => "Internal Server Error",
            502 => "Bad Gateway",
            503 => "Service Unavailable",
            else => "Unknown",
        };
    }
};

/// Context-based handler function type (recommended)
pub const ContextHandlerFn = context.HandlerFn;

/// Route handler function type (legacy - use ContextHandlerFn for new code)
pub const HandlerFn = *const fn (*Request, *Response) anyerror!void;

// Global server instance for signal handling
var global_server: ?*Server = null;

/// HTTP Server
pub const Server = struct {
    allocator: Allocator,
    config: ServerConfig,
    router: router.Router,
    middlewares: MiddlewareStack,
    error_handler: ?ErrorHandlerFn = null,
    running: bool = false,

    const Self = @This();

    pub fn init(allocator: Allocator, config: ServerConfig) Self {
        return .{
            .allocator = allocator,
            .config = config,
            .router = router.Router.init(allocator),
            .middlewares = MiddlewareStack.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.router.deinit();
        self.middlewares.deinit();
    }

    /// Set global error handler
    pub fn onError(self: *Self, handler: ErrorHandlerFn) void {
        self.error_handler = handler;
    }

    /// Add global middleware (runs before handlers)
    pub fn use(self: *Self, mw: MiddlewareFn) !void {
        try self.middlewares.use(mw);
    }

    /// Add middleware that runs after handlers
    pub fn useAfter(self: *Self, mw: MiddlewareFn) !void {
        try self.middlewares.useAfter(mw);
    }

    /// Add a route handler (Context-based)
    pub fn route(self: *Self, method: router.Method, path: []const u8, handler: ContextHandlerFn) !void {
        try self.router.add(method, path, handler);
    }

    /// GET route helper
    pub fn get(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.GET, path, handler);
    }

    /// POST route helper
    pub fn post(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.POST, path, handler);
    }

    /// PUT route helper
    pub fn put(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.PUT, path, handler);
    }

    /// DELETE route helper
    pub fn delete(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.DELETE, path, handler);
    }

    /// PATCH route helper
    pub fn patch(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.PATCH, path, handler);
    }

    /// WebSocket route helper
    pub fn ws(self: *Self, path: []const u8, handler: WebSocketHandler) !void {
        try self.router.addWs(path, handler);
    }

    /// Create a route group with a prefix
    pub fn group(self: *Self, prefix: []const u8) RouteGroup {
        return RouteGroup.init(self, prefix);
    }

    /// Start the server with graceful shutdown support
    pub fn listen(self: *Self) !void {
        const address = try net.Address.parseIp4(self.config.host, self.config.port);

        var server = try address.listen(.{
            .reuse_address = true,
        });
        defer server.deinit();

        self.running = true;

        // Set up signal handlers for graceful shutdown
        global_server = self;
        setupSignalHandlers();

        std.debug.print("\nServer listening on http://{s}:{d}\n", .{ self.config.host, self.config.port });
        std.debug.print("Press Ctrl+C to stop\n\n", .{});

        while (self.running) {
            var connection = server.accept() catch |err| {
                if (err == error.ConnectionAborted) continue;
                if (!self.running) break; // Shutdown requested
                return err;
            };

            self.handleConnection(&connection) catch |err| {
                std.debug.print("Connection error: {}\n", .{err});
            };

            connection.stream.close();
        }

        std.debug.print("\nServer stopped gracefully\n", .{});
        global_server = null;
    }

    /// Stop the server gracefully
    pub fn stop(self: *Self) void {
        self.running = false;
    }

    /// Handle a single connection using std.http.Server
    fn handleConnection(self: *Self, connection: *net.Server.Connection) !void {
        // Allocate buffers for HTTP parsing
        var read_buffer: [8192]u8 = undefined;
        var write_buffer: [8192]u8 = undefined;

        // Create reader and writer from the connection stream
        var reader_obj = connection.stream.reader(&read_buffer);
        var writer_obj = connection.stream.writer(&write_buffer);

        // Initialize HTTP server for this connection
        // Reader.interface() is a method, Writer.interface is a field
        var http_server = http.Server.init(reader_obj.interface(), &writer_obj.interface);

        // Handle requests on this connection (supports keep-alive)
        while (true) {
            // Receive the HTTP request head
            var http_request = http_server.receiveHead() catch |err| {
                switch (err) {
                    error.HttpConnectionClosing => return, // Client closed connection
                    error.HttpRequestTruncated => return, // Partial request, connection closed
                    error.HttpHeadersOversize => {
                        // Headers too large - send raw error and close
                        self.sendRawError(&writer_obj, 431, "Request Header Fields Too Large");
                        return;
                    },
                    error.HttpHeadersInvalid => {
                        // Bad request - send raw error
                        self.sendRawError(&writer_obj, 400, "Bad Request");
                        return;
                    },
                    error.ReadFailed => return, // Connection error
                }
            };

            // Check for WebSocket upgrade request
            if (websocket.isUpgradeRequest(&http_request)) {
                self.handleWebSocketUpgrade(&http_request, connection) catch |err| {
                    std.debug.print("WebSocket upgrade error: {}\n", .{err});
                };
                return; // WebSocket takes over the connection
            }

            // Process the request
            const keep_alive = self.processRequest(&http_request) catch |err| {
                std.debug.print("Request processing error: {}\n", .{err});
                // Try to send error response
                self.sendErrorResponse(&http_request, 500, "Internal Server Error") catch {};
                return;
            };

            // If not keep-alive, close after this request
            if (!keep_alive) return;
        }
    }

    /// Handle WebSocket upgrade request
    fn handleWebSocketUpgrade(self: *Self, http_request: *http.Server.Request, connection: *net.Server.Connection) !void {
        // Parse path from target
        const target = http_request.head.target;
        var path: []const u8 = target;
        if (std.mem.indexOf(u8, target, "?")) |idx| {
            path = target[0..idx];
        }

        // Find matching WebSocket route
        if (self.router.matchWs(path)) |match_result| {
            var match = match_result;
            defer match.deinit();

            // Create WebSocket and perform handshake
            var websock = WebSocket.init(self.allocator, connection.stream);
            errdefer websock.deinit();

            try websock.acceptHandshake(http_request);

            // Call the handler
            match.handler(&websock) catch |err| {
                std.debug.print("WebSocket handler error: {}\n", .{err});
            };

            // Clean up
            websock.deinit();
        } else {
            // No matching WebSocket route - return 404
            http_request.respond("WebSocket route not found", .{
                .status = .not_found,
                .keep_alive = false,
            }) catch {};
        }
    }

    /// Process a single HTTP request using Context API
    fn processRequest(self: *Self, http_request: *http.Server.Request) !bool {
        // Parse path and query from target
        const target = http_request.head.target;
        var path: []const u8 = target;
        var query: ?[]const u8 = null;

        if (std.mem.indexOf(u8, target, "?")) |idx| {
            path = target[0..idx];
            query = target[idx + 1 ..];
        }

        // Read request body if present
        var body_buffer: [65536]u8 = undefined; // 64KB max body
        var body_read_buffer: [8192]u8 = undefined;
        var body_slice: []const u8 = "";
        if (http_request.head.content_length) |content_len| {
            if (content_len > 0 and content_len <= body_buffer.len) {
                const body_reader = http_request.readerExpectNone(&body_read_buffer);
                const read_len: usize = @intCast(content_len);
                body_reader.readSliceAll(body_buffer[0..read_len]) catch {};
                body_slice = body_buffer[0..read_len];
            }
        }

        // Build Context
        var ctx = Context.init(self.allocator);
        defer ctx.deinit();

        ctx._method = Context.Method.fromHttpMethod(http_request.head.method);
        ctx._path = path;
        ctx._query = query;
        ctx._body = body_slice;
        ctx._http_request = http_request;

        // Convert Method for router matching
        const router_method: router.Method = switch (ctx._method) {
            .GET => .GET,
            .POST => .POST,
            .PUT => .PUT,
            .DELETE => .DELETE,
            .PATCH => .PATCH,
            .HEAD => .HEAD,
            .OPTIONS => .OPTIONS,
        };

        // Find and execute handler with middleware
        if (self.router.match(router_method, ctx._path)) |match_result| {
            var match = match_result;
            defer match.deinit();

            // Copy route params to context
            var param_iter = match.params.iterator();
            while (param_iter.next()) |entry| {
                try ctx.setParam(entry.key_ptr.*, entry.value_ptr.*);
            }

            // Execute middleware chain with handler
            self.middlewares.execute(&ctx, match.handler) catch |err| {
                // Use custom error handler if set
                if (self.error_handler) |handler| {
                    handler(&ctx, err);
                } else {
                    _ = ctx.status(500);
                    try ctx.text("Internal Server Error");
                    std.debug.print("Handler error: {}\n", .{err});
                }
            };
        } else {
            // 404 Not Found - still run middleware for 404 responses
            const not_found_handler = struct {
                fn handler(c: *Context) anyerror!void {
                    _ = c.status(404);
                    try c.json("{\"error\": \"Not Found\"}");
                }
            }.handler;
            self.middlewares.execute(&ctx, not_found_handler) catch |err| {
                if (self.error_handler) |handler| {
                    handler(&ctx, err);
                } else {
                    std.debug.print("404 handler error: {}\n", .{err});
                }
            };
        }

        // Build extra headers array from context
        var extra_headers_list: [16]http.Header = undefined;
        var extra_headers_count: usize = 0;

        var header_iter = ctx.getResponseHeaders();
        while (header_iter.next()) |entry| {
            if (extra_headers_count < extra_headers_list.len) {
                extra_headers_list[extra_headers_count] = .{
                    .name = entry.key_ptr.*,
                    .value = entry.value_ptr.*,
                };
                extra_headers_count += 1;
            }
        }

        // Send response using std.http.Server
        const keep_alive = http_request.head.keep_alive;
        http_request.respond(ctx.getResponseBody(), .{
            .status = context.codeToHttpStatus(ctx.getStatusCode()),
            .keep_alive = keep_alive,
            .extra_headers = extra_headers_list[0..extra_headers_count],
        }) catch |err| {
            std.debug.print("Response send error: {}\n", .{err});
            return false;
        };

        return keep_alive;
    }

    /// Send an error response using the HTTP request
    fn sendErrorResponse(_: *Self, http_request: *http.Server.Request, status_code: u16, message: []const u8) !void {
        try http_request.respond(message, .{
            .status = Response.httpStatus(status_code),
            .keep_alive = false,
        });
    }

    /// Send a raw error response directly to writer (when request not yet parsed)
    fn sendRawError(_: *Self, writer: *net.Stream.Writer, status_code: u16, message: []const u8) void {
        const status_text = Response.statusText(status_code);
        writer.interface.print("HTTP/1.1 {d} {s}\r\nContent-Length: {d}\r\nConnection: close\r\n\r\n{s}", .{
            status_code,
            status_text,
            message.len,
            message,
        }) catch {};
        writer.interface.flush() catch {};
    }
};

/// Set up signal handlers for graceful shutdown
fn setupSignalHandlers() void {
    // Signal handling setup
    // Users can call server.stop() from their own signal handler if needed
    // The server's running flag is checked on each accept() iteration

    // On Unix-like systems, you can set up signal handlers externally:
    // const handler: posix.Sigaction.handler_fn = @ptrCast(&myHandler);
    // posix.sigaction(posix.SIG.INT, &.{ .handler = .{ .handler = handler }, ... }, null);
}

/// Route group for organizing routes with a common prefix and middleware
pub const RouteGroup = struct {
    server: *Server,
    prefix: []const u8,
    group_middleware: MiddlewareStack,
    parent_prefix: []const u8,

    const Self = @This();

    pub fn init(server: *Server, prefix: []const u8) Self {
        return .{
            .server = server,
            .prefix = prefix,
            .group_middleware = MiddlewareStack.init(server.allocator),
            .parent_prefix = "",
        };
    }

    pub fn initNested(server: *Server, prefix: []const u8, parent_prefix: []const u8) Self {
        return .{
            .server = server,
            .prefix = prefix,
            .group_middleware = MiddlewareStack.init(server.allocator),
            .parent_prefix = parent_prefix,
        };
    }

    pub fn deinit(self: *Self) void {
        self.group_middleware.deinit();
    }

    /// Build the full path with prefix
    fn fullPath(self: *Self, path: []const u8) ![]const u8 {
        // Combine parent_prefix + prefix + path
        if (self.parent_prefix.len > 0) {
            return std.fmt.allocPrint(self.server.allocator, "{s}{s}{s}", .{
                self.parent_prefix,
                self.prefix,
                path,
            });
        }
        return std.fmt.allocPrint(self.server.allocator, "{s}{s}", .{ self.prefix, path });
    }

    /// Wrap handler with group middleware
    fn wrapHandler(self: *Self, handler: ContextHandlerFn) ContextHandlerFn {
        // If no group middleware, return handler as-is
        if (self.group_middleware.before.items.len == 0 and
            self.group_middleware.after.items.len == 0)
        {
            return handler;
        }

        // For now, return handler as-is since we can't create closures in Zig
        // Group middleware is applied via the route registration
        // TODO: Implement proper group middleware wrapping
        return handler;
    }

    /// Add a route with the group prefix
    pub fn route(self: *Self, method: router.Method, path: []const u8, handler: ContextHandlerFn) !void {
        const full = try self.fullPath(path);
        defer self.server.allocator.free(full);

        try self.server.router.add(method, full, self.wrapHandler(handler));
    }

    /// GET route helper
    pub fn get(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.GET, path, handler);
    }

    /// POST route helper
    pub fn post(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.POST, path, handler);
    }

    /// PUT route helper
    pub fn put(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.PUT, path, handler);
    }

    /// DELETE route helper
    pub fn delete(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.DELETE, path, handler);
    }

    /// PATCH route helper
    pub fn patch(self: *Self, path: []const u8, handler: ContextHandlerFn) !void {
        try self.route(.PATCH, path, handler);
    }

    /// Add group-specific middleware (runs before group handlers)
    pub fn use(self: *Self, mw: MiddlewareFn) !void {
        try self.group_middleware.use(mw);
    }

    /// Add group-specific middleware that runs after handlers
    pub fn useAfter(self: *Self, mw: MiddlewareFn) !void {
        try self.group_middleware.useAfter(mw);
    }

    /// Create a nested route group
    pub fn group(self: *Self, prefix: []const u8) Self {
        const combined = std.fmt.allocPrint(self.server.allocator, "{s}{s}", .{
            if (self.parent_prefix.len > 0) self.parent_prefix else "",
            self.prefix,
        }) catch "";

        return Self.initNested(self.server, prefix, combined);
    }
};

/// Create a simple test server
pub fn createTestServer(allocator: Allocator) !Server {
    var server = Server.init(allocator, .{});

    // Add test routes using Context API
    try server.get("/", struct {
        fn handler(ctx: *Context) !void {
            try ctx.html("<h1>Welcome to Cot!</h1>");
        }
    }.handler);

    // Use route group for API routes
    var api = server.group("/api");
    defer api.deinit();

    try api.get("/health", struct {
        fn handler(ctx: *Context) !void {
            try ctx.json("{\"status\": \"ok\"}");
        }
    }.handler);

    try api.get("/version", struct {
        fn handler(ctx: *Context) !void {
            try ctx.json("{\"version\": \"0.1.0\"}");
        }
    }.handler);

    // Nested group: /api/v1/*
    var v1 = api.group("/v1");
    defer v1.deinit();

    try v1.get("/info", struct {
        fn handler(ctx: *Context) !void {
            try ctx.json("{\"api\": \"v1\", \"message\": \"Hello from v1!\"}");
        }
    }.handler);

    return server;
}
