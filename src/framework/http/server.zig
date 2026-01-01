//! HTTP Server
//!
//! Embedded HTTP server for serving API routes and static files.
//! Inspired by Next.js dev server functionality.

const std = @import("std");
const Allocator = std.mem.Allocator;
const router = @import("router.zig");

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
};

/// HTTP Request wrapper for Cot
pub const Request = struct {
    method: Method,
    path: []const u8,
    query: ?[]const u8,
    headers: std.StringHashMap([]const u8),
    body: []const u8,
    params: std.StringHashMap([]const u8), // Route params like :id

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
    };

    pub fn init(allocator: Allocator) Request {
        return .{
            .method = .GET,
            .path = "",
            .query = null,
            .headers = std.StringHashMap([]const u8).init(allocator),
            .body = "",
            .params = std.StringHashMap([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *Request) void {
        self.headers.deinit();
        self.params.deinit();
    }

    /// Get a header value
    pub fn getHeader(self: *const Request, name: []const u8) ?[]const u8 {
        return self.headers.get(name);
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
    body: std.ArrayList(u8),
    sent: bool = false,
    allocator: Allocator,

    pub fn init(allocator: Allocator) Response {
        return .{
            .headers = std.StringHashMap([]const u8).init(allocator),
            .body = .{},
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

/// Route handler function type
pub const HandlerFn = *const fn (*Request, *Response) anyerror!void;

/// HTTP Server
pub const Server = struct {
    allocator: Allocator,
    config: ServerConfig,
    router: router.Router,
    running: bool = false,

    const Self = @This();

    pub fn init(allocator: Allocator, config: ServerConfig) Self {
        return .{
            .allocator = allocator,
            .config = config,
            .router = router.Router.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.router.deinit();
    }

    /// Add a route handler
    pub fn route(self: *Self, method: Request.Method, path: []const u8, handler: HandlerFn) !void {
        try self.router.add(method, path, handler);
    }

    /// GET route helper
    pub fn get(self: *Self, path: []const u8, handler: HandlerFn) !void {
        try self.route(.GET, path, handler);
    }

    /// POST route helper
    pub fn post(self: *Self, path: []const u8, handler: HandlerFn) !void {
        try self.route(.POST, path, handler);
    }

    /// PUT route helper
    pub fn put(self: *Self, path: []const u8, handler: HandlerFn) !void {
        try self.route(.PUT, path, handler);
    }

    /// DELETE route helper
    pub fn delete(self: *Self, path: []const u8, handler: HandlerFn) !void {
        try self.route(.DELETE, path, handler);
    }

    /// Start the server
    pub fn listen(self: *Self) !void {
        const address = try std.net.Address.parseIp4(self.config.host, self.config.port);

        var server = try address.listen(.{
            .reuse_address = true,
        });
        defer server.deinit();

        self.running = true;

        std.debug.print("\nServer listening on http://{s}:{d}\n", .{ self.config.host, self.config.port });
        std.debug.print("Press Ctrl+C to stop\n\n", .{});

        while (self.running) {
            var connection = server.accept() catch |err| {
                if (err == error.ConnectionAborted) continue;
                return err;
            };

            self.handleConnection(&connection) catch |err| {
                std.debug.print("Connection error: {}\n", .{err});
            };
        }
    }

    /// Stop the server
    pub fn stop(self: *Self) void {
        self.running = false;
    }

    /// Handle a single connection
    fn handleConnection(self: *Self, connection: *std.net.Server.Connection) !void {
        defer connection.stream.close();

        // Read request
        var read_buffer: [8192]u8 = undefined;
        const bytes_read = connection.stream.read(&read_buffer) catch |err| {
            std.debug.print("Read error: {}\n", .{err});
            return;
        };

        if (bytes_read == 0) return;

        const request_data = read_buffer[0..bytes_read];

        // Parse HTTP request line
        var req = Request.init(self.allocator);
        defer req.deinit();

        // Find first line
        const first_line_end = std.mem.indexOf(u8, request_data, "\r\n") orelse return;
        const first_line = request_data[0..first_line_end];

        // Parse "GET /path HTTP/1.1"
        var parts = std.mem.splitScalar(u8, first_line, ' ');
        const method_str = parts.next() orelse return;
        const path_str = parts.next() orelse return;

        req.method = Request.Method.fromString(method_str) orelse .GET;

        // Parse path and query
        if (std.mem.indexOf(u8, path_str, "?")) |idx| {
            req.path = path_str[0..idx];
            req.query = path_str[idx + 1 ..];
        } else {
            req.path = path_str;
        }

        // Build response
        var res = Response.init(self.allocator);
        defer res.deinit();

        // Find and execute handler
        if (self.router.match(req.method, req.path)) |match_result| {
            var match = match_result;
            defer match.deinit();

            // Copy route params
            var param_iter = match.params.iterator();
            while (param_iter.next()) |entry| {
                try req.params.put(entry.key_ptr.*, entry.value_ptr.*);
            }

            // Call handler
            match.handler(&req, &res) catch |err| {
                res.status_code = 500;
                try res.text("Internal Server Error");
                std.debug.print("Handler error: {}\n", .{err});
            };
        } else {
            // 404 Not Found
            res.status_code = 404;
            try res.json("{\"error\": \"Not Found\"}");
        }

        // Send response
        try self.sendResponse(connection, &res);
    }

    /// Send HTTP response
    fn sendResponse(_: *Self, connection: *std.net.Server.Connection, res: *Response) !void {
        // Build response
        var response_buf: [16384]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&response_buf);
        const writer = fbs.writer();

        // Status line
        try writer.print("HTTP/1.1 {d} {s}\r\n", .{ res.status_code, Response.statusText(res.status_code) });

        // Headers
        try writer.print("Content-Length: {d}\r\n", .{res.body.items.len});

        var header_iter = res.headers.iterator();
        while (header_iter.next()) |entry| {
            try writer.print("{s}: {s}\r\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }

        // End headers
        try writer.writeAll("\r\n");

        // Body
        try writer.writeAll(res.body.items);

        // Send
        _ = connection.stream.write(fbs.getWritten()) catch |err| {
            std.debug.print("Write error: {}\n", .{err});
        };
    }
};

/// Create a simple test server
pub fn createTestServer(allocator: Allocator) !Server {
    var server = Server.init(allocator, .{});

    // Add test routes
    try server.get("/", struct {
        fn handler(_: *Request, res: *Response) !void {
            try res.html("<h1>Welcome to Cot!</h1>");
        }
    }.handler);

    try server.get("/api/health", struct {
        fn handler(_: *Request, res: *Response) !void {
            try res.json("{\"status\": \"ok\"}");
        }
    }.handler);

    return server;
}
