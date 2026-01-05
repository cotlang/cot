//! HTTP Server Native Functions
//!
//! Exposes HTTP server functionality to Cot code with closure-based handlers.
//! Uses Zig's std.http.Server for the underlying implementation.
//!
//! Namespace: std.http.server
//!
//! Example usage:
//!   import std.http.server
//!
//!   fn main() {
//!       var app = http.server.new()
//!       app.get("/", fn(ctx) { ctx.text("Hello!") })
//!       app.listen(3000)
//!   }

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;
const Allocator = std.mem.Allocator;

/// HTTP Methods
pub const Method = enum {
    GET,
    POST,
    PUT,
    DELETE,
    PATCH,
    OPTIONS,
    HEAD,

    pub fn fromStd(method: std.http.Method) Method {
        return switch (method) {
            .GET => .GET,
            .POST => .POST,
            .PUT => .PUT,
            .DELETE => .DELETE,
            .PATCH => .PATCH,
            .OPTIONS => .OPTIONS,
            .HEAD => .HEAD,
            else => .GET,
        };
    }
};

/// Route handler with closure
const RouteHandler = struct {
    method: Method,
    pattern: []const u8,
    handler_closure: Value, // Cot closure value
};

/// Cot HTTP Server - manages routes and closure handlers
const CotServer = struct {
    allocator: Allocator,
    host: []const u8,
    routes: std.ArrayListUnmanaged(RouteHandler),
    global_middleware: std.ArrayListUnmanaged(Value), // Closure middleware
    after_middleware: std.ArrayListUnmanaged(Value),

    pub fn init(allocator: Allocator) CotServer {
        return .{
            .allocator = allocator,
            .host = "127.0.0.1",
            .routes = .empty,
            .global_middleware = .empty,
            .after_middleware = .empty,
        };
    }

    pub fn deinit(self: *CotServer) void {
        for (self.routes.items) |route| {
            self.allocator.free(route.pattern);
        }
        self.routes.deinit(self.allocator);
        self.global_middleware.deinit(self.allocator);
        self.after_middleware.deinit(self.allocator);
    }

    pub fn addRoute(self: *CotServer, method: Method, pattern: []const u8, handler: Value) !void {
        const owned_pattern = try self.allocator.dupe(u8, pattern);
        errdefer self.allocator.free(owned_pattern);
        try self.routes.append(self.allocator, .{
            .method = method,
            .pattern = owned_pattern,
            .handler_closure = handler,
        });
    }

    pub fn addMiddleware(self: *CotServer, closure: Value) !void {
        try self.global_middleware.append(self.allocator, closure);
    }

    pub fn addAfterMiddleware(self: *CotServer, closure: Value) !void {
        try self.after_middleware.append(self.allocator, closure);
    }

    /// Match a route against request method and path
    pub fn matchRoute(self: *CotServer, method: Method, path: []const u8) ?*const RouteHandler {
        // Normalize path by stripping query string
        const clean_path = if (std.mem.indexOf(u8, path, "?")) |idx| path[0..idx] else path;

        for (self.routes.items) |*route| {
            if (route.method == method and matchPattern(route.pattern, clean_path)) {
                return route;
            }
        }
        return null;
    }
};

/// Simple pattern matching (supports exact match and trailing slash normalization)
fn matchPattern(pattern: []const u8, path: []const u8) bool {
    // Exact match
    if (std.mem.eql(u8, pattern, path)) return true;

    // Handle trailing slash: "/foo" matches "/foo/"
    if (path.len > 0 and path[path.len - 1] == '/' and path.len - 1 == pattern.len) {
        return std.mem.eql(u8, pattern, path[0 .. path.len - 1]);
    }

    return false;
}

/// HTTP Request Context - passed to closure handlers
const CotRequestContext = struct {
    allocator: Allocator,
    request: *std.http.Server.Request,
    target: []const u8, // Cached target for path/query access
    response_status: std.http.Status,
    response_headers: std.ArrayListUnmanaged(Header),
    response_body: std.ArrayListUnmanaged(u8),
    params: std.StringHashMap([]const u8),
    responded: bool,
    body_read: bool,
    cached_body: []const u8,

    const Header = struct {
        name: []const u8,
        value: []const u8,
    };

    pub fn init(allocator: Allocator, request: *std.http.Server.Request) CotRequestContext {
        return .{
            .allocator = allocator,
            .request = request,
            .target = request.head.target,
            .response_status = .ok,
            .response_headers = .empty,
            .response_body = .empty,
            .params = std.StringHashMap([]const u8).init(allocator),
            .responded = false,
            .body_read = false,
            .cached_body = "",
        };
    }

    pub fn deinit(self: *CotRequestContext) void {
        self.response_headers.deinit(self.allocator);
        self.response_body.deinit(self.allocator);
        self.params.deinit();
        if (self.body_read and self.cached_body.len > 0) {
            self.allocator.free(self.cached_body);
        }
    }

    pub fn method(self: *CotRequestContext) []const u8 {
        return @tagName(self.request.head.method);
    }

    pub fn path(self: *CotRequestContext) []const u8 {
        if (std.mem.indexOf(u8, self.target, "?")) |idx| {
            return self.target[0..idx];
        }
        return self.target;
    }

    pub fn getQuery(self: *CotRequestContext, name: []const u8) ?[]const u8 {
        const query_start = std.mem.indexOf(u8, self.target, "?") orelse return null;
        const query = self.target[query_start + 1 ..];

        var it = std.mem.splitScalar(u8, query, '&');
        while (it.next()) |pair| {
            const eq_pos = std.mem.indexOf(u8, pair, "=") orelse continue;
            const key = pair[0..eq_pos];
            const value = pair[eq_pos + 1 ..];
            if (std.mem.eql(u8, key, name)) {
                return value;
            }
        }
        return null;
    }

    pub fn getHeader(self: *CotRequestContext, name: []const u8) ?[]const u8 {
        var it = self.request.iterateHeaders();
        while (it.next()) |header| {
            if (std.ascii.eqlIgnoreCase(header.name, name)) {
                return header.value;
            }
        }
        return null;
    }

    pub fn getBody(self: *CotRequestContext, body_buffer: []u8) ![]const u8 {
        if (self.body_read) return self.cached_body;

        // Get body reader - must handle expect header first
        if (self.request.head.expect) |_| {
            self.request.writeExpectContinue() catch return "";
        }

        const content_length = self.request.head.content_length orelse 0;
        if (content_length == 0) {
            self.body_read = true;
            return "";
        }

        // Read body using the Reader's API
        const body_reader = self.request.readerExpectNone(body_buffer);
        var body_list: std.ArrayListUnmanaged(u8) = .empty;
        errdefer body_list.deinit(self.allocator);

        var remaining = content_length;
        var read_buf: [4096]u8 = undefined;
        while (remaining > 0) {
            const to_read = @min(remaining, read_buf.len);
            const n = body_reader.readSliceShort(read_buf[0..to_read]) catch break;
            if (n == 0) break;
            body_list.appendSlice(self.allocator, read_buf[0..n]) catch break;
            remaining -= n;
        }

        self.cached_body = body_list.toOwnedSlice(self.allocator) catch "";
        self.body_read = true;
        return self.cached_body;
    }

    pub fn setStatus(self: *CotRequestContext, code: u16) void {
        self.response_status = @enumFromInt(code);
    }

    pub fn setHeader(self: *CotRequestContext, name: []const u8, value: []const u8) !void {
        try self.response_headers.append(self.allocator, .{ .name = name, .value = value });
    }

    pub fn writeBody(self: *CotRequestContext, data: []const u8) !void {
        try self.response_body.appendSlice(self.allocator, data);
    }
};

// Global server registry (handle -> CotServer)
var servers: std.AutoHashMap(u32, *CotServer) = undefined;
var contexts: std.AutoHashMap(u32, *CotRequestContext) = undefined;
var next_server_handle: u32 = 1;
var next_context_handle: u32 = 1;
var registry_initialized: bool = false;
var registry_allocator: Allocator = undefined;

fn ensureRegistryInit(allocator: Allocator) void {
    if (!registry_initialized) {
        registry_allocator = allocator;
        servers = std.AutoHashMap(u32, *CotServer).init(allocator);
        contexts = std.AutoHashMap(u32, *CotRequestContext).init(allocator);
        registry_initialized = true;
    }
}

/// Register all HTTP server functions
pub fn register(registry: anytype) !void {
    // Server lifecycle
    try registry.registerNative("std.http.server.new", server_new);
    try registry.registerNative("std.http.server.listen", server_listen);
    try registry.registerNative("std.http.server.close", server_close);

    // Route registration
    try registry.registerNative("std.http.server.get", server_get);
    try registry.registerNative("std.http.server.post", server_post);
    try registry.registerNative("std.http.server.put", server_put);
    try registry.registerNative("std.http.server.delete", server_delete);
    try registry.registerNative("std.http.server.patch", server_patch);
    try registry.registerNative("std.http.server.options", server_options);

    // Middleware
    try registry.registerNative("std.http.server.use", server_use);
    try registry.registerNative("std.http.server.use_after", server_use_after);

    // Context request methods
    try registry.registerNative("std.http.ctx.method", ctx_method);
    try registry.registerNative("std.http.ctx.path", ctx_path);
    try registry.registerNative("std.http.ctx.param", ctx_param);
    try registry.registerNative("std.http.ctx.query", ctx_query);
    try registry.registerNative("std.http.ctx.header", ctx_header);
    try registry.registerNative("std.http.ctx.body", ctx_body);

    // Context response methods
    try registry.registerNative("std.http.ctx.status", ctx_status);
    try registry.registerNative("std.http.ctx.set_header", ctx_set_header);
    try registry.registerNative("std.http.ctx.text", ctx_text);
    try registry.registerNative("std.http.ctx.json", ctx_json);
    try registry.registerNative("std.http.ctx.html", ctx_html);
    try registry.registerNative("std.http.ctx.redirect", ctx_redirect);

    // Short names for convenience
    try registry.registerNative("http_server_new", server_new);
    try registry.registerNative("http_server_listen", server_listen);
    try registry.registerNative("http_server_get", server_get);
    try registry.registerNative("http_server_post", server_post);
}

// =============================================================================
// Server Lifecycle
// =============================================================================

/// http.server.new() -> handle
fn server_new(ctx: *NativeContext) NativeError!?Value {
    ensureRegistryInit(ctx.allocator);

    const server = ctx.allocator.create(CotServer) catch return NativeError.OutOfMemory;
    server.* = CotServer.init(ctx.allocator);

    const handle = next_server_handle;
    next_server_handle += 1;

    servers.put(handle, server) catch {
        ctx.allocator.destroy(server);
        return NativeError.OutOfMemory;
    };

    debug.print(.general, "http_server: created server handle {d}", .{handle});
    return Value.initInt(@intCast(handle));
}

/// http.server.listen(handle, port) -> null
/// Starts the server listening. This is blocking.
fn server_listen(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const port_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;

    const server_handle: u32 = @intCast(handle_val.toInt());
    const port: u16 = @intCast(port_val.toInt());

    const cot_server = servers.get(server_handle) orelse return NativeError.InvalidArgument;

    debug.print(.general, "http_server: starting on port {d} with {d} routes", .{ port, cot_server.routes.items.len });

    // Create address
    const address = std.net.Address.parseIp4(cot_server.host, port) catch {
        return NativeError.InvalidArgument;
    };

    // Create and start server
    var server = address.listen(.{
        .reuse_address = true,
    }) catch |err| {
        debug.print(.general, "http_server: listen error: {}", .{err});
        return NativeError.FileError;
    };
    defer server.deinit();

    debug.print(.general, "http_server: listening on {s}:{d}", .{ cot_server.host, port });

    // Main request loop (single-threaded for simplicity in Phase 6)
    while (true) {
        var connection = server.accept() catch |err| {
            debug.print(.general, "http_server: accept error: {}", .{err});
            continue;
        };

        handleConnection(ctx, cot_server, &connection) catch |err| {
            debug.print(.general, "http_server: connection error: {}", .{err});
        };
    }
}

fn handleConnection(
    native_ctx: *NativeContext,
    cot_server: *CotServer,
    connection: *std.net.Server.Connection,
) !void {
    defer connection.stream.close();

    // Create reader and writer from the connection stream
    var read_buffer: [8192]u8 = undefined;
    var write_buffer: [8192]u8 = undefined;

    var reader_obj = connection.stream.reader(&read_buffer);
    var writer_obj = connection.stream.writer(&write_buffer);

    // Initialize HTTP server for this connection
    var http_server = std.http.Server.init(reader_obj.interface(), &writer_obj.interface);

    // Handle requests on this connection (supports keep-alive)
    while (true) {
        var request = http_server.receiveHead() catch |err| {
            switch (err) {
                error.HttpConnectionClosing => return,
                error.HttpRequestTruncated => return,
                else => return,
            }
        };

        // Create request context
        var req_ctx = CotRequestContext.init(native_ctx.allocator, &request);
        defer req_ctx.deinit();

        // Store context for native function access
        const ctx_handle = next_context_handle;
        next_context_handle += 1;
        contexts.put(ctx_handle, &req_ctx) catch return;
        defer _ = contexts.remove(ctx_handle);

        // Match route
        const method = Method.fromStd(request.head.method);
        const path = req_ctx.path();

        if (cot_server.matchRoute(method, path)) |route| {
            // Invoke handler closure
            if (native_ctx.canCallClosures()) {
                const ctx_val = Value.initInt(@intCast(ctx_handle));
                _ = native_ctx.callClosure(route.handler_closure, &.{ctx_val}) catch |err| {
                    debug.print(.general, "http_server: closure error: {}", .{err});
                    // Send 500 error
                    req_ctx.setStatus(500);
                    req_ctx.writeBody("Internal Server Error") catch {};
                };
            } else {
                debug.print(.general, "http_server: closure invocation not available", .{});
                req_ctx.setStatus(500);
                req_ctx.writeBody("Server Error: Closure invocation not available") catch {};
            }
        } else {
            // 404 Not Found
            req_ctx.setStatus(404);
            req_ctx.writeBody("Not Found") catch {};
        }

        // Send response
        var extra_headers: [16]std.http.Header = undefined;
        var header_count: usize = 0;

        for (req_ctx.response_headers.items) |h| {
            if (header_count < extra_headers.len) {
                extra_headers[header_count] = .{
                    .name = h.name,
                    .value = h.value,
                };
                header_count += 1;
            }
        }

        request.respond(
            req_ctx.response_body.items,
            .{
                .status = req_ctx.response_status,
                .extra_headers = extra_headers[0..header_count],
            },
        ) catch |err| {
            debug.print(.general, "http_server: respond error: {}", .{err});
            return;
        };

        // If not keep-alive, close connection
        if (!request.head.keep_alive) return;
    }
}

/// http.server.close(handle) -> null
fn server_close(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const server_handle: u32 = @intCast(handle_val.toInt());

    if (servers.fetchRemove(server_handle)) |entry| {
        entry.value.deinit();
        ctx.allocator.destroy(entry.value);
    }

    return Value.initNull();
}

// =============================================================================
// Route Registration
// =============================================================================

fn registerRoute(ctx: *NativeContext, method: Method) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const pattern_val = ctx.getArg(1) orelse return NativeError.InvalidArgument;
    const handler_closure = ctx.getArgClosure(2) orelse return NativeError.InvalidArgument;

    const server_handle: u32 = @intCast(handle_val.toInt());
    const pattern = pattern_val.toString();

    const server = servers.get(server_handle) orelse return NativeError.InvalidArgument;

    server.addRoute(method, pattern, handler_closure) catch return NativeError.OutOfMemory;

    debug.print(.general, "http_server: registered {s} {s}", .{ @tagName(method), pattern });

    return handle_val; // Return for chaining
}

fn server_get(ctx: *NativeContext) NativeError!?Value {
    return registerRoute(ctx, .GET);
}

fn server_post(ctx: *NativeContext) NativeError!?Value {
    return registerRoute(ctx, .POST);
}

fn server_put(ctx: *NativeContext) NativeError!?Value {
    return registerRoute(ctx, .PUT);
}

fn server_delete(ctx: *NativeContext) NativeError!?Value {
    return registerRoute(ctx, .DELETE);
}

fn server_patch(ctx: *NativeContext) NativeError!?Value {
    return registerRoute(ctx, .PATCH);
}

fn server_options(ctx: *NativeContext) NativeError!?Value {
    return registerRoute(ctx, .OPTIONS);
}

// =============================================================================
// Middleware
// =============================================================================

fn server_use(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const middleware_closure = ctx.getArgClosure(1) orelse return NativeError.InvalidArgument;

    const server_handle: u32 = @intCast(handle_val.toInt());
    const server = servers.get(server_handle) orelse return NativeError.InvalidArgument;

    server.addMiddleware(middleware_closure) catch return NativeError.OutOfMemory;

    return handle_val;
}

fn server_use_after(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const middleware_closure = ctx.getArgClosure(1) orelse return NativeError.InvalidArgument;

    const server_handle: u32 = @intCast(handle_val.toInt());
    const server = servers.get(server_handle) orelse return NativeError.InvalidArgument;

    server.addAfterMiddleware(middleware_closure) catch return NativeError.OutOfMemory;

    return handle_val;
}

// =============================================================================
// Context Methods
// =============================================================================

fn getContextFromHandle(ctx: *NativeContext) ?*CotRequestContext {
    const handle_val = ctx.getArg(0) orelse return null;
    const ctx_handle: u32 = @intCast(handle_val.toInt());
    return contexts.get(ctx_handle);
}

fn ctx_method(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    return Value.initString(ctx.allocator, cot_ctx.method()) catch return NativeError.OutOfMemory;
}

fn ctx_path(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    return Value.initString(ctx.allocator, cot_ctx.path()) catch return NativeError.OutOfMemory;
}

fn ctx_param(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const name = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    if (cot_ctx.params.get(name)) |value| {
        return Value.initString(ctx.allocator, value) catch return NativeError.OutOfMemory;
    }
    return Value.initNull();
}

fn ctx_query(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const name = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    if (cot_ctx.getQuery(name)) |value| {
        return Value.initString(ctx.allocator, value) catch return NativeError.OutOfMemory;
    }
    return Value.initNull();
}

fn ctx_header(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const name = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    if (cot_ctx.getHeader(name)) |value| {
        return Value.initString(ctx.allocator, value) catch return NativeError.OutOfMemory;
    }
    return Value.initNull();
}

fn ctx_body(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    var body_buffer: [8192]u8 = undefined;
    const body = cot_ctx.getBody(&body_buffer) catch "";
    return Value.initString(ctx.allocator, body) catch return NativeError.OutOfMemory;
}

fn ctx_status(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const code: u16 = @intCast(ctx.getArgInt(1) catch return NativeError.InvalidArgument);
    cot_ctx.setStatus(code);
    return handle_val;
}

fn ctx_set_header(ctx: *NativeContext) NativeError!?Value {
    const handle_val = ctx.getArg(0) orelse return NativeError.InvalidArgument;
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const name = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const value = ctx.getArgString(2) catch return NativeError.InvalidArgument;
    cot_ctx.setHeader(name, value) catch return NativeError.OutOfMemory;
    return handle_val;
}

fn ctx_text(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    cot_ctx.setHeader("Content-Type", "text/plain; charset=utf-8") catch return NativeError.OutOfMemory;
    cot_ctx.writeBody(data) catch return NativeError.OutOfMemory;
    return Value.initNull();
}

fn ctx_json(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    cot_ctx.setHeader("Content-Type", "application/json") catch return NativeError.OutOfMemory;
    cot_ctx.writeBody(data) catch return NativeError.OutOfMemory;
    return Value.initNull();
}

fn ctx_html(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    cot_ctx.setHeader("Content-Type", "text/html; charset=utf-8") catch return NativeError.OutOfMemory;
    cot_ctx.writeBody(data) catch return NativeError.OutOfMemory;
    return Value.initNull();
}

fn ctx_redirect(ctx: *NativeContext) NativeError!?Value {
    const cot_ctx = getContextFromHandle(ctx) orelse return NativeError.InvalidArgument;
    const url = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    cot_ctx.setStatus(302);
    cot_ctx.setHeader("Location", url) catch return NativeError.OutOfMemory;
    return Value.initNull();
}
