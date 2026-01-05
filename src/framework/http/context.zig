//! HTTP Context
//!
//! Unified request/response object providing an ergonomic API for HTTP handlers.
//! Inspired by Express.js, Gin, and Axum patterns.

const std = @import("std");
const http = std.http;
const Allocator = std.mem.Allocator;

/// HTTP Context - unified request/response object
pub const Context = struct {
    allocator: Allocator,

    // Request data
    _method: Method,
    _path: []const u8,
    _query: ?[]const u8,
    _body: []const u8,
    _params: std.StringHashMap([]const u8),
    _http_request: ?*http.Server.Request,

    // Response state
    _status_code: u16,
    _response_headers: std.StringHashMap([]const u8),
    _response_body: std.ArrayListUnmanaged(u8),
    _sent: bool,

    // Middleware state storage
    _state: std.StringHashMap([]const u8),

    pub const Method = enum {
        GET,
        POST,
        PUT,
        DELETE,
        PATCH,
        HEAD,
        OPTIONS,

        pub fn toString(self: Method) []const u8 {
            return switch (self) {
                .GET => "GET",
                .POST => "POST",
                .PUT => "PUT",
                .DELETE => "DELETE",
                .PATCH => "PATCH",
                .HEAD => "HEAD",
                .OPTIONS => "OPTIONS",
            };
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

    /// Initialize a new context
    pub fn init(allocator: Allocator) Context {
        return .{
            .allocator = allocator,
            ._method = .GET,
            ._path = "",
            ._query = null,
            ._body = "",
            ._params = std.StringHashMap([]const u8).init(allocator),
            ._http_request = null,
            ._status_code = 200,
            ._response_headers = std.StringHashMap([]const u8).init(allocator),
            ._response_body = .empty,
            ._sent = false,
            ._state = std.StringHashMap([]const u8).init(allocator),
        };
    }

    /// Clean up context resources
    pub fn deinit(self: *Context) void {
        self._params.deinit();
        self._response_headers.deinit();
        self._response_body.deinit(self.allocator);
        self._state.deinit();
    }

    // ============================================
    // Request Methods
    // ============================================

    /// Get the HTTP method
    pub fn method(self: *const Context) Method {
        return self._method;
    }

    /// Get the HTTP method as string
    pub fn methodString(self: *const Context) []const u8 {
        return self._method.toString();
    }

    /// Get the request path (without query string)
    pub fn path(self: *const Context) []const u8 {
        return self._path;
    }

    /// Get a route parameter (e.g., :id from /users/:id)
    pub fn param(self: *const Context, name: []const u8) ?[]const u8 {
        return self._params.get(name);
    }

    /// Get a query string parameter
    pub fn query(self: *const Context, name: []const u8) ?[]const u8 {
        const query_str = self._query orelse return null;

        // Parse query string to find the parameter
        var iter = std.mem.splitScalar(u8, query_str, '&');
        while (iter.next()) |pair| {
            if (std.mem.indexOf(u8, pair, "=")) |eq_idx| {
                const key = pair[0..eq_idx];
                const value = pair[eq_idx + 1 ..];
                if (std.mem.eql(u8, key, name)) {
                    return value;
                }
            } else {
                // Key without value
                if (std.mem.eql(u8, pair, name)) {
                    return "";
                }
            }
        }
        return null;
    }

    /// Get raw query string
    pub fn queryString(self: *const Context) ?[]const u8 {
        return self._query;
    }

    /// Get a request header (case-insensitive)
    pub fn header(self: *const Context, name: []const u8) ?[]const u8 {
        if (self._http_request) |req| {
            var iter = req.iterateHeaders();
            while (iter.next()) |h| {
                if (std.ascii.eqlIgnoreCase(h.name, name)) {
                    return h.value;
                }
            }
        }
        return null;
    }

    /// Get the request body as string
    pub fn body(self: *const Context) []const u8 {
        return self._body;
    }

    /// Get the Content-Type header
    pub fn contentType(self: *const Context) ?[]const u8 {
        return self.header("Content-Type");
    }

    // ============================================
    // Response Methods (Chainable)
    // ============================================

    /// Set the response status code
    pub fn status(self: *Context, code: u16) *Context {
        self._status_code = code;
        return self;
    }

    /// Set a response header
    pub fn setHeader(self: *Context, name: []const u8, value: []const u8) *Context {
        self._response_headers.put(name, value) catch {};
        return self;
    }

    /// Set Content-Type header
    pub fn setContentType(self: *Context, content_type: []const u8) *Context {
        return self.setHeader("Content-Type", content_type);
    }

    // ============================================
    // Response Senders (Terminal)
    // ============================================

    /// Send a plain text response
    pub fn text(self: *Context, data: []const u8) !void {
        _ = self.setContentType("text/plain; charset=utf-8");
        try self.send(data);
    }

    /// Send a JSON response
    pub fn json(self: *Context, data: []const u8) !void {
        _ = self.setContentType("application/json");
        try self.send(data);
    }

    /// Send an HTML response
    pub fn html(self: *Context, data: []const u8) !void {
        _ = self.setContentType("text/html; charset=utf-8");
        try self.send(data);
    }

    /// Send a redirect response
    pub fn redirect(self: *Context, url: []const u8) !void {
        self._status_code = 302;
        _ = self.setHeader("Location", url);
        try self.send("");
    }

    /// Send a permanent redirect (301)
    pub fn redirectPermanent(self: *Context, url: []const u8) !void {
        self._status_code = 301;
        _ = self.setHeader("Location", url);
        try self.send("");
    }

    /// Send raw bytes as response body
    pub fn send(self: *Context, data: []const u8) !void {
        if (self._sent) return; // Already sent

        try self._response_body.appendSlice(self.allocator, data);
        self._sent = true;
    }

    /// Write data to response body (can be called multiple times before send)
    pub fn write(self: *Context, data: []const u8) !void {
        try self._response_body.appendSlice(self.allocator, data);
    }

    /// Finalize and mark response as sent
    pub fn end(self: *Context) void {
        self._sent = true;
    }

    // ============================================
    // Middleware State Storage
    // ============================================

    /// Store a value in context state (for passing data between middleware)
    pub fn set(self: *Context, key: []const u8, value: []const u8) void {
        self._state.put(key, value) catch {};
    }

    /// Retrieve a value from context state
    pub fn get(self: *const Context, key: []const u8) ?[]const u8 {
        return self._state.get(key);
    }

    // ============================================
    // Internal Helpers
    // ============================================

    /// Check if response has been sent
    pub fn isSent(self: *const Context) bool {
        return self._sent;
    }

    /// Get the response status code
    pub fn getStatusCode(self: *const Context) u16 {
        return self._status_code;
    }

    /// Get response body content
    pub fn getResponseBody(self: *const Context) []const u8 {
        return self._response_body.items;
    }

    /// Get response headers iterator
    pub fn getResponseHeaders(self: *Context) std.StringHashMap([]const u8).Iterator {
        return self._response_headers.iterator();
    }

    /// Set route parameters (called by router)
    pub fn setParam(self: *Context, name: []const u8, value: []const u8) !void {
        try self._params.put(name, value);
    }
};

/// Handler function type using Context
pub const HandlerFn = *const fn (*Context) anyerror!void;

/// Convert http.Status code to u16
pub fn httpStatusToCode(status: http.Status) u16 {
    return @intFromEnum(status);
}

/// Convert u16 to http.Status
pub fn codeToHttpStatus(code: u16) http.Status {
    return @enumFromInt(code);
}
