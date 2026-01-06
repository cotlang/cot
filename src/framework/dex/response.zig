//! Dex Response Helpers
//!
//! Helpers for generating HTTP responses from Dex pages and components.
//! These are used in getServerProps and event handlers.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Response type for page rendering
pub const Response = struct {
    /// Response body content
    body: ?[]const u8 = null,
    /// HTTP status code
    status_code: u16 = 200,
    /// Content type
    content_type: ContentType = .html,
    /// Headers to set
    headers: std.StringHashMap([]const u8),
    /// Redirect URL (if redirecting)
    redirect_url: ?[]const u8 = null,
    /// Not found flag
    not_found: bool = false,
    /// Allocator for dynamic content
    allocator: Allocator,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .headers = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        self.headers.deinit();
        if (self.body) |b| {
            self.allocator.free(b);
        }
    }

    /// Set a response header
    pub fn setHeader(self: *Self, name: []const u8, value: []const u8) !void {
        try self.headers.put(name, value);
    }

    /// Get a response header
    pub fn getHeader(self: *const Self, name: []const u8) ?[]const u8 {
        return self.headers.get(name);
    }

    /// Check if this is a redirect response
    pub fn isRedirect(self: *const Self) bool {
        return self.redirect_url != null;
    }

    /// Check if this is a not found response
    pub fn isNotFound(self: *const Self) bool {
        return self.not_found;
    }
};

/// Content types for responses
pub const ContentType = enum {
    html,
    json,
    text,
    xml,

    pub fn toString(self: ContentType) []const u8 {
        return switch (self) {
            .html => "text/html; charset=utf-8",
            .json => "application/json; charset=utf-8",
            .text => "text/plain; charset=utf-8",
            .xml => "application/xml; charset=utf-8",
        };
    }
};

/// Create a redirect response
pub fn redirect(allocator: Allocator, url: []const u8, status: u16) Response {
    var response = Response.init(allocator);
    response.redirect_url = url;
    response.status_code = if (status == 0) 302 else status;
    response.setHeader("Location", url) catch {};
    return response;
}

/// Create a redirect response with default 302 status
pub fn redirectTo(allocator: Allocator, url: []const u8) Response {
    return redirect(allocator, url, 302);
}

/// Create a permanent redirect (301)
pub fn permanentRedirect(allocator: Allocator, url: []const u8) Response {
    return redirect(allocator, url, 301);
}

/// Create a not found response
pub fn notFound(allocator: Allocator) Response {
    var response = Response.init(allocator);
    response.not_found = true;
    response.status_code = 404;
    return response;
}

/// Create a JSON response
pub fn json(allocator: Allocator, data: []const u8) !Response {
    var response = Response.init(allocator);
    response.body = try allocator.dupe(u8, data);
    response.content_type = .json;
    return response;
}

/// Create a JSON response from a struct (serializes to JSON)
pub fn jsonValue(allocator: Allocator, value: anytype) !Response {
    var response = Response.init(allocator);

    // Serialize to JSON
    var json_buf: std.ArrayListUnmanaged(u8) = .empty;
    errdefer json_buf.deinit(allocator);

    try std.json.stringify(value, .{}, json_buf.writer(allocator));
    response.body = try json_buf.toOwnedSlice(allocator);
    response.content_type = .json;
    return response;
}

/// Create a plain text response
pub fn text(allocator: Allocator, content: []const u8) !Response {
    var response = Response.init(allocator);
    response.body = try allocator.dupe(u8, content);
    response.content_type = .text;
    return response;
}

/// Create an HTML response
pub fn html(allocator: Allocator, content: []const u8) !Response {
    var response = Response.init(allocator);
    response.body = try allocator.dupe(u8, content);
    response.content_type = .html;
    return response;
}

/// Create an error response
pub fn error_(allocator: Allocator, status: u16, message: []const u8) !Response {
    var response = Response.init(allocator);
    response.status_code = status;
    response.body = try allocator.dupe(u8, message);
    return response;
}

/// Create a forbidden response
pub fn forbidden(allocator: Allocator) Response {
    var response = Response.init(allocator);
    response.status_code = 403;
    return response;
}

/// Create an unauthorized response
pub fn unauthorized(allocator: Allocator) Response {
    var response = Response.init(allocator);
    response.status_code = 401;
    return response;
}

/// Create a bad request response
pub fn badRequest(allocator: Allocator, message: ?[]const u8) !Response {
    var response = Response.init(allocator);
    response.status_code = 400;
    if (message) |m| {
        response.body = try allocator.dupe(u8, m);
    }
    return response;
}

// ============================================================================
// Tests
// ============================================================================

test "redirect response" {
    const allocator = std.testing.allocator;

    var response = redirect(allocator, "/login", 302);
    defer response.deinit();

    try std.testing.expect(response.isRedirect());
    try std.testing.expectEqual(@as(u16, 302), response.status_code);
    try std.testing.expectEqualStrings("/login", response.redirect_url.?);
}

test "not found response" {
    const allocator = std.testing.allocator;

    var response = notFound(allocator);
    defer response.deinit();

    try std.testing.expect(response.isNotFound());
    try std.testing.expectEqual(@as(u16, 404), response.status_code);
}

test "json response" {
    const allocator = std.testing.allocator;

    var response = try json(allocator, "{\"ok\": true}");
    defer response.deinit();

    try std.testing.expectEqual(ContentType.json, response.content_type);
    try std.testing.expectEqualStrings("{\"ok\": true}", response.body.?);
}

test "text response" {
    const allocator = std.testing.allocator;

    var response = try text(allocator, "Hello, World!");
    defer response.deinit();

    try std.testing.expectEqual(ContentType.text, response.content_type);
    try std.testing.expectEqualStrings("Hello, World!", response.body.?);
}

test "html response" {
    const allocator = std.testing.allocator;

    var response = try html(allocator, "<h1>Hello</h1>");
    defer response.deinit();

    try std.testing.expectEqual(ContentType.html, response.content_type);
    try std.testing.expectEqualStrings("<h1>Hello</h1>", response.body.?);
}

test "set headers" {
    const allocator = std.testing.allocator;

    var response = Response.init(allocator);
    defer response.deinit();

    try response.setHeader("X-Custom", "value");
    try std.testing.expectEqualStrings("value", response.getHeader("X-Custom").?);
}
