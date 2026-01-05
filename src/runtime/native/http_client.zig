//! HTTP Client Native Functions
//!
//! Namespace: std.http.client
//! Functions: get, post, request
//!
//! Example usage:
//!   import std.http.client
//!   var response = std.http.client.get("https://api.github.com/repos/cot-lang/cot")
//!   println(response)

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

/// Register all HTTP client functions
pub fn register(registry: anytype) !void {
    // Namespaced names (std.http.client.*)
    try registry.registerNative("std.http.client.get", http_get);
    try registry.registerNative("std.http.client.post", http_post);
    try registry.registerNative("std.http.client.request", http_request);

    // Short names for convenience
    try registry.registerNative("http_get", http_get);
    try registry.registerNative("http_post", http_post);
    try registry.registerNative("http_request", http_request);
}

/// HTTP GET request
/// http_get(url: string) -> string (response body) or null on error
pub fn http_get(ctx: *NativeContext) NativeError!?Value {
    const url = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    debug.print(.general, "http_get: fetching '{s}'", .{url});

    const result = performRequest(ctx.allocator, .GET, url, null) catch |err| {
        debug.print(.general, "http_get: error: {}", .{err});
        return Value.initNull();
    };

    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// HTTP POST request
/// http_post(url: string, body: string) -> string (response body) or null on error
pub fn http_post(ctx: *NativeContext) NativeError!?Value {
    const url = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const body = ctx.getArgString(1) catch "";

    debug.print(.general, "http_post: posting to '{s}'", .{url});

    const result = performRequest(ctx.allocator, .POST, url, body) catch |err| {
        debug.print(.general, "http_post: error: {}", .{err});
        return Value.initNull();
    };

    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// HTTP request with method
/// http_request(method: string, url: string, body: ?string) -> string (response body) or null on error
pub fn http_request(ctx: *NativeContext) NativeError!?Value {
    const method_str = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const url = ctx.getArgString(1) catch return NativeError.InvalidArgument;
    const body = ctx.getArgString(2) catch null;

    debug.print(.general, "http_request: {s} '{s}'", .{ method_str, url });

    const method = parseMethod(method_str) orelse {
        debug.print(.general, "http_request: invalid method '{s}'", .{method_str});
        return Value.initNull();
    };

    const result = performRequest(ctx.allocator, method, url, body) catch |err| {
        debug.print(.general, "http_request: error: {}", .{err});
        return Value.initNull();
    };

    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// Parse HTTP method string
fn parseMethod(method: []const u8) ?std.http.Method {
    if (std.ascii.eqlIgnoreCase(method, "GET")) return .GET;
    if (std.ascii.eqlIgnoreCase(method, "POST")) return .POST;
    if (std.ascii.eqlIgnoreCase(method, "PUT")) return .PUT;
    if (std.ascii.eqlIgnoreCase(method, "DELETE")) return .DELETE;
    if (std.ascii.eqlIgnoreCase(method, "PATCH")) return .PATCH;
    if (std.ascii.eqlIgnoreCase(method, "HEAD")) return .HEAD;
    if (std.ascii.eqlIgnoreCase(method, "OPTIONS")) return .OPTIONS;
    return null;
}

/// Perform an HTTP request and return the response body
fn performRequest(
    allocator: std.mem.Allocator,
    method: std.http.Method,
    url: []const u8,
    body: ?[]const u8,
) ![]u8 {
    // Initialize HTTP client
    var client: std.http.Client = .{ .allocator = allocator };
    defer client.deinit();

    // Create response body writer
    var response_body: std.io.Writer.Allocating = .init(allocator);
    defer response_body.deinit();

    // Perform the request
    const result = client.fetch(.{
        .location = .{ .url = url },
        .method = method,
        .payload = body,
        .response_writer = &response_body.writer,
    }) catch |err| {
        debug.print(.general, "http fetch error: {}", .{err});
        return err;
    };

    debug.print(.general, "http response status: {}", .{@intFromEnum(result.status)});

    // Get the response body - need to dupe since response_body is deferred deinit
    const written = response_body.written();
    return allocator.dupe(u8, written);
}
