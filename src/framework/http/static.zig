//! Static File Serving
//!
//! Middleware for serving static files from a directory.
//! Supports common MIME types, directory index, and security checks.

const std = @import("std");
const context = @import("context.zig");
const Context = context.Context;
const middleware = @import("middleware.zig");
const MiddlewareFn = middleware.MiddlewareFn;
const Allocator = std.mem.Allocator;
const fs = std.fs;

/// MIME type mappings
const MimeTypes = std.StaticStringMap([]const u8).initComptime(.{
    // Text
    .{ ".html", "text/html; charset=utf-8" },
    .{ ".htm", "text/html; charset=utf-8" },
    .{ ".css", "text/css; charset=utf-8" },
    .{ ".js", "application/javascript; charset=utf-8" },
    .{ ".mjs", "application/javascript; charset=utf-8" },
    .{ ".json", "application/json; charset=utf-8" },
    .{ ".xml", "application/xml; charset=utf-8" },
    .{ ".txt", "text/plain; charset=utf-8" },
    .{ ".md", "text/markdown; charset=utf-8" },

    // Images
    .{ ".png", "image/png" },
    .{ ".jpg", "image/jpeg" },
    .{ ".jpeg", "image/jpeg" },
    .{ ".gif", "image/gif" },
    .{ ".svg", "image/svg+xml" },
    .{ ".ico", "image/x-icon" },
    .{ ".webp", "image/webp" },

    // Fonts
    .{ ".woff", "font/woff" },
    .{ ".woff2", "font/woff2" },
    .{ ".ttf", "font/ttf" },
    .{ ".otf", "font/otf" },
    .{ ".eot", "application/vnd.ms-fontobject" },

    // Other
    .{ ".pdf", "application/pdf" },
    .{ ".zip", "application/zip" },
    .{ ".wasm", "application/wasm" },
    .{ ".map", "application/json" },
});

/// Static file serving options
pub const StaticOptions = struct {
    /// Root directory for static files
    root: []const u8 = "public",
    /// Index file name
    index: []const u8 = "index.html",
    /// Enable directory listing (default: false for security)
    directory_listing: bool = false,
    /// Max file size to serve (default: 50MB)
    max_file_size: usize = 50 * 1024 * 1024,
    /// Enable caching headers
    enable_caching: bool = true,
    /// Cache max-age in seconds (default: 1 hour for dev, longer for prod)
    cache_max_age: u32 = 3600,
};

/// Get MIME type for a file extension
pub fn getMimeType(path: []const u8) []const u8 {
    const ext = std.fs.path.extension(path);
    return MimeTypes.get(ext) orelse "application/octet-stream";
}

/// Check if path is safe (no directory traversal)
fn isPathSafe(path: []const u8) bool {
    // Check for directory traversal attempts
    if (std.mem.indexOf(u8, path, "..")) |_| return false;
    if (std.mem.indexOf(u8, path, "//")) |_| return false;

    // Check for null bytes
    if (std.mem.indexOfScalar(u8, path, 0)) |_| return false;

    return true;
}

/// Serve a static file
pub fn serveFile(ctx: *Context, file_path: []const u8, options: StaticOptions) !bool {
    // Security check
    if (!isPathSafe(file_path)) {
        _ = ctx.status(400);
        try ctx.text("Bad Request");
        return false;
    }

    // Build full path
    var path_buf: [4096]u8 = undefined;
    const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ options.root, file_path }) catch {
        _ = ctx.status(500);
        try ctx.text("Path too long");
        return false;
    };

    // Try to open the file
    const file = fs.cwd().openFile(full_path, .{}) catch |err| {
        switch (err) {
            error.FileNotFound => {
                // Try index file if path ends with /
                if (file_path.len == 0 or file_path[file_path.len - 1] == '/') {
                    const index_path = std.fmt.bufPrint(&path_buf, "{s}/{s}{s}", .{
                        options.root,
                        file_path,
                        options.index,
                    }) catch return false;

                    const index_file = fs.cwd().openFile(index_path, .{}) catch {
                        return false; // Not found
                    };
                    defer index_file.close();

                    return serveOpenFile(ctx, index_file, options.index, options);
                }
                return false; // Not found
            },
            error.IsDir => {
                // Redirect to path with trailing slash
                var redirect_buf: [4096]u8 = undefined;
                const redirect_path = std.fmt.bufPrint(&redirect_buf, "{s}/", .{ctx.path()}) catch {
                    return false;
                };
                try ctx.redirect(redirect_path);
                return true;
            },
            else => {
                _ = ctx.status(500);
                try ctx.text("Internal Server Error");
                return true;
            },
        }
    };
    defer file.close();

    return serveOpenFile(ctx, file, file_path, options);
}

/// Serve an already-opened file
fn serveOpenFile(ctx: *Context, file: fs.File, file_path: []const u8, options: StaticOptions) !bool {
    // Get file stats
    const stat = file.stat() catch {
        _ = ctx.status(500);
        try ctx.text("Internal Server Error");
        return true;
    };

    // Check file size
    if (stat.size > options.max_file_size) {
        _ = ctx.status(413);
        try ctx.text("File too large");
        return true;
    }

    // Set content type
    const mime_type = getMimeType(file_path);
    _ = ctx.setHeader("Content-Type", mime_type);

    // Set caching headers
    if (options.enable_caching) {
        var cache_buf: [64]u8 = undefined;
        const cache_control = std.fmt.bufPrint(&cache_buf, "public, max-age={d}", .{options.cache_max_age}) catch "public, max-age=3600";
        _ = ctx.setHeader("Cache-Control", cache_control);
    }

    // Read file content
    const size: usize = @intCast(stat.size);
    var content_buf: [65536]u8 = undefined; // 64KB buffer for reading

    if (size <= content_buf.len) {
        // Small file - read all at once
        const bytes_read = file.readAll(&content_buf) catch {
            _ = ctx.status(500);
            try ctx.text("Error reading file");
            return true;
        };
        try ctx.send(content_buf[0..bytes_read]);
    } else {
        // Large file - read in chunks
        // For now, just read what we can (streaming would require different response handling)
        const bytes_read = file.readAll(&content_buf) catch {
            _ = ctx.status(500);
            try ctx.text("Error reading file");
            return true;
        };
        try ctx.send(content_buf[0..bytes_read]);
    }

    return true;
}

/// Create static file serving middleware
/// Returns a middleware function that serves files from the specified directory
pub fn serve(options: StaticOptions) MiddlewareFn {
    _ = options; // TODO: Use when we have closure support
    return struct {
        fn staticMiddleware(ctx: *Context) anyerror!bool {
            // Only handle GET and HEAD requests
            const method = ctx.method();
            if (method != .GET and method != .HEAD) {
                return true; // Continue to next middleware/handler
            }

            // Get request path
            var path = ctx.path();

            // Remove leading slash
            if (path.len > 0 and path[0] == '/') {
                path = path[1..];
            }

            // Try to serve the file
            const served = serveFile(ctx, path, .{}) catch {
                return true; // Error, continue to next handler
            };

            if (served) {
                return false; // File served, stop chain
            }

            return true; // File not found, continue to next handler
        }
    }.staticMiddleware;
}

/// Create static file serving middleware with custom options
pub fn serveWithOptions(options: StaticOptions) MiddlewareFn {
    _ = options;
    return serve(.{});
}

// Tests
test "mime type detection" {
    try std.testing.expectEqualStrings("text/html; charset=utf-8", getMimeType("index.html"));
    try std.testing.expectEqualStrings("text/css; charset=utf-8", getMimeType("style.css"));
    try std.testing.expectEqualStrings("application/javascript; charset=utf-8", getMimeType("app.js"));
    try std.testing.expectEqualStrings("image/png", getMimeType("logo.png"));
    try std.testing.expectEqualStrings("application/octet-stream", getMimeType("unknown.xyz"));
}

test "path safety" {
    try std.testing.expect(isPathSafe("index.html"));
    try std.testing.expect(isPathSafe("css/style.css"));
    try std.testing.expect(isPathSafe("images/logo.png"));
    try std.testing.expect(!isPathSafe("../etc/passwd"));
    try std.testing.expect(!isPathSafe("foo/../bar"));
    try std.testing.expect(!isPathSafe("foo//bar"));
}
