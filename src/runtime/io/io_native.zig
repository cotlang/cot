//! Native I/O implementation
//!
//! Uses std.io and std.http for native builds.

const std = @import("std");
const io = @import("io.zig");

/// Print text to stdout
pub fn print(text: []const u8) void {
    var stdout_buf: [4096]u8 = undefined;
    const stdout_file = std.fs.File.stdout();
    var stdout_writer = stdout_file.writer(&stdout_buf);
    const stdout = &stdout_writer.interface;
    stdout.writeAll(text) catch {};
    stdout.flush() catch {};
}

/// Perform HTTP POST request using std.http.Client
pub fn httpPost(allocator: std.mem.Allocator, url: []const u8, body: []const u8, headers: ?[]const [2][]const u8) io.HttpError!io.HttpResult {
    // Parse URL
    const uri = std.Uri.parse(url) catch {
        return io.HttpError.ConnectionFailed;
    };

    // Create HTTP client
    var client: std.http.Client = .{ .allocator = allocator };
    defer client.deinit();

    // Build extra headers
    var extra_headers: [16]std.http.Header = undefined;
    var extra_len: usize = 0;

    if (headers) |hdrs| {
        for (hdrs) |hdr| {
            if (extra_len >= 16) break;
            extra_headers[extra_len] = .{
                .name = hdr[0],
                .value = hdr[1],
            };
            extra_len += 1;
        }
    }

    // Create request
    var req = client.open(.POST, uri, .{
        .extra_headers = extra_headers[0..extra_len],
    }) catch {
        return io.HttpError.ConnectionFailed;
    };
    defer req.deinit();

    // Send body
    req.write(body) catch {
        return io.HttpError.RequestFailed;
    };

    // Wait for response
    req.wait() catch {
        return io.HttpError.RequestFailed;
    };

    // Read response body
    const max_size = 16 * 1024 * 1024; // 16MB max
    const response_body = req.reader().readAllAlloc(allocator, max_size) catch |err| {
        return switch (err) {
            error.OutOfMemory => io.HttpError.OutOfMemory,
            else => io.HttpError.ResponseTooLarge,
        };
    };

    return io.HttpResult{
        .status = @intFromEnum(req.status),
        .body = response_body,
        .allocator = allocator,
    };
}
