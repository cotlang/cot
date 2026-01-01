//! I/O abstraction layer (native only, WASM support removed)

const std = @import("std");

// Native I/O implementation
pub const impl = @import("io_native.zig");

/// Print text to output (stdout on native, JS console on WASM)
pub fn print(text: []const u8) void {
    impl.print(text);
}

/// Print formatted text
pub fn printf(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const slice = std.fmt.bufPrint(&buf, fmt, args) catch return;
    print(slice);
}

/// HTTP request result
pub const HttpResult = struct {
    status: i32,
    body: []u8,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *HttpResult) void {
        self.allocator.free(self.body);
    }
};

/// HTTP error type
pub const HttpError = error{
    ConnectionFailed,
    RequestFailed,
    ResponseTooLarge,
    OutOfMemory,
};

/// Perform an HTTP POST request (for Turso/Neon API calls)
pub fn httpPost(allocator: std.mem.Allocator, url: []const u8, body: []const u8, headers: ?[]const [2][]const u8) HttpError!HttpResult {
    return impl.httpPost(allocator, url, body, headers);
}

/// Check if we're running on WASM (always false - WASM support removed)
pub fn isWasm() bool {
    return false;
}
