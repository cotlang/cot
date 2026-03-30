//! LSP server entry point — stdio message loop.

const std = @import("std");
const transport = @import("transport.zig");
const Server = @import("server.zig").Server;

pub fn run(parent_allocator: std.mem.Allocator) void {
    var server = Server.init(parent_allocator);
    defer server.deinit();

    const stdin_file = std.fs.File{ .handle = std.posix.STDIN_FILENO };
    const stdout_file = std.fs.File{ .handle = std.posix.STDOUT_FILENO };

    while (server.status != .exit) {
        // Read one message
        const body = transport.readMessage(parent_allocator, stdin_file) catch |err| {
            switch (err) {
                transport.TransportError.EndOfStream => break,
                else => continue,
            }
        };
        defer parent_allocator.free(body);

        // Process with a per-message arena
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        defer arena.deinit();

        const response = server.handleMessage(arena.allocator(), body) catch continue;

        if (response) |resp| {
            transport.writeMessage(stdout_file, resp) catch break;
        }
    }
}

// Make submodules available for testing
test {
    _ = @import("transport.zig");
    _ = @import("types.zig");
    _ = @import("document_store.zig");
}
