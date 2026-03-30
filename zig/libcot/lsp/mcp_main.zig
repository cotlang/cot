//! MCP server entry point — newline-delimited JSON-RPC over stdio.
//! Unlike the LSP transport (Content-Length framed), MCP uses simple newline-delimited messages.

const std = @import("std");
const McpServer = @import("mcp_server.zig").McpServer;

pub fn run(parent_allocator: std.mem.Allocator) void {
    var server = McpServer.init(parent_allocator);
    defer server.deinit();

    const stdin = std.fs.File{ .handle = std.posix.STDIN_FILENO };
    const stdout = std.fs.File{ .handle = std.posix.STDOUT_FILENO };

    var buf: [1024 * 64]u8 = undefined;

    while (true) {
        // Read one line (newline-delimited JSON-RPC) byte by byte
        const line = readLine(stdin, &buf) orelse break;

        // Strip trailing \r if present
        const trimmed = if (line.len > 0 and line[line.len - 1] == '\r')
            line[0 .. line.len - 1]
        else
            line;

        if (trimmed.len == 0) continue;

        // Process with a per-message arena
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        defer arena.deinit();

        const response = server.handleMessage(arena.allocator(), trimmed) catch continue;

        if (response) |resp| {
            // Write response + newline
            stdout.writeAll(resp) catch break;
            stdout.writeAll("\n") catch break;
        }

        if (server.should_exit) break;
    }
}

/// Read a single line from file, returning the slice without the trailing newline.
/// Returns null on EOF before any data.
fn readLine(file: std.fs.File, buf: []u8) ?[]const u8 {
    var len: usize = 0;
    while (len < buf.len) {
        var byte: [1]u8 = undefined;
        const n = file.read(&byte) catch return null;
        if (n == 0) {
            if (len == 0) return null;
            return buf[0..len];
        }
        if (byte[0] == '\n') {
            return buf[0..len];
        }
        buf[len] = byte[0];
        len += 1;
    }
    // Line too long — return what we have
    return buf[0..len];
}

// Make submodules available for testing
test {
    _ = @import("mcp_server.zig");
}
