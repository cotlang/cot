//! JSON-RPC wire protocol over stdio.
//! Reads/writes Content-Length framed messages per the LSP specification.
//! Uses raw std.fs.File I/O for Zig 0.15 compatibility.

const std = @import("std");

pub const TransportError = error{
    InvalidHeader,
    MissingContentLength,
    EndOfStream,
    OutOfMemory,
    ReadError,
    WriteError,
};

/// Read one LSP message from a file (typically stdin).
/// Returns the JSON body as a newly-allocated slice.
pub fn readMessage(allocator: std.mem.Allocator, file: std.fs.File) TransportError![]u8 {
    var content_length: ?usize = null;

    // Parse headers line by line (terminated by \r\n\r\n)
    while (true) {
        const line = readLine(file) catch return TransportError.ReadError;
        const trimmed = line orelse return TransportError.EndOfStream;

        // Empty line = end of headers
        if (trimmed.len == 0) break;

        // Parse Content-Length header
        if (std.ascii.startsWithIgnoreCase(trimmed, "content-length:")) {
            const val = std.mem.trimLeft(u8, trimmed["content-length:".len..], " ");
            content_length = std.fmt.parseInt(usize, val, 10) catch return TransportError.InvalidHeader;
        }
        // Ignore other headers
    }

    const len = content_length orelse return TransportError.MissingContentLength;

    const body = allocator.alloc(u8, len) catch return TransportError.OutOfMemory;
    errdefer allocator.free(body);

    // Read exactly len bytes
    var total: usize = 0;
    while (total < len) {
        const n = file.read(body[total..]) catch {
            allocator.free(body);
            return TransportError.EndOfStream;
        };
        if (n == 0) {
            allocator.free(body);
            return TransportError.EndOfStream;
        }
        total += n;
    }

    return body;
}

/// Write one LSP message to a file (typically stdout).
pub fn writeMessage(file: std.fs.File, json_bytes: []const u8) TransportError!void {
    // Format header
    var header_buf: [64]u8 = undefined;
    const header = std.fmt.bufPrint(&header_buf, "Content-Length: {d}\r\n\r\n", .{json_bytes.len}) catch return TransportError.WriteError;

    file.writeAll(header) catch return TransportError.WriteError;
    file.writeAll(json_bytes) catch return TransportError.WriteError;
}

// Internal: read a single line from file, stripping \r\n.
// Returns null on EOF before any data. Returns empty string for blank lines.
// Uses a stack buffer — headers are always short.
var line_buf: [4096]u8 = undefined;
var line_len: usize = 0;

fn readLine(file: std.fs.File) !?[]const u8 {
    line_len = 0;
    while (true) {
        var byte: [1]u8 = undefined;
        const n = try file.read(&byte);
        if (n == 0) {
            if (line_len == 0) return null;
            return line_buf[0..line_len];
        }
        if (byte[0] == '\n') {
            // Strip trailing \r
            const end = if (line_len > 0 and line_buf[line_len - 1] == '\r') line_len - 1 else line_len;
            return line_buf[0..end];
        }
        if (line_len < line_buf.len) {
            line_buf[line_len] = byte[0];
            line_len += 1;
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

// Transport tests use pipe() for testability in Zig 0.15
const testing = std.testing;

test "writeMessage and readMessage roundtrip" {
    // Create a pipe
    const pipe = try std.posix.pipe();
    const read_file = std.fs.File{ .handle = pipe[0] };
    const write_file = std.fs.File{ .handle = pipe[1] };
    defer read_file.close();

    // Write a message
    try writeMessage(write_file, "{\"id\":1}");
    write_file.close();

    // Read it back
    const body = try readMessage(testing.allocator, read_file);
    defer testing.allocator.free(body);
    try testing.expectEqualStrings("{\"id\":1}", body);
}

test "readMessage: end of stream" {
    const pipe = try std.posix.pipe();
    const read_file = std.fs.File{ .handle = pipe[0] };
    const write_file = std.fs.File{ .handle = pipe[1] };
    write_file.close();
    defer read_file.close();

    try testing.expectError(TransportError.EndOfStream, readMessage(testing.allocator, read_file));
}
