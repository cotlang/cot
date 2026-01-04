//! Text File Handle
//!
//! Manages a single text file with buffered line-based I/O.
//! Used by both Cot Core File.* API and DBL channel-based I/O.

const std = @import("std");

/// Text file access mode
pub const Mode = enum {
    read,
    write,
    append,
    read_write,

    pub fn fromString(mode: []const u8) Mode {
        if (mode.len == 0) return .read;

        // Check for DBL-style modes first
        var upper_buf: [32]u8 = undefined;
        const len = @min(mode.len, 31);
        for (mode[0..len], 0..) |c, i| {
            upper_buf[i] = std.ascii.toUpper(c);
        }
        const upper = upper_buf[0..len];

        // DBL modes: I:SEQ, O:SEQ, A:SEQ, U:SEQ
        if (std.mem.indexOf(u8, upper, "O") != null) return .write;
        if (std.mem.indexOf(u8, upper, "A") != null) return .append;
        if (std.mem.indexOf(u8, upper, "U") != null) return .read_write;

        // Cot Core modes: "r", "w", "a", "rw"
        if (std.mem.eql(u8, mode, "w") or std.mem.eql(u8, mode, "write")) return .write;
        if (std.mem.eql(u8, mode, "a") or std.mem.eql(u8, mode, "append")) return .append;
        if (std.mem.eql(u8, mode, "rw") or std.mem.eql(u8, mode, "readWrite")) return .read_write;

        return .read;
    }
};

/// A text file handle with buffered reading
pub const TextFileHandle = struct {
    file: std.fs.File,
    mode: Mode,
    read_buffer: [4096]u8 = undefined,
    read_pos: usize = 0,
    read_len: usize = 0,
    eof_reached: bool = false,

    const Self = @This();

    /// Open a text file
    pub fn open(path: []const u8, mode: Mode) !Self {
        const file = switch (mode) {
            .write => std.fs.cwd().createFile(path, .{}) catch |err| {
                return err;
            },
            .append => blk: {
                var f = std.fs.cwd().openFile(path, .{ .mode = .write_only }) catch {
                    // Create if doesn't exist
                    break :blk std.fs.cwd().createFile(path, .{}) catch |err| {
                        return err;
                    };
                };
                f.seekFromEnd(0) catch {};
                break :blk f;
            },
            .read => std.fs.cwd().openFile(path, .{ .mode = .read_only }) catch |err| {
                return err;
            },
            .read_write => std.fs.cwd().openFile(path, .{ .mode = .read_write }) catch |err| {
                return err;
            },
        };

        return Self{
            .file = file,
            .mode = mode,
        };
    }

    /// Close the file
    pub fn close(self: *Self) void {
        self.file.close();
        self.read_pos = 0;
        self.read_len = 0;
        self.eof_reached = false;
    }

    /// Read a line from the file
    /// Returns null at EOF
    pub fn readLine(self: *Self, allocator: std.mem.Allocator) !?[]u8 {
        if (self.eof_reached) {
            return null;
        }

        var line_buf: std.ArrayListUnmanaged(u8) = .empty;
        errdefer line_buf.deinit(allocator);

        while (true) {
            // Refill buffer if needed
            if (self.read_pos >= self.read_len) {
                self.read_len = self.file.read(&self.read_buffer) catch |err| {
                    if (line_buf.items.len == 0) {
                        self.eof_reached = true;
                        return null;
                    }
                    return err;
                };
                self.read_pos = 0;
                if (self.read_len == 0) {
                    self.eof_reached = true;
                    if (line_buf.items.len == 0) {
                        return null;
                    }
                    break;
                }
            }

            const byte = self.read_buffer[self.read_pos];
            self.read_pos += 1;

            if (byte == '\n') break;
            if (byte != '\r') { // Skip CR in CRLF
                try line_buf.append(allocator, byte);
            }
        }

        return try line_buf.toOwnedSlice(allocator);
    }

    /// Write a line to the file (with newline)
    pub fn writeLine(self: *Self, data: []const u8) !void {
        try self.file.writeAll(data);
        try self.file.writeAll("\n");
    }

    /// Write raw data to the file (without newline)
    pub fn write(self: *Self, data: []const u8) !void {
        try self.file.writeAll(data);
    }

    /// Check if EOF has been reached
    pub fn eof(self: *const Self) bool {
        return self.eof_reached;
    }

    /// Flush any buffered writes (no-op for direct writes)
    pub fn flush(self: *Self) !void {
        _ = self;
        // Writes are direct, no buffering needed
    }
};

// ============================================================================
// Tests
// ============================================================================

test "TextFileHandle: write and read" {
    const allocator = std.testing.allocator;

    // Write test file
    {
        var f = try TextFileHandle.open("/tmp/test_text_handle.txt", .write);
        defer f.close();
        try f.writeLine("Hello");
        try f.writeLine("World");
    }

    // Read test file
    {
        var f = try TextFileHandle.open("/tmp/test_text_handle.txt", .read);
        defer f.close();

        const line1 = try f.readLine(allocator);
        defer if (line1) |l| allocator.free(l);
        try std.testing.expectEqualStrings("Hello", line1.?);

        const line2 = try f.readLine(allocator);
        defer if (line2) |l| allocator.free(l);
        try std.testing.expectEqualStrings("World", line2.?);

        const line3 = try f.readLine(allocator);
        try std.testing.expect(line3 == null);
        try std.testing.expect(f.eof());
    }
}

test "Mode.fromString" {
    try std.testing.expectEqual(Mode.read, Mode.fromString("r"));
    try std.testing.expectEqual(Mode.write, Mode.fromString("w"));
    try std.testing.expectEqual(Mode.append, Mode.fromString("a"));
    try std.testing.expectEqual(Mode.read_write, Mode.fromString("rw"));

    // DBL modes
    try std.testing.expectEqual(Mode.read, Mode.fromString("I:SEQ"));
    try std.testing.expectEqual(Mode.write, Mode.fromString("O:SEQ"));
    try std.testing.expectEqual(Mode.read_write, Mode.fromString("U:SEQ"));
}
