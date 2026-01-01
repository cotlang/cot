//! Unified Channel Manager
//!
//! Manages I/O channels that can be either:
//! - Text/Sequential files (line-based I/O)
//! - ISAM database files (keyed/indexed access via cotdb)
//!
//! Legacy open modes determine channel type:
//! - I:SEQ, O:SEQ, A:SEQ → Text file
//! - I:ISAM, U:ISAM → ISAM file
//! - I, O, U, A (no qualifier) → Auto-detect from extension

const std = @import("std");
const cotdb = @import("cotdb");
const debug = @import("debug.zig");

pub const MAX_CHANNELS: usize = 256;

/// Channel type determines which backend handles I/O
pub const ChannelType = enum {
    closed,
    text,
    isam,
};

/// Text file access mode
pub const TextMode = enum {
    read,       // I:SEQ - read existing
    write,      // O:SEQ - create/overwrite
    append,     // A:SEQ - append
    update,     // U:SEQ - read/write
};

/// A unified channel that can be text or ISAM
pub const Channel = struct {
    channel_type: ChannelType = .closed,

    // Text file state
    text_file: ?std.fs.File = null,
    text_mode: TextMode = .read,

    // Read buffer for line reading
    read_buffer: [4096]u8 = undefined,
    read_pos: usize = 0,
    read_len: usize = 0,

    // ISAM state is managed by CursorManager using same channel ID

    /// Close this channel
    pub fn close(self: *Channel) void {
        if (self.text_file) |f| {
            f.close();
            self.text_file = null;
        }
        self.read_pos = 0;
        self.read_len = 0;
        self.channel_type = .closed;
    }

    /// Read a line from text file
    pub fn readLine(self: *Channel, allocator: std.mem.Allocator) !?[]u8 {
        if (self.channel_type != .text) return error.WrongChannelType;

        const file = self.text_file orelse return error.ChannelNotOpen;

        // Read until newline
        var line_buf: std.ArrayListUnmanaged(u8) = .empty;
        errdefer line_buf.deinit(allocator);

        while (true) {
            // Refill buffer if needed
            if (self.read_pos >= self.read_len) {
                self.read_len = file.read(&self.read_buffer) catch |err| {
                    if (line_buf.items.len == 0) return null; // EOF
                    return err;
                };
                self.read_pos = 0;
                if (self.read_len == 0) {
                    if (line_buf.items.len == 0) return null; // EOF
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

    /// Write a line to text file
    pub fn writeLine(self: *Channel, data: []const u8) !void {
        if (self.channel_type != .text) return error.WrongChannelType;

        var file = self.text_file orelse return error.ChannelNotOpen;

        // Use the new Zig 0.15 writer API
        var write_buffer: [4096]u8 = undefined;
        var file_writer = file.writer(&write_buffer);
        const writer = &file_writer.interface;

        try writer.writeAll(data);
        try writer.writeByte('\n');
        writer.flush() catch {};  // Flush to ensure data is written
    }

    /// Flush any buffered writes (no-op with new unbuffered approach)
    pub fn flush(self: *Channel) !void {
        _ = self;
        // Writes are flushed immediately in the new implementation
    }
};

/// Unified channel manager for text and ISAM files
pub const ChannelManager = struct {
    allocator: std.mem.Allocator,
    channels: []Channel,
    isam_cursors: *cotdb.CursorManager,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, isam_cursors: *cotdb.CursorManager) !Self {
        const channels = try allocator.alloc(Channel, MAX_CHANNELS);
        for (channels) |*ch| {
            ch.* = Channel{};
        }

        return Self{
            .allocator = allocator,
            .channels = channels,
            .isam_cursors = isam_cursors,
        };
    }

    pub fn deinit(self: *Self) void {
        // Close all open channels
        for (self.channels) |*ch| {
            ch.close();
        }
        self.allocator.free(self.channels);
    }

    /// Open a channel with mode detection
    /// Mode format: [I|O|U|A][:SEQ|:ISAM]
    pub fn open(self: *Self, channel_id: u8, mode: []const u8, filename: []const u8) !void {
        if (channel_id >= MAX_CHANNELS) return error.InvalidChannel;

        var channel = &self.channels[channel_id];

        // Close if already open
        if (channel.channel_type != .closed) {
            channel.close();
            if (channel.channel_type == .isam) {
                self.isam_cursors.close(channel_id);
            }
        }

        // Parse mode to determine file type
        const file_type = detectFileType(mode, filename);

        debug.print(.general, "ChannelManager.open: ch={d} mode='{s}' file='{s}' type={s}", .{
            channel_id, mode, filename, @tagName(file_type)
        });

        switch (file_type) {
            .text => {
                const text_mode = parseTextMode(mode);
                const flags: std.fs.File.OpenFlags = switch (text_mode) {
                    .read => .{ .mode = .read_only },
                    .write => .{ .mode = .write_only },
                    .append => .{ .mode = .write_only },
                    .update => .{ .mode = .read_write },
                };

                const file = blk: {
                    if (text_mode == .write) {
                        // Create/truncate for write mode
                        break :blk std.fs.cwd().createFile(filename, .{}) catch |err| {
                            debug.print(.general, "  createFile failed: {}", .{err});
                            return error.FileError;
                        };
                    } else if (text_mode == .append) {
                        // Open for append
                        var f = std.fs.cwd().openFile(filename, flags) catch {
                            // Create if doesn't exist
                            break :blk std.fs.cwd().createFile(filename, .{}) catch |err| {
                                debug.print(.general, "  createFile failed: {}", .{err});
                                return error.FileError;
                            };
                        };
                        f.seekFromEnd(0) catch {};
                        break :blk f;
                    } else {
                        break :blk std.fs.cwd().openFile(filename, flags) catch |err| {
                            debug.print(.general, "  openFile failed: {}", .{err});
                            return error.FileError;
                        };
                    }
                };

                channel.channel_type = .text;
                channel.text_file = file;
                channel.text_mode = text_mode;
                channel.read_pos = 0;
                channel.read_len = 0;
            },
            .isam => {
                // Delegate to ISAM cursor manager
                try self.isam_cursors.open(channel_id, filename);
                channel.channel_type = .isam;
            },
            .closed => unreachable,
        }
    }

    /// Close a channel
    pub fn close(self: *Self, channel_id: u8) void {
        if (channel_id >= MAX_CHANNELS) return;

        var channel = &self.channels[channel_id];
        if (channel.channel_type == .isam) {
            self.isam_cursors.close(channel_id);
        }
        channel.close();
    }

    /// Get channel type
    pub fn getType(self: *Self, channel_id: u8) ChannelType {
        if (channel_id >= MAX_CHANNELS) return .closed;
        return self.channels[channel_id].channel_type;
    }

    /// Get channel for text operations
    pub fn getTextChannel(self: *Self, channel_id: u8) ?*Channel {
        if (channel_id >= MAX_CHANNELS) return null;
        const channel = &self.channels[channel_id];
        if (channel.channel_type != .text) return null;
        return channel;
    }

    /// Read line from text channel
    pub fn readLine(self: *Self, channel_id: u8) !?[]u8 {
        const channel = self.getTextChannel(channel_id) orelse return error.WrongChannelType;
        return channel.readLine(self.allocator);
    }

    /// Write line to text channel
    pub fn writeLine(self: *Self, channel_id: u8, data: []const u8) !void {
        const channel = self.getTextChannel(channel_id) orelse return error.WrongChannelType;
        return channel.writeLine(data);
    }
};

/// Detect file type from mode string and filename
fn detectFileType(mode: []const u8, filename: []const u8) ChannelType {
    // Check for explicit mode qualifiers
    var upper_mode: [32]u8 = undefined;
    const mode_len = @min(mode.len, 31);
    for (mode[0..mode_len], 0..) |c, i| {
        upper_mode[i] = std.ascii.toUpper(c);
    }
    const mode_upper = upper_mode[0..mode_len];

    // Explicit SEQ qualifier
    if (std.mem.indexOf(u8, mode_upper, ":SEQ") != null or
        std.mem.indexOf(u8, mode_upper, "SEQ") != null) {
        return .text;
    }

    // Explicit ISAM qualifier
    if (std.mem.indexOf(u8, mode_upper, ":ISAM") != null or
        std.mem.indexOf(u8, mode_upper, "ISAM") != null) {
        return .isam;
    }

    // Auto-detect from extension
    if (std.mem.endsWith(u8, filename, ".ism") or
        std.mem.endsWith(u8, filename, ".ISM") or
        std.mem.endsWith(u8, filename, ".ddf") or
        std.mem.endsWith(u8, filename, ".DDF")) {
        return .isam;
    }

    // Default to text for everything else
    return .text;
}

/// Parse text file mode from legacy mode string
fn parseTextMode(mode: []const u8) TextMode {
    if (mode.len == 0) return .read;

    const first = std.ascii.toUpper(mode[0]);
    return switch (first) {
        'O' => .write,
        'A' => .append,
        'U' => .update,
        else => .read, // I or anything else
    };
}

// ============================================================================
// Tests
// ============================================================================

test "detect file type from mode" {
    try std.testing.expectEqual(ChannelType.text, detectFileType("I:SEQ", "data.txt"));
    try std.testing.expectEqual(ChannelType.text, detectFileType("O:SEQ", "output.dat"));
    try std.testing.expectEqual(ChannelType.isam, detectFileType("I:ISAM", "customer.ism"));
    try std.testing.expectEqual(ChannelType.isam, detectFileType("U:ISAM", "orders.ddf"));

    // Auto-detect from extension
    try std.testing.expectEqual(ChannelType.isam, detectFileType("I", "customer.ism"));
    try std.testing.expectEqual(ChannelType.text, detectFileType("I", "report.txt"));
    try std.testing.expectEqual(ChannelType.text, detectFileType("O", "output.csv"));
}

test "parse text mode" {
    try std.testing.expectEqual(TextMode.read, parseTextMode("I"));
    try std.testing.expectEqual(TextMode.read, parseTextMode("I:SEQ"));
    try std.testing.expectEqual(TextMode.write, parseTextMode("O"));
    try std.testing.expectEqual(TextMode.write, parseTextMode("O:SEQ"));
    try std.testing.expectEqual(TextMode.append, parseTextMode("A"));
    try std.testing.expectEqual(TextMode.update, parseTextMode("U"));
}
