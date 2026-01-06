//! WebSocket Implementation (RFC 6455)
//!
//! Provides WebSocket support for the Cot HTTP server, enabling real-time
//! bidirectional communication for the Dex framework.

const std = @import("std");
const http = std.http;
const net = std.net;
const Allocator = std.mem.Allocator;
const base64 = std.base64;
const Sha1 = std.crypto.hash.Sha1;

/// WebSocket opcodes (RFC 6455 Section 5.2)
pub const Opcode = enum(u4) {
    continuation = 0x0,
    text = 0x1,
    binary = 0x2,
    // 0x3-0x7 reserved for further non-control frames
    close = 0x8,
    ping = 0x9,
    pong = 0xA,
    // 0xB-0xF reserved for further control frames

    pub fn isControl(self: Opcode) bool {
        return @intFromEnum(self) >= 0x8;
    }
};

/// WebSocket close codes (RFC 6455 Section 7.4)
pub const CloseCode = enum(u16) {
    normal = 1000,
    going_away = 1001,
    protocol_error = 1002,
    unsupported_data = 1003,
    no_status = 1005,
    abnormal = 1006,
    invalid_payload = 1007,
    policy_violation = 1008,
    message_too_big = 1009,
    mandatory_extension = 1010,
    internal_error = 1011,
    tls_handshake = 1015,
};

/// WebSocket frame header
pub const FrameHeader = struct {
    fin: bool,
    rsv1: bool = false,
    rsv2: bool = false,
    rsv3: bool = false,
    opcode: Opcode,
    masked: bool,
    payload_len: u64,
    mask_key: ?[4]u8,
};

/// WebSocket message (assembled from frames)
pub const Message = struct {
    opcode: Opcode,
    data: []const u8,
    allocator: Allocator,

    pub fn deinit(self: *Message) void {
        self.allocator.free(self.data);
    }

    pub fn text(self: *const Message) []const u8 {
        return self.data;
    }
};

/// WebSocket connection handler function type
pub const WebSocketHandler = *const fn (*WebSocket) anyerror!void;

/// WebSocket connection
pub const WebSocket = struct {
    allocator: Allocator,
    stream: net.Stream,
    state: State,
    read_buffer: [65536]u8, // 64KB read buffer
    fragment_buffer: std.ArrayListUnmanaged(u8),
    fragment_opcode: ?Opcode,

    const State = enum {
        connecting,
        open,
        closing,
        closed,
    };

    const Self = @This();

    /// Initialize a new WebSocket connection
    pub fn init(allocator: Allocator, stream: net.Stream) Self {
        return .{
            .allocator = allocator,
            .stream = stream,
            .state = .connecting,
            .read_buffer = undefined,
            .fragment_buffer = .empty,
            .fragment_opcode = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.fragment_buffer.deinit(self.allocator);
        if (self.state != .closed) {
            self.stream.close();
            self.state = .closed;
        }
    }

    /// Perform WebSocket handshake (server-side)
    /// Returns true if handshake successful
    pub fn acceptHandshake(self: *Self, request_headers: anytype) !void {
        // Look for required WebSocket headers
        var sec_key: ?[]const u8 = null;
        var has_upgrade = false;
        var has_connection_upgrade = false;
        var ws_version: ?[]const u8 = null;

        // Iterate through headers
        var iter = request_headers.iterateHeaders();
        while (iter.next()) |header| {
            if (std.ascii.eqlIgnoreCase(header.name, "Sec-WebSocket-Key")) {
                sec_key = header.value;
            } else if (std.ascii.eqlIgnoreCase(header.name, "Upgrade")) {
                has_upgrade = std.ascii.eqlIgnoreCase(header.value, "websocket");
            } else if (std.ascii.eqlIgnoreCase(header.name, "Connection")) {
                // Connection header may contain multiple values
                has_connection_upgrade = std.mem.indexOf(u8, header.value, "Upgrade") != null or
                    std.mem.indexOf(u8, header.value, "upgrade") != null;
            } else if (std.ascii.eqlIgnoreCase(header.name, "Sec-WebSocket-Version")) {
                ws_version = header.value;
            }
        }

        // Validate required headers
        if (!has_upgrade or !has_connection_upgrade) {
            return error.InvalidUpgradeHeader;
        }
        if (sec_key == null) {
            return error.MissingSecWebSocketKey;
        }
        if (ws_version == null or !std.mem.eql(u8, ws_version.?, "13")) {
            return error.UnsupportedWebSocketVersion;
        }

        // Compute Sec-WebSocket-Accept
        const accept_key = try computeAcceptKey(sec_key.?);

        // Send handshake response
        var write_buf: [1024]u8 = undefined;
        const response = std.fmt.bufPrint(&write_buf,
            \\HTTP/1.1 101 Switching Protocols
            \\Upgrade: websocket
            \\Connection: Upgrade
            \\Sec-WebSocket-Accept: {s}
            \\
            \\
        , .{accept_key}) catch return error.ResponseBufferTooSmall;

        _ = try self.stream.write(response);

        self.state = .open;
    }

    /// Read a complete message (handles fragmentation)
    pub fn readMessage(self: *Self) !?Message {
        while (true) {
            const frame = try self.readFrame() orelse return null;

            switch (frame.opcode) {
                .ping => {
                    // Respond with pong
                    try self.sendPong(frame.data);
                    self.allocator.free(frame.data);
                    continue;
                },
                .pong => {
                    // Pong received, ignore
                    self.allocator.free(frame.data);
                    continue;
                },
                .close => {
                    // Close frame received
                    if (self.state == .open) {
                        self.state = .closing;
                        // Echo close frame
                        try self.sendClose(.normal, "");
                    }
                    self.allocator.free(frame.data);
                    self.state = .closed;
                    return null;
                },
                .continuation => {
                    // Continue fragmented message
                    if (self.fragment_opcode == null) {
                        self.allocator.free(frame.data);
                        return error.UnexpectedContinuation;
                    }
                    try self.fragment_buffer.appendSlice(self.allocator, frame.data);
                    self.allocator.free(frame.data);

                    if (frame.fin) {
                        // Message complete
                        const data = try self.fragment_buffer.toOwnedSlice(self.allocator);
                        const opcode = self.fragment_opcode.?;
                        self.fragment_opcode = null;
                        return Message{
                            .opcode = opcode,
                            .data = data,
                            .allocator = self.allocator,
                        };
                    }
                },
                .text, .binary => {
                    if (frame.fin) {
                        // Complete single-frame message
                        return Message{
                            .opcode = frame.opcode,
                            .data = frame.data,
                            .allocator = self.allocator,
                        };
                    } else {
                        // Start of fragmented message
                        if (self.fragment_opcode != null) {
                            self.allocator.free(frame.data);
                            return error.NestedFragmentation;
                        }
                        self.fragment_opcode = frame.opcode;
                        self.fragment_buffer.clearRetainingCapacity();
                        try self.fragment_buffer.appendSlice(self.allocator, frame.data);
                        self.allocator.free(frame.data);
                    }
                },
            }
        }
    }

    /// Read a single frame
    fn readFrame(self: *Self) !?Frame {
        if (self.state == .closed) return null;

        // Read first 2 bytes (minimum header)
        var header_buf: [2]u8 = undefined;
        const header_read = self.stream.read(&header_buf) catch |err| {
            if (err == error.ConnectionResetByPeer or err == error.BrokenPipe) {
                self.state = .closed;
                return null;
            }
            return err;
        };
        if (header_read == 0) {
            self.state = .closed;
            return null;
        }
        if (header_read < 2) return error.IncompleteFrame;

        const fin = (header_buf[0] & 0x80) != 0;
        const opcode_raw = header_buf[0] & 0x0F;
        const opcode: Opcode = @enumFromInt(opcode_raw);
        const masked = (header_buf[1] & 0x80) != 0;
        var payload_len: u64 = header_buf[1] & 0x7F;

        // Extended payload length
        if (payload_len == 126) {
            var len_buf: [2]u8 = undefined;
            _ = try self.stream.readAll(&len_buf);
            payload_len = std.mem.readInt(u16, &len_buf, .big);
        } else if (payload_len == 127) {
            var len_buf: [8]u8 = undefined;
            _ = try self.stream.readAll(&len_buf);
            payload_len = std.mem.readInt(u64, &len_buf, .big);
        }

        // Read masking key if present
        var mask_key: [4]u8 = undefined;
        if (masked) {
            _ = try self.stream.readAll(&mask_key);
        }

        // Read payload
        if (payload_len > self.read_buffer.len) {
            return error.PayloadTooLarge;
        }

        const payload_slice = self.read_buffer[0..@intCast(payload_len)];
        if (payload_len > 0) {
            _ = try self.stream.readAll(payload_slice);
        }

        // Unmask if needed
        if (masked) {
            for (payload_slice, 0..) |*byte, i| {
                byte.* ^= mask_key[i % 4];
            }
        }

        // Copy payload to owned slice
        const data = try self.allocator.dupe(u8, payload_slice);

        return Frame{
            .fin = fin,
            .opcode = opcode,
            .data = data,
        };
    }

    const Frame = struct {
        fin: bool,
        opcode: Opcode,
        data: []const u8,
    };

    /// Send a text message
    pub fn sendText(self: *Self, data: []const u8) !void {
        try self.sendFrame(.text, data, true);
    }

    /// Send a binary message
    pub fn sendBinary(self: *Self, data: []const u8) !void {
        try self.sendFrame(.binary, data, true);
    }

    /// Send a ping
    pub fn sendPing(self: *Self, data: []const u8) !void {
        try self.sendFrame(.ping, data, true);
    }

    /// Send a pong
    pub fn sendPong(self: *Self, data: []const u8) !void {
        try self.sendFrame(.pong, data, true);
    }

    /// Send a close frame
    pub fn sendClose(self: *Self, code: CloseCode, reason: []const u8) !void {
        var close_data: [125]u8 = undefined;
        std.mem.writeInt(u16, close_data[0..2], @intFromEnum(code), .big);
        const reason_len = @min(reason.len, 123);
        @memcpy(close_data[2..][0..reason_len], reason[0..reason_len]);

        try self.sendFrame(.close, close_data[0 .. 2 + reason_len], true);
        self.state = .closing;
    }

    /// Send a frame (server -> client, unmasked)
    fn sendFrame(self: *Self, opcode: Opcode, data: []const u8, fin: bool) !void {
        if (self.state != .open and self.state != .closing) {
            return error.ConnectionNotOpen;
        }

        var header_buf: [10]u8 = undefined;
        var header_len: usize = 2;

        // First byte: FIN + opcode
        header_buf[0] = @as(u8, if (fin) 0x80 else 0x00) | @intFromEnum(opcode);

        // Second byte: mask (0 for server) + payload length
        if (data.len < 126) {
            header_buf[1] = @intCast(data.len);
        } else if (data.len <= 65535) {
            header_buf[1] = 126;
            std.mem.writeInt(u16, header_buf[2..4], @intCast(data.len), .big);
            header_len = 4;
        } else {
            header_buf[1] = 127;
            std.mem.writeInt(u64, header_buf[2..10], data.len, .big);
            header_len = 10;
        }

        // Send header and payload
        _ = try self.stream.write(header_buf[0..header_len]);
        if (data.len > 0) {
            _ = try self.stream.write(data);
        }
    }

    /// Close the connection gracefully
    pub fn close(self: *Self) void {
        if (self.state == .open) {
            self.sendClose(.normal, "") catch {};
        }
        self.state = .closed;
        self.stream.close();
    }

    /// Check if the connection is open
    pub fn isOpen(self: *const Self) bool {
        return self.state == .open;
    }
};

/// Compute Sec-WebSocket-Accept value
fn computeAcceptKey(client_key: []const u8) ![28]u8 {
    const magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

    // Concatenate client key with magic GUID
    var concat_buf: [60 + 36]u8 = undefined; // max key length + magic length
    const concat_len = client_key.len + magic.len;
    if (concat_len > concat_buf.len) return error.KeyTooLong;

    @memcpy(concat_buf[0..client_key.len], client_key);
    @memcpy(concat_buf[client_key.len..][0..magic.len], magic);

    // SHA-1 hash
    var hash: [20]u8 = undefined;
    Sha1.hash(concat_buf[0..concat_len], &hash, .{});

    // Base64 encode
    var result: [28]u8 = undefined;
    _ = base64.standard.Encoder.encode(&result, &hash);

    return result;
}

/// Check if an HTTP request is a WebSocket upgrade request
pub fn isUpgradeRequest(request: anytype) bool {
    var has_upgrade = false;
    var has_connection_upgrade = false;
    var has_ws_key = false;

    var iter = request.iterateHeaders();
    while (iter.next()) |header| {
        if (std.ascii.eqlIgnoreCase(header.name, "Upgrade")) {
            has_upgrade = std.ascii.eqlIgnoreCase(header.value, "websocket");
        } else if (std.ascii.eqlIgnoreCase(header.name, "Connection")) {
            has_connection_upgrade = std.mem.indexOf(u8, header.value, "Upgrade") != null or
                std.mem.indexOf(u8, header.value, "upgrade") != null;
        } else if (std.ascii.eqlIgnoreCase(header.name, "Sec-WebSocket-Key")) {
            has_ws_key = true;
        }
    }

    return has_upgrade and has_connection_upgrade and has_ws_key;
}

// ============================================================================
// Tests
// ============================================================================

test "compute accept key" {
    // Test vector from RFC 6455
    const client_key = "dGhlIHNhbXBsZSBub25jZQ==";
    const expected = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=";

    const result = try computeAcceptKey(client_key);
    try std.testing.expectEqualStrings(expected, &result);
}

test "opcode is control" {
    try std.testing.expect(!Opcode.text.isControl());
    try std.testing.expect(!Opcode.binary.isControl());
    try std.testing.expect(!Opcode.continuation.isControl());
    try std.testing.expect(Opcode.close.isControl());
    try std.testing.expect(Opcode.ping.isControl());
    try std.testing.expect(Opcode.pong.isControl());
}
