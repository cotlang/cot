//! ULID - Universally Unique Lexicographically Sortable Identifier
//!
//! A 128-bit identifier that is:
//! - Lexicographically sortable (by creation time)
//! - Globally unique
//! - Human readable (26 character Crockford Base32)
//! - URL safe
//!
//! Structure:
//!   01ARZ3NDEKTSV4RRFFQ69G5FAV
//!   |-------||----------------|
//!   Timestamp    Randomness
//!    48-bit       80-bit
//!    10 chars     16 chars

const std = @import("std");

/// ULID binary representation (128 bits = 16 bytes)
pub const ULID = struct {
    bytes: [16]u8,

    /// Create a new ULID with current timestamp
    pub fn new() ULID {
        return newWithTimestamp(std.time.milliTimestamp());
    }

    /// Create a ULID with a specific timestamp (milliseconds since Unix epoch)
    pub fn newWithTimestamp(timestamp_ms: i64) ULID {
        var ulid: ULID = undefined;

        // First 6 bytes: timestamp (48 bits, big endian)
        const ts: u48 = @intCast(@as(u64, @bitCast(timestamp_ms)) & 0xFFFFFFFFFFFF);
        ulid.bytes[0] = @intCast((ts >> 40) & 0xFF);
        ulid.bytes[1] = @intCast((ts >> 32) & 0xFF);
        ulid.bytes[2] = @intCast((ts >> 24) & 0xFF);
        ulid.bytes[3] = @intCast((ts >> 16) & 0xFF);
        ulid.bytes[4] = @intCast((ts >> 8) & 0xFF);
        ulid.bytes[5] = @intCast(ts & 0xFF);

        // Remaining 10 bytes: random
        std.crypto.random.bytes(ulid.bytes[6..16]);

        return ulid;
    }

    /// Encode ULID to 26-character Crockford Base32 string
    pub fn encode(self: ULID) [26]u8 {
        var result: [26]u8 = undefined;

        // Crockford's Base32 alphabet (excludes I, L, O, U to avoid confusion)
        const alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";

        // Encode timestamp (first 10 characters from first 6 bytes)
        // 48 bits = 10 base32 characters (each char = 5 bits, 10 * 5 = 50, we use 48)
        result[0] = alphabet[(self.bytes[0] & 0xE0) >> 5];
        result[1] = alphabet[self.bytes[0] & 0x1F];
        result[2] = alphabet[(self.bytes[1] & 0xF8) >> 3];
        result[3] = alphabet[((self.bytes[1] & 0x07) << 2) | ((self.bytes[2] & 0xC0) >> 6)];
        result[4] = alphabet[(self.bytes[2] & 0x3E) >> 1];
        result[5] = alphabet[((self.bytes[2] & 0x01) << 4) | ((self.bytes[3] & 0xF0) >> 4)];
        result[6] = alphabet[((self.bytes[3] & 0x0F) << 1) | ((self.bytes[4] & 0x80) >> 7)];
        result[7] = alphabet[(self.bytes[4] & 0x7C) >> 2];
        result[8] = alphabet[((self.bytes[4] & 0x03) << 3) | ((self.bytes[5] & 0xE0) >> 5)];
        result[9] = alphabet[self.bytes[5] & 0x1F];

        // Encode randomness (remaining 16 characters from last 10 bytes)
        // 80 bits = 16 base32 characters
        result[10] = alphabet[(self.bytes[6] & 0xF8) >> 3];
        result[11] = alphabet[((self.bytes[6] & 0x07) << 2) | ((self.bytes[7] & 0xC0) >> 6)];
        result[12] = alphabet[(self.bytes[7] & 0x3E) >> 1];
        result[13] = alphabet[((self.bytes[7] & 0x01) << 4) | ((self.bytes[8] & 0xF0) >> 4)];
        result[14] = alphabet[((self.bytes[8] & 0x0F) << 1) | ((self.bytes[9] & 0x80) >> 7)];
        result[15] = alphabet[(self.bytes[9] & 0x7C) >> 2];
        result[16] = alphabet[((self.bytes[9] & 0x03) << 3) | ((self.bytes[10] & 0xE0) >> 5)];
        result[17] = alphabet[self.bytes[10] & 0x1F];
        result[18] = alphabet[(self.bytes[11] & 0xF8) >> 3];
        result[19] = alphabet[((self.bytes[11] & 0x07) << 2) | ((self.bytes[12] & 0xC0) >> 6)];
        result[20] = alphabet[(self.bytes[12] & 0x3E) >> 1];
        result[21] = alphabet[((self.bytes[12] & 0x01) << 4) | ((self.bytes[13] & 0xF0) >> 4)];
        result[22] = alphabet[((self.bytes[13] & 0x0F) << 1) | ((self.bytes[14] & 0x80) >> 7)];
        result[23] = alphabet[(self.bytes[14] & 0x7C) >> 2];
        result[24] = alphabet[((self.bytes[14] & 0x03) << 3) | ((self.bytes[15] & 0xE0) >> 5)];
        result[25] = alphabet[self.bytes[15] & 0x1F];

        return result;
    }

    /// Decode a 26-character ULID string to binary
    pub fn decode(str: []const u8) !ULID {
        if (str.len != 26) {
            return error.InvalidLength;
        }

        var ulid: ULID = undefined;

        // Decode each character to 5-bit value
        var values: [26]u8 = undefined;
        for (str, 0..) |c, i| {
            values[i] = try decodeChar(c);
        }

        // Reconstruct bytes from 5-bit values
        // Timestamp (10 chars -> 6 bytes)
        ulid.bytes[0] = (values[0] << 5) | values[1];
        ulid.bytes[1] = (values[2] << 3) | (values[3] >> 2);
        ulid.bytes[2] = (values[3] << 6) | (values[4] << 1) | (values[5] >> 4);
        ulid.bytes[3] = (values[5] << 4) | (values[6] >> 1);
        ulid.bytes[4] = (values[6] << 7) | (values[7] << 2) | (values[8] >> 3);
        ulid.bytes[5] = (values[8] << 5) | values[9];

        // Randomness (16 chars -> 10 bytes)
        ulid.bytes[6] = (values[10] << 3) | (values[11] >> 2);
        ulid.bytes[7] = (values[11] << 6) | (values[12] << 1) | (values[13] >> 4);
        ulid.bytes[8] = (values[13] << 4) | (values[14] >> 1);
        ulid.bytes[9] = (values[14] << 7) | (values[15] << 2) | (values[16] >> 3);
        ulid.bytes[10] = (values[16] << 5) | values[17];
        ulid.bytes[11] = (values[18] << 3) | (values[19] >> 2);
        ulid.bytes[12] = (values[19] << 6) | (values[20] << 1) | (values[21] >> 4);
        ulid.bytes[13] = (values[21] << 4) | (values[22] >> 1);
        ulid.bytes[14] = (values[22] << 7) | (values[23] << 2) | (values[24] >> 3);
        ulid.bytes[15] = (values[24] << 5) | values[25];

        return ulid;
    }

    /// Get timestamp component (milliseconds since Unix epoch)
    pub fn timestamp(self: ULID) i64 {
        var ts: u48 = 0;
        ts |= @as(u48, self.bytes[0]) << 40;
        ts |= @as(u48, self.bytes[1]) << 32;
        ts |= @as(u48, self.bytes[2]) << 24;
        ts |= @as(u48, self.bytes[3]) << 16;
        ts |= @as(u48, self.bytes[4]) << 8;
        ts |= @as(u48, self.bytes[5]);
        return @intCast(ts);
    }

    /// Compare two ULIDs (for sorting)
    pub fn compare(self: ULID, other: ULID) std.math.Order {
        return std.mem.order(u8, &self.bytes, &other.bytes);
    }

    /// Check equality
    pub fn eql(self: ULID, other: ULID) bool {
        return std.mem.eql(u8, &self.bytes, &other.bytes);
    }

    /// Zero/nil ULID
    pub const nil: ULID = .{ .bytes = [_]u8{0} ** 16 };

    /// Check if ULID is nil
    pub fn isNil(self: ULID) bool {
        return std.mem.allEqual(u8, &self.bytes, 0);
    }
};

/// Decode a single Crockford Base32 character to its 5-bit value
fn decodeChar(c: u8) !u8 {
    return switch (c) {
        '0', 'O', 'o' => 0,
        '1', 'I', 'i', 'L', 'l' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        'A', 'a' => 10,
        'B', 'b' => 11,
        'C', 'c' => 12,
        'D', 'd' => 13,
        'E', 'e' => 14,
        'F', 'f' => 15,
        'G', 'g' => 16,
        'H', 'h' => 17,
        'J', 'j' => 18,
        'K', 'k' => 19,
        'M', 'm' => 20,
        'N', 'n' => 21,
        'P', 'p' => 22,
        'Q', 'q' => 23,
        'R', 'r' => 24,
        'S', 's' => 25,
        'T', 't' => 26,
        'V', 'v' => 27,
        'W', 'w' => 28,
        'X', 'x' => 29,
        'Y', 'y' => 30,
        'Z', 'z' => 31,
        else => error.InvalidCharacter,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "ULID generation and encoding" {
    const ulid = ULID.new();
    const encoded = ulid.encode();

    // Should be 26 characters
    try std.testing.expectEqual(@as(usize, 26), encoded.len);

    // Should only contain valid Crockford Base32 characters
    for (encoded) |c| {
        try std.testing.expect((c >= '0' and c <= '9') or (c >= 'A' and c <= 'Z'));
        // Should not contain I, L, O, U
        try std.testing.expect(c != 'I' and c != 'L' and c != 'O' and c != 'U');
    }
}

test "ULID decode and roundtrip" {
    const original = ULID.new();
    const encoded = original.encode();
    const decoded = try ULID.decode(&encoded);

    try std.testing.expect(original.eql(decoded));
}

test "ULID timestamp extraction" {
    const before = std.time.milliTimestamp();
    const ulid = ULID.new();
    const after = std.time.milliTimestamp();

    const ts = ulid.timestamp();
    try std.testing.expect(ts >= before);
    try std.testing.expect(ts <= after);
}

test "ULID ordering" {
    // ULIDs generated later should be greater
    const ulid1 = ULID.new();
    std.Thread.sleep(1_000_000); // 1ms
    const ulid2 = ULID.new();

    try std.testing.expect(ulid1.compare(ulid2) == .lt);
}

test "ULID nil" {
    const nil = ULID.nil;
    try std.testing.expect(nil.isNil());

    const ulid = ULID.new();
    try std.testing.expect(!ulid.isNil());
}

test "ULID case insensitive decode" {
    const ulid = ULID.new();
    const encoded = ulid.encode();

    // Convert to lowercase
    var lower: [26]u8 = undefined;
    for (encoded, 0..) |c, i| {
        lower[i] = std.ascii.toLower(c);
    }

    const decoded = try ULID.decode(&lower);
    try std.testing.expect(ulid.eql(decoded));
}
