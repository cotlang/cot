//! std.crypto - Cryptographic Hash Functions
//!
//! Hashing:
//!   sha256_hex(data)           - SHA-256 hash as hex string
//!   sha256(data)               - SHA-256 hash as raw bytes (32-byte string)
//!   sha512_hex(data)           - SHA-512 hash as hex string
//!   md5_hex(data)              - MD5 hash as hex string (legacy)
//!
//! HMAC:
//!   hmac_sha256_hex(key, data) - HMAC-SHA256 as hex string
//!
//! Utility:
//!   hex_encode(data)           - Encode bytes to hex string
//!   hex_decode(hex)            - Decode hex string to bytes

const std = @import("std");
const native = @import("native.zig");
const debug = @import("../debug.zig");
const NativeContext = native.NativeContext;
const NativeError = native.NativeError;
const Value = native.Value;

const Sha256 = std.crypto.hash.sha2.Sha256;
const Sha512 = std.crypto.hash.sha2.Sha512;
const Md5 = std.crypto.hash.Md5;
const HmacSha256 = std.crypto.auth.hmac.sha2.HmacSha256;

/// Register std.crypto functions
pub fn register(registry: anytype) !void {
    // SHA-256
    try registry.registerNative("std.crypto.sha256_hex", crypto_sha256_hex);
    try registry.registerNative("std.crypto.sha256", crypto_sha256);

    // SHA-512
    try registry.registerNative("std.crypto.sha512_hex", crypto_sha512_hex);

    // MD5 (legacy, not for security)
    try registry.registerNative("std.crypto.md5_hex", crypto_md5_hex);

    // HMAC
    try registry.registerNative("std.crypto.hmac_sha256_hex", crypto_hmac_sha256_hex);

    // Utility
    try registry.registerNative("std.crypto.hex_encode", crypto_hex_encode);
    try registry.registerNative("std.crypto.hex_decode", crypto_hex_decode);

    // Short names for convenience
    try registry.registerNative("sha256_hex", crypto_sha256_hex);
    try registry.registerNative("sha256", crypto_sha256);
    try registry.registerNative("sha512_hex", crypto_sha512_hex);
    try registry.registerNative("md5_hex", crypto_md5_hex);
    try registry.registerNative("hmac_sha256_hex", crypto_hmac_sha256_hex);
    try registry.registerNative("hex_encode", crypto_hex_encode);
    try registry.registerNative("hex_decode", crypto_hex_decode);
}

// =============================================================================
// SHA-256
// =============================================================================

/// sha256_hex(data) -> string
/// Compute SHA-256 hash and return as lowercase hex string (64 chars).
fn crypto_sha256_hex(ctx: *NativeContext) NativeError!?Value {
    const data = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(data, &digest, .{});

    // Convert to hex
    const hex = std.fmt.bytesToHex(digest, .lower);
    const result = ctx.allocator.dupe(u8, &hex) catch return NativeError.OutOfMemory;

    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

/// sha256(data) -> string
/// Compute SHA-256 hash and return as raw 32-byte string.
fn crypto_sha256(ctx: *NativeContext) NativeError!?Value {
    const data = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    var digest: [Sha256.digest_length]u8 = undefined;
    Sha256.hash(data, &digest, .{});

    const result = ctx.allocator.dupe(u8, &digest) catch return NativeError.OutOfMemory;
    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

// =============================================================================
// SHA-512
// =============================================================================

/// sha512_hex(data) -> string
/// Compute SHA-512 hash and return as lowercase hex string (128 chars).
fn crypto_sha512_hex(ctx: *NativeContext) NativeError!?Value {
    const data = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    var digest: [Sha512.digest_length]u8 = undefined;
    Sha512.hash(data, &digest, .{});

    const hex = std.fmt.bytesToHex(digest, .lower);
    const result = ctx.allocator.dupe(u8, &hex) catch return NativeError.OutOfMemory;

    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

// =============================================================================
// MD5 (legacy, not for security use)
// =============================================================================

/// md5_hex(data) -> string
/// Compute MD5 hash and return as lowercase hex string (32 chars).
/// Note: MD5 is cryptographically broken. Use only for legacy compatibility.
fn crypto_md5_hex(ctx: *NativeContext) NativeError!?Value {
    const data = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    var digest: [Md5.digest_length]u8 = undefined;
    Md5.hash(data, &digest, .{});

    const hex = std.fmt.bytesToHex(digest, .lower);
    const result = ctx.allocator.dupe(u8, &hex) catch return NativeError.OutOfMemory;

    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

// =============================================================================
// HMAC
// =============================================================================

/// hmac_sha256_hex(key, data) -> string
/// Compute HMAC-SHA256 and return as lowercase hex string.
/// Useful for webhook signature verification.
fn crypto_hmac_sha256_hex(ctx: *NativeContext) NativeError!?Value {
    const key = ctx.getArgString(0) catch return NativeError.InvalidArgument;
    const data = ctx.getArgString(1) catch return NativeError.InvalidArgument;

    var mac: [HmacSha256.mac_length]u8 = undefined;
    HmacSha256.create(&mac, data, key);

    const hex = std.fmt.bytesToHex(mac, .lower);
    const result = ctx.allocator.dupe(u8, &hex) catch return NativeError.OutOfMemory;

    return Value.initString(ctx.allocator, result) catch return NativeError.OutOfMemory;
}

// =============================================================================
// Utility Functions
// =============================================================================

/// hex_encode(data) -> string
/// Encode arbitrary bytes to lowercase hex string.
fn crypto_hex_encode(ctx: *NativeContext) NativeError!?Value {
    const data = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    // Allocate space for hex output (2 chars per byte)
    const hex_len = data.len * 2;
    const hex_buf = ctx.allocator.alloc(u8, hex_len) catch return NativeError.OutOfMemory;

    // Manual hex encoding since bytesToHex needs comptime-known size
    const hex_chars = "0123456789abcdef";
    for (data, 0..) |byte, i| {
        hex_buf[i * 2] = hex_chars[byte >> 4];
        hex_buf[i * 2 + 1] = hex_chars[byte & 0x0f];
    }

    return Value.initString(ctx.allocator, hex_buf) catch return NativeError.OutOfMemory;
}

/// hex_decode(hex) -> string
/// Decode hex string to bytes. Returns empty string on invalid hex.
fn crypto_hex_decode(ctx: *NativeContext) NativeError!?Value {
    const hex = ctx.getArgString(0) catch return NativeError.InvalidArgument;

    // Must be even length
    if (hex.len % 2 != 0) {
        debug.print(.general, "hex_decode: odd length input", .{});
        const empty = ctx.allocator.dupe(u8, "") catch return NativeError.OutOfMemory;
        return Value.initString(ctx.allocator, empty) catch return NativeError.OutOfMemory;
    }

    const out_len = hex.len / 2;
    const out_buf = ctx.allocator.alloc(u8, out_len) catch return NativeError.OutOfMemory;

    for (0..out_len) |i| {
        const high = hexCharToNibble(hex[i * 2]) orelse {
            debug.print(.general, "hex_decode: invalid char at position {}", .{i * 2});
            ctx.allocator.free(out_buf);
            const empty = ctx.allocator.dupe(u8, "") catch return NativeError.OutOfMemory;
            return Value.initString(ctx.allocator, empty) catch return NativeError.OutOfMemory;
        };
        const low = hexCharToNibble(hex[i * 2 + 1]) orelse {
            debug.print(.general, "hex_decode: invalid char at position {}", .{i * 2 + 1});
            ctx.allocator.free(out_buf);
            const empty = ctx.allocator.dupe(u8, "") catch return NativeError.OutOfMemory;
            return Value.initString(ctx.allocator, empty) catch return NativeError.OutOfMemory;
        };
        out_buf[i] = (@as(u8, high) << 4) | @as(u8, low);
    }

    return Value.initString(ctx.allocator, out_buf) catch return NativeError.OutOfMemory;
}

fn hexCharToNibble(c: u8) ?u4 {
    return switch (c) {
        '0'...'9' => @intCast(c - '0'),
        'a'...'f' => @intCast(c - 'a' + 10),
        'A'...'F' => @intCast(c - 'A' + 10),
        else => null,
    };
}
