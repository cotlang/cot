//! WebAssembly binary encoding utilities.
//! Reference: https://webassembly.github.io/spec/core/binary/conventions.html

const std = @import("std");
const wasm = @import("wasm_opcodes.zig");

// ============================================================================
// LEB128 Encoding
// ============================================================================

/// Encode unsigned LEB128 integer.
pub fn encodeULEB128(writer: anytype, value: u64) !void {
    var v = value;
    while (true) {
        const byte: u8 = @truncate(v & 0x7F);
        v >>= 7;
        if (v == 0) {
            try writer.writeByte(byte);
            return;
        }
        try writer.writeByte(byte | 0x80);
    }
}

/// Encode signed LEB128 integer.
pub fn encodeSLEB128(writer: anytype, value: i64) !void {
    var v = value;
    while (true) {
        const byte: u8 = @truncate(@as(u64, @bitCast(v)) & 0x7F);
        v >>= 7;

        // Check if we're done: remaining bits are all sign extension
        const done = (v == 0 and (byte & 0x40) == 0) or (v == -1 and (byte & 0x40) != 0);

        if (done) {
            try writer.writeByte(byte);
            return;
        }
        try writer.writeByte(byte | 0x80);
    }
}

// ============================================================================
// Section Writing
// ============================================================================

/// Write a section with content. Handles size encoding.
pub fn writeSection(writer: anytype, section: wasm.Section, content: []const u8) !void {
    try writer.writeByte(@intFromEnum(section));
    try encodeULEB128(writer, content.len);
    try writer.writeAll(content);
}

// ============================================================================
// Type Encoding
// ============================================================================

/// Write a function type signature.
pub fn writeFuncType(writer: anytype, params: []const wasm.ValType, results: []const wasm.ValType) !void {
    try writer.writeByte(wasm.FUNC_TYPE_TAG); // 0x60
    try encodeULEB128(writer, params.len);
    for (params) |p| try writer.writeByte(@intFromEnum(p));
    try encodeULEB128(writer, results.len);
    for (results) |r| try writer.writeByte(@intFromEnum(r));
}

// ============================================================================
// String Encoding
// ============================================================================

/// Write a name (length-prefixed UTF-8 string).
pub fn writeName(writer: anytype, name: []const u8) !void {
    try encodeULEB128(writer, name.len);
    try writer.writeAll(name);
}

// ============================================================================
// Module Header
// ============================================================================

/// Write the Wasm module header (magic + version).
pub fn writeHeader(writer: anytype) !void {
    try writer.writeAll(&wasm.MAGIC);
    try writer.writeAll(&wasm.VERSION);
}

// ============================================================================
// Tests
// ============================================================================

test "ULEB128 single byte" {
    var buf: [16]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    try encodeULEB128(writer, 0);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x00}, stream.getWritten());

    stream.reset();
    try encodeULEB128(writer, 1);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x01}, stream.getWritten());

    stream.reset();
    try encodeULEB128(writer, 127);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x7F}, stream.getWritten());
}

test "ULEB128 multi byte" {
    var buf: [16]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    // 128 = 0x80 = 10000000 -> 0x80 0x01
    try encodeULEB128(writer, 128);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x80, 0x01 }, stream.getWritten());

    stream.reset();
    // 624485 = 0x98765 -> E5 8E 26
    try encodeULEB128(writer, 624485);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xE5, 0x8E, 0x26 }, stream.getWritten());
}

test "SLEB128 positive" {
    var buf: [16]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    try encodeSLEB128(writer, 0);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x00}, stream.getWritten());

    stream.reset();
    try encodeSLEB128(writer, 1);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x01}, stream.getWritten());

    stream.reset();
    try encodeSLEB128(writer, 63);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x3F}, stream.getWritten());

    stream.reset();
    // 64 needs two bytes because bit 6 would be sign
    try encodeSLEB128(writer, 64);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xC0, 0x00 }, stream.getWritten());
}

test "SLEB128 negative" {
    var buf: [16]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    const writer = stream.writer();

    try encodeSLEB128(writer, -1);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x7F}, stream.getWritten());

    stream.reset();
    try encodeSLEB128(writer, -64);
    try std.testing.expectEqualSlices(u8, &[_]u8{0x40}, stream.getWritten());

    stream.reset();
    // -65 needs two bytes
    try encodeSLEB128(writer, -65);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xBF, 0x7F }, stream.getWritten());

    stream.reset();
    // -123456
    try encodeSLEB128(writer, -123456);
    try std.testing.expectEqualSlices(u8, &[_]u8{ 0xC0, 0xBB, 0x78 }, stream.getWritten());
}

test "write header" {
    var buf: [16]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    try writeHeader(stream.writer());
    try std.testing.expectEqualSlices(u8, &[_]u8{
        0x00, 0x61, 0x73, 0x6D, // \0asm
        0x01, 0x00, 0x00, 0x00, // version 1
    }, stream.getWritten());
}

test "write func type" {
    var buf: [32]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    // fn(i64, i64) -> i64
    try writeFuncType(stream.writer(), &[_]wasm.ValType{ .i64, .i64 }, &[_]wasm.ValType{.i64});
    try std.testing.expectEqualSlices(u8, &[_]u8{
        0x60,       // func type tag
        0x02,       // 2 params
        0x7E, 0x7E, // i64, i64
        0x01,       // 1 result
        0x7E,       // i64
    }, stream.getWritten());
}

test "write func type no params no results" {
    var buf: [32]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    // fn() -> void
    try writeFuncType(stream.writer(), &[_]wasm.ValType{}, &[_]wasm.ValType{});
    try std.testing.expectEqualSlices(u8, &[_]u8{
        0x60, // func type tag
        0x00, // 0 params
        0x00, // 0 results
    }, stream.getWritten());
}

test "write name" {
    var buf: [32]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    try writeName(stream.writer(), "main");
    try std.testing.expectEqualSlices(u8, &[_]u8{
        0x04,            // length
        'm', 'a', 'i', 'n',
    }, stream.getWritten());
}

test "write section" {
    var buf: [32]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);

    const content = [_]u8{ 0x01, 0x02, 0x03 };
    try writeSection(stream.writer(), .type, &content);
    try std.testing.expectEqualSlices(u8, &[_]u8{
        0x01,             // section id (type)
        0x03,             // content length
        0x01, 0x02, 0x03, // content
    }, stream.getWritten());
}
