//! Source text handling and position tracking.
//!
//! Every stage of the compiler needs to know WHERE something is in the
//! source code — for error messages, debug info, and LSP features. This
//! module provides three levels of position tracking:
//!
//!   Pos      — byte offset (compact, used internally everywhere)
//!   Span     — start + end offset (range of tokens or expressions)
//!   Position — filename + line + column (human-readable, for error output)
//!
//! Line offsets are computed lazily on first use and cached. Most
//! compilations never need line numbers (only on error), so this avoids
//! the cost of scanning for newlines in the happy path.

const std = @import("std");

/// Byte offset into source text. The fundamental position type used
/// throughout the compiler. Compact (4 bytes) and trivially comparable.
pub const Pos = struct {
    offset: u32,

    pub const zero = Pos{ .offset = 0 };

    pub fn advance(self: Pos, n: u32) Pos {
        return .{ .offset = self.offset + n };
    }
};

/// Human-readable source position for error messages (1-based line/column).
/// Only constructed when needed (errors, diagnostics) — not during normal
/// compilation. Implements `std.fmt` formatting for direct use in print.
pub const Position = struct {
    filename: []const u8,
    offset: u32,
    line: u32,
    column: u32,

    pub fn format(self: Position, comptime _: []const u8, _: std.fmt.FormatOptions, w: anytype) !void {
        if (self.filename.len > 0) try w.print("{s}:", .{self.filename});
        try w.print("{d}", .{self.line});
        if (self.column > 0) try w.print(":{d}", .{self.column});
    }

    pub fn toString(self: Position, allocator: std.mem.Allocator) ![]u8 {
        var buf = std.ArrayList(u8).init(allocator);
        try self.format("", .{}, buf.writer());
        return buf.toOwnedSlice();
    }
};

/// Range in source code (start offset to end offset).
/// Attached to every token and AST node so errors can point at the
/// exact characters that caused the problem.
pub const Span = struct {
    start: Pos,
    end: Pos,

    pub const zero = Span{ .start = Pos.zero, .end = Pos.zero };

    pub fn init(start: Pos, end: Pos) Span {
        return .{ .start = start, .end = end };
    }

    /// Create a zero-length span at a single position.
    pub fn fromPos(pos: Pos) Span {
        return .{ .start = pos, .end = pos };
    }

    /// Combine two spans into one covering both ranges.
    pub fn merge(self: Span, other: Span) Span {
        return .{
            .start = if (self.start.offset < other.start.offset) self.start else other.start,
            .end = if (self.end.offset > other.end.offset) self.end else other.end,
        };
    }

    pub fn len(self: Span) u32 {
        return self.end.offset - self.start.offset;
    }
};

/// Source file content with lazy line offset computation.
///
/// Holds the raw text of a source file and provides character access,
/// text extraction, and offset-to-line/column conversion.
///
/// Line offsets are computed on first call to `position()`, `getLine()`,
/// or `lineCount()`, then cached. This means reading tokens (the common
/// case) never pays for line tracking — only error reporting does.
pub const Source = struct {
    filename: []const u8,
    content: []const u8,
    line_offsets: ?[]u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, filename: []const u8, content: []const u8) Source {
        return .{
            .filename = filename,
            .content = content,
            .line_offsets = null,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Source) void {
        if (self.line_offsets) |offsets| self.allocator.free(offsets);
    }

    /// Read the character at a position. Returns null at end of file.
    pub fn at(self: *const Source, pos: Pos) ?u8 {
        return if (pos.offset >= self.content.len) null else self.content[pos.offset];
    }

    /// Extract a substring between two positions (clamped to content bounds).
    pub fn slice(self: *const Source, start: Pos, end: Pos) []const u8 {
        const s = @min(start.offset, @as(u32, @intCast(self.content.len)));
        const e = @min(end.offset, @as(u32, @intCast(self.content.len)));
        return self.content[s..e];
    }

    /// Extract the text covered by a span.
    pub fn spanText(self: *const Source, span: Span) []const u8 {
        return self.slice(span.start, span.end);
    }

    /// Convert a byte offset to a human-readable line:column position.
    /// Lazily computes line offsets on first call.
    pub fn position(self: *Source, pos: Pos) Position {
        self.ensureLineOffsets();
        const offsets = self.line_offsets.?;
        const offset = pos.offset;

        // Binary search for the line containing this offset
        var line: u32 = 0;
        var lo: usize = 0;
        var hi: usize = offsets.len;
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            if (offsets[mid] <= offset) {
                line = @intCast(mid);
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }

        return .{
            .filename = self.filename,
            .offset = offset,
            .line = line + 1,
            .column = offset - offsets[line] + 1,
        };
    }

    /// Get the full text of the line containing a position.
    pub fn getLine(self: *Source, pos: Pos) []const u8 {
        self.ensureLineOffsets();
        const loc = self.position(pos);
        const start = self.line_offsets.?[loc.line - 1];
        var end = start;
        while (end < self.content.len and self.content[end] != '\n') end += 1;
        return self.content[start..end];
    }

    pub fn lineCount(self: *Source) usize {
        self.ensureLineOffsets();
        return self.line_offsets.?.len;
    }

    /// Scan source text for newlines and build the line offset table.
    /// Called lazily — most compilations never need this (only on error).
    fn ensureLineOffsets(self: *Source) void {
        if (self.line_offsets != null) return;

        var count: usize = 1;
        for (self.content) |c| {
            if (c == '\n') count += 1;
        }

        const offsets = self.allocator.alloc(u32, count) catch return;
        offsets[0] = 0;
        var idx: usize = 1;
        for (self.content, 0..) |c, i| {
            if (c == '\n') {
                offsets[idx] = @intCast(i + 1);
                idx += 1;
            }
        }
        self.line_offsets = offsets;
    }
};


test "position advance" {
    const pos = Pos{ .offset = 5 };
    try std.testing.expectEqual(@as(u32, 8), pos.advance(3).offset);
}

test "span merge" {
    const a = Span.init(Pos{ .offset = 5 }, Pos{ .offset = 10 });
    const b = Span.init(Pos{ .offset = 8 }, Pos{ .offset = 15 });
    const merged = a.merge(b);
    try std.testing.expectEqual(@as(u32, 5), merged.start.offset);
    try std.testing.expectEqual(@as(u32, 15), merged.end.offset);
}

test "span length" {
    const span = Span.init(Pos{ .offset = 5 }, Pos{ .offset = 10 });
    try std.testing.expectEqual(@as(u32, 5), span.len());
}

test "line and column" {
    const content = "fn main() {\n    return 0\n}";
    var source = Source.init(std.testing.allocator, "test.cot", content);
    defer source.deinit();

    const pos0 = source.position(Pos{ .offset = 0 });
    try std.testing.expectEqual(@as(u32, 1), pos0.line);
    try std.testing.expectEqual(@as(u32, 1), pos0.column);

    const pos16 = source.position(Pos{ .offset = 16 });
    try std.testing.expectEqual(@as(u32, 2), pos16.line);
    try std.testing.expectEqual(@as(u32, 5), pos16.column);
}

test "span text extraction" {
    const content = "hello world";
    var source = Source.init(std.testing.allocator, "test.cot", content);
    defer source.deinit();
    try std.testing.expectEqualStrings("hello", source.spanText(
        Span.init(Pos{ .offset = 0 }, Pos{ .offset = 5 }),
    ));
}

test "get line" {
    const content = "line one\nline two\nline three";
    var source = Source.init(std.testing.allocator, "test.cot", content);
    defer source.deinit();
    try std.testing.expectEqualStrings("line one", source.getLine(Pos{ .offset = 0 }));
    try std.testing.expectEqualStrings("line two", source.getLine(Pos{ .offset = 10 }));
}

test "character access" {
    const content = "abc";
    var source = Source.init(std.testing.allocator, "test.cot", content);
    defer source.deinit();
    try std.testing.expectEqual(@as(?u8, 'a'), source.at(Pos{ .offset = 0 }));
    try std.testing.expectEqual(@as(?u8, null), source.at(Pos{ .offset = 3 }));
}

test "line count" {
    const content = "line 1\nline 2\nline 3";
    var source = Source.init(std.testing.allocator, "test.cot", content);
    defer source.deinit();
    try std.testing.expectEqual(@as(usize, 3), source.lineCount());
}

test "position formatting" {
    const pos = Position{ .filename = "test.cot", .offset = 10, .line = 2, .column = 5 };
    var buf: [64]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try pos.format("", .{}, stream.writer());
    try std.testing.expectEqualStrings("test.cot:2:5", stream.getWritten());
}
