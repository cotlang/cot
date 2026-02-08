//! LSP protocol types and position conversion helpers.

const std = @import("std");
const source = @import("../frontend/source.zig");

const Pos = source.Pos;
const Span = source.Span;
const Source = source.Source;

/// LSP Position (0-based line and character).
pub const Position = struct {
    line: u32,
    character: u32,
};

/// LSP Range.
pub const Range = struct {
    start: Position,
    end: Position,
};

/// LSP Location (URI + range).
pub const Location = struct {
    uri: []const u8,
    range: Range,
};

/// LSP DiagnosticSeverity.
pub const DiagnosticSeverity = enum(u8) {
    @"error" = 1,
    warning = 2,
    information = 3,
    hint = 4,
};

/// LSP Diagnostic.
pub const Diagnostic = struct {
    range: Range,
    severity: DiagnosticSeverity,
    code: ?[]const u8,
    message: []const u8,
};

/// LSP MarkupContent.
pub const MarkupContent = struct {
    kind: []const u8, // "plaintext" or "markdown"
    value: []const u8,
};

/// LSP SymbolKind (subset of spec values used by Cot).
pub const SymbolKind = enum(u8) {
    file = 1,
    module = 2,
    namespace = 3,
    class = 5,
    method = 6,
    property = 7,
    field = 8,
    constructor = 9,
    @"enum" = 10,
    interface = 11,
    function = 12,
    variable = 13,
    constant = 14,
    @"struct" = 23,
    event = 24,
    type_parameter = 26,

    pub fn value(self: SymbolKind) u8 {
        return @intFromEnum(self);
    }
};

/// LSP DocumentSymbol.
pub const DocumentSymbol = struct {
    name: []const u8,
    kind: SymbolKind,
    range: Range,
    selection_range: Range,
    children: []const DocumentSymbol,
};

// ============================================================================
// Position conversion helpers
// ============================================================================

/// Convert Cot source Pos (byte offset) to LSP Position (0-based line/character).
/// Walks the source content counting newlines.
pub fn cotPosToLsp(content: []const u8, pos: Pos) Position {
    var line: u32 = 0;
    var col: u32 = 0;
    const offset = @min(pos.offset, @as(u32, @intCast(content.len)));
    for (content[0..offset]) |c| {
        if (c == '\n') {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    return .{ .line = line, .character = col };
}

/// Convert Cot Span to LSP Range.
pub fn spanToRange(content: []const u8, span: Span) Range {
    return .{
        .start = cotPosToLsp(content, span.start),
        .end = cotPosToLsp(content, span.end),
    };
}

/// Convert LSP Position (0-based line/character) to byte offset in source content.
pub fn lspToByteOffset(content: []const u8, pos: Position) u32 {
    var line: u32 = 0;
    for (content, 0..) |c, i| {
        if (line == pos.line) {
            // We're on the target line; advance by character count
            var col: u32 = 0;
            var j = i;
            while (j < content.len and col < pos.character) {
                if (content[j] == '\n') break;
                col += 1;
                j += 1;
            }
            return @intCast(j);
        }
        if (c == '\n') line += 1;
    }
    return @intCast(content.len);
}

/// Escape a string for JSON embedding.
pub fn jsonEscape(allocator: std.mem.Allocator, str: []const u8) ![]u8 {
    var result = std.ArrayListUnmanaged(u8){};
    for (str) |c| {
        switch (c) {
            '"' => try result.appendSlice(allocator, "\\\""),
            '\\' => try result.appendSlice(allocator, "\\\\"),
            '\n' => try result.appendSlice(allocator, "\\n"),
            '\r' => try result.appendSlice(allocator, "\\r"),
            '\t' => try result.appendSlice(allocator, "\\t"),
            else => {
                if (c < 0x20) {
                    // Control character — emit \u00XX
                    var buf: [6]u8 = undefined;
                    _ = std.fmt.bufPrint(&buf, "\\u{x:0>4}", .{c}) catch unreachable;
                    try result.appendSlice(allocator, &buf);
                } else {
                    try result.append(allocator, c);
                }
            },
        }
    }
    return result.toOwnedSlice(allocator);
}

// ============================================================================
// Tests
// ============================================================================

test "cotPosToLsp: first char" {
    const content = "fn main() {}";
    const pos = cotPosToLsp(content, Pos{ .offset = 0 });
    try std.testing.expectEqual(@as(u32, 0), pos.line);
    try std.testing.expectEqual(@as(u32, 0), pos.character);
}

test "cotPosToLsp: second line" {
    const content = "fn main() {\n    return 0\n}";
    const pos = cotPosToLsp(content, Pos{ .offset = 16 });
    try std.testing.expectEqual(@as(u32, 1), pos.line);
    try std.testing.expectEqual(@as(u32, 4), pos.character);
}

test "spanToRange" {
    const content = "fn main() {\n    return 0\n}";
    const span = Span.init(Pos{ .offset = 0 }, Pos{ .offset = 2 });
    const range = spanToRange(content, span);
    try std.testing.expectEqual(@as(u32, 0), range.start.line);
    try std.testing.expectEqual(@as(u32, 0), range.start.character);
    try std.testing.expectEqual(@as(u32, 0), range.end.line);
    try std.testing.expectEqual(@as(u32, 2), range.end.character);
}

test "lspToByteOffset: start of line 2" {
    const content = "fn main() {\n    return 0\n}";
    const offset = lspToByteOffset(content, .{ .line = 1, .character = 4 });
    try std.testing.expectEqual(@as(u32, 16), offset);
}

test "jsonEscape: basic" {
    const allocator = std.testing.allocator;
    const result = try jsonEscape(allocator, "hello \"world\"\nnewline");
    defer allocator.free(result);
    try std.testing.expectEqualStrings("hello \\\"world\\\"\\nnewline", result);
}
