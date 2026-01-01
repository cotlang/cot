//! AST Typed Indices
//!
//! Provides type-safe indices into NodeStore arrays.
//! Using typed enums prevents accidentally mixing statement and expression indices.

const std = @import("std");

/// Index into statement storage
pub const StmtIdx = enum(u32) {
    /// Sentinel value for "no statement"
    null = std.math.maxInt(u32),
    _,

    pub fn isNull(self: StmtIdx) bool {
        return self == .null;
    }

    pub fn toInt(self: StmtIdx) u32 {
        return @intFromEnum(self);
    }

    pub fn fromInt(val: u32) StmtIdx {
        return @enumFromInt(val);
    }
};

/// Index into expression storage
pub const ExprIdx = enum(u32) {
    /// Sentinel value for "no expression"
    null = std.math.maxInt(u32),
    _,

    pub fn isNull(self: ExprIdx) bool {
        return self == .null;
    }

    pub fn toInt(self: ExprIdx) u32 {
        return @intFromEnum(self);
    }

    pub fn fromInt(val: u32) ExprIdx {
        return @enumFromInt(val);
    }
};

/// Index into type storage
pub const TypeIdx = enum(u32) {
    /// Sentinel value for "no type" or "inferred"
    null = std.math.maxInt(u32),
    _,

    pub fn isNull(self: TypeIdx) bool {
        return self == .null;
    }

    pub fn toInt(self: TypeIdx) u32 {
        return @intFromEnum(self);
    }

    pub fn fromInt(val: u32) TypeIdx {
        return @enumFromInt(val);
    }
};

/// Index into extra_data array
pub const ExtraIdx = enum(u32) {
    _,

    pub fn toInt(self: ExtraIdx) u32 {
        return @intFromEnum(self);
    }

    pub fn fromInt(val: u32) ExtraIdx {
        return @enumFromInt(val);
    }
};

/// Span of indices in extra_data array
/// Used for variable-length data like parameter lists, statement blocks, etc.
pub const ExtraSpan = struct {
    start: ExtraIdx,
    len: u32,

    pub const empty: ExtraSpan = .{
        .start = @enumFromInt(0),
        .len = 0,
    };

    pub fn isEmpty(self: ExtraSpan) bool {
        return self.len == 0;
    }
};

// ============================================================
// Tests
// ============================================================

test "StmtIdx null check" {
    const null_idx = StmtIdx.null;
    try std.testing.expect(null_idx.isNull());

    const valid_idx = StmtIdx.fromInt(5);
    try std.testing.expect(!valid_idx.isNull());
}

test "ExprIdx null check" {
    const null_idx = ExprIdx.null;
    try std.testing.expect(null_idx.isNull());

    const valid_idx = ExprIdx.fromInt(10);
    try std.testing.expect(!valid_idx.isNull());
    try std.testing.expectEqual(@as(u32, 10), valid_idx.toInt());
}

test "TypeIdx null check" {
    const null_idx = TypeIdx.null;
    try std.testing.expect(null_idx.isNull());
}

test "ExtraSpan empty" {
    const empty = ExtraSpan.empty;
    try std.testing.expect(empty.isEmpty());

    const non_empty = ExtraSpan{ .start = @enumFromInt(5), .len = 3 };
    try std.testing.expect(!non_empty.isEmpty());
}
