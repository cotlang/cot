//! Common types for the Cranelift code generator.
//!
//! Port of cranelift/codegen/src/ir/types.rs
//! Port of cranelift/codegen/shared/src/constants.rs

const std = @import("std");

// ============================================================================
// Constants from cranelift-codegen-shared/constants.rs
// ============================================================================

// Numbering scheme for value types:
//
// 0: Void/Invalid
// 0x01-0x6f: Special types
// 0x70-0x7d: Lane types
// 0x7e-0x7f: Reference types
// 0x80-0xff: Vector types
// 0x100-0x17f: Dynamic Vector types
//
// Vector types are encoded with the lane type in the low 4 bits and log2(lanes)
// in the next highest 4 bits, giving a range of 2-256 lanes.

/// Start of the lane types.
pub const LANE_BASE: u16 = 0x70;

/// Base for reference types.
pub const REFERENCE_BASE: u16 = 0x7E;

/// Start of the 2-lane vector types.
pub const VECTOR_BASE: u16 = 0x80;

/// Start of the dynamic vector types.
pub const DYNAMIC_VECTOR_BASE: u16 = 0x100;

// ============================================================================
// Type
// ============================================================================

/// The type of an SSA value.
///
/// The `INVALID` type isn't a real type, and is used as a placeholder in the IR where a type
/// field is present but no type is needed, such as the controlling type variable for a
/// non-polymorphic instruction.
///
/// Basic integer types: `I8`, `I16`, `I32`, `I64`, and `I128`. These types are sign-agnostic.
///
/// Basic floating point types: `F16`, `F32`, `F64`, and `F128`. IEEE half, single, double, and quadruple precision.
///
/// SIMD vector types have power-of-two lanes, up to 256. Lanes can be any int/float type.
pub const Type = struct {
    repr: u16,

    const Self = @This();

    // ========================================================================
    // Scalar Type Constants
    // ========================================================================

    /// Not a valid type. Can't be loaded or stored. Can't be part of a SIMD vector.
    pub const INVALID: Type = .{ .repr = 0 };

    /// An integer type with 8 bits.
    pub const I8: Type = .{ .repr = LANE_BASE + 4 };

    /// An integer type with 16 bits.
    pub const I16: Type = .{ .repr = LANE_BASE + 5 };

    /// An integer type with 32 bits.
    pub const I32: Type = .{ .repr = LANE_BASE + 6 };

    /// An integer type with 64 bits.
    pub const I64: Type = .{ .repr = LANE_BASE + 7 };

    /// An integer type with 128 bits.
    pub const I128: Type = .{ .repr = LANE_BASE + 8 };

    /// A 16-bit floating point type (IEEE 754-2008 binary16).
    pub const F16: Type = .{ .repr = LANE_BASE + 9 };

    /// A 32-bit floating point type (IEEE 754-2008 binary32).
    pub const F32: Type = .{ .repr = LANE_BASE + 10 };

    /// A 64-bit floating point type (IEEE 754-2008 binary64).
    pub const F64: Type = .{ .repr = LANE_BASE + 11 };

    /// A 128-bit floating point type (IEEE 754-2008 binary128).
    pub const F128: Type = .{ .repr = LANE_BASE + 12 };

    // ========================================================================
    // Common Vector Type Constants
    // ========================================================================
    // Encoding: lane.repr + (log2(lanes) << 4)
    // E.g., I32X4 = I32.repr + (2 << 4) = 0x76 + 0x20 = 0x96

    /// 8x8-bit integer vector (64-bit total).
    pub const I8X8: Type = .{ .repr = I8.repr + (3 << 4) };

    /// 4x16-bit integer vector (64-bit total).
    pub const I16X4: Type = .{ .repr = I16.repr + (2 << 4) };

    /// 2x32-bit integer vector (64-bit total).
    pub const I32X2: Type = .{ .repr = I32.repr + (1 << 4) };

    /// 16x8-bit integer vector (128-bit total).
    pub const I8X16: Type = .{ .repr = I8.repr + (4 << 4) };

    /// 8x16-bit integer vector (128-bit total).
    pub const I16X8: Type = .{ .repr = I16.repr + (3 << 4) };

    /// 4x32-bit integer vector (128-bit total).
    pub const I32X4: Type = .{ .repr = I32.repr + (2 << 4) };

    /// 2x64-bit integer vector (128-bit total).
    pub const I64X2: Type = .{ .repr = I64.repr + (1 << 4) };

    /// 4x32-bit float vector (128-bit total).
    pub const F32X4: Type = .{ .repr = F32.repr + (2 << 4) };

    /// 2x64-bit float vector (128-bit total).
    pub const F64X2: Type = .{ .repr = F64.repr + (1 << 4) };

    /// 32x8-bit integer vector (256-bit total).
    pub const I8X32: Type = .{ .repr = I8.repr + (5 << 4) };

    /// 16x16-bit integer vector (256-bit total).
    pub const I16X16: Type = .{ .repr = I16.repr + (4 << 4) };

    /// 8x32-bit integer vector (256-bit total).
    pub const I32X8: Type = .{ .repr = I32.repr + (3 << 4) };

    /// 4x64-bit integer vector (256-bit total).
    pub const I64X4: Type = .{ .repr = I64.repr + (2 << 4) };

    /// 8x32-bit float vector (256-bit total).
    pub const F32X8: Type = .{ .repr = F32.repr + (3 << 4) };

    /// 4x64-bit float vector (256-bit total).
    pub const F64X4: Type = .{ .repr = F64.repr + (2 << 4) };

    /// 8x64-bit float vector (512-bit total).
    pub const F64X8: Type = .{ .repr = F64.repr + (3 << 4) };

    // ========================================================================
    // Methods
    // ========================================================================

    /// Create a Type from a raw representation.
    pub fn fromRepr(repr: u16) Type {
        return .{ .repr = repr };
    }

    /// Check equality.
    pub fn eql(self: Self, other: Self) bool {
        return self.repr == other.repr;
    }

    /// Get the lane type of this SIMD vector type.
    ///
    /// A lane type is the same as a SIMD vector type with one lane, so it returns itself.
    pub fn laneType(self: Self) Self {
        if (self.repr < VECTOR_BASE) {
            return self;
        } else {
            return .{ .repr = LANE_BASE | (self.repr & 0x0f) };
        }
    }

    /// The type transformation that returns the lane type of a type variable.
    pub fn laneOf(self: Self) Self {
        return self.laneType();
    }

    /// Get log_2 of the number of bits in a lane.
    pub fn log2LaneBits(self: Self) u32 {
        const lane = self.laneType();
        if (lane.eql(I8)) return 3;
        if (lane.eql(I16) or lane.eql(F16)) return 4;
        if (lane.eql(I32) or lane.eql(F32)) return 5;
        if (lane.eql(I64) or lane.eql(F64)) return 6;
        if (lane.eql(I128) or lane.eql(F128)) return 7;
        return 0;
    }

    /// Get the number of bits in a lane.
    pub fn laneBits(self: Self) u32 {
        const lane = self.laneType();
        if (lane.eql(I8)) return 8;
        if (lane.eql(I16) or lane.eql(F16)) return 16;
        if (lane.eql(I32) or lane.eql(F32)) return 32;
        if (lane.eql(I64) or lane.eql(F64)) return 64;
        if (lane.eql(I128) or lane.eql(F128)) return 128;
        return 0;
    }

    /// Get an integer type with the requested number of bits.
    pub fn int(num_bits: u16) ?Self {
        return switch (num_bits) {
            8 => I8,
            16 => I16,
            32 => I32,
            64 => I64,
            128 => I128,
            else => null,
        };
    }

    /// Get an integer type with the requested number of bytes.
    pub fn intWithByteSize(num_bytes: u16) ?Self {
        const num_bits = @as(u32, num_bytes) * 8;
        if (num_bits > 128) return null;
        return int(@intCast(num_bits));
    }

    /// Get a type with the same number of lanes as `self`, but using `lane` as the lane type.
    fn replaceLanes(self: Self, lane: Self) Self {
        return .{ .repr = (lane.repr & 0x0f) | (self.repr & 0xf0) };
    }

    /// Get a type with the same number of lanes as this type, but with lanes that are half the
    /// number of bits.
    pub fn halfWidth(self: Self) ?Self {
        const lane = self.laneType();
        const half_lane: ?Type = blk: {
            if (lane.eql(I16)) break :blk I8;
            if (lane.eql(I32)) break :blk I16;
            if (lane.eql(I64)) break :blk I32;
            if (lane.eql(I128)) break :blk I64;
            if (lane.eql(F32)) break :blk F16;
            if (lane.eql(F64)) break :blk F32;
            if (lane.eql(F128)) break :blk F64;
            break :blk null;
        };
        if (half_lane) |hl| {
            return self.replaceLanes(hl);
        }
        return null;
    }

    /// Get a type with the same number of lanes as this type, but with lanes that are twice the
    /// number of bits.
    pub fn doubleWidth(self: Self) ?Self {
        const lane = self.laneType();
        const double_lane: ?Type = blk: {
            if (lane.eql(I8)) break :blk I16;
            if (lane.eql(I16)) break :blk I32;
            if (lane.eql(I32)) break :blk I64;
            if (lane.eql(I64)) break :blk I128;
            if (lane.eql(F16)) break :blk F32;
            if (lane.eql(F32)) break :blk F64;
            if (lane.eql(F64)) break :blk F128;
            break :blk null;
        };
        if (double_lane) |dl| {
            return self.replaceLanes(dl);
        }
        return null;
    }

    /// Is this the INVALID type?
    pub fn isInvalid(self: Self) bool {
        return self.eql(INVALID);
    }

    /// Is this a special type?
    pub fn isSpecial(self: Self) bool {
        return self.repr < LANE_BASE;
    }

    /// Is this a lane type?
    ///
    /// This is a scalar type that can also appear as the lane type of a SIMD vector.
    pub fn isLane(self: Self) bool {
        return LANE_BASE <= self.repr and self.repr < VECTOR_BASE;
    }

    /// Is this a SIMD vector type?
    ///
    /// A vector type has 2 or more lanes.
    pub fn isVector(self: Self) bool {
        return self.repr >= VECTOR_BASE and !self.isDynamicVector();
    }

    /// Is this a SIMD vector type with a runtime number of lanes?
    pub fn isDynamicVector(self: Self) bool {
        return self.repr >= DYNAMIC_VECTOR_BASE;
    }

    /// Is this a scalar integer type?
    pub fn isInt(self: Self) bool {
        return self.eql(I8) or self.eql(I16) or self.eql(I32) or self.eql(I64) or self.eql(I128);
    }

    /// Is this a scalar floating point type?
    pub fn isFloat(self: Self) bool {
        return self.eql(F16) or self.eql(F32) or self.eql(F64) or self.eql(F128);
    }

    /// Get the register class for this type.
    /// Used for register allocation.
    pub fn regClass(self: Self) @import("../../codegen/native/machinst/reg.zig").RegClass {
        if (self.isVector()) return .vector;
        if (self.isFloat()) return .float;
        return .int;
    }

    /// Get log_2 of the number of lanes in this SIMD vector type.
    ///
    /// All SIMD types have a lane count that is a power of two and no larger than 256, so this
    /// will be a number in the range 0-8.
    ///
    /// A scalar type is the same as a SIMD vector type with one lane, so it returns 0.
    pub fn log2LaneCount(self: Self) u32 {
        if (self.isDynamicVector()) {
            return 0;
        } else {
            const adjusted = if (self.repr >= LANE_BASE) self.repr - LANE_BASE else 0;
            return @intCast(adjusted >> 4);
        }
    }

    /// Get the number of lanes in this SIMD vector type.
    ///
    /// A scalar type is the same as a SIMD vector type with one lane, so it returns 1.
    pub fn laneCount(self: Self) u32 {
        if (self.isDynamicVector()) {
            return 0;
        } else {
            return @as(u32, 1) << @intCast(self.log2LaneCount());
        }
    }

    /// Get the total number of bits used to represent this type.
    pub fn bits(self: Self) u32 {
        if (self.isDynamicVector()) {
            return 0;
        } else {
            return self.laneBits() * self.laneCount();
        }
    }

    /// Get the number of bytes used to store this type in memory.
    pub fn bytes(self: Self) u32 {
        return (self.bits() + 7) / 8;
    }

    /// Get a SIMD vector type with `n` times more lanes than this one.
    ///
    /// If this is a scalar type, this produces a SIMD type with this as a lane type and `n` lanes.
    pub fn by(self: Self, n: u32) ?Self {
        if (self.isDynamicVector()) {
            return null;
        }
        if (self.laneBits() == 0 or !std.math.isPowerOfTwo(n)) {
            return null;
        }
        const log2_lanes: u32 = @ctz(n);
        const new_type = @as(u32, self.repr) + (log2_lanes << 4);
        if (new_type < DYNAMIC_VECTOR_BASE) {
            return .{ .repr = @intCast(new_type) };
        } else {
            return null;
        }
    }

    /// Get the type of a comparison result for the given type. For vectors this will be a vector
    /// with the same number of lanes and integer elements, and for scalar types this will be `i8`.
    pub fn asTruthy(self: Self) Self {
        if (!self.isVector()) {
            return I8;
        } else {
            return self.asTruthyPedantic();
        }
    }

    /// Get a type with the same number of lanes as this type, but with the lanes replaced by
    /// integers of the same size.
    pub fn asTruthyPedantic(self: Self) Self {
        const lane = self.laneType();
        const int_lane: Type = blk: {
            if (lane.eql(I8)) break :blk I8;
            if (lane.eql(I16) or lane.eql(F16)) break :blk I16;
            if (lane.eql(I32) or lane.eql(F32)) break :blk I32;
            if (lane.eql(I64) or lane.eql(F64)) break :blk I64;
            if (lane.eql(I128) or lane.eql(F128)) break :blk I128;
            break :blk I8;
        };
        return self.replaceLanes(int_lane);
    }

    /// Get a type with the same number of lanes as this type, but with the lanes replaced by
    /// integers of the same size.
    pub fn asInt(self: Self) Self {
        const lane = self.laneType();
        const int_lane: Type = blk: {
            if (lane.eql(I8)) break :blk I8;
            if (lane.eql(I16) or lane.eql(F16)) break :blk I16;
            if (lane.eql(I32) or lane.eql(F32)) break :blk I32;
            if (lane.eql(I64) or lane.eql(F64)) break :blk I64;
            if (lane.eql(I128) or lane.eql(F128)) break :blk I128;
            break :blk I8;
        };
        return self.replaceLanes(int_lane);
    }

    /// True iff: self.lane_count() == other.lane_count() and self.lane_bits() >= other.lane_bits()
    pub fn widerOrEqual(self: Self, other: Self) bool {
        return self.laneCount() == other.laneCount() and self.laneBits() >= other.laneBits();
    }

    /// Index of this type, for use with hash tables etc.
    pub fn index(self: Self) usize {
        return @intCast(self.repr);
    }

    /// Format the type for display.
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.isInt()) {
            try writer.print("i{d}", .{self.laneBits()});
        } else if (self.isFloat()) {
            try writer.print("f{d}", .{self.laneBits()});
        } else if (self.isVector()) {
            const lane = self.laneType();
            if (lane.isInt()) {
                try writer.print("i{d}x{d}", .{ lane.laneBits(), self.laneCount() });
            } else if (lane.isFloat()) {
                try writer.print("f{d}x{d}", .{ lane.laneBits(), self.laneCount() });
            }
        } else if (self.isInvalid()) {
            try writer.writeAll("invalid");
        } else {
            try writer.print("type(0x{x})", .{self.repr});
        }
    }
};

// ============================================================================
// Tests - Ported from Cranelift types.rs
// ============================================================================

test "basic scalars" {
    const testing = std.testing;

    try testing.expect(Type.INVALID.eql(Type.INVALID.laneType()));
    try testing.expectEqual(@as(u32, 0), Type.INVALID.bits());
    try testing.expect(Type.I8.eql(Type.I8.laneType()));
    try testing.expect(Type.I16.eql(Type.I16.laneType()));
    try testing.expect(Type.I32.eql(Type.I32.laneType()));
    try testing.expect(Type.I64.eql(Type.I64.laneType()));
    try testing.expect(Type.I128.eql(Type.I128.laneType()));
    try testing.expect(Type.F32.eql(Type.F32.laneType()));
    try testing.expect(Type.F16.eql(Type.F16.laneType()));
    try testing.expect(Type.F64.eql(Type.F64.laneType()));
    try testing.expect(Type.F128.eql(Type.F128.laneType()));
    try testing.expect(Type.I32.eql(Type.I32X4.laneType()));
    try testing.expect(Type.F64.eql(Type.F64X2.laneType()));

    try testing.expectEqual(@as(u32, 0), Type.INVALID.laneBits());
    try testing.expectEqual(@as(u32, 8), Type.I8.laneBits());
    try testing.expectEqual(@as(u32, 16), Type.I16.laneBits());
    try testing.expectEqual(@as(u32, 32), Type.I32.laneBits());
    try testing.expectEqual(@as(u32, 64), Type.I64.laneBits());
    try testing.expectEqual(@as(u32, 128), Type.I128.laneBits());
    try testing.expectEqual(@as(u32, 16), Type.F16.laneBits());
    try testing.expectEqual(@as(u32, 32), Type.F32.laneBits());
    try testing.expectEqual(@as(u32, 64), Type.F64.laneBits());
    try testing.expectEqual(@as(u32, 128), Type.F128.laneBits());
}

test "typevar functions" {
    const testing = std.testing;

    try testing.expect(Type.INVALID.halfWidth() == null);
    try testing.expect(Type.I8.halfWidth() == null);
    try testing.expect(Type.I16.halfWidth().?.eql(Type.I8));
    try testing.expect(Type.I32.halfWidth().?.eql(Type.I16));
    try testing.expect(Type.I32X4.halfWidth().?.eql(Type.I16X4));
    try testing.expect(Type.I64.halfWidth().?.eql(Type.I32));
    try testing.expect(Type.I128.halfWidth().?.eql(Type.I64));
    try testing.expect(Type.F16.halfWidth() == null);
    try testing.expect(Type.F32.halfWidth().?.eql(Type.F16));
    try testing.expect(Type.F64.halfWidth().?.eql(Type.F32));
    try testing.expect(Type.F128.halfWidth().?.eql(Type.F64));

    try testing.expect(Type.INVALID.doubleWidth() == null);
    try testing.expect(Type.I8.doubleWidth().?.eql(Type.I16));
    try testing.expect(Type.I16.doubleWidth().?.eql(Type.I32));
    try testing.expect(Type.I32.doubleWidth().?.eql(Type.I64));
    try testing.expect(Type.I32X4.doubleWidth().?.eql(Type.I64X4));
    try testing.expect(Type.I64.doubleWidth().?.eql(Type.I128));
    try testing.expect(Type.I128.doubleWidth() == null);
    try testing.expect(Type.F16.doubleWidth().?.eql(Type.F32));
    try testing.expect(Type.F32.doubleWidth().?.eql(Type.F64));
    try testing.expect(Type.F64.doubleWidth().?.eql(Type.F128));
    try testing.expect(Type.F128.doubleWidth() == null);
}

test "vectors" {
    const testing = std.testing;

    const big = Type.F64.by(256).?;
    try testing.expectEqual(@as(u32, 64), big.laneBits());
    try testing.expectEqual(@as(u32, 256), big.laneCount());
    try testing.expectEqual(@as(u32, 64 * 256), big.bits());

    // Check that the constants match the computed vector types.
    try testing.expect(Type.I32.by(4).?.eql(Type.I32X4));
    try testing.expect(Type.F64.by(8).?.eql(Type.F64X8));
}

test "int from size" {
    const testing = std.testing;

    try testing.expect(Type.int(0) == null);
    try testing.expect(Type.int(8).?.eql(Type.I8));
    try testing.expect(Type.int(33) == null);
    try testing.expect(Type.int(64).?.eql(Type.I64));

    try testing.expect(Type.intWithByteSize(0) == null);
    try testing.expect(Type.intWithByteSize(2).?.eql(Type.I16));
    try testing.expect(Type.intWithByteSize(6) == null);
    try testing.expect(Type.intWithByteSize(16).?.eql(Type.I128));
}

test "type queries" {
    const testing = std.testing;

    try testing.expect(Type.I32.isInt());
    try testing.expect(!Type.I32.isFloat());
    try testing.expect(Type.I32.isLane());
    try testing.expect(!Type.I32.isVector());

    try testing.expect(!Type.F64.isInt());
    try testing.expect(Type.F64.isFloat());
    try testing.expect(Type.F64.isLane());
    try testing.expect(!Type.F64.isVector());

    try testing.expect(!Type.I32X4.isInt());
    try testing.expect(!Type.I32X4.isFloat());
    try testing.expect(!Type.I32X4.isLane());
    try testing.expect(Type.I32X4.isVector());
}

test "as_truthy" {
    const testing = std.testing;

    try testing.expect(Type.I32X4.asTruthy().eql(Type.I32X4));
    try testing.expect(Type.I32.asTruthy().eql(Type.I8));
    try testing.expect(Type.I32X4.asTruthyPedantic().eql(Type.I32X4));
    try testing.expect(Type.I32.asTruthyPedantic().eql(Type.I32));
}

test "format scalars" {
    const testing = std.testing;
    var buf: [32]u8 = undefined;

    {
        var fbs = std.io.fixedBufferStream(&buf);
        try Type.I8.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("i8", fbs.getWritten());
    }
    {
        var fbs = std.io.fixedBufferStream(&buf);
        try Type.I32.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("i32", fbs.getWritten());
    }
    {
        var fbs = std.io.fixedBufferStream(&buf);
        try Type.F64.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("f64", fbs.getWritten());
    }
}

test "format vectors" {
    const testing = std.testing;
    var buf: [32]u8 = undefined;

    {
        var fbs = std.io.fixedBufferStream(&buf);
        try Type.I32X4.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("i32x4", fbs.getWritten());
    }
    {
        var fbs = std.io.fixedBufferStream(&buf);
        try Type.F64X2.format("", .{}, fbs.writer());
        try testing.expectEqualStrings("f64x2", fbs.getWritten());
    }
}
