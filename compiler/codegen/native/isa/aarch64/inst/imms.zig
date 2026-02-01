//! AArch64 ISA definitions: immediate constants.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/inst/imms.rs`

const std = @import("std");
pub const args = @import("args.zig");
pub const OperandSize = args.OperandSize;
pub const ScalarSize = args.ScalarSize;
pub const Type = args.Type;

/// An immediate that represents the NZCV flags.
pub const NZCV = struct {
    /// The negative condition flag.
    n: bool,
    /// The zero condition flag.
    z: bool,
    /// The carry condition flag.
    c: bool,
    /// The overflow condition flag.
    v: bool,

    /// Create a new NZCV flags representation.
    pub fn init(n: bool, z: bool, c: bool, v: bool) NZCV {
        return .{ .n = n, .z = z, .c = c, .v = v };
    }

    /// Bits for encoding.
    pub fn bits(self: NZCV) u32 {
        return (@as(u32, @intFromBool(self.n)) << 3) |
            (@as(u32, @intFromBool(self.z)) << 2) |
            (@as(u32, @intFromBool(self.c)) << 1) |
            @as(u32, @intFromBool(self.v));
    }

    pub fn prettyPrint(self: NZCV, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// An unsigned 5-bit immediate.
pub const UImm5 = struct {
    /// The value.
    value: u8,

    /// Create an unsigned 5-bit immediate from u8.
    pub fn maybeFromU8(value: u8) ?UImm5 {
        if (value < 32) {
            return UImm5{ .value = value };
        }
        return null;
    }

    /// Bits for encoding.
    pub fn bits(self: UImm5) u32 {
        return @as(u32, self.value);
    }

    pub fn prettyPrint(self: UImm5, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// A signed, scaled 7-bit offset.
pub const SImm7Scaled = struct {
    /// The value.
    value: i16,
    /// Multiplied by the size of this type.
    scale_ty: Type,

    /// Create a SImm7Scaled from a raw offset and the known scale type, if possible.
    pub fn maybeFromI64(value: i64, scale_ty: Type) ?SImm7Scaled {
        std.debug.assert(scale_ty.kind == .int64 or scale_ty.kind == .int32 or
            scale_ty.kind == .float64 or scale_ty.kind == .vec_i8x16);
        const scale = scale_ty.bytes();
        std.debug.assert(std.math.isPowerOfTwo(scale));
        const scale_i64: i64 = @intCast(scale);
        const upper_limit = 63 * scale_i64;
        const lower_limit = -(64 * scale_i64);
        if (value >= lower_limit and value <= upper_limit and @mod(value, scale_i64) == 0) {
            return SImm7Scaled{
                .value = @intCast(value),
                .scale_ty = scale_ty,
            };
        }
        return null;
    }

    /// Bits for encoding.
    pub fn bits(self: SImm7Scaled) u32 {
        const ty_bytes: i16 = @intCast(self.scale_ty.bytes());
        const scaled: i16 = @divExact(self.value, ty_bytes);
        std.debug.assert(scaled <= 63 and scaled >= -64);
        const scaled_i8: i8 = @intCast(scaled);
        const encoded: u32 = @bitCast(@as(i32, scaled_i8));
        return encoded & 0x7f;
    }

    pub fn prettyPrint(self: SImm7Scaled, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// Floating-point unit immediate left shift.
pub const FPULeftShiftImm = struct {
    /// Shift amount.
    amount: u8,
    /// Lane size in bits.
    lane_size_in_bits: u8,

    /// Create a floating-point unit immediate left shift from u8.
    pub fn maybeFromU8(amount: u8, lane_size_in_bits: u8) ?FPULeftShiftImm {
        std.debug.assert(lane_size_in_bits == 32 or lane_size_in_bits == 64);
        if (amount < lane_size_in_bits) {
            return FPULeftShiftImm{
                .amount = amount,
                .lane_size_in_bits = lane_size_in_bits,
            };
        }
        return null;
    }

    /// Returns the encoding of the immediate.
    pub fn enc(self: FPULeftShiftImm) u32 {
        std.debug.assert(std.math.isPowerOfTwo(self.lane_size_in_bits));
        std.debug.assert(self.lane_size_in_bits > self.amount);
        return @as(u32, self.lane_size_in_bits | self.amount);
    }

    pub fn prettyPrint(self: FPULeftShiftImm, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// Floating-point unit immediate right shift.
pub const FPURightShiftImm = struct {
    /// Shift amount.
    amount: u8,
    /// Lane size in bits.
    lane_size_in_bits: u8,

    /// Create a floating-point unit immediate right shift from u8.
    pub fn maybeFromU8(amount: u8, lane_size_in_bits: u8) ?FPURightShiftImm {
        std.debug.assert(lane_size_in_bits == 32 or lane_size_in_bits == 64);
        if (amount > 0 and amount <= lane_size_in_bits) {
            return FPURightShiftImm{
                .amount = amount,
                .lane_size_in_bits = lane_size_in_bits,
            };
        }
        return null;
    }

    /// Returns encoding of the immediate.
    pub fn enc(self: FPURightShiftImm) u32 {
        std.debug.assert(self.amount != 0);
        return @as(u32, (self.lane_size_in_bits * 2) - self.amount);
    }

    pub fn prettyPrint(self: FPURightShiftImm, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// A 9-bit signed offset.
pub const SImm9 = struct {
    /// The value.
    value: i16,

    /// Create a signed 9-bit offset from a full-range value, if possible.
    pub fn maybeFromI64(value: i64) ?SImm9 {
        if (value >= -256 and value <= 255) {
            return SImm9{ .value = @intCast(value) };
        }
        return null;
    }

    /// Bits for encoding.
    pub fn bits(self: SImm9) u32 {
        return @as(u32, @bitCast(@as(i32, self.value))) & 0x1ff;
    }

    /// Signed value of immediate.
    pub fn getValue(self: SImm9) i32 {
        return self.value;
    }

    pub fn prettyPrint(self: SImm9, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// An unsigned, scaled 12-bit offset.
pub const UImm12Scaled = struct {
    /// The value.
    value: u16,
    /// Multiplied by the size of this type.
    scale_ty: Type,

    /// Create a UImm12Scaled from a raw offset and the known scale type, if possible.
    pub fn maybeFromI64(value: i64, scale_ty: Type) ?UImm12Scaled {
        const scale = scale_ty.bytes();
        std.debug.assert(std.math.isPowerOfTwo(scale));
        const scale_i64: i64 = @intCast(scale);
        const limit = 4095 * scale_i64;
        if (value >= 0 and value <= limit and @mod(value, scale_i64) == 0) {
            return UImm12Scaled{
                .value = @intCast(value),
                .scale_ty = scale_ty,
            };
        }
        return null;
    }

    /// Create a zero immediate of this format.
    pub fn zero(scale_ty: Type) UImm12Scaled {
        return UImm12Scaled{ .value = 0, .scale_ty = scale_ty };
    }

    /// Encoded bits.
    pub fn bits(self: UImm12Scaled) u32 {
        return (@as(u32, self.value) / self.scale_ty.bytes()) & 0xfff;
    }

    /// Value after scaling.
    pub fn getValue(self: UImm12Scaled) u32 {
        return self.value;
    }

    pub fn prettyPrint(self: UImm12Scaled, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// A shifted immediate value in 'imm12' format: supports 12 bits, shifted
/// left by 0 or 12 places.
pub const Imm12 = struct {
    /// The immediate bits.
    bits_val: u16,
    /// Whether the immediate bits are shifted left by 12 or not.
    shift12: bool,

    /// Handy 0-value constant.
    pub const ZERO = Imm12{ .bits_val = 0, .shift12 = false };

    /// Compute an Imm12 from raw bits, if possible.
    pub fn maybeFromU64(val: u64) ?Imm12 {
        if (val & ~@as(u64, 0xfff) == 0) {
            return Imm12{
                .bits_val = @intCast(val),
                .shift12 = false,
            };
        } else if (val & ~(@as(u64, 0xfff) << 12) == 0) {
            return Imm12{
                .bits_val = @intCast(val >> 12),
                .shift12 = true,
            };
        }
        return null;
    }

    /// Bits for 2-bit "shift" field in e.g. AddI.
    pub fn shiftBits(self: Imm12) u32 {
        return if (self.shift12) 0b01 else 0b00;
    }

    /// Bits for 12-bit "imm" field in e.g. AddI.
    pub fn immBits(self: Imm12) u32 {
        return @as(u32, self.bits_val);
    }

    /// Get the actual value that this immediate corresponds to.
    pub fn value(self: Imm12) u32 {
        const base: u32 = self.bits_val;
        return if (self.shift12) base << 12 else base;
    }

    pub fn prettyPrint(self: Imm12, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// An immediate for logical instructions.
pub const ImmLogic = struct {
    /// The actual value.
    val: u64,
    /// `N` flag.
    n: bool,
    /// `R` field: rotate amount.
    r: u8,
    /// `S` field: element size and element bits.
    s: u8,
    /// Was this constructed for a 32-bit or 64-bit instruction?
    size: OperandSize,

    /// Compute an ImmLogic from raw bits, if possible.
    /// This is a port of VIXL's Assembler::IsImmLogical.
    pub fn maybeFromU64(original_value: u64, ty: Type) ?ImmLogic {
        if (ty.kind != .int64 and ty.kind != .int32) {
            return null;
        }
        const operand_size = OperandSize.fromTy(ty);

        var working_value = if (ty.kind == .int32) blk: {
            // To handle 32-bit logical immediates, repeat the input twice.
            const shifted = original_value << 32;
            break :blk shifted | (shifted >> 32);
        } else original_value;

        // Handle inversion
        const inverted = (working_value & 1) == 1;
        if (inverted) {
            working_value = ~working_value;
        }

        if (working_value == 0) {
            return null;
        }

        // Find lowest set bit
        const a = lowestSetBit(working_value);
        std.debug.assert(a != 0);

        const value_plus_a = working_value +% a;
        const b = lowestSetBit(value_plus_a);
        const value_plus_a_minus_b = value_plus_a -% b;
        const c_val = lowestSetBit(value_plus_a_minus_b);

        var d: u32 = undefined;
        var clz_a: u32 = undefined;
        var out_n: u32 = undefined;
        var mask: u64 = undefined;

        if (c_val != 0) {
            clz_a = @clz(a);
            const clz_c = @clz(c_val);
            d = clz_a - clz_c;
            mask = (@as(u64, 1) << @intCast(d)) - 1;
            out_n = 0;
        } else {
            d = 64;
            clz_a = @clz(a);
            out_n = 1;
            mask = std.math.maxInt(u64);
        }

        // If the repeat period d is not a power of two, it can't be encoded.
        if (!std.math.isPowerOfTwo(d)) {
            return null;
        }

        if ((b -% a) & ~mask != 0) {
            return null;
        }

        // Compute the candidate pattern
        const multipliers = [_]u64{
            0x0000000000000001,
            0x0000000100000001,
            0x0001000100010001,
            0x0101010101010101,
            0x1111111111111111,
            0x5555555555555555,
        };
        const mult_idx = @clz(@as(u64, d)) - 57;
        const multiplier = multipliers[mult_idx];
        const candidate = (b -% a) *% multiplier;

        if (working_value != candidate) {
            return null;
        }

        // Count set bits
        const clz_b: u32 = if (b == 0) std.math.maxInt(u32) else @clz(b);
        var s_val = clz_a -% clz_b;
        var r_val: u32 = undefined;

        if (inverted) {
            s_val = d - s_val;
            r_val = (clz_b +% 1) & (d - 1);
        } else {
            r_val = (clz_a + 1) & (d - 1);
        }

        // Encode s field
        s_val = ((d * 2) -% (s_val - 1) -% 1) & 0x3f;

        return ImmLogic{
            .val = original_value,
            .n = out_n != 0,
            .r = @intCast(r_val),
            .s = @intCast(s_val),
            .size = operand_size,
        };
    }

    fn lowestSetBit(val: u64) u64 {
        const bit = @ctz(val);
        // If bit >= 64, there's no set bit, return 0
        if (bit >= 64) {
            return 0;
        }
        return @as(u64, 1) << @intCast(bit);
    }

    /// Returns bits ready for encoding: (N:1, R:6, S:6)
    pub fn encBits(self: ImmLogic) u32 {
        return (@as(u32, @intFromBool(self.n)) << 12) |
            (@as(u32, self.r) << 6) |
            @as(u32, self.s);
    }

    /// Returns the value that this immediate represents.
    pub fn value(self: ImmLogic) u64 {
        return self.val;
    }

    /// Return an immediate for the bitwise-inverted value.
    pub fn invert(self: ImmLogic) ImmLogic {
        return maybeFromU64(~self.val, self.size.toTy()).?;
    }

    pub fn prettyPrint(self: ImmLogic, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// An immediate for shift instructions.
pub const ImmShift = struct {
    /// 6-bit shift amount.
    imm: u8,

    /// Create an ImmShift from raw bits, if possible.
    pub fn maybeFromU64(val: u64) ?ImmShift {
        if (val < 64) {
            return ImmShift{ .imm = @intCast(val) };
        }
        return null;
    }

    /// Get the immediate value.
    pub fn value(self: ImmShift) u8 {
        return self.imm;
    }

    pub fn prettyPrint(self: ImmShift, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// A 16-bit immediate for a MOVZ instruction, with a {0,16,32,48}-bit shift.
pub const MoveWideConst = struct {
    /// The value.
    bits_val: u16,
    /// Result is `bits` shifted 16*shift bits to the left.
    shift: u8,

    /// Construct a MoveWideConst from an arbitrary 64-bit constant if possible.
    pub fn maybeFromU64(value: u64) ?MoveWideConst {
        const mask0: u64 = 0x0000_0000_0000_ffff;
        const mask1: u64 = 0x0000_0000_ffff_0000;
        const mask2: u64 = 0x0000_ffff_0000_0000;
        const mask3: u64 = 0xffff_0000_0000_0000;

        if (value == (value & mask0)) {
            return MoveWideConst{
                .bits_val = @intCast(value & mask0),
                .shift = 0,
            };
        }
        if (value == (value & mask1)) {
            return MoveWideConst{
                .bits_val = @intCast((value >> 16) & mask0),
                .shift = 1,
            };
        }
        if (value == (value & mask2)) {
            return MoveWideConst{
                .bits_val = @intCast((value >> 32) & mask0),
                .shift = 2,
            };
        }
        if (value == (value & mask3)) {
            return MoveWideConst{
                .bits_val = @intCast((value >> 48) & mask0),
                .shift = 3,
            };
        }
        return null;
    }

    /// Create a `MoveWideConst` from a given shift, if possible.
    pub fn maybeWithShift(imm: u16, shift: u8) ?MoveWideConst {
        const shift_enc = shift / 16;
        if (shift_enc > 3) {
            return null;
        }
        return MoveWideConst{
            .bits_val = imm,
            .shift = shift_enc,
        };
    }

    /// Create a zero immediate of this format.
    pub fn zero() MoveWideConst {
        return MoveWideConst{ .bits_val = 0, .shift = 0 };
    }

    pub fn prettyPrint(self: MoveWideConst, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// Advanced SIMD modified immediate as used by MOVI/MVNI.
pub const ASIMDMovModImm = struct {
    imm: u8,
    shift: u8,
    is_64bit: bool,
    shift_ones: bool,

    /// Construct an ASIMDMovModImm from an arbitrary 64-bit constant, if possible.
    pub fn maybeFromU64(val: u64, size: ScalarSize) ?ASIMDMovModImm {
        switch (size) {
            .size8 => {
                return ASIMDMovModImm{
                    .imm = @truncate(val),
                    .shift = 0,
                    .is_64bit = false,
                    .shift_ones = false,
                };
            },
            .size16 => {
                const val16: u16 = @truncate(val);
                if (val16 >> 8 == 0) {
                    return ASIMDMovModImm{
                        .imm = @truncate(val16),
                        .shift = 0,
                        .is_64bit = false,
                        .shift_ones = false,
                    };
                } else if (@as(u8, @truncate(val16)) == 0) {
                    return ASIMDMovModImm{
                        .imm = @truncate(val16 >> 8),
                        .shift = 8,
                        .is_64bit = false,
                        .shift_ones = false,
                    };
                }
                return null;
            },
            .size32 => {
                const val32: u32 = @truncate(val);
                // Value is of the form 0x00MMFFFF
                if (val32 & 0xFF00FFFF == 0x0000FFFF) {
                    return ASIMDMovModImm{
                        .imm = @truncate(val32 >> 16),
                        .shift = 16,
                        .is_64bit = false,
                        .shift_ones = true,
                    };
                }
                // Value is of the form 0x0000MMFF
                if (val32 & 0xFFFF00FF == 0x000000FF) {
                    return ASIMDMovModImm{
                        .imm = @truncate(val32 >> 8),
                        .shift = 8,
                        .is_64bit = false,
                        .shift_ones = true,
                    };
                }
                // Check each byte position
                var shift: u5 = 0;
                while (shift < 32) : (shift += 8) {
                    if (val32 & (@as(u32, 0xFF) << shift) == val32) {
                        return ASIMDMovModImm{
                            .imm = @truncate(val32 >> shift),
                            .shift = shift,
                            .is_64bit = false,
                            .shift_ones = false,
                        };
                    }
                }
                return null;
            },
            .size64 => {
                var imm: u8 = 0;
                // Check if all bytes are either 0 or 0xFF
                var i: u3 = 0;
                while (i < 8) : (i += 1) {
                    const b: u8 = @truncate(val >> (@as(u6, i) * 8));
                    if (b == 0 or b == 0xFF) {
                        imm |= (b & 1) << i;
                    } else {
                        return null;
                    }
                }
                return ASIMDMovModImm{
                    .imm = imm,
                    .shift = 0,
                    .is_64bit = true,
                    .shift_ones = false,
                };
            },
            else => return null,
        }
    }

    /// Create a zero immediate of this format.
    pub fn zero_for_size(size: ScalarSize) ASIMDMovModImm {
        return ASIMDMovModImm{
            .imm = 0,
            .shift = 0,
            .is_64bit = size == .size64,
            .shift_ones = false,
        };
    }

    /// Returns the value that this immediate represents.
    pub fn value(self: ASIMDMovModImm) struct { imm: u8, shift: u32, shift_ones: bool } {
        return .{ .imm = self.imm, .shift = @as(u32, self.shift), .shift_ones = self.shift_ones };
    }

    pub fn prettyPrint(self: ASIMDMovModImm, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// Advanced SIMD modified immediate as used by the vector variant of FMOV.
pub const ASIMDFPModImm = struct {
    imm: u8,
    size: ScalarSize,

    /// Construct an ASIMDFPModImm from an arbitrary 64-bit constant, if possible.
    pub fn maybeFromU64(value: u64, size: ScalarSize) ?ASIMDFPModImm {
        switch (size) {
            .size16 => {
                const val16: u16 = @truncate(value);
                const b0_5: u16 = (val16 >> 6) & 0b111111;
                const b6: u16 = (val16 >> 6) & (1 << 6);
                const b7: u16 = (val16 >> 8) & (1 << 7);
                const imm: u8 = @truncate(b0_5 | b6 | b7);

                if (val16 == value16(imm)) {
                    return ASIMDFPModImm{ .imm = imm, .size = size };
                }
                return null;
            },
            .size32 => {
                const val32: u32 = @truncate(value);
                const b0_5: u32 = (val32 >> 19) & 0b111111;
                const b6: u32 = (val32 >> 19) & (1 << 6);
                const b7: u32 = (val32 >> 24) & (1 << 7);
                const imm: u8 = @truncate(b0_5 | b6 | b7);

                if (val32 == value32(imm)) {
                    return ASIMDFPModImm{ .imm = imm, .size = size };
                }
                return null;
            },
            .size64 => {
                const b0_5: u64 = (value >> 48) & 0b111111;
                const b6: u64 = (value >> 48) & (1 << 6);
                const b7: u64 = (value >> 56) & (1 << 7);
                const imm: u8 = @truncate(b0_5 | b6 | b7);

                if (value == value64(imm)) {
                    return ASIMDFPModImm{ .imm = imm, .size = size };
                }
                return null;
            },
            else => return null,
        }
    }

    /// Returns bits ready for encoding.
    pub fn encBits(self: ASIMDFPModImm) u8 {
        return self.imm;
    }

    /// Returns the 16-bit value that corresponds to an 8-bit encoding.
    fn value16(imm: u8) u16 {
        const imm16: u16 = imm;
        const b0_5 = imm16 & 0b111111;
        const b6 = (imm16 >> 6) & 1;
        const b6_inv = b6 ^ 1;
        const b7 = (imm16 >> 7) & 1;

        return (b0_5 << 6) | ((b6 * 0b11) << 12) | (b6_inv << 14) | (b7 << 15);
    }

    /// Returns the 32-bit value that corresponds to an 8-bit encoding.
    fn value32(imm: u8) u32 {
        const imm32: u32 = imm;
        const b0_5 = imm32 & 0b111111;
        const b6 = (imm32 >> 6) & 1;
        const b6_inv = b6 ^ 1;
        const b7 = (imm32 >> 7) & 1;

        return (b0_5 << 19) | ((b6 * 0b11111) << 25) | (b6_inv << 30) | (b7 << 31);
    }

    /// Returns the 64-bit value that corresponds to an 8-bit encoding.
    fn value64(imm: u8) u64 {
        const imm64: u64 = imm;
        const b0_5 = imm64 & 0b111111;
        const b6 = (imm64 >> 6) & 1;
        const b6_inv = b6 ^ 1;
        const b7 = (imm64 >> 7) & 1;

        return (b0_5 << 48) | ((b6 * 0b11111111) << 54) | (b6_inv << 62) | (b7 << 63);
    }

    pub fn prettyPrint(self: ASIMDFPModImm, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

//=============================================================================
// Tests

test "NZCV bits" {
    const testing = std.testing;
    const nzcv = NZCV.init(true, false, true, false);
    try testing.expectEqual(@as(u32, 0b1010), nzcv.bits());
}

test "UImm5 range" {
    const testing = std.testing;
    try testing.expect(UImm5.maybeFromU8(0) != null);
    try testing.expect(UImm5.maybeFromU8(31) != null);
    try testing.expect(UImm5.maybeFromU8(32) == null);
}

test "SImm9 range" {
    const testing = std.testing;
    try testing.expect(SImm9.maybeFromI64(-256) != null);
    try testing.expect(SImm9.maybeFromI64(255) != null);
    try testing.expect(SImm9.maybeFromI64(-257) == null);
    try testing.expect(SImm9.maybeFromI64(256) == null);
}

test "Imm12" {
    const testing = std.testing;
    // Simple value
    const imm1 = Imm12.maybeFromU64(0xfff);
    try testing.expect(imm1 != null);
    try testing.expectEqual(false, imm1.?.shift12);

    // Shifted value
    const imm2 = Imm12.maybeFromU64(0xfff000);
    try testing.expect(imm2 != null);
    try testing.expectEqual(true, imm2.?.shift12);

    // Invalid value
    try testing.expect(Imm12.maybeFromU64(0x1fff) == null);
}

test "ImmShift" {
    const testing = std.testing;
    try testing.expect(ImmShift.maybeFromU64(0) != null);
    try testing.expect(ImmShift.maybeFromU64(63) != null);
    try testing.expect(ImmShift.maybeFromU64(64) == null);
}

test "MoveWideConst" {
    const testing = std.testing;
    // Low 16 bits
    const mwc1 = MoveWideConst.maybeFromU64(0xffff);
    try testing.expect(mwc1 != null);
    try testing.expectEqual(@as(u8, 0), mwc1.?.shift);

    // Second 16 bits
    const mwc2 = MoveWideConst.maybeFromU64(0xffff0000);
    try testing.expect(mwc2 != null);
    try testing.expectEqual(@as(u8, 1), mwc2.?.shift);

    // Invalid (bits in multiple positions)
    try testing.expect(MoveWideConst.maybeFromU64(0x1_0001) == null);
}

test "ImmLogic basic values" {
    const testing = std.testing;

    // 0 should fail
    try testing.expect(ImmLogic.maybeFromU64(0, Type.i64) == null);

    // All 1s should fail
    try testing.expect(ImmLogic.maybeFromU64(std.math.maxInt(u64), Type.i64) == null);

    // 1 should succeed
    const imm1 = ImmLogic.maybeFromU64(1, Type.i64);
    try testing.expect(imm1 != null);
    try testing.expectEqual(@as(u64, 1), imm1.?.value());
}
