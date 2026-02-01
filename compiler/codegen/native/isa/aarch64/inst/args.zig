//! AArch64 ISA definitions: instruction arguments.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/inst/args.rs`

const std = @import("std");
const Allocator = std.mem.Allocator;

// Forward declare types that will be provided by machinst
// These are stub types for now until machinst integration is complete
pub const MachLabel = struct {
    index: u32,

    pub fn fromU32(i: u32) MachLabel {
        return .{ .index = i };
    }

    pub fn asU32(self: MachLabel) u32 {
        return self.index;
    }
};

pub const RegClass = enum {
    int,
    float,
    vector,
};

pub const Reg = struct {
    bits: u32,

    const CLASS_SHIFT = 30;
    const CLASS_MASK: u32 = 0x3 << CLASS_SHIFT;
    const INDEX_MASK: u32 = 0x3FFF_FFFF;

    pub fn class(self: Reg) RegClass {
        const cls_bits = (self.bits & CLASS_MASK) >> CLASS_SHIFT;
        return switch (cls_bits) {
            0 => .int,
            1 => .float,
            else => .vector,
        };
    }

    pub fn fromRealReg(preg: PReg) Reg {
        const cls_bits: u32 = switch (preg.class_val) {
            .int => 0,
            .float => 1,
            .vector => 2,
        };
        return .{ .bits = (cls_bits << CLASS_SHIFT) | @as(u32, preg.index_val) };
    }

    pub fn fromPReg(preg: PReg) Reg {
        return fromRealReg(preg);
    }

    pub fn fromVReg(vreg: VReg) Reg {
        const cls_bits: u32 = switch (vreg.cls) {
            .int => 0,
            .float => 1,
            .vector => 2,
        };
        return .{ .bits = (cls_bits << CLASS_SHIFT) | vreg.idx };
    }

    pub fn toRealReg(self: Reg) ?RealReg {
        // For now, all regs are considered "real" in this stub
        return RealReg{
            .hw_enc_val = @truncate(self.bits & INDEX_MASK),
            .cls = self.class(),
        };
    }
};

pub const VReg = struct {
    idx: u32,
    cls: RegClass,

    pub fn init(idx_val: anytype, cls_val: RegClass) VReg {
        return .{ .idx = @intCast(idx_val), .cls = cls_val };
    }

    pub fn index(self: VReg) usize {
        return self.idx;
    }
};

pub const RealReg = struct {
    hw_enc_val: u8,
    cls: RegClass,

    pub fn hwEnc(self: RealReg) u8 {
        return self.hw_enc_val;
    }

    pub fn class(self: RealReg) RegClass {
        return self.cls;
    }
};

pub const PReg = struct {
    index_val: u8,
    class_val: RegClass,

    pub fn init(idx: u8, cls: RegClass) PReg {
        return .{ .index_val = idx, .class_val = cls };
    }

    pub fn index(self: PReg) usize {
        return self.index_val;
    }

    pub fn class(self: PReg) RegClass {
        return self.class_val;
    }
};

//=============================================================================
// Instruction sub-components: shift and extend descriptors

/// A shift operator for a register or immediate.
pub const ShiftOp = enum(u8) {
    /// Logical shift left.
    lsl = 0b00,
    /// Logical shift right.
    lsr = 0b01,
    /// Arithmetic shift right.
    asr = 0b10,
    /// Rotate right.
    ror = 0b11,

    /// Get the encoding of this shift op.
    pub fn bits(self: ShiftOp) u8 {
        return @intFromEnum(self);
    }
};

/// A shift operator amount.
pub const ShiftOpShiftImm = struct {
    value: u8,

    /// Maximum shift for shifted-register operands.
    pub const MAX_SHIFT: u64 = 63;

    /// Create a new shiftop shift amount, if possible.
    pub fn maybeFromShift(shift: u64) ?ShiftOpShiftImm {
        if (shift <= MAX_SHIFT) {
            return ShiftOpShiftImm{ .value = @intCast(shift) };
        }
        return null;
    }

    /// Return the shift amount.
    pub fn getValue(self: ShiftOpShiftImm) u8 {
        return self.value;
    }

    /// Mask down to a given number of bits.
    pub fn mask(self: ShiftOpShiftImm, num_bits: u8) ShiftOpShiftImm {
        return ShiftOpShiftImm{ .value = self.value & (num_bits - 1) };
    }

    pub fn prettyPrint(self: ShiftOpShiftImm, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// A shift operator with an amount, guaranteed to be within range.
pub const ShiftOpAndAmt = struct {
    /// The shift operator.
    op: ShiftOp,
    /// The shift operator amount.
    shift: ShiftOpShiftImm,

    /// Create a new shift operator with an amount.
    pub fn init(op: ShiftOp, shift: ShiftOpShiftImm) ShiftOpAndAmt {
        return ShiftOpAndAmt{ .op = op, .shift = shift };
    }

    /// Get the shift op.
    pub fn getOp(self: ShiftOpAndAmt) ShiftOp {
        return self.op;
    }

    /// Get the shift amount.
    pub fn amt(self: ShiftOpAndAmt) ShiftOpShiftImm {
        return self.shift;
    }

    pub fn prettyPrint(self: ShiftOpAndAmt, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// An extend operator for a register.
pub const ExtendOp = enum(u8) {
    /// Unsigned extend byte.
    uxtb = 0b000,
    /// Unsigned extend halfword.
    uxth = 0b001,
    /// Unsigned extend word.
    uxtw = 0b010,
    /// Unsigned extend doubleword.
    uxtx = 0b011,
    /// Signed extend byte.
    sxtb = 0b100,
    /// Signed extend halfword.
    sxth = 0b101,
    /// Signed extend word.
    sxtw = 0b110,
    /// Signed extend doubleword.
    sxtx = 0b111,

    /// Encoding of this op.
    pub fn bits(self: ExtendOp) u8 {
        return @intFromEnum(self);
    }

    pub fn prettyPrint(self: ExtendOp, _: u8) []const u8 {
        return @tagName(self);
    }
};

//=============================================================================
// Instruction sub-components (memory addresses): definitions

/// A reference to some memory address.
pub const MemLabel = union(enum) {
    /// An address in the code, a constant pool or jumptable, with relative
    /// offset from this instruction. This form must be used at emission time.
    pc_rel: i32,
    /// An address that refers to a label within a MachBuffer, for example a
    /// constant that lives in the pool at the end of the function.
    mach: MachLabel,

    pub fn prettyPrint(self: MemLabel, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

//=============================================================================
// Instruction sub-components (conditions, branches and branch targets):
// definitions

/// Condition for conditional branches.
pub const Cond = enum(u8) {
    /// Equal.
    eq = 0,
    /// Not equal.
    ne = 1,
    /// Unsigned greater than or equal to.
    hs = 2,
    /// Unsigned less than.
    lo = 3,
    /// Minus, negative.
    mi = 4,
    /// Positive or zero.
    pl = 5,
    /// Signed overflow.
    vs = 6,
    /// No signed overflow.
    vc = 7,
    /// Unsigned greater than.
    hi = 8,
    /// Unsigned less than or equal to.
    ls = 9,
    /// Signed greater or equal to.
    ge = 10,
    /// Signed less than.
    lt = 11,
    /// Signed greater than.
    gt = 12,
    /// Signed less than or equal.
    le = 13,
    /// Always executed.
    al = 14,
    /// Always executed (alternative encoding).
    nv = 15,

    /// Return the inverted condition.
    pub fn invert(self: Cond) Cond {
        return switch (self) {
            .eq => .ne,
            .ne => .eq,
            .hs => .lo,
            .lo => .hs,
            .mi => .pl,
            .pl => .mi,
            .vs => .vc,
            .vc => .vs,
            .hi => .ls,
            .ls => .hi,
            .ge => .lt,
            .lt => .ge,
            .gt => .le,
            .le => .gt,
            .al => .nv,
            .nv => .al,
        };
    }

    /// Return the machine encoding of this condition.
    pub fn bits(self: Cond) u32 {
        return @intFromEnum(self);
    }

    pub fn prettyPrint(self: Cond, _: u8) []const u8 {
        return @tagName(self);
    }
};

/// The kind of conditional branch: the common-case-optimized "reg-is-zero" /
/// "reg-is-nonzero" variants, or the generic one that tests the machine
/// condition codes.
pub const CondBrKind = union(enum) {
    /// Condition: given register is zero.
    zero: struct { reg: Reg, size: OperandSize },
    /// Condition: given register is nonzero.
    not_zero: struct { reg: Reg, size: OperandSize },
    /// Condition: the given condition-code test is true.
    cond: Cond,

    /// Return the inverted branch condition.
    pub fn invert(self: CondBrKind) CondBrKind {
        return switch (self) {
            .zero => |z| .{ .not_zero = .{ .reg = z.reg, .size = z.size } },
            .not_zero => |nz| .{ .zero = .{ .reg = nz.reg, .size = nz.size } },
            .cond => |c| .{ .cond = c.invert() },
        };
    }
};

/// A branch target. Either unresolved (basic-block index) or resolved (offset
/// from end of current instruction).
pub const BranchTarget = union(enum) {
    /// An unresolved reference to a Label.
    label: MachLabel,
    /// A fixed PC offset.
    resolved_offset: i32,

    /// Return the target's label, if it is a label-based target.
    pub fn asLabel(self: BranchTarget) ?MachLabel {
        return switch (self) {
            .label => |l| l,
            .resolved_offset => null,
        };
    }

    /// Return the target's offset, if specified, or zero if label-based.
    pub fn asOffset14OrZero(self: BranchTarget) u32 {
        return self.asOffsetBounded(14);
    }

    /// Return the target's offset, if specified, or zero if label-based.
    pub fn asOffset19OrZero(self: BranchTarget) u32 {
        return self.asOffsetBounded(19);
    }

    /// Return the target's offset, if specified, or zero if label-based.
    pub fn asOffset26OrZero(self: BranchTarget) u32 {
        return self.asOffsetBounded(26);
    }

    fn asOffsetBounded(self: BranchTarget, num_bits: u32) u32 {
        const off: i32 = switch (self) {
            .resolved_offset => |o| @divExact(o, 4),
            .label => 0,
        };
        const hi: i32 = (@as(i32, 1) << @intCast(num_bits - 1)) - 1;
        const lo: i32 = -(@as(i32, 1) << @intCast(num_bits - 1));
        std.debug.assert(off <= hi);
        std.debug.assert(off >= lo);
        return @as(u32, @bitCast(off)) & ((@as(u32, 1) << @intCast(num_bits)) - 1);
    }

    pub fn prettyPrint(self: BranchTarget, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

//=============================================================================
// Operand sizes

/// Type used to communicate the operand size of a machine instruction, as AArch64 has 32- and
/// 64-bit variants of many instructions (and integer registers).
pub const OperandSize = enum {
    /// 32-bit.
    size32,
    /// 64-bit.
    size64,

    /// 32-bit case?
    pub fn is32(self: OperandSize) bool {
        return self == .size32;
    }

    /// 64-bit case?
    pub fn is64(self: OperandSize) bool {
        return self == .size64;
    }

    /// Convert from a needed width to the smallest size that fits.
    pub fn fromBits(num_bits: usize) OperandSize {
        std.debug.assert(num_bits <= 64);
        if (num_bits <= 32) {
            return .size32;
        } else {
            return .size64;
        }
    }

    /// Return the operand size in bits.
    pub fn bits(self: OperandSize) u8 {
        return switch (self) {
            .size32 => 32,
            .size64 => 64,
        };
    }

    /// Convert from an integer type into the smallest size that fits.
    pub fn fromTy(ty: Type) OperandSize {
        return fromBits(ty.bits());
    }

    /// Convert to I32 or I64.
    pub fn toTy(self: OperandSize) Type {
        return switch (self) {
            .size32 => Type.i32,
            .size64 => Type.i64,
        };
    }

    /// Register interpretation bit.
    /// When 0, the register is interpreted as the 32-bit version.
    /// When 1, the register is interpreted as the 64-bit version.
    pub fn sfBit(self: OperandSize) u32 {
        return switch (self) {
            .size32 => 0,
            .size64 => 1,
        };
    }

    /// The maximum unsigned value representable in a value of this size.
    pub fn maxValue(self: OperandSize) u64 {
        return switch (self) {
            .size32 => std.math.maxInt(u32),
            .size64 => std.math.maxInt(u64),
        };
    }
};

/// Type used to communicate the size of a scalar SIMD & FP operand.
pub const ScalarSize = enum {
    /// 8-bit.
    size8,
    /// 16-bit.
    size16,
    /// 32-bit.
    size32,
    /// 64-bit.
    size64,
    /// 128-bit.
    size128,

    /// Convert to an integer operand size.
    pub fn operandSize(self: ScalarSize) OperandSize {
        return switch (self) {
            .size8, .size16, .size32 => .size32,
            .size64 => .size64,
            .size128 => unreachable, // No 128-bit integer operand size
        };
    }

    /// Return the encoding bits that are used by some scalar FP instructions
    /// for a particular operand size.
    pub fn ftype(self: ScalarSize) u32 {
        return switch (self) {
            .size16 => 0b11,
            .size32 => 0b00,
            .size64 => 0b01,
            else => unreachable,
        };
    }

    /// Return the widened version of the scalar size.
    pub fn widen(self: ScalarSize) ScalarSize {
        return switch (self) {
            .size8 => .size16,
            .size16 => .size32,
            .size32 => .size64,
            .size64 => .size128,
            .size128 => unreachable,
        };
    }

    /// Return the narrowed version of the scalar size.
    pub fn narrow(self: ScalarSize) ScalarSize {
        return switch (self) {
            .size8 => unreachable,
            .size16 => .size8,
            .size32 => .size16,
            .size64 => .size32,
            .size128 => .size64,
        };
    }

    /// Return a type with the same size as this scalar.
    pub fn ty(self: ScalarSize) Type {
        return switch (self) {
            .size8 => Type.i8,
            .size16 => Type.i16,
            .size32 => Type.i32,
            .size64 => Type.i64,
            .size128 => Type.i128,
        };
    }
};

/// Type used to communicate the size of a vector operand.
pub const VectorSize = enum {
    /// 8-bit, 8 lanes.
    size8x8,
    /// 8-bit, 16 lanes.
    size8x16,
    /// 16-bit, 4 lanes.
    size16x4,
    /// 16-bit, 8 lanes.
    size16x8,
    /// 32-bit, 2 lanes.
    size32x2,
    /// 32-bit, 4 lanes.
    size32x4,
    /// 64-bit, 2 lanes.
    size64x2,

    /// Get the vector operand of the same size but with 8-bit lane size.
    pub fn asScalar8Vector(self: VectorSize) VectorSize {
        return switch (self) {
            // 64-bit vector
            .size8x8, .size16x4, .size32x2 => .size8x8,
            // 128-bit vector
            .size8x16, .size16x8, .size32x4, .size64x2 => .size8x16,
        };
    }

    /// Get the vector operand size with the given scalar size as lane size.
    pub fn fromLaneSize(size: ScalarSize, is_128bit: bool) VectorSize {
        return switch (size) {
            .size8 => if (is_128bit) .size8x16 else .size8x8,
            .size16 => if (is_128bit) .size16x8 else .size16x4,
            .size32 => if (is_128bit) .size32x4 else .size32x2,
            .size64 => if (is_128bit) .size64x2 else unreachable,
            else => unreachable,
        };
    }

    /// Get the integer operand size that corresponds to a lane of a vector with a certain size.
    pub fn operandSize(self: VectorSize) OperandSize {
        return switch (self) {
            .size64x2 => .size64,
            else => .size32,
        };
    }

    /// Get the scalar operand size that corresponds to a lane of a vector with a certain size.
    pub fn laneSize(self: VectorSize) ScalarSize {
        return switch (self) {
            .size8x8, .size8x16 => .size8,
            .size16x4, .size16x8 => .size16,
            .size32x2, .size32x4 => .size32,
            .size64x2 => .size64,
        };
    }

    /// Returns true if the VectorSize is 128-bits.
    pub fn is128bits(self: VectorSize) bool {
        return switch (self) {
            .size8x8, .size16x4, .size32x2 => false,
            .size8x16, .size16x8, .size32x4, .size64x2 => true,
        };
    }

    /// Return the encoding bits that are used by some SIMD instructions
    /// for a particular operand size.
    pub fn encSize(self: VectorSize) struct { q: u32, size: u32 } {
        const q: u32 = if (self.is128bits()) 1 else 0;
        const size: u32 = switch (self.laneSize()) {
            .size8 => 0b00,
            .size16 => 0b01,
            .size32 => 0b10,
            .size64 => 0b11,
            else => unreachable,
        };
        return .{ .q = q, .size = size };
    }

    /// Return the encoding bit that is used by some floating-point SIMD
    /// instructions for a particular operand size.
    pub fn encFloatSize(self: VectorSize) u32 {
        return switch (self.laneSize()) {
            .size32 => 0b0,
            .size64 => 0b1,
            else => unreachable,
        };
    }
};

/// Pointer authentication key.
pub const APIKey = enum {
    az,
    asp,
    bz,
    bsp,

    /// Returns the encoding of the `auti{key}` instruction used to decrypt the
    /// `lr` register.
    pub fn encAutiHint(self: APIKey) u32 {
        const crm_op2: struct { crm: u32, op2: u32 } = switch (self) {
            .az => .{ .crm = 0b0011, .op2 = 0b100 },
            .asp => .{ .crm = 0b0011, .op2 = 0b101 },
            .bz => .{ .crm = 0b0011, .op2 = 0b110 },
            .bsp => .{ .crm = 0b0011, .op2 = 0b111 },
        };
        return 0xd503201f | (crm_op2.crm << 8) | (crm_op2.op2 << 5);
    }
};

/// Kind for TestBitAndBranch instruction.
pub const TestBitAndBranchKind = enum {
    z,
    nz,

    /// Complements this branch condition to act on the opposite result.
    pub fn complement(self: TestBitAndBranchKind) TestBitAndBranchKind {
        return switch (self) {
            .z => .nz,
            .nz => .z,
        };
    }
};

/// Kind for BranchTargetType instruction (BTI).
pub const BranchTargetType = enum {
    /// No target type (just a NOP-like hint)
    none,
    /// Jump target (BR instruction).
    j,
    /// Call target (BLR instruction).
    c,
    /// Jump or call target.
    jc,

    pub fn bits(self: BranchTargetType) u32 {
        return switch (self) {
            .none => 0b00,
            .j => 0b01,
            .c => 0b10,
            .jc => 0b11,
        };
    }
};

//=============================================================================
// Stub Type for now (will be replaced with proper IR types)

pub const Type = struct {
    kind: Kind,

    pub const Kind = enum(u8) {
        int8,
        int16,
        int32,
        int64,
        int128,
        float16,
        float32,
        float64,
        float128,
        vec_i8x16,
    };

    pub const @"i8" = Type{ .kind = .int8 };
    pub const @"i16" = Type{ .kind = .int16 };
    pub const @"i32" = Type{ .kind = .int32 };
    pub const @"i64" = Type{ .kind = .int64 };
    pub const @"i128" = Type{ .kind = .int128 };
    pub const @"f16" = Type{ .kind = .float16 };
    pub const @"f32" = Type{ .kind = .float32 };
    pub const @"f64" = Type{ .kind = .float64 };
    pub const @"f128" = Type{ .kind = .float128 };
    pub const @"i8x16" = Type{ .kind = .vec_i8x16 };

    pub fn bits(self: Type) usize {
        return switch (self.kind) {
            .int8 => 8,
            .int16, .float16 => 16,
            .int32, .float32 => 32,
            .int64, .float64 => 64,
            .int128, .float128, .vec_i8x16 => 128,
        };
    }

    pub fn bytes(self: Type) u32 {
        return @intCast(self.bits() / 8);
    }

    pub fn isVector(self: Type) bool {
        return self.kind == .vec_i8x16;
    }

    pub fn isFloat(self: Type) bool {
        return switch (self.kind) {
            .float16, .float32, .float64, .float128 => true,
            else => false,
        };
    }
};

//=============================================================================
// Tests

test "ShiftOp encoding" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 0b00), ShiftOp.lsl.bits());
    try testing.expectEqual(@as(u8, 0b01), ShiftOp.lsr.bits());
    try testing.expectEqual(@as(u8, 0b10), ShiftOp.asr.bits());
    try testing.expectEqual(@as(u8, 0b11), ShiftOp.ror.bits());
}

test "ShiftOpShiftImm creation" {
    const testing = std.testing;
    try testing.expect(ShiftOpShiftImm.maybeFromShift(0) != null);
    try testing.expect(ShiftOpShiftImm.maybeFromShift(63) != null);
    try testing.expect(ShiftOpShiftImm.maybeFromShift(64) == null);
}

test "ExtendOp encoding" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 0b000), ExtendOp.uxtb.bits());
    try testing.expectEqual(@as(u8, 0b110), ExtendOp.sxtw.bits());
    try testing.expectEqual(@as(u8, 0b111), ExtendOp.sxtx.bits());
}

test "Cond inversion" {
    const testing = std.testing;
    try testing.expectEqual(Cond.ne, Cond.eq.invert());
    try testing.expectEqual(Cond.eq, Cond.ne.invert());
    try testing.expectEqual(Cond.lt, Cond.ge.invert());
    try testing.expectEqual(Cond.ge, Cond.lt.invert());
}

test "OperandSize" {
    const testing = std.testing;
    try testing.expect(OperandSize.size32.is32());
    try testing.expect(!OperandSize.size32.is64());
    try testing.expect(OperandSize.size64.is64());
    try testing.expectEqual(@as(u8, 32), OperandSize.size32.bits());
    try testing.expectEqual(@as(u8, 64), OperandSize.size64.bits());
    try testing.expectEqual(@as(u32, 0), OperandSize.size32.sfBit());
    try testing.expectEqual(@as(u32, 1), OperandSize.size64.sfBit());
}

test "ScalarSize" {
    const testing = std.testing;
    try testing.expectEqual(OperandSize.size32, ScalarSize.size8.operandSize());
    try testing.expectEqual(OperandSize.size64, ScalarSize.size64.operandSize());
    try testing.expectEqual(ScalarSize.size16, ScalarSize.size8.widen());
    try testing.expectEqual(ScalarSize.size32, ScalarSize.size64.narrow());
}

test "VectorSize" {
    const testing = std.testing;
    try testing.expect(!VectorSize.size8x8.is128bits());
    try testing.expect(VectorSize.size8x16.is128bits());
    try testing.expectEqual(ScalarSize.size8, VectorSize.size8x8.laneSize());
    try testing.expectEqual(ScalarSize.size64, VectorSize.size64x2.laneSize());
}

test "APIKey encoding" {
    const testing = std.testing;
    const az_enc = APIKey.az.encAutiHint();
    try testing.expect(az_enc != 0);
}

test "BranchTarget offset" {
    const testing = std.testing;
    const target = BranchTarget{ .resolved_offset = 16 };
    try testing.expectEqual(@as(u32, 4), target.asOffset19OrZero());
}
