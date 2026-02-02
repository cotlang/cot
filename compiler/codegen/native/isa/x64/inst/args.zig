//! x86-64 ISA definitions: instruction arguments.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/x64/inst/args.rs`

const std = @import("std");
const Allocator = std.mem.Allocator;

//=============================================================================
// Forward declare types that will be provided by machinst
// These are stub types for now until machinst integration is complete
//=============================================================================

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

    pub fn fromVReg(vreg_val: VReg) Reg {
        const cls_bits: u32 = switch (vreg_val.cls) {
            .int => 0,
            .float => 1,
            .vector => 2,
        };
        return .{ .bits = (cls_bits << CLASS_SHIFT) | vreg_val.idx };
    }

    pub fn toRealReg(self: Reg) ?RealReg {
        // For now, all regs are considered "real" in this stub
        return RealReg{
            .hw_enc_val = @truncate(self.bits & INDEX_MASK),
            .cls = self.class(),
        };
    }

    pub fn isReal(self: Reg) bool {
        return self.toRealReg() != null;
    }

    pub fn hwEnc(self: Reg) u8 {
        if (self.toRealReg()) |rreg| {
            return rreg.hwEnc();
        }
        unreachable;
    }

    pub fn eql(self: Reg, other: Reg) bool {
        return self.bits == other.bits;
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
// Writable wrapper type
//=============================================================================

pub fn Writable(comptime T: type) type {
    return struct {
        reg: T,

        const Self = @This();

        pub fn fromReg(r: T) Self {
            return .{ .reg = r };
        }

        pub fn toReg(self: Self) T {
            return self.reg;
        }

        pub fn map(self: Self, comptime f: fn (T) T) Self {
            return Self{ .reg = f(self.reg) };
        }
    };
}

//=============================================================================
// GPR newtype - wraps Reg and enforces int class
//=============================================================================

/// A general-purpose register (GPR).
pub const Gpr = struct {
    reg: Reg,

    /// Create a Gpr from a Reg, returning null if not an int register.
    pub fn new(r: Reg) ?Gpr {
        if (r.class() == .int) {
            return Gpr{ .reg = r };
        }
        return null;
    }

    /// Like new() but panics on failure.
    pub fn unwrapNew(r: Reg) Gpr {
        if (r.class() == .int) {
            return Gpr{ .reg = r };
        }
        @panic("cannot construct Gpr from non-int register");
    }

    /// Get the underlying Reg.
    pub fn toReg(self: Gpr) Reg {
        return self.reg;
    }

    /// Get the register class.
    pub fn class(self: Gpr) RegClass {
        return self.reg.class();
    }

    /// Get the hardware encoding.
    pub fn hwEnc(self: Gpr) u8 {
        return self.reg.hwEnc();
    }
};

/// Writable GPR.
pub const WritableGpr = Writable(Gpr);

/// Optional writable GPR.
pub const OptionWritableGpr = ?WritableGpr;

//=============================================================================
// XMM newtype - wraps Reg and enforces float class
//=============================================================================

/// An XMM (SSE/AVX) register.
pub const Xmm = struct {
    reg: Reg,

    /// Create an Xmm from a Reg, returning null if not a float register.
    pub fn new(r: Reg) ?Xmm {
        if (r.class() == .float) {
            return Xmm{ .reg = r };
        }
        return null;
    }

    /// Like new() but panics on failure.
    pub fn unwrapNew(r: Reg) Xmm {
        if (r.class() == .float) {
            return Xmm{ .reg = r };
        }
        @panic("cannot construct Xmm from non-float register");
    }

    /// Get the underlying Reg.
    pub fn toReg(self: Xmm) Reg {
        return self.reg;
    }

    /// Get the register class.
    pub fn class(self: Xmm) RegClass {
        return self.reg.class();
    }

    /// Get the hardware encoding.
    pub fn hwEnc(self: Xmm) u8 {
        return self.reg.hwEnc();
    }
};

/// Writable XMM register.
pub const WritableXmm = Writable(Xmm);

/// Optional writable XMM register.
pub const OptionWritableXmm = ?WritableXmm;

//=============================================================================
// Memory flags
//=============================================================================

pub const MemFlags = struct {
    aligned: bool = true,
    readonly: bool = false,
    trap: bool = false,
    notrap: bool = false,

    pub const empty = MemFlags{};
    pub const trusted = MemFlags{};

    pub fn isAligned(self: MemFlags) bool {
        return self.aligned;
    }
};

//=============================================================================
// Addressing modes
//=============================================================================

/// x86-64 addressing mode.
/// Unlike ARM64's various addressing modes, x64 uses a more uniform
/// [base + index*scale + displacement] format.
pub const Amode = union(enum) {
    /// [base + simm32]
    /// The base register plus a signed 32-bit displacement.
    imm_reg: struct {
        simm32: i32,
        base: Reg,
        flags: MemFlags,
    },

    /// [base + index*scale + simm32]
    /// Base register, plus index register scaled by 1/2/4/8, plus displacement.
    imm_reg_reg_shift: struct {
        simm32: i32,
        base: Gpr,
        index: Gpr,
        /// Shift amount: 0, 1, 2, or 3 (meaning scale = 1, 2, 4, or 8).
        shift: u8,
        flags: MemFlags,
    },

    /// RIP-relative addressing: [RIP + displacement]
    /// Used for position-independent code to access globals, constants, etc.
    rip_relative: struct {
        target: MachLabel,
    },

    /// Create an immediate + register addressing mode.
    pub fn immReg(simm32: i32, base: Reg) Amode {
        std.debug.assert(base.class() == .int);
        return .{
            .imm_reg = .{
                .simm32 = simm32,
                .base = base,
                .flags = MemFlags.trusted,
            },
        };
    }

    /// Create a base + index*scale + displacement addressing mode.
    pub fn immRegRegShift(simm32: i32, base: Gpr, index_reg: Gpr, shift: u8) Amode {
        std.debug.assert(shift <= 3);
        return .{
            .imm_reg_reg_shift = .{
                .simm32 = simm32,
                .base = base,
                .index = index_reg,
                .shift = shift,
                .flags = MemFlags.trusted,
            },
        };
    }

    /// Create a RIP-relative addressing mode.
    pub fn ripRelative(target: MachLabel) Amode {
        return .{
            .rip_relative = .{
                .target = target,
            },
        };
    }

    /// Set the memory flags.
    pub fn withFlags(self: Amode, flags: MemFlags) Amode {
        return switch (self) {
            .imm_reg => |ir| .{
                .imm_reg = .{
                    .simm32 = ir.simm32,
                    .base = ir.base,
                    .flags = flags,
                },
            },
            .imm_reg_reg_shift => |irrs| .{
                .imm_reg_reg_shift = .{
                    .simm32 = irrs.simm32,
                    .base = irrs.base,
                    .index = irrs.index,
                    .shift = irrs.shift,
                    .flags = flags,
                },
            },
            .rip_relative => @panic("RIP-relative addressing cannot take memflags"),
        };
    }

    /// Get the memory flags.
    pub fn getFlags(self: Amode) MemFlags {
        return switch (self) {
            .imm_reg => |ir| ir.flags,
            .imm_reg_reg_shift => |irrs| irrs.flags,
            .rip_relative => MemFlags.trusted,
        };
    }

    /// Offset the amode by a fixed amount.
    pub fn offset(self: Amode, off: i32) Amode {
        return switch (self) {
            .imm_reg => |ir| .{
                .imm_reg = .{
                    .simm32 = ir.simm32 + off,
                    .base = ir.base,
                    .flags = ir.flags,
                },
            },
            .imm_reg_reg_shift => |irrs| .{
                .imm_reg_reg_shift = .{
                    .simm32 = irrs.simm32 + off,
                    .base = irrs.base,
                    .index = irrs.index,
                    .shift = irrs.shift,
                    .flags = irrs.flags,
                },
            },
            .rip_relative => @panic("Cannot offset RIP-relative amode"),
        };
    }

    /// Check if this addressing mode is aligned.
    pub fn isAligned(self: Amode) bool {
        return self.getFlags().aligned;
    }

    pub fn prettyPrint(self: Amode, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

//=============================================================================
// Synthetic addressing modes (pre-finalization)
//=============================================================================

/// Virtual code constant reference (for constant pool).
pub const VCodeConstant = struct {
    index: u32,

    pub fn asU32(self: VCodeConstant) u32 {
        return self.index;
    }
};

/// A memory address used during compilation.
/// These are "synthetic" because they may reference things not yet resolved,
/// like stack slots or incoming arguments.
pub const SyntheticAmode = union(enum) {
    /// A real amode (already resolved).
    real: Amode,

    /// A (virtual) offset into the incoming argument area.
    incoming_arg: struct {
        /// The downward offset from the start of the incoming argument area.
        offset_val: u32,
    },

    /// A (virtual) offset to the slot area of the function frame.
    slot_offset: struct {
        simm32: i32,
    },

    /// A virtual offset to a constant in the constant pool.
    constant_offset: VCodeConstant,

    /// Create a real addressing mode.
    pub fn real_amode(amode: Amode) SyntheticAmode {
        return .{ .real = amode };
    }

    /// Create a slot offset.
    pub fn slotOffset(simm32: i32) SyntheticAmode {
        return .{ .slot_offset = .{ .simm32 = simm32 } };
    }

    /// Create an incoming argument offset.
    pub fn incomingArg(offset_val: u32) SyntheticAmode {
        return .{ .incoming_arg = .{ .offset_val = offset_val } };
    }

    /// Check if this addressing mode is aligned.
    pub fn isAligned(self: SyntheticAmode) bool {
        return switch (self) {
            .real => |r| r.isAligned(),
            .incoming_arg, .slot_offset, .constant_offset => true,
        };
    }

    /// Offset the synthetic amode.
    pub fn offset(self: SyntheticAmode, off: i32) SyntheticAmode {
        return switch (self) {
            .real => |r| .{ .real = r.offset(off) },
            .slot_offset => |s| .{ .slot_offset = .{ .simm32 = s.simm32 + off } },
            .incoming_arg, .constant_offset => @panic("Cannot offset this SyntheticAmode"),
        };
    }

    pub fn prettyPrint(self: SyntheticAmode, _: u8) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

//=============================================================================
// Register/Memory/Immediate operand types
//=============================================================================

/// An operand which is either a register, memory address, or immediate.
/// For the immediate form, the value is sign-extended to 64 bits.
pub const RegMemImm = union(enum) {
    /// A register operand.
    reg: Reg,
    /// A memory operand.
    mem: SyntheticAmode,
    /// An immediate operand (32-bit, sign-extended to 64).
    imm: u32,

    /// Create a register operand.
    pub fn fromReg(r: Reg) RegMemImm {
        std.debug.assert(r.class() == .int or r.class() == .float);
        return .{ .reg = r };
    }

    /// Create a memory operand.
    pub fn fromMem(addr: SyntheticAmode) RegMemImm {
        return .{ .mem = addr };
    }

    /// Create an immediate operand.
    pub fn fromImm(simm32: u32) RegMemImm {
        return .{ .imm = simm32 };
    }

    pub fn prettyPrint(self: RegMemImm, size: u8) []const u8 {
        _ = self;
        _ = size;
        return ""; // TODO: implement pretty printing
    }
};

/// An operand which is either a register or memory address.
pub const RegMem = union(enum) {
    /// A register operand.
    reg: Reg,
    /// A memory operand.
    mem: SyntheticAmode,

    /// Create a register operand.
    pub fn fromReg(r: Reg) RegMem {
        std.debug.assert(r.class() == .int or r.class() == .float);
        return .{ .reg = r };
    }

    /// Create a memory operand.
    pub fn fromMem(addr: SyntheticAmode) RegMem {
        return .{ .mem = addr };
    }

    /// Assert that if this is a register, it has the expected class.
    pub fn assertRegclassIs(self: RegMem, expected: RegClass) void {
        if (self == .reg) {
            std.debug.assert(self.reg.class() == expected);
        }
    }

    pub fn prettyPrint(self: RegMem, size: u8) []const u8 {
        _ = self;
        _ = size;
        return ""; // TODO: implement pretty printing
    }
};

/// GPR or memory operand.
pub const GprMem = struct {
    inner: RegMem,

    pub fn new(rm: RegMem) ?GprMem {
        switch (rm) {
            .reg => |r| {
                if (r.class() != .int) return null;
                return GprMem{ .inner = rm };
            },
            .mem => return GprMem{ .inner = rm },
        }
    }

    pub fn unwrapNew(rm: RegMem) GprMem {
        return new(rm) orelse @panic("GprMem requires int register or memory");
    }

    pub fn toRegMem(self: GprMem) RegMem {
        return self.inner;
    }
};

/// GPR, memory, or immediate operand.
pub const GprMemImm = struct {
    inner: RegMemImm,

    pub fn new(rmi: RegMemImm) ?GprMemImm {
        switch (rmi) {
            .reg => |r| {
                if (r.class() != .int) return null;
                return GprMemImm{ .inner = rmi };
            },
            .mem, .imm => return GprMemImm{ .inner = rmi },
        }
    }

    pub fn unwrapNew(rmi: RegMemImm) GprMemImm {
        return new(rmi) orelse @panic("GprMemImm requires int register, memory, or immediate");
    }

    pub fn toRegMemImm(self: GprMemImm) RegMemImm {
        return self.inner;
    }
};

/// XMM or memory operand.
pub const XmmMem = struct {
    inner: RegMem,

    pub fn new(rm: RegMem) ?XmmMem {
        switch (rm) {
            .reg => |r| {
                if (r.class() != .float) return null;
                return XmmMem{ .inner = rm };
            },
            .mem => return XmmMem{ .inner = rm },
        }
    }

    pub fn unwrapNew(rm: RegMem) XmmMem {
        return new(rm) orelse @panic("XmmMem requires float register or memory");
    }

    pub fn toRegMem(self: XmmMem) RegMem {
        return self.inner;
    }
};

/// XMM or aligned memory operand.
pub const XmmMemAligned = struct {
    inner: RegMem,

    pub fn new(rm: RegMem) ?XmmMemAligned {
        switch (rm) {
            .reg => |r| {
                if (r.class() != .float) return null;
                return XmmMemAligned{ .inner = rm };
            },
            .mem => |m| {
                if (!m.isAligned()) return null;
                return XmmMemAligned{ .inner = rm };
            },
        }
    }

    pub fn unwrapNew(rm: RegMem) XmmMemAligned {
        return new(rm) orelse @panic("XmmMemAligned requires float register or aligned memory");
    }

    pub fn toRegMem(self: XmmMemAligned) RegMem {
        return self.inner;
    }
};

/// XMM, memory, or immediate operand.
pub const XmmMemImm = struct {
    inner: RegMemImm,

    pub fn new(rmi: RegMemImm) ?XmmMemImm {
        switch (rmi) {
            .reg => |r| {
                if (r.class() != .float) return null;
                return XmmMemImm{ .inner = rmi };
            },
            .mem, .imm => return XmmMemImm{ .inner = rmi },
        }
    }

    pub fn unwrapNew(rmi: RegMemImm) XmmMemImm {
        return new(rmi) orelse @panic("XmmMemImm requires float register, memory, or immediate");
    }

    pub fn toRegMemImm(self: XmmMemImm) RegMemImm {
        return self.inner;
    }
};

//=============================================================================
// Extension modes
//=============================================================================

/// How a value should be extended when loading.
pub const ExtKind = enum {
    /// No extension needed.
    none,
    /// Sign-extend the value.
    sign_extend,
    /// Zero-extend the value.
    zero_extend,
};

/// Extension mode: source and destination widths.
/// B(yte) = 8 bits, W(ord) = 16 bits, L(ong) = 32 bits, Q(uad) = 64 bits.
pub const ExtMode = enum {
    /// Byte to Longword (8 -> 32).
    bl,
    /// Byte to Quadword (8 -> 64).
    bq,
    /// Word to Longword (16 -> 32).
    wl,
    /// Word to Quadword (16 -> 64).
    wq,
    /// Longword to Quadword (32 -> 64).
    lq,

    /// Calculate the ExtMode from source and destination bit widths.
    pub fn fromBits(from_bits: u16, to_bits: u16) ?ExtMode {
        return switch (from_bits) {
            1, 8 => switch (to_bits) {
                8, 16, 32 => .bl,
                64 => .bq,
                else => null,
            },
            16 => switch (to_bits) {
                32 => .wl,
                64 => .wq,
                else => null,
            },
            32 => switch (to_bits) {
                64 => .lq,
                else => null,
            },
            else => null,
        };
    }

    /// Get the name of this extension mode.
    pub fn name(self: ExtMode) []const u8 {
        return switch (self) {
            .bl => "bl",
            .bq => "bq",
            .wl => "wl",
            .wq => "wq",
            .lq => "lq",
        };
    }
};

//=============================================================================
// Condition codes
//=============================================================================

/// x86-64 condition codes.
/// These map to the EFLAGS register bits tested by conditional instructions.
pub const CC = enum(u8) {
    /// Overflow (OF=1).
    o = 0,
    /// No overflow (OF=0).
    no = 1,
    /// Below / Carry (CF=1). Unsigned less than.
    b = 2,
    /// Not below / No carry (CF=0). Unsigned greater than or equal.
    nb = 3,
    /// Zero / Equal (ZF=1).
    z = 4,
    /// Not zero / Not equal (ZF=0).
    nz = 5,
    /// Below or equal (CF=1 or ZF=1). Unsigned less than or equal.
    be = 6,
    /// Not below or equal / Above (CF=0 and ZF=0). Unsigned greater than.
    nbe = 7,
    /// Sign / Negative (SF=1).
    s = 8,
    /// Not sign / Non-negative (SF=0).
    ns = 9,
    /// Parity even (PF=1).
    p = 10,
    /// Parity odd (PF=0).
    np = 11,
    /// Less than (SF≠OF). Signed less than.
    l = 12,
    /// Not less / Greater or equal (SF=OF). Signed greater than or equal.
    nl = 13,
    /// Less or equal (ZF=1 or SF≠OF). Signed less than or equal.
    le = 14,
    /// Not less or equal / Greater (ZF=0 and SF=OF). Signed greater than.
    nle = 15,

    /// Get the encoding of this condition code.
    pub fn getEnc(self: CC) u8 {
        return @intFromEnum(self);
    }

    /// Invert the condition.
    pub fn invert(self: CC) CC {
        return switch (self) {
            .o => .no,
            .no => .o,
            .b => .nb,
            .nb => .b,
            .z => .nz,
            .nz => .z,
            .be => .nbe,
            .nbe => .be,
            .s => .ns,
            .ns => .s,
            .p => .np,
            .np => .p,
            .l => .nl,
            .nl => .l,
            .le => .nle,
            .nle => .le,
        };
    }

    /// Get the name of this condition code.
    pub fn name(self: CC) []const u8 {
        return switch (self) {
            .o => "o",
            .no => "no",
            .b => "b",
            .nb => "nb",
            .z => "z",
            .nz => "nz",
            .be => "be",
            .nbe => "nbe",
            .s => "s",
            .ns => "ns",
            .p => "p",
            .np => "np",
            .l => "l",
            .nl => "nl",
            .le => "le",
            .nle => "nle",
        };
    }

    /// Convert from IntCC (integer comparison result).
    pub fn fromIntCC(intcc: IntCC) CC {
        return switch (intcc) {
            .eq => .z,
            .ne => .nz,
            .sge => .nl,
            .sgt => .nle,
            .sle => .le,
            .slt => .l,
            .uge => .nb,
            .ugt => .nbe,
            .ule => .be,
            .ult => .b,
        };
    }
};

/// Integer comparison conditions (before lowering to x64 CC).
pub const IntCC = enum {
    eq,
    ne,
    sge,
    sgt,
    sle,
    slt,
    uge,
    ugt,
    ule,
    ult,
};

/// Floating-point comparison conditions.
pub const FloatCC = enum {
    eq,
    lt,
    le,
    unordered,
    ne,
    unordered_or_ge,
    unordered_or_gt,
    ordered,
};

//=============================================================================
// Floating-point comparison immediate
//=============================================================================

/// Immediate for floating-point comparison instructions (e.g., cmpps/cmppd).
/// This is different from CC which uses EFLAGS.
pub const FcmpImm = enum(u8) {
    /// Equal.
    eq = 0x00,
    /// Less than.
    lt = 0x01,
    /// Less than or equal.
    le = 0x02,
    /// Unordered (either operand is NaN).
    unordered = 0x03,
    /// Not equal.
    ne = 0x04,
    /// Unordered or greater than or equal.
    unordered_or_ge = 0x05,
    /// Unordered or greater than.
    unordered_or_gt = 0x06,
    /// Ordered (neither operand is NaN).
    ordered = 0x07,

    /// Get the encoding.
    pub fn encode(self: FcmpImm) u8 {
        return @intFromEnum(self);
    }

    /// Convert from FloatCC.
    pub fn fromFloatCC(fcc: FloatCC) FcmpImm {
        return switch (fcc) {
            .eq => .eq,
            .lt => .lt,
            .le => .le,
            .unordered => .unordered,
            .ne => .ne,
            .unordered_or_ge => .unordered_or_ge,
            .unordered_or_gt => .unordered_or_gt,
            .ordered => .ordered,
        };
    }
};

//=============================================================================
// Rounding mode immediate
//=============================================================================

/// Rounding mode for rounding instructions (e.g., roundss/roundsd).
/// These values go in bits 0-1 of the immediate.
pub const RoundImm = enum(u8) {
    /// Round to nearest (ties to even).
    round_nearest = 0x00,
    /// Round toward negative infinity.
    round_down = 0x01,
    /// Round toward positive infinity.
    round_up = 0x02,
    /// Round toward zero (truncate).
    round_zero = 0x03,

    /// Get the encoding.
    pub fn encode(self: RoundImm) u8 {
        return @intFromEnum(self);
    }
};

//=============================================================================
// Operand size
//=============================================================================

/// The size of an operand in bits.
pub const OperandSize = enum {
    /// 8-bit.
    size8,
    /// 16-bit.
    size16,
    /// 32-bit.
    size32,
    /// 64-bit.
    size64,

    /// Create from byte count.
    pub fn fromBytes(num_bytes: u32) OperandSize {
        return switch (num_bytes) {
            1 => .size8,
            2 => .size16,
            4 => .size32,
            8 => .size64,
            else => unreachable,
        };
    }

    /// Create from a type, using the lane size for vectors.
    pub fn fromTy(ty: Type) OperandSize {
        return fromBytes(ty.laneBytes());
    }

    /// Check if this size is one of the allowed sizes.
    pub fn isOneOf(self: OperandSize, sizes: []const OperandSize) bool {
        for (sizes) |s| {
            if (self == s) return true;
        }
        return false;
    }

    /// Get the size in bytes.
    pub fn toBytes(self: OperandSize) u8 {
        return switch (self) {
            .size8 => 1,
            .size16 => 2,
            .size32 => 4,
            .size64 => 8,
        };
    }

    /// Get the size in bits.
    pub fn toBits(self: OperandSize) u8 {
        return self.toBytes() * 8;
    }

    /// Is this 8-bit?
    pub fn is8(self: OperandSize) bool {
        return self == .size8;
    }

    /// Is this 16-bit?
    pub fn is16(self: OperandSize) bool {
        return self == .size16;
    }

    /// Is this 32-bit?
    pub fn is32(self: OperandSize) bool {
        return self == .size32;
    }

    /// Is this 64-bit?
    pub fn is64(self: OperandSize) bool {
        return self == .size64;
    }
};

//=============================================================================
// Type stub
//=============================================================================

pub const Type = struct {
    kind: Kind,

    pub const Kind = enum(u8) {
        i8,
        i16,
        i32,
        i64,
        i128,
        f16,
        f32,
        f64,
        f128,
        i8x16,
        i16x8,
        i32x4,
        i64x2,
        f32x4,
        f64x2,
    };

    pub const @"i8" = Type{ .kind = .i8 };
    pub const @"i16" = Type{ .kind = .i16 };
    pub const @"i32" = Type{ .kind = .i32 };
    pub const @"i64" = Type{ .kind = .i64 };
    pub const @"i128" = Type{ .kind = .i128 };
    pub const @"f16" = Type{ .kind = .f16 };
    pub const @"f32" = Type{ .kind = .f32 };
    pub const @"f64" = Type{ .kind = .f64 };
    pub const @"f128" = Type{ .kind = .f128 };
    pub const @"i8x16" = Type{ .kind = .i8x16 };
    pub const @"i16x8" = Type{ .kind = .i16x8 };
    pub const @"i32x4" = Type{ .kind = .i32x4 };
    pub const @"i64x2" = Type{ .kind = .i64x2 };
    pub const @"f32x4" = Type{ .kind = .f32x4 };
    pub const @"f64x2" = Type{ .kind = .f64x2 };

    pub fn bits(self: Type) usize {
        return switch (self.kind) {
            .i8 => 8,
            .i16, .f16 => 16,
            .i32, .f32 => 32,
            .i64, .f64 => 64,
            .i128, .f128, .i8x16, .i16x8, .i32x4, .i64x2, .f32x4, .f64x2 => 128,
        };
    }

    pub fn bytes(self: Type) u32 {
        return @intCast(self.bits() / 8);
    }

    /// Get the lane size in bytes for vectors, or the full size for scalars.
    pub fn laneBytes(self: Type) u32 {
        return switch (self.kind) {
            .i8x16 => 1,
            .i16x8 => 2,
            .i32x4, .f32x4 => 4,
            .i64x2, .f64x2 => 8,
            else => self.bytes(),
        };
    }

    pub fn isVector(self: Type) bool {
        return switch (self.kind) {
            .i8x16, .i16x8, .i32x4, .i64x2, .f32x4, .f64x2 => true,
            else => false,
        };
    }

    pub fn isFloat(self: Type) bool {
        return switch (self.kind) {
            .f16, .f32, .f64, .f128, .f32x4, .f64x2 => true,
            else => false,
        };
    }

    pub fn isInt(self: Type) bool {
        return !self.isFloat() and !self.isVector();
    }
};

//=============================================================================
// Shift kind for shift instructions
//=============================================================================

/// Shift operation kind.
pub const ShiftKind = enum(u8) {
    /// Shift left logical.
    shl = 4,
    /// Shift right logical.
    shr = 5,
    /// Shift right arithmetic.
    sar = 7,
    /// Rotate left.
    rol = 0,
    /// Rotate right.
    ror = 1,

    /// Get the encoding (bits 3-5 of the ModR/M reg field).
    pub fn enc(self: ShiftKind) u8 {
        return @intFromEnum(self);
    }
};

//=============================================================================
// Call argument list
//=============================================================================

pub const CallArgPair = struct {
    vreg: Reg,
    preg: PReg,
};

pub const CallArgList = std.ArrayListUnmanaged(CallArgPair);

//=============================================================================
// Tests
//=============================================================================

test "Reg class" {
    const testing = std.testing;
    const int_preg = PReg.init(0, .int);
    const int_reg = Reg.fromPReg(int_preg);
    try testing.expectEqual(RegClass.int, int_reg.class());

    const float_preg = PReg.init(0, .float);
    const float_reg = Reg.fromPReg(float_preg);
    try testing.expectEqual(RegClass.float, float_reg.class());
}

test "Gpr creation" {
    const testing = std.testing;
    const int_preg = PReg.init(0, .int);
    const int_reg = Reg.fromPReg(int_preg);
    const gpr = Gpr.new(int_reg);
    try testing.expect(gpr != null);

    const float_preg = PReg.init(0, .float);
    const float_reg = Reg.fromPReg(float_preg);
    const not_gpr = Gpr.new(float_reg);
    try testing.expect(not_gpr == null);
}

test "Xmm creation" {
    const testing = std.testing;
    const float_preg = PReg.init(0, .float);
    const float_reg = Reg.fromPReg(float_preg);
    const xmm = Xmm.new(float_reg);
    try testing.expect(xmm != null);

    const int_preg = PReg.init(0, .int);
    const int_reg = Reg.fromPReg(int_preg);
    const not_xmm = Xmm.new(int_reg);
    try testing.expect(not_xmm == null);
}

test "CC inversion" {
    const testing = std.testing;
    try testing.expectEqual(CC.no, CC.o.invert());
    try testing.expectEqual(CC.o, CC.no.invert());
    try testing.expectEqual(CC.nz, CC.z.invert());
    try testing.expectEqual(CC.z, CC.nz.invert());
    try testing.expectEqual(CC.nl, CC.l.invert());
    try testing.expectEqual(CC.l, CC.nl.invert());
}

test "CC encoding" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 0), CC.o.getEnc());
    try testing.expectEqual(@as(u8, 4), CC.z.getEnc());
    try testing.expectEqual(@as(u8, 12), CC.l.getEnc());
}

test "OperandSize" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 1), OperandSize.size8.toBytes());
    try testing.expectEqual(@as(u8, 2), OperandSize.size16.toBytes());
    try testing.expectEqual(@as(u8, 4), OperandSize.size32.toBytes());
    try testing.expectEqual(@as(u8, 8), OperandSize.size64.toBytes());
    try testing.expectEqual(@as(u8, 64), OperandSize.size64.toBits());
}

test "OperandSize fromBytes" {
    const testing = std.testing;
    try testing.expectEqual(OperandSize.size8, OperandSize.fromBytes(1));
    try testing.expectEqual(OperandSize.size16, OperandSize.fromBytes(2));
    try testing.expectEqual(OperandSize.size32, OperandSize.fromBytes(4));
    try testing.expectEqual(OperandSize.size64, OperandSize.fromBytes(8));
}

test "ExtMode fromBits" {
    const testing = std.testing;
    try testing.expectEqual(ExtMode.bl, ExtMode.fromBits(8, 32));
    try testing.expectEqual(ExtMode.bq, ExtMode.fromBits(8, 64));
    try testing.expectEqual(ExtMode.wl, ExtMode.fromBits(16, 32));
    try testing.expectEqual(ExtMode.wq, ExtMode.fromBits(16, 64));
    try testing.expectEqual(ExtMode.lq, ExtMode.fromBits(32, 64));
    try testing.expect(ExtMode.fromBits(32, 32) == null);
}

test "FcmpImm encoding" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 0x00), FcmpImm.eq.encode());
    try testing.expectEqual(@as(u8, 0x01), FcmpImm.lt.encode());
    try testing.expectEqual(@as(u8, 0x04), FcmpImm.ne.encode());
}

test "RoundImm encoding" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 0x00), RoundImm.round_nearest.encode());
    try testing.expectEqual(@as(u8, 0x03), RoundImm.round_zero.encode());
}

test "Amode offset" {
    const testing = std.testing;
    const base = Reg.fromPReg(PReg.init(0, .int));
    const amode = Amode.immReg(100, base);
    const offset_amode = amode.offset(50);
    switch (offset_amode) {
        .imm_reg => |ir| try testing.expectEqual(@as(i32, 150), ir.simm32),
        else => unreachable,
    }
}

test "Type properties" {
    const testing = std.testing;
    try testing.expectEqual(@as(usize, 8), Type.i8.bits());
    try testing.expectEqual(@as(usize, 64), Type.i64.bits());
    try testing.expectEqual(@as(usize, 128), Type.i8x16.bits());
    try testing.expect(Type.f32.isFloat());
    try testing.expect(!Type.i32.isFloat());
    try testing.expect(Type.i8x16.isVector());
    try testing.expect(!Type.i64.isVector());
}

test "ShiftKind encoding" {
    const testing = std.testing;
    try testing.expectEqual(@as(u8, 4), ShiftKind.shl.enc());
    try testing.expectEqual(@as(u8, 5), ShiftKind.shr.enc());
    try testing.expectEqual(@as(u8, 7), ShiftKind.sar.enc());
}
