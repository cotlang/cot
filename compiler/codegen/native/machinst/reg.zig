//! Register definitions for machine code generation.
//!
//! Port of cranelift/codegen/src/machinst/reg.rs
//!
//! Following Cranelift's architecture, this module IMPORTS types from
//! regalloc (the equivalent of regalloc2) and adds machine-specific wrappers.
//! Cranelift's machinst does NOT redefine regalloc2 types - it imports them.
//!
//! Type ownership (matching Cranelift/regalloc2):
//!   - regalloc/index.zig OWNS: RegClass, PReg, PRegSet, VReg, SpillSlot
//!   - regalloc/operand.zig OWNS: OperandKind, OperandPos, OperandConstraint, Operand, Allocation, etc.
//!   - This file (machinst/reg.zig) OWNS: Reg, RealReg, VirtualReg, Writable, OperandCollector

const std = @import("std");

// =============================================================================
// Import types from regalloc (equivalent to Cranelift importing from regalloc2)
// =============================================================================

const regalloc_index = @import("../regalloc/index.zig");
const regalloc_operand = @import("../regalloc/operand.zig");

// Re-export regalloc types for convenience (users of machinst/reg.zig can use these)
pub const RegClass = regalloc_index.RegClass;
pub const PReg = regalloc_index.PReg;
pub const PRegSet = regalloc_index.PRegSet;
pub const PRegSetIterator = regalloc_index.PRegSetIterator;
pub const VReg = regalloc_index.VReg;
pub const SpillSlot = regalloc_index.SpillSlot;
pub const Block = regalloc_index.Block;
pub const Inst = regalloc_index.Inst;
pub const InstRange = regalloc_index.InstRange;

// Re-export operand types
pub const OperandKind = regalloc_operand.OperandKind;
pub const OperandPos = regalloc_operand.OperandPos;
pub const OperandConstraint = regalloc_operand.OperandConstraint;
pub const Operand = regalloc_operand.Operand;
pub const Allocation = regalloc_operand.Allocation;
pub const AllocationKind = regalloc_operand.AllocationKind;
pub const ProgPoint = regalloc_operand.ProgPoint;
pub const InstPosition = regalloc_operand.InstPosition;
pub const Edit = regalloc_operand.Edit;

// =============================================================================
// Constants
// =============================================================================

/// The first 192 vregs (64 int, 64 float, 64 vec) are "pinned" to
/// physical registers. These must not be passed into the regalloc,
/// but they are used to represent physical registers in the same
/// Reg type post-regalloc.
pub const PINNED_VREGS: usize = 192;

/// Registers per class for pinned vregs
pub const REGS_PER_CLASS: usize = 64;

/// Spillslot bit in Reg encoding
const REG_SPILLSLOT_BIT: u32 = 0x8000_0000;
const REG_SPILLSLOT_MASK: u32 = ~REG_SPILLSLOT_BIT;

// =============================================================================
// Reg (Unified Register) - machinst-specific wrapper
// =============================================================================

/// A register named in an instruction. This register can be a virtual
/// register, a fixed physical register, or a named spillslot (after
/// regalloc).
///
/// This is a machinst-specific type that wraps VReg with additional
/// encoding for physical registers (via pinned vregs) and spillslots.
pub const Reg = struct {
    bits: u32,

    const Self = @This();

    /// Create a Reg from a VReg.
    pub fn fromVReg(vreg: VReg) Self {
        return .{ .bits = @intCast(vreg.rawBits()) };
    }

    /// Create an invalid Reg.
    pub fn invalid() Self {
        return .{ .bits = @intCast(VReg.invalid().rawBits()) };
    }

    /// Create a Reg from a PReg (physical register).
    /// Uses the "pinned vreg" encoding where preg index maps to vreg index.
    pub fn fromPReg(preg: PReg) Self {
        // Physical registers are encoded as pinned vregs.
        // The pinned vreg index encodes: class * 64 + hw_enc
        const pinned_idx = @as(usize, @intFromEnum(preg.class())) * REGS_PER_CLASS + preg.hwEnc();
        const vreg = VReg.new(pinned_idx, preg.class());
        return .{ .bits = @intCast(vreg.rawBits()) };
    }

    /// Create a Reg from a SpillSlot.
    pub fn fromSpillSlot(slot: SpillSlot) Self {
        return .{ .bits = REG_SPILLSLOT_BIT | @as(u32, @intCast(slot.index())) };
    }

    /// Get the physical register, if this is one.
    pub fn toRealReg(self: Self) ?RealReg {
        if (self.isSpillSlot()) return null;
        const vreg = self.toVRegRaw();
        if (vreg.vreg() < PINNED_VREGS) {
            return RealReg{ .preg = pinnedVRegToPReg(vreg).? };
        }
        return null;
    }

    /// Get the virtual register, if this is one.
    pub fn toVirtualReg(self: Self) ?VirtualReg {
        if (self.isSpillSlot()) return null;
        const vreg = self.toVRegRaw();
        if (vreg.vreg() >= PINNED_VREGS) {
            return VirtualReg{ .vreg = vreg };
        }
        return null;
    }

    /// Get the spillslot, if this is one.
    pub fn toSpillSlot(self: Self) ?SpillSlot {
        if ((self.bits & REG_SPILLSLOT_BIT) != 0) {
            return SpillSlot.new(self.bits & REG_SPILLSLOT_MASK);
        }
        return null;
    }

    /// Get the raw VReg encoding.
    fn toVRegRaw(self: Self) VReg {
        return VReg{ .bits = self.bits };
    }

    /// Get as VReg (for register allocation).
    pub fn toVReg(self: Self) VReg {
        std.debug.assert(!self.isSpillSlot());
        return self.toVRegRaw();
    }

    /// Get the register class.
    pub fn class(self: Self) RegClass {
        std.debug.assert(!self.isSpillSlot());
        return self.toVRegRaw().class();
    }

    /// Is this a physical register?
    pub fn isReal(self: Self) bool {
        return self.toRealReg() != null;
    }

    /// Is this a virtual register?
    pub fn isVirtual(self: Self) bool {
        return self.toVirtualReg() != null;
    }

    /// Is this a spillslot?
    pub fn isSpillSlot(self: Self) bool {
        return (self.bits & REG_SPILLSLOT_BIT) != 0;
    }

    /// Get the hardware encoding. Only valid for physical registers.
    pub fn hwEnc(self: Self) u8 {
        const rreg = self.toRealReg() orelse @panic("hwEnc called on non-physical register");
        return @intCast(rreg.preg.hwEnc());
    }

    /// Format for display.
    pub fn format(
        self: Self,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.toVRegRaw().isInvalid()) {
            try writer.writeAll("<invalid>");
        } else if (self.toSpillSlot()) |slot| {
            try writer.print("slot{d}", .{slot.index()});
        } else if (self.toRealReg()) |rreg| {
            try rreg.preg.format("", .{}, writer);
        } else if (self.toVirtualReg()) |vreg| {
            try vreg.vreg.format("", .{}, writer);
        } else {
            unreachable;
        }
    }
};

// =============================================================================
// RealReg (Physical Register wrapper) - machinst-specific
// =============================================================================

/// A real (physical) register.
pub const RealReg = struct {
    preg: PReg,

    const Self = @This();

    /// Get the register class.
    pub fn class(self: Self) RegClass {
        return self.preg.class();
    }

    /// Get the hardware encoding.
    pub fn hwEnc(self: Self) usize {
        return self.preg.hwEnc();
    }

    /// Convert to a Reg.
    pub fn toReg(self: Self) Reg {
        return Reg.fromPReg(self.preg);
    }
};

// =============================================================================
// VirtualReg (Virtual Register wrapper) - machinst-specific
// =============================================================================

/// A virtual register.
pub const VirtualReg = struct {
    vreg: VReg,

    const Self = @This();

    /// Get the register class.
    pub fn class(self: Self) RegClass {
        return self.vreg.class();
    }

    /// Get the vreg index.
    pub fn index(self: Self) usize {
        return self.vreg.vreg();
    }

    /// Convert to a Reg.
    pub fn toReg(self: Self) Reg {
        return Reg.fromVReg(self.vreg);
    }

    /// Get the underlying VReg.
    pub fn toVReg(self: Self) VReg {
        return self.vreg;
    }
};

// =============================================================================
// Writable - machinst-specific wrapper for writable registers
// =============================================================================

/// A type wrapper that indicates a register type is writable.
pub fn Writable(comptime T: type) type {
    return struct {
        reg: T,

        const Self = @This();

        /// Create a writable register.
        pub fn fromReg(reg: T) Self {
            return .{ .reg = reg };
        }

        /// Get the underlying register (read-only).
        pub fn toReg(self: Self) T {
            return self.reg;
        }

        /// Get a mutable reference to the register.
        pub fn regMut(self: *Self) *T {
            return &self.reg;
        }

        /// Map the register to another type.
        pub fn map(self: Self, comptime U: type, f: fn (T) U) Writable(U) {
            return Writable(U){ .reg = f(self.reg) };
        }

        /// Create an invalid writable register.
        pub fn invalid() Self {
            return .{ .reg = T.invalid() };
        }
    };
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Convert a PReg to its pinned VReg.
pub fn pregToPinnedVReg(preg: PReg) VReg {
    const pinned_idx = @as(usize, @intFromEnum(preg.class())) * REGS_PER_CLASS + preg.hwEnc();
    return VReg.new(pinned_idx, preg.class());
}

/// Convert a VReg to its pinned PReg, if any.
pub fn pinnedVRegToPReg(vreg: VReg) ?PReg {
    const idx = vreg.vreg();
    if (idx < PINNED_VREGS) {
        const class_idx = idx / REGS_PER_CLASS;
        const hw_enc = idx % REGS_PER_CLASS;
        return PReg.new(hw_enc, @enumFromInt(@as(u2, @intCast(class_idx))));
    }
    return null;
}

/// Get the first available vreg index for user code.
pub fn firstUserVRegIndex() usize {
    return PINNED_VREGS;
}

// =============================================================================
// OperandCollector - machinst-specific operand collection
// =============================================================================

/// An OperandCollector is a wrapper around a Vec of Operands
/// (flattened array for a whole sequence of instructions) that
/// gathers operands from a single instruction and provides the range
/// in the flattened array.
pub fn OperandCollector(comptime Renamer: type) type {
    return struct {
        operands: *std.ArrayListUnmanaged(Operand),
        allocator: std.mem.Allocator,
        clobbers: PRegSet,
        /// The subset of physical registers that are allocatable.
        allocatable: PRegSet,
        renamer: Renamer,

        const Self = @This();

        /// Start gathering operands into one flattened operand array.
        pub fn init(
            operands: *std.ArrayListUnmanaged(Operand),
            allocator: std.mem.Allocator,
            allocatable: PRegSet,
            renamer: Renamer,
        ) Self {
            return .{
                .operands = operands,
                .allocator = allocator,
                .clobbers = PRegSet.empty(),
                .allocatable = allocatable,
                .renamer = renamer,
            };
        }

        /// Finish the operand collection and return the tuple giving the
        /// range of indices in the flattened operand array, and the
        /// clobber set.
        pub fn finish(self: Self) struct { end: usize, clobbers: PRegSet } {
            return .{ .end = self.operands.items.len, .clobbers = self.clobbers };
        }

        // OperandVisitor implementation

        pub fn addOperand(
            self: *Self,
            reg: *Reg,
            constraint: OperandConstraint,
            kind: OperandKind,
            pos: OperandPos,
        ) void {
            std.debug.assert(!reg.isSpillSlot());
            // Apply renamer to the vreg
            const vreg_in = reg.toVReg();
            const renamed = self.renamer.rename(vreg_in);
            reg.* = Reg.fromVReg(renamed);
            self.operands.append(self.allocator, Operand.new(
                renamed,
                constraint,
                kind,
                pos,
            )) catch @panic("OOM in OperandCollector.addOperand");
        }

        pub fn debugAssertIsAllocatablePReg(self: Self, reg: PReg, expected: bool) void {
            std.debug.assert(self.allocatable.contains(reg) == expected);
        }

        pub fn regClobbers(self: *Self, regs: PRegSet) void {
            self.clobbers.unionFrom(regs);
        }

        // OperandVisitorImpl default implementations

        /// Add a use of a fixed, nonallocatable physical register.
        pub fn regFixedNonallocatable(self: *Self, preg: PReg) void {
            self.debugAssertIsAllocatablePReg(preg, false);
            // Since this operand does not participate in register allocation,
            // there's nothing to do here.
        }

        /// Add a register use, at the start of the instruction (`Before` position).
        pub fn regUse(self: *Self, reg: *Reg) void {
            self.regMaybeFixed(reg, .use, .early);
        }

        /// Add a register use, at the end of the instruction (`After` position).
        pub fn regLateUse(self: *Self, reg: *Reg) void {
            self.regMaybeFixed(reg, .use, .late);
        }

        /// Add a register def, at the end of the instruction (`After`
        /// position). Use only when this def will be written after all
        /// uses are read.
        pub fn regDef(self: *Self, reg: *Writable(Reg)) void {
            self.regMaybeFixed(reg.regMut(), .def, .late);
        }

        /// Add a register "early def", which logically occurs at the
        /// beginning of the instruction, alongside all uses. Use this
        /// when the def may be written before all uses are read; the
        /// regalloc will ensure that it does not overwrite any uses.
        pub fn regEarlyDef(self: *Self, reg: *Writable(Reg)) void {
            self.regMaybeFixed(reg.regMut(), .def, .early);
        }

        /// Add a register "fixed use", which ties a vreg to a particular
        /// RealReg at the end of the instruction.
        pub fn regFixedLateUse(self: *Self, reg: *Reg, rreg: Reg) void {
            self.regFixed(reg, rreg, .use, .late);
        }

        /// Add a register "fixed use", which ties a vreg to a particular
        /// RealReg at this point.
        pub fn regFixedUse(self: *Self, reg: *Reg, rreg: Reg) void {
            self.regFixed(reg, rreg, .use, .early);
        }

        /// Add a register "fixed def", which ties a vreg to a particular
        /// RealReg at this point.
        pub fn regFixedDef(self: *Self, reg: *Writable(Reg), rreg: Reg) void {
            self.regFixed(reg.regMut(), rreg, .def, .late);
        }

        /// Add an operand tying a virtual register to a physical register.
        pub fn regFixed(self: *Self, reg: *Reg, rreg: Reg, kind: OperandKind, pos: OperandPos) void {
            std.debug.assert(reg.isVirtual());
            const real_reg = rreg.toRealReg() orelse @panic("fixed reg is not a RealReg");
            self.debugAssertIsAllocatablePReg(real_reg.preg, true);
            const constraint = OperandConstraint{ .fixed_reg = real_reg.preg };
            self.addOperand(reg, constraint, kind, pos);
        }

        /// Add an operand which might already be a physical register.
        pub fn regMaybeFixed(self: *Self, reg: *Reg, kind: OperandKind, pos: OperandPos) void {
            if (reg.toRealReg()) |rreg| {
                self.regFixedNonallocatable(rreg.preg);
            } else {
                std.debug.assert(reg.isVirtual());
                self.addOperand(reg, .reg, kind, pos);
            }
        }

        /// Add a register def that reuses an earlier use-operand's
        /// allocation. The index of that earlier operand (relative to the
        /// current instruction's start of operands) must be known.
        pub fn regReuseDef(self: *Self, reg: *Writable(Reg), idx: usize) void {
            const r = reg.regMut();
            if (r.toRealReg()) |rreg| {
                // In some cases we see real register arguments to a reg_reuse_def
                // constraint. We assume the creator knows what they're doing
                // here, though we do also require that the real register be a
                // fixed-nonallocatable register.
                self.regFixedNonallocatable(rreg.preg);
            } else {
                std.debug.assert(r.isVirtual());
                // The operand we're reusing must not be fixed-nonallocatable, as
                // that would imply that the register has been allocated to a
                // virtual register.
                const constraint = OperandConstraint{ .reuse = idx };
                self.addOperand(r, constraint, .def, .late);
            }
        }

        /// Add a def that can be allocated to either a register or a
        /// spillslot, at the end of the instruction (`After`
        /// position). Use only when this def will be written after all
        /// uses are read.
        pub fn anyDef(self: *Self, reg: *Writable(Reg)) void {
            self.addOperand(reg.regMut(), .any, .def, .late);
        }

        /// Add a use that can be allocated to either a register or a
        /// spillslot, at the end of the instruction (`After` position).
        pub fn anyLateUse(self: *Self, reg: *Reg) void {
            self.addOperand(reg, .any, .use, .late);
        }
    };
}

// =============================================================================
// OperandVisitor Interface
// =============================================================================

/// Interface for visiting/collecting operands from instructions.
/// This is the Zig equivalent of Cranelift's OperandVisitor trait.
pub const OperandVisitorVTable = struct {
    addOperandFn: *const fn (ctx: *anyopaque, reg: *Reg, constraint: OperandConstraint, kind: OperandKind, pos: OperandPos) void,
    debugAssertIsAllocatablePRegFn: ?*const fn (ctx: *anyopaque, reg: PReg, expected: bool) void,
    regClobbersFn: ?*const fn (ctx: *anyopaque, regs: PRegSet) void,
};

/// Generic operand visitor that wraps any type implementing the visitor interface.
pub const OperandVisitorGeneric = struct {
    ctx: *anyopaque,
    vtable: *const OperandVisitorVTable,

    const Self = @This();

    pub fn addOperand(self: Self, reg: *Reg, constraint: OperandConstraint, kind: OperandKind, pos: OperandPos) void {
        self.vtable.addOperandFn(self.ctx, reg, constraint, kind, pos);
    }

    pub fn debugAssertIsAllocatablePReg(self: Self, reg: PReg, expected: bool) void {
        if (self.vtable.debugAssertIsAllocatablePRegFn) |f| {
            f(self.ctx, reg, expected);
        }
    }

    pub fn regClobbers(self: Self, regs: PRegSet) void {
        if (self.vtable.regClobbersFn) |f| {
            f(self.ctx, regs);
        }
    }

    // OperandVisitorImpl default implementations

    pub fn regFixedNonallocatable(self: Self, preg: PReg) void {
        self.debugAssertIsAllocatablePReg(preg, false);
    }

    pub fn regUse(self: Self, reg: *Reg) void {
        self.regMaybeFixed(reg, .use, .early);
    }

    pub fn regLateUse(self: Self, reg: *Reg) void {
        self.regMaybeFixed(reg, .use, .late);
    }

    pub fn regDef(self: Self, reg: *Writable(Reg)) void {
        self.regMaybeFixed(reg.regMut(), .def, .late);
    }

    pub fn regEarlyDef(self: Self, reg: *Writable(Reg)) void {
        self.regMaybeFixed(reg.regMut(), .def, .early);
    }

    pub fn regFixedLateUse(self: Self, reg: *Reg, rreg: Reg) void {
        self.regFixed(reg, rreg, .use, .late);
    }

    pub fn regFixedUse(self: Self, reg: *Reg, rreg: Reg) void {
        self.regFixed(reg, rreg, .use, .early);
    }

    pub fn regFixedDef(self: Self, reg: *Writable(Reg), rreg: Reg) void {
        self.regFixed(reg.regMut(), rreg, .def, .late);
    }

    pub fn regFixed(self: Self, reg: *Reg, rreg: Reg, kind: OperandKind, pos: OperandPos) void {
        std.debug.assert(reg.isVirtual());
        const real_reg = rreg.toRealReg() orelse @panic("fixed reg is not a RealReg");
        self.debugAssertIsAllocatablePReg(real_reg.preg, true);
        const constraint = OperandConstraint{ .fixed_reg = real_reg.preg };
        self.addOperand(reg, constraint, kind, pos);
    }

    pub fn regMaybeFixed(self: Self, reg: *Reg, kind: OperandKind, pos: OperandPos) void {
        if (reg.toRealReg()) |rreg| {
            self.regFixedNonallocatable(rreg.preg);
        } else {
            std.debug.assert(reg.isVirtual());
            self.addOperand(reg, .reg, kind, pos);
        }
    }

    pub fn regReuseDef(self: Self, reg: *Writable(Reg), idx: usize) void {
        const r = reg.regMut();
        if (r.toRealReg()) |rreg| {
            self.regFixedNonallocatable(rreg.preg);
        } else {
            std.debug.assert(r.isVirtual());
            const constraint = OperandConstraint{ .reuse = idx };
            self.addOperand(r, constraint, .def, .late);
        }
    }

    pub fn anyDef(self: Self, reg: *Writable(Reg)) void {
        self.addOperand(reg.regMut(), .any, .def, .late);
    }

    pub fn anyLateUse(self: Self, reg: *Reg) void {
        self.addOperand(reg, .any, .use, .late);
    }
};

// =============================================================================
// Identity Renamer
// =============================================================================

/// Identity renamer that passes through vregs unchanged.
pub const IdentityRenamer = struct {
    pub fn rename(_: IdentityRenamer, vreg: VReg) VReg {
        return vreg;
    }
};

// =============================================================================
// PrettyPrint Interface
// =============================================================================

/// Pretty-print part of a disassembly, with knowledge of
/// operand/instruction size, and optionally with regalloc
/// results. This can be used, for example, to print either `rax` or
/// `eax` for the register by those names on x86-64, depending on a
/// 64- or 32-bit context.
pub const PrettyPrintVTable = struct {
    prettyPrintFn: *const fn (ctx: *const anyopaque, size_bytes: u8) []const u8,
};

/// Generic pretty printer interface.
pub const PrettyPrintGeneric = struct {
    ctx: *const anyopaque,
    vtable: *const PrettyPrintVTable,

    const Self = @This();

    pub fn prettyPrint(self: Self, size_bytes: u8) []const u8 {
        return self.vtable.prettyPrintFn(self.ctx, size_bytes);
    }

    pub fn prettyPrintDefault(self: Self) []const u8 {
        return self.prettyPrint(0);
    }
};

// =============================================================================
// Tests
// =============================================================================

test "preg creation and encoding" {
    const testing = std.testing;

    const r0 = PReg.new(0, .int);
    try testing.expectEqual(@as(usize, 0), r0.hwEnc());
    try testing.expectEqual(RegClass.int, r0.class());

    const f5 = PReg.new(5, .float);
    try testing.expectEqual(@as(usize, 5), f5.hwEnc());
    try testing.expectEqual(RegClass.float, f5.class());

    const v31 = PReg.new(31, .vector);
    try testing.expectEqual(@as(usize, 31), v31.hwEnc());
    try testing.expectEqual(RegClass.vector, v31.class());
}

test "preg from/to index" {
    const testing = std.testing;

    // int regs: class 0
    const r0 = PReg.new(0, .int);
    const r15 = PReg.new(15, .int);
    try testing.expectEqual(RegClass.int, r0.class());
    try testing.expectEqual(@as(usize, 0), r0.hwEnc());
    try testing.expectEqual(@as(usize, 15), r15.hwEnc());

    // float regs: class 1
    const f0 = PReg.new(0, .float);
    try testing.expectEqual(RegClass.float, f0.class());
    try testing.expectEqual(@as(usize, 0), f0.hwEnc());

    // vector regs: class 2
    const v0 = PReg.new(0, .vector);
    try testing.expectEqual(RegClass.vector, v0.class());
    try testing.expectEqual(@as(usize, 0), v0.hwEnc());

    // Round-trip through index
    const idx = f0.index();
    const restored = PReg.fromIndex(idx);
    try testing.expect(f0.eql(restored));
}

test "vreg creation" {
    const testing = std.testing;

    const v0 = VReg.new(192, .int);
    try testing.expectEqual(@as(usize, 192), v0.vreg());
    try testing.expectEqual(RegClass.int, v0.class());
    try testing.expect(!v0.isInvalid());

    const invalid_vreg = VReg.invalid();
    try testing.expect(invalid_vreg.isInvalid());
}

test "reg from preg is real" {
    const testing = std.testing;

    const preg = PReg.new(5, .int);
    const reg = Reg.fromPReg(preg);

    try testing.expect(reg.isReal());
    try testing.expect(!reg.isVirtual());
    try testing.expect(!reg.isSpillSlot());

    const rreg = reg.toRealReg().?;
    try testing.expectEqual(@as(usize, 5), rreg.hwEnc());
}

test "reg from vreg is virtual" {
    const testing = std.testing;

    const vreg = VReg.new(200, .int);
    const reg = Reg.fromVReg(vreg);

    try testing.expect(!reg.isReal());
    try testing.expect(reg.isVirtual());
    try testing.expect(!reg.isSpillSlot());

    const vr = reg.toVirtualReg().?;
    try testing.expectEqual(@as(usize, 200), vr.index());
}

test "reg from spillslot" {
    const testing = std.testing;

    const slot = SpillSlot.new(42);
    const reg = Reg.fromSpillSlot(slot);

    try testing.expect(!reg.isReal());
    try testing.expect(!reg.isVirtual());
    try testing.expect(reg.isSpillSlot());

    const s = reg.toSpillSlot().?;
    try testing.expectEqual(@as(usize, 42), s.index());
}

test "writable reg" {
    const testing = std.testing;

    const preg = PReg.new(0, .int);
    const reg = Reg.fromPReg(preg);
    var wreg = Writable(Reg).fromReg(reg);

    try testing.expect(wreg.toReg().isReal());

    // Can modify through regMut
    const new_preg = PReg.new(1, .int);
    wreg.regMut().* = Reg.fromPReg(new_preg);
    try testing.expectEqual(@as(usize, 1), wreg.toReg().toRealReg().?.hwEnc());
}

test "preg set operations" {
    const testing = std.testing;

    var set = PRegSet.empty();
    try testing.expect(set.isEmpty());

    const r0 = PReg.new(0, .int);
    const r5 = PReg.new(5, .int);
    const f0 = PReg.new(0, .float);

    set.add(r0);
    try testing.expect(set.contains(r0));
    try testing.expect(!set.contains(r5));
    try testing.expect(!set.contains(f0));

    set.add(f0);
    try testing.expect(set.contains(f0));

    set.remove(r0);
    try testing.expect(!set.contains(r0));
}

test "pinned vreg to preg conversion" {
    const testing = std.testing;

    // Pinned vreg (< 192) should convert to preg
    // int class 0, hw_enc 5 -> vreg index = 0 * 64 + 5 = 5
    const pinned = VReg.new(5, .int);
    const preg = pinnedVRegToPReg(pinned);
    try testing.expect(preg != null);
    try testing.expectEqual(@as(usize, 5), preg.?.hwEnc());
    try testing.expectEqual(RegClass.int, preg.?.class());

    // Non-pinned vreg (>= 192) should not convert
    const user = VReg.new(200, .int);
    try testing.expect(pinnedVRegToPReg(user) == null);
}

test "operand collector basic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var operands: std.ArrayListUnmanaged(Operand) = .{};
    defer operands.deinit(allocator);

    var collector = OperandCollector(IdentityRenamer).init(
        &operands,
        allocator,
        PRegSet.empty(),
        IdentityRenamer{},
    );

    // Create a virtual register
    var reg = Reg.fromVReg(VReg.new(200, .int));
    collector.addOperand(&reg, .reg, .use, .early);

    const result = collector.finish();
    try testing.expectEqual(@as(usize, 1), result.end);
    try testing.expect(result.clobbers.isEmpty());

    try testing.expectEqual(@as(usize, 1), operands.items.len);
    try testing.expectEqual(OperandKind.use, operands.items[0].kind());
    try testing.expectEqual(OperandPos.early, operands.items[0].pos());
}

test "operand collector with clobbers" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var operands: std.ArrayListUnmanaged(Operand) = .{};
    defer operands.deinit(allocator);

    var collector = OperandCollector(IdentityRenamer).init(
        &operands,
        allocator,
        PRegSet.empty(),
        IdentityRenamer{},
    );

    var clobber_set = PRegSet.empty();
    clobber_set.add(PReg.new(0, .int));
    clobber_set.add(PReg.new(1, .int));
    collector.regClobbers(clobber_set);

    const result = collector.finish();
    try testing.expect(result.clobbers.contains(PReg.new(0, .int)));
    try testing.expect(result.clobbers.contains(PReg.new(1, .int)));
    try testing.expect(!result.clobbers.contains(PReg.new(2, .int)));
}
