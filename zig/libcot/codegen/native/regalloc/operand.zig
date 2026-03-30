//! Operand types for the register allocator.
//!
//! Ported from regalloc2's `src/lib.rs`.
//!
//! This module defines operands (references to vregs in instructions),
//! allocations (the result of register allocation), program points,
//! and edits (moves inserted by the allocator).

const std = @import("std");
const index = @import("index.zig");
const PReg = index.PReg;
const VReg = index.VReg;
const SpillSlot = index.SpillSlot;
const RegClass = index.RegClass;
const Inst = index.Inst;

//=============================================================================
// OperandConstraint - Constraint on where a vreg must be placed
//=============================================================================

/// An OperandConstraint specifies where a vreg's value must be placed
/// at a particular reference to that vreg via an Operand.
///
/// The constraint may be loose ("any register of a given class") or
/// very specific ("this particular physical register").
pub const OperandConstraint = union(enum) {
    /// Any location is fine (register or stack slot).
    any,
    /// Operand must be in a register. Register is read-only for Uses.
    reg,
    /// Operand must be on the stack.
    stack,
    /// Operand must be in a fixed register.
    fixed_reg: PReg,
    /// On defs only: reuse a use's register.
    /// The usize is the index of the use operand to reuse.
    reuse: usize,
    /// Operand must be in a specific range of registers [0, n).
    /// `n` must be a power of two and <= 64.
    limit: usize,

    pub fn format(
        self: OperandConstraint,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .any => try writer.writeAll("any"),
            .reg => try writer.writeAll("reg"),
            .stack => try writer.writeAll("stack"),
            .fixed_reg => |preg| {
                try writer.writeAll("fixed(");
                try preg.format("", .{}, writer);
                try writer.writeAll(")");
            },
            .reuse => |idx| try writer.print("reuse({d})", .{idx}),
            .limit => |max| try writer.print("limit(0..={d})", .{max - 1}),
        }
    }
};

//=============================================================================
// OperandKind - Whether operand reads or writes
//=============================================================================

/// The "kind" of the operand: whether it reads a vreg (Use) or writes
/// a vreg (Def).
pub const OperandKind = enum(u1) {
    /// Write to the vreg (definition).
    def = 0,
    /// Read from the vreg (use).
    use = 1,
};

//=============================================================================
// OperandPos - Position within instruction
//=============================================================================

/// The "position" of the operand: where it has its read/write effects.
///
/// These are positions "in" the instruction, and "early" and "late" are
/// relative to the instruction's main effect or computation.
///
/// The allocator assumes that the instruction:
/// (i) performs all reads and writes of "early" operands,
/// (ii) does its work, and
/// (iii) performs all reads and writes of its "late" operands.
pub const OperandPos = enum(u1) {
    early = 0,
    late = 1,
};

//=============================================================================
// Operand - A reference to a vreg in an instruction
//=============================================================================

/// An Operand encodes everything about a mention of a register in an
/// instruction: virtual register number, and any constraint that applies
/// to the register at this program point.
///
/// Bit-packed into 32 bits:
///   constraint:7 kind:1 pos:1 class:2 vreg:21
///
/// Constraint encoding:
///   - 1xxxxxx => FixedReg(preg), where xxxxxx is hw_enc
///   - 01xxxxx => Reuse(index), where xxxxx is the use index
///   - 001xxxx => Limit(max), where xxxx is log2(max)
///   - 0000000 => Any
///   - 0000001 => Reg
///   - 0000010 => Stack
pub const Operand = struct {
    bits: u32,

    /// Construct a new operand.
    pub fn new(
        vr: VReg,
        constr: OperandConstraint,
        op_kind: OperandKind,
        op_pos: OperandPos,
    ) Operand {
        const constraint_field: u32 = switch (constr) {
            .any => 0,
            .reg => 1,
            .stack => 2,
            .fixed_reg => |preg| blk: {
                std.debug.assert(preg.class() == vr.class());
                break :blk 0b1000000 | @as(u32, @intCast(preg.hwEnc()));
            },
            .reuse => |which| blk: {
                std.debug.assert(which <= 0b11111);
                break :blk 0b0100000 | @as(u32, @intCast(which));
            },
            .limit => |max| blk: {
                std.debug.assert(std.math.isPowerOfTwo(max));
                std.debug.assert(max <= PReg.MAX + 1);
                const log2 = std.math.log2_int(usize, max);
                std.debug.assert(log2 <= 0b1111);
                break :blk 0b0010000 | @as(u32, @intCast(log2));
            },
        };
        const class_field: u32 = @intFromEnum(vr.class());
        const pos_field: u32 = @intFromEnum(op_pos);
        const kind_field: u32 = @intFromEnum(op_kind);

        return .{
            .bits = @as(u32, @intCast(vr.vreg())) |
                (class_field << 21) |
                (pos_field << 23) |
                (kind_field << 24) |
                (constraint_field << 25),
        };
    }

    /// Create an Operand for a use that must be in a register (early position).
    pub fn regUse(vr: VReg) Operand {
        return Operand.new(vr, .reg, .use, .early);
    }

    /// Create an Operand for a use that must be in a register at late position.
    /// Use this when the value must not conflict with any results.
    pub fn regUseAtEnd(vr: VReg) Operand {
        return Operand.new(vr, .reg, .use, .late);
    }

    /// Create an Operand for a def that must be in a register (late position).
    pub fn regDef(vr: VReg) Operand {
        return Operand.new(vr, .reg, .def, .late);
    }

    /// Create an Operand for a def that must be in a register at early position.
    /// Use this for temporaries that must not conflict with any inputs.
    pub fn regDefAtStart(vr: VReg) Operand {
        return Operand.new(vr, .reg, .def, .early);
    }

    /// Create an Operand for a temporary register within the instruction.
    /// This is an alias for regDefAtStart.
    pub fn regTemp(vr: VReg) Operand {
        return regDefAtStart(vr);
    }

    /// Create an Operand for a def that reuses an input register.
    /// `idx` is the index of the use operand to reuse.
    pub fn regReuseDef(vr: VReg, reuse_idx: usize) Operand {
        return Operand.new(vr, .{ .reuse = reuse_idx }, .def, .late);
    }

    /// Create an Operand for a use in a fixed register.
    pub fn regFixedUse(vr: VReg, preg: PReg) Operand {
        return Operand.new(vr, .{ .fixed_reg = preg }, .use, .early);
    }

    /// Create an Operand for a def in a fixed register.
    pub fn regFixedDef(vr: VReg, preg: PReg) Operand {
        return Operand.new(vr, .{ .fixed_reg = preg }, .def, .late);
    }

    /// Create an Operand for a use in a fixed register at late position.
    pub fn regFixedUseAtEnd(vr: VReg, preg: PReg) Operand {
        return Operand.new(vr, .{ .fixed_reg = preg }, .use, .late);
    }

    /// Create an Operand for a def in a fixed register at early position.
    pub fn regFixedDefAtStart(vr: VReg, preg: PReg) Operand {
        return Operand.new(vr, .{ .fixed_reg = preg }, .def, .early);
    }

    /// Create an Operand for a use with no constraints.
    pub fn anyUse(vr: VReg) Operand {
        return Operand.new(vr, .any, .use, .early);
    }

    /// Create an Operand for a def with no constraints.
    pub fn anyDef(vr: VReg) Operand {
        return Operand.new(vr, .any, .def, .late);
    }

    /// Create an Operand that always results in an assignment to the given
    /// fixed preg, WITHOUT tracking liveranges. Must only be used for
    /// non-allocatable registers.
    pub fn fixedNonAllocatable(preg: PReg) Operand {
        return Operand.new(
            VReg.new(VReg.MAX, preg.class()),
            .{ .fixed_reg = preg },
            .use,
            .early,
        );
    }

    /// Get the virtual register.
    pub fn vreg(self: Operand) VReg {
        const vreg_idx = self.bits & VReg.MAX;
        return VReg.new(vreg_idx, self.class());
    }

    /// Get the register class.
    pub fn class(self: Operand) RegClass {
        const class_field = (self.bits >> 21) & 3;
        return @enumFromInt(@as(u2, @intCast(class_field)));
    }

    /// Get the operand kind (def or use).
    pub fn kind(self: Operand) OperandKind {
        const kind_field = (self.bits >> 24) & 1;
        return @enumFromInt(@as(u1, @intCast(kind_field)));
    }

    /// Get the operand position (early or late).
    pub fn pos(self: Operand) OperandPos {
        const pos_field = (self.bits >> 23) & 1;
        return @enumFromInt(@as(u1, @intCast(pos_field)));
    }

    /// Get the operand constraint.
    pub fn constraint(self: Operand) OperandConstraint {
        const constraint_field = (self.bits >> 25) & 0b1111111;
        if (constraint_field & 0b1000000 != 0) {
            return .{ .fixed_reg = PReg.new(constraint_field & 0b0111111, self.class()) };
        } else if (constraint_field & 0b0100000 != 0) {
            return .{ .reuse = constraint_field & 0b0011111 };
        } else if (constraint_field & 0b0010000 != 0) {
            return .{ .limit = @as(usize, 1) << @intCast(constraint_field & 0b0001111) };
        } else {
            return switch (constraint_field) {
                0 => .any,
                1 => .reg,
                2 => .stack,
                else => unreachable,
            };
        }
    }

    /// If this operand is for a fixed non-allocatable register, return the preg.
    pub fn asFixedNonAllocatable(self: Operand) ?PReg {
        switch (self.constraint()) {
            .fixed_reg => |preg| {
                if (self.vreg().vreg() == VReg.MAX) {
                    return preg;
                }
                return null;
            },
            else => return null,
        }
    }

    /// Get the raw 32-bit encoding.
    pub fn rawBits(self: Operand) u32 {
        return self.bits;
    }

    /// Construct from raw bits.
    pub fn fromBits(bits: u32) Operand {
        std.debug.assert(bits >> 29 <= 4);
        return .{ .bits = bits };
    }

    pub fn eql(self: Operand, other: Operand) bool {
        return self.bits == other.bits;
    }

    pub fn format(
        self: Operand,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.asFixedNonAllocatable()) |preg| {
            try writer.print("Fixed: {}", .{preg});
            return;
        }
        switch (self.kind()) {
            .def => {
                if (self.pos() == .late) {
                    try writer.writeAll("Def");
                } else {
                    try writer.writeAll("Def@Early");
                }
            },
            .use => {
                if (self.pos() == .early) {
                    try writer.writeAll("Use");
                } else {
                    try writer.writeAll("Use@Late");
                }
            },
        }
        const class_char: u8 = switch (self.class()) {
            .int => 'i',
            .float => 'f',
            .vector => 'v',
        };
        try writer.writeAll(": ");
        try self.vreg().format("", .{}, writer);
        try writer.print("{c} ", .{class_char});
        try self.constraint().format("", .{}, writer);
    }
};

//=============================================================================
// AllocationKind - What kind of allocation (none, reg, stack)
//=============================================================================

/// An allocation is one of three kinds: none, register, or stack.
pub const AllocationKind = enum(u3) {
    none = 0,
    reg = 1,
    stack = 2,
};

//=============================================================================
// Allocation - The result of register allocation for an operand
//=============================================================================

/// An Allocation represents the end result of regalloc for an Operand.
///
/// Bit-packed into 32 bits:
///   kind:3 unused:1 index:28
pub const Allocation = struct {
    bits: u32,

    /// Construct a new Allocation.
    fn init(alloc_kind: AllocationKind, idx: usize) Allocation {
        std.debug.assert(idx < (1 << 28));
        return .{
            .bits = (@as(u32, @intFromEnum(alloc_kind)) << 29) | @as(u32, @intCast(idx)),
        };
    }

    /// Get the "none" allocation.
    pub fn none() Allocation {
        return init(.none, 0);
    }

    /// Create an allocation into a register.
    pub fn reg(preg: PReg) Allocation {
        return init(.reg, preg.index());
    }

    /// Create an allocation into a spillslot.
    pub fn stack(slot: SpillSlot) Allocation {
        return init(.stack, slot.bits);
    }

    /// Get the allocation kind.
    pub fn kind(self: Allocation) AllocationKind {
        return @enumFromInt(@as(u3, @intCast((self.bits >> 29) & 7)));
    }

    /// Is the allocation "none"?
    pub fn isNone(self: Allocation) bool {
        return self.kind() == .none;
    }

    /// Is the allocation not "none"?
    pub fn isSome(self: Allocation) bool {
        return self.kind() != .none;
    }

    /// Is the allocation a register?
    pub fn isReg(self: Allocation) bool {
        return self.kind() == .reg;
    }

    /// Is the allocation on the stack?
    pub fn isStack(self: Allocation) bool {
        return self.kind() == .stack;
    }

    /// Get the index of the spillslot or register.
    pub fn index(self: Allocation) usize {
        return self.bits & ((1 << 28) - 1);
    }

    /// Get the allocation as a physical register, if any.
    pub fn asReg(self: Allocation) ?PReg {
        if (self.kind() == .reg) {
            return PReg.fromIndex(self.index());
        }
        return null;
    }

    /// Get the allocation as a spillslot, if any.
    pub fn asStack(self: Allocation) ?SpillSlot {
        if (self.kind() == .stack) {
            return .{ .bits = @intCast(self.index()) };
        }
        return null;
    }

    /// Get the raw bits.
    pub fn rawBits(self: Allocation) u32 {
        return self.bits;
    }

    /// Construct from raw bits.
    pub fn fromBits(bits: u32) Allocation {
        return .{ .bits = bits };
    }

    pub fn eql(self: Allocation, other: Allocation) bool {
        return self.bits == other.bits;
    }

    pub fn format(
        self: Allocation,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self.kind()) {
            .none => try writer.writeAll("none"),
            .reg => try self.asReg().?.format("", .{}, writer),
            .stack => try self.asStack().?.format("", .{}, writer),
        }
    }
};

//=============================================================================
// InstPosition - Before or after an instruction
//=============================================================================

/// A position before or after an instruction at which we can insert edits.
///
/// This differs from OperandPos in that the former describes a constraint
/// on an operand, while this describes a program point for insertions.
pub const InstPosition = enum(u1) {
    before = 0,
    after = 1,
};

//=============================================================================
// ProgPoint - A program point (instruction + position)
//=============================================================================

/// A program point: a single point before or after a given instruction.
///
/// Bit-packed: inst:31 pos:1
pub const ProgPoint = struct {
    bits: u32,

    /// Create a new ProgPoint.
    pub fn new(instr: Inst, position: InstPosition) ProgPoint {
        return .{
            .bits = (instr.index << 1) | @intFromEnum(position),
        };
    }

    /// Create a ProgPoint before the given instruction.
    pub fn before(instr: Inst) ProgPoint {
        return new(instr, .before);
    }

    /// Create a ProgPoint after the given instruction.
    pub fn after(instr: Inst) ProgPoint {
        return new(instr, .after);
    }

    /// Get the instruction.
    pub fn inst(self: ProgPoint) Inst {
        // Use arithmetic shift to preserve invalid (-1)
        const signed: i32 = @bitCast(self.bits);
        return .{ .index = @bitCast(@as(i32, signed >> 1)) };
    }

    /// Get the position.
    pub fn pos(self: ProgPoint) InstPosition {
        return @enumFromInt(@as(u1, @intCast(self.bits & 1)));
    }

    /// Get the next program point.
    pub fn nextPoint(self: ProgPoint) ProgPoint {
        return .{ .bits = self.bits + 1 };
    }

    /// Get the previous program point.
    pub fn prevPoint(self: ProgPoint) ProgPoint {
        return .{ .bits = self.bits - 1 };
    }

    /// Convert to raw index.
    pub fn toIndex(self: ProgPoint) u32 {
        return self.bits;
    }

    /// Construct from raw index.
    pub fn fromIndex(idx: u32) ProgPoint {
        return .{ .bits = idx };
    }

    /// Get the invalid program point.
    pub fn invalid() ProgPoint {
        return before(Inst.invalid());
    }

    pub fn eql(self: ProgPoint, other: ProgPoint) bool {
        return self.bits == other.bits;
    }

    pub fn order(self: ProgPoint, other: ProgPoint) std.math.Order {
        return std.math.order(self.bits, other.bits);
    }

    pub fn format(
        self: ProgPoint,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const pos_str: []const u8 = switch (self.pos()) {
            .before => "-pre",
            .after => "-post",
        };
        try writer.print("progpoint{d}{s}", .{ self.inst().index, pos_str });
    }
};

//=============================================================================
// Edit - An instruction to insert moves
//=============================================================================

/// An instruction to insert into the program to perform data movement.
pub const Edit = union(enum) {
    /// Move one allocation to another.
    /// Each allocation may be a register or stack slot.
    /// Stack-to-stack moves will never be generated.
    move: struct {
        from: Allocation,
        to: Allocation,
    },

    pub fn format(
        self: Edit,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .move => |m| {
                try writer.writeAll("move ");
                try m.from.format("", .{}, writer);
                try writer.writeAll(" -> ");
                try m.to.format("", .{}, writer);
            },
        }
    }
};

//=============================================================================
// Tests
//=============================================================================

test "OperandConstraint formatting" {
    const any_constr: OperandConstraint = .any;
    const reg_constr: OperandConstraint = .reg;
    const fixed: OperandConstraint = .{ .fixed_reg = PReg.new(5, .int) };
    const reuse_constraint: OperandConstraint = .{ .reuse = 0 };
    const limit_constraint: OperandConstraint = .{ .limit = 8 };

    var buf: [64]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    try any_constr.format("", .{}, writer);
    try std.testing.expectEqualStrings("any", fbs.getWritten());
    fbs.reset();

    try reg_constr.format("", .{}, writer);
    try std.testing.expectEqualStrings("reg", fbs.getWritten());
    fbs.reset();

    try fixed.format("", .{}, writer);
    try std.testing.expectEqualStrings("fixed(p5i)", fbs.getWritten());
    fbs.reset();

    try reuse_constraint.format("", .{}, writer);
    try std.testing.expectEqualStrings("reuse(0)", fbs.getWritten());
    fbs.reset();

    try limit_constraint.format("", .{}, writer);
    try std.testing.expectEqualStrings("limit(0..=7)", fbs.getWritten());
}

test "Operand creation and accessors" {
    const v0 = VReg.new(0, .int);
    const v1 = VReg.new(1, .float);

    // regUse
    const use = Operand.regUse(v0);
    try std.testing.expect(use.vreg().eql(v0));
    try std.testing.expectEqual(RegClass.int, use.class());
    try std.testing.expectEqual(OperandKind.use, use.kind());
    try std.testing.expectEqual(OperandPos.early, use.pos());
    try std.testing.expectEqual(OperandConstraint.reg, use.constraint());

    // regDef
    const def = Operand.regDef(v1);
    try std.testing.expect(def.vreg().eql(v1));
    try std.testing.expectEqual(RegClass.float, def.class());
    try std.testing.expectEqual(OperandKind.def, def.kind());
    try std.testing.expectEqual(OperandPos.late, def.pos());

    // fixedReg constraint
    const preg = PReg.new(10, .int);
    const fixed_use = Operand.regFixedUse(v0, preg);
    switch (fixed_use.constraint()) {
        .fixed_reg => |p| try std.testing.expect(p.eql(preg)),
        else => return error.TestUnexpectedResult,
    }

    // reuse constraint
    const reuse_def = Operand.regReuseDef(v0, 2);
    switch (reuse_def.constraint()) {
        .reuse => |idx| try std.testing.expectEqual(@as(usize, 2), idx),
        else => return error.TestUnexpectedResult,
    }
}

test "Operand bit packing round-trip" {
    const v0 = VReg.new(12345, .int);
    const preg = PReg.new(7, .int);

    const ops = [_]Operand{
        Operand.regUse(v0),
        Operand.regDef(v0),
        Operand.regUseAtEnd(v0),
        Operand.regDefAtStart(v0),
        Operand.regFixedUse(v0, preg),
        Operand.regReuseDef(v0, 3),
        Operand.anyUse(v0),
        Operand.anyDef(v0),
    };

    for (ops) |op| {
        const bits = op.rawBits();
        const restored = Operand.fromBits(bits);
        try std.testing.expect(op.eql(restored));
        try std.testing.expect(op.vreg().eql(restored.vreg()));
        try std.testing.expectEqual(op.kind(), restored.kind());
        try std.testing.expectEqual(op.pos(), restored.pos());
    }
}

test "Allocation creation and accessors" {
    // none
    const none_alloc = Allocation.none();
    try std.testing.expect(none_alloc.isNone());
    try std.testing.expect(!none_alloc.isSome());

    // reg
    const preg = PReg.new(5, .int);
    const reg_alloc = Allocation.reg(preg);
    try std.testing.expect(reg_alloc.isReg());
    try std.testing.expect(!reg_alloc.isStack());
    try std.testing.expect(reg_alloc.asReg().?.eql(preg));
    try std.testing.expect(reg_alloc.asStack() == null);

    // stack
    const slot = SpillSlot.new(42);
    const stack_alloc = Allocation.stack(slot);
    try std.testing.expect(stack_alloc.isStack());
    try std.testing.expect(!stack_alloc.isReg());
    try std.testing.expect(stack_alloc.asStack().?.eql(slot));
    try std.testing.expect(stack_alloc.asReg() == null);
}

test "ProgPoint creation and accessors" {
    const inst5 = Inst.new(5);

    const before_pt = ProgPoint.before(inst5);
    try std.testing.expectEqual(@as(u32, 5), before_pt.inst().index);
    try std.testing.expectEqual(InstPosition.before, before_pt.pos());

    const after_pt = ProgPoint.after(inst5);
    try std.testing.expectEqual(@as(u32, 5), after_pt.inst().index);
    try std.testing.expectEqual(InstPosition.after, after_pt.pos());

    // Ordering: before < after for same instruction
    try std.testing.expectEqual(std.math.Order.lt, before_pt.order(after_pt));

    // next/prev
    const next_pt = before_pt.nextPoint();
    try std.testing.expect(next_pt.eql(after_pt));

    const prev_pt = after_pt.prevPoint();
    try std.testing.expect(prev_pt.eql(before_pt));
}

test "Edit formatting" {
    const from_alloc = Allocation.reg(PReg.new(0, .int));
    const to_alloc = Allocation.reg(PReg.new(1, .int));
    const edit_move = Edit{ .move = .{ .from = from_alloc, .to = to_alloc } };

    var buf: [64]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try edit_move.format("", .{}, fbs.writer());
    try std.testing.expectEqualStrings("move p0i -> p1i", fbs.getWritten());
}

test "fixedNonAllocatable" {
    const preg = PReg.new(31, .int);
    const op = Operand.fixedNonAllocatable(preg);

    const fixed = op.asFixedNonAllocatable();
    try std.testing.expect(fixed != null);
    try std.testing.expect(fixed.?.eql(preg));

    // Regular fixed operand should not be detected as non-allocatable
    const v0 = VReg.new(0, .int);
    const regular_fixed = Operand.regFixedUse(v0, preg);
    try std.testing.expect(regular_fixed.asFixedNonAllocatable() == null);
}
