//! Register allocator core types.
//!
//! Ported from regalloc2's `src/lib.rs` and `src/index.rs`.
//!
//! This module defines the fundamental types used throughout the register
//! allocator: blocks, instructions, physical registers, virtual registers,
//! and spill slots.

const std = @import("std");

//=============================================================================
// Block - Basic block index
//=============================================================================

/// A basic block index.
/// Block indices must start at 0 and be numbered sequentially.
pub const Block = struct {
    index: u32,

    pub const INVALID: u32 = std.math.maxInt(u32);

    /// Create a new Block from an index.
    pub inline fn new(i: usize) Block {
        return .{ .index = @intCast(i) };
    }

    /// Get the block index.
    pub inline fn idx(self: Block) usize {
        std.debug.assert(self.isValid());
        return self.index;
    }


    /// Create an invalid block.
    pub inline fn invalid() Block {
        return .{ .index = INVALID };
    }

    /// Check if this block is invalid.
    pub inline fn isInvalid(self: Block) bool {
        return self.index == INVALID;
    }

    /// Check if this block is valid.
    pub inline fn isValid(self: Block) bool {
        return self.index != INVALID;
    }

    /// Get the next block.
    pub inline fn next(self: Block) Block {
        std.debug.assert(self.isValid());
        return .{ .index = self.index + 1 };
    }

    /// Get the previous block.
    pub inline fn prev(self: Block) Block {
        std.debug.assert(self.isValid());
        return .{ .index = self.index - 1 };
    }

    /// Get the raw u32 value.
    pub inline fn rawU32(self: Block) u32 {
        return self.index;
    }

    pub fn eql(self: Block, other: Block) bool {
        return self.index == other.index;
    }

    pub fn order(self: Block, other: Block) std.math.Order {
        return std.math.order(self.index, other.index);
    }
};

//=============================================================================
// Inst - Instruction index
//=============================================================================

/// An instruction index.
/// Instruction indices must start at 0 and be numbered sequentially.
pub const Inst = struct {
    index: u32,

    pub const INVALID: u32 = std.math.maxInt(u32);

    /// Create a new Inst from an index.
    pub inline fn new(i: usize) Inst {
        return .{ .index = @intCast(i) };
    }

    /// Get the instruction index.
    pub inline fn idx(self: Inst) usize {
        std.debug.assert(self.isValid());
        return self.index;
    }

    /// Compare instructions for ordering (for VCode compatibility).
    pub inline fn lessThan(self: Inst, other: Inst) bool {
        return self.index < other.index;
    }

    /// Create an invalid instruction.
    pub inline fn invalid() Inst {
        return .{ .index = INVALID };
    }

    /// Check if this instruction is invalid.
    pub inline fn isInvalid(self: Inst) bool {
        return self.index == INVALID;
    }

    /// Check if this instruction is valid.
    pub inline fn isValid(self: Inst) bool {
        return self.index != INVALID;
    }

    /// Get the next instruction.
    pub inline fn next(self: Inst) Inst {
        std.debug.assert(self.isValid());
        return .{ .index = self.index + 1 };
    }

    /// Get the previous instruction.
    pub inline fn prev(self: Inst) Inst {
        std.debug.assert(self.isValid());
        return .{ .index = self.index - 1 };
    }

    /// Get the raw u32 value.
    pub inline fn rawU32(self: Inst) u32 {
        return self.index;
    }

    pub fn eql(self: Inst, other: Inst) bool {
        return self.index == other.index;
    }

    pub fn order(self: Inst, other: Inst) std.math.Order {
        return std.math.order(self.index, other.index);
    }
};

//=============================================================================
// InstRange - Range of instructions
//=============================================================================

/// A range of instructions [from, to).
/// The range is half-open: includes `from`, excludes `to`.
pub const InstRange = struct {
    from: Inst,
    to: Inst,

    /// Create a new instruction range.
    pub fn new(from: Inst, to: Inst) InstRange {
        std.debug.assert(from.index <= to.index);
        return .{ .from = from, .to = to };
    }

    /// Get the first instruction in the range.
    pub fn first(self: InstRange) Inst {
        std.debug.assert(self.len() > 0);
        return self.from;
    }

    /// Get the last instruction in the range.
    pub fn last(self: InstRange) Inst {
        std.debug.assert(self.len() > 0);
        return self.to.prev();
    }

    /// Get the range without the first instruction.
    pub fn rest(self: InstRange) InstRange {
        std.debug.assert(self.len() > 0);
        return InstRange.new(self.from.next(), self.to);
    }

    /// Get the number of instructions in the range.
    pub fn len(self: InstRange) usize {
        return self.to.index - self.from.index;
    }

    /// Check if the range is empty.
    pub fn isEmpty(self: InstRange) bool {
        return self.len() == 0;
    }

    /// Check if the range contains an instruction.
    pub fn contains(self: InstRange, inst: Inst) bool {
        return inst.index >= self.from.index and inst.index < self.to.index;
    }

    /// Iterate over instructions in the range.
    pub fn iter(self: InstRange) InstRangeIterator {
        return .{ .current = self.from.index, .end = self.to.index };
    }

    /// Iterate over instructions in reverse order.
    /// Ported from regalloc2's InstRange::rev_iter()
    pub fn reverseIter(self: InstRange) InstRangeReverseIterator {
        return .{
            .current = if (self.to.index > 0) self.to.index - 1 else 0,
            .start = self.from.index,
            .done = self.isEmpty(),
        };
    }
};

pub const InstRangeIterator = struct {
    current: u32,
    end: u32,

    pub fn next(self: *InstRangeIterator) ?Inst {
        if (self.current >= self.end) return null;
        const inst = Inst{ .index = self.current };
        self.current += 1;
        return inst;
    }
};

/// Reverse iterator for instruction ranges.
/// Ported from regalloc2's InstRange reverse iteration support.
pub const InstRangeReverseIterator = struct {
    current: u32,
    start: u32,
    done: bool,

    pub fn next(self: *InstRangeReverseIterator) ?Inst {
        if (self.done) return null;
        const inst = Inst{ .index = self.current };
        if (self.current == self.start) {
            self.done = true;
        } else {
            self.current -= 1;
        }
        return inst;
    }
};

//=============================================================================
// RegClass - Register class
//=============================================================================

/// Register classes.
/// Every value has a "register class", which is like a type at the
/// register-allocator level. Every register must belong to only one class.
pub const RegClass = enum(u2) {
    /// Integer/general-purpose registers.
    int = 0,
    /// Floating-point registers.
    float = 1,
    /// Vector registers (if separate from float).
    vector = 2,
};

//=============================================================================
// PReg - Physical register
//=============================================================================

/// A physical register.
///
/// Contains a hardware encoding (0-63) and a register class.
/// The `hw_enc` field is in a logically separate index space per class;
/// Int register 0 is different than Float register 0.
pub const PReg = struct {
    bits: u8,

    pub const MAX_BITS: usize = 6;
    pub const MAX: usize = (1 << MAX_BITS) - 1; // 63
    pub const NUM_INDEX: usize = 1 << (MAX_BITS + 2); // 256 (including class bits)
    pub const INVALID_BITS: u8 = (@as(u8, @intFromEnum(RegClass.int)) << MAX_BITS) | @as(u8, MAX);

    /// Create a new PReg. The `hw_enc` must be <= 63.
    pub inline fn new(hw_enc: usize, reg_class: RegClass) PReg {
        std.debug.assert(hw_enc <= MAX);
        return .{
            .bits = (@as(u8, @intFromEnum(reg_class)) << MAX_BITS) | @as(u8, @intCast(hw_enc)),
        };
    }

    /// Alias for new() - for API compatibility.
    pub inline fn init(hw_enc: u8, reg_class: RegClass) PReg {
        return new(hw_enc, reg_class);
    }

    /// Get the physical register number (hardware encoding).
    pub inline fn hwEnc(self: PReg) usize {
        return self.bits & MAX;
    }

    /// Get the register class.
    pub inline fn class(self: PReg) RegClass {
        return @enumFromInt((self.bits >> MAX_BITS) & 0b11);
    }

    /// Get a unified index across all register classes.
    /// This allows indexing into arrays that track all physical registers.
    pub inline fn index(self: PReg) usize {
        return self.bits;
    }

    /// Construct a PReg from a unified index.
    pub inline fn fromIndex(idx: usize) PReg {
        return .{ .bits = @intCast(idx & (NUM_INDEX - 1)) };
    }

    /// Return the "invalid PReg", used for initialization.
    pub inline fn invalid() PReg {
        return .{ .bits = INVALID_BITS };
    }

    /// Return a valid PReg or null if invalid.
    pub inline fn asValid(self: PReg) ?PReg {
        if (self.bits == INVALID_BITS) return null;
        return self;
    }

    /// Check if this is the invalid PReg.
    pub inline fn isInvalid(self: PReg) bool {
        return self.bits == INVALID_BITS;
    }

    /// Check if this is a valid PReg.
    pub inline fn isValid(self: PReg) bool {
        return self.bits != INVALID_BITS;
    }

    pub fn eql(self: PReg, other: PReg) bool {
        return self.bits == other.bits;
    }

    pub fn format(
        self: PReg,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const class_char: u8 = switch (self.class()) {
            .int => 'i',
            .float => 'f',
            .vector => 'v',
        };
        try writer.print("p{d}{c}", .{ self.hwEnc(), class_char });
    }
};

//=============================================================================
// PRegSet - Set of physical registers
//=============================================================================

/// A set of physical registers, implemented as a bitset.
/// Guaranteed to have constant, small size.
pub const PRegSet = struct {
    pub const BITS_PER_ELEMENT: usize = 64;
    pub const LEN: usize = (PReg.NUM_INDEX + BITS_PER_ELEMENT - 1) / BITS_PER_ELEMENT;

    bits: [LEN]u64,

    /// Create an empty set.
    pub fn empty() PRegSet {
        return .{ .bits = [_]u64{0} ** LEN };
    }

    /// Split a register index into array index and bit position.
    fn splitIndex(reg: PReg) struct { idx: usize, bit: u6 } {
        const i = reg.index();
        return .{
            .idx = i >> 6, // i / 64
            .bit = @intCast(i & 63), // i % 64
        };
    }

    /// Check if the set contains a register.
    pub fn contains(self: PRegSet, reg: PReg) bool {
        const s = splitIndex(reg);
        return (self.bits[s.idx] & (@as(u64, 1) << s.bit)) != 0;
    }

    /// Add a register to the set (immutable).
    pub fn with(self: PRegSet, reg: PReg) PRegSet {
        var result = self;
        const s = splitIndex(reg);
        result.bits[s.idx] |= (@as(u64, 1) << s.bit);
        return result;
    }

    /// Add a register to the set (mutable).
    pub fn add(self: *PRegSet, reg: PReg) void {
        const s = splitIndex(reg);
        self.bits[s.idx] |= (@as(u64, 1) << s.bit);
    }

    /// Remove a register from the set.
    pub fn remove(self: *PRegSet, reg: PReg) void {
        const s = splitIndex(reg);
        self.bits[s.idx] &= ~(@as(u64, 1) << s.bit);
    }

    /// Union this set with another.
    pub fn unionFrom(self: *PRegSet, other: PRegSet) void {
        for (0..LEN) |i| {
            self.bits[i] |= other.bits[i];
        }
    }

    /// Intersect this set with another.
    pub fn intersectFrom(self: *PRegSet, other: PRegSet) void {
        for (0..LEN) |i| {
            self.bits[i] &= other.bits[i];
        }
    }

    /// Return the inverse of this set.
    pub fn invert(self: PRegSet) PRegSet {
        var result: PRegSet = undefined;
        for (0..LEN) |i| {
            result.bits[i] = ~self.bits[i];
        }
        return result;
    }

    /// Check if the set is empty for a given register class.
    pub fn isEmptyForClass(self: PRegSet, class: RegClass) bool {
        return self.bits[@intFromEnum(class)] == 0;
    }

    /// Count the number of registers in the set.
    pub fn count(self: PRegSet) u32 {
        var total: u32 = 0;
        for (self.bits) |b| {
            total += @popCount(b);
        }
        return total;
    }

    /// Check if the set is completely empty.
    pub fn isEmpty(self: PRegSet) bool {
        for (self.bits) |b| {
            if (b != 0) return false;
        }
        return true;
    }

    /// Iterate over registers in the set.
    /// Ported from regalloc2's PRegSet::iter()
    pub fn iter(self: *const PRegSet) PRegSetIterator {
        return .{ .bits = self.bits, .current_idx = 0 };
    }

    /// Alias for iter() - matches naming used in some call sites.
    pub fn iterate(self: PRegSet) PRegSetIterator {
        return .{ .bits = self.bits, .current_idx = 0 };
    }

    /// Union of two sets.
    pub fn unionWith(self: PRegSet, other: PRegSet) PRegSet {
        var result = self;
        result.unionFrom(other);
        return result;
    }

    /// Intersection of two sets.
    pub fn intersectWith(self: PRegSet, other: PRegSet) PRegSet {
        var result = self;
        result.intersectFrom(other);
        return result;
    }

    /// Find the maximum register index in the set.
    /// Returns null if the set is empty.
    pub fn maxPreg(self: PRegSet) ?PReg {
        // Search from highest to lowest
        var i: usize = LEN;
        while (i > 0) {
            i -= 1;
            if (self.bits[i] != 0) {
                // Find highest set bit: 63 - leading zeros
                const bit: u6 = @intCast(63 - @clz(self.bits[i]));
                const index: usize = @as(usize, bit) + i * 64;
                return PReg.fromIndex(index);
            }
        }
        return null;
    }
};

pub const PRegSetIterator = struct {
    bits: [PRegSet.LEN]u64,
    current_idx: usize,

    pub fn next(self: *PRegSetIterator) ?PReg {
        while (self.current_idx < PRegSet.LEN) {
            if (self.bits[self.current_idx] != 0) {
                const bit: u6 = @intCast(@ctz(self.bits[self.current_idx]));
                self.bits[self.current_idx] &= ~(@as(u64, 1) << bit);
                const index: usize = @as(usize, bit) + self.current_idx * 64;
                return PReg.fromIndex(index);
            }
            self.current_idx += 1;
        }
        return null;
    }
};

//=============================================================================
// VReg - Virtual register
//=============================================================================

/// A virtual register.
///
/// A virtual register corresponds to an SSA value. All dataflow in the
/// input program is specified via flow through virtual registers.
pub const VReg = struct {
    bits: u32,

    pub const MAX_BITS: usize = 21;
    pub const MAX: usize = (1 << MAX_BITS) - 1; // ~2 million

    /// Create a new VReg.
    pub inline fn new(vreg_num: usize, reg_class: RegClass) VReg {
        std.debug.assert(vreg_num <= MAX);
        return .{
            .bits = (@as(u32, @intCast(vreg_num)) << 2) | @intFromEnum(reg_class),
        };
    }

    /// Alias for new() - for API compatibility.
    pub inline fn init(vreg_num: u32, reg_class: RegClass) VReg {
        return new(vreg_num, reg_class);
    }

    /// Get the virtual register number.
    pub inline fn vreg(self: VReg) usize {
        return self.bits >> 2;
    }

    /// Get the register class.
    pub inline fn class(self: VReg) RegClass {
        return @enumFromInt(@as(u2, @intCast(self.bits & 0b11)));
    }

    /// Create an invalid VReg.
    pub inline fn invalid() VReg {
        return VReg.new(MAX, .int);
    }

    /// Check if this is the invalid VReg.
    pub inline fn isInvalid(self: VReg) bool {
        return self.vreg() == MAX;
    }

    /// Check if this is a valid VReg.
    pub inline fn isValid(self: VReg) bool {
        return self.vreg() != MAX;
    }

    /// Get the raw bits.
    pub inline fn rawBits(self: VReg) usize {
        return self.bits;
    }

    pub fn eql(self: VReg, other: VReg) bool {
        return self.bits == other.bits;
    }

    pub fn format(
        self: VReg,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("v{d}", .{self.vreg()});
    }
};

//=============================================================================
// SpillSlot - Stack spill slot
//=============================================================================

/// A spill slot in the stack frame.
///
/// The allocator is responsible for allocating indices in this space
/// and reports how many slots were used when allocation completes.
pub const SpillSlot = struct {
    bits: u32,

    pub const MAX: usize = (1 << 24) - 1;
    pub const INVALID_BITS: u32 = 0xffff_ffff;

    /// Create a new SpillSlot.
    pub inline fn new(slot: usize) SpillSlot {
        std.debug.assert(slot <= MAX);
        return .{ .bits = @intCast(slot) };
    }

    /// Get the spillslot index.
    pub inline fn index(self: SpillSlot) usize {
        return self.bits & 0x00ffffff;
    }

    /// Get the spillslot `offset` slots away.
    pub inline fn plus(self: SpillSlot, offset: usize) SpillSlot {
        return SpillSlot.new(self.index() + offset);
    }

    /// Get the invalid spillslot.
    pub inline fn invalid() SpillSlot {
        return .{ .bits = INVALID_BITS };
    }

    /// Check if this is the invalid spillslot.
    pub inline fn isInvalid(self: SpillSlot) bool {
        return self.bits == INVALID_BITS;
    }

    /// Check if this is a valid spillslot.
    pub inline fn isValid(self: SpillSlot) bool {
        return self.bits != INVALID_BITS;
    }

    pub fn eql(self: SpillSlot, other: SpillSlot) bool {
        return self.bits == other.bits;
    }

    pub fn format(
        self: SpillSlot,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("stack{d}", .{self.index()});
    }
};

//=============================================================================
// Tests
//=============================================================================

test "Block creation and operations" {
    const b0 = Block.new(0);
    const b1 = Block.new(1);

    try std.testing.expectEqual(@as(usize, 0), b0.idx());
    try std.testing.expectEqual(@as(usize, 1), b1.idx());
    try std.testing.expect(b0.isValid());
    try std.testing.expect(!b0.isInvalid());

    const next = b0.next();
    try std.testing.expectEqual(@as(usize, 1), next.idx());

    const inv = Block.invalid();
    try std.testing.expect(inv.isInvalid());
    try std.testing.expect(!inv.isValid());
}

test "Inst creation and operations" {
    const inst0 = Inst.new(0);
    const inst5 = Inst.new(5);

    try std.testing.expectEqual(@as(usize, 0), inst0.idx());
    try std.testing.expectEqual(@as(usize, 5), inst5.idx());
    try std.testing.expect(inst0.isValid());

    const prev = inst5.prev();
    try std.testing.expectEqual(@as(usize, 4), prev.idx());

    const inv = Inst.invalid();
    try std.testing.expect(inv.isInvalid());
}

test "InstRange iteration" {
    const range = InstRange.new(Inst.new(0), Inst.new(5));

    try std.testing.expectEqual(@as(usize, 5), range.len());
    try std.testing.expectEqual(@as(usize, 0), range.first().idx());
    try std.testing.expectEqual(@as(usize, 4), range.last().idx());

    var count: usize = 0;
    var it = range.iter();
    while (it.next()) |_| {
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 5), count);

    const empty_range = InstRange.new(Inst.new(0), Inst.new(0));
    try std.testing.expectEqual(@as(usize, 0), empty_range.len());
    try std.testing.expect(empty_range.isEmpty());
}

test "PReg creation and encoding" {
    const r0_int = PReg.new(0, .int);
    const r5_float = PReg.new(5, .float);
    const r15_int = PReg.new(15, .int);

    try std.testing.expectEqual(@as(usize, 0), r0_int.hwEnc());
    try std.testing.expectEqual(RegClass.int, r0_int.class());

    try std.testing.expectEqual(@as(usize, 5), r5_float.hwEnc());
    try std.testing.expectEqual(RegClass.float, r5_float.class());

    try std.testing.expectEqual(@as(usize, 15), r15_int.hwEnc());
    try std.testing.expectEqual(RegClass.int, r15_int.class());

    // Different classes have different indices
    try std.testing.expect(r0_int.index() != r5_float.index());

    // fromIndex round-trip
    const idx = r5_float.index();
    const restored = PReg.fromIndex(idx);
    try std.testing.expect(r5_float.eql(restored));

    // Invalid PReg
    const inv = PReg.invalid();
    try std.testing.expect(inv.isInvalid());
    try std.testing.expect(!inv.isValid());
    try std.testing.expect(inv.asValid() == null);
}

test "PRegSet operations" {
    var set = PRegSet.empty();
    try std.testing.expect(set.isEmpty());

    const r0 = PReg.new(0, .int);
    const r1 = PReg.new(1, .int);
    const r5 = PReg.new(5, .float);

    set.add(r0);
    try std.testing.expect(set.contains(r0));
    try std.testing.expect(!set.contains(r1));
    try std.testing.expect(!set.isEmpty());

    set.add(r1);
    set.add(r5);
    try std.testing.expectEqual(@as(u32, 3), set.count());

    set.remove(r1);
    try std.testing.expect(!set.contains(r1));
    try std.testing.expectEqual(@as(u32, 2), set.count());

    // Immutable with()
    const set2 = set.with(r1);
    try std.testing.expect(!set.contains(r1));
    try std.testing.expect(set2.contains(r1));
}

test "PRegSet iteration" {
    var set = PRegSet.empty();
    const r0 = PReg.new(0, .int);
    const r5 = PReg.new(5, .int);
    const r10 = PReg.new(10, .float);

    set.add(r0);
    set.add(r5);
    set.add(r10);

    var count: u32 = 0;
    var it = set.iter();
    while (it.next()) |_| {
        count += 1;
    }
    try std.testing.expectEqual(@as(u32, 3), count);
}

test "VReg creation" {
    const v0 = VReg.new(0, .int);
    const v100 = VReg.new(100, .float);

    try std.testing.expectEqual(@as(usize, 0), v0.vreg());
    try std.testing.expectEqual(RegClass.int, v0.class());

    try std.testing.expectEqual(@as(usize, 100), v100.vreg());
    try std.testing.expectEqual(RegClass.float, v100.class());

    const inv = VReg.invalid();
    try std.testing.expect(inv.isInvalid());
}

test "SpillSlot creation" {
    const s0 = SpillSlot.new(0);
    const s5 = SpillSlot.new(5);

    try std.testing.expectEqual(@as(usize, 0), s0.index());
    try std.testing.expectEqual(@as(usize, 5), s5.index());

    const s7 = s5.plus(2);
    try std.testing.expectEqual(@as(usize, 7), s7.index());

    const inv = SpillSlot.invalid();
    try std.testing.expect(inv.isInvalid());
}
