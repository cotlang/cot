//! Move insertion for the Ion register allocator.
//!
//! Ported from regalloc2's `src/ion/moves.rs` and `src/ion/redundant_moves.rs`.
//!
//! This module handles the final phase of register allocation:
//! - Applying allocations to instruction operands
//! - Inserting moves at live range boundaries
//! - Resolving parallel moves to sequential moves
//! - Eliminating redundant moves
//!
//! See audit/native/ion_moves_audit.md for full function-by-function mapping.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const ion_data = @import("ion_data.zig");
const cfg_mod = @import("cfg.zig");
const process_mod = @import("process.zig");
const moves_mod = @import("moves.zig");
const output_mod = @import("output.zig");

const Block = index.Block;
const Inst = index.Inst;
const VReg = index.VReg;
const PReg = index.PReg;
const RegClass = index.RegClass;
const SpillSlot = index.SpillSlot;
const Allocation = operand_mod.Allocation;
const ProgPoint = operand_mod.ProgPoint;
const InstPosition = operand_mod.InstPosition;
const Edit = operand_mod.Edit;
const OperandKind = operand_mod.OperandKind;
const OperandConstraint = operand_mod.OperandConstraint;
const CodeRange = ion_data.CodeRange;
const LiveRangeIndex = ion_data.LiveRangeIndex;
const LiveBundleIndex = ion_data.LiveBundleIndex;
const SpillSetIndex = ion_data.SpillSetIndex;
const SpillSlotIndex = ion_data.SpillSlotIndex;
const VRegIndex = ion_data.VRegIndex;
const LiveRange = ion_data.LiveRange;
const LiveBundle = ion_data.LiveBundle;
const LiveRangeListEntry = ion_data.LiveRangeListEntry;
const SpillSet = ion_data.SpillSet;
const SpillSlotData = ion_data.SpillSlotData;
const VRegData = ion_data.VRegData;
const LiveRangeKey = ion_data.LiveRangeKey;
const LiveRangeFlag = ion_data.LiveRangeFlag;
const InsertMovePrio = ion_data.InsertMovePrio;
const InsertedMove = ion_data.InsertedMove;
const InsertedMoves = ion_data.InsertedMoves;
const PosWithPrio = ion_data.PosWithPrio;
const Edits = ion_data.Edits;
const BlockparamIn = ion_data.BlockparamIn;
const BlockparamOut = ion_data.BlockparamOut;
const MultiFixedRegFixup = ion_data.MultiFixedRegFixup;
const FixedRegFixupLevel = ion_data.FixedRegFixupLevel;
const u64Key = ion_data.u64Key;
const CFGInfo = cfg_mod.CFGInfo;
const RegTraversalIter = process_mod.RegTraversalIter;
const Output = output_mod.Output;

//=============================================================================
// RedundantMoveState - State for move tracking
//=============================================================================

pub const RedundantMoveState = union(enum) {
    /// This location holds a copy of another location.
    copy: struct {
        alloc: Allocation,
        vreg: ?VReg,
    },
    /// This location holds the original value of a vreg.
    orig: VReg,
    /// No tracking information.
    none,
};

//=============================================================================
// RedundantMoveAction - Result of processing a move
//=============================================================================

pub const RedundantMoveAction = struct {
    elide: bool,
};

//=============================================================================
// RedundantMoveEliminator - Tracks copy chains to eliminate redundant moves
//=============================================================================

pub const RedundantMoveEliminator = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    allocs: std.AutoHashMapUnmanaged(Allocation, RedundantMoveState),
    reverse_allocs: std.AutoHashMapUnmanaged(Allocation, std.ArrayListUnmanaged(Allocation)),

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .allocs = .{},
            .reverse_allocs = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocs.deinit(self.allocator);
        var iter = self.reverse_allocs.valueIterator();
        while (iter.next()) |list| {
            list.deinit(self.allocator);
        }
        self.reverse_allocs.deinit(self.allocator);
    }

    /// Process a move and determine if it can be elided.
    pub fn processMove(
        self: *Self,
        from: Allocation,
        to: Allocation,
        to_vreg: ?VReg,
    ) !RedundantMoveAction {
        const from_state = self.allocs.get(from) orelse .none;
        const to_state = self.allocs.get(to) orelse .none;

        // Same source and dest with vreg - just update tracking
        if (from.bits == to.bits and to_vreg != null) {
            try self.clearAlloc(to);
            try self.allocs.put(self.allocator, to, .{ .orig = to_vreg.? });
            return .{ .elide = true };
        }

        // Get vreg from source state
        const src_vreg: ?VReg = switch (from_state) {
            .copy => |c| c.vreg,
            .orig => |v| v,
            .none => null,
        };
        const dst_vreg = to_vreg orelse src_vreg;

        // Check if move is redundant
        const elide = switch (from_state) {
            .none => switch (to_state) {
                .copy => |c| c.alloc.bits == from.bits,
                else => false,
            },
            .copy => |c| c.alloc.bits == to.bits,
            .orig => switch (to_state) {
                .copy => |c| c.alloc.bits == from.bits,
                else => false,
            },
        };

        // Invalidate existing copies of `to` if value actually changed
        if (!elide) {
            try self.clearAlloc(to);
        }

        // Set up forward and reverse mapping (don't track stack-to-stack)
        if (from.isReg() or to.isReg()) {
            try self.allocs.put(self.allocator, to, .{
                .copy = .{ .alloc = from, .vreg = dst_vreg },
            });

            const entry = try self.reverse_allocs.getOrPut(self.allocator, from);
            if (!entry.found_existing) {
                entry.value_ptr.* = .{};
            }
            try entry.value_ptr.append(self.allocator, to);
        }

        return .{ .elide = elide };
    }

    /// Clear all tracking state.
    pub fn clear(self: *Self) void {
        self.allocs.clearRetainingCapacity();
        var iter = self.reverse_allocs.valueIterator();
        while (iter.next()) |list| {
            list.clearRetainingCapacity();
        }
    }

    /// Clear tracking for a specific allocation.
    pub fn clearAlloc(self: *Self, alloc: Allocation) !void {
        // Clear all copies that point to this alloc
        if (self.reverse_allocs.getPtr(alloc)) |copies| {
            for (copies.items) |copy_alloc| {
                if (self.allocs.getPtr(copy_alloc)) |val| {
                    switch (val.*) {
                        .copy => |c| {
                            if (c.vreg) |vreg| {
                                val.* = .{ .orig = vreg };
                            } else {
                                val.* = .none;
                            }
                        },
                        else => val.* = .none,
                    }
                }
                _ = self.allocs.remove(copy_alloc);
            }
            copies.clearRetainingCapacity();
        }
        _ = self.allocs.remove(alloc);
    }
};

//=============================================================================
// PrevBuffer - Buffer for tracking previous live range
//=============================================================================

/// Buffered information about the previous liverange that was processed.
/// Used to determine intra-block move sources.
const PrevBuffer = struct {
    prev: ?LiveRangeListEntry,
    prev_ins_idx: usize,
    buffered: ?LiveRangeListEntry,
    buffered_ins_idx: usize,

    fn init(prev_ins_idx: usize) PrevBuffer {
        return .{
            .prev = null,
            .prev_ins_idx = prev_ins_idx,
            .buffered = null,
            .buffered_ins_idx = prev_ins_idx,
        };
    }

    /// Returns the previous LiveRangeListEntry when present.
    fn isValid(self: PrevBuffer) ?LiveRangeListEntry {
        return self.prev;
    }

    /// Get the current blockparam_ins index.
    fn blockparamInsIdx(self: PrevBuffer) usize {
        return self.prev_ins_idx;
    }

    /// Record this index for the next buffer advance.
    fn updateBlockparamInsIdx(self: *PrevBuffer, idx: usize) void {
        self.buffered_ins_idx = idx;
    }

    /// Advance the buffer with a new entry.
    /// Accumulates the longest-lived liverange as the most stable source.
    fn advance(self: *PrevBuffer, current: LiveRangeListEntry) void {
        // Advance prev to buffered if buffered starts before current
        if (self.buffered) |b| {
            if (b.range.from.bits < current.range.from.bits) {
                self.prev = self.buffered;
                self.prev_ins_idx = self.buffered_ins_idx;
            }
        }

        // Update buffered if current ends later
        if (self.buffered) |b| {
            if (b.range.to.bits < current.range.to.bits) {
                self.buffered = current;
            }
        } else {
            self.buffered = current;
        }
    }
};

//=============================================================================
// MoveContext - Context for move insertion
//=============================================================================

pub const MoveContext = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    // === Core data structures ===
    ranges: *std.ArrayListUnmanaged(LiveRange),
    bundles: *std.ArrayListUnmanaged(LiveBundle),
    spillsets: *std.ArrayListUnmanaged(SpillSet),
    spillslots: *std.ArrayListUnmanaged(SpillSlotData),
    vregs: *std.ArrayListUnmanaged(VRegData),

    // === CFG info ===
    cfginfo: *const CFGInfo,

    // === Output ===
    output: *Output,

    // === Block parameters ===
    blockparam_ins: []const BlockparamIn,
    blockparam_outs: []const BlockparamOut,

    // === Fixed-reg fixups ===
    multi_fixed_reg_fixups: *std.ArrayListUnmanaged(MultiFixedRegFixup),

    // === Extra spillslots for scratch ===
    extra_spillslots_by_class: *[3]std.ArrayListUnmanaged(Allocation),

    // === Preferred victim registers for scratch ===
    preferred_victim_by_class: [3]PReg,

    // === Number of spillslots ===
    num_spillslots: *usize,

    pub fn init(
        allocator: std.mem.Allocator,
        ranges: *std.ArrayListUnmanaged(LiveRange),
        bundles: *std.ArrayListUnmanaged(LiveBundle),
        spillsets: *std.ArrayListUnmanaged(SpillSet),
        spillslots: *std.ArrayListUnmanaged(SpillSlotData),
        vregs: *std.ArrayListUnmanaged(VRegData),
        cfginfo: *const CFGInfo,
        output: *Output,
        blockparam_ins: []const BlockparamIn,
        blockparam_outs: []const BlockparamOut,
        multi_fixed_reg_fixups: *std.ArrayListUnmanaged(MultiFixedRegFixup),
        extra_spillslots_by_class: *[3]std.ArrayListUnmanaged(Allocation),
        preferred_victim_by_class: [3]PReg,
        num_spillslots: *usize,
    ) Self {
        return .{
            .allocator = allocator,
            .ranges = ranges,
            .bundles = bundles,
            .spillsets = spillsets,
            .spillslots = spillslots,
            .vregs = vregs,
            .cfginfo = cfginfo,
            .output = output,
            .blockparam_ins = blockparam_ins,
            .blockparam_outs = blockparam_outs,
            .multi_fixed_reg_fixups = multi_fixed_reg_fixups,
            .extra_spillslots_by_class = extra_spillslots_by_class,
            .preferred_victim_by_class = preferred_victim_by_class,
            .num_spillslots = num_spillslots,
        };
    }

    //=========================================================================
    // Block boundary checks
    //=========================================================================

    /// Check if position is at the start of a block.
    pub fn isStartOfBlock(self: *const Self, pos: ProgPoint) bool {
        const block = self.cfginfo.insn_block.items[pos.inst().idx()];
        return pos.bits == self.cfginfo.block_entry.items[block.idx()].bits;
    }

    /// Check if position is at the end of a block.
    pub fn isEndOfBlock(self: *const Self, pos: ProgPoint) bool {
        const block = self.cfginfo.insn_block.items[pos.inst().idx()];
        return pos.bits == self.cfginfo.block_exit.items[block.idx()].bits;
    }

    //=========================================================================
    // Allocation accessors
    //=========================================================================

    /// Get allocation for an instruction slot.
    pub fn getAlloc(self: *const Self, inst: Inst, slot: usize) Allocation {
        const offset = self.output.inst_alloc_offsets.items[inst.idx()];
        return self.output.allocs.items[offset + slot];
    }

    /// Set allocation for an instruction slot.
    pub fn setAlloc(self: *Self, inst: Inst, slot: usize, alloc: Allocation) void {
        const offset = self.output.inst_alloc_offsets.items[inst.idx()];
        self.output.allocs.items[offset + slot] = alloc;
    }

    /// Get allocation for a live range (from bundle or spillslot).
    pub fn getAllocForRange(self: *const Self, range_idx: LiveRangeIndex) Allocation {
        const bundle = self.ranges.items[range_idx.index()].bundle;
        const bundledata = &self.bundles.items[bundle.index()];

        if (!bundledata.allocation.isNone()) {
            return bundledata.allocation;
        } else {
            // Get from spillslot
            const slot = self.spillsets.items[bundledata.spillset.index()].slot;
            return self.spillslots.items[slot.index()].alloc;
        }
    }

    //=========================================================================
    // Edge move location selection
    //=========================================================================

    /// Determine where to insert moves on a block edge.
    /// Returns (position, priority).
    fn chooseMoveLocation(
        _: *const Self,
        _: Block,
        to: Block,
        _: usize,
        func_entry_block: Block,
        from_last_insn: Inst,
        to_first_insn: Inst,
        from_is_ret: bool,
        from_succs_len: usize,
        to_preds_len: usize,
    ) struct { pos: ProgPoint, prio: InsertMovePrio } {
        const to_is_entry = func_entry_block.idx() == to.idx();
        const from_outs = from_succs_len + @as(usize, if (from_is_ret) 1 else 0);
        const to_ins = to_preds_len + @as(usize, if (to_is_entry) 1 else 0);

        if (to_ins > 1 and from_outs <= 1) {
            // Multiple in-edges to `to`, so moves go at tail of `from`
            return .{
                .pos = ProgPoint.before(from_last_insn),
                .prio = .out_edge_moves,
            };
        } else if (to_ins <= 1) {
            // Single in-edge, moves go at head of `to`
            return .{
                .pos = ProgPoint.before(to_first_insn),
                .prio = .in_edge_moves,
            };
        } else {
            // Critical edge - should not happen with proper edge splitting
            @panic("Critical edge: can't insert moves");
        }
    }

    //=========================================================================
    // Spillslot allocation
    //=========================================================================

    /// Allocate a spillslot for scratch use.
    fn allocateSpillslot(self: *Self, size: u32) Allocation {
        var offset: u32 = @intCast(self.num_spillslots.*);
        std.debug.assert(std.math.isPowerOfTwo(size));
        offset = (offset + size - 1) & ~(size - 1);
        const slot = offset;
        offset += size;
        self.num_spillslots.* = offset;
        return Allocation.stack(SpillSlot.new(slot));
    }
};

//=============================================================================
// Tests
//=============================================================================

test "RedundantMoveEliminator basic elision" {
    const allocator = std.testing.allocator;

    var elim = RedundantMoveEliminator.init(allocator);
    defer elim.deinit();

    const r0 = PReg.new(0, .int);
    const r1 = PReg.new(1, .int);
    const alloc_r0 = Allocation.reg(r0);
    const alloc_r1 = Allocation.reg(r1);

    const vreg = VReg.new(0, .int);

    // First move: r0 -> r1 (not redundant)
    const action1 = try elim.processMove(alloc_r0, alloc_r1, vreg);
    try std.testing.expect(!action1.elide);

    // Same move again: r0 -> r1 (redundant - r1 already has r0's value)
    const action2 = try elim.processMove(alloc_r0, alloc_r1, vreg);
    try std.testing.expect(action2.elide);

    // Reverse move: r1 -> r0 (redundant - r0's value was copied to r1)
    const action3 = try elim.processMove(alloc_r1, alloc_r0, vreg);
    try std.testing.expect(action3.elide);
}

test "RedundantMoveEliminator clear on def" {
    const allocator = std.testing.allocator;

    var elim = RedundantMoveEliminator.init(allocator);
    defer elim.deinit();

    const r0 = PReg.new(0, .int);
    const r1 = PReg.new(1, .int);
    const alloc_r0 = Allocation.reg(r0);
    const alloc_r1 = Allocation.reg(r1);

    const vreg = VReg.new(0, .int);

    // Move r0 -> r1
    const action1 = try elim.processMove(alloc_r0, alloc_r1, vreg);
    try std.testing.expect(!action1.elide);

    // Clear r1 (simulating a def)
    try elim.clearAlloc(alloc_r1);

    // Same move again: r0 -> r1 (NOT redundant now, r1 was cleared)
    const action2 = try elim.processMove(alloc_r0, alloc_r1, vreg);
    try std.testing.expect(!action2.elide);
}

test "PrevBuffer advancement" {
    const p0 = ProgPoint.before(Inst.new(0));
    const p1 = ProgPoint.before(Inst.new(1));
    const p2 = ProgPoint.before(Inst.new(2));
    const p3 = ProgPoint.before(Inst.new(3));

    var buf = PrevBuffer.init(0);

    // First entry: [0, 2)
    const e1 = LiveRangeListEntry{
        .range = .{ .from = p0, .to = p2 },
        .index = LiveRangeIndex.new(0),
    };
    buf.advance(e1);
    try std.testing.expect(buf.isValid() == null); // Not valid until second advance

    // Second entry: [1, 3) - starts later, so prev becomes e1
    const e2 = LiveRangeListEntry{
        .range = .{ .from = p1, .to = p3 },
        .index = LiveRangeIndex.new(1),
    };
    buf.advance(e2);
    const prev = buf.isValid();
    try std.testing.expect(prev != null);
    try std.testing.expectEqual(p0.bits, prev.?.range.from.bits);
}

test "MoveContext block boundaries" {
    // Create minimal CFGInfo for testing
    var insn_block: std.ArrayListUnmanaged(Block) = .{};
    var block_entry: std.ArrayListUnmanaged(ProgPoint) = .{};
    var block_exit: std.ArrayListUnmanaged(ProgPoint) = .{};
    var approx_loop_depth: std.ArrayListUnmanaged(u32) = .{};
    var postorder: std.ArrayListUnmanaged(Block) = .{};
    var domtree: std.ArrayListUnmanaged(Block) = .{};
    defer insn_block.deinit(std.testing.allocator);
    defer block_entry.deinit(std.testing.allocator);
    defer block_exit.deinit(std.testing.allocator);
    defer approx_loop_depth.deinit(std.testing.allocator);
    defer postorder.deinit(std.testing.allocator);
    defer domtree.deinit(std.testing.allocator);

    // Block 0: instructions 0-2
    try insn_block.append(std.testing.allocator, Block.new(0));
    try insn_block.append(std.testing.allocator, Block.new(0));
    try insn_block.append(std.testing.allocator, Block.new(0));

    try block_entry.append(std.testing.allocator, ProgPoint.before(Inst.new(0)));
    try block_exit.append(std.testing.allocator, ProgPoint.after(Inst.new(2)));
    try approx_loop_depth.append(std.testing.allocator, 0);
    try postorder.append(std.testing.allocator, Block.new(0));
    try domtree.append(std.testing.allocator, Block.invalid());

    const cfginfo = CFGInfo{
        .insn_block = insn_block,
        .block_entry = block_entry,
        .block_exit = block_exit,
        .approx_loop_depth = approx_loop_depth,
        .postorder = postorder,
        .domtree = domtree,
    };

    // Create minimal other structures
    var ranges: std.ArrayListUnmanaged(LiveRange) = .{};
    var bundles: std.ArrayListUnmanaged(LiveBundle) = .{};
    var spillsets: std.ArrayListUnmanaged(SpillSet) = .{};
    var spillslots: std.ArrayListUnmanaged(SpillSlotData) = .{};
    var vregs: std.ArrayListUnmanaged(VRegData) = .{};
    var output = Output.init();
    var fixups: std.ArrayListUnmanaged(MultiFixedRegFixup) = .{};
    var extra: [3]std.ArrayListUnmanaged(Allocation) = .{ .{}, .{}, .{} };
    var num_spillslots: usize = 0;

    const ctx = MoveContext.init(
        std.testing.allocator,
        &ranges,
        &bundles,
        &spillsets,
        &spillslots,
        &vregs,
        &cfginfo,
        &output,
        &.{},
        &.{},
        &fixups,
        &extra,
        .{ PReg.invalid(), PReg.invalid(), PReg.invalid() },
        &num_spillslots,
    );

    // Test block boundary detection
    try std.testing.expect(ctx.isStartOfBlock(ProgPoint.before(Inst.new(0))));
    try std.testing.expect(!ctx.isStartOfBlock(ProgPoint.before(Inst.new(1))));
    try std.testing.expect(ctx.isEndOfBlock(ProgPoint.after(Inst.new(2))));
    try std.testing.expect(!ctx.isEndOfBlock(ProgPoint.before(Inst.new(1))));
}
