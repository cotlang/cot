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
const env_mod = @import("env.zig");
const func_mod = @import("func.zig");
const indexset_mod = @import("indexset.zig");
const IndexSet = indexset_mod.IndexSet;

const Block = index.Block;
const Inst = index.Inst;
const VReg = index.VReg;
const PReg = index.PReg;
const PRegSet = index.PRegSet;
const RegClass = index.RegClass;
const SpillSlot = index.SpillSlot;
const Allocation = operand_mod.Allocation;
const ProgPoint = operand_mod.ProgPoint;
const InstPosition = operand_mod.InstPosition;
const Edit = operand_mod.Edit;
const Operand = operand_mod.Operand;
const OperandKind = operand_mod.OperandKind;
const OperandConstraint = operand_mod.OperandConstraint;
const MachineEnv = env_mod.MachineEnv;
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
// Inter-block move helper types
// Ported from regalloc2 moves.rs lines 222-265
//=============================================================================

/// Destination for an inter-block move (non-blockparam).
const InterBlockDest = struct {
    to: Block,
    from: Block,
    alloc: Allocation,

    fn key(self: InterBlockDest) u64 {
        return u64Key(self.from.rawU32(), self.to.rawU32());
    }
};

/// Destination for a blockparam move.
const BlockparamDest = struct {
    from_block: Block,
    to_block: Block,
    to_vreg: VRegIndex,
    alloc: Allocation,
};

/// Create a source key for blockparam move matching.
/// Key = u64_key(from_block, to_vreg) — matches Cranelift's BlockparamSourceKey.
fn blockparamSourceKey(from_block: Block, to_vreg: VRegIndex) u64 {
    return u64Key(from_block.rawU32(), to_vreg.rawU32());
}

//=============================================================================
// MoveContext - Context for move insertion
//=============================================================================

/// MoveContext - Context for move insertion.
///
/// Ported from regalloc2's `Env` struct in `src/ion/mod.rs`, specifically
/// the move-related functionality in `src/ion/moves.rs`.
pub fn MoveContext(comptime Func: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,

        // === Function interface ===
        func: *const Func,

        // === Machine environment ===
        env: *const MachineEnv,

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

        // === Liveness data (for inter-block moves) ===
        liveins: []const IndexSet,

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
            func: *const Func,
            env: *const MachineEnv,
            ranges: *std.ArrayListUnmanaged(LiveRange),
            bundles: *std.ArrayListUnmanaged(LiveBundle),
            spillsets: *std.ArrayListUnmanaged(SpillSet),
            spillslots: *std.ArrayListUnmanaged(SpillSlotData),
            vregs: *std.ArrayListUnmanaged(VRegData),
            cfginfo: *const CFGInfo,
            output: *Output,
            blockparam_ins: []const BlockparamIn,
            blockparam_outs: []const BlockparamOut,
            liveins: []const IndexSet,
            multi_fixed_reg_fixups: *std.ArrayListUnmanaged(MultiFixedRegFixup),
            extra_spillslots_by_class: *[3]std.ArrayListUnmanaged(Allocation),
            preferred_victim_by_class: [3]PReg,
            num_spillslots: *usize,
        ) Self {
            return .{
                .allocator = allocator,
                .func = func,
                .env = env,
                .ranges = ranges,
                .bundles = bundles,
                .spillsets = spillsets,
                .spillslots = spillslots,
                .vregs = vregs,
                .cfginfo = cfginfo,
                .output = output,
                .blockparam_ins = blockparam_ins,
                .blockparam_outs = blockparam_outs,
                .liveins = liveins,
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
            // Get from spillslot - bundle must have spillset.required = true
            const spillset = bundledata.spillset;
            const spillset_data = &self.spillsets.items[spillset.index()];

            // Defensive check: this should never happen in correct operation
            std.debug.assert(spillset_data.required);

            const slot = spillset_data.slot;
            return self.spillslots.items[slot.index()].alloc;
        }
    }

    //=========================================================================
    // Edge move location selection
    //=========================================================================

    /// Determine where to insert moves on a block edge.
    /// Ported from regalloc2's `choose_move_location` in moves.rs:172-219.
    fn chooseMoveLocation(self: *const Self, from: Block, to: Block) struct { pos: ProgPoint, prio: InsertMovePrio } {
        const from_last_insn = self.func.blockInsns(from).last();
        const to_first_insn = self.func.blockInsns(to).first();
        const from_is_ret = self.func.isRet(from_last_insn);
        const to_is_entry = self.func.entryBlock().idx() == to.idx();
        const from_outs = self.func.blockSuccs(from).len + @as(usize, if (from_is_ret) 1 else 0);
        const to_ins = self.func.blockPreds(to).len + @as(usize, if (to_is_entry) 1 else 0);

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

    /// Check if a vreg is live-in at a block.
    /// Ported from regalloc2's is_live_in.
    fn isLiveIn(self: *const Self, block: Block, vreg: VRegIndex) bool {
        if (block.idx() >= self.liveins.len) return false;
        return self.liveins[block.idx()].get(vreg.index());
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

    //=========================================================================
    // Apply allocations and insert moves
    //=========================================================================

    /// Main entry point: apply allocations to uses and insert moves.
    ///
    /// Ported from regalloc2's `apply_allocations_and_insert_moves` in moves.rs:76-686.
    /// Handles all cases:
    /// - Sort vreg range lists by start point
    /// - Apply allocations to all uses
    /// - Insert intra-block moves between adjacent live ranges
    /// - Insert inter-block source/dest half-moves at block edges
    /// - Insert blockparam source/dest moves
    /// - Handle multi-fixed-reg constraints
    pub fn applyAllocationsAndInsertMoves(self: *Self) !InsertedMoves {
        var inserted_moves = InsertedMoves.init();

        const num_blocks = self.func.numBlocks();

        // Sort vreg range lists by start point
        for (self.vregs.items) |*vreg| {
            for (vreg.ranges.items) |*entry| {
                entry.range = self.ranges.items[entry.index.index()].range;
            }
            std.mem.sort(LiveRangeListEntry, vreg.ranges.items, {}, struct {
                fn lessThan(_: void, a: LiveRangeListEntry, b: LiveRangeListEntry) bool {
                    return a.range.from.bits < b.range.from.bits;
                }
            }.lessThan);
        }

        // Inter-block move tracking (per-vreg, cleared each iteration)
        var inter_block_sources = std.AutoHashMapUnmanaged(u32, Allocation){};
        defer inter_block_sources.deinit(self.allocator);
        var inter_block_dests = std.ArrayListUnmanaged(InterBlockDest){};
        defer inter_block_dests.deinit(self.allocator);

        // Blockparam move tracking (accumulated across all vregs)
        var block_param_sources = std.AutoHashMapUnmanaged(u64, Allocation){};
        defer block_param_sources.deinit(self.allocator);
        var block_param_dests = std.ArrayListUnmanaged(BlockparamDest){};
        defer block_param_dests.deinit(self.allocator);

        var blockparam_in_idx: usize = 0;
        var blockparam_out_idx: usize = 0;

        // Process each vreg
        for (self.vregs.items, 0..) |*vreg_data, vreg_idx| {
            const vreg = VRegIndex.new(vreg_idx);

            // Skip unused vregs
            if (vreg_data.ranges.items.len == 0) continue;

            inter_block_sources.clearRetainingCapacity();

            var prev = PrevBuffer.init(blockparam_in_idx);

            for (vreg_data.ranges.items) |entry| {
                const alloc = self.getAllocForRange(entry.index);
                const range = entry.range;

                prev.advance(entry);

                // === Phase 1: Intra-block moves ===
                // If prev range abuts in same block, insert move
                if (prev.isValid()) |prev_entry| {
                    const prev_alloc = self.getAllocForRange(prev_entry.index);

                    if (prev_entry.range.to.bits >= range.from.bits and
                        (prev_entry.range.to.bits > range.from.bits or
                        !self.isStartOfBlock(range.from)) and
                        !self.ranges.items[entry.index.index()].hasFlag(.starts_at_def))
                    {
                        const v = VReg.new(vreg_idx, vreg_data.class orelse .int);
                        try inserted_moves.push(
                            self.allocator,
                            range.from,
                            .regular,
                            prev_alloc,
                            alloc,
                            v,
                        );
                    }
                }

                // === Phase 2A: Inter-block source half-moves + blockparam_outs ===
                // Scan blocks whose exits are covered by this range.
                {
                    var block = self.cfginfo.insn_block.items[range.from.inst().idx()];
                    while (block.isValid() and block.idx() < num_blocks) {
                        // Break if range doesn't cover the block exit
                        if (range.to.bits < self.cfginfo.block_exit.items[block.idx()].nextPoint().bits) {
                            break;
                        }

                        // Record source alloc for this block (prefer reg)
                        const gop = try inter_block_sources.getOrPut(self.allocator, block.rawU32());
                        if (!gop.found_existing) {
                            gop.value_ptr.* = alloc;
                        } else if (!gop.value_ptr.isReg()) {
                            gop.value_ptr.* = alloc;
                        }

                        // Scan blockparam_outs for matching (from_vreg, from_block)
                        while (blockparam_out_idx < self.blockparam_outs.len) {
                            const bpo = self.blockparam_outs[blockparam_out_idx];
                            // Tuple comparison: (from_vreg, from_block) > (vreg, block)
                            if (bpo.from_vreg.index() > vreg.index() or
                                (bpo.from_vreg.index() == vreg.index() and bpo.from_block.idx() > block.idx()))
                            {
                                break;
                            }
                            if (bpo.from_vreg.index() == vreg.index() and bpo.from_block.idx() == block.idx()) {
                                // Record blockparam source: key = (from_block, to_vreg)
                                const bp_key = blockparamSourceKey(bpo.from_block, bpo.to_vreg);
                                const bp_gop = try block_param_sources.getOrPut(self.allocator, bp_key);
                                if (!bp_gop.found_existing) {
                                    bp_gop.value_ptr.* = alloc;
                                } else if (!bp_gop.value_ptr.isReg()) {
                                    bp_gop.value_ptr.* = alloc;
                                }
                            }
                            blockparam_out_idx += 1;
                        }

                        block = block.next();
                    }
                }

                // === Phase 2B: Inter-block dest half-moves + blockparam_ins ===
                // Scan blocks whose entries are covered by this range.
                {
                    var block = self.cfginfo.insn_block.items[range.from.inst().idx()];
                    // If block entry is before range start, skip to next block
                    if (self.cfginfo.block_entry.items[block.idx()].bits < range.from.bits) {
                        block = block.next();
                    }
                    while (block.isValid() and block.idx() < num_blocks) {
                        if (self.cfginfo.block_entry.items[block.idx()].bits >= range.to.bits) {
                            break;
                        }

                        // Scan blockparam_ins for matching (to_vreg, to_block)
                        var bp_idx = prev.blockparamInsIdx();
                        while (bp_idx < self.blockparam_ins.len) {
                            const bpi = self.blockparam_ins[bp_idx];
                            // Tuple comparison: (to_vreg, to_block) > (vreg, block)
                            if (bpi.to_vreg.index() > vreg.index() or
                                (bpi.to_vreg.index() == vreg.index() and bpi.to_block.idx() > block.idx()))
                            {
                                break;
                            }
                            if (bpi.to_vreg.index() == vreg.index() and bpi.to_block.idx() == block.idx()) {
                                try block_param_dests.append(self.allocator, .{
                                    .from_block = bpi.from_block,
                                    .to_block = bpi.to_block,
                                    .to_vreg = bpi.to_vreg,
                                    .alloc = alloc,
                                });
                            }
                            bp_idx += 1;
                        }
                        prev.updateBlockparamInsIdx(bp_idx);

                        // Check if vreg is live-in to this block
                        if (!self.isLiveIn(block, vreg)) {
                            block = block.next();
                            continue;
                        }

                        // For each predecessor whose exit is NOT in same range,
                        // add inter-block dest half-move
                        for (self.func.blockPreds(block)) |pred| {
                            if (range.containsPoint(self.cfginfo.block_exit.items[pred.idx()])) {
                                continue;
                            }
                            try inter_block_dests.append(self.allocator, .{
                                .from = pred,
                                .to = block,
                                .alloc = alloc,
                            });
                        }

                        block = block.next();
                    }
                }

                // Apply allocations to all uses in this range
                for (self.ranges.items[entry.index.index()].uses.items) |usedata| {
                    self.setAlloc(usedata.pos.inst(), usedata.slot, alloc);
                }
            }

            // === Phase 3: Process inter-block dests for this vreg ===
            if (inter_block_dests.items.len > 0) {
                std.mem.sort(InterBlockDest, inter_block_dests.items, {}, struct {
                    fn lessThan(_: void, a: InterBlockDest, b: InterBlockDest) bool {
                        return a.key() < b.key();
                    }
                }.lessThan);

                const v = VReg.new(vreg_idx, vreg_data.class orelse .int);
                for (inter_block_dests.items) |dest| {
                    const src = inter_block_sources.get(dest.from.rawU32()) orelse {
                        // Source not found - this shouldn't happen in correct allocation
                        continue;
                    };
                    const loc = self.chooseMoveLocation(dest.from, dest.to);
                    try inserted_moves.push(self.allocator, loc.pos, loc.prio, src, dest.alloc, v);
                }
                inter_block_dests.clearRetainingCapacity();
            }

            blockparam_in_idx = prev.blockparamInsIdx();
        }

        // === Phase 4: Process blockparam dests (across all vregs) ===
        if (block_param_dests.items.len > 0) {
            for (block_param_dests.items) |dest| {
                const src_key = blockparamSourceKey(dest.from_block, dest.to_vreg);
                const src_alloc = block_param_sources.get(src_key) orelse {
                    continue;
                };
                const loc = self.chooseMoveLocation(dest.from_block, dest.to_block);
                const vreg_data = &self.vregs.items[dest.to_vreg.index()];
                const v = VReg.new(dest.to_vreg.index(), vreg_data.class orelse .int);
                try inserted_moves.push(self.allocator, loc.pos, loc.prio, src_alloc, dest.alloc, v);
            }
        }

        // Handle multi-fixed-reg fixups
        for (self.multi_fixed_reg_fixups.items) |fixup| {
            const from_alloc = self.getAlloc(fixup.pos.inst(), fixup.from_slot);
            const to_alloc = Allocation.reg(PReg.fromIndex(fixup.to_preg.index()));
            const prio: InsertMovePrio = switch (fixup.level) {
                .initial => .multi_fixed_reg_initial,
                .secondary => .multi_fixed_reg_secondary,
            };
            const v = VReg.new(fixup.vreg.index(), .int);
            try inserted_moves.push(self.allocator, fixup.pos, prio, from_alloc, to_alloc, v);
            self.setAlloc(fixup.pos.inst(), fixup.to_slot, to_alloc);
        }

        return inserted_moves;
    }

    //=========================================================================
    // Resolve inserted moves
    //=========================================================================

    /// Resolve parallel moves to sequential moves.
    ///
    /// Takes the InsertedMoves collection and produces a final Edits list
    /// with moves ordered correctly. Uses RedundantMoveEliminator to skip
    /// moves that don't actually change values.
    pub fn resolveInsertedMoves(self: *Self, inserted_moves: *InsertedMoves) !Edits {
        var edits = Edits.init();
        var redundant_moves = RedundantMoveEliminator.init(self.allocator);
        defer redundant_moves.deinit();

        // Sort moves by position and priority
        std.mem.sort(InsertedMove, inserted_moves.moves.items, {}, struct {
            fn lessThan(_: void, a: InsertedMove, b: InsertedMove) bool {
                return a.pos_prio.key() < b.pos_prio.key();
            }
        }.lessThan);

        var i: usize = 0;
        var last_pos = PosWithPrio{ .pos = ProgPoint.before(Inst.new(0)), .prio = 0 };

        while (i < inserted_moves.moves.items.len) {
            const start = i;
            const pos_prio = inserted_moves.moves.items[i].pos_prio;

            // Group moves at the same position and priority
            while (i < inserted_moves.moves.items.len and
                inserted_moves.moves.items[i].pos_prio.key() == pos_prio.key())
            {
                i += 1;
            }
            const moves = inserted_moves.moves.items[start..i];

            // Process side effects between last position and current
            try self.processRedundantMoveSideEffects(&redundant_moves, last_pos.pos, pos_prio.pos);
            last_pos = pos_prio;

            // Group moves by register class
            var int_moves = std.ArrayListUnmanaged(InsertedMove){};
            var float_moves = std.ArrayListUnmanaged(InsertedMove){};
            var vec_moves = std.ArrayListUnmanaged(InsertedMove){};
            defer int_moves.deinit(self.allocator);
            defer float_moves.deinit(self.allocator);
            defer vec_moves.deinit(self.allocator);

            for (moves) |m| {
                switch (m.to_vreg.class()) {
                    .int => try int_moves.append(self.allocator, m),
                    .float => try float_moves.append(self.allocator, m),
                    .vector => try vec_moves.append(self.allocator, m),
                }
            }

            // Process each class
            const class_moves = [_]struct { class: RegClass, moves: *std.ArrayListUnmanaged(InsertedMove) }{
                .{ .class = .int, .moves = &int_moves },
                .{ .class = .float, .moves = &float_moves },
                .{ .class = .vector, .moves = &vec_moves },
            };

            for (class_moves) |cm| {
                if (cm.moves.items.len == 0) continue;

                // Use parallel moves resolver
                var parallel = moves_mod.ParallelMoves(?VReg).init(self.allocator);
                defer parallel.deinit();

                for (cm.moves.items) |m| {
                    try parallel.add(m.from_alloc, m.to_alloc, m.to_vreg);
                }

                // Resolve to sequential moves
                var resolved = try parallel.resolve();
                defer resolved.deinit(self.allocator);

                // Get scratch register if needed
                const scratch = self.preferred_victim_by_class[@intFromEnum(cm.class)];
                const result = if (resolved.needsScratch())
                    try resolved.withScratch(self.allocator, Allocation.reg(scratch))
                else
                    resolved.withoutScratch() orelse unreachable;

                // Add each resolved move to edits
                for (result.items) |rm| {
                    const action = try redundant_moves.processMove(rm.from, rm.to, rm.data);
                    if (!action.elide) {
                        try edits.add(self.allocator, pos_prio, rm.from, rm.to);
                    }
                }
            }
        }

        // Sort edits by position (stable sort to preserve parallel move order)
        edits.sort();

        return edits;
    }

        /// Process side effects between two positions for redundant move elimination.
        ///
        /// Ported from regalloc2's `redundant_move_process_side_effects` in
        /// `src/ion/moves.rs:789-835`.
        ///
        /// For each instruction between `from` and `to`:
        /// - Clear allocations for Def operands (they overwrite the register)
        /// - Clear allocations for clobbered registers (garbage after instruction)
        /// - Clear allocations for scratch registers (may be used by any instruction)
        fn processRedundantMoveSideEffects(
            self: *const Self,
            redundant_moves: *RedundantMoveEliminator,
            from: ProgPoint,
            to: ProgPoint,
        ) !void {
            // If we cross a block boundary, clear and return.
            // Ported from: if this.cfginfo.insn_block[from.inst().index()] != ...
            if (self.cfginfo.insn_block.items[from.inst().idx()].idx() !=
                self.cfginfo.insn_block.items[to.inst().idx()].idx())
            {
                redundant_moves.clear();
                return;
            }

            // Calculate instruction range to process.
            // Ported from: let start_inst = if from.pos() == InstPosition::Before { ... }
            const start_inst: usize = if (from.pos() == .before)
                from.inst().idx()
            else
                from.inst().idx() + 1;
            const end_inst: usize = if (to.pos() == .before)
                to.inst().idx()
            else
                to.inst().idx() + 1;

            // For each instruction in range, clear allocations for defs and clobbers.
            // Ported from: for inst in start_inst.index()..end_inst.index() { ... }
            var inst_idx = start_inst;
            while (inst_idx < end_inst) : (inst_idx += 1) {
                const inst = Inst.new(inst_idx);

                // Clear allocations for Def operands.
                // Ported from: for (i, op) in this.func.inst_operands(inst).iter().enumerate() { ... }
                const operands = self.func.instOperands(inst);
                for (operands, 0..) |op, slot| {
                    if (op.kind() == .def) {
                        const alloc = self.getAlloc(inst, slot);
                        try redundant_moves.clearAlloc(alloc);
                    }
                }

                // Clear allocations for clobbered registers.
                // Ported from: for reg in this.func.inst_clobbers(inst) { ... }
                const clobbers = self.func.instClobbers(inst);
                var clobber_iter = clobbers.iter();
                while (clobber_iter.next()) |preg| {
                    try redundant_moves.clearAlloc(Allocation.reg(preg));
                }

                // Clear allocations for scratch registers.
                // Ported from: for reg in this.env.scratch_by_class { ... }
                for (self.env.scratch_by_class) |maybe_scratch| {
                    if (maybe_scratch) |scratch| {
                        try redundant_moves.clearAlloc(Allocation.reg(scratch));
                    }
                }
            }
        }
    };
}

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
    // Mock function type for testing
    const MockFunc = struct {
        pub fn numInsts(_: *const @This()) usize {
            return 3;
        }
        pub fn numBlocks(_: *const @This()) usize {
            return 1;
        }
        pub fn entryBlock(_: *const @This()) Block {
            return Block.new(0);
        }
        pub fn blockInsns(_: *const @This(), _: Block) index.InstRange {
            return index.InstRange.new(Inst.new(0), Inst.new(3));
        }
        pub fn blockSuccs(_: *const @This(), _: Block) []const Block {
            return &.{};
        }
        pub fn blockPreds(_: *const @This(), _: Block) []const Block {
            return &.{};
        }
        pub fn blockParams(_: *const @This(), _: Block) []const VReg {
            return &.{};
        }
        pub fn isRet(_: *const @This(), _: Inst) bool {
            return false;
        }
        pub fn isBranch(_: *const @This(), _: Inst) bool {
            return false;
        }
        pub fn branchBlockparams(_: *const @This(), _: Block, _: Inst, _: usize) []const VReg {
            return &.{};
        }
        pub fn instOperands(_: *const @This(), _: Inst) []const Operand {
            return &.{};
        }
        pub fn instClobbers(_: *const @This(), _: Inst) PRegSet {
            return PRegSet.empty();
        }
        pub fn numVregs(_: *const @This()) usize {
            return 0;
        }
        pub fn spillslotSize(_: *const @This(), _: RegClass) usize {
            return 1;
        }
    };

    const mock_func = MockFunc{};
    const mock_env = MachineEnv.empty();

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

    const ctx = MoveContext(MockFunc).init(
        std.testing.allocator,
        &mock_func,
        &mock_env,
        &ranges,
        &bundles,
        &spillsets,
        &spillslots,
        &vregs,
        &cfginfo,
        &output,
        &.{},
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
