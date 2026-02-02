//! Liveness analysis and live range computation.
//!
//! Ported from regalloc2's `src/ion/liveranges.rs`.
//!
//! This module computes liveness information (live-in/live-out sets)
//! and builds live ranges for each virtual register.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const indexset = @import("indexset.zig");
const ion_data = @import("ion_data.zig");
const cfg_mod = @import("cfg.zig");

const Block = index.Block;
const Inst = index.Inst;
const VReg = index.VReg;
const PReg = index.PReg;
const RegClass = index.RegClass;
const Operand = operand_mod.Operand;
const OperandConstraint = operand_mod.OperandConstraint;
const OperandKind = operand_mod.OperandKind;
const OperandPos = operand_mod.OperandPos;
const Allocation = operand_mod.Allocation;
const ProgPoint = operand_mod.ProgPoint;
const InstPosition = operand_mod.InstPosition;
const IndexSet = indexset.IndexSet;
const CodeRange = ion_data.CodeRange;
const LiveRangeIndex = ion_data.LiveRangeIndex;
const LiveBundleIndex = ion_data.LiveBundleIndex;
const VRegIndex = ion_data.VRegIndex;
const PRegIndex = ion_data.PRegIndex;
const LiveRange = ion_data.LiveRange;
const LiveRangeFlag = ion_data.LiveRangeFlag;
const LiveRangeListEntry = ion_data.LiveRangeListEntry;
const LiveRangeKey = ion_data.LiveRangeKey;
const Use = ion_data.Use;
const VRegData = ion_data.VRegData;
const PRegData = ion_data.PRegData;
const BlockparamIn = ion_data.BlockparamIn;
const BlockparamOut = ion_data.BlockparamOut;
const MultiFixedRegFixup = ion_data.MultiFixedRegFixup;
const FixedRegFixupLevel = ion_data.FixedRegFixupLevel;

//=============================================================================
// SpillWeight - Weight for spill decisions
//=============================================================================

/// A spill weight computed for a certain Use.
/// Stored internally as f32.
pub const SpillWeight = struct {
    value: f32,

    pub fn zero() SpillWeight {
        return .{ .value = 0.0 };
    }

    pub fn fromF32(x: f32) SpillWeight {
        return .{ .value = x };
    }

    pub fn toF32(self: SpillWeight) f32 {
        return self.value;
    }

    /// Convert to a u16 for compact storage (bfloat16-like format).
    /// Takes top 16 bits of f32 representation.
    pub fn toBits(self: SpillWeight) u16 {
        const bits: u32 = @bitCast(self.value);
        return @intCast(bits >> 15);
    }

    /// Convert from u16 bits back to SpillWeight.
    pub fn fromBits(bits: u16) SpillWeight {
        const expanded: u32 = @as(u32, bits) << 15;
        return .{ .value = @bitCast(expanded) };
    }

    pub fn toInt(self: SpillWeight) u32 {
        return @intFromFloat(self.value);
    }

    pub fn add(self: SpillWeight, other: SpillWeight) SpillWeight {
        return .{ .value = self.value + other.value };
    }
};

/// Compute spill weight from constraint, loop depth, and whether it's a def.
pub fn spillWeightFromConstraint(
    constraint: OperandConstraint,
    loop_depth: usize,
    is_def: bool,
) SpillWeight {
    // A bonus of 1000 for one loop level, 4000 for two, 16000 for three, etc.
    const clamped_depth = @min(loop_depth, 10);
    var hot_bonus: f32 = 1000.0;
    for (0..clamped_depth) |_| {
        hot_bonus *= 4.0;
    }

    const def_bonus: f32 = if (is_def) 2000.0 else 0.0;

    const constraint_bonus: f32 = switch (constraint) {
        .any => 1000.0,
        .reg, .fixed_reg => 2000.0,
        else => 0.0,
    };

    return .{ .value = hot_bonus + def_bonus + constraint_bonus };
}

//=============================================================================
// LivenessContext - Context for liveness computation
//=============================================================================

/// Context for liveness analysis, separate from the main allocator context.
pub const LivenessContext = struct {
    allocator: std.mem.Allocator,

    /// Live-in sets for each block.
    liveins: std.ArrayListUnmanaged(IndexSet),
    /// Live-out sets for each block.
    liveouts: std.ArrayListUnmanaged(IndexSet),

    /// All live ranges.
    ranges: std.ArrayListUnmanaged(LiveRange),
    /// Per-vreg data.
    vregs: std.ArrayListUnmanaged(VRegData),
    /// Per-preg data.
    pregs: std.ArrayListUnmanaged(PRegData),

    /// Block parameter flow: outgoing.
    blockparam_outs: std.ArrayListUnmanaged(BlockparamOut),
    /// Block parameter flow: incoming.
    blockparam_ins: std.ArrayListUnmanaged(BlockparamIn),

    /// Multi-fixed-reg fixups.
    multi_fixed_reg_fixups: std.ArrayListUnmanaged(MultiFixedRegFixup),

    /// Scratch: current live range for each vreg during computation.
    scratch_vreg_ranges: std.ArrayListUnmanaged(LiveRangeIndex),

    pub fn init(allocator: std.mem.Allocator) LivenessContext {
        return .{
            .allocator = allocator,
            .liveins = .{},
            .liveouts = .{},
            .ranges = .{},
            .vregs = .{},
            .pregs = .{},
            .blockparam_outs = .{},
            .blockparam_ins = .{},
            .multi_fixed_reg_fixups = .{},
            .scratch_vreg_ranges = .{},
        };
    }

    pub fn deinit(self: *LivenessContext) void {
        for (self.liveins.items) |*set| {
            set.deinit();
        }
        self.liveins.deinit(self.allocator);

        for (self.liveouts.items) |*set| {
            set.deinit();
        }
        self.liveouts.deinit(self.allocator);

        for (self.ranges.items) |*r| {
            r.deinit(self.allocator);
        }
        self.ranges.deinit(self.allocator);

        for (self.vregs.items) |*v| {
            v.deinit(self.allocator);
        }
        self.vregs.deinit(self.allocator);

        self.pregs.deinit(self.allocator);
        self.blockparam_outs.deinit(self.allocator);
        self.blockparam_ins.deinit(self.allocator);
        self.multi_fixed_reg_fixups.deinit(self.allocator);
        self.scratch_vreg_ranges.deinit(self.allocator);
    }

    /// Initialize VReg and PReg data structures.
    pub fn createPregsAndVregs(
        self: *LivenessContext,
        num_vregs: usize,
        num_pregs: usize,
    ) !void {
        // Create PRegs
        try self.pregs.resize(self.allocator, num_pregs);
        for (self.pregs.items) |*preg| {
            preg.* = PRegData.init();
        }

        // Create VRegs
        try self.vregs.resize(self.allocator, num_vregs);
        for (self.vregs.items) |*vreg| {
            vreg.* = VRegData.init();
        }

        // Initialize scratch
        try self.scratch_vreg_ranges.resize(self.allocator, num_vregs);
        @memset(self.scratch_vreg_ranges.items, LiveRangeIndex.invalid());
    }

    /// Add a live range for a vreg.
    pub fn addLiverangeToVreg(
        self: *LivenessContext,
        vreg: VRegIndex,
        range: CodeRange,
    ) !LiveRangeIndex {
        // Check if we can merge with the last range
        if (self.vregs.items[vreg.index()].ranges.items.len > 0) {
            const last_entry = self.vregs.items[vreg.index()].ranges.items[self.vregs.items[vreg.index()].ranges.items.len - 1];
            const last_range = self.ranges.items[last_entry.index.index()].range;

            // If contiguous, extend the existing range
            if (range.to.bits == last_range.from.bits) {
                const lr = last_entry.index;
                self.ranges.items[lr.index()].range.from = range.from;
                return lr;
            }
        }

        // Create new range
        var new_range = LiveRange.init(range);
        new_range.vreg = vreg;

        const lr = LiveRangeIndex.new(self.ranges.items.len);
        try self.ranges.append(self.allocator, new_range);

        try self.vregs.items[vreg.index()].ranges.append(self.allocator, .{
            .range = range,
            .index = lr,
        });

        return lr;
    }

    /// Insert a use into a live range.
    pub fn insertUseIntoLiverange(
        self: *LivenessContext,
        into: LiveRangeIndex,
        use_item: Use,
        loop_depth: usize,
    ) !void {
        var u = use_item;
        const constraint = u.operand.constraint();
        const weight = spillWeightFromConstraint(
            constraint,
            loop_depth,
            u.operand.kind() != .use,
        );
        u.weight = weight.toBits();

        try self.ranges.items[into.index()].uses.append(self.allocator, u);

        // Update spill weight
        const range_weight = self.ranges.items[into.index()].usesSpillWeight();
        const new_weight = SpillWeight.fromF32(range_weight.toF32() + weight.value);
        self.ranges.items[into.index()].setUsesSpillWeight(new_weight);
    }

    /// Find the live range for a vreg at a given position.
    pub fn findVregLiverangeForPos(
        self: *const LivenessContext,
        vreg: VRegIndex,
        pos: ProgPoint,
    ) ?LiveRangeIndex {
        for (self.vregs.items[vreg.index()].ranges.items) |entry| {
            if (entry.range.containsPoint(pos)) {
                return entry.index;
            }
        }
        return null;
    }

    /// Add a live range to a physical register.
    pub fn addLiverangeToPreg(
        self: *LivenessContext,
        range: CodeRange,
        preg: PReg,
    ) !void {
        const key = LiveRangeKey.fromRange(range);
        _ = try self.pregs.items[preg.index()].allocations.insert(
            self.allocator,
            key,
            LiveRangeIndex.invalid(),
        );
    }

    /// Check if a vreg is live-in at a block.
    pub fn isLiveIn(self: *const LivenessContext, block: Block, vreg: VRegIndex) bool {
        return self.liveins.items[block.idx()].get(vreg.index());
    }
};

//=============================================================================
// Liveness Computation Algorithm
//=============================================================================

/// Error type for liveness computation.
pub const LivenessError = error{
    EntryLivein,
    TooManyOperands,
    OutOfMemory,
};

/// Compute liveness information for all blocks.
/// This is the worklist algorithm that computes precise liveins/liveouts.
pub fn computeLiveness(
    ctx: *LivenessContext,
    comptime Func: type,
    func: *const Func,
    cfg: *const cfg_mod.CFGInfo,
) LivenessError!void {
    const num_blocks = func.numBlocks();

    // Create initial livein/liveout sets
    for (0..num_blocks) |_| {
        try ctx.liveins.append(ctx.allocator, IndexSet.init(ctx.allocator));
        try ctx.liveouts.append(ctx.allocator, IndexSet.init(ctx.allocator));
    }

    // Worklist algorithm
    var workqueue = std.fifo.LinearFifo(Block, .Dynamic).init(ctx.allocator);
    defer workqueue.deinit();

    var workqueue_set = std.AutoHashMap(u32, void).init(ctx.allocator);
    defer workqueue_set.deinit();

    // Initialize with postorder
    for (cfg.postorder.items) |block| {
        try workqueue.writeItem(block);
        try workqueue_set.put(block.rawU32(), {});
    }

    while (workqueue.readItem()) |block| {
        _ = workqueue_set.remove(block.rawU32());

        const insns = func.blockInsns(block);

        // Start with liveout
        var live = try ctx.liveouts.items[block.idx()].clone();
        defer live.deinit();

        // Include outgoing blockparams
        if (!insns.isEmpty() and func.isBranch(insns.last())) {
            const succs = func.blockSuccs(block);
            for (succs, 0..) |_, succ_idx| {
                for (func.branchBlockparams(block, insns.last(), succ_idx)) |param| {
                    try live.set(param.vreg(), true);
                }
            }
        }

        // Process instructions in reverse
        var iter = insns.reverseIter();
        while (iter.next()) |inst| {
            // Process Late then Early
            for ([_]OperandPos{ .late, .early }) |pos| {
                for (func.instOperands(inst)) |op| {
                    if (op.asFixedNonAllocatable() != null) {
                        continue;
                    }
                    if (op.pos() == pos) {
                        switch (op.kind()) {
                            .use => try live.set(op.vreg().vreg(), true),
                            .def => try live.set(op.vreg().vreg(), false),
                        }
                    }
                }
            }
        }

        // Remove block params from live set
        for (func.blockParams(block)) |param| {
            try live.set(param.vreg(), false);
        }

        // Propagate to predecessors
        for (func.blockPreds(block)) |pred| {
            if (try ctx.liveouts.items[pred.idx()].unionWith(&live)) {
                if (!workqueue_set.contains(pred.rawU32())) {
                    try workqueue_set.put(pred.rawU32(), {});
                    try workqueue.writeItem(pred);
                }
            }
        }

        // Store livein
        ctx.liveins.items[block.idx()].deinit();
        ctx.liveins.items[block.idx()] = try live.clone();
    }

    // Check entry block has no liveins
    if (!ctx.liveins.items[func.entryBlock().idx()].isEmpty()) {
        return LivenessError.EntryLivein;
    }
}

//=============================================================================
// Tests
//=============================================================================

test "SpillWeight basic operations" {
    const w1 = SpillWeight.fromF32(1000.0);
    try std.testing.expectEqual(@as(f32, 1000.0), w1.toF32());

    const w2 = SpillWeight.fromF32(2000.0);
    const sum = w1.add(w2);
    try std.testing.expectEqual(@as(f32, 3000.0), sum.toF32());
}

test "SpillWeight bits round-trip" {
    const original = SpillWeight.fromF32(1234.5);
    const bits = original.toBits();
    const recovered = SpillWeight.fromBits(bits);

    // Some precision loss is expected
    const diff = @abs(original.toF32() - recovered.toF32());
    try std.testing.expect(diff < 10.0);
}

test "spillWeightFromConstraint" {
    // Basic constraint weights
    const any_weight = spillWeightFromConstraint(.any, 0, false);
    try std.testing.expect(any_weight.toF32() >= 1000.0);

    const reg_weight = spillWeightFromConstraint(.reg, 0, false);
    try std.testing.expect(reg_weight.toF32() > any_weight.toF32());

    // Def bonus
    const def_weight = spillWeightFromConstraint(.any, 0, true);
    try std.testing.expect(def_weight.toF32() > any_weight.toF32());

    // Loop depth bonus
    const loop_weight = spillWeightFromConstraint(.any, 1, false);
    try std.testing.expect(loop_weight.toF32() > any_weight.toF32());
}

test "LivenessContext init/deinit" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    try ctx.createPregsAndVregs(10, 32);

    try std.testing.expectEqual(@as(usize, 10), ctx.vregs.items.len);
    try std.testing.expectEqual(@as(usize, 32), ctx.pregs.items.len);
}

test "LivenessContext addLiverangeToVreg" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    try ctx.createPregsAndVregs(5, 32);

    const vreg = VRegIndex.new(0);
    const range = CodeRange{
        .from = ProgPoint.before(Inst.new(0)),
        .to = ProgPoint.before(Inst.new(5)),
    };

    const lr = try ctx.addLiverangeToVreg(vreg, range);
    try std.testing.expect(lr.isValid());
    try std.testing.expectEqual(@as(usize, 1), ctx.ranges.items.len);
    try std.testing.expectEqual(@as(usize, 1), ctx.vregs.items[0].ranges.items.len);
}

test "LivenessContext contiguous ranges merged" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    try ctx.createPregsAndVregs(5, 32);

    const vreg = VRegIndex.new(0);

    // Add ranges in reverse order (as would happen during backward scan)
    const range2 = CodeRange{
        .from = ProgPoint.before(Inst.new(5)),
        .to = ProgPoint.before(Inst.new(10)),
    };
    const lr1 = try ctx.addLiverangeToVreg(vreg, range2);

    const range1 = CodeRange{
        .from = ProgPoint.before(Inst.new(0)),
        .to = ProgPoint.before(Inst.new(5)), // Contiguous with range2
    };
    const lr2 = try ctx.addLiverangeToVreg(vreg, range1);

    // Should return the same range (merged)
    try std.testing.expect(lr1.eql(lr2));
    try std.testing.expectEqual(@as(usize, 1), ctx.ranges.items.len);

    // Range should span full extent
    const merged = ctx.ranges.items[lr1.index()].range;
    try std.testing.expectEqual(range1.from.bits, merged.from.bits);
    try std.testing.expectEqual(range2.to.bits, merged.to.bits);
}
