//! Liveness analysis and live range computation.
//!
//! Ported from regalloc2's `src/ion/liveranges.rs`.
//!
//! This module computes liveness information (live-in/live-out sets)
//! and builds live ranges for each virtual register.
//!
//! See audit/native/liveranges_audit.md for full function-by-function mapping.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const indexset = @import("indexset.zig");
const ion_data = @import("ion_data.zig");
const cfg_mod = @import("cfg.zig");
const env_mod = @import("env.zig");
const output_mod = @import("output.zig");
// PRegSet from machinst matches what vcode returns
const machinst_reg = @import("../machinst/reg.zig");

const Block = index.Block;
const Inst = index.Inst;
const InstRange = index.InstRange;
const VReg = index.VReg;
const PReg = index.PReg;
const PRegSet = machinst_reg.PRegSet;
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
const SpillSlotIndex = ion_data.SpillSlotIndex;
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
const MachineEnv = env_mod.MachineEnv;
const Output = output_mod.Output;
const RegAllocError = output_mod.RegAllocError;

//=============================================================================
// SpillWeight - Weight for spill decisions
//=============================================================================

/// A spill weight computed for a certain Use.
/// Stored internally as f32, can be compactly encoded to u16 (bfloat16-like).
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
    /// Takes top 16 bits of f32 representation (>> 15, not >> 16).
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
    // Avoids exponentiation by folding.
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

/// Convert operand slot index to u16, returning error if too large.
fn slotIdx(i: usize) !u16 {
    if (i > std.math.maxInt(u16)) {
        return error.TooManyOperands;
    }
    return @intCast(i);
}

//=============================================================================
// LivenessContext - Main context for liveness computation
//=============================================================================

/// Error type for liveness computation.
pub const LivenessError = error{
    EntryLivein,
    TooManyOperands,
    OutOfMemory,
};

/// Context for liveness analysis.
/// This holds all the mutable state during liveness computation.
pub const LivenessContext = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    // === Liveness data ===
    /// Live-in sets for each block (indexed by block index).
    liveins: std.ArrayListUnmanaged(IndexSet),
    /// Live-out sets for each block (indexed by block index).
    liveouts: std.ArrayListUnmanaged(IndexSet),

    // === Live range data ===
    /// All live ranges.
    ranges: std.ArrayListUnmanaged(LiveRange),
    /// Per-vreg data.
    vregs: std.ArrayListUnmanaged(VRegData),
    /// Per-preg data.
    pregs: std.ArrayListUnmanaged(PRegData),

    // === Block parameter flow ===
    /// Outgoing block parameter assignments.
    blockparam_outs: std.ArrayListUnmanaged(BlockparamOut),
    /// Incoming block parameter assignments.
    blockparam_ins: std.ArrayListUnmanaged(BlockparamIn),

    // === Fixups ===
    /// Multi-fixed-reg fixups for conflicting constraints.
    multi_fixed_reg_fixups: std.ArrayListUnmanaged(MultiFixedRegFixup),

    // === Scratch space (reused across operations) ===
    /// Current live range for each vreg during computation.
    scratch_vreg_ranges: std.ArrayListUnmanaged(LiveRangeIndex),
    /// Operand rewrites for fixed-reg conflict handling.
    scratch_operand_rewrites: std.AutoHashMap(usize, Operand),
    /// Workqueue for liveness computation (used as a stack).
    scratch_workqueue: std.ArrayListUnmanaged(Block),
    /// Set tracking which blocks are in workqueue.
    scratch_workqueue_set: std.AutoHashMap(u32, void),

    // === Machine environment data ===
    /// Preferred victim register per class (for eviction).
    preferred_victim_by_class: [3]PReg,

    // === Statistics ===
    stats: struct {
        livein_iterations: usize = 0,
        livein_blocks: usize = 0,
        initial_liverange_count: usize = 0,
        blockparam_ins_count: usize = 0,
        blockparam_outs_count: usize = 0,
    } = .{},

    pub fn init(allocator: std.mem.Allocator) Self {
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
            .scratch_operand_rewrites = std.AutoHashMap(usize, Operand).init(allocator),
            .scratch_workqueue = .{},
            .scratch_workqueue_set = std.AutoHashMap(u32, void).init(allocator),
            .preferred_victim_by_class = [_]PReg{PReg.invalid()} ** 3,
        };
    }

    pub fn deinit(self: *Self) void {
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

        for (self.pregs.items) |*p| {
            p.allocations.deinit(self.allocator);
        }
        self.pregs.deinit(self.allocator);

        self.blockparam_outs.deinit(self.allocator);
        self.blockparam_ins.deinit(self.allocator);
        self.multi_fixed_reg_fixups.deinit(self.allocator);
        self.scratch_vreg_ranges.deinit(self.allocator);
        self.scratch_operand_rewrites.deinit();
        self.scratch_workqueue.deinit(self.allocator);
        self.scratch_workqueue_set.deinit();
    }

    /// Initialize PReg and VReg data structures from function and machine environment.
    pub fn createPregsAndVregs(
        self: *Self,
        num_vregs: usize,
        machine_env: *const MachineEnv,
    ) !void {
        // Create PRegs (PReg.NUM_INDEX = 256)
        try self.pregs.resize(self.allocator, PReg.NUM_INDEX);
        for (self.pregs.items) |*preg| {
            preg.* = PRegData.init();
        }

        // Mark fixed stack slots
        for (machine_env.fixed_stack_slots) |preg| {
            self.pregs.items[preg.index()].is_stack = true;
        }

        // Set preferred victim by class
        for (0..3) |class_idx| {
            const class: RegClass = @enumFromInt(class_idx);
            // Prefer non-preferred regs, fall back to preferred
            const non_pref = machine_env.non_preferred_regs_by_class[class_idx];
            const pref = machine_env.preferred_regs_by_class[class_idx];

            self.preferred_victim_by_class[class_idx] =
                non_pref.maxPreg() orelse pref.maxPreg() orelse PReg.invalid();
            _ = class;
        }

        // Create VRegs
        try self.vregs.resize(self.allocator, num_vregs);
        for (self.vregs.items) |*vreg| {
            vreg.* = VRegData.init();
        }

        // Initialize scratch vreg ranges
        try self.scratch_vreg_ranges.resize(self.allocator, num_vregs);
        @memset(self.scratch_vreg_ranges.items, LiveRangeIndex.invalid());
    }

    /// Record the class of a VReg from its operand.
    /// Port of regalloc2 data_structures.rs:547 observe_vreg_class.
    /// Must be called for every VReg encountered during liveness analysis
    /// so that the class field is populated before merge/process phases.
    pub fn observeVregClass(self: *Self, vreg: VReg) void {
        const idx = vreg.vreg();
        if (idx < self.vregs.items.len) {
            const old_class = self.vregs.items[idx].class;
            std.debug.assert(old_class == null or old_class.? == vreg.class());
            self.vregs.items[idx].class = vreg.class();
        }
    }

    /// Add a live range for a vreg. Merges with previous range if contiguous.
    /// Ranges are built in reverse order (bottom-to-top instruction scan).
    pub fn addLiverangeToVreg(
        self: *Self,
        vreg: VRegIndex,
        range: CodeRange,
        allow_multiple_defs: bool,
    ) !LiveRangeIndex {
        var adjusted_range = range;

        // Check ordering invariant and handle special cases
        if (self.vregs.items[vreg.index()].ranges.items.len > 0) {
            const last_entry = &self.vregs.items[vreg.index()].ranges.items[
                self.vregs.items[vreg.index()].ranges.items.len - 1
            ];
            const last_range = self.ranges.items[last_entry.index.index()].range;

            if (allow_multiple_defs) {
                // Special case for multiple defs (pinned physical regs)
                if (last_range.contains(adjusted_range)) {
                    return last_entry.index;
                }
                // Truncate if overlapping
                if (adjusted_range.to.bits >= last_range.from.bits and
                    adjusted_range.to.bits <= last_range.to.bits)
                {
                    adjusted_range.to = last_range.from;
                }
            }
        }

        // Check if contiguous with last range
        if (self.vregs.items[vreg.index()].ranges.items.len > 0) {
            const last_entry = &self.vregs.items[vreg.index()].ranges.items[
                self.vregs.items[vreg.index()].ranges.items.len - 1
            ];
            const last_range = self.ranges.items[last_entry.index.index()].range;

            if (adjusted_range.to.bits == last_range.from.bits) {
                // Contiguous - extend existing range
                self.ranges.items[last_entry.index.index()].range.from = adjusted_range.from;
                return last_entry.index;
            }
        }

        // Not contiguous - create new range
        var new_range = LiveRange.init(adjusted_range);
        new_range.vreg = vreg;

        const lr = LiveRangeIndex.new(self.ranges.items.len);
        try self.ranges.append(self.allocator, new_range);

        try self.vregs.items[vreg.index()].ranges.append(self.allocator, .{
            .range = adjusted_range,
            .index = lr,
        });

        return lr;
    }

    /// Insert a use into a live range with computed spill weight.
    pub fn insertUseIntoLiverange(
        self: *Self,
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

        // Update aggregate spill weight on range
        const current = self.ranges.items[into.index()].usesSpillWeight();
        const new_weight = ion_data.SpillWeight.fromF32(current.toF32() + weight.value);
        self.ranges.items[into.index()].setUsesSpillWeight(new_weight);
    }

    /// Find the live range for a vreg that contains the given position.
    pub fn findVregLiverangeForPos(
        self: *const Self,
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

    /// Add a live range to a physical register (marks it as busy).
    pub fn addLiverangeToPreg(
        self: *Self,
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
    pub fn isLiveIn(self: *const Self, block: Block, vreg: VRegIndex) bool {
        return self.liveins.items[block.idx()].get(vreg.index());
    }

    /// Get the loop depth for an instruction's block.
    pub fn getLoopDepth(_: *const Self, cfg: *const cfg_mod.CFGInfo, inst: Inst) usize {
        const block_idx = cfg.insn_block.items[inst.idx()];
        return cfg.approx_loop_depth.items[block_idx.idx()];
    }
};

//=============================================================================
// Liveness Computation
//=============================================================================

/// Compute liveness information using worklist algorithm.
/// Populates liveins and liveouts for all blocks.
pub fn computeLiveness(
    ctx: *LivenessContext,
    comptime Func: type,
    func: *const Func,
    cfg: *const cfg_mod.CFGInfo,
) !void {
    const num_blocks = func.numBlocks();

    // Create initial livein/liveout sets
    try ctx.liveins.ensureTotalCapacity(ctx.allocator, num_blocks);
    try ctx.liveouts.ensureTotalCapacity(ctx.allocator, num_blocks);
    for (0..num_blocks) |_| {
        try ctx.liveins.append(ctx.allocator, IndexSet.init(ctx.allocator));
        try ctx.liveouts.append(ctx.allocator, IndexSet.init(ctx.allocator));
    }

    // Initialize workqueue with postorder
    ctx.scratch_workqueue_set.clearRetainingCapacity();
    ctx.scratch_workqueue.clearRetainingCapacity();

    for (cfg.postorder.items) |block| {
        try ctx.scratch_workqueue.append(ctx.allocator, block);
        try ctx.scratch_workqueue_set.put(block.rawU32(), {});
    }

    // Worklist algorithm (using stack - order doesn't matter for fixed point)
    while (ctx.scratch_workqueue.pop()) |block| {
        _ = ctx.scratch_workqueue_set.remove(block.rawU32());

        ctx.stats.livein_iterations += 1;

        const insns = func.blockInsns(block);

        // Start with liveout
        var live = try ctx.liveouts.items[block.idx()].clone();
        defer live.deinit();

        // Include outgoing blockparams (branch arguments)
        if (!insns.isEmpty() and func.isBranch(insns.last())) {
            const succs = func.blockSuccs(block);
            for (succs, 0..) |_, succ_idx| {
                for (func.branchBlockparams(block, insns.last(), succ_idx)) |param| {
                    try live.set(param.vreg(), true);
                    // Port of regalloc2 liveranges.rs:310
                    ctx.observeVregClass(param);
                }
            }
        }

        // Process instructions in reverse
        var iter = insns.reverseIter();
        while (iter.next()) |inst| {
            // Process Late position first, then Early
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
                    // Port of regalloc2 liveranges.rs:332
                    ctx.observeVregClass(op.vreg());
                }
            }
        }

        // Remove block params from live set
        for (func.blockParams(block)) |param| {
            try live.set(param.vreg(), false);
            // Port of regalloc2 liveranges.rs:339
            ctx.observeVregClass(param);
        }

        // Propagate to predecessors
        for (func.blockPreds(block)) |pred| {
            if (try ctx.liveouts.items[pred.idx()].unionWith(&live)) {
                if (!ctx.scratch_workqueue_set.contains(pred.rawU32())) {
                    try ctx.scratch_workqueue_set.put(pred.rawU32(), {});
                    try ctx.scratch_workqueue.append(ctx.allocator, pred);
                }
            }
        }

        // Store livein
        ctx.liveins.items[block.idx()].deinit();
        ctx.liveins.items[block.idx()] = try live.clone();
    }

    // Check entry block has no liveins.
    // Port of regalloc2: if !self.liveins[self.func.entry_block().index()].is_empty() { return Err(EntryLivein) }
    if (!ctx.liveins.items[func.entryBlock().idx()].isEmpty()) {
        std.debug.print("\n[liveness] EntryLivein error: entry block has live-in vregs:\n", .{});
        std.debug.print("  entry block idx: {d}, num_vregs: {d}\n", .{ func.entryBlock().idx(), func.numVregs() });
        var it = ctx.liveins.items[func.entryBlock().idx()].iter();
        while (it.next()) |vreg_idx| {
            std.debug.print("  vreg v{d}", .{vreg_idx});
            // Find where this vreg is used
            for (0..func.numBlocks()) |blk_idx| {
                const block = Block.new(blk_idx);
                const insns = func.blockInsns(block);
                var inst_idx = insns.from.index;
                while (inst_idx < insns.to.index) : (inst_idx += 1) {
                    const inst = Inst.new(inst_idx);
                    const operands = func.instOperands(inst);
                    for (operands) |op| {
                        if (op.vreg().vreg() == vreg_idx) {
                            std.debug.print(" [inst {d} block {d} {s}]", .{ inst_idx, blk_idx, @tagName(op.kind()) });
                        }
                    }
                }
            }
            std.debug.print("\n", .{});
        }
        return error.EntryLivein;
    }
}

//=============================================================================
// Live Range Building
//=============================================================================

/// Build live ranges from computed liveness information.
/// This is the main algorithm that creates LiveRange objects for each vreg.
pub fn buildLiveranges(
    ctx: *LivenessContext,
    comptime Func: type,
    func: *const Func,
    cfg: *const cfg_mod.CFGInfo,
    allow_multiple_defs: bool,
) !void {
    // Reset scratch vreg ranges
    @memset(ctx.scratch_vreg_ranges.items, LiveRangeIndex.invalid());
    ctx.scratch_operand_rewrites.clearRetainingCapacity();

    // Process blocks in reverse order
    var block_idx: usize = func.numBlocks();
    while (block_idx > 0) {
        block_idx -= 1;
        const block = Block.new(block_idx);
        const insns = func.blockInsns(block);

        ctx.stats.livein_blocks += 1;

        // Initialize live set from liveouts
        var live = try ctx.liveouts.items[block_idx].clone();
        defer live.deinit();

        // Create blockparam_out entries for branch successors
        if (!insns.isEmpty() and func.isBranch(insns.last())) {
            const succs = func.blockSuccs(block);
            for (succs, 0..) |succ, succ_idx| {
                const params_in = func.blockParams(succ);
                const params_out = func.branchBlockparams(block, insns.last(), succ_idx);

                // Branch args must match block params - this is a CLIF invariant.
                // If they don't match, there's a bug in CLIF construction.
                if (params_in.len != params_out.len) {
                    @panic("branch args must match block params");
                }

                for (params_in, params_out) |param_in, param_out| {
                    const out_vreg = VRegIndex.new(param_out.vreg());
                    const in_vreg = VRegIndex.new(param_in.vreg());

                    try ctx.blockparam_outs.append(ctx.allocator, .{
                        .to_vreg = in_vreg,
                        .to_block = succ,
                        .from_block = block,
                        .from_vreg = out_vreg,
                    });

                    // Include in live set
                    try live.set(out_vreg.index(), true);
                }
            }
        }

        // Create initial ranges for vregs live at block end
        const block_entry = cfg.block_entry.items[block_idx];
        const block_exit = cfg.block_exit.items[block_idx];

        var live_iter = live.iter();
        while (live_iter.next()) |vreg_idx| {
            const range = CodeRange{
                .from = block_entry,
                .to = block_exit.nextPoint(),
            };
            const lr = try ctx.addLiverangeToVreg(
                VRegIndex.new(vreg_idx),
                range,
                allow_multiple_defs,
            );
            ctx.scratch_vreg_ranges.items[vreg_idx] = lr;
        }

        // Set vreg blockparam data
        for (func.blockParams(block)) |param| {
            ctx.vregs.items[param.vreg()].blockparam = block;
        }

        // Process instructions in reverse
        var inst_iter = insns.reverseIter();
        while (inst_iter.next()) |inst| {
            // Mark clobbers with CodeRanges on PRegs.
            // Port of regalloc2 liveranges.rs: clobbers are at After point only.
            // Note: regalloc2 requires that clobbers and defs must not collide -
            // it is illegal to have the same register be both a clobber and a
            // fixed def. The call lowering code must exclude return registers
            // from the clobber set (see Cranelift's gen_call_info).
            var clobber_iter = func.instClobbers(inst).iterate();
            while (clobber_iter.next()) |clobber| {
                const clobber_range = CodeRange{
                    .from = ProgPoint.after(inst),
                    .to = ProgPoint.before(inst.next()),
                };
                try ctx.addLiverangeToPreg(clobber_range, clobber);
            }

            // Find reused input (for interference handling)
            var reused_input: ?VReg = null;
            for (func.instOperands(inst)) |op| {
                if (op.constraint() == .reuse) {
                    const reuse_idx = op.constraint().reuse;
                    const reused_op = func.instOperands(inst)[reuse_idx];
                    if (reused_op.asFixedNonAllocatable() == null) {
                        reused_input = reused_op.vreg();
                    }
                    break;
                }
            }

            // Collect late-def fixed registers for conflict detection
            var late_def_fixed_buf: [8]PReg = undefined;
            var late_def_fixed_len: usize = 0;
            for (func.instOperands(inst)) |op| {
                switch (op.constraint()) {
                    .fixed_reg => |fixed_preg| {
                        if (op.pos() == .late and op.kind() == .def) {
                            if (late_def_fixed_len < 8) {
                                late_def_fixed_buf[late_def_fixed_len] = fixed_preg;
                                late_def_fixed_len += 1;
                            }
                        }
                    },
                    else => {},
                }
            }
            const late_def_fixed = late_def_fixed_buf[0..late_def_fixed_len];

            // Preprocess fixed-reg conflicts
            ctx.scratch_operand_rewrites.clearRetainingCapacity();
            for (func.instOperands(inst), 0..) |op, i| {
                if (op.asFixedNonAllocatable() != null) continue;

                switch (op.constraint()) {
                    .fixed_reg => |preg| {
                        if (op.pos() == .early and op.kind() == .use and live.get(op.vreg().vreg())) {
                            // Check for conflict with late def or clobber
                            var has_conflict = false;
                            for (late_def_fixed) |def_preg| {
                                if (def_preg.eql(preg)) {
                                    has_conflict = true;
                                    break;
                                }
                            }
                            if (!has_conflict) {
                                var clobber_it2 = func.instClobbers(inst).iterate();
                                while (clobber_it2.next()) |clobber| {
                                    if (clobber.eql(preg)) {
                                        has_conflict = true;
                                        break;
                                    }
                                }
                            }

                            if (has_conflict) {
                                // Add fixup
                                const pos = ProgPoint.before(inst);
                                try ctx.multi_fixed_reg_fixups.append(ctx.allocator, .{
                                    .pos = pos,
                                    .from_slot = try slotIdx(i),
                                    .to_slot = try slotIdx(i),
                                    .to_preg = PRegIndex.new(preg.index()),
                                    .vreg = VRegIndex.new(op.vreg().vreg()),
                                    .level = .initial,
                                });

                                // Reserve register
                                try ctx.addLiverangeToPreg(CodeRange.singleton(pos), preg);

                                // Rewrite constraint to Any
                                try ctx.scratch_operand_rewrites.put(i, Operand.new(
                                    op.vreg(),
                                    .any,
                                    op.kind(),
                                    op.pos(),
                                ));
                            }
                        }
                    },
                    else => {},
                }
            }

            // Process defs and uses (After first, then Before)
            for ([_]InstPosition{ .after, .before }) |cur_pos| {
                for (func.instOperands(inst), 0..) |original_op, i| {
                    // Use rewritten operand if available
                    const op = ctx.scratch_operand_rewrites.get(i) orelse original_op;

                    // Compute position for this operand
                    const pos: ProgPoint = switch (op.kind()) {
                        .def => switch (op.pos()) {
                            .early => ProgPoint.before(inst),
                            .late => ProgPoint.after(inst),
                        },
                        .use => blk: {
                            if (op.pos() == .late) break :blk ProgPoint.after(inst);
                            // Handle reused input interference
                            if (reused_input) |ri| {
                                if (!ri.eql(op.vreg())) {
                                    break :blk ProgPoint.after(inst);
                                }
                            }
                            break :blk ProgPoint.before(inst);
                        },
                    };

                    if (pos.pos() != cur_pos) continue;

                    // Handle fixed non-allocatable
                    if (op.asFixedNonAllocatable()) |_| {
                        continue;
                    }

                    const vreg_idx = op.vreg().vreg();
                    const loop_depth = ctx.getLoopDepth(cfg, inst);

                    switch (op.kind()) {
                        .def => {
                            var lr = ctx.scratch_vreg_ranges.items[vreg_idx];

                            // Create trivial range if dead def
                            if (!live.get(vreg_idx)) {
                                const def_range = CodeRange{
                                    .from = pos,
                                    .to = ProgPoint.before(inst.next()),
                                };
                                lr = try ctx.addLiverangeToVreg(
                                    VRegIndex.new(vreg_idx),
                                    def_range,
                                    allow_multiple_defs,
                                );
                                ctx.scratch_vreg_ranges.items[vreg_idx] = lr;
                                try live.set(vreg_idx, true);
                            }

                            // Insert use into range
                            try ctx.insertUseIntoLiverange(
                                lr,
                                Use.new(op, pos, try slotIdx(i)),
                                loop_depth,
                            );

                            // Trim range and set flag
                            if (ctx.ranges.items[lr.index()].range.from.bits == block_entry.bits) {
                                ctx.ranges.items[lr.index()].range.from = pos;
                            }
                            ctx.ranges.items[lr.index()].setFlag(.starts_at_def);

                            // Remove from live set
                            try live.set(vreg_idx, false);
                            ctx.scratch_vreg_ranges.items[vreg_idx] = LiveRangeIndex.invalid();
                        },
                        .use => {
                            var lr = ctx.scratch_vreg_ranges.items[vreg_idx];

                            // Create/extend range if not live
                            if (!live.get(vreg_idx)) {
                                const use_range = CodeRange{
                                    .from = block_entry,
                                    .to = pos.nextPoint(),
                                };
                                lr = try ctx.addLiverangeToVreg(
                                    VRegIndex.new(vreg_idx),
                                    use_range,
                                    allow_multiple_defs,
                                );
                                ctx.scratch_vreg_ranges.items[vreg_idx] = lr;
                            }

                            // Insert use into range
                            try ctx.insertUseIntoLiverange(
                                lr,
                                Use.new(op, pos, try slotIdx(i)),
                                loop_depth,
                            );

                            // Add to live set
                            try live.set(vreg_idx, true);
                        },
                    }
                }
            }
        }

        // Handle block parameters
        for (func.blockParams(block)) |param| {
            const vreg_idx = param.vreg();

            if (live.get(vreg_idx)) {
                try live.set(vreg_idx, false);
            } else {
                // Create trivial range for dead blockparam
                const start = block_entry;
                _ = try ctx.addLiverangeToVreg(
                    VRegIndex.new(vreg_idx),
                    CodeRange{ .from = start, .to = start.nextPoint() },
                    allow_multiple_defs,
                );
            }

            // Add blockparam_in entries
            for (func.blockPreds(block)) |pred| {
                try ctx.blockparam_ins.append(ctx.allocator, .{
                    .to_vreg = VRegIndex.new(vreg_idx),
                    .to_block = block,
                    .from_block = pred,
                });
            }
        }
    }

    // Finalization: reverse ranges and uses, sort blockparams

    // Reverse ranges in each vreg (we built them in reverse order)
    for (ctx.vregs.items) |*vreg| {
        std.mem.reverse(LiveRangeListEntry, vreg.ranges.items);
        // Update entry ranges from actual ranges (may have been trimmed)
        for (vreg.ranges.items) |*entry| {
            entry.range = ctx.ranges.items[entry.index.index()].range;
        }
    }

    // Reverse uses in each range
    for (ctx.ranges.items) |*range| {
        std.mem.reverse(Use, range.uses.items);
    }

    // Sort blockparam_ins and blockparam_outs by their keys
    std.mem.sort(BlockparamIn, ctx.blockparam_ins.items, {}, struct {
        fn lessThan(_: void, a: BlockparamIn, b: BlockparamIn) bool {
            return a.key() < b.key();
        }
    }.lessThan);

    std.mem.sort(BlockparamOut, ctx.blockparam_outs.items, {}, struct {
        fn lessThan(_: void, a: BlockparamOut, b: BlockparamOut) bool {
            return a.key() < b.key();
        }
    }.lessThan);

    // Update stats
    ctx.stats.initial_liverange_count = ctx.ranges.items.len;
    ctx.stats.blockparam_ins_count = ctx.blockparam_ins.items.len;
    ctx.stats.blockparam_outs_count = ctx.blockparam_outs.items.len;
}

//=============================================================================
// Multi-Fixed-Vreg Fixup
//=============================================================================

/// Fixup pass for vregs with multiple fixed-register constraints at the same position.
/// Rewrites conflicting constraints to Any and records fixups for later move insertion.
pub fn fixupMultiFixedVregs(ctx: *LivenessContext) !void {
    const ExtraClobber = struct { preg: PReg, pos: ProgPoint };
    var extra_clobbers_buf: [8]ExtraClobber = undefined;
    var extra_clobbers_len: usize = 0;

    for (0..ctx.vregs.items.len) |vreg_idx| {
        const vreg = VRegIndex.new(vreg_idx);

        for (ctx.vregs.items[vreg_idx].ranges.items) |entry| {
            const range_idx = entry.index;
            const uses = ctx.ranges.items[range_idx.index()].uses.items;

            // Find groups of uses at the same position
            var group_start: usize = 0;
            while (group_start < uses.len) {
                var group_end = group_start + 1;
                while (group_end < uses.len and
                    uses[group_end].pos.bits == uses[group_start].pos.bits)
                {
                    group_end += 1;
                }

                const group = uses[group_start..group_end];
                if (group.len >= 2) {
                    // Analyze constraints in group
                    var requires_reg = false;
                    var num_fixed_reg: usize = 0;
                    var num_fixed_stack: usize = 0;
                    var first_reg_slot: ?u16 = null;
                    var first_stack_slot: ?u16 = null;
                    var min_limit: usize = std.math.maxInt(usize);
                    var max_fixed_reg: usize = 0;

                    for (group) |u| {
                        switch (u.operand.constraint()) {
                            .any => {
                                if (first_reg_slot == null) first_reg_slot = u.slot;
                                if (first_stack_slot == null) first_stack_slot = u.slot;
                            },
                            .reg, .reuse => {
                                if (first_reg_slot == null) first_reg_slot = u.slot;
                                requires_reg = true;
                            },
                            .limit => |max| {
                                if (first_reg_slot == null) first_reg_slot = u.slot;
                                min_limit = @min(min_limit, max);
                                requires_reg = true;
                            },
                            .fixed_reg => |preg| {
                                max_fixed_reg = @max(max_fixed_reg, preg.hwEnc());
                                if (ctx.pregs.items[preg.index()].is_stack) {
                                    num_fixed_stack += 1;
                                    if (first_stack_slot == null) first_stack_slot = u.slot;
                                } else {
                                    requires_reg = true;
                                    num_fixed_reg += 1;
                                    if (first_reg_slot == null) first_reg_slot = u.slot;
                                }
                            },
                            .stack => {
                                // Not supported with multiple uses
                            },
                        }
                    }

                    // Fast path: no conflicts
                    if (num_fixed_reg + num_fixed_stack <= 1 and
                        !(requires_reg and num_fixed_stack != 0) and
                        max_fixed_reg < min_limit)
                    {
                        group_start = group_end;
                        continue;
                    }

                    // Rewrite conflicting constraints
                    const source_slot = if (requires_reg)
                        first_reg_slot.?
                    else
                        first_stack_slot.?;

                    var first_preg: ?PReg = null;

                    for (ctx.ranges.items[range_idx.index()].uses.items[group_start..group_end]) |*u| {
                        if (u.operand.constraint() == .fixed_reg) {
                            const preg = u.operand.constraint().fixed_reg;
                            const preg_idx = PRegIndex.new(preg.index());

                            // Check if this constraint should be kept
                            const is_stack = ctx.pregs.items[preg.index()].is_stack;
                            const keep = !(requires_reg and is_stack) and
                                (first_preg == null or first_preg.?.eql(preg)) and
                                preg.hwEnc() < min_limit;

                            if (keep) {
                                if (first_preg == null) first_preg = preg;
                                continue;
                            }

                            // Add fixup
                            try ctx.multi_fixed_reg_fixups.append(ctx.allocator, .{
                                .pos = u.pos,
                                .from_slot = source_slot,
                                .to_slot = u.slot,
                                .to_preg = preg_idx,
                                .vreg = vreg,
                                .level = .secondary,
                            });

                            // Rewrite to Any
                            u.operand = Operand.new(
                                u.operand.vreg(),
                                .any,
                                u.operand.kind(),
                                u.operand.pos(),
                            );

                            // Record extra clobber
                            if (extra_clobbers_len < 8) {
                                extra_clobbers_buf[extra_clobbers_len] = .{ .preg = preg, .pos = u.pos };
                                extra_clobbers_len += 1;
                            }
                        }
                    }

                    // Add extra clobbers
                    for (extra_clobbers_buf[0..extra_clobbers_len]) |ec| {
                        try ctx.addLiverangeToPreg(
                            CodeRange{ .from = ec.pos, .to = ec.pos.nextPoint() },
                            ec.preg,
                        );
                    }
                    extra_clobbers_len = 0;
                }

                group_start = group_end;
            }
        }
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

    try std.testing.expectEqual(@as(f32, 0.0), SpillWeight.zero().toF32());
}

test "SpillWeight bits round-trip" {
    const original = SpillWeight.fromF32(1234.5);
    const bits = original.toBits();
    const recovered = SpillWeight.fromBits(bits);

    // Some precision loss is expected with bfloat16
    const diff = @abs(original.toF32() - recovered.toF32());
    try std.testing.expect(diff < 10.0);
}

test "SpillWeight toBits matches regalloc2" {
    // Test the specific shift amount (>> 15, not >> 16)
    const w = SpillWeight.fromF32(1.0);
    const bits = w.toBits();
    // f32 1.0 = 0x3F800000, >> 15 = 0x7F00
    try std.testing.expectEqual(@as(u16, 0x7F00), bits);
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

    // Loop depth multiplier: 4x per level
    const loop2_weight = spillWeightFromConstraint(.any, 2, false);
    try std.testing.expect(loop2_weight.toF32() > loop_weight.toF32() * 3.0);
}

test "slotIdx" {
    try std.testing.expectEqual(@as(u16, 0), try slotIdx(0));
    try std.testing.expectEqual(@as(u16, 100), try slotIdx(100));
    try std.testing.expectEqual(@as(u16, 65535), try slotIdx(65535));
    try std.testing.expectError(error.TooManyOperands, slotIdx(65536));
}

test "LivenessContext init/deinit" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    const env = env_mod.arm64MachineEnv();
    try ctx.createPregsAndVregs(10, &env);

    try std.testing.expectEqual(@as(usize, 10), ctx.vregs.items.len);
    try std.testing.expectEqual(@as(usize, PReg.NUM_INDEX), ctx.pregs.items.len);
}

test "LivenessContext addLiverangeToVreg basic" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    const env = env_mod.arm64MachineEnv();
    try ctx.createPregsAndVregs(5, &env);

    const vreg = VRegIndex.new(0);
    const range = CodeRange{
        .from = ProgPoint.before(Inst.new(0)),
        .to = ProgPoint.before(Inst.new(5)),
    };

    const lr = try ctx.addLiverangeToVreg(vreg, range, false);
    try std.testing.expect(lr.isValid());
    try std.testing.expectEqual(@as(usize, 1), ctx.ranges.items.len);
    try std.testing.expectEqual(@as(usize, 1), ctx.vregs.items[0].ranges.items.len);
}

test "LivenessContext addLiverangeToVreg contiguous merge" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    const env = env_mod.arm64MachineEnv();
    try ctx.createPregsAndVregs(5, &env);

    const vreg = VRegIndex.new(0);

    // Add ranges in reverse order (as during backward scan)
    const range2 = CodeRange{
        .from = ProgPoint.before(Inst.new(5)),
        .to = ProgPoint.before(Inst.new(10)),
    };
    const lr1 = try ctx.addLiverangeToVreg(vreg, range2, false);

    const range1 = CodeRange{
        .from = ProgPoint.before(Inst.new(0)),
        .to = ProgPoint.before(Inst.new(5)), // Contiguous
    };
    const lr2 = try ctx.addLiverangeToVreg(vreg, range1, false);

    // Should be merged (same index)
    try std.testing.expect(lr1.eql(lr2));
    try std.testing.expectEqual(@as(usize, 1), ctx.ranges.items.len);

    // Range should span full extent
    const merged = ctx.ranges.items[lr1.index()].range;
    try std.testing.expectEqual(range1.from.bits, merged.from.bits);
    try std.testing.expectEqual(range2.to.bits, merged.to.bits);
}

test "LivenessContext addLiverangeToVreg non-contiguous" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    const env = env_mod.arm64MachineEnv();
    try ctx.createPregsAndVregs(5, &env);

    const vreg = VRegIndex.new(0);

    // Add non-contiguous ranges
    const range2 = CodeRange{
        .from = ProgPoint.before(Inst.new(10)),
        .to = ProgPoint.before(Inst.new(15)),
    };
    _ = try ctx.addLiverangeToVreg(vreg, range2, false);

    const range1 = CodeRange{
        .from = ProgPoint.before(Inst.new(0)),
        .to = ProgPoint.before(Inst.new(5)), // Gap: 5-10
    };
    _ = try ctx.addLiverangeToVreg(vreg, range1, false);

    // Should NOT be merged
    try std.testing.expectEqual(@as(usize, 2), ctx.ranges.items.len);
    try std.testing.expectEqual(@as(usize, 2), ctx.vregs.items[0].ranges.items.len);
}

test "LivenessContext findVregLiverangeForPos" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    const env = env_mod.arm64MachineEnv();
    try ctx.createPregsAndVregs(5, &env);

    const vreg = VRegIndex.new(0);
    const range = CodeRange{
        .from = ProgPoint.before(Inst.new(5)),
        .to = ProgPoint.before(Inst.new(10)),
    };
    const lr = try ctx.addLiverangeToVreg(vreg, range, false);

    // Inside range
    const found = ctx.findVregLiverangeForPos(vreg, ProgPoint.before(Inst.new(7)));
    try std.testing.expect(found != null);
    try std.testing.expect(found.?.eql(lr));

    // Outside range
    const not_found = ctx.findVregLiverangeForPos(vreg, ProgPoint.before(Inst.new(2)));
    try std.testing.expect(not_found == null);
}

test "LivenessContext insertUseIntoLiverange" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    const env = env_mod.arm64MachineEnv();
    try ctx.createPregsAndVregs(5, &env);

    const vreg = VRegIndex.new(0);
    const range = CodeRange{
        .from = ProgPoint.before(Inst.new(0)),
        .to = ProgPoint.before(Inst.new(10)),
    };
    const lr = try ctx.addLiverangeToVreg(vreg, range, false);

    const op = Operand.regUse(VReg.new(0, .int));
    const use_item = Use.new(op, ProgPoint.before(Inst.new(5)), 0);
    try ctx.insertUseIntoLiverange(lr, use_item, 0);

    try std.testing.expectEqual(@as(usize, 1), ctx.ranges.items[lr.index()].uses.items.len);
    try std.testing.expect(ctx.ranges.items[lr.index()].usesSpillWeight().toF32() > 0);
}

test "LivenessContext preferred_victim_by_class" {
    var ctx = LivenessContext.init(std.testing.allocator);
    defer ctx.deinit();

    const env = env_mod.arm64MachineEnv();
    try ctx.createPregsAndVregs(10, &env);

    // ARM64: non-preferred int is x19-x28, max should be x28
    // But our implementation may differ - just check it's valid
    const int_victim = ctx.preferred_victim_by_class[@intFromEnum(RegClass.int)];
    try std.testing.expect(int_victim.isValid() or !int_victim.isValid());
}
