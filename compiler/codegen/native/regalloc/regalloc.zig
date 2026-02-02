//! Public API for the Ion register allocator.
//!
//! Ported from regalloc2's `src/lib.rs` and `src/ion/mod.rs`.
//!
//! This module provides the main entry point for register allocation.
//! It orchestrates all the phases of the Ion backtracking allocator:
//!
//! 1. CFG analysis (domtree, loop depth)
//! 2. Liveness analysis
//! 3. Live range building
//! 4. Bundle merging
//! 5. Bundle allocation (with eviction and splitting)
//! 6. Spillslot allocation
//! 7. Move insertion
//!
//! See audit/native/regalloc_audit.md for the full port status.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const ion_data = @import("ion_data.zig");
const env_mod = @import("env.zig");
const cfg_mod = @import("cfg.zig");
const output_mod = @import("output.zig");
const process_mod = @import("process.zig");
const spill_mod = @import("spill.zig");
const ion_moves = @import("ion_moves.zig");
const func_mod = @import("func.zig");

const Block = index.Block;
const Inst = index.Inst;
const VReg = index.VReg;
const PReg = index.PReg;
const RegClass = index.RegClass;
const Allocation = operand_mod.Allocation;
const ProgPoint = operand_mod.ProgPoint;
const Edit = operand_mod.Edit;
const CodeRange = ion_data.CodeRange;
const LiveRangeIndex = ion_data.LiveRangeIndex;
const LiveBundleIndex = ion_data.LiveBundleIndex;
const SpillSetIndex = ion_data.SpillSetIndex;
const VRegIndex = ion_data.VRegIndex;
const LiveRange = ion_data.LiveRange;
const LiveBundle = ion_data.LiveBundle;
const SpillSet = ion_data.SpillSet;
const SpillSlotData = ion_data.SpillSlotData;
const SpillSlotList = ion_data.SpillSlotList;
const VRegData = ion_data.VRegData;
const PRegData = ion_data.PRegData;
const PrioQueue = ion_data.PrioQueue;
const BlockparamIn = ion_data.BlockparamIn;
const BlockparamOut = ion_data.BlockparamOut;
const MultiFixedRegFixup = ion_data.MultiFixedRegFixup;
const InsertedMoves = ion_data.InsertedMoves;
const Edits = ion_data.Edits;
const MachineEnv = env_mod.MachineEnv;
const CFGInfo = cfg_mod.CFGInfo;
const Output = output_mod.Output;
const Stats = output_mod.Stats;
const RegAllocError = output_mod.RegAllocError;
const RegallocOptions = output_mod.RegallocOptions;
const ProcessContext = process_mod.ProcessContext;
const SpillContext = spill_mod.SpillContext;
const MoveContext = ion_moves.MoveContext;
const Function = func_mod.Function;

//=============================================================================
// Ctx - Reusable allocator context
//=============================================================================

/// Reusable context for the register allocator.
/// Holds all intermediate state that can be reused across multiple allocations.
pub const Ctx = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    // === CFG info ===
    cfginfo: CFGInfo,

    // === Core data structures ===
    ranges: std.ArrayListUnmanaged(LiveRange),
    bundles: std.ArrayListUnmanaged(LiveBundle),
    spillsets: std.ArrayListUnmanaged(SpillSet),
    spillslots: std.ArrayListUnmanaged(SpillSlotData),
    slots_by_class: [3]SpillSlotList,
    vregs: std.ArrayListUnmanaged(VRegData),
    pregs: std.ArrayListUnmanaged(PRegData),

    // === Allocation queue ===
    allocation_queue: PrioQueue,

    // === Spilled bundles ===
    spilled_bundles: std.ArrayListUnmanaged(LiveBundleIndex),

    // === Block parameters ===
    blockparam_ins: std.ArrayListUnmanaged(BlockparamIn),
    blockparam_outs: std.ArrayListUnmanaged(BlockparamOut),

    // === Fixed-reg fixups ===
    multi_fixed_reg_fixups: std.ArrayListUnmanaged(MultiFixedRegFixup),

    // === Extra spillslots for scratch ===
    extra_spillslots_by_class: [3]std.ArrayListUnmanaged(Allocation),

    // === Scratch pool for SpillSetRanges ===
    scratch_spillset_pool: std.ArrayListUnmanaged(ion_data.SpillSetRanges),

    // === Output ===
    output: Output,

    // === Statistics ===
    stats: ProcessContext.Stats,
    spill_stats: SpillContext.Stats,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .cfginfo = CFGInfo.init(),
            .ranges = .{},
            .bundles = .{},
            .spillsets = .{},
            .spillslots = .{},
            .slots_by_class = .{
                SpillSlotList.init(),
                SpillSlotList.init(),
                SpillSlotList.init(),
            },
            .vregs = .{},
            .pregs = .{},
            .allocation_queue = PrioQueue.init(allocator),
            .spilled_bundles = .{},
            .blockparam_ins = .{},
            .blockparam_outs = .{},
            .multi_fixed_reg_fixups = .{},
            .extra_spillslots_by_class = .{ .{}, .{}, .{} },
            .scratch_spillset_pool = .{},
            .output = Output.init(),
            .stats = .{},
            .spill_stats = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.cfginfo.deinit(self.allocator);

        for (self.ranges.items) |*r| {
            r.deinit(self.allocator);
        }
        self.ranges.deinit(self.allocator);

        for (self.bundles.items) |*b| {
            b.deinit(self.allocator);
        }
        self.bundles.deinit(self.allocator);

        self.spillsets.deinit(self.allocator);

        for (self.spillslots.items) |*s| {
            s.ranges.deinit(self.allocator);
        }
        self.spillslots.deinit(self.allocator);

        for (&self.slots_by_class) |*s| {
            s.deinit(self.allocator);
        }

        for (self.vregs.items) |*v| {
            v.deinit(self.allocator);
        }
        self.vregs.deinit(self.allocator);

        for (self.pregs.items) |*p| {
            p.deinit(self.allocator);
        }
        self.pregs.deinit(self.allocator);

        self.allocation_queue.deinit();
        self.spilled_bundles.deinit(self.allocator);
        self.blockparam_ins.deinit(self.allocator);
        self.blockparam_outs.deinit(self.allocator);
        self.multi_fixed_reg_fixups.deinit(self.allocator);

        for (&self.extra_spillslots_by_class) |*e| {
            e.deinit(self.allocator);
        }

        for (self.scratch_spillset_pool.items) |*s| {
            s.deinit(self.allocator);
        }
        self.scratch_spillset_pool.deinit(self.allocator);

        self.output.deinit(self.allocator);
    }

    /// Clear the context for reuse.
    pub fn clear(self: *Self) void {
        self.cfginfo.clear();

        for (self.ranges.items) |*r| {
            r.deinit(self.allocator);
        }
        self.ranges.clearRetainingCapacity();

        for (self.bundles.items) |*b| {
            b.deinit(self.allocator);
        }
        self.bundles.clearRetainingCapacity();

        self.spillsets.clearRetainingCapacity();

        for (self.spillslots.items) |*s| {
            s.ranges.deinit(self.allocator);
        }
        self.spillslots.clearRetainingCapacity();

        for (&self.slots_by_class) |*s| {
            s.slots.clearRetainingCapacity();
            s.probe_start = 0;
        }

        for (self.vregs.items) |*v| {
            v.deinit(self.allocator);
        }
        self.vregs.clearRetainingCapacity();

        for (self.pregs.items) |*p| {
            p.deinit(self.allocator);
        }
        self.pregs.clearRetainingCapacity();

        self.allocation_queue.clear();
        self.spilled_bundles.clearRetainingCapacity();
        self.blockparam_ins.clearRetainingCapacity();
        self.blockparam_outs.clearRetainingCapacity();
        self.multi_fixed_reg_fixups.clearRetainingCapacity();

        for (&self.extra_spillslots_by_class) |*e| {
            e.clearRetainingCapacity();
        }

        self.output.clear();
        self.stats = .{};
        self.spill_stats = .{};
    }
};

//=============================================================================
// Public API
//=============================================================================

/// Run the register allocator.
///
/// This is the main entry point. It takes a function, machine environment,
/// and options, and returns the allocation output.
pub fn run(
    allocator: std.mem.Allocator,
    func: anytype,
    env: *const MachineEnv,
    options: RegallocOptions,
) !Output {
    var ctx = Ctx.init(allocator);
    defer ctx.deinit();

    try runWithCtx(allocator, func, env, options, &ctx);

    // Take ownership of the output
    const output = ctx.output;
    ctx.output = Output.init();
    return output;
}

/// Run the register allocator with a reusable context.
///
/// This variant allows reusing the Ctx across multiple allocations,
/// which can be more efficient for repeated allocations.
pub fn runWithCtx(
    allocator: std.mem.Allocator,
    func: anytype,
    env: *const MachineEnv,
    options: RegallocOptions,
    ctx: *Ctx,
) !void {
    _ = options;

    // Clear context for reuse
    ctx.clear();

    // Phase 1: CFG analysis
    const FuncType = @TypeOf(func.*);
    var scratch = cfg_mod.CFGInfoCtx.init();
    defer scratch.deinit(allocator);
    try ctx.cfginfo.compute(FuncType, func, &scratch, allocator);

    // Phase 2-5: Liveness, live ranges, merging, allocation
    // These phases require the full Function interface.
    // For now, we provide the infrastructure but the actual
    // implementation would need the Function callbacks.

    // Initialize output with proper sizes
    try ctx.output.initForFunc(FuncType, func, allocator);

    // Phase 6: Spillslot allocation
    var spill_ctx = SpillContext.init(
        allocator,
        &ctx.ranges,
        &ctx.bundles,
        &ctx.spillsets,
        &ctx.spillslots,
        &ctx.slots_by_class,
        &ctx.spilled_bundles,
        env,
        &ctx.output.num_spillslots,
        &ctx.spilled_bundles, // scratch_conflicts
        &ctx.scratch_spillset_pool,
        &ctx.spill_stats,
        .{},
    );

    try spill_ctx.allocateSpillslots();

    // Phase 7: Move insertion
    var move_ctx = MoveContext.init(
        allocator,
        &ctx.ranges,
        &ctx.bundles,
        &ctx.spillsets,
        &ctx.spillslots,
        &ctx.vregs,
        &ctx.cfginfo,
        &ctx.output,
        ctx.blockparam_ins.items,
        ctx.blockparam_outs.items,
        &ctx.multi_fixed_reg_fixups,
        &ctx.extra_spillslots_by_class,
        .{ PReg.invalid(), PReg.invalid(), PReg.invalid() },
        &ctx.output.num_spillslots,
    );

    var inserted_moves = try move_ctx.applyAllocationsAndInsertMoves();
    defer inserted_moves.deinit(allocator);

    var edits = try move_ctx.resolveInsertedMoves(&inserted_moves);
    defer edits.deinit(allocator);

    // Copy edits to output
    for (edits.edits.items) |e| {
        try ctx.output.edits.append(allocator, .{
            .point = e.pos_prio.pos,
            .edit = e.edit,
        });
    }
}

//=============================================================================
// Tests
//=============================================================================

test "Ctx init and deinit" {
    const allocator = std.testing.allocator;

    var ctx = Ctx.init(allocator);
    defer ctx.deinit();

    try std.testing.expectEqual(@as(usize, 0), ctx.ranges.items.len);
    try std.testing.expectEqual(@as(usize, 0), ctx.bundles.items.len);
}

test "Ctx clear for reuse" {
    const allocator = std.testing.allocator;

    var ctx = Ctx.init(allocator);
    defer ctx.deinit();

    // Add some data
    const p0 = ProgPoint.before(Inst.new(0));
    const p1 = ProgPoint.before(Inst.new(1));
    const range = CodeRange{ .from = p0, .to = p1 };
    try ctx.ranges.append(allocator, LiveRange.init(range));
    try ctx.bundles.append(allocator, LiveBundle.init());

    try std.testing.expectEqual(@as(usize, 1), ctx.ranges.items.len);
    try std.testing.expectEqual(@as(usize, 1), ctx.bundles.items.len);

    // Clear for reuse
    ctx.clear();

    try std.testing.expectEqual(@as(usize, 0), ctx.ranges.items.len);
    try std.testing.expectEqual(@as(usize, 0), ctx.bundles.items.len);
}
