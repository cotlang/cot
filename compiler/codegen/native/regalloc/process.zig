//! Allocation loop - main bundle processing.
//!
//! Ported from regalloc2's `src/ion/process.rs` and `src/ion/reg_traversal.rs`.
//!
//! This module implements the core allocation loop that processes bundles
//! and allocates them to physical registers. It includes:
//! - Register traversal iterator for trying registers in preferred order
//! - Bundle allocation attempts with conflict detection
//! - Bundle splitting when allocation fails
//! - Eviction of lower-priority bundles
//!
//! See audit/native/process_audit.md for full function-by-function mapping.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const ion_data = @import("ion_data.zig");
const env_mod = @import("env.zig");
const merge_mod = @import("merge.zig");
const liveness_mod = @import("liveness.zig");
const indexset = @import("indexset.zig");
const cfg_mod = @import("cfg.zig");

const Block = index.Block;
const Inst = index.Inst;
const InstPosition = operand_mod.InstPosition;
const VReg = index.VReg;
const PReg = index.PReg;
const PRegSet = index.PRegSet;
const RegClass = index.RegClass;
const Operand = operand_mod.Operand;
const OperandConstraint = operand_mod.OperandConstraint;
const OperandKind = operand_mod.OperandKind;
const Allocation = operand_mod.Allocation;
const ProgPoint = operand_mod.ProgPoint;
const CodeRange = ion_data.CodeRange;
const LiveRangeIndex = ion_data.LiveRangeIndex;
const LiveBundleIndex = ion_data.LiveBundleIndex;
const SpillSetIndex = ion_data.SpillSetIndex;
const VRegIndex = ion_data.VRegIndex;
const PRegIndex = ion_data.PRegIndex;
const LiveRange = ion_data.LiveRange;
const LiveBundle = ion_data.LiveBundle;
const LiveRangeListEntry = ion_data.LiveRangeListEntry;
const SpillSet = ion_data.SpillSet;
const VRegData = ion_data.VRegData;
const PRegData = ion_data.PRegData;
const LiveRangeSet = ion_data.LiveRangeSet;
const LiveRangeKey = ion_data.LiveRangeKey;
const PrioQueue = ion_data.PrioQueue;
const Use = ion_data.Use;
const SpillWeight = ion_data.SpillWeight;
const LiveRangeFlag = ion_data.LiveRangeFlag;
const MachineEnv = env_mod.MachineEnv;
const Requirement = merge_mod.Requirement;
const RequirementConflictAt = merge_mod.RequirementConflictAt;
const RawIndexSet = indexset.IndexSet;

/// Typed wrapper for IndexSet that works with strongly-typed index types.
fn IndexSet(comptime T: type) type {
    return struct {
        const Self = @This();
        inner: RawIndexSet,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .inner = RawIndexSet.init(allocator),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.inner.deinit();
        }

        pub fn insert(self: *Self, allocator: std.mem.Allocator, idx: T) !bool {
            _ = allocator;
            const was_set = self.inner.get(idx.index());
            try self.inner.set(idx.index(), true);
            return !was_set;
        }

        pub fn contains(self: *const Self, idx: T) bool {
            return self.inner.get(idx.index());
        }

        pub fn clear(self: *Self) void {
            // Reset by creating new empty set (since RawIndexSet doesn't have clear)
            self.inner.deinit();
            self.inner = RawIndexSet.init(self.allocator);
        }

        pub const Iterator = struct {
            inner_iter: RawIndexSet.Iterator,

            pub fn next(iter: *Iterator) ?T {
                if (iter.inner_iter.next()) |idx| {
                    return T.new(idx);
                }
                return null;
            }
        };

        pub fn iterator(self: *const Self) Iterator {
            return .{ .inner_iter = self.inner.iter() };
        }
    };
}
const CFGInfo = cfg_mod.CFGInfo;

// Spill weight constants
const BUNDLE_MAX_SPILL_WEIGHT = ion_data.BUNDLE_MAX_SPILL_WEIGHT;
const MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT = ion_data.MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT;
const MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT = ion_data.MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT;
const MINIMAL_BUNDLE_SPILL_WEIGHT = ion_data.MINIMAL_BUNDLE_SPILL_WEIGHT;
const BUNDLE_MAX_NORMAL_SPILL_WEIGHT = ion_data.BUNDLE_MAX_NORMAL_SPILL_WEIGHT;
const MAX_SPLITS_PER_SPILLSET = ion_data.MAX_SPLITS_PER_SPILLSET;

//=============================================================================
// RegAllocError - Error type for allocation failures
//=============================================================================

pub const RegAllocError = error{
    TooManyLiveRegs,
    OutOfMemory,
};

//=============================================================================
// AllocRegResult - Result of allocation attempt
//=============================================================================

/// Result of attempting to allocate a bundle to a register.
pub const AllocRegResult = union(enum) {
    /// Successfully allocated.
    allocated: Allocation,

    /// Conflicts with other bundles at the given point.
    conflict: struct {
        bundles: []const LiveBundleIndex,
        point: ProgPoint,
    },

    /// Conflicts with a fixed allocation.
    conflict_with_fixed: struct {
        max_cost: u32,
        point: ProgPoint,
    },

    /// Conflict cost exceeds threshold; skip this register.
    conflict_high_cost,
};

//=============================================================================
// Cursor - Helper for RegTraversalIter
//=============================================================================

/// Cursor for iterating through registers with offset-based ordering.
const Cursor = struct {
    /// Iterator for registers after the offset.
    first: PRegSet,
    first_iter_started: bool,
    /// Iterator for registers before the offset.
    second: PRegSet,
    second_iter_started: bool,
    class: RegClass,
    first_cursor: PRegSetCursor,
    second_cursor: PRegSetCursor,

    const PRegSetCursor = struct {
        bits: [PRegSet.LEN]u64,
        current_idx: usize,

        fn init(set: PRegSet) PRegSetCursor {
            return .{ .bits = set.bits, .current_idx = 0 };
        }

        fn next(self: *PRegSetCursor) ?PReg {
            while (self.current_idx < PRegSet.LEN) {
                if (self.bits[self.current_idx] != 0) {
                    const bit: u6 = @intCast(@ctz(self.bits[self.current_idx]));
                    self.bits[self.current_idx] &= ~(@as(u64, 1) << bit);
                    const idx: usize = @as(usize, bit) + self.current_idx * 64;
                    return PReg.fromIndex(idx);
                }
                self.current_idx += 1;
            }
            return null;
        }
    };

    fn init(registers: PRegSet, class: RegClass, offset_hint: usize) Cursor {
        // Create mask for registers up to offset
        var mask = PRegSet.empty();
        const offset = offset_hint % (PReg.MAX + 1);
        for (0..offset) |i| {
            mask.add(PReg.new(i, class));
        }

        // first = registers after offset (mask inverted)
        // second = registers before offset (mask)
        const first = registers.intersectWith(mask.invert());
        const second = registers.intersectWith(mask);

        return .{
            .first = first,
            .first_iter_started = false,
            .second = second,
            .second_iter_started = false,
            .class = class,
            .first_cursor = PRegSetCursor.init(first),
            .second_cursor = PRegSetCursor.init(second),
        };
    }

    fn next(self: *Cursor) ?PReg {
        if (self.first_cursor.next()) |preg| {
            return preg;
        }
        return self.second_cursor.next();
    }
};

//=============================================================================
// RegTraversalIter - Iterate registers in allocation order
//=============================================================================

/// Iterator for traversing registers in preferred allocation order.
///
/// The order is:
/// 1. If fixed, return that register only
/// 2. If there's a hint, try that first
/// 3. Try preferred registers (caller-saved) in offset order
/// 4. Try non-preferred registers (callee-saved) in offset order
pub const RegTraversalIter = struct {
    is_fixed: bool,
    fixed: ?PReg,
    fixed_returned: bool,
    use_hint: bool,
    hint: ?PReg,
    hint_returned: bool,
    preferred: Cursor,
    non_preferred: Cursor,
    limit: ?usize,

    pub fn init(
        env: *const MachineEnv,
        class: RegClass,
        fixed: ?PReg,
        hint: ?PReg,
        offset: usize,
        limit: ?usize,
    ) RegTraversalIter {
        const class_index = @intFromEnum(class);
        return .{
            .is_fixed = fixed != null,
            .fixed = fixed,
            .fixed_returned = false,
            .use_hint = hint != null,
            .hint = hint,
            .hint_returned = false,
            .preferred = Cursor.init(env.preferred_regs_by_class[class_index], class, offset),
            .non_preferred = Cursor.init(env.non_preferred_regs_by_class[class_index], class, offset),
            .limit = limit,
        };
    }

    pub fn next(self: *RegTraversalIter) ?PReg {
        // If fixed, return that register only (once)
        if (self.is_fixed) {
            if (!self.fixed_returned) {
                self.fixed_returned = true;
                return self.fixed;
            }
            return null;
        }

        // Try hint first (if within limit)
        if (self.use_hint and !self.hint_returned) {
            self.hint_returned = true;
            if (self.hint) |h| {
                if (h.hwEnc() < (self.limit orelse std.math.maxInt(usize))) {
                    return h;
                }
            }
        }

        // Try preferred registers
        while (self.preferred.next()) |reg| {
            // Skip if same as hint or outside limit
            if (self.hint) |h| {
                if (reg.eql(h)) continue;
            }
            if (reg.hwEnc() >= (self.limit orelse std.math.maxInt(usize))) {
                continue;
            }
            return reg;
        }

        // Try non-preferred registers
        while (self.non_preferred.next()) |reg| {
            // Skip if same as hint or outside limit
            if (self.hint) |h| {
                if (reg.eql(h)) continue;
            }
            if (reg.hwEnc() >= (self.limit orelse std.math.maxInt(usize))) {
                continue;
            }
            return reg;
        }

        return null;
    }
};

//=============================================================================
// ProcessContext - Context for allocation loop
//=============================================================================

/// Context for the allocation loop.
pub const ProcessContext = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    // === Core data structures ===
    ranges: *std.ArrayListUnmanaged(LiveRange),
    bundles: *std.ArrayListUnmanaged(LiveBundle),
    spillsets: *std.ArrayListUnmanaged(SpillSet),
    vregs: *std.ArrayListUnmanaged(VRegData),
    pregs: *std.ArrayListUnmanaged(PRegData),

    // === Machine environment ===
    env: *const MachineEnv,

    // === Allocation queue ===
    allocation_queue: *PrioQueue,

    // === CFG info for loop depth ===
    cfginfo: *const CFGInfo,

    // === Number of instructions ===
    num_insts: usize,

    // === Spilled bundles ===
    spilled_bundles: *std.ArrayListUnmanaged(LiveBundleIndex),

    // === Scratch memory (reused across calls) ===
    scratch_conflicts: std.ArrayListUnmanaged(LiveBundleIndex),
    scratch_bundle: std.ArrayListUnmanaged(LiveBundleIndex),
    conflict_set: IndexSet(LiveBundleIndex),
    scratch_removed_lrs: IndexSet(LiveRangeIndex),
    scratch_removed_lrs_vregs: IndexSet(VRegIndex),

    // === Statistics ===
    stats: *Stats,

    pub const Stats = struct {
        process_bundle_count: usize = 0,
        process_bundle_reg_probe_start_any: usize = 0,
        process_bundle_reg_probes_any: usize = 0,
        process_bundle_reg_success_any: usize = 0,
        evict_bundle_event: usize = 0,
        evict_bundle_count: usize = 0,
        splits: usize = 0,
        final_liverange_count: usize = 0,
        final_bundle_count: usize = 0,
        spill_bundle_count: usize = 0,
    };

    /// Initialize a new ProcessContext.
    pub fn init(
        allocator: std.mem.Allocator,
        ranges: *std.ArrayListUnmanaged(LiveRange),
        bundles: *std.ArrayListUnmanaged(LiveBundle),
        spillsets: *std.ArrayListUnmanaged(SpillSet),
        vregs: *std.ArrayListUnmanaged(VRegData),
        pregs: *std.ArrayListUnmanaged(PRegData),
        env: *const MachineEnv,
        allocation_queue: *PrioQueue,
        cfginfo: *const CFGInfo,
        num_insts: usize,
        spilled_bundles: *std.ArrayListUnmanaged(LiveBundleIndex),
        stats: *Stats,
    ) Self {
        return .{
            .allocator = allocator,
            .ranges = ranges,
            .bundles = bundles,
            .spillsets = spillsets,
            .vregs = vregs,
            .pregs = pregs,
            .env = env,
            .allocation_queue = allocation_queue,
            .cfginfo = cfginfo,
            .num_insts = num_insts,
            .spilled_bundles = spilled_bundles,
            .scratch_conflicts = .{},
            .scratch_bundle = .{},
            .conflict_set = IndexSet(LiveBundleIndex).init(allocator),
            .scratch_removed_lrs = IndexSet(LiveRangeIndex).init(allocator),
            .scratch_removed_lrs_vregs = IndexSet(VRegIndex).init(allocator),
            .stats = stats,
        };
    }

    pub fn deinit(self: *Self) void {
        self.scratch_conflicts.deinit(self.allocator);
        self.scratch_bundle.deinit(self.allocator);
        self.conflict_set.deinit();
        self.scratch_removed_lrs.deinit();
        self.scratch_removed_lrs_vregs.deinit();
    }

    //=========================================================================
    // Simple accessor functions
    //=========================================================================

    /// Get bundle's cached spill weight.
    pub fn bundleSpillWeight(self: *const Self, bundle: LiveBundleIndex) u32 {
        return self.bundles.items[bundle.index()].cachedSpillWeight();
    }

    /// Get maximum spill weight in a set of bundles.
    pub fn maximumSpillWeightInBundleSet(self: *const Self, bundle_list: []const LiveBundleIndex) u32 {
        var max: u32 = 0;
        for (bundle_list) |b| {
            const w = self.bundles.items[b.index()].cachedSpillWeight();
            if (w > max) max = w;
        }
        return max;
    }

    /// Check if a bundle is minimal (single use in single range).
    pub fn minimalBundle(self: *const Self, bundle: LiveBundleIndex) bool {
        return self.bundles.items[bundle.index()].cachedMinimal();
    }

    //=========================================================================
    // Range property computation
    //=========================================================================

    /// Recompute properties for a live range (spill weight and flags).
    pub fn recomputeRangeProperties(self: *Self, range: LiveRangeIndex) void {
        const rangedata = &self.ranges.items[range.index()];
        var w = SpillWeight.zero();
        for (rangedata.uses.items) |u| {
            w = w.add(SpillWeight{ .bits = u.weight });
        }
        rangedata.setUsesSpillWeight(w);

        if (rangedata.uses.items.len > 0 and
            rangedata.uses.items[0].operand.kind() == .def)
        {
            rangedata.setFlag(LiveRangeFlag.starts_at_def);
        }
    }

    //=========================================================================
    // Spill bundle management
    //=========================================================================

    /// Get or create a spill bundle for the given bundle's spillset.
    pub fn getOrCreateSpillBundle(
        self: *Self,
        bundle: LiveBundleIndex,
        create_if_absent: bool,
    ) !?LiveBundleIndex {
        const ssidx = self.bundles.items[bundle.index()].spillset;
        const idx = self.spillsets.items[ssidx.index()].spill_bundle;

        if (idx.isValid()) {
            return idx;
        } else if (create_if_absent) {
            const new_idx = LiveBundleIndex.new(self.bundles.items.len);
            try self.bundles.append(self.allocator, LiveBundle.init());
            self.spillsets.items[ssidx.index()].spill_bundle = new_idx;
            self.bundles.items[new_idx.index()].spillset = ssidx;
            try self.spilled_bundles.append(self.allocator, new_idx);
            return new_idx;
        } else {
            return null;
        }
    }

    //=========================================================================
    // Requirement computation (delegated to merge module pattern)
    //=========================================================================

    /// Convert an operand to a requirement.
    fn requirementFromOperand(self: *const Self, op: Operand) Requirement {
        return switch (op.constraint()) {
            .fixed_reg => |preg| {
                if (self.pregs.items[preg.index()].is_stack) {
                    return .{ .fixed_stack = preg };
                } else {
                    return .{ .fixed_reg = preg };
                }
            },
            .reg, .reuse => .register,
            .limit => |max| .{ .limit = max },
            .stack => .stack,
            .any => .any,
        };
    }

    /// Compute the combined requirement for a bundle.
    pub fn computeRequirement(
        self: *const Self,
        bundle: LiveBundleIndex,
    ) error{Conflict}!Requirement {
        var req: Requirement = .any;

        for (self.bundles.items[bundle.index()].ranges.items) |entry| {
            const range_data = &self.ranges.items[entry.index.index()];
            for (range_data.uses.items) |u| {
                const r = self.requirementFromOperand(u.operand);
                req = merge_mod.mergeRequirements(req, r) catch return error.Conflict;
            }
        }

        return req;
    }

    //=========================================================================
    // Bundle property recomputation
    //=========================================================================

    /// Recompute all properties for a bundle.
    pub fn recomputeBundleProperties(self: *Self, bundle: LiveBundleIndex) void {
        const bundledata = &self.bundles.items[bundle.index()];
        const num_ranges = bundledata.ranges.items.len;
        if (num_ranges == 0) return;

        const first_range = bundledata.ranges.items[0].index;
        const first_range_data = &self.ranges.items[first_range.index()];

        self.bundles.items[bundle.index()].prio = self.computeBundlePrio(bundle);
        self.bundles.items[bundle.index()].limit = self.computeBundleLimit(bundle);

        var minimal: bool = undefined;
        var fixed = false;
        var fixed_def = false;
        var stack = false;

        if (!first_range_data.vreg.isValid()) {
            minimal = true;
            fixed = true;
        } else if (num_ranges == 1) {
            for (first_range_data.uses.items) |u| {
                switch (u.operand.constraint()) {
                    .fixed_reg => {
                        fixed = true;
                        if (u.operand.kind() == .def) {
                            fixed_def = true;
                        }
                    },
                    .stack => stack = true,
                    else => {},
                }
                if (stack and fixed) break;
            }

            minimal = switch (first_range_data.uses.items.len) {
                0 => true,
                1 => blk: {
                    const only_use = &first_range_data.uses.items[0];
                    const min_range = minimalRangeForUse(only_use);
                    break :blk min_range.contains(first_range_data.range);
                },
                else => false,
            };
        } else {
            minimal = false;
        }

        const spill_weight: u32 = if (minimal) blk: {
            if (fixed) {
                break :blk MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT;
            } else if (self.bundles.items[bundle.index()].limit) |lim| {
                break :blk MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT - @as(u32, lim);
            } else {
                break :blk MINIMAL_BUNDLE_SPILL_WEIGHT;
            }
        } else blk: {
            var total = SpillWeight.zero();
            for (self.bundles.items[bundle.index()].ranges.items) |entry| {
                const range_data = &self.ranges.items[entry.index.index()];
                total = total.add(range_data.usesSpillWeight());
            }

            if (self.bundles.items[bundle.index()].prio > 0) {
                const final_weight = total.toInt() / self.bundles.items[bundle.index()].prio;
                break :blk @min(BUNDLE_MAX_NORMAL_SPILL_WEIGHT, final_weight);
            } else {
                break :blk 0;
            }
        };

        self.bundles.items[bundle.index()].setCachedSpillWeightAndProps(
            spill_weight,
            minimal,
            fixed,
            fixed_def,
            stack,
        );
    }

    /// Compute priority for a bundle (total instruction coverage).
    fn computeBundlePrio(self: *const Self, bundle: LiveBundleIndex) u32 {
        var total: u32 = 0;
        for (self.bundles.items[bundle.index()].ranges.items) |entry| {
            total += @intCast(entry.range.len());
        }
        return total;
    }

    /// Compute the most restrictive limit for a bundle.
    fn computeBundleLimit(self: *const Self, bundle: LiveBundleIndex) ?u8 {
        var limit: ?u8 = null;
        for (self.bundles.items[bundle.index()].ranges.items) |entry| {
            for (self.ranges.items[entry.index.index()].uses.items) |u| {
                switch (u.operand.constraint()) {
                    .limit => |current| {
                        const current_u8: u8 = @intCast(@min(current, 255));
                        if (limit) |prev| {
                            limit = @min(prev, current_u8);
                        } else {
                            limit = current_u8;
                        }
                    },
                    .fixed_reg, .stack => break,
                    else => continue,
                }
            }
        }
        return limit;
    }

    //=========================================================================
    // Allocation functions
    //=========================================================================

    /// Try to allocate a bundle to a specific register.
    pub fn tryToAllocateBundleToReg(
        self: *Self,
        bundle: LiveBundleIndex,
        reg: PRegIndex,
        max_allowable_cost: ?u32,
        conflicts: *std.ArrayListUnmanaged(LiveBundleIndex),
    ) !AllocRegResult {
        conflicts.clearRetainingCapacity();
        self.conflict_set.clear();
        var max_conflict_weight: u32 = 0;

        const bundle_ranges = self.bundles.items[bundle.index()].ranges.items;
        if (bundle_ranges.len == 0) {
            // Empty bundle - allocate trivially
            const preg = PReg.fromIndex(reg.index());
            self.bundles.items[bundle.index()].allocation = Allocation.reg(preg);
            return AllocRegResult{ .allocated = Allocation.reg(preg) };
        }

        var first_conflict: ?ProgPoint = null;

        // Check each bundle range against preg allocations
        for (bundle_ranges) |entry| {
            const key = LiveRangeKey.fromRange(entry.range);

            // Check for overlapping allocations in this preg
            const preg_allocs = &self.pregs.items[reg.index()].allocations;
            for (preg_allocs.items.items) |item| {
                // Check for overlap
                if (item.key.order(key) != .eq) continue;

                // Overlap found
                const preg_range = item.value;
                if (preg_range.isValid()) {
                    // Conflict with another bundle
                    const conflict_bundle = self.ranges.items[preg_range.index()].bundle;
                    if (try self.conflict_set.insert(self.allocator, conflict_bundle)) {
                        try conflicts.append(self.allocator, conflict_bundle);
                        max_conflict_weight = @max(
                            max_conflict_weight,
                            self.bundles.items[conflict_bundle.index()].cachedSpillWeight(),
                        );
                        if (max_allowable_cost) |max_cost| {
                            if (max_conflict_weight > max_cost) {
                                return AllocRegResult.conflict_high_cost;
                            }
                        }
                    }

                    if (first_conflict == null) {
                        first_conflict = ProgPoint{ .bits = @max(item.key.from, key.from) };
                    }
                } else {
                    // Conflict with fixed reservation
                    return AllocRegResult{
                        .conflict_with_fixed = .{
                            .max_cost = max_conflict_weight,
                            .point = ProgPoint{ .bits = item.key.from },
                        },
                    };
                }
            }
        }

        if (conflicts.items.len > 0) {
            return AllocRegResult{
                .conflict = .{
                    .bundles = conflicts.items,
                    .point = first_conflict.?,
                },
            };
        }

        // Success - add our ranges to the preg's allocation map
        const preg = PReg.fromIndex(reg.index());
        self.bundles.items[bundle.index()].allocation = Allocation.reg(preg);
        for (bundle_ranges) |entry| {
            const alloc_key = LiveRangeKey.fromRange(entry.range);
            _ = try self.pregs.items[reg.index()].allocations.insert(
                self.allocator,
                alloc_key,
                entry.index,
            );
        }

        return AllocRegResult{ .allocated = Allocation.reg(preg) };
    }

    /// Evict a bundle from its current allocation.
    pub fn evictBundle(self: *Self, bundle: LiveBundleIndex) !void {
        const preg = self.bundles.items[bundle.index()].allocation.asReg() orelse return;

        const preg_idx = PRegIndex.new(preg.index());
        self.bundles.items[bundle.index()].allocation = Allocation.none();

        // Remove all ranges from the preg's allocation map
        for (self.bundles.items[bundle.index()].ranges.items) |entry| {
            const key = LiveRangeKey.fromRange(entry.range);
            _ = self.pregs.items[preg_idx.index()].allocations.remove(key);
        }

        // Re-queue the bundle
        const prio = self.bundles.items[bundle.index()].prio;
        try self.allocation_queue.insert(bundle, prio, PReg.invalid());
    }

    //=========================================================================
    // Splitting functions
    //=========================================================================

    /// Split a bundle and requeue both parts.
    pub fn splitAndRequeueBundle(
        self: *Self,
        bundle: LiveBundleIndex,
        split_at_input: ProgPoint,
        hint: PReg,
        trim_ends_into_spill_bundle_input: bool,
    ) !void {
        self.stats.splits += 1;

        var split_at = split_at_input;
        var trim_ends_into_spill_bundle = trim_ends_into_spill_bundle_input;

        const spillset = self.bundles.items[bundle.index()].spillset;

        // Check max splits
        if (self.spillsets.items[spillset.index()].splits >= MAX_SPLITS_PER_SPILLSET) {
            try self.splitIntoMinimalBundles(bundle, hint);
            return;
        }
        self.spillsets.items[spillset.index()].splits += 1;

        const bundle_ranges = self.bundles.items[bundle.index()].ranges.items;
        if (bundle_ranges.len == 0) return;

        const bundle_start = bundle_ranges[0].range.from;
        const bundle_end = bundle_ranges[bundle_ranges.len - 1].range.to;

        // Check if bundle spans only one instruction
        if (bundle_end.prevPoint().inst().eql(bundle_start.inst())) {
            try self.splitIntoMinimalBundles(bundle, hint);
            return;
        }

        // Adjust split point if at start
        if (split_at.bits == bundle_start.bits) {
            var first_use: ?ProgPoint = null;
            outer: for (bundle_ranges) |entry| {
                for (self.ranges.items[entry.index.index()].uses.items) |u| {
                    first_use = u.pos;
                    break :outer;
                }
            }

            split_at = if (first_use) |pos| blk: {
                if (pos.inst().eql(bundle_start.inst())) {
                    break :blk ProgPoint.before(pos.inst().next());
                } else {
                    break :blk ProgPoint.before(pos.inst());
                }
            } else ProgPoint.before(bundle_ranges[0].range.from.inst().next());
        } else {
            // Don't split in middle of instruction
            if (split_at.pos() == .after) {
                split_at = split_at.nextPoint();
            }
            if (split_at.bits >= bundle_end.bits) {
                split_at = split_at.prevPoint().prevPoint();
            }
        }

        // Find which LRs go to which bundle
        var last_lr_in_old_bundle_idx: usize = 0;
        var first_lr_in_new_bundle_idx: usize = 0;
        for (bundle_ranges, 0..) |entry, i| {
            if (split_at.bits > entry.range.from.bits) {
                last_lr_in_old_bundle_idx = i;
                first_lr_in_new_bundle_idx = i;
            }
            if (split_at.bits < entry.range.to.bits) {
                first_lr_in_new_bundle_idx = i;

                // Advance split point for fixed constraints
                if (self.bundles.items[bundle.index()].cachedFixed()) {
                    for (self.ranges.items[entry.index.index()].uses.items) |u| {
                        if (u.pos.bits < split_at.bits) continue;
                        if (u.operand.constraint() == .fixed_reg) {
                            split_at = ProgPoint.before(u.pos.inst());
                            if (split_at.bits > entry.range.from.bits) {
                                last_lr_in_old_bundle_idx = i;
                            }
                            trim_ends_into_spill_bundle = false;
                            break;
                        }
                    }
                }
                break;
            }
        }

        // Create new LR list for new bundle
        var new_lr_list = std.ArrayListUnmanaged(LiveRangeListEntry){};
        try new_lr_list.appendSlice(
            self.allocator,
            bundle_ranges[first_lr_in_new_bundle_idx..],
        );

        // Truncate old bundle's list
        self.bundles.items[bundle.index()].ranges.shrinkRetainingCapacity(
            last_lr_in_old_bundle_idx + 1,
        );

        // Split LR if needed
        if (split_at.bits > new_lr_list.items[0].range.from.bits) {
            const orig_lr = new_lr_list.items[0].index;
            const new_range = CodeRange{
                .from = split_at,
                .to = new_lr_list.items[0].range.to,
            };

            // Create new live range
            const new_lr = LiveRangeIndex.new(self.ranges.items.len);
            try self.ranges.append(self.allocator, LiveRange.init(new_range));
            self.ranges.items[new_lr.index()].vreg = self.ranges.items[orig_lr.index()].vreg;

            // Split uses
            var first_use_idx: usize = 0;
            for (self.ranges.items[orig_lr.index()].uses.items, 0..) |u, idx| {
                if (u.pos.bits >= split_at.bits) {
                    first_use_idx = idx;
                    break;
                }
                first_use_idx = idx + 1;
            }

            // Move uses to new range
            try self.ranges.items[new_lr.index()].uses.appendSlice(
                self.allocator,
                self.ranges.items[orig_lr.index()].uses.items[first_use_idx..],
            );
            self.ranges.items[orig_lr.index()].uses.shrinkRetainingCapacity(first_use_idx);

            self.recomputeRangeProperties(orig_lr);
            self.recomputeRangeProperties(new_lr);

            new_lr_list.items[0].index = new_lr;
            new_lr_list.items[0].range = self.ranges.items[new_lr.index()].range;
            self.ranges.items[orig_lr.index()].range.to = split_at;
            self.bundles.items[bundle.index()].ranges.items[last_lr_in_old_bundle_idx].range =
                self.ranges.items[orig_lr.index()].range;

            // Add to vreg's range list
            const vreg = self.ranges.items[new_lr.index()].vreg;
            try self.vregs.items[vreg.index()].ranges.append(self.allocator, LiveRangeListEntry{
                .range = self.ranges.items[new_lr.index()].range,
                .index = new_lr,
            });
        }

        // Create new bundle
        const new_bundle = LiveBundleIndex.new(self.bundles.items.len);
        try self.bundles.append(self.allocator, LiveBundle.init());
        self.bundles.items[new_bundle.index()].spillset = spillset;

        for (new_lr_list.items) |entry| {
            self.ranges.items[entry.index.index()].bundle = new_bundle;
        }
        self.bundles.items[new_bundle.index()].ranges = new_lr_list;

        // Handle trimming ends into spill bundle (simplified version)
        if (trim_ends_into_spill_bundle) {
            // Trim trailing empty region from old bundle
            while (self.bundles.items[bundle.index()].ranges.items.len > 0) {
                const entry = self.bundles.items[bundle.index()].ranges.items[
                    self.bundles.items[bundle.index()].ranges.items.len - 1
                ];
                const uses = self.ranges.items[entry.index.index()].uses.items;
                if (uses.len == 0) {
                    const spill = (try self.getOrCreateSpillBundle(bundle, true)).?;
                    try self.bundles.items[spill.index()].ranges.append(self.allocator, entry);
                    _ = self.bundles.items[bundle.index()].ranges.pop();
                    self.ranges.items[entry.index.index()].bundle = spill;
                    continue;
                }
                break;
            }

            // Trim leading empty region from new bundle
            while (self.bundles.items[new_bundle.index()].ranges.items.len > 0) {
                const entry = self.bundles.items[new_bundle.index()].ranges.items[0];
                if (self.ranges.items[entry.index.index()].hasFlag(LiveRangeFlag.starts_at_def)) {
                    break;
                }
                const uses = self.ranges.items[entry.index.index()].uses.items;
                if (uses.len == 0) {
                    const spill = (try self.getOrCreateSpillBundle(new_bundle, true)).?;
                    try self.bundles.items[spill.index()].ranges.append(self.allocator, entry);
                    _ = self.bundles.items[new_bundle.index()].ranges.orderedRemove(0);
                    self.ranges.items[entry.index.index()].bundle = spill;
                    continue;
                }
                break;
            }
        }

        // Requeue both bundles
        if (self.bundles.items[bundle.index()].ranges.items.len > 0) {
            self.recomputeBundleProperties(bundle);
            const prio = self.bundles.items[bundle.index()].prio;
            try self.allocation_queue.insert(bundle, prio, hint);
        }
        if (self.bundles.items[new_bundle.index()].ranges.items.len > 0) {
            self.recomputeBundleProperties(new_bundle);
            const prio = self.bundles.items[new_bundle.index()].prio;
            try self.allocation_queue.insert(new_bundle, prio, hint);
        }
    }

    /// Split a bundle into minimal bundles (one per use).
    pub fn splitIntoMinimalBundles(self: *Self, bundle: LiveBundleIndex, hint: PReg) !void {
        self.scratch_removed_lrs_vregs.clear();
        self.scratch_removed_lrs.clear();

        var new_lrs = std.ArrayListUnmanaged(struct { vreg: VRegIndex, lr: LiveRangeIndex }){};
        defer new_lrs.deinit(self.allocator);

        var new_bundles = std.ArrayListUnmanaged(LiveBundleIndex){};
        defer new_bundles.deinit(self.allocator);

        const spillset = self.bundles.items[bundle.index()].spillset;
        const spill = (try self.getOrCreateSpillBundle(bundle, true)).?;

        var spill_uses = std.ArrayListUnmanaged(Use){};
        defer spill_uses.deinit(self.allocator);

        // Process each range in the bundle
        var old_ranges = self.bundles.items[bundle.index()].ranges;
        self.bundles.items[bundle.index()].ranges = .{};

        for (old_ranges.items) |entry| {
            const vreg = self.ranges.items[entry.index.index()].vreg;
            _ = try self.scratch_removed_lrs.insert(self.allocator, entry.index);
            _ = try self.scratch_removed_lrs_vregs.insert(self.allocator, vreg);

            var spill_range = entry.range;
            var spill_starts_def = false;

            // Process each use
            var old_uses = self.ranges.items[entry.index.index()].uses;
            self.ranges.items[entry.index.index()].uses = .{};

            for (old_uses.items) |u| {
                const is_def = u.operand.kind() == .def;

                // Migrate any-constrained uses to spill range
                if (u.operand.constraint() == .any) {
                    try spill_uses.append(self.allocator, u);
                    spill_starts_def = spill_starts_def or is_def;
                    continue;
                }

                // Adjust spill range for defs
                if (is_def) {
                    spill_range.from = ProgPoint.before(u.pos.inst().next());
                }

                // Create new LR for this use
                const cr = minimalRangeForUse(&u);
                const lr = LiveRangeIndex.new(self.ranges.items.len);
                try self.ranges.append(self.allocator, LiveRange.init(cr));
                try self.ranges.items[lr.index()].uses.append(self.allocator, u);
                self.ranges.items[lr.index()].vreg = vreg;

                try new_lrs.append(self.allocator, .{ .vreg = vreg, .lr = lr });

                // Create new bundle for this LR
                const new_bundle = LiveBundleIndex.new(self.bundles.items.len);
                try self.bundles.append(self.allocator, LiveBundle.init());
                self.ranges.items[lr.index()].bundle = new_bundle;
                self.bundles.items[new_bundle.index()].spillset = spillset;
                try self.bundles.items[new_bundle.index()].ranges.append(self.allocator, LiveRangeListEntry{
                    .range = cr,
                    .index = lr,
                });

                try new_bundles.append(self.allocator, new_bundle);

                if (is_def) {
                    self.ranges.items[lr.index()].setFlag(LiveRangeFlag.starts_at_def);
                }
            }

            old_uses.deinit(self.allocator);

            // Create spill range entry
            if (!spill_range.isEmpty()) {
                const spill_lr = LiveRangeIndex.new(self.ranges.items.len);
                try self.ranges.append(self.allocator, LiveRange.init(spill_range));
                self.ranges.items[spill_lr.index()].vreg = vreg;
                self.ranges.items[spill_lr.index()].bundle = spill;
                try self.ranges.items[spill_lr.index()].uses.appendSlice(self.allocator, spill_uses.items);
                spill_uses.clearRetainingCapacity();

                try new_lrs.append(self.allocator, .{ .vreg = vreg, .lr = spill_lr });

                if (spill_starts_def) {
                    self.ranges.items[spill_lr.index()].setFlag(LiveRangeFlag.starts_at_def);
                }

                try self.bundles.items[spill.index()].ranges.append(self.allocator, LiveRangeListEntry{
                    .range = spill_range,
                    .index = spill_lr,
                });
            }
        }

        old_ranges.deinit(self.allocator);

        // Remove old LRs from vreg lists
        var vreg_iter = self.scratch_removed_lrs_vregs.iterator();
        while (vreg_iter.next()) |vreg| {
            var i: usize = 0;
            while (i < self.vregs.items[vreg.index()].ranges.items.len) {
                const lr = self.vregs.items[vreg.index()].ranges.items[i].index;
                if (self.scratch_removed_lrs.contains(lr)) {
                    _ = self.vregs.items[vreg.index()].ranges.orderedRemove(i);
                } else {
                    i += 1;
                }
            }
        }

        // Add new LRs to vreg lists
        for (new_lrs.items) |item| {
            const range = self.ranges.items[item.lr.index()].range;
            try self.vregs.items[item.vreg.index()].ranges.append(self.allocator, LiveRangeListEntry{
                .range = range,
                .index = item.lr,
            });
        }

        // Requeue new bundles
        for (new_bundles.items) |b| {
            if (self.bundles.items[b.index()].ranges.items.len > 0) {
                self.recomputeBundleProperties(b);
                const prio = self.bundles.items[b.index()].prio;
                try self.allocation_queue.insert(b, prio, hint);
            }
        }
    }

    //=========================================================================
    // Main processing functions
    //=========================================================================

    /// Process a single bundle.
    pub fn processBundle(self: *Self, bundle: LiveBundleIndex, hint_input: PReg) RegAllocError!void {
        const class = self.spillsets.items[self.bundles.items[bundle.index()].spillset.index()].class;

        // Get hint from input or spillset
        var hint = if (!hint_input.isInvalid())
            hint_input
        else
            self.spillsets.items[self.bundles.items[bundle.index()].spillset.index()].hint;

        if (!hint.isInvalid() and self.pregs.items[hint.index()].is_stack) {
            hint = PReg.invalid();
        }

        // Compute requirement
        const req = self.computeRequirement(bundle) catch |err| switch (err) {
            error.Conflict => {
                // Must split - find split point (simplified: use first use)
                const split_point = self.bundles.items[bundle.index()].ranges.items[0].range.from;
                self.splitAndRequeueBundle(bundle, split_point, hint, true) catch |e| switch (e) {
                    error.OutOfMemory => return error.OutOfMemory,
                };
                return;
            },
        };

        // Handle Any requirement with existing spill bundle
        switch (req) {
            .any => {
                if (self.getOrCreateSpillBundle(bundle, false) catch return error.OutOfMemory) |spill| {
                    // Move ranges to spill bundle
                    var list = self.bundles.items[bundle.index()].ranges;
                    self.bundles.items[bundle.index()].ranges = .{};

                    for (list.items) |entry| {
                        self.ranges.items[entry.index.index()].bundle = spill;
                    }
                    self.bundles.items[spill.index()].ranges.appendSlice(self.allocator, list.items) catch
                        return error.OutOfMemory;
                    list.deinit(self.allocator);
                    return;
                }
            },
            else => {},
        }

        // Allocation loop
        var attempts: usize = 0;
        self.scratch_conflicts.clearRetainingCapacity();
        self.scratch_bundle.clearRetainingCapacity();

        outer: while (true) {
            attempts += 1;
            if (attempts >= 100 * self.num_insts) break;

            const fixed_preg: ?PReg = switch (req) {
                .fixed_reg => |preg| preg,
                .fixed_stack => |preg| preg,
                .register, .limit => null,
                .stack => {
                    // Must be on stack
                    const ss = self.bundles.items[bundle.index()].spillset;
                    self.spillsets.items[ss.index()].required = true;
                    return;
                },
                .any => {
                    self.spilled_bundles.append(self.allocator, bundle) catch
                        return error.OutOfMemory;
                    break;
                },
            };

            var lowest_cost_evict_conflict_cost: ?u32 = null;
            self.scratch_bundle.clearRetainingCapacity();

            var lowest_cost_split_conflict_cost: ?u32 = null;
            var lowest_cost_split_conflict_point = ProgPoint.before(Inst.new(0));
            var lowest_cost_split_conflict_reg = PReg.invalid();

            // Compute scan offset for register distribution
            const scan_offset = self.ranges.items[
                self.bundles.items[bundle.index()].ranges.items[0].index.index()
            ].range.from.inst().idx() + bundle.index();

            self.stats.process_bundle_reg_probe_start_any += 1;
            const limit: ?usize = if (self.bundles.items[bundle.index()].limit) |l|
                @as(usize, l)
            else
                null;

            var reg_iter = RegTraversalIter.init(
                self.env,
                class,
                fixed_preg,
                hint.asValid(),
                scan_offset,
                limit,
            );

            while (reg_iter.next()) |preg| {
                self.stats.process_bundle_reg_probes_any += 1;
                const preg_idx = PRegIndex.new(preg.index());

                const scan_limit_cost: ?u32 = if (lowest_cost_evict_conflict_cost != null and
                    lowest_cost_split_conflict_cost != null)
                    @max(lowest_cost_evict_conflict_cost.?, lowest_cost_split_conflict_cost.?)
                else
                    null;

                const result = self.tryToAllocateBundleToReg(
                    bundle,
                    preg_idx,
                    scan_limit_cost,
                    &self.scratch_conflicts,
                ) catch return error.OutOfMemory;

                switch (result) {
                    .allocated => |alloc| {
                        self.stats.process_bundle_reg_success_any += 1;
                        self.spillsets.items[
                            self.bundles.items[bundle.index()].spillset.index()
                        ].hint = alloc.asReg().?;
                        break :outer;
                    },
                    .conflict => |c| {
                        const conflict_cost = self.maximumSpillWeightInBundleSet(c.bundles);

                        if (lowest_cost_evict_conflict_cost == null or
                            conflict_cost < lowest_cost_evict_conflict_cost.?)
                        {
                            lowest_cost_evict_conflict_cost = conflict_cost;
                            self.scratch_bundle.clearRetainingCapacity();
                            self.scratch_bundle.appendSlice(self.allocator, c.bundles) catch
                                return error.OutOfMemory;
                        }

                        const loop_depth = self.cfginfo.approx_loop_depth.items[
                            self.cfginfo.insn_block.items[c.point.inst().idx()].idx()
                        ];
                        const move_cost = liveness_mod.spillWeightFromConstraint(
                            .reg,
                            loop_depth,
                            true,
                        ).toInt();

                        if (lowest_cost_split_conflict_cost == null or
                            (conflict_cost + move_cost) < lowest_cost_split_conflict_cost.?)
                        {
                            lowest_cost_split_conflict_cost = conflict_cost + move_cost;
                            lowest_cost_split_conflict_point = c.point;
                            lowest_cost_split_conflict_reg = preg;
                        }
                    },
                    .conflict_with_fixed => |c| {
                        const loop_depth = self.cfginfo.approx_loop_depth.items[
                            self.cfginfo.insn_block.items[c.point.inst().idx()].idx()
                        ];
                        const move_cost = liveness_mod.spillWeightFromConstraint(
                            .reg,
                            loop_depth,
                            true,
                        ).toInt();

                        if (lowest_cost_split_conflict_cost == null or
                            (c.max_cost + move_cost) < lowest_cost_split_conflict_cost.?)
                        {
                            lowest_cost_split_conflict_cost = c.max_cost + move_cost;
                            lowest_cost_split_conflict_point = c.point;
                            lowest_cost_split_conflict_reg = preg;
                        }
                    },
                    .conflict_high_cost => continue,
                }
            }

            // Couldn't allocate - need to evict or split
            const our_spill_weight = self.bundleSpillWeight(bundle);

            // Check for too-many-live-regs
            if (self.minimalBundle(bundle) and
                (attempts >= 2 or
                lowest_cost_evict_conflict_cost == null or
                lowest_cost_evict_conflict_cost.? >= our_spill_weight))
            {
                if (req == .register or req == .limit) {
                    return error.TooManyLiveRegs;
                }
            }

            // Decide: split or evict
            if (!self.minimalBundle(bundle) and
                (attempts >= 2 or
                lowest_cost_evict_conflict_cost == null or
                our_spill_weight <= lowest_cost_evict_conflict_cost.?))
            {
                // Split
                const bundle_start = self.bundles.items[bundle.index()].ranges.items[0].range.from;
                var split_at_point = if (lowest_cost_split_conflict_point.bits > bundle_start.bits)
                    lowest_cost_split_conflict_point
                else
                    bundle_start;

                // Adjust for loop depth
                const bundle_start_depth = self.cfginfo.approx_loop_depth.items[
                    self.cfginfo.insn_block.items[bundle_start.inst().idx()].idx()
                ];
                const split_at_depth = self.cfginfo.approx_loop_depth.items[
                    self.cfginfo.insn_block.items[split_at_point.inst().idx()].idx()
                ];

                if (split_at_depth > bundle_start_depth) {
                    const start_block = self.cfginfo.insn_block.items[bundle_start.inst().idx()].idx();
                    const split_block = self.cfginfo.insn_block.items[split_at_point.inst().idx()].idx();
                    for (start_block + 1..split_block + 1) |block| {
                        if (self.cfginfo.approx_loop_depth.items[block] > bundle_start_depth) {
                            split_at_point = self.cfginfo.block_entry.items[block];
                            break;
                        }
                    }
                }

                self.splitAndRequeueBundle(
                    bundle,
                    split_at_point,
                    lowest_cost_split_conflict_reg,
                    true,
                ) catch return error.OutOfMemory;
                break :outer;
            } else {
                // Evict
                self.stats.evict_bundle_event += 1;
                for (self.scratch_bundle.items) |b| {
                    self.evictBundle(b) catch return error.OutOfMemory;
                    self.stats.evict_bundle_count += 1;
                }
            }
        }
    }

    /// Process all bundles in the allocation queue.
    pub fn processBundles(self: *Self) RegAllocError!void {
        while (self.allocation_queue.pop()) |item| {
            self.stats.process_bundle_count += 1;
            try self.processBundle(item.bundle, item.hint);
        }
        self.stats.final_liverange_count = self.ranges.items.len;
        self.stats.final_bundle_count = self.bundles.items.len;
        self.stats.spill_bundle_count = self.spilled_bundles.items.len;
    }
};

//=============================================================================
// Helper functions
//=============================================================================

/// Compute the minimal range that covers a use in a minimal bundle.
pub fn minimalRangeForUse(u: *const Use) CodeRange {
    const inst = u.pos.inst();
    const early = ProgPoint.before(inst);
    const late = ProgPoint.after(inst);
    const next_early = ProgPoint.before(inst.next());

    return switch (u.pos.pos()) {
        .before => switch (u.operand.kind()) {
            .def => CodeRange{ .from = early, .to = next_early },
            .use => CodeRange{ .from = early, .to = late },
        },
        .after => switch (u.operand.kind()) {
            .def => CodeRange{ .from = late, .to = next_early },
            .use => CodeRange{ .from = early, .to = next_early },
        },
    };
}

//=============================================================================
// Tests
//=============================================================================

test "AllocRegResult variants" {
    const preg = PReg.new(5, RegClass.int);
    const alloc = Allocation.reg(preg);

    const result1 = AllocRegResult{ .allocated = alloc };
    switch (result1) {
        .allocated => |a| try std.testing.expect(a.asReg().?.eql(preg)),
        else => try std.testing.expect(false),
    }

    const result2 = AllocRegResult.conflict_high_cost;
    try std.testing.expect(result2 == .conflict_high_cost);

    const result3 = AllocRegResult{
        .conflict_with_fixed = .{
            .max_cost = 100,
            .point = ProgPoint.before(Inst.new(5)),
        },
    };
    switch (result3) {
        .conflict_with_fixed => |c| {
            try std.testing.expectEqual(@as(u32, 100), c.max_cost);
            try std.testing.expectEqual(@as(usize, 5), c.point.inst().idx());
        },
        else => try std.testing.expect(false),
    }
}

test "Cursor initialization and iteration" {
    var set = PRegSet.empty();
    set.add(PReg.new(0, .int));
    set.add(PReg.new(1, .int));
    set.add(PReg.new(2, .int));
    set.add(PReg.new(5, .int));

    var cursor = Cursor.init(set, .int, 2);

    // Should iterate starting from offset 2
    var count: usize = 0;
    while (cursor.next()) |_| {
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 4), count);
}

test "RegTraversalIter with fixed register" {
    const env = env_mod.arm64MachineEnv();
    const fixed = PReg.new(5, .int);

    var iter = RegTraversalIter.init(&env, .int, fixed, null, 0, null);

    // Should return only the fixed register
    const first = iter.next();
    try std.testing.expect(first != null);
    try std.testing.expect(first.?.eql(fixed));

    const second = iter.next();
    try std.testing.expect(second == null);
}

test "RegTraversalIter with hint" {
    const env = env_mod.arm64MachineEnv();
    const hint = PReg.new(3, .int);

    var iter = RegTraversalIter.init(&env, .int, null, hint, 0, null);

    // First should be the hint
    const first = iter.next();
    try std.testing.expect(first != null);
    try std.testing.expect(first.?.eql(hint));

    // Subsequent should be other registers, not hint again
    while (iter.next()) |reg| {
        try std.testing.expect(!reg.eql(hint));
    }
}

test "RegTraversalIter with limit" {
    const env = env_mod.arm64MachineEnv();

    var iter = RegTraversalIter.init(&env, .int, null, null, 0, 5);

    // All registers should have hw_enc < 5
    while (iter.next()) |reg| {
        try std.testing.expect(reg.hwEnc() < 5);
    }
}

test "minimalRangeForUse" {
    const inst = Inst.new(10);
    const preg = PReg.new(5, .int);

    // Early def: whole instruction
    const early_def = Use{
        .operand = Operand.new(VReg.new(0, .int), .{ .fixed_reg = preg }, .def, .early),
        .pos = ProgPoint.before(inst),
        .slot = 0,
        .weight = 0,
    };
    const range1 = minimalRangeForUse(&early_def);
    try std.testing.expectEqual(ProgPoint.before(inst).bits, range1.from.bits);
    try std.testing.expectEqual(ProgPoint.before(inst.next()).bits, range1.to.bits);

    // Late def: late only
    const late_def = Use{
        .operand = Operand.new(VReg.new(0, .int), .{ .fixed_reg = preg }, .def, .late),
        .pos = ProgPoint.after(inst),
        .slot = 0,
        .weight = 0,
    };
    const range2 = minimalRangeForUse(&late_def);
    try std.testing.expectEqual(ProgPoint.after(inst).bits, range2.from.bits);
    try std.testing.expectEqual(ProgPoint.before(inst.next()).bits, range2.to.bits);
}
