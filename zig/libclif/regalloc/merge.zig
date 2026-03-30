//! Bundle merging and requirements computation.
//!
//! Ported from regalloc2's `src/ion/merge.rs` and `src/ion/requirement.rs`.
//!
//! This module handles:
//! - Computing operand requirements for bundles
//! - Merging compatible bundles to reduce move operations
//! - Creating initial bundles from vregs
//! - Queuing bundles for allocation
//!
//! See audit/native/merge_audit.md for full function-by-function mapping.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const ion_data = @import("ion_data.zig");
const liveness = @import("liveness.zig");

const Block = index.Block;
const Inst = index.Inst;
const VReg = index.VReg;
const PReg = index.PReg;
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
const SpillSlotIndex = ion_data.SpillSlotIndex;
const VRegIndex = ion_data.VRegIndex;
const PRegIndex = ion_data.PRegIndex;
const LiveRange = ion_data.LiveRange;
const LiveBundle = ion_data.LiveBundle;
const LiveRangeListEntry = ion_data.LiveRangeListEntry;
const SpillSet = ion_data.SpillSet;
const VRegData = ion_data.VRegData;
const PRegData = ion_data.PRegData;
const BlockparamOut = ion_data.BlockparamOut;
const PrioQueue = ion_data.PrioQueue;
const Use = ion_data.Use;
const SpillWeight = ion_data.SpillWeight;

// Import spill weight constants from ion_data (canonical source)
const BUNDLE_MAX_SPILL_WEIGHT = ion_data.BUNDLE_MAX_SPILL_WEIGHT;
const MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT = ion_data.MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT;
const MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT = ion_data.MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT;
const MINIMAL_BUNDLE_SPILL_WEIGHT = ion_data.MINIMAL_BUNDLE_SPILL_WEIGHT;
const BUNDLE_MAX_NORMAL_SPILL_WEIGHT = ion_data.BUNDLE_MAX_NORMAL_SPILL_WEIGHT;

//=============================================================================
// RequirementConflict - Error type for requirement conflicts
//=============================================================================

/// Error indicating a requirement conflict during merge.
pub const RequirementConflict = error{
    Conflict,
};

//=============================================================================
// RequirementConflictAt - Conflict with suggested split point
//=============================================================================

/// Where a requirement conflict occurred, with suggested split point.
pub const RequirementConflictAt = union(enum) {
    /// Transition from stack-constrained to reg-constrained segment.
    /// Suggested split is late (just before the reg use).
    stack_to_reg: ProgPoint,

    /// Transition from reg-constrained to stack-constrained segment.
    /// Suggested split is early (just after the last reg use).
    reg_to_stack: ProgPoint,

    /// Any other conflict. Split point is late.
    other: ProgPoint,

    /// Should we trim edges around the split?
    pub fn shouldTrimEdgesAroundSplit(self: RequirementConflictAt) bool {
        return switch (self) {
            .stack_to_reg, .reg_to_stack => false,
            .other => true,
        };
    }

    /// Get the suggested split point.
    pub fn suggestedSplitPoint(self: RequirementConflictAt) ProgPoint {
        return switch (self) {
            .stack_to_reg => |pt| pt,
            .reg_to_stack => |pt| pt,
            .other => |pt| pt,
        };
    }
};

//=============================================================================
// Requirement - Operand requirement representation
//=============================================================================

/// Represents the requirement for an operand or bundle.
pub const Requirement = union(enum) {
    any,
    register,
    fixed_reg: PReg,
    limit: usize,
    stack,
    fixed_stack: PReg,


    /// Check if this requirement is stack-based.
    pub fn isStack(self: Requirement) bool {
        return switch (self) {
            .stack, .fixed_stack => true,
            .any, .register, .fixed_reg, .limit => false,
        };
    }

    /// Check if this requirement is register-based.
    pub fn isReg(self: Requirement) bool {
        return switch (self) {
            .register, .fixed_reg, .limit => true,
            .any, .stack, .fixed_stack => false,
        };
    }
};

/// Merge two requirements, returning the combined requirement or an error.
pub fn mergeRequirements(a: Requirement, b: Requirement) RequirementConflict!Requirement {
    return switch (a) {
        .any => b, // Any matches anything
        .register => switch (b) {
            .any => a,
            .register => .register,
            .fixed_reg => |preg| .{ .fixed_reg = preg },
            .limit => |max| .{ .limit = max },
            .stack, .fixed_stack => error.Conflict,
        },
        .fixed_reg => |a_preg| switch (b) {
            .any => a,
            .register => .{ .fixed_reg = a_preg },
            .fixed_reg => |b_preg| {
                if (a_preg.eql(b_preg)) {
                    return .{ .fixed_reg = a_preg };
                }
                return error.Conflict;
            },
            .limit => |max| {
                if (max > a_preg.hwEnc()) {
                    return .{ .fixed_reg = a_preg };
                }
                return error.Conflict;
            },
            .stack, .fixed_stack => error.Conflict,
        },
        .limit => |a_max| switch (b) {
            .any => a,
            .register => .{ .limit = a_max },
            .fixed_reg => |preg| {
                if (a_max > preg.hwEnc()) {
                    return .{ .fixed_reg = preg };
                }
                return error.Conflict;
            },
            .limit => |b_max| .{ .limit = @min(a_max, b_max) },
            .stack, .fixed_stack => error.Conflict,
        },
        .stack => switch (b) {
            .any => a,
            .stack => .stack,
            .fixed_stack => |preg| .{ .fixed_stack = preg },
            .register, .fixed_reg, .limit => error.Conflict,
        },
        .fixed_stack => |a_preg| switch (b) {
            .any => a,
            .stack => .{ .fixed_stack = a_preg },
            .fixed_stack => |b_preg| {
                if (a_preg.eql(b_preg)) {
                    return .{ .fixed_stack = a_preg };
                }
                return error.Conflict;
            },
            .register, .fixed_reg, .limit => error.Conflict,
        },
    };
}

//=============================================================================
// MergeContext - Context for bundle merging
//=============================================================================

/// Context for bundle merging operations.
/// Holds all the state needed for merging bundles.
pub const MergeContext = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    // === Core data structures (shared with liveness) ===
    ranges: *std.ArrayListUnmanaged(LiveRange),
    bundles: *std.ArrayListUnmanaged(LiveBundle),
    spillsets: *std.ArrayListUnmanaged(SpillSet),
    vregs: *std.ArrayListUnmanaged(VRegData),
    pregs: *std.ArrayListUnmanaged(PRegData),

    // === Block parameter flow ===
    blockparam_outs: *std.ArrayListUnmanaged(BlockparamOut),

    // === Allocation queue ===
    allocation_queue: *PrioQueue,

    // === Statistics ===
    merged_bundle_count: *usize,

    /// Convert an operand to a requirement.
    pub fn requirementFromOperand(self: *const Self, op: Operand) Requirement {
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
    ) error{ConflictAt}!Requirement {
        var req: Requirement = .any;
        var last_pos = ProgPoint.before(Inst.new(0));

        for (self.bundles.items[bundle.index()].ranges.items) |entry| {
            const range_data = &self.ranges.items[entry.index.index()];
            for (range_data.uses.items) |u| {
                const r = self.requirementFromOperand(u.operand);
                req = mergeRequirements(req, r) catch {
                    // Determine conflict type for split suggestion
                    _ = if (req.isStack() and r.isReg())
                        RequirementConflictAt{ .stack_to_reg = u.pos }
                    else if (req.isReg() and r.isStack())
                        RequirementConflictAt{ .reg_to_stack = last_pos }
                    else
                        RequirementConflictAt{ .other = u.pos };
                    return error.ConflictAt;
                };
                last_pos = u.pos;
            }
        }

        return req;
    }

    /// Merge the requirements of two bundles.
    pub fn mergeBundleRequirements(
        self: *const Self,
        a: LiveBundleIndex,
        b: LiveBundleIndex,
    ) RequirementConflict!Requirement {
        const req_a = self.computeRequirement(a) catch return error.Conflict;
        const req_b = self.computeRequirement(b) catch return error.Conflict;
        return mergeRequirements(req_a, req_b);
    }

    /// Transfer cached properties from one bundle to another.
    fn mergeBundleProperties(self: *Self, from: LiveBundleIndex, to: LiveBundleIndex) void {
        const from_bundle = &self.bundles.items[from.index()];
        const to_bundle = &self.bundles.items[to.index()];

        if (from_bundle.cachedFixed()) {
            self.bundles.items[to.index()].setCachedFixed();
        }
        if (from_bundle.cachedFixedDef()) {
            self.bundles.items[to.index()].setCachedFixedDef();
        }
        if (from_bundle.cachedStack()) {
            self.bundles.items[to.index()].setCachedStack();
        }

        if (from_bundle.limit) |theirs| {
            if (to_bundle.limit) |ours| {
                self.bundles.items[to.index()].limit = @min(ours, theirs);
            } else {
                self.bundles.items[to.index()].limit = theirs;
            }
        }
    }

    /// Attempt to merge two bundles.
    /// Returns true if merge succeeded, false if not possible.
    pub fn mergeBundles(self: *Self, from: LiveBundleIndex, to: LiveBundleIndex) !bool {
        // Trivial merge - same bundle
        if (from.eql(to)) {
            return true;
        }

        // Both bundles must have the same RegClass
        const from_rc = self.spillsets.items[self.bundles.items[from.index()].spillset.index()].class;
        const to_rc = self.spillsets.items[self.bundles.items[to.index()].spillset.index()].class;
        if (from_rc != to_rc) {
            return false;
        }

        // If either bundle is already assigned (pinned), don't merge
        if (!self.bundles.items[from.index()].allocation.isNone() or
            !self.bundles.items[to.index()].allocation.isNone())
        {
            return false;
        }

        // Helper to adjust range start for fixed_def overlap detection
        const adjust_range_start = struct {
            fn f(ctx: *const Self, bundle_idx: LiveBundleIndex, range: CodeRange) ProgPoint {
                if (ctx.bundles.items[bundle_idx.index()].cachedFixedDef()) {
                    return ProgPoint.before(range.from.inst());
                } else {
                    return range.from;
                }
            }
        }.f;

        // Check for overlap in ranges
        const ranges_from = self.bundles.items[from.index()].ranges.items;
        const ranges_to = self.bundles.items[to.index()].ranges.items;
        var idx_from: usize = 0;
        var idx_to: usize = 0;
        var range_count: usize = 0;

        while (idx_from < ranges_from.len and idx_to < ranges_to.len) {
            range_count += 1;
            // Limit merge complexity
            if (range_count > 200) {
                return false;
            }

            if (adjust_range_start(self, from, ranges_from[idx_from].range).bits >= ranges_to[idx_to].range.to.bits) {
                idx_to += 1;
            } else if (adjust_range_start(self, to, ranges_to[idx_to].range).bits >= ranges_from[idx_from].range.to.bits) {
                idx_from += 1;
            } else {
                // Overlap - cannot merge
                return false;
            }
        }

        // Check for requirements conflict
        if (self.bundles.items[from.index()].cachedStack() or
            self.bundles.items[from.index()].cachedFixed() or
            self.bundles.items[from.index()].limit != null or
            self.bundles.items[to.index()].cachedStack() or
            self.bundles.items[to.index()].cachedFixed() or
            self.bundles.items[to.index()].limit != null)
        {
            _ = self.mergeBundleRequirements(from, to) catch return false;
        }

        // Commit to merge
        if (ranges_from.len == 0) {
            // Empty from bundle - trivial merge
            return true;
        }

        if (ranges_to.len == 0) {
            // Empty to bundle - move list over
            const list = self.bundles.items[from.index()].ranges;
            self.bundles.items[from.index()].ranges = .{};

            for (list.items) |entry| {
                self.ranges.items[entry.index.index()].bundle = to;
            }
            self.bundles.items[to.index()].ranges = list;
            self.mergeBundleProperties(from, to);
            return true;
        }

        // Update bundle pointers for all ranges being merged
        for (self.bundles.items[from.index()].ranges.items) |entry| {
            self.ranges.items[entry.index.index()].bundle = to;
        }

        if (ranges_from.len == 1) {
            // Optimize single-item merge with binary search insert
            const single_entry = self.bundles.items[from.index()].ranges.items[0];
            self.bundles.items[from.index()].ranges.clearRetainingCapacity();

            // Binary search for insertion point
            const to_ranges = &self.bundles.items[to.index()].ranges;
            var left: usize = 0;
            var right: usize = to_ranges.items.len;
            while (left < right) {
                const mid = left + (right - left) / 2;
                if (to_ranges.items[mid].range.from.bits < single_entry.range.from.bits) {
                    left = mid + 1;
                } else {
                    right = mid;
                }
            }
            try to_ranges.insert(self.allocator, left, single_entry);
        } else {
            // Multiple items - concat and sort
            const from_ranges = &self.bundles.items[from.index()].ranges;
            try self.bundles.items[to.index()].ranges.appendSlice(self.allocator, from_ranges.items);
            from_ranges.clearRetainingCapacity();

            std.mem.sort(LiveRangeListEntry, self.bundles.items[to.index()].ranges.items, {}, struct {
                fn lessThan(_: void, a: LiveRangeListEntry, b: LiveRangeListEntry) bool {
                    return a.range.from.bits < b.range.from.bits;
                }
            }.lessThan);
        }

        // Update spillset range
        if (!self.bundles.items[from.index()].spillset.eql(self.bundles.items[to.index()].spillset)) {
            const from_range = self.spillsets.items[self.bundles.items[from.index()].spillset.index()].range;
            const to_spillset_idx = self.bundles.items[to.index()].spillset.index();
            self.spillsets.items[to_spillset_idx].range = self.spillsets.items[to_spillset_idx].range.join(from_range);
        }

        self.mergeBundleProperties(from, to);

        return true;
    }

    /// Create bundles for each vreg and attempt merges.
    pub fn mergeVregBundles(
        self: *Self,
        comptime Func: type,
        func: *const Func,
    ) !void {
        // Create a bundle for each vreg with ranges
        for (0..self.vregs.items.len) |vreg_idx| {
            if (self.vregs.items[vreg_idx].ranges.items.len == 0) {
                continue;
            }

            // Create new bundle
            const bundle = LiveBundleIndex.new(self.bundles.items.len);
            try self.bundles.append(self.allocator, LiveBundle.init());

            // Clone ranges from vreg to bundle
            var range = self.vregs.items[vreg_idx].ranges.items[0].range;
            try self.bundles.items[bundle.index()].ranges.appendSlice(
                self.allocator,
                self.vregs.items[vreg_idx].ranges.items,
            );

            // Update range bundle pointers and compute overall range
            for (self.bundles.items[bundle.index()].ranges.items) |entry| {
                range = range.join(entry.range);
                self.ranges.items[entry.index.index()].bundle = bundle;
            }

            // Scan uses to compute cached properties
            var fixed = false;
            var fixed_def = false;
            var stack = false;
            var limit: ?u8 = null;

            for (self.bundles.items[bundle.index()].ranges.items) |entry| {
                for (self.ranges.items[entry.index.index()].uses.items) |u| {
                    switch (u.operand.constraint()) {
                        .fixed_reg => {
                            fixed = true;
                            if (u.operand.kind() == .def) {
                                fixed_def = true;
                            }
                        },
                        .stack => stack = true,
                        .limit => |current| {
                            const current_u8: u8 = @intCast(@min(current, 255));
                            if (limit) |prev| {
                                limit = @min(prev, current_u8);
                            } else {
                                limit = current_u8;
                            }
                        },
                        .any, .reg, .reuse => continue,
                    }
                    if (fixed and stack and fixed_def) {
                        break;
                    }
                }
            }

            if (fixed) {
                self.bundles.items[bundle.index()].setCachedFixed();
            }
            if (fixed_def) {
                self.bundles.items[bundle.index()].setCachedFixedDef();
            }
            if (stack) {
                self.bundles.items[bundle.index()].setCachedStack();
            }
            self.bundles.items[bundle.index()].limit = limit;

            // Create spillset for this bundle
            const reg_class = self.vregs.items[vreg_idx].class orelse .int;
            const ssidx = SpillSetIndex.new(self.spillsets.items.len);
            try self.spillsets.append(self.allocator, .{
                .slot = SpillSlotIndex.invalid(),
                .required = false,
                .class = reg_class,
                .hint = PReg.invalid(),
                .spill_bundle = LiveBundleIndex.invalid(),
                .splits = 0,
                .range = range,
            });
            self.bundles.items[bundle.index()].spillset = ssidx;
        }

        // Merge reuse-constraint operands
        for (0..func.numInsts()) |inst_idx| {
            const inst = Inst.new(inst_idx);
            for (func.instOperands(inst)) |op| {
                if (op.constraint() == .reuse) {
                    const reuse_idx = op.constraint().reuse;
                    const src_vreg = op.vreg().vreg();
                    const dst_vreg = func.instOperands(inst)[reuse_idx].vreg().vreg();

                    const src_bundle = self.ranges.items[self.vregs.items[src_vreg].ranges.items[0].index.index()].bundle;
                    const dst_bundle = self.ranges.items[self.vregs.items[dst_vreg].ranges.items[0].index.index()].bundle;

                    _ = try self.mergeBundles(dst_bundle, src_bundle);
                }
            }
        }

        // Merge blockparams with their inputs
        for (self.blockparam_outs.items) |bpo| {
            const to_vreg = bpo.to_vreg.index();
            const from_vreg = bpo.from_vreg.index();

            if (self.vregs.items[to_vreg].ranges.items.len == 0 or
                self.vregs.items[from_vreg].ranges.items.len == 0)
            {
                continue;
            }

            const to_bundle = self.ranges.items[self.vregs.items[to_vreg].ranges.items[0].index.index()].bundle;
            const from_bundle = self.ranges.items[self.vregs.items[from_vreg].ranges.items[0].index.index()].bundle;

            _ = try self.mergeBundles(from_bundle, to_bundle);
        }
    }

    /// Compute priority for a bundle (total instruction coverage).
    pub fn computeBundlePrio(self: *const Self, bundle: LiveBundleIndex) u32 {
        var total: u32 = 0;
        for (self.bundles.items[bundle.index()].ranges.items) |entry| {
            total += @intCast(entry.range.len());
        }
        return total;
    }

    /// Compute the most restrictive limit for a bundle.
    pub fn computeBundleLimit(self: *const Self, bundle: LiveBundleIndex) ?u8 {
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
                    .any, .reg, .reuse => continue,
                }
            }
        }
        return limit;
    }

    /// Compute the minimal range for a use (for minimal bundle detection).
    fn minimalRangeForUse(u: *const Use) CodeRange {
        const inst = u.pos.inst();
        return switch (u.operand.kind()) {
            .def => switch (u.operand.pos()) {
                .early => CodeRange{
                    .from = ProgPoint.before(inst),
                    .to = ProgPoint.after(inst),
                },
                .late => CodeRange{
                    .from = ProgPoint.after(inst),
                    .to = ProgPoint.before(inst.next()),
                },
            },
            .use => switch (u.operand.pos()) {
                .early => CodeRange{
                    .from = ProgPoint.before(inst),
                    .to = ProgPoint.after(inst),
                },
                .late => CodeRange{
                    .from = ProgPoint.after(inst),
                    .to = ProgPoint.before(inst.next()),
                },
            },
        };
    }

    /// Recompute bundle properties (prio, limit, spill_weight, cached flags).
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
            // No vreg - minimal and fixed
            minimal = true;
            fixed = true;
        } else if (num_ranges == 1) {
            // Single range - check uses
            for (first_range_data.uses.items) |u| {
                if (u.operand.constraint() == .fixed_reg) {
                    fixed = true;
                    if (u.operand.kind() == .def) {
                        fixed_def = true;
                    }
                }
                if (u.operand.constraint() == .stack) {
                    stack = true;
                }
                if (stack and fixed) {
                    break;
                }
            }

            // Minimal if only one LR with at most one use
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

    /// Queue all bundles for allocation.
    pub fn queueBundles(self: *Self) !void {
        for (0..self.bundles.items.len) |bundle_idx| {
            const bundle = LiveBundleIndex.new(bundle_idx);
            if (self.bundles.items[bundle_idx].ranges.items.len == 0) {
                continue;
            }
            self.recomputeBundleProperties(bundle);
            const prio = self.bundles.items[bundle_idx].prio;
            try self.allocation_queue.insert(bundle, prio, PReg.invalid());
        }
        self.merged_bundle_count.* = self.allocation_queue.heap.items.len;
    }
};

//=============================================================================
// Tests
//=============================================================================

test "Requirement merge - Any with anything" {
    const any: Requirement = .any;

    try std.testing.expectEqual(Requirement.register, try mergeRequirements(any, .register));
    try std.testing.expectEqual(Requirement.stack, try mergeRequirements(any, .stack));
    try std.testing.expectEqual(Requirement{ .limit = 5 }, try mergeRequirements(any, .{ .limit = 5 }));
    try std.testing.expectEqual(Requirement.any, try mergeRequirements(any, .any));
}

test "Requirement merge - Same kinds" {
    const reg: Requirement = .register;
    const stk: Requirement = .stack;
    const lim5: Requirement = .{ .limit = 5 };
    const lim3: Requirement = .{ .limit = 3 };

    try std.testing.expectEqual(Requirement.register, try mergeRequirements(reg, .register));
    try std.testing.expectEqual(Requirement.stack, try mergeRequirements(stk, .stack));
    try std.testing.expectEqual(Requirement{ .limit = 3 }, try mergeRequirements(lim5, .{ .limit = 3 }));
    try std.testing.expectEqual(Requirement{ .limit = 3 }, try mergeRequirements(lim3, .{ .limit = 5 }));
}

test "Requirement merge - FixedReg same" {
    const preg = PReg.new(5, RegClass.int);
    const req: Requirement = .{ .fixed_reg = preg };
    try std.testing.expectEqual(req, try mergeRequirements(req, req));
}

test "Requirement merge - FixedReg different - conflict" {
    const preg1 = PReg.new(5, RegClass.int);
    const preg2 = PReg.new(6, RegClass.int);
    const req1: Requirement = .{ .fixed_reg = preg1 };
    const req2: Requirement = .{ .fixed_reg = preg2 };
    try std.testing.expectError(error.Conflict, mergeRequirements(req1, req2));
}

test "Requirement merge - Register with FixedReg" {
    const preg = PReg.new(5, RegClass.int);
    const reg: Requirement = .register;
    try std.testing.expectEqual(
        Requirement{ .fixed_reg = preg },
        try mergeRequirements(reg, .{ .fixed_reg = preg }),
    );
}

test "Requirement merge - Limit with FixedReg" {
    const preg = PReg.new(5, RegClass.int);
    const lim10: Requirement = .{ .limit = 10 };
    const lim3: Requirement = .{ .limit = 3 };
    // Limit(10) + FixedReg(5) = FixedReg(5) because 10 > 5
    try std.testing.expectEqual(
        Requirement{ .fixed_reg = preg },
        try mergeRequirements(lim10, .{ .fixed_reg = preg }),
    );
    // Limit(3) + FixedReg(5) = Conflict because 3 <= 5
    try std.testing.expectError(
        error.Conflict,
        mergeRequirements(lim3, .{ .fixed_reg = preg }),
    );
}

test "Requirement merge - Stack with Register - conflict" {
    const stk: Requirement = .stack;
    const reg: Requirement = .register;
    try std.testing.expectError(error.Conflict, mergeRequirements(stk, .register));
    try std.testing.expectError(error.Conflict, mergeRequirements(reg, .stack));
}

test "Requirement is_stack and is_reg" {
    const stk: Requirement = .stack;
    const fixed_stk: Requirement = .{ .fixed_stack = PReg.new(0, RegClass.int) };
    const reg: Requirement = .register;
    const any: Requirement = .any;
    const fixed_reg: Requirement = .{ .fixed_reg = PReg.new(0, RegClass.int) };
    const lim: Requirement = .{ .limit = 5 };

    try std.testing.expect(stk.isStack());
    try std.testing.expect(fixed_stk.isStack());
    try std.testing.expect(!reg.isStack());
    try std.testing.expect(!any.isStack());

    try std.testing.expect(reg.isReg());
    try std.testing.expect(fixed_reg.isReg());
    try std.testing.expect(lim.isReg());
    try std.testing.expect(!stk.isReg());
    try std.testing.expect(!any.isReg());
}

test "RequirementConflictAt methods" {
    const pt = ProgPoint.before(Inst.new(10));

    const stack_to_reg = RequirementConflictAt{ .stack_to_reg = pt };
    try std.testing.expect(!stack_to_reg.shouldTrimEdgesAroundSplit());
    try std.testing.expectEqual(pt.bits, stack_to_reg.suggestedSplitPoint().bits);

    const other = RequirementConflictAt{ .other = pt };
    try std.testing.expect(other.shouldTrimEdgesAroundSplit());
}

test "Spill weight constants" {
    try std.testing.expect(MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT > MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT);
    try std.testing.expect(MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT > MINIMAL_BUNDLE_SPILL_WEIGHT);
    try std.testing.expect(MINIMAL_BUNDLE_SPILL_WEIGHT > BUNDLE_MAX_NORMAL_SPILL_WEIGHT);
}
