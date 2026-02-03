//! Spillslot allocation.
//!
//! Ported from regalloc2's `src/ion/spill.rs`.
//!
//! This module handles the allocation of spillslots for values that cannot
//! be kept in registers. It includes:
//! - Attempting to allocate spilled bundles to registers (second chance)
//! - Checking if spillslots can fit spillsets (range overlap detection)
//! - Allocating spillsets to spillslots
//! - Allocating actual stack offsets with proper alignment
//!
//! See audit/native/spill_audit.md for full function-by-function mapping.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const ion_data = @import("ion_data.zig");
const env_mod = @import("env.zig");
const process_mod = @import("process.zig");

const PReg = index.PReg;
const RegClass = index.RegClass;
const SpillSlot = index.SpillSlot;
const Allocation = operand_mod.Allocation;
const ProgPoint = operand_mod.ProgPoint;
const CodeRange = ion_data.CodeRange;
const LiveRangeIndex = ion_data.LiveRangeIndex;
const LiveBundleIndex = ion_data.LiveBundleIndex;
const SpillSetIndex = ion_data.SpillSetIndex;
const SpillSlotIndex = ion_data.SpillSlotIndex;
const PRegIndex = ion_data.PRegIndex;
const LiveRange = ion_data.LiveRange;
const LiveBundle = ion_data.LiveBundle;
const LiveRangeListEntry = ion_data.LiveRangeListEntry;
const SpillSet = ion_data.SpillSet;
const SpillSlotData = ion_data.SpillSlotData;
const SpillSlotList = ion_data.SpillSlotList;
const SpillSetRanges = ion_data.SpillSetRanges;
const LiveRangeKey = ion_data.LiveRangeKey;
const MachineEnv = env_mod.MachineEnv;
const RegTraversalIter = process_mod.RegTraversalIter;
const AllocRegResult = process_mod.AllocRegResult;

/// Maximum number of existing spillslots to probe before creating a new one.
/// This limits the algorithm to O(n) rather than O(n²) for many spillsets.
const MAX_ATTEMPTS: usize = 10;

//=============================================================================
// SpillContext - State for spillslot allocation
//=============================================================================

/// Context for spillslot allocation operations.
pub const SpillContext = struct {
    const Self = @This();

    allocator: std.mem.Allocator,

    // === Core data structures ===
    ranges: *std.ArrayListUnmanaged(LiveRange),
    bundles: *std.ArrayListUnmanaged(LiveBundle),
    spillsets: *std.ArrayListUnmanaged(SpillSet),
    spillslots: *std.ArrayListUnmanaged(SpillSlotData),
    slots_by_class: *[3]SpillSlotList,

    // === Spilled bundles ===
    spilled_bundles: *std.ArrayListUnmanaged(LiveBundleIndex),

    // === Machine environment ===
    env: *const MachineEnv,

    // === Output state ===
    num_spillslots: *usize,

    // === Configuration ===
    /// Whether multi-slot spillslots are named by the last slot.
    /// Some ABIs (like x86-64 SysV) name by first slot, others by last.
    multi_spillslot_named_by_last_slot: bool,

    /// Spillslot sizes by register class (in slot units).
    spillslot_sizes: [3]u32,

    // === Scratch memory for allocation attempts ===
    scratch_conflicts: *std.ArrayListUnmanaged(LiveBundleIndex),

    // === Pool for reusing SpillSetRanges ===
    scratch_spillset_pool: *std.ArrayListUnmanaged(SpillSetRanges),

    // === Statistics ===
    stats: *Stats,

    pub const Stats = struct {
        spill_bundle_reg_probes: usize = 0,
        spill_bundle_reg_success: usize = 0,
    };

    /// Configuration for the spill context.
    pub const Config = struct {
        /// Whether multi-slot spillslots are named by the last slot.
        multi_spillslot_named_by_last_slot: bool = false,
        /// Spillslot sizes by register class (in slot units).
        /// [Int, Float, Vector]
        spillslot_sizes: [3]u32 = .{ 1, 1, 2 },
    };

    /// Initialize a new SpillContext.
    pub fn init(
        allocator: std.mem.Allocator,
        ranges: *std.ArrayListUnmanaged(LiveRange),
        bundles: *std.ArrayListUnmanaged(LiveBundle),
        spillsets: *std.ArrayListUnmanaged(SpillSet),
        spillslots: *std.ArrayListUnmanaged(SpillSlotData),
        slots_by_class: *[3]SpillSlotList,
        spilled_bundles: *std.ArrayListUnmanaged(LiveBundleIndex),
        env: *const MachineEnv,
        num_spillslots: *usize,
        scratch_conflicts: *std.ArrayListUnmanaged(LiveBundleIndex),
        scratch_spillset_pool: *std.ArrayListUnmanaged(SpillSetRanges),
        stats: *Stats,
        config: Config,
    ) Self {
        return .{
            .allocator = allocator,
            .ranges = ranges,
            .bundles = bundles,
            .spillsets = spillsets,
            .spillslots = spillslots,
            .slots_by_class = slots_by_class,
            .spilled_bundles = spilled_bundles,
            .env = env,
            .num_spillslots = num_spillslots,
            .multi_spillslot_named_by_last_slot = config.multi_spillslot_named_by_last_slot,
            .spillslot_sizes = config.spillslot_sizes,
            .scratch_conflicts = scratch_conflicts,
            .scratch_spillset_pool = scratch_spillset_pool,
            .stats = stats,
        };
    }

    //=========================================================================
    // Spillslot size helpers
    //=========================================================================

    /// Get the spillslot size for a register class.
    fn spillslotSize(self: *const Self, class: RegClass) u32 {
        return self.spillslot_sizes[@intFromEnum(class)];
    }

    //=========================================================================
    // try_allocating_regs_for_spilled_bundles
    //=========================================================================

    /// Try to allocate registers for spilled bundles (second chance allocation).
    ///
    /// After the main allocation loop, some bundles may have been marked as
    /// spilled. This function gives them one more chance to find a register
    /// by trying all available registers without the eviction cost limit.
    ///
    /// If allocation still fails, the spillset is marked as "required",
    /// meaning it needs an actual spillslot.
    pub fn tryAllocatingRegsForSpilledBundles(self: *Self) !void {
        // Don't borrow self during iteration - copy the length
        const len = self.spilled_bundles.items.len;
        std.debug.print("DEBUG tryAllocRegsForSpilled: spilled_bundles.len={}\n", .{len});

        for (0..len) |i| {
            const bundle = self.spilled_bundles.items[i];
            std.debug.print("DEBUG tryAlloc bundle={} ranges={}\n", .{
                bundle.index(),
                self.bundles.items[bundle.index()].ranges.items.len,
            });

            // Skip empty bundles
            if (self.bundles.items[bundle.index()].ranges.items.len == 0) {
                continue;
            }

            const spillset = self.bundles.items[bundle.index()].spillset;
            const class = self.spillsets.items[spillset.index()].class;
            const hint = self.spillsets.items[spillset.index()].hint.asValid();

            // Sort ranges by start point (may be unsorted for empty-range bundles)
            const bundle_ranges = &self.bundles.items[bundle.index()].ranges;
            std.mem.sort(LiveRangeListEntry, bundle_ranges.items, {}, struct {
                fn lessThan(_: void, a: LiveRangeListEntry, b: LiveRangeListEntry) bool {
                    return a.range.from.bits < b.range.from.bits;
                }
            }.lessThan);

            var success = false;
            self.stats.spill_bundle_reg_probes += 1;

            // Get limit from bundle
            const limit: ?usize = if (self.bundles.items[bundle.index()].limit) |l|
                @as(usize, l)
            else
                null;

            // Try all registers via RegTraversalIter
            std.debug.print("DEBUG RegTraversalIter: class={} hint_valid={} limit={?}\n", .{
                @intFromEnum(class),
                hint != null,
                limit,
            });
            std.debug.print("DEBUG env preferred int regs: {}\n", .{
                self.env.preferred_regs_by_class[@intFromEnum(RegClass.int)].isEmpty(),
            });
            std.debug.print("DEBUG env non_preferred int regs: {}\n", .{
                self.env.non_preferred_regs_by_class[@intFromEnum(RegClass.int)].isEmpty(),
            });
            var reg_iter = RegTraversalIter.init(
                self.env,
                class,
                null, // No fixed register requirement
                hint,
                bundle.index(), // Use bundle index as offset for distribution
                limit,
            );

            var reg_count: usize = 0;
            while (reg_iter.next()) |preg| {
                reg_count += 1;
                const preg_idx = PRegIndex.new(preg.index());
                const result = try self.tryToAllocateBundleToReg(bundle, preg_idx);

                switch (result) {
                    .allocated => {
                        std.debug.print("DEBUG tryAlloc bundle={} SUCCESS preg={}\n", .{ bundle.index(), preg.hwEnc() });
                        self.stats.spill_bundle_reg_success += 1;
                        success = true;
                        break;
                    },
                    else => {
                        // Keep trying other registers
                    },
                }
            }
            std.debug.print("DEBUG tryAlloc bundle={} regs_tried={} success={}\n", .{ bundle.index(), reg_count, success });

            if (!success) {
                // Mark spillset as required - it needs an actual spillslot
                self.spillsets.items[self.bundles.items[bundle.index()].spillset.index()].required = true;
            }
        }
    }

    /// Try to allocate a bundle to a specific register.
    /// Simplified version that just checks for conflicts (no eviction).
    fn tryToAllocateBundleToReg(
        self: *Self,
        bundle: LiveBundleIndex,
        reg: PRegIndex,
    ) !AllocRegResult {
        self.scratch_conflicts.clearRetainingCapacity();

        const bundle_ranges = self.bundles.items[bundle.index()].ranges.items;
        if (bundle_ranges.len == 0) {
            // Empty bundle - allocate trivially
            const preg = PReg.fromIndex(reg.index());
            self.bundles.items[bundle.index()].allocation = Allocation.reg(preg);
            return AllocRegResult{ .allocated = Allocation.reg(preg) };
        }

        var has_conflict = false;
        var first_conflict: ?ProgPoint = null;

        // Check each bundle range against the preg's allocation set
        for (bundle_ranges) |entry| {
            const key = LiveRangeKey.fromRange(entry.range);

            // Check the preg's allocated ranges for overlap
            const preg_ranges = &self.ranges.items[entry.index.index()];
            _ = preg_ranges;

            // For now, we need to check against all bundles allocated to this preg
            // This is a simplified check - in process.zig we have the full PRegData btree
            // For spill allocation, we mainly care about whether we succeed
            // The full conflict detection is in ProcessContext.tryToAllocateBundleToReg

            // Since we don't have access to PRegData here, we use a simplified approach:
            // Check if any other bundle with the same allocation overlaps
            for (self.bundles.items) |*other_bundle| {
                if (other_bundle.allocation.asReg()) |other_preg| {
                    if (other_preg.index() == reg.index()) {
                        // Same register - check for overlap
                        for (other_bundle.ranges.items) |other_entry| {
                            const other_key = LiveRangeKey.fromRange(other_entry.range);
                            if (key.order(other_key) == .eq) {
                                // Overlap detected
                                has_conflict = true;
                                if (first_conflict == null) {
                                    first_conflict = entry.range.from;
                                }
                                break;
                            }
                        }
                    }
                }
                if (has_conflict) break;
            }
            if (has_conflict) break;
        }

        if (has_conflict) {
            return AllocRegResult{
                .conflict = .{
                    .bundles = self.scratch_conflicts.items,
                    .point = first_conflict orelse ProgPoint{ .bits = 0 },
                },
            };
        }

        // Success - assign allocation
        const preg = PReg.fromIndex(reg.index());
        self.bundles.items[bundle.index()].allocation = Allocation.reg(preg);

        return AllocRegResult{ .allocated = Allocation.reg(preg) };
    }

    //=========================================================================
    // spillslot_can_fit_spillset
    //=========================================================================

    /// Check if a spillslot can accommodate a spillset (no range overlap).
    pub fn spillslotCanFitSpillset(
        self: *const Self,
        spillslot: SpillSlotIndex,
        spillset: SpillSetIndex,
    ) bool {
        const key = LiveRangeKey.fromRange(self.spillsets.items[spillset.index()].range);
        return !self.spillslots.items[spillslot.index()].ranges.containsKey(key);
    }

    //=========================================================================
    // allocate_spillset_to_spillslot
    //=========================================================================

    /// Assign a spillset to a spillslot.
    pub fn allocateSpillsetToSpillslot(
        self: *Self,
        spillset: SpillSetIndex,
        spillslot: SpillSlotIndex,
    ) !void {
        // Update spillset's slot reference
        self.spillsets.items[spillset.index()].slot = spillslot;

        // Insert the spillset's range into the spillslot's range tree
        const key = LiveRangeKey.fromRange(self.spillsets.items[spillset.index()].range);
        const result = try self.spillslots.items[spillslot.index()].ranges.insert(
            self.allocator,
            key,
            spillset,
        );

        // The range should not already exist (no overlap expected)
        std.debug.assert(result == null);
    }

    //=========================================================================
    // allocate_spillslots
    //=========================================================================

    /// Allocate spillslots for all required spillsets.
    ///
    /// This is the main spillslot allocation loop. For each spillset that
    /// requires a spillslot:
    /// 1. Try to reuse an existing spillslot (circular probing)
    /// 2. If no fit, create a new spillslot
    /// 3. Assign the actual stack allocation at the end
    pub fn allocateSpillslots(self: *Self) !void {
        // Iterate through all spillsets
        for (0..self.spillsets.items.len) |spillset_idx| {
            const spillset = SpillSetIndex.new(spillset_idx);

            // Skip spillsets that don't need a spillslot
            if (!self.spillsets.items[spillset.index()].required) {
                continue;
            }

            const class = @intFromEnum(self.spillsets.items[spillset.index()].class);

            // Try to find an existing spillslot that can fit this spillset
            var i = self.slots_by_class[class].probe_start;
            var success = false;

            // Limit probes to avoid O(n²) behavior
            const num_slots = self.slots_by_class[class].slots.items.len;
            const max_probes = @min(num_slots, MAX_ATTEMPTS);

            for (0..max_probes) |_| {
                const spillslot = self.slots_by_class[class].slots.items[i];

                if (self.spillslotCanFitSpillset(spillslot, spillset)) {
                    try self.allocateSpillsetToSpillslot(spillset, spillslot);
                    success = true;
                    self.slots_by_class[class].probe_start = i;
                    break;
                }

                // Circular probing
                i = self.slots_by_class[class].nextIndex(i);
            }

            if (!success) {
                // Allocate a new spillslot
                const spillslot = SpillSlotIndex.new(self.spillslots.items.len);

                // Get a SpillSetRanges from the pool or create a new one
                const ranges = if (self.scratch_spillset_pool.items.len > 0)
                    self.scratch_spillset_pool.pop().?
                else
                    SpillSetRanges.init();

                try self.spillslots.append(self.allocator, SpillSlotData{
                    .ranges = ranges,
                    .alloc = Allocation.none(),
                    .slots = self.spillslotSize(self.spillsets.items[spillset.index()].class),
                });

                try self.slots_by_class[class].slots.append(self.allocator, spillslot);
                self.slots_by_class[class].probe_start = self.slots_by_class[class].slots.items.len - 1;

                try self.allocateSpillsetToSpillslot(spillset, spillslot);
            }
        }

        // Assign actual slot indices to all spillslots
        for (self.spillslots.items) |*spillslot| {
            spillslot.alloc = self.allocateSpillslot(spillslot.slots);
        }
    }

    //=========================================================================
    // allocate_spillslot
    //=========================================================================

    /// Allocate an actual stack slot of the given size (in slot units).
    ///
    /// Returns an Allocation representing the stack slot. The allocation
    /// is aligned to the size (power of two alignment).
    pub fn allocateSpillslot(self: *Self, size: u32) Allocation {
        var offset: u32 = @intCast(self.num_spillslots.*);

        // Align up to `size` (size must be power of two)
        std.debug.assert(std.math.isPowerOfTwo(size));
        offset = (offset + size - 1) & ~(size - 1);

        // Determine the slot number based on ABI convention
        const slot = if (self.multi_spillslot_named_by_last_slot)
            offset + size - 1
        else
            offset;

        // Update total spillslot count
        offset += size;
        self.num_spillslots.* = offset;

        return Allocation.stack(SpillSlot.new(slot));
    }
};

//=============================================================================
// Tests
//=============================================================================

test "SpillSetRanges containsKey - overlapping ranges" {
    const allocator = std.testing.allocator;

    var ranges = SpillSetRanges.init();
    defer ranges.deinit(allocator);

    // Insert a range [10, 20)
    const key1 = LiveRangeKey{ .from = 10, .to = 20 };
    _ = try ranges.insert(allocator, key1, SpillSetIndex.new(0));

    // Check non-overlapping key
    const key2 = LiveRangeKey{ .from = 25, .to = 35 };
    try std.testing.expect(!ranges.containsKey(key2));

    // Check overlapping key
    const key3 = LiveRangeKey{ .from = 15, .to = 25 };
    try std.testing.expect(ranges.containsKey(key3));

    // Check exactly matching key
    try std.testing.expect(ranges.containsKey(key1));
}

test "allocateSpillslot alignment" {
    var num_spillslots: usize = 0;
    var spillslots: std.ArrayListUnmanaged(SpillSlotData) = .{};
    var slots_by_class = [3]SpillSlotList{
        SpillSlotList.init(),
        SpillSlotList.init(),
        SpillSlotList.init(),
    };
    var ranges: std.ArrayListUnmanaged(LiveRange) = .{};
    var bundles: std.ArrayListUnmanaged(LiveBundle) = .{};
    var spillsets: std.ArrayListUnmanaged(SpillSet) = .{};
    var spilled_bundles: std.ArrayListUnmanaged(LiveBundleIndex) = .{};
    var scratch_conflicts: std.ArrayListUnmanaged(LiveBundleIndex) = .{};
    var scratch_pool: std.ArrayListUnmanaged(SpillSetRanges) = .{};
    var stats = SpillContext.Stats{};

    const env = MachineEnv.empty();

    var ctx = SpillContext.init(
        std.testing.allocator,
        &ranges,
        &bundles,
        &spillsets,
        &spillslots,
        &slots_by_class,
        &spilled_bundles,
        &env,
        &num_spillslots,
        &scratch_conflicts,
        &scratch_pool,
        &stats,
        .{},
    );
    _ = &ctx;

    // Test size-1 allocation (no alignment needed)
    const alloc1 = ctx.allocateSpillslot(1);
    try std.testing.expectEqual(@as(usize, 0), alloc1.asStack().?.index());
    try std.testing.expectEqual(@as(usize, 1), num_spillslots);

    // Test size-2 allocation (needs alignment to 2)
    const alloc2 = ctx.allocateSpillslot(2);
    try std.testing.expectEqual(@as(usize, 2), alloc2.asStack().?.index()); // Aligned to 2
    try std.testing.expectEqual(@as(usize, 4), num_spillslots);

    // Test size-1 again (fills in gap)
    num_spillslots = 3; // Reset to odd number
    const alloc3 = ctx.allocateSpillslot(2);
    try std.testing.expectEqual(@as(usize, 4), alloc3.asStack().?.index()); // Aligned to 4
    try std.testing.expectEqual(@as(usize, 6), num_spillslots);
}

test "allocateSpillslot multi_spillslot_named_by_last_slot" {
    var num_spillslots: usize = 0;
    var spillslots: std.ArrayListUnmanaged(SpillSlotData) = .{};
    var slots_by_class = [3]SpillSlotList{
        SpillSlotList.init(),
        SpillSlotList.init(),
        SpillSlotList.init(),
    };
    var ranges: std.ArrayListUnmanaged(LiveRange) = .{};
    var bundles: std.ArrayListUnmanaged(LiveBundle) = .{};
    var spillsets: std.ArrayListUnmanaged(SpillSet) = .{};
    var spilled_bundles: std.ArrayListUnmanaged(LiveBundleIndex) = .{};
    var scratch_conflicts: std.ArrayListUnmanaged(LiveBundleIndex) = .{};
    var scratch_pool: std.ArrayListUnmanaged(SpillSetRanges) = .{};
    var stats = SpillContext.Stats{};

    const env = MachineEnv.empty();

    var ctx = SpillContext.init(
        std.testing.allocator,
        &ranges,
        &bundles,
        &spillsets,
        &spillslots,
        &slots_by_class,
        &spilled_bundles,
        &env,
        &num_spillslots,
        &scratch_conflicts,
        &scratch_pool,
        &stats,
        .{ .multi_spillslot_named_by_last_slot = true },
    );
    _ = &ctx;

    // With multi_spillslot_named_by_last_slot = true, a size-2 slot at offset 0
    // should be named by slot 1 (offset + size - 1)
    const alloc = ctx.allocateSpillslot(2);
    try std.testing.expectEqual(@as(usize, 1), alloc.asStack().?.index()); // Named by last slot
    try std.testing.expectEqual(@as(usize, 2), num_spillslots);
}

test "SpillSlotList nextIndex circular" {
    const allocator = std.testing.allocator;

    var list = SpillSlotList.init();
    defer list.deinit(allocator);

    try list.slots.append(allocator, SpillSlotIndex.new(0));
    try list.slots.append(allocator, SpillSlotIndex.new(1));
    try list.slots.append(allocator, SpillSlotIndex.new(2));

    // Normal increment
    try std.testing.expectEqual(@as(usize, 1), list.nextIndex(0));
    try std.testing.expectEqual(@as(usize, 2), list.nextIndex(1));

    // Wrap around
    try std.testing.expectEqual(@as(usize, 0), list.nextIndex(2));
}
