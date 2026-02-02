//! Data structures for the Ion backtracking allocator.
//!
//! Ported from regalloc2's `src/ion/data_structures.rs`.
//!
//! This module contains all the core data structures used by the Ion
//! register allocator, including live ranges, bundles, spill sets, and
//! the main allocator context.

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const indexset = @import("indexset.zig");
const cfg_mod = @import("cfg.zig");
const output_mod = @import("output.zig");

const Block = index.Block;
const Inst = index.Inst;
const VReg = index.VReg;
const PReg = index.PReg;
const PRegSet = index.PRegSet;
const RegClass = index.RegClass;
const SpillSlot = index.SpillSlot;
const Allocation = operand_mod.Allocation;
const Operand = operand_mod.Operand;
const ProgPoint = operand_mod.ProgPoint;
const Edit = operand_mod.Edit;
const IndexSet = indexset.IndexSet;
const CFGInfo = cfg_mod.CFGInfo;
const CFGInfoCtx = cfg_mod.CFGInfoCtx;
const Output = output_mod.Output;

//=============================================================================
// Index Types - Strongly typed indices into data structure arrays
//=============================================================================

/// Index into LiveRanges array.
pub const LiveRangeIndex = struct {
    idx: u32,

    pub fn new(i: usize) LiveRangeIndex {
        return .{ .idx = @intCast(i) };
    }

    pub fn index(self: LiveRangeIndex) usize {
        return self.idx;
    }

    pub fn invalid() LiveRangeIndex {
        return .{ .idx = std.math.maxInt(u32) };
    }

    pub fn isValid(self: LiveRangeIndex) bool {
        return self.idx != std.math.maxInt(u32);
    }

    pub fn eql(self: LiveRangeIndex, other: LiveRangeIndex) bool {
        return self.idx == other.idx;
    }

    pub fn rawU32(self: LiveRangeIndex) u32 {
        return self.idx;
    }
};

/// Index into LiveBundles array.
pub const LiveBundleIndex = struct {
    idx: u32,

    pub fn new(i: usize) LiveBundleIndex {
        return .{ .idx = @intCast(i) };
    }

    pub fn index(self: LiveBundleIndex) usize {
        return self.idx;
    }

    pub fn invalid() LiveBundleIndex {
        return .{ .idx = std.math.maxInt(u32) };
    }

    pub fn isValid(self: LiveBundleIndex) bool {
        return self.idx != std.math.maxInt(u32);
    }

    pub fn eql(self: LiveBundleIndex, other: LiveBundleIndex) bool {
        return self.idx == other.idx;
    }

    pub fn rawU32(self: LiveBundleIndex) u32 {
        return self.idx;
    }
};

/// Index into SpillSets array.
pub const SpillSetIndex = struct {
    idx: u32,

    pub fn new(i: usize) SpillSetIndex {
        return .{ .idx = @intCast(i) };
    }

    pub fn index(self: SpillSetIndex) usize {
        return self.idx;
    }

    pub fn invalid() SpillSetIndex {
        return .{ .idx = std.math.maxInt(u32) };
    }

    pub fn isValid(self: SpillSetIndex) bool {
        return self.idx != std.math.maxInt(u32);
    }

    pub fn eql(self: SpillSetIndex, other: SpillSetIndex) bool {
        return self.idx == other.idx;
    }
};

/// Index into Uses array.
pub const UseIndex = struct {
    idx: u32,

    pub fn new(i: usize) UseIndex {
        return .{ .idx = @intCast(i) };
    }

    pub fn index(self: UseIndex) usize {
        return self.idx;
    }

    pub fn invalid() UseIndex {
        return .{ .idx = std.math.maxInt(u32) };
    }

    pub fn isValid(self: UseIndex) bool {
        return self.idx != std.math.maxInt(u32);
    }
};

/// Index into VRegs array.
pub const VRegIndex = struct {
    idx: u32,

    pub fn new(i: usize) VRegIndex {
        return .{ .idx = @intCast(i) };
    }

    pub fn index(self: VRegIndex) usize {
        return self.idx;
    }

    pub fn invalid() VRegIndex {
        return .{ .idx = std.math.maxInt(u32) };
    }

    pub fn isValid(self: VRegIndex) bool {
        return self.idx != std.math.maxInt(u32);
    }

    pub fn eql(self: VRegIndex, other: VRegIndex) bool {
        return self.idx == other.idx;
    }

    pub fn rawU32(self: VRegIndex) u32 {
        return self.idx;
    }
};

/// Index into PRegs array.
pub const PRegIndex = struct {
    idx: u32,

    pub fn new(i: usize) PRegIndex {
        return .{ .idx = @intCast(i) };
    }

    pub fn index(self: PRegIndex) usize {
        return self.idx;
    }

    pub fn invalid() PRegIndex {
        return .{ .idx = std.math.maxInt(u32) };
    }

    pub fn isValid(self: PRegIndex) bool {
        return self.idx != std.math.maxInt(u32);
    }
};

/// Index into SpillSlots array.
pub const SpillSlotIndex = struct {
    idx: u32,

    pub fn new(i: usize) SpillSlotIndex {
        return .{ .idx = @intCast(i) };
    }

    pub fn index(self: SpillSlotIndex) usize {
        return self.idx;
    }

    pub fn invalid() SpillSlotIndex {
        return .{ .idx = std.math.maxInt(u32) };
    }

    pub fn isValid(self: SpillSlotIndex) bool {
        return self.idx != std.math.maxInt(u32);
    }
};

//=============================================================================
// CodeRange - A range of program points
//=============================================================================

/// A range from `from` (inclusive) to `to` (exclusive).
pub const CodeRange = struct {
    from: ProgPoint,
    to: ProgPoint,

    /// Check if the range is empty.
    pub fn isEmpty(self: CodeRange) bool {
        return self.from.bits >= self.to.bits;
    }

    /// Check if this range contains another range entirely.
    pub fn contains(self: CodeRange, other: CodeRange) bool {
        return other.from.bits >= self.from.bits and other.to.bits <= self.to.bits;
    }

    /// Check if this range contains a specific program point.
    pub fn containsPoint(self: CodeRange, point: ProgPoint) bool {
        return point.bits >= self.from.bits and point.bits < self.to.bits;
    }

    /// Check if this range overlaps another range.
    pub fn overlaps(self: CodeRange, other: CodeRange) bool {
        return other.to.bits > self.from.bits and other.from.bits < self.to.bits;
    }

    /// Get the length of the range in instructions.
    pub fn len(self: CodeRange) usize {
        return self.to.inst().idx() - self.from.inst().idx();
    }

    /// Create a range covering just one program point.
    pub fn singleton(pos: ProgPoint) CodeRange {
        return .{
            .from = pos,
            .to = pos.nextPoint(),
        };
    }

    /// Join two ranges, producing a range that includes both.
    pub fn join(self: CodeRange, other: CodeRange) CodeRange {
        return .{
            .from = if (self.from.bits < other.from.bits) self.from else other.from,
            .to = if (self.to.bits > other.to.bits) self.to else other.to,
        };
    }

    /// Compare two ranges for ordering.
    /// Ranges that don't overlap are ordered by position.
    /// Overlapping ranges are considered equal.
    pub fn order(self: CodeRange, other: CodeRange) std.math.Order {
        if (self.to.bits <= other.from.bits) {
            return .lt;
        } else if (self.from.bits >= other.to.bits) {
            return .gt;
        } else {
            return .eq;
        }
    }
};

//=============================================================================
// Use - A use of a virtual register
//=============================================================================

/// A use of a virtual register at a specific program point.
pub const Use = struct {
    operand: Operand,
    pos: ProgPoint,
    slot: u16,
    weight: u16,

    pub fn new(op: Operand, pos: ProgPoint, slot: u16) Use {
        return .{
            .operand = op,
            .pos = pos,
            .slot = slot,
            .weight = 0, // Updated on insertion into live range
        };
    }
};

//=============================================================================
// LiveRangeFlag - Flags for live ranges
//=============================================================================

pub const LiveRangeFlag = enum(u32) {
    starts_at_def = 1,
};

//=============================================================================
// SpillWeight - Weight for spill decisions
//=============================================================================

/// Spill weight stored as f32 bits.
pub const SpillWeight = struct {
    bits: u32,

    pub fn fromF32(f: f32) SpillWeight {
        return .{ .bits = @bitCast(f) };
    }

    pub fn toF32(self: SpillWeight) f32 {
        return @bitCast(self.bits);
    }

    pub fn zero() SpillWeight {
        return .{ .bits = 0 };
    }

    pub fn add(self: SpillWeight, other: SpillWeight) SpillWeight {
        const a: f32 = @bitCast(self.bits);
        const b: f32 = @bitCast(other.bits);
        return .{ .bits = @bitCast(a + b) };
    }

    pub fn toInt(self: SpillWeight) u32 {
        const f: f32 = @bitCast(self.bits);
        return @intFromFloat(f);
    }
};

//=============================================================================
// LiveRangeListEntry - Entry in a list of live ranges
//=============================================================================

pub const LiveRangeListEntry = struct {
    range: CodeRange,
    index: LiveRangeIndex,
};

//=============================================================================
// LiveRange - A live range of a virtual register
//=============================================================================

/// A contiguous range where a virtual register is live.
pub const LiveRange = struct {
    range: CodeRange,
    vreg: VRegIndex,
    bundle: LiveBundleIndex,
    /// Packed: bits 0-28 = spill weight, bits 29-31 = flags
    uses_spill_weight_and_flags: u32,
    uses: std.ArrayListUnmanaged(Use),

    pub fn init(rng: CodeRange) LiveRange {
        return .{
            .range = rng,
            .vreg = VRegIndex.invalid(),
            .bundle = LiveBundleIndex.invalid(),
            .uses_spill_weight_and_flags = 0,
            .uses = .{},
        };
    }

    pub fn deinit(self: *LiveRange, allocator: std.mem.Allocator) void {
        self.uses.deinit(allocator);
    }

    pub fn setFlag(self: *LiveRange, flag: LiveRangeFlag) void {
        self.uses_spill_weight_and_flags |= @as(u32, @intFromEnum(flag)) << 29;
    }

    pub fn clearFlag(self: *LiveRange, flag: LiveRangeFlag) void {
        self.uses_spill_weight_and_flags &= ~(@as(u32, @intFromEnum(flag)) << 29);
    }

    pub fn hasFlag(self: LiveRange, flag: LiveRangeFlag) bool {
        return (self.uses_spill_weight_and_flags & (@as(u32, @intFromEnum(flag)) << 29)) != 0;
    }

    pub fn flagWord(self: LiveRange) u32 {
        return self.uses_spill_weight_and_flags & 0xe000_0000;
    }

    pub fn mergeFlags(self: *LiveRange, flag_word: u32) void {
        self.uses_spill_weight_and_flags |= flag_word;
    }

    pub fn usesSpillWeight(self: LiveRange) SpillWeight {
        const bits = (self.uses_spill_weight_and_flags & 0x1fff_ffff) << 2;
        return SpillWeight.fromF32(@bitCast(bits));
    }

    pub fn setUsesSpillWeight(self: *LiveRange, weight: SpillWeight) void {
        const weight_bits = (weight.bits >> 2) & 0x1fff_ffff;
        self.uses_spill_weight_and_flags = (self.uses_spill_weight_and_flags & 0xe000_0000) | weight_bits;
    }
};

//=============================================================================
// LiveBundle - A bundle of live ranges that should be allocated together
//=============================================================================

pub const BUNDLE_MAX_SPILL_WEIGHT: u32 = (1 << 28) - 1;
pub const MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT: u32 = BUNDLE_MAX_SPILL_WEIGHT;
pub const MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT: u32 = BUNDLE_MAX_SPILL_WEIGHT - 1;
pub const MINIMAL_BUNDLE_SPILL_WEIGHT: u32 = MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT - 256;
pub const BUNDLE_MAX_NORMAL_SPILL_WEIGHT: u32 = MINIMAL_BUNDLE_SPILL_WEIGHT - 1;

/// A bundle of live ranges that should be allocated to the same location.
pub const LiveBundle = struct {
    ranges: std.ArrayListUnmanaged(LiveRangeListEntry),
    spillset: SpillSetIndex,
    allocation: Allocation,
    prio: u32,
    /// Packed: bits 0-27 = spill weight, bits 28-31 = properties
    spill_weight_and_props: u32,
    limit: ?u8,

    pub fn init() LiveBundle {
        return .{
            .ranges = .{},
            .spillset = SpillSetIndex.invalid(),
            .allocation = Allocation.none(),
            .prio = 0,
            .spill_weight_and_props = 0,
            .limit = null,
        };
    }

    pub fn deinit(self: *LiveBundle, allocator: std.mem.Allocator) void {
        self.ranges.deinit(allocator);
    }

    pub fn setCachedSpillWeightAndProps(
        self: *LiveBundle,
        spill_weight: u32,
        minimal: bool,
        fixed: bool,
        fixed_def: bool,
        stack: bool,
    ) void {
        self.spill_weight_and_props = spill_weight |
            (if (minimal) @as(u32, 1) << 31 else 0) |
            (if (fixed) @as(u32, 1) << 30 else 0) |
            (if (fixed_def) @as(u32, 1) << 29 else 0) |
            (if (stack) @as(u32, 1) << 28 else 0);
    }

    pub fn cachedMinimal(self: LiveBundle) bool {
        return (self.spill_weight_and_props & (1 << 31)) != 0;
    }

    pub fn cachedFixed(self: LiveBundle) bool {
        return (self.spill_weight_and_props & (1 << 30)) != 0;
    }

    pub fn cachedFixedDef(self: LiveBundle) bool {
        return (self.spill_weight_and_props & (1 << 29)) != 0;
    }

    pub fn cachedStack(self: LiveBundle) bool {
        return (self.spill_weight_and_props & (1 << 28)) != 0;
    }

    pub fn setCachedFixed(self: *LiveBundle) void {
        self.spill_weight_and_props |= 1 << 30;
    }

    pub fn setCachedFixedDef(self: *LiveBundle) void {
        self.spill_weight_and_props |= 1 << 29;
    }

    pub fn setCachedStack(self: *LiveBundle) void {
        self.spill_weight_and_props |= 1 << 28;
    }

    pub fn cachedSpillWeight(self: LiveBundle) u32 {
        return self.spill_weight_and_props & BUNDLE_MAX_SPILL_WEIGHT;
    }
};

//=============================================================================
// SpillSet - A set of bundles that share a spill slot
//=============================================================================

pub const MAX_SPLITS_PER_SPILLSET: u8 = 2;

/// A set of live bundles that can share a spill slot.
pub const SpillSet = struct {
    slot: SpillSlotIndex,
    hint: PReg,
    class: RegClass,
    spill_bundle: LiveBundleIndex,
    required: bool,
    splits: u8,
    range: CodeRange,
};

//=============================================================================
// VRegData - Data for a virtual register
//=============================================================================

/// Per-VReg data.
pub const VRegData = struct {
    ranges: std.ArrayListUnmanaged(LiveRangeListEntry),
    blockparam: Block,
    class: ?RegClass,

    pub fn init() VRegData {
        return .{
            .ranges = .{},
            .blockparam = Block.invalid(),
            .class = null,
        };
    }

    pub fn deinit(self: *VRegData, allocator: std.mem.Allocator) void {
        self.ranges.deinit(allocator);
    }
};

//=============================================================================
// PRegData - Data for a physical register
//=============================================================================

/// Per-PReg allocation data.
pub const PRegData = struct {
    allocations: LiveRangeSet,
    is_stack: bool,

    pub fn init() PRegData {
        return .{
            .allocations = LiveRangeSet.init(),
            .is_stack = false,
        };
    }

    pub fn deinit(self: *PRegData, allocator: std.mem.Allocator) void {
        self.allocations.deinit(allocator);
    }
};

//=============================================================================
// LiveRangeKey - Key for live range lookup (with overlap comparison)
//=============================================================================

/// Key for looking up live ranges in a map.
/// Comparison is based on overlap: overlapping keys are equal.
pub const LiveRangeKey = struct {
    from: u32,
    to: u32,

    pub fn fromRange(rng: CodeRange) LiveRangeKey {
        return .{
            .from = rng.from.bits,
            .to = rng.to.bits,
        };
    }

    pub fn toRange(self: LiveRangeKey) CodeRange {
        return .{
            .from = .{ .bits = self.from },
            .to = .{ .bits = self.to },
        };
    }

    /// Compare keys - overlapping keys are considered equal.
    pub fn order(self: LiveRangeKey, other: LiveRangeKey) std.math.Order {
        if (self.to <= other.from) {
            return .lt;
        } else if (self.from >= other.to) {
            return .gt;
        } else {
            return .eq;
        }
    }
};

//=============================================================================
// LiveRangeSet - Set of live ranges indexed by key
//=============================================================================

/// A set of live ranges, indexed by their code range.
pub const LiveRangeSet = struct {
    /// Map from range key to live range index.
    /// Uses std.BTreeMap equivalent.
    items: std.ArrayListUnmanaged(struct { key: LiveRangeKey, value: LiveRangeIndex }),

    pub fn init() LiveRangeSet {
        return .{ .items = .{} };
    }

    pub fn deinit(self: *LiveRangeSet, allocator: std.mem.Allocator) void {
        self.items.deinit(allocator);
    }

    /// Insert a range. Returns true if inserted, false if overlaps existing.
    pub fn insert(self: *LiveRangeSet, allocator: std.mem.Allocator, key: LiveRangeKey, value: LiveRangeIndex) !bool {
        // Check for overlap
        for (self.items.items) |item| {
            if (item.key.order(key) == .eq) {
                return false; // Overlaps
            }
        }
        try self.items.append(allocator, .{ .key = key, .value = value });
        return true;
    }

    /// Remove a range by key. Returns true if found and removed.
    pub fn remove(self: *LiveRangeSet, key: LiveRangeKey) bool {
        for (self.items.items, 0..) |item, i| {
            if (item.key.order(key) == .eq) {
                _ = self.items.orderedRemove(i);
                return true;
            }
        }
        return false;
    }

    /// Find a range that overlaps the given key.
    pub fn findOverlap(self: LiveRangeSet, key: LiveRangeKey) ?LiveRangeIndex {
        for (self.items.items) |item| {
            if (item.key.order(key) == .eq) {
                return item.value;
            }
        }
        return null;
    }
};

//=============================================================================
// BlockparamOut/In - Block parameter data flow
//=============================================================================

/// Outgoing block parameter assignment.
pub const BlockparamOut = struct {
    to_vreg: VRegIndex,
    to_block: Block,
    from_block: Block,
    from_vreg: VRegIndex,

    pub fn key(self: BlockparamOut) u128 {
        return u128Key(
            self.from_vreg.rawU32(),
            self.from_block.rawU32(),
            self.to_block.rawU32(),
            self.to_vreg.rawU32(),
        );
    }
};

/// Incoming block parameter assignment.
pub const BlockparamIn = struct {
    from_block: Block,
    to_block: Block,
    to_vreg: VRegIndex,

    pub fn key(self: BlockparamIn) u128 {
        return u128Key(
            self.to_vreg.rawU32(),
            self.to_block.rawU32(),
            self.from_block.rawU32(),
            0,
        );
    }
};

//=============================================================================
// FixedRegFixup - Fixup for multiple fixed-register constraints
//=============================================================================

pub const FixedRegFixupLevel = enum {
    /// Initial fixup copy; must come first.
    initial,
    /// Secondary fixup from first fixed reg to others; must come second.
    secondary,
};

pub const MultiFixedRegFixup = struct {
    pos: ProgPoint,
    from_slot: u16,
    to_slot: u16,
    level: FixedRegFixupLevel,
    to_preg: PRegIndex,
    vreg: VRegIndex,
};

//=============================================================================
// PrioQueue - Priority queue for bundle allocation
//=============================================================================

pub const PrioQueueEntry = struct {
    prio: u32,
    bundle: LiveBundleIndex,
    hint: PReg,
};

/// Priority queue for allocating bundles in priority order.
pub const PrioQueue = struct {
    heap: std.PriorityQueue(PrioQueueEntry, void, lessThan),

    fn lessThan(_: void, a: PrioQueueEntry, b: PrioQueueEntry) std.math.Order {
        // Max heap: higher priority comes first
        return std.math.order(b.prio, a.prio);
    }

    pub fn init(allocator: std.mem.Allocator) PrioQueue {
        return .{
            .heap = std.PriorityQueue(PrioQueueEntry, void, lessThan).init(allocator, {}),
        };
    }

    pub fn deinit(self: *PrioQueue) void {
        self.heap.deinit();
    }

    pub fn insert(self: *PrioQueue, bundle: LiveBundleIndex, prio: usize, hint: PReg) !void {
        try self.heap.add(.{
            .prio = @intCast(prio),
            .bundle = bundle,
            .hint = hint,
        });
    }

    pub fn isEmpty(self: PrioQueue) bool {
        return self.heap.count() == 0;
    }

    pub fn pop(self: *PrioQueue) ?struct { bundle: LiveBundleIndex, hint: PReg } {
        if (self.heap.removeOrNull()) |entry| {
            return .{ .bundle = entry.bundle, .hint = entry.hint };
        }
        return null;
    }

    pub fn clear(self: *PrioQueue) void {
        while (self.heap.removeOrNull()) |_| {}
    }
};

//=============================================================================
// InsertedMove - A move to be inserted
//=============================================================================

pub const InsertMovePrio = enum(u32) {
    in_edge_moves = 0,
    regular = 1,
    multi_fixed_reg_initial = 2,
    multi_fixed_reg_secondary = 3,
    reused_input = 4,
    out_edge_moves = 5,
};

pub const PosWithPrio = struct {
    prio: u32,
    pos: ProgPoint,

    pub fn key(self: PosWithPrio) u64 {
        return u64Key(self.pos.bits, self.prio);
    }
};

pub const InsertedMove = struct {
    pos_prio: PosWithPrio,
    from_alloc: Allocation,
    to_alloc: Allocation,
    to_vreg: VReg,
};

pub const InsertedMoves = struct {
    moves: std.ArrayListUnmanaged(InsertedMove),

    pub fn init() InsertedMoves {
        return .{ .moves = .{} };
    }

    pub fn deinit(self: *InsertedMoves, allocator: std.mem.Allocator) void {
        self.moves.deinit(allocator);
    }

    pub fn push(
        self: *InsertedMoves,
        allocator: std.mem.Allocator,
        pos: ProgPoint,
        prio: InsertMovePrio,
        from_alloc: Allocation,
        to_alloc: Allocation,
        to_vreg: VReg,
    ) !void {
        if (from_alloc.bits == to_alloc.bits) {
            return; // Skip moves with same source and dest
        }
        try self.moves.append(allocator, .{
            .pos_prio = .{ .pos = pos, .prio = @intFromEnum(prio) },
            .from_alloc = from_alloc,
            .to_alloc = to_alloc,
            .to_vreg = to_vreg,
        });
    }
};

//=============================================================================
// Edits - Edit list with priority
//=============================================================================

pub const Edits = struct {
    edits: std.ArrayListUnmanaged(struct { pos_prio: PosWithPrio, edit: Edit }),

    pub fn init() Edits {
        return .{ .edits = .{} };
    }

    pub fn deinit(self: *Edits, allocator: std.mem.Allocator) void {
        self.edits.deinit(allocator);
    }

    pub fn len(self: Edits) usize {
        return self.edits.items.len;
    }

    pub fn add(self: *Edits, allocator: std.mem.Allocator, pos_prio: PosWithPrio, from: Allocation, to: Allocation) !void {
        if (from.bits != to.bits) {
            try self.edits.append(allocator, .{
                .pos_prio = pos_prio,
                .edit = .{ .move = .{ .from = from, .to = to } },
            });
        }
    }

    pub fn sort(self: *Edits) void {
        std.mem.sort(
            @TypeOf(self.edits.items[0]),
            self.edits.items,
            {},
            struct {
                fn lessThan(_: void, a: anytype, b: anytype) bool {
                    return a.pos_prio.key() < b.pos_prio.key();
                }
            }.lessThan,
        );
    }
};

//=============================================================================
// SpillSlot structures
//=============================================================================

pub const SpillSetRanges = struct {
    /// Map from range key to spill set index.
    items: std.ArrayListUnmanaged(struct { key: LiveRangeKey, value: SpillSetIndex }),

    pub fn init() SpillSetRanges {
        return .{ .items = .{} };
    }

    pub fn deinit(self: *SpillSetRanges, allocator: std.mem.Allocator) void {
        self.items.deinit(allocator);
    }

    /// Check if any range overlaps the given key.
    pub fn containsKey(self: *const SpillSetRanges, key: LiveRangeKey) bool {
        for (self.items.items) |item| {
            if (item.key.order(key) == .eq) {
                return true;
            }
        }
        return false;
    }

    /// Insert a range. Returns the previous value if key overlapped.
    pub fn insert(self: *SpillSetRanges, allocator: std.mem.Allocator, key: LiveRangeKey, value: SpillSetIndex) !?SpillSetIndex {
        for (self.items.items) |*item| {
            if (item.key.order(key) == .eq) {
                const old = item.value;
                item.value = value;
                return old;
            }
        }
        try self.items.append(allocator, .{ .key = key, .value = value });
        return null;
    }
};

pub const SpillSlotData = struct {
    ranges: SpillSetRanges,
    slots: u32,
    alloc: Allocation,
};

pub const SpillSlotList = struct {
    slots: std.ArrayListUnmanaged(SpillSlotIndex),
    probe_start: usize,

    pub fn init() SpillSlotList {
        return .{ .slots = .{}, .probe_start = 0 };
    }

    pub fn deinit(self: *SpillSlotList, allocator: std.mem.Allocator) void {
        self.slots.deinit(allocator);
    }

    pub fn nextIndex(self: SpillSlotList, idx: usize) usize {
        if (idx == self.slots.items.len - 1) {
            return 0;
        } else {
            return idx + 1;
        }
    }
};

//=============================================================================
// Helper Functions
//=============================================================================

/// Create a u64 sort key from two u32 values.
pub fn u64Key(b: u32, a: u32) u64 {
    return @as(u64, a) | (@as(u64, b) << 32);
}

/// Create a u128 sort key from four u32 values.
pub fn u128Key(d: u32, c: u32, b: u32, a: u32) u128 {
    return @as(u128, a) | (@as(u128, b) << 32) | (@as(u128, c) << 64) | (@as(u128, d) << 96);
}

//=============================================================================
// Tests
//=============================================================================

test "CodeRange basic operations" {
    const p0 = ProgPoint.before(Inst.new(0));
    const p1 = ProgPoint.before(Inst.new(1));
    const p2 = ProgPoint.before(Inst.new(2));
    const p3 = ProgPoint.before(Inst.new(3));

    const r1 = CodeRange{ .from = p0, .to = p2 };
    const r2 = CodeRange{ .from = p1, .to = p3 };
    const r3 = CodeRange{ .from = p2, .to = p3 };

    // r1 and r2 overlap
    try std.testing.expect(r1.overlaps(r2));
    try std.testing.expect(r2.overlaps(r1));

    // r1 and r3 don't overlap (r1 ends at p2, r3 starts at p2)
    try std.testing.expect(!r1.overlaps(r3));
    try std.testing.expect(!r3.overlaps(r1));

    // Contains
    try std.testing.expect(r1.containsPoint(p0));
    try std.testing.expect(r1.containsPoint(p1));
    try std.testing.expect(!r1.containsPoint(p2)); // Exclusive end
}

test "LiveRangeIndex operations" {
    const idx = LiveRangeIndex.new(42);
    try std.testing.expectEqual(@as(usize, 42), idx.index());
    try std.testing.expect(idx.isValid());

    const invalid = LiveRangeIndex.invalid();
    try std.testing.expect(!invalid.isValid());
}

test "LiveBundle flags" {
    var bundle = LiveBundle.init();
    bundle.setCachedSpillWeightAndProps(1000, true, false, true, false);

    try std.testing.expect(bundle.cachedMinimal());
    try std.testing.expect(!bundle.cachedFixed());
    try std.testing.expect(bundle.cachedFixedDef());
    try std.testing.expect(!bundle.cachedStack());
    try std.testing.expectEqual(@as(u32, 1000), bundle.cachedSpillWeight());
}

test "LiveRangeKey ordering" {
    const k1 = LiveRangeKey{ .from = 0, .to = 10 };
    const k2 = LiveRangeKey{ .from = 5, .to = 15 };
    const k3 = LiveRangeKey{ .from = 20, .to = 30 };

    // k1 and k2 overlap
    try std.testing.expect(k1.order(k2) == .eq);
    try std.testing.expect(k2.order(k1) == .eq);

    // k1 < k3 (no overlap)
    try std.testing.expect(k1.order(k3) == .lt);
    try std.testing.expect(k3.order(k1) == .gt);
}

test "PrioQueue operations" {
    var queue = PrioQueue.init(std.testing.allocator);
    defer queue.deinit();

    try queue.insert(LiveBundleIndex.new(1), 100, PReg.invalid());
    try queue.insert(LiveBundleIndex.new(2), 200, PReg.invalid());
    try queue.insert(LiveBundleIndex.new(3), 50, PReg.invalid());

    // Should come out in priority order (highest first)
    const first = queue.pop().?;
    try std.testing.expectEqual(@as(usize, 2), first.bundle.index());

    const second = queue.pop().?;
    try std.testing.expectEqual(@as(usize, 1), second.bundle.index());

    const third = queue.pop().?;
    try std.testing.expectEqual(@as(usize, 3), third.bundle.index());

    try std.testing.expect(queue.isEmpty());
}

test "u64Key and u128Key" {
    const k64 = u64Key(0xABCD, 0x1234);
    try std.testing.expectEqual(@as(u64, 0xABCD00001234), k64);

    const k128 = u128Key(0xD, 0xC, 0xB, 0xA);
    try std.testing.expectEqual(@as(u128, 0xD0000000C0000000B0000000A), k128);
}
