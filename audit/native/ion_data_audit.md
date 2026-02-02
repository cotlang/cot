# Ion Data Structures Module Audit (Phase 6.10)

**Source**: `regalloc2/src/ion/data_structures.rs`
**Target**: `compiler/codegen/native/regalloc/ion_data.zig`
**Status**: ✅ Complete (~750 LOC, 6 tests)

---

## Index Types

| Rust Type | Zig Type | Notes |
|-----------|----------|-------|
| `LiveRangeIndex` | `LiveRangeIndex` | Index into live ranges array |
| `LiveBundleIndex` | `LiveBundleIndex` | Index into bundles array |
| `SpillSetIndex` | `SpillSetIndex` | Index into spill sets array |
| `UseIndex` | `UseIndex` | Index into uses array |
| `VRegIndex` | `VRegIndex` | Index into vregs array |
| `PRegIndex` | `PRegIndex` | Index into pregs array |
| `SpillSlotIndex` | `SpillSlotIndex` | Index into spill slots array |

All index types support:
- `new(usize)` - Create from index
- `index()` - Get index value
- `invalid()` - Sentinel value (u32::MAX)
- `isValid()` - Check if not invalid
- `eql(other)` - Equality comparison

---

## Core Types

### CodeRange
Range of program points [from, to):
```zig
pub const CodeRange = struct {
    from: ProgPoint,
    to: ProgPoint,

    pub fn isEmpty(self) bool
    pub fn contains(self, other: CodeRange) bool
    pub fn containsPoint(self, point: ProgPoint) bool
    pub fn overlaps(self, other: CodeRange) bool
    pub fn len(self) usize
    pub fn join(self, other: CodeRange) CodeRange
    pub fn order(self, other: CodeRange) Order  // For overlap comparison
};
```

### Use
A use of a vreg at a program point:
```zig
pub const Use = struct {
    operand: Operand,
    pos: ProgPoint,
    slot: u16,
    weight: u16,  // bfloat16-encoded spill weight
};
```

### LiveRange
Contiguous live range of a vreg:
```zig
pub const LiveRange = struct {
    range: CodeRange,
    vreg: VRegIndex,
    bundle: LiveBundleIndex,
    uses: ArrayList(Use),
    flags: LiveRangeFlag,
};
```

### LiveBundle
Bundle of ranges allocated together:
```zig
pub const LiveBundle = struct {
    ranges: ArrayList(LiveRangeListEntry),
    spillset: SpillSetIndex,
    allocation: Allocation,
    limit: ?u8,  // Register limit constraint
    // Cached properties (packed):
    cached_spill_weight: u32,
    cached_minimal: bool,
    cached_fixed: bool,
    cached_fixed_def: bool,
    cached_stack: bool,
};
```

### SpillSet
Bundles sharing a spill slot:
```zig
pub const SpillSet = struct {
    slot: SpillSlotIndex,
    hint: PReg,
    class: RegClass,
    spill_bundle: LiveBundleIndex,
    required: bool,
    splits: u8,
    range: CodeRange,
};
```

---

## Support Types

### LiveRangeKey
Key for overlap-based lookup:
```zig
pub const LiveRangeKey = struct {
    from: u32,
    to: u32,

    pub fn fromRange(range: CodeRange) LiveRangeKey
    pub fn order(self, other: LiveRangeKey) Order
    // .eq means overlapping, .lt/.gt means non-overlapping
};
```

### LiveRangeSet
Set of non-overlapping ranges:
```zig
pub const LiveRangeSet = struct {
    items: ArrayList(struct { key: LiveRangeKey, value: LiveRangeIndex }),

    pub fn insert(key, value) bool  // false if overlaps
    pub fn remove(key) bool
    pub fn containsKey(key) bool
};
```

### PrioQueue
Priority queue for bundle allocation:
```zig
pub const PrioQueue = struct {
    heap: PriorityQueue(PrioQueueEntry),

    pub fn insert(bundle, priority, hint) void
    pub fn pop() ?{bundle, hint}
    pub fn isEmpty() bool
};
```

---

## Move Insertion Types

### InsertMovePrio
Priority for ordering moves at same program point:
```zig
pub const InsertMovePrio = enum(u32) {
    in_edge_moves = 0,
    regular = 1,
    multi_fixed_reg_initial = 2,
    multi_fixed_reg_secondary = 3,
    reused_input = 4,
    out_edge_moves = 5,
};
```

### InsertedMove
A move to be inserted:
```zig
pub const InsertedMove = struct {
    pos_prio: PosWithPrio,
    from_alloc: Allocation,
    to_alloc: Allocation,
    to_vreg: VReg,
};
```

---

## Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `BUNDLE_MAX_SPILL_WEIGHT` | (1 << 28) - 1 | Maximum spill weight |
| `MINIMAL_FIXED_BUNDLE_SPILL_WEIGHT` | MAX | Fixed constraint weight |
| `MINIMAL_LIMITED_BUNDLE_SPILL_WEIGHT` | MAX - 1 | Limited constraint weight |
| `MINIMAL_BUNDLE_SPILL_WEIGHT` | LIMITED - 256 | Minimal bundle weight |
| `BUNDLE_MAX_NORMAL_SPILL_WEIGHT` | MINIMAL - 1 | Normal max weight |
| `MAX_SPLITS_PER_SPILLSET` | 2 | Split limit before minimal |

---

## SpillWeight Encoding

Weights stored as bfloat16-like format for compact storage in Use.weight:
```zig
pub const SpillWeight = struct {
    bits: u32,  // Actually stores f32 bits

    pub fn toBits(self) u16 {
        return @truncate(self.bits >> 15);  // Top 16 bits
    }

    pub fn fromBits(bits: u16) SpillWeight {
        return .{ .bits = @as(u32, bits) << 15 };
    }

    pub fn toInt(self) u32 {
        return @intFromFloat(@as(f32, @bitCast(self.bits)));
    }
};
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| CodeRange basic operations | ✅ | overlaps, contains, join |
| LiveRangeIndex operations | ✅ | new, index, isValid |
| LiveBundle flags | ✅ | Cached property packing |
| LiveRangeKey ordering | ✅ | Overlap-based comparison |
| PrioQueue operations | ✅ | insert, pop, priority order |
| u64Key and u128Key | ✅ | Sort key generation |

