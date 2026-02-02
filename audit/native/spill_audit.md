# Spillslot Allocation Module Audit

**Source**: `regalloc2/src/ion/spill.rs` (~174 lines)
**Target**: `compiler/codegen/native/regalloc/spill.zig`
**Status**: ✅ Complete (~577 LOC, 4 tests)

---

## Source File Analysis

### Line Count by Section

| Section | Lines | Description |
|---------|-------|-------------|
| try_allocating_regs_for_spilled_bundles | 22-68 (47) | Try register allocation for spilled bundles |
| spillslot_can_fit_spillset | 70-81 (12) | Check spillslot can fit spillset |
| allocate_spillset_to_spillslot | 83-96 (14) | Assign spillset to spillslot |
| allocate_spillslots | 98-157 (60) | Main spillslot allocation loop |
| allocate_spillslot | 159-172 (14) | Allocate actual stack slot |
| **Total** | **~147** | |

---

## Function-by-Function Audit

### 1. try_allocating_regs_for_spilled_bundles (lines 22-68)

**Rust:**
```rust
pub fn try_allocating_regs_for_spilled_bundles(&mut self) {
    for i in 0..self.ctx.spilled_bundles.len() {
        let bundle = self.ctx.spilled_bundles[i];
        // Sort ranges, try RegTraversalIter, mark spillset required if fail
    }
}
```

**Zig Port Status:** ✅ Done - `SpillContext.tryAllocatingRegsForSpilledBundles()`

| Step | Status | Notes |
|------|--------|-------|
| Iterate spilled bundles | ✅ | Don't borrow self |
| Skip empty bundles | ✅ | Check ranges.len == 0 |
| Get class and hint | ✅ | From spillset |
| Sort ranges by from | ✅ | For proper allocation |
| Try registers via RegTraversalIter | ✅ | With limit |
| Mark spillset required on failure | ✅ | For spillslot allocation |

### 2. spillslot_can_fit_spillset (lines 70-81)

**Rust:**
```rust
pub fn spillslot_can_fit_spillset(
    &mut self,
    spillslot: SpillSlotIndex,
    spillset: SpillSetIndex,
) -> bool
```

**Zig Port Status:** ✅ Done - `SpillContext.spillslotCanFitSpillset()`

### 3. allocate_spillset_to_spillslot (lines 83-96)

**Rust:**
```rust
pub fn allocate_spillset_to_spillslot(
    &mut self,
    spillset: SpillSetIndex,
    spillslot: SpillSlotIndex,
)
```

**Zig Port Status:** ✅ Done - `SpillContext.allocateSpillsetToSpillslot()`

### 4. allocate_spillslots (lines 98-157)

**Rust:**
```rust
pub fn allocate_spillslots(&mut self)
```

**Zig Port Status:** ✅ Done - `SpillContext.allocateSpillslots()`

| Step | Status | Notes |
|------|--------|-------|
| Iterate required spillsets | ✅ | Skip non-required |
| Probe existing slots | ✅ | Up to MAX_ATTEMPTS |
| Check spillslot fit | ✅ | Via spillslotCanFitSpillset |
| Create new slot on failure | ✅ | With proper size |
| Update probe_start | ✅ | Circular probing |
| Assign actual allocations | ✅ | Via allocateSpillslot |

### 5. allocate_spillslot (lines 159-172)

**Rust:**
```rust
pub fn allocate_spillslot(&mut self, size: u32) -> Allocation
```

**Zig Port Status:** ✅ Done - `SpillContext.allocateSpillslot()`

| Step | Status | Notes |
|------|--------|-------|
| Get current offset | ✅ | From num_spillslots |
| Align to size | ✅ | Power-of-two alignment |
| Handle multi_spillslot naming | ✅ | First vs last slot naming |
| Update num_spillslots | ✅ | |
| Return stack Allocation | ✅ | |

---

## Dependencies

### Required Data Structures

| Type | Source | Status |
|------|--------|--------|
| SpillSlotData | ion_data.zig | ✅ |
| SpillSlotList | ion_data.zig | ✅ |
| SpillSetRanges | ion_data.zig | ✅ (added containsKey, insert) |
| LiveRangeKey | ion_data.zig | ✅ |
| RegTraversalIter | process.zig | ✅ |
| AllocRegResult | process.zig | ✅ |

### Required from Previous Modules

| Module | What's Needed | Status |
|--------|---------------|--------|
| process.zig | RegTraversalIter, tryToAllocateBundleToReg | ✅ |
| ion_data.zig | SpillSlotData, SpillSlotList, SpillSetIndex | ✅ |
| index.zig | SpillSlot | ✅ |
| operand.zig | Allocation | ✅ |

---

## Key Algorithms

### Spilled Bundle Register Allocation

```
try_allocating_regs_for_spilled_bundles():
  for each spilled bundle:
    if empty: skip
    sort ranges by from
    for each preg via RegTraversalIter:
      if try_to_allocate succeeds:
        mark success, break
    if not success:
      mark spillset as required
```

### Spillslot Allocation

```
allocate_spillslots():
  for each spillset:
    if not required: skip
    probe up to MAX_ATTEMPTS existing slots:
      if slot can fit spillset:
        allocate to slot, break
    if no fit:
      create new spillslot
      allocate to new slot

  for each spillslot:
    assign actual stack allocation
```

### Stack Slot Allocation

```
allocate_spillslot(size):
  offset = num_spillslots
  offset = align_up(offset, size)
  slot = offset (or offset + size - 1 for last-slot naming)
  num_spillslots = offset + size
  return stack(slot)
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| SpillSetRanges containsKey | ✅ | Overlap detection |
| SpillSetRanges insert | ✅ | Adding ranges |
| allocateSpillslot alignment | ✅ | Power-of-two alignment |
| allocateSpillslot multi_spillslot | ✅ | Last-slot naming convention |
| SpillSlotList nextIndex circular | ✅ | Circular index wrapping |

---

## Implementation Plan

### Phase 1: Data Structure Updates
- [x] Add containsKey to SpillSetRanges
- [x] Add insert to SpillSetRanges

### Phase 2: SpillContext
- [x] SpillContext struct with state references
- [x] spillslotCanFitSpillset
- [x] allocateSpillsetToSpillslot
- [x] allocateSpillslot

### Phase 3: Main Functions
- [x] tryAllocatingRegsForSpilledBundles
- [x] allocateSpillslots

### Phase 4: Tests
- [x] Unit tests

---

## Notes

1. **Circular Probing**: The spillslot allocation uses circular probing through existing slots before creating new ones.

2. **MAX_ATTEMPTS**: Limited to 10 probes to avoid O(n²) behavior with many spillsets.

3. **Multi-slot Naming**: Some ABIs name multi-word spillslots by the last slot rather than the first.

4. **Alignment**: Stack slots are aligned to their size (power of two).
