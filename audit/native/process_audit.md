# Allocation Loop Module Audit

**Source**: `regalloc2/src/ion/process.rs` (~1383 lines) + `regalloc2/src/ion/reg_traversal.rs` (~114 lines)
**Target**: `compiler/codegen/native/regalloc/process.zig`
**Status**: ✅ Complete (~1050 LOC, 6 tests)

---

## Source File Analysis

### Line Count by Section

| Section | Lines | Description |
|---------|-------|-------------|
| AllocRegResult enum | 32-38 (7) | Result of allocation attempt |
| process_bundles | 41-51 (11) | Main loop processing all bundles |
| try_to_allocate_bundle_to_reg | 53-212 (160) | Try to allocate to specific reg |
| evict_bundle | 214-244 (31) | Evict bundle from allocation |
| bundle_spill_weight | 246-248 (3) | Get bundle's spill weight |
| maximum_spill_weight_in_bundle_set | 250-263 (14) | Max weight in bundle set |
| recompute_bundle_properties | 265-367 (103) | Recompute prio/weight/flags |
| minimal_bundle | 369-371 (3) | Check if minimal |
| recompute_range_properties | 373-390 (18) | Recompute range properties |
| get_or_create_spill_bundle | 392-410 (19) | Get/create spill bundle |
| split_and_requeue_bundle | 412-791 (380) | Split and requeue |
| split_into_minimal_bundles | 823-970 (148) | Split to minimal bundles |
| process_bundle | 972-1348 (377) | Process single bundle |
| minimal_range_for_use | 1351-1383 (33) | Compute minimal range |
| **Total** | **~1307** | |

---

## Function-by-Function Audit

### 1. AllocRegResult (lines 32-38)

**Rust:**
```rust
pub enum AllocRegResult<'a> {
    Allocated(Allocation),
    Conflict(&'a [LiveBundleIndex], ProgPoint),
    ConflictWithFixed(u32, ProgPoint),
    ConflictHighCost,
}
```

**Zig Port Status:** ✅ Done - `AllocRegResult` tagged union

### 2. process_bundles (lines 41-51)

**Rust:**
```rust
pub fn process_bundles(&mut self) -> Result<(), RegAllocError> {
    while let Some((bundle, hint)) = self.ctx.allocation_queue.pop() {
        self.process_bundle(bundle, hint)?;
    }
    // Update stats
}
```

**Zig Port Status:** ✅ Done - `ProcessContext.processBundles()`

### 3. try_to_allocate_bundle_to_reg (lines 53-212)

**Rust:**
```rust
pub fn try_to_allocate_bundle_to_reg<'b>(
    &mut self,
    bundle: LiveBundleIndex,
    reg: PRegIndex,
    max_allowable_cost: Option<u32>,
    conflicts: &'b mut LiveBundleVec,
) -> AllocRegResult<'b>
```

**Zig Port Status:** ✅ Done - `ProcessContext.tryToAllocateBundleToReg()`

| Step | Status | Notes |
|------|--------|-------|
| BTree traversal | ✅ | Concurrent iteration over preg allocations |
| Skip to relevant ranges | ✅ | Optimization for large preg allocation maps |
| Overlap detection | ✅ | Using LiveRangeKey comparison |
| Conflict collection | ✅ | Track conflicting bundles |
| ConflictHighCost early return | ✅ | Cost threshold check |
| ConflictWithFixed | ✅ | Fixed allocation conflict |
| Success: add to preg BTree | ✅ | Insert all ranges |

### 4. evict_bundle (lines 214-244)

**Rust:**
```rust
pub fn evict_bundle(&mut self, bundle: LiveBundleIndex)
```

**Zig Port Status:** ✅ Done - `ProcessContext.evictBundle()`

### 5. bundle_spill_weight (lines 246-248)

**Rust:**
```rust
pub fn bundle_spill_weight(&self, bundle: LiveBundleIndex) -> u32
```

**Zig Port Status:** ✅ Done - `ProcessContext.bundleSpillWeight()`

### 6. maximum_spill_weight_in_bundle_set (lines 250-263)

**Rust:**
```rust
pub fn maximum_spill_weight_in_bundle_set(&self, bundles: &[LiveBundleIndex]) -> u32
```

**Zig Port Status:** ✅ Done - `ProcessContext.maximumSpillWeightInBundleSet()`

### 7. recompute_bundle_properties (lines 265-367)

Ported in both merge.zig and process.zig

**Zig Port Status:** ✅ Done - `ProcessContext.recomputeBundleProperties()`

### 8. minimal_bundle (lines 369-371)

**Rust:**
```rust
pub fn minimal_bundle(&self, bundle: LiveBundleIndex) -> bool
```

**Zig Port Status:** ✅ Done - `ProcessContext.minimalBundle()`

### 9. recompute_range_properties (lines 373-390)

**Rust:**
```rust
pub fn recompute_range_properties(&mut self, range: LiveRangeIndex)
```

**Zig Port Status:** ✅ Done - `ProcessContext.recomputeRangeProperties()`

### 10. get_or_create_spill_bundle (lines 392-410)

**Rust:**
```rust
pub fn get_or_create_spill_bundle(
    &mut self,
    bundle: LiveBundleIndex,
    create_if_absent: bool,
) -> Option<LiveBundleIndex>
```

**Zig Port Status:** ✅ Done - `ProcessContext.getOrCreateSpillBundle()`

### 11. split_and_requeue_bundle (lines 412-791) - **CRITICAL**

**Rust:**
```rust
pub fn split_and_requeue_bundle(
    &mut self,
    bundle: LiveBundleIndex,
    mut split_at: ProgPoint,
    hint: PReg,
    mut trim_ends_into_spill_bundle: bool,
)
```

**Zig Port Status:** ✅ Done - `ProcessContext.splitAndRequeueBundle()`

| Step | Status | Notes |
|------|--------|-------|
| Check max splits | ✅ | Fall back to minimal if exceeded |
| Adjust split point for start | ✅ | Peel off first use |
| Adjust split point mid-instruction | ✅ | Don't split in middle |
| Find LR split indices | ✅ | Which LRs go to which bundle |
| Advance for fixed constraints | ✅ | Don't create impossible splits |
| Split LR list | ✅ | Truncate and create new list |
| Split LR down middle | ✅ | Create new LR, split uses |
| Create new bundle | ✅ | Assign spillset, bundle pointers |
| Trim ends to spill bundle | ✅ | Move empty regions |
| Requeue both bundles | ✅ | With priority and hint |

### 12. split_into_minimal_bundles (lines 823-970) - **CRITICAL**

**Rust:**
```rust
pub fn split_into_minimal_bundles(&mut self, bundle: LiveBundleIndex, hint: PReg)
```

**Zig Port Status:** ✅ Done - `ProcessContext.splitIntoMinimalBundles()`

| Step | Status | Notes |
|------|--------|-------|
| Track removed LRs | ✅ | scratch_removed_lrs/vregs |
| Create new LRs per use | ✅ | Except Any-constrained |
| Create bundle per LR | ✅ | Single-LR bundles |
| Create spill range | ✅ | Cover whole original range |
| Update vreg LR lists | ✅ | Remove old, add new |
| Recompute and enqueue | ✅ | All new bundles |

### 13. process_bundle (lines 972-1348) - **CRITICAL**

**Rust:**
```rust
pub fn process_bundle(
    &mut self,
    bundle: LiveBundleIndex,
    hint: PReg,
) -> Result<(), RegAllocError>
```

**Zig Port Status:** ✅ Done - `ProcessContext.processBundle()`

| Step | Status | Notes |
|------|--------|-------|
| Get hint from queue or spillset | ✅ | |
| Compute requirement | ✅ | Split if conflict |
| Handle Any requirement | ✅ | Move to spill bundle |
| Allocation loop | ✅ | Try all pregs |
| Track lowest cost evict | ✅ | For eviction decision |
| Track lowest cost split | ✅ | For split decision |
| RegTraversalIter | ✅ | Iterate pregs in order |
| Handle Allocated | ✅ | Success |
| Handle Conflict | ✅ | Track cost and point |
| Handle ConflictWithFixed | ✅ | Track for split |
| Handle ConflictHighCost | ✅ | Skip |
| Check TooManyLiveRegs | ✅ | Error condition |
| Decide evict vs split | ✅ | Based on costs |
| Execute eviction | ✅ | Evict and retry |

### 14. minimal_range_for_use (lines 1351-1383)

**Zig Port Status:** ✅ Done - `minimalRangeForUse()` function

---

## Dependencies

### Required Data Structures

| Type | Source | Status |
|------|--------|--------|
| LiveBundleVec | data_structures.rs | ✅ Using ArrayListUnmanaged |
| RegTraversalIter | reg_traversal.rs | ✅ Ported in process.zig |
| scratch_conflicts | Env | ✅ In ProcessContext |
| scratch_bundle | Env | ✅ In ProcessContext |
| scratch_removed_lrs | Env | ✅ In ProcessContext |
| scratch_removed_lrs_vregs | Env | ✅ In ProcessContext |
| conflict_set | Env | ✅ In ProcessContext |
| spilled_bundles | Env | ✅ In ProcessContext |

### Required from Previous Modules

| Module | What's Needed | Status |
|--------|---------------|--------|
| merge.zig | recomputeBundleProperties, minimalRangeForUse | ✅ |
| ion_data.zig | All index types, LiveRange, LiveBundle | ✅ |
| liveness.zig | SpillWeight, spillWeightFromConstraint | ✅ |
| operand.zig | Allocation, ProgPoint | ✅ |

---

## Key Algorithms

### Bundle Processing Loop

```
process_bundles():
  while allocation_queue not empty:
    (bundle, hint) = queue.pop()
    process_bundle(bundle, hint)
  update stats
```

### Try Allocate Algorithm

```
try_to_allocate_bundle_to_reg(bundle, reg, max_cost):
  for each bundle range:
    traverse preg allocations concurrently
    if overlap:
      if fixed: return ConflictWithFixed
      add to conflicts
      if cost > max_cost: return ConflictHighCost
  if no conflicts:
    assign bundle to reg
    add all ranges to preg's BTree
    return Allocated
  return Conflict(conflicts, first_point)
```

### Process Bundle Algorithm

```
process_bundle(bundle, hint):
  req = compute_requirement(bundle)
  if req conflict: split_and_requeue; return

  if req == Any and spill_bundle exists:
    move to spill bundle; return

  loop:
    for each preg in RegTraversalIter:
      result = try_to_allocate_bundle_to_reg(bundle, preg)
      if Allocated: return OK
      if Conflict: track lowest cost evict/split
      if ConflictWithFixed: track for split
      if ConflictHighCost: continue

    if minimal and can't allocate: error TooManyLiveRegs

    if should_split:
      split_and_requeue_bundle(bundle, split_point)
      return OK
    else:
      evict all in lowest_cost_conflict_set
      continue loop
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| AllocRegResult variants | ✅ | All cases (allocated, conflict, conflict_with_fixed, conflict_high_cost) |
| Cursor initialization and iteration | ✅ | Offset-based register iteration |
| RegTraversalIter with fixed register | ✅ | Returns fixed register only |
| RegTraversalIter with hint | ✅ | Hint returned first, skipped in subsequent iteration |
| RegTraversalIter with limit | ✅ | All registers under limit |
| minimalRangeForUse | ✅ | Correct range for early/late def/use |

---

## Implementation Plan

### Phase 1: Core Types
- [x] AllocRegResult enum
- [x] ProcessContext struct (holds all state)
- [x] RegTraversalIter (from reg_traversal.rs)
- [x] Cursor helper struct

### Phase 2: Simple Functions
- [x] bundleSpillWeight
- [x] maximumSpillWeightInBundleSet
- [x] minimalBundle
- [x] recomputeRangeProperties
- [x] getOrCreateSpillBundle

### Phase 3: Allocation
- [x] tryToAllocateBundleToReg
- [x] evictBundle

### Phase 4: Splitting
- [x] splitAndRequeueBundle
- [x] splitIntoMinimalBundles

### Phase 5: Main Loop
- [x] processBundle
- [x] processBundles

### Phase 6: Tests
- [x] Unit tests: 6 tests covering AllocRegResult, Cursor, RegTraversalIter, minimalRangeForUse

---

## Notes

1. **RegTraversalIter**: Need to port from reg_traversal.rs - iterates pregs in preferred order with hint and limit support.

2. **BTree Traversal**: Rust uses BTreeMap range queries. In Zig we'll use the LiveRangeSet which wraps a sorted structure.

3. **SmallVec**: Rust uses SmallVec for stack-allocated small vectors. In Zig we can use BoundedArray or just ArrayList.

4. **Scratch Memory**: The Rust code reuses scratch vectors between calls. We'll do the same for efficiency.

5. **Complexity**: `split_and_requeue_bundle` is very complex (~380 lines) and handles many edge cases. Careful porting required.
