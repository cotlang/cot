# Move Insertion Module Audit

**Source**: `regalloc2/src/ion/moves.rs` (~1010 lines) + `redundant_moves.rs` (~130 lines)
**Target**: `compiler/codegen/native/regalloc/ion_moves.zig`
**Status**: 🔄 In Progress (~450 LOC, 4 tests)

### Completed
- RedundantMoveEliminator with process_move, clear, clear_alloc
- PrevBuffer for tracking previous live range
- MoveContext with block boundary checks and allocation accessors
- chooseMoveLocation for edge move placement
- allocateSpillslot for scratch use

### Remaining
- apply_allocations_and_insert_moves (~700 lines)
- resolve_inserted_moves (~230 lines)

---

## Source File Analysis

### Line Count by Section (moves.rs)

| Section | Lines | Description |
|---------|-------|-------------|
| is_start_of_block / is_end_of_block | 37-44 (8) | Check block boundaries |
| get_alloc / set_alloc | 46-56 (11) | Access output allocations |
| get_alloc_for_range | 58-74 (17) | Get allocation for live range |
| PrevBuffer struct | 92-163 (72) | Buffer for previous LR tracking |
| choose_move_location | 172-219 (48) | Choose where to insert edge moves |
| InterBlockDest / BlockparamDest | 221-265 (45) | Helper structs for move destinations |
| apply_allocations_and_insert_moves | 76-776 (~700) | Main allocation/move function |
| resolve_inserted_moves | 778-1008 (~230) | Resolve parallel moves to sequential |
| **Total** | **~1010** | |

### Line Count by Section (redundant_moves.rs)

| Section | Lines | Description |
|---------|-------|-------------|
| RedundantMoveState enum | 6-11 (6) | State for tracking moves |
| RedundantMoveEliminator struct | 12-16 (5) | Main struct with hashmaps |
| RedundantMoveAction | 17-20 (4) | Result of move processing |
| process_move | 22-103 (82) | Process a move for elision |
| clear / clear_alloc | 105-128 (24) | Clear state |
| **Total** | **~130** | |

---

## Function-by-Function Audit

### 1. is_start_of_block / is_end_of_block (lines 37-44)

**Rust:**
```rust
pub fn is_start_of_block(&self, pos: ProgPoint) -> bool
pub fn is_end_of_block(&self, pos: ProgPoint) -> bool
```

**Zig Port Status:** ⏳ TODO

### 2. get_alloc / set_alloc (lines 46-56)

**Rust:**
```rust
pub fn get_alloc(&self, inst: Inst, slot: usize) -> Allocation
pub fn set_alloc(&mut self, inst: Inst, slot: usize, alloc: Allocation)
```

**Zig Port Status:** ⏳ TODO

### 3. get_alloc_for_range (lines 58-74)

**Rust:**
```rust
pub fn get_alloc_for_range(&self, range: LiveRangeIndex) -> Allocation
```

**Zig Port Status:** ⏳ TODO

### 4. apply_allocations_and_insert_moves (lines 76-776)

**Rust:**
```rust
pub fn apply_allocations_and_insert_moves(&mut self) -> InsertedMoves
```

**Zig Port Status:** ⏳ TODO

| Step | Status | Notes |
|------|--------|-------|
| Sort vreg range lists | ⏳ | Sort by range.from |
| PrevBuffer tracking | ⏳ | Track previous LR for intra-block moves |
| choose_move_location | ⏳ | Edge move placement |
| Inter-block source collection | ⏳ | Collect sources for inter-block moves |
| Blockparam out processing | ⏳ | Process outgoing blockparams |
| Blockparam in processing | ⏳ | Process incoming blockparams |
| Intra-block moves | ⏳ | Moves between adjacent LRs |
| Apply allocations to uses | ⏳ | Set allocation for each use |
| Debug labels | ⏳ | Optional debug info |
| Inter-block moves | ⏳ | Insert inter-block moves |
| Blockparam moves | ⏳ | Insert blockparam moves |
| Multi-fixed-reg fixups | ⏳ | Handle multi-fixed constraints |
| Reuse-input handling | ⏳ | Handle reuse constraints |

### 5. resolve_inserted_moves (lines 778-1008)

**Rust:**
```rust
pub fn resolve_inserted_moves(&mut self, inserted_moves: InsertedMoves) -> Edits
```

**Zig Port Status:** ⏳ TODO

| Step | Status | Notes |
|------|--------|-------|
| Sort moves by pos_prio | ⏳ | Group moves at same position |
| RedundantMoveEliminator | ⏳ | Track and elide redundant moves |
| Process side effects | ⏳ | Clear state on defs/clobbers |
| Group by RegClass | ⏳ | Separate Int/Float/Vector |
| ParallelMoves | ⏳ | Already ported in moves.zig |
| MoveAndScratchResolver | ⏳ | Find scratch registers |
| Extra spillslots | ⏳ | Allocate if needed |
| Output Edits | ⏳ | Final edit list |

---

## Dependencies

### Required Data Structures

| Type | Source | Status |
|------|--------|--------|
| InsertMovePrio | data_structures.rs | ⏳ TODO |
| InsertedMove | data_structures.rs | ⏳ TODO |
| InsertedMoves | data_structures.rs | ⏳ TODO |
| PosWithPrio | data_structures.rs | ⏳ TODO |
| BlockparamIn | data_structures.rs | ⏳ TODO |
| BlockparamOut | data_structures.rs | ⏳ TODO |
| FixedRegFixup | data_structures.rs | ⏳ TODO |
| FixedRegFixupLevel | data_structures.rs | ⏳ TODO |
| Edits | data_structures.rs | ⏳ TODO |
| RedundantMoveEliminator | redundant_moves.rs | ⏳ TODO |
| RedundantMoveState | redundant_moves.rs | ⏳ TODO |
| RedundantMoveAction | redundant_moves.rs | ⏳ TODO |
| ParallelMoves | moves.zig | ✅ Already ported |
| MoveAndScratchResolver | moves.zig | ✅ Already ported |

### Required from Previous Modules

| Module | What's Needed | Status |
|--------|---------------|--------|
| process.zig | RegTraversalIter | ✅ |
| ion_data.zig | LiveRangeKey, LiveRange, LiveBundle | ✅ |
| cfg.zig | CFGInfo, block_entry, block_exit | ✅ |
| output.zig | Output, allocs, inst_alloc_offsets | ✅ |

---

## Key Algorithms

### Apply Allocations and Insert Moves

```
apply_allocations_and_insert_moves():
  for each vreg:
    sort ranges by from
    for each range:
      get allocation

      // Intra-block moves
      if prev range abuts in same block:
        insert move from prev_alloc to alloc

      // Inter-block source collection
      for each block end in range:
        record (block -> alloc) as source
        process blockparam_outs

      // Inter-block dest collection
      for each block start in range:
        for each pred not in range:
          record (pred, block, alloc) as dest
        process blockparam_ins

      // Apply allocations
      for each use in range:
        set_alloc(inst, slot, alloc)

    // Insert inter-block moves
    for each (from, to, alloc) in dests:
      src_alloc = sources[from]
      insert move at edge

    // Insert blockparam moves
    for each blockparam dest:
      src_alloc = sources[key]
      insert move at edge

  // Handle multi-fixed-reg and reuse-input
```

### Resolve Inserted Moves

```
resolve_inserted_moves(inserted_moves):
  sort by pos_prio

  for each group at same pos_prio:
    update redundant_move_eliminator for side effects

    group moves by RegClass
    for each class:
      parallel_moves = ParallelMoves.new()
      add all moves
      resolve to sequential

      find scratch registers (RegTraversalIter)
      resolve scratch needs

      for each resolved move:
        if not redundant:
          add to edits

  sort edits by progpoint
  return edits
```

---

## Implementation Plan

### Phase 1: Data Structures (in ion_data.zig)
- [x] InsertMovePrio enum
- [x] PosWithPrio struct
- [x] InsertedMove struct
- [x] InsertedMoves struct
- [x] BlockparamIn struct
- [x] BlockparamOut struct
- [x] FixedRegFixup struct
- [x] FixedRegFixupLevel enum
- [x] Edits struct

### Phase 2: RedundantMoveEliminator
- [x] RedundantMoveState enum
- [x] RedundantMoveAction struct
- [x] RedundantMoveEliminator struct
- [x] process_move
- [x] clear / clear_alloc

### Phase 3: MoveContext
- [x] MoveContext struct with all state
- [x] is_start_of_block / is_end_of_block
- [x] get_alloc / set_alloc
- [x] get_alloc_for_range
- [x] chooseMoveLocation
- [x] allocateSpillslot

### Phase 4: Main Functions
- [ ] apply_allocations_and_insert_moves
- [ ] resolve_inserted_moves

### Phase 5: Tests
- [x] Unit tests (4 tests)

---

## Notes

1. **PrevBuffer**: Complex buffering logic for tracking previous live range to determine intra-block move sources.

2. **choose_move_location**: Critical edge handling - moves go at predecessor tail if multiple in-edges, otherwise at successor head.

3. **Half-moves**: The pattern of collecting sources and destinations separately, then matching them up at the end.

4. **RedundantMoveEliminator**: Tracks copy chains to elide moves that don't actually change values.

5. **Parallel to Sequential**: Uses existing ParallelMoves from moves.zig to convert concurrent moves to sequential.

6. **Scratch Registers**: May need to allocate extra spillslots if no free register is available for breaking cycles.

