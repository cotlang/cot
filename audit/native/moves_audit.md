# Parallel Moves Module Audit (Phase 6.9)

**Source**: `regalloc2/src/moves.rs`
**Target**: `compiler/codegen/native/regalloc/moves.zig`
**Status**: ✅ Complete (~450 LOC, 7 tests)

---

## Type Mapping

| Rust Type | Zig Type | Rust Location | Notes |
|-----------|----------|---------------|-------|
| `MoveVec<T>` | `MoveVec(T)` | moves.rs:12 | ArrayList of (from, to, data) tuples |
| `Move` tuple | `Move(T)` | moves.rs:12 | Struct with from, to, data fields |
| `MoveVecWithScratch<T>` | `MoveVecWithScratch(T)` | moves.rs:18 | Tagged union: no_scratch/scratch |
| `MoveVecWithScratch::NoScratch` | `.no_scratch` | moves.rs:20 | ✅ |
| `MoveVecWithScratch::Scratch` | `.scratch` | moves.rs:22 | ✅ |
| `MoveVecWithScratch::with_scratch()` | `.withScratch()` | moves.rs:240 | ✅ |
| `MoveVecWithScratch::without_scratch()` | `.withoutScratch()` | moves.rs:266 | ✅ |
| `MoveVecWithScratch::needs_scratch()` | `.needsScratch()` | moves.rs:274 | ✅ |
| `ParallelMoves<T>` | `ParallelMoves(T)` | moves.rs:31 | Main resolver struct |
| `ParallelMoves::new()` | `.init()` | moves.rs:36 | ✅ |
| `ParallelMoves::add()` | `.add()` | moves.rs:42 | ✅ |
| `ParallelMoves::resolve()` | `.resolve()` | moves.rs:74 | ✅ |
| `MoveAndScratchResolver` | `MoveAndScratchResolver(T)` | moves.rs:304 | Final resolution with scratch |
| `MoveAndScratchResolver::compute()` | `.compute()` | moves.rs:330 | ✅ |

---

## The Parallel Move Problem

When multiple moves must happen "simultaneously", their order matters:

```
Example: swap r0 and r1
  Parallel semantics: r0, r1 := r1, r0

  Wrong sequential:     Correct sequential:
    r0 := r1             tmp := r0
    r1 := r0  // WRONG!  r0 := r1
                         r1 := tmp
```

The challenge is finding a correct sequential order, using a scratch register to break cycles.

---

## Algorithm: Parallel Move Resolution

### Phase 1: Normalize
```
1. Sort moves by (dst, src) for efficient lookup
2. Remove duplicates
3. Remove self-moves (src == dst)
4. If no sources overlap destinations, return as-is
```

### Phase 2: Build Dependency Graph
```
must_come_before[i] = set of moves that must complete before move[i]

For each pair (i, j):
  if move[i].src == move[j].dst:
    must_come_before[j].add(i)  // move[i] reads what move[j] writes
```

### Phase 3: Topological Sort with Cycle Detection
```
DFS from each unvisited node:
  - Mark as "in_progress"
  - Visit all dependencies
  - If dependency is "in_progress", we found a cycle
  - Mark as "done", add to output (postorder)

Reverse the output for correct order.
```

### Phase 4: Break Cycles with Scratch
When a cycle is detected:
```
Cycle: A -> B -> C -> A

Emit:
  scratch := A.src
  A.dst := A.src    (but A.src is clobbered, use scratch later)
  B.dst := B.src
  C.dst := scratch  (originally A.src)
```

---

## Key Property

**Each destination has exactly one writer.**

This means cycles are simple rings, not complex SCCs. A single scratch register can break any cycle.

---

## Stack-to-Stack Moves

No architecture supports direct memory-to-memory moves. Resolution:

```
If both src and dst are stack slots:
  1. Find a free register
  2. Emit: reg := src
  3. Emit: dst := reg

If no free register:
  1. Borrow a register (save to stack first)
  2. Use borrowed register for the move
  3. Restore borrowed register
```

---

## MoveAndScratchResolver

Final resolution that handles scratch register allocation:

```zig
const resolver = MoveAndScratchResolver(T){
    .find_free_reg = findFreeReg,     // Returns free register if available
    .get_stackslot = getStackslot,    // Allocate temp stack slot
    .is_stack_alloc = isStackAlloc,   // Check if allocation is stack
    .borrowed_scratch_reg = victim,   // Preferred register to borrow
};

const result = resolver.compute(moves);
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| Simple moves (no conflict) | ✅ | A→B, C→D |
| Chain (ordered correctly) | ✅ | A→B, B→C (emits C→B first) |
| Swap (needs scratch) | ✅ | A→B, B→A |
| Triangle cycle | ✅ | A→B, B→C, C→A |
| Self-move removal | ✅ | A→A removed |
| Duplicate removal | ✅ | Same move twice |
| Stack-to-stack | ✅ | Via register intermediate |

