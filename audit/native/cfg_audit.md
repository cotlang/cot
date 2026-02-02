# CFG Analysis Module Audit (Phase 6.6)

**Source**: `regalloc2/src/cfg.rs`, `src/postorder.rs`, `src/domtree.rs`
**Target**: `compiler/codegen/native/regalloc/cfg.zig`
**Status**: ✅ Complete (~420 LOC, 5 tests)

---

## Type Mapping

| Rust Type/Function | Zig Type/Function | Source File | Notes |
|--------------------|-------------------|-------------|-------|
| `CFGInfoCtx` | `CFGInfoCtx` | cfg.rs:14 | Scratch space for reuse |
| `CFGInfo` | `CFGInfo` | cfg.rs:21 | Combined CFG analysis |
| `CFGInfo.postorder` | `CFGInfo.postorder` | cfg.rs:23 | ✅ |
| `CFGInfo.domtree` | `CFGInfo.domtree` | cfg.rs:25 | ✅ |
| `CFGInfo.insn_block` | `CFGInfo.insn_block` | cfg.rs:27 | ✅ |
| `CFGInfo.block_entry` | `CFGInfo.block_entry` | cfg.rs:29 | ✅ |
| `CFGInfo.block_exit` | `CFGInfo.block_exit` | cfg.rs:31 | ✅ |
| `CFGInfo.approx_loop_depth` | `CFGInfo.approx_loop_depth` | cfg.rs:39 | ✅ |
| `CFGInfo::init()` | `CFGInfo.compute()` | cfg.rs:50 | ✅ |
| `CFGInfo::dominates()` | `CFGInfo.blockDominates()` | cfg.rs:152 | ✅ |
| `postorder::calculate()` | `calculatePostorder()` | postorder.rs:12 | ✅ DFS with explicit stack |
| `domtree::calculate()` | `calculateDomtree()` | domtree.rs:44 | ✅ Cooper-Harvey-Kennedy |
| `domtree::dominates()` | `dominates()` | domtree.rs:109 | ✅ |
| `merge_sets()` | `mergeSets()` | domtree.rs:22 | ✅ |

---

## CFGInfo Fields

| Field | Type | Description |
|-------|------|-------------|
| `postorder` | `ArrayList(Block)` | Blocks in postorder traversal |
| `domtree` | `ArrayList(Block)` | Immediate dominator for each block |
| `insn_block` | `ArrayList(Block)` | Block containing each instruction |
| `block_entry` | `ArrayList(ProgPoint)` | Entry point of each block |
| `block_exit` | `ArrayList(ProgPoint)` | Exit point of each block |
| `approx_loop_depth` | `ArrayList(u32)` | Approximate loop nesting depth |

---

## Algorithm: Postorder Traversal

Uses explicit stack-based DFS to avoid recursion:

```
calculatePostorder(func):
  visited = bitset of all blocks
  stack = [(entry_block, succ_index=0)]
  postorder = []

  while stack not empty:
    (block, succ_idx) = stack.top()
    succs = func.blockSuccs(block)

    if succ_idx < succs.len:
      next = succs[succ_idx]
      stack.top().succ_idx++
      if not visited[next]:
        visited.set(next)
        stack.push((next, 0))
    else:
      stack.pop()
      postorder.append(block)

  return postorder
```

Complexity: O(V + E)

---

## Algorithm: Dominator Tree

Uses Cooper-Harvey-Kennedy algorithm from "A Simple, Fast Dominance Algorithm" (Rice University TR-06-33870):

```
calculateDomtree(func, postorder):
  rpo = reverse(postorder)
  rpo_order = map block -> rpo index
  domtree[entry] = entry

  changed = true
  while changed:
    changed = false
    for block in rpo (except entry):
      new_idom = first processed predecessor
      for pred in block.preds:
        if domtree[pred] set:
          new_idom = mergeSets(new_idom, pred, domtree, rpo_order)
      if domtree[block] != new_idom:
        domtree[block] = new_idom
        changed = true

  return domtree
```

The `mergeSets` function finds the lowest common ancestor in the dominator tree using RPO order.

---

## Algorithm: Loop Depth

Approximate loop depth based on backedges (edges to blocks with lower RPO index):

```
computeLoopDepth(func, postorder):
  rpo_order = map block -> rpo index
  depth[*] = 0

  for block in rpo:
    for succ in block.succs:
      if rpo_order[succ] <= rpo_order[block]:
        // Backedge detected
        for b in rpo[rpo_order[succ]..rpo_order[block]+1]:
          depth[b]++

  return depth
```

Note: This is precise for reducible CFGs in RPO order, approximate otherwise.

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| postorder simple | ✅ | Linear block chain |
| postorder with branch | ✅ | Diamond CFG pattern |
| domtree simple chain | ✅ | Linear dominator chain |
| CFGInfoCtx init/deinit | ✅ | Scratch space lifecycle |
| CFGInfo init/deinit | ✅ | Full CFG analysis lifecycle |

