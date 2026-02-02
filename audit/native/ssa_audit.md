# SSA Validation Module Audit (Phase 6.7)

**Source**: `regalloc2/src/ssa.rs`
**Target**: `compiler/codegen/native/regalloc/ssa.zig`
**Status**: ✅ Complete (~150 LOC, 1 test)

---

## Function Mapping

| Rust Function | Zig Function | Notes |
|---------------|--------------|-------|
| `validate_ssa()` | `validateSsa()` | Main validation function |

---

## Validation Checks

### 1. Single Definition
Each VReg must be defined exactly once:
- Either by a block parameter
- Or by an instruction def operand

### 2. Dominance
Every use must be dominated by its definition:
- If in same block: def must come before use (earlier instruction, or same instruction with Early < Late)
- If in different blocks: def's block must dominate use's block

### 3. Block Structure

| Check | Description |
|-------|-------------|
| Non-empty | Every block must have at least one instruction |
| Terminated | Last instruction must be branch or ret |
| No mid-block terminators | Branch/ret only at end of block |
| Branch arg match | Branch arguments must match successor block params |

### 4. Entry Block
Entry block must have no block parameters.

---

## Error Types

| Error | Description |
|-------|-------------|
| `VRegNotSequential` | VReg index >= num_vregs |
| `MultipleDefs` | Same VReg defined twice |
| `UseNotDominated` | Use not dominated by definition |
| `EmptyBlock` | Block has no instructions |
| `BlockNotTerminated` | Last instruction not branch/ret |
| `BranchArgMismatch` | Branch args don't match successor params |
| `TerminatorInMiddle` | Branch/ret not at block end |
| `EntryHasBlockParams` | Entry block has parameters |

---

## Algorithm

```
validateSsa(func, cfginfo):
  // Check entry block
  if func.blockParams(entry) not empty:
    error EntryHasBlockParams

  // Track definitions
  def_block = map vreg -> block
  def_inst = map vreg -> inst

  for block in blocks:
    insns = func.blockInsns(block)
    if insns.empty:
      error EmptyBlock

    // Register block params as defs
    for vreg in func.blockParams(block):
      if def_block[vreg] set:
        error MultipleDefs
      def_block[vreg] = block
      def_inst[vreg] = insns.first

    for inst in insns:
      // Check uses are dominated
      for operand in func.instOperands(inst):
        if operand.kind == Use:
          vreg = operand.vreg
          if not isDominated(def_block[vreg], def_inst[vreg], block, inst):
            error UseNotDominated

      // Register defs
      for operand in func.instOperands(inst):
        if operand.kind == Def:
          if def_block[vreg] set:
            error MultipleDefs
          def_block[vreg] = block
          def_inst[vreg] = inst

      // Check terminator position
      if func.isBranch(inst) or func.isRet(inst):
        if inst != insns.last:
          error TerminatorInMiddle

    // Check block termination
    if not func.isBranch(insns.last) and not func.isRet(insns.last):
      error BlockNotTerminated

    // Check branch args
    if func.isBranch(insns.last):
      for succ in func.blockSuccs(block):
        args = func.branchBlockparams(block, insns.last, succ)
        params = func.blockParams(succ)
        if args.len != params.len:
          error BranchArgMismatch
```

---

## Dominance Check

```
isDominated(def_block, def_inst, use_block, use_inst):
  if def_block == use_block:
    // Same block: check instruction order
    if def_inst > use_inst:
      return false
    if def_inst == use_inst:
      // Same instruction: check Early/Late position
      return def_pos <= use_pos
    return true
  else:
    // Different blocks: check block dominance
    return cfginfo.dominates(def_block, use_block)
```

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| SSA validation basic | ✅ | Valid SSA program |

