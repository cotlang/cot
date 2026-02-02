# Register Allocator Port Audit

**Source**: regalloc2 (https://github.com/bytecodealliance/regalloc2)
**Target**: `compiler/codegen/native/regalloc/`
**Source LOC**: 12,631 (Rust)
**Target LOC**: ~7,300 estimated (Zig)

---

## Port Status Overview

| Phase | Module | Source File(s) | Target File | Status | Tests |
|-------|--------|----------------|-------------|--------|-------|
| 6.1 | Core Types | lib.rs, index.rs | index.zig | ✅ Done | 8/8 |
| 6.2 | Operands | lib.rs:600-1180 | operand.zig | ✅ Done | 7/7 |
| 6.3 | Function Interface | lib.rs:1180-1330 | func.zig | ✅ Done | 2/2 |
| 6.4 | Machine Environment | lib.rs:1500-1550 | env.zig | ✅ Done | 4/4 |
| 6.5 | Output | lib.rs:1546-1650 | output.zig | ✅ Done | 6/6 |
| 6.6 | CFG Analysis | cfg.rs, postorder.rs, domtree.rs | cfg.zig | ⏳ TODO | - |
| 6.7 | SSA Validation | ssa.rs | ssa.zig | ⏳ TODO | - |
| 6.8 | Index Set | indexset.rs | indexset.zig | ⏳ TODO | - |
| 6.9 | Parallel Moves | moves.rs | moves.zig | ⏳ TODO | - |
| 6.10 | Ion Data Structures | ion/data_structures.rs | ion/data.zig | ⏳ TODO | - |
| 6.11 | Liveness Analysis | ion/liveranges.rs | ion/liveness.zig | ⏳ TODO | - |
| 6.12 | Liverange Building | ion/liveranges.rs | ion/liveranges.zig | ⏳ TODO | - |
| 6.13 | Bundle Merging | ion/merge.rs | ion/merge.zig | ⏳ TODO | - |
| 6.14 | Allocation Loop | ion/process.rs | ion/process.zig | ⏳ TODO | - |
| 6.15 | Spill Allocation | ion/spill.rs | ion/spill.zig | ⏳ TODO | - |
| 6.16 | Move Insertion | ion/moves.rs | ion/moves.zig | ⏳ TODO | - |
| 6.17 | Requirements | ion/requirement.rs | ion/requirement.zig | ⏳ TODO | - |
| 6.18 | Reg Traversal | ion/reg_traversal.rs | ion/traversal.zig | ⏳ TODO | - |
| 6.19 | Public API | lib.rs | lib.zig | ⏳ TODO | - |

---

## Phase 6.1: Core Types (index.zig)

**Source**: `src/lib.rs` lines 73-600, `src/index.rs`
**Target**: `compiler/codegen/native/regalloc/index.zig`
**Status**: ✅ Complete (727 LOC, 8 tests)

### Type Mapping

| Rust Type | Zig Type | Rust Location | Notes |
|-----------|----------|---------------|-------|
| `RegClass` | `RegClass` | lib.rs:86-92 | enum(u2) { int, float, vector } |
| `PReg` | `PReg` | lib.rs:108-180 | Bit-packed: class:2 + hw_enc:6 = 8 bits |
| `PReg::MAX_BITS` | `PReg.MAX_BITS` | lib.rs:115 | 6 |
| `PReg::MAX` | `PReg.MAX` | lib.rs:116 | 63 |
| `PReg::NUM_INDEX` | `PReg.NUM_INDEX` | lib.rs:117 | 256 |
| `PReg::new()` | `PReg.new()` | lib.rs:122 | ✅ Identical encoding |
| `PReg::hw_enc()` | `PReg.hwEnc()` | lib.rs:131 | ✅ |
| `PReg::class()` | `PReg.class()` | lib.rs:137 | ✅ |
| `PReg::index()` | `PReg.index()` | lib.rs:150 | ✅ |
| `PReg::from_index()` | `PReg.fromIndex()` | lib.rs:156 | ✅ |
| `PReg::invalid()` | `PReg.invalid()` | lib.rs:165 | ✅ |
| `PRegSet` | `PRegSet` | lib.rs:221-400 | Bitset, 4×64-bit words |
| `PRegSet::empty()` | `PRegSet.empty()` | lib.rs:233 | ✅ |
| `PRegSet::contains()` | `PRegSet.contains()` | lib.rs:246 | ✅ |
| `PRegSet::with()` | `PRegSet.with()` | lib.rs:252 | ✅ Immutable add |
| `PRegSet::add()` | `PRegSet.add()` | lib.rs:260 | ✅ Mutable add |
| `PRegSet::remove()` | `PRegSet.remove()` | lib.rs:266 | ✅ |
| `PRegSet::union_from()` | `PRegSet.unionFrom()` | lib.rs:273 | ✅ |
| `PRegSet::intersect_from()` | `PRegSet.intersectFrom()` | lib.rs:279 | ✅ |
| `PRegSet::invert()` | `PRegSet.invert()` | lib.rs:285 | ✅ |
| `PRegSet::len()` | `PRegSet.count()` | lib.rs:298 | ✅ |
| `PRegSetIter` | `PRegSetIterator` | lib.rs:356-375 | ✅ |
| `VReg` | `VReg` | lib.rs:422-488 | Bit-packed: vreg:21 + class:2 = 23 bits (in u32) |
| `VReg::MAX_BITS` | `VReg.MAX_BITS` | lib.rs:429 | 21 |
| `VReg::MAX` | `VReg.MAX` | lib.rs:430 | 2,097,151 |
| `VReg::new()` | `VReg.new()` | lib.rs:433 | ✅ Identical encoding |
| `VReg::vreg()` | `VReg.vreg()` | lib.rs:441 | ✅ |
| `VReg::class()` | `VReg.class()` | lib.rs:447 | ✅ |
| `VReg::invalid()` | `VReg.invalid()` | lib.rs:457 | ✅ |
| `SpillSlot` | `SpillSlot` | lib.rs:498-548 | 24-bit index in u32 |
| `SpillSlot::MAX` | `SpillSlot.MAX` | lib.rs:504 | 16,777,215 |
| `SpillSlot::new()` | `SpillSlot.new()` | lib.rs:508 | ✅ |
| `SpillSlot::index()` | `SpillSlot.index()` | lib.rs:515 | ✅ |
| `SpillSlot::plus()` | `SpillSlot.plus()` | lib.rs:521 | ✅ |
| `SpillSlot::invalid()` | `SpillSlot.invalid()` | lib.rs:527 | ✅ |
| `Block` | `Block` | index.rs:137 | u32 index |
| `Block::new()` | `Block.new()` | index.rs | ✅ |
| `Block::index()` | `Block.idx()` | index.rs | ✅ |
| `Block::invalid()` | `Block.invalid()` | index.rs | ✅ |
| `Block::next()` | `Block.next()` | index.rs | ✅ |
| `Block::prev()` | `Block.prev()` | index.rs | ✅ |
| `Inst` | `Inst` | index.rs:136 | u32 index |
| `Inst::new()` | `Inst.new()` | index.rs | ✅ |
| `Inst::index()` | `Inst.idx()` | index.rs | ✅ |
| `Inst::invalid()` | `Inst.invalid()` | index.rs | ✅ |
| `InstRange` | `InstRange` | index.rs:144 | Half-open range [from, to) |
| `InstRange::new()` | `InstRange.new()` | index.rs:148 | ✅ |
| `InstRange::first()` | `InstRange.first()` | index.rs:154 | ✅ |
| `InstRange::last()` | `InstRange.last()` | index.rs:160 | ✅ |
| `InstRange::rest()` | `InstRange.rest()` | index.rs:166 | ✅ |
| `InstRange::len()` | `InstRange.len()` | index.rs:172 | ✅ |
| `InstRange::iter()` | `InstRange.iter()` | index.rs:177 | ✅ |

### Bit Encoding Verification

**PReg** (8 bits):
```
Rust:  bits = (class << 6) | hw_enc
Zig:   bits = (class << 6) | hw_enc
       ✅ Identical
```

**VReg** (32 bits, using 23):
```
Rust:  bits = (vreg << 2) | class
Zig:   bits = (vreg << 2) | class
       ✅ Identical
```

---

## Phase 6.2: Operands (operand.zig)

**Source**: `src/lib.rs` lines 550-1180
**Target**: `compiler/codegen/native/regalloc/operand.zig`
**Status**: ✅ Complete (762 LOC, 7 tests)

### Type Mapping

| Rust Type | Zig Type | Rust Location | Notes |
|-----------|----------|---------------|-------|
| `OperandConstraint` | `OperandConstraint` | lib.rs:562-580 | Tagged union |
| `OperandConstraint::Any` | `.any` | lib.rs:564 | ✅ |
| `OperandConstraint::Reg` | `.reg` | lib.rs:566 | ✅ |
| `OperandConstraint::Stack` | `.stack` | lib.rs:568 | ✅ |
| `OperandConstraint::FixedReg(PReg)` | `.{ .fixed_reg = PReg }` | lib.rs:570 | ✅ |
| `OperandConstraint::Reuse(usize)` | `.{ .reuse = usize }` | lib.rs:572 | ✅ |
| `OperandConstraint::Limit(usize)` | `.{ .limit = usize }` | lib.rs:579 | ✅ |
| `OperandKind` | `OperandKind` | lib.rs:597-602 | enum(u1) |
| `OperandKind::Def` | `.def` | lib.rs:600 | = 0 |
| `OperandKind::Use` | `.use` | lib.rs:601 | = 1 |
| `OperandPos` | `OperandPos` | lib.rs:622-627 | enum(u1) |
| `OperandPos::Early` | `.early` | lib.rs:625 | = 0 |
| `OperandPos::Late` | `.late` | lib.rs:626 | = 1 |
| `Operand` | `Operand` | lib.rs:653-1000 | 32-bit packed |
| `Operand::new()` | `Operand.new()` | lib.rs:676 | ✅ |
| `Operand::reg_use()` | `Operand.regUse()` | lib.rs:721 | ✅ |
| `Operand::reg_use_at_end()` | `Operand.regUseAtEnd()` | lib.rs:734 | ✅ |
| `Operand::reg_def()` | `Operand.regDef()` | lib.rs:748 | ✅ |
| `Operand::reg_def_at_start()` | `Operand.regDefAtStart()` | lib.rs:769 | ✅ |
| `Operand::reg_temp()` | `Operand.regTemp()` | lib.rs:791 | ✅ |
| `Operand::reg_reuse_def()` | `Operand.regReuseDef()` | lib.rs:809 | ✅ |
| `Operand::reg_fixed_use()` | `Operand.regFixedUse()` | lib.rs:823 | ✅ |
| `Operand::reg_fixed_def()` | `Operand.regFixedDef()` | lib.rs:837 | ✅ |
| `Operand::any_use()` | `Operand.anyUse()` | lib.rs:872 | ✅ |
| `Operand::any_def()` | `Operand.anyDef()` | lib.rs:885 | ✅ |
| `Operand::fixed_nonallocatable()` | `Operand.fixedNonAllocatable()` | lib.rs:898 | ✅ |
| `Operand::vreg()` | `Operand.vreg()` | lib.rs:912 | ✅ |
| `Operand::class()` | `Operand.class()` | lib.rs:919 | ✅ |
| `Operand::kind()` | `Operand.kind()` | lib.rs:932 | ✅ |
| `Operand::pos()` | `Operand.pos()` | lib.rs:946 | ✅ |
| `Operand::constraint()` | `Operand.constraint()` | lib.rs:958 | ✅ |
| `AllocationKind` | `AllocationKind` | lib.rs:1169-1178 | enum(u3) |
| `AllocationKind::None` | `.none` | lib.rs:1175 | = 0 |
| `AllocationKind::Reg` | `.reg` | lib.rs:1176 | = 1 |
| `AllocationKind::Stack` | `.stack` | lib.rs:1177 | = 2 |
| `Allocation` | `Allocation` | lib.rs:1037-1167 | 32-bit packed |
| `Allocation::none()` | `Allocation.none()` | lib.rs:1075 | ✅ |
| `Allocation::reg()` | `Allocation.reg()` | lib.rs:1081 | ✅ |
| `Allocation::stack()` | `Allocation.stack()` | lib.rs:1087 | ✅ |
| `Allocation::kind()` | `Allocation.kind()` | lib.rs:1093 | ✅ |
| `Allocation::is_none()` | `Allocation.isNone()` | lib.rs:1104 | ✅ |
| `Allocation::is_reg()` | `Allocation.isReg()` | lib.rs:1116 | ✅ |
| `Allocation::is_stack()` | `Allocation.isStack()` | lib.rs:1122 | ✅ |
| `Allocation::index()` | `Allocation.index()` | lib.rs:1129 | ✅ |
| `Allocation::as_reg()` | `Allocation.asReg()` | lib.rs:1135 | ✅ |
| `Allocation::as_stack()` | `Allocation.asStack()` | lib.rs:1145 | ✅ |
| `InstPosition` | `InstPosition` | lib.rs:1341-1344 | enum(u1) |
| `InstPosition::Before` | `.before` | lib.rs:1342 | = 0 |
| `InstPosition::After` | `.after` | lib.rs:1343 | = 1 |
| `ProgPoint` | `ProgPoint` | lib.rs:1349-1441 | 32-bit packed |
| `ProgPoint::new()` | `ProgPoint.new()` | lib.rs:1370 | ✅ |
| `ProgPoint::before()` | `ProgPoint.before()` | lib.rs:1377 | ✅ |
| `ProgPoint::after()` | `ProgPoint.after()` | lib.rs:1383 | ✅ |
| `ProgPoint::inst()` | `ProgPoint.inst()` | lib.rs:1389 | ✅ |
| `ProgPoint::pos()` | `ProgPoint.pos()` | lib.rs:1398 | ✅ |
| `ProgPoint::next()` | `ProgPoint.nextPoint()` | lib.rs:1410 | ✅ |
| `ProgPoint::prev()` | `ProgPoint.prevPoint()` | lib.rs:1419 | ✅ |
| `Edit` | `Edit` | lib.rs:1446-1455 | Tagged union |
| `Edit::Move` | `.move` | lib.rs:1454 | ✅ |

### Bit Encoding Verification

**Operand** (32 bits):
```
Rust:  constraint:7 | kind:1 | pos:1 | class:2 | vreg:21
Zig:   constraint:7 | kind:1 | pos:1 | class:2 | vreg:21
       ✅ Identical

Constraint encoding:
  1xxxxxx = FixedReg(hw_enc = xxxxxx)
  01xxxxx = Reuse(index = xxxxx)
  001xxxx = Limit(max = 1 << xxxx)
  0000000 = Any
  0000001 = Reg
  0000010 = Stack
```

**Allocation** (32 bits):
```
Rust:  kind:3 | unused:1 | index:28
Zig:   kind:3 | unused:1 | index:28
       ✅ Identical
```

**ProgPoint** (32 bits):
```
Rust:  inst:31 | pos:1
Zig:   inst:31 | pos:1
       ✅ Identical
```

---

## Phase 6.3: Function Interface (func.zig)

**Source**: `src/lib.rs` lines 1180-1330
**Target**: `compiler/codegen/native/regalloc/func.zig`
**Status**: ✅ Complete (~320 LOC, 2 tests)

### Trait Method Mapping

| Rust Trait Method | Zig Method | Rust Location | Notes |
|-------------------|------------|---------------|-------|
| `num_insts(&self)` | `numInsts(self)` | lib.rs:1194 | ✅ |
| `num_blocks(&self)` | `numBlocks(self)` | lib.rs:1197 | ✅ |
| `entry_block(&self)` | `entryBlock(self)` | lib.rs:1200 | ✅ |
| `block_insns(&self, Block)` | `blockInsns(self, Block)` | lib.rs:1203 | ✅ |
| `block_succs(&self, Block)` | `blockSuccs(self, Block)` | lib.rs:1206 | ✅ |
| `block_preds(&self, Block)` | `blockPreds(self, Block)` | lib.rs:1209 | ✅ |
| `block_params(&self, Block)` | `blockParams(self, Block)` | lib.rs:1212 | ✅ |
| `is_ret(&self, Inst)` | `isRet(self, Inst)` | lib.rs:1215 | ✅ |
| `is_branch(&self, Inst)` | `isBranch(self, Inst)` | lib.rs:1219 | ✅ |
| `branch_blockparams(...)` | `branchBlockparams(...)` | lib.rs:1225 | ✅ |
| `inst_operands(&self, Inst)` | `instOperands(self, Inst)` | lib.rs:1232 | ✅ |
| `inst_clobbers(&self, Inst)` | `instClobbers(self, Inst)` | lib.rs:1263 | ✅ |
| `num_vregs(&self)` | `numVregs(self)` | lib.rs:1266 | ✅ |
| `debug_value_labels(&self)` | `debugValueLabels(self)` | lib.rs:1285 | ✅ Default: empty |
| `spillslot_size(&self, RegClass)` | `spillslotSize(self, RegClass)` | lib.rs:1300 | ✅ |
| `multi_spillslot_named_by_last_slot()` | `multiSpillslotNamedByLastSlot()` | lib.rs:1306 | ✅ Default: false |
| `allow_multiple_vreg_defs()` | `allowMultipleVregDefs()` | lib.rs:1324 | ✅ Default: false |

### Implementation Pattern

Zig uses comptime generics instead of Rust traits:

```zig
// Usage:
const MyFunc = Function(VCode);
const func = MyFunc.init(&vcode);
const n = func.numInsts();
```

Also provides `FunctionVTable` for runtime dispatch when needed.

---

## Phase 6.4: Machine Environment (env.zig)

**Source**: `src/lib.rs` lines 1497-1544
**Target**: `compiler/codegen/native/regalloc/env.zig`
**Status**: ✅ Complete (~230 LOC, 4 tests)

### Type Mapping

| Rust Field/Type | Zig Field/Type | Rust Location | Notes |
|-----------------|----------------|---------------|-------|
| `MachineEnv` | `MachineEnv` | lib.rs:1503 | ✅ |
| `preferred_regs_by_class: [PRegSet; 3]` | `preferred_regs_by_class: [3]PRegSet` | lib.rs:1509 | ✅ |
| `non_preferred_regs_by_class: [PRegSet; 3]` | `non_preferred_regs_by_class: [3]PRegSet` | lib.rs:1518 | ✅ |
| `scratch_by_class: [Option<PReg>; 3]` | `scratch_by_class: [3]?PReg` | lib.rs:1535 | ✅ |
| `fixed_stack_slots: Vec<PReg>` | `fixed_stack_slots: []const PReg` | lib.rs:1543 | ✅ |

### ISA Configurations

**ARM64** (`arm64MachineEnv()`):
- Integer: x0-x17 preferred, x19-x28 non-preferred
- Float: v0-v7, v16-v31 preferred, v8-v15 non-preferred
- Scratch: x16 (IP0)
- Not allocatable: x29 (FP), x30 (LR), x31 (SP/ZR)

**x86-64** (`x64MachineEnv()`):
- Integer: rax, rcx, rdx, rsi, rdi, r8-r10 preferred; rbx, r12-r15 non-preferred
- Float: xmm0-xmm15 all preferred (System V ABI)
- Scratch: r11
- Not allocatable: rsp (4), rbp (5)

---

## Phase 6.5: Output (output.zig)

**Source**: `src/lib.rs` lines 1546-1710, `src/ion/data_structures.rs` lines 820-853
**Target**: `compiler/codegen/native/regalloc/output.zig`
**Status**: ✅ Complete (~310 LOC, 6 tests)

### Type Mapping

| Rust Type | Zig Type | Rust Location | Notes |
|-----------|----------|---------------|-------|
| `Output` | `Output` | lib.rs:1549 | ✅ |
| `Output.num_spillslots` | `Output.num_spillslots` | lib.rs:1551 | ✅ |
| `Output.edits` | `Output.edits` | lib.rs:1555 | `ArrayListUnmanaged(EditAtPoint)` |
| `Output.allocs` | `Output.allocs` | lib.rs:1559 | `ArrayListUnmanaged(Allocation)` |
| `Output.inst_alloc_offsets` | `Output.inst_alloc_offsets` | lib.rs:1562 | `ArrayListUnmanaged(u32)` |
| `Output.debug_locations` | `Output.debug_locations` | lib.rs:1570 | `ArrayListUnmanaged(DebugLocation)` |
| `Output.stats` | `Output.stats` | lib.rs:1573 | ✅ |
| `Output::inst_allocs()` | `Output.instAllocs()` | lib.rs:1578 | ✅ |
| `Output::block_insts_and_edits()` | `Output.blockInstsAndEdits()` | lib.rs:1590 | ✅ |
| `InstOrEdit` | `InstOrEdit` | lib.rs:1459 | Tagged union |
| `OutputIter` | `OutputIterator` | lib.rs:1465 | ✅ |
| `Stats` | `Stats` | data_structures.rs:822 | 31 fields, all usize |
| `RegAllocError` | `RegAllocError` | lib.rs:1616 | Tagged union |
| `RegAllocError::CritEdge` | `.crit_edge` | lib.rs:1618 | ✅ |
| `RegAllocError::SSA` | `.ssa` | lib.rs:1622 | ✅ |
| `RegAllocError::BB` | `.bb` | lib.rs:1626 | ✅ |
| `RegAllocError::Branch` | `.branch` | lib.rs:1630 | ✅ |
| `RegAllocError::EntryLivein` | `.entry_livein` | lib.rs:1632 | ✅ |
| `RegAllocError::DisallowedBranchArg` | `.disallowed_branch_arg` | lib.rs:1638 | ✅ |
| `RegAllocError::TooManyLiveRegs` | `.too_many_live_regs` | lib.rs:1641 | ✅ |
| `RegAllocError::TooManyOperands` | `.too_many_operands` | lib.rs:1644 | ✅ |
| `Algorithm` | `Algorithm` | lib.rs:1692 | enum { ion, fastalloc } |
| `RegallocOptions` | `RegallocOptions` | lib.rs:1700 | ✅ |

---

## Remaining Phases (TODO)

### Phase 6.6: CFG Analysis (cfg.zig)
- `CFGInfo` struct
- Postorder traversal
- Dominator tree
- Loop detection

### Phase 6.7: SSA Validation (ssa.zig)
- `validate_ssa()` function
- Single definition check
- Dominance verification

### Phase 6.8: Index Set (indexset.zig)
- Sparse-dense hybrid set
- O(1) insert, contains, remove

### Phase 6.9: Parallel Moves (moves.zig)
- Cycle detection in move graphs
- Move serialization

### Phase 6.10-6.18: Ion Allocator
- Full backtracking register allocator
- Live range analysis
- Bundle merging
- Allocation loop
- Spill slot assignment
- Move insertion

### Phase 6.19: Public API (lib.zig)
- `run()` function
- `RegallocOptions`
- Integration with VCode

---

## Verification Checklist

- [x] All bit encodings match regalloc2 exactly
- [x] All method signatures preserved
- [x] All edge cases handled (invalid values, overflow)
- [x] Tests cover all public API
- [ ] Integration tests with real VCode
- [ ] Performance benchmarks vs regalloc2

---

## References

- regalloc2 source: https://github.com/bytecodealliance/regalloc2
- regalloc2 design doc: regalloc2/doc/DESIGN.md
- Ion algorithm paper: "Linear Scan Register Allocation for the Java HotSpot Client Compiler"
