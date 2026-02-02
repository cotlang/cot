# Register Allocator Execution Plan

**Source**: `regalloc2` crate (https://github.com/bytecodealliance/regalloc2)
**Target**: `compiler/codegen/native/regalloc/`
**Total Source LOC**: 12,631 (Rust)

---

## Architecture Overview

Regalloc is **ISA-independent**. One implementation serves both ARM64 and x64.

### How ISA Independence Works

```
                    ┌─────────────────────────────────────────┐
                    │             regalloc                     │
                    │  (ISA-independent allocation algorithm)  │
                    └─────────────────┬───────────────────────┘
                                      │
              ┌───────────────────────┴───────────────────────┐
              │                                               │
              ▼                                               ▼
    ┌─────────────────────┐                     ┌─────────────────────┐
    │   MachineEnv        │                     │   Function trait    │
    │   (ISA provides)    │                     │   (VCode implements)│
    ├─────────────────────┤                     ├─────────────────────┤
    │ ARM64:              │                     │ - num_insts()       │
    │   X0-X30 (int)      │                     │ - num_blocks()      │
    │   V0-V31 (float)    │                     │ - block_insns()     │
    │                     │                     │ - inst_operands()   │
    │ x64:                │                     │ - inst_clobbers()   │
    │   RAX-R15 (int)     │                     │ - is_branch()       │
    │   XMM0-XMM15 (float)│                     │ - etc.              │
    └─────────────────────┘                     └─────────────────────┘
```

---

## Module Dependency Graph

```
lib.zig (public API)
    │
    ├── index.zig          (Block, Inst, VReg, PReg types)
    │
    ├── cfg.zig            (CFG analysis)
    │       └── postorder.zig
    │       └── domtree.zig
    │
    ├── ssa.zig            (SSA validation)
    │
    ├── indexset.zig       (sparse/dense set)
    │
    ├── moves.zig          (parallel move resolution)
    │
    └── ion/               (main allocator)
            ├── data_structures.zig  (Ctx, Env, LiveRange, Bundle)
            ├── liveranges.zig       (liveness analysis)
            ├── merge.zig            (bundle merging)
            ├── process.zig          (allocation loop)
            ├── spill.zig            (spill slot allocation)
            ├── moves.zig            (move insertion)
            ├── requirement.zig      (constraint handling)
            ├── reg_traversal.zig    (register iteration)
            ├── redundant_moves.zig  (optimization)
            └── dump.zig             (debug output)
```

---

## File-by-File Port Plan

### Phase 6.1: Core Types (index.zig)

**Source**: `src/lib.rs` lines 73-600, `src/index.rs`
**Target**: `compiler/codegen/native/regalloc/index.zig`
**LOC**: ~400

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `RegClass` | lib.rs:86 | enum { Int, Float, Vector } |
| `PReg` | lib.rs:108 | Physical register (hw_enc + class) |
| `PReg.new()` | lib.rs:122 | Constructor |
| `PReg.hw_enc()` | lib.rs:131 | Get hardware encoding |
| `PReg.class()` | lib.rs:137 | Get register class |
| `PReg.index()` | lib.rs:150 | Get unified index |
| `PReg.from_index()` | lib.rs:156 | Construct from index |
| `PRegSet` | lib.rs:221 | Bitset of physical registers |
| `PRegSet.empty()` | lib.rs:233 | Empty set constructor |
| `PRegSet.contains()` | lib.rs:246 | Membership test |
| `PRegSet.with()` | lib.rs:252 | Add register (immutable) |
| `PRegSet.add()` | lib.rs:260 | Add register (mutable) |
| `PRegSet.remove()` | lib.rs:266 | Remove register |
| `PRegSet.union_from()` | lib.rs:273 | Set union |
| `PRegSet.intersect_from()` | lib.rs:279 | Set intersection |
| `VReg` | lib.rs:422 | Virtual register |
| `VReg.new()` | lib.rs:433 | Constructor |
| `VReg.vreg()` | lib.rs:441 | Get vreg number |
| `VReg.class()` | lib.rs:447 | Get register class |
| `SpillSlot` | lib.rs:498 | Stack slot for spills |
| `Block` | index.rs | Basic block index |
| `Inst` | index.rs | Instruction index |
| `InstRange` | index.rs | Range of instructions |

**Tests**: Constructor/accessor tests for each type.

---

### Phase 6.2: Operands (operand.zig)

**Source**: `src/lib.rs` lines 600-1180
**Target**: `compiler/codegen/native/regalloc/operand.zig`
**LOC**: ~350

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `Operand` | lib.rs:600 | Register operand descriptor |
| `OperandKind` | lib.rs:650 | Def, Use, DefUse |
| `OperandPos` | lib.rs:680 | Early, Late |
| `OperandConstraint` | lib.rs:700 | Any, Reg, FixedReg, Reuse, Stack |
| `Operand.new()` | lib.rs:750 | Constructor |
| `Operand.vreg()` | lib.rs:780 | Get virtual register |
| `Operand.kind()` | lib.rs:790 | Get operand kind |
| `Operand.pos()` | lib.rs:800 | Get position (early/late) |
| `Operand.constraint()` | lib.rs:810 | Get constraint |
| `Allocation` | lib.rs:900 | Result: PReg or SpillSlot |
| `Allocation.none()` | lib.rs:920 | No allocation |
| `Allocation.reg()` | lib.rs:930 | Register allocation |
| `Allocation.stack()` | lib.rs:940 | Stack allocation |
| `Edit` | lib.rs:1000 | Move or other edit |

**Tests**: Operand construction, constraint matching.

---

### Phase 6.3: Function Trait (func.zig)

**Source**: `src/lib.rs` lines 1180-1330
**Target**: `compiler/codegen/native/regalloc/func.zig`
**LOC**: ~150

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `Function` (interface) | lib.rs:1188 | Trait for input code |
| `num_insts()` | lib.rs:1194 | Instruction count |
| `num_blocks()` | lib.rs:1197 | Block count |
| `entry_block()` | lib.rs:1200 | Entry block index |
| `block_insns()` | lib.rs:1203 | Instructions in block |
| `block_succs()` | lib.rs:1206 | CFG successors |
| `block_preds()` | lib.rs:1209 | CFG predecessors |
| `block_params()` | lib.rs:1212 | Block parameters |
| `is_ret()` | lib.rs:1215 | Is return instruction? |
| `is_branch()` | lib.rs:1219 | Is branch instruction? |
| `branch_blockparams()` | lib.rs:1225 | Branch arguments |
| `inst_operands()` | lib.rs:1232 | Instruction operands |
| `inst_clobbers()` | lib.rs:1263 | Clobbered registers |
| `num_vregs()` | lib.rs:1266 | Virtual register count |
| `spillslot_size()` | lib.rs:1300 | Spill slot size per class |

**Implementation**: Zig interface (vtable or comptime dispatch).

---

### Phase 6.4: Machine Environment (env.zig)

**Source**: `src/lib.rs` lines 1500-1550
**Target**: `compiler/codegen/native/regalloc/env.zig`
**LOC**: ~100

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `MachineEnv` | lib.rs:1503 | ISA register info |
| `preferred_regs_by_class` | lib.rs:1509 | Preferred allocatable regs |
| `non_preferred_regs_by_class` | lib.rs:1518 | Secondary allocatable regs |
| `scratch_by_class` | lib.rs:1535 | Dedicated scratch regs |
| `fixed_stack_slots` | lib.rs:1543 | Pre-defined stack slots |

**Tests**: ARM64 and x64 MachineEnv construction.

---

### Phase 6.5: Output (output.zig)

**Source**: `src/lib.rs` lines 1546-1610
**Target**: `compiler/codegen/native/regalloc/output.zig`
**LOC**: ~100

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `Output` | lib.rs:1549 | Allocation result |
| `num_spillslots` | lib.rs:1551 | Spill slot count |
| `edits` | lib.rs:1555 | Inserted moves |
| `allocs` | lib.rs:1559 | Per-operand allocations |
| `inst_alloc_offsets` | lib.rs:1562 | Index into allocs |
| `inst_allocs()` | lib.rs:1578 | Get allocs for instruction |
| `block_insts_and_edits()` | lib.rs:1590 | Iterate with edits |
| `RegAllocError` | lib.rs:1616 | Error types |

---

### Phase 6.6: CFG Analysis (cfg.zig)

**Source**: `src/cfg.rs`, `src/postorder.rs`, `src/domtree.rs`
**Target**: `compiler/codegen/native/regalloc/cfg.zig`
**LOC**: ~330

| Type/Function | Source File | Description |
|---------------|-------------|-------------|
| `CFGInfo` | cfg.rs:20 | Cached CFG analysis |
| `CFGInfo.init()` | cfg.rs:50 | Compute CFG info |
| `postorder()` | postorder.rs | Postorder traversal |
| `DomTree` | domtree.rs | Dominator tree |
| `DomTree.dominates()` | domtree.rs | Dominance query |

---

### Phase 6.7: SSA Validation (ssa.zig)

**Source**: `src/ssa.rs`
**Target**: `compiler/codegen/native/regalloc/ssa.zig`
**LOC**: ~140

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `validate_ssa()` | ssa.rs:10 | Validate SSA form |
| Check single def | ssa.rs:30 | Each vreg defined once |
| Check def dominates use | ssa.rs:50 | Proper SSA dominance |

---

### Phase 6.8: Index Set (indexset.zig)

**Source**: `src/indexset.rs`
**Target**: `compiler/codegen/native/regalloc/indexset.zig`
**LOC**: ~350

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `IndexSet` | indexset.rs:20 | Sparse-dense hybrid set |
| `IndexSet.new()` | indexset.rs:40 | Constructor |
| `IndexSet.insert()` | indexset.rs:60 | Insert element |
| `IndexSet.contains()` | indexset.rs:80 | Membership test |
| `IndexSet.remove()` | indexset.rs:100 | Remove element |
| `IndexSet.iter()` | indexset.rs:120 | Iterate elements |

---

### Phase 6.9: Ion Data Structures (ion/data_structures.zig)

**Source**: `src/ion/data_structures.rs`
**Target**: `compiler/codegen/native/regalloc/ion/data_structures.zig`
**LOC**: ~870

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `Ctx` | data_structures.rs:50 | Reusable allocator context |
| `Env` | data_structures.rs:150 | Per-allocation environment |
| `LiveRange` | data_structures.rs:250 | Live range (start, end, vreg) |
| `LiveRangeKey` | data_structures.rs:280 | BTree key for ranges |
| `LiveRangeList` | data_structures.rs:300 | List of live ranges |
| `Bundle` | data_structures.rs:350 | Group of mergeable ranges |
| `SpillSet` | data_structures.rs:400 | Spill decision info |
| `SpillSlotList` | data_structures.rs:450 | Available spill slots |
| `PRegData` | data_structures.rs:500 | Per-preg allocation state |
| `VRegData` | data_structures.rs:550 | Per-vreg info |
| `AllocationQueue` | data_structures.rs:600 | Priority queue of bundles |
| `Stats` | data_structures.rs:700 | Allocation statistics |

---

### Phase 6.10: Liveness Analysis (ion/liveranges.zig)

**Source**: `src/ion/liveranges.rs`
**Target**: `compiler/codegen/native/regalloc/ion/liveranges.zig`
**LOC**: ~920

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `compute_liveness()` | liveranges.rs:20 | Main liveness pass |
| `build_liveranges()` | liveranges.rs:150 | Create live ranges |
| `add_liverange()` | liveranges.rs:300 | Add range for vreg |
| `fixup_multi_fixed_vregs()` | liveranges.rs:400 | Handle multi-def cases |
| Backward dataflow | liveranges.rs:500 | Classic liveness algorithm |

---

### Phase 6.11: Bundle Merging (ion/merge.zig)

**Source**: `src/ion/merge.rs`
**Target**: `compiler/codegen/native/regalloc/ion/merge.zig`
**LOC**: ~440

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `merge_vreg_bundles()` | merge.rs:20 | Merge compatible bundles |
| `can_merge()` | merge.rs:100 | Check merge compatibility |
| `do_merge()` | merge.rs:200 | Perform merge |
| `queue_bundles()` | merge.rs:350 | Add to allocation queue |

---

### Phase 6.12: Allocation Processing (ion/process.zig)

**Source**: `src/ion/process.rs`
**Target**: `compiler/codegen/native/regalloc/ion/process.zig`
**LOC**: ~1400

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `process_bundles()` | process.rs:20 | Main allocation loop |
| `try_allocate_bundle()` | process.rs:100 | Allocate one bundle |
| `find_best_reg()` | process.rs:250 | Find best register |
| `evict_bundle()` | process.rs:400 | Evict for better bundle |
| `split_bundle()` | process.rs:600 | Split at conflict point |
| `try_allocating_regs_for_spilled_bundles()` | process.rs:900 | Second-chance allocation |

---

### Phase 6.13: Spill Slot Allocation (ion/spill.zig)

**Source**: `src/ion/spill.rs`
**Target**: `compiler/codegen/native/regalloc/ion/spill.zig`
**LOC**: ~175

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `allocate_spillslots()` | spill.rs:20 | Assign stack slots |
| `find_or_create_spillslot()` | spill.rs:80 | Get slot for bundle |
| `reuse_spillslot()` | spill.rs:120 | Try to reuse existing |

---

### Phase 6.14: Move Resolution (ion/moves.zig + moves.zig)

**Source**: `src/ion/moves.rs`, `src/moves.rs`
**Target**: `compiler/codegen/native/regalloc/ion/moves.zig`, `moves.zig`
**LOC**: ~1460

| Type/Function | Source | Description |
|---------------|--------|-------------|
| `apply_allocations_and_insert_moves()` | ion/moves.rs:20 | Create move list |
| `resolve_inserted_moves()` | ion/moves.rs:200 | Order moves correctly |
| `ParallelMoves` | moves.rs:50 | Parallel move resolver |
| `resolve()` | moves.rs:100 | Handle cycles with scratch |
| `emit_move()` | moves.rs:200 | Output single move |

---

### Phase 6.15: Requirement Handling (ion/requirement.zig)

**Source**: `src/ion/requirement.rs`
**Target**: `compiler/codegen/native/regalloc/ion/requirement.zig`
**LOC**: ~185

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `Requirement` | requirement.rs:20 | Register constraint |
| `Requirement.merge()` | requirement.rs:60 | Combine constraints |
| `Requirement.is_satisfied()` | requirement.rs:100 | Check allocation |

---

### Phase 6.16: Register Traversal (ion/reg_traversal.zig)

**Source**: `src/ion/reg_traversal.rs`
**Target**: `compiler/codegen/native/regalloc/ion/reg_traversal.zig`
**LOC**: ~115

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `RegTraversal` | reg_traversal.rs:20 | Iterator over registers |
| `preferred_first()` | reg_traversal.rs:50 | Prefer cheap registers |

---

### Phase 6.17: Redundant Move Elimination (ion/redundant_moves.zig)

**Source**: `src/ion/redundant_moves.rs`
**Target**: `compiler/codegen/native/regalloc/ion/redundant_moves.zig`
**LOC**: ~130

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `remove_redundant_moves()` | redundant_moves.rs:20 | Eliminate no-op moves |

---

### Phase 6.18: Debug Output (ion/dump.zig)

**Source**: `src/ion/dump.rs`
**Target**: `compiler/codegen/native/regalloc/ion/dump.zig`
**LOC**: ~145

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `dump_state()` | dump.rs:20 | Debug print state |
| `dump_results()` | dump.rs:80 | Print final allocation |

---

### Phase 6.19: Top-Level API (lib.zig)

**Source**: `src/lib.rs` lines 1656-1720
**Target**: `compiler/codegen/native/regalloc/lib.zig`
**LOC**: ~100

| Type/Function | Rust Location | Description |
|---------------|---------------|-------------|
| `run()` | lib.rs:1656 | Main entry point |
| `run_with_ctx()` | lib.rs:1676 | Reusable context version |
| `RegallocOptions` | lib.rs:1699 | Configuration options |
| `Algorithm` | lib.rs:1692 | Ion vs Fastalloc |

---

## Directory Structure

```
compiler/codegen/native/regalloc/
├── lib.zig              # Public API, re-exports
├── index.zig            # Block, Inst, VReg, PReg, PRegSet, SpillSlot
├── operand.zig          # Operand, OperandKind, Allocation, Edit
├── func.zig             # Function interface
├── env.zig              # MachineEnv
├── output.zig           # Output, RegAllocError
├── cfg.zig              # CFGInfo, postorder, domtree
├── ssa.zig              # SSA validation
├── indexset.zig         # IndexSet (sparse-dense)
├── moves.zig            # ParallelMoves resolver
└── ion/
    ├── mod.zig          # Ion allocator entry
    ├── data_structures.zig
    ├── liveranges.zig
    ├── merge.zig
    ├── process.zig
    ├── spill.zig
    ├── moves.zig
    ├── requirement.zig
    ├── reg_traversal.zig
    ├── redundant_moves.zig
    └── dump.zig
```

---

## Implementation Order

| Task | File | LOC | Dependencies |
|------|------|-----|--------------|
| 6.1 | index.zig | 400 | None |
| 6.2 | operand.zig | 350 | index.zig |
| 6.3 | func.zig | 150 | index.zig, operand.zig |
| 6.4 | env.zig | 100 | index.zig |
| 6.5 | output.zig | 100 | index.zig, operand.zig |
| 6.6 | cfg.zig | 330 | index.zig, func.zig |
| 6.7 | ssa.zig | 140 | index.zig, func.zig, cfg.zig |
| 6.8 | indexset.zig | 350 | None |
| 6.9 | ion/data_structures.zig | 870 | index.zig, operand.zig, indexset.zig |
| 6.10 | ion/liveranges.zig | 920 | data_structures.zig, cfg.zig |
| 6.11 | ion/merge.zig | 440 | data_structures.zig |
| 6.12 | ion/process.zig | 1400 | data_structures.zig, merge.zig, liveranges.zig |
| 6.13 | ion/spill.zig | 175 | data_structures.zig |
| 6.14 | ion/moves.zig + moves.zig | 1460 | data_structures.zig |
| 6.15 | ion/requirement.zig | 185 | index.zig |
| 6.16 | ion/reg_traversal.zig | 115 | index.zig, env.zig |
| 6.17 | ion/redundant_moves.zig | 130 | data_structures.zig |
| 6.18 | ion/dump.zig | 145 | data_structures.zig |
| 6.19 | lib.zig | 100 | All above |

**Estimated Total**: ~7,300 LOC Zig (vs 12,631 Rust - Zig is more concise)

---

## Testing Strategy

1. **Unit tests per file** - Each .zig file has `test` blocks
2. **Integration tests** - Small CFGs with known-correct allocations
3. **Fuzzing** - Port `src/fuzzing/` (optional, for robustness)
4. **Checker** - Port `src/checker.rs` to validate allocations

---

## Integration Points

### With ARM64 Backend

```zig
// compiler/codegen/native/isa/aarch64/mod.zig
const regalloc = @import("../../regalloc/lib.zig");

pub fn createMachineEnv() regalloc.MachineEnv {
    return .{
        .preferred_regs_by_class = .{
            // Int: X0-X15 (caller-saved)
            PRegSet.from_slice(&[_]PReg{ x0, x1, x2, ... }),
            // Float: V0-V7 (caller-saved)
            PRegSet.from_slice(&[_]PReg{ v0, v1, v2, ... }),
            // Vector: same as float
            PRegSet.empty(),
        },
        .non_preferred_regs_by_class = .{
            // Int: X19-X28 (callee-saved)
            PRegSet.from_slice(&[_]PReg{ x19, x20, ... }),
            // Float: V8-V15 (callee-saved lower 64 bits)
            PRegSet.from_slice(&[_]PReg{ v8, v9, ... }),
            PRegSet.empty(),
        },
        .scratch_by_class = .{ x16, null, null }, // IP0
        .fixed_stack_slots = &[_]PReg{},
    };
}
```

### With x64 Backend

```zig
// compiler/codegen/native/isa/x64/mod.zig
pub fn createMachineEnv() regalloc.MachineEnv {
    return .{
        .preferred_regs_by_class = .{
            // Int: RAX, RCX, RDX, RSI, RDI, R8-R11 (caller-saved)
            PRegSet.from_slice(&[_]PReg{ rax, rcx, rdx, ... }),
            // Float: XMM0-XMM7 (caller-saved on SysV)
            PRegSet.from_slice(&[_]PReg{ xmm0, xmm1, ... }),
            PRegSet.empty(),
        },
        .non_preferred_regs_by_class = .{
            // Int: RBX, R12-R15 (callee-saved)
            PRegSet.from_slice(&[_]PReg{ rbx, r12, r13, r14, r15 }),
            // Float: XMM8-XMM15 (caller-saved on SysV, callee on Win64)
            PRegSet.from_slice(&[_]PReg{ xmm8, xmm9, ... }),
            PRegSet.empty(),
        },
        .scratch_by_class = .{ null, null, null }, // auto-allocate
        .fixed_stack_slots = &[_]PReg{},
    };
}
```

---

## Audit Documents

Each file gets an audit document in `audit/clif/regalloc/`:

```
audit/clif/regalloc/
├── index.zig.md
├── operand.zig.md
├── func.zig.md
├── env.zig.md
├── output.zig.md
├── cfg.zig.md
├── ssa.zig.md
├── indexset.zig.md
├── moves.zig.md
└── ion/
    ├── data_structures.zig.md
    ├── liveranges.zig.md
    ├── merge.zig.md
    ├── process.zig.md
    ├── spill.zig.md
    ├── moves.zig.md
    ├── requirement.zig.md
    ├── reg_traversal.zig.md
    ├── redundant_moves.zig.md
    └── dump.zig.md
```

---

## Verification Commands

```bash
# Test regalloc module
zig test compiler/codegen/native/regalloc/lib.zig

# Test with ARM64 integration
zig test compiler/codegen/native/isa/aarch64/mod.zig

# Test with x64 integration
zig test compiler/codegen/native/isa/x64/mod.zig

# Full native codegen test
zig build test
```
