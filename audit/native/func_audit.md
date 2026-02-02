# Function Interface Module Audit (Phase 6.3)

**Source**: `regalloc2/src/lib.rs` lines 1180-1330
**Target**: `compiler/codegen/native/regalloc/func.zig`
**Status**: ✅ Complete (~320 LOC, 2 tests)

---

## Trait Method Mapping

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

---

## Implementation Pattern

Zig uses comptime generics instead of Rust traits:

```zig
// Usage:
const MyFunc = Function(VCode);
const func = MyFunc.init(&vcode);
const n = func.numInsts();
```

Also provides `FunctionVTable` for runtime dispatch when needed.

---

## Required Methods

Methods that must be implemented by the wrapped type:

| Method | Return Type | Description |
|--------|-------------|-------------|
| `numInsts` | `usize` | Total instruction count |
| `numBlocks` | `usize` | Total block count |
| `entryBlock` | `Block` | Entry block (usually block 0) |
| `blockInsns` | `InstRange` | Instructions in a block |
| `blockSuccs` | `[]const Block` | Successor blocks |
| `blockPreds` | `[]const Block` | Predecessor blocks |
| `blockParams` | `[]const VReg` | Block parameters |
| `isRet` | `bool` | Is instruction a return? |
| `isBranch` | `bool` | Is instruction a branch? |
| `branchBlockparams` | `[]const VReg` | Args passed to successor |
| `instOperands` | `[]const Operand` | Instruction operands |
| `instClobbers` | `PRegSet` | Clobbered registers |
| `numVregs` | `usize` | Total vreg count |

---

## Optional Methods (with defaults)

| Method | Default | Description |
|--------|---------|-------------|
| `debugValueLabels` | `&.{}` | Debug value labels |
| `spillslotSize` | `1` | Spillslot size per class |
| `multiSpillslotNamedByLastSlot` | `false` | ABI convention |
| `allowMultipleVregDefs` | `false` | Allow multiple defs of same vreg |

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| Function interface wrapping | ✅ | Comptime generic wrapping |
| VTable runtime dispatch | ✅ | Runtime polymorphism |

