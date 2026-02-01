# Cranelift Port: Master Plan

## Document Purpose

This is the single source of truth for the surgical rewrite of Cot's native codegen.
Every file removed, every file created, every audit document - all tracked here.

**Philosophy**: No dead code. No orphaned docs. 100% Cranelift coverage with audit trail.

---

## Table of Contents

1. [Phase 0: Surgical Removal](#phase-0-surgical-removal)
2. [Phase 1: Foundation - CLIF IR](#phase-1-foundation---clif-ir)
3. [Phase 2: Wasm Translation](#phase-2-wasm-translation)
4. [Phase 3: Machine Instruction Framework](#phase-3-machine-instruction-framework)
5. [Phase 4: ARM64 Backend](#phase-4-arm64-backend)
6. [Phase 5: x86-64 Backend](#phase-5-x86-64-backend)
7. [Phase 6: Register Allocation](#phase-6-register-allocation)
8. [Phase 7: Integration](#phase-7-integration)
9. [Phase 8: Self-Hosting](#phase-8-self-hosting)
10. [Task Tracking](#task-tracking)

---

## Phase 0: Surgical Removal

**Principle**: Remove ALL dead/wrong code FIRST. No confusion, no orphans.

### 0.1 Code Files to DELETE

These files are based on the wrong architecture (Go SSA for Go→native, not Wasm→native):

| File | LOC | Reason for Removal |
|------|-----|-------------------|
| `compiler/codegen/native/wasm_to_ssa.zig` | 806 | Wrong architecture - Go SSA instead of CLIF |
| `compiler/codegen/native/regalloc.zig` | 859 | Too simple - need regalloc2 port |
| `compiler/codegen/native/liveness.zig` | 854 | Tied to wrong regalloc |
| `compiler/codegen/native/stackalloc.zig` | 363 | Tied to wrong architecture |
| `compiler/codegen/native/expand_calls.zig` | 256 | Go-style call expansion |
| `compiler/codegen/native/decompose.zig` | 285 | Go-style decomposition |
| `compiler/codegen/native/generic.zig` | 308 | Go-style generic lowering |
| `compiler/codegen/native/abi.zig` | 387 | Will be replaced by Cranelift ABI |
| `compiler/codegen/native/arm64.zig` | 2,911 | Wrong lowering architecture |
| `compiler/codegen/native/amd64.zig` | 3,146 | Wrong lowering architecture |
| `compiler/ssa/passes/insert_phi.zig` | 270 | Go-style phi (CLIF uses block params) |
| `compiler/ssa/passes/lower_phi.zig` | 180 | Go-style phi lowering |
| **TOTAL** | **10,625** | |

### 0.2 Code Files to KEEP

These are architecture-agnostic and correct:

| File | LOC | Reason to Keep |
|------|-----|----------------|
| `compiler/codegen/native/wasm_parser.zig` | 432 | Just parses Wasm binary - correct |
| `compiler/codegen/native/arm64_asm.zig` | 989 | Low-level instruction encoding - correct |
| `compiler/codegen/native/amd64_asm.zig` | 1,628 | Low-level instruction encoding - correct |
| `compiler/codegen/native/amd64_regs.zig` | 218 | Register definitions - correct |
| `compiler/codegen/native/macho.zig` | 544 | Mach-O object format - correct |
| `compiler/codegen/native/elf.zig` | 525 | ELF object format - correct |
| `compiler/codegen/native/dwarf.zig` | 363 | Debug info - correct |
| **TOTAL** | **4,699** | |

### 0.3 Audit Documents to DELETE

These audits reference removed/wrong code:

| File | Reason for Removal |
|------|-------------------|
| `audit/native/arm64.zig.md` | References wrong arm64.zig |
| `audit/native/amd64.zig.md` | References wrong amd64.zig |
| `audit/native/generic.zig.md` | References removed generic.zig |
| `audit/WASM_TO_SSA_CRANELIFT_AUDIT.md` | Partial/outdated Cranelift analysis |
| `audit/WASM_PHI_GO_PARITY.md` | Go phi - we're using CLIF block params |
| `audit/PHI_GO_PARITY.md` | Go phi - we're using CLIF block params |
| `audit/WASM_BR_TABLE_CRANELIFT.md` | Will be replaced with complete audit |

### 0.4 Planning Documents to DELETE

These plans are superseded by this document:

| File | Reason for Removal |
|------|-------------------|
| `AOT_EXECUTION_PLAN.md` | Superseded |
| `AOT_STRATEGY.md` | Superseded |
| `BR_TABLE_EXECUTION_PLAN.md` | Superseded |
| `NATIVE_CODEGEN_PORT_PLAN.md` | Superseded |
| `PHI_IMPLEMENTATION.md` | Go phi - superseded |
| `WASM_PHI_INSERTION_PLAN.md` | Go phi - superseded |

### 0.5 Driver.zig Modifications

Remove references to deleted code:

```zig
// REMOVE these imports:
const wasm_to_ssa = @import("codegen/native/wasm_to_ssa.zig");
const insert_phi = @import("ssa/passes/insert_phi.zig");
const expand_calls = @import("codegen/native/expand_calls.zig");
const decompose = @import("codegen/native/decompose.zig");

// REMOVE native codegen calls (lines ~291-337)
// Will be replaced with new Cranelift-style pipeline
```

### 0.6 Phase 0 Task Checklist

- [x] **0.1** Delete `wasm_to_ssa.zig`
- [x] **0.2** Delete `regalloc.zig`
- [x] **0.3** Delete `liveness.zig`
- [x] **0.4** Delete `stackalloc.zig`
- [x] **0.5** Delete `expand_calls.zig`
- [x] **0.6** Delete `decompose.zig` (native version)
- [x] **0.7** Delete `generic.zig`
- [x] **0.8** ~~Delete~~ Stub `abi.zig` (minimal type stubs for value.zig)
- [x] **0.9** Delete `arm64.zig`
- [x] **0.10** Delete `amd64.zig`
- [x] **0.11** Delete `insert_phi.zig`
- [x] **0.12** Delete `lower_phi.zig`
- [x] **0.13** Delete `audit/native/arm64.zig.md`
- [x] **0.14** Delete `audit/native/amd64.zig.md`
- [x] **0.15** Delete `audit/native/generic.zig.md`
- [x] **0.16** Delete `audit/WASM_TO_SSA_CRANELIFT_AUDIT.md`
- [x] **0.17** Delete `audit/WASM_PHI_GO_PARITY.md`
- [x] **0.18** Delete `audit/PHI_GO_PARITY.md`
- [x] **0.19** Delete `audit/WASM_BR_TABLE_CRANELIFT.md`
- [x] **0.20** Delete `AOT_EXECUTION_PLAN.md`
- [x] **0.21** Delete `AOT_STRATEGY.md`
- [x] **0.22** Delete `BR_TABLE_EXECUTION_PLAN.md`
- [x] **0.23** Delete `NATIVE_CODEGEN_PORT_PLAN.md`
- [x] **0.24** Delete `PHI_IMPLEMENTATION.md`
- [x] **0.25** Delete `WASM_PHI_INSERTION_PLAN.md`
- [x] **0.26** Update `driver.zig` - remove native codegen calls (stub with error)
- [x] **0.27** Run `zig build test` - verify Wasm still works
- [x] **0.28** Commit: "Remove Go-style native codegen for Cranelift rewrite"

---

## Phase 1: Foundation - CLIF IR

**Cranelift Source**: `cranelift/codegen/src/ir/`
**Cot Target**: `compiler/ir/clif/`

### 1.1 Type System

**Source**: `ir/types.rs` (800 LOC)
**Target**: `compiler/ir/clif/types.zig`

**Cranelift Types to Port**:
```rust
// From types.rs
pub enum Type {
    I8, I16, I32, I64, I128,
    F32, F64,
    B1, B8, B16, B32, B64, B128,  // Booleans
    R32, R64,                      // References
    I8X16, I16X8, I32X4, I64X2,   // SIMD
    F32X4, F64X2,
}
```

**Audit Document**: `audit/clif/types.zig.md`
- Function-by-function mapping to Cranelift
- 100% coverage verification
- Test cases from Cranelift

### 1.2 Instructions

**Source**: `ir/instructions.rs` (2,500 LOC)
**Target**: `compiler/ir/clif/instructions.zig`

**Cranelift Opcodes to Port** (partial list):
```rust
// Control flow
Jump, Brif, BrTable, Return, Call, CallIndirect,
// Integer arithmetic
Iadd, Isub, Imul, Udiv, Sdiv, Urem, Srem,
// Bitwise
Band, Bor, Bxor, Bnot, Ishl, Ushr, Sshr, Rotl, Rotr,
// Comparison
Icmp, // with IntCC: Equal, NotEqual, SignedLessThan, etc.
// Memory
Load, Store, StackLoad, StackStore,
// Constants
Iconst, F32const, F64const,
// Conversions
Uextend, Sextend, Ireduce, Bitcast,
// etc. (~200 opcodes total)
```

**Audit Document**: `audit/clif/instructions.zig.md`

### 1.3 Data Flow Graph

**Source**: `ir/dfg.rs` (3,200 LOC)
**Target**: `compiler/ir/clif/dfg.zig`

**Key Structures**:
```rust
pub struct DataFlowGraph {
    insts: PrimaryMap<Inst, InstructionData>,
    results: SecondaryMap<Inst, ValueList>,
    values: PrimaryMap<Value, ValueData>,
    blocks: PrimaryMap<Block, BlockData>,
    // Block parameters (NOT phi nodes!)
    block_params: SecondaryMap<Block, ValueList>,
}
```

**Critical**: CLIF uses **block parameters**, not phi nodes. This is fundamental.

**Audit Document**: `audit/clif/dfg.zig.md`

### 1.4 Layout

**Source**: `ir/layout.rs` (1,200 LOC)
**Target**: `compiler/ir/clif/layout.zig`

**Purpose**: Maintain block and instruction ordering.

**Audit Document**: `audit/clif/layout.zig.md`

### 1.5 Function

**Source**: `ir/function.rs` (1,500 LOC)
**Target**: `compiler/ir/clif/function.zig`

**Key Structure**:
```rust
pub struct Function {
    pub name: ExternalName,
    pub signature: Signature,
    pub dfg: DataFlowGraph,
    pub layout: Layout,
    pub stack_slots: StackSlots,
    pub jump_tables: JumpTables,
}
```

**Audit Document**: `audit/clif/function.zig.md`

### 1.6 Builder

**Source**: `ir/builder.rs` (800 LOC)
**Target**: `compiler/ir/clif/builder.zig`

**Purpose**: Safe API for constructing CLIF IR.

**Audit Document**: `audit/clif/builder.zig.md`

### 1.7 Jump Tables

**Source**: `ir/jumptable.rs` (200 LOC)
**Target**: `compiler/ir/clif/jumptable.zig`

**Purpose**: br_table target storage.

**Audit Document**: `audit/clif/jumptable.zig.md`

### 1.8 Phase 1 Task Checklist

- [ ] **1.1** Create `compiler/ir/clif/` directory
- [ ] **1.2** Port `types.rs` → `types.zig`
- [ ] **1.3** Create `audit/clif/types.zig.md` with 100% coverage
- [ ] **1.4** Port `instructions.rs` → `instructions.zig`
- [ ] **1.5** Create `audit/clif/instructions.zig.md`
- [ ] **1.6** Port `dfg.rs` → `dfg.zig`
- [ ] **1.7** Create `audit/clif/dfg.zig.md`
- [ ] **1.8** Port `layout.rs` → `layout.zig`
- [ ] **1.9** Create `audit/clif/layout.zig.md`
- [ ] **1.10** Port `function.rs` → `function.zig`
- [ ] **1.11** Create `audit/clif/function.zig.md`
- [ ] **1.12** Port `builder.rs` → `builder.zig`
- [ ] **1.13** Create `audit/clif/builder.zig.md`
- [ ] **1.14** Port `jumptable.rs` → `jumptable.zig`
- [ ] **1.15** Create `audit/clif/jumptable.zig.md`
- [ ] **1.16** Port Cranelift IR unit tests
- [ ] **1.17** Run tests, verify all pass
- [ ] **1.18** Commit: "Port Cranelift IR (CLIF) infrastructure"

---

## Phase 2: Wasm Translation

**Cranelift Source**: `crates/cranelift/src/translate/`
**Cot Target**: `compiler/codegen/native/wasm_to_clif/`

### 2.1 Control Stack

**Source**: `translate/stack.rs` (600 LOC)
**Target**: `compiler/codegen/native/wasm_to_clif/stack.zig`

**Key Structure**:
```rust
pub enum ControlStackFrame {
    Block {
        destination: Block,
        num_param_values: usize,
        num_return_values: usize,
        original_stack_size: usize,
        exit_is_branched_to: bool,
    },
    Loop {
        destination: Block,      // Exit block
        header: Block,           // Re-entry point
        num_param_values: usize,
        num_return_values: usize,
        original_stack_size: usize,
    },
    If {
        destination: Block,
        else_data: Option<ElseData>,
        num_param_values: usize,
        num_return_values: usize,
        original_stack_size: usize,
        exit_is_branched_to: bool,
        head_is_reachable: bool,
        consequent_ends_reachable: bool,
    },
}

impl ControlStackFrame {
    // CRITICAL: Loop returns to header, not destination
    pub fn br_destination(&self) -> Block {
        match *self {
            Self::Block { destination, .. } |
            Self::If { destination, .. } => destination,
            Self::Loop { header, .. } => header,
        }
    }
}
```

**Audit Document**: `audit/clif/wasm_to_clif/stack.zig.md`
- Must cover 100% of ControlStackFrame
- Must cover br_destination() semantics
- Must cover all helper methods

### 2.2 Value Stack

**Source**: Part of `translate/stack.rs`
**Target**: `compiler/codegen/native/wasm_to_clif/value_stack.zig`

**Purpose**: Track Wasm operand stack during translation.

**Audit Document**: `audit/clif/wasm_to_clif/value_stack.zig.md`

### 2.3 Code Translator

**Source**: `translate/code_translator.rs` (2,800 LOC)
**Target**: `compiler/codegen/native/wasm_to_clif/translator.zig`

**Key Functions to Port**:

```rust
// Control flow
fn translate_operator(&mut self, op: Operator) -> Result<()>
fn translate_block(&mut self, blockty: BlockType) -> Result<()>
fn translate_loop(&mut self, blockty: BlockType) -> Result<()>
fn translate_if(&mut self, blockty: BlockType) -> Result<()>
fn translate_else(&mut self) -> Result<()>
fn translate_end(&mut self) -> Result<()>
fn translate_br(&mut self, relative_depth: u32) -> Result<()>
fn translate_br_if(&mut self, relative_depth: u32) -> Result<()>
fn translate_br_table(&mut self, targets: BrTable) -> Result<()>  // CRITICAL
fn translate_return(&mut self) -> Result<()>
fn translate_call(&mut self, function_index: u32) -> Result<()>
fn translate_call_indirect(&mut self, ...) -> Result<()>

// Arithmetic
fn translate_i32_add(&mut self) -> Result<()>
fn translate_i64_add(&mut self) -> Result<()>
// ... all arithmetic ops

// Memory
fn translate_i32_load(&mut self, memarg: MemArg) -> Result<()>
fn translate_i64_load(&mut self, memarg: MemArg) -> Result<()>
fn translate_i32_store(&mut self, memarg: MemArg) -> Result<()>
fn translate_i64_store(&mut self, memarg: MemArg) -> Result<()>
// ... all memory ops

// Local/Global
fn translate_local_get(&mut self, local_index: u32) -> Result<()>
fn translate_local_set(&mut self, local_index: u32) -> Result<()>
fn translate_local_tee(&mut self, local_index: u32) -> Result<()>
fn translate_global_get(&mut self, global_index: u32) -> Result<()>
fn translate_global_set(&mut self, global_index: u32) -> Result<()>
```

**Audit Document**: `audit/clif/wasm_to_clif/translator.zig.md`
- Function-by-function coverage
- br_table edge splitting must be 100% covered
- All control flow semantics documented

### 2.4 br_table Implementation

**Source**: `code_translator.rs` lines 445-569
**Target**: Part of `translator.zig`

**Algorithm** (must be ported exactly):
```rust
Operator::BrTable { targets } => {
    let val = state.pop1();  // Pop dispatch index
    let default = targets.default();

    // 1. Compute minimum depth
    let mut min_depth = default;
    for depth in targets.targets() {
        if depth < min_depth { min_depth = depth; }
    }

    // 2. Get return count from min depth
    let jump_args_count = {
        let frame = &control_stack[len - 1 - min_depth];
        if frame.is_loop() { frame.num_param_values() }
        else { frame.num_return_values() }
    };

    // 3. Simple case (no args) or edge splitting (with args)
    if jump_args_count == 0 {
        // Direct jump table
        let mut data = Vec::new();
        for depth in targets.targets() {
            let block = control_stack[len - 1 - depth].br_destination();
            data.push(block);
        }
        let default_block = control_stack[len - 1 - default].br_destination();
        builder.ins().br_table(val, default_block, &data);
    } else {
        // Edge splitting with intermediate blocks
        let mut intermediate_map = HashMap::new();
        let mut intermediates = Vec::new();

        for depth in targets.targets() {
            let intermediate = *intermediate_map
                .entry(depth)
                .or_insert_with(|| {
                    let block = builder.create_block();
                    intermediates.push((depth, block));
                    block
                });
            data.push(intermediate);
        }

        // Emit br_table to intermediates
        builder.ins().br_table(val, default_intermediate, &data);

        // Fill intermediates with jumps to real targets
        for (depth, intermediate) in intermediates {
            builder.switch_to_block(intermediate);
            let real_target = control_stack[len - 1 - depth].br_destination();
            let args = state.peekn(jump_args_count);
            builder.ins().jump(real_target, args);
        }
    }
}
```

**Audit Document**: `audit/clif/wasm_to_clif/br_table.zig.md`
- MUST cover 100% of this algorithm
- Test cases for both simple and edge-split paths
- Verification against Cranelift behavior

### 2.5 Function Translator

**Source**: `translate/func_translator.rs` (400 LOC)
**Target**: `compiler/codegen/native/wasm_to_clif/func_translator.zig`

**Purpose**: Function-level translation orchestration.

**Audit Document**: `audit/clif/wasm_to_clif/func_translator.zig.md`

### 2.6 Phase 2 Task Checklist

- [ ] **2.1** Create `compiler/codegen/native/wasm_to_clif/` directory
- [ ] **2.2** Port `stack.rs` → `stack.zig`
- [ ] **2.3** Create `audit/clif/wasm_to_clif/stack.zig.md`
- [ ] **2.4** Create `value_stack.zig`
- [ ] **2.5** Create `audit/clif/wasm_to_clif/value_stack.zig.md`
- [ ] **2.6** Port `code_translator.rs` control flow → `translator.zig`
- [ ] **2.7** Create `audit/clif/wasm_to_clif/translator.zig.md`
- [ ] **2.8** Implement br_table with edge splitting
- [ ] **2.9** Create `audit/clif/wasm_to_clif/br_table.zig.md`
- [ ] **2.10** Port arithmetic instructions
- [ ] **2.11** Port memory instructions
- [ ] **2.12** Port local/global instructions
- [ ] **2.13** Port `func_translator.rs` → `func_translator.zig`
- [ ] **2.14** Create `audit/clif/wasm_to_clif/func_translator.zig.md`
- [ ] **2.15** Port Cranelift Wasm translation tests
- [ ] **2.16** Test with simple Wasm programs
- [ ] **2.17** Commit: "Port Cranelift Wasm→CLIF translator"

---

## Phase 3: Machine Instruction Framework

**Cranelift Source**: `cranelift/codegen/src/machinst/`
**Cot Target**: `compiler/codegen/native/machinst/`

### 3.1 Machine Instruction Trait

**Source**: `machinst/mod.rs` (1,500 LOC)
**Target**: `compiler/codegen/native/machinst/inst.zig`

**Key Abstractions**:
```rust
pub trait MachInst: Clone + Debug {
    type LabelUse: MachInstLabelUse;

    fn get_operands(&mut self, collector: &mut impl OperandVisitor);
    fn is_move(&self) -> Option<(Writable<Reg>, Reg)>;
    fn is_included_in_clobbers(&self) -> bool;
    fn is_trap(&self) -> bool;
    fn is_args(&self) -> bool;
    fn is_term(&self) -> MachTerminator;
    fn gen_move(to: Writable<Reg>, from: Reg, ty: Type) -> Self;
    fn gen_nop(preferred_size: usize) -> Self;
    // ... etc
}
```

**Audit Document**: `audit/clif/machinst/inst.zig.md`

### 3.2 Virtual Code

**Source**: `machinst/vcode.rs` (2,000 LOC)
**Target**: `compiler/codegen/native/machinst/vcode.zig`

**Purpose**: Pre-regalloc instruction representation.

**Audit Document**: `audit/clif/machinst/vcode.zig.md`

### 3.3 Registers

**Source**: `machinst/reg.rs` (1,200 LOC)
**Target**: `compiler/codegen/native/machinst/reg.zig`

**Key Types**:
```rust
pub struct Reg { /* virtual or physical */ }
pub struct VirtualReg { /* pre-regalloc */ }
pub struct RealReg { /* post-regalloc */ }
pub struct Writable<R> { /* writable register */ }
pub enum RegClass { Int, Float, Vector }
```

**Audit Document**: `audit/clif/machinst/reg.zig.md`

### 3.4 ABI Framework

**Source**: `machinst/abi.rs` (2,500 LOC)
**Target**: `compiler/codegen/native/machinst/abi.zig`

**Purpose**: Calling convention abstraction.

**Audit Document**: `audit/clif/machinst/abi.zig.md`

### 3.5 Code Buffer

**Source**: `machinst/buffer.rs` (1,500 LOC)
**Target**: `compiler/codegen/native/machinst/buffer.zig`

**Purpose**: Accumulate machine code with relocations.

**Audit Document**: `audit/clif/machinst/buffer.zig.md`

### 3.6 Block Ordering

**Source**: `machinst/blockorder.rs` (800 LOC)
**Target**: `compiler/codegen/native/machinst/blockorder.zig`

**Purpose**: Compute optimal block layout.

**Audit Document**: `audit/clif/machinst/blockorder.zig.md`

### 3.7 Lowering Framework

**Source**: `machinst/lower.rs` (1,500 LOC)
**Target**: `compiler/codegen/native/machinst/lower.zig`

**Purpose**: CLIF → MachInst conversion framework.

**Audit Document**: `audit/clif/machinst/lower.zig.md`

### 3.8 Phase 3 Task Checklist

- [ ] **3.1** Create `compiler/codegen/native/machinst/` directory
- [ ] **3.2** Port `mod.rs` → `inst.zig`
- [ ] **3.3** Create `audit/clif/machinst/inst.zig.md`
- [ ] **3.4** Port `vcode.rs` → `vcode.zig`
- [ ] **3.5** Create `audit/clif/machinst/vcode.zig.md`
- [ ] **3.6** Port `reg.rs` → `reg.zig`
- [ ] **3.7** Create `audit/clif/machinst/reg.zig.md`
- [ ] **3.8** Port `abi.rs` → `abi.zig`
- [ ] **3.9** Create `audit/clif/machinst/abi.zig.md`
- [ ] **3.10** Port `buffer.rs` → `buffer.zig`
- [ ] **3.11** Create `audit/clif/machinst/buffer.zig.md`
- [ ] **3.12** Port `blockorder.rs` → `blockorder.zig`
- [ ] **3.13** Create `audit/clif/machinst/blockorder.zig.md`
- [ ] **3.14** Port `lower.rs` → `lower.zig`
- [ ] **3.15** Create `audit/clif/machinst/lower.zig.md`
- [ ] **3.16** Commit: "Port Cranelift machine instruction framework"

---

## Phase 4: ARM64 Backend

**Cranelift Source**: `cranelift/codegen/src/isa/aarch64/`
**Cot Target**: `compiler/codegen/native/isa/aarch64/`

### 4.1 Instruction Definitions

**Source**: `isa/aarch64/inst.rs` (4,500 LOC)
**Target**: `compiler/codegen/native/isa/aarch64/inst.zig`

**All ARM64 instructions** defined as MachInst.

**Audit Document**: `audit/clif/isa/aarch64/inst.zig.md`

### 4.2 Lowering Rules

**Source**: `isa/aarch64/lower.rs` + `lower.isle` (5,000 LOC)
**Target**: `compiler/codegen/native/isa/aarch64/lower.zig`

**CLIF → ARM64 MachInst** conversion rules.

**Audit Document**: `audit/clif/isa/aarch64/lower.zig.md`

### 4.3 Emission

**Source**: `isa/aarch64/emit.rs` (4,000 LOC)
**Target**: `compiler/codegen/native/isa/aarch64/emit.zig`

**ARM64 MachInst → bytes**.

**Audit Document**: `audit/clif/isa/aarch64/emit.zig.md`

### 4.4 ABI

**Source**: `isa/aarch64/abi.rs` (2,500 LOC)
**Target**: `compiler/codegen/native/isa/aarch64/abi.zig`

**ARM64-specific calling conventions**.

**Audit Document**: `audit/clif/isa/aarch64/abi.zig.md`

### 4.5 Phase 4 Task Checklist

- [ ] **4.1** Create `compiler/codegen/native/isa/aarch64/` directory
- [ ] **4.2** Port `inst.rs` → `inst.zig`
- [ ] **4.3** Create `audit/clif/isa/aarch64/inst.zig.md`
- [ ] **4.4** Port `lower.rs` → `lower.zig`
- [ ] **4.5** Create `audit/clif/isa/aarch64/lower.zig.md`
- [ ] **4.6** Port `emit.rs` → `emit.zig`
- [ ] **4.7** Create `audit/clif/isa/aarch64/emit.zig.md`
- [ ] **4.8** Port `abi.rs` → `abi.zig`
- [ ] **4.9** Create `audit/clif/isa/aarch64/abi.zig.md`
- [ ] **4.10** Integration with machinst framework
- [ ] **4.11** Test simple programs on ARM64
- [ ] **4.12** Test control flow on ARM64
- [ ] **4.13** Test function calls on ARM64
- [ ] **4.14** Commit: "Port Cranelift ARM64 backend"

---

## Phase 5: x86-64 Backend

**Cranelift Source**: `cranelift/codegen/src/isa/x64/`
**Cot Target**: `compiler/codegen/native/isa/x64/`

### 5.1 Instruction Definitions

**Source**: `isa/x64/inst.rs` (2,500 LOC)
**Target**: `compiler/codegen/native/isa/x64/inst.zig`

**Audit Document**: `audit/clif/isa/x64/inst.zig.md`

### 5.2 Lowering Rules

**Source**: `isa/x64/lower.rs` + `lower.isle` (2,500 LOC)
**Target**: `compiler/codegen/native/isa/x64/lower.zig`

**Audit Document**: `audit/clif/isa/x64/lower.zig.md`

### 5.3 Emission

**Source**: `isa/x64/emit.rs` (2,000 LOC)
**Target**: `compiler/codegen/native/isa/x64/emit.zig`

**Audit Document**: `audit/clif/isa/x64/emit.zig.md`

### 5.4 ABI

**Source**: `isa/x64/abi.rs` (1,500 LOC)
**Target**: `compiler/codegen/native/isa/x64/abi.zig`

**Audit Document**: `audit/clif/isa/x64/abi.zig.md`

### 5.5 Encoding

**Source**: `isa/x64/encoding/` (1,500 LOC)
**Target**: `compiler/codegen/native/isa/x64/encoding.zig`

**Audit Document**: `audit/clif/isa/x64/encoding.zig.md`

### 5.6 Phase 5 Task Checklist

- [ ] **5.1** Create `compiler/codegen/native/isa/x64/` directory
- [ ] **5.2** Port `inst.rs` → `inst.zig`
- [ ] **5.3** Create `audit/clif/isa/x64/inst.zig.md`
- [ ] **5.4** Port `lower.rs` → `lower.zig`
- [ ] **5.5** Create `audit/clif/isa/x64/lower.zig.md`
- [ ] **5.6** Port `emit.rs` → `emit.zig`
- [ ] **5.7** Create `audit/clif/isa/x64/emit.zig.md`
- [ ] **5.8** Port `abi.rs` → `abi.zig`
- [ ] **5.9** Create `audit/clif/isa/x64/abi.zig.md`
- [ ] **5.10** Port `encoding/` → `encoding.zig`
- [ ] **5.11** Create `audit/clif/isa/x64/encoding.zig.md`
- [ ] **5.12** Test on x86-64 Linux
- [ ] **5.13** Commit: "Port Cranelift x86-64 backend"

---

## Phase 6: Register Allocation

**Source**: `regalloc2` crate
**Cot Target**: `compiler/codegen/native/regalloc/`

### 6.1 Core Allocator

**Target**: `compiler/codegen/native/regalloc/allocator.zig`

**Audit Document**: `audit/clif/regalloc/allocator.zig.md`

### 6.2 Liveness Analysis

**Target**: `compiler/codegen/native/regalloc/liveness.zig`

**Audit Document**: `audit/clif/regalloc/liveness.zig.md`

### 6.3 Interference Graph

**Target**: `compiler/codegen/native/regalloc/interference.zig`

**Audit Document**: `audit/clif/regalloc/interference.zig.md`

### 6.4 Spill Code

**Target**: `compiler/codegen/native/regalloc/spill.zig`

**Audit Document**: `audit/clif/regalloc/spill.zig.md`

### 6.5 Phase 6 Task Checklist

- [ ] **6.1** Create `compiler/codegen/native/regalloc/` directory
- [ ] **6.2** Port regalloc2 core → `allocator.zig`
- [ ] **6.3** Create `audit/clif/regalloc/allocator.zig.md`
- [ ] **6.4** Port liveness → `liveness.zig`
- [ ] **6.5** Create `audit/clif/regalloc/liveness.zig.md`
- [ ] **6.6** Port interference → `interference.zig`
- [ ] **6.7** Create `audit/clif/regalloc/interference.zig.md`
- [ ] **6.8** Port spill code → `spill.zig`
- [ ] **6.9** Create `audit/clif/regalloc/spill.zig.md`
- [ ] **6.10** Integration tests
- [ ] **6.11** Commit: "Port regalloc2 register allocator"

---

## Phase 7: Integration

### 7.1 Driver Integration

Update `compiler/driver.zig` to use new pipeline:

```zig
// NEW native codegen path
const clif = @import("ir/clif/clif.zig");
const wasm_to_clif = @import("codegen/native/wasm_to_clif/wasm_to_clif.zig");
const machinst = @import("codegen/native/machinst/machinst.zig");
const aarch64 = @import("codegen/native/isa/aarch64/aarch64.zig");
const x64 = @import("codegen/native/isa/x64/x64.zig");
const regalloc = @import("codegen/native/regalloc/regalloc.zig");
```

**Audit Document**: `audit/clif/integration/driver.zig.md`

### 7.2 Phase 7 Task Checklist

- [ ] **7.1** Wire CLIF IR into driver
- [ ] **7.2** Wire Wasm→CLIF translator
- [ ] **7.3** Wire lowering to MachInst
- [ ] **7.4** Wire register allocation
- [ ] **7.5** Wire code emission
- [ ] **7.6** Wire object file generation
- [ ] **7.7** End-to-end test: simple function
- [ ] **7.8** End-to-end test: control flow
- [ ] **7.9** End-to-end test: function calls
- [ ] **7.10** End-to-end test: memory operations
- [ ] **7.11** Create `audit/clif/integration/driver.zig.md`
- [ ] **7.12** Commit: "Integrate Cranelift-style native pipeline"

---

## Phase 8: Self-Hosting

### 8.1 Compile Cot with Cot

- [ ] **8.1** Write Cot compiler in Cot (or subset)
- [ ] **8.2** Compile via Wasm→native path
- [ ] **8.3** Verify self-compiled Cot produces correct output
- [ ] **8.4** Document self-hosting achievement

---

## Task Tracking

### Current Status

| Phase | Status | Progress |
|-------|--------|----------|
| 0: Removal | ✅ Complete | 28/28 |
| 1: CLIF IR | Not Started | 0/18 |
| 2: Wasm Translation | Not Started | 0/17 |
| 3: MachInst | Not Started | 0/16 |
| 4: ARM64 | Not Started | 0/14 |
| 5: x86-64 | Not Started | 0/13 |
| 6: Regalloc | Not Started | 0/11 |
| 7: Integration | Not Started | 0/12 |
| 8: Self-Hosting | Not Started | 0/4 |
| **TOTAL** | | **28/133** |

### Estimated LOC

| Phase | Cranelift LOC | Cot Target LOC |
|-------|---------------|----------------|
| 1: CLIF IR | 10,500 | ~8,000 |
| 2: Wasm Translation | 5,800 | ~4,500 |
| 3: MachInst | 12,400 | ~9,000 |
| 4: ARM64 | 20,700 | ~15,000 |
| 5: x86-64 | 10,000 | ~7,500 |
| 6: Regalloc | 5,000 | ~4,000 |
| 7: Integration | - | ~1,000 |
| **TOTAL** | **~64,400** | **~49,000** |

---

## Audit Document Template

Every new file MUST have an audit document with this structure:

```markdown
# [filename].zig - Cranelift Port Audit

## Cranelift Source
- File: `[path/to/cranelift/file.rs]`
- Lines: [start]-[end]
- Commit: [hash]

## Coverage Summary
| Cranelift Function | Cot Function | Status |
|-------------------|--------------|--------|
| fn foo() | fn foo() | ✅ 100% |
| fn bar() | fn bar() | ✅ 100% |

## Function-by-Function Analysis

### [function_name]

**Cranelift** (file.rs:line):
```rust
[original code]
```

**Cot** (file.zig:line):
```zig
[ported code]
```

**Coverage**: 100% - All branches, all edge cases.

**Tests**: [list of test functions]

## Differences from Cranelift
[Document any intentional differences and why]

## Test Coverage
- [ ] Unit tests ported from Cranelift
- [ ] Integration tests
- [ ] Edge case tests
```

---

## Rules

1. **No dead code** - Remove before rewrite
2. **No orphan docs** - Remove audits for removed code
3. **100% coverage** - Every Cranelift function must be ported
4. **Audit trail** - Every new file gets an audit document
5. **Systematic** - Follow phases in order
6. **Test continuously** - Tests must pass at each commit
