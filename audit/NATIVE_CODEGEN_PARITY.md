# Native Codegen Parity Audit

This document tracks parity between Cot's native codegen and Cranelift's implementation.

**Cranelift Repository:** https://github.com/bytecodealliance/wasmtime/tree/main/cranelift/codegen

## Reference Architecture

```
Cranelift Pipeline:
  CLIF IR → Lower (ISLE rules) → VCode → RegAlloc → Emit → Machine Code

Cranelift Source Structure:
  cranelift/codegen/src/
  ├── machinst/           # Generic machine instruction infrastructure
  │   ├── mod.rs          # Module root
  │   ├── lower.rs        # Generic lowering context
  │   ├── vcode.rs        # Virtual code container
  │   ├── abi.rs          # ABI trait definitions
  │   ├── blockorder.rs   # Block layout, critical edge splitting
  │   ├── buffer.rs       # Code buffer for emission
  │   └── reg.rs          # Register abstractions
  ├── isa/
  │   ├── aarch64/
  │   │   ├── lower.rs    # AArch64-specific lowering
  │   │   ├── lower.isle  # ISLE instruction selection rules
  │   │   ├── inst.isle   # Machine instruction definitions
  │   │   ├── abi.rs      # AArch64 ABI implementation
  │   │   └── inst/       # Instruction encoding
  │   └── x64/
  │       ├── lower.rs    # x64-specific lowering
  │       ├── lower.isle  # ISLE rules
  │       ├── abi.rs      # x64 ABI (SysV, Windows)
  │       └── inst/       # Instruction encoding
  └── ir/                 # CLIF IR definitions
```

---

## Parity Checklist

### machinst/lower.rs → machinst/lower.zig

| Component | Cranelift | Cot Status | Notes |
|-----------|-----------|------------|-------|
| Lower struct | `Lower<'func, I>` | ✅ `Lower(comptime I)` | Generic over instruction type |
| LowerCtx trait | `LowerCtx` trait | ✅ `LowerCtx` methods | Implemented as struct methods |
| Value use tracking | `ValueUseState` | ✅ Complete | Tracks single/multiple uses |
| Instruction sinking | `sink_inst()` | ✅ Complete | Sinks unused instructions |
| Constant extraction | `get_constant()` | ✅ Complete | Extracts immediates |
| Block lowering | `lower_clif_block()` | ✅ Complete | Iterates instructions |
| Branch lowering | `lower_clif_branch()` | ⚠️ PANICS | Panics if backend returns null |
| genArgSetup | `gen_arg_setup()` | ❌ STUB | Returns empty |
| genReturn | `gen_return()` | ❌ STUB | Returns empty |

**Evidence from Cranelift lower.rs:**
```rust
// cranelift/codegen/src/machinst/lower.rs
pub fn lower_clif_branch<B: LowerBackend<MInst = I>>(
    &mut self,
    backend: &B,
    block: Block,
    branch: Inst,
    targets: &[MachLabel],
) {
    self.cur_inst = Some(branch);
    backend.lower_branch(self, branch, targets);
    // ... finish up
}
```

---

### machinst/abi.rs → machinst/abi.zig

| Component | Cranelift | Cot Status | Notes |
|-----------|-----------|------------|-------|
| ABIMachineSpec trait | Full trait definition | ✅ Callee struct | Adapted to Zig |
| Signature handling | `Sig`, `SigData` | ✅ Complete | |
| Argument layout | `compute_arg_locs` | ✅ In ISA abi.zig | Per-backend |
| genPrologue | `gen_prologue_frame_setup` | ❌ STUB | Returns empty vec |
| genEpilogue | `gen_epilogue_frame_restore` | ❌ STUB | Returns empty vec |
| genSpill | `gen_spill` | ❌ PANICS | Unimplemented |
| genReload | `gen_reload` | ❌ PANICS | Unimplemented |
| Frame layout | `FrameLayout` | ✅ Complete | |

**Evidence from Cranelift abi.rs:**
```rust
// cranelift/codegen/src/machinst/abi.rs
pub trait ABIMachineSpec {
    fn gen_prologue_frame_setup(&self, ...) -> SmallInstVec<Self::I>;
    fn gen_epilogue_frame_restore(&self, ...) -> SmallInstVec<Self::I>;
    fn gen_spill(&self, to_slot: SpillSlot, from_reg: RealReg) -> Self::I;
    fn gen_reload(&self, to_reg: Writable<RealReg>, from_slot: SpillSlot) -> Self::I;
}
```

---

### isa/aarch64/lower.rs → isa/aarch64/lower.zig

| Opcode | Cranelift ISLE Rule | Cot Status | Notes |
|--------|---------------------|------------|-------|
| iconst | `(lower (iconst ...))` | ✅ Complete | Multi-instruction for large values |
| iadd | `(lower (iadd ...))` | ✅ Complete | Imm12 optimization |
| isub | `(lower (isub ...))` | ✅ Complete | |
| imul | `(lower (imul ...))` | ✅ Complete | |
| udiv/sdiv | `(lower (udiv/sdiv ...))` | ✅ Complete | |
| band/bor/bxor | `(lower (band/bor/bxor ...))` | ✅ Complete | |
| ishl/ushr/sshr | `(lower (ishl/ushr/sshr ...))` | ✅ Complete | |
| icmp | `(lower (icmp cc ...))` | ⚠️ HARDCODED | Always uses .eq condition |
| fcmp | `(lower (fcmp cc ...))` | ⚠️ HARDCODED | Always uses .eq condition |
| load | `(lower (load ...))` | ✅ Complete | |
| store | `(lower (store ...))` | ✅ Complete | |
| jump | `(lower_branch (jump ...))` | ✅ Complete | |
| brif | `(lower_branch (brif ...))` | ✅ Complete | |
| br_table | `(lower_branch (br_table ...))` | ❌ RETURNS NULL | Emit code exists but not wired |
| return | `(lower_branch (return ...))` | ✅ Complete | |
| call | `(lower (call ...))` | ❌ STUB | Hardcoded BlockIndex(0) |
| stack_load | `(lower (stack_load ...))` | ⚠️ HARDCODED | Offset always 0 |
| stack_store | `(lower (stack_store ...))` | ⚠️ HARDCODED | Offset always 0 |

**Evidence - Cranelift br_table handling:**
```rust
// cranelift/codegen/src/isa/aarch64/inst.isle
;; JTSequence: A compound instruction for jump tables
(JTSequence
  (default MachLabel)
  (targets BoxVecMachLabel)
  (ridx Reg)
  (rtmp1 WritableReg)
  (rtmp2 WritableReg))
```

**Evidence - Cranelift icmp handling:**
```lisp
;; cranelift/codegen/src/isa/aarch64/lower.isle
(rule (lower (icmp cc x @ (value_type (fits_in_64 ty)) y))
      (cmp_and_choose cc ty x y))
```
Note: Cranelift extracts `cc` (condition code) from the instruction.

---

### isa/x64/lower.rs → isa/x64/lower.zig

| Opcode | Cranelift | Cot Status | Notes |
|--------|-----------|------------|-------|
| iconst | ✅ | ✅ Complete | |
| iadd/isub | ✅ | ✅ Complete | |
| imul | ✅ | ⚠️ WRONG OPCODE | Uses .add placeholder |
| icmp | ✅ | ⚠️ HARDCODED | Always uses .z condition |
| br_table | ✅ | ❌ RETURNS NULL | |
| call | ✅ | ❌ RETURNS NULL | |

---

### frontend/ → frontend/

| Component | Cranelift | Cot Status | Notes |
|-----------|-----------|------------|-------|
| FunctionBuilder | ✅ | ✅ Complete | |
| SSABuilder | ✅ | ✅ Complete | Braun algorithm |
| Variable tracking | ✅ | ✅ Complete | |
| FuncInstBuilder.iconst | ✅ | ✅ Complete | Stores imm in InstData |
| FuncInstBuilder.iadd | ✅ | ✅ Complete | Stores args |
| FuncInstBuilder.icmp | ✅ | ✅ Complete | Stores cc in imm field |
| FuncInstBuilder.load | ✅ | ⚠️ DISCARDS FLAGS | flags param unused |
| FuncInstBuilder.brif | ✅ | ⚠️ DISCARDS ARGS | then_args/else_args unused |

---

## Gap Analysis Summary

### CRITICAL (Blocking return 42):

1. **br_table successor computation broken**
   - Cranelift: `visit_block_succs` extracts jump table from DFG
   - Cot: `InstData.getBrTableData()` returns null because it can't access Function
   - Root cause: `blockorder.zig:339` calls `inst_data.getBrTableData()` which always returns null
   - Fix: In `visitBlockSuccs`, extract JT index from `inst_data.imm`, look up in `func.dfg.jump_tables`
   - Location: `compiler/codegen/native/machinst/blockorder.zig:337-346`

2. **br_table lowering returns null**
   - Cranelift: Uses JTSequence (aarch64) / jmp_table_seq (x64)
   - Cot: Emit code exists at emit.zig:2513 but lowering returns null
   - Dependency: Requires fix #1 first (so targets are populated)
   - Fix: Wire lower.zig br_table case to generate jt_sequence
   - Location: `compiler/codegen/native/isa/aarch64/lower.zig:263-266`

3. **icmp condition code hardcoded**
   - Cranelift: Extracts cc from instruction via ISLE pattern matching
   - Cot: Hardcodes `.eq` condition
   - Fix: Extract condition from InstData.imm field (already stored by frontend.zig:688-693)

### HIGH (Needed for function calls):

3. **Call lowering is stub**
   - Cranelift: Full ABI handling via ABIMachineSpec trait
   - Cot: Returns stub with hardcoded BlockIndex(0)
   - Fix: Wire to ABI computeArgLocs, emit argument moves, call instruction

4. **Stack slot offsets hardcoded to 0**
   - Cranelift: Extracts slot index, computes offset from frame layout
   - Cot: Always uses offset=0
   - Fix: Extract slot from InstData, compute byte offset

### MEDIUM (Needed for complex functions):

5. **Prologue/epilogue generation stubs**
   - Cranelift: gen_prologue_frame_setup, gen_epilogue_frame_restore
   - Cot: Returns empty instruction vectors
   - Fix: Implement frame setup/teardown per ABI

6. **Spill/reload panics**
   - Cranelift: gen_spill, gen_reload methods
   - Cot: Panics with "not implemented"
   - Fix: Implement stack spill/reload instructions

---

## Fix Tracking

| # | Gap | Status | Commit |
|---|-----|--------|--------|
| 1 | br_table lowering | ⬜ TODO | |
| 2 | icmp condition code | ⬜ TODO | |
| 3 | Call lowering | ⬜ TODO | |
| 4 | Stack slot offsets | ⬜ TODO | |
| 5 | Prologue/epilogue | ⬜ TODO | |
| 6 | Spill/reload | ⬜ TODO | |

---

## References

- Cranelift Repository: https://github.com/bytecodealliance/wasmtime/tree/main/cranelift
- ISLE Language Reference: https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/isle/docs/language-reference.md
- Cranelift Codegen Primer: https://bouvier.cc/2021/02/17/cranelift-codegen-primer/
- ISLE Blog Post: https://cfallin.org/blog/2023/01/20/cranelift-isle/
