# AArch64 Instruction Module Audit

**Source:** `cranelift/codegen/src/isa/aarch64/inst/`

## ⚠️ MANDATORY COMPLETION NOTICE ⚠️

**Current Status: 50% ported. After Task 4.10, ALL deferred items MUST be completed.**

This module has deferred functionality that MUST be implemented for 100% coverage.
See `CRANELIFT_PORT_MASTER_PLAN.md` Tasks 4.15-4.23 for the mandatory completion checklist.

**DO NOT consider ARM64 backend complete until:**
- [ ] All 8 deferred items are implemented (Tasks 4.15-4.22)
- [ ] Final verification passes (Task 4.23)
- [ ] The `else => BRK` fallback is removed from emit.zig
- [ ] Coverage reaches 100%

---

## Overview

This module implements AArch64 (ARM64) machine instruction types, immediate encodings, register utilities, argument types, and binary code emission. This is Phase 4 Tasks 4.2 and 4.6 of the Cranelift port.

## Files Ported

| Cranelift File | Zig File | Rust Lines | Zig Lines | Coverage |
|----------------|----------|------------|-----------|----------|
| args.rs | args.zig | 726 | 790 | 109% |
| imms.rs | imms.zig | 1,242 | 806 | 65% |
| regs.rs | regs.zig | 281 | 297 | 106% |
| mod.rs | mod.zig | 3,114 | 1,166 | 37% |
| emit.rs | emit.zig | 3,687 | 1,424 | 39% |
| **Total** | | **9,050** | **4,483** | **50%** |

**Note:**
- mod.rs is partially ported - instruction enum variants and basic operations ported, but `aarch64_get_operands` (800+ lines) and `print_with_state` (1500+ lines) deferred.
- emit.rs is partially ported - core encoding helpers and emission for ALU, load/store, branch, and FPU instructions. Vector operations and complex sequences deferred.

## Types Ported

### From args.rs (args.zig)

| Cranelift Type | Zig Type | Status |
|----------------|----------|--------|
| `ShiftOp` | `ShiftOp` | ✅ Complete |
| `ShiftOpShiftImm` | `ShiftOpShiftImm` | ✅ Complete |
| `ShiftOpAndAmt` | `ShiftOpAndAmt` | ✅ Complete |
| `ExtendOp` | `ExtendOp` | ✅ Complete |
| `MemLabel` | `MemLabel` | ✅ Complete |
| `Cond` | `Cond` | ✅ Complete |
| `CondBrKind` | `CondBrKind` | ✅ Complete |
| `BranchTarget` | `BranchTarget` | ✅ Complete |
| `OperandSize` | `OperandSize` | ✅ Complete |
| `ScalarSize` | `ScalarSize` | ✅ Complete |
| `VectorSize` | `VectorSize` | ✅ Complete |
| `APIKey` | `APIKey` | ✅ Complete |
| `TestBitAndBranchKind` | `TestBitAndBranchKind` | ✅ Complete |
| `BranchTargetType` | `BranchTargetType` | ✅ Complete |

### From imms.rs (imms.zig)

| Cranelift Type | Zig Type | Status |
|----------------|----------|--------|
| `NZCV` | `NZCV` | ✅ Complete |
| `UImm5` | `UImm5` | ✅ Complete |
| `SImm7Scaled` | `SImm7Scaled` | ✅ Complete |
| `FPULeftShiftImm` | `FPULeftShiftImm` | ✅ Complete |
| `FPURightShiftImm` | `FPURightShiftImm` | ✅ Complete |
| `SImm9` | `SImm9` | ✅ Complete |
| `UImm12Scaled` | `UImm12Scaled` | ✅ Complete |
| `Imm12` | `Imm12` | ✅ Complete |
| `ImmLogic` | `ImmLogic` | ✅ Complete |
| `ImmShift` | `ImmShift` | ✅ Complete |
| `MoveWideConst` | `MoveWideConst` | ✅ Complete |
| `ASIMDMovModImm` | `ASIMDMovModImm` | ✅ Complete |
| `ASIMDFPModImm` | `ASIMDFPModImm` | ✅ Complete |

### From regs.rs (regs.zig)

| Cranelift Function | Zig Function | Status |
|--------------------|--------------|--------|
| `xreg()` | `xreg()` | ✅ Complete |
| `vreg()` | `vreg()` | ✅ Complete |
| `zero_reg()` | `zeroReg()` | ✅ Complete |
| `stack_reg()` | `stackReg()` | ✅ Complete |
| `link_reg()` | `linkReg()` | ✅ Complete |
| `fp_reg()` | `fpReg()` | ✅ Complete |
| `spilltmp_reg()` | `spilltmpReg()` | ✅ Complete |
| `tmp2_reg()` | `tmp2Reg()` | ✅ Complete |
| `pinned_reg()` | `pinnedReg()` | ✅ Complete |
| Pretty-print functions | Various | ✅ Stub (TODO: full impl) |

### From mod.rs (mod.zig)

| Cranelift Type | Zig Type | Status |
|----------------|----------|--------|
| `ALUOp` | `ALUOp` | ✅ Complete |
| `ALUOp3` | `ALUOp3` | ✅ Complete |
| `BitOp` | `BitOp` | ✅ Complete |
| `FPUOp1` | `FPUOp1` | ✅ Complete |
| `FPUOp2` | `FPUOp2` | ✅ Complete |
| `FPUOp3` | `FPUOp3` | ✅ Complete |
| `FpuRoundMode` | `FpuRoundMode` | ✅ Complete |
| `FpuToIntOp` | `FpuToIntOp` | ✅ Complete |
| `IntToFpuOp` | `IntToFpuOp` | ✅ Complete |
| `MoveWideOp` | `MoveWideOp` | ✅ Complete |
| `AtomicRMWOp` | `AtomicRMWOp` | ✅ Complete |
| `AtomicRMWLoopOp` | `AtomicRMWLoopOp` | ✅ Complete |
| `VecALUOp` | `VecALUOp` | ✅ Complete |
| `VecMisc2` | `VecMisc2` | ✅ Complete |
| `AMode` | `AMode` | ✅ Complete |
| `PairAMode` | `PairAMode` | ✅ Complete |
| `Inst` | `Inst` | ✅ Partial (core variants) |

### From emit.rs (emit.zig)

| Cranelift Function | Zig Function | Status |
|--------------------|--------------|--------|
| `machreg_to_gpr()` | `machregToGpr()` | ✅ Complete |
| `machreg_to_vec()` | `machregToVec()` | ✅ Complete |
| `enc_arith_rrr()` | `encArithRrr()` | ✅ Complete |
| `enc_arith_rr_imm12()` | `encArithRrImm12()` | ✅ Complete |
| `enc_arith_rr_imml()` | `encArithRrImml()` | ✅ Complete |
| `enc_arith_rrrr()` | `encArithRrrr()` | ✅ Complete |
| `enc_jump26()` | `encJump26()` | ✅ Complete |
| `enc_conditional_br()` | `encConditionalBr()` | ✅ Complete |
| `enc_move_wide()` | `encMoveWide()` | ✅ Complete |
| `enc_movk()` | `encMovk()` | ✅ Complete |
| `enc_ldst_pair()` | `encLdstPair()` | ✅ Complete |
| `enc_ldst_simm9()` | `encLdstSimm9()` | ✅ Complete |
| `enc_ldst_uimm12()` | `encLdstUimm12()` | ✅ Complete |
| `enc_ldst_reg()` | `encLdstReg()` | ✅ Complete |
| `enc_ldst_imm19()` | `encLdstImm19()` | ✅ Complete |
| `enc_bit_rr()` | `encBitRr()` | ✅ Complete |
| `enc_br()` | `encBr()` | ✅ Complete |
| `enc_adr()` | `encAdr()` | ✅ Complete |
| `enc_adrp()` | `encAdrp()` | ✅ Complete |
| `enc_csel()` | `encCsel()` | ✅ Complete |
| `enc_fcsel()` | `encFcsel()` | ✅ Complete |
| `enc_fcmp()` | `encFcmp()` | ✅ Complete |
| `enc_bfm()` | `encBfm()` | ✅ Complete |
| `enc_vecmov()` | `encVecmov()` | ✅ Complete |
| `enc_fpu*()` | `encFpu*()` | ✅ Complete |
| `enc_vec_rrr()` | `encVecRrr()` | ✅ Complete |
| `enc_dmb_ish()` | `encDmbIsh()` | ✅ Complete |
| `MachBuffer` | `MachBuffer` | ✅ Complete (basic) |
| `EmitState` | `EmitState` | ✅ Complete (basic) |
| `EmitInfo` | `EmitInfo` | ✅ Complete |
| `emit()` | `emit()` | ⏳ Partial |

**Instruction Emission Coverage:**

| Category | Status | Notes |
|----------|--------|-------|
| ALU RRR | ✅ Complete | ADD, SUB, ORR, AND, EOR, etc. |
| ALU RRRR | ✅ Complete | MADD, MSUB, UMADDL, SMADDL |
| ALU RR Imm12 | ✅ Complete | ADD/SUB with 12-bit immediate |
| ALU RR ImmLogic | ✅ Complete | ORR/AND/EOR with logical immediate |
| ALU RR ImmShift | ✅ Complete | LSL, LSR, ASR with immediate |
| ALU RRR Shift | ✅ Complete | Operations with shifted register |
| ALU RRR Extend | ✅ Complete | Operations with extended register |
| Bit Operations | ✅ Complete | CLZ, CLS, RBIT, REV |
| Move Wide | ✅ Complete | MOVZ, MOVN, MOVK |
| Move | ✅ Complete | Register moves, mov_from_preg, mov_to_preg |
| Conditional Select | ✅ Complete | CSEL, CSNEG, CSET, CSETM |
| Conditional Compare | ✅ Complete | CCMP, CCMP_imm |
| Extend | ✅ Complete | Sign/zero extension |
| Loads | ✅ Complete | LDRB/H/W/X signed/unsigned |
| Stores | ✅ Complete | STRB/H/W/X |
| Load/Store Pairs | ✅ Complete | LDP, STP (64-bit) |
| Load Address | ✅ Complete | LEA-like operations |
| Branches | ✅ Complete | B, BL, B.cond, RET, BR, BLR |
| Test & Branch | ✅ Complete | TBZ, TBNZ |
| ADR/ADRP | ✅ Complete | PC-relative addressing |
| FPU Moves | ✅ Complete | FMOV (32/64/128) |
| FPU Compare | ✅ Complete | FCMP |
| FPU Unary | ✅ Complete | FABS, FNEG, FSQRT, FCVT |
| FPU Binary | ✅ Complete | FADD, FSUB, FMUL, FDIV, FMAX, FMIN |
| FPU Ternary | ✅ Complete | FMADD, FMSUB |
| FPU Rounding | ✅ Complete | FRINTN, FRINTP, FRINTM, FRINTZ |
| FPU↔Int Convert | ✅ Complete | FCVTZS/U, SCVTF, UCVTF |
| FPU Load/Store | ✅ Complete | LDR/STR (32/64/128-bit FP) |
| NZCV Moves | ✅ Complete | MSR/MRS for NZCV |
| Fence | ✅ Complete | DMB ISH |
| BTI | ✅ Complete | Branch target identification |
| Nop/Brk/Udf | ✅ Complete | Debug/trap instructions |
| Trap If | ✅ Complete | Conditional trap |
| Atomics | ⏳ Deferred | AtomicRMW, CAS loops |
| Vector ALU | ⏳ Deferred | SIMD operations |
| Jump Tables | ⏳ Deferred | JTSequence |
| External Names | ⏳ Deferred | LoadExtNameGot, etc. |

## Test Coverage

| File | Tests |
|------|-------|
| args.zig | 9 |
| imms.zig | 7 |
| regs.zig | 4 |
| mod.zig | 4 |
| emit.zig | 4 |
| aarch64/mod.zig | 1 |
| **Total** | **28** |

## What's Deferred

**⚠️ CRITICAL: All items below MUST be completed after Task 4.10 ⚠️**

See `CRANELIFT_PORT_MASTER_PLAN.md` Tasks 4.15-4.23 for mandatory completion checklist.
These are NOT optional deferrals - they MUST be implemented for 100% coverage.

### 1. Atomic Operations (AtomicRMW, CAS loops) → Task 4.15
- **Why deferred:** Atomics require complex multi-instruction sequences with loops (ldaxr/stlxr). The Wasm backend doesn't use atomics yet (single-threaded model).
- **When to implement:** MANDATORY after Task 4.10. See Task 4.15 checklist.

### 2. Vector/SIMD Operations (VecALUOp, VecMisc2) → Task 4.16
- **Why deferred:** SIMD requires ~50 additional instruction variants. The core AOT path uses scalar operations only.
- **When to implement:** MANDATORY after Task 4.10. See Task 4.16 checklist (17 sub-items).

### 3. Jump Tables (JTSequence) → Task 4.17
- **Why deferred:** Jump tables are a complex multi-instruction pseudo-op requiring label management. Switch statements can use if/else chains initially.
- **When to implement:** MANDATORY after Task 4.10. See Task 4.17 checklist.

### 4. External Name Loading (LoadExtNameGot, LoadExtNameNear, LoadExtNameFar) → Task 4.18
- **Why deferred:** These handle dynamic linking relocations. Initial AOT produces static executables.
- **When to implement:** MANDATORY after Task 4.10. See Task 4.18 checklist.

### 5. mem_finalize() - Address Mode Finalization → Task 4.19
- **Why deferred:** Converts pseudo-modes (SPOffset, FPOffset) to real modes using spilltmp register. Requires frame layout integration.
- **When to implement:** MANDATORY after Task 4.10. See Task 4.19 checklist.

### 6. aarch64_get_operands() - Register Operand Collection → Task 4.20
- **Why deferred:** 800+ lines collecting register uses/defs for regalloc. Not needed until register allocator is wired in.
- **When to implement:** MANDATORY after Task 4.10. See Task 4.20 checklist.

### 7. print_with_state() - Pretty Printing → Task 4.21
- **Why deferred:** 1500+ lines for disassembly output. Required for debugging native code issues.
- **When to implement:** MANDATORY after Task 4.10. See Task 4.21 checklist.

### 8. emit_tests.rs (7,972 lines) → Task 4.22
- **Why deferred:** Comprehensive emission tests. The encoding helpers have basic tests; full coverage requires more infrastructure.
- **When to implement:** MANDATORY after Task 4.10. See Task 4.22 checklist.

## Key Algorithms

### ImmLogic Encoding (VIXL port)

The `ImmLogic.maybeFromU64()` function implements the complex algorithm for encoding AArch64 logical immediates. This is a direct port of VIXL's `Assembler::IsImmLogical`.

Key insight: AArch64 logical immediates are repeating bit patterns that can be encoded with just 13 bits (N, R, S).

### MoveWideConst

`MoveWideConst.maybeFromU64()` determines if a 64-bit constant can be loaded with a single MOVZ/MOVN instruction by checking if only one 16-bit chunk is non-zero.

### Instruction Encoding Helpers

The emit module contains ~30 encoding helper functions that construct the 32-bit instruction words according to the ARM64 instruction encoding specification. These handle:
- Register field placement (Rd, Rn, Rm positions)
- Immediate field encoding (simm9, uimm12, imm19, imm26)
- Size bit setting (sf for 32/64-bit)
- Opcode bit patterns for each instruction class

## Dependencies

Currently uses stub types for:
- `Reg`, `PReg`, `VReg`, `RegClass` - Will integrate with machinst when wired in
- `Type` - Will integrate with CLIF IR types
- `MachLabel` - Will integrate with MachBuffer

These stubs are in `args.zig` and will be replaced with proper imports when the backend is wired into the compiler (Task 4.10).
