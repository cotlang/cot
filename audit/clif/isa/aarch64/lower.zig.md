# AArch64 Instruction Lowering Audit

**File**: `compiler/codegen/native/isa/aarch64/lower.zig`
**Source**: `cranelift/codegen/src/isa/aarch64/lower.rs` + `lower.isle`
**LOC**: ~1,800 lines
**Tests**: 4 tests passing

## Overview

This module translates CLIF IR instructions to AArch64 machine instructions. The ISLE pattern-matching rules from Cranelift have been hand-translated to Zig switch statements.

## Implementation Status

### AArch64LowerBackend

The main lowering backend implementing the LowerBackend trait.

| Method | Status | Description |
|--------|--------|-------------|
| `lower()` | ✅ Complete | Main entry point for instruction selection |
| `lowerBranch()` | ✅ Complete | Branch instruction lowering |
| `maybePinnedReg()` | ✅ Complete | Returns pinned register (none for AArch64) |

### Integer Operations

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `iconst` | MOVZ/MOVK | ✅ Basic (TODO: multi-word constants) |
| `iadd` | ADD/ADD imm12 | ✅ Complete |
| `isub` | SUB | ✅ Complete |
| `ineg` | SUB (from XZR) | ✅ Complete |
| `imul` | MADD | ✅ Complete |
| `udiv` | UDIV | ✅ Complete |
| `sdiv` | SDIV | ✅ Complete |
| `urem` | UDIV + MSUB | ✅ Complete |
| `srem` | SDIV + MSUB | ✅ Complete |

### Bitwise Operations

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `band` | AND | ✅ Complete |
| `bor` | ORR | ✅ Complete |
| `bxor` | EOR | ✅ Complete |
| `bnot` | ORN (from XZR) | ✅ Complete |
| `ishl` | LSL | ✅ Complete |
| `ushr` | LSR | ✅ Complete |
| `sshr` | ASR | ✅ Complete |
| `rotl` | SUB + ROR | ✅ Complete |
| `rotr` | ROR (via EXTR) | ✅ Complete |

### Comparison Operations

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `icmp` | SUBS + CSET | ✅ Basic (TODO: extract condition code) |

### Floating Point Operations

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `f32const` | Placeholder | ⚠️ TODO: constant pool |
| `f64const` | Placeholder | ⚠️ TODO: constant pool |
| `fadd` | FADD | ✅ Complete |
| `fsub` | FSUB | ✅ Complete |
| `fmul` | FMUL | ✅ Complete |
| `fdiv` | FDIV | ✅ Complete |
| `fneg` | FNEG | ✅ Complete |
| `fabs` | FABS | ✅ Complete |
| `sqrt` | FSQRT | ✅ Complete |
| `fcmp` | FCMP + CSET | ✅ Basic (TODO: NaN handling) |

### Type Conversions

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `uextend` | UXTB/UXTH/UXTW | ✅ Complete |
| `sextend` | SXTB/SXTH/SXTW | ✅ Complete |
| `ireduce` | MOV (high bits ignored) | ✅ Complete |
| `fcvt_to_sint` | FCVTZS | ✅ Complete |
| `fcvt_to_uint` | FCVTZU | ✅ Complete |
| `fcvt_from_sint` | SCVTF | ✅ Complete |
| `fcvt_from_uint` | UCVTF | ✅ Complete |
| `fpromote` | FCVT (S→D) | ✅ Complete |
| `fdemote` | FCVT (D→S) | ✅ Complete |

### Memory Operations

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `load` | LDR variants | ✅ Basic (register addressing) |
| `store` | STR variants | ✅ Basic (register addressing) |
| `stack_load` | LDR [SP, #off] | ✅ Placeholder |
| `stack_store` | STR [SP, #off] | ✅ Placeholder |

### Control Flow

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `select` | SUBS + CSEL | ✅ Complete |
| `copy` | MOV | ✅ Complete |
| `jump` | B | ✅ Complete |
| `brif` | SUBS + B.cond | ✅ Complete |
| `br_table` | - | ❌ Not implemented |
| `return` | RET | ✅ Complete |

### Calls

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `call` | BL | ✅ Placeholder (TODO: ABI) |
| `call_indirect` | BLR | ✅ Placeholder (TODO: ABI) |

### Traps

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `trap` | UDF | ✅ Complete |
| `trapnz` | SUBS + conditional UDF | ✅ Complete |
| `trapz` | SUBS + conditional UDF | ✅ Complete |

### Other

| CLIF Opcode | ARM64 Instruction | Status |
|-------------|-------------------|--------|
| `func_addr` | ADR | ✅ Placeholder |
| `nop` | (no output) | ✅ Complete |

## ISLE Pattern Coverage

The following ISLE patterns from `lower.isle` have been translated:

### Implemented Patterns
- `iadd_base_case` - Register-register add
- `iadd_imm12_right` - Add with immediate (partial)
- Basic patterns for all arithmetic ops
- FPU operation patterns

### Not Yet Implemented
- `iadd_imm12_left` - Commutative immediate optimization
- `iadd_imm12_neg` - Subtract via negated add immediate
- `iadd_extend` - Add with extended register
- `iadd_ishl` - Add with shifted operand
- `iadd_imul` - Fused multiply-add optimization
- Vector/SIMD patterns
- i128 patterns

## Stub Types

The module is self-contained with stub types for:
- `Opcode` - CLIF opcodes
- `ClifType` - CLIF types
- `ClifInst`, `Value` - CLIF IR references
- `LowerCtx` - Lowering context
- `ValueRegs`, `InstOutput` - Register tracking

These stubs allow independent compilation and testing. Integration with the machinst framework (Task 4.10) will replace these with real types.

## Tests

| Test | Status |
|------|--------|
| `AArch64LowerBackend basic` | ✅ Pass |
| `operandSizeFromType` | ✅ Pass |
| `scalarSizeFromType` | ✅ Pass |
| `condFromIntCC` | ✅ Pass |

## TODO for Full Integration

1. **Task 4.5** ✅ Create this audit document
2. **Task 4.8** Port `abi.rs` → `abi.zig` for call/return handling
3. **Task 4.10** Integration with machinst framework:
   - Replace stub types with real CLIF IR types
   - Connect to VCode builder
   - Wire into compilation pipeline
4. Implement missing ISLE optimizations
5. Add br_table support
6. Add FP constant pool loading
7. Complete condition code extraction from icmp/fcmp

## Cranelift Reference

- `lower.rs`: Entry point, calls into ISLE-generated code
- `lower.isle`: ~3,000 lines of pattern-matching rules
- Key functions: `isle::lower()`, `isle::lower_branch()`
