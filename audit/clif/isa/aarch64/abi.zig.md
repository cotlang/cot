# AArch64 ABI Audit

**File**: `compiler/codegen/native/isa/aarch64/abi.zig`
**Source**: `cranelift/codegen/src/isa/aarch64/abi.rs`
**LOC**: ~1,700 lines
**Tests**: 8 tests passing

## Overview

This module implements the AArch64 Application Binary Interface (ABI), ported from Cranelift's `abi.rs`. The ABI defines how functions call each other, pass arguments, return values, and manage the call stack according to the AAPCS64 (Procedure Call Standard for the Arm 64-bit Architecture).

## Implementation Status

### Main Types

| Type | Status | Description |
|------|--------|-------------|
| `AArch64MachineDeps` | ✅ Complete | Main ABI implementation struct |
| `CallConv` | ✅ Complete | Calling conventions (SystemV, Apple, Tail, Winch, PreserveAll) |
| `FrameLayout` | ✅ Complete | Stack frame layout computation |
| `ABIArg` / `ABIArgSlot` | ✅ Complete | Argument location tracking |
| `PRegSet` | ✅ Complete | Physical register set bitmap |
| `MachineEnv` | ✅ Complete | Register allocation environment |
| `IsaFlags` / `SettingsFlags` | ✅ Complete | ISA and codegen settings |
| `Signature` | ✅ Complete | Function signature type |

### ABIMachineSpec Methods

| Method | Status | Description |
|--------|--------|-------------|
| `wordBits()` | ✅ Complete | Returns 64 |
| `wordType()` | ✅ Complete | Returns I64 |
| `stackAlign()` | ✅ Complete | Returns 16 (AAPCS64 requirement) |
| `rcForType()` | ✅ Complete | Register class for types |
| `computeArgLocs()` | ✅ Complete | Argument/return location computation |
| `genLoadStack()` | ✅ Complete | Stack load instruction |
| `genStoreStack()` | ✅ Complete | Stack store instruction |
| `genMove()` | ✅ Complete | Register move instruction |
| `genExtend()` | ✅ Complete | Sign/zero extend instruction |
| `genAddImm()` | ✅ Complete | Add with immediate |
| `genStackLowerBoundTrap()` | ✅ Complete | Stack overflow check |
| `genGetStackAddr()` | ✅ Complete | Get stack slot address |
| `getStacklimitReg()` | ✅ Complete | Stack limit register (x16) |
| `genLoadBaseOffset()` | ✅ Complete | Load from base+offset |
| `genStoreBaseOffset()` | ✅ Complete | Store to base+offset |
| `genSpRegAdjust()` | ✅ Complete | SP adjustment |
| `genPrologueFrameSetup()` | ✅ Complete | Prologue generation |
| `genEpilogueFrameRestore()` | ✅ Complete | Epilogue generation |
| `genReturn()` | ✅ Complete | Return instruction |
| `genClobberSave()` | ✅ Complete | Callee-save register save |
| `genClobberRestore()` | ✅ Complete | Callee-save register restore |
| `getNumberOfSpillslotsForValue()` | ✅ Complete | Spill slot count |
| `getRegsClobberedByCall()` | ✅ Complete | Call clobber sets |
| `getExtMode()` | ✅ Complete | Extension mode for CC |
| `computeFrameLayout()` | ✅ Complete | Frame layout computation |
| `retvalTempReg()` | ✅ Complete | Return value temp (x9) |
| `exceptionPayloadRegs()` | ✅ Complete | Exception registers (x0, x1) |
| `selectApiKey()` | ✅ Complete | PAC key selection |
| `genProbestackUnroll()` | ✅ Complete | Unrolled stack probe |
| `genInlineProbestack()` | ⚠️ Partial | Loop version TODO |

### Clobber Register Sets

| Set | Status | Description |
|-----|--------|-------------|
| `DEFAULT_AAPCS_CLOBBERS` | ✅ Complete | x0-x17, v0-v31 |
| `WINCH_CLOBBERS` | ✅ Complete | x0-x17, x19-x27, v0-v31 |
| `ALL_CLOBBERS` | ✅ Complete | x0-x28, v0-v31 |
| `NO_CLOBBERS` | ✅ Complete | Empty set |

### Helper Functions

| Function | Status | Description |
|----------|--------|-------------|
| `alignTo()` | ✅ Complete | Alignment helper |
| `computeClobberSize()` | ✅ Complete | Clobber area size calculation |
| `isRegSavedInPrologue()` | ✅ Complete | Callee-save determination |
| `loadConstant()` | ✅ Complete | Load 64-bit constant via MOVZ/MOVK |
| `createRegEnv()` | ✅ Complete | Machine environment setup |

### BoundedArray Compatibility

A custom `BoundedArray(T, capacity)` type is provided for Zig 0.15 compatibility, as `std.BoundedArray` was removed from the standard library.

## Calling Convention Details

### AAPCS64 (System V)

- **Integer arguments**: x0-x7
- **Float arguments**: v0-v7
- **Struct return pointer**: x8
- **Caller-saved**: x0-x17, v0-v31
- **Callee-saved**: x19-x28, v8-v15
- **Stack alignment**: 16 bytes

### Apple AArch64

- Same as AAPCS64 with minor differences:
  - No alignment requirement for i128 register pairs
  - Stack slots can be < 8 bytes

### Tail Call Convention

- x0 reserved for return area pointer
- x1 reserved for indirect call address
- Arguments start at x2

### Winch (WebAssembly Interpreter)

- Return area pointer in x0 (not x8)
- x19-x27 are caller-saved (not callee-saved)
- x28 is shadow stack pointer (callee-saved)

## Frame Layout

```
┌─────────────────────────────┐ ← Caller's SP
│   Incoming arguments        │
├─────────────────────────────┤
│   FP (x29) / LR (x30)       │ ← 16 bytes (setup area)
├─────────────────────────────┤
│   Callee-saved registers    │ ← clobber_size
├─────────────────────────────┤
│   Fixed frame storage       │ ← fixed_frame_storage_size
├─────────────────────────────┤
│   Stack slots               │ ← stackslots_size
├─────────────────────────────┤
│   Outgoing arguments        │ ← outgoing_args_size
└─────────────────────────────┘ ← SP
```

## Tests

| Test | Status |
|------|--------|
| `AArch64MachineDeps basic` | ✅ Pass |
| `computeClobberSize` | ✅ Pass |
| `isRegSavedInPrologue` | ✅ Pass |
| `alignTo` | ✅ Pass |
| `PRegSet operations` | ✅ Pass |
| `loadConstant` | ✅ Pass |
| `DEFAULT_AAPCS_CLOBBERS` | ✅ Pass |
| `createRegEnv` | ✅ Pass |

## TODO for Full Integration

1. **Task 4.10** Integration with machinst framework:
   - Replace stub types with real CLIF IR types
   - Connect to VCode builder
   - Wire into compilation pipeline
2. **Probestack loop**: Implement `StackProbeLoop` instruction
3. **Pointer Authentication**: Full PACI/AUTIA instruction support
4. **SVE support**: Z-register handling for SVE calling convention

## Cranelift Reference

- `abi.rs`: Main ABI implementation (~1634 lines)
- Key trait: `ABIMachineSpec`
- Key functions:
  - `compute_arg_locs()` - Argument passing rules
  - `gen_prologue_frame_setup()` / `gen_epilogue_frame_restore()`
  - `gen_clobber_save()` / `gen_clobber_restore()`
  - `compute_frame_layout()`
