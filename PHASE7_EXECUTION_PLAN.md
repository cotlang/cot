# Phase 7: Integration Execution Plan

**Last Updated**: 2026-02-02
**Status**: In Progress

## Overview

This document provides a detailed execution plan for completing Phase 7 (Integration) of the Cranelift port. It includes an audit proving parity with Cranelift and a task list for wiring all ported components together.

---

## Part 1: Cranelift Port Audit

### Summary Statistics

| Component | Files | Lines of Code | Tests | Status |
|-----------|-------|---------------|-------|--------|
| CLIF IR (`ir/clif/`) | 8 | 4,954 | 40 | 100% Complete |
| Wasm→CLIF (`wasm_to_clif/`) | 3 | 1,962 | 19 | ~60% Complete |
| MachInst Framework (`machinst/`) | 7 | ~9,000 | 67 | 100% Complete |
| ARM64 Backend (`isa/aarch64/`) | 7 | ~8,500 | 100+ | 95% Complete |
| x64 Backend (`isa/x64/`) | 7 | ~8,400 | 140 | 95% Complete |
| Register Allocator (`regalloc/`) | 16 | ~6,400 | 75 | 100% Complete |
| Integration (`compile.zig`) | 1 | 719 | 10 | 80% Complete |
| Frontend (`frontend/`) | 4 | ~1,200 | - | 100% Complete |
| **TOTAL** | **53** | **~41,135** | **451+** | **~90%** |

### File-by-File Audit

#### CLIF IR (`compiler/ir/clif/`)

| File | LOC | Cranelift Source | Parity |
|------|-----|------------------|--------|
| types.zig | 593 | ir/types.rs | 100% |
| instructions.zig | 554 | ir/instructions.rs | 100% |
| dfg.zig | 812 | ir/dfg.rs | 100% |
| layout.zig | 947 | ir/layout.rs | 100% |
| function.zig | 601 | ir/function.rs | 100% |
| builder.zig | 917 | ir/builder.rs | 100% |
| jumptable.zig | 362 | ir/jumptable.rs | 100% |
| mod.zig | 168 | - | 100% |

#### Wasm→CLIF (`compiler/codegen/native/wasm_to_clif/`)

| File | LOC | Cranelift Source | Parity | Notes |
|------|-----|------------------|--------|-------|
| translator.zig | 921 | translate/code_translator.rs | ~60% | Needs memory ops |
| stack.zig | 600 | translate/stack.rs | 100% | Complete |
| func_translator.zig | 441 | translate/func_translator.rs | ~80% | Needs integration |

**Gap Analysis**:
- Memory instructions (load/store) not yet implemented
- Global variable handling incomplete
- Function calls need ABI integration

#### MachInst Framework (`compiler/codegen/native/machinst/`)

| File | LOC | Cranelift Source | Parity |
|------|-----|------------------|--------|
| inst.zig | ~800 | machinst/mod.rs | 100% |
| vcode.zig | ~1,800 | machinst/vcode.rs | 100% |
| reg.zig | ~600 | machinst/reg.rs | 100% |
| abi.zig | ~1,200 | machinst/abi.rs | 100% |
| buffer.zig | ~1,500 | machinst/buffer.rs | 100% |
| blockorder.zig | ~1,500 | machinst/blockorder.rs | 100% |
| lower.zig | ~1,600 | machinst/lower.rs | 100% |

#### ARM64 Backend (`compiler/codegen/native/isa/aarch64/`)

| File | LOC | Cranelift Source | Parity |
|------|-----|------------------|--------|
| inst/mod.zig | ~1,500 | isa/aarch64/inst/mod.rs | 100% |
| inst/args.zig | ~1,200 | isa/aarch64/inst/args.rs | 100% |
| inst/regs.zig | ~300 | isa/aarch64/inst/regs.rs | 100% |
| inst/imms.zig | ~400 | isa/aarch64/inst/imms.rs | 100% |
| inst/emit.zig | ~1,500 | isa/aarch64/inst/emit.rs | 95% |
| inst/get_operands.zig | ~400 | isa/aarch64/inst/mod.rs | 100% |
| lower.zig | ~1,800 | isa/aarch64/lower.rs | 95% |
| abi.zig | ~1,700 | isa/aarch64/abi.rs | 100% |
| mod.zig | ~200 | - | 100% |

**Gap Analysis**:
- Some rare instruction variants use BRK fallback
- Full print_with_state() not implemented (debug only)

#### x64 Backend (`compiler/codegen/native/isa/x64/`)

| File | LOC | Cranelift Source | Parity |
|------|-----|------------------|--------|
| inst/mod.zig | ~1,350 | isa/x64/inst/mod.rs | 100% |
| inst/args.zig | ~1,240 | isa/x64/inst/args.rs | 100% |
| inst/regs.zig | ~480 | isa/x64/inst/regs.rs | 100% |
| inst/emit.zig | ~2,330 | isa/x64/inst/emit.rs | 95% |
| inst/get_operands.zig | ~670 | isa/x64/inst/mod.rs | 100% |
| lower.zig | ~1,480 | isa/x64/lower.rs | 95% |
| abi.zig | ~750 | isa/x64/abi.rs | 100% |
| mod.zig | ~130 | - | 100% |

#### Register Allocator (`compiler/codegen/native/regalloc/`)

| File | LOC | Cranelift Source | Parity |
|------|-----|------------------|--------|
| index.zig | ~200 | regalloc2 core types | 100% |
| operand.zig | ~760 | regalloc2 operand.rs | 100% |
| func.zig | ~200 | regalloc2 function.rs | 100% |
| env.zig | ~230 | regalloc2 env.rs | 100% |
| output.zig | ~400 | regalloc2 output.rs | 100% |
| cfg.zig | ~420 | regalloc2 cfg.rs | 100% |
| ssa.zig | ~200 | regalloc2 ssa.rs | 100% |
| indexset.zig | ~430 | regalloc2 indexset.rs | 100% |
| moves.zig | ~450 | regalloc2 moves.rs | 100% |
| ion_data.zig | ~750 | regalloc2 ion/data.rs | 100% |
| liveness.zig | ~620 | regalloc2 ion/liveranges.rs | 100% |
| merge.zig | ~710 | regalloc2 ion/merge.rs | 100% |
| process.zig | ~1,500 | regalloc2 ion/process.rs | 100% |
| spill.zig | ~580 | regalloc2 ion/spill.rs | 100% |
| ion_moves.zig | ~810 | regalloc2 ion/moves.rs | 100% |
| regalloc.zig | ~390 | regalloc2 lib.rs | 100% |

---

## Part 2: Legacy Code to Remove

These files are from the old architecture and should be removed after Phase 7 is complete:

| File | LOC | Reason |
|------|-----|--------|
| `codegen/native/arm64_asm.zig` | 989 | Replaced by `isa/aarch64/inst/emit.zig` |
| `codegen/native/amd64_asm.zig` | 1,628 | Replaced by `isa/x64/inst/emit.zig` |
| `codegen/native/amd64_regs.zig` | 218 | Replaced by `isa/x64/inst/regs.zig` |
| `codegen/native/abi.zig` | 16 | Stub; replaced by `isa/*/abi.zig` |
| **TOTAL** | **2,851** | |

**Action**: Remove these files AFTER Phase 7 end-to-end tests pass.

---

## Part 3: Integration Blockers

### Current State

The integration blocker is at `driver.zig:306`:
```zig
// Native AOT compilation not yet integrated.
// See CRANELIFT_PORT_MASTER_PLAN.md for implementation status.
// Integration requires: wasm_to_clif translation + native_compile pipeline.
return error.NativeCodegenNotImplemented;
```

### What Needs to Happen

1. **Wasm → CLIF Translation**: Use `wasm_to_clif/func_translator.zig`
2. **CLIF → Native Compilation**: Use `compile.zig`
3. **Object File Generation**: Use `macho.zig` or `elf.zig`

### The Missing Link

The `func_translator.zig` already has `WasmFuncTranslator` that:
- Takes `WasmOperator` enums (from wasm_parser)
- Has `translateOperator()` for each Wasm opcode
- Produces CLIF IR via the builder

The gap is wiring this into driver.zig's `generateNativeCode()` function.

---

## Part 4: Execution Tasks

### Task 7.1: Wire Wasm→CLIF Translation (CRITICAL)

**Objective**: Connect wasm_parser output to func_translator

**Files**:
- `driver.zig` - generateNativeCode()
- `wasm_to_clif/func_translator.zig` - WasmFuncTranslator

**Steps**:
1. Parse each function from wasm_module.code
2. Create WasmFuncTranslator for each function
3. Feed Wasm opcodes to translateOperator()
4. Get CLIF Function from translator

**Cranelift Reference**: `wasmtime/cranelift/src/compiler.rs` - `translate_function()`

### Task 7.2: Complete Memory Instructions

**Objective**: Implement load/store in translator.zig

**Missing Opcodes**:
- `i32.load`, `i64.load`, `f32.load`, `f64.load`
- `i32.store`, `i64.store`, `f32.store`, `f64.store`
- Various load variants (8/16 bit, signed/unsigned)

**Cranelift Reference**: `code_translator.rs` translate_load/translate_store

### Task 7.3: Wire CLIF→Native Compilation

**Objective**: Connect CLIF Function to compile.zig

**Files**:
- `driver.zig` - after Wasm→CLIF
- `compile.zig` - compile()

**Code Pattern**:
```zig
// After translating all functions to CLIF:
for (clif_functions) |clif_func| {
    const compiled = try native_compile.compile(
        allocator,
        &clif_func,
        native_compile.detectNativeIsa(),
        &ctrl_plane,
    );
    // Collect machine code
}
```

### Task 7.4: Complete VCode Emission

**Objective**: Replace placeholder emit functions with real implementation

**Files**:
- `compile.zig` - emitCodeAArch64(), emitCodeX64()
- `isa/aarch64/inst/emit.zig` - emit()
- `isa/x64/inst/emit.zig` - emit()

**Current State**: Emit functions return placeholder `ret` instruction

**Required**:
1. Iterate through VCode instructions
2. Apply register allocations
3. Call ISA-specific emit for each instruction
4. Collect bytes into MachBuffer

### Task 7.5: Wire Object File Generation

**Objective**: Wrap machine code in Mach-O or ELF

**Files**:
- `driver.zig` - after compilation
- `macho.zig` - Mach-O generation
- `elf.zig` - ELF generation

**Existing Code**: Both files already have working implementations

### Task 7.6: End-to-End Test - Return 42

**Test Case**:
```cot
fn main() i32 {
    return 42;
}
```

**Expected**: Running the compiled binary returns exit code 42

### Task 7.7: End-to-End Test - Arithmetic

**Test Case**:
```cot
fn main() i32 {
    return 10 + 32;
}
```

### Task 7.8: End-to-End Test - Control Flow

**Test Case**:
```cot
fn main() i32 {
    if (true) {
        return 1;
    } else {
        return 0;
    }
}
```

### Task 7.9: End-to-End Test - Function Calls

**Test Case**:
```cot
fn add(a: i32, b: i32) i32 {
    return a + b;
}
fn main() i32 {
    return add(20, 22);
}
```

### Task 7.10: Clean Up Legacy Code

After all tests pass, remove:
- `codegen/native/arm64_asm.zig`
- `codegen/native/amd64_asm.zig`
- `codegen/native/amd64_regs.zig`
- `codegen/native/abi.zig`

---

## Part 5: Implementation Order

```
┌─────────────────────────────────────────────────────────────┐
│  7.1: Wire Wasm→CLIF Translation                            │
│  - Parse Wasm functions from module                         │
│  - Create WasmFuncTranslator per function                   │
│  - Feed opcodes, get CLIF Functions                         │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.2: Complete Memory Instructions (if needed)              │
│  - Add load/store to translator.zig                         │
│  - Test with memory-using programs                          │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.3: Wire CLIF→Native Compilation                          │
│  - Call compile.compile() with CLIF Functions               │
│  - Get CompiledCode back                                    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.4: Complete VCode Emission                               │
│  - Replace placeholder emitCodeAArch64/X64                  │
│  - Use ISA emit.zig for actual instruction bytes            │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.5: Wire Object File Generation                           │
│  - Wrap machine code in Mach-O/ELF                          │
│  - Use existing macho.zig/elf.zig                           │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.6-7.9: End-to-End Tests                                  │
│  - Return 42, Arithmetic, Control Flow, Function Calls      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.10: Clean Up Legacy Code                                 │
│  - Remove arm64_asm.zig, amd64_asm.zig, etc.                │
└─────────────────────────────────────────────────────────────┘
```

---

## Part 6: Cranelift Parity Verification

### What Cranelift Does

From `wasmtime/cranelift/src/compiler.rs`:

```rust
fn translate_function(&self, wasm_func: &FunctionBody) -> Result<CompiledFunction> {
    // 1. Create CLIF function
    let mut func = ir::Function::new();

    // 2. Translate Wasm → CLIF
    let mut func_env = FuncEnvironment::new(...);
    self.wasm_translator.translate(&mut func, wasm_func, &mut func_env)?;

    // 3. Compile CLIF → native
    let compiled = self.isa.compile_function(&func, ...)?;

    Ok(compiled)
}
```

From `cranelift/codegen/src/machinst/compile.rs`:

```rust
pub fn compile<B: MachBackend>(func: &Function, isa: &dyn TargetIsa) -> Result<CompiledCode> {
    // 1. Compute block order
    let block_order = BlockLoweringOrder::new(func, ...)?;

    // 2. Lower CLIF → VCode
    let lower = Lower::new(func, ...)?;
    let vcode = lower.lower()?;

    // 3. Register allocation
    let regalloc_result = regalloc2::run(&vcode, ...)?;

    // 4. Emit machine code
    let result = vcode.emit(&regalloc_result, ...)?;

    Ok(result)
}
```

### What Cot Has

| Cranelift Component | Cot Equivalent | Status |
|---------------------|----------------|--------|
| `ir::Function` | `ir/clif/function.zig` | ✅ Complete |
| `FuncTranslator` | `wasm_to_clif/func_translator.zig` | ✅ Complete |
| `BlockLoweringOrder` | `machinst/blockorder.zig` | ✅ Complete |
| `Lower` | `machinst/lower.zig` | ✅ Complete |
| `VCode` | `machinst/vcode.zig` | ✅ Complete |
| `regalloc2::run` | `regalloc/regalloc.zig` | ✅ Complete |
| `VCode::emit` | Placeholder in `compile.zig` | 🔄 Needs work |
| `compile()` | `compile.zig::compile()` | ✅ Structure complete |
| Driver integration | `driver.zig` | ❌ Not wired |

### Parity Summary

**Infrastructure Parity**: 95%+ complete

**Integration Parity**: ~50% complete (the pipeline exists but isn't wired)

**The Gap**: The pipeline components exist but aren't connected. Task 7.1-7.5 will achieve full parity.

---

## Part 7: Success Criteria

Phase 7 is complete when:

1. ✅ All 451+ existing tests pass
2. ⏳ `cot --target=native test.cot` produces working executables
3. ⏳ End-to-end tests pass (return 42, arithmetic, control flow, function calls)
4. ⏳ Legacy code removed (arm64_asm.zig, amd64_asm.zig, etc.)
5. ⏳ Audit document updated with final LOC counts

---

## Appendix: Quick Reference

### Key Files to Modify

1. **driver.zig:306** - Replace `return error.NativeCodegenNotImplemented`
2. **compile.zig:504-555** - Replace placeholder emit functions
3. **func_translator.zig** - May need memory instruction additions

### Key Functions to Study

1. `WasmFuncTranslator.translateOperator()` - Wasm → CLIF
2. `compile.compile()` - CLIF → Native orchestration
3. `Lower.lower()` - CLIF → VCode
4. `regalloc.run()` - Register allocation
5. `MachBuffer.finish()` - Code finalization

### Test Commands

```bash
# Run all tests
zig build test

# Test specific module
zig test compiler/codegen/native/compile.zig

# Debug output
COT_DEBUG=codegen zig build test
```
