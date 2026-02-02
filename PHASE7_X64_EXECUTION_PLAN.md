# Phase 7: x64 Integration Execution Plan

**Objective**: Complete x64 backend to achieve full Cranelift parity and enable native code generation.

**Source Reference**: `~/learning/wasmtime/cranelift/codegen/src/isa/x64/`

---

## Current State Analysis

### Cranelift x64 Source Files (9,967 lines)

| File | Lines | Purpose |
|------|-------|---------|
| `abi.rs` | 1,348 | ABI implementation (ABIMachineSpec) |
| `inst/mod.rs` | 1,680 | Instruction definitions |
| `inst/emit.rs` | 2,195 | Instruction emission |
| `inst/args.rs` | 1,063 | Operand types |
| `inst/regs.rs` | 176 | Register definitions |
| `inst/emit_state.rs` | 55 | Emission state |
| `inst/external.rs` | 564 | External instruction handling |
| `inst/unwind/` | 225 | Unwind info (SystemV + WinX64) |
| `lower.rs` | 353 | CLIF lowering dispatch |
| `lower/isle.rs` | 1,251 | ISLE lowering rules |
| `mod.rs` | 272 | Top-level module |
| `pcc.rs` | 326 | Proof-carrying code |
| `settings.rs` | 9 | ISA settings |

### Cot x64 Current State (10,998 lines)

| File | Lines | Status |
|------|-------|--------|
| `abi.zig` | 1,921 | ✅ Core Complete (+1,170 lines) |
| `inst/mod.zig` | 1,972 | ✅ Complete (with printWithState) |
| `inst/emit.zig` | 3,049 | ✅ Complete (VEX/EVEX/atomics/traps) |
| `inst/args.zig` | 1,299 | ✅ Complete |
| `inst/regs.zig` | 478 | ✅ Complete |
| `inst/get_operands.zig` | 673 | ✅ Complete |
| `lower.zig` | 1,480 | ✅ Complete |
| `mod.zig` | 126 | **INCOMPLETE** - Missing exports |

### Gap Analysis: abi.zig

**Cranelift's `X64ABIMachineSpec` has 30+ methods. X64MachineDeps now implements 20+ methods.**

| Cranelift Method | Lines | Cot Status | Priority |
|------------------|-------|------------|----------|
| `word_bits()` | 3 | ✅ Complete | P1 |
| `stack_align()` | 3 | ✅ Complete | P1 |
| `compute_arg_locs()` | 320 | ✅ Complete | P1 |
| `gen_load_stack()` | 13 | ✅ Complete | P1 |
| `gen_store_stack()` | 9 | ✅ Complete | P1 |
| `gen_move()` | 5 | ✅ Complete | P1 |
| `gen_extend()` | 16 | ✅ Complete | P1 |
| `gen_args()` | 4 | ❌ Missing | P1 |
| `gen_rets()` | 4 | ❌ Missing | P1 |
| `gen_add_imm()` | 15 | ✅ Complete | P1 |
| `gen_stack_lower_bound_trap()` | 13 | ❌ Missing | P2 |
| `gen_get_stack_addr()` | 7 | ✅ Complete | P1 |
| `get_stacklimit_reg()` | 10 | ✅ Complete | P2 |
| `gen_load_base_offset()` | 8 | ✅ Complete | P1 |
| `gen_store_base_offset()` | 10 | ✅ Complete | P1 |
| `gen_sp_reg_adjust()` | 10 | ✅ Complete | P1 |
| `gen_prologue_frame_setup()` | 33 | ✅ Complete | P1 |
| `gen_epilogue_frame_restore()` | 21 | ✅ Complete | P1 |
| `gen_return()` | 20 | ✅ Complete | P1 |
| `gen_probestack()` | 16 | ❌ Missing | P2 |
| `gen_inline_probestack()` | 24 | ❌ Missing | P2 |
| `gen_clobber_save()` | 111 | ✅ Complete | P1 |
| `gen_clobber_restore()` | 99 | ✅ Complete | P1 |
| `get_number_of_spillslots_for_value()` | 13 | ✅ Complete | P1 |
| `get_machine_env()` | 10 | ❌ Missing (use regalloc.x64MachineEnv) | P1 |
| `get_regs_clobbered_by_call()` | 21 | ✅ Complete | P1 |
| `get_ext_mode()` | 7 | ✅ Complete | P1 |
| `compute_frame_layout()` | 58 | ✅ Complete | P1 |
| `retval_temp_reg()` | 7 | ✅ Complete | P1 |
| `exception_payload_regs()` | 12 | ❌ Missing | P2 |

### Missing Types in abi.zig

| Type | Cranelift Location | Purpose | Priority |
|------|-------------------|---------|----------|
| `StackAMode` | machinst/abi.rs | Stack addressing modes | P1 |
| `FrameLayout` | machinst/abi.rs | Full frame layout struct | P1 |
| `PRegSet` | regalloc2 | Physical register sets | P1 (use regalloc's) |
| `MachineEnv` | regalloc2 | Machine environment | P1 (use regalloc's) |
| `IsaFlags` | x64/settings.rs | ISA feature flags | P2 |
| `SettingsFlags` | settings.rs | Compiler settings | P2 |
| `UnwindInst` | x64/inst/unwind/ | Unwind instructions | P3 |
| `Signature` | machinst/abi.rs | Function signatures | P2 |

---

## Execution Plan

### Phase 7.1: Core Types and Imports (Day 1)

**Task 7.1.1**: Add missing type imports to abi.zig
- [x] Import `regalloc.MachineEnv` from `regalloc/env.zig` (deferred - local PRegSet used)
- [x] Import `regalloc.PRegSet` from `regalloc/index.zig` (deferred - local PRegSet used)
- [x] Add `StackAMode` union type
- [x] Add `FrameLayout` struct
- [x] Add `IsaFlags` struct (basic)
- [x] Add `SettingsFlags` struct (basic)
- [x] **Test**: Verify types compile (43 tests passing)
- [ ] **Audit**: Document type mappings to Cranelift

**Actual lines added**: ~200

### Phase 7.1.2: Update mod.zig exports
- [ ] Re-export `MachineEnv` from regalloc
- [ ] Re-export `createRegEnv` → `x64MachineEnv`
- [ ] Re-export new types from abi.zig
- [ ] **Test**: Verify all exports accessible
- [ ] **Audit**: Cross-check with ARM64 mod.zig

**Estimated lines**: ~30

---

### Phase 7.2: X64ABIMachineSpec - Basic Methods (Day 1-2)

**Task 7.2.1**: Create `X64MachineDeps` struct ✅ COMPLETE
```zig
pub const X64MachineDeps = struct {
    pub const I = Inst;
    pub const F = IsaFlags;
    pub const STACK_ARG_RET_SIZE_LIMIT: u32 = 128 * 1024 * 1024;

    pub fn wordBits() u32 { return 64; }
    pub fn wordType() Type { return Type.i64; }
    pub fn stackAlign(call_conv: CallConv) u32 { return 16; }
    // ... etc
};
```
- [x] `wordBits()` - 64
- [x] `wordType()` - Type.i64
- [x] `stackAlign()` - 16
- [x] `rcForType()` - register class for type
- [x] **Test**: Basic method tests (X64MachineDeps basic, rcForType tests)
- [ ] **Audit**: Verify matches Cranelift x64/abi.rs:75-93

**Actual lines**: ~60

**Task 7.2.2**: Port `computeArgLocs()` (full version) ✅ COMPLETE
- [x] Handle System V ABI argument passing
- [x] Handle Windows x64 argument passing
- [x] Handle shadow space (Windows)
- [x] Handle return area pointer
- [x] Handle multi-register arguments (i128)
- [ ] Handle struct arguments (deferred)
- [x] **Test**: System V args, Windows args, mixed args
- [ ] **Audit**: Line-by-line comparison with Cranelift

**Actual lines**: ~250

---

### Phase 7.3: Instruction Generation Methods (Day 2-3) ✅ COMPLETE

**Task 7.3.1**: Memory operation generators ✅ COMPLETE
- [x] `genLoadStack(mem: StackAMode, into_reg: Writable(Reg), ty: Type) -> Inst`
- [x] `genStoreStack(mem: StackAMode, from_reg: Reg, ty: Type) -> Inst`
- [x] `genLoadBaseOffset(into_reg, base, offset, ty) -> Inst`
- [x] `genStoreBaseOffset(base, offset, from_reg, ty) -> Inst`
- [ ] **Test**: Load/store for i8, i16, i32, i64, f32, f64
- [ ] **Audit**: Verify instruction encoding matches

**Actual lines**: ~40

**Task 7.3.2**: Register operation generators ✅ COMPLETE
- [x] `genMove(to_reg: Writable(Reg), from_reg: Reg, ty: Type) -> Inst`
- [x] `genExtend(to_reg, from_reg, signed, from_bits, to_bits) -> Inst`
- [ ] `genArgs(args: []ArgPair) -> Inst` (deferred)
- [ ] `genRets(rets: []RetPair) -> Inst` (deferred)
- [ ] **Test**: Move between GPR, XMM; extend 8→64, 16→64, 32→64
- [ ] **Audit**: Verify against Cranelift

**Actual lines**: ~40

**Task 7.3.3**: Arithmetic generators ✅ COMPLETE
- [x] `genAddImm(into_reg, from_reg, imm) -> SmallInstVec`
- [x] `genSpRegAdjust(amount: i32) -> SmallInstVec`
- [x] `genGetStackAddr(mem: StackAMode, into_reg) -> Inst`
- [ ] **Test**: Add positive/negative immediates, adjust RSP
- [ ] **Audit**: Verify instruction selection

**Actual lines**: ~50

---

### Phase 7.4: Prologue/Epilogue Generation (Day 3-4) ✅ COMPLETE

**Task 7.4.1**: Frame layout computation ✅ COMPLETE
- [x] Port full `FrameLayout` struct from ARM64
- [x] Port `computeFrameLayout()` with all fields
- [x] Handle fixed frame, spillslots, clobbers
- [x] **Test**: FrameLayout test (frameSize())
- [ ] **Audit**: Compare with Cranelift machinst/abi.rs

**Actual lines**: ~100

**Task 7.4.2**: Prologue generation ✅ COMPLETE
- [x] `genPrologueFrameSetup(call_conv, flags, clobbered_callee_saves, fixed_frame_storage_size, outgoing_args_size) -> SmallInstVec`
- [x] Handle frame pointer setup (push rbp; mov rbp, rsp)
- [ ] Handle callee-save spills (in genClobberSave)
- [ ] Handle stack allocation (in genClobberSave)
- [ ] **Test**: System V prologue, Windows prologue
- [ ] **Audit**: Verify matches Cranelift

**Actual lines**: ~40

**Task 7.4.3**: Epilogue generation ✅ COMPLETE
- [x] `genEpilogueFrameRestore(call_conv, flags, clobbered_callee_saves, fixed_frame_storage_size, outgoing_args_size) -> SmallInstVec`
- [x] Handle callee-save restores (in genClobberRestore)
- [x] Handle frame pointer restore (mov rsp, rbp; pop rbp)
- [ ] **Test**: System V epilogue, Windows epilogue
- [ ] **Audit**: Verify stack unwinding correctness

**Actual lines**: ~30

**Task 7.4.4**: Return generation ✅ COMPLETE
- [x] `genReturn(rets: []RetPair) -> Inst`
- [x] Handle RET instruction
- [ ] **Test**: Return with values
- [ ] **Audit**: Verify ABI compliance

**Actual lines**: ~10

---

### Phase 7.5: Callee-Save Handling (Day 4) ✅ COMPLETE

**Task 7.5.1**: Clobber save generation ✅ COMPLETE
- [x] `genClobberSave(call_conv, flags, clobbers, fixed_frame_storage_size, outgoing_args_size) -> SmallInstVec`
- [x] Handle GPR pushes
- [x] Handle XMM saves (Windows)
- [ ] **Test**: System V clobbers, Windows clobbers
- [ ] **Audit**: Verify register preservation

**Actual lines**: ~80

**Task 7.5.2**: Clobber restore generation ✅ COMPLETE
- [x] `genClobberRestore(call_conv, flags, clobbers, fixed_frame_storage_size, outgoing_args_size) -> SmallInstVec`
- [x] Handle GPR pops (reverse order)
- [x] Handle XMM restores (Windows)
- [ ] **Test**: Restore order, stack alignment
- [ ] **Audit**: Verify matches save order

**Actual lines**: ~70

---

### Phase 7.6: Regalloc Integration (Day 4-5) ✅ PARTIAL

**Task 7.6.1**: Machine environment integration
- [ ] `getMachineEnv(flags, call_conv) -> *MachineEnv` (deferred to driver integration)
- [ ] Wire to `regalloc.x64MachineEnv()` (exists in regalloc/env.zig)
- [ ] Handle pinned register configuration
- [ ] **Test**: Verify register sets
- [ ] **Audit**: Compare with ARM64 integration

**Estimated lines**: ~30

**Task 7.6.2**: Spillslot computation ✅ COMPLETE
- [x] `getNumberOfSpillslotsForValue(rc: RegClass, for_reload: bool) -> u32`
- [x] Handle GPR spillslots (1 slot = 8 bytes)
- [x] Handle XMM spillslots (2 slots = 16 bytes)
- [ ] **Test**: Various register classes
- [ ] **Audit**: Verify spillslot sizes

**Actual lines**: ~15

**Task 7.6.3**: Clobber sets as PRegSet ✅ COMPLETE
- [x] `getRegsClobberedByCall(call_conv) -> PRegSet`
- [x] Add DEFAULT_SYSV_CLOBBERS, DEFAULT_WIN64_CLOBBERS constants
- [x] **Test**: DEFAULT_SYSV_CLOBBERS test
- [ ] **Audit**: Verify all clobbered registers included

**Actual lines**: ~40

---

### Phase 7.7: Stack Probing (Day 5)

**Task 7.7.1**: Basic stack probing
- [ ] `genProbestack(frame_size: u32) -> SmallInstVec`
- [ ] Handle guard page detection
- [ ] **Test**: Small and large frames
- [ ] **Audit**: Verify probe sequence

**Estimated lines**: ~30

**Task 7.7.2**: Inline stack probing
- [ ] `genInlineProbestack(guard_size, probe_count) -> SmallInstVec`
- [ ] `genProbestackUnroll(guard_size, probe_count) -> SmallInstVec`
- [ ] `genProbestackLoop(frame_size, guard_size) -> SmallInstVec`
- [ ] **Test**: Unroll vs loop selection
- [ ] **Audit**: Verify probe coverage

**Estimated lines**: ~80

**Task 7.7.3**: Stack limit checking
- [ ] `genStackLowerBoundTrap(limit_reg) -> SmallInstVec`
- [ ] `getStacklimitReg(call_conv) -> Reg`
- [ ] **Test**: Trap on overflow
- [ ] **Audit**: Verify trap handling

**Estimated lines**: ~30

---

### Phase 7.8: Miscellaneous Methods (Day 5)

**Task 7.8.1**: Extension mode
- [ ] `getExtMode(src_bits, dst_bits) -> ExtKind`
- [ ] **Test**: All extension combinations
- [ ] **Audit**: Verify mode selection

**Estimated lines**: ~15

**Task 7.8.2**: Temporary registers
- [ ] `retvalTempReg(call_conv) -> Writable(Reg)`
- [ ] `exceptionPayloadRegs(call_conv) -> []const Reg`
- [ ] **Test**: Verify register choices
- [ ] **Audit**: Compare with Cranelift

**Estimated lines**: ~25

---

### Phase 7.9: Driver Integration (Day 6)

**Task 7.9.1**: Wire x64 backend into driver.zig
- [ ] Import x64 module
- [ ] Add x64 target detection
- [ ] Wire CLIF → x64 lowering
- [ ] Wire regalloc
- [ ] Wire emission
- [ ] **Test**: Compile simple function
- [ ] **Audit**: Full pipeline trace

**Estimated lines**: ~100

**Task 7.9.2**: Object file generation
- [ ] Wire x64 emission to ELF generator
- [ ] Handle relocations
- [ ] Handle symbols
- [ ] **Test**: Generate valid ELF
- [ ] **Audit**: Verify with objdump

**Estimated lines**: ~50

---

### Phase 7.10: End-to-End Tests (Day 6-7)

**Task 7.10.1**: Simple function tests
- [ ] Return constant (return 42)
- [ ] Return argument (return arg0)
- [ ] Arithmetic (add, sub, mul)
- [ ] **Test**: Execute via wasmtime or direct
- [ ] **Audit**: Verify output values

**Task 7.10.2**: Control flow tests
- [ ] If/else branches
- [ ] Loops (while, for)
- [ ] Switch/jump tables
- [ ] **Test**: Execute all paths
- [ ] **Audit**: Verify branch correctness

**Task 7.10.3**: Function call tests
- [ ] Call internal function
- [ ] Call with multiple args
- [ ] Call with FP args
- [ ] Recursive calls
- [ ] **Test**: Stack frame correctness
- [ ] **Audit**: ABI compliance

**Task 7.10.4**: Memory operation tests
- [ ] Load/store i8, i16, i32, i64
- [ ] Load/store f32, f64
- [ ] Stack locals
- [ ] **Test**: Memory roundtrip
- [ ] **Audit**: Alignment correctness

---

## Summary

### Progress: +1,170 lines added to abi.zig

| Phase | Estimated | Actual | Status |
|-------|-----------|--------|--------|
| 7.1 Core Types | 180 | ~200 | ✅ Complete |
| 7.2 Basic Methods | 370 | ~310 | ✅ Complete |
| 7.3 Instruction Gen | 160 | ~130 | ✅ Complete |
| 7.4 Prologue/Epilogue | 270 | ~180 | ✅ Complete |
| 7.5 Callee-Save | 220 | ~150 | ✅ Complete |
| 7.6 Regalloc Integration | 100 | ~55 | ⚠️ Partial |
| 7.7 Stack Probing | 140 | 0 | ❌ Pending |
| 7.8 Misc Methods | 40 | ~20 | ⚠️ Partial |
| 7.9 Driver Integration | 150 | 0 | ❌ Pending |
| 7.10 E2E Tests | - | - | ❌ Pending |
| **Total** | **~1,630** | **~1,170** | **72%** |

### Files Modified

| File | Action | Lines Added | Status |
|------|--------|-------------|--------|
| `x64/abi.zig` | Extended | +1,170 | ✅ Core complete |
| `x64/mod.zig` | Pending | +50 | ❌ Pending |
| `driver.zig` | Pending | +150 | ❌ Pending |
| `audit/clif/isa/x64/abi.md` | Pending | ~500 | ❌ Pending |

### Testing Strategy

1. **Unit tests**: Each method gets tests in abi.zig
2. **Integration tests**: Full pipeline tests in driver
3. **E2E tests**: Execute generated code
4. **Audit**: Line-by-line Cranelift comparison

### Audit Documents Required

- [ ] `audit/clif/isa/x64/abi.md` - Full abi.zig audit
- [ ] `audit/clif/isa/x64/integration.md` - Driver integration audit
- [ ] Update `audit/clif/isa/x64/inst.md` with final status

---

## Verification Commands

```bash
# Test x64 abi module
zig test compiler/codegen/native/isa/x64/abi.zig

# Test full x64 module
zig test compiler/codegen/native/isa/x64/mod.zig

# Test driver with x64
zig build test

# Generate and verify ELF
./zig-out/bin/cot --target=x86_64-linux test.cot -o test
objdump -d test
./test && echo $?
```

---

## References

- Cranelift x64 ABI: `~/learning/wasmtime/cranelift/codegen/src/isa/x64/abi.rs`
- Cranelift machinst ABI: `~/learning/wasmtime/cranelift/codegen/src/machinst/abi.rs`
- regalloc2 x64 env: `compiler/codegen/native/regalloc/env.zig`
- ARM64 reference: `compiler/codegen/native/isa/aarch64/abi.zig`
- System V ABI: https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf
- Windows x64 ABI: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention
