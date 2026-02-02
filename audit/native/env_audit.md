# Machine Environment Module Audit (Phase 6.4)

**Source**: `regalloc2/src/lib.rs` lines 1497-1544
**Target**: `compiler/codegen/native/regalloc/env.zig`
**Status**: ✅ Complete (~230 LOC, 4 tests)

---

## Type Mapping

| Rust Field/Type | Zig Field/Type | Rust Location | Notes |
|-----------------|----------------|---------------|-------|
| `MachineEnv` | `MachineEnv` | lib.rs:1503 | ✅ |
| `preferred_regs_by_class: [PRegSet; 3]` | `preferred_regs_by_class: [3]PRegSet` | lib.rs:1509 | ✅ |
| `non_preferred_regs_by_class: [PRegSet; 3]` | `non_preferred_regs_by_class: [3]PRegSet` | lib.rs:1518 | ✅ |
| `scratch_by_class: [Option<PReg>; 3]` | `scratch_by_class: [3]?PReg` | lib.rs:1535 | ✅ |
| `fixed_stack_slots: Vec<PReg>` | `fixed_stack_slots: []const PReg` | lib.rs:1543 | ✅ |

---

## Methods

| Rust Method | Zig Method | Notes |
|-------------|------------|-------|
| N/A | `empty()` | Empty environment with no allocatable regs |
| N/A | `allocatableRegs(class)` | Get preferred + non-preferred for class |
| N/A | `totalAllocatableCount()` | Total allocatable registers |
| N/A | `isAllocatable(preg)` | Check if register is allocatable |
| N/A | `isScratch(preg)` | Check if register is scratch |

---

## ISA Configurations

### ARM64 (`arm64MachineEnv()`)

**Integer Class:**
- Preferred: x0-x17 (18 regs)
- Non-preferred: x19-x28 (10 regs)
- Scratch: x16 (IP0)
- Not allocatable: x18 (platform), x29 (FP), x30 (LR), x31 (SP/ZR)

**Float Class:**
- Preferred: v0-v7, v16-v31 (24 regs)
- Non-preferred: v8-v15 (8 regs, callee-saved)
- Scratch: None (uses integer x16)

### x86-64 (`x64MachineEnv()`)

**Integer Class:**
- Preferred: rax (0), rcx (1), rdx (2), rsi (6), rdi (7), r8-r10 (8 regs)
- Non-preferred: rbx (3), r12-r15 (5 regs, callee-saved)
- Scratch: r11 (11)
- Not allocatable: rsp (4), rbp (5)

**Float Class:**
- Preferred: xmm0-xmm15 (16 regs, all caller-saved in System V ABI)
- Non-preferred: None
- Scratch: None

---

## Register Naming Convention

Registers are numbered by hardware encoding within their class:

**ARM64:**
- Integer: 0-30 = x0-x30
- Float: 0-31 = v0-v31

**x86-64:**
- Integer: 0=rax, 1=rcx, 2=rdx, 3=rbx, 4=rsp, 5=rbp, 6=rsi, 7=rdi, 8-15=r8-r15
- Float: 0-15 = xmm0-xmm15

---

## Test Coverage

| Test | Status | Description |
|------|--------|-------------|
| MachineEnv empty | ✅ | Empty configuration |
| ARM64 MachineEnv | ✅ | ARM64 register configuration |
| x64 MachineEnv | ✅ | x86-64 register configuration |
| MachineEnv isAllocatable and isScratch | ✅ | Register classification |

