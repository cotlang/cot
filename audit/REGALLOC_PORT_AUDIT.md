# Register Allocator Port Audit

**Date:** February 3, 2026
**Purpose:** Document why the current native codegen is broken and what needs to be properly ported from regalloc2/Cranelift.

## Executive Summary

The native codegen has **duplicate type definitions** and **incomplete ports** that were never properly connected. When the liveness/merge/process phases were finally wired up, compilation fails due to type mismatches between:

- `regalloc/index.zig` types (PReg, PRegSet, RegClass)
- `machinst/reg.zig` types (PReg, PRegSet, RegClass)

This is a fundamental architectural failure - the port created duplicate types instead of having a single source of truth.

---

## 1. Root Cause Analysis

### 1.1 Duplicate Type Definitions

The following types are defined in TWO places:

| Type | regalloc/index.zig | machinst/reg.zig | Notes |
|------|-------------------|------------------|-------|
| `PReg` | Lines 258-340 | Lines 55-150 | Different internal structure |
| `PRegSet` | Lines 343-430 | Lines 405-463 | Different bitmap layout |
| `RegClass` | Lines 211-220 | Lines 28-52 | Same enum values |

**In regalloc2 (Rust):** There is ONE definition of these types in `src/lib.rs` that is used everywhere.

**In Cot (Zig):** Two separate implementations that are incompatible.

### 1.2 Type Flow Problem

```
vcode.zig
  └── uses machinst/reg.zig types
  └── instClobbers() returns machinst.reg.PRegSet

compile.zig
  └── VCodeFunction wrapper
  └── instClobbers() declared to return regalloc.index.PRegSet  ← TYPE MISMATCH

liveness.zig
  └── imports regalloc/index.zig types
  └── expects regalloc.index.PReg in addLiverangeToPreg()
  └── receives machinst.reg.PReg from PRegSet.iterate()  ← TYPE MISMATCH
```

### 1.3 Why This Happened

The port was done in pieces without ensuring type consistency:
1. `machinst/` was ported with its own type definitions
2. `regalloc/` was ported with its own type definitions
3. Nobody verified they used the same types
4. The `run()` function in regalloc.zig never called the liveness phases, so the type mismatches were never discovered

---

## 2. Cranelift/regalloc2 Architecture

### 2.1 Type Ownership in regalloc2

**Source:** `~/learning/regalloc2/src/lib.rs`

```rust
// ALL types defined in ONE place: src/lib.rs

pub enum RegClass { Int = 0, Float = 1, Vector = 2 }

pub struct PReg { bits: u8 }  // Single definition

pub struct PRegSet { ... }    // Single definition

// These are re-exported and used everywhere
```

### 2.2 Cranelift machinst Types

**Source:** `~/learning/wasmtime/cranelift/codegen/src/machinst/`

Cranelift's machinst module **imports types from regalloc2**, it doesn't redefine them:

```rust
// cranelift/codegen/src/machinst/mod.rs
use regalloc2::{PReg, PRegSet, RegClass, VReg};
```

---

## 3. Required Refactoring

### 3.1 Single Source of Truth for Types

**Decision:** Use `regalloc/index.zig` as the canonical source for:
- `PReg`
- `PRegSet`
- `RegClass`
- `VReg`

**Rationale:** These are regalloc2 types. The regalloc module should own them.

### 3.2 Files That Need Modification

| File | Current State | Required Change |
|------|---------------|-----------------|
| `machinst/reg.zig` | Defines PReg, PRegSet, RegClass | Remove definitions, import from regalloc/index.zig |
| `machinst/vcode.zig` | Uses machinst/reg.zig types | Import from regalloc/index.zig |
| `compile.zig` | Mixed imports | Use only regalloc/index.zig types |
| `liveness.zig` | Uses regalloc/index.zig | Keep as-is (correct) |
| All ISA files | May use machinst/reg.zig | Audit and fix imports |

### 3.3 regalloc/index.zig Additions Needed

The `regalloc/index.zig` PRegSet needs methods that `machinst/reg.zig` has:
- `iterate()` - Iterator over registers in set

---

## 4. Execution Plan

### Phase 1: Audit Type Usage

1. List every file that imports `machinst/reg.zig`
2. List every file that imports `regalloc/index.zig`
3. Identify all type mismatches

### Phase 2: Unify Types

1. Add missing methods to `regalloc/index.zig` types (e.g., `iterate()`)
2. Modify `machinst/reg.zig` to re-export from `regalloc/index.zig` instead of defining
3. Update all imports throughout codebase

### Phase 3: Verify Port Correctness

1. Compare `regalloc/index.zig` PReg against `regalloc2/src/lib.rs` PReg
2. Compare `regalloc/index.zig` PRegSet against `regalloc2/src/lib.rs` PRegSet
3. Ensure method signatures match

### Phase 4: Wire Up Allocation Phases

1. Verify `runWithCtx()` calls all phases in correct order
2. Ensure data flows correctly between phases
3. Run tests

---

## 5. Cranelift Source References

### regalloc2 Types

| Type | Source File | Line |
|------|-------------|------|
| `RegClass` | `~/learning/regalloc2/src/lib.rs` | 86-92 |
| `PReg` | `~/learning/regalloc2/src/lib.rs` | 108-180 |
| `PRegSet` | `~/learning/regalloc2/src/lib.rs` | 230-330 |
| `VReg` | `~/learning/regalloc2/src/lib.rs` | 340-400 |

### regalloc2 Ion Allocator

| Function | Source File | Description |
|----------|-------------|-------------|
| `run()` | `~/learning/regalloc2/src/ion/mod.rs:108` | Main entry point |
| `Env::init()` | `~/learning/regalloc2/src/ion/mod.rs:83` | Phase 1-5 setup |
| `Env::run()` | `~/learning/regalloc2/src/ion/mod.rs:96` | Phase 6-10 execution |
| `create_pregs_and_vregs` | `~/learning/regalloc2/src/ion/liveranges.rs` | Initialize register data |
| `compute_liveness` | `~/learning/regalloc2/src/ion/liveranges.rs` | Liveness analysis |
| `build_liveranges` | `~/learning/regalloc2/src/ion/liveranges.rs` | Build live ranges |
| `merge_vreg_bundles` | `~/learning/regalloc2/src/ion/merge.rs` | Merge bundles |
| `queue_bundles` | `~/learning/regalloc2/src/ion/merge.rs` | Queue for allocation |
| `process_bundles` | `~/learning/regalloc2/src/ion/process.rs` | Main allocation loop |
| `allocate_spillslots` | `~/learning/regalloc2/src/ion/spill.rs` | Spillslot allocation |
| `apply_allocations_and_insert_moves` | `~/learning/regalloc2/src/ion/moves.rs` | Move insertion |

---

## 6. Detailed Refactoring Specification

### 6.1 regalloc2 Type Ownership (Source of Truth)

From `~/learning/regalloc2/src/lib.rs`:
```rust
pub enum RegClass { Int = 0, Float = 1, Vector = 2 }
pub struct PReg { bits: u8 }
pub struct PRegSet { ... }
pub struct VReg { ... }
pub struct SpillSlot { ... }
pub enum OperandConstraint { ... }
pub enum OperandKind { Use, Def }
pub enum OperandPos { Early, Late }
pub struct Operand { ... }
pub struct Allocation { ... }
pub enum InstPosition { Before, After }
pub struct ProgPoint { ... }
```

From `~/learning/regalloc2/src/index.rs`:
```rust
define_index!(Block);
define_index!(Inst);
pub struct InstRange(Inst, Inst);
```

### 6.2 Cot File Responsibilities After Refactor

**regalloc/index.zig** (matches regalloc2/src/lib.rs + index.rs):
- OWN: Block, Inst, InstRange, InstRangeIterator
- OWN: RegClass, PReg, PRegSet, PRegSetIterator, VReg, SpillSlot

**regalloc/operand.zig** (matches regalloc2/src/lib.rs):
- OWN: OperandKind, OperandPos, OperandConstraint, Operand
- OWN: Allocation, AllocationKind, InstPosition, ProgPoint, Edit

**machinst/reg.zig** (matches Cranelift's machinst/mod.rs):
- IMPORT from regalloc/index.zig: RegClass, PReg, PRegSet, VReg, SpillSlot
- RE-EXPORT: RegClass, PReg, PRegSet, VReg, SpillSlot
- OWN: Reg (union of VReg | PReg), RealReg, VirtualReg, Writable(T)
- IMPORT from regalloc/operand.zig: Operand, etc.
- RE-EXPORT: Operand types

### 6.3 Specific File Changes

#### machinst/reg.zig

**BEFORE:**
```zig
pub const RegClass = enum(u8) { int = 0, float = 1, vector = 2 };
pub const PReg = struct { bits: u8, ... };
pub const PRegSet = struct { int_regs: u64, float_regs: u64, vector_regs: u64, ... };
// ... full definitions
```

**AFTER:**
```zig
const regalloc_index = @import("../regalloc/index.zig");
const regalloc_operand = @import("../regalloc/operand.zig");

// Re-export regalloc types
pub const RegClass = regalloc_index.RegClass;
pub const PReg = regalloc_index.PReg;
pub const PRegSet = regalloc_index.PRegSet;
pub const VReg = regalloc_index.VReg;
pub const SpillSlot = regalloc_index.SpillSlot;

// Re-export operand types
pub const Operand = regalloc_operand.Operand;
pub const OperandKind = regalloc_operand.OperandKind;
pub const OperandPos = regalloc_operand.OperandPos;
pub const OperandConstraint = regalloc_operand.OperandConstraint;

// OWN these machinst-specific types:
pub const Reg = struct { ... };  // Union of virtual and physical
pub const RealReg = struct { ... };
pub const VirtualReg = struct { ... };
pub fn Writable(comptime T: type) type { ... }
```

#### regalloc/index.zig

**ADD:**
- `iterate()` method to PRegSet (currently missing)
- Ensure all methods match regalloc2's PReg, PRegSet, VReg signatures

**VERIFY:**
- PReg bit layout matches regalloc2 (class in high bits, hw_enc in low bits)
- PRegSet iteration matches regalloc2

#### ISA Files (isa/aarch64/inst/args.zig, isa/x64/inst/args.zig)

**NO CHANGES NEEDED** - They already import from machinst/reg.zig. Once machinst/reg.zig re-exports the unified types, these will automatically work.

#### compile.zig

**REMOVE:**
```zig
const machinst_reg = @import("machinst/reg.zig");
pub const PRegSet = machinst_reg.PRegSet;
```

**KEEP:**
```zig
pub const PReg = regalloc_index.PReg;
pub const PRegSet = regalloc_index.PRegSet;
// ... all from regalloc_index
```

#### liveness.zig

**REMOVE:**
```zig
const machinst_reg = @import("../machinst/reg.zig");
const PRegSet = machinst_reg.PRegSet;
```

**RESTORE:**
```zig
const PRegSet = index.PRegSet;
```

---

## 7. Verification Checklist

After refactoring, verify:

- [x] Only ONE definition of `PReg` exists (in regalloc/index.zig) ✅ COMPLETED
- [x] Only ONE definition of `PRegSet` exists (in regalloc/index.zig) ✅ COMPLETED
- [x] Only ONE definition of `RegClass` exists (in regalloc/index.zig) ✅ COMPLETED
- [x] `machinst/reg.zig` imports from regalloc/index.zig ✅ COMPLETED
- [x] `vcode.zig` uses regalloc types ✅ COMPLETED
- [x] `compile.zig` uses regalloc types ✅ COMPLETED
- [x] All ISA files use regalloc types ✅ COMPLETED
- [x] `zig build test` passes ✅ COMPLETED
- [ ] All liveness/merge/process phases execute without type errors (requires runtime testing)

---

## 8. Refactoring Completed - February 3, 2026

The type unification refactoring has been completed:

### Changes Made:

1. **machinst/reg.zig** - Completely rewritten:
   - NOW IMPORTS from regalloc/index.zig: RegClass, PReg, PRegSet, VReg, SpillSlot
   - NOW IMPORTS from regalloc/operand.zig: OperandKind, OperandPos, OperandConstraint, Operand, Allocation, etc.
   - KEEPS only machinst-specific types: Reg, RealReg, VirtualReg, Writable, OperandCollector
   - RE-EXPORTS imported types for convenience

2. **Type encoding now consistent:**
   - VReg: `bits = (vreg << 2) | class` (matches regalloc2)
   - PReg: `bits = (class << 6) | hw_enc` (matches regalloc2)
   - PRegSet: Array-based bitmap (matches regalloc2)

3. **Additional fixes required during refactoring:**
   - Added `init()` aliases to PReg and VReg for API compatibility
   - Fixed `SpillSlot.index()` method calls
   - Fixed `Operand.vreg()`, `Operand.constraint()`, etc. method calls
   - Fixed `hwEnc()` return type casts (usize → u32 where needed)
   - Added `isTerm()` method to x64 Inst
   - Changed `Requirement.mergeWith` to free function `mergeRequirements`
   - Fixed iterator usage for PRegSet (while loop with .next())

### Why This Matches Cranelift's Architecture:

In Cranelift/regalloc2:
- `regalloc2::lib.rs` DEFINES: PReg, VReg, PRegSet, RegClass, SpillSlot
- `cranelift::machinst` IMPORTS from regalloc2 (does NOT redefine)
- Type encodings are consistent throughout

In Cot (after refactoring):
- `regalloc/index.zig` DEFINES: PReg, VReg, PRegSet, RegClass, SpillSlot
- `machinst/reg.zig` IMPORTS from regalloc/ (does NOT redefine)
- Type encodings are now consistent throughout

---

## 9. Why This Matters

Without fixing this architectural issue:
1. The register allocator cannot run
2. No physical registers get assigned
3. Emit phase fails with "virtual register not allowed"
4. Native compilation is impossible

The previous "trivial allocator" hack bypassed all this by inventing code instead of porting properly. That approach was wrong and has been reverted.
