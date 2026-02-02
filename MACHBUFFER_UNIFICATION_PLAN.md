# MachBuffer Unification Plan

## Problem Statement

The Cranelift port has **two different MachBuffer types** that should be one:

1. `machinst/buffer.zig` → `MachBuffer(LabelUseType)`
2. `isa/aarch64/inst/emit.zig` → its own `MachBuffer` struct

This is a **porting error**. Cranelift only has ONE MachBuffer type.

## Root Cause Analysis

### How Cranelift Does It (Correct)

In Cranelift (`cranelift/codegen/src/machinst/buffer.rs`):

```rust
pub struct MachBuffer<I: VCodeInst> {
    data: SmallVec<[u8; 1024]>,
    relocs: SmallVec<[MachReloc; 16]>,
    // ... etc
}
```

Key insight: **MachBuffer is parameterized by the instruction type `I`**, not by LabelUse directly.

The `VCodeInst` trait (which `I` must implement) includes:
```rust
pub trait VCodeInst: Clone + Debug {
    type LabelUse: MachInstLabelUse;
    // ... other associated types
}
```

So the LabelUse type comes **through** the instruction type via Rust's trait system.

### What Happened in Our Port (Wrong)

Someone made two mistakes:

1. **Wrong parameterization**: `machinst/buffer.zig` parameterizes on `LabelUseType` directly instead of on the instruction type. This loses the connection to the instruction.

2. **Duplicate struct**: When the AArch64 emit code was ported, instead of fixing the parameterization, someone created a **separate** MachBuffer struct in `isa/aarch64/inst/emit.zig`.

This created the type mismatch:
- `VCode.emit()` uses `MachBuffer(DefaultLabelUse)` from `machinst/buffer.zig`
- `Inst.emitWithAllocs()` expects `emit_mod.MachBuffer` from `isa/aarch64/inst/emit.zig`

## Correct Solution: Copy Cranelift's Pattern

### Option A: Parameterize by Instruction Type (Preferred)

This matches Cranelift exactly.

**Step 1: Update `machinst/buffer.zig`**

Change from:
```zig
pub fn MachBuffer(comptime LabelUseType: type) type {
    return struct {
        // ...
    };
}
```

To:
```zig
pub fn MachBuffer(comptime InstType: type) type {
    // Get LabelUse from instruction type
    const LabelUseType = InstType.LabelUse;

    return struct {
        // ... same implementation, uses LabelUseType internally
    };
}
```

**Step 2: Update instruction types to expose LabelUse**

In `isa/aarch64/inst/mod.zig`:
```zig
pub const Inst = union(enum) {
    // ... instruction variants

    // Associated type for MachBuffer parameterization
    pub const LabelUse = AArch64LabelUse;
};
```

**Step 3: Delete the duplicate MachBuffer in `isa/aarch64/inst/emit.zig`**

Remove the entire `MachBuffer` struct definition. Use the common one:
```zig
const buffer = @import("../../../machinst/buffer.zig");
pub const MachBuffer = buffer.MachBuffer(Inst);
```

**Step 4: Update `emitWithAllocs` signature**

Change from using the local MachBuffer to using the common one.

### Option B: Use Type Alias (Simpler but Less Clean)

If the instruction type parameterization is complex, we can use a simpler approach:

**Step 1: Keep `machinst/buffer.zig` parameterized by LabelUse**

**Step 2: In each ISA, create a type alias**

In `isa/aarch64/inst/emit.zig`:
```zig
const buffer = @import("../../../machinst/buffer.zig");
pub const MachBuffer = buffer.MachBuffer(AArch64LabelUse);
```

**Step 3: Delete the duplicate struct definition**

**Step 4: Update all code to use the unified type**

## Files to Modify

| File | Change |
|------|--------|
| `machinst/buffer.zig` | Update parameterization OR keep as-is with Option B |
| `isa/aarch64/inst/emit.zig` | Delete ~250 lines of duplicate MachBuffer code |
| `isa/aarch64/inst/mod.zig` | Add `LabelUse` type alias, update `emitWithAllocs` signature |
| `isa/x64/inst/emit.zig` | Same changes as aarch64 |
| `isa/x64/inst/mod.zig` | Same changes as aarch64 |
| `machinst/vcode.zig` | May need updates to use correct MachBuffer instantiation |
| `compile.zig` | May need updates for emit function signatures |

## Verification

After unification:
1. `zig build test` passes
2. Only ONE MachBuffer struct definition exists (in `machinst/buffer.zig`)
3. ISA emit files import and use the common MachBuffer
4. `VCode.emit()` and `Inst.emitWithAllocs()` use the same MachBuffer type

## Why This Matters

Without this fix:
- VCode cannot emit instructions (type mismatch)
- The native codegen pipeline is broken at the emit phase
- We cannot complete the Wasm → Native compilation path

## Reference Files

Study these Cranelift files to understand the correct pattern:
- `~/learning/wasmtime/cranelift/codegen/src/machinst/buffer.rs` - The ONE MachBuffer
- `~/learning/wasmtime/cranelift/codegen/src/machinst/vcode.rs` - How VCode uses it
- `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/inst/emit.rs` - How AArch64 emit uses it
- `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/inst/mod.rs` - Inst type with LabelUse
