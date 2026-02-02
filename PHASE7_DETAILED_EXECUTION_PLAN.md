# Phase 7: Detailed Execution Plan

**Created**: 2026-02-02
**Status**: CRITICAL - Pipeline is architectural skeleton, not working compiler

## Brutal Honesty Assessment

The previous "95% complete" claims were **false**. The audit reveals:

| What Was Claimed | Reality |
|------------------|---------|
| CLIF→VCode 95% complete | Constants hardcoded to `0`, return not lowered |
| ARM64 emit 95% complete | Some encodings work, but relies on broken lowering |
| x64 emit 95% complete | `@panic` on most memory operations |
| Object file generation exists | Returns raw bytes, not valid Mach-O/ELF |

**A "return 42" program currently returns 0** because the constant value extraction was never implemented.

---

## Critical Blockers (In Order)

### Blocker 1: Constants Hardcoded to Zero

**File**: `compiler/codegen/native/isa/aarch64/lower.zig`
**Line**: ~333
**Current Code**:
```zig
fn lowerIconst(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
    const ty = ctx.outputTy(ir_inst, 0);
    const size = operandSizeFromType(ty) orelse return null;

    // Get constant value from instruction (placeholder - would extract from data)
    const value: u64 = 0; // TODO: extract from inst_data  <-- CRITICAL BUG
```

**Fix Required**: Extract actual constant from CLIF instruction data.

**Cranelift Reference**: `cranelift/codegen/src/isa/aarch64/lower/isle.rs`
```rust
(rule (lower (has_type ty (iconst (u64_from_imm64 n))))
      (imm ty (ImmExtend.Zero) n))
```

---

### Blocker 2: Return Not Lowered

**File**: `compiler/codegen/native/isa/aarch64/lower.zig`
**Line**: ~233
**Current Code**:
```zig
// Return (handled by lowerBranch)
.@"return" => null,
```

**Fix Required**: Implement return lowering that moves return values to x0/v0.

---

### Blocker 3: CLIF InstructionData Not Populated

**File**: `compiler/codegen/native/machinst/lower.zig`
**Problem**: `InstructionData` struct exists but `LowerCtx.data()` returns placeholder:
```zig
pub fn data(_: *LowerCtx, _: ClifInst) *const InstructionData {
    return &placeholder_inst_data;  // <-- Always returns empty placeholder
}

var placeholder_inst_data = InstructionData{
    .opcode = .nop,
    .args = &[_]Value{},
    .results = &[_]Value{},
};
```

**Fix Required**: Wire `LowerCtx` to actual CLIF `Function.dfg` to get real instruction data.

---

### Blocker 4: Object File Not Wrapped

**File**: `compiler/driver.zig`
**Lines**: 463-483 (Mach-O), 486-507 (ELF)
**Current Code**:
```zig
fn generateMachO(...) ![]u8 {
    // ... collect code ...
    // For now, return raw code - full Mach-O generation will use macho.zig
    // TODO: Use macho.zig to wrap in proper Mach-O format
    return code;  // <-- Raw bytes, not Mach-O
}
```

**Fix Required**: Use existing `macho.zig` to create valid Mach-O object file.

---

## Detailed Task Breakdown

### Task 7.1: Wire LowerCtx to Real CLIF Data

**Objective**: Make `LowerCtx.data()` return actual CLIF instruction data.

**Files to Modify**:
1. `compiler/codegen/native/machinst/lower.zig`
2. `compiler/codegen/native/compile.zig`

**Step 7.1.1**: Add CLIF Function reference to LowerCtx
```zig
// In lower.zig, modify LowerCtx:
pub const LowerCtx = struct {
    allocator: Allocator,
    clif_func: *const clif.Function,  // ADD THIS

    pub fn init(allocator: Allocator, func: *const clif.Function) LowerCtx {
        return .{ .allocator = allocator, .clif_func = func };
    }
```

**Step 7.1.2**: Implement real `data()` method
```zig
pub fn data(self: *LowerCtx, inst: ClifInst) *const InstructionData {
    return self.clif_func.dfg.getInstData(inst);
}
```

**Step 7.1.3**: Wire in compile.zig
```zig
// In compile.zig compileAArch64():
var ctx = LowerCtx.init(allocator, clif_func);  // Pass actual function
```

**Verification**:
```bash
# Add test that extracts constant value
zig test compiler/codegen/native/machinst/lower.zig
# Test should verify: data(iconst_inst).getImmediate() == 42
```

---

### Task 7.2: Fix Constant Extraction in ARM64 Lowering

**Objective**: `lowerIconst` extracts real constant value.

**File**: `compiler/codegen/native/isa/aarch64/lower.zig`

**Step 7.2.1**: Get constant from instruction data
```zig
fn lowerIconst(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
    const ty = ctx.outputTy(ir_inst, 0);
    const size = operandSizeFromType(ty) orelse return null;

    // FIXED: Extract actual constant value from CLIF instruction
    const inst_data = ctx.data(ir_inst);
    const value: u64 = inst_data.getImmediate() orelse return null;

    // ... rest of lowering with real value ...
}
```

**Step 7.2.2**: Add `getImmediate()` to InstructionData
```zig
// In lower.zig or clif/instructions.zig:
pub const InstructionData = struct {
    opcode: Opcode,
    imm: ?i64 = null,  // For iconst
    // ...

    pub fn getImmediate(self: *const InstructionData) ?u64 {
        return if (self.imm) |v| @bitCast(v) else null;
    }
};
```

**Verification**:
```bash
# Create test: lower iconst.i64 42, verify MOV emits 42 not 0
zig test compiler/codegen/native/isa/aarch64/lower.zig
```

---

### Task 7.3: Implement Return Lowering

**Objective**: `return` instruction moves values to return registers.

**File**: `compiler/codegen/native/isa/aarch64/lower.zig`

**Step 7.3.1**: Change return handling in switch
```zig
// Change from:
.@"return" => null,

// To:
.@"return" => self.lowerReturn(ctx, ir_inst),
```

**Step 7.3.2**: Implement lowerReturn
```zig
fn lowerReturn(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
    _ = self;
    const inst_data = ctx.data(ir_inst);

    // Get return values from instruction args
    for (inst_data.args, 0..) |arg, i| {
        const src = ctx.putInputInRegs(ir_inst, i);
        const src_reg = src.onlyReg() orelse continue;

        // Move to return register (x0 for first int, v0 for first float)
        const ret_reg = if (i == 0) xreg(0) else xreg(@intCast(i));

        ctx.emit(Inst{
            .mov = .{
                .size = .size64,
                .rd = Writable(Reg).fromReg(ret_reg),
                .rm = src_reg,
            },
        }) catch return null;
    }

    // Emit ret instruction
    ctx.emit(.ret) catch return null;

    return InstOutput{};
}
```

**Verification**:
```bash
# Test that return 42 produces: mov x0, <const>; ret
zig test compiler/codegen/native/isa/aarch64/lower.zig
```

---

### Task 7.4: Wire Mach-O Generation

**Objective**: Wrap machine code in valid Mach-O object file.

**File**: `compiler/driver.zig`

**Step 7.4.1**: Import macho module properly
```zig
const macho = @import("codegen/native/macho.zig");
```

**Step 7.4.2**: Replace generateMachO implementation
```zig
fn generateMachO(self: *Driver, compiled_funcs: []const native_compile.CompiledCode, exports: []const wasm_parser.Export) ![]u8 {
    var builder = macho.MachOBuilder.init(self.allocator);
    defer builder.deinit();

    // Add text section with code
    var code_offset: u32 = 0;
    for (compiled_funcs) |func| {
        try builder.addCode(func.code, code_offset);
        code_offset += @intCast(func.code.len);
    }

    // Add exports as symbols
    for (exports, 0..) |exp, i| {
        if (exp.kind == .function) {
            const func_idx = exp.index;
            // Calculate offset for this function
            var offset: u32 = 0;
            for (0..func_idx) |j| {
                offset += @intCast(compiled_funcs[j].code.len);
            }
            try builder.addSymbol(exp.name, offset);
        }
    }

    return builder.build();
}
```

**Step 7.4.3**: Verify macho.zig has required methods
Check `macho.zig` for `MachOBuilder`, `addCode`, `addSymbol`, `build`.
If missing, implement based on Mach-O format spec.

**Verification**:
```bash
# Compile test program, verify output is valid Mach-O
./zig-out/bin/cot test.cot -o test.o
file test.o  # Should say "Mach-O 64-bit object arm64"
```

---

### Task 7.5: Replicate for x64

After ARM64 works, apply same fixes to x64:

**Files**:
- `compiler/codegen/native/isa/x64/lower.zig` - Same constant/return fixes
- `compiler/driver.zig` - `generateElf()` using `elf.zig`

---

### Task 7.6: End-to-End Test - Return 42

**Test File**: `test/native/return_42.cot`
```cot
fn main() i64 {
    return 42;
}
```

**Test Script**:
```bash
#!/bin/bash
set -e

# Compile
./zig-out/bin/cot test/native/return_42.cot -o return_42

# Link (macOS)
clang return_42 -o return_42_exe

# Run
./return_42_exe
EXIT_CODE=$?

# Verify
if [ $EXIT_CODE -eq 42 ]; then
    echo "PASS: return 42 works"
else
    echo "FAIL: expected 42, got $EXIT_CODE"
    exit 1
fi
```

---

## Execution Order

```
┌─────────────────────────────────────────────────────────────┐
│  7.1: Wire LowerCtx to Real CLIF Data                       │
│  - Add clif_func reference to LowerCtx                      │
│  - Implement real data() method                             │
│  - TEST: Verify data() returns actual instruction info      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.2: Fix Constant Extraction (ARM64)                        │
│  - Add getImmediate() to InstructionData                    │
│  - Update lowerIconst to use real value                     │
│  - TEST: iconst 42 produces mov x0, 42                      │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.3: Implement Return Lowering (ARM64)                      │
│  - Add lowerReturn function                                 │
│  - Wire in opcode switch                                    │
│  - TEST: return emits mov x0, <val>; ret                    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.4: Wire Mach-O Generation                                 │
│  - Use macho.zig in generateMachO()                         │
│  - Add symbols for exports                                  │
│  - TEST: file output.o shows "Mach-O 64-bit object"         │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.5: Replicate for x64                                      │
│  - Same fixes in x64/lower.zig                              │
│  - Wire elf.zig in generateElf()                            │
│  - TEST: file output.o shows "ELF 64-bit"                   │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│  7.6: End-to-End Test - Return 42                            │
│  - Compile, link, run                                       │
│  - Verify exit code is 42                                   │
│  - TEST: ./return_42_exe; echo $? → 42                      │
└─────────────────────────────────────────────────────────────┘
```

---

## Verification Checklist

For EACH task, before marking complete:

- [ ] Code compiles without warnings
- [ ] Unit tests pass for the specific function
- [ ] Integration test shows correct behavior
- [ ] No `TODO`, `FIXME`, or placeholder values remain in the code path

---

## Time Estimate

This is NOT a few hours of work. Based on the gaps:

| Task | Estimated Effort |
|------|------------------|
| 7.1 Wire LowerCtx | Medium - requires understanding CLIF DFG |
| 7.2 Fix Constants | Small - once 7.1 is done |
| 7.3 Return Lowering | Medium - ABI considerations |
| 7.4 Mach-O Wiring | Medium - may need macho.zig fixes |
| 7.5 x64 Replication | Medium - similar work for different ISA |
| 7.6 E2E Test | Small - verification |

**Critical Path**: 7.1 → 7.2 → 7.3 → 7.4 → 7.6

---

## References

### Cranelift Files to Study

| Cot File | Cranelift Equivalent |
|----------|---------------------|
| `lower.zig` LowerCtx | `cranelift/codegen/src/machinst/lower.rs` Lower struct |
| `aarch64/lower.zig` | `cranelift/codegen/src/isa/aarch64/lower.rs` + `lower/isle.rs` |
| `InstructionData` | `cranelift/codegen/src/ir/instructions.rs` InstructionData enum |
| `macho.zig` | `cranelift/object/src/backend.rs` (object crate) |

### Key Cranelift Patterns

**Getting immediate from instruction**:
```rust
// Cranelift: cranelift/codegen/src/machinst/lower.rs
fn get_constant(&self, inst: Inst) -> Option<u64> {
    match self.dfg[inst] {
        InstructionData::UnaryImm { imm, .. } => Some(imm.bits() as u64),
        _ => None,
    }
}
```

**Return lowering**:
```rust
// Cranelift: cranelift/codegen/src/isa/aarch64/lower.rs
fn lower_return(&mut self, inst: Inst) {
    for (i, &ret_reg) in self.abi.ret_regs().iter().enumerate() {
        let val = self.dfg.inst_args(inst)[i];
        let src = self.put_in_reg(val);
        self.emit(MInst::mov(ret_reg, src));
    }
    self.emit(MInst::Ret);
}
```
