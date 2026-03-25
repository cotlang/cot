# br_table Architecture in the Cot Compiler Pipeline

**Created**: February 2026
**Purpose**: Document the proven architecture for br_table to prevent repeated confusion during debugging.

---

## Executive Summary

The br_table instruction appears in Cot-generated WebAssembly because **Cot copies the Go compiler's dispatch loop pattern**. This is intentional, proven architecture - NOT a bug.

**Key insight**: br_table is generated when a function has "resume points" (calls at top level). For simple functions without calls, br_table is NOT generated.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Cot Source Code                               │
│   fn check(n: i64) i64 { if n > 1 { return 99 } return 0 }          │
└───────────────────────────────────┬─────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│                  Cot → Wasm (Go compiler pattern)                    │
│                                                                      │
│   IF numResumePoints > 0:                                           │
│     → Generates dispatch loop with br_table                         │
│   ELSE:                                                              │
│     → Simple Wasm with if/br/br_if (NO br_table)                    │
└───────────────────────────────────┬─────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│                  Wasm → CLIF (Cranelift pattern)                     │
│                                                                      │
│   br_table instruction → JumpTable + br_table CLIF instruction      │
└───────────────────────────────────┬─────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────┐
│                CLIF → ARM64 (Cranelift pattern)                      │
│                                                                      │
│   br_table CLIF → jt_sequence pseudo-instruction:                   │
│     1. B.HS default       (bounds check)                            │
│     2. CSEL rtmp2, XZR, ridx, HS  (Spectre mitigation)             │
│     3. CSDB               (speculation barrier)                     │
│     4. ADR rtmp1, #16     (table address)                          │
│     5. LDR rtmp2, [rtmp1, rtmp2, UXTW #2]  (load offset)          │
│     6. ADD rtmp1, rtmp1, rtmp2  (compute target)                   │
│     7. BR rtmp1           (indirect branch)                         │
│     8. [jump table data]  (32-bit offsets)                         │
└─────────────────────────────────────────────────────────────────────┘
```

---

## When is br_table Generated?

### br_table IS generated when:
- Function has **resume points** (top-level calls)
- Function uses **coroutines/async**
- Function has **multiple basic blocks that need dynamic dispatch**

### br_table is NOT generated when:
- Simple functions with only if/else and returns
- Functions with no calls
- Leaf functions

### Code that triggers br_table (preprocess.zig:265):
```zig
if (entryPointLoopBranches.items.len > 0 or numResumePoints > 0) {
    // ... generates dispatch loop with br_table
}
```

---

## Reference Implementations

### 1. Go Compiler (Cot → Wasm source)

**File**: `references/go/src/cmd/internal/obj/wasm/wasmobj.go` lines 690-724

```go
// Go's dispatch loop pattern (lines 704-711):
if numResumePoints > 0 {
    // Add Block instructions for resume points and BrTable to jump to selected resume point.
    for i := 0; i < numResumePoints+1; i++ {
        p = appendp(p, ABlock)
    }
    p = appendp(p, AGet, regAddr(REG_PC_B)) // read next basic block from PC_B
    p = appendp(p, ABrTable, obj.Addr{Val: tableIdxs})
    p = appendp(p, AEnd) // end of Block
}
```

**Cot's copy**: `compiler/codegen/wasm/preprocess.zig` lines 279-300

### 2. Cranelift (Wasm → CLIF source)

**File**: `references/wasmtime/crates/cranelift/src/translate/code_translator.rs` lines 485-569

```rust
Operator::BrTable { targets } => {
    // ... creates JumpTable and emits br_table CLIF instruction
    let jt = builder.create_jump_table(JumpTableData::new(block, &data));
    builder.ins().br_table(val, jt);
}
```

**Cot's copy**: `compiler/codegen/native/ssa_to_clif.zig` (br_table handling in SSA → CLIF translation)

### 3. Cranelift ARM64 Backend (CLIF → Native source)

**File**: `references/wasmtime/cranelift/codegen/src/isa/aarch64/lower.rs`

**Cot's copy**: `compiler/codegen/native/isa/aarch64/lower.zig` lines 282-369

---

## The Dispatch Loop Pattern (Go Compiler)

The Go compiler uses a clever pattern to handle control flow in Wasm:

```
loop {                          ← entryPointLoop
    block {                     ← resume point 0
        block {                 ← resume point 1
            block {             ← resume point N
                get PC_B        ← read current program counter
                br_table [...]  ← jump to correct resume point
            } end
            ; ... code for resume point N ...
        } end
        ; ... code for resume point 1 ...
    } end
    ; ... code for resume point 0 ...
} end
unreachable
```

**Why this pattern?**
1. Wasm doesn't have arbitrary goto - only structured control flow
2. This pattern allows "jumping" between basic blocks using br_table
3. It's used by Go for coroutines, defer, and calls that may suspend

---

## Debugging Guide

### If you see br_table in generated code:

1. **First check**: Does the source function have calls?
   - YES → br_table is expected (dispatch loop pattern)
   - NO → br_table should NOT be present - investigate

2. **Verify the flow**:
   - Check `numResumePoints` in preprocess.zig
   - If 0, br_table should not be generated

3. **Common misunderstanding**:
   - br_table for a simple `if n > 1 { return 99 }` means the function
     must have calls somewhere that create resume points

### Native crash in jt_sequence:

The jt_sequence emits this ARM64 sequence:
```asm
B.HS default          ; bounds check (index >= table_size)
CSEL rtmp2, XZR, ridx, HS  ; Spectre mitigation
CSDB                  ; speculation barrier
ADR rtmp1, #16        ; address of jump table
LDR rtmp2, [rtmp1, rtmp2, UXTW #2]  ; load offset
ADD rtmp1, rtmp1, rtmp2  ; compute target
BR rtmp1              ; indirect jump
[4-byte offsets...]   ; jump table data
```

If crashing in jt_sequence:
1. Check the index register (ridx) value
2. Check the jump table has correct offsets
3. Verify labels are correctly resolved

---

## Verification Tests

### Test 1: Simple function without calls should NOT have br_table

```cot
fn simple(n: i64) i64 { return n + 1 }
```
→ Wasm should NOT contain br_table opcode (0x0E)

### Test 2: Function with call SHOULD have br_table

```cot
fn helper() i64 { return 42 }
fn caller() i64 { return helper() }
```
→ Wasm for `caller` SHOULD contain br_table opcode (0x0E)

---

## File Locations

| Component | File | Lines |
|-----------|------|-------|
| Dispatch loop generation | `compiler/codegen/wasm/preprocess.zig` | 265-315 |
| br_table assembly | `compiler/codegen/wasm/assemble.zig` | 267-275 |
| SSA → CLIF translation | `compiler/codegen/native/ssa_to_clif.zig` | br_table handling |
| CLIF → ARM64 lowering | `compiler/codegen/native/isa/aarch64/lower.zig` | 282-369 |
| ARM64 jt_sequence emit | `compiler/codegen/native/isa/aarch64/inst/emit.zig` | 2605-2634 |

---

## Conclusion

The br_table architecture is **proven and correct**:
- Copied from Go compiler for Wasm generation
- Copied from Cranelift for native compilation
- Both are production-quality implementations

If br_table appears unexpectedly, the issue is likely:
1. A function has calls when you didn't expect
2. Resume point counting is incorrect
3. Something else is creating resume points

**Do NOT "fix" br_table by removing it** - instead, investigate why resume points are being created.
