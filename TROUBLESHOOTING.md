# Cot Troubleshooting Framework

## Why This Document Exists

Cot has been rewritten 5 times. Each time, the failure mode is the same:
1. Claude encounters a bug
2. Claude tries to "figure out" the fix
3. Claude invents logic that doesn't match the reference implementation
4. The fix creates new bugs (whack-a-mole)
5. Project stalls

**The solution: NEVER invent. ALWAYS copy from reference.**

---

## The Golden Rule

> **If you're reasoning about what the code SHOULD do, you're doing it wrong.**
> **Find what the reference implementation DOES do, and copy it.**

---

## Reference Implementation Map

Every line of Cot is ported from somewhere. There are NO exceptions.

| Pipeline Stage | Our Code | Reference Code | Reference Location |
|----------------|----------|----------------|-------------------|
| Cot Source → AST | `compiler/frontend/` | - | Original (syntax is ours) |
| AST → SSA IR | `compiler/frontend/ssa_builder.zig` | Go SSA | `references/go/src/cmd/compile/internal/ssa/` |
| SSA Passes | `compiler/ssa/passes/` | Go SSA passes | `references/go/src/cmd/compile/internal/ssa/rewrite*.go` |
| SSA → Wasm Ops | `compiler/ssa/passes/lower_wasm.zig` | Go Wasm backend | `references/go/src/cmd/compile/internal/wasm/ssa.go` |
| Wasm Ops → Binary | `compiler/codegen/wasm/` | Go Wasm asm | `references/go/src/cmd/internal/obj/wasm/wasmobj.go` |
| **SSA → CLIF (direct)** | `compiler/codegen/native/ssa_to_clif.zig` | **rustc_codegen_cranelift** | `references/rust/compiler/rustc_codegen_cranelift/src/` |
| Wasm Binary → CLIF | `compiler/codegen/native/wasm_to_clif/` | Cranelift Wasm | `references/wasmtime/crates/cranelift/src/translate/` |
| CLIF IR Types | `compiler/ir/clif/` | Cranelift IR | `references/wasmtime/cranelift/codegen/src/ir/` |
| CLIF → MachInst | `compiler/codegen/native/machinst/` | Cranelift machinst | `references/wasmtime/cranelift/codegen/src/machinst/` |
| MachInst ARM64 | `compiler/codegen/native/isa/aarch64/` | Cranelift ARM64 | `references/wasmtime/cranelift/codegen/src/isa/aarch64/` |
| MachInst x64 | `compiler/codegen/native/isa/x64/` | Cranelift x64 | `references/wasmtime/cranelift/codegen/src/isa/x86/` |
| Register Alloc | `compiler/codegen/native/regalloc/` | regalloc2 | `references/wasmtime/cranelift/codegen/src/machinst/reg.rs` + regalloc2 crate |
| **ARC Insertion** | `compiler/frontend/arc_insertion.zig` | **Swift SILGen** | `references/swift/lib/SILGen/` (ManagedValue.h, Cleanup.h, SILGenExpr.cpp) |
| ARC Runtime | `compiler/codegen/wasm/arc.zig` | Swift ARC | `references/swift/stdlib/public/runtime/HeapObject.cpp` |

### Three Reference Codebases

The compiler draws from three distinct reference implementations. Know which one to use:

| Reference | Language | What It Covers | When to Use |
|-----------|----------|---------------|-------------|
| **Go compiler** | Go | Frontend → SSA → Wasm bytecode | SSA passes, Wasm ops, Wasm binary format |
| **Cranelift** (wasmtime) | Rust | Wasm → CLIF → MachInst → emit | Wasm-to-CLIF translation, ISA backends (ARM64/x64), register allocation, instruction emission |
| **rustc_codegen_cranelift** (cg_clif) | Rust | SSA → CLIF (direct native path) | `ssa_to_clif.zig` — how to build CLIF IR from a compiler's SSA IR using FunctionBuilder |

**For native codegen bugs:**
- If the bug is in **CLIF → machine code** (lowering, regalloc, emit): reference is **Cranelift** at `references/wasmtime/cranelift/codegen/src/`
- If the bug is in **SSA → CLIF translation** (building CLIF IR, signatures, loads/stores, function calls): reference is **cg_clif** at `references/rust/compiler/rustc_codegen_cranelift/src/`
- If the bug is in **Wasm → CLIF translation** (the indirect path): reference is **Cranelift Wasm frontend** at `references/wasmtime/crates/cranelift/src/translate/`

**Key cg_clif files:**
| File | What It Shows |
|------|---------------|
| `base.rs` | Core MIR→CLIF translation loop (block iteration, statement dispatch) |
| `value_and_place.rs` | CValue/CPlace — how values live in regs vs memory |
| `pointer.rs` | Pointer struct with Offset32 — how load/store offsets work |
| `abi/mod.rs` | Function signatures, call conventions, return handling |

### ARC Implementation: MUST Copy Swift

**⚠️ CRITICAL: ARC is complex. Swift has solved this problem. Copy their solution.**

For M17-M19 (ARC features), the reference is Swift's SILGen layer:

| Feature | Our File | Swift Reference |
|---------|----------|-----------------|
| ManagedValue (owned values) | `arc_insertion.zig` | `references/swift/lib/SILGen/ManagedValue.h:40-456` |
| Cleanup stack (deferred release) | `arc_insertion.zig` | `references/swift/lib/SILGen/Cleanup.h:85-317` |
| Emit copy/retain | `arc_insertion.zig` | `references/swift/lib/SILGen/SILGenExpr.cpp:70-109` |
| Destructor dispatch | `arc.zig` | `references/swift/stdlib/public/runtime/HeapObject.cpp:216-268` |

**DO NOT invent ARC logic.** Swift's approach:
1. `ManagedValue` pairs each value with an optional cleanup handle
2. `CleanupStack` tracks deferred releases in LIFO order
3. Scope exit emits all active cleanups in reverse order
4. Ownership transfer disables cleanup (no double-release)

Copy these patterns exactly.

---

## Debugging Methodology

### Step 1: Identify the Failing Stage

When a test fails, first determine WHERE in the pipeline it fails:

```
Cot Source
    ↓ (frontend)
SSA IR
    ├── Indirect path (default):
    │   ↓ (lower_wasm)
    │   Wasm SSA Ops
    │   ↓ (wasm codegen)
    │   Wasm Binary (.wasm)
    │   ↓ (wasm_to_clif)      ← If error mentions CLIF/blocks/values
    │   CLIF IR ─────────────────┐
    │                            │
    └── Direct path (--direct-native):
        ↓ (ssa_to_clif)        ← If error mentions CLIF/blocks/values
        CLIF IR ─────────────────┤
                                 ↓
                          (machinst/lower)    ← If error mentions MachInst/VCode
                          VCode
                          ↓ (regalloc)        ← If error mentions registers/liveness
                          Allocated VCode
                          ↓ (emit)            ← If error mentions encoding/bytes
                          Native Binary
```

**How to identify the stage:**
- Error mentions "block", "value", "params" → CLIF level (ssa_to_clif, wasm_to_clif, or ir/clif)
- Error mentions "VCode", "MachInst", "BlockIndex" → machinst level
- Error mentions "vreg", "liveness", "regalloc" → register allocation
- Error mentions "emit", "encoding" → emission level

### Step 2: Find the Exact Reference File

Once you know the stage, find the EXACT file in the reference:

**Example: "branch args must match block params" error**
- This is a CLIF-level error (mentions blocks and params)
- Our code: `compiler/codegen/native/wasm_to_clif/translator.zig`
- Reference: `references/wasmtime/crates/cranelift/src/translate/code_translator.rs`

### Step 3: Find the Exact Function

Search for the equivalent function:

```bash
# Our function
grep -n "translateEnd" compiler/codegen/native/wasm_to_clif/translator.zig

# Reference function
grep -n "Operator::End" references/wasmtime/crates/cranelift/src/translate/code_translator.rs
```

### Step 4: Line-by-Line Comparison

Create a side-by-side comparison. Look for ANY difference:

| Aspect | Reference (Cranelift) | Ours | Match? |
|--------|----------------------|------|--------|
| When is jump emitted? | Only if reachable | Only if reachable | ✓ |
| What args does jump use? | `return_args` from stack | `return_vals` from stack | ✓ |
| When is block sealed? | If `exit_is_branched_to \|\| reachable_anyway` | If `next_reachable` | ? |
| What about unreachable case? | Just set reachable=false, don't touch block | ... | ? |

### Step 5: Copy the Reference EXACTLY

If there's ANY difference, copy the reference pattern. Do not:
- "Improve" it
- "Simplify" it
- "Adapt" it for our needs

Just copy it. Translate syntax (Rust→Zig, Go→Zig) but preserve ALL logic.

### Step 6: Test

```bash
zig build test
```

If it fails, go back to Step 4. You missed something.

---

## Common Failure Patterns

### Pattern 1: "I'll figure out how this should work"

**Wrong:**
```
Claude reads the error message
Claude reasons about what the code should do
Claude writes a fix based on reasoning
```

**Right:**
```
Claude reads the error message
Claude finds the equivalent code in reference
Claude copies the reference pattern
```

### Pattern 2: "This is a simple fix"

**Wrong:**
```
Claude sees an obvious one-line fix
Claude makes the change without checking reference
Change creates subtle bugs elsewhere
```

**Right:**
```
Claude sees what looks like a simple fix
Claude checks how reference handles this case
Claude copies reference approach (even if it seems more complex)
```

### Pattern 3: "The reference is too complex, I'll simplify"

**Wrong:**
```
Claude reads reference code
Claude decides some parts are unnecessary
Claude implements simplified version
Simplified version is missing critical edge case handling
```

**Right:**
```
Claude reads reference code
Claude copies ALL of it, even parts that seem unnecessary
Tests pass because edge cases are handled
```

### Pattern 4: "Emitting ISA-specific instructions in shared code"

**Wrong:**
```
Claude needs to emit ARM64 instructions (stp, ldp, etc.)
Claude adds ARM64 machine code directly in vcode.zig
This breaks x64 builds and creates merge conflicts
```

**Right:**
```
Claude needs to emit ARM64 instructions
Claude adds them in compiler/codegen/native/isa/aarch64/ modules
Claude uses hasDecl() checks in vcode.zig to call ISA-specific methods
Shared code remains ISA-agnostic
```

**The rule:** `vcode.zig` and other `machinst/` files must NEVER contain hardcoded ARM64 or x64 instruction encodings. All ISA-specific code goes in:
- `isa/aarch64/` for ARM64
- `isa/x64/` for x64

If you need to emit prologue/epilogue, use `I.genPrologue()` / `I.genEpilogue()` patterns that dispatch to ISA-specific implementations.

---

## Debug Output

Use `COT_DEBUG` environment variable:

```bash
# See what's happening at each stage
COT_DEBUG=codegen zig build test

# Multiple stages
COT_DEBUG=parse,lower,codegen zig build test
```

Add debug output using `pipeline_debug.zig`:
```zig
const debug = @import("pipeline_debug.zig");
debug.log(.codegen, "block {d} terminator: {s}", .{block.index, @tagName(terminator)});
```

**But remember:** Debug output helps you FIND where the bug is. It doesn't help you FIX the bug. The fix comes from the reference implementation.

---

## Checklist Before Making Any Change

- [ ] I identified which pipeline stage has the bug
- [ ] I found the exact reference file for this stage
- [ ] I found the exact function in the reference
- [ ] I did a line-by-line comparison
- [ ] I found a difference between our code and reference
- [ ] My change copies the reference pattern exactly
- [ ] I did NOT invent any new logic

If you can't check all boxes, STOP. Go back and find the reference.

---

## When You're Truly Stuck

If you've followed all steps and still can't find the issue:

1. **Ask the user** - They may have context you don't
2. **Create an audit document** - Write down exactly what you compared
3. **Don't guess** - Never make changes without reference backing

Format for asking:
```
I'm stuck on [specific error].

Stage: [which pipeline stage]
Our file: [path]
Reference file: [path]
I compared: [what functions]
They appear identical in: [aspects]
I can't find the difference in: [aspects]

Should I [specific next step]?
```

---

## Updating This Document

When you solve a bug, add it to the "Common Failure Patterns" section so future Claude sessions can learn from it.
