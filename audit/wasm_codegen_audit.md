# Wasm Codegen Audit: Go vs Cot

## Summary

Comparing `~/learning/go/src/cmd/compile/internal/wasm/ssa.go` with `compiler/codegen/wasm_gen.zig`.

**STATUS: NEW GO-STYLE INFRASTRUCTURE IMPLEMENTED, NEEDS DEBUGGING**

The new Go-style two-pass architecture has been implemented in `compiler/codegen/wasm/`:
- gen.zig: Generates Prog chains from SSA with pseudo-jumps ✓
- preprocess.zig: Transforms pseudo-jumps, adds dispatch loop ✓
- assemble.zig: Converts Prog chains to bytecode ✓

**CURRENT ISSUE:** Control flow tests (while loops) fail with stack balance errors.
The dispatch loop pattern is generating invalid wasm (expected 0/1 elements on stack).

**WORKAROUND:** Driver currently uses old wasm_gen.zig. New module available via `wasm.generateFunc()`.

**NEXT STEPS:**
1. Debug the resume_point to .end conversion
2. Fix stack balance in dispatch loop block structure
3. Test with simple while loop until it passes
4. Then test while_continue (the original failing test)

Previous audit incorrectly claimed dispatch loop wasn't needed. It IS needed for all
block-to-block jumps in Wasm's structured control flow, not just goroutines.

## Go's Two-Pass Architecture

Go's Wasm codegen uses TWO files:

1. **ssa.go** (cmd/compile/internal/wasm/ssa.go)
   - `ssaGenValue`: emit Wasm ops for SSA values
   - `ssaGenBlock`: emit PSEUDO-JUMPS (obj.AJMP), not real branches

2. **wasmobj.go** (cmd/internal/obj/wasm/wasmobj.go)
   - `preprocess`: transform pseudo-instructions
   - Lines 394-404: AJMP → set PC_B, br to entryPointLoop
   - Lines 690-724: create dispatch structure (loop + blocks + br_table)
   - Lines 726-747: compute relative branch depths

## The Dispatch Loop Pattern

Go wraps EVERY function body in this structure:

```wasm
loop $entryPointLoop
  block $b0
    block $b1
      ...
      block $bN
        local.get $PC_B
        br_table 0 1 2 ... N  ;; dispatch based on PC_B
      end  ;; PC_B=N lands here, block N code follows
      ...
    end  ;; PC_B=1 lands here
    ...
  end  ;; PC_B=0 lands here
  ...
end  ;; entryPointLoop
```

ALL block-to-block jumps become:
```wasm
i32.const <target_block_index>
local.set $PC_B
br <depth_to_entryPointLoop>
```

## Why Cot Needs This

Wasm has STRUCTURED control flow - you can only branch to enclosing constructs.
Without the dispatch loop, forward jumps to non-adjacent blocks are impossible.

The broken code at wasm_gen.zig:499-501:
```zig
} else {
    // Forward jump to non-return block - emit br 0 and hope for the best
    // This is not correct for all cases but handles simple fallthrough
    try self.code.emitBr(0);
}
```

This CANNOT be fixed with simple patches. The architecture must change.

## Implementation Plan

1. Use the existing `wasm/` directory infrastructure:
   - `wasm/prog.zig` - Prog instruction type (like Go's obj.Prog)
   - `wasm/preprocess.zig` - Transform pseudo-instructions
   - `wasm/assemble.zig` - Emit binary

2. Rewrite `wasm_gen.zig` to emit pseudo-jumps:
   - ssaGenBlock emits AJMP with symbolic targets
   - Don't compute depths inline

3. Complete preprocess.zig with dispatch loop:
   - Copy Go's lines 394-404 (AJMP transform)
   - Copy Go's lines 690-724 (dispatch structure)
   - Copy Go's lines 726-747 (depth calculation)

## Functions That Match Go (Keep These)

| Function | Go Location | Status |
|----------|-------------|--------|
| `getValue64` | lines 491-503 | ✅ Matches |
| `getValue32` | lines 474-489 | ✅ Matches |
| `setReg` | lines 530-533 | ✅ Matches |
| `isCmp` | lines 463-472 | ✅ Matches |
| `ssaGenValueOnStack` | lines 313-461 | ✅ Matches |
| `ssaGenValue` | lines 217-311 | ✅ Matches |

## Functions That Must Be Rewritten

| Function | Issue |
|----------|-------|
| `ssaGenBlock` | Must emit AJMP pseudo-jumps, not real br |
| `emitBranchTo` | DELETE - replaced by pseudo-jumps |
| `generate` | Must call preprocess after emitting pseudo-jumps |
| `findLoopHeaders` | DELETE - dispatch loop handles all cases |

## Reference Files

- Go ssa.go: `~/learning/go/src/cmd/compile/internal/wasm/ssa.go`
- Go wasmobj.go: `~/learning/go/src/cmd/internal/obj/wasm/wasmobj.go`
- Our Prog type: `compiler/codegen/wasm/prog.zig`
- Our preprocess: `compiler/codegen/wasm/preprocess.zig`
