# br_table Jump Table Bug: Labels Resolve to Body Blocks Instead of Intermediate Blocks

## Summary

When a function contains a `br_table` dispatch loop (triggered by modulo/division or any resume point), the x64 jump table entries point directly to **body blocks** instead of **intermediate blocks**. The intermediate blocks contain regalloc edge moves that copy function arguments to the correct registers. Since the jump table bypasses these intermediate blocks, register values are wrong and functions return garbage.

**This bug causes 13 of 82 control_flow test failures on x64 Linux.** All 13 involve modulo or division, which trigger the dispatch loop pattern.

## Reproduction

```cot
fn test_it(n: i64) i64 {
    var i: i64 = 0;
    while i < 0 {
        var r: i64 = i % 2;
        i = i + 1;
    }
    return n;
}
fn main() i64 {
    return test_it(5);
}
```

Expected: returns 5. Actual: returns 72 (0x48).

The loop never executes (`i < 0` is false when `i = 0`), but the presence of `%` triggers a dispatch loop with `br_table`. The value 72 = 0x48 is the jump table offset that leaks into the return value.

## Architecture: How br_table Dispatch Works

Cot follows Go's dispatch loop pattern (see `docs/BR_TABLE_ARCHITECTURE.md`). When a function has resume points (calls, divisions with traps), the Wasm→CLIF translator creates:

1. **Dispatch block**: Contains `br_table` that jumps to the right resume point based on a selector
2. **Intermediate blocks**: Created by `translateBrTable()` when `jump_args_count > 0`. Each intermediate block has NO block params and contains a single `jump(body_block, args)` instruction
3. **Body blocks**: The actual code blocks that expect block parameters

The flow should be:
```
dispatch block → br_table → intermediate block → (edge moves) → body block
```

But on x64, the jump table skips the intermediate blocks:
```
dispatch block → br_table → (SKIPS intermediate) → body block (wrong register values)
```

## GDB Evidence

Disassembly of `test_it` at 0x1003120:

```
# Dispatch: jump table lookup
100312e: cmp    rsi, 0x6          # bounds check
1003132: jae    1003189           # default (ud2)
1003138: lea    rax, [rip+0x9]    # rax = jump table base (0x1003148)
100313f: movslq rcx, [rax+rsi*4] # rcx = jt[rsi] = 0x48
1003143: add    rax, rcx          # rax = 0x1003148 + 0x48 = 0x1003190
1003146: jmp    *rax              # jump to 0x1003190 (BODY block)

# Jump table at 0x1003148 (7 entries: 6 table + 1 default):
# Entry 0: 0x48 → 0x1003190 (body block)    ← WRONG, should be 0x1003164
# Entry 1: 0x48 → 0x1003190 (body block)    ← WRONG, should be 0x1003164
# Entry 2: 0x115 → 0x100325d (body block)
# ...

# Intermediate blocks at 0x1003164 (UNREACHABLE - jump table skips them):
1003164: mov    rcx, rdx          # edge move: copy arg n (rdx) to rcx
1003167: jmp    1003190           # then jump to body block

# Body block at 0x1003190:
1003190: mov    rax, r15          # starts executing with WRONG rcx
# ... eventually stores rcx (0x48) as n instead of 5
```

GDB confirms:
- At function entry: `rdx = 5` (correct argument)
- At store of `n`: `rcx = 0x48 = 72` (WRONG - should be 5)
- The intermediate block breakpoint at 0x1003164 is NEVER hit

## Root Cause Analysis

The issue is in how `blockLabel()` resolves for intermediate blocks. The chain:

1. `translateBrTable()` in `translator.zig:756-805` creates intermediate CLIF blocks and puts them in the jump table
2. x64 `lowerBranch` for `br_table` in `x64/lower.zig:319-397` calls `ctx.blockLabel(target_block)` for each jump table entry
3. `blockLabel()` in `machinst/lower.zig:1118` calls `loweredIndexForBlock(block)` which looks up `blockindex_by_block`
4. `blockindex_by_block` is populated during RPO walk in `blockorder.zig:679-707`

**The labels resolve to the WRONG offsets.** The intermediate block labels resolve to body block addresses instead of intermediate block code addresses.

From debug tracing of the emission loop:
```
BLOCK 0: offset=0x0, insns=18   ← dispatch block (includes jump table DATA)
BLOCK 1: offset=0x54, insns=1   ← starts AFTER intermediate blocks' code!?
```

Block 0 extends from offset 0x0 to 0x54. The intermediate block code (`mov rcx, rdx; jmp`) appears at offset 0x44 within block 0's range. This means the intermediate blocks' edge moves are being emitted as regalloc edits of the DISPATCH block, not as part of their own blocks.

## Key Question for ARM64 Investigation

**ARM64 works correctly.** This means either:

1. ARM64's block ordering places intermediate blocks differently
2. ARM64's label resolution works differently
3. ARM64's regalloc edge moves go to the right blocks
4. The bug is x64-specific in some other way

**Please investigate on ARM64:**

1. Compile the reproduction case above to native on ARM64
2. Disassemble the `test_it` function
3. Check: do the jump table entries point to intermediate blocks (with edge moves) or directly to body blocks?
4. Trace the block ordering: run with debug output to see how many lowered blocks there are and which CLIF blocks they correspond to
5. Compare the lowered block order between ARM64 and x64

## Files to Investigate

| File | Purpose |
|------|---------|
| `compiler/codegen/native/wasm_to_clif/translator.zig:714-810` | `translateBrTable` - creates intermediate blocks |
| `compiler/codegen/native/machinst/blockorder.zig:593-776` | Block ordering - RPO walk, critical edge splitting |
| `compiler/codegen/native/machinst/lower.zig:1118-1121` | `blockLabel()` - maps CLIF block to MachLabel |
| `compiler/codegen/native/machinst/lower.zig:1241-1337` | `lowerBranchBlockparamArgs` / `collectBlockCall` |
| `compiler/codegen/native/machinst/vcode.zig:1062-1170` | Emission loop - binds labels and emits code |
| `compiler/codegen/native/isa/x64/lower.zig:319-397` | x64 br_table lowering |
| `compiler/codegen/native/isa/aarch64/lower.zig` | ARM64 br_table lowering (reference) |
| `compiler/codegen/native/isa/x64/inst/emit.zig:1976-2041` | `jmp_table_seq` emission |

## Temporary Debug Code

There is temporary debug output in:
- `machinst/vcode.zig` - prints block index, offset, instruction count during emission
- `machinst/lower.zig` - prints lowered block order with CLIF block mapping
- `isa/x64/lower.zig` - prints br_table entry→label mapping

Remove all `std.debug.print` calls and surrounding blocks once the bug is fixed.

## Other Fix Applied This Session

`lowerTrapnz` and `lowerTrapz` in `x64/lower.zig` were fixed to match ARM64:
- Old: ignored input, emitted `trap_if` without setting flags
- New: `putInputInRegs` + `test_rmi_r` + `trap_if` (matches ARM64 pattern)
- This fix is correct but did NOT resolve the 13 test failures
