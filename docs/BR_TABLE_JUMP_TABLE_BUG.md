# br_table Jump Table Bug: x64 SIB R13 Base Encoding

## Status: RESOLVED (Feb 9, 2026)

Fixed by handling the x64 SIB byte special case where `base & 7 == 5` (RBP/R13) requires `mod=01` with a zero displacement byte instead of `mod=00`.

## What Happened

The batch test (all test files compiled into one native binary) crashed with SIGSEGV on x64 when executing the 9-param `sum9` function. GDB showed the crash at the MOVSXD instruction in the jump table dispatch: `movslq 0x41dd0149(,%rbx,4),%rbx` — the CPU interpreted the addressing mode as `[disp32 + rbx*4]` (no base register) instead of `[r13 + rbx*4]`.

**Root cause:** The `jmp_table_seq` emission in `x64/inst/emit.zig` manually encoded the MOVSXD instruction with SIB addressing but didn't handle the x64 encoding special case where `base & 7 == 5` (RBP/R13). When `mod=00` and SIB `base=101`, x64 interprets this as "no base + disp32" instead of `[base + index*scale]`.

**Why batch-only:** In batch mode with hundreds of functions, register pressure is higher, making regalloc more likely to assign R13 to the `tmp1` temporary in the jump table sequence.

**Why ARM64 works:** ARM64 register encoding has no special cases — all 32 registers work identically.

## The Fix

In `compiler/codegen/native/isa/x64/inst/emit.zig`, the MOVSXD encoding in `jmp_table_seq` now detects `tmp1_enc & 7 == GprEnc.RBP` and emits `mod=01` with a zero displacement byte, matching the pattern already used by `emitModrmSibDisp()` at line 714.

## Historical Context

This bug went through two phases of investigation:
1. **Phase 1 (commit `61be134`):** Fixed critical-edge-aware successor labels in br_table lowering, resolving 13 control_flow test failures
2. **Phase 2 (this fix):** The remaining batch test SIGSEGV was a separate x64 encoding bug (SIB R13 special case), not a label resolution issue

## References

| Pattern | Reference |
|---------|-----------|
| SIB base=5 handling | `emitModrmSibDisp()` in `x64/inst/emit.zig`, line 714 |
| x64 encoding rules | Intel SDM Vol. 2A, Table 2-3: 32-Bit Addressing with SIB |
