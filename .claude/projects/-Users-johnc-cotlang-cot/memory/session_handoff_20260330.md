---
name: session_handoff_20260330
description: Major 0.4.1 session — CIR architecture, Cranelift backend, Gap 9, libcot cleanup in progress
type: project
---

## Session: 2026-03-30 — v0.4.1 Release + Architecture Cleanup

### What was accomplished:
1. **CIR format rewrite**: call instructions now carry callee parameter types from CLIF signature (no more all-I64 guessing)
2. **78/78 e2e tests pass** through real Cranelift (rust/libclif)
3. **370/370 features pass** — fixed u32 constant sign-extension, multi-return, float ABI, func_addr, call_indirect args
4. **Gap 9 CLOSED**: native async state machines — 618/618 concurrency tests, 50/50 Swift parity
5. **v0.4.1 released** — CI green, Homebrew updated
6. **compiler/ archived** to cotlang/cot-0.4, removed from main repo
7. **zig/libclif/ extracted** — standalone hand-ported Cranelift backend with C ABI (clif_compile, clif_free, clif_version)
8. **`--backend=zig|cranelift` CLI flag** added
9. **Documentation cleanup** — 31 stale docs archived, CLAUDE.md rewritten
10. **ARC audit** — confirmed 1:1 Swift parity, selfcot2 crash is codegen bug not ARC design

### CURRENT STATE: BUILD IS BROKEN

**driver.zig imports were partially rewritten** (lines 30-60) but the function bodies still reference the old imports. ~33 compile errors.

**See `claude/HANDOFF_LIBCOT_CLEANUP.md`** for exact state, what to fix, and step-by-step plan.

### The cleanup goal:
Remove `codegen/native/` (68K lines) and `ir/clif/` (6K lines) from libcot. libcot should produce CIR bytes only — no CLIF IR, no native codegen. All native compilation goes through `clif_compile()` C ABI to rust/libclif.

### New files already written (ready to use):
- `codegen/ssa_to_cir.zig` (2,254 lines) — direct SSA → CIR emitter
- `codegen/arc_runtime.zig` (1,646 lines) — ARC runtime as CIR
- `codegen/io_runtime.zig` (847 lines) — I/O runtime as CIR
- `codegen/print_runtime_native.zig` (398 lines) — print runtime as CIR
- `codegen/test_runtime_native.zig` (323 lines) — test runtime as CIR
- `codegen/signal_runtime.zig` (536 lines) — signal handler as CIR
- `codegen/libclif.zig` (54 lines) — C ABI binding (copied from native/)

### Quick fix to restore build:
Replace the new imports at driver.zig lines 30-60 with the old imports (see handoff doc for exact code). The old codegen/native/ files still exist.

### Key files:
- `claude/HANDOFF_LIBCOT_CLEANUP.md` — detailed cleanup plan
- `claude/RELEASE_0_4_1.md` — release plan (Steps 1-3 done, Step 4 validated)
- `claude/ARC_AUDIT.md` — ARC parity audit results
