# Cot Self-Hosting: Status, Blockers, and Path to 0.4

**Updated:** 2026-03-19
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: 9 of 13 Frontend Files Compile to Valid Wasm

**Audited 2026-03-19** — verified with clean selfcot build + test of each file.

**Build:** `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` → native binary (Success)

| File | Location | Lines | Status | Blocker |
|------|----------|------:|--------|---------|
| token.cot | parse/ | 448 | **OK** | — |
| source.cot | parse/ | 315 | **OK** | — |
| errors.cot | check/ | 545 | **OK** | — |
| ast.cot | parse/ | 1,532 | **OK** | — |
| arc.cot | build/ | 443 | **OK** | — |
| scanner.cot | parse/ | 774 | **OK** | — |
| types.cot | check/ | 1,572 | **OK** | — |
| parser.cot | parse/ | 3,258 | **OK** | — |
| ir.cot | build/ | 1,467 | SIGSEGV | Crash during codegen |
| ssa.cot | build/ | 625 | SIGSEGV | Crash during codegen |
| builder.cot | build/ | 2,364 | SIGSEGV | Crash during codegen |
| checker.cot | check/ | 5,909 | Error | Check errors on imported files |
| lower.cot | build/ | 9,201 | Error | Check errors on imported files |

**Key facts:**
- 9 files produce valid Wasm (was 0 before ARC opt_make + alloc_raw fixes)
- 3 files crash with SIGSEGV during Wasm codegen (ir, ssa, builder)
- 2 files fail with check errors on multi-file imports (checker, lower)
- After frontend (13 files), codegen/ (17 files) and main.cot still need to compile
- ~44,700 lines across 42 files

---

## Resolved Bugs

### Bug 2: Multi-Param Function Call Type Mismatch — FIXED
**Root cause:** `alloc(0, ...)` used for raw buffers (FuncParam arrays, ErrorVariant arrays). The ARC header on these allocations caused memory corruption when the allocator reused the memory. Fix: `alloc(0, ...) → alloc_raw(...)` for all raw buffer allocations in `self/check/checker.cot` and `self/check/types.cot`.

### ARC ?*T SSA Mismatch — FIXED
**Root cause:** `?*T` was 16 bytes in struct fields but 8 bytes in SSA. Fix: `opt_make`/`opt_tag`/`opt_data` SSA decomposition (Go IMake pattern).

### Ad-Hoc ARC Dispatch — FIXED
All inline retain/release replaced with centralized `emitCopyValue`/`emitDestroyValue`.

---

## Remaining Blockers

### Blocker A: 3 Files SIGSEGV During Codegen (ir, ssa, builder)

These files pass selfcot check but crash during Wasm code generation. Likely a codegen bug triggered by specific IR patterns in these files. Needs investigation.

### Blocker B: 2 Files Fail on Multi-File Import Checking (checker, lower)

`checker.cot` and `lower.cot` import files that trigger check errors. These are the largest files (5,909 and 9,201 lines) with complex dependencies. The check errors may cascade from earlier import failures.

### Blocker C: Bug 1 — Enum Method Resolution (token.cot tests only)

```
self/parse/token.cot:325:26: error[E306]: field not found
        @assertEq(Token.kw_fn.toString(), "fn")
```

This only affects test blocks in token.cot — the file compiles to Wasm successfully (tests are excluded from build). Low priority since token.cot already builds.

### Bug 3: body_check_depth Counter Leak (Latent)

`checkFnDeclBody` depth counter leaks on `orelse return`. Masked by Bug 2 fix. The natural fix triggers `NoCurrentBlock` in the Zig compiler (Bug 4).

### Bug 4: orelse Block Return Lowering (Zig Compiler)

Fix identified and tested but not committed. Needed for Bug 3.

---

## Path to 0.4

### Phase 1: Complete Frontend Compilation (current — 4 files remaining)

- [x] 9/13 frontend files compile to valid Wasm
- [ ] Fix 3 SIGSEGV crashes in codegen (ir, ssa, builder)
- [ ] Fix 2 multi-file check errors (checker, lower)
- [ ] All 13 frontend files produce valid Wasm

### Phase 2: Codegen + Main Compilation

- [ ] codegen/ files (17 files) compile via selfcot
- [ ] main.cot compiles via selfcot
- [ ] `selfcot build self/main.cot` produces working Wasm
- [ ] Validate: `wasmtime selfcot.wasm build self/test_tiny.cot` succeeds

### Phase 3: Release Polish

- [ ] Homebrew tap + x86_64-macos binary
- [ ] VS Code marketplace extension
- [ ] Error messages polish pass

---

## Key Files

| File | Purpose |
|------|---------|
| `self/main.cot` | Selfcot entry point + multi-file pipeline |
| `self/check/checker.cot` | Type checker (~5,900 lines) — Bugs 1, 3 live here |
| `self/build/lower.cot` | IR lowering (~9,200 lines, most complex) |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (ARC dispatch, opt_make integration) |
| `compiler/frontend/ssa_builder.zig` | SSA builder (opt_make/opt_tag/opt_data decomposition) |
| `compiler/codegen/native/dwarf.zig` | DWARF debug info (subprograms, variables, types, frame) |
