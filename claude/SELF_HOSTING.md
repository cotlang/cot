# Cot Self-Hosting: Status, Blockers, and Path to 0.4

**Updated:** 2026-03-20
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: 9 of 13 Frontend Files Compile to Valid Wasm

**Audited 2026-03-20** — verified with clean selfcot build + test of each file.

**Build:** `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` → native binary (Success)

| File | Location | Lines | Status | Blocker |
|------|----------|------:|--------|---------|
| token.cot | parse/ | 451 | **OK** | — |
| source.cot | parse/ | 315 | **OK** | — |
| errors.cot | check/ | 545 | **OK** | — |
| ast.cot | parse/ | 1,533 | **OK** | — |
| arc.cot | build/ | 443 | **OK** | — |
| scanner.cot | parse/ | 774 | **OK** | — |
| types.cot | check/ | 1,609 | **OK** | — |
| parser.cot | parse/ | 3,260 | **OK** | — |
| ir.cot | build/ | 1,467 | SIGSEGV | Crash in lowerGenericFnInstance during lowering |
| ssa.cot | build/ | 625 | SIGSEGV | Same crash — generic fn instantiation |
| builder.cot | build/ | 2,364 | SIGSEGV | Same crash — generic fn instantiation |
| checker.cot | check/ | 5,981 | Error | `undefined type 'Scope'` — missing import |
| lower.cot | build/ | 9,201 | Error | Cascades from checker.cot import errors |

**Key facts:**
- 9 files produce valid Wasm
- 3 files crash with SIGSEGV in `Lowerer_lowerGenericFnInstance` (ir, ssa, builder) — same root cause
- 2 files fail with check errors: `Scope` type not found (checker.cot imports a file that doesn't export `Scope`)
- After frontend (13 files), codegen/ (17 files) and main.cot still need to compile
- ~43,600 lines across 42 files

### Recent Changes (2026-03-19 → 2026-03-20)

**`distinct` type feature** — fully implemented in both Zig compiler and selfcot:
- Syntax: `type RawPtr = distinct i64` — Go named type model
- Selfcot handles: parsing, type-checking, constructor calls (`RawPtr(expr)`), binary ops (`RawPtr + i64`)
- Applied to stdlib: `alloc_raw` → `RawPtr`, `dealloc_raw(RawPtr)` — compile-time safety

**Debug crash traces** — Go + Zig pattern:
- Release: Go-style pctab gives `at file.cot:42` for every crash (always embedded)
- Debug (`--debug`): DWARF `.debug_line` reader gives `at file.cot:42:13` with column numbers
- Per-frame resolution: every backtrace frame shows `at file:line`
- Multi-file support: imported files get correct source locations

**Crash fixes in selfcot:**
- Fixed `dealloc()` on `alloc_raw()` memory in `editDistance()` (heap corruption)
- Fixed Map state read as `*u8` instead of `*i64` in `findSimilarType()` (wrong slot access)

---

## Resolved Bugs

### Bug 5: `dealloc()` on `alloc_raw()` memory — FIXED (2026-03-19)
**Root cause:** `editDistance()` in `checker.cot` allocated with `alloc_raw()` (no ARC header) but freed with `dealloc()` (expects 32-byte ARC header at ptr-32). Now **impossible to reintroduce** — `alloc_raw` returns `RawPtr` (distinct type), `dealloc` takes `i64`. Mismatch is a compile error.

### Bug 6: Map states read as `*u8` instead of `*i64` — FIXED (2026-03-19)
**Root cause:** `findSimilarType()` iterated Map internal arrays reading states as single bytes instead of 8-byte i64 values. Fix: `@intToPtr(*u8, states + ki)` → `@intToPtr(*i64, states + ki * @sizeOf(i64))`.

### Bug 2: Multi-Param Function Call Type Mismatch — FIXED
**Root cause:** `alloc(0, ...)` used for raw buffers. Fix: `alloc(0, ...) → alloc_raw(...)`.

### ARC ?*T SSA Mismatch — FIXED
**Root cause:** `?*T` was 16 bytes in struct fields but 8 bytes in SSA. Fix: `opt_make`/`opt_tag`/`opt_data` SSA decomposition.

### Ad-Hoc ARC Dispatch — FIXED
All inline retain/release replaced with centralized `emitCopyValue`/`emitDestroyValue`.

---

## Remaining Blockers

### Blocker A: 3 Files SIGSEGV in lowerGenericFnInstance (ir, ssa, builder)

All three crash at the same point during lowering (not codegen). The crash trace shows:
```
SIGSEGV: segmentation fault
  at self/check/errors.cot:58
  Lowerer_lowerGenericFnInstanceInner
  Lowerer_lowerGenericFnInstance
  Lowerer_lowerQueuedGenericFunctions
  Lowerer_lowerToBuilder
```

The crash is in `ErrorReporter_reportError` — the ErrorReporter's internal list has a corrupt pointer. This happens during cross-file generic function instantiation: the lowerer re-checks a generic body from a different file, and the ErrorReporter belongs to the wrong file's context.

**Root cause hypothesis:** When a generic function from `list.cot` or `map.cot` is instantiated during lowering of `ir.cot`, the ErrorReporter used is `ir.cot`'s reporter. But if the generic body has a type error, the reporter tries to print using byte offsets from the defining file's AST, not the instantiating file's AST. This corrupts the error output.

**Investigation needed:** Reproduce with a minimal multi-file test that triggers generic instantiation + type error in the generic body.

### Blocker B: 2 Files Fail on Multi-File Import Checking (checker, lower)

`checker.cot` fails with: `undefined type 'Scope', did you mean 'Shape'?` at line 131. The `Scope` struct is defined in a separate file (`self/check/scope.cot` or similar) that selfcot doesn't resolve during import checking. This cascades to `lower.cot` which imports checker.cot.

**Fix needed:** Ensure all type dependencies are properly imported/exported across selfcot's multi-file checking pipeline.

### Blocker C: Bug 1 — Enum Method Resolution (token.cot tests only)

`Token.kw_fn.toString()` fails — only affects test blocks, not builds. Low priority.

### Bug 3: body_check_depth Counter Leak (Latent)

`checkFnDeclBody` depth counter leaks on `orelse return`. Masked by Bug 2 fix.

### Bug 4: orelse Block Return Lowering (Zig Compiler)

Fix identified and tested but not committed. Needed for Bug 3.

---

## Path to 0.4

### Phase 1: Complete Frontend Compilation (current — 5 files remaining)

- [x] 8/13 frontend files compile to valid Wasm (token, source, errors, ast, arc, scanner, types, parser)
- [ ] Fix SIGSEGV in lowerGenericFnInstance (unblocks ir, ssa, builder → 11/13)
- [ ] Fix `Scope` import resolution (unblocks checker, lower → 13/13)
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
| `self/check/checker.cot` | Type checker (~5,980 lines) — Scope import issue |
| `self/build/lower.cot` | IR lowering (~9,200 lines, most complex) — generic instantiation crash |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (ARC dispatch, opt_make integration) |
| `compiler/frontend/ssa_builder.zig` | SSA builder (opt_make/opt_tag/opt_data decomposition) |
| `compiler/codegen/native/dwarf.zig` | DWARF writer (subprograms, variables, types, frame) |
| `compiler/codegen/native/dwarf_reader.zig` | DWARF runtime reader (crash file:line:col) |
| `compiler/codegen/native/signal_native.zig` | Signal handler + pctab decoder + per-frame resolution |
