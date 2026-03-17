# Cot Self-Hosting: Status, Blockers, and Path to 0.4

**Updated:** 2026-03-18
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: 10 of 13 Frontend Files Produce Valid Wasm

| File | Lines | Wasm Output | Check | Build Exit | Blocker |
|------|-------|-------------|-------|------------|---------|
| token.cot | 448 | 6,214 bytes | Pass | 139 (cleanup) | ARC cleanup |
| scanner.cot | 774 | 29,325 bytes | Pass | 139 (cleanup) | ARC cleanup |
| source.cot | 315 | 13,152 bytes | Pass | 139 (cleanup) | ARC cleanup |
| errors.cot | 545 | 21,362 bytes | Pass | 139 (cleanup) | ARC cleanup |
| types.cot | 1,575 | 55,943 bytes | Pass | 139 (cleanup) | ARC cleanup |
| ast.cot | 1,541 | 27,499 bytes | Pass | 139 (cleanup) | ARC cleanup |
| ir.cot | 1,451 | 91,942 bytes | Pass | 139 (cleanup) | ARC cleanup |
| parser.cot | 3,235 | 180,321 bytes | Pass | 139 (cleanup) | ARC cleanup |
| ssa.cot | 619 | 75,994 bytes | Pass | 139 (cleanup) | ARC cleanup |
| arc_insertion.cot | 414 | — | Pass | 139 | ARC ?*T in lowering |
| ssa_builder.cot | 2,337 | — | Pass | 139 | ARC ?*T in lowering |
| checker.cot | 5,947 | — | Fail | 139 | Self-referential struct |
| lower.cot | 9,177 | — | Fail | 139 | Self-referential struct |

**Key facts:**
- 10 files produce valid wasm (verified with `wasmtime`)
- ALL files exit 139 (SIGSEGV) during process cleanup — the wasm output is correct
- 2 files (checker, lower) fail because selfcot's checker can't resolve `?*Scope` in the self-referential `Scope` struct
- 2 files (arc_insertion, ssa_builder) pass check but crash during Phase 3 lowering

After frontend, codegen/ (16 files) and main.cot still need to compile.

---

## Blocker 1: ARC Optional Pointer Handling (?*T)

**Status:** Partially fixed. Core issue identified and mitigated but not fully resolved.

Cot's optional managed pointers (`?*T`) have compound layout: 8-byte tag + 8-byte pointer = 16 bytes. The ARC runtime's `retain()` and `release()` expect a raw 8-byte pointer. When `?*T` values are passed to retain/release, the function receives the tag value (0 or 1) instead of the pointer, causing SIGSEGV at address `tag - 32`.

### What's Been Fixed (this session)

1. **Cleanup stack emission** — unwraps `?*T` before release: check tag, if non-null extract pointer at offset +8, release the inner pointer. Port of Swift `LoadableEnumTypeLowering::emitDestroyValue` (TypeLowering.cpp:1603).

2. **emitFieldReleases / emitPendingAutoDeinits** — unwraps `?*T` struct fields before releasing in deinit functions.

3. **Local assignment ARC** — skips `?*T` (handled by cleanup unwrap).

4. **Return value retain** — skips `?*T`.

5. **Var init ARC** — skips `?*T`.

6. **Field assignment ARC** — uses `.pointer` check instead of `couldBeARC` to exclude optionals.

7. **ARC range check** — `retain`/`release` skip values < 4096 (null page). Port of Swift `isValidPointerForNativeRetain` (EmbeddedRuntime.swift:431).

### What Remains

**6 struct field init sites** still use `couldBeARC(struct_field.type_idx)` which includes `?*T`. These emit `retain(optional_value)` during struct literal initialization, passing the compound 16-byte optional to retain. Each site needs `and self.type_reg.get(struct_field.type_idx) != .optional` added to the guard.

Lines: 2793, 2857, 5840, 5899, 6195, 6259 in `compiler/frontend/lower.zig`.

**The exit-139 crashes on all files** are from ARC cleanup releasing `?*T` locals at process exit. The cleanup unwrap code generates branch blocks during cleanup emission. This works for the wasm output but causes the selfcot native binary to crash during its own cleanup. The wasm output is unaffected — the crash is cosmetic for dogfooding.

### ARC Diagnostics (implemented this session)

- `ARC: bad ptr N` — retain called on non-heap pointer (wrong magic)
- `ARC: bad rel N` — release called on non-heap pointer
- `ARC: double-free N` — release called on object already being deinitialized
- Poison magic `0xDEADDEADDEADDEAD` written to header on dealloc (use-after-free detection)
- Range check: skip retain/release for pointers < 4096 (catches optional tags)

---

## Blocker 2: Self-Referential Struct Support in Selfcot

**Status:** Not started. Blocks checker.cot and lower.cot.

The selfcot's checker doesn't support self-referential struct types. The Scope struct (`struct Scope { parent: ?*Scope, ... }`) triggers "undefined type 'Scope'" because the selfcot resolves field types before registering the struct name.

The Zig compiler has this support (commit 137149a): register a placeholder type BEFORE resolving fields, then update the placeholder after. This needs to be ported to `self/frontend/checker.cot`.

**Impact:** checker.cot and lower.cot can't be compiled because they import checker.cot which defines the Scope struct. Once self-referential support is ported, these 2 files should compile (they already pass with the Zig compiler).

---

## Blocker 3: Struct Method Lowering Crash

**Status:** Diagnosed but not fixed. Blocks arc_insertion.cot and ssa_builder.cot.

When the selfcot compiles files containing struct methods with non-trivial bodies (var declarations, function calls, etc.), the Phase 3 lowering crashes. Free functions with identical code work fine.

**Evidence:** `struct Bar { x: i64, fn method() i64 { var a: i64 = 42; return a } }` — check passes, build crashes.

**Diagnosis:** The ARC diagnostics show thousands of "ARC: bad ptr" messages with sequential heap addresses (incrementing by 32 bytes). These are List element addresses being passed to retain/release — the struct field init ARC paths are retaining elements inside List backing buffers that don't have ARC headers.

**Fix:** The 6 struct field init sites (blocker 1 remaining items) need the `!= .optional` guard. This would eliminate the flood of bad retain/release calls on list elements.

---

## ARC Fixes Applied This Session

All ported from Swift SILGen reference (`references/swift/lib/SILGen/`, `references/swift/lib/SIL/IR/TypeLowering.cpp`):

| # | Fix | Reference | Commit |
|---|-----|-----------|--------|
| 1 | Retain-before-release for local assignment | TypeLowering.cpp:1213 | a58ba0b |
| 2 | Default `needs_retain=true` for complex expressions | ManagedValue.cpp:289 | a58ba0b |
| 3 | Struct field init retain (7 sites default fix) | ManagedValue.cpp:289 | 8ad8aaf |
| 4 | Wasm guard: `isWasmGC()` → `isWasm()` | — | 8ad8aaf |
| 5 | Field assignment ARC for local/global/nested | TypeLowering.cpp:1213 | a5233fc |
| 6 | Optional ?*T cleanup unwrap | TypeLowering.cpp:1603 | 54688b5 |
| 7 | Optional ?*T skip in assignment/return/init | — | 54688b5 |
| 8 | ARC pointer range check (< 4096) | EmbeddedRuntime.swift:431 | 25ece9c |
| 9 | ARC diagnostic messages | SWIFT_RT_TRACK_INVOCATION | 3b87ffc |
| 10 | Poison magic on dealloc | MallocScribble | 9a8106b |
| 11 | Double-free detection (IsDeiniting check) | RefCount.h:1040 | ff187c2 |

## Selfcot Fixes Applied This Session

| # | Fix | Reference | Commit |
|---|-----|-----------|--------|
| 1 | Parser: orelse return consuming next statement | Zig parser.zig:975 | d2e66e9 |
| 2 | Phase tracking: [cot] check/lower/codegen markers | Rust RUSTC_LOG | b954e2f |

---

## Crash Diagnostics Infrastructure (implemented this session)

| Feature | Status | Commit |
|---------|--------|--------|
| Signal handler calls backtrace | Done | 51546d1 |
| ARC pointer range check | Done | 25ece9c |
| ARC bad pointer messages | Done | 3b87ffc |
| Phase tracking in selfcot | Done | b954e2f |
| Double-free detection | Done | ff187c2 |
| Function name in @trap | Done | 9a8106b |
| Poison magic on dealloc | Done | 9a8106b |
| Debug build mode (`--debug`) | Planned (post-0.4) | claude/DEBUG_BUILD_MODE.md |

---

## Path to 0.4

### Phase 1: Complete Frontend Compilation (current)

- [x] 10/13 frontend files produce valid wasm
- [ ] Fix 6 remaining `?*T` struct field init sites in lower.zig
- [ ] Port self-referential struct support to selfcot checker (commit 137149a pattern)
- [ ] Get all 13 frontend files producing valid wasm
- [ ] Fix exit-cleanup crashes (cosmetic but noisy)

### Phase 2: Codegen + Main Compilation

- [ ] codegen/ files (16 files) compile via selfcot
- [ ] main.cot compiles via selfcot
- [ ] `selfcot build self/main.cot` produces working Wasm
- [ ] Validate: `wasmtime selfcot.wasm build self/test_tiny.cot` succeeds

### Phase 3: Release Polish (from RELEASE_PLAN.md)

- [ ] Homebrew tap + x86_64-macos binary
- [ ] VS Code marketplace extension
- [ ] Error messages polish pass
- [ ] Debug build mode (`cot build --debug`)

### Phase 4: Performance (post-release)

- [ ] Memory reduction (target: <200MB for self-compile)
- [ ] Inlining pass (target: 10x improvement)
- [ ] SSA optimization passes

---

## Key Files

| File | Purpose |
|------|---------|
| `self/main.cot` | Selfcot entry point + multi-file pipeline |
| `self/frontend/checker.cot` | Type checker (5,947 lines) — scope rewrite with `?*Scope` |
| `self/frontend/lower.cot` | IR lowering (9,177 lines, most complex) |
| `self/frontend/parser.cot` | Parser (3,235 lines) — orelse return fix applied |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (ARC fixes go here) |
| `compiler/codegen/native/arc_native.zig` | ARC runtime (range check, diagnostics, poison) |
| `compiler/codegen/native/signal_native.zig` | Signal handler + backtrace |
| `claude/DEBUG_BUILD_MODE.md` | Debug build mode plan (post-0.4) |
