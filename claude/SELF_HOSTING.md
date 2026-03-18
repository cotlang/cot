# Cot Self-Hosting: Status, Blockers, and Path to 0.4

**Updated:** 2026-03-18
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: 5 of 13 Files Produce Valid Wasm

**Audited 2026-03-18** — verified with clean selfcot build + test of each file.

**Structure:** `self/` reorganized as `parse/ → check/ → build/ → optimize/ → emit/`

| File | Location | Lines | Build | Blocker |
|------|----------|------:|-------|---------|
| token.cot | parse/ | 448 | **OK** | — |
| source.cot | parse/ | 315 | **OK** | — |
| errors.cot | check/ | 545 | **OK** | — |
| ast.cot | parse/ | 1,532 | **OK** | — |
| arc.cot | build/ | 443 | **OK** | — |
| scanner.cot | parse/ | 774 | SIGSEGV | Crash during check — Map/Scope corruption |
| types.cot | check/ | 1,572 | SIGSEGV | Crash during lowerChecked |
| ir.cot | build/ | 1,467 | SIGILL | Crash during check phase |
| parser.cot | parse/ | 3,258 | SIGILL | Crash during check phase |
| ssa.cot | build/ | 625 | SIGSEGV | Crash during lowerChecked |
| builder.cot | build/ | 2,364 | SIGSEGV | Map.getOrNull crash in checker.Scope_lookup |
| checker.cot | check/ | 5,909 | SIGILL | Crash during check phase |
| lower.cot | build/ | 9,201 | SIGILL | Crash during check phase |

**Key facts:**
- 5 files produce valid wasm: token, source, errors, ast, arc
- 4 files crash with SIGILL during check — miscompiled code in selfcot binary
- 4 files crash with SIGSEGV during lowering or checking — Map/Scope corruption
- selfcot check passes for all 38+ files (`/tmp/selfcot check self/main.cot` exits 0)
- Previous "9 of 13" status was inaccurate — many files had regressed

After frontend, codegen/ (17 files) and main.cot still need to compile.

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

## Blocker 2: body_check_depth Counter Leak (DIAGNOSED)

**Status:** Root cause identified. Blocks all 13 files. Requires Zig compiler fix.

**Root cause:** `checkFnDeclBody` in `self/check/checker.cot:955` increments `body_check_depth` at line 956 but the `orelse return` at line 958 doesn't decrement it. After a single failed `lookupSymbol`, the counter permanently leaks, causing ALL subsequent function body checks to be skipped (depth > 1 → return at line 955). This means params (including `self`) are never defined in scope.

**Evidence:** `stdlib/map.cot:35: undefined identifier 'self'` — the `impl Hashable for i64 { fn hash(self: *i64) }` body check is skipped because `body_check_depth` leaked from a previous failed lookup.

**Fix required:** Decrement `body_check_depth` on ALL early return paths. The natural fix is:
```cot
const sym = self.lookupSymbol(lookup_name) orelse {
    self.body_check_depth = self.body_check_depth - 1
    return
}
```
But this triggers `error.NoCurrentBlock` in the Zig compiler — the Zig compiler doesn't handle `orelse { block with return }`. **This is a Zig compiler bug that must be fixed before the selfcot checker works.**

**Workaround attempted:** Split into two functions (`checkFnDeclBody` + `checkFnDeclBodyInner`). This compiled but caused other regressions due to the optional-to-value parameter passing in @safe mode.

**Impact:** ALL 13 files fail because `body_check_depth` leaks during stdlib checking (map.cot's trait impl methods trigger the first leak, then all subsequent function body checks in the target file are skipped).

## Blocker 3: Scope Chain Corruption During Multi-File Lowering

**Status:** Diagnosed via signal handler. Secondary to Blocker 2.

When selfcot lowers a file, `Scope_lookup` calls `Map.getOrNull` with garbage pointers (x0=`0x83cf...`, fault=`0x6eb6...`). The scope chain's parent pointers are corrupted — ASCII text like "Undefine" (0x656e696665646e55) appears as pointer values.

**Evidence:** Signal handler output shows `at self/main.cot:316` and register dump with `x0=0x83cf8e8f9081468b` (garbage). Backtrace: `Scope_lookup → Map.getOrNull` crash.

This is likely a SECONDARY effect of Blocker 2 — when function body checks are skipped, scope push/pop becomes unbalanced, corrupting the scope chain for subsequent operations.

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
| `self/check/checker.cot` | Type checker (~5,900 lines) — scope rewrite with `?*Scope` |
| `self/build/lower.cot` | IR lowering (~9,200 lines, most complex) |
| `self/parse/parser.cot` | Parser (~3,250 lines) — orelse return fix applied |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (ARC fixes go here) |
| `compiler/codegen/native/arc_native.zig` | ARC runtime (range check, diagnostics, poison) |
| `compiler/codegen/native/signal_native.zig` | Signal handler + backtrace |
| `claude/DEBUG_BUILD_MODE.md` | Debug build mode plan (post-0.4) |
