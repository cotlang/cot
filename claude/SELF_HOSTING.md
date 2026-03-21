# Cot Self-Hosting: Status, Blockers, and Path to 0.4

**Updated:** 2026-03-21
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: All 41 Files Compile — selfcot2.wasm Validates and Runs

**Audited 2026-03-21** — verified with clean selfcot build + individual file compilation + whole-program build.

**Build chain:**
1. `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` → native binary (Success)
2. `/tmp/selfcot build self/main.cot -o /tmp/selfcot2.wasm` → 1.6MB valid Wasm (Success)
3. `wasm-tools validate /tmp/selfcot2.wasm` → 0 errors
4. `wasmtime --dir=. /tmp/selfcot2.wasm version` → `cot 0.3.7 (self-hosted)`

**All 41 files compile individually to valid Wasm via selfcot:**
- `self/parse/` — token, scanner, parser, ast, source (5 files)
- `self/check/` — checker, types, errors (3 files)
- `self/build/` — ir, lower, builder, arc, ssa (5 files)
- `self/optimize/` — copyelim, cse, deadcode, decompose, layout, rewrite, rewritedec, schedule (8 files)
- `self/emit/wasm/` — gen, assemble, link, preprocess, types, constants, builder, prog, mem, print, wasi, test, bench, slice, driver, passes, passes_dec, lower (18 files)
- `self/main.cot`, `self/test_tiny.cot` (2 files)

**~44,900 lines across 41 files.**

---

## Next Blocker: arg Parsing in selfcot2.wasm

selfcot2.wasm runs and handles 2-arg invocations correctly (`version`, `help`), but with 3+ args (e.g., `build file.cot -o out.wasm`), `arg(1)` returns wrong data (empty string or file contents instead of the command name).

**Symptoms:**
- `wasmtime --dir=. /tmp/selfcot2.wasm version` → works (2 args: program + "version")
- `wasmtime --dir=. /tmp/selfcot2.wasm build self/main.cot -o /tmp/out.wasm` → prints help text (arg(1) not recognized as "build")

**Root cause hypothesis:** Codegen bug with multi-value returns (string = ptr+len) in large Wasm binaries (~3,500 functions). Simple selfcot-compiled programs handle args correctly. Issue is specific to the large selfcot2.wasm binary.

**This is the first known blocker, not the last.** Fixing arg parsing will allow selfcot2.wasm to begin compiling files, which will exercise the entire pipeline (scanner, parser, checker, lowerer, SSA, codegen, emit) for the first time as Wasm-compiled code. Expect additional codegen bugs to surface — every code path beyond `version`/`help` is untested at runtime in selfcot2.wasm. The `readFile` hang (see background task output) is likely a preview of more issues to come.

---

## Recent Changes (2026-03-20 → 2026-03-21)

**ARC retain/release no-op on Wasm** — fixed validation error in `Map_remove`:
- selfcot was emitting `retain`/`release` calls on Wasm, but Wasm uses bump allocator (no ARC)
- `@arcRetain` now returns arg directly, `@arcRelease` returns null_node (matches Zig lines 10635/10645)
- This was the last Wasm validation error — selfcot2.wasm now validates clean

**String interpolation parsing** — fixed to match Zig compiler (parser strips delimiters)

**Block ordering** — fixed file reading hang in selfcot2.wasm

---

## Resolved Bugs

### Bug 7: ARC retain/release emitted on Wasm — FIXED (2026-03-21)
**Root cause:** selfcot's lowerer emitted `retain`/`release` calls on Wasm target, but Zig compiler skips them (Wasm = bump allocator). In `Map_remove`, `@arcRelease(string)` decomposed into (ptr, len) but `release` takes 1 i64 → extra value on stack → validation error.

### Bug 5: `dealloc()` on `alloc_raw()` memory — FIXED (2026-03-19)
**Root cause:** `editDistance()` allocated with `alloc_raw()` (no ARC header) but freed with `dealloc()` (expects 32-byte ARC header at ptr-32). Now **impossible to reintroduce** — `alloc_raw` returns `RawPtr` (distinct type), `dealloc` takes `i64`. Mismatch is a compile error.

### Bug 6: Map states read as `*u8` instead of `*i64` — FIXED (2026-03-19)
**Root cause:** `findSimilarType()` iterated Map internal arrays reading states as single bytes instead of 8-byte i64 values. Fix: `@intToPtr(*u8, states + ki)` → `@intToPtr(*i64, states + ki * @sizeOf(i64))`.

### Bug 2: Multi-Param Function Call Type Mismatch — FIXED
**Root cause:** `alloc(0, ...)` used for raw buffers. Fix: `alloc(0, ...) → alloc_raw(...)`.

### ARC ?*T SSA Mismatch — FIXED
**Root cause:** `?*T` was 16 bytes in struct fields but 8 bytes in SSA. Fix: `opt_make`/`opt_tag`/`opt_data` SSA decomposition.

### Ad-Hoc ARC Dispatch — FIXED
All inline retain/release replaced with centralized `emitCopyValue`/`emitDestroyValue`.

### Blockers A, B, C from 2026-03-20 — ALL RESOLVED
- **Blocker A** (SIGSEGV in lowerGenericFnInstance for ir, ssa, builder) — resolved
- **Blocker B** (Scope import resolution for checker, lower) — resolved
- **Blocker C** (Enum method resolution in token.cot tests) — only affects test blocks, not builds

---

## Path to 0.4

### Phase 1: All Files Compile to Valid Wasm — COMPLETE
- [x] All 41 self/ files compile individually to valid Wasm
- [x] Whole-program build produces valid selfcot2.wasm (0 validation errors)
- [x] selfcot2.wasm runs and responds to `version`/`help`

### Phase 2: selfcot2.wasm Can Compile Files (current)
- [x] `selfcot build self/main.cot -o /tmp/selfcot2.wasm` succeeds
- [x] `wasm-tools validate /tmp/selfcot2.wasm` → 0 errors
- [ ] Fix arg parsing bug (3+ args) in selfcot2.wasm
- [ ] Debug and fix runtime codegen bugs as they surface (expect many — every pipeline stage is untested as Wasm-compiled code beyond trivial paths like `version`/`help`)
- [ ] `wasmtime selfcot2.wasm build self/test_tiny.cot -o /tmp/out.wasm` succeeds
- [ ] `wasmtime selfcot2.wasm build self/main.cot -o /tmp/selfcot3.wasm` succeeds (full bootstrap)

**Note:** Phase 2 will likely be the longest phase. Structural validity (Phase 1) only means the Wasm binary is well-formed. Runtime correctness of a 44,900-line compiler executing as Wasm is a different bar entirely. Known early signal: `readFile` hangs in selfcot-compiled Wasm even for simple programs.

### Phase 3: Release Polish
- [ ] Homebrew tap + x86_64-macos binary
- [ ] VS Code marketplace extension
- [ ] Error messages polish pass

---

## Key Files

| File | Purpose |
|------|---------|
| `self/main.cot` | Selfcot entry point + multi-file pipeline |
| `self/check/checker.cot` | Type checker (~5,980 lines) |
| `self/build/lower.cot` | IR lowering (~9,200 lines, most complex) |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (ARC dispatch, opt_make integration) |
| `compiler/frontend/ssa_builder.zig` | SSA builder (opt_make/opt_tag/opt_data decomposition) |
| `compiler/codegen/native/dwarf.zig` | DWARF writer (subprograms, variables, types, frame) |
| `compiler/codegen/native/dwarf_reader.zig` | DWARF runtime reader (crash file:line:col) |
| `compiler/codegen/native/signal_native.zig` | Signal handler + pctab decoder + per-frame resolution |
