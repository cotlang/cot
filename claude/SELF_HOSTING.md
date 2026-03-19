# Cot Self-Hosting: Status, Blockers, and Path to 0.4

**Updated:** 2026-03-19
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: 0 Crashes, 2 Selfcot Checker Bugs Block All 13 Files

**Audited 2026-03-19** — verified with clean selfcot build + test of each file.

**Build:** `cot build self/main.cot -o /tmp/selfcot` → native binary (Success)

| File | Location | Lines | Status | Blocker |
|------|----------|------:|--------|---------|
| token.cot | parse/ | 448 | Error | Bug 1: enum method resolution |
| source.cot | parse/ | 315 | Error | Bug 2: stdlib/string.cot type mismatch |
| errors.cot | check/ | 545 | Error | Bug 2 |
| ast.cot | parse/ | 1,532 | Error | Bug 2 |
| arc.cot | build/ | 443 | Error | Bug 2 |
| scanner.cot | parse/ | 774 | Error | Bug 2 |
| types.cot | check/ | 1,572 | Error | Bug 2 |
| ir.cot | build/ | 1,467 | Error | Bug 2 |
| parser.cot | parse/ | 3,258 | Error | Bug 2 |
| ssa.cot | build/ | 625 | Error | Bug 2 |
| builder.cot | build/ | 2,364 | Error | Bug 2 |
| checker.cot | check/ | 5,909 | Error | Bug 2 |
| lower.cot | build/ | 9,201 | Error | Bug 2 |

**Key facts:**
- Zero SIGSEGV/SIGILL crashes (was 13/13 crashing before ARC opt_make fix)
- All 13 files reach the selfcot checker phase and fail on checker bugs
- `selfcot check self/main.cot` passes (38 files, multi-file type-checking)
- After frontend (13 files), codegen/ (17 files) and main.cot still need to compile
- ~44,700 lines across 42 files

---

## Bug 1: Enum Method Resolution (Blocks token.cot)

```
self/parse/token.cot:325:26: error[E306]: field not found
        @assertEq(Token.kw_fn.toString(), "fn")
```

Selfcot's checker doesn't resolve methods on enum values accessed via `EnumType.variant.method()`. The Zig compiler handles this correctly via `current_impl_type` tracking in the parser (parser.zig:520-526) which enables @safe self-injection for nested enum methods.

**Fix needed in:** `self/check/checker.cot` — enum method resolution for qualified enum value access.

## Bug 2: stdlib/string.cot Type Mismatch (Blocks 12/13 files)

```
stdlib/string.cot:79:31: error[E300]: type mismatch
        return indexOf(s, needle) >= 0
```

Selfcot's checker rejects `indexOf(...) >= 0` — likely a return type inference issue where `indexOf` returns `i64` but the comparison with `0` (untyped int) fails type checking. Every file that imports stdlib/string.cot (all 12 non-token files) hits this.

**Fix needed in:** `self/check/checker.cot` — binary comparison type resolution for `i64 >= untyped_int`.

## Bug 3: body_check_depth Counter Leak (Latent)

The `checkFnDeclBody` depth counter leaks when `lookupSymbol` fails via `orelse return`. This causes all subsequent function body checks to be skipped. Currently masked by Bug 2 (checker fails before reaching the affected code paths).

**Fix needed in:** `self/check/checker.cot` — decrement `body_check_depth` on all early return paths.

**Zig compiler dependency:** The natural fix (`orelse { decrement; return }`) triggers `error.NoCurrentBlock` in the Zig compiler's `lowerOrElseExpr`. Fix identified: replace `emitSelect` with branch+merge for pointer-like optional expr fallbacks with return.

## Bug 4: orelse Block Return Lowering (Zig Compiler)

The Zig compiler's `lowerOrElseExpr` uses `emitSelect` for pointer-like optional fallbacks. When the fallback contains `return`, the return terminates the current block, then `emitSelect` tries to emit in the terminated block → `NoCurrentBlock`.

**Fix identified and tested** — not yet committed. Needs selfcot regression testing.

---

## Resolved Issues (This Session)

| Issue | Root Cause | Fix |
|-------|-----------|-----|
| 13/13 SIGSEGV crashes | ?*T SSA: 16 bytes in struct, 8 bytes in SSA | `opt_make`/`opt_tag`/`opt_data` decomposition (Go pattern) |
| Map_rehash corruption | ?*T field loads returned tag (0/1) as pointer | `isPtrLikeOptional` returns false for managed pointers |
| Scope_lookup crash | ?*T parent pointer not retained in new_expr | Inline source-local extraction + retain in lowerNewExpr |
| Ad-hoc ARC dispatch | 31+ inline retain/release calls | All replaced with centralized emitCopyValue/emitDestroyValue |

---

## Path to 0.4

### Phase 1: Fix Selfcot Checker Bugs (current — 2 bugs)

- [ ] Bug 2: Fix `i64 >= untyped_int` comparison in selfcot checker
- [ ] Bug 1: Fix enum method resolution for `EnumType.variant.method()`
- [ ] Bug 3: Fix `body_check_depth` leak (requires Bug 4 first)
- [ ] Bug 4: Fix `orelse { block with return }` in Zig compiler

### Phase 2: Complete Frontend + Codegen Compilation

- [ ] All 13 frontend files produce valid Wasm via selfcot
- [ ] codegen/ files (17 files) compile via selfcot
- [ ] main.cot compiles via selfcot
- [ ] `selfcot build self/main.cot` produces working Wasm
- [ ] Validate: `wasmtime selfcot.wasm build self/test_tiny.cot` succeeds

### Phase 3: Release Polish (from RELEASE_PLAN.md)

- [ ] Homebrew tap + x86_64-macos binary
- [ ] VS Code marketplace extension
- [ ] Error messages polish pass

---

## Key Files

| File | Purpose |
|------|---------|
| `self/main.cot` | Selfcot entry point + multi-file pipeline |
| `self/check/checker.cot` | Type checker (~5,900 lines) — Bugs 1-3 live here |
| `self/build/lower.cot` | IR lowering (~9,200 lines, most complex) |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (ARC dispatch, opt_make integration) |
| `compiler/frontend/ssa_builder.zig` | SSA builder (opt_make/opt_tag/opt_data decomposition) |
| `compiler/codegen/native/dwarf.zig` | DWARF debug info (subprograms, variables, types, frame) |
