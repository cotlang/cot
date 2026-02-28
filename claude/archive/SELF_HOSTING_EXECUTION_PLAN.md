# Self-Hosted Compiler — Execution Plan

**Created:** 2026-02-27
**Updated:** 2026-02-28
**Current state:** Frontend ~80% fidelity (13,381 lines), Backend 0%, Tests 189 native. See `SELF_HOSTING_AUDIT.md` for gap analysis.

---

## Current Assessment

### Completed Phases

| Phase | Description | Status | Commit |
|-------|-------------|--------|--------|
| 1.1 | Fix garbage exit codes | ✅ DONE | `c487fa7` (Feb 26) |
| 1.2 | Fix SIGSEGV on enum switch | ✅ DONE | `0c341a4` (Feb 27) |
| 2.1 | Fix `defer expr()` | ✅ DONE | `a5594fd` (Feb 27) |
| 2.2 | Fix `switch x` (no parens) | ✅ DONE | `a5594fd` (Feb 27) |
| 2.3 | Fix `export fn` | ✅ DONE | `a5594fd` (Feb 27) |
| 2.4 | Fix `Error!T` | ✅ DONE | `a5594fd` (Feb 27) |
| 2.5 | Fix `impl Trait for i64` | ✅ DONE | `a5594fd` (Feb 27) |

### Remaining Phases

| Phase | Description | Status | Blocker |
|-------|-------------|--------|---------|
| 1.3 | Fix WASI file reading | Not started | Low priority |
| 3.1 | Verify checker end-to-end | **NEXT** | selfcot check crashes (SIGBUS) |
| 3.2 | Self-check milestone | Blocked | Needs 3.1 |
| 4.1 | Get all tests passing on native | ✅ DONE | 189 pass |
| 4.2 | Add integration tests | Not started | — |
| 4.3 | Update documentation | ✅ DONE | — |

---

## Phase 3: Verify Checker End-to-End (CURRENT)

### 3.1 Fix selfcot check crash

**Symptom:** `selfcot check self/main.cot` exits with code 132 (signal 4 = SIGILL). Previous SIGBUS (65-symbol) fixed.

- `selfcot parse self/main.cot` → works, exit 0
- `selfcot check /tmp/trivial.cot` → works, exit 0
- `selfcot check` with `import "std/sys"`, `import "std/list"`, `import "std/map"` → all work
- `selfcot check` with `import "std/string"` → SIGILL crash
- The previous SIGBUS (65-symbol crash) is FIXED and no longer reproduces

**Bisect results (Feb 27):**
- Single-file check works fine (even with 30+ functions, 65+ symbols)
- The crash is triggered specifically by `import "std/string"`
- Other stdlib imports (`std/sys`, `std/list`, `std/map`) work

**Likely root cause:** A native codegen issue triggered by a specific code pattern in `stdlib/string.cot` — possibly a string method, generic string operation, or expression shape that produces an illegal instruction on ARM64. Since `std/list` (also generic) works fine, the issue is specific to something `string.cot` does differently.

### 3.2 Self-check: `selfcot check self/main.cot`

The ultimate frontend milestone: can the self-hosted binary type-check its own source code?

This requires:
- All 9 self/ files parse correctly ✅
- Multi-file import resolution works (needs 3.1 fix)
- All stdlib imports resolve (std/os, std/fs, std/string, etc.)
- All types, generics, traits check correctly

### 3.3 Fix any remaining checker issues found during 3.1/3.2

Based on previous testing, the checker logic appears correct (works on wasm). Issues found will likely be either:
- Native codegen bugs causing checker to malfunction (fix in Zig compiler)
- Parser gaps preventing stdlib files from parsing (may still exist)
- Genuine checker logic bugs (fix in `self/frontend/checker.cot`)

---

## Phase 4: Harden and Document

### 4.1 Get all tests passing on native — ✅ DONE

189 tests pass on native.

### 4.2 Add integration tests

Add tests to `self/main.cot` or a new test file that exercise the binary end-to-end:
- `test "parse valid file"` — parse a known-good file, verify no errors
- `test "parse invalid file"` — parse bad syntax, verify error reported
- `test "check valid file"` — type-check a simple program
- `test "check type error"` — verify type errors are caught

### 4.3 Update documentation — ✅ DONE

---

## Execution Order (Updated)

```
Phase 1.1: Fix exit codes          ✅ DONE (c487fa7)
Phase 1.2: Fix SIGSEGV             ✅ DONE (0c341a4)
Phase 2.1: Fix defer expr          ✅ DONE (a5594fd)
Phase 2.2: Fix switch no-parens    ✅ DONE (a5594fd)
Phase 2.3: Fix export fn           ✅ DONE (a5594fd)
Phase 2.4: Fix Error!T             ✅ DONE (a5594fd)
Phase 2.5: Fix impl for primitive  ✅ DONE (a5594fd)
Phase 4.1: All native tests pass   ✅ DONE (142/142)
Phase 4.3: Update docs             ✅ DONE
Phase 3.1: Fix selfcot check       ← NEXT (SIGBUS crash)
Phase 3.2: Self-check milestone    ← after 3.1
Phase 4.2: Integration tests       ← after 3.2
Phase 1.3: Fix WASI file reading   ← low priority
```

---

## Success Criteria

- [x] `selfcot parse file.cot` → exit 0 on success, exit 1 on error
- [ ] `selfcot check file.cot` → exit 0 on success, exit 1 on error ← BLOCKED (SIGBUS)
- [x] 189 tests pass on native (`cot test self/main.cot`)
- [ ] ~~All tests pass on wasm~~ — wasm32 broken (`error.MissingValue`, pre-existing multi-file issue)
- [x] All 9 self/ files parse via `selfcot parse`
- [ ] All 31 stdlib files parse via `selfcot parse`
- [ ] `selfcot check self/main.cot` → ok (self-check milestone) ← BLOCKED
- [x] `defer expr()`, `switch x {}`, `export fn`, `Error!T`, `impl for i64` all parse
