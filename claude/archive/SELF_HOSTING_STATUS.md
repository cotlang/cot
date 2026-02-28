# Self-Hosted Compiler Status

**Updated:** 2026-02-28
**Location:** `self/` directory (9 files + `cot.json`)
**Lines:** ~13,381 total (including test blocks)
**Tests:** 189 pass native
**Audit:** See `SELF_HOSTING_AUDIT.md` for detailed fidelity analysis (~80% frontend)

---

## Summary

The self-hosted compiler is a Cot compiler written in Cot. It can **parse** and **type-check** Cot source files, including multi-file projects with imports. It cannot yet generate code (no IR, SSA, or codegen stages).

The frontend is complete. The self-hosted binary can parse all 9 of its own source files (414KB total). Multi-file import resolution with circular dependency detection is working. Exit codes are correct. All native tests pass.

---

## What Works

| Capability | Status | Notes |
|-----------|--------|-------|
| **Lexer/Scanner** | Complete | Full tokenization including string interpolation |
| **Parser** | Complete | All 54 AST node types, recursive descent |
| **Type Checker** | Complete | Generics, traits, error unions, @safe mode |
| **Type Registry** | Complete | 23 pre-registered types, 15 type variants |
| **Error Reporting** | Complete | 28 error codes, source location with line:col |
| **CLI** | Complete | `parse`, `check`, `lex`, `version`, `help` commands |
| **Multi-file imports** | Complete | Recursive resolution, stdlib walk-up, circular detection |
| **@safe mode** | Complete | Project-level `cot.json` detection, auto-ref |
| **Build (Native)** | Working | `cot build self/main.cot -o /tmp/selfcot` → binary |
| **Tests (Native)** | 189 pass | All tests pass |
| **Build (Wasm)** | Working | `cot build self/main.cot --target=wasm32` → .wasm |
| **Tests (Wasm)** | Broken | `error.MissingValue` — pre-existing multi-file wasm issue |
| **selfcot parse** | Working | `selfcot parse self/main.cot` → exit 0 |
| **selfcot check** | Crashes | `selfcot check self/main.cot` → exit 132 (SIGILL on `import "std/string"`) |

---

## File Breakdown

| File | Lines | Tests | Purpose |
|------|-------|-------|---------|
| `cot.json` | — | — | Project config (`"safe": true`) |
| `main.cot` | 301 | 2 | CLI entry point, multi-file import resolution |
| `frontend/token.cot` | 440 | 13 | 84 token types, keyword lookup, precedence tables |
| `frontend/source.cot` | 317 | 7 | `Pos`, `Span`, `Source` structs for source locations |
| `frontend/errors.cot` | 536 | 10 | `ErrorReporter` with 28 error codes, line:col formatting |
| `frontend/scanner.cot` | 776 | 17 | Full lexer with string interpolation, number literals |
| `frontend/ast.cot` | 1,459 | 16 | Flat-encoded AST, 54 `NodeTag` variants, 54 builtins |
| `frontend/parser.cot` | 2,868 | 37 | Recursive descent parser, all declarations/expressions/statements |
| `frontend/types.cot` | 1,502 | 26 | `TypeRegistry`, `Type` union with 15 variants, generic instantiation |
| `frontend/checker.cot` | 5,182 | 22 | Full type checker: generics, traits, error unions, @safe, multi-file |
| **Total** | **~13,381** | **189** | All run on native |

---

## Architecture Comparison (Self-Hosted vs Zig Compiler)

### Stages Completed

```
                    Zig Compiler              Self-Hosted
                    ────────────              ───────────
Scanner             scanner.zig (527 L)       scanner.cot (736 L)     ✅
Parser              parser.zig (1,984 L)      parser.cot (2,805 L)    ✅
Types               types.zig (678 L)         types.cot (1,256 L)     ✅
Checker             checker.zig (4,009 L)     checker.cot (4,082 L)   ✅
IR                  ir.zig (606 L)            —                       ❌
Lowerer             lower.zig (8,188 L)       —                       ❌
SSA                 ssa/ (~4,000 L)           —                       ❌
Codegen (Wasm)      codegen/wasm/ (~7,000 L)  —                       ❌
```

Frontend: **~80% fidelity** (13,381 lines implemented of ~7,200 line Zig frontend — larger due to Cot's more verbose syntax and inline tests). See `SELF_HOSTING_AUDIT.md` for detailed gap analysis.

Backend: **0%** (IR + lowerer + SSA + codegen = ~19,800 Zig lines remaining)

### Lines Estimate for Full Self-Hosting

| Stage | Zig Lines | Estimated Cot Lines | Notes |
|-------|-----------|-------------------|-------|
| Frontend (done) | ~7,200 | 11,229 | 1.56x expansion (syntax + tests) |
| IR | 606 | ~1,000 | Enum-heavy, straightforward port |
| Lowerer | 8,188 | ~13,000 | Largest single file, most complex |
| SSA passes | ~4,000 | ~6,500 | Schedule, layout, decompose, etc. |
| Native codegen | ~1,400 | ~2,200 | SSA → CLIF IR (ssa_to_clif.zig) |
| Native runtime | ~2,500 | ~4,000 | ARC, I/O, print, test as CLIF IR |
| CLIF → MachInst | ~5,000 | ~8,000 | Lowering, regalloc, emit |
| **Total** | ~29,000 | ~46,000 | |

Remaining: ~34,800 lines of Cot to write (~76% of total estimated work)

---

## Import Dependency Graph

```
main.cot
├── std/os          (argsCount, arg, exit)
├── std/fs          (readFile, fileExists)
├── std/sys         (fd_write)
├── std/string      (contains, startsWith, substring, etc.)
├── std/path        (dirname, join, join3)
├── std/map         (Map)
├── std/debug       (assertions)
├── frontend/token
├── frontend/scanner  → token
├── frontend/ast      → token
├── frontend/errors   → (standalone)
├── frontend/parser   → scanner, ast, errors, token
├── frontend/types    → ast, token
└── frontend/checker  → ast, errors, parser, scanner, token, types
```

---

## CLI Commands

The self-hosted binary supports 5 commands:

```
cot parse <file>    # Parse and report syntax errors
cot check <file>    # Type-check (single-file or multi-file with imports)
cot lex <file>      # Tokenize and print each token
cot version         # "cot 0.3.2 (self-hosted)"
cot help            # Print usage
```

### Multi-File Check

`cot check` detects imports and recursively resolves them:
1. Parse the entry file
2. If imports exist → depth-first recursive resolution
3. Walk up directories to find `stdlib/` (npm-style)
4. Shared `TypeRegistry` and `SharedCheckerState` across all files
5. Circular import detection via `in_progress` map

### Self-Parse Test

The self-hosted binary can parse all its own source files:
```
cot parse self/main.cot           # ok (28 declarations)
cot parse self/frontend/parser.cot  # ok (125 declarations)
cot parse self/frontend/checker.cot # ok (101 declarations)
# ... all 9 files parse successfully
```

---

## Known Issues

### 1. selfcot check crashes — SIGILL on `import "std/string"` (Medium)

`selfcot check self/main.cot` exits with code 132 (signal 4 = SIGILL). The crash is triggered specifically by importing `std/string`. Other stdlib imports work fine (`std/sys`, `std/list`, `std/map`). Simple single-file checks and even 60+ symbol files work correctly.

**What works:** `selfcot check` with no imports, `import "std/sys"`, `import "std/list"`, `import "std/map"`, 30+ functions (65+ symbols).
**What crashes:** `import "std/string"` — SIGILL (illegal instruction).

**Previous SIGBUS (65-symbol crash) is FIXED** — no longer reproduces. The current crash is a different bug, likely a native codegen issue triggered by a specific code pattern in `stdlib/string.cot` (possibly string methods, generic string operations, or a particular expression shape that emits an illegal instruction).

### 2. ~~SIGSEGV on Enum Switch Returning String~~ — FIXED (Feb 27, 2026)

Was: `BasicKind.name()` — 18-arm switch returning string literals — crashed with SIGSEGV. Fixed in commit `0c341a4` ("Fix SIGSEGV: rewrite string/slice component extraction of const_nil").

### 3. ~~EntryLivein Regalloc Error~~ — FIXED (Feb 26, 2026)

Was: `cot build self/main.cot` crashed with live-in vregs in entry block. Fixed by replacing manual phi→block-param handling with defVar/useVar (Braun et al. 2013) in `ssa_to_clif.zig`. Commit `425141f`.

### 4. ~~Exit Codes Garbage~~ — FIXED (Feb 26, 2026)

Was: `selfcot parse file.cot` printed "ok" but returned non-zero exit codes (35, 41, 42, 48). Fixed in commit `c487fa7` ("Fix void main exit code: set x0/rax=0 in _main wrapper").

### 5. ~~Parser Gaps (5 items)~~ — ALL FIXED (Feb 27, 2026)

Was: `defer expr()`, `switch x {}`, `export fn`, `Error!T`, `impl Trait for i64` all failed to parse. All 5 fixed in commit `a5594fd`.

### 6. checker.cot Tests Require Multi-File

`cot test self/frontend/checker.cot` fails because checker tests reference Scanner, Parser, etc. from other files. Tests only pass when compiled through `main.cot` which handles imports. This is expected behavior, not a bug.

---

## What's Next

### Immediate: Fix selfcot check crash

The blocker for the self-check milestone (`selfcot check self/main.cot` → exit 0) is the SIGBUS crash. This is a native codegen issue — the same code works on wasm. Investigate and fix.

### Backend Port (in order)

1. **IR** — Port `compiler/frontend/ir.zig` (606 lines). Define the IR instruction enum and operand types in Cot.

2. **Lowerer** — Port `compiler/frontend/lower.zig` (8,188 lines). Largest and most complex stage — translates checked AST into IR instructions.

3. **SSA** — Port `compiler/ssa/` passes. Transform IR into SSA form with phi nodes, then run optimization and lowering passes.

4. **Native Codegen** — Port `compiler/codegen/native/ssa_to_clif.zig` and supporting modules. SSA → CLIF IR → machine code. This completes the native compilation path.

Once all 4 stages are done, the self-hosted compiler can compile itself — achieving bootstrap.

---

## Related Documents

| Document | Purpose |
|----------|---------|
| `claude/SELF_HOSTING_EXECUTION_PLAN.md` | Execution plan with phases and success criteria |
| `claude/SELF_HOSTING_FEATURES.md` | Feature gap analysis — what Zig features need Cot equivalents |
| `claude/VERSION_TRAJECTORY.md` | Self-hosting trajectory benchmarked against Zig's history |
| `self/cot.json` | Project config (`"safe": true` enables @safe mode) |
