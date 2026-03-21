# Claude AI Instructions

## 🚨🚨🚨 CURRENT 0.4 FOCUS: FIX ALL SELFCOT WASM CODEGEN TO MATCH ZIG 🚨🚨🚨

**Goal:** selfcot must produce identical Wasm output to the Zig compiler. Run `selfcot test test/cases/*.cot` — every test that passes with `./zig-out/bin/cot test --target=wasm` must also pass with selfcot. Currently 6 of 21 test files have runtime failures (arrays, chars, loops, optional, strings, structs). Fix each by porting the exact Zig code to `self/`. Once all tests pass, selfcot2.wasm must compile selfcot3.wasm identically — that's full self-hosting and the 0.4 release gate.

**Method:** For every failing test, diff the Zig compiler code against selfcot. The divergence IS the bug. Port the Zig code exactly — no simplification, no invention. This has resolved every issue so far without exception.

**This is a long process and the user knows it.** Do not cut corners. Do not propose "quick fixes." Do not simplify or workaround. Every function in `self/` is production code that will permanently replace the Zig compiler. Port each function completely and correctly, matching the Zig implementation line-by-line. If a fix requires rewriting a large function, rewrite the entire function.

**Timeouts:** Nothing in this project takes more than a few seconds. If a command doesn't return in 3-5 seconds, it's an infinite loop — kill it immediately. Use `perl -e 'alarm 3; exec @ARGV'` or `timeout 3` wrappers. Never set timeouts above 15s for test runs.

---

## 🚨🚨🚨 `self/` MUST MATCH ZIG COMPILER 1:1 🚨🚨🚨

**The self-hosted compiler (`self/`) must be a mechanical 1:1 translation of the Zig compiler (`compiler/`).** Every function, every variable, every enum, every logic pattern must be identical. Any divergence is a bug.

- **If `self/` diverges from `compiler/`:** Fix `self/` to match the Zig compiler. No exceptions.
- **If `self/` has a genuinely better approach:** Port the improvement to the Zig compiler FIRST, verify all 360 tests pass, THEN update `self/` to match. Never let `self/` lead — the Zig compiler is always the source of truth.
- **When investigating bugs in `self/`:** Compare the generated output (SSA, Wasm) against the Zig compiler's output for the same input. The difference IS the bug.
- **When adding features:** Implement in the Zig compiler first, test, then port to `self/`.

---

## 🚨🚨🚨 `self/` IS `@safe` MODE — ALL STRUCT PARAMS ARE INFERRED POINTERS 🚨🚨🚨

**ALL files in `self/` use `@safe` mode (set in `self/cot.json`).** In `@safe` mode:
- **Struct params are automatically pointers.** `fn foo(list: List(int))` = `fn foo(list: *List(int))`. NEVER add `*` or `&`.
- **`self` is auto-injected** in struct/enum/impl methods. You do NOT write `self: *Type` — the compiler injects it. Just write `fn method() i64 { return self.x }`.
- **`+` on strings** auto-desugars to `++` (concatenation).

This is by design so Cot looks like TypeScript to users. **NEVER change `Type` to `*Type` in `self/` function signatures. NEVER add explicit `self` parameters. NEVER add `&` at call sites.** The compiler handles all of it.

---

## 🚨🚨🚨 GIT: NO DESTRUCTIVE OPERATIONS WITHOUT ASKING 🚨🚨🚨

**Standard git workflow is fine:** `git add`, `git commit`, `git push`, `git pull`, `git rebase`, `git status`, `git log`, `git diff`, `git fetch` are all allowed.

**NEVER use destructive operations without asking the user first:**
- **NEVER** `git checkout -- <file>` or `git restore` — destroys uncommitted work
- **NEVER** `git reset --hard` — destroys history and uncommitted work
- **NEVER** `git push --force` — destroys remote history
- **NEVER** `git stash drop` or `git stash clear` — destroys stashed work
- **NEVER** `git clean -f` — deletes untracked files permanently
- **To undo changes:** Edit the file manually with the Edit tool, or ask the user
- **To compare with old code:** Use `git diff` or `git show` to READ, never to modify
- **To compare with clean state:** `git stash && <test> && git stash pop`
- **To abandon changes:** ASK THE USER FIRST — never decide unilaterally
- Uncommitted changes often contain hours of debugging, partial fixes, and diagnostic code

---

## 🚨 CRITICAL: `self/` uses `@safe` mode via `self/cot.json` (`"safe": true`)

**ALL files in `self/` are compiled in `@safe` mode.** This is set at the project level in `self/cot.json`, NOT per-file. You do NOT need `@safe` at the top of each file — it applies automatically. This means:
- `self` is auto-injected in struct/enum methods (no explicit `self: *Type` parameter)
- `+` on strings auto-desugars to `++`
- All `@safe` mode behaviors apply

**DO NOT add `@safe` to individual files in `self/`.** It's already set project-wide.

---

## 🚨🚨🚨 ABSOLUTE #1 RULE — NEVER WORKAROUND, NEVER SIMPLIFY 🚨🚨🚨

**NEVER implement workarounds for Cot compiler bugs or missing features.** If the Cot compiler doesn't support a pattern you need, or if compiled code behaves incorrectly at runtime, **STOP and report the bug to the user** so they can fix the Zig compiler. The self-hosted compiler must use idiomatic Cot — every workaround is tech debt that defeats the purpose of dogfooding.

This applies to ALL compiler issues, including:
- **Missing language features** (syntax, type system, generics)
- **Codegen bugs** (wrong code generated for correct source — e.g. union tags corrupted after memcpy, struct layout wrong, ARC miscompilation)
- **Runtime crashes** (stack overflow from over-allocation of locals, SIGSEGV in compiled code)

- **NO** restructuring code to avoid a language limitation
- **NO** extracting logic to helper functions just because a construct doesn't compile
- **NO** falling back to if-else chains because switch doesn't support something
- **NO** simplifying data structures because generics don't work for a case
- **NO** moving data to different structs to avoid a codegen bug (e.g. moving union data out of a struct because the union tag gets corrupted)
- **NO** splitting functions just because the native codegen overflows the stack
- **ALWAYS** identify the exact compiler limitation and report it to the user
- The user will fix the Zig compiler. That is the correct workflow.

**The whole point of self-hosting is to find and fix compiler bugs.** When you encounter a bug, that IS the work — diagnose it precisely and report it so the Zig compiler can be fixed.

## 🚨 CRITICAL RULES

### 1. Never Invent — Always Copy Reference Implementations

**The #1 cause of project failure**: Claude tries to "figure out" fixes instead of copying the reference. This has caused 5 rewrites.

**The fix that works every time:**
1. Find the reference implementation (Go or Cranelift)
2. Do line-by-line comparison
3. Copy exactly — don't invent, don't simplify, don't "improve"

See `TROUBLESHOOTING.md` for full methodology.

### 2. Check Wasm 3.0 Before Adding Features

**Read `claude/specs/WASM_3_0_REFERENCE.md`** before implementing anything touching Wasm codegen. Cot currently emits Wasm 1.0 but Wasm 3.0 (released Sep 2025) may offer better solutions:
- Recursive functions → `return_call` (0x12) instead of `call` + `return`
- Closures/function pointers → `call_ref` (0x14) instead of `call_indirect`
- Error propagation → `try_table`/`throw` instead of manual checks

### 3. br_table is Intentional

Read `claude/BR_TABLE_ARCHITECTURE.md` if confused. br_table is copied from Go's dispatch loop pattern. Do NOT try to remove it.

---

## Project Overview

**Cot** is a compiled language for full-stack web development with native and Wasm targets.
**Pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.
**Compiler:** Currently written in Zig, being ported to Cot (full self-hosting). Zig becomes bootstrap-only.

### Self-Hosted Compiler (selfcot)

The self-hosted compiler lives in `self/` and is written in Cot. It compiles `.cot` files to `.wasm` binaries.

**How to build selfcot:**
```bash
# 1. Build the Zig compiler first (if not already done)
zig build

# 2. Build selfcot as a native binary using the dev compiler (full path!)
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot
```

**How to test selfcot:**
```bash
# Use selfcot to compile a file and measure memory
/usr/bin/time -l /tmp/selfcot build self/parse/scanner.cot -o /tmp/out.wasm

# Compare with dev compiler for the same file (baseline)
/usr/bin/time -l ./zig-out/bin/cot build self/parse/scanner.cot --target=wasm -o /tmp/out_zig.wasm
```

**Key facts:**
- `self/cot.json` sets `"safe": true` project-wide — all `self/` files use `@safe` mode
- selfcot only emits Wasm (no native target yet) — entry point is `self/main.cot`
- selfcot is compiled to a native binary by the Zig `cot` compiler
- The pipeline: `self/main.cot` → `self/parse/` → `self/check/` → `self/build/` → `self/optimize/` → `self/emit/wasm/`
- `self/test_tiny.cot` is a minimal test file for smoke-testing selfcot
- ~44,900 lines across 41 files — parse, check, build, optimize, emit all complete

**Current status (as of 2026-03-21):** selfcot compiles **all 41 files** individually to valid Wasm and builds itself to a valid 1.6MB Wasm binary (`selfcot2.wasm`). selfcot2.wasm validates clean and runs (`version`/`help` work). Next blocker: arg parsing bug with 3+ CLI args prevents selfcot2.wasm from compiling files. Expect more runtime codegen bugs once arg parsing is fixed — every pipeline stage is untested as Wasm-compiled code. See `claude/SELF_HOSTING.md` for detailed status.

**`distinct` types:** Both the Zig compiler and selfcot support `type X = distinct T`. Used in stdlib: `alloc_raw` returns `RawPtr` (distinct i64), `dealloc_raw` takes `RawPtr` — mismatching alloc/dealloc is now a compile-time error.

**Stdlib** is a separate repo (`cotlang/std`) included as a git submodule at `stdlib/`. After cloning: `git submodule update --init stdlib`. When modifying stdlib files, changes must be committed in the submodule first (`cd stdlib && git add . && git commit && git push`), then the updated submodule ref committed in the parent repo.

**🚨 TWO `cot` BINARIES — KNOW WHICH ONE YOU'RE USING:**
- **`cot`** on PATH → `/opt/homebrew/bin/cot` → **Homebrew release binary** (stable, for testing). Do NOT use this for development.
- **`./zig-out/bin/cot`** → **Local dev build** from `zig build`. Always use the full path `./zig-out/bin/cot` when developing the compiler.
- After `zig build`, the dev binary is at `./zig-out/bin/cot` — it is NOT on PATH. This is intentional.
- **NEVER** assume `cot` refers to the dev build. Always use the full path for compiler development.

**🚨 SUBMODULE COMMIT RULE:** When committing changes in the parent repo, **NEVER include `stdlib` in `git add`** unless you are intentionally updating the submodule reference. The `stdlib` directory will frequently show as "modified" in `git status`/`git diff` because the local checkout may be ahead of the tracked ref. **Always stage files by explicit name** (e.g., `git add compiler/foo.zig compiler/bar.zig`). **NEVER use `git add .` or `git add -A`**. If you accidentally commit a stdlib ref change pointing to a commit that doesn't exist on the remote, CI will break for ALL jobs because `actions/checkout` can't fetch the submodule.

---

## CLI

```
cot <file.cot>                  # Implicit build (shorthand for cot build)
cot build <file.cot> [-o name]  # Compile to executable (or .wasm)
cot build <file.cot> --debug    # Compile with full DWARF debug info (also: -g)
cot run <file.cot> [-- args]    # Compile, run in /tmp, clean up, forward exit code
cot test <file.cot>             # Compile + run in test mode
cot bench <file.cot>            # Run benchmarks
cot check <file.cot>            # Type-check without compiling
cot lint <file.cot>             # Check for warnings
cot fmt <file.cot>              # Format source code in-place (--check for CI, --stdout for pipe)
cot init [name]                 # Create new project (cot.json, src/main.cot, .gitignore)
cot lsp                         # Start language server (LSP over stdio)
cot version                     # Print version: cot 0.3.4 (arm64-macos)
cot help [command]              # Print help (per-subcommand help available)
```

**Key files:** `compiler/cli.zig` (arg parsing, help text), `compiler/main.zig` (command dispatch + compileAndLink), `compiler/project.zig` (cot.json manifest loading).

**cot.json integration:** All file-requiring commands (`build`, `run`, `test`, `bench`, `check`, `lint`, `fmt`) read `cot.json` from CWD when no input file is given. If `main` field is set, it's used as the input file. `cot init myapp && cd myapp && cot run` just works.

**Native library linking:** `"libs": ["sqlite3"]` in `cot.json` appends `-lsqlite3` to the linker invocation. Parsed by `project.zig:getLibs()`, wired in `main.zig:compileAndLinkFull()`. User-declared `extern fn` names are collected from checker scopes by `driver.zig:collectExternFns()` and registered in `func_index_map` (Cranelift `Linkage::Import` pattern).

**Output naming:** Strip path, strip `.cot`, append `.wasm` for wasm targets. `app.cot` → `./app` (native) or `./app.wasm` (wasm). Override with `-o`.

---

## Versioning

**Single source of truth:** `VERSION` file at repo root (plain text, e.g. `0.3.4`).

**Flow:** `VERSION` → `build.zig` reads via `@embedFile` → injected as `build_options` → `compiler/cli.zig` imports `@import("build_options").version`.

**To bump the version:** Edit `VERSION`, rebuild. That's it.

**Design (audited from Go/Zig/Rust/Deno):**
- Rust pattern: plain text VERSION file (simplest, CI-friendly)
- Zig pattern: `@import("build_options")` comptime injection
- SemVer `0.X.Y` (standard for pre-1.0)
- Help banner shows major.minor only (`Cot 0.3`), `cot version` shows full (`cot 0.3.4 (arm64-macos)`)

---

## Architecture

Native and Wasm targets use separate backend paths from SSA onwards.

```
Cot Source → Scanner → Parser → Checker → IR → SSA
  ├── --target=wasm → lower_wasm.zig → wasm/ → .wasm file
  └── --target=native (default) → ssa_to_clif.zig → ir/clif/ → machinst/ → isa/ → emit → .o → linker → executable
```

The native path translates SSA directly to CLIF IR (Cranelift's intermediate representation), then lowers through MachInst → register allocation → ARM64/x64 emission → object file → system linker. Runtime functions (ARC, I/O, print, test runner) are generated as CLIF IR by dedicated modules (`arc_native.zig`, `io_native.zig`, `print_native.zig`, `test_native.zig`) rather than going through Wasm.

**Key directories:**
| Path | Purpose | Reference |
|------|---------|-----------|
| `compiler/cli.zig` | CLI arg parsing, help text, version | — |
| `compiler/main.zig` | Command dispatch, compileAndLink | — |
| `compiler/frontend/` | Scanner, parser, checker, lowerer | — |
| `compiler/ssa/passes/` | rewritegeneric, decompose, rewritedec, schedule, layout, lower_wasm | Go `ssa/*.go` |
| `compiler/codegen/wasm/` | Wasm bytecode generation + linking (Wasm target only) | Go `wasm/ssa.go`, `wasmobj.go` |
| `compiler/codegen/native/ssa_to_clif.zig` | SSA → CLIF IR translation (native target) | `rustc_codegen_cranelift` `src/base.rs`, `pointer.rs` |
| `compiler/codegen/native/arc_native.zig` | ARC runtime as CLIF IR (alloc, retain, release, realloc) | Swift `HeapObject.cpp` |
| `compiler/codegen/native/io_native.zig` | I/O runtime as CLIF IR (fd_open, fd_write, fd_read, etc.) | libc wrappers |
| `compiler/codegen/native/print_native.zig` | Print runtime as CLIF IR (print_int, print_string, etc.) | — |
| `compiler/codegen/native/test_native.zig` | Test runner runtime as CLIF IR | — |
| `compiler/codegen/native/machinst/` | CLIF → MachInst lowering | Cranelift `machinst/` |
| `compiler/codegen/native/isa/aarch64/` | ARM64 backend | Cranelift `isa/aarch64/` |
| `compiler/codegen/native/isa/x64/` | x64 backend | Cranelift `isa/x64/` |
| `compiler/codegen/native/regalloc/` | Register allocator (regalloc2 port) | `references/regalloc2/src/` |
| `compiler/driver.zig` | Pipeline orchestrator | — |
| `compiler/lsp/` | Language server (LSP over stdio) | ZLS (Zig Language Server) |
| `editors/vscode/` | VS Code/Cursor extension (syntax + LSP client) | — |

**Reference implementations (copy, don't invent):**
| Component | Reference Location |
|-----------|-------------------|
| Cot → Wasm | `references/go/src/cmd/compile/internal/wasm/` |
| SSA → CLIF (native) | `references/rust/compiler/rustc_codegen_cranelift/src/` |
| CLIF → ARM64 | `references/wasmtime/cranelift/codegen/src/isa/aarch64/` |
| WasmGC patterns | `references/kotlin/wasm/` (Kotlin/Wasm compiler) |
| Language semantics | Zig (error unions, defer, comptime) |

---

## Builtin Pipeline

Two categories:
| Category | Examples | Implementation |
|----------|----------|----------------|
| **Compiler intrinsics (59)** | `@intCast`, `@sizeOf`, `@intToPtr`, `@ptrOf`, `@lenOf`, `@string`, `@assert`, `@panic` | Inline Wasm ops in `lower.zig` |
| **Runtime functions (~50)** | `alloc`, `dealloc`, `fd_write`, `exit`, `time`, `net_socket`, etc. | Wasm module functions in `arc.zig`/`wasi_runtime.zig` → `func_indices` in `driver.zig` |

**Runtime functions are NOT builtins.** They are regular functions exposed via `extern fn` declarations in `stdlib/sys.cot`. User code imports `std/sys` to access them. The compiler links them by name through `func_indices`.

**Wasm target:** Runtime functions are Wasm MODULE functions, NOT host imports. The compiler has ZERO host imports. If a function name is missing from `func_indices`, `wasm_gen.zig` silently calls function index 0 (alloc) — a silent bug.

**Native target:** Runtime functions are generated as CLIF IR by dedicated modules (`arc_native.zig`, `io_native.zig`, `print_native.zig`, `test_native.zig`). They call libc functions (e.g., `write`, `read`, `__open`, `malloc`) via undefined symbols resolved at link time.

**To add a new runtime function:**
- Wasm: `arc.zig` or `wasi_runtime.zig` (body + addToLinker) → `driver.zig` (`func_indices`)
- Native: `*_native.zig` module (CLIF IR generation) → `driver.zig` (`runtime_func_names` + `func_index_map`)
- Both: `stdlib/sys.cot` (extern fn declaration)

---

## Testing

**Workflow: `zig build test` once, then `cot test` for everything else.**

`zig build test` validates the Zig compiler internals. Run it once after changes to confirm the compiler builds correctly. After that, **use `cot test` as the primary verification tool** — it exercises the full pipeline (parse → check → SSA → native/Wasm → execute) and catches real-world regressions that unit tests miss.

```bash
zig build test                                                        # Compiler internals (run once)
./zig-out/bin/cot test test/e2e/features.cot                          # Primary: 350 feature tests, native
./zig-out/bin/cot test test/e2e/features.cot --target=wasm          # Primary: same tests, wasm via wasmtime
./zig-out/bin/cot test test/cases/<category>.cot                      # Targeted: specific category
./test/run_all.sh                                                     # Full suite (~1,670 tests across 70 files)
```

**`cot test --target=wasm`** runs Wasm binaries via `wasmtime` (must be installed). Use this to verify Wasm codegen — bugs often manifest on one target but not the other. Targets: `--target=wasm` (WASI/wasmtime), `--target=js` (browser). Aliases `wasm32`, `wasm32-wasi` still accepted.

**Troubleshooting tip:** When a feature works on native but fails on wasm (or vice versa), test both targets to isolate whether the bug is in the shared frontend or in a target-specific backend.

**Adding Cot tests:** Add `test "name" { }` blocks to `.cot` files. Run `cot test <file>`.
**Adding compiler tests:** Add `test "..." { }` blocks in Zig source with inline Cot snippets.
**Never embed `.cot` test files in Zig code.** Use `cot test` for that.

**Test directories:**
- `test/cases/` — Category unit tests (22 files, ~122 tests)
- `test/e2e/` — Comprehensive feature tests (48 files, ~1,536 tests)
- All tests use inline `test "name" { @assertEq(...) }` format
- See `claude/TESTING.md` for full details

**Every new feature must:**
1. Work on Wasm (`--target=wasm`)
2. Work on native (default target)
3. Have E2E test cases (both Wasm and native)
4. Copy the reference implementation

---

## Debugging

Use `compiler/pipeline_debug.zig`, NOT `std.debug.print`:
```zig
const debug = @import("pipeline_debug.zig");
debug.log(.codegen, "emitting {s}", .{op_name});
```

---

## Zig 0.15 Note

```zig
// Use ArrayListUnmanaged (allocator per operation)
var list: std.ArrayListUnmanaged(u8) = .{};
defer list.deinit(allocator);
try list.append(allocator, 42);
```

---

## Behavioral Guidelines

**DO:**
- Run `zig build test` once after compiler changes, then use `./zig-out/bin/cot test` for ongoing verification
- Use `./zig-out/bin/cot test test/e2e/features.cot` (native) and `./zig-out/bin/cot test test/e2e/features.cot --target=wasm` (wasm) as the primary check — these catch real regressions faster than unit tests
- After changing `compiler/lsp/`: run `zig build` to update the LSP binary
- After changing `editors/vscode/`: rebuild + reinstall extension (see Editor Extensions & LSP section)
- After changing either: do BOTH — `zig build` AND reinstall extension
- Check `claude/specs/WASM_3_0_REFERENCE.md` when touching Wasm codegen
- Check `claude/PIPELINE_ARCHITECTURE.md` for full pipeline reference map
- Make incremental changes, verify each one

**DO NOT:**
- **Run tests redundantly.** `./test/run_all.sh` takes ~60s. Run it ONCE per task, not multiple times. Do NOT run it just to count tests — use `grep -r 'test "' test/ | wc -l` instead. Do NOT run it multiple times with slightly different grep patterns to extract info. The user runs thousands of agent sessions per month; wasting minutes per session adds up to hours per day. Same applies to `zig build test` — run ONCE after compiler changes, not repeatedly.
- Skip testing
- Invent approaches — copy reference implementations
- Comment out failing tests, leave TODOs, or create "known issues"
- Give up on difficult code — study the reference until you understand it

**When stuck:** Read reference implementation → copy pattern → iterate until tests pass.

---

## Editor Extensions & LSP

**The LSP binary IS the `cot` binary** (`cot lsp`). Any change to `compiler/lsp/` requires rebuilding the compiler.

**After ANY change to `compiler/lsp/` or `editors/vscode/`**, run the full rebuild+reinstall:

```bash
# 1. Rebuild the cot binary (includes LSP server)
zig build

# 2. Rebuild and reinstall the VS Code/Cursor extension
cd editors/vscode && npm install && npm run compile && npx @vscode/vsce package --allow-missing-repository
cursor --uninstall-extension cot-lang.cot-lang 2>/dev/null; cursor --install-extension cot-lang-0.1.0.vsix --force
```

**Triggers for rebuild:**
- `compiler/lsp/*.zig` — LSP server code (diagnostics, hover, goto-def, semantic tokens, etc.) → **must `zig build`**
- `editors/vscode/syntaxes/cot.tmLanguage.json` — TextMate grammar → **must reinstall extension**
- `editors/vscode/src/extension.ts` — LSP client → **must reinstall extension**
- `editors/vscode/package.json` — extension manifest → **must reinstall extension**
- `editors/vscode/language-configuration.json` — brackets, comments → **must reinstall extension**

**If LSP behavior seems stale:** The most common cause is forgetting to run `zig build` after changing `compiler/lsp/`. The extension just spawns whatever `cot` binary is on PATH — if you didn't rebuild, Cursor is running the old LSP.

---

## Documents

| Document | Purpose |
|----------|---------|
| `VERSION` | **Version single source of truth** (edit to bump) |
| `TROUBLESHOOTING.md` | **Debugging methodology — read before any debugging** |
| `claude/SELF_HOSTING.md` | **Self-hosting status, blocker, and path to 0.4** |
| `claude/PIPELINE_ARCHITECTURE.md` | **Full pipeline map, reference for every stage** |
| `claude/BR_TABLE_ARCHITECTURE.md` | Why br_table appears in generated code |
| `claude/specs/WASM_3_0_REFERENCE.md` | Wasm 3.0 opcodes and adoption plan |
| `claude/ROADMAP.md` | Forward-looking roadmap: 0.4→1.0, competitive positioning |
| `claude/VERSION_TRAJECTORY.md` | **Self-hosting trajectory** — benchmarked against Zig, inspirational |
| `docs/syntax.md` | Complete language syntax reference with examples |
| `VISION.md` | Language vision, design principles, execution roadmap |
| `claude/BUSINESS_MODEL.md` | Licensing, trademark, revenue model, funding strategy |
| `claude/RELEASE_PLAN.md` | 0.4 release plan: branding, distribution, polish, criteria |
| `claude/TESTING.md` | Test system: 84 files, ~1,790 tests, error-union isolation |
| `claude/SELFCOT_CRASH_INVESTIGATION.md` | Selfcot crash root cause analysis (alloc/dealloc + Map state bugs) |
| `claude/LINUX_X64_PARITY.md` | Linux x86_64 backend investigation notes |
| `claude/archive/` | Historical: archived docs (completed milestones, past plans) |
