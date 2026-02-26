# Claude AI Instructions

## 🚨🚨🚨 ABSOLUTE #1 RULE — NEVER WORKAROUND, NEVER SIMPLIFY 🚨🚨🚨

**NEVER implement workarounds for missing Cot language features.** If the Cot compiler doesn't support a pattern you need (e.g. variable declarations in switch arms, a missing operator, a type system gap), **STOP and ask the user** to implement the missing feature in the Zig compiler first. The self-hosted compiler must use idiomatic Cot — every workaround is tech debt that defeats the purpose of dogfooding.

- **NO** restructuring code to avoid a language limitation
- **NO** extracting logic to helper functions just because a construct doesn't compile
- **NO** falling back to if-else chains because switch doesn't support something
- **NO** simplifying data structures because generics don't work for a case
- **ALWAYS** identify the exact compiler limitation and report it to the user
- The user will fix the Zig compiler. That is the correct workflow.

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

**Cot** is a Wasm-first compiled language for full-stack web development.
**Pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.
**Compiler:** Written in Zig (permanent, like Deno's Rust dependency).

**Stdlib** is a separate repo (`cotlang/std`) included as a git submodule at `stdlib/`. After cloning: `git submodule update --init stdlib`. When modifying stdlib files, changes must be committed in the submodule first (`cd stdlib && git add . && git commit && git push`), then the updated submodule ref committed in the parent repo.

---

## CLI

```
cot <file.cot>                  # Implicit build (shorthand for cot build)
cot build <file.cot> [-o name]  # Compile to executable (or .wasm)
cot run <file.cot> [-- args]    # Compile, run in /tmp, clean up, forward exit code
cot test <file.cot>             # Compile + run in test mode
cot bench <file.cot>            # Run benchmarks
cot check <file.cot>            # Type-check without compiling
cot lint <file.cot>             # Check for warnings
cot fmt <file.cot>              # Format source code in-place (--check for CI, --stdout for pipe)
cot init [name]                 # Create new project (cot.json, src/main.cot, .gitignore)
cot lsp                         # Start language server (LSP over stdio)
cot version                     # Print version: cot 0.3.2 (arm64-macos)
cot help [command]              # Print help (per-subcommand help available)
```

**Key files:** `compiler/cli.zig` (arg parsing, help text), `compiler/main.zig` (command dispatch + compileAndLink), `compiler/project.zig` (cot.json manifest loading).

**cot.json integration:** All file-requiring commands (`build`, `run`, `test`, `bench`, `check`, `lint`, `fmt`) read `cot.json` from CWD when no input file is given. If `main` field is set, it's used as the input file. `cot init myapp && cd myapp && cot run` just works.

**Output naming:** Strip path, strip `.cot`, append `.wasm` for wasm targets. `app.cot` → `./app` (native) or `./app.wasm` (wasm). Override with `-o`.

---

## Versioning

**Single source of truth:** `VERSION` file at repo root (plain text, e.g. `0.3.2`).

**Flow:** `VERSION` → `build.zig` reads via `@embedFile` → injected as `build_options` → `compiler/cli.zig` imports `@import("build_options").version`.

**To bump the version:** Edit `VERSION`, rebuild. That's it.

**Design (audited from Go/Zig/Rust/Deno):**
- Rust pattern: plain text VERSION file (simplest, CI-friendly)
- Zig pattern: `@import("build_options")` comptime injection
- SemVer `0.X.Y` (standard for pre-1.0)
- Help banner shows major.minor only (`Cot 0.3`), `cot version` shows full (`cot 0.3.2 (arm64-macos)`)

---

## Architecture

Two native compilation paths exist. The direct path (`--direct-native`) bypasses Wasm entirely.

```
Cot Source → Scanner → Parser → Checker → IR → SSA
  ├── --target=wasm32 → lower_wasm.zig → wasm/ → .wasm file
  ├── --target=native (default, indirect) → lower_wasm.zig → wasm/ → wasm_parser
  │     → wasm_to_clif/ → ir/clif/ → machinst/ → isa/ → emit → .o → linker → executable
  └── --direct-native → ssa_to_clif.zig → ir/clif/ → machinst/ → isa/ → emit → .o → linker → executable
```

**Key directories:**
| Path | Purpose | Reference |
|------|---------|-----------|
| `compiler/cli.zig` | CLI arg parsing, help text, version | — |
| `compiler/main.zig` | Command dispatch, compileAndLink | — |
| `compiler/frontend/` | Scanner, parser, checker, lowerer | — |
| `compiler/ssa/passes/` | rewritegeneric, decompose, rewritedec, schedule, layout, lower_wasm | Go `ssa/*.go` |
| `compiler/codegen/wasm/` | Wasm bytecode generation + linking | Go `wasm/ssa.go`, `wasmobj.go` |
| `compiler/codegen/native/ssa_to_clif.zig` | SSA → CLIF IR (direct native path) | `rustc_codegen_cranelift` `src/base.rs`, `pointer.rs` |
| `compiler/codegen/native/wasm_to_clif/` | Wasm → CLIF IR translation (indirect path) | Cranelift `cranelift/src/translate/` |
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
| SSA → CLIF (direct native) | `references/rust/compiler/rustc_codegen_cranelift/src/` |
| Wasm → CLIF (indirect path) | `references/wasmtime/crates/cranelift/src/translate/` |
| CLIF → ARM64 | `references/wasmtime/cranelift/codegen/src/isa/aarch64/` |
| Language semantics | Zig (error unions, defer, comptime) |

---

## Builtin Pipeline

Two categories:
| Category | Examples | Implementation |
|----------|----------|----------------|
| **Compiler intrinsics (~35)** | `@intCast`, `@sizeOf`, `@intToPtr`, `@ptrOf`, `@lenOf`, `@string`, `@assert`, `@panic` | Inline Wasm ops in `lower.zig` |
| **Runtime functions (~50)** | `alloc`, `dealloc`, `fd_write`, `exit`, `time`, `net_socket`, etc. | Wasm module functions in `arc.zig`/`wasi_runtime.zig` → `func_indices` in `driver.zig` |

**Runtime functions are NOT builtins.** They are regular functions exposed via `extern fn` declarations in `stdlib/sys.cot`. User code imports `std/sys` to access them. The compiler links them by name through `func_indices`.

**Runtime functions are Wasm MODULE functions, NOT host imports.** The compiler has ZERO host imports. If a function name is missing from `func_indices`, `wasm_gen.zig` silently calls function index 0 (alloc) — a silent bug.

**To add a new runtime function:** arc.zig or wasi_runtime.zig (body + addToLinker) → driver.zig (func_indices + native override) → stdlib/sys.cot (extern fn declaration)

---

## Testing

**Workflow: `zig build test` once, then `cot test` for everything else.**

`zig build test` validates the Zig compiler internals. Run it once after changes to confirm the compiler builds correctly. After that, **use `cot test` as the primary verification tool** — it exercises the full pipeline (parse → check → SSA → Wasm → execute) and catches real-world regressions that unit tests miss.

```bash
zig build test                                    # Compiler internals (~163 tests, run once)
cot test test/e2e/features.cot                    # Primary: 341 feature tests, native
cot test test/e2e/features.cot --target=wasm32    # Primary: same tests, wasm via wasmtime
cot test test/cases/<category>.cot                # Targeted: specific category
./test/run_all.sh                                 # Full suite (~1,620 tests across 66 files)
```

**`cot test --target=wasm32`** runs Wasm binaries via `wasmtime` (must be installed). Use this to verify Wasm codegen — bugs often manifest on one target but not the other.

**Troubleshooting tip:** When a feature works on native but fails on wasm (or vice versa), test both targets to isolate whether the bug is in the shared frontend or in a target-specific backend.

**Adding Cot tests:** Add `test "name" { }` blocks to `.cot` files. Run `cot test <file>`.
**Adding compiler tests:** Add `test "..." { }` blocks in Zig source with inline Cot snippets.
**Never embed `.cot` test files in Zig code.** Use `cot test` for that.

**Test directories:**
- `test/cases/` — Category unit tests (21 files, ~122 tests)
- `test/e2e/` — Comprehensive feature tests (46 files, ~1,500 tests)
- All tests use inline `test "name" { @assertEq(...) }` format
- See `claude/TESTING.md` for full details

**Every new feature must:**
1. Work on Wasm (`--target=wasm32`)
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
- Run `zig build test` once after compiler changes, then use `cot test` for ongoing verification
- Use `cot test test/e2e/features.cot` (native) and `cot test test/e2e/features.cot --target=wasm32` (wasm) as the primary check — these catch real regressions faster than unit tests
- After changing `compiler/lsp/`: run `zig build` to update the LSP binary
- After changing `editors/vscode/`: rebuild + reinstall extension (see Editor Extensions & LSP section)
- After changing either: do BOTH — `zig build` AND reinstall extension
- Check `claude/specs/WASM_3_0_REFERENCE.md` when touching Wasm codegen
- Check `claude/PIPELINE_ARCHITECTURE.md` for full pipeline reference map
- Reference `bootstrap-0.2/` for working code examples
- Make incremental changes, verify each one

**DO NOT:**
- Modify bootstrap-0.2 (frozen)
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
| `claude/PIPELINE_ARCHITECTURE.md` | **Full pipeline map, reference for every stage** |
| `claude/BR_TABLE_ARCHITECTURE.md` | Why br_table appears in generated code |
| `claude/specs/WASM_3_0_REFERENCE.md` | Wasm 3.0 opcodes and adoption plan |
| `claude/ROADMAP.md` | Forward-looking roadmap: 0.4→1.0, competitive positioning |
| `claude/VERSION_TRAJECTORY.md` | **Self-hosting trajectory** — benchmarked against Zig, inspirational |
| `claude/SELF_HOSTING_FEATURES.md` | Feature gap analysis for self-hosting (Zig→Cot patterns) |
| `claude/ARC_AUDIT.md` | ARC memory management audit — 4 critical gaps fixed |
| `docs/syntax.md` | Complete language syntax reference with examples |
| `VISION.md` | Language vision, design principles, execution roadmap |
| `claude/CONCURRENCY_DESIGN.md` | Concurrency roadmap: spawn, channels, work-stealing, atomic ARC |
| `claude/BUSINESS_MODEL.md` | Licensing, trademark, revenue model, funding strategy |
| `claude/RELEASE_PLAN.md` | 0.4 release plan: branding, distribution, polish, criteria |
| `claude/TESTING.md` | Test system: 66 files, ~1,620 tests, error-union isolation |
| `claude/archive/` | Historical: 17 archived docs (completed milestones, past plans) |
