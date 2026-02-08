# Claude AI Instructions

## 🚨 CRITICAL RULES

### 1. Never Invent — Always Copy Reference Implementations

**The #1 cause of project failure**: Claude tries to "figure out" fixes instead of copying the reference. This has caused 5 rewrites.

**The fix that works every time:**
1. Find the reference implementation (Go or Cranelift)
2. Do line-by-line comparison
3. Copy exactly — don't invent, don't simplify, don't "improve"

See `TROUBLESHOOTING.md` for full methodology.

### 2. Check Wasm 3.0 Before Adding Features

**Read `docs/specs/WASM_3_0_REFERENCE.md`** before implementing anything touching Wasm codegen. Cot currently emits Wasm 1.0 but Wasm 3.0 (released Sep 2025) may offer better solutions:
- Recursive functions → `return_call` (0x12) instead of `call` + `return`
- Closures/function pointers → `call_ref` (0x14) instead of `call_indirect`
- Error propagation → `try_table`/`throw` instead of manual checks

### 3. br_table is Intentional

Read `docs/BR_TABLE_ARCHITECTURE.md` if confused. br_table is copied from Go's dispatch loop pattern. Do NOT try to remove it.

---

## Project Overview

**Cot** is a Wasm-first compiled language for full-stack web development.
**Pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.
**Compiler:** Written in Zig (permanent, like Deno's Rust dependency).

---

## CLI

```
cot <file.cot>                  # Implicit build (shorthand for cot build)
cot build <file.cot> [-o name]  # Compile to executable (or .wasm)
cot run <file.cot> [-- args]    # Compile, run in /tmp, clean up, forward exit code
cot test <file.cot>             # Compile + run in test mode
cot lsp                         # Start language server (LSP over stdio)
cot version                     # Print version: cot 0.3.1 (arm64-macos)
cot help [command]              # Print help (per-subcommand help available)
```

**Key files:** `compiler/cli.zig` (arg parsing, help text), `compiler/main.zig` (command dispatch + compileAndLink).

**Output naming:** Strip path, strip `.cot`, append `.wasm` for wasm targets. `app.cot` → `./app` (native) or `./app.wasm` (wasm). Override with `-o`.

---

## Versioning

**Single source of truth:** `VERSION` file at repo root (plain text, e.g. `0.3.1`).

**Flow:** `VERSION` → `build.zig` reads via `@embedFile` → injected as `build_options` → `compiler/cli.zig` imports `@import("build_options").version`.

**To bump the version:** Edit `VERSION`, rebuild. That's it.

**Design (audited from Go/Zig/Rust/Deno):**
- Rust pattern: plain text VERSION file (simplest, CI-friendly)
- Zig pattern: `@import("build_options")` comptime injection
- SemVer `0.X.Y` (standard for pre-1.0)
- Help banner shows major.minor only (`Cot 0.3`), `cot version` shows full (`cot 0.3.1 (arm64-macos)`)

---

## Architecture

**ALL code goes through Wasm first.** Native is AOT-compiled FROM Wasm via Cranelift-port.

```
Cot Source → Scanner → Parser → Checker → IR → SSA
  → lower_wasm.zig (SSA → Wasm ops) → wasm/ (Wasm bytecode)
      ├── --target=wasm32 → .wasm file
      └── --target=native (default) → wasm_parser → wasm_to_clif/
          → ir/clif/ → machinst/lower.zig → isa/{aarch64,x64}/
          → emit.zig → .o → linker → executable
```

**Key directories:**
| Path | Purpose | Reference |
|------|---------|-----------|
| `compiler/cli.zig` | CLI arg parsing, help text, version | — |
| `compiler/main.zig` | Command dispatch, compileAndLink | — |
| `compiler/frontend/` | Scanner, parser, checker, lowerer | — |
| `compiler/ssa/passes/` | rewritegeneric, decompose, rewritedec, schedule, layout, lower_wasm | Go `ssa/*.go` |
| `compiler/codegen/wasm/` | Wasm bytecode generation + linking | Go `wasm/ssa.go`, `wasmobj.go` |
| `compiler/codegen/native/wasm_to_clif/` | Wasm → CLIF IR translation | Cranelift `cranelift/src/translate/` |
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
| Wasm → CLIF | `references/wasmtime/crates/cranelift/src/translate/` |
| CLIF → ARM64 | `references/wasmtime/cranelift/codegen/src/isa/aarch64/` |
| Language semantics | Zig (error unions, defer, comptime) |

---

## Builtin Pipeline

Two categories:
| Category | Examples | Implementation |
|----------|----------|----------------|
| **Compiler intrinsics** | `@intCast`, `@sizeOf`, `@intToPtr` | Inline Wasm ops in `lower.zig` |
| **Runtime functions** | `@alloc`, `@dealloc`, `@realloc`, `@memcpy` | Wasm module functions in `arc.zig` → `func_indices` in `driver.zig` |

**Runtime builtins are Wasm MODULE functions, NOT host imports.** The compiler has ZERO host imports. If a function name is missing from `func_indices`, `wasm_gen.zig` silently calls function index 0 (cot_alloc) — a silent bug.

**To add a new runtime builtin:** parser.zig → checker.zig → lower.zig → arc.zig (body + addToLinker) → driver.zig (func_indices)

---

## Testing

```bash
zig build test              # All tests (~660 Cot tests via native_e2e_test.zig)
cot test file.cot           # Run inline tests in a single file
COT_DEBUG=codegen zig build test  # With debug output
```

**Test directories:**
- `test/cases/` — Category unit tests (21 files, ~100 tests)
- `test/e2e/` — Comprehensive feature tests (8 files, ~560 tests)
- All tests use inline `test "name" { @assert_eq(...) }` format
- See `docs/TESTING.md` for full details

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
- Run `zig build test` after every change
- Reinstall editor extensions after modifying anything in `editors/` (see Editor Extensions section)
- Check `docs/specs/WASM_3_0_REFERENCE.md` when touching Wasm codegen
- Check `docs/PIPELINE_ARCHITECTURE.md` for full pipeline reference map
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

## Editor Extensions

**Location:** `editors/vscode/` — VS Code / Cursor extension (syntax highlighting + LSP client)

**After any change to the extension** (grammar, extension.ts, package.json), reinstall to Cursor:

```bash
cd editors/vscode && npm install && npm run compile && npx @vscode/vsce package --allow-missing-repository
cursor --uninstall-extension cot-lang.cot-lang 2>/dev/null; cursor --install-extension cot-lang-0.1.0.vsix --force
```

This includes changes to:
- `syntaxes/cot.tmLanguage.json` — syntax highlighting rules
- `src/extension.ts` — LSP client code
- `language-configuration.json` — bracket matching, comments, indentation
- `package.json` — capabilities, settings, dependencies

**Always reinstall after modifying any extension file.** Cursor caches extensions aggressively.

---

## Documents

| Document | Purpose |
|----------|---------|
| `VERSION` | **Version single source of truth** (edit to bump) |
| `TROUBLESHOOTING.md` | **Debugging methodology — read before any debugging** |
| `docs/PIPELINE_ARCHITECTURE.md` | **Full pipeline map, reference for every stage** |
| `docs/BR_TABLE_ARCHITECTURE.md` | Why br_table appears in generated code |
| `docs/specs/WASM_3_0_REFERENCE.md` | Wasm 3.0 opcodes and adoption plan |
| `docs/ROADMAP_1_0.md` | Road to 1.0: versioning, feature waves, Wasm IR analysis |
| `VISION.md` | Language vision, design principles, execution roadmap |
| `docs/archive/` | Historical: completed milestones, past bug fixes, postmortems |
