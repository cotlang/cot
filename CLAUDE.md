# Claude AI Instructions

## 🚨🚨🚨 MANDATORY: LEARN THE LANGUAGE BEFORE WRITING ANY CODE 🚨🚨🚨

**Before doing ANY work on this project, you MUST:**

1. **Read `docs/syntax.md` completely** — this is the full language syntax reference with examples for every feature. Commit every construct to memory: variables, types, structs, enums, unions, traits, generics, error handling, async/await, closures, `@safe` mode, builtins, operators, control flow.

2. **Read representative stdlib files** — at minimum read `stdlib/list.cot`, `stdlib/map.cot`, `stdlib/string.cot`, and `stdlib/sys.cot` to see real idiomatic Cot code. These show how structs, generics, methods, error handling, and memory management work in practice.

3. **Read `test/e2e/features.cot`** — 370 tests covering every language feature. This is the canonical reference for how the language actually works. If you're unsure about syntax, the answer is in this file.

**WHY:** Claude agents repeatedly waste hours inventing syntax that doesn't exist, using patterns from other languages (Go, Rust, C++) instead of Cot's actual syntax, and guessing at features that are already implemented. Every hour spent flailing with wrong syntax is an hour not spent on real work. The language is well-documented — read it first.

**If you skip this step, you WILL:**
- Write `func` instead of `fn`
- Write `[]int` instead of `[int]` or `List(int)`
- Forget that `@safe` mode auto-injects `self` and auto-refs struct params
- Miss that Cot has string interpolation (`"${x}"`), optional chaining (`?.`), error unions (`!T`)
- Invent workarounds for features that already exist

---

## 🚨 USE PIPELINE DEBUGGER FOR ALL BUGS — NO TEMPORARY PRINTS 🚨

**NEVER add temporary `std.debug.print` or `eprintln` statements to debug.** Use the pipeline debug system instead:

```bash
COT_DEBUG=all ./zig-out/bin/cot build file.cot       # All pipeline phases
COT_DEBUG=codegen ./zig-out/bin/cot build file.cot    # Codegen only
COT_SSA=funcName ./zig-out/bin/cot build file.cot     # SSA HTML visualizer
```

**Use `debug.zig`** for ALL logging. Enhance the debug system with new categories/phases as needed — it's an investment, not a workaround.

## 🚨 NEVER WORKAROUND, NEVER SIMPLIFY 🚨

**NEVER implement workarounds for Cot compiler bugs or missing features.** STOP and report the bug to the user so they can fix the compiler. The self-hosted compiler must use idiomatic Cot — every workaround is tech debt.

## 🚨 NEVER INVENT — ALWAYS COPY REFERENCE IMPLEMENTATIONS 🚨

1. Find the reference implementation (Go, Cranelift, or Swift)
2. Do line-by-line comparison
3. Copy exactly — don't invent, don't simplify, don't "improve"

## 🚨 GIT RULES 🚨

- **NEVER** destructive operations (`checkout --`, `reset --hard`, `push --force`, `clean -f`) without asking
- **NEVER** `git add .` or `git add -A` — stage files by name (stdlib submodule shows as modified)
- **ALWAYS** use absolute paths when checking if files/directories exist

---

## Project Overview

**Cot** is a compiled language for full-stack web development with native and Wasm targets.
**Pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

**Version:** 0.4.1 — 370/370 features, 78/78 e2e tests, Cranelift native backend.

---

## Architecture

```
zig/libcot/ (72K lines)                  rust/libclif/
  cli.zig (arg parsing)                    src/lib.rs (C ABI entry)
  main.zig (command dispatch)              src/cir.rs (CIR reader)
  driver.zig (pipeline orchestrator)       src/translate.rs (CIR → Cranelift)
  frontend/ (scanner, parser,              Cargo.toml (cranelift deps)
    checker, lower)
  ssa/ (SSA passes)                      zig/libclif/
  codegen/wasm/ (Wasm bytecode)            lib.zig (C ABI entry, same as rust)
  codegen/ssa_to_cir.zig (SSA → CIR)      cir_read.zig (CIR reader)
  codegen/arc_runtime.zig (ARC as CIR)    cir_translate.zig (CIR → hand-ported CLIF)
  codegen/io_runtime.zig (I/O as CIR)     compile.zig, machinst/, isa/, regalloc/
  codegen/libclif.zig (C ABI to libclif)
  lsp/ (language server)
```

**The boundary is CIR.** libcot produces CIR bytes. libclif (rust or zig) consumes them.

**Native compilation flow:**
```
Cot source → libcot (parse, check, lower, SSA passes)
  → ssa_to_cir.zig (SSA → CIR binary format, includes runtime + data + entry wrapper)
  → libclif (CIR → Cranelift IR → native .o)
  → linker (.o → executable)
```

**Backend selection:** `--backend=cranelift` (default, real Cranelift via `rust/libclif`) or `--backend=zig` (hand-ported Cranelift via `zig/libclif`). Both consume identical CIR bytes via the `clif_compile` C ABI.

**Key directories:**
| Path | Purpose |
|------|---------|
| `zig/libcot/cli.zig` | CLI arg parsing, `--backend` flag |
| `zig/libcot/main.zig` | Command dispatch, compileAndLink |
| `zig/libcot/driver.zig` | Pipeline orchestrator |
| `zig/libcot/frontend/` | Scanner, parser, checker, lowerer |
| `zig/libcot/ssa/passes/` | SSA optimization passes |
| `zig/libcot/codegen/wasm/` | Wasm bytecode generation |
| `zig/libcot/codegen/ssa_to_cir.zig` | SSA → CIR direct translation |
| `zig/libcot/codegen/arc_runtime.zig` | ARC runtime as CIR |
| `zig/libcot/codegen/io_runtime.zig` | I/O runtime as CIR |
| `zig/libcot/codegen/print_runtime_native.zig` | Print runtime as CIR |
| `zig/libcot/codegen/signal_runtime.zig` | Signal handler as CIR |
| `zig/libcot/codegen/test_runtime_native.zig` | Test runner as CIR |
| `zig/libcot/codegen/libclif.zig` | C ABI bindings to libclif |
| `zig/libcot/lsp/` | Language server (LSP over stdio) |
| `rust/libclif/` | Real Cranelift backend (CIR → .o) |
| `zig/libclif/` | Hand-ported Cranelift backend (CIR → .o, drop-in) |
| `editors/vscode/` | VS Code/Cursor extension |

**Reference implementations (copy, don't invent):**
| Component | Reference |
|-----------|-----------|
| Cot → Wasm | `references/go/src/cmd/compile/internal/wasm/` |
| SSA → CIR | `references/rust/compiler/rustc_codegen_cranelift/src/` |
| Concurrency | `references/swift/stdlib/public/Concurrency/` |

---

## CLI

```
cot build <file.cot> [-o name] [--backend=zig|cranelift]
cot run <file.cot> [-- args]
cot test <file.cot>
cot bench <file.cot>
cot check <file.cot>
cot lint <file.cot>
cot fmt <file.cot>
cot init [name]
cot lsp
cot version
cot help [command]
```

---

## Build & Test

```bash
# Build (requires Rust toolchain for Cranelift backend)
cd rust/libclif && cargo build --release && cd ../..
zig build                                    # From repo root

# Test
zig build test                               # Compiler unit tests
./zig-out/bin/cot test test/e2e/features.cot # 370 feature tests
./test/run_all.sh                            # Full suite (78 e2e files)

# Self-hosted compiler
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot  # 3.4s via Cranelift
/tmp/selfcot version                                     # cot 0.4.1 (self-hosted)
```

**Two `cot` binaries:**
- `cot` on PATH → Homebrew release binary. Do NOT use for development.
- `./zig-out/bin/cot` → Local dev build from `zig build`. Always use full path.

**Stdlib** is a git submodule at `stdlib/` (`cotlang/std`). NEVER `git add .` — stage files by name.

---

## Self-Hosted Compiler (selfcot)

Lives in `self/`, written in Cot. Compiles `.cot` → `.wasm`. ~46,000 lines across 41 files.

- `self/cot.json` sets `"safe": true` — all `self/` files use `@safe` mode
- Struct params are automatically pointers in `@safe` mode
- `self` is auto-injected in methods — no explicit `self: *Type`
- selfcot compiles via Cranelift in 3.4s, 375MB RAM
- selfcot2 (selfcot compiling itself) crashes in List realloc — known ARC bug in Wasm codegen

---

## CIR Binary Format

The contract between `zig/libcot` and `rust/libclif` (or `zig/libclif`). Serialized by `ssa_to_cir.zig`, read by `cir.rs` / `cir_read.zig`.

- SPIR-V-inspired binary format with string heap, function definitions, data definitions
- Section 0x01: string heap, Section 0x04: data/globals, Section 0x06: function definitions
- All integer params use CIR_I64 (Cranelift 64-bit ABI)
- `OP_BITCAST` (0x0049) for i64↔f64 conversion
- `OP_FUNC_ADDR` (0x0095) carries full callee signature for function pointers
- Entry wrapper (`__cot_entry`) stores argc/argv/envp and calls `_cot_main`
- See `claude/CIR_FORMAT_SPEC.md` for full spec

---

## Testing

```bash
zig build test                                    # Compiler unit tests (once)
./zig-out/bin/cot test test/e2e/features.cot      # 370 features, native
./zig-out/bin/cot test test/e2e/features.cot --target=wasm  # Wasm via wasmtime
./test/run_all.sh                                 # Full suite (78 e2e files)
```

**Test directories:**
- `test/cases/` — Category unit tests (22 files)
- `test/e2e/` — Comprehensive feature tests (78 files)

---

## Concurrency

618 tests, 50 Swift features at parity. See `claude/CONCURRENCY_SWIFT_PORT.md`.

**Gap 9 (native async state machines)** is unblocked — real Cranelift handles the switch dispatch that the hand-ported backend couldn't. Implementation planned in `claude/RELEASE_0_4_1.md`.

---

## Documents

| Document | Purpose |
|----------|---------|
| `VERSION` | Version single source of truth |
| `claude/RELEASE_0_4_1.md` | 0.4.1 release plan and status |
| `claude/CIR_FORMAT_SPEC.md` | CIR binary format specification |
| `claude/PIPELINE_ARCHITECTURE.md` | Full pipeline reference map |
| `claude/CONCURRENCY_SWIFT_PORT.md` | Swift concurrency parity (50 features) |
| `claude/CONCURRENCY.md` | Concurrency architecture + Gap 9 |
| `claude/ROADMAP.md` | Forward-looking roadmap |
| `claude/TESTING.md` | Test system details |
| `claude/WASM_CODEGEN_REFERENCE.md` | Wasm codegen pipeline reference |
| `claude/WASM_PLATFORM_VISION.md` | Wasm platform vision |
| `claude/COT_SSA_PLAN.md` | SSA HTML visualizer |
| `docs/syntax.md` | Complete language syntax reference |

---

## Behavioral Guidelines

**DO:**
- Use `./zig-out/bin/cot test test/e2e/features.cot` as primary check (370 tests)
- Run `zig build test` once after compiler changes
- Copy reference implementations line-by-line
- Use absolute paths for all file checks
- Make incremental changes, verify each one

**DO NOT:**
- Add temporary `std.debug.print` — use `debug.zig`
- Run tests redundantly — once per task
- Invent approaches — copy references
- Comment out failing tests or leave TODOs
- Use `git add .` — always stage by name
- Use destructive git commands without asking
