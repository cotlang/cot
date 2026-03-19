# Cot Roadmap

## Current: 0.3 (Mar 2026)

The language works. Both native and Wasm targets produce correct binaries. ARC memory management at Swift SILGen parity. 34 stdlib modules. LSP, formatter, linter, test runner, benchmarks, doc generator — all built in. Self-hosted compiler at 44,700 lines across 42 files, covering the full pipeline: parse, check, build, optimize, emit.

---

## 0.4 — Wasm Self-Hosting (Apr 2026)

**The compiler compiles itself to Wasm.**

`selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler that can then rebuild itself. The bootstrap chain closes: one portable `.wasm` blob replaces the Zig dependency for anyone building Cot.

| Milestone | Status |
|-----------|--------|
| Self-hosted frontend (parse, check, build) compiles to Wasm | 2 selfcot bugs remaining |
| Self-hosted optimize passes compile to Wasm | Blocked on frontend bugs |
| Self-hosted emit/wasm backend compiles to Wasm | Blocked on frontend bugs |
| selfcot produces a working Wasm binary from Cot source | Not yet |
| selfcot compiles itself (`bootstrap/cot.wasm` checked into repo) | Not yet |
| Bootstrap: `wasmtime bootstrap/cot.wasm build self/main.cot` works | Not yet |

**Release criteria:** The self-hosted compiler, compiled to Wasm, can compile itself. `bootstrap/cot.wasm` is checked into the repo. A fresh clone bootstraps with only `wasmtime`.

---

## 0.5 — Native Self-Hosting (2026)

**The compiler emits native binaries, written entirely in Cot.**

Port `emit/native/` to Cot: Cot IR, instruction selection, register allocation, ARM64/x64 emission, Mach-O/ELF linking. The Wasm bootstrap compiles the native backend, producing a native binary that runs at full speed.

| Component | Zig Lines | New Concepts |
|-----------|-----------|--------------|
| Cot IR (data structures, builder, DFG) | ~4,000 | Medium — data structures |
| SSA → Cot IR lowering | ~2,000 | Medium — translation |
| MachInst / VCode | ~10,000 | Medium — another IR layer |
| Instruction selection (aarch64 + x64) | ~6,300 | High — ISA-specific rules |
| Register allocation (Ion backtracking) | ~11,500 | Very high — the boss fight |
| Binary emission (aarch64 + x64) | ~6,800 | High — encoding tables |
| ABI / calling conventions | ~3,900 | High — platform-specific |
| Object formats (Mach-O + ELF) | ~1,600 | Medium — binary formats |
| Runtime (ARC, I/O, print, test) | ~7,400 | Low — reuse patterns from Wasm runtime |

**Bootstrap chain:**
```
bootstrap/cot.wasm (Wasm, checked in)
  → wasmtime builds native binary
    → native binary rebuilds itself
      → verify: outputs match
```

**Release criteria:** `wasmtime bootstrap/cot.wasm build self/main.cot --target=native -o cot` produces a native binary. That binary compiles itself. Zig is no longer needed.

---

## 0.6 — Language Polish (2026)

**Make the self-hosted compiler competitive.**

The compiler works but needs to be fast, produce good errors, and handle edge cases. This version is about quality before going public.

| Area | Work |
|------|------|
| Compiler performance | Incremental compilation, parallel type checking |
| Binary size | Dead code elimination, function-level linking |
| Error messages | Rich diagnostics, "did you mean X?", fix suggestions |
| Debug info | DWARF generation, source maps, stack traces with line numbers |
| Cross-compilation | `cot build --target=x86_64-linux` from macOS (and vice versa) |
| Test coverage | `cot coverage` — line/branch, lcov output, HTML report |

**Release criteria:** Self-hosted compiler is within 2x of Zig compiler performance on the same inputs. Debug builds produce usable stack traces. Cross-compilation works between macOS and Linux.

---

## 0.7 — Public Release (May 7, 2026)

**"A language developers take seriously."**

Everything before this is building the compiler. This version is about making it accessible to the world. Distribution, branding, ecosystem, first impressions.

| Category | Items |
|----------|-------|
| Distribution | Homebrew tap, apt repo, binaries for all platforms (aarch64-macos, x86_64-macos, x86_64-linux, aarch64-linux) |
| Editor | VS Code/Cursor extension on marketplace, LSP polished |
| CLI polish | `cot upgrade` (self-update), shell completions (zsh/bash/fish), `cot init` improvements |
| Branding | Logo, color palette, cot.dev launch (docs site + playground) |
| Package manager | `cot add`, `cot remove`, `cot publish`, lockfile, dependency resolution |
| Registry | cot.land — package registry |
| Documentation | Getting started guide, language tour, stdlib API docs, examples |

**Release criteria:** `brew install cotlang/tap/cot` works. VS Code extension installs from marketplace. cot.dev is live. A developer can go from zero to running project in 5 minutes.

---

## 0.8 — Build Real Products

**"Ship with confidence."**

The ecosystem matures. Developers can build, test, and deploy real applications.

| Category | Items |
|----------|-------|
| C interop | C source bundling in packages, cross-compile C dependencies |
| Web | `std/dom` for browser Wasm, web framework prototype |
| Database | Database drivers as registry packages (SQLite, Postgres) |
| Safety | Permission system (`--allow-read`, `--allow-net`), ASAN in debug mode |
| Observability | Built-in OpenTelemetry, auto-instrument HTTP |
| Stdlib | `std/csv`, `std/toml`, `std/streams`, `std/net` (DNS, TLS) |

---

## 0.9 — Production-Grade

**"Battle-tested."**

Performance, reliability, and the features that make teams choose Cot for production.

| Category | Items |
|----------|-------|
| Performance | SIMD vectors, PGO, link-time optimization |
| Concurrency | Atomic ARC, structured concurrency |
| Tooling | Profiler, memory leak detector, fuzzing support |
| Stability | Backwards compatibility guarantees begin, migration tooling |

---

## 1.0 — Stable Release

**Language syntax frozen. The promise of stability.**

| Item | Description |
|------|-------------|
| Language specification | Formal spec document |
| Semver commitment | Breaking changes only in major versions |
| Long-term support | Security fixes for 1.x line |
| Example applications | HTTP server, CLI tool, browser app, database-backed service |
| cot.dev playground | Interactive, runs in browser via Wasm |
| Migration guides | Comprehensive upgrade path documentation |

---

## Competitive Position

**Cot's unique combination** — no other language has all of:
- Server = native binary (no runtime, no cold starts)
- Browser = Wasm (first-class target, same source)
- Types and logic shared across client/server boundary
- Memory automatic (ARC) but predictable (no GC pauses)
- Fully self-hosted — compiler written in Cot, bootstrapped from Wasm
- Everything built in: fmt, lint, test, bench, doc, LSP — one binary

| vs | What Cot does differently |
|----|--------------------------|
| **TypeScript** | Compiles to native binary — no V8, no cold starts, no `node_modules` |
| **Go** | Wasm is a first-class target — same source runs server + browser |
| **Rust** | No borrow checker — ARC gives memory safety without the learning curve |
| **Zig** | Closures, `@safe` mode, full-stack story — not just systems programming |
| **Deno** | No runtime overhead — truly compiled, truly native |
