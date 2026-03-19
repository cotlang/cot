# Cot Roadmap

## Current: 0.3.6 (Mar 2026)

ARC at ~98% Swift SILGen parity: `opt_make`/`opt_tag`/`opt_data` SSA decomposition for `?*T`, centralized `emitCopyValue`/`emitDestroyValue` dispatch, zero ad-hoc retain/release. 84 test files (~1,790 tests, 83/83 passing on native + Wasm). Self-hosted compiler: 44,700 lines across 42 files, zero crashes (all 13 frontend files reach selfcot checker stage). Previous: concurrency (Go-style select, work-stealing scheduler), 34 stdlib modules, LSP, @safe mode, comptime, ARC, CI/CD.

**What ships:** A developer can `cot init`, write an HTTP server with crypto + regex + path handling + SQLite, `cot test --watch` during development, `cot lint` + `cot check` for fast feedback, `cot bench` for performance, `cot doc` for API docs, and `cot build` for a native binary. Like Deno, but compiled to native with zero runtime overhead.

---

## Next: 0.4 — The Public Release

**Theme: "A language developers take seriously."**

All language features and stdlib are done. What remains is distribution polish:

| # | Item | Status |
|---|------|--------|
| 1 | CI/CD pipeline | **Done** — test.yml (macOS + Linux), release.yml (binaries on tag) |
| 2 | Homebrew tap | Not started |
| 3 | x86_64-macos binary | Not started (release.yml currently: aarch64-macos + x86_64-linux) |
| 4 | VS Code marketplace | Not started |
| 5 | `cot upgrade` (self-update) | Not started |
| 6 | Shell completions (zsh/bash/fish) | Not started |
| 7 | `cot init` improvements (test template, next-steps) | Not started |
| 8 | Logo & brand assets | Not started |
| 9 | cot.dev launch (docs site + playground) | Not started |
| 10 | Self-hosting progress (frontend in Cot) | 44,700 lines across 42 files. All 13 frontend files reach selfcot checker stage (0 crashes). 2 selfcot checker bugs remain (enum method resolution, stdlib type mismatch). |

**Release criteria:** `brew install cotlang/tap/cot` works, VS Code extension on marketplace, all tests pass on native + Wasm.

---

## 0.5 — Build Real Products

**Theme: "Share and deploy."**

The centerpiece is the **package ecosystem** — a full port of JSR (jsr.io) to Cot. This includes the registry (cot.land), package manager (integrated into `cot` CLI), and C source bundling so packages can wrap native libraries without bloating the compiler distribution.

**Full vision:** [cot-land/pkg/VISION.md](https://github.com/cot-land/pkg/blob/main/VISION.md)

| Feature | Description | Reference |
|---------|-------------|-----------|
| Package manager | `cot add`, `cot remove`, `cot publish`. Lockfile, dependency resolution. | `deno add`, `cargo add` |
| Package registry | cot.land — full JSR port. Postgres, cloud storage, CDN. | jsr.io |
| C source bundling | `"c_sources"` in cot.json — packages bring their own C code, compiler compiles it for any target. Enables sqlite, postgres, image libs as registry packages, not stdlib. | Zig `addCSourceFile`, Go cgo |
| Cross-compilation | `cot build --target=x86_64-linux` from macOS, including C sources. | Zig cross-compile |
| Test coverage | `cot coverage` — line/branch, lcov output, HTML report. | `deno coverage` |
| `std/dom` | Browser DOM API for `--target=wasm32`. | wasm-bindgen |
| Web framework prototype | `@server`/`@client` annotations, shared types, auto-serialization. | Next.js, Fresh |
| `std/csv`, `std/toml` | Config file formats. | Deno std |
| `std/streams` | ReadableStream, WritableStream, pipe, buffer. | WHATWG Streams |
| `std/net` | DNS, TLS, connection pooling. | Go `net` |

**Key design principle:** Database drivers (SQLite, Postgres) and native library bindings are **registry packages**, not stdlib. `std/sqlite` exists temporarily to bootstrap cot.land, then migrates to a registry package once C source bundling ships. The compiler stays minimal — it provides the C compilation capability, packages provide the C source.

---

## 0.6 — Production-Grade

**Theme: "Ship with confidence."**

| Feature | Description | Reference |
|---------|-------------|-----------|
| Permission system | `--allow-read`, `--allow-net`, `--allow-env`. | Deno permissions |
| OpenTelemetry | Built-in tracing, auto-instrument HTTP. | Deno 2.2 OTel |
| ~~Work-stealing scheduler~~ | ~~Done in 0.3.5~~ | Go runtime `proc.go` |
| ~~`select` statement~~ | ~~Done in 0.3.5~~ | Go `select` |
| SIMD vectors | `@Vector(N, T)` mapped to hardware SIMD. | Zig `@Vector` |

---

## 0.7–0.9 — Compiler Maturity

| Version | Focus |
|---------|-------|
| 0.7 | Incremental compilation, compiler performance |
| 0.8 | Self-hosted IR + SSA in Cot |
| 0.9 | Self-hosted Wasm codegen in Cot |

---

## 0.10 — Self-Hosted Compiler

The compiler is written in Cot and compiles itself. Bootstrap chain: `cot-stage0` (Zig) → `cot-stage1` (Cot) → `cot-stage2` (Cot, identical binary).

See `VERSION_TRAJECTORY.md` for the detailed self-hosting roadmap benchmarked against Zig.

---

## 1.0 — Stability Commitment

Language syntax frozen. Semver. Migration guides. `cot.dev` with playground. Language specification. Example applications.

---

## Competitive Position

**Cot's unique combination** — no other language has all of:
- Server = native binary (no runtime, no cold starts)
- Browser = Wasm (first-class target, same source)
- Types and logic shared across client/server boundary
- Memory automatic (ARC) but predictable (no GC pauses)
- Everything built-in: fmt, lint, test, bench, doc, LSP — one binary

| vs | Cot exploits |
|----|-------------|
| TypeScript/Next.js | Not compiled, GC, weak types, node_modules bloat |
| Go | No browser target, GC, backend only |
| Rust | Complexity, learning curve, Wasm = afterthought |
| Zig | Manual memory, no closures, no full-stack story |
| Deno | V8 runtime overhead, GC, not truly compiled |
