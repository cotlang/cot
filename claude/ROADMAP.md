# Cot Roadmap

## Current: 0.3.2 (Feb 2026)

Compiler is feature-complete for the core language. 31 stdlib modules, 69 test files (~1,700 tests), LSP with 7 features, @safe mode, comptime infrastructure, ARC memory management. All 6 waves of 0.4 feature work are **DONE**. Runtime builtins moved to stdlib via `extern fn`. OS threading primitives (threads, mutex, condition variables, atomics, channels) implemented.

**What ships:** A developer can `cot init`, write an HTTP server with crypto + regex + path handling, `cot test --watch` during development, `cot lint` + `cot check` for fast feedback, `cot bench` for performance, `cot doc` for API docs, and `cot build` for a native binary. Like Deno, but compiled to native with zero runtime overhead.

---

## Next: 0.4 — The Public Release

**Theme: "A language developers take seriously."**

All language features and stdlib are done. What remains is distribution polish:

| # | Item | Status |
|---|------|--------|
| 1 | Homebrew tap | Not started |
| 2 | x86_64-macos binary | Not started |
| 3 | VS Code marketplace | Not started |
| 4 | `cot upgrade` (self-update) | Not started |
| 5 | Shell completions (zsh/bash/fish) | Not started |
| 6 | `cot init` improvements (test template, next-steps) | Not started |
| 7 | Improved `@assert_eq` failure output (expected vs actual diff) | Not started |
| 8 | Logo & brand assets | Not started |
| 9 | cot.dev launch (docs site + playground) | Not started |
| 10 | Self-hosting progress (frontend in Cot) | ~80% frontend fidelity (13,381 lines, 189 tests). See `SELF_HOSTING_AUDIT.md` |

**Release criteria:** `brew install cotlang/tap/cot` works, VS Code extension on marketplace, all tests pass on native + Wasm.

---

## 0.5 — Build Real Products

**Theme: "Share and deploy."**

| Feature | Description | Reference |
|---------|-------------|-----------|
| Package manager | `cot add`, `cot remove`, `cot publish`. Lockfile, dependency resolution. | `deno add`, `cargo add` |
| Package registry | cot.land — browse, search, publish packages. | jsr.io, crates.io |
| Cross-compilation | `cot build --target=x86_64-linux` from macOS. | Zig cross-compile |
| Test coverage | `cot coverage` — line/branch, lcov output, HTML report. | `deno coverage` |
| `std/db` | Database driver — SQLite first, then Postgres. | Go `database/sql` |
| `std/dom` | Browser DOM API for `--target=wasm32`. | wasm-bindgen |
| Web framework prototype | `@server`/`@client` annotations, shared types, auto-serialization. | Next.js, Fresh |
| `std/csv`, `std/toml` | Config file formats. | Deno std |
| `std/streams` | ReadableStream, WritableStream, pipe, buffer. | WHATWG Streams |
| `std/net` | DNS, TLS, connection pooling. | Go `net` |

---

## 0.6 — Production-Grade

**Theme: "Ship with confidence."**

| Feature | Description | Reference |
|---------|-------------|-----------|
| ~~Spawn + channels~~ | ~~Go-style: `spawn {}`, `Channel(T)`, `select`, work-stealing.~~ | **DONE** (0.3.x) — OS-level threads, mutex, conditions, atomics, Channel(T). See `threading-design.md` |
| ~~Atomic ARC~~ | ~~Thread-safe reference counting.~~ | **Foundation DONE** (atomics in 0.3.x). ARC atomic upgrade deferred to scheduler work |
| Permission system | `--allow-read`, `--allow-net`, `--allow-env`. | Deno permissions |
| OpenTelemetry | Built-in tracing, auto-instrument HTTP. | Deno 2.2 OTel |
| `spawn {}` + work-stealing | Lightweight task scheduler on top of OS threads. | Go runtime `proc.go` |
| `select` statement | Multiplexed channel receive. | Go `select` |
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
