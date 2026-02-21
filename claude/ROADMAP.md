# Cot Roadmap

## Current: 0.3.2 (Feb 2026)

Compiler is feature-complete for the core language. 31 stdlib modules, 66 test files (~1100+ tests), LSP with 7 features, @safe mode, comptime infrastructure, ARC memory management. All 6 waves of 0.4 feature work are **DONE**. Runtime builtins moved to stdlib via `extern fn`.

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
| 10 | Self-hosting progress (parser in Cot) | ~5% (scanner done) |

**Release criteria:** `brew install cot-land/tap/cot` works, VS Code extension on marketplace, all tests pass on native + Wasm.

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
| Spawn + channels | Go-style: `spawn {}`, `Channel(T)`, `select`, work-stealing. | Go goroutines |
| Atomic ARC | Thread-safe reference counting. | Swift atomic refcounting |
| Permission system | `--allow-read`, `--allow-net`, `--allow-env`. | Deno permissions |
| OpenTelemetry | Built-in tracing, auto-instrument HTTP. | Deno 2.2 OTel |
| `std/sync` | Mutex, RwLock, Atomic(T), WaitGroup. | Go `sync` |
| SIMD vectors | `@Vector(N, T)` mapped to hardware SIMD. | Zig `@Vector` |

---

## 0.7–0.9 — Compiler Maturity

| Version | Focus |
|---------|-------|
| 0.7 | Incremental compilation, compiler performance |
| 0.8 | Self-hosted parser + checker in Cot |
| 0.9 | Self-hosted SSA + Wasm codegen in Cot |

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
