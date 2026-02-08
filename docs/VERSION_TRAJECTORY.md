# Cot Version Trajectory — Benchmarked Against Zig

Zig took 10 years and 36+ contributors to reach 0.15. Cot can move faster with LLM-assisted development, but the milestones are real engineering gates that can't be skipped. This document maps Cot's planned trajectory against Zig's actual history.

## Where Zig Was at Each Version

| Zig Version | Date | Key Milestone | Contributors |
|-------------|------|---------------|-------------|
| 0.1.1 | Oct 2017 | First beta (C++ compiler, LLVM backend) | 3 |
| 0.2.0 | Mar 2018 | Error handling (`try`/`catch`), coroutines introduced | 16 |
| 0.3.0 | Sep 2018 | comptime reflection, `@typeInfo`, WebAssembly (experimental) | 36 |
| 0.4.0 | Apr 2019 | `zig cc`, SIMD vectors, bundled libc, Wasm default-enabled | 46 |
| 0.5.0 | Sep 2019 | Async redesign, WASI Tier 2, result location semantics | 67 |
| 0.6.0 | Apr 2020 | Tuples, `@as`, sentinel pointers, ZLS repo created | 122 |
| 0.7.0 | Nov 2020 | ZLS 0.1.0 released (1 week after), macOS cross-compilation | 187 |
| 0.8.0 | Jun 2021 | Self-hosted compiler major push, multiple native backends | 144 |
| 0.9.0 | Dec 2021 | WASI Tier 1, saturating arithmetic | 177 |
| **0.10.0** | **Oct 2022** | **Self-hosted compiler becomes DEFAULT** | 272 |
| **0.11.0** | **Aug 2023** | **Package manager debuts** (`build.zig.zon`) | 269 |
| 0.12.0 | Apr 2024 | x86 backend at 97%, lazy package deps | 268 |
| 0.13.0 | Jun 2024 | LLVM 18, small release | 73 |
| 0.14.0 | Mar 2025 | Incremental compilation, labeled switch | 251 |
| 0.15.0 | Aug 2025 | Self-hosted x86 backend default, async/await REMOVED | 162 |

**Key Zig timelines:**
- First commit → 0.3: **3 years** (Cot: 6 weeks)
- 0.3 → self-hosting (0.10): **4 years**
- 0.3 → package manager (0.11): **5 years**
- First commit → LSP (ZLS): **5 years** (Apr 2020)
- Total: first commit → 0.15: **10 years**

---

## Where Cot Is Now (0.3)

**Time from first commit:** ~6 weeks
**Contributors:** 1 + Claude

**Cot 0.3 already has things Zig 0.3 didn't:**
- LSP server with 5 features (Zig: 19 months after 0.3)
- First-class closures with capture (Zig: still doesn't have this)
- String interpolation (Zig: still doesn't have this)
- ARC memory management (fundamentally different approach)
- Traits (Zig uses comptime duck typing)
- Tuples (Zig: 0.6, two years later)
- Semantic tokens in editor (ZLS added this much later)

**Cot 0.3 lacks things Zig 0.3 had:**
- comptime (Zig's killer feature — full compile-time execution)
- 15+ architecture targets (via LLVM)
- Cross-compilation
- Code formatter
- Substantial stdlib (crypto, JSON, event I/O, concurrency)

---

## Cot's Planned Trajectory

### 0.3 → 0.4: Make It Pleasant to Use

**Zig parallel: 0.3 → 0.4 (6 months)**
Zig added `zig cc`, SIMD, bundled libc. Cot focuses on developer experience.

| Feature | Notes |
|---------|-------|
| `cot fmt` | Auto-formatter (like `zig fmt` which existed at Zig 0.3) |
| Improved error messages | Source locations, underlines, suggestions |
| LSP: autocomplete | The biggest missing IDE feature |
| LSP: rename, find references | |
| Tree-sitter grammar | Broader editor support |
| StringBuilder | Efficient string building |
| `for key, value in map` | Iterator protocol |
| `match` expressions | Full pattern matching |
| `std/fs` | File I/O (open, read, write) |
| `std/os` | Process args, env vars, exit |
| `std/fmt` | String formatting |
| `std/math` | Math functions beyond `@sqrt` |
| Build manifest | `cot.toml` for project configuration |

### 0.5: Make It Production-Capable

**Zig parallel: 0.5 (async redesign, WASI Tier 2)**
Both languages hit async at 0.5. Cot adds the full-stack web story.

| Feature | Notes |
|---------|-------|
| `async fn` / `await` | Language-level async |
| Native event loop | epoll (Linux), kqueue (macOS) |
| Browser async | JS Promise interop via Wasm |
| `std/net` | TCP/HTTP server and client |
| IR split (`lower_clif.zig`) | Bypass Wasm for async on native (if needed) |
| Web framework prototype | `@server` / `@client` annotations |
| WASI target | `--target=wasm32-wasi` |

### 0.6: Make It Community-Ready

**Zig parallel: 0.6 (tuples, ZLS created)**
Cot already has tuples and LSP. Focus shifts to ecosystem.

| Feature | Notes |
|---------|-------|
| Package manager | `cot add`, `cot remove`, dependency resolution |
| Package registry | cot.land |
| Cross-compilation | Native binary for any target from any host |
| Multi-file module system | Explicit exports, proper namespacing |

### 0.7: Expand the Standard Library

**Zig parallel: 0.7 (macOS cross-compilation, ZLS 0.1.0)**
Cot's LSP is already ahead of where ZLS was at 0.7.

| Feature | Notes |
|---------|-------|
| `std/json` | JSON parse/serialize |
| `std/crypto` | Hash functions, HMAC |
| `std/random` | PRNG |
| `std/time` | Timestamps, sleep, timers |
| `std/regex` | Regular expressions |
| `std/dom` | Browser DOM API |
| Cot framework v1 | Full-stack Next.js-style experience |
| Documentation | Language guide, API docs, tutorials |

### 0.8: Optimize the Compiler

**Zig parallel: 0.8 (self-hosted compiler major push)**
Both languages optimize their compilers at 0.8.

| Feature | Notes |
|---------|-------|
| Incremental compilation | Only recompile changed files |
| Compile-time evaluation | Expand const eval toward comptime |
| Compiler performance | Benchmark and optimize hot paths |
| Additional native targets | RISC-V, other ARM variants |
| Debug info | DWARF output for native debugging |

### 0.9: Mature the Ecosystem

**Zig parallel: 0.9 (WASI Tier 1, iOS targeting)**

| Feature | Notes |
|---------|-------|
| WASI Tier 1 | Full WASI support, tested against runtimes |
| Mobile targets | iOS, Android (via Wasm or native) |
| FFI | C interop, JS npm interop |
| Concurrency model | Decision: actors, message passing, or shared memory |
| Benchmark suite | Performance tracking across versions |

### 0.10: Prepare for Self-Hosting

**Zig parallel: 0.10 (self-hosted compiler becomes default)**
Zig achieved self-hosting here. Cot starts the self-hosting work.

| Feature | Notes |
|---------|-------|
| Cot parser in Cot | Rewrite scanner + parser in Cot |
| Cot checker in Cot | Type checking, scope resolution |
| Cot IR in Cot | SSA construction |
| Bootstrap chain | `cot-stage0` (Zig) compiles `cot-stage1` (Cot) |
| Language spec draft | Formal syntax and semantics |

### 0.11: Self-Hosting

**Zig parallel: 0.11 (package manager debuts)**
Zig's package manager came at 0.11. Cot targets self-hosting here — the compiler is written in Cot and compiles itself.

| Feature | Notes |
|---------|-------|
| **Self-hosted compiler** | Cot compiler written in Cot, compiles itself |
| Bootstrap verified | stage0 (Zig) → stage1 (Cot) → stage2 (Cot) produces identical binary |
| Zig dependency eliminated | The Zig codebase becomes bootstrap-only |
| Performance parity | Self-hosted compiler matches or beats Zig-hosted |

**What self-hosting means for Cot:**
- The compiler can evolve in its own language
- New contributors only need to know Cot (not Zig)
- Proves the language is powerful enough for systems programming
- Removes the "toy language" perception

**What self-hosting requires from Cot:**
- File I/O (reading source files)
- String manipulation (parsing, error messages)
- Hash maps (symbol tables, type registries)
- Tree data structures (AST)
- Binary output (Wasm bytecode, ELF/MachO)
- The entire compiler pipeline reimplemented

### 0.12 → 1.0: Polish and Stabilize

| Version | Focus |
|---------|-------|
| 0.12 | Optimize self-hosted compiler, incremental compilation |
| 0.13 | Language spec finalized, syntax frozen |
| 0.14 | Ecosystem maturity, package registry, framework v2 |
| 0.15 | Release candidates, stability testing |
| **1.0** | **Public release — stability commitment, semver** |

---

## Velocity Comparison

| Milestone | Zig (from first commit) | Cot (projected) | Speedup |
|-----------|------------------------|-----------------|---------|
| First beta (0.1) | 2 years | ~2 weeks | ~50x |
| Language real (0.3) | 3 years | ~6 weeks | ~26x |
| LSP | 5 years | 6 weeks | ~43x |
| Package manager | 8 years | ~0.6 (TBD) | — |
| Self-hosting | 7 years | ~0.11 (TBD) | — |
| 1.0 | Not reached (10+ years) | TBD | — |

The early-stage speedup is ~26x — LLM-assisted development compresses the "write boilerplate, port reference implementations" phase dramatically. 6 weeks to reach what took Zig 3 years with 36 contributors. The later milestones (self-hosting, ecosystem) involve design decisions and community building that can't be compressed the same way, but the velocity advantage should remain significant.

---

## Key Differences in Trajectory

### Things Cot Won't Copy from Zig

1. **Zig removed async at 0.15.** Cot should learn from this — design async right the first time (0.5) rather than adding and removing it.
2. **Zig's comptime is its killer feature but adds enormous compiler complexity.** Cot's monomorphized generics + const eval may be the right tradeoff for a language targeting web developers.
3. **Zig took 7 years to self-host.** With LLM assistance and Cot's simpler type system, 0.11 is achievable.
4. **Zig's package manager came very late (0.11, 8 years in).** Cot should have it at 0.6 — modern languages need packages early.

### Things Cot Should Copy from Zig

1. **Version discipline.** Zig doesn't rush to 1.0. Each version represents real progress.
2. **Test infrastructure.** Zig's behavior test suite (1,900+ tests) catches regressions. Cot's 1,600 tests are a good start.
3. **Cross-compilation as a feature.** Zig's ability to target any platform from any platform is a genuine differentiator.
4. **`zig fmt` existed at 0.3.** Cot should have `cot fmt` by 0.4 at the latest.

---

## Tracking Progress

Update this section as versions ship:

| Version | Target Date | Actual Date | Key Deliverable | Status |
|---------|-------------|-------------|-----------------|--------|
| 0.1 | — | Dec 2025 | First working compiler | Done |
| 0.2 | — | Jan 2026 | Generics, closures, native AOT | Done |
| 0.3 | — | Feb 2026 | CLI, LSP, stdlib, traits, tests | Done |
| 0.4 | TBD | — | Formatter, file I/O, better errors | Planned |
| 0.5 | TBD | — | Async, web framework | Planned |
| 0.6 | TBD | — | Package manager, cross-compilation | Planned |
| 0.7 | TBD | — | Expanded stdlib, framework v1 | Planned |
| 0.8 | TBD | — | Compiler optimization | Planned |
| 0.9 | TBD | — | WASI Tier 1, mobile, FFI | Planned |
| 0.10 | TBD | — | Self-hosting preparation | Planned |
| 0.11 | TBD | — | **Self-hosted compiler** | Planned |
| 1.0 | TBD | — | Public release | Planned |
