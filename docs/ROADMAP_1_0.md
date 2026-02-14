# Cot: Road to 1.0

## Where We Are: Cot 0.3

The compiler handles a real programming language — generics, closures, ARC, error unions, defer, traits, all working on both Wasm and native AOT. The architecture is proven: Cot source compiles to Wasm bytecode, which either runs in a browser or gets AOT-compiled to ARM64/x64 native executables through a Cranelift-port pipeline.

### What 0.3 Has Achieved

- **Working I/O** — `print`, `println`, `eprint`, `eprintln` with native syscalls
- **Standard library** — `List(T)` with ~35 methods, `Map(K,V)` with splitmix64 hash via `import "std/map"`
- **Cross-file generics** — `SharedGenericContext` enables multi-file programs with generics
- **CLI** — `cot build`, `cot run`, `cot test`, `cot version`, `cot help`
- **Test framework** — inline `test "name" { }` blocks with `@assert`/`@assert_eq`, summary output
- **Basic LSP** — diagnostics, hover, goto definition, document symbols
- **VS Code/Cursor extension** — TextMate syntax highlighting + LSP client
- **Auto scope-exit cleanup** — structs with `free()` get automatic cleanup
- **Traits with bounds** — `where T: Hashable` with monomorphized dispatch

### What 0.3 Still Lacks for Real Use

1. ~~**No collections beyond List(T)**~~ — **Done**: Map(K,V) with splitmix64 hash, Set(T) wrapping Map
2. ~~**No string interpolation**~~ — **Done**: `"Hello, ${name}"` with integer auto-conversion
3. ~~**No file I/O**~~ — **Done**: `std/fs` (File struct, openFile, createFile, 10 methods), `std/os` (args, environ, exit), `std/time`, `std/random`
4. **No package manager** — can't install libraries
5. **No async** — can't write a web server that handles concurrent connections
6. **No framework** — the full-stack client/server story doesn't exist yet
7. **No documentation** — no language guide, no API docs, no tutorials

---

## What 1.0 Means

**1.0 is not "feature-complete forever." It's "useful enough to build real things and stable enough to depend on."**

### The 1.0 Contract

A developer who installs Cot 1.0 should be able to:

1. **Write a web server** that handles HTTP requests, reads files, talks to a database
2. **Write a browser application** that manipulates the DOM, handles events, fetches APIs
3. **Share code between client and server** — the core promise
4. **Use typed collections** — `List(T)`, `Map(K,V)`, `Set(T)`
5. **Handle errors properly** — error unions, try/catch, meaningful error messages
6. **Import packages** — a package manager with a registry
7. **Get editor help** — LSP with autocomplete, go-to-definition, error highlighting
8. **Read documentation** — language guide, standard library API docs, examples
9. **Trust stability** — code that compiles today will compile tomorrow

### What 1.0 is NOT

- Not self-hosting (that's a maturity milestone, not a user requirement)
- Not the fastest language ever (competitive is enough)
- Not feature-frozen (1.x releases add features; 1.0 syntax is stable)

---

## The I/O Architecture: WASI as Design Blueprint

### The Insight

Other languages that compile to Wasm (Rust, Go, C) require an external Wasm runtime (wasmtime, wasmer) to run on the server. Cot doesn't — the compiler AOT-compiles directly to native. But Cot still needs an I/O model.

WASI provides a well-designed, community-vetted standard for system I/O (files, sockets, clocks, random). Rather than inventing our own abstractions, Cot uses WASI's interface design as the blueprint for its I/O layer. The compiler then translates these operations to the appropriate target:

```
                    Cot I/O call (e.g., file.read())
                              │
                    ┌─────────┼─────────────┐
                    ▼         ▼             ▼
              Native       Browser        WASI runtime
              libc/        Web API        WASI imports
              syscalls     imports        (Cloudflare, etc.)
```

### Why This Matters

| Language | Server-side Wasm | Native server | Browser |
|----------|-----------------|---------------|---------|
| Rust | wasmtime required | Yes (separate build) | wasm-bindgen required |
| Go | TinyGo + wasmtime | Yes (separate build) | TinyGo, limited stdlib |
| **Cot** | **Optional** (WASI target available) | **Yes** (AOT, no runtime) | **Yes** (first-class target) |

Cot is the only language where server = native binary (no runtime), browser = Wasm, and edge/WASI = optional bonus — all from the same source code, same compiler, same I/O model.

### The Three Output Modes

| Mode | CLI flag | I/O model | Use case |
|------|----------|-----------|----------|
| Native | (default) | libc/syscalls | Server binaries, CLI tools |
| Browser Wasm | `--target=wasm32` | Web API imports | Client-side apps |
| WASI Wasm | `--target=wasm32-wasi` | WASI imports | Edge, serverless, plugins |

For standalone compiler use, developers specify targets explicitly. For the Cot framework (full-stack apps), the framework handles target selection automatically.

---

## The Framework Layer

### Next.js-Style Full-Stack

The Cot framework is the product-level differentiator. Like Next.js, developers write code with annotations, and the build tool handles compilation for each target:

```cot
// shared — compiled for both targets
struct User { id: i64, name: []u8 }

// server — compiled to native
@server
fn get_user(id: i64) !User {
    return db.query("SELECT ...", id)
}

// client — compiled to Wasm
@client
fn render_profile(user: User) {
    dom.set_text("#name", user.name)
}
```

The framework:
1. Compiles `@server` code to a native binary
2. Compiles `@client` code to `.wasm` for the browser
3. Compiles shared code for both targets
4. Generates the boundary layer (serialization, HTTP transport)

Developers never think about `--target` flags in framework projects.

### Framework vs Compiler

The `cot` compiler is a low-level tool (like `rustc` or `zig`). The framework is a higher-level tool (like Next.js or Deno). Both are part of the Cot project, but the compiler exists independently for standalone use.

---

## The Wasm IR Question

### Current Architecture

Every Cot program compiles through Wasm bytecode as internal IR:

```
Cot Source → Frontend → SSA → Wasm bytecode → { Browser | AOT native }
```

This is the right architecture for 0.x. It simplifies the compiler (one backend), ensures feature parity between targets, and catches bugs early.

### Where Wasm Becomes Limiting

**Async/coroutines** is the only feature that truly cannot go through Wasm. Wasm has no stack switching — a function either runs to completion or traps. The [stack switching proposal](https://github.com/WebAssembly/stack-switching) is not in Wasm 3.0 and is years from standardization.

Everything else (including tail calls, exceptions, and typed function references) was resolved by Wasm 3.0 (released September 2025). See `docs/specs/WASM_3_0_REFERENCE.md` for details.

### The IR Split (When Async Demands It)

When implementing `async fn` for native, the compiler will need a direct SSA → CLIF IR path (`lower_clif.zig`) that bypasses Wasm:

```
                    Cot SSA IR (unified)
                          │
               ┌──────────┼──────────┐
               ▼                     ▼
         lower_wasm.zig        lower_clif.zig (NEW)
               │                     │
         Wasm bytecode          CLIF IR
               │                     │
         Browser/WASI           MachInst → Native
```

This is an additive change — the Wasm path remains for browser/WASI targets. The existing CLIF IR, MachInst, regalloc, and emission infrastructure is already built. Only the `lower_clif.zig` translation layer is new.

**Recommendation:** Don't split until async forces it. The current architecture is simpler and catches more bugs.

### Async Strategy

Different targets use different async mechanisms, unified by the same syntax:

- **Native:** OS event loop (epoll/kqueue) + coroutines (stack switching in native code)
- **Browser:** JavaScript Promise interop (`async fn` returns a Promise via JS glue)
- **WASI:** WASI 0.3 async model (component model `future`/`stream` types)

The syntax is the same (`async fn`, `await`). The lowering differs per target.

---

## Versioning Philosophy

**Cot follows Zig's model: versions mark architectural milestones, not feature checklists.**

Zig is 10+ years of development and still on 0.15. Each version represents a meaningful shift in capability. Cot takes the same approach — 1.0 will take multiple years, but the language should be viable for projects well before 1.0.

---

## Feature Roadmap

### 0.3: Make the Language Real (Largely Complete)

**Done:**
- `print` / `println` / `eprint` / `eprintln` — output to stdout/stderr
- `import "std/list"` — cross-file generics, stdlib discovery
- `cot build` / `cot run` / `cot test` / `cot version` — CLI subcommands
- Inline test framework with `@assert` / `@assert_eq`, summary output
- Trait bounds on generics — `where T: Trait` syntax, monomorphized dispatch
- Basic LSP server (diagnostics, hover, goto definition, document symbols)

**Remaining:**
- ~~Map(K,V)~~ — **Done** — hash map with set/get/has/delete/keys/values, auto-free
- ~~Set(T)~~ — **Done** — wraps Map(T, i64), add/has/remove/len/toList, auto-free
- ~~String interpolation~~ — **Done** — `"Hello, ${name}"` with integer auto-conversion
- ~~String equality~~ — **Done** — `@assert_eq("hello", "hello")` via `cot_string_eq`
- ~~`std/fs`~~ — **Done** — File struct, openFile, createFile, stdin/stdout/stderr, read/write/seek/close
- ~~`std/os`~~ — **Done** — argsCount, argLen, argPtr, environCount, environLen, environPtr, exit
- ~~`std/time`~~ — **Done** — nanoTimestamp, milliTimestamp, timestamp, Timer struct
- ~~`std/random`~~ — **Done** — fillBytes, randomInt, randomRange (via getentropy)
- ~~Comptime target builtins~~ — **Done** — `@target_os()`, `@target_arch()`, `@target()` + const-fold if-expressions
- ~~Platform-conditional stdlib~~ — **Done** — `stdlib/fs.cot` uses `if @target_os() == "linux" { ... } else { ... }` for flags
- ~~`--target=wasm32-wasi` WASI imports~~ — **Done** — emits `wasi_snapshot_preview1` imports
- ~~High-level stdlib APIs~~ — **Done** — `arg(n)`, `environ(n)`, `readFile(path)`, `writeFile(path, data)`
- ~~StringBuilder~~ — **Done** — in `std/string`, append/appendByte/appendInt/toString
- ~~String methods~~ — **Done** — `std/string` with ~25 functions (split, trim, indexOf, contains, startsWith, endsWith, replace, etc.)
- ~~`std/math`~~ — **Done** — @abs/@ceil/@floor/@trunc/@round/@sqrt builtins + `std/math` stdlib (abs, min, max, clamp, ipow, fabs, ceil, floor, sqrt, etc.)
- ~~`std/json`~~ — **Done** — recursive descent parser + StringBuilder-based encoder, ported from Go encoding/json
- ~~`std/sort`~~ — **Done** — insertion sort + reverse for List(T)
- ~~Comptime Tier 2~~ — **Done** — `comptime { }` blocks, `@compileError`, dead branch elimination, local const propagation
**Carried to 0.4 (now done):**
- ~~`for key, value in map`~~ — **Done** — iterator protocol, shipped in Wave 1 (`0c5914e`)
- `std/fmt` — partially covered by string interpolation + StringBuilder

**Carried to 0.4 (remaining):**
- Multiple return values — `fn divmod(a, b: i64) (i64, i64)` — deferred, not blocking current programs

**Carried to 0.5+:**
- `weak` references — ARC cycle breaker → 0.5
- `std/dom` — browser DOM API → 0.5 (web framework)

**I/O implementation:** I/O functions are defined using the WASI interface design. The compiler emits:
- Native: libc calls (`read()`, `write()`, `socket()`) linked at compile time
- Browser Wasm: Web API imports (fetch, DOM) via import section
- WASI Wasm: WASI imports consumed by WASI-compatible runtimes

### 0.4: Make It Pleasant to Use — The Deno-Alternative Release

**Theme:** Cot becomes a credible alternative to Deno for building CLI tools, web servers, and full-stack applications. Like Deno, everything is built-in: formatter, test runner, LSP, and standard library. Unlike Deno, Cot compiles to native binaries with zero runtime overhead.

**Philosophy:** The version number is arbitrary. The goal is features and maturity — each wave makes Cot more capable and more correct. We ship 0.4 when the work is done, not on a deadline.

#### Wave 1: Language Completeness — DONE

All seven features shipped in `0c5914e` and `531415f`.

| # | Feature | Status | Commit |
|---|---------|--------|--------|
| 1 | `errdefer` | **DONE** | `0c5914e` — Zig AstGen pattern, LIFO ordering with mixed defer/errdefer |
| 2 | `if (optional) \|val\|` unwrap | **DONE** | `0c5914e` — Zig payload capture syntax, reuses catch capture infra |
| 3 | `for key, value in map` | **DONE** | `0c5914e` — Go range pattern, hash table iteration over occupied slots |
| 4 | Named error set switch | **DONE** | `0c5914e` — `catch \|err\| switch (err) { error.X => ... }` with global variant table |
| 5 | `switch` range prongs | **DONE** | Pre-existing — `1..10 => "digit"`, formatter fixed in `531415f` |
| 6 | Labeled blocks / labeled continue | **DONE** | `0c5914e` — `break :outer` / `continue :outer` in nested loops |
| 7 | Require parens for if/while | **DONE** | `531415f` — Zig pattern, disambiguates `\|` (bitwise OR) from `\|val\|` (capture) |

**Carried forward:** Multiple return values (`fn divmod(a, b: i64) (i64, i64)`) — deferred, not critical for current programs.

#### Wave 2: Developer Experience — DONE

All six features shipped in `531415f`.

| # | Feature | Status | Commit |
|---|---------|--------|--------|
| 8 | `cot fmt` | **DONE** | `531415f` — AST-based formatter with comment interleaving (Go gofmt pattern) |
| 9 | Rich error messages | **DONE** | `531415f` — span underlines (`^~~~`) with ANSI color on TTY |
| 10 | Test filtering | **DONE** | `531415f` — `cot test file.cot --filter="name"` (Zig --test-filter pattern) |
| 11 | LSP: autocomplete | **DONE** | `531415f` — builtins, scope symbols, keywords |
| 12 | LSP: rename symbol | **DONE** | `531415f` — WorkspaceEdit via references |
| 13 | LSP: find references | **DONE** | `531415f` — AST walk collecting identifier matches |

#### Wave 3: Compiler Maturity + Project System (current work)

Combines compiler-internal improvements (Wasm codegen cleanup) with user-facing project infrastructure. The Wasm work makes the compiler more correct and eliminates workarounds before building new stdlib modules on top.

**Compiler internals (from WASM_UPGRADE_PLAN):**

| # | Feature | Description | Risk | Effort | Reference |
|---|---------|-------------|------|--------|-----------|
| 14 | `memory.fill` wiring | **DONE** — Codegen handler added for `wasm_lowered_zero` op. Emits `memory.fill` (0xFC 0x0B). | Low | Hours | Go `cmd/compile/internal/wasm` |
| 15 | Multi-value return cleanup | **DEFERRED** — Reviewed: `compound_len_locals` works correctly. Go reference doesn't use multi-value returns at all. Cleanup is high risk, low reward. Defer to post-1.0. | High | — | — |

**User-facing features:**

| # | Feature | Description | Reference |
|---|---------|-------------|-----------|
| 16 | `cot.json` project manifest | **DONE** — `compiler/project.zig` reads cot.json via `std.json.parseFromSlice`. | `deno.json`, `package.json` |
| 17 | `cot init` | **DONE** — Creates `cot.json`, `src/main.cot`, `.gitignore`. | `deno init`, `cargo init` |
| 18 | `std/http` | **DONE** — TCP sockets (`@net_socket`, `@net_bind`, `@net_listen`, `@net_accept`, `@net_connect`, `@net_set_reuse_addr`), sockaddr_in construction, HTTP response builder. ARM64+x64 native overrides. 11 tests. | Go `net/http`, POSIX sockets |
| 19 | `std/url` | **DONE** — URL parsing (scheme, host, port, path, query, fragment). Heap-allocated pattern. 13 tests. | Go `net/url` |
| 20 | `std/encoding` | **DONE** — Base64 encode/decode, hex encode/decode, URL-safe base64. 26 tests. | Deno `std/encoding`, Go `encoding/` |

**Why multi-value cleanup belongs here:** `std/http` will return strings and structs heavily. The `compound_len_locals` workaround has caused 5+ bugs already (see MEMORY.md items 12, 14, 17). Cleaning it up before building more stdlib on top prevents a class of compound-return regressions.

#### Wave 4: Ecosystem Polish

| # | Feature | Description | Reference |
|---|---------|-------------|-----------|
| 21 | Tree-sitter grammar | Enables syntax highlighting in any editor (Neovim, Helix, Zed, GitHub). | Zig tree-sitter-zig |
| 22 | `cot check` | Type-check without compiling — fast feedback loop. | `zig build check`, `deno check` |
| 23 | `cot lint` (basic) | Unused variables, unreachable code, shadowing warnings. | `deno lint`, `zig` warnings |
| 24 | Improved `cot test` output | Colors, timing per test, `--verbose` flag, failure diffs. | Deno test output, Zig test output |

#### Deferred Wasm Work (post-0.4)

These Wasm upgrades are tracked in `docs/WASM_UPGRADE_PLAN.md` but aren't needed for 0.4. They improve performance or enable future features, not current ones.

| Feature | Why deferred | Target |
|---------|-------------|--------|
| WasmGC arrays + nested structs | Only matters for `--target=wasm32-gc` (browser story, 0.5+) | 0.5 |
| `call_ref` typed function refs | `call_indirect` works fine, `call_ref` is a perf optimization | 0.5 |
| `throw`/`try_table` exceptions | ABI-breaking, 1-2 weeks, highest risk. Current error unions work. Enables reliable defer-across-calls when needed. | 0.5 |

#### Deno Feature Parity Comparison

| Deno Feature | Cot Status | Notes |
|--------------|-----------|-------|
| `deno run` | `cot run` | Done |
| `deno test` | `cot test --filter` | Done |
| `deno fmt` | `cot fmt` | Done |
| `deno lint` | — | Wave 4 |
| `deno check` | — | Wave 4 |
| `deno init` | `cot init` | Done |
| `deno.json` | `cot.json` | Done |
| Built-in HTTP server | `std/http` | Done |
| LSP | autocomplete, rename, references | Done |
| TypeScript types | Cot types (stronger) | Done |
| Single binary | `cot` binary | Done |
| Edge deploy | `--target=wasm32-wasi` | Done |

**What Cot has that Deno doesn't:**
- AOT compilation to native binary (no V8 runtime, no cold starts)
- ARC memory management (no GC pauses)
- Wasm as first-class browser target (not just server-side)
- ARM64/x64 native output from the same source
- MCP server for AI-assisted development (already built in Cot itself)

#### 0.4 Success Criteria

A developer should be able to:
1. `cot init myapp` → scaffold a project
2. Write an HTTP server that serves JSON API endpoints
3. `cot fmt` → auto-format their code
4. `cot test --filter "api"` → run targeted tests
5. `cot build` → get a native binary with zero dependencies
6. Get autocomplete in their editor for struct fields and methods
7. See clear, helpful error messages when code is wrong

#### Progress

- **Wave 1 (language):** 7/7 done
- **Wave 2 (DX):** 6/6 done
- **Wave 3 (maturity + project system):** 6/7 done (multi-value cleanup deferred)
- **Wave 4 (polish):** 0/4 — can slip to 0.4.x patches

### 0.5: Make It Production-Capable

- `async fn` / `await` — language-level async
- Native event loop (epoll/kqueue)
- Browser async (JS Promise interop)
- `std/net` — TCP/UDP sockets
- IR split (`lower_clif.zig`) if needed for async on native
- Web framework prototype (the @server/@client story)
- `std/crypto` — hash functions, HMAC
- Database driver (`std/sql` or `std/db`)

### 0.6+: Make It Community-Ready

- Package manager (`cot add`, `cot remove`, dependency resolution)
- Package registry (cot.land)
- Cot framework (the full-stack Next.js-style experience)
- Cross-compilation
- Multi-file module system

### 1.0: Public Release

- Language specification (syntax frozen)
- Language guide, tutorials, cookbook
- Standard library API documentation
- Example applications (TODO app, chat server, blog engine)
- cot.dev website with interactive playground
- Stability commitment (semver, deprecation policy)

---

## Competitive Positioning at 1.0

| Language | Strength | Weakness Cot Exploits |
|----------|----------|----------------------|
| **TypeScript + Next.js** | Full-stack ecosystem, familiarity | Not compiled, GC, weak types, node_modules bloat |
| **Go** | Simplicity, concurrency | No browser target, GC, backend only |
| **Rust** | Performance, safety | Complexity, learning curve, Wasm = afterthought |
| **Swift** | ARC, developer experience | No Wasm, Apple-centric |
| **Zig** | Performance, simplicity | Manual memory, no full-stack story |

**Cot's unique position:** The only language where server = native binary (no runtime), browser = Wasm (first-class), types and logic are shared across the boundary, and the framework handles compilation targets transparently.

---

## Dogfooding: MCP Server in Cot (COMPLETE)

The MCP server was the first real program written in Cot — proving the language can build useful tools.

**Location:** `mcp/cot-mcp.cot` (~380 lines, single file)
**Config:** `.mcp.json` at repo root
**Build:** `cot build mcp/cot-mcp.cot -o cot-mcp`
**Protocol:** JSON-RPC 2.0 over stdio, newline-delimited

### What It Provides

Three tools available to Claude Code:
1. **`get_syntax_reference`** — Complete Cot language syntax reference
2. **`get_stdlib_docs`** — Function signatures for any stdlib module (json, string, fs, io, list, map, os, time, random, sort, set, math)
3. **`get_project_info`** — Build/test commands and project structure

### What It Uses

- `std/json` — parse JSON-RPC requests, encode responses
- `std/string` — StringBuilder for building tool text
- `std/io` — BufferedReader/Writer for efficient stdio
- Error unions, generics, closures — real language features in production

### Success

A fresh Claude Code session can write valid Cot code on the first attempt, look up any syntax without guessing, and get stdlib function signatures on demand. The MCP server is actively used during Cot development itself.

All three success criteria met: Claude Code writes valid Cot on first attempt, looks up syntax without guessing, and gets stdlib function signatures on demand.

---

## Open Questions

These don't need answers now, but should be resolved before 1.0:

1. **Async model:** Coroutines (Go-style), async/await (JS/Rust-style), or both?
2. **Module system:** File-based (Go) or explicit exports (Rust/Zig)?
3. **Concurrency:** Shared memory + mutexes, message passing, or actors?
4. **FFI beyond C:** Interop with JS npm packages? Rust crates?
5. **Standard library scope:** Minimal (Go) or batteries-included (Python)?
6. **Governance:** BDFL, RFC process, or foundation?

---

## Summary

Cot 0.3 built the hard infrastructure — a complete compiler pipeline with dual-target output, ARC memory management, generics, closures, and 900+ passing tests. The MCP server (written in Cot) proves the language works for real tools. 0.4 Waves 1-2 are done — the language has errdefer, if-optional, map iteration, labeled loops, a formatter, rich errors, test filtering, and LSP autocomplete/rename/references.

The road to 1.0:

1. **0.3 (COMPLETE):** Language features, type system, stdlib, I/O, MCP server — Cot is a real language
2. **0.4 (IN PROGRESS):** Waves 1-3 done (language + DX + project system). Remaining: ecosystem polish (Wave 4)
3. **0.5:** Async, concurrency, WasmGC completion, web framework — make it production-capable
4. **0.6+:** Ecosystem, package manager — make it community-ready
5. **1.0:** Polish, docs, stability — make it public

The Wasm-as-IR architecture works for everything through 0.4. The IR split happens in 0.5 only if async demands it. Version numbers mark maturity milestones, not deadlines.

**0.4's goal: a developer can build a real project with `cot init`, `cot fmt`, `cot test`, and `cot build` — with good errors, autocomplete, and an HTTP server in the stdlib. Like Deno, but compiled to native.**
