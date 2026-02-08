# Cot: Road to 1.0

## Where We Are: Cot 0.3

The compiler handles a real programming language — generics, closures, ARC, error unions, defer, traits, all working on both Wasm and native AOT. The architecture is proven: Cot source compiles to Wasm bytecode, which either runs in a browser or gets AOT-compiled to ARM64/x64 native executables through a Cranelift-port pipeline.

### What 0.3 Lacks for Real Use

A developer downloading Cot today would immediately hit:

1. **No I/O** — can't read files, make HTTP requests, or print formatted output
2. **No standard library** — no collections beyond List(T), no JSON, no string formatting
3. **No package manager** — can't install libraries
4. **No string interpolation** — `"Hello, " + name` works but `"Hello, {name}"` doesn't
5. **No async** — can't write a web server that handles concurrent connections
6. **No editor support** — no LSP, no syntax highlighting
7. **No framework** — the full-stack client/server story doesn't exist yet
8. **No documentation** — no language guide, no API docs, no tutorials

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

### 0.3: Make the Language Real

All language features, standard library, and I/O land here.

**Collections and Strings:**
- Map(K,V) — hash map with set/get/has/delete/keys/values
- Set(T) — built on Map(K,V)
- String interpolation — `"Hello, {name}"`
- StringBuilder — efficient append-based string building
- `@print` / `@println` — output to stdout

**Type System:**
- Trait bounds on generics — `fn sort(T: Comparable)(list: *List(T))`
- `for key, value in map` — iterator protocol
- `match` expressions — full pattern matching
- Multiple return values — `fn divmod(a, b: i64) (i64, i64)`
- `weak` references — ARC cycle breaker

**I/O and Standard Library:**
- `std/fs` — file I/O (open, read, write, readFile, writeFile)
- `std/os` — process args, env vars, exit
- `std/fmt` — string formatting
- `std/math` — math functions
- `std/json` — JSON parse/serialize
- `std/dom` — browser DOM API
- WASI-modeled I/O abstraction layer

**I/O implementation:** I/O functions are defined using the WASI interface design. The compiler emits:
- Native: libc calls (`read()`, `write()`, `socket()`) linked at compile time
- Browser Wasm: Web API imports (fetch, DOM) via import section
- WASI Wasm: WASI imports consumed by WASI-compatible runtimes

### 0.4: Make It Pleasant to Use

- Error messages with source locations, underlines, suggestions
- LSP server (autocomplete, go-to-definition, hover types)
- `cot fmt` — auto-formatter
- `cot test` — test runner with `test "name" { ... }` blocks
- Syntax highlighting (VS Code, tree-sitter)
- Build system with project manifest (cot.toml)

### 0.5: Make It Production-Capable

- `async fn` / `await` — language-level async
- Native event loop (epoll/kqueue)
- Browser async (JS Promise interop)
- `std/net` — TCP/HTTP server and client
- IR split (`lower_clif.zig`) if needed for async on native
- Web framework prototype (the @server/@client story)

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

Cot 0.3 has built the hard infrastructure — a complete compiler pipeline with dual-target output, ARC memory management, generics, closures, and hundreds of passing tests. The road to 1.0:

1. **0.3:** Language features, type system, stdlib, I/O — make Cot a real language
2. **0.4:** Developer experience — make it pleasant to use
3. **0.5:** Async, concurrency, web framework — make it production-capable
4. **0.6+:** Ecosystem, package manager — make it community-ready
5. **1.0:** Polish, docs, stability — make it public

The Wasm-as-IR architecture works for everything through 0.4. The IR split happens in 0.5 only if async demands it. That's a future concern.

**The compiler is ready. Now build the language around it.**
