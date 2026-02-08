# Cot Language Vision

## What is Cot?

Cot is a compiled full-stack language that directly competes with TypeScript + Next.js.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

**The problem Cot solves:** TypeScript dominates full-stack development not because developers love it, but because it's the only language that works in both browser and server with shared types and logic. Developers tolerate TypeScript's weak type system, runtime errors, and performance overhead because no alternative offers the same full-stack story.

Cot is that alternative — a real compiled language with ARC memory management, native Wasm support, and a framework that handles client/server compilation transparently.

---

## Why TypeScript Wins (And Shouldn't)

TypeScript's dominance comes from one architectural accident: JavaScript runs in browsers, and Node.js brought it to servers. This means:

- Same language on client and server
- Shared types across the boundary (via Next.js RSC, tRPC, etc.)
- One ecosystem (npm) for everything
- Low friction — write code, it runs

But TypeScript has fundamental weaknesses:

| Problem | Impact |
|---------|--------|
| No real compilation | JIT overhead, cold starts, bundle size |
| Structural typing | Type safety is an illusion — runtime errors leak through |
| GC pauses | Unpredictable latency in server and client |
| node_modules | 200MB+ for a hello world |
| Not really one language | Server needs Node APIs, client needs DOM APIs, they're different worlds shimmed together |

**Cot takes what makes TypeScript win (full-stack, shared types, low friction) and rebuilds it on a real foundation.**

---

## Language Position

```
                    Simple ←────────────────→ Complex
                       │                         │
     TypeScript ───────┼─────────────────────────┤  GC, slow, not really compiled
                       │                         │
     Bun + Next.js ────┼─────────────────────────┤  Still TypeScript underneath
                       │                         │
           Cot ────────┼──────■                  │  ARC, compiled, Wasm-native
                       │                         │
         Swift ────────┼───────────■             │  ARC, no Wasm, Apple-only
                       │                         │
          Rust ────────┼─────────────────────────■  Borrow checker
```

### The Gap Cot Fills

No existing language offers all of:
- **Full-stack** — browser + server, shared types, one framework
- **Compiled** — real AOT compilation, not JIT
- **ARC** — no GC pauses, no manual memory, no borrow checker
- **Wasm-native** — browser target is first-class, not bolted on
- **Approachable** — if you know TypeScript, you can write Cot

---

## Design Principles

### 1. Zig-Inspired Syntax, Simplified

```cot
struct User {
    name: []u8
    email: []u8
}

impl User {
    fn display(self) []u8 {
        return self.name + " <" + self.email + ">"
    }
}

fn greet(name: []u8) []u8 {
    return "Hello, " + name
}
```

Familiar syntax. No semicolons. Traits and impl blocks. Type inference. No allocator parameters.

### 2. ARC Memory Management

Developers don't think about memory. The compiler handles it.

```cot
fn example() {
    let user = User { name: "Alice", email: "alice@example.com" }
    // Compiler inserts: retain(user)
    process(user)
    // Compiler inserts: release(user)
}
// No manual free, no GC pauses, no borrow checker fights
```

### 3. Wasm as Compilation Target AND Internal IR

Cot uses Wasm in two ways:

**As internal IR:** All code compiles through Wasm bytecode before AOT compilation to native. This simplifies the compiler — one backend, consistent semantics, every feature works on both targets automatically.

**As deployment format:** `--target=wasm32` outputs `.wasm` files that run directly in browsers. The same code runs natively on the server and as Wasm in the browser.

**The key differentiator:** Other languages that target Wasm (Rust, Go, C) require an external Wasm runtime (wasmtime, wasmer) to run on the server. Cot doesn't — the compiler itself AOT-compiles Wasm to native. No runtime dependency. You get a regular native binary.

```
Other languages:  Source → Wasm → wasmtime REQUIRED to run on server
Cot:              Source → Wasm → native binary (runs directly)
                              └──→ .wasm (for browser, or optionally WASI runtimes)
```

### 4. WASI-Modeled I/O

Cot uses [WASI](https://wasi.dev/) as the design blueprint for system I/O — not because Cot requires a WASI runtime, but because WASI is a well-designed, standardized interface for the operations every program needs (files, sockets, clocks, random).

The compiler translates WASI-modeled I/O to the appropriate target:

| Target | I/O becomes |
|--------|-------------|
| Native (default) | libc/syscalls directly |
| `--target=wasm32` (browser) | Web API imports (fetch, DOM, etc.) |
| `--target=wasm32-wasi` | WASI imports (for edge/serverless runtimes) |

This means the same Cot source code can produce:
- A native server binary (no external runtime)
- A `.wasm` file for the browser
- A `.wasm` file for Cloudflare Workers / Fermyon Spin / wasmCloud

No other language does all three from the same source without third-party tooling.

### 5. Full-Stack by Default

The Cot framework handles client/server compilation transparently, like Next.js but without the JavaScript:

```cot
// shared — compiled for both targets automatically
struct User {
    id: i64
    name: []u8
}

fn validate_email(email: []u8) bool {
    return email.contains("@")
}

// server — compiled to native
@server
fn get_user(id: i64) !User {
    return db.query("SELECT * FROM users WHERE id = ?", id)
}

// client — compiled to Wasm for browser
@client
fn render_user(user: User) {
    dom.set_text("#name", user.name)
}
```

The framework's build step calls the compiler twice — once for native (server), once for Wasm (client) — and wires up the boundary automatically (serialization, HTTP transport, etc.). Developers don't think about compilation targets.

For standalone use (CLI tools, libraries, Wasm modules), the `cot` compiler CLI accepts explicit targets. The framework is optional.

---

## Target Audience

**Primary:** Web developers currently using TypeScript + Next.js / Bun who want:
- A real compiled language without TypeScript's runtime surprises
- Better performance without Rust's complexity
- One language for browser and server that actually compiles

**Secondary:** Backend developers using Go/Python/Java who want:
- Browser deployment without learning a second language
- Modern syntax without legacy baggage
- ARC instead of GC

**Not for:**
- Operating system development
- Embedded systems with extreme memory constraints
- Developers who want fine-grained memory control (use Zig or Rust)

---

## Architecture

### Compilation Pipeline

```
                      ┌─────────────────────────────────┐
                      │         Cot Source Code          │
                      └─────────────────────────────────┘
                                       │
                                       ▼
                      ┌─────────────────────────────────┐
                      │         Cot Compiler (Zig)      │
                      │  Scanner → Parser → Checker     │
                      │  → Lowerer → SSA → Wasm Codegen │
                      └─────────────────────────────────┘
                                       │
                              Wasm bytecode (internal)
                                       │
                    ┌──────────────────┼──────────────────┐
                    ▼                  ▼                  ▼
              Native (default)    Wasm (browser)    Wasm (WASI)
              AOT → ARM64/x64    Runs in V8,       Runs on edge,
              No runtime needed   SpiderMonkey      Cloudflare, etc.
```

### Why Zig for the Compiler?

Following the Deno model: Deno is written in Rust and runs TypeScript. It's not self-hosted, and that's fine. Cot's compiler is written in Zig — a permanent tool, not a bootstrap.

### Self-Hosting Strategy

Self-hosting is a future goal, not a prerequisite. The plan:

1. Ship Cot with the Zig compiler
2. Build real applications in Cot (cot.land, cot.dev)
3. Stabilize the language
4. Attempt self-hosting when the language is mature

Self-hosting is the graduation ceremony, not the entrance exam.

### Dogfooding: Build the Ecosystem in Cot

Since self-hosting is deferred, we prove the language by building its own ecosystem:

**cot.land** — Package registry and manager (server-side Cot, HTTP, database, JSON). Proves the server story.

**cot.dev** — Documentation site + interactive playground (client-side Cot, Wasm in browser, DOM). Proves the browser story.

Both together prove the full-stack story — the same language, shared types, client and server.

---

## Execution Roadmap

See [docs/ROADMAP_1_0.md](docs/ROADMAP_1_0.md) for the detailed roadmap.

**Approach:** Zig-style versioning. Versions mark architectural milestones, not feature checklists. 1.0 will take multiple years, but Cot should be viable for projects well before 1.0.

| Version | Theme | Key Deliverables |
|---------|-------|-----------------|
| 0.3 | Make the language real | All language features, stdlib, I/O |
| 0.4 | Make it pleasant to use | LSP, formatter, test runner, error messages |
| 0.5 | Make it production-capable | Async, concurrency, web framework |
| 0.6+ | Make it community-ready | Package manager, registry, cot.land |
| 1.0 | Public release | Stable syntax, documentation, ecosystem |

---

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Primary competitor | TypeScript + Next.js | Only full-stack web language; Cot does it better |
| Primary target | Wasm | Universal — browser, server, edge from same source |
| Memory management | ARC | No GC pauses, no borrow checker, fits Wasm perfectly |
| I/O model | WASI-designed | Standardized, portable, maps to libc on native |
| Compiler language | Zig | Simple, fast, good Wasm support |
| Self-hosting | Deferred | Ship first, prove later |
| Syntax | Zig-inspired | Familiar, readable, minimal |
| Server deployment | Native binary (default) | No Wasm runtime required — AOT compiles directly |
| Framework model | Next.js-style | @server/@client annotations, transparent compilation |

---

## Summary

Cot is a pragmatic language for web developers who want:
- Performance without complexity
- One language for browser and server, with shared types
- A real type system, not TypeScript's structural typing
- Memory safety without mental overhead (ARC)
- Native server performance without requiring an external Wasm runtime

The compiler stays in Zig until the language proves itself through real applications (cot.land, cot.dev). Self-hosting is the final exam, not the entrance exam.
