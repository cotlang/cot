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

```zig
struct User {
    name: string,
    email: string,

    fn display(self: *User) string {
        return self.name ++ " <" ++ self.email ++ ">"
    }
}

fn greet(name: string) string {
    return "Hello, " ++ name
}
```

Familiar syntax. No semicolons. Methods inside struct bodies. Type inference. No allocator parameters.

### 2. ARC Memory Management

Developers don't think about memory. The compiler handles it.

```zig
fn example() {
    let user = User { name: "Alice", email: "alice@example.com" }
    // Compiler inserts: retain(user)
    process(user)
    // Compiler inserts: release(user)
}
// No manual free, no GC pauses, no borrow checker fights
```

### 3. Dual Backend: Native-First with Wasm Deployment

Cot compiles to native and Wasm through independent backend paths:

**Native (default):** SSA → CLIF IR → ARM64/x64 machine code. The compiler translates its SSA representation directly to CLIF IR (Cranelift's intermediate representation), then lowers through register allocation and instruction emission to produce native binaries. Runtime functions (ARC memory management, I/O, print) are generated as CLIF IR and call libc directly. No Wasm involved.

**Wasm (`--target=wasm32`):** SSA → Wasm bytecode. Outputs `.wasm` files that run in browsers, WASI runtimes, or edge platforms.

**The key differentiator:** Other languages that target Wasm (Rust, Go, C) require an external Wasm runtime (wasmtime, wasmer) to run on the server. Cot doesn't — native is the default target, producing regular native binaries with no runtime dependency. Wasm is available as a deployment target for browsers and edge.

```
Cot:  Source → SSA → native binary (default, runs directly)
                   └─→ .wasm (for browser, edge, WASI runtimes)
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

```zig
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
                      │  → Lowerer → SSA                │
                      └─────────────────────────────────┘
                                       │
                              SSA (internal IR)
                                       │
                    ┌──────────────────┼──────────────────┐
                    ▼                  ▼                  ▼
              Native (default)    Wasm (browser)    Wasm (WASI)
              SSA → CLIF → ARM64  SSA → Wasm        SSA → Wasm
              No runtime needed   Runs in V8        Runs on edge
```

### Why Zig for the Bootstrap Compiler?

Zig is the bootstrap language — it compiles the first version of Cot, which then compiles itself. Like Go (bootstrapped from C), Rust (bootstrapped from OCaml), and Zig (bootstrapped from C++), the bootstrap compiler is frozen once self-hosting is achieved.

### Self-Hosting Strategy

Full self-hosting is actively in progress — 21,264 lines of Cot across 10 files (237 tests), covering ~70% of the frontend. The goal is complete self-hosting including native codegen (ARM64, x64). Wasm self-compilation comes first (~20K new lines), then native codegen (~71K lines) is ported incrementally.

1. Ship Cot with the Zig compiler (current)
2. Build real applications in Cot (cot.land, cot.dev)
3. Complete self-hosted frontend, then backend
4. Transition when the self-hosted compiler passes all tests

Self-hosting is the graduation ceremony, not the entrance exam.

### Dogfooding: Build the Ecosystem in Cot

Since self-hosting is deferred, we prove the language by building its own ecosystem:

**cot.land** — Package registry and manager (server-side Cot, HTTP, database, JSON). Proves the server story.

**cot.dev** — Documentation site + interactive playground (client-side Cot, Wasm in browser, DOM). Proves the browser story.

Both together prove the full-stack story — the same language, shared types, client and server.

---

## Execution Roadmap

See [claude/ROADMAP.md](claude/ROADMAP.md) for the detailed roadmap.

**Approach:** Zig-style versioning. Versions mark architectural milestones, not feature checklists. 1.0 will take multiple years, but Cot should be viable for projects well before 1.0.

| Version | Theme | Key Deliverables |
|---------|-------|-----------------|
| 0.3 | Make the language real | All language features, stdlib, I/O, concurrency, generics optimization |
| 0.4 | Distribution polish | Homebrew, VS Code marketplace, `cot upgrade`, shell completions |
| 0.5 | Build real products | Package manager, web framework, database drivers |
| 0.6+ | Make it community-ready | Package registry (cot.land), cross-compilation, test coverage |
| 1.0 | Public release | Stable syntax, documentation, ecosystem |

---

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Primary competitor | TypeScript + Next.js | Only full-stack web language; Cot does it better |
| Primary target | Native + Wasm | Native by default; Wasm for browser, server, edge |
| Memory management | ARC | No GC pauses, no borrow checker, fits Wasm perfectly |
| I/O model | WASI-designed | Standardized, portable, maps to libc on native |
| Compiler language | Zig | Simple, fast, good Wasm support |
| Self-hosting | Deferred | Ship first, prove later |
| Syntax | Zig-inspired | Familiar, readable, minimal |
| Server deployment | Native binary (default) | Direct SSA → CLIF → native compilation, no Wasm runtime |
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
