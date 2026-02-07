# Cot Language Vision

## What is Cot?

Cot is a compiled programming language for full-stack web development.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

---

## Language Position

```
                    Simple ←────────────────→ Complex
                       │                         │
     TypeScript ───────┼─────────────────────────┤  GC, slow
                       │                         │
            Go ────────┼─────────────────────────┤  GC, backend only
                       │                         │
           Cot ────────┼──────■                  │  ARC, Wasm-native
                       │                         │
         Swift ────────┼───────────■             │  ARC, no Wasm
                       │                         │
          Rust ────────┼─────────────────────────■  Borrow checker
```

### Comparison

| Language | Memory | Wasm Support | Full-stack | Learning Curve |
|----------|--------|--------------|------------|----------------|
| TypeScript | GC | Via tooling | Yes | Low |
| Go | GC | Poor | Backend only | Low |
| Rust | Borrow checker | Excellent | Yes | High |
| Swift | ARC | Experimental | No | Medium |
| **Cot** | **ARC** | **Native** | **Yes** | **Low-Medium** |

### The Gap Cot Fills

No existing language offers:
- Systems-language performance
- No manual memory management (ARC, not GC)
- Full-stack via Wasm (browser + server)
- Modern, approachable syntax

---

## Design Principles

### 1. Zig-Inspired Syntax, Simplified

```cot
// No semicolons
// Traits and impl blocks
// Type inference
// No allocator parameters

fn greet(name: string) string {
    return "Hello, " + name
}

struct User {
    name: string
    email: string
}

impl User {
    fn display(self) string {
        return self.name + " <" + self.email + ">"
    }
}
```

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

### 3. Wasm as Primary Target

```
Cot Source → Compiler → .wasm → Runs in browser
                              → Runs on server (via AOT or runtime)
```

Single language, single binary format, multiple deployment targets.

### 4. Full-Stack by Default

```cot
// shared/models.cot - runs on BOTH client and server
struct User {
    id: i64
    name: string
}

fn validate_email(email: string) bool {
    return email.contains("@")
}

// server/main.cot
@server
fn get_user(id: i64) User {
    return db.query("SELECT * FROM users WHERE id = ?", id)
}

// client/main.cot
@client
fn render_user(user: User) {
    dom.set_text("#name", user.name)
}
```

---

## Target Audience

Web developers who:
- Are frustrated with JavaScript's performance and type system
- Want to use a compiled language but find Rust too hard
- Want to write server + client in the same language
- Care about performance but don't want to manage memory manually

**Not for:**
- Operating system development
- Embedded systems with extreme memory constraints
- Developers who want fine-grained memory control

---

## Architecture

### Compilation Pipeline

```
Cot Source
    │
    ▼
┌─────────────────────────────────┐
│         Cot Compiler            │
│  (Written in Zig - permanent)   │
├─────────────────────────────────┤
│  Scanner → Parser → Checker     │
│  → Lowerer → IR → Wasm Codegen  │
└─────────────────────────────────┘
    │
    ▼
.wasm file
    │
    ├──────────────────┬──────────────────┐
    ▼                  ▼                  ▼
Browser            Wasm Runtime        AOT Compiler
(V8, SpiderMonkey)  (Development)     (Production)
                                          │
                                          ▼
                                    Native Binary
                                    (ELF, Mach-O)
```

### Why Zig for the Compiler?

Following the **Deno model**: Deno is written in Rust and runs JavaScript/TypeScript. It's not self-hosted, and that's fine.

Similarly, Cot's compiler is written in Zig. This is a **permanent** dependency, not a bootstrap.

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| Compiler language | Zig | Simple, fast compilation, good Wasm support |
| Self-hosting | Future goal | Focus on language quality first |
| Zig dependency | Permanent (for now) | Like Deno's Rust dependency |

### Self-Hosting Strategy

Self-hosting is a **future goal**, not a prerequisite for shipping.

**Why defer self-hosting:**
- 5 previous attempts failed due to complexity
- Self-hosting proves "compiler complexity", not "web app capability"
- Better to prove the language with real applications first

**The plan:**
1. Ship Cot with Zig compiler
2. Build real applications in Cot
3. Stabilize the language
4. Attempt self-hosting when the language is mature

**When to attempt self-hosting:**
- Language syntax is frozen
- Standard library is complete
- Multiple real applications built successfully
- Compiler architecture is stable

Self-hosting becomes the "final exam" that proves language maturity, not the first test.

### Dogfooding Strategy

Since self-hosting is deferred, we need another way to prove the language works for real applications. The strategy: **build the Cot ecosystem in Cot**.

**cot.land - Package Manager**

Like deno.land, this is the official package registry and manager:
- Server-side Cot application
- API endpoints (package publish, search, download)
- Database integration (user accounts, package metadata)
- Authentication and authorization
- Proves: server-side Cot, HTTP, database, JSON handling

**cot.dev - Documentation & Playground**

The official Cot website:
- Documentation site (static generation)
- Interactive playground (Cot running in browser via Wasm)
- Marketing and community pages
- Proves: client-side Cot, DOM manipulation, Wasm in browser

**Why this approach:**
- Web framework is the target use case (proves what we're selling)
- More practical than compiler (serves the community)
- Exercises both client and server code paths
- Provides immediate value to users

**Self-hosting remains a long-term goal** for proving full language maturity. Once cot.land and cot.dev are running in production, and the language syntax is frozen, self-hosting becomes achievable and meaningful.

---

## Execution Roadmap

### Phase 1: Wasm Backend ✅ COMPLETE

**Goal:** Cot emits valid Wasm

See **[WASM_BACKEND.md](WASM_BACKEND.md)** for implementation details.

```
├── Wasm binary format (sections, types, code)
├── IR → Wasm stack machine translation
├── Function calls and control flow
└── All 26 wasm E2E tests pass
```

### Phase 2: Runtime & Memory ✅ COMPLETE

**Goal:** ARC memory management works in Wasm

```
├── Linear memory allocator (cot_alloc)
├── ARC retain/release runtime
├── Destructor calls on release to zero
├── String concat, indexing, bounds checks
├── Array literals, append builtin
└── For-range loops (for x in arr, for i in 0..n)
```

### Phase 3: Language Features (CURRENT)

**Goal:** Reach feature parity with bootstrap-0.2 (619 test cases) and add features needed for standard library.

**See [GAP_ANALYSIS.md](GAP_ANALYSIS.md) for detailed comparison with bootstrap-0.2.**

**Phase 3 features DONE (verified on both Wasm and native):**
```
├── ✅ impl     - Methods on structs
├── ✅ enum     - Enumeration types (simple + explicit values)
├── ✅ union    - Tagged unions (without payloads)
├── ✅ type     - Type aliases
├── ✅ import   - File imports (with cycle detection)
├── ✅ extern   - External function declarations
├── ✅ switch   - Switch expressions
├── ✅ Optional types (?T, .?, ??)
├── ✅ Bitwise operators (&, |, ^, ~, <<, >>)
├── ✅ Compound assignment (+=, -=, *=, /=, %=, &=, |=, ^=)
├── ✅ Character literals ('a', '\n')
├── ✅ Builtins (@sizeOf, @alignOf, @intCast)
├── ✅ For-range loops (for x in arr, for i in 0..n)
├── ✅ ARC (retain/release, destructors, heap allocation)
├── ✅ String ops (concat, indexing, bounds checks)
└── ✅ Array literals and append
```

**Phase 3 Wave 5 DONE (verified on both Wasm and native):**
```
├── ✅ Float types (f32, f64) - arithmetic, comparison, native FPU
├── ✅ Union payloads - switch with payload capture
├── ✅ Error unions (Zig-style !T, try, catch) - error sets, error propagation
├── ✅ Function pointers - first-class, indirect calls, reassignment
├── ✅ Closures - captured variables, higher-order functions, uniform representation
├── ✅ Defer - unified cleanup stack (Swift's CleanupManager pattern)
├── ✅ ARC coverage - call→+1, copy retain, reassignment, field assign
├── ✅ Global variables - read, write, multi-function (Wasm)
├── ✅ Sized integers - i8-u64, full type system, @intCast
├── ✅ Slice syntax - arr[start:end], Go-style decomposition
├── ✅ Generics - fn(T), struct(T,U), pure monomorphization (Zig pattern)
└── ✅ ARC runtime upgrade - freelist allocator, cot_dealloc/realloc, memory.grow, deinit/destructors
```

**Phase 3 features TODO (blocking standard library):**
```
├── Dynamic lists - List(T) with push/pop/get/set (generics ready)
├── Maps/dictionaries - Map(K,V) with set/get/has/delete (generics ready)
├── String interpolation - blocks developer experience
├── Traits/Interfaces - blocks polymorphism
├── Test runner - blocks testing framework
└── ~486 test cases to port from bootstrap-0.2
```

### Phase 4: AOT Native Compiler ✅ COMPLETE

**Goal:** Wasm → Native for production performance

See **[CRANELIFT_PORT_MASTER_PLAN.md](CRANELIFT_PORT_MASTER_PLAN.md)** for details.

```
├── Wasm parser (with element section for call_indirect)
├── Wasm → CLIF IR translation
├── CLIF → MachInst lowering
├── Register allocation (regalloc2 port)
├── ARM64 and AMD64 backends
├── Output ELF/Mach-O (Cranelift 3-phase: declare/define/finish)
└── All 24 native E2E tests pass
```

### Phase 5: Standard Library

**Goal:** Useful standard library written in mature Cot

```
std/
├── core (strings, arrays, math)
├── fs (file system - server only)
├── net (HTTP, WebSocket)
├── json (serialization)
└── dom (browser API - client only)
```

**Requires:** Phase 3 complete (language must be mature first)

### Phase 6: Ecosystem

**Goal:** Make Cot usable for real projects

```
├── Package manager (cot.land)
├── Build system
├── LSP (editor support)
├── Documentation generator
└── Example applications
```

### Phase 7: Self-Hosting (Future)

**Goal:** Prove language maturity

```
├── Compiler written in Cot
├── Compiles to Wasm
├── AOT compiles to native
├── Compiler compiles itself
└── Zig dependency becomes optional
```

---

## What Success Looks Like

### Completed ✅
- Cot compiles to Wasm (Phase 1)
- ARC memory management works (Phase 2)
- Phase 3 Wave 1-4 language features (methods, enums, unions, switch, imports, extern, bitwise, optionals, etc.)
- Phase 3 Wave 5 language features (floats, closures, function pointers, error unions, defer, ARC coverage, union payloads)
- Generics with pure monomorphization (Zig pattern: lazy, deferred, deduplicated)
- ARC runtime upgrade: freelist allocator, cot_dealloc/realloc, memory.grow, deinit/destructors, @alloc/@dealloc/@realloc builtins
- AOT native compilation works (Phase 4) - 39 native E2E tests pass
- 867 tests pass, 0 failures, 0 skipped

### Current Focus (Standard Library)
- **Feature gap:** Dynamic collections (List(T), Map(K,V)), string interpolation, traits
- **Test gap:** 120 test files vs bootstrap-0.2's 619 (see [GAP_ANALYSIS.md](GAP_ANALYSIS.md))
- **Priority:** Standard library in Cot (generics + ARC runtime now complete)

### Next (Phase 5-6)
- Standard library written in Cot (requires Phase 3 complete)
- Package manager (cot.land)
- Editor support (LSP)

### Long Term (Phase 7)
- Self-hosting achieved
- Production applications in the wild
- Ecosystem of libraries

---

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Primary target | Wasm | Universal, simpler than native |
| Memory management | ARC | No GC pauses, no borrow checker |
| Compiler language | Zig | Simple, fast, good Wasm support |
| Self-hosting | Deferred | Ship first, prove later |
| Syntax base | Zig-like | Familiar, readable, minimal |
| Niche | Full-stack web | Clear use case, underserved market |

---

## Reference Material

- `DESIGN.md` in bootstrap-0.2: Full technical architecture
- `README.md`: Current project status
- `CLAUDE.md`: Instructions for AI sessions
- `REFACTOR_PLAN.md`: Detailed progress tracking

---

## Summary

Cot is a pragmatic language for web developers who want:
- Performance without complexity
- One language for browser and server
- Modern syntax without legacy baggage
- Memory safety without mental overhead

The compiler stays in Zig until the language proves itself. Self-hosting is the graduation ceremony, not the entrance exam.
