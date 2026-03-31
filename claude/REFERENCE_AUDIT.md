# Reference Language Audit — Best-of-Breed Features for Cot Platform

**Date:** 2026-04-01
**Purpose:** Identify which features from each reference language should replace or extend Zig's implementations in the forked platform.
**Goal:** Build a world-class middle-end compiler by combining proven tech, then dogfood with TypeScript → native.

---

## Methodology

For each compiler layer, compare all reference languages and pick the best implementation. "Better than Zig" means more suitable for a full-stack compilation platform — not better for systems programming.

Reference repos audited:
- `references/zig/` — Zig (self-hosted, SoA architecture)
- `references/swift/` — Swift (ARC, concurrency, LLVM)
- `references/rust/` — Rust (multi-IR, Cranelift, borrow checker)
- `references/go/` — Go (simplicity, goroutines, GC)
- `references/kotlin/` — Kotlin (multiplatform, coroutines, null safety)
- `references/deno/` — Deno (TS runtime, V8 ops, permissions)
- `references/roc/` — Roc (ML, effects, static guarantees)

---

## Layer 1: AST / Frontend

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| AST storage | **Zig** | SoA MultiArrayList, 13 bytes/node, trivially serializable | Keep (baseline) |
| Parser design | **Zig** | Recursive descent, zero-alloc, error recovery | Keep (baseline) |
| Error messages | **Rust** | Best error messages in any compiler. Suggestions, spans, "did you mean?" | Yes — port error rendering approach |
| Incremental parsing | **Zig** | File-level incremental with InternPool | Keep (baseline) |
| Macro system | **Rust** | proc_macro + declarative macros. Most powerful metaprogramming | No — use Zig's comptime instead |
| String interpolation | **Kotlin** | `"hello ${expr}"` — cleanest syntax, type-safe | Already in Cot, matches TS |
| Null safety | **Kotlin** | `?` types pervasive, smart casts, `?.` `?:` `!!` | Already in Cot via Zig's optionals |
| Pattern matching | **Rust** | Exhaustive, nested, with guards. Most expressive | Partially in Zig, enhance with Rust-style guards |
| Destructuring | **Kotlin/TS** | Object and array destructuring | Need to add to IR |

**Verdict:** Keep Zig's AST/parser as baseline. Port Rust's error message quality. Everything else already exists or maps cleanly.

---

## Layer 2: Type System

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| Type inference | **Rust** | Hindley-Milner + trait bounds. Infers complex types | Yes — Zig's inference is limited |
| Generics | **Rust** | Monomorphization with trait bounds. Zero-cost | Yes — Zig uses comptime duck-typing |
| Trait system | **Rust** | Most powerful trait system. Default impls, associated types, where clauses | Yes — core of the type system |
| Union/sum types | **Rust** | `enum` with data. Pattern matching. Exhaustive | Zig has tagged unions, enhance |
| Structural typing | **Go/TS** | Interfaces satisfied implicitly | Yes — TS needs this |
| Type aliases | **All** | All languages have this | Keep Zig's |
| Generic constraints | **Rust** | `where T: Display + Clone` | Yes — critical for library code |
| Variance | **Kotlin** | `in`/`out` variance annotations | Nice to have, not critical |
| Type classes | **Roc** | Abilities (like Haskell type classes) | Consider for future |

**Verdict:** Port Rust's trait system and generic constraints. Add Go/TS structural typing for interface satisfaction. This is the biggest enhancement over Zig.

---

## Layer 3: Memory Management

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| ARC | **Swift** | Most mature ARC. Copy-on-write, weak refs, unowned refs | **Yes — primary memory model** |
| ARC optimization | **Swift** | Retain/release elision, guaranteed optimization passes | Yes — critical for performance |
| Tracing GC | **Go** | Sub-millisecond pause times, concurrent, production-proven | Yes — as opt-in for GC frontends |
| Borrow checker | **Rust** | Compile-time memory safety without GC | No — too complex for full-stack devs |
| Arena allocation | **Zig** | First-class arena pattern, allocator interface | Keep (baseline) |
| Copy-on-write | **Swift** | Automatic CoW for value types with heap storage | Yes — pairs with ARC |
| Move semantics | **Rust** | Zero-cost ownership transfer | Partial — use for performance-critical code |
| Cycle detection | **Swift** | Weak/unowned references break cycles | Yes — comes with ARC port |

**Verdict:** Swift ARC as default. Go GC as opt-in. Keep Zig's manual/arena as opt-in. Don't port Rust's borrow checker — it's the wrong tradeoff for the target audience.

---

## Layer 4: Concurrency

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| Structured concurrency | **Swift** | TaskGroup, async let, cancellation. Best design | **Already ported to Cot (618 tests)** |
| Actor model | **Swift** | Compile-time data race safety. Actor isolation | Already ported |
| Goroutines | **Go** | Simple, lightweight. M:N scheduling | No — Swift's model is superior |
| Coroutines | **Kotlin** | Suspend functions, flows. Well-designed | Similar to Swift, Swift is more complete |
| Async/await | **Swift** | Integrated with structured concurrency | Already ported |
| Channels | **Go** | CSP model, select statement | Nice to have, not core |
| Sendable checking | **Swift** | Compile-time verification of thread safety | Already ported |

**Verdict:** Already done. Swift concurrency is ported to Cot. Port forward to new platform.

---

## Layer 5: Error Handling

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| Error unions | **Zig** | `!T` — zero-cost, composable, try/catch sugar | Keep (baseline) |
| Result type | **Rust** | `Result<T, E>` — more explicit than Zig's | Zig's is cleaner |
| Typed throws | **Swift** | `throws(ErrorType)` — recent addition, good design | Consider enhancing Zig's model |
| Error propagation | **Zig** | `try` keyword — cleanest propagation syntax | Keep (baseline) |
| Deferred cleanup | **Zig/Go** | `defer` / `errdefer` | Keep (baseline) |
| Stack traces | **Zig** | Error return traces — unique, very useful | Keep (baseline) |

**Verdict:** Keep Zig's error handling. It's already the best design for compiled languages. Add Swift's typed throws as enhancement.

---

## Layer 6: IR / Middle-End

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| IR architecture | **Rust** | Three layers: HIR → MIR → LLVM. Each has clear purpose | Yes — multi-layer is correct |
| ZIR design | **Zig** | 390 instructions, index-based, SoA | Keep as Boundary 1 base |
| Sema design | **Zig** | 30K lines, comptime eval, incremental | Keep (baseline) |
| MIR optimizations | **Rust** | Inlining, const prop, dead code, borrow analysis | Yes — port optimization passes |
| FIR | **Kotlin** | "Frontend IR" — higher level, plugin-friendly | Study for Boundary 1 design |
| SSA construction | **Go** | Simple, fast SSA builder | Zig's approach is comparable |
| Comptime | **Zig** | Compile-time evaluation — most powerful of any language | Keep (baseline) |
| Effect system | **Roc** | Algebraic effects for controlled side effects | Future consideration |

**Verdict:** Keep Zig's ZIR/Sema/AIR stack. Port Rust's MIR-level optimization passes (inlining, const prop). Study Kotlin's FIR for plugin architecture.

---

## Layer 7: Code Generation

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| LLVM backend | **Zig/Swift/Rust** | All use LLVM for optimized builds | Keep Zig's LLVM backend |
| Cranelift backend | **Rust** | `rustc_codegen_cranelift` — fast debug builds | **Already built (rust/libclif)** |
| Self-hosted backend | **Zig** | x86_64, aarch64, wasm — no LLVM dependency | Keep (baseline) |
| Wasm backend | **Zig** | Direct Wasm emission, no LLVM | Keep (baseline) |
| WasmGC | **Kotlin** | Kotlin/Wasm uses WasmGC for objects | Yes — port for managed objects |
| Debug info (DWARF) | **Zig/Rust** | Both emit quality DWARF | Keep Zig's |
| Incremental codegen | **Rust** | Crate-level incremental | Zig has file-level, comparable |

**Verdict:** Keep all of Zig's backends. Keep Cot's Cranelift backend (CIR boundary). Port Kotlin's WasmGC approach.

---

## Layer 8: Standard Library

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| Collections | **Rust** | HashMap, BTreeMap, VecDeque — most complete | Zig's are good, enhance |
| String handling | **Go** | `strings` package — simple, comprehensive | Enhance Zig's |
| File I/O | **Go** | `os` package — cleanest API | Enhance Zig's |
| HTTP | **Go** | `net/http` — production HTTP in stdlib | Need to add |
| JSON | **Go** | `encoding/json` — struct tags, marshal/unmarshal | Need to add |
| Async I/O | **Zig** | `io_uring` / `kqueue` integration | Keep (baseline) |
| Crypto | **Go** | `crypto/` — comprehensive, audited | Need to add |
| Testing | **Zig** | Built-in `test` blocks — cleanest test integration | Keep (baseline) |
| Formatting | **Rust** | `std::fmt` — Display, Debug, custom formatters | Port trait-based formatting |

**Verdict:** Keep Zig's I/O and testing. Port Go's HTTP/JSON/crypto approach for full-stack. Port Rust's trait-based formatting.

---

## Layer 9: Tooling

| Feature | Best | Why | Port? |
|---------|------|-----|-------|
| Build system | **Zig** | `build.zig` — code as config, cross-compilation | Keep (baseline) |
| Package manager | **Go** | `go mod` — simple, reliable, reproducible | Enhance Zig's zon |
| Formatter | **Go** | `gofmt` — one canonical style, fast | Zig has one, keep |
| LSP | **Rust** | `rust-analyzer` — best LSP in any language | Port architecture, not code |
| REPL | **Kotlin** | Interactive evaluation of expressions | Need to add |
| Playground | **Go/Rust** | Web-based try-it-now | Need to build |

**Verdict:** Keep Zig's build system. Port `rust-analyzer` LSP architecture. Add REPL.

---

## Summary: What To Port From Each Language

### From Swift (highest priority)
1. **ARC runtime** — retain/release, weak/unowned, CoW optimization
2. **Concurrency** — already ported (618 tests), port forward
3. **Actor isolation** — compile-time data race safety
4. **ARC optimization passes** — retain/release elision

### From Rust (high priority)
1. **Trait system** — generic constraints, associated types, default impls
2. **Error messages** — spans, suggestions, "did you mean?"
3. **MIR optimizations** — inlining, const prop, dead code
4. **Cranelift backend** — already built (CIR boundary)

### From Go (medium priority)
1. **Tracing GC** — as opt-in memory model for GC frontends
2. **HTTP/JSON/crypto stdlib** — full-stack requires these
3. **Structural typing** — interfaces satisfied implicitly
4. **Simplicity philosophy** — keep APIs small and obvious

### From Kotlin (medium priority)
1. **WasmGC backend** — managed objects without linear memory
2. **Multiplatform architecture** — same code, multiple targets
3. **Coroutine design** — study for concurrency integration

### From Zig (keep as baseline)
1. **AST/ZIR/AIR architecture** — SoA, index-based, serializable
2. **Comptime** — compile-time evaluation
3. **Error handling** — error unions, try/catch, defer
4. **Build system** — build.zig
5. **Self-hosted backends** — x86_64, aarch64, wasm
6. **C interop** — @cImport, translate-c
7. **Incremental compilation**
8. **Cross-compilation**

### From Roc (future consideration)
1. **Effect system** — algebraic effects for controlled I/O
2. **Abilities** — Haskell-style type classes

### From Deno (reference only)
1. **Permission model** — `--allow-read`, `--allow-net`
2. **TS type stripping** — how they handle TS without type checking

---

## Implementation Priority

### Phase 1: Foundation (Fork + ARC)
1. Fork Zig, add C ABI boundaries
2. Port Swift ARC into Sema/AIR
3. Binary format for both boundaries

### Phase 2: Type System (Rust traits)
4. Port Rust trait system (constraints, associated types)
5. Add structural typing (Go/TS interfaces)
6. Enhanced type inference

### Phase 3: Full-Stack Stdlib
7. Port Go HTTP/JSON/crypto
8. Port Rust formatting traits
9. Node.js API compatibility layer

### Phase 4: TypeScript Frontend
10. Port libts to emit Boundary 1 format
11. TS → native end-to-end on new platform
12. npm package compilation

### Phase 5: Advanced Backends
13. WasmGC from Kotlin
14. Tracing GC from Go (opt-in)
15. LLVM optimization pipeline

---

## Decision Matrix

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Memory model (default) | Swift ARC | Best fit for TS semantics, zero-cost for most code |
| Memory model (opt-in) | Go GC | Enables Go/Java-style frontends |
| Type system | Rust traits + Zig comptime | Most powerful combination |
| Concurrency | Swift structured | Already proven in Cot, best design |
| Error handling | Zig error unions | Already the best, no change needed |
| AST format | Zig SoA | Best for serialization and cache performance |
| IR layers | Zig ZIR→Sema→AIR | Keep, extend with new ops |
| Native codegen (debug) | Cranelift | Fast compilation, already built |
| Native codegen (release) | LLVM | Optimal output, already in Zig |
| Wasm codegen | Zig self-hosted + WasmGC | Keep + extend |
| Build system | Zig build.zig | Already the best |
| Error messages | Rust style | Best developer experience |
| Structural typing | Go/TS style | Required for TS frontend |
| Killer app | TypeScript → native | Largest potential user base |
