# Cot Self-Hosting Trajectory

**Goal:** Cot compiles itself. The Zig dependency becomes bootstrap-only.

This document tracks Cot's path to self-hosting, benchmarked against Zig and other languages that achieved self-hosting. It exists to maintain focus and momentum.

---

## The Inspiration: Zig's Self-Hosting Journey

Zig is the closest parallel — a systems language that bootstrapped from C++ to self-hosted over 5 years:

| Date | Zig Version | Milestone | Time from 0.3 |
|------|-------------|-----------|---------------|
| Sep 2018 | 0.3.0 | comptime, @typeInfo, Wasm experimental | — |
| Apr 2019 | 0.4.0 | `zig cc`, SIMD, bundled libc | 7 months |
| Sep 2019 | 0.5.0 | Async redesign, WASI Tier 2 | 1 year |
| Apr 2020 | 0.6.0 | Tuples, `@as`, ZLS repo created | 1.5 years |
| Nov 2020 | 0.7.0 | ZLS 0.1.0, macOS cross-compilation | 2 years |
| Jun 2021 | 0.8.0 | **Self-hosted compiler major push begins** | 2.7 years |
| Dec 2021 | 0.9.0 | WASI Tier 1, continued self-hosting work | 3.2 years |
| **Oct 2022** | **0.10.0** | **Self-hosted compiler becomes DEFAULT** | **4 years** |
| Aug 2023 | 0.11.0 | Package manager, C++ compiler deleted | 5 years |

**Key insight:** Zig started serious self-hosting work at 0.8 and shipped it as default at 0.10. The actual compiler rewrite took **~16 months** (Jun 2021 → Oct 2022). Everything before 0.8 was building the language to the point where it *could* self-host.

### Other Languages' Self-Hosting Timelines

| Language | Bootstrap from | Time to self-host | Strategy |
|----------|---------------|-------------------|----------|
| **Go** | C | 1.5 years (Go 1.4→1.5) | Automated C→Go translator, then manual cleanup |
| **Rust** | OCaml (`rustboot`) | 11 months | Manual rewrite of core, not full compiler |
| **Inko** | Ruby | ~6 months (parser→bootstrap) | Iterative: parsed itself first, then compiled |
| **8cc** | C | 40 days | Tiny C subset, minimum viable |
| **Zig** | C++ | ~16 months (active rewrite) | Parallel development, switched default at 0.10 |

**Pattern:** Most languages self-host in **6-18 months** of focused work once the language is capable enough. The prerequisite is always the same: the language needs file I/O, collections, string handling, and enough type system maturity to express a compiler.

---

## Where Cot Is Now (0.3.2)

### Self-Hosting Infrastructure: Ready

Cot already has everything a compiler needs:

| Requirement | Status | Used by |
|-------------|--------|---------|
| File I/O | `std/fs` — readFile, writeFile, openFile | Reading source, writing output |
| String manipulation | `std/string` — 25+ functions, StringBuilder | Parsing, error messages |
| Hash maps | `Map(K,V)` with splitmix64 | Symbol tables, type registries |
| Dynamic arrays | `List(T)` with 35+ methods | AST nodes, IR instructions |
| Sets | `Set(T)` | Scope lookups, dedup |
| Binary output | `@intToPtr`, `@ptrCast`, raw memory | Wasm bytecode emission |
| Error handling | Error unions, try/catch, errdefer | Compiler diagnostics |
| Enums + tagged unions | Full support | AST node types, IR opcodes |
| Generics | Monomorphized `fn(T)(...)` | Generic data structures |
| Pattern matching | Switch on enums/unions | AST visitor, IR lowering |
| Comptime | `@typeInfo`, `inline for`, `comptime {}` | Compile-time tables |
| Closures | First-class with capture | Visitor callbacks |

### Self-Hosted Code: 5% Complete

The `self/` directory contains the beginning of the self-hosted compiler:

```
self/
  cot.json              # Project config (safe: true)
  main.cot              # Entry point (38 lines)
  frontend/
    token.cot           # Token enum + toString (425 lines)
    scanner.cot         # Full lexer (688 lines)
    source.cot          # Source positions + spans (111 lines)
    errors.cot          # Error reporter (140 lines)
    ast.cot             # AST node types (652 lines)
  total: 2,054 lines
```

**What's done:** Scanner, token, AST, parser all complete (~5,400 lines). Parser is ~2,650 lines with 85 tests covering all Cot syntax. All files use @safe mode — no `&` operators, structs passed by reference like TypeScript objects.

**What's next:** Checker (type checking, scope resolution).

---

## The Path Forward

### Phase 1: Parser (est. 3,000-4,000 lines)

The parser transforms tokens into an AST. This is the largest single component.

| Component | Zig Reference | Est. Lines |
|-----------|--------------|------------|
| Expression parsing | `parser.zig:parseExpr` | ~800 |
| Statement parsing | `parser.zig:parseStmt` | ~600 |
| Type parsing | `parser.zig:parseTypeExpr` | ~400 |
| Declaration parsing | `parser.zig:parseDecl` | ~500 |
| Pattern matching | `parser.zig:parseSwitch` | ~300 |
| Error recovery | `parser.zig:synchronize` | ~200 |
| Precedence climbing | `parser.zig:parsePrecedence` | ~400 |
| Tests | — | ~800 |

**Milestone:** `cot test self/frontend/parser.cot` passes, can parse `self/main.cot`.

### Phase 2: Type Checker (est. 3,000-4,000 lines)

| Component | Est. Lines |
|-----------|------------|
| Scope/symbol resolution | ~600 |
| Type inference | ~800 |
| Generic instantiation | ~500 |
| Error union checking | ~300 |
| Trait resolution | ~400 |
| Import resolution | ~400 |
| Tests | ~1,000 |

**Milestone:** Type-checks all `self/` files correctly.

### Phase 3: IR + SSA (est. 2,000-3,000 lines)

| Component | Est. Lines |
|-----------|------------|
| AST → IR lowering | ~800 |
| SSA construction | ~600 |
| SSA passes (decompose, schedule, layout) | ~800 |
| Tests | ~800 |

**Milestone:** Lowers checked AST to SSA IR.

### Phase 4: Wasm Codegen (est. 2,000-3,000 lines)

| Component | Est. Lines |
|-----------|------------|
| SSA → Wasm ops | ~600 |
| Wasm bytecode emission | ~800 |
| Linking (functions, memory, exports) | ~400 |
| Tests | ~400 |

**Milestone:** Produces valid `.wasm` from Cot source. **Self-hosting achieved** when this compiles `self/` itself.

### Total Estimate

| Phase | Lines | Cumulative |
|-------|-------|------------|
| Scanner + AST (done) | 2,054 | 2,054 |
| Parser | ~3,500 | ~5,500 |
| Type Checker | ~3,500 | ~9,000 |
| IR + SSA | ~2,500 | ~11,500 |
| Wasm Codegen | ~2,000 | ~13,500 |

**~13,500 lines total for MVP self-hosting.** 2,054 done. ~11,500 to go.

---

## Timeline: Cot vs Zig

| Milestone | Zig (from 0.3) | Cot (from 0.3) | Cot Speedup |
|-----------|---------------|----------------|-------------|
| LSP | +19 months (ZLS, Apr 2020) | Already done | >19 months ahead |
| Formatter | Already had at 0.3 | Already done | — |
| Async/await | +12 months (0.5) | Already done | >12 months ahead |
| Package manager | +59 months (0.11) | 0.5 (planned) | — |
| Self-hosting start | +33 months (0.8) | Now (0.3.2) | 33 months ahead |
| Self-hosting default | +49 months (0.10) | 0.10 (target) | — |

**Cot is starting self-hosting work 33 months earlier in its lifecycle than Zig did.** This is possible because:
1. LLM-assisted development compresses implementation time
2. Cot's simpler type system (no full comptime) makes the compiler easier to write
3. The @safe mode (auto-ref, implicit self, colon init) makes Cot feel like TypeScript
4. All infrastructure (collections, I/O, error handling) is already in place

---

## The Self-Hosting Payoff

### Why it matters

1. **Proves the language.** A compiler is the ultimate stress test — complex data structures, error handling, file I/O, string processing, binary output. If Cot can compile itself, it can build anything.

2. **Removes the Zig dependency.** Currently, changing Cot's compiler requires knowing Zig. Self-hosting means contributors only need to know Cot.

3. **Accelerates development.** Every improvement to Cot (better error messages, faster compilation, new features) immediately benefits the compiler itself.

4. **Credibility.** Self-hosting is the universally recognized milestone that separates "toy language" from "real language." Every major language has done it: C, Go, Rust, Zig, OCaml, Haskell.

### The bootstrap chain

```
Stage 0: Zig compiler (current)
  ↓ compiles
Stage 1: Cot compiler written in Cot (compiled by Stage 0)
  ↓ compiles
Stage 2: Cot compiler written in Cot (compiled by Stage 1)
  ↓ verify: Stage 1 output == Stage 2 output
```

When Stage 1 and Stage 2 produce identical binaries, self-hosting is verified. The Zig compiler becomes bootstrap-only (frozen, rarely touched).

---

## Tracking Progress

| Version | Date | Self-Hosting Milestone | Status |
|---------|------|----------------------|--------|
| 0.3.2 | Feb 2026 | Scanner + Token + AST in Cot (2,054 LOC) | **Done** |
| 0.4 | TBD | Parser in Cot (~5,500 LOC cumulative) | Planned |
| 0.5 | TBD | Type checker in Cot (~9,000 LOC) | Planned |
| 0.6-0.7 | TBD | IR + SSA in Cot (~11,500 LOC) | Planned |
| 0.8-0.9 | TBD | Wasm codegen in Cot (~13,500 LOC) | Planned |
| 0.10 | TBD | **Self-hosted compiler becomes default** | Goal |

---

## Velocity: 8 Weeks to 0.3

For perspective, here's what Cot achieved in its first 8 weeks:

- Complete compiler pipeline (Cot → SSA → Wasm → native ARM64/x64)
- ARC memory management with automatic cleanup
- Generics, closures, traits, error unions, tagged unions, optionals
- 31 stdlib modules (list, map, set, string, json, fs, os, time, crypto, regex, http, ...)
- LSP server with 7 features
- VS Code/Cursor extension
- 66 test files, ~1100+ tests
- CLI with 11 subcommands
- @safe mode for TypeScript-style DX
- Comptime infrastructure (@typeInfo, inline for, dead branch elimination)
- MCP server written in Cot
- Self-hosted scanner + AST (2,054 lines)

Zig took 3 years and 36 contributors to reach a comparable 0.3. The LLM-assisted development model compresses implementation dramatically. The same velocity advantage applies to the self-hosting work ahead.

**The only question is focus. This document exists to maintain it.**
