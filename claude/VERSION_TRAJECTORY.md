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

## Where Cot Is Now (0.3.5)

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
| Concurrency | `spawn`, `Channel(T)`, `select`, work-stealing | Parallel compilation (future) |

### Self-Hosted Code: ~70% of `compiler/frontend/` Ported

The `self/` directory contains a partial compiler frontend in Cot — parsing, type-checking, and IR lowering. SSA builder and ARC insertion are next.

```
self/
  cot.json              # Project config (safe: true)
  main.cot              # CLI entry point — parse/check/lex/help/version (380 lines)
  frontend/
    token.cot           # Token enum + keyword lookup (443 lines) — ~100% of token.zig
    scanner.cot         # Full lexer (774 lines) — ~100% of scanner.zig
    source.cot          # Source positions + spans (315 lines) — ~95% of source.zig
    errors.cot          # Error reporter (568 lines) — ~95% of errors.zig
    ast.cot             # AST nodes, 59/59 builtins (1,468 lines) — ~100% of ast.zig
    parser.cot          # Recursive descent parser (2,940 lines) — ~75% of parser.zig
    types.cot           # TypeRegistry + type structs (1,484 lines) — ~85% of types.zig
    checker.cot         # Type checker + SharedCheckerState (5,325 lines) — ~80% of checker.zig
    ir.cot              # IR data structures + FuncBuilder (1,329 lines, 24 tests) — COMPLETE
    lower.cot           # AST→IR lowering pass (6,618 lines, 20 tests) — ~66% of lower.zig
  total: 21,264 lines, 237 tests (of ~24,130 in compiler/frontend/)
```

**What works:** The self-hosted binary can lex, parse, type-check, and lower IR for Cot source files including multi-file import resolution. 237 tests pass on native. Backend IR (ir.cot) is complete with full FuncBuilder. All 10 lower.cot parity phases are done.

**Frontend gaps (~85% of parse+check ported):**
- AST: 59/59 builtins ported
- Parser: ~75% — some constructs simplified (export fn type params, error recovery)
- Checker: ~80% — comptime block execution incomplete, closure type inference simplified, error set merging simplified

**Backend gaps (ir.cot DONE, lower.cot ~66%, SSA+ARC 0%):**
- `ir.cot` (1,329 lines, 24 tests) — **COMPLETE**
- `lower.cot` (6,618 lines, 20 tests) — **~66%** — all 10 parity phases done, async (~1,170L) not yet ported
- `ssa_builder.zig` (~2,178 lines) — SSA construction (defVar/useVar, phi nodes) — NOT STARTED
- `arc_insertion.zig` (~444 lines) — Reference counting insertion — NOT STARTED
- `formatter.zig` (~1,173 lines) — Code formatting — NOT STARTED
- `comptime.zig` (~64 lines) — Comptime value infrastructure — NOT STARTED

**What's next:** Complete lower.cot async porting, then SSA builder.

---

## The Path Forward

### Phase 1: Scanner + Token + Source + Errors — ~98% ported

| Component | Cot Lines | Zig Lines | Parity |
|-----------|-----------|-----------|--------|
| Token enum + keyword lookup | 436 | 327 | ~100% |
| Full lexer | 736 | 527 | ~100% |
| Source positions + spans | 111 | 226 | ~95% |
| Error reporter | 203 | 337 | ~95% |

**Milestone:** All source files lex correctly.

### Phase 2: AST + Parser — ~85% ported

| Component | Cot Lines | Zig Lines | Parity |
|-----------|-----------|-----------|--------|
| AST nodes | 1,468 | 644 | ~100% — 59/59 builtins ported |
| Recursive descent parser | 2,940 | 2,020 | ~75% — export fn, error recovery simplified |

**Gaps:** Some parsing constructs simplified.

**Milestone:** `cot build self/main.cot -o /tmp/selfcot` produces a native binary. Parser self-parses all 10 source files.

### Phase 3: Type Registry + Type Checker — ~82% ported

| Component | Cot Lines | Zig Lines | Parity |
|-----------|-----------|-----------|--------|
| TypeRegistry + type structs | 1,287 | 753 | ~85% |
| Type checker + SharedCheckerState | 4,112 | 4,024 | ~80% |

**Gaps:** Comptime block execution incomplete, closure type inference simplified, error set merging simplified.

**Milestone:** Multi-file type checking with import resolution works. 142 tests pass on native.

### Phase 4: IR + Lowerer + SSA — IR COMPLETE, lowerer ~66%

| Component | Zig Lines | Cot Lines | Notes |
|-----------|-----------|-----------|-------|
| IR node definitions + FuncBuilder | 606 | 1,329 | **COMPLETE** — IR opcodes, instructions, blocks, FuncBuilder (24 tests) |
| AST → IR lowering | ~9,559 | 6,618 | **~66%** — all 10 parity phases done, async not yet ported (20 tests) |
| SSA construction | ~2,178 | 0 | defVar/useVar, phi nodes |
| ARC insertion | ~444 | 0 | Reference counting, cleanup stack |
| Comptime infrastructure | ~64 | 0 | Comptime value helpers |
| **Subtotal** | **~12,851** | **7,947** | |

### Phase 5: Code Generation — 0% ported

SSA passes (`compiler/ssa/passes/`) and codegen (`compiler/codegen/`) are outside `compiler/frontend/` and not counted here. These are the final stage after IR + SSA.

### Phase 6: Formatter — 0% ported

| Component | Zig Lines | Cot Lines |
|-----------|-----------|-----------|
| Code formatter | 1,173 | 0 |

Not blocking for self-hosting, but needed for `cot fmt`.

### Total Status

| Phase | Zig Lines | Cot Lines | Status |
|-------|-----------|-----------|--------|
| Scanner + Token + Source + Errors | 1,417 | 2,100 | **~98%** |
| AST + Parser | 2,664 | 4,408 | **~85%** |
| Types + Checker | 4,777 | 6,809 | **~82%** |
| CLI (main.cot) | — | 380 | Done |
| IR + Lowerer + SSA + ARC | ~12,851 | 7,947 | **~62%** (IR complete, lowerer ~66%) |
| Formatter | ~1,173 | 0 | **0%** |
| **Total frontend** | **~24,130** | **21,264** | **~88% by lines, ~70% by parity** |

The self-hosted compiler can currently **lex, parse, type-check, and lower IR** for Cot source. 237 tests pass. It cannot yet construct SSA form or generate executable code.

---

## Timeline: Cot vs Zig

| Milestone | Zig (from 0.3) | Cot (from 0.3) | Cot Speedup |
|-----------|---------------|----------------|-------------|
| LSP | +19 months (ZLS, Apr 2020) | Already done | >19 months ahead |
| Formatter | Already had at 0.3 | Already done | — |
| Async/await | +12 months (0.5) | Already done | >12 months ahead |
| Package manager | +59 months (0.11) | 0.5 (planned) | — |
| Self-hosting start | +33 months (0.8) | Now (0.3.4) | 33 months ahead |
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
| 0.3.3 | Feb 2026 | Frontend ~47% ported (11,153 LOC) — scanner, parser, types, checker ~80% each | **Done** |
| 0.3.4 | Feb 2026 | Frontend ~65% ported (15,012 LOC, 217 tests) — ir.cot COMPLETE, pointer capture `\|*val\|` | **Done** |
| 0.3.5 | Mar 2026 | Frontend ~70% ported (21,264 LOC, 237 tests) — lower.cot ~66% (all 10 parity phases), 59/59 builtins | **Done** |
| 0.4 | TBD | Complete lower.cot (async), begin SSA builder port | Planned |
| 0.5-0.6 | TBD | SSA builder + ARC insertion ported (~2,622 lines remaining) | Planned |
| 0.7-0.9 | TBD | SSA passes + codegen, self-hosted compiler produces executables | Planned |
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
- 70 test files, ~1,658 tests
- CLI with 11 subcommands
- @safe mode for TypeScript-style DX
- Comptime infrastructure (@typeInfo, inline for, dead branch elimination)
- MCP server written in Cot
- Self-hosted frontend ~70% ported (scanner, parser, types, checker, IR, lowerer — 21,264 lines, 237 tests)
- Go-style concurrency: spawn, channels, select, work-stealing scheduler

Zig took 3 years and 36 contributors to reach a comparable 0.3. The LLM-assisted development model compresses implementation dramatically. The same velocity advantage applies to the self-hosting work ahead — the full frontend was ported in under 4 weeks.

**The only question is focus. This document exists to maintain it.**
