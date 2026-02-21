# Cot Self-Hosting Bootstrap Plan

## Overview

The Cot compiler becomes self-hosted by writing a Cot→Wasm compiler in Cot itself.
The bootstrap uses a pre-built Wasm binary (`cot1.wasm`) checked into the repo —
same approach as Zig's `zig1.wasm`. No C compiler needed. Wasm runtimes are everywhere.

---

## Directory Structure

```
cot/
├── compiler/              # Stage 0 — Zig bootstrap compiler (permanent)
│   ├── frontend/          #   Scanner, parser, checker, lowerer, SSA
│   ├── codegen/           #   Wasm gen, native codegen (CLIF, ARM64, x64)
│   ├── driver.zig         #   Pipeline orchestrator
│   └── main.zig           #   CLI entry point
│
├── self/                  # Stage 1 — Self-hosted compiler in Cot
│   ├── cot.json           #   { "name": "cot", "main": "main.cot" }
│   ├── main.cot           #   CLI: arg parsing, command dispatch
│   ├── token.cot          #   Token enum, keyword StringMap
│   ├── scanner.cot        #   Lexer → token stream
│   ├── ast.cot            #   AST node types
│   ├── parser.cot         #   Token stream → AST
│   ├── types.cot          #   Type registry, type equality, sizing
│   ├── checker.cot        #   AST → typed AST (type checking)
│   ├── ir.cot             #   SSA IR nodes, FuncBuilder
│   ├── lower.cot          #   Typed AST → SSA IR
│   ├── ssa.cot            #   SSA passes (rewrite, decompose, schedule, lower_wasm)
│   ├── wasm.cot           #   SSA → Wasm bytecode + module linking
│   └── constants.cot      #   Wasm opcodes, section IDs, LEB128 encoding
│
├── bootstrap/
│   ├── cot1.wasm          #   Pre-built self-hosted compiler (checked into repo)
│   └── README.md          #   How to update cot1.wasm
│
├── stdlib/                # Standard library (shared by both stages)
├── test/                  # Tests (shared)
├── docs/                  # Public docs
└── CLAUDE.md              # AI instructions
```

### What lives where

| Directory | Language | Purpose | Lifespan |
|-----------|----------|---------|----------|
| `compiler/` | Zig | Full compiler: Cot → Wasm → native | Permanent (like Deno's Rust) |
| `self/` | Cot | Wasm-only compiler: Cot → Wasm | Grows over time, eventually primary |
| `bootstrap/` | Binary | Pre-built `cot1.wasm` for bootstrapping | Updated on each self-hosted release |
| `stdlib/` | Cot | Standard library | Shared by both compilers |

### Why `self/` not `src/`

- `src/` implies "the source" — but `compiler/` is also source
- `self/` clearly means "self-hosted compiler"
- Matches Zig's convention of distinguishing stage1 vs stage2
- Coexists cleanly with `compiler/`

---

## Bootstrap Chain

### Building from source (no pre-existing Cot compiler)

```
┌─────────────────────────────────────────────────────────┐
│ Step 1: wasmtime runs pre-built compiler                │
│                                                         │
│   wasmtime bootstrap/cot1.wasm build self/main.cot      │
│          ↓                            ↓                 │
│   (cot1.wasm = Cot compiler)    (self/*.cot = source)   │
│          ↓                                              │
│   Produces: cot2.wasm                                   │
│   (freshly compiled self-hosted compiler)               │
└─────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────┐
│ Step 2: Verify reproducibility                          │
│                                                         │
│   wasmtime cot2.wasm build self/main.cot → cot3.wasm    │
│                                                         │
│   diff cot2.wasm cot3.wasm  # Must be identical         │
│   (proves the compiler can compile itself correctly)    │
└─────────────────────────────────────────────────────────┘
```

### Day-to-day development

During development, the Zig compiler is the primary tool:

```bash
# Compile self-hosted compiler using Zig bootstrap
cot build self/main.cot -o self-cot.wasm --target=wasm32

# Test self-hosted compiler
wasmtime self-cot.wasm build test/e2e/features.cot --target=wasm32

# Run self-hosted compiler's own tests
cot test self/test_scanner.cot
```

### Native binaries

The self-hosted compiler only produces `.wasm` output. For native executables:

```bash
# Option A: Use Zig bootstrap for native (current approach)
cot build myapp.cot              # Zig compiler → native binary

# Option B: Use self-hosted + wasmtime (Wasm execution)
wasmtime cot1.wasm build myapp.cot --target=wasm32
wasmtime myapp.wasm              # Run via Wasm runtime

# Option C: Future — self-hosted compiler gains native codegen
# (not in scope for initial self-hosting)
```

---

## Bootstrap Dependencies

| To build Cot from source, you need: | Version |
|--------------------------------------|---------|
| `wasmtime` (or any WASI runtime) | 14.0+ |
| That's it. | |

Compare:
- **Zig**: needs a C compiler (to compile zig-wasm2c output)
- **Rust**: needs a previous Rust compiler
- **Go**: needs a previous Go compiler (1.4+ for bootstrap)
- **Cot**: needs only a Wasm runtime — no C compiler, no previous Cot

This is the simplest bootstrap chain of any compiled language.

---

## Self-Hosted Compiler Scope

### What the self-hosted compiler does (Wasm-only path)

```
Cot Source → Scanner → Parser → Checker → IR → SSA → Wasm bytecode → .wasm file
```

This is the `compiler/frontend/` + `compiler/codegen/wasm/` path. ~19K lines of Zig.

### What stays in Zig (not ported)

| Component | Lines | Why it stays |
|-----------|-------|--------------|
| Native codegen (CLIF → ARM64/x64) | ~15K | Complex, machine-specific, not needed for Wasm output |
| Wasm→CLIF translator | ~5K | Only needed for native |
| Register allocator | ~3K | Only needed for native |
| Linker (Mach-O, ELF) | ~4K | Only needed for native |
| LSP server | ~2K | Nice-to-have, not compiler core |

The Zig bootstrap remains the "full" compiler (Wasm + native). The self-hosted compiler
is the "portable" compiler (Wasm only). Both coexist permanently.

---

## Port Order

Files ordered by dependency (each file only depends on files above it):

| Phase | File | Zig Lines | Cot Est. | Dependencies | Milestone |
|-------|------|-----------|----------|--------------|-----------|
| 1 | `constants.cot` | 829 | ~700 | none | Wasm opcode definitions |
| 2 | `token.cot` | 319 | ~300 | constants | Token types + keywords |
| 3 | `scanner.cot` | 494 | ~450 | token | **Working lexer** |
| 4 | `ast.cot` | 759 | ~700 | token | AST node types |
| 5 | `types.cot` | 668 | ~600 | ast | Type system |
| 6 | `parser.cot` | 1908 | ~1800 | scanner, ast | **Working parser** |
| 7 | `ir.cot` | 598 | ~550 | types | SSA IR definitions |
| 8 | `checker.cot` | 3155 | ~2900 | parser, types | **Type checking** |
| 9 | `lower.cot` | 6937 | ~6500 | checker, ir | AST → SSA |
| 10 | `ssa.cot` | 1627 | ~1500 | ir | SSA optimization passes |
| 11 | `wasm.cot` | 1448 | ~1300 | ssa, constants | **Wasm binary output** |
| 12 | `main.cot` | ~300 | ~300 | everything | **CLI + pipeline glue** |
| | **Total** | **~19K** | **~17.5K** | | |

### Milestones

1. **Phase 3 complete**: Self-hosted lexer. Can tokenize any `.cot` file. Testable independently.
2. **Phase 6 complete**: Self-hosted parser. Can parse any `.cot` file to AST. Compare AST output with Zig compiler.
3. **Phase 8 complete**: Self-hosted type checker. Can type-check any `.cot` file. Compare diagnostics with Zig compiler.
4. **Phase 11 complete**: Self-hosted Wasm codegen. Can compile `.cot` → `.wasm`. Compare binary output with Zig compiler.
5. **Phase 12 complete**: Self-hosted compiler can compile itself. Bootstrap verified.

### Verification at each phase

Each phase is verified by comparing output with the Zig compiler:

```bash
# Phase 3: Compare token streams
cot lex test/e2e/features.cot > tokens_zig.txt
wasmtime self-cot.wasm lex test/e2e/features.cot > tokens_self.txt
diff tokens_zig.txt tokens_self.txt

# Phase 11: Compare Wasm output
cot build test/e2e/features.cot -o zig.wasm --target=wasm32
wasmtime self-cot.wasm build test/e2e/features.cot -o self.wasm
diff zig.wasm self.wasm  # Must be byte-identical
```

---

## Updating cot1.wasm

When the self-hosted compiler changes:

```bash
# 1. Build new compiler with current bootstrap
wasmtime bootstrap/cot1.wasm build self/main.cot -o cot-new.wasm --target=wasm32

# 2. Verify it can compile itself (three-stage test)
wasmtime cot-new.wasm build self/main.cot -o cot-verify.wasm --target=wasm32
diff cot-new.wasm cot-verify.wasm  # Must match

# 3. Run full test suite through new compiler
wasmtime cot-new.wasm test test/e2e/features.cot --target=wasm32

# 4. Update bootstrap binary
cp cot-new.wasm bootstrap/cot1.wasm
git add bootstrap/cot1.wasm
git commit -m "Update bootstrap compiler"
```

### Rules for updating cot1.wasm

1. **Never update without three-stage verification** — cot1→cot2→cot3, cot2==cot3
2. **Never update if tests fail** — all test suites must pass through the new compiler
3. **Keep cot1.wasm small** — strip debug info, optimize for size
4. **Tag releases** — each cot1.wasm update gets a git tag

---

## What Changes in Cot During Port

The port is mechanical (Zig syntax → Cot syntax). Key transformations:

| Zig | Cot | Notes |
|-----|-----|-------|
| `const x: T = ...;` | `const x: T = ...` | Drop semicolons |
| `var x: T = ...;` | `var x: T = ...` | Drop semicolons |
| `const E = enum { ... };` | `const E = enum { ... }` | Already supported |
| `const S = struct { ... };` | `struct S { ... }` | Keyword form |
| `fn foo(self: *T) void` | `fn foo(self: *T) void` | Same |
| `@intCast(val)` | `@intCast(T, val)` | Cot uses explicit target type |
| `std.ArrayList(T)` | `List(T)` | Cot stdlib name |
| `std.HashMap(K,V,...)` | `Map(K,V)` | Cot stdlib name |
| `try allocator.alloc(T, n)` | `@alloc(n * @sizeOf(T))` | ARC, no explicit allocator |
| `defer allocator.free(buf)` | (automatic) | ARC handles deallocation |
| `@panic("msg")` | `@panic("msg")` | Same |

The biggest simplification: **no allocator threading**. Cot's ARC handles memory
automatically. Every `allocator` parameter, `defer free()`, and `errdefer` for
cleanup disappears. This alone removes ~20% of the Zig code.

---

## Timeline Estimate

Not estimating calendar time. Estimating in terms of what ships when:

| Deliverable | Scope | Proves |
|-------------|-------|--------|
| Self-hosted lexer | constants + token + scanner (~1.5K lines) | Cot can express lexer logic |
| Self-hosted parser | + ast + parser (~2.7K lines) | Cot can express recursive descent |
| Self-hosted checker | + types + checker (~3.5K lines) | Cot can express complex type logic |
| Self-hosted codegen | + ir + lower + ssa + wasm (~10K lines) | Cot can express compiler backend |
| Bootstrap verified | + main (~300 lines) | **Cot compiles Cot** |
| cot1.wasm shipped | Binary in repo | **Anyone can build from source with just wasmtime** |
