# Cot Compiler

A Wasm-first language for full-stack web development.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

See **[VISION.md](VISION.md)** for the complete language vision and strategy.

## Project Status (January 2026)

**This is the Cot compiler, written in Zig.** Like Deno (Rust) compiling TypeScript, this compiler is a permanent tool, not a bootstrap.

| Component | Status | Description |
|-----------|--------|-------------|
| Frontend | ✅ Done | Scanner, parser, type checker, IR lowering |
| SSA Infrastructure | ✅ Done | Values, blocks, functions, passes |
| Wasm Backend | ✅ M1-M9 Done | Constants, arithmetic, control flow, loops, function calls, CLI |
| Wasm Backend | 🔄 M10 Next | Linear memory (load/store) - enables pointers, structs |
| AOT Native | ✅ Ported | ARM64/AMD64 codegen refactored (~20% reduction) |
| AOT Native | 🔄 Phase 4 Next | Wire into driver, enable native binary output |

**Tests: 376/398 passed, 22 skipped (native)**

## Architecture

```
Cot Source → Frontend → IR → Wasm Codegen → .wasm file
                                   ↓
                              [M1-M9 DONE]
                              - Constants, arithmetic
                              - Control flow (if/else, loops)
                              - Function calls
                              - CLI: cot --target=wasm32 file.cot

                              [M10+ TODO]
                              - Linear memory
                              - Pointers, structs, arrays
                              - Strings, ARC

AOT Path (future):
.wasm file → wasm_parser → wasm_to_ssa → SSA → regalloc → Native → ELF/Mach-O
                                          ↓
                                    [Ported, needs wiring]
```

## Key Documents

| Document | Purpose |
|----------|---------|
| [WASM_BACKEND.md](WASM_BACKEND.md) | Wasm backend milestones (M1-M16) |
| [AOT_EXECUTION_PLAN.md](AOT_EXECUTION_PLAN.md) | Native codegen phases |
| [VISION.md](VISION.md) | Language vision and strategy |
| [CLAUDE.md](CLAUDE.md) | AI session instructions |

## Quick Start

```bash
# Run all tests
zig build test

# Compile to Wasm
zig build
./zig-out/bin/cot --target=wasm32 examples/hello.cot -o hello.wasm

# Debug output
COT_DEBUG=parse,lower,codegen zig build test
```

## Repository Structure

```
cot/
├── compiler/
│   ├── core/              # Types, errors, target config
│   ├── frontend/          # Scanner, parser, checker, IR, lowerer
│   ├── ssa/               # SSA infrastructure
│   │   └── passes/        # schedule, layout, lower_wasm
│   ├── codegen/
│   │   ├── wasm*.zig      # Wasm backend (active development)
│   │   └── native/        # ARM64/AMD64 backends (AOT)
│   ├── driver.zig         # Compilation orchestration
│   └── main.zig           # CLI entry point
│
├── audit/                 # 60+ module verification docs
├── docs/                  # Documentation source
└── runtime/               # Wasm runtime support (future)
```

## Next Steps

### Wasm Backend (Primary)

See [WASM_BACKEND.md](WASM_BACKEND.md) for full details.

**M10: Linear Memory** (current priority)
- Add memory section to Wasm module
- Implement `wasm_i64_load`, `wasm_i64_store`
- Enables: local variables beyond registers, pointers

**M11-M16:** Pointers, structs, arrays, strings, ARC, browser imports

### AOT Native (Secondary)

See [AOT_EXECUTION_PLAN.md](AOT_EXECUTION_PLAN.md) for full details.

**Phase 4: Integration**
- Wire ARM64/AMD64 codegen into driver.zig
- Un-skip 22 native tests
- Enable: `cot --target=arm64-macos` or `--target=amd64-linux`

## Design Decisions

### Why Wasm as Primary Target

1. **Simpler compiler**: Stack machine eliminates register allocation
2. **Universal target**: Same binary runs in browser, server, edge
3. **Self-hosting achievable**: Previous native codegen attempts failed due to complexity
4. **AOT for performance**: Wasm → Native when needed

### Why ARC Memory Management

- Predictable (no GC pauses)
- Simpler than borrow checking
- Same semantics for Wasm and native targets

## Reference Code

**bootstrap-0.2** (`../bootstrap-0.2/`) - frozen reference:
- `DESIGN.md` - Full architecture specification
- `src/codegen/` - Working native codegen
- `src/cot1/` - Self-hosted compiler in Cot

## For Claude AI Sessions

See [CLAUDE.md](CLAUDE.md) for detailed instructions.
