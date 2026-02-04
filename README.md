# Cot Compiler

A Wasm-first language for full-stack web development.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

See **[VISION.md](VISION.md)** for the complete language vision and strategy.

## Project Status (February 2026)

**This is the Cot compiler, written in Zig.** Like Deno (Rust) compiling TypeScript, this compiler is a permanent tool, not a bootstrap.

| Component | Status | Description |
|-----------|--------|-------------|
| Frontend | ✅ Done | Scanner, parser, type checker, IR lowering |
| SSA Infrastructure | ✅ Done | Values, blocks, functions, passes |
| Wasm Backend | ✅ M1-M16 Done | Constants, arithmetic, control flow, loops, calls, memory, pointers, structs, slices, strings, ARC |
| Native AOT | 🔄 Partial | Infrastructure done, but only trivial programs work (see [NATIVE_AOT_FIXES.md](NATIVE_AOT_FIXES.md)) |

**Wasm Tests: All passing** | **Native Tests: Basic only**

## Quick Start

### Compile to Wasm (Primary Target)
```bash
# Build the compiler
zig build

# Compile to WebAssembly
./zig-out/bin/cot hello.cot -o hello.wasm

# Run with wasmtime
wasmtime hello.wasm
```

### Compile to Native (AOT)
```bash
# Compile to native executable (default target)
./zig-out/bin/cot hello.cot -o hello
./hello
echo $?  # Shows return value
```

### Run Tests
```bash
# All tests
zig build test

# With debug output
COT_DEBUG=parse,codegen zig build test
```

## Architecture

```
                      ┌─────────────────────────────────────┐
                      │         Cot Source Code             │
                      └─────────────────────────────────────┘
                                       │
                                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              FRONTEND                                        │
│  Scanner → Parser → Type Checker → IR Lowerer → SSA Builder                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                       │
                    ┌──────────────────┴──────────────────┐
                    │                                      │
                    ▼                                      ▼
┌─────────────────────────────────┐    ┌─────────────────────────────────────┐
│      WASM BACKEND (Primary)     │    │       NATIVE AOT (Performance)      │
│                                 │    │                                     │
│  SSA → lower_wasm → wasm_gen    │    │  Wasm → wasm_parser → wasm_to_ssa   │
│            │                    │    │           │                         │
│            ▼                    │    │           ▼                         │
│      .wasm binary               │    │  SSA → regalloc → ARM64/AMD64       │
│                                 │    │           │                         │
│  Runs in: Browser, Node,        │    │           ▼                         │
│           Deno, wasmtime        │    │  Mach-O / ELF executable            │
└─────────────────────────────────┘    └─────────────────────────────────────┘
```

## Example

```cot
// hello.cot
fn main() int {
    let x: int = 10;
    let y: int = 5;
    return x + y * 2;  // Returns 20
}
```

```bash
# Wasm target (default)
$ ./zig-out/bin/cot hello.cot -o hello.wasm
$ wasmtime hello.wasm
$ echo $?
20
```

## Key Documents

| Document | Purpose |
|----------|---------|
| [VISION.md](VISION.md) | Language vision and strategy |
| [WASM_BACKEND.md](WASM_BACKEND.md) | Wasm backend milestones (M1-M16) |
| [CRANELIFT_PORT_MASTER_PLAN.md](CRANELIFT_PORT_MASTER_PLAN.md) | Native AOT compilation (Cranelift port) |
| [NATIVE_AOT_FIXES.md](NATIVE_AOT_FIXES.md) | **Current work: Native AOT bug fixes needed** |
| [TROUBLESHOOTING.md](TROUBLESHOOTING.md) | Debugging methodology (MUST READ before fixing bugs) |
| [TESTING.md](TESTING.md) | Testing strategy and test organization |
| [CLAUDE.md](CLAUDE.md) | AI session instructions |

## Repository Structure

```
cot/
├── compiler/
│   ├── core/              # Types, errors, target config
│   ├── frontend/          # Scanner, parser, checker, IR, lowerer
│   ├── ssa/               # SSA infrastructure
│   │   └── passes/        # schedule, layout, lower_wasm
│   ├── codegen/
│   │   ├── wasm*.zig      # Wasm backend
│   │   └── native/        # ARM64/AMD64 backends (AOT)
│   ├── driver.zig         # Compilation orchestration
│   └── main.zig           # CLI entry point
│
├── test/                  # Test cases and harnesses
│   ├── cases/             # .cot test files
│   └── browser/           # Browser test harness
│
├── audit/                 # 60+ module verification docs
└── docs/                  # Documentation source
```

## Compilation Targets

| Target | Flag | Output | Status |
|--------|------|--------|--------|
| Native | (default) | executable | ✅ Working |
| Wasm32 | `--target=wasm32` | `.wasm` | ✅ Working |

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

## Current Capabilities

### Working (Wasm target)
- ✅ Functions (parameters, return values, recursion)
- ✅ Variables (let, const)
- ✅ Arithmetic (+, -, *, /, %)
- ✅ Comparisons (==, !=, <, <=, >, >=)
- ✅ Control flow (if/else, while, break, continue)
- ✅ Structs (field access, nested)
- ✅ Pointers (address-of, dereference)
- ✅ Arrays and slices
- ✅ Strings (literals, length)
- ✅ ARC (retain/release)

### In Progress
- 🔄 Browser imports (console.log, fetch)

### Planned
- ⬜ Enums
- ⬜ Defer
- ⬜ Closures
- ⬜ Generics

## Reference Code

**bootstrap-0.2** (`../bootstrap-0.2/`) - frozen reference:
- `DESIGN.md` - Full architecture specification
- `src/codegen/` - Working native codegen
- `src/cot1/` - Self-hosted compiler in Cot

**wasmtime** (`~/learning/wasmtime/`) - Wasm runtime reference:
- `cranelift/` - Wasm → native code generation
- Shows industry patterns for Wasm→SSA→Native

## For Claude AI Sessions

See [CLAUDE.md](CLAUDE.md) for detailed instructions.
