# Cot Compiler

[![CI](https://github.com/cot-land/cot/actions/workflows/test.yml/badge.svg)](https://github.com/cot-land/cot/actions/workflows/test.yml)
[![Release](https://github.com/cot-land/cot/actions/workflows/release.yml/badge.svg)](https://github.com/cot-land/cot/actions/workflows/release.yml)

A Wasm-first language for full-stack web development.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

See **[VISION.md](VISION.md)** for the complete language vision and strategy.

## Installation

### Quick Install (macOS / Linux)

```sh
curl -fsSL https://raw.githubusercontent.com/cot-land/cot/main/install.sh | sh
```

### From GitHub Releases

Download the latest binary from [GitHub Releases](https://github.com/cot-land/cot/releases).

### Build from Source

Requires [Zig 0.15+](https://ziglang.org/download/).

```sh
git clone https://github.com/cot-land/cot.git
cd cot
zig build
./zig-out/bin/cot version
```

## Quick Start

```bash
# Compile and run
cot run hello.cot

# Build to native executable
cot build hello.cot              # → hello
cot build hello.cot -o myapp     # → myapp

# Build to WebAssembly
cot build --target=wasm32 hello.cot   # → hello.wasm

# Version
cot version                      # → cot 0.3.1 (arm64-macos)

# Run tests
cot test myfile.cot
```

## Example

```cot
import "std/list"

struct Point { x: i64, y: i64 }

fn distance_sq(a: *Point, b: *Point) i64 {
    var dx = b.x - a.x;
    var dy = b.y - a.y;
    return dx * dx + dy * dy;
}

fn main() i64 {
    var a = Point { .x = 0, .y = 0 };
    var b = Point { .x = 3, .y = 4 };
    println(distance_sq(&a, &b));  // Prints 25

    var scores: List(i64) = .{};
    scores.append(100);
    scores.append(200);
    return scores.get(0) + scores.get(1);  // Returns 300
}
```

## Architecture

All code goes through Wasm first. Native output is AOT-compiled from Wasm via a Cranelift-style backend.

```
Cot Source → Scanner → Parser → Checker → IR → SSA
  → lower_wasm (SSA → Wasm ops) → wasm/ (Wasm bytecode)
      ├── --target=wasm32 → .wasm file
      └── --target=native (default)
          → wasm_parser → wasm_to_clif/ → CLIF IR
          → machinst/lower → isa/{aarch64,x64}/ → emit → .o → linker → executable
```

## Language Features

- Types: `i8`–`u64`, `f32`, `f64`, `bool`, `[]u8` (strings)
- Control flow: `if`/`else`, `while`, `for`-range, `break`, `continue`
- Data: structs, arrays, slices, enums, unions (with payloads), tuples
- Functions: closures, function pointers, generics (monomorphized)
- Error handling: error unions (`E!T`), `try`, `catch`
- Memory: ARC (automatic reference counting), `defer`, `new`/`@alloc`/`@dealloc`
- Traits: `trait`/`impl Trait for Type` (monomorphized, no vtables)
- I/O: `print`, `println`, `eprint`, `eprintln` (native syscalls)
- Imports: `import "std/list"` with cross-file generic instantiation
- Stdlib: `List(T)`, `Map(K,V)`, `Set(T)`, `std/string` (~25 functions + StringBuilder), `std/math`, `std/json` (parser + encoder), `std/sort`, `std/fs`, `std/os`, `std/time`, `std/random`
- Comptime: `comptime {}` blocks, `@compileError`, const-fold if-expressions, dead branch elimination
- CLI: `cot build`, `cot run`, `cot test`, `cot version`
- Targets: Wasm32, WASI (`--target=wasm32-wasi`), ARM64 (macOS), x64 (Linux)

## Project Status

**Compiler written in Zig. Both Wasm and native AOT targets working.**

All tests passing across Wasm E2E, native E2E, and unit tests.

| Component | Status |
|-----------|--------|
| Frontend (scanner, parser, checker, lowerer) | Complete |
| SSA infrastructure + passes | Complete |
| Wasm backend (bytecode gen + linking) | Complete |
| Native AOT (Cranelift-port: CLIF IR → regalloc2 → ARM64/x64) | Complete |
| ARC runtime (retain/release, heap, destructors) | Complete |

**Next:** MCP server dogfooding, iterator protocol, `cot fmt`, `errdefer`. See [docs/ROADMAP_1_0.md](docs/ROADMAP_1_0.md).

## Design Decisions

### Why Wasm-First
1. Stack machine eliminates register allocation complexity in the primary path
2. Same binary runs in browser, server, and edge
3. AOT compilation from Wasm provides native performance when needed
4. Previous direct native codegen attempts failed 5 times — Wasm-first succeeded

### Why ARC
- Predictable performance (no GC pauses)
- Simpler than borrow checking
- Same semantics for Wasm and native targets

## Documents

| Document | Purpose |
|----------|---------|
| [VISION.md](VISION.md) | Language vision, design principles |
| [CLAUDE.md](CLAUDE.md) | AI session instructions |
| [TROUBLESHOOTING.md](TROUBLESHOOTING.md) | Debugging methodology |
| [docs/ROADMAP_1_0.md](docs/ROADMAP_1_0.md) | Road to 1.0 |
| [docs/PIPELINE_ARCHITECTURE.md](docs/PIPELINE_ARCHITECTURE.md) | Full pipeline reference map |
| [docs/BR_TABLE_ARCHITECTURE.md](docs/BR_TABLE_ARCHITECTURE.md) | br_table dispatch pattern |
| [docs/COT_SYNTAX.md](docs/COT_SYNTAX.md) | Complete language syntax reference |
| [docs/specs/WASM_3_0_REFERENCE.md](docs/specs/WASM_3_0_REFERENCE.md) | Wasm 3.0 features |
| [docs/archive/](docs/archive/) | Historical milestones and postmortems |

## Repository Structure

```
cot/
├── compiler/
│   ├── cli.zig            # CLI subcommands (build, run, test, version)
│   ├── main.zig           # Entry point, compile+link core
│   ├── driver.zig         # Pipeline orchestrator
│   ├── core/              # Types, errors, target config
│   ├── frontend/          # Scanner, parser, checker, IR, lowerer
│   ├── ssa/               # SSA infrastructure + passes
│   └── codegen/
│       ├── wasm/          # Wasm backend (gen, link, preprocess)
│       ├── print_runtime.zig  # Print/println runtime functions
│       └── native/        # AOT backend (Cranelift-style)
│           ├── wasm_to_clif/  # Wasm → CLIF translation
│           ├── machinst/      # CLIF → MachInst lowering
│           ├── isa/aarch64/   # ARM64 backend
│           ├── isa/x64/       # x64 backend
│           └── regalloc/      # Register allocator (regalloc2 port)
├── stdlib/                # Standard library (.cot files)
│   ├── list.cot           # List(T) — ~20 methods
│   ├── map.cot            # Map(K,V) — hash map with splitmix64
│   ├── set.cot            # Set(T) — thin wrapper over Map
│   ├── string.cot         # ~25 string functions + StringBuilder
│   ├── math.cot           # Integer/float math utilities
│   ├── json.cot           # JSON parser + encoder
│   ├── sort.cot           # Insertion sort for List(T)
│   ├── fs.cot             # File I/O (File struct, openFile, readFile, etc.)
│   ├── os.cot             # Process args, env, exit
│   ├── time.cot           # Timestamps, Timer struct
│   └── random.cot         # Random bytes, ints, ranges
├── runtime/               # Native runtime (.o files)
├── test/cases/            # .cot test files
├── VERSION                # Semantic version (single source of truth)
└── docs/                  # Architecture docs, specs, roadmap
```

## For AI Sessions

See [CLAUDE.md](CLAUDE.md) for compiler instructions and reference implementation locations.
