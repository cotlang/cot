# Cot Compiler Pipeline

## IMPORTANT: Read This First

This document explains the compiler architecture. **There are TWO backends with separate paths from SSA onwards:**

1. **Native Backend** (default) — SSA → CLIF IR → MachInst → regalloc → ARM64/x64 → executable
2. **Wasm Backend** (`--target=wasm32`) — SSA → Wasm bytecode → `.wasm` file

**Key Rule:** The Wasm backend does NOT use register allocation. Wasm is a stack machine. The native backend does NOT go through Wasm — it translates SSA directly to CLIF IR.

---

## Directory Structure

```
compiler/
├── core/                   # Shared utilities
│   ├── target.zig          # Target detection (wasm32, arm64, amd64)
│   ├── types.zig           # Core type definitions
│   └── errors.zig          # Error types
│
├── frontend/               # Cot source → IR (used by both backends)
│   ├── scanner.zig         # Tokenizer
│   ├── parser.zig          # AST construction
│   ├── ast.zig             # AST types
│   ├── checker.zig         # Type checking
│   ├── types.zig           # Type registry
│   ├── lower.zig           # AST → IR
│   ├── ir.zig              # IR types
│   └── ssa_builder.zig     # IR → SSA
│
├── ssa/                    # SSA infrastructure (SHARED - minimal)
│   ├── op.zig              # SSA operations (generic + wasm + native)
│   ├── value.zig           # SSA values
│   ├── block.zig           # SSA blocks
│   ├── func.zig            # SSA functions
│   ├── dom.zig             # Dominator tree (shared)
│   ├── debug.zig           # Debug output (shared)
│   ├── test_helpers.zig    # Test utilities (shared)
│   └── passes/
│       ├── schedule.zig    # Value ordering (SHARED)
│       ├── layout.zig      # Block ordering (WASM BACKEND)
│       └── lower_wasm.zig  # Generic→Wasm ops (WASM BACKEND)
│
├── codegen/
│   ├── wasm/               # WASM BACKEND: SSA → Wasm bytecode
│   │   ├── wasm.zig        # Package index
│   │   ├── constants.zig   # Opcodes, registers
│   │   ├── prog.zig        # Instruction chain
│   │   ├── preprocess.zig  # High→low transform
│   │   ├── assemble.zig    # Prog→bytes
│   │   ├── link.zig        # Module assembly
│   │   └── gen.zig         # SSA codegen
│   │
│   ├── wasm.zig            # Old module builder (CodeBuilder)
│   ├── wasm_gen.zig        # SSA → Wasm (main entry point)
│   │
│   └── native/             # NATIVE BACKEND: SSA → Native (Cranelift-style)
│       │
│       ├── ssa_to_clif.zig     # SSA → CLIF IR translation (~1400 lines)
│       ├── arc_native.zig      # ARC runtime as CLIF IR (alloc, retain, release, realloc)
│       ├── io_native.zig       # I/O runtime as CLIF IR (fd_open, fd_write, fd_read, etc.)
│       ├── print_native.zig    # Print runtime as CLIF IR (print_int, print_string, etc.)
│       ├── test_native.zig     # Test runner as CLIF IR
│       │
│       ├── ir/clif/        # CLIF IR representation (from Cranelift)
│       │   ├── mod.zig         # Package index
│       │   ├── dfg.zig         # Data flow graph
│       │   ├── layout.zig      # Block/instruction layout
│       │   ├── function.zig    # Function representation
│       │   └── instructions.zig # CLIF opcodes
│       │
│       ├── frontend/       # CLIF IR construction helpers
│       │   ├── frontend.zig    # FunctionBuilder
│       │   └── ssa.zig         # SSA construction
│       │
│       ├── machinst/       # Machine instruction framework
│       │   ├── lower.zig       # CLIF → MachInst lowering
│       │   ├── vcode.zig       # Virtual code container
│       │   ├── buffer.zig      # Code buffer
│       │   ├── reg.zig         # Register types
│       │   └── blockorder.zig  # Block ordering
│       │
│       ├── regalloc/       # Register allocator (ion-based)
│       │   ├── regalloc.zig    # Main entry point
│       │   ├── liveranges.zig  # Live range computation
│       │   ├── ion.zig         # Ion allocator core
│       │   └── spill.zig       # Spill handling
│       │
│       ├── isa/aarch64/    # ARM64 backend
│       │   ├── lower.zig       # CLIF → ARM64 MachInst
│       │   ├── inst/           # ARM64 instruction types
│       │   │   ├── mod.zig     # Instruction definitions
│       │   │   ├── emit.zig    # Binary encoding
│       │   │   └── get_operands.zig # Regalloc operands
│       │   └── abi.zig         # ARM64 ABI
│       │
│       ├── isa/x64/        # AMD64 backend
│       │   ├── lower.zig       # CLIF → x64 MachInst
│       │   └── inst/           # x64 instruction types
│       │
│       ├── compile.zig         # Main compilation entry
│       ├── elf.zig             # Linux executables
│       ├── macho.zig           # macOS executables
│       └── dwarf.zig           # Debug info
│
├── driver.zig              # Orchestrates compilation
├── main.zig                # CLI entry point
└── pipeline_debug.zig      # Debug logging
```

---

## Pipeline Flows

### Native Backend (default)

```
cot build app.cot -o app

┌─────────────────────────────────────────────────────────────────┐
│  frontend/                                                      │
│  Scanner → Parser → Checker → Lowerer → IR → SSA Builder        │
└─────────────────────────────┬───────────────────────────────────┘
                              │ SSA (generic ops)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  ssa/passes/                                                    │
│  rewritegeneric → decompose → rewritedec → schedule             │
└─────────────────────────────┬───────────────────────────────────┘
                              │ SSA (lowered ops)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/ssa_to_clif.zig                                 │
│  Translate SSA ops → CLIF IR (register-based SSA)              │
│  Uses FunctionBuilder for SSA construction with block params    │
└─────────────────────────────┬───────────────────────────────────┘
                              │ CLIF IR
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/machinst/lower.zig                              │
│  Lower CLIF IR → Machine Instructions (virtual registers)       │
└─────────────────────────────┬───────────────────────────────────┘
                              │ MachInst (vregs)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/regalloc/                                       │
│  Register allocation (ion-based, from regalloc2)               │
└─────────────────────────────┬───────────────────────────────────┘
                              │ MachInst (pregs)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/isa/*/emit.zig                                  │
│  Emit native machine code bytes                                 │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
                        Native binary (ELF/Mach-O)
```

Runtime functions (ARC, I/O, print, test runner) are also compiled through this same CLIF → MachInst → emit pipeline. They are generated as CLIF IR by `arc_native.zig`, `io_native.zig`, `print_native.zig`, and `test_native.zig`, which call libc functions (e.g., `write`, `read`, `__open`, `malloc`) via undefined symbols resolved at link time by `zig cc`.

### Wasm Backend

```
cot build app.cot --target=wasm32 -o app.wasm

┌─────────────────────────────────────────────────────────────────┐
│  frontend/                                                      │
│  Scanner → Parser → Checker → Lowerer → IR → SSA Builder        │
└─────────────────────────────┬───────────────────────────────────┘
                              │ SSA (generic ops)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  ssa/passes/                                                    │
│  schedule → layout → lower_wasm                                 │
│                                                                 │
│  NO REGALLOC - Wasm is a stack machine!                        │
└─────────────────────────────┬───────────────────────────────────┘
                              │ SSA (wasm ops)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/wasm/                                                  │
│  wasm_gen → preprocess → assemble → link                       │
└─────────────────────────────┬───────────────────────────────────┘
                              │
                              ▼
                        .wasm file
```

---

## What Each Pass Does

### Native Backend Passes

| Pass | File | Purpose |
|------|------|---------|
| schedule | ssa/passes/schedule.zig | Order values within blocks for deterministic emission |
| ssa_to_clif | codegen/native/ssa_to_clif.zig | Translate SSA ops to CLIF IR (register-based SSA) |
| machinst/lower | codegen/native/machinst/lower.zig | CLIF IR → MachInst with virtual registers |
| regalloc | codegen/native/regalloc/ | Assign physical registers (ion-based) |
| emit | codegen/native/isa/*/emit.zig | MachInst → native bytes |

### Wasm Backend Passes

| Pass | File | Purpose |
|------|------|---------|
| schedule | ssa/passes/schedule.zig | Order values within blocks for deterministic emission |
| layout | ssa/passes/layout.zig | Order blocks for Wasm structured control flow |
| lower_wasm | ssa/passes/lower_wasm.zig | Convert generic ops (add, mul) to wasm ops (wasm_i64_add) |
| wasm_gen | codegen/wasm_gen.zig | Emit Wasm bytecode for each SSA value |

---

## Common Mistakes to Avoid

### Do NOT confuse the two backend paths

The native and Wasm backends are independent from SSA onwards. They share the frontend (scanner, parser, checker, lowerer, SSA builder) but diverge at code generation:
- **Native:** SSA → `ssa_to_clif.zig` → CLIF IR → machinst → regalloc → emit → .o → linker
- **Wasm:** SSA → `lower_wasm.zig` → `wasm_gen.zig` → Wasm bytecode → .wasm

### Do NOT run regalloc for Wasm targets

Wasm is a stack machine — register allocation is only for native targets.

### Do NOT mix runtime function patterns

Runtime functions have different implementations per target:
- **Wasm:** Generated as Wasm bytecode by `arc.zig`, `wasi_runtime.zig`, `test_runtime.zig`, `print_runtime.zig`
- **Native:** Generated as CLIF IR by `arc_native.zig`, `io_native.zig`, `test_native.zig`, `print_native.zig`

---

## File Ownership

| File | Belongs To | Notes |
|------|-----------|-------|
| ssa/passes/schedule.zig | SHARED | Used by both backends |
| ssa/passes/layout.zig | WASM | Block ordering for structured control flow |
| ssa/passes/lower_wasm.zig | WASM | Generic → Wasm ops |
| codegen/wasm/* | WASM | Wasm bytecode emission |
| codegen/native/ssa_to_clif.zig | NATIVE | SSA → CLIF IR translation |
| codegen/native/*_native.zig | NATIVE | Runtime functions as CLIF IR |
| codegen/native/machinst/* | NATIVE | CLIF → MachInst lowering |
| codegen/native/regalloc/* | NATIVE | Register allocation |
| codegen/native/isa/* | NATIVE | ISA-specific backends |

---

## Testing

### Native Backend Tests
```bash
zig build test                              # Compiler internals (includes native codegen tests)
cot test test/e2e/features.cot              # 341 E2E tests (native)
./test/run_all.sh                           # Full suite: 67/67 files, ~1,623 tests
```

### Wasm Backend Tests
```bash
cot test test/e2e/features.cot --target=wasm32   # 341 E2E tests (wasm, via wasmtime)
./test/run_all.sh --target=wasm32                 # Full suite via wasmtime
```

---

## Adding New Features

### Adding a new native backend feature
1. Add SSA op to `ssa/op.zig` if needed
2. Add CLIF translation in `codegen/native/ssa_to_clif.zig`
3. Ensure CLIF → MachInst lowering exists in `machinst/lower.zig` + `isa/aarch64/lower.zig`
4. Test with `cot test test/e2e/features.cot`

### Adding a new Wasm feature
1. Add op to `ssa/op.zig` if needed
2. Add lowering in `ssa/passes/lower_wasm.zig`
3. Add codegen in `codegen/wasm_gen.zig`
4. Test with `cot test test/e2e/features.cot --target=wasm32`

### Adding a new runtime function
1. **Wasm:** Add to `arc.zig` or `wasi_runtime.zig` (body + addToLinker) → `driver.zig` (`func_indices`)
2. **Native:** Add to appropriate `*_native.zig` module (CLIF IR generation) → `driver.zig` (`runtime_func_names` + `func_index_map`)
3. **Both:** Add `extern fn` declaration to `stdlib/sys.cot`
