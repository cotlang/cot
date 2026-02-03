# Cot Compiler Pipeline

## IMPORTANT: Read This First

This document explains the compiler architecture. **There are TWO backends:**

1. **Wasm Backend** (default) - Cot source → `.wasm`
2. **Native Backend** - Cot source → Wasm → CLIF IR → Native binary (~80% complete)

**Key Rule:** The Wasm backend does NOT use register allocation. Wasm is a stack machine.

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
│   └── native/             # NATIVE BACKEND: Wasm → Native (Cranelift port)
│       │                   # ~80% complete - see CRANELIFT_PORT_MASTER_PLAN.md
│       │
│       ├── ir/clif/        # CLIF IR representation (from Cranelift)
│       │   ├── mod.zig         # Package index
│       │   ├── dfg.zig         # Data flow graph
│       │   ├── layout.zig      # Block/instruction layout
│       │   ├── function.zig    # Function representation
│       │   └── instructions.zig # CLIF opcodes
│       │
│       ├── wasm_to_clif/   # Wasm → CLIF translation
│       │   ├── translator.zig  # Main translator
│       │   └── func_translator.zig # Per-function translation
│       │
│       ├── machinst/       # Machine instruction framework
│       │   ├── lower.zig       # CLIF → MachInst lowering
│       │   ├── vcode.zig       # Virtual code container
│       │   ├── buffer.zig      # Code buffer
│       │   ├── reg.zig         # Register types
│       │   └── blockorder.zig  # Block ordering
│       │
│       ├── frontend/       # CLIF IR construction helpers
│       │   ├── frontend.zig    # FunctionBuilder
│       │   └── ssa.zig         # SSA construction
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
│       ├── wasm_parser.zig     # Parse .wasm binary
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

### Wasm Backend (default)

```
cot build app.cot -o app.wasm

┌─────────────────────────────────────────────────────────────────┐
│  frontend/                                                      │
│  Scanner → Parser → Checker → Lowerer → IR → SSA Builder        │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ SSA (generic ops)
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  ssa/passes/                                                    │
│  schedule → layout → lower_wasm                                 │
│                                                                 │
│  NO REGALLOC - Wasm is a stack machine!                        │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ SSA (wasm ops)
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/wasm/                                                  │
│  wasm_gen → preprocess → assemble → link                       │
└─────────────────────────────────┬───────────────────────────────┘
                                  │
                                  ▼
                            .wasm file
```

### Native Backend (Cranelift Architecture)

```
cot build app.cot --target=native -o app

┌─────────────────────────────────────────────────────────────────┐
│  Wasm Backend (same as above)                                   │
│  Cot source → .wasm (in memory)                                │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ .wasm bytes
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/wasm_parser.zig                                 │
│  Parse Wasm binary format                                       │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ Wasm module
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/wasm_to_clif/                                   │
│  Translate Wasm stack ops → CLIF IR (SSA form)                 │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ CLIF IR
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/machinst/lower.zig                              │
│  Lower CLIF IR → Machine Instructions (virtual registers)       │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ MachInst (vregs)
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/regalloc/                                       │
│  Register allocation (ion-based, from regalloc2)               │
└─────────────────────────────────┬───────────────────────────────┘
                                  │ MachInst (pregs)
                                  ▼
┌─────────────────────────────────────────────────────────────────┐
│  codegen/native/isa/*/emit.zig                                  │
│  Emit native machine code bytes                                 │
└─────────────────────────────────┬───────────────────────────────┘
                                  │
                                  ▼
                          Native binary (ELF/Mach-O)
```

---

## What Each Pass Does

### Wasm Backend Passes

| Pass | File | Purpose |
|------|------|---------|
| schedule | ssa/passes/schedule.zig | Order values within blocks for deterministic emission |
| layout | ssa/passes/layout.zig | Order blocks for Wasm structured control flow |
| lower_wasm | ssa/passes/lower_wasm.zig | Convert generic ops (add, mul) to wasm ops (wasm_i64_add) |
| wasm_gen | codegen/wasm_gen.zig | Emit Wasm bytecode for each SSA value |

### Native Backend Passes (Cranelift Port)

| Pass | File | Purpose |
|------|------|---------|
| wasm_parser | codegen/native/wasm_parser.zig | Parse .wasm binary format |
| wasm_to_clif | codegen/native/wasm_to_clif/ | Convert Wasm stack ops to CLIF SSA |
| machinst/lower | codegen/native/machinst/lower.zig | CLIF IR → MachInst with virtual registers |
| regalloc | codegen/native/regalloc/ | Assign physical registers (ion-based) |
| emit | codegen/native/isa/*/emit.zig | MachInst → native bytes |

---

## Common Mistakes to Avoid

### ❌ DON'T run regalloc for Wasm targets

```zig
// WRONG - regalloc is for native only!
if (target.isWasm()) {
    var regalloc_state = try regalloc(allocator, ssa_func, target);  // NO!
}
```

### ✅ DO check target before using native passes

```zig
// CORRECT
if (target.isWasm()) {
    // Wasm path: no regalloc
    try schedule.schedule(ssa_func);
    try layout.layout(ssa_func);
    try lower_wasm.lower(ssa_func);
    return generateWasmCode(ssa_func);
} else {
    // Native path: use Cranelift architecture
    return generateNativeCode(ssa_func, target);
}
```

---

## File Ownership

| File | Belongs To | Notes |
|------|-----------|-------|
| ssa/passes/schedule.zig | SHARED | Used by both backends |
| ssa/passes/layout.zig | WASM | Block ordering for structured control flow |
| ssa/passes/lower_wasm.zig | WASM | Generic → Wasm ops |
| codegen/wasm/* | WASM | Wasm bytecode emission |
| codegen/native/* | NATIVE | Cranelift-based AOT compilation |

---

## Testing

### Wasm Backend Tests
```bash
zig test compiler/codegen/wasm/wasm.zig     # Wasm package tests
zig test compiler/codegen/wasm_gen.zig      # Codegen tests
zig test compiler/ssa/passes/lower_wasm.zig # Lowering tests
```

### Native Backend Tests
```bash
zig build test  # Includes 700+ native codegen tests
```

### E2E Tests
```bash
# All tests (Wasm + Native)
zig build test

# With debug output
COT_DEBUG=codegen zig build test
```

---

## Adding New Features

### Adding a new Wasm feature
1. Add op to `ssa/op.zig` if needed
2. Add lowering in `ssa/passes/lower_wasm.zig`
3. Add codegen in `codegen/wasm_gen.zig`
4. Test with `zig test compiler/codegen/wasm_gen.zig`

### Adding native backend support
1. Check `CRANELIFT_PORT_MASTER_PLAN.md` for current status
2. Follow Cranelift's architecture exactly
3. Add instruction lowering in `isa/aarch64/lower.zig` or `isa/x64/lower.zig`
4. Add emission in `isa/*/emit.zig`
5. Run tests with `zig build test`
