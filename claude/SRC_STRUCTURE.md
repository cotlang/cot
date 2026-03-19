# self/ Structure — Self-Hosted Compiler Layout

The self-hosted Cot compiler (`self/`) is organized as a pipeline.
Each folder is a stage. The folder names read as the compilation flow:

**parse → check → build → optimize → emit**

---

## Pipeline

```
self/
  main.cot

  parse/                # text → tree
    token.cot           # Token types (keywords, operators, literals)
    scanner.cot         # Lexer: source text → token stream
    parser.cot          # Parser: token stream → AST
    ast.cot             # AST node definitions
    source.cot          # Source file abstraction (positions, spans)

  check/                # tree → typed tree
    checker.cot         # Type checker: resolve types, validate semantics
    types.cot           # Type system definitions (primitives, generics, structs)
    errors.cot          # Diagnostic definitions and formatting

  build/                # typed tree → SSA
    ir.cot              # SSA IR definitions (ops, values, blocks, functions)
    lower.cot           # Checked AST → SSA translation (main lowering pass)
    builder.cot         # SSA construction helpers (block sealing, phi insertion)
    arc.cot             # ARC insertion pass (retain/release placement)

  optimize/             # SSA → better SSA
    copyelim.cot        # Copy elimination
    cse.cot             # Common subexpression elimination
    deadcode.cot        # Dead code elimination
    decompose.cot       # Composite type decomposition
    layout.cot          # Block layout optimization
    rewrite.cot         # Generic rewriting + decomposition rewrites
    schedule.cot        # Instruction scheduling

  emit/                 # SSA → binary
    wasm/               # Wasm backend
      gen.cot           # SSA → Wasm bytecode
      assemble.cot      # Bytecode assembly (instructions → binary)
      link.cot          # Wasm module linking (sections, indices, exports)
      preprocess.cot    # Pre-codegen SSA preprocessing
      types.cot         # Wasm type definitions
      constants.cot     # Constant pool management
      builder.cot       # Instruction building helpers
      prog.cot          # Program metadata (function/data indices)
      mem.cot           # Memory runtime (alloc, dealloc, realloc)
      print.cot         # Print runtime (print_int, print_string, etc.)
      wasi.cot          # WASI runtime (fd_write, fd_read, etc.)
      test.cot          # Test runner runtime
      bench.cot         # Benchmark runner runtime
      slice.cot         # Slice runtime

    native/             # Native backend (future)
      lower.cot         # SSA → Cot IR translation
      ir.cot            # Cot IR definitions (low-level, target-independent)
      machinst/         # Cot IR → VCode (virtual machine instructions)
      regalloc/         # Register allocation (virtual → physical registers)
      aarch64/          # ARM64 instruction selection + emission
      x64/              # x86-64 instruction selection + emission
      macho.cot         # Mach-O object file emitter
      elf.cot           # ELF object file emitter
      arc.cot           # ARC runtime as native code
      io.cot            # I/O runtime as native code
      print.cot         # Print runtime as native code
      test.cot          # Test runner as native code
```

---

## Design Principles

### Plain English, no jargon
Folder names are verbs a non-compiler-engineer understands: parse, check, build, optimize, emit.
Not `frontend`, `codegen`, `sema`, `ir`, `machinst`. Cot's pitch is "write like TypeScript" —
even the compiler internals should feel approachable.

### Each stage owns its data
- `parse/` defines AST — the thing it produces
- `check/` defines types — the thing it resolves
- `build/` defines SSA IR — the thing it constructs
- `emit/native/` defines Cot IR — the thing it lowers through

No shared `ir/` folder. Each stage exports what it creates; downstream stages import from upstream.
Import paths read naturally: `import build/ir` means "the IR that build produces."

### No abbreviations
`optimize/` not `opt/`. `emit/` not `codegen/`. Full words. At 2am you don't want to wonder
if `opt` means optimize, options, or optional.

### No unnecessary nesting
`aarch64/` and `x64/` sit directly under `emit/native/`, not under `emit/native/isa/`.
The folder names are self-explanatory — they don't need a parent folder to explain what they are.

### Flat where possible, nested where meaningful
Wasm runtime files (`mem.cot`, `print.cot`, `wasi.cot`) sit flat in `emit/wasm/` because
they're tightly coupled to the Wasm code generation machinery. A `runtime/` subfolder
would add nesting without clarity.

---

## Migration Path

Current `self/` → future `self/`:

| Current | Future | Notes |
|---------|--------|-------|
| `self/frontend/token.cot` | `self/parse/token.cot` | |
| `self/frontend/scanner.cot` | `self/parse/scanner.cot` | |
| `self/frontend/parser.cot` | `self/parse/parser.cot` | |
| `self/frontend/ast.cot` | `self/parse/ast.cot` | |
| `self/frontend/source.cot` | `self/parse/source.cot` | |
| `self/frontend/checker.cot` | `self/check/checker.cot` | |
| `self/frontend/types.cot` | `self/check/types.cot` | |
| `self/frontend/errors.cot` | `self/check/errors.cot` | |
| `self/frontend/ir.cot` | `self/build/ir.cot` | SSA IR definitions |
| `self/frontend/lower.cot` | `self/build/lower.cot` | 9K lines, biggest file |
| `self/frontend/ssa_builder.cot` | `self/build/builder.cot` | |
| `self/frontend/ssa.cot` | `self/build/ir.cot` | Merged with ir.cot |
| `self/frontend/arc_insertion.cot` | `self/build/arc.cot` | |
| `self/ssa/passes/copyelim.cot` | `self/optimize/copyelim.cot` | |
| `self/ssa/passes/cse.cot` | `self/optimize/cse.cot` | |
| `self/ssa/passes/deadcode.cot` | `self/optimize/deadcode.cot` | |
| `self/ssa/passes/decompose.cot` | `self/optimize/decompose.cot` | |
| `self/ssa/passes/layout.cot` | `self/optimize/layout.cot` | |
| `self/ssa/passes/rewritegeneric.cot` | `self/optimize/rewrite.cot` | Combined with rewritedec |
| `self/ssa/passes/rewritedec.cot` | `self/optimize/rewrite.cot` | Combined with rewritegeneric |
| `self/ssa/passes/schedule.cot` | `self/optimize/schedule.cot` | |
| `self/ssa/passes/lower_wasm.cot` | `self/emit/wasm/lower.cot` | Wasm-specific, belongs in emit |
| `self/codegen/wasm/wasm_gen.cot` | `self/emit/wasm/gen.cot` | |
| `self/codegen/wasm/assemble.cot` | `self/emit/wasm/assemble.cot` | |
| `self/codegen/wasm/link.cot` | `self/emit/wasm/link.cot` | |
| `self/codegen/wasm/preprocess.cot` | `self/emit/wasm/preprocess.cot` | |
| `self/codegen/wasm/wasm_types.cot` | `self/emit/wasm/types.cot` | |
| `self/codegen/wasm/constants.cot` | `self/emit/wasm/constants.cot` | |
| `self/codegen/wasm/code_builder.cot` | `self/emit/wasm/builder.cot` | |
| `self/codegen/wasm/prog.cot` | `self/emit/wasm/prog.cot` | |
| `self/codegen/wasm/mem_runtime.cot` | `self/emit/wasm/mem.cot` | |
| `self/codegen/wasm/print_runtime.cot` | `self/emit/wasm/print.cot` | |
| `self/codegen/wasm/wasi_runtime.cot` | `self/emit/wasm/wasi.cot` | |
| `self/codegen/wasm/test_runtime.cot` | `self/emit/wasm/test.cot` | |
| `self/codegen/wasm/bench_runtime.cot` | `self/emit/wasm/bench.cot` | |
| `self/codegen/wasm/slice_runtime.cot` | `self/emit/wasm/slice.cot` | |
| `self/codegen/wasm/driver.cot` | `self/emit/wasm/driver.cot` | Pipeline orchestration |
| `self/codegen/wasm/ssa_passes.cot` | `self/emit/wasm/passes.cot` | |
| `self/codegen/wasm/ssa_passes_dec.cot` | `self/emit/wasm/passes.cot` | Merged |

---

## Native Backend: Cot IR (future)

The native backend introduces a low-level IR (formerly called CLIF, renamed to Cot IR)
that sits between SSA and machine code. This is where abstract operations become
register-class-aware, calling-convention-aware instructions.

The native pipeline within `emit/native/`:
```
SSA → lower.cot → Cot IR → machinst/ → VCode → regalloc/ → aarch64/ or x64/ → binary
```

Cot IR is inspired by Cranelift's IR design but is owned by and evolved for Cot.
It is not a Cranelift compatibility layer.
