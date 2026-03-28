# Cot src/ Directory Structure

**Date:** 2026-03-28
**Status:** Design — Implementation Plan

---

## Naming Convention

- **No suffix** = Cot implementation (the target, the "real" version)
- **`-zig`** = Zig implementation (bootstrapper, ported from `compiler/`)
- **`-rs`** = Rust implementation
- **`-c`** = C/C++ wrapper (for LLVM)

Example: `libcot-zig/` is the Zig bootstrapper, `libcot/` is the hand-written Cot version.

---

## Directory Layout

```
src/
├── claude/                          Design documents
│   ├── COT_IR_ARCHITECTURE.md       Three-library architecture
│   ├── CIR_FORMAT_SPEC.md           CIR binary format (SPIR-V-style)
│   └── SRC_STRUCTURE.md             This file
│
├── include/                         C ABI headers (shared contract)
│   ├── cot.h                        libcot public API
│   ├── cir.h                        libcir public API (CIR builder)
│   └── clif.h                       libclif public API (native compile)
│
│
│ ══════════════════════════════════════════════════════════════
│  TIER 1: FRONTEND — Language-specific (Cot syntax, checker)
│ ══════════════════════════════════════════════════════════════
│
├── libcot-zig/                      Zig bootstrapper (~32K lines)
│   ├── scanner.zig                  ← compiler/frontend/scanner.zig
│   ├── parser.zig                   ← compiler/frontend/parser.zig
│   ├── ast.zig                      ← compiler/frontend/ast.zig
│   ├── checker.zig                  ← compiler/frontend/checker.zig
│   ├── types.zig                    ← compiler/frontend/types.zig
│   ├── lower.zig                    ← compiler/frontend/lower.zig
│   ├── source.zig                   ← compiler/frontend/source.zig
│   ├── errors.zig                   ← compiler/frontend/errors.zig
│   ├── target.zig                   ← compiler/frontend/target.zig
│   ├── vwt_gen.zig                  ← compiler/frontend/vwt_gen.zig
│   ├── pipeline_debug.zig           ← compiler/pipeline_debug.zig
│   ├── project.zig                  ← compiler/project.zig
│   ├── lsp/                         ← compiler/lsp/
│   │   ├── server.zig
│   │   ├── diagnostics.zig
│   │   ├── completion.zig
│   │   ├── hover.zig
│   │   ├── goto_def.zig
│   │   └── semantic_tokens.zig
│   ├── cot_api.zig                  C ABI exports (implements include/cot.h)
│   └── build.zig                    Build script
│
├── libcot/                          Cot implementation (hand-written later)
│   ├── scanner.cot
│   ├── parser.cot
│   ├── ast.cot
│   ├── checker.cot
│   ├── types.cot
│   ├── lower.cot
│   ├── source.cot
│   ├── errors.cot
│   ├── lsp/
│   │   └── ...
│   └── cot.json                     { "main": "lib.cot" }
│
│
│ ══════════════════════════════════════════════════════════════
│  TIER 2: IR — Language-agnostic (SSA, ARC, VWT, actors, Wasm)
│ ══════════════════════════════════════════════════════════════
│
├── libcir-zig/                      Zig implementation (~25K lines)
│   ├── ir/                          ← compiler/ir/
│   │   ├── func.zig                 IR function representation
│   │   ├── block.zig                IR basic block
│   │   ├── value.zig                IR values/nodes
│   │   └── mod.zig                  IR module
│   ├── ssa/                         ← compiler/ssa/
│   │   ├── func.zig                 SSA function
│   │   ├── block.zig                SSA basic block
│   │   ├── value.zig                SSA value (with op, type, args)
│   │   ├── op.zig                   SSA opcodes (350+)
│   │   └── passes/
│   │       ├── schedule.zig         Go schedule pass
│   │       ├── deadcode.zig         Dead code elimination
│   │       ├── lower_wasm.zig       Lower generic → wasm ops
│   │       ├── lower_native.zig     Lower generic → native ops
│   │       ├── decompose.zig        Multi-word value splitting
│   │       ├── rewritegeneric.zig   Generic rewriting
│   │       ├── rewritedec.zig       Decomposition rewriting
│   │       ├── copyelim.zig         Copy elimination
│   │       ├── cse.zig              Common subexpression elimination
│   │       └── async_split.zig      Async state machine splitting
│   ├── ssa_builder.zig              ← compiler/frontend/ssa_builder.zig
│   ├── wasm/                        ← compiler/codegen/wasm/
│   │   ├── gen.zig                  Wasm bytecode generation
│   │   ├── link.zig                 Wasm module linking
│   │   ├── assemble.zig             Wasm assembler
│   │   ├── prog.zig                 Wasm program builder
│   │   ├── wasm.zig                 Wasm types/constants
│   │   ├── constants.zig            Wasm opcodes
│   │   └── preprocess.zig           Wasm preprocessing
│   ├── runtime/                     ← compiler/codegen/ (runtime generators)
│   │   ├── arc.zig                  ARC runtime (Wasm module functions)
│   │   ├── wasi.zig                 WASI runtime functions
│   │   ├── print.zig                Print runtime
│   │   ├── test.zig                 Test runner runtime (Wasm)
│   │   └── bench.zig                Bench runner runtime
│   ├── cir/                         CIR binary format (NEW)
│   │   ├── serialize.zig            CIR → binary (SPIR-V encoding)
│   │   ├── deserialize.zig          Binary → CIR
│   │   ├── validate.zig             CIR validation rules
│   │   └── inspect.zig              CIR text dump (cot inspect)
│   ├── cir_api.zig                  C ABI exports (implements include/cir.h)
│   └── build.zig                    Build script
│
├── libcir/                          Cot implementation (hand-written later)
│   ├── ir/
│   ├── ssa/
│   ├── wasm/
│   ├── runtime/
│   ├── cir/
│   └── cot.json
│
│
│ ══════════════════════════════════════════════════════════════
│  TIER 3: BACKEND — Target-specific (native codegen)
│ ══════════════════════════════════════════════════════════════
│
├── libclif-zig/                     Zig Cranelift port (~68K lines)
│   ├── compile.zig                  ← compiler/codegen/native/compile.zig
│   ├── ssa_to_clif.zig              ← compiler/codegen/native/ssa_to_clif.zig
│   ├── clif/                        ← compiler/ir/clif/
│   │   ├── mod.zig                  CLIF IR types
│   │   ├── dfg.zig                  Data flow graph
│   │   └── ...
│   ├── frontend/                    ← compiler/codegen/native/frontend/
│   │   ├── mod.zig                  Function builder
│   │   └── ...
│   ├── machinst/                    ← compiler/codegen/native/machinst/
│   │   ├── vcode.zig                Virtual code representation
│   │   └── ...
│   ├── regalloc/                    ← compiler/codegen/native/regalloc/
│   │   └── ...
│   ├── isa/
│   │   ├── aarch64/                 ← compiler/codegen/native/isa/aarch64/
│   │   │   ├── lower.zig
│   │   │   ├── abi.zig
│   │   │   └── inst/
│   │   │       ├── emit.zig
│   │   │       └── mod.zig
│   │   └── x64/                     ← compiler/codegen/native/isa/x64/
│   │       ├── lower.zig
│   │       ├── abi.zig
│   │       └── inst/
│   │           ├── emit.zig
│   │           └── mod.zig
│   ├── runtime/                     ← compiler/codegen/native/ (runtime generators)
│   │   ├── arc_native.zig           ARC runtime as CLIF IR
│   │   ├── io_native.zig            I/O runtime as CLIF IR
│   │   ├── print_native.zig         Print runtime as CLIF IR
│   │   └── test_native.zig          Test runner as CLIF IR
│   ├── object/                      Object file generation
│   │   ├── macho.zig                Mach-O generation (macOS)
│   │   ├── elf.zig                  ELF generation (Linux)
│   │   └── mod.zig                  Object module abstraction
│   ├── clif_api.zig                 C ABI exports (implements include/clif.h)
│   └── build.zig
│
├── libclif-rs/                      Rust Cranelift wrapper (~5-10K lines)
│   ├── Cargo.toml
│   ├── src/
│   │   ├── lib.rs                   C ABI exports (implements include/clif.h)
│   │   ├── cir_reader.rs            Read CIR from memory or .wasm
│   │   ├── lower.rs                 CIR SSA → Cranelift CLIF
│   │   └── emit.rs                  Object file emission
│   └── build.rs
│
├── libllvm-c/                       LLVM backend wrapper (future)
│   ├── llvm_backend.c               C wrapper calling LLVM C API
│   ├── cir_to_llvm.c               CIR → LLVM IR translation
│   ├── llvm_api.c                   C ABI exports (implements include/clif.h)
│   ├── CMakeLists.txt
│   └── README.md                    Requires: llvm-dev package
│
├── libclif/                         Cot Cranelift port (ultimate endgame)
│   ├── compile.cot
│   ├── ssa_to_clif.cot
│   ├── isa/
│   │   ├── aarch64/
│   │   └── x64/
│   └── cot.json
│
│
│ ══════════════════════════════════════════════════════════════
│  CLI — Thin driver linking all three tiers
│ ══════════════════════════════════════════════════════════════
│
├── cli-zig/                         Zig CLI (initial)
│   ├── main.zig                     Entry point, arg parsing
│   ├── driver.zig                   Orchestrates libcot → libcir → libclif
│   ├── cli.zig                      Arg parsing (build, run, test, check, lsp)
│   └── build.zig
│
├── cli/                             Cot CLI (self-hosted, later)
│   ├── main.cot
│   ├── driver.cot
│   └── cot.json
│
│
│ ══════════════════════════════════════════════════════════════
│  SHARED
│ ══════════════════════════════════════════════════════════════
│
├── build.zig                        Top-level build: compiles all Zig libs + links CLI
└── README.md                        src/ overview
```

---

## C ABI Headers (include/)

The headers are the **contract** between tiers. Any implementation of a tier must satisfy its header.

### include/cot.h — Frontend API

```c
#ifndef COT_H
#define COT_H

#include <stdint.h>
#include <stdbool.h>
#include "cir.h"  // needs CirModuleRef

typedef struct CotCompiler* CotCompilerRef;
typedef struct CotDiagnostic {
    const char* file;
    uint32_t line;
    uint32_t col;
    const char* message;
    int severity;  // 0=error, 1=warning, 2=info
} CotDiagnostic;

// Lifecycle
CotCompilerRef  cot_compiler_create(void);
void            cot_compiler_destroy(CotCompilerRef compiler);

// Configuration
void            cot_set_target(CotCompilerRef c, const char* target);
void            cot_set_safe_mode(CotCompilerRef c, bool safe);
void            cot_set_test_mode(CotCompilerRef c, bool test);

// Compile source → CIR module
CirModuleRef    cot_compile(CotCompilerRef c, const char* source, const char* filename);
CirModuleRef    cot_compile_file(CotCompilerRef c, const char* path);

// Diagnostics
int             cot_diagnostic_count(CotCompilerRef c);
CotDiagnostic   cot_diagnostic_get(CotCompilerRef c, int index);
bool            cot_has_errors(CotCompilerRef c);

// Check only (no IR output)
bool            cot_check(CotCompilerRef c, const char* source, const char* filename);
bool            cot_check_file(CotCompilerRef c, const char* path);

#endif
```

### include/cir.h — IR API

See Section 3 of COT_IR_ARCHITECTURE.md for full API. Key functions:

```c
#ifndef CIR_H
#define CIR_H

#include <stdint.h>
#include <stdbool.h>

typedef struct CirModule*  CirModuleRef;
typedef struct CirFunc*    CirFuncRef;
typedef struct CirBlock*   CirBlockRef;
typedef uint32_t           CirValueRef;
typedef uint32_t           CirTypeRef;
typedef struct CirBuilder* CirBuilderRef;

// Module, function, block lifecycle
CirModuleRef  cir_module_create(const char* name);
void          cir_module_destroy(CirModuleRef mod);
CirFuncRef    cir_func_create(CirModuleRef mod, const char* name, CirTypeRef ret, bool is_async);
CirBlockRef   cir_block_create(CirFuncRef func);
CirBuilderRef cir_builder_create(CirBlockRef block);

// Build instructions (see CIR_FORMAT_SPEC.md for full opcode list)
CirValueRef   cir_build_add(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef   cir_build_retain(CirBuilderRef b, CirValueRef val);
CirValueRef   cir_build_release(CirBuilderRef b, CirValueRef val);
CirValueRef   cir_build_actor_enqueue(CirBuilderRef b, CirValueRef actor, CirValueRef job);
// ... ~80 instruction builders

// Passes and emission
void          cir_run_passes(CirModuleRef mod);
int           cir_emit_wasm(CirModuleRef mod, const char* path);
int           cir_emit_wasm_with_cir(CirModuleRef mod, const char* path);

// For in-process native compilation
const void*   cir_get_module_ptr(CirModuleRef mod);

// Validation
int           cir_validate(CirModuleRef mod, int level);  // 0=structural, 1=+arc, 2=+types, 3=all
const char*   cir_validation_error(CirModuleRef mod, int index);

#endif
```

### include/clif.h — Native Backend API

```c
#ifndef CLIF_H
#define CLIF_H

#include <stdint.h>

// In-process: compile CIR module from memory
int clif_compile(const void* cir_module,
                 const char* target,       // "arm64-macos", "x64-linux"
                 const char* output_path); // "main.o"

// From file: read .wasm with cot_ssa custom section
int clif_compile_file(const char* wasm_path,
                      const char* target,
                      const char* output_path);

// Query supported targets
int         clif_target_count(void);
const char* clif_target_name(int index);

// Version info
const char* clif_version(void);
const char* clif_backend_name(void);  // "cranelift-zig", "cranelift-rs", "llvm"

#endif
```

Any backend that implements `include/clif.h` can be linked. The CLI selects which one:

```bash
cot build main.cot --backend=cranelift   # libclif-rs or libclif-zig
cot build main.cot --backend=llvm        # libllvm-c
cot build main.cot --backend=zig         # libclif-zig (Cranelift port)
```

---

## Build Configuration

### Top-level build.zig

```zig
// Builds the complete cot compiler from selected implementations
const Backend = enum { cranelift_zig, cranelift_rs, llvm };
const Frontend = enum { zig, cot };

pub fn build(b: *std.Build) void {
    // Select implementations (configurable via -D flags)
    const backend = b.option(Backend, "backend", "Native backend") orelse .cranelift_zig;
    const frontend = b.option(Frontend, "frontend", "Frontend impl") orelse .zig;

    // Build libcot
    const libcot = switch (frontend) {
        .zig => buildZigLib(b, "libcot-zig"),
        .cot => buildCotLib(b, "libcot"),
    };

    // Build libcir (always Zig for now)
    const libcir = buildZigLib(b, "libcir-zig");

    // Build libclif
    const libclif = switch (backend) {
        .cranelift_zig => buildZigLib(b, "libclif-zig"),
        .cranelift_rs => buildRustLib(b, "libclif-rs"),
        .llvm => buildCLib(b, "libllvm-c"),
    };

    // Link CLI
    const cot = b.addExecutable(.{ .name = "cot" });
    cot.linkLibrary(libcot);
    cot.linkLibrary(libcir);
    cot.linkLibrary(libclif);
    b.installArtifact(cot);
}
```

---

## Migration Order

### Step 1: Create structure, copy files

```
compiler/frontend/     → src/libcot-zig/
compiler/ir/           → src/libcir-zig/ir/
compiler/ssa/          → src/libcir-zig/ssa/
compiler/codegen/wasm/ → src/libcir-zig/wasm/
compiler/codegen/native/ → src/libclif-zig/
compiler/lsp/          → src/libcot-zig/lsp/
```

### Step 2: Add C ABI wrappers

- `src/libcot-zig/cot_api.zig` — implements `include/cot.h`
- `src/libcir-zig/cir_api.zig` — implements `include/cir.h`
- `src/libclif-zig/clif_api.zig` — implements `include/clif.h`

### Step 3: Build and verify

All three link together via CLI. 370 tests pass. Selfcot builds.

### Step 4: Add CIR serialization

- `src/libcir-zig/cir/serialize.zig` — SPIR-V-style encoder
- `src/libcir-zig/cir/deserialize.zig` — decoder
- `src/libcir-zig/cir/validate.zig` — validation rules
- `cot inspect main.wasm` works

### Step 5: Add Rust Cranelift backend

- `src/libclif-rs/` — reads CIR, calls Cranelift crate
- Verify: same test output as Zig backend
- `cot build --backend=cranelift` selects it

### Step 6: Add LLVM backend (optional)

- `src/libllvm-c/` — reads CIR, calls LLVM C API
- `cot build --backend=llvm` selects it
- Better optimization, larger binary

### Step 7: Hand-port libcot to Cot

- `src/libcot/` — John writes the frontend in Cot
- Tests against `src/libcot-zig/` output

### Step 8: Hand-port libcir to Cot

- `src/libcir/` — John writes the IR in Cot

### Step 9: Hand-port libclif to Cot (ultimate)

- `src/libclif/` — Cranelift in Cot. Zero external dependencies.

---

## CLI Backend Selection

```bash
# Default: whatever is linked at build time
cot build main.cot -o main

# Explicit backend selection
cot build main.cot -o main --backend=cranelift-zig   # Zig Cranelift port (fast, dev)
cot build main.cot -o main --backend=cranelift-rs     # Rust Cranelift crate (fast, dev)
cot build main.cot -o main --backend=llvm             # LLVM (optimized, slow compile)

# Wasm doesn't need a backend flag (libcir handles it)
cot build main.cot -o main.wasm --target=wasm

# Emit CIR for offline native compilation
cot build main.cot --emit-ir -o main.wasm
clif-backend main.wasm -o main --target=arm64-macos

# CIR validation / inspection
cot inspect main.wasm                                  # structural validation
cot inspect main.wasm --verify-all                     # full validation
```

---

## Reference Language Audit Findings

Patterns validated and borrowed from 8 reference compiler architectures:

| Pattern | Reference | How Cot uses it |
|---------|-----------|-----------------|
| Unified IR → multiple backends | Roc (Mono IR), Kotlin (Backend IR) | libcir consumed by libclif-zig, libclif-rs, libllvm |
| Dev backend + optimized backend | Roc (gen_dev + gen_llvm) | Cranelift = fast dev builds, LLVM = optimized release |
| Serializable IR | Cranelift (postcard/serde), Kotlin (klib) | CIR binary format in Wasm custom section |
| Feature-gated backends | Cranelift, Zig, Roc | `--backend=` flag, compile-time or runtime |
| Custom linker / object gen | Roc (surgical linker), Cot (MachO/ELF gen) | No external ld dependency for basic linking |
| LSP reuses frontend only | Roc, Zig | libcot contains LSP, doesn't need libcir or libclif |
| Zig for builtins/runtime | Roc (177K lines Zig → LLVM bitcode) | Cot's runtime as CLIF IR (native) or Wasm module functions |
| Arena allocation for compilation | Roc (bumpalo) | Consider for libcir — single-phase arena for SSA values |
| Explicit ref-count as IR pass | Roc (insert_refcount_operations) | ARC retain/release as CIR instructions, optimized as a pass |
| SPIR-V-style binary SSA | Vulkan/Khronos | CIR binary format: word-aligned, Result IDs, typed instructions |

### Key insight from Roc

Roc has the closest architecture to what Cot is building. Their pipeline:
```
Parse → Canonicalize → Type Solve → Monomorphize → Insert Refcounts → Backend Codegen
```

Maps directly to Cot's:
```
libcot (Parse → Check → Lower) → libcir (SSA Build → ARC Optimize → Emit) → libclif (Native Codegen)
```

Roc's `gen_dev` backend (22K lines, fast compile, no optimization) validates having Cranelift as the "dev" backend alongside LLVM as the "release" backend. Same pattern, same rationale.

### What Roc does that Cot should consider

1. **DESIGN.md in compiler crate** — Roc has `crates/compiler/DESIGN.md` documenting the pipeline. Cot has this in `src/claude/` which is good but should be promoted to `src/DESIGN.md` when the restructure happens.

2. **Parallel module loading** — Roc parallelizes compilation across modules. Cot's actor-based concurrency could enable this naturally once concurrency is implemented.

3. **Bumpalo arena allocation** — Single arena for all compilation temporaries. Simplifies memory management in libcir. Worth considering when porting to Cot.
