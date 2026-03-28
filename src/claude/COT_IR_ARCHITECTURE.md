# Cot Compiler Architecture: Three Libraries

**Date:** 2026-03-28
**Author:** John Cottrell / Claude
**Status:** Design Document — Target Architecture

---

## 1. Overview

The Cot compiler is split into three libraries with C ABI boundaries. Each can be developed, tested, and replaced independently.

```
libcot (frontend)  →  libcir (IR + passes)  →  libclif (native backend)
```

**Why three, not two or one:**

- **libcot can be rewritten in Cot** while libcir stays stable in Zig — enables incremental self-hosting
- **libcir can be swapped** — a GC-based IR library could replace the ARC-based one without touching the frontend or backend
- **libclif is interchangeable** — the Zig Cranelift port or the Rust Cranelift crate, same C ABI
- **Other frontends** (different syntax, different language) call libcir directly, never touch libcot
- **Frontend syntax changes** don't affect the IR or backend

---

## 2. Architecture

```
┌──────────────────────────────────────────────────────────────────┐
│  cot (CLI binary) — thin driver                                   │
│  Links: libcot + libcir + libclif                                 │
├──────────────────────────────────────────────────────────────────┤
│                                                                    │
│  ┌─────────────┐   ┌──────────────────┐   ┌───────────────────┐  │
│  │  libcot      │   │  libcir           │   │  libclif          │  │
│  │  (Zig → Cot) │   │  (Zig)            │   │  (Zig or Rust)    │  │
│  │              │   │                   │   │                   │  │
│  │  Scanner     │   │  SSA Builder      │   │  Read CIR         │  │
│  │  Parser      │   │  SSA Passes       │   │  Lower to CLIF    │  │
│  │  AST         │   │  ARC Optimize     │   │  Cranelift         │  │
│  │  Checker     │   │  VWT / Generics   │   │  ARM64 backend    │  │
│  │  Types       │   │  Concurrency      │   │  x64 backend      │  │
│  │  Lowerer     │   │  Wasm Emit        │   │  Object file      │  │
│  │  LSP         │   │  CIR Serialize    │   │  Linker invoke    │  │
│  │              │   │                   │   │                   │  │
│  │  Language-   │   │  Language-         │   │  Target-          │  │
│  │  specific    │   │  agnostic          │   │  specific         │  │
│  └──────┬───────┘   └────────┬──────────┘   └────────┬──────────┘  │
│         │  C ABI             │  C ABI                │              │
│         └──────────→─────────┘──────────────→────────┘              │
└──────────────────────────────────────────────────────────────────┘
```

### Library Responsibilities

**libcot — Frontend (language-specific)**

Knows Cot syntax, `@safe` mode, method dispatch, struct literals, string interpolation. Parses `.cot` files, type-checks, and lowers to CIR instructions via C ABI calls to libcir.

- Scanner, parser, AST
- Checker (types, Sendable, actor isolation)
- Lowerer (AST → CIR instruction calls)
- LSP (diagnostics, completions, hover, goto-def)
- First library to be rewritten in Cot (self-hosting milestone)
- Another frontend replaces this entirely — libcir doesn't care where instructions come from

**libcir — IR (language-agnostic)**

Knows ARC, VWT, actors, generics — but NOT Cot syntax. Builds SSA from C ABI calls, runs optimization passes, emits Wasm bytecode and CIR binary format. This is Cot's equivalent of Swift's SIL.

- SSA builder (phi insertion, memory threading)
- SSA passes (ARC optimization, dead code, scheduling)
- VWT dispatch, generic specialization
- Concurrency lowering (state machines, actor queues)
- Wasm bytecode emission
- CIR binary serialization (SPIR-V-style, Wasm custom section)
- C ABI for other frontends: `cir_build_retain()`, `cir_build_actor_enqueue()`, etc.
- Stays in Zig longer — complex optimization code benefits from stability

**libclif — Native Backend (target-specific)**

Reads CIR and produces native machine code. Two interchangeable implementations:

- **Zig implementation** (current): Hand-ported Cranelift (~68K lines). Battle-tested, all tests pass.
- **Rust implementation** (future option): Thin wrapper around Cranelift crate (~5-10K lines). Free updates via `cargo update`.

Same C ABI, either can be linked. The Zig port works today. The Rust version is an option when the CIR boundary is stable.

---

## 3. C ABI Boundaries

### libcot → libcir

libcot's lowerer calls these to build IR:

```c
// Module lifecycle
CirModuleRef    cir_module_create(const char* name);
void            cir_module_destroy(CirModuleRef mod);

// Functions
CirFuncRef      cir_func_create(CirModuleRef mod, const char* name,
                                CirTypeRef return_type, bool is_async);
void            cir_func_set_actor_isolated(CirFuncRef func, CirActorRef actor);

// Blocks
CirBlockRef     cir_block_create(CirFuncRef func);
void            cir_block_set_entry(CirFuncRef func, CirBlockRef block);

// Types
CirTypeRef      cir_type_i64(void);
CirTypeRef      cir_type_f64(void);
CirTypeRef      cir_type_pointer(void);
CirTypeRef      cir_type_struct(const char* name, CirTypeRef* fields, uint32_t count);
CirTypeRef      cir_type_actor(const char* name, CirTypeRef* fields, uint32_t count);

// SSA Instructions — arithmetic, control flow
CirValueRef     cir_build_add(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef     cir_build_sub(CirBuilderRef b, CirValueRef lhs, CirValueRef rhs);
CirValueRef     cir_build_load(CirBuilderRef b, CirValueRef addr, CirTypeRef type);
CirValueRef     cir_build_store(CirBuilderRef b, CirValueRef addr, CirValueRef val);
CirValueRef     cir_build_call(CirBuilderRef b, CirFuncRef callee,
                               CirValueRef* args, uint32_t num_args);
void            cir_build_ret(CirBuilderRef b, CirValueRef val);
void            cir_build_ret_void(CirBuilderRef b);
void            cir_build_br(CirBuilderRef b, CirBlockRef target);
void            cir_build_brif(CirBuilderRef b, CirValueRef cond,
                               CirBlockRef then_b, CirBlockRef else_b);

// SSA Instructions — ARC
CirValueRef     cir_build_retain(CirBuilderRef b, CirValueRef val);
CirValueRef     cir_build_release(CirBuilderRef b, CirValueRef val);
CirValueRef     cir_build_alloc(CirBuilderRef b, CirTypeRef type, uint64_t size);
CirValueRef     cir_build_is_unique(CirBuilderRef b, CirValueRef val);

// SSA Instructions — VWT / Generics
CirValueRef     cir_build_vwt_copy(CirBuilderRef b, CirValueRef metadata,
                                    CirValueRef src, CirValueRef dst);
CirValueRef     cir_build_vwt_destroy(CirBuilderRef b, CirValueRef metadata,
                                       CirValueRef val);
CirValueRef     cir_build_vwt_size(CirBuilderRef b, CirValueRef metadata);

// SSA Instructions — Concurrency
CirValueRef     cir_build_actor_enqueue(CirBuilderRef b, CirValueRef actor,
                                         CirValueRef job);
CirValueRef     cir_build_task_create(CirBuilderRef b, CirFuncRef body,
                                       CirValueRef context);
CirValueRef     cir_build_task_switch(CirBuilderRef b, CirValueRef executor);
CirValueRef     cir_build_await(CirBuilderRef b, CirValueRef future);

// Passes and emission
void            cir_run_passes(CirModuleRef mod);
int             cir_emit_wasm(CirModuleRef mod, const char* output_path);
int             cir_emit_wasm_with_cir(CirModuleRef mod, const char* output_path);
const void*     cir_get_module_ptr(CirModuleRef mod);
```

### libcir → libclif

```c
// In-process: compile CIR module to native object file
int             clif_compile(const void* cir_module,
                             const char* target,       // "arm64-macos", "x64-linux"
                             const char* output_path); // "main.o"

// From serialized format: read .wasm with cot_ssa section
int             clif_compile_file(const char* wasm_path,
                                  const char* target,
                                  const char* output_path);
```

---

## 4. Data Flow

### Native Build (in-process)

```
cot build main.cot -o main

  libcot                   libcir                     libclif
  ┌──────────┐            ┌──────────┐              ┌──────────┐
  │ scan     │            │ build SSA│              │ read CIR │
  │ parse    │─cir_build_→│ ARC opt  │─cir_module*─→│ → CLIF   │
  │ check    │            │ schedule │              │ → ARM64  │
  │ lower    │            │          │              │ → .o     │
  └──────────┘            └──────────┘              └──────────┘
```

### Wasm Build (in-process)

```
cot build main.cot -o main.wasm

  libcot                   libcir
  ┌──────────┐            ┌──────────┐
  │ scan     │            │ build SSA│
  │ parse    │─cir_build_→│ ARC opt  │──→ .wasm file
  │ check    │            │ wasm emit│
  │ lower    │            └──────────┘
  └──────────┘
  libclif not involved.
```

### Wasm + CIR Build (portable artifact)

```
cot build main.cot --emit-ir -o main.wasm

  Produces .wasm with both:
    Code section   ← runnable by browsers/wasmtime
    cot_ssa section ← readable by libclif for native compilation

  Later: cot-backend main.wasm -o main
```

### Check / LSP

```
cot check main.cot       libcot only (no IR, no codegen)
cot lsp                   libcot only
```

### Other Frontend

```
PyCot (hypothetical):

  PyCot                   libcir                     libclif
  ┌──────────┐           ┌──────────┐              ┌──────────┐
  │ parse Py │─cir_build_→│ build SSA│─cir_module*─→│ → native │
  │ check Py │           │ ARC opt  │              │          │
  │ lower Py │           │ wasm emit│──→ .wasm     │          │
  └──────────┘           └──────────┘              └──────────┘

  PyCot gets ARC, actors, generics, Wasm, and native for free.
```

---

## 5. CIR Binary Format

CIR is serialized into a standard Wasm custom section named `cot_ssa`. The format uses SPIR-V-style 32-bit word-aligned encoding for fast parsing.

Full specification: **[CIR_FORMAT_SPEC.md](CIR_FORMAT_SPEC.md)**

Key properties:
- Header: magic (`CIR\0`), version, generator, bound (max Result ID)
- Instructions: `(word_count << 16) | opcode` first word, operand words follow
- String heap: deduplicated, offset-referenced
- Types are instructions with Result IDs
- ~80 language-agnostic opcodes (ARC, VWT, actors, generics)
- Unknown opcodes skippable via word count (forward-compatible)
- Validation: structural, ARC, type, and concurrency rules

---

## 6. File Mapping: Current Zig → New Structure

### libcot (Frontend) ~32K lines

| Current File | New Location |
|-------------|-------------|
| `compiler/frontend/scanner.zig` | `src/libcot/scanner.zig` |
| `compiler/frontend/parser.zig` | `src/libcot/parser.zig` |
| `compiler/frontend/ast.zig` | `src/libcot/ast.zig` |
| `compiler/frontend/checker.zig` | `src/libcot/checker.zig` |
| `compiler/frontend/lower.zig` | `src/libcot/lower.zig` |
| `compiler/frontend/types.zig` | `src/libcot/types.zig` |
| `compiler/frontend/source.zig` | `src/libcot/source.zig` |
| `compiler/frontend/errors.zig` | `src/libcot/errors.zig` |
| `compiler/frontend/vwt_gen.zig` | `src/libcot/vwt_gen.zig` |
| `compiler/lsp/` | `src/libcot/lsp/` |

### libcir (IR + Passes + Wasm Emit) ~25K lines

| Current File | New Location |
|-------------|-------------|
| `compiler/frontend/ssa_builder.zig` | `src/libcir/ssa_builder.zig` |
| `compiler/ir/` | `src/libcir/ir/` |
| `compiler/ssa/` | `src/libcir/ssa/` |
| `compiler/codegen/wasm/` | `src/libcir/wasm/` |
| `compiler/codegen/test_runtime.zig` | `src/libcir/test_runtime.zig` |
| New: C ABI exports | `src/libcir/cir_api.zig` |
| New: CIR serializer | `src/libcir/cir_serialize.zig` |

### libclif (Native Backend) ~68K lines Zig OR ~5-10K lines Rust

| Current File | New Location |
|-------------|-------------|
| `compiler/codegen/native/` | `src/libclif/` (Zig port) |
| OR new Rust project | `src/libclif-rs/` (Cranelift crate) |

### CLI Driver ~500 lines

| Current File | New Location |
|-------------|-------------|
| `compiler/driver.zig` | `src/cot-cli/main.zig` |
| `compiler/cli.zig` | `src/cot-cli/cli.zig` |
| `compiler/main.zig` | `src/cot-cli/main.zig` |

---

## 7. Self-Hosting Path

### Phase 1: Restructure Zig into three libraries

Same code, new boundaries. Add C ABI wrappers. Verify 370 tests pass.

```
src/libcot/    ← compiler/frontend/ + compiler/lsp/
src/libcir/    ← compiler/ir/ + compiler/ssa/ + compiler/codegen/wasm/
src/libclif/   ← compiler/codegen/native/
src/cot-cli/   ← thin driver
```

### Phase 2: Freeze Zig, hand-port libcot to Cot

John hand-writes the Cot frontend in Cot, learning every compiler stage. The Zig code is the answer key.

```
src/libcot/         ← hand-written in Cot (the frontend)
src/libcir/         ← still Zig (stable, complex optimization code)
src/libclif/        ← still Zig (unchanged)
```

### Phase 3: Hand-port libcir to Cot

```
src/libcot/         ← Cot
src/libcir/         ← hand-written in Cot (SSA, passes, wasm emit)
src/libclif/        ← still Zig or switch to Rust Cranelift crate
```

### Phase 4: Full self-hosting

```
src/libcot/         ← Cot (self-hosted frontend)
src/libcir/         ← Cot (self-hosted IR)
src/libclif/        ← Zig or Rust (native backend — only non-Cot piece)
```

### Phase 5 (Ultimate): Port Cranelift to Cot

```
src/libcot/         ← Cot
src/libcir/         ← Cot
src/libclif/        ← Cot (Cranelift port — ARM64, x64 only)

Zero external dependencies. ~100K lines of Cot.
The compiler compiles itself. That's it.
```

### Bootstrap Chain

```
First time:     Zig libcot + Zig libcir + Zig libclif → compiles Cot libcot
Every release:  Cot libcot(N) + libcir + libclif → compiles Cot libcot(N+1)
Final state:    Cot libcot(N) + Cot libcir(N) + Cot libclif → compiles everything(N+1)
```

---

## 8. The Bigger Vision: CIR as a Compilation Target

Once CIR has a stable C API (libcir) and binary format (cot_ssa), it becomes a compilation target for other languages:

```
LLVM:     "Write a frontend, get optimization + codegen free"
          Commoditized backends.

Cot CIR:  "Write a frontend, get ARC + actors + generics + Wasm + native free"
          Commoditizes memory management and concurrency.
```

Any language that wants automatic reference counting, actor-based concurrency, generic dispatch, and Wasm + native from the same compilation — targets CIR instead of implementing it from scratch.

---

## 9. Business Model

| Component | License | Rationale |
|-----------|---------|-----------|
| libcot | Open source (MIT) | The language is the community |
| libcir | Open source (MIT) | The IR needs ecosystem adoption |
| libclif | Open source (MIT) | Thin wrapper around open-source backends |
| cot-cli | Open source (MIT) | Users need the compiler |
| **Cot Studio** | **Proprietary** | IDE, component library, Canvas UI, deployment |
| **Cot Cloud** | **Proprietary** | Managed Wasm deployment, edge runtime |
