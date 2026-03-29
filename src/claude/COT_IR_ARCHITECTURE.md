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
┌─────────────────────────────────────────────────────────────────────────┐
│  cot (CLI binary) — thin driver                                          │
│  Links: (libcot or libts) + libcir + (libclif or libllvm)                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  ┌────────────┐                                                          │
│  │ libcot     │ Cot frontend (.cot files)                                │
│  │ (Zig→Cot)  │ Scanner, Parser, Checker, Lowerer, LSP                   │
│  └─────┬──────┘                                                          │
│        │ C ABI (cir.h)                                                   │
│        ▼                                                                  │
│  ┌────────────┐   ┌──────────────┐   ┌──────────────────────────────┐   │
│  │ libts      │   │              │   │  Native Backends (clif.h)     │   │
│  │ (Zig)      │──→│  libcir      │──→│                               │   │
│  │            │   │  (Zig)       │   │  libclif-zig  Cranelift port  │   │
│  │ TS/JS      │   │              │   │  libclif-rs   Cranelift crate │   │
│  │ frontend   │   │  SSA Builder │   │  libllvm-zig  LLVM C API     │   │
│  │ (.ts/.js)  │   │  SSA Passes  │   │  libclif      Cot self-host  │   │
│  └─────┬──────┘   │  ARC / VWT   │   │                               │   │
│        │ C ABI    │  Concurrency │   │  --backend=cranelift (fast)   │   │
│        ▼          │  Wasm Emit   │   │  --backend=llvm (optimized)   │   │
│  ┌────────────┐   │  CIR Serial  │   └──────────────────────────────┘   │
│  │ libfoo     │   │              │                                       │
│  │ (any lang) │──→│  Language-   │   Wasm output: libcir emits directly  │
│  │ Future     │   │  agnostic    │   Native output: via clif.h backend   │
│  │ frontends  │   │              │                                       │
│  └────────────┘   └──────────────┘                                       │
└─────────────────────────────────────────────────────────────────────────┘
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

Reads CIR and produces native machine code. Four interchangeable implementations behind the same C ABI (`clif.h`):

| Implementation | Language | Lines | Strengths | Use Case |
|---------------|----------|-------|-----------|----------|
| `libclif-zig` | Zig | ~68K | Battle-tested, all tests pass, no external deps | Default today |
| `libclif-rs` | Rust | ~5-10K | Thin wrapper around Cranelift crate, free updates | Fast compile, debug builds |
| `libllvm-zig` | Zig | ~3-5K | Calls LLVM C API (`llvm-c/Core.h`), optimized codegen | Release builds (`--release`) |
| `libclif` | Cot | ~60-80K | Full self-hosting, zero external deps | Ultimate goal |

Same `clif_compile(cir_module, target, output_path)` call — the CLI selects via `--backend=cranelift|llvm`.

**LLVM backend details:**
- Zig has first-class LLVM C API access via `@cImport` — the same mechanism Zig's own compiler uses
- CIR maps almost 1:1 to LLVM IR: `cir_build_add` → `LLVMBuildAdd`, `cir_build_brif` → `LLVMBuildCondBr`
- ARC compiles to plain LLVM IR: `retain` → `atomicrmw add`, `release` → `atomicrmw sub` + branch to dealloc
- Coroutines come free via LLVM intrinsics (`llvm.coro.begin/suspend/end`) — solves native async state machines
- Full optimization pipeline: O0/O1/O2/O3, LTO, autovectorization, PGO
- Wider target coverage: ARM64, x64, RISC-V, Wasm (via LLVM Wasm backend), anything LLVM supports

**Backend strategy (same model as Zig and Rust):**
```
cot build app.cot                  # Cranelift: fast compile (~0.1s), good code
cot build app.cot --release        # LLVM: slow compile (~2s), optimized code
```

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

### TypeScript Frontend (libts)

```
cot build app.ts -o app

  libts                    libcir                     libclif / libllvm
  ┌──────────┐            ┌──────────┐              ┌──────────┐
  │ scan TS  │            │ build SSA│              │ read CIR │
  │ parse TS │─cir_build_→│ ARC opt  │─cir_module*─→│ → ARM64  │
  │ check TS │            │ schedule │              │ → .o     │
  │ lower TS │            │          │              │ → link   │
  └──────────┘            └──────────┘              └──────────┘

  TypeScript gets ARC, actors, generics, Wasm, and native for free.
  Classes → structs with ARC. Promise<T> → async Task. throw → error union.
  Node stdlib (stdlib/*.ts) maps Node APIs to Cot's existing stdlib.
  npm packages compiled from source, cached in ~/.cot/packages/.
```

### Mixed Project (.cot + .ts files)

```
cot build src/

  main.ts ──→ libts  ──→ CIR ──┐
  utils.cot ──→ libcot ──→ CIR ──┼──→ libcir ──→ libclif ──→ binary
  lib.ts ──→ libts  ──→ CIR ──┘

  Zero interop cost — all frontends emit the same CIR.
  A Cot library imported from TypeScript is just a typed import.
```

### Other Frontends (future)

```
  libfoo (any language)   libcir                     libclif
  ┌──────────┐           ┌──────────┐              ┌──────────┐
  │ parse    │─cir_build_→│ build SSA│─cir_module*─→│ → native │
  │ check    │           │ ARC opt  │              │          │
  │ lower    │           │ wasm emit│──→ .wasm     │          │
  └──────────┘           └──────────┘              └──────────┘

  Any language that calls cir_build_*() gets ARC + actors + generics
  + Wasm + native for free.
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

### libts (TypeScript Frontend) ~10-15K lines Zig (new)

| File | Purpose |
|------|---------|
| `src/libts/scanner.zig` | JS/TS lexer |
| `src/libts/parser.zig` | JS/TS parser (ESM, strict mode, TS extensions) |
| `src/libts/checker.zig` | TypeScript type checker (structural subtyping) |
| `src/libts/lower.zig` | TS AST → CIR (classes→structs, Promise→Task, throw→error union) |
| `src/libts/tsconfig.zig` | tsconfig.json loader |
| `src/libts/node_resolve.zig` | Node module resolution (node_modules, package.json) |

See **[TYPESCRIPT_NATIVE.md](TYPESCRIPT_NATIVE.md)** for the full TypeScript plan.

### libclif (Native Backend) — interchangeable implementations

| Implementation | Location | Lines | Purpose |
|---------------|----------|-------|---------|
| Cranelift Zig port | `src/libclif-zig/` | ~68K | Current default, battle-tested |
| Cranelift Rust crate | `src/libclif-rs/` | ~5-10K | Thin wrapper, free updates |
| LLVM via Zig | `src/libllvm-zig/` | ~3-5K | Optimized release builds, coroutines |
| Cranelift Cot port | `src/libclif/` | ~60-80K | Ultimate self-hosting goal |

**libllvm-zig structure:**

| File | Purpose |
|------|---------|
| `src/libllvm-zig/lower.zig` | CIR → LLVM IR translation |
| `src/libllvm-zig/optimize.zig` | LLVM pass pipeline (O0-O3, LTO) |
| `src/libllvm-zig/emit.zig` | LLVM → object file (arm64, x64, wasm, risc-v) |
| `src/libllvm-zig/link.zig` | System linker invocation |
| `src/libllvm-zig/arc.zig` | ARC intrinsics as LLVM IR |
| `src/libllvm-zig/async.zig` | Coroutines via LLVM coro intrinsics (solves Gap 9) |

Zig calls the LLVM C API (`llvm-c/Core.h`) via `@cImport` — the same mechanism Zig's own compiler uses. No FFI wrapper needed.

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

## 8. The Bigger Vision: Native TypeScript + CIR as Platform

### TypeScript Native

The first major second frontend. Compiles `.ts` and `.js` files to native binaries using the same CIR pipeline. No V8, no JIT, no garbage collector. ARC memory, static dispatch, zero runtime overhead.

```
cot build app.ts -o app          # TypeScript → native binary
cot build app.ts --target=wasm   # TypeScript → Wasm
cot run app.ts                   # Compile + run
```

Two standard libraries:
- `stdlib/*.cot` — Cot stdlib (existing, Zig-style)
- `stdlib/*.ts` — Node-compatible API surface (TypeScript syntax, calls into Cot stdlib)

npm packages: downloaded from registry, compiled from source, cached in `~/.cot/packages/`. Pure TypeScript packages work. Native addons don't.

Compliance measured against industry-standard test suites:
- **Test262** (~50K JS tests): target 95%+ on strict mode subset
- **TypeScript test suite** (~80K cases): target 90%+ conformance
- **Node.js test suite** (~5K tests): target 80%+ on implemented modules

Full plan: **[TYPESCRIPT_NATIVE.md](TYPESCRIPT_NATIVE.md)**

### CIR as a Compilation Target

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
