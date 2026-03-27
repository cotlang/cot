# Cot IR Architecture: The Evolution from Monolith to Pluggable Compiler

**Date:** 2026-03-27
**Author:** John Cottrell / Claude
**Status:** Design Document — Future Architecture

---

## 1. How This Design Evolved

This document traces the evolution of an idea through a single conversation, from an initial question about compiler modularity to a concrete three-library architecture.

### Starting Point: "Best of Breed"

Cot's compiler is assembled from proven reference implementations:

| Component | Reference | Why |
|-----------|-----------|-----|
| Syntax | Zig | Clean systems-level syntax |
| DX sugar | TypeScript | Familiar feel (@safe mode, string interpolation) |
| impl/traits | Rust | Method dispatch, trait system |
| ARC + VWT | Swift | Non-GC memory management, value witnesses |
| Generics | Swift | Shared bodies + @inlinable monomorphization |
| Concurrency | Swift | Actors, async/await, Sendable |
| Native codegen | Cranelift (Rust/Wasmtime) | Fast compile, direct SSA → native |
| WasmGC | Kotlin/Wasm | GC struct/array types for browser |
| Parser/SSA | Go | Recursive descent, phi insertion, schedule pass |

The question arose: could each of these be a swappable library?

### The Pluggable Compiler Idea

The initial concept was radical — every compiler phase as a C ABI library:

```
Scanner library → Parser library → Type System library → Memory library → ...
```

With abstract interfaces (traits) for each concern:

```
trait MemoryStrategy {
    fn needsTracking(type) bool
    fn onValueCopied(value) → IR ops
    fn onScopeExit(live_values) → IR ops
}

// ARC implements MemoryStrategy
// GC implements MemoryStrategy
// Rust-style Ownership implements MemoryStrategy
```

This would allow switching ARC for GC, actors for goroutines, monomorphization for type erasure — all through C ABI plugin boundaries.

### Why Full Pluggability Is Impractical (For Now)

Cross-cutting concerns make full separation extremely hard:

- **ARC touches everything:** Checker (track ARC fields) → Lowerer (insert retain/release) → SSA passes (eliminate redundant pairs) → Codegen (emit calls)
- **Generics touch everything:** Checker (type params) → Lowerer (VWT/metadata) → Codegen (indirect calls)
- **Concurrency touches everything:** Checker (isolation) → Lowerer (state machines) → Runtime (executor)

The interactions between strategies compound: ARC + Actors (deinit on executor), Generics + ARC (VWT witnesses call retain/release), Concurrency + Generics (Sendable checking needs generic strategy).

**Conclusion:** Full pluggability is a research project. But the right boundaries DO exist.

### The LLVM Insight

LLVM proved that ONE boundary — frontend → IR → backend — is enough to create an ecosystem. Clang, Rust, Swift, Zig, Julia all share LLVM's backend through a common IR.

But LLVM IR is too low-level. It doesn't understand ARC, actors, or generics. Swift needed an extra layer (SIL) because LLVM couldn't optimize retain/release pairs — by the time they're lowered to `call @swift_retain`, LLVM sees opaque function calls.

**Cot's opportunity:** Create an IR that sits ABOVE LLVM's level — one that understands ARC, actors, VWT, and generics natively. This is what Swift's SIL does, but SIL was never designed as a public compilation target.

### The Wasm Custom Section Idea

A key realization: the serialized IR format could be embedded in a standard Wasm binary as a custom section. The same `.wasm` file is:

1. **Runnable by any Wasm runtime** (browsers, wasmtime) — they read the Code section
2. **Compilable to optimized native** by the Cot backend — it reads the cot_ssa custom section
3. **Inspectable by standard tools** (wasm-tools, wasm-objdump)

```
.wasm file layout:
  [ Standard Wasm sections ]     ← browsers/wasmtime execute this
  [ "cot_ssa" custom section ]   ← native backend reads this (SSA form)
  [ "cot_types" custom section ] ← type metadata, VWT info, actor annotations
```

The SSA section contains the full register-based IR with ARC instructions, actor operations, and generic dispatch — everything the native backend needs to produce optimized code without reconstructing it from stack-based Wasm bytecode.

Three build modes from the same compiler:

```bash
cot build main.cot -o main.wasm                # both sections (default)
cot build main.cot -o main.wasm --target=wasm   # strip cot_ssa (smaller)
cot build main.cot -o main.wasm --target=native  # strip Code section
```

### From Wasm-to-Native (Deleted) to CIR-to-Native

Cot previously had a wasm-to-native path that tried to reconstruct SSA from Wasm stack bytecode. It was fragile and buggy — information was lost during Wasm emission and imperfectly recovered.

The CIR approach fixes this: both the Wasm Code section and the cot_ssa section are generated from the same SSA simultaneously. No reconstruction, no information loss.

```
Old (deleted):  SSA → Wasm stack ops → parse → reconstruct SSA → native
New (proposed): SSA → Wasm stack ops (Code section) + SSA directly (custom section)
```

### The Final Architecture: Three Libraries

The design converged on three libraries with clear responsibilities and C ABI boundaries:

```
libcot (frontend)  →  libcir (IR + passes)  →  libclif (native backend)
     Zig → Cot            Zig → Cot              Rust (Cranelift crate)
```

---

## 2. Architecture Overview

```
┌──────────────────────────────────────────────────────────┐
│  cot (CLI binary) — thin driver                           │
│  Links: libcot + libcir + libclif                         │
│                                                           │
│  cot build main.cot -o main      (native via all three)  │
│  cot build main.cot -o main.wasm (wasm via libcot+libcir)│
│  cot check main.cot              (libcot only)           │
│  cot lsp                         (libcot only)           │
├──────────────────────────────────────────────────────────┤
│                                                           │
│  ┌──────────────┐ ┌───────────────┐ ┌─────────────────┐ │
│  │   libcot      │ │   libcir      │ │   libclif       │ │
│  │              │ │               │ │                 │ │
│  │  Scanner     │ │  SSA Builder  │ │  Read CIR       │ │
│  │  Parser      │ │  SSA Passes   │ │  Lower to CLIF  │ │
│  │  AST         │ │  ARC Optimize │ │  Cranelift crate│ │
│  │  Checker     │ │  VWT Dispatch │ │  ARM64 backend  │ │
│  │  Types       │ │  Concurrency  │ │  x64 backend    │ │
│  │  Lowerer     │ │  Wasm Emit    │ │  Object file    │ │
│  │  LSP         │ │  CIR Emit     │ │  Linker invoke  │ │
│  │              │ │               │ │                 │ │
│  │ Cot-specific │ │ Lang-agnostic │ │ Target-specific │ │
│  │              │ │               │ │                 │ │
│  └──────┬───────┘ └───────┬───────┘ └────────┬────────┘ │
│         │    C ABI        │    C ABI          │          │
│         └────────→────────┘──────────→────────┘          │
└──────────────────────────────────────────────────────────┘
```

### Library Responsibilities

**libcot — The Frontend (language-specific)**
- Knows Cot syntax, @safe mode, struct literals, method dispatch
- Parses `.cot` files into AST
- Type checks (generics, Sendable, actor isolation)
- Lowers AST → CIR instructions via C ABI calls to libcir
- Provides LSP for editor integration
- Another frontend (PyCot, CotScript) would replace this entirely

**libcir — The IR (language-agnostic)**
- Knows ARC, VWT, actors, generics — but NOT Cot syntax
- Builds SSA from C ABI calls (cir_build_retain, cir_build_actor_enqueue, etc.)
- Runs optimization passes (ARC pair elimination, dead code, scheduling)
- Emits Wasm bytecode (Code section)
- Emits CIR binary format (cot_ssa custom section)
- Any frontend that can call the C ABI can use this

**libclif — The Native Backend (target-specific)**
- Reads CIR module (in-memory pointer or from cot_ssa section)
- Translates SSA → Cranelift CLIF IR
- Cranelift compiles CLIF → ARM64/x64 machine code
- Emits object file, invokes system linker
- Written in Rust, uses cranelift crate directly (free updates)

---

## 3. C ABI Interfaces

### libcot → libcir Boundary

libcot's lowerer calls these to build the IR:

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

// SSA Instructions — ARC (memory management)
CirValueRef     cir_build_retain(CirBuilderRef b, CirValueRef val);
CirValueRef     cir_build_release(CirBuilderRef b, CirValueRef val);
CirValueRef     cir_build_alloc(CirBuilderRef b, CirTypeRef type, uint64_t size);
CirValueRef     cir_build_is_unique(CirBuilderRef b, CirValueRef val);

// SSA Instructions — VWT (generics)
CirValueRef     cir_build_vwt_copy(CirBuilderRef b, CirValueRef metadata,
                                    CirValueRef src, CirValueRef dst);
CirValueRef     cir_build_vwt_destroy(CirBuilderRef b, CirValueRef metadata,
                                       CirValueRef val);
CirValueRef     cir_build_vwt_size(CirBuilderRef b, CirValueRef metadata);

// SSA Instructions — Concurrency (actors, tasks)
CirValueRef     cir_build_actor_enqueue(CirBuilderRef b, CirValueRef actor,
                                         CirValueRef job);
CirValueRef     cir_build_task_create(CirBuilderRef b, CirFuncRef body,
                                       CirValueRef context);
CirValueRef     cir_build_task_switch(CirBuilderRef b, CirValueRef executor);
CirValueRef     cir_build_await(CirBuilderRef b, CirValueRef future);

// Passes and emission
void            cir_run_passes(CirModuleRef mod);
int             cir_emit_wasm(CirModuleRef mod, const char* output_path);
int             cir_emit_wasm_with_ssa(CirModuleRef mod, const char* output_path);

// For in-process native compilation (no serialization)
const void*     cir_get_module_ptr(CirModuleRef mod);
```

### libcir → libclif Boundary

```c
// Single function: compile CIR module to native object file
int             clif_compile(const void* cir_module,
                             const char* target,       // "arm64-macos", "x64-linux"
                             const char* output_path); // "main.o"

// Or from serialized format
int             clif_compile_file(const char* wasm_path,  // .wasm with cot_ssa section
                                  const char* target,
                                  const char* output_path);
```

---

## 4. Data Flow

### Native Build (in-process, fast)

```
cot build main.cot -o main

  libcot                     libcir                     libclif
  ┌──────────┐              ┌──────────┐              ┌──────────┐
  │ scan     │              │ SSA in   │              │ read CIR │
  │ parse    │──cir_build_*─→│ memory   │──cir_module*─→│ → CLIF   │
  │ check    │              │ ARC opt  │              │ → ARM64  │
  │ lower    │              │ schedule │              │ → .o     │
  └──────────┘              └──────────┘              └──────────┘
                                                         │
                                                      zig cc → binary
```

No serialization. Same process. Pointers passed via C ABI.

### Wasm Build (in-process)

```
cot build main.cot -o main.wasm

  libcot                     libcir
  ┌──────────┐              ┌──────────┐
  │ scan     │              │ SSA in   │
  │ parse    │──cir_build_*─→│ memory   │──→ .wasm file
  │ check    │              │ ARC opt  │
  │ lower    │              │ wasm emit│
  └──────────┘              └──────────┘

  libclif not involved.
```

### Wasm + CIR Build (portable artifact)

```
cot build main.cot -o main.wasm --emit-ir

  libcot                     libcir
  ┌──────────┐              ┌──────────┐
  │ scan     │              │ SSA in   │
  │ parse    │──cir_build_*─→│ memory   │──→ .wasm file with:
  │ check    │              │ ARC opt  │     ├── Code section (Wasm bytecode)
  │ lower    │              │ wasm emit│     └── cot_ssa section (SSA binary)
  └──────────┘              │ CIR emit │
                            └──────────┘

  Later, on any machine:
  cot-backend main.wasm -o main    (libclif reads cot_ssa → native)
```

### Check Only

```
cot check main.cot

  libcot only
  ┌──────────┐
  │ scan     │
  │ parse    │──→ errors/warnings/diagnostics
  │ check    │
  └──────────┘
```

### LSP

```
cot lsp

  libcot only
  ┌──────────┐
  │ scan     │
  │ parse    │──→ diagnostics, completions, hover, goto-def
  │ check    │
  │ LSP      │
  └──────────┘
```

### Other Frontend

```
PyCot frontend (hypothetical):

  PyCot                      libcir                     libclif
  ┌──────────┐              ┌──────────┐              ┌──────────┐
  │ scan Py  │              │ SSA in   │              │ read CIR │
  │ parse Py │──cir_build_*─→│ memory   │──cir_module*─→│ → CLIF   │
  │ check Py │              │ ARC opt  │              │ → native │
  │ lower Py │              │ passes   │              │          │
  └──────────┘              └──────────┘              └──────────┘

  PyCot gets ARC, actors, generics, Wasm, and native for free.
  It only implements Python syntax → CIR instruction translation.
```

---

## 5. File Mapping: Current Zig → New Structure

### libcot (Frontend)

| Current Zig File | New Location | Lines |
|------------------|--------------|-------|
| `compiler/frontend/scanner.zig` | `libcot/scanner.zig` | ~1,200 |
| `compiler/frontend/parser.zig` | `libcot/parser.zig` | ~2,400 |
| `compiler/frontend/ast.zig` | `libcot/ast.zig` | ~800 |
| `compiler/frontend/checker.zig` | `libcot/checker.zig` | ~5,000 |
| `compiler/frontend/lower.zig` | `libcot/lower.zig` | ~13,000 |
| `compiler/frontend/types.zig` | `libcot/types.zig` | ~2,500 |
| `compiler/frontend/source.zig` | `libcot/source.zig` | ~300 |
| `compiler/frontend/errors.zig` | `libcot/errors.zig` | ~200 |
| `compiler/frontend/target.zig` | `libcot/target.zig` | ~100 |
| `compiler/frontend/vwt_gen.zig` | `libcot/vwt_gen.zig` | ~1,500 |
| `compiler/lsp/` | `libcot/lsp/` | ~5,000 |
| **Total** | | **~32,000** |

### libcir (IR + Passes + Wasm Emit)

| Current Zig File | New Location | Lines |
|------------------|--------------|-------|
| `compiler/frontend/ssa_builder.zig` | `libcir/ssa_builder.zig` | ~2,900 |
| `compiler/ir/` | `libcir/ir/` | ~6,400 |
| `compiler/ssa/` | `libcir/ssa/` | ~8,200 |
| `compiler/codegen/wasm/` | `libcir/wasm/` | ~5,500 |
| `compiler/codegen/test_runtime.zig` | `libcir/test_runtime.zig` | ~500 |
| `compiler/codegen/bench_runtime.zig` | `libcir/bench_runtime.zig` | ~300 |
| New: `cir_api.zig` (C ABI exports) | `libcir/cir_api.zig` | ~500 |
| New: `cir_serialize.zig` (binary format) | `libcir/cir_serialize.zig` | ~1,000 |
| **Total** | | **~25,300** |

### libclif (Native Backend — Rust)

| Current Zig File | Replaced By | Lines |
|------------------|-------------|-------|
| `compiler/codegen/native/` (68K lines) | `libclif/src/` (~5-10K Rust) | ~5,000-10,000 |

The 68K lines of hand-ported Cranelift in Zig becomes ~5-10K lines of Rust calling the real Cranelift crate. The register allocator, instruction selector, ARM64/x64 emitters — all come from the crate.

### CLI Driver

| Current Zig File | New Location | Lines |
|------------------|--------------|-------|
| `compiler/driver.zig` (6,700 lines) | `cot-cli/main.zig` (~500 lines) | ~500 |
| `compiler/cli.zig` | `cot-cli/cli.zig` | ~300 |
| `compiler/main.zig` | `cot-cli/main.zig` | ~200 |

The driver shrinks from 6,700 lines to ~500 because all the logic moves into the libraries.

---

## 6. CIR Binary Format (Wasm Custom Section)

The CIR binary format lives inside a standard Wasm custom section named `cot_ssa`. It uses LEB128 encoding (same as Wasm) for compactness.

```
Custom Section "cot_ssa":
  Magic: 0x43 0x49 0x52 0x00    ("CIR\0")
  Version: LEB128

  Section: Types
    Count: LEB128
    [TypeEntry]:
      Kind: u8 (struct=1, actor=2, enum=3, func=4, ...)
      Name: string (LEB128 length + bytes)
      Fields: [FieldEntry] (type_idx + name + offset)

  Section: Metadata
    Count: LEB128
    [MetadataEntry]:
      Type: type_idx
      VWT pointer: u32 (index into function table)
      Size: LEB128
      Stride: LEB128
      Flags: u32 (Sendable, actor, trivial, ...)

  Section: Functions
    Count: LEB128
    [FuncEntry]:
      Name: string
      Signature: type_idx
      IsAsync: bool
      ActorIsolation: u8 (none=0, actor=1, global_actor=2)
      BlockCount: LEB128
      [BlockEntry]:
        ValueCount: LEB128
        [ValueEntry]:
          Opcode: u8
          Type: type_idx
          Args: [value_idx] (LEB128 each)
          AuxData: varies by opcode

  Section: Strings
    Count: LEB128
    [StringEntry]:
      Length: LEB128
      Bytes: [u8]

  Section: Globals
    Count: LEB128
    [GlobalEntry]:
      Name: string
      Type: type_idx
      Size: LEB128
```

### CIR Opcodes (subset)

```
// Arithmetic (0x00-0x1F)
0x00  add
0x01  sub
0x02  mul
0x03  div_s
0x04  div_u
0x05  rem_s
0x06  rem_u
0x07  and
0x08  or
0x09  xor
0x0A  shl
0x0B  shr_s
0x0C  shr_u

// Memory (0x20-0x3F)
0x20  load
0x21  store
0x22  local_addr
0x23  global_addr
0x24  off_ptr

// Control (0x40-0x5F)
0x40  br
0x41  brif
0x42  ret
0x43  ret_void
0x44  call
0x45  call_indirect
0x46  phi
0x47  select

// Constants (0x60-0x6F)
0x60  const_int
0x61  const_float
0x62  const_bool
0x63  const_nil

// Comparison (0x70-0x7F)
0x70  eq
0x71  ne
0x72  lt
0x73  le
0x74  gt
0x75  ge

// ARC (0xA0-0xAF) — language-agnostic memory management
0xA0  retain
0xA1  release
0xA2  alloc
0xA3  dealloc
0xA4  is_unique
0xA5  arc_retain_value    // VWT-aware retain
0xA6  arc_release_value   // VWT-aware release

// VWT / Generics (0xB0-0xBF)
0xB0  vwt_copy
0xB1  vwt_destroy
0xB2  vwt_size
0xB3  vwt_stride
0xB4  metadata_addr
0xB5  vwt_init_with_copy
0xB6  vwt_assign_with_copy

// Concurrency (0xC0-0xCF)
0xC0  actor_enqueue
0xC1  actor_resign
0xC2  task_create
0xC3  task_switch
0xC4  task_cancel
0xC5  task_is_cancelled
0xC6  await_future
0xC7  async_suspend        // state machine suspension point
0xC8  async_resume         // state machine resume point
```

---

## 7. The Self-Hosting Path

### Phase 1: Restructure Zig (current code, new boundaries)

```
zig-libcot/    ← compiler/frontend/ + compiler/lsp/
zig-libcir/    ← compiler/ir/ + compiler/ssa/ + compiler/codegen/wasm/
rust-libclif/  ← new Rust project, replaces compiler/codegen/native/
cot-cli/       ← thin driver
```

Verify: 370 tests pass, selfcot builds. No functional changes — just file reorganization + C ABI wrappers.

### Phase 2: Hand-port libcot to Cot

John hand-writes the Cot frontend in Cot, using the Zig code as reference. This is the learning phase — understanding every compiler stage by implementing it.

```
cot-libcot/    ← hand-written in Cot (scanner, parser, checker, lowerer)
zig-libcir/    ← still Zig (unchanged)
rust-libclif/  ← still Rust (unchanged)
```

### Phase 3: Hand-port libcir to Cot

```
cot-libcot/    ← Cot
cot-libcir/    ← hand-written in Cot (SSA, passes, wasm emit, CIR format)
rust-libclif/  ← still Rust (unchanged)
```

### Phase 4: Delete Zig entirely

```
cot-libcot/    ← Cot (self-hosted frontend)
cot-libcir/    ← Cot (self-hosted IR)
rust-libclif/  ← Rust (Cranelift crate, the only non-Cot dependency)
cot-cli/       ← Cot
```

The Zig code becomes the historical bootstrapper — frozen, never touched again. Like how Rust's original OCaml compiler is archived but never used.

### Bootstrap Chain

```
First time:
  zig-libcot + zig-libcir + rust-libclif → compiles cot-libcot → cot-libcot binary

Every release after:
  cot-libcot(N) + cot-libcir(N) + rust-libclif → compiles cot-libcot(N+1)

  Zig only needed if bootstrap from scratch.
  Rust only rebuilds if Cranelift updates.
```

---

## 8. The Bigger Vision: CIR as a Compilation Target

Once CIR has a stable C API (libcir) and a serialized binary format (cot_ssa custom section), it becomes a compilation target for other languages — not just Cot.

```
LLVM:     "Write a frontend, get optimization + codegen free"
          Commoditized backends.

Cot CIR:  "Write a frontend, get ARC + actors + generics + Wasm + native free"
          Commoditizes memory management and concurrency.
```

Any language that wants:
- Automatic reference counting (no GC pauses)
- Actor-based concurrency (compile-time data race prevention)
- Generic dispatch (VWT shared bodies or monomorphization)
- Wasm + native from the same compilation

...can target CIR instead of implementing all of it from scratch.

This is the long-term differentiator. Not the language — the IR.

---

## 9. Comparison with Existing IRs

| | LLVM IR | .NET IL | JVM Bytecode | Cot CIR |
|---|---|---|---|---|
| Level | Low (near assembly) | Medium (typed stack) | Medium (typed stack) | High (SSA + semantics) |
| Memory model | Manual | GC | GC | ARC |
| Understands generics | No | Yes (reified) | Yes (erased) | Yes (VWT) |
| Understands concurrency | No | Partial (async) | Partial (threads) | Yes (actors) |
| Understands ownership | No | No | No | Yes (retain/release) |
| Compilation | AOT | JIT (RyuJIT) | JIT (HotSpot) | AOT |
| Wasm target | Yes (backend) | Experimental | No | Yes (native emit) |
| Format | Bitcode (binary) | PE/.dll (binary) | .class (binary) | Wasm custom section |
| Self-hosted | No (C++) | No (C#→.NET) | No (Java→JVM) | Yes (Cot→CIR) |

CIR occupies a unique position: higher-level than LLVM (understands ARC, actors, generics), but AOT-compiled (no runtime JIT like .NET/JVM), with Wasm as a first-class target, and the entire stack self-hostable in the source language.

---

## 10. What This Means for the Business

The open-source / closed-source split becomes natural:

| Component | License | Rationale |
|-----------|---------|-----------|
| libcot (Cot frontend) | Open source (MIT) | The language is the community |
| libcir (CIR) | Open source (MIT) | The IR is the ecosystem — needs adoption |
| libclif (native backend) | Open source (MIT) | Thin wrapper around open-source Cranelift |
| cot-cli | Open source (MIT) | Users need the compiler |
| **Cot Studio** | **Proprietary** | IDE, component library, deployment platform |
| **Cot Cloud** | **Proprietary** | Managed Wasm deployment, edge runtime |
| **CIR Optimizer Pro** | **Proprietary** | Advanced ARC/actor optimization passes |

The language, IR, and compiler are free. The developer experience, deployment platform, and advanced tooling are the product. Same model as Rust (free) + Ferrocene (paid certified compiler) or VS Code (free) + GitHub Copilot (paid).
