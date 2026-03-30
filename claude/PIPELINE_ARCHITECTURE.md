# Cot Compilation Pipeline Architecture

## Purpose

This document maps **every stage of the Cot compilation pipeline** to its reference implementation. Each stage must be ported from a proven language — never invented.

**Read this document before working on any compilation pipeline feature.**

---

## The Full Pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                        COT SOURCE CODE                               │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │   STAGE 1: FRONTEND    │  Reference: Zig + Go
                    │  Scanner → Parser →    │
                    │  Checker → IR → SSA    │
                    └───────────┬───────────┘
                                │
              ┌─────────────────┼─────────────────┐
              │                                   │
    ┌─────────▼─────────┐             ┌───────────▼───────────┐
    │ --target=wasm32    │             │ --target=native       │
    │                    │             │ (DEFAULT)              │
    │  STAGE 2: WASM     │             │                        │
    │  SSA → Wasm ops    │ Ref: Go     │  STAGE 2: CIR          │ Ref: cg_clif
    │  → .wasm file      │             │  SSA → CIR bytes       │
    │  Done.             │             │  (+ runtime + data)    │
    └────────────────────┘             └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 3: LIBCLIF      │
                                       │  CIR → Cranelift IR    │ Ref: Cranelift
                                       │  → native .o           │
                                       │  (rust/libclif OR       │
                                       │   zig/libclif)          │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 4: LINK         │
                                       │  .o → executable       │ cc / zig cc
                                       └───────────────────────┘
```

---

## The CIR Boundary

**libcot produces CIR bytes. libclif produces .o files. That's the contract.**

CIR contains everything needed to produce a complete native binary:
- **Function definitions** — user functions + runtime functions (ARC, I/O, print, signal, test)
- **Data definitions** — string data, globals, type metadata (Section 0x04)
- **Entry wrapper** — `__cot_entry` function (stores argc/argv/envp, calls _cot_main)

Both `rust/libclif` and `zig/libclif` implement the same C ABI:
```c
int clif_compile(const uint8_t* cir, size_t len, const char* target, uint8_t** out, size_t* out_len);
void clif_free(uint8_t* ptr, size_t len);
```

---

## Stage Details

### Stage 1: Frontend (libcot)

| Step | File | Reference |
|------|------|-----------|
| Scan | `frontend/scanner.zig` | Zig tokenizer |
| Parse | `frontend/parser.zig` | Zig parser |
| Check | `frontend/checker.zig` | Zig Sema |
| Lower | `frontend/lower.zig` | Go SSA builder |
| SSA Build | `frontend/ssa_builder.zig` | Go SSA |
| SSA Passes | `ssa/passes/*.zig` | Go + Cranelift |

### Stage 2 (Native): CIR Generation (libcot)

| Step | File | Reference |
|------|------|-----------|
| SSA → CIR | `codegen/ssa_to_cir.zig` | rustc_codegen_cranelift |
| ARC runtime | `codegen/arc_runtime.zig` | Swift HeapObject |
| I/O runtime | `codegen/io_runtime.zig` | libc wrappers |
| Print runtime | `codegen/print_runtime_native.zig` | snprintf wrappers |
| Signal handler | `codegen/signal_runtime.zig` | Go signal_unix |
| Test runner | `codegen/test_runtime_native.zig` | Zig test runner |
| C ABI | `codegen/libclif.zig` | clif_compile binding |

### Stage 2 (Wasm): Wasm Bytecode (libcot)

| Step | File | Reference |
|------|------|-----------|
| SSA → Wasm | `codegen/wasm/wasm.zig` | Go cmd/compile/wasm |
| ARC runtime | `codegen/arc.zig` | Swift HeapObject |
| Memory | `codegen/mem_runtime.zig` | Go runtime |

### Stage 3: libclif (external)

| Backend | Location | Reference |
|---------|----------|-----------|
| Real Cranelift | `rust/libclif/` | Cranelift (crates.io) |
| Hand-ported | `zig/libclif/` | Wasmtime Cranelift source |

---

## CIR Format

See `claude/CIR_FORMAT_SPEC.md` for the complete specification.

Key sections:
- **Section 0x01** — String heap (interned names)
- **Section 0x04** — Data definitions (globals, string data, metadata)
- **Section 0x06** — Function definitions (opcodes, blocks, instructions)
