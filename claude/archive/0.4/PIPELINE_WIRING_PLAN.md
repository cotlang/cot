# Pipeline Wiring Plan: Source → Binary

**Date:** 2026-03-29
**Status:** Design document
**Goal:** Wire the ported libraries into a working compiler that produces .wasm and native binaries

---

## Current State

Three libraries compile and pass tests independently:
- **foundation** (4 files) — shared types
- **libcot-zig** (12 files) — frontend: parse → check → lower → IR
- **libcir-zig** (24 files) — SSA builder → passes → Wasm codegen

Integration tests validate: source → parse → check (6 tests pass).

**Missing:** The driver that connects lower → SSA builder → passes → codegen.

---

## The Pipeline

```
Source Text
    │
    ▼
┌─────────────────────────── libcot ───────────────────────────┐
│  Scanner → Parser → AST → Checker → Lower → IR Functions     │
└──────────────────────────────┬────────────────────────────────┘
                               │ ir.Func[]
                               ▼
┌─────────────────────────── libcir ───────────────────────────┐
│  SSA Builder → SSA Funcs                                      │
│       │                                                       │
│       ▼                                                       │
│  Passes: rewritegeneric → decompose → rewritedec →           │
│          lower_wasm → deadcode → copyelim → phielim →         │
│          cse → schedule → layout                              │
│       │                                                       │
│       ▼                                                       │
│  ┌─────────────────┐  ┌──────────────────────────────┐       │
│  │ Wasm Codegen     │  │ Native (via libclif)          │       │
│  │ gen → preprocess  │  │ clif_compile(cir_module, ...) │       │
│  │ → assemble → link │  │ → ARM64/x64 object file      │       │
│  │ → .wasm file      │  │ → linker → executable         │       │
│  └─────────────────┘  └──────────────────────────────┘       │
└──────────────────────────────────────────────────────────────┘
```

---

## Step 1: Driver Module (src/driver.zig)

Port the pipeline orchestration from `compiler/driver.zig` (6,730 lines). This is NOT a library — it's the glue that links everything. Key functions:

```zig
pub fn compileFile(path: []const u8, options: CompileOptions) !void {
    // 1. Read source file
    // 2. Parse (libcot: scanner → parser → AST)
    // 3. Check (libcot: checker → typed AST)
    // 4. Lower (libcot: lower → IR functions)
    // 5. VWT Gen (libcir: generate witness tables)
    // 6. SSA Build (libcir: IR → SSA for each function)
    // 7. SSA Passes (libcir: optimize)
    // 8. Codegen:
    //    - Wasm: gen → preprocess → assemble → link → .wasm
    //    - Native: clif_compile() via libclif-rs → .o → link → executable
}
```

The driver imports from all three libraries:
```zig
const libcot = @import("libcot");
const libcir = @import("libcir");
// libclif via C ABI: @cImport(@cInclude("clif.h"))
```

### What to port from compiler/driver.zig:

| Section | Lines | Port? |
|---------|-------|-------|
| compileFile pipeline | ~200 | Yes — core orchestration |
| Multi-file compilation | ~300 | Yes — import resolution |
| SSA pass pipeline | ~150 | Yes — pass ordering |
| Wasm codegen pipeline | ~200 | Yes — runtime function generation |
| Native codegen pipeline | ~500 | **Replace with libclif-rs** |
| Test/bench runner generation | ~100 | Yes — from lower.zig |
| Global init generation | ~100 | Yes — from lower.zig |
| String/ARC/WASI runtime emission | ~1,000 | Yes for Wasm, **replace for native** |
| MachO/ELF object file writing | ~2,000 | **Delete — libclif handles this** |
| Native linker invocation | ~500 | **Simplify — clif_link()** |
| Debug info (DWARF) | ~500 | **Defer — libclif handles this** |
| HTML visualizer | ~100 | Yes — from ssa/html.zig |

The native backend (~3,500 lines of ssa_to_clif, MachO, ELF, ARM64, x64) is completely replaced by `libclif-rs` — one function call: `clif_compile(cir_module, target, output_path)`.

---

## Step 2: libclif-rs (Rust Cranelift Wrapper)

A thin Rust crate that exposes the clif.h C ABI using the real Cranelift:

```
src/libclif-rs/
├── Cargo.toml
├── src/
│   └── lib.rs         ~500 lines — implements clif.h functions
└── build.rs           Links as C static library
```

### Cargo.toml
```toml
[package]
name = "cot-clif"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["staticlib"]

[dependencies]
cranelift-codegen = "0.116"
cranelift-frontend = "0.116"
cranelift-module = "0.116"
cranelift-object = "0.116"
cranelift-native = "0.116"
target-lexicon = "0.12"
```

### lib.rs — implements clif.h
```rust
use cranelift_codegen::*;
use cranelift_frontend::*;
use cranelift_module::*;
use cranelift_object::*;

// Read CIR module from memory pointer
// Translate each CIR function → Cranelift IR
// Compile → object file
// Link → executable

#[no_mangle]
pub extern "C" fn clif_compile(
    cir_module: *const std::ffi::c_void,
    target: *const std::ffi::c_char,
    output_path: *const std::ffi::c_char,
) -> i32 {
    // 1. Cast cir_module to Zig's SSA module representation
    // 2. For each SSA function:
    //    a. Create Cranelift FunctionBuilder
    //    b. Translate SSA ops → Cranelift IR
    //    c. Compile to machine code
    // 3. Write object file
    0 // success
}
```

### Why Cranelift (not hand-written codegen):

The current Zig Cranelift port (`compiler/codegen/native/`) is 68K lines of hand-ported Rust. It works but:
- Can't receive upstream Cranelift updates
- ARM64 and x64 backends must be maintained manually
- No access to Cranelift's `Switch::emit()` which solves the async dispatch bug (Gap 9)

The Rust wrapper is ~500 lines. `cargo update` gets Cranelift improvements for free. ARM64, x64, RISC-V, s390x — all supported out of the box.

---

## Step 3: Build System Integration

### Root build.zig additions
```zig
// Build libclif-rs via cargo
const cargo_build = b.addSystemCommand(&.{
    "cargo", "build", "--release",
    "--manifest-path", "src/libclif-rs/Cargo.toml",
});

// Link the Rust static library
exe.root_module.addLibraryPath(.{ .cwd_relative = "src/libclif-rs/target/release" });
exe.root_module.linkSystemLibrary("cot_clif");
```

### CLI (src/cli-zig/)
```zig
// cot build main.cot -o main
// cot build main.cot --target=wasm -o main.wasm
// cot run main.cot
// cot test main.cot
```

---

## Step 4: Runtime Functions

For Wasm: runtime functions (alloc, print, string ops, ARC) are emitted as Wasm module functions by the driver. These already work in libcir-zig's Wasm codegen.

For native: runtime functions are compiled by Cranelift or linked from a C runtime. Options:
1. **Compile runtime.zig to .o** and link alongside user code (current approach)
2. **Emit runtime as CIR** and compile through the same Cranelift path
3. **Compile runtime.c with system C compiler** and link

Option 1 is simplest and matches the current compiler's approach.

---

## Implementation Order

| Step | What | Effort | Depends On |
|------|------|--------|-----------|
| 1 | Wire integration test: parse → check → lower → IR | 1 day | Done files |
| 2 | Wire integration test: IR → SSA builder → passes | 1 day | Step 1 |
| 3 | Wire integration test: SSA → Wasm codegen → .wasm | 1 day | Step 2 |
| 4 | Port driver.zig (Wasm pipeline only) | 2-3 days | Steps 1-3 |
| 5 | Create libclif-rs Cargo crate | 1 day | Cranelift docs |
| 6 | Implement clif_compile (CIR → Cranelift → .o) | 3-5 days | Step 5 |
| 7 | Implement clif_link (.o → executable) | 1 day | Step 6 |
| 8 | CLI (cot build, cot run, cot test) | 1-2 days | Steps 4+7 |
| 9 | Gate test: `cot build hello.cot -o hello && ./hello` | 1 day | Step 8 |

**Gate:** Step 9 — compile a hello world Cot program to a native binary and execute it.

---

## What We Keep From compiler/

- driver.zig pipeline orchestration (~2,000 lines, adapted)
- Wasm runtime function emission (arc.zig, mem_runtime.zig, etc.)
- SSA pass ordering
- Multi-file import resolution
- Test/bench runner generation

## What We Replace

- Native codegen (68K lines of Zig Cranelift port) → 500 lines of Rust FFI
- MachO/ELF object writing → Cranelift handles it
- ARM64/x64 instruction selection → Cranelift handles it
- Register allocation → Cranelift handles it
- Debug info generation → Cranelift handles it (via gimli)

## What We Defer

- libclif-zig (Zig Cranelift port) — still exists in compiler/, can be used as fallback
- LLVM backend — future option via libllvm-c implementing same clif.h
- Source maps / debug info quality improvements
