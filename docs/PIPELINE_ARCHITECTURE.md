# Cot Compilation Pipeline Architecture

## Purpose

This document maps **every stage of the Cot compilation pipeline** to its reference implementation. Each stage must be ported from a proven language — never invented. This document exists because Claude has repeatedly confused the pipeline stages, mixed up native vs Wasm concerns, and invented approaches that fail.

**Read this document before working on any compilation pipeline feature.**

---

## The Full Pipeline

```
┌─────────────────────────────────────────────────────────────────────┐
│                        COT SOURCE CODE                               │
│                     (written by the user)                            │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                    ┌───────────▼───────────┐
                    │   STAGE 1: FRONTEND    │  Reference: Zig + Go
                    │  Scanner → Parser →    │
                    │  Checker → IR → SSA    │
                    └───────────┬───────────┘
                                │
                    ┌───────────▼───────────┐
                    │  STAGE 2: WASM CODEGEN │  Reference: Go
                    │  SSA → Wasm bytecode   │
                    │  (always, both targets)│
                    └───────────┬───────────┘
                                │
              ┌─────────────────┼─────────────────┐
              │                                   │
    ┌─────────▼─────────┐             ┌───────────▼───────────┐
    │ --target=wasm32    │             │ --target=native       │
    │                    │             │ (DEFAULT)              │
    │  Write .wasm file  │             │                        │
    │  Done.             │             │  STAGE 3: WASM PARSE   │ Ref: wasmparser
    └────────────────────┘             │  Parse Wasm binary     │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 4: CLIF IR      │ Ref: Cranelift
                                       │  Wasm → CLIF IR        │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 5: LOWERING     │ Ref: Cranelift
                                       │  CLIF → MachInst       │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 6: REGALLOC     │ Ref: regalloc2
                                       │  VReg → PReg           │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 7: EMIT         │ Ref: Cranelift
                                       │  MachInst → bytes      │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 8: OBJECT FILE  │ Ref: cranelift-object
                                       │  Mach-O or ELF .o      │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 9: LINK         │ External: zig cc
                                       │  .o → executable       │
                                       └───────────────────────┘
```

---

## Stage-by-Stage Reference Map

### Stage 1: Frontend (Scanner → Parser → Checker → IR → SSA)

**Reference: Zig for language semantics, Go for SSA construction**

| Component | Reference | Location |
|-----------|-----------|----------|
| Scanner | Standard (no specific port) | `compiler/frontend/scanner.zig` |
| Parser | Zig-inspired syntax | `compiler/frontend/parser.zig` |
| Checker | Zig type system (error unions, optionals) | `compiler/frontend/checker.zig` |
| IR lowering | Go SSA patterns | `compiler/frontend/lower.zig` |
| SSA builder | Go `compile/internal/ssa/` | `compiler/frontend/ssa_builder.zig` |

**What this stage does:**
- Parses Cot source into AST
- Type-checks all expressions (Zig-style type system)
- Lowers AST → flat IR (function bodies become IR nodes)
- Builds SSA form (Go's SSA construction patterns)

**Key files in Go reference:**
- `~/learning/go/src/cmd/compile/internal/ssa/` — SSA value, block, function types
- `~/learning/go/src/cmd/compile/internal/ssa/rewritegeneric.go` — algebraic rewrites
- `~/learning/go/src/cmd/compile/internal/ssa/rewritedec.go` — compound type decomposition

### Stage 2: Wasm Codegen (SSA → Wasm bytecode)

**Reference: Go's Wasm backend**

| Component | Reference | Location |
|-----------|-----------|----------|
| Op lowering | `compile/internal/ssa/lower.go` | `compiler/ssa/passes/lower_wasm.zig` |
| Wasm generation | `compile/internal/wasm/ssa.go` | `compiler/codegen/wasm/gen.zig` |
| Wasm assembly | `internal/obj/wasm/wasmobj.go` | `compiler/codegen/wasm/assemble.zig` |
| Wasm linking | `link/internal/wasm/asm.go` | `compiler/codegen/wasm/link.zig` |
| Slice decomposition | `compile/internal/ssa/rewritedec.go` | `compiler/ssa/passes/rewritedec.zig` |

**What this stage does:**
- Transforms SSA ops into Wasm-specific ops (e.g., `add` → `i64.add`)
- Generates Wasm bytecode for each function body
- Assembles a complete Wasm module (types, functions, memory, table, data sections)
- This stage runs for BOTH targets (Wasm and native use this output)

**Key files in Go reference:**
- `~/learning/go/src/cmd/compile/internal/wasm/ssa.go` — SSA op → Wasm instruction mapping
- `~/learning/go/src/cmd/internal/obj/wasm/wasmobj.go` — Wasm binary format encoding
- `~/learning/go/src/cmd/link/internal/wasm/asm.go` — Wasm section layout, import handling

**ARC runtime functions** (`cot_alloc`, `cot_retain`, `cot_release`) are generated as Wasm bytecode at this stage by `arc.zig`. They become regular Wasm functions in the module. The ARC pattern is ported from **Swift** (`HeapObject.cpp`), but the implementation is Wasm bytecode using Go's codegen patterns.

| ARC Function | Swift Reference | What It Does |
|-------------|-----------------|--------------|
| `cot_alloc` | `swift_allocObject` (HeapObject.cpp:247) | Bump-allocate with header (metadata + refcount) |
| `cot_retain` | `swift_retain` (HeapObject.cpp:476) | Increment refcount |
| `cot_release` | `swift_release` (HeapObject.cpp:835) | Decrement refcount, call destructor at zero |

### Stage 3: Wasm Parse (native path only)

**Reference: wasmparser crate (Rust)**

| Component | Reference | Location |
|-----------|-----------|----------|
| Wasm parser | `wasmparser` crate | `compiler/codegen/native/wasm_parser.zig` |
| Wasm decoder | Wasm spec binary format | `compiler/codegen/native/wasm_to_clif/decoder.zig` |

**What this stage does:**
- Parses the Wasm bytecode that Stage 2 produced
- Extracts types, functions, memory, table, data, element sections
- Provides structured data for the CLIF translator

**Important:** The native compiler reads its OWN Wasm output. The Wasm is an internal IR, never saved to disk for native targets.

### Stage 4: CLIF IR Translation (native path only)

**Reference: Cranelift's `cranelift-wasm` crate**

| Component | Reference | Location |
|-----------|-----------|----------|
| Function translator | `cranelift-wasm/src/code_translator.rs` | `compiler/codegen/native/wasm_to_clif/translator.zig` |
| Func environment | `cranelift-wasm/src/environ.rs` | `compiler/codegen/native/wasm_to_clif/func_environ.zig` |
| CLIF types | `cranelift/codegen/src/ir/types.rs` | `compiler/codegen/native/ir/clif/types.zig` |
| DFG (data flow graph) | `cranelift/codegen/src/ir/dfg.rs` | `compiler/codegen/native/ir/clif/dfg.zig` |
| SSA frontend | `cranelift/frontend/src/frontend.rs` | `compiler/codegen/native/ir/clif/frontend.zig` |

**What this stage does:**
- Walks each Wasm function's opcodes
- Translates Wasm stack machine ops → CLIF IR (register-based SSA)
- Handles Wasm-specific constructs (blocks, loops, br_table, memory ops)
- Manages the VMContext (heap base pointer, globals)

**Key Cranelift files:**
- `~/learning/wasmtime/crates/cranelift/src/translate/` — Wasm → CLIF translation
- `~/learning/wasmtime/cranelift/codegen/src/ir/` — CLIF IR types
- `~/learning/wasmtime/cranelift/frontend/src/frontend.rs` — SSA construction with block params

### Stage 5: Machine Instruction Lowering (native path only)

**Reference: Cranelift's `machinst` framework**

| Component | Reference | Location |
|-----------|-----------|----------|
| CLIF → MachInst | `cranelift/codegen/src/machinst/lower.rs` | `compiler/codegen/native/machinst/lower.zig` |
| ARM64 patterns | `cranelift/codegen/src/isa/aarch64/lower.rs` | `compiler/codegen/native/isa/aarch64/lower.zig` |
| x64 patterns | `cranelift/codegen/src/isa/x64/lower.rs` | `compiler/codegen/native/isa/x64/lower.zig` |
| MachInst types | `cranelift/codegen/src/isa/aarch64/inst/` | `compiler/codegen/native/isa/aarch64/inst/` |
| ABI | `cranelift/codegen/src/machinst/abi.rs` | `compiler/codegen/native/machinst/abi.zig` |

**What this stage does:**
- Pattern-matches CLIF IR operations into machine instructions
- Handles ISA-specific instruction selection (ARM64 vs x64)
- Manages calling conventions and stack frames

### Stage 6: Register Allocation (native path only)

**Reference: regalloc2 (Rust crate)**

| Component | Reference | Location |
|-----------|-----------|----------|
| Ion allocator | `regalloc2/src/ion/` | `compiler/codegen/native/regalloc/` |
| Liveness analysis | `regalloc2/src/ion/liveranges.rs` | `compiler/codegen/native/regalloc/liveness.zig` |
| Merge/split | `regalloc2/src/ion/merge.rs` | `compiler/codegen/native/regalloc/merge.zig` |
| VCode integration | `cranelift/codegen/src/machinst/vcode.rs` | `compiler/codegen/native/machinst/vcode.zig` |

**What this stage does:**
- Maps virtual registers to physical registers
- Handles spilling (when not enough physical registers)
- Resolves register moves at block boundaries

### Stage 7: Code Emission (native path only)

**Reference: Cranelift's ISA-specific emitters**

| Component | Reference | Location |
|-----------|-----------|----------|
| ARM64 emit | `cranelift/codegen/src/isa/aarch64/inst/emit.rs` | `compiler/codegen/native/isa/aarch64/emit.zig` |
| x64 emit | `cranelift/codegen/src/isa/x64/inst/emit.rs` | `compiler/codegen/native/isa/x64/emit.zig` |
| Code buffer | `cranelift/codegen/src/machinst/buffer.rs` | `compiler/codegen/native/machinst/buffer.zig` |

**What this stage does:**
- Converts machine instructions into actual bytes (ARM64/x64 encoding)
- Handles fixups for branch offsets
- Records relocations for function calls

### Stage 8: Object File Generation (native path only)

**Reference: cranelift-object crate**

| Component | Reference | Location |
|-----------|-----------|----------|
| Object module | `cranelift-object/src/backend.rs` | `compiler/codegen/native/object_module.zig` |
| Mach-O format | Apple ABI reference | `driver.zig:generateMachO` |
| ELF format | ELF spec | `driver.zig:generateElf` |

**What this stage does:**
- Wraps compiled machine code into an object file (.o)
- Declares function symbols (exported/local)
- Records relocations for cross-function calls
- Embeds data sections (string literals, linear memory)

**Key pattern from Cranelift:**
- Two-pass: declare ALL functions first, then define ALL functions
- This handles forward references (function A calls function B defined later)

### Stage 9: Linking (external tool)

**Current: `zig cc` (Zig's bundled clang/lld)**

```
zig cc -o output file.o -lSystem    # macOS (links libSystem = libc)
zig cc -o output file.o -lc         # Linux (links libc)
```

**What this stage does:**
- Takes the .o file from Stage 8
- Resolves undefined symbols against system libraries (libc)
- Produces the final executable (Mach-O executable on macOS, ELF executable on Linux)
- Sets up dynamic linker paths, entry point, etc.

**The linker is external.** The Cot compiler does not have a built-in linker. This is the same approach as:
- Rust (uses `cc` or `lld`)
- Early Go (used system `ld`, later added built-in linker)
- C compilers (separate `ld` step)

**For self-hosting:** The Cot compiler will still need an external linker. Writing a linker is a separate, massive project. The initial self-hosted compiler will shell out to `cc` or `ld`, just like today's Zig-based compiler does.

---

## Extern Functions: How They Work on Each Target

### What `extern fn` means

An `extern fn` declares a function that exists **outside** the Cot module. The implementation is provided by the host environment.

```cot
extern fn write(fd: i32, buf: *u8, count: i64) i64
```

This is **not** for memory allocation. This is for system calls and host interop — things the compiled code physically cannot do itself.

### Wasm Target (`--target=wasm32`)

**Reference: Go's `//go:wasmimport` (link/internal/wasm/asm.go:154-181, 316-334)**

```
extern fn write(fd, buf, count) i64
        ↓
Wasm import section entry: { module: "env", name: "write", type: func(i32,i32,i64)->i64 }
        ↓
Host provides at instantiation:
  WebAssembly.instantiate(bytes, { env: { write: (fd, buf, count) => { ... } } })
```

**Pipeline:**
1. Parser: `is_extern = true` on FnDecl AST node
2. Checker: Registers symbol with `is_extern = true`
3. Lowerer: Skips body generation (`if (fn_decl.is_extern) return`)
4. Wasm codegen: Should call `linker.addImport()` to generate import entry
5. Calls to extern functions use the import's function index

**Current status:** Steps 1-3 work. Step 4 is **not wired up** — the `WasmImport` infrastructure exists in `link.zig` but `driver.zig` never calls `addImport()` for extern functions.

**Go reference for import section generation:**
```go
// ~/learning/go/src/cmd/link/internal/wasm/asm.go:154-181
for _, fn := range ctxt.Textp {
    relocs := ldr.Relocs(fn)
    for ri := 0; ri < relocs.Count(); ri++ {
        r := relocs.At(ri)
        if r.Type() == objabi.R_WASMIMPORT {
            // Collect into hostImports list
        }
    }
}

// ~/learning/go/src/cmd/link/internal/wasm/asm.go:316-334
func writeImportSec(ctxt *ld.Link, hostImports []*wasmFunc) {
    writeUleb128(ctxt.Out, uint64(len(hostImports)))
    for _, fn := range hostImports {
        writeName(ctxt.Out, fn.Module)  // e.g., "env"
        writeName(ctxt.Out, fn.Name)    // e.g., "write"
        ctxt.Out.WriteByte(0x00)        // func import
        writeUleb128(ctxt.Out, uint64(fn.Type))
    }
}
```

### Native Target (default)

**Reference: Cranelift's cranelift-object (backend.rs)**

```
extern fn write(fd, buf, count) i64
        ↓
Undefined symbol "_write" in .o file relocation table
        ↓
zig cc -o output file.o -lSystem
        ↓
Linker resolves "_write" against libc → linked into executable
```

**Pipeline:**
1. Frontend: Same as Wasm (extern fn parsed, checked, lowerer skips body)
2. Wasm codegen: Generates Wasm import for the function
3. Wasm parser: Must parse the import section to know which functions are imports
4. CLIF translation: Must handle calls to imported functions differently
5. Object file: Emits undefined symbol for the import
6. Linker (`zig cc`): Resolves undefined symbol against libc

**Current status:** Steps 3-5 are **not implemented**. The native path doesn't parse Wasm imports and treats all functions as locally defined.

**Two approaches exist in Cranelift:**

**Approach A: Full VMContext (what Cranelift/Wasmtime does)**
- Imported functions are called indirectly via function pointers stored in VMContext
- Runtime populates VMContext with function addresses at module instantiation
- Reference: `~/learning/wasmtime/crates/cranelift/src/translate/func_environ.rs`
- This is for JIT/runtime scenarios — overkill for AOT

**Approach B: Undefined symbols (what AOT compilers do)**
- Imported functions become undefined symbols in the object file
- The system linker resolves them at link time
- Reference: How Cranelift-object handles `colocated = false` external names
- This is the right approach for Cot's AOT compiler

**To implement (Approach B):**
1. `wasm_parser.zig`: Parse import section (section 2) — track `num_imported_funcs`
2. `func_environ.zig`: Set `colocated = false` for imported function references
3. `object_module.zig`: Emit undefined symbol for non-colocated functions
4. Linker resolves these against libc or user-provided .o files

---

## Memory Allocation: How It Works

### The current allocator (`cot_alloc`)

**Reference: Swift's `swift_allocObject` (simplified)**

`cot_alloc` is a **bump allocator** generated as Wasm bytecode by `arc.zig`. It:
1. Bumps a global heap pointer forward
2. Writes a header (metadata pointer + refcount)
3. Returns pointer to user data after the header
4. **Never frees memory** — when `cot_release` hits refcount 0, it calls the destructor but doesn't reclaim the allocation

**Memory layout:**
```
Wasm linear memory:
┌──────────────┬───────────────────┬──────────────────────────┐
│  Stack       │   Data segments   │   Heap (bump allocator)  │
│  (grows ↓)   │   (strings, etc)  │   (grows →)              │
└──────────────┴───────────────────┴──────────────────────────┘
                                    ^
                                    heap_ptr global (bumps forward)
```

### Why this works on native

The Wasm bytecode (including `cot_alloc`) goes through the same AOT pipeline:
```
cot_alloc Wasm bytecode → CLIF IR → ARM64 → machine code in .o file
```

The native binary has a large `.bss` section (zeroed memory) that serves as "linear memory." The heap pointer is a global variable. `cot_alloc` is just a regular function in the binary that bumps this pointer.

### What needs to change for List(T)

The bump allocator cannot `free` or `realloc`. A `List(T)` that grows needs both.

**Two upgrade paths:**

**Path 1: Add `cot_realloc` and `cot_free` to the ARC runtime**
- Port from Go's `sbrk()` pattern (`runtime/mem_wasm.go`)
- Add a freelist for reclaiming memory
- Reference: Zig's `WasmAllocator.zig` for a complete Wasm allocator with freelists

**Path 2: Use `memory.grow` for expansion (Wasm path), mmap/brk for native**
- Wasm: `memory.grow` adds pages, allocator manages freelists within pages
- Native: The .bss section is fixed at compile time, so growth needs `mmap` or similar

**Current gap on native:** `memory.grow` is translated as "return -1 (failure)" in `translator.zig:1774`. This means any code path that tries to grow memory will silently fail on native.

**To fix:** Translate `memory.grow` to an actual memory growth mechanism on native:
- Cranelift/Wasmtime uses `mmap` via a runtime function
- Reference: `~/learning/wasmtime/crates/runtime/src/memory.rs`

### Why NOT `extern fn malloc`

Every production Wasm compiler (Go, Zig, Rust, Emscripten) implements its own allocator:
- **Go:** `sbrk()` + `memory.grow` in `runtime/mem_wasm.go`
- **Zig:** `WasmAllocator.zig` with size-class freelists + `@wasmMemoryGrow`
- **Rust:** `dlmalloc` or `wee_alloc` compiled to Wasm
- **Emscripten:** `dlmalloc` port + `memory.grow`

The allocator runs INSIDE the module. No host imports needed. This ensures:
- **Portability:** Works in any Wasm host (browser, Node, Wasmtime)
- **Sandboxing:** Wasm memory is self-contained
- **Performance:** No host function call overhead

On the **native path**, the AOT-compiled allocator becomes regular native code. It's the same allocator, just compiled to ARM64/x64 instead of running as Wasm. For native, `memory.grow` must be translated to actual OS-level memory growth (mmap or similar).

---

## Extern Functions: What They're Actually For

`extern fn` is for **host environment interop** — operations that require OS/platform support:

### Examples by category

**Console/Logging:**
```cot
extern fn console_log(ptr: i64, len: i64)     // Browser: console.log
extern fn fd_write(fd: i32, iovs: i32, ...) i32  // WASI: write to file descriptor
```

**File I/O (native via libc):**
```cot
extern fn open(path: *u8, flags: i32) i32
extern fn read(fd: i32, buf: *u8, count: i64) i64
extern fn write(fd: i32, buf: *u8, count: i64) i64
extern fn close(fd: i32) i32
```

**Networking (native via libc):**
```cot
extern fn socket(domain: i32, type: i32, protocol: i32) i32
extern fn bind(fd: i32, addr: *u8, len: i32) i32
extern fn listen(fd: i32, backlog: i32) i32
extern fn accept(fd: i32, addr: *u8, len: *i32) i32
```

**Browser DOM (Wasm only):**
```cot
extern fn document_getElementById(ptr: i64, len: i64) i32
extern fn element_setInnerHTML(id: i32, ptr: i64, len: i64)
```

### How each target resolves them

| Target | Mechanism | Resolver |
|--------|-----------|----------|
| **Wasm** | Wasm import section | Host runtime (browser JS, Node.js, Wasmtime) |
| **Native** | Undefined symbol in .o | System linker (`zig cc` → libc, libSystem) |

### An HTTP server in Cot

On **native** (the default target):
```cot
// These resolve against libc at link time via zig cc -lSystem / -lc
extern fn socket(domain: i32, type: i32, protocol: i32) i32
extern fn bind(fd: i32, addr: *u8, len: i32) i32
extern fn listen(fd: i32, backlog: i32) i32
extern fn accept(fd: i32, addr: *u8, len: *i32) i32
extern fn read(fd: i32, buf: *u8, count: i64) i64
extern fn write(fd: i32, buf: *u8, count: i64) i64

fn main() i64 {
    let fd = socket(2, 1, 0)    // AF_INET, SOCK_STREAM
    // ... bind, listen, accept loop ...
}
```

Compiled with: `cot server.cot -o server && ./server`

This is identical to how Zig, Rust, and Go call libc. The `extern fn` becomes an undefined symbol `_socket`, `_bind`, etc. The linker (`zig cc -lSystem` on macOS, `zig cc -lc` on Linux) resolves them against the system's libc.

On **Wasm** (`--target=wasm32`):
The same `extern fn` declarations generate Wasm import entries. The host runtime (e.g., a WASI runtime like Wasmtime, or custom JavaScript glue) provides the implementations at module instantiation.

---

## Self-Hosting: What Changes

When the Cot compiler is rewritten in Cot:

### What stays the same
- The 9-stage pipeline is identical
- Each stage ports from the same reference implementation
- `extern fn` works the same way (libc on native, imports on Wasm)
- The allocator is self-contained (no extern needed)

### What the self-hosted compiler looks like

```
cot compiler.cot -o cot        # Build the compiler itself
./cot server.cot -o server     # Use it to compile user programs
./server                        # Run the compiled program
```

The compiler is a **native executable** that:
1. Reads .cot source files
2. Runs all 9 pipeline stages
3. Writes a .o file
4. Shells out to a linker (`cc`, `ld`, or `lld`) to produce the final executable

**The compiler itself** would use `extern fn` for:
- File I/O (reading .cot files, writing .o files)
- Process spawning (calling the linker)
- Console output (error messages)

**The compiler does NOT need extern for:**
- Memory allocation (built-in allocator)
- String manipulation (Cot stdlib)
- Data structures (List, Map written in Cot)

### The standard library

The stdlib is **written in Cot** and compiled alongside user code:

```cot
// std/list.cot
struct List(T) {
    items: *T,
    count: i64,
    capacity: i64,
}

fn list_new(T)() List(T) { ... }
fn list_append(T)(list: *List(T), value: T) { ... }
```

The stdlib uses the **built-in allocator** (`cot_alloc` / future `cot_realloc`), not extern. It's self-contained Cot code.

For host interop (files, network, etc.), the stdlib would have **platform modules** that use `extern fn`:

```cot
// std/fs.cot (native platform module)
extern fn open(path: *u8, flags: i32) i32
extern fn read(fd: i32, buf: *u8, count: i64) i64

fn readFile(path: []u8) List(u8) { ... }
```

---

## Current Gaps for the Self-Hosting Path

| Gap | Blocks | Reference to Port | Effort |
|-----|--------|-------------------|--------|
| **Allocator upgrade** (free/realloc) | List(T), Map(K,V), real apps | Go `runtime/mem_wasm.go`, Zig `WasmAllocator.zig` | Medium |
| **`memory.grow` on native** | Allocator growth on native target | Wasmtime `runtime/src/memory.rs` | Medium |
| **Extern fn Wasm imports** | Host interop on Wasm target | Go `link/internal/wasm/asm.go:154-334` | Small |
| **Extern fn native symbols** | Host interop on native target | Cranelift-object `backend.rs` (colocated=false) | Small |
| **Imports on native** | Multi-file compilation on native | N/A (frontend concern) | Medium |
| **Built-in linker** (optional) | Remove `zig cc` dependency | Go `cmd/link` or Zig's linker | Very Large |

---

## Quick Reference: Which Language to Port From

| When working on... | Port from... | Reference location |
|--------------------|-------------|-------------------|
| Frontend (parser, checker) | Zig language spec | `~/learning/zig/` |
| SSA construction | Go compiler | `~/learning/go/src/cmd/compile/internal/ssa/` |
| Wasm codegen | Go Wasm backend | `~/learning/go/src/cmd/compile/internal/wasm/` |
| Wasm linking | Go Wasm linker | `~/learning/go/src/cmd/link/internal/wasm/` |
| Wasm import handling | Go Wasm linker | `~/learning/go/src/cmd/link/internal/wasm/asm.go:154-334` |
| ARC runtime | Swift | Swift `stdlib/public/runtime/HeapObject.cpp` |
| Wasm → CLIF translation | Cranelift | `~/learning/wasmtime/crates/cranelift/src/translate/` |
| CLIF IR types | Cranelift | `~/learning/wasmtime/cranelift/codegen/src/ir/` |
| Machine lowering | Cranelift | `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/` |
| Register allocation | regalloc2 | `~/learning/regalloc2/src/` |
| Code emission | Cranelift | `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/inst/emit.rs` |
| Object file generation | cranelift-object | `~/learning/wasmtime/cranelift/object/src/backend.rs` |
| Memory allocator | Go + Zig | Go `runtime/mem_wasm.go`, Zig `lib/std/heap/WasmAllocator.zig` |
| Extern fn (native) | Cranelift-object | `~/learning/wasmtime/cranelift/object/src/backend.rs` |
| Extern fn (Wasm) | Go linker | `~/learning/go/src/cmd/link/internal/wasm/asm.go` |
