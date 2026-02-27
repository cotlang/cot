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
              ┌─────────────────┼─────────────────┐
              │                                   │
    ┌─────────▼─────────┐             ┌───────────▼───────────┐
    │ --target=wasm32    │             │ --target=native       │
    │                    │             │ (DEFAULT)              │
    │  STAGE 2: WASM     │             │                        │
    │  SSA → Wasm ops    │ Ref: Go     │  STAGE 3: CLIF IR      │ Ref: cg_clif
    │  → .wasm file      │             │  SSA → CLIF IR         │
    │  Done.             │             └───────────┬───────────┘
    └────────────────────┘                         │
                                       ┌───────────▼───────────┐
                                       │  STAGE 4: LOWERING     │ Ref: Cranelift
                                       │  CLIF → MachInst       │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 5: REGALLOC     │ Ref: regalloc2
                                       │  VReg → PReg           │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 6: EMIT         │ Ref: Cranelift
                                       │  MachInst → bytes      │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 7: OBJECT FILE  │ Ref: cranelift-object
                                       │  Mach-O or ELF .o      │
                                       └───────────┬───────────┘
                                                   │
                                       ┌───────────▼───────────┐
                                       │  STAGE 8: LINK         │ External: zig cc
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
- `references/go/src/cmd/compile/internal/ssa/` — SSA value, block, function types
- `references/go/src/cmd/compile/internal/ssa/rewritegeneric.go` — algebraic rewrites
- `references/go/src/cmd/compile/internal/ssa/rewritedec.go` — compound type decomposition

### Stage 2: Wasm Codegen (SSA → Wasm bytecode) — Wasm target only

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
- This stage runs ONLY for Wasm targets (`--target=wasm32`)

**Key files in Go reference:**
- `references/go/src/cmd/compile/internal/wasm/ssa.go` — SSA op → Wasm instruction mapping
- `references/go/src/cmd/internal/obj/wasm/wasmobj.go` — Wasm binary format encoding
- `references/go/src/cmd/link/internal/wasm/asm.go` — Wasm section layout, import handling

**Wasm uses WasmGC** — structs are GC-managed objects (`struct.new`, `struct.get`, `struct.set`), not ARC. Linear memory allocation functions (`cot_alloc`, `cot_dealloc`, `cot_realloc`) are still generated as Wasm bytecode by `arc.zig` for string buffers and List backing arrays, but `cot_retain`/`cot_release` are dead code on Wasm.

| Runtime Function | Target | Reference | What It Does |
|-----------------|--------|-----------|--------------|
| `cot_alloc` | Native + Wasm | Swift `swift_allocObject` + Go `sbrk` | Freelist-first allocator. Native: 24-byte header `[total_size:i64][metadata:i64][refcount:i64]`. Wasm: linear memory for string/List buffers |
| `cot_dealloc` | Native + Wasm | Swift `swift_deallocObject` | Return freed block to freelist for reuse |
| `cot_realloc` | Native + Wasm | C `realloc` semantics | Shrink in-place or alloc+copy+dealloc. Used by growable containers |
| `cot_retain` | Native only | Swift `swift_retain` (HeapObject.cpp:476) | Increment refcount (null-safe, immortal-safe) |
| `cot_release` | Native only | Swift `swift_release` (HeapObject.cpp:835) | Decrement refcount, call destructor at zero, then dealloc |

**Networking runtime functions** are generated as WASI stubs (return -1) by `wasi_runtime.zig` for Wasm targets. On native, they are generated as CLIF IR by `io_native.zig` and call libc directly.

| Net Function | Syscall (macOS/Linux) | What It Does |
|-------------|----------------------|--------------|
| `cot_net_socket` | 97/41 | Create TCP socket (AF_INET, SOCK_STREAM, IPPROTO_TCP) |
| `cot_net_bind` | 104/49 | Bind socket to address |
| `cot_net_listen` | 106/50 | Start listening with backlog |
| `cot_net_accept` | 30/43 | Accept connection |
| `cot_net_connect` | 98/42 | Connect to remote address |
| `cot_net_set_reuse_addr` | 105/54 (setsockopt) | Set SO_REUSEADDR on socket |

### Stage 3: SSA → CLIF IR Translation (native path)

**Reference: rustc_codegen_cranelift (cg_clif)**

| Component | Reference | Location |
|-----------|-----------|----------|
| SSA → CLIF translator | `rustc_codegen_cranelift/src/base.rs` | `compiler/codegen/native/ssa_to_clif.zig` |
| Value/Place model | `rustc_codegen_cranelift/src/value_and_place.rs` | `compiler/codegen/native/ssa_to_clif.zig` |
| CLIF types | `cranelift/codegen/src/ir/types.rs` | `compiler/codegen/native/ir/clif/types.zig` |
| DFG (data flow graph) | `cranelift/codegen/src/ir/dfg.rs` | `compiler/codegen/native/ir/clif/dfg.zig` |
| SSA frontend | `cranelift/frontend/src/frontend.rs` | `compiler/codegen/native/frontend/frontend.zig` |

**What this stage does:**
- Iterates SSA blocks and values, translating each SSA op to CLIF IR instructions
- Uses `FunctionBuilder` for SSA construction with automatic block parameter insertion
- Handles all Cot-specific operations: ARC retain/release calls, struct layout, closures, error unions, etc.
- Maps SSA values to CLIF values via `value_map` and the Variable system for cross-block SSA

**Runtime functions as CLIF IR:** Unlike the Wasm path (which generates runtime functions as Wasm bytecode), the native path generates runtime functions directly as CLIF IR:

| Module | Functions | What It Does |
|--------|-----------|--------------|
| `arc_native.zig` | alloc, dealloc, realloc, retain, release | ARC memory management via libc malloc/free |
| `io_native.zig` | fd_write, fd_read, fd_close, fd_open, fd_seek, time, random, net_*, fork, waitpid, pipe, execve | I/O and process ops via libc |
| `print_native.zig` | print_int, print_string, eprint_*, println_*, int_to_string | Console output via libc write() |
| `test_native.zig` | __test_print_name, __test_pass, __test_fail, __test_summary | Test runner via libc write() |

These CLIF IR functions call libc via undefined symbols (e.g., `write`, `read`, `__open`, `malloc`, `free`) that are resolved at link time by `zig cc`.

**Key cg_clif files:**
- `references/rust/compiler/rustc_codegen_cranelift/src/base.rs` — Core MIR → CLIF translation loop
- `references/rust/compiler/rustc_codegen_cranelift/src/value_and_place.rs` — How values live in regs vs memory
- `references/rust/compiler/rustc_codegen_cranelift/src/pointer.rs` — Load/store offset handling

### Stage 4: Machine Instruction Lowering (native path)

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

### Stage 5: Register Allocation (native path)

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

### Stage 6: Code Emission (native path)

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

### Stage 7: Object File Generation (native path)

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

### Stage 8: Linking (external tool)

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
// references/go/src/cmd/link/internal/wasm/asm.go:154-181
for _, fn := range ctxt.Textp {
    relocs := ldr.Relocs(fn)
    for ri := 0; ri < relocs.Count(); ri++ {
        r := relocs.At(ri)
        if r.Type() == objabi.R_WASMIMPORT {
            // Collect into hostImports list
        }
    }
}

// references/go/src/cmd/link/internal/wasm/asm.go:316-334
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
1. Frontend: extern fn parsed, checked, lowerer skips body
2. SSA → CLIF: `ssa_to_clif.zig` emits `call` to an external function name (non-colocated)
3. Object file: Emits undefined symbol for the external function
4. Linker (`zig cc`): Resolves undefined symbol against libc

Extern functions become undefined symbols in the object file via `declareExternalName` in `driver.zig`. The Mach-O linker adds an underscore prefix (`write` → `_write`). The system linker resolves them against libc.

**Important:** Some libc functions are variadic (e.g., `open(const char *, int, ...)`). On Apple ARM64, variadic arguments go on the stack, not in registers. CLIF always passes arguments in registers. Workaround: use the non-variadic internal wrapper (e.g., `__open` instead of `open`).

---

## Memory Allocation: How It Works

### The current allocator (`cot_alloc`)

**Reference: Swift's `swift_allocObject` (simplified)**

`cot_alloc` is a **freelist allocator**. On Wasm it is generated as Wasm bytecode by `arc.zig` (used for string buffers and List backing arrays — structs use WasmGC). On native it is generated as CLIF IR by `arc_native.zig` (used for all heap allocations including ARC objects). It:
1. Checks the freelist for a reusable block (first-fit)
2. If no block found, bumps the heap pointer (with `memory.grow` on Wasm — Go's `sbrk` pattern)
3. Writes a header: native 24-byte `[total_size:i64][metadata:i64][refcount:i64]`, Wasm 16-byte `[total_size:i32][metadata:i32][refcount:i64]`
4. Returns pointer to user data after the header
5. On dealloc, pushes block onto freelist for reuse

**Memory layout:**
```
Wasm linear memory:
┌──────────────┬───────────────────┬─────────────────────────────┐
│  Stack       │   Data segments   │   Heap (freelist + bump)    │
│  (grows ↓)   │   (null sentinel) │   (grows via memory.grow)   │
└──────────────┴───────────────────┴─────────────────────────────┘
                                    ^
                                    heap_ptr + freelist_head globals

Allocation header (16 bytes):
┌─────────────┬─────────────┬──────────────┐
│ total_size  │ metadata    │   refcount   │
│   (i32)     │   (i32)     │    (i64)     │
│ offset 0    │ offset 4    │   offset 8   │
└─────────────┴─────────────┴──────────────┘
│← USER_DATA_OFFSET = 16 →│
```

### How it works on native

On native, ARC runtime functions (`alloc`, `dealloc`, `realloc`, `retain`, `release`) are generated directly as CLIF IR by `arc_native.zig`. They use libc `malloc`/`free` for heap allocation instead of the Wasm freelist allocator. The native ARC header is 24 bytes: `[total_size:i64][metadata:i64][refcount:i64]`. (Wasm does not use ARC — structs are WasmGC-managed.)

The native binary has a 16MB pre-allocated vmctx region for global variables. Global variables use a fixed 16-byte stride (Cranelift's `VMGlobalDefinition` pattern). Init values are written to vmctx_data before execution (Cranelift's `initialize_globals` pattern).

### ARC Runtime (COMPLETE)

The allocator supports full memory management:

- **Freelist**: Freed blocks added to singly-linked list for reuse (first-fit)
- **`cot_dealloc`**: Returns blocks to freelist (Swift's `swift_deallocObject` pattern)
- **`cot_realloc`**: Shrink in-place or alloc+copy+dealloc (C `realloc` semantics)
- **`memory.grow`**: Go's `sbrk` pattern on Wasm linear memory (pages), Cranelift inline pattern on native (bounds check against pre-allocated 16MB)
- **`@alloc(size)`/`@dealloc(ptr)`/`@realloc(ptr, size)`**: Builtins for manual memory control
- **Null sentinel**: 8 bytes of zeros at data offset 0 (C convention — metadata_ptr=0 is distinguishable from valid metadata)
- **Deinit/destructors**: `TypeName_deinit` functions called via `call_indirect` when refcount reaches 0

This enables `List(T)`, `Map(K,V)`, and all dynamic data structures.

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

On the **native path**, ARC uses libc `malloc`/`free` (via `arc_native.zig`). The allocator is generated as CLIF IR, not compiled from Wasm. Native uses a pre-allocated 16MB vmctx for global variables.

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
    var fd = socket(2, 1, 0)    // AF_INET, SOCK_STREAM
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
- The 8-stage pipeline is identical
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
2. Runs all 8 pipeline stages
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

## Non-Obvious Patterns (Things That Look Wrong But Aren't)

These patterns have confused Claude repeatedly. Read before debugging.

### Frontend & SSA

1. **STRING is internally a slice.** `type_registry.get(STRING)` returns `.slice { .elem = U8 }` (16 bytes: ptr + len). String loads produce `slice_make`, not `string_make`. Both `rewritedec.zig` and `decompose.zig` must check BOTH `string_make` AND `slice_make` when extracting ptr/len.

2. **SSA pass ordering matters.** The correct order is: `rewritegeneric` → `decompose` → `rewritedec` → `schedule` → `layout` → `lower_wasm`. Rewritegeneric must run before decompose (const_string → string_make). Decompose must run before rewritedec (phi decomposition before extraction).

3. **Cleanup stack is unified (Swift's CleanupManager).** ARC releases and defers share one LIFO stack. Scope depth marks the boundary. Break/continue emit cleanups without popping (other paths still need them). Return captures value into `__ret_tmp` BEFORE running cleanups (Zig semantics).

4. **`_deinit` suffix is reserved.** `driver.zig` scans ALL function names ending in `_deinit` and registers them as ARC destructors. Generic impl methods like `List(i64)_deinit` will be falsely detected. Use `free()` instead of `deinit()` for collection cleanup methods.

5. **Multi-file generics need fresh expr_types.** Each generic instantiation's re-check needs isolated `expr_types` because NodeIndex spaces from different ASTs collide. Generic instance symbols must be defined in `global_scope` (not function-local scope). `lowerQueuedGenericFunctions` must snapshot the map before processing (re-check can trigger new instantiations).

### Wasm Codegen

6. **br_table dispatch loop is intentional.** Go wraps functions with calls in: `loop $entryPointLoop { block $b0 { ... br_table } }`. All block-to-block jumps route through a single dispatch loop. This is NOT a bug. Read `claude/BR_TABLE_ARCHITECTURE.md`.

7. **Local variable offsets sum actual sizes.** `getLocalOffset()` sums `local_sizes` array entries — it does NOT multiply slot index by 8. String locals are 16 bytes. Two strings at slots 0,1 have offsets 0 and 16, not 0 and 8.

8. **OnWasmStack optimization.** Values with a single use as block control skip local allocation and are generated inline. Go's `wasm/ssa.go:299-304` does the same. Stack balance tracking must stay in sync between allocation and generation.

9. **Function indices offset by import count.** Import functions occupy indices 0..N-1. Native function indices start at N. Export section and element table must add import_count to the native function index.

10. **Metadata address resolution.** `metadata_addr` SSA op stores a symbolic type name in `aux.string`. Resolved to a memory address during Wasm codegen via `metadata_offsets` map lookup. Returns 0 (null sentinel) if no destructor exists.

### Native AOT

11. **VCode builds backward.** CLIF → MachInst lowering emits instructions in reverse order (use-before-def pattern, matching Cranelift). `VCodeBuilder` builds backward, then `reverseAndFinalize()` flips arrays and adjusts block range indices by `n_insts`.

12. **vmctx save MUST precede Args instruction.** `genArgSetup` must emit `mov x21, x0` BEFORE the `Args` pseudo-instruction, not after. Regalloc inserts edits around `Args` that move params to VRegs — if param is moved to x0, it clobbers vmctx.

13. **br_table requires edge splitting.** br_table cannot carry arguments in CLIF IR. When targets need block params: create intermediate blocks, emit br_table to intermediates, then jump to real targets with args. Also must deduplicate predecessor blocks (Cranelift's `EntitySet<Block>` pattern).

14. **Clobbered register class check.** When collecting callee-saved clobbers from regalloc output, must check `preg.class() == .int`. Float PReg d22 (class=float, hw_enc=22) is NOT the same as int x22 (class=int, hw_enc=22). Without this check: stack corruption.

15. **Two-pass function declaration.** Object file generation must declare ALL functions first, then define ALL functions. Forward references (function A calls function B at higher index) fail if B isn't declared when A's relocations are processed. Matches Cranelift's `declare_function` / `define_function` separation.

16. **AOT call_indirect filters by type_index.** The element table may contain functions with different signatures. When generating the if-else dispatch chain, only emit branches for entries where `func_to_type[func_idx] == expected_type_index`. Otherwise: branch argument count mismatch → regalloc panic.

### Regalloc (regalloc2 Port)

17. **Bit-packed types must match exactly.** PReg = 8 bits (class:2 | hw_enc:6). VReg = 32 bits (vreg:21 | class:2). Operand = 32 bits (constraint:7 | kind:1 | pos:1 | class:2 | vreg:21). ProgPoint = 32 bits (inst:31 | pos:1). Any mismatch causes silent corruption.

18. **`observeVregClass` must be called for every VReg.** Without this, `VRegData.class` stays null and `merge.zig` defaults ALL VRegs to `.int`. Float VRegs silently become int → wrong physical registers assigned → crash on emit.

19. **Spill weight uses bfloat16 encoding.** `toBits()` takes top 16 bits of f32. Precision loss is acceptable — enables compact 16-bit storage in `Use.weight`.

20. **Live ranges must be reversed after building.** Built in reverse block order (matching backward VCode). Must reverse per-VReg ranges before allocation. Non-contiguous ranges are merged if adjacent.

---

## Rust → Zig Type Mappings (regalloc2 / Cranelift Port)

| Rust | Zig |
|------|-----|
| `Vec<T>` | `std.ArrayListUnmanaged(T)` |
| `FxHashMap<K,V>` | `std.AutoHashMapUnmanaged(K,V)` |
| `FxHashSet<T>` | `std.AutoHashMapUnmanaged(T, void)` |
| `SmallVec<[T; N]>` | `std.ArrayListUnmanaged(T)` |
| `EntityMap<E,T>` (sparse) | `SecondaryMap(E,T)` (dense, with default) |
| `u64` bitset | `u64` (manual bit ops) |
| `trait VCodeInst` | `comptime type I` |
| `trait LowerBackend` | `isLowerBackend(T: type)` with `@hasDecl` |

---

## Quick Reference: Which Language to Port From

| When working on... | Port from... | Reference location |
|--------------------|-------------|-------------------|
| Frontend (parser, checker) | Zig language spec | `references/zig/` |
| SSA construction | Go compiler | `references/go/src/cmd/compile/internal/ssa/` |
| SSA → CLIF (native) | rustc_codegen_cranelift | `references/rust/compiler/rustc_codegen_cranelift/src/` |
| Wasm codegen | Go Wasm backend | `references/go/src/cmd/compile/internal/wasm/` |
| Wasm linking | Go Wasm linker | `references/go/src/cmd/link/internal/wasm/` |
| Wasm import handling | Go Wasm linker | `references/go/src/cmd/link/internal/wasm/asm.go:154-334` |
| ARC runtime (Wasm) | Swift | Swift `stdlib/public/runtime/HeapObject.cpp` |
| ARC runtime (native) | Swift + libc | `arc_native.zig` → libc malloc/free |
| CLIF IR types | Cranelift | `references/wasmtime/cranelift/codegen/src/ir/` |
| Machine lowering | Cranelift | `references/wasmtime/cranelift/codegen/src/isa/aarch64/` |
| Register allocation | regalloc2 | `references/regalloc2/src/` |
| Code emission | Cranelift | `references/wasmtime/cranelift/codegen/src/isa/aarch64/inst/emit.rs` |
| Object file generation | cranelift-object | `references/wasmtime/cranelift/object/src/backend.rs` |
| Extern fn (native) | Cranelift-object | `references/wasmtime/cranelift/object/src/backend.rs` |
| Extern fn (Wasm) | Go linker | `references/go/src/cmd/link/internal/wasm/asm.go` |
