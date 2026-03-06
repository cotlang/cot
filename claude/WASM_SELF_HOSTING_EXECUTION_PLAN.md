# Wasm Self-Hosting Execution Plan

**Date:** Mar 6, 2026
**Goal:** `selfcot build selfcot.cot --target=wasm32` produces a working `.wasm` binary.
**Current state:** Frontend complete (25K lines). No codegen, no SSA passes, no driver.

---

## Pipeline: What Needs to Exist

```
Source → Scanner → Parser → Checker → Lower → SSA Builder → ARC Insertion
  ↓ DONE (25K lines in self/)
SSA Passes: rewritegeneric → decompose → rewritedec → lower_wasm → schedule → layout
  ↓ THIS PLAN — Phase 1 (~2,400 lines)
Wasm Codegen: wasm_gen (SSA → Wasm instructions)
  ↓ THIS PLAN — Phase 2 (~2,200 lines)
Wasm Binary: wasm/ (bytecode emission, linking, assembly)
  ↓ THIS PLAN — Phase 3 (~4,900 lines)
Wasm Runtime: arc.zig, wasi_runtime.zig, slice_runtime.zig, print_runtime.zig, test_runtime.zig
  ↓ THIS PLAN — Phase 4 (~5,700 lines)
Driver: Multi-file orchestration, function index map, pass pipeline
  ↓ THIS PLAN — Phase 5 (~3,500 lines)
CLI: build/test/run commands, --target flag
  ↓ THIS PLAN — Phase 6 (~2,500 lines)
```

**Total new code:** ~21,200 lines of Cot

---

## Prerequisite: Expand ssa.cot with Wasm Ops

**Before ANY pass can be ported**, `ssa.cot` must include the full Wasm-specific op set.

Currently `ssa.cot` has ~100 generic ops. The Zig `op.zig` (713 lines) has ~350+ ops including:
- Wasm-specific ops (~160): `wasm_i64_add`, `wasm_i64_load`, `wasm_f64_const`, etc.
- Sized ops (~80): `add32`, `sub32`, `const_32`, `load8s`, etc.
- Lowered ops: `wasm_lowered_move`, `wasm_lowered_static_call`, etc.

**Action:** Add ALL Wasm ops and sized ops to `SsaOp` enum in `ssa.cot`. Skip ARM64/AMD64 ops (native-only). Estimated: ~100 new enum variants, ~50 lines.

Also add missing fields to `SsaValue`:
- `aux_float: f64` (already there? verify)
- Anything `lower_wasm.zig` reads (e.g., checking `v.type_idx` for float detection)

---

## Phase 1: SSA Passes (~2,400 lines)

Port 6 SSA passes from `compiler/ssa/passes/` to `self/ssa/passes/`. These run in order on each `SsaFunc` before codegen.

### File 1.1: `rewritegeneric.cot` (from `rewritegeneric.zig`, 154 lines → ~180 lines)

**Purpose:** Transform generic-level ops before lowering. E.g. `const_string → string_make`.

**Zig reference:** `compiler/ssa/passes/rewritegeneric.zig`
**Cot patterns:**
- Import `ssa` module for `SsaOp`, `SsaFunc`, `SsaValue`
- Single `fn rewrite(func: *SsaFunc) void` entry point
- Iterate `func.blocks`, iterate `block.values`, match on `val.op`
- Switch statement mapping ops (Cot switch = Zig switch, 1:1)

```cot
import "ssa"

fn rewrite(func: *SsaFunc) void {
    for i in 0..func.blocks.count {
        var blk = func.getBlock(i)
        for j in 0..blk.values.count {
            const vi = blk.values.get(j)
            var val = func.getValue(vi)
            rewriteValue(&val, func)
            func.setValue(vi, val)
        }
    }
}

fn rewriteValue(val: *SsaValue, func: *SsaFunc) void {
    switch (val.op) {
        SsaOp.const_string => {
            val.op = SsaOp.string_make
        }
        // ... other rewrites
        else => {}
    }
}
```

**Dogfooding features:** `for..in` range, switch on enum, pointer params (`*SsaValue`), method calls on structs.

---

### File 1.2: `decompose.cot` (from `decompose.zig`, 273 lines → ~300 lines)

**Purpose:** Decompose phi nodes on compound types (slices, strings) into component phis. A phi on a `[]T` becomes separate phis for `ptr`, `len`, `cap`.

**Zig reference:** `compiler/ssa/passes/decompose.zig`
**Key logic:**
- Walk all blocks, find phi values with compound types
- For each compound phi, create 2-3 new phis (ptr, len, cap)
- Rewrite users to reference the new component values
- Uses `TypeRegistry` to check if type is slice/string

**Cot patterns:**
- `import "types"` for `TypeRegistry`
- Conditional phi splitting based on type
- Creating new values via `func.newValue(...)`

---

### File 1.3: `rewritedec.cot` (from `rewritedec.zig`, 654 lines → ~700 lines)

**Purpose:** Decompose compound type extractions. `slice_ptr(load(addr))` → direct load from known offset. `string_len(x)` → load from offset 8 of string struct.

**This is the LARGEST pass** — lots of pattern matching on op combinations.

**Zig reference:** `compiler/ssa/passes/rewritedec.zig`
**Key logic:**
- Pattern match on `slice_ptr`, `slice_len`, `slice_cap`, `string_ptr`, `string_len`
- If argument is a `load`, decompose to offset-aware load
- If argument is a `slice_make`/`string_make`, extract the component directly
- String concat decomposition: `string_concat(a, b)` → `static_call("__string_concat", a_ptr, a_len, b_ptr, b_len)` + `string_make(result_ptr, result_len)`

**Cot patterns:**
- Nested switch matching: `switch (val.op) { SsaOp.slice_ptr => { ... match val.args ... } }`
- Creating replacement values with `func.newValue()`
- Offset calculations for struct field positions

---

### File 1.4: `lower_wasm.cot` (from `lower_wasm.zig`, 576 lines → ~600 lines)

**Purpose:** Map generic ops to Wasm-specific ops. `add → wasm_i64_add`, `load → wasm_i64_load`, etc.

**Zig reference:** `compiler/ssa/passes/lower_wasm.zig` (read in full above)
**This is a HUGE switch statement** — ~200 arms mapping generic → wasm ops.

**Cot pattern — the core is a massive switch:**
```cot
fn lowerValue(val: *SsaValue, func: *SsaFunc) bool {
    const new_op: ?SsaOp = switch (val.op) {
        SsaOp.const_int => SsaOp.wasm_i64_const,
        SsaOp.add => SsaOp.wasm_i64_add,
        SsaOp.sub => SsaOp.wasm_i64_sub,
        // ... ~200 more arms
        SsaOp.load => {
            if (isFloatType(val.type_idx)) { return SsaOp.wasm_f64_load }
            return SsaOp.wasm_i64_load
        }
        else => null,
    }
    if (new_op) |op| {
        val.op = op
        return true
    }
    return false
}
```

**NOTE:** Optional capture `if (x) |val|` is idiomatic Cot. This pass is almost pure data mapping — perfect for dogfooding enum matching.

**Dogfooding features:** Optional capture `|op|`, large switch, enum comparison, helper functions.

---

### File 1.5: `schedule.cot` (from `schedule.zig`, 264 lines → ~280 lines)

**Purpose:** Order values within each block for emission. Priority-based topological sort — values that are used earlier get emitted first. Memory operations preserve order.

**Zig reference:** `compiler/ssa/passes/schedule.zig`
**Key logic:**
- Compute `uses` count for each value
- Topological sort within each block respecting data dependencies
- Memory ops (loads/stores) maintain program order
- Result: `block.values` reordered for optimal emission

**Cot patterns:**
- `List(int)` as work queue
- Counting passes over value lists
- In-place reordering of `block.values`

---

### File 1.6: `layout.cot` (from `layout.zig`, 291 lines → ~310 lines)

**Purpose:** Order blocks to minimize jumps. Place fallthrough blocks adjacent. Place hot paths first, cold paths (error handling) last.

**Zig reference:** `compiler/ssa/passes/layout.zig`
**Key logic:**
- Start from entry block
- For `if_` blocks, place the "then" successor immediately after
- Use visited set to avoid cycles
- Result: `func.blocks` reordered for linear emission

**Cot patterns:**
- `Set(int)` for visited blocks (dogfooding `std/set`)
- `List(int)` for output order
- Block traversal via `blk.succs`

---

### Phase 1 Directory Structure

```
self/
  ssa/
    passes/
      rewritegeneric.cot    (~180 lines)
      decompose.cot          (~300 lines)
      rewritedec.cot         (~700 lines)
      lower_wasm.cot         (~600 lines)
      schedule.cot           (~280 lines)
      layout.cot             (~310 lines)
  frontend/
    ssa.cot                  (EXPAND: add Wasm ops, ~550 lines total)
    ... (existing files unchanged)
```

**Testing:** Each pass gets unit tests operating on manually-constructed `SsaFunc` instances. E.g.:
```cot
test "lower_wasm: add becomes wasm_i64_add" {
    var func = SsaFunc.init("test")
    const blk = func.newBlock(SsaBlockKind.plain)
    const v = func.newValue(SsaOp.add, I64, blk, 0, 0)
    func.addValueToBlock(v, blk)
    lower(&func)
    @assertEq(func.getValue(v).op == SsaOp.wasm_i64_add, true)
}
```

---

## Phase 2: Wasm Code Generator (~2,200 lines)

Port `compiler/codegen/wasm_gen.zig` (2,244 lines) → `self/codegen/wasm_gen.cot`.

### File 2.1: `wasm_gen.cot` (from `wasm_gen.zig`, 2,244 lines → ~2,200 lines)

**Purpose:** Walk the scheduled/laid-out SSA function and emit Wasm bytecode. This is the heart of Wasm codegen — each `SsaOp` becomes one or more Wasm instructions.

**Zig reference:** `compiler/codegen/wasm_gen.zig`
**Key entry point:** `ssaGenBlock(block, gen)` → iterates values → `ssaGenValue(value, gen)` → emits instructions.

**Core data structure:**
```cot
struct WasmGen {
    code: List(u8),          // Wasm bytecode buffer
    locals: List(int),       // local variable types
    value_to_local: Map(int, int),  // SSA value → Wasm local index
    func_indices: Map(string, int), // function name → index
    type_registry: *TypeRegistry,
    stack_pointer_global: int,

    fn emit(byte: u8) void { self.code.append(byte) }
    fn emitLEB128(val: int) void { ... }  // LEB128 encoding
    fn getValue64(v_idx: int, func: *SsaFunc) void { ... }
    fn getValue32(v_idx: int, func: *SsaFunc) void { ... }
    fn setReg(v_idx: int, func: *SsaFunc) void { ... }
    fn ssaGenValue(v_idx: int, func: *SsaFunc) void { ... }
    fn ssaGenBlock(blk_idx: int, func: *SsaFunc) void { ... }
}
```

**The main switch** in `ssaGenValue` handles each Wasm op:
```cot
fn ssaGenValue(v_idx: int, func: *SsaFunc) void {
    var val = func.getValue(v_idx)
    switch (val.op) {
        SsaOp.wasm_i64_const => {
            self.emit(0x42)  // i64.const
            self.emitSignedLEB128(val.aux_int)
        }
        SsaOp.wasm_i64_add => {
            self.getValue64(val.args.get(0), func)
            self.getValue64(val.args.get(1), func)
            self.emit(0x7C)  // i64.add
            self.setReg(v_idx, func)
        }
        // ... ~100 more arms
    }
}
```

**Block emission** handles control flow:
- `plain` blocks: emit values, then `br` to successor
- `if_` blocks: emit condition, `br_if` to then-block, `br` to else-block
- `ret` blocks: emit return value, `return`
- `jump_table` blocks: `br_table` (the Go dispatch loop pattern)

**Dogfooding features:** Large switch, `List(u8)` as byte buffer, `Map(string, int)` for indices, method-heavy struct, bitwise ops for LEB128 encoding.

---

## Phase 3: Wasm Binary Emission (~4,900 lines)

Port `compiler/codegen/wasm/` (7 files) → `self/codegen/wasm/`.

### File 3.1: `wasm_types.cot` (from `wasm.zig`, 175 lines → ~180 lines)

**Purpose:** Wasm type definitions — value types, section IDs, opcodes as constants.

```cot
// Wasm value types
const WASM_I32: u8 = 0x7F
const WASM_I64: u8 = 0x7E
const WASM_F32: u8 = 0x7D
const WASM_F64: u8 = 0x7C
const WASM_FUNCREF: u8 = 0x70
const WASM_EXTERNREF: u8 = 0x6F

// Section IDs
const SECTION_TYPE: u8 = 1
const SECTION_IMPORT: u8 = 2
const SECTION_FUNCTION: u8 = 3
// ...

// Opcodes (subset — most are emitted inline by wasm_gen)
const OP_UNREACHABLE: u8 = 0x00
const OP_NOP: u8 = 0x01
const OP_BLOCK: u8 = 0x02
const OP_LOOP: u8 = 0x03
const OP_IF: u8 = 0x04
const OP_ELSE: u8 = 0x05
const OP_END: u8 = 0x0B
const OP_BR: u8 = 0x0C
const OP_BR_IF: u8 = 0x0D
const OP_BR_TABLE: u8 = 0x0E
const OP_RETURN: u8 = 0x0F
const OP_CALL: u8 = 0x10
const OP_CALL_INDIRECT: u8 = 0x11
```

**Dogfooding:** Global constants with explicit types, no struct needed.

---

### File 3.2: `prog.cot` (from `prog.zig`, 270 lines → ~280 lines)

**Purpose:** Program structure — represents a complete Wasm module during construction.

```cot
struct WasmFunc {
    name: string,
    type_idx: int,
    code: List(u8),
    locals: List(u8),  // local declarations (type bytes)
    is_export: bool,
    is_import: bool,
}

struct WasmProg {
    funcs: List(WasmFunc),
    types: List(List(u8)),    // function type signatures
    data_segments: List(DataSegment),
    globals: List(WasmGlobal),
    memory_pages: int,
    table_size: int,

    static fn init() WasmProg { ... }
    fn addFunc(name: string, type_idx: int) int { ... }
    fn addType(params: List(u8), results: List(u8)) int { ... }
}
```

---

### File 3.3: `constants.cot` (from `constants.zig`, 829 lines → ~850 lines)

**Purpose:** LEB128 encoding/decoding, constant pool management, memarg encoding.

**Key functions:**
- `encodeLEB128(val: int, buf: *List(u8)) void`
- `encodeSignedLEB128(val: int, buf: *List(u8)) void`
- `encodeMemArg(align: int, offset: int, buf: *List(u8)) void`

**Dogfooding:** Bitwise operations (`>>`, `&`, `|`), while loops, `List(u8)` manipulation.

---

### File 3.4: `preprocess.cot` (from `preprocess.zig`, 699 lines → ~720 lines)

**Purpose:** Pre-codegen processing — assign function indices, build import/export tables, compute data segment offsets, build function type table.

**Key logic:**
- Assign sequential indices to all functions (imports first, then locals)
- Build function type signature dedup table
- Compute data segment layout (string literals, global data)
- Build indirect function table for closures/function pointers

---

### File 3.5: `gen.cot` (from `gen.zig`, 1,544 lines → ~1,600 lines)

**Purpose:** Core Wasm binary generation — emit section headers, function bodies, data sections. This is distinct from `wasm_gen.cot` (SSA→instructions) — this file writes the binary MODULE format.

**Key sections emitted:**
1. Type section (function signatures)
2. Import section (WASI imports if any)
3. Function section (type indices)
4. Table section (for call_indirect)
5. Memory section
6. Global section (stack pointer, heap pointer)
7. Export section (main, memory)
8. Element section (indirect call table)
9. Code section (function bodies from wasm_gen)
10. Data section (string literals, constants)

---

### File 3.6: `assemble.cot` (from `assemble.zig`, 609 lines → ~630 lines)

**Purpose:** Final binary assembly — concatenate all sections, compute sizes, write Wasm magic + version, emit final `.wasm` bytes.

```cot
fn assemble(prog: *WasmProg) List(u8) {
    var out: List(u8) = .{}
    // Magic: \0asm
    out.append(0x00); out.append(0x61); out.append(0x73); out.append(0x6D)
    // Version: 1
    out.append(0x01); out.append(0x00); out.append(0x00); out.append(0x00)
    // Sections...
    emitTypeSection(&out, prog)
    emitFunctionSection(&out, prog)
    // ...
    return out
}
```

---

### File 3.7: `link.cot` (from `link.zig`, 789 lines → ~810 lines)

**Purpose:** Multi-file linking — combine function bodies from multiple compilation units, resolve cross-module references, apply relocations.

**Key logic:**
- Merge function tables from multiple files
- Resolve function name → index across files
- Apply relocations for cross-file calls
- Emit final linked module

---

### Phase 3 Directory Structure

```
self/
  codegen/
    wasm_gen.cot         (~2,200 lines)
    wasm/
      wasm_types.cot     (~180 lines)
      prog.cot           (~280 lines)
      constants.cot      (~850 lines)
      preprocess.cot     (~720 lines)
      gen.cot            (~1,600 lines)
      assemble.cot       (~630 lines)
      link.cot           (~810 lines)
```

---

## Phase 4: Runtime Modules (~5,700 lines)

These generate Wasm MODULE FUNCTIONS (not host imports) that implement the runtime.

### File 4.1: `arc_runtime.cot` (from `arc.zig`, 1,559 lines → ~1,600 lines)

**Purpose:** ARC memory management as Wasm functions. Generates: `alloc`, `dealloc`, `retain`, `release`, `realloc`. Uses size-class freelists and bump allocation.

**Cot pattern — generates Wasm bytecode for runtime functions:**
```cot
fn generateAlloc(prog: *WasmProg) void {
    var code: List(u8) = .{}
    // function body: alloc(size: i64) -> i64
    // bump allocator: heap_ptr += aligned_size, return old heap_ptr
    code.append(0x23)  // global.get
    encodeLEB128(HEAP_PTR_GLOBAL, &code)
    // ... more Wasm instructions
    const func_idx = prog.addFunc("alloc", alloc_type_idx)
    prog.funcs.get(func_idx).code = code
}
```

**This is THE critical runtime** — without ARC, nothing works.

---

### File 4.2: `wasi_runtime.cot` (from `wasi_runtime.zig`, 1,275 lines → ~1,300 lines)

**Purpose:** WASI-like runtime functions: `fd_write`, `fd_read`, `fd_open`, `fd_close`, `lseek`, `fstat`, `time`, `exit`, `rename`, `unlink`, `args_get`, `env_get`.

**These are MODULE functions** that call into WASI host imports OR implement functionality directly.

---

### File 4.3: `slice_runtime.cot` (from `slice_runtime.zig`, ~800 lines → ~820 lines)

**Purpose:** Slice operations as Wasm functions: `slice_grow`, `slice_append`, `slice_concat`, `slice_insert`, `slice_remove`. Manages dynamic array memory.

---

### File 4.4: `print_runtime.cot` (from `print_runtime.zig`, ~700 lines → ~720 lines)

**Purpose:** Print functions: `print_int`, `print_float`, `print_string`, `print_bool`, `print_char`, `print_newline`. Integer-to-string conversion, float formatting.

---

### File 4.5: `test_runtime.cot` (from `test_runtime.zig`, ~500 lines → ~520 lines)

**Purpose:** Test runner: `test_start`, `test_pass`, `test_fail`, `test_assert_eq`. Counts pass/fail, prints results.

---

### File 4.6: `bench_runtime.cot` (from `bench_runtime.zig`, ~700 lines → ~720 lines)

**Purpose:** Benchmark runner: `bench_start`, `bench_iter`, `bench_end`. Timer-based iteration, ns/op reporting.

---

## Phase 5: Driver (~3,500 lines)

Port `compiler/driver.zig` (5,913 lines) → `self/driver.cot`. For Wasm-only, skip native codegen (~1,500 lines).

### File 5.1: `driver.cot` (~3,500 lines)

**Purpose:** Orchestrate the full pipeline from source files to `.wasm` binary.

**Key responsibilities:**
1. **Multi-file compilation** — resolve imports, parse all files, check all files
2. **Function index map** — assign global function indices across all files
3. **SSA pass pipeline** — run passes in order on each function
4. **Wasm codegen** — emit bytecode for each function
5. **Runtime registration** — add ARC, WASI, print, test, slice, bench functions
6. **Generic function processing** — instantiate and compile generics from queue
7. **Binary output** — assemble and write `.wasm` file

**Core structure:**
```cot
struct Driver {
    target: Target,
    test_mode: bool,
    test_filter: ?string,
    bench_mode: bool,

    // Compilation state
    parsed_files: List(ParsedFile),
    checked_files: List(CheckedFileEntry),
    func_indices: Map(string, int),
    type_registry: TypeRegistry,
    shared_generic_ctx: SharedGenericContext,

    fn compile(input_path: string) !List(u8) {
        // 1. Parse all files (recursive import resolution)
        self.parseAllFiles(input_path)

        // 2. Type-check all files
        self.checkAllFiles()

        // 3. Lower IR for all files
        var ir_funcs = self.lowerAllFiles()

        // 4. Build SSA for all functions
        var ssa_funcs = self.buildSSA(ir_funcs)

        // 5. Run SSA passes
        for i in 0..ssa_funcs.count {
            var f = ssa_funcs.getPtr(i)
            rewritegeneric.rewrite(f)
            decompose.decompose(f)
            rewritedec.rewrite(f)
            lower_wasm.lower(f)
            schedule.schedule(f)
            layout.layout(f)
        }

        // 6. Generate Wasm
        var prog = WasmProg.init()
        self.addRuntimeFunctions(&prog)
        for i in 0..ssa_funcs.count {
            wasm_gen.generate(ssa_funcs.getPtr(i), &prog)
        }

        // 7. Assemble binary
        return assemble.assemble(&prog)
    }
}
```

**Multi-file import resolution** already exists in `self/main.cot` (384 lines). The driver extends this with:
- `func_indices` map construction
- Cross-file generic instantiation queue
- `SharedGenericContext` for generic state sharing

**Dogfooding features:** Multi-file compilation (the self-hosted compiler compiling itself is multi-file), generic instantiation, error handling with `try`, `Map`/`List`/`Set` from stdlib.

---

## Phase 6: CLI Extension (~2,500 lines)

Extend `self/main.cot` (currently 384 lines) with `build`, `test`, `run` commands.

### File 6.1: `main.cot` extension (~1,800 lines added)

**New commands:**
- `build <file.cot> [--target=wasm32] [-o name]` — compile to `.wasm`
- `test <file.cot> [--target=wasm32]` — compile in test mode, run via wasmtime
- `run <file.cot> [-- args]` — compile, run, clean up

**New components:**
- `--target` flag parsing → `Target` struct
- Output file naming: strip `.cot`, append `.wasm`
- File I/O: write `.wasm` binary to disk
- For `test`/`run`: spawn wasmtime subprocess

### File 6.2: `target.cot` (~100 lines, from `target.zig` 148 lines)

```cot
const Arch = enum { arm64, amd64, wasm32 }
const Os = enum { macos, linux, freestanding, wasi }

struct Target {
    arch: Arch,
    os: Os,

    const wasm32 = Target { arch: Arch.wasm32, os: Os.freestanding }
    const wasm32_wasi = Target { arch: Arch.wasm32, os: Os.wasi }

    fn isWasm() bool { return self.arch == Arch.wasm32 }

    static fn parse(s: string) ?Target {
        if (s == "wasm32") { return Target.wasm32 }
        if (s == "wasm32-wasi" or s == "wasi") { return Target.wasm32_wasi }
        return null
    }
}
```

### File 6.3: `project.cot` (~250 lines, from `project.zig` 352 lines)

```cot
import "std/json"
import "std/fs"

struct Project {
    name: ?string,
    main: ?string,
    safe: bool,
    libs: List(string),
    c_sources: List(string),

    static fn load(path: string) ?Project {
        const content = fs.readFile(path) catch { return null }
        const json = Json.parse(content) catch { return null }
        // ... extract fields
    }
}
```

---

## Porting Order (Dependency-Driven)

```
Step 1: Expand ssa.cot (add Wasm ops)              ~50 lines    | DONE ✅
Step 2: target.cot                                  ~100 lines   | DONE ✅
Step 3: rewritegeneric.cot                          ~180 lines   | DONE ✅ (172 lines, 3 tests)
Step 4: decompose.cot                               ~300 lines   | DONE ✅ (300 lines, 6 tests)
Step 5: rewritedec.cot                              ~700 lines   | DONE ✅ (472 lines, 3 tests)
Step 6: lower_wasm.cot                              ~600 lines   | DONE ✅ (296 lines, 8 tests)
Step 7: schedule.cot                                ~280 lines   | DONE ✅ (254 lines, 3 tests)
Step 8: layout.cot                                  ~310 lines   | DONE ✅ (225 lines, 4 tests)
Step 9: wasm_types.cot + constants.cot              ~1,030 lines | DONE ✅ (649+153 lines, 9 tests)
Step 10: prog.cot                                   ~280 lines   | DONE ✅ (410 lines, 9 tests)
Step 11: wasm_gen.cot + code_builder.cot            ~2,200 lines | DONE ✅ (wasm_gen 1,112L/7 tests + code_builder 439L/7 tests)
Step 12: arc_runtime.cot                            ~1,600 lines | ARC memory management
Step 13: wasi_runtime.cot                           ~1,300 lines | System calls
Step 14: slice_runtime.cot + print_runtime.cot      ~1,540 lines | Slice ops + printing
Step 15: test_runtime.cot + bench_runtime.cot       ~1,240 lines | Test/bench runners
Step 16: preprocess.cot                             ~720 lines   | Pre-codegen processing
Step 17: gen.cot + assemble.cot + link.cot          ~3,040 lines | Binary emission + linking
Step 18: driver.cot                                 ~3,500 lines | Pipeline orchestrator
Step 19: project.cot + main.cot extension           ~2,050 lines | CLI: build/test/run
                                                    ─────────
                                                    ~21,020 lines total
```

---

## Validation Milestones

| Step | Milestone | Validation |
|------|-----------|------------|
| After Step 8 | All SSA passes compile | `cot test self/ssa/passes/*.cot` — unit tests on constructed SSA |
| After Step 11 | Wasm codegen compiles | `cot test self/codegen/wasm_gen.cot` — emit bytecode for simple functions |
| After Step 15 | Runtime compiles | `cot test self/codegen/arc_runtime.cot` — verify runtime function generation |
| After Step 17 | Binary emission works | Generate a `.wasm` from a trivial program, run with `wasmtime` |
| After Step 19 | **Full pipeline** | `selfcot build hello.cot --target=wasm32 && wasmtime hello.wasm` |
| Final | **Self-compilation** | `selfcot build self/main.cot --target=wasm32` produces `selfcot.wasm` |

---

## Key Design Decisions

1. **Index-based SSA** (not pointer-based): Matches existing `ssa.cot` and `ir.cot` patterns. `SsaValue.args` is `List(int)` not `[]*Value`. This is safer in Cot (no raw pointer lifetime issues).

2. **Single SsaOp enum** with Wasm ops inline: No separate `WasmOp` type. The `lower_wasm` pass rewrites `SsaOp.add → SsaOp.wasm_i64_add` in-place. Same as Zig version.

3. **Runtime as Wasm bytecode generators**: Each runtime module (arc, wasi, print, etc.) generates raw Wasm bytecode in `List(u8)`. No intermediate representation — direct byte emission. Copy the Go/Zig pattern exactly.

4. **Copy reference implementations**: Every file maps 1:1 to a Zig source file. Line-by-line port, not invention.

5. **@safe mode** for all self/ files: Auto-ref, implicit self, colon struct init. This is the canonical Cot style.

6. **Use modern Cot features for dogfooding**:
   - `for i in 0..n` range loops (not C-style)
   - `switch` on enums with exhaustive matching
   - Optional capture `if (opt) |val|`
   - Tagged unions for variant types
   - `List(T)`, `Map(K,V)`, `Set(T)` from stdlib
   - Error unions with `try`/`catch`
   - `string` as first-class type (not `[]const u8`)
   - Struct methods with inferred impl
   - `@assertEq` in tests
