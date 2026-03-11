# Wasm Self-Hosting Execution Plan

**Date:** Mar 6, 2026
**Goal:** `selfcot build selfcot.cot --target=wasm32` produces a working `.wasm` binary.
**Current state:** Frontend complete (25K lines). SSA passes + wasm codegen done. Need runtime, binary emission, driver.

---

## Architecture: Wasm = WasmGC, No ARC

**ARC is native-only.** The Wasm target uses WasmGC exclusively — the browser/runtime GC handles object lifetime. There are NO retain/release/alloc/dealloc calls in the Wasm pipeline.

The Wasm runtime provides:
- **Bump allocator** (heap_ptr global) for linear memory (slices, strings, stack frames)
- **WasmGC** for managed object lifetime (structs, closures)
- **WASI imports** for system calls (fd_write, fd_read, exit, time, etc.)

---

## Pipeline: What Exists vs What's Needed

```
Source -> Scanner -> Parser -> Checker -> Lower -> SSA Builder
  | DONE (25K lines in self/)
SSA Passes: rewritegeneric -> decompose -> rewritedec -> lower_wasm -> schedule -> layout
  | DONE (Steps 1-8)
Wasm Codegen: wasm_gen (SSA -> Wasm instructions)
  | DONE (Steps 9-11)
Wasm Binary: bytecode emission, linking, assembly
  | THIS PLAN -- Phase 3 (~3,000 lines)
Wasm Runtime: wasi_runtime, slice_runtime, print_runtime, test_runtime
  | THIS PLAN -- Phase 4 (~4,100 lines)
Driver: Multi-file orchestration, function index map, pass pipeline
  | THIS PLAN -- Phase 5 (~3,500 lines)
CLI: build/test/run commands, --target flag
  | THIS PLAN -- Phase 6 (~2,500 lines)
```

**Remaining new code:** ~13,100 lines of Cot

---

## Completed Steps

| Step | File | Lines | Tests |
|------|------|-------|-------|
| 1 | ssa.cot (expanded with Wasm ops) | ~550 | -- |
| 2 | target.cot | ~100 | -- |
| 3 | rewritegeneric.cot | 172 | 3 |
| 4 | decompose.cot | 300 | 6 |
| 5 | rewritedec.cot | 472 | 3 |
| 6 | lower_wasm.cot | 296 | 8 |
| 7 | schedule.cot | 254 | 3 |
| 8 | layout.cot | 225 | 4 |
| 9 | wasm_types.cot + constants.cot | 649+153 | 9 |
| 10 | prog.cot | 410 | 9 |
| 11 | wasm_gen.cot + code_builder.cot | 1,112+439 | 14 |
| 12 | preprocess.cot | 509 | -- |
| 13 | assemble.cot | 686 | 7 |
| 14 | link.cot | 650 | 7 |
| 15 | wasi_runtime.cot | 932 | 5 |
| 16 | slice_runtime.cot | 251 | 3 |
| 17 | print_runtime.cot + code_builder.cot updates | 909 | 6 |
| 18 | test_runtime.cot + bench_runtime.cot | 538+536 | 10+9 |
| 18b | mem_runtime.cot | 493 | 8 |
| 19 | driver.cot + ssa_passes.cot + ssa_passes_dec.cot | 599+13+11 | 11 |
| 20-21 | main.cot CLI: build, run, test, bench, -o flag | ~120 | -- |
| **Total done** | | **~12,015** | **~142** |

---

## Phase 3: Wasm Binary Emission (~3,000 lines)

Port `compiler/codegen/wasm/` -> `self/codegen/wasm/`.

### Step 12: `preprocess.cot` (from `preprocess.zig`, 699 lines -> ~720 lines)

**Purpose:** Pre-codegen processing -- assign function indices, build import/export tables, compute data segment offsets, build function type table.

Key logic:
- Assign sequential indices to all functions (imports first, then locals)
- Build function type signature dedup table
- Compute data segment layout (string literals, global data)
- Build indirect function table for closures/function pointers

### Step 13: `gen.cot` (from `gen.zig`, 1,544 lines -> ~1,600 lines)

**Purpose:** Core Wasm binary generation -- emit section headers, function bodies, data sections. Distinct from `wasm_gen.cot` (SSA->instructions) -- this writes the binary MODULE format.

Sections emitted:
1. Type section (function signatures)
2. Import section (WASI imports)
3. Function section (type indices)
4. Table section (for call_indirect)
5. Memory section
6. Global section (stack pointer, heap pointer)
7. Export section (main, memory)
8. Element section (indirect call table)
9. Code section (function bodies from wasm_gen)
10. Data section (string literals, constants)

### Step 14: `link.cot` (from `link.zig`, 789 lines -> ~810 lines)

**Purpose:** Multi-file linking -- combine function bodies, resolve cross-module references, apply relocations, emit final linked module.

---

## Phase 4: Wasm Runtime Modules (~4,100 lines)

These generate Wasm MODULE FUNCTIONS (not host imports). **No ARC** -- Wasm uses WasmGC + bump allocator.

### Step 15: `wasi_runtime.cot` (from `wasi_runtime.zig`, 1,275 lines -> ~1,300 lines)

**Purpose:** System call wrappers as Wasm functions: `fd_write`, `fd_read`, `fd_open`, `fd_close`, `fd_seek`, `time`, `exit`, `random`, `args_get`, `environ_get`. Plus networking: `net_socket`, `net_bind`, `net_listen`, `net_accept`, `net_connect`. Plus event loop: `kqueue_create`, `kevent_add/del/wait`, `epoll_create/add/del/wait`. Plus process: `fork`, `exec`.

These are MODULE functions that call into WASI host imports.

### Step 16: `slice_runtime.cot` (from `slice_runtime.zig`, ~800 lines -> ~820 lines)

**Purpose:** Slice operations as Wasm functions: `growslice`, `nextslicecap`. Uses bump allocator (heap_ptr global) for memory allocation -- NOT ARC.

### Step 17: `print_runtime.cot` (from `print_runtime.zig`, ~700 lines -> ~720 lines)

**Purpose:** Print functions: `write` (raw fd_write wrapper), `print_int`, `eprint_int`, `int_to_string`, `print_float`, `eprint_float`, `float_to_string`. Integer-to-string conversion, float formatting.

### Step 18: `test_runtime.cot` + `bench_runtime.cot` (from ~1,200 lines -> ~1,240 lines)

**Purpose:** Test runner (`test_start`, `test_pass`, `test_fail`, `test_assert_eq`) and benchmark runner (`bench_start`, `bench_iter`, `bench_end`).

---

## Phase 5: Driver (~3,500 lines)

### Step 19: `driver.cot` (from `driver.zig` Wasm path, ~5,913 lines -> ~3,500 lines Wasm-only)

**Purpose:** Orchestrate the full pipeline from source files to `.wasm` binary.

Key responsibilities:
1. **Multi-file compilation** -- resolve imports, parse all files, check all files
2. **Function index map** -- assign global function indices (runtime first, then user)
3. **SSA pass pipeline** -- run 6 passes in order on each function
4. **Wasm codegen** -- emit bytecode for each function
5. **Runtime registration** -- add WASI, print, slice, test, bench functions (NO ARC)
6. **Generic function processing** -- instantiate and compile generics from queue
7. **Binary output** -- assemble and write `.wasm` file

Runtime functions registered in func_indices (from driver.zig):
- slice: `growslice`, `nextslicecap`
- print: `write`, `print_int`, `eprint_int`, `int_to_string`, `print_float`, `eprint_float`, `float_to_string`
- wasi: `fd_write`, `fd_write_simple`, `fd_read_simple`, `fd_close`, `fd_seek`, `fd_open`, `time`, `random`, `exit`, `args_count`, `arg_len`, `arg_ptr`, `environ_count`, `environ_len`, `environ_ptr`, networking (10 funcs), event loop (8 funcs), `fork`, `exec`, `waitpid`, `pipe`
- test: `test_decl`, `assert_eq_int`, `assert_eq_float`, `assert_eq_string`, `assert_eq_bool`, `test_end`
- bench: `bench_start`, `bench_iter`, `bench_end`

---

## Phase 6: CLI Extension (~2,500 lines)

### Step 20: `target.cot` + `project.cot` (~350 lines)

`target.cot`: Arch/Os enums, Target struct with `isWasm()`, `parse("wasm32")`.
`project.cot`: Load `cot.json` manifest (name, main, safe, libs).

### Step 21: `main.cot` extension (~2,150 lines added)

New commands: `build`, `test`, `run` with `--target=wasm32` flag, output naming, file I/O.

---

## Porting Order (Remaining)

```
Step 12: preprocess.cot                           ~720 lines
Step 13: gen.cot                                  ~1,600 lines
Step 14: link.cot                                 ~810 lines
Step 15: wasi_runtime.cot                         ~1,300 lines
Step 16: slice_runtime.cot                        ~820 lines
Step 17: print_runtime.cot                        ~720 lines
Step 18: test_runtime.cot + bench_runtime.cot     ~1,240 lines
Step 19: driver.cot                               ~3,500 lines
Step 20: target.cot + project.cot                 ~350 lines
Step 21: main.cot extension                       ~2,150 lines
                                                  ---------
                                                  ~13,210 lines remaining
```

---

## Validation Milestones

| Step | Milestone | Validation |
|------|-----------|------------|
| After 14 | Binary emission works | Generate .wasm from trivial program, validate with wasm-tools |
| After 18 | Runtime complete | All runtime functions generate valid Wasm bytecode |
| After 19 | Full pipeline | `selfcot build hello.cot --target=wasm32 && wasmtime hello.wasm` |
| After 21 | CLI complete | `selfcot build/test/run` commands work |
| Final | **Self-compilation** | `selfcot build self/main.cot --target=wasm32` produces `selfcot.wasm` |

---

## Key Design Decisions

1. **Index-based SSA** (not pointer-based): `SsaValue.args` is `List(int)` not `[]*Value`. Safer in Cot.

2. **Single SsaOp enum** with Wasm ops inline: `lower_wasm` rewrites `SsaOp.add -> SsaOp.wasm_i64_add` in-place.

3. **No ARC in Wasm pipeline**: WasmGC handles managed objects. Bump allocator for linear memory (slices, strings). If `.retain`/`.release` SSA ops appear in Wasm codegen, they trap with `unreachable`.

4. **Runtime as Wasm bytecode generators**: Each runtime module generates raw Wasm bytecode in `List(u8)`. Direct byte emission, no intermediate representation.

5. **Copy reference implementations**: Every file maps 1:1 to a Zig source. Line-by-line port, not invention.

6. **@safe mode** for all self/ files: Auto-ref, implicit self, colon struct init.
