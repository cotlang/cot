# Self-Hosting Strategy: Zig Compiler → Cot Compiler

**Date:** Mar 6, 2026
**Goal:** Port the Cot compiler from Zig to Cot, achieving full self-hosting.

---

## Current State (Updated Mar 14, 2026)

| Subsystem | Zig Lines | Cot Lines | Status |
|-----------|-----------|-----------|--------|
| **Frontend** (scanner, parser, checker, lower, SSA builder, IR, types, AST, errors, arc_insertion) | 24,188 | 25,106 | **~100%** |
| SSA passes (rewritedec, layout, schedule, decompose, lower_wasm, rewritegeneric) | 4,557 | 1,896 | **92%** |
| Driver (pipeline orchestrator) | 5,913 | (in main.cot) | **partial** |
| Wasm codegen (wasm_gen, arc, wasi_runtime, link, assemble, constants) | 9,993 | 11,051 | **93%** |
| Native codegen (CLIF, MachInst, ARM64, x64, regalloc, runtime) | 70,934 | - | 0% |
| CLI / main / project | 2,971 | 1,029 | **100%** |
| LSP | 4,973 | - | 0% |
| Formatter | 1,234 | - | 0% |
| **Total** | **~139K** | **~41.8K** | **30%** |

**MILESTONE (Mar 13):** `selfcot check self/main.cot` passes — self-hosted type-checks itself.
**WasmGC (Mar 14):** 5 phases ported from Kotlin patterns (arrays, union subtypes, nullable refs, call_ref, GC strings).
The frontend, SSA passes, and Wasm backend are essentially complete. The remaining work is native codegen (~71K lines), LSP, and formatter.

---

## Strategy: Wasm-First, Then Native

### Why Wasm first?

The native backend is **71K lines** of Cranelift port (CLIF IR → MachInst → register allocation → ARM64/x64 emission → Mach-O/ELF). Porting it to Cot is a multi-month effort with complex ISA-specific logic.

The Wasm backend is **~10K lines** and produces a `.wasm` file that runs on wasmtime. It's the fastest path to a self-compiling binary:

```
selfcot build selfcot.cot --target=wasm32 → selfcot.wasm
wasmtime selfcot.wasm build hello.cot --target=wasm32 → hello.wasm
```

Once the Wasm path self-hosts, the native backend is ported incrementally — the goal is full self-hosting with zero Zig dependency beyond bootstrap.

### Milestone: Self-Compiling via Wasm

The minimal path to `selfcot build selfcot.cot --target=wasm32`:

```
Frontend (DONE, 25K) → SSA passes (4.6K) → Wasm codegen (10K) → Driver (6K) → CLI (3K)
                                                                    Total new: ~24K lines
```

---

## Phase Plan

### Phase 1: SSA Passes (~4,557 lines)

The SSA passes transform the IR graph before codegen. Required for both Wasm and native.

| File | Lines | Purpose | Priority |
|------|-------|---------|----------|
| `ssa/op.zig` | 713 | SSA opcode definitions | P0 — data types |
| `ssa/value.zig` | 322 | SSA value representation | P0 — data types |
| `ssa/block.zig` | 228 | SSA block + edges | P0 — data types |
| `ssa/func.zig` | 260 | SSA function container | P0 — data types |
| `ssa/dom.zig` | 255 | Dominator tree | P1 — used by passes |
| `ssa/passes/rewritegeneric.zig` | 154 | Generic type rewriting | P1 |
| `ssa/passes/decompose.zig` | 273 | Multi-value decomposition | P1 |
| `ssa/passes/rewritedec.zig` | 654 | Decimal rewriting | P1 |
| `ssa/passes/schedule.zig` | 264 | Instruction scheduling | P1 |
| `ssa/passes/layout.zig` | 291 | Block layout | P1 |
| `ssa/passes/lower_wasm.zig` | 576 | SSA → Wasm lowering | P0 — Wasm path |
| `ssa/passes/lower_native.zig` | 50 | SSA → native lowering | P2 — native only |
| `ssa/debug.zig` | 351 | Debug printing | P2 |

**Note:** `ssa.cot` (482 lines, SSA data structures) and `ssa_builder.cot` (2,099 lines, IR→SSA) already exist in self/frontend/. The SSA *passes* are what's missing.

### Phase 2: Wasm Codegen (~9,993 lines)

Two layers: high-level Wasm generation and low-level Wasm binary emission.

**Top-level (codegen/):**

| File | Lines | Purpose | Priority |
|------|-------|---------|----------|
| `codegen/wasm_gen.zig` | 2,244 | SSA → Wasm instructions | P0 |
| `codegen/arc.zig` | 1,559 | ARC runtime as Wasm module functions | P0 |
| `codegen/wasi_runtime.zig` | 1,275 | WASI runtime stubs | P0 |

**Low-level (codegen/wasm/):**

| File | Lines | Purpose | Priority |
|------|-------|---------|----------|
| `codegen/wasm/gen.zig` | 1,544 | Wasm bytecode emission | P0 |
| `codegen/wasm/constants.zig` | 829 | Wasm opcodes, section IDs | P0 |
| `codegen/wasm/link.zig` | 789 | Wasm module linking | P0 |
| `codegen/wasm/preprocess.zig` | 699 | Wasm IR preprocessing | P0 |
| `codegen/wasm/assemble.zig` | 609 | Wasm binary assembly | P0 |
| `codegen/wasm/prog.zig` | 270 | Program structure | P0 |
| `codegen/wasm/wasm.zig` | 175 | Wasm types | P0 |

### Phase 3: Driver (~5,913 lines)

The driver orchestrates the full pipeline: multi-file resolution, compilation, linking.

| Concern | Est. Lines | Notes |
|---------|-----------|-------|
| Multi-file import resolution | ~800 | File discovery, dependency ordering |
| Function index map construction | ~500 | Maps function names → indices |
| SSA pass pipeline | ~400 | Run passes in order |
| Wasm binary generation | ~600 | Call codegen, write output |
| Native binary generation | ~1,500 | CLIF → native (defer to Phase 5) |
| Test/bench runner wiring | ~300 | Test mode, filtering |
| Runtime function registration | ~500 | func_indices, extern fn mapping |
| Generic function queue processing | ~300 | Cross-file generic lowering |

For Wasm-only self-hosting, ~3,400 lines needed (skip native generation).

### Phase 4: CLI / Main (~2,971 lines)

| File | Lines | Purpose |
|------|-------|---------|
| `main.zig` | 1,769 | Command dispatch, compileAndLink |
| `cli.zig` | 850 | Argument parsing, help text |
| `project.zig` | 352 | cot.json manifest loading |

`self/main.cot` already has 384 lines (parse, check commands). Extend with `build`, `test`, `run`.

### Phase 5: Native Codegen (~70,934 lines)

The largest subsystem — porting it to Cot is the ultimate dogfooding exercise.

| Subsystem | Lines | Notes |
|-----------|-------|-------|
| ssa_to_clif.zig | 1,832 | SSA → CLIF IR translation |
| machinst/ (vcode, lower, buffer, inst) | 7,413 | CLIF → MachInst |
| isa/aarch64/ (lower, emit, mod, abi) | 10,613 | ARM64 backend |
| isa/x64/ (lower, emit, mod, abi) | 11,811 | x86_64 backend |
| regalloc/ | 5,785 | Register allocator (regalloc2 port) |
| Runtime (*_native.zig) | 8,444 | ARC, I/O, print, test as CLIF IR |
| Object/linker (object_module, macho, elf) | 5,000+ | Object file emission |
| Other (scheduler, frontend, ir/) | ~20,000 | Various |

This is a multi-month effort with complex ISA-specific logic, but it proves Cot can handle the hardest systems programming tasks. Strategy: port incrementally after Wasm self-hosts, starting with ssa_to_clif and working outward through machinst → regalloc → ISA backends → object emission.

### Phase 6: Optional

| Subsystem | Lines | Priority |
|-----------|-------|----------|
| Formatter | 1,234 | Nice to have |
| LSP | 4,973 | Nice to have |

---

## Line Count Summary

| Phase | Lines | Cumulative | Milestone |
|-------|-------|------------|-----------|
| Done (frontend) | 25,004 | 25,004 | `selfcot parse`, `selfcot check` |
| Phase 1 (SSA passes) | ~4,557 | ~29,561 | IR → SSA → optimized SSA |
| Phase 2 (Wasm codegen) | ~9,993 | ~39,554 | SSA → .wasm binary |
| Phase 3 (Driver) | ~3,400 | ~42,954 | Multi-file pipeline |
| Phase 4 (CLI) | ~2,587 | ~45,541 | `selfcot build x.cot --target=wasm32` |
| **Wasm self-hosting** | | **~45K** | **selfcot compiles itself to Wasm** |
| Phase 5 (Native) | ~70,934 | ~116K | **selfcot compiles itself to native** |
| Phase 6 (Fmt/LSP) | ~6,207 | ~122K | Full feature parity |

---

## Key Decisions

1. **Wasm-first**: ~20K new lines to self-compile via Wasm. This is the fastest path to a self-compiling binary.
2. **Full native self-hosting**: The native backend (71K lines) is ported to Cot after Wasm self-hosts. This is the ultimate dogfooding — if Cot can compile its own ISA backends, it can build anything. The Zig compiler becomes bootstrap-only.
3. **Incremental validation**: Each phase produces a testable artifact. Phase 1+2 can be tested with `cot test` on individual files before the driver is ported.
4. **Reference copying**: Every pass/codegen file has a Go or Cranelift reference. Copy exactly, don't invent.
