# Wasm Self-Hosting Execution Plan

**Date:** Mar 12, 2026
**Goal:** `selfcot build selfcot.cot --target=wasm32` produces a working `.wasm` binary.
**Current state:** All phases complete. 36,675 lines across 28 files. CLI has full parity (build/run/test/bench/init). Frontend ~80% ported, backend 100% ported.

---

## Architecture: Wasm = WasmGC, No ARC

**ARC is native-only.** The Wasm target uses WasmGC exclusively — the browser/runtime GC handles object lifetime. There are NO retain/release/alloc/dealloc calls in the Wasm pipeline.

The Wasm runtime provides:
- **Bump allocator** (heap_ptr global) for linear memory (slices, strings, stack frames)
- **WasmGC** for managed object lifetime (structs, closures)
- **WASI imports** for system calls (fd_write, fd_read, exit, time, etc.)

---

## Status: ALL PHASES COMPLETE

```
Source -> Scanner -> Parser -> Checker -> Lower -> SSA Builder
  | DONE (25K lines in self/frontend/)
SSA Passes: rewritegeneric -> decompose -> rewritedec -> lower_wasm -> schedule -> layout
  | DONE (Steps 1-8, ~2,400 lines)
Wasm Codegen: wasm_gen (SSA -> Wasm instructions)
  | DONE (Steps 9-11, ~2,700 lines)
Wasm Binary: bytecode emission, linking, assembly
  | DONE (Phase 3, Steps 12-14, ~2,250 lines)
Wasm Runtime: mem, wasi, slice, print, test, bench
  | DONE (Phase 4, Steps 15-18b, ~3,700 lines)
Driver: Multi-file orchestration, function index map, pass pipeline
  | DONE (Phase 5, Step 19, ~620 lines)
CLI: build/run/test/bench/init commands, -o flag
  | DONE (Phase 6, Steps 20-21, ~190 lines added to main.cot)
```

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
| 9 | wasm_types.cot + constants.cot | 599+152 | 9 |
| 10 | prog.cot | 405 | 9 |
| 11 | wasm_gen.cot + code_builder.cot | 1,103+533 | 14 |
| 12 | preprocess.cot | 551 | -- |
| 13 | assemble.cot | 806 | 7 |
| 14 | link.cot | 895 | 7 |
| 15 | wasi_runtime.cot | 932 | 5 |
| 16 | slice_runtime.cot | 250 | 3 |
| 17 | print_runtime.cot | 909 | 6 |
| 18 | test_runtime.cot + bench_runtime.cot | 538+536 | 10+9 |
| 18b | mem_runtime.cot | 556 | 8 |
| 19 | driver.cot + ssa_passes.cot + ssa_passes_dec.cot | 599+13+11 | 11 |
| 20-21 | main.cot CLI: build, run, test, bench, init, -o flag | ~190 | -- |
| **Total** | **28 files** | **~12,000 (backend) + 25K (frontend) = ~37K** | **~142** |

---

## Validation Milestones

| Step | Milestone | Status |
|------|-----------|--------|
| After 14 | Binary emission works | DONE — .wasm files generated |
| After 18b | Runtime complete | DONE — all 6 runtime modules |
| After 19 | Full pipeline | DONE — driver orchestrates full pipeline |
| After 21 | CLI complete | DONE — build/run/test/bench/init commands |
| Final | **Self-compilation** | BLOCKED — self-hosted parser ~75%, checker ~80%, lower ~66% |

### Remaining for Self-Compilation

The Wasm backend is 100% complete. The bottleneck is the **frontend** (parser/checker/lower):

- **Parser** (~75%): Missing labeled blocks, select expressions, some edge cases
- **Checker** (~80%): Missing some comptime evaluation, select/spawn checking
- **Lower** (~66%): Missing async (~1,170 lines), edge-case init variants (~1,180 lines)
- **Overall**: The self-hosted compiler can parse/check/lower simple programs but not itself yet

---

## Key Design Decisions

1. **Index-based SSA** (not pointer-based): `SsaValue.args` is `List(int)` not `[]*Value`. Safer in Cot.

2. **Single SsaOp enum** with Wasm ops inline: `lower_wasm` rewrites `SsaOp.add -> SsaOp.wasm_i64_add` in-place.

3. **No ARC in Wasm pipeline**: WasmGC handles managed objects. Bump allocator for linear memory (slices, strings). If `.retain`/`.release` SSA ops appear in Wasm codegen, they trap with `unreachable`.

4. **Runtime as Wasm bytecode generators**: Each runtime module generates raw Wasm bytecode in `List(u8)`. Direct byte emission, no intermediate representation.

5. **Copy reference implementations**: Every file maps 1:1 to a Zig source. Line-by-line port, not invention.

6. **@safe mode** for all self/ files: Auto-ref, implicit self, colon struct init.
