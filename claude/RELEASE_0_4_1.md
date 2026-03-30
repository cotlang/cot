# Release 0.4.1: Two-Tier Native Codegen via Real Cranelift

**Date:** 2026-03-30
**Status:** In progress
**Gate:** All 78 e2e tests pass + `self/` compiles through CIR→Cranelift path

---

## Architecture

```
zig/libcot/                          rust/libclif/
  frontend/ (scanner, parser,          src/
    checker, lower)                      cir.rs     — CIR binary reader
  ssa/ (SSA passes)                      translate.rs — CIR → Cranelift IR
  codegen/wasm/ (Wasm bytecode)          lib.rs     — entry point
                                         (links cranelift_codegen, cranelift_frontend,
                                          cranelift_object, cranelift_module)
zig/libclif/
  cir_write.zig   — CLIF → CIR serializer
  ssa_to_clif.zig — SSA → hand-ported CLIF IR
  arc_native.zig  — ARC runtime as CLIF IR
  io_native.zig   — I/O runtime as CLIF IR
  print_native.zig — print runtime as CLIF IR
  test_native.zig  — test runner runtime as CLIF IR
  signal_native.zig — signal handler runtime
  compile.zig      — hand-ported Cranelift backend (KEPT for runtime .o)
  ir/clif/         — CLIF IR data structures
  machinst/        — MachInst lowering
  isa/             — ARM64 + x64 backends
  regalloc/        — register allocator
  object/          — ELF/Mach-O emission
```

**Flow:**
```
Cot source → libcot (parse, check, lower, SSA)
  → zig/libclif/ssa_to_clif.zig (SSA → CLIF IR)
  → zig/libclif/cir_write.zig (CLIF IR → CIR bytes)
  → rust/libclif (CIR → real Cranelift → user .o)

  → zig/libclif/compile.zig (CLIF IR → hand-ported backend → runtime .o)

  → link user .o + runtime .o → executable
```

The hand-ported Cranelift backend is retained ONLY for the runtime .o (ARC, I/O, print, test runner, signal handlers). User functions go through real Cranelift. Once runtime functions are ported to CIR, the hand-ported backend can be deleted entirely.

---

## Step 1: Move Native Codegen to zig/libclif (1 day)

### What moves

All 62 files (68,715 lines) from `zig/libcot/codegen/native/` to `zig/libclif/`:

```
zig/libcot/codegen/native/ssa_to_clif.zig    → zig/libclif/ssa_to_clif.zig
zig/libcot/codegen/native/cir_write.zig       → zig/libclif/cir_write.zig
zig/libcot/codegen/native/arc_native.zig      → zig/libclif/arc_native.zig
zig/libcot/codegen/native/io_native.zig       → zig/libclif/io_native.zig
zig/libcot/codegen/native/print_native.zig    → zig/libclif/print_native.zig
zig/libcot/codegen/native/test_native.zig     → zig/libclif/test_native.zig
zig/libcot/codegen/native/signal_native.zig   → zig/libclif/signal_native.zig
zig/libcot/codegen/native/compile.zig         → zig/libclif/compile.zig
zig/libcot/codegen/native/object_module.zig   → zig/libclif/object_module.zig
zig/libcot/codegen/native/dwarf.zig           → zig/libclif/dwarf.zig
zig/libcot/codegen/native/dwarf_reader.zig    → zig/libclif/dwarf_reader.zig
zig/libcot/codegen/native/elf.zig             → zig/libclif/elf.zig
zig/libcot/codegen/native/macho.zig           → zig/libclif/macho.zig
zig/libcot/codegen/native/wasm_parser.zig     → zig/libclif/wasm_parser.zig
zig/libcot/codegen/native/abi.zig             → zig/libclif/abi.zig
zig/libcot/codegen/native/libclif.zig         → zig/libclif/libclif.zig
zig/libcot/codegen/native/ir/clif/           → zig/libclif/ir/clif/
zig/libcot/codegen/native/frontend/          → zig/libclif/frontend/
zig/libcot/codegen/native/machinst/          → zig/libclif/machinst/
zig/libcot/codegen/native/isa/               → zig/libclif/isa/
zig/libcot/codegen/native/regalloc/          → zig/libclif/regalloc/
```

### What stays in zig/libcot/codegen/

Only Wasm codegen remains:
```
zig/libcot/codegen/wasm/        — Wasm bytecode generation
zig/libcot/codegen/arc.zig      — Wasm ARC/freelist
zig/libcot/codegen/wasm.zig     — Wasm target
zig/libcot/codegen/wasm_encode.zig
zig/libcot/codegen/wasm_opcodes.zig
zig/libcot/codegen/*_runtime.zig — Wasm runtime modules
```

### Import path updates

`zig/libcot/driver.zig` imports change:
```zig
// Before:
const ssa_to_clif = @import("codegen/native/ssa_to_clif.zig");
const cir_write = @import("codegen/native/cir_write.zig");
const native_compile = @import("codegen/native/compile.zig");

// After:
const ssa_to_clif = @import("../../libclif/ssa_to_clif.zig");
const cir_write = @import("../../libclif/cir_write.zig");
const native_compile = @import("../../libclif/compile.zig");
```

### Build system

`zig/build.zig` needs to include `zig/libclif/` as a module or source directory. Currently the native codegen compiles as part of `zig/libcot/`. After the move, it must be wired as a separate compilation unit or dependency.

### Validation

After the move, all 73/78 e2e tests must still pass (no regressions). Run:
```bash
for f in test/e2e/*.cot; do
    perl -e 'alarm 30; exec @ARGV' ./zig/zig-out/bin/cot test "$f" 2>&1 | tail -1
done
```

---

## Step 2: Fix Remaining 5 E2E Failures (2-3 days)

### Current state: 73/78 passing

| # | Test | Error | Root Cause | Fix |
|---|------|-------|-----------|-----|
| 1 | `arc.cot` | Link: missing VWT symbols | Runtime .o doesn't export VWT table/metadata symbols needed by CIR .o | Audit which symbols CIR .o references (`nm -u`), ensure runtime .o exports them. May need to generate VWT functions through CIR path too. |
| 2 | `bench_test.cot` | Link: `_main` undefined | Bench mode doesn't generate `_main` entry point through CIR path | Wire bench entry point generation in driver.zig `generateNativeCodeViaCIR`. The old path's `main_wrapper` must be generated. |
| 3 | `concurrency.cot` | Runtime crash in async chain | 3 tests fail: `nonisolated actor method`, `actor field access`, `actor with multiple fields`. Crashes in `concurrency.c` (nested async fn). | Debug with CIR_TRACE. The async fn calling convention may need adjustment for nested async calls where the callee returns a task pointer. Compare CLIF IR with old path. |
| 4 | `json.cot` | Runtime crash in string parsing | `test_parse_nested` crashes. String operations produce wrong results through CIR path. | The `string_make` / `slice_make` decomposition may not roundtrip correctly through CIR. Check if string ptr+len are both preserved. |
| 5 | `print.cot` | Runtime crash in `float_to_string` | `test_interp_float` prints `5.47077e-315` (garbage) then crashes. Float bit pattern is mangled. | Check CIR serialization of f64 constants and float-to-string runtime function. The `float_to_string` function is in runtime .o — verify it receives correct f64 argument. |

### features.cot sub-failures (7/370)

| Test | Issue |
|------|-------|
| `string reassignment from function call` | String ARC reference counting through CIR |
| `packed struct bitfield read/write` (4 tests) | Sub-word load/store with bitfield offsets |
| `overflow: u32 add/mul no overflow` (2 tests) | Unsigned overflow detection intrinsics |

These are lower priority — 363/370 features pass.

---

## Step 3: Gap 9 — Native Async State Machines (1-2 weeks)

### Why this is now unblocked

The blocker was: "Native CLIF codegen can't handle jump_table dispatch from async_split. The async_split creates new if/else check blocks with `.arg` ops that reference entry params. These values cross block boundaries without phi nodes — SSA dominance violation in CLIF builder."

**Real Cranelift solves this.** The `Variable` system in `cranelift_frontend` automatically handles values that cross block boundaries by inserting block parameters. `Switch::emit()` generates efficient br_table or binary search dispatch. This is the exact code path that Rust's `cg_clif` uses for async.

### What needs to happen

**Phase 9.1: Verify async_split output compiles through CIR** (2 days)

The `async_split.zig` SSA pass already transforms async functions into state machines:
```
fn poll(frame_ptr: i64) -> i64 {
    state = load frame_ptr[STATE_OFFSET]
    switch (state) {
        0 => { /* run to first await, save locals, set state=1, return PENDING */ }
        1 => { /* restore locals, run to second await, set state=2, return PENDING */ }
        2 => { /* restore locals, run to completion, return READY */ }
    }
}
```

This is a normal function with a switch. The Zig ssa_to_clif.zig translates the switch to `br_table`. The CIR serializer already handles `br_table` (OP_BR_TABLE = 0x00A4). The Rust translator already handles `OP_BR_TABLE`.

**Test:** Enable async_split for native target, compile `test/e2e/cooperative_await.cot` through CIR path. If it compiles, the basic machinery works. If not, trace which instruction fails.

Currently `async_split` may only run for Wasm target. Check `driver.zig` to see if there's a target gate.

**Phase 9.2: Frame allocation and local save/restore** (3 days)

Async functions need a heap-allocated frame to store:
- State discriminant (i64)
- Saved local variables (one slot per live variable at each suspend point)
- Return value slot

The constructor allocates the frame, stores initial args, and returns the frame pointer as the "task". The poll function receives the frame pointer and dispatches.

**Swift reference:** `GenCoro.cpp` — `emitAllocCoroutineFrame`, `emitDeallocCoroutineFrame`
**Rust reference:** `coroutine.rs` — `StateTransform`, discriminant as first field, locals as subsequent fields
**Cot Wasm:** `async_split.zig` already does this — study it and replicate for native

The key question: does `async_split.zig` produce SSA that ssa_to_clif.zig can translate? The previous blocker was about CLIF IR issues, which real Cranelift now handles.

**Phase 9.3: Cooperative scheduling on native** (2 days)

Wasm cooperative scheduling works by:
1. Await sites call `run_until_task_done(task_ptr)` from stdlib
2. `run_until_task_done` polls the task, processes other tasks from the global queue, repeats until done

For native, the same pattern applies. The executor stdlib code (`executor.cot`, `task_queue.cot`, `cooperative.cot`) is target-independent Cot code. If it compiles on native (which it should — it already passes tests), the scheduling just works.

What needs wiring:
- `lower.zig`: Generate `run_until_task_done` calls at native await sites (currently only done for Wasm)
- `driver.zig`: Auto-import `std/executor` + `std/task_queue` for async files on native (currently Wasm-only)
- `async_split.zig`: Enable for native target

**Phase 9.4: Test parity** (2 days)

All 24 concurrency test files (618 tests) must pass on native:
```bash
for f in test/e2e/concurrency.cot test/e2e/cooperative_await.cot test/e2e/async_closures.cot \
         test/e2e/executor.cot test/e2e/task_group.cot test/e2e/channel.cot \
         test/e2e/cancellation.cot test/e2e/continuation.cot test/e2e/task_local.cot \
         test/e2e/async_stream.cot test/e2e/async_throwing_stream.cot \
         test/e2e/throwing_task_group.cot test/e2e/discarding_task_group.cot \
         test/e2e/async_ops.cot test/e2e/async_sequences.cot test/e2e/sending.cot \
         test/e2e/duration.cot test/e2e/priority_queue.cot test/e2e/task_queue.cot \
         test/e2e/global_executor.cot test/e2e/cooperative_task_group.cot \
         test/e2e/actor_executor.cot test/e2e/task_expr.cot test/e2e/assoc_types.cot; do
    perl -e 'alarm 30; exec @ARGV' ./zig/zig-out/bin/cot test "$f" 2>&1 | tail -1
done
```

---

## Step 4: Compile self/ Through CIR Path (validation gate)

Once Steps 1-3 are complete:

```bash
# Build selfcot using the CIR→Cranelift path
./zig/zig-out/bin/cot build self/main.cot -o /tmp/selfcot

# Verify selfcot works
/tmp/selfcot version

# Compile a test file with selfcot
/tmp/selfcot test test/cases/arithmetic.cot
```

If selfcot builds and runs through the CIR path, the two-tier architecture is validated. The hand-ported Cranelift backend (`compile.zig`, `machinst/`, `isa/`, `regalloc/`) becomes legacy — retained only for the runtime .o until those functions are ported to CIR.

---

## After 0.4.1: Roadmap to Full Cranelift

Once the runtime .o functions (ARC, I/O, print, test runner, signals) are also serialized to CIR and compiled by real Cranelift, the entire hand-ported backend (~50,000 lines) can be deleted:

```
DELETE: zig/libclif/compile.zig (1,185 lines)
DELETE: zig/libclif/machinst/ (~3,000 lines)
DELETE: zig/libclif/isa/ (~15,000 lines)
DELETE: zig/libclif/regalloc/ (~12,000 lines)
DELETE: zig/libclif/object_module.zig (885 lines)
DELETE: zig/libclif/elf.zig (782 lines)
DELETE: zig/libclif/macho.zig (635 lines)
DELETE: zig/libclif/dwarf.zig (913 lines)
DELETE: zig/libclif/dwarf_reader.zig (1,063 lines)
```

That's ~35,000 lines of hand-ported Cranelift code replaced by 1,250 lines of Rust (translate.rs + cir.rs) plus the real Cranelift crate.

---

## Success Criteria for 0.4.1

1. All 78 e2e test files pass through CIR→real Cranelift path
2. All 618 concurrency tests pass on native (Gap 9 closed)
3. `self/main.cot` compiles to a working binary through CIR path
4. Native codegen lives in `zig/libclif/` (clean separation from `zig/libcot/`)
5. No regressions on Wasm target
