# `cot bench` Native Crash — Debug Notes

## Status
Native `cot bench` crashes with SIGBUS. Wasm target works perfectly via wasmtime.

## The Crash
```
Crash: add + 180: str x2, [x0]
x0 = 0x13328b874 (garbage address — linmem_base + corrupted SP)
x21 = 0x100008008 (vmctx — correct)
x2 = 1 (correct arg)
```

## Root Cause (95% confident)
**The SP global at vmctx+0x10000 is corrupted from the very first call.**

Reading memory at crash time:
- SP at vmctx+0x10000 = `0x3324386c` (~860MB — way beyond 3-page linear memory of ~192KB)
- linmem_base at vmctx+0x20000 = `0x100048008` (looks correct)
- Expected SP: ~0x10000 (65536)

The corrupted SP makes `linmem_base + SP` point to unmapped memory → SIGBUS.

## Key Observation: bench_runtime.zig Global Offset Bug

**THIS IS ALMOST CERTAINLY THE BUG.** In `bench_runtime.zig:63-68`:

```zig
const bench_start_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });
const total_start_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });
const bench_iter_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 1 });
const bench_start_global = bench_start_dynamic + 1; // Offset by SP at index 0
const total_start_global = total_start_dynamic + 1;
const bench_iter_global = bench_iter_dynamic + 1;
```

The `+ 1` offset assumes SP is global 0 and bench globals start at index 1. But `addGlobal` returns the ACTUAL global index from the linker. If the module already has globals (SP at 0, plus others), the `+ 1` is WRONG — it double-counts.

Compare with `test_runtime.zig` which likely handles this correctly. The bench runtime was new code that may have gotten the global indexing wrong.

**If globals are at wrong indices, `_func_51` (calibrate_start) writes the timestamp to the WRONG global offset — potentially overwriting the SP global at vmctx+0x10000.**

`_func_51` disassembly confirms it writes to `vmctx + 0x10060`:
```asm
mov x9, #0x60
movk x9, #0x1, lsl #16    ; x9 = 0x10060
add x9, x7, x9            ; x9 = vmctx + 0x10060
str x0, [x9]               ; store timestamp here
```

vmctx + 0x10060 = global at offset 0x60 from global base (0x10000). With 16-byte stride: 0x60/16 = 6. So it writes to global index 6. **If SP is actually at a different index than expected, or if the bench globals are at wrong indices, this could corrupt SP.**

## What Works
- `cot test test/e2e/features.cot` — 154 native tests pass
- `cot bench /tmp/bench_simple.cot --target=wasm32` — works via wasmtime (Wasm is valid)
- All existing tests pass (`zig build test`, feature tests, etc.)

## Test File
```cot
// /tmp/bench_simple.cot
fn add(a: i64, b: i64) i64 {
    return a + b
}

bench "add" {
    add(1, 2)
}
```

## Call Chain at Crash
```
main → _wasm_main → bench_add → add → CRASH at str x2, [x0]
```

Backtrace confirms:
```
frame #0: add + 180
frame #1: bench_add + 100
frame #2: _wasm_main + 264  (second bench_add call)
frame #3: main + 72
```

Note: _wasm_main + 148 is first bench_add call (works), +264 is second (crashes). BUT ALSO: the very FIRST call to add crashes too (confirmed by lldb — crash happens immediately on launch).

Wait — actually inconsistent data here. In one lldb session the backtrace showed +264 (second call), in another the crash happened immediately. The crash is deterministic but the exact call may vary. The SP is corrupt from the start.

## Current Debug State of Files (MUST RESTORE)

### `compiler/frontend/lower.zig` (lines 839-874)
**generateBenchRunner is in "Test D" debug pattern** — simplified to just:
bench_fn, calibrate_start, calibrate_start, bench_fn (per bench).
Must restore to full version with: print_name, warmup loop, calibrate, measure loop, summary.

### `compiler/codegen/bench_runtime.zig`
calibrate_end is simplified (just stores constant 100). Must restore full calibration logic.

### `compiler/codegen/native/isa/aarch64/inst/emit.zig`
Has `JT_EMIT` debug print statements. Must remove.

### `compiler/codegen/native/machinst/buffer.zig`
Has `FIXUP_JT` debug print statements. Must remove.

### `compiler/main.zig` (lines 312, 316)
Cleanup calls commented out: `// cleanup(tmp_dir); // DEBUG`

## Disassembly of Key Functions

### `_func_51` (calibrate_start) — 14 instructions
```asm
stp x29, x30, [sp, #-0x10]!    ; save FP+LR
mov x29, sp
mov x21, x0                     ; x21 = vmctx
mov x1, x21                     ; prep cot_time call
mov x0, x1
bl cot_time                      ; returns nanoseconds in x0
mov x7, x21                     ; x7 = vmctx
mov x9, #0x60
movk x9, #0x1, lsl #16          ; x9 = 0x10060
add x9, x7, x9                  ; x9 = vmctx + 0x10060
str x0, [x9]                    ; store timestamp to global 6
b +48
ldp x29, x30, [sp], #0x10       ; restore
ret
```
Does NOT modify vmctx+0x10000 (SP). Does NOT corrupt x21. cot_time also preserves x21.

### `cot_time` — uses SVC #0x80 for gettimeofday, preserves x21, returns ns in x0

### `add` — standard br_table dispatch, loads SP from vmctx+0x10000, allocates 16-byte frame

### `bench_add` — wrapper, calls add(1, 2), returns 0

### `_wasm_main` (bench runner) — dispatches to block that:
1. SP -= 16, store back
2. call bench_add (first)
3. store result to linmem[SP]
4. call _func_51 (calibrate_start)
5. call _func_51 (calibrate_start)
6. call bench_add (second) → crashes inside add
7. store result
8. SP += 16, return 0

## Verified NOT the Cause
- Jump table encoding (pcRel32 math verified correct)
- cot_time clobbering x21 (it doesn't)
- _func_51 modifying SP global (it doesn't — writes to global 6 at vmctx+0x10060)
- Void vs non-void call handling in translator.zig (correct)
- x21 preservation across calls (all funcs set x21 = x0 = vmctx, same value)

## Investigation TODO
1. **Compare bench_runtime.zig global indexing with test_runtime.zig** — the `+ 1` offset pattern may be wrong
2. **Check how many globals the Wasm module has** and at what indices the bench globals end up
3. **Verify the SP global init value in the Wasm binary** — use `wasm-objdump -x` or `wasm-tools dump` on the .wasm file
4. **Check if the first call to add gets a valid SP** by breaking at add+80 with NAME-based breakpoint (address-based breakpoints don't fire for add — use `breakpoint set -n add` which matches 2 locations including system `add`)
5. **Read the Wasm global section** to see what global indices exist and their init values

## How to Reproduce
```bash
zig build
echo 'fn add(a: i64, b: i64) i64 { return a + b }
bench "add" { add(1, 2) }' > /tmp/bench_simple.cot
./zig-out/bin/cot bench /tmp/bench_simple.cot
# → "Benchmark killed by signal: 10" (SIGBUS)

# Wasm works fine:
./zig-out/bin/cot bench /tmp/bench_simple.cot --target=wasm32
# → runs successfully
```

## Fix Strategy
Per CLAUDE.md: copy reference implementation, don't invent. Compare with test_runtime.zig's global handling. The fix is likely a 1-3 line change in bench_runtime.zig's global index computation.
