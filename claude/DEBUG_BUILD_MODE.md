# Debug Build Mode — `cot build --debug`

**Updated:** 2026-03-19

## Overview

A `--debug` flag for `cot build` that controls safety check verbosity and diagnostic output. Most safety checks already exist in the compiler — the flag gates whether they print detailed diagnostics + abort (debug) or silently skip (release).

---

## What Cot Already Implements

Most debug features are **already in the compiled binary**. The `--debug` flag primarily controls their diagnostic output.

### Safety Trap Codes (always active)
| Check | Trap Code | Location |
|-------|-----------|----------|
| Integer overflow | `integer_overflow` | x64/aarch64 inst/mod.zig |
| Division by zero | `integer_division_by_zero` | x64/aarch64 lower.zig |
| Unreachable code | `unreachable_code_reached` | x64/aarch64 inst/mod.zig |
| Stack overflow | `stack_overflow` | x64/aarch64 inst/mod.zig |
| Heap misalignment | `heap_misaligned` | x64 inst/mod.zig |
| Bad integer cast | `bad_conversion_to_integer` | x64 inst/mod.zig |
| Corrupt switch value | `corruptSwitch` | x64/aarch64 inst/mod.zig |

### ARC Memory Safety (always active)
| Feature | Status | Location |
|---------|--------|----------|
| Redzone guards (0xFA left, 0xFB right) | Done | arc_native.zig:412-750 |
| Redzone validation on dealloc/realloc | Done | arc_native.zig:508-750 |
| Poison magic on free (0xDEADDEAD) | Done | arc_native.zig |
| Double-free detection (IsDeiniting) | Done | arc_native.zig |
| ARC magic sentinel (0xC07A8C00) | Done | arc_native.zig |
| Pointer range check (< 4096) | Done | arc_native.zig |
| ARC bad pointer diagnostics | Done | arc_native.zig |

### Crash Diagnostics (always active)
| Feature | Status | Location |
|---------|--------|----------|
| Signal handler (SIGSEGV/SIGILL/SIGBUS/SIGFPE/SIGABRT) | Done | signal_native.zig |
| Register dump (x0-x7, x16, fp, lr, sp, pc) | Done | signal_native.zig |
| Backtrace with symbols | Done | signal_native.zig |
| Source location (file:line via srcmap + dladdr) | Done | signal_native.zig |
| DWARF .debug_line section | Done | dwarf.zig, driver.zig |

### Bounds & Null Checks (always active)
| Feature | Status | Location |
|---------|--------|----------|
| Array/slice bounds checks | Done | lower.zig |
| Null pointer checks | Done | lower.zig |
| Optional unwrap checks | Done | lower.zig |

---

## What `--debug` Would Add

### Phase 1: CLI Flag + Gating (~30 min)

Add `--debug` flag to CLI. Pass `debug_mode: bool` through compile options. The flag gates:
- ARC diagnostics: debug → print + abort; release → silent skip
- Redzone corruption: debug → print details + abort; release → abort only
- Use-after-free (poison): debug → print diagnostic + backtrace; release → silent

### Phase 2: Enhanced Diagnostic Messages (~1 hour)

In debug mode, safety checks print detailed context before aborting:
- Bounds check: `"index out of bounds: index=5, length=3 at file.cot:42"`
- Null deref: `"null pointer dereference at file.cot:42 in func_name"`
- Optional unwrap: `"unwrap of null optional at file.cot:42"`
- Use-after-free: `"use-after-free: retain on freed object at 0xNNNN"`

Release mode: same checks fire, but with minimal output (just trap/abort).

### Phase 3: Fill-on-Alloc (gated by --debug) (~30 min)

- `alloc_raw`: fill user data with 0xAA (detects uninitialized reads)
- `dealloc_raw`: fill user data with 0xDD (detects use-after-free reads)
- Currently disabled because it overwrites Map buffers that callers immediately initialize. Debug flag enables it only when explicitly requested.

### Phase 4: Wasm Function Name Section (~1 hour)

In debug mode, emit Wasm custom name section (spec §5.5.10) so browser devtools and wasmtime show function names in stack traces. Native already has symbols via linker.

---

## Not Planned (Future / Post-1.0)

| Feature | Why Deferred | Reference |
|---------|-------------|-----------|
| Race detection | Requires ThreadSanitizer integration | Go `-race` |
| Memory statistics | Runtime heap metering | Go `ReadMemStats` |
| Full shadow memory ASan | ~5000 lines, LLVM-style | ASAN_IMPLEMENTATION.md |
| Sentinel value validation | Niche, Zig-specific | Zig `sentinelMismatch` |
| Union field tracking | Low priority | Zig `inactiveUnionField` |
| Escape analysis debugging | Compiler internals | Go `-gcflags='-m'` |
| Error unwrap checks | Beyond optional unwrap | Zig `unwrapError` |

---

## Effort Estimate

| Phase | Description | Effort |
|-------|-------------|--------|
| 1 | CLI flag + gating plumbing | 30 min |
| 2 | Enhanced diagnostic messages | 1 hour |
| 3 | Fill-on-alloc (debug-gated) | 30 min |
| 4 | Wasm name section | 1 hour |
| **Total** | | **~3 hours** |

Most safety infrastructure is already built. The `--debug` flag is polish, not architecture.
