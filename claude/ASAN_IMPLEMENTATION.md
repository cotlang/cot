# ASan Implementation Plan for Cot

**Date:** 2026-03-18
**Updated:** 2026-03-19
**Goal:** Memory safety instrumentation for debug builds, catching heap buffer overflow, use-after-free, double-free, and uninitialized reads.

---

## Current Status

| Tier | Feature | Status | Location |
|------|---------|--------|----------|
| 1 | Redzone allocator | **DONE** | `arc_native.zig:412-750` |
| 1 | Redzone validation on dealloc | **DONE** | `arc_native.zig:613-750` |
| 1 | Redzone validation on realloc | **DONE** | `arc_native.zig:508-602` |
| 2 | Poison on dealloc (0xDEADDEAD) | **DONE** | `arc_native.zig` |
| 2 | Double-free detection (IsDeiniting) | **DONE** | `arc_native.zig` |
| 2 | ARC magic sentinel (0xC07A8C00) | **DONE** | `arc_native.zig` |
| 2 | Pointer range check (< 4096) | **DONE** | `arc_native.zig` |
| 2 | Loud use-after-free reporting | **Not done** | Poison detection is silent — should print diagnostic + backtrace |
| 3 | Fill-on-alloc (0xAA pattern) | **Not done** | Disabled — overwrites Map buffers that are immediately initialized |

### Tier 1: Redzone Allocator — COMPLETE

`alloc_raw` adds 40-byte overhead: `[size:8][LEFT_REDZONE:16 @ 0xFA][user_data:N][RIGHT_REDZONE:16 @ 0xFB]`. `dealloc_raw` and `realloc_raw` validate both redzones before freeing. Corruption prints diagnostic and aborts.

### Tier 2: Use-After-Free Detection — MOSTLY COMPLETE

Remaining: change poison detection from silent skip to loud abort in debug mode. When `retain`/`release` sees magic == `0xDEADDEAD` (freed object), it should print `"use-after-free: retain/release on freed object at 0xNNNN"`, call `__cot_print_backtrace()`, and `exit(2)`.

~30 lines of CLIF IR in retain + release functions.

### Tier 3: Fill-on-Alloc — NOT STARTED

Disabled because it overwrites buffers that callers immediately initialize (e.g., `Map.ensureCapacity` zeros states after alloc). Requires `--debug` flag gating (see `DEBUG_BUILD_MODE.md`).

---

## Gating

Not yet gated by debug/release mode. Redzones are always enabled. Needs `debug_mode: bool` parameter to `arc_native.generate()` to:
- Debug (default): redzones, poison detection, fill-on-alloc
- Release (`--release`): no redzones, no fill, minimal checks

---

## Future: Full ASan (Post-1.0)

Shadow memory (1 byte per 8 app bytes) + instrumentation pass in `ssa_to_clif.zig` inserting shadow checks before every load/store. ~5000 lines. Parity with Go/Rust sanitizer support.
