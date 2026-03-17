# Debug Build Mode — `cot build --debug`

## Overview

A `--debug` flag for `cot build` that embeds additional safety checks and diagnostic information in the compiled binary. Modeled after Zig's `ReleaseSafe` mode and Rust's `debug_assertions`.

When enabled, the compiled binary includes:
- Function name strings embedded in the binary (better backtraces)
- Bounds checks on all array/list/slice accesses
- Null checks before pointer dereferences
- ARC diagnostic messages (bad pointer, double-free, underflow)
- Optional unwrap checks with file:line messages

When disabled (default), these checks are omitted for performance.

---

## Reference Implementations

### Zig — Build Modes
- **Debug**: All safety checks on, no optimizations, fast compile
- **ReleaseSafe**: Optimized but retains safety checks (bounds, overflow, null)
- **ReleaseFast**: All checks removed, maximum performance
- **ReleaseSmall**: Size-optimized, no checks

Key file: `std/debug.zig` — Zig's safety check infrastructure.

Pattern: Zig uses `@import("builtin").mode` to branch at comptime:
```zig
if (builtin.mode == .Debug or builtin.mode == .ReleaseSafe) {
    if (index >= len) @panic("index out of bounds");
}
```

### Rust — debug_assertions
- `cargo build` (debug): includes `debug_assert!`, bounds checks, overflow checks
- `cargo build --release`: strips all debug assertions

Pattern: `#[cfg(debug_assertions)]` gates debug-only code:
```rust
#[cfg(debug_assertions)]
fn check_bounds(idx: usize, len: usize) {
    assert!(idx < len, "index out of bounds: {idx} >= {len}");
}
```

### Swift — -Onone vs -O
- `-Onone` (debug): includes precondition checks, ARC logging, exclusivity checks
- `-O` (release): strips most checks, optimizes ARC

Pattern: `_isDebugAssertConfiguration()` runtime check:
```swift
if _isDebugAssertConfiguration() {
    precondition(index < count, "Index out of range")
}
```

### Go — GORACE / -race
- `go build -race`: enables data race detector
- Go doesn't have a general debug mode, but uses build tags for assertions

---

## Implementation Plan

### Phase 1: CLI Flag + Lowerer Plumbing

**Files:** `self/main.cot`, `compiler/main.zig`, `compiler/cli.zig`

1. Add `--debug` flag to `cot build` CLI parsing
2. Pass `debug_mode: bool` through `LowerOptions` / `CompileOptions`
3. Store in `Lowerer.debug_mode` field
4. The lowerer checks `self.debug_mode` before emitting safety checks

### Phase 2: Bounds Checks

**Files:** `compiler/frontend/lower.zig` (index access lowering)

When `debug_mode` is true, emit bounds checks before every index operation:
```
if debug_mode:
    if index < 0 or index >= length:
        print "file:line: index out of bounds (index=N, length=M) in func_name"
        backtrace()
        exit(2)
```

Currently, `@trap()` in List.get/set handles this. In debug mode, the trap message should include the index value and array length, not just file:line.

Reference: Zig `std.ArrayList` bounds check prints the actual values.

### Phase 3: Null Pointer Checks

**Files:** `compiler/frontend/lower.zig` (pointer deref lowering)

When `debug_mode` is true, emit null check before every pointer dereference:
```
if debug_mode and ptr == null:
    print "file:line: null pointer dereference in func_name"
    backtrace()
    exit(2)
```

Reference: Zig emits `@panic("reached unreachable")` for undefined behavior in safe modes.

### Phase 4: ARC Debug Output Control

**Files:** `compiler/codegen/native/arc_native.zig`

The ARC diagnostics (bad pointer, double-free, underflow) are currently ALWAYS emitted. In release mode they should be silent. In debug mode they should print AND abort.

Debug mode ARC behavior:
- Bad pointer in retain/release: print diagnostic + abort (currently prints + continues)
- Double-free: print diagnostic + abort (currently prints + continues)
- Refcount underflow: print diagnostic + abort
- Use-after-free (poison magic): print "ARC: use-after-free at 0xNNNN" + abort

Release mode ARC behavior:
- Bad pointer: silently skip (current behavior without diagnostics)
- Double-free: silently skip
- Everything else: no overhead

Implementation: The ARC runtime functions check a global `__cot_debug_mode` variable (set at startup from the CLI flag). If set, print + abort. If not, skip silently.

### Phase 5: Function Name Embedding

**Files:** `compiler/codegen/wasm/`, `compiler/codegen/native/`

In debug mode, embed function name strings in the binary so backtraces show readable names instead of hex addresses. Currently the native backend already includes symbol names (from `nm`), but wasm binaries don't have this.

For wasm: add a custom name section (Wasm name section, spec §5.5.10) with function names.
For native: already works via linker symbols.

### Phase 6: Optional Unwrap Checks

**Files:** `compiler/frontend/lower.zig` (if-optional lowering)

When `debug_mode` is true and an optional is force-unwrapped (`.?` operator), emit a check:
```
if debug_mode and optional.tag == 0:
    print "file:line: unwrap of null optional in func_name"
    backtrace()
    exit(2)
```

---

## Global Debug Mode Variable

To avoid passing `debug_mode` through every function, use a global variable:

```cot
// In the compiled binary's data section:
var __cot_debug_mode: i64 = 0  // set to 1 by main() if --debug was passed
```

The ARC runtime and safety checks read this global. The lowerer emits loads from `__cot_debug_mode` before each check, so the overhead in release mode is just a single load + branch-not-taken (predictable, minimal).

Reference: Go's `runtime.raceenabled` global for race detector gating.

---

## Effort Estimate

| Phase | Description | Effort |
|-------|-------------|--------|
| 1 | CLI flag + plumbing | 30 min |
| 2 | Bounds checks with values | 1 hour |
| 3 | Null pointer checks | 1 hour |
| 4 | ARC debug output control | 30 min |
| 5 | Function name embedding | 2 hours |
| 6 | Optional unwrap checks | 30 min |
| **Total** | | **~5.5 hours** |

---

## Interaction with Existing Features

- `COT_DEBUG=phase` (pipeline_debug.zig): Unrelated — this is for Zig compiler internals
- `@trap()`: Debug mode enhances trap messages with more context
- `@panic()`: Already includes file:line + backtrace, no change needed
- Signal handler: Debug mode doesn't change signal handling (already has backtrace)
- `--release` flag (future): Opposite of `--debug`, strips ALL checks for maximum performance
