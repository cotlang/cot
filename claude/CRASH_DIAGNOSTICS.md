# Crash Diagnostics Improvement Plan

## Current State

### What We Have
1. **Signal handler** (`compiler/codegen/native/signal_native.zig`):
   - `__cot_signal_handler(sig)` — prints "fatal error: signal N", exits
   - `__cot_install_signals()` — registers handlers for SIGILL/SIGSEGV/SIGBUS/SIGFPE/SIGABRT
   - `__cot_print_backtrace()` — calls libc `backtrace()` + `backtrace_symbols_fd()`

2. **ARC magic sentinel** (`compiler/codegen/native/arc_native.zig`):
   - `ARC_HEAP_MAGIC` = `0xC07A_8C00_C07A_8C00` stored at allocation header
   - `retain()` and `release()` check null, then check magic before touching refcounts
   - Reference: Swift `isValidPointerForNativeRetain` (HeapObject.cpp:550)

3. **Pipeline debug** (`compiler/pipeline_debug.zig`):
   - `COT_DEBUG=phase1,phase2` env var for debug output per compilation phase
   - `COT_TRACE=funcname` to trace a function through all passes

4. **@panic** — prints `file:line: panic: message` then exits with backtrace
5. **@trap** — prints `file:line: trap` then exits

### What's Missing

**BUG: Signal handler doesn't print backtrace.** `__cot_signal_handler` writes the error message and calls `_exit` but NEVER calls `__cot_print_backtrace`. The backtrace function exists but is unused by the signal handler. This is why all selfcot crashes show only "fatal error: signal 11" with no stack trace.

**BUG: ARC magic check crashes instead of reporting.** When `retain(1)` or `release(1)` is called with a bad pointer (e.g., optional tag value), the magic check tries to read from `1 - 32 = -31`, causing SIGSEGV. The magic check should detect invalid pointers BEFORE the dereference, not crash during it.

**Missing: No ARC-specific crash message.** When ARC detects a bad pointer (wrong magic, null in unexpected place, refcount underflow), it should print a diagnostic message identifying the ARC operation and the bad pointer value, not just crash with a generic signal.

**Missing: No phase tracking in selfcot.** The selfcot binary has no equivalent of `COT_DEBUG` or phase markers. Crashes during compilation show no context about which file or declaration was being processed.

---

## Improvements (Ordered by Impact)

### 1. Signal Handler Must Call Backtrace (Critical — 5 min fix)

**File:** `compiler/codegen/native/signal_native.zig`, `generateSignalHandler()`

**Bug:** The signal handler at line 62-188 writes "fatal error: signal N" then calls `_exit`. It never calls `__cot_print_backtrace()`.

**Fix:** Before calling `_exit`, import and call `__cot_print_backtrace`. This is a ~10-line addition to the CLIF IR generation.

**Reference:** Go's `runtime.sigpanic()` (runtime/signal_unix.go) calls `traceback()` before `exit`.

**Expected output after fix:**
```
fatal error: signal 11
0   selfcot  0x100060388  checker.Scope_lookup + 0
1   selfcot  0x10006c4a0  checker.Checker_checkExpr + 1280
2   selfcot  0x100068000  checker.Checker_checkFile + 840
3   selfcot  0x1001ac198  main.checkFileRecursive + 416
```

### 2. ARC Pointer Validation Before Dereference (Critical — 30 min fix)

**File:** `compiler/codegen/native/arc_native.zig`, `generateRetain()` and `generateRelease()`

**Bug:** The magic check loads from `obj - 32 + 0` without first validating the pointer is in a reasonable range. For `obj = 1` (optional tag value), this reads from address `-31` → SIGSEGV.

**Fix:** Add a range check BEFORE the magic load. Swift pattern from `isValidPointerForNativeRetain` (EmbeddedRuntime.swift:431-439):
```
if objectBits == 0 { return false }                    // null
if objectBits < 4096 { return false }                  // small values (tags, offsets)
if (objectBits & HeapObject.immortalBit) != 0 { ... }  // immortal
```

Port this: add a check `if obj < 4096 { return obj }` BEFORE the magic dereference in both retain and release. Values < 4096 are guaranteed to be non-heap (null page on all modern OSes). This catches optional tag values (0, 1), small integers, and null.

**Reference:** Swift `isValidPointerForNativeRetain` (EmbeddedRuntime.swift:431)

### 3. ARC Diagnostic Messages (High — 1 hour)

**File:** `compiler/codegen/native/arc_native.zig`

When the magic check fails (pointer is >= 4096 but magic doesn't match), instead of silently skipping, print a diagnostic:
```
ARC error: retain called with non-heap pointer 0x10045a3b0 (magic=0x5555555555555555, expected 0xC07A8C00C07A8C00)
```

This would immediately identify use-after-free (freed memory has scribbled magic) vs bad pointer (wrong address entirely) vs optional tag (small value).

**Implementation:** After the magic load, add a branch: if magic != ARC_HEAP_MAGIC and magic != 0 (not uninitialized), write diagnostic to stderr.

**Reference:** Swift's runtime assertion `SWIFT_RT_TRACK_INVOCATION` macro tracks retain/release for debugging.

### 4. Phase Tracking in Selfcot (High — 30 min)

**File:** `self/main.cot`

Add `eprintln` markers at phase boundaries that are ALWAYS printed (not debug-only). When the selfcot crashes, the last printed marker shows which phase was active:
```
[cot] parse: stdlib/sys.cot
[cot] check: stdlib/sys.cot
[cot] check: stdlib/list.cot
[cot] lower: stdlib/sys.cot
[cot] lower: stdlib/list.cot
[cot] codegen: generating wasm
fatal error: signal 11
0  selfcot  0x100146da8  ...
```

**Implementation:** Add prints in `compileWithImports`, `checkFileRecursive`, `checkAndStoreChecker`, `lowerOneFile`, `generateWasmCode`. Guard with a `COT_VERBOSE` env var or `--verbose` flag so they don't pollute normal output.

**Reference:** Rust's `RUSTC_LOG` environment variable for compiler phase logging.

### 5. Refcount Underflow Detection (Medium — 30 min)

**File:** `compiler/codegen/native/arc_native.zig`, `generateRelease()`

After the atomic decrement, check if the old StrongExtra was already 0 and the decrement would underflow. Print:
```
ARC error: release underflow at 0x10045a3b0 (refcount was already 0)
```

**Reference:** Swift's `SWIFT_OBJECT_IS_BEING_DEINITIALIZED` check — the deinit flag prevents double-release.

### 6. Function Name in @trap Output (Medium — 15 min)

**File:** `compiler/frontend/lower.zig`, `@trap` lowering

Currently `@trap` prints `file:line: trap`. Add the enclosing function name:
```
file.cot:42: trap in List(int)_get
```

**Implementation:** The lowerer knows the current function name (`fb.name`). Embed it in the trap message string.

### 7. Debug Build Mode for Selfcot (Low — 1 hour)

Add a `--debug` flag to `cot build` that:
- Embeds function names as string data in the binary (for better backtraces)
- Adds bounds checks to all array/list accesses
- Adds null checks before pointer dereferences
- Enables ARC diagnostic messages

**Reference:** Zig's `ReleaseSafe` mode which includes safety checks in optimized builds.

### 8. MallocScribble Equivalent in ARC (Low — 30 min)

**File:** `compiler/codegen/native/arc_native.zig`, `generateDealloc()`

After freeing an object, overwrite the user data with a pattern (0xDEADBEEF). This makes use-after-free immediately visible — any access to freed ARC objects reads recognizable garbage instead of stale valid-looking data.

**Reference:** macOS `MallocScribble=1`, Zig's `undefined` memory fill pattern (0xAA).

---

## Priority Order

| # | Fix | Impact | Effort | Status |
|---|-----|--------|--------|--------|
| 1 | Signal handler calls backtrace | Critical | 5 min | DONE (51546d1) |
| 2 | ARC pointer range check | Critical | 30 min | DONE (25ece9c) |
| 3 | ARC diagnostic messages | High | 1 hour | DONE (3b87ffc) |
| 4 | Phase tracking in selfcot | High | 30 min | DONE (b954e2f) |
| 5 | Refcount underflow detection | Medium | 30 min | Not started |
| 6 | Function name in @trap | Medium | 15 min | DONE (9a8106b) |
| 7 | Debug build mode | Low | 1 hour | Not started |
| 8 | MallocScribble in dealloc | Low | 30 min | DONE (9a8106b) |

## Key Reference Files

| Reference | File | Pattern |
|-----------|------|---------|
| Swift pointer validation | `references/swift/stdlib/public/core/EmbeddedRuntime.swift:431-439` | `isValidPointerForNativeRetain`: null check + range check + immortal check |
| Swift retain/release | `references/swift/stdlib/public/runtime/HeapObject.cpp:548-551` | Check before decrement, not during |
| Go signal handler | `references/go/src/runtime/signal_unix.go` | Signal → traceback → exit |
| Go backtrace | `references/go/src/runtime/traceback.go` | Walk stack frames, print function + file + line |
| Cot signal handler | `compiler/codegen/native/signal_native.zig:62-188` | Missing backtrace call |
| Cot ARC magic | `compiler/codegen/native/arc_native.zig:399-430` | Magic check dereferences bad pointers |
| Cot pipeline debug | `compiler/pipeline_debug.zig` | COT_DEBUG env var for phase logging |
