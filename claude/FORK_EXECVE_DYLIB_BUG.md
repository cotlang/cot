# Bug: environ_count/len/ptr crash in Dylib mode — Uninitialized envp

## Status: ROOT CAUSE IDENTIFIED (Feb 27, 2026)

## Summary

When Cot code compiled as a dylib (`cot build --lib`) calls `environ_count()`, the process crashes with SIGSEGV. This affects ALL stdlib process functions that call `buildEnvp()` (which calls `environ_count()`): `run()`, `run0()`, `run2()`, `output()`, `output0()`, etc.

**Original hypothesis was wrong**: The crash is NOT caused by stale vmctx registers after fork. Fork/execve works correctly from dylibs — vmctx is preserved via the x21 pinned register and COW pages.

**Actual root cause**: The `environ_count()` runtime function reads the envp pointer from `vmctx + 0x30010`, but the dylib C-ABI wrapper (`generateLibWrappersMachO`) never initializes this field. It's zero (from `@memset(vmctx_data, 0)`), so `environ_count` dereferences NULL → SIGSEGV.

## Impact — Blocks Cotty PTY Spawning

This bug blocks Cotty's PTY shell spawning from Cot code. Currently Cotty works around this by spawning the shell from Swift using `forkpty()` instead of Cot's `Pty.spawn()`.

## What Works and What Doesn't from Dylib

| Operation | Status | Notes |
|-----------|--------|-------|
| fork() + exit(42) | WORKS | waitpid returns 42 correctly |
| fork() + alloc + execve | WORKS | Manual buffer construction + execve succeeds |
| fork() + buildPath + argvBuild1 + emptyEnvp() + execve | WORKS | Returns correct exit code |
| environ_count() | CRASHES | SIGSEGV — envp pointer at vmctx+0x30010 is NULL |
| buildEnvp() | CRASHES | Calls environ_count() |
| run0(), run(), output(), etc. | CRASHES | All call buildEnvp() |

## Root Cause

### vmctx + 0x30000 Area (argc/argv/envp)

In **executable mode**, the main wrapper (`generateMainWrapperMachO`) initializes:
```asm
// driver.zig ~line 2944-2951
str x19, [x10]      // argc at vmctx + 0x30000
str x20, [x10, #8]  // argv at vmctx + 0x30008
str x21, [x10, #16] // envp at vmctx + 0x30010
```

In **dylib mode**, the lib wrapper (`generateLibWrappersMachO`) only initializes:
```asm
// driver.zig ~line 3310-3316
add x8, x0, #0x20, lsl #12  // heap setup pointer
add x9, x0, #0x40, lsl #12  // linmem base
str x9, [x8]                  // store heap base
// NO argc/argv/envp initialization
```

### environ_count() Dereferences NULL

```asm
arm64_environ_count:
  add x8, x0, #0x30, lsl #12  // x8 = vmctx + 0x30000
  ldr x9, [x8, #16]           // x9 = *(vmctx + 0x30010) = envp = 0 (NULL!)
  movz x0, #0                 // count = 0
.loop:
  ldr x10, [x9, x0, lsl #3]  // SIGSEGV: load from NULL + 0
```

## Fix

### Option A: Initialize envp in lib wrapper from C `environ` (Recommended)

Add to `generateLibWrappersMachO` (after heap base setup):

```asm
// Load C environ global (char **environ)
adrp x10, _environ@GOTPAGE
ldr  x10, [x10, _environ@GOTPAGEOFF]
ldr  x10, [x10]                        // x10 = environ pointer

// Store at vmctx + 0x30010
add  x11, x0, #0x30, lsl #12           // x11 = vmctx + 0x30000
str  x10, [x11, #16]                   // envp = environ
str  xzr, [x11]                        // argc = 0
str  xzr, [x11, #8]                    // argv = NULL
```

This uses the C runtime's `environ` global variable, which is available in any process that links libc. The host process (Swift app) has its environment set up, so `environ` points to valid env vars.

**Files to modify:**
- `compiler/driver.zig` — `generateLibWrappersMachO` (~line 3310): add envp initialization

### Option B: Null-check in environ_count/len/ptr (Simpler but less correct)

Add a `cbz x9, .done` after loading the envp pointer:

```asm
arm64_environ_count:
  add x8, x0, #0x30, lsl #12
  ldr x9, [x8, #16]
  cbz x9, .done              // NULL envp → return 0
  movz x0, #0
.loop:
  ...
```

**Files to modify:**
- `compiler/driver.zig` — `arm64_environ_count`, `arm64_environ_len`, `arm64_environ_ptr` (lines ~1925-1994)
- Same for x64 equivalents (lines ~3792-3870)

### Option C: Both (Belt and suspenders)

Do Option A (initialize envp properly) AND Option B (null-check for safety). This ensures environ works AND prevents crashes if envp somehow becomes NULL.

## Verification Test

```bash
# Build test dylib
cat > /tmp/test_envp.cot << 'EOF'
import "std/sys"
import "std/process"
export fn testRun0() i64 { return run0("/usr/bin/false") }
export fn testEnvCount() i64 { return environ_count() }
EOF
cot build /tmp/test_envp.cot --lib -o /tmp/libtest_envp.dylib

# Build host
cat > /tmp/test_host.c << 'EOF'
#include <stdio.h>
#include <dlfcn.h>
int main() {
    void *lib = dlopen("/tmp/libtest_envp.dylib", RTLD_NOW);
    long (*fn_count)(void) = dlsym(lib, "testEnvCount");
    long (*fn_run)(void) = dlsym(lib, "testRun0");
    printf("environ_count: %ld\n", fn_count());  // Should NOT crash
    printf("run0(/usr/bin/false): %ld\n", fn_run());  // Should return 1
    dlclose(lib);
}
EOF
cc -o /tmp/test_host /tmp/test_host.c && /tmp/test_host
```

## Previous Analysis (Partially Incorrect)

The original bug document hypothesized that vmctx register preservation after fork was the issue. Investigation on Feb 27, 2026 proved this was wrong:

1. **vmctx IS preserved after fork** — the pinned register x21 (callee-saved) survives the fork syscall, and the COW pages preserve the vmctx data
2. **fork + alloc + execve works** — tested with manual buffer construction from a dylib, returns correct exit codes
3. **The crash is in environ_count(), not in alloc() or execve()** — the envp pointer at vmctx+0x30010 is simply never initialized in dylib mode

## Relationship to Other Bugs

- **Not related to ARC overhaul** — this is a dylib initialization issue
- **Not related to STRUCT_RETURN_CODEGEN_BUG.md** — different subsystem
- **Standalone executable works** — the main wrapper initializes envp correctly
