# Bootstrap Bugs: Execution Plan

**Date:** 2026-03-20
**Status:** 40/40 files compile. Selfcot compiles itself to Wasm. Wasm output invalid.

---

## Bug 1: Function Index Mapping Defaults to 0 (fd_write)

**Severity:** Critical — blocks all Wasm output from running
**Location:** Selfcot's Wasm codegen function index resolution

### Evidence

```
wasmtime compile /tmp/selfcot2.wasm
Error: Invalid input WebAssembly code at offset 43745: type mismatch: expected i32, found i64
```

Analysis:
- Function 84 (`StringBuilder_append`) calls function index 0 (`fd_write`) with i64 args
- `fd_write` expects `(i32, i32, i32, i32) -> i32`
- 997 calls to function 0 in selfcot2.wasm (should be ~2-10)
- Function 0 is `wasi_snapshot_preview1.fd_write` — the first WASI import
- All unmapped runtime functions (memcpy, alloc, realloc, string_concat, etc.) fall back to index 0

### Root Cause

When selfcot's Wasm codegen can't find a function name in `func_indices`, it returns index 0 instead of reporting an error. This is the same bug documented in CLAUDE.md for the Zig compiler:

> "If a function name is missing from `func_indices`, `wasm_gen.zig` silently calls function index 0 (alloc) — a silent bug."

### Zig Compiler Reference

**File:** `compiler/codegen/wasm/gen.zig` — `getFuncIndex(name)`
**File:** `compiler/driver.zig` — `func_indices` population (lines 6100-6200)

The Zig compiler builds `func_indices` by:
1. Registering WASI imports (fd_write, fd_read, proc_exit, etc.)
2. Registering runtime functions (alloc, dealloc, memcpy, string_concat, etc.)
3. Registering user functions
4. Each gets a sequential function index

### Fix

**File:** `self/emit/wasm/driver.cot` — Find where `func_indices` is built

1. Audit which runtime function names are registered in selfcot's func_indices
2. Compare against the Zig compiler's function index list (`compiler/driver.zig`)
3. Add any missing entries (memcpy, alloc, realloc, string_concat, retain, release, etc.)
4. Add an error/warning when a function name isn't found (instead of silent fallback to 0)

### Debugging Steps

```bash
# List all function names in selfcot2.wasm
wasm-objdump -x /tmp/selfcot2.wasm | grep "func\[" | head -30

# Compare against Zig compiler output
./zig-out/bin/cot build self/test_tiny.cot --target=wasm -o /tmp/tiny_zig.wasm
wasm-objdump -x /tmp/tiny_zig.wasm | grep "func\[" | head -30

# Find which functions are mapped to index 0 in selfcot
wasm-objdump -d /tmp/selfcot2.wasm | grep "call 0 " | wc -l
```

### Expected Fix Size

~20-50 lines: add missing function name → index entries to selfcot's func_indices builder.

---

## Bug 2: Missing Optimizer Pass Invocations

**Severity:** Medium — affects code quality, not correctness
**Location:** `self/emit/wasm/driver.cot` lines 358-363

### Evidence

Current pass pipeline:
```
passRewriteGeneric → decompose → passRewriteDec → schedule → layout → lower
```

Zig compiler pipeline (`compiler/driver.zig:6158-6167`):
```
copyelim → passRewriteGeneric → decompose → passRewriteDec → copyelim → cse → deadcode → schedule → layout → lower
```

### Missing Passes

1. `copyelim()` — called TWICE (before rewriteGeneric and after rewriteDec)
2. `cse()` — Common Subexpression Elimination
3. `deadcode()` — Dead Code Elimination

### Fix

**File:** `self/emit/wasm/driver.cot` — Add pass calls in correct order
**File:** `self/main.cot` — Add imports for optimize/copyelim, optimize/cse, optimize/deadcode

### Priority

Fix AFTER Bug 1. Without working Wasm output, optimizer correctness can't be tested. The missing passes cause larger/slower Wasm but don't cause validation errors.

---

## Bug 3: Wasm Output Doesn't Print (fd_write not connected)

**Severity:** High — blocks testing of Wasm output
**Dependency:** Likely a symptom of Bug 1 (function index mapping)

### Evidence

```bash
/tmp/selfcot build /tmp/test_hello.cot -o /tmp/hello.wasm
wasmtime /tmp/hello.wasm  # no output
```

### Root Cause (hypothesis)

Same as Bug 1: the `println` runtime function resolves to `fd_write` → `print_int` → `alloc` etc. through a chain of calls. If any link in this chain maps to function index 0, the call chain breaks.

### Fix

Fix Bug 1 first. If printing still doesn't work after Bug 1 is fixed, investigate the WASI import/export wiring in `self/emit/wasm/wasi.cot` and `self/emit/wasm/link.cot`.

---

## Implementation Order

1. **Bug 1** (function index mapping) — Critical, blocks everything
2. **Bug 3** (printing) — Likely resolved by Bug 1 fix
3. **Bug 2** (optimizer passes) — Code quality, not blocking

## Key Files

| File | Purpose |
|------|---------|
| `self/emit/wasm/driver.cot` | Wasm codegen driver — func_indices builder + pass pipeline |
| `self/emit/wasm/gen.cot` | Wasm code generation — instruction emission |
| `self/emit/wasm/link.cot` | Wasm module linking — function index assignment |
| `self/emit/wasm/wasi.cot` | WASI import declarations |
| `compiler/driver.zig` | Zig compiler reference — func_indices population |
| `compiler/codegen/wasm/gen.zig` | Zig compiler reference — getFuncIndex |
