# Handoff: Remove Native Codegen from libcot

**Date:** 2026-03-30
**Status:** NEW FILES WRITTEN, BUILD IS CLEAN, DRIVER NOT YET REWIRED
**Priority:** CRITICAL — this completes the clean library boundary

---

## Current State (BUILD IS CLEAN)

The build passes. 78/78 e2e tests pass. 370/370 features pass. v0.4.1 is released.

The old code path is still active — driver.zig imports from `codegen/native/` and `ir/clif/`. The new CIR generators exist alongside the old code but are not wired in yet.

---

## The Problem

libcot has 74K lines of code that violates the library boundary:

```
zig/libcot/codegen/native/     — 68K lines of hand-ported Cranelift (compile.zig, machinst/, isa/, regalloc/)
zig/libcot/ir/clif/            — 6K lines of CLIF IR data structures
```

libcot should produce CIR bytes ONLY. Native codegen belongs in libclif (Rust or Zig).

The pipeline should be:
```
libcot: Cot source → parse → check → lower → SSA → CIR bytes
rust/libclif: CIR bytes → Cranelift IR → native .o
```

Currently it's:
```
libcot: Cot source → parse → check → lower → SSA → CLIF IR → CIR bytes (user funcs)
libcot: ALSO builds CLIF IR for runtime funcs → compiles via hand-ported backend → runtime .o
rust/libclif: CIR bytes → Cranelift IR → user .o
linker: user .o + runtime .o → executable
```

---

## New Files Already Written (6 files, ~6K lines)

These are COMPLETE and COMPILE. They replace the old two-step pipeline (SSA→CLIF IR→CIR) with a direct SSA→CIR emitter, and replace the 5 runtime generators to emit CIR instead of CLIF IR.

### 1. `zig/libcot/codegen/ssa_to_cir.zig` (2,254 lines)

Direct SSA → CIR emitter. Replaces:
- `codegen/native/ssa_to_clif.zig` (2,119 lines — SSA → CLIF IR)
- `codegen/native/cir_write.zig` (815 lines — CLIF IR → CIR bytes)

Contains `CirWriter` struct (CIR binary format builder) and `translateFunc()`/`translateModule()` functions. Every SSA op is mapped directly to a CIR opcode — no intermediate CLIF IR.

**API:**
```zig
const ssa_to_cir = @import("codegen/ssa_to_cir.zig");

// Translate a single SSA function, appending to an existing CirWriter
ssa_to_cir.translateFunc(
    allocator,
    &writer,          // *CirWriter — shared across all functions
    ssa_func,         // *ssa_mod.Func
    ir_func,          // *ir_mod.Func
    type_reg,         // *TypeRegistry
    func_index_map,   // *StringHashMapUnmanaged(u32)
    ir_funcs,         // []const ir_mod.Func
    string_data_symbol_idx, // ?u32
    ctxt_symbol_idx,  // u32
    global_symbol_map, // *StringHashMapUnmanaged(u32)
);
```

### 2. `zig/libcot/codegen/arc_runtime.zig` (1,646 lines)

ARC runtime as CIR. Replaces `codegen/native/arc_native.zig` (2,783 lines).

17 functions: alloc, dealloc, alloc_raw, realloc_raw, dealloc_raw, retain, release, cot_realloc, string_concat, string_eq, unowned_retain, unowned_release, unowned_load_strong, weak_form_reference, weak_retain, weak_release, weak_load_strong.

**API:** `arc_runtime_cir.generate(writer: *CirWriter) void`

### 3. `zig/libcot/codegen/io_runtime.zig` (847 lines)

I/O runtime as CIR. Replaces `codegen/native/io_native.zig` (3,178 lines).

~30 functions: fd_write, fd_read, fd_close, exit, fd_open, time, random, growslice, nextslicecap, args/environ accessors, directory ops, filesystem ops, network ops, event loop, process ops.

**API:** `io_runtime_cir.generate(writer: *CirWriter, argc_idx, argv_idx, envp_idx, lib_mode, target_os) void`

### 4. `zig/libcot/codegen/print_runtime_native.zig` (398 lines)

Print runtime as CIR. Replaces `codegen/native/print_native.zig` (706 lines).

6 functions: print_int, eprint_int, int_to_string, print_float, eprint_float, float_to_string.

**API:** `print_runtime_cir.generate(writer: *CirWriter) void`

### 5. `zig/libcot/codegen/test_runtime_native.zig` (323 lines)

Test runner as CIR. Replaces `codegen/native/test_native.zig` (492 lines).

6 functions: __test_begin, __test_print_name, __test_pass, __test_fail, __test_summary, __test_store_fail_values.

**API:** `test_runtime_cir.generate(writer: *CirWriter) void`

### 6. `zig/libcot/codegen/signal_runtime.zig` (536 lines)

Signal handler as CIR. Replaces `codegen/native/signal_native.zig` (1,697 lines).

5 functions: __cot_print_hex, __cot_signal_handler, __cot_install_signals, __cot_print_backtrace, __cot_print_source_loc.

**API:** `signal_runtime_cir.generate(writer: *CirWriter) void`

### 7. `zig/libcot/codegen/libclif.zig` (54 lines)

C ABI binding to `rust/libclif`. Copied from `codegen/native/libclif.zig`. Just extern fn declarations for `clif_compile`, `clif_free`, `clif_version`.

---

## Known Issues in New Generators

The new generator files have Zig 0.15 compile errors that must be fixed:

1. **Pointless discards**: Lines like `_ = varname;` where the variable IS used later. Zig 0.15 rejects these. Fix: delete the discard line.

2. **Unused local constants**: Dead code in generated functions (values computed but never used). Fix: delete or use.

3. **Unused function parameters**: Parameters declared but not used in function body. Fix: prefix with `_`.

These are in: `arc_runtime.zig`, `io_runtime.zig`, `print_runtime_native.zig`, `test_runtime_native.zig`, `signal_runtime.zig`.

To find them all: temporarily add `_ = @import("codegen/arc_runtime.zig");` etc. to main.zig and run `zig build`.

---

## Step-by-Step Plan

### Step 1: Fix Zig 0.15 Errors in New Generators

Add test imports to `main.zig` to force compilation:
```zig
test {
    _ = @import("codegen/ssa_to_cir.zig");
    _ = @import("codegen/arc_runtime.zig");
    _ = @import("codegen/io_runtime.zig");
    _ = @import("codegen/print_runtime_native.zig");
    _ = @import("codegen/test_runtime_native.zig");
    _ = @import("codegen/signal_runtime.zig");
}
```

Run `zig build` and fix all errors. Remove the test imports after.

### Step 2: Rewrite generateNativeCodeViaCIR

Location: `driver.zig` line ~1725, function `generateNativeCodeViaCIR`.

**Current flow** (lines 1725-2090):
1. Build `func_index_map`, `string_offsets`, etc. (lines 1730-1870) — KEEP
2. Build resolver from combined func_index_map + global_symbol_map (lines 1870-1892) — DELETE (resolver is for CLIF→CIR, we do SSA→CIR now)
3. Create CirWriter, for each func: build SSA → ssa_to_clif → cir_write (lines 1894-1964) — REPLACE
4. Call clif_compile for user .o (lines 1969-1980) — KEEP but combine with runtime
5. Generate runtime functions via arc_native/io_native/etc (lines 1985-2053) — REPLACE with CIR generators
6. Call generateCIRRuntimeObj for runtime .o (lines 2069-2088) — DELETE (no separate runtime .o)
7. Return user .o + set cir_runtime_obj_path (lines 2089-2095) — SIMPLIFY (one .o, no runtime path)

**New flow:**
```zig
fn generateNativeCodeViaCIR(self, funcs, globals, type_reg) ![]u8 {
    // 1. Build func_index_map, string_offsets (SAME AS BEFORE)

    // 2. Create ONE CirWriter for everything
    var writer = ssa_to_cir.CirWriter.init(self.allocator);
    defer writer.deinit();

    // 3. For each user function: SSA → CIR
    for (funcs) |*ir_func| {
        // Build SSA (same as before)
        var ssa_builder = try ssa_builder_mod.SSABuilder.init(func_alloc, ir_func, globals, type_reg, self.target);
        const ssa_func = try ssa_builder.build();
        // Run SSA passes (same as before)
        try rewritegeneric.rewrite(...);
        try decompose_builtin.decompose(...);
        // ... other passes ...
        // Translate SSA → CIR directly
        try ssa_to_cir.translateFunc(self.allocator, &writer, ssa_func, ir_func, type_reg, &func_index_map, funcs, string_data_symbol_idx, ctxt_symbol_idx, &global_symbol_map);
    }

    // 4. Append runtime functions as CIR
    arc_runtime_cir.generate(&writer);
    io_runtime_cir.generate(&writer, ...);
    print_runtime_cir.generate(&writer);
    signal_runtime_cir.generate(&writer);
    if (self.test_mode) test_runtime_cir.generate(&writer);

    // 5. Serialize and compile
    const cir_bytes = writer.finish();
    const libclif_mod = @import("codegen/libclif.zig");
    const obj_bytes = try libclif_mod.compileToObject(cir_bytes, target_str);

    // 6. Return single .o (no separate runtime .o)
    const result = try self.allocator.alloc(u8, obj_bytes.len);
    @memcpy(result, obj_bytes);
    libclif_mod.freeObjectBytes(obj_bytes);
    return result;
}
```

### Step 3: Update main.zig

Remove the runtime .o linking code:
```zig
// DELETE these lines (around line 1495):
if (compile_driver.cir_runtime_obj_path) |rtp| {
    link_args.append(allocator, rtp) catch { ... };
}
```

Remove `cir_runtime_obj_path` field from Driver struct.

### Step 4: Handle --backend=zig

The `--backend=zig` path uses `generateNativeCode` which calls the hand-ported backend directly. Two options:

**Option A (recommended):** Keep `--backend=zig` working. The old `generateNativeCode` function stays but moves its imports inline (lazy imports using `@import` inside the function body, not at file scope). This way the hand-ported backend code in `codegen/native/` is only loaded when `--backend=zig` is used.

**Option B:** Remove `--backend=zig` entirely. Delete `generateNativeCode`. This means only `--backend=cranelift` works.

### Step 5: Remove old driver.zig imports

Delete from the import section at top of driver.zig:
```zig
// DELETE all of these:
const ssa_to_clif = @import("codegen/native/ssa_to_clif.zig");
const arc_native = @import("codegen/native/arc_native.zig");
const io_native = @import("codegen/native/io_native.zig");
const print_native = @import("codegen/native/print_native.zig");
const test_native_rt = @import("codegen/native/test_native.zig");
const signal_native = @import("codegen/native/signal_native.zig");
const native_compile = @import("codegen/native/compile.zig");
const wasm_parser = @import("codegen/native/wasm_parser.zig");
const clif = @import("ir/clif/mod.zig");
const macho = @import("codegen/native/macho.zig");
const elf = @import("codegen/native/elf.zig");
const object_module = @import("codegen/native/object_module.zig");
const dwarf_mod = @import("codegen/native/dwarf.zig");
const buffer_mod = @import("codegen/native/machinst/buffer.zig");
```

Replace with:
```zig
const ssa_to_cir = @import("codegen/ssa_to_cir.zig");
const arc_runtime_cir = @import("codegen/arc_runtime.zig");
const io_runtime_cir = @import("codegen/io_runtime.zig");
const print_runtime_cir = @import("codegen/print_runtime_native.zig");
const test_runtime_cir = @import("codegen/test_runtime_native.zig");
const signal_runtime_cir = @import("codegen/signal_runtime.zig");
const libclif = @import("codegen/libclif.zig");
```

If keeping `--backend=zig`, add lazy imports inside the zig backend function instead.

### Step 6: Delete codegen/native/ and ir/clif/

```bash
rm -rf zig/libcot/codegen/native/
rm -rf zig/libcot/ir/clif/
rm -f zig/libcot/ir/clif  # in case it's a file not dir
```

Also handle:
- `main.zig` line ~1811: `@import("codegen/native_e2e_test.zig")` — move this test file to `zig/libclif/` or delete it (its tests are redundant with cot test suite)
- `ir/clif/types.zig` line 301: `@import("../../codegen/native/machinst/reg.zig").RegClass` — this is in the ir/clif copy that's being deleted, not relevant

### Step 7: Verify

```bash
zig build                                              # Compiles
zig build test                                         # Unit tests
./zig-out/bin/cot test test/e2e/features.cot           # 370/370
./test/run_all.sh                                      # 78/78 e2e files
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot  # selfcot builds
/tmp/selfcot version                                    # works
```

### Step 8: Measure

```bash
# libcot should be ~72K lines now (was 146K)
find zig/libcot -name "*.zig" -exec cat {} + | wc -l

# No native codegen in libcot
find zig/libcot -name "*.zig" | xargs grep -l "machinst\|regalloc\|isa/aarch64\|isa/x64" | wc -l
# Should be 0
```

---

## What NOT to Do

1. **Don't delete zig/libclif/** — it's the standalone hand-ported Cranelift backend. It's a CORRECT copy.
2. **Don't modify rust/libclif/** — it's the production Cranelift backend. It just compiles CIR.
3. **Don't touch codegen/wasm/** — Wasm codegen is separate and working.
4. **Don't touch stdlib/** — unrelated.
5. **Don't run `git checkout` or `git restore`** — CLAUDE.md prohibits destructive git operations.
6. **Don't add temporary `std.debug.print`** — use `pipeline_debug.zig`.

---

## References

- `claude/CIR_FORMAT_SPEC.md` — CIR binary format specification
- `claude/RELEASE_0_4_1.md` — release plan (architecture diagram)
- `rust/libclif/src/cir.rs` — CIR reader (Rust side)
- `rust/libclif/src/translate.rs` — CIR → Cranelift translator (Rust side)
- `zig/libclif/cir_read.zig` — CIR reader (Zig side)
- `zig/libclif/cir_translate.zig` — CIR → hand-ported CLIF translator (Zig side)
