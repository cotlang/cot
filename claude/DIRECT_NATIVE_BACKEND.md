# Direct SSA → CLIF Native Backend

## Current Status (Feb 26, 2026)

**The direct native backend is the DEFAULT for all native targets.** The `--direct-native` flag is no longer needed — native compilation automatically uses the direct SSA → CLIF path. Wasm targets (`--target=wasm32`) still use the original Wasm pipeline.

### Test Results

| Target | Tests | Status |
|--------|-------|--------|
| Native (direct) | 133/341 | 39% — crashes at `list_struct_8_fields` |
| Wasm | 341/341 | All pass (unchanged) |
| Compiler unit tests | All pass | `zig build test` green |

### What's Working (133 tests)

- Constants, arithmetic (int + float), comparisons, bitwise, type conversions
- Control flow: if/else, while loops, switch, br_table, phi nodes
- Function calls (direct `static_call`)
- Stack slots, loads/stores with offsets
- Structs (pass/return, SRET for large structs)
- Move/zero (memcpy/memset)
- String literals in `.rodata` data section
- ARC: retain/release, alloc/dealloc/realloc
- Destructor dispatch (Swift `_swift_release_dealloc` pattern with `call_indirect`)
- Error unions, defer, unions with switch/capture
- Deinit methods (basic, mixed, reuse)
- I/O: fd_write, fd_read, fd_close, exit, fd_seek
- Print: print_int, eprint_int
- Test runner: __test_begin, __test_print_name, __test_pass/fail, __test_summary
- **Function pointers**: `addr` op → `func_addr` CLIF → ADRP+ADD on ARM64
- **Indirect calls**: `closure_call` (callee pinned to x16), `inter_call`
- **Closures with captures**: context via `_cot_ctxt` global (mirrors Wasm CTXT global)
- **Generics**: generic functions + structs (monomorphized)
- **Traits**: trait bounds, self type
- **Match expressions**: wildcards, guards, ranges
- **Tuples**: basic, nested, three-element
- **SRET**: tuple return, struct return, chaining
- **Collections (List)**: append, get, free, clear, insert, remove, reverse, clone, contains, indexOf, equal, compact, resize, delete_range, etc.

### What's Blocking (Test 134+)

Test 134 (`list_struct_8_fields`) crashes with SIGSEGV. This is a struct with a `string` field and 8 fields total (64 bytes) — likely a large-struct-in-collection issue.

| Feature | Impact | Difficulty |
|---------|--------|------------|
| **Large structs with string fields in collections** | ~10+ tests | Medium — memcpy/layout issue |
| **String comparison** | Some tests | Investigate — may need string_eq wiring |
| **Global variables** | `global_addr` SSA op | Medium — data section symbol |
| **Metadata system** | Destructor correctness | Medium — wire metadata_addrs |
| **Missing runtime functions** | Various | Low — add as CLIF IR |

### Completed Runtime Functions

| Module | Functions | Implementation |
|--------|-----------|---------------|
| `arc_native.zig` | alloc, dealloc, retain, release, realloc, string_concat, string_eq | CLIF IR → native |
| `io_native.zig` | fd_write, fd_read, fd_close, exit, fd_seek, memset_zero | CLIF IR → libc |
| `print_native.zig` | print_int, eprint_int | CLIF IR → libc write |
| `test_native.zig` | __test_begin, __test_print_name, __test_pass, __test_fail, __test_summary, __test_store_fail_values | CLIF IR |

### Not Yet Compiled (Registered but Stub)

These are in `runtime_func_names` but not compiled as CLIF IR. They will cause linker errors if used:

`fd_open`, `time`, `random`, `int_to_string`, `growslice`, `nextslicecap`

### Native ARC Header Layout (24 bytes)

Different from Wasm path (16 bytes). Native uses i64 metadata for 64-bit function pointers:

```
Offset 0:  alloc_size (i64) — total allocation including header
Offset 8:  metadata   (i64) — HeapMetadata* (full 64-bit pointer)
Offset 16: refcount   (i64) — reference count
Offset 24: user_data  [...] — actual object data
```

Wasm path: `total_size(i32):0, metadata(i32):4, refcount(i64):8, user_data:16`

### Next Steps (Priority Order)

1. **Debug `list_struct_8_fields` crash** — large struct (64 bytes) with string field in List generic, likely memcpy or field offset issue
2. **Implement `global_addr`** — data section symbol for module-level globals
3. **Wire metadata system** — pass `metadata_addrs` to ssa_to_clif, build metadata data section
4. **Implement missing runtime functions** — fd_open, time, random, int_to_string, growslice, nextslicecap
5. **Implement `bounds_check`** — array/slice bounds checking (currently no-op)
6. **String operations** — string comparison, concatenation in more contexts

### Key Implementation Details

**Closure context (`_cot_ctxt`):** Native closures use a global variable `_cot_ctxt` (8 bytes in `.data` section) to pass the closure environment pointer, mirroring the Wasm CTXT global (index 1). The caller stores the context pointer before `call_indirect`, and the closure body reads it via `wasm_global_get(1)` → `global_value` + `load`.

**Function pointers (`addr` op):** The `addr` SSA op emits `func_addr` CLIF instruction, which the ARM64 backend lowers to `load_ext_name_near` (ADRP+ADD pair). The callee register for indirect calls is pinned to x16 (IP0 scratch) to avoid conflicts with argument registers.

**Indirect call args convention:**
- `closure_call`: `args[0]=callee`, `args[1]=context_ptr`, `args[2..]=function_args`
- `inter_call`: `args[0]=callee`, `args[1..]=function_args`

---

## Overview

This document specifies the direct SSA → CLIF native compilation path, which replaces the Wasm-based native pipeline. The goal: eliminate the Wasm intermediate representation for native targets so that pointers are real 64-bit native addresses, enabling natural FFI with Swift/C hosts.

**Active pipeline (native targets, DEFAULT):**
```
Cot Source → SSA → lower_native → CLIF IR → VCode → regalloc → emit → Mach-O/ELF
```

**Wasm pipeline (browser/WASI targets, --target=wasm32):**
```
Cot Source → SSA → lower_wasm → Wasm bytecode
```

**Legacy pipeline (Wasm AOT, cot build <.wasm>):**
```
.wasm file → wasm_parser → wasm_to_clif → CLIF IR → VCode → regalloc → emit → Mach-O/ELF
```

Everything downstream of CLIF IR is unchanged: `compile.zig` (BlockLoweringOrder → Lower CLIF to VCode → regalloc → emit machine code) stays exactly as-is.

---

## Section 1: Architecture Overview

### What Changes

| Component | Before | After |
|---|---|---|
| SSA lowering pass | `lower_wasm.zig` (generic → wasm_* ops) | `lower_native.zig` (generic → native_* ops) — **NEW FILE** |
| SSA → IR translation | `wasm_to_clif/translator.zig` (Wasm stack machine → CLIF) | `ssa_to_clif.zig` (SSA directly → CLIF) — **NEW FILE** |
| Memory model | 256MB linear memory, 32-bit offsets, vmctx, bounds checks | Native 64-bit pointers, system malloc/mmap, no vmctx |
| Function signatures | `(callee_vmctx, caller_vmctx, ...params) → results` | `(...params) → results` (standard C ABI) |
| Globals | vmctx + 0x10000 + idx*16 loads | Module-level static symbols (GOT or direct) |
| ARC runtime | Wasm bytecode (arc.zig) → Wasm → CLIF | CLIF IR generated directly, calling system malloc/free |
| I/O runtime | Wasm stubs + ARM64 overrides in driver.zig | Direct libc/syscall calls from CLIF IR |
| Main wrapper | Allocates 256MB vmctx, copies data segments | Simple `_main` that calls `__cot_main` directly |
| Lib wrappers | C-ABI wrappers that shift vmctx params | Not needed — functions already have C ABI |

### What Stays the Same

- `compile.zig` — CLIF → VCode → regalloc → machine code (100% unchanged)
- `object_module.zig` — Mach-O/ELF object file generation (unchanged)
- All frontend stages: Scanner → Parser → Checker → IR → SSA → SSA passes
- The Wasm codegen path (`lower_wasm.zig`, `wasm_to_clif/`, `wasm_parser.zig`) for browser/WASI targets

### File Map

| Status | File | Description |
|---|---|---|
| **DONE** | `compiler/ssa/passes/lower_native.zig` | Lowers generic SSA ops to native-ready forms |
| **DONE** | `compiler/codegen/native/ssa_to_clif.zig` | Translates SSA → CLIF IR (~1400 lines, 120+ ops) |
| **DONE** | `compiler/codegen/native/arc_native.zig` | ARC runtime as CLIF IR (~840 lines, 7 functions) |
| **DONE** | `compiler/codegen/native/io_native.zig` | I/O runtime as CLIF IR (6 libc-forwarding functions) |
| **DONE** | `compiler/codegen/native/print_native.zig` | Print runtime as CLIF IR (print_int, eprint_int) |
| **DONE** | `compiler/codegen/native/test_native.zig` | Test runner as CLIF IR (6 functions) |
| **DONE** | `compiler/driver.zig` | `generateNativeCodeDirect()` + `generateMachODirect()` |
| **DONE** | `compiler/main.zig` | Direct native default for non-wasm targets |
| KEEP (Wasm) | `compiler/ssa/passes/lower_wasm.zig` | Still used for `--target=wasm32` |
| KEEP (Wasm) | `compiler/codegen/native/wasm_to_clif/` | Powers `cot build <.wasm>` AOT path |
| KEEP | `compiler/codegen/native/compile.zig` | CLIF → machine code (unchanged) |

---

## Section 2: The SSA → CLIF Translator

This is the core of the migration. Currently, translating SSA to CLIF requires two steps:
1. `lower_wasm.zig`: SSA generic ops → Wasm-specific ops
2. `translator.zig`: Wasm ops → CLIF IR (via stack machine emulation)

The new path composes these two steps into one: SSA generic ops → CLIF IR directly. No stack machine, no Wasm encoding/decoding, no bounds checking.

### Design: Reuse Generic Ops

Instead of creating `native_*` op variants like the `wasm_*` variants, the native path can work directly with the **generic SSA ops** (add, sub, load, store, etc.) and the **sized ops** (add32, add64, load8, etc.). The `lower_native.zig` pass only needs to handle a few transformations:

- `neg` → `sub(0, x)` (CLIF has `ineg` for this)
- `not` → `bxor(x, -1)` (CLIF has `bnot`)
- `bool_not` → `icmp(eq, x, 0)`
- `retain` → call to native retain function
- `release` → call to native release function
- `static_call` / `closure_call` / `inter_call` → lowered call forms
- `nil_check` → `trapz`
- `move` → lowered memcpy
- `zero` → lowered memset

### Complete Op → CLIF Mapping Table

This table maps every generic/sized SSA op to the CLIF instruction(s) it should emit. The `ssa_to_clif.zig` translator walks each SSA block's values and emits the corresponding CLIF.

#### Constants

| SSA Op | CLIF Emission | Notes |
|---|---|---|
| `const_int`, `const_64` | `builder.ins().iconst(I64, v.aux_int)` | |
| `const_32` | `builder.ins().iconst(I32, v.aux_int)` | |
| `const_8`, `const_16` | `builder.ins().iconst(I32, v.aux_int)` | Small constants use I32 |
| `const_bool` | `builder.ins().iconst(I64, v.aux_int)` | 0 or 1 |
| `const_float` | `builder.ins().f64const(v.aux_float)` | |
| `const_nil` | `builder.ins().iconst(I64, 0)` | Nil = 0 |
| `const_string` | `builder.ins().globalValue(I64, gv)` where `gv` is a symbol referencing the string's read-only data section entry | In Wasm path, `aux` is a linear memory offset; in native path, it becomes a symbol reference to `.rodata` / `__TEXT,__const`. The SSA `aux.string` field identifies the literal; `ssa_to_clif.zig` maps it to a `GlobalValueData.symbol`. |
| `const_ptr` | `builder.ins().iconst(I64, v.aux_int)` | Native pointer value |

#### Integer Arithmetic

| SSA Op | CLIF Emission | Notes |
|---|---|---|
| `add`, `add64` | `builder.ins().iadd(lhs, rhs)` | CLIF infers I64 from operands |
| `sub`, `sub64` | `builder.ins().isub(lhs, rhs)` | |
| `mul`, `mul64` | `builder.ins().imul(lhs, rhs)` | |
| `div` | `guard_zero_divisor(rhs)` + `builder.ins().sdiv(lhs, rhs)` | Signed div |
| `udiv` | `guard_zero_divisor(rhs)` + `builder.ins().udiv(lhs, rhs)` | Unsigned div |
| `mod` | `guard_zero_divisor(rhs)` + `builder.ins().srem(lhs, rhs)` | Signed remainder |
| `umod` | `guard_zero_divisor(rhs)` + `builder.ins().urem(lhs, rhs)` | Unsigned remainder |
| `neg` | `builder.ins().ineg(arg)` | `ineg` added to `FuncInstBuilder` (Section 2B done) |
| `add32` | `builder.ins().iadd(lhs, rhs)` | Operands are I32 |
| `sub32` | `builder.ins().isub(lhs, rhs)` | |
| `mul32` | `builder.ins().imul(lhs, rhs)` | |
| `add8`, `add16` | `builder.ins().iadd(lhs, rhs)` | I32 operands |
| `sub8`, `sub16` | `builder.ins().isub(lhs, rhs)` | |
| `mul8`, `mul16` | `builder.ins().imul(lhs, rhs)` | |

#### Bitwise

| SSA Op | CLIF Emission | Notes |
|---|---|---|
| `and_`, `and64` | `builder.ins().band(lhs, rhs)` | |
| `or_`, `or64` | `builder.ins().bor(lhs, rhs)` | |
| `xor`, `xor64` | `builder.ins().bxor(lhs, rhs)` | |
| `shl`, `shl64` | `builder.ins().ishl(lhs, rhs)` | |
| `shr`, `shr64` | `builder.ins().ushr(lhs, rhs)` | Unsigned (logical) shift right |
| `sar`, `sar64` | `builder.ins().sshr(lhs, rhs)` | Signed (arithmetic) shift right |
| `not` | `builder.ins().bnot(arg)` | `bnot` added to `FuncInstBuilder` (Section 2B done) |
| `bool_not` | `icmp(eq, arg, iconst(0))` | Matches Go's OpNot → OpWasmI64Eqz |
| `and32` | `builder.ins().band(lhs, rhs)` | I32 operands |
| `or32` | `builder.ins().bor(lhs, rhs)` | |
| `xor32` | `builder.ins().bxor(lhs, rhs)` | |
| `shl32` | `builder.ins().ishl(lhs, rhs)` | |
| `shr32` | `builder.ins().ushr(lhs, rhs)` | |
| `sar32` | `builder.ins().sshr(lhs, rhs)` | |
| `and8`, `and16` | `builder.ins().band(lhs, rhs)` | I32 operands |
| `or8`, `or16` | `builder.ins().bor(lhs, rhs)` | |
| `xor8`, `xor16` | `builder.ins().bxor(lhs, rhs)` | |
| `shl8`, `shl16` | `builder.ins().ishl(lhs, rhs)` | |
| `shr8`, `shr16` | `builder.ins().ushr(lhs, rhs)` | |
| `sar8`, `sar16` | `builder.ins().sshr(lhs, rhs)` | |
| `com8`, `com16`, `com32`, `com64` | `builder.ins().bnot(arg)` | Bitwise complement. `bnot` added to `FuncInstBuilder` (Section 2B done) |
| `clz64` | `builder.ins().clz(arg)` | I64 |
| `ctz64` | `builder.ins().ctz(arg)` | I64 |
| `popcnt64` | `builder.ins().popcnt(arg)` | I64 |
| `clz32` | `builder.ins().clz(arg)` | I32 |
| `ctz32` | `builder.ins().ctz(arg)` | I32 |
| `popcnt32` | `builder.ins().popcnt(arg)` | I32 |

#### Integer Comparisons

| SSA Op | CLIF Emission |
|---|---|
| `eq`, `eq64` | `builder.ins().icmp(IntCC.eq, lhs, rhs)` |
| `ne`, `ne64` | `builder.ins().icmp(IntCC.ne, lhs, rhs)` |
| `lt`, `lt64` | `builder.ins().icmp(IntCC.slt, lhs, rhs)` |
| `le`, `le64` | `builder.ins().icmp(IntCC.sle, lhs, rhs)` |
| `gt`, `gt64` | `builder.ins().icmp(IntCC.sgt, lhs, rhs)` |
| `ge`, `ge64` | `builder.ins().icmp(IntCC.sge, lhs, rhs)` |
| `ult` | `builder.ins().icmp(IntCC.ult, lhs, rhs)` |
| `ule` | `builder.ins().icmp(IntCC.ule, lhs, rhs)` |
| `ugt` | `builder.ins().icmp(IntCC.ugt, lhs, rhs)` |
| `uge` | `builder.ins().icmp(IntCC.uge, lhs, rhs)` |
| `eq32` | `builder.ins().icmp(IntCC.eq, lhs, rhs)` |
| `ne32` | `builder.ins().icmp(IntCC.ne, lhs, rhs)` |
| `lt32` | `builder.ins().icmp(IntCC.slt, lhs, rhs)` |
| `le32` | `builder.ins().icmp(IntCC.sle, lhs, rhs)` |
| `gt32` | `builder.ins().icmp(IntCC.sgt, lhs, rhs)` |
| `ge32` | `builder.ins().icmp(IntCC.sge, lhs, rhs)` |
| `eq8`, `eq16` | `builder.ins().icmp(IntCC.eq, lhs, rhs)` |
| `ne8`, `ne16` | `builder.ins().icmp(IntCC.ne, lhs, rhs)` |

#### Float Arithmetic

Note: Unlike integer ops which auto-infer type from operands, float ops require an explicit `ty` parameter.

| SSA Op | CLIF Emission |
|---|---|
| `add64f` | `builder.ins().fadd(F64, lhs, rhs)` |
| `sub64f` | `builder.ins().fsub(F64, lhs, rhs)` |
| `mul64f` | `builder.ins().fmul(F64, lhs, rhs)` |
| `div64f` | `builder.ins().fdiv(F64, lhs, rhs)` |
| `neg64f` | `builder.ins().fneg(F64, arg)` |
| `sqrt64f` | `builder.ins().sqrt(F64, arg)` |
| `add32f` | `builder.ins().fadd(F32, lhs, rhs)` |
| `sub32f` | `builder.ins().fsub(F32, lhs, rhs)` |
| `mul32f` | `builder.ins().fmul(F32, lhs, rhs)` |
| `div32f` | `builder.ins().fdiv(F32, lhs, rhs)` |
| `neg32f` | `builder.ins().fneg(F32, arg)` |
| `sqrt32f` | `builder.ins().sqrt(F32, arg)` |

#### Float Comparisons

| SSA Op | CLIF Emission |
|---|---|
| `eq64f` | `builder.ins().fcmp(FloatCC.eq, lhs, rhs)` |
| `ne64f` | `builder.ins().fcmp(FloatCC.ne, lhs, rhs)` |
| `lt64f` | `builder.ins().fcmp(FloatCC.lt, lhs, rhs)` |
| `le64f` | `builder.ins().fcmp(FloatCC.le, lhs, rhs)` |
| `gt64f` | `builder.ins().fcmp(FloatCC.gt, lhs, rhs)` |
| `ge64f` | `builder.ins().fcmp(FloatCC.ge, lhs, rhs)` |
| `eq32f` | `builder.ins().fcmp(FloatCC.eq, lhs, rhs)` |
| `ne32f` | `builder.ins().fcmp(FloatCC.ne, lhs, rhs)` |
| `lt32f` | `builder.ins().fcmp(FloatCC.lt, lhs, rhs)` |
| `le32f` | `builder.ins().fcmp(FloatCC.le, lhs, rhs)` |
| `gt32f` | `builder.ins().fcmp(FloatCC.gt, lhs, rhs)` |
| `ge32f` | `builder.ins().fcmp(FloatCC.ge, lhs, rhs)` |

#### Type Conversions

| SSA Op | CLIF Emission |
|---|---|
| `sign_ext32to64` | `builder.ins().sextend(I64, arg)` |
| `zero_ext32to64` | `builder.ins().uextend(I64, arg)` |
| `trunc64to32` | `builder.ins().ireduce(I32, arg)` |
| `sign_ext8to16` | `builder.ins().sextend(I16, arg)` |
| `sign_ext8to32` | `builder.ins().sextend(I32, arg)` |
| `sign_ext8to64` | `builder.ins().sextend(I64, arg)` |
| `sign_ext16to32` | `builder.ins().sextend(I32, arg)` |
| `sign_ext16to64` | `builder.ins().sextend(I64, arg)` |
| `zero_ext8to16` | `builder.ins().uextend(I16, arg)` |
| `zero_ext8to32` | `builder.ins().uextend(I32, arg)` |
| `zero_ext8to64` | `builder.ins().uextend(I64, arg)` |
| `zero_ext16to32` | `builder.ins().uextend(I32, arg)` |
| `zero_ext16to64` | `builder.ins().uextend(I64, arg)` |
| `trunc16to8` | `builder.ins().ireduce(I8, arg)` |
| `trunc32to8` | `builder.ins().ireduce(I8, arg)` |
| `trunc32to16` | `builder.ins().ireduce(I16, arg)` |
| `trunc64to8` | `builder.ins().ireduce(I8, arg)` |
| `trunc64to16` | `builder.ins().ireduce(I16, arg)` |
| `cvt64to64f` | `builder.ins().fcvtFromSint(F64, arg)` |
| `cvt32to64f` | `builder.ins().fcvtFromSint(F64, arg)` |
| `cvt64fto64` | `builder.ins().fcvtToSint(I64, arg)` |
| `cvt64fto32` | `builder.ins().fcvtToSint(I32, arg)` |
| `cvt32fto64f` | `builder.ins().fpromote(F64, arg)` |
| `cvt64fto32f` | `builder.ins().fdemote(F32, arg)` |
| `cvt64to32f` | `builder.ins().fcvtFromSint(F32, arg)` |
| `cvt32fto64` | `builder.ins().fcvtToSint(I64, arg)` |
| `cvt32to32f` | `builder.ins().fcvtFromSint(F32, arg)` |
| `cvt32fto32` | `builder.ins().fcvtToSint(I32, arg)` |
| `convert` | Dispatches based on `v.aux_type` to appropriate conversion above | Generic convert |

#### High Multiply and Divmod

| SSA Op | CLIF Emission | Notes |
|---|---|---|
| `hmul32` | `imul(sextend(I64, lhs), sextend(I64, rhs))` + `ushr(result, 32)` | High 32 bits of signed 32×32 multiply |
| `hmul32u` | `imul(uextend(I64, lhs), uextend(I64, rhs))` + `ushr(result, 32)` | High 32 bits of unsigned 32×32 multiply |
| `hmul64` | Widening multiply (no `smulhi` in CLIF — **requires extension**) | High 64 bits of signed 64×64 multiply |
| `hmul64u` | Widening multiply (no `umulhi` in CLIF — **requires extension**) | High 64 bits of unsigned 64×64 multiply |
| `divmod32` | `sdiv(lhs, rhs)` + `srem(lhs, rhs)` → make_tuple | Signed divmod, returns (quotient, remainder) |
| `divmod64` | `sdiv(lhs, rhs)` + `srem(lhs, rhs)` → make_tuple | |
| `divmodu32` | `udiv(lhs, rhs)` + `urem(lhs, rhs)` → make_tuple | Unsigned divmod |
| `divmodu64` | `udiv(lhs, rhs)` + `urem(lhs, rhs)` → make_tuple | |

#### Atomics (Future — Not Yet Used)

These ops exist in op.zig but are not yet used by the Cot compiler. **Note:** These CLIF builder methods do not exist yet — only MachInst-level atomic instructions exist (ARM64: `atomic_rmw`, `atomic_cas`; x64: `atomic_rmw_seq`). New CLIF opcodes, instruction data variants, and CLIF→MachInst lowering rules must be added before these can be used. When needed:

| SSA Op | CLIF Emission | Notes |
|---|---|---|
| `atomic_load32` | `builder.ins().atomic_load(I32, addr, 0)` | Atomic load |
| `atomic_load64` | `builder.ins().atomic_load(I64, addr, 0)` | |
| `atomic_store32` | `builder.ins().atomic_store(val, addr, 0)` | Atomic store |
| `atomic_store64` | `builder.ins().atomic_store(val, addr, 0)` | |
| `atomic_add32` | `builder.ins().atomic_rmw(I32, AtomicRmwOp.Add, addr, val)` | Atomic read-modify-write |
| `atomic_add64` | `builder.ins().atomic_rmw(I64, AtomicRmwOp.Add, addr, val)` | |
| `atomic_cas32` | `builder.ins().atomic_cas(I32, addr, expected, new)` | Compare-and-swap |
| `atomic_cas64` | `builder.ins().atomic_cas(I64, addr, expected, new)` | |
| `atomic_exchange32` | `builder.ins().atomic_rmw(I32, AtomicRmwOp.Xchg, addr, val)` | Atomic exchange |
| `atomic_exchange64` | `builder.ins().atomic_rmw(I64, AtomicRmwOp.Xchg, addr, val)` | |

#### Memory Operations — THE KEY CHANGE

In the Wasm path, every load/store goes through `boundsCheckAndComputeAddr()`:
```
addr = heap_base + index + offset  (with bounds check)
```

In the native path, addresses are **direct native pointers**. No heap_base, no bounds check:
```
addr = base_ptr + offset  (direct native address)
```

| SSA Op | CLIF Emission (Native) | Notes |
|---|---|---|
| `load`, `load64` | `builder.ins().load(I64, MemFlags.DEFAULT, addr, offset)` | addr is native ptr; **float-typed values use `load(F64, ...)`** — check `isFloatType(v.type_idx)` |
| `load32` | `builder.ins().load(I32, MemFlags.DEFAULT, addr, offset)` | |
| `load32s` | `load(I32, ...) + sextend(I64, ...)` | Sign-extending load |
| `load16` | `load(I16, ...) + uextend(I64, ...)` | Zero-extending load |
| `load16s` | `load(I16, ...) + sextend(I64, ...)` | Sign-extending load |
| `load8` | `load(I8, ...) + uextend(I64, ...)` | Zero-extending load |
| `load8s` | `load(I8, ...) + sextend(I64, ...)` | Sign-extending load |
| `store`, `store64` | `builder.ins().store(MemFlags.DEFAULT, val, addr, offset)` | **float-typed values use F64 store** — check `isFloatType(v.args[1].type_idx)` |
| `store32` | `ireduce(I32, val) + store(...)` | Truncating store |
| `store16` | `ireduce(I16, val) + store(...)` | |
| `store8` | `ireduce(I8, val) + store(...)` | |

**Address computation ops:**

| SSA Op | CLIF Emission (Native) | Notes |
|---|---|---|
| `addr` | `builder.ins().globalValue(I64, gv)` where `gv` has `GlobalValueData.symbol` | Symbol address (GOT entry or direct) |
| `local_addr` | `builder.ins().stack_addr(I64, stack_slot, 0)` | Stack variable address — **requires adding `stack_addr` to FuncInstBuilder** (see CLIF Extensions) |
| `global_addr` | `builder.ins().globalValue(I64, gv)` where `gv` has `GlobalValueData.symbol` | Module-level static |
| `metadata_addr` | `builder.ins().globalValue(I64, gv)` where `gv` has `GlobalValueData.symbol` | Type metadata pointer (native function pointer to `TypeName_deinit`, resolved via relocation at link time) |
| `off_ptr` | `builder.ins().iadd_imm(base, offset)` | Base + constant offset. `iadd_imm` added to `FuncInstBuilder` (Section 2B done) |
| `add_ptr` | `builder.ins().iadd(base, index)` | Base + dynamic offset |
| `sub_ptr` | `builder.ins().isub(base, index)` | Base - dynamic offset |

#### Control Flow

SSA control flow maps to CLIF blocks directly. This is much simpler than the Wasm path because SSA already has explicit block structure and phi nodes — no need for the Wasm stack machine emulation.

| SSA Construct | CLIF Emission |
|---|---|
| SSA Block | `builder.createBlock()` + `builder.switchToBlock(block)` |
| `phi` | `builder.appendBlockParam(block, ty)` — phis become block params |
| `arg` | Block param at entry block (function argument) |
| `copy` | Pass value directly (no CLIF instruction needed) |
| Block terminator: branch to one target | `builder.ins().jump(target, args)` |
| Block terminator: conditional branch | `builder.ins().brif(cond, then_block, then_args, else_block, else_args)` |
| Block terminator: return | `builder.ins().return_(return_values)` |
| Block terminator: unreachable | `builder.ins().trap(TrapCode.unreachable_code_reached)` |

**SSA→CLIF block translation algorithm:**
```
For each SSA function:
  1. Create CLIF entry block, append function params as block params
  2. Create a CLIF block for each SSA block
  3. Map SSA phi values → CLIF block params on target blocks
  4. For each SSA block:
     a. switchToBlock(clif_block)
     b. For each value in block: emit CLIF instruction, store result in value→clif_value map
     c. Emit block terminator (jump/brif/return/trap)
  5. sealBlock all blocks (after all predecessors are known)
```

#### Function Calls — NO MORE vmctx

| SSA Op | CLIF Emission (Native) | Notes |
|---|---|---|
| `static_call` | `builder.ins().call(func_ref, args)` | Direct call, no vmctx prepended. Returns `struct { inst, results }` — access results via `.results[0]`, `.results[1]`, etc. |
| `call` | `builder.ins().call(func_ref, args)` | Same — direct call |
| `closure_call` | `builder.ins().call_indirect(sig, callee_ptr, args)` | Indirect via function pointer. Same `struct { inst, results }` return. |
| `inter_call` | `builder.ins().call_indirect(sig, callee_ptr, args)` | Interface dispatch |
| `tail_call` | `call + return` | (Or: `builder.ins().return_call` when available) |

Function signatures in the native path use standard C ABI:
```
// OLD (Wasm path): signature includes vmctx params
sig = (callee_vmctx: I64, caller_vmctx: I64, param0: I64, param1: I64) -> I64

// NEW (Native path): no vmctx, standard C ABI
sig = (param0: I64, param1: I64) -> I64
```

#### ARC Operations

| SSA Op | CLIF Emission (Native) |
|---|---|
| `retain` | `builder.ins().call(retain_func_ref, &[_]{arg})` |
| `release` | `builder.ins().call(release_func_ref, &[_]{arg})` |

#### Safety Checks

| SSA Op | CLIF Emission |
|---|---|
| `nil_check` | `builder.ins().trapz(ptr, TrapCode.null_reference)` |
| `is_non_nil` | `builder.ins().icmp(IntCC.ne, ptr, iconst(0))` |
| `is_nil` | `builder.ins().icmp(IntCC.eq, ptr, iconst(0))` |
| `bounds_check` | `icmp(IntCC.uge, idx, len) + trapnz(cond, TrapCode.heap_oob)` |
| `slice_bounds` | `icmp(IntCC.ugt, new_len, cap) + trapnz(cond, TrapCode.heap_oob)` | Slice capacity check |

#### Tuple/Select/String/Slice

| SSA Op | CLIF Emission | Notes |
|---|---|---|
| `cond_select` | `builder.ins().select(ty, cond, val1, val2)` | |
| `select0` | First result of multi-return call | |
| `select1` | Second result of multi-return call | |
| `select_n` | Nth result | |
| `make_tuple` | Bundle multiple values (no CLIF instruction) | |
| `string_ptr` | `load(I64, str_ptr, 0)` | Load ptr field from string struct |
| `string_len` | `load(I64, str_ptr, 8)` | Load len field |
| `string_make` | Store ptr+len into struct | |
| `string_concat` | `call(string_concat_func, ...)` | |
| `slice_ptr` | `load(I64, slice_ptr, 0)` | |
| `slice_len` | `load(I64, slice_ptr, 8)` | |
| `slice_cap` | `load(I64, slice_ptr, 16)` | |
| `slice_make` | Store ptr+len+cap into struct | |

#### Data Movement

| SSA Op | CLIF Emission |
|---|---|
| `move` | `call(memcpy_func, dst, src, size)` |
| `zero` | `call(memset_zero_func, ptr, size)` |
| `store_wb` | Same as `store` + possible write barrier (future GC) |

#### Ops That Don't Generate CLIF

| SSA Op | Handling |
|---|---|
| `init_mem` | No-op (memory state tracking) |
| `invalid` | Should never appear |
| `var_def`, `var_live`, `var_kill` | SSA bookkeeping, no code |
| `store_reg`, `load_reg` | Register allocator ops, handled by regalloc |
| `sp`, `store_sp` | Wasm stack pointer ops — **eliminated** in native path |
| `fwd_ref` | Resolved before codegen |

---

## Section 2B: CLIF Extensions Required

All Phase 1 extensions have been implemented and are passing tests.

### Completed

| Method | Status | Files Modified |
|---|---|---|
| `ineg(arg) → Value` | **Done** | `frontend.zig` — added `ineg()` to `FuncInstBuilder`, delegates to `Unary(.ineg, ...)` with auto type inference. |
| `bnot(arg) → Value` | **Done** | `frontend.zig` — added `bnot()` to `FuncInstBuilder`, delegates to `Unary(.bnot, ...)` with auto type inference. |
| `stack_addr(ty, slot, offset) → Value` | **Done** | Full stack: `.stack_addr` opcode in `instructions.zig`, `stackAddr()` on `FuncBuilder` (reuses `.stack_load` InstructionData with `.stack_addr` opcode), `stackAddr()` on `FuncInstBuilder`, ARM64 lowering emits `load_addr` (ADD rd, sp, #offset), x64 lowering emits `lea`. |
| `iaddImm(arg, imm) → Value` | **Done** | Full stack: `.iadd_imm` opcode in `instructions.zig`, `iaddImm()` on `FuncBuilder` using `binary_imm64` format, `iaddImm()` on `FuncInstBuilder` with auto type inference + `BinaryImm64` helper. ARM64 lowering tries imm12 ADD, falls back to SUB for negative, then movz/movk + ADD rrr for large constants. x64 lowering uses ADD r/m64,imm32 when fits in i32, otherwise materializes 64-bit constant via MOV then ADD. |

### Future (When Needed)

| Method | Implementation |
|---|---|
| `smulhi(lhs, rhs) → Value` | New opcode + lowering rules. Needed for `hmul64`. Not urgent — currently unused. |
| `umulhi(lhs, rhs) → Value` | Same, for `hmul64u`. |
| `atomic_load(ty, addr, offset) → Value` | CLIF atomic opcodes don't exist yet. MachInst-level implementations do (ARM64 `atomic_rmw`, x64 `atomic_rmw_seq`). Need new opcodes + CLIF→MachInst lowering. |
| `atomic_store`, `atomic_rmw`, `atomic_cas` | Same — MachInst exists, CLIF bridge needed. |

---

## Section 3: Native Memory Model

### No VMContext

The entire vmctx concept is eliminated for native targets:

| Wasm Path | Native Path |
|---|---|
| vmctx pinned in x21 (ARM64) | No vmctx register |
| Globals at vmctx + 0x10000 + idx*16 | Module-level statics (data segment symbols) |
| Heap metadata at vmctx + 0x20000 | Not needed |
| Linear memory at vmctx + 0x40000 | Not needed — native pointers |
| Heap allocations at vmctx + 0x800000 | System malloc/mmap |
| 256MB virtual memory region | Process address space (unlimited) |

### Native Pointers Everywhere

All pointers are 64-bit native addresses. `*T` in Cot maps to a real native pointer. This means:
- FFI pointers from Swift/C work natively — no address translation
- No 256MB memory limit
- No bounds checking overhead (safety checks are opt-in via `bounds_check` ops)
- System allocator (malloc/free) instead of bump allocator

### Global Variables

In the Wasm path, globals are stored in the vmctx region and accessed via `global_value + load/store` chains that go through vmctx offsets:
```
// OLD: vmctx-based global access
vmctx_gv = get vmctx base
addr_gv = iadd_imm(vmctx_gv, 0x10000 + idx * 16)
val = load(I64, addr_gv, 0)
```

In the native path, globals become module-level static symbols:
```
// NEW: symbol-based global access (using GlobalValueData.symbol)
gv = create_global_value(GlobalValueData { .symbol = "_global_N" })
addr = globalValue(I64, gv)
val = load(I64, addr, 0)
```

Alternatively, for simpler initial implementation, globals can live in a single static data section with known offsets (similar to the vmctx layout but as a regular data symbol):
```
// SIMPLER: single globals section
gv = create_global_value(GlobalValueData { .symbol = "_cot_globals" })
globals_addr = globalValue(I64, gv)
val = load(I64, globals_addr, idx * 16)
```

### Stack Variables

Wasm locals are currently translated to CLIF Variables (via declareVar/defVar/useVar). In the native path, SSA values naturally map to CLIF values. Stack-allocated variables (struct instances, arrays) use CLIF stack slots:
```
slot = builder.create_sized_stack_slot(StackSlotData { size: N, kind: .explicit })
addr = builder.ins().stack_addr(I64, slot, 0)  // implemented (Section 2B done)
```

### String Literals and Data Segments

String literals and other constant data currently live in the Wasm data segment at known offsets into linear memory. In the native path:
- String literal data goes into a read-only data section (`.rodata` / `__TEXT,__const`)
- Each string literal gets a `GlobalValueData.symbol` reference
- `const_string` emits `globalValue(I64, gv)` to get the native address
- The SSA `aux.string` field identifies the literal; `ssa_to_clif.zig` maps it to the corresponding data section symbol

---

## Section 4: ARC Runtime — IMPLEMENTED

The native ARC runtime (`arc_native.zig`) generates CLIF IR compiled through `native_compile.compile()`. Uses libc malloc/free (no custom freelist). All 7 functions are working and passing tests.

### Memory Layout (Native 64-bit — 24 bytes)

Each ARC-managed object has a 24-byte header followed by user data:
```
+0   alloc_size: i64    (total allocation including header, for realloc)
+8   metadata:   i64    (HeapMetadata* — full 64-bit pointer for destructor dispatch)
+16  refcount:   i64    (reference count; IMMORTAL = 0x7FFFFFFFFFFFFFFF)
+24  user_data:  [...]  (actual object fields)
```

This differs from the Wasm path (16-byte header with i32 fields) because:
- Native needs i64 metadata for 64-bit function pointers in destructor dispatch
- Swift HeapObject uses a full pointer for metadata

Constants (in `arc_native.zig`):
- `HEAP_OBJECT_HEADER_SIZE` = 24
- `SIZE_OFFSET` = 0
- `METADATA_OFFSET` = 8
- `REFCOUNT_OFFSET` = 16
- `IMMORTAL_REFCOUNT` = 0x7FFFFFFFFFFFFFFF
- `METADATA_DESTRUCTOR_OFFSET` = 8 (within metadata struct)

### Function Signatures (Native C ABI) — ALL IMPLEMENTED

All functions are compiled as CLIF IR via `arc_native.zig`. Uses libc malloc/free (Option A from original plan). No custom freelist (deferred optimization).

| Function | Signature | Status | Notes |
|---|---|---|---|
| `alloc` | `(metadata: i64, size: i64) → i64` | **Done** | malloc + 24-byte header init |
| `retain` | `(obj: i64) → i64` | **Done** | null/immortal checks, rc++ |
| `release` | `(obj: i64) → void` | **Done** | Destructor dispatch via call_indirect |
| `dealloc` | `(obj: i64) → void` | **Done** | null check, free(obj - 24) |
| `realloc` | `(obj: i64, new_size: i64) → i64` | **Done** | fits check, alloc+memcpy+dealloc |
| `string_concat` | `(s1_ptr, s1_len, s2_ptr, s2_len) → i64` | **Done** | alloc + two memcpy calls |
| `string_eq` | `(s1_ptr, s1_len, s2_ptr, s2_len) → i64` | **Done** | len check + memcmp + brif |
| `memset_zero` | `(ptr: i64, size: i64) → void` | **Done** | In io_native.zig, calls libc memset |
| `memcpy` | N/A — direct libc external | **Done** | Not a wrapper (avoids infinite recursion) |

### Function-by-Function Specification

#### `alloc(metadata: i64, size: i64) → i64`

```
total_size = size + HEAP_OBJECT_HEADER_SIZE  // 16
total_size = (total_size + 7) & ~7           // align to 8

// Try size-class freelist first (4 classes: ≤32, ≤80, ≤272, ≤1040)
class_idx = size_to_class(total_size)
if (class_idx >= 0) {
    head = load(freelist_globals[class_idx])
    if (head != 0) {
        next = load_i32(head + FREELIST_NEXT_OFFSET)
        store(freelist_globals[class_idx], next)
        goto init_header
    }
}

// Fallback: system malloc
ptr = call libc malloc(total_size)
if (ptr == 0) trap  // OOM

init_header:
store_i32(ptr + SIZE_OFFSET, total_size)
store_i32(ptr + METADATA_OFFSET, metadata)
store_i64(ptr + REFCOUNT_OFFSET, INITIAL_REFCOUNT)  // 1
return ptr + USER_DATA_OFFSET  // return pointer to user data
```

**Key difference from Wasm version**: Uses system `malloc` instead of bump allocator with `memory.grow`. The freelist approach is kept for fast reuse of common sizes.

#### `retain(obj: i64) → i64`

```
if (obj == 0) return 0                          // null check
header = obj - USER_DATA_OFFSET
refcount = load_i64(header + REFCOUNT_OFFSET)
if (refcount == IMMORTAL_REFCOUNT) return obj   // immortal check
store_i64(header + REFCOUNT_OFFSET, refcount + 1)
return obj
```

#### `release(obj: i64) → void` — IMPLEMENTED with Destructor Dispatch

The release function has 8 CLIF blocks implementing the full Swift `_swift_release_dealloc` pattern:

```
if (obj == 0) return                            // block_entry → block_return
header = obj - 24
refcount = load_i64(header + 16)
if (refcount >= IMMORTAL) return                // block_check_immortal → block_return

new_refcount = refcount - 1
store_i64(header + 16, new_refcount)
if (new_refcount != 0) return                   // block_decrement → block_return

// Destructor dispatch (Swift _swift_release_dealloc pattern)
metadata_ptr = load_i64(header + 8)             // block_check_metadata
if (metadata_ptr == 0) goto dealloc
destructor = load_i64(metadata_ptr + 8)         // block_check_destructor
if (destructor == 0) goto dealloc
call_indirect(destructor, obj)                  // block_call_destructor — native function pointer call

dealloc:
call dealloc(obj)                               // block_dealloc
```

**Key difference from Wasm**: metadata is a full i64 native pointer (not i32 Wasm table index). The destructor is loaded as a function pointer and called via CLIF `call_indirect` (which ARM64 lowering handles as `blr xN`). No Wasm function table indirection.

**Note**: The metadata system is not yet wired up in `ssa_to_clif.zig` — all `metadata_addr` SSA ops currently emit 0. When wired up, destructors will automatically be dispatched because the release() code structure is already correct.

#### `dealloc(obj: i64) → void`

```
header = obj - USER_DATA_OFFSET
total_size = load_i32(header + SIZE_OFFSET)
class_idx = size_to_class(total_size)

if (class_idx >= 0) {
    // Return to freelist
    head = load(freelist_globals[class_idx])
    store_i32(header + FREELIST_NEXT_OFFSET, head)
    store(freelist_globals[class_idx], header)
} else {
    // Large allocation: free to system
    call libc free(header)
}
```

#### `realloc(obj: i64, new_size: i64) → i64`

```
header = obj - USER_DATA_OFFSET
old_total = load_i32(header + SIZE_OFFSET)
new_total = new_size + HEAP_OBJECT_HEADER_SIZE
new_total = (new_total + 7) & ~7

if (new_total <= old_total) return obj  // fits in current block

// Allocate new, copy, dealloc old
metadata = load_i32(header + METADATA_OFFSET)
new_obj = call alloc(metadata, new_size)
old_data_size = old_total - HEAP_OBJECT_HEADER_SIZE
call memcpy(new_obj, obj, old_data_size)
call dealloc(obj)
return new_obj
```

#### `string_concat(s1_ptr, s1_len, s2_ptr, s2_len) → i64`

```
total_len = s1_len + s2_len
if (total_len == 0) return 0

// Allocate buffer for concatenated string
new_ptr = call alloc(0, total_len)
call memcpy(new_ptr, s1_ptr, s1_len)
call memcpy(new_ptr + s1_len, s2_ptr, s2_len)
return new_ptr
```

#### `string_eq(s1_ptr, s1_len, s2_ptr, s2_len) → i64`

```
if (s1_len != s2_len) return 0
if (s1_len == 0) return 1

// Byte-by-byte comparison
i = 0
while (i < s1_len) {
    if (load_u8(s1_ptr + i) != load_u8(s2_ptr + i)) return 0
    i += 1
}
return 1
```

#### `memset_zero(ptr, size) → void`

```
i = 0
while (i < size) {
    store_u8(ptr + i, 0)
    i += 1
}
```

#### `memcpy(dst, src, len) → void`

```
if (len == 0) return
if (dst > src) {
    // Backward copy (overlapping regions)
    i = len - 1
    while (i >= 0) {
        store_u8(dst + i, load_u8(src + i))
        i -= 1
    }
} else {
    // Forward copy
    i = 0
    while (i < len) {
        store_u8(dst + i, load_u8(src + i))
        i += 1
    }
}
```

### Freelist Globals

The 4 size-class freelist globals become module-level static data:
```
_cot_freelist_0: i64 = 0   // class 0: total_size ≤ 32
_cot_freelist_1: i64 = 0   // class 1: total_size ≤ 80
_cot_freelist_2: i64 = 0   // class 2: total_size ≤ 272
_cot_freelist_3: i64 = 0   // class 3: total_size ≤ 1040
```

Note: In the Wasm path these are i32 globals (because pointers are 32-bit offsets, verified in `arc.zig` line 190: `.val_type = .i32`). In the native path they are i64 (native pointer width). The metadata field in the header also widens from i32 to i64 to hold native function pointers.

---

## Section 5: I/O and Print Runtime

### Current State

The I/O runtime has two layers:
1. **Wasm stubs** (`print_runtime.zig`, `wasi_runtime.zig`): generate Wasm bytecode for `write`, `print_int`, `fd_read`, `fd_close`, etc.
2. **ARM64 overrides** (in `driver.zig` `generateMachO()`): replace the Wasm stub code with hand-coded ARM64 syscall sequences for ~30 functions.

The ARM64 overrides assume the Wasm memory model: they read `vmctx + 0x40000` to convert Wasm pointer offsets to native addresses.

### Native Path Changes

In the native path, I/O functions receive **native pointers directly** — no pointer translation needed.

**Option A: Direct syscalls via hand-coded ARM64/x64**

Keep the current approach of hand-coded machine code stubs, but remove the vmctx pointer translation:

```
// OLD (Wasm): convert wasm offset to native address
add x8, x0, #0x40, lsl #12    // x8 = vmctx + 0x40000 (linmem base)
add x1, x8, x3                // x1 = linmem + wasm_ptr

// NEW (Native): pointer is already native
mov x1, x3                    // x1 = native_ptr (no translation)
```

Affected functions: `write/fd_write`, `fd_read`, `fd_close`, `fd_seek`, `fd_open`, `exit`, `time`, `random`, `fork`, `execve`, `waitpid`, `pipe`, `dup2`, `isatty`, `net_socket`, `net_bind`, `net_listen`, `net_accept`, `net_connect`, `net_set_reuse_addr`, `kqueue_create`, `kevent_add`, `kevent_del`, `kevent_wait`, and more.

For each syscall stub that takes pointer arguments:
- Remove the vmctx parameter (x0 was callee_vmctx, x1 was caller_vmctx)
- Arguments shift: x0=first_arg, x1=second_arg, etc. (standard C ABI)
- Remove `add x8, x0, #0x40, lsl #12` pointer translation
- Pointer arguments are passed directly to the syscall

**Option B: Link against libc**

For macOS/Linux, link against system libc and call `write()`, `read()`, `close()`, etc. directly. The CLIF translator can emit `call` instructions to imported libc symbols.

This is simpler and more portable. The only downside is the libc dependency.

**Recommendation: Option B (libc) for most I/O, with Option A for performance-critical paths if needed.**

### `print_int` and `int_to_string`

Currently generated as Wasm bytecode in `print_runtime.zig`. For the native path:
- Generate as CLIF IR in `io_native.zig`
- Or compile from Cot source (dogfooding!)
- Or hand-code ARM64 (fast, like current approach)

The logic is simple: divide by 10 loop to produce decimal digits, call write().

---

## Section 6: driver.zig Changes

### Current Flow

```zig
fn generateCode(self, funcs, globals, type_reg, source_file, source_text) ![]u8 {
    if (self.target.isWasm()) {
        return self.generateWasmCode(funcs, type_reg);
    }
    // Native: Wasm → Native
    const wasm_bytes = try self.generateWasmCode(funcs, type_reg);
    return self.generateNativeCode(wasm_bytes);
}
```

### New Flow

```zig
fn generateCode(self, funcs, globals, type_reg, source_file, source_text) ![]u8 {
    if (self.target.isWasm()) {
        return self.generateWasmCode(funcs, type_reg);
    }
    // Native: SSA → CLIF → Native (direct, no Wasm intermediate)
    return self.generateNativeCodeDirect(funcs, globals, type_reg, source_file, source_text);
}
```

### `generateNativeCodeDirect()` — New Function

This replaces `generateNativeCode(wasm_bytes)` for native targets. Instead of parsing Wasm and translating through the stack machine, it takes SSA functions directly.

```zig
fn generateNativeCodeDirect(self: *Driver, funcs: []const ir_mod.Func, globals: []const ir_mod.Global, type_reg: *types_mod.TypeRegistry, source_file: []const u8, source_text: []const u8) ![]u8 {
    // Step 1: Run lower_native pass on SSA functions
    // (Instead of lower_wasm — some ops get transformed for native codegen)
    for (ssa_funcs) |*func| {
        try lower_native.lower(func);
    }

    // Step 2: Translate each SSA function to CLIF IR
    var compiled_funcs = ArrayList(CompiledCode){};
    for (ssa_funcs, 0..) |*ssa_func, i| {
        var clif_func = clif.Function.init(self.allocator);
        defer clif_func.deinit();

        // Translate SSA → CLIF (the new translator)
        try ssa_to_clif.translate(ssa_func, &clif_func, self.allocator);

        // Step 3: Compile CLIF → machine code (unchanged pipeline)
        const compiled = try native_compile.compile(&clif_func, isa, self.allocator);
        try compiled_funcs.append(compiled);
    }

    // Step 4: Add ARC runtime functions (compiled from CLIF)
    const arc_funcs = try arc_native.generateRuntimeFunctions(self.allocator, isa);
    // ... append to compiled_funcs

    // Step 5: Add I/O runtime functions
    const io_funcs = try io_native.generateRuntimeFunctions(self.allocator, isa);
    // ... append to compiled_funcs

    // Step 6: Generate object file (Mach-O or ELF)
    return self.generateNativeMachO(compiled_funcs.items, ...);
}
```

### Main Wrapper Changes

The current `generateMainWrapperMachO()` allocates 256MB of vmctx, copies data segments, initializes globals, and calls `__wasm_main` with vmctx as the first argument.

The new main wrapper is dramatically simpler:

```
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp

    // Save argc/argv for args_count/arg_ptr functions
    // (Store in static globals)
    adrp x8, _cot_argc@PAGE
    str x0, [x8, _cot_argc@PAGEOFF]
    adrp x8, _cot_argv@PAGE
    str x1, [x8, _cot_argv@PAGEOFF]

    // Call the Cot main function directly (no vmctx!)
    bl _main__cot    // or whatever the mangled name is

    // Exit with return value
    mov x16, #1      // SYS_exit
    svc #0x80

    ldp x29, x30, [sp], #16
    ret
```

No 256MB allocation. No data segment copying. No vmctx initialization. No signal handler registration (keep that if desired, but it doesn't need vmctx).

### Lib Mode Wrapper Changes

The current lib mode generates C-ABI wrappers that shift vmctx params:
```
// OLD: wrapper shifts vmctx
_cotty_insert_char(i64 ch):
    vmctx = load _vmctx_data address
    call _cotty_insert_char__wasm(vmctx, vmctx, ch)
```

In the native path, exported functions already have C ABI — **no wrappers needed**:
```
// NEW: function IS the C-ABI entry point
_cotty_insert_char(i64 ch):
    // directly compiled from Cot source, no indirection
```

### Data Segments

String literals and other constant data that currently live in the Wasm data segment (copied into vmctx at init) instead become:
- A read-only data section in the object file
- Referenced via symbol addresses
- No runtime copying needed

### Static Data Section

For mutable global variables, create a `_cot_globals` data section:
```
_cot_globals:
    .quad <heap_ptr_init>     // global 0 (e.g., heap_ptr for ARC allocator)
    .quad <freelist_0>        // global 1
    .quad <freelist_1>        // global 2
    .quad <freelist_2>        // global 3
    .quad <freelist_3>        // global 4
    // ... user globals
```

Each global is accessed as: `globalValue(_cot_globals) + idx * 8`.

---

## Section 7: `cot build <.wasm>` — Built-in Wasm AOT Compiler

### Motivation

The current native pipeline (SSA → Wasm → parse Wasm → wasm_to_clif → CLIF → native) is being replaced with a direct path (SSA → CLIF → native). Rather than leaving the `wasm_to_clif/` code as dead weight, we expose it as a **user-facing feature**: any `.wasm` file can be AOT-compiled to a native executable.

This is what Wasmtime's `wasmtime compile` does — but built into the Cot CLI. No external toolchain needed.

### CLI

```
cot build app.wasm [-o app]           # AOT compile any .wasm to native executable
cot build app.wasm --target=aarch64   # Cross-compile Wasm to ARM64
cot build app.wasm --target=x86_64    # Cross-compile Wasm to x64
```

When `cot build` receives a `.wasm` file instead of a `.cot` file, it skips the frontend entirely and runs:
```
.wasm file → wasm_parser → wasm_to_clif → CLIF IR → VCode → regalloc → emit → Mach-O/ELF
```

This is the **exact same pipeline** that currently powers native compilation, just exposed directly.

### Implementation

The change is minimal — `driver.zig`'s `generateCode` dispatch gains a third path:

```zig
fn generateCode(self, ...) ![]u8 {
    if (self.target.isWasm()) {
        return self.generateWasmCode(funcs, type_reg);
    }
    // Direct SSA → CLIF for Cot source (new path)
    return self.generateNativeCodeDirect(funcs, globals, type_reg, source_file, source_text);
}

// Separate entry point for .wasm files (called from CLI dispatch)
fn generateNativeFromWasm(self, wasm_bytes: []const u8) ![]u8 {
    return self.generateNativeCode(wasm_bytes);  // existing pipeline, unchanged
}
```

The CLI (`cli.zig`) checks the input file extension:
- `.cot` → normal compile pipeline (frontend + backend)
- `.wasm` → `generateNativeFromWasm()` (backend only)

### What This Means for Code Organization

**Nothing is dead code.** Every file has a clear owner:

| File | Used By | Purpose |
|---|---|---|
| `wasm_to_clif/translator.zig` | `cot build <.wasm>` | Wasm stack machine → CLIF translation |
| `wasm_to_clif/func_environ.zig` | `cot build <.wasm>` | vmctx/linear memory mapping |
| `wasm_to_clif/bounds_checks.zig` | `cot build <.wasm>` | Wasm memory safety |
| `wasm_to_clif/decoder.zig` | `cot build <.wasm>` | Wasm bytecode decoder |
| `wasm_to_clif/stack.zig` | `cot build <.wasm>` | Value/control stack emulation |
| `wasm_parser.zig` | `cot build <.wasm>` | Parse Wasm module binary |
| `ssa_to_clif.zig` | `cot build <.cot>` (native) | **NEW** — direct SSA → CLIF |
| `lower_wasm.zig` | `cot build <.cot>` (wasm target) | SSA → Wasm ops |
| `lower_native.zig` | `cot build <.cot>` (native target) | **NEW** — SSA → native ops |
| `arc.zig` | `cot build <.cot>` (wasm target) + `cot build <.wasm>` | Wasm ARC runtime |
| `arc_native.zig` | `cot build <.cot>` (native target) | **NEW** — native ARC runtime |

### Functions in driver.zig

| Function | Status |
|---|---|
| `generateCode()` | **Modify**: dispatch to `generateNativeCodeDirect()` for Cot native targets |
| `generateNativeCode(wasm_bytes)` | **Keep**: powers `cot build <.wasm>` |
| `generateNativeFromWasm(wasm_bytes)` | **NEW**: public entry point for CLI `.wasm` dispatch |
| `generateNativeCodeDirect(funcs, ...)` | **NEW**: direct SSA → CLIF path |
| `generateMainWrapperMachO()` | **Keep for `cot build <.wasm>`**: 256MB vmctx needed for Wasm memory model |
| `generateLibWrappersMachO()` | **Keep for `cot build <.wasm>`**: vmctx wrappers needed for Wasm ABI |
| `generateMachO()` | **Modify**: add native-direct object generation path alongside existing Wasm-based path |
| `generateElf()` | **Modify**: same |
| `generateWasmCode()` | **Keep**: still used for `--target=wasm32` |

### Why This Is Better Than Dead Code

1. **Every line of code has a purpose** — no "kept for reference" dead code
2. **The Wasm→native pipeline is continuously tested** via `cot build <.wasm>` tests
3. **Useful feature**: compile any Wasm module (from Rust, Go, C, etc.) to native without wasmtime
4. **Migration safety**: if the new direct path has bugs, users can work around with `cot build <.cot> --target=wasm32` then `cot build output.wasm`
5. **Competitive advantage**: Cot is both a language compiler AND a Wasm AOT compiler

---

## Section 8: Phased Execution Plan

### Phase 1: Minimal SSA → CLIF Translation — COMPLETE ✓

Constants, arithmetic, return. `ssa_to_clif.zig` (~1400 lines) handles 120+ SSA ops.

### Phase 2: Control Flow — COMPLETE ✓

SSA blocks → CLIF blocks, phi → block params, brif, jump, br_table, return, trap.

### Phase 3: Memory Operations — COMPLETE ✓

All load/store sizes, stack_addr, off_ptr, add_ptr, sub_ptr, local_addr. String literals in .rodata.

**Remaining:** `global_addr` (not yet), `addr` (function addresses — not yet).

### Phase 4: Function Calls — PARTIAL (Direct calls work, indirect calls need debugging)

Direct `static_call` works. `closure_call`/`inter_call` via `emitIndirectCall()` exists but crashes on test 30 (fnptr_basic, SIGBUS). Multi-return `select0` works, `select1` needs implementation.

**Remaining:**
- Debug `closure_call` indirect call path (likely function address resolution issue)
- Implement `addr` op → `func_addr` CLIF instruction (needed for function pointers)
- Implement `select1`/`select_n` for multi-return values

### Phase 5: ARC Runtime — COMPLETE ✓

All 7 functions in `arc_native.zig` as CLIF IR: alloc, dealloc, retain, release, realloc, string_concat, string_eq. Release includes full destructor dispatch (Swift pattern). 24-byte native header with i64 metadata pointer.

**Remaining:** Wire metadata system (metadata_addr in ssa_to_clif, metadata data section in driver).

### Phase 6: driver.zig Integration — COMPLETE ✓

`generateNativeCodeDirect()` compiles user functions + runtime functions (arc → io → print → test). Direct native is default for all native targets. Simple _main wrapper, .rodata for strings. `runtime_func_names` ordering matches compiled_funcs append order.

**Remaining:** Add globals data section, wire metadata, add missing runtime functions.

### Phase 7: Verification — IN PROGRESS (29/341 E2E tests)

29/341 native E2E tests pass. 341/341 wasm tests pass. All compiler unit tests pass.

**Next milestones:**
- 36/341: After implementing `addr` op + fixing indirect calls (function pointer tests)
- 50/341: After implementing `select1` + generics support
- 100/341: After implementing `global_addr`, `bounds_check`, missing runtime functions
- 200+/341: After implementing remaining SSA ops (string ops, slice ops, etc.)
- 341/341: Full parity with Wasm path

### Phase 8: Future Optimization

Not yet started. Potential improvements after reaching full parity:

1. **Freelist allocator**: Port from arc.zig (4 size classes) for allocation-heavy code
2. **Hand-coded retain/release**: ARM64/x64 assembly for hot path (like current syscall stubs)
3. **Tail call optimization**: CLIF `return_call` for recursive functions
4. **Inline ARC ops**: Skip function call overhead for simple retain/release patterns

---

## Section 9: Reference Map

### Primary References

| What | File Path | Used For |
|---|---|---|
| SSA op definitions | `compiler/ssa/op.zig` | Complete list of all ops to translate |
| SSA → Wasm mappings | `compiler/ssa/passes/lower_wasm.zig` | What each generic op maps to (first half of composition) |
| Wasm → CLIF translation | `compiler/codegen/native/wasm_to_clif/translator.zig` | CLIF emission patterns (second half of composition) |
| CLIF FunctionBuilder API | `compiler/codegen/native/frontend/frontend.zig` (`FunctionBuilder`, `FuncInstBuilder` at line 487) | How to build CLIF IR |
| CLIF instruction builders | (via `builder.ins().iadd()`, etc.) | Available CLIF instructions |
| VMContext/linear memory | `compiler/codegen/native/wasm_to_clif/func_environ.zig` | What to eliminate |
| Bounds checking | `compiler/codegen/native/wasm_to_clif/bounds_checks.zig` | What to eliminate |
| ARC runtime logic | `compiler/codegen/arc.zig` | Function logic to rewrite |
| I/O runtime logic | `compiler/codegen/print_runtime.zig` | Function logic to rewrite |
| WASI/syscall runtime | `compiler/codegen/wasi_runtime.zig` | Syscall stubs to simplify |
| Driver pipeline | `compiler/driver.zig` | Integration point |
| CLIF → machine code | `compiler/codegen/native/compile.zig` | Unchanged downstream pipeline |
| SSA Function struct | `compiler/ssa/func.zig` | Input to translator (blocks, values, etc.) |
| SSA Value struct | `compiler/ssa/value.zig` | Value representation (op, args, aux_int, type_idx) |
| SSA Block struct | `compiler/ssa/block.zig` | Block structure (values, succs, preds, kind) |

### Cross-Reference: SSA Op → Wasm Op → CLIF Instruction

This table shows the complete composition for key ops:

| SSA Op | lower_wasm.zig | translator.zig | Direct CLIF |
|---|---|---|---|
| `const_int` | `wasm_i64_const` | `iconst(I64, val)` | `iconst(I64, val)` |
| `add` | `wasm_i64_add` | `iadd(lhs, rhs)` | `iadd(lhs, rhs)` |
| `sub` | `wasm_i64_sub` | `isub(lhs, rhs)` | `isub(lhs, rhs)` |
| `mul` | `wasm_i64_mul` | `imul(lhs, rhs)` | `imul(lhs, rhs)` |
| `div` | `wasm_i64_div_s` | `guard + sdiv(lhs, rhs)` | `guard + sdiv(lhs, rhs)` |
| `and_` | `wasm_i64_and` | `band(lhs, rhs)` | `band(lhs, rhs)` |
| `shl` | `wasm_i64_shl` | `ishl(lhs, rhs)` | `ishl(lhs, rhs)` |
| `shr` | `wasm_i64_shr_u` | `ushr(lhs, rhs)` | `ushr(lhs, rhs)` |
| `sar` | `wasm_i64_shr_s` | `sshr(lhs, rhs)` | `sshr(lhs, rhs)` |
| `eq` | `wasm_i64_eq` | `icmp(IntCC.eq, ...)` | `icmp(IntCC.eq, ...)` |
| `lt` | `wasm_i64_lt_s` | `icmp(IntCC.slt, ...)` | `icmp(IntCC.slt, ...)` |
| `ult` | `wasm_i64_lt_u` | `icmp(IntCC.ult, ...)` | `icmp(IntCC.ult, ...)` |
| `bool_not` | `wasm_i64_eqz` | `icmp(eq, arg, iconst(0))` | `icmp(eq, arg, iconst(0))` |
| `load` | `wasm_i64_load` (or `wasm_f64_load` if float-typed) | `prepareAddr + load(I64, ...)` | `load(I64, addr, offset)` (or `load(F64, ...)` if float) |
| `store` | `wasm_i64_store` (or `wasm_f64_store` if float-typed) | `prepareAddr + store(...)` | `store(val, addr, offset)` |
| `sign_ext32to64` | `wasm_i64_extend_i32_s` | `sextend(I64, arg)` | `sextend(I64, arg)` |
| `trunc64to32` | `wasm_i32_wrap_i64` | `ireduce(I32, arg)` | `ireduce(I32, arg)` |
| `static_call` | `wasm_lowered_static_call` | `call(ref, [vmctx, vmctx, ...args])` | `call(ref, args)` |
| `retain` | `wasm_lowered_retain` | `call(retain_ref, [vmctx, vmctx, obj])` | `call(retain_ref, [obj])` |
| `release` | `wasm_lowered_release` | `call(release_ref, [vmctx, vmctx, obj])` | `call(release_ref, [obj])` |
| `add64f` | `wasm_f64_add` | `fadd(F64, lhs, rhs)` | `fadd(F64, lhs, rhs)` |
| `cvt64to64f` | `wasm_f64_convert_i64_s` | `fcvtFromSint(F64, arg)` | `fcvtFromSint(F64, arg)` |
| `nil_check` | `wasm_lowered_nil_check` | (custom emission) | `trapz(ptr, TrapCode.null_reference)` |

**Key observation:** For arithmetic, bitwise, comparisons, conversions, and float ops, the direct CLIF emission is **identical** to what translator.zig already emits. The only differences are:
1. **Memory ops**: No `prepareAddr` (bounds check + heap_base translation) — direct native address
2. **Calls**: No vmctx params prepended
3. **Globals**: Symbol-based instead of vmctx-offset-based
4. **Control flow**: SSA blocks map directly (no Wasm stack machine emulation)
