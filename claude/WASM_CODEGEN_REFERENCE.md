# Cot Wasm Codegen — Complete Reference

**Date:** 2026-03-22
**Purpose:** Permanent reference for all Wasm codegen work. Read this before touching anything in `compiler/codegen/wasm/`, `compiler/ssa/passes/lower_wasm.zig`, or `compiler/codegen/wasi_runtime.zig`.

---

## Pipeline Overview

```
Cot source
  → Frontend (scan, parse, check)
  → SSA IR (value.zig, block.zig, func.zig)
  → lower_wasm.zig    — generic SSA ops → wasm_* ops
  → gen.zig           — SSA → Prog chain (pseudo-instructions)
  → preprocess.zig    — AJMP/ARET → dispatch loop + br_table
  → assemble.zig      — Prog chain → Wasm bytecode
  → link.zig          — Build Wasm module (type/import/export/code sections)
  → .wasm file
```

**Go reference:** Every file in `compiler/codegen/wasm/` is ported from Go's Wasm compiler:
- `gen.zig` ← `references/go/src/cmd/compile/internal/wasm/ssa.go`
- `preprocess.zig` ← `references/go/src/cmd/internal/obj/wasm/wasmobj.go` (preprocess)
- `assemble.zig` ← `references/go/src/cmd/internal/obj/wasm/wasmobj.go` (assemble)

---

## Cot's Type System (What the Wasm Backend Must Handle)

Cot has a rich type system — richer than Go's. Every type listed here is real, tested, and in production.

### Primitive Types

| Type | Size | Wasm Representation | Notes |
|------|------|---------------------|-------|
| `i8`, `i16`, `i32` | 1-4B | **i64** (sign/zero-extended) | Narrowed for ops via i32_wrap_i64, re-extended |
| `i64` (alias: `int`) | 8B | **i64** | Default integer type |
| `u8` (alias: `byte`), `u16`, `u32` | 1-4B | **i64** (zero-extended) | Unsigned variants |
| `u64` | 8B | **i64** | |
| `f32` | 4B | **f32** | Native Wasm f32 |
| `f64` (alias: `float`) | 8B | **f64** | Native Wasm f64 |
| `bool` | 1B | **i64** (0 or 1) | Comparisons produce i32, extended to i64 |
| `void` | 0B | No Wasm value | Function returns nothing |
| `noreturn` | 0B | `unreachable` | Bottom type (panic, exit) |

### Compound Types

| Type | Size | Wasm Representation | Notes |
|------|------|---------------------|-------|
| `string` | 16B | **Two i64** (ptr + len) | Decomposed at call boundaries |
| `[]T` (slice) | 24B | **Three i64** (ptr + len + cap) | Go-style slice ABI |
| `[N]T` (array) | N×size(T) | In linear memory | Stack or heap allocated |
| `?T` (optional) | 8+size(T) | **Two i64** (tag + payload) | Tag: 0=null, non-zero=present |
| `E!T` (error union) | 8+size(T) | **Two i64** (error_tag + payload) | Tag: 0=success, non-zero=error |
| `(T1, T2, ...)` (tuple) | 8×count | **N × i64** | Each element padded to 8 bytes |
| `*T` (pointer) | 8B | **i64** | Linear memory address |
| `fn(T)->R` (function) | 8B | **i64** | Function index or closure ptr |

### Aggregate Types

| Type | Size | Wasm Representation | Notes |
|------|------|---------------------|-------|
| `struct` | Sum of fields | In linear memory | Field offsets computed by checker |
| `enum` | size(backing) | **i64** (or backing type) | Default backing is i64 |
| `enum(u8)` | 1B | **i64** (extended) | Sized enum backing |
| `union` | 8+max(payload) | **i64** tag + payload in memory | Tagged union |
| `error` set | 0B (compile-time) | Integer tags in error unions | No runtime representation |
| `distinct T` | same as T | Same as underlying | Zero-cost nominal wrapper |

### Generics
Generics use **shape stenciling (3-tier)** — not pure monomorphization. See `claude/archive/SHAPE_STENCILING.md` for the full design.

### Memory Management
**Wasm does NOT use ARC.** Wasm uses WasmGC (browser garbage collection). ARC retain/release is native-only.

---

## The i64-for-Everything Design

### What It Means

All user values on the Wasm stack are **i64**, with these exceptions:
- **f32** values use native Wasm f32
- **f64** values use native Wasm f64
- **PC_B** (dispatch block ID) is i32
- **SP** (stack pointer global) is i32
- **Comparison results** are i32 (0/1), extended to i64 when used as values

### Why It Works This Way

This is a **deliberate architectural choice**, not a simplification:

1. **Uniform representation.** All Cot values (pointers, integers, bools, enum tags, struct field offsets) fit in 64 bits. Using i64 everywhere eliminates type-mismatch issues at Wasm boundaries.

2. **Matches Go's data value model.** Go also stores all non-float data values as i64 in Wasm (see `ssa.go` lines 535-594: `loadOp` uses `AI64Load` for all integer sizes, sign/zero-extending as needed). Cot does the same.

3. **32-bit ops still exist.** When the SSA has `add32`, `eq32`, etc., they lower to `wasm_i32_add`, `wasm_i32_eq`. The gen.zig pattern is: `getValue64` → `i32_wrap_i64` → i32 op → `i64_extend_i32_u`. This matches Go exactly.

4. **Float ops use native types.** `lower_wasm.zig` checks `isFloatType()` for loads/stores and emits `wasm_f64_load`/`wasm_f64_store` or `wasm_f32_load`/`wasm_f32_store` as appropriate. Floats are NOT stored as i64.

### How It Differs From Go

| Aspect | Go | Cot | Why |
|--------|----|----|-----|
| Function signature | ALL functions: `(i32) → i32` | Heterogeneous: `(i64, i64) → i64` etc. | Go needs uniform signature for goroutine suspension/resumption. Cot doesn't have goroutines. |
| Argument passing | Via Go stack in linear memory (SP+offset) | Via Wasm function parameters + locals | Cot uses Wasm's native parameter mechanism. |
| Return values | Via Go stack in linear memory | Via Wasm return values | Cot returns directly on Wasm stack. |
| Compound returns | Written to Go stack at caller-specified offsets | Multiple Wasm return values (e.g., two i64 for string) | Cot uses Wasm multi-value returns. |
| Host imports | ALL imports: single `(sp)` parameter, read args from memory | Typed parameters matching the extern fn declaration | Cot imports have proper typed signatures. |

### What This Means for the JS Bridge

Cot's extern fns compile to **real Wasm imports with typed i64 parameters**. This is different from Go's `wasm_exec.js` where all imports receive a single SP and read/write memory.

For Cot's JS bridge:
- `extern fn canvas_fill_rect(ctx: i64, x: f64, y: f64, w: f64, h: f64) void`
  - Compiles to Wasm import: `("env", "canvas_fill_rect") : (i64, i64, i64, i64, i64) → ()`
  - Note: f64 params are currently passed as i64 (reinterpreted). This may change.
  - JS receives BigInt values, must convert: `Number(ctx)`, `Number(x)`, etc.
- `extern fn js_log(ptr: i64, len: i64) void`
  - Compiles to Wasm import: `("env", "js_log") : (i64, i64) → ()`
  - JS reads string: `new TextDecoder().decode(new Uint8Array(memory.buffer, Number(ptr), Number(len)))`

---

## File-by-File Reference

### 1. `compiler/ssa/passes/lower_wasm.zig` — SSA Lowering

**Purpose:** Convert generic SSA ops to Wasm-specific ops.

#### SSA Op → Wasm Op Mappings (Complete)

**Integer Arithmetic (i64):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `add` / `add64` | `wasm_i64_add` | 64 |
| `sub` / `sub64` | `wasm_i64_sub` | 65 |
| `mul` / `mul64` | `wasm_i64_mul` | 66 |
| `div` | `wasm_i64_div_s` | 67 |
| `udiv` | `wasm_i64_div_u` | 68 |
| `mod` | `wasm_i64_rem_s` | 69 |
| `umod` | `wasm_i64_rem_u` | 70 |

**Integer Arithmetic (i32):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `add32` | `wasm_i32_add` | 73 |
| `sub32` | `wasm_i32_sub` | 74 |
| `mul32` | `wasm_i32_mul` | 75 |

**Bitwise (i64):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `and_` / `and64` | `wasm_i64_and` | 81 |
| `or_` / `or64` | `wasm_i64_or` | 82 |
| `xor` / `xor64` | `wasm_i64_xor` | 83 |
| `shl` / `shl64` | `wasm_i64_shl` | 84 |
| `shr` / `shr64` | `wasm_i64_shr_u` | 85 |
| `sar` / `sar64` | `wasm_i64_shr_s` | 86 |
| `bool_not` | `wasm_i64_eqz` | 88 |
| `clz` | `wasm_i64_clz` | 89 |
| `ctz` | `wasm_i64_ctz` | 90 |
| `popcnt` | `wasm_i64_popcnt` | 91 |

**Bitwise (i32):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `and32` | `wasm_i32_and` | 94 |
| `or32` | `wasm_i32_or` | 95 |
| `xor32` | `wasm_i32_xor` | 96 |
| `shl32` | `wasm_i32_shl` | 97 |
| `shr32` | `wasm_i32_shr_u` | 98 |
| `sar32` | `wasm_i32_shr_s` | 99 |
| `clz32` | `wasm_i32_clz` | 100 |
| `ctz32` | `wasm_i32_ctz` | 101 |
| `popcnt32` | `wasm_i32_popcnt` | 102 |

**Comparisons (i64):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `eq` / `eq64` | `wasm_i64_eq` | 109 |
| `ne` / `ne64` | `wasm_i64_ne` | 110 |
| `lt` / `lt64` | `wasm_i64_lt_s` | 111 |
| `le` / `le64` | `wasm_i64_le_s` | 112 |
| `gt` / `gt64` | `wasm_i64_gt_s` | 113 |
| `ge` / `ge64` | `wasm_i64_ge_s` | 114 |
| `ult` | `wasm_i64_lt_u` | 115 |
| `ule` | `wasm_i64_le_u` | 116 |
| `ugt` | `wasm_i64_gt_u` | 117 |
| `uge` | `wasm_i64_ge_u` | 118 |

**Comparisons (i32):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `eq32` | `wasm_i32_eq` | 120 |
| `ne32` | `wasm_i32_ne` | 121 |
| `lt32` | `wasm_i32_lt_s` | 122 |
| `le32` | `wasm_i32_le_s` | 123 |
| `gt32` | `wasm_i32_gt_s` | 124 |
| `ge32` | `wasm_i32_ge_s` | 125 |
| `ult32` | `wasm_i32_lt_u` | 126 |
| `ule32` | `wasm_i32_le_u` | 127 |
| `ugt32` | `wasm_i32_gt_u` | 128 |
| `uge32` | `wasm_i32_ge_u` | 129 |

**Float Arithmetic (f64):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `add64f` | `wasm_f64_add` | 131 |
| `sub64f` | `wasm_f64_sub` | 132 |
| `mul64f` | `wasm_f64_mul` | 133 |
| `div64f` | `wasm_f64_div` | 134 |
| `neg64f` | `wasm_f64_neg` | 135 |
| `sqrt64f` | `wasm_f64_sqrt` | 136 |

**Float Arithmetic (f32):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `add32f` | `wasm_f32_add` | 138 |
| `sub32f` | `wasm_f32_sub` | 139 |
| `mul32f` | `wasm_f32_mul` | 140 |
| `div32f` | `wasm_f32_div` | 141 |
| `neg32f` | `wasm_f32_neg` | 142 |
| `sqrt32f` | `wasm_f32_sqrt` | 143 |

**Float Comparisons (f64):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `eq64f` | `wasm_f64_eq` | 145 |
| `ne64f` | `wasm_f64_ne` | 146 |
| `lt64f` | `wasm_f64_lt` | 147 |
| `le64f` | `wasm_f64_le` | 148 |
| `gt64f` | `wasm_f64_gt` | 149 |
| `ge64f` | `wasm_f64_ge` | 150 |

**Float Comparisons (f32):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `eq32f` | `wasm_f32_eq` | 152 |
| `ne32f` | `wasm_f32_ne` | 153 |
| `lt32f` | `wasm_f32_lt` | 154 |
| `le32f` | `wasm_f32_le` | 155 |
| `gt32f` | `wasm_f32_gt` | 156 |
| `ge32f` | `wasm_f32_ge` | 157 |

**Type Conversions:**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `sign_ext32to64` | `wasm_i64_extend_i32_s` | 165 |
| `zero_ext32to64` | `wasm_i64_extend_i32_u` | 166 |
| `trunc64to32` | `wasm_i32_wrap_i64` | 167 |
| `cvt64to64f` | `wasm_f64_convert_i64_s` | 168 |
| `cvt64uto64f` | `wasm_f64_convert_i64_u` | 169 |
| `cvt64fto64` | `wasm_i64_trunc_f64_s` | 170 |
| `cvt32to32f` | `wasm_f32_convert_i64_s` | 171 |
| `cvt32fto32` | `wasm_i64_trunc_f32_s` | 172 |
| `cvt32fto64f` | `wasm_f64_promote_f32` | 173 |
| `cvt64fto32f` | `wasm_f32_demote_f64` | 174 |
| `reinterpret64fto64` | `wasm_i64_reinterpret_f64` | 175 |
| `reinterpret64to64f` | `wasm_f64_reinterpret_i64` | 176 |
| `reinterpret32fto32` | `wasm_i32_reinterpret_f32` | 177 |
| `reinterpret32to32f` | `wasm_f32_reinterpret_i32` | 178 |

**Memory (type-aware for floats):**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `load` / `load64` | `wasm_f64_load` if float, else `wasm_i64_load` | 180 |
| `load32` | `wasm_i64_load32_s` | 181 |
| `load32u` | `wasm_i64_load32_u` | 182 |
| `load16` | `wasm_i64_load16_s` | 183 |
| `load16u` | `wasm_i64_load16_u` | 184 |
| `load8` | `wasm_i64_load8_s` | 185 |
| `load8u` | `wasm_i64_load8_u` | 186 |
| `store` / `store64` | `wasm_f64_store` if float arg, else `wasm_i64_store` | 188 |
| `store32` | `wasm_i64_store32` | 189 |
| `store16` | `wasm_i64_store16` | 190 |
| `store8` | `wasm_i64_store8` | 191 |
| `load_f32` | `wasm_f32_load` | 193 |
| `store_f32` | `wasm_f32_store` | 194 |

**Function Calls:**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `static_call` | `wasm_lowered_static_call` | 196 |
| `call` | `wasm_call` | 197 |
| `closure_call` | `wasm_lowered_closure_call` | 198 |
| `inter_call` | `wasm_lowered_inter_call` | 199 |

**Data Movement & Safety:**
| SSA Op | Wasm Op | Line |
|--------|---------|------|
| `move` | `wasm_lowered_move` | 209 |
| `zero` | `wasm_lowered_zero` | 210 |
| `nil_check` | `wasm_lowered_nil_check` | 204 |

**Pass-through (not lowered — handled by gen.zig directly):**
- `phi`, `copy`, `fwd_ref`, `arg`
- `select0`, `select1`, `select_n`, `make_tuple`, `cond_select`
- `string_make`, `slice_make`, `opt_make`
- `addr`, `local_addr`, `global_addr`, `metadata_addr`, `off_ptr`, `add_ptr`, `sub_ptr`
- All `wasm_*` ops (already lowered)

---

### 2. `compiler/codegen/wasm/gen.zig` — Wasm Code Generation

**Purpose:** Transform SSA → Prog chain. Reference: Go `ssa.go`.

#### GenState (line 49)
Key fields:
- `value_to_local`: SSA value ID → Wasm local index
- `compound_len_locals`: SSA value ID → secondary local for compound types (string len, slice len, optional tag)
- `func_indices`: function name → Wasm function index
- `float_local_count`: number of f64 locals (declared at end of local range)

#### Compound Type Handling
Strings, slices, and optionals are represented as **two Wasm values**:
- Primary local: pointer (string), pointer (slice), tag (optional)
- Secondary local (in `compound_len_locals`): length (string), length (slice), payload (optional)

When a function returns a compound type, it pushes two i64 values onto the Wasm stack. The caller stores them in two separate locals.

#### Dispatch Loop
- `ssaGenBlock()` (line 184) generates control flow per SSA block
- Plain blocks: unconditional jump or fallthrough
- If blocks: condition → `i32_eqz` → `if` block → `br`
- Ret blocks: push return values → `aret` (pseudo-return)

---

### 3. `compiler/codegen/wasm/preprocess.zig` — Pseudo-Instruction Transformation

**Purpose:** Transform AJMP/ARET to dispatch loop. Reference: Go `wasmobj.go` preprocess().

#### Six Passes:

1. **Count resume points** (line 42): Assign PC values, build `tableIdxs` for br_table
2. **Emit prologue** (line 111): `SP -= framesize` if frame needed
3. **Transform instructions** (line 140):
   - AJMP → `i32.const <target.pc>` + `set PC_B` + `br`
   - ARET → SP restoration + `return`
   - `return_call` → SP restoration + Wasm 3.0 tail call
4. **Adjust variable offsets** (line 280): Add framesize to auto/param offsets
5. **Build dispatch loop** (line 309): Loop + blocks + br_table + end
6. **Compute branch depths** (line 365): Resolve relative depths for br/br_if

---

### 4. `compiler/codegen/wasm/assemble.zig` — Bytecode Encoding

**Purpose:** Convert Prog chain to Wasm bytes. Reference: Go `wasmobj.go` assemble().

#### Local Declaration Layout (line 103)
```
Params:     0 .. param_count-1        (not declared, part of signature)
PC_B:       param_count               (i32, if dispatch loop exists)
i64 locals: param_count+1 .. +N       (general purpose)
f64 locals: param_count+N+1 .. +N+M   (float values)
GC refs:    after all above            (each its own type group)
```

#### Instruction Encoding
Every Wasm opcode is encoded with appropriate LEB128 immediates:
- Constants: opcode + SLEB128 value
- Loads/stores: opcode + ULEB128 alignment + ULEB128 offset
- Branches: opcode + ULEB128 relative depth
- br_table: opcode + ULEB128 count + (count+1) × ULEB128 label indices
- Calls: opcode + ULEB128 function index
- call_indirect: opcode + ULEB128 type index + 0x00 (table index)
- WasmGC: 0xFB prefix + sub-opcode + ULEB128 type/field indices

---

### 5. `compiler/codegen/wasm/link.zig` — Module Linking

**Purpose:** Build complete Wasm module. Reference: Go `asm.go`.

#### Type Index Layout
```
GC struct types:  0 .. S-1
GC array types:   S .. S+A-1
Function types:   S+A .. S+A+F-1
```
When referencing function types in import/function sections, raw type index must be offset by `gc_offset = S + A`.

#### Function Index Layout
```
Imports:         0 .. I-1              (import_count = I)
Module funcs:    I .. I+M-1            (offset by import_count)
```
All module function indices must add `import_count` to get the final Wasm function index.

#### Wasm Binary Sections (in order)
1. **Type section** (0x01): GC struct types → GC array types → function types
2. **Import section** (0x02): module + name + kind(0x00=func) + type_idx
3. **Function section** (0x03): type_idx for each module function
4. **Table section** (0x04): funcref table if call_indirect used
5. **Memory section** (0x05): min/max pages
6. **Global section** (0x06): type + mutability + init expression
7. **Export section** (0x07): name + kind + index
8. **Element section** (0x09): table initialization
9. **Code section** (0x0A): function bodies
10. **Data section** (0x0B): static data segments

---

### 6. `compiler/codegen/wasm/constants.zig` — Wasm Types & Opcodes

#### ValType (line 16)
```zig
i32 = 0x7F, i64 = 0x7E, f32 = 0x7D, f64 = 0x7C, funcref = 0x70
```

#### WasmType (line 28)
Extended type with optional GC ref: `{ val: ValType, gc_ref: ?u32 }`

#### Virtual Registers (line 62)
Globals: SP(0, i32), CTXT(1, i64), g(2, i64), RET0-3(3-6, i64), PAUSE(7, i32)
i64 locals: R0-R15 (16-31)
f32 locals: F0-F15 (32-47)
f64 locals: F16-F31 (48-63)
Special: PC_B(64, i32)

---

### 7. `compiler/codegen/wasi_runtime.zig` — Runtime Functions

#### Two Modes
- **WASI target** (`--target=wasm`): Real WASI host imports (`wasi_snapshot_preview1`) + Cot adapter shims that translate i64 ABI ↔ WASI i32 ABI
- **Non-WASI target** (`--target=js`, native): Stub functions that return 0 or trap

#### Runtime Function Names (40+)
I/O: `fd_write`, `fd_read`, `fd_close`, `fd_seek`, `fd_open`
Process: `exit`, `args_count`, `arg_len`, `arg_ptr`, `environ_count`, `environ_len`, `environ_ptr`
Time/RNG: `time`, `random`
Networking: `net_socket`, `net_bind`, `net_listen`, `net_accept`, `net_connect`, `net_set_reuse_addr`
Event loop: `kqueue_create`, `kevent_add`, `kevent_del`, `kevent_wait`, `epoll_create`, `epoll_add`, `epoll_del`, `epoll_wait`, `set_nonblocking`
Process control: `fork`, `execve`, `dup2`, `pipe`, `setsid`, `waitpid`, `kill`, `ioctl_winsize`, `ioctl_set_ctty`, `openpty`, `isatty`, `poll_read`
Filesystem: `mkdir`, `dir_open`, `dir_next`, `dir_close`, `stat_type`, `unlink`

#### Scratch Memory Layout
```
0xE0000: iov struct (ptr + len, 8 bytes)
0xE0008: nwritten/nread (4 bytes)
0xE0010: generic result (8 bytes)
0xE1000: argv array (1024 × 4 bytes)
0xE2000: argv string buffer (8KB)
0xE4000: environ array
0xE5000: environ string buffer (8KB)
```

---

### 8. `compiler/codegen/js_glue.zig` — Browser JS Generation

**Purpose:** Generate `.js` companion file for `--target=js` Wasm modules.

#### Generated JS Template
- `init(wasmPath)`: Load + instantiate Wasm module
- `getExports()`: Return wrapped exports (sync + async)
- `wrapAsync(ctor, poll)`: Wrap async Cot functions as JS Promises
- Filters internal exports (`cot_*`, `__*` prefixes)
- Async detection: if export `foo` + `foo_poll` both exist → wrap as Promise

#### Current Limitation
The `importObject` is `{ env: {} }` — empty. User extern fns for `--target=js` are stubbed, not real imports. This is the gap that needs to be fixed for the Canvas2D bridge.

---

### 9. `compiler/driver.zig` — Pipeline Orchestration

#### `generateWasmCode()` (line 5689)
1. Create Linker, set memory (256 pages = 16MB)
2. Add globals: CTXT (i64, global 1), heap_ptr (i32, global 2 = 0x800000)
3. Register WasmGC types (if enabled)
4. Add WASI runtime (imports for WASI, stubs for others)
5. Add user extern fn imports (for `--target=js`)
6. Read `import_count` — all module function indices offset by this
7. Add runtime functions: mem, slice, print, test, bench, ARC
8. Build `func_indices` map: function name → Wasm function index
9. For each SSA function: generate code, add to linker, mark exports
10. Call `linker.emit()` to produce Wasm binary

---

## Critical Invariants

### 1. Import Count Offset
ALL module function indices must be offset by `import_count`. Imports occupy indices 0..I-1, module functions start at I. If you add a new import, every module function index shifts.

### 2. Type Index Offset
Function type indices in the type section are offset by `gc_offset = gc_struct_count + gc_array_count`. GC types occupy indices 0..gc_offset-1.

### 3. Dispatch Loop
Only generated if there are inter-block jumps or resume points. Functions with a single block and no calls may not have a dispatch loop.

### 4. Compound Return Convention
Functions returning string/slice/optional push TWO i64 values onto the Wasm stack. The caller stores them in two separate locals (primary + compound_len_local).

### 5. Stack Frame Layout
```
Before prologue:    SP → [caller's frame]
After prologue:     SP → [current frame (framesize bytes)] [caller's frame]
Auto vars:          SP + 0 .. SP + framesize - 1
Params:             SP + framesize + 8 .. (after return address)
```

### 6. Tail Calls
`return_call` (Wasm 3.0, opcode 0x12) is currently DISABLED in `lower_wasm.zig` line 385 due to native backend SIGSEGV. The frame epilogue (SP restoration) must happen BEFORE `return_call`, not after.
