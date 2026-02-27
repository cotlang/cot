# WebAssembly 3.0 Reference for Cot Compiler

**Released:** September 17, 2025 (live standard, evergreen updates)
**Full spec:** `claude/specs/wasm-3.0-full.txt` (25,919 lines)
**Binary format detail:** `claude/specs/wasm-3.0-binary-format.txt`
**Browser support:** All 10 features shipped in Chrome/Firefox/Safari as of 2025

This document extracts the parts of Wasm 3.0 that matter for the Cot compiler. Read this instead of the full spec.

---

## Cot's Current Wasm 3.0 Adoption

**None.** Cot currently emits Wasm 1.0 with selective Wasm 2.0 features (sign extension, reference types). All 3.0 opcodes and sections are absent from the compiler. The SSA has a `tail_call` op but it is explicitly stubbed out: `lower_wasm.zig:347` says `"Wasm has no tail call (yet)"`.

---

## What's New in Wasm 3.0 vs 2.0

10 proposals merged:

| # | Feature | Cot Priority | Why |
|---|---------|-------------|-----|
| 1 | Tail calls | **HIGH** | `return_call` for recursive patterns, prevents stack overflow |
| 2 | Exception handling | **HIGH** | `try_table`/`throw` for defer cleanup, error propagation |
| 3 | Typed function references | **HIGH** | `call_ref` for faster closures, no table indirection |
| 4 | Multiple memories | LOW | Separate heap/stack/metadata memories |
| 5 | 64-bit address space | LOW | >4GB linear memory for server workloads |
| 6 | GC types | LOW (interop) | Cot uses ARC, not GC — but needed for WasmGC language interop |
| 7 | Extended constant expressions | LOW | Arithmetic in global initializers |
| 8 | Relaxed SIMD | LOW | Not for target audience |
| 9 | Profiles | LOW | Deterministic mode |
| 10 | Custom annotations | NONE | Text format only |

---

## New Opcodes — Compiler Implementation Reference

### Tail Calls (Priority: HIGH)

Reuse the caller's stack frame instead of pushing a new one. Prevents stack overflow in recursive algorithms.

| Instruction | Opcode | Semantics |
|-------------|--------|-----------|
| `return_call x` | `0x12` | Tail call function x |
| `return_call_indirect x y` | `0x13` | Tail call through table y with type x |
| `return_call_ref x` | `0x15` | Tail call through typed function reference |

**Binary encoding:**
```
return_call:          0x12 funcidx:u32
return_call_indirect: 0x13 tableidx:u32 typeidx:u32
return_call_ref:      0x15 typeidx:u32
```

**Current state in Cot:**
- `op.zig:74`: SSA `tail_call` op exists with `call` + `has_side_effects` flags
- `lower_wasm.zig:347`: `tail_call => null, // Wasm has no tail call (yet)` — **stubbed out**
- `aarch64/inst/mod.zig:1702`: ARM64 `RetCall` instruction defined
- `x64/inst/mod.zig:669-675`: `return_call_known`, `return_call_unknown` defined but TODO

**Implementation plan:**
1. `wasm_opcodes.zig`: Add `return_call = 0x12`
2. `lower_wasm.zig`: Detect tail position, emit `wasm_return_call` instead of `wasm_call` + `ret`
3. `gen.zig`: Map `wasm_return_call` → opcode `0x12`
4. `wasm_parser.zig`: Parse `0x12`
5. `decoder.zig`: Decode `0x12` → CLIF tail call
6. Native: ARM64 `b` (branch, no link) / x64 `jmp` instead of `bl`/`call`

**Tail position detection (in `lower.zig` or `lower_wasm.zig`):**
- Last statement in function body is `return f(args)` → tail call
- Last statement in `if` branch is `return f(args)` → tail call
- NOT a tail call if: any `defer` is active, or there are ARC releases pending

---

### Exception Handling (Priority: HIGH)

Structured exception handling with declared tags, payloads, and handler dispatch. Replaces manual error propagation.

| Instruction | Opcode | Semantics |
|-------------|--------|-----------|
| `throw x` | `0x08` | Throw exception with tag x |
| `throw_ref` | `0x0A` | Re-throw exception reference |
| `try_table bt catch* instr* end` | `0x1F` | Exception handler block |

**Catch clause encoding (inside try_table):**
```
catch x l:       0x00 tagidx:u32 labelidx:u32  — catch tag, branch to l
catch_ref x l:   0x01 tagidx:u32 labelidx:u32  — catch tag, push exnref, branch
catch_all l:     0x02 labelidx:u32              — catch any exception
catch_all_ref l: 0x03 labelidx:u32              — catch any, push exnref
```

**Tag section (NEW section ID 13):**
```
tagsec ::= section_13(tag*)
tag    ::= 0x00 typeidx:u32
```

**New heap types:**
```
exn   = 0x69 (-23 as s7)   -- exception reference
noexn = 0x74 (-12 as s7)   -- bottom of exception hierarchy
```

**Import/export extension:**
```
tag import: 0x04 typeidx:u32
tag export: 0x04 tagidx:u32
```

**How Cot could use this:**

Cot currently handles errors via error unions (`FsError!i64`) which compile to manual checks. This works but has limitations:
- `defer` cleanup across call boundaries requires manually unwinding
- Panic/trap cannot be caught — the program just dies
- Error propagation (`try`) generates branching code at every call site

With exception handling:
- `defer` cleanup: wrap function body in `try_table`, catch clause runs deferred cleanup
- Error propagation: `throw` the error, `try_table` at the caller catches it
- Panic recovery: `catch_all` enables process-level panic handlers

**Implementation in Cot:**
1. `link.zig`: Add tag section (ID 13) after global section
2. `wasm_opcodes.zig`: Add opcodes `0x08`, `0x0A`, `0x1F`
3. `gen.zig`: Emit `try_table` around calls with defer/errdefer active
4. `wasm_parser.zig`: Parse section 13, parse new opcodes
5. `decoder.zig`/`translator.zig`: Translate to CLIF exception handling

**Note:** This is the highest-complexity 3.0 feature. Consider implementing after tail calls and typed refs.

---

### Typed Function References (Priority: HIGH)

Eliminates table indirection for closures and function pointers. `call_ref` is faster than `call_indirect` because it needs no table lookup or runtime type check.

| Instruction | Opcode | Semantics |
|-------------|--------|-----------|
| `call_ref x` | `0x14` | Call function through typed reference |
| `ref.as_non_null` | `0xD4` | Assert non-null reference |
| `br_on_null l` | `0xD5` | Branch if null |
| `br_on_non_null l` | `0xD6` | Branch if non-null |
| `ref.eq` | `0xD3` | Compare two references |

**Reference type encoding:**
```
(ref null ht) = 0x63 heaptype    -- nullable reference
(ref ht)      = 0x64 heaptype    -- non-nullable reference
```

**Extended heap type encoding:**
```
func     = 0x70 (-16)     any      = 0x6E (-18)     struct   = 0x6B (-21)
extern   = 0x6F (-17)     eq       = 0x6D (-19)     array    = 0x6A (-22)
                           i31      = 0x6C (-20)     exn      = 0x69 (-23)
none     = 0x71 (-15)     nofunc   = 0x73 (-13)     noexn    = 0x74 (-12)
noextern = 0x72 (-14)     typeidx  = u32 (positive = concrete type)
```

**How Cot currently handles closures:**
1. Closure body stored in function table
2. `call_indirect` with table index + runtime type check
3. Captured environment passed as i64 pointer

**How typed refs improve this:**
1. Closure becomes `(ref $closure_fn_type)` — a typed, non-nullable function reference
2. `call_ref $type` — direct call, no table lookup, no runtime check
3. Null closures: `(ref null $closure_fn_type)` + `br_on_null` for safety

**Implementation in Cot:**
1. `wasm_opcodes.zig`: Add `call_ref = 0x14`, `ref_as_non_null = 0xD4`, etc.
2. Type section: Define concrete function reference types for each closure signature
3. `gen.zig`: Emit `ref.func` to create references, `call_ref` instead of `call_indirect`
4. `wasm_parser.zig`: Parse `0x14`, `0xD3`–`0xD6`
5. `decoder.zig`/`translator.zig`: Translate `call_ref` to CLIF indirect call

**Benefit:** Both safer (typed) and faster (no table indirection). The table can be eliminated for programs that only use closures, not raw function pointers.

---

### Multiple Memories (Priority: LOW)

| Instruction change | Encoding |
|-------------------|----------|
| All load/store ops | Now take `memidx:u32` before `memarg` |
| `memory.size x` | `0x3F memidx:u32` |
| `memory.grow x` | `0x40 memidx:u32` |
| `memory.fill x` | `0xFC 0x0B memidx:u32` |
| `memory.copy x y` | `0xFC 0x0A memidx:u32 memidx:u32` |
| `memory.init x y` | `0xFC 0x08 dataidx:u32 memidx:u32` |
| `data.drop x` | `0xFC 0x09 dataidx:u32` |

Backward compatible: single-memory modules need no changes (index implicitly 0).

**Potential use in Cot:** Separate heap (user allocations) from metadata (ARC counters, vtables). Could improve cache locality and enable memory sandboxing. Not needed yet.

---

### 64-bit Address Space (Priority: LOW)

```
memtype ::= addrtype limits
addrtype ::= 0x00 (i32) | 0x04 (i64)
```

When i64 address type: all memory addresses become i64, `memory.size` returns i64, `memory.grow` takes i64.

**Note:** Safari does not yet ship Memory64. Limits browser Wasm deployment.

**Potential use in Cot:** Server workloads >4GB. Cot already uses i64 internally for all pointers (Wasm memory addresses are narrowed to i32 at load/store boundaries). Adopting memory64 would remove these narrowing casts.

---

### GC Types (Priority: LOW — interop only)

All under `0xFB` prefix. Managed structs and arrays whose lifetime is handled by the runtime GC.

| Instruction | Opcode | Purpose |
|-------------|--------|---------|
| `struct.new x` | `0xFB 0x00` | Create GC struct |
| `struct.get x y` | `0xFB 0x02` | Get field |
| `struct.set x y` | `0xFB 0x05` | Set field |
| `array.new x` | `0xFB 0x06` | Create GC array |
| `array.get x` | `0xFB 0x0B` | Get element |
| `array.set x` | `0xFB 0x0E` | Set element |
| `array.len` | `0xFB 0x0F` | Get length |
| `ref.cast (ref t)` | `0xFB 0x16` | Cast reference |
| `br_on_cast` | `0xFB 0x18` | Branch on cast success |
| `ref.i31` | `0xFB 0x1C` | Box i32 as i31ref |
| `i31.get_s/u` | `0xFB 0x1D/1E` | Unbox i31ref |

**Type definitions (new composite types):**
```
struct fieldtype*     = 0x5F (-33)
array fieldtype       = 0x5E (-34)
sub typeidx* comptype = 0x50 (-48)    -- non-final subtype
sub final ...         = 0x4F (-49)    -- final subtype
rec subtype*          = 0x4E (-50)    -- recursive type group
```

**Packed field types:** `i8` (0x78), `i16` (0x77)

**Cot uses WasmGC for all Wasm struct objects.** `--target=wasm32` produces WasmGC output where structs are GC-managed (`struct.new`, `struct.get`, `struct.set`). ARC is only used on native targets. WasmGC interop with Kotlin/Wasm, Dart, Java, or Scheme modules becomes relevant when Cot adds package/module interop.

---

### Extended Constant Expressions (Priority: LOW)

Now valid in global initializers and segment offsets:
```
i32.add (0x6A), i32.sub (0x6B), i32.mul (0x6C)
i64.add (0x7C), i64.sub (0x7D), i64.mul (0x7E)
global.get (any immutable global, not just imports)
```

Useful for computed global offsets without runtime initialization code.

---

## Module Binary Format — Complete Section Layout

```
Module layout (section IDs):
  0  - Custom section (can appear anywhere)
  1  - Type section
  2  - Import section
  3  - Function section
  4  - Table section
  5  - Memory section
  6  - Global section
  7  - Export section
  8  - Start section
  9  - Element section
  10 - Code section
  11 - Data section
  12 - Data count section (Wasm 2.0+)
  13 - Tag section (Wasm 3.0)
```

**Tag section placement:** After global section (6), before export section (7).

**Import/export kinds:**
```
0x00 - func
0x01 - table
0x02 - mem
0x03 - global
0x04 - tag (3.0)
```

---

## Post-3.0 Proposals Relevant to Cot

These are NOT in Wasm 3.0 but are actively being standardized. Cot should track them.

### Threads / Shared Memory Atomics (Phase 4 — shipping in all browsers)

**Opcode prefix:** `0xFE`

Shared memory flag, atomic load/store/RMW/CAS, wait/notify. Essential for:
- Multi-threaded server workloads
- Atomic reference counting (ARC could use `i64.atomic.rmw.add` instead of non-atomic increment)
- Shared data between Web Workers

**Note:** Phase 4 and shipping in all browsers since 2020, but technically not in Wasm 2.0 or 3.0 spec text. Treated as a de facto standard.

### Stack Switching / Continuations (Phase 3)

`cont.new`, `cont.bind`, `resume`, `suspend`, `barrier`. Enables:
- **async/await** without JavaScript Promise interop or Asyncify transformation
- Coroutines, generators, green threads at the Wasm level
- This is the feature Cot needs for `async fn` on the Wasm target

**Status:** Phase 3. Chrome has JSPI (JS Promise Integration) as a stopgap. Wasmtime has experimental support.

### Component Model (Active development)

Higher-level module format with typed interfaces (WIT). Enables:
- Cross-language interop (Cot + Rust + Python modules composing)
- WASI 0.3+ is built on the component model
- Rich import/export types (strings, records, variants, lists)

### Branch Hinting (Phase 5 — standardized)

Custom section `metadata.code.branch_hint` marking likely/unlikely branches. Cot could emit these for error paths and bounds checks.

### Wide Arithmetic (Phase 3)

64x64→128 bit multiply, 128-bit add/sub. Useful for:
- Hash function intermediate values (splitmix64 could benefit)
- BigInt support
- Cryptographic operations

---

## Implementation Priority for Cot

### Phase 1: Wasm 2.0 gaps (low effort, high value)

| Feature | Files | Effort |
|---------|-------|--------|
| `memory.copy` / `memory.fill` in gen.zig | gen.zig | 1 hour |
| Data count section | link.zig | 30 min |
| `trunc_sat` for float→int casts | gen.zig, opcodes | 30 min |

### Phase 2: Wasm 3.0 tail calls (medium effort)

| Feature | Files | Effort |
|---------|-------|--------|
| `return_call` opcode + detection | opcodes, lower_wasm, gen, parser, decoder | 1-2 days |
| ARM64/x64 `b`/`jmp` for tail calls | aarch64/, x64/ | Already stubbed |

### Phase 3: Wasm 3.0 typed refs (medium effort)

| Feature | Files | Effort |
|---------|-------|--------|
| `call_ref` for closures | opcodes, gen, type section, parser, decoder | 2-3 days |
| Remove table for closure-only programs | link.zig, gen.zig | 1 day |

### Phase 4: Wasm 3.0 exceptions (high effort)

| Feature | Files | Effort |
|---------|-------|--------|
| Tag section, `try_table`, `throw` | link, gen, parser, decoder, translator | 1-2 weeks |
| defer-across-calls cleanup | lower.zig, gen.zig | Complex |

### Phase 5: Post-3.0 (when available)

| Feature | Dependency | Cot use case |
|---------|-----------|--------------|
| Threads/atomics | Already shipping | Concurrent server, atomic ARC |
| Stack switching | Phase 3, ~2026-2027 | `async fn` on Wasm target |
| Component model | WASI 0.3 | Cross-language module interop |

---

## Files to Modify

```
compiler/codegen/wasm_opcodes.zig         — new opcode constants
compiler/codegen/wasm/gen.zig             — emit new opcodes, memory.copy/fill
compiler/codegen/wasm/link.zig            — tag section, data count section, type section
compiler/codegen/wasm/assemble.zig        — new instruction encoding
compiler/codegen/native/wasm_parser.zig   — parse new opcodes/sections
compiler/codegen/native/wasm_to_clif/decoder.zig    — decode new instructions
compiler/codegen/native/wasm_to_clif/translator.zig — translate to CLIF
compiler/ssa/op.zig                       — SSA ops (tail_call already exists)
compiler/ssa/passes/lower_wasm.zig        — new lowering rules (tail_call stub → real)
compiler/frontend/lower.zig              — tail position detection
```
