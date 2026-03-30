# CIR Binary Format Specification v1.1

**Date:** 2026-03-30
**Status:** Implementation-Verified — Matches working code in `cir_write.zig`, `cir.rs`, `translate.rs`
**Encoding Reference:** SPIR-V (word-aligned SSA)

---

## 1. Design Principles

1. **SPIR-V-style encoding** — 32-bit word-aligned instructions, fast to parse
2. **SSA-native** — Result IDs map directly to SSA value IDs
3. **Pre-registered types** — primitive types are implicit (no type section needed for basic programs)
4. **String heap** — deduplicated, offset-referenced (Java/.NET style)
5. **Extensible** — unknown opcodes can be skipped via word count
6. **Loosely typed** — the Zig CLIF IR allows type mismatches that Cranelift doesn't; the translator auto-widens via `coerce_types`

---

## 2. Binary Layout

```
[CIR Header]           5 words (20 bytes)
[String Heap]           Section 0x01
[Function Definitions]  Section 0x06
```

Only sections 0x01 and 0x06 are currently emitted. Sections 0x02–0x05 and 0x07–0x08 are reserved for future use (type declarations, globals, debug info, decorations).

### 2.1 Header (5 words, 20 bytes)

```
Word 0: Magic             0x00434952  ("CIR\0" little-endian)
Word 1: Version           0x00010000  (major=1, minor=0)
Word 2: Generator         0           (0 = cot compiler)
Word 3: Bound             u32         Upper bound on all Result IDs
Word 4: Reserved          0
```

### 2.2 Instruction Encoding

Every instruction is one or more 32-bit little-endian words:

```
Word 0: (word_count << 16) | opcode
  Bits 31–16: Total word count (including word 0)
  Bits 15–0:  Opcode

Words 1..N: Operands
```

A consumer extracts `word_count = word0 >> 16` and `opcode = word0 & 0xFFFF`, then reads `word_count - 1` operand words.

### 2.3 Section Headers

```
(section_word_count << 16) | section_id
```

Where `section_word_count` includes the header word itself. A word count of 0 means "read until end of data" (used for FUNC_DEFS).

---

## 3. String Heap (Section 0x01)

Deduplicated string table. All names referenced by byte offset.

```
Word 0:    (word_count << 16) | 0x01
Word 1:    heap_byte_count (u32)
Words 2+:  Heap data (padded to 4-byte alignment)
```

Each string at offset X:
```
X+0:     length (u32 little-endian)
X+4:     UTF-8 bytes
X+4+len: Zero padding to next 4-byte boundary
```

Example:
```
Offset  0: [5] "alloc" [3 pad]    → reference: 0
Offset 12: [7] "release" [1 pad]  → reference: 12
```

---

## 4. Pre-registered Type Indices

These are implicit — no type section needed.

```
 0  INVALID         12  VOID
 1  BOOL            13  UNTYPED_INT
 2  I8              14  UNTYPED_FLOAT
 3  I16             15  UNTYPED_BOOL
 4  I32             16  UNTYPED_NULL
 5  I64             17  STRING
 6  U8              18  SSA_MEM
 7  U16             19  SSA_FLAGS
 8  U32             20  SSA_TUPLE
 9  U64             21  SSA_RESULTS
10  F32             22  NORETURN
11  F64
```

**Cranelift mapping:** BOOL/I8/U8 → i8, I16/U16 → i16, I32/U32 → i32, I64/U64 → i64, F32 → f32, F64 → f64. VOID/NORETURN → invalid (no value). Everything else → i64 (pointers, strings, slices).

---

## 5. Structure Markers

### FUNC_BEGIN (0xFF00)

```
Word 0: (7 << 16) | 0xFF00
Word 1: name_offset     — string heap offset
Word 2: type_idx        — return type index
Word 3: param_count     — number of function parameters
Word 4: block_count     — number of blocks (informational)
Word 5: flags           — bit 1 = export
```

### FUNC_END (0xFF01)

```
Word 0: (1 << 16) | 0xFF01
```

### BLOCK_BEGIN (0xFF02)

```
Word 0: (N << 16) | 0xFF02     where N = 5 + succ_count + pred_count
Word 1: block_id (u32)         — sequential index (0 = entry)
Word 2: kind (u8 in u32)       — 0x05 = entry, 0x00 = plain
Word 3: successor_count
Words:  [successor block IDs...]
Word:   predecessor_count
Words:  [predecessor block IDs...]
```

### BLOCK_END (0xFF03)

```
Word 0: (1 << 16) | 0xFF03
```

---

## 6. Opcode Map

```
Range           Category              Implemented
──────────────────────────────────────────────────
0x0000–0x000F   Constants             CONST_BOOL, CONST_INT, CONST_FLOAT
0x0010–0x001F   Integer Arithmetic    ADD, SUB, MUL, DIV, UDIV, MOD, UMOD
0x0020–0x002F   Bitwise               AND, OR, XOR, SHL, SHR, SAR, NOT
0x0030–0x003F   Comparison            EQ, NE, LT, LE, GT, GE, ULT, ULE, UGT, UGE
0x0040–0x004F   Type Conversion       UEXTEND, SEXTEND, IREDUCE
0x0050–0x005F   Float Arithmetic      ADD_F, SUB_F, MUL_F, DIV_F, NEG_F
0x0060–0x006F   Float Comparison      EQ_F, NE_F, LT_F, LE_F, GT_F, GE_F
0x0070–0x008F   Memory                LOAD, STORE, LOCAL_ADDR
0x0090–0x009F   Control Flow (SSA)    COPY, ARG, STATIC_CALL, RET, RET_VOID, COND_SELECT
0x00A0–0x00AF   Control Flow (CFG)    JUMP, BRIF, TRAP, COND_TRAP
0x00B0–0x00BF   Declarations          STACK_SLOT_DECL
0x00C0–0x00CF   ARC                   RETAIN, RELEASE (reserved, not yet translated)
0xFF00–0xFFFF   Structure Markers     FUNC_BEGIN, FUNC_END, BLOCK_BEGIN, BLOCK_END
```

---

## 7. Instruction Reference

### 7.1 Constants

| Hex | Name | Operands | Words |
|-----|------|----------|-------|
| 0x0000 | CONST_BOOL | (result, type, value: 0 or 1) | 4 |
| 0x0001 | CONST_INT | (result, type, imm_lo [, imm_hi]) | 4–5 |
| 0x0002 | CONST_FLOAT | (result, type, bits_lo, bits_hi) | 5 |

`CONST_INT`: If `imm_hi` is nonzero, 5 words. Otherwise 4 words. The type comes from the CLIF result type (e.g., i8 for boolean comparisons, i32 for narrow integers, i64 for default).

### 7.2 Integer Arithmetic

All: `(result, type, lhs, rhs)` — 5 words. Type is the operand type.

| Hex | Name | Cranelift |
|-----|------|-----------|
| 0x0010 | ADD | iadd |
| 0x0011 | SUB | isub |
| 0x0012 | MUL | imul |
| 0x0013 | DIV | sdiv |
| 0x0014 | UDIV | udiv |
| 0x0015 | MOD | srem |
| 0x0016 | UMOD | urem |

**Translator note:** `coerce_types` auto-widens if lhs/rhs have different widths.

### 7.3 Bitwise

Binary: `(result, type, lhs, rhs)` — 5 words. Unary: `(result, type, operand)` — 4 words.

| Hex | Name | Cranelift |
|-----|------|-----------|
| 0x0020 | AND | band |
| 0x0021 | OR | bor |
| 0x0022 | XOR | bxor |
| 0x0023 | SHL | ishl |
| 0x0024 | SHR | ushr |
| 0x0025 | SAR | sshr |
| 0x0026 | NOT | bnot |

### 7.4 Integer Comparison

All: `(result, operand_type, lhs, rhs)` — 5 words. Note: `operand_type` is the type of the operands, NOT the result (which is always i8).

| Hex | Name | Cranelift Condition |
|-----|------|---------------------|
| 0x0030 | EQ | Equal |
| 0x0031 | NE | NotEqual |
| 0x0032 | LT | SignedLessThan |
| 0x0033 | LE | SignedLessThanOrEqual |
| 0x0034 | GT | SignedGreaterThan |
| 0x0035 | GE | SignedGreaterThanOrEqual |
| 0x0036 | ULT | UnsignedLessThan |
| 0x0037 | ULE | UnsignedLessThanOrEqual |
| 0x0038 | UGT | UnsignedGreaterThan |
| 0x0039 | UGE | UnsignedGreaterThanOrEqual |

**Translator note:** `coerce_types` auto-widens if lhs/rhs have different widths.

### 7.5 Type Conversion

All: `(result, target_type, value)` — 4 words.

| Hex | Name | Cranelift |
|-----|------|-----------|
| 0x0040 | UEXTEND | uextend |
| 0x0041 | SEXTEND | sextend |
| 0x0042 | IREDUCE | ireduce |

### 7.6 Float Arithmetic

Binary: `(result, type, lhs, rhs)` — 5 words. Unary: `(result, type, operand)` — 4 words.

| Hex | Name | Cranelift |
|-----|------|-----------|
| 0x0050 | ADD_F | fadd |
| 0x0051 | SUB_F | fsub |
| 0x0052 | MUL_F | fmul |
| 0x0053 | DIV_F | fdiv |
| 0x0054 | NEG_F | fneg |

### 7.7 Float Comparison

All: `(result, operand_type, lhs, rhs)` — 5 words.

| Hex | Name | Cranelift Condition |
|-----|------|---------------------|
| 0x0060 | EQ_F | Equal |
| 0x0061 | NE_F | NotEqual |
| 0x0062 | LT_F | LessThan |
| 0x0063 | LE_F | LessThanOrEqual |
| 0x0064 | GT_F | GreaterThan |
| 0x0065 | GE_F | GreaterThanOrEqual |

### 7.8 Memory

| Hex | Name | Operands | Words | Cranelift |
|-----|------|----------|-------|-----------|
| 0x0070 | LOAD | (result, type, addr) | 4 | load |
| 0x0071 | STORE | (type, addr, value) | 4 | store |
| 0x0080 | LOCAL_ADDR | (result, type, slot_index) | 4 | stack_addr |

**LOAD/STORE offset handling:** The Zig CLIF IR has offsets on load/store. The serializer decomposes `load offset(addr)` into `const off; add addr, off; load result` using synthetic IDs. The Rust side sees only zero-offset loads/stores.

**STORE operand order:** `(type, addr, value)` — the type is the VALUE type, not the address type. Address is always i64.

### 7.9 Control Flow — SSA

| Hex | Name | Operands | Words |
|-----|------|----------|-------|
| 0x0091 | COPY | (result, type, source) | 4 |
| 0x0092 | ARG | (result, type, param_index) | 4 |
| 0x0093 | STATIC_CALL | (result, return_type, name_offset, args...) | 4+ |
| 0x0097 | RET | (value) | 2 |
| 0x0098 | RET_VOID | () | 1 |
| 0x0099 | COND_SELECT | (result, type, cond, true_val, false_val) | 6 |

**ARG:** Emitted in both entry blocks (function parameters) and non-entry blocks (phi values). The param_index maps to Cranelift block params.

**STATIC_CALL:** Function name is a string heap offset. The translator declares callees with `Linkage::Import` and builds signatures from argument count (all i64) + return type. `coerce_types` is NOT applied to call args — they're assumed to be i64.

**COPY:** On the Rust side this is a pure value alias (no Cranelift instruction emitted).

### 7.10 Control Flow — CFG

| Hex | Name | Operands | Words |
|-----|------|----------|-------|
| 0x00A0 | JUMP | (target_block, arg_count, args...) | 3+ |
| 0x00A1 | BRIF | (cond, then_block, else_block, then_arg_count, then_args..., else_arg_count, else_args...) | 6+ |
| 0x00A2 | TRAP | () | 1 |
| 0x00A3 | COND_TRAP | (condition) | 2 |

**JUMP:** Unconditional branch. `args` are phi values passed to the target block's parameters.

**BRIF:** Conditional branch. Both branches carry independent phi argument lists. Format:
```
Word 0: opcode header
Word 1: condition value ID
Word 2: then-block ID
Word 3: else-block ID
Word 4: then-arg-count
Words:  [then-arg value IDs...]
Word:   else-arg-count
Words:  [else-arg value IDs...]
```

**TRAP:** Unconditional trap. Maps to `trap(TrapCode::unwrap_user(1))`.

**COND_TRAP:** Trap if condition is nonzero. Maps to `trapnz(cond, TrapCode::unwrap_user(1))`.

### 7.11 Declarations

| Hex | Name | Operands | Words |
|-----|------|----------|-------|
| 0x00B0 | STACK_SLOT_DECL | (slot_index, size, alignment) | 4 |

**Placement:** Must appear in the entry block (block 0), before any instructions that reference the slot. The Rust translator pre-scans all blocks for these and creates Cranelift stack slots before processing other instructions.

---

## 8. Synthetic Value IDs

The serializer creates intermediate values not present in the original CLIF IR (e.g., decomposing `load offset(addr)` into `const + add + load`). These use IDs starting at `0xC0000000`, incremented per use, reset per function.

The translator treats them identically to normal value IDs — they're just u32 keys in the value_map.

---

## 9. Type Coercion

The Zig CLIF IR is loosely typed — operands to comparisons and arithmetic may have different widths (e.g., i32 vs i64). Cranelift requires strict type matching. The Rust translator applies `coerce_types(lhs, rhs)` which auto-extends the narrower operand via `uextend`:

```
if lhs_type.bits() != rhs_type.bits():
    widen the narrower to match the wider via uextend
```

Applied to: all integer comparisons (EQ–UGE), integer arithmetic (ADD–UMOD), COND_SELECT operands.

---

## 10. Function Name Mangling

| Source Name | CIR Name | macOS Symbol | Linux Symbol |
|-------------|----------|-------------|-------------|
| `main` | `_cot_main` | `__cot_main` | `_cot_main` |
| `foo` | `foo` | `_foo` | `foo` |

Cranelift automatically adds `_` prefix on macOS (MachO convention). The `main` → `_cot_main` rename happens in the translator. The runtime .o provides the actual `_main` entry point wrapper that calls `__cot_main`.

---

## 11. Runtime Architecture

The CIR path produces TWO object files:

1. **CIR .o** (from Cranelift) — user functions, all exported
2. **Runtime .o** (from Zig native backend) — runtime functions (ARC, I/O, print, signal, test), entry point wrapper (`_main`), data sections (string literals, globals, argc/argv/envp), source map placeholders

The system linker combines both. User functions in the CIR .o call runtime functions in the runtime .o via Import linkage. The entry wrapper calls `__cot_main` (in CIR .o) via linker resolution.

---

## 12. Not Yet Implemented

These are defined in the opcode space but not yet serialized or translated:

- **ARC opcodes** (0x00C0–0x00C6): RETAIN, RELEASE, ALLOC, DEALLOC — currently handled by runtime functions, not CIR instructions
- **VWT opcodes** (0x00D0–0x00D7): VWT_COPY, VWT_DESTROY, etc. — same, runtime functions
- **Concurrency opcodes** (0x00E0–0x00EA): ACTOR_ENQUEUE, TASK_CREATE, etc.
- **Atomics** (0x00F0–0x00F4): ATOMIC_LOAD, ATOMIC_STORE, etc.
- **Type Declarations** (Section 0x02, opcodes 0xF000–0xF010): not needed for current compilation
- **Global variables** (Section 0x04): globals are in the runtime .o, not CIR
- **Debug info** (Section 0x07): source positions not yet propagated through CIR
- **call_indirect** (0x0094): indirect/closure calls
- **branch_table**: multi-way switch dispatch
- **Sized load/store variants** (LOAD8, STORE16, etc.): offsets are decomposed in the serializer instead

---

## 13. Test Status

17/22 test case files pass through the CIR path (113+ individual tests). Remaining failures:

- `arc.cot`, `methods.cot`, `structs.cot`, `strings.cot` — runtime crashes from missing global_value resolution (globals point to data sections in the runtime .o that the CIR .o can't reference yet — Problem 4 from handoff)
- `union.cot` — `STORE val: unknown value v19` in `cu_extract_kind` function (likely a call returning void but CIR assigns a result_id that later instructions try to use)
