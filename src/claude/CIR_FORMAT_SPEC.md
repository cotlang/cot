# CIR Binary Format Specification v1

**Date:** 2026-03-27
**Status:** Draft — Pre-Implementation
**Encoding Reference:** SPIR-V (word-aligned SSA), with Wasm custom section packaging
**Packaging:** Embedded in standard WebAssembly binary as custom section `cot_ssa`

---

## 1. Design Principles

1. **SPIR-V-style encoding** — 32-bit word-aligned instructions, fast to parse
2. **SSA-native** — Result IDs map directly to SSA value IDs
3. **Types are instructions** — type declarations produce Result IDs like any other instruction
4. **Logical ordering** — types before constants before functions (single-pass processing)
5. **String heap** — deduplicated, offset-referenced (Java/.NET style, not SPIR-V inline)
6. **Extensible** — unknown opcodes can be skipped via word count
7. **Validatable** — structural, ARC, type, and concurrency rules checkable offline

---

## 2. Packaging: Wasm Custom Section

CIR is embedded in a standard WebAssembly binary as a custom section. The `.wasm` file remains valid and runnable — runtimes ignore the custom section.

```
Standard .wasm file:
  0x00 'a' 's' 'm' 0x01 0x00 0x00 0x00    Wasm magic + version
  [Type section]                            Standard Wasm
  [Function section]                        Standard Wasm
  [Code section]                            Standard Wasm (runnable)
  [Data section]                            Standard Wasm
  [Custom section: "cot_ssa"]               CIR binary (this spec)
  [Custom section: "cot_types"]             Extended type metadata
  [Custom section: "name"]                  Standard debug names
```

The custom section envelope follows Wasm spec:
```
Section ID:    0x00 (custom)
Section size:  u32 LEB128 (total payload bytes)
Name length:   u32 LEB128
Name bytes:    "cot_ssa" (7 bytes)
Payload:       CIR binary data (this spec)
```

Build modes:
```bash
cot build main.cot -o main.wasm                  # Wasm Code + CIR (default)
cot build main.cot -o main.wasm --target=wasm     # Wasm Code only (strip CIR)
cot build main.cot -o main.wasm --target=native    # CIR only (strip Code)
```

---

## 3. CIR Binary Layout

### 3.1 Header (5 words, 20 bytes)

```
Word 0: Magic number          0x00434952  ("CIR\0" little-endian)
Word 1: Version               (major << 16) | minor  — v1.0 = 0x00010000
Word 2: Generator             Tool ID (0 = cot compiler, other values for third-party)
Word 3: Bound                 Upper bound on all Result IDs (enables array pre-allocation)
Word 4: Reserved              0x00000000
```

The Bound field is critical for performance: a consumer allocates `Value[Bound]` once and indexes directly. No hash maps, no resizing.

### 3.2 Instruction Encoding

Every instruction is one or more 32-bit words:

```
Word 0: (word_count << 16) | opcode
  Bits 31-16: Total word count for this instruction (including word 0)
  Bits 15-0:  Opcode (CIR opcode, NOT Wasm opcode)

Word 1..N: Operands (Result ID, Type ID, literals, string offsets)
```

Example encodings:

```
add v5 : i64 = v3, v4

  Word 0: (4 << 16) | 0x0010          4 words, opcode ADD
  Word 1: 5                            Result ID
  Word 2: 5                            Type (I64 = TypeIndex 5)
  Word 3: 3                            Arg 0 (value v3)
  Word 4: 4                            Arg 1 (value v4)

retain v10 = v9

  Word 0: (3 << 16) | 0x00A0          3 words, opcode RETAIN
  Word 1: 10                           Result ID
  Word 2: 9                            Arg 0 (value to retain)

const_int v2 : i64 = 42

  Word 0: (4 << 16) | 0x0060          4 words, opcode CONST_INT
  Word 1: 2                            Result ID
  Word 2: 5                            Type (I64)
  Word 3: 42                           Immediate value (low 32 bits)

const_int v3 : i64 = 0x100000000  (large immediate)

  Word 0: (5 << 16) | 0x0060          5 words, opcode CONST_INT
  Word 1: 3                            Result ID
  Word 2: 5                            Type (I64)
  Word 3: 0x00000000                   Immediate low 32 bits
  Word 4: 0x00000001                   Immediate high 32 bits

static_call v8 : void = "alloc", v5, v6, v7

  Word 0: (7 << 16) | 0x0082          7 words, opcode STATIC_CALL
  Word 1: 8                            Result ID
  Word 2: 12                           Type (VOID)
  Word 3: <string_offset>              Function name in string heap
  Word 4: 5                            Arg 0
  Word 5: 6                            Arg 1
  Word 6: 7                            Arg 2
```

### 3.3 Section Structure

CIR payload is divided into logical sections, each prefixed with a section ID and word count:

```
Section header:
  Word 0: (section_word_count << 16) | section_id

Section IDs:
  0x01  String Heap
  0x02  Type Declarations
  0x03  Type Metadata (VWT, Sendable, actor flags)
  0x04  Global Variables
  0x05  Function Declarations (signatures only)
  0x06  Function Definitions (blocks + instructions)
  0x07  Debug Info (source positions, names)
  0x08  Decorations (ARC annotations, isolation info)
```

Sections MUST appear in numerical order. A consumer can skip unknown sections by reading the word count.

Full layout:

```
[CIR Header]           5 words
[String Heap]           Section 0x01
[Type Declarations]     Section 0x02
[Type Metadata]         Section 0x03
[Global Variables]      Section 0x04
[Function Declarations] Section 0x05
[Function Definitions]  Section 0x06
[Debug Info]            Section 0x07 (optional)
[Decorations]           Section 0x08 (optional)
```

---

## 4. String Heap (Section 0x01)

Deduplicated string table. All names (function names, type names, field names, string literals) are stored here and referenced by byte offset.

```
Word 0: Section header
Word 1: Total byte count of heap data
[Packed UTF-8 strings, each preceded by u32 byte length]
  u32 length, bytes[length], padding to 4-byte alignment
  u32 length, bytes[length], padding to 4-byte alignment
  ...
```

String references in instructions are u32 byte offsets into this heap.

Example:
```
Offset 0:  [5] "alloc"  [pad 3]     →  reference: 0
Offset 12: [7] "release" [pad 1]    →  reference: 12
Offset 24: [11] "BankAccount" [pad 1] → reference: 24
```

---

## 5. Type Declarations (Section 0x02)

Types are instructions with Result IDs, following SPIR-V's model. Each type declaration produces a TypeIndex that subsequent instructions reference.

### 5.1 Pre-registered Types (implicit, no encoding needed)

These TypeIndex values are reserved and MUST NOT be redeclared:

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

User-defined types start at TypeIndex 23.

### 5.2 Type Opcodes

```
0xF000  CIR_TYPE_POINTER        (result_id, elem_type, managed: u32)
0xF001  CIR_TYPE_OPTIONAL       (result_id, elem_type)
0xF002  CIR_TYPE_ERROR_UNION    (result_id, elem_type, error_set_type)
0xF003  CIR_TYPE_ERROR_SET      (result_id, name_offset, variant_count, [name_offsets...])
0xF004  CIR_TYPE_SLICE          (result_id, elem_type)
0xF005  CIR_TYPE_ARRAY          (result_id, elem_type, length_lo, length_hi)
0xF006  CIR_TYPE_MAP            (result_id, key_type, value_type)
0xF007  CIR_TYPE_LIST           (result_id, elem_type)
0xF008  CIR_TYPE_TUPLE          (result_id, count, [elem_types...])
0xF009  CIR_TYPE_STRUCT         (result_id, name_offset, field_count, size, alignment,
                                 [field_name_offset, field_type, field_offset]...)
0xF00A  CIR_TYPE_ENUM           (result_id, name_offset, backing_type, variant_count,
                                 [variant_name_offset, variant_value]...)
0xF00B  CIR_TYPE_UNION          (result_id, name_offset, tag_type, variant_count,
                                 [variant_name_offset, payload_type]...)
0xF00C  CIR_TYPE_FUNC           (result_id, return_type, param_count, [param_type]...)
0xF00D  CIR_TYPE_DISTINCT       (result_id, name_offset, underlying_type)
0xF00E  CIR_TYPE_EXISTENTIAL    (result_id, trait_name_offset, method_count,
                                 [method_name_offsets...])
0xF00F  CIR_TYPE_TASK           (result_id, result_type)
0xF010  CIR_TYPE_ACTOR          (result_id, name_offset, field_count, size, alignment,
                                 [field_name_offset, field_type, field_offset]...)
```

---

## 6. CIR Opcodes (Section 0x06)

### 6.1 Opcode Map

CIR opcodes are organized by category in the u16 opcode space. Only generic/language-level ops are included — target-specific ops (arm64_*, amd64_*, wasm_*) are NOT in CIR. Those are generated by the backend from CIR instructions.

```
Category            Range       Count   Description
─────────────────────────────────────────────────────────
Constants           0x0000-0x000F  10   const_int, const_float, const_bool, etc.
Integer Arithmetic  0x0010-0x001F   8   add, sub, mul, div, mod, neg, etc.
Bitwise             0x0020-0x002F   8   and, or, xor, shl, shr, sar, not
Comparison          0x0030-0x003F  10   eq, ne, lt, le, gt, ge, ult, ule, ugt, uge
Type Conversion     0x0040-0x004F   4   convert, sign_ext, zero_ext, trunc
Float Arithmetic    0x0050-0x005F   8   add_f, sub_f, mul_f, div_f, neg_f, sqrt_f
Float Comparison    0x0060-0x006F   6   eq_f, ne_f, lt_f, le_f, gt_f, ge_f
Memory              0x0070-0x008F  16   load, store, local_addr, global_addr, off_ptr, move
Control Flow        0x0090-0x009F  10   phi, copy, arg, call, static_call, closure_call, ret
Tuples/Compounds    0x00A0-0x00AF  10   select0, select1, make_tuple, string_make, slice_make
Safety              0x00B0-0x00BF   5   nil_check, bounds_check, is_nil, is_non_nil
ARC                 0x00C0-0x00CF   6   retain, release, alloc, dealloc, is_unique
VWT / Generics      0x00D0-0x00DF   8   vwt_copy, vwt_destroy, vwt_size, metadata_addr
Concurrency         0x00E0-0x00EF  10   actor_enqueue, task_create, task_switch, await
Atomics             0x00F0-0x00FF   6   atomic_load, atomic_store, atomic_add, atomic_cas
Reserved            0x0100-0xEFFF  —    Future expansion
Type Declarations   0xF000-0xF0FF  17   CIR_TYPE_* (see Section 5.2)
Section Markers     0xFF00-0xFFFF  —    Section headers
```

### 6.2 Instruction Details

#### Constants (0x0000-0x000F)

```
0x0000  CONST_BOOL      (result, type, value: 0 or 1)                    3 words
0x0001  CONST_INT       (result, type, imm_lo [, imm_hi])                4-5 words
0x0002  CONST_FLOAT     (result, type, bits_lo, bits_hi)                 5 words
0x0003  CONST_NIL       (result, type)                                   3 words
0x0004  CONST_STRING    (result, type, string_offset, string_length)     5 words
0x0005  CONST_PTR       (result, type, value_lo, value_hi)               5 words
```

#### Integer Arithmetic (0x0010-0x001F)

```
0x0010  ADD             (result, type, lhs, rhs)                         5 words
0x0011  SUB             (result, type, lhs, rhs)                         5 words
0x0012  MUL             (result, type, lhs, rhs)                         5 words
0x0013  DIV             (result, type, lhs, rhs)                         5 words
0x0014  UDIV            (result, type, lhs, rhs)                         5 words
0x0015  MOD             (result, type, lhs, rhs)                         5 words
0x0016  UMOD            (result, type, lhs, rhs)                         5 words
0x0017  NEG             (result, type, operand)                          4 words
```

#### Bitwise (0x0020-0x002F)

```
0x0020  AND             (result, type, lhs, rhs)                         5 words
0x0021  OR              (result, type, lhs, rhs)                         5 words
0x0022  XOR             (result, type, lhs, rhs)                         5 words
0x0023  SHL             (result, type, lhs, rhs)                         5 words
0x0024  SHR             (result, type, lhs, rhs)                         5 words
0x0025  SAR             (result, type, lhs, rhs)                         5 words
0x0026  NOT             (result, type, operand)                          4 words
0x0027  BOOL_NOT        (result, type, operand)                          4 words
```

#### Comparison (0x0030-0x003F)

```
0x0030  EQ              (result, type, lhs, rhs)                         5 words
0x0031  NE              (result, type, lhs, rhs)                         5 words
0x0032  LT              (result, type, lhs, rhs)                         5 words
0x0033  LE              (result, type, lhs, rhs)                         5 words
0x0034  GT              (result, type, lhs, rhs)                         5 words
0x0035  GE              (result, type, lhs, rhs)                         5 words
0x0036  ULT             (result, type, lhs, rhs)                         5 words
0x0037  ULE             (result, type, lhs, rhs)                         5 words
0x0038  UGT             (result, type, lhs, rhs)                         5 words
0x0039  UGE             (result, type, lhs, rhs)                         5 words
```

#### Memory (0x0070-0x008F)

```
0x0070  LOAD            (result, type, addr)                             4 words
0x0071  STORE           (type, addr, value)                              4 words  [no result]
0x0072  LOAD8           (result, type, addr)                             4 words
0x0073  LOAD16          (result, type, addr)                             4 words
0x0074  LOAD32          (result, type, addr)                             4 words
0x0075  STORE8          (type, addr, value)                              4 words
0x0076  STORE16         (type, addr, value)                              4 words
0x0077  STORE32         (type, addr, value)                              4 words
0x0078  LOAD8S          (result, type, addr)                             4 words  [sign-extending]
0x0079  LOAD16S         (result, type, addr)                             4 words
0x007A  LOAD32S         (result, type, addr)                             4 words
0x0080  LOCAL_ADDR      (result, type, slot_index: u32)                  4 words
0x0081  GLOBAL_ADDR     (result, type, name_offset: u32)                 4 words
0x0082  METADATA_ADDR   (result, type, name_offset: u32)                 4 words
0x0083  OFF_PTR         (result, type, base, offset: i32)                5 words
0x0084  MOVE            (type, dst, src, size)                           5 words  [bulk copy]
0x0085  ZERO            (type, dst, size)                                4 words  [bulk zero]
```

#### Control Flow (0x0090-0x009F)

```
0x0090  PHI             (result, type, arg_count, [args...])             4+ words
0x0091  COPY            (result, type, source)                           4 words
0x0092  ARG             (result, type, arg_index: u32)                   4 words
0x0093  STATIC_CALL     (result, type, name_offset, arg_count, [args..]) 5+ words
0x0094  CALL            (result, type, callee, arg_count, [args...])     5+ words
0x0095  CLOSURE_CALL    (result, type, table_idx, ctx, argc, [args...])  6+ words
0x0096  INTER_CALL      (result, type, receiver, method_idx, argc, [a...]) 6+ words
0x0097  RET             (value)                                          2 words
0x0098  RET_VOID        ()                                               1 word
0x0099  COND_SELECT     (result, type, cond, true_val, false_val)        6 words
```

#### ARC (0x00C0-0x00CF)

```
0x00C0  RETAIN          (result, operand)                                3 words
0x00C1  RELEASE         (result, operand)                                3 words
0x00C2  ALLOC           (result, type, metadata, size)                   5 words
0x00C3  DEALLOC         (operand)                                        2 words
0x00C4  IS_UNIQUE       (result, operand)                                3 words
0x00C5  ARC_COPY_VALUE  (result, type, metadata, src, dst)               6 words  [VWT-aware]
0x00C6  ARC_DESTROY     (result, type, metadata, value)                  5 words  [VWT-aware]
```

#### VWT / Generics (0x00D0-0x00DF)

```
0x00D0  VWT_COPY            (result, metadata, src, dst)                 5 words
0x00D1  VWT_DESTROY         (result, metadata, value)                    4 words
0x00D2  VWT_SIZE            (result, metadata)                           3 words
0x00D3  VWT_STRIDE          (result, metadata)                           3 words
0x00D4  VWT_INIT_WITH_COPY  (result, metadata, src, dst)                 5 words
0x00D5  VWT_ASSIGN_WITH_COPY (result, metadata, src, dst)                5 words
0x00D6  VWT_INIT_WITH_TAKE  (result, metadata, src, dst)                 5 words
0x00D7  VWT_ASSIGN_WITH_TAKE (result, metadata, src, dst)                5 words
```

#### Concurrency (0x00E0-0x00EF)

```
0x00E0  ACTOR_ENQUEUE       (result, actor, job)                         4 words
0x00E1  ACTOR_RESIGN        (result, actor)                              3 words
0x00E2  TASK_CREATE          (result, func_name_offset, context)          4 words
0x00E3  TASK_SWITCH          (result, executor)                           3 words
0x00E4  TASK_CANCEL          (result, task)                               3 words
0x00E5  TASK_IS_CANCELLED    (result, task)                               3 words
0x00E6  AWAIT_FUTURE         (result, type, future)                       4 words
0x00E7  ASYNC_SUSPEND        (state_index: u32)                           2 words
0x00E8  ASYNC_RESUME         (state_index: u32)                           2 words
0x00E9  TASK_LOCAL_GET        (result, type, key_offset)                   4 words
0x00EA  TASK_LOCAL_BIND       (key_offset, value)                          3 words
```

#### Atomics (0x00F0-0x00FF)

```
0x00F0  ATOMIC_LOAD     (result, type, addr)                             4 words
0x00F1  ATOMIC_STORE    (type, addr, value)                              4 words
0x00F2  ATOMIC_ADD      (result, type, addr, value)                      5 words
0x00F3  ATOMIC_CAS      (result, type, addr, expected, desired)          6 words
0x00F4  ATOMIC_EXCHANGE (result, type, addr, value)                      5 words
```

---

## 7. Function Definitions (Section 0x06)

Each function is encoded as:

```
FUNC_BEGIN      (name_offset, type_idx, param_count, block_count, flags)
  flags: bit 0 = is_async
         bit 1 = is_export
         bit 2 = is_actor_isolated
         bit 3 = is_destructor
         bits 4-7 = reserved

  BLOCK_BEGIN   (block_id, kind: u8, succ_count, [succ_block_ids...], pred_count, [pred_block_ids...])
    [instructions...]
  BLOCK_END

  BLOCK_BEGIN   (...)
    [instructions...]
  BLOCK_END

FUNC_END
```

Block kinds:
```
0x00  plain       Unconditional fallthrough
0x01  if_         Conditional branch (2 successors)
0x02  ret         Return
0x03  exit        Unreachable after noreturn call
0x04  defer_      Defer block
0x05  first       Entry block
0x06  jump_table  Multi-way branch
```

---

## 8. Type Metadata (Section 0x03)

Extended metadata beyond what type declarations provide:

```
METADATA_ENTRY  (type_idx, flags, vwt_func_offset, size, stride)
  flags: bit 0 = is_sendable
         bit 1 = is_actor
         bit 2 = is_trivial (no ARC needed)
         bit 3 = has_vwt
         bit 4 = is_generic_param
         bits 5-31 = reserved
```

---

## 9. Decorations (Section 0x08)

Metadata attached to specific Result IDs, following SPIR-V's decoration model:

```
DECORATE  (target_id, decoration_kind, [payload...])

Decoration kinds:
  0x01  SOURCE_POS        (line: u32, col: u32, file_offset: u32)
  0x02  ARC_BALANCED      () — marks value as having balanced retain/release
  0x03  ACTOR_ISOLATION    (actor_type_idx)
  0x04  SENDABLE_CHECKED   () — Sendable verification passed
  0x05  NONISOLATED        () — accessible without await
  0x06  INLINABLE          () — monomorphize at instantiation
  0x07  BRANCH_HINT        (likely: i8)  — branch prediction
```

---

## 10. Validation Rules

### 10.1 Structural Validation

```
[S1] Every Result ID must be unique within its function
[S2] Every Result ID referenced as an operand must be defined
     before use (SSA dominance) or in a PHI instruction
[S3] Every block must end with a terminator (RET, RET_VOID, BR, BRIF, or JUMP_TABLE control)
[S4] Entry block must be block 0
[S5] Function signatures must match: param count matches ARG instructions,
     return type matches RET value type
[S6] PHI instructions must appear at the start of a block (before non-PHI instructions)
[S7] PHI arg count must equal predecessor count
[S8] The Bound in the header must be >= the highest Result ID + 1
[S9] All string offsets must be within the string heap bounds
[S10] All type references must be valid (pre-registered 0-22, or declared in Section 0x02)
```

### 10.2 ARC Validation

```
[A1] Every RETAIN must have a matching RELEASE on all control flow paths
[A2] RELEASE must not be called on a value with refcount 0 (no double-free)
[A3] Values returned from functions must be retained (caller takes ownership)
[A4] Values passed to DEALLOC must have refcount exactly 1
[A5] IS_UNIQUE result must only be used in conditional branches (COW pattern)
```

### 10.3 Type Validation

```
[T1] LOAD/STORE types must match the pointed-to type
[T2] ADD/SUB/MUL operands must have matching numeric types
[T3] Comparison operands must have matching types
[T4] CALL argument types must match function parameter types
[T5] PHI arguments must all have the same type as the PHI result
[T6] OFF_PTR base must be a pointer type
[T7] STRUCT field offsets must be within the struct's declared size
```

### 10.4 Concurrency Validation

```
[C1] ACTOR_ENQUEUE target must reference a value of actor type
[C2] AWAIT must appear only in functions marked is_async
[C3] Values crossing actor isolation boundaries must have Sendable types
[C4] Actor-isolated function access from outside the actor must go through ACTOR_ENQUEUE
[C5] NONISOLATED decorated functions must not access mutable actor state
```

### 10.5 Validation Levels

```bash
cot inspect main.wasm                     # [S*] structural only (fast)
cot inspect main.wasm --verify-arc        # [S*] + [A*] ARC balance checking
cot inspect main.wasm --verify-types      # [S*] + [T*] type consistency
cot inspect main.wasm --verify-concurrency # [S*] + [C*] actor isolation
cot inspect main.wasm --verify-all        # everything
```

---

## 11. Versioning and Compatibility

### Version Evolution

- **Minor version bump** (1.0 → 1.1): new opcodes added, existing opcodes unchanged. Old consumers skip unknown opcodes via word count.
- **Major version bump** (1.x → 2.0): breaking changes to existing opcode encoding. Consumers must check major version.

### Forward Compatibility

A consumer that understands version 1.0 can read a 1.1 file by skipping unknown opcodes (the word count tells it how many words to skip). This is the key advantage of the SPIR-V encoding — every instruction is self-describing in size.

### Backward Compatibility

A version 1.1 consumer can always read 1.0 files (subset of opcodes).

---

## 12. Size Characteristics

For a typical module (100 functions, 5000 SSA values):

```
Section           Estimated Size
──────────────────────────────
Header            20 bytes
String Heap       2-8 KB (function names, type names)
Type Declarations 1-4 KB (20-100 types)
Type Metadata     0.5-2 KB
Global Variables  0.2-1 KB
Function Decls    0.5-2 KB
Function Defs     20-80 KB (bulk of the data)
Debug Info        2-8 KB (optional)
Decorations       1-4 KB (optional)
──────────────────────────────
Total CIR:        ~30-110 KB

Wasm Code section: ~30-100 KB (similar size)
Total .wasm file:  ~60-210 KB (both sections)
Wasm-only:         ~30-100 KB (strip CIR)
CIR-only:          ~30-110 KB (strip Code)
```

The 32-bit word alignment adds ~20-30% overhead compared to LEB128 encoding but makes parsing trivially fast. For modules where size matters, external compression (gzip, LZ4) on the `.wasm` file is more effective than format-level compression.

---

## 13. Reference Comparison

```
                    CIR v1          SPIR-V          LLVM Bitcode    Wasm
────────────────────────────────────────────────────────────────────────────
Alignment           32-bit word     32-bit word     Bit-level       Byte (LEB128)
SSA values          Result IDs      Result IDs      Relative enc.   Stack-based
Types               Instructions    Instructions    Type table      Type section
Strings             Heap (offset)   Inline          Strtab block    Inline
Extensions          Reserved range  OpExtInst       Metadata        Custom sections
Parse complexity    Trivial loop    Trivial loop    Bitstream rdr   LEB128 decoder
Understands ARC     Yes             No              No              No
Understands actors  Yes             No              No              No
Understands VWT     Yes             No              No              No
Self-describing     Yes (word cnt)  Yes (word cnt)  Yes (abbrevs)   Yes (section sz)
```
