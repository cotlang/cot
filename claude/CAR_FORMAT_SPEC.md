# CAR Binary Format Specification (Draft)

**Cot AST Representation — Binary AST interchange format**

**Date:** 2026-03-31
**Status:** Design document — not yet implemented
**Purpose:** Decouple libts from libcot via a binary AST format, same pattern as CIR

---

## 1. Motivation

Currently, `ts_transform.zig` lives inside libcot and imports both libts types (via module import) and Cot AST types (via relative import). This works but couples the two codebases at the Zig module level.

A binary AST format (CAR) would create a hard C ABI boundary:

```
libts (any language)  → CAR bytes → [C ABI: car_to_ast()] → libcot (Zig)

                                     Same pattern as:
libcot (Zig)          → CIR bytes → [C ABI: clif_compile()] → libclif (Rust)
```

**Benefits:**
- libts could be written in Rust, Go, or any language with C FFI
- The transform becomes a serialization step, not a shared-memory AST walk
- Testing: dump CAR bytes to files, inspect with tools, replay without re-parsing
- Caching: hash source → cache CAR bytes → skip parse+transform on unchanged files
- Parallel compilation: parse TS files in parallel, serialize to CAR, merge into checker

**Cost:**
- Serialization/deserialization overhead (~5-10% of parse time)
- Two representations of the AST (CAR binary + Cot AST in memory)
- More code to maintain (writer + reader)

---

## 2. Design Principles

1. **CIR-style encoding** — 32-bit word-aligned, SPIR-V inspired
2. **Tree-native** — unlike CIR (SSA/linear), CAR represents a tree structure
3. **String heap** — deduplicated, offset-referenced (same as CIR)
4. **Section-based** — declarations, expressions, statements, types in separate sections
5. **Self-describing** — each node has a kind tag + child count, so unknown nodes can be skipped
6. **Round-trip safe** — CAR → Cot AST → CAR produces identical bytes

---

## 3. Binary Layout

```
[CAR Header]           5 words (20 bytes)
[String Heap]          Section 0x01
[Node Pool]            Section 0x02
[Root Declarations]    Section 0x03
[File Metadata]        Section 0x04
```

### 3.1 Header (5 words, 20 bytes)

```
Word 0: Magic             0x00434152  ("CAR\0" little-endian)
Word 1: Version           0x00010000  (major=1, minor=0)
Word 2: Generator         0           (0 = libts, 1 = cot parser, 2+ = external tools)
Word 3: Node Count        u32         Total number of nodes in the pool
Word 4: Reserved          0
```

### 3.2 String Heap (Section 0x01)

Same format as CIR string heap:

```
Section header: (word_count << 16) | 0x01
Entries: [u32 offset, u32 length, ...bytes (padded to 4-byte alignment)]
```

All identifiers, string literals, and type names reference the string heap by offset. Deduplication happens at write time.

### 3.3 Node Pool (Section 0x02)

Flat array of AST nodes. Each node:

```
Word 0: (word_count << 16) | node_kind
Word 1: span_start (byte offset in source)
Word 2: span_end (byte offset in source)
Words 3..N: Kind-specific fields (child indices, string heap offsets, flags)
```

Node kinds map directly to Cot AST node types:

#### Node Kind Table

```
// Declarations (0x0100 range)
0x0100  FN_DECL           name:str, params:u32[], return_type:idx, body:idx, flags:u32
0x0101  VAR_DECL           name:str, type:idx, value:idx, flags:u32
0x0102  STRUCT_DECL        name:str, fields:u32[], flags:u32
0x0103  IMPL_BLOCK         type_name:str, methods:u32[]
0x0104  TRAIT_DECL         name:str, methods:u32[]
0x0105  IMPL_TRAIT         trait:str, target:str, methods:u32[]
0x0106  ENUM_DECL          name:str, variants:u32[]
0x0107  UNION_DECL         name:str, variants:u32[]
0x0108  TYPE_ALIAS         name:str, target:idx
0x0109  IMPORT_DECL        path:str
0x010A  ERROR_SET          name:str, variants:str[]
0x010B  TEST_DECL          name:str, body:idx
0x010C  BENCH_DECL         name:str, body:idx

// Expressions (0x0200 range)
0x0200  IDENT              name:str
0x0201  LITERAL            kind:u8, value:str
0x0202  BINARY             op:u8, left:idx, right:idx
0x0203  UNARY              op:u8, operand:idx
0x0204  CALL               callee:idx, args:u32[]
0x0205  INDEX              base:idx, index:idx
0x0206  FIELD_ACCESS       base:idx, field:str
0x0207  ARRAY_LITERAL      elements:u32[]
0x0208  STRUCT_INIT        type_name:str, fields:FieldInit[]
0x0209  NEW_EXPR           type_name:str, args:u32[]
0x020A  IF_EXPR            cond:idx, then:idx, else:idx
0x020B  SWITCH_EXPR        subject:idx, cases:Case[]
0x020C  BLOCK_EXPR         stmts:u32[], result:idx
0x020D  STRING_INTERP      segments:Segment[]
0x020E  CLOSURE_EXPR       params:Field[], return_type:idx, body:idx
0x020F  TRY_EXPR           operand:idx
0x0210  CATCH_EXPR         operand:idx, capture:str, handler:idx
0x0211  ORELSE_EXPR        operand:idx, fallback:idx
0x0212  AWAIT_EXPR         operand:idx
0x0213  ERROR_LITERAL      name:str
0x0214  PAREN              inner:idx
0x0215  BUILTIN_CALL       kind:u16, args:u32[]
0x0216  TYPE_EXPR          type_kind:u8, ...type-specific fields
0x0217  ADDR_OF            operand:idx
0x0218  DEREF              operand:idx

// Statements (0x0300 range)
0x0300  EXPR_STMT          expr:idx
0x0301  RETURN_STMT        value:idx
0x0302  IF_STMT            cond:idx, then:idx, else:idx
0x0303  WHILE_STMT         cond:idx, body:idx
0x0304  FOR_STMT           binding:str, iterable:idx, body:idx
0x0305  BLOCK_STMT         stmts:u32[]
0x0306  BREAK_STMT         label:str, value:idx
0x0307  CONTINUE_STMT      label:str
0x0308  DEFER_STMT         body:idx, is_errdefer:u8
0x0309  VAR_STMT           name:str, type:idx, value:idx, flags:u32
0x030A  ASSIGN_STMT        target:idx, value:idx, op:u8
0x030B  ASYNC_LET          name:str, value:idx

// Fields and parameters (inline, not standalone nodes)
// Encoded as: name:str, type:idx, default:idx, flags:u8
```

### 3.4 Root Declarations (Section 0x03)

```
Section header: (word_count << 16) | 0x03
u32 count
u32[count] declaration_indices  (indices into Node Pool)
```

### 3.5 File Metadata (Section 0x04)

```
Section header: (word_count << 16) | 0x04
u32 filename_offset      (string heap offset)
u32 flags                (bit 0 = safe_mode)
```

---

## 4. Encoding Details

### 4.1 Node Index

`u32` — index into the Node Pool. `0xFFFFFFFF` = null node (same as Cot AST's `null_node`).

### 4.2 Variable-Length Arrays

Child arrays (params, args, stmts, etc.) are encoded inline:

```
u32 count
u32[count] values
```

Padding to 4-byte alignment after the array.

### 4.3 Field Encoding

```
u32 name_offset     (string heap)
u32 type_node_idx   (null_node if no annotation)
u32 default_idx     (null_node if no default)
u8  flags           (bit 0 = is_sending)
u8[3] padding
```

### 4.4 Flags Word

Bitfield for declaration flags:

```
Bit 0:  is_const
Bit 1:  is_export
Bit 2:  is_extern
Bit 3:  is_async
Bit 4:  is_static
Bit 5:  is_inlinable
Bit 6:  is_nonisolated
Bit 7:  safe_mode
Bit 8:  is_actor
Bit 9:  is_weak
Bit 10: is_unowned
```

### 4.5 Operator Encoding

Single byte matching Cot's Token enum value:

```
0x00: +    0x01: -    0x02: *    0x03: /    0x04: %
0x05: &    0x06: |    0x07: ^    0x08: <<   0x09: >>
0x0A: ==   0x0B: !=   0x0C: <    0x0D: <=   0x0E: >
0x0F: >=   0x10: &&   0x11: ||   0x12: !    0x13: ~
0x14: =    0x15: +=   0x16: -=   0x17: *=   0x18: /=
0x19: %=   0x1A: &=   0x1B: |=   0x1C: ^=
```

### 4.6 Type Expression Encoding

Type nodes use a sub-kind byte:

```
0x00: named          name:str
0x01: pointer        elem:idx
0x02: optional       elem:idx
0x03: error_union    error:idx, elem:idx
0x04: slice          elem:idx
0x05: array          size:idx, elem:idx
0x06: map            key:idx, value:idx
0x07: list           elem:idx
0x08: function       params:idx[], ret:idx
0x09: tuple          elems:idx[]
0x0A: generic_inst   name:str, args:idx[]
0x0B: existential    trait:idx
```

---

## 5. C ABI Entry Points

```c
// Writer: Cot AST → CAR bytes (called by libts transform or Cot parser)
// Returns allocated buffer. Caller frees with car_free().
CarResult car_from_ast(const CotAst* ast);

// Reader: CAR bytes → Cot AST (called by libcot driver)
// Populates the provided Ast struct.
int car_to_ast(const uint8_t* bytes, size_t len, CotAst* out_ast);

// Free a CAR buffer returned by car_from_ast.
void car_free(CarResult result);

typedef struct {
    uint8_t* data;
    size_t   len;
    int      error;  // 0 = success
} CarResult;
```

### 5.1 Usage in the Pipeline

```
Current:
  .ts → libts parse → TS AST → ts_transform.zig (in-process) → Cot AST → checker

With CAR:
  .ts → libts parse → TS AST → car_write (in libts) → CAR bytes
      → [C ABI boundary]
      → car_read (in libcot) → Cot AST → checker

With caching:
  .ts → hash source → cache hit? → load CAR from disk → car_read → Cot AST
                     → cache miss → libts parse → TS AST → car_write → save + car_read
```

---

## 6. Size Estimates

For a typical 100-line TS file producing ~200 AST nodes:
- String heap: ~2 KB (identifiers, literals)
- Node pool: ~200 × 32 bytes avg = ~6 KB
- Root decls + metadata: ~100 bytes
- **Total: ~8 KB** (vs ~30 KB for the in-memory Cot AST with pointers)

The binary format is more compact than the in-memory representation because it uses indices instead of pointers (4 bytes vs 8 bytes) and packs flags into bitfields.

---

## 7. Implementation Plan

| Phase | What | Effort |
|-------|------|--------|
| 1 | Define `car_format.zig` with opcodes and encoding helpers | 1 day |
| 2 | `car_write.zig` — serialize Cot AST → CAR bytes | 2 days |
| 3 | `car_read.zig` — deserialize CAR bytes → Cot AST | 2 days |
| 4 | Wire into driver: parse → transform → car_write → car_read → check | 1 day |
| 5 | C ABI wrapper (`car_api.zig` with `export fn`) | 1 day |
| 6 | Caching: hash source → CAR file on disk | 2 days |
| 7 | Move libts transform to produce CAR directly | 2 days |

**Total: ~11 days.** Not urgent — the current in-process transform works. This becomes valuable when:
- libts needs to be a separate binary (e.g., for parallel compilation)
- Caching becomes important for large projects
- We want libts in a different language (Rust, Go)

---

## 8. Comparison with CIR

| Aspect | CIR | CAR |
|--------|-----|-----|
| Purpose | SSA IR → machine code | AST → type checker |
| Structure | Linear (basic blocks + instructions) | Tree (declarations + nested nodes) |
| Consumer | Cranelift (Rust) | Cot checker (Zig) |
| Encoding | SPIR-V word-aligned | SPIR-V word-aligned (same) |
| String heap | Yes | Yes (same format) |
| Type system | Pre-registered primitives | Node-embedded type expressions |
| Complexity | ~100 opcodes | ~50 node kinds |
| Typical size | 10-50 KB per function | 5-20 KB per file |

Both formats share the same encoding philosophy: word-aligned, section-based, string-heap-referenced. A tool that reads CIR headers can read CAR headers. The difference is content — CIR has SSA instructions, CAR has tree nodes.
