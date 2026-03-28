# Frontend Redesign: Zig-Inspired AST + Parser

**Date:** 2026-03-28
**Status:** Design document — ready for review
**Scope:** ast.zig + parser.zig redesign for src/libcot-zig/

---

## Why Redesign?

The current Cot frontend was originally ported from Go's `cmd/compile` patterns:
- Rich tagged unions with heap-allocated slices per node
- 60-line `deinit()` manually freeing each variant's slices
- Go-style `ArrayList → owned slice` for every variable-length list
- ~940 access sites across 17 files — but nobody is using the language yet

Zig's own compiler uses a fundamentally different approach that's worth studying. The question: **what can we borrow from Zig without rewriting the entire compiler?**

---

## What Zig Does Differently

### Node Representation

**Zig:** Every node is 13 bytes (padded to ~16):
```zig
pub const Node = struct {
    tag: Tag,           // 1 byte — enum with 165 variants
    main_token: u32,    // 4 bytes — primary token for this node
    data: Data,         // 8 bytes — union of two u32s
};
```

**Cot today:** Nodes are rich tagged unions:
```zig
pub const Node = union(enum) {
    decl: Decl,   // union(enum) { fn_decl: FnDecl, struct_decl: StructDecl, ... }
    expr: Expr,   // union(enum) { binary: Binary, call: Call, ... }
    stmt: Stmt,   // union(enum) { var_stmt: VarStmt, if_stmt: IfStmt, ... }
};
```
Each variant struct contains heap-allocated slices (`[]const Field`, `[]const NodeIndex`), strings, booleans, and spans.

### Variable-Length Data

**Zig:** Flat `extra_data: []u32` sidecar. Lists are stored as contiguous u32s referenced by `SubRange { start, end }`. No per-node heap allocations.

**Cot today:** Each node that needs a list allocates `[]const T` via `allocator.dupe()` during parsing.

### Storage Layout

**Zig:** `MultiArrayList(Node)` — struct-of-arrays. All tags in one array, all data in another. Cache-friendly for passes that scan tags.

**Cot today:** `ArrayListUnmanaged(Node)` — array-of-structs. Each node is a fat union.

### Accessor Pattern

**Zig:** Compact storage + `full.*` accessor structs. Consumers call `tree.ifFull(node)` which unpacks compact data into a rich `full.If` struct on the stack. 56 accessor functions, 16 full structs.

**Cot today:** Direct field access: `node.expr.call.args`, `decl.fn_decl.params`.

---

## Design: What We Borrow, What We Keep

### The Principle

**Adopt Zig's storage model. Keep Cot's semantic model.**

Zig's AST is optimized for a language with 165 syntax forms and no type annotations on nodes. Cot has ~50 syntax forms but richer per-node data (type params, doc comments, @safe flags, async/actor annotations, etc.). We can't blindly copy Zig's Tag enum — but we CAN adopt:

1. Compact fixed-size nodes
2. The extra_data sidecar for variable-length data
3. MultiArrayList storage
4. The `full.*` accessor pattern
5. Type-safe index wrappers (`enum(u32)` instead of raw `u32`)
6. Scratch buffer for list collection during parsing

### What We Keep From Cot

1. The three-level hierarchy (Decl/Expr/Stmt) — Cot's grammar is different from Zig's
2. Span tracking per node — Cot's error messages reference source ranges, not just tokens
3. @safe mode context in the parser
4. String interpolation parsing
5. All Cot-specific syntax (traits, impl blocks, actors, async let, etc.)

---

## Detailed Design

### 1. Node — Fixed-Size Compact Representation

```zig
pub const Node = struct {
    tag: Tag,           // 1 byte
    span_start: u32,    // 4 bytes — byte offset of node start
    data: Data,         // 8 bytes — two u32 payload

    // Total: 13 bytes, padded to 16 in MultiArrayList
};
```

**Why `span_start` instead of `main_token`?** Zig uses `main_token` because Zig's tokens carry their byte offset. Cot's scanner produces `TokenInfo` with spans, so we store the start byte offset directly. The end offset can be derived from the last child node or stored in extra_data when needed.

### 2. Tag — All Cot Syntax Forms

```zig
pub const Tag = enum(u8) {
    // === Declarations ===
    fn_decl,            // data: extra(FnDecl) + body node
    fn_decl_extern,     // data: extra(FnDeclExtern) — no body
    var_decl,           // data: name_token + extra(VarDecl)
    struct_decl,        // data: extra(StructDecl) + members SubRange
    enum_decl,          // data: extra(EnumDecl) + variants SubRange
    union_decl,         // data: extra(UnionDecl) + variants SubRange
    type_alias,         // data: name_token + target node
    type_alias_distinct,// data: name_token + target node
    import_decl,        // data: path_token (unused second slot)
    impl_block,         // data: extra(ImplBlock) + methods SubRange
    trait_decl,         // data: extra(TraitDecl) + methods SubRange
    impl_trait,         // data: extra(ImplTrait) + methods SubRange
    error_set_decl,     // data: extra(ErrorSetDecl) + variants SubRange
    test_decl,          // data: name_token + body node
    bench_decl,         // data: name_token + body node

    // === Expressions ===
    ident,              // data: name_token (unused second)
    literal_int,        // data: value_token (unused second)
    literal_float,      // data: value_token (unused second)
    literal_string,     // data: value_token (unused second)
    literal_char,       // data: value_token (unused second)
    literal_true,       // data: unused
    literal_false,      // data: unused
    literal_null,       // data: unused
    literal_undefined,  // data: unused
    literal_unreachable,// data: unused

    binary_add,         // data: left node + right node
    binary_sub,
    binary_mul,
    binary_div,
    binary_mod,
    binary_eq,
    binary_neq,
    binary_lt,
    binary_gt,
    binary_lte,
    binary_gte,
    binary_and,
    binary_or,
    binary_bit_and,
    binary_bit_or,
    binary_bit_xor,
    binary_shl,
    binary_shr,
    binary_concat,      // ++
    binary_pipe,        // |>

    unary_neg,          // data: operand node (unused second)
    unary_not,
    unary_bit_not,
    unary_addr_of,      // &x
    unary_deref,        // x.*
    unary_try,          // try x
    unary_await,        // await x

    call,               // data: callee node + extra(SubRange) for args
    call_one,           // data: callee node + single arg node (common case)
    call_zero,          // data: callee node (no args)

    index,              // data: base node + index node
    slice,              // data: base node + extra(Slice)
    field_access,       // data: base node + field_name_token

    array_literal,      // data: extra(SubRange) for elements
    array_literal_one,  // data: single element node
    array_literal_empty,// data: unused

    paren,              // data: inner node (unused second)

    if_expr,            // data: condition node + extra(IfExpr)
    if_expr_simple,     // data: condition node + then node (no else)
    switch_expr,        // data: subject node + extra(SubRange) for cases
    block_expr,         // data: extra(SubRange) for stmts + result node
    block_expr_two,     // data: stmt1 node + stmt2/result node

    struct_init,        // data: extra(StructInit) + fields SubRange
    new_expr,           // data: extra(NewExpr) + fields/args SubRange

    builtin_call,       // data: extra(BuiltinCall)
    string_interp,      // data: extra(SubRange) for segments

    type_named,         // data: name_token
    type_pointer,       // data: pointee node
    type_optional,      // data: child node
    type_error_union,   // data: error_set node + elem node
    type_slice,         // data: elem node
    type_array,         // data: size node + elem node
    type_map,           // data: key node + value node
    type_list,          // data: elem node
    type_function,      // data: extra(FnType)
    type_tuple,         // data: extra(SubRange) for element types
    type_generic,       // data: extra(GenericInstance)
    type_existential,   // data: trait node

    catch_expr,         // data: operand node + extra(CatchExpr)
    orelse_expr,        // data: operand node + fallback node
    error_literal,      // data: name_token
    closure_expr,       // data: extra(ClosureExpr)
    tuple_literal,      // data: extra(SubRange) for elements
    comptime_block,     // data: body node
    task_expr,          // data: body node (is_detached in extra)
    zero_init,          // data: unused

    // === Statements ===
    expr_stmt,          // data: expr node
    return_stmt,        // data: value node (or null_node)
    return_void,        // data: unused
    var_stmt,           // data: extra(VarStmt)
    const_stmt,         // data: extra(VarStmt)
    assign,             // data: target node + value node
    assign_add,         // data: target node + value node
    assign_sub,
    assign_mul,
    assign_div,
    assign_mod,
    assign_bit_and,
    assign_bit_or,
    assign_bit_xor,

    if_stmt,            // data: condition node + extra(IfStmt)
    if_stmt_simple,     // data: condition node + then node
    while_stmt,         // data: condition node + extra(WhileStmt)
    for_stmt,           // data: extra(ForStmt)
    block_stmt,         // data: extra(SubRange) for stmts
    break_stmt,         // data: value node (or null_node)
    break_labeled,      // data: label_token + value node
    continue_stmt,      // data: unused
    continue_labeled,   // data: label_token
    defer_stmt,         // data: expr node
    errdefer_stmt,      // data: expr node
    async_let,          // data: name_token + value node
    destructure_stmt,   // data: extra(DestructureStmt)

    bad_node,           // data: unused — error recovery sentinel

    comptime {
        // Must fit in 1 byte
        std.debug.assert(@sizeOf(Tag) == 1);
    }
};
```

**~130 tags** — fits in 1 byte with room to spare. Splitting binary/assign operators into individual tags (like Zig does) eliminates storing the operator token in the data payload.

### 3. Data — Two u32 Payload

```zig
pub const Data = union {
    node: Index,
    opt_node: OptionalIndex,
    token: TokenIndex,
    node_and_node: struct { Index, Index },
    node_and_opt_node: struct { Index, OptionalIndex },
    node_and_extra: struct { Index, ExtraIndex },
    node_and_token: struct { Index, TokenIndex },
    token_and_node: struct { TokenIndex, Index },
    extra_and_node: struct { ExtraIndex, Index },
    extra_range: SubRange,
    unused: void,
};
```

### 4. Extra Data Structs

These are packed into the `extra_data: []u32` sidecar:

```zig
// Function declaration (beyond what fits in Data)
pub const FnDecl = struct {
    name_token: TokenIndex,
    type_params: SubRange,      // [] = empty
    param_bounds: SubRange,     // parallel with type_params
    params: SubRange,           // Field indices in extra_data
    return_type: OptionalIndex,
    flags: FnFlags,             // packed u32: is_async, is_static, is_inlinable, etc.
    doc_comment_token: OptionalTokenIndex,
    span_end: u32,              // byte offset of node end
};

pub const FnFlags = packed struct(u32) {
    is_extern: bool = false,
    is_export: bool = false,
    is_async: bool = false,
    is_static: bool = false,
    is_inlinable: bool = false,
    is_nonisolated: bool = false,
    has_global_actor: bool = false,
    _padding: u25 = 0,
};

// Variable statement
pub const VarStmt = struct {
    name_token: TokenIndex,
    type_expr: OptionalIndex,
    span_end: u32,
};

// Struct declaration
pub const StructDecl = struct {
    name_token: TokenIndex,
    type_params: SubRange,
    fields: SubRange,           // Field extra data
    nested_decls: SubRange,
    layout: StructLayout,       // auto/packed/extern encoded as u32
    flags: StructFlags,         // packed u32: is_actor, etc.
    doc_comment_token: OptionalTokenIndex,
    span_end: u32,
};

// If expression/statement with full else + capture
pub const IfExpr = struct {
    then_branch: Index,
    else_branch: OptionalIndex,
    capture_token: OptionalTokenIndex,  // binding name token
    flags: IfFlags,             // capture_is_ptr
};

// While statement
pub const WhileStmt = struct {
    body: Index,
    label_token: OptionalTokenIndex,
    capture_token: OptionalTokenIndex,
    continue_expr: OptionalIndex,
    span_end: u32,
};

// For statement
pub const ForStmt = struct {
    binding_token: TokenIndex,
    index_binding_token: OptionalTokenIndex,
    iterable: Index,
    range_start: OptionalIndex,
    range_end: OptionalIndex,
    body: Index,
    label_token: OptionalTokenIndex,
    flags: ForFlags,            // is_inline, is_await
    span_end: u32,
};

// Switch case
pub const SwitchCase = struct {
    patterns: SubRange,
    guard: OptionalIndex,
    capture_token: OptionalTokenIndex,
    body: Index,
    flags: SwitchCaseFlags,     // is_range, capture_is_ptr
};

// Builtin call
pub const BuiltinCall = struct {
    kind: u32,                  // BuiltinKind enum value
    type_arg: OptionalIndex,
    args: [3]u32,               // up to 3 arg node indices (null_node if unused)
    span_end: u32,
};

// String interpolation segment (stored in extra_data)
// tag u32: 0 = text (next u32 = token index), 1 = expr (next u32 = node index)

// Closure
pub const ClosureExpr = struct {
    params: SubRange,
    return_type: OptionalIndex,
    body: Index,
    flags: ClosureFlags,        // is_sendable, is_async
    span_end: u32,
};

// Field (used by fn params, struct fields)
pub const Field = struct {
    name_token: TokenIndex,
    type_expr: Index,
    default_value: OptionalIndex,
    flags: FieldFlags,          // is_sending
};

// Generic instance: Name(T1, T2)
pub const GenericInstance = struct {
    name_token: TokenIndex,
    type_args: SubRange,
};
```

### 5. Index Types — Type-Safe Wrappers

```zig
pub const Index = enum(u32) {
    root = 0,
    _,
    pub fn toOptional(i: Index) OptionalIndex {
        const result: OptionalIndex = @enumFromInt(@intFromEnum(i));
        std.debug.assert(result != .none);
        return result;
    }
};

pub const OptionalIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,
    pub fn unwrap(oi: OptionalIndex) ?Index {
        return if (oi == .none) null else @enumFromInt(@intFromEnum(oi));
    }
};

pub const TokenIndex = u32;

pub const OptionalTokenIndex = enum(u32) {
    none = std.math.maxInt(u32),
    _,
    pub fn unwrap(oti: OptionalTokenIndex) ?TokenIndex {
        return if (oti == .none) null else @intFromEnum(oti);
    }
};

pub const ExtraIndex = enum(u32) { _ };

pub const SubRange = struct {
    start: ExtraIndex,
    end: ExtraIndex,
};
```

### 6. Ast Storage

```zig
pub const Ast = struct {
    source: []const u8,
    tokens: TokenList.Slice,        // from scanner
    nodes: std.MultiArrayList(Node),
    extra_data: []u32,
    errors: []const Error,
    safe_mode: bool,

    pub const TokenList = std.MultiArrayList(struct {
        tag: token.Token,
        start: u32,             // byte offset
    });

    pub fn deinit(self: *Ast, allocator: std.mem.Allocator) void {
        self.nodes.deinit(allocator);
        allocator.free(self.extra_data);
        allocator.free(self.errors);
    }

    // Accessors
    pub fn nodeTag(self: *const Ast, node: Index) Tag { ... }
    pub fn nodeData(self: *const Ast, node: Index) Data { ... }
    pub fn extraData(self: *const Ast, index: ExtraIndex, comptime T: type) T { ... }
    pub fn extraDataSlice(self: *const Ast, range: SubRange) []const u32 { ... }

    // Full accessors — unpack compact nodes into rich structs
    pub fn fnDecl(self: *const Ast, node: Index) full.FnDecl { ... }
    pub fn structDecl(self: *const Ast, node: Index) full.StructDecl { ... }
    pub fn ifExpr(self: *const Ast, node: Index) full.If { ... }
    pub fn forStmt(self: *const Ast, node: Index) full.For { ... }
    pub fn switchExpr(self: *const Ast, node: Index) full.Switch { ... }
    pub fn callExpr(self: *const Ast, node: Index) full.Call { ... }
    // ... etc for each complex node type
};
```

### 7. The `full` Namespace — Rich Accessor Structs

```zig
pub const full = struct {
    pub const FnDecl = struct {
        name: []const u8,           // resolved from token
        type_params: []const []const u8,
        params: []const Field,
        return_type: ?Index,
        body: ?Index,
        is_extern: bool,
        is_async: bool,
        is_static: bool,
        is_inlinable: bool,
        is_nonisolated: bool,
        global_actor: ?[]const u8,
        doc_comment: []const u8,
        span: Span,
    };

    pub const If = struct {
        condition: Index,
        then_branch: Index,
        else_branch: ?Index,
        capture: ?[]const u8,
        capture_is_ptr: bool,
        span: Span,
    };

    pub const Call = struct {
        callee: Index,
        args: []const Index,
        span: Span,
    };

    pub const For = struct {
        binding: []const u8,
        index_binding: ?[]const u8,
        iterable: ?Index,
        range_start: ?Index,
        range_end: ?Index,
        body: Index,
        label: ?[]const u8,
        is_inline: bool,
        is_await: bool,
        span: Span,
    };

    // ... one for each complex node type
};
```

**Key insight:** The `full.*` structs look exactly like the OLD Cot AST node structs — same field names, same types. Consumers get the same API. The only change is HOW they're obtained:

```zig
// OLD: direct field access
const f = node.decl.fn_decl;
const name = f.name;
const params = f.params;

// NEW: accessor function
const f = tree.fnDecl(node_idx);
const name = f.name;
const params = f.params;
```

The checker, lowerer, formatter don't need a rewrite — they need a mechanical find-and-replace.

### 8. Parser Changes

The parser switches from heap-allocating per-node to packing into extra_data:

```zig
pub const Parser = struct {
    gpa: std.mem.Allocator,
    scan: *Scanner,
    source: []const u8,

    // Output arrays (mutable during parsing, frozen after)
    tokens: Ast.TokenList,
    nodes: std.MultiArrayList(Node),
    extra_data: std.ArrayListUnmanaged(u32),
    errors: std.ArrayListUnmanaged(Ast.Error),

    // Temporary scratch buffer for collecting lists
    scratch: std.ArrayListUnmanaged(u32),

    // Context
    safe_mode: bool,
    current_impl_type: ?[]const u8,
    // ... same context fields as today

    /// Pack a struct into extra_data, return its start index.
    fn addExtra(p: *Parser, extra: anytype) !ExtraIndex {
        // Zig's comptime reflection pattern — iterates fields,
        // appends each as u32
    }

    /// Move scratch items to extra_data as a SubRange.
    fn listToSpan(p: *Parser, items: []const u32) !SubRange {
        try p.extra_data.appendSlice(p.gpa, items);
        return .{
            .start = @enumFromInt(p.extra_data.items.len - items.len),
            .end = @enumFromInt(p.extra_data.items.len),
        };
    }

    /// Add a node.
    fn addNode(p: *Parser, node: Node) !Index {
        const idx: Index = @enumFromInt(p.nodes.len);
        try p.nodes.append(p.gpa, node);
        return idx;
    }
};
```

**The scratch buffer** replaces per-list ArrayLists. Instead of:
```zig
var fields = std.ArrayListUnmanaged(ast.Field){};
defer fields.deinit(self.allocator);
// ... collect ...
return try self.allocator.dupe(ast.Field, fields.items);
```

We do:
```zig
const scratch_top = p.scratch.items.len;
defer p.scratch.shrinkRetainingCapacity(scratch_top);
// ... collect field indices into scratch ...
const fields = try p.listToSpan(p.scratch.items[scratch_top..]);
```

One scratch buffer, reused for every list. Zero per-list heap allocations.

### 9. BuiltinKind — Kept Separate, Still Comptime

BuiltinKind stays as a standalone enum with the comptime snakeToCamel transformation. It maps to a u32 value stored in extra_data. The enum lives in ast.zig but is independent of the node representation.

---

## Migration Path

### Phase 1: New ast.zig + parser.zig (src/libcot-zig/)

Write the new compact AST and parser from scratch in `src/libcot-zig/`. This doesn't touch `compiler/` at all. Verify with tests that parse Cot source and produce correct trees.

### Phase 2: Adapter Layer

Write a thin adapter that converts the compact AST back to the old `Decl`/`Expr`/`Stmt` union types. This lets the existing checker/lowerer/formatter work unchanged while we port them one at a time.

```zig
// adapter.zig — temporary bridge
pub fn toLegacyDecl(tree: *const Ast, node: Index) legacy.Decl {
    return switch (tree.nodeTag(node)) {
        .fn_decl => .{ .fn_decl = tree.fnDecl(node).toLegacy() },
        .struct_decl => .{ .struct_decl = tree.structDecl(node).toLegacy() },
        // ...
    };
}
```

### Phase 3: Port Consumers

Port checker.zig, lower.zig, formatter.zig one at a time to use `tree.fnDecl(node)` instead of `node.decl.fn_decl`. Each is a mechanical transformation — the `full.*` structs have the same field names.

### Phase 4: Delete Adapter

Once all consumers use the new API, delete the adapter layer and the old AST types.

---

## What This Buys Us

| Metric | Current (Go-style) | After (Zig-style) |
|--------|--------------------|--------------------|
| Node size | 48-200+ bytes (varies) | 16 bytes (fixed) |
| Per-node heap allocs | Yes (params, fields, etc.) | Zero |
| `deinit()` | 60 lines of switch/case | 3 lines (free 3 arrays) |
| Storage layout | Array-of-structs | Struct-of-arrays (MultiArrayList) |
| List collection | ArrayList per list | Single scratch buffer |
| Index type safety | Raw u32 + sentinel | `enum(u32)` wrappers |
| Consumer API | `node.decl.fn_decl.params` | `tree.fnDecl(node).params` |
| Consumer migration | — | Mechanical find-and-replace |

---

## What This Does NOT Change

- The Cot grammar and syntax
- The three-level Decl/Expr/Stmt semantic model
- The compilation pipeline (scan → parse → check → lower → SSA)
- The token definitions
- The scanner
- The error reporting
- @safe mode behavior
- String interpolation
- Any language feature

This is purely a representation change. The tree structure is identical — only how it's stored in memory changes.

---

## Files Affected

**New files (src/libcot-zig/):**
- `ast.zig` — Node, Tag, Data, extra structs, full accessors, BuiltinKind
- `parser.zig` — Recursive descent parser producing compact AST

**Unchanged:**
- `token.zig`, `source.zig`, `errors.zig`, `scanner.zig`, `debug.zig`, `target.zig`

**Later (when porting to src/):**
- `checker.zig` — use `tree.fnDecl()` accessors
- `lower.zig` — use `tree.fnDecl()` accessors
- `formatter.zig` — use `tree.fnDecl()` accessors
- LSP files — use accessors

---

## Open Questions

1. **Token storage:** Should we store tokens in a `MultiArrayList` like Zig, or keep the current `Scanner` interface? Zig pre-tokenizes everything into an array; Cot's scanner is streaming. Pre-tokenizing would allow the parser to use token indices directly (no `peek_tok` state), which is cleaner.

2. **Span storage:** Zig derives spans from `main_token` + grammar knowledge. We store `span_start` in each node. Should we also store `span_end` (4 more bytes per node) or compute it on demand? Storing it is simpler; computing saves memory.

3. **Doc comments:** Zig handles doc comments at the token level. Cot accumulates `///` lines into a string during parsing. With the compact model, doc comments could be stored as token ranges in extra_data rather than allocated strings.

4. **How far to push the "two" optimization?** Zig has `call_one` vs `call`, `block_two` vs `block`, etc. — saving extra_data for common 0-1 element cases. Worth it for calls (most have 0-3 args) and blocks (many have 1-2 stmts). Diminishing returns beyond that.

---

## Implementation Order

1. Write `ast.zig` — Tag enum, Data union, extra structs, Index types, full accessors, BuiltinKind
2. Write tests for `ast.zig` — verify accessor round-trips
3. Write `parser.zig` — recursive descent producing compact AST
4. Write parser tests — verify parse of all Cot syntax forms
5. Write companion docs (`ac/src/libcot-zig/ast.md`, `parser.md`)
6. Verify: `zig test src/libcot-zig/ast.zig` and `zig test src/libcot-zig/parser.zig`
