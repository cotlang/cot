# ast.zig Port Strategy

**Status:** Ready to execute — research complete, transformation designed
**Source:** `compiler/frontend/ast.zig` (733 lines)
**Target:** `src/libcot-zig/ast.zig` (estimated ~650 lines after cleanup)

---

## Key Transformation: BuiltinKind (183 lines → ~70 lines)

The original has 61 builtin variants repeated THREE times:
1. Enum definition (61 variants)
2. StaticStringMap (61 entries mapping source name → enum)
3. `sourceName()` switch (61 arms mapping enum → source name)

**Finding:** 59 of 61 builtins follow a mechanical `snake_case → camelCase` rule. Only 2 exceptions:
- `type_of` → `TypeOf` (capital T, not `typeOf`)
- `atomic_cas` → `atomicCAS` (all-caps CAS, not `atomicCas`)

**Transformation:** Replace the triple repetition with a comptime-generated mapping:

```zig
pub const BuiltinKind = enum {
    size_of, align_of, enum_len,
    int_cast, float_cast, ...

    /// Comptime override table for source names that don't follow snake_to_camel.
    const source_name_overrides = .{
        .{ .type_of, "TypeOf" },
        .{ .atomic_cas, "atomicCAS" },
    };

    /// Get the user-facing source name (@sizeOf, @intCast, etc.).
    /// Auto-converts snake_case tag name to camelCase, with overrides for exceptions.
    pub fn sourceName(self: BuiltinKind) []const u8 {
        inline for (source_name_overrides) |entry| {
            if (self == entry[0]) return entry[1];
        }
        return comptime snakeToCamel(@tagName(self));
        // OR: use a comptime-built lookup array
    }

    /// Build the reverse lookup map at comptime (source name string → enum).
    pub const map = blk: {
        // Use @typeInfo(BuiltinKind).enum.fields to iterate all variants
        // For each, compute sourceName and add to StaticStringMap
        break :blk std.StaticStringMap(BuiltinKind).initComptime(entries);
    };

    pub fn fromString(s: []const u8) ?BuiltinKind {
        return map.get(s);
    }
};
```

**Challenge:** `snakeToCamel` needs to work at comptime. Zig can do comptime string manipulation but it requires careful handling (comptime allocator or fixed buffer). Alternative: use a comptime lookup array indexed by enum value, populated with `inline for`.

**Fallback if comptime string manipulation is too complex:** Keep the explicit `sourceName` switch (it's the most readable form) but generate the StaticStringMap from `@typeInfo` + `sourceName()` at comptime. This eliminates one of the three repetitions.

---

## Other Cleanup

### Expand one-liner structs

The original crams complex structs onto single lines:
```zig
pub const VarDecl = struct { name: []const u8, type_expr: NodeIndex, value: NodeIndex, is_const: bool, doc_comment: []const u8 = "", span: Span };
```

Port to multi-line with field documentation:
```zig
pub const VarDecl = struct {
    name: []const u8,
    type_expr: NodeIndex,
    value: NodeIndex,
    is_const: bool,
    doc_comment: []const u8 = "",
    span: Span,
};
```

### Remove reference comments

Delete: `// Rust: where T: Ord clause`, `// Go 1.18: [T comparable]`, `// Swift SE-xxxx`, `// Reference: Go's walkNew`

### Add documentation

- Module doc explaining the three-level node hierarchy (Decl, Expr, Stmt)
- Doc comments on Decl, Expr, Stmt unions explaining each variant category
- Doc comments on the Ast storage struct explaining the node arena pattern

### Ast.deinit() — consider simplification

The `deinit()` function manually walks every node type to free allocated slices. This is 60 lines of repetitive switch/case. Consider whether there's a data-driven approach (e.g., a `freeSliceFields` helper using reflection). However, this may be over-engineering — the explicit approach is clear and correct.

---

## File Layout (target structure)

```
//! Module doc (AST structure, three-level hierarchy)

// Imports
// Core index types (NodeIndex, null_node, NodeList)

// File — top-level source file representation
// Decl union + all declaration structs
// Field, EnumVariant, UnionVariant (shared sub-structs)
// Expr union + all expression structs
// BuiltinKind enum (TRANSFORMED — data-driven)
// BuiltinCall, StringInterp, TypeExpr, TypeKind
// Stmt union + all statement structs
// Node union (Decl | Expr | Stmt)
// Ast storage struct

// Tests
```

---

## Companion doc

`ac/src/libcot-zig/ast.md` should cover:
- The three-level hierarchy and why it exists
- How NodeIndex works (arena-based, null_node sentinel)
- The BuiltinKind transformation (before/after, why it matters)
- How the Ast arena pattern works (addNode → getNode → indices)
- The deinit ownership model

---

## Dependencies

ast.zig depends on: source.zig (Span, Pos), token.zig (Token)
Both already ported. ast.zig can compile immediately after porting.

## Verification

After porting, run: `zig test src/libcot-zig/ast.zig`
Should pass all existing tests (8 tests) plus any new ones added.
