# Execution Plan: Self-Hosted Type System & Checker

**Port `compiler/frontend/types.zig` (673 lines) + `compiler/frontend/checker.zig` (3,870 lines) to Cot.**

Target: `self/frontend/types.cot` + `self/frontend/checker.cot`
Estimated output: ~3,500–4,500 lines of Cot (compression from Zig's verbosity + simpler error handling)
Current self/: 5,599 lines → target: ~9,500–10,100 lines (~70–75% of self-hosting)

---

## Critical Constraint: No `List(struct)` / `List(union)`

The ast.cot comment documents a known compiler limitation: `List(union)` and `List(struct)` crash. All complex data must use the **parallel-array flat encoding** pattern established in ast.cot.

This means the Zig `Type` tagged union (`union(enum) { basic, pointer, optional, ... }`) cannot be stored as `List(Type)`. Instead, use parallel arrays with a tag discriminant, exactly like `Ast.tags` + `Ast.data`.

---

## Phase 1: `self/frontend/types.cot` (~600–800 lines)

### Reference: `compiler/frontend/types.zig` (673 lines)

### 1A. BasicKind Enum and TypeTag Enum

```cot
/// Primitive type kinds (matches Zig BasicKind, line 8-27)
const BasicKind = enum(u8) {
    i8, i16, i32, i64,
    u8, u16, u32, u64,
    f32, f64,
    bool, void, noreturn,
    untyped_int, untyped_float, untyped_bool, untyped_null, invalid
}

/// Discriminant for the Type union — replaces Zig's `Type = union(enum) {...}`
const TypeTag = enum(u8) {
    basic, pointer, optional, error_union, error_set,
    slice, array, map, list, tuple, struct_type,
    enum_type, union_type, func, future
}
```

Port `BasicKind` methods: `name()`, `isNumeric()`, `isInteger()`, `isSigned()`, `isUnsigned()`, `isFloat()`, `isUntyped()`, `size()` — all simple switches, ~80 lines.

### 1B. TypeRegistry — Flat Encoded Type Storage

**The core design decision.** Replace `ArrayList(Type)` with parallel arrays:

```cot
struct TypeRegistry {
    /// Type tag per index (TypeTag as int)
    tags: List(int),

    /// Per-tag data, stored as int values.
    /// Layout depends on tag:
    ///   basic:       data0 = BasicKind as int
    ///   pointer:     data0 = elem TypeIndex
    ///   optional:    data0 = elem TypeIndex
    ///   error_union: data0 = elem TypeIndex, data1 = error_set TypeIndex
    ///   error_set:   data0 = name string index, data1 = variants_start, data2 = variants_count
    ///   slice:       data0 = elem TypeIndex
    ///   array:       data0 = elem TypeIndex, data1 = length
    ///   map:         data0 = key TypeIndex, data1 = value TypeIndex
    ///   list:        data0 = elem TypeIndex
    ///   tuple:       data0 = elems_start, data1 = elems_count  (into type_list)
    ///   struct_type: data0 = name str idx, data1 = fields_start, data2 = fields_count, data3 = size, data4 = align, data5 = layout
    ///   enum_type:   data0 = name str idx, data1 = variants_start, data2 = variants_count, data3 = backing_type
    ///   union_type:  data0 = name str idx, data1 = variants_start, data2 = variants_count, data3 = tag_type
    ///   func:        data0 = params_start, data1 = params_count, data2 = return_type
    ///   future:      data0 = result_type
    data0: List(int),
    data1: List(int),
    data2: List(int),
    data3: List(int),
    data4: List(int),
    data5: List(int),

    /// Auxiliary lists for variable-length sub-arrays
    /// Struct fields: [name_str_idx, type_idx, offset, default_value] × N
    field_data: List(int),
    field_names: List(string),
    /// Enum/union variants: [name_str_idx, value_or_type] × N
    variant_data: List(int),
    variant_names: List(string),
    /// Tuple element types, func param types
    type_list: List(int),
    /// Func param names
    param_names: List(string),
    /// Error set variant names
    error_names: List(string),
    /// String pool for type names
    type_name_strings: List(string),
    /// Name → TypeIndex lookup (manual linear scan, no Map dependency)
    named_type_names: List(string),
    named_type_indices: List(int),
    /// Method registry: type_name → method list
    /// Encoded as parallel arrays (avoid Map(string, List))
    method_type_names: List(string),
    method_names: List(string),
    method_func_types: List(int),    // TypeIndex of the method's func type
    method_self_types: List(int),    // TypeIndex of self param
}
```

**Pre-registered constants** (lines 0–22 in Zig): INVALID=0, BOOL=1, I8=2..I64=5, U8=6..U64=9, F32=10, F64=11, VOID=12, UNTYPED_INT=13, UNTYPED_FLOAT=14, UNTYPED_BOOL=15, UNTYPED_NULL=16, STRING=17, NORETURN=22. Initialize in `registryInit()`.

### 1C. Factory Methods

Port these as methods on `impl TypeRegistry`:

| Method | Complexity | Notes |
|--------|-----------|-------|
| `add(tag, d0, d1, d2, d3, d4, d5)` | Simple | Append to all parallel lists, return new index |
| `getTag(idx)` → int | Simple | `tags.get(idx)` |
| `getData(idx, field)` → int | Simple | Switch on field to pick data0..data5 |
| `makePointer(elem)` → int | Simple | `add(pointer, elem, 0, 0, 0, 0, 0)` |
| `makeOptional(elem)` → int | Simple | Same pattern |
| `makeSlice(elem)` → int | Simple | Same pattern |
| `makeArray(elem, len)` → int | Simple | 2 data fields |
| `makeErrorUnion(elem)` → int | Medium | 2 data fields |
| `makeFunc(params, return_type)` → int | Medium | Pack params into type_list, store start+count |
| `makeTuple(elems)` → int | Medium | Pack elems into type_list |
| `lookupByName(name)` → ?int | Medium | Linear scan of named_type_names |
| `typeName(idx)` → string | Medium | Switch on tag, format name |
| `sizeOf(idx)` → int | Medium | Switch on tag (STRING=24, etc.) |
| `isAssignable(from, to)` → bool | Complex | Largest method (~100 lines Zig) |
| `commonType(a, b)` → int | Complex | Peer type resolution (~50 lines Zig) |
| `equal(a, b)` → bool | Medium | Structural equality |
| `isTrivial(idx)` → bool | Simple | Tag-based check |
| `couldBeARC(idx)` → bool | Simple | Pointer/optional check |
| `registerMethod(type_name, method_name, func_type, self_type)` | Medium | Append to parallel method arrays |
| `lookupMethod(type_name, method_name)` → ?(int, int) | Medium | Linear scan of method arrays, return (func_type, self_type) |

### 1D. Tests (~15 tests)

```cot
test "pre-registered types" { ... }
test "makePointer" { ... }
test "makeOptional" { ... }
test "sizeOf basics" { ... }
test "isAssignable int widening" { ... }
test "isAssignable T to ?T" { ... }
test "commonType" { ... }
test "lookupByName" { ... }
test "registerMethod and lookupMethod" { ... }
test "equal" { ... }
test "typeName" { ... }
test "makeFunc" { ... }
test "makeTuple" { ... }
test "struct type with fields" { ... }
test "enum type with variants" { ... }
```

### 1E. Imports

```cot
import "std/list"
import "std/string"
```

No other dependencies. types.cot is a leaf module.

---

## Phase 2: `self/frontend/checker.cot` (~2,500–3,500 lines)

### Reference: `compiler/frontend/checker.zig` (3,870 lines)

### 2A. Symbol and Scope

```cot
/// Symbol kinds (matches Zig SymbolKind)
const SymbolKind = enum(u8) { variable, constant, function, type_name, parameter }

/// Scope — flat parallel arrays, no Map dependency
struct Scope {
    sym_names: List(string),
    sym_kinds: List(int),          // SymbolKind as int
    sym_types: List(int),          // TypeIndex
    sym_nodes: List(int),          // NodeIndex
    sym_mutable: List(int),        // 0 or 1
    sym_extern: List(int),         // 0 or 1
    sym_const_val: List(int),      // const i64 value (0 if not const)
    sym_has_const: List(int),      // 1 if sym_const_val is valid
    sym_used: List(int),           // 0 or 1
    parent_scope: ?int,            // index into checker's scope_pool, or null
}
```

**Scope pool pattern:** Instead of heap-allocating Scope structs (which would need `new` and `*Scope` chains), use a pool in the Checker:

```cot
struct Checker {
    // Scope pool — avoids new/heap Scope structs
    scope_tags: List(int),           // unused, but keeps indices aligned
    scope_parents: List(int),        // parent scope index (-1 for none)
    // Per-scope symbol storage: symbols are appended to global flat lists
    // with a scope_id tag to identify which scope they belong to
    scope_sym_start: List(int),      // start index into sym_* arrays for this scope
    scope_sym_count: List(int),      // count of symbols in this scope
    current_scope: int,              // index of current scope in pool
    // ... all sym_* arrays are global, filtered by scope_id during lookup
}
```

**Actually, simpler approach:** Use the same flat-array approach but with a linked-list via parent indices:

```cot
struct Checker {
    // Global symbol table — all symbols across all scopes
    sym_names: List(string),
    sym_kinds: List(int),
    sym_types: List(int),
    sym_nodes: List(int),
    sym_mutable: List(int),
    sym_scope: List(int),       // which scope this symbol belongs to
    // Scope hierarchy
    scope_parent: List(int),    // parent scope index (-1 = none)
    scope_count: int,           // total scopes created
    current_scope: int,         // active scope index
    ...
}
```

**Symbol lookup:** Walk up scope chain:
```cot
fn lookup(name: string) ?int {
    var sc = self.current_scope
    while (sc >= 0) {
        // scan symbols in this scope
        var i = 0
        while (i < self.sym_names.count) {
            if (self.sym_scope.get(i) == sc and strEqual(self.sym_names.get(i), name)) {
                return self.sym_types.get(i)
            }
            i = i + 1
        }
        sc = self.scope_parent.get(sc)
    }
    return null
}
```

### 2B. Checker Struct Fields

```cot
struct Checker {
    // Type system
    types: *TypeRegistry,

    // AST being checked (swapped during cross-file generics)
    tree: *Ast,

    // Error reporting
    errors: *ErrorReporter,

    // Scope management (flat-encoded)
    sym_names: List(string),
    sym_kinds: List(int),
    sym_types: List(int),
    sym_nodes: List(int),
    sym_mutable: List(int),
    sym_scope: List(int),
    scope_parent: List(int),
    scope_count: int,
    current_scope: int,

    // Expression type cache: NodeIndex → TypeIndex
    // Parallel arrays (avoid Map)
    expr_type_nodes: List(int),
    expr_type_types: List(int),

    // Function context
    current_return_type: int,    // TypeIndex
    expected_type: int,          // for anon struct init
    in_loop: bool,

    // Mode flags
    safe_mode: bool,
    current_switch_enum_type: int,
}
```

### 2C. Three-Pass checkFile

Port directly from Zig (line 309):

```cot
fn checkFile() void {
    self.safe_mode = self.tree.safe_mode

    // Pass 1: collect type declarations (struct, enum, union, error_set, trait)
    var i = 0
    while (i < self.tree.file_decls.count) {
        self.collectTypeDecl(self.tree.file_decls.get(i))
        i = i + 1
    }
    // Pass 2: collect non-type declarations (fn, var, impl)
    i = 0
    while (i < self.tree.file_decls.count) {
        self.collectNonTypeDecl(self.tree.file_decls.get(i))
        i = i + 1
    }
    // Pass 3: check all bodies
    i = 0
    while (i < self.tree.file_decls.count) {
        self.checkDecl(self.tree.file_decls.get(i))
        i = i + 1
    }
}
```

### 2D. Expression Type Checking — Phased Port

The `checkExpr` function (72 sub-functions, ~2,500 lines) should be ported in phases:

#### Phase 2D-1: Core expressions (~400 lines)
- `checkExpr` dispatch + cache
- `checkLiteral` (int, float, bool, string, null, undefined)
- `checkIdentifier` (scope lookup)
- `checkBinary` (arithmetic, comparison, logical, concat)
- `checkUnary` (negate, not, address-of)
- `checkParen` (trivial delegation)

#### Phase 2D-2: Control flow + blocks (~300 lines)
- `checkIfExpr` (condition type, branch types, optional capture)
- `checkBlock` (scope push/pop, statement checking)
- `checkSwitchExpr` (enum, union, string switches)

#### Phase 2D-3: Types and access (~400 lines)
- `checkFieldAccess` (struct fields, enum variants, auto-deref, methods)
- `checkIndex` (array, slice, list, map indexing)
- `checkSliceExpr` (slice syntax a[start:end])
- `checkStructInit` (field validation, anonymous structs)
- `checkNewExpr` (heap allocation, constructor sugar)
- `checkArrayLiteral` / `checkTupleLiteral`

#### Phase 2D-4: Calls (~300 lines)
- `checkCall` (arg validation, method resolution, generic dispatch)
- `resolveMethodCall` (type → method lookup)
- `checkBuiltinCall` (~35 builtins, large switch)

#### Phase 2D-5: Error handling + async (~200 lines)
- `checkTryExpr` (error union unwrap)
- `checkCatchExpr` (error union catch handler)
- `checkErrorLiteral` (error.X)
- `checkAwaitExpr` (future unwrap)

#### Phase 2D-6: Statements (~400 lines)
- `checkStmt` dispatch
- `checkReturn` (return type validation)
- `checkVarStmt` / `checkVarDecl` (type inference, var vs const)
- `checkAssign` (mutability, type compatibility)
- `checkIfStmt` / `checkWhileStmt` / `checkForStmt`
- `checkBlockStmt`

#### Phase 2D-7: Type resolution (~300 lines)
- `resolveTypeExpr` (type expression → TypeIndex)
- `resolveType` (named types, generic instances, built-in type names)
- `buildStructType` / `buildEnumType` / `buildUnionType`
- `buildFuncType`

#### Phase 2D-8: Declaration collection (~400 lines)
- `collectTypeDecl` (register struct/enum/union/trait/error_set)
- `collectNonTypeDecl` (register fn/var/impl blocks)
- `checkDecl` (check fn/var/test/bench bodies)
- `registerMethod` (impl block method registration)

### 2E. Deferred: Generic Instantiation

The generic instantiation subsystem (`resolveGenericInstance`, `instantiateGenericFunc`, `instantiateGenericImplMethods`, `buildGenericCacheKey`) is ~300 lines and the most complex part. **Defer to Phase 3** — the self-hosted checker doesn't need to handle generics to be useful. The parser already parses generic syntax; the checker can report "generics not yet supported in self-hosted checker" for now.

### 2F. Deferred: Comptime Evaluation

The comptime subsystem (`evalComptimeValue`, `evalComptimeBlock`, `evalConstExpr`, `evalConstFloat`, `evalConstString`) is ~500 lines. **Defer to Phase 3** — needed for `comptime { }` blocks but not for basic type checking.

### 2G. Deferred: Lint, Suggestions

`runLintChecks`, `editDistance`, `findSimilarName`, `errWithSuggestion` — ~150 lines of UX polish. **Defer to Phase 3.**

### 2H. Tests (~30 tests)

```
test "check literal int"
test "check literal string"
test "check literal bool"
test "check binary arithmetic"
test "check binary comparison"
test "check unary negate"
test "check identifier lookup"
test "check undefined identifier error"
test "check var decl type inference"
test "check const decl"
test "check function decl"
test "check function call"
test "check return type mismatch error"
test "check if expr types"
test "check while condition must be bool"
test "check struct init"
test "check field access"
test "check index array"
test "check slice expr"
test "check optional unwrap"
test "check try expr"
test "check catch expr"
test "check error literal"
test "check switch enum"
test "check block scope"
test "check nested scopes"
test "check method call"
test "check builtin sizeOf"
test "check builtin intCast"
test "check string interp"
```

---

## Phase 3: Integration & Advanced Features (future)

After types.cot + checker.cot are working:
- Generic instantiation (`resolveGenericInstance`, `instantiateGenericFunc`)
- Comptime evaluation (`evalComptimeValue`, `evalConstExpr`)
- Lint mode (`runLintChecks`, `editDistance`, suggestions)
- CLI integration: `cot_self check <file>` command in main.cot

---

## Execution Order

```
1. types.cot Phase 1A: BasicKind enum + TypeTag enum                    (~80 lines)
2. types.cot Phase 1B: TypeRegistry struct with parallel arrays         (~120 lines)
3. types.cot Phase 1C: registryInit() with pre-registered types        (~80 lines)
4. types.cot Phase 1C: Factory methods (makePointer, makeOptional...)  (~100 lines)
5. types.cot Phase 1C: Query methods (sizeOf, typeName, getTag...)     (~100 lines)
6. types.cot Phase 1C: isAssignable + commonType + equal               (~120 lines)
7. types.cot Phase 1D: Tests                                           (~80 lines)
   → CHECKPOINT: `cot test self/main.cot` — all types tests pass

8. checker.cot Phase 2A: Symbol + Scope (flat parallel arrays)         (~100 lines)
9. checker.cot Phase 2B: Checker struct + checkerInit()                (~80 lines)
10. checker.cot Phase 2C: Three-pass checkFile                         (~60 lines)
11. checker.cot Phase 2D-1: Core expression checking                   (~400 lines)
12. checker.cot Phase 2D-6: Statement checking                         (~400 lines)
    → CHECKPOINT: `cot test self/main.cot` — basic check tests pass

13. checker.cot Phase 2D-2: Control flow (if/switch/block)             (~300 lines)
14. checker.cot Phase 2D-3: Type access (fields, index, slice)         (~400 lines)
15. checker.cot Phase 2D-4: Calls + method resolution                  (~300 lines)
16. checker.cot Phase 2D-5: Error handling + async                     (~200 lines)
17. checker.cot Phase 2D-7: Type resolution                            (~300 lines)
18. checker.cot Phase 2D-8: Declaration collection                     (~400 lines)
19. checker.cot Phase 2H: Tests                                        (~200 lines)
    → CHECKPOINT: `cot test self/main.cot` — all checker tests pass

20. main.cot: Add `check` CLI command                                  (~30 lines)
    → CHECKPOINT: `cot build self/main.cot && /tmp/cot_self check self/main.cot`
```

---

## Risk Assessment

| Risk | Mitigation |
|------|-----------|
| `List(struct)` crash | Parallel-array encoding throughout (proven in ast.cot) |
| `Map` untested in self/ | Avoid Map entirely — use parallel List(string)+List(int) with linear scan |
| Large file compilation | types.cot ~700 lines, checker.cot ~3,000 lines — similar to parser.cot (2,647 lines), proven to work |
| Method registry complexity | Flat arrays with linear scan — O(n) but n is small (~100 methods) |
| Scope chain depth | Parent index walk — simple, no heap allocation |
| expr_types cache size | Parallel arrays — O(1) amortized append, O(n) lookup. For self-hosting compiler the n is manageable |

---

## Dependencies

```
types.cot imports:
  std/list
  std/string

checker.cot imports:
  std/list
  std/string
  frontend/types      (TypeRegistry)
  frontend/ast        (Ast, NodeTag)
  frontend/errors     (ErrorReporter)
  frontend/source     (Pos, Span)
  frontend/token      (Token — for keyword checking)
```

Both files are under `self/cot.json` `"safe": true` — all @safe patterns apply.
