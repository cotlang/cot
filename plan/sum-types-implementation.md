# Sum Types (Tagged Unions) Implementation Plan

**Status:** PLANNED
**Priority:** HIGH - Critical for idiomatic Cot
**Created:** 2026-01-08

## Overview

Sum types (also called tagged unions, discriminated unions, or algebraic data types) allow enum variants to carry associated data. This is a fundamental feature for type-safe, expressive code.

### Current State

```cot
// Current: Simple enums (no payloads)
enum Color { Red, Green, Blue }
```

### Target State

```cot
// Goal: Enums with payloads
enum Option<T> {
    None,
    Some(T),
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}

enum Expr {
    IntLit(i64),
    BinaryOp { op: TokenType, left: *Expr, right: *Expr },
    Call { callee: *Expr, args: []*Expr },
}
```

---

## Syntax Design

### Variant Syntax Options

**Option A: Rust-style (Recommended)**
```cot
enum Message {
    Quit,                              // No payload
    Move { x: i64, y: i64 },           // Named fields (struct-like)
    Write(string),                     // Tuple-like (single value)
    ChangeColor(i64, i64, i64),        // Tuple-like (multiple values)
}
```

**Option B: Swift-style**
```cot
enum Message {
    case quit
    case move(x: i64, y: i64)
    case write(string)
}
```

**Recommendation:** Option A (Rust-style) - aligns with existing Cot struct syntax and is familiar to Rust/Zig developers.

### Pattern Matching Syntax

```cot
switch (msg) {
    Message.Quit => { println("quit") }
    Message.Move { x, y } => { println("move to ${x}, ${y}") }  // Destructure named
    Message.Write(text) => { println(text) }                    // Destructure tuple
    Message.ChangeColor(r, g, b) => { setColor(r, g, b) }
}

// Or with binding syntax for single values
switch (option) {
    Option.None => { return -1 }
    Option.Some(value) => { return value }
}
```

### Construction Syntax

```cot
var msg = Message.Quit                           // No payload
var msg = Message.Move{ .x = 10, .y = 20 }      // Named fields
var msg = Message.Write("hello")                 // Tuple-like
var opt: Option<i64> = Option.Some(42)          // Generic
var opt: Option<i64> = Option.None              // Generic (no payload)
```

---

## Implementation Phases

### Phase 1: Lexer Changes (Minimal)

**Files:** `src/lexer/lexer.zig`

No new tokens needed - existing tokens are sufficient:
- `(` `)` for tuple-like variants
- `{` `}` for struct-like variants
- `,` for separating fields/values

**Tasks:**
- [ ] Verify all necessary tokens exist (they do)
- [ ] No changes required

---

### Phase 2: AST Changes

**Files:**
- `src/ast/statements.zig`
- `src/ast/node_store.zig`
- `src/ast/views.zig`

#### 2.1 Enum Variant Representation

Add new types to represent variant payloads:

```zig
// In statements.zig or a new sum_types.zig

/// Kind of enum variant payload
pub const VariantPayloadKind = enum(u2) {
    none,        // No payload: Quit
    tuple,       // Tuple-like: Write(string)
    struct_like, // Named fields: Move { x: i64, y: i64 }
};
```

#### 2.2 Extra Data Format

Current enum format:
```
extra_data: [count, name1, value1, name2, value2, ...]
```

New format with payloads:
```
extra_data: [
    variant_count,
    // For each variant:
    variant_name: StringId,
    variant_value: u32,        // Discriminant
    payload_kind: u8,          // VariantPayloadKind
    payload_field_count: u8,   // Number of fields (0 for none)
    // For each payload field (if any):
    field_name: StringId,      // 0 for tuple-like (positional)
    field_type: TypeIdx,
]
```

**Tasks:**
- [ ] Define `VariantPayloadKind` enum
- [ ] Add helper methods to `NodeStore` for reading/writing variant data
- [ ] Add `EnumVariantView` in `views.zig` for easy access to variant info
- [ ] Update `emitter.zig` to emit new format

---

### Phase 3: Parser Changes

**Files:** `src/parser/parser.zig`

#### 3.1 Update `parseEnumDef`

```zig
fn parseEnumDef(self: *Self) ParseError!StmtIdx {
    // ... existing name parsing ...

    while (!self.check(.rbrace) and !self.isAtEnd()) {
        const variant_token = try self.consume(.identifier, "Expected variant name");
        const variant_name = self.internString(variant_token.lexeme) catch return error.OutOfMemory;

        // NEW: Check for payload
        const payload = try self.parseVariantPayload();

        // Store variant info
        self.store.pushScratchU32(@intFromEnum(variant_name)) catch return error.OutOfMemory;
        self.store.pushScratchU32(payload.kind) catch return error.OutOfMemory;
        self.store.pushScratchU32(payload.fields_start) catch return error.OutOfMemory;
        self.store.pushScratchU32(payload.field_count) catch return error.OutOfMemory;

        _ = self.match(&[_]TokenType{.comma});
    }
    // ...
}

fn parseVariantPayload(self: *Self) ParseError!VariantPayload {
    if (self.match(&[_]TokenType{.lparen})) {
        // Tuple-like: Variant(T1, T2, ...)
        return self.parseTuplePayload();
    } else if (self.match(&[_]TokenType{.lbrace})) {
        // Struct-like: Variant { field: Type, ... }
        return self.parseStructPayload();
    }
    // No payload
    return .{ .kind = .none, .fields_start = 0, .field_count = 0 };
}
```

**Tasks:**
- [ ] Add `parseVariantPayload` function
- [ ] Add `parseTuplePayload` function (parse comma-separated types)
- [ ] Add `parseStructPayload` function (parse name: Type pairs)
- [ ] Update `parseEnumDef` to call payload parsing
- [ ] Update extra_data writing to include payload info
- [ ] Add tests for each payload style

---

### Phase 4: Type Checker Changes

**Files:**
- `src/compiler/type_checker.zig`
- `src/compiler/types.zig`

#### 4.1 Enum Type Representation

```zig
// In types.zig
pub const EnumVariantInfo = struct {
    name: StringId,
    discriminant: u32,
    payload_kind: VariantPayloadKind,
    payload_types: []const TypeId,  // Field types (in order)
    payload_names: []const StringId, // Field names (for struct-like; empty for tuple)
};

pub const EnumTypeInfo = struct {
    name: StringId,
    type_params: []const StringId,  // For generics
    variants: []const EnumVariantInfo,
};
```

#### 4.2 Type Checking Rules

1. **Variant Construction:**
   - `Message.Quit` - valid only if variant has no payload
   - `Message.Write("hello")` - check argument types match payload
   - `Message.Move{ .x = 1, .y = 2 }` - check all fields present and correct types

2. **Pattern Matching:**
   - Binding variables get types from variant payload
   - Exhaustiveness checking (all variants covered or else clause)

3. **Generic Instantiation:**
   - `Option<i64>.Some(42)` - instantiate type parameter in payload

**Tasks:**
- [ ] Add `EnumVariantInfo` and `EnumTypeInfo` structs
- [ ] Update `resolveEnumType` to parse variant payloads
- [ ] Add `checkEnumConstruction` for variant instantiation
- [ ] Add `checkSwitchPattern` for pattern matching with bindings
- [ ] Add exhaustiveness checking for switch on sum types
- [ ] Handle generic enum instantiation

---

### Phase 5: IR Changes

**Files:**
- `src/ir/ir.zig`
- `src/ir/lower.zig`
- `src/ir/lower_expr.zig`

#### 5.1 New IR Operations

```zig
// In ir.zig
pub const IROp = enum {
    // ... existing ops ...

    // Sum type operations
    variant_create,    // Create a variant value: (tag, payload...) -> variant
    variant_tag,       // Extract discriminant: variant -> i64
    variant_payload,   // Extract payload: (variant, expected_tag) -> payload (may panic)
    variant_is,        // Check variant tag: (variant, tag) -> bool
};
```

#### 5.2 Memory Layout

Option 1: **Tagged pointer** (if payload fits in pointer)
```
| 3-bit tag | 61-bit payload or pointer |
```

Option 2: **Heap-allocated struct** (general case)
```
Heap allocation: | tag: i64 | payload bytes... |
Value on stack: pointer to heap allocation
```

Option 3: **Inline for small payloads**
```
Stack value: | tag: u8 | padding | payload (up to 7 bytes) |
```

**Recommendation:** Start with Option 2 (heap-allocated) for simplicity, optimize later.

**Tasks:**
- [ ] Add `variant_create`, `variant_tag`, `variant_payload`, `variant_is` IR ops
- [ ] Implement `lowerVariantCreate` in lower_expr.zig
- [ ] Implement `lowerSwitchWithBinding` for destructuring patterns
- [ ] Handle ARC for variant values with heap payloads

---

### Phase 6: Bytecode Emission

**Files:**
- `src/ir/emit_bytecode.zig`
- `src/runtime/bytecode/opcodes.zig`

#### 6.1 New Opcodes

```zig
// In opcodes.zig
pub const Opcode = enum(u8) {
    // ... existing opcodes ...

    // Sum type operations
    OP_VARIANT_CREATE = 0xE0,  // Create variant: [tag, payload_reg_count, regs...] -> variant
    OP_VARIANT_TAG = 0xE1,     // Get tag: [variant_reg] -> tag (i64)
    OP_VARIANT_PAYLOAD = 0xE2, // Get payload field: [variant_reg, field_idx] -> value
    OP_VARIANT_IS = 0xE3,      // Check tag: [variant_reg, expected_tag] -> bool
};
```

**Tasks:**
- [ ] Add opcodes to `opcodes.zig`
- [ ] Add emission for each IR op in `emit_bytecode.zig`
- [ ] Handle variant payload register allocation

---

### Phase 7: VM Runtime

**Files:**
- `src/runtime/bytecode/vm.zig`
- `src/runtime/bytecode/vm_opcodes.zig`
- `src/runtime/bytecode/value.zig`

#### 7.1 Runtime Representation

```zig
// In value.zig
pub const VariantValue = struct {
    tag: u32,           // Discriminant
    enum_type_id: u32,  // Which enum type this is
    payload: ?*anyopaque, // Pointer to heap-allocated payload (null if none)
    payload_size: u32,  // Size of payload in bytes
};

// Add to Value union
pub const Value = union(enum) {
    // ... existing types ...
    variant: *VariantValue,
};
```

#### 7.2 Opcode Handlers

```zig
// In vm_opcodes.zig
fn opVariantCreate(vm: *VM) !void {
    const tag = vm.readU32();
    const field_count = vm.readU8();

    // Allocate variant
    const variant = try vm.allocator.create(VariantValue);
    variant.tag = tag;

    if (field_count > 0) {
        // Allocate and populate payload
        const payload = try vm.allocatePayload(field_count);
        for (0..field_count) |i| {
            const reg = vm.readU8();
            payload[i] = vm.getRegister(reg);
        }
        variant.payload = payload;
    }

    vm.push(.{ .variant = variant });
}

fn opVariantTag(vm: *VM) !void {
    const variant = vm.pop().variant;
    vm.push(.{ .int = @intCast(variant.tag) });
}

fn opVariantPayload(vm: *VM) !void {
    const variant = vm.pop().variant;
    const field_idx = vm.readU8();
    const expected_tag = vm.readU32();

    if (variant.tag != expected_tag) {
        return error.VariantTagMismatch; // Or panic
    }

    const payload = @as([*]Value, @ptrCast(variant.payload.?));
    vm.push(payload[field_idx]);
}
```

**Tasks:**
- [ ] Add `VariantValue` struct to value.zig
- [ ] Add variant to `Value` union
- [ ] Implement `opVariantCreate` handler
- [ ] Implement `opVariantTag` handler
- [ ] Implement `opVariantPayload` handler
- [ ] Implement `opVariantIs` handler
- [ ] Add ARC support for variant values (ref counting payload)
- [ ] Handle variant in cycle collector

---

### Phase 8: Pattern Matching Enhancement

**Files:**
- `src/parser/parser.zig`
- `src/ir/lower.zig`

#### 8.1 Switch Arm Pattern Parsing

```cot
switch (expr) {
    Option.Some(x) => { use(x) }      // Bind x to payload
    Option.Some(42) => { specific() }  // Match specific value
    Option.None => { default() }
    Message.Move { x, y } => { }       // Destructure struct-like
    Message.Move { x: a, y: b } => { } // Rename bindings
    _ => { }                            // Wildcard
}
```

**Tasks:**
- [ ] Parse binding patterns in switch arms: `Variant(binding_name)`
- [ ] Parse destructuring patterns: `Variant { field: binding }`
- [ ] Parse nested patterns (future): `Some(Some(x))`
- [ ] Lower patterns to variant_is + variant_payload ops
- [ ] Add pattern variables to scope in arm body

---

### Phase 9: Self-Hosted Compiler Updates

**Files:** `~/cotlang/cot-compiler/src/*.cot`

Once sum types work in the Zig compiler, update the self-hosted compiler:

**Tasks:**
- [ ] Refactor `Expr` from flat struct to sum type
- [ ] Refactor `Stmt` from flat struct to sum type
- [ ] Refactor `TypeRef` from flat struct to sum type
- [ ] Update all switch statements to use destructuring
- [ ] Verify self-hosted compiler still compiles

---

### Phase 10: Standard Library Integration

**Tasks:**
- [ ] Add `Option<T>` to prelude
- [ ] Add `Result<T, E>` to prelude
- [ ] Add `.map()`, `.unwrap()`, `.unwrap_or()` methods to Option/Result
- [ ] Add `?` operator for Option/Result propagation (future)

---

## Testing Strategy

### Unit Tests

1. **Parser tests:**
   - Parse tuple-like variants
   - Parse struct-like variants
   - Parse mixed variants
   - Parse generic enums with payloads

2. **Type checker tests:**
   - Variant construction type checking
   - Pattern match exhaustiveness
   - Generic instantiation
   - Payload type inference

3. **Codegen tests:**
   - Variant creation
   - Tag extraction
   - Payload extraction
   - Pattern match code generation

### Integration Tests

```cot
// test_sum_types.cot

enum Option<T> {
    None,
    Some(T),
}

enum Shape {
    Circle(f64),                         // radius
    Rectangle { width: f64, height: f64 },
}

fn area(shape: Shape) f64 {
    switch (shape) {
        Shape.Circle(r) => { return 3.14159 * r * r }
        Shape.Rectangle { width, height } => { return width * height }
    }
}

test "option some" {
    var opt: Option<i64> = Option.Some(42)
    switch (opt) {
        Option.Some(x) => { assert(x == 42) }
        Option.None => { fail() }
    }
}

test "option none" {
    var opt: Option<i64> = Option.None
    switch (opt) {
        Option.Some(_) => { fail() }
        Option.None => { pass() }
    }
}

test "shape area" {
    var circle = Shape.Circle(10.0)
    var rect = Shape.Rectangle{ .width = 5.0, .height = 3.0 }

    assert(area(circle) > 314.0)
    assert(area(rect) == 15.0)
}
```

---

## Task Checklist

### Phase 1: Lexer (0 tasks - no changes needed)
- [x] Verify tokens exist

### Phase 2: AST (5 tasks)
- [ ] 2.1 Define `VariantPayloadKind` enum
- [ ] 2.2 Add `EnumVariantView` to views.zig
- [ ] 2.3 Add NodeStore helpers for variant data
- [ ] 2.4 Update emitter.zig for new format
- [ ] 2.5 Add tests for AST variant representation

### Phase 3: Parser (6 tasks)
- [ ] 3.1 Add `parseVariantPayload` function
- [ ] 3.2 Add `parseTuplePayload` function
- [ ] 3.3 Add `parseStructPayload` function
- [ ] 3.4 Update `parseEnumDef` for payloads
- [ ] 3.5 Parse switch patterns with bindings
- [ ] 3.6 Add parser tests for variant syntax

### Phase 4: Type Checker (7 tasks)
- [ ] 4.1 Add `EnumVariantInfo` struct
- [ ] 4.2 Update `resolveEnumType` for payloads
- [ ] 4.3 Implement `checkEnumConstruction`
- [ ] 4.4 Implement `checkSwitchPattern`
- [ ] 4.5 Add exhaustiveness checking
- [ ] 4.6 Handle generic enum instantiation
- [ ] 4.7 Add type checker tests

### Phase 5: IR (5 tasks)
- [ ] 5.1 Add variant IR operations
- [ ] 5.2 Implement `lowerVariantCreate`
- [ ] 5.3 Implement `lowerVariantAccess`
- [ ] 5.4 Implement `lowerSwitchWithBinding`
- [ ] 5.5 Add IR tests

### Phase 6: Bytecode (4 tasks)
- [ ] 6.1 Add OP_VARIANT_* opcodes
- [ ] 6.2 Implement emission for variant ops
- [ ] 6.3 Handle payload register allocation
- [ ] 6.4 Add bytecode tests

### Phase 7: VM (7 tasks)
- [ ] 7.1 Add `VariantValue` struct
- [ ] 7.2 Implement `opVariantCreate`
- [ ] 7.3 Implement `opVariantTag`
- [ ] 7.4 Implement `opVariantPayload`
- [ ] 7.5 Implement `opVariantIs`
- [ ] 7.6 Add ARC support for variants
- [ ] 7.7 Handle variants in cycle collector

### Phase 8: Pattern Matching (4 tasks)
- [ ] 8.1 Parse binding patterns
- [ ] 8.2 Parse destructuring patterns
- [ ] 8.3 Lower patterns to IR
- [ ] 8.4 Add pattern binding to scope

### Phase 9: Self-Hosted Compiler (5 tasks)
- [ ] 9.1 Refactor `Expr` to sum type
- [ ] 9.2 Refactor `Stmt` to sum type
- [ ] 9.3 Refactor `TypeRef` to sum type
- [ ] 9.4 Update all switch statements
- [ ] 9.5 Verify compilation

### Phase 10: Standard Library (4 tasks)
- [ ] 10.1 Add `Option<T>` to prelude
- [ ] 10.2 Add `Result<T, E>` to prelude
- [ ] 10.3 Add Option/Result methods
- [ ] 10.4 Documentation

---

## Total: 47 tasks

## Dependencies

```
Phase 1 (Lexer) ─────────────────────────────────────────────────────┐
                                                                      │
Phase 2 (AST) ──┬─► Phase 3 (Parser) ──┬─► Phase 4 (Type Checker) ──┤
                │                       │                             │
                │                       └─► Phase 8 (Patterns) ───────┤
                │                                                      │
                └─► Phase 5 (IR) ──────────► Phase 6 (Bytecode) ──────┤
                                                                       │
                                           Phase 7 (VM) ───────────────┤
                                                                       │
                                           Phase 9 (Self-Hosted) ◄─────┘
                                                                       │
                                           Phase 10 (stdlib) ◄─────────┘
```

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Complex memory layout | HIGH | Start with simple heap-allocated design |
| Pattern matching complexity | MEDIUM | Support only simple patterns initially |
| Generic instantiation | MEDIUM | Leverage existing generic infrastructure |
| ARC for nested payloads | MEDIUM | Track all heap pointers in variant |
| Self-hosted refactor | LOW | Can be done incrementally |

---

## Success Criteria

1. **Syntax works:** Parse and compile enum variants with payloads
2. **Type safety:** Compiler rejects invalid variant construction/access
3. **Pattern matching:** Destructure variants in switch statements
4. **Exhaustiveness:** Compiler warns on non-exhaustive switches
5. **Runtime correct:** Variants work correctly at runtime
6. **Self-hosted uses it:** cot-compiler uses sum types for AST
7. **Option/Result in prelude:** Standard sum types available

---

## Open Questions

1. **Nesting depth:** Should we support `Some(Some(x))` patterns initially?
   - Recommendation: No, add in Phase 2

2. **Irrefutable patterns:** Should `let Some(x) = opt` be allowed?
   - Recommendation: No, require switch for safety

3. **if-let syntax:** Add `if let Some(x) = opt { }` sugar?
   - Recommendation: Yes, after core implementation

4. **Payload optimization:** Inline small payloads in NaN-boxed value?
   - Recommendation: Future optimization, not for v1
