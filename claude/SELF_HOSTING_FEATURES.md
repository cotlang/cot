# Self-Hosting Feature Gap: Cot → Wasm Compiler in Cot

**Goal**: Bring Cot to 95% parity with Zig features used in the compiler's Wasm path,
so the Cot→Wasm compiler can be cleanly ported from Zig to Cot without workarounds.

**Scope**: Only features the compiler actually uses. Not "all of Zig".

**Reference**: `compiler/` directory — 116K lines of Zig across ~60 files.

---

## Already Supported (no work needed)

These Zig features have direct Cot equivalents:

| Zig Feature | Cot Equivalent | Usage Count |
|---|---|---|
| Structs + methods (`self: *T`) | Same | 1000+ |
| Tagged unions (`union(enum)`) + switch captures | `union` + switch | 3750+ |
| Optionals `?T` + `if (opt) \|val\|` | Same | 400+ |
| Error unions `!T` + `try`/`catch` | Same | 2000+ |
| `defer` / `errdefer` | Same | 500+ |
| Generics (monomorphization) | Same | Heavy |
| Traits / trait bounds | Same | Type constraints |
| Enums with backing values | Same | 60+ types |
| Labeled blocks / loops | Same | 400+ |
| Slices `[]T` | Same | 500+ |
| `inline for` | Same | 20+ |
| `@hasField`, `@TypeOf`, `@field` | Same | 15+ |
| `@sizeOf`, `@alignOf` | Same | 50+ |
| `@intCast`, `@ptrCast` | Same | 300+ |
| `@intToPtr`, `@ptrToInt` | Same | 20+ |
| String interpolation | `"${expr}"` replaces `std.fmt` | 50+ |
| Function pointers | Same | 20+ |
| Tuples + destructuring | Same | Multi-value returns |
| Comptime blocks + dead branch elim | Same | Target-specific code |
| `for i, item in collection` | Same | 200+ |
| `noreturn` type | Same | Functions that trap |
| Default struct field values | Same (fully wired: parser → checker → lowerer) | 100+ structs |
| `??` nullish coalesce | Same as Zig's `orelse` | 150+ |
| `@trap()` | Same as Zig's `unreachable` | 50+ |
| `@fmin`, `@fmax` | Same (float) | 30+ |

---

## Tier 1: Language Features

These require parser, checker, and/or lowerer changes.

### 1.1 `while (optional) |capture|` ✅ DONE

**What**: Loop that repeatedly unwraps an optional, breaking when null.

**Zig**:
```zig
while (iter.next()) |token| {
    process(token);
}
```

**Cot syntax** (proposed):
```cot
while (iter.next()) |token| {
    process(token)
}
```

**Why**: The scanner, parser, and checker all use this pattern for iteration (~30 uses).
Without it, every iterator loop becomes:
```cot
// Ugly workaround
while (true) {
    var maybe = iter.next()
    if (maybe) |token| {
        process(token)
    } else {
        break
    }
}
```

**Reference**: Zig `while` with optional — `references/zig/lib/std/mem.zig`.

**Changes**: parser.zig (parse optional capture after while condition), checker.zig (unwrap
optional type for capture binding), lower.zig (emit null check + branch + capture load).

**Depends on**: Nothing.

---

### 1.2 `while` Continue Expression ✅ DONE

**What**: Expression evaluated after each loop iteration, before re-checking condition.

**Zig**:
```zig
var i: usize = 0;
while (i < items.len) : (i += 1) {
    process(items[i]);
}
```

**Cot syntax** (proposed):
```cot
var i: i64 = 0
while (i < items.len()) : (i += 1) {
    process(items[i])
}
```

**Why**: Used in low-level loops where `for` range doesn't fit — byte buffer processing,
manual index manipulation, LEB128 encoding loops. ~20 uses in codegen.

**Changes**: ast.zig (add `continue_expr` field to WhileStmt), parser.zig (parse `: (expr)`
after condition), lower.zig (emit continue expression before loop-back branch).

**Depends on**: Nothing.

---

### 1.3 Enum Methods (`impl` on Enums) ✅ DONE

**What**: Methods defined directly on enum types via impl blocks.

**Zig**:
```zig
const Token = enum(u8) {
    kw_fn, kw_if, ident, ...

    pub fn isKeyword(self: Token) bool {
        return @intFromEnum(self) >= @intFromEnum(Token.kw_fn)
            and @intFromEnum(self) <= @intFromEnum(Token.kw_while);
    }

    pub fn precedence(self: Token) u8 {
        return switch (self) {
            .add, .sub => 5,
            .mul, .div => 6,
            else => 0,
        };
    }
};
```

**Cot syntax** (proposed):
```cot
enum Token { kw_fn, kw_if, ident }

impl Token {
    fn isKeyword(self: Token) bool {
        return @intFromEnum(self) >= @intFromEnum(Token.kw_fn)
            and @intFromEnum(self) <= @intFromEnum(Token.kw_while)
    }

    fn precedence(self: Token) i64 {
        return switch self {
            .add, .sub => 5,
            .mul, .div => 6,
            else => 0,
        }
    }
}
```

**Why**: The `Token` enum alone has ~10 methods (`isKeyword`, `precedence`, `toString`,
`isOperator`, etc.). `BasicKind` has `size()`, `name()`, `isInteger()`, `isFloat()`.
`Op` has `info()`, `isCommutative()`. Without this, you'd need 30+ free functions like
`tokenIsKeyword(t)` — messy and un-idiomatic.

**Changes**: checker.zig (allow impl blocks where target type is enum, resolve self as
enum type not pointer), lower.zig (method dispatch for enum receiver — pass value, not
pointer, since enums are integer-sized).

**Depends on**: Nothing.

---

### 1.4 Exhaustive Switch ✅ DONE

**What**: When switching on an enum or tagged union with all cases covered, `else` is
not required. If a case is missing, the compiler emits an error.

**Zig**:
```zig
switch (token) {
    .kw_fn => handleFn(),
    .kw_if => handleIf(),
    .ident => handleIdent(),
    // compile error: missing case .string_lit
}
```

**Cot syntax** (proposed — same as current, just enforce completeness):
```cot
switch token {
    .kw_fn => handleFn(),
    .kw_if => handleIf(),
    .ident => handleIdent(),
    // Error: non-exhaustive switch on enum Token, missing: .string_lit
}
```

When `else =>` is present, exhaustiveness is not checked (opt-out).

**Why**: The compiler has 3750+ switch statements. Exhaustive checking catches bugs at
compile time when new enum variants are added. Without it, a new `Op` variant silently
falls through to `else` and produces wrong code. This is a safety feature, not cosmetic.

**Changes**: checker.zig (when switch target is enum/union type and no `else` arm: collect
all variant names, compare against arm patterns, report missing ones). No parser change.

**Depends on**: Nothing.

---

### 1.5 `unreachable` Keyword ✅ DONE

**What**: Expression that marks code as provably unreachable. Traps at runtime in debug,
undefined behavior in release. Distinct from `@trap()` because it has a type (`noreturn`)
and can be used as an expression.

**Zig**:
```zig
const val = switch (tag) {
    .a => 1,
    .b => 2,
    else => unreachable,
};
```

**Cot syntax** (proposed):
```cot
const val = switch tag {
    .a => 1,
    .b => 2,
    else => unreachable,
}
```

**Why**: Used 50+ times in the compiler. `@trap()` works but reads as "intentional crash"
rather than "this can't happen". `unreachable` is a semantic signal that enables optimizer
assumptions in release mode. Also returns `noreturn` type, which allows it in expression
position (e.g., switch arms, `orelse unreachable`).

**Changes**: token.zig (add keyword), parser.zig (parse as primary expression), checker.zig
(type is `noreturn`), lower.zig (emit trap, or nothing in release mode).

**Depends on**: Nothing (noreturn type already exists).

---

### 1.6 Nested Type Namespaces ✅ DONE

**What**: Declare types inside structs, creating a namespace.

**Zig**:
```zig
const Parser = struct {
    const Error = error{ UnexpectedToken, EndOfInput };
    const State = enum { start, in_expr, done };

    state: State = .start,

    pub fn parse(self: *Parser) Error!*Ast { ... }
};
```

**Cot syntax** (proposed):
```cot
struct Parser {
    const Error = error { UnexpectedToken, EndOfInput }
    const State = enum { start, in_expr, done }

    state: Parser.State

    fn parse(self: *Parser) Parser.Error!*Ast { ... }
}
```

**Why**: The compiler defines many closely related types: `GenState` + `GenError`,
`Checker` + `CheckError` + `CheckState`, `Parser` + `ParseError`. Without namespacing,
these all pollute the top-level scope. The compiler has ~20 error sets and ~15 state enums
that logically belong to their parent struct.

**Changes**: parser.zig (allow const/type declarations inside struct body), checker.zig
(create nested scope for struct namespace, resolve `Struct.Type` dot access), lower.zig
(flatten to top-level with mangled names for codegen).

**Depends on**: Nothing, but interacts with enum methods (1.3).

---

### 1.7 Anonymous Struct Literals ✅ DONE

**What**: Struct values without naming the type, inferred from context.

**Zig**:
```zig
fn addSucc(self: *Self, succ: BlockIndex, args: []const Reg) void { ... }

// At call site:
try self.vcode.addSucc(result.succ, result.args);

// Returning anonymous struct:
return .{ .succ = succ, .args = buffer.items };
```

**Cot syntax** (proposed):
```cot
fn collectBlockCall(self: *Self) struct { succ: BlockIndex, args: []Reg } {
    return .{ .succ = succ, .args = buffer.items }
}
```

**Why**: Used 500+ times for struct initialization where the type is obvious from context.
Especially common for multi-value returns (returning a struct from a function). Without
this, every return site needs `MyReturnStruct { .field = val }` with the full type name.

**Changes**: parser.zig (parse `.{ ... }` as anonymous struct literal), checker.zig
(infer struct type from expected type context — function return, variable type annotation,
parameter type), lower.zig (emit same as named struct init).

**Depends on**: Nothing.

---

### 1.8 Default Struct Field Values ✅ DONE

**What**: Struct fields with default values that are used when the field is omitted
at initialization.

**Zig**:
```zig
const Config = struct {
    debug: bool = false,
    level: u8 = 0,
    name: []const u8,
};

const c = Config{ .name = "test" }; // debug=false, level=0 filled in
```

**Cot**:
```cot
struct Config {
    debug: bool = false
    level: i64 = 0
    name: string
}

var c = Config { .name = "test" }  // debug=false, level=0 filled in
```

**Status**: **Fully implemented.** Parser stores `default_value` on Field, checker validates
missing fields have defaults, lowerer emits default values in all 5 init paths (stack struct,
stack struct expr, new expr ARC, new expr WasmGC, new expr heap). Empty struct init `Config {}`
supported when all fields have defaults.

**Why**: Used on 100+ struct definitions in the compiler. Almost every struct has at least
one field with a default. Without it, every struct init must specify all fields — even
booleans and counts that are almost always zero.

**Changes**: checker.zig (in struct init checking: for each field not provided, check that
default_value exists, type-check default against field type), lower.zig (in struct init
lowering: for each missing field, lower the default_value expression and store it).

**Depends on**: Nothing (AST/parser already done).

---

### 1.9 Packed Structs ✅ DONE

**What**: Structs with no padding between fields, laid out in declaration order with
bit-level precision.

**Zig**:
```zig
const WasmHeader = packed struct {
    magic: u32,     // 4 bytes at offset 0
    version: u32,   // 4 bytes at offset 4
};

const Flags = packed struct {
    readable: bool,   // 1 bit
    writable: bool,   // 1 bit
    _: u6 = 0,        // 6 padding bits
};
```

**Cot syntax** (proposed):
```cot
packed struct WasmHeader {
    magic: u32
    version: u32
}
```

**Why**: Wasm binary format has specific byte layouts. Encoding section headers, type
sections, function bodies all require precise byte-level struct packing. Currently done
with manual pointer arithmetic and byte stores — packed structs make this clean.

**Changes**: parser.zig (parse `packed` modifier before `struct`), ast.zig (add
`is_packed: bool` to StructDecl), checker.zig (compute field offsets with zero padding,
bit-level for bool fields), lower.zig (field access via bit shifts/masks for sub-byte
fields, direct offset for byte-aligned fields).

**Depends on**: Nothing.

---

### 1.10 Extern Structs ✅ DONE

**What**: Structs with C ABI layout, for interop with system calls and binary formats.

**Zig**:
```zig
const sockaddr_in = extern struct {
    sin_family: u16,
    sin_port: u16,
    sin_addr: u32,
    sin_zero: [8]u8,
};
```

**Cot syntax** (proposed):
```cot
extern struct sockaddr_in {
    sin_family: u16
    sin_port: u16
    sin_addr: u32
    sin_zero: [8]u8
}
```

**Why**: Networking code, file I/O structures, and Wasm binary headers all need C-compatible
layout. The HTTP stdlib currently uses raw pointer arithmetic to build sockaddr_in —
extern structs make this type-safe.

**Changes**: parser.zig (parse `extern` modifier before `struct`), ast.zig (add
`is_extern: bool` to StructDecl), checker.zig (compute C ABI field layout — platform
alignment rules), lower.zig (same codegen as regular structs but with C offsets).

**Depends on**: Nothing.

---

## Tier 2: Builtins

New `@builtin` registrations through the standard pipeline:
parser.zig → checker.zig → lower.zig → (some need wasi_runtime.zig + driver.zig)

### 2.1 `@intFromEnum(e)` / `@enumFromInt(T, i)` ✅ DONE

**What**: Convert between enum values and their backing integer.

**Zig**:
```zig
const idx = @intFromEnum(Token.kw_fn);   // → integer
const tok = @enumFromInt(Token, 5);       // → Token value
```

**Cot**:
```cot
const idx = @intFromEnum(Token.kw_fn)
const tok = @enumFromInt(Token, 5)
```

**Why**: Used 50+ times. Token lookup tables, opcode encoding, type registry indexing.
Without this, there's no clean way to map between enum variants and integer indices.

**Implementation**: Compiler intrinsic (no runtime function). @intFromEnum lowers to
the value itself (enums are integers). @enumFromInt is an identity operation with a
type-check that the target is an enum.

---

### 2.2 `@bitCast(T, val)` ✅ DONE

**What**: Reinterpret the bits of a value as a different type without conversion.

**Zig**:
```zig
const bits = @bitCast(u64, my_f64);      // f64 → u64 (same bits)
const float = @bitCast(f64, 0x3FF0...);  // u64 → f64 (same bits)
```

**Cot**:
```cot
const bits = @bitCast(u64, my_f64)
const float = @bitCast(f64, 0x3FF0000000000000)
```

**Why**: Used 30+ times. Wasm encodes f64 constants as their IEEE 754 bit pattern.
Without @bitCast, you can't emit float constants in the Wasm assembler. Also needed
for NaN boxing, hash functions on floats, and serialization.

**Wasm ops**: `f64.reinterpret_i64` (0xBF), `i64.reinterpret_f64` (0xBD),
`f32.reinterpret_i32` (0xBE), `i32.reinterpret_f32` (0xBC).

**Implementation**: Compiler intrinsic → appropriate Wasm reinterpret instruction.

---

### 2.3 `@truncate(T, val)` ✅ DONE

**What**: Explicitly truncate an integer to a narrower type, discarding high bits.

**Zig**:
```zig
const byte: u8 = @truncate(large_value);   // Keep low 8 bits
const word: u16 = @truncate(large_value);   // Keep low 16 bits
```

**Cot**:
```cot
const byte: u8 = @truncate(u8, large_value)
const word: u16 = @truncate(u16, large_value)
```

**Why**: Used 15+ times in Wasm encoding (extracting bytes from larger values).
Currently requires `val & 0xFF` which is error-prone and doesn't convey intent.

**Implementation**: Compiler intrinsic → `i64.and` with appropriate mask
(0xFF for u8, 0xFFFF for u16, 0xFFFFFFFF for u32). Or Wasm `i32.wrap_i64` for
i64 → i32.

---

### 2.4 `@as(T, val)` ✅ DONE

**What**: Explicit type coercion. Asserts that val is coercible to T.

**Zig**:
```zig
const x = @as(u32, @intCast(v.aux_int));
const y = @as(usize, 0);
```

**Cot**:
```cot
const x = @as(u32, @intCast(v.aux_int))
const y = @as(i64, 0)
```

**Why**: Used 200+ times. Makes type intent explicit at call sites, resolves ambiguous
literal types, and documents expected types for maintainability. Without it, type
annotations are the only option: `const y: i64 = 0` (less flexible in expression position).

**Implementation**: Compiler intrinsic → type check + identity (no codegen if types match)
or @intCast-style widening/narrowing as needed.

---

### 2.5 `@offsetOf(T, "field")` ✅ DONE

**What**: Returns the byte offset of a field within a struct.

**Zig**:
```zig
const off = @offsetOf(MyStruct, "data");
```

**Cot**:
```cot
const off = @offsetOf(MyStruct, "data")
```

**Why**: Used 10+ times for manual pointer arithmetic in codegen, especially for
accessing struct fields through raw pointers in the native backend.

**Implementation**: Compiler intrinsic → constant fold at compile time using the struct
layout already computed by the checker.

---

### 2.6 `@min(a, b)` / `@max(a, b)` ✅ DONE

**What**: Integer min/max (complement existing @fmin/@fmax for floats).

**Zig**:
```zig
const result = @min(a, b);
const clamped = @min(@max(val, lo), hi);
```

**Cot**:
```cot
const result = @min(a, b)
const clamped = @min(@max(val, lo), hi)
```

**Why**: Used 30+ times for bounds clamping, buffer size calculations, register allocation.
Currently requires `if (a < b) { a } else { b }` — works but verbose in expression position.

**Implementation**: Compiler intrinsic → `if a < b then a else b` lowered as select or
conditional branch.

---

### 2.7 `@tagName(val)` ✅ DONE

**What**: Get the name of an enum variant or tagged union tag as a string.

**Zig**:
```zig
std.debug.print("op: {s}\n", .{@tagName(node.op)});
```

**Cot**:
```cot
println("op: ${@tagName(node.op)}")
```

**Why**: Used in debug printing, error messages, and diagnostics throughout the compiler.
Without it, every enum needs a manual `toString()` method or a switch-based name function.
The `Op` enum has 100+ variants — maintaining a manual name map is unsustainable.

**Implementation**: Compiler intrinsic → at comptime, generates a string table indexed by
enum backing value. Returns slice into the table. Requires enum to have contiguous backing
values starting from 0.

---

### 2.8 `@errorName(err)` ✅ DONE

**What**: Get the name of an error value as a string.

**Zig**:
```zig
std.debug.print("error: {s}\n", .{@errorName(err)});
```

**Cot**:
```cot
println("error: ${@errorName(err)}")
```

**Why**: Error reporting in the compiler. When a compilation step fails, the error name
appears in diagnostics. Without it, error messages show numeric codes instead of names.

**Implementation**: Same pattern as @tagName — string table indexed by error value.

---

### 2.9 `@intFromBool(b)` ✅ DONE

**What**: Convert bool to integer (false → 0, true → 1).

**Zig**:
```zig
count += @intFromBool(is_valid);
```

**Cot**:
```cot
count += @intFromBool(is_valid)
```

**Why**: Used in codegen for conditional counting, flag accumulation, and bitmask
construction. Without it: `count += if (is_valid) { 1 } else { 0 }` — verbose.

**Implementation**: Compiler intrinsic → identity (bools are already 0/1 at Wasm level).
Type check that input is bool, result is integer.

---

### 2.10 `@alignCast(alignment, ptr)` ✅ DONE

**What**: Assert pointer alignment at runtime (debug) or assume it (release).

**Zig**:
```zig
const aligned = @alignCast(@alignOf(u64), raw_ptr);
```

**Cot**:
```cot
const aligned = @alignCast(8, raw_ptr)
```

**Why**: Used 5+ times when casting between pointer types with different alignments.
Ensures memory safety for typed pointer access.

**Implementation**: Debug mode: emit alignment check (ptr & (align-1) == 0, else trap).
Release mode: no-op. Returns same pointer with adjusted type.

---

### 2.11 `@constCast(ptr)` / `@volatileCast(ptr)` ✅ DONE

**What**: Remove const qualifier from a pointer type.

**Zig**:
```zig
const mutable_ptr = @constCast(const_ptr);
```

**Why**: Used when APIs return const pointers but the caller knows mutation is safe.
~5 uses. Low priority but needed for clean code.

**Implementation**: Type-system only operation (no codegen). Checker adjusts pointer
mutability.

---

## Tier 3: Standard Library

These are Cot stdlib modules/patterns needed for clean compiler code.

**Note**: Allocator interface and arena allocator were removed from this tier.
Cot uses ARC (like Swift) — memory management is automatic. Go and Swift both
compile fast without allocator interfaces. Adding one would be unnecessary complexity.

### 3.1 `std/mem` Functions

**What**: Memory comparison and search utilities.

**Zig**:
```zig
std.mem.eql(u8, a, b)              // byte-level equality
std.mem.indexOf(u8, haystack, needle)  // substring search
std.mem.startsWith(u8, str, prefix)    // prefix check
std.mem.endsWith(u8, str, suffix)      // suffix check
std.mem.zeroes(T)                      // zero-initialized value
```

**Cot**: Most of these already exist in `stdlib/string.cot`:
- `compare(a, b)` → equivalent of `eql`
- `indexOf(s, needle)` → equivalent
- `startsWith(s, prefix)` → equivalent
- `endsWith(s, suffix)` → equivalent

**Gap**: Need byte-level versions that work on `[]u8` slices, not just `string`.
Also need `@memcmp(a, b, len)` builtin for raw pointer comparison.

**Stdlib location**: `stdlib/mem.cot`

**Depends on**: Nothing (can use existing @memcpy pattern).

---

### 3.2 Writer / Reader Interfaces

**What**: Trait-based streaming I/O for composable output.

**Zig**:
```zig
fn emit(writer: anytype) !void {
    try writer.writeAll(&.{ 0x00, 0x61, 0x73, 0x6d });
    try writer.writeInt(u32, 1, .little);
}
```

**Cot design**:
```cot
trait Writer {
    fn write(self: *Self, bytes: []u8) !i64
    fn writeByte(self: *Self, b: u8) !void
    fn writeAll(self: *Self, bytes: []u8) !void
}

// Buffer writer for building Wasm bytecode in memory
struct BufferWriter {
    buf: []u8
    pos: i64
}

impl Writer for BufferWriter {
    fn write(self: *BufferWriter, bytes: []u8) !i64 { ... }
}
```

**Why**: The Wasm assembler writes bytecode to a buffer. Currently uses raw pointer
arithmetic. A Writer trait enables: buffer writer (for in-memory assembly), file writer
(for direct output), counting writer (for size calculation). Composable and testable.

**Stdlib location**: `stdlib/io.cot` (extend existing module)

**Depends on**: Traits (already supported).

---

### 3.3 `std/fmt` — Format Strings

**What**: Type-safe string formatting beyond interpolation.

**Zig**:
```zig
const msg = try std.fmt.allocPrint(allocator, "expected {s}, got {s}", .{expected, got});
const n = std.fmt.bufPrint(&buf, "{d:0>4}", .{value});
```

**Cot approach**: String interpolation (`"${expr}"`) covers 80% of formatting use cases.
The remaining 20% needs:
```cot
import "std/fmt"

// Pad/align
fmt.padLeft("42", 8, '0')   // "00000042"
fmt.padRight("hi", 10, ' ') // "hi        "

// Number formatting
fmt.hex(255)         // "ff"
fmt.binary(42)       // "101010"
fmt.octal(8)         // "10"
```

**Stdlib location**: `stdlib/fmt.cot` (extend existing module with number formatting)

**Depends on**: Nothing (builds on string interpolation + StringBuilder).

---

### 3.4 `std/debug` — Assert and Debug Print

**What**: Debug assertions with messages, conditional debug output.

**Zig**:
```zig
std.debug.assert(idx < len);  // crashes with source location on failure
std.debug.print("value: {}\n", .{val});
```

**Cot design**:
```cot
import "std/debug"

debug.assert(idx < len, "index out of bounds")
debug.print("value: ${val}")
```

**Why**: The compiler has 50+ assertions. Currently uses `@assert` which traps without
a message. Debug assertions with messages make failures diagnosable.

**Gap**: `@assert` exists but doesn't support messages. Need `@assert(cond, "message")`
or a stdlib wrapper.

**Stdlib location**: `stdlib/debug.cot`

**Depends on**: Nothing.

---

## Tier 4: Type System Enhancements

These are deeper type system changes that improve correctness and ergonomics.

### 4.1 Integer Type Promotion Rules

**What**: Define clear rules for mixed-width arithmetic and implicit promotion.

**Zig rules**:
- Arithmetic on same-width types preserves width: `u8 + u8 → u8`
- Mixed widths: narrower promotes to wider: `u8 + u32 → u32`
- Untyped integer literals adopt the type of the other operand
- Overflow is undefined (debug: trap, release: wrap)

**Cot current**: Type is preserved (`u8 + u8 → u8`), untyped materializes to i64.

**Gap**: Mixed-width promotion rules are unclear. `u8 + i32` — what happens? Need to
define and implement consistent promotion rules matching Zig's semantics.

**Changes**: checker.zig (add `commonType(a, b)` that returns the wider of two integer
types, respecting signedness).

---

### 4.2 Overflow Detection (Debug Mode)

**What**: In debug mode, integer overflow traps. In release mode, wraps.

**Zig**:
```zig
var x: u8 = 200;
x += 100; // debug: trap (overflow), release: wraps to 44
```

**Why**: Catches silent data corruption bugs. The compiler operates on indices, sizes, and
offsets — overflow in any of these produces wrong code silently. Matches the existing
`--release` flag (1.5 already adds bounds checks in debug mode).

**Changes**: lower.zig (after narrow-type arithmetic, emit overflow check in debug mode:
compare result against type min/max, trap if out of range). Release mode: emit
`@truncate`-style masking.

**Depends on**: `@truncate` (2.3), `--release` flag (already exists).

---

## Implementation Order

Dependencies determine order. Independent features can be parallelized.

### Phase 1: Foundation (no dependencies)
All independent, can be implemented in any order:
- 2.1 `@intFromEnum` / `@enumFromInt`
- 2.2 `@bitCast`
- 2.3 `@truncate`
- 2.4 `@as`
- 2.5 `@offsetOf`
- 2.6 `@min` / `@max`
- 2.7 `@tagName`
- 2.8 `@errorName`
- 2.9 `@intFromBool`
- 2.10 `@alignCast`
- 1.5 `unreachable` keyword

### Phase 2: Language Features
- 1.1 `while (optional) |capture|`
- 1.2 `while` continue expression
- 1.3 Enum methods
- 1.4 Exhaustive switch
- 1.6 Nested type namespaces
- 1.7 Anonymous struct literals
- 1.8 Default struct field values (complete checker/lowerer wiring)

### Phase 3: Advanced Structs
- 1.9 Packed structs
- 1.10 Extern structs

### Phase 4: Standard Library
- 3.1 `std/mem` functions
- 3.2 Writer/Reader interfaces
- 3.3 `std/fmt` number formatting
- 3.4 `std/debug` assertions

### Phase 5: Type System Polish
- 4.1 Integer promotion rules
- 4.2 Overflow detection

### Phase 0 (DONE): ARC Critical Gaps

**All 4 critical ARC gaps fixed (Feb 18, 2026).**
See `claude/ARC_AUDIT.md` for full audit details.

1. **Ownership formalization** (DONE) — `lowerAssign` now checks `is_owned` before retaining, matching Swift SILGenAssign pattern. Deref assignment retains +0 sources.
2. **Widen ARC detection** (DONE) — `couldBeARC()` now handles `*EnumType`, `*UnionType`, `?*T` (optional wrapping ARC pointer).
3. **Collection element ARC** (DONE) — Added `@arc_retain`/`@arc_release` conditional builtins (no-op for non-ARC types, resolved at monomorphization). Updated List.free/set/clear/deleteRange/compact/removeIf and Map.free/set/clear/delete.
4. **Weak references** (DONE) — `weak var x = expr` keyword suppresses ARC retain/cleanup. Breaks reference cycles. Tests: `test/e2e/arc.cot` (15 tests).

---

## Verification

After each phase, port a section of the compiler to Cot as a proof-of-concept:

| Phase | Proof-of-Concept |
|-------|-----------------|
| Phase 1 | Port `token.zig` — enum with methods, @intFromEnum, @tagName |
| Phase 2 | Port `scanner.zig` — while-capture, switch, enum methods |
| Phase 3 | Port `wasm/assemble.zig` — packed structs, byte-level I/O |
| Phase 4 | Port `parser.zig` — arena allocator, writer interface |
| Phase 5 | Port `checker.zig` — full type system in use |

---

## Estimated Scope

| Category | Features | Estimated Compiler Changes |
|----------|----------|---------------------------|
| Tier 1: Language | 10 features | ~2200 lines across parser/checker/lower | ✅ DONE |
| Tier 2: Builtins | 11 features | ~800 lines across ast/checker/lower | ✅ DONE |
| Tier 3: Stdlib | 4 modules | ~1000 lines of Cot stdlib code | ✅ DONE |
| Tier 4: Type System | 2 features | ~400 lines in checker/lower | ✅ DONE |
| **Total** | **27 features** | **~4400 lines** |

This brings Cot from ~75% to ~95% Zig feature parity for the compiler's needs,
enabling a clean self-hosted Cot→Wasm compiler without workarounds.
