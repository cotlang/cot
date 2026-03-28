# Zig Language Parity Tracker

**Date:** 2026-03-28
**Goal:** 100% coverage of Zig's language features in Cot before self-hosting begins.
**Rationale:** The Zig libraries (libcot-zig, libcir-zig, libclif-zig) must be portable to Cot line-by-line. Any Zig feature used in the compiler that Cot doesn't support becomes a blocker during the self-hosted port. Close every gap FIRST, then port.

This is the same strategy as Deno/Bun covering Node.js — you don't start building until the foundation covers the target.

---

## Status Key

- **Y** — Works, tested
- **P** — Partial (some cases work, others don't)
- **N** — Not implemented
- **—** — Not applicable / intentionally different in Cot

---

## Comptime Features

| Feature | Zig Syntax | Cot Status | Notes |
|---------|-----------|------------|-------|
| Comptime block (implicit return) | `comptime { expr }` | **Y** | Last expression is return value |
| Comptime block (labeled break) | `comptime blk: { break :blk val; }` | **N** | Parser rejects `comptime blk:` — needs parser fix |
| Comptime int arithmetic | `comptime { 40 + 2 }` | **Y** | |
| Comptime variables | `comptime { var x = 10; x += 5; x }` | **Y** | |
| Typed array + undefined init | `var s: [3]i64 = undefined; s[0] = 10;` | **Y** | Works in comptime blocks |
| Array literal init in comptime | `var s = [_]i64{0, 0, 0};` | **N** | See COMPTIME_MAP_PLAN.md Step 0 |
| Typed array + init list | `var s: [3]i64 = .{0, 0, 0};` | **N** | See COMPTIME_MAP_PLAN.md Step 0 |
| Comptime array element assign | `s[i] = val` (comptime i) | **Y** | |
| Inline for over range | `inline for (0..5) \|i\|` | **Y** | |
| Inline for over comptime array | `inline for (fields) \|f\|` | **Y** | |
| `@typeInfo(T).fields` | `std.meta.fields(T)` | **Y** | Returns comptime array of field info |
| `@intFromEnum(val)` | `@intFromEnum(val)` | **Y** | |
| `@enumFromInt(n)` | `@enumFromInt(n)` | **Y** | |
| `@enumLen(T)` | — (Cot-specific) | **Y** | Zig uses `std.meta.fields(T).len` |
| `@hasField(T, name)` | `@hasField(T, name)` | **Y** | |
| `@field(obj, name)` | `@field(obj, name)` | **Y** | |
| `@embedFile(path)` | `@embedFile(path)` | **Y** | Reads file at compile time, returns string |
| StaticStringMap (comptime hash map) | `std.StaticStringMap(T).initComptime(...)` | **N** | See COMPTIME_MAP_PLAN.md Steps 1-4 |
| Comptime map creation | — | **N** | Planned: `Map(K,V)` in comptime blocks |
| Comptime map .set/.get | — | **N** | Planned: method calls on comptime maps |
| Comptime map iteration | — | **N** | Planned: `inline for (k, v) in map` |
| Comptime function evaluation | `fn foo() comptime { ... }` | **N** | Not yet — comptime only in blocks |
| `@compileError(msg)` | `@compileError(msg)` | **N** | Emit custom compile-time error |
| `@compileLog(...)` | `@compileLog(...)` | **N** | Debug print at compile time |

---

## Type System

| Feature | Zig | Cot Status | Notes |
|---------|-----|------------|-------|
| Integer types (i8-i64, u8-u64) | `i32`, `u64` | **Y** | |
| Float types (f32, f64) | `f32`, `f64` | **Y** | |
| Bool | `bool` | **Y** | |
| Void | `void` | **Y** | |
| Noreturn | `noreturn` | **Y** | |
| Optional | `?T` | **Y** | |
| Error union | `E!T` | **Y** | |
| Error set | `error{A, B, C}` | **Y** | |
| Pointer | `*T` | **Y** | |
| Slice | `[]T` | **Y** | |
| Array | `[N]T` | **Y** | |
| Struct | `struct { ... }` | **Y** | |
| Enum | `enum { ... }` | **Y** | With backing type `enum(u8)` |
| Union (tagged) | `union(enum) { ... }` | **Y** | |
| Tuple | `struct { f1: T1, f2: T2 }` | **Y** | Cot uses `(T1, T2)` syntax |
| Function type | `fn(i64) i64` | **Y** | |
| Distinct type | — (Cot-specific) | **Y** | `type X = distinct T` |
| Existential | — (Cot-specific) | **Y** | `any Trait` |
| Managed pointer | — (Cot-specific) | **Y** | ARC-managed `*T` |

---

## Control Flow

| Feature | Zig | Cot Status | Notes |
|---------|-----|------------|-------|
| if/else | `if (cond) x else y` | **Y** | |
| while | `while (cond) { ... }` | **Y** | |
| for (range) | `for (0..10) \|i\|` | **Y** | Cot: `for i in 0..10` |
| for (slice) | `for (slice) \|item\|` | **Y** | |
| switch | `switch (val) { ... }` | **Y** | |
| break (labeled) | `break :label val` | **Y** | |
| continue | `continue` | **Y** | |
| return | `return val` | **Y** | |
| defer | `defer expr` | **Y** | |
| errdefer | `errdefer expr` | **Y** | |
| try | `try expr` | **Y** | |
| catch | `expr catch \|e\| ...` | **Y** | |
| orelse | `expr orelse default` | **Y** | |
| Labeled blocks | `blk: { break :blk val }` | **Y** | Works at runtime |
| Unreachable | `unreachable` | **Y** | `@unreachable` / `@trap()` |

---

## Functions

| Feature | Zig | Cot Status | Notes |
|---------|-----|------------|-------|
| Regular functions | `fn add(a: i64, b: i64) i64` | **Y** | |
| Methods (explicit self) | `fn method(self: *T) void` | **Y** | Also @safe auto-inject |
| Static methods | `fn static() void` | **Y** | |
| Generic functions | `fn max(T)(a: T, b: T) T` | **Y** | |
| Closures | `\|x\| x + 1` | **Y** | Cot: `{ x in x + 1 }` or `fn(x) { x + 1 }` |
| Function pointers | `*const fn(i64) i64` | **Y** | |
| Async functions | `async fn fetch()` | **Y** | Swift-style, not Zig-style |
| Extern functions | `extern fn write(...)` | **Y** | |
| Export functions | `export fn main()` | **Y** | |
| Inline | `inline fn ...` | **P** | `@inlinable` for generics, not general inline |
| Test functions | `test "name" { ... }` | **Y** | |

---

## Memory / Builtins

| Feature | Zig | Cot Status | Notes |
|---------|-----|------------|-------|
| `@sizeOf(T)` | `@sizeOf(T)` | **Y** | |
| `@alignOf(T)` | `@alignOf(T)` | **Y** | |
| `@intToPtr(*T, addr)` | `@intToPtr(*T, addr)` | **Y** | |
| `@ptrToInt(ptr)` | `@ptrToInt(ptr)` | **Y** | Called `@ptrOf` in Cot |
| `@intCast(val)` | `@intCast(val)` | **Y** | |
| `@floatCast(val)` | `@floatCast(val)` | **Y** | |
| `@bitCast(val)` | — | **N** | Not yet needed |
| `@memcpy(dst, src)` | — | **P** | Via runtime function, not builtin |
| `@memset(dst, val, len)` | — | **P** | Via runtime function |
| `@panic(msg)` | `@panic(msg)` | **Y** | |
| `@assert(cond)` | — (Cot-specific) | **Y** | Zig uses `std.debug.assert` |
| `@assertEq(a, b)` | — (Cot-specific) | **Y** | |
| `@trap()` | — (Cot-specific) | **Y** | Like Zig's `@trap()` |

---

## Declarations

| Feature | Zig | Cot Status | Notes |
|---------|-----|------------|-------|
| `const` | `const x = 42` | **Y** | |
| `var` | `var x: i64 = 0` | **Y** | |
| `let` | — (Cot-specific) | **Y** | Alias for const |
| Struct with methods | `const S = struct { fn m() {} }` | **Y** | |
| Impl blocks | — (Cot-specific, Rust) | **Y** | `impl T { ... }` |
| Trait/protocol | — (Cot-specific, Rust) | **Y** | `trait T { ... }` |
| Import | `@import("file.zig")` | **Y** | Cot: `import "std/list"` |

---

## Operators

| Feature | Zig | Cot Status | Notes |
|---------|-----|------------|-------|
| Arithmetic (+, -, *, /, %) | All | **Y** | |
| Bitwise (&, \|, ^, ~, <<, >>) | All | **Y** | |
| Comparison (==, !=, <, <=, >, >=) | All | **Y** | |
| Logical (&&, \|\|, !) | All | **Y** | Also `and`, `or`, `not` keywords |
| Compound assign (+=, -=, etc.) | All | **Y** | |
| Concatenation (++) | `++` | **Y** | |
| Optional unwrap (.?) | `.?` | **Y** | |
| Deref (.*) | `.*` | **Y** | |
| Optional chain (?.) | — (Cot-specific) | **Y** | |
| Range (..) | — (Cot-specific) | **Y** | |
| Arrow (->) | — | **Y** | Return type annotation |
| Fat arrow (=>) | — | **Y** | Match arms |

---

## Gaps Blocking Self-Hosting Port

These must be closed before libcot-zig can be mechanically translated to libcot (Cot):

### Critical (blocks token.zig port)

| Gap | Effort | Tracked In |
|-----|--------|-----------|
| Comptime array literal init (`var arr = [0, 0, 0]` in comptime) | 1 day | COMPTIME_MAP_PLAN.md Step 0 |
| Comptime map support | 2-3 days | COMPTIME_MAP_PLAN.md Steps 1-4 |

### Important (blocks other files)

| Gap | Effort | Notes |
|-----|--------|-------|
| `comptime blk: { break :blk val }` | Half day | Parser: accept label after `comptime` keyword |
| `@compileError(msg)` | Half day | Emit error during comptime evaluation |
| `@bitCast` | 1 day | Reinterpret bits between same-size types |
| General `inline` functions (not just `@inlinable` generics) | 2-3 days | Force inline at call sites |

### Nice to Have (not blocking)

| Gap | Effort | Notes |
|-----|--------|-------|
| `@compileLog` | Half day | Debug output during comptime |
| Comptime function evaluation | 3-5 days | Evaluate entire functions at compile time, not just blocks |
| `@ctz`, `@clz`, `@popCount` builtins | 1 day | Bit manipulation intrinsics |
| `@src()` builtin | Half day | Current source location as struct |

---

## Priority Order

1. **Comptime array literal init** — Step 0, prerequisite for everything
2. **Comptime maps** — Steps 1-4, enables keyword table self-hosting
3. **`comptime blk:` labeled blocks** — Zig parity, small parser fix
4. **`@compileError`** — useful for self-hosted compiler error paths
5. **Everything else** — as needed during the port

**Total effort to unblock self-hosting: ~5-7 days of comptime work.**
