# Cot Language Syntax Reference

Complete reference for every language feature. All examples from `test/e2e/features.cot`.

## Variables & Constants

Like Zig: `const` (immutable) and `var` (mutable). `let` is an alias for `var`.

```cot
const x: i64 = 10        // immutable, typed
const y = 20              // immutable, inferred
var z = 30                // mutable, inferred
var w: i64 = 40           // mutable, typed
let a = 50                // alias for var (for devs used to JS/Rust) — avoid in examples
const MyError = error { Fail, NotFound }
```

## Functions

```cot
fn add(a: i64, b: i64) i64 { return a + b }
fn noop() void { return }
fn apply_fn(f: fn(i64) -> i64, x: i64) i64 { return f(x) }
extern fn c_function(x: i64) i64
```

## Generics

Type params in **separate parens** (not angle brackets):

```cot
fn max(T)(a: T, b: T) T { if a > b { return a } return b }
struct Box(T) { value: T }
fn Box_getValue(T)(self: *Box(T)) T { return self.value }

// Instantiation:
max(i64)(3, 7)
var b = Box(i64) { .value = 42 }
```

## Structs

```cot
struct Foo { x: i64 }
struct Point { x: i64, y: i64 }
struct Pair(T, U) { first: T, second: U }
```

## Struct Init — TWO Syntaxes

**Stack (value type):** period prefix `.field`, equals `=`
```cot
var p = Point { .x = 10, .y = 20 }
var pair = Pair(i64, i64) { .first = 10, .second = 32 }
```

**Heap (pointer via `new`):** NO period, colon `:`
```cot
var p = new Foo { x: 42 }
return new Foo { x: val }
```

## Enums

```cot
enum Color { Red, Green, Blue }
enum Status { Ok = 0, Warning = 50, Error = 100 }
```

Access: `Color.Red`, `Status.Ok`

## Unions (tagged)

```cot
union State { Init, Running, Done }
union Result { Ok: i64, Err: i32 }
union Event { Click: i64, Hover, KeyPress: i64 }
```

## Error Sets & Error Handling

```cot
const MyError = error { Fail, NotFound }

fn mayFail(x: i64) MyError!i64 {
    if x < 0 { return error.Fail }
    return x * 2
}

// Try — propagate error to caller
var x = try mayFail(5)

// Catch — handle error with fallback
var y = mayFail(-1) catch 99

// Catch with capture
var z = mayFail(-1) catch |err| { 0 }
```

## Type Aliases

```cot
type Coord = Point
```

## Impl Blocks

```cot
impl Point {
    fn add(self: *Point, other: *Point) Point {
        return Point { .x = self.x + other.x, .y = self.y + other.y }
    }
}

// Generic impl:
impl List(T) {
    fn append(self: *List(T), value: T) void { ... }
}
```

## Traits

```cot
trait Animal {
    fn speak(self: *Self) i64
}

impl Animal for Dog {
    fn speak(self: *Dog) i64 { return self.age }
}

// Trait bounds:
fn bounded_max(T)(a: T, b: T) T where T: Comparable { ... }
```

## Control Flow

```cot
// If/else
if x > 0 { ... }
if x > 0 { ... } else { ... }
if x > 0 { ... } else if x == 0 { ... } else { ... }

// While
while x < 10 { x = x + 1 }

// For-in (collection)
for item in collection { ... }
for i, item in collection { ... }    // indexed

// For-in (range)
for i in 0..10 { ... }

// Break / Continue
break
continue
```

## Switch

```cot
switch value {
    1 => result1,
    2, 3 => result2,        // multiple patterns
    else => default_result,
}

// With capture:
switch result {
    Result.Ok |val| => val,
    Result.Err => 0,
}

// With guard:
switch x {
    1 if x > 0 => 10,
    _ => 0,
}
```

## Defer

```cot
defer @dealloc(ptr)           // runs at end of scope
defer { cleanup_stuff() }     // block form, LIFO order
```

## Closures / Anonymous Functions

```cot
const f = fn(x: i64) i64 { return x * 2 }
const g = fn(x: i64) i64 { return captured_var + x }
apply_fn(fn(x: i64) i64 { return x + 1 }, 5)
```

## String Interpolation

```cot
var msg = "Value is ${x}, next is ${y + 1}"
```

## Types

```cot
i64                    // signed 64-bit integer
i32                    // signed 32-bit integer
f64                    // 64-bit float
f32                    // 32-bit float
bool                   // boolean
void                   // no value
*T                     // pointer to T
?T                     // optional T
E!T                    // error union (E = error set, T = value type)
[]T                    // slice of T
[N]T                   // fixed-size array
fn(T1, T2) -> R        // function type
(T1, T2, T3)           // tuple type
```

## Operators

### Arithmetic
`+` `-` `*` `/` `%`

### Comparison
`<` `<=` `>` `>=` `==` `!=`

### Logical (keywords, not symbols)
`and` `or` `not`

### Bitwise
`&` `|` `^` `<<` `>>` `~` (unary NOT)

### Assignment
`=` `+=` `-=` `*=` `/=` `%=` `&=` `|=` `^=` `<<=` `>>=`

### Unary
`-x` (negate) `~x` (bitwise NOT) `!x` / `not x` (logical NOT) `try expr` `&x` (address of)

### Postfix
`ptr.*` (dereference) `opt.?` (unwrap optional) `.field` (field access) `[i]` (index) `[start:end]` (slice) `(args)` (call)

### Nullish Coalesce
`opt ?? default_value`

## Operator Semantics

- `>>` is **unsigned shift** (logical, zero-fills) — like C's `>>` on `uint64_t`
- `and` / `or` **short-circuit**
- Integer division truncates toward zero: `-100 / 7 == -14`
- Modulo sign follows dividend: `-17 % 5 == -2`

## Indexing & Slicing

```cot
arr[i]               // index
arr[start:end]       // slice
arr[:end]            // slice from start
arr[start:]          // slice to end
```

## Literals

```cot
42                   // integer
0xFF                 // hex
3.14                 // float
"hello"              // string
'A'                  // character (i64)
true / false         // boolean
null                 // null/nil
error.Fail           // error literal
[1, 2, 3]            // array literal
(10, 20)             // tuple literal
```

## Memory & Pointers

```cot
var ptr = @intToPtr(*i64, @alloc(@sizeOf(i64)))
ptr.* = 42                          // dereference assign
var val = ptr.*                     // dereference read
var addr = @ptrToInt(ptr)
@dealloc(addr, @sizeOf(i64))
```

## Builtins

| Builtin | Purpose |
|---------|---------|
| `@alloc(size)` | Allocate bytes |
| `@dealloc(ptr, size)` | Free memory |
| `@realloc(ptr, new_size)` | Reallocate |
| `@memcpy(dst, src, len)` | Copy memory |
| `@sizeOf(T)` | Size of type in bytes |
| `@intCast(T, value)` | Cast integer to type T |
| `@intToPtr(*T, addr)` | Cast integer to pointer |
| `@ptrToInt(ptr)` | Cast pointer to integer |
| `@ptrCast(*T, ptr)` | Cast pointer type |
| `@sqrt(value)` | Square root |
| `@assert(cond)` | Runtime assertion |
| `@trap()` | Unreachable/abort |

## ARC (Automatic Reference Counting)

Heap objects (`new`) are reference-counted automatically:
```cot
var p = new Foo { x: 42 }   // refcount = 1
var q = p                    // refcount = 2
// freed when last reference dies
```

Destructors: functions named `TypeName_deinit` are called automatically:
```cot
fn DFoo_deinit(self: *DFoo) void { ... }
```

## Tests

```cot
test "my test" {
    assert_eq(1 + 1, 2)
}
```

Run with: `cot test file.cot`

## Import

```cot
import "module/path"
```

## Print

```cot
print(42)
println(42)
eprint(42)
eprintln(42)
```

## No Semicolons

Cot does not use semicolons. Newlines terminate statements.
