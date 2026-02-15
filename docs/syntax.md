# Cot Language Syntax Reference

Complete reference for every language feature. Examples from `test/e2e/features.cot`.

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

## Types

### Primitive Types

```cot
// Signed integers
i8  i16  i32  i64

// Unsigned integers
u8  u16  u32  u64

// Floating point
f32  f64

// Other primitives
bool                       // true or false
void                       // no value / no return
noreturn                   // bottom type (functions that never return)
```

### Type Aliases (keywords)

```cot
int                        // alias for i64
float                      // alias for f64
byte                       // alias for u8
string                     // alias for []u8 (ptr + len pair at ABI level)
```

### Composite Types

```cot
*T                         // pointer to T
?T                         // optional T (can be null)
E!T                        // error union (E = error set, T = value type)
!T                         // inferred error union (compiler tracks error set)
[]T                        // slice of T (ptr + len)
[N]T                       // fixed-size array of N elements of type T
[K]V                       // map type (key K, value V)
[T]                        // list type (dynamic array of T)
fn(T1, T2) -> R            // function type
(T1, T2, T3)               // tuple type
```

## Literals

```cot
// Integers
42                         // decimal
0xFF                       // hexadecimal
0b1010                     // binary
0o777                      // octal
1_000_000                  // underscores for readability

// Floats
3.14                       // decimal
1e-5                       // scientific notation
1_000.5                    // underscores

// Strings
"hello"                    // string literal
"Value is ${x}"            // string interpolation

// Characters
'A'                        // character literal (i64 value)
'\n'                       // escape sequences

// Other
true                       // boolean true
false                      // boolean false
null                       // null value (for optionals)
undefined                  // undefined value
error.Fail                 // error literal
[1, 2, 3]                 // array literal
(10, 20)                   // tuple literal
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
fn max(T)(a: T, b: T) T { if (a > b) { return a } return b }
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
    if (x < 0) { return error.Fail }
    return x * 2
}

// Inferred error set — no need to name the error set
fn mayFail2(x: i64) !i64 {
    if (x < 0) { return error.Negative }
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
// If/else (parens required, like Zig)
if (x > 0) { ... }
if (x > 0) { ... } else { ... }
if (x > 0) { ... } else if (x == 0) { ... } else { ... }

// If optional unwrap (Zig pattern)
if (optional_val) |val| { ... } else { ... }

// While (parens required)
while (x < 10) { x = x + 1 }

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

## Defer / Errdefer

```cot
defer @dealloc(ptr)           // runs at end of scope
defer { cleanup_stuff() }     // block form, LIFO order
errdefer @dealloc(ptr)        // runs ONLY if function returns error
```

`defer` runs unconditionally at scope exit. `errdefer` runs only on error return (Zig pattern). Both follow LIFO ordering and can be mixed freely.

## Async / Await

```cot
async fn fetchData(url: string) HttpError!string {
    var fd = try await asyncConnect(loop_fd, sock, addr, len)
    var result = try await asyncRead(loop_fd, fd, buf, 1024)
    return @string(buf, result)
}

// Await extracts result from future
const data = await fetchData("http://example.com")

// try await — propagate errors across await points
const data = try await fetchData("http://example.com")

// await with catch
const data = await fetchData("http://example.com") catch "fallback"
```

**Dual backend:**
- Wasm: Rust-style stackless state machine (constructor + poll function)
- Native: Zig-style eager evaluation (body runs as normal function)

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

## Operators

### Arithmetic
`+` `-` `*` `/` `%`

### Comparison
`<` `<=` `>` `>=` `==` `!=`

### Logical
`and` `or` `not` (keywords)
`&&` `||` `!` (symbol alternatives)

### Bitwise
`&` `|` `^` `<<` `>>` `~` (unary NOT)

### Assignment
`=` `+=` `-=` `*=` `/=` `%=` `&=` `|=` `^=`

### Unary
`-x` (negate) `~x` (bitwise NOT) `!x` / `not x` (logical NOT) `try expr` `await expr` `&x` (address of)

### Postfix
`ptr.*` (dereference) `opt.?` (unwrap optional) `?.` (optional chain) `.field` (field access) `[i]` (index) `[start:end]` (slice) `(args)` (call)

### Nullish Coalesce
`opt ?? default_value`

### Operator Precedence (low to high)

| Precedence | Operators |
|------------|-----------|
| 1 (lowest) | `??` |
| 2 | `\|\|`, `or` |
| 3 | `&&`, `and` |
| 4 | `==`, `!=`, `<`, `<=`, `>`, `>=` |
| 5 | `+`, `-`, `\|`, `^` |
| 6 (highest) | `*`, `/`, `%`, `&`, `<<`, `>>` |

### Operator Semantics

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

## Memory & Pointers

```cot
var ptr = @intToPtr(*i64, @alloc(@sizeOf(i64)))
ptr.* = 42                          // dereference assign
var val = ptr.*                     // dereference read
var addr = @ptrToInt(ptr)
@dealloc(addr, @sizeOf(i64))
&expr                               // address-of
```

## Builtins

### Type Intrinsics

| Builtin | Purpose |
|---------|---------|
| `@sizeOf(T)` | Size of type in bytes |
| `@alignOf(T)` | Alignment of type |
| `@intCast(T, value)` | Cast integer to type T |
| `@ptrCast(*T, ptr)` | Cast pointer type |
| `@intToPtr(*T, addr)` | Cast integer to pointer |
| `@ptrToInt(ptr)` | Cast pointer to integer |

### Memory

| Builtin | Purpose |
|---------|---------|
| `@alloc(size)` | Allocate bytes on heap |
| `@dealloc(ptr)` | Free memory |
| `@realloc(ptr, new_size)` | Reallocate |
| `@memcpy(dst, src, len)` | Copy memory |

### String

| Builtin | Purpose |
|---------|---------|
| `@string(ptr, len)` | Construct string from ptr and len |
| `@ptrOf(s)` | Extract raw pointer from string |
| `@lenOf(s)` | Extract length from string |

### Math

| Builtin | Purpose |
|---------|---------|
| `@abs(value)` | Absolute value (float) |
| `@ceil(value)` | Ceiling (float) |
| `@floor(value)` | Floor (float) |
| `@trunc(value)` | Truncate toward zero (float) |
| `@round(value)` | Round to nearest (float) |
| `@sqrt(value)` | Square root (float) |
| `@fmin(a, b)` | Minimum of two floats |
| `@fmax(a, b)` | Maximum of two floats |

### File I/O (WASI)

| Builtin | Purpose |
|---------|---------|
| `@fd_write(fd, ptr, len)` | Write to file descriptor |
| `@fd_read(fd, buf, len)` | Read from file descriptor |
| `@fd_close(fd)` | Close file descriptor |
| `@fd_seek(fd, offset, whence)` | Seek in file |
| `@fd_open(path_ptr, path_len, flags)` | Open file |

### Process

| Builtin | Purpose |
|---------|---------|
| `@exit(code)` | Exit process |
| `@args_count()` | Number of CLI arguments |
| `@arg_len(n)` | Length of argument n |
| `@arg_ptr(n)` | Pointer to argument n |
| `@environ_count()` | Number of environment variables |
| `@environ_len(n)` | Length of env var n |
| `@environ_ptr(n)` | Pointer to env var n |

### Networking

| Builtin | Purpose |
|---------|---------|
| `@net_socket(domain, type, protocol)` | Create a socket |
| `@net_bind(fd, addr, len)` | Bind socket to address |
| `@net_listen(fd, backlog)` | Listen for connections |
| `@net_accept(fd)` | Accept a connection |
| `@net_connect(fd, addr, len)` | Connect to address |
| `@net_set_reuse_addr(fd)` | Set SO_REUSEADDR on socket |

### Event Loop

| Builtin | Purpose |
|---------|---------|
| `@kqueue_create()` | Create kqueue fd (macOS) |
| `@kevent_add(kq, fd, filter)` | Register fd for events (macOS) |
| `@kevent_del(kq, fd, filter)` | Remove fd from kqueue (macOS) |
| `@kevent_wait(kq, buf, max)` | Wait for kqueue events (macOS) |
| `@epoll_create()` | Create epoll fd (Linux) |
| `@epoll_add(epfd, fd, events)` | Register fd for events (Linux) |
| `@epoll_del(epfd, fd)` | Remove fd from epoll (Linux) |
| `@epoll_wait(epfd, buf, max)` | Wait for epoll events (Linux) |
| `@set_nonblocking(fd)` | Set fd to non-blocking mode |

### System

| Builtin | Purpose |
|---------|---------|
| `@time()` | Wall clock time in nanoseconds |
| `@random(buf, len)` | Fill buffer with random bytes |
| `@trap()` | Unconditional trap / unreachable |

### Comptime

| Builtin | Purpose |
|---------|---------|
| `@target_os()` | Target OS as string ("darwin", "linux") |
| `@target_arch()` | Target arch as string ("arm64", "x86_64") |
| `@target()` | Full target description |
| `@compileError("msg")` | Trigger compile-time error with message |
| `@embedFile("path")` | Embed file contents as string at compile time |

`comptime { expr }` blocks evaluate expressions at compile time:

```cot
const SIZE = comptime { 4 * 1024 }
```

Dead branch elimination: if an `if` condition is comptime-known, only the taken branch is checked. This enables `@compileError` in unreachable branches:

```cot
if @target_os() == "darwin" {
    // macOS code
} else {
    @compileError("unsupported OS")
}
```

### Testing

| Builtin | Purpose |
|---------|---------|
| `@assert(cond)` | Assert condition is true |
| `@assert_eq(a, b)` | Assert values are equal |

## Print Functions

Built-in functions (not regular functions, handled specially by the compiler):

```cot
print(42)              // print to stdout, no newline
println(42)            // print to stdout with newline
eprint(42)             // print to stderr, no newline
eprintln(42)           // print to stderr with newline
```

All accept `i64`, `f64`, or `string` arguments.

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
    @assert_eq(1 + 1, 2)
}
```

Run with: `cot test file.cot`

## Import

```cot
import "module/path"
import "std/list"          // stdlib modules
```

## Stdlib Modules

| Module | Import | Contents |
|--------|--------|----------|
| `fs` | `import "std/fs"` | File struct, openFile, createFile, readFile, writeFile, stdin/stdout/stderr |
| `os` | `import "std/os"` | exit, arg(n), environ(n), argsCount, environCount |
| `time` | `import "std/time"` | Timer struct, nanoTimestamp, milliTimestamp, timestamp |
| `random` | `import "std/random"` | fillBytes, randomInt, randomRange |
| `list` | `import "std/list"` | List(T) with ~20 methods (append, get, remove, indexOf, clone, etc.) |
| `map` | `import "std/map"` | Map(K,V) with splitmix64 hash, ~25 methods |
| `set` | `import "std/set"` | Set(T) wrapping Map(T, i64) |
| `string` | `import "std/string"` | ~25 string functions (indexOf, split, trim, replace, etc.) + StringBuilder |
| `math` | `import "std/math"` | abs, min, max, clamp, ipow, fabs, ceil, floor, sqrt, fmin, fmax, PI, E |
| `json` | `import "std/json"` | JSON parser (parse) + encoder (encode), JsonValue constructors + accessors |
| `sort` | `import "std/sort"` | Insertion sort + reverse for List(T) |
| `io` | `import "std/io"` | BufferedReader, BufferedWriter (4KB buffer) |
| `encoding` | `import "std/encoding"` | Base64 (standard + URL-safe) + hex encode/decode |
| `url` | `import "std/url"` | URL parsing (scheme, host, port, path, query, fragment) |
| `http` | `import "std/http"` | TCP sockets, HTTP response builder, sockaddr_in |
| `async` | `import "std/async"` | Event loop (kqueue/epoll), async I/O wrappers (asyncRead, asyncWrite, asyncAccept, asyncConnect) |
| `path` | `import "std/path"` | join, dirname, basename, extname, isAbsolute, relative, clean |
| `crypto` | `import "std/crypto"` | SHA-256, HMAC-SHA256 |
| `fmt` | `import "std/fmt"` | ANSI colors, text styles, formatBytes, formatDuration, padding |
| `log` | `import "std/log"` | Structured logging (debug/info/warn/logError), timestamps, key-value |
| `dotenv` | `import "std/dotenv"` | Parse .env files, get/has/entryCount |
| `cli` | `import "std/cli"` | --flag=value, -f, positional args, getFlag/hasFlag/getFlagInt |
| `uuid` | `import "std/uuid"` | UUID v4 generation, isValid, version |
| `semver` | `import "std/semver"` | Parse, compare (gt/lt/eq), format, incMajor/incMinor/incPatch |
| `testing` | `import "std/testing"` | assertContains, assertStartsWith, assertGt, assertTrue, assertLen |

## @safe Mode

The `@safe` file annotation enables C#/TypeScript-friendly extensions. All `@safe` features are **opt-in** — they only activate in files that start with `@safe` and never affect the base language.

```cot
@safe  // must be the first line

struct Point { x: i64, y: i64 }
```

### Feature Summary

| Feature | Standard Cot | @safe Mode |
|---------|-------------|------------|
| Struct init (stack) | `Point { .x = 10, .y = 20 }` | `Point { x: 10, y: 20 }` (colon syntax) |
| Field shorthand | Not available | `new Point { x, y }` → `new Point { x: x, y: y }` |
| Self parameter | `fn getX(self: *Point) i64` | `fn getX() i64` (self injected) |
| Constructor | `new Point { x: 10, y: 20 }` | `new Point(10, 20)` (calls init) |
| Struct params | `fn foo(p: *Point)` explicit | `fn foo(p: Point)` auto-wrapped to pointer |

### Unified Init Syntax (Colon Syntax)

```cot
// Standard:   Point { .x = 10, .y = 20 }
// @safe mode: Point { x: 10, y: 20 }     — colon instead of period+equals
var p = Point { x: 10, y: 20 }
```

### Field Init Shorthand

```cot
var x: i64 = 10
var y: i64 = 20
var p = new Point { x, y }        // shorthand for { x: x, y: y }
var q = new Point { x, y: 99 }    // mixed shorthand + explicit
```

### Implicit Self

In non-generic impl blocks, `self: *Type` is injected automatically:

```cot
impl Point {
    // Standard:   fn getX(self: *Point) i64 { return self.x }
    // @safe mode: fn getX() i64 { return self.x }
    fn getX() i64 { return self.x }
    fn setX(val: i64) void { self.x = val }
}
```

Explicit `self` still works. Generic impl blocks require explicit self.

### Constructor Sugar

`new Type(args...)` calls the `init(self: *Type, ...)` method after allocation:

```cot
impl Rect {
    fn init(w: i64, h: i64) void {  // self injected by implicit self
        self.width = w
        self.height = h
    }
}

var r = new Rect(10, 20)  // allocates + calls init
```

## Destructuring

Unpack tuples into multiple variables:

```cot
fn getPair() (i64, i64) { return (10, 20) }

// Destructure a tuple literal
const a, b = (10, 20)

// Destructure from a function return
const x, y = getPair()

// Mutable destructured bindings
var p, q = (1, 2)
p = p + 10

// Three or more elements
const a, b, c = (1, 2, 3)
```

All bindings in a destructure statement share the same mutability (`const` or `var`).

## noreturn Type

The `noreturn` type represents functions that never return — they always exit, trap, or loop forever. It is a bottom type that coerces to any other type.

```cot
fn exit_wrapper(code: i64) noreturn {
    @exit(code)
}

// noreturn in if branches — the other branch determines the type
var x = if (cond) { @exit(1) } else { 42 }
```

Builtins `@exit` and `@trap` return `noreturn`. A function with return type `noreturn` cannot contain a `return` statement.

## @embedFile

Embed a file's contents as a `string` at compile time:

```cot
const data = @embedFile("config.txt")
const template = @embedFile("templates/page.html")
```

The path is resolved **relative to the source file**, not the working directory. Maximum file size is 10MB.

## No Semicolons

Cot does not use semicolons. Newlines terminate statements.
