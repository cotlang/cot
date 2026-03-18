# Getting Started with Cot

This guide walks you through installing Cot, writing your first program, and exploring the language features.

## Installation

### Quick Install (macOS / Linux)

```sh
curl -fsSL https://raw.githubusercontent.com/cotlang/cot/main/install.sh | sh
```

### From GitHub Releases

Download the latest binary from [GitHub Releases](https://github.com/cotlang/cot/releases).

### Build from Source

Requires [Zig 0.15+](https://ziglang.org/download/).

```sh
git clone https://github.com/cotlang/cot.git
cd cot
zig build
./zig-out/bin/cot version
```

## Hello World

Create a file called `hello.cot`:

```zig
fn main() i64 {
    println("Hello, world!")
    return 0
}
```

Run it:

```sh
cot run hello.cot
```

Or compile to a native binary:

```sh
cot build hello.cot
./hello
```

Every Cot program starts with a `main()` function that returns `i64` (the exit code).

## Variables

Use `const` for immutable values and `var` for mutable ones. Types are inferred when possible.

```zig
fn main() i64 {
    const name = "Cot"              // immutable, type inferred as string
    var count: i64 = 0              // mutable, explicitly typed
    count = count + 1

    const pi = 3.14                 // f64
    const flag = true               // bool

    println("Hello from ${name}!")  // string interpolation
    println(count)                  // prints 1
    return 0
}
```

## Types

### Primitive Types

| Type | Description |
|------|-------------|
| `i8`, `i16`, `i32`, `i64` | Signed integers |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers |
| `f32`, `f64` | Floating point |
| `bool` | `true` or `false` |
| `string` | Text (alias for `[]u8`) |
| `void` | No value |

Convenience aliases: `int` = `i64`, `float` = `f64`, `byte` = `u8`.

### Arrays and Slices

```zig
const arr = [1, 2, 3, 4, 5]     // fixed-size array [5]i64
const first = arr[0]              // indexing
const mid = arr[1:3]              // slicing → [2, 3]
```

## Functions

Functions declare parameter types and return type explicitly. No semicolons needed.

```zig
fn add(a: i64, b: i64) i64 {
    return a + b
}

fn greet(name: string) void {
    println("Hello, ${name}!")
}

fn main() i64 {
    println(add(3, 4))    // 7
    greet("world")         // Hello, world!
    return 0
}
```

## Control Flow

`if` and `while` require parentheses around the condition (like Zig, unlike Go).

```zig
fn fizzbuzz(n: i64) i64 {
    var i = 1
    while (i <= n) {
        if (i % 15 == 0) {
            println("FizzBuzz")
        } else if (i % 3 == 0) {
            println("Fizz")
        } else if (i % 5 == 0) {
            println("Buzz")
        } else {
            println(i)
        }
        i = i + 1
    }
    return 0
}
```

### For Loops

```zig
// Range-based
for i in 0..10 {
    println(i)
}

// Collection iteration
for item in collection {
    println(item)
}

// With index
for i, item in collection {
    println("${i}: ${item}")
}
```

## Structs

```zig
struct Point { x: i64, y: i64 }

fn main() i64 {
    // Stack allocation (period prefix, equals sign)
    var p = Point { .x = 10, .y = 20 }
    println(p.x + p.y)    // 30
    return 0
}
```

### Methods with Impl Blocks

```zig
struct Rectangle { width: i64, height: i64 }

impl Rectangle {
    fn area(self: *Rectangle) i64 {
        return self.width * self.height
    }

    fn scale(self: *Rectangle, factor: i64) void {
        self.width = self.width * factor
        self.height = self.height * factor
    }
}

fn main() i64 {
    var r = Rectangle { .width = 5, .height = 3 }
    println(r.area())     // 15
    r.scale(2)
    println(r.area())     // 60
    return 0
}
```

Methods take `self: *Type` as the first parameter (explicit, like Zig).

## Error Handling

Cot uses error unions (`E!T`) — a value that is either an error or a success value.

```zig
const MathError = error { DivByZero }

fn divide(a: i64, b: i64) MathError!i64 {
    if (b == 0) {
        return error.DivByZero
    }
    return a / b
}

fn main() i64 {
    // try — propagate errors to caller
    var result = try divide(10, 2)
    println(result)    // 5

    // catch — provide a fallback
    var safe = divide(10, 0) catch 0
    println(safe)      // 0

    // catch with error capture
    var handled = divide(10, 0) catch |err| {
        println("Got an error")
        -1
    }
    println(handled)   // -1
    return 0
}
```

## Generics

Type parameters go in **separate parentheses** (not angle brackets):

```zig
fn max(T)(a: T, b: T) T {
    if (a > b) { return a }
    return b
}

fn main() i64 {
    println(max(i64)(10, 20))       // 20
    println(max(f64)(3.14, 2.71))   // 3.14
    return 0
}
```

Generic structs work the same way:

```zig
struct Pair(T) { first: T, second: T }

fn main() i64 {
    var p = Pair(i64) { .first = 1, .second = 2 }
    println(p.first + p.second)    // 3
    return 0
}
```

## Imports and Standard Library

Import stdlib modules with `import "std/<module>"`:

```zig
import "std/list"
import "std/string"

fn main() i64 {
    // List — dynamic array
    var numbers: List(i64) = .{}
    numbers.append(10)
    numbers.append(20)
    numbers.append(30)
    println(numbers.len())     // 3
    println(numbers.get(1))    // 20

    // String functions
    var s = "Hello, World!"
    println(toUpper(s))        // HELLO, WORLD!
    println(contains(s, "World"))  // true

    return 0
}
```

### Available Stdlib Modules

| Module | Import | Description |
|--------|--------|-------------|
| List | `import "std/list"` | Dynamic array `List(T)` with append, get, set, len, etc. |
| Map | `import "std/map"` | Hash map `Map(K,V)` with get, set, contains, keys, values |
| Set | `import "std/set"` | Hash set `Set(T)` with add, contains, remove |
| String | `import "std/string"` | String functions + `StringBuilder` |
| Math | `import "std/math"` | Integer/float math: abs, min, max, sqrt, pow |
| JSON | `import "std/json"` | JSON parser + encoder |
| File I/O | `import "std/fs"` | File read/write: `readFile`, `writeFile`, `openFile` |
| OS | `import "std/os"` | Process args, environment variables, exit |
| Time | `import "std/time"` | Timestamps, Timer struct |
| Random | `import "std/random"` | Random bytes, integers, ranges |
| Buffered I/O | `import "std/io"` | Buffered reader/writer |
| Encoding | `import "std/encoding"` | Base64 + hex encode/decode |
| URL | `import "std/url"` | URL parsing |
| HTTP | `import "std/http"` | TCP sockets + HTTP response builder |
| Sort | `import "std/sort"` | Sorting for `List(T)` |
| Async | `import "std/async"` | Event loop + async I/O |

## Enums and Unions

```zig
enum Color { Red, Green, Blue }

union Shape {
    Circle: i64          // radius
    Rectangle: i64       // encoded width*height
    Point                // no payload
}
```

## Traits

Define shared behavior across types:

```zig
trait Printable {
    fn display(self: *Self) i64
}

struct Dog { age: i64 }
struct Cat { lives: i64 }

impl Printable for Dog {
    fn display(self: *Dog) i64 { return self.age }
}

impl Printable for Cat {
    fn display(self: *Cat) i64 { return self.lives }
}
```

## Memory Management

Cot uses ARC (Automatic Reference Counting). You rarely need to think about memory.

- **Stack values**: Structs are stack-allocated by default
- **Heap allocation**: Use `new` for heap-allocated structs
- **`defer`**: Runs cleanup at scope exit (LIFO order)
- **`errdefer`**: Runs cleanup only if the function returns an error

```zig
fn example() void {
    // Heap allocation
    var obj = new Foo { x: 42 }
    // ARC automatically manages refcount for heap objects
    // For manual memory: import "std/sys" and use alloc()/dealloc()

    // ARC handles List/Map/Set automatically
    var list: List(i64) = .{}
    list.append(1)
    // list is cleaned up automatically at scope exit
}
```

## Testing

Write tests inline with `test "name" { }` blocks:

```zig
fn add(a: i64, b: i64) i64 {
    return a + b
}

test "addition" {
    @assertEq(add(2, 3), 5)
    @assertEq(add(-1, 1), 0)
}

test "string length" {
    const s = "hello"
    @assertEq(@lenOf(s), 5)
}
```

Run tests:

```sh
cot test myfile.cot
```

## Build Targets

```sh
# Native executable (default — macOS ARM64 or Linux x64)
cot build app.cot

# WebAssembly
cot build --target=wasm32 app.cot       # → app.wasm

# WASI (for edge runtimes like Cloudflare Workers)
cot build --target=wasm32-wasi app.cot   # → app.wasm
```

The same source code compiles to all targets without changes.

## Project Setup

Create a new project with `cot init`:

```sh
cot init myproject
cd myproject
```

This creates:
- `cot.json` — project manifest
- `src/main.cot` — entry point
- `.gitignore`

## Next Steps

- **[Language Syntax Reference](syntax.md)** — Complete reference for every language feature
- **[Examples](https://github.com/cotlang/cot/tree/main/examples)** — Runnable example programs
- **[Vision](https://github.com/cotlang/cot/blob/main/VISION.md)** — Language design philosophy
