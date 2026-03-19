# Cot

[![CI](https://github.com/cotlang/cot/actions/workflows/test.yml/badge.svg)](https://github.com/cotlang/cot/actions/workflows/test.yml)
[![Release](https://github.com/cotlang/cot/actions/workflows/release.yml/badge.svg)](https://github.com/cotlang/cot/actions/workflows/release.yml)

**Write like TypeScript. Run like Rust. Deploy anywhere. Never think about memory.**

Cot is a compiled language that gives you TypeScript's developer experience with Rust's performance. One binary compiles to native ARM64/x64 executables or WebAssembly — same source, both targets. Memory is managed automatically via ARC (no garbage collector, no borrow checker). Everything is built in: formatter, linter, test runner, benchmarks, LSP, and package init.

## What Cot Looks Like

Cot has a `@safe` mode that makes it feel like TypeScript — structs are passed by reference automatically, no `&` or `*` needed, field shorthand works, and methods get implicit `self`:

```zig
@safe

import "std/list"
import "std/json"
import "std/fs"

// --- Types ---

const Priority = enum { Low, Medium, High, Critical }

const TaskError = error { NotFound, InvalidInput }

struct Task {
    title: string,
    priority: Priority,
    done: bool,

    static fn create(title: string, priority: Priority) Task {
        return Task { title, priority, done: false }     // field shorthand
    }

    fn complete() void {
        self.done = true                                  // implicit self
    }

    fn display() string {
        const status = if (self.done) "done" else "todo"
        return "[${status}] ${self.title}"                // string interpolation
    }
}

// --- App ---

struct TaskApp {
    tasks: List(Task),                                    // generic collections
    name: string,

    static fn init(name: string) TaskApp {
        var tasks: List(Task) = .{}
        return TaskApp { tasks, name }
    }

    fn add(title: string, priority: Priority) void {
        self.tasks.append(Task.create(title, priority))   // static method
    }

    fn findByTitle(title: string) TaskError!*Task {       // error union return
        var i = 0
        while (i < self.tasks.len()) {
            var task = self.tasks.getPtr(i)
            if (task.title == title) { return task }
            i += 1
        }
        return error.NotFound
    }

    fn completedCount() i64 {
        var count: i64 = 0
        var i = 0
        while (i < self.tasks.len()) {
            if (self.tasks.get(i).done) { count += 1 }
            i += 1
        }
        return count
    }
}

// --- Inline tests ---

test "task lifecycle" {
    var app = TaskApp.init("work")
    app.add("Ship v1", .Critical)
    app.add("Write docs", .Medium)

    var task = try app.findByTitle("Ship v1")      // try propagates errors
    task.complete()

    @assertEq(app.completedCount(), 1)
    @assertEq(task.display(), "[done] Ship v1")
}

test "error handling" {
    var app = TaskApp.init("work")
    var result = app.findByTitle("nope") catch |err| {
        @assertEq(err, error.NotFound)
        return
    }
}
```

## Real Projects in Cot

### Cotty — A GPU-accelerated terminal emulator (6,500 lines)

[Cotty](https://github.com/cot-land/cotty) is a terminal emulator written in Cot with a native macOS frontend — VT100 parser, gap buffer, terminal grid, JSON config, FFI to Metal/CoreText:

```zig
@safe

struct VtParser {
    state: i64,
    params: List(i64),
    current_param: i64,

    static fn init() VtParser { ... }

    fn feed(terminal: Terminal, byte: i64) void {
        if (self.state == VT_GROUND) {
            if (byte == 0x1B) { self.state = VT_ESCAPE; return }
            if (byte >= 0x20) { terminal.putChar(byte) }
        } else if (self.state == VT_CSI_PARAM) {
            self.handleCSI(terminal, byte)
        }
    }
}
```

```zig
static fn loadFromFile(path: string) Config {
    var cfg = Config.init()
    const content = readFile(path) catch { return cfg }  // graceful fallback
    const root = parse(content)                          // JSON parsing

    const font_val = jsonObjectGet(root, "font-family")
    if (font_val != 0 and jsonTag(font_val) == JSON_STRING) {
        cfg.font_name = jsonGetString(font_val)
    }
    // ...
    return cfg
}
```

### Self-hosted compiler (21,000+ lines)

The Cot compiler is being rewritten in Cot itself. The `self/` directory contains a working compiler frontend — scanner, parser, type checker, and IR builder — all in `@safe` mode:

```zig
fn main() void {
    const cmd = arg(1)
    const path = arg(2)
    const content = readFile(path) catch {
        writeErr("error: cannot read file: ${path}\n")
        exit(1)
    }

    switch (cmd) {
        "parse" => cmdParse(path, content),
        "check" => cmdCheck(path, content),
        "lex"   => cmdLex(path, content),
        else    => { writeErr("unknown command: ${cmd}\n"); exit(1) },
    }
}
```

## Installation

### Quick Install (macOS / Linux)

```sh
curl -fsSL https://raw.githubusercontent.com/cotlang/cot/main/install.sh | sh
```

### From GitHub Releases

Download the latest binary from [GitHub Releases](https://github.com/cotlang/cot/releases):
- `cot-aarch64-macos.tar.gz` — Apple Silicon
- `cot-x86_64-linux.tar.gz` — Linux x64

### Build from Source

Requires [Zig 0.15+](https://ziglang.org/download/).

```sh
git clone https://github.com/cotlang/cot.git && cd cot
git submodule update --init stdlib
zig build
./zig-out/bin/cot version    # → cot 0.3.5 (arm64-macos)
```

## Quick Start

```sh
cot init myapp && cd myapp   # scaffold project with cot.json + src/main.cot
cot run                      # compile and run (reads main from cot.json)
cot test                     # run inline tests
cot build -o myapp           # native binary
cot build --target=wasm      # WebAssembly (WASI — runs via wasmtime)
cot build --target=js        # WebAssembly (browser — JS glue)
cot check                    # type-check without compiling
cot lint                     # warnings
cot fmt                      # format in-place
cot bench                    # benchmarks
cot lsp                      # language server (VS Code/Cursor extension available)
```

## Language at a Glance

### Types and Variables

```zig
const name = "cot"                   // immutable
var count: i64 = 0                   // mutable, explicit type
var ratio = 3.14                     // f64 inferred

// Integer types: i8, i16, i32, i64, u8, u16, u32, u64
// Float types: f32, f64
// Other: bool, string, void, noreturn
```

### Structs, Enums, Unions

```zig
struct Point {
    x: i64, y: i64

    fn magnitude(self: *Point) i64 {       // methods inside struct body
        return self.x * self.x + self.y * self.y
    }

    static fn origin() Point {              // static methods too
        return Point { .x = 0, .y = 0 }
    }
}

const Color = enum { Red, Green, Blue }
const Status = enum { Ok = 0, Warning = 50, Error = 100 }

union Shape {
    Circle: i64,          // payload = radius
    Rect: Point,          // payload = dimensions
    None,                 // no payload
}
```

### Generics

```zig
fn max(T)(a: T, b: T) T {
    if (a > b) { return a }
    return b
}

struct Pair(T, U) { first: T, second: U }

var p = Pair(i64, string) { .first = 42, .second = "hello" }
```

### Error Handling

```zig
const FileError = error { NotFound, PermissionDenied }

fn readConfig(path: string) FileError!string {
    const content = readFile(path) catch { return error.NotFound }
    return content
}

fn loadApp() FileError!void {
    const cfg = try readConfig("app.json")   // propagate with try
    println(cfg)
}

// catch with error matching
var result = readConfig("x") catch |err| switch err {
    error.NotFound => "default",
    error.PermissionDenied => { eprintln("denied"); "fallback" },
}
```

### Closures

```zig
var multiplier: i64 = 3
var triple = fn(x: i64) i64 { return x * multiplier }   // captures multiplier
println(triple(7))   // 21
```

### Traits

```zig
trait Printable {
    fn display(self: *Self) string
}

impl Printable for Point {
    fn display(self: *Point) string {
        return "(${self.x}, ${self.y})"
    }
}
```

### Optionals and Pattern Matching

```zig
var name: ?string = "cot"

if (name) |val| {                    // unwrap with capture
    println("name is ${val}")
}

if (name) |*val| {                   // pointer capture — mutate in place
    val.* = "COT"
}

var x: ?i64 = 42
var y = x ?? 0                       // nullish coalesce
var z = x.?                          // force unwrap (traps if null)
```

### Switch

```zig
switch color {
    .Red => handleRed(),
    .Green, .Blue => handleCool(),
}

// Switch on unions with payload capture
switch shape {
    .Circle => |radius| area = 3 * radius * radius,
    .Rect => |dims| area = dims.x * dims.y,
    .None => area = 0,
}
```

### Defer and Errdefer

```zig
fn processFile(path: string) !void {
    var file = try openFile(path)
    defer closeFile(file)              // runs on ALL exits
    errdefer logError("failed: ${path}")  // runs ONLY on error

    try writeToFile(file, "data")
}
```

### Concurrency

```zig
import "std/channel"

var ch = Channel(i64).init(10)             // buffered channel

spawn {                                     // lightweight goroutine-style tasks
    ch.send(42)
}

var value = ch.recv()                       // blocks until value available
@assertEq(value, 42)

// select statement for multiplexing channels
var ch1 = Channel(i64).init(1)
var ch2 = Channel(string).init(1)
ch1.send(99)

select {
    ch1.recv() => |val| { println("got ${val}") },
    ch2.recv() => |msg| { println("got ${msg}") },
}
```

### Async/Await

```zig
async fn fetchData(url: string) !string {
    var response = await httpGet(url)
    return response.body
}

var data = try await fetchData("https://api.example.com")
```

### `@safe` Mode

Enable with `"safe": true` in `cot.json` or `@safe` at the top of a file. Gives you TypeScript/C# semantics:

| Feature | Normal mode | `@safe` mode |
|---------|------------|-------------|
| Struct params | `fn f(p: *Point)` + `f(&point)` | `fn f(p: Point)` + `f(point)` — auto-ref |
| Struct init | `Point { .x = 1, .y = 2 }` | `Point { x: 1, y: 2 }` — colon syntax |
| Field shorthand | — | `Point { x, y }` → `Point { x: x, y: y }` |
| Methods | `fn getX(self: *Point) i64` | `fn getX() i64` — implicit self |
| Constructors | `new Foo { x: val }` | `new Foo(val)` — calls init() |
| String concat | `a ++ b` | `a + b` — auto-desugars to `++` |

### Testing

```zig
test "math works" {
    @assertEq(2 + 2, 4)
    @assert(10 > 5)
}

test "error handling" {
    var result = mayFail(-1) catch 99
    @assertEq(result, 99)
}
```

```
$ cot test myfile.cot
test "math works" ... ok
test "error handling" ... ok

2 passed
```

## Architecture

```
Cot Source → Scanner → Parser → Checker → IR → SSA
  ├── --target=wasm → lower_wasm → wasm/ → .wasm
  └── --target=native (default)
      → ssa_to_clif → CLIF IR → machinst → regalloc
      → isa/{aarch64,x64}/ → emit → .o → linker → executable
```

Native and Wasm targets have independent backend paths from SSA onwards. The native path is a port of Cranelift — CLIF IR, MachInst lowering, register allocation, ARM64/x64 emission. No LLVM dependency, no runtime, no VM.

## Standard Library

34 modules, all pure Cot:

| Module | Description |
|--------|-------------|
| `std/list` | `List(T)` — dynamic array with 35+ methods |
| `std/map` | `Map(K,V)` — hash map (splitmix64) |
| `std/set` | `Set(T)` — hash set |
| `std/string` | 25+ string functions, `StringBuilder` |
| `std/string_map` | String-keyed hash map (FNV-1a) |
| `std/json` | JSON parser + encoder |
| `std/fs` | File I/O (`openFile`, `readFile`, `writeFile`) |
| `std/os` | Process args, env vars, exit |
| `std/process` | Spawn processes, pipes, PTY |
| `std/time` | Timestamps, `Timer` |
| `std/http` | TCP sockets, HTTP response builder |
| `std/async` | Event loop (kqueue/epoll), async I/O |
| `std/channel` | `Channel(T)` — typed channels for concurrency |
| `std/crypto` | SHA-256, HMAC |
| `std/regex` | Regular expressions |
| `std/io` | Buffered reader/writer, `trait Writer` |
| `std/encoding` | Base64, hex encode/decode |
| `std/url` | URL parser |
| `std/path` | Path manipulation |
| `std/math` | Integer/float math |
| `std/sort` | Sorting for `List(T)` |
| `std/random` | Random bytes, ints, ranges |
| `std/fmt` | Number formatting (hex, binary, pad) |
| `std/cli` | CLI argument parser |
| `std/log` | Structured logging |
| `std/semver` | Semantic versioning |
| `std/uuid` | UUID generation |
| `std/sqlite` | SQLite bindings |
| `std/dotenv` | `.env` file loading |
| `std/mem` | Memory utilities |
| `std/debug` | Debug assertions with messages |
| `std/testing` | Test utilities |
| `std/thread` | OS threads, mutex, channels, atomics |
| `std/sys` | Runtime extern fn declarations |

## Project Status

**v0.3.5** — Compiler written in Zig. Both Wasm and native AOT targets working.

~1,738 tests passing across 73 files. Self-hosted compiler at ~70% frontend parity (21,264 lines of Cot).

| Component | Status |
|-----------|--------|
| Frontend (scanner, parser, checker, lowerer) | Complete |
| SSA infrastructure + optimization passes | Complete |
| Wasm backend (bytecode gen + linking) | Complete |
| Native backend (CLIF IR → regalloc2 → ARM64/x64) | Complete |
| ARC runtime (retain/release, destructor chains) | Complete |
| Generics (shape stenciling + dictionary dispatch) | Complete |
| Concurrency (`spawn`, `Channel(T)`, `select`) | Complete |
| 34-module standard library | Complete |
| LSP server (7 features) | Complete |
| VS Code/Cursor extension | Complete |
| Self-hosted compiler (`self/`) | ~70% — 21,264 lines, 237 tests |

## Why Cot

| vs | What Cot does differently |
|----|--------------------------|
| **TypeScript** | Compiles to native binary — no V8, no cold starts, no `node_modules` |
| **Go** | Wasm is a first-class target — same source runs server + browser |
| **Rust** | No borrow checker — ARC gives memory safety without the learning curve |
| **Zig** | Closures, `@safe` mode, full-stack story — not just systems programming |

## Documents

| Document | Purpose |
|----------|---------|
| [docs/syntax.md](docs/syntax.md) | Complete language syntax reference |
| [VISION.md](VISION.md) | Language vision and design principles |
| [claude/ROADMAP.md](claude/ROADMAP.md) | Roadmap: 0.4 → 1.0 |
| [claude/VERSION_TRAJECTORY.md](claude/VERSION_TRAJECTORY.md) | Self-hosting trajectory |

## License

See [LICENSE](LICENSE) for details.
