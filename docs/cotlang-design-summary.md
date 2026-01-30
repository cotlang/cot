# Cot Language Design Summary

Reference document summarizing key design decisions for the Cot programming language.

---

## 1. Language Syntax Reference

### Core Syntax Decisions (Zig-inspired with Rust influences)

**Variables:**
```cot
var x = 42              // Mutable, inferred type
const w: i64 = 42       // Immutable, explicit type
```

**Functions:**
```cot
fn add(a: i64, b: i64) i64 {    // Return type after params, no colon
    return a + b
}
```

**Structs (Zig-style initialization):**
```cot
struct Point { x: i64, y: i64 }
var p = Point{ .x = 10, .y = 20 }    // MUST use .field = value
```

**Methods (impl blocks):**
```cot
impl Point {
    fn init(x: i64, y: i64) Point {    // Constructor convention: init()
        return Point{ .x = x, .y = y }
    }
    fn distance(self: Point, other: Point) f64 { ... }
    fn move(self: *Point, dx: i64) { ... }    // Pointer self for mutation
}
```

**Control Flow:**
```cot
if (condition) { }           // Parentheses REQUIRED
for i in 0..5 { }            // Range iteration (exclusive)
for i in 0..=5 { }           // Inclusive range
switch (value) { 1 => { }, else => { } }    // Not "match"
```

**Error Handling (Zig-style error unions):**
```cot
fn readFile(path: string) !string {     // ! = can return error
    if (notFound) return error.FileNotFound
    return contents
}

const data = try readFile("config.txt")           // Propagate error
const data = readFile("x") catch "default"        // Default on error
const data = readFile("x") catch |err| { ... }    // Handle error

defer cleanup()              // Runs on scope exit
```

**Optional Types:**
```cot
var x: ?i64 = null
x?.field                     // Null-safe access
x ?? default                 // Null coalescing
```

**Key Operators:**
- `and`, `or`, `not` (preferred over `&&`, `||`, `!`)
- `p.*` for pointer dereference (Zig-style, NOT `*p`)

**Generics:**
```cot
struct Box<T> { value: T }
fn identity<T>(x: T) T { return x }
fn compare<T: Comparable>(a: T, b: T) bool { ... }
```

**Traits:**
```cot
trait Printable { fn print(self); }
impl Printable for Point { fn print(self) { ... } }
var obj: dyn Printable = point    // Dynamic dispatch
```

**Lambdas:**
```cot
var add = |a: i64, b: i64| { return a + b }
```

**Built-in Collections:**
```cot
var list = new List<i64>
var map = new Map<string, i64>
```

**Heap Allocation (ARC-managed):**
```cot
var p = new Point{ .x = 10, .y = 20 }    // Returns *Point
```

**String Interpolation:**
```cot
var msg = "Hello ${name}!"
var calc = "Sum: ${a + b}"
```

**Compile-time Features:**
```cot
comptime { }
comptime if (@os() == "windows") { }
@sizeof(T), @typeName(T), @hasField(T, "x")
```

### Primitive Types

**Friendly aliases** (for developer convenience):
- `int` → `i64`, `float` → `f64`, `uint` → `u64`
- `string` → `[]u8` (slice of bytes)
- `byte` → `u8`

**Full precision types** (when needed):
- Signed: `i8`, `i16`, `i32`, `i64`, `isize`
- Unsigned: `u8`, `u16`, `u32`, `u64`, `usize`
- Float: `f32`, `f64`
- Other: `bool`, `void`

---

## 2. Package Manager (cot.land) Architecture

### Design Philosophy
Following the **deno.land model**: lightweight GitHub proxy, not npm-style registry.

**Key decisions:**
1. No "publishing" - authors just push git tags
2. Immutable versions - once `oak@1.0.0` exists, it never changes
3. GitHub as source of truth - no package upload/storage
4. URL-addressable - every file has permanent URL

### URL Structure
```
cot.land/std/@0.1.0/http/server.cot     # Standard library
cot.land/x/oak@1.0.0/mod.cot            # Third-party packages
```

### Package Manifest (cot.toml)
```toml
[package]
name = "myapp"
version = "0.1.0"

[dependencies]
std = "0.2.0"
oak = "1.0.0"
```

### CLI Commands
```bash
cot pkg install      # Download dependencies
cot pkg add oak@1.0  # Add dependency
```

### Infrastructure Recommendation
**Cloudflare Workers + R2** for initial deployment:
- Edge caching for popular packages
- R2 for file storage (free tier generous)
- D1 for SQLite registry database

### API Endpoints
```
GET /std/@{version}/{path}           # Standard library files
GET /x/{package}@{version}/{path}    # Third-party files
GET /api/packages/{name}             # Package metadata
GET /api/registry.json               # Full registry for CLI caching
POST /api/packages                   # Register package (GitHub OAuth)
POST /api/webhook/github             # New tag notifications
```

### Cache Strategy
- Versioned files: `Cache-Control: immutable` (never invalidate)
- Latest pointers: 5 min TTL
- Registry JSON: 1 min TTL

### Security Considerations
- Reserved names list for std modules
- Version immutability enforced (keep old cache even if tag changes)
- Rely on GitHub's existing trust model

---

## 3. Language Features Required for Dogfooding

### Feature Dependency Order (for building cot.land in Cot)

**Core Language (must work first):**
1. impl method dispatch
2. Collection iteration (for-in loops)
3. Closures with variable capture

**Native Bindings:**
4. JSON parsing/serialization
5. HTTP client (for GitHub API)
6. File I/O (for caching)
7. Cryptographic hashing (SHA-256 for checksums, HMAC for webhooks)
8. SQLite database

**HTTP Server:**
9. Route registration with patterns (`/api/packages/:name`)
10. Closure-based handlers
11. Request/Response types

### Standard Library Namespaces
```cot
std.json     // parse(), stringify()
std.http     // client: get(), post(), request()
             // server: Server, Request, Response
std.fs       // read_file(), write_file(), mkdir_all(), etc.
std.crypto   // sha256_hex(), hmac_sha256_hex(), hex_encode()
std.sql      // sql_open(), sql_exec(), sql_query(), sql_fetch()
```

---

## 4. Core Language Identity

### Target Market
**Modern business applications** - the space between:
- Systems languages (Rust/Zig) - too low-level
- Enterprise legacy (Java/C#) - too verbose
- Dynamic languages (Python/JS) - not enough safety/performance

### Design Principles

1. **Explicit over implicit** - No hidden control flow or magic
2. **Composition over inheritance** - Traits and impl, not class hierarchies
3. **Null safety by default** - Optional types are explicit (`?T`)
4. **Friendly types** - `int`, `float`, `string` for everyday use
5. **Fast compilation, fast execution**
6. **Pragmatic, not dogmatic** - Good ideas from many languages

### Memory Model: ARC (Swift-inspired)
- Automatic Reference Counting, not traditional GC
- Objects freed immediately when refcount = 0
- No GC pauses, predictable memory usage
- Background cycle collector for rare circular references
- `weak` keyword as expert escape hatch

### Error Handling Philosophy (Zig-style)
- **Error unions**: `fn read() !Data` - errors are values, not exceptions
- **try keyword**: Propagates errors to caller
- **catch keyword**: Handle or provide default inline
- **defer for cleanup**: Always runs on scope exit
- **No exceptions**: All errors are explicit in function signatures

### Syntax Philosophy
- Familiar to C#/Java developers
- Expression-oriented where it makes sense
- No unnecessary ceremony

### What Cot Is NOT
- Not a systems language (kernels, drivers, embedded)
- Not a scripting language (one-off scripts, notebooks)
- Not maximally minimalist (has generics, ADTs, traits from day one)

---

## 5. Competitive Positioning

### Positioning Statement
"The native Go with richer types, the practical Rust without the learning curve, the modern Java without verbosity."

### Unique Selling Points
1. **Wasm-first** - Same code runs in browser, server, and edge
2. **Native compilation** - AOT to native when performance matters
3. **Approachable types** - Friendly aliases, not systems-programming verbosity
4. **Clean syntax** - Modern, not 1990s enterprise
5. **Fast compilation** - Zig-speed compilation

### Competitive Advantages by Language

**vs Java:**
- Clean syntax, no ceremony
- Null safety required (no NPE)
- Fast startup (no JVM warmup)
- First-class generics (no type erasure)

**vs Go:**
- Richer type system (generics, ADTs, traits)
- No `if err != nil` everywhere (Result types)
- More expressive while remaining simple

**vs Rust:**
- Simpler memory model (ARC vs ownership)
- Lower learning curve
- Friendly type aliases for everyday use

**vs C#:**
- Cross-platform native (no Windows perception)
- Null safety required from start (not opt-in)

**vs TypeScript:**
- Native performance (no JavaScript runtime ceiling)
- Better for CPU-intensive business logic

### Realistic Success Factors

**Required for success:**
- Genuine productivity advantage (not just different)
- Competitive performance (faster than JVM, comparable to Go)
- One killer use case (full-stack Wasm apps)
- Sustained multi-year development

**Risk factors:**
- Ecosystem may not materialize
- Single maintainer burnout
- Better alternative could emerge

---

## Summary of Key Syntax Decisions

| Feature | Cot Syntax | NOT This |
|---------|------------|----------|
| Struct init | `Point{ .x = 10, .y = 20 }` | `Point{ x: 10 }` |
| Pointer deref | `ptr.*` | `*ptr` |
| Return type | `fn foo() i64` | `fn foo(): i64` |
| Conditionals | `if (cond) { }` | `if cond { }` |
| Pattern match | `switch (x) { }` | `match (x) { }` |
| Logic ops | `and`, `or`, `not` | `&&`, `\|\|`, `!` (work but not preferred) |
| Static method | `Type.method()` | `Type::method()` |
| Heap alloc | `new Type{ }` | `alloc(Type)` |
| Constructor | `Type.init(...)` | `new Type(...)` |
