# Cot: Road to 1.0

## Where We Are: Cot 0.3

The compiler handles a real programming language — generics, closures, ARC, error unions, defer, traits, all working on both Wasm and native AOT. The architecture is proven: Cot source compiles to Wasm bytecode, which either runs in a browser or gets AOT-compiled to ARM64/x64 native executables through a Cranelift-port pipeline.

### What 0.3 Has Achieved

- **Working I/O** — `print`, `println`, `eprint`, `eprintln` with native syscalls
- **Standard library** — `List(T)` with ~35 methods, `Map(K,V)` with splitmix64 hash via `import "std/map"`
- **Cross-file generics** — `SharedGenericContext` enables multi-file programs with generics
- **CLI** — `cot build`, `cot run`, `cot test`, `cot bench`, `cot check`, `cot lint`, `cot fmt`, `cot init`, `cot version`, `cot help` — all with `cot.json` fallback
- **Test framework** — inline `test "name" { }` blocks with `@assert`/`@assert_eq`, summary output
- **Basic LSP** — diagnostics, hover, goto definition, document symbols
- **VS Code/Cursor extension** — TextMate syntax highlighting + LSP client
- **Auto scope-exit cleanup** — structs with `free()` get automatic cleanup
- **Traits with bounds** — `where T: Hashable` with monomorphized dispatch

### What 0.3 Still Lacks for Real Use

1. ~~**No collections beyond List(T)**~~ — **Done**: Map(K,V) with splitmix64 hash, Set(T) wrapping Map
2. ~~**No string interpolation**~~ — **Done**: `"Hello, ${name}"` with integer auto-conversion
3. ~~**No file I/O**~~ — **Done**: `std/fs` (File struct, openFile, createFile, 10 methods), `std/os` (args, environ, exit), `std/time`, `std/random`
4. **No package manager** — can't install libraries
5. ~~**No async**~~ — **Done**: `async fn` / `await` with dual backend (Wasm state machine + native eager eval), event loop (kqueue/epoll), async I/O wrappers
6. **No framework** — the full-stack client/server story doesn't exist yet
7. **No documentation** — no language guide, no API docs, no tutorials

---

## What 1.0 Means

**1.0 is not "feature-complete forever." It's "useful enough to build real things and stable enough to depend on."**

### The 1.0 Contract

A developer who installs Cot 1.0 should be able to:

1. **Write a web server** that handles HTTP requests, reads files, talks to a database
2. **Write a browser application** that manipulates the DOM, handles events, fetches APIs
3. **Share code between client and server** — the core promise
4. **Use typed collections** — `List(T)`, `Map(K,V)`, `Set(T)`
5. **Handle errors properly** — error unions, try/catch, meaningful error messages
6. **Import packages** — a package manager with a registry
7. **Get editor help** — LSP with autocomplete, go-to-definition, error highlighting
8. **Read documentation** — language guide, standard library API docs, examples
9. **Trust stability** — code that compiles today will compile tomorrow

### What 1.0 is NOT

- Not self-hosting (that's a maturity milestone, not a user requirement)
- Not the fastest language ever (competitive is enough)
- Not feature-frozen (1.x releases add features; 1.0 syntax is stable)

---

## The I/O Architecture: WASI as Design Blueprint

### The Insight

Other languages that compile to Wasm (Rust, Go, C) require an external Wasm runtime (wasmtime, wasmer) to run on the server. Cot doesn't — the compiler AOT-compiles directly to native. But Cot still needs an I/O model.

WASI provides a well-designed, community-vetted standard for system I/O (files, sockets, clocks, random). Rather than inventing our own abstractions, Cot uses WASI's interface design as the blueprint for its I/O layer. The compiler then translates these operations to the appropriate target:

```
                    Cot I/O call (e.g., file.read())
                              │
                    ┌─────────┼─────────────┐
                    ▼         ▼             ▼
              Native       Browser        WASI runtime
              libc/        Web API        WASI imports
              syscalls     imports        (Cloudflare, etc.)
```

### Why This Matters

| Language | Server-side Wasm | Native server | Browser |
|----------|-----------------|---------------|---------|
| Rust | wasmtime required | Yes (separate build) | wasm-bindgen required |
| Go | TinyGo + wasmtime | Yes (separate build) | TinyGo, limited stdlib |
| **Cot** | **Optional** (WASI target available) | **Yes** (AOT, no runtime) | **Yes** (first-class target) |

Cot is the only language where server = native binary (no runtime), browser = Wasm, and edge/WASI = optional bonus — all from the same source code, same compiler, same I/O model.

### The Three Output Modes

| Mode | CLI flag | I/O model | Use case |
|------|----------|-----------|----------|
| Native | (default) | libc/syscalls | Server binaries, CLI tools |
| Browser Wasm | `--target=wasm32` | Web API imports | Client-side apps |
| WASI Wasm | `--target=wasm32-wasi` | WASI imports | Edge, serverless, plugins |

For standalone compiler use, developers specify targets explicitly. For the Cot framework (full-stack apps), the framework handles target selection automatically.

---

## The Framework Layer

### Next.js-Style Full-Stack

The Cot framework is the product-level differentiator. Like Next.js, developers write code with annotations, and the build tool handles compilation for each target:

```cot
// shared — compiled for both targets
struct User { id: i64, name: []u8 }

// server — compiled to native
@server
fn get_user(id: i64) !User {
    return db.query("SELECT ...", id)
}

// client — compiled to Wasm
@client
fn render_profile(user: User) {
    dom.set_text("#name", user.name)
}
```

The framework:
1. Compiles `@server` code to a native binary
2. Compiles `@client` code to `.wasm` for the browser
3. Compiles shared code for both targets
4. Generates the boundary layer (serialization, HTTP transport)

Developers never think about `--target` flags in framework projects.

### Framework vs Compiler

The `cot` compiler is a low-level tool (like `rustc` or `zig`). The framework is a higher-level tool (like Next.js or Deno). Both are part of the Cot project, but the compiler exists independently for standalone use.

---

## The Wasm IR Question

### Current Architecture

Every Cot program compiles through Wasm bytecode as internal IR:

```
Cot Source → Frontend → SSA → Wasm bytecode → { Browser | AOT native }
```

This is the right architecture for 0.x. It simplifies the compiler (one backend), ensures feature parity between targets, and catches bugs early.

### Where Wasm Becomes Limiting

**Async was predicted to require an IR split** — Wasm has no stack switching, so `async fn` for native would need a direct SSA → CLIF IR path bypassing Wasm. However, the actual implementation avoids this:

- **Wasm target**: Rust-style stackless state machine (poll-based). The compiler transforms `async fn` into a constructor + poll function pair, with if-chain dispatch on state. This goes through the normal Wasm pipeline.
- **Native target**: Zig-style eager evaluation. The async function body runs as a normal function, with results stored in a heap-allocated future. No state machine transform needed.

Both approaches stay within the existing Wasm-first architecture — no IR split was required.

Everything else (including tail calls, exceptions, and typed function references) was resolved by Wasm 3.0 (released September 2025). See `claude/specs/WASM_3_0_REFERENCE.md` for details.

### The IR Split (If Ever Needed)

The IR split remains an option for future features that truly cannot go through Wasm. The existing CLIF IR, MachInst, regalloc, and emission infrastructure is already built — only a `lower_clif.zig` translation layer would be new. But async/await did not require it.

### Async Strategy (Implemented)

Different targets use different async mechanisms, unified by the same syntax:

- **Native:** Zig-style eager evaluation (body as normal function) + OS event loop (kqueue on macOS, epoll on Linux) + async I/O wrappers (EAGAIN → register → wait → retry)
- **Wasm:** Rust-style stackless state machine (constructor + poll function, if-chain dispatch)
- **Browser:** JavaScript Promise interop (`async fn` returns a Promise via JS glue) — planned
- **WASI:** WASI 0.3 async model (component model `future`/`stream` types) — planned

The syntax is the same (`async fn`, `await`, `try await`). The lowering differs per target.

---

## Versioning Philosophy

**Cot follows Zig's model: versions mark architectural milestones, not feature checklists.**

Zig is 10+ years of development and still on 0.15. Each version represents a meaningful shift in capability. Cot takes the same approach — 1.0 will take multiple years, but the language should be viable for projects well before 1.0.

---

## Feature Roadmap

### 0.3: Make the Language Real (Largely Complete)

**Done:**
- `print` / `println` / `eprint` / `eprintln` — output to stdout/stderr
- `import "std/list"` — cross-file generics, stdlib discovery
- `cot build` / `cot run` / `cot test` / `cot version` — CLI subcommands
- Inline test framework with `@assert` / `@assert_eq`, summary output
- Trait bounds on generics — `where T: Trait` syntax, monomorphized dispatch
- Basic LSP server (diagnostics, hover, goto definition, document symbols)

**Remaining:**
- ~~Map(K,V)~~ — **Done** — hash map with set/get/has/delete/keys/values, auto-free
- ~~Set(T)~~ — **Done** — wraps Map(T, i64), add/has/remove/len/toList, auto-free
- ~~String interpolation~~ — **Done** — `"Hello, ${name}"` with integer auto-conversion
- ~~String equality~~ — **Done** — `@assert_eq("hello", "hello")` via `cot_string_eq`
- ~~`std/fs`~~ — **Done** — File struct, openFile, createFile, stdin/stdout/stderr, read/write/seek/close
- ~~`std/os`~~ — **Done** — argsCount, argLen, argPtr, environCount, environLen, environPtr, exit
- ~~`std/time`~~ — **Done** — nanoTimestamp, milliTimestamp, timestamp, Timer struct
- ~~`std/random`~~ — **Done** — fillBytes, randomInt, randomRange (via getentropy)
- ~~Comptime target builtins~~ — **Done** — `@target_os()`, `@target_arch()`, `@target()` + const-fold if-expressions
- ~~Platform-conditional stdlib~~ — **Done** — `stdlib/fs.cot` uses `if @target_os() == "linux" { ... } else { ... }` for flags
- ~~`--target=wasm32-wasi` WASI imports~~ — **Done** — emits `wasi_snapshot_preview1` imports
- ~~High-level stdlib APIs~~ — **Done** — `arg(n)`, `environ(n)`, `readFile(path)`, `writeFile(path, data)`
- ~~StringBuilder~~ — **Done** — in `std/string`, append/appendByte/appendInt/toString
- ~~String methods~~ — **Done** — `std/string` with ~25 functions (split, trim, indexOf, contains, startsWith, endsWith, replace, etc.)
- ~~`std/math`~~ — **Done** — @abs/@ceil/@floor/@trunc/@round/@sqrt builtins + `std/math` stdlib (abs, min, max, clamp, ipow, fabs, ceil, floor, sqrt, etc.)
- ~~`std/json`~~ — **Done** — recursive descent parser + StringBuilder-based encoder, ported from Go encoding/json
- ~~`std/sort`~~ — **Done** — insertion sort + reverse for List(T)
- ~~Comptime Tier 2~~ — **Done** — `comptime { }` blocks, `@compileError`, dead branch elimination, local const propagation
**Carried to 0.4 (now done):**
- ~~`for key, value in map`~~ — **Done** — iterator protocol, shipped in Wave 1 (`0c5914e`)
- `std/fmt` — partially covered by string interpolation + StringBuilder

**Carried to 0.4 (remaining):**
- Multiple return values — `fn divmod(a, b: i64) (i64, i64)` — deferred, not blocking current programs

**Carried to 0.5+:**
- `weak` references — ARC cycle breaker → 0.5
- `std/dom` — browser DOM API → 0.5 (web framework)

**I/O implementation:** I/O functions are defined using the WASI interface design. The compiler emits:
- Native: libc calls (`read()`, `write()`, `socket()`) linked at compile time
- Browser Wasm: Web API imports (fetch, DOM) via import section
- WASI Wasm: WASI imports consumed by WASI-compatible runtimes

### 0.4: Make It Pleasant to Use — The Deno-Alternative Release

**Theme:** Cot becomes a credible alternative to Deno for building CLI tools, web servers, and full-stack applications. Like Deno, everything is built-in: formatter, test runner, LSP, and standard library. Unlike Deno, Cot compiles to native binaries with zero runtime overhead.

**Philosophy:** The version number is arbitrary. The goal is features and maturity — each wave makes Cot more capable and more correct. We ship 0.4 when the work is done, not on a deadline.

#### Wave 1: Language Completeness — DONE

All seven features shipped in `0c5914e` and `531415f`.

| # | Feature | Status | Commit |
|---|---------|--------|--------|
| 1 | `errdefer` | **DONE** | `0c5914e` — Zig AstGen pattern, LIFO ordering with mixed defer/errdefer |
| 2 | `if (optional) \|val\|` unwrap | **DONE** | `0c5914e` — Zig payload capture syntax, reuses catch capture infra |
| 3 | `for key, value in map` | **DONE** | `0c5914e` — Go range pattern, hash table iteration over occupied slots |
| 4 | Named error set switch | **DONE** | `0c5914e` — `catch \|err\| switch (err) { error.X => ... }` with global variant table |
| 5 | `switch` range prongs | **DONE** | Pre-existing — `1..10 => "digit"`, formatter fixed in `531415f` |
| 6 | Labeled blocks / labeled continue | **DONE** | `0c5914e` — `break :outer` / `continue :outer` in nested loops |
| 7 | Require parens for if/while | **DONE** | `531415f` — Zig pattern, disambiguates `\|` (bitwise OR) from `\|val\|` (capture) |

**Carried forward:** Multiple return values (`fn divmod(a, b: i64) (i64, i64)`) — deferred, not critical for current programs.

#### Wave 2: Developer Experience — DONE

All six features shipped in `531415f`.

| # | Feature | Status | Commit |
|---|---------|--------|--------|
| 8 | `cot fmt` | **DONE** | `531415f` — AST-based formatter with comment interleaving (Go gofmt pattern) |
| 9 | Rich error messages | **DONE** | `531415f` — span underlines (`^~~~`) with ANSI color on TTY |
| 10 | Test filtering | **DONE** | `531415f` — `cot test file.cot --filter="name"` (Zig --test-filter pattern) |
| 11 | LSP: autocomplete | **DONE** | `531415f` — builtins, scope symbols, keywords |
| 12 | LSP: rename symbol | **DONE** | `531415f` — WorkspaceEdit via references |
| 13 | LSP: find references | **DONE** | `531415f` — AST walk collecting identifier matches |

#### Wave 3: Compiler Maturity + Project System — DONE

Combines compiler-internal improvements (Wasm codegen cleanup) with user-facing project infrastructure. The Wasm work makes the compiler more correct and eliminates workarounds before building new stdlib modules on top.

**Compiler internals (from WASM_UPGRADE_PLAN):**

| # | Feature | Description | Risk | Effort | Reference |
|---|---------|-------------|------|--------|-----------|
| 14 | `memory.fill` wiring | **DONE** — Codegen handler added for `wasm_lowered_zero` op. Emits `memory.fill` (0xFC 0x0B). | Low | Hours | Go `cmd/compile/internal/wasm` |
| 15 | Multi-value return cleanup | **DEFERRED** — Reviewed: `compound_len_locals` works correctly. Go reference doesn't use multi-value returns at all. Cleanup is high risk, low reward. Defer to post-1.0. | High | — | — |

**User-facing features:**

| # | Feature | Description | Reference |
|---|---------|-------------|-----------|
| 16 | `cot.json` project manifest | **DONE** — `compiler/project.zig` reads cot.json via `std.json.parseFromSlice`. | `deno.json`, `package.json` |
| 17 | `cot init` | **DONE** — Creates `cot.json`, `src/main.cot`, `.gitignore`. | `deno init`, `cargo init` |
| 18 | `std/http` | **DONE** — TCP sockets (`@net_socket`, `@net_bind`, `@net_listen`, `@net_accept`, `@net_connect`, `@net_set_reuse_addr`), sockaddr_in construction, HTTP response builder. ARM64+x64 native overrides. 11 tests. | Go `net/http`, POSIX sockets |
| 19 | `std/url` | **DONE** — URL parsing (scheme, host, port, path, query, fragment). Heap-allocated pattern. 13 tests. | Go `net/url` |
| 20 | `std/encoding` | **DONE** — Base64 encode/decode, hex encode/decode, URL-safe base64. 26 tests. | Deno `std/encoding`, Go `encoding/` |
| 21 | Cross-file LSP intelligence | **DONE** — Goto-definition, find-references, and completions work across imported files. LSP resolves symbols from dependencies. | ZLS, rust-analyzer |
| 22 | `@safe` mode | **DONE** — Opt-in `@safe` file annotation enables C#/TS-friendly extensions: colon struct init, field shorthand, implicit self, constructor sugar (`new Type(args...)`), auto pointer wrapping. All gated — zero impact on base language. 22 tests. | C#, TypeScript |

#### Wave 4: Ecosystem Polish — DONE

| # | Feature | Status | Description | Reference |
|---|---------|--------|-------------|-----------|
| 23 | Tree-sitter grammar | **DONE** | Syntax highlighting in any editor (Neovim, Helix, Zed, GitHub). Zero errors across all 62 Cot files. | Zig tree-sitter-zig |
| 24 | `cot check` | **DONE** | Type-check without compiling — fast feedback loop. | `zig build check`, `deno check` |
| 25 | `cot lint` (basic) | **DONE** | Unused variables, unreachable code, shadowing warnings. Expanded rules: unused imports, empty blocks, shadowed variables. | `deno lint`, `zig` warnings |
| 26 | Improved `cot test` output | **DONE** | Colors, timing per test, `--verbose` flag, failure diffs. | Deno test output, Zig test output |
| 27 | Watch mode (`--watch`) | **DONE** | `cot run --watch`, `cot test --watch` — auto-restart on file changes via fsevents/inotify. | `deno run --watch` |
| 28 | `cot bench` | **DONE** | `bench "name" { }` blocks with Go-style adaptive calibration, timing output, ns/op. | `deno bench`, Zig `@Timer` |
| 29 | `cot task` | **DONE** | Run tasks defined in `cot.json` — like npm scripts but built-in. | `deno task`, npm scripts |

#### Wave 5: Language Maturity (ported from Zig)

Language features that make Cot a more capable and ergonomic language, ported from Zig's proven designs.

| # | Feature | Description | Reference |
|---|---------|-------------|-----------|
| 30 | Destructuring | **DONE** — `const a, b = getTuple()` and `const a, b, c = getTriple()`. New `DestructureStmt` AST node, tuple element extraction, compound type decomposition. | Zig 0.14 destructuring |
| 31 | Inferred error sets (`!T`) | **DONE** — `fn read() ![]u8` — compiler tracks exact error set. Parser handles `!T`, checker creates `ErrorUnionType` with inferred set. | Zig `!T` syntax |
| 32 | `noreturn` type | **DONE** — Bottom type for functions that never return (`@exit`, `@trap`). Coerces to any type. `noreturn` functions cannot return. | Zig `noreturn` |
| 33 | Doc comments (`///`) | **DONE** — Parse `///` comments above declarations, store in AST. | Zig `///`, Rust `///` |
| 34 | `cot doc` | **DONE** — Generate API documentation from doc comments. HTML output. | `deno doc`, `zig doc` |
| 35 | `@embedFile("path")` | **DONE** — Compile-time file embedding — returns `string`. Path relative to source file. 10MB limit. | Zig `@embedFile` |
| 36 | `@TypeOf(expr)` | Get the type of any expression at comptime. Needed for type-level programming. | Zig `@TypeOf` |
| 37 | `@hasField(T, "name")` | Comptime query: does struct T have this field? Enables generic serialization. | Zig `@hasField` |
| 38 | `@field(value, "name")` | Access struct field by comptime string. Key enabler for JSON derive, ORM, reflection. | Zig `@field` |
| 39 | `inline for` | Comptime loop unrolling — iterate struct fields at compile time. Combined with `@typeInfo`, enables derive-like patterns. | Zig `inline for` |
| 40 | Runtime safety (debug mode) | Integer overflow, array bounds, null unwrap, unreachable — panic in debug builds, unchecked in release. `cot build --release` disables checks. | Zig ReleaseSafe |
| 41 | Error set merge (`\|\|`) | `const AllErrors = FileError \|\| NetworkError` — compose error sets from multiple sources. | Zig error set merge |

#### Wave 6: Standard Library Expansion (ported from Deno)

Batteries-included standard library matching Deno's breadth. Every module is pure Cot, tested on both native and Wasm.

| # | Feature | Description | Reference |
|---|---------|-------------|-----------|
| 42 | `std/path` | **DONE** — `join`, `dirname`, `basename`, `extname`, `isAbsolute`, `relative`, `clean`. Ported from Go `path/filepath`. 38 tests. | Deno `@std/path`, Go `path/filepath` |
| 43 | `std/crypto` | **DONE** — SHA-256 (FIPS 180-4) + HMAC-SHA256 (RFC 2104). Pure Cot, no C deps. 17 tests including NIST vectors. | Deno `@std/crypto`, Go `crypto/sha256` |
| 44 | `std/regex` | Regular expression engine. NFA-based (Thompson's construction). `match`, `find`, `findAll`, `replace`, `split`. | Go `regexp`, Rust `regex` |
| 45 | `std/fmt` | **DONE** — ANSI colors (red/green/yellow/blue/magenta/cyan/gray/white), text styles (bold/dim/italic/underline/strikethrough), stripAnsi, formatBytes, formatDuration, padLeft/padRight/center, zeroPad, hex. 36 tests. | Deno `@std/fmt`, Go `fmt` |
| 46 | `std/log` | **DONE** — Structured logging with levels (debug/info/warn/logError). Configurable timestamps, level tags. Key-value pairs. 14 tests. | Deno `@std/log`, Go `log/slog` |
| 47 | `std/dotenv` | **DONE** — Parse `.env` files into key-value pairs. `parseEnv(text)`, `get`/`has`/`entryCount`/`entryKey`/`entryValue`. 12 tests. | Deno `@std/dotenv` |
| 48 | `std/cli` | **DONE** — Parse `--flag=value`, `-f value`, positional args, `--` separator. `getFlag`/`hasFlag`/`getFlagInt`/`positional`. 13 tests. | Deno `@std/cli`, Go `flag` |
| 49 | `std/uuid` | **DONE** — UUID v4 generation (random-based), `isValid`, `version`. Uses `@random` builtin. 10 tests. | Deno `@std/uuid` |
| 50 | `std/semver` | **DONE** — Parse, compare (`cmp`/`gt`/`gte`/`lt`/`lte`/`eq`), `format`, `incMajor`/`incMinor`/`incPatch`. 28 tests. | Deno `@std/semver` |
| 51 | `std/testing` | **DONE** — `assertContains`, `assertStartsWith`, `assertEndsWith`, `assertStrEq`, `assertGt`/`assertGte`/`assertLt`/`assertLte`, `assertInRange`, `assertTrue`/`assertFalse`, `assertEmpty`/`assertNotEmpty`/`assertLen`. 21 tests. | Deno `@std/testing`, Zig testing |
| 52 | `std/process` | Subprocess spawning: `exec("ls", ["-la"])` → output string. Pipe stdin/stdout. Exit code. | Deno `Deno.Command`, Go `os/exec` |

#### Wave 5 (legacy): Production Capabilities

| # | Feature | Status | Reference |
|---|---------|--------|-----------|
| 53 | `async fn` / `await` | **DONE** — Dual backend: Wasm state machine (Rust) + native eager eval (Zig). `try await` for error unions across await points. 18 tests. | Rust coroutine.rs, Zig async |
| 54 | Native event loop | **DONE** — 9 builtins (kqueue/epoll/fcntl), `std/async` with platform-abstracted API, async I/O wrappers (asyncAccept/Read/Write/Connect). 14 tests. | Go netpoll, Zig Kqueue.zig |
| 55 | Browser async | JS Promise interop for `async fn` on `--target=wasm32`. | wasm-bindgen futures |
| 56 | IR split (`lower_clif.zig`) | **NOT NEEDED** — Async implemented within Wasm-first architecture. Deferred indefinitely. | — |

#### Deferred Wasm Work (post-0.4)

| Feature | Why deferred | Target |
|---------|-------------|--------|
| WasmGC arrays + nested structs | Only matters for `--target=wasm32-gc` (browser story, 0.5+) | 0.5 |
| `call_ref` typed function refs | `call_indirect` works fine, `call_ref` is a perf optimization | 0.5 |
| `throw`/`try_table` exceptions | ABI-breaking, 1-2 weeks, highest risk. Current error unions work. | 0.5 |

#### Deno Feature Parity Comparison

| Deno Feature | Cot Status | Notes |
|--------------|-----------|-------|
| `deno run` | `cot run` | Done |
| `deno test` | `cot test --filter` | Done |
| `deno fmt` | `cot fmt` | Done |
| `deno lint` | `cot lint` | Done |
| `deno check` | `cot check` | Done |
| `deno bench` | `cot bench` | Done |
| `deno doc` | `cot doc` | Done |
| `deno init` | `cot init` | Done |
| `deno task` | `cot task` | Done |
| `deno.json` | `cot.json` | Done |
| `deno compile` | `cot build` | Done (native binary, no runtime) |
| `deno serve` | `std/http` | Done |
| LSP | autocomplete, rename, references, cross-file | Done |
| TypeScript types | Cot types (stronger, compiled) | Done |
| Single binary | `cot` binary | Done |
| Edge deploy | `--target=wasm32-wasi` | Done |
| Watch mode | `--watch` | Done |
| Permissions | — | 0.6 |
| Test coverage | — | 0.5 |

**What Cot has that Deno doesn't:**
- AOT compilation to native binary (no V8 runtime, no cold starts)
- ARC memory management (no GC pauses)
- Wasm as first-class browser target (not just server-side)
- ARM64/x64 native output from the same source
- MCP server for AI-assisted development (already built in Cot itself)
- No node_modules, no package.json, no transpilation step

#### Zig Feature Parity Comparison

| Zig Feature | Cot Status | Notes |
|-------------|-----------|-------|
| Structs + methods | Done | Cot has `impl` blocks |
| Enums + tagged unions | Done | Zig pattern |
| Optionals (`?T`) | Done | `??` (orelse), `.?`, if-unwrap |
| Error unions (`!T`) | Done | Named sets + inferred `!T` |
| `try` / `catch` / `errdefer` | Done | |
| Generics (comptime T) | Done | Cot uses `fn(T)` syntax, monomorphized |
| Traits / interfaces | Done | Cot has explicit `trait` keyword (Zig uses duck-typing) |
| `defer` / `errdefer` LIFO | Done | |
| Test blocks | Done | `test "name" { }` |
| Slices `[]T` | Done | |
| Comptime blocks | Done | Dead branch elimination, `@compileError` |
| `@target_os()` / `@target_arch()` | Done | |
| Labeled blocks / break / continue | Done | |
| Switch with ranges | Done | |
| `@sizeOf` / `@alignOf` / casts | Done | |
| `@embedFile` | Done | Compile-time file embedding |
| Destructuring | Done | `const a, b = getTuple()` |
| `@TypeOf` / `@hasField` / `@field` | — | Wave 5 |
| `inline for` | — | Wave 5 |
| Inferred error sets (`!T`) | Done | `fn read() ![]u8` |
| Runtime safety (debug mode) | — | Wave 5 |
| `noreturn` type | Done | Bottom type for @exit, @trap |
| Doc comments (`///`) | Done | `///` parsed, stored in AST, used by `cot doc` |
| Packed structs / bitfields | — | 0.5 |
| Wrapping arithmetic (`+%`) | — | 0.5 |
| SIMD vectors | — | 0.6+ |
| Sentinel-terminated types | — | 0.6+ (FFI story) |
| Async/await (removed from Zig) | Done | Cot has it, Zig removed it |
| Closures | Done | Zig doesn't have closures |
| String interpolation | Done | Zig doesn't have it |
| ARC memory management | Done | Zig requires manual memory |
| Traits (explicit) | Done | Zig uses duck-typing only |

#### 0.4 Success Criteria

A developer should be able to:
1. `cot init myapp` → scaffold a project
2. Write an HTTP server that serves JSON API endpoints with `std/http`
3. `cot fmt` → auto-format their code
4. `cot test --filter "api"` → run targeted tests
5. `cot build` → get a native binary with zero dependencies
6. `cot check` → fast type-checking without full compilation
7. `cot lint` → catch unused variables and unreachable code
8. `cot bench` → benchmark critical paths
9. `cot doc` → generate API documentation
10. Use `std/crypto` for auth tokens, `std/regex` for validation, `std/path` for file paths
11. Get autocomplete, goto-def, and references across imported files
12. See clear error messages with source spans and ANSI colors
13. Write `--watch` during development for auto-restart

#### Progress

- **Wave 1 (language):** 7/7 done
- **Wave 2 (DX):** 6/6 done
- **Wave 3 (maturity + project system + DX):** 8/9 done (multi-value cleanup deferred)
- **Wave 4 (ecosystem polish):** 7/7 done
- **Wave 5 (language maturity — Zig ports):** 6/12 (doc comments, cot doc, noreturn, !T, destructuring, @embedFile done)
- **Wave 6 (stdlib expansion — Deno ports):** 9/11 (path, uuid, semver, dotenv, crypto, fmt, log, cli, testing done)
- **Wave 5-legacy (production):** 2/4 (async/await, event loop done; browser async, IR split remaining)

---

### 0.5: Make It Community-Ready

The package manager + web framework release. Cot becomes something you can build real products with and share libraries.

#### Language Features (ported from Zig)

| Feature | Description | Reference |
|---------|-------------|-----------|
| Packed structs / bitfields | `packed struct { flags: u3, mode: u2 }` — bit-level layout, backed by integer. For binary protocols, network headers, flags. | Zig `packed struct` |
| Wrapping/saturating arithmetic | `+%` (wrapping), `+\|` (saturating) — explicit overflow behavior. | Zig `+%`, `+\|` |
| `for` multi-sequence | `for (a, b) \|x, y\|` — iterate multiple collections in lockstep. | Zig multi-object for |
| `while` continue expression | `while (i < n) : (i += 1) { }` — elegant loop increment. | Zig while continue |
| Non-exhaustive enums | `enum(u8) { A, B, _ }` — allows values outside the defined set. For forward-compatible protocols. | Zig non-exhaustive |
| `weak` references | ARC cycle breaker. `weak var ref: ?*Node = null` — doesn't prevent deallocation. | Swift `weak` |

#### Ecosystem

| Feature | Description | Reference |
|---------|-------------|-----------|
| Package manager | `cot add <pkg>`, `cot remove <pkg>`, `cot publish`. Lockfile, dependency resolution, version constraints. | `deno add`, `cargo add` |
| Package registry | cot.land — browse, search, publish packages. Integrity hashes. | jsr.io, crates.io |
| Cross-compilation | `cot build --target=x86_64-linux`, `--target=aarch64-linux`. Emit native binary for different OS/arch. | Zig cross-compile, `deno compile --target` |
| Test coverage | `cot coverage` — line/branch coverage, lcov output, HTML report. | `deno coverage` |
| Signal handling | `@onSignal(SIGINT, handler)` — respond to OS signals. For graceful shutdown. | Deno `Deno.addSignalListener` |

#### Framework & Database

| Feature | Description | Reference |
|---------|-------------|-----------|
| `std/db` | Database driver — connect, query, parameterized statements. SQLite first, then Postgres. | Go `database/sql` |
| `std/dom` | Browser DOM API for `--target=wasm32`. Element creation, event handling, attribute manipulation. | wasm-bindgen, Emscripten |
| Web framework prototype | `@server`/`@client` annotations. Shared types across boundary. Auto-generated serialization + HTTP transport. File-based routing. | Next.js, Fresh, SvelteKit |
| Browser async | JS Promise interop for `async fn` on `--target=wasm32`. | wasm-bindgen futures |

#### More Standard Library

| Feature | Description | Reference |
|---------|-------------|-----------|
| `std/csv` | CSV reading/writing with field quoting, custom delimiters. | Deno `@std/csv` |
| `std/toml` | TOML parsing and serialization. For config files. | Deno `@std/toml` |
| `std/streams` | Streaming data: `ReadableStream`, `WritableStream`, `TransformStream`, pipe, buffer. | Deno `@std/streams`, WHATWG Streams |
| `std/net` | Higher-level networking: DNS resolution, TLS, connection pooling. | Go `net`, Deno `Deno.connect` |

---

### 0.6: Production Hardening

The release where Cot becomes safe and observable enough for production services.

| Feature | Description | Reference |
|---------|-------------|-----------|
| Spawn + channels | Go-style concurrency: `spawn { }` blocks, `Channel(T)` with send/recv/close, `select` statement. Work-stealing scheduler. See `claude/CONCURRENCY_DESIGN.md`. | Go goroutines/channels |
| Atomic ARC | Thread-safe reference counting for concurrent programs. | Swift atomic refcounting |
| Permission system | Sandboxed execution: `--allow-read`, `--allow-net`, `--allow-env`. Deny overrides. Config-based permissions in `cot.json`. | Deno permissions |
| OpenTelemetry | Built-in tracing: auto-instrument HTTP servers, fetch calls. Export to OTLP collector. | Deno 2.2 OTel |
| `std/sync` | `Mutex`, `RwLock`, `Atomic(T)`, `WaitGroup`, `Once` — low-level concurrency primitives. See `claude/CONCURRENCY_DESIGN.md`. | Go `sync`, Zig `std.Thread` |
| SIMD vectors | `@Vector(N, T)` mapped to hardware SIMD. Element-wise arithmetic, `@shuffle`, `@reduce`. | Zig `@Vector` |
| Sentinel-terminated types | `[:0]u8` for null-terminated strings, `[*:0]const u8` for C interop. Sentinel in the type. | Zig sentinel types |
| Subprocess management | `Process.spawn("cmd", args)` — pipe stdin/stdout/stderr, wait, kill. | Deno `Deno.Command` |

#### More Standard Library

| Feature | Description |
|---------|-------------|
| `std/yaml` | YAML parsing and serialization |
| `std/msgpack` | MessagePack binary encoding |
| `std/tar` | Tar archive streaming (read/write) |
| `std/html` | HTML entity escaping/unescaping |

---

### 1.0: Public Release

The stability release. Language syntax is frozen. Code that compiles today will compile tomorrow.

| Feature | Description |
|---------|-------------|
| Language specification | Formal syntax and semantics document. Syntax frozen at 1.0. |
| Language guide | Tutorial-style guide: from "Hello World" to building a web app. |
| Standard library API docs | Complete documentation for every stdlib function. Generated via `cot doc`. |
| Example applications | TODO app, chat server, blog engine, CLI tool — each demonstrating different Cot strengths. |
| cot.dev website | Interactive playground (compile + run Cot in the browser). Package search. Docs. |
| Stability commitment | Semver. Deprecation policy. Migration guides between versions. |
| Backwards compatibility | 1.x releases add features but don't break existing code. |
| `cot upgrade` | Self-update to latest version. |

---

## Competitive Positioning at 1.0

| Language | Strength | Weakness Cot Exploits |
|----------|----------|----------------------|
| **TypeScript + Next.js** | Full-stack ecosystem, familiarity | Not compiled, GC, weak types, node_modules bloat |
| **Go** | Simplicity, concurrency | No browser target, GC, backend only |
| **Rust** | Performance, safety | Complexity, learning curve, Wasm = afterthought |
| **Swift** | ARC, developer experience | No Wasm, Apple-centric |
| **Zig** | Performance, simplicity | Manual memory, no closures, no full-stack story |
| **Deno** | DX, TypeScript, batteries-included | V8 runtime overhead, GC, not truly compiled |

**Cot's unique position:** The only language where:
- Server = native binary (no runtime, no cold starts)
- Browser = Wasm (first-class target, same source)
- Types and logic are shared across the client/server boundary
- The framework handles compilation targets transparently
- Memory is automatic (ARC) but predictable (no GC pauses)
- Everything is built-in: fmt, lint, test, bench, doc, LSP — one binary

**What Cot takes from each:**

| Source | What we port | What we skip |
|--------|-------------|-------------|
| **Zig** | Comptime, error unions, defer, type reflection, runtime safety, test blocks, packed structs | Manual memory, no closures, no string interp, no async |
| **Deno** | Built-in toolchain (fmt/lint/test/bench/doc), batteries-included stdlib, watch mode, permissions, web APIs | V8 runtime, JavaScript, npm baggage, node_modules |
| **Go** | Spawn + channels, goroutine-style concurrency, simple error handling, fast compilation | GC, no generics (until recently), no Wasm browser target |
| **Rust** | Trait system (simplified), pattern matching, error propagation (`try`), ARC model | Borrow checker complexity, lifetime annotations, steep learning curve |
| **Swift** | ARC semantics, optional handling, clean syntax | Apple lock-in, no Wasm, heavyweight runtime |

---

## Dogfooding: MCP Server in Cot (COMPLETE)

The MCP server was the first real program written in Cot — proving the language can build useful tools.

**Location:** `mcp/cot-mcp.cot` (~380 lines, single file)
**Config:** `.mcp.json` at repo root
**Build:** `cot build mcp/cot-mcp.cot -o cot-mcp`
**Protocol:** JSON-RPC 2.0 over stdio, newline-delimited

### What It Provides

Three tools available to Claude Code:
1. **`get_syntax_reference`** — Complete Cot language syntax reference
2. **`get_stdlib_docs`** — Function signatures for any stdlib module (json, string, fs, io, list, map, os, time, random, sort, set, math)
3. **`get_project_info`** — Build/test commands and project structure

### What It Uses

- `std/json` — parse JSON-RPC requests, encode responses
- `std/string` — StringBuilder for building tool text
- `std/io` — BufferedReader/Writer for efficient stdio
- Error unions, generics, closures — real language features in production

### Success

All three success criteria met: Claude Code writes valid Cot on first attempt, looks up syntax without guessing, and gets stdlib function signatures on demand.

---

## Open Questions

These don't need answers now, but should be resolved before 1.0:

1. ~~**Async model:**~~ **Answered** — async/await with dual backend.
2. **Module system:** File-based (Go) or explicit exports (Rust/Zig)?
3. ~~**Concurrency:**~~ **Answered** — spawn + channels. See `claude/CONCURRENCY_DESIGN.md`.
4. **FFI beyond C:** Interop with JS npm packages? Rust crates?
5. ~~**Standard library scope:**~~ **Answered** — Batteries-included (Deno model). 20+ stdlib modules by 0.5.
6. **Governance:** BDFL, RFC process, or foundation?

---

## Summary

Cot 0.3 built the hard infrastructure — a complete compiler pipeline with dual-target output, ARC memory management, generics, closures, and 1000+ passing tests. The MCP server (written in Cot) proves the language works for real tools. 0.4 Waves 1-3 are done.

Remaining 0.4 work is massive and intentional — three new waves (ecosystem polish, language maturity from Zig, stdlib expansion from Deno) that together make Cot a credible alternative to Deno for building real applications. With AI-assisted development, this is achievable at a pace that would be impossible for a traditional single-developer project.

The road to 1.0:

1. **0.3 (COMPLETE):** Language core, type system, stdlib, I/O, MCP server — Cot is a real language
2. **0.4 (IN PROGRESS):** 7 waves total. Language maturity (Zig ports), stdlib expansion (Deno ports), tooling (check, lint, bench, doc, watch). The "build real things" release.
3. **0.5:** Package manager, web framework, database, cross-compilation — the "share and deploy" release
4. **0.6:** Concurrency (spawn/channels), permissions, observability — the "production" release
5. **1.0:** Specification, docs, stability, playground — the "public" release

**0.4's goal: A developer can `cot init`, write a server with crypto + regex + path handling, `cot test --watch` during development, `cot lint` + `cot check` for fast feedback, `cot bench` for performance, `cot doc` for API docs, and `cot build` for a native binary. Like Deno, but compiled to native with zero runtime overhead.**
