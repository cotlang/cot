# TypeScript Native — Architecture Plan

**Vision:** Compile TypeScript and JavaScript to native binaries using Cot's compiler pipeline. No V8, no JIT, no garbage collector. ARC memory management. Same backend as Cot — ARM64, x64, Wasm.

**Pitch:** "Run TypeScript like Rust. Ship a static binary. Zero runtime."

---

## Architecture

libts is a **pre-processor** that transforms TypeScript AST into Cot AST in memory, then hands it to libcot's existing checker and lowerer. No duplication of compiler logic. No intermediate `.cot` files on disk.

```
.cot  ──→  libcot scanner+parser  ──→  Cot AST  ──┐
                                                     ├──→  libcot checker+lowerer  ──→  CIR  ──→  native/Wasm
.ts   ──→  libts scanner+parser   ──→  TS AST  ──→  libts transform  ──→  Cot AST  ──┘
.js   ──→  (same as .ts, all types inferred)
```

Both paths produce the same Cot AST. The checker and lowerer are reused entirely — libts is ~5-8K lines, not a whole compiler. A project can mix `.cot` and `.ts` files with zero interop cost.

### Two Standard Libraries

```
stdlib/          Cot standard library (existing, Zig-style syntax)
  sys.cot          Low-level: alloc, dealloc, fd_write, exit, time
  fs.cot           File I/O: openFile, readFile, writeFile
  http.cot         TCP sockets, HTTP response builder
  list.cot         List(T) with ~20 methods
  map.cot          Map(K,V) with splitmix64 hash
  string.cot       25 string functions + StringBuilder
  path.cot         join, dirname, basename, extname
  json.cot         JSON parse + encode
  crypto.cot       SHA-256, HMAC-SHA256
  os.cot           exit, arg, environ
  process.cot      run, output (fork/exec)
  ...              (34 modules total)

stdlib/          TypeScript standard library (Node-compatible API, TS syntax)
  fs.ts            readFile, writeFile, readdir, stat, mkdir, rm
  path.ts          join, resolve, dirname, basename, extname, parse
  http.ts          createServer, request, Server, IncomingMessage, ServerResponse
  https.ts         TLS wrapper (future)
  crypto.ts        createHash, randomBytes, randomUUID
  buffer.ts        Buffer class (alloc, from, toString, slice, concat)
  console.ts       log, error, warn, info, debug, time, timeEnd
  process.ts       env, argv, exit, cwd, pid, platform, arch
  events.ts        EventEmitter (on, emit, once, off, removeListener)
  stream.ts        Readable, Writable, Transform, pipeline
  url.ts           URL class, URLSearchParams
  util.ts          promisify, inspect, format, types
  child_process.ts spawn, exec, execSync, fork
  os.ts            platform, arch, cpus, hostname, tmpdir, homedir
  timers.ts        setTimeout, setInterval, clearTimeout, clearInterval
  net.ts           Socket, Server, createConnection
  assert.ts        ok, equal, deepEqual, throws
  querystring.ts   parse, stringify
  zlib.ts          gzip, gunzip, deflate, inflate (future)
  worker_threads.ts Worker, parentPort, workerData (future)
```

Each `stdlib/*.ts` file is written in TypeScript, compiled by libts, and calls into `stdlib/*.cot` or CIR primitives for the actual implementation. TypeScript developers import from familiar names:

```typescript
import { readFile } from 'fs'        // resolves to stdlib/fs.ts
import { join } from 'path'          // resolves to stdlib/path.ts
```

### Import Resolution

```
import 'fs'           → stdlib/fs.ts (Node built-in)
import 'std/fs'       → stdlib/fs.cot (Cot stdlib, available from TS too)
import './foo'         → ./foo.ts or ./foo.js (relative)
import 'zod'          → node_modules/zod/index.ts (package)
```

---

## libts — TypeScript/JavaScript Frontend

### Scope: Modern JS/TS (strict mode, ESM)

**Supported (covers ~99% of real-world code):**
- `const`, `let`, `var` (var treated as let — no hoisting across blocks)
- Arrow functions, regular functions, async/await
- Classes (→ structs with ARC), constructors, methods, static methods
- Interfaces (→ traits), type aliases, enums
- Generics `<T>` (→ Cot generics `(T)`)
- Object literals, destructuring (objects and arrays), spread
- Template literals (→ string interpolation)
- `for...of`, `for...in` (objects only), `while`, `do...while`
- Optional chaining `?.` (→ Cot `?.`)
- Nullish coalescing `??` (→ Cot `orelse`)
- `try/catch/finally` (→ error unions + defer)
- `Promise<T>` (→ `Task(T)`), `async/await`
- ESM `import/export`
- `typeof`, `instanceof` (compile-time where possible)
- `Array<T>`, `Map<K,V>`, `Set<T>`, `WeakMap`, `WeakSet`
- `JSON.parse()`, `JSON.stringify()`
- `RegExp` (→ `std/regex`)
- String methods (→ `std/string`)
- `console.log/error/warn`
- Type assertions `as T`, non-null assertion `!`
- Union types `A | B` (→ tagged unions)
- Intersection types `A & B` (→ struct merge)
- Literal types, discriminated unions
- `keyof`, `typeof` in type position
- Mapped types (subset)
- Conditional types (subset)

**Not supported (legacy/dynamic):**
- `eval()`, `Function()` constructor
- `with` statement
- `arguments` object (use rest params)
- `Proxy`, `Reflect`
- `Symbol.toPrimitive` and exotic object protocols
- Sloppy mode `this` binding
- CommonJS `require()` (use ESM)
- Dynamic `import()` expressions (future consideration)
- Decorators (future — wait for TC39 Stage 4 to stabilize)
- `globalThis` mutation

### Type Mapping

| TypeScript | CIR / Cot | Notes |
|-----------|-----------|-------|
| `number` | `f64` | IEEE 754, same as V8 |
| `bigint` | `i64` | Narrower than spec but covers real usage |
| `string` | `string` (ptr+len, ARC) | Immutable, UTF-8 |
| `boolean` | `bool` | |
| `null` | `null` (optional) | `T \| null` → `?T` |
| `undefined` | `null` (optional) | Unified with null at CIR level |
| `void` | `void` | |
| `never` | `noreturn` | |
| `any` | tagged union (all types) | Performance warning in strict mode |
| `unknown` | tagged union (all types) | Requires type narrowing before use |
| `object` | `*ArcObject` | ARC heap allocation |
| `Array<T>` | `List(T)` | COW, ARC managed |
| `Map<K,V>` | `Map(K,V)` | COW, ARC managed |
| `Set<T>` | `Set(T)` | |
| `Promise<T>` | `Task(T)` / async fn return | |
| `Date` | `Instant` | |
| `RegExp` | `Regex` | Thompson NFA |
| `Error` | error union | `throw` → `return error.X` |
| `Buffer` | `[]u8` | |
| `T \| U` | `union { t: T, u: U }` | Tagged union, pattern matched |
| `T & U` | merged struct | Field union |
| `interface Foo` | `trait Foo` | Structural subtyping |
| `class Foo` | `struct Foo` + ARC | Heap allocated via `new` |
| `enum` | `const E = enum { ... }` | Numeric or string enums |
| `T[]` | `List(T)` | |
| `[T, U]` | `(T, U)` | Tuple |
| `Record<K,V>` | `Map(K,V)` | |
| `Partial<T>` | All fields `?T` | Compile-time transform |
| `Required<T>` | All fields non-optional | Compile-time transform |
| `Readonly<T>` | `const` fields | |

### Error Handling Mapping

TypeScript uses `throw/try/catch`. Cot uses error unions. The mapping:

```typescript
// TypeScript
function parse(input: string): Config {
  if (!input) throw new Error('empty input')
  return JSON.parse(input)
}

try {
  const config = parse(data)
} catch (e) {
  console.error(e.message)
}
```

Compiles to CIR equivalent of:

```zig
fn parse(input: string) !Config {
    if (input.len == 0) return error.EmptyInput
    return json.parse(input)
}

const config = parse(data) catch |err| {
    eprintln(err.message)
}
```

The `throw` → `return error`, `try/catch` → `catch` mapping is mechanical. Error types are inferred from throw sites (like Zig's inferred error sets).

### Class → Struct Mapping

```typescript
class User {
  name: string
  age: number

  constructor(name: string, age: number) {
    this.name = name
    this.age = age
  }

  greet(): string {
    return `Hello, I'm ${this.name}`
  }
}

const user = new User('Alice', 30)
console.log(user.greet())
```

Compiles to CIR equivalent of:

```zig
struct User { name: string, age: f64 }

fn User_init(name: string, age: f64) *User {
    return new User { name: name, age: age }
}

fn User_greet(self: *User) string {
    return "Hello, I'm " ++ self.name
}

const user = User_init("Alice", 30)
println(User_greet(user))
```

Classes are always heap-allocated (ARC). `this` → `self`. Methods → static dispatch. No prototype chain.

### Async Mapping

```typescript
async function fetchUser(id: number): Promise<User> {
  const response = await fetch(`/api/users/${id}`)
  const data = await response.json()
  return new User(data.name, data.age)
}
```

Compiles to CIR equivalent of:

```zig
async fn fetchUser(id: f64) User {
    const response = await fetch("/api/users/${id}")
    const data = await response.json()
    return User_init(data.name, data.age)
}
```

`Promise<T>` maps directly to async function return. `await` maps 1:1. The cooperative executor handles scheduling — same infrastructure as Cot's concurrency system.

---

## Frontend Structure

libts is a **pre-processor**, not a full compiler. It has no checker, no lowerer — those are in libcot and reused via `cot_compile_ast()`.

```
src/
  libts/                   TypeScript pre-processor (~5-8K lines)
    scanner.zig              JS/TS lexer (template literals, `?.`, `??`, `=>`, `<T>`)
    parser.zig               JS/TS parser (ESM, strict mode, TS type annotations)
    transform.zig            TS AST → Cot AST in-memory (class→struct, interface→trait,
                               Promise→async, throw→error union, T|U→tagged union)
    source_map.zig           TS span → Cot AST span tracking (errors point to .ts lines)
    tsconfig.zig             tsconfig.json loader (compilerOptions mapping)
    node_resolve.zig         Node module resolution (node_modules, package.json)

  libcot/                  Cot frontend (existing, unchanged)
    ...                      cot_compile_ast() entry point receives Cot AST from libts
                             Checker and lowerer run on the transformed AST — no duplication
```

### New C ABI entry point on libcot

```c
// cot.h — new entry point for pre-built ASTs
CirModuleRef cot_compile_ast(CotCompilerRef c, CotAst* ast);
```

libts calls this instead of `cot_compile(source_text)`. The checker and lowerer receive a Cot AST and don't know it originated from TypeScript.

### Key transform rules (transform.zig)

| TypeScript | Cot AST output | Notes |
|-----------|----------------|-------|
| `class Foo { ... }` | `struct Foo { ... }` + `new` + methods | Heap-allocated, ARC |
| `interface Bar { ... }` | `trait Bar { ... }` | Auto-generate `impl` for matching structs |
| `foo(x: number): string` | `fn foo(x: f64) string` | Type annotation mapping |
| `async function f()` | `async fn f()` | Direct mapping |
| `throw new Error(msg)` | `return error.X` | Error union |
| `try { } catch (e) { }` | `x catch \|err\| { }` | Error handling |
| `x?.foo` | `x?.foo` | Already identical |
| `x ?? y` | `x orelse y` | Already equivalent |
| `` `hello ${name}` `` | `"hello ${name}"` | String interpolation |
| `const f = (x) => x * 2` | `const f = fn(x: f64) f64 { return x * 2 }` | Closure |
| `T \| U` | `union { t: T, u: U }` | Tagged union |
| `T[]` / `Array<T>` | `List(T)` | |
| `Map<K,V>` | `Map(K,V)` | |
| `Promise<T>` | async fn return | |
| `null` / `undefined` | `null` (optional `?T`) | Unified |

### Module Resolution (Node algorithm)

```
import 'foo'
  1. Check stdlib/foo.ts (Node built-in)
  2. Check node_modules/foo/package.json → "main" or "exports"
  3. Check node_modules/foo/index.ts
  4. Check node_modules/foo/index.js
  5. Walk up parent directories repeating 2-4
```

### tsconfig.json Support

```json
{
  "compilerOptions": {
    "strict": true,           // → strict type checking in libts
    "target": "native",       // → ignored (always native/wasm)
    "module": "ESNext",       // → only ESM supported
    "outDir": "./dist",       // → -o flag
    "rootDir": "./src"        // → source root
  }
}
```

Most tsconfig options become no-ops (they control JS emit, which we don't have). `strict` controls type checking strictness. `paths` controls import aliases.

---

## CLI Integration

```bash
cot build app.ts                    # Compile TS to native binary
cot build app.ts --target=wasm      # Compile TS to Wasm
cot run app.ts                      # Compile + run
cot test app.test.ts                # Compile + run tests
cot check app.ts                    # Type-check only (like tsc --noEmit)

cot build app.cot                   # Cot source (existing)
cot build src/                      # Mixed .cot + .ts project
```

Frontend selection is automatic based on file extension:
- `.cot` → libcot
- `.ts` → libts
- `.js` → libts (all types inferred as `any` equivalent)

A `cot.json` or `tsconfig.json` can set project-level defaults.

---

## Implementation Phases

**Prerequisite:** libcot restructuring complete (src/libcot-zig with `cot_compile_ast()` entry point).

### Phase 1: TS Scanner + Parser + Basic Transform (4-6 weeks)
- `scanner.zig`: Lex JS/TS tokens (superset of Cot — add `?.`, `??`, `=>`, `<T>`, `:` type annotations)
- `parser.zig`: Parse ESM + strict mode JS/TS into TS AST
- `transform.zig`: Basic TS AST → Cot AST transform (functions, variables, classes, basic types)
- `source_map.zig`: Span tracking so errors point to `.ts` lines
- No structural subtyping yet — just direct type mapping (`number`→`f64`, `string`→`string`)
- **Gate:** `cot run hello.ts` where hello.ts is `console.log("hello world")` produces a native binary

### Phase 2: Full Transform + Structural Subtyping (4-6 weeks)
- Complete transform rules: interfaces→traits, union types→tagged unions, throw→error unions
- Auto-generate `impl Trait for Struct` when a class satisfies an interface structurally
- Generic `<T>` → Cot `(T)` mapping
- `async/await` + `Promise<T>` → Cot async functions
- `any`/`unknown` → tagged union with runtime dispatch
- **Gate:** Real-world TS files with types compile and run correctly

### Phase 3: Node-Compatible Standard Library (4-6 weeks)
- `stdlib/*.ts` files implementing Node API surface
- `fs`, `path`, `http`, `crypto`, `console`, `process`, `events`, `stream`, `url`, `buffer`
- Each `.ts` stdlib file compiled by libts, calls into existing Cot stdlib
- **Gate:** A basic HTTP server compiles and serves requests

### Phase 4: npm Package Compatibility (4-6 weeks)
- `node_resolve.zig`: `node_modules` resolution, `package.json` reading
- `tsconfig.zig`: `tsconfig.json` loader
- Compile pure-TS npm packages from source, cache compiled CIR in `~/.cot/packages/`
- **Gate:** A project using `zod` for validation compiles and runs

### Phase 5: Compliance + Polish (ongoing)
- Run Test262 strict mode subset, track pass rate toward 95%+
- Run TypeScript conformance tests, track toward 90%+
- Run Node.js API tests for implemented modules
- Optimize tagged union dispatch for `any`/`unknown`
- Tree shaking for unused stdlib
- `cot init --template=ts` for new TS projects

---

## Competitive Positioning

| | V8 (Node/Bun/Deno) | Cot TS Native |
|---|---|---|
| Runtime | 50MB+ JS engine | Static binary (~1-5MB) |
| Memory | GC (pauses) | ARC (deterministic) |
| Startup | JIT warmup | Instant (native) |
| Peak perf | Good (after warmup) | Better (AOT compiled) |
| Deployment | Install Node/Bun | Copy single binary |
| Docker image | 100MB+ | 5-10MB (FROM scratch) |
| Cold start (serverless) | 50-200ms | <1ms |
| npm compat | Full | Pure TS packages only |
| Dynamic eval | Yes | No |
| Type safety | Runtime type errors | Compile-time type errors |

The value proposition is clearest for:
- **Serverless/edge:** Cold start <1ms vs 50-200ms. Tiny binary. No runtime.
- **CLI tools:** Ship one binary. No `npm install`. No Node dependency.
- **Microservices:** 5MB Docker image vs 100MB. Deterministic memory. No GC pauses.
- **Embedded/IoT:** Static binary runs anywhere. No interpreter overhead.

---

## Package Management

### Global Package Cache

```
~/.cot/packages/
  zod@3.22.4/
    src/              ← downloaded TS source from npm registry
    lib.cir           ← compiled CIR (cached, reused across projects)
  drizzle-orm@0.30.1/
    src/
    lib.cir
  hono@4.2.0/
    src/
    lib.cir
```

**No `node_modules` in your project.** Packages live in a global cache, compiled once, shared across all projects. Same model as Cargo (`~/.cargo/registry`), Go (`~/go/pkg/mod`), and Zig (`~/.cache/zig`).

### CLI

```bash
cot add zod                       # Download from npm, store in ~/.cot/packages/
cot add hono@4.2                  # Pin version
cot remove zod                    # Remove from project (source stays in cache)
cot install                       # Install all deps from cot.lock
cot update                        # Update to latest compatible versions
```

### How It Works

1. `cot add zod` reads npm registry, downloads tarball, extracts to `~/.cot/packages/zod@3.22.4/src/`
2. On first `import 'zod'`, libts compiles the source to CIR, caches as `lib.cir`
3. Subsequent imports across any project read cached CIR — no re-parsing, no re-checking
4. `cot.lock` pins exact versions. `cot.json` lists dependencies with semver ranges.

### What Compiles

- **Pure TypeScript packages:** Full support. Zod, tRPC types, Drizzle ORM, date-fns, lodash-es, etc.
- **Packages with JS + types:** Support. The `.js` source compiles with inferred types, `.d.ts` provides type info.
- **Packages with native addons:** Not supported. Anything using `node-gyp`, N-API, or V8 C++ bindings won't compile.
- **Packages using `eval`/`Function()`:** Not supported. Dynamic code generation requires a runtime.

The npm ecosystem is large enough that pure-TS packages cover most use cases: validation (Zod), HTTP (Hono), ORM (Drizzle, Prisma types), auth (jose), encoding (base64, msgpack), testing (vitest types), CLI (commander), and utilities (lodash-es, date-fns, nanoid).

---

## Compliance Testing

### Test262 — JavaScript Conformance (~50,000 tests)

**Repository:** `github.com/tc39/test262`
**Maintained by:** TC39 (ECMAScript standards body)
**Used by:** V8, SpiderMonkey, JavaScriptCore, Hermes, engine262

The official test suite for JavaScript engine compliance. Every JS runtime runs it. Tests are organized by spec section (Chapter 6: Types, Chapter 13: ECMAScript Language: Statements, etc.) with precise coverage of every language feature.

**Our approach — two tiers:**

| Tier | Tests | Coverage | Target |
|------|-------|----------|--------|
| Supported subset | ~35,000-40,000 | Strict mode, ESM, classes, async/await, iterators, destructuring, Map/Set/Array, Promise, generators, for-of, spread, rest, template literals, optional chaining, nullish coalescing, numeric separators, logical assignment, private fields | **95%+ pass rate** |
| Excluded | ~10,000-15,000 | `eval`, `Proxy`, `Reflect`, `Symbol` exotics, `with`, sloppy mode, `arguments` object, `Function()` constructor, `RegExp` named groups (future), tail call optimization, SharedArrayBuffer, Atomics | Documented as unsupported |

**Test262 has metadata flags** that make subsetting clean:
- `flags: [onlyStrict]` — strict mode only (we run these)
- `flags: [noStrict]` — sloppy mode only (we skip these)
- `features: [Proxy]` — requires Proxy (we skip these)
- `features: [async-functions]` — requires async (we run these)

We parse the metadata, filter to our supported feature set, and run the rest. The pass rate on our subset is the compliance number we publish.

**Integration:** Download Test262 as a submodule. CI runs the supported subset on every commit. Regressions block merge.

```bash
cot test262                            # Run full supported subset
cot test262 --section=classes          # Run specific section
cot test262 --report                   # Generate compliance report
```

### TypeScript Test Suite — Type Checker Conformance (~80,000 cases)

**Repository:** `github.com/microsoft/TypeScript` (`tests/cases/`)
**Maintained by:** Microsoft TypeScript team

Four categories of tests:

| Category | Count | What It Tests | Our Use |
|----------|-------|---------------|---------|
| `conformance/` | ~25,000 | Type checking: does tsc accept/reject this code correctly | **Primary target** |
| `compiler/` | ~20,000 | Compiler behavior: output, declaration emit, module resolution | Partially applicable |
| `fourslash/` | ~30,000 | Language service: completions, hover, refactors, go-to-def | Not applicable (LSP separate) |
| `cases/projects/` | ~5,000 | Multi-file projects, module resolution, declaration merging | Applicable |

**Our approach:**

| Tier | Tests | Target |
|------|-------|--------|
| `conformance/` strict mode tests | ~15,000-20,000 | **90%+ pass rate** |
| `compiler/` type resolution tests | ~10,000-15,000 | **85%+ pass rate** |
| `fourslash/` language service | Skipped | Not applicable |
| Complex type gymnastics (conditional types, template literals, mapped types) | ~5,000 | Best effort — these are type-level programming, not runtime code |

**TypeScript test format:** Each test is a `.ts` file with expected errors marked as `// @errors: 2345` comments. We parse these expectations and compare against libts diagnostics.

```bash
cot tstest                             # Run full supported subset
cot tstest --section=conformance       # Run specific category
cot tstest --report                    # Generate compliance report
```

### Node.js Test Suite — API Conformance (~5,000 tests)

**Repository:** `github.com/nodejs/node` (`test/parallel/`, `test/sequential/`)
**Maintained by:** Node.js Foundation

Tests the Node standard library API. We run the subset matching our `stdlib/*.ts` implementations:

| Module | Node Tests | Our Coverage |
|--------|-----------|-------------|
| `fs` | ~300 | readFile, writeFile, readdir, stat, mkdir |
| `path` | ~50 | join, resolve, dirname, basename, extname |
| `http` | ~200 | createServer, request basics |
| `crypto` | ~150 | createHash, randomBytes |
| `buffer` | ~200 | alloc, from, toString, slice |
| `url` | ~80 | URL class, URLSearchParams |
| `events` | ~50 | EventEmitter core methods |
| `stream` | ~150 | Readable, Writable basics |
| `child_process` | ~100 | spawn, exec |
| `assert` | ~50 | ok, equal, deepEqual |

**Target: 80%+ on tests for implemented modules.** We won't pass tests for APIs we haven't implemented yet — the score grows as we add more stdlib surface.

```bash
cot nodetest                           # Run Node API tests
cot nodetest --module=fs               # Run specific module
```

### Compliance Dashboard

Published numbers (updated on every release):

```
Cot TypeScript Native v0.X Compliance
──────────────────────────────────────
Test262 (strict mode subset):     96.2%  (34,847 / 36,210 pass)
TypeScript conformance:           91.4%  (14,112 / 15,439 pass)
TypeScript compiler:              87.1%  (10,023 / 11,509 pass)
Node.js API (implemented modules): 83.5%  (1,254 / 1,502 pass)
```

These numbers are the trust signal. Developers look at Test262 compliance the same way they look at Web Platform Tests for browsers. Publishing them prominently on the website says "we take compatibility seriously, and here's the proof."

### CI Integration

```yaml
# .github/workflows/compliance.yml
test262:
  - download tc39/test262 (pinned commit)
  - cot test262 --report --strict-only --skip-features=Proxy,eval,with
  - compare against baseline, fail on regression

tstest:
  - download microsoft/TypeScript tests (pinned tag)
  - cot tstest --section=conformance --strict
  - compare against baseline, fail on regression

nodetest:
  - download nodejs/node tests (pinned tag)
  - cot nodetest --modules=fs,path,http,crypto,buffer,url
  - compare against baseline, fail on regression
```

Every PR shows: "Test262: +3 passing, 0 regressions. TS conformance: +7 passing, 0 regressions."

---

## What This Is Not

- Not a JavaScript runtime (no `eval`, no dynamic code loading)
- Not a TypeScript transpiler (no JS output — goes straight to native)
- Not npm-compatible for packages with native addons or V8 bindings
- Not a browser runtime (no DOM, no Web APIs — server/CLI only, Wasm for browser)

It IS:
- A native compiler for TypeScript
- Node API compatible for server-side code
- Zero-overhead abstraction over Cot's proven compiler pipeline
- The first tool that lets you `cargo build` a TypeScript project
