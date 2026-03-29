# Syntax Modes: @zig and @ts

**Date:** 2026-03-29
**Status:** Design document

---

## Concept

Cot supports two syntax modes, selected at the file or project level:

- **`@zig`** — Zig-style syntax (current Cot). Explicit, systems-level. Default.
- **`@ts`** — TypeScript-style syntax. Familiar to web developers. Classes, interfaces, arrow functions, optional chaining.

Both compile to the same IR, use the same ARC memory management, same concurrency model, same Wasm/native codegen. The difference is purely syntactic — which frontend parses the source.

```
@zig                              @ts
fn add(a: i64, b: i64) i64 {     function add(a: number, b: number): number {
    return a + b                      return a + b
}                                 }
```

Both produce identical IR. Both compile to the same native binary.

---

## File-Level Selection

```cot
@zig
// This file uses Zig-style syntax
fn main() { ... }
```

```cot
@ts
// This file uses TypeScript-style syntax
function main() { ... }
```

A project can mix modes — some files `@zig`, some `@ts`. The checker and lowerer see the same AST/IR regardless of which frontend produced it.

## Project-Level Selection

In `cot.json`:
```json
{
    "syntax": "ts",
    "main": "src/main.ts"
}
```

When `syntax` is set at the project level, all files in the project use that mode unless overridden with a file-level annotation.

File extensions:
- `.cot` — uses project default or `@zig`
- `.zig` files — always `@zig` mode (Zig-compatible syntax)
- `.ts` files — always `@ts` mode (TypeScript-compatible syntax)

---

## What @ts Mode Changes

**Syntax only.** The type system, memory model, concurrency, and compilation pipeline are identical.

| Feature | @zig (default) | @ts |
|---------|---------------|-----|
| Function declaration | `fn add(a: i64, b: i64) i64` | `function add(a: number, b: number): number` |
| Arrow functions | `fn(x: i64) i64 { return x + 1 }` | `(x: number): number => x + 1` |
| Variable declaration | `const x = 42` / `var y = 0` | `const x = 42` / `let y = 0` |
| Type annotation position | `name: Type` (after colon) | `name: Type` (same — TS uses this too) |
| Struct declaration | `struct Point { x: i64, y: i64 }` | `interface Point { x: number; y: number }` or `class Point { x: number; y: number }` |
| Trait/interface | `trait Hashable { fn hash() i64 }` | `interface Hashable { hash(): number }` |
| Impl block | `impl Point { fn scale() ... }` | Methods defined inside `class Point { scale() ... }` |
| Optional | `?T` | `T \| null` or `T?` |
| Error union | `!T` | `Result<T, Error>` |
| String interpolation | `"hello ${name}"` | `` `hello ${name}` `` (template literals) |
| Null check | `if (x) \|val\| { ... }` | `if (x !== null) { ... }` |
| For loop | `for item in list { ... }` | `for (const item of list) { ... }` |
| Import | `import "std/io"` | `import { io } from "std"` |
| Enum | `const Color = enum { Red, Green, Blue }` | `enum Color { Red, Green, Blue }` |
| Type alias | `type ID = distinct i64` | `type ID = Distinct<number>` |
| Generics | `fn max(T)(a: T, b: T) T` | `function max<T>(a: T, b: T): T` |
| Async | `async fn fetch() !Response` | `async function fetch(): Promise<Response>` |
| Await | `const data = await fetch()` | `const data = await fetch()` (same) |

**Type mapping:**
| TypeScript | Cot internal |
|-----------|-------------|
| `number` | `i64` (or `f64` depending on context) |
| `string` | `string` (same) |
| `boolean` | `bool` |
| `void` | `void` (same) |
| `null` | `null` (same) |
| `undefined` | `undefined` (same) |
| `T[]` | `List(T)` |
| `Map<K, V>` | `Map(K, V)` |
| `T \| null` | `?T` |

---

## Architecture

```
@zig files ──► libcot (Zig-style frontend) ──┐
                                              ├──► libcir (shared IR) ──► native/wasm
@ts files  ──► libts  (TS-style frontend)  ──┘
```

Both frontends:
1. Tokenize source (different Token enums)
2. Parse to AST (different AST but same semantic model)
3. Type-check (same type system, different syntax for annotations)
4. Lower to IR (same IR instructions via cir.h)

The foundation module (types, source, target, debug) is shared by all frontends.

---

## Competitive Position

**vs Bun/Deno:** Those are JavaScript runtimes — they interpret or JIT TypeScript. Cot compiles TypeScript to native binaries with no runtime, no GC, deterministic memory via ARC. Orders of magnitude smaller binaries, predictable performance.

**vs tsc:** tsc type-checks and emits JavaScript. Cot type-checks and emits native code or Wasm. The type checking is compatible (same syntax), but the output is fundamentally different.

**vs Go:** Go compiles to native but has a GC and no generics until recently. Cot has ARC (no GC pauses), full generics, and TypeScript syntax that 15M developers already know.

**The pitch:** "Your TypeScript code, compiled to a native binary. No runtime. No GC. Just `cot build main.ts -o main`."

---

## Implementation Plan

1. **libts-zig/** — TypeScript-syntax frontend
   - ts_token.zig — TypeScript tokens (`function`, `class`, `interface`, `let`, `const`, `=>`, etc.)
   - ts_scanner.zig — TypeScript lexer (template literals, JSX eventually)
   - ts_ast.zig — TypeScript AST (compact, same data-oriented pattern as libcot)
   - ts_parser.zig — TypeScript parser
   - ts_checker.zig — TypeScript type checker (maps TS types to foundation TypeIndex)
   - ts_lower.zig — TypeScript lowerer (emits same IR as libcot)

2. **Shared infrastructure** — foundation + libcir unchanged

3. **CLI integration** — `cot build main.ts` auto-selects libts; `cot build main.cot` auto-selects libcot

4. **Mixed projects** — `cot.json` with `"syntax": "ts"` default, per-file override with `@zig`/`@ts`

---

## What's NOT Different Between Modes

- ARC memory management (no GC in either mode)
- Actor concurrency (Swift-style, same in both)
- Value Witness Tables for generics
- Error unions / Result types (syntax differs, semantics identical)
- Struct layout, alignment, size
- Wasm and native code generation
- All runtime functions (alloc, dealloc, print, I/O)
- The IR instruction set
- All optimization passes
