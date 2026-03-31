# Cot Platform Vision — Forking Zig for Full-Stack Development

**Date:** 2026-04-01
**Status:** Design vision — pre-implementation

---

## 1. The Idea

Fork the Zig compiler. Strip the systems-programming opinions. Keep the 10 years of battle-tested infrastructure. Add ARC, WasmGC, Swift-style concurrency, TypeScript syntax. Expose C ABI boundaries with a unified SPIR-V binary format so anyone can build a language frontend on top.

**Cot becomes two things:**
1. A full-stack developer-friendly language (TypeScript-like syntax, native performance)
2. A compilation platform that other languages can target

---

## 2. What Zig Gives Us (The Fork Baseline)

| Component | Zig's Implementation | Lines | Maturity |
|-----------|---------------------|-------|----------|
| AST | SoA MultiArrayList, 213 tags, 13 bytes/node | ~5K | 10 years |
| ZIR | Untyped IR, 390 instructions, index-based | ~15K | 10 years |
| Sema | Semantic analysis, type checking, comptime | ~30K | 10 years |
| AIR | Typed IR, architecture-aware | ~10K | 10 years |
| LLVM backend | Full LLVM codegen | ~20K | 10 years |
| Self-hosted backend | x86_64, aarch64, wasm | ~40K | 5 years |
| Incremental compilation | File-level, cached | ~10K | 3 years |
| Build system | `build.zig`, cross-compilation | ~5K | 8 years |
| Package manager | `build.zig.zon`, fetching | ~5K | 3 years |
| C interop | `@cImport`, translate-c | ~15K | 10 years |
| Debug info | DWARF emission | ~5K | 5 years |
| Test framework | Built-in `test` blocks | ~2K | 10 years |

**Total: ~160K lines of proven compiler infrastructure we don't have to write.**

---

## 3. What We Strip (Zig-Specific Opinions)

| Zig Opinion | What We Change |
|-------------|---------------|
| Manual memory management | Add ARC (default) + optional tracing GC |
| No hidden allocations | Allow implicit allocations in safe mode |
| No operator overloading | Keep — this is a good opinion |
| No default parameters | Already added to Cot — port forward |
| Compile-time everything | Keep comptime but add runtime reflection option |
| No closures capturing mutable state | Allow closures with ARC-managed captures |
| No exceptions | Keep error unions, add try/catch sugar |
| No OOP | Add structs-as-classes with `@safe` mode |
| No implicit conversions | Allow in `@safe` mode (number → f64, etc.) |
| No garbage collection | Add as opt-in for GC language frontends |

**Philosophy shift:** Zig says "no hidden control flow, no hidden allocations." We say "hidden is fine in safe mode — the compiler is smarter than you think."

---

## 4. What We Add

### 4.1 Memory Models (Choose Per-Language)

```
@memory("arc")     // Default — Swift/Cot style. Zero-cost for most code.
@memory("gc")      // Opt-in tracing GC. For Go/Java/C# frontends.
@memory("manual")  // Zig's original model. For systems code.
@memory("arena")   // Zig arena pattern. Bulk alloc + bulk free.
```

Each frontend chooses its memory model. The IR encodes the choice. Sema inserts the right operations (retain/release for ARC, safepoints for GC, nothing for manual).

### 4.2 Concurrency (Swift Model)

Port from `references/swift/stdlib/public/Concurrency/`:
- Structured concurrency (Task, TaskGroup)
- Actor isolation (data race safety at compile time)
- Async/await with cooperative scheduling
- Sendable checking

Already implemented in Cot (618 tests, 50 Swift features). Port to the new IR.

### 4.3 WasmGC Target

Extend Zig's existing Wasm backend:
- WasmGC struct/array types
- WasmGC reference types
- No linear memory for managed objects
- Interop with browser DOM via externref

### 4.4 Frontend C ABI

Any language that can call C functions can produce IR:

```c
// Create a compilation unit
CotUnit* unit = cot_unit_create();

// Emit IR (boundary 1 — ZIR-level)
cot_emit_struct_decl(unit, "Point", fields, field_count);
cot_emit_fn_decl(unit, "distance", params, param_count, body);
cot_emit_call(unit, "distance", args, arg_count);

// Compile to native
CotResult result = cot_compile(unit, COT_TARGET_NATIVE);
write(fd, result.bytes, result.len);

// Or compile to Wasm
CotResult wasm = cot_compile(unit, COT_TARGET_WASM);
```

### 4.5 TypeScript Frontend (libts)

Already built — 6,800 lines. Becomes the first non-Zig frontend on the platform.

---

## 5. The Unified Binary Format

**Same SPIR-V encoding at both boundaries. Same tooling reads both.**

### 5.1 Encoding (Shared)

```
Word 0: (word_count << 16) | opcode
Words 1..N: operands
```

32-bit little-endian, word-aligned. String heap with deduplication. Section-based layout.

### 5.2 Boundary 1 — ZIR Level (Frontend → Sema)

High-level, untyped, preserves language semantics.

```
Opcode range: 0x0100 — 0x04FF

Declarations (0x0100):
  STRUCT_DECL, FN_DECL, VAR_DECL, ENUM_DECL, UNION_DECL,
  TRAIT_DECL, IMPL_BLOCK, IMPORT_DECL, TYPE_ALIAS

Expressions (0x0200):
  IDENT, LITERAL, BINARY, UNARY, CALL, FIELD_ACCESS,
  INDEX, ARRAY_LITERAL, STRUCT_INIT, IF_EXPR, SWITCH_EXPR,
  CLOSURE, AWAIT, TRY, CATCH, NEW

Statements (0x0300):
  EXPR_STMT, RETURN, IF, WHILE, FOR, BLOCK, BREAK,
  CONTINUE, DEFER, VAR, ASSIGN

Types (0x0400):
  NAMED, POINTER, OPTIONAL, ERROR_UNION, SLICE, ARRAY,
  FUNCTION, TUPLE, GENERIC_INSTANCE
```

### 5.3 Boundary 2 — AIR Level (Sema → Codegen)

Low-level, typed, ready for machine code emission.

```
Opcode range: 0x0500 — 0x08FF

Arithmetic (0x0500):
  ADD, SUB, MUL, DIV, MOD, NEG, FADD, FSUB, FMUL, FDIV

Memory (0x0600):
  LOAD, STORE, ALLOC, FREE, STACK_ALLOC, RETAIN, RELEASE,
  GC_SAFEPOINT, FIELD_PTR, INDEX_PTR

Control (0x0700):
  BR, BR_COND, SWITCH, CALL, CALL_INDIRECT, RET,
  UNREACHABLE, ASYNC_SUSPEND, ASYNC_RESUME

Type (0x0800):
  BITCAST, INTCAST, FLOATCAST, PTR_TO_INT, INT_TO_PTR,
  OPTIONAL_WRAP, OPTIONAL_UNWRAP, ERROR_WRAP
```

### 5.4 File Format

```
[Header]              Magic, version, generator, section count
[String Heap]         Section 0x01 — deduplicated strings
[Boundary 1 or 2]    Section 0x02 — IR instructions
[Debug Info]          Section 0x03 — source maps, file references
[Metadata]            Section 0x04 — memory model, target, features
```

Same file extension: `.cir` — the header indicates which boundary level.

### 5.5 Wasm as Output Format

Boundary 2 bytes map naturally to Wasm:

| AIR Opcode | Wasm Equivalent |
|-----------|----------------|
| ADD (i64) | i64.add |
| FADD (f64) | f64.add |
| LOAD | local.get / memory.load |
| STORE | local.set / memory.store |
| CALL | call |
| BR_COND | br_if |
| RETAIN | call $arc_retain (for linear memory Wasm) |
| RETAIN | ref.cast (for WasmGC) |

A "portable CIR" file could be:
1. AOT-compiled to native via Cranelift or LLVM
2. JIT-compiled at runtime via Cranelift
3. Emitted as a Wasm module for browsers
4. Interpreted directly (for REPL/scripting use cases)

---

## 6. How Languages Plug In

### 6.1 A Language Author's Workflow

```
1. Write a scanner + parser for your syntax
2. Emit Boundary 1 bytes (ZIR-level) via C ABI
3. Ship your frontend as a single binary or library
4. Users run: your-lang build file.xyz → native binary
```

The platform handles: type checking, optimization, memory management, codegen, linking, debug info, cross-compilation.

### 6.2 Configuration Per-Language

```json
{
  "language": "myLang",
  "memory": "arc",
  "concurrency": "swift",
  "error_handling": "error_unions",
  "implicit_conversions": true,
  "safe_mode": true,
  "targets": ["native", "wasm", "wasmgc"]
}
```

### 6.3 First-Party Frontends

| Frontend | Syntax | Memory | Status |
|----------|--------|--------|--------|
| Cot | Cot syntax | ARC | Existing (370 features) |
| libts | TypeScript/JavaScript | ARC | Existing (72% coverage) |
| Zig-compat | Zig syntax | Manual | Fork baseline |

### 6.4 Potential Third-Party Frontends

| Frontend | Syntax | Memory | Effort |
|----------|--------|--------|--------|
| Go-like | Go syntax | GC | Medium — needs tracing GC |
| Kotlin-like | Kotlin syntax | ARC or GC | Medium |
| Swift-like | Swift syntax | ARC | Low — semantics match closely |
| C | C syntax | Manual | Low — Zig already has translate-c |
| Rust-like | Rust syntax | Manual + borrow checker | High |

---

## 7. Implementation Plan

### Phase 1: Fork and Reorganize (2-4 weeks)

1. Fork `ziglang/zig` into `cotlang/cot-platform`
2. Reorganize into:
   ```
   platform/
     frontend/     (Zig scanner, parser, AST — kept as reference frontend)
     zir/          (ZIR instruction set — extend for multi-language)
     sema/         (Semantic analysis — extend with ARC, GC, async)
     air/          (Typed IR — extend with memory model ops)
     codegen/
       cranelift/  (CIR → Cranelift, existing rust/libclif)
       llvm/       (AIR → LLVM, existing Zig backend)
       wasm/       (AIR → Wasm, existing Zig backend)
       wasmgc/     (AIR → WasmGC, new)
     binary/       (SPIR-V format reader/writer for both boundaries)
     cabi/         (C ABI entry points for frontends)
   ```
3. Add C ABI boundary at ZIR level
4. Verify existing Zig test suite still passes

### Phase 2: Add Memory Models (2-4 weeks)

1. Add ARC ops to AIR (retain, release, weak_retain, weak_release)
2. Port Cot's ARC insertion pass to work on AIR
3. Add GC safepoint ops to AIR (for future GC frontend support)
4. Add `@memory` annotation to ZIR

### Phase 3: Port Cot Features (4-8 weeks)

1. Port Swift concurrency to new Sema/AIR
2. Port error union enhancements
3. Port `@safe` mode
4. Port default parameters, closures, string interpolation
5. Port WasmGC backend

### Phase 4: Binary Format (2-4 weeks)

1. Implement SPIR-V encoder/decoder for ZIR level (Boundary 1)
2. Implement SPIR-V encoder/decoder for AIR level (Boundary 2)
3. File caching: hash source → cached Boundary 1 → skip parse
4. Incremental compilation via cached boundaries

### Phase 5: libts on New Platform (2-4 weeks)

1. Port libts scanner/parser (already written, 6,800 lines)
2. Update transform to emit Boundary 1 bytes instead of Cot AST
3. Verify 72+ compliance tests pass on new platform
4. End-to-end: `cot run hello.ts` on forked platform

### Phase 6: Stabilize and Document (ongoing)

1. Stabilize Boundary 1 opcode set
2. Stabilize Boundary 2 opcode set
3. Write "Build a Language" tutorial
4. Publish C ABI headers
5. Example: build a minimal language in 500 lines

---

## 8. Why This Works

**For full-stack developers:** Write TypeScript, get native binaries. Same language web + server + mobile. No V8, no GC pauses, sub-millisecond cold start.

**For language designers:** Write a parser (the fun part), skip the compiler backend (the hard part). Get native + Wasm + cross-compilation for free.

**For the ecosystem:** One optimizer, one debugger, one profiler, one package format. Languages interop at the IR level — call a Cot function from a Go-like frontend, seamlessly.

**Why Zig as the base:** Because Zig already solved the hardest problems (codegen, cross-compilation, incremental compilation, C interop) and did it with a data-oriented architecture that's perfect for binary serialization. We're not rewriting those 160K lines. We're extending them.

---

## 9. What This Is NOT

- **Not LLVM.** LLVM's IR is too low — languages lose their semantics. Our IR preserves structs, closures, error unions, async.
- **Not the JVM/.NET CLR.** Those are VMs. We compile to native. No runtime, no bytecode interpreter.
- **Not a Zig contribution.** This is a fork with a fundamentally different philosophy. Zig is a systems language. We're building a full-stack platform.
- **Not vaporware.** Cot already compiles TypeScript to native binaries (72 tests passing). The platform is an architectural restructuring, not a ground-up rewrite.
