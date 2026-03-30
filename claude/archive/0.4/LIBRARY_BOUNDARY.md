# Library Boundary: libcot ↔ libcir

**Date:** 2026-03-29
**Status:** Reference document for ongoing port

---

## The Boundary

The boundary between libcot and libcir is **cir.h** — the C ABI header already defined in `src/include/cir.h`.

**libcot's job:** Parse Cot source, type-check it, and call `cir_build_*()` functions to emit IR.

**libcir's job:** Receive those calls, build SSA, optimize, and emit Wasm/native code.

The lowerer (`lower.zig`) is the last file in libcot. It walks the type-checked AST and translates each node into a sequence of `cir_build_*()` calls. Everything downstream — SSA construction, optimization passes, ARC insertion, VWT generation, code emission — is libcir.

---

## File Classification

### libcot-zig/ (frontend, language-specific)

These files know about Cot syntax, @safe mode, string interpolation, traits, etc.

| File | Lines | Status | Notes |
|------|-------|--------|-------|
| token.zig | 565 | Done | |
| source.zig | 264 | Done | |
| errors.zig | 415 | Done | |
| scanner.zig | 643 | Done | |
| target.zig | 153 | Done | |
| debug.zig | 168 | Done | |
| ast.zig | 1,594 | Done | Zig-style data-oriented |
| parser.zig | 3,218 | Done | Compact AST output |
| comptime.zig | 108 | Done | |
| types.zig | 1,027 | Done | Shared by libcot + libcir (type definitions cross the boundary) |
| checker.zig | 5,424 | Done | + 4 submodule extraction planned |
| formatter.zig | 1,399 | Done | |
| ir.zig | 1,419 | Done | **THE BOUNDARY** — defines IR nodes that cir.h wraps |
| lower.zig | 13,528 | **Next** | Last libcot file — calls ir.FuncBuilder to emit IR |

### libcir-zig/ (IR, language-agnostic)

These files know about ARC, VWT, SSA, actors, generics — but NOT Cot syntax. Another frontend (TypeScript, Python) would use the exact same libcir.

| File | Lines | Status | Notes |
|------|-------|--------|-------|
| arc_insertion.zig | 456 | Not started | ARC cleanup stack tracking |
| vwt_gen.zig | 1,378 | Not started | Value Witness Table generation |
| ssa_builder.zig | 2,908 | Not started | IR → SSA conversion (phi insertion) |
| ssa/op.zig | ~800 | Not started | SSA opcodes |
| ssa/value.zig | ~600 | Not started | SSA values |
| ssa/func.zig | ~800 | Not started | SSA functions |
| ssa/block.zig | ~400 | Not started | SSA blocks |
| ssa/passes/*.zig | ~5,000 | Not started | Optimization passes |
| codegen/wasm/*.zig | ~5,500 | Not started | Wasm bytecode emission |

---

## The ir.zig Question

`ir.zig` sits on the boundary. It defines the IR node types (`ir.Node`, `ir.FuncBuilder`, `ir.Func`) that both libraries use:

- **libcot's lowerer** creates `ir.Node` values via `FuncBuilder.emit*()` methods
- **libcir's SSA builder** reads `ir.Func` as input

In the current codebase (no C ABI boundary yet), both libraries import `ir.zig` directly. When the C ABI boundary is established:

1. `ir.zig`'s data structures become **libcir-internal**
2. `cir.h` functions wrap `FuncBuilder` methods: `cir_build_add()` calls `fb.emitBinary(.add, ...)`
3. libcot calls through `cir.h` — never imports `ir.zig` directly
4. The `ir.Node.Data` enum becomes libcir's internal representation

For now, `ir.zig` lives in `libcot-zig/` so `lower.zig` can import it directly. When the boundary is enforced, it moves to `libcir-zig/` and libcot only sees `cir.h`.

---

## types.zig — Shared Across the Boundary

`types.zig` is special — type definitions cross the library boundary. Both libcot (checker needs types for validation) and libcir (SSA builder needs types for code generation) use `TypeIndex`, `TypeRegistry`, `BasicKind`, etc.

Options:
1. **Duplicate** — libcot and libcir each have their own copy (divergence risk)
2. **Shared library** — types.zig becomes a fourth shared library (over-engineering)
3. **Serialized** — types are passed through `cir.h` as `CirTypeRef` values, reconstructed in libcir

The current `cir.h` already takes option 3 — type construction functions (`cir_type_pointer()`, `cir_type_struct()`, etc.) let the frontend describe types through the C ABI. libcir builds its own internal TypeRegistry from those calls. TypeIndex values are just u32 handles that both sides agree on.

This means types.zig in libcot is the **frontend's type system** (used for type checking), while libcir has its own type storage built from `cir_type_*()` calls. The type handles (u32 integers) are the same on both sides.

---

## How a TypeScript Frontend Would Work

```
libtypescript                    libcir (same for all frontends)
─────────────                    ─────────────────────────────
ts_parse("index.ts")
ts_check(ast)
                                 mod = cir_module_create("index")
ts_lower_func("add"):
  a_type = cir_type_i64()        → registers type, returns CirTypeRef=5
  func = cir_func_create("add")  → creates CIR function
  b = cir_block_create(func)     → creates entry block
  p0 = cir_build_arg(b, a_type)  → param node
  p1 = cir_build_arg(b, a_type)  → param node
  sum = cir_build_add(b, p0, p1) → binary add node
  cir_build_ret(b, sum)          → return node

                                 cir_run_passes(mod)  → SSA, ARC, optimize
                                 cir_emit_wasm(mod)   → .wasm output
```

The TypeScript frontend never imports `ir.zig`. It only calls `cir_build_*()` functions through the C ABI. libcir handles everything from SSA construction onwards.

**Key insight:** The TypeScript frontend doesn't need to know about ARC at all. When it calls `cir_build_retain()` / `cir_build_release()`, it's because the TypeScript type checker determined an object needs reference counting. Or it doesn't call them at all if it's using GC — that decision is in the frontend, not libcir. libcir just executes whatever IR instructions it receives.

Wait — actually, ARC insertion is different. The frontend decides WHEN to retain/release (based on scope analysis). But the MECHANISM (how retain/release work at the machine level) is libcir's job.

Correction: ARC insertion can live on either side:
- **Frontend-side ARC** (current approach): `lower.zig` uses `arc_insertion.zig` to track cleanup stacks and emit `cir_build_retain()`/`cir_build_release()` calls at the right points
- **IR-side ARC** (Swift approach): The frontend emits high-level lifetime markers, and an optimization pass in libcir inserts the actual retain/release

The current Cot compiler does frontend-side ARC — the lowerer explicitly emits retain/release. This is simpler but means every frontend must implement ARC tracking. If we wanted libcir-side ARC, we'd add lifetime markers to the IR and let a libcir pass insert retain/release.

For now: `arc_insertion.zig` stays as a libcir concern (it's called by the lowerer but operates on IR nodes). The lowerer calls it to decide WHERE to emit retain/release, then calls `cir_build_retain()`/`cir_build_release()` through the C ABI.

---

## What lower.zig Needs From libcir

When lower.zig is ported, it needs to call ir.FuncBuilder methods. In the current codebase, it imports ir.zig directly. At the C ABI boundary:

```zig
// Current (direct import):
const ir = @import("ir.zig");
var fb = ir.FuncBuilder.init(allocator, name, type_idx, ret_type, span);
const val = try fb.emitBinary(.add, left, right, type_idx, span);

// Future (C ABI):
const cir = @cImport(@cInclude("cir.h"));
var func = cir.cir_func_create(mod, name, ret_type, false);
var block = cir.cir_block_create(func);
var builder = cir.cir_builder_create(block);
const val = cir.cir_build_add(builder, left, right);
```

The logic of WHAT to emit stays the same. Only HOW it's called changes. This is why porting lower.zig now (with direct ir.zig import) is correct — the C ABI wrapper is a thin layer added later.
