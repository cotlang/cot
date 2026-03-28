# lower.zig Port Plan

**Date:** 2026-03-29
**Source:** `compiler/frontend/lower.zig` (13,528 lines, 170 functions)
**Target:** `src/libcot-zig/lower.zig` (~4,000 lines core) + 7 submodules (~9,500 lines)
**Dependencies:** ast, types, checker, ir, comptime, target, errors, source, debug (all ported)

---

## Problem

13,528 lines in one file. Porting rules say aim for files under 2K lines. The file has 13 functional groups with varying coupling — some can be cleanly extracted, others are tightly integrated.

---

## Split Strategy

```
src/libcot-zig/
├── lower.zig                    (~4,000 lines) Core orchestrator
├── lower/
│   ├── expr.zig                 (~1,650 lines) Expression lowering
│   ├── calls.zig                (~2,230 lines) Call/method/builtin lowering
│   ├── structs.zig              (~1,330 lines) Struct/union/enum operations
│   ├── generics.zig             (~720 lines)   Generic instantiation lowering
│   ├── type_ops.zig             (~700 lines)   ARC emit, VWT dispatch, existentials
│   ├── async_lower.zig          (~750 lines)   Async/await, Task, closures
│   └── collections.zig          (~730 lines)   List/Map/String/Error operations
```

---

## What Goes Where

### lower.zig — Core (~4,000 lines)

The orchestrator that stays unified because of tight coupling:

| Section | Lines | Why it stays |
|---------|-------|-------------|
| Lowerer struct + init/deinit | ~200 | Central state, used by everything |
| lowerFile, lowerFnDecl, lowerDecl | ~520 | Entry points, manage function builder lifecycle |
| lowerStmt + all statement lowering | ~2,300 | Tightly coupled to ARC cleanup stack |
| ARC cleanup management | ~310 | Called at every scope exit, every return, every defer |
| Async constructor/poll split | ~260 | Integrated with function declaration lowering |
| Helpers (resolveLocal, emitConst, etc.) | ~400 | Cross-cutting utilities used by every submodule |

Statement lowering MUST stay in core because every statement can trigger:
- ARC cleanup (scope exit releases locals)
- Defer execution (deferred exprs run at scope boundaries)
- Error propagation (errdefer runs on error paths)
- Loop break/continue (cleanup between current scope and target)

These interactions make statement lowering inseparable from the cleanup stack.

### lower/expr.zig — Expression Lowering (~1,650 lines)

| Function | Lines | What it does |
|----------|-------|-------------|
| lowerExpr | ~50 | Expression dispatch |
| lowerBinary | ~300 | All binary operators |
| lowerUnary | ~60 | Unary operators |
| lowerFieldAccess | ~400 | Struct/enum/union/tuple field access |
| lowerIndex | ~100 | Array/slice/list indexing |
| lowerSlice | ~80 | Slice expressions |
| lowerIf | ~150 | If expressions |
| lowerSwitch | ~200 | Switch expressions with pattern matching |
| lowerBlock | ~100 | Block expressions |
| lowerParen, lowerArray, lowerTuple | ~200 | Simple expression forms |

**Coupling:** Calls back into `lower.zig` for `emitRetain`, `emitRelease`, `resolveLocal`, `emitConst`. Takes `*Lowerer` as first parameter.

### lower/calls.zig — Call and Builtin Lowering (~2,230 lines)

| Function | Lines | What it does |
|----------|-------|-------------|
| lowerCall | ~676 | Function/method call lowering |
| lowerBuiltinCall | ~916 | All @builtin dispatch |
| lowerMethodCall | ~200 | Method resolution + dispatch |
| lowerCallIndirect | ~100 | Indirect (function pointer) calls |
| lowerClosureCall | ~100 | Closure calls with context |
| resolveOverload | ~240 | Method overload resolution |

**Why extract:** `lowerBuiltinCall` alone is 916 lines — it's a pure dispatch table mapping BuiltinKind to IR emission. No ARC coupling, no scope management. Same pattern as checker/builtins.zig.

### lower/structs.zig — Struct/Union/Enum Operations (~1,330 lines)

| Function | Lines | What it does |
|----------|-------|-------------|
| lowerStructInit | ~200 | Struct literal initialization |
| lowerNewExpr | ~250 | Heap allocation with field init |
| lowerUnionInit | ~100 | Union variant construction |
| lowerUnionPayload | ~80 | Union payload extraction |
| lowerUnionTag | ~50 | Union tag read |
| emitStructCopy | ~200 | Deep struct copy (with nested ARC) |
| emitStructDestroy | ~150 | Deep struct destroy |
| lowerEnumToInt | ~50 | Enum backing value |
| emitFieldOffset | ~100 | Field offset calculation |
| lowerExistentialInit | ~150 | Existential container boxing |

### lower/generics.zig — Generic Instantiation (~720 lines)

| Function | Lines | What it does |
|----------|-------|-------------|
| lowerGenericCall | ~300 | Lower a call to a generic function |
| instantiateGenericBody | ~200 | Clone and specialize a generic function body |
| resolveGenericType | ~100 | Map type params to concrete types |
| emitMonomorphized | ~120 | Emit the specialized function |

**Why extract:** Generic lowering is self-contained — it reads generic definitions, substitutes types, and emits new functions. The only coupling is to the Lowerer's function emission API.

### lower/type_ops.zig — ARC/VWT/Type Operations (~700 lines)

| Function | Lines | What it does |
|----------|-------|-------------|
| emitCopyValue | ~200 | Deep copy dispatched by type (VWT for generics, inline for known types) |
| emitDestroyValue | ~200 | Deep destroy dispatched by type |
| emitRetain | ~50 | ARC retain (increment refcount) |
| emitRelease | ~50 | ARC release (decrement, destroy if zero) |
| emitVWTDispatch | ~200 | Call through Value Witness Table function pointers |

**Why extract:** These are the ARC/VWT operations that libcir will eventually own. Clean extraction now makes the future C ABI transition easier — these functions become the implementation behind `cir_build_retain()`, `cir_build_release()`, `cir_build_vwt_copy()`.

### lower/async_lower.zig — Async/Concurrency (~750 lines)

| Function | Lines | What it does |
|----------|-------|-------------|
| lowerAwait | ~150 | Await expression lowering |
| lowerTaskCreate | ~100 | Task {} creation |
| lowerAsyncLet | ~100 | async let parallel binding |
| lowerClosureExpr | ~200 | Closure creation with captured variables |
| emitCooperativeScheduling | ~200 | Wasm cooperative await via run_until_task_done |

### lower/collections.zig — Collections/Strings/Errors (~730 lines)

| Function | Lines | What it does |
|----------|-------|-------------|
| lowerStringConcat | ~100 | String concatenation |
| lowerStringInterp | ~140 | String interpolation segments |
| lowerListOps | ~150 | List append/get/set/len/free |
| lowerMapOps | ~150 | Map set/get/has/free |
| lowerErrorUnion | ~100 | Error union construction/extraction |
| lowerCatch | ~90 | catch expression lowering |

---

## Port Order

Each chunk ports one submodule completely, tests it, commits.

| Step | File | Lines | Depends on |
|------|------|-------|-----------|
| 1 | lower.zig core (Lowerer struct, lowerFile, lowerFnDecl, stmts, ARC, helpers) | ~4,000 | All ported files |
| 2 | lower/expr.zig | ~1,650 | Core |
| 3 | lower/calls.zig | ~2,230 | Core + expr |
| 4 | lower/structs.zig | ~1,330 | Core |
| 5 | lower/type_ops.zig | ~700 | Core |
| 6 | lower/generics.zig | ~720 | Core + calls |
| 7 | lower/async_lower.zig | ~750 | Core + expr |
| 8 | lower/collections.zig | ~730 | Core + expr |

Steps 2-8 can be partially parallelized since they all depend on core but not each other (except calls needs expr for argument lowering, and generics needs calls for call-site detection).

---

## Key Challenges

1. **The Lowerer struct has ~40 fields** — function builder, cleanup stack, scope, generic context, async state, etc. Every submodule needs access via `*Lowerer`.

2. **lowerBuiltinCall is 916 lines** — the single largest function. It's a dispatch table but some builtins need access to the function builder, cleanup stack, and type registry. Consider splitting further into builtin groups (math, cast, arc, io, string, etc.).

3. **The cleanup stack** is central to everything. Statement lowering, expression lowering (try, catch, orelse), and function return all interact with it. It stays in core.

4. **Generic instantiation** creates new functions mid-lowering. The Lowerer must be re-entrant for this — lowerGenericCall calls back into lowerFnDecl with substituted types.

5. **Adapting to compact AST** — every old `self.tree.getNode(idx).asExpr().binary.left` becomes `self.tree.nodeData(idx).node_and_node[0]`. The lowerer has the most AST access of any file.
