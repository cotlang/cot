# Cot Compiler Improvement Plan — Based on Go & Swift Audits

**Date:** 2026-03-17
**Context:** Audited Go compiler (GC-based) and Swift compiler (ARC-based) memory patterns. Go eagerly frees ASTs after type-checking. Swift uses `load [copy]` / `store [assign]` SIL patterns for precise ARC. Cot's ARC has gaps exposed by selfcot dogfooding.

---

## Executive Summary

The self-hosting effort exposed three classes of issues:

1. **ARC correctness** — managed pointer loads from fields through pointer deref weren't retained (FIXED in lower.zig:2448-2470)
2. **Phase architecture** — ASTs kept alive through lowering for generic re-checking, unlike Go which frees after type-check
3. **Native codegen stack frames 2x too large** — the Zig compiler generates stack frames for selfcot functions that are 2x larger than its own equivalent functions (e.g., `checkFnDeclBody`: 3,392 bytes vs Zig's `checkFnDeclWithName`: 1,648 bytes). Deep call chains during lowering overflow the 8MB stack, corrupting the Lowerer's data on the stack.

This plan addresses all three, plus Go-inspired improvements for memory efficiency.

---

## Phase 1: Fix Native Codegen Stack Frame Sizes (CRITICAL — Blocks Self-Hosting)

**Problem:** The Zig compiler's native backend generates stack frames for selfcot functions that are **2x larger** than its own equivalent functions. Deep call chains during ir.cot lowering (86 methods in FuncBuilder struct, each triggering `lowerMethodWithName → lowerBlockNode → lowerStmt → lowerExprNode → lowerCall`) overflow the 8MB stack and corrupt the Lowerer struct's data.

**Hard evidence (ARM64 prologue disassembly):**

| Function | Selfcot frame | Zig compiler's own | Ratio |
|----------|--------------|-------------------|-------|
| checkFnDeclBody | 3,392 bytes | 1,648 bytes | **2.1x** |
| instantiateGenericFunc | 2,736 | — | — |
| lowerBlockNode | 2,992 | — | — |
| lowerCall | 2,688 | — | — |
| lowerMethodCall | 2,480 | — | — |

A single call chain from `lowerToBuilder` to `checkFnDeclBody` uses **~23KB of stack**. With ir.cot processing 86 nested methods (each re-entering the lowering pipeline), the stack overflows.

**Evidence of corruption:**
- `lowered_generics.count = 4355079168` (0x103C00000) — pointer value in count field
- `lowered_generics.capacity = 9` — not a power of 2 (should be 0, 8, 16, 32...)
- Non-deterministic: sometimes SIGBUS (guard page), sometimes @trap (data corruption)
- Struct layout is verified correct (offsets match expected values)
- Map works correctly in isolation tests
- Crash occurs even with generic body lowering completely disabled — it's the REGULAR method lowering that overflows

**Root cause in the Zig compiler:** Functions with many local variables across switch/if-else branches allocate ALL locals upfront instead of sharing stack slots between branches. The `overlap_group` mechanism exists in `ir.zig` and is used for switch arms in `lower.zig`, but:
1. If-else chains don't use overlap groups
2. The optimization may not be aggressive enough for the selfcot's deeply-nested pattern-matching code

**Fix areas:**
- `compiler/frontend/lower.zig` — add `beginOverlapGroup()`/`nextOverlapArm()` for if-else chains
- `compiler/codegen/native/ssa_to_clif.zig` — optimize stack slot allocation to reuse slots for locals in different basic blocks
- `compiler/frontend/ssa_builder.zig` — ensure overlap group stack sharing works for deeply nested cases

**Success criteria:** `selfcot build self/frontend/ir.cot` produces valid `.wasm` without stack corruption.

---

## Phase 2: Complete ARC Correctness (Per Swift SILGen Reference)

The ARC fix from today (retain on field access through pointer deref) is correct but may not cover all cases. A systematic audit against Swift's SIL patterns is needed.

### 2a: Audit all managed pointer load/store paths

**Swift SIL has 4 load semantics:**
| SIL | Meaning | Cot equivalent |
|-----|---------|----------------|
| `load [trivial]` | Non-ARC type, raw copy | `couldBeARC() = false` |
| `load [copy]` | ARC type, retain on load | Field access → local (TODAY'S FIX) |
| `load [take]` | ARC type, transfer ownership (no retain, source destroyed) | Move semantics (not yet in Cot) |
| `load [borrow]` | ARC type, no retain (caller guarantees lifetime) | Current default for locals from locals |

**Swift SIL has 3 store semantics:**
| SIL | Meaning | Cot equivalent |
|-----|---------|----------------|
| `store [trivial]` | Non-ARC type, raw store | `couldBeARC() = false` |
| `store [init]` | ARC type, retain new (no release of old — slot was uninitialized) | `var x = expr` initialization |
| `store [assign]` | ARC type, retain new, release old | `self.field = expr` (line 3274 in lower.zig) |

**Audit checklist for `compiler/frontend/lower.zig`:**
- [ ] `const x = self.field` where field is `*T` — must `load [copy]` (retain + cleanup) ✅ FIXED
- [ ] `const x = list.get(i)` where return type contains `*T` field — verify no retain needed (Cot's `couldBeARC(struct) = false` is correct)
- [ ] `self.field = x` through pointer deref — must `store [assign]` (retain new, release old) ✅ EXISTS
- [ ] `@ptrToInt(managed_ptr)` — must transfer ownership (`disableForLocal`) ✅ FIXED (commit 81f65b8)
- [ ] `@intToPtr(*T, raw_int)` — creates borrowed (+0) reference, no retain
- [ ] Function return of managed pointer — must be +1 (caller-owned)
- [ ] Function parameter of managed pointer — is +0 (borrowed, caller retains)

### 2b: Consider `borrow` scope for save/restore patterns

The selfcot's generic lowering does save/restore patterns:
```cot
const saved = self.tree     // retain (load [copy])
self.tree = other           // store [assign]: retain other, release saved? No — release old self.tree
// ... work ...
self.tree = saved           // store [assign]: retain saved, release other
// scope exit: release saved (cleanup)
```

This creates 2 extra retain/release pairs per swap. Swift optimizes this with `borrow` scopes. Consider:
- **Option A:** Accept the overhead (simple, correct)
- **Option B:** Add `@borrow` annotation that emits `load [borrow]` (no retain, no cleanup) — caller guarantees lifetime
- **Option C:** Compiler optimization pass that detects save/restore patterns and eliminates redundant retain/release

**Recommendation:** Option A for now. The overhead is negligible (2 atomic increments per generic function). Option C later as an optimization pass.

---

## Phase 3: Eliminate Phase 3 Re-Checking (Go Pattern)

**Current architecture (Zig reference):**
```
Phase 1: Parse all files
Phase 2: Check all files (populates expr_types per generic instance)
Phase 3: Lower all files
  └── For each generic: re-check body with fresh expr_types, THEN lower body
```

**Go's architecture:**
```
Phase 1: Parse
Phase 2: Type-check (includes generic stenciling — all instances fully resolved)
Phase 3: Convert to IR (no re-checking needed)
Phase 4: SSA (per-function, independent)
```

**The problem:** Phase 3 re-checking (`checkFnDeclBody` from `lowerGenericFnInstance`) exists because generic bodies share AST nodes but need per-instance `expr_types`. The re-check walks the body AST and populates `expr_types` for THIS instantiation's concrete types. The lowerer then reads `expr_types` to know each sub-expression's type.

**Why checkers persist (from driver.zig audit):** The Zig driver stores checkers in a `checkers` ArrayList (line 450) specifically because lowering needs them for `checkFnDeclWithName`. The checker's scope chain, type substitution, and `expr_types` map are all needed during Phase 3 re-checking. ASTs are kept alive for the entire compilation via `parsed_files` defer block (lines 387-395). If we eliminate re-checking, BOTH checkers and ASTs can be freed after Phase 2.

**The fix:** Eliminate Phase 3 re-checking by making the lowerer resolve types WITHOUT `expr_types`.

### 3a: Extend `resolveTypeNode` fallbacks

The selfcot's `resolveTypeNode` already has fallbacks that resolve types by name when `expr_types` is empty. Extend these to cover ALL expression types the lowerer needs:

1. **Binary ops:** Infer result type from operand types (already done in Zig's `inferExprType`)
2. **Call expressions:** Look up function in checker scope, get return type
3. **Field access:** Resolve base type, look up field type
4. **Index expressions:** Resolve base type, get element type
5. **Method calls:** Resolve receiver type, look up method, get return type

This is the `inferExprType` pattern already implemented in the Zig lowerer.

### 3b: Remove `checkFnDeclBody` call from lowering

Once `resolveTypeNode` fallbacks cover all cases:
1. Remove `self.chk.checkFnDeclBody(inst.generic_node, concrete_name)` from `lowerGenericFnInstanceInner`
2. Remove the `expr_types` save/restore around it
3. Remove the checker AST swap (`self.chk.ast = self.tree`)
4. The lowerer only needs `self.tree` (defining file AST) and `self.types` (shared TypeRegistry)

**Benefits:**
- Eliminates AST lifetime dependency during lowering (Go pattern)
- Eliminates exponential re-checking risk
- Simplifies ARC — no checker field swaps needed
- Reduces memory — no fresh `expr_types` maps allocated per generic

### 3c: Free ASTs after Phase 2 (Go `freePackage` pattern)

Once Phase 3 no longer needs checker.ast:
1. After Phase 2 completes, the checker's `ast` field can be nulled
2. ASTs are only needed by the lowerer's `self.tree` for reading nodes
3. After Phase 3 completes for a file, its AST can be freed
4. Only the IR (Builder's function list) needs to survive to codegen

This matches Go's `freePackage()` — free syntax trees once lowering produces IR.

**Implementation:** Add `ast.free()` method. Call it in `lowerOneFile` after `lowerToBuilder()` returns, before propagating shared state back. The IR is self-contained — it doesn't reference AST nodes.

Wait — the lowerer's `lowerQueuedGenericFunctions` accesses the DEFINING file's AST (`inst.getAst()`). These ASTs must survive until ALL files are lowered (since file N's generics might reference file M's AST). So ASTs can only be freed after ALL files complete Phase 3.

**Revised approach:**
- After ALL Phase 3 files are lowered: free all ASTs and checkers
- Before codegen: only IR and TypeRegistry remain
- This matches Go's "free after type-check" but scoped to "free after lower"

---

## Phase 4: Per-Function SSA with Cache Reuse (Go Pattern)

**Current:** The SSA builder and codegen process one function at a time, but allocate fresh data structures for each. No reuse.

**Go's pattern:**
- `ssa.Cache` object per worker thread
- `Cache.Reset()` between functions — clears used portion, keeps allocations
- `sync.Pool` for value/block slices — 27 size-class pools

### 4a: Add SSA Cache to the Cot SSA builder

```cot
struct SsaCache {
    values: List(SsaValue),      // pre-allocated, reset between functions
    blocks: List(SsaBlock),      // pre-allocated, reset between functions
    // ... other reusable buffers
}

fn reset() void {
    // Clear used portions but keep capacity
    self.values.count = 0
    self.blocks.count = 0
}
```

The SSA builder takes a `*SsaCache` and uses its pre-allocated lists instead of allocating new ones per function.

### 4b: Add growslice cache for hot paths

The selfcot's IR builder allocates `List(int)` frequently (function params, block preds, etc.). These small lists dominate allocation count. Add a free-list for recently-freed small lists:

```cot
struct SmallListPool {
    free_8: List(int),     // pools of List capacity 8
    free_16: List(int),    // pools of List capacity 16
    free_32: List(int),    // pools of List capacity 32
}
```

When a small List is no longer needed, return its backing array to the pool instead of calling `dealloc`. When a new small List is needed, check the pool first.

---

## Phase 5: Memory Profiling Infrastructure

### 5a: Add allocation counter to runtime

Add a global counter incremented by every `alloc()` call. Print between phases:

```cot
// In stdlib/sys.cot or as a compiler-generated global
var __cot_alloc_count: int = 0
var __cot_alloc_bytes: int = 0

fn alloc(metadata: int, size: int) int {
    __cot_alloc_count += 1
    __cot_alloc_bytes += size
    // ... existing alloc logic
}
```

The selfcot can print these between phases to track WHERE allocations happen.

### 5b: Add RSS measurement (macOS/Linux)

For macOS native binaries, call `task_info` via extern to get resident set size:
```cot
extern fn task_info_rss() int  // implemented in io_native.zig
```

For Wasm, not applicable — but the Zig compiler can add a `--profile-memory` flag that instruments the compilation pipeline.

---

## Phase 6: Clean Up Self-Hosted Code

### 6a: Remove all debug instrumentation

- Remove `ast_swap_count` from Checker
- Remove `eprintln` debug traces from checker.cot, lower.cot, main.cot
- Remove `retain(@ptrToInt(...))` / `release(@ptrToInt(...))` manual ARC calls (replaced by compiler fix)
- Remove `zeroBytes` / `copyBytes` hacks in main.cot
- Restore `getNode` to simple `return self.nodes.get(idx)`

### 6b: Simplify lowerOneFile shared state propagation

The current pattern of copying Maps to/from SharedLowerState is fragile. Instead:
- Make SharedLowerState hold pointers to the Maps (not copies)
- The Lowerer gets pointers to the shared Maps directly
- No copy-in/copy-out needed
- No risk of stale pointers after rehash

```cot
struct SharedLowerState {
    builder: *Builder,           // pointer, not value
    lowered_generics: *Map(string, int),  // pointer, shared
    // ...
}
```

This matches the Zig driver pattern where `lowered_generics` is a pointer field.

---

## Execution Order

| # | Phase | Prerequisite | Blocks |
|---|-------|-------------|--------|
| 1 | Fix struct field corruption | None | Everything — current blocker |
| 2 | Complete ARC audit | Phase 1 | Correctness of all ARC code |
| 3a | Extend resolveTypeNode | None | Phase 3b |
| 3b | Remove Phase 3 re-checking | 3a | Phase 3c |
| 3c | Free ASTs after Phase 3 | 3b | Memory optimization |
| 4 | SSA cache reuse | None | Performance |
| 5 | Memory profiling | None | Debugging future issues |
| 6a | Clean up self/ debug | Phase 1 | Code quality |
| 6b | Simplify shared state | Phase 1 | Code quality |

**Critical path:** Phase 1 → Phase 6a → Phase 3a → Phase 3b → Phase 3c

---

## What NOT to Do

- Do NOT add workarounds to self/ code — all fixes in the Zig compiler
- Do NOT add `@arcRetain` / `retain(@ptrToInt(...))` calls in selfcot
- Do NOT add recursion depth limits — fix the architecture instead
- Do NOT add `zeroBytes` / `copyBytes` hacks
- Do NOT increase stack size — it's not the problem (verified: 64MB stack still crashes)
- Do NOT guess at bugs — use lldb, MallocScribble, and allocation counters
- Do NOT over-engineer the SSA cache before the basic pipeline works
