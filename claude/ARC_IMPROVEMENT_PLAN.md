# ARC Improvement Plan — Proper Fix Per Swift Reference

**Date:** 2026-03-17
**Status:** PARTIALLY RESOLVED — RC1 (copy witness) confirmed NOT a bug, RC2 (field assignment ARC) FIXED via `load [copy]` pattern in lower.zig:2448-2470, RC3 (stack overflow) root-caused to native codegen stack frame bloat (see STACK_FRAME_ANALYSIS.md).

**Context:** Self-hosting dogfooding exposed that the Zig compiler's ARC implementation doesn't properly handle managed pointer lifetimes in three scenarios. Instead of fixing these in the Zig compiler, 15+ hacks were added to self/ code. This plan reverts all hacks and fixes the root causes.

---

## Problem Statement

The Cot ARC system correctly handles:
- Local variable retain/release at scope entry/exit
- `@arcRetain`/`@arcRelease` builtins
- Null pointer checks in retain/release

The Cot ARC system DOES NOT handle:
1. **Managed pointer field assignment through pointer dereference** — `self.ast = X` in @safe mode releases the old value, which may free objects still referenced elsewhere
2. **Struct value copies containing managed pointers** — `List.get()` returns a struct by value, the `*Ast` field inside is not retained in the copy
3. **`@ptrToInt` ownership transfer** — converting managed pointer to int strips ARC tracking ✅ FIXED (commit 81f65b8)

---

## Root Causes in the Zig Compiler

### RC1: No copy witness for struct value returns

**Swift reference:** `initializeWithCopy` value witness (HeapObject.cpp)

When a struct containing managed pointer fields is returned by value (e.g., from `List(ParsedFile).get()`), Swift generates a copy witness that retains each managed field in the copy. Cot does not.

**Impact:** `ParsedFile.ast: *Ast` is not retained when `parsed_files.get(fi)` returns a copy. If the copy goes to a function that assigns `self.tree = cf.ast`, the ARC field assignment retains it there. But if the copy is temporary (not stored), the managed field is a borrowed reference with no retain — fine as long as the List outlives the copy. This is actually CORRECT for Cot's current model where struct copies are shallow and untracked.

**Verdict:** NOT a bug. Struct value copies DON'T need copy witnesses because Cot's ARC only tracks top-level managed pointers, not fields inside structs. The `couldBeARC(struct) = false` design is intentional.

### RC2: @safe field assignment emits ARC retain/release

**Impact on self/:** When the selfcot's checker does `self.ast = new_ast`, the Zig compiler generates:
1. Load old value from `self.ast`
2. `@arcRetain(new_ast)`
3. Store `new_ast` to `self.ast`
4. `@arcRelease(old_value)`

If the old AST's refcount drops to 0, it's freed. Other code (GenericInfo.ast_ptr) still references it via raw int.

**The real question:** Does the Zig compiler's lowerer ACTUALLY emit ARC retain/release for `self.field = X` assignments? Let me check by examining the generated IR.

If the lowerer does NOT emit ARC for field stores (which the audit showed — `emitStoreLocalField` is a raw store), then RC2 is NOT the issue. The field assignment is just a raw copy with no refcount change.

### RC3: Stack overflow from deep generic checking

**The real bug:** `checkFnDeclBody` called from Phase 3 `lowerGenericFnInstanceInner` recursively checks callee bodies. Each call pushes ~3-6KB on the 8MB stack. With 20+ levels of generic recursion (List→Map→ensureCapacity→realloc→...), the stack overflows.

**Swift reference:** Swift's SILGen doesn't recursively check function bodies during lowering. Type checking is complete before lowering begins.

**Zig reference:** The Zig compiler does the same re-check but survives because:
1. Zig functions have smaller stack frames (compiled with Zig's optimizer)
2. The Zig checker's `checkFnDeclWithName` is shallower (fewer local variables)

**Proper fix:** The Phase 3 re-check should be iterative, not recursive. When `checkFnDeclBody` encounters a generic call, it should QUEUE the callee for later checking instead of immediately recursing. This matches the `lowerQueuedGenericFunctions` pattern — iterative processing of a worklist.

---

## The Plan

### Step 1: Revert all self/ hacks

Remove every `@arcRetain`, `copyString`, `retain(@ptrToInt(...))`, `zeroBytes`, `in_body_recheck`, dummy function stub, and debug trace from self/. Return self/ to clean code that matches the Zig reference.

### Step 2: Verify what actually causes the ARC issue

Before fixing, VERIFY the hypothesis. The Zig compiler may or may not emit ARC for `self.field = X`. Check the actual generated IR/SSA for a simple test case:

```cot
struct Holder { ptr: *Foo }
fn swap(h: *Holder, new_ptr: *Foo) void {
    h.ptr = new_ptr  // Does this emit retain/release?
}
```

If the lowerer does NOT emit ARC for field stores, the use-after-free has a different cause entirely — and all the @arcRetain hacks were addressing a nonexistent problem.

### Step 3: Fix the actual issue based on Step 2's findings

**If field stores DO emit ARC:**
- Option A: Don't emit ARC for field stores through pointer dereference (match C semantics — the caller manages lifetimes)
- Option B: Emit ARC but add a "borrow" annotation that skips release (like Swift's `unowned`)
- Option C: Make the selfcot code use defer-style restore patterns that balance retains

**If field stores DON'T emit ARC:**
- The use-after-free is from a DIFFERENT source entirely
- Use MallocScribble + the runtime diagnostics to find the actual release site
- May be in the checker's scope cleanup, or in List.set overwriting an entry

### Step 4: Fix the stack overflow properly

The `checkFnDeclBody` recursion should be bounded. Two options:

**Option A (match Zig):** Accept the recursion but ensure the selfcot binary has enough stack. On macOS, use `pthread_attr_setstacksize` for the main thread or create a worker thread with a larger stack.

**Option B (better than Zig):** Make `checkFnDeclBody` iterative:
1. Add a `pending_body_checks: List(PendingCheck)` to the checker
2. When `checkGenericCall` would recurse into `checkFnDeclBody`, add to the pending list instead
3. After the current body check completes, process pending checks iteratively
4. This matches the `lowerQueuedGenericFunctions` worklist pattern

### Step 5: Remove unnecessary Phase 3 re-checking

The Zig compiler re-checks generic bodies in Phase 3 to populate `expr_types` for the lowerer. But the selfcot lowerer has `resolveTypeNode` fallbacks that resolve types without `expr_types`. If we can make these fallbacks sufficient, the Phase 3 re-check becomes unnecessary — eliminating both the stack overflow and the performance issue.

Test: remove the Phase 3 `checkFnDeclBody` call and see which files still compile. For files that break, identify which specific `expr_types` entries are needed and add targeted fallbacks.

---

## Execution Order

1. **Revert self/ hacks** (30 min)
2. **Verify ARC hypothesis** — check if `self.field = X` emits retain/release (30 min)
3. **Fix based on findings** (1-2 hours)
4. **Fix stack overflow** — iterative checkFnDeclBody or skip Phase 3 re-check (1 hour)
5. **Test all 12 frontend files** progressively
6. **Build main.cot** — full self-compilation

---

## What NOT to Do

- Do NOT add `@arcRetain` calls to self/ code
- Do NOT add `retain(@ptrToInt(...))` calls
- Do NOT add `zeroBytes` calls to prevent ARC cleanup
- Do NOT add `copyString` calls for string lifetime extension
- Do NOT add recursion depth limits
- Do NOT add dummy function stubs
- Every fix goes in the Zig compiler (`compiler/`), not the selfcot (`self/`)
