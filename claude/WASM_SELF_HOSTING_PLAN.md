# Wasm Self-Hosting Plan

**Date:** 2026-03-16 (updated end of session)
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm binary that can compile itself.
**Metric:** `wasmtime /tmp/selfcot.wasm build self/test_tiny.cot -o /tmp/out.wasm` succeeds.

---

## Current State (End of Mar 16 Session)

**Compiles successfully (6 of 12 frontend files):**
| File | Lines | RSS | Time |
|------|-------|-----|------|
| token.cot | 448 | 5MB | <0.01s |
| source.cot | 315 | ~5MB | <0.01s |
| errors.cot | 545 | ~5MB | <0.01s |
| types.cot | 1,572 | ~10MB | ~0.1s |
| scanner.cot | 774 | ~10MB | ~0.1s |
| ast.cot | 1,532 | ~10MB | ~0.1s |

**Crashes:**
| File | Lines | Issue |
|------|-------|-------|
| ir.cot | 1,451 | `retain()` on freed pointer (ARC use-after-free) — compiles in 0.5s/15MB but crashes |
| ssa.cot | 619 | Untested (likely same ARC issue) |
| ssa_builder.cot | 2,337 | Untested |
| parser.cot | 3,235 | Untested |
| checker.cot | 5,907 | Untested |
| lower.cot | 9,177 | Untested |

**Also working:**
- `selfcot check self/main.cot` — PASS (38 files, all type-checking passes)
- `selfcot build self/test_tiny.cot` — PASS (valid Wasm, 3MB)

---

## Current Blocker: ARC Use-After-Free

**Symptom:** `retain()` called on freed memory during ir.cot compilation. Crash in `retain + 52` with `EXC_BAD_ACCESS (code=2)`.

**Root cause:** The Cot ARC system frees managed pointers when locals go out of scope. But when code stores a managed pointer as a raw int via `@ptrToInt` (e.g., in a Map value), ARC doesn't know the int IS a pointer. When the local goes out of scope, ARC decrements the refcount to 0 and frees the object. The Map still holds the raw int, which is now a dangling pointer.

**Where it happens:** `checkAndStoreChecker` in main.cot creates a Checker on the stack, copies it to heap, but ARC frees the stack copy's managed fields. The `zeroBytes` fix handles this specific case, but there are other paths where managed objects in the checker/lowerer get freed while raw pointer references survive.

**The fix needed:** Either:
1. Make `@ptrToInt` emit a `retain` automatically (ownership transfer) — requires Zig compiler change in `lower.zig`
2. Or: the selfcot code must manually `retain()` every managed pointer before `@ptrToInt` (tedious, error-prone)
3. Or: use `alloc` directly instead of `new` for objects that need to outlive their declaring scope (bypass ARC entirely for these specific allocations)

**Reference:** Swift's `Unmanaged<T>` pattern — explicitly manages ownership transfer for `UnsafeRawPointer` conversions.

---

## Fixes Applied This Session (22 commits, Mar 15-16)

### Zig Compiler Fixes
1. **wasm32 gc=false** — linear memory, not WasmGC (fixed CI — all Wasm tests green)
2. **ARC skip on Wasm** — 10 guards changed from `isWasmGC()` to `isWasm()` (retain/release don't exist on Wasm)
3. **Signal handlers for native** — `__cot_signal_handler`, `__cot_install_signals` (CLIF IR, signal() libc)
4. **@trap file:line diagnostics** — every @trap prints source location before halting
5. **Stack traces** — `__cot_print_backtrace` using `backtrace_symbols_fd` with symbol resolution
6. **@trap source location fix** — correct filename for cross-file generics (bounds check on span offset)

### Selfcot Fixes
7. **Infinite generic lowering loop** — spurious `self.builder.func() orelse return` guard removed (6.5GB → 10MB)
8. **@safe self param type** — parser creates `type_expr(TYPE_NAMED)` instead of `Expr.ident` for self params
9. **resolveTypeNode Expr.ident fallback** — resolve by NAME not AST node index (prevents cross-file mismatch)
10. **SSA pred edge block index** — store INDEX not ID in predecessor edges
11. **Unreachable block verify skip** — don't verify fwd_refs in 0-pred blocks
12. **null_node handling** — convertNode returns null_value for out-of-range indices
13. **emitMove load-unwrap removed** — was an INVENTION not in Zig reference (per TROUBLESHOOTING.md)
14. **null_value guards in wasm codegen** — computeUses, getValue32/64 skip invalid indices
15. **generateGlobalInitsNamed guard** — skip cross-file AST node indices
16. **checkFnDeclBody sym.type_idx guard** — prevent crash on invalid type index
17. **GenericInstInfo map key lifetime** — copyString for ARC safety
18. **Exponential generic re-checking fix** — isDefined guard matching Zig reference
19. **Phase 3 re-check guard** — skip checkFnDeclBody if builder already has the function (20 min → 0.5s)
20. **ARC zeroBytes** — zero stack checker after heap copy to prevent ARC freeing shared data

---

## Steps to Complete Self-Hosting

### Step 1: Fix ARC use-after-free for ir.cot
The `retain()` crash in ir.cot is the primary blocker. Fix the ARC lifetime management for `@ptrToInt` conversions.

### Step 2: Compile all 12 frontend files
Once ir.cot compiles, test ssa.cot → ssa_builder.cot → parser.cot → checker.cot → lower.cot progressively.

### Step 3: Compile codegen + runtime files
Test all files in `self/codegen/wasm/` and `self/ssa/passes/`.

### Step 4: Build main.cot (full self-compilation)
`/tmp/selfcot build self/main.cot -o /tmp/selfcot_gen1.wasm`

### Step 5: Test and bootstrap
Run gen1 Wasm binary with wasmtime. Compile itself to gen2. Verify gen1 == gen2 (fixed point).
