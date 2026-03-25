# Selfcot Bugs — Documented Reproduction + Root Cause

**Date:** 2026-03-18
**Status:** Diagnosed, not yet fixed

---

## Bug 1: Enum method `self` not recognized in selfcot checker

**Reproduction:**
```bash
/tmp/selfcot build self/parse/token.cot -o /tmp/token.wasm
```

**Error:**
```
self/parse/token.cot:52:24: error[E301]: undefined identifier 'self'
            return switch (self) {
                           ^
```

**Source:** `self/parse/token.cot:51-52`
```cot
fn precedence() u8 {
    return switch (self) {
```

**What it is:** Token is an enum. `fn precedence()` is a nested method inside the enum body. In @safe mode, the parser should auto-inject `self: Token` as the first parameter. The Zig compiler handles this correctly — selfcot does not.

**Zig compiler reference:** `compiler/frontend/parser.zig:520-526` — sets `current_impl_type` for nested struct/enum methods so @safe self-injection works.

**Selfcot checker reference:** `self/check/checker.cot` — needs to check how `checkEnumNestedMethods` handles self injection for enum methods.

**Blocked by:** This error cascades into Bug 2 (scope corruption).

---

## Bug 2: Scope chain corruption (SIGSEGV in popScope)

**Reproduction:**
```bash
/tmp/selfcot build self/parse/token.cot -o /tmp/token.wasm
```

**Error:**
```
SIGSEGV: segmentation fault
PC=0x... addr=0x656e696665646e55
fault 0x656e696665646e55
1   selfcot  Checker_popScope + 120
```

**What it is:** The fault address `0x656e696665646e55` = ASCII "Undefine" — a string being dereferenced as a pointer. The scope chain's Map has corrupted data. `popScope` tries to follow a parent pointer that's actually a string.

**Root cause hypothesis:** When Bug 1 triggers (undefined identifier error), the error reporting or recovery path corrupts the scope chain. The checker reports the error, then continues checking, but the scope state is inconsistent. `popScope` then accesses garbage.

**Needs:** Minimal reproduction without token.cot (a simple enum method test that triggers the same error in selfcot).

---

## Bug 3: stdlib/map.cot `self` not recognized in trait impl methods

**Reproduction:**
```bash
/tmp/selfcot check stdlib/map.cot
```

**Error:**
```
stdlib/map.cot:35:22: error[E301]: undefined identifier 'self'
            var h: i64 = self.*
                         ^
```

**Source:** `stdlib/map.cot:33-35`
```cot
impl Hashable for i64 {
    fn hash(self: *i64) i64 {
        var h: i64 = self.*
```

**What it is:** `self` IS explicitly declared as a parameter (`self: *i64`), but the selfcot checker doesn't define it in scope when checking the method body. The Zig compiler handles this correctly.

**Root cause:** `checkFnDeclBody` at line 955 has a `body_check_depth > 1` guard that returns early, skipping parameter definition. The depth counter leaks because `orelse return` at line 958 increments but doesn't decrement on early exit.

**Zig compiler reference:** `compiler/frontend/checker.zig:763-808` (`checkFnDeclWithName`) — defines all params in scope before checking body. No depth counter.

**Affects:** ALL files that import stdlib/map.cot (types, ssa, builder, ir, parser, checker, lower).

---

## Bug 4: `orelse { block with return }` triggers NoCurrentBlock

**Reproduction:**
```bash
# Apply fix to self/check/checker.cot line 958:
#   const sym = self.lookupSymbol(lookup_name) orelse {
#       self.body_check_depth = self.body_check_depth - 1
#       return
#   }
# Then:
cot build self/main.cot -o /tmp/selfcot
```

**Error:**
```
INTERNAL ERROR: NoCurrentBlock in convertNode() during SSA conversion
of 'Checker_checkFnDeclBody' (source offset 38803)
```

**What it is:** The Zig compiler's `lowerOrElseExpr` uses `emitSelect` for pointer-like optional expr fallbacks (line 11174-11179). `emitSelect` evaluates both branches eagerly. When the fallback is a block containing `return`, the return terminates the current block, then `emitSelect` tries to emit in the terminated block.

**Zig compiler reference:** `compiler/frontend/lower.zig:11174-11179` — the pointer-like optional expr path uses select instead of branch+merge.

**Fix:** Replace `emitSelect` with branch+merge for pointer-like optional expr fallbacks, matching the compound optional path at lines 11102-11143 which already has `needsTerminator()` check.

**Status:** Fix identified and tested (compiles, 79/79 tests pass). Not yet committed because it changes orelse codegen for ALL code, needs selfcot regression testing.

---

## Bug 5: ARC refcount regression — use-after-free in multi-file checking (ROOT CAUSE)

**Reproduction:**
```bash
# Current selfcot (with ARC fix):
/tmp/selfcot check /tmp/test_import_sys.cot   # CRASH

# Old selfcot (before ARC fix):
/tmp/selfcot_nostencil check /tmp/test_import_sys.cot   # OK
```

Where test_import_sys.cot is:
```cot
import "std/sys"
fn main() { println(42) }
```

**Error:** SIGSEGV in Scope_lookup → Map.getOrNull with x0 = `0x3173c900e37ae1df` (same garbage every time). Crashes during `collectNonTypeDecl → buildFuncType → resolveTypeExpr` for the imported file.

**Root cause:** Commit `71cbfc9` changed `INITIAL_REFCOUNT` from `STRONG_RC_ONE | PURE_SWIFT_DEALLOC | UNOWNED_RC_ONE` to `PURE_SWIFT_DEALLOC | UNOWNED_RC_ONE`. This reduced the initial strong refcount from 2 logical to 1 logical. The old double-counting masked a missing retain — some ARC-managed object (likely a Scope or Map) was being released too early, causing use-after-free.

**The old selfcot binary was compiled with the old ARC refcount (objects start with 2 refs).** The new selfcot binary is compiled with the fixed refcount (objects start with 1 ref). With 1 ref, a single release frees the object. If there's a missing retain somewhere in the selfcot code (or the Zig compiler's codegen for selfcot), the object gets freed while still in use.

**This is the PRIMARY blocker.** Bugs 1-3 may resolve once this is fixed.

**Investigation:** The missing retain is in the Zig compiler's ARC codegen path for whatever object holds the Scope's Map. Need to trace which ARC-managed object is being freed too early during multi-file checking.

**Zig compiler reference:** `compiler/frontend/lower.zig` (ARC retain/release emission), `compiler/codegen/native/arc_native.zig` (ARC runtime).

## Investigation Order

1. **Bug 5** first — the ARC refcount regression. This is the root cause. The old selfcot works; the new one doesn't. The only difference is the ARC initial refcount. Find the missing retain.
2. **Bug 4** — already fixed (orelse block return lowering).
3. **Bug 3** — already fixed (body_check_depth decrement).
4. **Bug 1** and **Bug 2** — may resolve once Bug 5 is fixed.

## Minimal Test Files Needed

- [ ] `test/selfcot/orelse_block_return.cot` — reproduces Bug 4
- [ ] `test/selfcot/trait_impl_self.cot` — reproduces Bug 3 (selfcot-only)
- [ ] `test/selfcot/enum_method_self.cot` — reproduces Bug 1 (selfcot-only)
