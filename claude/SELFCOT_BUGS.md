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

## Investigation Order

1. **Bug 4** first — fix the Zig compiler's orelse lowering. This unblocks Bug 3's fix.
2. **Bug 3** second — fix the depth counter leak in selfcot's checker. This unblocks map.cot checking.
3. **Bug 1** third — fix selfcot's enum method self injection. This unblocks token.cot.
4. **Bug 2** last — may resolve automatically once Bugs 1 and 3 are fixed (the scope corruption is likely caused by error recovery after the other bugs).

## Minimal Test Files Needed

- [ ] `test/selfcot/orelse_block_return.cot` — reproduces Bug 4
- [ ] `test/selfcot/trait_impl_self.cot` — reproduces Bug 3 (selfcot-only)
- [ ] `test/selfcot/enum_method_self.cot` — reproduces Bug 1 (selfcot-only)
