# Selfcot Parity Audit — Every Difference from Zig Compiler

**Date:** 2026-03-17
**Goal:** 99% line-by-line parity with `compiler/frontend/*.zig`

---

## Summary: 146 `@ptrToInt`/`@intToPtr` Hacks, 6 Structural Divergences

The selfcot was NOT ported line-by-line. Previous Claude sessions invented patterns instead of copying the reference. The Zig compiler uses zero `@ptrToInt` — the selfcot uses 146.

---

## Fix 1: Checker Map Fields — `int` → `*Map(K,V)` [~50 hacks]

**Zig:** Direct owned HashMaps
```zig
generic_structs: std.StringHashMap(GenericInfo),
```

**Selfcot (WRONG):**
```cot
generic_structs: int,  // raw ptr to shared Map
@intToPtr(*Map(string, int), self.generic_structs).set(name, @ptrToInt(info))
```

**Selfcot (CORRECT — proven working):**
```cot
generic_structs: *Map(string, *GenericInfo),
self.generic_structs.set(name, info)
```

**Files:** checker.cot (6 fields × ~8 usages each = ~50 casts)
**Status:** [ ] Not started

---

## Fix 2: GenericInfo/GenericInstInfo — `ast_ptr: int` → `ast: *Ast` [~23 hacks]

**Zig:**
```zig
tree: *const Ast,
```

**Selfcot (WRONG):**
```cot
ast_ptr: int,  // @ptrToInt of declaring file's *Ast
retain(@ptrToInt(self.ast))
```

**Selfcot (CORRECT):**
```cot
ast: *Ast,
// ARC handles retain/release automatically
```

**Files:** checker.cot (GenericInfo, GenericInstInfo, GenericImplEntry, Symbol.source_tree)
**Status:** [ ] Not started

---

## Fix 3: Manual `retain`/`release` Calls [23 hacks]

**Zig:** No manual retain/release — Zig doesn't use ARC.

**Selfcot (WRONG):**
```cot
retain(@ptrToInt(self.ast))   // AST escapes as raw int
retain(@ptrToInt(info))       // Keep GenericInfo alive past local scope
```

**Selfcot (CORRECT):** Not needed when using typed pointers. ARC handles it:
```cot
// `new GenericInfo { ast: self.ast }` — ARC retains self.ast in the `new`
self.generic_structs.set(name, info)  // Map.set retains info
```

**Files:** checker.cot (10 retain + 13 release = 23 calls)
**Status:** [ ] Blocked on Fix 1 and Fix 2

---

## Fix 4: Pointer Arithmetic for Struct Fields [~15 hacks]

**Zig:**
```zig
for (struct_type.fields) |field| { ... }
```

**Selfcot (WRONG):**
```cot
const sf = @intToPtr(*StructField, st.fields + fi * @sizeOf(StructField))
```

**Selfcot (CORRECT — use List):**
```cot
// Store fields as List(StructField) instead of raw pointer + count
const sf = st.fields.get(fi)
```

**Files:** checker.cot (~15), types.cot (~8)
**Status:** [ ] Requires types.cot StructType change

---

## Fix 5: Symbol Lookup Returns Index + `@intToPtr` Dereference [~5 hacks]

**Zig:**
```zig
const sym = self.scope.lookup(name) orelse return;
```

**Selfcot (WRONG):**
```cot
const sym_idx = self.lookupSymbol(name)
const sym = self.getSymbol(sym_idx)  // @intToPtr inside getSymbol
```

**Selfcot (CORRECT):** Return `?*Symbol` directly.

**Files:** checker.cot (getSymbol at line 443)
**Status:** [ ] Not started

---

## Fix 6: Edit Distance Pointer Arithmetic [~30 hacks]

**Zig:** Uses allocator + proper arrays for DP table.

**Selfcot (WRONG):** Manual alloc + @intToPtr for every cell access:
```cot
@intToPtr(*int, dp + (i * (n + 1) + j) * 8).* = cost
```

**Selfcot (CORRECT):** Use `List(int)` for the DP table.

**Files:** checker.cot (lines 5292-5440, ~24 @intToPtr casts)
**Status:** [ ] Not started

---

## Fix 7: `allocMap()` Returns `int` [~6 hacks]

**Zig:** Direct HashMap initialization.

**Selfcot (WRONG):**
```cot
fn allocMap() int {
    const ptr = alloc(0, 40)
    @intToPtr(*Map(string, int), ptr).keys = 0
    return ptr
}
```

**Selfcot (CORRECT):**
```cot
// Use `new Map(string, int) { ... }` — returns *Map directly
```

**Files:** checker.cot (allocMap called 7 times in init)
**Status:** [ ] Not started

---

## Fix 8: `copyBytes`/`zeroBytes` in main.cot [~10 hacks]

**Zig:** ArrayListUnmanaged stores checkers directly. No manual copy.

**Selfcot (WRONG):**
```cot
const chk_heap = alloc(0, @sizeOf(Checker))
copyBytes(chk_heap, @ptrToInt(&checker), @sizeOf(Checker))
zeroBytes(@ptrToInt(&checker), @sizeOf(Checker))
```

**Selfcot (CORRECT):** Use `new Checker { ... }` or store `*Checker` in a `List(*Checker)`.

**Files:** main.cot (checkAndStoreChecker, lines 1010-1017)
**Status:** [x] Partially fixed (symbols now shared pointer)

---

## Fix 9: SSA Layer 80% Missing [structural]

**Zig:** 4 files, 1,580 lines, 75 functions
**Selfcot:** 1 file, 625 lines, 16 functions

Missing: majority of SsaValue methods, SsaBlock lifecycle, SsaFunc optimization helpers.

**Status:** [ ] Not started — needs separate audit

---

## Fix 10: Missing Checker Functions [structural]

Functions in Zig but NOT in selfcot:
- `checkVarDecl` (different implementation path)
- Multiple `eval*` comptime functions
- Error suggestion functions (`editDistSuggest`, `errWithSuggestion`)
- Scope iteration helpers

**Status:** [ ] Needs line-by-line audit per function

---

## Fix 11: `new` Doesn't Work with Generics in Selfcot Free Functions [compiler bug]

**Proven:**
```cot
fn makeList() *List(Symbol) {
    return new List(Symbol) { items: 0, count: 0, capacity: 0 }
}
```
Works with Zig compiler, fails with selfcot: `error[E301]: undefined type 'List'`

**Root cause:** Selfcot checker doesn't resolve generic types in free function `new` expressions.

**Files:** selfcot checker.cot (checkNewExpr or resolveGenericInstance)
**Status:** [ ] Not started

---

## Execution Order

1. **Fix 1** (Map fields) — eliminates 50 hacks, unblocks Fix 3
2. **Fix 2** (GenericInfo ast field) — eliminates 23 hacks, unblocks Fix 3
3. **Fix 3** (remove retain/release) — automatic after Fix 1+2
4. **Fix 7** (allocMap) — clean up initialization
5. **Fix 8** (copyBytes) — clean up main.cot
6. **Fix 5** (symbol lookup) — cleaner API
7. **Fix 4** (struct field loops) — requires types.cot change
8. **Fix 6** (edit distance) — isolated, low priority
9. **Fix 11** (selfcot new bug) — compiler fix
10. **Fix 9** (SSA parity) — large, separate effort
11. **Fix 10** (missing functions) — incremental
