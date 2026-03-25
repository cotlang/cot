# Selfcot Scope Corruption: Root Cause Analysis

**Date:** 2026-03-19
**Status:** Root cause identified, fix requires ?*T unwrap-then-retain pattern
**Blocks:** All selfcot compilation (0/13 files compile)

---

## Reproduction

```bash
# Minimal test — no imports, trivial function:
cat > /tmp/test.cot << 'EOF'
fn add(a: i64, b: i64) i64 { return a + b }
fn main() { println(add(1, 2)) }
EOF

# Old selfcot (pre-ARC fix): PASSES
/tmp/selfcot_nostencil check /tmp/test.cot   # ok

# New selfcot (post-ARC fix): CRASHES
/tmp/selfcot check /tmp/test.cot
# error[E301]: undefined identifier 'a'
# SIGSEGV in Scope_lookup, x0 = 0x656e696665646ea5 ("Undefine...")

# Trivial file WITHOUT params: PASSES
echo 'fn main() { println(42) }' > /tmp/trivial.cot
/tmp/selfcot check /tmp/trivial.cot   # ok
```

The crash happens during `checkFnDeclBody → pushScope → defineSymbol → ... → lookupSymbol → Scope_lookup → Map.getOrNull`. The Scope's Map contains freed memory (error message strings as pointer values).

---

## Root Cause

### The Chain of Events

1. `checkFnDeclBody` calls `pushScope()`
2. `pushScope()` does `self.scope = newScope(self.scope)`
3. `newScope(parent)` creates `new Scope { parent: parent, symbols: .{} }`
4. The `parent: ?*Scope` field stores the old scope pointer WITHOUT retaining it
5. Back in `pushScope`, the assignment `self.scope = newScope(...)` is a managed assign:
   - Load old value (old scope)
   - Store new value (new scope)
   - **Release old value** ← this frees the old scope
6. The old scope's refcount drops from 1 → 0 → freed
7. But the new scope's `parent` field still points to the freed old scope
8. Later, `Scope_lookup` traverses `parent` → accesses freed memory → SIGSEGV

### Why Old Selfcot Worked

Old `INITIAL_REFCOUNT = 0x200000003` (StrongExtra=1, logical 2 strong refs).
New `INITIAL_REFCOUNT = 0x3` (StrongExtra=0, logical 1 strong ref).

With 2 logical refs, the release at step 5 drops to 1 — scope survives.
With 1 logical ref (correct per Swift RefCountBits(0,1)), the release drops to 0 — freed.

### The Missing Retain

**Swift reference:** `TypeLowering.cpp:1388-1422` — `emitCopyValue` for aggregate types destructures the struct and retains each non-trivial field. When `new Scope { parent: old_scope }` initializes the `parent` field, Swift would retain `old_scope` via `store [init]` which calls `emitCopyValue` on the stored value.

**Cot's bug:** `lowerNewExpr` (lower.zig:6244) only retains for `.pointer` fields:
```zig
if (self.type_reg.get(struct_field.type_idx) == .pointer and
    self.type_reg.get(struct_field.type_idx).pointer.managed) {
    // retain here
}
```

The `parent: ?*Scope` field is `.optional`, not `.pointer`. The retain check MISSES it.

### Why Simple Fix Doesn't Work

Adding `isPtrLikeOptional` check to the condition correctly identifies `?*Scope` as needing retain. But `?*Scope` is a **16-byte compound optional** (tag + pointer). You can't call `retain(compound_value)` — retain expects an 8-byte pointer.

The fix requires the same unwrap-then-retain pattern used by the cleanup stack's `?*T` release:
1. Load the tag from the compound optional
2. If tag != 0 (non-null): extract the inner pointer, call `retain(inner_ptr)`
3. If tag == 0 (null): skip retain

This matches Swift's `LoadableEnumTypeLowering::emitCopyValue` (TypeLowering.cpp:1603) which switches on the enum tag and copies each non-trivial payload.

---

## Affected Code Paths

The missing retain affects ALL struct init paths where a `?*T` (managed optional pointer) field is initialized:

| Path | File | Line | Status |
|------|------|------|--------|
| `lowerNewExpr` field init | lower.zig | ~6244 | Missing ?*T retain |
| `lowerStructInitExpr` field init | lower.zig | ~5860 | Missing ?*T retain |
| `lowerStructInit` field init | lower.zig | ~2752 | Has auto-deref but no ?*T retain |

All three paths need the unwrap-then-retain pattern for `?*T` fields.

---

## Evidence

### Disassembly: newScope stores parent WITHOUT retain

```asm
; selfcot newScope:
100061148: ldr x0, [sp]         ; x0 = parent (?*Scope value)
100061150: add x1, x24, #0x0    ; x1 = alloc_result + 0 (parent field offset)
100061154: str x0, [x1]         ; store parent — NO RETAIN
```

No `bl _retain` between the load and store. Compare with Swift which would emit `retain_value` or `copy_value` here.

### Disassembly: pushScope releases old scope

```asm
; selfcot pushScope:
100062d38: ldr x20, [x0]        ; x20 = old self.scope (saved for release)
...
100062d54: str x0, [x1]         ; self.scope = new_scope
100062d58: mov x0, x20           ; x0 = old scope
100062d5c: bl _release           ; release(old_scope) → refcount 1→0 → FREED
```

After release, old scope is freed. But new scope's `parent` points to it.

### Memory contents at crash

`x0 = 0x656e696665646ea5` = ASCII "Undefine" — the freed scope's memory was reused for an error message string. `Scope_lookup` reads `self.symbols.states` from this garbage and crashes.

---

## Fix: Unwrap-Then-Retain for ?*T Fields in Struct Init

**Reference:** Swift `LoadableEnumTypeLowering::emitCopyValue` (TypeLowering.cpp:1603)

For each struct field init where the field type is `?*T` with managed inner pointer:

```
// Pseudocode for the fix:
if (field_type is ?*T where T is managed) {
    // Store the compound optional value first
    store field_value at field_offset

    // Then retain the inner pointer if non-null
    tag = load_i64(field_addr + 0)       // tag at offset 0
    if (tag != 0) {
        inner_ptr = load_i64(field_addr + 8)  // pointer at offset 8
        retain(inner_ptr)
    }
}
```

This pattern:
1. Stores the full 16-byte compound optional (tag + pointer)
2. Checks if non-null (tag != 0)
3. Retains the inner pointer to prevent premature deallocation

The retain ensures the pointed-to object survives when the source reference is released elsewhere (e.g., `pushScope`'s assign-release of old scope).

---

## Verification Plan

1. Implement unwrap-then-retain in `lowerNewExpr` for ?*T fields
2. Rebuild selfcot: `cot build self/main.cot -o /tmp/selfcot`
3. Test: `/tmp/selfcot check /tmp/test.cot` where test.cot has `fn add(a, b) { return a + b }`
4. If passes, test all 13 frontend files
5. Write regression test (requires non-self-referential version since selfcot can't handle those)

## Related Bugs

- **Double evaluation in lowerAssign** (claude/ASSIGN_DOUBLE_EVAL.md) — `pushScope` called `newScope` twice. Fixed. Not the root cause but a related ARC bug.
- **8-byte struct field value** — `convertFieldValue` returned address instead of value. Fixed. Not the root cause.
- **@safe struct init auto-deref** — `*T` stored where `T` expected. Fixed. Not the root cause.
- **body_check_depth leak** — `orelse return` didn't decrement counter. Fixed (requires orelse block lowering fix).
