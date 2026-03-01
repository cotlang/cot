# Compiler Bugs Found While Building cot.land Package Registry

**Project**: `~/cot-land/pkg` — package registry written in Cot
**Found**: 2026-03-01

---

## 1. Module-Level `var`/`const` String Values Are Empty at Runtime — FIXED

**Priority**: HIGH — affects any module that uses module-level string constants
**Status**: FIXED (2026-03-01) — `__cot_init_globals` init function pattern

### Root Cause

`lowerGlobalVarDecl` in `compiler/frontend/lower.zig` used the init expression for type inference, then created a zero-initialized global without emitting any store code. Integer/float `const` declarations worked because the checker const-folds them (inlined at use sites), but `var` declarations and string `const` declarations were silently zeroed.

### Fix

Go init function pattern: generate a `__cot_init_globals` function containing `global_store` operations for each non-folded initializer. Called at the start of every entry point (main, test runner, bench runner).

- `compiler/frontend/lower.zig` — `GlobalInit` struct, `pending_global_inits` list, `generateGlobalInits()`, entry point injection
- `compiler/driver.zig` — call `generateGlobalInits()` once per compilation (single-file and multi-file paths)

### Original Symptom

```cot
var VERSION: string = "0.1.0"

fn main() void {
    info(VERSION)            // prints empty string
    var sb: StringBuilder = .{}
    sb.append(VERSION)       // appends nothing (length 0)
    info(sb.toString())      // prints empty string
}
```

---

## 2. Module-Level `var` Integer Values Have Garbage Upper Bits — FIXED

**Priority**: MEDIUM
**Status**: FIXED (2026-03-01) — same root cause as Bug #1

### Root Cause

Same as Bug #1 — global variable initializers were never emitted. The "garbage upper bits" were from reading uninitialized memory. Now that `__cot_init_globals` properly stores values, integers are correct.

### Original Symptom

```cot
var DEFAULT_PORT: int = 8080
// intToString(DEFAULT_PORT) → "8589942672" (garbage upper bits)
```

---

## 3. String Interpolation Doesn't Work in Native Codegen

**Priority**: MEDIUM — string interpolation `"text ${expr}"` produces incorrect output in compiled binaries
**Status**: OPEN — may be partially fixed by Bug #1/#2 fix, needs re-testing

### Symptom

```cot
var port = 8080
info("listening on :${intToString(port)}")
// Output: "listening on :8589942672"  (same garbage-bits issue)
```

Even with local variables, string interpolation with function calls produces incorrect values. This may be a separate issue from Bug #2, or it may interact with how the interpolation desugaring passes values.

### Verified behavior

| Pattern | Result |
|---|---|
| `"text ${intToString(local_var)}"` | Garbage — **BUG** |
| `sb.append("text"); sb.appendInt(local_var); sb.toString()` | Correct |
| `"literal string"` (no interpolation) | Correct |

### Workaround

Use StringBuilder instead of string interpolation for dynamic content.

---

## Summary

| Bug | Severity | Status | Root Cause |
|-----|----------|--------|------------|
| Module-level string globals empty | HIGH | FIXED | Missing global init stores → `__cot_init_globals` |
| Module-level int globals garbage bits | MEDIUM | FIXED | Same root cause |
| String interpolation broken | MEDIUM | OPEN | Interpolation desugaring or codegen |
