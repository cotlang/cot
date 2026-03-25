# Selfcot Crash Investigation — RESOLVED

**Date:** 2026-03-19
**Status:** Both bugs fixed and verified

---

## Bug 1: `dealloc()` called on `alloc_raw()` memory — FIXED

**File:** `self/check/checker.cot` — `editDistance()` function

`editDistance()` allocated with `alloc_raw()` (no ARC header) but freed with `dealloc()` (expects ARC header at ptr-32). This caused `POINTER_BEING_FREED_WAS_NOT_ALLOCATED` from libmalloc.

**Fix:** Changed `dealloc(prev_ptr)` and `dealloc(curr_ptr)` to `dealloc_raw(prev_ptr)` and `dealloc_raw(curr_ptr)`.

**Audit:** Searched all `self/` for `dealloc(` calls — these were the only two mismatches. All other `alloc_raw` allocations either use `dealloc_raw` or are intentionally leaked (heap-copy pattern in main.cot).

---

## Bug 2: Map state accessed as `*u8` instead of `*i64` — FIXED

**File:** `self/check/checker.cot` — `findSimilarType()` function

The `findSimilarType()` function manually iterated over the Map's internal arrays but read states as `*u8` (1 byte) instead of `*i64` (8 bytes). Since Map stores states as i64 values, this caused completely wrong state/key slot correspondence:

```
// BUG: reads byte `ki` of the states array (1 byte per access)
if (@intToPtr(*u8, self.types.name_map.states + ki).* != 1) { continue }

// FIX: reads i64 at slot `ki` (8 bytes per access, matching Map internals)
if (@intToPtr(*i64, self.types.name_map.states + ki * @sizeOf(i64)).* != 1) { continue }
```

**Root cause analysis:** On little-endian (ARM64), state value `1` is stored as bytes `01 00 00 00 00 00 00 00`. Reading byte `ki` of the states array only finds occupied entries at multiples of 8 (where byte 0 of each i64 state is read). But the corresponding key access reads slot `ki`, not slot `ki/8`. This means:

- `ki=0`: state[0] byte 0 → key[0] → **correct match**
- `ki=8`: state[1] byte 0 → key[8] → **wrong key** (should be key[1])
- `ki=496`: state[62] byte 0 → key[496] → **reads uninitialized memory**

Without `MallocScribble`, uninitialized key slots happened to contain zeros or stale valid data, so `editDistance` returned high values (no suggestion shown) or didn't crash. With `MallocScribble=1`, uninitialized memory was filled with `0xAA`, causing `@lenOf()` to read massive garbage lengths and crash in `alloc_raw`.

**Fix:** Both state reads in `findSimilarType()` (name_map and generic_structs loops) changed from `*u8` to `*i64` with proper `@sizeOf(i64)` stride.

**Symptoms before fix:**
- With `MallocScribble=1`: SIGSEGV in `alloc_raw` (garbage size from corrupt string length)
- Without `MallocScribble`: No crash, but "did you mean?" suggestions never appeared for type typos

**After fix:** `MallocScribble=1` passes cleanly, and "did you mean 'MyStruct'?" suggestions now work correctly.

---

## Verification

All three test cases pass with `MallocScribble=1`:

```bash
# Test 1: field access typo → "field not found" (uses findSimilarField, was already correct)
# Test 2: type name typo with imports → "undefined type 'MyStrct', did you mean 'MyStruct'?"
# Test 3: type name typo without imports → "undefined type 'MyStrct', did you mean 'MyStruct'?"
```

Selfcot still compiles its own files (scanner.cot, types.cot, ast.cot) successfully.

---

## NOT an ARC bug

Initial suspicion was that ARC was releasing strings prematurely (use-after-free). Investigation proved both bugs were manual pointer arithmetic errors in the selfcot Cot code, not compiler/ARC issues:

1. Bug 1: Wrong dealloc variant (`dealloc` vs `dealloc_raw`)
2. Bug 2: Wrong pointer cast type for Map state access (`*u8` vs `*i64`)
