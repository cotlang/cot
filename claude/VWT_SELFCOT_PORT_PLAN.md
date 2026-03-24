# VWT Port to Self-Hosted Compiler (self/)

**Goal:** Port VWT generic lowering from `compiler/frontend/lower.zig` to `self/build/lower.cot`.
After this, selfcot generates the same function names and calling conventions as the Zig compiler.

**Scope:** Generic function lowering only. ARC/VWT witnesses are N/A (wasm skips ARC).

---

## Current State

| | Zig compiler | selfcot |
|---|---|---|
| **Generic strategy** | VWT: one body per base name (`List_append`) | Monomorphization: one body per concrete name (`List(5)_append`) |
| **Name computation** | `computeGenericBaseName` strips `(args)` | None — uses concrete name directly |
| **Metadata params** | Appended after user params (0 placeholder) | Not present |
| **has_indirect_result** | Checked for SRET decision | Not used |
| **Call sites** | Resolve to base name + append metadata | Use concrete name directly |
| **Queue loop** | Checks `hasFunc(base_name)` to dedup | Checks `hasFunc(concrete_name)` |

**Key reference functions (Zig compiler):**
- `lowerGenericFnInstanceVWT` — lower.zig:8864-9011
- `computeGenericBaseName` — lower.zig:9015-9026
- `lowerQueuedGenericFunctions` — lower.zig:8820-8858
- `ensureGenericFnQueued` — lower.zig:8812-8815
- `lowerMethodCall` (generic section) — lower.zig:9310-9351

**Key selfcot functions to modify:**
- `lowerGenericFnInstanceInner` — lower.cot:7649-7760 (REPLACE)
- `lowerQueuedGenericFunctions` — lower.cot:7605-7639 (MODIFY)
- `ensureGenericFnQueued` — lower.cot:7519-7524 (KEEP as-is)
- `lowerGenericFnInstance` — lower.cot:7642-7647 (MODIFY)
- `lowerMethodCall` — lower.cot:~3550-3710 (ADD metadata + base name resolution)

---

## Execution Steps

### Step 1: Add `computeGenericBaseName` to selfcot

**What:** Port `lower.zig:9015-9026` to `self/build/lower.cot`.

**Zig reference:**
```zig
fn computeGenericBaseName(self: *Lowerer, concrete_name: []const u8) ![]const u8 {
    const paren_open = std.mem.indexOf(u8, concrete_name, "(") orelse return concrete_name;
    const paren_close = std.mem.indexOf(u8, concrete_name, ")") orelse return concrete_name;
    if (paren_close + 1 >= concrete_name.len) {
        return concrete_name[0..paren_open];  // "List(5)" → "List"
    }
    // "List(5)_append" → "List" + "_append" → "List_append"
    const prefix = concrete_name[0..paren_open];
    const suffix = concrete_name[paren_close + 1 ..];
    return allocPrint("{s}{s}", .{ prefix, suffix });
}
```

**Cot translation:** Add to Lowerer struct in `self/build/lower.cot`, near `parseAndPushTypeSubs`:
```cot
fn computeGenericBaseName(concrete_name: string) string {
    const paren_open = concrete_name.indexOf("(")
    if (paren_open < 0) { return concrete_name }
    const paren_close = concrete_name.indexOf(")")
    if (paren_close < 0) { return concrete_name }
    if (paren_close + 1 >= concrete_name.len) {
        return concrete_name.slice(0, paren_open)
    }
    const prefix = concrete_name.slice(0, paren_open)
    const suffix = concrete_name.slice(paren_close + 1, concrete_name.len)
    return prefix ++ suffix
}
```

**Test:** Add test block verifying:
- `"List(5)"` → `"List"`
- `"List(5)_append"` → `"List_append"`
- `"Map(5;6)_get"` → `"Map_get"`
- `"foo"` → `"foo"` (no parens = passthrough)

---

### Step 2: Modify `lowerQueuedGenericFunctions` to use base names

**What:** Change the dedup check from `hasFunc(concrete_name)` to `hasFunc(base_name)`.

**Current selfcot (lower.cot:7605-7639):**
```cot
// Current: checks concrete name
if (self.builder.hasFunc(key)) { continue }
```

**Port to match Zig (lower.zig:8846-8853):**
```cot
// VWT: check BASE name (one body per generic definition)
const bn = self.computeGenericBaseName(key)
if (self.builder.hasFunc(bn)) { continue }
```

Both the filter loop AND the process loop need this change.

---

### Step 3: Rewrite `lowerGenericFnInstanceInner` → `lowerGenericFnInstanceVWT`

**What:** Replace the monomorphization function with VWT-style lowering. This is the biggest change.

**Key differences from current `lowerGenericFnInstanceInner`:**

| Aspect | Current (mono) | New (VWT) |
|--------|---------------|-----------|
| Function name | `concrete_name` ("List(5)_append") | `base_name` ("List_append") |
| Early exit | `builder.hasFunc(concrete_name)` | `builder.hasFunc(base_name)` |
| SRET decision | `self.needsSret(return_type)` | `inst.has_indirect_result or self.needsSret(return_type)` |
| After user params | nothing | metadata params (`__metadata_T`, one per type param) |
| Type sub setup | `parseAndPushTypeSubs` (string-based) | Same — but Zig uses `StringHashMap`, selfcot uses `type_sub_keys/vals` |

**Implementation: Rename and rewrite `lowerGenericFnInstanceInner`:**

1. Compute `base_name = self.computeGenericBaseName(concrete_name)`
2. `if (self.builder.hasFunc(base_name)) { return }` — early exit
3. Swap AST (same as current)
4. Extract `fn_decl` (same as current)
5. Parse type subs via `parseAndPushTypeSubs` (same as current)
6. Use preserved expr_types OR re-check (same as current)
7. **CHANGED:** `uses_sret = inst.has_indirect_result or self.needsSret(return_type)`
   - Need to check: does GenericInstInfo in selfcot have `has_indirect_result`?
   - If not, add it (Step 3a)
8. **CHANGED:** `self.builder.startFunc(base_name, ...)` instead of `concrete_name`
9. Add params: SRET, @safe self, user params (same as current)
10. **NEW:** After user params, add metadata params:
    ```cot
    // Parse param names from inst.type_param_names (semicolon-separated)
    // For each param name, add __metadata_{name} param (I64, 8 bytes)
    ```
11. Lower body (same as current)
12. Restore state (same as current)

**Step 3a: Check `has_indirect_result` in selfcot's GenericInstInfo**

Look in `self/check/checker.cot` for GenericInstInfo struct. If `has_indirect_result` is missing, add it and set it during instantiation (mirror checker.zig).

---

### Step 4: Update `lowerMethodCall` to use base names + append metadata

**What:** When calling a generic method, resolve to base name and append metadata args.

**Zig reference (lower.zig:9310-9337):**
```zig
// Resolve to base name for generic calls
const is_generic_call = self.chk.generics.generic_inst_by_name.contains(method_info.func_name);
const method_link_name = if (is_generic_call)
    self.computeGenericBaseName(method_info.func_name)
else
    self.resolveMethodName(...);

// Append metadata placeholder args
if (is_generic_call) {
    if (self.chk.generics.generic_inst_by_name.get(method_info.func_name)) |gi| {
        for (gi.type_args) |_| {
            const meta = fb.emitConstInt(0, I64, span);
            args.append(meta);
        }
    }
}

// SRET: check callee's has_indirect_result flag
const callee_has_indirect = if (is_generic_call)
    (if (generic_inst_by_name.get(func_name)) |gi| gi.has_indirect_result else false)
else false;
if (self.needsSret(return_type) or callee_has_indirect) { ... }
```

**Selfcot port:** Find `lowerMethodCall` in `self/build/lower.cot`. Before the final `fb.emitCall(resolved_method, ...)`:

1. Check if `resolved_method` is a generic instance (lookup in `self.chk.generic_inst_by_name`)
2. If generic: replace `resolved_method` with `self.computeGenericBaseName(resolved_method)`
3. Look up GenericInstInfo, count type_args
4. Append that many `0` (i64 const) metadata args to the args list
5. Check `has_indirect_result` for SRET decision

---

### Step 5: Update non-method generic call sites

**What:** The `lowerCall` function (for non-method calls like `List_ensureCapacity(T)(self, n)`) also needs base name resolution + metadata.

Find all call sites that call `ensureGenericFnQueued` — these are the points where generic calls are detected. Each one needs:
1. Base name resolution for the call target
2. Metadata arg appending

**Call sites in selfcot (from grep):**
- lower.cot:3473 — likely in lowerCall for direct generic function calls
- lower.cot:3712 — likely in lowerMethodCall
- lower.cot:6703 — likely in another call path

Each site that currently emits `fb.emitCall(concrete_name, ...)` needs to emit `fb.emitCall(base_name, ..., metadata_args)` instead.

---

### Step 6: Update `self/main.cot` SharedLowerState (if needed)

**What:** `SharedLowerState` propagates `lowered_generics` across files. This should continue working as-is since `lowered_generics` maps concrete names → queued status, and the queue processor resolves to base names. No changes expected.

**Verify:** After Steps 1-5, multi-file builds still work (selfcot compiles all 42 self/ files).

---

### Step 7: Test

```bash
# 1. Build Zig compiler
zig build

# 2. Build selfcot
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot

# 3. Verify selfcot works
/tmp/selfcot version

# 4. Run selfcot's own test suite
/tmp/selfcot test test/cases/arithmetic.cot
/tmp/selfcot test test/cases/structs.cot

# 5. Compare function names: Zig compiler vs selfcot output
# Both should now emit "List_append" (base) not "List(5)_append" (concrete)
./zig-out/bin/cot build self/parse/scanner.cot --target=wasm -o /tmp/zig_out.wasm
/tmp/selfcot build self/parse/scanner.cot -o /tmp/selfcot_out.wasm
# Compare exported function names — should match

# 6. Selfcot2 build
/tmp/selfcot build self/main.cot -o /tmp/selfcot2.wasm
```

---

## Risks and Mitigations

1. **`has_indirect_result` missing from selfcot's GenericInstInfo**
   - Check `self/check/checker.cot` for the struct definition
   - If missing, add the field and set it during `instantiateGenericImplMethods` / `resolveGenericInstance`
   - Mirror `compiler/frontend/checker.zig` where `isTypeParamName` is used

2. **String-based type substitution vs HashMap**
   - Zig: `type_substitution: ?StringHashMap(TypeIndex)` — HashMap of name→TypeIndex
   - Selfcot: `type_sub_keys: List(string)` + `type_sub_vals: List(int)` — parallel lists
   - Both achieve the same result; no structural change needed in selfcot

3. **Metadata params unused on wasm**
   - The metadata params are `0` placeholders. On wasm, VWT witnesses aren't emitted.
   - The params just add an extra i64=0 per type param to each generic call.
   - This is correct — the calling convention must match between selfcot and Zig compiler output.

4. **Multi-file generic dedup with base names**
   - `lowered_generics` tracks CONCRETE names (e.g., "List(5)_append")
   - `builder.hasFunc` now checks BASE names (e.g., "List_append")
   - Both "List(5)_append" and "List(17)_append" map to "List_append" → correct dedup

5. **@safe self param injection with base name**
   - Current code uses `concrete_name` to extract struct name (before last "_")
   - Must keep using `concrete_name` (or `inst.concrete_name`) for struct resolution,
     NOT `base_name` — the struct name is in the concrete name

---

## File Change Summary

| File | Change | Lines |
|------|--------|-------|
| `self/build/lower.cot` | Add `computeGenericBaseName`, rewrite `lowerGenericFnInstanceInner` → VWT, update `lowerQueuedGenericFunctions`, update `lowerMethodCall` + call sites | ~150 lines changed |
| `self/check/checker.cot` | Add `has_indirect_result` to GenericInstInfo if missing | ~10 lines |
| `self/main.cot` | No changes expected (SharedLowerState unchanged) | 0 |

**Total estimate: ~160 lines changed, 0 new files.**
