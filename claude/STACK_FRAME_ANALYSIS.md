# Stack Frame Size Analysis — 2026-03-17

## Problem

The Zig compiler generates native stack frames 2x larger than necessary for selfcot functions. This causes stack overflow during deep call chains in ir.cot lowering, corrupting data on the stack.

## Hard Data: checkFnDeclBody Locals (3,392 bytes = 424 slots)

Dumped via instrumented `ssa_builder.zig`:

```
47 locals total (30 with overlap groups, 17 without)

[0]  self              8B   slot=0    og=(0,0)  — self pointer
[1]  idx               8B   slot=1    og=(0,0)  — function parameter
[2]  lookup_name      24B   slot=2    og=(0,0)  — string parameter (ptr+len+cap)
[3]  sym_idx           8B   slot=5    og=(0,0)
[4]  sym               8B   slot=6    og=(0,0)
[5]  return_type       8B   slot=7    og=(0,0)
[6]  __sc_result       8B   slot=8    og=(0,0)  — switch case result temp
[7]  func_t           72B   slot=9    og=(0,0)  — Type union (full copy from types.get)
[8]  __sret_tmp       72B   slot=18   og=(0,0)  — SRET temp for types.get call
[9]  ft               24B   slot=27   og=(4,1)  — FuncType capture
[10] __switch_result  24B   slot=30   og=(4,1)
[11] f                24B   slot=33   og=(5,1)  — inner switch capture (reused slot!)
[12] __struct_tmp     24B   slot=33   og=(5,2)  — shares slot with f (overlap works)
[13] fn_node         176B   slot=36   og=(0,0)  — *** Node union from getNode ***
[14] __sret_tmp      176B   slot=58   og=(0,0)  — *** SRET temp for getNode ***
[15] d               168B   slot=80   og=(6,1)  — Node.decl capture
[16] f               160B   slot=101  og=(7,1)  — Decl.fn_decl capture
[17] old_ret           8B   slot=121  og=(0,0)
[18] pi                8B   slot=122  og=(0,0)  — for loop counter
[19] __for_end         8B   slot=123  og=(0,0)
[20] param_idx         8B   slot=124  og=(0,0)
[21] p_node          176B   slot=125  og=(9,1)  — *** SECOND getNode result ***
[22] __sret_tmp      176B   slot=147  og=(9,1)  — *** SECOND SRET temp ***
[23] e               128B   slot=169  og=(10,1) — Expr capture
[24] fd               80B   slot=185  og=(11,1) — field_def capture
[25-29] pname, ptype_n, ptype, is_substituted, __sc_result (various)
[30] ptype_tag        24B   slot=202  og=(12,1) — string
[31] pt_node         176B   slot=205  og=(13,1) — *** THIRD getNode ***
[32] __sret_tmp      176B   slot=227  og=(13,1) — *** THIRD SRET temp ***
[33] pe              128B   slot=249  og=(14,1) — Expr capture
[34] id               40B   slot=265  og=(15,1) — Ident capture
[35] pt_node         176B   slot=270  og=(17,1) — *** FOURTH getNode ***
[36] __sret_tmp      176B   slot=292  og=(17,1) — *** FOURTH SRET temp ***
[37] pe              128B   slot=314  og=(18,1)
[38] te               40B   slot=330  og=(19,1)
[39] inner_n           8B   slot=335  og=(20,1)
[40] __sc_result       8B   slot=336  og=(20,1)
[41] in_node         176B   slot=337  og=(21,1) — *** FIFTH getNode ***
[42] __sret_tmp      176B   slot=359  og=(21,1) — *** FIFTH SRET temp ***
[43] ie              128B   slot=381  og=(22,1)
[44] iid              40B   slot=397  og=(23,1)
[45] __sret_tmp       88B   slot=402  og=(0,0)
[46] __safe_ref       88B   slot=413  og=(0,0)
```

## Analysis: Where the 3,392 Bytes Come From

### 1. Node union copies via getNode (5 calls × 352 bytes = 1,760 bytes = 52%)

Each `self.ast.getNode(idx)` returns a `Node` union by value (176 bytes via SRET).
The lowerer creates TWO locals per call:
- `fn_node` / `p_node` / `pt_node` / `in_node` — named result local (176B)
- `__sret_tmp` — SRET return buffer (176B)

Total per getNode call: **352 bytes**. Five calls = **1,760 bytes** = 52% of the frame.

### 2. Union variant captures from pattern matching (~1,000 bytes = 29%)

Each `switch (node) { Node.decl |d| => ... }` creates a capture variable:
- `d` (Decl): 168B
- `f` (FnDecl): 160B
- `e` (Expr): 128B × 3 = 384B
- `fd` (FieldDef): 80B
- `id` (Ident): 40B × 2 = 80B
- `te` (TypeExpr): 40B
- Subtotal: ~992 bytes

### 3. Overhead (parameters, temps, loop vars) (~632 bytes = 19%)

## Comparison with Zig Compiler's Own checkFnDeclWithName (1,648 bytes)

The Zig compiler compiles itself with Zig's optimizer, which:
1. **Eliminates SRET double-copies** — the Zig optimizer can place the return value directly in the destination
2. **Uses register allocation for small values** — values ≤ 16 bytes stay in registers
3. **Dead code elimination** — unused switch arms don't allocate locals

## Root Causes in the Cot Native Codegen

### RC1: SRET Double-Copy (52% of bloat)

**File:** `compiler/frontend/lower.zig:7998, 8223, 9189, 9218`

Every method call returning a struct > 8 bytes creates TWO locals:
1. `__sret_tmp` — callee writes the return value here
2. Named local (e.g., `fn_node`) — the value is COPIED from __sret_tmp

**Cranelift reference:** Cranelift uses the destination slot directly as the SRET buffer. No intermediate copy.

**Fix:** When a function call's result is immediately stored to a named local (`const fn_node = self.ast.getNode(idx)`), use the named local's stack slot as the SRET destination directly. Eliminate the `__sret_tmp` allocation.

### RC2: No Liveness-Based Stack Slot Reuse (29% of bloat)

Union capture variables from pattern matching are allocated sequentially. Captures in different switch arms with different overlap groups get separate slots even when they're NOT simultaneously live.

The overlap group system works for captures WITHIN the same switch level, but captures from DIFFERENT levels (nested switches) each get their own group and never share.

**Fix:** After overlap group assignment, run a second pass that merges locals with non-overlapping lifetimes. This is standard "interval graph coloring" for stack slots — the same algorithm used in register allocation.

### RC3: Large Union Values Copied by Value (structural)

The `Node` union in the selfcot is 176 bytes. Every `getNode` returns it by value.
The Zig compiler's `Node` is a tagged union pointer (8 bytes) — it returns a POINTER, not a copy.

**This is a design difference in the selfcot**, not a codegen bug. The selfcot could return `*Node` from `getNode` instead of `Node`, but that requires selfcot code changes.

## Progress Made

### SRET Shared Local (implemented)
- checkFnDeclBody: 3,392 → **2,528 bytes (25% reduction)**
- lowerBlockNode: 2,992 → **2,384 bytes (20% reduction)**
- lowerCall: 2,688 → **2,096 bytes (22% reduction)**

### Infinite Generic Queue Loop (fixed)
`List(809)_get` and `List(809)_append` were failing to lower but not marked as complete, causing the queue to reprocess them forever. Fixed by emitting stub functions when lowering fails.

### If-Else Overlap Groups (implemented)
Added `beginOverlapGroup`/`nextOverlapArm`/`endOverlapGroup` to `lowerIf`, `lowerIfOptional`, `lowerIfOptionalExpr`, and `lowerCatchExpr`.

### Remaining Issue: Depth Limit vs expr_types
- Depth 0: SSA builder errors (unresolved fwd_ref) — not enough type info
- Depth 1: SSA builder errors for some generics — still not enough
- Depth unlimited: Stack overflow (SIGBUS) — too deep
- Depth 2+: Memory blowup (6GB+) — too many re-instantiations

The Zig compiler works without a depth limit because its frames are 2x smaller. Even with our SRET optimization (22% reduction), the frames are still too large.

## Recommended Fix Order

1. **RC1: Eliminate SRET double-copy** — biggest impact (52%), pure codegen fix
   - In `lower.zig` where `__sret_tmp` is created: if the result is immediately assigned to a `const` or `var`, use that local's slot as the SRET target
   - Expected savings: ~1,760 bytes for checkFnDeclBody

2. **RC2: Liveness-based slot reuse** — medium impact (29%), requires analysis pass
   - Add post-overlap-group pass in `ssa_builder.zig` that coalesces locals with non-overlapping liveness
   - Expected savings: ~500-800 bytes

3. **RC3: Return pointers instead of values** — requires selfcot code change
   - `getNode` returns `*Node` or index-based access
   - This is a selfcot architecture decision, not a codegen fix

## Top 20 Largest Functions (from SSA builder debug)

| Function | Locals | Slots | Bytes | Overlap? |
|----------|--------|-------|-------|----------|
| preprocess.expandMov | 55 | 935 | 7,480 | Yes |
| checker.evalComptimeStmts | 100 | 715 | 5,720 | Yes |
| checker.checkFnDeclBody | 47 | 424 | 3,392 | Yes |
| checker.checkSwitchExpr | 77 | 402 | 3,216 | Yes |
| lower.lowerBuiltinCall | 161 | 400 | 3,200 | Yes |
| checker.instantiateNestedMethods | 38 | 395 | 3,160 | Yes |
| checker.checkBuiltinCall | 95 | 392 | 3,136 | Yes |
| ssa_builder.SSABuilder_init | 46 | 376 | 3,008 | Yes |
| scanner.scanOperator | 81 | 372 | 2,976 | Yes |
| lower.lowerGenericFnInstanceInner | 68 | 369 | 2,952 | Yes |
| main.compileWithImports | 64 | 368 | 2,944 | Yes |
| parser.parseType | 66 | 362 | 2,896 | Yes |
| checker.instantiateGenericImplMethods | 50 | 360 | 2,880 | Yes |
| lower.lowerBlockNode | 31 | 357 | 2,856 | Yes |
| preprocess.transformAret | 22 | 352 | 2,816 | Yes |
| checker.collectImplTrait | 32 | 352 | 2,816 | Yes |
| preprocess.pass5DispatchLoop | 30 | 349 | 2,792 | Yes |
| scanner.scanOperator2 | 74 | 343 | 2,744 | Yes |
| wasm_gen.allocateLocals | 77 | 334 | 2,672 | Yes |
| lower.lowerCall | 76 | 329 | 2,632 | Yes |
