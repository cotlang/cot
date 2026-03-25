# TODO Audit — 11 Remaining Items

**Date:** 2026-03-19
**Context:** 51 TODO comments audited. 40 removed (stale/misleading). 11 remain as legitimate future work.

---

## Dependency Graph

```
#1, #3 (callType tail call cases)
   ↓
#8, #9 (re-enable tail call optimization)
   ↓
Wasm 3.0 return_call support
```

Items #2, #4, #5, #6, #7, #10, #11 are independent.

---

## 1. x64 Tail Call Type Classification

**File:** `compiler/codegen/native/isa/x64/inst/mod.zig:1029`
**Effort:** Trivial (1 line)

Add `.return_call_known, .return_call_unknown => CallType.TailCall` to `callType()`. The enum variants already exist (line 675-680). Only the switch case is missing.

**Reference:** Cranelift `isa/x64/inst/mod.rs:1303-1314`
**Blocks:** Tail call optimization (#8)

## 2. ARM64 Stack Probe Loop

**File:** `compiler/codegen/native/isa/aarch64/abi.zig:1397`
**Effort:** Small

When `probe_count > PROBE_MAX_UNROLL` (3), emit a `StackProbeLoop` instruction instead of returning empty. Load start=0 and end=frame_size into registers, emit loop with guard_size step.

**Reference:** Cranelift `isa/aarch64/abi.rs:1230-1246`
**Blocks:** Proper stack probing for deep frames (>48 bytes with guard_size 16)

## 3. ARM64 Tail Call Type Classification

**File:** `compiler/codegen/native/isa/aarch64/inst/mod.zig:1827`
**Effort:** Trivial (1 line)

Add `.return_call, .return_call_ind => CallType.TailCall` to `callType()`. Same pattern as #1.

**Reference:** Cranelift `isa/aarch64/inst/mod.rs:1007-1018`
**Blocks:** Tail call optimization (#9)

## 4. Signed Division Overflow Guard

**File:** `compiler/codegen/native/wasm_to_clif/translator.zig:1016`
**Effort:** Small

Add guard for `MIN_INT / -1` overflow. Currently only guards division by zero. Need to check `rhs == -1 AND lhs == INT_MIN` and trap with `INTEGER_OVERFLOW`.

```
minus_one = iconst(ty, -1)
rhs_is_minus_one = icmp(eq, rhs, minus_one)
int_min = iconst(ty, MIN_VALUE)
lhs_is_int_min = icmp(eq, lhs, int_min)
is_overflow = band(rhs_is_minus_one, lhs_is_int_min)
trapnz(is_overflow, INTEGER_OVERFLOW)
```

**Reference:** Cranelift `func_environ.rs:1090-1115`
**Blocks:** Correct sdiv semantics on platforms that don't auto-trap

## 5. Float Copy Sign

**File:** `compiler/codegen/native/wasm_to_clif/translator.zig:1652`
**Effort:** Small

Currently returns `iconst(0)` — completely wrong. Implement using bitwise operations:

```
For f32: sign_mask = 0x80000000
  result = (a & ~sign_mask) | (b & sign_mask)

For f64: sign_mask = 0x8000000000000000
  result = (a & ~sign_mask) | (b & sign_mask)
```

Bitcast float→int, apply mask, bitcast back.

**Reference:** Cranelift `isa/x64/lower.isle:4257-4273`
**Blocks:** Wasm `f32.copysign` / `f64.copysign` opcodes

## 6. Regalloc Operand Type Unification

**File:** `compiler/codegen/native/machinst/regalloc_adapter.zig:178`
**Effort:** Trivial

`convertOperandSlice` copies VCode operands to a static 256-element buffer. The VCode and regalloc Operand types are identical — the conversion is redundant. Either cast directly or verify the types match and remove the copy.

Risk: static buffer overflows if any instruction has >256 operands (unlikely but fragile).

**Reference:** N/A (code cleanup)
**Blocks:** Nothing currently

## 7. use_def Operand Handling

**File:** `compiler/codegen/native/machinst/regalloc_adapter.zig:201`
**Effort:** Small

`use_def` operands (read-modify-write) are mapped to `.use`, losing the write information. Regalloc2 has no `use_def` kind — it uses separate Use and Def operands with Early/Late positions.

Fix: split `use_def` into two operands — a `Use(early)` and `Def(late)` — matching how regalloc2 handles read-modify-write.

**Reference:** regalloc2 `lib.rs:599-609`
**Blocks:** Correct register allocation for read-modify-write instructions

## 8. Re-enable Wasm Tail Call Optimization (Zig Compiler)

**File:** `compiler/ssa/passes/lower_wasm.zig:389`
**Effort:** Medium

`optimizeTailCalls()` is stubbed out. It converts `call + return` patterns to `wasm_return_call` (Wasm 3.0 tail call). Disabled because the block kind change from `.ret` to `.exit` causes SIGILL in the native pipeline for complex call signatures.

**Dependency:** Requires #1 and #3 (callType fixes) first. Also requires fixing the native wasm_to_clif translator's handling of return_call with many parameters.

**Reference:** Wasm 3.0 spec `return_call` (0x12)
**Blocks:** Wasm 3.0 tail call support

## 9. Re-enable Selfcot Tail Call Optimization

**File:** `self/emit/wasm/lower.cot:246`
**Effort:** Medium

Selfcot equivalent of #8. Same stub, same dependency on native return_call fix.

**Dependency:** #8 must be fixed first, then port to selfcot.
**Blocks:** Selfcot Wasm 3.0 tail call parity

## 10. Comptime Evaluation in Selfcot Checker

**File:** `self/build/lower.cot:811`
**Effort:** Medium

Comptime blocks are detected but not evaluated. Global comptime values are registered without initialization. Need to implement `evalComptimeValue()` in the checker phase to fold comptime expressions at compile time.

**Reference:** Zig compiler's checker evaluates comptime expressions during type checking.
**Blocks:** Comptime global variable initialization in selfcot

## 11. Generic Type Resolution in Free Functions

**File:** `self/check/checker.cot:5547`
**Effort:** Medium

Selfcot can't resolve `new List(Symbol)` in free functions (non-method context). Workaround: `alloc_raw(24) + memset_zero`. The checker's generic resolution assumes method context for constructor calls.

Fix: update selfcot's type resolution to handle generic type constructors in free function scope — the `new T(Args)` syntax needs to work everywhere, not just inside methods.

**Blocks:** Idiomatic memory allocation patterns in selfcot

---

## Priority Order

| Priority | Items | Why |
|----------|-------|-----|
| **P1** | #1, #3 | Trivial — unblocks tail calls |
| **P2** | #4, #5 | Small — correctness (sdiv overflow, fcopysign) |
| **P3** | #6, #7 | Small — code quality (regalloc adapter) |
| **P4** | #2 | Small — stack probing for large frames |
| **P5** | #8, #9 | Medium — requires P1 + native fix |
| **P6** | #10, #11 | Medium — selfcot feature gaps |
