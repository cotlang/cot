# Audit: lower_wasm.zig

## Status: PARTIAL IMPLEMENTATION

| Metric | Value |
|--------|-------|
| Lines | 510 |
| Go Reference | cmd/compile/internal/ssa/rewriteWasm.go (1800+ lines) |
| Tests | 9 unit tests |
| E2E Status | Basic lowering works |

---

## Purpose

This pass transforms generic SSA ops (add, sub, load) into Wasm-specific ops (wasm_i64_add, wasm_i64_sub, wasm_i64_load).

**Go reference**: `rewriteWasm.go` contains auto-generated rewrite rules from `Wasm.rules`.

---

## Go's Approach vs Ours

### Go's rewriteWasm.go

Go generates rewrite rules from `Wasm.rules`:
```go
func rewriteValueWasm(v *Value) bool {
    switch v.Op {
    case OpAdd64:
        return rewriteValueWasm_OpAdd64(v)
    case OpLoad:
        return rewriteValueWasm_OpLoad(v)
    // ... hundreds of cases
    }
}

func rewriteValueWasm_OpAdd64(v *Value) bool {
    // v.Op == OpAdd64
    // Result: (I64Add x y)
    v.reset(OpWasmI64Add)
    return true
}
```

### Our lower_wasm.zig

We use a direct switch statement:
```zig
fn lowerValue(v: *Value) bool {
    const new_op: ?Op = switch (v.op) {
        .add, .add64 => .wasm_i64_add,
        .sub, .sub64 => .wasm_i64_sub,
        .load, .load64 => .wasm_i64_load,
        // ...
    };
    if (new_op) |op| { v.op = op; return true; }
    return false;
}
```

---

## Function Comparison

| Go Function | Go Lines | Our Function | Our Lines | Parity |
|-------------|----------|--------------|-----------|--------|
| `rewriteValueWasm` | ~50 (dispatch) | `lowerValue` | 43-314 | **PARTIAL** |
| `rewriteValueWasm_OpAdd64` | 6 | inline switch | 1 | **YES** |
| `rewriteValueWasm_OpLoad` | 15 | inline switch | 1 | **PARTIAL** |
| `rewriteBlockWasm` | ~20 | N/A | N/A | **MISSING** |

---

## Op Lowering Comparison

### Constants

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpConst64` | `OpWasmI64Const` | `const_int, const_64` | `wasm_i64_const` | **YES** |
| `OpConst32` | `OpWasmI64Const` | `const_32` | `wasm_i32_const` | **DIFFERENT** (we use i32) |
| `OpConstFloat` | `OpWasmF64Const` | `const_float` | `wasm_f64_const` | **YES** |

### Integer Arithmetic

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpAdd64` | `OpWasmI64Add` | `add, add64` | `wasm_i64_add` | **YES** |
| `OpSub64` | `OpWasmI64Sub` | `sub, sub64` | `wasm_i64_sub` | **YES** |
| `OpMul64` | `OpWasmI64Mul` | `mul, mul64` | `wasm_i64_mul` | **YES** |
| `OpDiv64` | `OpWasmI64DivS` | `div` | `wasm_i64_div_s` | **YES** |
| `OpDiv64u` | `OpWasmI64DivU` | `udiv` | `wasm_i64_div_u` | **YES** |
| `OpMod64` | `OpWasmI64RemS` | `mod` | `wasm_i64_rem_s` | **YES** |
| `OpMod64u` | `OpWasmI64RemU` | `umod` | `wasm_i64_rem_u` | **YES** |

### Integer Bitwise

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpAnd64` | `OpWasmI64And` | `and_, and64` | `wasm_i64_and` | **YES** |
| `OpOr64` | `OpWasmI64Or` | `or_, or64` | `wasm_i64_or` | **YES** |
| `OpXor64` | `OpWasmI64Xor` | `xor, xor64` | `wasm_i64_xor` | **YES** |
| `OpLsh64x64` | `OpWasmI64Shl` | `shl, shl64` | `wasm_i64_shl` | **YES** |
| `OpRsh64Ux64` | `OpWasmI64ShrU` | `shr, shr64` | `wasm_i64_shr_u` | **YES** |
| `OpRsh64x64` | `OpWasmI64ShrS` | `sar, sar64` | `wasm_i64_shr_s` | **YES** |

### Integer Comparisons

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpEq64` | `OpWasmI64Eq` | `eq, eq64` | `wasm_i64_eq` | **YES** |
| `OpNeq64` | `OpWasmI64Ne` | `ne, ne64` | `wasm_i64_ne` | **YES** |
| `OpLess64` | `OpWasmI64LtS` | `lt, lt64` | `wasm_i64_lt_s` | **YES** |
| `OpLeq64` | `OpWasmI64LeS` | `le, le64` | `wasm_i64_le_s` | **YES** |
| `OpLess64U` | `OpWasmI64LtU` | `ult` | `wasm_i64_lt_u` | **YES** |
| `OpLeq64U` | `OpWasmI64LeU` | `ule` | `wasm_i64_le_u` | **YES** |

### Float Arithmetic

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpAdd64F` | `OpWasmF64Add` | `add64f` | `wasm_f64_add` | **YES** |
| `OpSub64F` | `OpWasmF64Sub` | `sub64f` | `wasm_f64_sub` | **YES** |
| `OpMul64F` | `OpWasmF64Mul` | `mul64f` | `wasm_f64_mul` | **YES** |
| `OpDiv64F` | `OpWasmF64Div` | `div64f` | `wasm_f64_div` | **YES** |
| `OpNeg64F` | `OpWasmF64Neg` | `neg64f` | `wasm_f64_neg` | **YES** |
| `OpSqrt64F` | `OpWasmF64Sqrt` | `sqrt64f` | `wasm_f64_sqrt` | **YES** |

### Memory Operations

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpLoad` (64-bit) | `OpWasmI64Load` | `load, load64` | `wasm_i64_load` | **YES** |
| `OpLoad` (32-bit) | `OpWasmI64Load32U/S` | `load32` | `wasm_i32_load` | **DIFFERENT** |
| `OpStore` (64-bit) | `OpWasmI64Store` | `store, store64` | `wasm_i64_store` | **YES** |
| `OpStore` (32-bit) | `OpWasmI64Store32` | `store32` | `wasm_i64_store32` | **YES** |
| `OpZero` | `OpWasmLoweredZero` | `zero` | `wasm_lowered_zero` | **YES** |
| `OpMove` | `OpWasmLoweredMove` | `move` | `wasm_lowered_move` | **YES** |

### Conversions

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpSignExt32to64` | `OpWasmI64Extend32S` | `sign_ext32to64` | `wasm_i64_extend_i32_s` | **YES** |
| `OpZeroExt32to64` | `OpWasmI64Extend32U` | `zero_ext32to64` | `wasm_i64_extend_i32_u` | **YES** |
| `OpTrunc64to32` | `OpWasmI32WrapI64` | `trunc64to32` | `wasm_i32_wrap_i64` | **YES** |
| `OpCvt64to64F` | `OpWasmF64ConvertI64S` | `cvt64to64f` | `wasm_f64_convert_i64_s` | **YES** |
| `OpCvt64Fto64` | `OpWasmI64TruncSatF64S` | `cvt64fto64` | `wasm_i64_trunc_f64_s` | **PARTIAL** (no sat) |

### Function Calls

| Go Op | Go Target | Our Op | Our Target | Parity |
|-------|-----------|--------|------------|--------|
| `OpStaticCall` | `OpWasmLoweredStaticCall` | `static_call` | `wasm_lowered_static_call` | **YES** |
| `OpClosureCall` | `OpWasmLoweredClosureCall` | `closure_call` | `wasm_lowered_closure_call` | **YES** |
| `OpInterCall` | `OpWasmLoweredInterCall` | `inter_call` | `wasm_lowered_inter_call` | **YES** |

---

## Missing from Go's rewriteWasm.go

### Complex Rewrites Not Implemented

1. **Address lowering**: Go has `OpAddr` → `OpWasmLoweredAddr` with symbol resolution
2. **Atomic operations**: Go has full atomic support for Wasm threads
3. **Select instruction**: Go has `OpCondSelect` → `OpWasmSelect`
4. **Rotations**: Go has `OpRotateLeft64` → `OpWasmI64Rotl`
5. **Population count**: Go maps to Wasm intrinsics

### Ops We Pass Through (May Need Lowering)

| Op | Status | Note |
|----|--------|------|
| `phi` | Pass-through | Handled in wasm_gen |
| `copy` | Pass-through | Handled in wasm_gen |
| `arg` | Pass-through | Handled in wasm_gen |
| `local_addr` | Pass-through | Needs lowering for symbols |
| `addr` | Pass-through | Needs OpWasmLoweredAddr |

---

## Verification

```
$ zig build test
All 9 lower_wasm.zig tests pass

# Tests verify:
# - const_int -> wasm_i64_const
# - add -> wasm_i64_add
# - comparisons (lt, ult) -> wasm_i64_lt_s, wasm_i64_lt_u
# - float ops -> wasm_f64_*
# - memory ops -> wasm_i64_load/store
# - static_call -> wasm_lowered_static_call
# - phi/arg remain unchanged
# - already-wasm ops unchanged
```

**VERDICT: ~70% parity with Go's rewriteWasm.go. Core lowering rules match. Missing complex address lowering, atomics, and some specialized ops.**
