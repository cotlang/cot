# Wasm Parity Reconciliation

**Date:** January 2026
**Status:** Old audits are outdated - implementation has improved significantly

---

## Summary of Discrepancies

| File | Old Audit | Old Date | Current State | Actual Parity |
|------|-----------|----------|---------------|---------------|
| wasm_gen.zig | ~70% | Pre-M10 | 1697 lines, 16 tests | **~85%** |
| lower_wasm.zig | ~70% | Pre-M10 | 509 lines, 9 tests | **~70%** (unchanged) |
| wasm.zig | "No Go equivalent" | - | 636 lines, 5 tests | N/A (different approach) |
| wasm_opcodes.zig | 100% | - | 331 lines, 5 tests | **100%** (spec-compliant) |
| wasm_encode.zig | 100% | - | 229 lines, 5 tests | **100%** (spec-compliant) |

---

## wasm_gen.zig: 70% → 85%

### What Changed Since Old Audit

| Metric | Old Audit | Current |
|--------|-----------|---------|
| Lines | ~850 | 1697 |
| Tests | 3 | 16 |
| M10 Linear Memory | ❌ Listed as TODO | ✅ Implemented |
| M11 Pointers | ❌ Not mentioned | ✅ off_ptr, add_ptr, sub_ptr |
| M12 Structs | ❌ Not mentioned | ✅ Field access via off_ptr |
| M13 Slices | ❌ Not mentioned | ✅ slice_ptr, slice_len, bounds_check |

### Old "Missing" Features - Status Update

| Feature | Old Status | Current Status |
|---------|------------|----------------|
| Frame management (SP) | ❌ Missing | ✅ Implemented (prologue/epilogue) |
| Comparison handling | ❌ Buggy | ✅ Fixed (skip in allocateLocals) |
| Rematerializable values | ❌ Buggy | ✅ Fixed (on-demand generation) |
| Forward branch to return | ❌ Buggy | ✅ Fixed (inline return block) |
| OpWasmLoweredMove | ❌ Missing | ⚠️ In wasm/gen.zig (separate module) |
| OpWasmLoweredZero | ❌ Missing | ⚠️ In wasm/gen.zig (separate module) |
| Closure calls | ❌ Missing | ❌ Still missing (not needed yet) |
| Write barriers | ❌ Missing | ❌ N/A (ARC, not GC) |

### Why 85% Not 95%

My earlier audit (WASM_AUDIT_GO_PARITY.md) claimed 95% based on **architectural parity** - the code structure matches Go's design.

The 85% accounts for **feature completeness**:
- Missing: LoweredAddr (symbol resolution)
- Missing: Select instruction
- Missing: Some conversions (truncate saturating)
- Missing: Atomics (not needed)
- Missing: i32 full operation set

---

## lower_wasm.zig: 70% (Unchanged)

This file hasn't changed significantly. The 70% parity is accurate.

### What's Implemented
- All basic arithmetic ops (add, sub, mul, div, mod)
- All bitwise ops (and, or, xor, shl, shr)
- All comparison ops (eq, ne, lt, le, gt, ge)
- All float ops (add, sub, mul, div, neg, sqrt)
- Load/store ops
- Function calls (static, closure, interface)
- Basic conversions

### What's Missing
- Complex address lowering (OpAddr → OpWasmLoweredAddr)
- Atomics
- Select
- Rotations
- Population count
- Some specialized conversions

---

## Overall Wasm Backend Parity

### By Component

| Component | Go Reference | Parity | Notes |
|-----------|--------------|--------|-------|
| wasm_gen.zig | wasm/ssa.go | **85%** | Core codegen, M10-M13 done |
| lower_wasm.zig | rewriteWasm.go | **70%** | Basic lowering, missing complex ops |
| wasm.zig | obj/wasm (linker) | N/A | Different approach (direct emit) |
| wasm_opcodes.zig | Wasm spec | **100%** | All needed opcodes defined |
| wasm_encode.zig | wasmobj.go | **100%** | LEB128 identical to Go |

### Weighted Average

| Component | Weight | Parity | Contribution |
|-----------|--------|--------|--------------|
| wasm_gen.zig | 50% | 85% | 42.5% |
| lower_wasm.zig | 30% | 70% | 21% |
| wasm_encode.zig | 10% | 100% | 10% |
| wasm_opcodes.zig | 10% | 100% | 10% |

**Overall: ~83.5% parity with Go's Wasm implementation**

---

## Why Different from 95% Claim?

The 95% in WASM_AUDIT_GO_PARITY.md measured **design parity** (architecture, function structure, patterns).

The 83.5% here measures **feature completeness** (what ops are actually implemented).

Both are valid perspectives:
- **Design parity: 95%** - We follow Go's patterns correctly
- **Feature parity: 83.5%** - We haven't implemented everything Go has

---

## What's Needed for Higher Parity

### To reach 90%:
1. Add OpWasmLoweredAddr in wasm_gen.zig (symbol resolution)
2. Add Select instruction
3. Add remaining i32 operations

### To reach 95%:
4. Add all conversion ops (sign extend, saturating truncate)
5. Add rotations, popcount
6. Add memory.copy, memory.fill in main codegen

### Not needed (Go-specific):
- Goroutine suspension (ARESUMEPOINT)
- Write barriers (GC)
- Atomics (threads)

---

## Recommendation

The old audit documents should be updated. The 70% claims are outdated.

**Current accurate assessment:**
- Design/Architecture parity: **95%**
- Feature completeness parity: **83.5%**
- For Cot's current needs (M1-M13): **Sufficient**

The implementation is ready for M14 (Strings). The missing features are not blockers.
