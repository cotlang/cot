# Slice Architecture Fix Plan

## Status: ✅ COMPLETE (February 2026)

All tasks are done. Slice architecture matches Go's proven design.

## Problem Summary

Cot's slice handling differs architecturally from Go's proven design, causing Wasm validation errors ("not enough arguments on the stack for local.set").

## Root Cause

Go fully decomposes slice operations before Wasm codegen. Cot leaves `slice_make` values in SSA, which then get assigned Wasm locals but generate no code.

## Go's Architecture (Reference: decompose.go, rewritedec.go)

```
Source: var s []int = arr[:]
         ↓
SSA: SliceMake(ptr, len, cap)
         ↓ decompose.go
Split into 3 variables: s.ptr, s.len, s.cap
         ↓
Each component assigned separate register/local
         ↓
Wasm: 3 separate local.get/local.set operations
```

## Cot's Current Architecture (Broken)

```
Source: var s []i64 = arr[:]
         ↓
IR: string_header(ptr, len)
         ↓ ssa_builder.zig
SSA: slice_make(ptr, len) + store(addr, ptr) + store(addr+8, len)
         ↓ allocateLocals
slice_make gets Wasm local assigned (BUG!)
         ↓ gen.zig
slice_make → NO CODE, but local.set emitted → STACK ERROR
```

## Target Architecture (Matching Go)

```
Source: var s []i64 = arr[:]
         ↓
IR: string_header(ptr, len)
         ↓ ssa_builder.zig
SSA: slice_make(ptr, len) + store(addr, ptr) + store(addr+8, len)
         ↓ NEW: decompose pass
slice_make removed, only ptr/len values remain
         ↓ allocateLocals
Only ptr/len values get Wasm locals
         ↓ gen.zig
No slice_make to process, clean stack operations
```

---

## Execution Plan

### Phase 1: Immediate Fix (Stop the Bleeding) ✅ COMPLETE

**Task 1.1: Skip compound types in allocateLocals** ✅
- File: `compiler/codegen/wasm/gen.zig`
- Done: `isCompoundType()` function skips `.slice_make`, `.string_make`
- Go ref: Go's regalloc never sees SliceMake

**Task 1.2: Handle compound types in ssaGenValue** ✅
- File: `compiler/codegen/wasm/gen.zig`
- Done: `ssaGenValue()` calls `isCompoundType()` and skips

### Phase 2: Decompose Slice Values (Core Fix) ✅ COMPLETE

**Task 2.1: Create decompose.zig pass** ✅
- File: `compiler/ssa/passes/decompose.zig`
- Go ref: `cmd/compile/internal/ssa/decompose.go`
- Done: `decompose()`, `decomposeBuiltinPhi()`, `decomposeSlicePhi()`, `decomposeStringPhi()`

**Task 2.2: Wire decompose pass into pipeline** ✅
- File: `compiler/driver.zig` line 489
- Done: `try decompose_builtin.decompose(self.allocator, ssa_func);`

### Phase 3: Strengthen rewritedec.zig ✅ COMPLETE

**Task 3.1: Handle all slice_ptr patterns** ✅
- File: `compiler/ssa/passes/rewritedec.zig`
- Done: 4 patterns matching Go's rewritedec.go lines 539-572

**Task 3.2: Handle all slice_len patterns** ✅
- File: `compiler/ssa/passes/rewritedec.zig`
- Done: 4 patterns matching Go's rewritedec.go lines 499-537

**Task 3.3: Add iteration until fixpoint** ✅
- Done: `while (iterations < max_iterations)` loop in `rewrite()`

### Phase 4: Slice Phi Decomposition ✅ COMPLETE

**Task 4.1: Implement decomposeSlicePhi** ✅
- File: `compiler/ssa/passes/decompose.zig`
- Done: Matches Go's decompose.go lines 159-176

### Phase 5: Verification & Cleanup ✅ COMPLETE

**Task 5.1: Add assertion in gen.zig** ✅
- Done: Added error logging if slice_make/string_make has uses > 0
- File: `compiler/codegen/wasm/gen.zig` lines 281-291

**Task 5.2: Add tests for slice operations** ✅ COMPLETE
- Test: slice from array `arr[:]` ✅
- Test: slice assignment ✅
- Test: slice in control flow (if/else with slices) ✅
- Test: append to slice ✅

**Task 5.3: Update CLAUDE.md with slice architecture** ✅
- Done: Added "Slice/Compound Type Architecture" section
- Documents pipeline and Go references

---

## File Changes Summary

| File | Changes |
|------|---------|
| `compiler/codegen/wasm/gen.zig` | Skip compound types in allocateLocals, defensive check in ssaGenValue |
| `compiler/ssa/passes/decompose.zig` | NEW FILE - slice decomposition pass |
| `compiler/ssa/passes/rewritedec.zig` | Strengthen extraction decomposition |
| `compiler/codegen/wasm/compile.zig` | Wire decompose pass into pipeline |
| `test/` | Add slice operation tests |

---

## Go Reference Files

- `~/learning/go/src/cmd/compile/internal/ssa/decompose.go` - Main decomposition logic
- `~/learning/go/src/cmd/compile/internal/ssa/rewritedec.go` - Extraction decomposition
- `~/learning/go/src/cmd/compile/internal/ssa/func.go` - SplitSlice function
- `~/learning/go/src/cmd/compile/internal/wasm/ssa.go` - Wasm codegen (verify SliceMake never appears)

---

## Success Criteria

1. `zig build test` passes
2. All slice test cases pass:
   - `var arr2 = append(arr, 4); return arr2[0]` returns correct value
   - `var s = arr[:]; return s[0]` returns correct value
   - Slice passed to function works
3. No `slice_make` or `string_make` values have Wasm locals assigned
4. No compound type ops reach gen.zig's ssaGenValueOnStack
