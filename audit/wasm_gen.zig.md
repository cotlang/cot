# Audit: wasm_gen.zig

## Status: WORKING - ~70% PARITY

| Metric | Value |
|--------|-------|
| Lines | ~850 |
| Go Reference | ~/learning/go/src/cmd/compile/internal/wasm/ssa.go (595 lines) |
| Tests | 3 unit tests |
| E2E Status | ✅ All core features working (return, arithmetic, calls, conditionals, recursion) |

---

## Recent Fixes (January 2026)

### 1. Memory Slot Alignment - FIXED
**Before**: `STACK_BASE + slot` (slots overlapped!)
**After**: `SP + (slot * 8)` (proper i64 alignment)

### 2. SP Stack Management - IMPLEMENTED
**Go-style prologue/epilogue**:
- Prologue: `global.get SP; i32.const frame_size; i32.sub; global.set SP`
- Epilogue: `global.get SP; i32.const frame_size; i32.add; global.set SP`
- Uses SP global (index 0) from Linker

### 3. Comparison Handling - FIXED
**Before**: Comparisons stored to i64 locals, causing type mismatch
**After**:
- Skip comparisons in `allocateLocals()` (no local allocated)
- Skip comparisons in `ssaGenValue()` (generated on-demand by branches)
- Comparisons produce i32, used directly by `if` instruction

### 4. Rematerializable Values - FIXED
**Before**: Generated in `ssaGenValue`, then regenerated in `getValue*`
**After**: Skip in `ssaGenValue`, only generate on-demand (like Go's OnWasmStack)

### 5. Forward Branch to Return Blocks - FIXED
**Before**: Emitted invalid `br 0` for forward jumps
**After**: Inline the return block's code (generate values + return)

### 6. Function Call Name Resolution - FIXED
**Before**: Checked `aux_call.fn_name` (always null)
**After**: Check `aux.string` (where SSA builder stores function name)

---

## Go Reference Functions vs Our Implementation

### Core Code Generation Functions

| Go Function | Go Lines | Our Function | Our Lines | Parity |
|-------------|----------|--------------|-----------|--------|
| `ssaGenValue` | 217-311 | `ssaGenValue` | 411-520 | **GOOD** |
| `ssaGenValueOnStack` | 313-461 | `ssaGenValueOnStack` | 522-700 | **GOOD** |
| `ssaGenBlock` | 169-215 | `ssaGenBlock` | 310-382 | **GOOD** |
| `getValue32` | 474-489 | `getValue32` | 710-730 | **GOOD** |
| `getValue64` | 491-503 | `getValue64` | 760-775 | **GOOD** |
| `setReg` | 530-533 | `setReg` | 780-785 | **YES** |
| `getReg` | 525-528 | N/A | N/A | **DIFFERENT** (use locals directly) |
| `isCmp` | 463-472 | `isCmp` | 225-238 | **YES** |
| Frame management | wasmobj.go | prologue/epilogue | 145-160 | **YES** |

---

## What Works

1. ✅ Simple functions returning constants: `return 42`
2. ✅ Arithmetic on parameters: `return a + b`
3. ✅ Function calls: `return add(10, 32)`
4. ✅ Nested function calls: `return double(double(x))`
5. ✅ Recursive functions: `factorial(5)`, `fib(10)`
6. ✅ Conditionals: `if a > b { return a } return b`
7. ✅ Loops (via recursion): Fibonacci works

---

## Still Missing (Future Work)

### High Priority
1. `OpWasmLoweredMove` - memory.copy for struct assignment
2. `OpWasmLoweredZero` - memory.fill for zeroing
3. `OpWasmLoweredNilCheck` - nil pointer checks

### Medium Priority
1. Closure calls (`OpWasmLoweredClosureCall`)
2. Interface calls (`OpWasmLoweredInterCall`)
3. Write barriers (`OpWasmLoweredWB`) for GC
4. `OpWasmSelect` - conditional select

### Low Priority
1. Tail calls (`OpWasmLoweredTailCall`)
2. `OpWasmLoweredGetClosurePtr`, `OpWasmLoweredGetCallerPC`, `OpWasmLoweredGetCallerSP`

---

## Verification

```bash
$ zig build test
# All wasm tests pass

$ ./zig-out/bin/cot --target=wasm32 test.cot && node -e "..."
main() = 42n    # return 42 ✅
add() = 42n     # add(10, 32) ✅
fact() = 120n   # factorial(5) ✅
fib() = 55n     # fib(10) ✅
max() = 10n     # max(10, 5) ✅
wrap() = 12n    # double(double(3)) ✅
```

**VERDICT: ~70% parity with Go's wasm/ssa.go. Core functionality working, advanced features (GC, closures, interfaces) not yet implemented.**
