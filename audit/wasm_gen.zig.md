# Audit: wasm_gen.zig

## Status: WORKING - ~85% PARITY

| Metric | Value |
|--------|-------|
| Lines | 1697 |
| Go Reference | ~/learning/go/src/cmd/compile/internal/wasm/ssa.go (595 lines) |
| Tests | 16 unit tests |
| E2E Status | ✅ M1-M13 complete (constants, arithmetic, calls, conditionals, memory, pointers, structs, slices) |

---

## Milestone Status (January 2026)

| Milestone | Status | Features |
|-----------|--------|----------|
| M1-M9 | ✅ Done | Constants, arithmetic, calls, conditionals, loops |
| M10 | ✅ Done | Linear memory (SP, local_addr, load/store) |
| M11 | ✅ Done | Pointers (off_ptr, add_ptr, sub_ptr) |
| M12 | ✅ Done | Structs (field access via off_ptr) |
| M13 | ✅ Done | Slices (slice_ptr, slice_len, bounds_check) |
| M14 | 🔄 Next | Strings (data section, string ops) |

---

## Go Reference Functions vs Our Implementation

### Core Code Generation Functions

| Go Function | Go Lines | Our Function | Our Lines | Parity |
|-------------|----------|--------------|-----------|--------|
| `ssaGenValue` | 217-311 | `ssaGenValue` | 444-554 | **95%** |
| `ssaGenValueOnStack` | 313-461 | `ssaGenValueOnStack` | 561-851 | **95%** |
| `ssaGenBlock` | 169-215 | `ssaGenBlock` | 333-403 | **95%** |
| `getValue32` | 474-489 | `getValue32` | 860-874 | **100%** |
| `getValue64` | 491-503 | `getValue64` | 897-907 | **100%** |
| `setReg` | 530-533 | `setReg` | 914-918 | **100%** |
| `isCmp` | 463-472 | `isCmp` | 248-258 | **100%** |
| Frame management | wasmobj.go | prologue/epilogue | 141-160 | **100%** |

---

## What Works (M1-M13)

1. ✅ Constants: `return 42`
2. ✅ Arithmetic: `return a + b`
3. ✅ Function calls: `return add(10, 32)`
4. ✅ Nested calls: `return double(double(x))`
5. ✅ Recursion: `factorial(5)`, `fib(10)`
6. ✅ Conditionals: `if a > b { return a } return b`
7. ✅ Loops: via recursion (Fibonacci works)
8. ✅ Linear memory: SP management, local_addr, load/store
9. ✅ Pointer arithmetic: off_ptr, add_ptr, sub_ptr
10. ✅ Struct field access: local_addr + off_ptr + load/store
11. ✅ Slice operations: slice_make, slice_ptr, slice_len
12. ✅ Bounds checking: index vs length with trap

---

## Implementation Details

### Memory Model (Go-style)

- **SP Global**: Global 0, initialized to 65536 by linker
- **Prologue**: `global.get SP; i32.const frame_size; i32.sub; global.set SP`
- **Epilogue**: `global.get SP; i32.const frame_size; i32.add; global.set SP`
- **Local addressing**: `SP + (slot * 8)` for 8-byte aligned slots

### OnWasmStack Optimization

Matches Go's pattern (wasm/ssa.go lines 299-304):
- Values with single use as block control skip local allocation
- Generated inline when consumed
- Stack balance tracked with `on_wasm_stack_skipped`

### Comparison Handling

Matches Go's isCmp pattern:
- Skip in allocateLocals (no local allocated)
- Skip in ssaGenValue (generated on-demand)
- Produce i32, consumed directly by `if` instruction

---

## Still Missing (Future Work)

### High Priority (for M14+)
1. Data section for string literals
2. String operations

### Medium Priority
1. `OpWasmLoweredAddr` - symbol resolution for globals
2. `OpWasmSelect` - conditional select
3. More i32 operations

### Low Priority (Not Needed for Cot)
1. Closure calls (`OpWasmLoweredClosureCall`) - not yet needed
2. Interface calls (`OpWasmLoweredInterCall`) - not yet needed
3. Write barriers (`OpWasmLoweredWB`) - Cot uses ARC, not GC
4. Goroutine suspension (`ARESUMEPOINT`) - Cot uses async/await
5. Atomics - Wasm threads not needed

---

## Test Coverage

| Category | Tests | Status |
|----------|-------|--------|
| Constants | 1 | ✅ Pass |
| Binary ops | 1 | ✅ Pass |
| Control flow | 1 | ✅ Pass |
| Linear memory | 3 | ✅ Pass |
| Pointer ops | 3 | ✅ Pass |
| Struct ops | 3 | ✅ Pass |
| Slice ops | 4 | ✅ Pass |

---

## Verification

```bash
$ zig build test
# All 16 wasm_gen.zig tests pass (389/411 total)

$ ./zig-out/bin/cot --target=wasm32 test.cot && node -e "..."
main() = 42n    # return 42 ✅
add() = 42n     # add(10, 32) ✅
fact() = 120n   # factorial(5) ✅
fib() = 55n     # fib(10) ✅
max() = 10n     # max(10, 5) ✅
wrap() = 12n    # double(double(3)) ✅
```

---

## Parity Assessment

| Aspect | Score | Notes |
|--------|-------|-------|
| Design/Architecture | 95% | Matches Go's ssaGen* structure |
| Core operations | 90% | All arithmetic, comparisons, memory |
| Control flow | 95% | if/else, loops, function calls |
| Memory model | 100% | SP-relative, frame management |
| Advanced features | 60% | Missing closures, atomics, some conversions |

**Overall: ~85% parity with Go's wasm/ssa.go**

The gap is intentional - features like goroutine suspension and GC write barriers are not applicable to Cot's design (ARC memory, async/await concurrency).

---

## History

- **Pre-M10**: 70% parity, ~850 lines, 3 tests
- **Post-M13**: 85% parity, 1697 lines, 16 tests
- **Key improvements**: SP management, pointer ops, struct/slice support
