# Wasm Backend Implementation

## Architecture

Following Go's two-pass Wasm architecture:

```
Cot Source → Frontend → IR → SSA → lower_wasm.zig → wasm/ package → .wasm
                                         ↓
                        compiler/codegen/wasm/
                        ├── gen.zig        (SSA → Prog chain, like Go's ssa.go)
                        ├── preprocess.zig (Transform pseudo-ops, like Go's wasmobj.go)
                        ├── assemble.zig   (Prog → bytes, like Go's wasmobj.go)
                        ├── link.zig       (Module assembly, like Go's asm.go)
                        ├── prog.zig       (Instruction chain, like Go's obj.Prog)
                        └── constants.zig  (Opcodes/registers, like Go's a.out.go)
```

## Go Reference Files

| Our File | Go Reference | Lines |
|----------|--------------|-------|
| `gen.zig` | `cmd/compile/internal/wasm/ssa.go` | 595 |
| `preprocess.zig` | `cmd/internal/obj/wasm/wasmobj.go` preprocess() | ~400 |
| `assemble.zig` | `cmd/internal/obj/wasm/wasmobj.go` assemble() | ~300 |
| `link.zig` | `cmd/link/internal/wasm/asm.go` | 707 |
| `constants.zig` | `cmd/internal/obj/wasm/a.out.go` | 342 |

## Implementation Status

### Completed (M1-M12)

| Component | Status | Notes |
|-----------|--------|-------|
| Wasm SSA ops | ✅ Done | `compiler/ssa/op.zig` |
| Wasm lowering | ✅ Done | `compiler/ssa/passes/lower_wasm.zig` |
| gen.zig | ✅ Done | ssaGenValue, ssaGenBlock, getValue64 |
| preprocess.zig | ✅ Done | AJMP/ARET transforms, dispatch loop |
| assemble.zig | ✅ Done | Prog→bytes, local declarations |
| link.zig | ✅ Done | Basic module assembly |
| Arithmetic | ✅ Done | i64 add/sub/mul/div/rem |
| Comparisons | ✅ Done | eq/ne/lt/gt/le/ge |
| Control flow | ✅ Done | if/else, while, break, continue |
| Functions | ✅ Done | Calls, params, recursion |
| Memory | ✅ Done | load/store, SP global, frames |
| Structs | ✅ Done | off_ptr for field access |

### In Progress (M13)

| Component | Status | Notes |
|-----------|--------|-------|
| slice_len | ⏳ SSA only | Needs gen.zig implementation |
| slice_ptr | ⏳ SSA only | Needs gen.zig implementation |
| slice_make | ⏳ SSA only | Needs gen.zig implementation |
| bounds_check | ⏳ SSA only | Needs gen.zig implementation |

### TODO (M14-M16)

| Component | Status | Notes |
|-----------|--------|-------|
| string_len | ⏳ SSA only | M14 |
| string_ptr | ⏳ SSA only | M14 |
| string_make | ⏳ SSA only | M14 |
| string_concat | ⏳ SSA only | M14 |
| retain/release | ⏳ SSA only | M15 - needs runtime |
| Import section | ❌ TODO | M16 - JS interop |

## Test Results

**50/50 Wasm tests passing:**

```
Arithmetic:    10/10 ✓
Control Flow:  14/14 ✓
Functions:     16/16 ✓
Memory:         5/5  ✓
Structs:        5/5  ✓
```

## Key Implementation Details

### Dispatch Loop (Go's approach)

Every function uses a dispatch loop for control flow:

```wasm
(loop $dispatch
  (block $b0 (block $b1 (block $b2
    (br_table $b0 $b1 $b2 (local.get $PC_B))
  ) ;; $b2: block 0 code
  ) ;; $b1: block 1 code
  ) ;; $b0: block 2 code
)
```

- PC_B (i32 local) tracks which block to execute next
- AJMP becomes: `i32.const <target>, local.set $PC_B, br $dispatch`
- ARET becomes: stack frame restore + real return

### Local Index Layout

```
For fn foo(a, b):
  local 0 = param a (i64, assigned by Wasm)
  local 1 = param b (i64, assigned by Wasm)
  local 2 = PC_B (i32, first declared local)
  local 3+ = value locals (i64)
```

### Frame Allocation

```
Prologue: SP = SP - framesize
Epilogue: SP = SP + framesize (before return)
```

## Running Tests

```bash
# Compile to wasm
./zig-out/bin/cot --target=wasm32 test.cot -o test.wasm

# Run with Node.js
node -e 'const fs=require("fs"); const wasm=fs.readFileSync("test.wasm");
WebAssembly.instantiate(wasm).then(r=>console.log(r.instance.exports.main()));'
```

## Next Steps

1. **M13: Arrays/Slices** - Implement slice_len, slice_ptr, bounds_check in gen.zig
2. **M14: Strings** - Implement string ops, data section in link.zig
3. **M15: ARC** - Implement retain/release calls, runtime integration
4. **M16: Browser** - Import section for JS interop
