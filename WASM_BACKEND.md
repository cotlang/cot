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

### Completed (M13-M15)

| Component | Status | Notes |
|-----------|--------|-------|
| string_len | ✅ Done | M14 - rewritegeneric + rewritedec |
| string_ptr | ✅ Done | M14 - decomposed to ptr_const |
| string_make | ✅ Done | M14 - Go-style decomposition |
| retain/release | ✅ Done | M15 - ARC runtime in wasm/arc.zig |

### Completed (M16)

| Component | Status | Notes |
|-----------|--------|-------|
| Import section | ✅ Done | M16 - WasmImport struct, addImport(), writeImportSec() |
| Import-aware exports | ✅ Done | M16 - Function indices offset by import count |

### TODO (Future)

| Component | Status | Notes |
|-----------|--------|-------|
| Frontend import syntax | ⏳ TODO | Parse `import func ... from "module"` |
| console.log builtin | ⏳ TODO | Map to env.console_log import |

## Test Results

**65+ Wasm tests passing:**

```
Arithmetic:    10/10 ✓
Control Flow:  14/14 ✓
Functions:     16/16 ✓
Memory:         5/5  ✓
Structs:        5/5  ✓
Arrays:         5/5  ✓
Strings:        3/3  ✓
ARC:            5/5  ✓
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

All Wasm backend milestones (M1-M16) are complete. Future work includes:

1. **M17: Frontend ARC** - Emit retain/release calls automatically in frontend
2. **M18: Heap allocation** - `new` keyword, `cot_alloc` integration
3. **Native AOT** - Complete Phase 7 integration (see `CRANELIFT_PORT_MASTER_PLAN.md`)
