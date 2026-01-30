# Audit: wasm Package

## Overview

The `compiler/codegen/wasm/` package implements WebAssembly code generation following Go's architecture.

## Go Reference Files

| Go File | Lines | Description |
|---------|-------|-------------|
| cmd/internal/obj/wasm/a.out.go | 342 | Instructions and registers |
| cmd/internal/obj/link.go | - | Prog, Addr, LSym structures |
| cmd/internal/obj/wasm/wasmobj.go | 1453 | preprocess() and assemble() |
| cmd/link/internal/wasm/asm.go | 707 | Module linking |
| cmd/compile/internal/wasm/ssa.go | 595 | SSA code generation |

## Our Files

| File | Lines | Go Reference | Parity |
|------|-------|--------------|--------|
| constants.zig | 711 | a.out.go | **95%** |
| prog.zig | 306 | link.go | **90%** |
| preprocess.zig | 443 | wasmobj.go preprocess | **60%** |
| assemble.zig | 478 | wasmobj.go assemble | **80%** |
| link.zig | 381 | asm.go | **75%** |
| gen.zig | 585 | ssa.go | **70%** |
| wasm.zig | 51 | (index) | N/A |

**Total: ~2,955 lines** (Go reference: ~3,097 lines)

## Architecture

```
Go Pipeline:
  SSA → ssa.go → Prog chain → wasmobj.go preprocess → wasmobj.go assemble → asm.go link

Our Pipeline:
  SSA → gen.zig → Prog chain → preprocess.zig → assemble.zig → link.zig
```

## Status Summary

| Component | Status | What Works |
|-----------|--------|------------|
| constants.zig | **COMPLETE** | All Wasm opcodes and registers |
| prog.zig | **COMPLETE** | Prog chain structure |
| preprocess.zig | **PARTIAL** | Frame alloc, epilogue, basic transforms |
| assemble.zig | **WORKING** | Instruction encoding, LEB128 |
| link.zig | **WORKING** | Type, function, memory, global, export, code sections |
| gen.zig | **WORKING** | Value/block generation, getValue, setReg |

## Test Results

```bash
$ zig test compiler/codegen/wasm/constants.zig  # 3 tests pass
$ zig test compiler/codegen/wasm/prog.zig       # 3 tests pass
$ zig test compiler/codegen/wasm/preprocess.zig # 2 tests pass
$ zig test compiler/codegen/wasm/assemble.zig   # 3 tests pass
$ zig test compiler/codegen/wasm/link.zig       # 2 tests pass
$ zig test compiler/codegen/wasm/gen.zig        # 3 tests pass
```

## What's Not Implemented (Future Work)

### High Priority
- Import section (host functions)
- Data section (static data)
- More global variables (CTXT, g, RET0-3, PAUSE)

### Medium Priority
- Closure calls
- Interface calls
- Table section (indirect calls)
- Element section

### Low Priority
- Name section (debug info)
- Write barriers (GC)
- Goroutine support (resume points, PC_B)

## Key Design Decisions

1. **Prog-based architecture**: Following Go's assembler design with linked list of Progs.

2. **Separate preprocess/assemble**: Two-pass design enables high-level transforms before encoding.

3. **SP-based stack management**: Using Wasm global 0 for stack pointer, same as Go.

4. **OnWasmStack optimization**: Tracking values that can stay on Wasm stack.

5. **Type deduplication**: Linker deduplicates function types automatically.
