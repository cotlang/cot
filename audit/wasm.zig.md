# Audit: wasm.zig

## Status: CUSTOM IMPLEMENTATION - NO DIRECT GO EQUIVALENT

| Metric | Value |
|--------|-------|
| Lines | 594 |
| Go Reference | No direct equivalent (Go uses obj package for module emission) |
| Tests | 6 unit tests |
| E2E Status | Produces valid Wasm binaries |

---

## Purpose

This file handles Wasm binary module construction - building the actual `.wasm` file with proper sections, type definitions, exports, etc.

**Go's approach is different**: Go uses its `cmd/internal/obj` linker infrastructure. The Wasm-specific parts are in `cmd/internal/obj/wasm/a.out.go` and the generic object writer. We emit Wasm directly without a linker step.

---

## Our Implementation vs Go Architecture

### Module Builder

| Our Component | Lines | Go Equivalent | Notes |
|---------------|-------|---------------|-------|
| `Module` struct | 17-145 | `obj/wasm/a.out.go` + linker | We build module directly |
| `Module.init` | 36-38 | N/A | Simple allocator setup |
| `Module.deinit` | 40-45 | N/A | Memory cleanup |
| `Module.addMemory` | 49-53 | linker handles | We configure memory section |
| `Module.addFuncType` | 56-61 | `obj.Link` type table | We manage types directly |
| `Module.addFunc` | 64-69 | `obj.LSym` registration | Function declaration |
| `Module.addExport` | 72-78 | `wasm.Export` in obj | Export entry |
| `Module.addCode` | 81-85 | `obj.Prog` encoding | Function body bytes |
| `Module.emit` | 88-144 | `wasm.headSize` + writer | Full module emission |

### CodeBuilder

| Our Component | Lines | Go Equivalent | Notes |
|---------------|-------|---------------|-------|
| `CodeBuilder` struct | 151-175 | `obj.Prog` chain | We use byte buffer |
| `emitI64Const` | 169-172 | `wasm.AI64Const` | Constant emission |
| `emitLocalGet` | 181-184 | `wasm.AGet` | Local access |
| `emitLocalSet` | 187-190 | `wasm.ASet` | Local write |
| `emitI64Add` | 193-195 | `wasm.AI64Add` | Binary op |
| `emitI64Store` | 426-430 | `wasm.AI64Store` | Memory store |
| `emitI64Load` | 418-422 | `wasm.AI64Load` | Memory load |
| `emitBlock/Loop/If` | 367-382 | `wasm.ABlock/ALoop/AIf` | Control flow |
| `emitBr/BrIf` | 389-399 | `wasm.ABr/ABrIf` | Branches |
| `setLocalCount` | 447-449 | implicit in Go | Local declaration count |
| `finish` | 453-474 | `wasm.headSize` | Build function body bytes |

---

## Section Emission Comparison

### Go's Approach (obj/wasm/a.out.go)

Go's linker builds sections from object symbols:
- Types gathered from function signatures
- Functions from `obj.LSym` entries
- Memory configured via linker flags
- Exports marked with `obj.STEXT` + export attributes

### Our Approach (wasm.zig)

We build sections incrementally:
1. `addFuncType` appends to types buffer, increments counter
2. `addFunc` references type index
3. `addExport` appends to exports buffer
4. `addCode` appends body bytes
5. `emit` writes header + all sections with proper LEB128 sizes

---

## Parity Analysis

### What Matches Go's Intent

1. **Section ordering**: Type, Function, Memory, Export, Code - matches Wasm spec
2. **LEB128 encoding**: Via `wasm_encode.zig`, same as Go's `writeUleb128`
3. **Type signatures**: `0x60` prefix, param count, params, result count, results
4. **Export format**: Name length + name + kind + index
5. **Code format**: Body size + locals + instructions + end

### What Differs from Go

1. **No linker**: We emit directly, Go goes through obj → linker → binary
2. **No relocation**: Go handles symbols/relocations, we use direct indices
3. **No data section**: Go emits `.rodata`, we don't have globals yet
4. **No import section**: Go imports runtime functions, we're self-contained
5. **No custom sections**: Go embeds debug info, we don't

### Missing for Parity

| Feature | Go Has | We Have | Priority |
|---------|--------|---------|----------|
| Data section | Yes | No | Medium |
| Import section | Yes | No | High (for runtime) |
| Global section | Yes | No | Medium |
| Table section | Yes | No | Low (for indirect calls) |
| Element section | Yes | No | Low |
| Custom/name section | Yes | No | Low |

---

## CodeBuilder Instructions Implemented

| Category | Instructions | Parity |
|----------|--------------|--------|
| Constants | i32.const, i64.const, f64.const | **YES** |
| Locals | local.get, local.set | **YES** |
| i64 Arithmetic | add, sub, mul, div_s, rem_s | **YES** |
| i64 Bitwise | and, or, xor, shl, shr_s | **YES** |
| i64 Compare | eq, ne, lt_s, le_s, gt_s, ge_s | **YES** |
| f64 Arithmetic | add, sub, mul, div, neg | **YES** |
| f64 Compare | eq, ne, lt, le, gt, ge | **YES** |
| Memory | i64.load, i64.store, i32.load, i32.store | **YES** |
| Control | block, loop, if, else, end, br, br_if | **YES** |
| Other | call, drop, return, nop, unreachable | **YES** |
| Conversion | i32.wrap_i64, i32.eqz, i64.eqz | **YES** |

### Missing Instructions

| Instruction | Go Has | Priority |
|-------------|--------|----------|
| f32 operations | Yes | Low |
| i32 full set | Yes | Medium |
| memory.copy | Yes | High (for LoweredMove) |
| memory.fill | Yes | High (for LoweredZero) |
| select | Yes | Medium |
| i64.extend_i32_u/s | Yes | High |

---

## Verification

```
$ zig build test
All 6 wasm.zig tests pass

$ ./zig-out/bin/cot --target=wasm32 simple.cot
Produces valid Wasm module (verified with wasm-validate)

$ wasm-objdump -h output.wasm
Type section present
Function section present
Memory section present
Export section present
Code section present
```

**VERDICT: Good implementation for basic module emission. Matches Wasm spec directly rather than copying Go's linker infrastructure (which would be inappropriate for our architecture).**
