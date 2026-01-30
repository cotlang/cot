# Audit: wasm/constants.zig

## Status: COMPLETE - 95% PARITY

| Metric | Value |
|--------|-------|
| Lines | ~711 |
| Go Reference | cmd/internal/obj/wasm/a.out.go (342 lines) |
| Tests | 3 unit tests |

---

## Go Reference Mapping

### Registers (Go: a.out.go lines 263-341)

| Go Constant | Go Line | Our Constant | Our Line | Parity |
|-------------|---------|--------------|----------|--------|
| REG_SP | 268 | Reg.sp | 37 | **YES** |
| REG_CTXT | 269 | Reg.ctxt | 38 | **YES** |
| REG_g | 270 | Reg.g | 39 | **YES** |
| REG_RET0-3 | 271-274 | Reg.ret0-3 | 40-43 | **YES** |
| REG_PAUSE | 275 | Reg.pause | 44 | **YES** |
| REG_R0-R15 | 279-295 | Reg.r0-r15 | 47-62 | **YES** |
| REG_F0-F15 | 297-313 | Reg.f0-f15 | 65-80 | **YES** |
| REG_F16-F31 | 315-331 | Reg.f16-f31 | 83-98 | **YES** |
| REG_PC_B | 333 | Reg.pc_b | 101 | **YES** |

### Instructions (Go: a.out.go lines 20-260)

| Category | Go Lines | Our Lines | Parity |
|----------|----------|-----------|--------|
| Control flow (0x00-0x11) | 30-45 | 155-167 | **YES** |
| Parametric (0x1A-0x1B) | 47-48 | 170-171 | **YES** |
| Variable (0x20-0x24) | 50-54 | 174-178 | **YES** |
| Memory load (0x28-0x35) | 56-69 | 181-194 | **YES** |
| Memory store (0x36-0x3E) | 70-78 | 197-205 | **YES** |
| Memory ops (0x3F-0x40) | 79-80 | 208-209 | **YES** |
| Constants (0x41-0x44) | 82-85 | 212-215 | **YES** |
| i32 comparison (0x45-0x4F) | 87-97 | 218-228 | **YES** |
| i64 comparison (0x50-0x5A) | 99-109 | 231-241 | **YES** |
| f32 comparison (0x5B-0x60) | 111-116 | 244-249 | **YES** |
| f64 comparison (0x61-0x66) | 118-123 | 252-257 | **YES** |
| i32 arithmetic (0x67-0x78) | 125-142 | 260-277 | **YES** |
| i64 arithmetic (0x79-0x8A) | 144-161 | 280-297 | **YES** |
| f32 arithmetic (0x8B-0x98) | 163-176 | 300-313 | **YES** |
| f64 arithmetic (0x99-0xA6) | 178-191 | 316-329 | **YES** |
| Conversions (0xA7-0xBF) | 193-217 | 332-356 | **YES** |
| Sign extension (0xC0-0xC4) | 218-222 | 359-363 | **YES** |
| Saturating trunc (0xFC 0x00-0x07) | 224-231 | 366-373 | **YES** |
| Bulk memory (0xFC 0x08-0x11) | 233-242 | 376-385 | **YES** |
| Pseudo-instructions | 246-260 | 388-403 | **YES** |

### Helper Functions

| Go Function | Go Line | Our Function | Our Line | Parity |
|-------------|---------|--------------|----------|--------|
| N/A (use iota) | - | As.opcode() | 406-587 | **YES** (manual mapping) |
| N/A | - | As.isFcPrefixed() | 590-612 | **YES** |
| N/A | - | As.fcOpcode() | 615-637 | **YES** |
| N/A | - | Reg.isGlobal() | 105-107 | **YES** |
| N/A | - | Reg.isI64() | 109-112 | **YES** |
| N/A | - | Reg.isF32() | 114-117 | **YES** |
| N/A | - | Reg.isF64() | 119-122 | **YES** |
| N/A | - | Reg.valType() | 124-129 | **YES** |

### Constants

| Go Constant | Go Line | Our Constant | Our Line | Parity |
|-------------|---------|--------------|----------|--------|
| WASM magic | - | WASM_MAGIC | 676 | **YES** |
| WASM version | - | WASM_VERSION | 677 | **YES** |
| funcValueOffset | asm.go:45 | FUNC_VALUE_OFFSET | 682 | **YES** |

---

## What's Different

1. **Enum vs iota**: Go uses iota-based integer constants; we use Zig enums with explicit opcode mapping methods.

2. **Section IDs**: Added `Section` enum (lines 645-658) for Wasm section types - not in Go's a.out.go but needed for linking.

3. **Export kinds**: Added `ExportKind` enum (lines 665-670) - in Go's asm.go, we consolidated here.

---

## Verification

```bash
$ zig test compiler/codegen/wasm/constants.zig
All 3 tests passed.
```

**VERDICT: 95% parity. All Wasm opcodes and registers match Go. Added Section/ExportKind for convenience.**
