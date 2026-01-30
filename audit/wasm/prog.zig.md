# Audit: wasm/prog.zig

## Status: COMPLETE - 90% PARITY

| Metric | Value |
|--------|-------|
| Lines | ~306 |
| Go Reference | cmd/internal/obj/link.go (Prog, Addr, LSym structs) |
| Tests | 3 unit tests |

---

## Go Reference Mapping

### Addr Structure (Go: obj.Addr)

| Go Field | Go Type | Our Field | Our Type | Parity |
|----------|---------|-----------|----------|--------|
| Type | AddrType | type | AddrType | **YES** |
| Name | AddrName | name | AddrName | **YES** |
| Reg | int16 | reg | Reg | **YES** |
| Offset | int64 | offset | i64 | **YES** |
| Sym | *LSym | sym | ?*Symbol | **YES** |
| Val | interface{} | val | Value | **SIMPLIFIED** |

### AddrType Enum (Go: obj/link.go)

| Go Constant | Our Constant | Parity |
|-------------|--------------|--------|
| TYPE_NONE | .none | **YES** |
| TYPE_REG | .reg | **YES** |
| TYPE_CONST | .const_int | **YES** |
| TYPE_FCONST | .const_float | **YES** |
| TYPE_MEM | .mem | **YES** |
| TYPE_ADDR | .addr | **YES** |
| TYPE_BRANCH | .branch | **YES** |

### AddrName Enum (Go: obj/link.go)

| Go Constant | Our Constant | Parity |
|-------------|--------------|--------|
| NAME_NONE | .none | **YES** |
| NAME_EXTERN | .@"extern" | **YES** |
| NAME_STATIC | .static | **YES** |
| NAME_AUTO | .auto | **YES** |
| NAME_PARAM | .param | **YES** |

### Prog Structure (Go: obj.Prog)

| Go Field | Go Type | Our Field | Our Type | Parity |
|----------|---------|-----------|----------|--------|
| As | obj.As | as | As | **YES** |
| From | Addr | from | Addr | **YES** |
| To | Addr | to | Addr | **YES** |
| Link | *Prog | link | ?*Prog | **YES** |
| Pc | int64 | pc | i64 | **YES** |
| Spadj | int32 | spadj | i32 | **YES** |
| Mark | uint32 | mark | u32 | **YES** |

### Symbol Structure (Go: obj.LSym)

| Go Field | Go Type | Our Field | Our Type | Parity |
|----------|---------|-----------|----------|--------|
| Name | string | name | []const u8 | **YES** |
| Size | int64 | size | i64 | **YES** |
| - | - | index | u32 | **ADDED** |
| Func | *FuncInfo | is_func, frame_size, etc | fields | **SIMPLIFIED** |
| P | []byte | N/A | - | **NOT NEEDED** |
| R | []Reloc | N/A | - | **FUTURE** |
| Func.Text | *Prog | text | ?*Prog | **YES** |

### Helper Functions

| Go Function | Go Location | Our Function | Our Line | Parity |
|-------------|-------------|--------------|----------|--------|
| isUnaryDst | wasmobj.go:99-117 | isUnaryDst | 136-158 | **YES** |
| Appendp | obj/util.go | ProgBuilder.appendAfter | 205-215 | **YES** |
| constAddr | wasmobj.go | constAddr | 248-250 | **YES** |
| regAddr | wasmobj.go | regAddr | 252-254 | **YES** |

---

## ProgBuilder (Cot Addition)

We added a `ProgBuilder` helper (lines 161-232) not present in Go. Go builds Prog chains directly; we use a builder pattern for safer memory management.

| Method | Purpose | Go Equivalent |
|--------|---------|---------------|
| init | Create builder | Direct allocation |
| deinit | Free all progs | Manual freeing |
| append | Add instruction | obj.Appendp |
| appendFrom | Add with from operand | - |
| appendTo | Add with to operand | - |
| appendAfter | Insert after a prog | obj.Appendp |
| iterator | Iterate progs | Direct pointer walking |

---

## What's Different

1. **ProgBuilder**: Added builder pattern for ergonomic Prog chain construction.

2. **Simplified Symbol**: Go's LSym has many fields for relocation, DWARF, etc. We only include what's needed for Wasm.

3. **Value union**: Go uses `interface{}` for Addr.Val; we use a typed union.

---

## Verification

```bash
$ zig test compiler/codegen/wasm/prog.zig
All 3 tests passed.
```

**VERDICT: 90% parity. Core structures match Go. Simplified some fields, added ProgBuilder for ergonomics.**
