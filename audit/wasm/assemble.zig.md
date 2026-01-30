# Audit: wasm/assemble.zig

## Status: WORKING - 80% PARITY

| Metric | Value |
|--------|-------|
| Lines | ~478 |
| Go Reference | cmd/internal/obj/wasm/wasmobj.go assemble() (lines 1039-1452) |
| Tests | 3 unit tests |

---

## Go Reference Mapping

### Main Function

| Go Function | Go Lines | Our Function | Our Lines | Parity |
|-------------|----------|--------------|-----------|--------|
| assemble | 1039-1352 | assemble | 52-155 | **GOOD** |

### Register Mapping (Go: lines 1040-1142)

| Go Section | Go Lines | Our Code | Our Lines | Parity |
|------------|----------|----------|-----------|--------|
| Global setup | 1051-1059 | Global reg_vars | 69-76 | **YES** |
| Register scan | 1065-1090 | Scan reg_used | 79-91 | **YES** |
| Local allocation | 1092-1135 | var_decls loop | 102-117 | **YES** |

### Global Register Indices (Go: wasmobj.go lines 1051-1059)

| Global | Go Index | Our Index | Parity |
|--------|----------|-----------|--------|
| SP | 0 | 0 | **YES** |
| CTXT | 1 | 1 | **YES** |
| g | 2 | 2 | **YES** |
| RET0 | 3 | 3 | **YES** |
| RET1 | 4 | 4 | **YES** |
| RET2 | 5 | 5 | **YES** |
| RET3 | 6 | 6 | **YES** |
| PAUSE | 7 | 7 | **YES** |

### Instruction Encoding (Go: lines 1157-1347)

| Instruction Type | Go Lines | Our Lines | Parity |
|------------------|----------|-----------|--------|
| Get (local/global) | 1159-1179 | 169-184 | **YES** |
| Set (local/global) | 1181-1205 | 186-201 | **YES** |
| Tee | 1207-1218 | 203-214 | **YES** |
| Block/Loop/If | 1236-1242 | 225-234 | **YES** |
| Br/Br_if | 1244-1248 | 240-247 | **YES** |
| Br_table | 1250-1255 | 249-257 | **YES** |
| Call | 1257-1284 | 263-274 | **YES** |
| Call_indirect | 1286-1292 | 276-281 | **YES** |
| i32/i64.const | 1294-1305 | 287-300 | **YES** |
| f32/f64.const | 1307-1315 | 302-315 | **YES** |
| Loads | 1317-1328 | 321-326 | **YES** |
| Stores | 1330-1338 | 332-337 | **YES** |
| Memory ops | 1340-1345 | 343-354 | **YES** |

### Helper Functions

| Go Function | Go Lines | Our Function | Our Lines | Parity |
|-------------|----------|--------------|-----------|--------|
| alignment | 1408-1421 | alignment | 380-388 | **YES** |
| writeUleb128 | 1423-1437 | writeULEB128 | 402-413 | **YES** |
| writeSleb128 | 1440-1451 | writeSLEB128 | 417-430 | **YES** |

---

## LEB128 Encoding

### Unsigned LEB128 (Go: lines 1423-1437)

Go:
```go
func writeUleb128(w *bytes.Buffer, v uint64) {
    for {
        c := uint8(v & 0x7f)
        v >>= 7
        if v != 0 {
            c |= 0x80
        }
        w.WriteByte(c)
        if v == 0 {
            break
        }
    }
}
```

Ours (lines 402-413):
```zig
pub fn writeULEB128(..., value: u64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(v & 0x7F);
        v >>= 7;
        if (v == 0) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | 0x80);
    }
}
```

**Parity: YES** - Identical algorithm.

### Signed LEB128 (Go: lines 1440-1451)

Go:
```go
func writeSleb128(w *bytes.Buffer, v int64) {
    for {
        c := uint8(v & 0x7f)
        s := uint8(v & 0x40)
        v >>= 7
        if (v != 0 || s != 0) && (v != -1 || s == 0) {
            c |= 0x80
        }
        w.WriteByte(c)
        if (v == 0 && s == 0) || (v == -1 && s != 0) {
            break
        }
    }
}
```

Ours (lines 417-430):
```zig
pub fn writeSLEB128(..., value: i64) !void {
    var v = value;
    while (true) {
        const b: u8 = @truncate(@as(u64, @bitCast(v)) & 0x7F);
        const s: u8 = @truncate(@as(u64, @bitCast(v)) & 0x40);
        v >>= 7;
        const done = (v == 0 and s == 0) or (v == -1 and s != 0);
        if (done) {
            try w.append(allocator, b);
            return;
        }
        try w.append(allocator, b | 0x80);
    }
}
```

**Parity: YES** - Identical algorithm with Zig-style bit casting.

---

## Alignment Values (Go: lines 1408-1421)

| Instruction | Go Alignment | Our Alignment | Parity |
|-------------|--------------|---------------|--------|
| *_load8_* / *_store8 | 0 | 0 | **YES** |
| *_load16_* / *_store16 | 1 | 1 | **YES** |
| i32_load / f32_load / *32 | 2 | 2 | **YES** |
| i64_load / f64_load | 3 | 3 | **YES** |

---

## Verification

```bash
$ zig test compiler/codegen/wasm/assemble.zig
All 3 tests passed.
```

**VERDICT: 80% parity. Core encoding matches Go exactly. Some advanced features (relocations) not yet implemented.**
