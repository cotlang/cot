# Wasm Imports Implementation Audit

## Reference
- Primary: `~/learning/go/src/cmd/link/internal/wasm/asm.go`
- Secondary: Wasm binary format specification

## Summary

**Status: CORRECTLY COPIES Go's import section logic**

The implementation matches Go's writeImportSec pattern with acceptable simplifications.

---

## Binary Format Verification

Per Wasm spec, import section format:
```
import_section ::= section_2(vec(import))
import ::= module:name name:name d:importdesc
importdesc ::= 0x00 x:typeidx  (func)
```

**Cot encodes identically to Go:**
1. Section ID (0x02)
2. Section size (ULEB128)
3. Import count (ULEB128)
4. For each import:
   - Module name length + bytes
   - Function name length + bytes
   - Kind byte (0x00 for function)
   - Type index (ULEB128)

---

## Code Comparison

### 1. Import Struct

**Cot (link.zig lines 54-59):**
```zig
pub const WasmImport = struct {
    module: []const u8,
    name: []const u8,
    type_idx: u32,
};
```

**Go (asm.go + obj/link.go):**
```go
type wasmFunc struct {
    Module string
    Name   string
    Type   uint32
    Code   []byte
}
```

**Assessment:** ✅ Structurally equivalent

---

### 2. Import Collection

**Cot (link.zig lines 128-137):**
```zig
pub fn addImport(self: *Linker, import: WasmImport) !u32 {
    const idx: u32 = @intCast(self.imports.items.len);
    try self.imports.append(self.allocator, import);
    return idx;
}
```

**Go (asm.go lines 154-181):**
```go
// During relocation pass
for ri := 0; ri < relocs.Count(); ri++ {
    r := relocs.At(ri)
    switch r.Type() {
    case objabi.R_WASMIMPORT:
        hostImportMap[s] = int64(len(hostImports))
        hostImports = append(hostImports, wasmFunc{...})
    }
}
```

**Assessment:** ✅ Same sequential index assignment pattern

---

### 3. writeImportSec

**Cot (link.zig lines 210-228):**
```zig
try assemble.writeULEB128(self.allocator, &import_buf, self.imports.items.len);
for (self.imports.items) |imp| {
    try assemble.writeULEB128(self.allocator, &import_buf, imp.module.len);
    try import_buf.appendSlice(self.allocator, imp.module);
    try assemble.writeULEB128(self.allocator, &import_buf, imp.name.len);
    try import_buf.appendSlice(self.allocator, imp.name);
    try import_buf.append(self.allocator, 0x00); // func kind
    try assemble.writeULEB128(self.allocator, &import_buf, imp.type_idx);
}
```

**Go (asm.go lines 316-334):**
```go
writeUleb128(ctxt.Out, uint64(len(hostImports)))
for _, fn := range hostImports {
    if fn.Module != "" {
        writeName(ctxt.Out, fn.Module)
    } else {
        writeName(ctxt.Out, wasm.GojsModule)
    }
    writeName(ctxt.Out, fn.Name)
    ctxt.Out.WriteByte(0x00) // func
    writeUleb128(ctxt.Out, uint64(fn.Type))
}
```

**Assessment:** ✅ Identical encoding logic

**Minor deviation:** Go has `GojsModule` fallback when Module is empty. Cot requires explicit module names. This is acceptable - Cot's model is simpler and the fallback can be added if needed for browser interop.

---

### 4. Function Index Offsetting

**Cot (link.zig lines 308-318):**
```zig
const import_count = self.imports.items.len;
for (self.funcs.items, 0..) |f, i| {
    if (f.exported) {
        // Function index = import_count + native_index
        try assemble.writeULEB128(self.allocator, &export_buf, import_count + i);
    }
}
```

**Go (asm.go export section):**
```go
idx := uint32(lenHostImports) + uint32(ldr.SymValue(s)>>16) - funcValueOffset
writeUleb128(ctxt.Out, uint64(idx))
```

**Assessment:** ✅ Same invariant: `export_idx = import_count + native_func_idx`

Go's formula is more complex due to runtime address encoding (funcValueOffset = 0x1000), but achieves the same result.

---

## Test Coverage

**Cot test (link.zig lines 474-534):**
- Creates import with type
- Verifies import index assignment
- Verifies import section presence in binary

**What's tested:**
- ✅ addImport returns correct indices
- ✅ numImports returns correct count
- ✅ Import section appears in output
- ✅ Function indices are offset correctly

---

## Deviations from Go (All Acceptable)

| Aspect | Go | Cot | Reason |
|--------|-----|-----|--------|
| Module fallback | "gojs" default | Explicit only | Simpler model |
| Type storage | Params/Results | type_idx | Pre-resolved at creation |
| Collection timing | Relocation pass | Explicit API | Linker abstraction |

---

## Conclusion

**PASSES AUDIT** - The implementation correctly copies Go's import section encoding. No invented logic detected. Binary format matches Wasm spec exactly.
