# Stdlib: `rename` Implementation Plan

## Overview

Add `rename(oldpath, newpath)` to the filesystem stdlib, following the exact pattern used by `deleteFile`/`unlink`.

## Reference: Zig `std.fs.rename`

Zig's `renameatZ` calls `system.renameat` → libc `rename(2)`. Simple: two null-terminated paths, returns 0 or -1 with errno.

## Changes Required

### 1. `stdlib/sys.cot` — extern fn declaration

```cot
extern fn rename(old_ptr: i64, old_len: i64, new_ptr: i64, new_len: i64) i64
```

Pattern matches `unlink(path_ptr: i64, path_len: i64) i64`.

### 2. `stdlib/fs.cot` — high-level API

```cot
fn renameFile(oldpath: string, newpath: string) FsError!void {
    var result = rename(@ptrOf(oldpath), @lenOf(oldpath), @ptrOf(newpath), @lenOf(newpath))
    if (result < 0) {
        return error.IoError
    }
}
```

Pattern matches `deleteFile`:
```cot
fn deleteFile(path: string) FsError!void {
    var result = unlink(@ptrOf(path), @lenOf(path))
    if (result < 0) {
        return error.IoError
    }
}
```

### 3. `compiler/codegen/native/io_native.zig` — `generateRename`

CLIF IR function following `generateUnlink` pattern (line ~2985), but with TWO paths:

```
fn cot_rename(old_ptr: i64, old_len: i64, new_ptr: i64, new_len: i64) -> i64
  1. Create stack slot for old_path (1024 bytes)
  2. Create stack slot for new_path (1024 bytes)
  3. Memcpy old_ptr → old_slot for old_len bytes
  4. Null-terminate old_slot[old_len] = 0
  5. Memcpy new_ptr → new_slot for new_len bytes
  6. Null-terminate new_slot[new_len] = 0
  7. Call c_rename(old_slot_addr, new_slot_addr)
  8. Check result: 0 → return 0, else → return -1
```

Key details:
- Function name: `cot_rename` (avoids libc collision)
- Libc symbol: `c_rename` (alias for libc `rename`)
- Two stack slots needed (unlike `unlink` which has one)
- Both paths need null-termination (libc expects C strings)

### 4. `compiler/driver.zig` — registration

Add to `runtime_func_names` array:
```zig
"cot_rename",
```

Add libc alias to `func_index_map`:
```zig
// In the alias block after runtime functions
try self.func_index_map.put("c_rename", ...); // maps to libc "rename"
```

Add to extern fn name mapping (same pattern as `cot_unlink` → `unlink`):
```zig
"rename" => "cot_rename",
```

### 5. `compiler/codegen/wasi_runtime.zig` — Wasm stub

Add stub returning -1 (same as `unlink`):
```zig
try generateStubReturnsNegOne(wasm_bin, "rename");
```

WASI doesn't have a direct `path_rename` equivalent that maps cleanly. Stub for now.

### 6. `compiler/codegen/native/io_native.zig` — wire into `generateAllFunctions`

Add call to `generateRename` in the function generation list (same location as `generateUnlink`).

## Testing

### `test/cases/fs.cot` or `test/e2e/features.cot`

```cot
test "rename file" {
    var path1 = "/tmp/cot_test_rename_old.txt"
    var path2 = "/tmp/cot_test_rename_new.txt"
    writeFile(path1, "hello")
    renameFile(path1, path2) catch { @assert(false) }
    var content = readFile(path2) catch ""
    @assertEq(content, "hello")
    deleteFile(path2) catch {}
}
```

## Where It's Needed

`cotlang/cotty` — file tree rename operation in Cotty IDE.

## Checklist

- [ ] `stdlib/sys.cot` — add `extern fn rename`
- [ ] `stdlib/fs.cot` — add `fn renameFile`
- [ ] `compiler/codegen/native/io_native.zig` — add `generateRename`
- [ ] `compiler/codegen/native/io_native.zig` — wire into `generateAllFunctions`
- [ ] `compiler/driver.zig` — add to `runtime_func_names`
- [ ] `compiler/driver.zig` — add `c_rename` alias and `rename` → `cot_rename` mapping
- [ ] `compiler/codegen/wasi_runtime.zig` — add stub
- [ ] Test on native
- [ ] Test on wasm (expect stub behavior)
