# WASI I/O Design: Standard Library Specification

Cot's I/O standard library uses **WASI Preview 1** (`wasi_snapshot_preview1`) as the internal syscall interface between Cot stdlib and the runtime. This document specifies the full design.

---

## 1. Architecture Overview

```
Cot stdlib (fs.cot, os.cot)
        │
        ▼
  WASI function calls
  (fd_write, fd_read, path_open, args_get, ...)
        │
        ├── --target=wasm32-wasi  →  Real WASI imports (host provides)
        ├── --target=native       →  ARM64/x64 syscalls (compiler provides)
        └── --target=wasm32       →  Stubs that trap (no I/O without host)
```

Cot stdlib code is written **once** against WASI APIs. The compiler translates each WASI function to the appropriate target:

| Target | WASI implementation | Who provides it |
|--------|-------------------|-----------------|
| `wasm32-wasi` | Real Wasm imports from `wasi_snapshot_preview1` module | WASI host (wasmtime, Cloudflare Workers, etc.) |
| `native` (default) | Raw OS syscalls (macOS ARM64 `svc #0x80`, Linux x64 `syscall`) | Cot compiler, in `driver.zig` |
| `wasm32` (bare) | Stubs returning `errno.NOSYS` (trap on I/O) | Cot compiler, as Wasm function bodies |

**Proven pattern:** The existing `cot_write` override in `driver.zig:648-675` replaces a Wasm stub with raw ARM64 `SYS_write` machine code. This design extends that pattern to the full WASI Preview 1 surface.

**References:**
- Wasmtime `wasi-common` crate: WASI-to-native syscall translation
- Go `syscall` package: OS syscall wrappers per platform
- WASI Preview 1 spec: Function signatures, iovec format, errno codes

---

## 2. WASI Functions

### Phase 1 — File I/O

| WASI Function | Signature | Purpose |
|---|---|---|
| `fd_write(fd, iovs, iovs_len, nwritten)` | `(i32, i32, i32, i32) → i32` | Write to file descriptor |
| `fd_read(fd, iovs, iovs_len, nread)` | `(i32, i32, i32, i32) → i32` | Read from file descriptor |
| `fd_close(fd)` | `(i32) → i32` | Close file descriptor |
| `fd_seek(fd, offset, whence, newoffset)` | `(i32, i64, i32, i32) → i32` | Seek in file |
| `fd_fdstat_get(fd, fdstat)` | `(i32, i32) → i32` | Get fd metadata (type, flags, rights) |
| `path_open(dirfd, dirflags, path, path_len, oflags, fs_rights_base, fs_rights_inheriting, fdflags, fd)` | `(i32, i32, i32, i32, i32, i64, i64, i32, i32) → i32` | Open file by path |

All functions return `errno` (0 = success). Output values are written to pointers in linear memory.

`fd_write` and `fd_read` use WASI's scatter-gather iovec format:
```
// WASI iovec (8 bytes, packed in linear memory)
struct __wasi_iovec_t {
    buf: u32,     // pointer to buffer in linear memory
    buf_len: u32, // length of buffer
}
```

### Phase 2 — Process

| WASI Function | Signature | Purpose |
|---|---|---|
| `args_get(argv, argv_buf)` | `(i32, i32) → i32` | Get CLI argument pointers + strings |
| `args_sizes_get(argc, argv_buf_size)` | `(i32, i32) → i32` | Get argument count and total buffer size |
| `environ_get(environ, environ_buf)` | `(i32, i32) → i32` | Get environment variable pointers + strings |
| `environ_sizes_get(environc, environ_buf_size)` | `(i32, i32) → i32` | Get env var count and total buffer size |
| `proc_exit(rval)` | `(i32) → ∅` | Exit process (no return) |

WASI args/environ use a two-call pattern: first call `*_sizes_get` to learn buffer sizes, then allocate, then call `*_get` to fill the buffers.

### Phase 3 — Clock / Random

| WASI Function | Signature | Purpose |
|---|---|---|
| `clock_time_get(id, precision, time)` | `(i32, i64, i32) → i32` | Get current time (nanoseconds) |
| `random_get(buf, buf_len)` | `(i32, i32) → i32` | Fill buffer with cryptographic random bytes |

Clock IDs: `0` = realtime, `1` = monotonic.

---

## 3. Cot Standard Library API

The stdlib is written in Cot, calling WASI functions as external imports. API design follows Go's `os` package pattern (simple, concrete types, no unnecessary abstraction).

### std/fs — File I/O

```cot
struct File {
    fd: i64
}

impl File {
    // Open a file by path. Returns File with fd, or error.
    fn open(path: []u8) File

    // Read into buffer. Returns number of bytes read.
    fn read(self: *File, buf: []u8) i64

    // Write data. Returns number of bytes written.
    fn write(self: *File, data: []u8) i64

    // Seek to offset. whence: 0=SET, 1=CUR, 2=END. Returns new position.
    fn seek(self: *File, offset: i64, whence: i64) i64

    // Close the file descriptor.
    fn close(self: *File) void
}

// Convenience: read entire file contents into allocated buffer.
fn readFile(path: []u8) []u8

// Convenience: write data to file, creating/truncating as needed.
fn writeFile(path: []u8, data: []u8) void
```

Well-known file descriptors:
- `File { .fd = 0 }` — stdin
- `File { .fd = 1 }` — stdout
- `File { .fd = 2 }` — stderr

### std/os — Process

```cot
// Get CLI arguments as a list of strings.
fn args() List([]u8)

// Get environment variable by key. Returns empty string if not found.
fn getenv(key: []u8) []u8

// Exit the process with the given exit code. Does not return.
fn exit(code: i64) void
```

### std/time — Clock (Phase 3)

```cot
// Get current wall-clock time in nanoseconds since Unix epoch.
fn now() i64

// Get monotonic time in nanoseconds (for measuring durations).
fn monotonic() i64
```

### std/random — Random (Phase 3)

```cot
// Fill buffer with cryptographic random bytes.
fn randomBytes(buf: []u8) void
```

---

## 4. Compiler Implementation — Three Targets

### Target: `--target=wasm32-wasi`

Each WASI function becomes a Wasm import from the `wasi_snapshot_preview1` module. The infrastructure already exists in `link.zig`:

```zig
// link.zig already supports this pattern:
const import = WasmImport{
    .module = "wasi_snapshot_preview1",
    .name = "fd_write",
    .type_idx = fd_write_type_idx,
};
const fd_write_idx = try linker.addImport(import);
```

The `WasmImport` struct (`link.zig:55-59`) has `module` and `name` fields ready for WASI. `addImport()` (`link.zig:156-160`) returns the import's function index. Cot stdlib calls these imports by index — no runtime code needed; the WASI host provides implementations.

**Required changes:** Add WASI imports alongside existing function registrations in the linker. Stdlib Cot files call these via `@wasiCall` or equivalent builtin.

### Target: `--target=native` (macOS ARM64 + Linux x64)

Extend the `driver.zig` override pattern from `cot_write` to all WASI functions. Currently (`driver.zig:648-675`):

1. `cot_write` is registered as a Wasm function with a stub body (returns 0)
2. During native codegen, `driver.zig` detects `cot_write` by name
3. It replaces the compiled function body with raw ARM64 machine code (`svc #0x80`)

For WASI functions, the same pattern applies:

1. Register each WASI function as a Wasm module function (stub body for Wasm, real body for native)
2. During `generateMachO`, detect WASI function names and replace with platform-specific machine code
3. Each WASI function gets a hand-written ARM64 sequence (and later x64)

**Native implementation details:**

Each WASI function translates its WASI-format arguments (pointers into linear memory, iovec arrays) to OS syscall arguments:

```
fd_write(fd=i32, iovs=i32, iovs_len=i32, nwritten=i32) → errno
  ↓ native override
  1. Read iovec array from linear_memory[iovs..iovs + iovs_len * 8]
  2. For each iovec: translate buf pointer (linmem + buf_offset)
  3. Call SYS_writev (or loop SYS_write for each iovec)
  4. Write bytes_written to linear_memory[nwritten]
  5. Return 0 (success) or WASI errno
```

The Cranelift calling convention passes `vmctx` in x0, which contains the linear memory base at a known offset. The existing `cot_write` override already demonstrates reading the linear memory base:
```asm
add x8, x0, #0x40, lsl #12   // x8 = vmctx + 0x40000 (linear memory base)
add x1, x8, x3                // real_ptr = linmem + wasm_ptr
```

**Simplification for single-buffer calls:** Since Cot stdlib always passes a single buffer (1-element iovec), the native override can fast-path this case without looping.

### Target: `--target=wasm32` (bare Wasm)

WASI functions are registered as module functions with stub bodies that return `errno.NOSYS` (76). Programs that don't use I/O compile and run fine. Programs that call I/O functions get a predictable error.

Exception: `print`/`println` continue to use the existing `cot_write` path, which is independent of WASI.

---

## 5. WASI iovec Adapter Pattern

WASI `fd_read` and `fd_write` use scatter-gather I/O with iovec arrays. The adapter translates between Cot's simple buffer API and WASI's iovec format.

### Cot stdlib side (runs as Wasm)

```
File.write(self, data: []u8) → i64:
    // Build 1-element iovec on Wasm stack
    stack_alloc(8)                    // room for 1 iovec
    i32.store(sp, data.ptr)           // iovec[0].buf = data pointer
    i32.store(sp + 4, data.len)       // iovec[0].buf_len = data length
    stack_alloc(4)                    // room for nwritten output
    fd_write(self.fd, sp_iovec, 1, sp_nwritten)
    result = i32.load(sp_nwritten)    // read bytes written
    return result
```

### Native override side (runs as ARM64)

```
fd_write native override:
    // Inputs: fd, iovs_ptr (wasm), iovs_len, nwritten_ptr (wasm)
    // x8 = linear_memory_base (from vmctx)

    // For each iovec entry:
    //   real_buf = linmem + i32.load(linmem + iovs_ptr + i*8)
    //   buf_len  = i32.load(linmem + iovs_ptr + i*8 + 4)
    //   SYS_write(fd, real_buf, buf_len)
    //   total += bytes_written

    // Store total to linmem + nwritten_ptr
    // Return 0 (success)
```

For the common 1-iovec case, this simplifies to a single `SYS_write` syscall — essentially the same code as the current `cot_write` override, with the addition of reading the iovec from memory and writing the result back.

**Reference:** Wasmtime `wasi-common/src/snapshots/preview_1.rs` — `fd_write` implementation reads iovecs from guest memory, translates pointers, calls host `writev`.

---

## 6. Migration Path from cot_write

### Current state

- `cot_write(fd, ptr, len) → i64` — simple 3-argument write
- Registered in `print_runtime.zig:addToLinker()` as a Wasm module function
- Stub body for Wasm (returns 0), ARM64 override for native (`driver.zig:648-675`)
- Called by `cot_print_int` and `cot_eprint_int` (also in `print_runtime.zig`)
- Used by `print`, `println`, `eprint`, `eprintln` builtins

### Target state

- `fd_write(fd, iovs, iovs_len, nwritten) → errno` — WASI-compliant 4-argument write
- Registered as a WASI function alongside existing runtime functions
- Cot stdlib `File.write()` calls `fd_write`

### Migration strategy: coexistence, not replacement

```
print("hello")  →  cot_print_int / cot_write   (existing path, unchanged)
File.write(data) →  fd_write                     (new WASI path)
```

Both paths coexist. `cot_write` remains as the internal helper for `print`/`println` builtins. `fd_write` is the WASI-compliant function for stdlib `File` operations. No breaking changes.

Eventually, `cot_write` could be reimplemented on top of `fd_write` (building a 1-element iovec and calling it), but this is optional and low priority.

---

## 7. Implementation Phases

| Phase | What | Files to modify | Effort |
|-------|------|-----------------|--------|
| **1a** | `fd_write` + `fd_read` + `fd_close` WASI functions | `driver.zig`, `arc.zig` or new `wasi_runtime.zig`, `link.zig` | Medium |
| **1b** | `path_open` + `fd_seek` + `fd_fdstat_get` | `driver.zig` (native overrides) | Medium |
| **1c** | `std/fs.cot` stdlib — File struct with open/read/write/seek/close | `stdlib/fs.cot` (new) | Small |
| **2a** | `args_get` + `args_sizes_get` native implementation | `driver.zig` | Small |
| **2b** | `environ_get` + `environ_sizes_get` native implementation | `driver.zig` | Small |
| **2c** | `std/os.cot` stdlib — args(), getenv(), exit() | `stdlib/os.cot` (new) | Small |
| **3a** | `clock_time_get` + `random_get` native implementation | `driver.zig` | Small |
| **3b** | `std/time.cot` + `std/random.cot` stdlib | `stdlib/time.cot`, `stdlib/random.cot` (new) | Small |

**Phase 1a is the critical path** — it proves the full WASI function pattern (registration, iovec handling, native override, Wasm import) end-to-end. All subsequent phases follow the same pattern.

### Dependency graph

```
Phase 1a (fd_write/fd_read/fd_close)
    ↓
Phase 1b (path_open/fd_seek)
    ↓
Phase 1c (std/fs.cot)    Phase 2a (args)    Phase 3a (clock/random)
                              ↓                    ↓
                          Phase 2b (environ)   Phase 3b (std/time, std/random)
                              ↓
                          Phase 2c (std/os.cot)
```

Phases 1c, 2a, and 3a can proceed in parallel once 1a establishes the pattern.

---

## 8. Native Syscall Reference Table

### macOS ARM64

Syscall convention: `x16` = syscall number, `svc #0x80`, args in `x0-x5`, return in `x0`. Error: carry flag set, `x0` = errno.

| WASI Function | macOS Syscall | Number | Notes |
|---|---|---|---|
| `fd_write` | `SYS_write` | 4 | `write(fd, buf, nbyte)` — existing `cot_write` uses this |
| `fd_read` | `SYS_read` | 3 | `read(fd, buf, nbyte)` |
| `fd_close` | `SYS_close` | 6 | `close(fd)` |
| `fd_seek` | `SYS_lseek` | 199 | `lseek(fd, offset, whence)` |
| `path_open` | `SYS_openat` | 463 | `openat(dirfd, path, flags, mode)` — prefer over `SYS_open` (5) |
| `proc_exit` | `SYS_exit` | 1 | `exit(status)` |
| `clock_time_get` | `SYS_clock_gettime` | — | Not available on macOS; use `mach_absolute_time` or `SYS_gettimeofday` (116) |
| `random_get` | `SYS_getentropy` | 244 | `getentropy(buf, buflen)` — max 256 bytes per call |

### Linux x64

Syscall convention: `rax` = syscall number, `syscall` instruction, args in `rdi, rsi, rdx, r10, r8, r9`, return in `rax`. Error: negative return = -errno.

| WASI Function | Linux Syscall | Number | Notes |
|---|---|---|---|
| `fd_write` | `SYS_write` | 1 | `write(fd, buf, count)` |
| `fd_read` | `SYS_read` | 0 | `read(fd, buf, count)` |
| `fd_close` | `SYS_close` | 3 | `close(fd)` |
| `fd_seek` | `SYS_lseek` | 8 | `lseek(fd, offset, whence)` |
| `path_open` | `SYS_openat` | 257 | `openat(dirfd, pathname, flags, mode)` |
| `proc_exit` | `SYS_exit_group` | 231 | `exit_group(status)` — kills all threads |
| `clock_time_get` | `SYS_clock_gettime` | 228 | `clock_gettime(clockid, timespec)` |
| `random_get` | `SYS_getrandom` | 318 | `getrandom(buf, buflen, flags)` |

### WASI errno to native errno mapping

WASI defines its own errno constants. The native override must translate OS errno to WASI errno on error:

| WASI errno | Value | Meaning |
|---|---|---|
| `ESUCCESS` | 0 | No error |
| `EBADF` | 8 | Bad file descriptor |
| `EINVAL` | 28 | Invalid argument |
| `EIO` | 29 | I/O error |
| `ENOENT` | 44 | No such file or directory |
| `ENOSYS` | 52 | Function not supported |
| `EPERM` | 63 | Operation not permitted |
| `EACCES` | 2 | Permission denied |

### Process args/environ on native

Unlike file I/O, `args_get` and `environ_get` don't map to a single syscall. On native:

- **macOS/Linux:** The kernel places `argc`, `argv` pointers, and `envp` pointers on the stack at process start. The `_start` entry point (or `main` wrapper) receives them.
- **Implementation:** The `generateMainWrapperMachO` in `driver.zig` already sets up the entry point. Extend it to save `argc`/`argv`/`envp` to known locations in linear memory. The WASI `args_get`/`environ_get` overrides then copy from those saved locations.

---

## 9. Open Questions

1. **WASI function registration location:** Should WASI functions go in `print_runtime.zig` (renamed to `runtime.zig`), a new `wasi_runtime.zig`, or `arc.zig`? Recommendation: new `wasi_runtime.zig` to keep concerns separated.

2. **Vectored I/O:** Should `fd_write`/`fd_read` native overrides use `SYS_writev`/`SYS_readv` (single syscall for multiple iovecs) or loop over `SYS_write`/`SYS_read`? Recommendation: start with loop (simpler), optimize to `writev`/`readv` later if needed.

3. **Error handling in stdlib:** Cot has error unions (Zig pattern). Should `File.open()` return `File!Error` (error union) or trap on failure? Recommendation: error unions — matches language design.

4. **Multi-file imports:** Currently Cot's multi-file support uses `SharedGenericContext`. Stdlib modules (`std/fs.cot`, `std/os.cot`) will need to be importable. This depends on the import system design, which is a separate concern.
