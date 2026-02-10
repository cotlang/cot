# Comptime & @target Design

Detailed design for compile-time evaluation and target-conditional compilation in Cot.

## Problem Statement

Cot compiles to Wasm first, then AOT to native. Platform-specific values (file flags, syscall numbers, struct layouts) currently use macOS values as canonical constants with runtime translation on Linux. This is fragile and doesn't scale.

We need: constants that resolve to the correct platform value **at compile time**, matching how Go and Zig solve this.

## Reference Implementations

### Go: File Suffixes + Build Tags + `runtime.GOOS`

Go uses three complementary mechanisms:

**1. File-suffix convention** (compile-time file selection):
```
syscall/zerrors_linux_amd64.go    →  O_CREAT = 0x40
syscall/zerrors_darwin_amd64.go   →  O_CREAT = 0x200
```
The Go toolchain selects files by `_{GOOS}_{GOARCH}.go` suffix. Only the matching file is compiled. No runtime cost.

**2. Build tags** (explicit file-level constraints):
```go
//go:build linux && amd64
```

**3. Runtime constants** (for runtime branching):
```go
if runtime.GOOS == "linux" { ... }
```
`runtime.GOOS` and `runtime.GOARCH` are string constants injected at compile time. The compiler can dead-code-eliminate branches on these constants.

**How `os.O_CREATE` works end-to-end:**
```
os/file.go:        O_CREATE int = syscall.O_CREAT
                                       ↓
syscall/zerrors_linux_amd64.go:   O_CREAT = 0x40    (selected at compile time by filename)
syscall/zerrors_darwin_amd64.go:  O_CREAT = 0x200   (not compiled on linux)
```
User code writes `os.O_CREATE`. The compiler resolves it to the OS-native value. Zero runtime cost.

**For WASI** (Go's Wasm target), Go translates at runtime in `syscall/fs_wasip1.go`:
```go
func openat(..., openmode int, ...) {
    var oflags oflags
    if (openmode & O_CREATE) != 0 { oflags |= OFLAG_CREATE }
    if (openmode & O_TRUNC) != 0  { oflags |= OFLAG_TRUNC }
    // ...
}
```
This is the pattern Cot currently uses: macOS canonical values + runtime translation.

### Zig: `comptime` + `@import("builtin")` + Packed Structs

Zig uses compile-time evaluation as a first-class language feature:

**1. Target detection via `builtin`:**
```zig
const builtin = @import("builtin");
const native_os = builtin.os.tag;    // .linux, .macos, etc.
const native_arch = builtin.cpu.arch; // .x86_64, .aarch64, etc.
```

**2. Comptime switch on target** (type-level):
```zig
// lib/std/c.zig — O flags are a DIFFERENT TYPE per OS
pub const O = switch (native_os) {
    .linux => linux.O,                    // bit 6 = CREAT
    .macos, ... => packed struct(u32) {   // bit 9 = CREAT
        ACCMODE: ACCMODE = .RDONLY,
        NONBLOCK: bool = false,
        APPEND: bool = false,
        // ... bit positions match OS ABI
        CREAT: bool = false,
        TRUNC: bool = false,
        // ...
    },
    .wasi => packed struct(u32) {         // WASI's own bit layout
        APPEND: bool = false,
        // ...
        CREAT: bool = false,
        // ...
    },
};
```
User code writes `.{ .CREAT = true, .TRUNC = true }`. The packed struct layout matches the OS's bit positions. Zero translation needed.

**3. Even within Linux, arch-specific layouts:**
```zig
// lib/std/os/linux.zig — O flags differ by CPU architecture!
pub const O = switch (native_arch) {
    .x86_64 => packed struct(u32) {
        ACCMODE: ACCMODE = .RDONLY,
        _2: u4 = 0,
        CREAT: bool = false,     // bit 6 = 0x40
        // ...
    },
    .mips, .mips64 => packed struct(u32) {
        ACCMODE: ACCMODE = .RDONLY,
        // ... completely different layout (MIPS has O_CREAT = 0x100)
    },
};
```

**Key insight**: Zig doesn't translate flags. The type system ensures the right bits are set at compile time via struct field positions. No magic numbers in user code.

## Design for Cot

### Constraint: Wasm-First Architecture

Cot's pipeline is: `Source → Wasm → CLIF → Native`. The Wasm stage is target-independent. Target-specific code must be resolved **before** Wasm emission, or handled by native overrides **after**.

This means comptime in Cot operates at the **frontend** level (parser/checker/lowerer), not at the Wasm level. The Wasm bytecode is always the same; the native override layer handles platform differences.

### Phase 1: `@target` Builtins (Minimal, Immediate)

Add three compile-time builtins that resolve to string constants during checking:

```cot
@target_os()    // → "macos", "linux", "wasi", "freestanding"
@target_arch()  // → "arm64", "amd64", "wasm32"
@target()       // → "arm64-macos", "amd64-linux", "wasm32", "wasm32-wasi"
```

These are **compiler intrinsics** (like `@sizeOf`), resolved at compile time to string constants. The compiler already knows the target from `Driver.target`.

**Usage in stdlib:**
```cot
// stdlib/fs.cot
const O_CREAT: i64 = if @target_os() == "linux" { 64 } else { 512 }
const O_TRUNC: i64 = if @target_os() == "linux" { 512 } else { 1024 }
const O_APPEND: i64 = if @target_os() == "linux" { 1024 } else { 8 }
```

**Requires**: `if` expressions (Cot already has these). The compiler should constant-fold `@target_os() == "linux"` at compile time and dead-code-eliminate the other branch.

**Implementation** (parser → checker → lowerer):
- Parser: Recognize `@target_os`, `@target_arch`, `@target` as 0-arg builtins
- Checker: Return `TypeRegistry.String` (or a new comptime string type)
- Lowerer: Emit the string constant directly (e.g., `"linux"`)
- Constant folding: `if <comptime_true> { A } else { B }` → `A`

### Phase 2: `comptime` Blocks (Medium-Term)

Full compile-time evaluation, following Zig's model:

```cot
comptime {
    if @target_os() == "linux" {
        const O_CREAT: i64 = 64
    } else {
        const O_CREAT: i64 = 512
    }
}

// Or as expressions:
const O_CREAT = comptime if @target_os() == "linux" { 64 } else { 512 }
```

**Scope**: `comptime` blocks execute during compilation. Variables defined inside are compile-time constants. Only pure expressions allowed (no I/O, no allocation).

**Implementation**: The checker/lowerer evaluates `comptime` blocks immediately. The result is a constant folded into the IR.

### Phase 3: Per-Platform Source Files (Long-Term)

Following Go's file-suffix convention:

```
stdlib/
  fs.cot              // shared code
  fs_linux.cot         // Linux-specific constants and functions
  fs_macos.cot         // macOS-specific constants and functions
  fs_wasi.cot          // WASI-specific implementations
```

The import resolver selects the matching file based on the compilation target. This is the most scalable approach for large platform-specific codebases.

**Implementation**: Modify the import resolver in `driver.zig` to check for `_{os}.cot` and `_{arch}.cot` suffixed files.

## Recommendation: Phase 1 First

Phase 1 (`@target_os()` builtins) is:
- **Minimal implementation**: ~20 lines across parser/checker/lowerer
- **Immediately useful**: Fixes the file flags problem
- **No new syntax**: Uses existing builtins + if-expressions
- **Matches Go's `runtime.GOOS` pattern**: Proven approach
- **Forward-compatible**: Phases 2 and 3 build on it

The native override flag translation in `driver.zig` can then be removed — the stdlib will emit the correct values directly.

## Flag Constants: Correct Approach (Post-Phase 1)

Once `@target_os()` is available, the stdlib should define platform-correct constants:

```cot
// stdlib/fs.cot — correct approach (Go pattern)
const O_RDONLY: i64 = 0
const O_WRONLY: i64 = 1
const O_RDWR: i64 = 2

const O_CREAT: i64 = if @target_os() == "linux" { 64 } else { 512 }
const O_TRUNC: i64 = if @target_os() == "linux" { 512 } else { 1024 }
const O_APPEND: i64 = if @target_os() == "linux" { 1024 } else { 8 }
const O_EXCL: i64 = if @target_os() == "linux" { 128 } else { 2048 }

// Convenience combos
const O_WRITE_CREATE: i64 = O_WRONLY + O_CREAT + O_TRUNC
```

The x64 `cot_fd_open` override then passes flags directly to `openat` — no runtime translation needed. The ARM64 override already does this today.

**Migration plan:**
1. Implement `@target_os()` builtin
2. Update `stdlib/fs.cot` constants to use comptime conditionals
3. Remove flag translation from x64 `cot_fd_open` in `driver.zig`
4. Verify all wasi_io and std_io tests pass on both platforms

## Reference Table: Flag Values by Platform

| Flag | macOS (Darwin) | Linux (x86_64) | WASI (musl) |
|------|---------------|----------------|-------------|
| O_RDONLY | 0x0000 | 0x0000 | 0x0000 |
| O_WRONLY | 0x0001 | 0x0001 | 0x0001 |
| O_RDWR | 0x0002 | 0x0002 | 0x0002 |
| O_APPEND | 0x0008 (bit 3) | 0x0400 (bit 10) | 0x0001 (bit 0) |
| O_CREAT | 0x0200 (bit 9) | 0x0040 (bit 6) | 0x1000 (bit 12) |
| O_TRUNC | 0x0400 (bit 10) | 0x0200 (bit 9) | 0x8000 (bit 15) |
| O_EXCL | 0x0800 (bit 11) | 0x0080 (bit 7) | 0x4000 (bit 14) |

Source: Go `zerrors_darwin_amd64.go`, `zerrors_linux_amd64.go`; Zig `lib/std/c.zig` WASI O struct.

## Appendix: Current Interim Approach

Until `@target_os()` is implemented, Cot uses Go's WASI pattern:
- `stdlib/fs.cot` defines macOS values as canonical
- ARM64 macOS: flags pass through to syscall directly
- x64 Linux: `cot_fd_open` override translates macOS→Linux at runtime (bit extraction + remapping)
- This works but is an extra ~42 bytes of x86 machine code per fd_open call
