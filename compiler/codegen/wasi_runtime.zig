//! WASI Runtime for Cot (WebAssembly)
//!
//! Provides WASI-compatible I/O functions.
//! Reference: WASI preview1 (Go: syscall/fs_wasip1.go, Wasmtime: wasi-common)
//!
//! Functions:
//!   wasi_fd_write(fd, iovs, iovs_len, nwritten) -> i64  — WASI fd_write (stub, ARM64 override)
//!   cot_fd_write_simple(fd, ptr, len) -> i64             — simple write (stub, ARM64 override)
//!   cot_fd_read_simple(fd, buf, len) -> i64              — simple read (stub, ARM64 override)
//!   cot_fd_close(fd) -> i64                              — close fd (stub, ARM64 override)
//!   cot_fd_seek(fd, offset, whence) -> i64               — seek (stub, ARM64 override)
//!   cot_fd_open(path_ptr, path_len, flags) -> i64        — open file (stub, ARM64 override)
//!   cot_time() -> i64                                    — wall clock nanos (stub, ARM64 override)
//!   cot_random(buf, len) -> i64                          — random bytes (stub, ARM64 override)
//!   cot_exit(code) -> void                               — exit process (trap stub, ARM64 override)

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/wasm.zig");
const ValType = wasm_link.ValType;

const wasm_op = @import("wasm_opcodes.zig");

// =============================================================================
// Function Names
// =============================================================================

pub const FD_WRITE_NAME = "wasi_fd_write";
pub const FD_WRITE_SIMPLE_NAME = "cot_fd_write_simple";
pub const FD_READ_SIMPLE_NAME = "cot_fd_read_simple";
pub const FD_CLOSE_NAME = "cot_fd_close";
pub const FD_SEEK_NAME = "cot_fd_seek";
pub const FD_OPEN_NAME = "cot_fd_open";
pub const TIME_NAME = "cot_time";
pub const RANDOM_NAME = "cot_random";
pub const EXIT_NAME = "cot_exit";
pub const ARGS_COUNT_NAME = "cot_args_count";
pub const ARG_LEN_NAME = "cot_arg_len";
pub const ARG_PTR_NAME = "cot_arg_ptr";
pub const ENVIRON_COUNT_NAME = "cot_environ_count";
pub const ENVIRON_LEN_NAME = "cot_environ_len";
pub const ENVIRON_PTR_NAME = "cot_environ_ptr";

// =============================================================================
// Return Type
// =============================================================================

pub const WasiFunctions = struct {
    fd_write_idx: u32,
    fd_write_simple_idx: u32,
    fd_read_simple_idx: u32,
    fd_close_idx: u32,
    fd_seek_idx: u32,
    fd_open_idx: u32,
    time_idx: u32,
    random_idx: u32,
    exit_idx: u32,
    args_count_idx: u32,
    arg_len_idx: u32,
    arg_ptr_idx: u32,
    environ_count_idx: u32,
    environ_len_idx: u32,
    environ_ptr_idx: u32,
};

// =============================================================================
// addToLinker — register all WASI runtime functions
// =============================================================================

pub fn addToLinker(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker) !WasiFunctions {
    // wasi_fd_write: (fd: i64, iovs: i64, iovs_len: i64, nwritten: i64) -> i64
    // Returns WASI errno (0 = success)
    const fd_write_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const fd_write_body = try generateFdWriteStubBody(allocator);
    const fd_write_idx = try linker.addFunc(.{
        .name = FD_WRITE_NAME,
        .type_idx = fd_write_type,
        .code = fd_write_body,
        .exported = true, // So generateMachO can find it by name for ARM64 override
    });

    // cot_fd_write_simple: (fd: i64, ptr: i64, len: i64) -> i64
    // Same signature as cot_write. Exported so native can override with ARM64 syscall.
    // Wasm stub returns 0. On native, ARM64 override does real SYS_write.
    const fd_write_simple_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64 },
        &[_]ValType{.i64},
    );
    const fd_write_simple_body = try generateStubReturnsZero(allocator);
    const fd_write_simple_idx = try linker.addFunc(.{
        .name = FD_WRITE_SIMPLE_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_write_simple_body,
        .exported = true, // So generateMachO can find it for ARM64 override
    });

    // cot_fd_read_simple: (fd: i64, buf: i64, len: i64) -> i64
    // Reference: Go syscall/fs_wasip1.go:900 Read() — builds 1-element iovec, calls fd_read
    // Same pattern as cot_fd_write_simple: stub on Wasm, ARM64 SYS_read override on native.
    // Returns bytes read (0 = EOF).
    const fd_read_simple_body = try generateStubReturnsZero(allocator);
    const fd_read_simple_idx = try linker.addFunc(.{
        .name = FD_READ_SIMPLE_NAME,
        .type_idx = fd_write_simple_type, // Same type: (i64, i64, i64) -> i64
        .code = fd_read_simple_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_fd_close: (fd: i64) -> i64
    // Reference: Go syscall/fs_wasip1.go:203 fd_close(fd int32) Errno
    // Returns 0 on success, WASI errno on error.
    const fd_close_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{.i64},
    );
    const fd_close_body = try generateStubReturnsZero(allocator);
    const fd_close_idx = try linker.addFunc(.{
        .name = FD_CLOSE_NAME,
        .type_idx = fd_close_type,
        .code = fd_close_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_fd_seek: (fd: i64, offset: i64, whence: i64) -> i64
    // Reference: Go syscall/fs_wasip1.go:928 Seek() — lseek(fd, offset, whence) -> newoffset
    // Returns new offset on success. On native, ARM64 override does SYS_lseek.
    const fd_seek_body = try generateStubReturnsZero(allocator);
    const fd_seek_idx = try linker.addFunc(.{
        .name = FD_SEEK_NAME,
        .type_idx = fd_write_simple_type, // Same type: (i64, i64, i64) -> i64
        .code = fd_seek_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_fd_open: (path_ptr: i64, path_len: i64, flags: i64) -> i64
    // Reference: Go syscall/zsyscall_darwin_arm64.go openat() — openat(AT_FDCWD, path, flags, mode)
    // Returns fd on success, errno on error. On native, ARM64 override does SYS_openat.
    const fd_open_body = try generateStubReturnsZero(allocator);
    const fd_open_idx = try linker.addFunc(.{
        .name = FD_OPEN_NAME,
        .type_idx = fd_write_simple_type, // Same type: (i64, i64, i64) -> i64
        .code = fd_open_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_time: () -> i64
    // Reference: Go runtime/sys_darwin_arm64.s walltime_trampoline → clock_gettime(CLOCK_REALTIME)
    // Returns nanoseconds since epoch. On native, ARM64 override does SYS_gettimeofday.
    const time_type = try linker.addType(
        &[_]ValType{},
        &[_]ValType{.i64},
    );
    const time_body = try generateStubReturnsZero(allocator);
    const time_idx = try linker.addFunc(.{
        .name = TIME_NAME,
        .type_idx = time_type,
        .code = time_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_random: (buf: i64, len: i64) -> i64
    // Reference: Go runtime/sys_darwin_arm64.s arc4random_buf_trampoline
    // Fills buf with len random bytes. On native, ARM64 override does SYS_getentropy.
    const random_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );
    const random_body = try generateStubReturnsZero(allocator);
    const random_idx = try linker.addFunc(.{
        .name = RANDOM_NAME,
        .type_idx = random_type,
        .code = random_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_exit: (code: i64) -> void
    // Reference: WASI proc_exit(rval), macOS SYS_exit(1)
    // Exits the process. Never returns. On native, ARM64 override does SYS_exit.
    const exit_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{},
    );
    const exit_body = try generateStubTrap(allocator);
    const exit_idx = try linker.addFunc(.{
        .name = EXIT_NAME,
        .type_idx = exit_type,
        .code = exit_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_args_count: () -> i64
    // Returns number of CLI arguments (argc). On native, reads from vmctx+0x30000.
    const args_count_body = try generateStubReturnsZero(allocator);
    const args_count_idx = try linker.addFunc(.{
        .name = ARGS_COUNT_NAME,
        .type_idx = time_type, // Same type: () -> i64
        .code = args_count_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_arg_len: (n: i64) -> i64
    // Returns length of CLI argument n (strlen(argv[n])). On native, walks argv array.
    const arg_len_body = try generateStubReturnsZero(allocator);
    const arg_len_idx = try linker.addFunc(.{
        .name = ARG_LEN_NAME,
        .type_idx = fd_close_type, // Same type: (i64) -> i64
        .code = arg_len_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_arg_ptr: (n: i64) -> i64
    // Copies CLI argument n into linear memory at reserved offset 0xF0000, returns wasm pointer.
    // On native, copies from real argv[n] into linmem.
    const arg_ptr_body = try generateStubReturnsZero(allocator);
    const arg_ptr_idx = try linker.addFunc(.{
        .name = ARG_PTR_NAME,
        .type_idx = fd_close_type, // Same type: (i64) -> i64
        .code = arg_ptr_body,
        .exported = true, // ARM64 override in driver.zig
    });

    // cot_environ_count: () -> i64
    // Returns number of environment variables. On native, walks envp from vmctx+0x30010.
    const environ_count_body = try generateStubReturnsZero(allocator);
    const environ_count_idx = try linker.addFunc(.{
        .name = ENVIRON_COUNT_NAME,
        .type_idx = time_type, // Same type: () -> i64
        .code = environ_count_body,
        .exported = true, // ARM64/x64 override in driver.zig
    });

    // cot_environ_len: (n: i64) -> i64
    // Returns length of environment variable n (strlen(envp[n])). On native, walks envp array.
    const environ_len_body = try generateStubReturnsZero(allocator);
    const environ_len_idx = try linker.addFunc(.{
        .name = ENVIRON_LEN_NAME,
        .type_idx = fd_close_type, // Same type: (i64) -> i64
        .code = environ_len_body,
        .exported = true, // ARM64/x64 override in driver.zig
    });

    // cot_environ_ptr: (n: i64) -> i64
    // Copies env var n into linear memory at 0x7F000 + n*4096, returns wasm pointer.
    const environ_ptr_body = try generateStubReturnsZero(allocator);
    const environ_ptr_idx = try linker.addFunc(.{
        .name = ENVIRON_PTR_NAME,
        .type_idx = fd_close_type, // Same type: (i64) -> i64
        .code = environ_ptr_body,
        .exported = true, // ARM64/x64 override in driver.zig
    });

    return WasiFunctions{
        .fd_write_idx = fd_write_idx,
        .fd_write_simple_idx = fd_write_simple_idx,
        .fd_read_simple_idx = fd_read_simple_idx,
        .fd_close_idx = fd_close_idx,
        .fd_seek_idx = fd_seek_idx,
        .fd_open_idx = fd_open_idx,
        .time_idx = time_idx,
        .random_idx = random_idx,
        .exit_idx = exit_idx,
        .args_count_idx = args_count_idx,
        .arg_len_idx = arg_len_idx,
        .arg_ptr_idx = arg_ptr_idx,
        .environ_count_idx = environ_count_idx,
        .environ_len_idx = environ_len_idx,
        .environ_ptr_idx = environ_ptr_idx,
    };
}

// =============================================================================
// wasi_fd_write stub — returns ENOSYS (52)
// Pure Wasm can't do I/O; native overrides this with ARM64 syscall
// =============================================================================

fn generateFdWriteStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: fd (local 0), iovs (local 1), iovs_len (local 2), nwritten (local 3)
    // Stub: return 52 (WASI ENOSYS)
    try code.emitI64Const(52);
    return try code.finish();
}

// =============================================================================
// Shared stub — drops args, returns i64(0)
// Used by cot_fd_write_simple, cot_fd_read_simple, cot_fd_close.
// Same pattern as cot_write: Wasm can't do I/O, native overrides with ARM64 syscall.
// Reference: print_runtime.zig generateWriteStubBody
// =============================================================================

fn generateStubReturnsZero(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitI64Const(0);
    return try code.finish();
}

// =============================================================================
// Stub that traps — for functions that should never return (e.g., cot_exit)
// On Wasm: emits unreachable. On native: ARM64 override does real SYS_exit.
// =============================================================================

fn generateStubTrap(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitUnreachable();
    return try code.finish();
}
