//! WASI Runtime for Cot (WebAssembly)
//!
//! Provides WASI-compatible I/O functions.
//! Reference: WASI preview1 fd_write (Go: syscall/fs_wasip1.go, Wasmtime: wasi-common)
//!
//! Two functions:
//!   wasi_fd_write(fd, iovs, iovs_len, nwritten) -> i64  — WASI fd_write (stub in Wasm, ARM64 override in native)
//!   cot_fd_write_simple(fd, ptr, len) -> i64             — adapter: builds iovec, calls wasi_fd_write

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

// =============================================================================
// Return Type
// =============================================================================

pub const WasiFunctions = struct {
    fd_write_idx: u32,
    fd_write_simple_idx: u32,
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
    const fd_write_simple_body = try generateFdWriteSimpleStubBody(allocator);
    const fd_write_simple_idx = try linker.addFunc(.{
        .name = FD_WRITE_SIMPLE_NAME,
        .type_idx = fd_write_simple_type,
        .code = fd_write_simple_body,
        .exported = true, // So generateMachO can find it for ARM64 override
    });

    return WasiFunctions{
        .fd_write_idx = fd_write_idx,
        .fd_write_simple_idx = fd_write_simple_idx,
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
// cot_fd_write_simple stub — drops args, returns 0
// Same pattern as cot_write: Wasm can't do I/O, native overrides with ARM64 syscall.
// =============================================================================

fn generateFdWriteSimpleStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Parameters: fd (local 0), ptr (local 1), len (local 2) — all i64
    // Stub: just return 0 (Wasm can't do I/O)
    try code.emitI64Const(0);
    return try code.finish();
}
