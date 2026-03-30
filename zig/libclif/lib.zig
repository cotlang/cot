//! zig/libclif — Native backend for the Cot compiler.
//!
//! Implements the clif.h C ABI: receives CIR bytes, compiles to native .o.
//! Drop-in replacement for rust/libclif.
//!
//! Self-contained — does NOT import from libcot.

const std = @import("std");
const cir_read = @import("cir_read.zig");
const cir_translate = @import("cir_translate.zig");

// Re-exports for internal use
pub const clif = @import("clif_ir/mod.zig");
pub const types_mod = @import("types.zig");
pub const target_mod = @import("target.zig");
pub const debug_mod = @import("debug.zig");

// --- clif.h C ABI implementation ---

/// Compile CIR bytes to a native object file.
/// Returns 0 on success, non-zero on error.
/// On success, out_ptr/out_len contain the .o file bytes (free with clif_free).
export fn clif_compile(
    cir_ptr: ?[*]const u8,
    cir_len: usize,
    target_str: [*:0]const u8,
    out_ptr: *[*]const u8,
    out_len: *usize,
) callconv(.c) i32 {
    compileImpl(cir_ptr, cir_len, target_str, out_ptr, out_len) catch |err| {
        std.debug.print("zig-libclif error: {s}\n", .{@errorName(err)});
        return 1;
    };
    return 0;
}

/// Free bytes previously returned by clif_compile.
export fn clif_free(ptr: [*]u8, len: usize) callconv(.c) void {
    if (len > 0) {
        std.heap.page_allocator.free(ptr[0..len]);
    }
}

/// Returns the version string.
export fn clif_version() callconv(.c) [*:0]const u8 {
    return "libclif 0.1.0 (zig hand-ported cranelift)";
}

fn compileImpl(
    cir_ptr: ?[*]const u8,
    cir_len: usize,
    target_str: [*:0]const u8,
    out_ptr: *[*]const u8,
    out_len: *usize,
) !void {
    if (cir_ptr == null or cir_len == 0) return error.NoCirData;

    const cir_bytes = cir_ptr.?[0..cir_len];
    const allocator = std.heap.page_allocator;

    // Resolve target ISA
    _ = target_str;
    const compile_mod = @import("compile.zig");
    const isa = compile_mod.detectNativeIsa();

    // Parse CIR
    var module = try cir_read.readModule(allocator, cir_bytes);
    defer module.deinit();

    // Translate CIR → CLIF IR → hand-ported backend → .o
    const obj_bytes = try cir_translate.translateModule(allocator, &module, isa);

    // Copy to caller-owned memory
    out_ptr.* = obj_bytes.ptr;
    out_len.* = obj_bytes.len;
}
