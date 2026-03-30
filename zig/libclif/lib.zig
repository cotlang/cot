//! zig/libclif — Native backend for the Cot compiler.
//!
//! Implements the clif.h C ABI: receives CIR bytes, compiles to native .o.
//! Drop-in replacement for rust/libclif.
//!
//! Self-contained — does NOT import from libcot.

const std = @import("std");
const cir_read = @import("cir_read.zig");
const compile_mod = @import("compile.zig");

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
    const result = compileImpl(cir_ptr, cir_len, target_str, out_ptr, out_len) catch |err| {
        std.debug.print("zig-libclif error: {s}\n", .{@errorName(err)});
        return 1;
    };
    _ = result;
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
    _ = target_str;

    if (cir_ptr == null or cir_len == 0) return error.NoCirData;

    const cir_bytes = cir_ptr.?[0..cir_len];
    const allocator = std.heap.page_allocator;

    // Parse CIR
    var module = try cir_read.readModule(allocator, cir_bytes);
    defer module.deinit();

    // TODO: Translate CIR functions → CLIF IR → hand-ported backend → .o
    // For now, verify the CIR parsed correctly by checking function count
    if (module.functions.len == 0) return error.NoFunctions;

    // Placeholder: return empty .o (will be replaced with real compilation)
    _ = out_ptr;
    _ = out_len;
    return error.NotYetImplemented;
}
