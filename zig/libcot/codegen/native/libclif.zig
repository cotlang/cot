//! C ABI bindings to rust/libclif — the Cranelift native backend.
//!
//! libclif receives CIR bytes and compiles them to a native object file.
//! This replaces the hand-ported machinst/regalloc/isa code with real Cranelift.

const std = @import("std");

// C ABI functions exported by rust/libclif
extern "C" fn clif_compile(
    cir_ptr: ?[*]const u8,
    cir_len: usize,
    target_str: [*:0]const u8,
    out_ptr: *[*]const u8,
    out_len: *usize,
) callconv(.c) i32;

extern "C" fn clif_free(ptr: [*]u8, len: usize) callconv(.c) void;
extern "C" fn clif_version() callconv(.c) [*:0]const u8;

/// Compile CIR bytes to a native object file using Cranelift.
/// Returns the .o file bytes (caller must call freeObjectBytes when done).
pub fn compileToObject(cir_bytes: []const u8, target: []const u8) ![]const u8 {
    // Null-terminate the target string for C ABI
    var target_buf: [64]u8 = undefined;
    if (target.len >= target_buf.len) return error.TargetTooLong;
    @memcpy(target_buf[0..target.len], target);
    target_buf[target.len] = 0;

    var out_ptr: [*]const u8 = undefined;
    var out_len: usize = 0;

    const result = clif_compile(
        if (cir_bytes.len > 0) cir_bytes.ptr else null,
        cir_bytes.len,
        @ptrCast(&target_buf),
        &out_ptr,
        &out_len,
    );

    if (result != 0) return error.ClifCompileFailed;

    return out_ptr[0..out_len];
}

/// Free object bytes returned by compileToObject.
pub fn freeObjectBytes(bytes: []const u8) void {
    clif_free(@constCast(bytes.ptr), bytes.len);
}

/// Get the version string of the Cranelift backend.
pub fn version() []const u8 {
    const ptr = clif_version();
    return std.mem.sliceTo(ptr, 0);
}
