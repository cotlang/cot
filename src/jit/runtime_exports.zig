//! JIT Runtime Exports
//!
//! This module exports Cot runtime functions with C calling convention
//! so that JIT-compiled native code can call them.
//!
//! These functions are registered with the Cranelift JIT context during
//! initialization. The JIT-compiled code calls them via function pointers.
//!
//! Naming convention: cot_rt_<category>_<operation>
//!   - cot_rt_str_* : String operations
//!   - cot_rt_dec_* : Decimal operations
//!   - cot_rt_io_*  : I/O operations
//!   - cot_rt_mem_* : Memory management
//!   - cot_rt_err_* : Error/exception handling

const std = @import("std");
const ffi = @import("ffi.zig");

// =============================================================================
// String Operations
// =============================================================================

/// Concatenate two strings, returning a new allocated string
/// Caller is responsible for freeing the result
export fn cot_rt_str_concat(
    a_ptr: [*]const u8,
    a_len: u32,
    b_ptr: [*]const u8,
    b_len: u32,
    out_ptr: *[*]u8,
    out_len: *u32,
) i32 {
    const allocator = std.heap.c_allocator;

    const total_len = a_len + b_len;
    const result = allocator.alloc(u8, total_len) catch return -1;

    @memcpy(result[0..a_len], a_ptr[0..a_len]);
    @memcpy(result[a_len..total_len], b_ptr[0..b_len]);

    out_ptr.* = result.ptr;
    out_len.* = total_len;
    return 0;
}

/// Compare two strings
/// Returns: -1 if a < b, 0 if a == b, 1 if a > b
export fn cot_rt_str_compare(
    a_ptr: [*]const u8,
    a_len: u32,
    b_ptr: [*]const u8,
    b_len: u32,
) i32 {
    const a = a_ptr[0..a_len];
    const b = b_ptr[0..b_len];

    const order = std.mem.order(u8, a, b);
    return switch (order) {
        .lt => -1,
        .eq => 0,
        .gt => 1,
    };
}

/// Get string length
export fn cot_rt_str_len(ptr: [*]const u8, len: u32) u32 {
    _ = ptr;
    return len;
}

/// Copy a string
export fn cot_rt_str_copy(
    src_ptr: [*]const u8,
    src_len: u32,
    out_ptr: *[*]u8,
    out_len: *u32,
) i32 {
    const allocator = std.heap.c_allocator;

    const result = allocator.alloc(u8, src_len) catch return -1;
    @memcpy(result, src_ptr[0..src_len]);

    out_ptr.* = result.ptr;
    out_len.* = src_len;
    return 0;
}

/// Extract a substring (slice)
export fn cot_rt_str_slice(
    src_ptr: [*]const u8,
    src_len: u32,
    start: i64,
    end: i64,
    out_ptr: *[*]u8,
    out_len: *u32,
) i32 {
    const allocator = std.heap.c_allocator;

    // Convert to 0-based indices with bounds checking
    const s: usize = if (start < 0) 0 else @min(@as(usize, @intCast(start)), src_len);
    const e: usize = if (end < 0) src_len else @min(@as(usize, @intCast(end)), src_len);

    if (s >= e) {
        out_ptr.* = @ptrFromInt(1); // Non-null empty string
        out_len.* = 0;
        return 0;
    }

    const slice_len: u32 = @intCast(e - s);
    const result = allocator.alloc(u8, slice_len) catch return -1;
    @memcpy(result, src_ptr[s..e]);

    out_ptr.* = result.ptr;
    out_len.* = slice_len;
    return 0;
}

// =============================================================================
// Decimal Operations
// =============================================================================

/// Decimal representation: value is stored as i64, scale determines decimal point
/// Example: 12345 with scale=2 represents 123.45

/// Add two decimals with scale alignment
export fn cot_rt_dec_add(a: i64, b: i64, scale_a: u8, scale_b: u8, scale_out: u8) i64 {
    // Align scales
    const a_aligned = alignScale(a, scale_a, scale_out);
    const b_aligned = alignScale(b, scale_b, scale_out);
    return a_aligned + b_aligned;
}

/// Subtract two decimals with scale alignment
export fn cot_rt_dec_sub(a: i64, b: i64, scale_a: u8, scale_b: u8, scale_out: u8) i64 {
    const a_aligned = alignScale(a, scale_a, scale_out);
    const b_aligned = alignScale(b, scale_b, scale_out);
    return a_aligned - b_aligned;
}

/// Multiply two decimals
export fn cot_rt_dec_mul(a: i64, b: i64, scale_a: u8, scale_b: u8, scale_out: u8) i64 {
    // Product has scale = scale_a + scale_b, need to adjust to scale_out
    const product = a * b;
    const product_scale = scale_a + scale_b;

    if (product_scale == scale_out) {
        return product;
    } else if (product_scale > scale_out) {
        // Need to reduce scale (divide)
        const diff = product_scale - scale_out;
        const divisor = pow10(diff);
        return @divTrunc(product + @divTrunc(divisor, 2), divisor); // Round half up
    } else {
        // Need to increase scale (multiply)
        const diff = scale_out - product_scale;
        return product * pow10(diff);
    }
}

/// Divide two decimals
export fn cot_rt_dec_div(a: i64, b: i64, scale_a: u8, scale_b: u8, scale_out: u8) i64 {
    if (b == 0) return 0; // Division by zero returns 0 (or could trap)

    // Scale up dividend to maintain precision
    const extra_scale: u8 = scale_out + scale_b;
    const scaled_a = a * pow10(extra_scale);
    const result = @divTrunc(scaled_a, b);

    // Adjust from scale_a + extra_scale to scale_out
    const current_scale = scale_a + extra_scale;
    if (current_scale > scale_out) {
        const diff = current_scale - scale_out;
        return @divTrunc(result, pow10(diff));
    }
    return result;
}

/// Compare two decimals
export fn cot_rt_dec_compare(a: i64, b: i64, scale_a: u8, scale_b: u8) i32 {
    const max_scale = @max(scale_a, scale_b);
    const a_aligned = alignScale(a, scale_a, max_scale);
    const b_aligned = alignScale(b, scale_b, max_scale);

    if (a_aligned < b_aligned) return -1;
    if (a_aligned > b_aligned) return 1;
    return 0;
}

fn alignScale(value: i64, from_scale: u8, to_scale: u8) i64 {
    if (from_scale == to_scale) return value;
    if (from_scale < to_scale) {
        return value * pow10(to_scale - from_scale);
    } else {
        return @divTrunc(value, pow10(from_scale - to_scale));
    }
}

fn pow10(exp: u8) i64 {
    var result: i64 = 1;
    var i: u8 = 0;
    while (i < exp) : (i += 1) {
        result *= 10;
    }
    return result;
}

// =============================================================================
// Memory Management
// =============================================================================

/// Allocate memory
export fn cot_rt_mem_alloc(size: usize) ?[*]u8 {
    const allocator = std.heap.c_allocator;
    const mem = allocator.alloc(u8, size) catch return null;
    return mem.ptr;
}

/// Free memory
export fn cot_rt_mem_free(ptr: [*]u8, size: usize) void {
    const allocator = std.heap.c_allocator;
    allocator.free(ptr[0..size]);
}

/// Reallocate memory
export fn cot_rt_mem_realloc(ptr: ?[*]u8, old_size: usize, new_size: usize) ?[*]u8 {
    const allocator = std.heap.c_allocator;

    if (ptr) |p| {
        const old_slice = p[0..old_size];
        if (allocator.resize(old_slice, new_size)) |new_slice| {
            _ = new_slice;
            return p;
        }
        // Resize failed, allocate new and copy
        const new_mem = allocator.alloc(u8, new_size) catch return null;
        const copy_len = @min(old_size, new_size);
        @memcpy(new_mem[0..copy_len], p[0..copy_len]);
        allocator.free(old_slice);
        return new_mem.ptr;
    } else {
        // ptr is null, just allocate
        const mem = allocator.alloc(u8, new_size) catch return null;
        return mem.ptr;
    }
}

// =============================================================================
// Array Operations
// =============================================================================

/// Check array bounds, trap if out of bounds
export fn cot_rt_array_bounds_check(index: i64, length: u64) void {
    if (index < 0 or @as(u64, @intCast(index)) >= length) {
        @panic("array index out of bounds");
    }
}

// =============================================================================
// Error/Exception Handling
// =============================================================================

/// Thread-local error state
threadlocal var current_error: ?CotError = null;

const CotError = struct {
    code: u32,
    message_ptr: [*]const u8,
    message_len: u32,
};

/// Throw an error (sets thread-local error state)
export fn cot_rt_err_throw(code: u32, message_ptr: [*]const u8, message_len: u32) void {
    current_error = .{
        .code = code,
        .message_ptr = message_ptr,
        .message_len = message_len,
    };
}

/// Check if there's a pending error
export fn cot_rt_err_pending() bool {
    return current_error != null;
}

/// Get error code (0 if no error)
export fn cot_rt_err_code() u32 {
    if (current_error) |e| return e.code;
    return 0;
}

/// Get error message
export fn cot_rt_err_message(out_ptr: *[*]const u8, out_len: *u32) void {
    if (current_error) |e| {
        out_ptr.* = e.message_ptr;
        out_len.* = e.message_len;
    } else {
        out_ptr.* = "";
        out_len.* = 0;
    }
}

/// Clear error state
export fn cot_rt_err_clear() void {
    current_error = null;
}

// =============================================================================
// I/O Operations (stubs - will integrate with channels.zig)
// =============================================================================

/// Open a file/channel
export fn cot_rt_io_open(
    channel: u32,
    filename_ptr: [*]const u8,
    filename_len: u32,
    mode: u8,
) i32 {
    // TODO: Integrate with ChannelManager
    _ = channel;
    _ = filename_ptr;
    _ = filename_len;
    _ = mode;
    return -1; // Not implemented
}

/// Close a channel
export fn cot_rt_io_close(channel: u32) i32 {
    _ = channel;
    return -1; // Not implemented
}

/// Read from a channel
export fn cot_rt_io_read(
    channel: u32,
    buffer_ptr: [*]u8,
    buffer_len: u32,
) i32 {
    _ = channel;
    _ = buffer_ptr;
    _ = buffer_len;
    return -1; // Not implemented
}

/// Write to a channel
export fn cot_rt_io_write(
    channel: u32,
    buffer_ptr: [*]const u8,
    buffer_len: u32,
) i32 {
    _ = channel;
    _ = buffer_ptr;
    _ = buffer_len;
    return -1; // Not implemented
}

// =============================================================================
// Debug/Diagnostic
// =============================================================================

/// Print debug message to stderr
export fn cot_rt_debug_print(ptr: [*]const u8, len: u32) void {
    const stderr = std.io.getStdErr().writer();
    stderr.writeAll(ptr[0..len]) catch {};
    stderr.writeByte('\n') catch {};
}

// =============================================================================
// Registration
// =============================================================================

/// Runtime function entry for registration
pub const RuntimeFn = struct {
    name: [:0]const u8,
    ptr: *const anyopaque,
};

/// List of all runtime functions to register with JIT
pub const runtime_functions = [_]RuntimeFn{
    // String
    .{ .name = "cot_rt_str_concat", .ptr = @ptrCast(&cot_rt_str_concat) },
    .{ .name = "cot_rt_str_compare", .ptr = @ptrCast(&cot_rt_str_compare) },
    .{ .name = "cot_rt_str_len", .ptr = @ptrCast(&cot_rt_str_len) },
    .{ .name = "cot_rt_str_copy", .ptr = @ptrCast(&cot_rt_str_copy) },
    .{ .name = "cot_rt_str_slice", .ptr = @ptrCast(&cot_rt_str_slice) },

    // Decimal
    .{ .name = "cot_rt_dec_add", .ptr = @ptrCast(&cot_rt_dec_add) },
    .{ .name = "cot_rt_dec_sub", .ptr = @ptrCast(&cot_rt_dec_sub) },
    .{ .name = "cot_rt_dec_mul", .ptr = @ptrCast(&cot_rt_dec_mul) },
    .{ .name = "cot_rt_dec_div", .ptr = @ptrCast(&cot_rt_dec_div) },
    .{ .name = "cot_rt_dec_compare", .ptr = @ptrCast(&cot_rt_dec_compare) },

    // Memory
    .{ .name = "cot_rt_mem_alloc", .ptr = @ptrCast(&cot_rt_mem_alloc) },
    .{ .name = "cot_rt_mem_free", .ptr = @ptrCast(&cot_rt_mem_free) },
    .{ .name = "cot_rt_mem_realloc", .ptr = @ptrCast(&cot_rt_mem_realloc) },

    // Array
    .{ .name = "cot_rt_array_bounds_check", .ptr = @ptrCast(&cot_rt_array_bounds_check) },

    // Error
    .{ .name = "cot_rt_err_throw", .ptr = @ptrCast(&cot_rt_err_throw) },
    .{ .name = "cot_rt_err_pending", .ptr = @ptrCast(&cot_rt_err_pending) },
    .{ .name = "cot_rt_err_code", .ptr = @ptrCast(&cot_rt_err_code) },
    .{ .name = "cot_rt_err_message", .ptr = @ptrCast(&cot_rt_err_message) },
    .{ .name = "cot_rt_err_clear", .ptr = @ptrCast(&cot_rt_err_clear) },

    // I/O
    .{ .name = "cot_rt_io_open", .ptr = @ptrCast(&cot_rt_io_open) },
    .{ .name = "cot_rt_io_close", .ptr = @ptrCast(&cot_rt_io_close) },
    .{ .name = "cot_rt_io_read", .ptr = @ptrCast(&cot_rt_io_read) },
    .{ .name = "cot_rt_io_write", .ptr = @ptrCast(&cot_rt_io_write) },

    // Debug
    .{ .name = "cot_rt_debug_print", .ptr = @ptrCast(&cot_rt_debug_print) },
};

/// Register all runtime functions with the JIT context
pub fn registerAll(ctx: *ffi.JitContext) void {
    for (runtime_functions) |func| {
        ffi.registerRuntimeFn(ctx, func.name, func.ptr);
    }
}

// =============================================================================
// Tests
// =============================================================================

test "string concatenation" {
    const a = "Hello, ";
    const b = "World!";
    var out_ptr: [*]u8 = undefined;
    var out_len: u32 = undefined;

    const result = cot_rt_str_concat(a.ptr, a.len, b.ptr, b.len, &out_ptr, &out_len);
    try std.testing.expectEqual(@as(i32, 0), result);
    try std.testing.expectEqual(@as(u32, 13), out_len);
    try std.testing.expectEqualStrings("Hello, World!", out_ptr[0..out_len]);

    // Free the result
    std.heap.c_allocator.free(out_ptr[0..out_len]);
}

test "decimal addition" {
    // 123.45 + 67.89 = 191.34
    const a: i64 = 12345; // scale 2
    const b: i64 = 6789; // scale 2
    const result = cot_rt_dec_add(a, b, 2, 2, 2);
    try std.testing.expectEqual(@as(i64, 19134), result);
}

test "decimal multiplication" {
    // 12.34 * 5.6 = 69.104 -> rounded to 69.10 at scale 2
    const a: i64 = 1234; // scale 2
    const b: i64 = 56; // scale 1
    const result = cot_rt_dec_mul(a, b, 2, 1, 2);
    try std.testing.expectEqual(@as(i64, 6910), result);
}

test "string comparison" {
    try std.testing.expectEqual(@as(i32, 0), cot_rt_str_compare("abc", 3, "abc", 3));
    try std.testing.expectEqual(@as(i32, -1), cot_rt_str_compare("abc", 3, "abd", 3));
    try std.testing.expectEqual(@as(i32, 1), cot_rt_str_compare("abd", 3, "abc", 3));
}
