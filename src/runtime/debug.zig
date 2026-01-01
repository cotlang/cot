//! Debug utilities for Cot
//!
//! Debug mode is controlled via environment variables:
//!   COT_DEBUG=1           Enable all debug output
//!   COT_DEBUG_LEXER=1     Enable lexer debug output
//!   COT_DEBUG_PARSER=1    Enable parser debug output
//!   COT_DEBUG_IR=1        Enable IR lowerer debug output
//!   COT_DEBUG_EMIT=1      Enable bytecode emitter debug output
//!   COT_DEBUG_VM=1        Enable VM execution debug output
//!
//! Debug output is written to stderr to not interfere with program output.
//! When debug mode is disabled, these functions are no-ops with minimal overhead.

const std = @import("std");
const builtin = @import("builtin");

/// Debug categories for granular control
pub const Category = enum {
    general,
    lexer,
    parser,
    ir,
    emit,
    vm,
};

// Platform detection
const is_wasm = builtin.cpu.arch == .wasm32;

/// Check if debug mode is enabled for a category.
/// This is called at runtime but the env var check is fast.
/// On WASM, debug is always disabled (no environment variables).
pub fn isEnabled(category: Category) bool {
    // WASM doesn't support environment variables
    if (is_wasm) return false;

    // Check category-specific env var first
    const category_var = switch (category) {
        .general => "COT_DEBUG",
        .lexer => "COT_DEBUG_LEXER",
        .parser => "COT_DEBUG_PARSER",
        .ir => "COT_DEBUG_IR",
        .emit => "COT_DEBUG_EMIT",
        .vm => "COT_DEBUG_VM",
    };

    if (std.posix.getenv(category_var)) |val| {
        return val.len > 0 and val[0] != '0';
    }

    // Fall back to global debug flag (except for general which uses it directly)
    if (category != .general) {
        if (std.posix.getenv("COT_DEBUG")) |val| {
            return val.len > 0 and val[0] != '0';
        }
    }

    return false;
}

/// Print debug message if debug mode is enabled for the category.
/// Format and args are evaluated lazily only if debug is enabled.
pub fn print(category: Category, comptime fmt: []const u8, args: anytype) void {
    if (!isEnabled(category)) return;

    // Use std.debug.print which goes directly to stderr
    switch (category) {
        .general => std.debug.print("[DEBUG] ", .{}),
        .lexer => std.debug.print("[LEXER] ", .{}),
        .parser => std.debug.print("[PARSER] ", .{}),
        .ir => std.debug.print("[IR] ", .{}),
        .emit => std.debug.print("[EMIT] ", .{}),
        .vm => std.debug.print("[VM] ", .{}),
    }

    std.debug.print(fmt ++ "\n", args);
}

/// Print debug message without category prefix
pub fn raw(category: Category, comptime fmt: []const u8, args: anytype) void {
    if (!isEnabled(category)) return;
    std.debug.print(fmt, args);
}

/// Print a section header for debug output
pub fn section(category: Category, title: []const u8) void {
    if (!isEnabled(category)) return;
    std.debug.print("\n=== {s} ===\n", .{title});
}

/// Print a separator line
pub fn separator(category: Category) void {
    if (!isEnabled(category)) return;
    std.debug.print("----------------------------------------\n", .{});
}

/// Dump a byte slice as hex for debugging
pub fn dumpBytes(category: Category, label: []const u8, bytes: []const u8) void {
    if (!isEnabled(category)) return;

    std.debug.print("{s} ({d} bytes): ", .{ label, bytes.len });
    for (bytes) |b| {
        std.debug.print("{x:0>2} ", .{b});
    }
    std.debug.print("\n", .{});
}

/// Dump a value with a label
pub fn dump(category: Category, label: []const u8, value: anytype) void {
    if (!isEnabled(category)) return;
    std.debug.print("{s}: {any}\n", .{ label, value });
}

test "debug disabled by default" {
    // Without env vars set, debug should be disabled
    try std.testing.expect(!isEnabled(.general));
}
