//! Compile-time builtin functions
//!
//! Zig-style: @defined(), @os(), @arch(), @sizeOf(), @alignOf(), @TypeOf(), @date(), @time(), @version(), @file(), @line()

const std = @import("std");
const Value = @import("value.zig").Value;
const ast = @import("../ast/mod.zig");
const ExprIdx = ast.ExprIdx;
const StringId = ast.StringId;
const StringInterner = ast.StringInterner;
const builtin = @import("builtin");

pub const BuiltinError = error{
    UnknownBuiltin,
    InvalidArgument,
    MissingArgument,
};

/// Context for evaluating builtins
pub const BuiltinContext = struct {
    /// Map of defined constants (using StringId keys)
    constants: *const std.AutoHashMap(StringId, Value),
    /// Reference to string interner
    strings: *const StringInterner,
    /// Current source file (for @file())
    source_file: ?[]const u8 = null,
    /// Current line number (for @line())
    line: u32 = 0,
};

/// Evaluate a compile-time builtin
pub fn evaluate(
    name: []const u8,
    args: []const ExprIdx,
    ctx: *const BuiltinContext,
    evaluator: anytype,
) BuiltinError!Value {
    // @defined(NAME) - check if a constant is defined
    if (std.mem.eql(u8, name, "defined")) {
        if (args.len < 1) return BuiltinError.MissingArgument;
        const arg = args[0];

        // The argument should be an identifier - we need to get its name
        const expr_tag = evaluator.store.exprTag(arg);
        if (expr_tag != .identifier) return BuiltinError.InvalidArgument;

        const view = ast.views.IdentifierView.from(evaluator.store, arg);
        const is_defined = evaluator.isDefinedByName(ctx.strings.get(view.name));
        return Value.fromBool(is_defined);
    }

    // @os() - current operating system
    if (std.mem.eql(u8, name, "os")) {
        const os_name = switch (builtin.os.tag) {
            .macos => "macos",
            .linux => "linux",
            .windows => "windows",
            .freebsd => "freebsd",
            .openbsd => "openbsd",
            .netbsd => "netbsd",
            .freestanding => "freestanding",
            else => "unknown",
        };
        return Value.fromString(os_name);
    }

    // @arch() - current CPU architecture
    if (std.mem.eql(u8, name, "arch")) {
        const arch_name = switch (builtin.cpu.arch) {
            .x86_64 => "x86_64",
            .aarch64 => "arm64",
            .x86 => "x86",
            .arm => "arm",
            .wasm32 => "wasm32",
            .wasm64 => "wasm64",
            .riscv64 => "riscv64",
            else => "unknown",
        };
        return Value.fromString(arch_name);
    }

    // @sizeOf(TYPE) - size of a type in bytes (Zig-style)
    if (std.mem.eql(u8, name, "sizeOf")) {
        if (args.len < 1) return BuiltinError.MissingArgument;
        // For now, just return a placeholder. Real implementation would
        // need type information from the semantic analysis phase.
        return Value.fromInt(0);
    }

    // @alignOf(TYPE) - alignment of a type in bytes (Zig-style)
    if (std.mem.eql(u8, name, "alignOf")) {
        if (args.len < 1) return BuiltinError.MissingArgument;
        // For now, just return a placeholder. Real implementation would
        // need type information from the semantic analysis phase.
        return Value.fromInt(0);
    }

    // @TypeOf(x) - type of an expression (Zig-style)
    if (std.mem.eql(u8, name, "TypeOf")) {
        if (args.len < 1) return BuiltinError.MissingArgument;
        // For now, just return a placeholder. Real implementation would
        // need type information from the semantic analysis phase.
        return Value.fromString("unknown");
    }

    // @date() - compile date as "YYYY-MM-DD"
    if (std.mem.eql(u8, name, "date")) {
        // Use build timestamp
        // For now, return a placeholder
        return Value.fromString("2024-01-01");
    }

    // @time() - compile time as "HH:MM:SS"
    if (std.mem.eql(u8, name, "time")) {
        return Value.fromString("00:00:00");
    }

    // @version() - cot compiler version
    if (std.mem.eql(u8, name, "version")) {
        return Value.fromString("0.1.0");
    }

    // @file() - current source file path
    if (std.mem.eql(u8, name, "file")) {
        return Value.fromString(ctx.source_file orelse "unknown");
    }

    // @line() - current line number
    if (std.mem.eql(u8, name, "line")) {
        return Value.fromInt(@intCast(ctx.line));
    }

    return BuiltinError.UnknownBuiltin;
}

test "builtin @os" {
    const allocator = std.testing.allocator;

    var strings = ast.StringInterner.init(allocator);
    defer strings.deinit();
    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var constants = std.AutoHashMap(StringId, Value).init(allocator);
    defer constants.deinit();

    const ctx = BuiltinContext{
        .constants = &constants,
        .strings = &strings,
    };

    // Create a mock evaluator that provides the store
    const MockEvaluator = struct {
        store: *const ast.NodeStore,

        fn isDefinedByName(_: @This(), _: []const u8) bool {
            return false;
        }
    };
    const mock_eval = MockEvaluator{ .store = &store };

    const result = try evaluate("os", &[_]ExprIdx{}, &ctx, mock_eval);
    // Just check it returns a non-empty string
    try std.testing.expect(result.string.len > 0);
}

test "builtin @arch" {
    const allocator = std.testing.allocator;

    var strings = ast.StringInterner.init(allocator);
    defer strings.deinit();
    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var constants = std.AutoHashMap(StringId, Value).init(allocator);
    defer constants.deinit();

    const ctx = BuiltinContext{
        .constants = &constants,
        .strings = &strings,
    };

    const MockEvaluator = struct {
        store: *const ast.NodeStore,

        fn isDefinedByName(_: @This(), _: []const u8) bool {
            return false;
        }
    };
    const mock_eval = MockEvaluator{ .store = &store };

    const result = try evaluate("arch", &[_]ExprIdx{}, &ctx, mock_eval);
    try std.testing.expect(result.string.len > 0);
}
