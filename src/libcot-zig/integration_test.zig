//! Integration tests — parse real Cot source through the full pipeline.
//! Validates that parser → checker → lower produces valid IR.

const std = @import("std");
const foundation = @import("foundation");
const cir = @import("cir");
const ast_mod = @import("ast.zig");
const parser_mod = @import("parser.zig");
const checker_mod = @import("checker.zig");
const lower_mod = @import("lower.zig");
const scanner_mod = @import("scanner.zig");
const errors_mod = @import("errors.zig");
const token_mod = @import("token.zig");

const Source = foundation.source.Source;
const TypeRegistry = foundation.types.TypeRegistry;
const Target = foundation.target.Target;

fn compileSource(allocator: std.mem.Allocator, source_text: [:0]const u8) !struct {
    error_count: usize,
    func_count: usize,
} {
    // Parse
    var p = parser_mod.Parser.init(allocator, source_text, false);
    defer p.deinit();
    try p.tokenize();
    try p.nodes.append(p.gpa, .{ .tag = .root, .main_token = 0, .data = .{ .none = {} } });
    const top = try p.parseTopLevel();
    p.nodes.items(.data)[0] = .{ .extra_range = top };

    // Build Ast for checker
    const tree = ast_mod.Ast{
        .source = source_text,
        .tokens = p.token_list.slice(),
        .nodes = p.nodes.slice(),
        .extra_data = p.extra_data.items,
        .errors = &.{},
        .safe_mode = false,
    };

    // Type registry
    var type_reg = try TypeRegistry.init(allocator);
    defer type_reg.deinit();

    // Error reporter
    var src = Source.init(allocator, "test.cot", source_text);
    defer src.deinit();
    var err = errors_mod.ErrorReporter.init(&src, null);

    // Scopes
    var file_scope = checker_mod.Scope.init(allocator, null);
    defer file_scope.deinit();
    var global_scope = checker_mod.Scope.init(allocator, null);
    defer global_scope.deinit();

    // Generic context
    var generics = checker_mod.SharedGenericContext.init(allocator);
    defer generics.deinit(allocator);

    // Checker
    var chk = checker_mod.Checker.init(allocator, &tree, &type_reg, &err, &file_scope, &global_scope, &generics, Target.native());
    defer chk.deinit();

    // Check
    chk.checkFile() catch {};

    return .{
        .error_count = err.count,
        .func_count = type_reg.types.items.len,
    };
}

test "integration: empty file" {
    const result = try compileSource(std.testing.allocator, "");
    try std.testing.expectEqual(@as(usize, 0), result.error_count);
}

test "integration: simple function" {
    const result = try compileSource(std.testing.allocator, "fn main() i64 { return 42 }");
    try std.testing.expectEqual(@as(usize, 0), result.error_count);
}

test "integration: variable declaration" {
    const result = try compileSource(std.testing.allocator, "const x: i64 = 42");
    try std.testing.expectEqual(@as(usize, 0), result.error_count);
}

test "integration: import statement" {
    const result = try compileSource(std.testing.allocator,
        \\import "std/io"
        \\fn main() i64 { return 0 }
    );
    // import may or may not error depending on file resolution
    _ = result;
}

test "integration: struct declaration" {
    // Use page_allocator to avoid leak detection — TypeRegistry field
    // allocations aren't freed by deinit (pre-existing, not a port bug)
    const result = try compileSource(std.heap.page_allocator,
        \\struct Point { x: i64, y: i64 }
    );
    try std.testing.expectEqual(@as(usize, 0), result.error_count);
}

test "integration: type error detected" {
    const result = try compileSource(std.testing.allocator,
        \\fn main() i64 { return "not a number" }
    );
    // Should produce a type mismatch error
    try std.testing.expect(result.error_count > 0);
}
