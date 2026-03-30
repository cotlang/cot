//! Integration test for the frontend pipeline.
//! Verifies that parser → checker → types work together correctly.

const std = @import("std");
const ast = @import("ast.zig");
const parser_mod = @import("parser.zig");
const scanner_mod = @import("scanner.zig");
const checker = @import("checker.zig");
const types = @import("types.zig");
const errors = @import("errors.zig");
const source_mod = @import("source.zig");

fn testPipeline(allocator: std.mem.Allocator, code: []const u8) !struct {
    has_errors: bool,
    func_count: usize,
    type_reg: types.TypeRegistry,
} {
    // Setup source
    var src = source_mod.Source.init(allocator, "test.cot", code);

    // Setup error reporter
    var err = errors.ErrorReporter.init(&src, null);

    // Setup scanner
    var scan = scanner_mod.Scanner.initWithErrors(&src, &err);

    // Setup AST
    var tree = ast.Ast.init(allocator);
    errdefer tree.deinit();

    // Parse
    var parser = parser_mod.Parser.init(allocator, &scan, &tree, &err);
    parser.parseFile() catch |e| {
        std.debug.print("Parse error: {}\n", .{e});
        return .{ .has_errors = true, .func_count = 0, .type_reg = try types.TypeRegistry.init(allocator) };
    };

    if (err.hasErrors()) {
        return .{ .has_errors = true, .func_count = 0, .type_reg = try types.TypeRegistry.init(allocator) };
    }

    // Type check
    var type_reg = try types.TypeRegistry.init(allocator);
    var global_scope = checker.Scope.init(allocator, null);
    defer global_scope.deinit();
    var generic_ctx = checker.SharedGenericContext.init(allocator);
    defer generic_ctx.deinit(allocator);

    const target = @import("target.zig").Target.native();
    var check = checker.Checker.init(allocator, &tree, &type_reg, &err, &global_scope, &global_scope, &generic_ctx, target);
    defer check.deinit();

    check.checkFile() catch |e| {
        std.debug.print("Checker error: {}\n", .{e});
        return .{ .has_errors = true, .func_count = 0, .type_reg = type_reg };
    };

    // Count functions
    var func_count: usize = 0;
    if (tree.file) |file| {
        for (file.decls) |decl_idx| {
            if (tree.getNode(decl_idx)) |node| {
                if (node.asDecl()) |decl| {
                    if (decl == .fn_decl) func_count += 1;
                }
            }
        }
    }

    return .{ .has_errors = err.hasErrors(), .func_count = func_count, .type_reg = type_reg };
}

test "integration: simple function" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 1), result.func_count);
}

test "integration: multiple functions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn foo() i64 { return 1 }
        \\fn bar() i64 { return 2 }
        \\fn baz() i64 { return foo() + bar() }
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 3), result.func_count);
}

test "integration: struct definition" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\struct Point {
        \\    x: i64,
        \\    y: i64,
        \\}
        \\
        \\fn makePoint() Point {
        \\    return Point { .x = 10, .y = 20 }
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 1), result.func_count);
    // Verify struct was registered
    try std.testing.expect(result.type_reg.lookupByName("Point") != null);
}

test "integration: variable declarations" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn test_vars() i64 {
        \\    var x = 10
        \\    const y = 20
        \\    x = x + y
        \\    return x
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
}

test "integration: if statement" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn max(a: i64, b: i64) i64 {
        \\    if (a > b) {
        \\        return a
        \\    } else {
        \\        return b
        \\    }
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
}

test "integration: while loop" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn sum_to(n: i64) i64 {
        \\    var total = 0
        \\    var i = 0
        \\    while (i <= n) {
        \\        total = total + i
        \\        i = i + 1
        \\    }
        \\    return total
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
}

test "integration: array and indexing" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn first_element() i64 {
        \\    var arr = [1, 2, 3]
        \\    return arr[0]
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
}

test "integration: type error is detected" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn bad() i64 {
        \\    return "hello"
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    // This should have a type error - returning string from i64 function
    try std.testing.expect(result.has_errors);
}

test "integration: undefined variable is detected" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\fn bad() i64 {
        \\    return undefined_var
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    // This should have an error - undefined variable
    try std.testing.expect(result.has_errors);
}

test "integration: enum definition" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const code =
        \\const Color = enum { red, green, blue }
        \\
        \\fn get_color() Color {
        \\    return Color.red
        \\}
    ;

    var result = try testPipeline(allocator, code);
    defer result.type_reg.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.type_reg.lookupByName("Color") != null);
}
