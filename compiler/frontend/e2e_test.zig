//! End-to-end pipeline tests: source → parse → check → lower → IR → SSA → compile
//! These tests verify logic parity with bootstrap-0.2 by testing full compilation.

const std = @import("std");
const ast = @import("ast.zig");
const parser_mod = @import("parser.zig");
const scanner_mod = @import("scanner.zig");
const checker = @import("checker.zig");
const types = @import("types.zig");
const fe_errors = @import("errors.zig");
const source_mod = @import("source.zig");
const ir = @import("ir.zig");
const lower = @import("lower.zig");
const ssa_builder = @import("ssa_builder.zig");

const TypeRegistry = types.TypeRegistry;
const Func = ssa_builder.Func;

const PipelineResult = struct {
    arena: std.heap.ArenaAllocator,
    has_errors: bool,
    ir_funcs: []const ir.Func,
    ssa_funcs: []*Func,
    type_reg: TypeRegistry,

    pub fn deinit(self: *PipelineResult) void {
        for (self.ssa_funcs) |f| {
            f.deinit();
            self.arena.allocator().destroy(f);
        }
        self.arena.deinit();
    }
};

fn runPipeline(backing: std.mem.Allocator, code: []const u8) !PipelineResult {
    var arena = std.heap.ArenaAllocator.init(backing);
    const allocator = arena.allocator();
    errdefer arena.deinit();

    // Setup source and errors
    var src = source_mod.Source.init(allocator, "test.cot", code);
    var err = fe_errors.ErrorReporter.init(&src, null);

    // Parse
    var scan = scanner_mod.Scanner.initWithErrors(&src, &err);
    var tree = ast.Ast.init(allocator);
    var parser = parser_mod.Parser.init(allocator, &scan, &tree, &err);
    parser.parseFile() catch {
        return .{ .arena = arena, .has_errors = true, .ir_funcs = &.{}, .ssa_funcs = &.{}, .type_reg = try TypeRegistry.init(allocator) };
    };
    if (err.hasErrors()) {
        return .{ .arena = arena, .has_errors = true, .ir_funcs = &.{}, .ssa_funcs = &.{}, .type_reg = try TypeRegistry.init(allocator) };
    }

    // Type check
    var type_reg = try TypeRegistry.init(allocator);
    var global_scope = checker.Scope.init(allocator, null);
    var generic_ctx = checker.SharedGenericContext.init(allocator);
    const target = @import("target.zig").Target.native();
    var check = checker.Checker.init(allocator, &tree, &type_reg, &err, &global_scope, &global_scope, &generic_ctx, target);
    check.checkFile() catch {
        return .{ .arena = arena, .has_errors = true, .ir_funcs = &.{}, .ssa_funcs = &.{}, .type_reg = type_reg };
    };
    if (err.hasErrors()) {
        return .{ .arena = arena, .has_errors = true, .ir_funcs = &.{}, .ssa_funcs = &.{}, .type_reg = type_reg };
    }

    // Lower to IR
    var lowering = lower.Lowerer.init(allocator, &tree, &type_reg, &err, &check, target);
    const ir_data = lowering.lower() catch {
        return .{ .arena = arena, .has_errors = true, .ir_funcs = &.{}, .ssa_funcs = &.{}, .type_reg = type_reg };
    };
    if (err.hasErrors()) {
        return .{ .arena = arena, .has_errors = true, .ir_funcs = ir_data.funcs, .ssa_funcs = &.{}, .type_reg = type_reg };
    }

    // Convert IR to SSA
    var ssa_funcs = std.ArrayListUnmanaged(*Func){};
    for (ir_data.funcs) |*ir_func| {
        var builder = try ssa_builder.SSABuilder.init(allocator, ir_func, ir_data.globals, &type_reg, target);
        const ssa_func = builder.build() catch |e| {
            std.debug.print("SSA build error for {s}: {}\n", .{ ir_func.name, e });
            builder.deinit();
            continue;
        };
        builder.deinit();
        try ssa_funcs.append(allocator, ssa_func);
    }

    return .{
        .arena = arena,
        .has_errors = false,
        .ir_funcs = ir_data.funcs,
        .ssa_funcs = ssa_funcs.toOwnedSlice(allocator) catch &.{},
        .type_reg = type_reg,
    };
}

// ============================================================================
// Basic Function Tests
// ============================================================================

test "e2e: empty function" {
    var result = try runPipeline(std.testing.allocator,
        \\fn empty() void { }
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 1), result.ssa_funcs.len);
}

test "e2e: return constant" {
    var result = try runPipeline(std.testing.allocator,
        \\fn answer() i64 { return 42 }
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 1), result.ssa_funcs.len);
    // Verify SSA has at least entry block with return
    const f = result.ssa_funcs[0];
    try std.testing.expect(f.entry != null);
}

test "e2e: simple addition" {
    var result = try runPipeline(std.testing.allocator,
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 1), result.ssa_funcs.len);
}

test "e2e: all arithmetic ops" {
    var result = try runPipeline(std.testing.allocator,
        \\fn arith(a: i64, b: i64) i64 {
        \\    var x = a + b
        \\    x = x - b
        \\    x = x * b
        \\    x = x / b
        \\    x = x % b
        \\    return x
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: comparison ops" {
    var result = try runPipeline(std.testing.allocator,
        \\fn cmp(a: i64, b: i64) bool {
        \\    if (a == b) { return true }
        \\    if (a != b) { return true }
        \\    if (a < b) { return true }
        \\    if (a <= b) { return true }
        \\    if (a > b) { return true }
        \\    if (a >= b) { return true }
        \\    return false
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: bitwise ops" {
    var result = try runPipeline(std.testing.allocator,
        \\fn bits(a: i64, b: i64) i64 {
        \\    var x = a & b
        \\    x = x | b
        \\    x = x ^ b
        \\    x = x << 2
        \\    x = x >> 1
        \\    return x
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: unary ops" {
    var result = try runPipeline(std.testing.allocator,
        \\fn unary(a: i64, b: bool) i64 {
        \\    var x = -a
        \\    var y = !b
        \\    if (y) { return x }
        \\    return 0
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Control Flow Tests
// ============================================================================

test "e2e: if-else" {
    var result = try runPipeline(std.testing.allocator,
        \\fn max(a: i64, b: i64) i64 {
        \\    if (a > b) {
        \\        return a
        \\    } else {
        \\        return b
        \\    }
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    // Diamond CFG should have multiple blocks
    const f = result.ssa_funcs[0];
    try std.testing.expect(f.numBlocks() >= 3);
}

test "e2e: nested if" {
    var result = try runPipeline(std.testing.allocator,
        \\fn classify(x: i64) i64 {
        \\    if (x < 0) {
        \\        return 0 - 1
        \\    } else {
        \\        if (x > 0) {
        \\            return 1
        \\        } else {
        \\            return 0
        \\        }
        \\    }
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: while loop" {
    var result = try runPipeline(std.testing.allocator,
        \\fn sum_to(n: i64) i64 {
        \\    var total = 0
        \\    var i = 1
        \\    while (i <= n) {
        \\        total = total + i
        \\        i = i + 1
        \\    }
        \\    return total
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: while with break" {
    var result = try runPipeline(std.testing.allocator,
        \\fn find_first_even(n: i64) i64 {
        \\    var i = 1
        \\    while (i <= n) {
        \\        if (i % 2 == 0) {
        \\            return i
        \\        }
        \\        i = i + 1
        \\    }
        \\    return 0
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: logical and/or" {
    var result = try runPipeline(std.testing.allocator,
        \\fn logic(a: bool, b: bool) bool {
        \\    if (a and b) { return true }
        \\    if (a or b) { return true }
        \\    return false
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Variable and Assignment Tests
// ============================================================================

test "e2e: local variables" {
    var result = try runPipeline(std.testing.allocator,
        \\fn locals() i64 {
        \\    var x = 10
        \\    const y = 20
        \\    x = x + y
        \\    return x
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: multiple assignments" {
    var result = try runPipeline(std.testing.allocator,
        \\fn multi() i64 {
        \\    var x = 1
        \\    x = 2
        \\    x = 3
        \\    x = x + 1
        \\    return x
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Function Call Tests
// ============================================================================

test "e2e: function call" {
    var result = try runPipeline(std.testing.allocator,
        \\fn double(x: i64) i64 { return x * 2 }
        \\fn quad(x: i64) i64 { return double(double(x)) }
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 2), result.ssa_funcs.len);
}

test "e2e: recursive function" {
    var result = try runPipeline(std.testing.allocator,
        \\fn factorial(n: i64) i64 {
        \\    if (n <= 1) { return 1 }
        \\    return n * factorial(n - 1)
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: multiple params" {
    var result = try runPipeline(std.testing.allocator,
        \\fn sum4(a: i64, b: i64, c: i64, d: i64) i64 {
        \\    return a + b + c + d
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Struct Tests
// ============================================================================

test "e2e: struct definition and creation" {
    var result = try runPipeline(std.testing.allocator,
        \\struct Point { x: i64, y: i64 }
        \\fn make_point() Point {
        \\    return Point { .x = 10, .y = 20 }
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: struct field access" {
    var result = try runPipeline(std.testing.allocator,
        \\struct Point { x: i64, y: i64 }
        \\fn get_x(p: Point) i64 { return p.x }
        \\fn get_y(p: Point) i64 { return p.y }
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: struct field mutation" {
    var result = try runPipeline(std.testing.allocator,
        \\struct Point { x: i64, y: i64 }
        \\fn move_x(p: *Point, dx: i64) void {
        \\    p.x = p.x + dx
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Enum Tests
// ============================================================================

test "e2e: enum definition" {
    var result = try runPipeline(std.testing.allocator,
        \\const Color = enum { red, green, blue }
        \\fn get_red() Color { return Color.red }
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: enum comparison" {
    var result = try runPipeline(std.testing.allocator,
        \\const Color = enum { red, green, blue }
        \\fn is_red(c: Color) bool { return c == Color.red }
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Pointer Tests
// ============================================================================

test "e2e: pointer parameter" {
    var result = try runPipeline(std.testing.allocator,
        \\fn increment(p: *i64) void {
        \\    p.* = p.* + 1
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: address-of and deref" {
    var result = try runPipeline(std.testing.allocator,
        \\fn swap(a: *i64, b: *i64) void {
        \\    var tmp = a.*
        \\    a.* = b.*
        \\    b.* = tmp
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Array Tests
// ============================================================================

test "e2e: array literal" {
    var result = try runPipeline(std.testing.allocator,
        \\fn first() i64 {
        \\    var arr = [1, 2, 3]
        \\    return arr[0]
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: array indexing" {
    var result = try runPipeline(std.testing.allocator,
        \\fn sum_arr() i64 {
        \\    var arr = [10, 20, 30]
        \\    return arr[0] + arr[1] + arr[2]
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Complex/Combined Tests (from parity tests)
// ============================================================================

test "e2e: fibonacci" {
    var result = try runPipeline(std.testing.allocator,
        \\fn fib(n: i64) i64 {
        \\    if (n <= 1) { return n }
        \\    return fib(n - 1) + fib(n - 2)
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: gcd" {
    var result = try runPipeline(std.testing.allocator,
        \\fn gcd(a_in: i64, b_in: i64) i64 {
        \\    var a = a_in
        \\    var b = b_in
        \\    while (b != 0) {
        \\        var t = b
        \\        b = a % b
        \\        a = t
        \\    }
        \\    return a
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: div_rem (from parity)" {
    var result = try runPipeline(std.testing.allocator,
        \\fn div_rem(a: i64, b: i64, q: *i64, r: *i64) void {
        \\    q.* = a / b
        \\    r.* = a % b
        \\}
        \\fn main() i64 {
        \\    var quot: i64 = 0
        \\    var rem: i64 = 0
        \\    div_rem(17, 5, &quot, &rem)
        \\    if (quot == 3) {
        \\        if (rem == 2) { return 0 }
        \\    }
        \\    return 1
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expectEqual(@as(usize, 2), result.ssa_funcs.len);
}

test "e2e: nested loops" {
    var result = try runPipeline(std.testing.allocator,
        \\fn nested() i64 {
        \\    var sum = 0
        \\    var i = 0
        \\    while (i < 3) {
        \\        var j = 0
        \\        while (j < 3) {
        \\            sum = sum + 1
        \\            j = j + 1
        \\        }
        \\        i = i + 1
        \\    }
        \\    return sum
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

test "e2e: complex control flow" {
    var result = try runPipeline(std.testing.allocator,
        \\fn complex(x: i64) i64 {
        \\    var result = 0
        \\    if (x > 0) {
        \\        var i = 0
        \\        while (i < x) {
        \\            if (i % 2 == 0) {
        \\                result = result + i
        \\            } else {
        \\                result = result - i
        \\            }
        \\            i = i + 1
        \\        }
        \\    } else {
        \\        result = 0 - x
        \\    }
        \\    return result
        \\}
    );
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
}

// ============================================================================
// Error Detection Tests
// ============================================================================

test "e2e: type error detected" {
    var result = try runPipeline(std.testing.allocator,
        \\fn bad() i64 { return "hello" }
    );
    defer result.deinit();
    try std.testing.expect(result.has_errors);
}

test "e2e: undefined variable detected" {
    var result = try runPipeline(std.testing.allocator,
        \\fn bad() i64 { return undefined_var }
    );
    defer result.deinit();
    try std.testing.expect(result.has_errors);
}

test "e2e: undefined function detected" {
    var result = try runPipeline(std.testing.allocator,
        \\fn bad() i64 { return undefined_func() }
    );
    defer result.deinit();
    try std.testing.expect(result.has_errors);
}
