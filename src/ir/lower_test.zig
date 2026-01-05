//! IR Lowering Tests
//!
//! Comprehensive tests for the AST to IR lowering process.
//! Tests verify that source code is correctly transformed into IR instructions.

const std = @import("std");
const testing = std.testing;

const cot = @import("../root.zig");
const ast = cot.ast;
const ir = @import("ir.zig");
const lower = @import("lower.zig");
const lexer = cot.lexer;
const parser = cot.parser;
const base = cot.base;

// ============================================================================
// Test Helpers
// ============================================================================

/// Parse source code and lower to IR, returning the module
fn lowerSource(allocator: std.mem.Allocator, source: []const u8) !*ir.Module {
    // Tokenize
    var lex = lexer.Lexer.init(source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse
    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parse = parser.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();

    const top_level = try parse.parse();
    defer allocator.free(top_level);

    // Lower to IR
    return lower.lower(allocator, &store, &strings, top_level, "test");
}

/// Check if a function exists in the module
fn hasFunction(module: *const ir.Module, name: []const u8) bool {
    for (module.functions.items) |func| {
        if (std.mem.eql(u8, func.name, name)) {
            return true;
        }
    }
    return false;
}

/// Get a function by name
fn getFunction(module: *const ir.Module, name: []const u8) ?*const ir.Function {
    for (module.functions.items) |func| {
        if (std.mem.eql(u8, func.name, name)) {
            return func;
        }
    }
    return null;
}

/// Count instructions in a function's entry block
fn countInstructions(func: *const ir.Function) usize {
    if (func.blocks.items.len == 0) return 0;
    return func.blocks.items[0].instructions.items.len;
}

/// Check if function has a specific instruction type
fn hasInstruction(func: *const ir.Function, comptime tag: std.meta.Tag(ir.Instruction)) bool {
    for (func.blocks.items) |block| {
        for (block.instructions.items) |inst| {
            if (std.meta.activeTag(inst) == tag) {
                return true;
            }
        }
    }
    return false;
}

// ============================================================================
// Basic Lowering Tests
// ============================================================================

test "lower: empty main function" {
    const allocator = testing.allocator;
    const source = "fn main() {}";

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    try testing.expect(hasFunction(module, "main"));
    try testing.expectEqual(@as(usize, 1), module.functions.items.len);
}

test "lower: function with return type" {
    const allocator = testing.allocator;
    const source =
        \\fn get_value() i64 {
        \\    return 42
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    try testing.expect(hasFunction(module, "get_value"));
    try testing.expect(hasFunction(module, "main"));
    try testing.expectEqual(@as(usize, 2), module.functions.items.len);

    const get_value = getFunction(module, "get_value").?;
    try testing.expectEqual(ir.Type.i64, get_value.signature.return_type);
}

test "lower: function with parameters" {
    const allocator = testing.allocator;
    const source =
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const add_fn = getFunction(module, "add").?;
    try testing.expectEqual(@as(usize, 2), add_fn.signature.params.len);
}

test "lower: multiple functions" {
    const allocator = testing.allocator;
    const source =
        \\fn foo() i64 { return 1 }
        \\fn bar() i64 { return 2 }
        \\fn baz() i64 { return 3 }
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    try testing.expectEqual(@as(usize, 4), module.functions.items.len);
    try testing.expect(hasFunction(module, "foo"));
    try testing.expect(hasFunction(module, "bar"));
    try testing.expect(hasFunction(module, "baz"));
    try testing.expect(hasFunction(module, "main"));
}

// ============================================================================
// Variable Declaration Tests
// ============================================================================

test "lower: variable declaration" {
    const allocator = testing.allocator;
    const source =
        \\fn main() {
        \\    var x = 42
        \\}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const main_fn = getFunction(module, "main").?;
    // Should have alloca for variable and store
    try testing.expect(hasInstruction(main_fn, .alloca));
}

test "lower: const declaration" {
    const allocator = testing.allocator;
    const source =
        \\fn main() {
        \\    const x = 100
        \\}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    try testing.expect(hasFunction(module, "main"));
}

test "lower: multiple variable declarations" {
    const allocator = testing.allocator;
    const source =
        \\fn main() {
        \\    var a = 1
        \\    var b = 2
        \\    var c = 3
        \\}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const main_fn = getFunction(module, "main").?;
    try testing.expect(countInstructions(main_fn) >= 3);
}

// ============================================================================
// Arithmetic Expression Tests
// ============================================================================

test "lower: binary addition" {
    const allocator = testing.allocator;
    const source =
        \\fn add() i64 {
        \\    return 10 + 20
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const add_fn = getFunction(module, "add").?;
    try testing.expect(hasInstruction(add_fn, .iadd));
}

test "lower: binary subtraction" {
    const allocator = testing.allocator;
    const source =
        \\fn sub() i64 {
        \\    return 100 - 50
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const sub_fn = getFunction(module, "sub").?;
    try testing.expect(hasInstruction(sub_fn, .isub));
}

test "lower: binary multiplication" {
    const allocator = testing.allocator;
    const source =
        \\fn mul() i64 {
        \\    return 7 * 6
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const mul_fn = getFunction(module, "mul").?;
    try testing.expect(hasInstruction(mul_fn, .imul));
}

test "lower: binary division" {
    const allocator = testing.allocator;
    const source =
        \\fn div() i64 {
        \\    return 100 / 5
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const div_fn = getFunction(module, "div").?;
    try testing.expect(hasInstruction(div_fn, .sdiv) or hasInstruction(div_fn, .udiv));
}

test "lower: complex arithmetic expression" {
    const allocator = testing.allocator;
    const source =
        \\fn calc() i64 {
        \\    return (2 + 3) * 4
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const calc_fn = getFunction(module, "calc").?;
    try testing.expect(hasInstruction(calc_fn, .iadd));
    try testing.expect(hasInstruction(calc_fn, .imul));
}

// ============================================================================
// Comparison Expression Tests
// ============================================================================

test "lower: less than comparison" {
    const allocator = testing.allocator;
    const source =
        \\fn compare() i64 {
        \\    if 1 < 2 { return 1 }
        \\    return 0
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const cmp_fn = getFunction(module, "compare").?;
    try testing.expect(hasInstruction(cmp_fn, .icmp));
}

test "lower: equality comparison" {
    const allocator = testing.allocator;
    const source =
        \\fn is_equal(a: i64, b: i64) i64 {
        \\    if a == b { return 1 }
        \\    return 0
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const fn_ptr = getFunction(module, "is_equal").?;
    try testing.expect(hasInstruction(fn_ptr, .icmp));
}

// ============================================================================
// Control Flow Tests
// ============================================================================

test "lower: if statement" {
    const allocator = testing.allocator;
    const source =
        \\fn check(x: i64) i64 {
        \\    if x > 0 {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const check_fn = getFunction(module, "check").?;
    // If statements generate brif (branch if) instructions
    try testing.expect(hasInstruction(check_fn, .brif));
}

test "lower: if-else statement" {
    const allocator = testing.allocator;
    const source =
        \\fn test_if(x: i64) i64 {
        \\    if x > 0 {
        \\        return 1
        \\    } else {
        \\        return 0
        \\    }
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const test_fn = getFunction(module, "test_if").?;
    try testing.expect(hasInstruction(test_fn, .brif));
    // Multiple blocks for if/else
    try testing.expect(test_fn.blocks.items.len >= 2);
}

// ============================================================================
// Function Call Tests
// ============================================================================

test "lower: function call" {
    const allocator = testing.allocator;
    const source =
        \\fn helper() i64 { return 42 }
        \\fn main() {
        \\    var x = helper()
        \\}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const main_fn = getFunction(module, "main").?;
    try testing.expect(hasInstruction(main_fn, .call));
}

test "lower: function call with arguments" {
    const allocator = testing.allocator;
    const source =
        \\fn add(a: i64, b: i64) i64 { return a + b }
        \\fn main() {
        \\    var result = add(10, 20)
        \\}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const main_fn = getFunction(module, "main").?;
    try testing.expect(hasInstruction(main_fn, .call));
}

test "lower: recursive call" {
    const allocator = testing.allocator;
    const source =
        \\fn fac(n: i64) i64 {
        \\    if n <= 1 { return 1 }
        \\    return n * fac(n - 1)
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const fac_fn = getFunction(module, "fac").?;
    try testing.expect(hasInstruction(fac_fn, .call));
}

// ============================================================================
// Return Statement Tests
// ============================================================================

test "lower: return constant" {
    const allocator = testing.allocator;
    const source =
        \\fn get42() i64 {
        \\    return 42
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const get_fn = getFunction(module, "get42").?;
    try testing.expect(hasInstruction(get_fn, .return_));
    try testing.expect(hasInstruction(get_fn, .iconst));
}

test "lower: return expression" {
    const allocator = testing.allocator;
    const source =
        \\fn calc() i64 {
        \\    return 10 + 20
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const calc_fn = getFunction(module, "calc").?;
    try testing.expect(hasInstruction(calc_fn, .return_));
}

test "lower: early return in if" {
    const allocator = testing.allocator;
    const source =
        \\fn early(x: i64) i64 {
        \\    if x > 0 {
        \\        return x
        \\    }
        \\    return 0
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const early_fn = getFunction(module, "early").?;
    // Should have multiple return instructions
    var return_count: usize = 0;
    for (early_fn.blocks.items) |block| {
        for (block.instructions.items) |inst| {
            if (std.meta.activeTag(inst) == .return_) {
                return_count += 1;
            }
        }
    }
    try testing.expect(return_count >= 2);
}

// ============================================================================
// Struct Definition Tests
// ============================================================================

test "lower: struct definition" {
    const allocator = testing.allocator;
    const source =
        \\struct Point {
        \\    x: i64,
        \\    y: i64,
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    try testing.expect(hasFunction(module, "main"));
}

// ============================================================================
// Edge Case Tests
// ============================================================================

test "lower: deeply nested expressions" {
    const allocator = testing.allocator;
    const source =
        \\fn nested() i64 {
        \\    return ((1 + 2) * (3 + 4)) + ((5 - 6) * (7 - 8))
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const nested_fn = getFunction(module, "nested").?;
    try testing.expect(hasInstruction(nested_fn, .iadd));
    try testing.expect(hasInstruction(nested_fn, .imul));
    try testing.expect(hasInstruction(nested_fn, .isub));
}

test "lower: chained function calls" {
    const allocator = testing.allocator;
    const source =
        \\fn inc(x: i64) i64 { return x + 1 }
        \\fn double(x: i64) i64 { return x * 2 }
        \\fn main() {
        \\    var x = double(inc(5))
        \\}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const main_fn = getFunction(module, "main").?;
    // Should have two call instructions
    var call_count: usize = 0;
    for (main_fn.blocks.items) |block| {
        for (block.instructions.items) |inst| {
            if (std.meta.activeTag(inst) == .call) {
                call_count += 1;
            }
        }
    }
    try testing.expect(call_count >= 2);
}

test "lower: multiple comparisons" {
    const allocator = testing.allocator;
    const source =
        \\fn in_range(x: i64) i64 {
        \\    if x > 0 {
        \\        if x < 100 {
        \\            return 1
        \\        }
        \\    }
        \\    return 0
        \\}
        \\fn main() {}
    ;

    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const range_fn = getFunction(module, "in_range").?;
    // Should have multiple comparison instructions
    var cmp_count: usize = 0;
    for (range_fn.blocks.items) |block| {
        for (block.instructions.items) |inst| {
            if (std.meta.activeTag(inst) == .icmp) {
                cmp_count += 1;
            }
        }
    }
    try testing.expect(cmp_count >= 2);
}

// ============================================================================
// ARC Emission Tests
// ============================================================================

/// Parse source code and lower to IR with ARC emission enabled
fn lowerSourceWithArc(allocator: std.mem.Allocator, source: []const u8) !*ir.Module {
    // Tokenize
    var lex = lexer.Lexer.init(source);
    const tokens = try lex.tokenize(allocator);
    defer allocator.free(tokens);

    // Parse
    var strings = base.StringInterner.init(allocator);
    defer strings.deinit();

    var store = ast.NodeStore.init(allocator, &strings);
    defer store.deinit();

    var parse = parser.Parser.init(allocator, tokens, &store, &strings);
    defer parse.deinit();

    const top_level = try parse.parse();
    defer allocator.free(top_level);

    // Lower to IR with ARC enabled
    return lower.lowerWithOptions(allocator, &store, &strings, top_level, "test", .{ .emit_arc = true });
}

/// Count instructions of a specific type in a function
fn countInstructionsByTag(func: *const ir.Function, comptime tag: std.meta.Tag(ir.Instruction)) usize {
    var count: usize = 0;
    for (func.blocks.items) |block| {
        for (block.instructions.items) |inst| {
            if (std.meta.activeTag(inst) == tag) {
                count += 1;
            }
        }
    }
    return count;
}

test "ARC: emit_arc flag emits arc_retain for string return" {
    const allocator = testing.allocator;
    const source =
        \\fn get_name(): string {
        \\    return "hello"
        \\}
        \\fn main() {}
    ;

    var module = try lowerSourceWithArc(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const get_name_fn = getFunction(module, "get_name").?;

    // Should have arc_retain instruction for the return value
    const retain_count = countInstructionsByTag(get_name_fn, .arc_retain);
    try testing.expect(retain_count >= 1);
}

test "ARC: emit_arc flag emits arc_retain for string assignment" {
    const allocator = testing.allocator;
    const source =
        \\fn test_assign() {
        \\    let s: string = "hello"
        \\}
        \\fn main() {}
    ;

    var module = try lowerSourceWithArc(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const test_fn = getFunction(module, "test_assign").?;

    // Should have arc_retain for the string assignment
    const retain_count = countInstructionsByTag(test_fn, .arc_retain);
    try testing.expect(retain_count >= 1);
}

test "ARC: emit_arc disabled does not emit arc_retain" {
    const allocator = testing.allocator;
    const source =
        \\fn get_name(): string {
        \\    return "hello"
        \\}
        \\fn main() {}
    ;

    // Use normal lowering (emit_arc = false)
    var module = try lowerSource(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const get_name_fn = getFunction(module, "get_name").?;

    // Should NOT have arc_retain instruction
    const retain_count = countInstructionsByTag(get_name_fn, .arc_retain);
    try testing.expectEqual(@as(usize, 0), retain_count);
}

test "ARC: emit_arc flag emits arc_release for function return scope cleanup" {
    const allocator = testing.allocator;
    const source =
        \\fn test_scope(): i64 {
        \\    let s: string = "hello"
        \\    return 42
        \\}
        \\fn main() {}
    ;

    var module = try lowerSourceWithArc(allocator, source);
    defer {
        module.deinit();
        allocator.destroy(module);
    }

    const test_fn = getFunction(module, "test_scope").?;

    // Should have arc_release for the string local before return
    const release_count = countInstructionsByTag(test_fn, .arc_release);
    try testing.expect(release_count >= 1);
}
