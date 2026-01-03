//! Integration Tests
//!
//! End-to-end tests that compile Cot source and execute it in the VM.
//! These tests verify the complete pipeline: lexer -> parser -> IR -> bytecode -> VM.

const std = @import("std");
const cot = @import("../root.zig");
const bytecode = cot.bytecode;

// ============================================================================
// Compilation Tests
// ============================================================================

test "integration: empty main compiles" {
    const source = "fn main() {}";
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: function with return compiles" {
    const source =
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\fn main() {}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: multiple functions compile" {
    const source =
        \\fn foo() i64 { return 1 }
        \\fn bar() i64 { return 2 }
        \\fn baz() i64 { return 3 }
        \\fn main() {}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: variable declaration compiles" {
    const source =
        \\fn main() {
        \\    var x = 42
        \\    var y = 100
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: const declaration compiles" {
    const source =
        \\fn main() {
        \\    const x = 42
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: if statement compiles" {
    const source =
        \\fn main() {
        \\    var x = 10
        \\    if x > 5 {
        \\        var y = 1
        \\    }
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: if-else compiles" {
    const source =
        \\fn check(x: i64) i64 {
        \\    if x > 0 {
        \\        return 1
        \\    } else {
        \\        return 0
        \\    }
        \\}
        \\fn main() {}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: nested function calls compile" {
    const source =
        \\fn double(x: i64) i64 { return x * 2 }
        \\fn quadruple(x: i64) i64 { return double(double(x)) }
        \\fn main() {
        \\    var result = quadruple(5)
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: recursive function compiles" {
    const source =
        \\fn factorial(n: i64) i64 {
        \\    if n <= 1 {
        \\        return 1
        \\    }
        \\    return n * factorial(n - 1)
        \\}
        \\fn main() {}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: arithmetic expressions compile" {
    const source =
        \\fn main() {
        \\    var a = 10 + 20
        \\    var b = 100 - 50
        \\    var c = 7 * 6
        \\    var d = 100 / 5
        \\    var e = 17 % 5
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: comparison expressions compile" {
    const source =
        \\fn main() {
        \\    var a = 10 < 20
        \\    var b = 10 <= 20
        \\    var c = 10 > 5
        \\    var d = 10 >= 10
        \\    var e = 10 == 10
        \\    var f = 10 != 5
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: boolean expressions compile" {
    const source =
        \\fn main() {
        \\    var a = true
        \\    var b = false
        \\    var c = !a
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: struct definition compiles" {
    const source =
        \\struct Point {
        \\    x: i64,
        \\    y: i64,
        \\}
        \\fn main() {}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: negative literal compiles" {
    const source =
        \\fn main() {
        \\    var x = 0 - 42
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: parenthesized expression compiles" {
    const source =
        \\fn main() {
        \\    var x = (2 + 3) * 4
        \\}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

test "integration: function with multiple params compiles" {
    const source =
        \\fn sum3(a: i64, b: i64, c: i64) i64 {
        \\    return a + b + c
        \\}
        \\fn main() {}
    ;
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();
}

// ============================================================================
// Execution Tests
// ============================================================================

fn runAndGetResult(source: []const u8) !bytecode.Value {
    var module = try cot.compileToModule(std.testing.allocator, source, "test");
    defer module.deinit();

    var vm = bytecode.VM.init(std.testing.allocator);
    defer vm.deinit();

    try vm.execute(&module);
    return vm.registers[15]; // Return value register
}

test "integration: execute empty main" {
    const source = "fn main() {}";
    _ = try runAndGetResult(source);
}

test "integration: execute simple return" {
    const source =
        \\fn get42() i64 {
        \\    return 42
        \\}
        \\fn main() {
        \\    var x = get42()
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: execute addition" {
    const source =
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\fn main() {
        \\    var result = add(10, 32)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: execute if-true branch" {
    const source =
        \\fn test_if() i64 {
        \\    if 1 > 0 {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\fn main() {
        \\    var x = test_if()
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: execute if-false branch" {
    const source =
        \\fn test_if() i64 {
        \\    if 0 > 1 {
        \\        return 1
        \\    }
        \\    return 0
        \\}
        \\fn main() {
        \\    var x = test_if()
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: execute nested calls" {
    const source =
        \\fn inc(x: i64) i64 { return x + 1 }
        \\fn inc3(x: i64) i64 { return inc(inc(inc(x))) }
        \\fn main() {
        \\    var result = inc3(0)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: execute arithmetic chain" {
    const source =
        \\fn calculate() i64 {
        \\    var a = 10
        \\    var b = 20
        \\    var c = a + b
        \\    var d = c * 2
        \\    return d
        \\}
        \\fn main() {
        \\    var x = calculate()
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: execute comparison chain" {
    const source =
        \\fn is_in_range(x: i64) i64 {
        \\    if x > 0 {
        \\        if x < 100 {
        \\            return 1
        \\        }
        \\    }
        \\    return 0
        \\}
        \\fn main() {
        \\    var a = is_in_range(50)
        \\}
    ;
    _ = try runAndGetResult(source);
}

// ============================================================================
// Error Cases
// ============================================================================

test "integration: parse error on missing brace" {
    const source = "fn main() {";
    const result = cot.compileToModule(std.testing.allocator, source, "test");
    try std.testing.expectError(error.ParseError, result);
}

test "integration: parse error on missing paren" {
    const source = "fn main( {}";
    const result = cot.compileToModule(std.testing.allocator, source, "test");
    try std.testing.expectError(error.ParseError, result);
}

test "integration: parse error on invalid token" {
    const source = "fn main() { @ }";
    const result = cot.compileToModule(std.testing.allocator, source, "test");
    try std.testing.expectError(error.ParseError, result);
}

test "integration: parse error on incomplete if" {
    const source =
        \\fn main() {
        \\    if
        \\}
    ;
    const result = cot.compileToModule(std.testing.allocator, source, "test");
    try std.testing.expectError(error.ParseError, result);
}

// ============================================================================
// Complex Scenarios
// ============================================================================

test "integration: fibonacci-like recursion" {
    const source =
        \\fn fib(n: i64) i64 {
        \\    if n <= 1 {
        \\        return n
        \\    }
        \\    return fib(n - 1) + fib(n - 2)
        \\}
        \\fn main() {
        \\    var f = fib(5)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: mutual recursion" {
    const source =
        \\fn is_even(n: i64) i64 {
        \\    if n == 0 { return 1 }
        \\    return is_odd(n - 1)
        \\}
        \\fn is_odd(n: i64) i64 {
        \\    if n == 0 { return 0 }
        \\    return is_even(n - 1)
        \\}
        \\fn main() {
        \\    var even = is_even(4)
        \\    var odd = is_odd(5)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: abs function" {
    const source =
        \\fn abs(x: i64) i64 {
        \\    if x < 0 {
        \\        return 0 - x
        \\    }
        \\    return x
        \\}
        \\fn main() {
        \\    var a = abs(0 - 5)
        \\    var b = abs(10)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: max function" {
    const source =
        \\fn max(a: i64, b: i64) i64 {
        \\    if a > b {
        \\        return a
        \\    }
        \\    return b
        \\}
        \\fn main() {
        \\    var m = max(10, 20)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: min function" {
    const source =
        \\fn min(a: i64, b: i64) i64 {
        \\    if a < b {
        \\        return a
        \\    }
        \\    return b
        \\}
        \\fn main() {
        \\    var m = min(10, 20)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: sign function" {
    const source =
        \\fn sign(x: i64) i64 {
        \\    if x > 0 {
        \\        return 1
        \\    }
        \\    if x < 0 {
        \\        return 0 - 1
        \\    }
        \\    return 0
        \\}
        \\fn main() {
        \\    var pos = sign(100)
        \\    var neg = sign(0 - 50)
        \\    var zero = sign(0)
        \\}
    ;
    _ = try runAndGetResult(source);
}

test "integration: clamp function" {
    const source =
        \\fn clamp(x: i64, lo: i64, hi: i64) i64 {
        \\    if x < lo {
        \\        return lo
        \\    }
        \\    if x > hi {
        \\        return hi
        \\    }
        \\    return x
        \\}
        \\fn main() {
        \\    var c = clamp(50, 0, 100)
        \\}
    ;
    _ = try runAndGetResult(source);
}
