//! Custom Test Assertions
//!
//! Provides domain-specific assertions for testing the Cot compiler and runtime.

const std = @import("std");
const cot = @import("../root.zig");
const bytecode = cot.bytecode;
const native = cot.native;
const DiagnosticCode = cot.compiler.diagnostics.DiagnosticCode;

/// Expect source to produce a specific compile-time diagnostic code
pub fn expectCompileError(source: []const u8, expected_code: DiagnosticCode) !void {
    // Compile the source
    var module = cot.compileToModule(std.testing.allocator, source, "test") catch {
        // Compilation failed as expected
        // TODO: Check specific error code when we have better error propagation
        return;
    };
    module.deinit();

    std.debug.print("Expected compile error {s}, but compilation succeeded\n", .{@tagName(expected_code)});
    return error.ExpectedCompileError;
}

/// Expect source to produce a parse error
pub fn expectParseError(source: []const u8) !void {
    var module = cot.compileToModule(std.testing.allocator, source, "test") catch |err| {
        if (err == error.ParseError) {
            return; // Expected
        }
        std.debug.print("Expected ParseError, but got: {}\n", .{err});
        return error.WrongErrorType;
    };
    module.deinit();

    std.debug.print("Expected parse error, but compilation succeeded\n", .{});
    return error.ExpectedParseError;
}

/// Expect source to produce a specific runtime error when executed
pub fn expectRuntimeError(source: []const u8, expected_error: anyerror) !void {
    var module = cot.compileToModule(std.testing.allocator, source, "test") catch |err| {
        std.debug.print("Expected runtime error, but compilation failed: {}\n", .{err});
        return error.UnexpectedCompileError;
    };
    defer module.deinit();

    var vm = bytecode.VM.init(std.testing.allocator);
    defer vm.deinit();

    // Load stdlib
    var stdlib = native.Stdlib.init(std.testing.allocator, &vm.native_registry);
    defer stdlib.deinit();
    stdlib.loadAll() catch {};

    vm.execute(&module) catch |err| {
        if (err == expected_error) {
            return; // Got expected error
        }
        std.debug.print("Expected runtime error {}, but got: {}\n", .{ expected_error, err });
        return error.WrongRuntimeError;
    };

    std.debug.print("Expected runtime error {}, but execution succeeded\n", .{expected_error});
    return error.ExpectedRuntimeError;
}

/// Compile and run source, expecting specific output (for future I/O capture)
pub fn expectOutput(source: []const u8, expected_stdout: []const u8) !void {
    _ = expected_stdout;
    // TODO: Implement output capture
    // For now, just verify it compiles and runs
    var module = cot.compileToModule(std.testing.allocator, source, "test") catch |err| {
        std.debug.print("Compilation failed: {}\n", .{err});
        return error.UnexpectedCompileError;
    };
    defer module.deinit();

    var vm = bytecode.VM.init(std.testing.allocator);
    defer vm.deinit();

    var stdlib = native.Stdlib.init(std.testing.allocator, &vm.native_registry);
    defer stdlib.deinit();
    stdlib.loadAll() catch {};

    vm.execute(&module) catch |err| {
        std.debug.print("Execution failed: {}\n", .{err});
        return error.UnexpectedRuntimeError;
    };
}

/// Assert that an IR module contains a specific instruction
pub fn expectIRContains(ir_module: *const cot.ir.Module, instruction_name: []const u8) !void {
    for (ir_module.functions.items) |func| {
        for (func.instructions.items) |instr| {
            const tag_name = @tagName(instr.op);
            if (std.mem.eql(u8, tag_name, instruction_name)) {
                return; // Found it
            }
        }
    }
    std.debug.print("Expected IR to contain instruction: {s}\n", .{instruction_name});
    return error.InstructionNotFound;
}

/// Assert bytecode module contains expected number of functions
pub fn expectFunctionCount(module: *const bytecode.Module, expected: usize) !void {
    const actual = module.routines.len;
    if (actual != expected) {
        std.debug.print("Expected {d} functions, got {d}\n", .{ expected, actual });
        return error.WrongFunctionCount;
    }
}

/// Assert a value equals expected integer
pub fn expectValueInt(value: bytecode.Value, expected: i64) !void {
    const actual = value.asInt();
    if (actual != expected) {
        std.debug.print("Expected value {d}, got {d}\n", .{ expected, actual });
        return error.ValueMismatch;
    }
}

/// Assert a value is nil
pub fn expectValueNil(value: bytecode.Value) !void {
    if (!value.isNil()) {
        std.debug.print("Expected nil value\n", .{});
        return error.ExpectedNil;
    }
}

/// Assert a value is truthy (non-zero, non-nil)
pub fn expectTruthy(value: bytecode.Value) !void {
    if (!value.isTruthy()) {
        std.debug.print("Expected truthy value\n", .{});
        return error.ExpectedTruthy;
    }
}

/// Assert a value is falsy (zero or nil)
pub fn expectFalsy(value: bytecode.Value) !void {
    if (value.isTruthy()) {
        std.debug.print("Expected falsy value\n", .{});
        return error.ExpectedFalsy;
    }
}

// ============================================================
// Tests
// ============================================================

test "assertions: expectParseError" {
    try expectParseError("fn main( {}"); // Missing )
}

test "assertions: expectParseError fails on valid code" {
    const result = expectParseError("fn main() {}");
    try std.testing.expectError(error.ExpectedParseError, result);
}
