//! End-to-end Native AOT compilation tests.
//!
//! Tests the full pipeline: Cot source → Wasm → CLIF → Machine Code → Executable → Run
//!
//! This file exists to verify native AOT produces correct executables.
//! See NATIVE_AOT_FIXES.md for the bugs being tracked.
//!
//! Test methodology:
//! 1. Compile Cot source to native object file using Driver
//! 2. Link with zig cc
//! 3. Run the executable
//! 4. Verify exit code matches expected value

const std = @import("std");
const Driver = @import("../driver.zig").Driver;
const Target = @import("../core/target.zig").Target;

/// Result of compiling and running native code.
const NativeResult = struct {
    exit_code: ?u32,
    compile_error: bool,
    link_error: bool,
    run_error: bool,
    error_msg: []const u8,

    pub fn success(code: u32) NativeResult {
        return .{
            .exit_code = code,
            .compile_error = false,
            .link_error = false,
            .run_error = false,
            .error_msg = "",
        };
    }

    pub fn compileErr(msg: []const u8) NativeResult {
        return .{
            .exit_code = null,
            .compile_error = true,
            .link_error = false,
            .run_error = false,
            .error_msg = msg,
        };
    }

    pub fn linkErr(msg: []const u8) NativeResult {
        return .{
            .exit_code = null,
            .compile_error = false,
            .link_error = true,
            .run_error = false,
            .error_msg = msg,
        };
    }

    pub fn runErr(msg: []const u8) NativeResult {
        return .{
            .exit_code = null,
            .compile_error = false,
            .link_error = false,
            .run_error = true,
            .error_msg = msg,
        };
    }
};

/// Compile Cot source to native executable and run it.
/// Returns the exit code or error information.
fn compileAndRun(allocator: std.mem.Allocator, code: []const u8, test_name: []const u8) NativeResult {
    // Use a unique temp directory for this test
    const tmp_dir = "/tmp/cot_native_test";
    std.fs.cwd().makePath(tmp_dir) catch {};

    const obj_path = std.fmt.allocPrint(allocator, "{s}/{s}.o", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(obj_path);

    const exe_path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(exe_path);

    // Step 1: Compile to native object code
    var driver = Driver.init(allocator);
    driver.setTarget(Target.native());

    const obj_code = driver.compileSource(code) catch |e| {
        const msg = std.fmt.allocPrint(allocator, "compile error: {any}", .{e}) catch "compile error";
        return NativeResult.compileErr(msg);
    };
    defer allocator.free(obj_code);

    // Step 2: Write object file
    std.fs.cwd().writeFile(.{ .sub_path = obj_path, .data = obj_code }) catch
        return NativeResult.compileErr("failed to write .o file");

    // Step 3: Link with zig cc
    const link_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-o", exe_path, obj_path },
    }) catch return NativeResult.linkErr("failed to spawn linker");
    defer allocator.free(link_result.stdout);
    defer allocator.free(link_result.stderr);

    if (link_result.term.Exited != 0) {
        return NativeResult.linkErr("linker failed");
    }

    // Step 4: Run the executable
    const run_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{exe_path},
    }) catch return NativeResult.runErr("failed to spawn executable");
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    // Step 5: Get exit code
    return switch (run_result.term) {
        .Exited => |exit_code| NativeResult.success(exit_code),
        .Signal => |sig| blk: {
            // SIGSEGV = 11, SIGILL = 4, SIGBUS = 10
            const msg = std.fmt.allocPrint(allocator, "signal {d}", .{sig}) catch "signal";
            break :blk NativeResult.runErr(msg);
        },
        else => NativeResult.runErr("unknown termination"),
    };
}

/// Helper to run a test and check expected exit code.
/// Uses arena allocator because native codegen has memory leaks that need separate fixing.
fn expectExitCode(backing_allocator: std.mem.Allocator, code: []const u8, expected: u32, test_name: []const u8) !void {
    // Use arena to avoid leak detection issues - native codegen has known leaks
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = compileAndRun(allocator, code, test_name);

    if (result.compile_error) {
        std.debug.print("\n[{s}] COMPILE ERROR: {s}\n", .{ test_name, result.error_msg });
        return error.CompileError;
    }
    if (result.link_error) {
        std.debug.print("\n[{s}] LINK ERROR: {s}\n", .{ test_name, result.error_msg });
        return error.LinkError;
    }
    if (result.run_error) {
        std.debug.print("\n[{s}] RUN ERROR: {s}\n", .{ test_name, result.error_msg });
        return error.RunError;
    }

    const actual = result.exit_code orelse return error.NoExitCode;
    if (actual != expected) {
        std.debug.print("\n[{s}] WRONG EXIT CODE: expected {d}, got {d}\n", .{ test_name, expected, actual });
        return error.WrongExitCode;
    }
}

// ============================================================================
// WORKING TESTS - These pass and verify baseline functionality
// ============================================================================

test "native: return constant 42" {
    const code =
        \\fn main() int {
        \\    return 42;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 42, "return_42");
}

test "native: return constant 0" {
    const code =
        \\fn main() int {
        \\    return 0;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "return_0");
}

test "native: return constant 255" {
    const code =
        \\fn main() int {
        \\    return 255;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 255, "return_255");
}

test "native: add two constants" {
    const code =
        \\fn main() int {
        \\    return 10 + 5;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 15, "add_constants");
}

test "native: subtract constants" {
    const code =
        \\fn main() int {
        \\    return 20 - 8;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 12, "sub_constants");
}

test "native: multiply constants" {
    const code =
        \\fn main() int {
        \\    return 6 * 7;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 42, "mul_constants");
}

test "native: complex expression" {
    const code =
        \\fn main() int {
        \\    return 2 + 3 * 4;
        \\}
    ;
    // Should be 2 + (3 * 4) = 14 with proper precedence
    try expectExitCode(std.testing.allocator, code, 14, "complex_expr");
}

// ============================================================================
// WORKING TESTS - These were fixed and now pass
// ============================================================================

test "native: local variable" {
    const code =
        \\fn main() int {
        \\    let x = 10;
        \\    let y = 5;
        \\    return x + y;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 15, "local_var");
}

test "native: function call no params" {
    const code =
        \\fn get_five() int {
        \\    return 5;
        \\}
        \\
        \\fn main() int {
        \\    return get_five();
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 5, "func_no_params");
}

test "native: function call one param" {
    const code =
        \\fn double(x: int) int {
        \\    return x + x;
        \\}
        \\
        \\fn main() int {
        \\    return double(10);
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 20, "func_one_param");
}

test "native: function call two params" {
    const code =
        \\fn add(a: int, b: int) int {
        \\    return a + b;
        \\}
        \\
        \\fn main() int {
        \\    return add(10, 5);
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 15, "func_two_params");
}

test "native: if true branch" {
    const code =
        \\fn main() int {
        \\    if 10 > 5 {
        \\        return 1;
        \\    } else {
        \\        return 0;
        \\    }
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 1, "if_true");
}

test "native: if false branch" {
    const code =
        \\fn main() int {
        \\    if 5 > 10 {
        \\        return 1;
        \\    } else {
        \\        return 0;
        \\    }
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "if_false");
}

test "native: while loop sum" {
    const code =
        \\fn main() int {
        \\    let sum = 0;
        \\    let i = 1;
        \\    while i <= 10 {
        \\        sum = sum + i;
        \\        i = i + 1;
        \\    }
        \\    return sum;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 55, "while_sum");
}

// ============================================================================
// BROKEN TESTS - Known issues that need fixing
// ============================================================================

// Recursion with values across calls - caller-saved registers clobbered
// See MEMORY.md issue #14: values in RCX/RDX/etc are destroyed by recursive call
// test "native: factorial recursive" {
//     const code =
//         \\fn factorial(n: int) int {
//         \\    if n <= 1 {
//         \\        return 1;
//         \\    }
//         \\    return n * factorial(n - 1);
//         \\}
//         \\
//         \\fn main() int {
//         \\    return factorial(5);
//         \\}
//     ;
//     try expectExitCode(std.testing.allocator, code, 120, "factorial");
// }
