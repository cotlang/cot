//! Native AOT compilation tests (Zig-level).
//!
//! Tests the full pipeline: Cot source -> Wasm -> CLIF -> Machine Code -> Executable -> Run
//!
//! These are Zig codegen tests using small inline Cot snippets (3-5 lines each).
//! They verify print/println output, fd_write/fd_read, @exit, and test-mode formatting.
//!
//! For comprehensive Cot language tests, use `cot test` on files in test/e2e/ and test/cases/.
//! See test/run_all.sh for running all Cot tests.

const std = @import("std");
const Driver = @import("../driver.zig").Driver;
const Target = @import("../frontend/target.zig").Target;

const NativeResult = struct {
    exit_code: ?u32,
    compile_error: bool,
    link_error: bool,
    run_error: bool,
    error_msg: []const u8,
    stdout: []const u8,
    stderr: []const u8 = "",

    pub fn success(code: u32) NativeResult {
        return .{ .exit_code = code, .compile_error = false, .link_error = false, .run_error = false, .error_msg = "", .stdout = "" };
    }
    pub fn successWithOutput(code: u32, output: []const u8) NativeResult {
        return .{ .exit_code = code, .compile_error = false, .link_error = false, .run_error = false, .error_msg = "", .stdout = output };
    }
    pub fn successFull(code: u32, stdout_out: []const u8, stderr_out: []const u8) NativeResult {
        return .{ .exit_code = code, .compile_error = false, .link_error = false, .run_error = false, .error_msg = "", .stdout = stdout_out, .stderr = stderr_out };
    }
    pub fn compileErr(msg: []const u8) NativeResult {
        return .{ .exit_code = null, .compile_error = true, .link_error = false, .run_error = false, .error_msg = msg, .stdout = "" };
    }
    pub fn linkErr(msg: []const u8) NativeResult {
        return .{ .exit_code = null, .compile_error = false, .link_error = true, .run_error = false, .error_msg = msg, .stdout = "" };
    }
    pub fn runErr(msg: []const u8) NativeResult {
        return .{ .exit_code = null, .compile_error = false, .link_error = false, .run_error = true, .error_msg = msg, .stdout = "" };
    }
};

// ============================================================================
// Print tests: verify print/println produce correct stdout output
// ============================================================================

fn compileAndRun(allocator: std.mem.Allocator, code: []const u8, test_name: []const u8) NativeResult {
    const tmp_dir = "/tmp/cot_native_test";
    std.fs.cwd().makePath(tmp_dir) catch {};

    const obj_path = std.fmt.allocPrint(allocator, "{s}/{s}.o", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(obj_path);

    const exe_path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(exe_path);

    var driver = Driver.init(allocator);
    driver.setTarget(Target.native());

    const obj_code = driver.compileSource(code) catch |e| {
        const msg = std.fmt.allocPrint(allocator, "compile error: {any}", .{e}) catch "compile error";
        return NativeResult.compileErr(msg);
    };
    defer allocator.free(obj_code);

    std.fs.cwd().writeFile(.{ .sub_path = obj_path, .data = obj_code }) catch
        return NativeResult.compileErr("failed to write .o file");

    const link_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-o", exe_path, obj_path },
    }) catch return NativeResult.linkErr("failed to spawn linker");
    defer allocator.free(link_result.stdout);
    defer allocator.free(link_result.stderr);

    if (link_result.term.Exited != 0) {
        if (link_result.stderr.len > 0) std.debug.print("LINKER STDERR: {s}\n", .{link_result.stderr});
        return NativeResult.linkErr("linker failed");
    }

    const run_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{exe_path},
    }) catch return NativeResult.runErr("failed to spawn executable");
    defer allocator.free(run_result.stderr);

    return switch (run_result.term) {
        .Exited => |exit_code| NativeResult.successWithOutput(exit_code, run_result.stdout),
        .Signal => |sig| blk: {
            allocator.free(run_result.stdout);
            const msg = std.fmt.allocPrint(allocator, "signal {d}", .{sig}) catch "signal";
            break :blk NativeResult.runErr(msg);
        },
        else => blk: {
            allocator.free(run_result.stdout);
            break :blk NativeResult.runErr("unknown termination");
        },
    };
}

fn expectOutput(backing_allocator: std.mem.Allocator, code: []const u8, expected_exit: u32, expected_stdout: []const u8, test_name: []const u8) !void {
    var timer = std.time.Timer.start() catch {
        std.debug.print("[native] {s}...", .{test_name});
        return expectOutputInner(backing_allocator, code, expected_exit, expected_stdout, test_name);
    };

    std.debug.print("[native] {s}...", .{test_name});

    try expectOutputInner(backing_allocator, code, expected_exit, expected_stdout, test_name);

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    if (elapsed_ms >= 1000.0) {
        std.debug.print("ok ({d:.0}ms) SLOW\n", .{elapsed_ms});
    } else {
        std.debug.print("ok ({d:.0}ms)\n", .{elapsed_ms});
    }
}

fn expectOutputInner(backing_allocator: std.mem.Allocator, code: []const u8, expected_exit: u32, expected_stdout: []const u8, test_name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = compileAndRun(allocator, code, test_name);

    if (result.compile_error) {
        std.debug.print("COMPILE ERROR: {s}\n", .{result.error_msg});
        return error.CompileError;
    }
    if (result.link_error) {
        std.debug.print("LINK ERROR: {s}\n", .{result.error_msg});
        return error.LinkError;
    }
    if (result.run_error) {
        std.debug.print("RUN ERROR: {s}\n", .{result.error_msg});
        return error.RunError;
    }

    const actual_exit = result.exit_code orelse return error.NoExitCode;
    if (actual_exit != expected_exit) {
        std.debug.print("WRONG EXIT CODE: expected {d}, got {d}\n", .{ expected_exit, actual_exit });
        return error.WrongExitCode;
    }

    if (!std.mem.eql(u8, result.stdout, expected_stdout)) {
        std.debug.print("WRONG STDOUT:\n  expected: \"{s}\" ({d} bytes)\n  actual:   \"{s}\" ({d} bytes)\n", .{
            expected_stdout, expected_stdout.len,
            result.stdout,   result.stdout.len,
        });
        return error.WrongOutput;
    }
}

test "native: print integer" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    print(42)
        \\    return 0
        \\}
    , 0, "42", "print_int");
}

test "native: println integer" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    println(0)
        \\    return 0
        \\}
    , 0, "0\n", "println_zero");
}

test "native: print negative" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    print(-1)
        \\    return 0
        \\}
    , 0, "-1", "print_neg");
}

test "native: println string" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    println("hello")
        \\    return 0
        \\}
    , 0, "hello\n", "println_str");
}

test "native: print does not corrupt return value" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    print(42)
        \\    return 7
        \\}
    , 7, "42", "print_ret");
}

test "native: fd_write to stdout" {
    try expectOutput(std.testing.allocator,
        \\extern fn fd_write(fd: i64, ptr: i64, len: i64) i64
        \\fn main() i64 {
        \\    var msg = "OK"
        \\    var n = fd_write(1, @ptrOf(msg), @lenOf(msg))
        \\    return 0
        \\}
    , 0, "OK", "fd_write_stdout");
}

// ============================================================================
// WASI I/O: fd_read with piped stdin
// Reference: Wasmtime piped_tests.rs — pipe producer→consumer via stdin
// ============================================================================

/// Compile, link, and run with stdin data piped via shell redirection.
/// Copied from compileAndRun, modified only for stdin piping.
fn compileAndRunWithStdin(allocator: std.mem.Allocator, code: []const u8, stdin_data: []const u8, test_name: []const u8) NativeResult {
    const tmp_dir = "/tmp/cot_native_test";
    std.fs.cwd().makePath(tmp_dir) catch {};

    const obj_path = std.fmt.allocPrint(allocator, "{s}/{s}.o", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(obj_path);

    const exe_path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(exe_path);

    var driver = Driver.init(allocator);
    driver.setTarget(Target.native());

    const obj_code = driver.compileSource(code) catch |e| {
        const msg = std.fmt.allocPrint(allocator, "compile error: {any}", .{e}) catch "compile error";
        return NativeResult.compileErr(msg);
    };
    defer allocator.free(obj_code);

    std.fs.cwd().writeFile(.{ .sub_path = obj_path, .data = obj_code }) catch
        return NativeResult.compileErr("failed to write .o file");

    const link_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-o", exe_path, obj_path },
    }) catch return NativeResult.linkErr("failed to spawn linker");
    defer allocator.free(link_result.stdout);
    defer allocator.free(link_result.stderr);

    if (link_result.term.Exited != 0) {
        if (link_result.stderr.len > 0) std.debug.print("LINKER STDERR: {s}\n", .{link_result.stderr});
        return NativeResult.linkErr("linker failed");
    }

    // Write stdin data to temp file, run exe with shell redirection
    // Reference: Wasmtime piped_tests.rs uses Stdio::piped() + take()
    const stdin_path = std.fmt.allocPrint(allocator, "{s}/{s}_stdin", .{ tmp_dir, test_name }) catch
        return NativeResult.runErr("allocPrint failed");
    defer allocator.free(stdin_path);

    std.fs.cwd().writeFile(.{ .sub_path = stdin_path, .data = stdin_data }) catch
        return NativeResult.runErr("failed to write stdin data");

    const shell_cmd = std.fmt.allocPrint(allocator, "{s} < {s}", .{ exe_path, stdin_path }) catch
        return NativeResult.runErr("allocPrint failed");
    defer allocator.free(shell_cmd);

    const run_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "/bin/sh", "-c", shell_cmd },
    }) catch return NativeResult.runErr("failed to spawn executable");
    defer allocator.free(run_result.stderr);

    return switch (run_result.term) {
        .Exited => |exit_code| NativeResult.successWithOutput(exit_code, run_result.stdout),
        .Signal => |sig| blk: {
            allocator.free(run_result.stdout);
            const msg = std.fmt.allocPrint(allocator, "signal {d}", .{sig}) catch "signal";
            break :blk NativeResult.runErr(msg);
        },
        else => blk: {
            allocator.free(run_result.stdout);
            break :blk NativeResult.runErr("unknown termination");
        },
    };
}

fn expectOutputWithStdin(backing_allocator: std.mem.Allocator, code: []const u8, stdin_data: []const u8, expected_exit: u32, expected_stdout: []const u8, test_name: []const u8) !void {
    var timer = std.time.Timer.start() catch {
        std.debug.print("[native] {s}...", .{test_name});
        return expectOutputWithStdinInner(backing_allocator, code, stdin_data, expected_exit, expected_stdout, test_name);
    };

    std.debug.print("[native] {s}...", .{test_name});

    try expectOutputWithStdinInner(backing_allocator, code, stdin_data, expected_exit, expected_stdout, test_name);

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    if (elapsed_ms >= 1000.0) {
        std.debug.print("ok ({d:.0}ms) SLOW\n", .{elapsed_ms});
    } else {
        std.debug.print("ok ({d:.0}ms)\n", .{elapsed_ms});
    }
}

fn expectOutputWithStdinInner(backing_allocator: std.mem.Allocator, code: []const u8, stdin_data: []const u8, expected_exit: u32, expected_stdout: []const u8, test_name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = compileAndRunWithStdin(allocator, code, stdin_data, test_name);

    if (result.compile_error) {
        std.debug.print("COMPILE ERROR: {s}\n", .{result.error_msg});
        return error.CompileError;
    }
    if (result.link_error) {
        std.debug.print("LINK ERROR: {s}\n", .{result.error_msg});
        return error.LinkError;
    }
    if (result.run_error) {
        std.debug.print("RUN ERROR: {s}\n", .{result.error_msg});
        return error.RunError;
    }

    const actual_exit = result.exit_code orelse return error.NoExitCode;
    if (actual_exit != expected_exit) {
        std.debug.print("WRONG EXIT CODE: expected {d}, got {d}\n", .{ expected_exit, actual_exit });
        return error.WrongExitCode;
    }
    if (!std.mem.eql(u8, result.stdout, expected_stdout)) {
        std.debug.print("WRONG STDOUT:\n  expected: \"{s}\" (len={d})\n  actual:   \"{s}\" (len={d})\n", .{
            expected_stdout, expected_stdout.len,
            result.stdout,   result.stdout.len,
        });
        return error.WrongOutput;
    }
}

// fd_read: read from piped stdin, echo to stdout
// Reference: Wasmtime piped_simple.rs — consumer reads from stdin, verifies data
test "native: fd_read from stdin" {
    try expectOutputWithStdin(std.testing.allocator,
        \\extern fn alloc(metadata: i64, size: i64) i64
        \\extern fn dealloc(ptr: i64) void
        \\extern fn fd_read(fd: i64, buf: i64, len: i64) i64
        \\extern fn fd_write(fd: i64, ptr: i64, len: i64) i64
        \\fn main() i64 {
        \\    var buf = alloc(0, 16)
        \\    var n = fd_read(0, buf, 5)
        \\    fd_write(1, buf, n)
        \\    dealloc(buf)
        \\    return 0
        \\}
    , "hello", 0, "hello", "fd_read_stdin");
}

// fd_close, fd_open, time, random, exit, args_count, arg_len, arg_ptr
// are tested via `cot test test/e2e/wasi_io.cot`. No individual Zig tests needed.

// ============================================================================
// Test Mode (cot test) E2E Tests -- inline code, verify output format
// ============================================================================

fn compileAndRunTestMode(allocator: std.mem.Allocator, code: []const u8, test_name: []const u8) NativeResult {
    const tmp_dir = "/tmp/cot_native_test";
    std.fs.cwd().makePath(tmp_dir) catch {};

    const obj_path = std.fmt.allocPrint(allocator, "{s}/{s}.o", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(obj_path);

    const exe_path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ tmp_dir, test_name }) catch
        return NativeResult.compileErr("allocPrint failed");
    defer allocator.free(exe_path);

    var driver = Driver.init(allocator);
    driver.setTarget(Target.native());
    driver.setTestMode(true);

    const obj_code = driver.compileSource(code) catch |e| {
        const msg = std.fmt.allocPrint(allocator, "compile error: {any}", .{e}) catch "compile error";
        return NativeResult.compileErr(msg);
    };
    defer allocator.free(obj_code);

    std.fs.cwd().writeFile(.{ .sub_path = obj_path, .data = obj_code }) catch
        return NativeResult.compileErr("failed to write .o file");

    const link_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-o", exe_path, obj_path },
    }) catch return NativeResult.linkErr("failed to spawn linker");
    defer allocator.free(link_result.stdout);
    defer allocator.free(link_result.stderr);

    if (link_result.term.Exited != 0) {
        if (link_result.stderr.len > 0) std.debug.print("LINKER STDERR: {s}\n", .{link_result.stderr});
        return NativeResult.linkErr("linker failed");
    }

    const run_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{exe_path},
    }) catch return NativeResult.runErr("failed to spawn executable");

    return switch (run_result.term) {
        .Exited => |exit_code| NativeResult.successFull(exit_code, run_result.stdout, run_result.stderr),
        .Signal => |sig| blk: {
            allocator.free(run_result.stdout);
            allocator.free(run_result.stderr);
            const msg = std.fmt.allocPrint(allocator, "signal {d}", .{sig}) catch "signal";
            break :blk NativeResult.runErr(msg);
        },
        else => blk: {
            allocator.free(run_result.stdout);
            allocator.free(run_result.stderr);
            break :blk NativeResult.runErr("unknown termination");
        },
    };
}

fn expectTestMode(backing_allocator: std.mem.Allocator, code: []const u8, expected_exit: u32, expected_stderr: []const u8, test_name: []const u8) !void {
    var timer = std.time.Timer.start() catch {
        std.debug.print("[native] {s}...", .{test_name});
        return expectTestModeInner(backing_allocator, code, expected_exit, expected_stderr, test_name);
    };

    std.debug.print("[native] {s}...", .{test_name});

    try expectTestModeInner(backing_allocator, code, expected_exit, expected_stderr, test_name);

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    if (elapsed_ms >= 1000.0) {
        std.debug.print("ok ({d:.0}ms) SLOW\n", .{elapsed_ms});
    } else {
        std.debug.print("ok ({d:.0}ms)\n", .{elapsed_ms});
    }
}

/// Strip ANSI escape sequences (\x1b[...m) and timing suffixes like " (Nms)" from test output
/// so that expected strings can be plain text without colors or non-deterministic timing.
fn stripAnsiAndTiming(allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
    var out = std.ArrayListUnmanaged(u8){};
    var i: usize = 0;
    while (i < input.len) {
        // Skip ANSI escape sequences: \x1b[ ... m
        if (i + 1 < input.len and input[i] == 0x1b and input[i + 1] == '[') {
            i += 2;
            while (i < input.len and input[i] != 'm') : (i += 1) {}
            if (i < input.len) i += 1; // skip 'm'
            continue;
        }
        // Skip timing suffixes: " (Nms)" where N is digits
        if (i + 2 < input.len and input[i] == ' ' and input[i + 1] == '(') {
            var j = i + 2;
            // Check for optional '-' then digits
            while (j < input.len and (input[j] >= '0' and input[j] <= '9')) : (j += 1) {}
            if (j + 2 < input.len and input[j] == 'm' and input[j + 1] == 's' and input[j + 2] == ')') {
                // Found " (Nms)", skip it
                i = j + 3;
                continue;
            }
        }
        try out.append(allocator, input[i]);
        i += 1;
    }
    return out.items;
}

fn expectTestModeInner(backing_allocator: std.mem.Allocator, code: []const u8, expected_exit: u32, expected_stderr: []const u8, test_name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = compileAndRunTestMode(allocator, code, test_name);

    if (result.compile_error) {
        std.debug.print("COMPILE ERROR: {s}\n", .{result.error_msg});
        return error.CompileError;
    }
    if (result.link_error) {
        std.debug.print("LINK ERROR: {s}\n", .{result.error_msg});
        return error.LinkError;
    }
    if (result.run_error) {
        std.debug.print("RUN ERROR: {s}\n", .{result.error_msg});
        return error.RunError;
    }

    const actual_exit = result.exit_code orelse return error.NoExitCode;
    if (actual_exit != expected_exit) {
        std.debug.print("WRONG EXIT CODE: expected {d}, got {d}\nSTDERR: {s}\n", .{ expected_exit, actual_exit, result.stderr });
        return error.WrongExitCode;
    }

    // Strip ANSI codes and timing from actual output for comparison
    const stripped = try stripAnsiAndTiming(allocator, result.stderr);
    if (!std.mem.eql(u8, stripped, expected_stderr)) {
        std.debug.print("WRONG STDERR:\n  expected: \"{s}\" ({d} bytes)\n  actual:   \"{s}\" ({d} bytes)\n  stripped: \"{s}\" ({d} bytes)\n", .{
            expected_stderr, expected_stderr.len,
            result.stderr,   result.stderr.len,
            stripped,         stripped.len,
        });
        return error.WrongOutput;
    }
}

// @exit() terminates the process, so it can't be batch-tested.
// These must remain as individual subprocess tests.
test "native: exit with code 42" {
    try expectOutput(std.testing.allocator,
        \\extern fn exit(code: i64) void
        \\fn main() void {
        \\    exit(42)
        \\}
    , 42, "", "exit_42");
}

test "native: exit with code 0" {
    try expectOutput(std.testing.allocator,
        \\extern fn exit(code: i64) void
        \\fn main() void {
        \\    exit(0)
        \\}
    , 0, "", "exit_0");
}

test "native: test mode - all pass" {
    try expectTestMode(std.testing.allocator,
        \\test "math" {
        \\    @assert(1 + 1 == 2)
        \\}
        \\test "bool" {
        \\    @assert(1 == 1)
        \\}
    , 0,
        "test \"math\" ... ok\ntest \"bool\" ... ok\n\nok | 2 passed\n",
        "test_all_pass");
}

test "native: test mode - one failure" {
    try expectTestMode(std.testing.allocator,
        \\test "pass" {
        \\    @assert(1 == 1)
        \\}
        \\test "fail" {
        \\    @assert(1 == 2)
        \\}
        \\test "also pass" {
        \\    @assert(2 == 2)
        \\}
    , 1,
        "test \"pass\" ... ok\ntest \"fail\" ... FAIL\ntest \"also pass\" ... ok\n\nFAILED | 2 passed | 1 failed\n",
        "test_one_fail");
}

test "native: test mode - assert_eq" {
    try expectTestMode(std.testing.allocator,
        \\test "eq" {
        \\    @assertEq(42, 42)
        \\}
    , 0,
        "test \"eq\" ... ok\n\nok | 1 passed\n",
        "test_assert_eq");
}
