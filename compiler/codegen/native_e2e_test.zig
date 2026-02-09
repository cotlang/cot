//! End-to-end Native AOT compilation tests.
//!
//! Tests the full pipeline: Cot source -> Wasm -> CLIF -> Machine Code -> Executable -> Run
//!
//! Architecture: One batch test compiles ALL test files together (1 compile, 1 link, 1 run),
//! plus 8 special tests that must remain isolated (print tests, test-mode format tests).
//!
//! All test files use inline `test "name" { }` format with error-union isolation.
//! Test files live in test/e2e/ (comprehensive) and test/cases/ (category unit tests).

const std = @import("std");
const Driver = @import("../driver.zig").Driver;
const Target = @import("../core/target.zig").Target;

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
// Test file paths and expected counts
// ============================================================================

const TestFileSpec = struct {
    path: []const u8,
    test_count: u32,
};

const batch_files = [_]TestFileSpec{
    // e2e/
    .{ .path = "test/e2e/features.cot", .test_count = 107 },
    .{ .path = "test/e2e/expressions.cot", .test_count = 160 },
    .{ .path = "test/e2e/functions.cot", .test_count = 107 },
    .{ .path = "test/e2e/control_flow.cot", .test_count = 82 },
    .{ .path = "test/e2e/variables.cot", .test_count = 40 },
    .{ .path = "test/e2e/types.cot", .test_count = 46 },
    .{ .path = "test/e2e/memory.cot", .test_count = 17 },
    .{ .path = "test/e2e/stdlib.cot", .test_count = 8 },
    .{ .path = "test/e2e/map.cot", .test_count = 25 },
    .{ .path = "test/e2e/auto_free.cot", .test_count = 5 },
    .{ .path = "test/e2e/set.cot", .test_count = 10 },
    .{ .path = "test/e2e/string_interp.cot", .test_count = 10 },
    .{ .path = "test/e2e/wasi_io.cot", .test_count = 15 },
    // cases/
    .{ .path = "test/cases/arithmetic.cot", .test_count = 10 },
    .{ .path = "test/cases/arrays.cot", .test_count = 6 },
    .{ .path = "test/cases/bitwise.cot", .test_count = 6 },
    .{ .path = "test/cases/builtins.cot", .test_count = 4 },
    .{ .path = "test/cases/chars.cot", .test_count = 2 },
    .{ .path = "test/cases/compound.cot", .test_count = 8 },
    .{ .path = "test/cases/control_flow.cot", .test_count = 14 },
    .{ .path = "test/cases/enum.cot", .test_count = 2 },
    .{ .path = "test/cases/extern.cot", .test_count = 1 },
    .{ .path = "test/cases/float.cot", .test_count = 1 },
    .{ .path = "test/cases/functions.cot", .test_count = 16 },
    .{ .path = "test/cases/loops.cot", .test_count = 3 },
    .{ .path = "test/cases/memory.cot", .test_count = 5 },
    .{ .path = "test/cases/methods.cot", .test_count = 1 },
    .{ .path = "test/cases/optional.cot", .test_count = 3 },
    .{ .path = "test/cases/strings.cot", .test_count = 13 },
    .{ .path = "test/cases/structs.cot", .test_count = 5 },
    .{ .path = "test/cases/switch.cot", .test_count = 2 },
    .{ .path = "test/cases/types.cot", .test_count = 2 },
    .{ .path = "test/cases/union.cot", .test_count = 4 },
    .{ .path = "test/cases/arc.cot", .test_count = 5 },
};

const total_test_count: u32 = blk: {
    var sum: u32 = 0;
    for (batch_files) |f| sum += f.test_count;
    break :blk sum;
};

// ============================================================================
// Combined source builder
// ============================================================================

/// Build combined source from all test files.
/// Reads files from disk, strips import lines, deduplicates them, prepends at top, concatenates all bodies.
fn buildCombinedSource(allocator: std.mem.Allocator) ![]const u8 {
    var result = std.ArrayListUnmanaged(u8){};
    errdefer result.deinit(allocator);

    // Read all file contents
    var contents = std.ArrayListUnmanaged([]const u8){};
    defer contents.deinit(allocator);

    for (batch_files) |spec| {
        const content = std.fs.cwd().readFileAlloc(allocator, spec.path, 1024 * 1024) catch |e| {
            std.debug.print("Failed to read {s}: {any}\n", .{ spec.path, e });
            return error.CompileError;
        };
        try contents.append(allocator, content);
    }

    // Collect unique imports
    var imports = std.StringHashMap(void).init(allocator);
    defer imports.deinit();

    for (contents.items) |content| {
        var lines = std.mem.splitScalar(u8, content, '\n');
        while (lines.next()) |line| {
            const trimmed = std.mem.trimLeft(u8, line, " \t");
            if (std.mem.startsWith(u8, trimmed, "import ")) {
                try imports.put(trimmed, {});
            }
        }
    }

    // Write imports at top
    var import_iter = imports.keyIterator();
    while (import_iter.next()) |key| {
        try result.appendSlice(allocator, key.*);
        try result.append(allocator, '\n');
    }
    try result.append(allocator, '\n');

    // Write file bodies (without import lines)
    for (contents.items) |content| {
        var lines = std.mem.splitScalar(u8, content, '\n');
        while (lines.next()) |line| {
            const trimmed = std.mem.trimLeft(u8, line, " \t");
            if (std.mem.startsWith(u8, trimmed, "import ")) continue;
            try result.appendSlice(allocator, line);
            try result.append(allocator, '\n');
        }
        try result.append(allocator, '\n');
    }

    return result.toOwnedSlice(allocator);
}

// ============================================================================
// Batch test: ALL test files compiled together (1 compile, 1 link, 1 run)
// ============================================================================

test "all native tests (734 tests)" {
    var timer = std.time.Timer.start() catch {
        std.debug.print("[native] all tests (batch)...", .{});
        return runBatchTest(std.testing.allocator);
    };

    std.debug.print("[native] all tests (batch)...", .{});

    try runBatchTest(std.testing.allocator);

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    if (elapsed_ms >= 1000.0) {
        std.debug.print("ok ({d:.0}ms) SLOW\n", .{elapsed_ms});
    } else {
        std.debug.print("ok ({d:.0}ms)\n", .{elapsed_ms});
    }
}

fn runBatchTest(backing_allocator: std.mem.Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const tmp_dir = "/tmp/cot_native_test";
    std.fs.cwd().makePath(tmp_dir) catch {};

    // Build combined source
    const combined = try buildCombinedSource(allocator);
    const cot_path = "/tmp/cot_native_test/batch.cot";
    std.fs.cwd().writeFile(.{ .sub_path = cot_path, .data = combined }) catch
        return error.CompileError;

    // Compile
    const obj_path = "/tmp/cot_native_test/batch.o";
    const exe_path = "/tmp/cot_native_test/batch";

    var driver = Driver.init(allocator);
    driver.setTarget(Target.native());
    driver.setTestMode(true);

    const obj_code = driver.compileFile(cot_path) catch |e| {
        std.debug.print("COMPILE ERROR: {any}\n", .{e});
        return error.CompileError;
    };
    defer allocator.free(obj_code);

    std.fs.cwd().writeFile(.{ .sub_path = obj_path, .data = obj_code }) catch
        return error.CompileError;

    // Link
    const link_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "zig", "cc", "-o", exe_path, obj_path },
    }) catch return error.LinkError;
    defer allocator.free(link_result.stdout);
    defer allocator.free(link_result.stderr);

    if (link_result.term.Exited != 0) {
        if (link_result.stderr.len > 0) std.debug.print("LINKER STDERR: {s}\n", .{link_result.stderr});
        return error.LinkError;
    }

    // Run
    const run_result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{exe_path},
        .max_output_bytes = 256 * 1024,
    }) catch return error.RunError;

    switch (run_result.term) {
        .Exited => |exit_code| {
            if (exit_code != 0) {
                std.debug.print("WRONG EXIT CODE: expected 0, got {d}\nSTDERR (last 2000 chars):\n{s}\n", .{
                    exit_code,
                    if (run_result.stderr.len > 2000) run_result.stderr[run_result.stderr.len - 2000 ..] else run_result.stderr,
                });
                return error.WrongExitCode;
            }

            // Verify pass count
            const expected_summary = std.fmt.allocPrint(allocator, "\n{d} passed\n", .{total_test_count}) catch
                return error.CompileError;
            if (!std.mem.containsAtLeast(u8, run_result.stderr, 1, expected_summary)) {
                std.debug.print("WRONG SUMMARY: expected \"{s}\" in stderr\nSTDERR (last 2000 chars):\n{s}\n", .{
                    expected_summary,
                    if (run_result.stderr.len > 2000) run_result.stderr[run_result.stderr.len - 2000 ..] else run_result.stderr,
                });
                return error.WrongOutput;
            }
        },
        .Signal => |sig| {
            std.debug.print("SIGNAL: {d}\n", .{sig});
            return error.RunError;
        },
        else => return error.RunError,
    }
}

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
        \\fn main() i64 {
        \\    var msg = "OK"
        \\    var n = @fd_write(1, @ptrOf(msg), @lenOf(msg))
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
        \\fn main() i64 {
        \\    var buf = @alloc(16)
        \\    var n = @fd_read(0, buf, 5)
        \\    @fd_write(1, buf, n)
        \\    @dealloc(buf)
        \\    return 0
        \\}
    , "hello", 0, "hello", "fd_read_stdin");
}

// fd_close: close stderr (fd 2), verify no crash, write still works on stdout
// Reference: Wasmtime p1_close_preopen.rs — close fd, verify program continues
test "native: fd_close valid fd" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    var msg = "OK"
        \\    var r = @fd_close(2)
        \\    @fd_write(1, @ptrOf(msg), @lenOf(msg))
        \\    return 0
        \\}
    , 0, "OK", "fd_close_valid");
}

// fd_open: verify we can open /dev/null and get a valid fd
// Simplified to avoid register pressure in main() mode
test "native: fd_open dev null" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    var path = "/dev/null"
        \\    var fd = @fd_open(@ptrOf(path), @lenOf(path), 0)
        \\    @fd_close(fd)
        \\    return fd
        \\}
    , 3, "", "fd_open_devnull");
}

// @time() — verify it returns a positive value (nanoseconds since epoch)
test "native: time returns positive" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    var t = @time()
        \\    if t > 0 {
        \\        return 42
        \\    }
        \\    return 1
        \\}
    , 42, "", "time_positive");
}

// @random(buf, len) — verify it fills buffer and returns 0
test "native: random returns success" {
    try expectOutput(std.testing.allocator,
        \\fn main() i64 {
        \\    var buf = @alloc(16)
        \\    var result = @random(buf, 16)
        \\    @dealloc(buf)
        \\    return result
        \\}
    , 0, "", "random_success");
}

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

    if (!std.mem.eql(u8, result.stderr, expected_stderr)) {
        std.debug.print("WRONG STDERR:\n  expected: \"{s}\" ({d} bytes)\n  actual:   \"{s}\" ({d} bytes)\n", .{
            expected_stderr, expected_stderr.len,
            result.stderr,   result.stderr.len,
        });
        return error.WrongOutput;
    }
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
        "test \"math\" ... ok\ntest \"bool\" ... ok\n\n2 passed\n",
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
        "test \"pass\" ... ok\ntest \"fail\" ... FAIL\ntest \"also pass\" ... ok\n\n2 passed, 1 failed\n",
        "test_one_fail");
}

test "native: test mode - assert_eq" {
    try expectTestMode(std.testing.allocator,
        \\test "eq" {
        \\    @assert_eq(42, 42)
        \\}
    , 0,
        "test \"eq\" ... ok\n\n1 passed\n",
        "test_assert_eq");
}
