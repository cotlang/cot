//! End-to-end Native AOT compilation tests.
//!
//! Tests the full pipeline: Cot source -> Wasm -> CLIF -> Machine Code -> Executable -> Run
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

/// Compile a .cot file (supports imports) and run the resulting executable.
fn compileAndRunFile(allocator: std.mem.Allocator, file_path: []const u8, test_name: []const u8) NativeResult {
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

    const obj_code = driver.compileFile(file_path) catch |e| {
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
    defer allocator.free(run_result.stdout);
    defer allocator.free(run_result.stderr);

    return switch (run_result.term) {
        .Exited => |exit_code| NativeResult.success(exit_code),
        .Signal => |sig| blk: {
            const msg = std.fmt.allocPrint(allocator, "signal {d}", .{sig}) catch "signal";
            break :blk NativeResult.runErr(msg);
        },
        else => NativeResult.runErr("unknown termination"),
    };
}

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
    // stdout ownership transferred to NativeResult (arena-allocated)

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

fn expectFileExitCode(backing_allocator: std.mem.Allocator, file_path: []const u8, expected: u32, test_name: []const u8) !void {
    var timer = std.time.Timer.start() catch {
        std.debug.print("[native] {s}...", .{test_name});
        return expectFileExitCodeInner(backing_allocator, file_path, expected, test_name);
    };

    std.debug.print("[native] {s}...", .{test_name});

    try expectFileExitCodeInner(backing_allocator, file_path, expected, test_name);

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    if (elapsed_ms >= 1000.0) {
        std.debug.print("ok ({d:.0}ms) SLOW\n", .{elapsed_ms});
    } else {
        std.debug.print("ok ({d:.0}ms)\n", .{elapsed_ms});
    }
}

fn expectFileExitCodeInner(backing_allocator: std.mem.Allocator, file_path: []const u8, expected: u32, test_name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = compileAndRunFile(allocator, file_path, test_name);

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

    const actual = result.exit_code orelse return error.NoExitCode;
    if (actual != expected) {
        std.debug.print("WRONG EXIT CODE: expected {d}, got {d}\n", .{ expected, actual });
        return error.WrongExitCode;
    }
}

fn expectExitCode(backing_allocator: std.mem.Allocator, code: []const u8, expected: u32, test_name: []const u8) !void {
    var timer = std.time.Timer.start() catch {
        std.debug.print("[native] {s}...", .{test_name});
        return expectExitCodeInner(backing_allocator, code, expected, test_name);
    };

    std.debug.print("[native] {s}...", .{test_name});

    try expectExitCodeInner(backing_allocator, code, expected, test_name);

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    if (elapsed_ms >= 1000.0) {
        std.debug.print("ok ({d:.0}ms) SLOW\n", .{elapsed_ms});
    } else {
        std.debug.print("ok ({d:.0}ms)\n", .{elapsed_ms});
    }
}

fn expectExitCodeInner(backing_allocator: std.mem.Allocator, code: []const u8, expected: u32, test_name: []const u8) !void {
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

    const actual = result.exit_code orelse return error.NoExitCode;
    if (actual != expected) {
        const name = subTestName(actual);
        std.debug.print("WRONG EXIT CODE: expected {d}, got {d} ({s})\n", .{ expected, actual, name });
        return error.WrongExitCode;
    }
}

/// Maps exit codes from e2e_all.cot main() to sub-test names.
/// Exit code N means sub-test N failed. 0 = all pass.
fn subTestName(exit_code: u32) []const u8 {
    const names = [_][]const u8{
        "unknown",                          // 0 (success)
        "test_baseline",                    // 1
        "test_phase3",                      // 2
        "test_func_call",                   // 3
        "test_float_locals",                // 4
        "test_union_payload",               // 5
        "test_union_mixed",                 // 6
        "test_union_switch_no_capture",     // 7
        "test_union_switch_capture",        // 8
        "test_error_union_catch",           // 9
        "test_error_union_try",             // 10
        "test_defer_basic",                 // 11
        "test_defer_loop_break",            // 12
        "test_defer_lifo",                  // 13
        "test_arc_func_return_new",         // 14
        "test_arc_copy_local",              // 15
        "test_arc_reassignment",            // 16
        "test_arc_return_forward",          // 17
        "test_arc_dealloc_basic",           // 18
        "test_arc_dealloc_multi",           // 19
        "test_builtin_alloc_dealloc",       // 20
        "test_builtin_realloc",             // 21
        "test_freelist_reuse",              // 22
        "test_deinit_basic",                // 23
        "test_deinit_no_use",               // 24
        "test_deinit_mixed",                // 25
        "test_deinit_alloc_reuse",          // 26
        "test_fnptr_basic",                 // 27
        "test_fnptr_param",                 // 28
        "test_fnptr_reassign",              // 29
        "test_closure_no_capture",          // 30
        "test_closure_capture",             // 31
        "test_closure_multi_capture",       // 32
        "test_closure_passed",              // 33
        "test_generic_fn_basic",            // 34
        "test_generic_struct_basic",        // 35
        "test_generic_fn_multi_inst",       // 36
        "test_generic_struct_param",        // 37
        "test_sizeof_generic",              // 38
        "test_alloc_inttoptr_generic",      // 39
        "test_generic_field_mutation",      // 40
        "test_list_basic",                  // 41
        "test_list_growth",                 // 42
        "test_list_pop",                    // 43
        "test_list_set",                    // 44
        "test_list_multi_type",             // 45
        "test_list_impl_basic",             // 46
        "test_list_impl_growth",            // 47
        "test_list_impl_pop",              // 48
        "test_list_impl_set",               // 49
        "test_list_impl_multi_type",        // 50
        "test_generic_impl_basic",          // 51
        "test_generic_impl_self_call",      // 52
        "test_generic_impl_multi_inst",     // 53
        "test_generic_impl_forward_ref",    // 54
        "test_generic_impl_two_params",     // 55
        "test_generic_struct_literal",      // 56
        "test_generic_struct_literal_methods", // 57
        "test_zero_init_basic",             // 58
        "test_zero_init_generic",           // 59
        "test_new_generic",                 // 60
        "test_memcpy_basic",                // 61
        "test_memcpy_zero_length",          // 62
        "test_trap_not_reached",            // 63
        "test_slice_param_basic",           // 64
        "test_slice_param_iteration",       // 65
        "test_trait_basic",                 // 66
        "test_trait_primitive",             // 67
        "test_trait_multi_impl",            // 68
        "test_trait_generic_usage",         // 69
        "test_trait_self_type",             // 70
        "test_const_eval_arithmetic",       // 71
        "test_const_eval_sizeof",           // 72
        "test_trait_bound_basic",           // 73
        "test_trait_bound_multi",           // 74
        "test_match_wildcard",              // 75
        "test_match_guard",                 // 76
        "test_match_range",                 // 77
        "test_tuple_basic",                 // 78
        "test_tuple_nested",               // 79
        "test_tuple_three",                 // 80
        "test_sret_tuple_return",           // 81
        "test_sret_struct_return",          // 82
        "test_sret_chain",                  // 83
        "test_list_bounds_get",             // 84
        "test_list_growth_go",              // 85
        "test_list_free",                   // 86
        "test_list_clear",                  // 87
        "test_list_insert",                 // 88
        "test_list_ordered_remove",         // 89
        "test_list_swap_remove",            // 90
        "test_list_reverse",                // 91
        "test_list_clone",                  // 92
        "test_list_last",                   // 93
        "test_memcpy_overlap",              // 94
        "test_list_contains",               // 95
        "test_list_index_of",               // 96
        "test_list_equal",                  // 97
        "test_list_first",                  // 98
        "test_list_resize",                 // 99
        "test_list_append_n_times",         // 100
        "test_list_insert_slice",           // 101
        "test_list_replace_range",          // 102
        "test_list_shrink_and_free",        // 103
        "test_list_delete_range",           // 104
        "test_list_ensure_unused",          // 105
        "test_list_is_empty",              // 106
        "test_list_compact",                // 107
    };
    if (exit_code < names.len) return names[exit_code];
    return "unknown (exit code out of range)";
}

// ============================================================================
// E2E tests: comprehensive feature tests (test/e2e/)
// ============================================================================

test "e2e: features (107 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/features.cot", 107, "all_e2e");
}

test "e2e: expressions (160 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/expressions.cot", 160, "expressions");
}

test "e2e: functions (107 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/functions.cot", 107, "functions");
}

test "e2e: control_flow (82 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/control_flow.cot", 82, "control_flow");
}

test "e2e: variables (40 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/variables.cot", 40, "variables");
}

test "e2e: types (46 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/types.cot", 46, "types");
}

test "e2e: memory (17 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/memory.cot", 17, "memory");
}

test "e2e: stdlib imports (5 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/stdlib.cot", 5, "import_list");
}

test "e2e: map (25 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/map.cot", 25, "e2e_map");
}

test "e2e: auto_free (5 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/auto_free.cot", 5, "e2e_auto_free");
}

test "e2e: set (10 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/set.cot", 10, "e2e_set");
}

test "e2e: string_interp (10 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/e2e/string_interp.cot", 10, "e2e_string_interp");
}

// ============================================================================
// Print tests: verify print/println produce correct stdout output
// ============================================================================

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

// ============================================================================
// Test Mode (cot test) E2E Tests
// ============================================================================

/// Compile source in test mode and run. Returns NativeResult with stderr captured.
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
    // stdout and stderr ownership transferred to NativeResult (arena-allocated)

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

/// Compile a .cot file in test mode and run. Returns NativeResult with stderr captured.
fn compileAndRunTestModeFile(allocator: std.mem.Allocator, file_path: []const u8, test_name: []const u8) NativeResult {
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

    const obj_code = driver.compileFile(file_path) catch |e| {
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

/// Compile a .cot file in test mode, expect exit code 0 and verify summary contains expected pass count.
fn expectTestModeFile(backing_allocator: std.mem.Allocator, file_path: []const u8, expected_pass_count: u32, test_name: []const u8) !void {
    var timer = std.time.Timer.start() catch {
        std.debug.print("[native] {s}...", .{test_name});
        return expectTestModeFileInner(backing_allocator, file_path, expected_pass_count, test_name);
    };

    std.debug.print("[native] {s}...", .{test_name});

    try expectTestModeFileInner(backing_allocator, file_path, expected_pass_count, test_name);

    const elapsed_ns = timer.read();
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    if (elapsed_ms >= 1000.0) {
        std.debug.print("ok ({d:.0}ms) SLOW\n", .{elapsed_ms});
    } else {
        std.debug.print("ok ({d:.0}ms)\n", .{elapsed_ms});
    }
}

fn expectTestModeFileInner(backing_allocator: std.mem.Allocator, file_path: []const u8, expected_pass_count: u32, test_name: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const result = compileAndRunTestModeFile(allocator, file_path, test_name);

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
    if (actual_exit != 0) {
        std.debug.print("WRONG EXIT CODE: expected 0, got {d}\nSTDERR: {s}\n", .{ actual_exit, result.stderr });
        return error.WrongExitCode;
    }

    // Verify summary line contains expected pass count
    const expected_summary = std.fmt.allocPrint(allocator, "\n{d} passed\n", .{expected_pass_count}) catch
        return error.CompileError;
    if (!std.mem.containsAtLeast(u8, result.stderr, 1, expected_summary)) {
        std.debug.print("WRONG SUMMARY: expected \"{s}\" in stderr\nSTDERR: {s}\n", .{ expected_summary, result.stderr });
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

// ============================================================================
// Inline test files: test/cases/ (converted from orphaned exit-code tests)
// ============================================================================

test "cases: arithmetic (10 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/arithmetic.cot", 10, "cases_arithmetic");
}

test "cases: arrays (6 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/arrays.cot", 6, "cases_arrays");
}

test "cases: bitwise (6 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/bitwise.cot", 6, "cases_bitwise");
}

test "cases: builtins (4 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/builtins.cot", 4, "cases_builtins");
}

test "cases: chars (2 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/chars.cot", 2, "cases_chars");
}

test "cases: compound (8 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/compound.cot", 8, "cases_compound");
}

test "cases: control flow (14 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/control_flow.cot", 14, "cases_control_flow");
}

test "cases: enum (2 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/enum.cot", 2, "cases_enum");
}

test "cases: extern (1 test)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/extern.cot", 1, "cases_extern");
}

test "cases: float (1 test)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/float.cot", 1, "cases_float");
}

test "cases: functions (16 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/functions.cot", 16, "cases_functions");
}

test "cases: loops (3 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/loops.cot", 3, "cases_loops");
}

test "cases: memory (5 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/memory.cot", 5, "cases_memory");
}

test "cases: methods (1 test)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/methods.cot", 1, "cases_methods");
}

test "cases: optional (3 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/optional.cot", 3, "cases_optional");
}

test "cases: strings (13 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/strings.cot", 13, "cases_strings");
}

test "cases: structs (5 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/structs.cot", 5, "cases_structs");
}

test "cases: switch (2 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/switch.cot", 2, "cases_switch");
}

test "cases: types (2 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/types.cot", 2, "cases_types");
}

test "cases: union (4 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/union.cot", 4, "cases_union");
}

test "cases: arc (5 tests)" {
    try expectTestModeFile(std.testing.allocator, "test/cases/arc.cot", 5, "cases_arc");
}
