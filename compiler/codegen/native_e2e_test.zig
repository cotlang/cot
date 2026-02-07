//! End-to-end Native AOT compilation tests.
//!
//! Tests the full pipeline: Cot source -> Wasm -> CLIF -> Machine Code -> Executable -> Run
//!
//! The main test compiles a single combined program (test/native/e2e_all.cot) containing
//! all 60 sub-tests. One compile + one link = ~500ms total (vs ~30s with individual tests).
//! Returns 0 on success or a unique error code identifying which sub-test failed.
//!
//! Parity tests (expressions, functions, control_flow, variables) remain as separate entries.

const std = @import("std");
const Driver = @import("../driver.zig").Driver;
const Target = @import("../core/target.zig").Target;

const NativeResult = struct {
    exit_code: ?u32,
    compile_error: bool,
    link_error: bool,
    run_error: bool,
    error_msg: []const u8,

    pub fn success(code: u32) NativeResult {
        return .{ .exit_code = code, .compile_error = false, .link_error = false, .run_error = false, .error_msg = "" };
    }
    pub fn compileErr(msg: []const u8) NativeResult {
        return .{ .exit_code = null, .compile_error = true, .link_error = false, .run_error = false, .error_msg = msg };
    }
    pub fn linkErr(msg: []const u8) NativeResult {
        return .{ .exit_code = null, .compile_error = false, .link_error = true, .run_error = false, .error_msg = msg };
    }
    pub fn runErr(msg: []const u8) NativeResult {
        return .{ .exit_code = null, .compile_error = false, .link_error = false, .run_error = true, .error_msg = msg };
    }
};

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
    };
    if (exit_code < names.len) return names[exit_code];
    return "unknown (exit code out of range)";
}

// ============================================================================
// Combined native E2E test: all 60 sub-tests in one program
// ============================================================================

test "native: all e2e tests (60 sub-tests)" {
    const code = @constCast(@as([]const u8, std.fs.cwd().readFileAlloc(std.testing.allocator, "test/native/e2e_all.cot", 1024 * 1024) catch |e| {
        std.debug.print("Failed to read test file: {any}\n", .{e});
        return error.FileNotFound;
    }));
    defer std.testing.allocator.free(code);
    try expectExitCode(std.testing.allocator, code, 0, "all_e2e");
}

// ============================================================================
// Parity tests: ported from bootstrap-0.2 (compound test files)
// ============================================================================

test "parity: expressions (160 tests)" {
    const code = @constCast(@as([]const u8, std.fs.cwd().readFileAlloc(std.testing.allocator, "test/parity/expressions.cot", 1024 * 1024) catch |e| {
        std.debug.print("Failed to read test file: {any}\n", .{e});
        return error.FileNotFound;
    }));
    defer std.testing.allocator.free(code);
    try expectExitCode(std.testing.allocator, code, 0, "parity_expressions");
}

test "parity: functions (110 tests)" {
    const code = @constCast(@as([]const u8, std.fs.cwd().readFileAlloc(std.testing.allocator, "test/parity/functions.cot", 1024 * 1024) catch |e| {
        std.debug.print("Failed to read test file: {any}\n", .{e});
        return error.FileNotFound;
    }));
    defer std.testing.allocator.free(code);
    try expectExitCode(std.testing.allocator, code, 0, "parity_functions");
}

test "parity: control_flow (80 tests)" {
    const code = @constCast(@as([]const u8, std.fs.cwd().readFileAlloc(std.testing.allocator, "test/parity/control_flow.cot", 1024 * 1024) catch |e| {
        std.debug.print("Failed to read test file: {any}\n", .{e});
        return error.FileNotFound;
    }));
    defer std.testing.allocator.free(code);
    try expectExitCode(std.testing.allocator, code, 0, "parity_control_flow");
}

test "parity: variables (40 tests)" {
    const code = @constCast(@as([]const u8, std.fs.cwd().readFileAlloc(std.testing.allocator, "test/parity/variables.cot", 1024 * 1024) catch |e| {
        std.debug.print("Failed to read test file: {any}\n", .{e});
        return error.FileNotFound;
    }));
    defer std.testing.allocator.free(code);
    try expectExitCode(std.testing.allocator, code, 0, "parity_variables");
}
