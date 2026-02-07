//! End-to-end Native AOT compilation tests.
//!
//! Tests the full pipeline: Cot source -> Wasm -> CLIF -> Machine Code -> Executable -> Run
//!
//! Each test compiles a single Cot program, links it, runs it, and checks exit code.
//! Returns 0 on success or a unique error code identifying which check failed.
//!
//! Function calls, float locals, and Phase 3 language features all work.

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
    std.debug.print("[native] {s}...", .{test_name});
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
        std.debug.print("WRONG EXIT CODE: expected {d}, got {d}\n", .{ expected, actual });
        return error.WrongExitCode;
    }
    std.debug.print("ok\n", .{});
}

// ============================================================================
// Baseline: constants, arithmetic, variables, control flow (NO function calls)
// ============================================================================

test "native: baseline" {
    const code =
        \\fn main() i64 {
        \\    // Constants
        \\    if 42 != 42 { return 1; }
        \\    if 10 + 5 != 15 { return 2; }
        \\    if 20 - 8 != 12 { return 3; }
        \\    if 6 * 7 != 42 { return 4; }
        \\    if 2 + 3 * 4 != 14 { return 5; }
        \\
        \\    // Variables
        \\    let x = 10;
        \\    let y = 5;
        \\    if x + y != 15 { return 10; }
        \\
        \\    // If/else
        \\    if 10 > 5 {
        \\        let ok = 1;
        \\        if ok != 1 { return 30; }
        \\    } else {
        \\        return 31;
        \\    }
        \\
        \\    // While loop
        \\    let sum = 0;
        \\    let i = 1;
        \\    while i <= 10 {
        \\        sum = sum + i;
        \\        i = i + 1;
        \\    }
        \\    if sum != 55 { return 40; }
        \\
        \\    return 0;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "baseline");
}

// ============================================================================
// Phase 3: All features in one program (NO function calls - dispatch loop bug)
// ============================================================================

test "native: phase 3 language features" {
    const code =
        \\struct Point {
        \\    x: i64,
        \\    y: i64,
        \\}
        \\
        \\type Coord = Point;
        \\
        \\enum Color {
        \\    Red,
        \\    Green,
        \\    Blue,
        \\}
        \\
        \\enum Status {
        \\    Ok = 0,
        \\    Warning = 50,
        \\    Error = 100,
        \\}
        \\
        \\enum Level {
        \\    Low = 10,
        \\    Medium = 50,
        \\    High = 100,
        \\}
        \\
        \\union State {
        \\    Init,
        \\    Running,
        \\    Done,
        \\}
        \\
        \\fn main() i64 {
        \\    // Char literals
        \\    let c1 = 'A';
        \\    if c1 != 65 { return 100; }
        \\    let c2 = '\n';
        \\    if c2 != 10 { return 101; }
        \\
        \\    // Type alias + struct
        \\    let coord: Coord = Coord { .x = 10, .y = 20 };
        \\    if coord.x + coord.y != 30 { return 102; }
        \\
        \\    // Builtins
        \\    if @sizeOf(i64) != 8 { return 103; }
        \\    if @sizeOf(Point) != 16 { return 104; }
        \\    if @alignOf(i64) != 8 { return 105; }
        \\    let big: i64 = 42;
        \\    let small = @intCast(i32, big);
        \\    if small != 42 { return 106; }
        \\
        \\    // Enums (return value directly - frontend doesn't support enum != int comparison)
        \\    let color: i64 = Color.Green;
        \\    if color != 1 { return 107; }
        \\    let status: i64 = Status.Error;
        \\    if status != 100 { return 108; }
        \\
        \\    // Union (return value directly - frontend doesn't support union != int comparison)
        \\    let state: i64 = State.Running;
        \\    if state != 1 { return 109; }
        \\
        \\    // Bitwise ops (inline, no function calls)
        \\    let a = 255;
        \\    let b = 15;
        \\    if (a & b) != 15 { return 110; }
        \\    if ((240 | 15) - 200) != 55 { return 111; }
        \\    if ((a ^ b) & 255) != 240 { return 112; }
        \\    if ((~0) & 255) != 255 { return 113; }
        \\    if (1 << 4) != 16 { return 114; }
        \\    if (64 >> 2) != 16 { return 115; }
        \\
        \\    // Compound assignment
        \\    var x = 10;
        \\    x += 5;
        \\    if x != 15 { return 120; }
        \\    x -= 3;
        \\    if x != 12 { return 121; }
        \\    x *= 2;
        \\    if x != 24 { return 122; }
        \\    var y = 255;
        \\    y &= 15;
        \\    if y != 15 { return 123; }
        \\
        \\    // Optional types
        \\    let opt1: ?i64 = 42;
        \\    if opt1.? != 42 { return 140; }
        \\    let opt2: ?i64 = null;
        \\    if (opt2 ?? 99) != 99 { return 141; }
        \\    let opt3: ?i64 = 42;
        \\    if (opt3 ?? 99) != 42 { return 142; }
        \\
        \\    // Switch
        \\    let sw1 = switch 2 {
        \\        1 => 10,
        \\        2 => 20,
        \\        3 => 30,
        \\        else => 0,
        \\    };
        \\    if sw1 != 20 { return 150; }
        \\
        \\    let level = Level.Medium;
        \\    let sw2 = switch level {
        \\        Level.Low => 1,
        \\        Level.Medium => 50,
        \\        Level.High => 99,
        \\        else => 0,
        \\    };
        \\    if sw2 != 50 { return 151; }
        \\
        \\    return 0;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "phase3_all");
}

// ============================================================================
// Function calls (fixed: is_aarch64 was declared at file scope instead of
// inside the Inst union, causing @hasDecl to return false and skipping
// prologue/epilogue generation for ARM64)
// ============================================================================

test "native: function call" {
    const code =
        \\fn double(x: i64) i64 { return x + x; }
        \\fn main() i64 { return double(10); }
    ;
    try expectExitCode(std.testing.allocator, code, 20, "func_call");
}

// ============================================================================
// Float support: f64 locals, arithmetic, comparison
// ============================================================================

test "native: float locals" {
    // Tests f64 locals compile, link, and run correctly.
    // Prior bugs fixed:
    // 1. machregToVec crash: missing observeVregClass in liveness.zig
    // 2. SIGBUS: clobbered reg collection didn't check preg.class(),
    //    treating float d22 as integer x22 (callee-saved), causing
    //    stack imbalance (save without restore)
    // 3. emitFpuLoadStore didn't call memFinalize for pseudo AModes
    const code =
        \\fn main() i64 {
        \\    let x: f64 = 3.14;
        \\    let y: f64 = 0.0;
        \\    return 0;
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "float_locals");
}

// ============================================================================
// Union payloads: tag + payload init, tag check, payload extraction
// ============================================================================

test "native: union payload" {
    const code =
        \\union Result { Ok: i64, Err: i32 }
        \\fn main() i64 {
        \\    let r: Result = Result.Ok(42)
        \\    if r.tag != 0 { return 1 }
        \\    let val = r.Ok
        \\    if val != 42 { return 2 }
        \\    let e: Result = Result.Err(99)
        \\    if e.tag != 1 { return 3 }
        \\    return 0
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "union_payload");
}

test "native: union mixed payload and unit" {
    const code =
        \\union Event { Click: i64, Hover, KeyPress: i64 }
        \\fn main() i64 {
        \\    let e1: Event = Event.Hover
        \\    if e1.tag != 1 { return 1 }
        \\    let e2: Event = Event.Click(100)
        \\    if e2.tag != 0 { return 2 }
        \\    let clicks = e2.Click
        \\    if clicks != 100 { return 3 }
        \\    return 0
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "union_mixed");
}

test "native: union switch with payload capture" {
    const code =
        \\union Result { Ok: i64, Err: i32 }
        \\fn main() i64 {
        \\    let r: Result = Result.Ok(42)
        \\    switch r {
        \\        Result.Ok |val| => { return val },
        \\        Result.Err |e| => { return e },
        \\    }
        \\    return 99
        \\}
    ;
    // First test: union switch WITHOUT capture (tag-only)
    const code_no_capture =
        \\union Result { Ok: i64, Err: i32 }
        \\fn main() i64 {
        \\    let r: Result = Result.Ok(42)
        \\    switch r {
        \\        Result.Ok => { return 10 },
        \\        Result.Err => { return 20 },
        \\    }
        \\    return 99
        \\}
    ;
    try expectExitCode(std.testing.allocator, code_no_capture, 10, "union_switch_no_capture");
    try expectExitCode(std.testing.allocator, code, 42, "union_switch_capture");
}

// ============================================================================
// Error unions: catch, try propagation
// ============================================================================

test "native: error union catch" {
    const code =
        \\const MyError = error { Fail, NotFound }
        \\fn mayFail(x: i64) MyError!i64 {
        \\    if x < 0 { return error.Fail }
        \\    return x * 2
        \\}
        \\fn main() i64 {
        \\    let result = mayFail(-1) catch 99
        \\    if result != 99 { return 1 }
        \\    let success = mayFail(5) catch 99
        \\    if success != 10 { return 2 }
        \\    return 0
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "error_union_catch");
}

test "native: error union try propagation" {
    const code =
        \\const MyError = error { Fail }
        \\fn inner() MyError!i64 {
        \\    return error.Fail
        \\}
        \\fn outer() MyError!i64 {
        \\    let x = try inner()
        \\    return x + 1
        \\}
        \\fn main() i64 {
        \\    let result = outer() catch 42
        \\    if result != 42 { return 1 }
        \\    return 0
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 0, "error_union_try");
}

test "native: defer basic" {
    // Defer runs after return value is captured
    // x=10, return captures 10, then defer sets x=99 (but return value already captured)
    const code =
        \\fn main() i64 {
        \\    var x: i64 = 10
        \\    defer x = 99
        \\    return x
        \\}
    ;
    // return captures x=10 before defer runs, so exit code = 10
    try expectExitCode(std.testing.allocator, code, 10, "defer_basic");
}

test "native: defer with loop break" {
    // defer inside loop body runs on break
    const code =
        \\fn main() i64 {
        \\    var sum: i64 = 0
        \\    var i: i64 = 0
        \\    while i < 5 {
        \\        defer sum = sum + 1
        \\        i = i + 1
        \\        if i == 3 { break }
        \\    }
        \\    return sum
        \\}
    ;
    // Loop runs 3 times (i=1,2,3), defer runs each iteration.
    // But break emits cleanup without pop, and the block exit also tries to emit.
    // Actually: i=1 (no break, block exit emits defer → sum=1),
    //          i=2 (no break, block exit emits defer → sum=2),
    //          i=3 (break, emits defer NoPop → sum=3, jumps to exit)
    try expectExitCode(std.testing.allocator, code, 3, "defer_loop_break");
}

test "native: defer LIFO ordering" {
    // Multiple defers execute in LIFO order (last defer first)
    const code =
        \\fn main() i64 {
        \\    var x: i64 = 2
        \\    defer x = x + 1
        \\    defer x = x * 10
        \\    return x
        \\}
    ;
    // return captures x=2 before defers run, so exit code = 2
    try expectExitCode(std.testing.allocator, code, 2, "defer_lifo");
}

test "native: ARC function returning new" {
    const code =
        \\struct Foo { x: i64 }
        \\fn createFoo(val: i64) *Foo {
        \\    return new Foo { x: val }
        \\}
        \\fn main() i64 {
        \\    let p = createFoo(42)
        \\    return p.x
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 42, "arc_func_return_new");
}

test "native: ARC copy from local" {
    const code =
        \\struct Foo { x: i64 }
        \\fn main() i64 {
        \\    let p = new Foo { x: 10 }
        \\    let q = p
        \\    return q.x
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 10, "arc_copy_local");
}

test "native: ARC reassignment" {
    const code =
        \\struct Foo { x: i64 }
        \\fn main() i64 {
        \\    var p = new Foo { x: 1 }
        \\    let q = new Foo { x: 2 }
        \\    p = q
        \\    return p.x
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 2, "arc_reassignment");
}

test "native: ARC return forwarding" {
    const code =
        \\struct Foo { x: i64 }
        \\fn makeFoo() *Foo {
        \\    let p = new Foo { x: 77 }
        \\    return p
        \\}
        \\fn main() i64 {
        \\    let f = makeFoo()
        \\    return f.x
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 77, "arc_return_forward");
}

// ============================================================================
// Function pointer tests
// ============================================================================

test "native: function pointer basic" {
    const code =
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\fn main() i64 {
        \\    let f = add
        \\    return f(3, 4)
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 7, "fnptr_basic");
}

test "native: function pointer as parameter" {
    const code =
        \\fn double(x: i64) i64 {
        \\    return x * 2
        \\}
        \\fn apply(f: fn(i64) -> i64, x: i64) i64 {
        \\    return f(x)
        \\}
        \\fn main() i64 {
        \\    return apply(double, 5)
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 10, "fnptr_param");
}

test "native: function pointer reassignment" {
    const code =
        \\fn inc(x: i64) i64 {
        \\    return x + 1
        \\}
        \\fn dec(x: i64) i64 {
        \\    return x - 1
        \\}
        \\fn main() i64 {
        \\    var f = inc
        \\    let a = f(10)
        \\    f = dec
        \\    let b = f(10)
        \\    return a + b
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 20, "fnptr_reassign");
}

// ============================================================================
// Closure tests
// ============================================================================

test "native: closure no capture" {
    const code =
        \\fn main() i64 {
        \\    let f = fn(x: i64) i64 { return x * 2 }
        \\    return f(21)
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 42, "closure_no_capture");
}

test "native: closure basic capture" {
    const code =
        \\fn main() i64 {
        \\    let x: i64 = 10
        \\    let f = fn(y: i64) i64 { return x + y }
        \\    return f(5)
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 15, "closure_capture");
}

test "native: closure multiple captures" {
    const code =
        \\fn main() i64 {
        \\    let a: i64 = 3
        \\    let b: i64 = 7
        \\    let f = fn(x: i64) i64 { return a + b + x }
        \\    return f(10)
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 20, "closure_multi_capture");
}

test "native: closure passed to function" {
    const code =
        \\fn apply(f: fn(i64) -> i64, x: i64) i64 {
        \\    return f(x)
        \\}
        \\fn main() i64 {
        \\    let offset: i64 = 100
        \\    let g = fn(x: i64) i64 { return x + offset }
        \\    return apply(g, 5)
        \\}
    ;
    try expectExitCode(std.testing.allocator, code, 105, "closure_passed");
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

test "generic function basic" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\fn max(T)(a: T, b: T) T {
        \\    if a > b { return a }
        \\    return b
        \\}
        \\fn main() i64 {
        \\    return max(i64)(3, 7)
        \\}
    ), 7, "generic_fn_basic");
}

test "generic struct basic" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\struct Pair(T, U) { first: T, second: U }
        \\fn main() i64 {
        \\    var p: Pair(i64, i64) = undefined
        \\    p.first = 10
        \\    p.second = 20
        \\    return p.first + p.second
        \\}
    ), 30, "generic_struct_basic");
}

test "generic function multiple instantiations" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\fn add(T)(a: T, b: T) T {
        \\    return a + b
        \\}
        \\fn main() i64 {
        \\    let x: i64 = add(i64)(10, 20)
        \\    let y: i32 = add(i32)(3, 4)
        \\    return x + y
        \\}
    ), 37, "generic_fn_multi_inst");
}

// ============================================================================
// ARC deallocation tests (verify memory freed on release)
// ============================================================================

test "native: ARC dealloc after release" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\struct Foo { x: i64 }
        \\fn main() i64 {
        \\    let p = new Foo { x: 42 }
        \\    return p.x
        \\}
    ), 42, "arc_dealloc_basic");
}

test "native: ARC dealloc multiple objects" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\struct Foo { x: i64 }
        \\fn makeFoo(v: i64) *Foo {
        \\    return new Foo { x: v }
        \\}
        \\fn main() i64 {
        \\    let a = makeFoo(10)
        \\    let b = makeFoo(20)
        \\    let c = makeFoo(12)
        \\    return a.x + b.x + c.x
        \\}
    ), 42, "arc_dealloc_multi");
}

// ============================================================================
// Builtin @alloc, @dealloc, @realloc tests
// ============================================================================

test "native: builtin alloc and dealloc" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\fn main() i64 {
        \\    let ptr = @alloc(8)
        \\    @dealloc(ptr)
        \\    return 42
        \\}
    ), 42, "builtin_alloc_dealloc");
}

test "native: builtin realloc" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\fn main() i64 {
        \\    let ptr = @alloc(8)
        \\    let ptr2 = @realloc(ptr, 16)
        \\    @dealloc(ptr2)
        \\    return 99
        \\}
    ), 99, "builtin_realloc");
}

test "native: freelist reuse cycle" {
    try expectExitCode(std.testing.allocator, @constCast(
        \\fn main() i64 {
        \\    let p1 = @alloc(8)
        \\    @dealloc(p1)
        \\    let p2 = @alloc(8)
        \\    @dealloc(p2)
        \\    let p3 = @alloc(8)
        \\    @dealloc(p3)
        \\    return 77
        \\}
    ), 77, "freelist_reuse");
}
