//! End-to-end Wasm compilation tests.
//!
//! Tests the full pipeline: Cot source → Parse → Check → IR → SSA → Wasm lowering → Wasm bytecode
//!
//! M4: "Return 42" test
//! M5: "Add two numbers" test

const std = @import("std");
const ast = @import("../frontend/ast.zig");
const parser_mod = @import("../frontend/parser.zig");
const scanner_mod = @import("../frontend/scanner.zig");
const checker = @import("../frontend/checker.zig");
const types = @import("../frontend/types.zig");
const fe_errors = @import("../frontend/errors.zig");
const source_mod = @import("../frontend/source.zig");
const ir = @import("../frontend/ir.zig");
const lower = @import("../frontend/lower.zig");
const ssa_builder = @import("../frontend/ssa_builder.zig");
const lower_wasm = @import("../ssa/passes/lower_wasm.zig");
const schedule = @import("../ssa/passes/schedule.zig");
const wasm = @import("wasm.zig");
const wasm_gen = @import("wasm_gen.zig");
const arc = @import("arc.zig");

const TypeRegistry = types.TypeRegistry;
const Func = ssa_builder.Func;
const ValType = wasm.ValType;

/// Result of compiling Cot source to Wasm.
const WasmResult = struct {
    arena: std.heap.ArenaAllocator,
    has_errors: bool,
    wasm_bytes: []const u8,

    pub fn deinit(self: *WasmResult) void {
        self.arena.deinit();
    }
};

/// Compile Cot source code to Wasm binary.
fn compileToWasm(backing: std.mem.Allocator, code: []const u8) !WasmResult {
    var arena = std.heap.ArenaAllocator.init(backing);
    const allocator = arena.allocator();
    errdefer arena.deinit();

    // Parse
    var src = source_mod.Source.init(allocator, "test.cot", code);
    var err = fe_errors.ErrorReporter.init(&src, null);
    var scan = scanner_mod.Scanner.initWithErrors(&src, &err);
    var tree = ast.Ast.init(allocator);
    var parser = parser_mod.Parser.init(allocator, &scan, &tree, &err);
    parser.parseFile() catch {
        return .{ .arena = arena, .has_errors = true, .wasm_bytes = &.{} };
    };
    if (err.hasErrors()) {
        return .{ .arena = arena, .has_errors = true, .wasm_bytes = &.{} };
    }

    // Type check
    var type_reg = try TypeRegistry.init(allocator);
    var global_scope = checker.Scope.init(allocator, null);
    var check = checker.Checker.init(allocator, &tree, &type_reg, &err, &global_scope);
    check.checkFile() catch {
        return .{ .arena = arena, .has_errors = true, .wasm_bytes = &.{} };
    };
    if (err.hasErrors()) {
        return .{ .arena = arena, .has_errors = true, .wasm_bytes = &.{} };
    }

    // Lower to IR
    var lowering = lower.Lowerer.init(allocator, &tree, &type_reg, &err, &check);
    const ir_data = lowering.lower() catch {
        return .{ .arena = arena, .has_errors = true, .wasm_bytes = &.{} };
    };
    if (err.hasErrors()) {
        return .{ .arena = arena, .has_errors = true, .wasm_bytes = &.{} };
    }

    // Build Wasm module
    var module = wasm.Module.init(allocator);

    // Add memory for linear memory (1 page = 64KB)
    module.addMemory(1, null);

    // Add stack pointer global (index 0, expected by wasm_gen)
    // SP starts at 65536 (top of first memory page, grows down)
    _ = try module.addGlobal(.i32, true, 65536);

    // Add ARC runtime functions (they get indices 0-4)
    // Note: heap_ptr global will be at index 1
    const runtime_funcs = try arc.addRuntimeFunctions(&module);

    // Count runtime functions for index offset
    // Runtime functions: alloc, retain, dealloc, release, retain_count, is_unique = 6 functions
    const runtime_func_count: u32 = 6;

    // First pass: build function name -> index mapping (offset by runtime functions)
    var func_indices = wasm_gen.FuncIndexMap{};
    for (ir_data.funcs, 0..) |*ir_func, i| {
        try func_indices.put(allocator, ir_func.name, @as(u32, @intCast(i)) + runtime_func_count);
    }

    // Second pass: process each function
    for (ir_data.funcs) |*ir_func| {
        // Build SSA
        var builder = try ssa_builder.SSABuilder.init(allocator, ir_func, &type_reg);
        const ssa_func = builder.build() catch |e| {
            std.debug.print("SSA build error for {s}: {}\n", .{ ir_func.name, e });
            builder.deinit();
            continue;
        };
        defer {
            ssa_func.deinit();
            allocator.destroy(ssa_func);
        }
        builder.deinit();

        // Schedule (order values for codegen)
        try schedule.schedule(ssa_func);

        // Lower to Wasm ops
        try lower_wasm.lower(ssa_func);

        // Determine function signature
        const param_count: u32 = @intCast(ir_func.params.len);
        var params: [16]ValType = undefined;
        for (ir_func.params, 0..) |param, i| {
            const is_float = param.type_idx == types.TypeRegistry.F64 or
                param.type_idx == types.TypeRegistry.F32;
            params[i] = if (is_float) .f64 else .i64;
        }
        const has_return = ir_func.return_type != types.TypeRegistry.VOID;
        const ret_is_float = ir_func.return_type == types.TypeRegistry.F64 or
            ir_func.return_type == types.TypeRegistry.F32;
        const results: []const ValType = if (!has_return)
            &[_]ValType{}
        else if (ret_is_float)
            &[_]ValType{.f64}
        else
            &[_]ValType{.i64};

        // Add type
        const type_idx = try module.addFuncType(params[0..param_count], results);

        // Add function
        const func_idx = try module.addFunc(type_idx);

        // Export if it's "main" or "answer" or "add"
        if (std.mem.eql(u8, ir_func.name, "main") or
            std.mem.eql(u8, ir_func.name, "answer") or
            std.mem.eql(u8, ir_func.name, "add"))
        {
            try module.addExport(ir_func.name, .func, func_idx);
        }

        // Generate code with function index resolution and ARC runtime
        const body = try wasm_gen.genFuncWithIndices(allocator, ssa_func, &func_indices, runtime_funcs);
        try module.addCode(body);
    }

    // Emit Wasm binary
    var output: std.ArrayListUnmanaged(u8) = .{};
    try module.emit(output.writer(allocator));
    const wasm_bytes = try output.toOwnedSlice(allocator);

    return .{ .arena = arena, .has_errors = false, .wasm_bytes = wasm_bytes };
}

fn countParams(ssa_func: *const Func) u32 {
    var max_arg: u32 = 0;
    for (ssa_func.blocks.items) |block| {
        for (block.values.items) |v| {
            if (v.op == .arg) {
                const arg_idx: u32 = @intCast(v.aux_int);
                if (arg_idx >= max_arg) max_arg = arg_idx + 1;
            }
        }
    }
    return max_arg;
}

// ============================================================================
// Tests
// ============================================================================

test "M4: return 42" {
    const code =
        \\fn answer() int {
        \\    return 42;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);

    // Verify Wasm header
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
    try std.testing.expectEqual(@as(u8, 1), result.wasm_bytes[4]); // version

    // The module should contain:
    // - Type section (function signature)
    // - Function section
    // - Export section (for "answer")
    // - Code section (i64.const 42, end)
    try std.testing.expect(result.wasm_bytes.len >= 20);
}

test "M5: add two numbers" {
    const code =
        \\fn add(a: int, b: int) int {
        \\    return a + b;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);

    // Verify Wasm header
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "compile simple expression" {
    const code =
        \\fn expr() int {
        \\    return 10 + 20;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "compile with multiplication" {
    const code =
        \\fn mul(x: int, y: int) int {
        \\    return x * y;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "compile void function" {
    const code =
        \\fn noop() {
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "M6: if statement" {
    const code =
        \\fn max(a: int, b: int) int {
        \\    if a > b {
        \\        return a;
        \\    } else {
        \\        return b;
        \\    }
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);

    // Verify Wasm header
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "M6: simple conditional return" {
    const code =
        \\fn sign(x: int) int {
        \\    if x > 0 { return 1; }
        \\    if x < 0 { return -1; }
        \\    return 0;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "M7: simple while loop" {
    const code =
        \\fn sum_to(n: int) int {
        \\    let i = 0;
        \\    let total = 0;
        \\    while i < n {
        \\        i = i + 1;
        \\        total = total + i;
        \\    }
        \\    return total;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);

    // Verify Wasm header
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "M8: function calls between functions" {
    const code =
        \\fn double(x: int) int {
        \\    return x + x;
        \\}
        \\
        \\fn quadruple(x: int) int {
        \\    return double(double(x));
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);

    // Verify Wasm header
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "M8: recursive function call" {
    const code =
        \\fn factorial(n: int) int {
        \\    if n <= 1 { return 1; }
        \\    return n * factorial(n - 1);
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

// ============================================================================
// P1: Control Flow Completeness (Parity with bootstrap-0.2)
// ============================================================================

test "P1.1: break in while loop" {
    const code =
        \\fn find_threshold(limit: int) int {
        \\    var i = 0;
        \\    var sum = 0;
        \\    while i < 100 {
        \\        sum = sum + i;
        \\        if sum > limit {
        \\            break;
        \\        }
        \\        i = i + 1;
        \\    }
        \\    return i;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "P1.1: break in nested loops" {
    const code =
        \\fn nested_break() int {
        \\    var count = 0;
        \\    var i = 0;
        \\    while i < 10 {
        \\        var j = 0;
        \\        while j < 10 {
        \\            count = count + 1;
        \\            if j > 3 {
        \\                break;
        \\            }
        \\            j = j + 1;
        \\        }
        \\        i = i + 1;
        \\    }
        \\    return count;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "P1.2: continue in while loop" {
    const code =
        \\fn sum_odd(n: int) int {
        \\    var i = 0;
        \\    var sum = 0;
        \\    while i < n {
        \\        i = i + 1;
        \\        if i % 2 == 0 {
        \\            continue;
        \\        }
        \\        sum = sum + i;
        \\    }
        \\    return sum;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "P1.2: continue in nested loops" {
    const code =
        \\fn skip_evens() int {
        \\    var total = 0;
        \\    var i = 0;
        \\    while i < 5 {
        \\        i = i + 1;
        \\        var j = 0;
        \\        while j < 5 {
        \\            j = j + 1;
        \\            if j % 2 == 0 {
        \\                continue;
        \\            }
        \\            total = total + 1;
        \\        }
        \\    }
        \\    return total;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

// ============================================================================
// P2: Variables & Scope (Parity with bootstrap-0.2)
// ============================================================================

test "P2.1: global variable read" {
    const code =
        \\var counter: int = 42;
        \\
        \\fn get_counter() int {
        \\    return counter;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "P2.1: global variable write" {
    const code =
        \\var counter: int = 0;
        \\
        \\fn increment() int {
        \\    counter = counter + 1;
        \\    return counter;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "P2.1: global used in multiple functions" {
    const code =
        \\var total: int = 0;
        \\
        \\fn add_to_total(x: int) {
        \\    total = total + x;
        \\}
        \\
        \\fn get_total() int {
        \\    return total;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

// ============================================================================
// P3: Type System (Parity with bootstrap-0.2)
// ============================================================================

test "P3.3: float arithmetic" {
    const code =
        \\fn add_floats(a: float, b: float) float {
        \\    return a + b;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "P3.3: float comparison" {
    const code =
        \\fn max_float(a: float, b: float) float {
        \\    if a > b {
        \\        return a;
        \\    }
        \\    return b;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "P3.3: float constant" {
    const code =
        \\fn pi() float {
        \\    return 3.14159;
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

// ============================================================================
// M22: For-Range Loops
// ============================================================================

test "M22: for-range numeric sum" {
    const code =
        \\fn main() i64 {
        \\    var sum: i64 = 0
        \\    for i in 0..5 {
        \\        sum = sum + i
        \\    }
        \\    return sum
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "M22: for-range with index binding" {
    // for i, x in arr - tests the index_binding feature
    const code =
        \\fn main() i64 {
        \\    var arr = [10, 20, 30]
        \\    var sum: i64 = 0
        \\    for i, x in arr {
        \\        sum = sum + i
        \\    }
        \\    return sum
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "M22: for-range index and value" {
    // Tests using both index and value in loop body
    const code =
        \\fn main() i64 {
        \\    var arr = [10, 20, 30]
        \\    var sum: i64 = 0
        \\    for i, x in arr {
        \\        sum = sum + i + x
        \\    }
        \\    return sum
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: union with payload" {
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: union mixed payload and unit variants" {
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: union switch with payload capture" {
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: error union catch" {
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: error union try propagation" {
    const code =
        \\const MyError = error { Fail }
        \\fn inner() MyError!i64 { return error.Fail }
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: defer basic" {
    // Defer with variable mutation — return captures value before defer runs
    const code =
        \\fn main() i64 {
        \\    var x: i64 = 10
        \\    defer x = 99
        \\    return x
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: defer LIFO ordering" {
    // Multiple defers execute in LIFO order
    const code =
        \\fn main() i64 {
        \\    var x: i64 = 0
        \\    defer x = x + 1
        \\    defer x = x * 10
        \\    return x
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: defer with loop break" {
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: ARC function returning new" {
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: ARC copy from local" {
    const code =
        \\struct Foo { x: i64 }
        \\fn main() i64 {
        \\    let p = new Foo { x: 10 }
        \\    let q = p
        \\    return q.x
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: ARC reassignment" {
    const code =
        \\struct Foo { x: i64 }
        \\fn main() i64 {
        \\    var p = new Foo { x: 1 }
        \\    let q = new Foo { x: 2 }
        \\    p = q
        \\    return p.x
        \\}
    ;

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

test "wasm e2e: ARC return forwarding" {
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

    var result = try compileToWasm(std.testing.allocator, code);
    defer result.deinit();

    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
}

// ============================================================================
// Function pointer tests (via Driver path for full table support)
// ============================================================================

const Driver = @import("../driver.zig").Driver;
const Target = @import("../core/target.zig").Target;

fn compileToWasmViaDriver(backing_allocator: std.mem.Allocator, code: []const u8) !WasmResult {
    var arena = std.heap.ArenaAllocator.init(backing_allocator);
    const allocator = arena.allocator();
    errdefer arena.deinit();

    var driver = Driver.init(allocator);
    driver.setTarget(.{ .arch = .wasm32, .os = .freestanding });
    const wasm_bytes = driver.compileSource(code) catch |e| {
        std.debug.print("compile error: {any}\n", .{e});
        return .{ .arena = arena, .has_errors = true, .wasm_bytes = &.{} };
    };
    return .{ .arena = arena, .has_errors = false, .wasm_bytes = wasm_bytes };
}

test "wasm e2e: function pointer basic" {
    const code =
        \\fn add(a: i64, b: i64) i64 {
        \\    return a + b
        \\}
        \\fn main() i64 {
        \\    let f = add
        \\    return f(3, 4)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: function pointer as parameter" {
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
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: function pointer reassignment" {
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
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: closure no capture" {
    const code =
        \\fn main() i64 {
        \\    let f = fn(x: i64) i64 { return x * 2 }
        \\    return f(21)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: closure basic capture" {
    const code =
        \\fn main() i64 {
        \\    let x: i64 = 10
        \\    let f = fn(y: i64) i64 { return x + y }
        \\    return f(5)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: closure multiple captures" {
    const code =
        \\fn main() i64 {
        \\    let a: i64 = 3
        \\    let b: i64 = 7
        \\    let f = fn(x: i64) i64 { return a + b + x }
        \\    return f(10)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: closure passed to function" {
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
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: generic function basic" {
    const code =
        \\fn max(T)(a: T, b: T) T {
        \\    if a > b { return a }
        \\    return b
        \\}
        \\fn main() i64 {
        \\    return max(i64)(3, 7)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: generic struct basic" {
    const code =
        \\struct Pair(T, U) { first: T, second: U }
        \\fn main() i64 {
        \\    var p: Pair(i64, i64) = undefined
        \\    p.first = 10
        \\    p.second = 20
        \\    return p.first + p.second
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: generic function multiple instantiations" {
    const code =
        \\fn add(T)(a: T, b: T) T {
        \\    return a + b
        \\}
        \\fn main() i64 {
        \\    let x: i64 = add(i64)(10, 20)
        \\    let y: i32 = add(i32)(3, 4)
        \\    return x + y
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

// ============================================================================
// ARC deallocation tests
// ============================================================================

test "wasm e2e: ARC dealloc after release" {
    const code =
        \\struct Foo { x: i64 }
        \\fn main() i64 {
        \\    let p = new Foo { x: 42 }
        \\    return p.x
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: ARC dealloc multiple objects" {
    const code =
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
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

// ============================================================================
// Builtin @alloc, @dealloc, @realloc tests
// ============================================================================

test "wasm e2e: builtin alloc and dealloc" {
    const code =
        \\fn main() i64 {
        \\    let ptr = @alloc(8)
        \\    @dealloc(ptr)
        \\    return 42
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: builtin realloc" {
    const code =
        \\fn main() i64 {
        \\    let ptr = @alloc(8)
        \\    let ptr2 = @realloc(ptr, 16)
        \\    @dealloc(ptr2)
        \\    return 99
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: freelist reuse cycle" {
    const code =
        \\fn main() i64 {
        \\    let p1 = @alloc(8)
        \\    @dealloc(p1)
        \\    let p2 = @alloc(8)
        \\    @dealloc(p2)
        \\    let p3 = @alloc(8)
        \\    @dealloc(p3)
        \\    return 77
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: deinit basic" {
    // Struct with deinit — destructor called on release, returns p.x
    const code =
        \\struct Foo { x: i64 }
        \\fn Foo_deinit(self: *Foo) void {
        \\    return
        \\}
        \\fn main() i64 {
        \\    let p = new Foo { x: 42 }
        \\    return p.x
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: deinit no use after new" {
    // Struct with deinit — release without using the value
    const code =
        \\struct Foo { x: i64 }
        \\fn Foo_deinit(self: *Foo) void {
        \\    return
        \\}
        \\fn main() i64 {
        \\    let p = new Foo { x: 42 }
        \\    return 99
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: deinit with no-deinit struct" {
    // Mix: Foo has deinit, Bar does not
    const code =
        \\struct Foo { x: i64 }
        \\struct Bar { y: i64 }
        \\fn Foo_deinit(self: *Foo) void {
        \\    return
        \\}
        \\fn main() i64 {
        \\    let f = new Foo { x: 10 }
        \\    let b = new Bar { y: 20 }
        \\    return f.x + b.y
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: deinit alloc reuse after release" {
    const code =
        \\struct Foo { x: i64 }
        \\fn Foo_deinit(self: *Foo) void {
        \\    return
        \\}
        \\fn createFoo(val: i64) *Foo {
        \\    return new Foo { x: val }
        \\}
        \\fn main() i64 {
        \\    let p1 = createFoo(10)
        \\    let v1 = p1.x
        \\    let p2 = createFoo(20)
        \\    return v1 + p2.x
        \\}
    ;

    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

// ============================================================================
// Generic building blocks tests (Phase 1 for List(T))
// ============================================================================

test "wasm e2e: generic function with generic struct param" {
    const code =
        \\struct Box(T) { value: T }
        \\fn Box_getValue(T)(self: *Box(T)) T {
        \\    return self.value
        \\}
        \\fn main() i64 {
        \\    var b: Box(i64) = undefined
        \\    b.value = 42
        \\    return Box_getValue(i64)(&b)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: sizeOf in generic context" {
    const code =
        \\fn getSize(T)() i64 {
        \\    return @sizeOf(T)
        \\}
        \\fn main() i64 {
        \\    return getSize(i64)()
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: alloc intToPtr in generic context" {
    const code =
        \\fn allocOne(T)(value: T) *T {
        \\    let ptr = @intToPtr(*T, @alloc(@sizeOf(T)))
        \\    ptr.* = value
        \\    return ptr
        \\}
        \\fn main() i64 {
        \\    let p = allocOne(i64)(42)
        \\    let result = p.*
        \\    @dealloc(@ptrToInt(p))
        \\    return result
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: field mutation on generic struct via pointer" {
    const code =
        \\struct Counter(T) { value: T, count: i64 }
        \\fn Counter_increment(T)(self: *Counter(T)) void {
        \\    self.count = self.count + 1
        \\}
        \\fn main() i64 {
        \\    var c: Counter(i64) = undefined
        \\    c.value = 10
        \\    c.count = 0
        \\    Counter_increment(i64)(&c)
        \\    Counter_increment(i64)(&c)
        \\    return c.count
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

// ============================================================================
// List(T) stdlib tests
// ============================================================================

test "wasm e2e: list basic" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\fn List_ensureCapacity(T)(self: *List(T), needed: i64) void {
        \\    if self.capacity >= needed { return }
        \\    var new_cap: i64 = 8
        \\    if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\    let bytes = new_cap * @sizeOf(T)
        \\    if self.capacity == 0 {
        \\        self.items = @alloc(bytes)
        \\    } else {
        \\        self.items = @realloc(self.items, bytes)
        \\    }
        \\    self.capacity = new_cap
        \\}
        \\fn List_append(T)(self: *List(T), value: T) void {
        \\    List_ensureCapacity(T)(self, self.count + 1)
        \\    let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\    ptr.* = value
        \\    self.count = self.count + 1
        \\}
        \\fn List_get(T)(self: *List(T), index: i64) T {
        \\    let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\    return ptr.*
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    List_append(i64)(&list, 10)
        \\    List_append(i64)(&list, 20)
        \\    List_append(i64)(&list, 30)
        \\    let a = List_get(i64)(&list, 0)
        \\    let b = List_get(i64)(&list, 1)
        \\    let c = List_get(i64)(&list, 2)
        \\    return a + b + c
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list growth" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\fn List_ensureCapacity(T)(self: *List(T), needed: i64) void {
        \\    if self.capacity >= needed { return }
        \\    var new_cap: i64 = 8
        \\    if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\    let bytes = new_cap * @sizeOf(T)
        \\    if self.capacity == 0 {
        \\        self.items = @alloc(bytes)
        \\    } else {
        \\        self.items = @realloc(self.items, bytes)
        \\    }
        \\    self.capacity = new_cap
        \\}
        \\fn List_append(T)(self: *List(T), value: T) void {
        \\    List_ensureCapacity(T)(self, self.count + 1)
        \\    let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\    ptr.* = value
        \\    self.count = self.count + 1
        \\}
        \\fn List_get(T)(self: *List(T), index: i64) T {
        \\    let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\    return ptr.*
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    var i: i64 = 0
        \\    while i < 20 {
        \\        List_append(i64)(&list, i)
        \\        i = i + 1
        \\    }
        \\    let first = List_get(i64)(&list, 0)
        \\    let last = List_get(i64)(&list, 19)
        \\    return first + last + list.count
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list pop" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\fn List_ensureCapacity(T)(self: *List(T), needed: i64) void {
        \\    if self.capacity >= needed { return }
        \\    var new_cap: i64 = 8
        \\    if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\    let bytes = new_cap * @sizeOf(T)
        \\    if self.capacity == 0 {
        \\        self.items = @alloc(bytes)
        \\    } else {
        \\        self.items = @realloc(self.items, bytes)
        \\    }
        \\    self.capacity = new_cap
        \\}
        \\fn List_append(T)(self: *List(T), value: T) void {
        \\    List_ensureCapacity(T)(self, self.count + 1)
        \\    let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\    ptr.* = value
        \\    self.count = self.count + 1
        \\}
        \\fn List_get(T)(self: *List(T), index: i64) T {
        \\    let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\    return ptr.*
        \\}
        \\fn List_pop(T)(self: *List(T)) T {
        \\    self.count = self.count - 1
        \\    let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\    return ptr.*
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    List_append(i64)(&list, 10)
        \\    List_append(i64)(&list, 20)
        \\    List_append(i64)(&list, 30)
        \\    let popped = List_pop(i64)(&list)
        \\    return popped + list.count
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list set" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\fn List_ensureCapacity(T)(self: *List(T), needed: i64) void {
        \\    if self.capacity >= needed { return }
        \\    var new_cap: i64 = 8
        \\    if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\    let bytes = new_cap * @sizeOf(T)
        \\    if self.capacity == 0 {
        \\        self.items = @alloc(bytes)
        \\    } else {
        \\        self.items = @realloc(self.items, bytes)
        \\    }
        \\    self.capacity = new_cap
        \\}
        \\fn List_append(T)(self: *List(T), value: T) void {
        \\    List_ensureCapacity(T)(self, self.count + 1)
        \\    let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\    ptr.* = value
        \\    self.count = self.count + 1
        \\}
        \\fn List_get(T)(self: *List(T), index: i64) T {
        \\    let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\    return ptr.*
        \\}
        \\fn List_set(T)(self: *List(T), index: i64, value: T) void {
        \\    let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\    ptr.* = value
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    List_append(i64)(&list, 10)
        \\    List_append(i64)(&list, 20)
        \\    List_append(i64)(&list, 30)
        \\    List_set(i64)(&list, 1, 50)
        \\    return List_get(i64)(&list, 0) + List_get(i64)(&list, 1) + List_get(i64)(&list, 2)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list multi type" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\fn List_ensureCapacity(T)(self: *List(T), needed: i64) void {
        \\    if self.capacity >= needed { return }
        \\    var new_cap: i64 = 8
        \\    if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\    let bytes = new_cap * @sizeOf(T)
        \\    if self.capacity == 0 {
        \\        self.items = @alloc(bytes)
        \\    } else {
        \\        self.items = @realloc(self.items, bytes)
        \\    }
        \\    self.capacity = new_cap
        \\}
        \\fn List_append(T)(self: *List(T), value: T) void {
        \\    List_ensureCapacity(T)(self, self.count + 1)
        \\    let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\    ptr.* = value
        \\    self.count = self.count + 1
        \\}
        \\fn List_get(T)(self: *List(T), index: i64) T {
        \\    let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\    return ptr.*
        \\}
        \\fn main() i64 {
        \\    var a: List(i64) = undefined
        \\    a.items = 0
        \\    a.count = 0
        \\    a.capacity = 0
        \\    var b: List(i32) = undefined
        \\    b.items = 0
        \\    b.count = 0
        \\    b.capacity = 0
        \\    List_append(i64)(&a, 100)
        \\    List_append(i32)(&b, 5)
        \\    let x = List_get(i64)(&a, 0)
        \\    let y: i64 = List_get(i32)(&b, 0)
        \\    return x + y
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

// ============================================================================
// List(T) with impl blocks (dot-call syntax)
// ============================================================================

test "wasm e2e: list impl basic" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\impl List(T) {
        \\    fn ensureCapacity(self: *List(T), needed: i64) void {
        \\        if self.capacity >= needed { return }
        \\        var new_cap: i64 = 8
        \\        if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\        let bytes = new_cap * @sizeOf(T)
        \\        if self.capacity == 0 {
        \\            self.items = @alloc(bytes)
        \\        } else {
        \\            self.items = @realloc(self.items, bytes)
        \\        }
        \\        self.capacity = new_cap
        \\    }
        \\    fn append(self: *List(T), value: T) void {
        \\        self.ensureCapacity(self.count + 1)
        \\        let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\        ptr.* = value
        \\        self.count = self.count + 1
        \\    }
        \\    fn get(self: *List(T), index: i64) T {
        \\        let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\        return ptr.*
        \\    }
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    list.append(10)
        \\    list.append(20)
        \\    list.append(30)
        \\    let a = list.get(0)
        \\    let b = list.get(1)
        \\    let c = list.get(2)
        \\    return a + b + c
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list impl growth" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\impl List(T) {
        \\    fn ensureCapacity(self: *List(T), needed: i64) void {
        \\        if self.capacity >= needed { return }
        \\        var new_cap: i64 = 8
        \\        if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\        let bytes = new_cap * @sizeOf(T)
        \\        if self.capacity == 0 {
        \\            self.items = @alloc(bytes)
        \\        } else {
        \\            self.items = @realloc(self.items, bytes)
        \\        }
        \\        self.capacity = new_cap
        \\    }
        \\    fn append(self: *List(T), value: T) void {
        \\        self.ensureCapacity(self.count + 1)
        \\        let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\        ptr.* = value
        \\        self.count = self.count + 1
        \\    }
        \\    fn get(self: *List(T), index: i64) T {
        \\        let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\        return ptr.*
        \\    }
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    var i: i64 = 0
        \\    while i < 20 {
        \\        list.append(i)
        \\        i = i + 1
        \\    }
        \\    let first = list.get(0)
        \\    let last = list.get(19)
        \\    return first + last + list.count
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list impl pop" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\impl List(T) {
        \\    fn ensureCapacity(self: *List(T), needed: i64) void {
        \\        if self.capacity >= needed { return }
        \\        var new_cap: i64 = 8
        \\        if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\        let bytes = new_cap * @sizeOf(T)
        \\        if self.capacity == 0 {
        \\            self.items = @alloc(bytes)
        \\        } else {
        \\            self.items = @realloc(self.items, bytes)
        \\        }
        \\        self.capacity = new_cap
        \\    }
        \\    fn append(self: *List(T), value: T) void {
        \\        self.ensureCapacity(self.count + 1)
        \\        let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\        ptr.* = value
        \\        self.count = self.count + 1
        \\    }
        \\    fn get(self: *List(T), index: i64) T {
        \\        let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\        return ptr.*
        \\    }
        \\    fn pop(self: *List(T)) T {
        \\        self.count = self.count - 1
        \\        let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\        return ptr.*
        \\    }
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    list.append(10)
        \\    list.append(20)
        \\    list.append(30)
        \\    let popped = list.pop()
        \\    return popped + list.count
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list impl set" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\impl List(T) {
        \\    fn ensureCapacity(self: *List(T), needed: i64) void {
        \\        if self.capacity >= needed { return }
        \\        var new_cap: i64 = 8
        \\        if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\        let bytes = new_cap * @sizeOf(T)
        \\        if self.capacity == 0 {
        \\            self.items = @alloc(bytes)
        \\        } else {
        \\            self.items = @realloc(self.items, bytes)
        \\        }
        \\        self.capacity = new_cap
        \\    }
        \\    fn append(self: *List(T), value: T) void {
        \\        self.ensureCapacity(self.count + 1)
        \\        let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\        ptr.* = value
        \\        self.count = self.count + 1
        \\    }
        \\    fn get(self: *List(T), index: i64) T {
        \\        let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\        return ptr.*
        \\    }
        \\    fn set(self: *List(T), index: i64, value: T) void {
        \\        let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\        ptr.* = value
        \\    }
        \\}
        \\fn main() i64 {
        \\    var list: List(i64) = undefined
        \\    list.items = 0
        \\    list.count = 0
        \\    list.capacity = 0
        \\    list.append(10)
        \\    list.append(20)
        \\    list.append(30)
        \\    list.set(1, 50)
        \\    return list.get(0) + list.get(1) + list.get(2)
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: list impl multi type" {
    const code =
        \\struct List(T) {
        \\    items: i64,
        \\    count: i64,
        \\    capacity: i64,
        \\}
        \\impl List(T) {
        \\    fn ensureCapacity(self: *List(T), needed: i64) void {
        \\        if self.capacity >= needed { return }
        \\        var new_cap: i64 = 8
        \\        if self.capacity > 0 { new_cap = self.capacity * 2 }
        \\        let bytes = new_cap * @sizeOf(T)
        \\        if self.capacity == 0 {
        \\            self.items = @alloc(bytes)
        \\        } else {
        \\            self.items = @realloc(self.items, bytes)
        \\        }
        \\        self.capacity = new_cap
        \\    }
        \\    fn append(self: *List(T), value: T) void {
        \\        self.ensureCapacity(self.count + 1)
        \\        let ptr = @intToPtr(*T, self.items + self.count * @sizeOf(T))
        \\        ptr.* = value
        \\        self.count = self.count + 1
        \\    }
        \\    fn get(self: *List(T), index: i64) T {
        \\        let ptr = @intToPtr(*T, self.items + index * @sizeOf(T))
        \\        return ptr.*
        \\    }
        \\}
        \\fn main() i64 {
        \\    var a: List(i64) = undefined
        \\    a.items = 0
        \\    a.count = 0
        \\    a.capacity = 0
        \\    var b: List(i32) = undefined
        \\    b.items = 0
        \\    b.count = 0
        \\    b.capacity = 0
        \\    a.append(100)
        \\    b.append(5)
        \\    let x = a.get(0)
        \\    let y: i64 = b.get(0)
        \\    return x + y
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

// ============================================================================
// Generic impl blocks
// ============================================================================

test "wasm e2e: generic impl basic" {
    const code =
        \\struct Counter(T) { value: T }
        \\impl Counter(T) {
        \\    fn get(self: *Counter(T)) T {
        \\        return self.value
        \\    }
        \\}
        \\fn main() i64 {
        \\    var c: Counter(i64) = undefined
        \\    c.value = 42
        \\    return c.get()
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: generic impl self call" {
    const code =
        \\struct Box(T) { value: T, count: i64 }
        \\impl Box(T) {
        \\    fn getCount(self: *Box(T)) i64 {
        \\        return self.count
        \\    }
        \\    fn increment(self: *Box(T)) void {
        \\        self.count = self.getCount() + 1
        \\    }
        \\}
        \\fn main() i64 {
        \\    var b: Box(i64) = undefined
        \\    b.value = 10
        \\    b.count = 0
        \\    b.increment()
        \\    b.increment()
        \\    b.increment()
        \\    return b.count + b.value
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: generic impl multiple instantiations" {
    const code =
        \\struct Box(T) { value: T }
        \\impl Box(T) {
        \\    fn get(self: *Box(T)) T {
        \\        return self.value
        \\    }
        \\}
        \\fn main() i64 {
        \\    var a: Box(i64) = undefined
        \\    a.value = 30
        \\    var b: Box(i32) = undefined
        \\    b.value = 12
        \\    let x: i64 = a.get()
        \\    let y: i64 = b.get()
        \\    return x + y
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: generic impl forward ref" {
    // Tests that methods can call sibling methods defined LATER in the impl block.
    // This catches the single-pass bug where registration and checking happen in one loop.
    const code =
        \\struct Box(T) { value: T, count: i64 }
        \\impl Box(T) {
        \\    fn increment(self: *Box(T)) void {
        \\        self.count = self.getCount() + 1
        \\    }
        \\    fn getCount(self: *Box(T)) i64 {
        \\        return self.count
        \\    }
        \\}
        \\fn main() i64 {
        \\    var b: Box(i64) = undefined
        \\    b.value = 10
        \\    b.count = 0
        \\    b.increment()
        \\    b.increment()
        \\    return b.count + b.value
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}

test "wasm e2e: generic impl two type params" {
    const code =
        \\struct Pair(T, U) { first: T, second: U }
        \\impl Pair(T, U) {
        \\    fn sum(self: *Pair(T, U)) i64 {
        \\        return self.first + self.second
        \\    }
        \\}
        \\fn main() i64 {
        \\    var p: Pair(i64, i64) = undefined
        \\    p.first = 30
        \\    p.second = 12
        \\    return p.sum()
        \\}
    ;
    var result = try compileToWasmViaDriver(std.testing.allocator, code);
    defer result.deinit();
    try std.testing.expect(!result.has_errors);
    try std.testing.expect(result.wasm_bytes.len > 0);
    try std.testing.expectEqualSlices(u8, "\x00asm", result.wasm_bytes[0..4]);
}
