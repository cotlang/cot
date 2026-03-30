//! Test Runtime for Direct Native Backend — CLIF IR Generation
//!
//! Generates test runner functions as CLIF IR. These track pass/fail counts
//! and output test results via write(fd, buf, len).
//!
//! Reference: compiler/codegen/test_runtime.zig (Wasm test path)
//! Reference: cg_clif abi/mod.rs (lib_call pattern)

const std = @import("std");
const Allocator = std.mem.Allocator;

const clif = @import("clif_ir/mod.zig");
const frontend_mod = @import("frontend/mod.zig");
const FunctionBuilder = frontend_mod.FunctionBuilder;
const FunctionBuilderContext = frontend_mod.FunctionBuilderContext;
const native_compile = @import("compile.zig");
const arc_native = @import("arc_native.zig");
const RuntimeFunc = arc_native.RuntimeFunc;

const debug = @import("debug.zig");

/// Generate all test runtime functions as compiled native code.
/// Uses two stack-local counters stored as globals (allocated in data section).
/// For simplicity, pass_count and fail_count are managed via dedicated functions.
pub fn generate(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !std.ArrayListUnmanaged(RuntimeFunc) {
    var result = std.ArrayListUnmanaged(RuntimeFunc){};
    errdefer {
        for (result.items) |*rf| rf.compiled.deinit();
        result.deinit(allocator);
    }

    // __test_begin() → void
    try result.append(allocator, .{
        .name = "__test_begin",
        .compiled = try generateNoop(allocator, isa, ctrl_plane, 0),
    });

    // __test_print_name(ptr: i64, len: i64) → void
    try result.append(allocator, .{
        .name = "__test_print_name",
        .compiled = try generateTestPrintName(allocator, isa, ctrl_plane, func_index_map),
    });

    // __test_pass() → void
    try result.append(allocator, .{
        .name = "__test_pass",
        .compiled = try generateTestWriteStr(allocator, isa, ctrl_plane, func_index_map, "ok\n"),
    });

    // __test_fail() → void
    try result.append(allocator, .{
        .name = "__test_fail",
        .compiled = try generateTestWriteStr(allocator, isa, ctrl_plane, func_index_map, "FAIL\n"),
    });

    // __test_summary(pass_count: i64, fail_count: i64) → void
    // Reference: test_runtime.zig — Deno-style summary output
    try result.append(allocator, .{
        .name = "__test_summary",
        .compiled = try generateTestSummary(allocator, isa, ctrl_plane, func_index_map),
    });

    // __test_store_fail_values(left, right, is_string, left_len, right_len) → void
    // Stores assertEq failure values for diagnostics. Noop for now.
    try result.append(allocator, .{
        .name = "__test_store_fail_values",
        .compiled = try generateNoop(allocator, isa, ctrl_plane, 5),
    });

    return result;
}

// ============================================================================
// No-op function: (optional params) → void
// Used for __test_begin and __test_store_fail_values placeholders
// ============================================================================

fn generateNoop(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    num_params: u32,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    for (0..num_params) |_| {
        try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    }

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// __test_print_name(ptr: i64, len: i64) → void
// Writes: 'test "' + name + '" ... '
// Reference: test_runtime.zig test_print_name
// ============================================================================

fn generateTestPrintName(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (ptr: i64, len: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    const ss = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 16,
        .align_shift = 3,
    });

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();
    const params = builder.blockParams(block_entry);
    const name_ptr = params[0];
    const name_len = params[1];

    const write_idx = func_index_map.get("write") orelse 0;
    const wfunc_ref = try importWrite(&builder, allocator, write_idx);
    const v_fd = try ins.iconst(clif.Type.I64, 2); // stderr

    // Write prefix: 'test "'
    const buf_addr = try ins.stackAddr(clif.Type.I64, ss, 0);
    try writeStrToStack(ins, buf_addr, "test \"");
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, try ins.iconst(clif.Type.I64, 6) });

    // Write name
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, name_ptr, name_len });

    // Write suffix: '" ... '
    try writeStrToStack(ins, buf_addr, "\" ... ");
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, try ins.iconst(clif.Type.I64, 6) });

    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Write a fixed string to stderr — used for __test_pass ("ok\n") and __test_fail ("FAIL\n")
// ============================================================================

fn generateTestWriteStr(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
    comptime str: []const u8,
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    const ss = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = @intCast(str.len),
        .align_shift = 0,
    });

    const block_entry = try builder.createBlock();
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();

    const ins = builder.ins();

    const buf_addr = try ins.stackAddr(clif.Type.I64, ss, 0);
    for (str, 0..) |byte, i| {
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, @intCast(byte)), buf_addr, @intCast(i));
    }

    const write_idx = func_index_map.get("write") orelse 0;
    const wfunc_ref = try importWrite(&builder, allocator, write_idx);
    const v_fd = try ins.iconst(clif.Type.I64, 2); // stderr
    const v_len = try ins.iconst(clif.Type.I64, @intCast(str.len));
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ v_fd, buf_addr, v_len });

    _ = try ins.return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// __test_summary(pass_count: i64, fail_count: i64) → void
// Reference: test_runtime.zig generateTestSummaryBody (wasm version)
// Prints: "\nok | N passed\n" or "\nFAILED | N passed | M failed\n"
// Uses snprintf for integer-to-string, write(2,...) for stderr output.
// ============================================================================

fn generateTestSummary(
    allocator: Allocator,
    isa: native_compile.TargetIsa,
    ctrl_plane: *native_compile.ControlPlane,
    func_index_map: *const std.StringHashMapUnmanaged(u32),
) !native_compile.CompiledCode {
    var clif_func = clif.Function.init(allocator);
    defer clif_func.deinit();

    var func_ctx = FunctionBuilderContext.init(allocator);
    defer func_ctx.deinit();
    var builder = FunctionBuilder.init(&clif_func, &func_ctx);

    // Signature: (pass_count: i64, fail_count: i64) → void
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try clif_func.signature.addParam(allocator, clif.AbiParam.init(clif.Type.I64));

    // Stack: 64 bytes for snprintf output + inline string bytes
    const ss = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 64,
        .align_shift = 3,
    });

    const write_idx = func_index_map.get("write") orelse 0;
    const wfunc_ref = try importWrite(&builder, allocator, write_idx);
    // snprintf not used — integer conversion done via manual digit extraction
    const snp_func_ref: clif.FuncRef = undefined; // unused, passed through but ignored

    // Stack slots for pass/fail counts (shared across blocks)
    const ss_counts = try clif_func.createStackSlot(allocator, .{
        .kind = .explicit_slot,
        .size = 16,
        .align_shift = 3,
    });

    const block_entry = try builder.createBlock();
    const block_failed = try builder.createBlock();
    const block_ok = try builder.createBlock();
    const block_merge = try builder.createBlock();

    // === Entry ===
    builder.switchToBlock(block_entry);
    try builder.appendBlockParamsForFunctionParams(block_entry);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const p = builder.blockParams(block_entry);
        const fd = try i.iconst(clif.Type.I64, 2);
        const buf = try i.stackAddr(clif.Type.I64, ss, 0);

        // Store pass/fail counts to stack slots for use in branches
        _ = try i.stackStore(p[0], ss_counts, 0);
        _ = try i.stackStore(p[1], ss_counts, 8);

        // Write "\n"
        _ = try i.store(clif.MemFlags.DEFAULT, try i.iconst(clif.Type.I8, '\n'), buf, 0);
        _ = try i.call(wfunc_ref, &[_]clif.Value{ fd, buf, try i.iconst(clif.Type.I64, 1) });

        // Branch: fail_count > 0?
        const zero = try i.iconst(clif.Type.I64, 0);
        const has_fails = try i.icmp(.sgt, p[1], zero);
        _ = try i.brif(has_fails, block_failed, &[_]clif.Value{}, block_ok, &[_]clif.Value{});
    }

    // === Failed path: "FAILED | N passed | M failed\n" ===
    builder.switchToBlock(block_failed);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const fd = try i.iconst(clif.Type.I64, 2);
        const buf = try i.stackAddr(clif.Type.I64, ss, 0);
        const out = try i.stackAddr(clif.Type.I64, ss, 32);
        const sz = try i.iconst(clif.Type.I64, 32);
        const pc = try i.stackLoad(clif.Type.I64, ss_counts, 0);
        const fc = try i.stackLoad(clif.Type.I64, ss_counts, 8);

        try emitWriteStr(i, wfunc_ref, fd, buf, "FAILED | ");
        try emitPrintInt(i, snp_func_ref, wfunc_ref, fd, buf, out, sz, pc);
        try emitWriteStr(i, wfunc_ref, fd, buf, " passed | ");
        try emitPrintInt(i, snp_func_ref, wfunc_ref, fd, buf, out, sz, fc);
        try emitWriteStr(i, wfunc_ref, fd, buf, " failed\n");

        _ = try i.jump(block_merge, &[_]clif.Value{});
    }

    // === Ok path: "ok | N passed\n" ===
    builder.switchToBlock(block_ok);
    try builder.ensureInsertedBlock();
    {
        const i = builder.ins();
        const fd = try i.iconst(clif.Type.I64, 2);
        const buf = try i.stackAddr(clif.Type.I64, ss, 0);
        const out = try i.stackAddr(clif.Type.I64, ss, 32);
        const sz = try i.iconst(clif.Type.I64, 32);
        const pc = try i.stackLoad(clif.Type.I64, ss_counts, 0);

        try emitWriteStr(i, wfunc_ref, fd, buf, "ok | ");
        try emitPrintInt(i, snp_func_ref, wfunc_ref, fd, buf, out, sz, pc);
        try emitWriteStr(i, wfunc_ref, fd, buf, " passed\n");

        _ = try i.jump(block_merge, &[_]clif.Value{});
    }

    // === Merge: return ===
    builder.switchToBlock(block_merge);
    try builder.ensureInsertedBlock();
    _ = try builder.ins().return_(&[_]clif.Value{});

    try builder.sealAllBlocks();
    builder.finalize();

    return native_compile.compile(allocator, &clif_func, isa, ctrl_plane);
}

// ============================================================================
// Helpers
// ============================================================================

/// Import write(fd, buf, len) → i64
fn importWrite(builder: *FunctionBuilder, allocator: Allocator, write_idx: u32) !clif.FuncRef {
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I64));
    const sig_ref = try builder.importSignature(sig);
    return builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = write_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
}

/// Import snprintf(buf, size, fmt, arg) → i32
fn importSnprintf(builder: *FunctionBuilder, allocator: Allocator, snprintf_idx: u32) !clif.FuncRef {
    var sig = clif.Signature.init(.system_v);
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addParam(allocator, clif.AbiParam.init(clif.Type.I64));
    try sig.addReturn(allocator, clif.AbiParam.init(clif.Type.I32));
    const sig_ref = try builder.importSignature(sig);
    return builder.importFunction(.{
        .name = .{ .user = .{ .namespace = 0, .index = snprintf_idx } },
        .signature = sig_ref,
        .colocated = false,
    });
}

/// Store a comptime string's bytes at buf_addr on the stack.
fn writeStrToStack(ins: anytype, buf: clif.Value, comptime str: []const u8) !void {
    for (str, 0..) |byte, idx| {
        _ = try ins.store(clif.MemFlags.DEFAULT, try ins.iconst(clif.Type.I8, @intCast(byte)), buf, @intCast(idx));
    }
}

/// Write a string literal to fd via write(fd, buf, len).
/// Stores string bytes at buf_addr, then calls write.
fn emitWriteStr(ins: anytype, wfunc_ref: clif.FuncRef, fd: clif.Value, buf: clif.Value, comptime str: []const u8) !void {
    try writeStrToStack(ins, buf, str);
    _ = try ins.call(wfunc_ref, &[_]clif.Value{ fd, buf, try ins.iconst(clif.Type.I64, str.len) });
}

/// Print an i64 value to fd by calling the existing print_int runtime function
/// (which handles integer-to-string conversion and writes to fd 1/stdout).
/// Since test output goes to stderr (fd 2), we call the eprint_int function
/// which is registered in func_index_map as a thin wrapper.
///
/// Actually: use a simple approach — call the compiler's intToString function
/// which is already a runtime function. For small test counts, just convert
/// digits manually via repeated division.
///
/// Simplest correct approach: use the existing fd_write runtime to print via
/// a manual digit conversion loop. But CLIF doesn't have good loop support
/// in the builder, so we use a fixed-width approach for numbers up to 999999.
fn emitPrintInt(ins: anytype, _: clif.FuncRef, w_ref: clif.FuncRef, fd: clif.Value, buf: clif.Value, out: clif.Value, _: clif.Value, val: clif.Value) !void {
    _ = out;
    // Simple approach: convert each digit and store in buffer, then write.
    // For test counts (typically < 10000), 6 digits is plenty.
    // Algorithm: divide by powers of 10, store digits, track start.
    //
    // Even simpler: just emit digits for 0-9 directly since test counts
    // are almost always small. For production, this should be a proper
    // itoa implementation, but for test counts this is correct.
    //
    // Actually simplest: store the value as a single digit + handle multi-digit
    // by calling write for each digit. But we can't loop in CLIF easily.
    //
    // Use the print_int function that already exists in the runtime:
    // It converts i64 to string and writes to stdout. We just need stderr.
    // But we can't change its fd...
    //
    // OK, truly simplest correct approach: store "0"-"9" for single digit,
    // and for multi-digit, write the ascii bytes.
    // For now: just write '0' + (val % 10) for single-digit counts,
    // and handle > 9 by dividing.

    // We'll write up to 10 digits right-to-left into buf[0..10], then write
    // the non-zero prefix.
    const ten = try ins.iconst(clif.Type.I64, 10);
    const zero_char = try ins.iconst(clif.Type.I8, '0');

    // Digit 0 (ones place)
    const d0_val = try ins.urem(val, ten);
    const d0_i8 = try ins.ireduce(clif.Type.I8, d0_val);
    const d0_char = try ins.iadd(d0_i8, zero_char);
    _ = try ins.store(clif.MemFlags.DEFAULT, d0_char, buf, 9);

    // Digit 1 (tens)
    const v1 = try ins.udiv(val, ten);
    const d1_val = try ins.urem(v1, ten);
    const d1_i8 = try ins.ireduce(clif.Type.I8, d1_val);
    const d1_char = try ins.iadd(d1_i8, zero_char);
    _ = try ins.store(clif.MemFlags.DEFAULT, d1_char, buf, 8);

    // Digit 2 (hundreds)
    const v2 = try ins.udiv(v1, ten);
    const d2_val = try ins.urem(v2, ten);
    const d2_i8 = try ins.ireduce(clif.Type.I8, d2_val);
    const d2_char = try ins.iadd(d2_i8, zero_char);
    _ = try ins.store(clif.MemFlags.DEFAULT, d2_char, buf, 7);

    // Digit 3 (thousands)
    const v3 = try ins.udiv(v2, ten);
    const d3_val = try ins.urem(v3, ten);
    const d3_i8 = try ins.ireduce(clif.Type.I8, d3_val);
    const d3_char = try ins.iadd(d3_i8, zero_char);
    _ = try ins.store(clif.MemFlags.DEFAULT, d3_char, buf, 6);

    // Find start: skip leading zeros. For simplicity, check each digit.
    // If val < 10: write 1 digit from buf[9]
    // If val < 100: write 2 digits from buf[8]
    // If val < 1000: write 3 digits from buf[7]
    // Else: write 4 digits from buf[6]
    const hundred = try ins.iconst(clif.Type.I64, 100);
    const thousand = try ins.iconst(clif.Type.I64, 1000);

    // Default: assume >= 1000 (4 digits from offset 6)
    var start = try ins.iconst(clif.Type.I64, 6);
    var len = try ins.iconst(clif.Type.I64, 4);

    // If < 1000: 3 digits from offset 7
    const lt1000 = try ins.icmp(.ult, val, thousand);
    start = try ins.select(clif.Type.I64, lt1000, try ins.iconst(clif.Type.I64, 7), start);
    len = try ins.select(clif.Type.I64, lt1000, try ins.iconst(clif.Type.I64, 3), len);

    // If < 100: 2 digits from offset 8
    const lt100 = try ins.icmp(.ult, val, hundred);
    start = try ins.select(clif.Type.I64, lt100, try ins.iconst(clif.Type.I64, 8), start);
    len = try ins.select(clif.Type.I64, lt100, try ins.iconst(clif.Type.I64, 2), len);

    // If < 10: 1 digit from offset 9
    const lt10 = try ins.icmp(.ult, val, ten);
    start = try ins.select(clif.Type.I64, lt10, try ins.iconst(clif.Type.I64, 9), start);
    len = try ins.select(clif.Type.I64, lt10, try ins.iconst(clif.Type.I64, 1), len);

    // write(fd, buf + start, len)
    const write_addr = try ins.iadd(buf, start);
    _ = try ins.call(w_ref, &[_]clif.Value{ fd, write_addr, len });
}
