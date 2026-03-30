//! Bench Runtime for Cot (WebAssembly)
//!
//! Provides benchmark output functions for the `cot bench` runner.
//! Reference: Go testing.B (adaptive calibration), Deno bench (colored output)
//!
//! Timing architecture (same as test_runtime.zig):
//!   Timing is measured INSIDE the runtime functions, not in the generated bench runner IR.
//!   Uses Wasm globals for timing storage to avoid cross-block IR node reference issues.
//!
//! Three Wasm globals for timing:
//!   bench_start_global:       per-measurement start timestamp (i64, ns)
//!   total_start_global:       total start timestamp (i64, ns), lazy-init
//!   bench_iter_global:        calibrated iteration count (i64), init=1
//!
//! Seven functions:
//!   __bench_print_name(ptr, len) -> void   — write 'bench "name" ... ', lazy-init total start
//!   __bench_calibrate_start() -> void      — store @time() in bench_start_global
//!   __bench_calibrate_end() -> void        — N = 1B / (time - start), clamp [1, 10M], store
//!   __bench_measure_start() -> void        — store @time() in bench_start_global
//!   __bench_measure_end() -> void          — elapsed = time - start, ns_op = elapsed / N, print
//!   __bench_get_n() -> i64                 — return bench_iter_global value
//!   __bench_summary(count) -> void         — print "\nok | N benchmarks (X.XXs)\n"

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/wasm.zig");
const ValType = wasm_link.ValType;
const wasm_op = @import("wasm_opcodes.zig");
const BLOCK_VOID: u8 = wasm_op.BLOCK_VOID;

// =============================================================================
// Function Names
// =============================================================================

pub const BENCH_PRINT_NAME_NAME = "__bench_print_name";
pub const BENCH_CALIBRATE_START_NAME = "__bench_calibrate_start";
pub const BENCH_CALIBRATE_END_NAME = "__bench_calibrate_end";
pub const BENCH_MEASURE_START_NAME = "__bench_measure_start";
pub const BENCH_MEASURE_END_NAME = "__bench_measure_end";
pub const BENCH_GET_N_NAME = "__bench_get_n";
pub const BENCH_SUMMARY_NAME = "__bench_summary";

// =============================================================================
// Return Type
// =============================================================================

pub const BenchFunctions = struct {
    bench_print_name_idx: u32,
    bench_calibrate_start_idx: u32,
    bench_calibrate_end_idx: u32,
    bench_measure_start_idx: u32,
    bench_measure_end_idx: u32,
    bench_get_n_idx: u32,
    bench_summary_idx: u32,
};

// =============================================================================
// addToLinker — register globals and all bench runtime functions
// =============================================================================

pub fn addToLinker(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker, write_func_idx: u32, eprint_int_func_idx: u32, time_func_idx: u32, user_n: ?i64) !BenchFunctions {
    // Three globals for bench timing/iteration storage
    const bench_start_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });
    const total_start_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });
    const bench_iter_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 1 });
    const bench_start_global = bench_start_dynamic + 1; // Offset by SP at index 0
    const total_start_global = total_start_dynamic + 1;
    const bench_iter_global = bench_iter_dynamic + 1;

    // Void type: () -> void
    const void_type = try linker.addType(
        &[_]ValType{},
        &[_]ValType{},
    );

    // __bench_print_name: (ptr: i64, len: i64) -> void
    const print_name_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{},
    );
    const print_name_body = try generateBenchPrintNameBody(allocator, write_func_idx, time_func_idx, bench_start_global, total_start_global);
    const bench_print_name_idx = try linker.addFunc(.{
        .name = BENCH_PRINT_NAME_NAME,
        .type_idx = print_name_type,
        .code = print_name_body,
        .exported = false,
    });

    // __bench_calibrate_start: () -> void
    const cal_start_body = try generateCalibrateStartBody(allocator, time_func_idx, bench_start_global);
    const bench_calibrate_start_idx = try linker.addFunc(.{
        .name = BENCH_CALIBRATE_START_NAME,
        .type_idx = void_type,
        .code = cal_start_body,
        .exported = false,
    });

    // __bench_calibrate_end: () -> void
    const cal_end_body = try generateCalibrateEndBody(allocator, time_func_idx, bench_start_global, bench_iter_global, user_n);
    const bench_calibrate_end_idx = try linker.addFunc(.{
        .name = BENCH_CALIBRATE_END_NAME,
        .type_idx = void_type,
        .code = cal_end_body,
        .exported = false,
    });

    // __bench_measure_start: () -> void
    const meas_start_body = try generateMeasureStartBody(allocator, time_func_idx, bench_start_global);
    const bench_measure_start_idx = try linker.addFunc(.{
        .name = BENCH_MEASURE_START_NAME,
        .type_idx = void_type,
        .code = meas_start_body,
        .exported = false,
    });

    // __bench_measure_end: () -> void
    const meas_end_body = try generateMeasureEndBody(allocator, write_func_idx, eprint_int_func_idx, time_func_idx, bench_start_global, bench_iter_global);
    const bench_measure_end_idx = try linker.addFunc(.{
        .name = BENCH_MEASURE_END_NAME,
        .type_idx = void_type,
        .code = meas_end_body,
        .exported = false,
    });

    // __bench_get_n: () -> i64
    const get_n_type = try linker.addType(
        &[_]ValType{},
        &[_]ValType{.i64},
    );
    const get_n_body = try generateGetNBody(allocator, bench_iter_global);
    const bench_get_n_idx = try linker.addFunc(.{
        .name = BENCH_GET_N_NAME,
        .type_idx = get_n_type,
        .code = get_n_body,
        .exported = false,
    });

    // __bench_summary: (count: i64) -> void
    const summary_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{},
    );
    const summary_body = try generateBenchSummaryBody(allocator, write_func_idx, eprint_int_func_idx, time_func_idx, total_start_global);
    const bench_summary_idx = try linker.addFunc(.{
        .name = BENCH_SUMMARY_NAME,
        .type_idx = summary_type,
        .code = summary_body,
        .exported = false,
    });

    return BenchFunctions{
        .bench_print_name_idx = bench_print_name_idx,
        .bench_calibrate_start_idx = bench_calibrate_start_idx,
        .bench_calibrate_end_idx = bench_calibrate_end_idx,
        .bench_measure_start_idx = bench_measure_start_idx,
        .bench_measure_end_idx = bench_measure_end_idx,
        .bench_get_n_idx = bench_get_n_idx,
        .bench_summary_idx = bench_summary_idx,
    };
}

// =============================================================================
// Helper: emit a short string literal to stderr via cot_write
// =============================================================================

fn emitWriteString(code: *wasm.CodeBuilder, write_func_idx: u32, buf_local: u32, bytes: []const u8) !void {
    var offset: u32 = 0;
    while (offset < bytes.len) {
        const remaining = bytes.len - offset;
        const chunk_len = if (remaining >= 8) 8 else remaining;

        var val: i64 = 0;
        for (0..chunk_len) |j| {
            val |= @as(i64, bytes[offset + j]) << @intCast(j * 8);
        }

        try code.emitLocalGet(buf_local);
        try code.emitI64Const(val);
        try code.emitI64Store(3, offset);
        offset += 8;
    }

    try code.emitI64Const(2); // fd = stderr
    try code.emitLocalGet(buf_local);
    try code.emitI64ExtendI32U();
    try code.emitI64Const(@intCast(bytes.len));
    try code.emitCall(write_func_idx);
    try code.emitDrop();
}

// =============================================================================
// __bench_print_name — write 'bench "name" ... ', lazy-init total start, store per-bench start
// Params: (ptr: i64, len: i64)
// Locals: local 2 = buf_ptr (i32)
// =============================================================================

fn generateBenchPrintNameBody(allocator: std.mem.Allocator, write_func_idx: u32, time_func_idx: u32, bench_start_global: u32, total_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    _ = try code.declareLocals(&[_]wasm.ValType{.i32});
    // param 0 = ptr, param 1 = len, local 2 = buf_ptr

    // Store per-bench start time
    try code.emitCall(time_func_idx);
    try code.emitGlobalSet(bench_start_global);

    // Lazy-initialize total start time on first bench
    try code.emitGlobalGet(total_start_global);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitGlobalGet(bench_start_global);
        try code.emitGlobalSet(total_start_global);
    }
    try code.emitEnd();

    // Allocate 16 bytes on Wasm stack
    try code.emitGlobalGet(0);
    try code.emitI32Const(16);
    try code.emitI32Sub();
    try code.emitLocalTee(2);
    try code.emitGlobalSet(0);

    // Write 'bench "' (7 bytes)
    try emitWriteString(&code, write_func_idx, 2, "bench \"");

    // Write name bytes (ptr, len from params)
    try code.emitI64Const(2);
    try code.emitLocalGet(0);
    try code.emitLocalGet(1);
    try code.emitCall(write_func_idx);
    try code.emitDrop();

    // Write '" ... ' (6 bytes)
    try emitWriteString(&code, write_func_idx, 2, "\" ... ");

    // Restore stack pointer
    try code.emitLocalGet(2);
    try code.emitI32Const(16);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    return try code.finish();
}

// =============================================================================
// __bench_calibrate_start — store @time() in bench_start_global
// =============================================================================

fn generateCalibrateStartBody(allocator: std.mem.Allocator, time_func_idx: u32, bench_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitCall(time_func_idx);
    try code.emitGlobalSet(bench_start_global);

    return try code.finish();
}

// =============================================================================
// __bench_calibrate_end — compute N from single-call timing, store in iter_global
// N = 1_000_000_000 / elapsed_ns, clamped to [1, 10_000_000]
// If user_n is set (--n flag), use that instead.
// =============================================================================

fn generateCalibrateEndBody(allocator: std.mem.Allocator, time_func_idx: u32, bench_start_global: u32, bench_iter_global: u32, user_n: ?i64) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    if (user_n) |n| {
        // --n flag: use user-specified iteration count
        try code.emitI64Const(n);
        try code.emitGlobalSet(bench_iter_global);
        return try code.finish();
    }

    // local 0 = elapsed / N (reused)
    _ = try code.declareLocals(&[_]wasm.ValType{.i64});

    // N = 1_000_000_000 / elapsed_ns, clamped to [1, 10_000_000]
    // elapsed = @time() - bench_start
    try code.emitCall(time_func_idx);
    try code.emitGlobalGet(bench_start_global);
    try code.emitI64Sub();

    // Store elapsed in local 0 for reuse
    try code.emitLocalTee(0);

    // Go testing.B: N = 1_000_000_000 / elapsed, targeting ~1s total runtime.
    // Cot's bench loop has higher per-iteration overhead (br_table dispatch),
    // so cap at 10M iterations to keep total time practical.
    const max_n: i64 = 10_000_000;

    // if elapsed <= 0, set N = max_n (avoid div-by-zero)
    try code.emitI64Const(0);
    try code.emitI64LeS();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(max_n);
    try code.emitGlobalSet(bench_iter_global);
    try code.emitElse();

    // N = 1_000_000_000 / elapsed (Go: benchmarkOnce predictN)
    try code.emitI64Const(1_000_000_000);
    try code.emitLocalGet(0);
    try code.emitI64DivU();

    // Clamp: if N < 1, N = 1
    try code.emitLocalTee(0);
    try code.emitI64Const(1);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(1);
    try code.emitGlobalSet(bench_iter_global);
    try code.emitElse();
    // Clamp: if N > max_n, N = max_n
    try code.emitLocalGet(0);
    try code.emitI64Const(max_n);
    try code.emitI64GtS();
    try code.emitIf(BLOCK_VOID);
    try code.emitI64Const(max_n);
    try code.emitGlobalSet(bench_iter_global);
    try code.emitElse();
    try code.emitLocalGet(0);
    try code.emitGlobalSet(bench_iter_global);
    try code.emitEnd(); // inner clamp
    try code.emitEnd(); // outer clamp
    try code.emitEnd(); // elapsed <= 0

    return try code.finish();
}

// =============================================================================
// __bench_measure_start — store @time() in bench_start_global
// =============================================================================

fn generateMeasureStartBody(allocator: std.mem.Allocator, time_func_idx: u32, bench_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitCall(time_func_idx);
    try code.emitGlobalSet(bench_start_global);

    return try code.finish();
}

// =============================================================================
// __bench_measure_end — compute ns/op, print result line
// Output: "N iterations  XXXX ns/op (X.XXs)\n"
// Locals: local 0 = buf_ptr (i32), local 1 = elapsed_ns (i64),
//         local 2 = ns_op (i64), local 3 = elapsed_s_x100 (i64)
// =============================================================================

fn generateMeasureEndBody(allocator: std.mem.Allocator, write_func_idx: u32, eprint_int_func_idx: u32, time_func_idx: u32, bench_start_global: u32, bench_iter_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i64, .i64, .i64 });

    // elapsed_ns = time() - start
    try code.emitCall(time_func_idx);
    try code.emitGlobalGet(bench_start_global);
    try code.emitI64Sub();
    try code.emitLocalSet(1);

    // ns_op = elapsed_ns / N
    try code.emitLocalGet(1);
    try code.emitGlobalGet(bench_iter_global);
    try code.emitI64DivS();
    try code.emitLocalSet(2);

    // elapsed_s_x100 = elapsed_ns / 10_000_000 (for X.XX seconds display)
    try code.emitLocalGet(1);
    try code.emitI64Const(10_000_000);
    try code.emitI64DivS();
    try code.emitLocalSet(3);

    // Allocate 32 bytes on stack
    try code.emitGlobalGet(0);
    try code.emitI32Const(32);
    try code.emitI32Sub();
    try code.emitLocalTee(0);
    try code.emitGlobalSet(0);

    // Print N (iterations count)
    try code.emitGlobalGet(bench_iter_global);
    try code.emitCall(eprint_int_func_idx);

    // " iterations  "
    try emitWriteString(&code, write_func_idx, 0, " iterations  ");

    // Print ns_op
    try code.emitLocalGet(2);
    try code.emitCall(eprint_int_func_idx);

    // " ns/op ("
    try emitWriteString(&code, write_func_idx, 0, " ns/op (");

    // Print seconds: elapsed_s_x100 / 100 (integer part)
    try code.emitLocalGet(3);
    try code.emitI64Const(100);
    try code.emitI64DivS();
    try code.emitCall(eprint_int_func_idx);

    // "."
    try emitWriteString(&code, write_func_idx, 0, ".");

    // Print fractional: elapsed_s_x100 % 100 (with leading zero if needed)
    // We need to handle the case where fraction < 10 (print "0" prefix)
    try code.emitLocalGet(3);
    try code.emitI64Const(100);
    try code.emitI64RemS();
    // Make it positive (in case of negative remainder)
    // abs: if negative, negate
    try code.emitLocalSet(3); // reuse local 3 for fraction
    try code.emitLocalGet(3);
    try code.emitI64Const(0);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitI64Const(0);
        try code.emitLocalGet(3);
        try code.emitI64Sub();
        try code.emitLocalSet(3);
    }
    try code.emitEnd();

    // If fraction < 10, print leading "0"
    try code.emitLocalGet(3);
    try code.emitI64Const(10);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_VOID);
    {
        try emitWriteString(&code, write_func_idx, 0, "0");
    }
    try code.emitEnd();

    // Print fraction digits
    try code.emitLocalGet(3);
    try code.emitCall(eprint_int_func_idx);

    // "s)\n"
    try emitWriteString(&code, write_func_idx, 0, "s)\n");

    // Restore stack pointer
    try code.emitLocalGet(0);
    try code.emitI32Const(32);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    return try code.finish();
}

// =============================================================================
// __bench_get_n — return bench_iter_global value
// =============================================================================

fn generateGetNBody(allocator: std.mem.Allocator, bench_iter_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitGlobalGet(bench_iter_global);

    return try code.finish();
}

// =============================================================================
// __bench_summary — print "\nok | N benchmarks (X.XXs)\n"
// Params: (count: i64)
// Locals: local 1 = buf_ptr (i32), local 2 = total_s_x100 (i64)
// =============================================================================

fn generateBenchSummaryBody(allocator: std.mem.Allocator, write_func_idx: u32, eprint_int_func_idx: u32, time_func_idx: u32, total_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 1: buf_ptr (i32), local 2: total_s_x100 (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i64 });

    // Compute total elapsed: (time() - total_start) / 10_000_000 → hundredths of second
    try code.emitCall(time_func_idx);
    try code.emitGlobalGet(total_start_global);
    try code.emitI64Sub();
    try code.emitI64Const(10_000_000);
    try code.emitI64DivS();
    try code.emitLocalSet(2);

    // Allocate 32 bytes on Wasm stack
    try code.emitGlobalGet(0);
    try code.emitI32Const(32);
    try code.emitI32Sub();
    try code.emitLocalTee(1);
    try code.emitGlobalSet(0);

    // "\n"
    try emitWriteString(&code, write_func_idx, 1, "\n");

    // Green "ok"
    try emitWriteString(&code, write_func_idx, 1, "\x1b[1;32mok\x1b[0m");

    // " | "
    try emitWriteString(&code, write_func_idx, 1, " | ");

    // Print count
    try code.emitLocalGet(0); // param 0 = count
    try code.emitCall(eprint_int_func_idx);

    // " benchmarks ("
    try emitWriteString(&code, write_func_idx, 1, " benchmarks (");

    // Print seconds: total_s_x100 / 100
    try code.emitLocalGet(2);
    try code.emitI64Const(100);
    try code.emitI64DivS();
    try code.emitCall(eprint_int_func_idx);

    // "."
    try emitWriteString(&code, write_func_idx, 1, ".");

    // Print fractional part with leading zero
    try code.emitLocalGet(2);
    try code.emitI64Const(100);
    try code.emitI64RemS();
    try code.emitLocalSet(2); // reuse for fraction
    // Make positive
    try code.emitLocalGet(2);
    try code.emitI64Const(0);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitI64Const(0);
        try code.emitLocalGet(2);
        try code.emitI64Sub();
        try code.emitLocalSet(2);
    }
    try code.emitEnd();

    // Leading zero if < 10
    try code.emitLocalGet(2);
    try code.emitI64Const(10);
    try code.emitI64LtS();
    try code.emitIf(BLOCK_VOID);
    {
        try emitWriteString(&code, write_func_idx, 1, "0");
    }
    try code.emitEnd();

    try code.emitLocalGet(2);
    try code.emitCall(eprint_int_func_idx);

    // "s)\n"
    try emitWriteString(&code, write_func_idx, 1, "s)\n");

    // Restore stack pointer
    try code.emitLocalGet(1);
    try code.emitI32Const(32);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    return try code.finish();
}
