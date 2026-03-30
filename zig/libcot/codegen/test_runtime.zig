//! Test Runtime for Cot (WebAssembly)
//!
//! Provides test output functions for the `cot test` runner.
//! Reference: Deno test runner output format (timing inside runtime, not in IR)
//!
//! Timing architecture (Deno pattern):
//!   Timing is measured INSIDE the runtime functions, not in the generated test runner IR.
//!   __test_print_name calls cot_time() and stores start in a Wasm global.
//!   __test_pass/__test_fail call cot_time() and compute elapsed internally.
//!   This avoids cross-block IR node reference issues with the br_table dispatch.
//!
//! Two Wasm globals are added for timing:
//!   test_start_global:  per-test start timestamp (i64, ns)
//!   total_start_global: total start timestamp (i64, ns)
//!
//! Five functions:
//!   __test_begin() -> void                       — store total start time in global
//!   __test_print_name(ptr, len) -> void          — store per-test start, write 'test "name" ... '
//!   __test_pass() -> void                        — compute elapsed, write green "ok (Nms)\n"
//!   __test_fail() -> void                        — compute elapsed, write red "FAIL (Nms)\n"
//!   __test_summary(passed, failed) -> void       — compute total, write colored summary

const std = @import("std");
const wasm = @import("wasm.zig");
const wasm_link = @import("wasm/wasm.zig");
const ValType = wasm_link.ValType;
const wasm_op = @import("wasm_opcodes.zig");
const BLOCK_VOID: u8 = wasm_op.BLOCK_VOID;

// =============================================================================
// Function Names
// =============================================================================

pub const TEST_BEGIN_NAME = "__test_begin";
pub const TEST_PRINT_NAME_NAME = "__test_print_name";
pub const TEST_PASS_NAME = "__test_pass";
pub const TEST_FAIL_NAME = "__test_fail";
pub const TEST_SUMMARY_NAME = "__test_summary";
pub const TEST_STORE_FAIL_VALUES_NAME = "__test_store_fail_values";

// =============================================================================
// Return Type
// =============================================================================

pub const TestFunctions = struct {
    test_begin_idx: u32,
    test_print_name_idx: u32,
    test_pass_idx: u32,
    test_fail_idx: u32,
    test_summary_idx: u32,
    test_store_fail_values_idx: u32,
};

// =============================================================================
// addToLinker — register globals and all test runtime functions
// =============================================================================

pub fn addToLinker(allocator: std.mem.Allocator, linker: *@import("wasm/link.zig").Linker, write_func_idx: u32, eprint_int_func_idx: u32, time_func_idx: u32) !TestFunctions {
    // Add two globals for timing storage (avoids fixed memory addresses)
    // Note: addGlobal returns index in dynamic list, but SP is at index 0
    // so actual global index is dynamic_idx + 1 (same pattern as arc.zig)
    const test_start_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });
    const total_start_dynamic = try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 });
    const test_start_global = test_start_dynamic + 1; // Offset by SP
    const total_start_global = total_start_dynamic + 1; // Offset by SP

    // Add globals for storing assertEq failure values (Deno pattern: show expected vs actual)
    // fail_left_val: left value (i64 for ints, ptr for strings)
    // fail_right_val: right value (i64 for ints, ptr for strings)
    // fail_left_len: 0 for ints, string length for strings
    // fail_right_len: 0 for ints, string length for strings
    // fail_is_string: 0 = integer, 1 = string
    // fail_has_values: 0 = no values stored, 1 = values pending display
    const fail_left_val_global = (try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 })) + 1;
    const fail_right_val_global = (try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 })) + 1;
    const fail_left_len_global = (try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 })) + 1;
    const fail_right_len_global = (try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 })) + 1;
    const fail_is_string_global = (try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 })) + 1;
    const fail_has_values_global = (try linker.addGlobal(.{ .val_type = .i64, .mutable = true, .init_i64 = 0 })) + 1;

    // __test_begin: () -> void
    const void_type = try linker.addType(
        &[_]ValType{},
        &[_]ValType{},
    );
    const begin_body = try generateTestBeginBody(allocator, time_func_idx, total_start_global);
    const test_begin_idx = try linker.addFunc(.{
        .name = TEST_BEGIN_NAME,
        .type_idx = void_type,
        .code = begin_body,
        .exported = true,
    });

    // __test_print_name: (ptr: i64, len: i64) -> void
    const print_name_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{},
    );
    const print_name_body = try generateTestPrintNameBody(allocator, write_func_idx, time_func_idx, test_start_global, total_start_global);
    const test_print_name_idx = try linker.addFunc(.{
        .name = TEST_PRINT_NAME_NAME,
        .type_idx = print_name_type,
        .code = print_name_body,
        .exported = false,
    });

    // __test_pass: () -> void
    const pass_body = try generateTestPassBody(allocator, write_func_idx, eprint_int_func_idx, time_func_idx, test_start_global);
    const test_pass_idx = try linker.addFunc(.{
        .name = TEST_PASS_NAME,
        .type_idx = void_type,
        .code = pass_body,
        .exported = false,
    });

    // __test_fail: () -> void
    const fail_body = try generateTestFailBody(allocator, write_func_idx, eprint_int_func_idx, time_func_idx, test_start_global, fail_has_values_global, fail_left_val_global, fail_right_val_global, fail_left_len_global, fail_right_len_global, fail_is_string_global);
    const test_fail_idx = try linker.addFunc(.{
        .name = TEST_FAIL_NAME,
        .type_idx = void_type,
        .code = fail_body,
        .exported = false,
    });

    // __test_store_fail_values: (left: i64, right: i64, is_string: i64, left_len: i64, right_len: i64) -> void
    const store_fail_type = try linker.addType(
        &[_]ValType{ .i64, .i64, .i64, .i64, .i64 },
        &[_]ValType{},
    );
    const store_fail_body = try generateTestStoreFailValuesBody(allocator, fail_left_val_global, fail_right_val_global, fail_is_string_global, fail_left_len_global, fail_right_len_global, fail_has_values_global);
    const test_store_fail_values_idx = try linker.addFunc(.{
        .name = TEST_STORE_FAIL_VALUES_NAME,
        .type_idx = store_fail_type,
        .code = store_fail_body,
        .exported = false,
    });

    // __test_summary: (passed: i64, failed: i64) -> void
    const summary_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{},
    );
    const summary_body = try generateTestSummaryBody(allocator, write_func_idx, eprint_int_func_idx, time_func_idx, total_start_global);
    const test_summary_idx = try linker.addFunc(.{
        .name = TEST_SUMMARY_NAME,
        .type_idx = summary_type,
        .code = summary_body,
        .exported = false,
    });

    return TestFunctions{
        .test_begin_idx = test_begin_idx,
        .test_print_name_idx = test_print_name_idx,
        .test_pass_idx = test_pass_idx,
        .test_fail_idx = test_fail_idx,
        .test_summary_idx = test_summary_idx,
        .test_store_fail_values_idx = test_store_fail_values_idx,
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
// Helper: compute elapsed_ms = (cot_time() - global_start) / 1_000_000
// Leaves elapsed_ms (i64) on the Wasm stack
// =============================================================================

fn emitComputeElapsedMs(code: *wasm.CodeBuilder, time_func_idx: u32, start_global: u32) !void {
    try code.emitCall(time_func_idx); // end_time on stack
    try code.emitGlobalGet(start_global); // start_time on stack
    try code.emitI64Sub(); // elapsed_ns = end - start
    try code.emitI64Const(1_000_000); // divisor
    try code.emitI64DivS(); // elapsed_ms
}

// =============================================================================
// __test_begin — stores total start time in global
// =============================================================================

fn generateTestBeginBody(allocator: std.mem.Allocator, time_func_idx: u32, total_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    try code.emitCall(time_func_idx); // cot_time() -> i64
    try code.emitGlobalSet(total_start_global); // store in global

    return try code.finish();
}

// =============================================================================
// __test_print_name — stores per-test start time, writes: test "name" ...
// =============================================================================

fn generateTestPrintNameBody(allocator: std.mem.Allocator, write_func_idx: u32, time_func_idx: u32, test_start_global: u32, total_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    _ = try code.declareLocals(&[_]wasm.ValType{.i32});
    // param 0 = ptr, param 1 = len, local 2 = buf_ptr

    // Store per-test start time in global
    try code.emitCall(time_func_idx);
    try code.emitGlobalSet(test_start_global);

    // Lazy-initialize total start time on first test
    // (avoids adding extra IR to test runner which disrupts native br_table dispatch)
    try code.emitGlobalGet(total_start_global);
    try code.emitI64Eqz();
    try code.emitIf(BLOCK_VOID);
    {
        try code.emitGlobalGet(test_start_global); // Reuse per-test start time
        try code.emitGlobalSet(total_start_global);
    }
    try code.emitEnd();

    // Allocate 16 bytes on Wasm stack
    try code.emitGlobalGet(0);
    try code.emitI32Const(16);
    try code.emitI32Sub();
    try code.emitLocalTee(2);
    try code.emitGlobalSet(0);

    // Write 'test "' (6 bytes)
    try emitWriteString(&code, write_func_idx, 2, "test \"");

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
// __test_pass — computes elapsed, writes "\x1b[1;32mok\x1b[0m (Nms)\n"
// =============================================================================

fn generateTestPassBody(allocator: std.mem.Allocator, write_func_idx: u32, eprint_int_func_idx: u32, time_func_idx: u32, test_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: buf_ptr (i32), local 1: elapsed_ms (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i64 });

    // Compute elapsed_ms from global start time
    try emitComputeElapsedMs(&code, time_func_idx, test_start_global);
    try code.emitLocalSet(1);

    // Allocate 24 bytes on stack
    try code.emitGlobalGet(0);
    try code.emitI32Const(24);
    try code.emitI32Sub();
    try code.emitLocalTee(0);
    try code.emitGlobalSet(0);

    // Write "\x1b[1;32mok\x1b[0m ("
    try emitWriteString(&code, write_func_idx, 0, "\x1b[1;32mok\x1b[0m (");

    // Print elapsed_ms
    try code.emitLocalGet(1);
    try code.emitCall(eprint_int_func_idx);

    // Write "ms)\n"
    try emitWriteString(&code, write_func_idx, 0, "ms)\n");

    // Restore stack pointer
    try code.emitLocalGet(0);
    try code.emitI32Const(24);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    return try code.finish();
}

// =============================================================================
// __test_fail — computes elapsed, writes "\x1b[1;31mFAIL\x1b[0m (Nms)\n"
// =============================================================================

fn generateTestFailBody(
    allocator: std.mem.Allocator,
    write_func_idx: u32,
    eprint_int_func_idx: u32,
    time_func_idx: u32,
    test_start_global: u32,
    fail_has_values_global: u32,
    fail_left_val_global: u32,
    fail_right_val_global: u32,
    fail_left_len_global: u32,
    fail_right_len_global: u32,
    fail_is_string_global: u32,
) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 0: buf_ptr (i32), local 1: elapsed_ms (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i64 });

    // Compute elapsed_ms from global start time
    try emitComputeElapsedMs(&code, time_func_idx, test_start_global);
    try code.emitLocalSet(1);

    // Allocate 32 bytes on stack (need extra for value display)
    try code.emitGlobalGet(0);
    try code.emitI32Const(32);
    try code.emitI32Sub();
    try code.emitLocalTee(0);
    try code.emitGlobalSet(0);

    // Write "\x1b[1;31mFAIL\x1b[0m ("
    try emitWriteString(&code, write_func_idx, 0, "\x1b[1;31mFAIL\x1b[0m (");

    // Print elapsed_ms
    try code.emitLocalGet(1);
    try code.emitCall(eprint_int_func_idx);

    // Write "ms)\n"
    try emitWriteString(&code, write_func_idx, 0, "ms)\n");

    // If fail values are stored, print expected vs actual (Deno pattern)
    try code.emitGlobalGet(fail_has_values_global);
    try code.emitI64Const(0);
    try code.emitI64Ne();
    try code.emitIf(BLOCK_VOID);
    {
        // Check if string comparison
        try code.emitGlobalGet(fail_is_string_global);
        try code.emitI64Const(0);
        try code.emitI64Ne();
        try code.emitIf(BLOCK_VOID);
        {
            // String values: write "  expected: " + left string + "\n"
            try emitWriteString(&code, write_func_idx, 0, "  expected: \"");
            // Write left string (ptr, len)
            try code.emitI64Const(2); // fd = stderr
            try code.emitGlobalGet(fail_left_val_global); // ptr
            try code.emitGlobalGet(fail_left_len_global); // len
            try code.emitCall(write_func_idx);
            try code.emitDrop();
            try emitWriteString(&code, write_func_idx, 0, "\"\n");

            // Write "  received: " + right string + "\n"
            try emitWriteString(&code, write_func_idx, 0, "  received: \"");
            try code.emitI64Const(2); // fd = stderr
            try code.emitGlobalGet(fail_right_val_global); // ptr
            try code.emitGlobalGet(fail_right_len_global); // len
            try code.emitCall(write_func_idx);
            try code.emitDrop();
            try emitWriteString(&code, write_func_idx, 0, "\"\n");
        }
        try code.emitElse();
        {
            // Integer values: write "  expected: " + int + "\n"
            try emitWriteString(&code, write_func_idx, 0, "  expected: ");
            try code.emitGlobalGet(fail_left_val_global);
            try code.emitCall(eprint_int_func_idx);
            try emitWriteString(&code, write_func_idx, 0, "\n");

            // Write "  received: " + int + "\n"
            try emitWriteString(&code, write_func_idx, 0, "  received: ");
            try code.emitGlobalGet(fail_right_val_global);
            try code.emitCall(eprint_int_func_idx);
            try emitWriteString(&code, write_func_idx, 0, "\n");
        }
        try code.emitEnd();

        // Reset fail_has_values
        try code.emitI64Const(0);
        try code.emitGlobalSet(fail_has_values_global);
    }
    try code.emitEnd();

    // Restore stack pointer
    try code.emitLocalGet(0);
    try code.emitI32Const(32);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    return try code.finish();
}

// =============================================================================
// __test_summary — Deno-style: "\nok | N passed | M failed (Xms)\n"
//
// Wasm locals layout:
//   param 0: passed (i64)
//   param 1: failed (i64)
//   local 2: buf_ptr (i32)
//   local 3: total_ms (i64)
// =============================================================================

fn generateTestSummaryBody(allocator: std.mem.Allocator, write_func_idx: u32, eprint_int_func_idx: u32, time_func_idx: u32, total_start_global: u32) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // local 2: buf_ptr (i32), local 3: total_ms (i64)
    _ = try code.declareLocals(&[_]wasm.ValType{ .i32, .i64 });

    // Compute total_ms from global total start time
    try emitComputeElapsedMs(&code, time_func_idx, total_start_global);
    try code.emitLocalSet(3);

    // Allocate 32 bytes on Wasm stack
    try code.emitGlobalGet(0);
    try code.emitI32Const(32);
    try code.emitI32Sub();
    try code.emitLocalTee(2);
    try code.emitGlobalSet(0);

    // Write "\n"
    try emitWriteString(&code, write_func_idx, 2, "\n");

    // Status: green "ok" if no failures, red "FAILED" if failures
    try code.emitLocalGet(1);
    try code.emitI64Const(0);
    try code.emitI64GtS();
    try code.emitIf(BLOCK_VOID);
    {
        try emitWriteString(&code, write_func_idx, 2, "\x1b[1;31mFAILED\x1b[0m");
    }
    try code.emitElse();
    {
        try emitWriteString(&code, write_func_idx, 2, "\x1b[1;32mok\x1b[0m");
    }
    try code.emitEnd();

    // Write " | "
    try emitWriteString(&code, write_func_idx, 2, " | ");

    // Print passed count + " passed"
    try code.emitLocalGet(0);
    try code.emitCall(eprint_int_func_idx);
    try emitWriteString(&code, write_func_idx, 2, " passed");

    // If failed > 0: write " | " + failed count + " failed"
    try code.emitLocalGet(1);
    try code.emitI64Const(0);
    try code.emitI64GtS();
    try code.emitIf(BLOCK_VOID);
    {
        try emitWriteString(&code, write_func_idx, 2, " | ");
        try code.emitLocalGet(1);
        try code.emitCall(eprint_int_func_idx);
        try emitWriteString(&code, write_func_idx, 2, " failed");
    }
    try code.emitEnd();

    // Write " (" + total_ms + "ms)\n"
    try emitWriteString(&code, write_func_idx, 2, " (");
    try code.emitLocalGet(3);
    try code.emitCall(eprint_int_func_idx);
    try emitWriteString(&code, write_func_idx, 2, "ms)\n");

    // Restore stack pointer
    try code.emitLocalGet(2);
    try code.emitI32Const(32);
    try code.emitI32Add();
    try code.emitGlobalSet(0);

    return try code.finish();
}

// =============================================================================
// __test_store_fail_values — stores assertEq left/right values in globals
// Called from @assertEq fail path before returning error union.
// Params: left_val, right_val, is_string, left_len, right_len
// =============================================================================

fn generateTestStoreFailValuesBody(
    allocator: std.mem.Allocator,
    fail_left_val_global: u32,
    fail_right_val_global: u32,
    fail_is_string_global: u32,
    fail_left_len_global: u32,
    fail_right_len_global: u32,
    fail_has_values_global: u32,
) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // param 0: left_val, param 1: right_val, param 2: is_string
    // param 3: left_len, param 4: right_len

    try code.emitLocalGet(0); // left_val
    try code.emitGlobalSet(fail_left_val_global);

    try code.emitLocalGet(1); // right_val
    try code.emitGlobalSet(fail_right_val_global);

    try code.emitLocalGet(2); // is_string
    try code.emitGlobalSet(fail_is_string_global);

    try code.emitLocalGet(3); // left_len
    try code.emitGlobalSet(fail_left_len_global);

    try code.emitLocalGet(4); // right_len
    try code.emitGlobalSet(fail_right_len_global);

    // Mark that values are available
    try code.emitI64Const(1);
    try code.emitGlobalSet(fail_has_values_global);

    return try code.finish();
}
