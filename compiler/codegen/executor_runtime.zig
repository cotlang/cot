//! Executor runtime functions for async task polling.
//!
//! Provides `executor_poll_task(fn_idx, frame_ptr) -> status` which calls
//! a poll function via call_indirect. This bridges stdlib Cot code (executor)
//! to compiler-generated poll functions (async_split state machines).
//!
//! Swift reference: ExecutorJob.runSynchronously(on:) — calls the job's function.

const std = @import("std");
const wasm_link = @import("wasm/link.zig");
const ValType = @import("wasm/wasm.zig").ValType;

pub const POLL_TASK_NAME = "executor_poll_task";
pub const RUN_UNTIL_COMPLETE_NAME = "executor_run_until_complete";
pub const ExecutorFunctions = struct {
    poll_task_idx: u32,
    run_until_complete_idx: u32,
};

pub fn addToLinker(allocator: std.mem.Allocator, linker: *wasm_link.Linker) !ExecutorFunctions {
    // executor_poll_task(fn_idx: i64, frame_ptr: i64) -> i64
    // Calls the poll function at table index fn_idx with frame_ptr as argument.
    // Returns the poll function's return value (0 = PENDING, 1 = READY).
    //
    // Swift reference: ExecutorJob.runSynchronously(on:) (CooperativeExecutor.swift:308)
    // This is the runtime bridge that allows stdlib Cot code to call
    // compiler-generated poll functions via call_indirect.

    // Type for poll functions: (i64) -> i64
    // The poll function takes a frame pointer and returns status.
    const poll_fn_type = try linker.addType(
        &[_]ValType{.i64},
        &[_]ValType{.i64},
    );

    // Type for executor_poll_task: (i64, i64) -> i64
    const poll_task_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{.i64},
    );

    const body = try generatePollTaskBody(allocator, poll_fn_type);
    const poll_task_idx = try linker.addFunc(.{
        .name = POLL_TASK_NAME,
        .type_idx = poll_task_type,
        .code = body,
        .exported = false,
    });

    // executor_run_until_complete(fn_idx: i64, frame_ptr: i64) -> void
    // Polls a task in a loop until it returns JOB_READY (1).
    // Swift reference: CooperativeExecutor.runUntil() (lines 291-336)
    // Replaces the constructor's inline busy-loop with a runtime function
    // that can later be extended to support multi-task interleaving.
    const run_complete_body = try generateRunUntilCompleteBody(allocator, poll_fn_type);
    const run_complete_type = try linker.addType(
        &[_]ValType{ .i64, .i64 },
        &[_]ValType{},
    );
    const run_until_complete_idx = try linker.addFunc(.{
        .name = RUN_UNTIL_COMPLETE_NAME,
        .type_idx = run_complete_type,
        .code = run_complete_body,
        .exported = false,
    });

    return .{
        .poll_task_idx = poll_task_idx,
        .run_until_complete_idx = run_until_complete_idx,
    };
}

/// Generate Wasm bytecode for executor_poll_task(fn_idx, frame_ptr) -> status.
/// Emits: local.get 1 (frame_ptr), local.get 0 (fn_idx), call_indirect type(i64)->i64
fn generatePollTaskBody(allocator: std.mem.Allocator, poll_fn_type_idx: u32) ![]u8 {
    var code = std.ArrayListUnmanaged(u8){};

    // Locals declaration: 0 additional locals (params are local 0 and 1)
    try code.append(allocator, 0x00); // 0 local groups

    // Push frame_ptr (arg for poll function)
    try code.append(allocator, 0x20); // local.get
    try code.append(allocator, 0x01); // local index 1 (frame_ptr)

    // Push fn_idx (table index for call_indirect)
    try code.append(allocator, 0x20); // local.get
    try code.append(allocator, 0x00); // local index 0 (fn_idx)

    // Wrap fn_idx to i32 for call_indirect table index
    try code.append(allocator, 0xA7); // i32.wrap_i64

    // call_indirect type_idx, table 0
    try code.append(allocator, 0x11); // call_indirect
    // Type index as ULEB128 (manual encode)
    {
        var val = poll_fn_type_idx;
        while (val >= 0x80) {
            try code.append(allocator, @intCast((val & 0x7F) | 0x80));
            val >>= 7;
        }
        try code.append(allocator, @intCast(val));
    }
    try code.append(allocator, 0x00); // table index 0

    // The call_indirect returns i64 (poll status), which is our return value
    try code.append(allocator, 0x0B); // end

    return code.toOwnedSlice(allocator);
}

/// Generate Wasm bytecode for executor_run_until_complete(fn_idx, frame_ptr).
/// Loops: call_indirect fn_idx(frame_ptr), check result, loop if PENDING.
/// Swift reference: CooperativeExecutor.runUntil() poll loop.
fn generateRunUntilCompleteBody(allocator: std.mem.Allocator, poll_fn_type_idx: u32) ![]u8 {
    var code = std.ArrayListUnmanaged(u8){};

    // Locals: 0 additional (params are local 0=fn_idx, 1=frame_ptr)
    try code.append(allocator, 0x00);

    // loop $poll_loop
    try code.append(allocator, 0x03); // loop
    try code.append(allocator, 0x40); // block type void

    // Push frame_ptr for call
    try code.append(allocator, 0x20); // local.get
    try code.append(allocator, 0x01); // local 1 (frame_ptr)

    // Push fn_idx for call_indirect
    try code.append(allocator, 0x20); // local.get
    try code.append(allocator, 0x00); // local 0 (fn_idx)
    try code.append(allocator, 0xA7); // i32.wrap_i64

    // call_indirect poll_fn_type, table 0
    try code.append(allocator, 0x11); // call_indirect
    {
        var val = poll_fn_type_idx;
        while (val >= 0x80) {
            try code.append(allocator, @intCast((val & 0x7F) | 0x80));
            val >>= 7;
        }
        try code.append(allocator, @intCast(val));
    }
    try code.append(allocator, 0x00); // table 0

    // Check result: if status == 0 (PENDING), branch back to loop
    try code.append(allocator, 0x50); // i64.eqz (1 if PENDING, 0 if READY)
    try code.append(allocator, 0x0D); // br_if
    try code.append(allocator, 0x00); // label 0 (innermost loop)

    // end loop
    try code.append(allocator, 0x0B); // end (loop)

    // end function
    try code.append(allocator, 0x0B); // end

    return code.toOwnedSlice(allocator);
}
