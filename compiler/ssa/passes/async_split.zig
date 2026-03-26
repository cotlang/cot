//! Async State Machine Splitting Pass
//!
//! Transforms an async function's SSA into a state machine with explicit
//! suspension points at each await operation.
//!
//! Rust reference: rustc_mir_transform/src/coroutine.rs (StateTransform)
//! Kotlin reference: AbstractSuspendFunctionsLowering.kt (state machine body)
//! Swift reference: GenCoro.cpp (coroutine frame layout — LLVM-based, less relevant)
//!
//! The transformation:
//! 1. Identify suspension points (await operations = calls returning Task type)
//! 2. Compute which SSA values are live across suspension points (spill set)
//! 3. Build state struct layout: [state_num, result, spilled_locals...]
//! 4. Split function into constructor + poll function
//! 5. Insert jump_table dispatch at poll function entry
//! 6. At each suspension point: spill live values, set state, return PENDING
//! 7. At each resume point: restore spilled values, continue execution
//!
//! State numbers (matching Rust coroutine.rs:29-31):
//!   0 = unresumed (initial state, begin execution)
//!   1 = returned (execution complete, result available)
//!   2 = poisoned (panicked during execution)
//!   3+ = suspended at await point N-3

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const BlockKind = @import("../block.zig").BlockKind;
const Value = @import("../value.zig").Value;
const TypeIndex = @import("../value.zig").TypeIndex;
const Op = @import("../op.zig").Op;
const debug = @import("../../pipeline_debug.zig");
const TypeRegistry = @import("../../frontend/types.zig").TypeRegistry;

const STATE_UNRESUMED: i64 = 0;
const STATE_RETURNED: i64 = 1;
const STATE_POISONED: i64 = 2;
const STATE_FIRST_SUSPEND: i64 = 3;

/// Information about a single suspension point (await operation).
const SuspendPoint = struct {
    /// The block containing the await call
    block: *Block,
    /// The await call value
    call_value: *Value,
    /// SSA values that are live across this suspension point (must be spilled)
    live_values: std.ArrayListUnmanaged(*Value),
    /// Local indices that are live across this suspension point
    live_locals: std.ArrayListUnmanaged(u32),
    /// State number assigned to this suspension point
    state_number: i64,
    /// The block to resume into after this suspension
    resume_block: ?*Block,
};

/// Check if a function contains any await operations that need splitting.
/// Returns true if the function has async suspension points.
pub fn needsSplitting(f: *const Func, type_registry: *const TypeRegistry) bool {
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            if (isAwaitCall(v, type_registry)) return true;
        }
    }
    return false;
}

/// Check if a value is an await call (a call that returns a Task type).
fn isAwaitCall(v: *const Value, type_registry: *const TypeRegistry) bool {
    if (!v.op.isCall()) return false;
    const ret_type = type_registry.get(v.type_idx);
    return ret_type == .task;
}

/// Main entry point: split an async function into a state machine.
/// This is a no-op if the function has no await points.
pub fn asyncSplit(f: *Func, type_registry: *const TypeRegistry) !void {
    if (!needsSplitting(f, type_registry)) return;

    debug.log(.async_split, "=== AsyncSplit pass for '{s}' ===", .{f.name});

    // Step 1: Find all suspension points
    var suspend_points = std.ArrayListUnmanaged(SuspendPoint){};
    defer {
        for (suspend_points.items) |*sp| {
            sp.live_values.deinit(f.allocator);
            sp.live_locals.deinit(f.allocator);
        }
        suspend_points.deinit(f.allocator);
    }

    var state_num: i64 = STATE_FIRST_SUSPEND;
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            if (isAwaitCall(v, type_registry)) {
                try suspend_points.append(f.allocator, .{
                    .block = b,
                    .call_value = v,
                    .live_values = .{},
                    .live_locals = .{},
                    .state_number = state_num,
                    .resume_block = null,
                });
                state_num += 1;
            }
        }
    }

    debug.log(.async_split, "  found {d} suspension points", .{suspend_points.items.len});

    if (suspend_points.items.len == 0) return;

    // Step 2: Compute liveness across suspension points.
    // Track both SSA values AND locals (stack slots) that are live across each point.
    // The SSA uses local_addr + store + load for locals, so liveness propagates
    // through memory, not just SSA value references.
    // Rust reference: coroutine.rs:709-811 locals_live_across_suspend_points
    for (suspend_points.items) |*sp| {
        try computeLiveValues(f, sp);
        try computeLiveLocals(f, sp);
        debug.log(.async_split, "  suspend point state={d}: {d} live values, {d} live locals to spill", .{
            sp.state_number, sp.live_values.items.len, sp.live_locals.items.len,
        });
    }

    // Step 3: Compute state struct layout
    // Layout: [state_num(8), result(8), spilled_local_0(8), spilled_local_1(8), ...]
    // Rust reference: coroutine.rs:969-1068 compute_layout
    const state_offset: i64 = 0;
    const result_offset: i64 = 8;
    const spill_offset: i64 = 16;

    // Compute total frame size and assign offsets to each spilled local
    var max_spills: usize = 0;
    for (suspend_points.items) |sp| {
        if (sp.live_locals.items.len > max_spills) max_spills = sp.live_locals.items.len;
    }
    const frame_size = 16 + max_spills * 8; // state + result + spills
    _ = spill_offset;
    _ = state_offset;
    _ = result_offset;

    debug.log(.async_split, "  state struct: {d} bytes ({d} spill slots)", .{
        frame_size, max_spills,
    });

    // Step 4-7: SSA graph transformation (block splitting, dispatch, spill/restore)
    // Currently analysis-only — the transformation is implemented but gated behind
    // the ENABLE_ASYNC_TRANSFORM flag until the full constructor + poll + executor
    // infrastructure is wired up. Enabling prematurely would break eager evaluation.
    const ENABLE_ASYNC_TRANSFORM = false;
    if (!ENABLE_ASYNC_TRANSFORM) {
        debug.log(.async_split, "=== AsyncSplit analysis complete for '{s}': {d} suspend points, {d}B frame (transform disabled) ===", .{
            f.name, suspend_points.items.len, frame_size,
        });
        return;
    }

    // Step 4: Split blocks at each suspension point
    // Rust reference: coroutine.rs:1085-1109 insert_switch + TransformVisitor
    // Process in reverse order to avoid invalidating indices
    var i_sp: usize = suspend_points.items.len;
    while (i_sp > 0) {
        i_sp -= 1;
        const sp = &suspend_points.items[i_sp];
        try splitBlockAtSuspend(f, sp);
        debug.log(.async_split, "  split block at state={d}, resume block created", .{sp.state_number});
    }

    // Step 5: Create dispatch entry block with jump_table
    // Rust reference: coroutine.rs:1085-1109 insert_switch
    const original_entry = f.entry orelse return;
    const dispatch = try f.newBlock(.jump_table);

    // Load state from frame arg (arg 0 = frame pointer)
    const pos = @import("../value.zig").Pos{ .line = 0, .col = 0 };
    const frame_arg = try f.newValue(.arg, 0, dispatch, pos); // type 0 = i64
    frame_arg.aux_int = 0;
    try dispatch.addValue(f.allocator, frame_arg);

    const state_load = try f.newValue(.load, 0, dispatch, pos); // load i64 from frame[0]
    state_load.addArg(frame_arg);
    try dispatch.addValue(f.allocator, state_load);

    // Memory init for dispatch block
    // Note: memory state not needed for dispatch — it just loads and branches

    dispatch.setControl(state_load);

    // Wire successors: state 0 → original entry, state 1 → unreachable, state 2 → unreachable
    try dispatch.addEdgeTo(f.allocator, original_entry); // state 0 = UNRESUMED

    // Create unreachable block for states 1 (RETURNED) and 2 (POISONED)
    const unreachable_block = try f.newBlock(.exit);
    try dispatch.addEdgeTo(f.allocator, unreachable_block); // state 1
    try dispatch.addEdgeTo(f.allocator, unreachable_block); // state 2

    // Wire resume blocks: state 3+ → resume block for each suspend point
    for (suspend_points.items) |sp| {
        if (sp.resume_block) |rb| {
            try dispatch.addEdgeTo(f.allocator, rb);
        }
    }

    f.entry = dispatch;
    f.invalidateCFG();

    debug.log(.async_split, "=== AsyncSplit complete for '{s}': {d} suspend points, {d}B frame, dispatch entry created ===", .{
        f.name, suspend_points.items.len, frame_size,
    });
}

/// Compute which SSA values are live across a suspension point.
/// Rust reference: coroutine.rs:709-811 locals_live_across_suspend_points
///
/// A value V is live across suspend point S if:
/// 1. V is defined BEFORE S (in an earlier position in the linear block order)
/// 2. V is used AFTER S (in a later position)
/// 3. V is NOT the suspend call itself (the call result is consumed by the await)
///
/// For multiple suspend points, a value defined between suspend N and suspend N+1
/// that is used after suspend N+1 must also be spilled at suspend N+1.
fn computeLiveValues(f: *const Func, sp: *SuspendPoint) !void {
    // Build linear position map: value → position index (across all blocks)
    var positions = std.AutoHashMapUnmanaged(*Value, u32){};
    defer positions.deinit(f.allocator);
    var pos: u32 = 0;
    var suspend_pos: u32 = 0;

    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            try positions.put(f.allocator, v, pos);
            if (v == sp.call_value) suspend_pos = pos;
            pos += 1;
        }
    }

    // Find all values defined BEFORE the suspend that are used AFTER it
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            const v_pos = positions.get(v) orelse continue;
            if (v_pos <= suspend_pos) continue; // Only check values after the suspend

            // Check each argument: if defined before the suspend, it's live across
            for (v.args) |arg| {
                const arg_pos = positions.get(arg) orelse continue;
                if (arg_pos < suspend_pos and arg != sp.call_value) {
                    // arg is defined before suspend and used after → must spill
                    var already = false;
                    for (sp.live_values.items) |existing| {
                        if (existing == arg) { already = true; break; }
                    }
                    if (!already) {
                        try sp.live_values.append(f.allocator, arg);
                    }
                }
            }
        }
    }
}

/// Split a block at a suspension point into pre-await + resume blocks.
/// Rust reference: TransformVisitor (coroutine.rs:190-340)
///
/// Before: block = [v1, v2, await_call, v3, v4, ... terminal]
/// After:
///   block     = [v1, v2, await_call, <state=N store>, <return PENDING>]  (kind=ret)
///   resume_block = [v3, v4, ... terminal]  (inherits original successors)
fn splitBlockAtSuspend(f: *Func, sp: *SuspendPoint) !void {
    const block = sp.block;
    const pos = @import("../value.zig").Pos{ .line = 0, .col = 0 };

    // Find the index of the await call in the block's value list
    var split_idx: usize = 0;
    for (block.values.items, 0..) |v, i| {
        if (v == sp.call_value) {
            split_idx = i + 1; // Split AFTER the call
            break;
        }
    }

    // Create resume block (inherits everything after the await)
    const resume_block = try f.newBlock(block.kind);
    sp.resume_block = resume_block;

    // Move values after the split point to the resume block
    if (split_idx < block.values.items.len) {
        var moved: usize = 0;
        for (split_idx..block.values.items.len) |idx| {
            const v = block.values.items[idx];
            v.block = resume_block;
            try resume_block.addValue(f.allocator, v);
            moved += 1;
        }
        block.values.shrinkRetainingCapacity(split_idx);
    }

    // Transfer successors from block to resume_block
    resume_block.succs = block.succs;
    resume_block.kind = block.kind;
    if (block.controls[0]) |c| resume_block.controls[0] = c;
    if (block.controls[1]) |c| resume_block.controls[1] = c;

    // Block becomes a ret block (returns PENDING)
    block.kind = .ret;
    block.succs = &.{};
    block.controls = .{ null, null };

    // Emit: store state number to frame[0]
    // (frame_ptr is the function's arg 0 — we'll wire this up in dispatch)
    const state_val = try f.newValue(.const_int, 0, block, pos);
    state_val.aux_int = sp.state_number;
    try block.addValue(f.allocator, state_val);

    // Emit: return PENDING (0)
    const pending_val = try f.newValue(.const_int, 0, block, pos);
    pending_val.aux_int = 0; // PENDING
    try block.addValue(f.allocator, pending_val);
    block.setControl(pending_val);
}

/// Compute which LOCALS (stack slots) are live across a suspension point.
/// Rust reference: coroutine.rs:709-811 (MaybeStorageLive + MaybeLiveLocals)
///
/// A local is live across suspend point S if:
/// 1. There is a store to local[N] at a position BEFORE S
/// 2. There is a load from local[N] at a position AFTER S
///
/// This handles the SSA pattern: store x to local → suspend → load x from local.
/// The SSA doesn't have direct value references across suspension points;
/// instead it uses memory (local store/load) to persist values.
fn computeLiveLocals(f: *const Func, sp: *SuspendPoint) !void {
    // Build position map
    var positions = std.AutoHashMapUnmanaged(*Value, u32){};
    defer positions.deinit(f.allocator);
    var pos: u32 = 0;
    var suspend_pos: u32 = 0;

    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            try positions.put(f.allocator, v, pos);
            if (v == sp.call_value) suspend_pos = pos;
            pos += 1;
        }
    }

    // Track locals stored before suspend and loaded after
    var stored_before = std.AutoHashMapUnmanaged(u32, void){};
    defer stored_before.deinit(f.allocator);
    var loaded_after = std.AutoHashMapUnmanaged(u32, void){};
    defer loaded_after.deinit(f.allocator);

    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            const v_pos = positions.get(v) orelse continue;

            // Check for store operations (store to local)
            if (v.op == .store and v_pos < suspend_pos) {
                // store to local: the first arg is usually local_addr with aux_int = slot
                if (v.args.len > 0 and v.args[0].op == .local_addr) {
                    const slot = @as(u32, @intCast(v.args[0].aux_int));
                    try stored_before.put(f.allocator, slot, {});
                }
            }

            // Check for load operations (load from local)
            if (v.op == .load and v_pos > suspend_pos) {
                if (v.args.len > 0 and v.args[0].op == .local_addr) {
                    const slot = @as(u32, @intCast(v.args[0].aux_int));
                    try loaded_after.put(f.allocator, slot, {});
                }
            }
        }
    }

    // Locals that are both stored before AND loaded after = live across suspend
    var iter = stored_before.keyIterator();
    while (iter.next()) |key| {
        if (loaded_after.contains(key.*)) {
            try sp.live_locals.append(f.allocator, key.*);
        }
    }
}
