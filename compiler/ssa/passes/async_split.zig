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

    debug.log(.ssa, "=== AsyncSplit pass for '{s}' ===", .{f.name});

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

    debug.log(.ssa, "  found {d} suspension points", .{suspend_points.items.len});

    if (suspend_points.items.len == 0) return;

    // Step 2: Compute liveness across suspension points.
    // Track both SSA values AND locals (stack slots) that are live across each point.
    // The SSA uses local_addr + store + load for locals, so liveness propagates
    // through memory, not just SSA value references.
    // Rust reference: coroutine.rs:709-811 locals_live_across_suspend_points
    for (suspend_points.items) |*sp| {
        try computeLiveValues(f, sp);
        try computeLiveLocals(f, sp);
        debug.log(.ssa, "  suspend point state={d}: {d} live values, {d} live locals to spill", .{
            sp.state_number, sp.live_values.items.len, sp.live_locals.items.len,
        });
    }

    // Step 3+: State struct layout, block splitting, dispatch insertion
    // TODO: Steps 3-7 (state struct layout, block splitting, dispatch insertion)
    // These require modifying the SSA graph, which is the most complex part.
    // For now, log the analysis results so we can verify correctness.

    debug.log(.ssa, "=== AsyncSplit analysis complete for '{s}': {d} suspend points ===", .{
        f.name, suspend_points.items.len,
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
