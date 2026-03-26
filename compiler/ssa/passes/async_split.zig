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
        for (suspend_points.items) |*sp| sp.live_values.deinit(f.allocator);
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
                    .state_number = state_num,
                    .resume_block = null,
                });
                state_num += 1;
            }
        }
    }

    debug.log(.ssa, "  found {d} suspension points", .{suspend_points.items.len});

    if (suspend_points.items.len == 0) return;

    // Step 2: Compute liveness across suspension points
    // For each suspend point, find values defined before it that are used after it.
    // Rust reference: coroutine.rs:709-811 locals_live_across_suspend_points
    for (suspend_points.items) |*sp| {
        try computeLiveValues(f, sp);
        debug.log(.ssa, "  suspend point state={d}: {d} live values to spill", .{
            sp.state_number, sp.live_values.items.len,
        });
    }

    // TODO: Steps 3-7 (state struct layout, block splitting, dispatch insertion)
    // These require modifying the SSA graph, which is the most complex part.
    // For now, log the analysis results so we can verify correctness.

    debug.log(.ssa, "=== AsyncSplit analysis complete for '{s}': {d} suspend points ===", .{
        f.name, suspend_points.items.len,
    });
}

/// Compute which SSA values are live across a suspension point.
/// A value is live across a suspend point if:
/// 1. It is defined in a block that dominates the suspend point's block
/// 2. It is used in a block that is reachable from the resume point
/// Simplified for Phase 2: any value used after the suspend point in the
/// same function that was defined before or at the suspend point.
fn computeLiveValues(f: *const Func, sp: *SuspendPoint) !void {
    // Build set of all values defined at or before the suspend point
    var defined_before = std.AutoHashMapUnmanaged(*Value, void){};
    defer defined_before.deinit(f.allocator);

    var found_suspend = false;
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            if (v == sp.call_value) {
                found_suspend = true;
                break;
            }
            try defined_before.put(f.allocator, v, {});
        }
        if (found_suspend) break;
    }

    // Find values used after the suspend point
    var after_suspend = false;
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            if (v == sp.call_value) {
                after_suspend = true;
                continue;
            }
            if (!after_suspend) continue;

            // Check if any argument of v was defined before the suspend
            for (v.args) |arg| {
                if (defined_before.contains(arg)) {
                    // This value is live across the suspension
                    // Avoid duplicates
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
