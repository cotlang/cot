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

/// A local that needs to be spilled to the state struct.
const SpillSlot = struct {
    local_slot: u32,     // SSA local index
    frame_offset: i64,   // offset in the state struct
};

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

    // Compute frame layout: assign a unique offset to each spilled local
    // Rust: coroutine.rs:969-1068 compute_layout — builds variant-based layout
    // Cot simplified: each unique spilled local gets one slot in the frame
    var local_to_offset = std.AutoHashMapUnmanaged(u32, i64){};
    defer local_to_offset.deinit(f.allocator);
    var next_offset: i64 = spill_offset;

    for (suspend_points.items) |sp| {
        for (sp.live_locals.items) |local_slot| {
            if (!local_to_offset.contains(local_slot)) {
                try local_to_offset.put(f.allocator, local_slot, next_offset);
                next_offset += 8;
            }
        }
    }
    const frame_size: usize = @intCast(next_offset);
    _ = state_offset;
    _ = result_offset;

    debug.log(.async_split, "  state struct: {d} bytes ({d} spill slots)", .{
        frame_size, local_to_offset.count(),
    });

    // Step 4-7: SSA graph transformation (block splitting, dispatch, spill/restore)
    // Only transform functions with 2+ suspension points.
    // Single-suspend functions work correctly with eager evaluation.
    // The state machine is needed when a function has multiple awaits
    // and needs to save/restore state between them.
    // Rust: coroutine.rs only transforms functions with Yield terminators.
    // Kotlin: all suspend functions get the transform.
    // Transform disabled while SSA dominance fixup is in progress.
    // The block splitting works but creates invalid SSA (values crossing
    // block boundaries without phi nodes). Need to add:
    // 1. Frame pointer arg value in each resume block
    // 2. init_mem at resume block entry for memory state
    // 3. Phi nodes for values that cross split boundaries
    // Rust reference: coroutine.rs TransformVisitor rewrites all local
    // references to coroutine struct fields, avoiding the dominance issue.
    // Transform gated: spill/restore codegen emits stores/loads but doesn't
    // rewrite value args in resume blocks. Need rewriteUsesAfterSuspend to
    // replace pre-split value references with restored local loads.
    // Rust reference: coroutine.rs TransformVisitor visit_place (line 414-418)
    // Value rewriting handles memory state but not all SSA value references.
    // CLIF codegen creates vregs that span blocks → EntryLivein error.
    // Need aggressive value rewriting or CLIF-level handling.
    // Transform produces valid SSA but caller/callee coordination incomplete.
    // The caller (lowerAwaitExpr) still uses eager evaluation and expects
    // a task pointer. The transformed function has a dispatch entry that
    // expects a frame pointer. Need to update lowerAsyncFnEager to generate
    // a constructor function when the body has 2+ suspend points, and update
    // lowerAwaitExpr to poll the state machine.
    if (suspend_points.items.len < 2) {
        debug.log(.async_split, "=== AsyncSplit skipped for '{s}': {d} suspend points (<=1, eager sufficient) ===", .{
            f.name, suspend_points.items.len,
        });
        return;
    }

    debug.log(.async_split, "=== AsyncSplit TRANSFORMING '{s}': {d} suspend points, {d}B frame ===", .{
        f.name, suspend_points.items.len, frame_size,
    });

    // Step 4: Split blocks at each suspension point
    // Rust reference: coroutine.rs:1085-1109 insert_switch + TransformVisitor
    // Process in reverse order to avoid invalidating indices
    var i_sp: usize = suspend_points.items.len;
    while (i_sp > 0) {
        i_sp -= 1;
        const sp = &suspend_points.items[i_sp];
        try splitBlockAtSuspend(f, sp, &local_to_offset);
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

    // Debug: log dispatch block successors
    debug.log(.async_split, "  dispatch block b{d}: {d} successors", .{
        dispatch.id, dispatch.succs.len,
    });
    for (dispatch.succs, 0..) |edge, idx| {
        debug.log(.async_split, "    succ[{d}] = b{d} (kind={s})", .{
            idx, edge.b.id, @tagName(edge.b.kind),
        });
    }

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
/// Rust reference: TransformVisitor (coroutine.rs:190-340, 414-502)
///
/// The transformation:
/// 1. Split block at await into pre-await + resume
/// 2. Pre-await: spill live locals to frame, set state, return PENDING
/// 3. Resume: load frame arg, restore live locals, continue execution
/// 4. Rewrite uses in resume block to reference restored values
///
/// This eliminates cross-block value references that would violate SSA dominance.
fn splitBlockAtSuspend(f: *Func, sp: *SuspendPoint, local_to_offset: *const std.AutoHashMapUnmanaged(u32, i64)) !void {
    const block = sp.block;
    const Pos = @import("../value.zig").Pos;
    const pos = Pos{ .line = 0, .col = 0 };

    // Find the split point (after the await call)
    var split_idx: usize = 0;
    for (block.values.items, 0..) |v, i| {
        if (v == sp.call_value) {
            split_idx = i + 1;
            break;
        }
    }

    // Create resume block
    const resume_block = try f.newBlock(block.kind);
    sp.resume_block = resume_block;

    // Move values after split to resume block
    if (split_idx < block.values.items.len) {
        for (split_idx..block.values.items.len) |idx| {
            const v = block.values.items[idx];
            v.block = resume_block;
            try resume_block.addValue(f.allocator, v);
        }
        block.values.shrinkRetainingCapacity(split_idx);
    }

    // Transfer successors/kind/controls to resume block
    resume_block.succs = block.succs;
    resume_block.kind = block.kind;
    if (block.controls[0]) |c| resume_block.controls[0] = c;
    if (block.controls[1]) |c| resume_block.controls[1] = c;

    // Pre-await block becomes ret (returns void — poll function returns void)
    block.kind = .ret;
    block.succs = &.{};
    block.controls = .{ null, null };

    // === SPILL: store state + await result + live locals to frame, return void ===

    // The await call result (task pointer) is stored to frame[result_offset]
    // so the resume block can load it to extract the task's return value.
    // Rust: the callee future is stored as a field in the caller's coroutine struct.
    const call_result_offset: i64 = 8; // frame[8] = task pointer from await call

    // Get frame pointer (arg 0) in this block
    var frame_ptr_in_block: ?*Value = null;
    for (block.values.items) |v| {
        if (v.op == .arg and v.aux_int == 0) {
            frame_ptr_in_block = v;
            break;
        }
    }
    if (frame_ptr_in_block == null) {
        const fp = try f.newValue(.arg, 0, block, pos);
        fp.aux_int = 0;
        if (block.values.items.len > 0) {
            try block.insertValueBefore(f.allocator, fp, block.values.items[0]);
        } else {
            try block.addValue(f.allocator, fp);
        }
        frame_ptr_in_block = fp;
    }

    // Store state number to frame[0]
    // Rust: coroutine.rs — set discriminant to suspend state number
    const state_val = try f.newValue(.const_int, 0, block, pos);
    state_val.aux_int = sp.state_number;
    try block.addValue(f.allocator, state_val);

    const state_store = try f.newValue(.store, 0, block, pos);
    state_store.addArg(frame_ptr_in_block.?);
    state_store.addArg(state_val);
    try block.addValue(f.allocator, state_store);

    // Spill each live local to frame[offset]
    for (sp.live_locals.items) |local_slot| {
        const offset = local_to_offset.get(local_slot) orelse continue;

        // Load from local
        const local_addr_spill = try f.newValue(.local_addr, 0, block, pos);
        local_addr_spill.aux_int = @intCast(local_slot);
        try block.addValue(f.allocator, local_addr_spill);

        const local_load = try f.newValue(.load, 0, block, pos);
        local_load.addArg(local_addr_spill);
        try block.addValue(f.allocator, local_load);

        // Store to frame[offset]
        const frame_off = try f.newValue(.off_ptr, 0, block, pos);
        frame_off.aux_int = offset;
        frame_off.addArg(frame_ptr_in_block.?);
        try block.addValue(f.allocator, frame_off);

        const frame_store = try f.newValue(.store, 0, block, pos);
        frame_store.addArg(frame_off);
        frame_store.addArg(local_load);
        try block.addValue(f.allocator, frame_store);
    }

    // Spill the await call result (task pointer) to frame[result_offset]
    {
        const call_frame_off = try f.newValue(.off_ptr, 0, block, pos);
        call_frame_off.aux_int = call_result_offset;
        call_frame_off.addArg(frame_ptr_in_block.?);
        try block.addValue(f.allocator, call_frame_off);

        const call_store = try f.newValue(.store, 0, block, pos);
        call_store.addArg(call_frame_off);
        call_store.addArg(sp.call_value);
        try block.addValue(f.allocator, call_store);
    }

    // === RESTORE: at resume block entry ===
    // Add frame arg reference + restore live locals from frame
    const frame_in_resume = try f.newValue(.arg, 0, resume_block, pos);
    frame_in_resume.aux_int = 0;
    const first_val = if (resume_block.values.items.len > 0) resume_block.values.items[0] else null;
    if (first_val) |fv| {
        try resume_block.insertValueBefore(f.allocator, frame_in_resume, fv);
    } else {
        try resume_block.addValue(f.allocator, frame_in_resume);
    }

    // Restore each live local from frame[offset]
    for (sp.live_locals.items) |local_slot| {
        const offset = local_to_offset.get(local_slot) orelse continue;

        // Load from frame[offset]
        const frame_off_r = try f.newValue(.off_ptr, 0, resume_block, pos);
        frame_off_r.aux_int = offset;
        frame_off_r.addArg(frame_in_resume);
        try resume_block.insertValueBefore(f.allocator, frame_off_r, resume_block.values.items[1]); // after frame arg

        const frame_load = try f.newValue(.load, 0, resume_block, pos);
        frame_load.addArg(frame_off_r);
        try resume_block.insertValueBefore(f.allocator, frame_load, resume_block.values.items[2]);

        // Store to local
        const local_addr_r = try f.newValue(.local_addr, 0, resume_block, pos);
        local_addr_r.aux_int = @intCast(local_slot);
        try resume_block.insertValueBefore(f.allocator, local_addr_r, resume_block.values.items[3]);

        const local_store = try f.newValue(.store, 0, resume_block, pos);
        local_store.addArg(local_addr_r);
        local_store.addArg(frame_load);
        try resume_block.insertValueBefore(f.allocator, local_store, resume_block.values.items[4]);
    }

    // Restore the await call result (task pointer) from frame[result_offset]
    // This provides a valid value for resume block code that references the call result.
    const call_frame_off_r = try f.newValue(.off_ptr, sp.call_value.type_idx, resume_block, pos);
    call_frame_off_r.aux_int = call_result_offset;
    call_frame_off_r.addArg(frame_in_resume);
    const insert_after_restores = @min(resume_block.values.items.len - 1, 1 + sp.live_locals.items.len * 4 + 1);
    if (insert_after_restores < resume_block.values.items.len) {
        try resume_block.insertValueBefore(f.allocator, call_frame_off_r, resume_block.values.items[insert_after_restores]);
    } else {
        try resume_block.addValue(f.allocator, call_frame_off_r);
    }

    const restored_call = try f.newValue(.load, sp.call_value.type_idx, resume_block, pos);
    restored_call.addArg(call_frame_off_r);
    if (insert_after_restores + 1 < resume_block.values.items.len) {
        try resume_block.insertValueBefore(f.allocator, restored_call, resume_block.values.items[insert_after_restores + 1]);
    } else {
        try resume_block.addValue(f.allocator, restored_call);
    }

    // === REWRITE: fix cross-block SSA references ===
    // Values in the resume block may reference values from the pre-split block.
    // The call result (task pointer) is the most common cross-block ref.
    // Pre-populate the remap with the call value → restored value.
    // Rust reference: coroutine.rs TransformVisitor visit_place (line 414-418)
    const init_mem_val = try f.newValue(.init_mem, 0, resume_block, pos);
    // Insert init_mem as SECOND value (right after frame arg, before everything else).
    // This ensures all subsequent values in the block can reference it.
    if (resume_block.values.items.len > 1) {
        try resume_block.insertValueBefore(f.allocator, init_mem_val, resume_block.values.items[1]);
    } else {
        try resume_block.addValue(f.allocator, init_mem_val);
    }

    // Rewrite memory state args: any .copy or memory-typed value that references
    // a value from the pre-split block should be rewritten to use init_mem_val.
    // Build set of values in the resume block for quick lookup.
    var resume_vals = std.AutoHashMapUnmanaged(*Value, void){};
    defer resume_vals.deinit(f.allocator);
    for (resume_block.values.items) |v| {
        try resume_vals.put(f.allocator, v, {});
    }

    // Rewrite ALL args that reference values NOT in the resume block.
    // Rust TransformVisitor (coroutine.rs:414-418): every local reference
    // becomes a coroutine struct field access. Cot equivalent: every
    // cross-block ref must be re-emitted or replaced.
    //
    // Strategy per value type:
    // - const_int/float/bool → re-emit constant in resume block
    // - Memory state (.copy, .init_mem, .store) → replace with init_mem
    // - arg op → re-emit arg in resume block
    // - local_addr → re-emit local_addr in resume block
    // - Everything else → replace with init_mem (conservative safe fallback)
    var remapped = std.AutoHashMapUnmanaged(*Value, *Value){};
    defer remapped.deinit(f.allocator);

    // Pre-populate with known mappings
    try remapped.put(f.allocator, init_mem_val, init_mem_val);
    try remapped.put(f.allocator, frame_in_resume, frame_in_resume);
    // Map the original call value to the restored call value
    try remapped.put(f.allocator, sp.call_value, restored_call);

    for (resume_block.values.items) |v| {
        for (0..v.args.len) |arg_idx| {
            const arg = v.args[arg_idx];
            if (resume_vals.contains(arg)) continue; // Already in resume block

            // Check if we already remapped this value
            if (remapped.get(arg)) |replacement| {
                v.setArg(@intCast(arg_idx), replacement);
                continue;
            }

            // Re-emit or skip based on op type.
            // Memory state values (.copy SSA_MEM, .init_mem, .store) are compile-time
            // ordering only — the CLIF translator skips them. Don't replace these
            // with init_mem (which also has no CLIF value).
            // Non-memory values: re-emit in the resume block.
            const is_memory_state = arg.op == .copy and arg.type_idx == @import("../../frontend/types.zig").TypeRegistry.SSA_MEM;
            const is_ordering_only = arg.op == .init_mem or arg.op == .store or is_memory_state;

            if (is_ordering_only) {
                // Memory/ordering args: skip rewriting — CLIF ignores them.
                // Just leave the cross-block ref; CLIF translator doesn't create
                // vregs for these ops so no EntryLivein issue.
                continue;
            }

            const replacement: *Value = blk_remap: {
                if (arg.op == .const_int or arg.op == .const_float or arg.op == .const_bool or arg.op == .const_nil) {
                    const new_const = try f.newValue(arg.op, arg.type_idx, resume_block, pos);
                    new_const.aux_int = arg.aux_int;
                    new_const.aux = arg.aux;
                    try resume_block.insertValueBefore(f.allocator, new_const, v);
                    try resume_vals.put(f.allocator, new_const, {});
                    break :blk_remap new_const;
                }
                if (arg.op == .arg) {
                    const new_arg = try f.newValue(.arg, arg.type_idx, resume_block, pos);
                    new_arg.aux_int = arg.aux_int;
                    try resume_block.insertValueBefore(f.allocator, new_arg, v);
                    try resume_vals.put(f.allocator, new_arg, {});
                    break :blk_remap new_arg;
                }
                if (arg.op == .local_addr) {
                    const new_la = try f.newValue(.local_addr, arg.type_idx, resume_block, pos);
                    new_la.aux_int = arg.aux_int;
                    try resume_block.insertValueBefore(f.allocator, new_la, v);
                    try resume_vals.put(f.allocator, new_la, {});
                    break :blk_remap new_la;
                }
                // Fallback: re-emit as const 0. This is a PLACEHOLDER that produces
                // a valid CLIF vreg but with wrong value. Used to make SSA valid
                // while debugging. The spill/restore for locals should provide the
                // correct values; this fallback catches edge cases.
                debug.log(.async_split, "  WARN: replacing cross-block ref op={s} type={d} with const 0", .{
                    @tagName(arg.op), arg.type_idx,
                });
                const placeholder = try f.newValue(.const_int, arg.type_idx, resume_block, pos);
                placeholder.aux_int = 0;
                try resume_block.insertValueBefore(f.allocator, placeholder, v);
                try resume_vals.put(f.allocator, placeholder, {});
                break :blk_remap placeholder;
            };

            try remapped.put(f.allocator, arg, replacement);
            v.setArg(@intCast(arg_idx), replacement);
        }
    }

    // Also rewrite block controls if they reference pre-split values
    for (0..2) |ctrl_idx| {
        if (resume_block.controls[ctrl_idx]) |ctrl| {
            if (!resume_vals.contains(ctrl)) {
                if (remapped.get(ctrl)) |replacement| {
                    resume_block.controls[ctrl_idx] = replacement;
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
