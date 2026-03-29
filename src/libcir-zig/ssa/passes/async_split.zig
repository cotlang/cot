//! Async State Machine Splitting Pass
//!
//! Transforms an async function's SSA into a state machine with explicit
//! suspension points at each await operation.
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
//! State numbers:
//!   0 = unresumed (initial state, begin execution)
//!   1 = returned (execution complete, result available)
//!   2 = poisoned (panicked during execution)
//!   3+ = suspended at await point N-3

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const BlockKind = @import("../block.zig").BlockKind;
const value_mod = @import("../value.zig");
const Value = value_mod.Value;
const TypeIndex = value_mod.TypeIndex;
const Pos = value_mod.Pos;
const Op = @import("../op.zig").Op;
const foundation = @import("foundation");
const debug = foundation.debug;
const TypeRegistry = foundation.types.TypeRegistry;

const STATE_UNRESUMED: i64 = 0;
const STATE_RETURNED: i64 = 1;
const STATE_POISONED: i64 = 2;
const STATE_FIRST_SUSPEND: i64 = 3;

const SpillSlot = struct {
    local_slot: u32,
    frame_offset: i64,
};

const SuspendPoint = struct {
    block: *Block,
    call_value: *Value,
    live_values: std.ArrayListUnmanaged(*Value),
    live_locals: std.ArrayListUnmanaged(u32),
    state_number: i64,
    resume_block: ?*Block,
};

pub fn needsSplitting(f: *const Func, type_registry: *const TypeRegistry) bool {
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            if (isAwaitCall(v, type_registry)) return true;
        }
    }
    return false;
}

fn isAwaitCall(v: *const Value, type_registry: *const TypeRegistry) bool {
    if (!v.op.isCall()) return false;
    const ret_type = type_registry.get(v.type_idx);
    return ret_type == .task;
}

pub fn asyncSplit(f: *Func, type_registry: *const TypeRegistry) !void {
    if (!needsSplitting(f, type_registry)) return;

    debug.log(.async_split, "=== AsyncSplit pass for '{s}' ===", .{f.name});

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

    for (suspend_points.items) |*sp| {
        try computeLiveValues(f, sp);
        try computeLiveLocals(f, sp);
        debug.log(.async_split, "  suspend point state={d}: {d} live values, {d} live locals to spill", .{
            sp.state_number, sp.live_values.items.len, sp.live_locals.items.len,
        });
    }

    const result_offset: i64 = 0;
    const state_offset: i64 = 8;
    const call_result_offset: i64 = 16;
    const spill_offset: i64 = 24;

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
    _ = result_offset;

    debug.log(.async_split, "  state struct: {d} bytes ({d} spill slots)", .{
        frame_size, local_to_offset.count(),
    });

    if (suspend_points.items.len == 0) {
        debug.log(.async_split, "=== AsyncSplit skipped for '{s}': 0 suspend points (no awaits) ===", .{f.name});
        return;
    }

    if (!std.mem.endsWith(u8, f.name, "__poll")) {
        debug.log(.async_split, "=== AsyncSplit skipped for '{s}': not a __poll function, using eager evaluation ===", .{f.name});
        return;
    }

    debug.log(.async_split, "=== AsyncSplit TRANSFORMING '{s}': {d} suspend points, {d}B frame ===", .{
        f.name, suspend_points.items.len, frame_size,
    });

    var i_sp: usize = suspend_points.items.len;
    while (i_sp > 0) {
        i_sp -= 1;
        const sp = &suspend_points.items[i_sp];
        try splitBlockAtSuspend(f, sp, &local_to_offset, state_offset, call_result_offset);
        debug.log(.async_split, "  split block at state={d}, resume block created", .{sp.state_number});
    }

    const original_entry = f.entry orelse return;
    const pos = Pos{ .line = 0, .col = 0 };

    var prev_block = original_entry;
    var sp_idx: usize = suspend_points.items.len;
    while (sp_idx > 0) {
        sp_idx -= 1;
        const sp = suspend_points.items[sp_idx];
        const rb = sp.resume_block orelse continue;

        const check_block = try f.newBlock(.if_);

        const frame_arg = try f.newValue(.arg, 0, check_block, pos);
        frame_arg.aux_int = 0;
        try check_block.addValue(f.allocator, frame_arg);

        const frame_state_addr = try f.newValue(.off_ptr, 0, check_block, pos);
        frame_state_addr.aux_int = state_offset;
        frame_state_addr.addArg(frame_arg);
        try check_block.addValue(f.allocator, frame_state_addr);

        const state_load = try f.newValue(.load, 0, check_block, pos);
        state_load.addArg(frame_state_addr);
        try check_block.addValue(f.allocator, state_load);

        const state_const = try f.newValue(.const_int, 0, check_block, pos);
        state_const.aux_int = sp.state_number;
        try check_block.addValue(f.allocator, state_const);

        const cmp = try f.newValue(.eq, 0, check_block, pos);
        cmp.addArg2(state_load, state_const);
        try check_block.addValue(f.allocator, cmp);

        check_block.setControl(cmp);

        try check_block.addEdgeTo(f.allocator, rb);
        try check_block.addEdgeTo(f.allocator, prev_block);

        prev_block = check_block;
    }

    f.entry = prev_block;
    f.invalidateCFG();

    debug.log(.async_split, "  dispatch: if/else chain with {d} checks, entry=b{d}", .{
        suspend_points.items.len, prev_block.id,
    });

    debug.log(.async_split, "=== AsyncSplit complete for '{s}': {d} suspend points, {d}B frame ===", .{
        f.name, suspend_points.items.len, frame_size,
    });
}

fn computeLiveValues(f: *const Func, sp: *SuspendPoint) !void {
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

    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            const v_pos = positions.get(v) orelse continue;
            if (v_pos <= suspend_pos) continue;

            for (v.args) |arg| {
                const arg_pos = positions.get(arg) orelse continue;
                if (arg_pos < suspend_pos and arg != sp.call_value) {
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

fn splitBlockAtSuspend(f: *Func, sp: *SuspendPoint, local_to_offset: *const std.AutoHashMapUnmanaged(u32, i64), state_offset: i64, call_result_offset: i64) !void {
    const block = sp.block;
    const pos = Pos{ .line = 0, .col = 0 };

    var split_idx: usize = 0;
    for (block.values.items, 0..) |v, i| {
        if (v == sp.call_value) {
            split_idx = i + 1;
            break;
        }
    }

    const resume_block = try f.newBlock(block.kind);
    sp.resume_block = resume_block;

    if (split_idx < block.values.items.len) {
        for (split_idx..block.values.items.len) |idx| {
            const v = block.values.items[idx];
            v.block = resume_block;
            try resume_block.addValue(f.allocator, v);
        }
        block.values.shrinkRetainingCapacity(split_idx);
    }

    resume_block.succs = block.succs;
    for (resume_block.succs) |edge| {
        for (edge.b.preds) |*pred| {
            if (pred.b == block) {
                pred.b = resume_block;
                break;
            }
        }
    }
    resume_block.kind = block.kind;
    if (block.controls[0]) |c| resume_block.controls[0] = c;
    if (block.controls[1]) |c| resume_block.controls[1] = c;

    block.kind = .ret;
    block.succs = &.{};
    block.controls = .{ null, null };

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

    const state_val = try f.newValue(.const_int, 0, block, pos);
    state_val.aux_int = sp.state_number;
    try block.addValue(f.allocator, state_val);

    const state_addr = try f.newValue(.off_ptr, 0, block, pos);
    state_addr.aux_int = state_offset;
    state_addr.addArg(frame_ptr_in_block.?);
    try block.addValue(f.allocator, state_addr);

    const state_store = try f.newValue(.store, 0, block, pos);
    state_store.addArg(state_addr);
    state_store.addArg(state_val);
    try block.addValue(f.allocator, state_store);

    for (sp.live_locals.items) |local_slot| {
        const offset = local_to_offset.get(local_slot) orelse continue;

        const local_addr_spill = try f.newValue(.local_addr, 0, block, pos);
        local_addr_spill.aux_int = @intCast(local_slot);
        try block.addValue(f.allocator, local_addr_spill);

        const local_load = try f.newValue(.load, 0, block, pos);
        local_load.addArg(local_addr_spill);
        try block.addValue(f.allocator, local_load);

        const frame_off = try f.newValue(.off_ptr, 0, block, pos);
        frame_off.aux_int = offset;
        frame_off.addArg(frame_ptr_in_block.?);
        try block.addValue(f.allocator, frame_off);

        const frame_store = try f.newValue(.store, 0, block, pos);
        frame_store.addArg(frame_off);
        frame_store.addArg(local_load);
        try block.addValue(f.allocator, frame_store);
    }

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

    {
        const yield_val = try f.newValue(.const_int, 0, block, pos);
        yield_val.aux_int = 0;
        try block.addValue(f.allocator, yield_val);
        block.controls[0] = yield_val;
    }

    const frame_in_resume = try f.newValue(.arg, 0, resume_block, pos);
    frame_in_resume.aux_int = 0;
    const first_val = if (resume_block.values.items.len > 0) resume_block.values.items[0] else null;
    if (first_val) |fv| {
        try resume_block.insertValueBefore(f.allocator, frame_in_resume, fv);
    } else {
        try resume_block.addValue(f.allocator, frame_in_resume);
    }

    for (sp.live_locals.items) |local_slot| {
        const offset = local_to_offset.get(local_slot) orelse continue;

        const frame_off_r = try f.newValue(.off_ptr, 0, resume_block, pos);
        frame_off_r.aux_int = offset;
        frame_off_r.addArg(frame_in_resume);
        try resume_block.insertValueBefore(f.allocator, frame_off_r, resume_block.values.items[1]);

        const frame_load = try f.newValue(.load, 0, resume_block, pos);
        frame_load.addArg(frame_off_r);
        try resume_block.insertValueBefore(f.allocator, frame_load, resume_block.values.items[2]);

        const local_addr_r = try f.newValue(.local_addr, 0, resume_block, pos);
        local_addr_r.aux_int = @intCast(local_slot);
        try resume_block.insertValueBefore(f.allocator, local_addr_r, resume_block.values.items[3]);

        const local_store = try f.newValue(.store, 0, resume_block, pos);
        local_store.addArg(local_addr_r);
        local_store.addArg(frame_load);
        try resume_block.insertValueBefore(f.allocator, local_store, resume_block.values.items[4]);
    }

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

    const init_mem_val = try f.newValue(.init_mem, 0, resume_block, pos);
    if (resume_block.values.items.len > 1) {
        try resume_block.insertValueBefore(f.allocator, init_mem_val, resume_block.values.items[1]);
    } else {
        try resume_block.addValue(f.allocator, init_mem_val);
    }

    var resume_vals = std.AutoHashMapUnmanaged(*Value, void){};
    defer resume_vals.deinit(f.allocator);
    for (resume_block.values.items) |v| {
        try resume_vals.put(f.allocator, v, {});
    }

    var remapped = std.AutoHashMapUnmanaged(*Value, *Value){};
    defer remapped.deinit(f.allocator);

    try remapped.put(f.allocator, init_mem_val, init_mem_val);
    try remapped.put(f.allocator, frame_in_resume, frame_in_resume);
    try remapped.put(f.allocator, sp.call_value, restored_call);

    for (resume_block.values.items) |v| {
        for (0..v.args.len) |arg_idx| {
            const arg = v.args[arg_idx];
            if (resume_vals.contains(arg)) continue;

            if (remapped.get(arg)) |replacement| {
                v.setArg(@intCast(arg_idx), replacement);
                continue;
            }

            const is_memory_state = arg.op == .copy and arg.type_idx == TypeRegistry.SSA_MEM;
            const is_ordering_only = arg.op == .init_mem or arg.op == .store or is_memory_state;

            if (is_ordering_only) {
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

fn computeLiveLocals(f: *const Func, sp: *SuspendPoint) !void {
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

    var stored_before = std.AutoHashMapUnmanaged(u32, void){};
    defer stored_before.deinit(f.allocator);
    var loaded_after = std.AutoHashMapUnmanaged(u32, void){};
    defer loaded_after.deinit(f.allocator);

    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            const v_pos = positions.get(v) orelse continue;

            if (v.op == .store and v_pos < suspend_pos) {
                if (v.args.len > 0 and v.args[0].op == .local_addr) {
                    const slot = @as(u32, @intCast(v.args[0].aux_int));
                    try stored_before.put(f.allocator, slot, {});
                }
            }

            if (v.op == .load and v_pos > suspend_pos) {
                if (v.args.len > 0 and v.args[0].op == .local_addr) {
                    const slot = @as(u32, @intCast(v.args[0].aux_int));
                    try loaded_after.put(f.allocator, slot, {});
                }
            }
        }
    }

    var iter = stored_before.keyIterator();
    while (iter.next()) |key| {
        if (loaded_after.contains(key.*)) {
            try sp.live_locals.append(f.allocator, key.*);
        }
    }
}
