//! Schedule Pass - Order values within blocks for emission.
//!
//! Go reference: cmd/compile/internal/ssa/schedule.go
//!
//! After this pass, b.values order is the final emission order.
//! Algorithm: priority-based topological sort respecting data dependencies.

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const debug = @import("../../pipeline_debug.zig");

/// Priority scores - lower numbers scheduled earlier (Go's pattern).
pub const Score = enum(i8) {
    phi = 0, // Phis must be first
    arg = 1, // Arguments early (entry block)
    read_tuple = 2, // select_n must follow call immediately
    memory = 3, // Stores early (reduces register pressure)
    default = 4, // Normal instructions
    control = 5, // Branch/return last
};

/// Get the scheduling score for a value.
fn getScore(v: *Value, is_control: bool) Score {
    if (is_control) return .control;
    return switch (v.op) {
        .phi => .phi,
        .arg => .arg,
        .select_n => .read_tuple,
        .store, .store_reg => .memory,
        else => .default,
    };
}

/// Schedule all blocks in a function.
pub fn schedule(f: *Func) !void {
    debug.log(.schedule, "=== Schedule pass for '{s}' ===", .{f.name});

    for (f.blocks.items) |block| {
        try scheduleBlock(f.allocator, block, f);
    }

    debug.log(.schedule, "=== Schedule complete ===", .{});
}

fn scheduleBlock(allocator: std.mem.Allocator, block: *Block, f: *Func) !void {
    const values = block.values.items;
    if (values.len == 0) return;

    debug.log(.schedule, "  block b{d}: {d} values", .{ block.id, values.len });

    // Compute scores and original positions
    var scores = try allocator.alloc(Score, f.vid.next_id);
    defer allocator.free(scores);
    for (scores) |*s| s.* = .default;

    var orig_pos = try allocator.alloc(u32, f.vid.next_id);
    defer allocator.free(orig_pos);
    for (orig_pos) |*p| p.* = std.math.maxInt(u32);

    // Track control values
    var control_ids = std.AutoHashMapUnmanaged(u32, void){};
    defer control_ids.deinit(allocator);
    for (block.controlValues()) |ctrl| {
        try control_ids.put(allocator, ctrl.id, {});
    }

    for (values, 0..) |v, i| {
        orig_pos[v.id] = @intCast(i);
        scores[v.id] = getScore(v, control_ids.contains(v.id));
    }

    // Build dependency edges (in-block only)
    const Edge = struct { x: *Value, y: *Value };
    var edges = std.ArrayListUnmanaged(Edge){};
    defer edges.deinit(allocator);

    for (values) |v| {
        if (v.op == .phi) continue; // Phi args from predecessors
        for (v.args) |arg| {
            if (arg.block == block) {
                try edges.append(allocator, .{ .x = arg, .y = v });
            }
        }
    }

    // Memory ordering: chain stores, loads, and calls.
    // Calls are memory barriers: they may read/write any memory (via pointers).
    // Go reference: schedule.go treats calls as both reads and writes.
    var last_mem: ?*Value = null;
    for (values) |v| {
        if (v.op == .store or v.op == .store_reg or
            v.op == .wasm_i64_store or v.op == .wasm_i64_store8 or
            v.op == .wasm_i64_store16 or v.op == .wasm_i64_store32)
        {
            if (last_mem) |lm| try edges.append(allocator, .{ .x = lm, .y = v });
            last_mem = v;
        } else if (v.op == .load or v.op == .load_reg or
            v.op == .wasm_i64_load or v.op == .wasm_i64_load8_u or
            v.op == .wasm_i64_load16_u or v.op == .wasm_i64_load32_u or
            v.op == .wasm_i64_load8_s or v.op == .wasm_i64_load16_s or
            v.op == .wasm_i64_load32_s)
        {
            if (last_mem) |lm| try edges.append(allocator, .{ .x = lm, .y = v });
        } else if (v.op == .static_call or v.op == .wasm_lowered_static_call or
            v.op == .call or v.op == .wasm_call or
            v.op == .closure_call or v.op == .wasm_lowered_closure_call or
            v.op == .inter_call or v.op == .wasm_lowered_inter_call or
            v.op == .move or v.op == .wasm_lowered_move or
            v.op == .zero or v.op == .wasm_lowered_zero)
        {
            // Calls are memory barriers: must order after prior stores/loads
            // and before subsequent loads/stores.
            // move (memory.copy) and zero (memory.fill) are bulk memory ops
            // that both read and write — treat as barriers.
            if (last_mem) |lm| try edges.append(allocator, .{ .x = lm, .y = v });
            last_mem = v;
        }
    }

    // Count incoming edges
    var in_edges = try allocator.alloc(u32, f.vid.next_id);
    defer allocator.free(in_edges);
    for (in_edges) |*e| e.* = 0;
    for (edges.items) |e| in_edges[e.y.id] += 1;

    // Initialize ready set
    var ready = std.ArrayListUnmanaged(*Value){};
    defer ready.deinit(allocator);
    for (values) |v| {
        if (in_edges[v.id] == 0) try ready.append(allocator, v);
    }

    // Process in priority order (lowest score first, then original position)
    var result = std.ArrayListUnmanaged(*Value){};
    defer result.deinit(allocator);

    while (ready.items.len > 0) {
        // Find best candidate
        var best_idx: usize = 0;
        var best_score = scores[ready.items[0].id];
        var best_pos = orig_pos[ready.items[0].id];

        for (ready.items[1..], 1..) |v, i| {
            const s = scores[v.id];
            const p = orig_pos[v.id];
            if (@intFromEnum(s) < @intFromEnum(best_score) or
                (@intFromEnum(s) == @intFromEnum(best_score) and p < best_pos))
            {
                best_score = s;
                best_pos = p;
                best_idx = i;
            }
        }

        const v = ready.swapRemove(best_idx);
        try result.append(allocator, v);

        // Update ready set with newly unblocked values
        for (edges.items) |e| {
            if (e.x == v) {
                in_edges[e.y.id] -= 1;
                if (in_edges[e.y.id] == 0) try ready.append(allocator, e.y);
            }
        }
    }

    if (result.items.len != values.len) {
        debug.log(.schedule, "ERROR: scheduled {d} of {d} values", .{ result.items.len, values.len });
        return error.ScheduleIncomplete;
    }

    // Replace block's values with scheduled order
    block.values.clearRetainingCapacity();
    try block.values.appendSlice(allocator, result.items);
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "Score ordering" {
    // Verify score order matches expected emission order
    try testing.expect(@intFromEnum(Score.phi) < @intFromEnum(Score.arg));
    try testing.expect(@intFromEnum(Score.arg) < @intFromEnum(Score.memory));
    try testing.expect(@intFromEnum(Score.memory) < @intFromEnum(Score.default));
    try testing.expect(@intFromEnum(Score.default) < @intFromEnum(Score.control));
}

test "getScore returns correct priorities" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.first);

    // Create values and add to block so they're cleaned up by f.deinit()
    const phi = try f.newValue(.phi, 0, b, .{});
    try b.addValue(allocator, phi);
    const arg = try f.newValue(.arg, 0, b, .{});
    try b.addValue(allocator, arg);
    const add = try f.newValue(.add, 0, b, .{});
    try b.addValue(allocator, add);
    const store = try f.newValue(.store, 0, b, .{});
    try b.addValue(allocator, store);

    try testing.expectEqual(Score.phi, getScore(phi, false));
    try testing.expectEqual(Score.arg, getScore(arg, false));
    try testing.expectEqual(Score.default, getScore(add, false));
    try testing.expectEqual(Score.memory, getScore(store, false));
    try testing.expectEqual(Score.control, getScore(add, true)); // control overrides
}

test "schedule empty function" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "empty");
    defer f.deinit();

    const b = try f.newBlock(.first);

    try schedule(&f);
    try testing.expectEqual(@as(usize, 0), b.values.items.len);
}

test "schedule preserves dependencies" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "deps");
    defer f.deinit();

    const b = try f.newBlock(.first);

    // Create: v1 = const, v2 = add v1 v1
    const v1 = try f.newValue(.const_int, 0, b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.add, 0, b, .{});
    v2.addArg(v1);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    try schedule(&f);

    // v1 must come before v2
    var v1_pos: ?usize = null;
    var v2_pos: ?usize = null;
    for (b.values.items, 0..) |v, i| {
        if (v.id == v1.id) v1_pos = i;
        if (v.id == v2.id) v2_pos = i;
    }

    try testing.expect(v1_pos.? < v2_pos.?);
}
