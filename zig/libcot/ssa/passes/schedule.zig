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
const frontend_types = @import("../../frontend/types.zig");
const debug = @import("../../debug.zig");

/// Priority scores - lower numbers scheduled earlier.
/// Go reference: schedule.go:16-28 (ScorePhi through ScoreControl).
pub const Score = enum(i8) {
    phi = 0, // Phis must be first
    arg = 1, // Arguments early (entry block)
    init_mem = 2, // After args — Go: ScoreInitMem (debug info marker)
    read_tuple = 3, // select_n must follow call immediately
    nil_check = 4, // Nil checks before loads from same address
    memory = 5, // Stores/calls early (reduces register pressure)
    default = 6, // Normal instructions
    control = 7, // Branch/return last
};

/// Get the scheduling score for a value.
/// Go reference: schedule.go:137-212.
fn getScore(v: *Value, is_control: bool) Score {
    if (is_control) return .control;
    // Go: v.Type.IsMemory() → ScoreMemory (includes stores AND calls)
    if (v.type_idx == frontend_types.TypeRegistry.SSA_MEM or v.writesMemory()) return .memory;
    return switch (v.op) {
        .phi => .phi,
        .arg => .arg,
        .init_mem => .init_mem,
        .select_n => .read_tuple,
        .nil_check => .nil_check,
        else => .default,
    };
}

/// Schedule all blocks in a function.
pub fn schedule(f: *Func) !void {
    var total_values: usize = 0;
    for (f.blocks.items) |b| total_values += b.values.items.len;
    debug.log(.schedule, "=== Schedule pass for '{s}' ({d} blocks, {d} values) ===", .{
        f.name, f.blocks.items.len, total_values,
    });

    for (f.blocks.items) |block| {
        try scheduleBlock(f.allocator, block, f);
    }
    f.scheduled = true;

    debug.log(.schedule, "=== Schedule complete for '{s}' ===", .{f.name});
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

    // Go schedule.go:257-264 — build nextMem: maps memory input → next memory output.
    // Go uses v.Type.IsMemory() which covers stores (TypeMem) and calls (tuple with TypeMem).
    // Cot equivalent: stores/moves have type SSA_MEM, calls have writes_memory flag.
    // We check both to cover all memory-producing ops.
    var next_mem = try allocator.alloc(?*Value, f.vid.next_id);
    defer allocator.free(next_mem);
    @memset(next_mem, null);

    for (values) |v| {
        if (v.op == .phi or v.op == .init_mem) continue;
        // Go: v.Type.IsMemory() — true for stores AND calls
        const is_mem_producer = v.type_idx == frontend_types.TypeRegistry.SSA_MEM or v.writesMemory();
        if (is_mem_producer) {
            if (v.memoryArg()) |mem_arg| {
                next_mem[mem_arg.id] = v;
            }
        }
    }

    // Go schedule.go:266-278 — add load→store edges.
    // For every non-phi, non-memory-producing value that has a memory arg:
    // the value must be scheduled before the next memory op in the chain.
    for (values) |v| {
        if (v.op == .phi) continue;
        const is_mem_producer = v.type_idx == frontend_types.TypeRegistry.SSA_MEM or v.writesMemory();
        if (is_mem_producer) continue; // skip memory producers (Go: v.Type.IsMemory())
        const mem_arg = v.memoryArg() orelse continue;
        if (next_mem[mem_arg.id]) |next_store| {
            if (next_store.block == block) {
                try edges.append(allocator, .{ .x = v, .y = next_store });
            }
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
        debug.log(.schedule, "ERROR in block b{d}: scheduled {d} of {d} values", .{ block.id, result.items.len, values.len });
        // Print stuck values (in_edges > 0)
        for (values) |v| {
            if (in_edges[v.id] > 0) {
                debug.log(.schedule, "  STUCK v{d} (op={s}, in_edges={d}, args={d})", .{
                    v.id, @tagName(v.op), in_edges[v.id], v.args.len,
                });
                for (v.args, 0..) |arg, ai| {
                    debug.log(.schedule, "    arg[{d}] = v{d} (op={s}, block=b{d}, in_edges={d})", .{
                        ai, arg.id, @tagName(arg.op),
                        if (arg.block) |ab| ab.id else 999,
                        in_edges[arg.id],
                    });
                }
            }
        }
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
    // Verify score order matches expected emission order (Go schedule.go:16-28)
    try testing.expect(@intFromEnum(Score.phi) < @intFromEnum(Score.arg));
    try testing.expect(@intFromEnum(Score.arg) < @intFromEnum(Score.init_mem));
    try testing.expect(@intFromEnum(Score.init_mem) < @intFromEnum(Score.read_tuple));
    try testing.expect(@intFromEnum(Score.read_tuple) < @intFromEnum(Score.memory));
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
    // Store with SSA_MEM type (Go: stores produce TypeMem)
    const store = try f.newValue(.store, frontend_types.TypeRegistry.SSA_MEM, b, .{});
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
