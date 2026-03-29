//! Block Layout Pass - Orders blocks to minimize control flow instructions.
//!
//! After this pass, the order of func.blocks is the order to emit blocks.
//! The goal is to place successor blocks immediately after their predecessors
//! when possible, minimizing the need for explicit jumps.

const std = @import("std");
const Block = @import("../block.zig").Block;
const BlockKind = @import("../block.zig").BlockKind;
const Func = @import("../func.zig").Func;
const foundation = @import("foundation");
const debug = foundation.debug;

pub fn layout(f: *Func) !void {
    debug.log(.codegen, "=== Layout pass for '{s}' ({d} blocks, entry=b{d}) ===", .{
        f.name,
        f.blocks.items.len,
        if (f.entry) |e| e.id else 0,
    });

    if (f.blocks.items.len <= 1) {
        f.laidout = true;
        return;
    }

    const allocator = f.allocator;
    const num_blocks = f.blocks.items.len;

    var id_to_idx = std.AutoHashMapUnmanaged(u32, usize){};
    defer id_to_idx.deinit(allocator);
    for (f.blocks.items, 0..) |b, i| {
        try id_to_idx.put(allocator, b.id, i);
    }

    var scheduled = try allocator.alloc(bool, num_blocks);
    defer allocator.free(scheduled);
    @memset(scheduled, false);

    var indegree = try allocator.alloc(isize, num_blocks);
    defer allocator.free(indegree);
    @memset(indegree, 0);

    var order = std.ArrayListUnmanaged(*Block){};
    defer order.deinit(allocator);

    var zerodegree = std.ArrayListUnmanaged(usize){};
    defer zerodegree.deinit(allocator);

    var succs = std.ArrayListUnmanaged(usize){};
    defer succs.deinit(allocator);

    var is_exit = try allocator.alloc(bool, num_blocks);
    defer allocator.free(is_exit);
    @memset(is_exit, false);
    for (f.blocks.items, 0..) |b, i| {
        if (b.kind == .ret or b.kind == .exit) {
            is_exit[i] = true;
        }
    }

    for (f.blocks.items, 0..) |b, i| {
        if (is_exit[i]) continue;
        indegree[i] = @intCast(b.preds.len);
        if (b.preds.len == 0) {
            try zerodegree.append(allocator, i);
        }
    }

    var entry_idx: usize = 0;
    if (f.entry) |entry| {
        if (id_to_idx.get(entry.id)) |idx| {
            entry_idx = idx;
        }
    }

    var current_idx = entry_idx;

    while (order.items.len < num_blocks) {
        const b = f.blocks.items[current_idx];
        try order.append(allocator, b);
        scheduled[current_idx] = true;

        var i: usize = b.succs.len;
        while (i > 0) {
            i -= 1;
            const succ = b.succs[i].b;
            const succ_idx = id_to_idx.get(succ.id) orelse continue;

            indegree[succ_idx] -= 1;

            if (indegree[succ_idx] == 0) {
                try zerodegree.append(allocator, succ_idx);
            } else {
                try succs.append(allocator, succ_idx);
            }
        }

        if (b.likely == .likely and b.succs.len > 0) {
            const likely_idx = id_to_idx.get(b.succs[0].b.id) orelse 0;
            if (!scheduled[likely_idx]) {
                current_idx = likely_idx;
                continue;
            }
        } else if (b.likely == .unlikely and b.succs.len > 1) {
            const unlikely_idx = id_to_idx.get(b.succs[1].b.id) orelse 0;
            if (!scheduled[unlikely_idx]) {
                current_idx = unlikely_idx;
                continue;
            }
        }

        var found = false;
        for (b.succs) |edge| {
            const succ_idx = id_to_idx.get(edge.b.id) orelse continue;
            if (!scheduled[succ_idx] and !is_exit[succ_idx]) {
                current_idx = succ_idx;
                found = true;
                break;
            }
        }
        if (found) continue;

        found = false;
        while (zerodegree.items.len > 0) {
            const idx = zerodegree.pop() orelse break;
            if (!scheduled[idx]) {
                current_idx = idx;
                found = true;
                break;
            }
        }
        if (found) continue;

        while (succs.items.len > 0) {
            const idx = succs.pop() orelse break;
            if (!scheduled[idx]) {
                current_idx = idx;
                found = true;
                break;
            }
        }
        if (found) continue;

        for (0..num_blocks) |idx| {
            if (!scheduled[idx]) {
                current_idx = idx;
                found = true;
                break;
            }
        }
        if (!found) break;
    }

    f.blocks.clearRetainingCapacity();
    for (order.items) |b| {
        try f.blocks.append(allocator, b);
    }

    f.laidout = true;
    debug.log(.codegen, "  laid out {d} blocks, first=b{d}", .{
        order.items.len,
        if (order.items.len > 0) order.items[0].id else 0,
    });
}

const testing = std.testing;

test "layout single block" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "single");
    defer f.deinit();

    _ = try f.newBlock(.ret);

    try layout(&f);
    try testing.expectEqual(@as(usize, 1), f.blocks.items.len);
    try testing.expect(f.laidout);
}

test "layout linear blocks" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "linear");
    defer f.deinit();

    const b1 = try f.newBlock(.plain);
    const b2 = try f.newBlock(.plain);
    const b3 = try f.newBlock(.ret);

    try b1.addEdgeTo(allocator, b2);
    try b2.addEdgeTo(allocator, b3);

    try layout(&f);

    try testing.expectEqual(@as(usize, 3), f.blocks.items.len);
    try testing.expectEqual(b1.id, f.blocks.items[0].id);
    try testing.expectEqual(b2.id, f.blocks.items[1].id);
    try testing.expectEqual(b3.id, f.blocks.items[2].id);
}

test "layout diamond" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "diamond");
    defer f.deinit();

    const b1 = try f.newBlock(.if_);
    const b2 = try f.newBlock(.plain);
    const b3 = try f.newBlock(.plain);
    const b4 = try f.newBlock(.ret);

    try b1.addEdgeTo(allocator, b2);
    try b1.addEdgeTo(allocator, b3);
    try b2.addEdgeTo(allocator, b4);
    try b3.addEdgeTo(allocator, b4);

    try layout(&f);

    try testing.expectEqual(@as(usize, 4), f.blocks.items.len);
    try testing.expectEqual(b1.id, f.blocks.items[0].id);
    try testing.expectEqual(b4.id, f.blocks.items[3].id);
}

test "layout with loop" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "loop");
    defer f.deinit();

    const b1 = try f.newBlock(.plain);
    const b2 = try f.newBlock(.if_);
    const b3 = try f.newBlock(.plain);
    const b4 = try f.newBlock(.ret);

    try b1.addEdgeTo(allocator, b2);
    try b2.addEdgeTo(allocator, b3);
    try b2.addEdgeTo(allocator, b4);
    try b3.addEdgeTo(allocator, b2);

    try layout(&f);

    try testing.expectEqual(@as(usize, 4), f.blocks.items.len);
    try testing.expectEqual(b1.id, f.blocks.items[0].id);
    try testing.expectEqual(b4.id, f.blocks.items[3].id);
}
