//! Block Layout Pass - Orders blocks to minimize control flow instructions.
//!
//! Go reference: cmd/compile/internal/ssa/layout.go
//!
//! After this pass, the order of func.blocks is the order to emit blocks.
//! The goal is to place successor blocks immediately after their predecessors
//! when possible, minimizing the need for explicit jumps.

const std = @import("std");
const Block = @import("../block.zig").Block;
const BlockKind = @import("../block.zig").BlockKind;
const Func = @import("../func.zig").Func;
const debug = @import("../../pipeline_debug.zig");

/// Reorder blocks to minimize jumps.
pub fn layout(f: *Func) !void {
    debug.log(.codegen, "layout: processing '{s}', entry=b{d}", .{
        f.name,
        if (f.entry) |e| e.id else 0,
    });

    if (f.blocks.items.len <= 1) {
        f.laidout = true;
        return;
    }

    const allocator = f.allocator;
    const num_blocks = f.blocks.items.len;

    // Build block ID to index mapping
    var id_to_idx = std.AutoHashMapUnmanaged(u32, usize){};
    defer id_to_idx.deinit(allocator);
    for (f.blocks.items, 0..) |b, i| {
        try id_to_idx.put(allocator, b.id, i);
    }

    // Track scheduled blocks
    var scheduled = try allocator.alloc(bool, num_blocks);
    defer allocator.free(scheduled);
    @memset(scheduled, false);

    // Track indegree (number of unscheduled predecessors)
    // Use isize to allow negative values (Go pattern for detecting exit blocks)
    var indegree = try allocator.alloc(isize, num_blocks);
    defer allocator.free(indegree);
    @memset(indegree, 0); // Initialize to 0 (Go pattern: exit blocks stay at 0)

    // Result order
    var order = std.ArrayListUnmanaged(*Block){};
    defer order.deinit(allocator);

    // Zero-degree blocks (ready to schedule)
    var zerodegree = std.ArrayListUnmanaged(usize){};
    defer zerodegree.deinit(allocator);

    // Successor blocks of recently scheduled blocks
    var succs = std.ArrayListUnmanaged(usize){};
    defer succs.deinit(allocator);

    // Find exit blocks
    var is_exit = try allocator.alloc(bool, num_blocks);
    defer allocator.free(is_exit);
    @memset(is_exit, false);
    for (f.blocks.items, 0..) |b, i| {
        if (b.kind == .ret or b.kind == .exit) {
            is_exit[i] = true;
        }
    }

    // Initialize indegree and zerodegree for NON-exit blocks only (Go pattern)
    // Exit blocks keep indegree=0, so when decremented they go negative,
    // which signals they should be scheduled last.
    for (f.blocks.items, 0..) |b, i| {
        if (is_exit[i]) continue; // Exit blocks stay at indegree=0
        indegree[i] = @intCast(b.preds.len);
        if (b.preds.len == 0) {
            try zerodegree.append(allocator, i);
        }
    }

    // Start with entry block
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

        // Update successors' indegree (Go pattern from layout.go:111-120)
        // Traverse in reverse order for better scheduling
        // Key: ALWAYS decrement indegree, even for exit blocks (allows negative)
        // Exit blocks start at 0, go to -1 when decremented, get added to succs
        var i: usize = b.succs.len;
        while (i > 0) {
            i -= 1;
            const succ = b.succs[i].b;
            const succ_idx = id_to_idx.get(succ.id) orelse continue;

            // Go: indegree[c.ID]-- (unconditional, can go negative)
            indegree[succ_idx] -= 1;

            if (indegree[succ_idx] == 0) {
                // Go: add to zerodegree (exit blocks never reach 0, they go -1)
                try zerodegree.append(allocator, succ_idx);
            } else {
                // Go: add to succs if indegree != 0 (includes negative for exit blocks)
                try succs.append(allocator, succ_idx);
            }
        }

        // Pick next block
        // 1. Try likely successor (if branch prediction available)
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

        // 2. Try first unscheduled successor
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

        // 3. Try zero-degree blocks (LIFO for depth-first)
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

        // 4. Try recently seen successors (Go pattern: no is_exit check)
        // Exit blocks get scheduled here after zerodegree is empty.
        // This is correct because loop bodies reach indegree=0 (go to zerodegree)
        // while exit blocks go to indegree=-1 (stay in succs until now).
        while (succs.items.len > 0) {
            const idx = succs.pop() orelse break;
            if (!scheduled[idx]) {
                current_idx = idx;
                found = true;
                break;
            }
        }
        if (found) continue;

        // 5. Try any unscheduled block (fallback)
        for (0..num_blocks) |idx| {
            if (!scheduled[idx]) {
                current_idx = idx;
                found = true;
                break;
            }
        }
        if (!found) break;
    }

    // Update f.blocks with new order
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

// ============================================================================
// Tests
// ============================================================================

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

    // Should maintain order: b1 -> b2 -> b3
    try testing.expectEqual(@as(usize, 3), f.blocks.items.len);
    try testing.expectEqual(b1.id, f.blocks.items[0].id);
    try testing.expectEqual(b2.id, f.blocks.items[1].id);
    try testing.expectEqual(b3.id, f.blocks.items[2].id);
}

test "layout diamond" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "diamond");
    defer f.deinit();

    //     b1 (if)
    //    /  \
    //   b2  b3
    //    \  /
    //     b4 (ret)
    const b1 = try f.newBlock(.if_);
    const b2 = try f.newBlock(.plain);
    const b3 = try f.newBlock(.plain);
    const b4 = try f.newBlock(.ret);

    try b1.addEdgeTo(allocator, b2);
    try b1.addEdgeTo(allocator, b3);
    try b2.addEdgeTo(allocator, b4);
    try b3.addEdgeTo(allocator, b4);

    try layout(&f);

    // b1 should be first, b4 (exit) should be last
    try testing.expectEqual(@as(usize, 4), f.blocks.items.len);
    try testing.expectEqual(b1.id, f.blocks.items[0].id);
    try testing.expectEqual(b4.id, f.blocks.items[3].id);
}

test "layout with loop" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "loop");
    defer f.deinit();

    //   b1 (entry)
    //    |
    //   b2 (if) <--+
    //  / |         |
    // |  b3 -------+
    // |
    // b4 (ret)
    const b1 = try f.newBlock(.plain);
    const b2 = try f.newBlock(.if_);
    const b3 = try f.newBlock(.plain);
    const b4 = try f.newBlock(.ret);

    try b1.addEdgeTo(allocator, b2);
    try b2.addEdgeTo(allocator, b3);
    try b2.addEdgeTo(allocator, b4);
    try b3.addEdgeTo(allocator, b2); // back edge

    try layout(&f);

    // Entry should be first, exit should be last
    try testing.expectEqual(@as(usize, 4), f.blocks.items.len);
    try testing.expectEqual(b1.id, f.blocks.items[0].id);
    try testing.expectEqual(b4.id, f.blocks.items[3].id);
}
