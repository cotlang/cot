//! Dead Code Elimination Pass - Remove unreachable blocks and dead values.
//!
//! Algorithm:
//! 1. ReachableBlocks: BFS from entry, mark reachable blocks
//! 2. liveValues:
//!    - Mark control values of reachable blocks as live
//!    - Mark values with side effects (calls, stores, nil checks) as live
//!    - Transitive closure: mark all args of live values as live
//!    - For phi args, skip args from unreachable predecessor blocks
//! 3. Remove dead values from blocks (freeValue)
//! 4. Remove unreachable blocks (freeBlock)

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const ID = @import("../value.zig").ID;
const Op = @import("../op.zig").Op;
const copyelim_pass = @import("copyelim.zig");
const foundation = @import("foundation");
const debug = foundation.debug;

pub fn deadcode(f: *Func) !void {
    debug.log(.deadcode, "=== Deadcode pass for '{s}' ({d} blocks, {d} values) ===", .{
        f.name,
        f.blocks.items.len,
        countValues(f),
    });

    const allocator = f.allocator;

    const reachable = try allocator.alloc(bool, f.numBlocks());
    defer allocator.free(reachable);
    @memset(reachable, false);

    try reachableBlocks(f, reachable);

    {
        var n_reachable: usize = 0;
        var n_unreachable: usize = 0;
        for (f.blocks.items) |b| {
            if (reachable[b.id]) {
                n_reachable += 1;
            } else {
                n_unreachable += 1;
                debug.log(.deadcode, "  block b{d} unreachable ({s})", .{ b.id, @tagName(b.kind) });
            }
        }
        if (n_unreachable > 0) {
            debug.log(.deadcode, "  reachable: {d}, unreachable: {d}", .{ n_reachable, n_unreachable });
        }
    }

    for (f.blocks.items) |b| {
        if (reachable[b.id]) continue;
        var i: usize = 0;
        while (i < b.succs.len) {
            const e = b.succs[i];
            if (reachable[e.b.id]) {
                removeEdge(b, i);
            } else {
                i += 1;
            }
        }
    }

    for (f.blocks.items) |b| {
        if (!reachable[b.id]) continue;
        if (b.kind != .first) continue;
        if (b.succs.len > 1) {
            removeEdge(b, 1);
        }
        b.kind = .plain;
        b.likely = .unknown;
    }

    try copyelim_pass.copyelim(f);

    const live = try allocator.alloc(bool, f.numValues());
    defer allocator.free(live);
    @memset(live, false);

    try liveValues(f, reachable, live);

    for (f.blocks.items) |b| {
        if (!reachable[b.id]) {
            b.resetControls();
        }
        for (b.values.items) |v| {
            if (!live[v.id]) {
                v.resetArgs();
            }
        }
    }

    for (f.blocks.items) |b| {
        var i: usize = 0;
        var write: usize = 0;
        while (i < b.values.items.len) : (i += 1) {
            const v = b.values.items[i];
            if (live[v.id]) {
                b.values.items[write] = v;
                write += 1;
            } else {
                f.freeValue(v);
            }
        }
        b.values.shrinkRetainingCapacity(write);
    }

    {
        var write: usize = 0;
        for (f.blocks.items) |b| {
            if (reachable[b.id]) {
                f.blocks.items[write] = b;
                write += 1;
            } else {
                b.deinit(allocator);
                b.next_free = f.free_blocks;
                f.free_blocks = b;
            }
        }
        f.blocks.shrinkRetainingCapacity(write);
        f.invalidateCFG();
    }

    debug.log(.deadcode, "=== Deadcode complete for '{s}': {d} blocks, {d} values remaining ===", .{
        f.name,
        f.blocks.items.len,
        countValues(f),
    });
}

pub fn removeUnreachableBlocksOnly(f: *Func) !void {
    const allocator = f.allocator;

    const reachable = try allocator.alloc(bool, f.numBlocks());
    defer allocator.free(reachable);
    @memset(reachable, false);

    try reachableBlocks(f, reachable);

    var n_unreachable: usize = 0;
    for (f.blocks.items) |b| {
        if (!reachable[b.id]) n_unreachable += 1;
    }
    if (n_unreachable == 0) return;

    for (f.blocks.items) |b| {
        if (reachable[b.id]) continue;
        var i: usize = 0;
        while (i < b.succs.len) {
            const e = b.succs[i];
            if (reachable[e.b.id]) {
                removeEdge(b, i);
            } else {
                i += 1;
            }
        }
    }

    var write: usize = 0;
    for (f.blocks.items) |b| {
        if (reachable[b.id]) {
            f.blocks.items[write] = b;
            write += 1;
        } else {
            b.deinit(allocator);
            b.next_free = f.free_blocks;
            f.free_blocks = b;
        }
    }
    f.blocks.shrinkRetainingCapacity(write);
    f.invalidateCFG();

    debug.log(.deadcode, "removeUnreachableBlocksOnly '{s}': removed {d} blocks", .{ f.name, n_unreachable });
}

fn reachableBlocks(f: *Func, reachable: []bool) !void {
    const allocator = f.allocator;
    const entry = f.entry orelse return;

    reachable[entry.id] = true;

    var worklist = std.ArrayListUnmanaged(*Block){};
    defer worklist.deinit(allocator);
    try worklist.append(allocator, entry);

    while (worklist.items.len > 0) {
        const b = worklist.pop().?;
        const succs = b.succs;
        for (succs) |e| {
            const c = e.b;
            if (!reachable[c.id]) {
                reachable[c.id] = true;
                try worklist.append(allocator, c);
            }
        }
    }
}

fn liveValues(f: *Func, reachable: []bool, live: []bool) !void {
    const allocator = f.allocator;

    var q = std.ArrayListUnmanaged(*Value){};
    defer q.deinit(allocator);

    for (f.blocks.items) |b| {
        if (!reachable[b.id]) continue;

        for (b.controlValues()) |v| {
            if (!live[v.id]) {
                live[v.id] = true;
                try q.append(allocator, v);
            }
        }

        for (b.values.items) |v| {
            const info = v.op.info();
            const is_root = info.call or info.has_side_effects or info.nil_check or v.uses > 0;
            if (is_root and !live[v.id]) {
                live[v.id] = true;
                try q.append(allocator, v);
            }
        }
    }

    while (q.items.len > 0) {
        const v = q.pop().?;
        for (v.args, 0..) |x, i| {
            if (v.op == .phi) {
                if (v.block) |vb| {
                    if (i < vb.preds.len and !reachable[vb.preds[i].b.id]) {
                        continue;
                    }
                }
            }
            if (!live[x.id]) {
                live[x.id] = true;
                try q.append(allocator, x);
            }
        }
    }
}

fn removeEdge(b: *Block, i: usize) void {
    const e = b.succs[i];
    const c = e.b;
    const j = e.i;

    removeSucc(b, i);
    removePred(c, j);

    for (c.values.items) |v| {
        if (v.op != .phi) continue;
        removePhiArg(c, v, j);
    }
}

fn removeSucc(b: *Block, i: usize) void {
    const n = b.succs.len - 1;
    if (i != n) {
        const e = b.succs[n];
        b.succs[i] = e;
        e.b.preds[e.i].i = i;
    }
    b.succs.len -= 1;
}

fn removePred(b: *Block, j: usize) void {
    const n = b.preds.len - 1;
    if (j != n) {
        const e = b.preds[n];
        b.preds[j] = e;
        e.b.succs[e.i].i = j;
    }
    b.preds.len -= 1;
}

fn removePhiArg(c: *Block, phi: *Value, i: usize) void {
    const n = c.preds.len;
    if (i < phi.args.len) {
        phi.args[i].uses -= 1;
        if (phi.args[i].uses < 0) {
            debug.log(.deadcode, "  USE WATCH: removePhiArg v{d} decremented v{d} to uses={d} (block b{d}, idx={d}, n={d})", .{ phi.id, phi.args[i].id, phi.args[i].uses, c.id, i, n });
        }
    }
    if (n < phi.args.len) {
        phi.args[i] = phi.args[n];
    }
    phi.args = phi.args[0..n];
}

fn countValues(f: *const Func) usize {
    var n: usize = 0;
    for (f.blocks.items) |b| {
        n += b.values.items.len;
    }
    return n;
}

const testing = std.testing;

test "reachableBlocks finds entry" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b1 = try f.newBlock(.plain);
    _ = try f.newBlock(.ret);

    const reachable = try allocator.alloc(bool, f.numBlocks());
    defer allocator.free(reachable);
    @memset(reachable, false);

    try reachableBlocks(&f, reachable);

    try testing.expect(reachable[b1.id]);
}

test "reachableBlocks follows edges" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b1 = try f.newBlock(.plain);
    const b2 = try f.newBlock(.plain);
    const b3 = try f.newBlock(.ret);
    try b1.addEdgeTo(allocator, b2);
    try b2.addEdgeTo(allocator, b3);

    const reachable = try allocator.alloc(bool, f.numBlocks());
    defer allocator.free(reachable);
    @memset(reachable, false);

    try reachableBlocks(&f, reachable);

    try testing.expect(reachable[b1.id]);
    try testing.expect(reachable[b2.id]);
    try testing.expect(reachable[b3.id]);
}

test "deadcode removes dead values" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v2.aux_int = 10;
    try b.addValue(allocator, v2);
    b.setControl(v2);

    try deadcode(&f);

    try testing.expectEqual(@as(usize, 1), b.values.items.len);
    try testing.expectEqual(v2, b.values.items[0]);
}

test "deadcode removes unreachable blocks" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b1 = try f.newBlock(.ret);
    _ = try f.newBlock(.ret);

    try deadcode(&f);

    try testing.expectEqual(@as(usize, 1), f.blocks.items.len);
    try testing.expectEqual(b1, f.blocks.items[0]);
}

test "deadcode keeps side-effecting values" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.store, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v2);

    try deadcode(&f);

    try testing.expectEqual(@as(usize, 1), b.values.items.len);
    try testing.expectEqual(v2, b.values.items[0]);
}

test "deadcode transitive liveness" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.add, @enumFromInt(0), b, .{});
    v2.addArg(v1);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    b.setControl(v2);

    const v3 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v3.aux_int = 99;
    try b.addValue(allocator, v3);

    try deadcode(&f);

    try testing.expectEqual(@as(usize, 2), b.values.items.len);
    var found_v1 = false;
    var found_v2 = false;
    for (b.values.items) |v| {
        if (v == v1) found_v1 = true;
        if (v == v2) found_v2 = true;
    }
    try testing.expect(found_v1);
    try testing.expect(found_v2);
}
