//! Dead Code Elimination Pass - Remove unreachable blocks and dead values.
//!
//! Go reference: cmd/compile/internal/ssa/deadcode.go
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
const debug = @import("../../pipeline_debug.zig");

/// deadcode removes dead code from f.
/// Reference: Go deadcode.go — remove unreachable blocks and dead values.
pub fn deadcode(f: *Func) !void {
    // Go compile.go: f.Logf("  pass %s begin\n", p.name)
    debug.log(.deadcode, "=== Deadcode pass for '{s}' ({d} blocks, {d} values) ===", .{
        f.name,
        f.blocks.items.len,
        countValues(f),
    });

    const allocator = f.allocator;

    // 1. Find reachable blocks via BFS from entry.
    const reachable = try allocator.alloc(bool, f.numBlocks());
    defer allocator.free(reachable);
    @memset(reachable, false);

    try reachableBlocks(f, reachable);

    // Log reachability results (Go: f.Logf for debug > 0)
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

    // 2. Remove edges from dead blocks to live blocks.
    // This is important for phi argument consistency.
    for (f.blocks.items) |b| {
        if (reachable[b.id]) continue;
        // Remove edges from this dead block to live successors.
        var i: usize = 0;
        while (i < b.succs.len) {
            const e = b.succs[i];
            if (reachable[e.b.id]) {
                // Remove this edge: dead -> live.
                removeEdge(b, i);
            } else {
                i += 1;
            }
        }
    }

    // 3. Remove dead edges from live code (BlockFirst → BlockPlain).
    for (f.blocks.items) |b| {
        if (!reachable[b.id]) continue;
        if (b.kind != .first) continue;
        // BlockFirst only takes first successor. Remove second edge.
        if (b.succs.len > 1) {
            removeEdge(b, 1);
        }
        b.kind = .plain;
        b.likely = .unknown;
    }

    // 4. Splice out any copies introduced during dead block removal.
    try copyelim_pass.copyelim(f);

    // 5. Find live values.
    const live = try allocator.alloc(bool, f.numValues());
    defer allocator.free(live);
    @memset(live, false);

    try liveValues(f, reachable, live);

    // 6. Reset controls and args of dead values.
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

    // 7. Remove dead values from blocks' value lists.
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

    // 8. Remove unreachable blocks.
    {
        var write: usize = 0;
        for (f.blocks.items) |b| {
            if (reachable[b.id]) {
                f.blocks.items[write] = b;
                write += 1;
            } else {
                // All values should already be freed above.
                // Free the block itself.
                b.deinit(allocator);
                b.next_free = f.free_blocks;
                f.free_blocks = b;
            }
        }
        f.blocks.shrinkRetainingCapacity(write);
        f.invalidateCFG();
    }

    // Go LogStat: "deadcode REMOVED <count>"
    debug.log(.deadcode, "=== Deadcode complete for '{s}': {d} blocks, {d} values remaining ===", .{
        f.name,
        f.blocks.items.len,
        countValues(f),
    });
}

/// BFS from entry block to find all reachable blocks.
fn reachableBlocks(f: *Func, reachable: []bool) !void {
    const allocator = f.allocator;
    const entry = f.entry orelse return;

    reachable[entry.id] = true;

    var worklist = std.ArrayListUnmanaged(*Block){};
    defer worklist.deinit(allocator);
    try worklist.append(allocator, entry);

    while (worklist.items.len > 0) {
        // Pop from end (stack-like, matching Go).
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

/// Mark live values via transitive closure from roots.
/// Roots are: control values of reachable blocks, and values with side effects.
/// Conservative: any value with uses > 0 is also kept alive to avoid
/// removing values needed by other passes or codegen.
fn liveValues(f: *Func, reachable: []bool, live: []bool) !void {
    const allocator = f.allocator;

    // Worklist for transitive closure.
    var q = std.ArrayListUnmanaged(*Value){};
    defer q.deinit(allocator);

    // Seed: control values of reachable blocks + side-effecting values + used values.
    for (f.blocks.items) |b| {
        if (!reachable[b.id]) continue;

        // Control values are live.
        for (b.controlValues()) |v| {
            if (!live[v.id]) {
                live[v.id] = true;
                try q.append(allocator, v);
            }
        }

        // Values with side effects OR any uses are live.
        // Conservative: keep anything that has uses, since we don't
        // know if all op flags are correctly set yet.
        for (b.values.items) |v| {
            const info = v.op.info();
            const is_root = info.call or info.has_side_effects or info.nil_check or v.uses > 0;
            if (is_root and !live[v.id]) {
                live[v.id] = true;
                try q.append(allocator, v);
            }
        }
    }

    // Transitive closure: mark all args of live values as live.
    while (q.items.len > 0) {
        const v = q.pop().?;
        for (v.args, 0..) |x, i| {
            // For phi values, skip args from unreachable predecessor blocks.
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

/// Remove the i'th outgoing edge from b (and the corresponding incoming edge
/// from b.Succs[i].b). Also removes phi args from the successor.
/// Ported from Go's Block.removeEdge.
fn removeEdge(b: *Block, i: usize) void {
    const e = b.succs[i];
    const c = e.b;
    const j = e.i; // index into c.preds

    // Go order: remove succs first, then preds, then phi args.
    // Reference: block.go:311-327 removeEdge
    removeSucc(b, i);

    // Remove c.preds[j]
    removePred(c, j);

    // Remove phi args from c's phis at position j.
    // Must be AFTER removePred so len(preds) reflects the new count.
    // Reference: block.go:322-327
    for (c.values.items) |v| {
        if (v.op != .phi) continue;
        if (j < v.args.len) {
            removePhiArg(v, j);
        }
    }

    // Fix up cross-references after swap-remove.
    if (i < b.succs.len) {
        // The edge that was swapped into position i needs its back-reference updated.
        b.succs[i].b.preds[b.succs[i].i].i = i;
    }
    if (j < c.preds.len) {
        // The edge that was swapped into position j needs its back-reference updated.
        c.preds[j].b.succs[c.preds[j].i].i = j;
    }
}

fn removeSucc(b: *Block, i: usize) void {
    const last = b.succs.len - 1;
    if (i != last) {
        b.succs[i] = b.succs[last];
    }
    b.succs.len -= 1;
}

fn removePred(b: *Block, j: usize) void {
    const last = b.preds.len - 1;
    if (j != last) {
        b.preds[j] = b.preds[last];
    }
    b.preds.len -= 1;
}

fn removePhiArg(v: *Value, j: usize) void {
    // Decrement uses for the removed arg.
    if (j < v.args.len) {
        v.args[j].uses -= 1;
    }
    // Swap-remove from args.
    const last = v.args.len - 1;
    if (j != last) {
        v.args[j] = v.args[last];
    }
    v.args = v.args[0..last];
}

/// Count total values across all blocks.
fn countValues(f: *const Func) usize {
    var n: usize = 0;
    for (f.blocks.items) |b| {
        n += b.values.items.len;
    }
    return n;
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "reachableBlocks finds entry" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b1 = try f.newBlock(.plain);
    _ = try f.newBlock(.ret); // b2, unreachable (no edge to it)

    const reachable = try allocator.alloc(bool, f.numBlocks());
    defer allocator.free(reachable);
    @memset(reachable, false);

    try reachableBlocks(&f, reachable);

    try testing.expect(reachable[b1.id]);
    // b2 has no predecessor, but it was never connected
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

    // v1 = const_int 42 (dead — no uses, no side effects)
    const v1 = try f.newValue(.const_int, 0, b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    // v2 = const_int 10 (will be used as control)
    const v2 = try f.newValue(.const_int, 0, b, .{});
    v2.aux_int = 10;
    try b.addValue(allocator, v2);
    b.setControl(v2);

    try deadcode(&f);

    // v1 should be removed (dead), v2 should remain (control)
    try testing.expectEqual(@as(usize, 1), b.values.items.len);
    try testing.expectEqual(v2, b.values.items[0]);
}

test "deadcode removes unreachable blocks" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b1 = try f.newBlock(.ret);
    _ = try f.newBlock(.ret); // b2, unreachable

    try deadcode(&f);

    // Only b1 should remain
    try testing.expectEqual(@as(usize, 1), f.blocks.items.len);
    try testing.expectEqual(b1, f.blocks.items[0]);
}

test "deadcode keeps side-effecting values" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // v1 = const_int (dead)
    const v1 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v1);

    // v2 = store (side effect — kept alive)
    const v2 = try f.newValue(.store, 0, b, .{});
    try b.addValue(allocator, v2);

    try deadcode(&f);

    // v2 (store) should be kept, v1 should be removed
    try testing.expectEqual(@as(usize, 1), b.values.items.len);
    try testing.expectEqual(v2, b.values.items[0]);
}

test "deadcode transitive liveness" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // v1 = const_int
    const v1 = try f.newValue(.const_int, 0, b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    // v2 = add v1, v1 (used by control)
    const v2 = try f.newValue(.add, 0, b, .{});
    v2.addArg(v1);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    b.setControl(v2);

    // v3 = const_int (dead, not used by anything)
    const v3 = try f.newValue(.const_int, 0, b, .{});
    v3.aux_int = 99;
    try b.addValue(allocator, v3);

    try deadcode(&f);

    // v1 and v2 should be kept (v2 is control, v1 is arg of v2)
    // v3 should be removed
    try testing.expectEqual(@as(usize, 2), b.values.items.len);
    // Check that both v1 and v2 are present
    var found_v1 = false;
    var found_v2 = false;
    for (b.values.items) |v| {
        if (v == v1) found_v1 = true;
        if (v == v2) found_v2 = true;
    }
    try testing.expect(found_v1);
    try testing.expect(found_v2);
}
