//! Copy Elimination Pass - Remove uses of OpCopy values.
//!
//! Go reference: cmd/compile/internal/ssa/copyelim.go
//!
//! copyelim removes all uses of OpCopy values from f.
//! A subsequent deadcode pass is needed to actually remove the copies.

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const debug = @import("../../pipeline_debug.zig");

/// copyelim removes all uses of OpCopy values.
/// Combines copyelim and phielim into a single pass (matching Go).
pub fn copyelim(f: *Func) !void {
    var total_values: usize = 0;
    var total_copies: usize = 0;
    var total_phis: usize = 0;
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            total_values += 1;
            if (v.op == .copy) total_copies += 1;
            if (v.op == .phi) total_phis += 1;
        }
    }
    debug.log(.copyelim, "=== Copyelim pass for '{s}' ({d} values, {d} copies, {d} phis) ===", .{
        f.name, total_values, total_copies, total_phis,
    });

    // Run phielim first (converts redundant phis to copies), then
    // eliminate copy uses. Matches Go's copyelim() which calls phielim() first.
    try phielim(f);

    // Update block control values.
    var ctrl_rewrites: usize = 0;
    for (f.blocks.items) |b| {
        for (&b.controls) |*ctrl| {
            if (ctrl.*) |v| {
                if (v.op == .copy) {
                    const src = copySource(v);
                    v.uses -= 1;
                    src.uses += 1;
                    ctrl.* = src;
                    ctrl_rewrites += 1;
                }
            }
        }
    }

    debug.log(.copyelim, "=== Copyelim complete for '{s}': {d} control rewrites ===", .{
        f.name, ctrl_rewrites,
    });
}

/// copySource returns the (non-copy) op which is the ultimate source of v.
/// v must be a copy op.
/// Uses Floyd's tortoise-and-hare cycle detection to handle infinite copy
/// loops in unreachable code.
fn copySource(v: *Value) *Value {
    var w = v.args[0];

    // Floyd's cycle detection: slow pointer advances every other step.
    var slow = w;
    var advance = false;
    while (w.op == .copy) {
        w = w.args[0];
        if (w == slow) {
            // Cycle detected in unreachable code. Break it.
            w.op = .invalid;
            break;
        }
        if (advance) {
            slow = slow.args[0];
        }
        advance = !advance;
    }

    // Path compression: update all copies in the chain to point directly to w.
    // This prevents O(n^2) work for a chain of n copies.
    var cur = v;
    while (cur != w) {
        const x = cur.args[0];
        cur.setArg(0, w);
        cur = x;
    }
    return w;
}

/// copyelimValue ensures that no args of v are copies.
fn copyelimValue(v: *Value) void {
    for (v.args, 0..) |a, i| {
        if (a.op == .copy) {
            v.setArg(i, copySource(a));
        }
    }
}

/// phielim eliminates redundant phi values from f.
/// A phi is redundant if its arguments are all equal (ignoring self-references).
/// Both of these phis are redundant:
///   v = phi(x, x, x)
///   v = phi(x, v, x, v)
/// Repeats until fixed point to catch transitive cases like:
///   v = phi(x, phi(x, x), phi(x, v))
fn phielim(f: *Func) !void {
    while (true) {
        var change = false;
        for (f.blocks.items) |b| {
            for (b.values.items) |v| {
                // Eliminate copy args first (matching Go's combined pass).
                copyelimValue(v);
                if (phielimValue(v)) change = true;
            }
        }
        if (!change) break;
    }
}

/// phielimValue tries to convert the phi v to a copy.
/// Returns true if the phi was converted.
fn phielimValue(v: *Value) bool {
    if (v.op != .phi) return false;

    // If there are two distinct args of v which are not v itself,
    // then the phi must remain. Otherwise, we can replace it with a copy.
    var w: ?*Value = null;
    for (v.args) |x| {
        if (x == v) continue;
        if (w) |wv| {
            if (x == wv) continue;
            // Two distinct non-self args — phi is not redundant.
            return false;
        }
        w = x;
    }

    if (w == null) {
        // v references only itself. Dead code loop. Don't modify.
        return false;
    }

    // Convert phi to copy of w.
    v.op = .copy;
    // Reset args and set single arg to w.
    // Decrement uses for all old args, then set new single arg.
    for (v.args) |arg| arg.uses -= 1;
    v.args = v.args_storage[0..1];
    v.args_dynamic = false;
    v.args_capacity = 0;
    v.args[0] = w.?;
    w.?.uses += 1;

    return true;
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "copyelimValue replaces copy args" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    // v1 = const_int 42
    const v1 = try f.newValue(.const_int, 0, b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    // v2 = copy v1
    const v2 = try f.newValue(.copy, 0, b, .{});
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    // v3 = add v2, v2  (uses copies)
    const v3 = try f.newValue(.add, 0, b, .{});
    v3.addArg(v2);
    v3.addArg(v2);
    try b.addValue(allocator, v3);

    copyelimValue(v3);

    // v3's args should now point to v1 (the source), not v2 (the copy)
    try testing.expectEqual(v1, v3.args[0]);
    try testing.expectEqual(v1, v3.args[1]);
}

test "copySource follows chain" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    // v1 = const_int
    const v1 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v1);

    // v2 = copy v1
    const v2 = try f.newValue(.copy, 0, b, .{});
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    // v3 = copy v2  (chain: v3 -> v2 -> v1)
    const v3 = try f.newValue(.copy, 0, b, .{});
    v3.addArg(v2);
    try b.addValue(allocator, v3);

    const src = copySource(v3);
    try testing.expectEqual(v1, src);
}

test "phielimValue converts redundant phi" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    // v1 = const_int
    const v1 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v1);

    // v2 = phi(v1, v1) — redundant
    const v2 = try f.newValue(.phi, 0, b, .{});
    v2.addArg(v1);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    const changed = phielimValue(v2);
    try testing.expect(changed);
    try testing.expectEqual(Op.copy, v2.op);
    try testing.expectEqual(@as(usize, 1), v2.args.len);
    try testing.expectEqual(v1, v2.args[0]);
}

test "phielimValue keeps non-redundant phi" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v1);
    const v2 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v2);

    // v3 = phi(v1, v2) — NOT redundant (two distinct args)
    const v3 = try f.newValue(.phi, 0, b, .{});
    v3.addArg(v1);
    v3.addArg(v2);
    try b.addValue(allocator, v3);

    const changed = phielimValue(v3);
    try testing.expect(!changed);
    try testing.expectEqual(Op.phi, v3.op);
}

test "phielimValue ignores self-references" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v1);

    // v2 = phi(v1, v2, v1) — redundant (self-refs ignored, only v1 remains)
    const v2 = try f.newValue(.phi, 0, b, .{});
    v2.addArg(v1);
    v2.addArg(v2);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    const changed = phielimValue(v2);
    try testing.expect(changed);
    try testing.expectEqual(Op.copy, v2.op);
    try testing.expectEqual(v1, v2.args[0]);
}
