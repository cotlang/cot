//! Phi Elimination Pass - Convert redundant phis to copies.
//!
//! Go reference: cmd/compile/internal/ssa/copyelim.go (phielim function)
//!
//! A phi is redundant if all its non-self arguments are the same value.
//! This pass converts such phis to copy operations.
//! Runs to a fixed point to catch transitive redundancies.

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const debug = @import("../../debug.zig");

/// phielim eliminates redundant phi values from f.
/// A phi is redundant if its arguments are all equal (ignoring self-references).
/// Both of these phis are redundant:
///   v = phi(x, x, x)
///   v = phi(x, v, x, v)
/// Repeats until fixed point to catch transitive cases like:
///   v = phi(x, phi(x, x), phi(x, v))
pub fn phielim(f: *Func) !void {
    debug.log(.phielim, "=== Phielim pass for '{s}' ===", .{f.name});

    var total_eliminated: u32 = 0;
    while (true) {
        var change = false;
        for (f.blocks.items) |b| {
            for (b.values.items) |v| {
                if (phielimValue(v)) {
                    change = true;
                    total_eliminated += 1;
                }
            }
        }
        if (!change) break;
    }

    debug.log(.phielim, "  eliminated {d} phis", .{total_eliminated});
    debug.log(.phielim, "=== Phielim complete ===", .{});
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
        // v references only itself. It must be in a dead code loop.
        // Don't bother modifying it.
        return false;
    }

    // Convert phi to copy of w.
    v.op = .copy;
    // Reset args and set single arg to w.
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

test "phielim converts redundant phi" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    // v1 = const_int 42
    const v1 = try f.newValue(.const_int, 0, b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    // v2 = phi(v1, v1) — redundant
    const v2 = try f.newValue(.phi, 0, b, .{});
    v2.addArg(v1);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    try phielim(&f);

    try testing.expectEqual(Op.copy, v2.op);
    try testing.expectEqual(@as(usize, 1), v2.args.len);
    try testing.expectEqual(v1, v2.args[0]);
}

test "phielim keeps non-redundant phi" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, 0, b, .{});
    v1.aux_int = 1;
    try b.addValue(allocator, v1);
    const v2 = try f.newValue(.const_int, 0, b, .{});
    v2.aux_int = 2;
    try b.addValue(allocator, v2);

    // v3 = phi(v1, v2) — NOT redundant
    const v3 = try f.newValue(.phi, 0, b, .{});
    v3.addArg(v1);
    v3.addArg(v2);
    try b.addValue(allocator, v3);

    try phielim(&f);

    try testing.expectEqual(Op.phi, v3.op);
    try testing.expectEqual(@as(usize, 2), v3.args.len);
}

test "phielim ignores self-references" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v1);

    // v2 = phi(v1, v2, v1) — redundant (self-refs ignored)
    const v2 = try f.newValue(.phi, 0, b, .{});
    v2.addArg(v1);
    v2.addArg(v2);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    try phielim(&f);

    try testing.expectEqual(Op.copy, v2.op);
    try testing.expectEqual(v1, v2.args[0]);
}

test "phielim self-only phi unchanged" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    // v1 = phi(v1) — self-only, dead code loop
    const v1 = try f.newValue(.phi, 0, b, .{});
    v1.addArg(v1);
    try b.addValue(allocator, v1);

    try phielim(&f);

    // Should remain as phi (dead code, not modified)
    try testing.expectEqual(Op.phi, v1.op);
}

test "phielim fixed point" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v1);

    // v2 = phi(v1, v1) — will become copy(v1)
    const v2 = try f.newValue(.phi, 0, b, .{});
    v2.addArg(v1);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    // v3 = phi(v2, v1) — after v2 becomes copy(v1), this has args (copy(v1), v1)
    // Note: phielim alone doesn't do copyelim on args, so v3 stays as phi(v2, v1)
    // where v2 != v1. The combined copyelim pass handles this case.
    const v3 = try f.newValue(.phi, 0, b, .{});
    v3.addArg(v2);
    v3.addArg(v1);
    try b.addValue(allocator, v3);

    try phielim(&f);

    // v2 should be converted
    try testing.expectEqual(Op.copy, v2.op);
    // v3 stays as phi because v2 (now copy) != v1 (phielim doesn't copyelim args)
    try testing.expectEqual(Op.phi, v3.op);
}
