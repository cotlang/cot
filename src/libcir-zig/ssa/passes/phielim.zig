//! Phi Elimination Pass - Convert redundant phis to copies.
//!
//! A phi is redundant if all its non-self arguments are the same value.
//! This pass converts such phis to copy operations.
//! Runs to a fixed point to catch transitive redundancies.

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const foundation = @import("foundation");
const debug = foundation.debug;

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

fn phielimValue(v: *Value) bool {
    if (v.op != .phi) return false;

    var w: ?*Value = null;
    for (v.args) |x| {
        if (x == v) continue;
        if (w) |wv| {
            if (x == wv) continue;
            return false;
        }
        w = x;
    }

    if (w == null) {
        return false;
    }

    v.op = .copy;
    for (v.args) |arg| arg.uses -= 1;
    v.args = v.args_storage[0..1];
    v.args_dynamic = false;
    v.args_capacity = 0;
    v.args[0] = w.?;
    w.?.uses += 1;

    return true;
}

const testing = std.testing;

test "phielim converts redundant phi" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.phi, @enumFromInt(0), b, .{});
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

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v1.aux_int = 1;
    try b.addValue(allocator, v1);
    const v2 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v2.aux_int = 2;
    try b.addValue(allocator, v2);

    const v3 = try f.newValue(.phi, @enumFromInt(0), b, .{});
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

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.phi, @enumFromInt(0), b, .{});
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

    const v1 = try f.newValue(.phi, @enumFromInt(0), b, .{});
    v1.addArg(v1);
    try b.addValue(allocator, v1);

    try phielim(&f);

    try testing.expectEqual(Op.phi, v1.op);
}

test "phielim fixed point" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.phi, @enumFromInt(0), b, .{});
    v2.addArg(v1);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    const v3 = try f.newValue(.phi, @enumFromInt(0), b, .{});
    v3.addArg(v2);
    v3.addArg(v1);
    try b.addValue(allocator, v3);

    try phielim(&f);

    try testing.expectEqual(Op.copy, v2.op);
    try testing.expectEqual(Op.phi, v3.op);
}
