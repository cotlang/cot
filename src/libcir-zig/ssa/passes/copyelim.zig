//! Copy Elimination Pass - Remove uses of OpCopy values.
//!
//! copyelim removes all uses of OpCopy values from f.
//! A subsequent deadcode pass is needed to actually remove the copies.

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const foundation = @import("foundation");
const debug = foundation.debug;

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

    try phielim(f);

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

fn copySource(v: *Value) *Value {
    var w = v.args[0];

    var slow = w;
    var advance = false;
    while (w.op == .copy) {
        w = w.args[0];
        if (w == slow) {
            w.op = .invalid;
            break;
        }
        if (advance) {
            slow = slow.args[0];
        }
        advance = !advance;
    }

    var cur = v;
    while (cur != w) {
        const x = cur.args[0];
        cur.setArg(0, w);
        cur = x;
    }
    return w;
}

fn copyelimValue(v: *Value) void {
    for (v.args, 0..) |a, i| {
        if (a.op == .copy) {
            v.setArg(i, copySource(a));
        }
    }
}

fn phielim(f: *Func) !void {
    while (true) {
        var change = false;
        for (f.blocks.items) |b| {
            for (b.values.items) |v| {
                copyelimValue(v);
                if (phielimValue(v)) change = true;
            }
        }
        if (!change) break;
    }
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

test "copyelimValue replaces copy args" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.copy, @enumFromInt(0), b, .{});
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    const v3 = try f.newValue(.add, @enumFromInt(0), b, .{});
    v3.addArg(v2);
    v3.addArg(v2);
    try b.addValue(allocator, v3);

    copyelimValue(v3);

    try testing.expectEqual(v1, v3.args[0]);
    try testing.expectEqual(v1, v3.args[1]);
}

test "copySource follows chain" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.plain);

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.copy, @enumFromInt(0), b, .{});
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    const v3 = try f.newValue(.copy, @enumFromInt(0), b, .{});
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

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.phi, @enumFromInt(0), b, .{});
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

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);
    const v2 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v2);

    const v3 = try f.newValue(.phi, @enumFromInt(0), b, .{});
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

    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);

    const v2 = try f.newValue(.phi, @enumFromInt(0), b, .{});
    v2.addArg(v1);
    v2.addArg(v2);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    const changed = phielimValue(v2);
    try testing.expect(changed);
    try testing.expectEqual(Op.copy, v2.op);
    try testing.expectEqual(v1, v2.args[0]);
}
