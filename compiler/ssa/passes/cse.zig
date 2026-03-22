//! Common Subexpression Elimination — Replace duplicate computations.
//!
//! Go reference: cmd/compile/internal/ssa/cse.go
//!
//! Simplified approach: hash-based value numbering within each block.
//! Two values are equivalent if they have the same op, type, aux_int, and args.

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const debug = @import("../../pipeline_debug.zig");

/// Run CSE on a function.
/// Reference: Go cse.go — hash-based value numbering, LogStat("CSE REWRITES", n).
pub fn cse(f: *Func) !void {
    var total_values: usize = 0;
    for (f.blocks.items) |b| total_values += b.values.items.len;
    debug.log(.codegen, "=== CSE pass for '{s}' ({d} blocks, {d} values) ===", .{
        f.name, f.blocks.items.len, total_values,
    });

    const allocator = f.allocator;
    var rewrites: usize = 0;

    // rewrite[v.id] = replacement value (or null)
    const rewrite = try allocator.alloc(?*Value, f.numValues());
    defer allocator.free(rewrite);
    @memset(rewrite, null);

    // Per-block hash-based CSE
    for (f.blocks.items) |b| {
        // Hash table: hash -> first value with that hash
        var seen = std.AutoHashMapUnmanaged(u64, *Value){};
        defer seen.deinit(allocator);

        for (b.values.items) |v| {
            if (!canCSE(v.op)) continue;

            const h = hashValue(v);
            if (seen.get(h)) |existing| {
                if (valuesEqual(v, existing)) {
                    rewrite[v.id] = existing;
                    rewrites += 1;
                }
            } else {
                try seen.put(allocator, h, v);
            }
        }
    }

    if (rewrites == 0) return;

    // Apply rewrites
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            for (v.args, 0..) |a, i| {
                if (rewrite[a.id]) |replacement| {
                    v.args[i] = replacement;
                }
            }
        }
        for (b.controls, 0..) |mc, i| {
            if (mc) |c| {
                if (rewrite[c.id]) |replacement| {
                    b.controls[i] = replacement;
                }
            }
        }
    }

    // Go LogStat: "CSE REWRITES" count
    debug.log(.codegen, "=== CSE complete for '{s}': {d} rewrites ===", .{ f.name, rewrites });
}

fn canCSE(op: Op) bool {
    const info = op.info();
    // Can't CSE side-effecting ops, calls, memory ops, or phis
    if (info.has_side_effects or info.call or info.nil_check) return false;
    if (info.writes_memory or info.reads_memory) return false;
    if (op == .phi or op == .arg or op == .copy or op == .fwd_ref) return false;
    if (op == .sp or op == .init_mem) return false;
    return true;
}

fn hashValue(v: *Value) u64 {
    var h: u64 = @intFromEnum(v.op);
    h = h *% 31 +% @as(u64, @intCast(v.type_idx));
    h = h *% 17 +% @as(u64, @bitCast(v.aux_int));
    for (v.args, 0..) |a, i| {
        h = h *% 13 +% @as(u64, @intCast(a.id)) *% (@as(u64, 7) +% @as(u64, @intCast(i)));
    }
    return h;
}

fn valuesEqual(a: *Value, b: *Value) bool {
    if (a.op != b.op) return false;
    if (a.type_idx != b.type_idx) return false;
    if (a.aux_int != b.aux_int) return false;
    if (a.args.len != b.args.len) return false;
    for (a.args, b.args) |aa, ba| {
        if (aa != ba) return false;
    }
    return true;
}

// Tests
const testing = std.testing;

test "cse basic" {
    const allocator = testing.allocator;
    var f = Func.init(allocator, "test_cse");
    defer f.deinit();

    const b = try f.newBlock(.ret);

    // v0 = const_int 42
    const v0 = try f.newValue(.const_int, 0, b, .{});
    v0.aux_int = 42;
    try b.addValue(allocator, v0);

    // v1 = const_int 42 (duplicate)
    const v1 = try f.newValue(.const_int, 0, b, .{});
    v1.aux_int = 42;
    try b.addValue(allocator, v1);

    // v2 = add(v0, v1)
    const v2 = try f.newValue(.add, 0, b, .{});
    v2.addArg(v0);
    v2.addArg(v1);
    try b.addValue(allocator, v2);

    try cse(&f);

    // v2's args should both point to v0 now
    try testing.expectEqual(v0, v2.args[0]);
    try testing.expectEqual(v0, v2.args[1]);
}
