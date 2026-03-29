//! Generic Rewrite Pass - Transform generic ops before architecture lowering.
//!
//! This pass runs BEFORE decomposition and lowering. It transforms:
//! - const_string -> string_make(ptr_const, len_const)
//!
//! The transformation creates StringMake nodes that can then be decomposed
//! by the rewritedec pass when accessed via StringPtr/StringLen.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const TypeIndex = @import("../value.zig").TypeIndex;
const foundation = @import("foundation");
const TypeRegistry = foundation.types.TypeRegistry;
const debug = foundation.debug;

pub const StringOffsetMap = std.StringHashMap(i32);

pub fn rewrite(allocator: std.mem.Allocator, f: *Func, string_offsets: ?*const StringOffsetMap) !void {
    var total_values: usize = 0;
    var const_strings: usize = 0;
    for (f.blocks.items) |b| {
        for (b.values.items) |v| {
            total_values += 1;
            if (v.op == .const_string) const_strings += 1;
        }
    }
    debug.log(.codegen, "=== Rewritegeneric pass for '{s}' ({d} blocks, {d} values, {d} const_strings) ===", .{
        f.name, f.blocks.items.len, total_values, const_strings,
    });

    var rewritten: usize = 0;

    for (f.blocks.items) |block| {
        var to_rewrite = std.ArrayListUnmanaged(*Value){};
        defer to_rewrite.deinit(allocator);

        for (block.values.items) |v| {
            if (v.op == .const_string) {
                try to_rewrite.append(allocator, v);
            }
        }

        for (to_rewrite.items) |v| {
            if (try rewriteConstString(allocator, f, block, v, string_offsets)) {
                rewritten += 1;
            }
        }
    }

    debug.log(.codegen, "=== Rewritegeneric complete for '{s}': {d} const_strings rewritten ===", .{
        f.name, rewritten,
    });
}

fn rewriteConstString(
    allocator: std.mem.Allocator,
    f: *Func,
    block: *Block,
    v: *Value,
    string_offsets: ?*const StringOffsetMap,
) !bool {
    const string_idx: usize = @intCast(v.aux_int);

    if (string_idx >= f.string_literals.len) {
        debug.log(.codegen, "  v{d}: const_string idx {d} out of bounds", .{ v.id, string_idx });
        return false;
    }
    const str = f.string_literals[string_idx];
    const str_len: i64 = @intCast(str.len);

    const offset: i64 = if (string_offsets) |offsets| blk: {
        if (offsets.get(str)) |off| {
            break :blk @intCast(off);
        }
        debug.log(.codegen, "  v{d}: string not in data section: \"{s}\"", .{ v.id, str });
        break :blk 0;
    } else 0;

    debug.log(.codegen, "  v{d}: const_string -> string_make(ptr={d}, len={d})", .{ v.id, offset, str_len });

    debug.log(.codegen, "    v{d} BEFORE rewrite: op={s} args.len={d}", .{ v.id, @tagName(v.op), v.args.len });

    const ptr_val = try f.newValue(.const_64, TypeRegistry.I64, block, v.pos);
    ptr_val.aux_int = offset;
    try block.addValue(allocator, ptr_val);

    const len_val = try f.newValue(.const_64, TypeRegistry.I64, block, v.pos);
    len_val.aux_int = str_len;
    try block.addValue(allocator, len_val);

    v.op = .string_make;
    v.aux_int = 0;
    v.resetArgs();
    v.addArg2(ptr_val, len_val);

    debug.log(.codegen, "    v{d} AFTER rewrite: args=[v{d}, v{d}] ptr_uses={d} len_uses={d}", .{ v.id, ptr_val.id, len_val.id, ptr_val.uses, len_val.uses });

    return true;
}

const testing = std.testing;

test "rewriteConstString basic" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_string");
    defer f.deinit();

    const block = try f.newBlock(.first);

    const literals = try allocator.alloc([]const u8, 1);
    defer allocator.free(literals);
    literals[0] = "hello";
    f.string_literals = literals;

    const v = try f.newValue(.const_string, TypeRegistry.STRING, block, .{});
    v.aux_int = 0;
    try block.addValue(allocator, v);

    var string_offsets = StringOffsetMap.init(allocator);
    defer string_offsets.deinit();
    try string_offsets.put("hello", 100);

    try rewrite(allocator, &f, &string_offsets);

    try testing.expectEqual(Op.string_make, v.op);
    try testing.expectEqual(@as(usize, 2), v.args.len);
    try testing.expectEqual(Op.const_64, v.args[0].op);
    try testing.expectEqual(@as(i64, 100), v.args[0].aux_int);
    try testing.expectEqual(Op.const_64, v.args[1].op);
    try testing.expectEqual(@as(i64, 5), v.args[1].aux_int);
}
