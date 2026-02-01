//! Generic Rewrite Pass - Transform generic ops before architecture lowering.
//!
//! Go reference: cmd/compile/internal/ssa/rewritegeneric.go
//!
//! This pass runs BEFORE decomposition and lowering. It transforms:
//! - const_string → string_make(ptr_const, len_const)
//!
//! The transformation creates StringMake nodes that can then be decomposed
//! by the rewritedec pass when accessed via StringPtr/StringLen.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Block = @import("../block.zig").Block;
const Func = @import("../func.zig").Func;
const Op = @import("../op.zig").Op;
const TypeIndex = @import("../value.zig").TypeIndex;
const TypeRegistry = @import("../../frontend/types.zig").TypeRegistry;
const debug = @import("../../pipeline_debug.zig");

/// String offset map type - maps string content to data section offset
pub const StringOffsetMap = std.StringHashMap(i32);

/// Run the generic rewrite pass.
/// Go reference: rewritegeneric.go main loop
pub fn rewrite(allocator: std.mem.Allocator, f: *Func, string_offsets: ?*const StringOffsetMap) !void {
    debug.log(.codegen, "rewritegeneric: processing '{s}'", .{f.name});

    var rewritten: usize = 0;

    for (f.blocks.items) |block| {
        // We need to iterate over values and potentially add new ones.
        // To avoid issues with modifying while iterating, collect values to rewrite first.
        var to_rewrite = std.ArrayListUnmanaged(*Value){};
        defer to_rewrite.deinit(allocator);

        for (block.values.items) |v| {
            if (v.op == .const_string) {
                try to_rewrite.append(allocator, v);
            }
        }

        // Now rewrite each collected value
        for (to_rewrite.items) |v| {
            if (try rewriteConstString(allocator, f, block, v, string_offsets)) {
                rewritten += 1;
            }
        }
    }

    debug.log(.codegen, "  rewritten {d} values", .{rewritten});
}

/// Rewrite const_string to string_make(ptr_const, len_const).
/// Go reference: rewriteValuegeneric_OpConstString (lines 6424-6493)
///
/// Go transforms:
///   (ConstString {str}) => (StringMake (Addr {fe.StringData(str)} (SB)) (Const64 [len(str)]))
///
/// For Cot/Wasm, we transform:
///   (const_string aux_int=idx) => (string_make (const_64 [offset]) (const_64 [len]))
///
/// Where offset comes from string_offsets map (data section location).
fn rewriteConstString(
    allocator: std.mem.Allocator,
    f: *Func,
    block: *Block,
    v: *Value,
    string_offsets: ?*const StringOffsetMap,
) !bool {
    // Get string index from aux_int
    const string_idx: usize = @intCast(v.aux_int);

    // Get the actual string content
    if (string_idx >= f.string_literals.len) {
        debug.log(.codegen, "  v{d}: const_string idx {d} out of bounds", .{ v.id, string_idx });
        return false;
    }
    const str = f.string_literals[string_idx];
    const str_len: i64 = @intCast(str.len);

    // Get data section offset for this string
    const offset: i64 = if (string_offsets) |offsets| blk: {
        if (offsets.get(str)) |off| {
            break :blk @intCast(off);
        }
        debug.log(.codegen, "  v{d}: string not in data section: \"{s}\"", .{ v.id, str });
        break :blk 0;
    } else 0;

    debug.log(.codegen, "  v{d}: const_string -> string_make(ptr={d}, len={d})", .{ v.id, offset, str_len });

    // Create ptr constant: const_64 with data section offset
    // Go: v0 := b.NewValue0(v.Pos, OpAddr, typ.BytePtr)
    const ptr_val = try f.newValue(.const_64, TypeRegistry.I64, block, v.pos);
    ptr_val.aux_int = offset;
    try block.addValue(allocator, ptr_val);

    // Create len constant: const_64 with string length
    // Go: v2 := b.NewValue0(v.Pos, OpConst64, typ.Int)
    const len_val = try f.newValue(.const_64, TypeRegistry.I64, block, v.pos);
    len_val.aux_int = str_len;
    try block.addValue(allocator, len_val);

    // Transform const_string to string_make
    // Go: v.reset(OpStringMake)
    v.op = .string_make;
    v.aux_int = 0; // Clear the string index
    v.resetArgs();
    v.addArg2(ptr_val, len_val);

    return true;
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

test "rewriteConstString basic" {
    const allocator = testing.allocator;

    var f = Func.init(allocator, "test_string");
    defer f.deinit();

    const block = try f.newBlock(.first);

    // Set up string literals manually
    const literals = try allocator.alloc([]const u8, 1);
    defer allocator.free(literals);
    literals[0] = "hello";
    f.string_literals = literals;

    // Create const_string value (string index 0)
    const v = try f.newValue(.const_string, TypeRegistry.STRING, block, .{});
    v.aux_int = 0;
    try block.addValue(allocator, v);

    // Create string offsets map
    var string_offsets = StringOffsetMap.init(allocator);
    defer string_offsets.deinit();
    try string_offsets.put("hello", 100);

    // Run rewrite
    try rewrite(allocator, &f, &string_offsets);

    // Verify transformation
    try testing.expectEqual(Op.string_make, v.op);
    try testing.expectEqual(@as(usize, 2), v.args.len);
    try testing.expectEqual(Op.const_64, v.args[0].op);
    try testing.expectEqual(@as(i64, 100), v.args[0].aux_int); // offset
    try testing.expectEqual(Op.const_64, v.args[1].op);
    try testing.expectEqual(@as(i64, 5), v.args[1].aux_int); // len("hello")
}
