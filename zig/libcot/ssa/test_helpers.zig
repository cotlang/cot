//! Test helpers for SSA testing - fixtures and validation.

const std = @import("std");
const Func = @import("func.zig").Func;
const Block = @import("block.zig").Block;
const BlockKind = @import("block.zig").BlockKind;
const Value = @import("value.zig").Value;
const Op = @import("op.zig").Op;
const ID = @import("value.zig").ID;

pub const DiamondCFG = struct { entry: *Block, then_block: *Block, else_block: *Block, merge: *Block, condition: *Value, phi: ?*Value };
pub const LinearCFG = struct { blocks: []*Block, entry: *Block, exit: *Block };

pub const TestFuncBuilder = struct {
    func: *Func,
    allocator: std.mem.Allocator,
    owned: bool,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) !TestFuncBuilder {
        const f = try allocator.create(Func);
        f.* = Func.init(allocator, name);
        return .{ .func = f, .allocator = allocator, .owned = true };
    }

    pub fn deinit(self: *TestFuncBuilder) void {
        if (self.owned) {
            self.func.deinit();
            self.allocator.destroy(self.func);
        }
    }

    pub fn createDiamondCFG(self: *TestFuncBuilder) !DiamondCFG {
        const entry = try self.func.newBlock(.if_);
        const then_block = try self.func.newBlock(.plain);
        const else_block = try self.func.newBlock(.plain);
        const merge = try self.func.newBlock(.ret);

        const cond = try self.func.newValue(.const_bool, 0, entry, .{});
        cond.aux_int = 1;
        try entry.addValue(self.allocator, cond);
        entry.setControl(cond);

        try entry.addEdgeTo(self.allocator, then_block);
        try entry.addEdgeTo(self.allocator, else_block);
        try then_block.addEdgeTo(self.allocator, merge);
        try else_block.addEdgeTo(self.allocator, merge);

        return .{ .entry = entry, .then_block = then_block, .else_block = else_block, .merge = merge, .condition = cond, .phi = null };
    }

    pub fn createDiamondWithPhi(self: *TestFuncBuilder) !DiamondCFG {
        var diamond = try self.createDiamondCFG();

        const then_val = try self.func.newValue(.const_int, 0, diamond.then_block, .{});
        then_val.aux_int = 1;
        try diamond.then_block.addValue(self.allocator, then_val);

        const else_val = try self.func.newValue(.const_int, 0, diamond.else_block, .{});
        else_val.aux_int = 2;
        try diamond.else_block.addValue(self.allocator, else_val);

        const phi = try self.func.newValue(.phi, 0, diamond.merge, .{});
        phi.addArg2(then_val, else_val);
        try diamond.merge.addValue(self.allocator, phi);
        diamond.merge.setControl(phi);
        diamond.phi = phi;

        return diamond;
    }

    pub fn createLinearCFG(self: *TestFuncBuilder, count: usize) !LinearCFG {
        if (count == 0) return error.InvalidCount;
        var blocks = try self.allocator.alloc(*Block, count);
        for (0..count) |i| blocks[i] = try self.func.newBlock(if (i == count - 1) .ret else .plain);
        for (0..count - 1) |i| try blocks[i].addEdgeTo(self.allocator, blocks[i + 1]);
        return .{ .blocks = blocks, .entry = blocks[0], .exit = blocks[count - 1] };
    }

    pub fn addConst(self: *TestFuncBuilder, block: *Block, value: i64) !*Value {
        const v = try self.func.newValue(.const_int, 0, block, .{});
        v.aux_int = value;
        try block.addValue(self.allocator, v);
        return v;
    }
};

pub fn validateInvariants(f: *const Func, allocator: std.mem.Allocator) ![]VerifyError {
    var errs = std.ArrayListUnmanaged(VerifyError){};
    for (f.blocks.items) |b| {
        for (b.values.items) |v| if (v.block != b) try errs.append(allocator, .{ .message = "value block mismatch", .block_id = b.id, .value_id = v.id });
        for (b.succs) |succ| if (succ.i >= succ.b.preds.len or succ.b.preds[succ.i].b != b) try errs.append(allocator, .{ .message = "edge invariant", .block_id = b.id });
    }
    return errs.toOwnedSlice(allocator);
}

pub fn validateUseCounts(f: *const Func, allocator: std.mem.Allocator) ![]VerifyError {
    var errs = std.ArrayListUnmanaged(VerifyError){};
    var expected = std.AutoHashMapUnmanaged(ID, i32){};
    defer expected.deinit(allocator);

    for (f.blocks.items) |b| {
        for (b.values.items) |v| for (v.args) |arg| {
            const gop = try expected.getOrPut(allocator, arg.id);
            if (!gop.found_existing) gop.value_ptr.* = 0;
            gop.value_ptr.* += 1;
        };
        for (b.controlValues()) |cv| {
            const gop = try expected.getOrPut(allocator, cv.id);
            if (!gop.found_existing) gop.value_ptr.* = 0;
            gop.value_ptr.* += 1;
        }
    }
    for (f.blocks.items) |b| for (b.values.items) |v| if (v.uses != (expected.get(v.id) orelse 0)) try errs.append(allocator, .{ .message = "use count", .value_id = v.id, .block_id = b.id });
    return errs.toOwnedSlice(allocator);
}

pub const VerifyError = struct { message: []const u8, block_id: ID = 0, value_id: ID = 0 };
pub fn freeErrors(errs: []VerifyError, allocator: std.mem.Allocator) void { allocator.free(errs); }

// ============================================================================
// Tests
// ============================================================================

test "TestFuncBuilder diamond CFG" {
    const allocator = std.testing.allocator;
    var builder = try TestFuncBuilder.init(allocator, "test_diamond");
    defer builder.deinit();

    const diamond = try builder.createDiamondCFG();
    // numBlocks returns next_id (max ID + 1), not actual count - matches Go
    try std.testing.expectEqual(@as(usize, 5), builder.func.numBlocks());
    try std.testing.expectEqual(@as(usize, 4), builder.func.blocks.items.len); // actual count
    try std.testing.expectEqual(@as(usize, 2), diamond.entry.succs.len);
    try std.testing.expectEqual(@as(usize, 2), diamond.merge.preds.len);

    const errs = try validateInvariants(builder.func, allocator);
    defer freeErrors(errs, allocator);
    try std.testing.expectEqual(@as(usize, 0), errs.len);
}

test "TestFuncBuilder diamond with phi" {
    const allocator = std.testing.allocator;
    var builder = try TestFuncBuilder.init(allocator, "test_phi");
    defer builder.deinit();

    const diamond = try builder.createDiamondWithPhi();
    try std.testing.expect(diamond.phi != null);
    try std.testing.expectEqual(@as(usize, 2), diamond.phi.?.argsLen());

    const errs = try validateUseCounts(builder.func, allocator);
    defer freeErrors(errs, allocator);
    try std.testing.expectEqual(@as(usize, 0), errs.len);
}

test "TestFuncBuilder linear CFG" {
    const allocator = std.testing.allocator;
    var builder = try TestFuncBuilder.init(allocator, "test_linear");
    defer builder.deinit();

    const linear = try builder.createLinearCFG(5);
    defer builder.allocator.free(linear.blocks);

    try std.testing.expectEqual(@as(usize, 5), linear.blocks.len);
    try std.testing.expectEqual(linear.blocks[0], linear.entry);
    try std.testing.expectEqual(linear.blocks[4], linear.exit);
}
