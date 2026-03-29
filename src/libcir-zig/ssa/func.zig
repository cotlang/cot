//! SSA Function representation.
//!
//! A Func owns all Blocks and Values for a single function. It provides
//! ID allocation, value/block recycling via free lists, constant caching,
//! register allocation storage, and CFG analysis (postorder, dominators).

const std = @import("std");
const foundation = @import("foundation");
const TypeIndex = foundation.types.TypeIndex;
const Value = @import("value.zig").Value;
const Block = @import("block.zig").Block;
const BlockKind = @import("block.zig").BlockKind;
const Op = @import("op.zig").Op;
const ID = @import("value.zig").ID;
const Pos = @import("value.zig").Pos;
const Location = @import("value.zig").Location;

/// Sequential ID allocator for Values and Blocks.
pub const IDAllocator = struct {
    next_id: ID = 1,
    pub fn next(self: *IDAllocator) ID {
        const id = self.next_id;
        self.next_id += 1;
        return id;
    }
};

/// SSA Function — owns all blocks, values, and associated metadata.
pub const Func = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    type_idx: TypeIndex = @enumFromInt(0),
    blocks: std.ArrayListUnmanaged(*Block),
    entry: ?*Block = null,
    bid: IDAllocator = .{},
    vid: IDAllocator = .{},
    reg_alloc: []?Location = &.{},
    constants: std.AutoHashMapUnmanaged(i64, std.ArrayListUnmanaged(*Value)),
    free_values: ?*Value = null,
    free_blocks: ?*Block = null,
    cached_postorder: ?[]*Block = null,
    cached_idom: ?[]*Block = null,
    scheduled: bool = false,
    laidout: bool = false,
    is_export: bool = false,
    local_sizes: []u32 = &.{},
    local_offsets: []i32 = &.{},
    local_overlap_groups: []u16 = &.{},
    local_overlap_arms: []u16 = &.{},
    local_slot_offsets: []u32 = &.{},
    string_literals: []const []const u8 = &.{},

    pub fn init(allocator: std.mem.Allocator, name: []const u8) Func {
        return .{ .allocator = allocator, .name = name, .blocks = .{}, .constants = .{} };
    }

    pub fn initDefault(name: []const u8) Func { return init(std.heap.page_allocator, name); }

    pub fn deinit(self: *Func) void {
        for (self.blocks.items) |b| {
            for (b.values.items) |v| self.allocator.destroy(v);
            b.deinit(self.allocator);
            self.allocator.destroy(b);
        }
        self.blocks.deinit(self.allocator);

        var v = self.free_values;
        while (v) |value| { const n = value.next_free; self.allocator.destroy(value); v = n; }
        var b = self.free_blocks;
        while (b) |block| { const n = block.next_free; self.allocator.destroy(block); b = n; }

        if (self.reg_alloc.len > 0) self.allocator.free(self.reg_alloc);
        if (self.local_sizes.len > 0) self.allocator.free(self.local_sizes);
        if (self.local_offsets.len > 0) self.allocator.free(self.local_offsets);
        if (self.local_overlap_groups.len > 0) self.allocator.free(self.local_overlap_groups);
        if (self.local_overlap_arms.len > 0) self.allocator.free(self.local_overlap_arms);
        if (self.local_slot_offsets.len > 0) self.allocator.free(self.local_slot_offsets);

        var const_it = self.constants.valueIterator();
        while (const_it.next()) |list| list.deinit(self.allocator);
        self.constants.deinit(self.allocator);

        if (self.cached_postorder) |po| self.allocator.free(po);
        if (self.cached_idom) |idom| self.allocator.free(idom);
    }

    pub fn getHome(self: *const Func, vid: ID) ?Location {
        if (vid >= self.reg_alloc.len) return null;
        return self.reg_alloc[vid];
    }

    pub fn setHome(self: *Func, v: *const Value, loc: Location) !void {
        if (v.id >= self.reg_alloc.len) {
            const new_len = v.id + 1;
            if (self.reg_alloc.len == 0) {
                self.reg_alloc = try self.allocator.alloc(?Location, new_len);
                for (self.reg_alloc) |*slot| slot.* = null;
            } else {
                const old_len = self.reg_alloc.len;
                self.reg_alloc = try self.allocator.realloc(self.reg_alloc, new_len);
                for (old_len..new_len) |i| self.reg_alloc[i] = null;
            }
        }
        self.reg_alloc[v.id] = loc;
    }

    pub fn setReg(self: *Func, v: *const Value, reg: u8) !void { try self.setHome(v, .{ .register = reg }); }
    pub fn setStack(self: *Func, v: *const Value, offset: i32) !void { try self.setHome(v, .{ .stack = offset }); }
    pub fn clearHome(self: *Func, vid: ID) void { if (vid < self.reg_alloc.len) self.reg_alloc[vid] = null; }

    pub fn newValue(self: *Func, op: Op, type_idx: TypeIndex, block: ?*Block, pos: Pos) !*Value {
        var val: *Value = undefined;
        if (self.free_values) |fv| {
            val = fv;
            self.free_values = fv.next_free;
            val.* = Value.init(self.vid.next(), op, type_idx, block, pos);
        } else {
            val = try self.allocator.create(Value);
            val.* = Value.init(self.vid.next(), op, type_idx, block, pos);
        }
        return val;
    }

    pub fn constInt(self: *Func, type_idx: TypeIndex, value: i64) !*Value {
        if (self.constants.get(value)) |list| {
            for (list.items) |v| if (v.type_idx == type_idx) return v;
        }
        const v = try self.newValue(.const_int, type_idx, self.entry, .{});
        v.aux_int = value;
        v.in_cache = true;
        const result = try self.constants.getOrPut(self.allocator, value);
        if (!result.found_existing) result.value_ptr.* = .{};
        try result.value_ptr.append(self.allocator, v);
        return v;
    }

    pub fn freeValue(self: *Func, v: *Value) void {
        if (v.in_cache) {
            if (self.constants.getPtr(v.aux_int)) |list| {
                for (list.items, 0..) |cv, i| if (cv == v) { _ = list.swapRemove(i); break; };
            }
        }
        v.resetArgs();
        v.next_free = self.free_values;
        self.free_values = v;
    }

    pub fn newBlock(self: *Func, kind: BlockKind) !*Block {
        var b: *Block = undefined;
        if (self.free_blocks) |fb| {
            b = fb;
            self.free_blocks = fb.next_free;
            b.* = Block.init(self.bid.next(), kind, self);
        } else {
            b = try self.allocator.create(Block);
            b.* = Block.init(self.bid.next(), kind, self);
        }
        try self.blocks.append(self.allocator, b);
        if (self.entry == null) self.entry = b;
        return b;
    }

    pub fn freeBlock(self: *Func, b: *Block) void {
        for (self.blocks.items, 0..) |block, i| {
            if (block == b) { _ = self.blocks.swapRemove(i); break; }
        }
        b.deinit(self.allocator);
        b.next_free = self.free_blocks;
        self.free_blocks = b;
        self.invalidateCFG();
    }

    pub fn invalidateCFG(self: *Func) void {
        if (self.cached_postorder) |po| { self.allocator.free(po); self.cached_postorder = null; }
        if (self.cached_idom) |idom| { self.allocator.free(idom); self.cached_idom = null; }
    }

    pub fn postorder(self: *Func) ![]*Block {
        if (self.cached_postorder) |po| return po;
        var result = std.ArrayListUnmanaged(*Block){};
        var visited = std.AutoHashMapUnmanaged(*Block, void){};
        defer visited.deinit(self.allocator);
        try self.postorderDFS(self.entry.?, &visited, &result);
        self.cached_postorder = try result.toOwnedSlice(self.allocator);
        return self.cached_postorder.?;
    }

    fn postorderDFS(self: *Func, b: *Block, visited: *std.AutoHashMapUnmanaged(*Block, void), result: *std.ArrayListUnmanaged(*Block)) !void {
        if (visited.contains(b)) return;
        try visited.put(self.allocator, b, {});
        for (b.succs) |edge| try self.postorderDFS(edge.b, visited, result);
        try result.append(self.allocator, b);
    }

    pub fn numBlocks(self: *const Func) usize { return self.bid.next_id; }
    pub fn numValues(self: *const Func) ID { return self.vid.next_id; }

    pub fn dump(self: *const Func, writer: anytype) !void {
        try writer.print("func {s}:\n", .{self.name});
        for (self.blocks.items) |b| {
            try writer.print("  {}\n", .{b});
            for (b.values.items) |v| try writer.print("    {}\n", .{v});
        }
    }
};

test "Func creation" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();
    try std.testing.expectEqualStrings("test", f.name);
}

test "Func block allocation" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();
    const b1 = try f.newBlock(.plain);
    const b2 = try f.newBlock(.ret);
    try std.testing.expectEqual(@as(usize, 3), f.numBlocks());
    try std.testing.expectEqual(@as(usize, 2), f.blocks.items.len);
    try std.testing.expectEqual(f.entry, b1);
    try std.testing.expectEqual(@as(ID, 1), b1.id);
    try std.testing.expectEqual(@as(ID, 2), b2.id);
}

test "Func value allocation" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();
    const b = try f.newBlock(.plain);
    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v1);
    const v2 = try f.newValue(.add, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v2);
    try std.testing.expectEqual(@as(ID, 1), v1.id);
    try std.testing.expectEqual(@as(ID, 2), v2.id);
}

test "Func constant caching" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();
    const b = try f.newBlock(.plain);
    const c1 = try f.constInt(@enumFromInt(0), 42);
    try b.addValue(allocator, c1);
    const c2 = try f.constInt(@enumFromInt(0), 42);
    const c3 = try f.constInt(@enumFromInt(0), 100);
    try b.addValue(allocator, c3);
    try std.testing.expectEqual(c1, c2);
    try std.testing.expect(c1 != c3);
}

test "Func value recycling" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();
    const b = try f.newBlock(.plain);
    const v1 = try f.newValue(.const_int, @enumFromInt(0), b, .{});
    const v1_ptr = v1;
    f.freeValue(v1);
    const v2 = try f.newValue(.add, @enumFromInt(0), b, .{});
    try b.addValue(allocator, v2);
    try std.testing.expectEqual(v1_ptr, v2);
}
