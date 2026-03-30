//! SSA Basic Block representation.

const std = @import("std");
const value_mod = @import("value.zig");
const Value = value_mod.Value;
const ID = value_mod.ID;
const INVALID_ID = value_mod.INVALID_ID;
const Pos = value_mod.Pos;

pub const BlockKind = enum(u8) {
    invalid, plain, if_, ret, exit, defer_, first, jump_table,
    // ARM64-specific
    arm64_cbz, arm64_cbnz, arm64_tbz, arm64_tbnz,
    // x86_64-specific
    x86_64_eq, x86_64_ne, x86_64_lt, x86_64_le, x86_64_gt, x86_64_ge,
    x86_64_ult, x86_64_ule, x86_64_ugt, x86_64_uge,

    pub fn numSuccs(self: BlockKind) i8 {
        return switch (self) {
            .invalid => 0,
            .plain, .defer_, .first => 1,
            .if_, .arm64_cbz, .arm64_cbnz, .arm64_tbz, .arm64_tbnz => 2,
            .x86_64_eq, .x86_64_ne, .x86_64_lt, .x86_64_le, .x86_64_gt, .x86_64_ge => 2,
            .x86_64_ult, .x86_64_ule, .x86_64_ugt, .x86_64_uge => 2,
            .ret, .exit => 0,
            .jump_table => -1,
        };
    }

    pub fn numControls(self: BlockKind) u8 {
        return switch (self) {
            .invalid, .plain, .first, .exit => 0,
            .if_, .ret, .defer_, .jump_table => 1,
            .arm64_cbz, .arm64_cbnz, .arm64_tbz, .arm64_tbnz => 1,
            .x86_64_eq, .x86_64_ne, .x86_64_lt, .x86_64_le, .x86_64_gt, .x86_64_ge => 0,
            .x86_64_ult, .x86_64_ule, .x86_64_ugt, .x86_64_uge => 0,
        };
    }

    pub fn isConditional(self: BlockKind) bool {
        return switch (self) {
            .if_, .arm64_cbz, .arm64_cbnz, .arm64_tbz, .arm64_tbnz => true,
            .x86_64_eq, .x86_64_ne, .x86_64_lt, .x86_64_le, .x86_64_gt, .x86_64_ge => true,
            .x86_64_ult, .x86_64_ule, .x86_64_ugt, .x86_64_uge => true,
            else => false,
        };
    }
};

pub const Edge = struct {
    b: *Block,
    i: usize,

    pub fn format(self: Edge, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("b{d}", .{self.b.id});
    }
};

pub const BranchPrediction = enum(i8) { unlikely = -1, unknown = 0, likely = 1 };

pub const Block = struct {
    id: ID = INVALID_ID,
    kind: BlockKind = .invalid,
    succs: []Edge = &.{},
    preds: []Edge = &.{},
    controls: [2]?*Value = .{ null, null },
    values: std.ArrayListUnmanaged(*Value),
    func: *Func,
    pos: Pos = .{},
    likely: BranchPrediction = .unknown,
    flags_live_at_end: bool = false,
    succs_storage: [4]Edge = undefined,
    preds_storage: [4]Edge = undefined,
    next_free: ?*Block = null,

    pub fn init(id: ID, kind: BlockKind, func: *Func) Block {
        return .{ .id = id, .kind = kind, .func = func, .values = .{} };
    }

    pub fn deinit(self: *Block, allocator: std.mem.Allocator) void { self.values.deinit(allocator); }

    pub fn numControls(self: *const Block) usize {
        if (self.controls[1] != null) return 2;
        if (self.controls[0] != null) return 1;
        return 0;
    }

    pub fn controlValues(self: *const Block) []const *Value {
        const n = self.numControls();
        if (n == 0) return &.{};
        const ptr: [*]const *Value = @ptrCast(&self.controls);
        return ptr[0..n];
    }

    pub fn setControl(self: *Block, v: *Value) void {
        if (self.controls[0]) |old| old.uses -= 1;
        if (self.controls[1]) |old| old.uses -= 1;
        self.controls[0] = v;
        self.controls[1] = null;
        v.uses += 1;
    }

    pub fn addControl(self: *Block, v: *Value) void {
        const n = self.numControls();
        if (n >= 2) @panic("block already has 2 controls");
        self.controls[n] = v;
        v.uses += 1;
    }

    pub fn resetControls(self: *Block) void {
        if (self.controls[0]) |v| v.uses -= 1;
        if (self.controls[1]) |v| v.uses -= 1;
        self.controls = .{ null, null };
    }

    pub fn addEdgeTo(self: *Block, allocator: std.mem.Allocator, succ: *Block) !void {
        const succ_pred_idx = succ.preds.len;
        const self_succ_idx = self.succs.len;
        self.succs = try appendEdge(allocator, self.succs, &self.succs_storage, .{ .b = succ, .i = succ_pred_idx });
        succ.preds = try appendEdge(allocator, succ.preds, &succ.preds_storage, .{ .b = self, .i = self_succ_idx });
    }

    pub fn removeEdgeTo(self: *Block, succ: *Block) void {
        for (self.succs, 0..) |edge, i| {
            if (edge.b == succ) {
                const pred_idx = edge.i;
                _ = removeEdgeAt(&self.succs, i);
                _ = removeEdgeAt(&succ.preds, pred_idx);
                if (i < self.succs.len) self.succs[i].b.preds[self.succs[i].i].i = i;
                if (pred_idx < succ.preds.len) succ.preds[pred_idx].b.succs[succ.preds[pred_idx].i].i = pred_idx;
                return;
            }
        }
    }

    pub fn addValue(self: *Block, allocator: std.mem.Allocator, v: *Value) !void {
        v.block = self;
        try self.values.append(allocator, v);
    }

    pub fn insertValueBefore(self: *Block, allocator: std.mem.Allocator, new_val: *Value, before: *Value) !void {
        new_val.block = self;
        for (self.values.items, 0..) |v, i| {
            if (v == before) {
                try self.values.insert(allocator, i, new_val);
                return;
            }
        }
        try self.values.append(allocator, new_val);
    }

    pub fn format(self: *const Block, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("b{d} ({s})", .{ self.id, @tagName(self.kind) });
        if (self.succs.len > 0) {
            try writer.writeAll(" -> ");
            for (self.succs, 0..) |succ, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("b{d}", .{succ.b.id});
            }
        }
    }
};

pub const Func = @import("func.zig").Func;

fn appendEdge(allocator: std.mem.Allocator, slice: []Edge, storage: *[4]Edge, edge: Edge) ![]Edge {
    const new_len = slice.len + 1;
    if (new_len <= storage.len and slice.ptr == @as([*]Edge, storage)) {
        storage[slice.len] = edge;
        return storage[0..new_len];
    }
    if (slice.len == 0 and new_len <= storage.len) {
        storage[0] = edge;
        return storage[0..1];
    }
    var new_slice = try allocator.alloc(Edge, new_len);
    @memcpy(new_slice[0..slice.len], slice);
    new_slice[slice.len] = edge;
    return new_slice;
}

fn removeEdgeAt(slice: *[]Edge, idx: usize) Edge {
    const removed = slice.*[idx];
    slice.*[idx] = slice.*[slice.len - 1];
    slice.len -= 1;
    return removed;
}

// ============================================================================
// Tests
// ============================================================================

test "Block creation" {
    const allocator = std.testing.allocator;
    var func: Func = undefined;
    var b = Block.init(1, .plain, &func);
    defer b.deinit(allocator);
    try std.testing.expectEqual(@as(ID, 1), b.id);
    try std.testing.expectEqual(BlockKind.plain, b.kind);
}

test "Block control values" {
    const allocator = std.testing.allocator;
    var func: Func = undefined;
    var b = Block.init(1, .if_, &func);
    defer b.deinit(allocator);
    var v = Value.init(1, .const_bool, 0, &b, .{});
    b.setControl(&v);
    try std.testing.expectEqual(@as(usize, 1), b.numControls());
    try std.testing.expectEqual(@as(i32, 1), v.uses);
    b.resetControls();
    try std.testing.expectEqual(@as(usize, 0), b.numControls());
    try std.testing.expectEqual(@as(i32, 0), v.uses);
}

test "Block edge management" {
    const allocator = std.testing.allocator;
    var func: Func = undefined;
    var b1 = Block.init(1, .plain, &func);
    defer b1.deinit(allocator);
    var b2 = Block.init(2, .plain, &func);
    defer b2.deinit(allocator);
    try b1.addEdgeTo(allocator, &b2);
    try std.testing.expectEqual(@as(usize, 1), b1.succs.len);
    try std.testing.expectEqual(@as(usize, 1), b2.preds.len);
    try std.testing.expectEqual(&b2, b1.succs[0].b);
    try std.testing.expectEqual(&b1, b2.preds[0].b);
}
