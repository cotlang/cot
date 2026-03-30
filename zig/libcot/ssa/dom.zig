//! Dominator Tree Computation - iterative algorithm.

const std = @import("std");
const Func = @import("func.zig").Func;
const Block = @import("block.zig").Block;
const ID = @import("value.zig").ID;

pub const DomTree = struct {
    allocator: std.mem.Allocator,
    idom: []?*Block,
    children: []std.ArrayListUnmanaged(*Block),
    depth: []u32,
    max_id: ID,

    pub fn init(allocator: std.mem.Allocator, max_id: ID) !DomTree {
        const size = max_id + 1;
        const idom = try allocator.alloc(?*Block, size);
        @memset(idom, null);
        const children = try allocator.alloc(std.ArrayListUnmanaged(*Block), size);
        for (children) |*c| c.* = .{};
        const depth = try allocator.alloc(u32, size);
        @memset(depth, 0);
        return .{ .allocator = allocator, .idom = idom, .children = children, .depth = depth, .max_id = max_id };
    }

    pub fn deinit(self: *DomTree) void {
        for (self.children) |*c| c.deinit(self.allocator);
        self.allocator.free(self.idom);
        self.allocator.free(self.children);
        self.allocator.free(self.depth);
    }

    pub fn getIdom(self: *const DomTree, b: *const Block) ?*Block {
        return if (b.id <= self.max_id) self.idom[b.id] else null;
    }

    pub fn getChildren(self: *const DomTree, b: *const Block) []const *Block {
        return if (b.id <= self.max_id) self.children[b.id].items else &.{};
    }

    pub fn getDepth(self: *const DomTree, b: *const Block) u32 {
        return if (b.id <= self.max_id) self.depth[b.id] else 0;
    }

    pub fn dominates(self: *const DomTree, a: *const Block, b: *const Block) bool {
        var cur: ?*Block = @constCast(b);
        while (cur) |c| {
            if (c == a) return true;
            cur = self.getIdom(c);
        }
        return false;
    }

    pub fn strictlyDominates(self: *const DomTree, a: *const Block, b: *const Block) bool {
        return a != b and self.dominates(a, b);
    }
};

pub fn computeDominators(f: *Func) !DomTree {
    const allocator = f.allocator;
    var max_id: ID = 0;
    for (f.blocks.items) |b| if (b.id > max_id) { max_id = b.id; };

    var dom = try DomTree.init(allocator, max_id);
    errdefer dom.deinit();

    const entry = f.entry orelse return dom;
    const rpo = try reversePostorder(f);
    defer allocator.free(rpo);

    dom.idom[entry.id] = null;
    dom.depth[entry.id] = 0;

    // Iterative dataflow
    var changed = true;
    while (changed) {
        changed = false;
        for (rpo) |b| {
            if (b == entry) continue;
            var new_idom: ?*Block = null;
            for (b.preds) |pred_edge| {
                const pred = pred_edge.b;
                if (dom.idom[pred.id] == null and pred != entry) continue;
                new_idom = if (new_idom) |ni| intersect(&dom, ni, pred, entry) else pred;
            }
            if (new_idom != dom.idom[b.id]) {
                dom.idom[b.id] = new_idom;
                changed = true;
            }
        }
    }

    // Build children and compute depths
    for (f.blocks.items) |b| if (dom.idom[b.id]) |parent| try dom.children[parent.id].append(allocator, b);
    try computeDepths(&dom, entry, allocator);
    return dom;
}

fn intersect(dom: *const DomTree, b1: *Block, b2: *Block, entry: *Block) *Block {
    var f1, var f2 = .{ b1, b2 };
    while (f1 != f2) {
        while (rpoNum(f1, entry) > rpoNum(f2, entry)) f1 = dom.idom[f1.id] orelse entry;
        while (rpoNum(f2, entry) > rpoNum(f1, entry)) f2 = dom.idom[f2.id] orelse entry;
    }
    return f1;
}

fn rpoNum(b: *Block, entry: *Block) ID {
    return if (b == entry) 0 else b.id;
}

fn reversePostorder(f: *Func) ![]*Block {
    const allocator = f.allocator;
    const entry = f.entry orelse return &.{};
    var result = std.ArrayListUnmanaged(*Block){};
    var visited = std.AutoHashMapUnmanaged(*Block, void){};
    defer visited.deinit(allocator);

    var stack = std.ArrayListUnmanaged(struct { b: *Block, idx: usize }){};
    defer stack.deinit(allocator);
    try stack.append(allocator, .{ .b = entry, .idx = 0 });
    try visited.put(allocator, entry, {});

    while (stack.items.len > 0) {
        const top = &stack.items[stack.items.len - 1];
        if (top.idx < top.b.succs.len) {
            const succ = top.b.succs[top.idx].b;
            top.idx += 1;
            if (!visited.contains(succ)) {
                try visited.put(allocator, succ, {});
                try stack.append(allocator, .{ .b = succ, .idx = 0 });
            }
        } else {
            try result.append(allocator, top.b);
            _ = stack.pop();
        }
    }

    std.mem.reverse(*Block, result.items);
    return result.toOwnedSlice(allocator);
}

fn computeDepths(dom: *DomTree, entry: *Block, allocator: std.mem.Allocator) !void {
    var queue = std.ArrayListUnmanaged(*Block){};
    defer queue.deinit(allocator);
    try queue.append(allocator, entry);
    dom.depth[entry.id] = 0;

    var idx: usize = 0;
    while (idx < queue.items.len) : (idx += 1) {
        const b = queue.items[idx];
        for (dom.children[b.id].items) |child| {
            dom.depth[child.id] = dom.depth[b.id] + 1;
            try queue.append(allocator, child);
        }
    }
}

pub fn computeDominanceFrontier(dom: *const DomTree, f: *const Func, allocator: std.mem.Allocator) ![]std.ArrayListUnmanaged(*Block) {
    const frontier = try allocator.alloc(std.ArrayListUnmanaged(*Block), dom.max_id + 1);
    for (frontier) |*fr| fr.* = .{};

    for (f.blocks.items) |b| {
        if (b.preds.len < 2) continue;
        for (b.preds) |pred_edge| {
            var runner: ?*Block = pred_edge.b;
            while (runner != null and runner != dom.idom[b.id]) {
                try frontier[runner.?.id].append(allocator, b);
                runner = dom.idom[runner.?.id];
            }
        }
    }
    return frontier;
}

pub fn freeDominanceFrontier(frontier: []std.ArrayListUnmanaged(*Block), allocator: std.mem.Allocator) void {
    for (frontier) |*fr| fr.deinit(allocator);
    allocator.free(frontier);
}

// ============================================================================
// Tests
// ============================================================================

test "dominator tree simple" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const entry = try f.newBlock(.plain);
    const b1 = try f.newBlock(.plain);
    const b2 = try f.newBlock(.ret);
    try entry.addEdgeTo(allocator, b1);
    try b1.addEdgeTo(allocator, b2);

    var dom = try computeDominators(&f);
    defer dom.deinit();

    try std.testing.expect(dom.dominates(entry, entry));
    try std.testing.expect(dom.dominates(entry, b1));
    try std.testing.expect(dom.dominates(entry, b2));
    try std.testing.expect(dom.dominates(b1, b2));
    try std.testing.expect(!dom.dominates(b2, b1));
    try std.testing.expectEqual(entry, dom.getIdom(b1).?);
    try std.testing.expectEqual(b1, dom.getIdom(b2).?);
}

test "dominator tree with diamond" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test_diamond");
    defer f.deinit();

    const entry = try f.newBlock(.if_);
    const left = try f.newBlock(.plain);
    const right = try f.newBlock(.plain);
    const merge = try f.newBlock(.ret);
    try entry.addEdgeTo(allocator, left);
    try entry.addEdgeTo(allocator, right);
    try left.addEdgeTo(allocator, merge);
    try right.addEdgeTo(allocator, merge);

    var dom = try computeDominators(&f);
    defer dom.deinit();

    try std.testing.expect(dom.dominates(entry, left));
    try std.testing.expect(dom.dominates(entry, right));
    try std.testing.expect(dom.dominates(entry, merge));
    try std.testing.expect(!dom.dominates(left, right));
    try std.testing.expect(!dom.dominates(right, left));
    try std.testing.expect(!dom.strictlyDominates(left, merge));
    try std.testing.expect(!dom.strictlyDominates(right, merge));
    try std.testing.expectEqual(entry, dom.getIdom(merge).?);
}

test "dominator depths" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test_depth");
    defer f.deinit();

    const entry = try f.newBlock(.plain);
    const b1 = try f.newBlock(.plain);
    const b2 = try f.newBlock(.plain);
    const b3 = try f.newBlock(.ret);
    try entry.addEdgeTo(allocator, b1);
    try b1.addEdgeTo(allocator, b2);
    try b2.addEdgeTo(allocator, b3);

    var dom = try computeDominators(&f);
    defer dom.deinit();

    try std.testing.expectEqual(@as(u32, 0), dom.getDepth(entry));
    try std.testing.expectEqual(@as(u32, 1), dom.getDepth(b1));
    try std.testing.expectEqual(@as(u32, 2), dom.getDepth(b2));
    try std.testing.expectEqual(@as(u32, 3), dom.getDepth(b3));
}
