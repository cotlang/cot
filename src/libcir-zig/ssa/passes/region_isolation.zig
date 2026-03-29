//! Region Isolation Analysis - Swift SE-0430 (sending) + SE-0414 (regions)
//!
//! Forward dataflow analysis on SSA that tracks value isolation regions.
//! Detects use-after-send errors where a value is used after being transferred
//! via a `sending` parameter.
//!
//! Algorithm (forward fixed-point):
//!   1. Initialize entry block: AssignFresh for each function parameter
//!   2. Worklist loop in RPO:
//!      a. Compute entry = join(predecessor exit partitions)
//!      b. Apply PartitionOps for each SSA value in block
//!      c. If exit partition changed, mark successors for re-visit
//!   3. Errors detected when Require finds a sent region

const std = @import("std");
const Func = @import("../func.zig").Func;
const Block = @import("../block.zig").Block;
const Value = @import("../value.zig").Value;
const Op = @import("../op.zig").Op;
const foundation = @import("foundation");
const debug = foundation.debug;

const Partition = struct {
    element_to_region: std.AutoHashMapUnmanaged(u32, u32),
    sent_regions: std.AutoHashMapUnmanaged(u32, void),
    fresh_label: u32,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) Partition {
        return .{
            .element_to_region = .{},
            .sent_regions = .{},
            .fresh_label = 0,
            .allocator = allocator,
        };
    }

    fn deinit(self: *Partition) void {
        self.element_to_region.deinit(self.allocator);
        self.sent_regions.deinit(self.allocator);
    }

    fn clone(self: *const Partition) !Partition {
        return .{
            .element_to_region = try self.element_to_region.clone(self.allocator),
            .sent_regions = try self.sent_regions.clone(self.allocator),
            .fresh_label = self.fresh_label,
            .allocator = self.allocator,
        };
    }

    fn assignFresh(self: *Partition, element: u32) !void {
        const region = self.fresh_label;
        self.fresh_label += 1;
        try self.element_to_region.put(self.allocator, element, region);
    }

    fn assignDirect(self: *Partition, dst: u32, src: u32) !void {
        const src_region = self.element_to_region.get(src) orelse return;
        try self.element_to_region.put(self.allocator, dst, src_region);
    }

    fn merge(self: *Partition, elt1: u32, elt2: u32) !void {
        const r1 = self.element_to_region.get(elt1) orelse return;
        const r2 = self.element_to_region.get(elt2) orelse return;
        if (r1 == r2) return;
        const keep = @min(r1, r2);
        const replace = @max(r1, r2);
        var it = self.element_to_region.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* == replace) {
                entry.value_ptr.* = keep;
            }
        }
        if (self.sent_regions.contains(replace)) {
            try self.sent_regions.put(self.allocator, keep, {});
            _ = self.sent_regions.remove(replace);
        }
    }

    fn markSent(self: *Partition, element: u32) !void {
        const region = self.element_to_region.get(element) orelse return;
        try self.sent_regions.put(self.allocator, region, {});
    }

    fn isSent(self: *const Partition, element: u32) bool {
        const region = self.element_to_region.get(element) orelse return false;
        return self.sent_regions.contains(region);
    }

    fn join(allocator: std.mem.Allocator, p1: *const Partition, p2: *const Partition) !Partition {
        var result = Partition.init(allocator);
        result.fresh_label = @max(p1.fresh_label, p2.fresh_label);

        var all_elements = std.AutoHashMapUnmanaged(u32, void){};
        defer all_elements.deinit(allocator);
        {
            var it = p1.element_to_region.iterator();
            while (it.next()) |e| try all_elements.put(allocator, e.key_ptr.*, {});
        }
        {
            var it = p2.element_to_region.iterator();
            while (it.next()) |e| try all_elements.put(allocator, e.key_ptr.*, {});
        }

        var eit = all_elements.iterator();
        while (eit.next()) |e| {
            try result.assignFresh(e.key_ptr.*);
        }

        {
            var region_members = std.AutoHashMapUnmanaged(u32, u32){};
            defer region_members.deinit(allocator);
            var it = p1.element_to_region.iterator();
            while (it.next()) |entry| {
                if (region_members.get(entry.value_ptr.*)) |existing| {
                    try result.merge(existing, entry.key_ptr.*);
                } else {
                    try region_members.put(allocator, entry.value_ptr.*, entry.key_ptr.*);
                }
            }
        }

        {
            var region_members = std.AutoHashMapUnmanaged(u32, u32){};
            defer region_members.deinit(allocator);
            var it = p2.element_to_region.iterator();
            while (it.next()) |entry| {
                if (region_members.get(entry.value_ptr.*)) |existing| {
                    try result.merge(existing, entry.key_ptr.*);
                } else {
                    try region_members.put(allocator, entry.value_ptr.*, entry.key_ptr.*);
                }
            }
        }

        {
            var it = p1.element_to_region.iterator();
            while (it.next()) |entry| {
                if (p1.sent_regions.contains(entry.value_ptr.*)) {
                    try result.markSent(entry.key_ptr.*);
                }
            }
        }
        {
            var it = p2.element_to_region.iterator();
            while (it.next()) |entry| {
                if (p2.sent_regions.contains(entry.value_ptr.*)) {
                    try result.markSent(entry.key_ptr.*);
                }
            }
        }

        return result;
    }

    fn eql(p1: *const Partition, p2: *const Partition) bool {
        if (p1.element_to_region.count() != p2.element_to_region.count()) return false;
        if (p1.sent_regions.count() != p2.sent_regions.count()) return false;

        var it = p1.element_to_region.iterator();
        while (it.next()) |entry| {
            const r2 = p2.element_to_region.get(entry.key_ptr.*) orelse return false;
            if (entry.value_ptr.* != r2) return false;
        }
        var sit = p1.sent_regions.iterator();
        while (sit.next()) |entry| {
            if (!p2.sent_regions.contains(entry.key_ptr.*)) return false;
        }
        return true;
    }
};

const BlockState = struct {
    entry: Partition,
    exit: Partition,
    needs_update: bool,
};

pub fn regionIsolation(f: *Func) !u32 {
    const allocator = f.allocator;
    var diagnostics: u32 = 0;

    if (f.blocks.items.len == 0) return 0;

    const num_blocks = f.blocks.items.len;
    var block_ops = try allocator.alloc(std.ArrayListUnmanaged(PartitionOp), num_blocks);
    defer {
        for (block_ops) |*ops| ops.deinit(allocator);
        allocator.free(block_ops);
    }
    for (block_ops) |*ops| ops.* = .{};

    for (f.blocks.items, 0..) |b, bi| {
        for (b.values.items) |v| {
            for (v.args) |arg| {
                try block_ops[bi].append(allocator, .{ .kind = .require, .arg1 = arg.id });
            }

            if (v.op.info().call) {
                for (v.args) |arg| {
                    if (isSendingArg(arg)) {
                        try block_ops[bi].append(allocator, .{ .kind = .send, .arg1 = arg.id });
                    }
                }
            }

            if (v.op == .copy) {
                if (v.args.len > 0) {
                    try block_ops[bi].append(allocator, .{
                        .kind = .assign_direct,
                        .arg1 = v.id,
                        .arg2 = v.args[0].id,
                    });
                }
            }

            if (v.op == .arg or v.op == .const_int or v.op == .const_float or
                v.op == .const_string or v.op == .const_ptr)
            {
                try block_ops[bi].append(allocator, .{ .kind = .assign_fresh, .arg1 = v.id });
            }
        }
    }

    var block_states = try allocator.alloc(BlockState, num_blocks);
    defer {
        for (block_states) |*bs| {
            bs.entry.deinit();
            bs.exit.deinit();
        }
        allocator.free(block_states);
    }

    for (block_states) |*bs| {
        bs.entry = Partition.init(allocator);
        bs.exit = Partition.init(allocator);
        bs.needs_update = true;
    }

    if (f.blocks.items.len > 0) {
        for (f.blocks.items[0].values.items) |v| {
            if (v.op == .arg) {
                try block_states[0].entry.assignFresh(v.id);
            }
        }
    }

    var changed = true;
    var iteration: u32 = 0;
    const max_iterations: u32 = 100;
    while (changed and iteration < max_iterations) {
        changed = false;
        iteration += 1;

        for (0..num_blocks) |bi| {
            if (!block_states[bi].needs_update) continue;
            block_states[bi].needs_update = false;

            if (bi > 0) {
                const b = f.blocks.items[bi];
                var new_entry = Partition.init(allocator);
                var has_preds = false;
                for (b.preds) |pred_edge| {
                    const pred_idx = blockIndex(f, pred_edge.b) orelse continue;
                    if (!has_preds) {
                        new_entry.deinit();
                        new_entry = try block_states[pred_idx].exit.clone();
                        has_preds = true;
                    } else {
                        const joined = try Partition.join(allocator, &new_entry, &block_states[pred_idx].exit);
                        new_entry.deinit();
                        new_entry = joined;
                    }
                }
                block_states[bi].entry.deinit();
                block_states[bi].entry = new_entry;
            }

            var working = try block_states[bi].entry.clone();
            for (block_ops[bi].items) |op| {
                switch (op.kind) {
                    .assign_fresh => try working.assignFresh(op.arg1),
                    .assign_direct => try working.assignDirect(op.arg1, op.arg2),
                    .merge => try working.merge(op.arg1, op.arg2),
                    .send => try working.markSent(op.arg1),
                    .require => {
                        if (working.isSent(op.arg1)) {
                            debug.log(.codegen, "region_isolation: use-after-send: v{d} in '{s}'", .{
                                op.arg1, f.name,
                            });
                            diagnostics += 1;
                        }
                    },
                }
            }

            if (!Partition.eql(&block_states[bi].exit, &working)) {
                block_states[bi].exit.deinit();
                block_states[bi].exit = working;
                changed = true;
                const b = f.blocks.items[bi];
                for (b.succs) |succ_edge| {
                    if (blockIndex(f, succ_edge.b)) |succ_idx| {
                        block_states[succ_idx].needs_update = true;
                    }
                }
            } else {
                working.deinit();
            }
        }
    }

    if (diagnostics > 0) {
        debug.log(.codegen, "region_isolation: {d} use-after-send errors in '{s}' ({d} iterations)", .{
            diagnostics, f.name, iteration,
        });
    }

    return diagnostics;
}

fn isSendingArg(arg: *const Value) bool {
    // Sending detection requires IR-level local metadata which is not available
    // on the SSA Func struct. This is a placeholder that always returns false;
    // the full implementation requires wiring IR locals into the SSA func.
    _ = arg;
    return false;
}

fn blockIndex(f: *const Func, block: *const Block) ?usize {
    for (f.blocks.items, 0..) |b, i| {
        if (b == block) return i;
    }
    return null;
}

const PartitionOp = struct {
    kind: PartitionOpKind,
    arg1: u32,
    arg2: u32 = 0,
};

const PartitionOpKind = enum(u8) {
    send,
    require,
    assign_direct,
    assign_fresh,
    merge,
};
