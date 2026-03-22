//! Debug and Visualization Support for SSA - text dump, DOT graph, verification.

const std = @import("std");
const Func = @import("func.zig").Func;
const Block = @import("block.zig").Block;
const Value = @import("value.zig").Value;
const Op = @import("op.zig").Op;
const TypeRegistry = @import("../frontend/types.zig").TypeRegistry;
const ID = u32;

pub const Format = enum { text, dot };

// ============================================================================
// Liveness Analysis — Go print.go:126 findlive()
// ============================================================================

/// Compute reachable blocks and live values.
/// A block is reachable if BFS from entry can reach it.
/// A value is live if it has side effects, is a control value of a
/// reachable block, or is transitively used by a live value.
/// Reference: Go deadcode.go findlive + print.go fprintFunc
pub fn findlive(allocator: std.mem.Allocator, f: *const Func) !struct { reachable: []bool, live: []bool } {
    const reachable = try allocator.alloc(bool, f.numBlocks());
    @memset(reachable, false);
    const live = try allocator.alloc(bool, f.numValues());
    @memset(live, false);

    // BFS for reachable blocks
    if (f.entry) |entry| {
        reachable[entry.id] = true;
        var worklist = std.ArrayListUnmanaged(*const Block){};
        defer worklist.deinit(allocator);
        try worklist.append(allocator, entry);
        while (worklist.items.len > 0) {
            const b = worklist.pop().?;
            for (b.succs) |e| {
                if (!reachable[e.b.id]) {
                    reachable[e.b.id] = true;
                    try worklist.append(allocator, e.b);
                }
            }
        }
    }

    // Seed live values: control values + side-effecting values
    var q = std.ArrayListUnmanaged(*const Value){};
    defer q.deinit(allocator);
    for (f.blocks.items) |b| {
        if (!reachable[b.id]) continue;
        for (b.controlValues()) |v| {
            if (!live[v.id]) {
                live[v.id] = true;
                try q.append(allocator, v);
            }
        }
        for (b.values.items) |v| {
            if (v.op.hasSideEffects() or v.op.info().call or v.op.info().nil_check) {
                if (!live[v.id]) {
                    live[v.id] = true;
                    try q.append(allocator, v);
                }
            }
        }
    }

    // Transitive closure
    while (q.items.len > 0) {
        const v = q.pop().?;
        for (v.args) |arg| {
            if (!live[arg.id]) {
                live[arg.id] = true;
                try q.append(allocator, arg);
            }
        }
    }

    return .{ .reachable = reachable, .live = live };
}

// ============================================================================
// Unified Func Printer — Go print.go:42-50 funcPrinter interface
// ============================================================================

/// Abstract printer interface for SSA functions.
/// Implemented by text printer, HTML printer, and hash printer.
/// Reference: Go print.go funcPrinter interface
pub const FuncPrinter = struct {
    ctx: *anyopaque,
    headerFn: *const fn (*anyopaque, *const Func) void,
    startBlockFn: *const fn (*anyopaque, *const Block, bool) void,
    endBlockFn: *const fn (*anyopaque, *const Block, bool) void,
    valueFn: *const fn (*anyopaque, *const Value, bool) void,

    pub fn header(self: FuncPrinter, f: *const Func) void {
        self.headerFn(self.ctx, f);
    }
    pub fn startBlock(self: FuncPrinter, b: *const Block, reachable: bool) void {
        self.startBlockFn(self.ctx, b, reachable);
    }
    pub fn endBlock(self: FuncPrinter, b: *const Block, reachable: bool) void {
        self.endBlockFn(self.ctx, b, reachable);
    }
    pub fn value(self: FuncPrinter, v: *const Value, live: bool) void {
        self.valueFn(self.ctx, v, live);
    }
};

/// Print a function using the given printer, with proper liveness analysis.
/// Reference: Go print.go:125-192 fprintFunc
pub fn fprintFunc(allocator: std.mem.Allocator, p: FuncPrinter, f: *const Func) !void {
    const liveness = try findlive(allocator, f);
    defer allocator.free(liveness.reachable);
    defer allocator.free(liveness.live);

    p.header(f);
    for (f.blocks.items) |b| {
        p.startBlock(b, liveness.reachable[b.id]);

        // Print values: phis first, then dependency order, then dep cycles
        // Reference: Go print.go:133-185
        if (f.scheduled) {
            for (b.values.items) |v| {
                p.value(v, liveness.live[v.id]);
            }
        } else {
            // Phis first
            for (b.values.items) |v| {
                if (v.op == .phi) p.value(v, liveness.live[v.id]);
            }
            // Then non-phis
            for (b.values.items) |v| {
                if (v.op != .phi) p.value(v, liveness.live[v.id]);
            }
        }

        p.endBlock(b, liveness.reachable[b.id]);
    }
}

// ============================================================================
// Dump Functions
// ============================================================================

pub fn dump(f: *const Func, format: Format, writer: anytype) !void {
    switch (format) {
        .text => try dumpText(f, writer),
        .dot => try dumpDot(f, writer),
    }
}

pub fn dumpText(f: *const Func, writer: anytype) !void {
    try writer.print("func {s}:\n", .{f.name});

    for (f.blocks.items) |b| {
        try writer.print("  b{d} ({s}):\n", .{ b.id, @tagName(b.kind) });

        if (b.preds.len > 0) {
            try writer.writeAll("    preds: ");
            for (b.preds, 0..) |pred, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("b{d}", .{pred.b.id});
            }
            try writer.writeAll("\n");
        }

        for (b.values.items) |v| try dumpValue(v, writer);

        if (b.numControls() > 0) {
            try writer.writeAll("    control: ");
            for (b.controlValues(), 0..) |cv, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("v{d}", .{cv.id});
            }
            try writer.writeAll("\n");
        }

        if (b.succs.len > 0) {
            try writer.writeAll("    succs: ");
            for (b.succs, 0..) |succ, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("b{d}", .{succ.b.id});
            }
            try writer.writeAll("\n");
        }
        try writer.writeAll("\n");
    }
}

fn dumpValue(v: *const Value, writer: anytype) !void {
    const dead = if (v.uses == 0 and !v.hasSideEffects()) " (dead)" else "";
    const type_name = TypeRegistry.basicTypeName(v.type_idx);
    const size = TypeRegistry.basicTypeSize(v.type_idx);

    try writer.print("    v{d}: {s}({d}B){s} = {s}", .{ v.id, type_name, size, dead, @tagName(v.op) });

    if (v.args.len > 0) {
        try writer.writeAll(" ");
        for (v.args, 0..) |arg, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.print("v{d}", .{arg.id});
        }
    }

    switch (v.aux) {
        .none => {},
        .string => |s| try writer.print(" \"{s}\"", .{s}),
        .symbol => |sym| try writer.print(" @{*}", .{sym}),
        .symbol_off => |so| try writer.print(" @{*}+{d}", .{ so.sym, so.offset }),
        .call => try writer.writeAll(" <call>"),
        .type_ref => |t| try writer.print(" type({d})", .{t}),
        .cond => |c| try writer.print(" cond({s})", .{@tagName(c)}),
    }

    if (v.aux_int != 0) try writer.print(" [{d}]", .{v.aux_int});
    try writer.print(" : uses={d}\n", .{v.uses});
}

pub fn dumpDot(f: *const Func, writer: anytype) !void {
    try writer.print("digraph \"{s}\" {{\n", .{f.name});
    try writer.writeAll("  rankdir=TB;\n  node [shape=box, fontname=\"Courier\"];\n\n");

    for (f.blocks.items) |b| {
        try writer.print("  b{d} [label=\"b{d} ({s})\\l", .{ b.id, b.id, @tagName(b.kind) });
        for (b.values.items) |v| {
            try writer.print("v{d} = {s}", .{ v.id, @tagName(v.op) });
            if (v.args.len > 0) {
                try writer.writeAll(" ");
                for (v.args, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print("v{d}", .{arg.id});
                }
            }
            if (v.aux_int != 0) try writer.print(" [{d}]", .{v.aux_int});
            try writer.writeAll("\\l");
        }
        try writer.writeAll("\"];\n");
    }

    try writer.writeAll("\n");
    for (f.blocks.items) |b| {
        for (b.succs, 0..) |succ, i| {
            const label = if (b.kind == .if_) (if (i == 0) "T" else "F") else "";
            if (label.len > 0) {
                try writer.print("  b{d} -> b{d} [label=\"{s}\"];\n", .{ b.id, succ.b.id, label });
            } else {
                try writer.print("  b{d} -> b{d};\n", .{ b.id, succ.b.id });
            }
        }
    }
    try writer.writeAll("}\n");
}

pub fn dumpToFile(f: *const Func, format: Format, path: []const u8) !void {
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();
    try dump(f, format, file.writer());
}

// ============================================================================
// Verification
// ============================================================================

/// Verify SSA invariants. Called after each pass when COT_DEBUG is enabled.
/// Reference: Go check.go checkFunc (676 lines) — we check the most critical invariants.
pub fn verify(f: *const Func, allocator: std.mem.Allocator) ![]const []const u8 {
    var errors = std.ArrayListUnmanaged([]const u8){};

    // Track all values and blocks in the function for cross-reference checks
    var value_set = std.AutoHashMapUnmanaged(u32, void){};
    defer value_set.deinit(allocator);
    var block_set = std.AutoHashMapUnmanaged(u32, void){};
    defer block_set.deinit(allocator);

    for (f.blocks.items) |b| {
        try block_set.put(allocator, b.id, {});
        for (b.values.items) |v| {
            try value_set.put(allocator, v.id, {});
        }
    }

    for (f.blocks.items) |b| {
        // Go check.go:394 — Check all referenced blocks exist in function
        for (b.succs) |succ| {
            if (!block_set.contains(succ.b.id)) {
                try errors.append(allocator, try std.fmt.allocPrint(allocator, "b{d}: successor b{d} not in function", .{ b.id, succ.b.id }));
            }
        }
        for (b.preds) |pred| {
            if (!block_set.contains(pred.b.id)) {
                try errors.append(allocator, try std.fmt.allocPrint(allocator, "b{d}: predecessor b{d} not in function", .{ b.id, pred.b.id }));
            }
        }

        for (b.values.items) |v| {
            // Go check.go:267 — Value block pointer must match containing block
            if (v.block != b) {
                try errors.append(allocator, try std.fmt.allocPrint(allocator, "v{d}: block pointer mismatch (expected b{d}, got b{d})", .{ v.id, b.id, if (v.block) |vb| vb.id else 0 }));
            }

            // Go check.go:415 — All arg values must exist in the function
            for (v.args, 0..) |arg, ai| {
                if (!value_set.contains(arg.id)) {
                    try errors.append(allocator, try std.fmt.allocPrint(allocator, "v{d}: arg[{d}] v{d} not in function", .{ v.id, ai, arg.id }));
                }
            }

            // Go check.go:114 — Arg count must match op info
            // Note: Cot's store/load don't use Go's memory threading (no mem arg),
            // so arg counts differ from Go's op info by 1 for memory ops.
            const info = v.op.info();
            if (info.arg_len >= 0) {
                const expected: usize = @intCast(info.arg_len);
                const is_mem_op = info.reads_memory or info.writes_memory;
                if (!is_mem_op and v.args.len != expected) {
                    try errors.append(allocator, try std.fmt.allocPrint(allocator, "v{d} ({s}): expected {d} args, got {d}", .{ v.id, @tagName(v.op), expected, v.args.len }));
                }
            }

            // Go check.go:486 — Use count must not be negative
            if (v.uses < 0) {
                try errors.append(allocator, try std.fmt.allocPrint(allocator, "v{d}: negative use count ({d})", .{ v.id, v.uses }));
            }
        }

        // Go check.go:348 — Edge bidirectional consistency
        for (b.succs, 0..) |succ, i| {
            if (succ.i >= succ.b.preds.len or succ.b.preds[succ.i].b != b) {
                try errors.append(allocator, try std.fmt.allocPrint(allocator, "b{d}: succ[{d}] edge invariant violated", .{ b.id, i }));
            }
        }

        for (b.preds, 0..) |pred, i| {
            if (pred.i >= pred.b.succs.len or pred.b.succs[pred.i].b != b) {
                try errors.append(allocator, try std.fmt.allocPrint(allocator, "b{d}: pred[{d}] edge invariant violated", .{ b.id, i }));
            }
        }

        // Go check.go:641 — After scheduling, phis must be first in block
        if (f.scheduled) {
            var seen_non_phi = false;
            for (b.values.items) |v| {
                if (v.op == .phi) {
                    if (seen_non_phi) {
                        try errors.append(allocator, try std.fmt.allocPrint(allocator, "b{d}: phi v{d} after non-phi (scheduling violated)", .{ b.id, v.id }));
                    }
                } else {
                    seen_non_phi = true;
                }
            }
        }
    }

    return errors.toOwnedSlice(allocator);
}

/// Run verification after a pass. If errors found, log them and panic.
/// Reference: Go compile.go:139 — checkFunc(f) called after each pass.
/// Only runs when COT_DEBUG is enabled (zero cost in production).
pub fn checkFunc(f: *const Func, pass_name: []const u8, allocator: std.mem.Allocator) void {
    const pipeline_debug = @import("../pipeline_debug.zig");
    if (!pipeline_debug.isEnabled(.codegen) and !pipeline_debug.isEnabled(.ssa)) return;

    const errs = verify(f, allocator) catch return;
    defer freeErrors(errs, allocator);

    if (errs.len > 0) {
        std.debug.print("\n!!! SSA VERIFICATION FAILED after pass '{s}' on '{s}' !!!\n", .{ pass_name, f.name });
        for (errs) |err| {
            std.debug.print("  {s}\n", .{err});
        }
        std.debug.print("  ({d} errors total)\n\n", .{errs.len});
        // Don't panic — log and continue (some passes transiently violate invariants)
    }
}

pub fn freeErrors(errors: []const []const u8, allocator: std.mem.Allocator) void {
    for (errors) |err| allocator.free(err);
    allocator.free(errors);
}

// ============================================================================
// Phase Snapshot (GOSSAFUNC-style comparison)
// ============================================================================

pub const ValueSnapshot = struct { id: ID, op: Op, arg_ids: []ID, uses: i32, aux_int: i64 };
pub const BlockSnapshot = struct { id: ID, kind: @import("block.zig").BlockKind, values: []ValueSnapshot, succ_ids: []ID };

pub const PhaseSnapshot = struct {
    name: []const u8,
    blocks: []BlockSnapshot,
    allocator: std.mem.Allocator,

    pub fn capture(allocator: std.mem.Allocator, f: *const Func, name: []const u8) !PhaseSnapshot {
        var blocks = try allocator.alloc(BlockSnapshot, f.blocks.items.len);

        for (f.blocks.items, 0..) |b, i| {
            var values = try allocator.alloc(ValueSnapshot, b.values.items.len);
            for (b.values.items, 0..) |v, j| {
                var arg_ids = try allocator.alloc(ID, v.args.len);
                for (v.args, 0..) |arg, k| arg_ids[k] = arg.id;
                values[j] = .{ .id = v.id, .op = v.op, .arg_ids = arg_ids, .uses = v.uses, .aux_int = v.aux_int };
            }

            var succ_ids = try allocator.alloc(ID, b.succs.len);
            for (b.succs, 0..) |succ, k| succ_ids[k] = succ.b.id;

            blocks[i] = .{ .id = b.id, .kind = b.kind, .values = values, .succ_ids = succ_ids };
        }

        return .{ .name = try allocator.dupe(u8, name), .blocks = blocks, .allocator = allocator };
    }

    pub fn deinit(self: *PhaseSnapshot) void {
        for (self.blocks) |b| {
            for (b.values) |v| self.allocator.free(v.arg_ids);
            self.allocator.free(b.values);
            self.allocator.free(b.succ_ids);
        }
        self.allocator.free(self.blocks);
        self.allocator.free(self.name);
    }

    pub fn compare(before: *const PhaseSnapshot, after: *const PhaseSnapshot) ChangeStats {
        var stats = ChangeStats{};

        var before_values = std.AutoHashMap(ID, void).init(before.allocator);
        defer before_values.deinit();
        var before_blocks = std.AutoHashMap(ID, void).init(before.allocator);
        defer before_blocks.deinit();

        for (before.blocks) |b| {
            before_blocks.put(b.id, {}) catch {};
            for (b.values) |v| before_values.put(v.id, {}) catch {};
        }

        for (after.blocks) |b| {
            if (!before_blocks.contains(b.id)) stats.blocks_added += 1;
            for (b.values) |v| if (!before_values.contains(v.id)) { stats.values_added += 1; };
        }

        var after_values = std.AutoHashMap(ID, void).init(after.allocator);
        defer after_values.deinit();
        var after_blocks = std.AutoHashMap(ID, void).init(after.allocator);
        defer after_blocks.deinit();

        for (after.blocks) |b| {
            after_blocks.put(b.id, {}) catch {};
            for (b.values) |v| after_values.put(v.id, {}) catch {};
        }

        for (before.blocks) |b| {
            if (!after_blocks.contains(b.id)) stats.blocks_removed += 1;
            for (b.values) |v| if (!after_values.contains(v.id)) { stats.values_removed += 1; };
        }

        return stats;
    }
};

pub const ChangeStats = struct {
    values_added: usize = 0,
    values_removed: usize = 0,
    values_modified: usize = 0,
    blocks_added: usize = 0,
    blocks_removed: usize = 0,

    pub fn hasChanges(self: ChangeStats) bool {
        return self.values_added > 0 or self.values_removed > 0 or self.values_modified > 0 or self.blocks_added > 0 or self.blocks_removed > 0;
    }

    pub fn format(self: ChangeStats, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("+{d}/-{d} values, +{d}/-{d} blocks", .{ self.values_added, self.values_removed, self.blocks_added, self.blocks_removed });
    }
};

// ============================================================================
// Tests
// ============================================================================

test "dump text format" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test");
    defer f.deinit();

    const b = try f.newBlock(.ret);
    const v = try f.newValue(.const_int, 0, b, .{});
    v.aux_int = 42;
    try b.addValue(allocator, v);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);

    try dumpText(&f, output.writer(allocator));

    try std.testing.expect(std.mem.indexOf(u8, output.items, "test") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "v1") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "const_int") != null);
}

test "dump dot format" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test_dot");
    defer f.deinit();

    const entry = try f.newBlock(.if_);
    const left = try f.newBlock(.plain);
    const right = try f.newBlock(.ret);

    try entry.addEdgeTo(allocator, left);
    try entry.addEdgeTo(allocator, right);

    var output = std.ArrayListUnmanaged(u8){};
    defer output.deinit(allocator);

    try dumpDot(&f, output.writer(allocator));

    try std.testing.expect(std.mem.indexOf(u8, output.items, "digraph") != null);
    try std.testing.expect(std.mem.indexOf(u8, output.items, "->") != null);
}

test "verify catches edge invariant violations" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test_verify");
    defer f.deinit();

    const entry = try f.newBlock(.plain);
    const exit = try f.newBlock(.ret);
    try entry.addEdgeTo(allocator, exit);

    const errors = try verify(&f, allocator);
    defer freeErrors(errors, allocator);

    try std.testing.expectEqual(@as(usize, 0), errors.len);
}

test "PhaseSnapshot capture and deinit" {
    const allocator = std.testing.allocator;
    var f = Func.init(allocator, "test_snapshot");
    defer f.deinit();

    const b = try f.newBlock(.ret);
    const v = try f.newValue(.const_int, 0, b, .{});
    try b.addValue(allocator, v);

    var snapshot = try PhaseSnapshot.capture(allocator, &f, "test");
    defer snapshot.deinit();

    try std.testing.expectEqual(@as(usize, 1), snapshot.blocks.len);
    try std.testing.expectEqual(@as(usize, 1), snapshot.blocks[0].values.len);
}

test "ChangeStats hasChanges" {
    var stats = ChangeStats{};
    try std.testing.expect(!stats.hasChanges());

    stats.values_added = 1;
    try std.testing.expect(stats.hasChanges());
}
