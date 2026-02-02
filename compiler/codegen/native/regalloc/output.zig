//! Output types for the register allocator.
//!
//! Ported from regalloc2's `src/lib.rs` Output struct and related types.
//!
//! This module defines the result of register allocation, including
//! allocations, edits (inserted moves), and error types.

const std = @import("std");
const index = @import("index.zig");
const operand = @import("operand.zig");

const Block = index.Block;
const Inst = index.Inst;
const InstRange = index.InstRange;
const VReg = index.VReg;
const PReg = index.PReg;
const RegClass = index.RegClass;
const Allocation = operand.Allocation;
const ProgPoint = operand.ProgPoint;
const Edit = operand.Edit;

//=============================================================================
// Stats - Allocator statistics
//=============================================================================

/// Internal statistics from the allocator.
/// Useful for debugging and performance analysis.
pub const Stats = struct {
    livein_blocks: usize = 0,
    livein_iterations: usize = 0,
    initial_liverange_count: usize = 0,
    merged_bundle_count: usize = 0,
    process_bundle_count: usize = 0,
    process_bundle_reg_probes_fixed: usize = 0,
    process_bundle_reg_success_fixed: usize = 0,
    process_bundle_bounding_range_probe_start_any: usize = 0,
    process_bundle_bounding_range_probes_any: usize = 0,
    process_bundle_bounding_range_success_any: usize = 0,
    process_bundle_reg_probe_start_any: usize = 0,
    process_bundle_reg_probes_any: usize = 0,
    process_bundle_reg_success_any: usize = 0,
    evict_bundle_event: usize = 0,
    evict_bundle_count: usize = 0,
    splits: usize = 0,
    splits_clobbers: usize = 0,
    splits_hot: usize = 0,
    splits_conflicts: usize = 0,
    splits_defs: usize = 0,
    splits_all: usize = 0,
    final_liverange_count: usize = 0,
    final_bundle_count: usize = 0,
    spill_bundle_count: usize = 0,
    spill_bundle_reg_probes: usize = 0,
    spill_bundle_reg_success: usize = 0,
    blockparam_ins_count: usize = 0,
    blockparam_outs_count: usize = 0,
    halfmoves_count: usize = 0,
    edits_count: usize = 0,
};

//=============================================================================
// DebugLocation - Debug info for value tracking
//=============================================================================

/// Debug location: (label_id, start, end, allocation)
pub const DebugLocation = struct {
    label: u32,
    start: ProgPoint,
    end: ProgPoint,
    alloc: Allocation,
};

//=============================================================================
// Output - The result of register allocation
//=============================================================================

/// The output of the register allocator.
pub const Output = struct {
    /// How many spillslots are needed in the frame?
    num_spillslots: usize,

    /// Edits (move insertions). Guaranteed to be sorted by program point.
    edits: std.ArrayListUnmanaged(EditAtPoint),

    /// Allocations for each operand. Indexed via inst_alloc_offsets.
    allocs: std.ArrayListUnmanaged(Allocation),

    /// Allocation offset in `allocs` for each instruction.
    /// inst_allocs(i) = allocs[inst_alloc_offsets[i]..inst_alloc_offsets[i+1]]
    inst_alloc_offsets: std.ArrayListUnmanaged(u32),

    /// Debug info: value locations for DWARF generation.
    /// Sorted by label and program point, ranges are disjoint.
    debug_locations: std.ArrayListUnmanaged(DebugLocation),

    /// Internal stats from the allocator.
    stats: Stats,

    pub const EditAtPoint = struct {
        point: ProgPoint,
        edit: Edit,
    };

    /// Initialize an empty Output.
    pub fn init() Output {
        return .{
            .num_spillslots = 0,
            .edits = .{},
            .allocs = .{},
            .inst_alloc_offsets = .{},
            .debug_locations = .{},
            .stats = .{},
        };
    }

    /// Free all allocated memory.
    pub fn deinit(self: *Output, allocator: std.mem.Allocator) void {
        self.edits.deinit(allocator);
        self.allocs.deinit(allocator);
        self.inst_alloc_offsets.deinit(allocator);
        self.debug_locations.deinit(allocator);
    }

    /// Clear the output for reuse.
    pub fn clear(self: *Output) void {
        self.num_spillslots = 0;
        self.edits.clearRetainingCapacity();
        self.allocs.clearRetainingCapacity();
        self.inst_alloc_offsets.clearRetainingCapacity();
        self.debug_locations.clearRetainingCapacity();
        self.stats = .{};
    }

    /// Get the allocations assigned to a given instruction.
    pub fn instAllocs(self: *const Output, inst: Inst) []const Allocation {
        const idx = inst.idx();
        if (idx >= self.inst_alloc_offsets.items.len) {
            return &.{};
        }
        const start = self.inst_alloc_offsets.items[idx];
        const end = if (idx + 1 < self.inst_alloc_offsets.items.len)
            self.inst_alloc_offsets.items[idx + 1]
        else
            @as(u32, @intCast(self.allocs.items.len));
        return self.allocs.items[start..end];
    }

    /// Returns an iterator over instructions and edits in a block, in order.
    /// The Function interface is needed to get block instruction ranges.
    pub fn blockInstsAndEdits(
        self: *const Output,
        inst_range: InstRange,
    ) OutputIterator {
        // Binary search to find the first edit >= block start
        const edit_idx = self.findFirstEditIdx(inst_range);
        return .{
            .edits = self.edits.items[edit_idx..],
            .inst_range = inst_range,
        };
    }

    /// Find the index of the first edit at or after the start of the block.
    fn findFirstEditIdx(self: *const Output, inst_range: InstRange) usize {
        if (inst_range.isEmpty()) return self.edits.items.len;

        const target = ProgPoint.before(inst_range.first());

        // Binary search
        var left: usize = 0;
        var right: usize = self.edits.items.len;

        while (left < right) {
            const mid = left + (right - left) / 2;
            if (self.edits.items[mid].point.order(target) == .lt) {
                left = mid + 1;
            } else {
                right = mid;
            }
        }

        return left;
    }
};

//=============================================================================
// OutputIterator - Iterate over instructions and edits
//=============================================================================

/// Wrapper around either an original instruction or an inserted edit.
pub const InstOrEdit = union(enum) {
    inst: Inst,
    edit: *const Edit,
};

/// Iterator over the instructions and edits in a block.
pub const OutputIterator = struct {
    edits: []const Output.EditAtPoint,
    inst_range: InstRange,

    pub fn next(self: *OutputIterator) ?InstOrEdit {
        // No more instructions = done
        if (self.inst_range.isEmpty()) {
            return null;
        }

        // Return any edits that happen before the next instruction
        const next_inst = self.inst_range.first();
        if (self.edits.len > 0) {
            const edit = &self.edits[0];
            if (edit.point.order(ProgPoint.before(next_inst)) != .gt) {
                self.edits = self.edits[1..];
                return .{ .edit = &edit.edit };
            }
        }

        // Return the next instruction
        self.inst_range = self.inst_range.rest();
        return .{ .inst = next_inst };
    }
};

//=============================================================================
// RegAllocError - Errors that prevent allocation
//=============================================================================

/// An error that prevents allocation.
pub const RegAllocError = union(enum) {
    /// Critical edge is not split between given blocks.
    crit_edge: struct { from: Block, to: Block },

    /// Invalid SSA for given vreg at given inst: multiple defs or illegal use.
    /// inst may be invalid if this concerns a block param.
    ssa: struct { vreg: VReg, inst: Inst },

    /// Invalid basic block: does not end in branch/ret, or contains
    /// branch/ret in the middle, or VReg/Block ids not sequential from 0.
    bb: Block,

    /// Invalid branch: operand count doesn't match sum of block params
    /// of successor blocks, or block ids not sequential from 0.
    branch: Inst,

    /// A VReg is live-in on entry; this is not allowed.
    entry_livein,

    /// A branch has non-blockparam arg(s) and at least one successor
    /// has multiple predecessors, forcing edge-moves before this branch.
    /// This is disallowed; insert an edge block to avoid.
    disallowed_branch_arg: Inst,

    /// Too many pinned VRegs + Reg-constrained Operands are live at once.
    too_many_live_regs,

    /// Too many operands on a single instruction (beyond 2^16 - 1).
    too_many_operands,

    pub fn format(
        self: RegAllocError,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .crit_edge => |e| try writer.print("CritEdge({d}, {d})", .{ e.from.idx(), e.to.idx() }),
            .ssa => |e| try writer.print("SSA(v{d}, inst {d})", .{ e.vreg.vreg(), e.inst.index }),
            .bb => |b| try writer.print("BB({d})", .{b.idx()}),
            .branch => |i| try writer.print("Branch({d})", .{i.idx()}),
            .entry_livein => try writer.writeAll("EntryLivein"),
            .disallowed_branch_arg => |i| try writer.print("DisallowedBranchArg({d})", .{i.idx()}),
            .too_many_live_regs => try writer.writeAll("TooManyLiveRegs"),
            .too_many_operands => try writer.writeAll("TooManyOperands"),
        }
    }
};

//=============================================================================
// RegallocOptions - Options for allocation
//=============================================================================

/// Algorithm selection.
pub const Algorithm = enum {
    /// Ion backtracking allocator (default, higher quality).
    ion,
    /// Fast allocator (faster, lower quality).
    fastalloc,
};

/// Options for allocation.
pub const RegallocOptions = struct {
    /// Add extra verbosity to debug logs.
    verbose_log: bool = false,

    /// Run the SSA validator before allocating registers.
    validate_ssa: bool = false,

    /// The register allocation algorithm to use.
    algorithm: Algorithm = .ion,
};

//=============================================================================
// Tests
//=============================================================================

test "Output init and deinit" {
    var output = Output.init();
    defer output.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 0), output.num_spillslots);
    try std.testing.expectEqual(@as(usize, 0), output.edits.items.len);
    try std.testing.expectEqual(@as(usize, 0), output.allocs.items.len);
}

test "Output instAllocs" {
    var output = Output.init();
    defer output.deinit(std.testing.allocator);

    // Set up allocations for 3 instructions
    // Inst 0: 2 operands, Inst 1: 1 operand, Inst 2: 3 operands
    try output.inst_alloc_offsets.append(std.testing.allocator, 0);
    try output.inst_alloc_offsets.append(std.testing.allocator, 2);
    try output.inst_alloc_offsets.append(std.testing.allocator, 3);

    const r0 = PReg.new(0, .int);
    const r1 = PReg.new(1, .int);
    const r2 = PReg.new(2, .int);

    try output.allocs.append(std.testing.allocator, Allocation.reg(r0));
    try output.allocs.append(std.testing.allocator, Allocation.reg(r1));
    try output.allocs.append(std.testing.allocator, Allocation.reg(r2));
    try output.allocs.append(std.testing.allocator, Allocation.reg(r0));
    try output.allocs.append(std.testing.allocator, Allocation.reg(r1));
    try output.allocs.append(std.testing.allocator, Allocation.reg(r2));

    // Inst 0 has 2 allocations
    const allocs0 = output.instAllocs(Inst.new(0));
    try std.testing.expectEqual(@as(usize, 2), allocs0.len);
    try std.testing.expect(allocs0[0].asReg().?.eql(r0));
    try std.testing.expect(allocs0[1].asReg().?.eql(r1));

    // Inst 1 has 1 allocation
    const allocs1 = output.instAllocs(Inst.new(1));
    try std.testing.expectEqual(@as(usize, 1), allocs1.len);
    try std.testing.expect(allocs1[0].asReg().?.eql(r2));

    // Inst 2 has 3 allocations (rest of allocs)
    const allocs2 = output.instAllocs(Inst.new(2));
    try std.testing.expectEqual(@as(usize, 3), allocs2.len);
}

test "OutputIterator basic" {
    var output = Output.init();
    defer output.deinit(std.testing.allocator);

    // No edits, just iterate instructions
    const inst_range = InstRange.new(Inst.new(0), Inst.new(3));
    var iter = output.blockInstsAndEdits(inst_range);

    var count: usize = 0;
    while (iter.next()) |item| {
        switch (item) {
            .inst => count += 1,
            .edit => {},
        }
    }
    try std.testing.expectEqual(@as(usize, 3), count);
}

test "RegAllocError formatting" {
    const err = RegAllocError{ .crit_edge = .{ .from = Block.new(0), .to = Block.new(1) } };

    var buf: [64]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    try err.format("", .{}, fbs.writer());
    try std.testing.expectEqualStrings("CritEdge(0, 1)", fbs.getWritten());
}

test "Stats default values" {
    const stats = Stats{};
    try std.testing.expectEqual(@as(usize, 0), stats.livein_blocks);
    try std.testing.expectEqual(@as(usize, 0), stats.edits_count);
}

test "RegallocOptions defaults" {
    const options = RegallocOptions{};
    try std.testing.expect(!options.verbose_log);
    try std.testing.expect(!options.validate_ssa);
    try std.testing.expectEqual(Algorithm.ion, options.algorithm);
}
