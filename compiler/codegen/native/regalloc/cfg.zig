//! CFG analysis for the register allocator.
//!
//! Ported from regalloc2's `src/cfg.rs`, `src/postorder.rs`, `src/domtree.rs`.
//!
//! This module provides:
//! - Postorder traversal of the CFG
//! - Dominator tree computation (Cooper-Harvey-Kennedy algorithm)
//! - Combined CFGInfo with instruction-to-block mapping and loop depth

const std = @import("std");
const index = @import("index.zig");
const operand = @import("operand.zig");
const output = @import("output.zig");

const Block = index.Block;
const Inst = index.Inst;
const InstRange = index.InstRange;
const ProgPoint = operand.ProgPoint;
const RegAllocError = output.RegAllocError;

//=============================================================================
// Postorder Traversal
//=============================================================================

/// Calculate postorder traversal of blocks.
///
/// Uses explicit DFS stack to avoid recursion.
/// Returns error if any block index is out of bounds.
///
/// The `Ctx` type must have a method `blockSuccs(Block) -> []const Block`.
pub fn calculatePostorder(
    comptime Ctx: type,
    ctx: Ctx,
    num_blocks: usize,
    entry: Block,
    visited: []bool,
    result: *std.ArrayListUnmanaged(Block),
    allocator: std.mem.Allocator,
) !void {
    const State = struct {
        block: Block,
        succ_idx: usize,
    };

    // Initialize visited array
    @memset(visited, false);

    var stack = std.ArrayListUnmanaged(State){};
    defer stack.deinit(allocator);

    result.clearRetainingCapacity();

    // Validate entry block
    if (entry.idx() >= num_blocks) {
        return error.InvalidBlock;
    }

    visited[entry.idx()] = true;
    try stack.append(allocator, .{ .block = entry, .succ_idx = 0 });

    while (stack.items.len > 0) {
        const state = &stack.items[stack.items.len - 1];
        const succs = ctx.blockSuccs(state.block);

        // Find next unvisited successor
        while (state.succ_idx < succs.len) {
            const succ = succs[state.succ_idx];
            state.succ_idx += 1;

            if (succ.idx() >= num_blocks) {
                return error.InvalidBlock;
            }

            if (!visited[succ.idx()]) {
                visited[succ.idx()] = true;
                try stack.append(allocator, .{ .block = succ, .succ_idx = 0 });
                break;
            }
        } else {
            // All successors visited, add to postorder
            try result.append(allocator, state.block);
            _ = stack.pop();
        }
    }
}

//=============================================================================
// Dominator Tree
//=============================================================================

/// Merge sets for dominator tree computation (intersect).
fn mergeSets(
    idom: []const Block,
    block_to_rpo: []const ?u32,
    node1_in: Block,
    node2_in: Block,
) Block {
    var node1 = node1_in;
    var node2 = node2_in;

    while (!node1.eql(node2)) {
        if (node1.isInvalid() or node2.isInvalid()) {
            return Block.invalid();
        }

        const rpo1 = block_to_rpo[node1.idx()].?;
        const rpo2 = block_to_rpo[node2.idx()].?;

        if (rpo1 > rpo2) {
            node1 = idom[node1.idx()];
        } else if (rpo2 > rpo1) {
            node2 = idom[node2.idx()];
        }
    }

    return node1;
}

/// Calculate dominator tree using Cooper-Harvey-Kennedy algorithm.
///
/// Reference: "A Simple, Fast Dominance Algorithm"
/// https://www.cs.rice.edu/~keith/EMBED/dom.pdf
///
/// The `Ctx` type must have a method `blockPreds(Block) -> []const Block`.
pub fn calculateDomtree(
    comptime Ctx: type,
    ctx: Ctx,
    postorder: []const Block,
    block_to_rpo: []?u32,
    idom: []Block,
    start: Block,
) void {
    const num_blocks = idom.len;

    // Compute block_to_rpo mapping (reverse postorder index)
    @memset(block_to_rpo, null);
    var rpo_idx: u32 = 0;
    var i = postorder.len;
    while (i > 0) {
        i -= 1;
        block_to_rpo[postorder[i].idx()] = rpo_idx;
        rpo_idx += 1;
    }

    // Initialize idom
    for (idom) |*dom| {
        dom.* = Block.invalid();
    }
    // Start node is its own dominator initially
    if (start.idx() < num_blocks) {
        idom[start.idx()] = start;
    }

    // Iterate until convergence
    var changed = true;
    while (changed) {
        changed = false;

        // Consider blocks in reverse postorder
        var j = postorder.len;
        while (j > 0) {
            j -= 1;
            const node = postorder[j];
            const rponum = block_to_rpo[node.idx()] orelse continue;

            // Find first predecessor with lower RPO number
            var parent = Block.invalid();
            const preds = ctx.blockPreds(node);
            for (preds) |pred| {
                const pred_rpo = block_to_rpo[pred.idx()] orelse continue;
                if (pred_rpo < rponum) {
                    parent = pred;
                    break;
                }
            }

            // Merge with other predecessors
            if (parent.isValid()) {
                for (preds) |pred| {
                    if (pred.eql(parent)) continue;
                    if (idom[pred.idx()].isInvalid()) continue;
                    parent = mergeSets(idom, block_to_rpo, parent, pred);
                }
            }

            if (parent.isValid() and !parent.eql(idom[node.idx()])) {
                idom[node.idx()] = parent;
                changed = true;
            }
        }
    }

    // Set start node's dominator to invalid (terminates dominates() loop)
    if (start.idx() < num_blocks) {
        idom[start.idx()] = Block.invalid();
    }
}

/// Check if block `a` dominates block `b`.
pub fn dominates(idom: []const Block, a: Block, b_in: Block) bool {
    var b = b_in;
    while (true) {
        if (a.eql(b)) return true;
        if (b.isInvalid()) return false;
        b = idom[b.idx()];
    }
}

//=============================================================================
// CFGInfo - Combined CFG analysis
//=============================================================================

/// Scratch space for CFGInfo computation (reusable across allocations).
pub const CFGInfoCtx = struct {
    visited: std.ArrayListUnmanaged(bool),
    block_to_rpo: std.ArrayListUnmanaged(?u32),
    backedge: std.ArrayListUnmanaged(u32),

    pub fn init() CFGInfoCtx {
        return .{
            .visited = .{},
            .block_to_rpo = .{},
            .backedge = .{},
        };
    }

    pub fn deinit(self: *CFGInfoCtx, allocator: std.mem.Allocator) void {
        self.visited.deinit(allocator);
        self.block_to_rpo.deinit(allocator);
        self.backedge.deinit(allocator);
    }
};

/// Combined CFG analysis results.
pub const CFGInfo = struct {
    /// Postorder traversal of blocks.
    postorder: std.ArrayListUnmanaged(Block),

    /// Domtree parents, indexed by block.
    domtree: std.ArrayListUnmanaged(Block),

    /// For each instruction, the block it belongs to.
    insn_block: std.ArrayListUnmanaged(Block),

    /// For each block, the first program point.
    block_entry: std.ArrayListUnmanaged(ProgPoint),

    /// For each block, the last program point.
    block_exit: std.ArrayListUnmanaged(ProgPoint),

    /// For each block, approximate loop depth.
    /// Precise iff CFG is reducible and blocks are in RPO.
    approx_loop_depth: std.ArrayListUnmanaged(u32),

    pub fn init() CFGInfo {
        return .{
            .postorder = .{},
            .domtree = .{},
            .insn_block = .{},
            .block_entry = .{},
            .block_exit = .{},
            .approx_loop_depth = .{},
        };
    }

    pub fn deinit(self: *CFGInfo, allocator: std.mem.Allocator) void {
        self.postorder.deinit(allocator);
        self.domtree.deinit(allocator);
        self.insn_block.deinit(allocator);
        self.block_entry.deinit(allocator);
        self.block_exit.deinit(allocator);
        self.approx_loop_depth.deinit(allocator);
    }

    /// Clear for reuse, retaining capacity.
    pub fn clear(self: *CFGInfo) void {
        self.postorder.clearRetainingCapacity();
        self.domtree.clearRetainingCapacity();
        self.insn_block.clearRetainingCapacity();
        self.block_entry.clearRetainingCapacity();
        self.block_exit.clearRetainingCapacity();
        self.approx_loop_depth.clearRetainingCapacity();
    }

    /// Initialize CFGInfo from a Function.
    ///
    /// The `Func` type must have methods:
    /// - numBlocks() -> usize
    /// - numInsts() -> usize
    /// - entryBlock() -> Block
    /// - blockInsns(Block) -> InstRange
    /// - blockSuccs(Block) -> []const Block
    /// - blockPreds(Block) -> []const Block
    /// - instOperands(Inst) -> []const anyopaque (only .len is used)
    pub fn compute(
        self: *CFGInfo,
        comptime Func: type,
        func: *const Func,
        scratch: *CFGInfoCtx,
        allocator: std.mem.Allocator,
    ) !void {
        const nb = func.numBlocks();
        const ni = func.numInsts();
        const entry = func.entryBlock();

        // Resize scratch arrays
        try scratch.visited.resize(allocator, nb);
        try scratch.block_to_rpo.resize(allocator, nb);
        try scratch.backedge.resize(allocator, nb * 2);

        // Calculate postorder
        try calculatePostorder(
            Func,
            func.*,
            nb,
            entry,
            scratch.visited.items,
            &self.postorder,
            allocator,
        );

        // Calculate dominator tree
        try self.domtree.resize(allocator, nb);
        calculateDomtree(
            Func,
            func.*,
            self.postorder.items,
            scratch.block_to_rpo.items,
            self.domtree.items,
            entry,
        );

        // Resize and initialize arrays
        try self.insn_block.resize(allocator, ni);
        @memset(self.insn_block.items, Block.invalid());

        try self.block_entry.resize(allocator, nb);
        try self.block_exit.resize(allocator, nb);

        // Initialize backedge counters
        const backedge_in = scratch.backedge.items[0..nb];
        const backedge_out = scratch.backedge.items[nb .. nb * 2];
        @memset(backedge_in, 0);
        @memset(backedge_out, 0);

        // Process each block
        for (0..nb) |block_idx| {
            const block = Block.new(block_idx);
            const insns = func.blockInsns(block);

            // Map instructions to block
            var iter = insns.iter();
            while (iter.next()) |inst| {
                self.insn_block.items[inst.idx()] = block;
            }

            // Block entry/exit points
            if (!insns.isEmpty()) {
                self.block_entry.items[block_idx] = ProgPoint.before(insns.first());
                self.block_exit.items[block_idx] = ProgPoint.after(insns.last());
            }

            // Check critical edge condition
            const preds = func.blockPreds(block);
            const num_preds = preds.len + @as(usize, if (block.eql(entry)) 1 else 0);
            if (num_preds > 1) {
                for (preds) |pred| {
                    const succs = func.blockSuccs(pred);
                    if (succs.len > 1) {
                        return error.CriticalEdge;
                    }
                }
            }

            // Check branch-arg condition
            var require_no_branch_args = false;
            const succs = func.blockSuccs(block);
            for (succs) |succ| {
                const succ_preds = func.blockPreds(succ);
                const succ_num_preds = succ_preds.len + @as(usize, if (succ.eql(entry)) 1 else 0);
                if (succ_num_preds > 1) {
                    require_no_branch_args = true;
                    break;
                }
            }
            if (require_no_branch_args and !insns.isEmpty()) {
                const last = insns.last();
                const ops = func.instOperands(last);
                if (ops.len > 0) {
                    return error.DisallowedBranchArg;
                }
            }

            // Track backedges for loop depth
            for (succs) |succ| {
                if (succ.idx() <= block_idx) {
                    backedge_in[succ.idx()] += 1;
                    backedge_out[block_idx] += 1;
                }
            }
        }

        // Calculate approximate loop depth
        try self.approx_loop_depth.resize(allocator, nb);

        var backedge_stack = std.ArrayListUnmanaged(u32){};
        defer backedge_stack.deinit(allocator);

        var cur_depth: u32 = 0;
        for (0..nb) |block_idx| {
            if (backedge_in[block_idx] > 0) {
                cur_depth += 1;
                try backedge_stack.append(allocator, backedge_in[block_idx]);
            }

            self.approx_loop_depth.items[block_idx] = cur_depth;

            while (backedge_stack.items.len > 0 and backedge_out[block_idx] > 0) {
                backedge_out[block_idx] -= 1;
                backedge_stack.items[backedge_stack.items.len - 1] -= 1;
                if (backedge_stack.items[backedge_stack.items.len - 1] == 0) {
                    cur_depth -= 1;
                    _ = backedge_stack.pop();
                }
            }
        }
    }

    /// Check if block `a` dominates block `b`.
    pub fn blockDominates(self: *const CFGInfo, a: Block, b: Block) bool {
        return dominates(self.domtree.items, a, b);
    }
};

//=============================================================================
// Tests
//=============================================================================

const TestCFG = struct {
    succs_data: []const []const Block,
    preds_data: []const []const Block,

    pub fn blockSuccs(self: TestCFG, block: Block) []const Block {
        return self.succs_data[block.idx()];
    }

    pub fn blockPreds(self: TestCFG, block: Block) []const Block {
        return self.preds_data[block.idx()];
    }
};

test "postorder simple" {
    // Simple CFG: 0 -> 1 -> 2
    const succs = [_][]const Block{
        &[_]Block{Block.new(1)}, // Block 0 -> 1
        &[_]Block{Block.new(2)}, // Block 1 -> 2
        &[_]Block{}, // Block 2 -> (none)
    };
    const preds = [_][]const Block{
        &[_]Block{},
        &[_]Block{Block.new(0)},
        &[_]Block{Block.new(1)},
    };
    const cfg = TestCFG{ .succs_data = &succs, .preds_data = &preds };

    var visited = [_]bool{false} ** 3;
    var result = std.ArrayListUnmanaged(Block){};
    defer result.deinit(std.testing.allocator);

    try calculatePostorder(TestCFG, cfg, 3, Block.new(0), &visited, &result, std.testing.allocator);

    // Postorder: 2, 1, 0
    try std.testing.expectEqual(@as(usize, 3), result.items.len);
    try std.testing.expectEqual(@as(usize, 2), result.items[0].idx());
    try std.testing.expectEqual(@as(usize, 1), result.items[1].idx());
    try std.testing.expectEqual(@as(usize, 0), result.items[2].idx());
}

test "postorder with branch" {
    // CFG: 0 -> 1, 0 -> 2, 1 -> 3, 2 -> 3
    const succs = [_][]const Block{
        &[_]Block{ Block.new(1), Block.new(2) }, // Block 0 -> 1, 2
        &[_]Block{Block.new(3)}, // Block 1 -> 3
        &[_]Block{Block.new(3)}, // Block 2 -> 3
        &[_]Block{}, // Block 3 -> (none)
    };
    const preds = [_][]const Block{
        &[_]Block{},
        &[_]Block{Block.new(0)},
        &[_]Block{Block.new(0)},
        &[_]Block{ Block.new(1), Block.new(2) },
    };
    const cfg = TestCFG{ .succs_data = &succs, .preds_data = &preds };

    var visited = [_]bool{false} ** 4;
    var result = std.ArrayListUnmanaged(Block){};
    defer result.deinit(std.testing.allocator);

    try calculatePostorder(TestCFG, cfg, 4, Block.new(0), &visited, &result, std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 4), result.items.len);
    // Block 0 should be last (entry)
    try std.testing.expectEqual(@as(usize, 0), result.items[3].idx());
}

test "domtree simple chain" {
    // CFG: 0 -> 1 -> 2
    const succs = [_][]const Block{
        &[_]Block{Block.new(1)},
        &[_]Block{Block.new(2)},
        &[_]Block{},
    };
    const preds = [_][]const Block{
        &[_]Block{}, // Block 0 has no preds
        &[_]Block{Block.new(0)}, // Block 1 <- 0
        &[_]Block{Block.new(1)}, // Block 2 <- 1
    };
    const cfg = TestCFG{ .succs_data = &succs, .preds_data = &preds };

    const postorder = [_]Block{ Block.new(2), Block.new(1), Block.new(0) };
    var block_to_rpo = [_]?u32{null} ** 3;
    var idom = [_]Block{Block.invalid()} ** 3;

    calculateDomtree(TestCFG, cfg, &postorder, &block_to_rpo, &idom, Block.new(0));

    // Block 0 dominates all
    try std.testing.expect(dominates(&idom, Block.new(0), Block.new(0)));
    try std.testing.expect(dominates(&idom, Block.new(0), Block.new(1)));
    try std.testing.expect(dominates(&idom, Block.new(0), Block.new(2)));

    // Block 1 dominates 2
    try std.testing.expect(dominates(&idom, Block.new(1), Block.new(2)));

    // Block 2 only dominates itself
    try std.testing.expect(dominates(&idom, Block.new(2), Block.new(2)));
    try std.testing.expect(!dominates(&idom, Block.new(2), Block.new(1)));
}

test "CFGInfoCtx init/deinit" {
    var ctx = CFGInfoCtx.init();
    defer ctx.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 0), ctx.visited.items.len);
}

test "CFGInfo init/deinit" {
    var info = CFGInfo.init();
    defer info.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 0), info.postorder.items.len);
}
