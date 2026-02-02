//! Computation of basic block order in emitted code.
//!
//! This is a faithful port of Cranelift's `machinst/blockorder.rs`.
//!
//! This module handles the translation from CLIF BBs to VCode BBs.
//!
//! The basic idea is that we compute a sequence of "lowered blocks" that
//! correspond to one or more blocks in the graph: (CLIF CFG) `union` (implicit
//! block on *every* edge). Conceptually, the lowering pipeline wants to insert
//! moves for phi-nodes on every block-to-block transfer; these blocks always
//! conceptually exist, but may be merged with an "original" CLIF block (and
//! hence not actually exist; this is equivalent to inserting the blocks only on
//! critical edges).
//!
//! In other words, starting from a CFG like this (where each "CLIF block" and
//! "(edge N->M)" is a separate basic block):
//!
//! ```plain
//!
//!              CLIF block 0
//!               /           \
//!       (edge 0->1)         (edge 0->2)
//!              |                |
//!       CLIF block 1         CLIF block 2
//!              \                /
//!           (edge 1->3)   (edge 2->3)
//!                   \      /
//!                 CLIF block 3
//! ```
//!
//! We can produce a CFG of lowered blocks like so:
//!
//! ```plain
//!            +--------------+
//!            | CLIF block 0 |
//!            +--------------+
//!               /           \
//!     +--------------+     +--------------+
//!     | (edge 0->1)  |     | (edge 0->2)  |
//!     | CLIF block 1 |     | CLIF block 2 |
//!     | (edge 1->3)  |     | (edge 2->3)  |
//!     +--------------+     +--------------+
//!                \           /
//!                 \         /
//!                +------------+
//!                |CLIF block 3|
//!                +------------+
//! ```
//!
//! Each `LoweredBlock` names just an original CLIF block, or just an edge block.
//!
//! To compute this lowering, we do a DFS over the CLIF-plus-edge-block graph
//! (never actually materialized, just defined by a "successors" function), and
//! compute the reverse postorder.
//!
//! Reference: ~/learning/wasmtime/cranelift/codegen/src/machinst/blockorder.rs

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import machinst types
const inst_mod = @import("inst.zig");
pub const BlockIndex = inst_mod.BlockIndex;

// =============================================================================
// CLIF IR Imports
// Import real CLIF types from compiler/ir/clif/
// Follows Cranelift pattern: use crate::ir::{Block, Function, Inst, Opcode};
// =============================================================================

const clif = @import("../../../ir/clif/mod.zig");

// Entity types
pub const Block = clif.Block;
pub const Inst = clif.Inst;
pub const Value = clif.Value;

// Opcode
pub const Opcode = clif.Opcode;

// Data structures
pub const InstData = clif.InstData;
pub const DataFlowGraph = clif.DataFlowGraph;
pub const Layout = clif.Layout;
pub const Function = clif.Function;

// Block iterator (from layout)
pub const BlockIterator = clif.BlockIterator;

// =============================================================================
// SecondaryMap - entity map indexed by Block/Inst
// Port of cranelift/entity/src/secondary.rs
// =============================================================================

/// A mapping from entity references to values.
/// Unlike PrimaryMap, SecondaryMap can have holes (unmapped entities).
pub fn SecondaryMap(comptime K: type, comptime V: type) type {
    return struct {
        const Self = @This();

        /// The underlying storage.
        data: std.ArrayListUnmanaged(V),
        /// Default value for unmapped entries.
        default: V,

        /// Create a new secondary map with the given default value.
        pub fn initWithDefault(default: V) Self {
            return .{
                .data = .{},
                .default = default,
            };
        }

        /// Create with allocator capacity.
        pub fn initWithCapacity(allocator: Allocator, default: V, capacity: usize) !Self {
            var data = std.ArrayListUnmanaged(V){};
            try data.ensureTotalCapacity(allocator, capacity);
            return .{
                .data = data,
                .default = default,
            };
        }

        /// Free resources.
        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.data.deinit(allocator);
        }

        /// Clear all entries.
        pub fn clear(self: *Self) void {
            self.data.clearRetainingCapacity();
        }

        /// Resize to accommodate key.
        pub fn resize(self: *Self, allocator: Allocator, size: usize) !void {
            if (self.data.items.len < size) {
                const old_len = self.data.items.len;
                try self.data.resize(allocator, size);
                for (self.data.items[old_len..]) |*item| {
                    item.* = self.default;
                }
            }
        }

        /// Get value for key, returning default if not present.
        pub fn get(self: *const Self, key: K) V {
            const idx = keyToIndex(key);
            if (idx < self.data.items.len) {
                return self.data.items[idx];
            }
            return self.default;
        }

        /// Get mutable pointer to value, resizing if needed.
        pub fn getPtr(self: *Self, allocator: Allocator, key: K) !*V {
            const idx = keyToIndex(key);
            if (idx >= self.data.items.len) {
                try self.resize(allocator, idx + 1);
            }
            return &self.data.items[idx];
        }

        /// Set value for key.
        pub fn set(self: *Self, allocator: Allocator, key: K, value: V) !void {
            const ptr = try self.getPtr(allocator, key);
            ptr.* = value;
        }

        fn keyToIndex(key: K) usize {
            if (@hasField(K, "index")) {
                return key.index;
            } else if (@hasDecl(K, "asU32")) {
                return key.asU32();
            } else {
                @compileError("Key type must have index field or asU32 method");
            }
        }
    };
}

// =============================================================================
// ControlFlowGraph
// Port of cranelift/codegen/src/flowgraph.rs
// =============================================================================

/// A predecessor of a block.
pub const BlockPredecessor = struct {
    /// The predecessor block.
    block: Block,
    /// The branch instruction.
    inst: Inst,
};

/// Control flow graph for a function.
pub const ControlFlowGraph = struct {
    const Self = @This();

    allocator: Allocator,
    /// Predecessors for each block.
    predecessors: std.AutoHashMapUnmanaged(Block, std.ArrayListUnmanaged(BlockPredecessor)),
    /// Successors for each block.
    successors: std.AutoHashMapUnmanaged(Block, std.ArrayListUnmanaged(Block)),
    valid: bool,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .predecessors = .{},
            .successors = .{},
            .valid = false,
        };
    }

    pub fn deinit(self: *Self) void {
        var pred_iter = self.predecessors.valueIterator();
        while (pred_iter.next()) |list| {
            list.deinit(self.allocator);
        }
        self.predecessors.deinit(self.allocator);

        var succ_iter = self.successors.valueIterator();
        while (succ_iter.next()) |list| {
            list.deinit(self.allocator);
        }
        self.successors.deinit(self.allocator);
    }

    pub fn clear(self: *Self) void {
        var pred_iter = self.predecessors.valueIterator();
        while (pred_iter.next()) |list| {
            list.clearRetainingCapacity();
        }

        var succ_iter = self.successors.valueIterator();
        while (succ_iter.next()) |list| {
            list.clearRetainingCapacity();
        }

        self.valid = false;
    }

    /// Compute the CFG for a function.
    pub fn compute(self: *Self, func: *const Function) !void {
        self.clear();

        var block_iter = func.layout.blocks();
        while (block_iter.next()) |block| {
            try self.computeBlock(func, block);
        }

        self.valid = true;
    }

    fn computeBlock(self: *Self, func: *const Function, block: Block) !void {
        // Visit successors of this block
        var visitor = struct {
            cfg: *Self,
            from_block: Block,

            pub fn call(ctx: *@This(), from_inst: Inst, succ: Block, _: bool) !void {
                try ctx.cfg.addEdge(ctx.from_block, from_inst, succ);
            }
        }{ .cfg = self, .from_block = block };
        try visitBlockSuccs(func, block, &visitor);
    }

    fn addEdge(self: *Self, from: Block, from_inst: Inst, to: Block) !void {
        // Add successor
        const succ_result = try self.successors.getOrPut(self.allocator, from);
        if (!succ_result.found_existing) {
            succ_result.value_ptr.* = .{};
        }
        try succ_result.value_ptr.append(self.allocator, to);

        // Add predecessor
        const pred_result = try self.predecessors.getOrPut(self.allocator, to);
        if (!pred_result.found_existing) {
            pred_result.value_ptr.* = .{};
        }
        try pred_result.value_ptr.append(self.allocator, .{ .block = from, .inst = from_inst });
    }

    /// Get predecessors of a block.
    pub fn getPredecessors(self: *const Self, block: Block) []const BlockPredecessor {
        if (self.predecessors.get(block)) |list| {
            return list.items;
        }
        return &.{};
    }

    /// Get successors of a block.
    pub fn getSuccessors(self: *const Self, block: Block) []const Block {
        if (self.successors.get(block)) |list| {
            return list.items;
        }
        return &.{};
    }

    pub fn isValid(self: *const Self) bool {
        return self.valid;
    }
};

// =============================================================================
// visit_block_succs
// Port of cranelift/codegen/src/inst_predicates.rs visit_block_succs
// =============================================================================

/// Visit all successor blocks of a block.
/// The callback receives (inst, successor_block, from_table).
/// `from_table` is true if the successor comes from a br_table.
pub fn visitBlockSuccs(
    func: *const Function,
    block: Block,
    context: anytype,
) !void {
    const last_inst = func.layout.lastInst(block) orelse return;

    // Get the instruction data
    const inst_data = func.dfg.getInstData(last_inst);
    const opcode = inst_data.opcode;

    switch (opcode) {
        .jump => {
            // Jump has one destination
            if (inst_data.getBlockDest()) |dest| {
                try context.call(last_inst, dest, false);
            }
        },
        .brif => {
            // Conditional branch has two destinations
            if (inst_data.getBrifDests()) |dests| {
                try context.call(last_inst, dests.then_dest, false);
                try context.call(last_inst, dests.else_dest, false);
            }
        },
        .br_table => {
            // Branch table has default + table entries
            if (inst_data.getBrTableData()) |table_data| {
                // Default destination (not from table)
                try context.call(last_inst, table_data.default, false);
                // Table entries (from table)
                for (table_data.targets) |target| {
                    try context.call(last_inst, target, true);
                }
            }
        },
        .@"return", .trap => {
            // No successors for return/trap
        },
        else => {
            // For other terminators, check if they have destinations
            if (inst_data.getBlockDest()) |dest| {
                try context.call(last_inst, dest, false);
            }
        },
    }
}

// =============================================================================
// DominatorTree
// Simplified port of cranelift/codegen/src/dominator_tree.rs
// We only need cfg_rpo() for blockorder, so this is a minimal implementation.
// =============================================================================

/// Dominator tree computed from a control flow graph.
pub const DominatorTree = struct {
    const Self = @This();

    allocator: Allocator,
    /// Blocks in post-order (computed via DFS).
    postorder: std.ArrayListUnmanaged(Block),
    /// Immediate dominator for each block.
    idom: std.AutoHashMapUnmanaged(Block, ?Block),
    valid: bool,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .postorder = .{},
            .idom = .{},
            .valid = false,
        };
    }

    pub fn deinit(self: *Self) void {
        self.postorder.deinit(self.allocator);
        self.idom.deinit(self.allocator);
    }

    /// Compute the dominator tree for a function.
    pub fn compute(self: *Self, func: *const Function, cfg: *const ControlFlowGraph) !void {
        _ = cfg;
        self.postorder.clearRetainingCapacity();
        self.idom.clearRetainingCapacity();

        const entry = func.layout.entryBlock() orelse return;

        // Compute post-order via DFS
        var visited = std.AutoHashMapUnmanaged(Block, void){};
        defer visited.deinit(self.allocator);

        var stack = std.ArrayListUnmanaged(struct { block: Block, state: enum { enter, exit } }){};
        defer stack.deinit(self.allocator);

        try stack.append(self.allocator, .{ .block = entry, .state = .enter });

        while (stack.pop()) |item| {
            switch (item.state) {
                .enter => {
                    if (visited.contains(item.block)) continue;
                    try visited.put(self.allocator, item.block, {});

                    // Push exit state
                    try stack.append(self.allocator, .{ .block = item.block, .state = .exit });

                    // Push successors
                    var succ_visitor = struct {
                        stk: *@TypeOf(stack),
                        alloc: Allocator,

                        pub fn call(ctx: *@This(), _: Inst, succ: Block, _: bool) !void {
                            try ctx.stk.append(ctx.alloc, .{ .block = succ, .state = .enter });
                        }
                    }{ .stk = &stack, .alloc = self.allocator };
                    try visitBlockSuccs(func, item.block, &succ_visitor);
                },
                .exit => {
                    try self.postorder.append(self.allocator, item.block);
                },
            }
        }

        self.valid = true;
    }

    /// Get blocks in post-order.
    pub fn cfgPostorder(self: *const Self) []const Block {
        std.debug.assert(self.valid);
        return self.postorder.items;
    }

    /// Get an iterator over blocks in reverse post-order.
    pub fn cfgRpo(self: *const Self) RpoIterator {
        std.debug.assert(self.valid);
        return .{ .postorder = self.postorder.items, .index = self.postorder.items.len };
    }

    pub const RpoIterator = struct {
        postorder: []const Block,
        index: usize,

        pub fn next(self: *RpoIterator) ?Block {
            if (self.index == 0) return null;
            self.index -= 1;
            return self.postorder[self.index];
        }
    };

    pub fn isValid(self: *const Self) bool {
        return self.valid;
    }
};

// =============================================================================
// LoweredBlock
// Port of cranelift/codegen/src/machinst/blockorder.rs LoweredBlock
// =============================================================================

/// A lowered block represents either an original CLIF block or a
/// critical edge between two CLIF blocks.
pub const LoweredBlock = union(enum) {
    /// Block in original CLIF.
    orig: struct {
        block: Block,
    },

    /// Critical edge between two CLIF blocks.
    critical_edge: struct {
        /// The predecessor block.
        pred: Block,
        /// The successor block.
        succ: Block,
        /// The index of this branch in the successor edges from `pred`.
        succ_idx: u32,
    },

    const Self = @This();

    /// Unwrap an `Orig` block.
    pub fn origBlock(self: Self) ?Block {
        return switch (self) {
            .orig => |o| o.block,
            .critical_edge => null,
        };
    }

    /// The associated in-edge predecessor, if this is a critical edge.
    pub fn inEdge(self: Self) ?Block {
        return switch (self) {
            .critical_edge => |e| e.pred,
            .orig => null,
        };
    }

    /// The associated out-edge successor, if this is a critical edge.
    pub fn outEdge(self: Self) ?Block {
        return switch (self) {
            .critical_edge => |e| e.succ,
            .orig => null,
        };
    }

    pub fn eql(self: Self, other: Self) bool {
        return switch (self) {
            .orig => |o| switch (other) {
                .orig => |o2| o.block.eql(o2.block),
                .critical_edge => false,
            },
            .critical_edge => |e| switch (other) {
                .orig => false,
                .critical_edge => |e2| e.pred.eql(e2.pred) and e.succ.eql(e2.succ) and e.succ_idx == e2.succ_idx,
            },
        };
    }

    pub fn hash(self: Self) u64 {
        var hasher = std.hash.Wyhash.init(0);
        switch (self) {
            .orig => |o| {
                hasher.update(&[_]u8{0});
                hasher.update(std.mem.asBytes(&o.block.index));
            },
            .critical_edge => |e| {
                hasher.update(&[_]u8{1});
                hasher.update(std.mem.asBytes(&e.pred.index));
                hasher.update(std.mem.asBytes(&e.succ.index));
                hasher.update(std.mem.asBytes(&e.succ_idx));
            },
        }
        return hasher.final();
    }
};

/// Hash context for LoweredBlock in hash maps.
pub const LoweredBlockContext = struct {
    pub fn hash(_: LoweredBlockContext, lb: LoweredBlock) u64 {
        return lb.hash();
    }

    pub fn eql(_: LoweredBlockContext, a: LoweredBlock, b: LoweredBlock) bool {
        return a.eql(b);
    }
};

// =============================================================================
// BlockLoweringOrder
// Port of cranelift/codegen/src/machinst/blockorder.rs BlockLoweringOrder
// =============================================================================

/// Mapping from CLIF BBs to VCode BBs.
pub const BlockLoweringOrder = struct {
    const Self = @This();

    allocator: Allocator,

    /// Lowered blocks, in BlockIndex order.
    lowered_order: std.ArrayListUnmanaged(LoweredBlock),

    /// BlockIndex values for successors for all lowered blocks.
    lowered_succ_indices: std.ArrayListUnmanaged(BlockIndex),

    /// Ranges in `lowered_succ_indices` giving the successor lists for each lowered block.
    /// Also stores the optional terminator instruction.
    lowered_succ_ranges: std.ArrayListUnmanaged(SuccRange),

    /// BlockIndex for each original Block.
    blockindex_by_block: SecondaryMap(Block, BlockIndex),

    /// Cold blocks.
    cold_blocks: std.AutoHashMapUnmanaged(BlockIndex, void),

    /// Indirect branch targets.
    indirect_branch_targets: std.AutoHashMapUnmanaged(BlockIndex, void),

    const SuccRange = struct {
        opt_inst: ?Inst,
        start: usize,
        end: usize,
    };

    /// Compute and return a lowered block order for a function.
    pub fn init(
        allocator: Allocator,
        func: *const Function,
        domtree: *const DominatorTree,
    ) !Self {
        var self = Self{
            .allocator = allocator,
            .lowered_order = .{},
            .lowered_succ_indices = .{},
            .lowered_succ_ranges = .{},
            .blockindex_by_block = SecondaryMap(Block, BlockIndex).initWithDefault(BlockIndex.invalid()),
            .cold_blocks = .{},
            .indirect_branch_targets = .{},
        };
        errdefer self.deinit();

        // Step 1: compute the in-edge and out-edge count of every block.
        var block_in_count = SecondaryMap(Block, u32).initWithDefault(0);
        defer block_in_count.deinit(allocator);

        var block_out_count = SecondaryMap(Block, u32).initWithDefault(0);
        defer block_out_count.deinit(allocator);

        // Block successors stored as LoweredBlocks
        var block_succs = std.ArrayListUnmanaged(LoweredBlock){};
        defer block_succs.deinit(allocator);

        var block_succ_range = SecondaryMap(Block, struct { start: usize, end: usize }).initWithDefault(.{ .start = 0, .end = 0 });
        defer block_succ_range.deinit(allocator);

        var indirect_branch_target_clif_blocks = std.AutoHashMapUnmanaged(Block, void){};
        defer indirect_branch_target_clif_blocks.deinit(allocator);

        // Iterate over all blocks
        var block_iter = func.layout.blocks();
        while (block_iter.next()) |block| {
            const start = block_succs.items.len;

            var edge_visitor = struct {
                alloc: Allocator,
                succs: *@TypeOf(block_succs),
                out_count: *SecondaryMap(Block, u32),
                in_count: *SecondaryMap(Block, u32),
                indirect_targets: *@TypeOf(indirect_branch_target_clif_blocks),
                from_block: Block,

                pub fn call(ctx: *@This(), _: Inst, succ: Block, from_table: bool) !void {
                    const out_ptr = try ctx.out_count.getPtr(ctx.alloc, ctx.from_block);
                    out_ptr.* += 1;

                    const in_ptr = try ctx.in_count.getPtr(ctx.alloc, succ);
                    in_ptr.* += 1;

                    try ctx.succs.append(ctx.alloc, .{ .orig = .{ .block = succ } });

                    if (from_table) {
                        try ctx.indirect_targets.put(ctx.alloc, succ, {});
                    }
                }
            }{
                .alloc = allocator,
                .succs = &block_succs,
                .out_count = &block_out_count,
                .in_count = &block_in_count,
                .indirect_targets = &indirect_branch_target_clif_blocks,
                .from_block = block,
            };
            try visitBlockSuccs(func, block, &edge_visitor);

            // Handle br_table with empty table - still treat as conditional
            if (func.layout.lastInst(block)) |last_inst| {
                const inst_data = func.dfg.getInstData(last_inst);
                switch (inst_data.opcode) {
                    .br_table => {
                        const out_ptr = try block_out_count.getPtr(allocator, block);
                        out_ptr.* = @max(out_ptr.*, 2);
                    },
                    else => {},
                }
            }

            const end = block_succs.items.len;
            try block_succ_range.set(allocator, block, .{ .start = start, .end = end });
        }

        // Step 2: walk the postorder from the domtree in reverse to produce lowering order
        var rpo_iter = domtree.cfgRpo();
        while (rpo_iter.next()) |block| {
            const idx = BlockIndex.init(@intCast(self.lowered_order.items.len));
            try self.lowered_order.append(allocator, .{ .orig = .{ .block = block } });
            try self.blockindex_by_block.set(allocator, block, idx);

            // Check for critical edges
            if (block_out_count.get(block) > 1) {
                const range = block_succ_range.get(block);

                var succ_idx: u32 = 0;
                for (block_succs.items[range.start..range.end]) |*lb| {
                    const succ = lb.origBlock().?;
                    if (block_in_count.get(succ) > 1) {
                        // Critical edge - mutate to CriticalEdge
                        lb.* = .{
                            .critical_edge = .{
                                .pred = block,
                                .succ = succ,
                                .succ_idx = succ_idx,
                            },
                        };
                        try self.lowered_order.append(allocator, lb.*);
                    }
                    succ_idx += 1;
                }
            }
        }

        // Build lb_to_bindex map
        var lb_to_bindex = std.HashMapUnmanaged(LoweredBlock, BlockIndex, LoweredBlockContext, std.hash_map.default_max_load_percentage){};
        defer lb_to_bindex.deinit(allocator);

        for (self.lowered_order.items, 0..) |lb, i| {
            try lb_to_bindex.put(allocator, lb, BlockIndex.init(@intCast(i)));
        }

        // Step 3: build the successor tables
        for (self.lowered_order.items, 0..) |lb, ix| {
            const bindex = BlockIndex.init(@intCast(ix));
            const start = self.lowered_succ_indices.items.len;
            var opt_inst: ?Inst = null;

            switch (lb) {
                .orig => |o| {
                    const range = block_succ_range.get(o.block);
                    for (block_succs.items[range.start..range.end]) |succ_lb| {
                        if (lb_to_bindex.get(succ_lb)) |succ_idx| {
                            try self.lowered_succ_indices.append(allocator, succ_idx);
                        }
                    }

                    if (func.layout.isBlockCold(o.block)) {
                        try self.cold_blocks.put(allocator, bindex, {});
                    }

                    if (indirect_branch_target_clif_blocks.contains(o.block)) {
                        try self.indirect_branch_targets.put(allocator, bindex, {});
                    }

                    if (func.layout.lastInst(o.block)) |last| {
                        const inst_data = func.dfg.getInstData(last);
                        if (inst_data.opcode.isTerminator() and inst_data.opcode.isBranch()) {
                            opt_inst = last;
                        }
                    }
                },
                .critical_edge => |e| {
                    // Critical edge has single successor
                    const succ_lb = LoweredBlock{ .orig = .{ .block = e.succ } };
                    if (lb_to_bindex.get(succ_lb)) |succ_idx| {
                        try self.lowered_succ_indices.append(allocator, succ_idx);
                    }

                    // Inherit cold and indirect from successor
                    if (func.layout.isBlockCold(e.succ)) {
                        try self.cold_blocks.put(allocator, bindex, {});
                    }

                    if (indirect_branch_target_clif_blocks.contains(e.succ)) {
                        try self.indirect_branch_targets.put(allocator, bindex, {});
                    }
                },
            }

            const end = self.lowered_succ_indices.items.len;
            try self.lowered_succ_ranges.append(allocator, .{
                .opt_inst = opt_inst,
                .start = start,
                .end = end,
            });
        }

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.lowered_order.deinit(self.allocator);
        self.lowered_succ_indices.deinit(self.allocator);
        self.lowered_succ_ranges.deinit(self.allocator);
        self.blockindex_by_block.deinit(self.allocator);
        self.cold_blocks.deinit(self.allocator);
        self.indirect_branch_targets.deinit(self.allocator);
    }

    /// Get the lowered order of blocks.
    pub fn loweredOrder(self: *const Self) []const LoweredBlock {
        return self.lowered_order.items;
    }

    /// Get the BlockIndex, if any, for a given Block.
    /// Returns null if the block is unreachable.
    pub fn loweredIndexForBlock(self: *const Self, block: Block) ?BlockIndex {
        const idx = self.blockindex_by_block.get(block);
        if (idx.isValid()) {
            return idx;
        }
        return null;
    }

    /// Get the successor indices for a lowered block.
    pub fn succIndices(self: *const Self, block: BlockIndex) struct { opt_inst: ?Inst, succs: []const BlockIndex } {
        const range = self.lowered_succ_ranges.items[block.index()];
        return .{
            .opt_inst = range.opt_inst,
            .succs = self.lowered_succ_indices.items[range.start..range.end],
        };
    }

    /// Determine whether the given lowered-block index is cold.
    pub fn isCold(self: *const Self, block: BlockIndex) bool {
        return self.cold_blocks.contains(block);
    }

    /// Determine whether the given lowered block index is an indirect branch target.
    pub fn isIndirectBranchTarget(self: *const Self, block: BlockIndex) bool {
        return self.indirect_branch_targets.contains(block);
    }

    /// Get the number of lowered blocks.
    pub fn numBlocks(self: *const Self) usize {
        return self.lowered_order.items.len;
    }
};

// =============================================================================
// Tests
// =============================================================================

test "SecondaryMap basic operations" {
    const allocator = std.testing.allocator;

    var map = SecondaryMap(Block, u32).initWithDefault(0);
    defer map.deinit(allocator);

    const block0 = Block.fromIndex(0);
    const block1 = Block.fromIndex(1);
    const block5 = Block.fromIndex(5);

    // Default value
    try std.testing.expectEqual(@as(u32, 0), map.get(block0));

    // Set and get
    try map.set(allocator, block0, 42);
    try std.testing.expectEqual(@as(u32, 42), map.get(block0));

    // Sparse access
    try map.set(allocator, block5, 100);
    try std.testing.expectEqual(@as(u32, 100), map.get(block5));
    try std.testing.expectEqual(@as(u32, 0), map.get(block1)); // Still default
}

test "LoweredBlock equality and hashing" {
    const orig1 = LoweredBlock{ .orig = .{ .block = Block.fromIndex(0) } };
    const orig2 = LoweredBlock{ .orig = .{ .block = Block.fromIndex(0) } };
    const orig3 = LoweredBlock{ .orig = .{ .block = Block.fromIndex(1) } };

    try std.testing.expect(orig1.eql(orig2));
    try std.testing.expect(!orig1.eql(orig3));

    const edge1 = LoweredBlock{ .critical_edge = .{
        .pred = Block.fromIndex(0),
        .succ = Block.fromIndex(1),
        .succ_idx = 0,
    } };
    const edge2 = LoweredBlock{ .critical_edge = .{
        .pred = Block.fromIndex(0),
        .succ = Block.fromIndex(1),
        .succ_idx = 0,
    } };

    try std.testing.expect(edge1.eql(edge2));
    try std.testing.expect(!orig1.eql(edge1));
}

test "LoweredBlock orig_block and edges" {
    const orig = LoweredBlock{ .orig = .{ .block = Block.fromIndex(5) } };
    try std.testing.expectEqual(Block.fromIndex(5), orig.origBlock().?);
    try std.testing.expectEqual(@as(?Block, null), orig.inEdge());
    try std.testing.expectEqual(@as(?Block, null), orig.outEdge());

    const edge = LoweredBlock{ .critical_edge = .{
        .pred = Block.fromIndex(2),
        .succ = Block.fromIndex(3),
        .succ_idx = 1,
    } };
    try std.testing.expectEqual(@as(?Block, null), edge.origBlock());
    try std.testing.expectEqual(Block.fromIndex(2), edge.inEdge().?);
    try std.testing.expectEqual(Block.fromIndex(3), edge.outEdge().?);
}

test "BlockIndex operations" {
    const idx0 = BlockIndex.init(0);
    const idx1 = BlockIndex.init(1);
    const invalid = BlockIndex.invalid();

    try std.testing.expect(idx0.isValid());
    try std.testing.expect(idx1.isValid());
    try std.testing.expect(!invalid.isValid());

    try std.testing.expectEqual(@as(usize, 0), idx0.index());
    try std.testing.expectEqual(@as(usize, 1), idx1.index());
}

test "ControlFlowGraph initialization" {
    const allocator = std.testing.allocator;

    var cfg = ControlFlowGraph.init(allocator);
    defer cfg.deinit();

    try std.testing.expect(!cfg.isValid());
}

test "DominatorTree initialization" {
    const allocator = std.testing.allocator;

    var domtree = DominatorTree.init(allocator);
    defer domtree.deinit();

    try std.testing.expect(!domtree.isValid());
}

test "SecondaryMap resize" {
    const allocator = std.testing.allocator;

    var map = SecondaryMap(Block, u32).initWithDefault(99);
    defer map.deinit(allocator);

    try map.resize(allocator, 10);

    // All entries should have default value
    for (0..10) |i| {
        try std.testing.expectEqual(@as(u32, 99), map.get(Block.fromIndex(@intCast(i))));
    }
}

test "LoweredBlock hash consistency" {
    const orig = LoweredBlock{ .orig = .{ .block = Block.fromIndex(42) } };

    // Same value should produce same hash
    const hash1 = orig.hash();
    const hash2 = orig.hash();
    try std.testing.expectEqual(hash1, hash2);

    // Different values should (likely) produce different hashes
    const other = LoweredBlock{ .orig = .{ .block = Block.fromIndex(43) } };
    const hash3 = other.hash();
    try std.testing.expect(hash1 != hash3);
}

test "DominatorTree RPO iterator" {
    const allocator = std.testing.allocator;

    // Create a simple function with blocks 0 -> 1 -> 2 using proper CLIF APIs
    var func = Function.init(allocator);
    defer func.deinit();

    // Create blocks
    const block0 = try func.dfg.makeBlock();
    const block1 = try func.dfg.makeBlock();
    const block2 = try func.dfg.makeBlock();

    // Add blocks to layout
    try func.layout.appendBlock(allocator, block0);
    try func.layout.appendBlock(allocator, block1);
    try func.layout.appendBlock(allocator, block2);

    // Block 0 jumps to block 1
    const inst0 = try func.dfg.makeInstWithData(.{
        .opcode = .jump,
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
        .dest = block1,
    });
    try func.layout.appendInst(allocator, inst0, block0);

    // Block 1 jumps to block 2
    const inst1 = try func.dfg.makeInstWithData(.{
        .opcode = .jump,
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
        .dest = block2,
    });
    try func.layout.appendInst(allocator, inst1, block1);

    // Block 2 returns
    const inst2 = try func.dfg.makeInstWithData(.{
        .opcode = .@"return",
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
    });
    try func.layout.appendInst(allocator, inst2, block2);

    // Compute CFG and domtree
    var cfg = ControlFlowGraph.init(allocator);
    defer cfg.deinit();
    try cfg.compute(&func);

    var domtree = DominatorTree.init(allocator);
    defer domtree.deinit();
    try domtree.compute(&func, &cfg);

    // Check RPO order
    var rpo_iter = domtree.cfgRpo();
    const first = rpo_iter.next();
    try std.testing.expect(first != null);
    try std.testing.expectEqual(block0, first.?);

    const second = rpo_iter.next();
    try std.testing.expect(second != null);
    try std.testing.expectEqual(block1, second.?);

    const third = rpo_iter.next();
    try std.testing.expect(third != null);
    try std.testing.expectEqual(block2, third.?);

    try std.testing.expectEqual(@as(?Block, null), rpo_iter.next());
}

test "BlockLoweringOrder simple linear CFG" {
    const allocator = std.testing.allocator;

    // Create function: block0 -> block1 -> block2 (return) using proper CLIF APIs
    var func = Function.init(allocator);
    defer func.deinit();

    // Create blocks
    const block0 = try func.dfg.makeBlock();
    const block1 = try func.dfg.makeBlock();
    const block2 = try func.dfg.makeBlock();

    // Add blocks to layout
    try func.layout.appendBlock(allocator, block0);
    try func.layout.appendBlock(allocator, block1);
    try func.layout.appendBlock(allocator, block2);

    // Block 0 jumps to block 1
    const inst0 = try func.dfg.makeInstWithData(.{
        .opcode = .jump,
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
        .dest = block1,
    });
    try func.layout.appendInst(allocator, inst0, block0);

    // Block 1 jumps to block 2
    const inst1 = try func.dfg.makeInstWithData(.{
        .opcode = .jump,
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
        .dest = block2,
    });
    try func.layout.appendInst(allocator, inst1, block1);

    // Block 2 returns
    const inst2 = try func.dfg.makeInstWithData(.{
        .opcode = .@"return",
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
    });
    try func.layout.appendInst(allocator, inst2, block2);

    // Compute domtree
    var cfg = ControlFlowGraph.init(allocator);
    defer cfg.deinit();
    try cfg.compute(&func);

    var domtree = DominatorTree.init(allocator);
    defer domtree.deinit();
    try domtree.compute(&func, &cfg);

    // Compute block lowering order
    var order = try BlockLoweringOrder.init(allocator, &func, &domtree);
    defer order.deinit();

    // Linear CFG has no critical edges, so 3 blocks
    try std.testing.expectEqual(@as(usize, 3), order.numBlocks());

    // All should be original blocks
    const lowered = order.loweredOrder();
    try std.testing.expectEqual(block0, lowered[0].origBlock().?);
    try std.testing.expectEqual(block1, lowered[1].origBlock().?);
    try std.testing.expectEqual(block2, lowered[2].origBlock().?);
}

test "BlockLoweringOrder diamond CFG no critical edges" {
    const allocator = std.testing.allocator;

    // Diamond: block0 -> block1, block0 -> block2, block1 -> block3, block2 -> block3
    // No critical edges because each predecessor has only one path to successors
    var func = Function.init(allocator);
    defer func.deinit();

    // Create blocks
    const block0 = try func.dfg.makeBlock();
    const block1 = try func.dfg.makeBlock();
    const block2 = try func.dfg.makeBlock();
    const block3 = try func.dfg.makeBlock();

    // Add blocks to layout
    try func.layout.appendBlock(allocator, block0);
    try func.layout.appendBlock(allocator, block1);
    try func.layout.appendBlock(allocator, block2);
    try func.layout.appendBlock(allocator, block3);

    // Block 0: brif -> block1, block2
    const inst0 = try func.dfg.makeInstWithData(.{
        .opcode = .brif,
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
        .then_dest = block1,
        .else_dest = block2,
    });
    try func.layout.appendInst(allocator, inst0, block0);

    // Block 1: jump -> block3
    const inst1 = try func.dfg.makeInstWithData(.{
        .opcode = .jump,
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
        .dest = block3,
    });
    try func.layout.appendInst(allocator, inst1, block1);

    // Block 2: jump -> block3
    const inst2 = try func.dfg.makeInstWithData(.{
        .opcode = .jump,
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
        .dest = block3,
    });
    try func.layout.appendInst(allocator, inst2, block2);

    // Block 3: return
    const inst3 = try func.dfg.makeInstWithData(.{
        .opcode = .@"return",
        .args = clif.ValueList.EMPTY,
        .ctrl_type = clif.Type.INVALID,
    });
    try func.layout.appendInst(allocator, inst3, block3);

    var cfg = ControlFlowGraph.init(allocator);
    defer cfg.deinit();
    try cfg.compute(&func);

    var domtree = DominatorTree.init(allocator);
    defer domtree.deinit();
    try domtree.compute(&func, &cfg);

    var order = try BlockLoweringOrder.init(allocator, &func, &domtree);
    defer order.deinit();

    // Diamond with no critical edges should have 4 blocks
    // (critical edges only occur when pred has >1 out-edge AND succ has >1 in-edge)
    try std.testing.expectEqual(@as(usize, 4), order.numBlocks());
}
