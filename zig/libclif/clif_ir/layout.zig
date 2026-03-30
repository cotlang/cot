//! Block and instruction layout.
//!
//! Port of cranelift/codegen/src/ir/layout.rs
//!
//! The `Layout` struct determines the layout of blocks and instructions in a function.
//! It does not contain definitions of instructions or blocks, but depends on `Inst`
//! and `Block` entity references being defined elsewhere.
//!
//! Both blocks and instructions are ordered independently with doubly-linked lists.

const std = @import("std");
const dfg = @import("dfg.zig");

pub const Block = dfg.Block;
pub const Inst = dfg.Inst;

// ============================================================================
// Sequence Numbers
// Port of layout.rs SequenceNumber
// ============================================================================

/// A sequence number assigned to an instruction or block.
/// Sequence numbers are used for fast ordering comparisons in `pp_cmp`.
pub const SequenceNumber = u32;

/// The initial stride used for assigning sequence numbers.
const MAJOR_STRIDE: SequenceNumber = 10;

/// The minimum stride - when below this, renumbering is required.
const MINOR_STRIDE: SequenceNumber = 2;

/// Compute midpoint between a and b. Returns null if no space.
fn midpoint(a: SequenceNumber, b: SequenceNumber) ?SequenceNumber {
    if (b < a) return null;
    const diff = b - a;
    if (diff < 2) return null;
    return a + diff / 2;
}

// ============================================================================
// BlockNode
// Port of layout.rs BlockNode
// ============================================================================

/// Data associated with a block in the layout.
const BlockNode = struct {
    /// Previous block in layout, or null if this is the first.
    prev: ?Block = null,
    /// Next block in layout, or null if this is the last.
    next: ?Block = null,
    /// First instruction in block, or null if empty.
    first_inst: ?Inst = null,
    /// Last instruction in block, or null if empty.
    last_inst: ?Inst = null,
    /// Is this a cold block?
    cold: bool = false,
    /// Sequence number for ordering.
    seq: SequenceNumber = 0,

    /// Check if this node is inserted in the layout.
    pub fn isInserted(self: BlockNode) bool {
        // A block is inserted if it has non-zero sequence number OR
        // it has a prev/next pointer. We use seq != 0 as the primary check.
        // However, the first block has seq=MAJOR_STRIDE.
        return self.seq != 0 or self.prev != null or self.next != null;
    }
};

// ============================================================================
// InstNode
// Port of layout.rs InstNode
// ============================================================================

/// Data associated with an instruction in the layout.
const InstNode = struct {
    /// Block containing this instruction, or null if not inserted.
    block: ?Block = null,
    /// Previous instruction in block, or null if this is the first.
    prev: ?Inst = null,
    /// Next instruction in block, or null if this is the last.
    next: ?Inst = null,
    /// Sequence number for ordering.
    seq: SequenceNumber = 0,
};

// ============================================================================
// Layout
// Port of layout.rs Layout
// ============================================================================

/// The layout of blocks and instructions in a function.
///
/// The function body consists of an ordered sequence of blocks. Each block
/// contains an ordered sequence of instructions. The ordering is determined
/// by this layout struct, not by instruction or block numbers.
pub const Layout = struct {
    /// Block data, indexed by Block.
    block_nodes: std.ArrayListUnmanaged(BlockNode) = .{},
    /// Instruction data, indexed by Inst.
    inst_nodes: std.ArrayListUnmanaged(InstNode) = .{},
    /// First block in layout order, or null if empty.
    first_block: ?Block = null,
    /// Last block in layout order, or null if empty.
    last_block: ?Block = null,

    const Self = @This();

    /// Initialize a new empty layout.
    pub fn init() Self {
        return .{};
    }

    /// Deallocate storage.
    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.block_nodes.deinit(allocator);
        self.inst_nodes.deinit(allocator);
    }

    /// Clear all data.
    pub fn clear(self: *Self) void {
        self.block_nodes.clearRetainingCapacity();
        self.inst_nodes.clearRetainingCapacity();
        self.first_block = null;
        self.last_block = null;
    }

    // ------------------------------------------------------------------------
    // Block methods
    // ------------------------------------------------------------------------

    /// Get the first block in the layout, or null if empty.
    pub fn entryBlock(self: Self) ?Block {
        return self.first_block;
    }

    /// Get the last block in the layout, or null if empty.
    pub fn lastBlock(self: Self) ?Block {
        return self.last_block;
    }

    /// Check if a block is inserted in the layout.
    pub fn isBlockInserted(self: Self, block: Block) bool {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) return false;
        // Check if it's the first block or has linkage
        if (self.first_block) |fb| {
            if (fb.index == block.index) return true;
        }
        const node = self.block_nodes.items[idx];
        return node.prev != null or node.next != null or node.seq != 0;
    }

    /// Ensure block storage is large enough.
    fn ensureBlockCapacity(self: *Self, allocator: std.mem.Allocator, block: Block) !void {
        const idx = block.asU32();
        while (self.block_nodes.items.len <= idx) {
            try self.block_nodes.append(allocator, BlockNode{});
        }
    }

    /// Append a block to the end of the layout.
    pub fn appendBlock(self: *Self, allocator: std.mem.Allocator, block: Block) !void {
        try self.ensureBlockCapacity(allocator, block);
        const idx = block.asU32();

        // Assign sequence number
        const seq = if (self.last_block) |last|
            self.block_nodes.items[last.asU32()].seq + MAJOR_STRIDE
        else
            MAJOR_STRIDE;

        self.block_nodes.items[idx] = BlockNode{
            .prev = self.last_block,
            .next = null,
            .first_inst = null,
            .last_inst = null,
            .cold = false,
            .seq = seq,
        };

        // Update last block's next pointer
        if (self.last_block) |last| {
            self.block_nodes.items[last.asU32()].next = block;
        }

        // Update first/last block
        if (self.first_block == null) {
            self.first_block = block;
        }
        self.last_block = block;
    }

    /// Insert a block before another block.
    pub fn insertBlock(self: *Self, allocator: std.mem.Allocator, block: Block, before: Block) !void {
        try self.ensureBlockCapacity(allocator, block);
        const idx = block.asU32();
        const before_idx = before.asU32();

        const before_node = self.block_nodes.items[before_idx];
        const prev = before_node.prev;

        // Compute sequence number
        const seq = if (prev) |p|
            midpoint(self.block_nodes.items[p.asU32()].seq, before_node.seq) orelse blk: {
                // Renumber needed - use stride
                break :blk before_node.seq -| MINOR_STRIDE;
            }
        else
            before_node.seq / 2;

        self.block_nodes.items[idx] = BlockNode{
            .prev = prev,
            .next = before,
            .first_inst = null,
            .last_inst = null,
            .cold = false,
            .seq = seq,
        };

        // Update linkage
        self.block_nodes.items[before_idx].prev = block;
        if (prev) |p| {
            self.block_nodes.items[p.asU32()].next = block;
        } else {
            self.first_block = block;
        }
    }

    /// Insert a block after another block.
    pub fn insertBlockAfter(self: *Self, allocator: std.mem.Allocator, block: Block, after: Block) !void {
        try self.ensureBlockCapacity(allocator, block);
        const idx = block.asU32();
        const after_idx = after.asU32();

        const after_node = self.block_nodes.items[after_idx];
        const next = after_node.next;

        // Compute sequence number
        const seq = if (next) |n|
            midpoint(after_node.seq, self.block_nodes.items[n.asU32()].seq) orelse
                after_node.seq + MINOR_STRIDE
        else
            after_node.seq + MAJOR_STRIDE;

        self.block_nodes.items[idx] = BlockNode{
            .prev = after,
            .next = next,
            .first_inst = null,
            .last_inst = null,
            .cold = false,
            .seq = seq,
        };

        // Update linkage
        self.block_nodes.items[after_idx].next = block;
        if (next) |n| {
            self.block_nodes.items[n.asU32()].prev = block;
        } else {
            self.last_block = block;
        }
    }

    /// Remove a block from the layout. Does not remove instructions.
    pub fn removeBlock(self: *Self, block: Block) void {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) return;

        const node = self.block_nodes.items[idx];
        const prev = node.prev;
        const next = node.next;

        // Update linkage
        if (prev) |p| {
            self.block_nodes.items[p.asU32()].next = next;
        } else {
            self.first_block = next;
        }

        if (next) |n| {
            self.block_nodes.items[n.asU32()].prev = prev;
        } else {
            self.last_block = prev;
        }

        // Clear the node
        self.block_nodes.items[idx] = BlockNode{};
    }

    /// Get next block in layout order.
    pub fn nextBlock(self: Self, block: Block) ?Block {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) return null;
        return self.block_nodes.items[idx].next;
    }

    /// Get previous block in layout order.
    pub fn prevBlock(self: Self, block: Block) ?Block {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) return null;
        return self.block_nodes.items[idx].prev;
    }

    /// Iterator over blocks in layout order.
    pub fn blocks(self: *const Self) BlockIterator {
        return BlockIterator{
            .layout = self,
            .head = self.first_block,
            .tail = self.last_block,
        };
    }

    // ------------------------------------------------------------------------
    // Instruction methods
    // ------------------------------------------------------------------------

    /// Ensure instruction storage is large enough.
    fn ensureInstCapacity(self: *Self, allocator: std.mem.Allocator, inst: Inst) !void {
        const idx = inst.asU32();
        while (self.inst_nodes.items.len <= idx) {
            try self.inst_nodes.append(allocator, InstNode{});
        }
    }

    /// Check if an instruction is inserted in the layout.
    pub fn isInstInserted(self: Self, inst: Inst) bool {
        const idx = inst.asU32();
        if (idx >= self.inst_nodes.items.len) return false;
        return self.inst_nodes.items[idx].block != null;
    }

    /// Get the block containing an instruction.
    pub fn instBlock(self: Self, inst: Inst) ?Block {
        const idx = inst.asU32();
        if (idx >= self.inst_nodes.items.len) return null;
        return self.inst_nodes.items[idx].block;
    }

    /// Append an instruction to the end of a block.
    pub fn appendInst(self: *Self, allocator: std.mem.Allocator, inst: Inst, block: Block) !void {
        try self.ensureInstCapacity(allocator, inst);
        const inst_idx = inst.asU32();
        const block_idx = block.asU32();

        const block_node = &self.block_nodes.items[block_idx];
        const last = block_node.last_inst;

        // Compute sequence number
        const seq = if (last) |l|
            self.inst_nodes.items[l.asU32()].seq + MAJOR_STRIDE
        else
            MAJOR_STRIDE;

        self.inst_nodes.items[inst_idx] = InstNode{
            .block = block,
            .prev = last,
            .next = null,
            .seq = seq,
        };

        // Update last instruction's next pointer
        if (last) |l| {
            self.inst_nodes.items[l.asU32()].next = inst;
        }

        // Update block's first/last instruction
        if (block_node.first_inst == null) {
            block_node.first_inst = inst;
        }
        block_node.last_inst = inst;
    }

    /// Insert an instruction before another instruction.
    pub fn insertInst(self: *Self, allocator: std.mem.Allocator, inst: Inst, before: Inst) !void {
        try self.ensureInstCapacity(allocator, inst);
        const inst_idx = inst.asU32();
        const before_idx = before.asU32();

        const before_node = self.inst_nodes.items[before_idx];
        const block = before_node.block.?;
        const prev = before_node.prev;

        // Compute sequence number
        const seq = if (prev) |p|
            midpoint(self.inst_nodes.items[p.asU32()].seq, before_node.seq) orelse
                before_node.seq -| MINOR_STRIDE
        else
            before_node.seq / 2;

        self.inst_nodes.items[inst_idx] = InstNode{
            .block = block,
            .prev = prev,
            .next = before,
            .seq = if (seq == 0) 1 else seq,
        };

        // Update linkage
        self.inst_nodes.items[before_idx].prev = inst;
        if (prev) |p| {
            self.inst_nodes.items[p.asU32()].next = inst;
        } else {
            self.block_nodes.items[block.asU32()].first_inst = inst;
        }
    }

    /// Remove an instruction from the layout.
    pub fn removeInst(self: *Self, inst: Inst) void {
        const idx = inst.asU32();
        if (idx >= self.inst_nodes.items.len) return;

        const node = self.inst_nodes.items[idx];
        const block = node.block orelse return;
        const prev = node.prev;
        const next = node.next;

        // Update linkage
        if (prev) |p| {
            self.inst_nodes.items[p.asU32()].next = next;
        } else {
            self.block_nodes.items[block.asU32()].first_inst = next;
        }

        if (next) |n| {
            self.inst_nodes.items[n.asU32()].prev = prev;
        } else {
            self.block_nodes.items[block.asU32()].last_inst = prev;
        }

        // Clear the node
        self.inst_nodes.items[idx] = InstNode{};
    }

    /// Get next instruction in layout order.
    pub fn nextInst(self: Self, inst: Inst) ?Inst {
        const idx = inst.asU32();
        if (idx >= self.inst_nodes.items.len) return null;
        return self.inst_nodes.items[idx].next;
    }

    /// Get previous instruction in layout order.
    pub fn prevInst(self: Self, inst: Inst) ?Inst {
        const idx = inst.asU32();
        if (idx >= self.inst_nodes.items.len) return null;
        return self.inst_nodes.items[idx].prev;
    }

    /// Get first instruction in a block.
    pub fn firstInst(self: Self, block: Block) ?Inst {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) return null;
        return self.block_nodes.items[idx].first_inst;
    }

    /// Get last instruction in a block.
    pub fn lastInst(self: Self, block: Block) ?Inst {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) return null;
        return self.block_nodes.items[idx].last_inst;
    }

    /// Iterator over instructions in a block.
    pub fn blockInsts(self: *const Self, block: Block) InstIterator {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) {
            return InstIterator{
                .layout = self,
                .head = null,
                .tail = null,
            };
        }
        const node = self.block_nodes.items[idx];
        return InstIterator{
            .layout = self,
            .head = node.first_inst,
            .tail = node.last_inst,
        };
    }

    // ------------------------------------------------------------------------
    // Program point comparison
    // ------------------------------------------------------------------------

    /// Compare two program points for ordering.
    /// Works with both blocks and instructions.
    pub fn ppCmpBlock(self: Self, a: Block, b: Block) std.math.Order {
        if (a.index == b.index) return .eq;
        const a_seq = self.block_nodes.items[a.asU32()].seq;
        const b_seq = self.block_nodes.items[b.asU32()].seq;
        return std.math.order(a_seq, b_seq);
    }

    /// Compare a block and an instruction for ordering.
    pub fn ppCmpBlockInst(self: Self, block: Block, inst: Inst) std.math.Order {
        const inst_block = self.inst_nodes.items[inst.asU32()].block.?;
        const cmp = self.ppCmpBlock(block, inst_block);
        if (cmp != .eq) return cmp;
        // Block comes before its instructions
        return .lt;
    }

    /// Compare two instructions for ordering.
    pub fn ppCmpInst(self: Self, a: Inst, b: Inst) std.math.Order {
        if (a.index == b.index) return .eq;
        const a_node = self.inst_nodes.items[a.asU32()];
        const b_node = self.inst_nodes.items[b.asU32()];

        // First compare blocks
        const block_cmp = self.ppCmpBlock(a_node.block.?, b_node.block.?);
        if (block_cmp != .eq) return block_cmp;

        // Same block - compare sequence numbers
        return std.math.order(a_node.seq, b_node.seq);
    }

    // ------------------------------------------------------------------------
    // Block splitting
    // ------------------------------------------------------------------------

    /// Split a block at the given instruction.
    /// All instructions starting from `before` are moved to `new_block`.
    /// The `new_block` is inserted after `old_block` in the layout.
    pub fn splitBlock(self: *Self, allocator: std.mem.Allocator, new_block: Block, before: Inst) !void {
        try self.ensureBlockCapacity(allocator, new_block);

        const before_idx = before.asU32();
        const old_block = self.inst_nodes.items[before_idx].block.?;
        const old_block_idx = old_block.asU32();

        // Get old block info
        const next_block = self.block_nodes.items[old_block_idx].next;
        const last_inst = self.block_nodes.items[old_block_idx].last_inst;

        // Sequence for new block
        const seq = if (next_block) |n|
            midpoint(self.block_nodes.items[old_block_idx].seq, self.block_nodes.items[n.asU32()].seq) orelse
                self.block_nodes.items[old_block_idx].seq + MINOR_STRIDE
        else
            self.block_nodes.items[old_block_idx].seq + MAJOR_STRIDE;

        // Set up new block
        self.block_nodes.items[new_block.asU32()] = BlockNode{
            .prev = old_block,
            .next = next_block,
            .first_inst = before,
            .last_inst = last_inst,
            .cold = false,
            .seq = seq,
        };

        // Update block linkage
        self.block_nodes.items[old_block_idx].next = new_block;
        if (next_block) |n| {
            self.block_nodes.items[n.asU32()].prev = new_block;
        } else {
            self.last_block = new_block;
        }

        // Disconnect instruction links
        const prev_inst = self.inst_nodes.items[before_idx].prev;
        self.inst_nodes.items[before_idx].prev = null;
        self.block_nodes.items[old_block_idx].last_inst = prev_inst;
        if (prev_inst) |pi| {
            self.inst_nodes.items[pi.asU32()].next = null;
        } else {
            self.block_nodes.items[old_block_idx].first_inst = null;
        }

        // Fix instruction -> block pointers
        var opt_i: ?Inst = before;
        while (opt_i) |i| {
            self.inst_nodes.items[i.asU32()].block = new_block;
            opt_i = self.inst_nodes.items[i.asU32()].next;
        }
    }

    /// Mark a block as cold.
    pub fn setBlockCold(self: *Self, block: Block, cold: bool) void {
        const idx = block.asU32();
        if (idx < self.block_nodes.items.len) {
            self.block_nodes.items[idx].cold = cold;
        }
    }

    /// Check if a block is cold.
    pub fn isBlockCold(self: Self, block: Block) bool {
        const idx = block.asU32();
        if (idx >= self.block_nodes.items.len) return false;
        return self.block_nodes.items[idx].cold;
    }
};

// ============================================================================
// Iterators
// ============================================================================

/// Iterator over blocks in layout order.
pub const BlockIterator = struct {
    layout: *const Layout,
    head: ?Block,
    tail: ?Block,

    pub fn next(self: *BlockIterator) ?Block {
        const result = self.head orelse return null;
        if (self.head) |h| {
            if (self.tail) |t| {
                if (h.index == t.index) {
                    self.head = null;
                    self.tail = null;
                } else {
                    self.head = self.layout.nextBlock(h);
                }
            }
        }
        return result;
    }

    /// Iterate backwards.
    pub fn nextBack(self: *BlockIterator) ?Block {
        const result = self.tail orelse return null;
        if (self.tail) |t| {
            if (self.head) |h| {
                if (h.index == t.index) {
                    self.head = null;
                    self.tail = null;
                } else {
                    self.tail = self.layout.prevBlock(t);
                }
            }
        }
        return result;
    }
};

/// Iterator over instructions in a block.
pub const InstIterator = struct {
    layout: *const Layout,
    head: ?Inst,
    tail: ?Inst,

    pub fn next(self: *InstIterator) ?Inst {
        const result = self.head orelse return null;
        if (self.head) |h| {
            if (self.tail) |t| {
                if (h.index == t.index) {
                    self.head = null;
                    self.tail = null;
                } else {
                    self.head = self.layout.nextInst(h);
                }
            }
        }
        return result;
    }

    /// Iterate backwards.
    pub fn nextBack(self: *InstIterator) ?Inst {
        const result = self.tail orelse return null;
        if (self.tail) |t| {
            if (self.head) |h| {
                if (h.index == t.index) {
                    self.head = null;
                    self.tail = null;
                } else {
                    self.tail = self.layout.prevInst(t);
                }
            }
        }
        return result;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "midpoint" {
    const testing = std.testing;

    try testing.expect(midpoint(0, 1) == null);
    try testing.expectEqual(@as(?SequenceNumber, 1), midpoint(0, 2));
    try testing.expectEqual(@as(?SequenceNumber, 1), midpoint(0, 3));
    try testing.expectEqual(@as(?SequenceNumber, 2), midpoint(0, 4));
    try testing.expectEqual(@as(?SequenceNumber, 2), midpoint(1, 4));
    try testing.expectEqual(@as(?SequenceNumber, 3), midpoint(2, 4));
    try testing.expect(midpoint(3, 4) == null);
}

test "append block" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e0 = Block.fromIndex(0);
    const e1 = Block.fromIndex(1);
    const e2 = Block.fromIndex(2);

    try testing.expect(!layout.isBlockInserted(e0));
    try testing.expect(!layout.isBlockInserted(e1));

    try layout.appendBlock(allocator, e1);
    try testing.expect(!layout.isBlockInserted(e0));
    try testing.expect(layout.isBlockInserted(e1));
    try testing.expect(!layout.isBlockInserted(e2));

    // Check iterator
    var iter = layout.blocks();
    try testing.expectEqual(e1, iter.next().?);
    try testing.expect(iter.next() == null);

    try layout.appendBlock(allocator, e2);
    try layout.appendBlock(allocator, e0);

    iter = layout.blocks();
    try testing.expectEqual(e1, iter.next().?);
    try testing.expectEqual(e2, iter.next().?);
    try testing.expectEqual(e0, iter.next().?);
    try testing.expect(iter.next() == null);
}

test "insert block" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e0 = Block.fromIndex(0);
    const e1 = Block.fromIndex(1);
    const e2 = Block.fromIndex(2);

    try layout.appendBlock(allocator, e1);
    try layout.insertBlock(allocator, e2, e1);

    var iter = layout.blocks();
    try testing.expectEqual(e2, iter.next().?);
    try testing.expectEqual(e1, iter.next().?);
    try testing.expect(iter.next() == null);

    try layout.insertBlock(allocator, e0, e1);
    iter = layout.blocks();
    try testing.expectEqual(e2, iter.next().?);
    try testing.expectEqual(e0, iter.next().?);
    try testing.expectEqual(e1, iter.next().?);
    try testing.expect(iter.next() == null);
}

test "append inst" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e1 = Block.fromIndex(1);
    try layout.appendBlock(allocator, e1);

    const inst0 = Inst.fromIndex(0);
    const inst1 = Inst.fromIndex(1);
    const inst2 = Inst.fromIndex(2);

    try testing.expect(layout.instBlock(inst0) == null);
    try testing.expect(layout.instBlock(inst1) == null);

    try layout.appendInst(allocator, inst1, e1);
    try testing.expect(layout.instBlock(inst0) == null);
    try testing.expectEqual(e1, layout.instBlock(inst1).?);

    try layout.appendInst(allocator, inst2, e1);
    try layout.appendInst(allocator, inst0, e1);

    var iter = layout.blockInsts(e1);
    try testing.expectEqual(inst1, iter.next().?);
    try testing.expectEqual(inst2, iter.next().?);
    try testing.expectEqual(inst0, iter.next().?);
    try testing.expect(iter.next() == null);

    // Test reverse iteration
    iter = layout.blockInsts(e1);
    try testing.expectEqual(inst0, iter.nextBack().?);
    try testing.expectEqual(inst2, iter.nextBack().?);
    try testing.expectEqual(inst1, iter.nextBack().?);
    try testing.expect(iter.nextBack() == null);
}

test "insert inst" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e1 = Block.fromIndex(1);
    try layout.appendBlock(allocator, e1);

    const inst0 = Inst.fromIndex(0);
    const inst1 = Inst.fromIndex(1);
    const inst2 = Inst.fromIndex(2);

    try layout.appendInst(allocator, inst1, e1);
    try layout.insertInst(allocator, inst2, inst1);
    try layout.insertInst(allocator, inst0, inst1);

    var iter = layout.blockInsts(e1);
    try testing.expectEqual(inst2, iter.next().?);
    try testing.expectEqual(inst0, iter.next().?);
    try testing.expectEqual(inst1, iter.next().?);
    try testing.expect(iter.next() == null);
}

test "remove inst" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e1 = Block.fromIndex(1);
    try layout.appendBlock(allocator, e1);

    const inst0 = Inst.fromIndex(0);
    const inst1 = Inst.fromIndex(1);
    const inst2 = Inst.fromIndex(2);

    try layout.appendInst(allocator, inst1, e1);
    try layout.appendInst(allocator, inst2, e1);
    try layout.appendInst(allocator, inst0, e1);

    layout.removeInst(inst2);
    try testing.expect(layout.instBlock(inst2) == null);

    var iter = layout.blockInsts(e1);
    try testing.expectEqual(inst1, iter.next().?);
    try testing.expectEqual(inst0, iter.next().?);
    try testing.expect(iter.next() == null);
}

test "multiple blocks" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e0 = Block.fromIndex(0);
    const e1 = Block.fromIndex(1);

    try testing.expect(layout.entryBlock() == null);
    try layout.appendBlock(allocator, e0);
    try testing.expectEqual(e0, layout.entryBlock().?);
    try layout.appendBlock(allocator, e1);
    try testing.expectEqual(e0, layout.entryBlock().?);

    const inst0 = Inst.fromIndex(0);
    const inst1 = Inst.fromIndex(1);
    const inst2 = Inst.fromIndex(2);
    const inst3 = Inst.fromIndex(3);

    try layout.appendInst(allocator, inst0, e0);
    try layout.appendInst(allocator, inst1, e0);
    try layout.appendInst(allocator, inst2, e1);
    try layout.appendInst(allocator, inst3, e1);

    var iter0 = layout.blockInsts(e0);
    try testing.expectEqual(inst0, iter0.next().?);
    try testing.expectEqual(inst1, iter0.next().?);
    try testing.expect(iter0.next() == null);

    var iter1 = layout.blockInsts(e1);
    try testing.expectEqual(inst2, iter1.next().?);
    try testing.expectEqual(inst3, iter1.next().?);
    try testing.expect(iter1.next() == null);
}

test "split block" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e0 = Block.fromIndex(0);
    const e1 = Block.fromIndex(1);

    const inst0 = Inst.fromIndex(0);
    const inst1 = Inst.fromIndex(1);
    const inst2 = Inst.fromIndex(2);

    try layout.appendBlock(allocator, e0);
    try layout.appendInst(allocator, inst0, e0);
    try layout.appendInst(allocator, inst1, e0);
    try layout.appendInst(allocator, inst2, e0);

    // Split at inst1, so e0 keeps inst0, e1 gets inst1 and inst2
    try layout.splitBlock(allocator, e1, inst1);

    try testing.expectEqual(e0, layout.instBlock(inst0).?);
    try testing.expectEqual(e1, layout.instBlock(inst1).?);
    try testing.expectEqual(e1, layout.instBlock(inst2).?);

    // Check block order
    var block_iter = layout.blocks();
    try testing.expectEqual(e0, block_iter.next().?);
    try testing.expectEqual(e1, block_iter.next().?);
    try testing.expect(block_iter.next() == null);

    // Check e0 instructions
    var iter0 = layout.blockInsts(e0);
    try testing.expectEqual(inst0, iter0.next().?);
    try testing.expect(iter0.next() == null);

    // Check e1 instructions
    var iter1 = layout.blockInsts(e1);
    try testing.expectEqual(inst1, iter1.next().?);
    try testing.expectEqual(inst2, iter1.next().?);
    try testing.expect(iter1.next() == null);
}

test "pp_cmp" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var layout = Layout.init();
    defer layout.deinit(allocator);

    const e0 = Block.fromIndex(0);
    const e1 = Block.fromIndex(1);

    const inst0 = Inst.fromIndex(0);
    const inst1 = Inst.fromIndex(1);

    try layout.appendBlock(allocator, e0);
    try layout.appendBlock(allocator, e1);
    try layout.appendInst(allocator, inst0, e0);
    try layout.appendInst(allocator, inst1, e1);

    try testing.expectEqual(std.math.Order.eq, layout.ppCmpBlock(e0, e0));
    try testing.expectEqual(std.math.Order.lt, layout.ppCmpBlock(e0, e1));
    try testing.expectEqual(std.math.Order.gt, layout.ppCmpBlock(e1, e0));

    try testing.expectEqual(std.math.Order.lt, layout.ppCmpBlockInst(e0, inst0));
    try testing.expectEqual(std.math.Order.lt, layout.ppCmpBlockInst(e0, inst1));

    try testing.expectEqual(std.math.Order.eq, layout.ppCmpInst(inst0, inst0));
    try testing.expectEqual(std.math.Order.lt, layout.ppCmpInst(inst0, inst1));
    try testing.expectEqual(std.math.Order.gt, layout.ppCmpInst(inst1, inst0));
}
