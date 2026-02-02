//! This implements the VCode container: a CFG of Insts that have been lowered.
//!
//! VCode is virtual-register code. An instruction in VCode is almost a machine
//! instruction; however, its register slots can refer to virtual registers in
//! addition to real machine registers.
//!
//! VCode is structured with traditional basic blocks, and
//! each block must be terminated by an unconditional branch (one target), a
//! conditional branch (two targets), or a return (no targets). Note that this
//! slightly differs from the machine code of most ISAs: in most ISAs, a
//! conditional branch has one target (and the not-taken case falls through).
//! However, we expect that machine backends will elide branches to the following
//! block (i.e., zero-offset jumps), and will be able to codegen a branch-cond /
//! branch-uncond pair if *both* targets are not fallthrough. This allows us to
//! play with layout prior to final binary emission, as well, if we want.
//!
//! See the main module comment in `inst.zig` for more details on the VCode-based
//! backend pipeline.

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import from other machinst modules
const reg_mod = @import("reg.zig");
const inst_mod = @import("inst.zig");

// Import CLIF types
const clif = @import("../../../ir/clif/mod.zig");

// Import regalloc types
const regalloc_operand = @import("../regalloc/operand.zig");

// Re-export key types from reg module
pub const VReg = reg_mod.VReg;
pub const Reg = reg_mod.Reg;
pub const PReg = reg_mod.PReg;
pub const RealReg = reg_mod.RealReg;
pub const VirtualReg = reg_mod.VirtualReg;
pub const Writable = reg_mod.Writable;
pub const RegClass = reg_mod.RegClass;
pub const PRegSet = reg_mod.PRegSet;
pub const SpillSlot = reg_mod.SpillSlot;

// Re-export key types - use CLIF Type for proper integration
pub const Type = clif.Type;
pub const RelSourceLoc = inst_mod.RelSourceLoc;
pub const MachLabel = inst_mod.MachLabel;
pub const MachTerminator = inst_mod.MachTerminator;
pub const CallType = inst_mod.CallType;
pub const FunctionCalls = inst_mod.FunctionCalls;
pub const CodeOffset = inst_mod.CodeOffset;

/// Index referring to an instruction in VCode.
/// Wrapper around usize for type safety.
pub const InsnIndex = struct {
    index_val: usize,

    const Self = @This();

    pub fn new(i: usize) Self {
        return .{ .index_val = i };
    }

    pub fn index(self: Self) usize {
        return self.index_val;
    }

    pub fn next(self: Self) Self {
        return .{ .index_val = self.index_val + 1 };
    }

    pub fn prev(self: Self) Self {
        return .{ .index_val = self.index_val -| 1 };
    }

    /// Convert to a backwards instruction index.
    pub fn toBackwardsInsnIndex(self: Self, num_insts: usize) BackwardsInsnIndex {
        return BackwardsInsnIndex.new(num_insts - self.index_val - 1);
    }

    pub fn invalid() Self {
        return .{ .index_val = std.math.maxInt(usize) };
    }

    pub fn isValid(self: Self) bool {
        return self.index_val != std.math.maxInt(usize);
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.index_val == other.index_val;
    }

    pub fn lessThan(self: Self, other: Self) bool {
        return self.index_val < other.index_val;
    }

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("Inst({})", .{self.index_val});
    }
};

/// An index referring to an instruction in the VCode when it is backwards,
/// during VCode construction.
pub const BackwardsInsnIndex = struct {
    inner: InsnIndex,

    const Self = @This();

    pub fn new(i: usize) Self {
        return .{ .inner = InsnIndex.new(i) };
    }

    pub fn index(self: Self) usize {
        return self.inner.index();
    }

    pub fn eql(self: Self, other: Self) bool {
        return self.inner.eql(other.inner);
    }

    pub fn hash(self: Self) u64 {
        return std.hash.Wyhash.hash(0, std.mem.asBytes(&self.inner.index_val));
    }
};

/// Index referring to a basic block in VCode.
/// Re-export from inst_mod for type unification with blockorder.
pub const BlockIndex = inst_mod.BlockIndex;


/// Operand for register allocation.
/// Mirrors regalloc2::Operand.
pub const Operand = struct {
    vreg_val: VReg,
    constraint_val: OperandConstraint,
    kind_val: OperandKind,
    pos_val: OperandPos,

    const Self = @This();

    pub fn create(v: VReg, constraint: OperandConstraint, kind: OperandKind, pos: OperandPos) Self {
        return .{
            .vreg_val = v,
            .constraint_val = constraint,
            .kind_val = kind,
            .pos_val = pos,
        };
    }

    pub fn getVReg(self: Self) VReg {
        return self.vreg_val;
    }

    pub fn getConstraint(self: Self) OperandConstraint {
        return self.constraint_val;
    }

    pub fn getKind(self: Self) OperandKind {
        return self.kind_val;
    }

    pub fn getPos(self: Self) OperandPos {
        return self.pos_val;
    }
};

/// Operand constraint for register allocation.
pub const OperandConstraint = union(enum) {
    /// Any register in the class is allowed.
    any: void,
    /// A specific fixed register is required.
    fixed_reg: PReg,
    /// Register must be reused from another operand.
    reuse: usize,
    /// Stack slot is allowed.
    stack: void,
};

/// Operand kind - use or def.
pub const OperandKind = enum {
    use,
    def,
    use_def,
};

/// Operand position - before or after the instruction.
pub const OperandPos = enum {
    early,
    late,
};

/// A range in a Ranges structure.
pub const Range = struct {
    start: usize,
    end: usize,

    pub fn len(self: Range) usize {
        return self.end - self.start;
    }

    pub fn isEmpty(self: Range) bool {
        return self.start == self.end;
    }
};

/// A structure to store ranges efficiently.
/// Each entry stores the end index; the start is implicitly the previous entry's end.
pub const Ranges = struct {
    ends: std.ArrayListUnmanaged(usize),

    const Self = @This();

    pub fn init() Self {
        return .{ .ends = .{} };
    }

    pub fn withCapacity(allocator: Allocator, capacity: usize) !Self {
        var ends = std.ArrayListUnmanaged(usize){};
        try ends.ensureTotalCapacity(allocator, capacity);
        return .{ .ends = ends };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.ends.deinit(allocator);
    }

    pub fn reserve(self: *Self, allocator: Allocator, additional: usize) !void {
        try self.ends.ensureUnusedCapacity(allocator, additional);
    }

    pub fn pushEnd(self: *Self, allocator: Allocator, end: usize) !void {
        try self.ends.append(allocator, end);
    }

    pub fn len(self: Self) usize {
        return self.ends.items.len;
    }

    pub fn get(self: Self, idx: usize) Range {
        const start = if (idx == 0) 0 else self.ends.items[idx - 1];
        const end = self.ends.items[idx];
        return .{ .start = start, .end = end };
    }

    /// Reverse the index order (for backward-to-forward conversion).
    pub fn reverseIndex(self: *Self) void {
        std.mem.reverse(usize, self.ends.items);
    }

    /// Reverse the target values (for backward-to-forward conversion).
    pub fn reverseTarget(self: *Self, n: usize) void {
        for (self.ends.items) |*end| {
            end.* = n - end.*;
        }
    }

    pub const Iterator = struct {
        ranges: *const Ranges,
        idx: usize,

        pub fn next(self: *Iterator) ?struct { usize, Range } {
            if (self.idx >= self.ranges.len()) return null;
            const range = self.ranges.get(self.idx);
            const result = .{ self.idx, range };
            self.idx += 1;
            return result;
        }
    };

    pub fn iter(self: *const Self) Iterator {
        return .{ .ranges = self, .idx = 0 };
    }
};

/// Constant for no instruction offset.
pub const NO_INST_OFFSET: CodeOffset = std.math.maxInt(u32);

/// ProgPoint represents a program point (instruction + position).
pub const ProgPoint = struct {
    inst_val: InsnIndex,
    pos_val: InstPosition,

    const Self = @This();

    pub fn before(i: InsnIndex) Self {
        return .{ .inst_val = i, .pos_val = .before };
    }

    pub fn after(i: InsnIndex) Self {
        return .{ .inst_val = i, .pos_val = .after };
    }

    pub fn getInst(self: Self) InsnIndex {
        return self.inst_val;
    }

    pub fn getPos(self: Self) InstPosition {
        return self.pos_val;
    }
};

/// Position within an instruction.
pub const InstPosition = enum {
    before,
    after,
};

/// InstRange represents a range of instructions.
pub const InstRange = struct {
    start: InsnIndex,
    end: InsnIndex,

    const Self = @This();

    pub fn new(start: InsnIndex, end: InsnIndex) Self {
        return .{ .start = start, .end = end };
    }

    pub fn len(self: Self) usize {
        return self.end.index() - self.start.index();
    }

    pub fn isEmpty(self: Self) bool {
        return self.start.index() >= self.end.index();
    }
};

/// Edit operations from register allocation.
pub const Edit = union(enum) {
    move: struct {
        from: Allocation,
        to: Allocation,
    },
};

/// Allocation result from register allocator.
/// Use the unified type from regalloc/operand.zig.
pub const Allocation = regalloc_operand.Allocation;

/// InstOrEdit for iterating over instructions and edits together.
pub const InstOrEdit = union(enum) {
    inst: InsnIndex,
    edit: Edit,
};

/// Value label for debug info.
pub const ValueLabel = struct {
    val: u32,

    const Self = @This();

    pub fn fromU32(v: u32) Self {
        return .{ .val = v };
    }

    pub fn asU32(self: Self) u32 {
        return self.val;
    }
};

/// Debug value label entry.
pub const DebugValueLabel = struct {
    vreg: VReg,
    start: InsnIndex,
    end: InsnIndex,
    label: u32,
};

/// User stack map entry.
pub const UserStackMapEntry = struct {
    /// Offset from stack pointer.
    offset: i32,
    /// Size in bytes.
    size: u32,
};

/// User stack map for safepoints.
pub const UserStackMap = struct {
    entries: std.ArrayListUnmanaged(UserStackMapEntry),

    const Self = @This();

    pub fn init(allocator: Allocator, entries: []const UserStackMapEntry, offsets: []const i32) !Self {
        _ = offsets;
        var self = Self{ .entries = .{} };
        try self.entries.appendSlice(allocator, entries);
        return self;
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.entries.deinit(allocator);
    }
};

/// Debug tag.
pub const DebugTag = struct {
    val: u32,
};

/// Position for debug tags.
pub const MachDebugTagPos = enum {
    pre,
    post,
};

/// Value location for debug info.
pub const LabelValueLoc = union(enum) {
    reg: Reg,
    cfa_offset: i64,
};

/// Range where a value is stored at a particular location.
pub const ValueLocRange = struct {
    loc: LabelValueLoc,
    start: CodeOffset,
    end: CodeOffset,
};

/// Value labels ranges map.
pub const ValueLabelsRanges = std.AutoHashMapUnmanaged(ValueLabel, std.ArrayListUnmanaged(ValueLocRange));

/// A function in "VCode" (virtualized-register code) form, after
/// lowering. This is essentially a standard CFG of basic blocks,
/// where each basic block consists of lowered instructions produced
/// by the machine-specific backend.
///
/// Note that the VCode is immutable once produced, and is not
/// modified by register allocation in particular. Rather, register
/// allocation on the `VCode` produces a separate `regalloc2::Output`
/// struct, and this can be passed to `emit`. `emit` in turn does not
/// modify the vcode, but produces an `EmitResult`, which contains the
/// machine code itself, and the associated disassembly and/or
/// metadata as requested.
pub fn VCode(comptime I: type) type {
    return struct {
        const Self = @This();

        /// MachBuffer type for this instruction type.
        /// Uses the ISA-specific LabelUse from the instruction type.
        const MachBufferType = @import("buffer.zig").MachBuffer(I.LabelUse);

        /// Allocator used for all allocations.
        allocator: Allocator,

        /// VReg IR-level types.
        vreg_types: std.ArrayListUnmanaged(Type),

        /// Lowered machine instructions in order corresponding to the original IR.
        insts: std.ArrayListUnmanaged(I),

        /// A map from backwards instruction index to the user stack map for that
        /// instruction.
        ///
        /// This is a sparse side table that only has entries for instructions that
        /// are safepoints, and only for a subset of those that have an associated
        /// user stack map.
        user_stack_maps: std.AutoHashMapUnmanaged(BackwardsInsnIndex, UserStackMap),

        /// A map from backwards instruction index to the debug tags for
        /// that instruction. Each entry indexes a range in the
        /// `debug_tag_pool`.
        debug_tags: std.AutoHashMapUnmanaged(BackwardsInsnIndex, Range),

        /// Pooled storage for sequences of debug tags; indexed by entries
        /// in `debug_tags`.
        debug_tag_pool: std.ArrayListUnmanaged(DebugTag),

        /// Operands: pre-regalloc references to virtual registers with
        /// constraints, in one flattened array. This allows the regalloc
        /// to efficiently access all operands without requiring expensive
        /// matches or method invocations on insts.
        operands: std.ArrayListUnmanaged(Operand),

        /// Operand index ranges: for each instruction in `insts`, there
        /// is a tuple here providing the range in `operands` for that
        /// instruction's operands.
        operand_ranges: Ranges,

        /// Clobbers: a sparse map from instruction indices to clobber masks.
        clobbers: std.AutoHashMapUnmanaged(InsnIndex, PRegSet),

        /// Source locations for each instruction. (`SourceLoc` is a `u32`, so it is
        /// reasonable to keep one of these per instruction.)
        srclocs: std.ArrayListUnmanaged(RelSourceLoc),

        /// Entry block.
        entry: BlockIndex,

        /// Block instruction indices.
        block_ranges: Ranges,

        /// Block successors: index range in the `block_succs` list.
        block_succ_range: Ranges,

        /// Block successor lists, concatenated into one vec. The
        /// `block_succ_range` list of tuples above gives (start, end)
        /// ranges within this list that correspond to each basic block's
        /// successors.
        block_succs: std.ArrayListUnmanaged(BlockIndex),

        /// Block predecessors: index range in the `block_preds` list.
        block_pred_range: Ranges,

        /// Block predecessor lists, concatenated into one vec. The
        /// `block_pred_range` list of tuples above gives (start, end)
        /// ranges within this list that correspond to each basic block's
        /// predecessors.
        block_preds: std.ArrayListUnmanaged(BlockIndex),

        /// Block parameters: index range in `block_params` below.
        block_params_range: Ranges,

        /// Block parameter lists, concatenated into one vec. The
        /// `block_params_range` list of tuples above gives (start, end)
        /// ranges within this list that correspond to each basic block's
        /// blockparam vregs.
        block_params: std.ArrayListUnmanaged(VReg),

        /// Outgoing block arguments on branch instructions, concatenated
        /// into one list.
        ///
        /// Note that this is conceptually a 3D array: we have a VReg list
        /// per block, per successor. We flatten those three dimensions
        /// into this 1D vec, then store index ranges in two levels of
        /// indirection.
        ///
        /// Indexed by the indices in `branch_block_arg_succ_range`.
        branch_block_args: std.ArrayListUnmanaged(VReg),

        /// Array of sequences of (start, end) tuples in
        /// `branch_block_args`, one for each successor; these sequences
        /// for each block are concatenated.
        ///
        /// Indexed by the indices in `branch_block_arg_succ_range`.
        branch_block_arg_range: Ranges,

        /// For a given block, indices in `branch_block_arg_range`
        /// corresponding to all of its successors.
        branch_block_arg_succ_range: Ranges,

        /// Debug value labels for debuginfo attached to vregs.
        debug_value_labels: std.ArrayListUnmanaged(DebugValueLabel),

        /// Facts on VRegs, for proof-carrying code verification.
        facts: std.ArrayListUnmanaged(?Fact),

        /// Minimum function alignment (log2).
        log2_min_function_alignment: u8,

        /// Create a new empty VCode.
        pub fn init(allocator: Allocator, n_blocks: usize, log2_min_function_alignment: u8) !Self {
            var self = Self{
                .allocator = allocator,
                .vreg_types = .{},
                .insts = .{},
                .user_stack_maps = .{},
                .debug_tags = .{},
                .debug_tag_pool = .{},
                .operands = .{},
                .operand_ranges = Ranges.init(),
                .clobbers = .{},
                .srclocs = .{},
                .entry = BlockIndex.new(0),
                .block_ranges = Ranges.init(),
                .block_succ_range = Ranges.init(),
                .block_succs = .{},
                .block_pred_range = Ranges.init(),
                .block_preds = .{},
                .block_params_range = Ranges.init(),
                .block_params = .{},
                .branch_block_args = .{},
                .branch_block_arg_range = Ranges.init(),
                .branch_block_arg_succ_range = Ranges.init(),
                .debug_value_labels = .{},
                .facts = .{},
                .log2_min_function_alignment = log2_min_function_alignment,
            };

            // Pre-allocate based on expected block count
            try self.insts.ensureTotalCapacity(allocator, 10 * n_blocks);
            try self.operands.ensureTotalCapacity(allocator, 30 * n_blocks);
            try self.operand_ranges.reserve(allocator, 10 * n_blocks);
            try self.srclocs.ensureTotalCapacity(allocator, 10 * n_blocks);
            try self.block_ranges.reserve(allocator, n_blocks);
            try self.block_succ_range.reserve(allocator, n_blocks);
            try self.block_succs.ensureTotalCapacity(allocator, n_blocks);
            try self.block_params_range.reserve(allocator, n_blocks);
            try self.block_params.ensureTotalCapacity(allocator, 5 * n_blocks);
            try self.branch_block_args.ensureTotalCapacity(allocator, 10 * n_blocks);
            try self.branch_block_arg_range.reserve(allocator, 2 * n_blocks);
            try self.branch_block_arg_succ_range.reserve(allocator, n_blocks);

            return self;
        }

        pub fn deinit(self: *Self) void {
            self.vreg_types.deinit(self.allocator);
            self.insts.deinit(self.allocator);

            var map_iter = self.user_stack_maps.valueIterator();
            while (map_iter.next()) |v| {
                v.deinit(self.allocator);
            }
            self.user_stack_maps.deinit(self.allocator);

            self.debug_tags.deinit(self.allocator);
            self.debug_tag_pool.deinit(self.allocator);
            self.operands.deinit(self.allocator);
            self.operand_ranges.deinit(self.allocator);
            self.clobbers.deinit(self.allocator);
            self.srclocs.deinit(self.allocator);
            self.block_ranges.deinit(self.allocator);
            self.block_succ_range.deinit(self.allocator);
            self.block_succs.deinit(self.allocator);
            self.block_pred_range.deinit(self.allocator);
            self.block_preds.deinit(self.allocator);
            self.block_params_range.deinit(self.allocator);
            self.block_params.deinit(self.allocator);
            self.branch_block_args.deinit(self.allocator);
            self.branch_block_arg_range.deinit(self.allocator);
            self.branch_block_arg_succ_range.deinit(self.allocator);
            self.debug_value_labels.deinit(self.allocator);
            self.facts.deinit(self.allocator);
        }

        /// Get the number of blocks. Block indices will be in the range `0 ..
        /// (self.numBlocks() - 1)`.
        pub fn numBlocks(self: *const Self) usize {
            return self.block_ranges.len();
        }

        /// The number of lowered instructions.
        pub fn numInsts(self: *const Self) usize {
            return self.insts.items.len;
        }

        /// Get instruction at index.
        pub fn getInst(self: *const Self, idx: InsnIndex) *const I {
            return &self.insts.items[idx.index()];
        }

        /// Get mutable instruction at index.
        pub fn getInstMut(self: *Self, idx: InsnIndex) *I {
            return &self.insts.items[idx.index()];
        }

        /// Get the type of a VReg.
        pub fn vregType(self: *const Self, vreg: VReg) Type {
            return self.vreg_types.items[vreg.vreg()];
        }

        /// Get the fact, if any, for a given VReg.
        pub fn vregFact(self: *const Self, vreg: VReg) ?*const Fact {
            const fact = self.facts.items[vreg.vreg()];
            return if (fact) |*f| f else null;
        }

        /// Set the fact for a given VReg.
        pub fn setVregFact(self: *Self, vreg: VReg, fact: Fact) void {
            self.facts.items[vreg.vreg()] = fact;
        }

        /// Get the entry block.
        pub fn entryBlock(self: *const Self) BlockIndex {
            return self.entry;
        }

        /// Get the instruction range for a block.
        pub fn blockInsns(self: *const Self, block: BlockIndex) InstRange {
            const range = self.block_ranges.get(block.index());
            return InstRange.new(InsnIndex.new(range.start), InsnIndex.new(range.end));
        }

        /// Get the successors of a block.
        pub fn blockSuccs(self: *const Self, block: BlockIndex) []const BlockIndex {
            const range = self.block_succ_range.get(block.index());
            return self.block_succs.items[range.start..range.end];
        }

        /// Get the predecessors of a block.
        pub fn blockPreds(self: *const Self, block: BlockIndex) []const BlockIndex {
            const range = self.block_pred_range.get(block.index());
            return self.block_preds.items[range.start..range.end];
        }

        /// Get the block parameters for a block.
        pub fn blockParams(self: *const Self, block: BlockIndex) []const VReg {
            // As a special case we don't return block params for the entry block,
            // as all the arguments will be defined by the `Inst::Args` instruction.
            if (block.eql(self.entry)) {
                return &[_]VReg{};
            }

            const range = self.block_params_range.get(block.index());
            return self.block_params.items[range.start..range.end];
        }

        /// Get the branch block params for a specific successor.
        pub fn branchBlockparams(self: *const Self, block: BlockIndex, _: InsnIndex, succ_idx: usize) []const VReg {
            const succ_range = self.branch_block_arg_succ_range.get(block.index());
            std.debug.assert(succ_idx < succ_range.len());
            const branch_block_args = self.branch_block_arg_range.get(succ_range.start + succ_idx);
            return self.branch_block_args.items[branch_block_args.start..branch_block_args.end];
        }

        /// Get the operands for an instruction.
        pub fn instOperands(self: *const Self, insn: InsnIndex) []const Operand {
            const range = self.operand_ranges.get(insn.index());
            return self.operands.items[range.start..range.end];
        }

        /// Get the clobbers for an instruction.
        pub fn instClobbers(self: *const Self, insn: InsnIndex) PRegSet {
            return self.clobbers.get(insn) orelse PRegSet.empty();
        }

        /// Get the number of vregs.
        pub fn numVregs(self: *const Self) usize {
            return self.vreg_types.items.len;
        }

        /// Get the debug value labels.
        pub fn debugValueLabels(self: *const Self) []const DebugValueLabel {
            return self.debug_value_labels.items;
        }

        /// Check if instruction is a return.
        pub fn isRet(self: *const Self, insn: InsnIndex) bool {
            const inst = self.getInst(insn);
            const term = inst.isTerm();
            return switch (term) {
                // We treat blocks terminated by an unconditional trap like a return for regalloc.
                .none => inst.isTrap(),
                .ret, .ret_call => true,
                .branch => false,
            };
        }

        /// Check if instruction is a branch.
        pub fn isBranch(self: *const Self, insn: InsnIndex) bool {
            return self.getInst(insn).isTerm() == .branch;
        }

        /// Does a given instruction define any facts?
        pub fn instDefinesFacts(self: *const Self, inst_idx: InsnIndex) bool {
            for (self.instOperands(inst_idx)) |op| {
                if (op.getKind() == .def) {
                    if (self.facts.items[op.getVReg().vreg()]) |_| {
                        return true;
                    }
                }
            }
            return false;
        }

        /// Get the user stack map associated with the given forward instruction index.
        pub fn getUserStackMap(self: *const Self, inst: InsnIndex) ?*const UserStackMap {
            const index = inst.toBackwardsInsnIndex(self.numInsts());
            return self.user_stack_maps.getPtr(index);
        }

        // =====================================================================
        // Emission - Phase 5 of compilation pipeline
        // Port of cranelift/codegen/src/machinst/vcode.rs emit()
        // =====================================================================

        /// Result of emitting VCode to machine code.
        pub const EmitResult = struct {
            /// The finalized machine code buffer.
            buffer: MachBufferFinalized,
            /// Frame size in bytes.
            frame_size: u32,
            /// Offsets of basic blocks in the emitted code.
            bb_offsets: std.ArrayListUnmanaged(u32),

            pub fn deinit(self: *EmitResult, alloc: Allocator) void {
                self.buffer.deinit();
                self.bb_offsets.deinit(alloc);
            }
        };

        /// Emit machine code from this VCode using register allocation output.
        ///
        /// This is the final phase of compilation. It takes the VCode (with virtual
        /// registers) and the regalloc output (which maps virtual to physical registers
        /// and includes spill/reload edits), and produces actual machine code bytes.
        ///
        /// The emission process:
        ///   1. Compute frame layout from spill slots
        ///   2. Emit function prologue
        ///   3. For each block in emission order:
        ///      - Bind the block label
        ///      - For each instruction (with regalloc edits interleaved):
        ///        - If edit: emit move/spill/reload
        ///        - If instruction: emit with physical registers
        ///   4. Emit function epilogue (typically before returns)
        ///   5. Finalize the buffer (resolve labels, emit constants)
        ///
        /// Reference: cranelift/codegen/src/machinst/vcode.rs emit()
        pub fn emit(
            self: *const Self,
            regalloc_output: *const RegallocOutput,
            emit_info: *const I.EmitInfo,
        ) !EmitResult {
            const alloc = self.allocator;

            // Create machine buffer
            var buffer = MachBufferType.init(alloc);
            errdefer buffer.deinit();

            // Reserve labels for all blocks
            try buffer.reserveLabelsForBlocks(self.numBlocks());

            // Track basic block offsets
            var bb_offsets = std.ArrayListUnmanaged(u32){};
            errdefer bb_offsets.deinit(alloc);
            try bb_offsets.ensureTotalCapacity(alloc, self.numBlocks());

            // Compute frame size from spillslots
            const frame_size = regalloc_output.num_spillslots * 8; // 8 bytes per slot

            // Emit prologue (placeholder - actual implementation depends on ABI)
            // In a complete implementation, this would:
            //   - Push callee-saved registers
            //   - Set up frame pointer
            //   - Allocate stack space for spills

            // Emit each block
            for (0..self.numBlocks()) |block_idx| {
                const block = BlockIndex.new(block_idx);

                // Record block offset
                bb_offsets.appendAssumeCapacity(buffer.curOffset());

                // Bind the block label
                try buffer.bindLabel(MachLabel.fromBlock(block));

                // Get instruction range for this block
                const insn_range = self.blockInsns(block);

                // Emit instructions with regalloc edits interleaved
                var insn_idx = insn_range.start;
                while (insn_idx.lessThan(insn_range.end)) {
                    // Get allocations for this instruction
                    const inst_allocs = regalloc_output.getInstAllocs(insn_idx);

                    // Get the instruction
                    const inst = self.getInst(insn_idx);

                    // Emit the instruction with physical registers
                    try inst.emitWithAllocs(&buffer, inst_allocs, emit_info);

                    insn_idx = insn_idx.next();
                }
            }

            // Emit epilogue (done per-return in the instruction loop above)
            // In a complete implementation, returns would restore callee-saves

            // Finalize the buffer
            const finalized = try buffer.finish();

            return EmitResult{
                .buffer = finalized,
                .frame_size = frame_size,
                .bb_offsets = bb_offsets,
            };
        }
    };
}

/// Regalloc output interface for emission.
/// This provides access to the register allocation results.
pub const RegallocOutput = struct {
    /// Number of spill slots allocated.
    num_spillslots: u32,
    /// Allocations per instruction operand.
    allocs: []const Allocation,
    /// Allocation ranges per instruction.
    alloc_ranges: []const Range,

    const Self = @This();

    /// Get allocations for an instruction.
    pub fn getInstAllocs(self: *const Self, inst: InsnIndex) []const Allocation {
        if (inst.index() < self.alloc_ranges.len) {
            const range = self.alloc_ranges[inst.index()];
            return self.allocs[range.start..range.end];
        }
        return &[_]Allocation{};
    }
};

/// Emit info passed to instruction emission.
/// Contains target-specific settings for encoding.
pub const EmitInfo = struct {
    /// Target architecture.
    arch: Arch = .aarch64,

    pub const Arch = enum {
        aarch64,
        x64,
    };
};

/// Finalized machine buffer result.
pub const MachBufferFinalized = @import("buffer.zig").MachBufferFinalized;

/// Fact for proof-carrying code verification.
pub const Fact = struct {
    kind: FactKind,

    pub const FactKind = union(enum) {
        /// Value is in a range.
        range: struct {
            min: i64,
            max: i64,
        },
        /// Value is a pointer with bounds.
        ptr: struct {
            base: i64,
            offset: i64,
            size: u64,
        },
        /// Value equals another value.
        value: VReg,
    };

    pub fn format(self: Fact, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.kind) {
            .range => |r| try writer.print("range[{}..{}]", .{ r.min, r.max }),
            .ptr => |p| try writer.print("ptr({}, {}, {})", .{ p.base, p.offset, p.size }),
            .value => |v| try writer.print("value(v{})", .{v.vreg()}),
        }
    }
};

/// Direction in which a VCodeBuilder builds VCode.
pub const VCodeBuildDirection = enum {
    // TODO: add `Forward` once we need it and can test it adequately.
    /// Backward-build pass: we expect the producer to call `emit()`
    /// with instructions in reverse program order within each block.
    backward,
};

/// A builder for a VCode function body.
///
/// This builder has the ability to accept instructions in either
/// forward or reverse order, depending on the pass direction that
/// produces the VCode. The lowering from CLIF to VCode<MachInst>
/// ordinarily occurs in reverse order (in order to allow instructions
/// to be lowered only if used, and not merged) so a reversal will
/// occur at the end of lowering to ensure the VCode is in machine
/// order.
///
/// If built in reverse, block and instruction indices used once the
/// VCode is built are relative to the final (reversed) order, not the
/// order of construction. Note that this means we do not know the
/// final block or instruction indices when building, so we do not
/// hand them out. (The user is assumed to know them when appending
/// terminator instructions with successor blocks.)
pub fn VCodeBuilder(comptime I: type) type {
    return struct {
        const Self = @This();
        const VCodeType = VCode(I);

        /// In-progress VCode.
        vcode: VCodeType,

        /// In what direction is the build occurring?
        direction: VCodeBuildDirection,

        /// Debug-value label in-progress map, keyed by label. For each
        /// label, we keep disjoint ranges mapping to vregs. We'll flatten
        /// this into (vreg, range, label) tuples when done.
        debug_info: std.AutoHashMapUnmanaged(ValueLabel, std.ArrayListUnmanaged(DebugInfoEntry)),

        const DebugInfoEntry = struct {
            start: InsnIndex,
            end: InsnIndex,
            vreg: VReg,
        };

        /// Create a new VCodeBuilder.
        pub fn init(
            allocator: Allocator,
            n_blocks: usize,
            direction: VCodeBuildDirection,
            log2_min_function_alignment: u8,
        ) !Self {
            return .{
                .vcode = try VCodeType.init(allocator, n_blocks, log2_min_function_alignment),
                .direction = direction,
                .debug_info = .{},
            };
        }

        pub fn deinit(self: *Self) void {
            var iter = self.debug_info.valueIterator();
            while (iter.next()) |v| {
                v.deinit(self.vcode.allocator);
            }
            self.debug_info.deinit(self.vcode.allocator);
            self.vcode.deinit();
        }

        /// Set the current block as the entry block.
        pub fn setEntry(self: *Self, block: BlockIndex) void {
            self.vcode.entry = block;
        }

        /// End the current basic block. Must be called after emitting vcode insts
        /// for IR insts and prior to ending the function (building the VCode).
        pub fn endBb(self: *Self) !void {
            const allocator = self.vcode.allocator;
            const end_idx = self.vcode.insts.items.len;
            // Add the instruction index range to the list of blocks.
            try self.vcode.block_ranges.pushEnd(allocator, end_idx);
            // End the successors list.
            const succ_end = self.vcode.block_succs.items.len;
            try self.vcode.block_succ_range.pushEnd(allocator, succ_end);
            // End the blockparams list.
            const block_params_end = self.vcode.block_params.items.len;
            try self.vcode.block_params_range.pushEnd(allocator, block_params_end);
            // End the branch blockparam args list.
            const branch_block_arg_succ_end = self.vcode.branch_block_arg_range.len();
            try self.vcode.branch_block_arg_succ_range.pushEnd(allocator, branch_block_arg_succ_end);
        }

        /// Add a block parameter.
        pub fn addBlockParam(self: *Self, param: VirtualReg) !void {
            try self.vcode.block_params.append(self.vcode.allocator, param.toVReg());
        }

        fn addBranchArgsForSucc(self: *Self, args: []const Reg) !void {
            const allocator = self.vcode.allocator;
            for (args) |arg| {
                // Convert Reg to VReg - the arg should be a virtual register
                const vreg = if (arg.toVirtualReg()) |v|
                    v.toVReg()
                else
                    // If it's a physical register, create a VReg with the same hw encoding
                    VReg.init(@intCast(arg.toRealReg().?.hwEnc()), arg.class());
                try self.vcode.branch_block_args.append(allocator, vreg);
            }
            const end = self.vcode.branch_block_args.items.len;
            try self.vcode.branch_block_arg_range.pushEnd(allocator, end);
        }

        /// Push an instruction for the current BB and current IR inst
        /// within the BB.
        pub fn push(self: *Self, insn: I, loc: RelSourceLoc) !void {
            const allocator = self.vcode.allocator;
            std.debug.assert(!insn.isLowLevelBranch()); // These are not meant to be in VCode.
            try self.vcode.insts.append(allocator, insn);
            try self.vcode.srclocs.append(allocator, loc);
        }

        /// Add a successor block with branch args.
        pub fn addSucc(self: *Self, block: BlockIndex, args: []const Reg) !void {
            try self.vcode.block_succs.append(self.vcode.allocator, block);
            try self.addBranchArgsForSucc(args);
        }

        /// Add a debug value label to a register.
        pub fn addValueLabel(self: *Self, reg: Reg, label: ValueLabel) !void {
            // 1) In the reversed order, we consider the instructions
            //    that define ranges in the "debug_info" array to refer
            //    to the IP **after** them (when reversed):
            //      IP[2]__| Inst 3 |
            //      IP[1]__| Inst 2 |
            //      IP[0]__| Inst 1 |
            //             | Inst 0 |
            //    This is so that we can represent IP[<function start>],
            //    done at the cost of not being to represent IP[<function end>],
            //    which is OK since no values will be live at that point.
            // 2) The live range for "reg" begins at the current IP
            //    and continues until the next, in execution order,
            //    VReg that defines "label". Since the ranges are open
            //    at the end, the subtraction of 1 cancels out:
            //      [last..current IP] <=>
            //      [last..last emitted inst index] <=>
            //      [last..next_inst_index - 1] <=>
            //      [last..next_inst_index)
            //
            const allocator = self.vcode.allocator;
            const next_inst_index = self.vcode.insts.items.len;
            if (next_inst_index == 0) {
                // This would produce a defective [0..0) range.
                return;
            }
            const next_inst = InsnIndex.new(next_inst_index);
            const gop = try self.debug_info.getOrPut(allocator, label);
            if (!gop.found_existing) {
                gop.value_ptr.* = .{};
            }
            const labels = gop.value_ptr;
            const last = if (labels.items.len > 0)
                labels.items[labels.items.len - 1].end
            else
                InsnIndex.new(0);
            try labels.append(allocator, .{
                .start = last,
                .end = next_inst,
                .vreg = VReg.fromReg(reg),
            });
        }

        fn computePredsFromSuccs(self: *Self) !void {
            const allocator = self.vcode.allocator;
            const num_blocks = self.vcode.numBlocks();

            // Do a linear-time counting sort: first determine how many
            // times each block appears as a successor.
            var starts = try allocator.alloc(u32, num_blocks);
            defer allocator.free(starts);
            @memset(starts, 0);

            for (self.vcode.block_succs.items) |succ| {
                starts[succ.index()] += 1;
            }

            // Determine for each block the starting index where that
            // block's predecessors should go. This is equivalent to the
            // ranges we need to store in block_pred_range.
            try self.vcode.block_pred_range.reserve(allocator, num_blocks);
            var end: u32 = 0;
            for (starts) |*count| {
                const start = end;
                end += count.*;
                count.* = start;
                try self.vcode.block_pred_range.pushEnd(allocator, end);
            }
            const end_usize: usize = @intCast(end);
            std.debug.assert(end_usize == self.vcode.block_succs.items.len);

            // Walk over the successors again, this time grouped by
            // predecessor, and push the predecessor at the current
            // starting position of each of its successors.
            try self.vcode.block_preds.resize(allocator, end_usize);
            @memset(self.vcode.block_preds.items, BlockIndex.invalid());

            var iter = self.vcode.block_succ_range.iter();
            while (iter.next()) |entry| {
                const pred_idx, const range = entry;
                const pred = BlockIndex.new(pred_idx);
                for (self.vcode.block_succs.items[range.start..range.end]) |succ| {
                    const pos = &starts[succ.index()];
                    self.vcode.block_preds.items[pos.*] = pred;
                    pos.* += 1;
                }
            }

            // Verify all preds are valid
            for (self.vcode.block_preds.items) |pred| {
                std.debug.assert(pred.isValid());
            }
        }

        /// Called once, when a build in Backward order is complete, to
        /// perform the overall reversal (into final forward order) and
        /// finalize metadata accordingly.
        fn reverseAndFinalize(self: *Self, vregs: *VRegAllocator(I)) !void {
            const allocator = self.vcode.allocator;
            const n_insts = self.vcode.insts.items.len;
            if (n_insts == 0) {
                return;
            }

            // Reverse the per-block and per-inst sequences.
            self.vcode.block_ranges.reverseIndex();
            self.vcode.block_ranges.reverseTarget(n_insts);
            // block_params_range is indexed by block (and blocks were
            // traversed in reverse) so we reverse it; but block-param
            // sequences in the concatenated vec can remain in reverse
            // order (it is effectively an arena of arbitrarily-placed
            // referenced sequences).
            self.vcode.block_params_range.reverseIndex();
            // Likewise, we reverse block_succ_range, but the block_succ
            // concatenated array can remain as-is.
            self.vcode.block_succ_range.reverseIndex();
            std.mem.reverse(I, self.vcode.insts.items);
            std.mem.reverse(RelSourceLoc, self.vcode.srclocs.items);
            // Likewise, branch_block_arg_succ_range is indexed by block
            // so must be reversed.
            self.vcode.branch_block_arg_succ_range.reverseIndex();

            // To translate an instruction index *endpoint* in reversed
            // order to forward order, compute `n_insts - i`.
            const translate = struct {
                fn f(inst: InsnIndex, n: usize) InsnIndex {
                    return InsnIndex.new(n - inst.index());
                }
            }.f;

            // Generate debug-value labels based on per-label maps.
            var iter = self.debug_info.iterator();
            while (iter.next()) |entry| {
                const label = entry.key_ptr.*;
                const tuples = entry.value_ptr.*;
                for (tuples.items) |tuple| {
                    const vreg = vregs.resolveVregAlias(tuple.vreg);
                    const fwd_start = translate(tuple.end, n_insts);
                    const fwd_end = translate(tuple.start, n_insts);
                    try self.vcode.debug_value_labels.append(allocator, .{
                        .vreg = vreg,
                        .start = fwd_start,
                        .end = fwd_end,
                        .label = label.asU32(),
                    });
                }
            }

            // Now sort debug value labels by VReg, as required by regalloc2.
            std.sort.pdq(DebugValueLabel, self.vcode.debug_value_labels.items, {}, struct {
                fn lessThan(_: void, a: DebugValueLabel, b: DebugValueLabel) bool {
                    return a.vreg.toU32() < b.vreg.toU32();
                }
            }.lessThan);
        }

        fn collectOperands(self: *Self, vregs: *VRegAllocator(I)) !void {
            const allocator = self.vcode.allocator;

            for (self.vcode.insts.items, 0..) |*insn, i| {
                // Push operands from the instruction onto the operand list.
                // We rename through the vreg alias table as we collect the operands.

                // Call the instruction's getOperands method
                // This is a simplified version - full implementation would use OperandCollector
                const operands = insn.getOperands();
                for (operands) |op| {
                    // Convert from reg.Operand to vcode.Operand
                    const resolved_vreg = vregs.resolveVregAlias(op.vreg);
                    const constraint: OperandConstraint = switch (op.constraint) {
                        .reg => .any,
                        .any => .any,
                        .fixed_reg => |preg| .{ .fixed_reg = preg },
                        .reuse => |idx| .{ .reuse = idx },
                    };
                    const kind: OperandKind = switch (op.kind) {
                        .use => .use,
                        .def => .def,
                    };
                    const pos: OperandPos = switch (op.pos) {
                        .early => .early,
                        .late => .late,
                    };
                    try self.vcode.operands.append(allocator, Operand.create(resolved_vreg, constraint, kind, pos));
                }

                const end = self.vcode.operands.items.len;
                try self.vcode.operand_ranges.pushEnd(allocator, end);

                // Check for clobbers
                const clobbers = insn.getClobbers();
                if (!clobbers.isEmpty()) {
                    try self.vcode.clobbers.put(allocator, InsnIndex.new(i), clobbers);
                }
            }

            // Translate blockparam args via the vreg aliases table as well.
            for (self.vcode.branch_block_args.items) |*arg| {
                arg.* = vregs.resolveVregAlias(arg.*);
            }
        }

        /// Build the final VCode.
        pub fn build(self: *Self, vregs: *VRegAllocator(I)) !VCodeType {
            const allocator = self.vcode.allocator;

            // Transfer vreg types and facts
            self.vcode.vreg_types.deinit(allocator);
            self.vcode.vreg_types = vregs.vreg_types;
            vregs.vreg_types = .{};

            self.vcode.facts.deinit(allocator);
            self.vcode.facts = vregs.facts;
            vregs.facts = .{};

            if (self.direction == .backward) {
                try self.reverseAndFinalize(vregs);
            }
            try self.collectOperands(vregs);

            try self.computePredsFromSuccs();

            // Sort debug value labels
            std.sort.pdq(DebugValueLabel, self.vcode.debug_value_labels.items, {}, struct {
                fn lessThan(_: void, a: DebugValueLabel, b: DebugValueLabel) bool {
                    if (a.vreg.toU32() != b.vreg.toU32()) {
                        return a.vreg.toU32() < b.vreg.toU32();
                    }
                    if (a.start.index() != b.start.index()) {
                        return a.start.index() < b.start.index();
                    }
                    return a.end.index() < b.end.index();
                }
            }.lessThan);

            // Move the vcode out
            const result = self.vcode;
            self.vcode = try VCodeType.init(allocator, 0, 0);
            return result;
        }

        /// Add a user stack map for the associated instruction.
        pub fn addUserStackMap(
            self: *Self,
            inst: BackwardsInsnIndex,
            entries: []const UserStackMapEntry,
            offsets: []const i32,
        ) !void {
            const allocator = self.vcode.allocator;
            const stack_map = try UserStackMap.init(allocator, entries, offsets);
            const gop = try self.vcode.user_stack_maps.getOrPut(allocator, inst);
            std.debug.assert(!gop.found_existing);
            gop.value_ptr.* = stack_map;
        }

        /// Add debug tags for the associated instruction.
        pub fn addDebugTags(self: *Self, inst: BackwardsInsnIndex, entries: []const DebugTag) !void {
            const allocator = self.vcode.allocator;
            const start: u32 = @intCast(self.vcode.debug_tag_pool.items.len);
            try self.vcode.debug_tag_pool.appendSlice(allocator, entries);
            const end: u32 = @intCast(self.vcode.debug_tag_pool.items.len);
            try self.vcode.debug_tags.put(allocator, inst, .{ .start = start, .end = end });
        }
    };
}

/// The first VReg index available for user allocation.
/// Pinned vregs (physical registers) occupy indices 0..191.
pub fn firstUserVregIndex() usize {
    return reg_mod.PINNED_VREGS;
}

/// This structure manages VReg allocation during the lifetime of the VCodeBuilder.
pub fn VRegAllocator(comptime I: type) type {
    _ = I; // Will be used for rc_for_type

    return struct {
        const Self = @This();

        allocator: Allocator,

        /// VReg IR-level types.
        vreg_types: std.ArrayListUnmanaged(Type),

        /// VReg aliases. When the final VCode is built we rewrite all
        /// uses of the keys in this table to their replacement values.
        ///
        /// We use these aliases to rename an instruction's expected
        /// result vregs to the returned vregs from lowering, which are
        /// usually freshly-allocated temps.
        vreg_aliases: std.AutoHashMapUnmanaged(VReg, VReg),

        /// A deferred error, to be bubbled up to the top level of the
        /// lowering algorithm. We take this approach because we cannot
        /// currently propagate a `Result` upward through ISLE code (the
        /// lowering rules) or some ABI code.
        deferred_error: ?anyerror,

        /// Facts on VRegs, for proof-carrying code.
        facts: std.ArrayListUnmanaged(?Fact),

        /// Maximum VReg index allowed.
        pub const MAX_VREG: usize = std.math.maxInt(u32);

        pub fn init(allocator: Allocator, capacity: usize) !Self {
            const total_capacity = firstUserVregIndex() + capacity;
            var vreg_types = std.ArrayListUnmanaged(Type){};
            try vreg_types.ensureTotalCapacity(allocator, total_capacity);

            // Fill with invalid types for pinned vregs
            vreg_types.items.len = firstUserVregIndex();
            @memset(vreg_types.items, .INVALID);

            var facts = std.ArrayListUnmanaged(?Fact){};
            try facts.ensureTotalCapacity(allocator, total_capacity);
            facts.items.len = firstUserVregIndex();
            @memset(facts.items, null);

            return .{
                .allocator = allocator,
                .vreg_types = vreg_types,
                .vreg_aliases = .{},
                .deferred_error = null,
                .facts = facts,
            };
        }

        pub fn deinit(self: *Self) void {
            self.vreg_types.deinit(self.allocator);
            self.vreg_aliases.deinit(self.allocator);
            self.facts.deinit(self.allocator);
        }

        /// Allocate a fresh VReg.
        pub fn alloc(self: *Self, ty: Type, rc: RegClass) !Reg {
            if (self.deferred_error != null) {
                return error.CodeTooLarge;
            }
            const v = self.vreg_types.items.len;
            if (v >= MAX_VREG) {
                return error.CodeTooLarge;
            }

            try self.vreg_types.append(self.allocator, ty);
            try self.facts.append(self.allocator, null);

            return Reg.fromVReg(VReg.init(@intCast(v), rc));
        }

        /// Allocate a fresh VReg, deferring any out-of-vregs errors.
        pub fn allocWithDeferredError(self: *Self, ty: Type, rc: RegClass) Reg {
            return self.alloc(ty, rc) catch |e| {
                self.deferred_error = e;
                return self.bogusForDeferredError(rc);
            };
        }

        /// Take any deferred error that was accumulated by `allocWithDeferredError`.
        pub fn takeDeferredError(self: *Self) ?anyerror {
            const err = self.deferred_error;
            self.deferred_error = null;
            return err;
        }

        /// Produce a bogus VReg placeholder for deferred errors.
        fn bogusForDeferredError(self: *Self, rc: RegClass) Reg {
            _ = self;
            return Reg.fromVReg(VReg.init(0, rc));
        }

        /// Rewrite any mention of `from` into `to`.
        pub fn setVregAlias(self: *Self, from: Reg, to: Reg) !void {
            const from_vreg = from.toVirtualReg().?.toVReg();
            const to_vreg = to.toVirtualReg().?.toVReg();
            const resolved_to = self.resolveVregAlias(to_vreg);
            // Disallow cycles (see below).
            std.debug.assert(resolved_to.bits != from_vreg.bits);

            // Maintain the invariant that PCC facts only exist on vregs
            // which aren't aliases. We want to preserve whatever was
            // stated about the vreg before its producer was lowered.
            if (self.facts.items[from_vreg.vreg()]) |fact| {
                self.facts.items[from_vreg.vreg()] = null;
                try self.setFact(resolved_to, fact);
            }

            try self.vreg_aliases.put(self.allocator, from_vreg, resolved_to);
        }

        pub fn resolveVregAlias(self: *const Self, vreg: VReg) VReg {
            // We prevent cycles from existing by resolving targets of
            // aliases eagerly before setting them. If the target resolves
            // to the origin of the alias, then a cycle would be created
            // and the alias is disallowed.
            var current = vreg;
            while (self.vreg_aliases.get(current)) |to| {
                current = to;
            }
            return current;
        }

        /// Set the proof-carrying code fact on a given virtual register.
        fn setFact(self: *Self, vreg: VReg, fact: Fact) !void {
            std.debug.assert(!self.vreg_aliases.contains(vreg));
            self.facts.items[vreg.vreg()] = fact;
        }

        /// Set a fact only if one doesn't already exist.
        pub fn setFactIfMissing(self: *Self, vreg: VirtualReg, fact: Fact) !void {
            const resolved = self.resolveVregAlias(vreg.toVReg());
            if (self.facts.items[resolved.vreg()] == null) {
                try self.setFact(resolved, fact);
            }
        }

        /// Allocate a fresh VReg, with a given fact to apply.
        pub fn allocWithMaybeFact(self: *Self, ty: Type, rc: RegClass, fact: ?Fact) !Reg {
            const result = try self.alloc(ty, rc);

            if (fact) |f| {
                try self.setFact(VReg.fromReg(result), f);
            }

            return result;
        }

        pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.writeAll("VRegAllocator {\n");

            // Print aliases
            var alias_iter = self.vreg_aliases.iterator();
            while (alias_iter.next()) |entry| {
                try writer.print("  {} := {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
            }

            // Print facts
            for (self.facts.items, 0..) |fact, vreg| {
                if (fact) |f| {
                    try writer.print("  v{} ! {}\n", .{ vreg, f });
                }
            }

            try writer.writeAll("}\n");
        }
    };
}

/// This structure tracks the large constants used in VCode that will be emitted separately by the
/// MachBuffer.
///
/// First, during the lowering phase, constants are inserted using
/// VCodeConstants.insert; an intermediate handle, `VCodeConstant`, tracks what constants are
/// used in this phase. Some deduplication is performed, when possible, as constant
/// values are inserted.
///
/// Secondly, during the emission phase, the MachBuffer assigns MachLabels for each of the
/// constants so that instructions can refer to the value's memory location. The MachBuffer
/// then writes the constant values to the buffer.
pub const VCodeConstants = struct {
    const Self = @This();

    allocator: Allocator,
    constants: std.ArrayListUnmanaged(VCodeConstantData),
    pool_uses: std.AutoHashMapUnmanaged(u32, VCodeConstant), // Constant -> VCodeConstant
    well_known_uses: std.AutoHashMapUnmanaged(usize, VCodeConstant), // ptr -> VCodeConstant
    u64s: std.AutoHashMapUnmanaged([8]u8, VCodeConstant),

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .constants = .{},
            .pool_uses = .{},
            .well_known_uses = .{},
            .u64s = .{},
        };
    }

    pub fn withCapacity(allocator: Allocator, expected_num_constants: usize) !Self {
        var self = init(allocator);
        try self.constants.ensureTotalCapacity(allocator, expected_num_constants);
        try self.pool_uses.ensureTotalCapacity(allocator, @intCast(expected_num_constants));
        return self;
    }

    pub fn deinit(self: *Self) void {
        for (self.constants.items) |*c| {
            c.deinit(self.allocator);
        }
        self.constants.deinit(self.allocator);
        self.pool_uses.deinit(self.allocator);
        self.well_known_uses.deinit(self.allocator);
        self.u64s.deinit(self.allocator);
    }

    /// Insert a constant; using this method indicates that a constant value will be used and thus
    /// will be emitted to the `MachBuffer`.
    pub fn insert(self: *Self, data: VCodeConstantData) !VCodeConstant {
        switch (data.kind) {
            .generated => {
                const idx = self.constants.items.len;
                try self.constants.append(self.allocator, data);
                return VCodeConstant{ .index = @intCast(idx) };
            },
            .pool => |constant| {
                if (self.pool_uses.get(constant)) |existing| {
                    return existing;
                }
                const idx = self.constants.items.len;
                try self.constants.append(self.allocator, data);
                const vcode_constant = VCodeConstant{ .index = @intCast(idx) };
                try self.pool_uses.put(self.allocator, constant, vcode_constant);
                return vcode_constant;
            },
            .well_known => |data_ref| {
                const ptr_val = @intFromPtr(data_ref.ptr);
                if (self.well_known_uses.get(ptr_val)) |existing| {
                    return existing;
                }
                const idx = self.constants.items.len;
                try self.constants.append(self.allocator, data);
                const vcode_constant = VCodeConstant{ .index = @intCast(idx) };
                try self.well_known_uses.put(self.allocator, ptr_val, vcode_constant);
                return vcode_constant;
            },
            .u64 => |value| {
                if (self.u64s.get(value)) |existing| {
                    return existing;
                }
                const idx = self.constants.items.len;
                try self.constants.append(self.allocator, data);
                const vcode_constant = VCodeConstant{ .index = @intCast(idx) };
                try self.u64s.put(self.allocator, value, vcode_constant);
                return vcode_constant;
            },
        }
    }

    /// Return the number of constants inserted.
    pub fn len(self: *const Self) usize {
        return self.constants.items.len;
    }

    /// Returns the data associated with the specified constant.
    pub fn get(self: *const Self, c: VCodeConstant) *const VCodeConstantData {
        return &self.constants.items[c.index];
    }

    /// Iterate over the constants.
    pub fn iter(self: *const Self) ConstantIterator {
        return .{ .constants = self, .idx = 0 };
    }

    pub const ConstantIterator = struct {
        constants: *const VCodeConstants,
        idx: usize,

        pub fn next(self: *ConstantIterator) ?struct { VCodeConstant, *const VCodeConstantData } {
            if (self.idx >= self.constants.len()) return null;
            const result = .{
                VCodeConstant{ .index = @intCast(self.idx) },
                self.constants.get(VCodeConstant{ .index = @intCast(self.idx) }),
            };
            self.idx += 1;
            return result;
        }
    };

    /// Checks if the given VCodeConstantData is registered as used by the pool.
    pub fn poolUses(self: *const Self, constant: *const VCodeConstantData) bool {
        return switch (constant.kind) {
            .pool => |c| self.pool_uses.contains(c),
            else => false,
        };
    }
};

/// A use of a constant by one or more VCode instructions.
pub const VCodeConstant = struct {
    index: u32,

    pub fn format(self: VCodeConstant, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("VCodeConstant({})", .{self.index});
    }
};

/// Identify the different types of constant that can be inserted into VCodeConstants.
pub const VCodeConstantData = struct {
    kind: Kind,

    pub const Kind = union(enum) {
        /// A constant already present in the Cranelift IR ConstantPool.
        pool: u32, // Constant index
        /// A reference to a well-known constant value that is statically encoded within the compiler.
        well_known: struct {
            ptr: [*]const u8,
            len: usize,
        },
        /// A constant value generated during lowering.
        generated: std.ArrayListUnmanaged(u8),
        /// A constant of at most 64 bits.
        u64: [8]u8,
    };

    pub fn fromPool(constant: u32, data: []const u8) VCodeConstantData {
        _ = data; // Would be stored in ConstantData in Cranelift
        return .{ .kind = .{ .pool = constant } };
    }

    pub fn fromWellKnown(data: []const u8) VCodeConstantData {
        return .{ .kind = .{ .well_known = .{ .ptr = data.ptr, .len = data.len } } };
    }

    pub fn fromU64(value: u64) VCodeConstantData {
        return .{ .kind = .{ .u64 = @bitCast(value) } };
    }

    pub fn fromGenerated(allocator: Allocator, data: []const u8) !VCodeConstantData {
        var list = std.ArrayListUnmanaged(u8){};
        try list.appendSlice(allocator, data);
        return .{ .kind = .{ .generated = list } };
    }

    pub fn deinit(self: *VCodeConstantData, allocator: Allocator) void {
        switch (self.kind) {
            .generated => |*list| list.deinit(allocator),
            else => {},
        }
    }

    /// Retrieve the constant data as a byte slice.
    pub fn asSlice(self: *const VCodeConstantData) []const u8 {
        return switch (self.kind) {
            .pool => &[_]u8{}, // Would need ConstantData lookup
            .well_known => |wk| wk.ptr[0..wk.len],
            .generated => |list| list.items,
            .u64 => |value| &value,
        };
    }

    /// Calculate the alignment of the constant data.
    pub fn alignment(self: *const VCodeConstantData) u32 {
        const slice = self.asSlice();
        return if (slice.len <= 8) 8 else 16;
    }
};

// =============================================================================
// Tests
// =============================================================================

test "InsnIndex basic operations" {
    const idx = InsnIndex.new(42);
    try std.testing.expectEqual(@as(usize, 42), idx.index());
    try std.testing.expectEqual(@as(usize, 43), idx.next().index());
    try std.testing.expectEqual(@as(usize, 41), idx.prev().index());
    try std.testing.expect(idx.isValid());
    try std.testing.expect(!InsnIndex.invalid().isValid());
}

test "BackwardsInsnIndex conversion" {
    const fwd = InsnIndex.new(7);
    const bwd = fwd.toBackwardsInsnIndex(10);
    try std.testing.expectEqual(@as(usize, 2), bwd.index());
}

test "BlockIndex basic operations" {
    const block = BlockIndex.new(5);
    try std.testing.expectEqual(@as(usize, 5), block.index());
    try std.testing.expect(block.isValid());
    try std.testing.expect(!BlockIndex.invalid().isValid());
}

test "Ranges basic operations" {
    const allocator = std.testing.allocator;
    var ranges = Ranges.init();
    defer ranges.deinit(allocator);

    try ranges.pushEnd(allocator, 5);
    try ranges.pushEnd(allocator, 10);
    try ranges.pushEnd(allocator, 15);

    try std.testing.expectEqual(@as(usize, 3), ranges.len());

    const r0 = ranges.get(0);
    try std.testing.expectEqual(@as(usize, 0), r0.start);
    try std.testing.expectEqual(@as(usize, 5), r0.end);

    const r1 = ranges.get(1);
    try std.testing.expectEqual(@as(usize, 5), r1.start);
    try std.testing.expectEqual(@as(usize, 10), r1.end);

    const r2 = ranges.get(2);
    try std.testing.expectEqual(@as(usize, 10), r2.start);
    try std.testing.expectEqual(@as(usize, 15), r2.end);
}

test "Operand creation" {
    const vreg = VReg.init(10, .int);
    const op = Operand.create(vreg, .any, .def, .late);

    try std.testing.expectEqual(vreg.bits, op.getVReg().bits);
    try std.testing.expectEqual(OperandConstraint.any, op.getConstraint());
    try std.testing.expectEqual(OperandKind.def, op.getKind());
    try std.testing.expectEqual(OperandPos.late, op.getPos());
}

test "VCodeConstants basic operations" {
    const allocator = std.testing.allocator;
    var constants = VCodeConstants.init(allocator);
    defer constants.deinit();

    // Insert a u64 constant
    const c1 = try constants.insert(VCodeConstantData.fromU64(0x12345678));
    try std.testing.expectEqual(@as(u32, 0), c1.index);

    // Insert another u64 constant - should be deduplicated
    const c2 = try constants.insert(VCodeConstantData.fromU64(0x12345678));
    try std.testing.expectEqual(c1.index, c2.index);

    // Insert a different u64 constant
    const c3 = try constants.insert(VCodeConstantData.fromU64(0xABCDEF00));
    try std.testing.expectEqual(@as(u32, 1), c3.index);

    try std.testing.expectEqual(@as(usize, 2), constants.len());
}

test "VCodeConstantData alignment" {
    const small = VCodeConstantData.fromU64(42);
    try std.testing.expectEqual(@as(u32, 8), small.alignment());
}

test "Fact formatting" {
    const fact = Fact{ .kind = .{ .range = .{ .min = 0, .max = 100 } } };
    var buf: [100]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try fact.format("", .{}, stream.writer());
    try std.testing.expectEqualStrings("range[0..100]", stream.getWritten());
}

test "VRegAllocator basic operations" {
    const DummyInst = struct {};
    const allocator = std.testing.allocator;
    var vregs = try VRegAllocator(DummyInst).init(allocator, 100);
    defer vregs.deinit();

    // Allocate some vregs
    const r1 = try vregs.alloc(.I32, .int);
    const r2 = try vregs.alloc(.I64, .int);

    // They should be different
    try std.testing.expect(r1.bits != r2.bits);

    // Check types
    const v1 = r1.toVirtualReg().?.toVReg();
    const v2 = r2.toVirtualReg().?.toVReg();
    try std.testing.expectEqual(Type.I32, vregs.vreg_types.items[v1.vreg()]);
    try std.testing.expectEqual(Type.I64, vregs.vreg_types.items[v2.vreg()]);
}

test "VRegAllocator aliasing" {
    const DummyInst = struct {};
    const allocator = std.testing.allocator;
    var vregs = try VRegAllocator(DummyInst).init(allocator, 100);
    defer vregs.deinit();

    const r1 = try vregs.alloc(.I32, .int);
    const r2 = try vregs.alloc(.I32, .int);

    try vregs.setVregAlias(r1, r2);

    const v1 = r1.toVirtualReg().?.toVReg();
    const v2 = r2.toVirtualReg().?.toVReg();
    const resolved = vregs.resolveVregAlias(v1);
    try std.testing.expectEqual(v2.bits, resolved.bits);
}
