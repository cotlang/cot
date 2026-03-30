//! Function interface for the register allocator.
//!
//! Ported from regalloc2's `src/lib.rs` Function trait.
//!
//! This module defines the interface that the register allocator uses to
//! query information about the input code. The VCode implementation must
//! provide these methods.

const std = @import("std");
const index = @import("index.zig");
const operand = @import("operand.zig");

const Block = index.Block;
const Inst = index.Inst;
const InstRange = index.InstRange;
const VReg = index.VReg;
const PReg = index.PReg;
const PRegSet = index.PRegSet;
const RegClass = index.RegClass;
const Operand = operand.Operand;

//=============================================================================
// Function - Interface for input code
//=============================================================================

/// A Function provides access to the machine-instruction / CFG representation.
///
/// This is the interface that the register allocator uses to query the input
/// code. The VCode implementation must provide these methods.
///
/// Requirements:
/// - Block IDs must start at 0 and be numbered sequentially
/// - VReg IDs must start at 0 and be numbered sequentially
/// - Instruction IDs must start at 0 and be numbered sequentially
///
/// Usage in Zig (comptime dispatch):
/// ```
/// fn runRegalloc(comptime F: type, func: *const F, env: MachineEnv) Output {
///     // Call methods directly on func
///     const n_blocks = Function.numBlocks(func);
///     // ...
/// }
/// ```
pub fn Function(comptime Impl: type) type {
    return struct {
        const Self = @This();

        impl: *const Impl,

        pub fn init(impl: *const Impl) Self {
            return .{ .impl = impl };
        }

        // -------------------------
        // CFG traversal
        // -------------------------

        /// How many instructions are there?
        pub inline fn numInsts(self: Self) usize {
            return self.impl.numInsts();
        }

        /// How many blocks are there?
        pub inline fn numBlocks(self: Self) usize {
            return self.impl.numBlocks();
        }

        /// Get the index of the entry block.
        pub inline fn entryBlock(self: Self) Block {
            return self.impl.entryBlock();
        }

        /// Get the range of instructions in a block.
        pub inline fn blockInsns(self: Self, block: Block) InstRange {
            return self.impl.blockInsns(block);
        }

        /// Get CFG successors for a given block.
        pub inline fn blockSuccs(self: Self, block: Block) []const Block {
            return self.impl.blockSuccs(block);
        }

        /// Get CFG predecessors for a given block.
        pub inline fn blockPreds(self: Self, block: Block) []const Block {
            return self.impl.blockPreds(block);
        }

        /// Get the block parameters (phi inputs) for a given block.
        pub inline fn blockParams(self: Self, block: Block) []const VReg {
            return self.impl.blockParams(block);
        }

        /// Determine whether an instruction is a return instruction.
        pub inline fn isRet(self: Self, inst: Inst) bool {
            return self.impl.isRet(inst);
        }

        /// Determine whether an instruction is the end-of-block branch.
        pub inline fn isBranch(self: Self, inst: Inst) bool {
            return self.impl.isBranch(inst);
        }

        /// If `inst` is a branch at the end of `block`, returns the outgoing
        /// blockparam arguments for the given successor index.
        pub inline fn branchBlockparams(self: Self, block: Block, inst: Inst, succ_idx: usize) []const VReg {
            return self.impl.branchBlockparams(block, inst, succ_idx);
        }

        // -------------------------
        // Instruction register slots
        // -------------------------

        /// Get the operands for an instruction.
        pub inline fn instOperands(self: Self, inst: Inst) []const Operand {
            return self.impl.instOperands(inst);
        }

        /// Get the clobbered registers for an instruction.
        /// These are registers that hold garbage values after the instruction.
        pub inline fn instClobbers(self: Self, inst: Inst) PRegSet {
            return self.impl.instClobbers(inst);
        }

        /// Get the number of virtual registers in use.
        pub inline fn numVregs(self: Self) usize {
            return self.impl.numVregs();
        }

        // -------------------------
        // Spills/reloads
        // -------------------------

        /// How many logical spill slots does the given regclass require?
        /// E.g., a 128-bit vector may need 2 slots on a 64-bit machine.
        pub inline fn spillslotSize(self: Self, reg_class: RegClass) usize {
            return self.impl.spillslotSize(reg_class);
        }

        // -------------------------
        // Optional methods with defaults
        // -------------------------

        /// Get debug value labels (for DWARF generation).
        /// Default: empty slice.
        pub inline fn debugValueLabels(self: Self) []const DebugLabel {
            if (@hasDecl(Impl, "debugValueLabels")) {
                return self.impl.debugValueLabels();
            }
            return &.{};
        }

        /// When providing a spillslot number for a multi-slot spillslot,
        /// do we provide the first or the last?
        /// Default: false (provide first).
        pub inline fn multiSpillslotNamedByLastSlot(self: Self) bool {
            if (@hasDecl(Impl, "multiSpillslotNamedByLastSlot")) {
                return self.impl.multiSpillslotNamedByLastSlot();
            }
            return false;
        }

        /// Allow a single instruction to define a vreg multiple times.
        /// Default: false.
        pub inline fn allowMultipleVregDefs(self: Self) bool {
            if (@hasDecl(Impl, "allowMultipleVregDefs")) {
                return self.impl.allowMultipleVregDefs();
            }
            return false;
        }
    };
}

/// Debug label tuple: (vreg, start_inst, end_inst, label_id)
pub const DebugLabel = struct {
    vreg: VReg,
    start: Inst,
    end: Inst,
    label: u32,
};

//=============================================================================
// FunctionVTable - Runtime dispatch version
//=============================================================================

/// Runtime-dispatch version of Function for use when comptime dispatch
/// is not feasible (e.g., crossing library boundaries).
pub const FunctionVTable = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        numInsts: *const fn (*anyopaque) usize,
        numBlocks: *const fn (*anyopaque) usize,
        entryBlock: *const fn (*anyopaque) Block,
        blockInsns: *const fn (*anyopaque, Block) InstRange,
        blockSuccs: *const fn (*anyopaque, Block) []const Block,
        blockPreds: *const fn (*anyopaque, Block) []const Block,
        blockParams: *const fn (*anyopaque, Block) []const VReg,
        isRet: *const fn (*anyopaque, Inst) bool,
        isBranch: *const fn (*anyopaque, Inst) bool,
        branchBlockparams: *const fn (*anyopaque, Block, Inst, usize) []const VReg,
        instOperands: *const fn (*anyopaque, Inst) []const Operand,
        instClobbers: *const fn (*anyopaque, Inst) PRegSet,
        numVregs: *const fn (*anyopaque) usize,
        spillslotSize: *const fn (*anyopaque, RegClass) usize,
        debugValueLabels: *const fn (*anyopaque) []const DebugLabel,
        multiSpillslotNamedByLastSlot: *const fn (*anyopaque) bool,
        allowMultipleVregDefs: *const fn (*anyopaque) bool,
    };

    pub inline fn numInsts(self: FunctionVTable) usize {
        return self.vtable.numInsts(self.ptr);
    }

    pub inline fn numBlocks(self: FunctionVTable) usize {
        return self.vtable.numBlocks(self.ptr);
    }

    pub inline fn entryBlock(self: FunctionVTable) Block {
        return self.vtable.entryBlock(self.ptr);
    }

    pub inline fn blockInsns(self: FunctionVTable, block: Block) InstRange {
        return self.vtable.blockInsns(self.ptr, block);
    }

    pub inline fn blockSuccs(self: FunctionVTable, block: Block) []const Block {
        return self.vtable.blockSuccs(self.ptr, block);
    }

    pub inline fn blockPreds(self: FunctionVTable, block: Block) []const Block {
        return self.vtable.blockPreds(self.ptr, block);
    }

    pub inline fn blockParams(self: FunctionVTable, block: Block) []const VReg {
        return self.vtable.blockParams(self.ptr, block);
    }

    pub inline fn isRet(self: FunctionVTable, inst: Inst) bool {
        return self.vtable.isRet(self.ptr, inst);
    }

    pub inline fn isBranch(self: FunctionVTable, inst: Inst) bool {
        return self.vtable.isBranch(self.ptr, inst);
    }

    pub inline fn branchBlockparams(self: FunctionVTable, block: Block, inst: Inst, succ_idx: usize) []const VReg {
        return self.vtable.branchBlockparams(self.ptr, block, inst, succ_idx);
    }

    pub inline fn instOperands(self: FunctionVTable, inst: Inst) []const Operand {
        return self.vtable.instOperands(self.ptr, inst);
    }

    pub inline fn instClobbers(self: FunctionVTable, inst: Inst) PRegSet {
        return self.vtable.instClobbers(self.ptr, inst);
    }

    pub inline fn numVregs(self: FunctionVTable) usize {
        return self.vtable.numVregs(self.ptr);
    }

    pub inline fn spillslotSize(self: FunctionVTable, reg_class: RegClass) usize {
        return self.vtable.spillslotSize(self.ptr, reg_class);
    }

    pub inline fn debugValueLabels(self: FunctionVTable) []const DebugLabel {
        return self.vtable.debugValueLabels(self.ptr);
    }

    pub inline fn multiSpillslotNamedByLastSlot(self: FunctionVTable) bool {
        return self.vtable.multiSpillslotNamedByLastSlot(self.ptr);
    }

    pub inline fn allowMultipleVregDefs(self: FunctionVTable) bool {
        return self.vtable.allowMultipleVregDefs(self.ptr);
    }
};

//=============================================================================
// Tests
//=============================================================================

test "Function interface - mock implementation" {
    // A minimal mock implementation for testing
    const MockFunction = struct {
        n_insts: usize,
        n_blocks: usize,
        n_vregs: usize,

        pub fn numInsts(self: *const @This()) usize {
            return self.n_insts;
        }

        pub fn numBlocks(self: *const @This()) usize {
            return self.n_blocks;
        }

        pub fn entryBlock(_: *const @This()) Block {
            return Block.new(0);
        }

        pub fn blockInsns(_: *const @This(), block: Block) InstRange {
            // Simple: each block has 2 instructions
            const start = block.idx() * 2;
            return InstRange.new(Inst.new(start), Inst.new(start + 2));
        }

        pub fn blockSuccs(_: *const @This(), _: Block) []const Block {
            return &.{};
        }

        pub fn blockPreds(_: *const @This(), _: Block) []const Block {
            return &.{};
        }

        pub fn blockParams(_: *const @This(), _: Block) []const VReg {
            return &.{};
        }

        pub fn isRet(_: *const @This(), _: Inst) bool {
            return false;
        }

        pub fn isBranch(_: *const @This(), _: Inst) bool {
            return false;
        }

        pub fn branchBlockparams(_: *const @This(), _: Block, _: Inst, _: usize) []const VReg {
            return &.{};
        }

        pub fn instOperands(_: *const @This(), _: Inst) []const Operand {
            return &.{};
        }

        pub fn instClobbers(_: *const @This(), _: Inst) PRegSet {
            return PRegSet.empty();
        }

        pub fn numVregs(self: *const @This()) usize {
            return self.n_vregs;
        }

        pub fn spillslotSize(_: *const @This(), _: RegClass) usize {
            return 1;
        }
    };

    const mock = MockFunction{
        .n_insts = 10,
        .n_blocks = 3,
        .n_vregs = 5,
    };

    const func = Function(MockFunction).init(&mock);

    try std.testing.expectEqual(@as(usize, 10), func.numInsts());
    try std.testing.expectEqual(@as(usize, 3), func.numBlocks());
    try std.testing.expectEqual(@as(usize, 5), func.numVregs());
    try std.testing.expectEqual(@as(usize, 0), func.entryBlock().idx());

    // Test block instructions
    const insns = func.blockInsns(Block.new(1));
    try std.testing.expectEqual(@as(usize, 2), insns.first().idx());
    try std.testing.expectEqual(@as(usize, 2), insns.len());

    // Test defaults
    try std.testing.expect(!func.multiSpillslotNamedByLastSlot());
    try std.testing.expect(!func.allowMultipleVregDefs());
    try std.testing.expectEqual(@as(usize, 0), func.debugValueLabels().len);
}

test "DebugLabel construction" {
    const label = DebugLabel{
        .vreg = VReg.new(0, .int),
        .start = Inst.new(0),
        .end = Inst.new(10),
        .label = 42,
    };

    try std.testing.expectEqual(@as(usize, 0), label.vreg.vreg());
    try std.testing.expectEqual(@as(usize, 0), label.start.idx());
    try std.testing.expectEqual(@as(usize, 10), label.end.idx());
    try std.testing.expectEqual(@as(u32, 42), label.label);
}
