//! VCode-to-Regalloc Adapter
//!
//! Port of Cranelift's impl RegallocFunction for VCode (vcode.rs:1568-1657)
//!
//! This adapter makes VCode implement the regalloc2 Function interface,
//! allowing the register allocator to operate on VCode directly.
//!
//! The adapter handles type conversions between VCode types (InsnIndex, BlockIndex)
//! and regalloc types (Inst, Block).

const std = @import("std");

// Import regalloc types
const regalloc_index = @import("../regalloc/index.zig");
const regalloc_operand = @import("../regalloc/operand.zig");
const regalloc_func = @import("../regalloc/func.zig");

const Block = regalloc_index.Block;
const Inst = regalloc_index.Inst;
const InstRange = regalloc_index.InstRange;
const VReg = regalloc_index.VReg;
const PReg = regalloc_index.PReg;
const PRegSet = regalloc_index.PRegSet;
const RegClass = regalloc_index.RegClass;
const Operand = regalloc_operand.Operand;

// Import VCode types
const vcode_mod = @import("vcode.zig");

/// Create an adapter that makes VCode implement regalloc's Function interface.
///
/// This follows Cranelift's pattern where VCode directly implements
/// RegallocFunction (vcode.rs:1568-1657).
///
/// Usage:
/// ```zig
/// const vcode = try buildVCode(...);
/// const adapter = VCodeRegallocAdapter(MachInst).init(&vcode);
/// const func = regalloc_func.Function(@TypeOf(adapter)).init(&adapter);
/// const output = try regalloc.run(func, machine_env, options);
/// ```
pub fn VCodeRegallocAdapter(comptime I: type) type {
    const VCodeType = vcode_mod.VCode(I);
    const InsnIndex = vcode_mod.InsnIndex;
    const BlockIndex = vcode_mod.BlockIndex;
    const VCodeOperand = vcode_mod.Operand;

    return struct {
        const Self = @This();

        vcode: *const VCodeType,

        pub fn init(vcode: *const VCodeType) Self {
            return .{ .vcode = vcode };
        }

        // =====================================================================
        // CFG traversal - Required by regalloc Function interface
        // =====================================================================

        /// How many instructions are there?
        pub fn numInsts(self: *const Self) usize {
            return self.vcode.numInsts();
        }

        /// How many blocks are there?
        pub fn numBlocks(self: *const Self) usize {
            return self.vcode.numBlocks();
        }

        /// Get the index of the entry block.
        pub fn entryBlock(self: *const Self) Block {
            return blockFromVCode(self.vcode.entryBlock());
        }

        /// Get the range of instructions in a block.
        pub fn blockInsns(self: *const Self, block: Block) InstRange {
            const vcode_range = self.vcode.blockInsns(blockToVCode(block));
            return InstRange.new(
                instFromVCode(vcode_range.start),
                instFromVCode(vcode_range.end),
            );
        }

        /// Get CFG successors for a given block.
        pub fn blockSuccs(self: *const Self, block: Block) []const Block {
            const vcode_succs = self.vcode.blockSuccs(blockToVCode(block));
            // Note: This requires the underlying slice to have compatible memory layout.
            // BlockIndex and Block should have the same layout (both are u32 wrappers).
            return @ptrCast(vcode_succs);
        }

        /// Get CFG predecessors for a given block.
        pub fn blockPreds(self: *const Self, block: Block) []const Block {
            const vcode_preds = self.vcode.blockPreds(blockToVCode(block));
            return @ptrCast(vcode_preds);
        }

        /// Get the block parameters (phi inputs) for a given block.
        pub fn blockParams(self: *const Self, block: Block) []const VReg {
            return self.vcode.blockParams(blockToVCode(block));
        }

        /// Determine whether an instruction is a return instruction.
        pub fn isRet(self: *const Self, inst: Inst) bool {
            return self.vcode.isRet(instToVCode(inst));
        }

        /// Determine whether an instruction is the end-of-block branch.
        pub fn isBranch(self: *const Self, inst: Inst) bool {
            return self.vcode.isBranch(instToVCode(inst));
        }

        /// If `inst` is a branch at the end of `block`, returns the outgoing
        /// blockparam arguments for the given successor index.
        pub fn branchBlockparams(self: *const Self, block: Block, inst: Inst, succ_idx: usize) []const VReg {
            return self.vcode.branchBlockparams(blockToVCode(block), instToVCode(inst), succ_idx);
        }

        // =====================================================================
        // Instruction register slots
        // =====================================================================

        /// Get the operands for an instruction.
        pub fn instOperands(self: *const Self, inst: Inst) []const Operand {
            const vcode_operands = self.vcode.instOperands(instToVCode(inst));
            // Convert VCode Operand slice to regalloc Operand slice.
            // VCode stores operands in a compatible format.
            return convertOperandSlice(vcode_operands);
        }

        /// Get the clobbered registers for an instruction.
        pub fn instClobbers(self: *const Self, inst: Inst) PRegSet {
            return self.vcode.instClobbers(instToVCode(inst));
        }

        /// Get the number of virtual registers in use.
        pub fn numVregs(self: *const Self) usize {
            return self.vcode.numVregs();
        }

        // =====================================================================
        // Spills/reloads
        // =====================================================================

        /// How many logical spill slots does the given regclass require?
        pub fn spillslotSize(_: *const Self, _: RegClass) usize {
            // Default: 1 slot per register (8 bytes on 64-bit)
            // This should be overridden based on ABI if available
            return 1;
        }

        // =====================================================================
        // Type conversion helpers
        // =====================================================================

        fn blockFromVCode(vcode_block: BlockIndex) Block {
            return Block.new(vcode_block.idx());
        }

        fn blockToVCode(block: Block) BlockIndex {
            return BlockIndex.new(block.idx());
        }

        fn instFromVCode(vcode_inst: InsnIndex) Inst {
            return Inst.new(vcode_inst.index());
        }

        fn instToVCode(inst: Inst) InsnIndex {
            return InsnIndex.new(inst.idx());
        }

        fn convertOperandSlice(vcode_operands: []const VCodeOperand) []const Operand {
            // VCode's Operand and regalloc's Operand need to be converted.
            // For now, we use a static buffer. In production, this should be
            // properly allocated or the types should be unified.
            //
            // TODO: Unify Operand types to avoid this conversion.
            const S = struct {
                var buffer: [256]Operand = undefined;
            };

            const len = @min(vcode_operands.len, S.buffer.len);
            for (vcode_operands[0..len], 0..) |vcode_op, i| {
                S.buffer[i] = convertOperand(vcode_op);
            }
            return S.buffer[0..len];
        }

        fn convertOperand(vcode_op: VCodeOperand) Operand {
            const vreg = vcode_op.getVReg();
            const constraint: regalloc_operand.OperandConstraint = switch (vcode_op.getConstraint()) {
                .any => .any,
                .stack => .stack,
                .fixed_reg => |preg| .{ .fixed_reg = preg },
                .reuse => |idx| .{ .reuse = idx },
            };
            const kind: regalloc_operand.OperandKind = switch (vcode_op.getKind()) {
                .use => .use,
                .def => .def,
                .use_def => .use, // TODO: Handle use_def properly
            };
            const pos: regalloc_operand.OperandPos = switch (vcode_op.getPos()) {
                .early => .early,
                .late => .late,
            };
            return Operand.new(vreg, constraint, kind, pos);
        }
    };
}

// =============================================================================
// Tests
// =============================================================================

test "VCodeRegallocAdapter - type conversion" {
    const Block_t = regalloc_index.Block;
    const Inst_t = regalloc_index.Inst;

    // Test block conversion
    const block = Block_t.new(5);
    try std.testing.expectEqual(@as(usize, 5), block.idx());

    // Test inst conversion
    const inst = Inst_t.new(10);
    try std.testing.expectEqual(@as(usize, 10), inst.idx());

    // Test range
    const range = InstRange.new(Inst_t.new(0), Inst_t.new(5));
    try std.testing.expectEqual(@as(usize, 5), range.len());
}
