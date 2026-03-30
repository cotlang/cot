//! SSA validation for the register allocator.
//!
//! Ported from regalloc2's `src/ssa.rs`.
//!
//! This module validates that the input IR is in proper SSA form:
//! - Each VReg is defined exactly once
//! - Every use is dominated by its definition
//! - Block structure is valid (terminators at end, branch args match)

const std = @import("std");
const index = @import("index.zig");
const operand_mod = @import("operand.zig");
const output = @import("output.zig");
const cfg_mod = @import("cfg.zig");

const Block = index.Block;
const Inst = index.Inst;
const VReg = index.VReg;
const Operand = operand_mod.Operand;
const OperandKind = operand_mod.OperandKind;
const RegAllocError = output.RegAllocError;
const CFGInfo = cfg_mod.CFGInfo;

//=============================================================================
// SSA Validation
//=============================================================================

/// Validate that the input function is in proper SSA form.
///
/// The `Func` type must have methods:
/// - numBlocks() -> usize
/// - numVregs() -> usize
/// - entryBlock() -> Block
/// - blockParams(Block) -> []const VReg
/// - blockInsns(Block) -> InstRange
/// - blockSuccs(Block) -> []const Block
/// - instOperands(Inst) -> []const Operand
/// - branchBlockparams(Block, Inst, usize) -> []const VReg
/// - isRet(Inst) -> bool
/// - isBranch(Inst) -> bool
pub fn validateSsa(
    comptime Func: type,
    func: *const Func,
    cfginfo: *const CFGInfo,
    allocator: std.mem.Allocator,
) !void {
    const num_vregs = func.numVregs();
    const num_blocks = func.numBlocks();

    // For every block param and inst def, check that this is the only def.
    var defined_in = try allocator.alloc(Block, num_vregs);
    defer allocator.free(defined_in);
    @memset(defined_in, Block.invalid());

    for (0..num_blocks) |block_idx| {
        const block = Block.new(block_idx);

        // Check block params
        for (func.blockParams(block)) |param| {
            if (param.vreg() >= defined_in.len) {
                return error.VRegNotSequential;
            }
            if (defined_in[param.vreg()].isValid()) {
                return error.MultipleDefs;
            }
            defined_in[param.vreg()] = block;
        }

        // Check instruction defs
        var iter = func.blockInsns(block).iter();
        while (iter.next()) |inst| {
            for (func.instOperands(inst)) |op| {
                if (op.kind() == .def) {
                    const vreg = op.vreg();
                    if (vreg.vreg() >= defined_in.len) {
                        return error.VRegNotSequential;
                    }
                    if (defined_in[vreg.vreg()].isValid()) {
                        return error.MultipleDefs;
                    }
                    defined_in[vreg.vreg()] = block;
                }
            }
        }
    }

    // Check that every use is dominated by its definition
    var local = std.AutoHashMap(usize, void).init(allocator);
    defer local.deinit();

    for (0..num_blocks) |block_idx| {
        const block = Block.new(block_idx);
        local.clearRetainingCapacity();

        // Block params are defined at block entry
        for (func.blockParams(block)) |param| {
            try local.put(param.vreg(), {});
        }

        var inst_iter = func.blockInsns(block).iter();
        while (inst_iter.next()) |inst| {
            const operands = func.instOperands(inst);

            // Check uses first
            for (operands) |op| {
                // Skip fixed non-allocatable registers
                if (op.asFixedNonAllocatable() != null) {
                    continue;
                }

                if (op.kind() == .use) {
                    const vreg = op.vreg();
                    const def_block = defined_in[vreg.vreg()];

                    const okay = def_block.isValid() and
                        (if (def_block.eql(block))
                        local.contains(vreg.vreg())
                    else
                        cfginfo.blockDominates(def_block, block));

                    if (!okay) {
                        return error.UseNotDominated;
                    }
                }
            }

            // Record defs after checking uses (SSA: can't use and def same vreg in one inst)
            for (operands) |op| {
                if (op.kind() == .def) {
                    try local.put(op.vreg().vreg(), {});
                }
            }
        }
    }

    // Check block structure
    for (0..num_blocks) |block_idx| {
        const block = Block.new(block_idx);
        const insns = func.blockInsns(block);

        if (insns.isEmpty()) {
            return error.EmptyBlock;
        }

        var insn_iter = insns.iter();
        while (insn_iter.next()) |inst| {
            const is_last = inst.eql(insns.last());

            if (is_last) {
                // Last instruction must be branch or ret
                if (!func.isBranch(inst) and !func.isRet(inst)) {
                    return error.BlockNotTerminated;
                }

                // Check branch args match successor block params
                if (func.isBranch(inst)) {
                    const succs = func.blockSuccs(block);
                    for (succs, 0..) |succ, succ_idx| {
                        const blockparams_in = func.blockParams(succ);
                        const blockparams_out = func.branchBlockparams(block, inst, succ_idx);
                        if (blockparams_in.len != blockparams_out.len) {
                            return error.BranchArgMismatch;
                        }
                    }
                }
            } else {
                // Non-last instruction must not be branch or ret
                if (func.isBranch(inst) or func.isRet(inst)) {
                    return error.TerminatorInMiddle;
                }
            }
        }
    }

    // Entry block must not have block params
    if (func.blockParams(func.entryBlock()).len > 0) {
        return error.EntryHasBlockParams;
    }
}

/// Validation errors
pub const SsaValidationError = error{
    VRegNotSequential,
    MultipleDefs,
    UseNotDominated,
    EmptyBlock,
    BlockNotTerminated,
    BranchArgMismatch,
    TerminatorInMiddle,
    EntryHasBlockParams,
    OutOfMemory,
};

//=============================================================================
// Tests
//=============================================================================

test "SSA validation - imports compile" {
    // Just verify the module compiles
    _ = Block;
    _ = Inst;
    _ = VReg;
    _ = Operand;
    _ = CFGInfo;
}
