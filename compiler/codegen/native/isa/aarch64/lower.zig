//! AArch64 instruction lowering.
//!
//! This module translates CLIF IR instructions to AArch64 machine instructions.
//! Port of cranelift/codegen/src/isa/aarch64/lower.rs and lower.isle
//!
//! The ISLE patterns from Cranelift are hand-translated to Zig switch statements.

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import CLIF IR types from machinst (which imports from compiler/ir/clif/)
// Must be imported early so MachLabel can be used below
const machinst = @import("../../machinst/mod.zig");
const lower_mod = machinst.lower;

// Import AArch64 instruction types from local modules
const inst_mod = @import("inst/mod.zig");
const Inst = inst_mod.Inst;
const ALUOp = inst_mod.ALUOp;
const ALUOp3 = inst_mod.ALUOp3;
const BitOp = inst_mod.BitOp;
const FPUOp1 = inst_mod.FPUOp1;
const FPUOp2 = inst_mod.FPUOp2;
const FpuToIntOp = inst_mod.FpuToIntOp;
const IntToFpuOp = inst_mod.IntToFpuOp;
const OperandSize = inst_mod.OperandSize;
const ScalarSize = inst_mod.ScalarSize;
const Cond = inst_mod.Cond;
const CondBrKind = inst_mod.CondBrKind;
const BranchTarget = inst_mod.BranchTarget;
const AMode = inst_mod.AMode;
const MemFlags = inst_mod.MemFlags;
const Imm12 = inst_mod.Imm12;
const MoveWideConst = inst_mod.MoveWideConst;
const MoveWideOp = inst_mod.MoveWideOp;
// MachLabel from machinst (not local args.zig) for compatibility with Lower(I)
const MachLabel = machinst.MachLabel;
const Type = inst_mod.Type;

// Import register types from local modules
const regs = inst_mod.regs;
const Reg = regs.Reg;
const PReg = regs.PReg;
const VReg = regs.VReg;
const Writable = regs.Writable;
const zeroReg = regs.zeroReg;
const writableZeroReg = regs.writableZeroReg;

// Import ArgPair and RetPair for argument/return value constraints
const ArgPair = inst_mod.ArgPair;
const RetPair = inst_mod.RetPair;

// =============================================================================
// CLIF IR Types
// Imported from machinst which imports from compiler/ir/clif/.
// =============================================================================

/// Opcode for CLIF instructions.
pub const Opcode = lower_mod.Opcode;

/// Integer condition codes.
pub const IntCC = lower_mod.IntCC;

/// Floating point condition codes.
pub const FloatCC = lower_mod.FloatCC;

/// CLIF instruction reference.
pub const ClifInst = lower_mod.Inst;

/// CLIF value reference.
pub const Value = lower_mod.Value;

/// Block index for lowered blocks.
pub const BlockIndex = lower_mod.BlockIndex;

/// Value registers - multiple registers for a single value.
pub const ValueRegs = lower_mod.ValueRegs;

/// Instruction output - vector of value register sets.
pub const InstOutput = lower_mod.InstOutput;

/// Non-register input information.
pub const NonRegInput = lower_mod.NonRegInput;

/// CLIF type - using the real Type from clif.
pub const ClifType = lower_mod.Type;

/// Instruction data from machinst.
/// Instruction data from dfg - the struct stored in dfg.insts.
pub const InstData = lower_mod.InstData;

/// Stack slot reference.
pub const StackSlot = lower_mod.StackSlot;

/// Stack slot data (size, alignment, etc.).
pub const StackSlotData = lower_mod.StackSlotData;

/// Argument purpose - for identifying special parameters like vmctx.
pub const ArgumentPurpose = lower_mod.ArgumentPurpose;

/// CLIF IR trap code (distinct from ARM64 backend u16 trap code).
const ClifTrapCode = lower_mod.TrapCode;

/// Map CLIF IR trap code to ARM64 backend u16 trap code.
/// Uses the CLIF enum's integer value directly.
fn clifToArm64TrapCode(clif_tc: ClifTrapCode) u16 {
    return @intFromEnum(clif_tc);
}

// =============================================================================
// Lower context
// This is the actual Lower(Inst) type from machinst/lower.zig.
// =============================================================================

/// Lowering context providing access to the function being lowered.
/// This is the real Lower(Inst) type, not a stub.
pub const LowerCtx = lower_mod.Lower(Inst);

// =============================================================================
// AArch64LowerBackend
// Port of cranelift/codegen/src/isa/aarch64/lower.rs
// =============================================================================

/// AArch64 lowering backend.
/// This implements the LowerBackend trait for AArch64.
pub const AArch64LowerBackend = struct {
    const Self = @This();

    /// The machine instruction type.
    pub const MInst = Inst;

    /// Settings for the backend.
    use_lse: bool = false,
    use_fp16: bool = false,

    pub fn init() Self {
        return .{};
    }

    /// Lower a single CLIF instruction to AArch64 machine instructions.
    /// This is the main entry point for instruction selection.
    ///
    /// Returns the output registers containing the result, or null if lowering failed.
    pub fn lower(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const inst_data = ctx.data(ir_inst);
        const opcode = inst_data.opcode();

        return switch (opcode) {
            // Integer constants
            .iconst => self.lowerIconst(ctx, ir_inst),

            // Integer arithmetic
            .iadd => self.lowerIadd(ctx, ir_inst),
            .isub => self.lowerIsub(ctx, ir_inst),
            .ineg => self.lowerIneg(ctx, ir_inst),
            .imul => self.lowerImul(ctx, ir_inst),
            .udiv => self.lowerUdiv(ctx, ir_inst),
            .sdiv => self.lowerSdiv(ctx, ir_inst),
            .urem => self.lowerUrem(ctx, ir_inst),
            .srem => self.lowerSrem(ctx, ir_inst),

            // Bitwise operations
            .band => self.lowerBand(ctx, ir_inst),
            .bor => self.lowerBor(ctx, ir_inst),
            .bxor => self.lowerBxor(ctx, ir_inst),
            .bnot => self.lowerBnot(ctx, ir_inst),
            .ishl => self.lowerIshl(ctx, ir_inst),
            .ushr => self.lowerUshr(ctx, ir_inst),
            .sshr => self.lowerSshr(ctx, ir_inst),
            .rotl => self.lowerRotl(ctx, ir_inst),
            .rotr => self.lowerRotr(ctx, ir_inst),

            // Integer comparison
            .icmp => self.lowerIcmp(ctx, ir_inst),

            // Floating point
            .f32const, .f64const => self.lowerFconst(ctx, ir_inst),
            .fadd => self.lowerFadd(ctx, ir_inst),
            .fsub => self.lowerFsub(ctx, ir_inst),
            .fmul => self.lowerFmul(ctx, ir_inst),
            .fdiv => self.lowerFdiv(ctx, ir_inst),
            .fneg => self.lowerFneg(ctx, ir_inst),
            .fabs => self.lowerFabs(ctx, ir_inst),
            .sqrt => self.lowerSqrt(ctx, ir_inst),
            .fcmp => self.lowerFcmp(ctx, ir_inst),

            // Conversions
            .uextend => self.lowerUextend(ctx, ir_inst),
            .sextend => self.lowerSextend(ctx, ir_inst),
            .ireduce => self.lowerIreduce(ctx, ir_inst),
            .fcvt_to_sint => self.lowerFcvtToSint(ctx, ir_inst),
            .fcvt_to_uint => self.lowerFcvtToUint(ctx, ir_inst),
            .fcvt_from_sint => self.lowerFcvtFromSint(ctx, ir_inst),
            .fcvt_from_uint => self.lowerFcvtFromUint(ctx, ir_inst),
            .fpromote => self.lowerFpromote(ctx, ir_inst),
            .fdemote => self.lowerFdemote(ctx, ir_inst),

            // Memory operations
            .load => self.lowerLoad(ctx, ir_inst),
            .store => self.lowerStore(ctx, ir_inst),

            // Select
            .select => self.lowerSelect(ctx, ir_inst),

            // Copy
            .copy => self.lowerCopy(ctx, ir_inst),

            // Nop (produces no value)
            .nop => {
                return InstOutput{};
            },

            // Return (handled by lowerBranch)
            .@"return" => null,

            // Branches (handled by lowerBranch)
            .jump, .brif, .br_table => null,

            // Calls
            .call => self.lowerCall(ctx, ir_inst),
            .call_indirect => self.lowerCallIndirect(ctx, ir_inst),

            // Traps
            .trap => self.lowerTrap(ctx, ir_inst),
            .trapnz => self.lowerTrapnz(ctx, ir_inst),
            .trapz => self.lowerTrapz(ctx, ir_inst),

            // Function address
            .func_addr => self.lowerFuncAddr(ctx, ir_inst),

            // Stack operations
            .stack_load => self.lowerStackLoad(ctx, ir_inst),
            .stack_store => self.lowerStackStore(ctx, ir_inst),

            // Global values
            // Port of cranelift/codegen/src/legalizer/globalvalue.rs
            .global_value => self.lowerGlobalValue(ctx, ir_inst),

            // Unhandled opcodes - return null to indicate lowering failed
            else => null,
        };
    }

    /// Lower a branch instruction.
    /// Returns null if the branch type is not supported.
    pub fn lowerBranch(
        self: *const Self,
        ctx: *LowerCtx,
        ir_inst: ClifInst,
        targets: []const MachLabel,
    ) ?void {
        const inst_data = ctx.data(ir_inst);
        const opcode = inst_data.opcode();

        switch (opcode) {
            .jump => {
                // Unconditional jump
                const target = if (targets.len > 0) targets[0] else return null;
                ctx.emit(Inst.genJump(target)) catch return null;
            },
            .brif => {
                // Conditional branch
                if (targets.len < 2) return null;
                const taken = targets[0];
                const not_taken = targets[1];

                // Get the condition value
                const cond_val = ctx.putInputInRegs(ir_inst, 0);
                const cond_reg = cond_val.onlyReg() orelse return null;

                // Compare with zero to set flags
                ctx.emit(Inst{
                    .alu_rr_imm12 = .{
                        .alu_op = .subs,
                        .size = .size64,
                        .rd = Writable(Reg).fromReg(zeroReg()),
                        .rn = cond_reg,
                        .imm12 = Imm12.zero(),
                    },
                }) catch return null;

                // Conditional branch based on result
                ctx.emit(Inst{
                    .cond_br = .{
                        .kind = .{ .cond = .ne },
                        .taken = .{ .label = taken },
                        .not_taken = .{ .label = not_taken },
                    },
                }) catch return null;
            },
            .br_table => {
                // Jump table lowering.
                // Port of cranelift/codegen/src/isa/aarch64/lower.rs br_table lowering
                //
                // Uses targets from collectBranchAndTargets (critical-edge-aware labels).
                // targets[0] = default label, targets[1..] = jump table entry labels.

                // Get the index operand (input 0)
                const idx_val = ctx.putInputInRegs(ir_inst, 0);
                const idx_reg = idx_val.onlyReg() orelse return null;

                // targets[0] = default, targets[1..] = table entries
                // Port of Cranelift jump_table_targets (isle.rs:759-769)
                if (targets.len == 0) return null;
                const default_label = targets[0];
                const table_size = targets.len - 1;

                // Allocate a copy of the target labels (ArrayList backing may be reallocated later)
                const jt_targets = ctx.allocator.dupe(MachLabel, targets[1..]) catch return null;

                // If table is empty, just jump to default
                if (table_size == 0) {
                    ctx.emit(Inst{
                        .jump = .{
                            .dest = .{ .label = default_label },
                        },
                    }) catch return null;
                    return;
                }

                // Allocate temp registers
                // allocTmp returns ValueRegs(Writable(Reg)), onlyReg returns Writable(Reg)
                const tmp1_regs = ctx.allocTmp(.I64) catch return null;
                const tmp1_wreg = tmp1_regs.onlyReg() orelse return null;

                const tmp2_regs = ctx.allocTmp(.I64) catch return null;
                const tmp2_wreg = tmp2_regs.onlyReg() orelse return null;

                // CRITICAL: Emit CMP instruction before jt_sequence to set flags
                // Port of Cranelift's two-rule pattern:
                //   Rule 1 (priority 0): imm12_from_u64 succeeds → CMP immediate
                //   Rule 2 (priority -1): fallback → load into register, CMP register
                if (Imm12.maybeFromU64(@intCast(table_size))) |imm12| {
                    // Fast path: table size fits in Imm12 (≤4095)
                    // Emit: CMP Widx, #table_size (SUBS WZR, Widx, #table_size)
                    ctx.emit(Inst{
                        .alu_rr_imm12 = .{
                            .alu_op = .subs,
                            .size = .size32,
                            .rd = writableZeroReg(),
                            .rn = idx_reg,
                            .imm12 = imm12,
                        },
                    }) catch return null;
                } else {
                    // Fallback: table size doesn't fit in Imm12
                    // Load table_size into a temp register, then CMP register-register
                    // Port of Cranelift: (cmp (OperandSize.Size32) ridx (imm $I64 jt_size))
                    const size_reg_pair = ctx.allocTmp(.I64) catch return null;
                    const size_wreg = size_reg_pair.onlyReg() orelse return null;

                    const mwc = MoveWideConst.maybeFromU64(@intCast(table_size)) orelse return null;
                    ctx.emit(Inst{
                        .mov_wide = .{
                            .op = .movz,
                            .rd = size_wreg,
                            .imm = mwc,
                            .size = .size32,
                        },
                    }) catch return null;

                    // Emit: CMP Widx, Wsize (SUBS WZR, Widx, Wsize)
                    ctx.emit(Inst{
                        .alu_rrr = .{
                            .alu_op = .subs,
                            .size = .size32,
                            .rd = writableZeroReg(),
                            .rn = idx_reg,
                            .rm = size_wreg.toReg(),
                        },
                    }) catch return null;
                }

                // Emit the jump table sequence using critical-edge-aware labels
                ctx.emit(Inst{
                    .jt_sequence = .{
                        .ridx = idx_reg,
                        .rtmp1 = tmp1_wreg,
                        .rtmp2 = tmp2_wreg,
                        .default = default_label,
                        .targets = jt_targets,
                    },
                }) catch return null;
            },
            .trap => {
                // Trap instruction - emit undefined instruction.
                // This is a terminator that causes a hardware trap.
                ctx.emit(Inst{
                    .udf = .{
                        .trap_code = 0,
                    },
                }) catch return null;
            },
            .@"return" => {
                // Port of Cranelift's gen_return pattern from lower.rs.
                // Instead of emitting explicit moves to return registers, we:
                // 1. Collect RetPair entries mapping vregs to physical return registers
                // 2. Emit Inst.rets with those pairs
                // 3. regalloc2 sees the reg_fixed_use constraints and inserts moves as needed

                const num_inputs = ctx.numInputs(ir_inst);

                // Allocate space for return pairs
                // ARM64 ABI: integer returns in x0, x1, x2, ... ; float returns in v0, v1, ...
                var ret_pairs = ctx.allocator.alloc(RetPair, num_inputs) catch return null;
                var pair_idx: usize = 0;
                var int_ret_idx: u8 = 0;
                var float_ret_idx: u8 = 0;

                for (0..num_inputs) |i| {
                    const ret_val = ctx.putInputInRegs(ir_inst, i);
                    const ret_reg = ret_val.onlyReg() orelse continue;
                    const ty = ctx.inputTy(ir_inst, i);

                    // Determine the physical register for this return value
                    const preg: PReg = if (ty.isFloat())
                        blk: {
                            const idx = float_ret_idx;
                            float_ret_idx += 1;
                            break :blk regs.vregPreg(idx);
                        }
                    else
                        blk: {
                            const idx = int_ret_idx;
                            int_ret_idx += 1;
                            break :blk regs.xregPreg(idx);
                        };

                    // Create RetPair mapping vreg to preg
                    // regalloc2 will see reg_fixed_use(vreg, preg) from get_operands
                    // and ensure vreg is in preg at this point, inserting moves as needed
                    ret_pairs[pair_idx] = RetPair{
                        .vreg = ret_reg,
                        .preg = preg,
                    };
                    pair_idx += 1;
                }

                // Emit the rets instruction with the return pairs
                // This is a pseudo-instruction that tells regalloc about the constraints
                // and emits as just a `ret` instruction
                ctx.emit(Inst{
                    .rets = .{
                        .rets = ret_pairs[0..pair_idx],
                    },
                }) catch return null;
            },
            else => return null,
        }
        _ = self;
    }

    /// Get the pinned register, if any.
    /// Returns machinst.Reg to match the lower.zig pinned_reg field type.
    /// Port of Cranelift's maybe_pinned_reg() from cranelift/codegen/src/isa/aarch64/lower.rs:130-131
    /// which returns Some(regs::pinned_reg()) = x21
    pub fn maybePinnedReg(_: *const Self) ?machinst.Reg {
        // Return x21 as the pinned register for vmctx
        // This tells the register allocator that get_pinned_reg results are in x21
        return regs.pinnedReg();
    }

    /// Generate the Args pseudo-instruction for function parameters.
    /// Port of Cranelift's gen_args pattern from abi.rs.
    /// This creates fixed register constraints (x0, x1, etc.) for function params.
    ///
    /// Also emits a mov from vmctx (x0) to the pinned register (x21).
    /// Port of Cranelift's set_pinned_reg pattern from lower.isle:2791-2792
    pub fn genArgSetup(_: *const Self, ctx: *LowerCtx) !void {
        // Get the entry block
        const entry_block = ctx.f.layout.entryBlock() orelse return;

        // Get the entry block's params (which are function parameters)
        const block_params = ctx.f.dfg.blockParams(entry_block);
        if (block_params.len == 0) return;

        // Check if we have a vmctx parameter using Function.specialParam
        // vmctx is typically the first integer parameter (x0)
        const has_vmctx = ctx.f.specialParam(.vmctx) != null;

        // Allocate ArgPairs for each function parameter
        var arg_pairs = try ctx.allocator.alloc(ArgPair, block_params.len);
        var int_arg_idx: u8 = 0;
        var float_arg_idx: u8 = 0;

        for (block_params, 0..) |param, i| {
            // Get the vreg assigned to this param
            const value_regs = ctx.value_regs.get(param);
            const vreg = value_regs.onlyReg() orelse continue;

            // Get the type to determine if it's float or int
            const ty = ctx.f.dfg.valueType(param);

            // Determine the physical register for this argument
            // Internal CC: integers in x0-x15, floats in v0-v7
            // x0-x15 are all caller-saved; x16-x17 reserved for linker (IP0/IP1)
            const preg: PReg = if (ty.isFloat())
                blk: {
                    std.debug.assert(float_arg_idx < 8);
                    const idx = float_arg_idx;
                    float_arg_idx += 1;
                    break :blk regs.vregPreg(idx);
                }
            else
                blk: {
                    std.debug.assert(int_arg_idx < 16);
                    const idx = int_arg_idx;
                    int_arg_idx += 1;
                    break :blk regs.xregPreg(idx);
                };

            // Create ArgPair: vreg (def) = preg (fixed physical register)
            arg_pairs[i] = ArgPair{
                .vreg = Writable(Reg).fromReg(vreg),
                .preg = preg,
            };

        }

        // If we have a vmctx parameter, move it to the pinned register (x21)
        // MUST happen BEFORE Args instruction so regalloc edits for Args
        // don't clobber x0 before we save vmctx.
        // Port of Cranelift's set_pinned_reg from lower.isle:2791-2792
        if (has_vmctx) {
            // vmctx is in x0 (first int arg), move to x21 (pinned reg)
            // orr x21, xzr, x0  (equivalent to mov x21, x0)
            try ctx.emit(Inst{
                .alu_rrr = .{
                    .alu_op = .orr,
                    .size = .size64,
                    .rd = Writable(Reg).fromReg(regs.pinnedReg()),
                    .rn = zeroReg(),
                    .rm = regs.xreg(0), // vmctx is in x0
                },
            });
        }

        // Emit the Args instruction (regalloc edits may move params around)
        try ctx.emit(Inst.genArgs(arg_pairs));
    }

    // =========================================================================
    // Integer constant lowering
    // ISLE: (rule iconst (lower (has_type ty (iconst (u64_from_imm64 n))))
    //            (imm ty (ImmExtend.Zero) n))
    // =========================================================================

    fn lowerIconst(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        // Get constant value - first try the constant map (populated during init),
        // then fall back to instruction data
        const value: u64 = ctx.getConstant(ir_inst) orelse blk: {
            const imm = ctx.data(ir_inst).getImmediate() orelse return null;
            break :blk @bitCast(imm);
        };

        // Allocate destination register
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // Emit move wide instruction(s) to load the constant
        if (MoveWideConst.maybeFromU64(value)) |imm| {
            // Simple case: fits in single movz
            ctx.emit(Inst{
                .mov_wide = .{
                    .op = .movz,
                    .rd = dst_reg,
                    .imm = imm,
                    .size = size,
                },
            }) catch return null;
        } else {
            // Larger constant: use movz for low bits, then movk for higher bits
            // Start with movz for bits 0-15
            ctx.emit(Inst{
                .mov_wide = .{
                    .op = .movz,
                    .rd = dst_reg,
                    .imm = MoveWideConst.maybeWithShift(@truncate(value & 0xFFFF), 0) orelse return null,
                    .size = size,
                },
            }) catch return null;

            // Add movk for bits 16-31 if non-zero
            const bits_16_31: u16 = @truncate((value >> 16) & 0xFFFF);
            if (bits_16_31 != 0) {
                ctx.emit(Inst{
                    .mov_wide = .{
                        .op = .movk,
                        .rd = dst_reg,
                        .imm = MoveWideConst.maybeWithShift(bits_16_31, 16) orelse return null,
                        .size = size,
                    },
                }) catch return null;
            }

            // Add movk for bits 32-47 if non-zero (64-bit only)
            if (size == .size64) {
                const bits_32_47: u16 = @truncate((value >> 32) & 0xFFFF);
                if (bits_32_47 != 0) {
                    ctx.emit(Inst{
                        .mov_wide = .{
                            .op = .movk,
                            .rd = dst_reg,
                            .imm = MoveWideConst.maybeWithShift(bits_32_47, 32) orelse return null,
                            .size = size,
                        },
                    }) catch return null;
                }

                // Add movk for bits 48-63 if non-zero
                const bits_48_63: u16 = @truncate((value >> 48) & 0xFFFF);
                if (bits_48_63 != 0) {
                    ctx.emit(Inst{
                        .mov_wide = .{
                            .op = .movk,
                            .rd = dst_reg,
                            .imm = MoveWideConst.maybeWithShift(bits_48_63, 48) orelse return null,
                            .size = size,
                        },
                    }) catch return null;
                }
            }
        }

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Integer addition lowering
    // ISLE patterns:
    //   (rule iadd_base_case -1 (lower (has_type (fits_in_64 ty) (iadd x y)))
    //         (add ty x y))
    //   (rule iadd_imm12_right 4 (lower ... (iadd x (imm12_from_value y)))
    //         (add_imm ty x y))
    // =========================================================================

    fn lowerIadd(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        // Get input registers
        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        // Allocate destination register
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // Check if we can use an immediate form
        const rhs_input = ctx.getInputAsSourceOrConst(ir_inst, 1);
        if (rhs_input.constant) |c| {
            if (Imm12.maybeFromU64(@intCast(c))) |imm12| {
                ctx.emit(Inst{
                    .alu_rr_imm12 = .{
                        .alu_op = .add,
                        .size = size,
                        .rd = dst_reg,
                        .rn = lhs_reg,
                        .imm12 = imm12,
                    },
                }) catch return null;

                var output = InstOutput{};
                output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
                return output;
            }
        }

        // Emit register-register add
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .add,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Integer subtraction
    // =========================================================================

    fn lowerIsub(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .sub,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Integer negation
    // =========================================================================

    fn lowerIneg(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // neg rd, rm  is  sub rd, xzr, rm
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .sub,
                .size = size,
                .rd = dst_reg,
                .rn = zeroReg(),
                .rm = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Integer multiplication
    // ISLE: (rule (lower (has_type (fits_in_64 ty) (imul x y)))
    //             (madd ty x y (zero_reg)))
    // =========================================================================

    fn lowerImul(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // mul rd, rn, rm  is  madd rd, rn, rm, xzr
        ctx.emit(Inst{
            .alu_rrrr = .{
                .alu_op = .madd,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
                .ra = zeroReg(),
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Unsigned division
    // =========================================================================

    fn lowerUdiv(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .udiv,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Signed division
    // =========================================================================

    fn lowerSdiv(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .sdiv,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Unsigned remainder (urem = a - (a / b) * b)
    // =========================================================================

    fn lowerUrem(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const tmp = ctx.allocTmp(ty) catch return null;
        const tmp_reg = tmp.onlyReg() orelse return null;

        // tmp = lhs / rhs
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .udiv,
                .size = size,
                .rd = tmp_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        // dst = lhs - tmp * rhs  (using msub: rd = ra - rn * rm)
        ctx.emit(Inst{
            .alu_rrrr = .{
                .alu_op = .msub,
                .size = size,
                .rd = dst_reg,
                .rn = tmp_reg.toReg(),
                .rm = rhs_reg,
                .ra = lhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Signed remainder
    // =========================================================================

    fn lowerSrem(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const tmp = ctx.allocTmp(ty) catch return null;
        const tmp_reg = tmp.onlyReg() orelse return null;

        // tmp = lhs / rhs (signed)
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .sdiv,
                .size = size,
                .rd = tmp_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        // dst = lhs - tmp * rhs
        ctx.emit(Inst{
            .alu_rrrr = .{
                .alu_op = .msub,
                .size = size,
                .rd = dst_reg,
                .rn = tmp_reg.toReg(),
                .rm = rhs_reg,
                .ra = lhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Bitwise AND
    // =========================================================================

    fn lowerBand(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .@"and",
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Bitwise OR
    // =========================================================================

    fn lowerBor(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .orr,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Bitwise XOR
    // =========================================================================

    fn lowerBxor(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .eor,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Bitwise NOT
    // =========================================================================

    fn lowerBnot(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // mvn rd, rm  is  orn rd, xzr, rm
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .orr_not,
                .size = size,
                .rd = dst_reg,
                .rn = zeroReg(),
                .rm = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Shift left
    // =========================================================================

    fn lowerIshl(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .lsl,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Unsigned shift right
    // =========================================================================

    fn lowerUshr(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .lsr,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Signed shift right
    // =========================================================================

    fn lowerSshr(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .asr,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Rotate left
    // =========================================================================

    fn lowerRotl(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // AArch64 doesn't have rotl directly, use rotr with negated shift
        // rotl(x, n) = rotr(x, -n) = rotr(x, width - n)
        const tmp = ctx.allocTmp(ty) catch return null;
        const tmp_reg = tmp.onlyReg() orelse return null;

        // tmp = -rhs
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .sub,
                .size = size,
                .rd = tmp_reg,
                .rn = zeroReg(),
                .rm = rhs_reg,
            },
        }) catch return null;

        // dst = ror(lhs, tmp)
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .extr, // ROR uses EXTR with Rm = Rn
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = tmp_reg.toReg(),
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Rotate right
    // =========================================================================

    fn lowerRotr(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // ROR uses EXTR where both source regs are the same
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .extr,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Integer comparison
    // =========================================================================

    fn lowerIcmp(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.inputTy(ir_inst, 0);
        const size = operandSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ClifType.I8) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // Compare (sets flags)
        ctx.emit(Inst{
            .alu_rrr = .{
                .alu_op = .subs,
                .size = size,
                .rd = Writable(Reg).fromReg(zeroReg()),
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        // Get the condition code from instruction data
        const inst_data = ctx.data(ir_inst);
        const intcc = inst_data.getIntCC() orelse return null;
        const cond = condFromIntCC(intcc);

        // Conditional set
        ctx.emit(Inst{
            .cset = .{
                .rd = dst_reg,
                .cond = cond,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Floating point constant
    // =========================================================================

    fn lowerFconst(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const fpu_size = scalarSizeFromType(ty) orelse return null;
        const data = ctx.data(ir_inst);

        // Get float constant bit pattern
        // Cranelift: f64const stores as unary_ieee64, f32const as unary_ieee32
        const is_f32 = ty.eql(ClifType.F32);
        const bits: u64 = if (is_f32)
            @as(u64, @as(u32, @bitCast(data.unary_ieee32.imm)))
        else
            @bitCast(data.unary_ieee64.imm);

        // Cranelift pattern (inst.isle:4071):
        //   (rule (lower (has_type $F64 (f64const (u64_from_ieee64 n))))
        //     (mov_to_fpu (imm $I64 (ImmExtend.Zero) n) (ScalarSize.Size64)))
        // Step 1: Load constant bits into GPR temp (integer movz/movk)
        const int_size: OperandSize = if (is_f32) .size32 else .size64;
        const int_ty = if (is_f32) ClifType.I32 else ClifType.I64;
        const gpr_tmp = ctx.allocTmp(int_ty) catch return null;
        const gpr_reg = gpr_tmp.onlyReg() orelse return null;

        // Emit movz + movk (same pattern as lowerIconst)
        if (MoveWideConst.maybeFromU64(bits)) |imm| {
            ctx.emit(Inst{ .mov_wide = .{
                .op = .movz,
                .rd = gpr_reg,
                .imm = imm,
                .size = int_size,
            } }) catch return null;
        } else {
            // Multi-instruction: movz for bits 0-15, movk for higher halfwords
            ctx.emit(Inst{ .mov_wide = .{
                .op = .movz,
                .rd = gpr_reg,
                .imm = MoveWideConst.maybeWithShift(@truncate(bits & 0xFFFF), 0) orelse return null,
                .size = int_size,
            } }) catch return null;

            const bits_16_31: u16 = @truncate((bits >> 16) & 0xFFFF);
            if (bits_16_31 != 0) {
                ctx.emit(Inst{ .mov_wide = .{
                    .op = .movk,
                    .rd = gpr_reg,
                    .imm = MoveWideConst.maybeWithShift(bits_16_31, 16) orelse return null,
                    .size = int_size,
                } }) catch return null;
            }

            if (int_size == .size64) {
                const bits_32_47: u16 = @truncate((bits >> 32) & 0xFFFF);
                if (bits_32_47 != 0) {
                    ctx.emit(Inst{ .mov_wide = .{
                        .op = .movk,
                        .rd = gpr_reg,
                        .imm = MoveWideConst.maybeWithShift(bits_32_47, 32) orelse return null,
                        .size = int_size,
                    } }) catch return null;
                }

                const bits_48_63: u16 = @truncate((bits >> 48) & 0xFFFF);
                if (bits_48_63 != 0) {
                    ctx.emit(Inst{ .mov_wide = .{
                        .op = .movk,
                        .rd = gpr_reg,
                        .imm = MoveWideConst.maybeWithShift(bits_48_63, 48) orelse return null,
                        .size = int_size,
                    } }) catch return null;
                }
            }
        }

        // Step 2: FMOV from GPR to FPU (Cranelift: MovToFpu)
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        ctx.emit(Inst{ .mov_to_fpu = .{
            .rd = dst_reg,
            .rn = gpr_reg.toReg(),
            .size = fpu_size,
        } }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Floating point operations
    // =========================================================================

    fn lowerFadd(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerFpuBinop(ctx, ir_inst, .add);
    }

    fn lowerFsub(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerFpuBinop(ctx, ir_inst, .sub);
    }

    fn lowerFmul(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerFpuBinop(ctx, ir_inst, .mul);
    }

    fn lowerFdiv(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerFpuBinop(ctx, ir_inst, .div);
    }

    fn lowerFpuBinop(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst, op: FPUOp2) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = scalarSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .fpu_rrr = .{
                .fpu_op = op,
                .size = size,
                .rd = dst_reg,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFneg(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerFpuUnaryOp(ctx, ir_inst, .neg);
    }

    fn lowerFabs(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerFpuUnaryOp(ctx, ir_inst, .abs);
    }

    fn lowerSqrt(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerFpuUnaryOp(ctx, ir_inst, .sqrt);
    }

    fn lowerFpuUnaryOp(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst, op: FPUOp1) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = scalarSizeFromType(ty) orelse return null;

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .fpu_rr = .{
                .fpu_op = op,
                .size = size,
                .rd = dst_reg,
                .rn = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcmp(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.inputTy(ir_inst, 0);
        const size = scalarSizeFromType(ty) orelse return null;

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ClifType.I8) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // FPU compare
        ctx.emit(Inst{
            .fpu_cmp = .{
                .size = size,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        // Get the float condition code from instruction data (Cranelift pattern)
        const inst_data = ctx.data(ir_inst);
        const floatcc = inst_data.getFloatCC() orelse return null;
        const cond = condFromFloatCC(floatcc);

        ctx.emit(Inst{
            .cset = .{
                .rd = dst_reg,
                .cond = cond,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Type conversions
    // =========================================================================

    fn lowerUextend(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const from_bits = src_ty.bits();
        const to_bits = dst_ty.bits();

        ctx.emit(Inst{
            .extend = .{
                .rd = dst_reg,
                .rn = src_reg,
                .signed = false,
                .from_bits = @intCast(from_bits),
                .to_bits = @intCast(to_bits),
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerSextend(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const from_bits = src_ty.bits();
        const to_bits = dst_ty.bits();

        ctx.emit(Inst{
            .extend = .{
                .rd = dst_reg,
                .rn = src_reg,
                .signed = true,
                .from_bits = @intCast(from_bits),
                .to_bits = @intCast(to_bits),
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerIreduce(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const dst_ty = ctx.outputTy(ir_inst, 0);

        // ireduce is just a move - the value is already in a register
        // The high bits are simply ignored
        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst{
            .mov = .{
                .size = .size64,
                .rd = dst_reg,
                .rm = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcvtToSint(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const op = fpuToIntOp(src_ty, dst_ty, true) orelse return null;

        ctx.emit(Inst{
            .fpu_to_int = .{
                .op = op,
                .rd = dst_reg,
                .rn = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcvtToUint(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const op = fpuToIntOp(src_ty, dst_ty, false) orelse return null;

        ctx.emit(Inst{
            .fpu_to_int = .{
                .op = op,
                .rd = dst_reg,
                .rn = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcvtFromSint(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const op = intToFpuOp(src_ty, dst_ty, true) orelse return null;

        ctx.emit(Inst{
            .int_to_fpu = .{
                .op = op,
                .rd = dst_reg,
                .rn = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcvtFromUint(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const op = intToFpuOp(src_ty, dst_ty, false) orelse return null;

        ctx.emit(Inst{
            .int_to_fpu = .{
                .op = op,
                .rd = dst_reg,
                .rn = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFpromote(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // f32 -> f64
        ctx.emit(Inst{
            .fpu_rr = .{
                .fpu_op = .cvt32_to_64,
                .size = .size64,
                .rd = dst_reg,
                .rn = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFdemote(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const dst_ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // f64 -> f32
        ctx.emit(Inst{
            .fpu_rr = .{
                .fpu_op = .cvt64_to_32,
                .size = .size32,
                .rd = dst_reg,
                .rn = src_reg,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Memory operations
    // =========================================================================

    fn lowerLoad(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);

        const addr = ctx.putInputInRegs(ir_inst, 0);
        const addr_reg = addr.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        const mem = AMode.reg(addr_reg);
        const flags = MemFlags.empty;

        ctx.emit(Inst.genLoad(dst_reg, mem, typeFromClif(ty), flags)) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerStore(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const val_ty = ctx.inputTy(ir_inst, 0);

        const val = ctx.putInputInRegs(ir_inst, 0);
        const addr = ctx.putInputInRegs(ir_inst, 1);
        const val_reg = val.onlyReg() orelse return null;
        const addr_reg = addr.onlyReg() orelse return null;

        const mem = AMode.reg(addr_reg);
        const flags = MemFlags.empty;

        ctx.emit(Inst.genStore(mem, val_reg, typeFromClif(val_ty), flags)) catch return null;

        // Stores have no output
        return InstOutput{};
    }

    // =========================================================================
    // Select
    // =========================================================================

    fn lowerSelect(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);

        const cond = ctx.putInputInRegs(ir_inst, 0);
        const if_true = ctx.putInputInRegs(ir_inst, 1);
        const if_false = ctx.putInputInRegs(ir_inst, 2);
        const cond_reg = cond.onlyReg() orelse return null;
        const if_true_reg = if_true.onlyReg() orelse return null;
        const if_false_reg = if_false.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // Compare cond with zero
        ctx.emit(Inst{
            .alu_rr_imm12 = .{
                .alu_op = .subs,
                .size = .size64,
                .rd = Writable(Reg).fromReg(zeroReg()),
                .rn = cond_reg,
                .imm12 = Imm12.zero(),
            },
        }) catch return null;

        // Conditional select: dst = (cond != 0) ? if_true : if_false
        ctx.emit(Inst{
            .csel = .{
                .rd = dst_reg,
                .rn = if_true_reg,
                .rm = if_false_reg,
                .cond = .ne,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Copy
    // =========================================================================

    fn lowerCopy(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);

        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        ctx.emit(Inst.genMove(dst_reg, src_reg, typeFromClif(ty))) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Calls
    // =========================================================================

    fn lowerCall(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        // Call lowering.
        // Port of cranelift/codegen/src/isa/aarch64/lower.isle:2533-2551
        //
        // AAPCS64 ABI:
        // - Integer/pointer arguments: X0-X7
        // - Float arguments: V0-V7
        // - Return value: X0 (integer), V0 (float)
        // - Caller-saved (clobbered): X0-X17, V0-V31
        //
        // Cranelift pattern: NO explicit mov instructions for register args/returns.
        // Instead, populate uses/defs with fixed register constraints.
        // Regalloc will insert moves as needed to satisfy constraints.
        _ = self;

        const inst_data = ctx.data(ir_inst);

        // Get the function reference
        const func_ref = inst_data.getFuncRef() orelse return null;

        // Get the external function data (contains the function name)
        const ext_func = ctx.extFuncData(func_ref) orelse return null;

        // Get arguments
        const num_args = ctx.numInputs(ir_inst);

        // Create CallInfo structure with the external function destination
        // Port of Cranelift's gen_call_info from aarch64/lower/isle.rs
        const call_info = ctx.allocator.create(inst_mod.CallInfo) catch return null;
        call_info.* = inst_mod.CallInfo.init(
            ext_func.name,
            .fast, // callee_conv
            .fast, // caller_conv
        );

        // Build AAPCS64 clobber set: X0-X17 and V0-V31 are caller-saved
        // Port of Cranelift's get_regs_clobbered_by_call
        var clobbers = inst_mod.PRegSet.empty();
        var i: u8 = 0;
        while (i <= 17) : (i += 1) {
            clobbers = clobbers.with(regs.xregPreg(i));
        }
        i = 0;
        while (i < 32) : (i += 1) {
            clobbers = clobbers.with(regs.vregPreg(i));
        }
        // NOTE: clobbers will be assigned to call_info AFTER removing return regs

        // Build uses list: gen_call_args(abi, args)
        // Each argument vreg is constrained to its ABI register (X0-X7 for integers)
        // Cranelift does NOT emit explicit mov instructions - regalloc handles it
        var int_arg_idx: u8 = 0;
        for (0..num_args) |idx| {
            const arg_val = ctx.putInputInRegs(ir_inst, idx);
            const arg_reg = arg_val.onlyReg() orelse continue;
            const arg_ty = ctx.inputTy(ir_inst, idx);

            if (!arg_ty.isFloat() and int_arg_idx < 16) {
                // Add to uses: this vreg MUST be in this preg at the call
                // Internal CC uses x0-x15 for integer args (x0-x7 standard AAPCS64,
                // x8-x15 extended for our Wasm-internal calling convention).
                // All x0-x15 are caller-saved and in clobber set.
                call_info.uses.append(ctx.allocator, .{
                    .vreg = arg_reg,
                    .preg = regs.xregPreg(int_arg_idx),
                }) catch return null;
                int_arg_idx += 1;
            }
        }

        // Build defs list: gen_call_rets(abi, output)
        // Each return value vreg is defined in its ABI register
        // Cranelift does NOT emit explicit mov instructions - regalloc handles it
        const num_outputs = ctx.numOutputs(ir_inst);
        var output = InstOutput{};
        if (num_outputs > 0) {
            const ret_ty = ctx.outputTy(ir_inst, 0);
            const dst = ctx.allocTmp(ret_ty) catch return null;
            const dst_reg = dst.onlyReg() orelse return null;

            // Add to defs: this vreg is DEFINED BY the call in X0
            const ret_preg = regs.xregPreg(0);
            call_info.defs.append(ctx.allocator, .{
                .vreg = dst_reg,
                .location = .{ .reg = ret_preg },
            }) catch return null;

            // Port of Cranelift's gen_call_info: remove return regs from clobbers.
            // regalloc2 requires that clobbers and defs must not collide.
            // See cranelift/codegen/src/machinst/abi.rs lines 2114-2119
            clobbers.remove(ret_preg);

            output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        }

        // Assign clobbers after removing return registers
        call_info.clobbers = clobbers;

        // Emit the call instruction with proper CallInfo
        // Regalloc sees uses/defs/clobbers and inserts moves as needed
        ctx.emit(Inst{
            .call = .{ .info = call_info },
        }) catch return null;

        return output;
    }

    fn lowerCallIndirect(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        // Indirect call lowering.
        // Port of cranelift/codegen/src/isa/aarch64/lower.isle:2542-2551
        // The callee address is the first input, remaining inputs are arguments.
        //
        // Internal calling convention (Wasm-internal):
        // - Integer/pointer arguments: X0-X15 (extended from AAPCS64 X0-X7)
        // - Float arguments: V0-V7
        // - Return value: X0 (integer), V0 (float)
        // - Caller-saved (clobbered): X0-X17, V0-V31
        //
        // Cranelift pattern: NO explicit mov instructions for register args/returns.
        // Instead, populate uses/defs with fixed register constraints.
        // Regalloc will insert moves as needed to satisfy constraints.
        _ = self;

        const num_inputs = ctx.numInputs(ir_inst);
        if (num_inputs == 0) return null;

        // Get the callee (first input)
        const callee_val = ctx.putInputInRegs(ir_inst, 0);
        const callee_reg = callee_val.onlyReg() orelse return null;

        // Create CallIndInfo structure for indirect call
        const call_ind_info = ctx.allocator.create(inst_mod.CallIndInfo) catch return null;
        call_ind_info.* = .{
            .dest = callee_reg,
            .uses = .{},
            .defs = .{},
            .clobbers = inst_mod.PRegSet.empty(),
            .callee_conv = .fast,
            .caller_conv = .fast,
            .try_call_info = null,
        };

        // Build clobber set: X0-X17 and V0-V31 are caller-saved
        // Port of Cranelift's get_regs_clobbered_by_call
        var clobbers = inst_mod.PRegSet.empty();
        var i: u8 = 0;
        while (i <= 17) : (i += 1) {
            clobbers = clobbers.with(regs.xregPreg(i));
        }
        i = 0;
        while (i < 32) : (i += 1) {
            clobbers = clobbers.with(regs.vregPreg(i));
        }
        // NOTE: clobbers will be assigned to call_ind_info AFTER removing return regs

        // Build uses list: gen_call_args(abi, args)
        // Skip first input which is callee, remaining inputs are arguments
        // Cranelift does NOT emit explicit mov instructions - regalloc handles it
        var int_arg_idx: u8 = 0;
        for (1..num_inputs) |idx| {
            const arg_val = ctx.putInputInRegs(ir_inst, idx);
            const arg_reg = arg_val.onlyReg() orelse continue;
            const arg_ty = ctx.inputTy(ir_inst, idx);

            if (!arg_ty.isFloat() and int_arg_idx < 16) {
                // Add to uses: this vreg MUST be in this preg at the call
                // Internal CC uses x0-x15 for integer args.
                call_ind_info.uses.append(ctx.allocator, .{
                    .vreg = arg_reg,
                    .preg = regs.xregPreg(int_arg_idx),
                }) catch return null;
                int_arg_idx += 1;
            }
        }

        // Build defs list: gen_call_rets(abi, output)
        // Each return value vreg is defined in its ABI register
        // Cranelift does NOT emit explicit mov instructions - regalloc handles it
        const num_outputs = ctx.numOutputs(ir_inst);
        var output = InstOutput{};
        if (num_outputs > 0) {
            const ret_ty = ctx.outputTy(ir_inst, 0);
            const dst = ctx.allocTmp(ret_ty) catch return null;
            const dst_reg = dst.onlyReg() orelse return null;

            // Add to defs: this vreg is DEFINED BY the call in X0
            const ret_preg = regs.xregPreg(0);
            call_ind_info.defs.append(ctx.allocator, .{
                .vreg = dst_reg,
                .location = .{ .reg = ret_preg },
            }) catch return null;

            // Port of Cranelift's gen_call_info: remove return regs from clobbers.
            // regalloc2 requires that clobbers and defs must not collide.
            // See cranelift/codegen/src/machinst/abi.rs lines 2114-2119
            clobbers.remove(ret_preg);

            output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        }

        // Assign clobbers after removing return registers
        call_ind_info.clobbers = clobbers;

        // Emit indirect call through the callee register
        // Regalloc sees uses/defs/clobbers and inserts moves as needed
        ctx.emit(Inst{
            .call_ind = .{ .info = call_ind_info },
        }) catch return null;

        return output;
    }

    // =========================================================================
    // Traps
    // =========================================================================

    fn lowerTrap(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;

        const trap_code = clifToArm64TrapCode(ctx.data(ir_inst).getTrapCode() orelse .unreachable_code_reached);

        ctx.emit(Inst{
            .udf = .{
                .trap_code = trap_code,
            },
        }) catch return null;

        return InstOutput{};
    }

    fn lowerTrapnz(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;

        const cond = ctx.putInputInRegs(ir_inst, 0);
        const cond_reg = cond.onlyReg() orelse return null;

        const trap_code = clifToArm64TrapCode(ctx.data(ir_inst).getTrapCode() orelse .heap_out_of_bounds);

        // Compare with zero
        ctx.emit(Inst{
            .alu_rr_imm12 = .{
                .alu_op = .subs,
                .size = .size64,
                .rd = Writable(Reg).fromReg(zeroReg()),
                .rn = cond_reg,
                .imm12 = Imm12.zero(),
            },
        }) catch return null;

        // Trap if not zero
        ctx.emit(Inst{
            .trap_if = .{
                .kind = .{ .cond = .ne },
                .trap_code = trap_code,
            },
        }) catch return null;

        return InstOutput{};
    }

    fn lowerTrapz(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;

        const cond = ctx.putInputInRegs(ir_inst, 0);
        const cond_reg = cond.onlyReg() orelse return null;

        const trap_code = clifToArm64TrapCode(ctx.data(ir_inst).getTrapCode() orelse .heap_out_of_bounds);

        // Compare with zero
        ctx.emit(Inst{
            .alu_rr_imm12 = .{
                .alu_op = .subs,
                .size = .size64,
                .rd = Writable(Reg).fromReg(zeroReg()),
                .rn = cond_reg,
                .imm12 = Imm12.zero(),
            },
        }) catch return null;

        // Trap if zero
        ctx.emit(Inst{
            .trap_if = .{
                .kind = .{ .cond = .eq },
                .trap_code = trap_code,
            },
        }) catch return null;

        return InstOutput{};
    }

    // =========================================================================
    // Function address
    // =========================================================================

    fn lowerFuncAddr(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        _ = ir_inst;

        const dst = ctx.allocTmp(ClifType.I64) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // TODO: Load actual function address
        // This is a placeholder using adr
        // TODO: Load actual function address from GOT
        // For now, use a placeholder pc-relative offset
        ctx.emit(Inst{
            .adr = .{
                .rd = dst_reg,
                .label = .{ .pc_rel = 0 },
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Stack operations
    // =========================================================================

    fn lowerStackLoad(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // Get stack slot from instruction data
        const inst_data = ctx.data(ir_inst);
        const slot = inst_data.getStackSlot() orelse return null;
        const extra_offset = inst_data.getOffset() orelse 0;

        // Get the slot's byte offset from computed stackslot offsets
        // Port of cranelift/codegen/src/machinst/abi.rs Callee::sized_stackslot_offsets
        const slot_base_offset = ctx.sizedStackslotOffset(slot);
        const slot_byte_offset = @as(i32, @intCast(slot_base_offset)) + extra_offset;

        const mem = AMode{ .sp_offset = .{ .offset = slot_byte_offset } };
        const flags = MemFlags.empty;

        ctx.emit(Inst.genLoad(dst_reg, mem, typeFromClif(ty), flags)) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerStackStore(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const val_ty = ctx.inputTy(ir_inst, 0);

        const val = ctx.putInputInRegs(ir_inst, 0);
        const val_reg = val.onlyReg() orelse return null;

        // Get stack slot from instruction data
        const inst_data = ctx.data(ir_inst);
        const slot = inst_data.getStackSlot() orelse return null;
        const extra_offset = inst_data.getOffset() orelse 0;

        // Get the slot's byte offset from computed stackslot offsets
        // Port of cranelift/codegen/src/machinst/abi.rs Callee::sized_stackslot_offsets
        const slot_base_offset = ctx.sizedStackslotOffset(slot);
        const slot_byte_offset = @as(i32, @intCast(slot_base_offset)) + extra_offset;

        const mem = AMode{ .sp_offset = .{ .offset = slot_byte_offset } };
        const flags = MemFlags.empty;

        ctx.emit(Inst.genStore(mem, val_reg, typeFromClif(val_ty), flags)) catch return null;

        return InstOutput{};
    }

    /// Lower a global_value instruction.
    ///
    /// Port of cranelift/codegen/src/legalizer/globalvalue.rs expand_global_value
    ///
    /// GlobalValue is expanded based on its GlobalValueData:
    /// - VMContext: Load from vmctx parameter/register
    /// - IAddImm: Recursively expand base, then add offset
    /// - Load: Recursively expand base, then load from offset
    /// - Symbol: Load symbol address (requires relocation)
    fn lowerGlobalValue(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const ty = ctx.outputTy(ir_inst, 0);

        // Get the GlobalValue reference from instruction data
        const inst_data = ctx.data(ir_inst);
        const gv = inst_data.getGlobalValue() orelse return null;

        // Get GlobalValueData from function
        const gv_data = ctx.globalValueData(gv) orelse return null;

        // Allocate destination register
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        switch (gv_data) {
            .vmcontext => {
                // VMContext: Return the pinned register (x21) directly.
                // Port of Cranelift's get_pinned_reg from lower.isle:2788-2789
                // which returns mov_from_preg(preg_pinned) = x21
                //
                // At function entry, vmctx is moved from x0 into x21.
                // x21 is excluded from register allocation, so it can't be clobbered.
                // This ensures vmctx is always available regardless of control flow.
                //
                // Copy x21 to the destination register.
                // orr dst, xzr, x21  (equivalent to mov dst, x21)
                ctx.emit(Inst{
                    .alu_rrr = .{
                        .alu_op = .orr,
                        .size = .size64,
                        .rd = dst_reg,
                        .rn = zeroReg(),
                        .rm = regs.pinnedReg(), // x21 = pinned register
                    },
                }) catch return null;
            },

            .iadd_imm => |d| {
                // IAddImm: Recursively expand base, then add offset
                // Port of cranelift/codegen/src/legalizer/globalvalue.rs iadd_imm_addr
                const base_addr = self.materializeGlobalValue(ctx, d.base, ty) orelse return null;

                // Add the offset
                if (d.offset == 0) {
                    // No offset, just copy
                    ctx.emit(Inst{
                        .alu_rrr = .{
                            .alu_op = .orr,
                            .size = .size64,
                            .rd = dst_reg,
                            .rn = zeroReg(),
                            .rm = base_addr,
                        },
                    }) catch return null;
                } else if (Imm12.maybeFromU64(@as(u64, @intCast(@abs(d.offset))))) |imm12| {
                    // Offset fits in imm12
                    if (d.offset >= 0) {
                        ctx.emit(Inst{
                            .alu_rr_imm12 = .{
                                .alu_op = .add,
                                .size = .size64,
                                .rd = dst_reg,
                                .rn = base_addr,
                                .imm12 = imm12,
                            },
                        }) catch return null;
                    } else {
                        ctx.emit(Inst{
                            .alu_rr_imm12 = .{
                                .alu_op = .sub,
                                .size = .size64,
                                .rd = dst_reg,
                                .rn = base_addr,
                                .imm12 = imm12,
                            },
                        }) catch return null;
                    }
                } else {
                    // Offset doesn't fit in imm12, need to load into temp register
                    const tmp = ctx.allocTmp(ClifType.I64) catch return null;
                    const tmp_reg = tmp.onlyReg() orelse return null;

                    const offset_val: u64 = @bitCast(d.offset);
                    emitLoadConstant(ctx, tmp_reg, offset_val, .size64) catch return null;

                    ctx.emit(Inst{
                        .alu_rrr = .{
                            .alu_op = .add,
                            .size = .size64,
                            .rd = dst_reg,
                            .rn = base_addr,
                            .rm = tmp_reg.toReg(),
                        },
                    }) catch return null;
                }
            },

            .load => |d| {
                // Load: Recursively expand base, then load from offset
                // Port of cranelift/codegen/src/legalizer/globalvalue.rs load_addr
                const base_addr = self.materializeGlobalValue(ctx, d.base, ClifType.I64) orelse return null;

                // Load from base + offset
                const mem = AMode{ .reg_offset = .{
                    .rn = base_addr,
                    .offset = d.offset,
                    .ty = d.global_type,
                } };
                ctx.emit(Inst.genLoad(dst_reg, mem, d.global_type, MemFlags.empty)) catch return null;
            },

            .symbol => {
                // Symbol: For now, just load a placeholder address
                // Full implementation would emit relocation
                // Port of cranelift/codegen/src/legalizer/globalvalue.rs symbol
                const placeholder_addr: u64 = 0;
                emitLoadConstant(ctx, dst_reg, placeholder_addr, .size64) catch return null;
            },

            .dyn_scale_target_const => {
                // Dynamic vector scale - emit constant 1 for now
                emitLoadConstant(ctx, dst_reg, 1, .size64) catch return null;
            },
        }

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    /// Helper to recursively materialize a GlobalValue into a register.
    /// Returns the register containing the computed address.
    fn materializeGlobalValue(self: *const Self, ctx: *LowerCtx, gv: lower_mod.GlobalValue, ty: ClifType) ?Reg {
        const gv_data = ctx.globalValueData(gv) orelse return null;

        const tmp = ctx.allocTmp(ty) catch return null;
        const tmp_reg = tmp.onlyReg() orelse return null;

        switch (gv_data) {
            .vmcontext => {
                // VMContext: Return the pinned register (x21) directly.
                // Port of Cranelift's get_pinned_reg from lower.isle:2788-2789
                // At function entry, vmctx is moved from x0 into x21.
                // x21 is excluded from register allocation, so it can't be clobbered.
                //
                // Copy x21 to the temp register.
                ctx.emit(Inst{
                    .alu_rrr = .{
                        .alu_op = .orr,
                        .size = .size64,
                        .rd = tmp_reg,
                        .rn = zeroReg(),
                        .rm = regs.pinnedReg(), // x21 = pinned register
                    },
                }) catch return null;
                return tmp_reg.toReg();
            },
            .iadd_imm => |d| {
                const base_addr = self.materializeGlobalValue(ctx, d.base, ty) orelse return null;

                if (d.offset == 0) {
                    return base_addr;
                } else if (Imm12.maybeFromU64(@as(u64, @intCast(@abs(d.offset))))) |imm12| {
                    if (d.offset >= 0) {
                        ctx.emit(Inst{
                            .alu_rr_imm12 = .{
                                .alu_op = .add,
                                .size = .size64,
                                .rd = tmp_reg,
                                .rn = base_addr,
                                .imm12 = imm12,
                            },
                        }) catch return null;
                    } else {
                        ctx.emit(Inst{
                            .alu_rr_imm12 = .{
                                .alu_op = .sub,
                                .size = .size64,
                                .rd = tmp_reg,
                                .rn = base_addr,
                                .imm12 = imm12,
                            },
                        }) catch return null;
                    }
                } else {
                    const offset_val: u64 = @bitCast(d.offset);
                    emitLoadConstant(ctx, tmp_reg, offset_val, .size64) catch return null;

                    ctx.emit(Inst{
                        .alu_rrr = .{
                            .alu_op = .add,
                            .size = .size64,
                            .rd = tmp_reg,
                            .rn = base_addr,
                            .rm = tmp_reg.toReg(),
                        },
                    }) catch return null;
                }
                return tmp_reg.toReg();
            },
            .load => |d| {
                const base_addr = self.materializeGlobalValue(ctx, d.base, ClifType.I64) orelse return null;
                const mem = AMode{ .reg_offset = .{
                    .rn = base_addr,
                    .offset = d.offset,
                    .ty = d.global_type,
                } };
                ctx.emit(Inst.genLoad(tmp_reg, mem, d.global_type, MemFlags.empty)) catch return null;
                return tmp_reg.toReg();
            },
            .symbol, .dyn_scale_target_const => {
                emitLoadConstant(ctx, tmp_reg, 0, .size64) catch return null;
                return tmp_reg.toReg();
            },
        }
    }
};

// =============================================================================
// Helper functions
// =============================================================================

/// Emit instructions to load a 64-bit constant into a register.
/// Uses movz/movk sequence for constants that don't fit in a single immediate.
fn emitLoadConstant(ctx: *LowerCtx, dst: Writable(Reg), value: u64, size: OperandSize) !void {
    if (MoveWideConst.maybeFromU64(value)) |imm| {
        // Simple case: fits in single movz
        try ctx.emit(Inst{
            .mov_wide = .{
                .op = .movz,
                .rd = dst,
                .imm = imm,
                .size = size,
            },
        });
    } else {
        // Use movz for bits 0-15, then movk for higher bits
        try ctx.emit(Inst{
            .mov_wide = .{
                .op = .movz,
                .rd = dst,
                .imm = MoveWideConst.maybeWithShift(@truncate(value & 0xFFFF), 0) orelse return error.InvalidConstant,
                .size = size,
            },
        });

        const bits_16_31: u16 = @truncate((value >> 16) & 0xFFFF);
        if (bits_16_31 != 0) {
            try ctx.emit(Inst{
                .mov_wide = .{
                    .op = .movk,
                    .rd = dst,
                    .imm = MoveWideConst.maybeWithShift(bits_16_31, 16) orelse return error.InvalidConstant,
                    .size = size,
                },
            });
        }

        const bits_32_47: u16 = @truncate((value >> 32) & 0xFFFF);
        if (bits_32_47 != 0) {
            try ctx.emit(Inst{
                .mov_wide = .{
                    .op = .movk,
                    .rd = dst,
                    .imm = MoveWideConst.maybeWithShift(bits_32_47, 32) orelse return error.InvalidConstant,
                    .size = size,
                },
            });
        }

        const bits_48_63: u16 = @truncate((value >> 48) & 0xFFFF);
        if (bits_48_63 != 0) {
            try ctx.emit(Inst{
                .mov_wide = .{
                    .op = .movk,
                    .rd = dst,
                    .imm = MoveWideConst.maybeWithShift(bits_48_63, 48) orelse return error.InvalidConstant,
                    .size = size,
                },
            });
        }
    }
}

/// Convert CLIF type to AArch64 operand size.
fn operandSizeFromType(ty: ClifType) ?OperandSize {
    const bits = ty.bits();
    if (bits <= 32) return .size32;
    if (bits <= 64) return .size64;
    return null;
}

/// Convert CLIF type to AArch64 scalar size (for FPU operations).
fn scalarSizeFromType(ty: ClifType) ?ScalarSize {
    const bits = ty.bits();
    if (bits == 32) return .size32;
    if (bits == 64) return .size64;
    return null;
}

/// Convert ClifType to inst_mod.Type.
/// Since both are now the same CLIF Type, just return the input.
fn typeFromClif(ty: ClifType) Type {
    return ty;
}

/// Determine the FPU to int conversion operation.
fn fpuToIntOp(src_ty: ClifType, dst_ty: ClifType, signed: bool) ?FpuToIntOp {
    const src_bits = src_ty.bits();
    const dst_bits = dst_ty.bits();

    if (src_bits == 32) {
        if (dst_bits <= 32) {
            return if (signed) .f32_to_i32 else .f32_to_u32;
        } else {
            return if (signed) .f32_to_i64 else .f32_to_u64;
        }
    } else if (src_bits == 64) {
        if (dst_bits <= 32) {
            return if (signed) .f64_to_i32 else .f64_to_u32;
        } else {
            return if (signed) .f64_to_i64 else .f64_to_u64;
        }
    }
    return null;
}

/// Determine the int to FPU conversion operation.
fn intToFpuOp(src_ty: ClifType, dst_ty: ClifType, signed: bool) ?IntToFpuOp {
    const src_bits = src_ty.bits();
    const dst_bits = dst_ty.bits();

    if (dst_bits == 32) {
        if (src_bits <= 32) {
            return if (signed) .i32_to_f32 else .u32_to_f32;
        } else {
            return if (signed) .i64_to_f32 else .u64_to_f32;
        }
    } else if (dst_bits == 64) {
        if (src_bits <= 32) {
            return if (signed) .i32_to_f64 else .u32_to_f64;
        } else {
            return if (signed) .i64_to_f64 else .u64_to_f64;
        }
    }
    return null;
}

/// Convert IntCC to AArch64 condition code.
pub fn condFromIntCC(cc: IntCC) Cond {
    return switch (cc) {
        .eq => .eq,
        .ne => .ne,
        .slt => .lt,
        .sge => .ge,
        .sgt => .gt,
        .sle => .le,
        .ult => .lo,
        .uge => .hs,
        .ugt => .hi,
        .ule => .ls,
    };
}

/// Convert FloatCC to AArch64 condition code.
/// Note: Some FP comparisons require additional handling for NaN.
/// Ported from Cranelift: cranelift/codegen/src/isa/aarch64/lower.rs lower_fp_condcode
/// After ARM64 `fcmp`, NZCV flags have FP-specific meanings:
///   N=1 (mi) → less than (ordered)
///   Z=1 (eq) → equal
///   C=0 (cc) → unordered or less than
///   V=1 (vs) → unordered (NaN)
pub fn condFromFloatCC(cc: FloatCC) Cond {
    return switch (cc) {
        .eq => .eq, // Equal (Z=1)
        .ne => .ne, // Not equal (Z=0)
        .lt => .mi, // Less than, ordered (N=1)
        .le => .ls, // Less or equal, ordered (C=0 or Z=1)
        .gt => .gt, // Greater than (Z=0 and N=V)
        .ge => .ge, // Greater or equal (N=V)
        .ord => .vc, // Ordered, no NaN (V=0)
        .uno => .vs, // Unordered, NaN (V=1)
        else => .eq,
    };
}

// =============================================================================
// Tests
// =============================================================================

test "AArch64LowerBackend basic" {
    const backend = AArch64LowerBackend.init();
    _ = backend;
}

test "operandSizeFromType" {
    const testing = std.testing;

    try testing.expectEqual(OperandSize.size32, operandSizeFromType(ClifType.I8).?);
    try testing.expectEqual(OperandSize.size32, operandSizeFromType(ClifType.I16).?);
    try testing.expectEqual(OperandSize.size32, operandSizeFromType(ClifType.I32).?);
    try testing.expectEqual(OperandSize.size64, operandSizeFromType(ClifType.I64).?);
}

test "scalarSizeFromType" {
    const testing = std.testing;

    try testing.expectEqual(ScalarSize.size32, scalarSizeFromType(ClifType.F32).?);
    try testing.expectEqual(ScalarSize.size64, scalarSizeFromType(ClifType.F64).?);
}

test "condFromIntCC" {
    const testing = std.testing;

    try testing.expectEqual(Cond.eq, condFromIntCC(.eq));
    try testing.expectEqual(Cond.ne, condFromIntCC(.ne));
    try testing.expectEqual(Cond.lt, condFromIntCC(.slt));
    try testing.expectEqual(Cond.lo, condFromIntCC(.ult));
}
