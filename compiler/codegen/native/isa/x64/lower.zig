//! x86-64 instruction lowering.
//!
//! This module translates CLIF IR instructions to x86-64 machine instructions.
//! Port of cranelift/codegen/src/isa/x64/lower.rs and lower.isle
//!
//! The ISLE patterns from Cranelift are hand-translated to Zig switch statements.

const std = @import("std");
const Allocator = std.mem.Allocator;

// Import CLIF IR types from machinst (which imports from compiler/ir/clif/)
// Must be imported early so MachLabel can be used below
const machinst = @import("../../machinst/mod.zig");
const lower_mod = machinst.lower;

// Import x86-64 instruction types from local modules
const inst_mod = @import("inst/mod.zig");
const Inst = inst_mod.Inst;
const AluRmiROpcode = inst_mod.AluRmiROpcode;
const UnaryRmROpcode = inst_mod.UnaryRmROpcode;
const ShiftKind = inst_mod.args.ShiftKind;
const OperandSize = inst_mod.args.OperandSize;
const CC = inst_mod.args.CC;
const ExtMode = inst_mod.args.ExtMode;
const Amode = inst_mod.args.Amode;
const SyntheticAmode = inst_mod.args.SyntheticAmode;
const MemFlags = inst_mod.args.MemFlags;
// MachLabel from machinst (not local args.zig) for compatibility with Lower(I)
const MachLabel = machinst.MachLabel;
const Type = inst_mod.args.Type;
const GprMem = inst_mod.args.GprMem;
const GprMemImm = inst_mod.args.GprMemImm;
const XmmMem = inst_mod.args.XmmMem;
const RegMem = inst_mod.args.RegMem;
const RegMemImm = inst_mod.args.RegMemImm;
const SseOpcode = inst_mod.SseOpcode;
const TrapCode = inst_mod.TrapCode;

// Import register types from local modules
const regs = inst_mod.regs;
const Reg = inst_mod.args.Reg;
const VReg = inst_mod.args.VReg;
const Writable = inst_mod.args.Writable;
const Gpr = inst_mod.args.Gpr;
const WritableGpr = inst_mod.args.WritableGpr;
const Xmm = inst_mod.args.Xmm;
const WritableXmm = inst_mod.args.WritableXmm;
const GprEnc = regs.GprEnc;
const PReg = inst_mod.PReg;
const CallArgPair = inst_mod.CallArgPair;
const CallInfo = inst_mod.CallInfo;
const CallRetList = inst_mod.CallRetList;
const CallRetPair = inst_mod.CallRetPair;
const PRegSet = inst_mod.PRegSet;
const RetLocation = inst_mod.RetLocation;
const ExternalName = inst_mod.ExternalName;
const CallArgList = inst_mod.args.CallArgList;

// Import ABI module
const abi = @import("abi.zig");
const CallConv = abi.CallConv;
const RegClass = inst_mod.args.RegClass;

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

/// Jump table reference.
pub const JumpTable = lower_mod.JumpTable;

/// Jump table data.
pub const JumpTableData = lower_mod.JumpTableData;

/// Stack slot reference.
pub const StackSlot = lower_mod.StackSlot;

/// Stack slot data (size, alignment, etc.).
pub const StackSlotData = lower_mod.StackSlotData;

/// Value registers - multiple registers for a single value.
pub const ValueRegs = lower_mod.ValueRegs;

/// Instruction output - vector of value register sets.
pub const InstOutput = lower_mod.InstOutput;

/// Non-register input information.
pub const NonRegInput = lower_mod.NonRegInput;

/// CLIF type - using the real Type from clif.
pub const ClifType = lower_mod.Type;

/// Instruction data from dfg - the struct stored in dfg.insts.
pub const InstData = lower_mod.InstData;

// =============================================================================
// Lower context
// This is the actual Lower(Inst) type from machinst/lower.zig.
// =============================================================================

/// Lowering context providing access to the function being lowered.
/// This is the real Lower(Inst) type, not a stub.
pub const LowerCtx = lower_mod.Lower(Inst);

// =============================================================================
// X64LowerBackend
// Port of cranelift/codegen/src/isa/x64/lower.rs
// =============================================================================

/// x86-64 lowering backend.
/// This implements the LowerBackend trait for x86-64.
pub const X64LowerBackend = struct {
    const Self = @This();

    /// The machine instruction type.
    pub const MInst = Inst;

    /// Calling convention.
    call_conv: CallConv = .system_v,
    /// Whether to use SSE4.1 instructions.
    use_sse41: bool = true,
    /// Whether to use SSE4.2 instructions.
    use_sse42: bool = true,
    /// Whether to use AVX instructions.
    use_avx: bool = false,
    /// Whether to use BMI1 instructions.
    use_bmi1: bool = false,
    /// Whether to use BMI2 instructions.
    use_bmi2: bool = false,
    /// Whether to use LZCNT.
    use_lzcnt: bool = false,
    /// Whether to use POPCNT.
    use_popcnt: bool = false,

    pub fn init() Self {
        return .{};
    }

    /// Get the pinned register, if any.
    /// Returns machinst.Reg to match the lower.zig pinned_reg field type.
    /// Port of Cranelift's maybe_pinned_reg() which returns r15 for x64.
    pub fn maybePinnedReg(_: *const Self) ?machinst.Reg {
        // Return r15 as the pinned register for vmctx
        // This tells the register allocator that get_pinned_reg results are in r15
        return regs.pinnedReg();
    }

    /// Lower a single CLIF instruction to x86-64 machine instructions.
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
            .bitcast => self.lowerBitcast(ctx, ir_inst),

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
        _ = self;
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
                const cond_gpr = Gpr.unwrapNew(cond_reg);

                // Compare with zero using TEST (more efficient than CMP for this case)
                ctx.emit(Inst{
                    .test_rmi_r = .{
                        .size = .size64,
                        .src = GprMemImm.unwrapNew(RegMemImm.fromReg(cond_gpr.toReg())),
                        .dst = cond_gpr,
                    },
                }) catch return null;

                // Conditional jump (jump to taken if not zero)
                ctx.emit(Inst{
                    .jmp_cond = .{
                        .cc = .nz,
                        .taken = taken,
                        .not_taken = not_taken,
                    },
                }) catch return null;

                // jmp_cond already handles both branches, no fallthrough needed
            },
            .br_table => {
                // Jump table lowering.
                // Port of cranelift/codegen/src/isa/x64/lower.rs br_table lowering
                //
                // The br_table instruction implements an indirect branch using a jump table.
                // We emit:
                //   cmp idx, table_size
                //   jae default_label
                //   jmp_table_seq idx, targets, default

                // Get the index operand (input 0)
                const idx_val = ctx.putInputInRegs(ir_inst, 0);
                const idx_reg = idx_val.onlyReg() orelse return null;

                // Get the jump table reference
                const jt_ref = inst_data.getJumpTable() orelse return null;

                // Look up the jump table data
                const jt_data = ctx.jumpTableData(jt_ref) orelse return null;

                // Get default target label
                const default_block = jt_data.getDefaultBlock().getBlock();
                const default_label = ctx.blockLabel(default_block) orelse return null;

                // Get table size
                const table_entries = jt_data.asSlice();
                const table_size = table_entries.len;

                // If table is empty, just jump to default
                if (table_size == 0) {
                    ctx.emit(Inst.genJump(default_label)) catch return null;
                    return;
                }

                // Allocate and populate targets array
                const target_labels = ctx.allocator.alloc(MachLabel, table_size) catch return null;
                for (table_entries, 0..) |entry, i| {
                    const target_block = entry.getBlock();
                    target_labels[i] = ctx.blockLabel(target_block) orelse return null;
                }

                // Allocate temp registers
                // allocTmp returns ValueRegs(Writable(Reg)), onlyReg returns Writable(Reg)
                const tmp1_regs = ctx.allocTmp(.I64) catch return null;
                const tmp1_wreg = tmp1_regs.onlyReg() orelse return null;

                const tmp2_regs = ctx.allocTmp(.I64) catch return null;
                const tmp2_wreg = tmp2_regs.onlyReg() orelse return null;

                // Emit bounds check: cmp idx, table_size
                const idx_gpr = Gpr.unwrapNew(idx_reg);
                ctx.emit(Inst{
                    .cmp_rmi_r = .{
                        .size = .size64,
                        .src = GprMemImm.unwrapNew(RegMemImm.fromImm(@intCast(table_size))),
                        .dst = idx_gpr,
                    },
                }) catch return null;

                // Emit conditional jump to default if out of bounds: jae default
                // Using winch_jmp_if which is a one-way conditional jump (no not_taken)
                // Note: .nb = "not below" = CF=0 = unsigned >= (same as jae)
                ctx.emit(Inst{
                    .winch_jmp_if = .{
                        .cc = .nb, // jae = jnb: jump if not below (unsigned >=)
                        .taken = default_label,
                    },
                }) catch return null;

                // Emit the jump table sequence
                ctx.emit(Inst{
                    .jmp_table_seq = .{
                        .idx = idx_reg,
                        .tmp1 = tmp1_wreg,
                        .tmp2 = tmp2_wreg,
                        .default = default_label,
                        .targets = target_labels,
                    },
                }) catch return null;
            },
            .@"return" => {
                // Port of Cranelift's gen_return pattern from lower.rs.
                // Instead of emitting explicit moves to return registers, we:
                // 1. Collect CallArgPair entries mapping vregs to physical return registers
                // 2. Emit Inst.rets with those pairs
                // 3. regalloc2 sees the reg_fixed_use constraints and inserts moves as needed

                const num_inputs = ctx.numInputs(ir_inst);

                // Allocate space for return pairs
                // System V AMD64 ABI: integer returns in RAX, RDX; float returns in XMM0, XMM1
                var ret_pairs = ctx.allocator.alloc(CallArgPair, num_inputs) catch return null;
                var pair_idx: usize = 0;
                var int_ret_idx: usize = 0;
                var float_ret_idx: usize = 0;

                for (0..num_inputs) |i| {
                    const ret_val = ctx.putInputInRegs(ir_inst, i);
                    const ret_reg = ret_val.onlyReg() orelse continue;
                    const ty = ctx.inputTy(ir_inst, i);

                    // Determine the physical register for this return value
                    const preg: PReg = if (ty.isFloat())
                        blk: {
                            if (float_ret_idx >= abi.SYSV_RET_FPRS.len) continue;
                            const enc = abi.SYSV_RET_FPRS[float_ret_idx];
                            float_ret_idx += 1;
                            break :blk regs.fprPreg(enc);
                        }
                    else
                        blk: {
                            if (int_ret_idx >= abi.SYSV_RET_GPRS.len) continue;
                            const enc = abi.SYSV_RET_GPRS[int_ret_idx];
                            int_ret_idx += 1;
                            break :blk regs.gprPreg(enc);
                        };

                    // Create CallArgPair mapping vreg to preg
                    // regalloc2 will see reg_fixed_use(vreg, preg) from get_operands
                    // and ensure vreg is in preg at this point, inserting moves as needed
                    ret_pairs[pair_idx] = CallArgPair{
                        .vreg = ret_reg,
                        .preg = preg,
                    };
                    pair_idx += 1;
                }

                // Emit the rets instruction with the return pairs
                // This is a pseudo-instruction that tells regalloc about the constraints
                // and emits as just a `ret` instruction
                ctx.emit(Inst.genRets(ret_pairs[0..pair_idx])) catch return null;
            },
            .trap => {
                // Trap instruction - emit undefined instruction (UD2).
                // This is a terminator that causes a hardware trap.
                ctx.emit(Inst{
                    .ud2 = .{ .trap_code = .unreachable_code_reached },
                }) catch return null;
            },
            else => return null,
        }
    }

    /// Generate the Args pseudo-instruction for function parameters.
    /// Port of Cranelift's gen_args pattern from abi.rs.
    /// This creates fixed register constraints (rdi, rsi, etc.) for function params.
    ///
    /// System V AMD64 ABI:
    /// - Integer args: RDI, RSI, RDX, RCX, R8, R9
    /// - Float args: XMM0-XMM7
    pub fn genArgSetup(_: *const Self, ctx: *LowerCtx) !void {
        // Get the entry block
        const entry_block = ctx.f.layout.entryBlock() orelse return;

        // Get the entry block's params (which are function parameters)
        const block_params = ctx.f.dfg.blockParams(entry_block);
        if (block_params.len == 0) return;

        // Allocate CallArgPairs for each function parameter
        var arg_pairs = try ctx.allocator.alloc(CallArgPair, block_params.len);
        var int_arg_idx: usize = 0;
        var float_arg_idx: usize = 0;

        for (block_params, 0..) |param, i| {
            // Get the vreg assigned to this param
            const value_regs = ctx.value_regs.get(param);
            const vreg = value_regs.onlyReg() orelse continue;

            // Get the type to determine if it's float or int
            const ty = ctx.f.dfg.valueType(param);

            // Determine the physical register for this argument
            // System V AMD64 ABI: integers in rdi,rsi,rdx,rcx,r8,r9; floats in xmm0-xmm7
            const preg: PReg = if (ty.isFloat())
                blk: {
                    if (float_arg_idx >= abi.SYSV_ARG_FPRS.len) {
                        // Stack argument - skip for now
                        continue;
                    }
                    const enc = abi.SYSV_ARG_FPRS[float_arg_idx];
                    float_arg_idx += 1;
                    break :blk regs.fprPreg(enc);
                }
            else
                blk: {
                    if (int_arg_idx >= abi.SYSV_ARG_GPRS.len) {
                        // Stack argument - skip for now
                        continue;
                    }
                    const enc = abi.SYSV_ARG_GPRS[int_arg_idx];
                    int_arg_idx += 1;
                    break :blk regs.gprPreg(enc);
                };

            // Create CallArgPair: vreg (def) = preg (fixed physical register)
            arg_pairs[i] = CallArgPair{
                .vreg = vreg,
                .preg = preg,
            };
        }

        // Emit the Args instruction
        ctx.emit(Inst.genArgs(arg_pairs)) catch return;

        // Check if we have a vmctx parameter using Function.specialParam
        // vmctx is typically the first integer parameter (rdi)
        const has_vmctx = ctx.f.specialParam(.vmctx) != null;

        // If we have a vmctx parameter, move it to the pinned register (r15)
        // Port of Cranelift's set_pinned_reg pattern
        // This ensures vmctx is always available in r15, which is excluded from allocation
        if (has_vmctx) {
            // vmctx is in rdi (first int arg), move to r15 (pinned reg)
            // mov r15, rdi
            ctx.emit(Inst{
                .mov_r_r = .{
                    .size = .size64,
                    .src = Gpr.unwrapNew(regs.rdi()), // vmctx is in rdi
                    .dst = WritableGpr.fromReg(Gpr.unwrapNew(regs.pinnedReg())),
                },
            }) catch return;
        }
    }

    // =========================================================================
    // Integer constants
    // =========================================================================

    fn lowerIconst(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);

        // Get constant value - first try the constant map, then fall back to instruction data
        // Port of Cranelift's iconst lowering from x64/lower.isle
        const value: u64 = ctx.getConstant(ir_inst) orelse blk: {
            const imm = ctx.data(ir_inst).getImmediate() orelse return null;
            break :blk @bitCast(imm);
        };

        // Allocate destination register
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // Emit MOV imm, dst
        ctx.emit(Inst{
            .imm = .{
                .dst_size = size,
                .simm64 = value,
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Integer arithmetic
    // =========================================================================

    fn lowerIadd(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryAlu(ctx, ir_inst, .add);
    }

    fn lowerIsub(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryAlu(ctx, ir_inst, .sub);
    }

    fn lowerIneg(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // MOV src, dst
        ctx.emit(Inst{
            .mov_r_r = .{
                .size = size,
                .src = src_gpr,
                .dst = dst_gpr,
            },
        }) catch return null;

        // NEG dst
        ctx.emit(Inst{
            .unary_rm_r = .{
                .size = size,
                .op = .neg,
                .src = GprMem{ .inner = RegMem{ .reg = dst_gpr.toReg().toReg() } },
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerImul(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);

        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const dst = ctx.allocTmp(ty) catch return null;

        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;
        const lhs_gpr = Gpr.unwrapNew(lhs_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // For IMUL r64, r/m64, the result goes to the first operand
        // MOV lhs, dst
        ctx.emit(Inst{
            .mov_r_r = .{
                .size = size,
                .src = lhs_gpr,
                .dst = dst_gpr,
            },
        }) catch return null;

        // IMUL dst, rhs (two-operand form: dst = dst * rhs)
        // This is represented as alu_rmi_r with a special opcode
        // Note: x86 IMUL is complex; we use the 2-operand form
        const rhs_rmi = GprMemImm{ .inner = RegMemImm{ .reg = rhs_reg } };

        ctx.emit(Inst{
            .alu_rmi_r = .{
                .size = size,
                .op = .imul,
                .src = rhs_rmi,
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerUdiv(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerDiv(ctx, ir_inst, false, false);
    }

    fn lowerSdiv(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerDiv(ctx, ir_inst, true, false);
    }

    fn lowerUrem(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerDiv(ctx, ir_inst, false, true);
    }

    fn lowerSrem(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerDiv(ctx, ir_inst, true, true);
    }

    fn lowerDiv(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst, is_signed: bool, want_remainder: bool) ?InstOutput {
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);

        const dividend = ctx.putInputInRegs(ir_inst, 0);
        const divisor = ctx.putInputInRegs(ir_inst, 1);
        const dst = ctx.allocTmp(ty) catch return null;

        const dividend_reg = dividend.onlyReg() orelse return null;
        const divisor_reg = divisor.onlyReg() orelse return null;
        const dividend_gpr = Gpr.unwrapNew(dividend_reg);
        const divisor_gpr = Gpr.unwrapNew(divisor_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // Division uses RAX:RDX / divisor, quotient in RAX, remainder in RDX
        // For now, emit the div instruction with appropriate setup

        // Placeholder for high part of dividend
        const rdx_gpr = Gpr.unwrapNew(regs.rdx());

        ctx.emit(Inst{
            .div = .{
                .size = size,
                .signed = is_signed,
                .divisor = GprMem{ .inner = RegMem{ .reg = divisor_gpr.toReg() } },
                .dividend_lo = dividend_gpr,
                .dividend_hi = rdx_gpr,
                .dst_quotient = if (want_remainder) WritableGpr.fromReg(Gpr.unwrapNew(regs.rax())) else dst_gpr,
                .dst_remainder = if (want_remainder) dst_gpr else WritableGpr.fromReg(Gpr.unwrapNew(regs.rdx())),
                .trap_code = .integer_division_by_zero,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Bitwise operations
    // =========================================================================

    fn lowerBand(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryAlu(ctx, ir_inst, .@"and");
    }

    fn lowerBor(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryAlu(ctx, ir_inst, .@"or");
    }

    fn lowerBxor(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryAlu(ctx, ir_inst, .xor);
    }

    fn lowerBnot(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // MOV src, dst
        ctx.emit(Inst{
            .mov_r_r = .{
                .size = size,
                .src = src_gpr,
                .dst = dst_gpr,
            },
        }) catch return null;

        // NOT dst
        ctx.emit(Inst{
            .unary_rm_r = .{
                .size = size,
                .op = .not,
                .src = GprMem{ .inner = RegMem{ .reg = dst_gpr.toReg().toReg() } },
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerIshl(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerShift(ctx, ir_inst, .shl);
    }

    fn lowerUshr(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerShift(ctx, ir_inst, .shr);
    }

    fn lowerSshr(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerShift(ctx, ir_inst, .sar);
    }

    fn lowerRotl(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerShift(ctx, ir_inst, .rol);
    }

    fn lowerRotr(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerShift(ctx, ir_inst, .ror);
    }

    fn lowerShift(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst, kind: ShiftKind) ?InstOutput {
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // MOV src, dst
        ctx.emit(Inst{
            .mov_r_r = .{
                .size = size,
                .src = src_gpr,
                .dst = dst_gpr,
            },
        }) catch return null;

        // For now, assume shift by CL
        ctx.emit(Inst{
            .shift_r = .{
                .size = size,
                .kind = kind,
                .shift_by = .cl,
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerClz(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // LZCNT
        ctx.emit(Inst{
            .unary_rm_r = .{
                .size = size,
                .op = .lzcnt,
                .src = GprMem{ .inner = RegMem{ .reg = src_gpr.toReg() } },
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerCtz(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // TZCNT
        ctx.emit(Inst{
            .unary_rm_r = .{
                .size = size,
                .op = .tzcnt,
                .src = GprMem{ .inner = RegMem{ .reg = src_gpr.toReg() } },
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerPopcnt(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // POPCNT
        ctx.emit(Inst{
            .unary_rm_r = .{
                .size = size,
                .op = .popcnt,
                .src = GprMem{ .inner = RegMem{ .reg = src_gpr.toReg() } },
                .dst = dst_gpr,
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
        const size = operandSizeFromType(ty);
        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);

        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;
        const lhs_gpr = Gpr.unwrapNew(lhs_reg);
        const rhs_rmi = GprMemImm{ .inner = RegMemImm{ .reg = rhs_reg } };

        // CMP lhs, rhs
        ctx.emit(Inst{
            .cmp_rmi_r = .{
                .size = size,
                .src = rhs_rmi,
                .dst = lhs_gpr,
            },
        }) catch return null;

        // Allocate result
        const dst = ctx.allocTmp(ClifType.I8) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // Get the condition code from instruction data
        const inst_data = ctx.data(ir_inst);
        const intcc = inst_data.getIntCC() orelse return null;
        const cc = ccFromIntCC(intcc);

        // SETCC
        ctx.emit(Inst{
            .setcc = .{
                .cc = cc,
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Floating point
    // =========================================================================

    fn lowerFconst(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const ty = ctx.outputTy(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // Floating-point constants need to be loaded from memory or materialized
        // This is a simplified version
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        // XOR the register with itself to zero it (for zero constant)
        const op: SseOpcode = if (ty.bytes() == 4) .movss else .movsd;
        ctx.emit(Inst{
            .xmm_unary_rm_r = .{
                .op = op,
                .src = XmmMem{ .inner = RegMem{ .reg = dst_xmm.toReg().toReg() } },
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFadd(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryFp(ctx, ir_inst, .addss, .addsd);
    }

    fn lowerFsub(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryFp(ctx, ir_inst, .subss, .subsd);
    }

    fn lowerFmul(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryFp(ctx, ir_inst, .mulss, .mulsd);
    }

    fn lowerFdiv(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryFp(ctx, ir_inst, .divss, .divsd);
    }

    fn lowerFneg(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const op: SseOpcode = if (ty.bytes() == 4) .xorps else .xorpd;
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        // XOR with sign bit mask
        ctx.emit(Inst{
            .xmm_rm_r = .{
                .op = op,
                .src = XmmMem{ .inner = RegMem{ .reg = src_reg } },
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFabs(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const op: SseOpcode = if (ty.bytes() == 4) .andps else .andpd;
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        // AND with abs mask (clear sign bit)
        ctx.emit(Inst{
            .xmm_rm_r = .{
                .op = op,
                .src = XmmMem{ .inner = RegMem{ .reg = src_reg } },
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerSqrt(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const op: SseOpcode = if (ty.bytes() == 4) .sqrtss else .sqrtsd;
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .xmm_unary_rm_r = .{
                .op = op,
                .src = XmmMem{ .inner = RegMem{ .reg = src_reg } },
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcmp(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.inputTy(ir_inst, 0);
        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);

        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;
        const op: SseOpcode = if (ty.bytes() == 4) .ucomiss else .ucomisd;
        const lhs_xmm = Xmm.unwrapNew(lhs_reg);

        // UCOMISS/UCOMISD
        ctx.emit(Inst{
            .xmm_cmp_rm_r = .{
                .op = op,
                .src = XmmMem{ .inner = RegMem{ .reg = rhs_reg } },
                .dst = lhs_xmm,
            },
        }) catch return null;

        // Allocate result
        const dst = ctx.allocTmp(ClifType.I8) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // SETCC
        ctx.emit(Inst{
            .setcc = .{
                .cc = .z, // Placeholder
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFmin(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryFp(ctx, ir_inst, .minss, .minsd);
    }

    fn lowerFmax(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        return self.lowerBinaryFp(ctx, ir_inst, .maxss, .maxsd);
    }

    // =========================================================================
    // Conversions
    // =========================================================================

    fn lowerUextend(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const ext_mode = ExtMode.fromBits(@intCast(src_ty.bits()), @intCast(dst_ty.bits())) orelse return null;
        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .movzx_rm_r = .{
                .ext_mode = ext_mode,
                .src = GprMem{ .inner = RegMem{ .reg = src_gpr.toReg() } },
                .dst = dst_gpr,
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
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const ext_mode = ExtMode.fromBits(@intCast(src_ty.bits()), @intCast(dst_ty.bits())) orelse return null;
        const src_reg = src.onlyReg() orelse return null;
        const src_gpr = Gpr.unwrapNew(src_reg);
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .movsx_rm_r = .{
                .ext_mode = ext_mode,
                .src = GprMem{ .inner = RegMem{ .reg = src_gpr.toReg() } },
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerIreduce(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        // For reduce, we just use the lower bits
        _ = self;
        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;
        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(src_reg)) catch return null;
        return output;
    }

    fn lowerFcvtToSint(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const op: SseOpcode = if (src_ty.bytes() == 4) .cvttss2si else .cvttsd2si;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .xmm_to_gpr = .{
                .op = op,
                .src = Xmm.unwrapNew(src_reg),
                .dst = dst_gpr,
                .dst_size = operandSizeFromType(dst_ty),
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcvtToUint(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        // Unsigned conversion needs special handling for large values
        // This is a simplified version
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .xmm_to_gpr = .{
                .op = .cvttss2si,
                .src = Xmm.unwrapNew(src_reg),
                .dst = dst_gpr,
                .dst_size = operandSizeFromType(dst_ty),
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcvtFromSint(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const src_ty = ctx.inputTy(ir_inst, 0);
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const op: SseOpcode = if (dst_ty.bytes() == 4) .cvtsi2ss else .cvtsi2sd;
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .gpr_to_xmm = .{
                .op = op,
                .src = GprMem{ .inner = RegMem{ .reg = src_reg } },
                .src_size = operandSizeFromType(src_ty),
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFcvtFromUint(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        // Unsigned conversion needs special handling
        return self.lowerFcvtFromSint(ctx, ir_inst);
    }

    fn lowerFpromote(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        // CVTSS2SD
        ctx.emit(Inst{
            .xmm_unary_rm_r = .{
                .op = .cvtss2sd,
                .src = XmmMem{ .inner = RegMem{ .reg = src_reg } },
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerFdemote(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        // CVTSD2SS
        ctx.emit(Inst{
            .xmm_unary_rm_r = .{
                .op = .cvtsd2ss,
                .src = XmmMem{ .inner = RegMem{ .reg = src_reg } },
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerBitcast(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const src_reg = src.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        // For same-size bitcast, just copy registers
        // May need GPR<->XMM move for int/float conversions
        const src_class = src_reg.class();
        const dst_class = if (dst_ty.isFloat()) RegClass.float else RegClass.int;

        if (src_class == dst_class) {
            // Same class, just copy
            var output = InstOutput{};
            output.append(ValueRegs(Reg).one(src_reg)) catch return null;
            return output;
        }

        if (src_class == .int and dst_class == .float) {
            const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));
            ctx.emit(Inst{
                .gpr_to_xmm = .{
                    .op = .movd,
                    .src = GprMem{ .inner = RegMem{ .reg = src_reg } },
                    .src_size = operandSizeFromType(dst_ty),
                    .dst = dst_xmm,
                },
            }) catch return null;
        } else {
            const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));
            ctx.emit(Inst{
                .xmm_to_gpr = .{
                    .op = .movd,
                    .src = Xmm.unwrapNew(src_reg),
                    .dst = dst_gpr,
                    .dst_size = operandSizeFromType(dst_ty),
                },
            }) catch return null;
        }

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Memory operations
    // =========================================================================

    fn lowerLoad(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const addr = ctx.putInputInRegs(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const addr_reg = addr.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const addr_gpr = Gpr.unwrapNew(addr_reg);
        const amode = SyntheticAmode.real_amode(Amode.immReg(0, addr_gpr.toReg()));

        if (dst_ty.isFloat()) {
            const op: SseOpcode = if (dst_ty.bytes() == 4) .movss else .movsd;
            const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

            ctx.emit(Inst{
                .xmm_mov_m_r = .{
                    .op = op,
                    .src = amode,
                    .dst = dst_xmm,
                },
            }) catch return null;
        } else {
            const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

            ctx.emit(Inst{
                .mov_m_r = .{
                    .size = operandSizeFromType(dst_ty),
                    .src = amode,
                    .dst = dst_gpr,
                },
            }) catch return null;
        }

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerStore(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src_ty = ctx.inputTy(ir_inst, 0);
        const src = ctx.putInputInRegs(ir_inst, 0);
        const addr = ctx.putInputInRegs(ir_inst, 1);

        const src_reg = src.onlyReg() orelse return null;
        const addr_reg = addr.onlyReg() orelse return null;
        const addr_gpr = Gpr.unwrapNew(addr_reg);
        const amode = SyntheticAmode.real_amode(Amode.immReg(0, addr_gpr.toReg()));

        if (src_ty.isFloat()) {
            const op: SseOpcode = if (src_ty.bytes() == 4) .movss else .movsd;
            const src_xmm = Xmm.unwrapNew(src_reg);

            ctx.emit(Inst{
                .xmm_mov_r_m = .{
                    .op = op,
                    .src = src_xmm,
                    .dst = amode,
                },
            }) catch return null;
        } else {
            const src_gpr = Gpr.unwrapNew(src_reg);

            ctx.emit(Inst{
                .mov_r_m = .{
                    .size = operandSizeFromType(src_ty),
                    .src = src_gpr,
                    .dst = amode,
                },
            }) catch return null;
        }

        return InstOutput{};
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

        // Use slotOffset for stack-relative addressing
        const amode = SyntheticAmode.slotOffset(slot_byte_offset);

        if (ty.isFloat()) {
            const op: SseOpcode = if (ty.bytes() == 4) .movss else .movsd;
            const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

            ctx.emit(Inst{
                .xmm_mov_m_r = .{
                    .op = op,
                    .src = amode,
                    .dst = dst_xmm,
                },
            }) catch return null;
        } else {
            const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

            ctx.emit(Inst{
                .mov_m_r = .{
                    .size = operandSizeFromType(ty),
                    .src = amode,
                    .dst = dst_gpr,
                },
            }) catch return null;
        }

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

        // Use slotOffset for stack-relative addressing
        const amode = SyntheticAmode.slotOffset(slot_byte_offset);

        if (val_ty.isFloat()) {
            const op: SseOpcode = if (val_ty.bytes() == 4) .movss else .movsd;
            const src_xmm = Xmm.unwrapNew(val_reg);

            ctx.emit(Inst{
                .xmm_mov_r_m = .{
                    .op = op,
                    .src = src_xmm,
                    .dst = amode,
                },
            }) catch return null;
        } else {
            const src_gpr = Gpr.unwrapNew(val_reg);

            ctx.emit(Inst{
                .mov_r_m = .{
                    .size = operandSizeFromType(val_ty),
                    .src = src_gpr,
                    .dst = amode,
                },
            }) catch return null;
        }

        return InstOutput{};
    }

    // =========================================================================
    // Select and Copy
    // =========================================================================

    fn lowerSelect(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const consequent = ctx.putInputInRegs(ir_inst, 1);
        const alternative = ctx.putInputInRegs(ir_inst, 2);
        const dst = ctx.allocTmp(dst_ty) catch return null;

        const cons_reg = consequent.onlyReg() orelse return null;
        const alt_reg = alternative.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const cons_gpr = Gpr.unwrapNew(cons_reg);
        const alt_gpr = Gpr.unwrapNew(alt_reg);
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // MOV alternative, dst
        ctx.emit(Inst{
            .mov_r_r = .{
                .size = operandSizeFromType(dst_ty),
                .src = alt_gpr,
                .dst = dst_gpr,
            },
        }) catch return null;

        // CMOV consequent, dst
        ctx.emit(Inst{
            .cmove = .{
                .size = operandSizeFromType(dst_ty),
                .cc = .nz, // Based on condition
                .src = GprMem{ .inner = RegMem{ .reg = cons_gpr.toReg() } },
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerCopy(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const src = ctx.putInputInRegs(ir_inst, 0);
        const src_reg = src.onlyReg() orelse return null;
        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(src_reg)) catch return null;
        return output;
    }

    // =========================================================================
    // Calls and traps
    // =========================================================================

    fn lowerCall(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        // Call lowering.
        // Port of cranelift/codegen/src/isa/x64/lower.rs call lowering
        //
        // System V AMD64 ABI:
        // - Integer arguments: RDI, RSI, RDX, RCX, R8, R9
        // - Float arguments: XMM0-XMM7
        // - Return value: RAX (integer), XMM0 (float)
        // - Caller-saved (clobbered): RAX, RCX, RDX, RSI, RDI, R8-R11, XMM0-XMM15
        //
        // Cranelift pattern: NO explicit mov instructions for register args/returns.
        // Instead, populate uses/defs with fixed register constraints.
        // Regalloc will insert moves as needed to satisfy constraints.

        const inst_data = ctx.data(ir_inst);

        // Get the function reference
        const func_ref = inst_data.getFuncRef() orelse return null;

        // Get the external function data (contains the function name)
        const ext_func = ctx.extFuncData(func_ref) orelse return null;

        // Get arguments
        const num_args = ctx.numInputs(ir_inst);

        // Create CallInfo structure with the external function destination
        const call_info = ctx.allocator.create(CallInfo) catch return null;
        call_info.* = CallInfo{
            .dest = ext_func.name,
            .uses = .{},
            .defs = .{},
            .clobbers = PRegSet.empty,
            .callee_conv = .system_v,
            .caller_conv = .system_v,
            .opcode = null,
            .try_call_info = null,
        };

        // Build System V AMD64 clobber set: RAX, RCX, RDX, RSI, RDI, R8-R11, XMM0-15
        // Port of Cranelift's get_regs_clobbered_by_call
        var clobbers = PRegSet.empty;
        // Integer caller-saved: RAX(0), RCX(1), RDX(2), RSI(6), RDI(7), R8-R11(8-11)
        clobbers = clobbers.add(regs.gprPreg(GprEnc.RAX));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.RCX));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.RDX));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.RSI));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.RDI));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.R8));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.R9));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.R10));
        clobbers = clobbers.add(regs.gprPreg(GprEnc.R11));
        // TODO: Add float clobbers XMM0-XMM15 (requires fixing x64 PRegSet to use regalloc's PRegSet)
        // For now, skip float clobbers since we don't use float values yet

        // System V ABI integer argument registers (in order)
        const int_arg_pregs = [_]PReg{
            regs.gprPreg(GprEnc.RDI),
            regs.gprPreg(GprEnc.RSI),
            regs.gprPreg(GprEnc.RDX),
            regs.gprPreg(GprEnc.RCX),
            regs.gprPreg(GprEnc.R8),
            regs.gprPreg(GprEnc.R9),
        };

        // Build uses list: gen_call_args(abi, args)
        // Each argument vreg is constrained to its ABI register
        // Cranelift does NOT emit explicit mov instructions - regalloc handles it
        var int_arg_idx: u8 = 0;
        for (0..num_args) |idx| {
            const arg_val = ctx.putInputInRegs(ir_inst, idx);
            const arg_reg = arg_val.onlyReg() orelse continue;
            const arg_ty = ctx.inputTy(ir_inst, idx);

            if (!arg_ty.isFloat() and int_arg_idx < int_arg_pregs.len) {
                // Add to uses: this vreg MUST be in this preg at the call
                call_info.uses.append(ctx.allocator, .{
                    .vreg = arg_reg,
                    .preg = int_arg_pregs[int_arg_idx],
                }) catch return null;
                int_arg_idx += 1;
            }
            // TODO: Handle float arguments and stack arguments
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

            // Add to defs: this vreg is DEFINED BY the call in RAX
            const ret_preg = regs.gprPreg(GprEnc.RAX);
            call_info.defs.append(ctx.allocator, .{
                .vreg = dst_reg,
                .location = .{ .reg = ret_preg },
            }) catch return null;

            // Port of Cranelift's gen_call_info: remove return regs from clobbers.
            // regalloc2 requires that clobbers and defs must not collide.
            clobbers.remove(ret_preg);

            output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        }

        // Assign clobbers after removing return registers
        call_info.clobbers = clobbers;

        // Emit the call instruction with proper CallInfo
        // Regalloc sees uses/defs/clobbers and inserts moves as needed
        ctx.emit(Inst{
            .call_known = .{ .info = call_info },
        }) catch return null;

        return output;
    }

    fn lowerCallIndirect(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        // Indirect call lowering.
        // The callee address is the first input, remaining inputs are arguments.

        const num_inputs = ctx.numInputs(ir_inst);
        if (num_inputs == 0) return null;

        // Get the callee (first input)
        const callee_val = ctx.putInputInRegs(ir_inst, 0);
        const callee_reg = callee_val.onlyReg() orelse return null;

        // System V ABI integer argument registers
        const int_arg_regs = [_]Reg{
            regs.rdi(),
            regs.rsi(),
            regs.rdx(),
            regs.rcx(),
            regs.r8(),
            regs.r9(),
        };

        // Move arguments to ABI registers (skip first input which is callee)
        var int_arg_idx: usize = 0;
        for (1..num_inputs) |i| {
            const arg_val = ctx.putInputInRegs(ir_inst, i);
            const arg_reg = arg_val.onlyReg() orelse continue;
            const arg_ty = ctx.inputTy(ir_inst, i);

            if (!arg_ty.isFloat() and int_arg_idx < int_arg_regs.len) {
                const dst_reg = int_arg_regs[int_arg_idx];
                const src_gpr = Gpr.unwrapNew(arg_reg);
                const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg));

                ctx.emit(Inst{
                    .mov_r_r = .{
                        .size = operandSizeFromType(arg_ty),
                        .src = src_gpr,
                        .dst = dst_gpr,
                    },
                }) catch return null;

                int_arg_idx += 1;
            }
        }

        // Emit indirect call through the callee register
        ctx.emit(Inst{
            .jmp_unknown = .{
                .target = RegMem.fromReg(callee_reg),
            },
        }) catch return null;

        // Handle return value
        const num_outputs = ctx.numOutputs(ir_inst);
        if (num_outputs > 0) {
            const ret_ty = ctx.outputTy(ir_inst, 0);
            const dst = ctx.allocTmp(ret_ty) catch return null;
            const dst_reg = dst.onlyReg() orelse return null;
            const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

            ctx.emit(Inst{
                .mov_r_r = .{
                    .size = operandSizeFromType(ret_ty),
                    .src = Gpr.unwrapNew(regs.rax()),
                    .dst = dst_gpr,
                },
            }) catch return null;

            var output = InstOutput{};
            output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
            return output;
        }

        return InstOutput{};
    }

    fn lowerTrap(_: *const Self, ctx: *LowerCtx, _: ClifInst) ?InstOutput {
        ctx.emit(Inst{
            .ud2 = .{ .trap_code = .unreachable_code_reached },
        }) catch return null;
        return InstOutput{};
    }

    fn lowerTrapnz(_: *const Self, ctx: *LowerCtx, _: ClifInst) ?InstOutput {
        ctx.emit(Inst{
            .trap_if = .{
                .cc = .nz,
                .trap_code = .unreachable_code_reached,
            },
        }) catch return null;
        return InstOutput{};
    }

    fn lowerTrapz(_: *const Self, ctx: *LowerCtx, _: ClifInst) ?InstOutput {
        ctx.emit(Inst{
            .trap_if = .{
                .cc = .z,
                .trap_code = .unreachable_code_reached,
            },
        }) catch return null;
        return InstOutput{};
    }

    fn lowerFuncAddr(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .load_ext_name = .{
                .dst = dst_gpr,
                .name = ExternalName.initUser(0, 0),
                .offset = 0,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerStackAddr(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .lea = .{
                .size = .size64,
                .src = SyntheticAmode.slot_offset(0),
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    /// Lower a global_value instruction.
    ///
    /// Port of cranelift/codegen/src/legalizer/globalvalue.rs expand_global_value
    ///
    /// GlobalValue is expanded based on its GlobalValueData:
    /// - VMContext: Load from vmctx parameter (pinned register r15)
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
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        switch (gv_data) {
            .vmcontext => {
                // VMContext: Return the pinned register (r15) directly.
                // Port of Cranelift's get_pinned_reg pattern.
                //
                // At function entry, vmctx is moved from rdi into r15.
                // r15 is excluded from register allocation, so it can't be clobbered.
                // This ensures vmctx is always available regardless of control flow.
                //
                // Copy r15 to the destination register.
                ctx.emit(Inst{
                    .mov_r_r = .{
                        .size = .size64,
                        .src = Gpr.unwrapNew(regs.pinnedReg()),
                        .dst = dst_gpr,
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
                        .mov_r_r = .{
                            .size = .size64,
                            .src = Gpr.unwrapNew(base_addr),
                            .dst = dst_gpr,
                        },
                    }) catch return null;
                } else {
                    // lea dst, [base + offset]
                    ctx.emit(Inst{
                        .mov_r_r = .{
                            .size = .size64,
                            .src = Gpr.unwrapNew(base_addr),
                            .dst = dst_gpr,
                        },
                    }) catch return null;

                    // add dst, offset
                    ctx.emit(Inst{
                        .alu_rmi_r = .{
                            .size = .size64,
                            .op = .add,
                            .src = GprMemImm{ .inner = RegMemImm{ .imm = @as(u32, @intCast(d.offset)) } },
                            .dst = dst_gpr,
                        },
                    }) catch return null;
                }
            },

            .load => |d| {
                // Load: Recursively expand base, then load from offset
                // Port of cranelift/codegen/src/legalizer/globalvalue.rs load_addr
                const base_addr = self.materializeGlobalValue(ctx, d.base, ClifType.I64) orelse return null;

                // Load from base + offset
                // mov dst, [base + offset]
                const amode = SyntheticAmode.real_amode(Amode{ .imm_reg = .{
                    .simm32 = @intCast(d.offset),
                    .base = base_addr,
                    .flags = MemFlags.empty,
                } });
                ctx.emit(Inst{
                    .mov_m_r = .{
                        .size = operandSizeFromType(d.global_type),
                        .src = amode,
                        .dst = dst_gpr,
                    },
                }) catch return null;
            },

            .symbol => {
                // Symbol: For now, just load a placeholder address
                // Full implementation would emit relocation
                // Port of cranelift/codegen/src/legalizer/globalvalue.rs symbol
                ctx.emit(Inst{
                    .imm = .{
                        .dst_size = .size64,
                        .simm64 = 0,
                        .dst = dst_gpr,
                    },
                }) catch return null;
            },

            .dyn_scale_target_const => {
                // Dynamic vector scale - emit constant 1 for now
                ctx.emit(Inst{
                    .imm = .{
                        .dst_size = .size64,
                        .simm64 = 1,
                        .dst = dst_gpr,
                    },
                }) catch return null;
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
        const tmp_gpr = WritableGpr.fromReg(Gpr.unwrapNew(tmp_reg.toReg()));

        switch (gv_data) {
            .vmcontext => {
                // VMContext: Copy from pinned register (r15).
                // Port of Cranelift's get_pinned_reg pattern.
                ctx.emit(Inst{
                    .mov_r_r = .{
                        .size = .size64,
                        .src = Gpr.unwrapNew(regs.pinnedReg()),
                        .dst = tmp_gpr,
                    },
                }) catch return null;
                return tmp_reg.toReg();
            },
            .iadd_imm => |d| {
                const base_addr = self.materializeGlobalValue(ctx, d.base, ty) orelse return null;

                if (d.offset == 0) {
                    return base_addr;
                }

                // mov tmp, base
                ctx.emit(Inst{
                    .mov_r_r = .{
                        .size = .size64,
                        .src = Gpr.unwrapNew(base_addr),
                        .dst = tmp_gpr,
                    },
                }) catch return null;

                // add tmp, offset
                ctx.emit(Inst{
                    .alu_rmi_r = .{
                        .size = .size64,
                        .op = .add,
                        .src = GprMemImm{ .inner = RegMemImm{ .imm = @as(u32, @intCast(d.offset)) } },
                        .dst = tmp_gpr,
                    },
                }) catch return null;

                return tmp_reg.toReg();
            },
            .load => |d| {
                const base_addr = self.materializeGlobalValue(ctx, d.base, ClifType.I64) orelse return null;
                const amode = SyntheticAmode.real_amode(Amode{ .imm_reg = .{
                    .simm32 = @intCast(d.offset),
                    .base = base_addr,
                    .flags = MemFlags.empty,
                } });
                ctx.emit(Inst{
                    .mov_m_r = .{
                        .size = operandSizeFromType(d.global_type),
                        .src = amode,
                        .dst = tmp_gpr,
                    },
                }) catch return null;
                return tmp_reg.toReg();
            },
            .symbol => {
                // Load placeholder for symbol
                ctx.emit(Inst{
                    .imm = .{
                        .dst_size = .size64,
                        .simm64 = 0,
                        .dst = tmp_gpr,
                    },
                }) catch return null;
                return tmp_reg.toReg();
            },
            .dyn_scale_target_const => {
                ctx.emit(Inst{
                    .imm = .{
                        .dst_size = .size64,
                        .simm64 = 1,
                        .dst = tmp_gpr,
                    },
                }) catch return null;
                return tmp_reg.toReg();
            },
        }
    }

    fn lowerSymbolValue(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .load_ext_name = .{
                .dst = dst_gpr,
                .name = ExternalName.initUser(0, 0),
                .offset = 0,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    // =========================================================================
    // Helper functions
    // =========================================================================

    fn lowerBinaryAlu(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst, op: AluRmiROpcode) ?InstOutput {
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);
        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const dst = ctx.allocTmp(ty) catch return null;

        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const lhs_gpr = Gpr.unwrapNew(lhs_reg);
        const rhs_rmi = GprMemImm{ .inner = RegMemImm{ .reg = rhs_reg } };
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // MOV lhs, dst
        ctx.emit(Inst{
            .mov_r_r = .{
                .size = size,
                .src = lhs_gpr,
                .dst = dst_gpr,
            },
        }) catch return null;

        // OP rhs, dst
        ctx.emit(Inst{
            .alu_rmi_r = .{
                .size = size,
                .op = op,
                .src = rhs_rmi,
                .dst = dst_gpr,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerBinaryFp(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst, op32: SseOpcode, op64: SseOpcode) ?InstOutput {
        const ty = ctx.outputTy(ir_inst, 0);
        const lhs = ctx.putInputInRegs(ir_inst, 0);
        const rhs = ctx.putInputInRegs(ir_inst, 1);
        const dst = ctx.allocTmp(ty) catch return null;

        const lhs_reg = lhs.onlyReg() orelse return null;
        const rhs_reg = rhs.onlyReg() orelse return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const op = if (ty.bytes() == 4) op32 else op64;
        const dst_xmm = WritableXmm.fromReg(Xmm.unwrapNew(dst_reg.toReg()));

        // For SSE, we need dst = lhs first
        ctx.emit(Inst{
            .xmm_unary_rm_r = .{
                .op = if (ty.bytes() == 4) .movss else .movsd,
                .src = XmmMem{ .inner = RegMem{ .reg = lhs_reg } },
                .dst = dst_xmm,
            },
        }) catch return null;

        // OP rhs, dst
        ctx.emit(Inst{
            .xmm_rm_r = .{
                .op = op,
                .src = XmmMem{ .inner = RegMem{ .reg = rhs_reg } },
                .dst = dst_xmm,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }
};

// =============================================================================
// Helper functions
// =============================================================================

/// Convert CLIF type to operand size.
fn operandSizeFromType(ty: ClifType) OperandSize {
    return switch (ty.bits()) {
        8 => .size8,
        16 => .size16,
        32 => .size32,
        64 => .size64,
        else => .size64,
    };
}

/// Convert IntCC to x64 condition code.
pub fn ccFromIntCC(cc: IntCC) CC {
    return switch (cc) {
        .eq => .z,
        .ne => .nz,
        .slt => .l,
        .sge => .nl,
        .sgt => .nle,
        .sle => .le,
        .ult => .b,
        .uge => .nb,
        .ugt => .nbe,
        .ule => .be,
    };
}

// =============================================================================
// Tests
// =============================================================================

test "X64LowerBackend init" {
    const backend = X64LowerBackend.init();
    try std.testing.expectEqual(CallConv.system_v, backend.call_conv);
    try std.testing.expect(backend.use_sse41);
    try std.testing.expect(backend.use_sse42);
}

test "operandSizeFromType" {
    try std.testing.expectEqual(OperandSize.size8, operandSizeFromType(ClifType.I8));
    try std.testing.expectEqual(OperandSize.size16, operandSizeFromType(ClifType.I16));
    try std.testing.expectEqual(OperandSize.size32, operandSizeFromType(ClifType.I32));
    try std.testing.expectEqual(OperandSize.size64, operandSizeFromType(ClifType.I64));
}

test "ccFromIntCC" {
    try std.testing.expectEqual(CC.z, ccFromIntCC(.eq));
    try std.testing.expectEqual(CC.nz, ccFromIntCC(.ne));
    try std.testing.expectEqual(CC.l, ccFromIntCC(.slt));
    try std.testing.expectEqual(CC.b, ccFromIntCC(.ult));
}
