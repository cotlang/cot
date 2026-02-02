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

    /// Lower a single CLIF instruction to x86-64 machine instructions.
    /// This is the main entry point for instruction selection.
    ///
    /// Returns the output registers containing the result, or null if lowering failed.
    pub fn lower(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const inst_data = ctx.data(ir_inst);
        const opcode = inst_data.opcode;

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
        const opcode = inst_data.opcode;

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
                // Jump table - not yet implemented
                return null;
            },
            .@"return" => {
                // Return instruction
                ctx.emit(Inst{ .ret = .{ .stack_bytes_to_pop = 0 } }) catch return null;
            },
            else => return null,
        }
    }

    // =========================================================================
    // Integer constants
    // =========================================================================

    fn lowerIconst(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        const ty = ctx.outputTy(ir_inst, 0);
        const size = operandSizeFromType(ty);

        // Get the constant value
        const imm: u64 = 42; // Placeholder

        // Allocate destination register
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        // Emit MOV imm, dst
        ctx.emit(Inst{
            .imm = .{
                .dst_size = size,
                .simm64 = imm,
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
                .op = .add, // Placeholder - need IMUL opcode
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

        // SETCC
        ctx.emit(Inst{
            .setcc = .{
                .cc = .z, // Placeholder - should be based on IntCC
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

    fn lowerCall(_: *const Self, _: *LowerCtx, _: ClifInst) ?InstOutput {
        // Calls are complex - need ABI handling
        return null;
    }

    fn lowerCallIndirect(_: *const Self, _: *LowerCtx, _: ClifInst) ?InstOutput {
        return null;
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
                .name = .{ .User = inst_mod.UserExternalNameRef.initFull(0, 0) },
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

    fn lowerGlobalValue(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .load_ext_name = .{
                .dst = dst_gpr,
                .name = .{ .known_symbol = .elf_global_offset_table },
                .offset = 0,
            },
        }) catch return null;

        var output = InstOutput{};
        output.append(ValueRegs(Reg).one(dst_reg.toReg())) catch return null;
        return output;
    }

    fn lowerSymbolValue(_: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        const dst_ty = ctx.outputTy(ir_inst, 0);
        const dst = ctx.allocTmp(dst_ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;
        const dst_gpr = WritableGpr.fromReg(Gpr.unwrapNew(dst_reg.toReg()));

        ctx.emit(Inst{
            .load_ext_name = .{
                .dst = dst_gpr,
                .name = .{ .User = inst_mod.UserExternalNameRef.initFull(0, 0) },
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
