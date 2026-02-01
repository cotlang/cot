//! AArch64 instruction lowering.
//!
//! This module translates CLIF IR instructions to AArch64 machine instructions.
//! Port of cranelift/codegen/src/isa/aarch64/lower.rs and lower.isle
//!
//! The ISLE patterns from Cranelift are hand-translated to Zig switch statements.

const std = @import("std");
const Allocator = std.mem.Allocator;

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
const MachLabel = inst_mod.args.MachLabel;
const Type = inst_mod.Type;

// Import register types from local modules
const regs = inst_mod.regs;
const Reg = regs.Reg;
const VReg = regs.VReg;
const Writable = regs.Writable;
const zeroReg = regs.zeroReg;

// =============================================================================
// Stub types for CLIF IR
// These match the interfaces from machinst/lower.zig to keep this module
// self-contained. In the full integration, these would come from the machinst
// lowering framework.
// =============================================================================

/// Opcode for CLIF instructions.
pub const Opcode = enum {
    nop,
    iconst,
    iadd,
    isub,
    ineg,
    imul,
    udiv,
    sdiv,
    urem,
    srem,
    band,
    bor,
    bxor,
    bnot,
    ishl,
    ushr,
    sshr,
    rotl,
    rotr,
    icmp,
    f32const,
    f64const,
    fadd,
    fsub,
    fmul,
    fdiv,
    fneg,
    fabs,
    sqrt,
    fcmp,
    uextend,
    sextend,
    ireduce,
    fcvt_to_sint,
    fcvt_to_uint,
    fcvt_from_sint,
    fcvt_from_uint,
    fpromote,
    fdemote,
    load,
    store,
    select,
    copy,
    @"return",
    jump,
    brif,
    br_table,
    call,
    call_indirect,
    trap,
    trapnz,
    trapz,
    func_addr,
    stack_load,
    stack_store,

    pub fn isBranch(self: Opcode) bool {
        return switch (self) {
            .jump, .brif, .br_table, .@"return" => true,
            else => false,
        };
    }
};

/// Integer condition codes.
pub const IntCC = enum {
    eq,
    ne,
    slt,
    sge,
    sgt,
    sle,
    ult,
    uge,
    ugt,
    ule,
};

/// Floating point condition codes.
pub const FloatCC = enum {
    ord,
    uno,
    eq,
    ne,
    one,
    ueq,
    lt,
    le,
    gt,
    ge,
    ult,
    ule,
    ugt,
    uge,
};

/// CLIF instruction reference.
pub const ClifInst = struct {
    index: u32,

    pub fn fromIndex(idx: u32) ClifInst {
        return .{ .index = idx };
    }
};

/// CLIF value reference.
pub const Value = struct {
    index: u32,

    pub fn fromIndex(idx: u32) Value {
        return .{ .index = idx };
    }
};

/// Block index for lowered blocks.
pub const BlockIndex = struct {
    idx: u32,

    pub fn new(idx: usize) BlockIndex {
        return .{ .idx = @intCast(idx) };
    }

    pub fn index(self: BlockIndex) usize {
        return self.idx;
    }
};

/// Instruction data stub.
pub const InstructionData = struct {
    opcode: Opcode,
    args: []const Value,
    results: []const Value,
};

/// Value registers - multiple registers for a single value.
pub fn ValueRegs(comptime R: type) type {
    return struct {
        regs_array: [2]R,
        len: u8,

        const Self = @This();

        pub fn invalid() Self {
            return .{
                .regs_array = .{ R.invalid(), R.invalid() },
                .len = 0,
            };
        }

        pub fn one(r: R) Self {
            return .{
                .regs_array = .{ r, R.invalid() },
                .len = 1,
            };
        }

        pub fn two(r1: R, r2: R) Self {
            return .{
                .regs_array = .{ r1, r2 },
                .len = 2,
            };
        }

        pub fn regs(self: *const Self) []const R {
            return self.regs_array[0..self.len];
        }

        pub fn isValid(self: Self) bool {
            return self.len > 0;
        }

        pub fn isInvalid(self: Self) bool {
            return self.len == 0;
        }

        pub fn length(self: Self) usize {
            return self.len;
        }

        pub fn onlyReg(self: Self) ?R {
            if (self.len == 1) return self.regs_array[0];
            return null;
        }
    };
}

/// Instruction output - vector of value register sets.
pub const InstOutput = std.BoundedArray(ValueRegs(Reg), 2);

/// Non-register input information.
pub const NonRegInput = struct {
    constant: ?u64,
};

/// CLIF type stub.
pub const ClifType = struct {
    kind: enum {
        i8,
        i16,
        i32,
        i64,
        f32,
        f64,
        invalid,
    },

    pub fn bits(self: ClifType) u16 {
        return switch (self.kind) {
            .i8 => 8,
            .i16 => 16,
            .i32 => 32,
            .i64 => 64,
            .f32 => 32,
            .f64 => 64,
            .invalid => 0,
        };
    }

    pub fn int8() ClifType {
        return .{ .kind = .i8 };
    }
    pub fn int16() ClifType {
        return .{ .kind = .i16 };
    }
    pub fn int32() ClifType {
        return .{ .kind = .i32 };
    }
    pub fn int64() ClifType {
        return .{ .kind = .i64 };
    }
    pub fn float32() ClifType {
        return .{ .kind = .f32 };
    }
    pub fn float64() ClifType {
        return .{ .kind = .f64 };
    }
};

// =============================================================================
// Lower context stub
// This matches the interface from machinst/lower.zig's Lower struct.
// In production, this would be the actual Lower(Inst) type.
// =============================================================================

/// Lowering context providing access to the function being lowered.
pub const LowerCtx = struct {
    allocator: Allocator,

    /// Get instruction data for a CLIF instruction.
    pub fn data(_: *LowerCtx, _: ClifInst) *const InstructionData {
        return &placeholder_inst_data;
    }

    /// Get the type of an output.
    pub fn outputTy(_: *LowerCtx, _: ClifInst, _: usize) ClifType {
        return ClifType.int64();
    }

    /// Get the type of an input.
    pub fn inputTy(_: *LowerCtx, _: ClifInst, _: usize) ClifType {
        return ClifType.int64();
    }

    /// Put an input value into registers.
    pub fn putInputInRegs(_: *LowerCtx, _: ClifInst, _: usize) ValueRegs(Reg) {
        // Return a placeholder register
        const vreg_val = VReg.init(200, .int);
        return ValueRegs(Reg).one(Reg.fromVReg(vreg_val));
    }

    /// Get input as source or constant.
    pub fn getInputAsSourceOrConst(_: *LowerCtx, _: ClifInst, _: usize) NonRegInput {
        return .{ .constant = null };
    }

    /// Allocate a temporary register.
    pub fn allocTmp(_: *LowerCtx, _: ClifType) !ValueRegs(Writable(Reg)) {
        const vreg_val = VReg.init(201, .int);
        return ValueRegs(Writable(Reg)).one(Writable(Reg).fromReg(Reg.fromVReg(vreg_val)));
    }

    /// Emit a machine instruction.
    pub fn emit(_: *LowerCtx, _: Inst) !void {
        // Placeholder - would add to instruction buffer
    }

    var placeholder_inst_data = InstructionData{
        .opcode = .nop,
        .args = &[_]Value{},
        .results = &[_]Value{},
    };
};

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
                // Jump table - not yet implemented
                return null;
            },
            .@"return" => {
                // Return from function
                ctx.emit(.ret) catch return null;
            },
            else => return null,
        }
        _ = self;
    }

    /// Get the pinned register, if any.
    pub fn maybePinnedReg(_: *const Self) ?Reg {
        return null;
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

        // Get constant value from instruction (placeholder - would extract from data)
        const value: u64 = 0; // TODO: extract from inst_data

        // Allocate destination register
        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // Emit move wide instruction
        if (MoveWideConst.maybe(value, size)) |imm| {
            ctx.emit(Inst{
                .mov_wide = .{
                    .op = .movz,
                    .rd = dst_reg,
                    .imm = imm,
                    .size = size,
                },
            }) catch return null;
        } else {
            // For larger constants, we need multiple instructions
            // This is a simplified version - full implementation would use multiple movk
            ctx.emit(Inst{
                .mov_wide = .{
                    .op = .movz,
                    .rd = dst_reg,
                    .imm = MoveWideConst.maybeWithShift(@truncate(value), 0) orelse return null,
                    .size = size,
                },
            }) catch return null;
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
            if (Imm12.maybe(@intCast(c))) |imm12| {
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

        const dst = ctx.allocTmp(ClifType.int8()) catch return null;
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

        // TODO: Get the actual condition code from the instruction data
        // For now, default to eq
        const cond = Cond.eq;

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

        const dst = ctx.allocTmp(ty) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // For FP constants, we typically load from a constant pool
        // or use fmov with immediate (limited to certain values)
        // This is a placeholder - full implementation would check
        // if the constant fits in fmov immediate or use constant pool
        ctx.emit(Inst{
            .mov_wide = .{
                .op = .movz,
                .rd = Writable(Reg).fromReg(dst_reg.toReg()),
                .imm = MoveWideConst.maybeWithShift(0, 0) orelse return null,
                .size = .size64,
            },
        }) catch return null;

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

        const dst = ctx.allocTmp(ClifType.int8()) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // FPU compare
        ctx.emit(Inst{
            .fpu_cmp = .{
                .size = size,
                .rn = lhs_reg,
                .rm = rhs_reg,
            },
        }) catch return null;

        // TODO: Get actual condition from instruction
        ctx.emit(Inst{
            .cset = .{
                .rd = dst_reg,
                .cond = .eq,
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
        _ = self;
        _ = ir_inst;

        // TODO: Full call lowering requires ABI handling
        // This is a placeholder
        ctx.emit(Inst{
            .call = .{
                .dest = .{ .label = MachLabel.fromBlock(BlockIndex.new(0)) },
            },
        }) catch return null;

        return InstOutput{};
    }

    fn lowerCallIndirect(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;

        const callee = ctx.putInputInRegs(ir_inst, 0);
        const callee_reg = callee.onlyReg() orelse return null;

        ctx.emit(Inst{
            .call_ind = .{
                .rn = callee_reg,
            },
        }) catch return null;

        return InstOutput{};
    }

    // =========================================================================
    // Traps
    // =========================================================================

    fn lowerTrap(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;
        _ = ir_inst;

        ctx.emit(Inst{
            .udf = .{
                .trap_code = 0,
            },
        }) catch return null;

        return InstOutput{};
    }

    fn lowerTrapnz(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;

        const cond = ctx.putInputInRegs(ir_inst, 0);
        const cond_reg = cond.onlyReg() orelse return null;

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
                .trap_code = 0,
            },
        }) catch return null;

        return InstOutput{};
    }

    fn lowerTrapz(self: *const Self, ctx: *LowerCtx, ir_inst: ClifInst) ?InstOutput {
        _ = self;

        const cond = ctx.putInputInRegs(ir_inst, 0);
        const cond_reg = cond.onlyReg() orelse return null;

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
                .trap_code = 0,
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

        const dst = ctx.allocTmp(ClifType.int64()) catch return null;
        const dst_reg = dst.onlyReg() orelse return null;

        // TODO: Load actual function address
        // This is a placeholder using adr
        ctx.emit(Inst{
            .adr = .{
                .rd = dst_reg,
                .label = .{ .got_entry = 0 },
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

        // TODO: Get stack slot from instruction data
        const mem = AMode{ .sp_offset = .{ .offset = 0 } };
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

        // TODO: Get stack slot from instruction data
        const mem = AMode{ .sp_offset = .{ .offset = 0 } };
        const flags = MemFlags.empty;

        ctx.emit(Inst.genStore(mem, val_reg, typeFromClif(val_ty), flags)) catch return null;

        return InstOutput{};
    }
};

// =============================================================================
// Helper functions
// =============================================================================

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
fn typeFromClif(ty: ClifType) Type {
    return switch (ty.kind) {
        .i8 => Type{ .kind = .i8 },
        .i16 => Type{ .kind = .i16 },
        .i32 => Type{ .kind = .i32 },
        .i64 => Type{ .kind = .i64 },
        .f32 => Type{ .kind = .f32 },
        .f64 => Type{ .kind = .f64 },
        .invalid => Type{ .kind = .i64 },
    };
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
pub fn condFromFloatCC(cc: FloatCC) Cond {
    return switch (cc) {
        .eq => .eq,
        .ne => .ne,
        .lt => .lt,
        .le => .le,
        .gt => .gt,
        .ge => .ge,
        .ord => .vc, // no overflow (no NaN)
        .uno => .vs, // overflow (NaN)
        else => .eq, // TODO: Handle other cases
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

    try testing.expectEqual(OperandSize.size32, operandSizeFromType(ClifType.int8()).?);
    try testing.expectEqual(OperandSize.size32, operandSizeFromType(ClifType.int16()).?);
    try testing.expectEqual(OperandSize.size32, operandSizeFromType(ClifType.int32()).?);
    try testing.expectEqual(OperandSize.size64, operandSizeFromType(ClifType.int64()).?);
}

test "scalarSizeFromType" {
    const testing = std.testing;

    try testing.expectEqual(ScalarSize.size32, scalarSizeFromType(ClifType.float32()).?);
    try testing.expectEqual(ScalarSize.size64, scalarSizeFromType(ClifType.float64()).?);
}

test "condFromIntCC" {
    const testing = std.testing;

    try testing.expectEqual(Cond.eq, condFromIntCC(.eq));
    try testing.expectEqual(Cond.ne, condFromIntCC(.ne));
    try testing.expectEqual(Cond.lt, condFromIntCC(.slt));
    try testing.expectEqual(Cond.lo, condFromIntCC(.ult));
}
