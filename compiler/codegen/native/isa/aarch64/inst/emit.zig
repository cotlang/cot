//! AArch64 ISA: binary code emission.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/inst/emit.rs`

const std = @import("std");
const Allocator = std.mem.Allocator;

const mod = @import("mod.zig");
const args = @import("args.zig");
const imms = @import("imms.zig");
const regs = @import("regs.zig");

// Re-export types we use
pub const Inst = mod.Inst;
pub const ALUOp = mod.ALUOp;
pub const ALUOp3 = mod.ALUOp3;
pub const BitOp = mod.BitOp;
pub const FPUOp1 = mod.FPUOp1;
pub const FPUOp2 = mod.FPUOp2;
pub const FPUOp3 = mod.FPUOp3;
pub const FpuRoundMode = mod.FpuRoundMode;
pub const FpuToIntOp = mod.FpuToIntOp;
pub const IntToFpuOp = mod.IntToFpuOp;
pub const MoveWideOp = mod.MoveWideOp;
pub const AtomicRMWOp = mod.AtomicRMWOp;
pub const AtomicRMWLoopOp = mod.AtomicRMWLoopOp;
pub const AMode = mod.AMode;
pub const PairAMode = mod.PairAMode;
pub const MemFlags = mod.MemFlags;
pub const VecALUOp = mod.VecALUOp;
pub const VecALUModOp = mod.VecALUModOp;
pub const VecMisc2 = mod.VecMisc2;
pub const VecLanesOp = mod.VecLanesOp;
pub const VecShiftImmOp = mod.VecShiftImmOp;
pub const VecShiftImmModOp = mod.VecShiftImmModOp;
pub const VecExtendOp = mod.VecExtendOp;
pub const VecRRLongOp = mod.VecRRLongOp;
pub const VecRRNarrowOp = mod.VecRRNarrowOp;
pub const VecRRRLongOp = mod.VecRRRLongOp;
pub const VecRRRLongModOp = mod.VecRRRLongModOp;
pub const VecRRPairLongOp = mod.VecRRPairLongOp;

pub const Reg = args.Reg;
pub const PReg = args.PReg;
pub const RegClass = args.RegClass;
pub const Cond = args.Cond;
pub const CondBrKind = args.CondBrKind;
pub const BranchTarget = args.BranchTarget;
pub const OperandSize = args.OperandSize;
pub const ScalarSize = args.ScalarSize;
pub const VectorSize = args.VectorSize;
pub const ExtendOp = args.ExtendOp;
pub const ShiftOp = args.ShiftOp;
pub const MachLabel = args.MachLabel;
pub const MemLabel = args.MemLabel;
pub const TestBitAndBranchKind = args.TestBitAndBranchKind;
pub const BranchTargetType = args.BranchTargetType;
pub const Type = args.Type;

pub const Imm12 = imms.Imm12;
pub const ImmLogic = imms.ImmLogic;
pub const ImmShift = imms.ImmShift;
pub const MoveWideConst = imms.MoveWideConst;
pub const SImm9 = imms.SImm9;
pub const SImm7Scaled = imms.SImm7Scaled;
pub const UImm12Scaled = imms.UImm12Scaled;
pub const UImm5 = imms.UImm5;
pub const NZCV = imms.NZCV;

pub const Writable = regs.Writable;
pub const xreg = regs.xreg;
pub const vreg = regs.vreg;
pub const zeroReg = regs.zeroReg;
pub const stackReg = regs.stackReg;
pub const fpReg = regs.fpReg;
pub const spilltmpReg = regs.spilltmpReg;
pub const tmp2Reg = regs.tmp2Reg;
pub const writableXreg = regs.writableXreg;
pub const writableZeroReg = regs.writableZeroReg;
pub const writableStackReg = regs.writableStackReg;
pub const writableSilltmpReg = regs.writableSilltmpReg;
pub const writableTmp2Reg = regs.writableTmp2Reg;

//=============================================================================
// Type helpers - convert CLIF Type to ARM64 size encodings
//=============================================================================

/// Convert CLIF Type to ARM64 size encoding (for atomic ops, loads, stores).
/// Returns: 0b11 for 64-bit, 0b10 for 32-bit, 0b01 for 16-bit, 0b00 for 8-bit.
fn typeToSizeEnc(ty: Type) u32 {
    if (ty.eql(Type.I64)) return 0b11;
    if (ty.eql(Type.I32)) return 0b10;
    if (ty.eql(Type.I16)) return 0b01;
    if (ty.eql(Type.I8)) return 0b00;
    unreachable;
}

/// Check if type is 64-bit integer.
fn typeIsI64(ty: Type) bool {
    return ty.eql(Type.I64);
}

/// Check if type is 8-bit integer.
fn typeIsI8(ty: Type) bool {
    return ty.eql(Type.I8);
}

/// Check if type is 16-bit integer.
fn typeIsI16(ty: Type) bool {
    return ty.eql(Type.I16);
}

/// Check if type is 32-bit integer.
fn typeIsI32(ty: Type) bool {
    return ty.eql(Type.I32);
}

/// Get the bit width of a type (for sign extension calculations).
/// Mirrors Cranelift's ty.bits() usage.
fn typeBits(ty: Type) u8 {
    return @intCast(ty.bits());
}

//=============================================================================
// AArch64 Label Use - matches Cranelift's LabelUse for ARM64
//=============================================================================

const buffer = @import("../../../machinst/buffer.zig");

/// ARM64 label use types with ranges and patching behavior.
/// Matches Cranelift's `cranelift/codegen/src/isa/aarch64/inst/emit.rs` LabelUse.
pub const AArch64LabelUse = struct {
    kind: Kind,
    /// Maximum positive range (forward reference).
    max_pos_range: buffer.CodeOffset,
    /// Maximum negative range (backward reference).
    max_neg_range: buffer.CodeOffset,
    /// Patch size in bytes.
    patch_size: usize,
    /// Whether this use supports veneers.
    supports_veneer: bool,

    pub const Kind = enum {
        /// 19-bit PC-relative branch (B.cond, CBZ, CBNZ).
        /// Range: ±1MB.
        branch19,
        /// 26-bit PC-relative branch (B, BL).
        /// Range: ±128MB.
        branch26,
        /// 14-bit PC-relative branch (TBZ, TBNZ).
        /// Range: ±32KB.
        branch14,
        /// 32-bit PC-relative for ADR (±4GB in practice, using imm21).
        pcRel32,
        /// 19-bit PC-relative for LDR literal.
        /// Range: ±1MB.
        ldr19,
    };

    /// Branch19: ±1MB (19 bits signed, shifted by 2).
    pub const branch19 = AArch64LabelUse{
        .kind = .branch19,
        .max_pos_range = (1 << 20) - 4, // 1MB - 4
        .max_neg_range = 1 << 20, // 1MB
        .patch_size = 4,
        .supports_veneer = true,
    };

    /// Branch26: ±128MB (26 bits signed, shifted by 2).
    pub const branch26 = AArch64LabelUse{
        .kind = .branch26,
        .max_pos_range = (1 << 27) - 4, // 128MB - 4
        .max_neg_range = 1 << 27, // 128MB
        .patch_size = 4,
        .supports_veneer = true,
    };

    /// Branch14: ±32KB (14 bits signed, shifted by 2).
    pub const branch14 = AArch64LabelUse{
        .kind = .branch14,
        .max_pos_range = (1 << 15) - 4, // 32KB - 4
        .max_neg_range = 1 << 15, // 32KB
        .patch_size = 4,
        .supports_veneer = true,
    };

    /// PCRel32: Used for ADR instruction (21-bit immediate).
    pub const pcRel32 = AArch64LabelUse{
        .kind = .pcRel32,
        .max_pos_range = (1 << 20) - 4, // 1MB - 4
        .max_neg_range = 1 << 20, // 1MB
        .patch_size = 4,
        .supports_veneer = false,
    };

    /// Ldr19: LDR literal (19 bits signed, shifted by 2).
    pub const ldr19 = AArch64LabelUse{
        .kind = .ldr19,
        .max_pos_range = (1 << 20) - 4, // 1MB - 4
        .max_neg_range = 1 << 20, // 1MB
        .patch_size = 4,
        .supports_veneer = false,
    };

    /// Patch the instruction at use_offset to branch/reference label_offset.
    pub fn patch(self: AArch64LabelUse, buf: []u8, use_offset: buffer.CodeOffset, label_offset: buffer.CodeOffset) void {
        const pc_rel = @as(i64, @intCast(label_offset)) - @as(i64, @intCast(use_offset));

        switch (self.kind) {
            .branch19 => {
                // B.cond, CBZ, CBNZ: imm19 at bits [23:5], shifted left by 2.
                const imm19: u32 = @bitCast(@as(i32, @intCast(pc_rel >> 2)) & 0x7FFFF);
                const insn = std.mem.readInt(u32, buf[use_offset..][0..4], .little);
                const patched = (insn & 0xFF00001F) | (imm19 << 5);
                std.mem.writeInt(u32, buf[use_offset..][0..4], patched, .little);
            },
            .branch26 => {
                // B, BL: imm26 at bits [25:0], shifted left by 2.
                const imm26: u32 = @bitCast(@as(i32, @intCast(pc_rel >> 2)) & 0x3FFFFFF);
                const insn = std.mem.readInt(u32, buf[use_offset..][0..4], .little);
                const patched = (insn & 0xFC000000) | imm26;
                std.mem.writeInt(u32, buf[use_offset..][0..4], patched, .little);
            },
            .branch14 => {
                // TBZ, TBNZ: imm14 at bits [18:5], shifted left by 2.
                const imm14: u32 = @bitCast(@as(i32, @intCast(pc_rel >> 2)) & 0x3FFF);
                const insn = std.mem.readInt(u32, buf[use_offset..][0..4], .little);
                const patched = (insn & 0xFFF8001F) | (imm14 << 5);
                std.mem.writeInt(u32, buf[use_offset..][0..4], patched, .little);
            },
            .pcRel32 => {
                // ADR: immlo at bits [30:29], immhi at bits [23:5].
                const imm21: i32 = @intCast(pc_rel);
                const immlo: u32 = @bitCast(imm21 & 0x3);
                const immhi: u32 = @bitCast((imm21 >> 2) & 0x7FFFF);
                const insn = std.mem.readInt(u32, buf[use_offset..][0..4], .little);
                const patched = (insn & 0x9F00001F) | (immlo << 29) | (immhi << 5);
                std.mem.writeInt(u32, buf[use_offset..][0..4], patched, .little);
            },
            .ldr19 => {
                // LDR literal: imm19 at bits [23:5], shifted left by 2.
                const imm19: u32 = @bitCast(@as(i32, @intCast(pc_rel >> 2)) & 0x7FFFF);
                const insn = std.mem.readInt(u32, buf[use_offset..][0..4], .little);
                const patched = (insn & 0xFF00001F) | (imm19 << 5);
                std.mem.writeInt(u32, buf[use_offset..][0..4], patched, .little);
            },
        }
    }
};

/// Machine buffer for AArch64 - uses the common MachBuffer with AArch64LabelUse.
pub const MachBuffer = buffer.MachBuffer(AArch64LabelUse);

//=============================================================================
// Emission state
//=============================================================================

/// State carried between emissions of a sequence of instructions.
pub const EmitState = struct {
    /// Frame layout information
    frame_layout: FrameLayout = .{},

    pub fn frameLayout(self: *const EmitState) *const FrameLayout {
        return &self.frame_layout;
    }
};

/// Frame layout information.
pub const FrameLayout = struct {
    setup_area_size: u32 = 0,
    tail_args_size: u32 = 0,
    clobber_size: u32 = 0,
    fixed_frame_storage_size: u32 = 0,
    outgoing_args_size: u32 = 0,
    stackslots_size: u32 = 0,
};

/// Constant state used during function compilation.
pub const EmitInfo = struct {
    /// Compiler flags
    flags: Flags = .{},

    pub const Flags = struct {
        // Add relevant flags here
    };

    pub fn init(flags: Flags) EmitInfo {
        return .{ .flags = flags };
    }
};

//=============================================================================
// Address mode finalization
//=============================================================================

/// Result of mem_finalize: instructions to emit before the memory access, and the finalized AMode.
pub const MemFinalizeResult = struct {
    /// Instructions to emit before the memory operation (for large offsets).
    pre_insts: [4]?Inst,
    pre_inst_count: u8,
    /// The finalized addressing mode.
    amode: AMode,
};

/// Convert pseudo addressing modes (SPOffset, FPOffset, etc.) to real addressing modes.
/// For large offsets, this may generate instructions to load the offset into a temp register.
pub fn memFinalize(mem: AMode, state: *const EmitState) MemFinalizeResult {
    var result = MemFinalizeResult{
        .pre_insts = .{ null, null, null, null },
        .pre_inst_count = 0,
        .amode = mem,
    };

    switch (mem) {
        .sp_offset => |m| {
            const off = m.off;
            const basereg = stackReg();
            result.amode = finalizeOffset(off, basereg, &result);
        },
        .fp_offset => |m| {
            const off = m.off;
            const basereg = fpReg();
            result.amode = finalizeOffset(off, basereg, &result);
        },
        .slot_offset => |m| {
            // Slot offsets need adjustment for outgoing args area
            const adj = @as(i64, state.frame_layout.outgoing_args_size);
            const off = m.off + adj;
            const basereg = stackReg();
            result.amode = finalizeOffset(off, basereg, &result);
        },
        .incoming_arg => |m| {
            // Incoming args are above the frame
            const fl = state.frame_layout;
            const total_frame: i64 = @as(i64, fl.setup_area_size) +
                @as(i64, fl.tail_args_size) +
                @as(i64, fl.clobber_size) +
                @as(i64, fl.fixed_frame_storage_size) +
                @as(i64, fl.outgoing_args_size);
            const off = total_frame - m.off;
            const basereg = stackReg();
            result.amode = finalizeOffset(off, basereg, &result);
        },
        else => {
            // Already a real addressing mode, return as-is
        },
    }

    return result;
}

fn finalizeOffset(off: i64, basereg: Reg, result: *MemFinalizeResult) AMode {
    // Try SImm9 encoding (-256 to 255)
    if (off >= -256 and off <= 255) {
        return AMode{ .unscaled = .{
            .rn = basereg,
            .simm9 = SImm9.maybeFromI64(off).?,
        } };
    }
    // Try UImm12 scaled encoding (0 to 32760 for 8-byte access)
    if (off >= 0 and off <= 32760 and @mod(off, 8) == 0) {
        return AMode{ .unsigned_offset = .{
            .rn = basereg,
            .uimm12 = UImm12Scaled.maybeFromI64(off, 8).?,
        } };
    }
    // Large offset: load into spilltmp register
    const tmp = spilltmpReg();
    const tmp_wr = Writable(Reg).fromReg(tmp);

    // Generate MOVZ/MOVK sequence to load the offset
    // For now, handle 16-bit positive offsets with MOVZ
    if (off >= 0 and off <= 0xFFFF) {
        const imm = MoveWideConst.maybeFromU64(@intCast(off)).?;
        result.pre_insts[0] = Inst{ .mov_wide = .{
            .op = .movz,
            .rd = tmp_wr,
            .imm = imm,
            .size = .size64,
        } };
        result.pre_inst_count = 1;
    } else {
        // For larger offsets, we'd need MOVZ + MOVK sequence
        // For now, just use MOVZ with truncated value (TODO: fix for large frames)
        const imm = MoveWideConst.maybeFromU64(@intCast(off & 0xFFFF)).?;
        result.pre_insts[0] = Inst{ .mov_wide = .{
            .op = .movz,
            .rd = tmp_wr,
            .imm = imm,
            .size = .size64,
        } };
        result.pre_inst_count = 1;
    }

    return AMode{ .reg_extended = .{
        .rn = basereg,
        .rm = tmp,
        .extendop = .sxtx,
    } };
}

//=============================================================================
// Register encoding helpers
//=============================================================================

/// Convert a register to its GPR encoding (5 bits).
pub fn machregToGpr(m: Reg) u32 {
    std.debug.assert(m.class() == .int);
    if (m.toRealReg()) |rreg| {
        return @as(u32, rreg.hwEnc() & 31);
    }
    unreachable; // Virtual register not allowed here
}

/// Convert a register to its vector/FP encoding (5 bits).
pub fn machregToVec(m: Reg) u32 {
    std.debug.assert(m.class() == .float);
    if (m.toRealReg()) |rreg| {
        return @as(u32, rreg.hwEnc());
    }
    unreachable;
}

/// Convert a register to its encoding regardless of class.
fn machregToGprOrVec(m: Reg) u32 {
    if (m.toRealReg()) |rreg| {
        return @as(u32, rreg.hwEnc() & 31);
    }
    unreachable;
}

//=============================================================================
// Instruction encoding helpers
//=============================================================================

/// Encode a 3-register arithmetic instruction.
pub fn encArithRrr(bits_31_21: u32, bits_15_10: u32, rd: Writable(Reg), rn: Reg, rm: Reg) u32 {
    return (bits_31_21 << 21) |
        (bits_15_10 << 10) |
        machregToGpr(rd.toReg()) |
        (machregToGpr(rn) << 5) |
        (machregToGpr(rm) << 16);
}

/// Encode an arithmetic instruction with register and 12-bit immediate.
fn encArithRrImm12(bits_31_24: u32, immshift: u32, imm12_val: u32, rn: Reg, rd: Writable(Reg)) u32 {
    return (bits_31_24 << 24) |
        (immshift << 22) |
        (imm12_val << 10) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode an arithmetic instruction with register and logical immediate.
fn encArithRrImml(bits_31_23: u32, imm_bits: u32, rn: Reg, rd: Writable(Reg)) u32 {
    return (bits_31_23 << 23) |
        (imm_bits << 10) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode a 4-register arithmetic instruction.
fn encArithRrrr(top11: u32, rm: Reg, bit15: u32, ra: Reg, rn: Reg, rd: Writable(Reg)) u32 {
    return (top11 << 21) |
        (machregToGpr(rm) << 16) |
        (bit15 << 15) |
        (machregToGpr(ra) << 10) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode a 26-bit jump offset.
fn encJump26(op_31_26: u32, off_26_0: u32) u32 {
    std.debug.assert(off_26_0 < (1 << 26));
    return (op_31_26 << 26) | off_26_0;
}

/// Encode a compare-and-branch instruction.
fn encCmpbr(op_31_24: u32, off_18_0: u32, reg: Reg) u32 {
    std.debug.assert(off_18_0 < (1 << 19));
    return (op_31_24 << 24) | (off_18_0 << 5) | machregToGpr(reg);
}

/// Encode a conditional branch instruction.
fn encCbr(op_31_24: u32, off_18_0: u32, op_4: u32, cond: u32) u32 {
    std.debug.assert(off_18_0 < (1 << 19));
    std.debug.assert(cond < (1 << 4));
    return (op_31_24 << 24) | (off_18_0 << 5) | (op_4 << 4) | cond;
}

/// Set the size bit of an instruction.
fn encOpSize(op: u32, size: OperandSize) u32 {
    return (op & ~@as(u32, 1 << 31)) | (@as(u32, size.sfBit()) << 31);
}

/// Encode a conditional branch.
fn encConditionalBr(taken: BranchTarget, kind: CondBrKind) u32 {
    return switch (kind) {
        .zero => |payload| encOpSize(
            encCmpbr(0b0_011010_0, taken.asOffset19OrZero(), payload.reg),
            payload.size,
        ),
        .not_zero => |payload| encOpSize(
            encCmpbr(0b0_011010_1, taken.asOffset19OrZero(), payload.reg),
            payload.size,
        ),
        .cond => |c| encCbr(0b01010100, taken.asOffset19OrZero(), 0b0, c.bits()),
    };
}

/// Encode a move-wide instruction.
pub fn encMoveWide(op: MoveWideOp, rd: Writable(Reg), imm_const: MoveWideConst, size: OperandSize) u32 {
    std.debug.assert(imm_const.shift <= 0b11);

    // MOVK has a different base opcode (0x72800000) than MOVZ/MOVN (0x12800000)
    if (op == .movk) {
        return 0x72800000 |
            (@as(u32, size.sfBit()) << 31) |
            (@as(u32, imm_const.shift) << 21) |
            (@as(u32, imm_const.bits_val) << 5) |
            machregToGpr(rd.toReg());
    }

    const op_bits: u32 = switch (op) {
        .movn => 0b00,
        .movz => 0b10,
        .movk => unreachable, // Handled above
    };
    return 0x12800000 |
        (@as(u32, size.sfBit()) << 31) |
        (op_bits << 29) |
        (@as(u32, imm_const.shift) << 21) |
        (@as(u32, imm_const.bits_val) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode a MOVK instruction.
pub fn encMovk(rd: Writable(Reg), imm_const: MoveWideConst, size: OperandSize) u32 {
    std.debug.assert(imm_const.shift <= 0b11);
    return 0x72800000 |
        (@as(u32, size.sfBit()) << 31) |
        (@as(u32, imm_const.shift) << 21) |
        (@as(u32, imm_const.bits_val) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode a load/store pair instruction.
fn encLdstPair(op_31_22: u32, simm7: SImm7Scaled, rn: Reg, rt: Reg, rt2: Reg) u32 {
    return (op_31_22 << 22) |
        (@as(u32, simm7.bits()) << 15) |
        (machregToGpr(rt2) << 10) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rt);
}

/// Encode a load/store with signed 9-bit immediate.
fn encLdstSimm9(op_31_22: u32, simm9: SImm9, op_11_10: u32, rn: Reg, rd: Reg) u32 {
    return (op_31_22 << 22) |
        (@as(u32, simm9.bits()) << 12) |
        (op_11_10 << 10) |
        (machregToGpr(rn) << 5) |
        machregToGprOrVec(rd);
}

/// Encode a load/store with unsigned 12-bit immediate.
fn encLdstUimm12(op_31_22: u32, uimm12: UImm12Scaled, rn: Reg, rd: Reg) u32 {
    return (op_31_22 << 22) |
        (0b1 << 24) |
        (@as(u32, uimm12.bits()) << 10) |
        (machregToGpr(rn) << 5) |
        machregToGprOrVec(rd);
}

/// Encode a load/store with register offset.
fn encLdstReg(op_31_22: u32, rn: Reg, rm: Reg, s_bit: bool, extendop: ?ExtendOp, rd: Reg) u32 {
    const s: u32 = if (s_bit) 1 else 0;
    const extend_bits: u32 = if (extendop) |ext| switch (ext) {
        .uxtw => 0b010,
        .sxtw => 0b110,
        .sxtx => 0b111,
        else => @panic("bad extend mode for ld/st AMode"),
    } else 0b011; // LSL

    return (op_31_22 << 22) |
        (1 << 21) |
        (machregToGpr(rm) << 16) |
        (extend_bits << 13) |
        (s << 12) |
        (0b10 << 10) |
        (machregToGpr(rn) << 5) |
        machregToGprOrVec(rd);
}

/// Encode a load/store with 19-bit immediate (PC-relative).
pub fn encLdstImm19(op_31_24: u32, imm19: u32, rd: Reg) u32 {
    return (op_31_24 << 24) | (imm19 << 5) | machregToGprOrVec(rd);
}

/// Encode a bit manipulation instruction.
fn encBitRr(size: u32, opcode2: u32, opcode1: u32, rn: Reg, rd: Writable(Reg)) u32 {
    return (0b01011010110 << 21) |
        (size << 31) |
        (opcode2 << 16) |
        (opcode1 << 10) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode a branch register instruction.
pub fn encBr(rn: Reg) u32 {
    return 0b1101011_0000_11111_000000_00000_00000 | (machregToGpr(rn) << 5);
}

/// Encode an ADR instruction.
pub fn encAdrInst(opcode: u32, off: i32, rd: Writable(Reg)) u32 {
    const off_u: u32 = @bitCast(off);
    const immlo = off_u & 3;
    const immhi = (off_u >> 2) & ((1 << 19) - 1);
    return opcode | (immlo << 29) | (immhi << 5) | machregToGpr(rd.toReg());
}

/// Encode an ADR instruction.
pub fn encAdr(off: i32, rd: Writable(Reg)) u32 {
    const opcode: u32 = 0b00010000 << 24;
    return encAdrInst(opcode, off, rd);
}

/// Encode an ADRP instruction.
pub fn encAdrp(off: i32, rd: Writable(Reg)) u32 {
    const opcode: u32 = 0b10010000 << 24;
    return encAdrInst(opcode, off, rd);
}

/// Encode a conditional select instruction.
fn encCsel(rd: Writable(Reg), rn: Reg, rm: Reg, cond: Cond, op: u32, o2: u32) u32 {
    std.debug.assert(op & 0b1 == op);
    std.debug.assert(o2 & 0b1 == o2);
    return 0b100_11010100_00000_0000_00_00000_00000 |
        (op << 30) |
        (machregToGpr(rm) << 16) |
        (@as(u32, cond.bits()) << 12) |
        (o2 << 10) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode a floating-point conditional select instruction.
fn encFcsel(rd: Writable(Reg), rn: Reg, rm: Reg, cond: Cond, size: ScalarSize) u32 {
    return 0b000_11110_00_1_00000_0000_11_00000_00000 |
        (@as(u32, size.ftype()) << 22) |
        (machregToVec(rm) << 16) |
        (machregToVec(rn) << 5) |
        machregToVec(rd.toReg()) |
        (@as(u32, cond.bits()) << 12);
}

/// Encode a floating-point compare instruction.
fn encFcmp(size: ScalarSize, rn: Reg, rm: Reg) u32 {
    return 0b000_11110_00_1_00000_00_1000_00000_00000 |
        (@as(u32, size.ftype()) << 22) |
        (machregToVec(rm) << 16) |
        (machregToVec(rn) << 5);
}

/// Encode a bitfield move instruction.
fn encBfm(opc: u8, size: OperandSize, rd: Writable(Reg), rn: Reg, immr: u8, imms_val: u8) u32 {
    switch (size) {
        .size64 => {
            std.debug.assert(immr <= 63);
            std.debug.assert(imms_val <= 63);
        },
        .size32 => {
            std.debug.assert(immr <= 31);
            std.debug.assert(imms_val <= 31);
        },
    }
    std.debug.assert(opc & 0b11 == opc);
    const n_bit = size.sfBit();
    return 0b0_00_100110_0_000000_000000_00000_00000 |
        (@as(u32, size.sfBit()) << 31) |
        (@as(u32, opc) << 29) |
        (@as(u32, n_bit) << 22) |
        (@as(u32, immr) << 16) |
        (@as(u32, imms_val) << 10) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rd.toReg());
}

/// Encode a vector move instruction.
fn encVecmov(is_16b: bool, rd: Writable(Reg), rn: Reg) u32 {
    return 0b00001110_101_00000_00011_1_00000_00000 |
        (@as(u32, @intFromBool(is_16b)) << 30) |
        machregToVec(rd.toReg()) |
        (machregToVec(rn) << 16) |
        (machregToVec(rn) << 5);
}

/// Encode a FPU register-register instruction.
fn encFpurr(top22: u32, rd: Writable(Reg), rn: Reg) u32 {
    return (top22 << 10) | (machregToVec(rn) << 5) | machregToVec(rd.toReg());
}

/// Encode a FPU 3-register instruction.
fn encFpurrr(top22: u32, rd: Writable(Reg), rn: Reg, rm: Reg) u32 {
    return (top22 << 10) |
        (machregToVec(rm) << 16) |
        (machregToVec(rn) << 5) |
        machregToVec(rd.toReg());
}

/// Encode a FPU 4-register instruction.
fn encFpurrrr(top17: u32, rd: Writable(Reg), rn: Reg, rm: Reg, ra: Reg) u32 {
    return (top17 << 15) |
        (machregToVec(rm) << 16) |
        (machregToVec(ra) << 10) |
        (machregToVec(rn) << 5) |
        machregToVec(rd.toReg());
}

/// Encode a FPU to integer conversion.
fn encFputoint(top16: u32, rd: Writable(Reg), rn: Reg) u32 {
    return (top16 << 16) | (machregToVec(rn) << 5) | machregToGpr(rd.toReg());
}

/// Encode an integer to FPU conversion.
fn encInttofpu(top16: u32, rd: Writable(Reg), rn: Reg) u32 {
    return (top16 << 16) | (machregToGpr(rn) << 5) | machregToVec(rd.toReg());
}

/// Encode a FPU rounding instruction.
fn encFround(top22: u32, rd: Writable(Reg), rn: Reg) u32 {
    return (top22 << 10) | (machregToVec(rn) << 5) | machregToVec(rd.toReg());
}

/// Encode a vector 3-register instruction.
fn encVecRrr(top11: u32, rm: Reg, bit15_10: u32, rn: Reg, rd: Writable(Reg)) u32 {
    return (top11 << 21) |
        (machregToVec(rm) << 16) |
        (bit15_10 << 10) |
        (machregToVec(rn) << 5) |
        machregToVec(rd.toReg());
}

/// Encode a DMB ISH instruction.
fn encDmbIsh() u32 {
    return 0xD5033BBF;
}

/// Helper to emit FPU load/store instructions.
fn emitFpuLoadStore(sink: *MachBuffer, op: u32, rd: Reg, mem: AMode, is_load: bool) !void {
    _ = is_load;
    switch (mem) {
        .unscaled => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b00, m.rn, rd));
        },
        .unsigned_offset => |m| {
            try sink.put4(encLdstUimm12(op, m.uimm12, m.rn, rd));
        },
        .reg_reg => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, false, null, rd));
        },
        .reg_scaled => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, true, null, rd));
        },
        .reg_scaled_extended => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, true, m.extendop, rd));
        },
        .reg_extended => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, false, m.extendop, rd));
        },
        .sp_pre_indexed => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b11, stackReg(), rd));
        },
        .sp_post_indexed => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b01, stackReg(), rd));
        },
        .label, .fp_offset, .sp_offset, .incoming_arg, .slot_offset, .reg_offset, .constant => {
            @panic("Pseudo addressing mode not resolved before emission");
        },
    }
}

//=============================================================================
// Atomic instruction encoding helpers
//=============================================================================

/// Encode an atomic read-modify-write instruction using LSE atomics.
/// This encodes LDADDAL, LDCLRAL, LDEORAL, LDSETAL, LDSMAXAL, LDUMAXAL, LDSMINAL, LDUMINAL, SWPAL.
fn encAtomicRmw(ty: Type, op: AtomicRMWOp, rs: Reg, rt: Writable(Reg), rn: Reg) u32 {
    std.debug.assert(machregToGpr(rt.toReg()) != 31);
    const sz: u32 = typeToSizeEnc(ty);
    const bit15: u32 = switch (op) {
        .swp => 0b1,
        else => 0b0,
    };
    const op_bits: u32 = switch (op) {
        .add => 0b000,
        .clr => 0b001,
        .eor => 0b010,
        .set => 0b011,
        .smax => 0b100,
        .smin => 0b101,
        .umax => 0b110,
        .umin => 0b111,
        .swp => 0b000,
    };
    return 0b00_111_000_111_00000_0_000_00_00000_00000 |
        (sz << 30) |
        (machregToGpr(rs) << 16) |
        (bit15 << 15) |
        (op_bits << 12) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rt.toReg());
}

/// Encode LDAR (load-acquire register).
fn encLdar(ty: Type, rt: Writable(Reg), rn: Reg) u32 {
    const sz: u32 = typeToSizeEnc(ty);
    return 0b00_001000_1_1_0_11111_1_11111_00000_00000 |
        (sz << 30) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rt.toReg());
}

/// Encode STLR (store-release register).
fn encStlr(ty: Type, rt: Reg, rn: Reg) u32 {
    const sz: u32 = typeToSizeEnc(ty);
    return 0b00_001000_100_11111_1_11111_00000_00000 |
        (sz << 30) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rt);
}

/// Encode LDAXR (load-acquire exclusive register).
fn encLdaxr(ty: Type, rt: Writable(Reg), rn: Reg) u32 {
    const sz: u32 = typeToSizeEnc(ty);
    return 0b00_001000_0_1_0_11111_1_11111_00000_00000 |
        (sz << 30) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rt.toReg());
}

/// Encode STLXR (store-release exclusive register).
fn encStlxr(ty: Type, rs: Writable(Reg), rt: Reg, rn: Reg) u32 {
    const sz: u32 = typeToSizeEnc(ty);
    return 0b00_001000_000_00000_1_11111_00000_00000 |
        (sz << 30) |
        (machregToGpr(rs.toReg()) << 16) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rt);
}

/// Encode CAS (compare-and-swap) instruction.
fn encCas(ty: Type, rs: Writable(Reg), rt: Reg, rn: Reg) u32 {
    const sz: u32 = typeToSizeEnc(ty);
    return 0b00_0010001_1_1_00000_1_11111_00000_00000 |
        (sz << 30) |
        (machregToGpr(rs.toReg()) << 16) |
        (machregToGpr(rn) << 5) |
        machregToGpr(rt);
}

//=============================================================================
// Load/Store helper functions
//=============================================================================

/// Helper function for emitting load instructions.
fn emitLoad(sink: *MachBuffer, rd: Writable(Reg), mem: AMode, op: u32) !void {
    const rd_reg = rd.toReg();
    switch (mem) {
        .unscaled => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b00, m.rn, rd_reg));
        },
        .unsigned_offset => |m| {
            try sink.put4(encLdstUimm12(op, m.uimm12, m.rn, rd_reg));
        },
        .reg_reg => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, false, null, rd_reg));
        },
        .reg_scaled => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, true, null, rd_reg));
        },
        .reg_scaled_extended => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, true, m.extendop, rd_reg));
        },
        .reg_extended => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, false, m.extendop, rd_reg));
        },
        .sp_pre_indexed => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b11, stackReg(), rd_reg));
        },
        .sp_post_indexed => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b01, stackReg(), rd_reg));
        },
        .label, .fp_offset, .sp_offset, .incoming_arg, .slot_offset, .reg_offset, .constant => {
            @panic("Pseudo addressing mode not resolved before emission");
        },
    }
}

/// Helper function for emitting store instructions.
fn emitStore(sink: *MachBuffer, rd: Reg, mem: AMode, op: u32) !void {
    switch (mem) {
        .unscaled => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b00, m.rn, rd));
        },
        .unsigned_offset => |m| {
            try sink.put4(encLdstUimm12(op, m.uimm12, m.rn, rd));
        },
        .reg_reg => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, false, null, rd));
        },
        .reg_scaled => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, true, null, rd));
        },
        .reg_scaled_extended => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, true, m.extendop, rd));
        },
        .reg_extended => |m| {
            try sink.put4(encLdstReg(op, m.rn, m.rm, false, m.extendop, rd));
        },
        .sp_pre_indexed => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b11, stackReg(), rd));
        },
        .sp_post_indexed => |m| {
            try sink.put4(encLdstSimm9(op, m.simm9, 0b01, stackReg(), rd));
        },
        .label => {
            @panic("Store to a MemLabel not supported");
        },
        .fp_offset, .sp_offset, .incoming_arg, .slot_offset, .reg_offset, .constant => {
            @panic("Pseudo addressing mode not resolved before emission");
        },
    }
}

//=============================================================================
// Main emission function
//=============================================================================

/// Emit an instruction to the buffer.
pub fn emit(inst: *const Inst, sink: *MachBuffer, emit_info: *const EmitInfo, _: *EmitState) !void {
    _ = emit_info;

    switch (inst.*) {
        .alu_rrr => |payload| {
            const top11: u32 = switch (payload.alu_op) {
                .add => 0b00001011_000,
                .adc => 0b00011010_000,
                .adcs => 0b00111010_000,
                .sub => 0b01001011_000,
                .sbc => 0b01011010_000,
                .sbcs => 0b01111010_000,
                .orr => 0b00101010_000,
                .@"and" => 0b00001010_000,
                .ands => 0b01101010_000,
                .eor => 0b01001010_000,
                .orr_not => 0b00101010_001,
                .and_not => 0b00001010_001,
                .eor_not => 0b01001010_001,
                .adds => 0b00101011_000,
                .subs => 0b01101011_000,
                .sdiv, .udiv => 0b00011010_110,
                .extr, .lsr, .asr, .lsl => 0b00011010_110,
                .smulh => 0b10011011_010,
                .umulh => 0b10011011_110,
            };

            const top11_sized = top11 | (@as(u32, payload.size.sfBit()) << 10);
            const bit15_10: u32 = switch (payload.alu_op) {
                .sdiv => 0b000011,
                .udiv => 0b000010,
                .extr => 0b001011,
                .lsr => 0b001001,
                .asr => 0b001010,
                .lsl => 0b001000,
                .smulh, .umulh => 0b011111,
                else => 0b000000,
            };

            try sink.put4(encArithRrr(top11_sized, bit15_10, payload.rd, payload.rn, payload.rm));
        },

        .alu_rrrr => |payload| {
            const result = switch (payload.alu_op) {
                .madd => .{ @as(u32, 0b0_00_11011_000), @as(u32, 0) },
                .msub => .{ @as(u32, 0b0_00_11011_000), @as(u32, 1) },
                .umaddl => .{ @as(u32, 0b1_00_11011_1_01), @as(u32, 0) },
                .smaddl => .{ @as(u32, 0b1_00_11011_0_01), @as(u32, 0) },
            };
            const top11 = result[0];
            const bit15 = result[1];
            const top11_sized = top11 | (@as(u32, payload.size.sfBit()) << 10);
            try sink.put4(encArithRrrr(top11_sized, payload.rm, bit15, payload.ra, payload.rn, payload.rd));
        },

        .alu_rr_imm12 => |payload| {
            const top8: u32 = switch (payload.alu_op) {
                .add => 0b000_10001,
                .sub => 0b010_10001,
                .adds => 0b001_10001,
                .subs => 0b011_10001,
                else => @panic("Unsupported ALU op for imm12"),
            };
            const top8_sized = top8 | (@as(u32, payload.size.sfBit()) << 7);
            try sink.put4(encArithRrImm12(
                top8_sized,
                payload.imm12.shiftBits(),
                payload.imm12.immBits(),
                payload.rn,
                payload.rd,
            ));
        },

        .alu_rr_imm_logic => |payload| {
            const result = switch (payload.alu_op) {
                .orr => .{ @as(u32, 0b001_100100), false },
                .@"and" => .{ @as(u32, 0b000_100100), false },
                .ands => .{ @as(u32, 0b011_100100), false },
                .eor => .{ @as(u32, 0b010_100100), false },
                .orr_not => .{ @as(u32, 0b001_100100), true },
                .and_not => .{ @as(u32, 0b000_100100), true },
                .eor_not => .{ @as(u32, 0b010_100100), true },
                else => @panic("Unsupported ALU op for immLogic"),
            };
            const top9 = result[0];
            const inv = result[1];
            const top9_sized = top9 | (@as(u32, payload.size.sfBit()) << 8);
            const imml = if (inv) payload.imml.invert() else payload.imml;
            try sink.put4(encArithRrImml(top9_sized, imml.encBits(), payload.rn, payload.rd));
        },

        .bit_rr => |payload| {
            const result = switch (payload.op) {
                .rbit => .{ @as(u32, 0b00000), @as(u32, 0b000000) },
                .clz => .{ @as(u32, 0b00000), @as(u32, 0b000100) },
                .cls => .{ @as(u32, 0b00000), @as(u32, 0b000101) },
                .rev16 => .{ @as(u32, 0b00000), @as(u32, 0b000001) },
                .rev32 => .{ @as(u32, 0b00000), @as(u32, 0b000010) },
                .rev64 => .{ @as(u32, 0b00000), @as(u32, 0b000011) },
            };
            const op1 = result[0];
            const op2 = result[1];
            try sink.put4(encBitRr(payload.size.sfBit(), op1, op2, payload.rn, payload.rd));
        },

        .mov_wide => |payload| {
            try sink.put4(encMoveWide(payload.op, payload.rd, payload.imm, payload.size));
        },

        .movk => |payload| {
            try sink.put4(encMovk(payload.rd, payload.imm, payload.size));
        },

        // Move from physical register - used after regalloc
        .mov_from_preg => |payload| {
            // This is a pseudo-op that should have been resolved to a real move.
            // If we get here, the preg should be directly usable.
            const rm_enc = @as(u32, payload.rm.hwEnc()) & 31;
            const rd_enc = machregToGpr(payload.rd.toReg());
            // ORR rd, xzr, rm (64-bit move)
            try sink.put4(0xaa000000 | (rm_enc << 16) | rd_enc);
        },

        // Move to physical register - used during regalloc
        .mov_to_preg => |payload| {
            // This is a pseudo-op for moving to a physical register.
            const rd_enc = @as(u32, payload.rd.hwEnc()) & 31;
            const rm_enc = machregToGpr(payload.rm);
            // ORR rd, xzr, rm (64-bit move)
            try sink.put4(0xaa000000 | (rm_enc << 16) | rd_enc);
        },

        .mov => |payload| {
            std.debug.assert(payload.rd.toReg().class() == payload.rm.class());
            std.debug.assert(payload.rm.class() == .int);

            switch (payload.size) {
                .size64 => {
                    std.debug.assert(payload.rd.toReg().bits != stackReg().bits);
                    if (payload.rm.bits == stackReg().bits) {
                        // Use add rd, sp, #0 instead of ORR
                        const imm12 = Imm12.maybeFromU64(0).?;
                        try sink.put4(encArithRrImm12(
                            0b100_10001,
                            imm12.shiftBits(),
                            imm12.immBits(),
                            payload.rm,
                            payload.rd,
                        ));
                    } else {
                        // Encoded as ORR rd, rm, zero.
                        try sink.put4(encArithRrr(0b10101010_000, 0b000_000, payload.rd, zeroReg(), payload.rm));
                    }
                },
                .size32 => {
                    // Encoded as ORR rd, rm, zero.
                    try sink.put4(encArithRrr(0b00101010_000, 0b000_000, payload.rd, zeroReg(), payload.rm));
                },
            }
        },

        .csel => |payload| {
            try sink.put4(encCsel(payload.rd, payload.rn, payload.rm, payload.cond, 0, 0));
        },

        .csneg => |payload| {
            try sink.put4(encCsel(payload.rd, payload.rn, payload.rm, payload.cond, 1, 1));
        },

        .cset => |payload| {
            try sink.put4(encCsel(payload.rd, zeroReg(), zeroReg(), payload.cond.invert(), 0, 1));
        },

        .csetm => |payload| {
            try sink.put4(encCsel(payload.rd, zeroReg(), zeroReg(), payload.cond.invert(), 1, 0));
        },

        .jump => |payload| {
            const off = sink.curOffset();
            if (payload.dest.asLabel()) |l| {
                try sink.useLabelAtOffset(off, l, .branch26);
            }
            try sink.put4(encJump26(0b000101, payload.dest.asOffset26OrZero()));
        },

        .cond_br => |payload| {
            // Conditional part first
            const cond_off = sink.curOffset();
            if (payload.taken.asLabel()) |l| {
                try sink.useLabelAtOffset(cond_off, l, .branch19);
            }
            try sink.put4(encConditionalBr(payload.taken, payload.kind));

            // Unconditional part next
            const uncond_off = sink.curOffset();
            if (payload.not_taken.asLabel()) |l| {
                try sink.useLabelAtOffset(uncond_off, l, .branch26);
            }
            try sink.put4(encJump26(0b000101, payload.not_taken.asOffset26OrZero()));
        },

        .ret => {
            try sink.put4(0xd65f03c0);
        },

        .indirect_br => |payload| {
            try sink.put4(encBr(payload.rn));
        },

        .nop0 => {
            // Emit nothing
        },

        .nop4 => {
            try sink.put4(0xd503201f);
        },

        .brk => {
            try sink.put4(0xd43e0000);
        },

        .udf => |payload| {
            // UDF with imm16 trap code
            try sink.put4(0x00000000 | @as(u32, payload.trap_code));
        },

        .adr => |payload| {
            const off: i32 = switch (payload.label) {
                .pc_rel => |o| o,
                .mach => |l| blk: {
                    // Register relocation for later resolution
                    try sink.useLabelAtOffset(sink.curOffset(), l, .pcRel32);
                    break :blk 0;
                },
            };
            std.debug.assert(off > -(1 << 20));
            std.debug.assert(off < (1 << 20));
            try sink.put4(encAdr(off, payload.rd));
        },

        .adrp => |payload| {
            const off: i32 = switch (payload.label) {
                .pc_rel => |o| o,
                .mach => |l| blk: {
                    try sink.useLabelAtOffset(sink.curOffset(), l, .pcRel32);
                    break :blk 0;
                },
            };
            std.debug.assert(off > -(1 << 20));
            std.debug.assert(off < (1 << 20));
            try sink.put4(encAdrp(off, payload.rd));
        },

        .word4 => |payload| {
            try sink.put4(payload.data);
        },

        .word8 => |payload| {
            try sink.put8(payload.data);
        },

        .fence => {
            try sink.put4(encDmbIsh());
        },

        .csdb => {
            try sink.put4(0xd503229f);
        },

        .fpu_move32 => |payload| {
            try sink.put4(encFpurr(0b000_11110_00_1_000000_10000, payload.rd, payload.rn));
        },

        .fpu_move64 => |payload| {
            try sink.put4(encFpurr(0b000_11110_01_1_000000_10000, payload.rd, payload.rn));
        },

        .fpu_move128 => |payload| {
            try sink.put4(encVecmov(true, payload.rd, payload.rn));
        },

        .fpu_cmp => |payload| {
            try sink.put4(encFcmp(payload.size, payload.rn, payload.rm));
        },

        // FPU unary operations
        .fpu_rr => |payload| {
            const ftype = @as(u32, payload.size.ftype());
            const enc: u32 = switch (payload.fpu_op) {
                .abs => 0b000_11110_00_1_000001_10000, // FABS
                .neg => 0b000_11110_00_1_000010_10000, // FNEG
                .sqrt => 0b000_11110_00_1_000011_10000, // FSQRT
                .cvt32_to_64 => 0b000_11110_00_1_00010_1_10000, // FCVT S->D
                .cvt64_to_32 => 0b000_11110_01_1_00010_0_10000, // FCVT D->S
            };
            // For cvt operations, ftype is already encoded in the opcode
            const adjusted_enc = switch (payload.fpu_op) {
                .cvt32_to_64, .cvt64_to_32 => enc,
                else => enc | (ftype << 22),
            };
            try sink.put4(encFpurr(adjusted_enc >> 10, payload.rd, payload.rn));
        },

        // FPU binary operations
        .fpu_rrr => |payload| {
            const ftype = @as(u32, payload.size.ftype());
            const opcode: u32 = switch (payload.fpu_op) {
                .add => 0b001010, // FADD
                .sub => 0b001110, // FSUB
                .mul => 0b000010, // FMUL
                .div => 0b000110, // FDIV
                .max => 0b010010, // FMAX
                .min => 0b010110, // FMIN
            };
            // 000 11110 ty 1 Rm opcode Rn Rd
            try sink.put4(encFpurrr(
                0b000_11110_00_1 | (ftype << 0),
                payload.rd,
                payload.rn,
                payload.rm,
            ) | (opcode << 10));
        },

        // FPU ternary operations (fused multiply-add/sub)
        .fpu_rrrr => |payload| {
            const ftype = @as(u32, payload.size.ftype());
            const o1: u32 = switch (payload.fpu_op) {
                .madd => 0, // FMADD
                .msub => 1, // FMSUB
            };
            // 000 11111 ty o1 Rm o0 Ra Rn Rd
            try sink.put4(encFpurrrr(
                0b000_11111_00 | (ftype << 0) | (o1 << 5),
                payload.rd,
                payload.rn,
                payload.rm,
                payload.ra,
            ));
        },

        // FPU rounding
        .fpu_round => |payload| {
            const ftype = @as(u32, payload.size.ftype());
            const rmode: u32 = switch (payload.mode) {
                .nearest => 0b00, // FRINTN
                .plus_infinity => 0b01, // FRINTP
                .minus_infinity => 0b10, // FRINTM
                .zero => 0b11, // FRINTZ
            };
            // 000 11110 ty 1 001 00 rmode 10000 Rn Rd
            try sink.put4(
                0b000_11110_00_1_001_00_00_10000_00000_00000 |
                    (ftype << 22) |
                    (rmode << 12) |
                    (machregToVec(payload.rn) << 5) |
                    machregToVec(payload.rd.toReg()),
            );
        },

        // FPU to integer conversion
        .fpu_to_int => |payload| {
            const enc: u32 = switch (payload.op) {
                .f32_to_u32 => 0b0_00_11110_00_1_11_001_000000, // FCVTZU Wd, Sn
                .f32_to_i32 => 0b0_00_11110_00_1_11_000_000000, // FCVTZS Wd, Sn
                .f32_to_u64 => 0b1_00_11110_00_1_11_001_000000, // FCVTZU Xd, Sn
                .f32_to_i64 => 0b1_00_11110_00_1_11_000_000000, // FCVTZS Xd, Sn
                .f64_to_u32 => 0b0_00_11110_01_1_11_001_000000, // FCVTZU Wd, Dn
                .f64_to_i32 => 0b0_00_11110_01_1_11_000_000000, // FCVTZS Wd, Dn
                .f64_to_u64 => 0b1_00_11110_01_1_11_001_000000, // FCVTZU Xd, Dn
                .f64_to_i64 => 0b1_00_11110_01_1_11_000_000000, // FCVTZS Xd, Dn
            };
            try sink.put4(encFputoint(enc >> 16, payload.rd, payload.rn));
        },

        // Integer to FPU conversion
        .int_to_fpu => |payload| {
            const enc: u32 = switch (payload.op) {
                .u32_to_f32 => 0b0_00_11110_00_1_00_011_000000, // UCVTF Sd, Wn
                .i32_to_f32 => 0b0_00_11110_00_1_00_010_000000, // SCVTF Sd, Wn
                .u64_to_f32 => 0b1_00_11110_00_1_00_011_000000, // UCVTF Sd, Xn
                .i64_to_f32 => 0b1_00_11110_00_1_00_010_000000, // SCVTF Sd, Xn
                .u32_to_f64 => 0b0_00_11110_01_1_00_011_000000, // UCVTF Dd, Wn
                .i32_to_f64 => 0b0_00_11110_01_1_00_010_000000, // SCVTF Dd, Wn
                .u64_to_f64 => 0b1_00_11110_01_1_00_011_000000, // UCVTF Dd, Xn
                .i64_to_f64 => 0b1_00_11110_01_1_00_010_000000, // SCVTF Dd, Xn
            };
            try sink.put4(encInttofpu(enc >> 16, payload.rd, payload.rn));
        },

        // FPU loads
        .fpu_load32 => |payload| {
            const op: u32 = 0b1011110001; // LDR (32-bit FP)
            try emitFpuLoadStore(sink, op, payload.rd.toReg(), payload.mem, true);
        },

        .fpu_load64 => |payload| {
            const op: u32 = 0b1111110001; // LDR (64-bit FP)
            try emitFpuLoadStore(sink, op, payload.rd.toReg(), payload.mem, true);
        },

        .fpu_load128 => |payload| {
            const op: u32 = 0b0011110011; // LDR (128-bit FP/SIMD)
            try emitFpuLoadStore(sink, op, payload.rd.toReg(), payload.mem, true);
        },

        // FPU stores
        .fpu_store32 => |payload| {
            const op: u32 = 0b1011110000; // STR (32-bit FP)
            try emitFpuLoadStore(sink, op, payload.rd, payload.mem, false);
        },

        .fpu_store64 => |payload| {
            const op: u32 = 0b1111110000; // STR (64-bit FP)
            try emitFpuLoadStore(sink, op, payload.rd, payload.mem, false);
        },

        .fpu_store128 => |payload| {
            const op: u32 = 0b0011110010; // STR (128-bit FP/SIMD)
            try emitFpuLoadStore(sink, op, payload.rd, payload.mem, false);
        },

        .mov_to_nzcv => |payload| {
            try sink.put4(0xd51b4200 | machregToGpr(payload.rn));
        },

        .mov_from_nzcv => |payload| {
            try sink.put4(0xd53b4200 | machregToGpr(payload.rd.toReg()));
        },

        // Load instructions
        .uload8 => |payload| try emitLoad(sink, payload.rd, payload.mem, 0b0011100001),
        .sload8 => |payload| try emitLoad(sink, payload.rd, payload.mem, 0b0011100010),
        .uload16 => |payload| try emitLoad(sink, payload.rd, payload.mem, 0b0111100001),
        .sload16 => |payload| try emitLoad(sink, payload.rd, payload.mem, 0b0111100010),
        .uload32 => |payload| try emitLoad(sink, payload.rd, payload.mem, 0b1011100001),
        .sload32 => |payload| try emitLoad(sink, payload.rd, payload.mem, 0b1011100010),
        .uload64 => |payload| try emitLoad(sink, payload.rd, payload.mem, 0b1111100001),

        // Store instructions
        .store8 => |payload| try emitStore(sink, payload.rd, payload.mem, 0b0011100000),
        .store16 => |payload| try emitStore(sink, payload.rd, payload.mem, 0b0111100000),
        .store32 => |payload| try emitStore(sink, payload.rd, payload.mem, 0b1011100000),
        .store64 => |payload| try emitStore(sink, payload.rd, payload.mem, 0b1111100000),

        // Load pair (64-bit)
        .load_p64 => |payload| {
            const rt = payload.rt.toReg();
            const rt2 = payload.rt2.toReg();
            switch (payload.mem) {
                .signed_offset => |m| {
                    try sink.put4(encLdstPair(0b1010100101, m.simm7, m.reg, rt, rt2));
                },
                .sp_pre_indexed => |m| {
                    try sink.put4(encLdstPair(0b1010100111, m.simm7, stackReg(), rt, rt2));
                },
                .sp_post_indexed => |m| {
                    try sink.put4(encLdstPair(0b1010100011, m.simm7, stackReg(), rt, rt2));
                },
            }
        },

        // Store pair (64-bit)
        .store_p64 => |payload| {
            switch (payload.mem) {
                .signed_offset => |m| {
                    try sink.put4(encLdstPair(0b1010100100, m.simm7, m.reg, payload.rt, payload.rt2));
                },
                .sp_pre_indexed => |m| {
                    try sink.put4(encLdstPair(0b1010100110, m.simm7, stackReg(), payload.rt, payload.rt2));
                },
                .sp_post_indexed => |m| {
                    try sink.put4(encLdstPair(0b1010100010, m.simm7, stackReg(), payload.rt, payload.rt2));
                },
            }
        },

        // ALU operation with shift immediate: rd = rn op imm_shift
        .alu_rr_imm_shift => |payload| {
            // LSL, LSR, ASR with immediate
            const is_lsl = payload.alu_op == .lsl;
            const is_asr = payload.alu_op == .asr;

            // These are aliases for UBFM/SBFM
            const size_bits: u8 = switch (payload.size) {
                .size32 => 31,
                .size64 => 63,
            };
            const immr: u8 = if (is_lsl)
                @intCast((size_bits + 1 - payload.immshift.imm) & size_bits)
            else
                payload.immshift.imm;
            const imms_val: u8 = if (is_lsl)
                @intCast(size_bits - payload.immshift.imm)
            else
                size_bits;
            const opc: u8 = if (is_asr) 0b00 else 0b10; // SBFM for ASR, UBFM for LSL/LSR
            try sink.put4(encBfm(opc, payload.size, payload.rd, payload.rn, immr, imms_val));
        },

        // ALU operation with shifted register: rd = rn op (rm shift amt)
        .alu_rrr_shift => |payload| {
            const shift_enc: u32 = switch (payload.shiftop.op) {
                .lsl => 0b00,
                .lsr => 0b01,
                .asr => 0b10,
                .ror => 0b11,
            };
            const top11: u32 = switch (payload.alu_op) {
                .add => 0b00001011_000 | (shift_enc << 0),
                .sub => 0b01001011_000 | (shift_enc << 0),
                .adds => 0b00101011_000 | (shift_enc << 0),
                .subs => 0b01101011_000 | (shift_enc << 0),
                .orr => 0b00101010_000 | (shift_enc << 0),
                .@"and" => 0b00001010_000 | (shift_enc << 0),
                .ands => 0b01101010_000 | (shift_enc << 0),
                .eor => 0b01001010_000 | (shift_enc << 0),
                .orr_not => 0b00101010_001 | (shift_enc << 0),
                .and_not => 0b00001010_001 | (shift_enc << 0),
                .eor_not => 0b01001010_001 | (shift_enc << 0),
                else => @panic("Unsupported ALU op for shifted register"),
            };
            const top11_sized = top11 | (@as(u32, payload.size.sfBit()) << 10);
            const bit15_10 = @as(u32, payload.shiftop.shift.value) & 0x3f;
            try sink.put4(encArithRrr(top11_sized, bit15_10, payload.rd, payload.rn, payload.rm));
        },

        // ALU operation with extended register: rd = rn op ext(rm)
        .alu_rrr_extend => |payload| {
            const ext_enc: u32 = switch (payload.extendop) {
                .uxtb => 0b000,
                .uxth => 0b001,
                .uxtw => 0b010,
                .uxtx => 0b011,
                .sxtb => 0b100,
                .sxth => 0b101,
                .sxtw => 0b110,
                .sxtx => 0b111,
            };
            const top11: u32 = switch (payload.alu_op) {
                .add => 0b00001011_001,
                .sub => 0b01001011_001,
                .adds => 0b00101011_001,
                .subs => 0b01101011_001,
                else => @panic("Unsupported ALU op for extended register"),
            };
            const top11_sized = top11 | (@as(u32, payload.size.sfBit()) << 10);
            // Extended register encoding: option(3) | imm3(3)
            const bit15_10 = (ext_enc << 3) | 0; // imm3=0 (no additional shift)
            try sink.put4(encArithRrr(top11_sized, bit15_10, payload.rd, payload.rn, payload.rm));
        },

        // Conditional compare register
        .ccmp => |payload| {
            const sf_bit = @as(u32, payload.size.sfBit());
            try sink.put4(
                (sf_bit << 31) |
                    0b1111010010 << 21 |
                    (machregToGpr(payload.rm) << 16) |
                    (@as(u32, payload.cond.bits()) << 12) |
                    0b00 << 10 |
                    (machregToGpr(payload.rn) << 5) |
                    0b0 << 4 |
                    @as(u32, payload.nzcv.bits()),
            );
        },

        // Conditional compare immediate
        .ccmp_imm => |payload| {
            const sf_bit = @as(u32, payload.size.sfBit());
            try sink.put4(
                (sf_bit << 31) |
                    0b1111010010 << 21 |
                    (@as(u32, payload.imm.bits()) << 16) |
                    (@as(u32, payload.cond.bits()) << 12) |
                    0b10 << 10 |
                    (machregToGpr(payload.rn) << 5) |
                    0b0 << 4 |
                    @as(u32, payload.nzcv.bits()),
            );
        },

        // Sign/zero extend
        .extend => |payload| {
            const rd = payload.rd;
            const rn = payload.rn;

            if (payload.signed) {
                // SBFM for sign extension
                const opc: u8 = 0b00;
                const immr: u8 = 0;
                const imms_sbfm: u8 = payload.from_bits - 1;
                const size: OperandSize = if (payload.to_bits <= 32) .size32 else .size64;
                try sink.put4(encBfm(opc, size, rd, rn, immr, imms_sbfm));
            } else {
                // UBFM for zero extension (or AND with mask)
                if (payload.from_bits == 8 and payload.to_bits == 32) {
                    // UXTB (alias for UBFM Wd, Wn, #0, #7)
                    try sink.put4(encBfm(0b10, .size32, rd, rn, 0, 7));
                } else if (payload.from_bits == 16 and payload.to_bits == 32) {
                    // UXTH (alias for UBFM Wd, Wn, #0, #15)
                    try sink.put4(encBfm(0b10, .size32, rd, rn, 0, 15));
                } else if (payload.from_bits == 32 and payload.to_bits == 64) {
                    // Zero-extend 32 to 64: just use MOV (ORR with zero)
                    try sink.put4(encArithRrr(0b00101010_000, 0b000_000, rd, zeroReg(), rn));
                } else {
                    const imms_ubfm: u8 = payload.from_bits - 1;
                    const size_ext: OperandSize = if (payload.to_bits <= 32) .size32 else .size64;
                    try sink.put4(encBfm(0b10, size_ext, rd, rn, 0, imms_ubfm));
                }
            }
        },

        // Test bit and branch
        .test_bit_and_branch => |payload| {
            const off = sink.curOffset();
            if (payload.taken.asLabel()) |l| {
                try sink.useLabelAtOffset(off, l, .branch14);
            }
            const imm14 = payload.taken.asOffset14OrZero();
            const b5 = (@as(u32, payload.bit) >> 5) & 1;
            const b40 = @as(u32, payload.bit) & 0x1f;
            const op: u32 = switch (payload.kind) {
                .z => 0,
                .nz => 1,
            };
            try sink.put4(
                (b5 << 31) |
                    (0b011011 << 25) |
                    (op << 24) |
                    (b40 << 19) |
                    (imm14 << 5) |
                    machregToGpr(payload.rn),
            );
            // Fall through to not_taken
            const uncond_off = sink.curOffset();
            if (payload.not_taken.asLabel()) |l| {
                try sink.useLabelAtOffset(uncond_off, l, .branch26);
            }
            try sink.put4(encJump26(0b000101, payload.not_taken.asOffset26OrZero()));
        },

        // Trap if condition
        .trap_if => |payload| {
            // Emit conditional branch over the trap
            const skip_label = try sink.getLabel();
            const inverted = payload.kind.invert();
            const cond_off = sink.curOffset();
            _ = cond_off;
            // Branch past the trap if condition is NOT met
            try sink.put4(encConditionalBr(BranchTarget{ .resolved_offset = 8 }, inverted));
            // UDF with trap code
            try sink.put4(0x00000000 | @as(u32, payload.trap_code));
            try sink.bindLabel(skip_label);
        },

        // Load address (pseudo-instruction, generates ADD or similar)
        .load_addr => |payload| {
            switch (payload.mem) {
                .reg_offset => |m| {
                    if (m.offset == 0) {
                        // Simple MOV
                        try sink.put4(encArithRrr(0b10101010_000, 0b000_000, payload.rd, zeroReg(), m.rn));
                    } else if (m.offset >= 0 and m.offset < 4096) {
                        // ADD rd, rn, #offset
                        const imm12 = Imm12.maybeFromU64(@intCast(m.offset)).?;
                        try sink.put4(encArithRrImm12(0b100_10001, imm12.shiftBits(), imm12.immBits(), m.rn, payload.rd));
                    } else if (m.offset < 0 and m.offset > -4096) {
                        // SUB rd, rn, #-offset
                        const imm12 = Imm12.maybeFromU64(@intCast(-m.offset)).?;
                        try sink.put4(encArithRrImm12(0b110_10001, imm12.shiftBits(), imm12.immBits(), m.rn, payload.rd));
                    } else {
                        @panic("load_addr offset too large");
                    }
                },
                .sp_offset => |m| {
                    if (m.offset >= 0 and m.offset < 4096) {
                        const imm12 = Imm12.maybeFromU64(@intCast(m.offset)).?;
                        try sink.put4(encArithRrImm12(0b100_10001, imm12.shiftBits(), imm12.immBits(), stackReg(), payload.rd));
                    } else {
                        @panic("load_addr sp_offset too large");
                    }
                },
                .fp_offset => |m| {
                    if (m.offset >= 0 and m.offset < 4096) {
                        const imm12 = Imm12.maybeFromU64(@intCast(m.offset)).?;
                        try sink.put4(encArithRrImm12(0b100_10001, imm12.shiftBits(), imm12.immBits(), fpReg(), payload.rd));
                    } else {
                        @panic("load_addr fp_offset too large");
                    }
                },
                else => @panic("Unsupported AMode for load_addr"),
            }
        },

        // Branch target identification
        .bti => |payload| {
            const op2: u32 = switch (payload.targets) {
                .none => 0b000,
                .c => 0b010,
                .j => 0b100,
                .jc => 0b110,
            };
            try sink.put4(0xd503201f | (op2 << 6));
        },

        // Call instruction
        .call => |payload| {
            const off = sink.curOffset();
            if (payload.dest.asLabel()) |l| {
                try sink.useLabelAtOffset(off, l, .branch26);
            }
            // BL encoding: 1 00101 imm26
            try sink.put4(encJump26(0b100101, payload.dest.asOffset26OrZero()));
        },

        // Call indirect
        .call_ind => |payload| {
            // BLR rn
            try sink.put4(0b1101011_0001_11111_000000_00000_00000 | (machregToGpr(payload.rn) << 5));
        },

        // ==========================================================================
        // Atomic Operations
        // ==========================================================================

        // Atomic RMW using LSE atomics
        .atomic_rmw => |payload| {
            try sink.put4(encAtomicRmw(payload.ty, payload.op, payload.rs, payload.rt, payload.rn));
        },

        // Atomic RMW loop (for systems without LSE) - complex pseudo-instruction
        .atomic_rmw_loop => |payload| {
            // Emit this sequence:
            //   again:
            //     ldaxr{,b,h}  x/w27, [x25]
            //     // maybe sign extend
            //     op          x28, x27, x26 // op is add,sub,and,orr,eor
            //     stlxr{,b,h}  w24, x/w28, [x25]
            //     cbnz        x24, again
            //
            // Operand conventions:
            //   IN:  x25 (addr), x26 (2nd arg for op)
            //   OUT: x27 (old value), x24 (trashed), x28 (trashed)

            const x24 = payload.scratch1; // Success flag
            const x25 = payload.addr; // Address
            const x26 = payload.operand; // 2nd operand
            const x27 = payload.oldval; // Old value (result)
            const x28 = payload.scratch2; // New value

            // Determine operand size from type
            const size: OperandSize = if (typeIsI64(payload.ty)) .size64 else .size32;

            // Get label for loop start
            const again_label = try sink.getLabel();

            // again:
            try sink.bindLabel(again_label);

            // ldaxr x27, [x25]
            try sink.put4(encLdaxr(payload.ty, x27, x25));

            // Sign extension for smin/smax on smaller types
            const need_sign_ext = switch (payload.op) {
                .smin, .smax => typeIsI8(payload.ty) or typeIsI16(payload.ty),
                else => false,
            };

            if (need_sign_ext) {
                // SBFM for sign extension: sxt{b|h} x27, x27
                const from_bits: u8 = typeBits(payload.ty);
                const imms_sbfm: u8 = from_bits - 1;
                try sink.put4(encBfm(0b00, size, x27, x27.toReg(), 0, imms_sbfm));
            }

            // Perform the operation
            switch (payload.op) {
                .xchg => {}, // No operation needed, we'll store x26 directly

                .nand => {
                    // and x28, x27, x26
                    // mvn x28, x28 (orr_not with xzr)
                    try sink.put4(encArithRrr(
                        0b00001010_000 | (@as(u32, size.sfBit()) << 10),
                        0b000000,
                        x28,
                        x27.toReg(),
                        x26,
                    ));
                    // MVN x28, x28 = ORR x28, XZR, x28, LSL #0 then NOT
                    try sink.put4(encArithRrr(
                        0b00101010_001 | (@as(u32, size.sfBit()) << 10),
                        0b000000,
                        x28,
                        zeroReg(),
                        x28.toReg(),
                    ));
                },

                .umin, .umax, .smin, .smax => {
                    // cmp x27, x26 (subs xzr, x27, x26)
                    // csel x28, x27, x26, cond
                    const cond: Cond = switch (payload.op) {
                        .umin => .lo, // unsigned lower
                        .umax => .hi, // unsigned higher
                        .smin => .lt, // signed less than
                        .smax => .gt, // signed greater than
                        else => unreachable,
                    };

                    // CMP (SUBS xzr, x27, x26)
                    try sink.put4(encArithRrr(
                        0b01101011_000 | (@as(u32, size.sfBit()) << 10),
                        0b000000,
                        writableZeroReg(),
                        x27.toReg(),
                        x26,
                    ));

                    // CSEL x28, x27, x26, cond
                    try sink.put4(encCsel(x28, x27.toReg(), x26, cond, 0, 0));
                },

                else => {
                    // add/sub/and/orr/eor x28, x27, x26
                    const alu_enc: u32 = switch (payload.op) {
                        .add => 0b00001011_000,
                        .sub => 0b01001011_000,
                        .@"and" => 0b00001010_000,
                        .orr => 0b00101010_000,
                        .eor => 0b01001010_000,
                        else => unreachable,
                    };
                    try sink.put4(encArithRrr(
                        alu_enc | (@as(u32, size.sfBit()) << 10),
                        0b000000,
                        x28,
                        x27.toReg(),
                        x26,
                    ));
                },
            }

            // stlxr w24, x28, [x25] (or x26 for xchg)
            const store_val = if (payload.op == .xchg) x26 else x28.toReg();
            try sink.put4(encStlxr(payload.ty, x24, store_val, x25));

            // cbnz w24, again
            // CBNZ encoding: sf=0 (32-bit), 011010 1, imm19, Rt
            const br_offset = sink.curOffset();
            try sink.useLabelAtOffset(br_offset, again_label, .branch19);
            // Placeholder branch (imm19=0, will be patched by label resolution)
            try sink.put4(0b0_0110101_0000000000000000000_00000 | machregToGpr(x24.toReg()));
        },

        // Atomic CAS using LSE atomics
        .atomic_cas => |payload| {
            try sink.put4(encCas(payload.ty, payload.rd, payload.rt, payload.rn));
        },

        // Atomic CAS loop (for systems without LSE) - complex pseudo-instruction
        .atomic_cas_loop => |payload| {
            // Emit this sequence:
            //   again:
            //     ldaxr{,b,h} x/w27, [x25]
            //     cmp         x27, x/w26 uxt{b,h}
            //     b.ne        out
            //     stlxr{,b,h} w24, x/w28, [x25]
            //     cbnz        x24, again
            //   out:
            //
            // Operand conventions:
            //   IN:  x25 (addr), x26 (expected value), x28 (replacement value)
            //   OUT: x27 (old value), x24 (trashed)

            const x24 = payload.scratch; // Success flag
            const x25 = payload.addr; // Address
            const x26 = payload.expected; // Expected value
            const x27 = payload.oldval; // Old value (result)
            const x28 = payload.replacement; // Replacement value

            // Get labels
            const again_label = try sink.getLabel();
            const out_label = try sink.getLabel();

            // again:
            try sink.bindLabel(again_label);

            // ldaxr x27, [x25]
            try sink.put4(encLdaxr(payload.ty, x27, x25));

            // cmp x27, x26 (with uxtb/uxth extension for i8/i16)
            // The top 32-bits are zero-extended by ldaxr, so we compare with extension.
            // subs xzr, x27, x26 {uxtb|uxth}
            const is_i8 = typeIsI8(payload.ty);
            const is_i16 = typeIsI16(payload.ty);
            const is_i64 = typeIsI64(payload.ty);

            const extend_enc: u32 = if (is_i8)
                (0b1 << 21) | (0b000 << 13) // uxtb
            else if (is_i16)
                (0b1 << 21) | (0b001 << 13) // uxth
            else
                0; // no extension needed

            if (is_i8 or is_i16) {
                // SUBS with extend: 1 11 01011 00 1 Rm option imm3 Rn Rd
                // For 64-bit with uxtb/uxth extension
                try sink.put4(
                    (0b1_11_01011_00_1 << 21) |
                        (machregToGpr(x26) << 16) |
                        extend_enc |
                        (0b000 << 10) | // imm3=0
                        (machregToGpr(x27.toReg()) << 5) |
                        machregToGpr(zeroReg()),
                );
            } else {
                // Regular SUBS: sf 1101011 000 Rm 000000 Rn Rd
                const sf: u32 = if (is_i64) 1 else 0;
                try sink.put4(
                    (sf << 31) |
                        (0b1101011_000 << 21) |
                        (machregToGpr(x26) << 16) |
                        (0b000000 << 10) |
                        (machregToGpr(x27.toReg()) << 5) |
                        machregToGpr(zeroReg()),
                );
            }

            // b.ne out
            const br_out_offset = sink.curOffset();
            try sink.useLabelAtOffset(br_out_offset, out_label, .branch19);
            // B.cond encoding: 0101010 0 imm19 0 cond
            try sink.put4(0b0101010_0_0000000000000000000_0_0001); // cond=0001 (ne)

            // stlxr w24, x28, [x25]
            try sink.put4(encStlxr(payload.ty, x24, x28, x25));

            // cbnz w24, again
            const br_again_offset = sink.curOffset();
            try sink.useLabelAtOffset(br_again_offset, again_label, .branch19);
            // CBNZ encoding: sf=0 (32-bit), 011010 1, imm19, Rt
            try sink.put4(0b0_0110101_0000000000000000000_00000 | machregToGpr(x24.toReg()));

            // out:
            try sink.bindLabel(out_label);
        },

        // Load-acquire exclusive register
        .ldaxr => |payload| {
            try sink.put4(encLdaxr(payload.ty, payload.rt, payload.rn));
        },

        // Store-release exclusive register
        .stlxr => |payload| {
            try sink.put4(encStlxr(payload.ty, payload.rs, payload.rt, payload.rn));
        },

        // Load-acquire register
        .ldar => |payload| {
            try sink.put4(encLdar(payload.ty, payload.rt, payload.rn));
        },

        // Store-release register
        .stlr => |payload| {
            try sink.put4(encStlr(payload.ty, payload.rt, payload.rn));
        },

        // ==========================================================================
        // Vector/SIMD Operations
        // ==========================================================================

        // Vector 3-register ALU operation
        .vec_rrr => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;
            const enc_size = enc.size;

            // Check if this is a floating-point operation
            const is_float = switch (payload.op) {
                .fcmeq, .fcmgt, .fcmge, .fadd, .fsub, .fdiv, .fmax, .fmin, .fmul, .fmaxnm, .fminnm => true,
                else => false,
            };

            const bits = switch (payload.op) {
                .sqadd => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b000011) },
                .sqsub => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b001011) },
                .uqadd => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b000011) },
                .uqsub => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b001011) },
                .cmeq => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b100011) },
                .cmge => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b001111) },
                .cmgt => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b001101) },
                .cmhi => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b001101) },
                .cmhs => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b001111) },
                .fcmeq => .{ @as(u32, 0b000_01110_00_1), @as(u32, 0b111001) },
                .fcmgt => .{ @as(u32, 0b001_01110_10_1), @as(u32, 0b111001) },
                .fcmge => .{ @as(u32, 0b001_01110_00_1), @as(u32, 0b111001) },
                // Logical ops operate on bytes, not encoded differently for different vector types
                .@"and" => .{ @as(u32, 0b000_01110_00_1), @as(u32, 0b000111) },
                .bic => .{ @as(u32, 0b000_01110_01_1), @as(u32, 0b000111) },
                .orr => .{ @as(u32, 0b000_01110_10_1), @as(u32, 0b000111) },
                .eor => .{ @as(u32, 0b001_01110_00_1), @as(u32, 0b000111) },
                .umaxp => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b101001) },
                .add => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b100001) },
                .sub => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b100001) },
                .mul => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b100111) },
                .sshl => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b010001) },
                .ushl => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b010001) },
                .srshl => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b010101) },
                .urshl => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b010101) },
                .umin => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b011011) },
                .smin => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b011011) },
                .umax => .{ @as(u32, 0b001_01110_00_1) | enc_size << 1, @as(u32, 0b011001) },
                .smax => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b011001) },
                .fadd => .{ @as(u32, 0b000_01110_00_1), @as(u32, 0b110101) },
                .fsub => .{ @as(u32, 0b000_01110_10_1), @as(u32, 0b110101) },
                .fdiv => .{ @as(u32, 0b001_01110_00_1), @as(u32, 0b111111) },
                .fmax => .{ @as(u32, 0b000_01110_00_1), @as(u32, 0b111101) },
                .fmin => .{ @as(u32, 0b000_01110_10_1), @as(u32, 0b111101) },
                .fmaxnm => .{ @as(u32, 0b000_01110_00_1), @as(u32, 0b110001) },
                .fminnm => .{ @as(u32, 0b000_01110_10_1), @as(u32, 0b110001) },
                .fmul => .{ @as(u32, 0b001_01110_00_1), @as(u32, 0b110111) },
                .addp => .{ @as(u32, 0b000_01110_00_1) | enc_size << 1, @as(u32, 0b101111) },
                .bsl => .{ @as(u32, 0b001_01110_01_1), @as(u32, 0b000111) },
                // Long operations - these are more complex, emit BRK for now
                .umlal, .smull, .smull2, .umull, .umull2 => {
                    try sink.put4(0xd43e0000); // BRK - TODO: implement long operations
                    return;
                },
            };
            var top11 = bits[0];
            const bit15_10 = bits[1];

            // For float ops, encode the float size into top11
            if (is_float) {
                top11 |= payload.size.encFloatSize() << 1;
            }

            try sink.put4(encVecRrr(top11 | q << 9, payload.rm, bit15_10, payload.rn, payload.rd));
        },

        // Vector 3-register ALU with modifier
        .vec_rrr_mod => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;

            const bits = switch (payload.op) {
                .sqrdmlah => .{ @as(u32, 0b001_01110_00_1), @as(u32, 0b100001) },
                .sqrdmlsh => .{ @as(u32, 0b001_01110_00_1), @as(u32, 0b100011) },
                .umlal => .{ @as(u32, 0b001_01110_00_1), @as(u32, 0b100000) },
                .fmla => .{ @as(u32, 0b000_01110_00_1) | payload.size.encFloatSize() << 1, @as(u32, 0b110011) },
                .fmls => .{ @as(u32, 0b000_01110_10_1) | payload.size.encFloatSize() << 1, @as(u32, 0b110011) },
            };
            const top11 = bits[0];
            const bit15_10 = bits[1];

            try sink.put4(encVecRrr(top11 | q << 9, payload.rm, bit15_10, payload.rn, payload.rd));
        },

        // Vector 2-operand misc operation
        .vec_misc => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;
            const enc_size = enc.size;

            const bits = switch (payload.op) {
                .not => .{ @as(u32, 0b1), @as(u32, 0b00101), @as(u32, 0b00) },
                .neg => .{ @as(u32, 0b1), @as(u32, 0b01011), enc_size },
                .abs => .{ @as(u32, 0b0), @as(u32, 0b01011), enc_size },
                .fabs => .{ @as(u32, 0b0), @as(u32, 0b01111), payload.size.encFloatSize() | 0b10 },
                .fneg => .{ @as(u32, 0b1), @as(u32, 0b01111), payload.size.encFloatSize() | 0b10 },
                .fsqrt => .{ @as(u32, 0b1), @as(u32, 0b11111), payload.size.encFloatSize() | 0b10 },
                .rev16 => .{ @as(u32, 0b0), @as(u32, 0b00001), @as(u32, 0b00) },
                .rev32 => .{ @as(u32, 0b1), @as(u32, 0b00000), enc_size },
                .rev64 => .{ @as(u32, 0b0), @as(u32, 0b00000), enc_size },
                .fcvtzs => .{ @as(u32, 0b0), @as(u32, 0b11011), payload.size.encFloatSize() | 0b10 },
                .fcvtzu => .{ @as(u32, 0b1), @as(u32, 0b11011), payload.size.encFloatSize() | 0b10 },
                .scvtf => .{ @as(u32, 0b0), @as(u32, 0b11101), enc_size },
                .ucvtf => .{ @as(u32, 0b1), @as(u32, 0b11101), enc_size },
                .frintn => .{ @as(u32, 0b0), @as(u32, 0b11000), payload.size.encFloatSize() | 0b10 },
                .frintz => .{ @as(u32, 0b0), @as(u32, 0b11001), payload.size.encFloatSize() | 0b10 },
                .frintm => .{ @as(u32, 0b0), @as(u32, 0b11001), payload.size.encFloatSize() },
                .frintp => .{ @as(u32, 0b0), @as(u32, 0b11000), payload.size.encFloatSize() },
                .cnt => .{ @as(u32, 0b0), @as(u32, 0b00101), @as(u32, 0b00) },
                .cmeq0 => .{ @as(u32, 0b0), @as(u32, 0b01001), enc_size },
                .cmge0 => .{ @as(u32, 0b1), @as(u32, 0b01000), enc_size },
                .cmgt0 => .{ @as(u32, 0b0), @as(u32, 0b01000), enc_size },
                .cmle0 => .{ @as(u32, 0b1), @as(u32, 0b01001), enc_size },
                .cmlt0 => .{ @as(u32, 0b0), @as(u32, 0b01010), enc_size },
                .fcmeq0 => .{ @as(u32, 0b0), @as(u32, 0b01101), payload.size.encFloatSize() | 0b10 },
                .fcmge0 => .{ @as(u32, 0b1), @as(u32, 0b01100), payload.size.encFloatSize() | 0b10 },
                .fcmgt0 => .{ @as(u32, 0b0), @as(u32, 0b01100), payload.size.encFloatSize() | 0b10 },
                .fcmle0 => .{ @as(u32, 0b1), @as(u32, 0b01101), payload.size.encFloatSize() | 0b10 },
                .fcmlt0 => .{ @as(u32, 0b0), @as(u32, 0b01110), payload.size.encFloatSize() | 0b10 },
            };
            const u = bits[0];
            const opcode = bits[1];
            const size = bits[2];

            // Encoding: 0 Q U 01110 size 10000 opcode 10 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b01110 << 24) |
                (size << 22) |
                (0b10000 << 17) |
                (opcode << 12) |
                (0b10 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector lanes operation
        .vec_lanes => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;
            const enc_size = enc.size;

            const bits = switch (payload.op) {
                .addv => .{ @as(u32, 0b0), @as(u32, 0b11011) },
                .uminv => .{ @as(u32, 0b1), @as(u32, 0b11010) },
                .saddlv => .{ @as(u32, 0b0), @as(u32, 0b00011) },
                .uaddlv => .{ @as(u32, 0b1), @as(u32, 0b00011) },
            };
            const u = bits[0];
            const opcode = bits[1];

            // Encoding: 0 Q U 01110 size 11000 opcode 10 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b01110 << 24) |
                (enc_size << 22) |
                (0b11000 << 17) |
                (opcode << 12) |
                (0b10 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector shift by immediate
        .vec_shift_imm => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;
            const lane_bits: u8 = switch (payload.size.laneSize()) {
                .size8 => 8,
                .size16 => 16,
                .size32 => 32,
                .size64 => 64,
                else => unreachable,
            };

            const bits = switch (payload.op) {
                .shl => .{ @as(u32, 0b0), @as(u32, 0b01010) },
                .sshr => .{ @as(u32, 0b0), @as(u32, 0b00000) },
                .ushr => .{ @as(u32, 0b1), @as(u32, 0b00000) },
            };
            const u = bits[0];
            const opcode = bits[1];

            // Calculate immh:immb encoding
            // For SHL: immh:immb = lane_bits + shift_amount
            // For SSHR/USHR: immh:immb = 2*lane_bits - shift_amount
            const shift_enc: u32 = switch (payload.op) {
                .shl => @as(u32, lane_bits) + @as(u32, payload.imm),
                .sshr, .ushr => @as(u32, lane_bits) * 2 - @as(u32, payload.imm),
            };
            const immh: u32 = shift_enc >> 3;
            const immb: u32 = shift_enc & 0b111;

            // Encoding: 0 Q U 011110 immh immb opcode 1 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b011110 << 23) |
                (immh << 19) |
                (immb << 16) |
                (opcode << 11) |
                (0b1 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector duplicate from scalar
        .vec_dup => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;
            const enc_size = enc.size;

            // DUP (general) encoding: 0 Q 0 01110000 imm5 0 0011 1 Rn Rd
            // imm5 encodes the element size: 00001=byte, 00010=half, 00100=word, 01000=dword
            const imm5: u32 = @as(u32, 1) << @as(u5, @intCast(enc_size));

            const encoding: u32 = (q << 30) |
                (0b001110000 << 21) |
                (imm5 << 16) |
                (0b000111 << 10) |
                (machregToGpr(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector duplicate from immediate - emit MOVI for now (simplified)
        .vec_dup_imm => |payload| {
            // For immediate duplication, we use MOVI instruction
            // This is a simplified implementation that handles common cases
            const enc = payload.size.encSize();
            const q = enc.q;

            // MOVI encoding for 8-bit immediate to all lanes
            // 0 Q op 0111100000 a b c cmode 0 1 d e f g h Rd
            const imm8: u32 = @truncate(payload.imm & 0xFF);
            const abc = (imm8 >> 5) & 0b111;
            const defgh = imm8 & 0b11111;

            const encoding: u32 = (q << 30) |
                (0b0111100000 << 20) |
                (abc << 16) |
                (0b1110 << 12) | // cmode for 8-bit immediate
                (0b01 << 10) |
                (defgh << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Move to vector element
        .mov_to_vec => |payload| {
            const enc_size: u32 = switch (payload.size.laneSize()) {
                .size8 => 0b00,
                .size16 => 0b01,
                .size32 => 0b10,
                .size64 => 0b11,
                else => unreachable,
            };

            // INS (general) encoding: 01001110000 imm5 0 0011 1 Rn Rd
            // imm5 encodes both size and index: size bits + index * size
            const imm5: u32 = (@as(u32, 1) << @as(u5, @intCast(enc_size))) | (@as(u32, payload.idx) << @as(u5, @intCast(enc_size + 1)));

            const encoding: u32 = (0b01001110000 << 21) |
                (imm5 << 16) |
                (0b000111 << 10) |
                (machregToGpr(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Move from vector element
        .mov_from_vec => |payload| {
            const enc_size: u32 = switch (payload.size.laneSize()) {
                .size8 => 0b00,
                .size16 => 0b01,
                .size32 => 0b10,
                .size64 => 0b11,
                else => unreachable,
            };

            // UMOV encoding: 0 Q 001110000 imm5 0 0111 1 Rn Rd
            // For 64-bit, Q=1; for 32-bit and below, Q=0
            const q: u32 = if (enc_size == 0b11) 1 else 0;
            const imm5: u32 = (@as(u32, 1) << @as(u5, @intCast(enc_size))) | (@as(u32, payload.idx) << @as(u5, @intCast(enc_size + 1)));

            const encoding: u32 = (q << 30) |
                (0b001110000 << 21) |
                (imm5 << 16) |
                (0b001111 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToGpr(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector shift immediate with modifier (SLI, SRI, etc.)
        .vec_shift_imm_mod => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;
            const lane_bits: u8 = switch (payload.size.laneSize()) {
                .size8 => 8,
                .size16 => 16,
                .size32 => 32,
                .size64 => 64,
                else => unreachable,
            };

            // SLI is shift-left insert (not a right shift)
            const is_shr = switch (payload.op) {
                .sli => false,
                .sri, .srshr, .urshr, .ssra, .usra => true,
            };

            const template: u32 = switch (payload.op) {
                .sli => 0b001_011110_0000_000_010101_00000_00000,
                .sri => 0b001_011110_0000_000_010001_00000_00000,
                .srshr => 0b000_011110_0000_000_001001_00000_00000,
                .urshr => 0b001_011110_0000_000_001001_00000_00000,
                .ssra => 0b000_011110_0000_000_000101_00000_00000,
                .usra => 0b001_011110_0000_000_000101_00000_00000,
            };

            // Calculate immh:immb encoding
            const imm: u32 = @as(u32, payload.imm);
            const immh_immb: u32 = if (is_shr)
                (@as(u32, lane_bits) << 3) | (@as(u32, lane_bits) - imm)
            else
                (@as(u32, lane_bits) << 3) | imm;

            const rn_enc = machregToVec(payload.rn);
            const rd_enc = machregToVec(payload.rd.toReg());
            try sink.put4((template | (q << 30)) | (immh_immb << 16) | (rn_enc << 5) | rd_enc);
        },

        // Vector extend operations (SXTL, UXTL, etc.)
        .vec_extend => |payload| {
            const q: u32 = if (payload.high_half) 1 else 0;

            const bits = switch (payload.op) {
                .sxtl, .sxtl2 => .{ @as(u32, 0b0), @as(u32, 0b10100) }, // SSHLL with shift=0
                .uxtl, .uxtl2 => .{ @as(u32, 0b1), @as(u32, 0b10100) }, // USHLL with shift=0
            };
            const u = bits[0];
            const opcode = bits[1];

            // Encoding: 0 Q U 011110 immh immb opcode 1 Rn Rd
            // For SXTL/UXTL, shift=0 so immh:immb encodes just the size
            const immh: u32 = 0b0001; // 8-bit lanes (will widen to 16-bit)

            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b011110 << 23) |
                (immh << 19) |
                (0b000 << 16) | // immb = 0
                (opcode << 11) |
                (0b1 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector RR long operations (FCVTL, SHLL, etc.)
        .vec_rr_long => |payload| {
            const q: u32 = if (payload.high_half) 1 else 0;

            const bits = switch (payload.op) {
                .fcvtl, .fcvtl2 => .{ @as(u32, 0b0), @as(u32, 0b10111), @as(u32, 0b00) }, // FCVTL
                .shll, .shll2 => .{ @as(u32, 0b1), @as(u32, 0b10011), @as(u32, 0b00) }, // SHLL
            };
            const u = bits[0];
            const opcode = bits[1];
            const size = bits[2];

            // Encoding: 0 Q U 01110 size 10000 opcode 10 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b01110 << 24) |
                (size << 22) |
                (0b10000 << 17) |
                (opcode << 12) |
                (0b10 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector RR narrow operations (XTN, SQXTN, etc.)
        .vec_rr_narrow => |payload| {
            const q: u32 = if (payload.high_half) 1 else 0;

            const bits = switch (payload.op) {
                .xtn => .{ @as(u32, 0b0), @as(u32, 0b10010) },
                .sqxtn => .{ @as(u32, 0b0), @as(u32, 0b10100) },
                .sqxtun => .{ @as(u32, 0b1), @as(u32, 0b10010) },
                .uqxtn => .{ @as(u32, 0b1), @as(u32, 0b10100) },
                .fcvtn => .{ @as(u32, 0b0), @as(u32, 0b10110) },
            };
            const u = bits[0];
            const opcode = bits[1];

            // Encoding: 0 Q U 01110 size 10000 opcode 10 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b01110 << 24) |
                (0b00 << 22) | // size
                (0b10000 << 17) |
                (opcode << 12) |
                (0b10 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector RRR long operations (SMULL, UMULL, etc.)
        .vec_rrr_long => |payload| {
            const q: u32 = if (payload.high_half) 1 else 0;

            const bits = switch (payload.op) {
                .smull, .smull2 => .{ @as(u32, 0b0), @as(u32, 0b1100) },
                .umull, .umull2 => .{ @as(u32, 0b1), @as(u32, 0b1100) },
                .umlsl, .umlsl2 => .{ @as(u32, 0b1), @as(u32, 0b1010) },
                .smlsl, .smlsl2 => .{ @as(u32, 0b0), @as(u32, 0b1010) },
            };
            const u = bits[0];
            const opcode = bits[1];

            // Encoding: 0 Q U 01110 size 1 Rm opcode 00 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b01110 << 24) |
                (0b00 << 22) | // size
                (0b1 << 21) |
                (machregToVec(payload.rm) << 16) |
                (opcode << 12) |
                (0b00 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector RRR long operations with modifier (UMLAL, SMLAL, etc.)
        .vec_rrr_long_mod => |payload| {
            const q: u32 = if (payload.high_half) 1 else 0;

            const bits = switch (payload.op) {
                .umlal, .umlal2 => .{ @as(u32, 0b1), @as(u32, 0b1000) },
                .smlal, .smlal2 => .{ @as(u32, 0b0), @as(u32, 0b1000) },
            };
            const u = bits[0];
            const opcode = bits[1];

            // Encoding: 0 Q U 01110 size 1 Rm opcode 00 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b01110 << 24) |
                (0b00 << 22) | // size
                (0b1 << 21) |
                (machregToVec(payload.rm) << 16) |
                (opcode << 12) |
                (0b00 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector RR pair long operations (SADDLP, UADDLP)
        .vec_rr_pair_long => |payload| {
            const enc = payload.size.encSize();
            const q = enc.q;
            const enc_size = enc.size;

            const u: u32 = switch (payload.op) {
                .saddlp => 0b0,
                .uaddlp => 0b1,
            };

            // Encoding: 0 Q U 01110 size 10000 00010 10 Rn Rd
            const encoding: u32 = (q << 30) |
                (u << 29) |
                (0b01110 << 24) |
                (enc_size << 22) |
                (0b10000 << 17) |
                (0b00010 << 12) |
                (0b10 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector extract (EXT instruction)
        .vec_extract => |payload| {
            std.debug.assert(payload.imm4 < 16);

            // EXT encoding: 0 1 101110 00 0 Rm 0 imm4 0 Rn Rd
            const encoding: u32 = (0b01101110000 << 21) |
                (machregToVec(payload.rm) << 16) |
                (@as(u32, payload.imm4) << 11) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector table lookup (TBL)
        .vec_tbl => |payload| {
            // TBL encoding: 0 Q 001110 00 0 Rm 0 len 0 0 Rn Rd
            // len=00 for single register table
            const encoding: u32 = (0b01001110000 << 21) |
                (machregToVec(payload.rm) << 16) |
                (0b0 << 15) | // is_extension = false
                (0b00 << 13) | // len = 00
                (0b00 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
        },

        // Vector table lookup with extension (TBX)
        .vec_tbl_ext => |payload| {
            // TBX encoding: 0 Q 001110 00 0 Rm 0 len 1 0 Rn Rd
            // len=00 for single register table
            const encoding: u32 = (0b01001110000 << 21) |
                (machregToVec(payload.rm) << 16) |
                (0b1 << 15) | // is_extension = true
                (0b00 << 13) | // len = 00
                (0b00 << 10) |
                (machregToVec(payload.rn) << 5) |
                machregToVec(payload.rd.toReg());
            try sink.put4(encoding);
            _ = payload.ri; // Extension takes existing value
        },

        // ==========================================================================
        // External Symbol Loading
        // ==========================================================================

        // Load external symbol address via GOT (for PIC code)
        // Emits: adrp rd, :got:X; ldr rd, [rd, :got_lo12:X]
        .load_ext_name_got => |payload| {
            // ADRP rd, :got:X (page address of GOT entry)
            try sink.addRelocExternalName(.Arm64AdrGotPage21, .{ .User = buffer.UserExternalNameRef.init(payload.symbol_idx) }, 0);
            const adrp_enc: u32 = (0b1 << 31) | (0b10000 << 24) | machregToGpr(payload.rd.toReg());
            try sink.put4(adrp_enc);

            // LDR rd, [rd, :got_lo12:X] (load from GOT entry)
            try sink.addRelocExternalName(.Arm64Ld64GotLo12Nc, .{ .User = buffer.UserExternalNameRef.init(payload.symbol_idx) }, 0);
            // LDR (unsigned offset) encoding: 11 111 0 01 01 imm12 Rn Rt
            const ldr_enc: u32 = (0b11_111_0_01_01 << 22) |
                (0 << 10) | // imm12=0, will be patched by relocation
                (machregToGpr(payload.rd.toReg()) << 5) |
                machregToGpr(payload.rd.toReg());
            try sink.put4(ldr_enc);
        },

        // Load external symbol address (near, ADRP + ADD sequence)
        // Emits: adrp rd, X; add rd, rd, :lo12:X
        .load_ext_name_near => |payload| {
            // ADRP rd, symbol (page address)
            try sink.addRelocExternalName(.Aarch64AdrPrelPgHi21, .{ .User = buffer.UserExternalNameRef.init(payload.symbol_idx) }, payload.offset);
            const adrp_enc: u32 = (0b1 << 31) | (0b10000 << 24) | machregToGpr(payload.rd.toReg());
            try sink.put4(adrp_enc);

            // ADD rd, rd, :lo12:X (page offset)
            try sink.addRelocExternalName(.Aarch64AddAbsLo12Nc, .{ .User = buffer.UserExternalNameRef.init(payload.symbol_idx) }, payload.offset);
            const add_enc: u32 = (0b1_0_0_10001_0 << 22) | // sf=1, op=0, S=0, shift=0
                (0 << 10) | // imm12=0, will be patched by relocation
                (machregToGpr(payload.rd.toReg()) << 5) |
                machregToGpr(payload.rd.toReg());
            try sink.put4(add_enc);
        },

        // Load external symbol address (far, literal pool)
        // Emits: ldr rd, #8; b #12; <8-byte address>
        .load_ext_name_far => |payload| {
            // LDR rd, #8 (load from 8 bytes ahead, the literal pool)
            // LDR (literal) encoding: 01 011 0 00 imm19 Rt
            const ldr_enc: u32 = (0b01_011_0_00 << 24) |
                (2 << 5) | // imm19=2 (8 bytes / 4 = 2 instructions ahead)
                machregToGpr(payload.rd.toReg());
            try sink.put4(ldr_enc);

            // B #12 (branch over the 8-byte literal)
            // B encoding: 000101 imm26
            const b_enc: u32 = (0b000101 << 26) | 3; // imm26=3 (12 bytes / 4 = 3 instructions)
            try sink.put4(b_enc);

            // 8-byte literal (absolute address, patched by linker)
            try sink.addRelocExternalName(.Abs8, .{ .User = buffer.UserExternalNameRef.init(payload.symbol_idx) }, payload.offset);
            try sink.put8(0);
        },

        // ==========================================================================
        // Jump Table Sequence
        // ==========================================================================

        // Jump table pseudo-instruction - expands to multiple real instructions
        .jt_sequence => |payload| {
            // This sequence must be emitted as a single unit because we use PC-relative addressing.
            // Sequence:
            // 1. B.HS default       - Branch to default if out of bounds (assumes prior CMP)
            // 2. CSEL rtmp2, XZR, ridx, HS - Spectre mitigation: zero if out of bounds
            // 3. CSDB               - Speculation barrier
            // 4. ADR rtmp1, #16     - Load address of jump table (16 bytes ahead)
            // 5. LDR rtmp2, [rtmp1, rtmp2, UXTW #2] - Load offset from table
            // 6. ADD rtmp1, rtmp1, rtmp2 - Compute target address
            // 7. BR rtmp1           - Branch to target
            // 8. [jump table data]  - 32-bit offsets for each target

            // 1. B.HS default - conditional branch to default target
            const br_default = encConditionalBr(BranchTarget{ .label = payload.default }, CondBrKind{ .cond = Cond.hs });
            const br_offset = sink.curOffset();
            try sink.useLabelAtOffset(br_offset, payload.default, .branch19);
            try sink.put4(br_default);

            // 2. CSEL rtmp2, XZR, ridx, HS - select zero if out of bounds (Spectre mitigation)
            // Encoding: 1 0011010100 Rm cond 00 Rn Rd
            const csel_enc: u32 = (0b1_0011010100 << 21) |
                (machregToGpr(payload.ridx) << 16) |
                (@as(u32, Cond.hs.bits()) << 12) |
                (0b00 << 10) |
                (machregToGpr(zeroReg()) << 5) |
                machregToGpr(payload.rtmp2.toReg());
            try sink.put4(csel_enc);

            // 3. CSDB - speculation barrier
            try sink.put4(0xd503229f);

            // 4. ADR rtmp1, #16 - load address of jump table
            // The jump table starts 16 bytes after this instruction (4 instructions * 4 bytes)
            const adr_off: i32 = 16;
            const immlo: u32 = @as(u32, @intCast(adr_off & 3));
            const immhi: u32 = @as(u32, @intCast((adr_off >> 2) & ((1 << 19) - 1)));
            const adr_enc: u32 = (0b00010000 << 24) | (immlo << 29) | (immhi << 5) | machregToGpr(payload.rtmp1.toReg());
            try sink.put4(adr_enc);

            // 5. LDR rtmp2, [rtmp1, rtmp2, UXTW #2] - load 32-bit offset from jump table
            // Encoding for LDRSW with register+register addressing, scaled
            // 10 111000 10 1 Rm option S 10 Rn Rt
            // option=011 (UXTW), S=1 (scaled by 4)
            const ldr_enc: u32 = (0b10111000101 << 21) |
                (machregToGpr(payload.rtmp2.toReg()) << 16) |
                (0b011 << 13) | // UXTW
                (1 << 12) | // S=1 (scaled)
                (0b10 << 10) |
                (machregToGpr(payload.rtmp1.toReg()) << 5) |
                machregToGpr(payload.rtmp2.toReg());
            try sink.put4(ldr_enc);

            // 6. ADD rtmp1, rtmp1, rtmp2 - add base to offset
            const add_enc: u32 = (0b10001011000 << 21) |
                (machregToGpr(payload.rtmp2.toReg()) << 16) |
                (0b000000 << 10) |
                (machregToGpr(payload.rtmp1.toReg()) << 5) |
                machregToGpr(payload.rtmp1.toReg());
            try sink.put4(add_enc);

            // 7. BR rtmp1 - indirect branch
            const br_enc: u32 = 0b1101011_0000_11111_000000_00000_00000 | (machregToGpr(payload.rtmp1.toReg()) << 5);
            try sink.put4(br_enc);

            // 8. Jump table data - emit 32-bit offsets for each target
            const jt_start = sink.curOffset();
            for (payload.targets) |target| {
                const word_off = sink.curOffset();
                // The offset is relative to the jump table start
                const off_into_table = word_off - jt_start;
                try sink.useLabelAtOffset(word_off, target, .pcRel32);
                try sink.put4(off_into_table);
            }
        },
    }
}

//=============================================================================
// Tests
//=============================================================================

test "encode ALU RRR add" {
    const testing = std.testing;
    const x0 = xreg(0);
    const x1 = xreg(1);
    const x2 = xreg(2);
    const rd = Writable(Reg).fromReg(x0);

    // ADD X0, X1, X2 (64-bit)
    const encoded = encArithRrr(0b10001011_000, 0b000000, rd, x1, x2);
    // Expected: sf=1, op=0, S=0, shift=00, Rm=x2, imm6=000000, Rn=x1, Rd=x0
    try testing.expect(encoded & 0x1F == 0); // Rd = 0
    try testing.expect((encoded >> 5) & 0x1F == 1); // Rn = 1
    try testing.expect((encoded >> 16) & 0x1F == 2); // Rm = 2
}

test "encode move wide" {
    const testing = std.testing;
    const x0 = xreg(0);
    const rd = Writable(Reg).fromReg(x0);

    // MOVZ X0, #0x1234
    const imm = MoveWideConst.maybeFromU64(0x1234).?;
    const encoded = encMoveWide(.movz, rd, imm, .size64);
    try testing.expect((encoded >> 31) & 1 == 1); // sf = 1
    try testing.expect((encoded >> 29) & 0b11 == 0b10); // opc = 10 (MOVZ)
}

test "encode jump 26" {
    const testing = std.testing;

    // B #0 (unconditional branch)
    const encoded = encJump26(0b000101, 0);
    try testing.expect((encoded >> 26) == 0b000101);
    try testing.expect((encoded & ((1 << 26) - 1)) == 0);
}

test "MachBuffer basic operations" {
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    try testing.expectEqual(@as(u32, 0), buf.curOffset());

    try buf.put4(0x12345678);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    try buf.put4(0xABCDEF00);
    try testing.expectEqual(@as(u32, 8), buf.curOffset());

    try testing.expectEqual(@as(usize, 8), buf.data.items.len);
}

// =============================================================================
// Task 4.11: Test simple programs using emit() with real Inst structures
// =============================================================================

test "emit return 42" {
    // Test: MOVZ X0, #42 followed by RET
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    // MOVZ X0, #42
    const x0 = xreg(0);
    const imm42 = MoveWideConst.maybeFromU64(42).?;
    const movz_inst = Inst{ .mov_wide = .{
        .op = .movz,
        .rd = Writable(Reg).fromReg(x0),
        .imm = imm42,
        .size = .size64,
    } };
    try emit(&movz_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    // RET
    const ret_inst = Inst{ .ret = {} };
    try emit(&ret_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 8), buf.curOffset());

    // Verify RET encoding: D65F03C0
    const ret_bytes = buf.data.items[4..8];
    try testing.expectEqual(@as(u8, 0xC0), ret_bytes[0]);
    try testing.expectEqual(@as(u8, 0x03), ret_bytes[1]);
    try testing.expectEqual(@as(u8, 0x5F), ret_bytes[2]);
    try testing.expectEqual(@as(u8, 0xD6), ret_bytes[3]);
}

test "emit add two registers" {
    // Test: ADD X0, X0, X1
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0);
    const x1 = xreg(1);
    const add_inst = Inst{ .alu_rrr = .{
        .alu_op = .add,
        .size = .size64,
        .rd = Writable(Reg).fromReg(x0),
        .rn = x0,
        .rm = x1,
    } };
    try emit(&add_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());
}

test "emit sub immediate" {
    // Test: SUB X0, X0, #10
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0);
    const imm10 = Imm12.maybeFromU64(10).?;
    const sub_inst = Inst{ .alu_rr_imm12 = .{
        .alu_op = .sub,
        .size = .size64,
        .rd = Writable(Reg).fromReg(x0),
        .rn = x0,
        .imm12 = imm10,
    } };
    try emit(&sub_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());
}

// Note: alu_rrrr (MADD/MSUB) not yet implemented in emit - see Task 4.15+

// =============================================================================
// Task 4.12: Test control flow instructions
// =============================================================================

test "emit unconditional jump" {
    // Test: B label
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    // Allocate label in the buffer before using it
    const label = try buf.getLabel();
    const jump_inst = Inst{ .jump = .{
        .dest = BranchTarget{ .label = label },
    } };
    try emit(&jump_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());
}

test "emit conditional branch" {
    // Test: B.EQ label (branch if equal)
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    // Allocate labels in the buffer before using them
    const label = try buf.getLabel();
    const fallthrough = try buf.getLabel();
    const cond_br_inst = Inst{ .cond_br = .{
        .kind = CondBrKind{ .cond = Cond.eq },
        .taken = BranchTarget{ .label = label },
        .not_taken = BranchTarget{ .label = fallthrough },
    } };
    try emit(&cond_br_inst, &buf, &emit_info, &emit_state);
    // cond_br emits both the conditional branch and unconditional fallthrough (8 bytes total)
    try testing.expectEqual(@as(u32, 8), buf.curOffset());
}

test "emit compare and branch" {
    // Test: SUBS XZR, X0, X1 (CMP X0, X1) followed by B.LT
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    // CMP X0, X1 is actually SUBS XZR, X0, X1
    const x0 = xreg(0);
    const x1 = xreg(1);
    const xzr = zeroReg();
    const cmp_inst = Inst{ .alu_rrr = .{
        .alu_op = .subs,
        .size = .size64,
        .rd = Writable(Reg).fromReg(xzr),
        .rn = x0,
        .rm = x1,
    } };
    try emit(&cmp_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    // B.LT label - allocate labels in the buffer first
    const label = try buf.getLabel();
    const fallthrough = try buf.getLabel();
    const blt_inst = Inst{ .cond_br = .{
        .kind = CondBrKind{ .cond = Cond.lt },
        .taken = BranchTarget{ .label = label },
        .not_taken = BranchTarget{ .label = fallthrough },
    } };
    try emit(&blt_inst, &buf, &emit_info, &emit_state);
    // cond_br emits both the conditional branch and unconditional fallthrough (8 bytes total)
    // So total is 4 (CMP) + 8 (B.cond + B) = 12 bytes
    try testing.expectEqual(@as(u32, 12), buf.curOffset());
}

// =============================================================================
// Task 4.13: Test function call instructions
// =============================================================================

test "emit function call" {
    // Test: BL label (branch with link)
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    // Allocate label in the buffer before using it
    const target = try buf.getLabel();
    const call_inst = Inst{ .call = .{
        .dest = BranchTarget{ .label = target },
    } };
    try emit(&call_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());
}

test "emit indirect call" {
    // Test: BLR X8 (branch with link to register)
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x8 = xreg(8);
    const call_ind_inst = Inst{ .call_ind = .{
        .rn = x8,
    } };
    try emit(&call_ind_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());
}

test "emit stack frame setup" {
    // Test: STP X29, X30, [SP, #-16]! (prologue) and LDP X29, X30, [SP], #16 (epilogue)
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x29 = xreg(29);
    const x30 = xreg(30);

    // STP X29, X30, [SP, #-16]! (save frame pointer and link register)
    const offset_neg = SImm7Scaled.maybeFromI64(-16, Type.I64).?;
    const stp_inst = Inst{ .store_p64 = .{
        .rt = x29,
        .rt2 = x30,
        .mem = PairAMode{ .sp_pre_indexed = .{ .simm7 = offset_neg } },
        .flags = MemFlags{},
    } };
    try emit(&stp_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    // LDP X29, X30, [SP], #16 (restore frame pointer and link register)
    const offset_pos = SImm7Scaled.maybeFromI64(16, Type.I64).?;
    const ldp_inst = Inst{ .load_p64 = .{
        .rt = Writable(Reg).fromReg(x29),
        .rt2 = Writable(Reg).fromReg(x30),
        .mem = PairAMode{ .sp_post_indexed = .{ .simm7 = offset_pos } },
        .flags = MemFlags{},
    } };
    try emit(&ldp_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 8), buf.curOffset());
}

// =============================================================================
// Task 4.15: Test atomic operations
// =============================================================================

test "emit atomic rmw (LSE)" {
    // Test: LDADDAL X1, X0, [X2] (atomic add)
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0);
    const x1 = xreg(1);
    const x2 = xreg(2);
    const atomic_add = Inst{ .atomic_rmw = .{
        .op = .add,
        .rs = x1,
        .rt = Writable(Reg).fromReg(x0),
        .rn = x2,
        .ty = Type.I64,
    } };
    try emit(&atomic_add, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());
}

test "emit atomic cas (LSE)" {
    // Test: CASAL X1, X2, [X0] (compare and swap)
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0);
    const x1 = xreg(1);
    const x2 = xreg(2);
    const atomic_cas = Inst{ .atomic_cas = .{
        .rd = Writable(Reg).fromReg(x1),
        .rs = x1,
        .rt = x2,
        .rn = x0,
        .ty = Type.I64,
    } };
    try emit(&atomic_cas, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());
}

test "emit ldaxr and stlxr" {
    // Test exclusive load/store for atomic loop
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0);
    const x1 = xreg(1);
    const x2 = xreg(2);

    // LDAXR X0, [X2]
    const ldaxr_inst = Inst{ .ldaxr = .{
        .rt = Writable(Reg).fromReg(x0),
        .rn = x2,
        .ty = Type.I64,
    } };
    try emit(&ldaxr_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    // STLXR W1, X0, [X2]
    const stlxr_inst = Inst{ .stlxr = .{
        .rs = Writable(Reg).fromReg(x1),
        .rt = x0,
        .rn = x2,
        .ty = Type.I64,
    } };
    try emit(&stlxr_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 8), buf.curOffset());
}

test "emit ldar and stlr" {
    // Test acquire/release load/store
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0);
    const x2 = xreg(2);

    // LDAR X0, [X2]
    const ldar_inst = Inst{ .ldar = .{
        .rt = Writable(Reg).fromReg(x0),
        .rn = x2,
        .ty = Type.I64,
    } };
    try emit(&ldar_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    // STLR X0, [X2]
    const stlr_inst = Inst{ .stlr = .{
        .rt = x0,
        .rn = x2,
        .ty = Type.I64,
    } };
    try emit(&stlr_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 8), buf.curOffset());
}

// =============================================================================
// Task 4.16: Test vector/SIMD operations
// =============================================================================

test "emit vec_rrr add" {
    // Test: ADD V0.16B, V1.16B, V2.16B
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const v0 = vreg(0);
    const v1 = vreg(1);
    const v2 = vreg(2);
    const vec_add_inst = Inst{ .vec_rrr = .{
        .op = .add,
        .rd = Writable(Reg).fromReg(v0),
        .rn = v1,
        .rm = v2,
        .size = .size8x16,
    } };
    try emit(&vec_add_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    // Verify we got a valid encoding (not BRK)
    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

test "emit vec_rrr fadd" {
    // Test: FADD V0.4S, V1.4S, V2.4S
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const v0 = vreg(0);
    const v1 = vreg(1);
    const v2 = vreg(2);
    const vec_fadd_inst = Inst{ .vec_rrr = .{
        .op = .fadd,
        .rd = Writable(Reg).fromReg(v0),
        .rn = v1,
        .rm = v2,
        .size = .size32x4,
    } };
    try emit(&vec_fadd_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

test "emit vec_misc neg" {
    // Test: NEG V0.4S, V1.4S
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const v0 = vreg(0);
    const v1 = vreg(1);
    const vec_neg_inst = Inst{ .vec_misc = .{
        .op = .neg,
        .rd = Writable(Reg).fromReg(v0),
        .rn = v1,
        .size = .size32x4,
    } };
    try emit(&vec_neg_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

test "emit vec_lanes addv" {
    // Test: ADDV S0, V1.4S
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const v0 = vreg(0);
    const v1 = vreg(1);
    const vec_addv_inst = Inst{ .vec_lanes = .{
        .op = .addv,
        .rd = Writable(Reg).fromReg(v0),
        .rn = v1,
        .size = .size32x4,
    } };
    try emit(&vec_addv_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

test "emit vec_shift_imm shl" {
    // Test: SHL V0.4S, V1.4S, #3
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const v0 = vreg(0);
    const v1 = vreg(1);
    const vec_shl_inst = Inst{ .vec_shift_imm = .{
        .op = .shl,
        .rd = Writable(Reg).fromReg(v0),
        .rn = v1,
        .size = .size32x4,
        .imm = 3,
    } };
    try emit(&vec_shl_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

test "emit vec_dup from GPR" {
    // Test: DUP V0.4S, W1
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const v0 = vreg(0);
    const x1 = xreg(1);
    const vec_dup_inst = Inst{ .vec_dup = .{
        .rd = Writable(Reg).fromReg(v0),
        .rn = x1,
        .size = .size32x4,
    } };
    try emit(&vec_dup_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

test "emit mov_to_vec" {
    // Test: INS V0.S[0], W1
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const v0 = vreg(0);
    const x1 = xreg(1);
    const mov_inst = Inst{ .mov_to_vec = .{
        .rd = Writable(Reg).fromReg(v0),
        .ri = v0, // Source vector
        .rn = x1, // GPR to insert
        .idx = 0,
        .size = .size32x4,
    } };
    try emit(&mov_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

test "emit mov_from_vec" {
    // Test: UMOV W0, V1.S[0]
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0);
    const v1 = vreg(1);
    const mov_inst = Inst{ .mov_from_vec = .{
        .rd = Writable(Reg).fromReg(x0),
        .rn = v1,
        .idx = 0,
        .size = .size32x4,
    } };
    try emit(&mov_inst, &buf, &emit_info, &emit_state);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    const encoded = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(encoded != 0xd43e0000); // Not BRK
}

// =============================================================================
// Task 4.17: Test jump table operations
// =============================================================================

test "emit jt_sequence" {
    // Test: Jump table with 3 targets
    const testing = std.testing;
    var buf = MachBuffer.init(testing.allocator);
    defer buf.deinit();

    var emit_state = EmitState{};
    const emit_info = EmitInfo.init(.{});

    const x0 = xreg(0); // Index register
    const x1 = xreg(1); // Temp 1
    const x2 = xreg(2); // Temp 2

    // Create labels for targets
    const default_label = try buf.getLabel();
    const target0 = try buf.getLabel();
    const target1 = try buf.getLabel();
    const target2 = try buf.getLabel();

    const targets = [_]MachLabel{ target0, target1, target2 };

    const jt_inst = Inst{ .jt_sequence = .{
        .ridx = x0,
        .rtmp1 = Writable(Reg).fromReg(x1),
        .rtmp2 = Writable(Reg).fromReg(x2),
        .default = default_label,
        .targets = &targets,
    } };
    try emit(&jt_inst, &buf, &emit_info, &emit_state);

    // The sequence should emit:
    // - B.HS (4 bytes)
    // - CSEL (4 bytes)
    // - CSDB (4 bytes)
    // - ADR (4 bytes)
    // - LDR (4 bytes)
    // - ADD (4 bytes)
    // - BR (4 bytes)
    // - 3 x 4-byte offsets (12 bytes)
    // Total: 7 * 4 + 3 * 4 = 40 bytes
    try testing.expectEqual(@as(u32, 40), buf.curOffset());

    // Verify the first instruction is not BRK (it should be B.HS)
    const first_inst = std.mem.bytesToValue(u32, buf.data.items[0..4]);
    try testing.expect(first_inst != 0xd43e0000); // Not BRK

    // Verify CSDB is at offset 8 (after B.HS and CSEL)
    const csdb = std.mem.bytesToValue(u32, buf.data.items[8..12]);
    try testing.expectEqual(@as(u32, 0xd503229f), csdb);
}
