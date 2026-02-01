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
pub const MoveWideOp = mod.MoveWideOp;
pub const AMode = mod.AMode;
pub const PairAMode = mod.PairAMode;

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

pub const Imm12 = imms.Imm12;
pub const ImmLogic = imms.ImmLogic;
pub const ImmShift = imms.ImmShift;
pub const MoveWideConst = imms.MoveWideConst;
pub const SImm9 = imms.SImm9;
pub const SImm7Scaled = imms.SImm7Scaled;
pub const UImm12Scaled = imms.UImm12Scaled;

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
// Machine buffer for instruction emission
//=============================================================================

/// A buffer that collects emitted machine code.
pub const MachBuffer = struct {
    data: std.ArrayListUnmanaged(u8) = .{},
    labels: std.ArrayListUnmanaged(LabelInfo) = .{},
    pending_relocs: std.ArrayListUnmanaged(Relocation) = .{},
    cur_offset_val: u32 = 0,
    allocator: Allocator,

    pub const LabelInfo = struct {
        offset: ?u32 = null,
    };

    pub const Relocation = struct {
        offset: u32,
        label: MachLabel,
        kind: LabelUse,
    };

    pub const LabelUse = enum {
        branch19,
        branch26,
        branch14,
        pcRel32,
        ldr19,
    };

    pub fn init(allocator: Allocator) MachBuffer {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *MachBuffer) void {
        self.data.deinit(self.allocator);
        self.labels.deinit(self.allocator);
        self.pending_relocs.deinit(self.allocator);
    }

    pub fn curOffset(self: *const MachBuffer) u32 {
        return self.cur_offset_val;
    }

    pub fn put4(self: *MachBuffer, val: u32) void {
        const bytes = std.mem.asBytes(&val);
        self.data.appendSlice(self.allocator, bytes) catch unreachable;
        self.cur_offset_val += 4;
    }

    pub fn put8(self: *MachBuffer, val: u64) void {
        const bytes = std.mem.asBytes(&val);
        self.data.appendSlice(self.allocator, bytes) catch unreachable;
        self.cur_offset_val += 8;
    }

    pub fn putData(self: *MachBuffer, data: []const u8) void {
        self.data.appendSlice(self.allocator, data) catch unreachable;
        self.cur_offset_val += @intCast(data.len);
    }

    pub fn getLabel(self: *MachBuffer) MachLabel {
        const idx = self.labels.items.len;
        self.labels.append(self.allocator, .{ .offset = null }) catch unreachable;
        return @intCast(idx);
    }

    pub fn bindLabel(self: *MachBuffer, label: MachLabel) void {
        self.labels.items[label].offset = self.cur_offset_val;
    }

    pub fn useLabelAtOffset(self: *MachBuffer, offset: u32, label: MachLabel, kind: LabelUse) void {
        self.pending_relocs.append(self.allocator, .{
            .offset = offset,
            .label = label,
            .kind = kind,
        }) catch unreachable;
    }
};

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
        .notZero => |payload| encOpSize(
            encCmpbr(0b0_011010_1, taken.asOffset19OrZero(), payload.reg),
            payload.size,
        ),
        .cond => |c| encCbr(0b01010100, taken.asOffset19OrZero(), 0b0, c.bits()),
    };
}

/// Encode a move-wide instruction.
pub fn encMoveWide(op: MoveWideOp, rd: Writable(Reg), imm_const: MoveWideConst, size: OperandSize) u32 {
    std.debug.assert(imm_const.shift <= 0b11);
    const op_bits: u32 = switch (op) {
        .movn => 0b00,
        .movz => 0b10,
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

//=============================================================================
// Main emission function
//=============================================================================

/// Emit an instruction to the buffer.
pub fn emit(inst: *const Inst, sink: *MachBuffer, emit_info: *const EmitInfo, state: *EmitState) void {
    _ = emit_info;

    switch (inst.*) {
        .aluRRR => |payload| {
            const top11: u32 = switch (payload.alu_op) {
                .add => 0b00001011_000,
                .adc => 0b00011010_000,
                .adcS => 0b00111010_000,
                .sub => 0b01001011_000,
                .sbc => 0b01011010_000,
                .sbcS => 0b01111010_000,
                .orr => 0b00101010_000,
                .@"and" => 0b00001010_000,
                .andS => 0b01101010_000,
                .eor => 0b01001010_000,
                .orrNot => 0b00101010_001,
                .andNot => 0b00001010_001,
                .eorNot => 0b01001010_001,
                .addS => 0b00101011_000,
                .subS => 0b01101011_000,
                .sdiv, .udiv => 0b00011010_110,
                .extr, .lsr, .asr, .lsl => 0b00011010_110,
                .smulH => 0b10011011_010,
                .umulH => 0b10011011_110,
            };

            const top11_sized = top11 | (@as(u32, payload.size.sfBit()) << 10);
            const bit15_10: u32 = switch (payload.alu_op) {
                .sdiv => 0b000011,
                .udiv => 0b000010,
                .extr => 0b001011,
                .lsr => 0b001001,
                .asr => 0b001010,
                .lsl => 0b001000,
                .smulH, .umulH => 0b011111,
                else => 0b000000,
            };

            sink.put4(encArithRrr(top11_sized, bit15_10, payload.rd, payload.rn, payload.rm));
        },

        .aluRRRR => |payload| {
            const result = switch (payload.alu_op) {
                .madd => .{ @as(u32, 0b0_00_11011_000), @as(u32, 0) },
                .msub => .{ @as(u32, 0b0_00_11011_000), @as(u32, 1) },
                .umaddl => .{ @as(u32, 0b1_00_11011_1_01), @as(u32, 0) },
                .smaddl => .{ @as(u32, 0b1_00_11011_0_01), @as(u32, 0) },
            };
            const top11 = result[0];
            const bit15 = result[1];
            const top11_sized = top11 | (@as(u32, payload.size.sfBit()) << 10);
            sink.put4(encArithRrrr(top11_sized, payload.rm, bit15, payload.ra, payload.rn, payload.rd));
        },

        .aluRRImm12 => |payload| {
            const top8: u32 = switch (payload.alu_op) {
                .add => 0b000_10001,
                .sub => 0b010_10001,
                .addS => 0b001_10001,
                .subS => 0b011_10001,
                else => @panic("Unsupported ALU op for imm12"),
            };
            const top8_sized = top8 | (@as(u32, payload.size.sfBit()) << 7);
            sink.put4(encArithRrImm12(
                top8_sized,
                payload.imm12.shiftBits(),
                payload.imm12.immBits(),
                payload.rn,
                payload.rd,
            ));
        },

        .aluRRImmLogic => |payload| {
            const result = switch (payload.alu_op) {
                .orr => .{ @as(u32, 0b001_100100), false },
                .@"and" => .{ @as(u32, 0b000_100100), false },
                .andS => .{ @as(u32, 0b011_100100), false },
                .eor => .{ @as(u32, 0b010_100100), false },
                .orrNot => .{ @as(u32, 0b001_100100), true },
                .andNot => .{ @as(u32, 0b000_100100), true },
                .eorNot => .{ @as(u32, 0b010_100100), true },
                else => @panic("Unsupported ALU op for immLogic"),
            };
            const top9 = result[0];
            const inv = result[1];
            const top9_sized = top9 | (@as(u32, payload.size.sfBit()) << 8);
            const imml = if (inv) payload.imml.invert() else payload.imml;
            sink.put4(encArithRrImml(top9_sized, imml.encBits(), payload.rn, payload.rd));
        },

        .bitRR => |payload| {
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
            sink.put4(encBitRr(payload.size.sfBit(), op1, op2, payload.rn, payload.rd));
        },

        .movWide => |payload| {
            sink.put4(encMoveWide(payload.op, payload.rd, payload.imm, payload.size));
        },

        .movK => |payload| {
            sink.put4(encMovk(payload.rd, payload.imm, payload.size));
        },

        .mov => |payload| {
            std.debug.assert(payload.rd.toReg().class() == payload.rm.class());
            std.debug.assert(payload.rm.class() == .int);

            switch (payload.size) {
                .size64 => {
                    std.debug.assert(payload.rd.toReg().hw_enc != stackReg().hw_enc);
                    if (payload.rm.hw_enc == stackReg().hw_enc) {
                        // Use add rd, sp, #0 instead of ORR
                        const imm12 = Imm12.maybeFromU64(0).?;
                        sink.put4(encArithRrImm12(
                            0b100_10001,
                            imm12.shiftBits(),
                            imm12.immBits(),
                            payload.rm,
                            payload.rd,
                        ));
                    } else {
                        // Encoded as ORR rd, rm, zero.
                        sink.put4(encArithRrr(0b10101010_000, 0b000_000, payload.rd, zeroReg(), payload.rm));
                    }
                },
                .size32 => {
                    // Encoded as ORR rd, rm, zero.
                    sink.put4(encArithRrr(0b00101010_000, 0b000_000, payload.rd, zeroReg(), payload.rm));
                },
            }
        },

        .csel => |payload| {
            sink.put4(encCsel(payload.rd, payload.rn, payload.rm, payload.cond, 0, 0));
        },

        .csneg => |payload| {
            sink.put4(encCsel(payload.rd, payload.rn, payload.rm, payload.cond, 1, 1));
        },

        .cset => |payload| {
            sink.put4(encCsel(payload.rd, zeroReg(), zeroReg(), payload.cond.invert(), 0, 1));
        },

        .csetm => |payload| {
            sink.put4(encCsel(payload.rd, zeroReg(), zeroReg(), payload.cond.invert(), 1, 0));
        },

        .jump => |payload| {
            const off = sink.curOffset();
            if (payload.dest.asLabel()) |l| {
                sink.useLabelAtOffset(off, l, .branch26);
            }
            sink.put4(encJump26(0b000101, payload.dest.asOffset26OrZero()));
        },

        .condBr => |payload| {
            // Conditional part first
            const cond_off = sink.curOffset();
            if (payload.taken.asLabel()) |l| {
                sink.useLabelAtOffset(cond_off, l, .branch19);
            }
            sink.put4(encConditionalBr(payload.taken, payload.kind));

            // Unconditional part next
            const uncond_off = sink.curOffset();
            if (payload.not_taken.asLabel()) |l| {
                sink.useLabelAtOffset(uncond_off, l, .branch26);
            }
            sink.put4(encJump26(0b000101, payload.not_taken.asOffset26OrZero()));
        },

        .ret => {
            sink.put4(0xd65f03c0);
        },

        .indirectBr => |payload| {
            sink.put4(encBr(payload.rn));
        },

        .nop0 => {
            // Emit nothing
        },

        .nop4 => {
            sink.put4(0xd503201f);
        },

        .brk => {
            sink.put4(0xd43e0000);
        },

        .udf => {
            sink.put4(0x00000000); // UDF encoding
        },

        .adr => |payload| {
            std.debug.assert(payload.off > -(1 << 20));
            std.debug.assert(payload.off < (1 << 20));
            sink.put4(encAdr(payload.off, payload.rd));
        },

        .adrp => |payload| {
            std.debug.assert(payload.off > -(1 << 20));
            std.debug.assert(payload.off < (1 << 20));
            sink.put4(encAdrp(payload.off, payload.rd));
        },

        .word4 => |payload| {
            sink.put4(payload.data);
        },

        .word8 => |payload| {
            sink.put8(payload.data);
        },

        .fence => {
            sink.put4(encDmbIsh());
        },

        .csdb => {
            sink.put4(0xd503229f);
        },

        .fpuMove32 => |payload| {
            sink.put4(encFpurr(0b000_11110_00_1_000000_10000, payload.rd, payload.rn));
        },

        .fpuMove64 => |payload| {
            sink.put4(encFpurr(0b000_11110_01_1_000000_10000, payload.rd, payload.rn));
        },

        .fpuMove128 => |payload| {
            sink.put4(encVecmov(true, payload.rd, payload.rn));
        },

        .fpuCmp => |payload| {
            sink.put4(encFcmp(payload.size, payload.rn, payload.rm));
        },

        .movToNzcv => |payload| {
            sink.put4(0xd51b4200 | machregToGpr(payload.rn));
        },

        .movFromNzcv => |payload| {
            sink.put4(0xd53b4200 | machregToGpr(payload.rd.toReg()));
        },

        // Load instructions
        .uload8, .sload8, .uload16, .sload16, .uload32, .sload32, .uload64 => {
            const payload = switch (inst.*) {
                .uload8 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b0011100001) },
                .sload8 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b0011100010) },
                .uload16 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b0111100001) },
                .sload16 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b0111100010) },
                .uload32 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b1011100001) },
                .sload32 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b1011100010) },
                .uload64 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b1111100001) },
                else => unreachable,
            };

            const rd = payload.rd.toReg();
            const op = payload.op;

            switch (payload.mem) {
                .unscaled => |m| {
                    sink.put4(encLdstSimm9(op, m.simm9, 0b00, m.rn, rd));
                },
                .unsigned_offset => |m| {
                    sink.put4(encLdstUimm12(op, m.uimm12, m.rn, rd));
                },
                .reg_reg => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, false, null, rd));
                },
                .reg_scaled => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, true, null, rd));
                },
                .reg_scaled_extended => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, true, m.extendop, rd));
                },
                .reg_extended => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, false, m.extendop, rd));
                },
                .sp_pre_indexed => |m| {
                    sink.put4(encLdstSimm9(op, m.simm9, 0b11, stackReg(), rd));
                },
                .sp_post_indexed => |m| {
                    sink.put4(encLdstSimm9(op, m.simm9, 0b01, stackReg(), rd));
                },
                .label, .fp_offset, .sp_offset, .incoming_arg, .slot_offset, .reg_offset, .constant => {
                    // These pseudo-modes should be resolved before emission
                    @panic("Pseudo addressing mode not resolved before emission");
                },
            }
        },

        // Store instructions
        .store8, .store16, .store32, .store64 => {
            const payload = switch (inst.*) {
                .store8 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b0011100000) },
                .store16 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b0111100000) },
                .store32 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b1011100000) },
                .store64 => |p| .{ .rd = p.rd, .mem = p.mem, .flags = p.flags, .op = @as(u32, 0b1111100000) },
                else => unreachable,
            };

            const rd = payload.rd;
            const op = payload.op;

            switch (payload.mem) {
                .unscaled => |m| {
                    sink.put4(encLdstSimm9(op, m.simm9, 0b00, m.rn, rd));
                },
                .unsigned_offset => |m| {
                    sink.put4(encLdstUimm12(op, m.uimm12, m.rn, rd));
                },
                .reg_reg => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, false, null, rd));
                },
                .reg_scaled => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, true, null, rd));
                },
                .reg_scaled_extended => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, true, m.extendop, rd));
                },
                .reg_extended => |m| {
                    sink.put4(encLdstReg(op, m.rn, m.rm, false, m.extendop, rd));
                },
                .sp_pre_indexed => |m| {
                    sink.put4(encLdstSimm9(op, m.simm9, 0b11, stackReg(), rd));
                },
                .sp_post_indexed => |m| {
                    sink.put4(encLdstSimm9(op, m.simm9, 0b01, stackReg(), rd));
                },
                .label => {
                    @panic("Store to a MemLabel not supported");
                },
                .fp_offset, .sp_offset, .incoming_arg, .slot_offset, .reg_offset, .constant => {
                    @panic("Pseudo addressing mode not resolved before emission");
                },
            }
        },

        // Load pair (64-bit)
        .load_p64 => |payload| {
            const rt = payload.rt.toReg();
            const rt2 = payload.rt2.toReg();
            switch (payload.mem) {
                .signed_offset => |m| {
                    sink.put4(encLdstPair(0b1010100101, m.simm7, m.reg, rt, rt2));
                },
                .sp_pre_indexed => |m| {
                    sink.put4(encLdstPair(0b1010100111, m.simm7, stackReg(), rt, rt2));
                },
                .sp_post_indexed => |m| {
                    sink.put4(encLdstPair(0b1010100011, m.simm7, stackReg(), rt, rt2));
                },
            }
        },

        // Store pair (64-bit)
        .store_p64 => |payload| {
            switch (payload.mem) {
                .signed_offset => |m| {
                    sink.put4(encLdstPair(0b1010100100, m.simm7, m.reg, payload.rt, payload.rt2));
                },
                .sp_pre_indexed => |m| {
                    sink.put4(encLdstPair(0b1010100110, m.simm7, stackReg(), payload.rt, payload.rt2));
                },
                .sp_post_indexed => |m| {
                    sink.put4(encLdstPair(0b1010100010, m.simm7, stackReg(), payload.rt, payload.rt2));
                },
            }
        },

        // For unimplemented instructions, emit a trap
        else => {
            _ = state;
            sink.put4(0xd43e0000); // BRK
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

    buf.put4(0x12345678);
    try testing.expectEqual(@as(u32, 4), buf.curOffset());

    buf.put4(0xABCDEF00);
    try testing.expectEqual(@as(u32, 8), buf.curOffset());

    try testing.expectEqual(@as(usize, 8), buf.data.items.len);
}
