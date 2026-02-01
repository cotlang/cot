//! AArch64 ISA definitions: machine instruction types.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/aarch64/inst/mod.rs`

const std = @import("std");
const Allocator = std.mem.Allocator;

// Re-export sub-modules
pub const regs = @import("regs.zig");
pub const imms = @import("imms.zig");
pub const args = @import("args.zig");
pub const emit = @import("emit.zig");

// Re-export commonly used types
pub const xreg = regs.xreg;
pub const vreg = regs.vreg;
pub const zeroReg = regs.zeroReg;
pub const stackReg = regs.stackReg;
pub const linkReg = regs.linkReg;
pub const fpReg = regs.fpReg;
pub const spilltmpReg = regs.spilltmpReg;
pub const writableXreg = regs.writableXreg;
pub const writableVreg = regs.writableVreg;
pub const prettyPrintReg = regs.prettyPrintReg;
pub const prettyPrintIreg = regs.prettyPrintIreg;

pub const OperandSize = args.OperandSize;
pub const ScalarSize = args.ScalarSize;
pub const VectorSize = args.VectorSize;
pub const Cond = args.Cond;
pub const CondBrKind = args.CondBrKind;
pub const BranchTarget = args.BranchTarget;
pub const ShiftOp = args.ShiftOp;
pub const ShiftOpShiftImm = args.ShiftOpShiftImm;
pub const ShiftOpAndAmt = args.ShiftOpAndAmt;
pub const ExtendOp = args.ExtendOp;
pub const MemLabel = args.MemLabel;
pub const APIKey = args.APIKey;
pub const TestBitAndBranchKind = args.TestBitAndBranchKind;
pub const BranchTargetType = args.BranchTargetType;
pub const Type = args.Type;

pub const NZCV = imms.NZCV;
pub const UImm5 = imms.UImm5;
pub const SImm7Scaled = imms.SImm7Scaled;
pub const FPULeftShiftImm = imms.FPULeftShiftImm;
pub const FPURightShiftImm = imms.FPURightShiftImm;
pub const SImm9 = imms.SImm9;
pub const UImm12Scaled = imms.UImm12Scaled;
pub const Imm12 = imms.Imm12;
pub const ImmLogic = imms.ImmLogic;
pub const ImmShift = imms.ImmShift;
pub const MoveWideConst = imms.MoveWideConst;
pub const ASIMDMovModImm = imms.ASIMDMovModImm;
pub const ASIMDFPModImm = imms.ASIMDFPModImm;

// Import from local modules
const regs_mod = @import("regs.zig");
const Reg = regs_mod.Reg;
const PReg = regs_mod.PReg;
const VReg = regs_mod.VReg;
const RegClass = regs_mod.RegClass;
const Writable = regs_mod.Writable;
const MachLabel = args.MachLabel;

// Stub types
pub const CallArgList = std.ArrayListUnmanaged(CallArgPair);

pub const MemFlags = struct {
    aligned: bool = true,
    readonly: bool = false,
    trap: bool = false,
    notrap: bool = false,

    pub const empty = MemFlags{};
};

pub const CallArgPair = struct {
    vreg: Reg,
    preg: PReg,
};

//=============================================================================
// Instructions (top level): definition

/// ALU operations (two-operand).
pub const ALUOp = enum {
    /// Add.
    add,
    /// Subtract.
    sub,
    /// Bitwise OR.
    orr,
    /// Bitwise AND.
    @"and",
    /// Bitwise AND with flags update.
    ands,
    /// Bitwise XOR.
    eor,
    /// Add with flags update.
    adds,
    /// Subtract with flags update.
    subs,
    /// Signed multiply high.
    smulh,
    /// Unsigned multiply high.
    umulh,
    /// Signed divide.
    sdiv,
    /// Unsigned divide.
    udiv,
    /// Bitwise AND NOT.
    and_not,
    /// Bitwise OR NOT.
    orr_not,
    /// Bitwise XOR NOT.
    eor_not,
    /// Extract (bitfield extract).
    extr,
    /// Logical shift right.
    lsr,
    /// Arithmetic shift right.
    asr,
    /// Logical shift left.
    lsl,
    /// Add with carry.
    adc,
    /// Add with carry and flags update.
    adcs,
    /// Subtract with carry (borrow).
    sbc,
    /// Subtract with carry and flags update.
    sbcs,

    /// Get the assembly mnemonic for this opcode.
    pub fn opStr(self: ALUOp) []const u8 {
        return switch (self) {
            .add => "add",
            .sub => "sub",
            .orr => "orr",
            .@"and" => "and",
            .ands => "ands",
            .eor => "eor",
            .adds => "adds",
            .subs => "subs",
            .smulh => "smulh",
            .umulh => "umulh",
            .sdiv => "sdiv",
            .udiv => "udiv",
            .and_not => "bic",
            .orr_not => "orn",
            .eor_not => "eon",
            .extr => "extr",
            .lsr => "lsr",
            .asr => "asr",
            .lsl => "lsl",
            .adc => "adc",
            .adcs => "adcs",
            .sbc => "sbc",
            .sbcs => "sbcs",
        };
    }
};

/// ALU operations (three-operand, e.g., madd).
pub const ALUOp3 = enum {
    /// Multiply-add.
    madd,
    /// Multiply-subtract.
    msub,
    /// Unsigned multiply-add long.
    umaddl,
    /// Signed multiply-add long.
    smaddl,
};

/// Bit manipulation operations.
pub const BitOp = enum {
    /// Reverse bits.
    rbit,
    /// Count leading zeros.
    clz,
    /// Count leading sign bits.
    cls,
    /// Reverse bytes in 16-bit halfwords.
    rev16,
    /// Reverse bytes in 32-bit words.
    rev32,
    /// Reverse bytes in 64-bit doubleword.
    rev64,

    /// Get the assembly mnemonic for this opcode.
    pub fn opStr(self: BitOp) []const u8 {
        return switch (self) {
            .rbit => "rbit",
            .clz => "clz",
            .cls => "cls",
            .rev16 => "rev16",
            .rev32 => "rev32",
            .rev64 => "rev64",
        };
    }
};

/// FPU operations (single-operand).
pub const FPUOp1 = enum {
    abs,
    neg,
    sqrt,
    cvt32_to_64,
    cvt64_to_32,
};

/// FPU operations (two-operand).
pub const FPUOp2 = enum {
    add,
    sub,
    mul,
    div,
    max,
    min,
};

/// FPU operations (three-operand).
pub const FPUOp3 = enum {
    madd,
    msub,
};

/// FPU rounding modes.
pub const FpuRoundMode = enum {
    minus_infinity,
    plus_infinity,
    zero,
    nearest,
};

/// FPU to integer conversion operations.
pub const FpuToIntOp = enum {
    f32_to_u32,
    f32_to_i32,
    f32_to_u64,
    f32_to_i64,
    f64_to_u32,
    f64_to_i32,
    f64_to_u64,
    f64_to_i64,
};

/// Integer to FPU conversion operations.
pub const IntToFpuOp = enum {
    u32_to_f32,
    i32_to_f32,
    u64_to_f32,
    i64_to_f32,
    u32_to_f64,
    i32_to_f64,
    u64_to_f64,
    i64_to_f64,
};

/// Move wide operations.
pub const MoveWideOp = enum {
    /// Move with zero (MOVZ).
    movz,
    /// Move with NOT (MOVN).
    movn,
};

/// Atomic read-modify-write operations.
pub const AtomicRMWOp = enum {
    add,
    clr,
    eor,
    set,
    smax,
    umax,
    smin,
    umin,
    swp,
};

/// Atomic read-modify-write loop operations.
pub const AtomicRMWLoopOp = enum {
    add,
    sub,
    eor,
    orr,
    @"and",
    nand,
    smin,
    smax,
    umin,
    umax,
    xchg,
};

/// Vector ALU operations.
pub const VecALUOp = enum {
    add,
    sub,
    mul,
    sqadd,
    uqadd,
    sqsub,
    uqsub,
    smin,
    umin,
    smax,
    umax,
    @"and",
    orr,
    eor,
    bic,
    bsl,
    umaxp,
    cmeq,
    cmge,
    cmgt,
    cmhs,
    cmhi,
    sshl,
    ushl,
    srshl,
    urshl,
    addp,
    umlal,
    smull,
    smull2,
    umull,
    umull2,
    fmul,
    fdiv,
    fmax,
    fmin,
    fmaxnm,
    fminnm,
    fadd,
    fsub,
    fcmeq,
    fcmge,
    fcmgt,
};

/// Vector ALU operations with modifier.
pub const VecALUModOp = enum {
    /// Signed saturating rounding doubling multiply accumulate returning high half.
    sqrdmlah,
    /// Signed saturating rounding doubling multiply subtract returning high half.
    sqrdmlsh,
    /// Multiply accumulate long.
    umlal,
    /// Fused multiply-add.
    fmla,
    /// Fused multiply-subtract.
    fmls,
};

/// Vector miscellaneous operations (two-operand).
pub const VecMisc2 = enum {
    not,
    neg,
    abs,
    fabs,
    fneg,
    fsqrt,
    rev16,
    rev32,
    rev64,
    fcvtzs,
    fcvtzu,
    scvtf,
    ucvtf,
    frintn,
    frintz,
    frintm,
    frintp,
    cnt,
    cmeq0,
    cmge0,
    cmgt0,
    cmle0,
    cmlt0,
    fcmeq0,
    fcmge0,
    fcmgt0,
    fcmle0,
    fcmlt0,
};

/// Vector lanes operations.
pub const VecLanesOp = enum {
    addv,
    uminv,
    saddlv,
    uaddlv,
};

/// Vector pair operations.
pub const VecPairOp = enum {
    addp,
};

/// Vector shift immediate operations.
pub const VecShiftImmOp = enum {
    shl,
    sshr,
    ushr,
};

/// Vector shift immediate operations with modifier.
pub const VecShiftImmModOp = enum {
    sli,
    sri,
    srshr,
    urshr,
    ssra,
    usra,
};

/// Vector extend operations.
pub const VecExtendOp = enum {
    sxtl,
    sxtl2,
    uxtl,
    uxtl2,
};

/// Vector RR long operations.
pub const VecRRLongOp = enum {
    fcvtl,
    fcvtl2,
    shll,
    shll2,
};

/// Vector RR narrow operations.
pub const VecRRNarrowOp = enum {
    xtn,
    sqxtn,
    sqxtun,
    uqxtn,
    fcvtn,
};

/// Vector RRR long operations.
pub const VecRRRLongOp = enum {
    smull,
    smull2,
    umull,
    umull2,
    umlsl,
    umlsl2,
    smlsl,
    smlsl2,
};

/// Vector RRR long operations with modifier.
pub const VecRRRLongModOp = enum {
    umlal,
    umlal2,
    smlal,
    smlal2,
};

/// Vector RR pair long operations.
pub const VecRRPairLongOp = enum {
    saddlp,
    uaddlp,
};

/// A floating-point unit (FPU) operation with two args, a register and an immediate.
pub const FPUOpRI = union(enum) {
    /// Unsigned right shift (32-bit). Rd = Rn << #imm
    ushr32: FPURightShiftImm,
    /// Unsigned right shift (64-bit). Rd = Rn << #imm
    ushr64: FPURightShiftImm,
};

/// A floating-point unit (FPU) operation with two args, a register and
/// an immediate that modifies its dest.
pub const FPUOpRIMod = union(enum) {
    /// Shift left and insert (32-bit). Rd |= Rn << #imm
    sli32: FPULeftShiftImm,
    /// Shift left and insert (64-bit). Rd |= Rn << #imm
    sli64: FPULeftShiftImm,
};

/// Addressing mode for memory operations.
pub const AMode = union(enum) {
    /// Unscaled immediate offset: [reg + simm9]
    unscaled: struct { rn: Reg, simm9: SImm9 },
    /// Unsigned scaled offset: [reg + uimm12 * scale]
    unsigned_offset: struct { rn: Reg, uimm12: UImm12Scaled },
    /// Register + register: [reg + reg]
    reg_reg: struct { rn: Reg, rm: Reg },
    /// Register + scaled register: [reg + reg * scale]
    reg_scaled: struct { rn: Reg, rm: Reg },
    /// Register + scaled register with extend: [reg + ext(reg) * scale]
    reg_scaled_extended: struct { rn: Reg, rm: Reg, extendop: ExtendOp },
    /// Register + extended register: [reg + ext(reg)]
    reg_extended: struct { rn: Reg, rm: Reg, extendop: ExtendOp },
    /// PC-relative label.
    label: struct { label: MemLabel },
    /// SP pre-indexed: [sp + simm9]!
    sp_pre_indexed: struct { simm9: SImm9 },
    /// SP post-indexed: [sp], simm9
    sp_post_indexed: struct { simm9: SImm9 },
    /// FP-relative offset (pseudo-mode, resolved before emission).
    fp_offset: struct { offset: i64 },
    /// SP-relative offset (pseudo-mode, resolved before emission).
    sp_offset: struct { offset: i64 },
    /// Incoming argument offset (pseudo-mode).
    incoming_arg: struct { offset: i64 },
    /// Slot offset (pseudo-mode).
    slot_offset: struct { offset: i64 },
    /// Register + immediate offset (pseudo-mode).
    reg_offset: struct { rn: Reg, offset: i64, ty: Type },
    /// Constant pool address.
    constant: struct { addr: MachLabel },

    /// Memory reference using an address in a register.
    pub fn reg(register: Reg) AMode {
        return AMode{
            .unsigned_offset = .{
                .rn = register,
                .uimm12 = UImm12Scaled.zero(Type.i64),
            },
        };
    }

    /// Memory reference using `reg1 + sizeof(ty) * reg2` as an address.
    pub fn regPlusRegScaledExtended(reg1: Reg, reg2: Reg, op: ExtendOp) AMode {
        return AMode{
            .reg_scaled_extended = .{
                .rn = reg1,
                .rm = reg2,
                .extendop = op,
            },
        };
    }

    pub fn prettyPrint(self: AMode, size_bytes: u8) []const u8 {
        _ = self;
        _ = size_bytes;
        return ""; // TODO: implement pretty printing
    }
};

/// Pair addressing mode.
pub const PairAMode = union(enum) {
    /// Signed offset: [reg + simm7]
    signed_offset: struct { reg: Reg, simm7: SImm7Scaled },
    /// SP pre-indexed: [sp + simm7]!
    sp_pre_indexed: struct { simm7: SImm7Scaled },
    /// SP post-indexed: [sp], simm7
    sp_post_indexed: struct { simm7: SImm7Scaled },

    pub fn prettyPrintDefault(self: PairAMode) []const u8 {
        _ = self;
        return ""; // TODO: implement pretty printing
    }
};

/// Additional information for return_call[_ind] instructions.
pub fn ReturnCallInfo(comptime T: type) type {
    return struct {
        /// Where this call is going to.
        dest: T,
        /// Arguments to the call instruction.
        uses: CallArgList,
        /// The size of the new stack frame's stack arguments.
        new_stack_arg_size: u32,
        /// API key to use to restore the return address, if any.
        key: ?APIKey,
    };
}

/// AArch64 machine instruction.
pub const Inst = union(enum) {
    // Zero-length NOP (for alignment).
    nop0,

    // 4-byte NOP.
    nop4,

    // ALU operation: rd = rn op rm
    alu_rrr: struct {
        alu_op: ALUOp,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
    },

    // ALU operation with three source registers: rd = rn op rm op ra
    alu_rrrr: struct {
        alu_op: ALUOp3,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        ra: Reg,
    },

    // ALU operation with 12-bit immediate: rd = rn op imm12
    alu_rr_imm12: struct {
        alu_op: ALUOp,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
        imm12: Imm12,
    },

    // ALU operation with logical immediate: rd = rn op imm_logic
    alu_rr_imm_logic: struct {
        alu_op: ALUOp,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
        imml: ImmLogic,
    },

    // ALU operation with shift immediate: rd = rn op imm_shift
    alu_rr_imm_shift: struct {
        alu_op: ALUOp,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
        immshift: ImmShift,
    },

    // ALU operation with shifted register: rd = rn op (rm shift amt)
    alu_rrr_shift: struct {
        alu_op: ALUOp,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        shiftop: ShiftOpAndAmt,
    },

    // ALU operation with extended register: rd = rn op ext(rm)
    alu_rrr_extend: struct {
        alu_op: ALUOp,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        extendop: ExtendOp,
    },

    // Bit manipulation operation: rd = op(rn)
    bit_rr: struct {
        op: BitOp,
        size: OperandSize,
        rd: Writable(Reg),
        rn: Reg,
    },

    // Unsigned byte load: rd = zero_extend(mem[rn + offset])
    uload8: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // Signed byte load.
    sload8: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // Unsigned halfword load.
    uload16: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // Signed halfword load.
    sload16: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // Unsigned word load.
    uload32: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // Signed word load.
    sload32: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // Doubleword load.
    uload64: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // Byte store.
    store8: struct {
        rd: Reg,
        mem: AMode,
        flags: MemFlags,
    },

    // Halfword store.
    store16: struct {
        rd: Reg,
        mem: AMode,
        flags: MemFlags,
    },

    // Word store.
    store32: struct {
        rd: Reg,
        mem: AMode,
        flags: MemFlags,
    },

    // Doubleword store.
    store64: struct {
        rd: Reg,
        mem: AMode,
        flags: MemFlags,
    },

    // Store pair of 64-bit registers.
    store_p64: struct {
        rt: Reg,
        rt2: Reg,
        mem: PairAMode,
        flags: MemFlags,
    },

    // Load pair of 64-bit registers.
    load_p64: struct {
        rt: Writable(Reg),
        rt2: Writable(Reg),
        mem: PairAMode,
        flags: MemFlags,
    },

    // Move: rd = rm
    mov: struct {
        size: OperandSize,
        rd: Writable(Reg),
        rm: Reg,
    },

    // Move from physical register.
    mov_from_preg: struct {
        rd: Writable(Reg),
        rm: PReg,
    },

    // Move to physical register.
    mov_to_preg: struct {
        rd: PReg,
        rm: Reg,
    },

    // Move keep: rd = rd | (imm << shift)
    movk: struct {
        rd: Writable(Reg),
        rn: Reg,
        imm: MoveWideConst,
        size: OperandSize,
    },

    // Move wide (zero/not): rd = imm << shift
    mov_wide: struct {
        op: MoveWideOp,
        rd: Writable(Reg),
        imm: MoveWideConst,
        size: OperandSize,
    },

    // Conditional select: rd = cond ? rn : rm
    csel: struct {
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        cond: Cond,
    },

    // Conditional select negate: rd = cond ? rn : -rm
    csneg: struct {
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        cond: Cond,
    },

    // Conditional set: rd = cond ? 1 : 0
    cset: struct {
        rd: Writable(Reg),
        cond: Cond,
    },

    // Conditional set mask: rd = cond ? -1 : 0
    csetm: struct {
        rd: Writable(Reg),
        cond: Cond,
    },

    // Conditional compare: flags = cond ? compare(rn, rm) : nzcv
    ccmp: struct {
        size: OperandSize,
        rn: Reg,
        rm: Reg,
        nzcv: NZCV,
        cond: Cond,
    },

    // Conditional compare immediate.
    ccmp_imm: struct {
        size: OperandSize,
        rn: Reg,
        imm: UImm5,
        nzcv: NZCV,
        cond: Cond,
    },

    // Fence (data memory barrier).
    fence,

    // Consumption of speculative data barrier.
    csdb,

    // FPU move (32-bit).
    fpu_move32: struct {
        rd: Writable(Reg),
        rn: Reg,
    },

    // FPU move (64-bit).
    fpu_move64: struct {
        rd: Writable(Reg),
        rn: Reg,
    },

    // FPU move (128-bit).
    fpu_move128: struct {
        rd: Writable(Reg),
        rn: Reg,
    },

    // Sign/zero extend.
    extend: struct {
        rd: Writable(Reg),
        rn: Reg,
        signed: bool,
        from_bits: u8,
        to_bits: u8,
    },

    // Unconditional jump.
    jump: struct {
        dest: BranchTarget,
    },

    // Conditional branch.
    cond_br: struct {
        kind: CondBrKind,
        taken: BranchTarget,
        not_taken: BranchTarget,
    },

    // Test bit and branch.
    test_bit_and_branch: struct {
        kind: TestBitAndBranchKind,
        rn: Reg,
        bit: u6,
        taken: BranchTarget,
        not_taken: BranchTarget,
    },

    // Indirect branch.
    indirect_br: struct {
        rn: Reg,
        targets: []const MachLabel,
    },

    // Software breakpoint.
    brk,

    // Undefined instruction.
    udf: struct {
        trap_code: u16,
    },

    // Trap if condition.
    trap_if: struct {
        kind: CondBrKind,
        trap_code: u16,
    },

    // Address of PC-relative label.
    adr: struct {
        rd: Writable(Reg),
        label: MemLabel,
    },

    // Address of PC-relative page.
    adrp: struct {
        rd: Writable(Reg),
        label: MemLabel,
    },

    // Literal word.
    word4: struct {
        data: u32,
    },

    // Literal doubleword.
    word8: struct {
        data: u64,
    },

    // Load address.
    load_addr: struct {
        rd: Writable(Reg),
        mem: AMode,
    },

    // Branch target identification.
    bti: struct {
        targets: BranchTargetType,
    },

    // Return.
    ret,

    // FPU compare.
    fpu_cmp: struct {
        size: ScalarSize,
        rn: Reg,
        rm: Reg,
    },

    // FPU unary operation (abs, neg, sqrt, cvt).
    fpu_rr: struct {
        fpu_op: FPUOp1,
        size: ScalarSize,
        rd: Writable(Reg),
        rn: Reg,
    },

    // FPU binary operation (add, sub, mul, div, max, min).
    fpu_rrr: struct {
        fpu_op: FPUOp2,
        size: ScalarSize,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
    },

    // FPU ternary operation (madd, msub).
    fpu_rrrr: struct {
        fpu_op: FPUOp3,
        size: ScalarSize,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        ra: Reg,
    },

    // FPU rounding.
    fpu_round: struct {
        mode: FpuRoundMode,
        size: ScalarSize,
        rd: Writable(Reg),
        rn: Reg,
    },

    // FPU to integer conversion.
    fpu_to_int: struct {
        op: FpuToIntOp,
        rd: Writable(Reg),
        rn: Reg,
    },

    // Integer to FPU conversion.
    int_to_fpu: struct {
        op: IntToFpuOp,
        rd: Writable(Reg),
        rn: Reg,
    },

    // FPU load (32-bit).
    fpu_load32: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // FPU load (64-bit).
    fpu_load64: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // FPU load (128-bit).
    fpu_load128: struct {
        rd: Writable(Reg),
        mem: AMode,
        flags: MemFlags,
    },

    // FPU store (32-bit).
    fpu_store32: struct {
        rd: Reg,
        mem: AMode,
        flags: MemFlags,
    },

    // FPU store (64-bit).
    fpu_store64: struct {
        rd: Reg,
        mem: AMode,
        flags: MemFlags,
    },

    // FPU store (128-bit).
    fpu_store128: struct {
        rd: Reg,
        mem: AMode,
        flags: MemFlags,
    },

    // Move to NZCV flags.
    mov_to_nzcv: struct {
        rn: Reg,
    },

    // Move from NZCV flags.
    mov_from_nzcv: struct {
        rd: Writable(Reg),
    },

    // Call instruction.
    call: struct {
        dest: BranchTarget,
    },

    // Call indirect.
    call_ind: struct {
        rn: Reg,
    },

    /// Generic constructor for a load (zero-extending where appropriate).
    pub fn genLoad(into_reg: Writable(Reg), mem: AMode, ty: Type, flags: MemFlags) Inst {
        return switch (ty.kind) {
            .i8 => Inst{ .uload8 = .{ .rd = into_reg, .mem = mem, .flags = flags } },
            .i16 => Inst{ .uload16 = .{ .rd = into_reg, .mem = mem, .flags = flags } },
            .i32 => Inst{ .uload32 = .{ .rd = into_reg, .mem = mem, .flags = flags } },
            .i64 => Inst{ .uload64 = .{ .rd = into_reg, .mem = mem, .flags = flags } },
            else => unreachable,
        };
    }

    /// Generic constructor for a store.
    pub fn genStore(mem: AMode, from_reg: Reg, ty: Type, flags: MemFlags) Inst {
        return switch (ty.kind) {
            .i8 => Inst{ .store8 = .{ .rd = from_reg, .mem = mem, .flags = flags } },
            .i16 => Inst{ .store16 = .{ .rd = from_reg, .mem = mem, .flags = flags } },
            .i32 => Inst{ .store32 = .{ .rd = from_reg, .mem = mem, .flags = flags } },
            .i64 => Inst{ .store64 = .{ .rd = from_reg, .mem = mem, .flags = flags } },
            else => unreachable,
        };
    }

    /// Generate a jump to a label.
    pub fn genJump(target: MachLabel) Inst {
        return Inst{
            .jump = .{
                .dest = BranchTarget{ .label = target },
            },
        };
    }

    /// Generate a NOP.
    pub fn genNop(preferred_size: usize) Inst {
        if (preferred_size == 0) {
            return .nop0;
        }
        std.debug.assert(preferred_size >= 4);
        return .nop4;
    }

    /// Generate a move between two registers.
    pub fn genMove(to_reg: Writable(Reg), from_reg: Reg, ty: Type) Inst {
        const num_bits = ty.bits();
        std.debug.assert(num_bits <= 128);
        std.debug.assert(to_reg.toReg().class() == from_reg.class());

        return switch (from_reg.class()) {
            .int => Inst{
                .mov = .{
                    .size = .size64,
                    .rd = to_reg,
                    .rm = from_reg,
                },
            },
            .float => if (num_bits > 64)
                Inst{ .fpu_move128 = .{ .rd = to_reg, .rn = from_reg } }
            else
                Inst{ .fpu_move64 = .{ .rd = to_reg, .rn = from_reg } },
            .vector => unreachable,
        };
    }
};

fn countZeroHalfWords(value: u64, num_half_words: u8) usize {
    var val = value;
    var count: usize = 0;
    var i: u8 = 0;
    while (i < num_half_words) : (i += 1) {
        if (val & 0xffff == 0) {
            count += 1;
        }
        val >>= 16;
    }
    return count;
}

//=============================================================================
// Tests

test "ALUOp mnemonic" {
    const testing = std.testing;
    try testing.expectEqualStrings("add", ALUOp.add.opStr());
    try testing.expectEqualStrings("sub", ALUOp.sub.opStr());
    try testing.expectEqualStrings("ands", ALUOp.ands.opStr());
}

test "BitOp mnemonic" {
    const testing = std.testing;
    try testing.expectEqualStrings("clz", BitOp.clz.opStr());
    try testing.expectEqualStrings("rbit", BitOp.rbit.opStr());
}

test "Inst.genNop" {
    const testing = std.testing;
    try testing.expectEqual(Inst.nop0, Inst.genNop(0));
    try testing.expectEqual(Inst.nop4, Inst.genNop(4));
}

test "countZeroHalfWords" {
    const testing = std.testing;
    try testing.expectEqual(@as(usize, 4), countZeroHalfWords(0, 4));
    try testing.expectEqual(@as(usize, 3), countZeroHalfWords(0xffff, 4));
    try testing.expectEqual(@as(usize, 0), countZeroHalfWords(0xffff_ffff_ffff_ffff, 4));
}
