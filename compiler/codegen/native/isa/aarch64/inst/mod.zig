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
pub const get_operands = @import("get_operands.zig");

// Export LabelUse for MachBuffer parameterization (matches Cranelift pattern)
pub const LabelUse = emit.AArch64LabelUse;

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

// Import from machinst for unified types
const machinst_inst = @import("../../../machinst/inst.zig");
const machinst_reg = @import("../../../machinst/reg.zig");
const MachLabel = machinst_inst.MachLabel;

// Import emit module for MachBuffer
const emit_mod = @import("emit.zig");

// Import regalloc types
const regalloc_operand = @import("../../../regalloc/operand.zig");
const Allocation = regalloc_operand.Allocation;

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
    /// Move with keep (MOVK) - inserts 16-bit value at specified position.
    movk,
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
                .uimm12 = UImm12Scaled.zero(Type.I64),
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

    /// Jump table sequence pseudo-instruction.
    /// Expands to: bounds check, Spectre mitigation, table lookup, indirect branch, and table data.
    /// This must be emitted as a single unit - regalloc cannot insert spills/reloads in the middle.
    jt_sequence: struct {
        /// Index register (must be bounds-checked before this instruction).
        ridx: Reg,
        /// Temporary register 1 (for address computation).
        rtmp1: Writable(Reg),
        /// Temporary register 2 (for value loading).
        rtmp2: Writable(Reg),
        /// Default/out-of-bounds branch target.
        default: MachLabel,
        /// Table of branch targets (one per case).
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

    /// Load address of external symbol via GOT (Global Offset Table).
    /// Emits: adrp rd, :got:X; ldr rd, [rd, :got_lo12:X]
    /// Used for position-independent code (PIC).
    load_ext_name_got: struct {
        rd: Writable(Reg),
        /// Symbol index (references external symbol table).
        symbol_idx: u32,
    },

    /// Load address of external symbol (near, within ±4GB).
    /// Emits: adrp rd, X; add rd, rd, :lo12:X
    /// Used for static linking with nearby symbols.
    load_ext_name_near: struct {
        rd: Writable(Reg),
        /// Symbol index (references external symbol table).
        symbol_idx: u32,
        /// Offset from symbol (e.g., for struct field access).
        offset: i64,
    },

    /// Load address of external symbol (far, absolute address).
    /// Emits: ldr rd, #8; b #12; <8-byte address>
    /// Used when symbol may be outside ±4GB range.
    load_ext_name_far: struct {
        rd: Writable(Reg),
        /// Symbol index (references external symbol table).
        symbol_idx: u32,
        /// Offset from symbol.
        offset: i64,
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

    // ==========================================================================
    // Atomic Operations
    // ==========================================================================

    /// Atomic read-modify-write using LSE atomics (ARMv8.1+).
    /// Uses ldaddal, ldclral, ldeoral, ldsetal, ldsmaxal, ldumaxal, ldsminal, lduminal, swpal.
    atomic_rmw: struct {
        op: AtomicRMWOp,
        rs: Reg, // Value to combine with memory
        rt: Writable(Reg), // Destination for old value
        rn: Reg, // Address
        ty: Type, // I8, I16, I32, I64
    },

    /// Atomic read-modify-write loop (for systems without LSE).
    /// Uses ldaxr/stlxr loop with ALU operation.
    atomic_rmw_loop: struct {
        op: AtomicRMWLoopOp,
        addr: Reg, // Fixed to X25
        operand: Reg, // Fixed to X26
        oldval: Writable(Reg), // Fixed to X27
        scratch1: Writable(Reg), // Fixed to X24
        scratch2: Writable(Reg), // Fixed to X28
        ty: Type,
        flags: MemFlags,
    },

    /// Atomic compare-and-swap using LSE atomics (ARMv8.1+).
    /// Uses CASAL instruction.
    atomic_cas: struct {
        rd: Writable(Reg), // Destination (reuses rs)
        rs: Reg, // Expected value
        rt: Reg, // New value
        rn: Reg, // Address
        ty: Type,
    },

    /// Atomic compare-and-swap loop (for systems without LSE).
    /// Uses ldaxr/stlxr loop.
    atomic_cas_loop: struct {
        addr: Reg, // Fixed to X25
        expected: Reg, // Fixed to X26
        replacement: Reg, // Fixed to X28
        oldval: Writable(Reg), // Fixed to X27
        scratch: Writable(Reg), // Fixed to X24
        ty: Type,
        flags: MemFlags,
    },

    /// Load-acquire exclusive register.
    ldaxr: struct {
        rt: Writable(Reg),
        rn: Reg,
        ty: Type,
    },

    /// Store-release exclusive register.
    stlxr: struct {
        rs: Writable(Reg), // Status (0 = success)
        rt: Reg, // Value to store
        rn: Reg, // Address
        ty: Type,
    },

    /// Load-acquire register.
    ldar: struct {
        rt: Writable(Reg),
        rn: Reg,
        ty: Type,
    },

    /// Store-release register.
    stlr: struct {
        rt: Reg,
        rn: Reg,
        ty: Type,
    },

    // ==========================================================================
    // Vector/SIMD Operations
    // ==========================================================================

    /// Vector 3-register ALU operation (ADD, SUB, MUL, AND, ORR, EOR, etc.)
    vec_rrr: struct {
        op: VecALUOp,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        size: VectorSize,
    },

    /// Vector 3-register ALU with modifier (SQRDMLAH, UMLAL, FMLA, etc.)
    vec_rrr_mod: struct {
        op: VecALUModOp,
        rd: Writable(Reg),
        ri: Reg, // Input that rd is modified from
        rn: Reg,
        rm: Reg,
        size: VectorSize,
    },

    /// Vector 2-operand misc operation (NOT, NEG, ABS, etc.)
    vec_misc: struct {
        op: VecMisc2,
        rd: Writable(Reg),
        rn: Reg,
        size: VectorSize,
    },

    /// Vector lanes operation (ADDV, UMINV, etc.)
    vec_lanes: struct {
        op: VecLanesOp,
        rd: Writable(Reg),
        rn: Reg,
        size: VectorSize,
    },

    /// Vector shift by immediate
    vec_shift_imm: struct {
        op: VecShiftImmOp,
        rd: Writable(Reg),
        rn: Reg,
        size: VectorSize,
        imm: u8,
    },

    /// Vector duplicate from scalar
    vec_dup: struct {
        rd: Writable(Reg),
        rn: Reg,
        size: VectorSize,
    },

    /// Vector duplicate from immediate
    vec_dup_imm: struct {
        rd: Writable(Reg),
        imm: u64,
        size: VectorSize,
    },

    /// Move to vector element
    mov_to_vec: struct {
        rd: Writable(Reg),
        ri: Reg, // Source vector
        rn: Reg, // GPR to insert
        idx: u8,
        size: VectorSize,
    },

    /// Move from vector element
    mov_from_vec: struct {
        rd: Writable(Reg),
        rn: Reg, // Source vector
        idx: u8,
        size: VectorSize,
    },

    /// Vector shift immediate with modifier (SLI, SRI, etc.)
    vec_shift_imm_mod: struct {
        op: VecShiftImmModOp,
        rd: Writable(Reg),
        ri: Reg, // Input that rd is modified from
        rn: Reg,
        size: VectorSize,
        imm: u8,
    },

    /// Vector extend operations (SXTL, UXTL, etc.)
    vec_extend: struct {
        op: VecExtendOp,
        rd: Writable(Reg),
        rn: Reg,
        /// High bit indicates 128-bit source (sxtl2/uxtl2)
        high_half: bool,
    },

    /// Vector RR long operations (FCVTL, SHLL, etc.)
    vec_rr_long: struct {
        op: VecRRLongOp,
        rd: Writable(Reg),
        rn: Reg,
        /// High bit indicates 128-bit source (fcvtl2/shll2)
        high_half: bool,
    },

    /// Vector RR narrow operations (XTN, SQXTN, etc.)
    vec_rr_narrow: struct {
        op: VecRRNarrowOp,
        rd: Writable(Reg),
        rn: Reg,
        /// High bit indicates writing to upper half
        high_half: bool,
    },

    /// Vector RRR long operations (SMULL, UMULL, etc.)
    vec_rrr_long: struct {
        op: VecRRRLongOp,
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        /// High bit indicates 128-bit source (smull2/umull2)
        high_half: bool,
    },

    /// Vector RRR long operations with modifier (UMLAL, SMLAL, etc.)
    vec_rrr_long_mod: struct {
        op: VecRRRLongModOp,
        rd: Writable(Reg),
        ri: Reg, // Input that rd is modified from
        rn: Reg,
        rm: Reg,
        /// High bit indicates 128-bit source (umlal2/smlal2)
        high_half: bool,
    },

    /// Vector RR pair long operations (SADDLP, UADDLP)
    vec_rr_pair_long: struct {
        op: VecRRPairLongOp,
        rd: Writable(Reg),
        rn: Reg,
        size: VectorSize,
    },

    /// Vector extract (EXT instruction)
    vec_extract: struct {
        rd: Writable(Reg),
        rn: Reg,
        rm: Reg,
        imm4: u8, // Extract index (0-15)
    },

    /// Vector table lookup (TBL)
    vec_tbl: struct {
        rd: Writable(Reg),
        rn: Reg, // Table register
        rm: Reg, // Index register
    },

    /// Vector table lookup with extension (TBX)
    vec_tbl_ext: struct {
        rd: Writable(Reg),
        ri: Reg, // Input that rd is modified from
        rn: Reg, // Table register
        rm: Reg, // Index register
    },

    /// Generic constructor for a load (zero-extending where appropriate).
    pub fn genLoad(into_reg: Writable(Reg), mem: AMode, ty: Type, flags: MemFlags) Inst {
        // Use bits() for type comparison since CLIF Type is a struct, not enum
        const bits_val = ty.bits();
        if (bits_val <= 8) return Inst{ .uload8 = .{ .rd = into_reg, .mem = mem, .flags = flags } };
        if (bits_val <= 16) return Inst{ .uload16 = .{ .rd = into_reg, .mem = mem, .flags = flags } };
        if (bits_val <= 32) return Inst{ .uload32 = .{ .rd = into_reg, .mem = mem, .flags = flags } };
        if (bits_val <= 64) return Inst{ .uload64 = .{ .rd = into_reg, .mem = mem, .flags = flags } };
        unreachable;
    }

    /// Generic constructor for a store.
    pub fn genStore(mem: AMode, from_reg: Reg, ty: Type, flags: MemFlags) Inst {
        // Use bits() for type comparison since CLIF Type is a struct, not enum
        const bits_val = ty.bits();
        if (bits_val <= 8) return Inst{ .store8 = .{ .rd = from_reg, .mem = mem, .flags = flags } };
        if (bits_val <= 16) return Inst{ .store16 = .{ .rd = from_reg, .mem = mem, .flags = flags } };
        if (bits_val <= 32) return Inst{ .store32 = .{ .rd = from_reg, .mem = mem, .flags = flags } };
        if (bits_val <= 64) return Inst{ .store64 = .{ .rd = from_reg, .mem = mem, .flags = flags } };
        unreachable;
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

    /// LabelUse type for this instruction type (for MachBuffer parameterization).
    /// Matches Cranelift's pattern where VCodeInst trait has associated LabelUse type.
    pub const LabelUse = emit_mod.AArch64LabelUse;

    /// EmitInfo type for this instruction type.
    pub const EmitInfo = emit_mod.EmitInfo;

    /// Check if this is a "low-level" branch that should not appear in VCode.
    /// Low-level branches are internal implementation details that get expanded
    /// into real branch instructions during emission.
    pub fn isLowLevelBranch(self: Inst) bool {
        // AArch64 doesn't have low-level branches that need special handling.
        // All branches in VCode are valid high-level branches.
        _ = self;
        return false;
    }

    /// Stub: Get operands for register allocation.
    /// TODO: Implement proper operand collection via get_operands.zig
    pub fn getOperands(_: Inst) []const machinst_reg.Operand {
        return &[_]machinst_reg.Operand{};
    }

    /// Stub: Get clobbered registers.
    /// TODO: Implement proper clobber tracking
    pub fn getClobbers(_: Inst) machinst_reg.PRegSet {
        return machinst_reg.PRegSet.empty();
    }

    /// Emit this instruction with register allocations applied.
    /// Takes the allocations for this instruction's operands (in the order
    /// they were collected by getOperands: defs first, then uses) and emits
    /// the instruction with virtual registers replaced by physical registers.
    pub fn emitWithAllocs(
        self: *const Inst,
        sink: *emit_mod.MachBuffer,
        allocs: []const Allocation,
        emit_info: *const emit_mod.EmitInfo,
    ) !void {
        // Create a mutable copy of the instruction
        var inst_copy = self.*;

        // Apply allocations to the instruction
        // The allocation order matches getOperands: defs first, then uses
        var alloc_idx: usize = 0;

        switch (inst_copy) {
            // Instructions with no register operands
            .nop0, .nop4, .fence, .csdb, .brk, .ret => {},

            .udf => {},
            .word4 => {},
            .word8 => {},
            .bti => {},

            // ALU operations: rd (def), rn (use), rm (use)
            .alu_rrr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // ALU with 4 registers: rd (def), rn (use), rm (use), ra (use)
            .alu_rrrr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ra = applyAlloc(p.ra, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // ALU with 12-bit immediate: rd (def), rn (use)
            .alu_rr_imm12 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // ALU with logic immediate: rd (def), rn (use)
            .alu_rr_imm_logic => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // ALU with shift immediate: rd (def), rn (use)
            .alu_rr_imm_shift => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // ALU with shift: rd (def), rn (use), rm (use)
            .alu_rrr_shift => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // ALU with extend: rd (def), rn (use), rm (use)
            .alu_rrr_extend => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Bit operations: rd (def), rn (use)
            .bit_rr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Loads: rd (def), mem (uses)
            .uload8 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .sload8 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .uload16 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .sload16 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .uload32 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .sload32 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .uload64 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },

            // Stores: rd (use), mem (uses)
            .store8 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAlloc(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .store16 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAlloc(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .store32 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAlloc(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .store64 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAlloc(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },

            // Pair load: rt (def), rt2 (def), mem (uses)
            .load_p64 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rt = applyAllocWritable(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rt2 = applyAllocWritable(p.rt2, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocPairAMode(&p.mem, allocs[alloc_idx..]);
            },

            // Pair store: rt (use), rt2 (use), mem (uses)
            .store_p64 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rt = applyAlloc(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rt2 = applyAlloc(p.rt2, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocPairAMode(&p.mem, allocs[alloc_idx..]);
            },

            // Move: rd (def), rm (use)
            .mov => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Move from preg: rd (def) - preg is already physical
            .mov_from_preg => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Move to preg: rm (use) - preg is already physical
            .mov_to_preg => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Move wide: rd (def)
            .mov_wide => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Movk: rd (def/use - read-modify-write), rn is source for rd
            .movk => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Conditional select: rd (def), rn (use), rm (use)
            .csel => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .csneg => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Conditional set: rd (def)
            .cset => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .csetm => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Conditional compare: rn (use), rm (use)
            .ccmp => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Conditional compare immediate: rn (use)
            .ccmp_imm => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // FPU moves: rd (def), rn (use)
            .fpu_move32 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .fpu_move64 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .fpu_move128 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Extend: rd (def), rn (use)
            .extend => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Control flow - no register operands (labels only)
            .jump, .cond_br => {},

            // Test and branch: rn (use)
            .test_bit_and_branch => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Indirect branch: rn (use)
            .indirect_br => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Jump table sequence: ridx (use), rtmp1 (def), rtmp2 (def)
            .jt_sequence => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rtmp1 = applyAllocWritable(p.rtmp1, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rtmp2 = applyAllocWritable(p.rtmp2, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ridx = applyAlloc(p.ridx, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Trap if: same as cond_br
            .trap_if => {},

            // ADR: rd (def)
            .adr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            // ADRP: rd (def)
            .adrp => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Load address: rd (def), mem (uses)
            .load_addr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },

            // External symbol loads: rd (def)
            .load_ext_name_got => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .load_ext_name_near => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .load_ext_name_far => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // FPU compare: rn (use), rm (use)
            .fpu_cmp => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // FPU unary: rd (def), rn (use)
            .fpu_rr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .fpu_round => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .fpu_to_int => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .int_to_fpu => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // FPU binary: rd (def), rn (use), rm (use)
            .fpu_rrr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // FPU ternary: rd (def), rn (use), rm (use), ra (use)
            .fpu_rrrr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ra = applyAlloc(p.ra, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // FPU loads: rd (def), mem (uses)
            .fpu_load32 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .fpu_load64 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .fpu_load128 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },

            // FPU stores: rd (use), mem (uses)
            .fpu_store32 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAlloc(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .fpu_store64 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAlloc(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },
            .fpu_store128 => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAlloc(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                alloc_idx += applyAllocAMode(&p.mem, allocs[alloc_idx..]);
            },

            // Move to NZCV: rn (use)
            .mov_to_nzcv => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Move from NZCV: rd (def)
            .mov_from_nzcv => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Call: no register operands (target is label/address)
            .call, .call_ind => {},

            // Atomic operations
            .atomic_rmw => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rt = applyAllocWritable(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rs = applyAlloc(p.rs, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .atomic_rmw_loop => |*p| {
                if (alloc_idx < allocs.len) {
                    p.oldval = applyAllocWritable(p.oldval, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.scratch1 = applyAllocWritable(p.scratch1, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.scratch2 = applyAllocWritable(p.scratch2, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.addr = applyAlloc(p.addr, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.operand = applyAlloc(p.operand, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .atomic_cas => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rs = applyAlloc(p.rs, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rt = applyAlloc(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .atomic_cas_loop => |*p| {
                if (alloc_idx < allocs.len) {
                    p.oldval = applyAllocWritable(p.oldval, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.scratch = applyAllocWritable(p.scratch, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.addr = applyAlloc(p.addr, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.expected = applyAlloc(p.expected, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.replacement = applyAlloc(p.replacement, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .ldaxr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rt = applyAllocWritable(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .stlxr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rs = applyAllocWritable(p.rs, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rt = applyAlloc(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .ldar => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rt = applyAllocWritable(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .stlr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rt = applyAlloc(p.rt, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            // Vector operations
            .vec_rrr => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_rrr_mod => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ri = applyAlloc(p.ri, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_misc => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .vec_lanes => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_shift_imm => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_dup => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_dup_imm => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .mov_to_vec => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ri = applyAlloc(p.ri, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .mov_from_vec => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_shift_imm_mod => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ri = applyAlloc(p.ri, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_extend => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .vec_rr_long => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
            .vec_rr_narrow => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_rrr_long => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_rrr_long_mod => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ri = applyAlloc(p.ri, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_rr_pair_long => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_extract => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_tbl => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },

            .vec_tbl_ext => |*p| {
                if (alloc_idx < allocs.len) {
                    p.rd = applyAllocWritable(p.rd, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.ri = applyAlloc(p.ri, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rn = applyAlloc(p.rn, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
                if (alloc_idx < allocs.len) {
                    p.rm = applyAlloc(p.rm, allocs[alloc_idx]);
                    alloc_idx += 1;
                }
            },
        }

        // Emit the instruction with resolved registers
        var state = emit_mod.EmitState{};
        try emit_mod.emit(&inst_copy, sink, emit_info, &state);
    }
};

/// Apply an allocation to a register, returning the physical register.
fn applyAlloc(reg: Reg, alloc: Allocation) Reg {
    if (alloc.isNone()) {
        // No allocation - register is already physical or unused
        return reg;
    }
    if (alloc.asReg()) |regalloc_preg| {
        // Convert regalloc PReg to machinst PReg (they have same encoding but different types)
        const machinst_preg = machinst_reg.PReg.init(
            @intCast(regalloc_preg.hwEnc()),
            switch (regalloc_preg.class()) {
                .int => machinst_reg.RegClass.int,
                .float => machinst_reg.RegClass.float,
                .vector => machinst_reg.RegClass.vector,
            },
        );
        return Reg.fromPReg(machinst_preg);
    }
    // Stack allocation - should have been handled by regalloc edits
    // Return original register (will cause emit error if virtual)
    return reg;
}

/// Apply an allocation to a writable register.
fn applyAllocWritable(reg: Writable(Reg), alloc: Allocation) Writable(Reg) {
    return Writable(Reg).fromReg(applyAlloc(reg.toReg(), alloc));
}

/// Apply allocations to an addressing mode. Returns number of allocations consumed.
fn applyAllocAMode(mem: *AMode, allocs: []const Allocation) usize {
    var count: usize = 0;
    switch (mem.*) {
        .unscaled => |*m| {
            if (count < allocs.len) {
                m.rn = applyAlloc(m.rn, allocs[count]);
                count += 1;
            }
        },
        .unsigned_offset => |*m| {
            if (count < allocs.len) {
                m.rn = applyAlloc(m.rn, allocs[count]);
                count += 1;
            }
        },
        .reg_reg => |*m| {
            if (count < allocs.len) {
                m.rn = applyAlloc(m.rn, allocs[count]);
                count += 1;
            }
            if (count < allocs.len) {
                m.rm = applyAlloc(m.rm, allocs[count]);
                count += 1;
            }
        },
        .reg_scaled => |*m| {
            if (count < allocs.len) {
                m.rn = applyAlloc(m.rn, allocs[count]);
                count += 1;
            }
            if (count < allocs.len) {
                m.rm = applyAlloc(m.rm, allocs[count]);
                count += 1;
            }
        },
        .reg_scaled_extended => |*m| {
            if (count < allocs.len) {
                m.rn = applyAlloc(m.rn, allocs[count]);
                count += 1;
            }
            if (count < allocs.len) {
                m.rm = applyAlloc(m.rm, allocs[count]);
                count += 1;
            }
        },
        .reg_extended => |*m| {
            if (count < allocs.len) {
                m.rn = applyAlloc(m.rn, allocs[count]);
                count += 1;
            }
            if (count < allocs.len) {
                m.rm = applyAlloc(m.rm, allocs[count]);
                count += 1;
            }
        },
        .reg_offset => |*m| {
            if (count < allocs.len) {
                m.rn = applyAlloc(m.rn, allocs[count]);
                count += 1;
            }
        },
        // These modes don't use allocatable registers
        .sp_offset, .fp_offset, .slot_offset, .incoming_arg,
        .sp_pre_indexed, .sp_post_indexed, .label, .constant => {},
    }
    return count;
}

/// Apply allocations to a pair addressing mode. Returns number of allocations consumed.
fn applyAllocPairAMode(mem: *PairAMode, allocs: []const Allocation) usize {
    var count: usize = 0;
    switch (mem.*) {
        .signed_offset => |*m| {
            if (count < allocs.len) {
                m.reg = applyAlloc(m.reg, allocs[count]);
                count += 1;
            }
        },
        // SP modes don't use allocatable registers
        .sp_pre_indexed, .sp_post_indexed => {},
    }
    return count;
}

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
