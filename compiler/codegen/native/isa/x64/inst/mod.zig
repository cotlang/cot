//! x86-64 ISA definitions: machine instruction types.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/x64/inst/mod.rs`

const std = @import("std");
const Allocator = std.mem.Allocator;

// Re-export sub-modules
pub const regs = @import("regs.zig");
pub const args = @import("args.zig");
pub const emit = @import("emit.zig");
pub const get_operands = @import("get_operands.zig");

// Re-export commonly used types from args
pub const Reg = args.Reg;
pub const PReg = args.PReg;
pub const VReg = args.VReg;
pub const RealReg = args.RealReg;
pub const RegClass = args.RegClass;
pub const MachLabel = args.MachLabel;
pub const Writable = args.Writable;
pub const Gpr = args.Gpr;
pub const Xmm = args.Xmm;
pub const WritableGpr = args.WritableGpr;
pub const WritableXmm = args.WritableXmm;
pub const OperandSize = args.OperandSize;
pub const CC = args.CC;
pub const FcmpImm = args.FcmpImm;
pub const RoundImm = args.RoundImm;
pub const ExtMode = args.ExtMode;
pub const ExtKind = args.ExtKind;
pub const ShiftKind = args.ShiftKind;
pub const Amode = args.Amode;
pub const SyntheticAmode = args.SyntheticAmode;
pub const RegMem = args.RegMem;
pub const RegMemImm = args.RegMemImm;
pub const GprMem = args.GprMem;
pub const GprMemImm = args.GprMemImm;
pub const XmmMem = args.XmmMem;
pub const XmmMemImm = args.XmmMemImm;
pub const MemFlags = args.MemFlags;
pub const Type = args.Type;
pub const CallArgPair = args.CallArgPair;
pub const CallArgList = args.CallArgList;

// Re-export register functions
pub const rax = regs.rax;
pub const rcx = regs.rcx;
pub const rdx = regs.rdx;
pub const rbx = regs.rbx;
pub const rsp = regs.rsp;
pub const rbp = regs.rbp;
pub const rsi = regs.rsi;
pub const rdi = regs.rdi;
pub const r8 = regs.r8;
pub const r9 = regs.r9;
pub const r10 = regs.r10;
pub const r11 = regs.r11;
pub const r12 = regs.r12;
pub const r13 = regs.r13;
pub const r14 = regs.r14;
pub const r15 = regs.r15;
pub const xmm0 = regs.xmm0;
pub const xmm1 = regs.xmm1;
pub const pinnedReg = regs.pinnedReg;
pub const prettyPrintReg = regs.prettyPrintReg;

//=============================================================================
// Trap codes
//=============================================================================

pub const TrapCode = enum(u16) {
    stack_overflow = 0,
    heap_out_of_bounds = 1,
    heap_misaligned = 2,
    table_out_of_bounds = 3,
    indirect_call_to_null = 4,
    bad_signature = 5,
    integer_overflow = 6,
    integer_division_by_zero = 7,
    bad_conversion_to_integer = 8,
    unreachable_code_reached = 9,
    interrupt = 10,
    user0 = 11,
    user1 = 12,
    always = 13,
    null_reference = 14,
    null_i31_ref = 15,
    array_out_of_bounds = 16,

    pub const INTEGER_DIVISION_BY_ZERO = TrapCode.integer_division_by_zero;
    pub const INTEGER_OVERFLOW = TrapCode.integer_overflow;
    pub const BAD_CONVERSION_TO_INTEGER = TrapCode.bad_conversion_to_integer;
    pub const STACK_OVERFLOW = TrapCode.stack_overflow;
    pub const HEAP_OUT_OF_BOUNDS = TrapCode.heap_out_of_bounds;
    pub const UNREACHABLE = TrapCode.unreachable_code_reached;
};

//=============================================================================
// External name (for calls)
//=============================================================================

pub const ExternalName = union(enum) {
    /// A user-defined function, identified by index.
    user: struct {
        namespace: u32,
        index: u32,
    },
    /// A library call.
    lib_call: LibCall,
    /// A known symbol.
    known_symbol: KnownSymbol,

    pub const LibCall = enum {
        probestack,
        floor_f32,
        floor_f64,
        ceil_f32,
        ceil_f64,
        trunc_f32,
        trunc_f64,
        nearest_f32,
        nearest_f64,
        fma_f32,
        fma_f64,
        memcpy,
        memset,
        memmove,
        memcmp,
    };

    pub const KnownSymbol = enum {
        elf_global_offset_table,
        plt_got,
    };
};

//=============================================================================
// Call info structures
//=============================================================================

/// Information about a function call.
pub const CallInfo = struct {
    /// Where this call is going.
    dest: ExternalName,
    /// Arguments to the call instruction.
    uses: CallArgList,
    /// Return values from the call.
    defs: CallRetList,
    /// Registers clobbered by the call.
    clobbers: PRegSet,
    /// Calling convention.
    callee_conv: CallConv,
    /// Caller's calling convention.
    caller_conv: CallConv,
    /// Opcode for the call (used for calling convention variants).
    opcode: ?CallOpcode,
    /// Try-call info for exception handling.
    try_call_info: ?TryCallInfo,
};

/// Information about a function call with an unknown target.
pub const CallInfoUnknown = struct {
    /// The target of the call (register or memory).
    dest: RegMem,
    /// Arguments to the call instruction.
    uses: CallArgList,
    /// Return values from the call.
    defs: CallRetList,
    /// Registers clobbered by the call.
    clobbers: PRegSet,
    /// Calling convention.
    callee_conv: CallConv,
    /// Caller's calling convention.
    caller_conv: CallConv,
    /// Opcode for the call.
    opcode: ?CallOpcode,
    /// Try-call info for exception handling.
    try_call_info: ?TryCallInfo,
};

/// Information about a return-call (tail call).
pub const ReturnCallInfo = struct {
    /// Where this call is going.
    dest: ExternalName,
    /// The size of the new stack frame's stack arguments.
    new_stack_arg_size: u32,
    /// Arguments to the call instruction.
    uses: CallArgList,
    /// A temporary register for moving the return address.
    tmp: WritableGpr,
};

/// Information about a return-call with an unknown target.
pub const ReturnCallInfoUnknown = struct {
    /// The target register.
    dest: Reg,
    /// The size of the new stack frame's stack arguments.
    new_stack_arg_size: u32,
    /// Arguments to the call instruction.
    uses: CallArgList,
    /// A temporary register for moving the return address.
    tmp: WritableGpr,
};

/// Return value location.
pub const RetLocation = union(enum) {
    /// Return value in a register.
    reg: PReg,
    /// Return value on the stack.
    stack: struct {
        offset: i32,
        size: u32,
    },
};

/// Return value pair.
pub const CallRetPair = struct {
    vreg: Writable(Reg),
    location: RetLocation,
};

pub const CallRetList = std.ArrayListUnmanaged(CallRetPair);

/// Try-call info for exception handling.
pub const TryCallInfo = struct {
    /// Continuation label on success.
    continuation: MachLabel,
    /// Exception destinations.
    exception_dests: []const ExceptionDest,
};

pub const ExceptionDest = struct {
    tag: u32,
    label: MachLabel,
};

/// Calling conventions.
pub const CallConv = enum {
    fast,
    cold,
    system_v,
    windows_fastcall,
    apple_aarch64,
    probestack,
    wasm,
    winch,
    tail,
};

/// Call opcode variants.
pub const CallOpcode = enum {
    call,
    call_indirect,
};

/// Physical register set (for clobbers).
pub const PRegSet = struct {
    bits: u64 = 0,

    pub const empty = PRegSet{};

    pub fn add(self: PRegSet, preg: PReg) PRegSet {
        return PRegSet{ .bits = self.bits | (@as(u64, 1) << @intCast(preg.index())) };
    }

    pub fn contains(self: PRegSet, preg: PReg) bool {
        return (self.bits & (@as(u64, 1) << @intCast(preg.index()))) != 0;
    }
};

//=============================================================================
// Atomic operations
//=============================================================================

/// Atomic read-modify-write sequence operation.
pub const AtomicRmwSeqOp = enum {
    add,
    sub,
    @"and",
    @"or",
    xor,
    xchg,
    nand,
    umin,
    umax,
    smin,
    smax,
};

/// Atomic 128-bit read-modify-write operation.
pub const Atomic128RmwSeqOp = enum {
    add,
    sub,
    @"and",
    @"or",
    xor,
};

//=============================================================================
// Unwind info
//=============================================================================

/// Unwind information for stack unwinding.
pub const UnwindInst = union(enum) {
    /// Push a register to the stack.
    push_reg: struct {
        reg: Reg,
    },
    /// Save a register to a stack slot.
    save_reg: struct {
        reg: Reg,
        offset: i32,
    },
    /// Allocate stack space.
    stack_alloc: struct {
        size: u32,
    },
    /// Set up frame pointer.
    set_frame_pointer: struct {
        offset: u32,
    },
};

//=============================================================================
// x86-64 machine instruction
//=============================================================================

/// x86-64 machine instruction.
/// Note: Unlike ARM64, many x64 instructions are encoded via External which
/// wraps the cranelift-assembler-x64 types. In our Zig port, we define the
/// common instructions directly and implement encoding in emit.zig.
pub const Inst = union(enum) {
    //=========================================================================
    // Pseudo-instructions (expanded during emission)
    //=========================================================================

    /// Checked signed remainder sequence.
    /// Handles the special case where divisor is -1 (result is 0).
    checked_srem_seq: struct {
        size: OperandSize,
        divisor: Gpr,
        dividend_lo: Gpr,
        dividend_hi: Gpr,
        dst_quotient: WritableGpr,
        dst_remainder: WritableGpr,
    },

    /// Checked signed remainder sequence for 8-bit.
    checked_srem_seq8: struct {
        divisor: Gpr,
        dividend: Gpr,
        dst: WritableGpr,
    },

    /// XMM uninitialized value (for register allocation).
    xmm_uninitialized_value: struct {
        dst: WritableXmm,
    },

    /// GPR uninitialized value (for register allocation).
    gpr_uninitialized_value: struct {
        dst: WritableGpr,
    },

    /// XMM min/max sequence.
    /// Handles NaN propagation correctly for IEEE 754.
    xmm_min_max_seq: struct {
        size: OperandSize,
        is_min: bool,
        lhs: Xmm,
        rhs: Xmm,
        dst: WritableXmm,
    },

    /// Convert unsigned 64-bit integer to float sequence.
    cvt_uint64_to_float_seq: struct {
        src: Gpr,
        dst: WritableXmm,
        dst_size: OperandSize,
        tmp_gpr1: WritableGpr,
        tmp_gpr2: WritableGpr,
    },

    /// Convert float to signed integer sequence.
    cvt_float_to_sint_seq: struct {
        src: Xmm,
        dst: WritableGpr,
        src_size: OperandSize,
        dst_size: OperandSize,
        tmp_xmm: WritableXmm,
        tmp_gpr: WritableGpr,
        is_saturating: bool,
    },

    /// Convert float to unsigned integer sequence.
    cvt_float_to_uint_seq: struct {
        src: Xmm,
        dst: WritableGpr,
        src_size: OperandSize,
        dst_size: OperandSize,
        tmp_xmm: WritableXmm,
        tmp_xmm2: WritableXmm,
        tmp_gpr: WritableGpr,
        is_saturating: bool,
    },

    /// XMM conditional move sequence.
    xmm_cmove: struct {
        ty: Type,
        cc: CC,
        consequent: Xmm,
        alternative: Xmm,
        dst: WritableXmm,
    },

    /// Stack probe loop.
    stack_probe_loop: struct {
        tmp: WritableGpr,
        frame_size: u32,
        guard_size: u32,
    },

    //=========================================================================
    // Move instructions
    //=========================================================================

    /// Move from a physical register.
    mov_from_preg: struct {
        src: PReg,
        dst: WritableGpr,
    },

    /// Move to a physical register.
    mov_to_preg: struct {
        src: Gpr,
        dst: PReg,
    },

    //=========================================================================
    // ALU instructions
    //=========================================================================

    /// ALU operation: dst = dst op src (reg/mem/imm).
    /// Covers ADD, SUB, AND, OR, XOR, CMP, TEST.
    alu_rmi_r: struct {
        size: OperandSize,
        op: AluRmiROpcode,
        src: GprMemImm,
        dst: WritableGpr,
    },

    /// Unary ALU operation: dst = op src.
    /// Covers NOT, NEG, INC, DEC.
    unary_rm_r: struct {
        size: OperandSize,
        op: UnaryRmROpcode,
        src: GprMem,
        dst: WritableGpr,
    },

    /// Shift/rotate: dst = dst op cl/imm8.
    shift_r: struct {
        size: OperandSize,
        kind: ShiftKind,
        /// Shift amount: either an immediate or CL register.
        shift_by: ShiftBy,
        dst: WritableGpr,
    },

    /// Multiply (widening): (rdx, rax) = rax * src.
    mul: struct {
        size: OperandSize,
        signed: bool,
        src: GprMem,
    },

    /// Divide: (rax, rdx) = (rdx:rax) / src.
    div: struct {
        size: OperandSize,
        signed: bool,
        divisor: GprMem,
        dividend_lo: Gpr,
        dividend_hi: Gpr,
        dst_quotient: WritableGpr,
        dst_remainder: WritableGpr,
        trap_code: TrapCode,
    },

    /// Sign-extend AL/AX/EAX/RAX into AH/DX/EDX/RDX (for division).
    sign_extend_data: struct {
        size: OperandSize,
        src: Gpr,
        dst: WritableGpr,
    },

    //=========================================================================
    // Move instructions
    //=========================================================================

    /// Move immediate to register.
    imm: struct {
        dst_size: OperandSize,
        simm64: u64,
        dst: WritableGpr,
    },

    /// Move register to register.
    mov_r_r: struct {
        size: OperandSize,
        src: Gpr,
        dst: WritableGpr,
    },

    /// Move with zero extension.
    movzx_rm_r: struct {
        ext_mode: ExtMode,
        src: GprMem,
        dst: WritableGpr,
    },

    /// Move with sign extension.
    movsx_rm_r: struct {
        ext_mode: ExtMode,
        src: GprMem,
        dst: WritableGpr,
    },

    /// Move register to memory.
    mov_r_m: struct {
        size: OperandSize,
        src: Gpr,
        dst: SyntheticAmode,
    },

    /// Move memory to register.
    mov_m_r: struct {
        size: OperandSize,
        src: SyntheticAmode,
        dst: WritableGpr,
    },

    //=========================================================================
    // LEA instruction
    //=========================================================================

    /// Load effective address.
    lea: struct {
        size: OperandSize,
        src: SyntheticAmode,
        dst: WritableGpr,
    },

    //=========================================================================
    // Comparison instructions
    //=========================================================================

    /// Compare register/memory with immediate.
    cmp_rmi_r: struct {
        size: OperandSize,
        src: GprMemImm,
        dst: Gpr,
    },

    /// Test (AND without storing result).
    test_rmi_r: struct {
        size: OperandSize,
        src: GprMemImm,
        dst: Gpr,
    },

    /// Set byte based on condition.
    setcc: struct {
        cc: CC,
        dst: WritableGpr,
    },

    /// Conditional move.
    cmove: struct {
        size: OperandSize,
        cc: CC,
        src: GprMem,
        dst: WritableGpr,
    },

    //=========================================================================
    // Control flow
    //=========================================================================

    /// Unconditional jump to a known label.
    jmp_known: struct {
        dst: MachLabel,
    },

    /// Conditional jump.
    jmp_cond: struct {
        cc: CC,
        taken: MachLabel,
        not_taken: MachLabel,
    },

    /// Conditional jump with OR of two conditions.
    jmp_cond_or: struct {
        cc1: CC,
        cc2: CC,
        taken: MachLabel,
        not_taken: MachLabel,
    },

    /// Winch-style one-way conditional jump.
    winch_jmp_if: struct {
        cc: CC,
        taken: MachLabel,
    },

    /// Jump table sequence.
    jmp_table_seq: struct {
        idx: Reg,
        tmp1: Writable(Reg),
        tmp2: Writable(Reg),
        default: MachLabel,
        targets: []const MachLabel,
    },

    /// Indirect jump.
    jmp_unknown: struct {
        target: RegMem,
    },

    /// Return.
    ret: struct {
        /// Stack bytes to pop (for callee-cleanup calling conventions).
        stack_bytes_to_pop: u32,
    },

    //=========================================================================
    // Call instructions
    //=========================================================================

    /// Call to a known target.
    call_known: struct {
        info: *CallInfo,
    },

    /// Call to an unknown target.
    call_unknown: struct {
        info: *CallInfoUnknown,
    },

    /// Return-call (tail call) to a known target.
    return_call_known: struct {
        info: *ReturnCallInfo,
    },

    /// Return-call to an unknown target.
    return_call_unknown: struct {
        info: *ReturnCallInfoUnknown,
    },

    //=========================================================================
    // Trap instructions
    //=========================================================================

    /// Trap if condition is true.
    trap_if: struct {
        cc: CC,
        trap_code: TrapCode,
    },

    /// Trap if both conditions are true (cc1 AND cc2).
    trap_if_and: struct {
        cc1: CC,
        cc2: CC,
        trap_code: TrapCode,
    },

    /// Trap if either condition is true (cc1 OR cc2).
    trap_if_or: struct {
        cc1: CC,
        cc2: CC,
        trap_code: TrapCode,
    },

    /// Unconditional trap (UD2).
    ud2: struct {
        trap_code: TrapCode,
    },

    //=========================================================================
    // XMM (SSE/AVX) instructions
    //=========================================================================

    /// XMM binary operation (addss, subss, mulss, divss, etc.).
    xmm_rm_r: struct {
        op: SseOpcode,
        src: XmmMem,
        dst: WritableXmm,
    },

    /// XMM binary operation with three operands (dst = src1 op src2).
    xmm_rm_r_evex: struct {
        op: Avx512Opcode,
        src1: Xmm,
        src2: XmmMem,
        dst: WritableXmm,
    },

    /// XMM unary operation (sqrtss, cvtss2sd, etc.).
    xmm_unary_rm_r: struct {
        op: SseOpcode,
        src: XmmMem,
        dst: WritableXmm,
    },

    /// XMM move (movss, movsd, movaps, movdqa, etc.).
    xmm_mov_r_m: struct {
        op: SseOpcode,
        src: Xmm,
        dst: SyntheticAmode,
    },

    /// XMM load.
    xmm_mov_m_r: struct {
        op: SseOpcode,
        src: SyntheticAmode,
        dst: WritableXmm,
    },

    /// XMM compare.
    xmm_cmp_rm_r: struct {
        op: SseOpcode,
        src: XmmMem,
        dst: Xmm,
    },

    /// XMM to GPR move.
    xmm_to_gpr: struct {
        op: SseOpcode,
        src: Xmm,
        dst: WritableGpr,
        dst_size: OperandSize,
    },

    /// GPR to XMM move.
    gpr_to_xmm: struct {
        op: SseOpcode,
        src: GprMem,
        src_size: OperandSize,
        dst: WritableXmm,
    },

    /// XMM compare with immediate (cmpss, cmppd, etc.).
    xmm_cmp_imm: struct {
        op: SseOpcode,
        src: XmmMem,
        dst: WritableXmm,
        imm: FcmpImm,
    },

    /// XMM rounding (roundss, roundsd).
    xmm_round: struct {
        op: SseOpcode,
        src: XmmMem,
        dst: WritableXmm,
        imm: RoundImm,
    },

    //=========================================================================
    // Atomic instructions
    //=========================================================================

    /// Atomic read-modify-write sequence.
    atomic_rmw_seq: struct {
        ty: Type,
        op: AtomicRmwSeqOp,
        mem: SyntheticAmode,
        operand: Gpr,
        dst_old: WritableGpr,
        tmp: WritableGpr,
    },

    /// Atomic 128-bit read-modify-write sequence.
    atomic_128_rmw_seq: struct {
        op: Atomic128RmwSeqOp,
        mem: SyntheticAmode,
        operand_low: Gpr,
        operand_high: Gpr,
        temp_low: WritableGpr,
        temp_high: WritableGpr,
        dst_old_low: WritableGpr,
        dst_old_high: WritableGpr,
    },

    /// Atomic 128-bit exchange sequence.
    atomic_128_xchg_seq: struct {
        mem: SyntheticAmode,
        operand_low: Gpr,
        operand_high: Gpr,
        dst_old_low: WritableGpr,
        dst_old_high: WritableGpr,
    },

    /// Memory fence.
    fence: struct {
        kind: FenceKind,
    },

    //=========================================================================
    // TLS (Thread-Local Storage) instructions
    //=========================================================================

    /// ELF TLS get address.
    elf_tls_get_addr: struct {
        symbol: ExternalName,
        dst: WritableGpr,
    },

    /// Mach-O TLS get address.
    macho_tls_get_addr: struct {
        symbol: ExternalName,
        dst: WritableGpr,
    },

    /// COFF TLS get address.
    coff_tls_get_addr: struct {
        symbol: ExternalName,
        dst: WritableGpr,
        tmp: WritableGpr,
    },

    //=========================================================================
    // Miscellaneous
    //=========================================================================

    /// Load external name (for PIC).
    load_ext_name: struct {
        dst: WritableGpr,
        name: ExternalName,
        offset: i64,
    },

    /// Arguments pseudo-instruction.
    args: struct {
        args_list: []const CallArgPair,
    },

    /// Returns pseudo-instruction.
    rets: struct {
        rets_list: []const CallArgPair,
    },

    /// Stack switch (for fibers/coroutines).
    stack_switch_basic: struct {
        store_context_ptr: Gpr,
        load_context_ptr: Gpr,
        in_payload0: Gpr,
        out_payload0: WritableGpr,
    },

    /// Unwind pseudo-instruction.
    unwind: struct {
        inst: UnwindInst,
    },

    /// Dummy use (keeps register alive).
    dummy_use: struct {
        reg: Reg,
    },

    /// Label address (lea of a label).
    label_address: struct {
        dst: WritableGpr,
        label: MachLabel,
    },

    /// Sequence point (for debugging).
    sequence_point,

    /// NOP instruction.
    nop: struct {
        len: u8, // 1-9 bytes
    },

    //=========================================================================
    // Helper methods
    //=========================================================================

    /// Create a NOP of the given length (1-9 bytes).
    pub fn genNop(len: u8) Inst {
        std.debug.assert(len > 0 and len <= 9);
        return .{ .nop = .{ .len = len } };
    }

    /// Create an immediate load instruction.
    pub fn genImm(dst_size: OperandSize, simm64: u64, dst: Writable(Reg)) Inst {
        const gpr = WritableGpr{ .reg = Gpr.unwrapNew(dst.toReg()) };
        return .{
            .imm = .{
                .dst_size = dst_size,
                .simm64 = simm64,
                .dst = gpr,
            },
        };
    }

    /// Create a register-to-register move.
    pub fn genMove(dst: Writable(Reg), src: Reg, ty: Type) Inst {
        _ = ty;
        if (src.class() == .int) {
            return .{
                .mov_r_r = .{
                    .size = .size64,
                    .src = Gpr.unwrapNew(src),
                    .dst = WritableGpr{ .reg = Gpr.unwrapNew(dst.toReg()) },
                },
            };
        } else {
            // XMM move
            return .{
                .xmm_mov_m_r = .{
                    .op = .movaps,
                    .src = SyntheticAmode.real_amode(Amode.immReg(0, src)),
                    .dst = WritableXmm{ .reg = Xmm.unwrapNew(dst.toReg()) },
                },
            };
        }
    }

    /// Create an unconditional jump.
    pub fn genJump(target: MachLabel) Inst {
        return .{ .jmp_known = .{ .dst = target } };
    }

    /// Create a return instruction.
    pub fn genRet(stack_bytes_to_pop: u32) Inst {
        return .{ .ret = .{ .stack_bytes_to_pop = stack_bytes_to_pop } };
    }

    /// Create a load instruction.
    pub fn genLoad(ty: Type, addr: SyntheticAmode, dst: Writable(Reg), ext_kind: ExtKind) Inst {
        const size_bytes = ty.laneBytes();
        if (dst.toReg().class() == .int) {
            const ext_mode = ExtMode.fromBits(@intCast(size_bytes * 8), 64);
            if (ext_mode) |em| {
                return switch (ext_kind) {
                    .sign_extend => .{
                        .movsx_rm_r = .{
                            .ext_mode = em,
                            .src = GprMem{ .inner = RegMem.fromMem(addr) },
                            .dst = WritableGpr{ .reg = Gpr.unwrapNew(dst.toReg()) },
                        },
                    },
                    .zero_extend, .none => .{
                        .movzx_rm_r = .{
                            .ext_mode = em,
                            .src = GprMem{ .inner = RegMem.fromMem(addr) },
                            .dst = WritableGpr{ .reg = Gpr.unwrapNew(dst.toReg()) },
                        },
                    },
                };
            } else {
                // 64-bit load
                return .{
                    .mov_m_r = .{
                        .size = .size64,
                        .src = addr,
                        .dst = WritableGpr{ .reg = Gpr.unwrapNew(dst.toReg()) },
                    },
                };
            }
        } else {
            // XMM load
            const op: SseOpcode = if (ty.isFloat() and size_bytes == 4)
                .movss
            else if (ty.isFloat() and size_bytes == 8)
                .movsd
            else
                .movdqu;
            return .{
                .xmm_mov_m_r = .{
                    .op = op,
                    .src = addr,
                    .dst = WritableXmm{ .reg = Xmm.unwrapNew(dst.toReg()) },
                },
            };
        }
    }

    /// Create a store instruction.
    pub fn genStore(ty: Type, addr: SyntheticAmode, src: Reg) Inst {
        const size_bytes = ty.laneBytes();
        if (src.class() == .int) {
            return .{
                .mov_r_m = .{
                    .size = OperandSize.fromBytes(size_bytes),
                    .src = Gpr.unwrapNew(src),
                    .dst = addr,
                },
            };
        } else {
            // XMM store
            const op: SseOpcode = if (ty.isFloat() and size_bytes == 4)
                .movss
            else if (ty.isFloat() and size_bytes == 8)
                .movsd
            else
                .movdqu;
            return .{
                .xmm_mov_r_m = .{
                    .op = op,
                    .src = Xmm.unwrapNew(src),
                    .dst = addr,
                },
            };
        }
    }
};

//=============================================================================
// ALU opcodes
//=============================================================================

/// ALU operations with register/memory/immediate source.
pub const AluRmiROpcode = enum {
    add,
    sub,
    @"and",
    @"or",
    xor,
    /// Add with carry.
    adc,
    /// Subtract with borrow.
    sbb,

    pub fn name(self: AluRmiROpcode) []const u8 {
        return switch (self) {
            .add => "add",
            .sub => "sub",
            .@"and" => "and",
            .@"or" => "or",
            .xor => "xor",
            .adc => "adc",
            .sbb => "sbb",
        };
    }
};

/// Unary ALU operations.
pub const UnaryRmROpcode = enum {
    not,
    neg,
    inc,
    dec,
    /// Bit scan forward.
    bsf,
    /// Bit scan reverse.
    bsr,
    /// Count leading zeros (LZCNT).
    lzcnt,
    /// Count trailing zeros (TZCNT).
    tzcnt,
    /// Population count (POPCNT).
    popcnt,

    pub fn name(self: UnaryRmROpcode) []const u8 {
        return switch (self) {
            .not => "not",
            .neg => "neg",
            .inc => "inc",
            .dec => "dec",
            .bsf => "bsf",
            .bsr => "bsr",
            .lzcnt => "lzcnt",
            .tzcnt => "tzcnt",
            .popcnt => "popcnt",
        };
    }
};

/// Shift amount: either immediate or CL register.
pub const ShiftBy = union(enum) {
    imm: u8,
    cl,
};

/// Memory fence kinds.
pub const FenceKind = enum {
    /// MFENCE - full barrier.
    mfence,
    /// LFENCE - load barrier.
    lfence,
    /// SFENCE - store barrier.
    sfence,
};

//=============================================================================
// SSE/AVX opcodes
//=============================================================================

/// SSE/SSE2/SSE3/SSSE3/SSE4.1/SSE4.2 opcodes.
pub const SseOpcode = enum {
    // Moves
    movss,
    movsd,
    movaps,
    movapd,
    movups,
    movupd,
    movdqa,
    movdqu,
    movlps,
    movlpd,
    movhps,
    movhpd,

    // Arithmetic (scalar)
    addss,
    addsd,
    subss,
    subsd,
    mulss,
    mulsd,
    divss,
    divsd,
    sqrtss,
    sqrtsd,
    maxss,
    maxsd,
    minss,
    minsd,
    rcpss,
    rsqrtss,

    // Arithmetic (packed)
    addps,
    addpd,
    subps,
    subpd,
    mulps,
    mulpd,
    divps,
    divpd,
    sqrtps,
    sqrtpd,
    maxps,
    maxpd,
    minps,
    minpd,
    rcpps,
    rsqrtps,

    // Integer arithmetic
    paddb,
    paddw,
    paddd,
    paddq,
    psubb,
    psubw,
    psubd,
    psubq,
    pmullw,
    pmulld,
    pmulhw,
    pmulhuw,
    pmuludq,

    // Logical
    andps,
    andpd,
    andnps,
    andnpd,
    orps,
    orpd,
    xorps,
    xorpd,
    pand,
    pandn,
    por,
    pxor,

    // Comparison
    cmpss,
    cmpsd,
    cmpps,
    cmppd,
    ucomiss,
    ucomisd,
    comiss,
    comisd,
    pcmpeqb,
    pcmpeqw,
    pcmpeqd,
    pcmpeqq,
    pcmpgtb,
    pcmpgtw,
    pcmpgtd,
    pcmpgtq,

    // Conversion
    cvtss2sd,
    cvtsd2ss,
    cvtsi2ss,
    cvtsi2sd,
    cvtss2si,
    cvtsd2si,
    cvttss2si,
    cvttsd2si,
    cvtdq2ps,
    cvtdq2pd,
    cvtps2dq,
    cvtpd2dq,
    cvttps2dq,
    cvttpd2dq,

    // Shuffle/permute
    shufps,
    shufpd,
    pshufd,
    pshufhw,
    pshuflw,
    punpcklbw,
    punpcklwd,
    punpckldq,
    punpcklqdq,
    punpckhbw,
    punpckhwd,
    punpckhdq,
    punpckhqdq,
    pblendw,
    blendps,
    blendpd,
    pblendvb,
    blendvps,
    blendvpd,
    insertps,
    pinsrb,
    pinsrw,
    pinsrd,
    pinsrq,
    extractps,
    pextrb,
    pextrw,
    pextrd,
    pextrq,
    movmskps,
    movmskpd,
    pmovmskb,

    // Shifts
    psllw,
    pslld,
    psllq,
    psrlw,
    psrld,
    psrlq,
    psraw,
    psrad,

    // Horizontal operations
    haddps,
    haddpd,
    hsubps,
    hsubpd,
    phaddd,
    phaddw,
    phsubd,
    phsubw,

    // Rounding
    roundss,
    roundsd,
    roundps,
    roundpd,

    // Other
    ptest,
    pmaxsb,
    pmaxsw,
    pmaxsd,
    pmaxub,
    pmaxuw,
    pmaxud,
    pminsb,
    pminsw,
    pminsd,
    pminub,
    pminuw,
    pminud,
    pabsb,
    pabsw,
    pabsd,
    packuswb,
    packusdw,
    packsswb,
    packssdw,
    unpcklps,
    unpcklpd,
    unpckhps,
    unpckhpd,
};

/// AVX-512 opcodes (for EVEX-encoded instructions).
pub const Avx512Opcode = enum {
    vpaddb,
    vpaddw,
    vpaddd,
    vpaddq,
    vpsubb,
    vpsubw,
    vpsubd,
    vpsubq,
    vpmullw,
    vpmulld,
    vpand,
    vpor,
    vpxor,
    vpcmpeqb,
    vpcmpeqw,
    vpcmpeqd,
    vpcmpeqq,
};

//=============================================================================
// Tests
//=============================================================================

test "Inst.genNop" {
    const testing = std.testing;
    const nop1 = Inst.genNop(1);
    switch (nop1) {
        .nop => |n| try testing.expectEqual(@as(u8, 1), n.len),
        else => unreachable,
    }
}

test "AluRmiROpcode names" {
    const testing = std.testing;
    try testing.expectEqualStrings("add", AluRmiROpcode.add.name());
    try testing.expectEqualStrings("sub", AluRmiROpcode.sub.name());
    try testing.expectEqualStrings("and", AluRmiROpcode.@"and".name());
}

test "UnaryRmROpcode names" {
    const testing = std.testing;
    try testing.expectEqualStrings("not", UnaryRmROpcode.not.name());
    try testing.expectEqualStrings("neg", UnaryRmROpcode.neg.name());
}

test "TrapCode values" {
    const testing = std.testing;
    try testing.expectEqual(@as(u16, 7), @intFromEnum(TrapCode.integer_division_by_zero));
    try testing.expectEqual(@as(u16, 0), @intFromEnum(TrapCode.stack_overflow));
}
