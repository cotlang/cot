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

// Import machinst types for Operand, PRegSet, and CallType
const machinst_reg = @import("../../../machinst/reg.zig");
const machinst_inst = @import("../../../machinst/inst.zig");
const CallType = machinst_inst.CallType;

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

// Import emit module for MachBuffer
const emit_mod = @import("emit.zig");

// Import common types from machinst (following Cranelift's crate::ir pattern)
const buffer_mod = @import("../../../machinst/buffer.zig");
// Import ExternalName from CLIF for call destinations (like ARM64 does)
const clif = @import("../../../../../ir/clif/mod.zig");
pub const ExternalName = clif.ExternalName;
// Also keep buffer's types for relocation emission
pub const BufferExternalName = buffer_mod.ExternalName;
pub const UserExternalNameRef = buffer_mod.UserExternalNameRef;
pub const LibCall = buffer_mod.LibCall;
pub const KnownSymbol = buffer_mod.KnownSymbol;
pub const Reloc = buffer_mod.Reloc;

// Import regalloc types
const regalloc_operand = @import("../../../regalloc/operand.zig");
const Allocation = regalloc_operand.Allocation;

// Import ABI module for prologue/epilogue generation
const abi = @import("../abi.zig");

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
    /// Bytes of stack space needed for stack arguments.
    /// Port of Cranelift's sized_stack_arg_space pattern.
    stack_args_size: u32 = 0,
    /// Calling convention.
    callee_conv: CallConv,
    /// Caller's calling convention.
    caller_conv: CallConv,
    /// Opcode for the call (used for calling convention variants).
    opcode: ?CallOpcode,
    /// Try-call info for exception handling.
    try_call_info: ?TryCallInfo,

    pub fn deinit(self: *CallInfo, allocator: std.mem.Allocator) void {
        self.uses.deinit(allocator);
        self.defs.deinit(allocator);
    }
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

    pub fn deinit(self: *CallInfoUnknown, allocator: std.mem.Allocator) void {
        self.uses.deinit(allocator);
        self.defs.deinit(allocator);
    }
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
/// Import from machinst_reg to support full 256-bit register space (all classes).
pub const PRegSet = machinst_reg.PRegSet;

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
    /// ISA identifier for compile-time ISA detection in generic code (e.g. VCode).
    pub const is_x64 = true;

    // Reference to the get_operands module for register allocation.
    // This allows generic code to access I.get_operands.getOperands() and I.get_operands.OperandVisitor.
    pub const get_operands = @import("get_operands.zig");

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
    /// ALU operation: dst = src1 OP src2.
    /// 3-operand form matching Cranelift's AluRmiR.
    /// Emit code handles `mov src1, dst` then `OP src2, dst`.
    alu_rmi_r: struct {
        size: OperandSize,
        op: AluRmiROpcode,
        src1: Gpr,
        src2: GprMemImm,
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
    /// When shift_by == .cl, src holds the shift amount vreg constrained to RCX.
    /// Port of Cranelift's ShiftR with num_shift_uses pattern.
    shift_r: struct {
        size: OperandSize,
        kind: ShiftKind,
        /// Shift amount: either an immediate or CL register.
        shift_by: ShiftBy,
        dst: WritableGpr,
        /// Shift amount source vreg (used when shift_by == .cl, constrained to RCX).
        src: Gpr = Gpr.unwrapNew(regs.rcx()),
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

    //=========================================================================
    // Stack operations
    //=========================================================================

    /// Push register to stack (decrements rsp by 8, then writes).
    push: struct {
        src: Gpr,
    },

    /// Pop from stack to register (reads, then increments rsp by 8).
    pop: struct {
        dst: WritableGpr,
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

    /// Return value copy: moves a vreg into a fixed physical return register.
    /// Uses regFixedDef (which regalloc handles correctly) instead of
    /// regFixedUse (which has a bug with long live ranges).
    /// Emits as: mov src, dst (where dst is the fixed ABI return register).
    ret_value_copy: struct {
        src: Gpr,
        dst: WritableGpr,
        preg: PReg,
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

    /// Get the canonical type for a register class.
    /// This is used for spill/reload operations where we need a type
    /// that can hold any value in that register class.
    /// Ported from Cranelift's canonical_type_for_rc.
    pub fn canonicalTypeForRc(rc: RegClass) Type {
        return switch (rc) {
            .int => Type.I64,
            .float => Type.F64,
            .vector => Type.I8X16,
        };
    }

    /// Generate a load from a stack slot.
    /// Used for spill reloads during register allocation.
    pub fn genLoadStack(into_reg: Writable(Reg), slot_offset: i64, ty: Type) Inst {
        const addr = SyntheticAmode.slotOffset(@intCast(slot_offset));
        return genLoad(ty, addr, into_reg, .none);
    }

    /// Generate a store to a stack slot.
    /// Used for spills during register allocation.
    pub fn genStoreStack(from_reg: Reg, slot_offset: i64, ty: Type) Inst {
        const addr = SyntheticAmode.slotOffset(@intCast(slot_offset));
        return genStore(ty, addr, from_reg);
    }

    /// LabelUse type for this instruction type (for MachBuffer parameterization).
    /// Matches Cranelift's pattern where VCodeInst trait has associated LabelUse type.
    pub const LabelUse = emit_mod.X64LabelUse;

    /// EmitInfo type for this instruction type.
    pub const EmitInfo = emit_mod.EmitInfo;

    /// Check if this instruction is a low-level branch.
    /// Low-level branches are typically expanded during emission.
    pub fn isLowLevelBranch(self: Inst) bool {
        _ = self;
        return false;
    }

    /// Machine terminator types.
    pub const MachTerminator = enum {
        /// Not a terminator.
        none,
        /// A return instruction.
        ret,
        /// A tail call instruction (returns from callee).
        ret_call,
        /// A branch instruction.
        branch,
    };

    /// Classify the type of call instruction this is.
    /// Port of Cranelift's call_type() from x64/inst/mod.rs.
    /// Returns CallType::Regular for call instructions, CallType::None otherwise.
    pub fn callType(self: *const Inst) CallType {
        return switch (self.*) {
            .call_known, .call_unknown => CallType.Regular,
            // TODO: Add return_call_known, return_call_unknown when tail calls needed
            else => CallType.None,
        };
    }

    /// Check if this instruction is an unconditional trap.
    pub fn isTrap(self: Inst) bool {
        return switch (self) {
            .ud2 => true,
            else => false,
        };
    }

    /// Check if this instruction is a terminator and what kind.
    /// Ported from cranelift/codegen/src/isa/x64/inst/mod.rs is_term()
    pub fn isTerm(self: Inst) MachTerminator {
        return switch (self) {
            .ret, .rets => .ret,
            .return_call_known, .return_call_unknown => .ret_call,
            .jmp_known, .jmp_cond, .jmp_cond_or, .jmp_table_seq, .jmp_unknown => .branch,
            else => .none,
        };
    }

    /// Check if this instruction is a return (rets pseudo-instruction).
    /// Used by emit to insert epilogue before returns.
    pub fn isRets(self: Inst) bool {
        return switch (self) {
            .rets => true,
            else => false,
        };
    }

    /// Get the operands for register allocation.
    pub fn getOperands(_: Inst) []const machinst_reg.Operand {
        return &[_]machinst_reg.Operand{};
    }

    /// Get the clobbered registers.
    pub fn getClobbers(_: Inst) machinst_reg.PRegSet {
        return machinst_reg.PRegSet.empty();
    }

    /// Generate an Args instruction for function parameters.
    /// Port of Cranelift's gen_args from abi.rs.
    /// This is a pseudo-instruction that tells regalloc which vregs are
    /// defined by function arguments in which registers. Emits nothing.
    pub fn genArgs(args_pairs: []const CallArgPair) Inst {
        return .{ .args = .{ .args_list = args_pairs } };
    }

    /// Generate a Rets instruction for return values.
    /// Port of Cranelift's gen_rets from abi.rs.
    pub fn genRets(ret_pairs: []const CallArgPair) Inst {
        return .{ .rets = .{ .rets_list = ret_pairs } };
    }

    /// Create a return instruction.
    pub fn genRet(stack_bytes_to_pop: u32) Inst {
        return .{ .ret = .{ .stack_bytes_to_pop = stack_bytes_to_pop } };
    }

    /// Create a load instruction.
    pub fn genLoad(ty: Type, addr: SyntheticAmode, dst: Writable(Reg), ext_kind: ExtKind) Inst {
        const size_bytes = ty.bytes();
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
        const size_bytes = ty.bytes();
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

    //=========================================================================
    // Emit with allocations
    //=========================================================================

    /// Emit this instruction with register allocations applied.
    /// Takes the allocations for this instruction's operands (in the order
    /// they were collected by getOperands: defs first, then uses) and emits
    /// the instruction with virtual registers replaced by physical registers.
    pub fn emitWithAllocs(
        self: *const Inst,
        sink: *emit_mod.MachBuffer,
        allocs: []const Allocation,
        emit_info: *const emit_mod.EmitInfo,
        state: *const emit_mod.EmitState,
    ) !void {
        // Create a mutable copy of the instruction
        var inst_copy = self.*;

        // Apply allocations via the visitor callback pattern.
        // Reference: wasmtime/cranelift/codegen/src/machinst/vcode.rs:1017-1035
        const AllocApplyCtx = struct {
            allocs: []const Allocation,
            idx: usize,

            fn callback(
                ctx_ptr: *anyopaque,
                reg: *Reg,
                constraint: Inst.get_operands.OperandConstraint,
                _: Inst.get_operands.OperandKind,
                _: Inst.get_operands.OperandPos,
            ) void {
                const ctx: *@This() = @ptrCast(@alignCast(ctx_ptr));

                // For reuse constraints, the same register is visited twice (def and use).
                // Both visits correspond to allocations in collectOperands, so we must
                // consume an allocation for each, even if the register is now physical
                // (converted by the previous callback on the same register).
                //
                // For non-reuse constraints, only virtual registers have allocations.
                // Physical registers (that were never virtual) don't consume allocations.
                const should_consume = reg.isVirtual() or (constraint == .reuse);
                if (!should_consume) return;

                if (ctx.idx >= ctx.allocs.len) return;

                const alloc = ctx.allocs[ctx.idx];
                ctx.idx += 1;

                // Only apply allocation if register is still virtual.
                // For reuse patterns, the first callback converts the register,
                // and the second callback just consumes (without re-applying).
                if (!reg.isVirtual()) return;

                if (alloc.asReg()) |regalloc_preg| {
                    // Convert regalloc PReg to machinst PReg
                    const machinst_preg = machinst_reg.PReg.init(
                        @intCast(regalloc_preg.hwEnc()),
                        switch (regalloc_preg.class()) {
                            .int => machinst_reg.RegClass.int,
                            .float => machinst_reg.RegClass.float,
                            .vector => machinst_reg.RegClass.vector,
                        },
                    );
                    reg.* = Reg.fromPReg(machinst_preg);
                }
                // Note: stack allocations would need special handling for spills,
                // but regalloc inserts explicit load/store instructions for those.
            }
        };

        var ctx = AllocApplyCtx{ .allocs = allocs, .idx = 0 };
        var visitor = Inst.get_operands.OperandVisitor.initCallback(@ptrCast(&ctx), AllocApplyCtx.callback);

        // Apply allocations by visiting all operands
        Inst.get_operands.getOperands(&inst_copy, &visitor);

        // Emit the instruction with the shared EmitState (contains frame_layout).
        // Port of Cranelift: state is shared across all emissions in a function.
        try emit_mod.emit(&inst_copy, sink, emit_info, state);
    }


    //=========================================================================
    // Debug printing (print_with_state)
    //=========================================================================

    /// Print instruction to a writer for debugging.
    /// This is the x64 equivalent of Cranelift's print_with_state.
    pub fn printWithState(self: Inst, writer: anytype) !void {
        switch (self) {
            // Pseudo-instructions
            .checked_srem_seq => |p| {
                try writer.print("checked_srem_seq {s}, {s}, {s}", .{
                    regs.prettyPrintReg(p.divisor.toReg(), p.size.bytes()),
                    regs.prettyPrintReg(p.dividend_lo.toReg(), p.size.bytes()),
                    regs.prettyPrintReg(p.dividend_hi.toReg(), p.size.bytes()),
                });
            },
            .checked_srem_seq8 => |p| {
                try writer.print("checked_srem_seq8 {s}, {s}", .{
                    regs.prettyPrintReg(p.divisor.toReg(), 1),
                    regs.prettyPrintReg(p.dividend.toReg(), 1),
                });
            },
            .xmm_uninitialized_value => |p| {
                try writer.print("xmm_uninit -> {s}", .{
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .gpr_uninitialized_value => |p| {
                try writer.print("gpr_uninit -> {s}", .{
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },
            .xmm_min_max_seq => |p| {
                const op_name = if (p.is_min) "xmm_min_seq" else "xmm_max_seq";
                try writer.print("{s} {s}, {s}, {s} -> {s}", .{
                    op_name,
                    p.size.name(),
                    regs.prettyPrintReg(p.lhs.toReg(), 16),
                    regs.prettyPrintReg(p.rhs.toReg(), 16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .cvt_uint64_to_float_seq => |p| {
                try writer.print("cvt_u64_to_f{d} {s} -> {s}", .{
                    p.dst_size.bits(),
                    regs.prettyPrintReg(p.src.toReg(), 8),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .cvt_float_to_sint_seq => |p| {
                try writer.print("cvt_f{d}_to_s{d} {s} -> {s}", .{
                    p.src_size.bits(),
                    p.dst_size.bits(),
                    regs.prettyPrintReg(p.src.toReg(), 16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.dst_size.bytes()),
                });
            },
            .cvt_float_to_uint_seq => |p| {
                try writer.print("cvt_f{d}_to_u{d} {s} -> {s}", .{
                    p.src_size.bits(),
                    p.dst_size.bits(),
                    regs.prettyPrintReg(p.src.toReg(), 16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.dst_size.bytes()),
                });
            },
            .xmm_cmove => |p| {
                try writer.print("xmm_cmov{s} {s}, {s} -> {s}", .{
                    p.cc.name(),
                    regs.prettyPrintReg(p.consequent.toReg(), 16),
                    regs.prettyPrintReg(p.alternative.toReg(), 16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .stack_probe_loop => |p| {
                try writer.print("stack_probe_loop frame_size={d}, guard_size={d}", .{
                    p.frame_size,
                    p.guard_size,
                });
            },

            // Move instructions
            .mov_from_preg => |p| {
                try writer.print("mov {s} -> {s}", .{
                    prettyPrintPReg(p.src),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },
            .mov_to_preg => |p| {
                try writer.print("mov {s} -> {s}", .{
                    regs.prettyPrintReg(p.src.toReg(), 8),
                    prettyPrintPReg(p.dst),
                });
            },

            // ALU instructions
            .alu_rmi_r => |p| {
                try writer.print("{s}{s} {s}, {s} -> {s}", .{
                    p.op.name(),
                    p.size.suffix(),
                    regs.prettyPrintReg(p.src1.toReg(), p.size.bytes()),
                    p.src2.inner.prettyPrint(p.size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.size.bytes()),
                });
            },
            .unary_rm_r => |p| {
                try writer.print("{s}{s} {s} -> {s}", .{
                    p.op.name(),
                    p.size.suffix(),
                    p.src.inner.prettyPrint(p.size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.size.bytes()),
                });
            },
            .shift_r => |p| {
                const shift_str = switch (p.shift_by) {
                    .imm => |i| blk: {
                        var buf: [8]u8 = undefined;
                        break :blk std.fmt.bufPrint(&buf, "${d}", .{i}) catch "?";
                    },
                    .cl => "cl",
                };
                try writer.print("{s}{s} {s}, {s}", .{
                    p.kind.name(),
                    p.size.suffix(),
                    shift_str,
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.size.bytes()),
                });
            },
            .mul => |p| {
                const op_name = if (p.signed) "imul" else "mul";
                try writer.print("{s}{s} {s}", .{
                    op_name,
                    p.size.suffix(),
                    p.src.inner.prettyPrint(p.size.bytes()),
                });
            },
            .div => |p| {
                const op_name = if (p.signed) "idiv" else "div";
                try writer.print("{s}{s} {s}", .{
                    op_name,
                    p.size.suffix(),
                    p.divisor.inner.prettyPrint(p.size.bytes()),
                });
            },
            .sign_extend_data => |p| {
                const op_name: []const u8 = switch (p.size) {
                    .size8 => "cbw",
                    .size16 => "cwd",
                    .size32 => "cdq",
                    .size64 => "cqo",
                };
                try writer.print("{s}", .{op_name});
            },

            // Move instructions
            .imm => |p| {
                if (p.simm64 <= 0xFFFFFFFF) {
                    try writer.print("mov{s} ${d}, {s}", .{
                        p.dst_size.suffix(),
                        p.simm64,
                        regs.prettyPrintReg(p.dst.toReg().toReg(), p.dst_size.bytes()),
                    });
                } else {
                    try writer.print("movabs ${x}, {s}", .{
                        p.simm64,
                        regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                    });
                }
            },
            .mov_r_r => |p| {
                try writer.print("mov{s} {s}, {s}", .{
                    p.size.suffix(),
                    regs.prettyPrintReg(p.src.toReg(), p.size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.size.bytes()),
                });
            },
            .movzx_rm_r => |p| {
                try writer.print("movzx {s}, {s}", .{
                    p.src.inner.prettyPrint(p.ext_mode.srcBytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.ext_mode.dstBytes()),
                });
            },
            .movsx_rm_r => |p| {
                try writer.print("movsx {s}, {s}", .{
                    p.src.inner.prettyPrint(p.ext_mode.srcBytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.ext_mode.dstBytes()),
                });
            },
            .mov_r_m => |p| {
                try writer.print("mov{s} {s}, {s}", .{
                    p.size.suffix(),
                    regs.prettyPrintReg(p.src.toReg(), p.size.bytes()),
                    p.dst.prettyPrint(p.size.bytes()),
                });
            },
            .mov_m_r => |p| {
                try writer.print("mov{s} {s}, {s}", .{
                    p.size.suffix(),
                    p.src.prettyPrint(p.size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.size.bytes()),
                });
            },

            // LEA
            .lea => |p| {
                try writer.print("lea{s} {s}, {s}", .{
                    p.size.suffix(),
                    p.src.prettyPrint(8),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.size.bytes()),
                });
            },

            // Comparison
            .cmp_rmi_r => |p| {
                try writer.print("cmp{s} {s}, {s}", .{
                    p.size.suffix(),
                    p.src.inner.prettyPrint(p.size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg(), p.size.bytes()),
                });
            },
            .test_rmi_r => |p| {
                try writer.print("test{s} {s}, {s}", .{
                    p.size.suffix(),
                    p.src.inner.prettyPrint(p.size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg(), p.size.bytes()),
                });
            },
            .setcc => |p| {
                try writer.print("set{s} {s}", .{
                    p.cc.name(),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 1),
                });
            },
            .cmove => |p| {
                try writer.print("cmov{s}{s} {s}, {s}", .{
                    p.cc.name(),
                    p.size.suffix(),
                    p.src.inner.prettyPrint(p.size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.size.bytes()),
                });
            },

            // Control flow
            .jmp_known => |p| {
                try writer.print("jmp label{d}", .{p.dst.index});
            },
            .jmp_cond => |p| {
                try writer.print("j{s} label{d} ; else label{d}", .{
                    p.cc.name(),
                    p.taken.index,
                    p.not_taken.index,
                });
            },
            .jmp_cond_or => |p| {
                try writer.print("j{s}|{s} label{d} ; else label{d}", .{
                    p.cc1.name(),
                    p.cc2.name(),
                    p.taken.index,
                    p.not_taken.index,
                });
            },
            .winch_jmp_if => |p| {
                try writer.print("j{s} label{d}", .{
                    p.cc.name(),
                    p.taken.index,
                });
            },
            .jmp_table_seq => |p| {
                try writer.print("jmp_table idx={s}, targets=[{d}], default=label{d}", .{
                    regs.prettyPrintReg(p.idx, 8),
                    p.targets.len,
                    p.default.index,
                });
            },
            .jmp_unknown => |p| {
                try writer.print("jmp *{s}", .{
                    p.target.prettyPrint(8),
                });
            },
            .ret => |p| {
                if (p.stack_bytes_to_pop > 0) {
                    try writer.print("ret ${d}", .{p.stack_bytes_to_pop});
                } else {
                    try writer.writeAll("ret");
                }
            },
            .push => |p| {
                try writer.print("push {s}", .{regs.prettyPrintReg(p.src.toReg(), 8)});
            },
            .pop => |p| {
                try writer.print("pop {s}", .{regs.prettyPrintReg(p.dst.toReg().toReg(), 8)});
            },

            // Calls
            .call_known => |_| {
                try writer.writeAll("call <known>");
            },
            .call_unknown => |_| {
                try writer.writeAll("call <unknown>");
            },
            .return_call_known => |_| {
                try writer.writeAll("tail_call <known>");
            },
            .return_call_unknown => |_| {
                try writer.writeAll("tail_call <unknown>");
            },

            // Traps
            .trap_if => |p| {
                try writer.print("trap_if {s} ; {s}", .{
                    p.cc.name(),
                    trapCodeName(p.trap_code),
                });
            },
            .trap_if_and => |p| {
                try writer.print("trap_if {s} && {s} ; {s}", .{
                    p.cc1.name(),
                    p.cc2.name(),
                    trapCodeName(p.trap_code),
                });
            },
            .trap_if_or => |p| {
                try writer.print("trap_if {s} || {s} ; {s}", .{
                    p.cc1.name(),
                    p.cc2.name(),
                    trapCodeName(p.trap_code),
                });
            },
            .ud2 => |p| {
                try writer.print("ud2 ; {s}", .{trapCodeName(p.trap_code)});
            },

            // XMM instructions
            .xmm_rm_r => |p| {
                try writer.print("{s} {s}, {s}", .{
                    sseOpcodeName(p.op),
                    p.src.inner.prettyPrint(16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .xmm_rm_r_evex => |p| {
                try writer.print("{s} {s}, {s}, {s}", .{
                    avx512OpcodeName(p.op),
                    regs.prettyPrintReg(p.src1.toReg(), 16),
                    p.src2.inner.prettyPrint(16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .xmm_unary_rm_r => |p| {
                try writer.print("{s} {s}, {s}", .{
                    sseOpcodeName(p.op),
                    p.src.inner.prettyPrint(16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .xmm_mov_r_m => |p| {
                try writer.print("{s} {s}, {s}", .{
                    sseOpcodeName(p.op),
                    regs.prettyPrintReg(p.src.toReg(), 16),
                    p.dst.prettyPrint(16),
                });
            },
            .xmm_mov_m_r => |p| {
                try writer.print("{s} {s}, {s}", .{
                    sseOpcodeName(p.op),
                    p.src.prettyPrint(16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .xmm_cmp_rm_r => |p| {
                try writer.print("{s} {s}, {s}", .{
                    sseOpcodeName(p.op),
                    p.src.inner.prettyPrint(16),
                    regs.prettyPrintReg(p.dst.toReg(), 16),
                });
            },
            .xmm_to_gpr => |p| {
                try writer.print("{s} {s}, {s}", .{
                    sseOpcodeName(p.op),
                    regs.prettyPrintReg(p.src.toReg(), 16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), p.dst_size.bytes()),
                });
            },
            .gpr_to_xmm => |p| {
                try writer.print("{s} {s}, {s}", .{
                    sseOpcodeName(p.op),
                    p.src.inner.prettyPrint(p.src_size.bytes()),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .xmm_cmp_imm => |p| {
                try writer.print("{s} ${d}, {s}, {s}", .{
                    sseOpcodeName(p.op),
                    @intFromEnum(p.imm),
                    p.src.inner.prettyPrint(16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },
            .xmm_round => |p| {
                try writer.print("{s} ${d}, {s}, {s}", .{
                    sseOpcodeName(p.op),
                    @intFromEnum(p.imm),
                    p.src.inner.prettyPrint(16),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 16),
                });
            },

            // Atomics
            .atomic_rmw_seq => |p| {
                const size: u8 = @intCast(p.ty.bytes());
                try writer.print("atomic_{s} {s}, {s} -> {s}", .{
                    atomicRmwOpName(p.op),
                    p.mem.prettyPrint(size),
                    regs.prettyPrintReg(p.operand.toReg(), size),
                    regs.prettyPrintReg(p.dst_old.toReg().toReg(), size),
                });
            },
            .atomic_128_rmw_seq => |p| {
                try writer.print("atomic128_{s} {s}", .{
                    atomic128RmwOpName(p.op),
                    p.mem.prettyPrint(16),
                });
            },
            .atomic_128_xchg_seq => |p| {
                try writer.print("atomic128_xchg {s}", .{
                    p.mem.prettyPrint(16),
                });
            },
            .fence => |p| {
                try writer.print("{s}", .{fenceKindName(p.kind)});
            },

            // TLS
            .elf_tls_get_addr => |p| {
                try writer.print("elf_tls_get_addr {s} -> {s}", .{
                    externalNameStr(p.symbol),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },
            .macho_tls_get_addr => |p| {
                try writer.print("macho_tls_get_addr {s} -> {s}", .{
                    externalNameStr(p.symbol),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },
            .coff_tls_get_addr => |p| {
                try writer.print("coff_tls_get_addr {s} -> {s}", .{
                    externalNameStr(p.symbol),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },

            // Miscellaneous
            .load_ext_name => |p| {
                try writer.print("lea {s}+{d}, {s}", .{
                    externalNameStr(p.name),
                    p.offset,
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },
            .args => |_| {
                try writer.writeAll("args <...>");
            },
            .rets => |_| {
                try writer.writeAll("rets <...>");
            },
            .ret_value_copy => |p| {
                try writer.print("ret_value_copy {s} -> {s}", .{
                    regs.prettyPrintReg(p.src.toReg(), 8),
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },
            .stack_switch_basic => |_| {
                try writer.writeAll("stack_switch_basic");
            },
            .unwind => |p| {
                switch (p.inst) {
                    .push_reg => |u| try writer.print("unwind: push {s}", .{
                        regs.prettyPrintReg(u.reg, 8),
                    }),
                    .save_reg => |u| try writer.print("unwind: save {s} at {d}", .{
                        regs.prettyPrintReg(u.reg, 8),
                        u.offset,
                    }),
                    .stack_alloc => |u| try writer.print("unwind: alloc {d}", .{u.size}),
                    .set_frame_pointer => |u| try writer.print("unwind: set_fp offset={d}", .{u.offset}),
                }
            },
            .dummy_use => |p| {
                try writer.print("dummy_use {s}", .{
                    regs.prettyPrintReg(p.reg, 8),
                });
            },
            .label_address => |p| {
                try writer.print("lea label{d}, {s}", .{
                    p.label.index,
                    regs.prettyPrintReg(p.dst.toReg().toReg(), 8),
                });
            },
            .sequence_point => {
                try writer.writeAll("sequence_point");
            },
            .nop => |p| {
                try writer.print("nop{d}", .{p.len});
            },
        }
    }

    /// Format instruction to a string (convenience wrapper).
    pub fn format(self: Inst, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try self.printWithState(writer);
    }

    /// Emit this instruction directly (with physical registers already in place).
    /// Used for prologue/epilogue and regalloc-inserted moves.
    /// Port of Cranelift: EmitState is passed through from vcode emission,
    /// carrying frame_layout needed for incoming_arg and slot_offset resolution.
    pub fn emit(
        self: *const Inst,
        sink: *emit_mod.MachBuffer,
        emit_info: *const emit_mod.EmitInfo,
        state: *const emit_mod.EmitState,
    ) !void {
        try emit_mod.emit(self, sink, emit_info, state);
    }

    /// EmitState type for this instruction type (used for emission context).
    pub const EmitState = emit_mod.EmitState;

    /// Deinitialize the instruction, freeing any owned memory.
    /// This must be called for instructions that own allocated slices:
    /// - args: allocated CallArgPair slice for function arguments
    /// - rets: allocated CallArgPair slice for return values
    /// - jmp_table_seq: allocated MachLabel slice for jump table targets
    /// - call_known: allocated CallInfo with uses/defs arrays
    /// - call_unknown: allocated CallInfoUnknown with uses/defs arrays
    pub fn deinit(self: *const Inst, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .rets => |r| allocator.free(r.rets_list),
            .args => |a| allocator.free(a.args_list),
            .jmp_table_seq => |j| allocator.free(j.targets),
            .call_known => |c| {
                // Free the CallInfo's internal arrays, then the CallInfo itself
                // Cast away const to call deinit (we own this memory)
                const info_ptr = @constCast(c.info);
                info_ptr.deinit(allocator);
                allocator.destroy(info_ptr);
            },
            .call_unknown => |c| {
                // Free the CallInfoUnknown's internal arrays, then the CallInfoUnknown itself
                // Cast away const to call deinit (we own this memory)
                const info_ptr = @constCast(c.info);
                info_ptr.deinit(allocator);
                allocator.destroy(info_ptr);
            },
            else => {},
        }
    }

    /// FrameLayout type used for prologue/epilogue generation.
    /// Uses a simple struct with the fields needed for prologue/epilogue.
    pub const FrameLayout = struct {
        setup_area_size: u32 = 0,
        clobber_size: u32 = 0,
        fixed_frame_storage_size: u32 = 0,
        outgoing_args_size: u32 = 0,
        stackslots_size: u32 = 0,
        /// Callee-saved registers that were used and need to be saved/restored.
        /// Contains GPR hardware encodings (e.g., 3=RBX, 12=R12, etc.)
        clobbered_callee_saves: [5]u8 = undefined,
        num_clobbered_callee_saves: usize = 0,
    };

    /// Generate prologue instructions for stack frame setup.
    /// Called at entry block to set up the stack frame.
    /// Returns a bounded array of instructions that should be emitted.
    ///
    /// Following Cranelift pattern from abi.rs gen_prologue():
    /// - Push rbp (save frame pointer)
    /// - mov rbp, rsp (establish frame)
    /// - Push callee-saved registers
    ///
    /// Reference: cranelift/codegen/src/isa/x64/abi.rs gen_prologue_frame_setup()
    pub fn genPrologue(frame_layout: *const FrameLayout) abi.BoundedArray(Inst, 16) {
        var insts = abi.BoundedArray(Inst, 16){};
        const setup_frame = frame_layout.setup_area_size > 0;

        if (setup_frame) {
            // push rbp
            insts.appendAssumeCapacity(.{
                .push = .{
                    .src = Gpr.unwrapNew(rbp()),
                },
            });

            // mov rbp, rsp
            insts.appendAssumeCapacity(.{
                .mov_r_r = .{
                    .size = .size64,
                    .src = Gpr.unwrapNew(rsp()),
                    .dst = WritableGpr.fromReg(Gpr.unwrapNew(rbp())),
                },
            });
        }

        // Push callee-saved registers that were used
        // These are pushed after rbp so they're below it on the stack
        for (0..frame_layout.num_clobbered_callee_saves) |i| {
            const reg_enc = frame_layout.clobbered_callee_saves[i];
            insts.appendAssumeCapacity(.{
                .push = .{
                    .src = Gpr.unwrapNew(regs.gpr(reg_enc)),
                },
            });
        }

        return insts;
    }

    /// Generate epilogue instructions for stack frame teardown.
    /// Called before return instructions to restore the stack frame.
    /// Returns a bounded array of instructions that should be emitted.
    ///
    /// Following Cranelift pattern from abi.rs gen_epilogue():
    /// - Pop callee-saved registers (reverse order)
    /// - mov rsp, rbp (restore stack pointer)
    /// - pop rbp (restore frame pointer)
    ///
    /// Reference: cranelift/codegen/src/isa/x64/abi.rs gen_epilogue_frame_restore()
    pub fn genEpilogue(frame_layout: *const FrameLayout) abi.BoundedArray(Inst, 16) {
        var insts = abi.BoundedArray(Inst, 16){};
        const setup_frame = frame_layout.setup_area_size > 0;

        // Pop callee-saved registers in reverse order
        var i: usize = frame_layout.num_clobbered_callee_saves;
        while (i > 0) {
            i -= 1;
            const reg_enc = frame_layout.clobbered_callee_saves[i];
            insts.appendAssumeCapacity(.{
                .pop = .{
                    .dst = WritableGpr.fromReg(Gpr.unwrapNew(regs.gpr(reg_enc))),
                },
            });
        }

        if (setup_frame) {
            // mov rsp, rbp
            insts.appendAssumeCapacity(.{
                .mov_r_r = .{
                    .size = .size64,
                    .src = Gpr.unwrapNew(rbp()),
                    .dst = WritableGpr.fromReg(Gpr.unwrapNew(rsp())),
                },
            });

            // pop rbp
            insts.appendAssumeCapacity(.{
                .pop = .{
                    .dst = WritableGpr.fromReg(Gpr.unwrapNew(rbp())),
                },
            });
        }

        return insts;
    }

    /// Generate stack allocation instruction.
    /// Called after prologue to allocate space for local variables.
    /// Returns sub rsp, size instruction or nop if size is 0.
    pub fn genStackAlloc(size: u32) Inst {
        if (size == 0) {
            return .{ .nop = .{ .len = 0 } };
        }
        // sub rsp, size
        const rsp_gpr = Gpr.unwrapNew(rsp());
        return .{
            .alu_rmi_r = .{
                .size = .size64,
                .op = .sub,
                .src1 = rsp_gpr,
                .src2 = GprMemImm{ .inner = .{ .imm = size } },
                .dst = WritableGpr.fromReg(rsp_gpr),
            },
        };
    }
};

//=============================================================================
// Helper functions for printing
//=============================================================================

fn prettyPrintPReg(preg: PReg) []const u8 {
    // Physical register names
    const enc = preg.index();
    if (preg.class() == .int) {
        return regs.prettyPrintReg(args.Reg.fromPReg(preg), 8);
    } else {
        return regs.prettyPrintReg(args.Reg.fromPReg(preg), 16);
    }
    _ = enc;
}

fn trapCodeName(tc: TrapCode) []const u8 {
    return switch (tc) {
        .stack_overflow => "stack_overflow",
        .heap_out_of_bounds => "heap_oob",
        .heap_misaligned => "heap_misaligned",
        .table_out_of_bounds => "table_oob",
        .indirect_call_to_null => "indirect_call_null",
        .bad_signature => "bad_signature",
        .integer_overflow => "int_overflow",
        .integer_division_by_zero => "int_divz",
        .bad_conversion_to_integer => "bad_int_conv",
        .unreachable_code_reached => "unreachable",
        .interrupt => "interrupt",
        .user0 => "user0",
        .user1 => "user1",
        .always => "always",
        .null_reference => "null_ref",
        .null_i31_ref => "null_i31",
        .array_out_of_bounds => "array_oob",
    };
}

fn sseOpcodeName(op: SseOpcode) []const u8 {
    return @tagName(op);
}

fn avx512OpcodeName(op: Avx512Opcode) []const u8 {
    return @tagName(op);
}

fn atomicRmwOpName(op: AtomicRmwSeqOp) []const u8 {
    return switch (op) {
        .add => "add",
        .sub => "sub",
        .@"and" => "and",
        .@"or" => "or",
        .xor => "xor",
        .xchg => "xchg",
        .nand => "nand",
        .umin => "umin",
        .umax => "umax",
        .smin => "smin",
        .smax => "smax",
    };
}

fn atomic128RmwOpName(op: Atomic128RmwSeqOp) []const u8 {
    return switch (op) {
        .add => "add",
        .sub => "sub",
        .@"and" => "and",
        .@"or" => "or",
        .xor => "xor",
    };
}

fn fenceKindName(kind: FenceKind) []const u8 {
    return switch (kind) {
        .mfence => "mfence",
        .lfence => "lfence",
        .sfence => "sfence",
    };
}

fn externalNameStr(name: ExternalName) []const u8 {
    _ = name;
    return "<extern>";
}

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
    /// Signed multiply (two-operand form: dst = dst * src).
    imul,

    pub fn name(self: AluRmiROpcode) []const u8 {
        return switch (self) {
            .add => "add",
            .sub => "sub",
            .@"and" => "and",
            .@"or" => "or",
            .xor => "xor",
            .adc => "adc",
            .sbb => "sbb",
            .imul => "imul",
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
    // GPR↔XMM moves
    movd, // 32-bit GPR to/from XMM
    movq, // 64-bit GPR to/from XMM

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

test "Inst.printWithState" {
    const testing = std.testing;
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    // Test NOP
    const nop = Inst.genNop(3);
    try nop.printWithState(writer);
    try testing.expectEqualStrings("nop3", fbs.getWritten());

    // Test ret
    fbs.reset();
    const ret = Inst{ .ret = .{ .stack_bytes_to_pop = 0 } };
    try ret.printWithState(writer);
    try testing.expectEqualStrings("ret", fbs.getWritten());

    // Test ret with pop
    fbs.reset();
    const ret_pop = Inst{ .ret = .{ .stack_bytes_to_pop = 16 } };
    try ret_pop.printWithState(writer);
    try testing.expectEqualStrings("ret $16", fbs.getWritten());

    // Test jmp_known
    fbs.reset();
    const jmp = Inst{ .jmp_known = .{ .dst = MachLabel{ .index = 5 } } };
    try jmp.printWithState(writer);
    try testing.expectEqualStrings("jmp label5", fbs.getWritten());

    // Test ud2
    fbs.reset();
    const ud2 = Inst{ .ud2 = .{ .trap_code = .unreachable_code_reached } };
    try ud2.printWithState(writer);
    try testing.expectEqualStrings("ud2 ; unreachable", fbs.getWritten());
}
