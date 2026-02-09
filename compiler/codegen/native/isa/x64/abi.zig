//! Implementation of x86-64 ABIs.
//!
//! Ported from Cranelift's `cranelift/codegen/src/isa/x64/abi.rs`
//!
//! This module implements the x86-64 Application Binary Interface (ABI),
//! which defines how functions call each other, pass arguments, return values,
//! and manage the call stack.
//!
//! Two main ABIs are supported:
//! - System V AMD64 ABI (Linux, macOS, BSD)
//! - Windows x64 calling convention

const std = @import("std");
const Allocator = std.mem.Allocator;

//=============================================================================
// BoundedArray implementation for Zig 0.15 compatibility

pub fn BoundedArray(comptime T: type, comptime capacity: usize) type {
    return struct {
        buffer: [capacity]T = undefined,
        len: usize = 0,

        const Self = @This();

        pub fn appendAssumeCapacity(self: *Self, item: T) void {
            std.debug.assert(self.len < capacity);
            self.buffer[self.len] = item;
            self.len += 1;
        }

        pub fn append(self: *Self, item: T) !void {
            if (self.len >= capacity) return error.OutOfMemory;
            self.appendAssumeCapacity(item);
        }

        pub fn slice(self: *Self) []T {
            return self.buffer[0..self.len];
        }

        pub fn constSlice(self: *const Self) []const T {
            return self.buffer[0..self.len];
        }

        pub fn get(self: *const Self, index: usize) T {
            std.debug.assert(index < self.len);
            return self.buffer[index];
        }
    };
}

// Import instruction module types
const inst = @import("inst/mod.zig");
const regs = inst.regs;
const args = inst.args;

// Re-export commonly used types
const Inst = inst.Inst;
const AluRmiROpcode = inst.AluRmiROpcode;
const UnaryRmROpcode = inst.UnaryRmROpcode;
const ShiftKind = args.ShiftKind;
const Amode = args.Amode;
const SyntheticAmode = args.SyntheticAmode;
const OperandSize = args.OperandSize;
const CC = args.CC;
const MachLabel = args.MachLabel;
const MemFlags = args.MemFlags;
const Type = args.Type;
const ExtMode = args.ExtMode;

// Register types
const Reg = args.Reg;
const PReg = args.PReg;
const VReg = args.VReg;
const RealReg = args.RealReg;
const RegClass = args.RegClass;
const Writable = args.Writable;
const Gpr = args.Gpr;
const WritableGpr = args.WritableGpr;
const Xmm = args.Xmm;
const WritableXmm = args.WritableXmm;

// Register constructors
const rax = regs.rax;
const rcx = regs.rcx;
const rdx = regs.rdx;
const rbx = regs.rbx;
const rsp = regs.rsp;
const rbp = regs.rbp;
const rsi = regs.rsi;
const rdi = regs.rdi;
const r8 = regs.r8;
const r9 = regs.r9;
const r10 = regs.r10;
const r11 = regs.r11;
const r12 = regs.r12;
const r13 = regs.r13;
const r14 = regs.r14;
const r15 = regs.r15;
const xmm0 = regs.xmm0;
const xmm1 = regs.xmm1;
const gprPreg = regs.gprPreg;
const fprPreg = regs.fprPreg;

const GprEnc = regs.GprEnc;
const XmmEnc = regs.XmmEnc;

//=============================================================================
// Calling convention definitions

/// Calling convention enum.
pub const CallConv = enum {
    /// System V AMD64 ABI (Linux, macOS, BSD).
    system_v,
    /// Windows x64 calling convention.
    windows_fastcall,
    /// Tail call convention.
    tail,
    /// Winch (WebAssembly interpreter) calling convention.
    winch,
    /// Probestack variant.
    probestack,
    /// Wasmtime variant of System V.
    wasmtime_system_v,
    /// Wasmtime variant of Windows fastcall.
    wasmtime_fastcall,
};

/// Arguments or return values?
pub const ArgsOrRets = enum {
    args,
    rets,
};

/// Whether a function is a leaf (calls no other functions) or not.
pub const FunctionCalls = enum {
    none,
    some,
};

/// Argument extension mode.
pub const ArgumentExtension = enum {
    none,
    uext,
    sext,
};

/// Argument purpose.
pub const ArgumentPurpose = enum {
    normal,
    struct_return,
    struct_argument,
    vmctx,
    sigid,
    callee_tlsbase,
    callee_vm_runtime_limits,
};

//=============================================================================
// ABI parameter types

/// An ABI parameter (argument or return value).
pub const AbiParam = struct {
    /// The value type.
    value_type: Type,
    /// Extension mode.
    extension: ArgumentExtension = .none,
    /// Purpose of this argument.
    purpose: ArgumentPurpose = .normal,
};

/// A slot in an ABI argument: either in a register or on the stack.
pub const ABIArgSlot = union(enum) {
    /// In a register.
    reg: struct {
        reg: RealReg,
        ty: Type,
        extension: ArgumentExtension,
    },
    /// On the stack.
    stack: struct {
        offset: i64,
        ty: Type,
        extension: ArgumentExtension,
    },
};

/// An ABI argument (collection of slots).
pub const ABIArg = union(enum) {
    /// Slots for this argument.
    slots: struct {
        slots: BoundedArray(ABIArgSlot, 4),
        purpose: ArgumentPurpose,
    },

    /// Create a single-slot register argument.
    pub fn reg(r: RealReg, ty: Type, extension: ArgumentExtension, purpose: ArgumentPurpose) ABIArg {
        var slots_array = BoundedArray(ABIArgSlot, 4){};
        slots_array.appendAssumeCapacity(.{
            .reg = .{
                .reg = r,
                .ty = ty,
                .extension = extension,
            },
        });
        return .{
            .slots = .{
                .slots = slots_array,
                .purpose = purpose,
            },
        };
    }

    /// Create a single-slot stack argument.
    pub fn stack(offset: i64, ty: Type, extension: ArgumentExtension, purpose: ArgumentPurpose) ABIArg {
        var slots_array = BoundedArray(ABIArgSlot, 4){};
        slots_array.appendAssumeCapacity(.{
            .stack = .{
                .offset = offset,
                .ty = ty,
                .extension = extension,
            },
        });
        return .{
            .slots = .{
                .slots = slots_array,
                .purpose = purpose,
            },
        };
    }
};

//=============================================================================
// Calling convention constants
//=============================================================================

/// System V AMD64 ABI: Integer/pointer argument registers.
/// Order: RDI, RSI, RDX, RCX, R8, R9
pub const SYSV_ARG_GPRS = [_]u8{
    GprEnc.RDI, GprEnc.RSI, GprEnc.RDX, GprEnc.RCX, GprEnc.R8, GprEnc.R9,
};

/// System V AMD64 ABI: Floating-point argument registers.
/// Order: XMM0-XMM7
pub const SYSV_ARG_FPRS = [_]u8{
    XmmEnc.XMM0, XmmEnc.XMM1, XmmEnc.XMM2, XmmEnc.XMM3,
    XmmEnc.XMM4, XmmEnc.XMM5, XmmEnc.XMM6, XmmEnc.XMM7,
};

/// System V AMD64 ABI: Return value registers (integer).
/// RAX, RDX (for 128-bit returns)
pub const SYSV_RET_GPRS = [_]u8{ GprEnc.RAX, GprEnc.RDX };

/// System V AMD64 ABI: Return value registers (floating-point).
/// XMM0, XMM1 (for 2-float returns)
pub const SYSV_RET_FPRS = [_]u8{ XmmEnc.XMM0, XmmEnc.XMM1 };

/// Windows x64: Integer/pointer argument registers.
/// Order: RCX, RDX, R8, R9
pub const WIN64_ARG_GPRS = [_]u8{
    GprEnc.RCX, GprEnc.RDX, GprEnc.R8, GprEnc.R9,
};

/// Windows x64: Floating-point argument registers.
/// XMM0-XMM3 (same positions as integer registers)
pub const WIN64_ARG_FPRS = [_]u8{
    XmmEnc.XMM0, XmmEnc.XMM1, XmmEnc.XMM2, XmmEnc.XMM3,
};

/// Windows x64: Return value register (integer).
pub const WIN64_RET_GPR = GprEnc.RAX;

/// Windows x64: Return value register (floating-point).
pub const WIN64_RET_FPR = XmmEnc.XMM0;

/// System V AMD64 ABI: Caller-saved (volatile) registers.
/// These are clobbered by function calls.
pub const SYSV_CLOBBERED_GPRS = [_]u8{
    GprEnc.RAX, GprEnc.RCX, GprEnc.RDX,
    GprEnc.RSI, GprEnc.RDI, GprEnc.R8,
    GprEnc.R9,  GprEnc.R10, GprEnc.R11,
};

/// System V AMD64 ABI: Callee-saved registers.
/// These must be preserved across function calls.
pub const SYSV_CALLEE_SAVED_GPRS = [_]u8{
    GprEnc.RBX, GprEnc.RBP, GprEnc.R12,
    GprEnc.R13, GprEnc.R14, GprEnc.R15,
};

/// Windows x64: Caller-saved (volatile) registers.
pub const WIN64_CLOBBERED_GPRS = [_]u8{
    GprEnc.RAX, GprEnc.RCX, GprEnc.RDX,
    GprEnc.R8,  GprEnc.R9,  GprEnc.R10,
    GprEnc.R11,
};

/// Windows x64: Callee-saved registers.
pub const WIN64_CALLEE_SAVED_GPRS = [_]u8{
    GprEnc.RBX, GprEnc.RBP, GprEnc.RDI,
    GprEnc.RSI, GprEnc.R12, GprEnc.R13,
    GprEnc.R14, GprEnc.R15,
};

/// All XMM registers (0-15) are caller-saved in System V.
pub const SYSV_CLOBBERED_FPRS = [_]u8{
    XmmEnc.XMM0,  XmmEnc.XMM1,  XmmEnc.XMM2,  XmmEnc.XMM3,
    XmmEnc.XMM4,  XmmEnc.XMM5,  XmmEnc.XMM6,  XmmEnc.XMM7,
    XmmEnc.XMM8,  XmmEnc.XMM9,  XmmEnc.XMM10, XmmEnc.XMM11,
    XmmEnc.XMM12, XmmEnc.XMM13, XmmEnc.XMM14, XmmEnc.XMM15,
};

/// Windows x64: Only XMM0-XMM5 are caller-saved.
/// XMM6-XMM15 are callee-saved (must preserve low 128 bits).
pub const WIN64_CLOBBERED_FPRS = [_]u8{
    XmmEnc.XMM0, XmmEnc.XMM1, XmmEnc.XMM2,
    XmmEnc.XMM3, XmmEnc.XMM4, XmmEnc.XMM5,
};

/// Windows x64: Callee-saved XMM registers.
pub const WIN64_CALLEE_SAVED_FPRS = [_]u8{
    XmmEnc.XMM6,  XmmEnc.XMM7,  XmmEnc.XMM8,  XmmEnc.XMM9,
    XmmEnc.XMM10, XmmEnc.XMM11, XmmEnc.XMM12, XmmEnc.XMM13,
    XmmEnc.XMM14, XmmEnc.XMM15,
};

//=============================================================================
// Stack frame layout
//=============================================================================

/// Standard x86-64 stack alignment (16 bytes before CALL).
pub const STACK_ALIGNMENT: u32 = 16;

/// Size of the return address pushed by CALL.
pub const RETURN_ADDRESS_SIZE: u32 = 8;

/// Shadow space required by Windows x64 ABI (32 bytes).
/// This is space the callee can use to spill the first 4 arguments.
pub const WIN64_SHADOW_SPACE: u32 = 32;

//=============================================================================
// ABI state tracking
//=============================================================================

/// Tracks state during argument/return value computation.
pub const ABIState = struct {
    /// Current GPR argument index (System V).
    gpr_idx: usize = 0,
    /// Current FPR argument index (System V).
    fpr_idx: usize = 0,
    /// Current argument index (Windows, unified).
    arg_idx: usize = 0,
    /// Current stack offset.
    stack_offset: i64 = 0,
    /// Calling convention.
    call_conv: CallConv,

    /// Initialize state for the given calling convention.
    pub fn init(call_conv: CallConv) ABIState {
        return .{
            .call_conv = call_conv,
        };
    }

    /// Get the next GPR for arguments, or null if exhausted.
    pub fn nextArgGpr(self: *ABIState) ?u8 {
        return switch (self.call_conv) {
            .system_v, .wasmtime_system_v, .probestack => {
                if (self.gpr_idx < SYSV_ARG_GPRS.len) {
                    const reg = SYSV_ARG_GPRS[self.gpr_idx];
                    self.gpr_idx += 1;
                    return reg;
                }
                return null;
            },
            .windows_fastcall, .wasmtime_fastcall => {
                if (self.arg_idx < WIN64_ARG_GPRS.len) {
                    const reg = WIN64_ARG_GPRS[self.arg_idx];
                    self.arg_idx += 1;
                    return reg;
                }
                return null;
            },
            .tail, .winch => {
                // Tail calls and winch may have different conventions
                if (self.gpr_idx < SYSV_ARG_GPRS.len) {
                    const reg = SYSV_ARG_GPRS[self.gpr_idx];
                    self.gpr_idx += 1;
                    return reg;
                }
                return null;
            },
        };
    }

    /// Get the next FPR for arguments, or null if exhausted.
    pub fn nextArgFpr(self: *ABIState) ?u8 {
        return switch (self.call_conv) {
            .system_v, .wasmtime_system_v, .probestack => {
                if (self.fpr_idx < SYSV_ARG_FPRS.len) {
                    const reg = SYSV_ARG_FPRS[self.fpr_idx];
                    self.fpr_idx += 1;
                    return reg;
                }
                return null;
            },
            .windows_fastcall, .wasmtime_fastcall => {
                // Windows x64: FPR uses same slot as GPR
                if (self.arg_idx < WIN64_ARG_FPRS.len) {
                    const reg = WIN64_ARG_FPRS[self.arg_idx];
                    self.arg_idx += 1;
                    return reg;
                }
                return null;
            },
            .tail, .winch => {
                if (self.fpr_idx < SYSV_ARG_FPRS.len) {
                    const reg = SYSV_ARG_FPRS[self.fpr_idx];
                    self.fpr_idx += 1;
                    return reg;
                }
                return null;
            },
        };
    }

    /// Allocate stack space for an argument.
    pub fn allocStackArg(self: *ABIState, size: u32) i64 {
        const offset = self.stack_offset;
        // Align to 8 bytes
        const aligned_size = (size + 7) & ~@as(u32, 7);
        self.stack_offset += aligned_size;
        return offset;
    }
};

//=============================================================================
// Argument assignment
//=============================================================================

/// Assign an argument to a register or stack slot.
pub fn assignArg(
    state: *ABIState,
    param: AbiParam,
    _: ArgsOrRets,
) ABIArg {
    const ty = param.value_type;
    const purpose = param.purpose;
    const extension = param.extension;

    // Check if it's a float type
    if (ty.isFloat()) {
        if (state.nextArgFpr()) |fpr_enc| {
            const rreg = RealReg{ .preg = PReg.init(fpr_enc, .float) };
            return ABIArg.reg(rreg, ty, extension, purpose);
        }
    } else {
        // Integer/pointer type
        if (state.nextArgGpr()) |gpr_enc| {
            const rreg = RealReg{ .preg = PReg.init(gpr_enc, .int) };
            return ABIArg.reg(rreg, ty, extension, purpose);
        }
    }

    // Fall through to stack
    const size = ty.bytes();
    const offset = state.allocStackArg(size);
    return ABIArg.stack(offset, ty, extension, purpose);
}

/// Assign a return value to a register or stack slot.
pub fn assignRet(
    call_conv: CallConv,
    param: AbiParam,
    ret_idx: usize,
) ABIArg {
    const ty = param.value_type;
    const extension = param.extension;

    if (ty.isFloat()) {
        // FP return in XMM0/XMM1
        if (ret_idx < 2) {
            const fpr_enc = if (ret_idx == 0) XmmEnc.XMM0 else XmmEnc.XMM1;
            const rreg = RealReg{ .preg = PReg.init(fpr_enc, .float) };
            return ABIArg.reg(rreg, ty, extension, .normal);
        }
    } else {
        // Integer return in RAX/RDX
        switch (call_conv) {
            .system_v, .wasmtime_system_v, .probestack, .tail, .winch => {
                if (ret_idx < SYSV_RET_GPRS.len) {
                    const gpr_enc = SYSV_RET_GPRS[ret_idx];
                    const rreg = RealReg{ .preg = PReg.init(gpr_enc, .int) };
                    return ABIArg.reg(rreg, ty, extension, .normal);
                }
            },
            .windows_fastcall, .wasmtime_fastcall => {
                if (ret_idx == 0) {
                    const rreg = RealReg{ .preg = PReg.init(WIN64_RET_GPR, .int) };
                    return ABIArg.reg(rreg, ty, extension, .normal);
                }
            },
        }
    }

    // For complex returns (struct return), use stack
    // This is a simplified version; full implementation would handle sret
    return ABIArg.stack(0, ty, extension, .struct_return);
}

//=============================================================================
// Prologue/Epilogue generation
//=============================================================================

/// Frame setup information.
pub const FrameSetup = struct {
    /// Total frame size (including locals, spills, outgoing args).
    frame_size: u32 = 0,
    /// Size of callee-saved register spill area.
    csr_spill_size: u32 = 0,
    /// Offset to local variables.
    locals_offset: i32 = 0,
    /// Whether we use a frame pointer.
    uses_frame_pointer: bool = false,
    /// Callee-saved GPRs to spill.
    csr_gprs: BoundedArray(u8, 16) = .{},
    /// Callee-saved FPRs to spill.
    csr_fprs: BoundedArray(u8, 16) = .{},
};

/// Compute frame setup for the given calling convention.
pub fn computeFrameSetup(
    call_conv: CallConv,
    locals_size: u32,
    outgoing_args_size: u32,
    used_callee_saved: []const u8,
    _: []const u8, // used_callee_saved_fprs
) FrameSetup {
    var setup = FrameSetup{};

    // Determine which callee-saved registers are used
    const callee_saved = switch (call_conv) {
        .windows_fastcall, .wasmtime_fastcall => &WIN64_CALLEE_SAVED_GPRS,
        else => &SYSV_CALLEE_SAVED_GPRS,
    };

    for (callee_saved) |reg| {
        for (used_callee_saved) |used| {
            if (reg == used) {
                setup.csr_gprs.appendAssumeCapacity(reg);
                setup.csr_spill_size += 8;
                break;
            }
        }
    }

    // Frame size = return addr + csr spills + locals + outgoing args
    // Align to 16 bytes
    const total = RETURN_ADDRESS_SIZE + setup.csr_spill_size + locals_size + outgoing_args_size;
    setup.frame_size = (total + 15) & ~@as(u32, 15);

    // Use frame pointer if we have locals or complex stack
    setup.uses_frame_pointer = locals_size > 0 or setup.csr_spill_size > 0;

    // Locals are at negative offset from RBP (if using frame pointer)
    setup.locals_offset = -@as(i32, @intCast(locals_size));

    return setup;
}

//=============================================================================
// Clobber sets
//=============================================================================

/// Get the set of registers clobbered by a call.
pub fn getClobberSet(call_conv: CallConv) struct { gprs: []const u8, fprs: []const u8 } {
    return switch (call_conv) {
        .windows_fastcall, .wasmtime_fastcall => .{
            .gprs = &WIN64_CLOBBERED_GPRS,
            .fprs = &WIN64_CLOBBERED_FPRS,
        },
        else => .{
            .gprs = &SYSV_CLOBBERED_GPRS,
            .fprs = &SYSV_CLOBBERED_FPRS,
        },
    };
}

//=============================================================================
// Helper functions
//=============================================================================

/// Check if a register is callee-saved in the given calling convention.
pub fn isCalleeSaved(call_conv: CallConv, reg_enc: u8, is_fpr: bool) bool {
    if (is_fpr) {
        return switch (call_conv) {
            .windows_fastcall, .wasmtime_fastcall => {
                for (WIN64_CALLEE_SAVED_FPRS) |saved| {
                    if (saved == reg_enc) return true;
                }
                return false;
            },
            else => false, // System V: all XMM are caller-saved
        };
    } else {
        const callee_saved = switch (call_conv) {
            .windows_fastcall, .wasmtime_fastcall => &WIN64_CALLEE_SAVED_GPRS,
            else => &SYSV_CALLEE_SAVED_GPRS,
        };
        for (callee_saved) |saved| {
            if (saved == reg_enc) return true;
        }
        return false;
    }
}

/// Get the stack pointer register.
pub fn stackPointerReg() Reg {
    return rsp();
}

/// Get the frame pointer register.
pub fn framePointerReg() Reg {
    return rbp();
}

/// Get the link register (return address).
/// Note: x86-64 doesn't have a dedicated link register; the return
/// address is on the stack. For consistency, we return nothing.
pub fn linkReg() ?Reg {
    return null;
}

/// Align a value up to the given alignment.
pub fn alignTo(value: u32, alignment: u32) u32 {
    std.debug.assert(std.math.isPowerOfTwo(alignment));
    return (value + alignment - 1) & ~(alignment - 1);
}

//=============================================================================
// Stack addressing mode
//=============================================================================

/// Stack addressing mode for x86-64.
pub const StackAMode = union(enum) {
    /// Incoming argument on stack (relative to caller's SP).
    incoming_arg: struct {
        off: i64,
        stack_args_size: u32,
    },
    /// Stack slot offset (relative to nominal SP).
    slot: i64,
    /// Outgoing argument (relative to current SP).
    outgoing_arg: i64,

    /// Convert to SyntheticAmode for emission.
    pub fn toAmode(self: StackAMode) SyntheticAmode {
        return switch (self) {
            .incoming_arg => |ia| SyntheticAmode.incomingArg(
                @intCast(@as(i64, ia.stack_args_size) + RETURN_ADDRESS_SIZE - ia.off),
            ),
            .slot => |off| SyntheticAmode.slotOffset(@intCast(off)),
            .outgoing_arg => |off| SyntheticAmode.real_amode(
                Amode.immReg(@intCast(off), rsp()),
            ),
        };
    }
};

//=============================================================================
// Frame layout
//=============================================================================

/// Frame layout for x86-64 functions.
/// This describes the stack frame structure after prologue execution.
pub const FrameLayout = struct {
    /// Word size in bytes (8 for x86-64).
    word_bytes: u32 = 8,
    /// Size of incoming arguments on the stack.
    incoming_args_size: u32 = 0,
    /// Size of tail call arguments area.
    tail_args_size: u32 = 0,
    /// Size of the setup area (RBP save).
    setup_area_size: u32 = 0,
    /// Size of clobber save area.
    clobber_size: u32 = 0,
    /// Size of fixed frame storage (locals).
    fixed_frame_storage_size: u32 = 0,
    /// Size of stack slots.
    stackslots_size: u32 = 0,
    /// Size of outgoing arguments area.
    outgoing_args_size: u32 = 0,
    /// Clobbered callee-saved registers.
    clobbered_callee_saves: BoundedArray(Writable(RealReg), 32) = .{},
    /// Function calls status.
    function_calls: FunctionCalls = .none,

    /// Get the total frame size.
    pub fn frameSize(self: *const FrameLayout) u32 {
        return self.setup_area_size + self.clobber_size +
            self.fixed_frame_storage_size + self.stackslots_size +
            self.outgoing_args_size;
    }

    /// Get the clobbered callee-saves by class.
    pub fn clobberedCalleeSavesByClass(self: *const FrameLayout) struct {
        int_regs: []const Writable(RealReg),
        vec_regs: []const Writable(RealReg),
    } {
        var int_count: usize = 0;
        var vec_start: usize = self.clobbered_callee_saves.len;

        // Count int registers first
        for (self.clobbered_callee_saves.constSlice(), 0..) |reg, i| {
            if (reg.toReg().class() == .int) {
                int_count += 1;
            } else {
                vec_start = i;
                break;
            }
        }

        if (int_count == self.clobbered_callee_saves.len) {
            // All int, no vec
            return .{
                .int_regs = self.clobbered_callee_saves.constSlice(),
                .vec_regs = &[_]Writable(RealReg){},
            };
        }

        return .{
            .int_regs = self.clobbered_callee_saves.constSlice()[0..int_count],
            .vec_regs = self.clobbered_callee_saves.constSlice()[vec_start..],
        };
    }
};

//=============================================================================
// ISA flags
//=============================================================================

/// x86-64 ISA-specific flags.
pub const IsaFlags = struct {
    /// Use AVX instructions.
    use_avx: bool = false,
    /// Use AVX2 instructions.
    use_avx2: bool = false,
    /// Use AVX-512 instructions.
    use_avx512: bool = false,
    /// Use BMI1 instructions.
    use_bmi1: bool = false,
    /// Use BMI2 instructions.
    use_bmi2: bool = false,
    /// Use LZCNT instruction.
    use_lzcnt: bool = false,
    /// Use POPCNT instruction.
    use_popcnt: bool = false,
    /// Use FMA instructions.
    use_fma: bool = false,
    /// Use SSE4.1 instructions.
    use_sse41: bool = false,
    /// Use SSE4.2 instructions.
    use_sse42: bool = false,

    pub const default = IsaFlags{};
};

/// General settings flags.
pub const SettingsFlags = struct {
    /// Preserve frame pointers.
    preserve_frame_pointers: bool = true,
    /// Enable pinned register.
    enable_pinned_reg: bool = false,
    /// Generate unwind info.
    unwind_info: bool = true,
    /// Enable multi-return implicit sret.
    enable_multi_ret_implicit_sret: bool = false,
    /// Enable LLVM ABI extensions.
    enable_llvm_abi_extensions: bool = false,

    pub const default = SettingsFlags{};
};

//=============================================================================
// Signature
//=============================================================================

/// Function signature.
pub const Signature = struct {
    /// Parameters.
    params: []const AbiParam,
    /// Return values.
    returns: []const AbiParam,
};

//=============================================================================
// Unwind instructions
//=============================================================================

/// Unwind instruction for DWARF/etc.
pub const UnwindInst = union(enum) {
    /// Define a new frame.
    define_new_frame: struct {
        offset_downward_to_clobbers: u32,
        offset_upward_to_caller_sp: u32,
    },
    /// Push frame register (RBP).
    push_frame_reg: struct {
        offset_upward_to_caller_sp: u32,
    },
    /// Save a register.
    save_reg: struct {
        clobber_offset: u32,
        reg: RealReg,
    },
    /// Stack allocation.
    stack_alloc: struct {
        size: u32,
    },
};

//=============================================================================
// PRegSet for x64 - import from inst/mod.zig (which uses regalloc's PRegSet)
//=============================================================================

/// Use the regalloc-compatible PRegSet that supports the full register space.
pub const PRegSet = inst.PRegSet;

//=============================================================================
// Default clobber sets
//=============================================================================
// Machine environment for register allocation
//=============================================================================

/// Machine environment for register allocation.
/// This matches the ARM64 pattern for consistency.
pub const MachineEnv = struct {
    /// Preferred registers by class (caller-saved, easy to allocate).
    preferred_regs_by_class: [3]BoundedArray(PReg, 32),
    /// Non-preferred registers by class (callee-saved, need to be preserved).
    non_preferred_regs_by_class: [3]BoundedArray(PReg, 16),
    /// Fixed stack slots.
    fixed_stack_slots: []const u32,
    /// Scratch register by class.
    scratch_by_class: [3]?PReg,
};

/// Create the register environment for x64.
/// This is used by the register allocator to know which registers are available.
pub fn createRegEnv(enable_pinned_reg: bool) MachineEnv {
    var env = MachineEnv{
        .preferred_regs_by_class = .{
            BoundedArray(PReg, 32){}, // int
            BoundedArray(PReg, 32){}, // float
            BoundedArray(PReg, 32){}, // vector (unused)
        },
        .non_preferred_regs_by_class = .{
            BoundedArray(PReg, 16){}, // int
            BoundedArray(PReg, 16){}, // float
            BoundedArray(PReg, 16){}, // vector (unused)
        },
        .fixed_stack_slots = &[_]u32{},
        .scratch_by_class = .{ null, null, null },
    };

    // Preferred integer registers: caller-saved (rax, rcx, rdx, rsi, rdi, r8-r10)
    // Note: r11 is scratch, rsp/rbp not allocatable
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.RAX));
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.RCX));
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.RDX));
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.RSI));
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.RDI));
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.R8));
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.R9));
    env.preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.R10));
    // r11 is scratch register, not in preferred

    // Preferred float registers: xmm0-xmm15 (all caller-saved on System V)
    var i: u8 = 0;
    while (i < 16) : (i += 1) {
        env.preferred_regs_by_class[1].appendAssumeCapacity(fprPreg(i));
    }

    // Non-preferred integer registers: callee-saved (rbx, r12-r14)
    // When enable_pinned_reg is true, R15 is excluded (used as vmctx pinned register).
    // Port of ARM64 pattern: cranelift/codegen/src/isa/x64/abi.rs
    env.non_preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.RBX));
    env.non_preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.R12));
    env.non_preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.R13));
    env.non_preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.R14));
    if (!enable_pinned_reg) {
        env.non_preferred_regs_by_class[0].appendAssumeCapacity(gprPreg(GprEnc.R15));
    }

    // Set r11 as scratch register
    env.scratch_by_class[0] = gprPreg(GprEnc.R11);

    return env;
}

//=============================================================================
// Default clobber sets
// Using regalloc-compatible PRegSet with .with() for compile-time construction
//=============================================================================

/// System V x64 clobbers: rax, rcx, rdx, rsi, rdi, r8-r11, and all xmm.
pub const DEFAULT_SYSV_CLOBBERS: PRegSet = blk: {
    // Start with empty set and add caller-saved GPRs
    var set = PRegSet.empty()
        .with(gprPreg(GprEnc.RAX))
        .with(gprPreg(GprEnc.RCX))
        .with(gprPreg(GprEnc.RDX))
        .with(gprPreg(GprEnc.RSI))
        .with(gprPreg(GprEnc.RDI))
        .with(gprPreg(GprEnc.R8))
        .with(gprPreg(GprEnc.R9))
        .with(gprPreg(GprEnc.R10))
        .with(gprPreg(GprEnc.R11));
    // All XMM are caller-saved in System V (xmm0-xmm15)
    var i: u8 = 0;
    while (i < 16) : (i += 1) {
        set = set.with(fprPreg(i));
    }
    break :blk set;
};

/// Windows x64 clobbers: rax, rcx, rdx, r8-r11, xmm0-xmm5.
pub const DEFAULT_WIN64_CLOBBERS: PRegSet = blk: {
    // Caller-saved GPRs (note: RSI/RDI are callee-saved on Windows!)
    var set = PRegSet.empty()
        .with(gprPreg(GprEnc.RAX))
        .with(gprPreg(GprEnc.RCX))
        .with(gprPreg(GprEnc.RDX))
        .with(gprPreg(GprEnc.R8))
        .with(gprPreg(GprEnc.R9))
        .with(gprPreg(GprEnc.R10))
        .with(gprPreg(GprEnc.R11));
    // Only XMM0-5 are caller-saved in Windows
    var i: u8 = 0;
    while (i < 6) : (i += 1) {
        set = set.with(fprPreg(i));
    }
    break :blk set;
};

/// All registers clobbered (for exceptions).
pub const ALL_CLOBBERS: PRegSet = blk: {
    var set = PRegSet.empty();
    // All GPRs
    var i: u8 = 0;
    while (i < 16) : (i += 1) {
        set = set.with(gprPreg(i));
    }
    // All XMMs
    i = 0;
    while (i < 16) : (i += 1) {
        set = set.with(fprPreg(i));
    }
    break :blk set;
};

/// No clobbers.
pub const NO_CLOBBERS: PRegSet = PRegSet.empty();

//=============================================================================
// X64MachineDeps - main ABI implementation
//=============================================================================

/// x86-64-specific ABI behavior. This struct just serves as an implementation
/// point for the trait; it is never actually instantiated.
pub const X64MachineDeps = struct {
    const Self = @This();

    /// Instruction type.
    pub const I = Inst;

    /// ISA flags type.
    pub const F = IsaFlags;

    /// This is the limit for the size of argument and return-value areas on the
    /// stack. We place a reasonable limit here to avoid integer overflow issues
    /// with 32-bit arithmetic: for now, 128 MB.
    pub const STACK_ARG_RET_SIZE_LIMIT: u32 = 128 * 1024 * 1024;

    /// Word size in bits.
    pub fn wordBits() u32 {
        return 64;
    }

    /// Word type.
    pub fn wordType() Type {
        return Type.I64;
    }

    /// Return required stack alignment in bytes.
    pub fn stackAlign(call_conv: CallConv) u32 {
        _ = call_conv;
        return 16;
    }

    /// Get the register class for a type.
    pub fn rcForType(value_type: Type) struct { rcs: []const RegClass, reg_types: []const Type } {
        const static_int: []const RegClass = &[_]RegClass{.int};
        const static_float: []const RegClass = &[_]RegClass{.float};
        const static_int_pair: []const RegClass = &[_]RegClass{ .int, .int };

        const static_i64: []const Type = &[_]Type{Type.I64};
        const static_f64: []const Type = &[_]Type{Type.F64};
        const static_i64_pair: []const Type = &[_]Type{ Type.I64, Type.I64 };

        // I128 special case (two registers)
        if (value_type.eql(Type.I128)) {
            return .{ .rcs = static_int_pair, .reg_types = static_i64_pair };
        }
        // Integer types
        if (value_type.isInt()) {
            return .{ .rcs = static_int, .reg_types = static_i64 };
        }
        // Float types
        if (value_type.isFloat()) {
            return .{ .rcs = static_float, .reg_types = static_f64 };
        }
        // Vector types use float registers
        if (value_type.isVector()) {
            return .{ .rcs = static_float, .reg_types = static_f64 };
        }
        // Default to int
        return .{ .rcs = static_int, .reg_types = static_i64 };
    }

    /// Compute argument/return value locations.
    /// Implements System V AMD64 and Windows x64 ABIs.
    pub fn computeArgLocs(
        call_conv: CallConv,
        flags: SettingsFlags,
        params: []const AbiParam,
        args_or_rets: ArgsOrRets,
        add_ret_area_ptr: bool,
        allocator: Allocator,
    ) !struct { stack_size: u32, ret_area_ptr_idx: ?usize, args: std.ArrayListUnmanaged(ABIArg) } {
        const is_win64 = call_conv == .windows_fastcall or call_conv == .wasmtime_fastcall;

        var next_gpr: u8 = 0;
        var next_fpr: u8 = 0;
        var arg_idx: u8 = 0; // For Windows unified arg slots
        var next_stack: u32 = 0;

        // Windows needs 32-byte shadow space for first 4 args
        if (is_win64 and args_or_rets == .args) {
            next_stack = WIN64_SHADOW_SPACE;
        }

        var args_list = std.ArrayListUnmanaged(ABIArg){};

        // Handle return area pointer
        var ret_area_ptr: ?ABIArg = null;
        if (add_ret_area_ptr) {
            std.debug.assert(args_or_rets == .args);
            // Return area pointer goes in first integer arg reg
            if (is_win64) {
                ret_area_ptr = ABIArg.reg(
                    RealReg{ .preg = PReg.init(WIN64_ARG_GPRS[0], .int) },
                    Type.I64,
                    .none,
                    .struct_return,
                );
                arg_idx += 1;
            } else {
                ret_area_ptr = ABIArg.reg(
                    RealReg{ .preg = PReg.init(SYSV_ARG_GPRS[0], .int) },
                    Type.I64,
                    .none,
                    .struct_return,
                );
                next_gpr += 1;
            }
        }

        for (params) |param| {
            const rc_info = rcForType(param.value_type);
            const rcs = rc_info.rcs;
            const reg_types = rc_info.reg_types;

            // Handle StructReturn
            if (param.purpose == .struct_return) {
                std.debug.assert(param.value_type.eql(Type.I64));
                // Already handled by add_ret_area_ptr
                continue;
            }

            // Handle multi-register params (i128)
            const is_multi_reg = rcs.len >= 2;
            if (is_multi_reg) {
                std.debug.assert(rcs.len == 2);

                if (is_win64) {
                    // Windows: i128 goes on stack
                    const offset = next_stack;
                    next_stack += 16;

                    var slots_array = BoundedArray(ABIArgSlot, 4){};
                    slots_array.appendAssumeCapacity(.{
                        .stack = .{
                            .offset = @intCast(offset),
                            .ty = reg_types[0],
                            .extension = param.extension,
                        },
                    });
                    slots_array.appendAssumeCapacity(.{
                        .stack = .{
                            .offset = @intCast(offset + 8),
                            .ty = reg_types[1],
                            .extension = param.extension,
                        },
                    });

                    try args_list.append(allocator, .{
                        .slots = .{
                            .slots = slots_array,
                            .purpose = param.purpose,
                        },
                    });
                } else {
                    // System V: i128 in RDX:RAX for returns, or two consecutive regs for args
                    if (args_or_rets == .rets) {
                        var slots_array = BoundedArray(ABIArgSlot, 4){};
                        slots_array.appendAssumeCapacity(.{
                            .reg = .{
                                .reg = RealReg{ .preg = PReg.init(GprEnc.RAX, .int) },
                                .ty = reg_types[0],
                                .extension = param.extension,
                            },
                        });
                        slots_array.appendAssumeCapacity(.{
                            .reg = .{
                                .reg = RealReg{ .preg = PReg.init(GprEnc.RDX, .int) },
                                .ty = reg_types[1],
                                .extension = param.extension,
                            },
                        });

                        try args_list.append(allocator, .{
                            .slots = .{
                                .slots = slots_array,
                                .purpose = param.purpose,
                            },
                        });
                    } else if (next_gpr + 2 <= SYSV_ARG_GPRS.len) {
                        var slots_array = BoundedArray(ABIArgSlot, 4){};
                        slots_array.appendAssumeCapacity(.{
                            .reg = .{
                                .reg = RealReg{ .preg = PReg.init(SYSV_ARG_GPRS[next_gpr], .int) },
                                .ty = reg_types[0],
                                .extension = param.extension,
                            },
                        });
                        slots_array.appendAssumeCapacity(.{
                            .reg = .{
                                .reg = RealReg{ .preg = PReg.init(SYSV_ARG_GPRS[next_gpr + 1], .int) },
                                .ty = reg_types[1],
                                .extension = param.extension,
                            },
                        });

                        try args_list.append(allocator, .{
                            .slots = .{
                                .slots = slots_array,
                                .purpose = param.purpose,
                            },
                        });
                        next_gpr += 2;
                    } else {
                        // Spill to stack
                        const offset = next_stack;
                        next_stack += 16;

                        var slots_array = BoundedArray(ABIArgSlot, 4){};
                        slots_array.appendAssumeCapacity(.{
                            .stack = .{
                                .offset = @intCast(offset),
                                .ty = reg_types[0],
                                .extension = param.extension,
                            },
                        });
                        slots_array.appendAssumeCapacity(.{
                            .stack = .{
                                .offset = @intCast(offset + 8),
                                .ty = reg_types[1],
                                .extension = param.extension,
                            },
                        });

                        try args_list.append(allocator, .{
                            .slots = .{
                                .slots = slots_array,
                                .purpose = param.purpose,
                            },
                        });
                    }
                }
                continue;
            }

            // Single register parameter
            const rc = rcs[0];

            if (is_win64) {
                // Windows: unified slots, same index for GPR and FPR
                if (arg_idx < 4) {
                    const reg_enc = if (rc == .float)
                        WIN64_ARG_FPRS[arg_idx]
                    else
                        WIN64_ARG_GPRS[arg_idx];

                    const reg_class: RegClass = if (rc == .float) .float else .int;

                    try args_list.append(allocator, ABIArg.reg(
                        RealReg{ .preg = PReg.init(reg_enc, reg_class) },
                        param.value_type,
                        param.extension,
                        param.purpose,
                    ));
                    arg_idx += 1;
                    continue;
                }
            } else {
                // System V: separate GPR and FPR counts
                const max_gprs: u8 = @intCast(SYSV_ARG_GPRS.len);
                const max_fprs: u8 = @intCast(SYSV_ARG_FPRS.len);

                if (rc == .float) {
                    if (next_fpr < max_fprs) {
                        try args_list.append(allocator, ABIArg.reg(
                            RealReg{ .preg = PReg.init(SYSV_ARG_FPRS[next_fpr], .float) },
                            param.value_type,
                            param.extension,
                            param.purpose,
                        ));
                        next_fpr += 1;
                        continue;
                    }
                } else {
                    if (next_gpr < max_gprs) {
                        try args_list.append(allocator, ABIArg.reg(
                            RealReg{ .preg = PReg.init(SYSV_ARG_GPRS[next_gpr], .int) },
                            param.value_type,
                            param.extension,
                            param.purpose,
                        ));
                        next_gpr += 1;
                        continue;
                    }
                }
            }

            // Spill to stack
            if (args_or_rets == .rets and !flags.enable_multi_ret_implicit_sret) {
                return error.TooManyReturnValues;
            }

            const size: u32 = @intCast(param.value_type.bits() / 8);
            const aligned_size = @max(size, 8); // Min 8-byte stack slots
            next_stack = alignTo(next_stack, aligned_size);

            try args_list.append(allocator, ABIArg.stack(
                @intCast(next_stack),
                param.value_type,
                param.extension,
                param.purpose,
            ));
            next_stack += aligned_size;
        }

        // Handle return area pointer
        var ret_area_ptr_idx: ?usize = null;
        if (ret_area_ptr) |ptr| {
            try args_list.append(allocator, ptr);
            ret_area_ptr_idx = args_list.items.len - 1;
        }

        // Align stack to 16 bytes
        next_stack = alignTo(next_stack, 16);

        return .{
            .stack_size = next_stack,
            .ret_area_ptr_idx = ret_area_ptr_idx,
            .args = args_list,
        };
    }

    /// Generate a load from stack.
    pub fn genLoadStack(mem: StackAMode, into_reg: Writable(Reg), ty: Type) Inst {
        const amode = mem.toAmode();
        return Inst.movRM(.{
            .size = OperandSize.fromType(ty),
            .src = amode,
            .dst = into_reg.toGpr(),
        });
    }

    /// Generate a store to stack.
    pub fn genStoreStack(mem: StackAMode, from_reg: Reg, ty: Type) Inst {
        const amode = mem.toAmode();
        return Inst.movMR(.{
            .size = OperandSize.fromType(ty),
            .src = Gpr.fromReg(from_reg),
            .dst = amode,
        });
    }

    /// Generate a move between registers.
    pub fn genMove(to_reg: Writable(Reg), from_reg: Reg, ty: Type) Inst {
        if (ty.isFloat()) {
            // Use MOVAPS for XMM registers
            return Inst.xmmMov(.{
                .op = .movaps,
                .src = Xmm.fromReg(from_reg),
                .dst = to_reg.toXmm(),
            });
        } else {
            // Use MOV for GPRs
            return Inst.movRR(.{
                .size = OperandSize.fromType(ty),
                .src = Gpr.fromReg(from_reg),
                .dst = to_reg.toGpr(),
            });
        }
    }

    /// Generate an extend instruction.
    pub fn genExtend(
        to_reg: Writable(Reg),
        from_reg: Reg,
        signed: bool,
        from_bits: u8,
        to_bits: u8,
    ) Inst {
        std.debug.assert(from_bits < to_bits);

        const ext_mode = ExtMode.fromBits(from_bits, to_bits);

        if (signed) {
            return Inst.movsxRmR(.{
                .ext_mode = ext_mode,
                .src = .{ .reg = Gpr.fromReg(from_reg) },
                .dst = to_reg.toGpr(),
            });
        } else {
            return Inst.movzxRmR(.{
                .ext_mode = ext_mode,
                .src = .{ .reg = Gpr.fromReg(from_reg) },
                .dst = to_reg.toGpr(),
            });
        }
    }

    /// Generate an add with immediate.
    pub fn genAddImm(
        call_conv: CallConv,
        into_reg: Writable(Reg),
        from_reg: Reg,
        imm: u32,
    ) BoundedArray(Inst, 4) {
        _ = call_conv;
        var insts = BoundedArray(Inst, 4){};

        if (imm == 0) {
            // Just move if no add needed
            if (into_reg.toReg().bits != from_reg.bits) {
                insts.appendAssumeCapacity(genMove(into_reg, from_reg, Type.I64));
            }
            return insts;
        }

        // If into_reg != from_reg, move first then add
        if (into_reg.toReg().bits != from_reg.bits) {
            insts.appendAssumeCapacity(genMove(into_reg, from_reg, Type.I64));
        }

        // ADD r64, imm32
        insts.appendAssumeCapacity(Inst.aluRmiR(.{
            .size = .size64,
            .op = .add,
            .src1 = into_reg.toGpr(),
            .src2 = .{ .imm = imm },
            .dst = into_reg.toGpr(),
        }));

        return insts;
    }

    /// Generate SP register adjustment.
    pub fn genSpRegAdjust(amount: i32) BoundedArray(Inst, 4) {
        var ret = BoundedArray(Inst, 4){};

        if (amount == 0) {
            return ret;
        }

        const abs_amount: u32 = if (amount > 0)
            @intCast(amount)
        else
            @intCast(-amount);
        const is_sub = amount < 0;
        const op: AluRmiROpcode = if (is_sub) .sub else .add;

        // Use RSP as both src and dst
        const rsp_w = Writable(Gpr).fromReg(Gpr.fromReg(rsp()));

        ret.appendAssumeCapacity(Inst.aluRmiR(.{
            .size = .size64,
            .op = op,
            .src1 = rsp_w.toReg(),
            .src2 = .{ .imm = abs_amount },
            .dst = rsp_w,
        }));

        return ret;
    }

    /// Get address of stack slot.
    pub fn genGetStackAddr(mem: StackAMode, into_reg: Writable(Reg)) Inst {
        const amode = mem.toAmode();
        return Inst.lea(.{
            .size = .size64,
            .src = amode,
            .dst = into_reg.toGpr(),
        });
    }

    /// Get the stack limit register.
    pub fn getStacklimitReg(call_conv: CallConv) Reg {
        _ = call_conv;
        // Use r11 as scratch/stack limit register
        return r11();
    }

    /// Generate a load from base + offset.
    pub fn genLoadBaseOffset(into_reg: Writable(Reg), base: Reg, offset: i32, ty: Type) Inst {
        const amode = SyntheticAmode{ .real_reg = .{ .reg = Gpr.fromReg(base), .offset = offset } };
        return Inst.movRM(.{
            .size = OperandSize.fromType(ty),
            .src = amode,
            .dst = into_reg.toGpr(),
        });
    }

    /// Generate a store to base + offset.
    pub fn genStoreBaseOffset(base: Reg, offset: i32, from_reg: Reg, ty: Type) Inst {
        const amode = SyntheticAmode{ .real_reg = .{ .reg = Gpr.fromReg(base), .offset = offset } };
        return Inst.movMR(.{
            .size = OperandSize.fromType(ty),
            .src = Gpr.fromReg(from_reg),
            .dst = amode,
        });
    }

    /// Generate prologue frame setup.
    /// Standard x64 prologue: push rbp; mov rbp, rsp
    pub fn genPrologueFrameSetup(
        call_conv: CallConv,
        flags: SettingsFlags,
        isa_flags: IsaFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 16) {
        _ = call_conv;
        _ = isa_flags;
        const setup_frame = frame_layout.setup_area_size > 0;
        var insts = BoundedArray(Inst, 16){};

        if (setup_frame) {
            // push rbp
            insts.appendAssumeCapacity(Inst.push64(.{
                .src = .{ .reg = Gpr.fromReg(rbp()) },
            }));

            // mov rbp, rsp
            const rbp_w = Writable(Gpr).fromReg(Gpr.fromReg(rbp()));
            insts.appendAssumeCapacity(Inst.movRR(.{
                .size = .size64,
                .src = Gpr.fromReg(rsp()),
                .dst = rbp_w,
            }));

            if (flags.unwind_info) {
                // CFI directives would be added here
            }
        }

        return insts;
    }

    /// Generate epilogue frame restore.
    pub fn genEpilogueFrameRestore(
        call_conv: CallConv,
        flags: SettingsFlags,
        isa_flags: IsaFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 8) {
        _ = call_conv;
        _ = flags;
        _ = isa_flags;
        const setup_frame = frame_layout.setup_area_size > 0;
        var insts = BoundedArray(Inst, 8){};

        if (setup_frame) {
            // mov rsp, rbp
            const rsp_w = Writable(Gpr).fromReg(Gpr.fromReg(rsp()));
            insts.appendAssumeCapacity(Inst.movRR(.{
                .size = .size64,
                .src = Gpr.fromReg(rbp()),
                .dst = rsp_w,
            }));

            // pop rbp
            const rbp_w = Writable(Gpr).fromReg(Gpr.fromReg(rbp()));
            insts.appendAssumeCapacity(Inst.pop64(.{
                .dst = rbp_w,
            }));
        }

        return insts;
    }

    /// Generate return instruction.
    pub fn genReturn(
        call_conv: CallConv,
        isa_flags: IsaFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 2) {
        _ = call_conv;
        _ = isa_flags;
        _ = frame_layout;
        var insts = BoundedArray(Inst, 2){};
        insts.appendAssumeCapacity(Inst.ret());
        return insts;
    }

    /// Generate clobber save.
    pub fn genClobberSave(
        call_conv: CallConv,
        flags: SettingsFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 32) {
        _ = call_conv;
        _ = flags;
        var insts = BoundedArray(Inst, 32){};
        const clobbered = frame_layout.clobberedCalleeSavesByClass();

        // Push integer registers
        for (clobbered.int_regs) |reg| {
            const preg = PReg.init(reg.toReg().hwEnc(), .int);
            insts.appendAssumeCapacity(Inst.push64(.{
                .src = .{ .reg = Gpr.fromReg(Reg.fromRealReg(preg)) },
            }));
        }

        // Save XMM registers (Windows x64 only, System V doesn't save XMM)
        // For simplicity, use SUB rsp followed by MOVAPS
        const vec_regs = clobbered.vec_regs;
        if (vec_regs.len > 0) {
            const vec_save_size: u32 = @intCast(vec_regs.len * 16);
            // Allocate space for XMM saves
            const adj_insts = genSpRegAdjust(-@as(i32, @intCast(vec_save_size)));
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }

            // Save each XMM register
            var offset: i32 = 0;
            for (vec_regs) |reg| {
                const preg = PReg.init(reg.toReg().hwEnc(), .float);
                const amode = SyntheticAmode{ .real_sp = .{ .offset = offset } };
                insts.appendAssumeCapacity(Inst.xmmMovRM(.{
                    .op = .movaps,
                    .src = Xmm.fromReg(Reg.fromRealReg(preg)),
                    .dst = amode,
                }));
                offset += 16;
            }
        }

        // Allocate the fixed frame
        const stack_size = frame_layout.fixed_frame_storage_size + frame_layout.outgoing_args_size;
        if (stack_size > 0) {
            const adj_insts = genSpRegAdjust(-@as(i32, @intCast(stack_size)));
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }
        }

        return insts;
    }

    /// Generate clobber restore.
    pub fn genClobberRestore(
        call_conv: CallConv,
        flags: SettingsFlags,
        frame_layout: *const FrameLayout,
    ) BoundedArray(Inst, 32) {
        _ = call_conv;
        _ = flags;
        var insts = BoundedArray(Inst, 32){};
        const clobbered = frame_layout.clobberedCalleeSavesByClass();

        // Free the fixed frame
        const stack_size = frame_layout.fixed_frame_storage_size + frame_layout.outgoing_args_size;
        if (stack_size > 0) {
            const adj_insts = genSpRegAdjust(@intCast(stack_size));
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }
        }

        // Restore XMM registers (reverse order)
        const vec_regs = clobbered.vec_regs;
        if (vec_regs.len > 0) {
            // Restore each XMM register
            var offset: i32 = @intCast((vec_regs.len - 1) * 16);
            var i: usize = vec_regs.len;
            while (i > 0) {
                i -= 1;
                const reg = vec_regs[i];
                const preg = PReg.init(reg.toReg().hwEnc(), .float);
                const amode = SyntheticAmode{ .real_sp = .{ .offset = offset } };
                const xmm_w = Writable(Xmm).fromReg(Xmm.fromReg(Reg.fromRealReg(preg)));
                insts.appendAssumeCapacity(Inst.xmmRmR(.{
                    .op = .movaps,
                    .src = amode,
                    .dst = xmm_w,
                }));
                offset -= 16;
            }

            // Deallocate XMM save area
            const vec_save_size: u32 = @intCast(vec_regs.len * 16);
            const adj_insts = genSpRegAdjust(@intCast(vec_save_size));
            for (adj_insts.constSlice()) |adj_inst| {
                insts.appendAssumeCapacity(adj_inst);
            }
        }

        // Pop integer registers (reverse order)
        var i: usize = clobbered.int_regs.len;
        while (i > 0) {
            i -= 1;
            const reg = clobbered.int_regs[i];
            const preg = PReg.init(reg.toReg().hwEnc(), .int);
            const gpr_w = Writable(Gpr).fromReg(Gpr.fromReg(Reg.fromRealReg(preg)));
            insts.appendAssumeCapacity(Inst.pop64(.{
                .dst = gpr_w,
            }));
        }

        return insts;
    }

    /// Get number of spill slots for a value.
    pub fn getNumberOfSpillslotsForValue(
        rc: RegClass,
        vector_size: u32,
        isa_flags: IsaFlags,
    ) u32 {
        _ = isa_flags;
        std.debug.assert(vector_size % 8 == 0);
        // We allocate in terms of 8-byte slots
        return switch (rc) {
            .int => 1,
            .float => vector_size / 8, // XMM is 16 bytes = 2 slots
            .vector => unreachable,
        };
    }

    /// Get registers clobbered by call.
    pub fn getRegsClobberedByCall(call_conv: CallConv, is_exception: bool) PRegSet {
        if (is_exception) {
            return ALL_CLOBBERS;
        }
        return switch (call_conv) {
            .windows_fastcall, .wasmtime_fastcall => DEFAULT_WIN64_CLOBBERS,
            else => DEFAULT_SYSV_CLOBBERS,
        };
    }

    /// Get extension mode for a calling convention.
    pub fn getExtMode(
        call_conv: CallConv,
        specified: ArgumentExtension,
    ) ArgumentExtension {
        _ = call_conv;
        return specified;
    }

    /// Compute frame layout.
    pub fn computeFrameLayout(
        call_conv: CallConv,
        flags: SettingsFlags,
        sig: Signature,
        clobbered_regs: []const Writable(RealReg),
        function_calls: FunctionCalls,
        incoming_args_size: u32,
        tail_args_size: u32,
        stackslots_size: u32,
        fixed_frame_storage_size: u32,
        outgoing_args_size: u32,
    ) FrameLayout {
        _ = sig;

        // Filter to callee-saved registers
        var callee_saves = BoundedArray(Writable(RealReg), 32){};
        for (clobbered_regs) |reg| {
            if (isCalleeSaved(call_conv, reg.toReg().hwEnc(), reg.toReg().class() == .float)) {
                callee_saves.appendAssumeCapacity(reg);
            }
        }

        // Sort registers for deterministic output
        std.mem.sort(
            Writable(RealReg),
            callee_saves.slice(),
            {},
            struct {
                fn cmp(_: void, a: Writable(RealReg), b: Writable(RealReg)) bool {
                    const a_enc = a.toReg().hwEnc();
                    const b_enc = b.toReg().hwEnc();
                    return a_enc < b_enc;
                }
            }.cmp,
        );

        // Compute clobber size
        var clobber_size: u32 = 0;
        for (callee_saves.constSlice()) |reg| {
            if (reg.toReg().class() == .int) {
                clobber_size += 8; // GPRs are 8 bytes
            } else {
                clobber_size += 16; // XMM are 16 bytes
            }
        }

        // Compute setup area size
        const setup_area_size: u32 = if (flags.preserve_frame_pointers or
            function_calls != .none or
            incoming_args_size > 0 or
            clobber_size > 0 or
            fixed_frame_storage_size > 0)
            8 // Just RBP (return address is pushed by CALL)
        else
            0;

        return FrameLayout{
            .word_bytes = 8,
            .incoming_args_size = incoming_args_size,
            .tail_args_size = tail_args_size,
            .setup_area_size = setup_area_size,
            .clobber_size = clobber_size,
            .fixed_frame_storage_size = fixed_frame_storage_size,
            .stackslots_size = stackslots_size,
            .outgoing_args_size = outgoing_args_size,
            .clobbered_callee_saves = callee_saves,
            .function_calls = function_calls,
        };
    }

    /// Get the return value temporary register.
    pub fn retvalTempReg(call_conv: CallConv) Writable(Reg) {
        _ = call_conv;
        return Writable(Reg).fromReg(r11());
    }

    /// Get exception payload registers.
    /// These are the registers used to pass exception information.
    pub fn exceptionPayloadRegs(call_conv: CallConv) []const Reg {
        _ = call_conv;
        // x64 uses RAX and RDX for exception payload (like return values)
        const static_regs = [_]Reg{ rax(), rdx() };
        return &static_regs;
    }

    /// Generate stack lower bound trap.
    /// Checks if SP is below the limit and traps if so.
    pub fn genStackLowerBoundTrap(limit_reg: Reg) BoundedArray(Inst, 4) {
        var insts = BoundedArray(Inst, 4){};

        // CMP rsp, limit_reg
        insts.appendAssumeCapacity(Inst.cmpRmiR(.{
            .size = .size64,
            .src = .{ .reg = Gpr.fromReg(limit_reg) },
            .dst = Gpr.fromReg(rsp()),
        }));

        // JB trap (jump if below, unsigned)
        insts.appendAssumeCapacity(Inst.trapIf(.{
            .cc = .b, // Below (unsigned less than)
            .trap_code = 0, // STACK_OVERFLOW
        }));

        return insts;
    }

    /// Generate stack probing for large stack frames.
    /// This touches each page to ensure guard pages are hit.
    pub fn genProbestack(frame_size: u32, guard_size: u32) BoundedArray(Inst, 8) {
        var insts = BoundedArray(Inst, 8){};

        if (frame_size <= guard_size) {
            // No probing needed for small frames
            return insts;
        }

        // For large frames, we need to probe each page
        // Use a loop or unroll depending on size
        const probe_count = (frame_size + guard_size - 1) / guard_size;

        if (probe_count <= 4) {
            // Unroll for small number of probes
            const unroll_insts = genProbestackUnroll(guard_size, probe_count);
            for (unroll_insts.constSlice()) |inst_item| {
                insts.appendAssumeCapacity(inst_item);
            }
        } else {
            // Use loop for many probes
            const loop_insts = genProbestackLoop(frame_size, guard_size);
            for (loop_insts.constSlice()) |inst_item| {
                insts.appendAssumeCapacity(inst_item);
            }
        }

        return insts;
    }

    /// Generate unrolled stack probing.
    pub fn genProbestackUnroll(guard_size: u32, probe_count: u32) BoundedArray(Inst, 8) {
        var insts = BoundedArray(Inst, 8){};

        var i: u32 = 0;
        while (i < probe_count and i < 8) : (i += 1) {
            const offset = -@as(i32, @intCast((i + 1) * guard_size));

            // Test memory at [rsp + offset] by writing zero
            // This will trigger guard page if we've gone too far
            insts.appendAssumeCapacity(Inst.movMI(.{
                .size = .size64,
                .dst = SyntheticAmode.real_amode(Amode.immReg(offset, rsp())),
                .imm = 0,
            }));
        }

        return insts;
    }

    /// Generate inline stack probing (selects unroll or loop based on count).
    pub fn genInlineProbestack(guard_size: u32, probe_count: u32) BoundedArray(Inst, 16) {
        var insts = BoundedArray(Inst, 16){};

        if (probe_count <= 4) {
            const unroll_insts = genProbestackUnroll(guard_size, probe_count);
            for (unroll_insts.constSlice()) |inst_item| {
                insts.appendAssumeCapacity(inst_item);
            }
        } else {
            const loop_insts = genProbestackLoop(probe_count * guard_size, guard_size);
            for (loop_insts.constSlice()) |inst_item| {
                insts.appendAssumeCapacity(inst_item);
            }
        }

        return insts;
    }

    /// Generate looped stack probing for very large frames.
    pub fn genProbestackLoop(frame_size: u32, guard_size: u32) BoundedArray(Inst, 8) {
        var insts = BoundedArray(Inst, 8){};

        // Use r11 as scratch for the loop counter
        const scratch = r11();
        const scratch_w = Writable(Gpr).fromReg(Gpr.fromReg(scratch));
        const rsp_gpr = Gpr.fromReg(rsp());

        // mov r11, rsp
        insts.appendAssumeCapacity(Inst.movRR(.{
            .size = .size64,
            .src = rsp_gpr,
            .dst = scratch_w,
        }));

        // sub r11, frame_size (end point)
        insts.appendAssumeCapacity(Inst.aluRmiR(.{
            .size = .size64,
            .op = .sub,
            .src1 = scratch_w.toReg(),
            .src2 = .{ .imm = frame_size },
            .dst = scratch_w,
        }));

        // Loop: probe and decrement rsp by guard_size until we reach r11
        // This is simplified - full implementation would have proper loop structure
        _ = guard_size;

        return insts;
    }
};

//=============================================================================
// Tests
//=============================================================================

test "ABIState System V argument assignment" {
    const testing = std.testing;

    var state = ABIState.init(.system_v);

    // First 6 integer args go to RDI, RSI, RDX, RCX, R8, R9
    try testing.expectEqual(@as(?u8, GprEnc.RDI), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.RSI), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.RDX), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.RCX), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.R8), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.R9), state.nextArgGpr());

    // 7th goes to stack
    try testing.expectEqual(@as(?u8, null), state.nextArgGpr());
}

test "ABIState Windows x64 argument assignment" {
    const testing = std.testing;

    var state = ABIState.init(.windows_fastcall);

    // First 4 args go to RCX, RDX, R8, R9
    try testing.expectEqual(@as(?u8, GprEnc.RCX), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.RDX), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.R8), state.nextArgGpr());
    try testing.expectEqual(@as(?u8, GprEnc.R9), state.nextArgGpr());

    // 5th goes to stack
    try testing.expectEqual(@as(?u8, null), state.nextArgGpr());
}

test "ABIState FPR assignment" {
    const testing = std.testing;

    var state = ABIState.init(.system_v);

    // First 8 FP args go to XMM0-XMM7
    try testing.expectEqual(@as(?u8, XmmEnc.XMM0), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, XmmEnc.XMM1), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, XmmEnc.XMM2), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, XmmEnc.XMM3), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, XmmEnc.XMM4), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, XmmEnc.XMM5), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, XmmEnc.XMM6), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, XmmEnc.XMM7), state.nextArgFpr());
    try testing.expectEqual(@as(?u8, null), state.nextArgFpr());
}

test "isCalleeSaved" {
    const testing = std.testing;

    // System V: RBX, RBP, R12-R15 are callee-saved
    try testing.expect(isCalleeSaved(.system_v, GprEnc.RBX, false));
    try testing.expect(isCalleeSaved(.system_v, GprEnc.RBP, false));
    try testing.expect(isCalleeSaved(.system_v, GprEnc.R12, false));
    try testing.expect(!isCalleeSaved(.system_v, GprEnc.RAX, false));
    try testing.expect(!isCalleeSaved(.system_v, GprEnc.RDI, false));

    // System V: No XMM registers are callee-saved
    try testing.expect(!isCalleeSaved(.system_v, XmmEnc.XMM0, true));

    // Windows x64: XMM6-XMM15 are callee-saved
    try testing.expect(isCalleeSaved(.windows_fastcall, XmmEnc.XMM6, true));
    try testing.expect(!isCalleeSaved(.windows_fastcall, XmmEnc.XMM0, true));
}

test "stack alignment" {
    const testing = std.testing;
    try testing.expectEqual(@as(u32, 16), STACK_ALIGNMENT);
    try testing.expectEqual(@as(u32, 8), RETURN_ADDRESS_SIZE);
    try testing.expectEqual(@as(u32, 32), WIN64_SHADOW_SPACE);
}

test "assignArg" {
    // Basic test for argument assignment
    var state = ABIState.init(.system_v);
    const param = AbiParam{
        .value_type = Type.I64,
        .extension = .none,
        .purpose = .normal,
    };

    const arg = assignArg(&state, param, .args);

    // First integer arg should go to RDI
    switch (arg) {
        .slots => |s| {
            try std.testing.expectEqual(@as(usize, 1), s.slots.len);
            switch (s.slots.get(0)) {
                .reg => |r| {
                    try std.testing.expectEqual(GprEnc.RDI, r.reg.hwEnc());
                    try std.testing.expectEqual(RegClass.int, r.reg.class());
                },
                .stack => unreachable,
            }
        },
    }
}

test "X64MachineDeps basic" {
    const testing = std.testing;

    try testing.expectEqual(@as(u32, 64), X64MachineDeps.wordBits());
    try testing.expectEqual(@as(u32, 16), X64MachineDeps.stackAlign(.system_v));
    try testing.expectEqual(@as(u32, 16), X64MachineDeps.stackAlign(.windows_fastcall));
}

test "X64MachineDeps rcForType" {
    const testing = std.testing;

    const i64_info = X64MachineDeps.rcForType(Type.I64);
    try testing.expectEqual(@as(usize, 1), i64_info.rcs.len);
    try testing.expectEqual(RegClass.int, i64_info.rcs[0]);

    const f64_info = X64MachineDeps.rcForType(Type.F64);
    try testing.expectEqual(@as(usize, 1), f64_info.rcs.len);
    try testing.expectEqual(RegClass.float, f64_info.rcs[0]);

    const i128_info = X64MachineDeps.rcForType(Type.I128);
    try testing.expectEqual(@as(usize, 2), i128_info.rcs.len);
}

test "StackAMode" {
    // Test incoming arg conversion
    const incoming = StackAMode{ .incoming_arg = .{ .off = 8, .stack_args_size = 32 } };
    const amode = incoming.toAmode();
    switch (amode) {
        .incoming_arg => |ia| {
            // stack_args_size(32) + RETURN_ADDRESS_SIZE(8) - off(8) = 32
            try std.testing.expectEqual(@as(u32, 32), ia.offset_val);
        },
        else => unreachable,
    }

    // Test slot conversion
    const slot = StackAMode{ .slot = 16 };
    const slot_amode = slot.toAmode();
    switch (slot_amode) {
        .slot_offset => |off| {
            try std.testing.expectEqual(@as(i32, 16), off.simm32);
        },
        else => unreachable,
    }
}

test "FrameLayout" {
    var layout = FrameLayout{
        .setup_area_size = 8,
        .clobber_size = 24,
        .fixed_frame_storage_size = 32,
        .stackslots_size = 16,
        .outgoing_args_size = 8,
    };

    // Total: 8 + 24 + 32 + 16 + 8 = 88
    try std.testing.expectEqual(@as(u32, 88), layout.frameSize());
}

test "PRegSet" {
    var set = PRegSet.empty();

    // Add RAX
    set.add(PReg.init(GprEnc.RAX, .int));
    try std.testing.expect(set.contains(PReg.init(GprEnc.RAX, .int)));
    try std.testing.expect(!set.contains(PReg.init(GprEnc.RBX, .int)));

    // Add XMM0
    set.add(PReg.init(XmmEnc.XMM0, .float));
    try std.testing.expect(set.contains(PReg.init(XmmEnc.XMM0, .float)));
    try std.testing.expect(!set.contains(PReg.init(XmmEnc.XMM1, .float)));

    // Test union via unionFrom
    var set2 = PRegSet.empty();
    set2.add(PReg.init(GprEnc.RBX, .int));
    var combined = set;
    combined.unionFrom(set2);
    try std.testing.expect(combined.contains(PReg.init(GprEnc.RAX, .int)));
    try std.testing.expect(combined.contains(PReg.init(GprEnc.RBX, .int)));
}

test "DEFAULT_SYSV_CLOBBERS" {
    // Verify System V clobbers include caller-saved registers
    try std.testing.expect(DEFAULT_SYSV_CLOBBERS.contains(PReg.init(GprEnc.RAX, .int)));
    try std.testing.expect(DEFAULT_SYSV_CLOBBERS.contains(PReg.init(GprEnc.RCX, .int)));
    try std.testing.expect(DEFAULT_SYSV_CLOBBERS.contains(PReg.init(GprEnc.RDI, .int)));
    try std.testing.expect(DEFAULT_SYSV_CLOBBERS.contains(PReg.init(GprEnc.R11, .int)));

    // Verify callee-saved are NOT in clobbers
    try std.testing.expect(!DEFAULT_SYSV_CLOBBERS.contains(PReg.init(GprEnc.RBX, .int)));
    try std.testing.expect(!DEFAULT_SYSV_CLOBBERS.contains(PReg.init(GprEnc.R12, .int)));
}

test "alignTo" {
    try std.testing.expectEqual(@as(u32, 16), alignTo(9, 16));
    try std.testing.expectEqual(@as(u32, 16), alignTo(16, 16));
    try std.testing.expectEqual(@as(u32, 32), alignTo(17, 16));
    try std.testing.expectEqual(@as(u32, 8), alignTo(5, 8));
}

test "createRegEnv" {
    const env = createRegEnv(false);

    // Should have 8 preferred int regs (rax, rcx, rdx, rsi, rdi, r8, r9, r10)
    try std.testing.expectEqual(@as(usize, 8), env.preferred_regs_by_class[0].len);

    // Should have 16 preferred float regs (xmm0-xmm15)
    try std.testing.expectEqual(@as(usize, 16), env.preferred_regs_by_class[1].len);

    // Should have 5 non-preferred int regs (rbx, r12-r15)
    try std.testing.expectEqual(@as(usize, 5), env.non_preferred_regs_by_class[0].len);

    // r11 should be scratch
    try std.testing.expect(env.scratch_by_class[0] != null);
    try std.testing.expectEqual(@as(u8, GprEnc.R11), env.scratch_by_class[0].?.hwEnc());
}
