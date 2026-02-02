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
            const rreg = RealReg{
                .hw_enc_val = fpr_enc,
                .cls = .float,
            };
            return ABIArg.reg(rreg, ty, extension, purpose);
        }
    } else {
        // Integer/pointer type
        if (state.nextArgGpr()) |gpr_enc| {
            const rreg = RealReg{
                .hw_enc_val = gpr_enc,
                .cls = .int,
            };
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
            const rreg = RealReg{
                .hw_enc_val = fpr_enc,
                .cls = .float,
            };
            return ABIArg.reg(rreg, ty, extension, .normal);
        }
    } else {
        // Integer return in RAX/RDX
        switch (call_conv) {
            .system_v, .wasmtime_system_v, .probestack, .tail, .winch => {
                if (ret_idx < SYSV_RET_GPRS.len) {
                    const gpr_enc = SYSV_RET_GPRS[ret_idx];
                    const rreg = RealReg{
                        .hw_enc_val = gpr_enc,
                        .cls = .int,
                    };
                    return ABIArg.reg(rreg, ty, extension, .normal);
                }
            },
            .windows_fastcall, .wasmtime_fastcall => {
                if (ret_idx == 0) {
                    const rreg = RealReg{
                        .hw_enc_val = WIN64_RET_GPR,
                        .cls = .int,
                    };
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
        .value_type = Type.i64,
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
                    try std.testing.expectEqual(GprEnc.RDI, r.reg.hw_enc_val);
                    try std.testing.expectEqual(RegClass.int, r.reg.cls);
                },
                .stack => unreachable,
            }
        },
    }
}
