//! x86-64 ISA backend.
//!
//! This module implements the x86-64 (AMD64) instruction set architecture
//! backend, ported from Cranelift's `cranelift/codegen/src/isa/x64/`.

const std = @import("std");

// Sub-modules
pub const inst = @import("inst/mod.zig");
pub const regs = inst.regs;
pub const args = inst.args;
pub const emit = inst.emit;
pub const abi = @import("abi.zig");
pub const lower = @import("lower.zig");

// Re-export instruction types
pub const Inst = inst.Inst;
pub const AluRmiROpcode = inst.AluRmiROpcode;
pub const UnaryRmROpcode = inst.UnaryRmROpcode;
pub const SseOpcode = inst.SseOpcode;
pub const AvxOpcode = inst.AvxOpcode;
pub const ShiftKind = inst.ShiftKind;
pub const TrapCode = inst.TrapCode;
pub const CC = inst.CC;

// Re-export size types
pub const OperandSize = inst.OperandSize;
pub const ExtMode = inst.ExtMode;
pub const ExtKind = inst.ExtKind;

// Re-export register types
pub const Reg = inst.Reg;
pub const PReg = inst.PReg;
pub const VReg = inst.VReg;
pub const RealReg = inst.RealReg;
pub const RegClass = inst.RegClass;
pub const Gpr = inst.Gpr;
pub const Xmm = inst.Xmm;
pub const WritableGpr = inst.WritableGpr;
pub const WritableXmm = inst.WritableXmm;
pub const Writable = inst.Writable;

// Re-export addressing types
pub const Amode = inst.Amode;
pub const SyntheticAmode = inst.SyntheticAmode;
pub const RegMem = inst.RegMem;
pub const RegMemImm = inst.RegMemImm;
pub const GprMem = inst.GprMem;
pub const GprMemImm = inst.GprMemImm;
pub const XmmMem = inst.XmmMem;
pub const XmmMemImm = inst.XmmMemImm;

// Re-export other types
pub const MachLabel = inst.MachLabel;
pub const MemFlags = inst.MemFlags;
pub const Type = inst.Type;
pub const CallInfo = inst.CallInfo;
pub const CallInfoUnknown = inst.CallInfoUnknown;
pub const ExternalName = inst.ExternalName;
pub const CallConv = inst.CallConv;

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

// Re-export emission types
pub const MachBuffer = emit.MachBuffer;
pub const EmitState = emit.EmitState;

// Re-export lowering backend
pub const X64LowerBackend = lower.X64LowerBackend;

// Settings (placeholder for now)
pub const Settings = struct {
    /// Enable AVX instructions.
    use_avx: bool = false,
    /// Enable AVX-512 instructions.
    use_avx512: bool = false,
    /// Enable BMI1 instructions.
    use_bmi1: bool = false,
    /// Enable BMI2 instructions.
    use_bmi2: bool = false,
    /// Enable FMA instructions.
    use_fma: bool = false,
    /// Enable LZCNT instruction.
    use_lzcnt: bool = false,
    /// Enable POPCNT instruction.
    use_popcnt: bool = false,
    /// Enable SSE4.1 instructions.
    use_sse41: bool = false,
    /// Enable SSE4.2 instructions.
    use_sse42: bool = false,
    /// Enable SSSE3 instructions.
    use_ssse3: bool = false,

    pub const default = Settings{};
};

test "x64 module compiles" {
    const testing = std.testing;
    // Verify basic types work
    const ax = rax();
    try testing.expect(ax.class() == .int);

    const x0 = xmm0();
    try testing.expect(x0.class() == .float);

    // Test condition code inversion (z = zero/equal, nz = not zero/not equal)
    const cond = CC.z;
    try testing.expect(cond.invert() == CC.nz);
}
