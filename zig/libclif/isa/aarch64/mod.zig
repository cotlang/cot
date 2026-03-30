//! AArch64 ISA backend.
//!
//! This module implements the AArch64 (ARM64) instruction set architecture
//! backend, ported from Cranelift's `cranelift/codegen/src/isa/aarch64/`.

const std = @import("std");

// Sub-modules
pub const inst = @import("inst/mod.zig");
pub const lower = @import("lower.zig");
pub const abi = @import("abi.zig");
pub const regs = inst.regs;
pub const imms = inst.imms;
pub const args = inst.args;

// Re-export lowering backend
pub const AArch64LowerBackend = lower.AArch64LowerBackend;

// Re-export ABI types
pub const AArch64MachineDeps = abi.AArch64MachineDeps;
pub const CallConv = abi.CallConv;
pub const ArgsOrRets = abi.ArgsOrRets;
pub const FrameLayout = abi.FrameLayout;
pub const ABIArg = abi.ABIArg;
pub const ABIArgSlot = abi.ABIArgSlot;
pub const AbiParam = abi.AbiParam;
pub const StackAMode = abi.StackAMode;
pub const PRegSet = abi.PRegSet;
pub const MachineEnv = abi.MachineEnv;
pub const IsaFlags = abi.IsaFlags;
pub const SettingsFlags = abi.SettingsFlags;
pub const Signature = abi.Signature;
pub const createRegEnv = abi.createRegEnv;

// Re-export instruction types
pub const Inst = inst.Inst;
pub const ALUOp = inst.ALUOp;
pub const ALUOp3 = inst.ALUOp3;
pub const BitOp = inst.BitOp;
pub const FPUOp1 = inst.FPUOp1;
pub const FPUOp2 = inst.FPUOp2;
pub const FPUOp3 = inst.FPUOp3;
pub const FpuRoundMode = inst.FpuRoundMode;
pub const FpuToIntOp = inst.FpuToIntOp;
pub const IntToFpuOp = inst.IntToFpuOp;
pub const MoveWideOp = inst.MoveWideOp;
pub const AtomicRMWOp = inst.AtomicRMWOp;
pub const AtomicRMWLoopOp = inst.AtomicRMWLoopOp;
pub const VecALUOp = inst.VecALUOp;
pub const VecMisc2 = inst.VecMisc2;
pub const AMode = inst.AMode;
pub const PairAMode = inst.PairAMode;

// Re-export size types
pub const OperandSize = inst.OperandSize;
pub const ScalarSize = inst.ScalarSize;
pub const VectorSize = inst.VectorSize;

// Re-export condition types
pub const Cond = inst.Cond;
pub const CondBrKind = inst.CondBrKind;
pub const BranchTarget = inst.BranchTarget;

// Re-export immediate types
pub const NZCV = inst.NZCV;
pub const UImm5 = inst.UImm5;
pub const SImm9 = inst.SImm9;
pub const Imm12 = inst.Imm12;
pub const ImmLogic = inst.ImmLogic;
pub const ImmShift = inst.ImmShift;
pub const MoveWideConst = inst.MoveWideConst;

// Re-export register utilities
pub const xreg = regs.xreg;
pub const vreg = regs.vreg;
pub const zeroReg = regs.zeroReg;
pub const stackReg = regs.stackReg;
pub const linkReg = regs.linkReg;
pub const fpReg = regs.fpReg;
pub const spilltmpReg = regs.spilltmpReg;
pub const prettyPrintReg = regs.prettyPrintReg;
pub const prettyPrintIreg = regs.prettyPrintIreg;

// Settings (placeholder for now)
pub const Settings = struct {
    /// Use pointer authentication (PAC).
    use_pac: bool = false,
    /// Use branch target identification (BTI).
    use_bti: bool = false,
    /// Use LSE atomics (ARMv8.1+).
    use_lse: bool = false,
    /// Use half-precision floating point (ARMv8.2+).
    use_fp16: bool = false,
    /// Use dot product (ARMv8.4+).
    use_dotprod: bool = false,

    pub const default = Settings{};
};

test "aarch64 module compiles" {
    const testing = std.testing;
    // Verify basic types work
    const x0 = xreg(0);
    try testing.expect(x0.class() == .int);

    const v0 = vreg(0);
    try testing.expect(v0.class() == .float);

    const cond = Cond.eq;
    try testing.expect(cond.invert() == Cond.ne);
}
