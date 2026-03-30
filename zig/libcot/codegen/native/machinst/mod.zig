//! Machinst module - Machine instruction infrastructure.
//!
//! This module provides the core types and infrastructure for machine code
//! generation, ported from Cranelift's machinst module.

const std = @import("std");

// Re-export sub-modules
pub const reg = @import("reg.zig");
pub const inst = @import("inst.zig");
pub const vcode = @import("vcode.zig");
pub const abi = @import("abi.zig");
pub const buffer = @import("buffer.zig");
pub const blockorder = @import("blockorder.zig");
pub const lower = @import("lower.zig");
pub const regalloc_adapter = @import("regalloc_adapter.zig");

// Re-export commonly used types from reg.zig
pub const RegClass = reg.RegClass;
pub const PReg = reg.PReg;
pub const VReg = reg.VReg;
pub const Reg = reg.Reg;
pub const RealReg = reg.RealReg;
pub const Writable = reg.Writable;
pub const PRegSet = reg.PRegSet;
pub const OperandCollector = reg.OperandCollector;
pub const Operand = reg.Operand;
pub const OperandConstraint = reg.OperandConstraint;
pub const OperandKind = reg.OperandKind;
pub const OperandPos = reg.OperandPos;
pub const SpillSlot = reg.SpillSlot;

// Re-export commonly used types from inst.zig
pub const MachLabel = inst.MachLabel;
pub const Type = inst.Type;
pub const CallInfo = inst.CallInfo;
pub const ArgPair = inst.ArgPair;
pub const RetPair = inst.RetPair;
pub const CallArgPair = inst.CallArgPair;
pub const CallRetPair = inst.CallRetPair;
pub const CallArgList = inst.CallArgList;
pub const TryCallInfo = inst.TryCallInfo;
pub const MachTerminator = inst.MachTerminator;
pub const CallType = inst.CallType;

// Re-export commonly used types from vcode.zig
pub const VCodeBuilder = vcode.VCodeBuilder;
pub const VRegAllocator = vcode.VRegAllocator;
pub const VCodeConstants = vcode.VCodeConstants;
pub const InsnIndex = vcode.InsnIndex;
pub const BlockIndex = vcode.BlockIndex;
pub const InsnRange = vcode.InsnRange;

// Re-export regalloc adapter
pub const VCodeRegallocAdapter = regalloc_adapter.VCodeRegallocAdapter;

// Re-export commonly used types from abi.zig
pub const ABIArg = abi.ABIArg;
pub const ABIArgSlot = abi.ABIArgSlot;
pub const StackAMode = abi.StackAMode;
pub const Sig = abi.Sig;
pub const SigSet = abi.SigSet;
pub const FrameLayout = abi.FrameLayout;
pub const CallConv = abi.CallConv;

// Re-export commonly used types from buffer.zig
pub const MachBuffer = buffer.MachBuffer;
pub const MachBufferFinalized = buffer.MachBufferFinalized;
pub const MachTextSectionBuilder = buffer.MachTextSectionBuilder;

// Re-export commonly used types from blockorder.zig
pub const BlockLoweringOrder = blockorder.BlockLoweringOrder;
pub const LoweredBlock = blockorder.LoweredBlock;

// Re-export commonly used types from lower.zig
pub const Lower = lower.Lower;
pub const InstColor = lower.InstColor;
pub const ValueUseState = lower.ValueUseState;
pub const InputSourceInst = lower.InputSourceInst;
pub const NonRegInput = lower.NonRegInput;
pub const RelocDistance = lower.RelocDistance;
pub const ValueRegs = lower.ValueRegs;
pub const InstOutput = lower.InstOutput;

// Re-export CLIF IR types from lower.zig (which imports from compiler/ir/clif/)
pub const Block = lower.Block;
pub const Value = lower.Value;
pub const Inst = lower.Inst;
pub const StackSlot = lower.StackSlot;
pub const FuncRef = lower.FuncRef;
pub const SigRef = lower.SigRef;
pub const JumpTable = lower.JumpTable;
pub const Opcode = lower.Opcode;
pub const IntCC = lower.IntCC;
pub const FloatCC = lower.FloatCC;
pub const TrapCode = lower.TrapCode;
pub const InstructionData = lower.InstructionData;
pub const ClifType = lower.Type;

// Additional types that may be used by backends
pub const MemFlags = struct {
    aligned: bool = true,
    readonly: bool = false,
    trap: bool = false,
    notrap: bool = false,

    pub const empty = MemFlags{};

    pub fn isAligned(self: MemFlags) bool {
        return self.aligned;
    }

    pub fn isReadonly(self: MemFlags) bool {
        return self.readonly;
    }

    pub fn canTrap(self: MemFlags) bool {
        return self.trap and !self.notrap;
    }
};

test "machinst module compiles" {
    const testing = std.testing;
    // Just verify the re-exports work
    _ = MachLabel.init(0);
    _ = PReg.init(0, .int);
    _ = VReg.init(0, .int);
    try testing.expect(true);
}
