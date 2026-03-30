//! Cranelift Intermediate Format (CLIF) IR.
//!
//! This module provides the intermediate representation used for
//! lowering to native machine code. It is a port of Cranelift's IR
//! from cranelift/codegen/src/ir/.
//!
//! The main types are:
//! - `Function` - A complete function with signature, blocks, and instructions
//! - `DataFlowGraph` - SSA values, blocks, and value definitions
//! - `Layout` - Ordering of blocks and instructions
//! - `Type` - SSA value types (I8, I32, I64, F32, F64, vectors)
//! - `Opcode` - Instruction opcodes
//!
//! Usage:
//! ```zig
//! const clif = @import("compiler/ir/clif/mod.zig");
//! var func = clif.Function.init(allocator);
//! defer func.deinit();
//! ```

const std = @import("std");

// =============================================================================
// Sub-modules
// =============================================================================

pub const types_mod = @import("types.zig");
pub const instructions_mod = @import("instructions.zig");
pub const dfg_mod = @import("dfg.zig");
pub const layout_mod = @import("layout.zig");
pub const function_mod = @import("function.zig");
pub const builder_mod = @import("builder.zig");
pub const jumptable_mod = @import("jumptable.zig");
pub const globalvalue_mod = @import("globalvalue.zig");

// =============================================================================
// Type System (from types.zig)
// =============================================================================

pub const Type = types_mod.Type;

// Type constants
pub const LANE_BASE = types_mod.LANE_BASE;
pub const REFERENCE_BASE = types_mod.REFERENCE_BASE;
pub const VECTOR_BASE = types_mod.VECTOR_BASE;
pub const DYNAMIC_VECTOR_BASE = types_mod.DYNAMIC_VECTOR_BASE;

// =============================================================================
// Condition Codes and Opcodes (from instructions.zig)
// =============================================================================

pub const IntCC = instructions_mod.IntCC;
pub const FloatCC = instructions_mod.FloatCC;
pub const TrapCode = instructions_mod.TrapCode;
pub const Opcode = instructions_mod.Opcode;

// =============================================================================
// Entity References (from dfg.zig)
// =============================================================================

pub const Block = dfg_mod.Block;
pub const Value = dfg_mod.Value;
pub const Inst = dfg_mod.Inst;
pub const StackSlot = dfg_mod.StackSlot;
pub const FuncRef = dfg_mod.FuncRef;
pub const SigRef = dfg_mod.SigRef;
pub const JumpTable = dfg_mod.JumpTable;
pub const GlobalValue = dfg_mod.GlobalValue;

// =============================================================================
// Value Management (from dfg.zig)
// =============================================================================

pub const ValueList = dfg_mod.ValueList;
pub const ValueListPool = dfg_mod.ValueListPool;
pub const ValueDef = dfg_mod.ValueDef;
pub const ValueData = dfg_mod.ValueData;
pub const BlockData = dfg_mod.BlockData;
pub const DataFlowGraph = dfg_mod.DataFlowGraph;
pub const InstValuesIterator = dfg_mod.InstValuesIterator;

// =============================================================================
// Layout (from layout.zig)
// =============================================================================

pub const Layout = layout_mod.Layout;
pub const BlockIterator = layout_mod.BlockIterator;
pub const InstIterator = layout_mod.InstIterator;
pub const SequenceNumber = layout_mod.SequenceNumber;

// =============================================================================
// Function (from function.zig)
// =============================================================================

pub const CallConv = function_mod.CallConv;
pub const ArgumentExtension = function_mod.ArgumentExtension;
pub const ArgumentPurpose = function_mod.ArgumentPurpose;
pub const AbiParam = function_mod.AbiParam;
pub const Signature = function_mod.Signature;
pub const StackSize = function_mod.StackSize;
pub const StackSlotKind = function_mod.StackSlotKind;
pub const StackSlotData = function_mod.StackSlotData;
pub const ExternalName = function_mod.ExternalName;
pub const ExtFuncData = function_mod.ExtFuncData;
pub const Function = function_mod.Function;

// =============================================================================
// Instruction Builder (from builder.zig)
// =============================================================================

pub const InstructionFormat = builder_mod.InstructionFormat;
pub const InstructionData = builder_mod.InstructionData;
pub const MemFlags = builder_mod.MemFlags;
pub const FuncBuilder = builder_mod.FuncBuilder;

// =============================================================================
// Jump Tables (from jumptable.zig)
// =============================================================================

pub const BlockCall = jumptable_mod.BlockCall;
pub const JumpTableData = jumptable_mod.JumpTableData;
pub const JumpTables = jumptable_mod.JumpTables;

// =============================================================================
// Global Values (from globalvalue.zig)
// Port of cranelift/codegen/src/ir/globalvalue.rs
// =============================================================================

pub const GlobalValueData = globalvalue_mod.GlobalValueData;
pub const Offset32 = globalvalue_mod.Offset32;
pub const Imm64 = globalvalue_mod.Imm64;

// =============================================================================
// Tests
// =============================================================================

test "clif module exports" {
    const testing = std.testing;

    // Test type exports
    try testing.expect(Type.I32.isInt());
    try testing.expect(Type.F64.isFloat());

    // Test condition codes
    try testing.expect(IntCC.eq.complement() == .ne);
    try testing.expect(FloatCC.lt.swapArgs() == .gt);

    // Test opcodes
    try testing.expect(Opcode.jump.isBranch());
    try testing.expect(!Opcode.iadd.isBranch());

    // Test entity refs
    const v = Value.fromIndex(42);
    try testing.expect(v.asU32() == 42);

    const b = Block.fromIndex(5);
    try testing.expect(b.asU32() == 5);
}

test "clif types integration" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a function
    var func = Function.init(allocator);
    defer func.deinit();

    // Create a block
    const block0 = try func.dfg.makeBlock();
    try testing.expect(func.dfg.blockIsValid(block0));

    // Add a parameter
    const param0 = try func.dfg.appendBlockParam(block0, Type.I64);
    try testing.expect(func.dfg.valueType(param0).eql(Type.I64));

    // Add to layout
    try func.layout.appendBlock(allocator, block0);
    try testing.expect(func.layout.entryBlock() != null);
}
