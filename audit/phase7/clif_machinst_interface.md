# CLIF IR Interface Required by machinst Modules

## Purpose

This document specifies exactly which CLIF IR types and methods are used by
the machinst modules (lower.zig, blockorder.zig). This is the contract that
must be satisfied when replacing stub types with real CLIF imports.

## Cranelift Source Reference

**lower.rs imports** (lines 11-15):
```rust
use crate::ir::{
    ArgumentPurpose, Block, BlockArg, Constant, ConstantData, DataFlowGraph, ExternalName,
    Function, GlobalValue, GlobalValueData, Immediate, Inst, InstructionData, MemFlags,
    RelSourceLoc, SigRef, Signature, Type, Value, ValueDef, ValueLabelAssignments, ValueLabelStart,
};
```

**blockorder.rs imports** (line 66):
```rust
use crate::ir::{Block, Function, Inst, Opcode};
```

## Required Entity Types

### Block (dfg.zig)
```zig
pub const Block = struct {
    index: u32,
    pub fn fromIndex(u32) Block;
    pub fn asU32(self) u32;
    pub fn eql(self, other: Block) bool;
};
```
**Cot CLIF**: `ir/clif/dfg.zig` line 17 - MATCHES

### Inst (dfg.zig)
```zig
pub const Inst = struct {
    index: u32,
    pub fn fromIndex(u32) Inst;
    pub fn asU32(self) u32;
    pub fn eql(self, other: Inst) bool;
};
```
**Cot CLIF**: `ir/clif/dfg.zig` line 73 - MATCHES

### Value (dfg.zig)
```zig
pub const Value = struct {
    index: u32,
    pub fn fromIndex(u32) Value;
    pub fn asU32(self) u32;
    pub fn eql(self, other: Value) bool;
};
```
**Cot CLIF**: `ir/clif/dfg.zig` line 45 - MATCHES

## Required Layout Interface

### Layout (layout.zig)
```zig
pub const Layout = struct {
    pub fn entryBlock(self) ?Block;
    pub fn blocks(self) BlockIterator;
    pub fn lastInst(self, block: Block) ?Inst;
    pub fn isBlockCold(self, block: Block) bool;  // Note: stub uses isCold()
};
```
**Cot CLIF**: `ir/clif/layout.zig` line 96 - MATCHES (with `isBlockCold` naming)

## Required DataFlowGraph Interface

### DataFlowGraph (dfg.zig)
```zig
pub const DataFlowGraph = struct {
    pub fn getInstData(self, inst: Inst) InstData;
    // InstData has .opcode field
};
```
**Cot CLIF**: `ir/clif/dfg.zig` line 400 - MATCHES

## Required Function Interface

### Function (function.zig)
```zig
pub const Function = struct {
    dfg: DataFlowGraph,
    layout: Layout,
    // Other fields: name, signature, stack_slots, etc.
};
```
**Cot CLIF**: `ir/clif/function.zig` line 377 - MATCHES

## Required Opcode Interface

### Opcode (instructions.zig)
```zig
pub const Opcode = enum {
    jump, brif, br_table, @"return", trap, ...
    pub fn isBranch(self) bool;
    pub fn isTerminator(self) bool;
};
```
**Cot CLIF**: `ir/clif/instructions.zig` - MATCHES

## Naming Differences to Address

| Stub Name | CLIF Name | Location |
|-----------|-----------|----------|
| `Layout.isCold(block)` | `Layout.isBlockCold(block)` | blockorder.zig |
| `InstructionData` | `InstData` | blockorder.zig |

## Summary

The CLIF types in `ir/clif/` fully satisfy the interface requirements for
machinst modules. Only two naming adjustments are needed:

1. Change `isCold(block)` calls to `isBlockCold(block)`
2. Change `InstructionData` references to `InstData`

## Implementation Checklist

- [x] Block - CLIF matches stub interface
- [x] Inst - CLIF matches stub interface
- [x] Value - CLIF matches stub interface
- [x] Layout - CLIF matches (with isBlockCold naming)
- [x] DataFlowGraph - CLIF matches stub interface
- [x] Function - CLIF matches stub interface
- [x] Opcode - CLIF matches stub interface
- [x] blockorder.zig - Replaced stubs with CLIF imports (2026-02-02)
- [x] lower.zig - Replaced stubs with CLIF imports (2026-02-02)

## Types Still Stubbed (Not Yet in CLIF)

The following types are still defined locally in lower.zig as they are not
yet part of the CLIF IR implementation:

- `GlobalValue`, `GlobalValueData` - Global value references
- `Constant`, `ConstantData`, `ConstantPool` - Constant handling
- `Immediate` - Immediate value references
- `ExceptionTableData`, `ExceptionTable` - Exception handling
- `BlockArg` - Block argument types for try/catch
- `ValueLabelAssignments`, `ValueLabelStart`, `ValueLabel` - Debug info
- `RelSourceLocMap` - Source location mapping
- `ReverseInstIterator` - Reverse iteration helper
- `Fact`, `Flags` - Proof-carrying code and compilation flags
