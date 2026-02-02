# blockorder.zig CLIF Integration - Full Port Documentation

## Errors to Fix

### Error 1: Missing `return_call` Opcode
**Status**: FIXED
- Location: blockorder.zig:348
- Issue: `return_call` and `return_call_indirect` don't exist in CLIF Opcode enum
- Fix: Remove these from the switch case

### Error 2: Test Function Creation
**Status**: PENDING
- Location: blockorder.zig:953, 1017, 1079
- Issue: Tests create `Function{}` but CLIF Function requires all fields
- Fix: Use `Function.init(allocator)` instead

### Error 3: Test Uses Stub-Specific APIs
**Status**: PENDING
- Tests use fields/methods that don't exist in real CLIF:
  - `func.layout.block_list` (stub) → `func.layout.appendBlock()` (CLIF)
  - `func.dfg.instructions` (stub) → `func.dfg.makeInstWithData()` (CLIF)
  - `func.layout.last_insts` (stub) → instructions added via `layout.appendInst()` (CLIF)
  - Stub InstData had `.dest`, `.then_dest`, `.else_dest` → CLIF uses different encoding

## CLIF API Reference

### Creating a Function
```zig
var func = Function.init(allocator);
defer func.deinit();
```

### Adding Blocks
```zig
// Create block in DFG
const block = func.dfg.makeBlock();
// Add to layout
try func.layout.appendBlock(allocator, block);
```

### Creating Instructions
```zig
// Create instruction with data
const inst = try func.dfg.makeInstWithData(.{
    .opcode = .jump,
    .args = ValueList.EMPTY,
    .ctrl_type = Type.INVALID,
});
// Add to layout in a block
try func.layout.appendInst(allocator, inst, block);
```

### Getting Last Instruction
```zig
const last = func.layout.lastInst(block);
```

### Getting Instruction Data
```zig
const data = func.dfg.getInstData(inst);
```

## Test Rewrite Plan

### Test 1: "DominatorTree RPO iterator"
Need to:
1. Use `Function.init(allocator)`
2. Create blocks with `dfg.makeBlock()` + `layout.appendBlock()`
3. Create instructions with `dfg.makeInstWithData()` + `layout.appendInst()`
4. Branch destinations need proper encoding in InstData

### Test 2: "BlockLoweringOrder simple linear CFG"
Same changes as Test 1.

### Test 3: "BlockLoweringOrder diamond CFG no critical edges"
Same changes as Test 1, plus handle `brif` instruction properly.

## Branch Destination Encoding

blockorder.zig uses these methods on InstData:
- `getBlockDest()` - for jump instruction
- `getBrifDests()` - for brif instruction (returns then_dest, else_dest)
- `getBrTableData()` - for br_table instruction (returns default + targets)

These methods do NOT exist in CLIF InstData. Need to add them.

### Required CLIF Enhancement: Add Branch Destination Methods to InstData

The InstData struct needs to store branch destinations. Options:
1. Add `dest: ?Block` field for jump
2. Add `then_dest: ?Block, else_dest: ?Block` fields for brif
3. Add reference to JumpTableData for br_table

Cranelift uses InstructionData with full format support. Our simplified InstData
needs extension to support CFG construction.

## Implementation Checklist

### Completed
- [x] Remove `return_call` from opcode switch (blockorder.zig)
- [x] Verify `makeBlock()` method exists in DFG
- [x] Add `dest: ?Block` field to InstData for jump destinations
- [x] Add `then_dest: ?Block, else_dest: ?Block` fields for brif
- [x] Add `getBlockDest()` method to InstData
- [x] Add `getBrifDests()` method to InstData
- [x] Add `getBrTableData()` method to InstData (stub for now)
- [x] Rewrite test "DominatorTree RPO iterator" to use CLIF APIs
- [x] Rewrite test "BlockLoweringOrder simple linear CFG" to use CLIF APIs
- [x] Rewrite test "BlockLoweringOrder diamond CFG" to use CLIF APIs
- [x] Add regClass() method to CLIF Type
- [x] Add regClass() method to inst.zig Type
- [x] Update vcode.zig to use CLIF Type
- [x] Fix VRegAllocator.alloc call with RegClass parameter
- [x] Fix return_ -> @"return" in CLIF Opcode usage
- [x] Add alias case to ValueDef switch in lower.zig
- [x] Fix MachLabel.fromU32 -> MachLabel.init
- [x] Fix maybePinnedReg return type in aarch64 lower.zig
- [x] Replace std.BoundedArray with abi_mod.BoundedArray

### Remaining
- [ ] Add blockOrder() method to VCodeBuilder OR refactor lower() to use stored block_order
- [ ] Verify VCodeBuilder has all required methods for lower.zig
- [ ] Ensure all tests pass
- [ ] Wire compiler.zig to use compile_mod.compile()
