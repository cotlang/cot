# Native Pipeline Fixes - Execution Plan

**Date**: February 2026
**Status**: Blocking issues preventing Cot → Wasm → Native compilation

## Executive Summary

Two bugs are blocking the native compilation pipeline:

1. **arc.zig**: `generateStubBody` emits `i64.const 0` for void functions, causing Wasm validation failure
2. **translator.zig**: Blocks created but never used remain in DFG but not Layout, causing lowering panic

---

## Task List

| ID | Task | Priority | Status | Reference |
|----|------|----------|--------|-----------|
| T1 | Fix generateStubBody for void functions | P0 | ✅ DONE | Swift HeapObject.cpp |
| T2 | Fix unused blocks in DFG not in Layout | P0 | TODO | Cranelift frontend.rs |
| T2b | Fix br_table successor collection overflow | P0 | PARTIAL | Cranelift lower.rs |
| T2c | Fix lowerBranch for trap terminator | P0 | PARTIAL | Cranelift aarch64/lower.rs |
| T3 | Fix memory leak in function call test | P2 | TODO | - |
| T4 | Un-skip memory operations test | P1 | TODO | - |

---

## T1: Fix generateStubBody for Void Functions

### Broken Code (arc.zig:216-225)

```zig
/// Generates a stub function body that returns 0 (for i64 returns) or does nothing (for void)
/// Used for unimplemented functions
fn generateStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    // Return 0 for any return type (harmless for void returns)  <-- WRONG!
    try code.emitI64Const(0);
    // Note: finish() adds the end opcode automatically

    return try code.finish();
}
```

**Problem**: The comment says "harmless for void returns" but this is **incorrect**. Wasm validation requires the stack to be empty at function end for void functions. Emitting `i64.const 0` leaves a value on the stack.

**Error**: `WebAssembly.instantiate(): Compiling function #5 failed: expected 0 elements on the stack for fallthru, found 1`

### Swift Reference (HeapObject.cpp)

Swift void functions simply have empty bodies or early returns with `return;`:

```cpp
// From ~/learning/swift/stdlib/public/runtime/HeapObject.cpp
void swift::swift_deallocBox(HeapObject *o) {
  auto metadata = static_cast<const GenericBoxHeapMetadata *>(o->metadata);
  o->refCounts.decrementFromOneNonAtomic();
  swift_deallocObject(o, metadata->getAllocSize(),
                      metadata->getAllocAlignMask());
  // No explicit return - void function
}

void swift::swift_nonatomic_unownedRelease(HeapObject *object) {
  if (!isValidPointerForNativeRetain(object))
    return;  // Early return for void function - no value
  // ...
}
```

### Fix

Create two stub body generators - one for void, one for non-void:

```zig
/// Generates a stub function body for void functions (empty body)
fn generateVoidStubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    // Empty body - just the end opcode (added by finish())
    return try code.finish();
}

/// Generates a stub function body for functions returning i64 (returns 0)
fn generateI64StubBody(allocator: std.mem.Allocator) ![]const u8 {
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();
    try code.emitI64Const(0);
    return try code.finish();
}
```

Then update callers:

```zig
// For memset_zero (void return):
const memset_zero_body = try generateVoidStubBody(allocator);

// For append (i64 return):
const append_body = try generateI64StubBody(allocator);
```

---

## T2: Fix Blocks in DFG Not in Layout

### Broken Behavior

Debug output shows:
```
driver: translated function 6 to CLIF (7 blocks, 8 insts)
DFG blocks: 7
Layout blocks: 6
Blocks in DFG but NOT in Layout:
  - Block 1
Status: INCONSISTENT - blocks missing from Layout!
```

Then lowering panics: `Unimplemented branch lowering`

### Root Cause

In `translator.zig:initializeFunction`, the exit block is created but never switched to if all code paths are unreachable:

```zig
pub fn initializeFunction(...) !void {
    const entry_block = try self.builder.createBlock();  // Block 0
    const exit_block = try self.builder.createBlock();   // Block 1 - NEVER USED!

    // ... entry block is used ...

    // exit_block is pushed to control stack but may never be jumped to
    try self.state.initialize(exit_block, num_returns);
}
```

In `translateEnd`, the exit block is only switched to if reachable:

```zig
pub fn translateEnd(self: *Self) !void {
    // ...
    if (next_reachable) {
        self.builder.switchToBlock(next_block);  // Only if reachable!
        try self.builder.ensureInsertedBlock();
        // ...
    }
}
```

### Cranelift Reference (frontend.rs)

Cranelift handles this by NOT requiring all DFG blocks to be in Layout. Dead blocks are simply not included. The key is in `ensure_inserted_block()`:

```rust
// From ~/learning/wasmtime/cranelift/frontend/src/frontend.rs:609-622
pub fn ensure_inserted_block(&mut self) {
    let block = self.position.unwrap();
    if self.is_pristine(block) {
        if !self.func.layout.is_block_inserted(block) {
            self.func.layout.append_block(block);  // Only adds if pristine
        }
        self.func_ctx.status[block] = BlockStatus::Partial;
    }
}
```

Cranelift's lowering also handles missing blocks gracefully - it only iterates over blocks in the Layout, not all blocks in DFG.

### Fix Options

**Option A: Clean up unused blocks before lowering** (Recommended)

Add a cleanup pass after translation that removes blocks from DFG that aren't in Layout:

```zig
// In driver.zig after translation:
pub fn cleanupUnusedBlocks(func: *clif.Function) void {
    // Iterate DFG blocks and remove any not in Layout
    var to_remove = std.ArrayList(clif.Block).init(func.dfg.allocator);
    defer to_remove.deinit();

    for (func.dfg.blocks.items, 0..) |_, idx| {
        const block = clif.Block.fromIndex(@intCast(idx));
        if (!func.layout.isBlockInserted(block)) {
            to_remove.append(block) catch continue;
        }
    }

    for (to_remove.items) |block| {
        func.dfg.removeBlock(block);
    }
}
```

**Option B: Ensure exit block is always in Layout**

Always switch to exit block at function finalization:

```zig
// In translator.zig, add finalize method:
pub fn finalizeFunction(self: *Self) !void {
    // Ensure exit block is in layout even if unreachable
    const exit_block = self.state.getExitBlock();
    if (!self.builder.func.layout.isBlockInserted(exit_block)) {
        self.builder.switchToBlock(exit_block);
        try self.builder.ensureInsertedBlock();
        // Add unreachable terminator
        _ = try self.builder.ins().trap(.unreachable_code);
    }
}
```

**Option C: Make lowering tolerate missing blocks**

Check if block is in Layout before lowering:

```zig
// In machinst/lower.zig:
fn lower(...) !VCode {
    var layout_iter = func.layout.blockIterator();
    while (layout_iter.next()) |block| {
        // Only lower blocks that are in Layout
        try self.lowerClifBlock(block);
    }
}
```

This is actually already the case - the bug is that a branch instruction references a block not in Layout. The fix is Option A or B.

---

## T3: Fix Memory Leak in Function Call Test

### Location

`compiler/codegen/native/compile.zig` - test "V2: compile function call produces correct code"

### Issue

The test leaks memory according to Zig's testing allocator. The leaked strings show error messages from other tests, suggesting state pollution.

### Investigation Needed

1. Check if `ext_sig` needs explicit cleanup
2. Check if `call_result` has any allocations
3. Verify all defers are properly set up

---

## T4: Un-skip Memory Operations Test

### Location

`compiler/codegen/native/compile.zig:975-979`

```zig
test "V2: compile memory operations (stack load/store) produces correct code" {
    // TODO: Stack slot offset computation needs to be wired properly
    // The sized_stackslots array is not being populated during lowering
    // This test crashes with SIGABRT - skip until fixed
    return error.SkipZigTest;
}
```

### Issue

Stack slot wiring is incomplete. The `sized_stackslots` array is not populated during CLIF lowering.

### Cranelift Reference

Stack slots in Cranelift are created via `create_sized_stack_slot()` and tracked in the function's stack slot data:

```rust
// cranelift-codegen/src/ir/stackslot.rs
pub fn create_sized_stack_slot(&mut self, data: StackSlotData) -> StackSlot {
    self.sized_stack_slots.push(data)
}
```

### Fix

Audit our stack slot creation path and ensure `sized_stackslots` is populated when stack_addr/stack_load/stack_store instructions are created.

---

## Execution Order

1. **T1** - Quick fix, 5 minutes, unblocks Wasm validation
2. **T2** - Medium fix, 30 minutes, unblocks native lowering
3. **T3** - Investigation, 15 minutes
4. **T4** - Requires deeper investigation of stack slot wiring

After T1, Wasm compilation works. T2 is more complex due to:
- br_table having many successors (overflows BoundedArray(2))
- trap terminator needing special handling in lowerBranch
- Block params for exit block not being satisfied when unreachable

---

## T2 Investigation Findings (February 2026)

### Problem Chain

1. **Blocks in DFG not in Layout**: When code paths are unreachable, blocks created for control flow (like exit_block) may never be switched to, leaving them in DFG but not Layout.

2. **br_table successor overflow**: The `collectBranchAndTargets` function uses `BoundedArray(MachLabel, 2)` but br_table can have many more successors.

3. **trap terminator not handled**: When emitting trap for unreachable code, `lowerBranch` didn't handle `.trap` opcode.

4. **Block params mismatch**: When exit block has params (for return values) but is unreachable, branches to it don't pass args.

### Partial Fixes Applied

1. **collectBranchAndTargets** (lower.zig): Skip collecting successors for br_table/trap/return since they're handled separately.

2. **lowerBranch** (aarch64/lower.zig): Added `.trap` case to emit `udf` instruction.

### Remaining Issue

The liveness check fails because block 0 (entry) has block 1 (exit) as a successor via some path, but the branch args don't match the exit block's params. This requires deeper investigation of how the Wasm→CLIF translation handles the function exit block when paths are partially reachable.

---

## Verification

After fixes, run:

```bash
# Test Wasm output validates
echo 'fn main() i32 { return 42; }' > /tmp/test.cot
./zig-out/bin/cot --target=wasm32 /tmp/test.cot -o /tmp/test.wasm
node -e 'const fs=require("fs"); WebAssembly.instantiate(fs.readFileSync("/tmp/test.wasm")).then(r=>console.log(r.instance.exports.main()));'
# Expected: 42

# Test native output
./zig-out/bin/cot /tmp/test.cot -o /tmp/test
/tmp/test
echo $?
# Expected: 42

# Run all tests
zig build test
# Expected: 787/787 passed, 0 skipped
```
