# Memory Leak Fix Plan for V2 Native Compilation Tests

## Status: FULLY FIXED

All memory leaks in the native compilation pipeline have been resolved.

## Problem Summary

Two tests were leaking memory:
1. `V2: compile CLIF function produces non-zero output`
2. `V2: compile function with control flow produces non-zero output`

## Root Causes and Fixes

### Leak 1: MachBuffer errdefer vs defer

**Location:** `compiler/codegen/native/machinst/vcode.zig` emit() function

**Problem:** `errdefer buffer.deinit()` only runs on error, but `buffer.finish()` copies data (doesn't move), so the original buffer must be freed even on success.

**Fix:** Changed `errdefer` to `defer` in vcode.zig:
```zig
var buffer = MachBufferType.init(alloc);
defer buffer.deinit();  // Always clean up, even on success
```

### Leak 2: Instruction-owned slices not freed

**Location:** `compiler/codegen/native/isa/aarch64/lower.zig` and `inst/mod.zig`

**Problem:** During lowering, several instructions allocate slices for their data:
- `rets.rets: []const RetPair` - return value constraints
- `args.args: []const ArgPair` - argument constraints
- `indirect_br.targets: []const MachLabel` - branch targets
- `jt_sequence.targets: []const MachLabel` - jump table targets

These allocations were never freed when VCode was deinited.

**Fix:** Added `deinit()` method to AArch64 Inst type in `inst/mod.zig`:
```zig
pub fn deinit(self: *const Inst, allocator: Allocator) void {
    switch (self.*) {
        .rets => |r| allocator.free(r.rets),
        .args => |a| allocator.free(a.args),
        .indirect_br => |b| allocator.free(b.targets),
        .jt_sequence => |j| allocator.free(j.targets),
        else => {},
    }
}
```

And updated `VCodeBuilder.deinit()` in `vcode.zig` to call instruction deinit:
```zig
pub fn deinit(self: *Self) void {
    self.vreg_types.deinit(self.allocator);
    // Free instruction-owned memory (slices allocated during lowering)
    if (@hasDecl(I, "deinit")) {
        for (self.insts.items) |*inst| {
            inst.deinit(self.allocator);
        }
    }
    self.insts.deinit(self.allocator);
    // ... rest of cleanup
}
```

## Design Note: Rust vs Zig Memory Patterns

In Rust/Cranelift, instructions can store slices backed by arenas or use move semantics:
```rust
pub fn finish(self, ...) -> MachBufferFinalized<...> {
    // self is consumed, no cleanup needed
}
```

In Zig, we must explicitly:
1. Track all allocations
2. Call deinit on owners when done
3. Use `defer` (not `errdefer`) when cleanup is needed regardless of success/error

## Test Verification

All tests now pass with no memory leaks:
```bash
zig build test && echo "ALL TESTS PASSED"
```

## Files Changed

1. `compiler/codegen/native/machinst/vcode.zig` - Changed errdefer to defer, added instruction deinit loop
2. `compiler/codegen/native/isa/aarch64/inst/mod.zig` - Added Inst.deinit() method
