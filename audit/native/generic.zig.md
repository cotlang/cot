# Audit: generic.zig

## Status: VERIFIED CORRECT

| Metric | Value |
|--------|-------|
| 0.2 lines | 308 |
| 0.3 lines | 308 |
| Reduction | 0% (direct copy) |
| Tests | 1/1 pass |

---

## Purpose

Reference code generator that produces human-readable pseudo-assembly. Used for:
1. Testing the full pipeline without machine code complexity
2. Debugging SSA output
3. Fallback for unsupported architectures

---

## Function-by-Function Verification

### GenericCodeGen Struct

| Method | Lines | Verdict |
|--------|-------|---------|
| init | 5 | IDENTICAL |
| deinit | 3 | IDENTICAL |
| generate | 14 | IDENTICAL |
| generateBlock | 40 | IDENTICAL |
| generateValue | 100 | IDENTICAL |
| generateBinaryOp | 12 | IDENTICAL |
| generateUnaryOp | 10 | IDENTICAL |
| generateLoad | 5 | IDENTICAL |
| allocSlot | 10 | IDENTICAL |

---

## Key Changes

### 1. Import Paths Updated

**0.2** (location: `src/codegen/generic.zig`):
```zig
const Func = @import("../ssa/func.zig").Func;
const Block = @import("../ssa/block.zig").Block;
```

**0.3** (location: `compiler/codegen/native/generic.zig`):
```zig
const Func = @import("../../ssa/func.zig").Func;
const Block = @import("../../ssa/block.zig").Block;
```

### 2. No Logic Changes

The file is a direct copy with only import path adjustments. This is intentional because:
- generic.zig is already minimal (no unused code)
- It serves as a reference implementation
- Simplification would reduce debugging value

---

## Output Format

The generator produces pseudo-assembly like:

```
.func test_add:
  ; b1 (ret)
  stack[0] = const_int 40    ; v1
  stack[8] = const_int 2     ; v2
  r0 = stack[0]    ; load v1
  r1 = stack[8]    ; load v2
  stack[16] = add r0, r1    ; v3
  r0 = stack[16]    ; load v3
  return r0

.end test_add
```

---

## Supported Operations

| Category | Operations |
|----------|------------|
| Constants | const_int, const_8/16/32/64, const_bool, const_nil |
| Binary | add, sub, mul, div, and_, or_, xor |
| Compare | eq, ne, lt, le, gt, ge |
| Unary | neg, not |
| Memory | load, store |
| Control | phi, copy, call, static_call |

---

## Integration

Added to `main.zig`:
```zig
pub const native_generic = @import("codegen/native/generic.zig");
```

---

## Verification

```bash
zig build test
# 376/398 tests passed, 22 skipped
# generic.zig test passes
```

**VERIFIED: Logic 100% identical. 0% reduction. 1 test passes.**
