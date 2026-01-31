# Wasm Backend Implementation Plan

## Architecture (Go's Approach)

Go's Wasm backend uses SSA with Wasm-specific ops:

```
Cot Source → Frontend → IR → SSA (generic ops) → SSA (wasm ops) → Wasm bytecode
                                      ↓                 ↓
                               compiler/ssa/      compiler/ssa/passes/
                               op.zig             lower_wasm.zig
                                                        ↓
                                              compiler/codegen/
                                              wasm_gen.zig
```

**Go's pattern:**
1. Generic SSA ops (`add`, `mul`, `load`) defined in `op.zig` - DONE
2. Wasm-specific SSA ops (`wasm_i64_add`, `wasm_i64_const`) - TODO
3. Lowering pass converts generic → Wasm ops - TODO
4. `ssaGenValue` emits Wasm bytecode for each Wasm op - TODO

---

## Status

| Component | Status | Location |
|-----------|--------|----------|
| SSA infrastructure | DONE | `compiler/ssa/` |
| Generic ops (add, sub, etc.) | DONE | `compiler/ssa/op.zig` |
| ARM64 ops | DONE | `compiler/ssa/op.zig` |
| AMD64 ops | DONE | `compiler/ssa/op.zig` |
| **Wasm ops** | TODO | `compiler/ssa/op.zig` |
| **Wasm lowering pass** | TODO | `compiler/ssa/passes/lower_wasm.zig` |
| Wasm binary encoding | DONE | `compiler/codegen/wasm_encode.zig` |
| Wasm opcodes | DONE | `compiler/codegen/wasm_opcodes.zig` |
| Module builder | DONE | `compiler/codegen/wasm.zig` |
| **Wasm codegen (ssaGenValue)** | TODO | `compiler/codegen/wasm_gen.zig` |

---

## Step 1: Add Wasm SSA Ops to op.zig

**File:** `compiler/ssa/op.zig`

**Add after AMD64 ops (around line 115):**

```zig
// === Wasm-Specific ===
// Constants
wasm_i64_const, wasm_f64_const, wasm_f32_const,

// Integer arithmetic
wasm_i64_add, wasm_i64_sub, wasm_i64_mul, wasm_i64_div_s, wasm_i64_div_u,
wasm_i64_rem_s, wasm_i64_rem_u,

// Integer bitwise
wasm_i64_and, wasm_i64_or, wasm_i64_xor, wasm_i64_shl, wasm_i64_shr_s, wasm_i64_shr_u,
wasm_i64_clz, wasm_i64_ctz, wasm_i64_popcnt, wasm_i64_rotl, wasm_i64_rotr,

// Integer comparison
wasm_i64_eqz, wasm_i64_eq, wasm_i64_ne,
wasm_i64_lt_s, wasm_i64_lt_u, wasm_i64_gt_s, wasm_i64_gt_u,
wasm_i64_le_s, wasm_i64_le_u, wasm_i64_ge_s, wasm_i64_ge_u,

// Float arithmetic
wasm_f64_add, wasm_f64_sub, wasm_f64_mul, wasm_f64_div,
wasm_f64_neg, wasm_f64_abs, wasm_f64_sqrt, wasm_f64_ceil, wasm_f64_floor,

// Float comparison
wasm_f64_eq, wasm_f64_ne, wasm_f64_lt, wasm_f64_gt, wasm_f64_le, wasm_f64_ge,

// Conversions
wasm_i64_trunc_f64_s, wasm_i64_trunc_f64_u,
wasm_f64_convert_i64_s, wasm_f64_convert_i64_u,
wasm_i64_extend_i32_s, wasm_i64_extend_i32_u,
wasm_i32_wrap_i64,

// Memory
wasm_i64_load, wasm_i64_store, wasm_i32_load, wasm_i32_store,
wasm_f64_load, wasm_f64_store,
wasm_i64_load8_s, wasm_i64_load8_u, wasm_i64_load16_s, wasm_i64_load16_u,
wasm_i64_load32_s, wasm_i64_load32_u,
wasm_i64_store8, wasm_i64_store16, wasm_i64_store32,

// Variables
wasm_local_get, wasm_local_set, wasm_local_tee,
wasm_global_get, wasm_global_set,

// Control flow
wasm_call, wasm_call_indirect,
wasm_drop, wasm_select,

// Lowered operations (like Go's OpWasmLowered*)
wasm_lowered_move, wasm_lowered_zero,
wasm_lowered_nil_check,
wasm_lowered_static_call, wasm_lowered_closure_call,
```

**Add to op_info_table:**

```zig
// Wasm ops
table[@intFromEnum(Op.wasm_i64_const)] = .{ .name = "WasmI64Const", .aux_type = .int64, .rematerializable = true, .generic = false };
table[@intFromEnum(Op.wasm_i64_add)] = .{ .name = "WasmI64Add", .arg_len = 2, .commutative = true, .generic = false };
table[@intFromEnum(Op.wasm_i64_sub)] = .{ .name = "WasmI64Sub", .arg_len = 2, .generic = false };
// ... etc for all wasm ops
```

**Test:** `zig test compiler/ssa/op.zig` passes

---

## Step 2: Create Wasm Lowering Pass

**File:** `compiler/ssa/passes/lower_wasm.zig`

**Purpose:** Convert generic SSA ops to Wasm-specific ops (like Go's `lower` pass)

```zig
//! Wasm lowering pass - converts generic SSA ops to Wasm-specific ops.
//! Based on Go's cmd/compile/internal/ssa/lower*.go

const std = @import("std");
const ssa = @import("../func.zig");
const op = @import("../op.zig");
const value = @import("../value.zig");

/// Lower a function's generic ops to Wasm ops.
pub fn lowerFunc(func: *ssa.Func) !void {
    for (func.blocks.items) |*block| {
        for (block.values.items) |*val| {
            try lowerValue(func, val);
        }
    }
}

fn lowerValue(func: *ssa.Func, val: *value.Value) !void {
    switch (val.op) {
        // Constants
        .const_int, .const_64 => val.op = .wasm_i64_const,
        .const_float => val.op = .wasm_f64_const,

        // Arithmetic
        .add, .add64 => val.op = .wasm_i64_add,
        .sub, .sub64 => val.op = .wasm_i64_sub,
        .mul, .mul64 => val.op = .wasm_i64_mul,
        .div => val.op = .wasm_i64_div_s,
        .udiv => val.op = .wasm_i64_div_u,
        .mod => val.op = .wasm_i64_rem_s,
        .umod => val.op = .wasm_i64_rem_u,

        // Bitwise
        .and_, .and64 => val.op = .wasm_i64_and,
        .or_, .or64 => val.op = .wasm_i64_or,
        .xor, .xor64 => val.op = .wasm_i64_xor,
        .shl, .shl64 => val.op = .wasm_i64_shl,
        .shr, .shr64 => val.op = .wasm_i64_shr_u,
        .sar, .sar64 => val.op = .wasm_i64_shr_s,

        // Comparisons
        .eq, .eq64 => val.op = .wasm_i64_eq,
        .ne, .ne64 => val.op = .wasm_i64_ne,
        .lt, .lt64 => val.op = .wasm_i64_lt_s,
        .le, .le64 => val.op = .wasm_i64_le_s,
        .gt, .gt64 => val.op = .wasm_i64_gt_s,
        .ge, .ge64 => val.op = .wasm_i64_ge_s,
        .ult => val.op = .wasm_i64_lt_u,
        .ule => val.op = .wasm_i64_le_u,
        .ugt => val.op = .wasm_i64_gt_u,
        .uge => val.op = .wasm_i64_ge_u,

        // Float
        .add64f => val.op = .wasm_f64_add,
        .sub64f => val.op = .wasm_f64_sub,
        .mul64f => val.op = .wasm_f64_mul,
        .div64f => val.op = .wasm_f64_div,
        .neg64f => val.op = .wasm_f64_neg,

        // Memory
        .load, .load64 => val.op = .wasm_i64_load,
        .store, .store64 => val.op = .wasm_i64_store,

        // Function calls
        .static_call => val.op = .wasm_lowered_static_call,
        .call => val.op = .wasm_call,

        // Already Wasm-specific or control flow - leave unchanged
        else => {},
    }
    _ = func;
}

// ============================================================================
// Tests
// ============================================================================

test "lower add to wasm_i64_add" {
    // Create a simple function with add op
    // Verify it gets lowered to wasm_i64_add
}

test "lower const_int to wasm_i64_const" {
    // ...
}
```

**Test:** `zig test compiler/ssa/passes/lower_wasm.zig` passes

---

## Step 3: Create Wasm Code Generator

**File:** `compiler/codegen/wasm_gen.zig`

**Purpose:** Emit Wasm bytecode for each Wasm SSA op (like Go's `ssaGenValue`)

```zig
//! Wasm code generator - emits bytecode for Wasm SSA ops.
//! Based on Go's cmd/compile/internal/wasm/ssa.go

const std = @import("std");
const ssa = @import("../ssa/func.zig");
const op = @import("../ssa/op.zig");
const value = @import("../ssa/value.zig");
const wasm = @import("wasm.zig");

/// Generate Wasm code for an SSA function.
pub fn genFunc(func: *const ssa.Func, module: *wasm.Module) !void {
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    // Generate code for each block
    for (func.blocks.items) |block| {
        try genBlock(func, block, &code);
    }

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);
}

fn genBlock(func: *const ssa.Func, block: *const ssa.Block, code: *wasm.CodeBuilder) !void {
    for (block.values.items) |val| {
        try genValue(func, val, code);
    }
    _ = func;
}

/// Generate Wasm code for a single SSA value (like Go's ssaGenValue).
fn genValue(func: *const ssa.Func, val: *const value.Value, code: *wasm.CodeBuilder) !void {
    switch (val.op) {
        // Constants
        .wasm_i64_const => try code.emitI64Const(val.aux_int),
        .wasm_f64_const => try code.emitF64Const(val.aux_float),

        // Arithmetic (2 args, already on stack from recursive calls)
        .wasm_i64_add => try code.emitI64Add(),
        .wasm_i64_sub => try code.emitI64Sub(),
        .wasm_i64_mul => try code.emitI64Mul(),
        .wasm_i64_div_s => try code.emitI64DivS(),
        .wasm_i64_rem_s => try code.emitI64RemS(),

        // Bitwise
        .wasm_i64_and => try code.emitI64And(),
        .wasm_i64_or => try code.emitI64Or(),
        .wasm_i64_xor => try code.emitI64Xor(),
        .wasm_i64_shl => try code.emitI64Shl(),
        .wasm_i64_shr_s => try code.emitI64ShrS(),

        // Comparisons
        .wasm_i64_eq => try code.emitI64Eq(),
        .wasm_i64_ne => try code.emitI64Ne(),
        .wasm_i64_lt_s => try code.emitI64LtS(),
        .wasm_i64_le_s => try code.emitI64LeS(),
        .wasm_i64_gt_s => try code.emitI64GtS(),
        .wasm_i64_ge_s => try code.emitI64GeS(),
        .wasm_i64_eqz => try code.emitI64Eqz(),

        // Float
        .wasm_f64_add => try code.emitF64Add(),
        .wasm_f64_sub => try code.emitF64Sub(),
        .wasm_f64_mul => try code.emitF64Mul(),
        .wasm_f64_div => try code.emitF64Div(),
        .wasm_f64_neg => try code.emitF64Neg(),

        // Variables
        .wasm_local_get => try code.emitLocalGet(@intCast(val.aux_int)),
        .wasm_local_set => try code.emitLocalSet(@intCast(val.aux_int)),

        // Function calls
        .wasm_call => try code.emitCall(@intCast(val.aux_int)),

        // Memory - TODO
        .wasm_i64_load, .wasm_i64_store => {},

        else => {},
    }
    _ = func;
}

// ============================================================================
// Tests
// ============================================================================

test "genValue wasm_i64_const" {
    const allocator = std.testing.allocator;
    var code = wasm.CodeBuilder.init(allocator);
    defer code.deinit();

    var val = value.Value{ .op = .wasm_i64_const, .aux_int = 42 };
    try genValue(undefined, &val, &code);

    try std.testing.expectEqualSlices(u8, &[_]u8{ 0x42, 42 }, code.buf.items);
}

test "genValue wasm_i64_add" {
    // ...
}
```

**Test:** `zig test compiler/codegen/wasm_gen.zig` passes

---

## Step 4: Wire Up the Pipeline

**File:** `compiler/driver.zig` (or new `compiler/codegen/wasm_driver.zig`)

```zig
pub fn compileToWasm(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    // 1. Frontend: source → IR
    const ir_funcs = try frontend.compile(allocator, source);

    // 2. Build SSA
    var ssa_funcs = try ssa.buildFromIR(allocator, ir_funcs);

    // 3. Lower to Wasm ops
    for (ssa_funcs) |*func| {
        try lower_wasm.lowerFunc(func);
    }

    // 4. Generate Wasm
    var module = wasm.Module.init(allocator);
    for (ssa_funcs) |*func| {
        try wasm_gen.genFunc(func, &module);
    }

    // 5. Emit binary
    var output: std.ArrayListUnmanaged(u8) = .{};
    try module.emit(output.writer(allocator));
    return output.toOwnedSlice(allocator);
}
```

---

## Milestones (Revised)

### M1: Wasm SSA Ops (Step 1) ✅
- [x] Add `wasm_*` ops to `compiler/ssa/op.zig`
- [x] Add op_info entries for each
- [x] Test: `zig test compiler/ssa/op.zig` passes
- [x] Commit: 3da7295

### M2: Wasm Lowering Pass (Step 2) ✅
- [x] Create `compiler/ssa/passes/lower_wasm.zig`
- [x] Implement `lowerFunc` and `lowerValue`
- [x] Test: generic ops convert to wasm ops
- [x] Commit: 92f8ffc

### M3: Wasm Code Generator (Step 3) ✅
- [x] Create `compiler/codegen/wasm_gen.zig`
- [x] Implement `genFunc`, `genBlock`, `genValue`
- [x] Test: wasm ops emit correct bytecode
- [x] Commit: 0a488a0

### M4: E2E "Return 42" ✅
- [x] Wire up pipeline in `compiler/codegen/wasm_e2e_test.zig`
- [x] Test: compile `fn answer() int { return 42 }` → produces valid Wasm
- [x] Commit

### M5: E2E "Add Two Numbers" ✅
- [x] Test: compile `fn add(a: int, b: int) int { return a + b }` → produces valid Wasm
- [x] Commit

### M6: Control Flow (if/else) ✅
- [x] Add control flow emit functions to `compiler/codegen/wasm.zig`
- [x] Add layout pass `compiler/ssa/passes/layout.zig`
- [x] Implement `ssaGenBlock` in `compiler/codegen/wasm_gen.zig`
- [x] Test: if/else compiles to valid Wasm

### M7: Loops (while) ✅
- [x] Handle loop block kinds in layout pass
- [x] Emit `loop`/`br`/`br_if` in ssaGenBlock (Go's blockDepths pattern)
- [x] Track block nesting depth for relative branch calculation
- [x] Test: `while` compiles to valid Wasm
- Note: Loop control flow works; memory ops (local_addr, load/store) pending M10

### M8: Function Calls Between Cot Functions ✅
- [x] Track function index mapping (FuncIndexMap)
- [x] Emit `call` with correct function indices via aux_call.fn_name lookup
- [x] Test: calling one function from another works
- [x] Test: recursive function calls work

### M9: CLI Emits .wasm ✅
- [x] Add `--target=wasm32` to CLI
- [x] Add wasm32 to target.zig (with freestanding OS)
- [x] Output `.wasm` file directly (no linking needed)
- [x] Test: `fn main() int { return 42; }` compiles and runs correctly
- Note: Functions with parameters need M10 (linear memory) for full support

### M10: Linear Memory ✅
- [x] Memory section in Linker (1 page = 64KB)
- [x] Global section with SP (global 0, initialized to 65536)
- [x] `local_addr` generates SP-relative addresses
- [x] `wasm_i64_load`, `wasm_i64_store` with offsets
- [x] `wasm_i32_load`, `wasm_i32_store` for addresses
- [x] Prologue/epilogue SP adjustment for frame allocation
- [x] Tests: local_addr, store+load round-trip, frame size
- Go reference: wasm/ssa.go lines 280-284 (stores), 379-382 (loads)

### M11: Pointers
- [ ] Pointer types in Wasm (i32 addresses)
- [ ] Address-of, dereference operations

### M12: Structs
- [ ] Struct layout in linear memory
- [ ] Field access via memory offsets

### M13: Arrays/Slices
- [ ] Array bounds checking
- [ ] Slice representation (ptr + len)

### M14: Strings
- [ ] String data in data section
- [ ] String operations via runtime

### M15: ARC Basics
- [ ] Retain/release function calls
- [ ] Reference counting for heap objects

### M16: Browser Imports
- [ ] Import section for JS interop
- [ ] console.log, DOM access

---

## Rules

1. **Follow Go's pattern exactly.** We have Go-style SSA. Use it.
2. **One step at a time.** M1 → M2 → M3 → M4. No skipping.
3. **Test each component in isolation.** Unit tests before integration.
4. **Commit after each passing milestone.**
5. **ALWAYS check Go's code first for design.** Reference: `~/learning/go/src/cmd/compile/internal/wasm/`
   - `ssa.go` - ssaGenValue, ssaGenBlock patterns
   - `ops.go` - Wasm op definitions
   - Key insight: Go handles Wasm's structured control flow with block/loop/if constructs

---

## Reference Files

| Our File | Go Equivalent | Purpose |
|----------|---------------|---------|
| `compiler/ssa/op.zig` | `cmd/compile/internal/ssa/op.go` | SSA operations |
| `compiler/ssa/passes/lower_wasm.zig` | `cmd/compile/internal/ssa/lower*.go` | Generic → target lowering |
| `compiler/codegen/wasm_gen.zig` | `cmd/compile/internal/wasm/ssa.go` | SSA → Wasm bytecode |
| `compiler/codegen/wasm.zig` | N/A (Go uses obj/wasm) | Module builder |
