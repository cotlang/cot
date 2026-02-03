# Task 7.1 Architectural Fix: Proper Cranelift GlobalValue Port

**Created**: 2026-02-03
**Updated**: 2026-02-03 (All Phases Complete)
**Status**: COMPLETE
**Priority**: P0 - Must complete before other Phase 7 tasks

---

## Executive Summary

Task 7.1 (Add Global Variable Support) was implemented but with architectural shortcuts that deviate from Cranelift's proven design. This document provides a detailed execution plan to align our implementation with Cranelift's architecture.

### Progress

**Phase 1: CLIF IR Types - COMPLETE**
- [x] Task A: GlobalValue entity type added to dfg.zig
- [x] Task B: GlobalValueData enum created in globalvalue.zig
- [x] Task C: global_values table added to Function
- [x] Task D: global_value opcode added to instructions.zig
- [x] Task E: unary_global_value instruction format added to builder.zig
- [x] Task F: GlobalValue field added to InstData
- [x] Task G: FuncBuilder.globalValue() method added

**Phase 2: Frontend Integration - COMPLETE**
- [x] Task H: FuncInstBuilder.globalValue() method added to frontend.zig

**Phase 3: Wasm Translation - COMPLETE**
- [x] Task I: FuncEnvironment created (func_environ.zig) with GlobalVariable, ConstantValue
- [x] Task J: translateGlobalGet/Set updated to use FuncEnvironment and GlobalValue

**Phase 4: Machine Code Lowering - COMPLETE**
- [x] Task K: global_value lowering added to aarch64/lower.zig (port of legalizer/globalvalue.rs)

### Final State

The full GlobalValue infrastructure is now ported from Cranelift:

**CLIF IR Layer:**
- `GlobalValue` entity type with RESERVED sentinel
- `GlobalValueData` enum (vmcontext, load, iadd_imm, symbol, dyn_scale_target_const)
- `Function.global_values` table
- `global_value` opcode and instruction format
- `FuncBuilder.globalValue()` and `FuncInstBuilder.globalValue()` builder methods

**Wasm Translation Layer:**
- `FuncEnvironment` for per-function global variable management
- `GlobalVariable` enum (Constant, Memory) following Cranelift's spec.rs
- `ConstantValue` enum (i32, i64, f32, f64)
- `translateGlobalGet/Set` using proper GlobalValue chain (vmctx → iadd_imm → load/store)

**Machine Code Lowering:**
- `global_value` instruction lowering following Cranelift's legalizer pattern
- Recursive `materializeGlobalValue` for handling GlobalValueData chains
- Support for vmcontext, iadd_imm, load, and symbol variants

---

## Part 1: Cranelift Reference Analysis

### 1.1 Entity Types (entities.rs:160-188)

```rust
/// An opaque reference to a global value.
#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GlobalValue(u32);
entity_impl!(GlobalValue, "gv");
```

The `entity_impl!` macro generates:
- `new(index: u32)` - create from index
- `index(self)` - get underlying index
- `reserved_value()` - sentinel for "no value" (u32::MAX)
- Display trait implementation

### 1.2 GlobalValueData (globalvalue.rs:14-84)

```rust
pub enum GlobalValueData {
    /// Value is the address of the VM context struct.
    VMContext,

    /// Value is pointed to by another global value.
    Load {
        base: GlobalValue,
        offset: Offset32,
        global_type: Type,
        flags: MemFlags,
    },

    /// Value is an offset from another global value.
    IAddImm {
        base: GlobalValue,
        offset: Imm64,
        global_type: Type,
    },

    /// Symbolic value resolved by linker.
    Symbol {
        name: ExternalName,
        offset: Imm64,
        colocated: bool,
        tls: bool,
    },

    /// Dynamic vector scale constant.
    DynScaleTargetConst {
        vector_type: Type,
    },
}
```

### 1.3 Function GlobalValues Table (function.rs:174-177)

```rust
pub struct Function {
    // ... other fields ...
    pub global_values: PrimaryMap<ir::GlobalValue, ir::GlobalValueData>,
    pub global_value_facts: SecondaryMap<ir::GlobalValue, Option<Fact>>,
}
```

### 1.4 global_value Instruction

The `global_value` opcode takes a `GlobalValue` reference and returns a `Value` containing the computed address. This instruction is materialized into actual address computation during lowering.

### 1.5 translate_global_get Pattern (func_environ.rs:3134-3196)

```rust
pub fn translate_global_get(&mut self, builder: &mut FunctionBuilder, global_index: GlobalIndex) -> WasmResult<ir::Value> {
    match self.get_or_create_global(builder.func, global_index) {
        GlobalVariable::Constant { value } => {
            // Emit iconst/f32const/f64const directly - no load needed
            Ok(builder.ins().iconst(ir::types::I32, value))
        }
        GlobalVariable::Memory { gv, offset, ty } => {
            // Use global_value instruction to get base address
            let addr = builder.ins().global_value(self.pointer_type(), gv);
            // Load the actual value
            Ok(builder.ins().load(ty, flags, addr, offset))
        }
        GlobalVariable::Custom => {
            // GC reference handling
        }
    }
}
```

---

## Part 2: Implementation Tasks

### Task A: Add GlobalValue Entity Type

**File**: `compiler/ir/clif/dfg.zig`

**Location**: After StackSlot definition (~line 131)

```zig
/// An opaque reference to a global value.
/// A GlobalValue is computed lazily from GlobalValueData during lowering.
pub const GlobalValue = struct {
    index: u32,

    pub const RESERVED: GlobalValue = .{ .index = std.math.maxInt(u32) };

    pub fn fromIndex(index: u32) GlobalValue {
        return .{ .index = index };
    }

    pub fn asU32(self: GlobalValue) u32 {
        return self.index;
    }

    pub fn eql(self: GlobalValue, other: GlobalValue) bool {
        return self.index == other.index;
    }

    pub fn format(
        self: GlobalValue,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("gv{d}", .{self.index});
    }
};
```

**Export in mod.zig**: Add `pub const GlobalValue = dfg_mod.GlobalValue;`

### Task B: Add GlobalValueData Enum

**File**: `compiler/ir/clif/dfg.zig` (or new file `globalvalue.zig`)

```zig
/// Information about a global value declaration.
/// Port of cranelift/codegen/src/ir/globalvalue.rs
pub const GlobalValueData = union(enum) {
    /// Value is the address of the VM context struct.
    vmcontext,

    /// Value is pointed to by another global value (load).
    load: struct {
        base: GlobalValue,
        offset: i32,
        global_type: Type,
        flags: MemFlags,
    },

    /// Value is an offset from another global value.
    iadd_imm: struct {
        base: GlobalValue,
        offset: i64,
        global_type: Type,
    },

    /// Symbolic value resolved by linker.
    symbol: struct {
        name: ExternalName,
        offset: i64,
        colocated: bool,
        tls: bool,
    },

    const Self = @This();

    /// Get the type of this global value.
    pub fn globalType(self: Self, pointer_type: Type) Type {
        return switch (self) {
            .vmcontext, .symbol => pointer_type,
            .load => |d| d.global_type,
            .iadd_imm => |d| d.global_type,
        };
    }
};
```

### Task C: Add GlobalValues Table to Function

**File**: `compiler/ir/clif/function.zig`

Add to Function struct:
```zig
pub const Function = struct {
    // ... existing fields ...

    /// Global value definitions.
    /// Port of cranelift/codegen/src/ir/function.rs:174
    global_values: std.ArrayListUnmanaged(GlobalValueData),

    // ... methods ...

    pub fn createGlobalValue(self: *Function, data: GlobalValueData) !GlobalValue {
        const index: u32 = @intCast(self.global_values.items.len);
        try self.global_values.append(self.allocator, data);
        return GlobalValue.fromIndex(index);
    }

    pub fn getGlobalValue(self: *const Function, gv: GlobalValue) GlobalValueData {
        return self.global_values.items[gv.index];
    }
};
```

### Task D: Add global_value Opcode

**File**: `compiler/ir/clif/instructions.zig`

Add to Opcode enum:
```zig
/// Get the value of a global variable.
/// Takes a GlobalValue reference and returns the computed address.
global_value,
```

### Task E: Add global_value Instruction Format

**File**: `compiler/ir/clif/builder.zig`

Add InstructionFormat:
```zig
pub const InstructionFormat = enum {
    // ... existing formats ...
    global_value,  // GlobalValue operand
};
```

Add InstructionData variant:
```zig
pub const InstructionData = union(enum) {
    // ... existing variants ...

    /// Global value computation.
    global_value: struct {
        opcode: Opcode,
        global_value: GlobalValue,
    },
};
```

Add builder method:
```zig
/// Compute global value address.
pub fn globalValue(self: *Self, ty: Type, gv: GlobalValue) !Value {
    const r = try self.insertInst(.{
        .global_value = .{ .opcode = .global_value, .global_value = gv },
    }, ty);
    return r.result.?;
}
```

### Task F: Add GlobalValue to InstData

**File**: `compiler/ir/clif/dfg.zig`

Add to InstData struct:
```zig
pub const InstData = struct {
    // ... existing fields ...

    /// GlobalValue reference (for global_value instruction).
    global_value: ?GlobalValue = null,
};
```

### Task G: Update FunctionBuilder insertInst

**File**: `compiler/ir/clif/builder.zig`

Update insertInst to handle global_value format:
```zig
// Extract GlobalValue for global_value instruction
const gv: ?GlobalValue = switch (data) {
    .global_value => |d| d.global_value,
    else => null,
};

try self.dfg.setInstData(inst, .{
    // ... existing fields ...
    .global_value = gv,
});
```

### Task H: Update Frontend FuncInstBuilder

**File**: `compiler/codegen/native/frontend/frontend.zig`

Add globalValue method to FuncInstBuilder that delegates to FuncBuilder.

### Task I: Create FuncEnvironment for Global Management

**File**: `compiler/codegen/native/wasm_to_clif/func_environ.zig` (new file)

```zig
//! Wasm function environment for global variable handling.
//! Port of wasmtime/crates/cranelift/src/func_environ.rs

const std = @import("std");
const clif = @import("../../../ir/clif/mod.zig");

/// Represents how a Wasm global is implemented.
pub const GlobalVariable = union(enum) {
    /// Global is a constant and can be emitted directly.
    constant: ConstantValue,

    /// Global is in memory and needs load/store.
    memory: struct {
        gv: clif.GlobalValue,
        offset: i32,
        ty: clif.Type,
    },
};

pub const ConstantValue = union(enum) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
};

/// Function environment managing globals for a single function translation.
pub const FuncEnvironment = struct {
    allocator: std.mem.Allocator,

    /// VMContext global value (created once per function).
    vmctx: ?clif.GlobalValue,

    /// Cached global variable info per wasm global index.
    globals: std.AutoHashMapUnmanaged(u32, GlobalVariable),

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .vmctx = null,
            .globals = .{},
        };
    }

    pub fn deinit(self: *Self) void {
        self.globals.deinit(self.allocator);
    }

    /// Get or create the VMContext global value.
    pub fn vmctxVal(self: *Self, func: *clif.Function) !clif.GlobalValue {
        if (self.vmctx) |gv| return gv;

        const gv = try func.createGlobalValue(.vmcontext);
        self.vmctx = gv;
        return gv;
    }

    /// Get or create global variable info for a wasm global.
    pub fn getOrCreateGlobal(
        self: *Self,
        func: *clif.Function,
        global_index: u32,
        global_type: clif.Type,
        is_constant: bool,
        constant_value: ?ConstantValue,
    ) !GlobalVariable {
        if (self.globals.get(global_index)) |gv| return gv;

        if (is_constant) {
            const var_info: GlobalVariable = .{
                .constant = constant_value.?,
            };
            try self.globals.put(self.allocator, global_index, var_info);
            return var_info;
        }

        // Create chain: vmctx → iadd_imm to get global address
        const vmctx = try self.vmctxVal(func);
        const offset = @as(i64, global_index) * 8; // Assuming 8-byte aligned globals

        const gv = try func.createGlobalValue(.{
            .iadd_imm = .{
                .base = vmctx,
                .offset = offset,
                .global_type = clif.Type.I64, // pointer type
            },
        });

        const var_info: GlobalVariable = .{
            .memory = .{
                .gv = gv,
                .offset = 0,
                .ty = global_type,
            },
        };
        try self.globals.put(self.allocator, global_index, var_info);
        return var_info;
    }
};
```

### Task J: Update translator.zig translateGlobalGet/Set

**File**: `compiler/codegen/native/wasm_to_clif/translator.zig`

Replace current hardcoded implementation with proper GlobalValue usage:

```zig
// Add field to FuncTranslator
env: FuncEnvironment,

// Update translateGlobalGet
pub fn translateGlobalGet(self: *Self, global_index: u32) !void {
    const global_val_type: WasmValType = if (global_index < self.globals.len)
        self.globals[global_index].val_type
    else
        .i64;
    const global_type = global_val_type.toClifType();

    // Determine if constant or memory-based
    // For now, treat all as memory-based (future: check mutability)
    const var_info = try self.env.getOrCreateGlobal(
        self.builder.func,
        global_index,
        global_type,
        false, // is_constant
        null,  // constant_value
    );

    switch (var_info) {
        .constant => |c| {
            // Emit constant directly
            const val = switch (c) {
                .i32 => |v| try self.builder.ins().iconst(Type.I32, v),
                .i64 => |v| try self.builder.ins().iconst(Type.I64, v),
                // ... f32, f64
            };
            try self.state.push1(val);
        },
        .memory => |m| {
            // Use global_value instruction to get address
            const addr = try self.builder.ins().globalValue(Type.I64, m.gv);
            // Load the value
            const value = try self.builder.ins().load(m.ty, clif.MemFlags.DEFAULT, addr, m.offset);
            try self.state.push1(value);
        },
    }
}

pub fn translateGlobalSet(self: *Self, global_index: u32) !void {
    const value = self.state.pop1();

    const global_val_type: WasmValType = if (global_index < self.globals.len)
        self.globals[global_index].val_type
    else
        .i64;
    const global_type = global_val_type.toClifType();

    const var_info = try self.env.getOrCreateGlobal(
        self.builder.func,
        global_index,
        global_type,
        false,
        null,
    );

    switch (var_info) {
        .constant => unreachable, // Cannot set constant globals
        .memory => |m| {
            const addr = try self.builder.ins().globalValue(Type.I64, m.gv);
            _ = try self.builder.ins().store(clif.MemFlags.DEFAULT, value, addr, m.offset);
        },
    }
}
```

### Task K: Update MachInst Lower to Handle global_value

**File**: `compiler/codegen/native/machinst/lower.zig`

Add lowering case for global_value opcode that materializes the address computation based on GlobalValueData:

```zig
.global_value => {
    // Get GlobalValue from instruction
    const gv = inst_data.global_value.?;
    const gv_data = ctx.func.getGlobalValue(gv);

    switch (gv_data) {
        .vmcontext => {
            // VMContext is typically passed as a function argument
            // or accessed via a dedicated register
            // Emit: load vmctx from known location
        },
        .iadd_imm => |d| {
            // Recursively get base, then add immediate
            const base_addr = try lower_global_value(ctx, d.base);
            // Emit: add immediate to base
        },
        .load => |d| {
            // Load from computed address
            const base_addr = try lower_global_value(ctx, d.base);
            // Emit: load from base + offset
        },
        .symbol => {
            // Emit: load symbol address via GOT or PC-relative
        },
    }
},
```

---

## Part 3: Verification Checklist

### Unit Tests

- [ ] GlobalValue entity creation and RESERVED constant
- [ ] GlobalValueData creation for each variant
- [ ] Function.createGlobalValue adds to table correctly
- [ ] FuncBuilder.globalValue creates instruction correctly
- [ ] InstData.global_value field populated correctly

### Integration Tests

- [ ] translate simple function using globals still works
- [ ] All 7 ARC functions translate without panic
- [ ] GlobalValue chain (vmctx → iadd_imm) created correctly

### End-to-End Test

```bash
echo 'fn main() i32 { return 42; }' | cot -
# Should compile without stack underflow
```

---

## Part 4: Execution Order

```
Phase 1: CLIF IR Types
├── Task A: Add GlobalValue entity type
├── Task B: Add GlobalValueData enum
├── Task C: Add global_values table to Function
├── Task D: Add global_value opcode
├── Task E: Add global_value instruction format
├── Task F: Add GlobalValue to InstData
├── Task G: Update FuncBuilder insertInst
│
Phase 2: Frontend Integration
├── Task H: Update Frontend FuncInstBuilder
│
Phase 3: Wasm Translation
├── Task I: Create FuncEnvironment
├── Task J: Update translateGlobalGet/Set
│
Phase 4: Machine Code Lowering
├── Task K: Update MachInst Lower
│
Phase 5: Verification
├── Run unit tests
├── Run integration tests
└── Run end-to-end test
```

---

## Part 5: Files Modified Summary

| File | Changes |
|------|---------|
| `compiler/ir/clif/dfg.zig` | Add GlobalValue, GlobalValueData, InstData.global_value |
| `compiler/ir/clif/mod.zig` | Export GlobalValue, GlobalValueData |
| `compiler/ir/clif/function.zig` | Add global_values table, createGlobalValue |
| `compiler/ir/clif/instructions.zig` | Add global_value opcode |
| `compiler/ir/clif/builder.zig` | Add InstructionFormat.global_value, InstructionData.global_value, globalValue() |
| `compiler/codegen/native/frontend/frontend.zig` | Add globalValue to FuncInstBuilder |
| `compiler/codegen/native/wasm_to_clif/func_environ.zig` | New file: FuncEnvironment, GlobalVariable |
| `compiler/codegen/native/wasm_to_clif/translator.zig` | Add env field, update translateGlobalGet/Set |
| `compiler/codegen/native/machinst/lower.zig` | Add global_value lowering case |

---

## Appendix: Cranelift Source File References

| Component | File | Lines |
|-----------|------|-------|
| GlobalValue entity | entities.rs | 160-188 |
| GlobalValueData | globalvalue.rs | 14-84 |
| Function.global_values | function.rs | 174-177 |
| translate_global_get | func_environ.rs | 3134-3196 |
| translate_global_set | func_environ.rs | 3198-3246 |
| GlobalVariable enum | func_environ.rs | ~3050-3080 |
