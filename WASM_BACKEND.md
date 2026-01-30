# Wasm Backend Implementation Plan

## Status

| Milestone | Status | Tests |
|-----------|--------|-------|
| M1: Binary Encoding | DONE | 14 tests |
| M2: Module Builder | DONE | 5 tests |
| M3: E2E Return 42 | TODO | 0 tests |
| M4: E2E Add Function | TODO | 0 tests |
| M5: Control Flow | TODO | 0 tests |
| M6: Strings | TODO | 0 tests |
| M7: Memory/ARC | TODO | 0 tests |

---

## Architecture (Based on Go's Wasm Backend)

Go's Wasm backend (`cmd/compile/internal/wasm/ssa.go`) teaches us:

1. **Clear separation**: `ssaGenValue` handles each SSA op, `ssaGenBlock` handles control flow
2. **Helper functions**: `getValue64()` pushes operand to stack, `setReg()` stores result
3. **Type-driven dispatch**: Switch on operation type, emit corresponding Wasm instructions

**Cot's architecture:**

```
┌─────────────────────────────────────────────────────────────┐
│  compiler/codegen/                                          │
├─────────────────────────────────────────────────────────────┤
│  wasm_opcodes.zig    │ Wasm instruction constants (DONE)    │
│  wasm_encode.zig     │ LEB128, section encoding (DONE)      │
│  wasm.zig            │ Module/CodeBuilder (DONE)            │
│  wasm_codegen.zig    │ IR → Wasm translation (TODO)         │
└─────────────────────────────────────────────────────────────┘
```

**Key insight from Go**: The codegen walks the IR, and for each node:
1. Recursively emit operands (pushes values to Wasm stack)
2. Emit the operation (consumes stack values, produces result)
3. Store result if needed (to Wasm local)

---

## Milestone 3: E2E "Return 42"

**Goal:** Compile Cot source `fn answer() int { return 42 }` to working Wasm.

### Step 3.1: Install wasmtime

```bash
brew install wasmtime
```

**Verify:** `wasmtime --version` works.

### Step 3.2: Write the E2E test FIRST

**File:** `compiler/codegen/wasm_codegen_test.zig`

```zig
const std = @import("std");
const wasm_codegen = @import("wasm_codegen.zig");

test "e2e: return 42" {
    const source = "fn answer() int { return 42 }";

    // Compile to Wasm bytes
    const wasm_bytes = try wasm_codegen.compileSource(std.testing.allocator, source);
    defer std.testing.allocator.free(wasm_bytes);

    // Write to temp file
    const tmp_path = "/tmp/test_return42.wasm";
    try std.fs.cwd().writeFile(tmp_path, wasm_bytes);

    // Run with wasmtime
    const result = try std.process.Child.run(.{
        .allocator = std.testing.allocator,
        .argv = &[_][]const u8{ "wasmtime", tmp_path, "--invoke", "answer" },
    });
    defer std.testing.allocator.free(result.stdout);
    defer std.testing.allocator.free(result.stderr);

    // Verify output
    try std.testing.expectEqualStrings("42\n", result.stdout);
}
```

**This test will FAIL initially. That's correct. Now implement to make it pass.**

### Step 3.3: Create wasm_codegen.zig

**File:** `compiler/codegen/wasm_codegen.zig`

**Structure (follow Go's pattern):**

```zig
//! IR to WebAssembly code generator.
//! Follows Go's wasm/ssa.go architecture.

const std = @import("std");
const wasm = @import("wasm.zig");
const ir = @import("../frontend/ir.zig");
const parser = @import("../frontend/parser.zig");
const checker = @import("../frontend/checker.zig");
const lower = @import("../frontend/lower.zig");
const types = @import("../frontend/types.zig");

/// Compile Cot source code to Wasm binary.
pub fn compileSource(allocator: std.mem.Allocator, source: []const u8) ![]u8 {
    // 1. Parse
    var p = parser.Parser.init(allocator, source, "test.cot");
    const ast = try p.parse();

    // 2. Type check
    var type_reg = try types.TypeRegistry.init(allocator);
    defer type_reg.deinit();
    var c = checker.Checker.init(allocator, &type_reg);
    try c.check(ast);

    // 3. Lower to IR
    var l = lower.Lowerer.init(allocator, &type_reg);
    const ir_funcs = try l.lower(ast);

    // 4. Generate Wasm
    return try compileModule(allocator, ir_funcs, &type_reg);
}

/// Compile IR functions to Wasm module.
pub fn compileModule(
    allocator: std.mem.Allocator,
    funcs: []const ir.Func,
    type_reg: *const types.TypeRegistry,
) ![]u8 {
    var module = wasm.Module.init(allocator);
    defer module.deinit();

    for (funcs) |*func| {
        try compileFunc(&module, func, type_reg);
    }

    var output: std.ArrayListUnmanaged(u8) = .{};
    try module.emit(output.writer(allocator));
    return output.toOwnedSlice(allocator);
}

/// Compile a single IR function to Wasm.
fn compileFunc(
    module: *wasm.Module,
    func: *const ir.Func,
    type_reg: *const types.TypeRegistry,
) !void {
    // 1. Build Wasm function type
    const params = try buildParamTypes(module.allocator, func);
    const results = try buildResultTypes(module.allocator, func);
    const type_idx = try module.addFuncType(params, results);

    // 2. Add function to module
    const func_idx = try module.addFunc(type_idx);

    // 3. Export if public (for now, export all)
    try module.addExport(func.name, .func, func_idx);

    // 4. Generate function body
    var code = wasm.CodeBuilder.init(module.allocator);
    defer code.deinit();

    try emitFuncBody(&code, func, type_reg);

    const body = try code.finish();
    defer module.allocator.free(body);
    try module.addCode(body);
}

/// Emit function body by walking IR nodes.
fn emitFuncBody(
    code: *wasm.CodeBuilder,
    func: *const ir.Func,
    type_reg: *const types.TypeRegistry,
) !void {
    // Walk blocks in order
    for (func.blocks) |block| {
        for (block.nodes) |node_idx| {
            try emitNode(code, func.getNode(node_idx), func, type_reg);
        }
    }
}

/// Emit a single IR node (like Go's ssaGenValue).
fn emitNode(
    code: *wasm.CodeBuilder,
    node: *const ir.Node,
    func: *const ir.Func,
    type_reg: *const types.TypeRegistry,
) !void {
    switch (node.data) {
        .const_int => |ci| try code.emitI64Const(ci.value),
        .ret => |r| {
            if (r.value) |val_idx| {
                try emitNode(code, func.getNode(val_idx), func, type_reg);
            }
            // Implicit return at function end - no explicit return needed
        },
        else => {},
    }
}

fn buildParamTypes(allocator: std.mem.Allocator, func: *const ir.Func) ![]const wasm.ValType {
    var params: std.ArrayListUnmanaged(wasm.ValType) = .{};
    for (func.params) |param| {
        try params.append(allocator, cotTypeToWasm(param.type_idx));
    }
    return params.toOwnedSlice(allocator);
}

fn buildResultTypes(allocator: std.mem.Allocator, func: *const ir.Func) ![]const wasm.ValType {
    if (func.return_type == types.TypeRegistry.VOID) {
        return &[_]wasm.ValType{};
    }
    var results: std.ArrayListUnmanaged(wasm.ValType) = .{};
    try results.append(allocator, cotTypeToWasm(func.return_type));
    return results.toOwnedSlice(allocator);
}

fn cotTypeToWasm(type_idx: types.TypeIndex) wasm.ValType {
    return switch (type_idx) {
        types.TypeRegistry.BOOL => .i32,
        types.TypeRegistry.I32, types.TypeRegistry.U32 => .i32,
        types.TypeRegistry.I64, types.TypeRegistry.U64 => .i64,
        types.TypeRegistry.F32 => .f32,
        types.TypeRegistry.F64 => .f64,
        else => .i64, // Default for int, pointers, etc.
    };
}
```

### Step 3.4: Success Criteria

**STOP when:**
- [ ] `zig test compiler/codegen/wasm_codegen_test.zig` passes
- [ ] `wasmtime /tmp/test_return42.wasm --invoke answer` outputs `42`

**DO NOT proceed to Milestone 4 until M3 is complete.**

---

## Milestone 4: E2E "Add Two Numbers"

**Goal:** Compile `fn add(a: int, b: int) int { return a + b }` to working Wasm.

### Step 4.1: Write the E2E test FIRST

```zig
test "e2e: add two numbers" {
    const source = "fn add(a: int, b: int) int { return a + b }";

    const wasm_bytes = try wasm_codegen.compileSource(std.testing.allocator, source);
    defer std.testing.allocator.free(wasm_bytes);

    try std.fs.cwd().writeFile("/tmp/test_add.wasm", wasm_bytes);

    // wasmtime run /tmp/test_add.wasm --invoke add 3 5
    const result = try std.process.Child.run(.{
        .allocator = std.testing.allocator,
        .argv = &[_][]const u8{ "wasmtime", "/tmp/test_add.wasm", "--invoke", "add", "3", "5" },
    });
    defer std.testing.allocator.free(result.stdout);
    defer std.testing.allocator.free(result.stderr);

    try std.testing.expectEqualStrings("8\n", result.stdout);
}
```

### Step 4.2: Add to emitNode

```zig
fn emitNode(...) !void {
    switch (node.data) {
        .const_int => |ci| try code.emitI64Const(ci.value),

        .load_local, .local_ref => |lr| try code.emitLocalGet(lr.local_idx),

        .binary => |bin| {
            // Emit left operand (pushes to stack)
            try emitNode(code, func.getNode(bin.left), func, type_reg);
            // Emit right operand (pushes to stack)
            try emitNode(code, func.getNode(bin.right), func, type_reg);
            // Emit operation (consumes two, produces one)
            try emitBinaryOp(code, bin.op, isFloat(node.type_idx));
        },

        .ret => |r| {
            if (r.value) |val_idx| {
                try emitNode(code, func.getNode(val_idx), func, type_reg);
            }
        },

        else => {},
    }
}

fn emitBinaryOp(code: *wasm.CodeBuilder, op: ir.BinaryOp, is_float: bool) !void {
    if (is_float) {
        switch (op) {
            .add => try code.emitF64Add(),
            .sub => try code.emitF64Sub(),
            .mul => try code.emitF64Mul(),
            .div => try code.emitF64Div(),
            else => {},
        }
    } else {
        switch (op) {
            .add => try code.emitI64Add(),
            .sub => try code.emitI64Sub(),
            .mul => try code.emitI64Mul(),
            .div => try code.emitI64DivS(),
            .mod => try code.emitI64RemS(),
            else => {},
        }
    }
}
```

### Step 4.3: Success Criteria

**STOP when:**
- [ ] `zig test compiler/codegen/wasm_codegen_test.zig` passes (both tests)
- [ ] `wasmtime /tmp/test_add.wasm --invoke add 3 5` outputs `8`
- [ ] `wasmtime /tmp/test_add.wasm --invoke add 100 200` outputs `300`

---

## Milestone 5: Control Flow

**Goal:** Compile functions with if/else and loops.

### Step 5.1: Write tests FIRST

```zig
test "e2e: absolute value" {
    const source =
        \\fn abs(x: int) int {
        \\    if (x < 0) { return -x }
        \\    return x
        \\}
    ;
    // ... test abs(-5) == 5, abs(5) == 5
}

test "e2e: factorial" {
    const source =
        \\fn factorial(n: int) int {
        \\    var result: int = 1
        \\    var i: int = 1
        \\    while (i <= n) {
        \\        result = result * i
        \\        i = i + 1
        \\    }
        \\    return result
        \\}
    ;
    // ... test factorial(5) == 120
}
```

### Step 5.2: Control Flow Strategy

**Wasm has structured control flow.** No arbitrary gotos.

**IR `branch` → Wasm `if`/`else`/`end`:**
```
IR:                         Wasm:
branch cond, then, else  →  (if (result i64)
                              (then ...)
                              (else ...))
```

**IR loops → Wasm `loop`/`block`/`br`:**
```
IR:                         Wasm:
loop_header:              →  (block $exit
  branch cond, body, exit     (loop $loop
body:                           ;; condition
  ...                           br_if $exit
  jump loop_header              ;; body
exit:                           br $loop))
```

### Step 5.3: Implementation

Add to `emitNode`:
- `branch` → `if`/`else`/`end`
- `jump` → `br`
- Handle block structure

**This is the hardest part.** Take it slow. One test at a time.

---

## Milestone 6: Strings

**Goal:** Compile programs with string literals.

### Step 6.1: Tests FIRST

```zig
test "e2e: hello world" {
    const source =
        \\fn main() {
        \\    print("Hello, World!")
        \\}
    ;
    // Run and verify stdout contains "Hello, World!"
}
```

### Step 6.2: Implementation

1. **Data section**: String literals go in Wasm data section
2. **Import print**: `(import "env" "print" (func $print (param i32 i32)))`
3. **String representation**: `{ ptr: i32, len: i32 }`

---

## Milestone 7: Memory and ARC

**Goal:** Dynamic memory allocation with automatic reference counting.

### Step 7.1: Tests FIRST

```zig
test "e2e: dynamic string" {
    const source =
        \\fn greet(name: string) string {
        \\    return "Hello, " + name + "!"
        \\}
    ;
    // Test that memory is properly allocated and freed
}
```

### Step 7.2: Implementation

1. **Memory section**: Declare linear memory
2. **Allocator**: Bump allocator or free list
3. **ARC runtime**: `cot_retain`, `cot_release`
4. **Compiler inserts calls**: At function entry/exit, assignments

---

## Rules for Implementation

1. **Write test FIRST.** Test must fail before implementing.
2. **One milestone at a time.** Do not start M4 until M3 passes.
3. **Run tests after every change.** `zig test compiler/codegen/wasm_codegen_test.zig`
4. **Commit after each passing test.** Small, incremental commits.
5. **If stuck, ask.** Don't spiral. Don't hack. Ask for direction.

---

## Files to Create (Summary)

| Milestone | File | Purpose |
|-----------|------|---------|
| M3 | `compiler/codegen/wasm_codegen.zig` | IR → Wasm translation |
| M3 | `compiler/codegen/wasm_codegen_test.zig` | E2E tests |
| M6 | Update `wasm.zig` | Data section support |
| M7 | `runtime/wasm_runtime.zig` | ARC functions in Wasm |

---

## Reference

- Go's Wasm backend: `~/learning/go/src/cmd/compile/internal/wasm/ssa.go`
- Wasm spec: https://webassembly.github.io/spec/core/
- `bootstrap-0.2/DESIGN.md` - Overall architecture
