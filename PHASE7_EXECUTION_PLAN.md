# Phase 7: Integration Execution Plan

**Source**: Cranelift `machinst/compile.rs`, `context.rs`, `vcode.rs`
**Target**: `compiler/codegen/native/compile.zig` + driver integration
**Estimated LOC**: ~2,000

---

## Overview

Phase 7 wires together all ported components into a working native codegen pipeline.

**The Cranelift Pipeline** (what we're copying exactly):

```
Context::compile()
  ├─ Context::optimize()           // IR optimization passes
  │   ├─ legalize()
  │   ├─ compute_cfg()
  │   ├─ compute_domtree()
  │   └─ eliminate_unreachable_code()
  │
  └─ TargetIsa::compile_function()
      └─ compile::<Backend>()      // machinst/compile.rs
          ├─ BlockLoweringOrder::new()
          ├─ Lower::new()
          ├─ Lower::lower()        // CLIF → VCode (vregs)
          ├─ regalloc2::run()      // vregs → pregs
          └─ VCode::emit()         // VCode + regalloc → bytes
              ├─ gen_prologue()
              ├─ for each block: emit instructions with regalloc edits
              ├─ gen_epilogue()
              └─ MachBuffer::finish()
```

---

## Module Structure

```
compiler/codegen/native/
├── compile.zig           # NEW: Main compile() function
├── context.zig           # NEW: CompileContext struct
├── vcode_adapter.zig     # NEW: VCode → regalloc2::Function adapter
├── emit_context.zig      # NEW: Emission with regalloc output
│
├── wasm_parser.zig       # EXISTING
├── wasm_to_clif/         # EXISTING
├── machinst/             # EXISTING
├── isa/aarch64/          # EXISTING
├── isa/x64/              # EXISTING
├── regalloc/             # EXISTING
├── macho.zig             # EXISTING
└── elf.zig               # EXISTING
```

---

## Phase 7.1: compile.zig - Main Orchestration

**Cranelift Source**: `machinst/compile.rs` lines 17-100
**Target**: `compiler/codegen/native/compile.zig`
**Estimated LOC**: ~300

### Core Function

```zig
/// Main compilation entry point - mirrors Cranelift's compile::<B>()
pub fn compile(
    allocator: std.mem.Allocator,
    clif_func: *const clif.Function,
    isa: TargetIsa,
    ctrl_plane: *CtrlPlane,
) !CompiledCode {
    // 1. Compute block ordering
    const block_order = try BlockLoweringOrder.compute(allocator, clif_func);
    defer block_order.deinit(allocator);

    // 2. Create lowering context
    var lower = try Lower.init(
        allocator,
        clif_func,
        isa.abi(),
        isa.emit_info(),
        block_order,
        isa.flags(),
    );
    defer lower.deinit();

    // 3. Lower CLIF to VCode (virtual registers)
    var vcode = try lower.lower(isa.backend(), ctrl_plane);
    defer vcode.deinit(allocator);

    // 4. Run register allocation
    const regalloc_result = try runRegalloc(allocator, &vcode, isa.machine_env());

    // 5. Emit machine code
    const emit_result = try vcode.emit(allocator, &regalloc_result, isa.emit_info());

    return CompiledCode{
        .buffer = emit_result.buffer,
        .frame_size = emit_result.frame_size,
        .bb_offsets = emit_result.bb_offsets,
    };
}
```

### Supporting Types

```zig
pub const CompiledCode = struct {
    buffer: MachBufferFinalized,
    frame_size: u32,
    bb_offsets: []const u32,

    pub fn code(self: *const CompiledCode) []const u8 {
        return self.buffer.data();
    }

    pub fn relocations(self: *const CompiledCode) []const Relocation {
        return self.buffer.relocations();
    }
};

pub const TargetIsa = union(enum) {
    aarch64: *const aarch64.Backend,
    x64: *const x64.Backend,

    pub fn backend(self: TargetIsa) anytype { ... }
    pub fn abi(self: TargetIsa) *const Abi { ... }
    pub fn machine_env(self: TargetIsa) MachineEnv { ... }
    pub fn emit_info(self: TargetIsa) EmitInfo { ... }
};
```

### Task Checklist

- [ ] **7.1.1** Create `compile.zig` with `compile()` function signature
- [ ] **7.1.2** Implement `TargetIsa` union for backend dispatch
- [ ] **7.1.3** Implement `CompiledCode` result struct
- [ ] **7.1.4** Wire BlockLoweringOrder (from blockorder.zig)
- [ ] **7.1.5** Wire Lower context creation
- [ ] **7.1.6** Wire VCode emission
- [ ] **7.1.7** Add unit tests

---

## Phase 7.2: vcode_adapter.zig - VCode ↔ Regalloc2 Bridge

**Cranelift Source**: `machinst/vcode.rs` impl Function for VCode
**Target**: `compiler/codegen/native/vcode_adapter.zig`
**Estimated LOC**: ~400

### Purpose

Regalloc2 requires a `Function` interface. VCode must implement this.

### Cranelift Implementation

```rust
// From vcode.rs - VCode implements regalloc2::Function
impl<I: VCodeInst> regalloc2::Function for VCode<I> {
    fn num_insts(&self) -> usize { self.insts.len() }
    fn num_blocks(&self) -> usize { self.block_ranges.len() }
    fn entry_block(&self) -> regalloc2::Block { Block::new(0) }

    fn block_insns(&self, block: Block) -> InstRange {
        let range = self.block_ranges[block.index()];
        InstRange::forward(Inst::new(range.start), Inst::new(range.end))
    }

    fn block_succs(&self, block: Block) -> &[Block] {
        &self.block_succs[self.block_succ_range[block.index()].clone()]
    }

    fn block_preds(&self, block: Block) -> &[Block] {
        &self.block_preds[self.block_pred_range[block.index()].clone()]
    }

    fn block_params(&self, block: Block) -> &[VReg] {
        &self.block_params[self.block_params_range[block.index()].clone()]
    }

    fn inst_operands(&self, inst: Inst) -> &[Operand] {
        &self.operands[self.operand_ranges[inst.index()].clone()]
    }

    fn inst_clobbers(&self, inst: Inst) -> PRegSet {
        self.clobbers[inst.index()]
    }

    fn num_vregs(&self) -> usize {
        self.vreg_types.len()
    }

    fn is_ret(&self, inst: Inst) -> bool {
        self.insts[inst.index()].is_term() == MachTerminator::Ret
    }

    fn is_branch(&self, inst: Inst) -> bool {
        matches!(self.insts[inst.index()].is_term(),
            MachTerminator::Branch | MachTerminator::Cond)
    }

    fn branch_blockparams(&self, block: Block, inst: Inst, succ: usize) -> &[VReg] {
        // Return vregs passed to successor block
        ...
    }

    fn spillslot_size(&self, regclass: RegClass) -> usize {
        self.abi.slot_size(regclass)
    }
}
```

### Zig Implementation

```zig
/// Adapter that makes VCode implement regalloc.Function interface
pub const VCodeAdapter = struct {
    vcode: *const VCode,

    pub fn init(vcode: *const VCode) VCodeAdapter {
        return .{ .vcode = vcode };
    }

    // Implement all Function interface methods
    pub fn numInsts(self: VCodeAdapter) usize {
        return self.vcode.insts.items.len;
    }

    pub fn numBlocks(self: VCodeAdapter) usize {
        return self.vcode.block_ranges.items.len;
    }

    pub fn entryBlock(self: VCodeAdapter) regalloc.Block {
        _ = self;
        return regalloc.Block.new(0);
    }

    pub fn blockInsns(self: VCodeAdapter, block: regalloc.Block) regalloc.InstRange {
        const range = self.vcode.block_ranges.items[block.idx()];
        return regalloc.InstRange.new(
            regalloc.Inst.new(range.start),
            regalloc.Inst.new(range.end),
        );
    }

    pub fn blockSuccs(self: VCodeAdapter, block: regalloc.Block) []const regalloc.Block {
        const range = self.vcode.block_succ_range.items[block.idx()];
        return self.vcode.block_succs.items[range.start..range.end];
    }

    pub fn blockPreds(self: VCodeAdapter, block: regalloc.Block) []const regalloc.Block {
        const range = self.vcode.block_pred_range.items[block.idx()];
        return self.vcode.block_preds.items[range.start..range.end];
    }

    pub fn blockParams(self: VCodeAdapter, block: regalloc.Block) []const regalloc.VReg {
        const range = self.vcode.block_params_range.items[block.idx()];
        return self.vcode.block_params.items[range.start..range.end];
    }

    pub fn instOperands(self: VCodeAdapter, inst: regalloc.Inst) []const regalloc.Operand {
        const range = self.vcode.operand_ranges.items[inst.idx()];
        return self.vcode.operands.items[range.start..range.end];
    }

    pub fn instClobbers(self: VCodeAdapter, inst: regalloc.Inst) regalloc.PRegSet {
        return self.vcode.clobbers.items[inst.idx()];
    }

    pub fn numVregs(self: VCodeAdapter) usize {
        return self.vcode.vreg_types.items.len;
    }

    pub fn isRet(self: VCodeAdapter, inst: regalloc.Inst) bool {
        return self.vcode.insts.items[inst.idx()].isTerm() == .ret;
    }

    pub fn isBranch(self: VCodeAdapter, inst: regalloc.Inst) bool {
        const term = self.vcode.insts.items[inst.idx()].isTerm();
        return term == .branch or term == .cond;
    }

    pub fn branchBlockparams(
        self: VCodeAdapter,
        block: regalloc.Block,
        inst: regalloc.Inst,
        succ_idx: usize,
    ) []const regalloc.VReg {
        // Look up branch arguments for this successor
        ...
    }

    pub fn spillslotSize(self: VCodeAdapter, regclass: regalloc.RegClass) usize {
        return self.vcode.abi.slotSize(regclass);
    }
};
```

### Task Checklist

- [ ] **7.2.1** Create `vcode_adapter.zig`
- [ ] **7.2.2** Implement `VCodeAdapter` struct
- [ ] **7.2.3** Implement all `regalloc.Function` interface methods
- [ ] **7.2.4** Add `branchBlockparams()` with proper block param tracking
- [ ] **7.2.5** Wire to VCode in machinst/vcode.zig
- [ ] **7.2.6** Add unit tests with mock VCode

---

## Phase 7.3: Emission with Regalloc Output

**Cranelift Source**: `machinst/vcode.rs` `VCode::emit()` lines 755-1050
**Target**: `compiler/codegen/native/machinst/vcode.zig` (extend existing)
**Estimated LOC**: ~500

### The Key Challenge

After regalloc, we have:
- `VCode` with instructions using virtual registers
- `regalloc::Output` with:
  - `allocs`: Physical register for each virtual register operand
  - `edits`: Move/spill/reload instructions to insert
  - `num_spillslots`: Stack space needed

We must emit code that:
1. Replaces virtual registers with physical registers
2. Inserts regalloc edits (moves) at the right points
3. Generates prologue/epilogue with correct frame size

### Cranelift emit() Flow

```rust
pub fn emit(
    &self,
    regalloc: &regalloc2::Output,
    want_disasm: bool,
    emit_info: &I::Info,
    ctrl_plane: &mut ControlPlane,
) -> EmitResult {
    // 1. Create buffer
    let mut buffer = MachBuffer::new();

    // 2. Compute frame layout with spill slots
    let frame_layout = self.abi.compute_frame_layout(regalloc.num_spillslots);

    // 3. Emit prologue
    for inst in self.abi.gen_prologue(&frame_layout) {
        inst.emit(&mut buffer, emit_info, &mut state);
    }

    // 4. Emit blocks
    for block in self.block_order.iter() {
        // Process instructions with regalloc edits interleaved
        for inst_or_edit in regalloc.block_insts_and_edits(self, block) {
            match inst_or_edit {
                InstOrEdit::Inst(iix) => {
                    // Get allocated registers for this instruction
                    let allocs = regalloc.inst_allocs(iix);
                    // Emit instruction with physical registers
                    self.insts[iix].emit_with_allocs(&mut buffer, allocs, emit_info);
                }
                InstOrEdit::Edit(Edit::Move { from, to, .. }) => {
                    // Emit move instruction
                    let mv = I::gen_move(to, from, self.vreg_types[to.vreg()]);
                    mv.emit(&mut buffer, emit_info, &mut state);
                }
            }
        }
    }

    // 5. Emit epilogue (before returns)
    // ...

    // 6. Finalize buffer
    buffer.finish()
}
```

### Zig Implementation

```zig
/// Emit machine code from VCode using regalloc output
pub fn emit(
    self: *const VCode,
    allocator: std.mem.Allocator,
    regalloc_output: *const regalloc.Output,
    emit_info: EmitInfo,
) !EmitResult {
    var buffer = MachBuffer.init(allocator);
    errdefer buffer.deinit();

    // 1. Compute frame layout
    const frame_layout = self.abi.computeFrameLayout(regalloc_output.num_spillslots);

    // 2. Emit prologue
    const prologue_insts = self.abi.genPrologue(&frame_layout);
    for (prologue_insts) |inst| {
        try inst.emit(&buffer, emit_info);
    }

    // 3. Emit blocks in order
    for (self.block_order.items) |block_idx| {
        const block = regalloc.Block.new(block_idx);

        // Iterate instructions with regalloc edits
        var iter = regalloc_output.blockInstsAndEdits(self.adapter(), block);
        while (iter.next()) |item| {
            switch (item) {
                .inst => |iix| {
                    // Get physical register allocations
                    const allocs = regalloc_output.instAllocs(iix);

                    // Emit instruction with allocated registers
                    const inst = self.insts.items[iix.idx()];
                    try inst.emitWithAllocs(&buffer, allocs, emit_info);
                },
                .edit => |edit| {
                    switch (edit) {
                        .move => |mv| {
                            // Generate move instruction
                            const move_inst = MachInst.genMove(
                                mv.to_alloc,
                                mv.from_alloc,
                                self.vreg_types.items[mv.to_vreg.vreg()],
                            );
                            try move_inst.emit(&buffer, emit_info);
                        },
                    }
                },
            }
        }
    }

    // 4. Emit epilogue (done per-return in block loop)

    // 5. Finalize
    return EmitResult{
        .buffer = try buffer.finish(),
        .frame_size = frame_layout.frame_size,
        .bb_offsets = try self.computeBBOffsets(allocator, &buffer),
    };
}
```

### MachInst.emitWithAllocs()

Each backend's MachInst needs a method to emit with physical registers:

```zig
// In isa/aarch64/inst/mod.zig
pub fn emitWithAllocs(
    self: Inst,
    buffer: *MachBuffer,
    allocs: []const regalloc.Allocation,
    emit_info: EmitInfo,
) !void {
    // Replace virtual registers with physical registers from allocs
    var inst_copy = self;

    // Substitute each operand
    var alloc_idx: usize = 0;
    inline for (std.meta.fields(@TypeOf(self))) |field| {
        if (isRegField(field)) {
            const alloc = allocs[alloc_idx];
            @field(inst_copy, field.name) = alloc.asReg().?;
            alloc_idx += 1;
        }
    }

    // Emit with physical registers
    return inst_copy.emit(buffer, emit_info);
}
```

### Task Checklist

- [ ] **7.3.1** Add `emit()` method to VCode in machinst/vcode.zig
- [ ] **7.3.2** Implement `blockInstsAndEdits()` iterator
- [ ] **7.3.3** Add `emitWithAllocs()` to ARM64 MachInst
- [ ] **7.3.4** Add `emitWithAllocs()` to x64 MachInst
- [ ] **7.3.5** Implement `genMove()` for ARM64
- [ ] **7.3.6** Implement `genMove()` for x64
- [ ] **7.3.7** Wire frame layout computation
- [ ] **7.3.8** Add prologue/epilogue emission
- [ ] **7.3.9** Add unit tests

---

## Phase 7.4: Driver Integration

**Target**: `compiler/driver.zig`
**Estimated LOC**: ~200

### Current State

```zig
// driver.zig line 248-265
// Native target: AOT compilation path (Wasm → Native)
// NOTE: Native codegen is being rewritten...
_ = source_file;
_ = source_text;
pipeline_debug.log(.codegen, "driver: native codegen not yet implemented", .{});
return error.NativeCodegenNotImplemented;
```

### Target State

```zig
fn generateNativeCode(
    self: *Driver,
    wasm_bytes: []const u8,
    source_file: []const u8,
) ![]u8 {
    pipeline_debug.log(.codegen, "driver: generating native code for {s}", .{source_file});

    // 1. Parse Wasm module
    var wasm_module = try wasm_parser.parse(self.allocator, wasm_bytes);
    defer wasm_module.deinit(self.allocator);

    // 2. Select target ISA
    const isa: compile.TargetIsa = switch (self.target.arch) {
        .aarch64 => .{ .aarch64 = &aarch64.default_backend },
        .amd64 => .{ .x64 = &x64.default_backend },
        else => return error.UnsupportedTarget,
    };

    // 3. Compile each function
    var compiled_funcs = std.ArrayListUnmanaged(CompiledFunc){};
    defer compiled_funcs.deinit(self.allocator);

    for (wasm_module.functions, 0..) |wasm_func, i| {
        // 3a. Translate Wasm → CLIF
        var clif_func = try wasm_to_clif.translate(
            self.allocator,
            &wasm_func,
            &wasm_module,
        );
        defer clif_func.deinit(self.allocator);

        // 3b. Compile CLIF → native
        var ctrl_plane = CtrlPlane.init();
        const compiled = try compile.compile(
            self.allocator,
            &clif_func,
            isa,
            &ctrl_plane,
        );

        try compiled_funcs.append(self.allocator, .{
            .name = wasm_func.name,
            .code = compiled.buffer.data(),
            .relocations = compiled.buffer.relocations(),
        });
    }

    // 4. Link into object file
    return switch (self.target.os) {
        .macos => try macho.link(self.allocator, compiled_funcs.items),
        .linux => try elf.link(self.allocator, compiled_funcs.items),
        else => error.UnsupportedOS,
    };
}
```

### Full Pipeline

```
Source Code (.cot)
       ↓
    Scanner
       ↓
    Parser → AST
       ↓
    Checker → Typed AST
       ↓
    Lowerer → IR
       ↓
    SSA Builder → SSA
       ↓
    Wasm Passes (schedule, layout, lower_wasm)
       ↓
    Wasm Codegen → Wasm bytes
       ↓
    === NATIVE PATH STARTS HERE ===
       ↓
    wasm_parser → WasmModule
       ↓
    wasm_to_clif → CLIF Function
       ↓
    compile.compile():
       ├─ BlockLoweringOrder
       ├─ Lower::lower() → VCode (vregs)
       ├─ regalloc::run() → Output
       └─ VCode::emit() → MachBuffer
       ↓
    macho/elf → Object file
       ↓
    clang linker → Executable
```

### Task Checklist

- [ ] **7.4.1** Add native codegen imports to driver.zig
- [ ] **7.4.2** Implement `generateNativeCode()` function
- [ ] **7.4.3** Wire Wasm parser
- [ ] **7.4.4** Wire Wasm→CLIF translator
- [ ] **7.4.5** Wire compile.compile()
- [ ] **7.4.6** Wire object file generation
- [ ] **7.4.7** Add integration tests

---

## Phase 7.5: End-to-End Tests

### Test 1: Return 42

```cot
fn main() i32 {
    return 42;
}
```

Expected:
- Compiles to Wasm
- Translates to CLIF: `return iconst.i32 42`
- Lowers to ARM64: `mov w0, #42; ret`
- Links to Mach-O/ELF
- Executes and returns 42

### Test 2: Arithmetic

```cot
fn add(a: i32, b: i32) i32 {
    return a + b;
}

fn main() i32 {
    return add(10, 32);
}
```

### Test 3: Control Flow

```cot
fn max(a: i32, b: i32) i32 {
    if (a > b) {
        return a;
    }
    return b;
}

fn main() i32 {
    return max(10, 42);
}
```

### Test 4: Function Calls

```cot
fn fib(n: i32) i32 {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

fn main() i32 {
    return fib(10);  // 55
}
```

### Test 5: Memory

```cot
fn sum_array(arr: *i32, len: i32) i32 {
    var total: i32 = 0;
    var i: i32 = 0;
    while (i < len) {
        total = total + arr[i];
        i = i + 1;
    }
    return total;
}
```

### Task Checklist

- [ ] **7.5.1** Create test/native/ directory
- [ ] **7.5.2** Test: return 42
- [ ] **7.5.3** Test: arithmetic
- [ ] **7.5.4** Test: control flow (if/else)
- [ ] **7.5.5** Test: loops (while)
- [ ] **7.5.6** Test: function calls
- [ ] **7.5.7** Test: recursion (fib)
- [ ] **7.5.8** Test: memory operations
- [ ] **7.5.9** Test: both ARM64 and x64

---

## Phase 7.6: Object File Generation

### Mach-O (macOS)

**Existing**: `compiler/codegen/native/macho.zig`

Needs:
- [ ] **7.6.1** Accept compiled functions with relocations
- [ ] **7.6.2** Generate proper symbol table
- [ ] **7.6.3** Handle relocations (adrp, add, bl)
- [ ] **7.6.4** Generate proper __text section

### ELF (Linux)

**Existing**: `compiler/codegen/native/elf.zig`

Needs:
- [ ] **7.6.5** Accept compiled functions with relocations
- [ ] **7.6.6** Generate proper symbol table
- [ ] **7.6.7** Handle relocations (got, plt)
- [ ] **7.6.8** Generate proper .text section

---

## Implementation Order

### Week 1: Core Infrastructure

| Day | Task | Description |
|-----|------|-------------|
| 1 | 7.1.1-7.1.3 | Create compile.zig skeleton |
| 2 | 7.2.1-7.2.4 | Create VCodeAdapter |
| 3 | 7.2.5-7.2.6 | Wire adapter, add tests |
| 4 | 7.3.1-7.3.3 | Add emit() to VCode |
| 5 | 7.3.4-7.3.6 | Add emitWithAllocs() to backends |

### Week 2: Integration

| Day | Task | Description |
|-----|------|-------------|
| 6 | 7.3.7-7.3.9 | Frame layout, prologue/epilogue |
| 7 | 7.4.1-7.4.4 | Driver integration |
| 8 | 7.4.5-7.4.7 | Complete driver, add tests |
| 9 | 7.5.1-7.5.4 | Basic end-to-end tests |
| 10 | 7.5.5-7.5.9 | Complex end-to-end tests |

### Week 3: Polish

| Day | Task | Description |
|-----|------|-------------|
| 11 | 7.6.1-7.6.4 | Mach-O improvements |
| 12 | 7.6.5-7.6.8 | ELF improvements |
| 13 | Testing | Full test suite |
| 14 | Audit | Create audit documents |
| 15 | Commit | Final commit |

---

## Verification Commands

```bash
# Run all tests
zig build test

# Test specific module
zig test compiler/codegen/native/compile.zig

# End-to-end test
./zig-out/bin/cot test/native/return42.cot -o test42
./test42
echo $?  # Should print 42

# Debug output
COT_DEBUG=codegen ./zig-out/bin/cot test.cot -o test
```

---

## References

### Cranelift Source Files

| File | Purpose |
|------|---------|
| `context.rs` | Compilation context and orchestration |
| `machinst/compile.rs` | Main compile() function |
| `machinst/vcode.rs` | VCode and emit() |
| `machinst/lower.rs` | Lowering framework |
| `machinst/buffer.rs` | MachBuffer |
| `isa/aarch64/mod.rs` | ARM64 backend entry |
| `isa/x64/mod.rs` | x64 backend entry |

### Cot Source Files

| File | Purpose |
|------|---------|
| `driver.zig` | Compilation driver |
| `codegen/native/compile.zig` | NEW: Main compile function |
| `codegen/native/vcode_adapter.zig` | NEW: VCode→regalloc adapter |
| `codegen/native/machinst/vcode.zig` | VCode with emit() |
| `codegen/native/regalloc/regalloc.zig` | Register allocator |
| `codegen/native/isa/aarch64/` | ARM64 backend |
| `codegen/native/isa/x64/` | x64 backend |

---

## Success Criteria

Phase 7 is complete when:

1. ✅ `cot test.cot -o test` produces a working native executable
2. ✅ All 5 end-to-end tests pass on ARM64
3. ✅ All 5 end-to-end tests pass on x64
4. ✅ Object files link correctly with system linker
5. ✅ No regressions in Wasm output
6. ✅ Audit documents created for all new code
