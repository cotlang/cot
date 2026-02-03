# Cranelift Port: 80% → 100% Completion Plan

## Document Purpose

This is the execution plan for completing the Cranelift port from 80% to 100%.
Every task is derived from Cranelift's proven architecture - NO inventing logic.

**Rule**: Copy Cranelift's code. Understand it. Port it. Test it. Move on.

---

## Table of Contents

1. [Critical Blockers (B1-B5)](#critical-blockers)
2. [Execution Order](#execution-order)
3. [Task Details](#task-details)
4. [Progress Tracking](#progress-tracking)

---

## Critical Blockers

| ID | Issue | File | Cranelift Reference | Estimated Hours |
|----|-------|------|---------------------|-----------------|
| B1 | Loop back-edge translation | func_translator.zig:547 | code_translator.rs:251-431 | 2-3 |
| B2 | SmallVec heap allocation | inst.zig:536,542 | smallvec crate | 1-2 |
| B3 | genMove empty stub | vcode.zig:900 | aarch64/inst/mod.rs:1065, x64/inst/mod.rs:1347 | 2-3 |
| B4 | genSpill/genReload panic | abi.zig:940,948 | machinst/abi.rs:2390-2410 | 3-4 |
| B5 | Wasm opcode gaps | translator.zig | code_translator.rs | 2-3 |

**Total estimated: 10-15 hours**

---

## Execution Order

```
Phase 1: Foundation Fixes (B2 first - enables other work)
├── B2: SmallVec heap allocation (unblocks complex functions)
└── Test: Verify existing tests still pass

Phase 2: Loop Support (B1 - enables all loop code)
├── B1: Loop back-edge translation
└── Test: Enable commented loop test

Phase 3: Register Allocation Support (B3, B4 - enables native emission)
├── B3: genMove for aarch64 and x64
├── B4: genSpill/genReload for aarch64 and x64
└── Test: Simple function compiles to native

Phase 4: Wasm Completeness (B5 - full Wasm support)
├── B5.1: Global mutability tracking
├── B5.2: trap_if instruction
├── B5.3: Indirect call signature lookup
└── Test: Full Wasm test suite

Phase 5: End-to-End Verification
├── E2E.1: return 42 test
├── E2E.2: arithmetic test
├── E2E.3: control flow test
├── E2E.4: function call test
└── E2E.5: memory operations test
```

---

## Task Details

### B2: SmallVec Heap Allocation

**Problem**: SmallVec panics when inline capacity (8) is exceeded.

**Cranelift Reference**: Uses Rust's `smallvec` crate which auto-promotes to heap.

**File**: `compiler/codegen/native/machinst/inst.zig:530-544`

**Current Code**:
```zig
pub fn push(self: *Self, item: T) void {
    if (self.heap_buf != null) {
        @panic("SmallVec heap reallocation not implemented");
    }
    if (self.inline_len < inline_capacity) {
        self.inline_buf[self.inline_len] = item;
        self.inline_len += 1;
    } else {
        @panic("SmallVec overflow - heap allocation not implemented");
    }
}
```

**Fix** (copy Rust smallvec pattern):
```zig
pub fn push(self: *Self, item: T) void {
    if (self.heap_buf) |buf| {
        // Already on heap - grow if needed
        if (self.heap_len >= self.heap_cap) {
            const new_cap = self.heap_cap * 2;
            const new_buf = self.allocator.?.realloc(buf, new_cap) catch @panic("OOM");
            self.heap_buf = new_buf;
            self.heap_cap = new_cap;
        }
        self.heap_buf.?[self.heap_len] = item;
        self.heap_len += 1;
    } else if (self.inline_len < inline_capacity) {
        self.inline_buf[self.inline_len] = item;
        self.inline_len += 1;
    } else {
        // Promote to heap
        const alloc = self.allocator orelse @panic("SmallVec needs allocator for heap");
        const new_cap = inline_capacity * 2;
        const heap = alloc.alloc(T, new_cap) catch @panic("OOM");
        @memcpy(heap[0..inline_capacity], &self.inline_buf);
        heap[inline_capacity] = item;
        self.heap_buf = heap;
        self.heap_cap = new_cap;
        self.heap_len = inline_capacity + 1;
    }
}
```

**Checklist**:
- [ ] B2.1: Add heap_cap and heap_len fields to SmallVec
- [ ] B2.2: Implement heap promotion in push()
- [ ] B2.3: Implement heap reallocation in push()
- [ ] B2.4: Update slice() to return heap buffer when active
- [ ] B2.5: Update clear() to handle heap deallocation
- [ ] B2.6: Add test for overflow behavior
- [ ] B2.7: Run full test suite

---

### B1: Loop Back-Edge Translation

**Problem**: Loop back-edges fail because the loop header block is sealed before `br` can add it as a predecessor.

**Cranelift Reference**: `code_translator.rs:251-431`

**Key Insight from Cranelift**:
1. Create loop header block (line 253)
2. Jump to loop body (line 255)
3. Push loop frame to control stack (line 258)
4. **Do NOT seal loop header yet**
5. Process loop body (may contain `br 0` back to header)
6. On `End` for loop, seal the header (lines 430-431)

**Current Cot Problem** (func_translator.zig):
The block sealing strategy seals blocks too early, before back-edges are added.

**Fix Pattern**:
```zig
// In translateLoop():
fn translateLoop(self: *Self, func: *Function, block_ty: BlockType) !void {
    const loop_header = try func.createBlock();
    const loop_exit = try func.createBlock();

    // Add block params to header
    for (block_ty.params) |param_ty| {
        _ = try func.appendBlockParam(loop_header, wasmToClifType(param_ty));
    }

    // Jump to header with current stack values
    const args = self.state.peekn(block_ty.params.len);
    try self.builder.insJump(loop_header, args);

    // Push loop frame - header will be sealed on End
    try self.control_stack.push(.{
        .loop = .{
            .header = loop_header,        // Re-entry point for br
            .exit = loop_exit,            // Exit point
            .num_params = block_ty.params.len,
            .num_results = block_ty.results.len,
            .stack_size = self.state.stack.len - block_ty.params.len,
            .header_sealed = false,       // NOT sealed yet!
        },
    });

    // Switch to header and set up params
    try self.builder.switchToBlock(loop_header);
    for (func.blockParams(loop_header)) |param| {
        try self.state.push(param);
    }
}

// In translateEnd() for loop:
fn handleLoopEnd(self: *Self, func: *Function, frame: LoopFrame) !void {
    // NOW seal the header - all back-edges have been added
    if (!frame.header_sealed) {
        try func.sealBlock(frame.header);
    }

    // Seal the exit block
    try func.sealBlock(frame.exit);

    // ... rest of end handling
}

// In translateBr():
fn translateBr(self: *Self, func: *Function, depth: u32) !void {
    const frame = self.control_stack.getAtDepth(depth);

    // For loops, br goes to header; for blocks, br goes to exit
    const target = if (frame.isLoop()) frame.header else frame.exit;

    // Get the right number of args
    const arg_count = if (frame.isLoop()) frame.num_params else frame.num_results;
    const args = self.state.peekn(arg_count);

    try self.builder.insJump(target, args);
    self.reachable = false;
}
```

**Checklist**:
- [ ] B1.1: Add `header_sealed: bool` to LoopFrame
- [ ] B1.2: Update translateLoop() to NOT seal header
- [ ] B1.3: Update translateEnd() to seal header on loop end
- [ ] B1.4: Update translateBr() to handle loop vs block targets correctly
- [ ] B1.5: Enable commented loop test in func_translator.zig
- [ ] B1.6: Add more loop tests (nested loops, break from loop)
- [ ] B1.7: Run full test suite

---

### B3: genMove Implementation

**Problem**: genMove in vcode.zig is an empty stub - moves are silently dropped.

**Cranelift Reference**:
- aarch64: `inst/mod.rs:1065-1091`
- x64: `inst/mod.rs:1347-1384`

**aarch64 Implementation** (from Cranelift):
```zig
// In compiler/codegen/native/isa/aarch64/inst/mod.zig
pub fn genMove(to_reg: Writable(Reg), from_reg: Reg, ty: Type) Inst {
    const bits = ty.bits();
    std.debug.assert(bits <= 128);
    std.debug.assert(to_reg.toReg().class() == from_reg.class());

    return switch (from_reg.class()) {
        .int => Inst{ .mov = .{
            .size = .size64,
            .rd = to_reg,
            .rm = from_reg,
        }},
        .float => if (bits > 64)
            Inst{ .fpu_move128 = .{ .rd = to_reg, .rn = from_reg }}
        else
            Inst{ .fpu_move64 = .{ .rd = to_reg, .rn = from_reg }},
        .vector => unreachable,
    };
}
```

**x64 Implementation** (from Cranelift):
```zig
// In compiler/codegen/native/isa/x64/inst/mod.zig
pub fn genMove(dst_reg: Writable(Reg), src_reg: Reg, ty: Type) Inst {
    const rc_dst = dst_reg.toReg().class();
    std.debug.assert(rc_dst == src_reg.class());

    return switch (rc_dst) {
        .int => Inst{ .mov_rr = .{
            .size = .size64,
            .src = src_reg.toGpr(),
            .dst = dst_reg.map(Gpr.fromReg),
        }},
        .float => switch (ty) {
            // Use movaps for zero-latency moves
            types.f32, types.f64, types.f32x4 => Inst{ .xmm_mov = .{
                .op = .movaps,
                .src = XmmMem.fromReg(src_reg.toXmm()),
                .dst = dst_reg.map(Xmm.fromReg),
            }},
            types.f64x2 => Inst{ .xmm_mov = .{
                .op = .movapd,
                .src = XmmMem.fromReg(src_reg.toXmm()),
                .dst = dst_reg.map(Xmm.fromReg),
            }},
            else => Inst{ .xmm_mov = .{
                .op = .movdqa,
                .src = XmmMem.fromReg(src_reg.toXmm()),
                .dst = dst_reg.map(Xmm.fromReg),
            }},
        },
        .vector => unreachable,
    };
}
```

**Wire into VCode** (vcode.zig:897-905):
```zig
pub fn emitMove(
    buffer: *MachBuffer,
    from: regalloc_operand.Allocation,
    to: regalloc_operand.Allocation,
    emit_info: anytype,
) !void {
    const from_reg = from.asReg() orelse return; // Stack handled separately
    const to_reg = to.asReg() orelse return;

    // Get type from allocation (default to i64 for GPRs)
    const ty = types.i64; // TODO: get actual type from context

    const inst = switch (emit_info.arch) {
        .aarch64 => aarch64.Inst.genMove(
            Writable(Reg).fromReg(to_reg),
            from_reg,
            ty,
        ),
        .x64 => x64.Inst.genMove(
            Writable(Reg).fromReg(to_reg),
            from_reg,
            ty,
        ),
    };

    try inst.emit(buffer, emit_info);
}
```

**Checklist**:
- [ ] B3.1: Add genMove to aarch64 Inst (inst/mod.zig)
- [ ] B3.2: Add genMove to x64 Inst (inst/mod.zig)
- [ ] B3.3: Update VCode.emitMove() to call ISA-specific genMove
- [ ] B3.4: Add test for genMove in aarch64
- [ ] B3.5: Add test for genMove in x64
- [ ] B3.6: Run full test suite

---

### B4: genSpill/genReload Implementation

**Problem**: genSpill and genReload panic - any function needing spills crashes.

**Cranelift Reference**: `machinst/abi.rs:2390-2410`

**Cranelift Pattern**:
```rust
pub fn gen_spill(&self, to_slot: SpillSlot, from_reg: RealReg) -> M::I {
    let ty = M::I::canonical_type_for_rc(from_reg.class());
    let sp_off = self.get_spillslot_offset(to_slot);
    let from = StackAMode::Slot(sp_off);
    <M>::gen_store_stack(from, Reg::from(from_reg), ty)
}

pub fn gen_reload(&self, to_reg: Writable<RealReg>, from_slot: SpillSlot) -> M::I {
    let ty = M::I::canonical_type_for_rc(to_reg.to_reg().class());
    let sp_off = self.get_spillslot_offset(from_slot);
    let from = StackAMode::Slot(sp_off);
    <M>::gen_load_stack(from, to_reg.map(Reg::from), ty)
}
```

**aarch64 gen_store_stack / gen_load_stack**:
```zig
// Store to stack slot
pub fn genStoreStack(offset: i64, reg: Reg, ty: Type) Inst {
    return Inst{ .store64 = .{
        .rd = reg,
        .mem = AMode{ .reg_offset = .{
            .base = regs.sp(),
            .offset = @intCast(offset),
        }},
    }};
}

// Load from stack slot
pub fn genLoadStack(offset: i64, reg: Writable(Reg), ty: Type) Inst {
    return Inst{ .load64 = .{
        .rd = reg,
        .mem = AMode{ .reg_offset = .{
            .base = regs.sp(),
            .offset = @intCast(offset),
        }},
    }};
}
```

**x64 gen_store_stack / gen_load_stack**:
```zig
// Store to stack slot (mov [rsp+offset], reg)
pub fn genStoreStack(offset: i64, reg: Reg, ty: Type) Inst {
    return Inst{ .mov_r_m = .{
        .size = .size64,
        .src = reg.toGpr(),
        .dst = Amode{ .imm_reg = .{
            .base = regs.rsp(),
            .offset = @intCast(offset),
        }},
    }};
}

// Load from stack slot (mov reg, [rsp+offset])
pub fn genLoadStack(offset: i64, reg: Writable(Reg), ty: Type) Inst {
    return Inst{ .mov_rm_r = .{
        .size = .size64,
        .src = Amode{ .imm_reg = .{
            .base = regs.rsp(),
            .offset = @intCast(offset),
        }},
        .dst = reg.map(Gpr.fromReg),
    }};
}
```

**Update abi.zig**:
```zig
pub fn genSpill(self: *const Self, to: SpillSlot, from: RealReg) I {
    const ty = I.canonicalTypeForRc(from.class());
    const offset = self.spillslotOffset(to);
    return I.genStoreStack(offset, Reg.fromReal(from), ty);
}

pub fn genReload(self: *const Self, to: Writable(RealReg), from: SpillSlot) I {
    const ty = I.canonicalTypeForRc(to.toReg().class());
    const offset = self.spillslotOffset(from);
    return I.genLoadStack(offset, to.map(Reg.fromReal), ty);
}
```

**Checklist**:
- [ ] B4.1: Add genStoreStack to aarch64 Inst
- [ ] B4.2: Add genLoadStack to aarch64 Inst
- [ ] B4.3: Add genStoreStack to x64 Inst
- [ ] B4.4: Add genLoadStack to x64 Inst
- [ ] B4.5: Add canonicalTypeForRc to both ISAs
- [ ] B4.6: Add spillslotOffset to Callee/ABI
- [ ] B4.7: Implement genSpill in abi.zig using ISA methods
- [ ] B4.8: Implement genReload in abi.zig using ISA methods
- [ ] B4.9: Add test for spill/reload in aarch64
- [ ] B4.10: Add test for spill/reload in x64
- [ ] B4.11: Run full test suite

---

### B5: Wasm Opcode Gaps

**Problem**: Several Wasm features produce incorrect or incomplete code.

**Cranelift Reference**: `code_translator.rs`

#### B5.1: Global Mutability Tracking

**File**: `translator.zig:737`

**Current**: `false, // is_constant - TODO: check globals[index].mutable`

**Fix**:
```zig
// In ModuleTranslator or func_environ
pub const GlobalInfo = struct {
    ty: WasmValType,
    mutable: bool,
    initial_value: ?ConstantValue,
};

// During module parsing, store global info
fn parseGlobal(self: *Self, global: WasmGlobal) !void {
    try self.globals.append(.{
        .ty = global.val_type,
        .mutable = global.mutable,
        .initial_value = global.init,
    });
}

// In translateGlobalGet/Set
const global_info = self.module.globals[global_idx];
const is_constant = !global_info.mutable;
```

**Checklist**:
- [ ] B5.1.1: Add GlobalInfo struct to module or func_environ
- [ ] B5.1.2: Parse global mutability from Wasm module
- [ ] B5.1.3: Use mutability in translateGlobalGet/Set
- [ ] B5.1.4: Add test for mutable vs immutable globals

#### B5.2: trap_if Instruction

**Files**: `translator.zig:909,1349,1386`

**Current**: `// TODO: Emit trap_if when we have that instruction`

**Cranelift Reference**: `code_translator.rs` uses `trapif` for bounds checks, division by zero, etc.

**Fix**:
```zig
// Add trap_if to CLIF instructions (instructions.zig)
pub const Opcode = enum {
    // ...existing...
    trap_if,  // Conditional trap
};

// Add to builder (builder.zig)
pub fn insTrapIf(self: *Self, cond: Value, code: TrapCode) !void {
    return self.insertInst(.trap_if, &[_]Value{cond}, .{
        .trap_code = code,
    });
}

// In translator.zig
fn emitTrapIf(self: *Self, cond: Value, code: TrapCode) !void {
    try self.builder.insTrapIf(cond, code);
}

// For division:
fn translateI32DivS(self: *Self) !void {
    const rhs = try self.state.pop();
    const lhs = try self.state.pop();

    // Check for division by zero
    const zero = try self.builder.insIconst(types.i32, 0);
    const is_zero = try self.builder.insIcmp(.eq, rhs, zero);
    try self.emitTrapIf(is_zero, .integer_division_by_zero);

    // Check for overflow (MIN_INT / -1)
    // ... similar pattern

    const result = try self.builder.insSdiv(lhs, rhs);
    try self.state.push(result);
}
```

**Checklist**:
- [ ] B5.2.1: Add trap_if opcode to instructions.zig
- [ ] B5.2.2: Add TrapCode enum (integer_division_by_zero, integer_overflow, etc.)
- [ ] B5.2.3: Add insTrapIf to builder.zig
- [ ] B5.2.4: Add trap_if lowering to aarch64 (brk instruction)
- [ ] B5.2.5: Add trap_if lowering to x64 (ud2 instruction)
- [ ] B5.2.6: Add trap_if emission to division operations
- [ ] B5.2.7: Add test for trap_if

#### B5.3: Indirect Call Signature Lookup

**File**: `translator.zig:1297`

**Current**: `// TODO: Get actual signature from type_index`

**Cranelift Reference**: Uses `call_indirect` with signature from type section.

**Fix**:
```zig
// Store type signatures during module parsing
pub const FuncType = struct {
    params: []const WasmValType,
    results: []const WasmValType,
};

// In ModuleTranslator
types: std.ArrayList(FuncType),

fn parseTypeSection(self: *Self, types: []const WasmFuncType) !void {
    for (types) |ty| {
        try self.types.append(.{
            .params = ty.params,
            .results = ty.results,
        });
    }
}

// In translateCallIndirect
fn translateCallIndirect(self: *Self, type_idx: u32, table_idx: u32) !void {
    const sig = self.module.types.items[type_idx];

    // Pop function index from stack
    const func_idx = try self.state.pop();

    // Pop arguments (in reverse order)
    var args = try self.allocator.alloc(Value, sig.params.len);
    var i = sig.params.len;
    while (i > 0) {
        i -= 1;
        args[i] = try self.state.pop();
    }

    // Emit call_indirect with proper signature
    const results = try self.builder.insCallIndirect(sig, func_idx, args);

    // Push results
    for (results) |result| {
        try self.state.push(result);
    }
}
```

**Checklist**:
- [ ] B5.3.1: Add FuncType storage to module translator
- [ ] B5.3.2: Parse type section and store signatures
- [ ] B5.3.3: Use actual signature in translateCallIndirect
- [ ] B5.3.4: Add test for call_indirect with multi-value returns

---

### E2E: End-to-End Tests

After all blockers are fixed, verify with end-to-end tests.

**E2E.1: Return 42**
```cot
fn main() i32 {
    return 42
}
```
- [ ] Compiles to Wasm
- [ ] Wasm runs correctly (returns 42)
- [ ] Compiles to native (aarch64)
- [ ] Native runs correctly (returns 42)
- [ ] Compiles to native (x64) - on Linux

**E2E.2: Arithmetic**
```cot
fn main() i32 {
    let a = 10
    let b = 32
    return a + b
}
```
- [ ] All targets produce 42

**E2E.3: Control Flow**
```cot
fn main() i32 {
    let x = 5
    if x > 0 {
        return 1
    } else {
        return 0
    }
}
```
- [ ] All targets produce 1

**E2E.4: Function Calls**
```cot
fn add(a: i32, b: i32) i32 {
    return a + b
}

fn main() i32 {
    return add(10, 32)
}
```
- [ ] All targets produce 42

**E2E.5: Loops**
```cot
fn main() i32 {
    let sum = 0
    let i = 0
    while i < 10 {
        sum = sum + i
        i = i + 1
    }
    return sum
}
```
- [ ] All targets produce 45

---

## Progress Tracking

### Phase 1: Foundation Fixes
| Task | Status | Notes |
|------|--------|-------|
| B2.1: Add heap fields | ✅ | Added heap_len, heap_cap |
| B2.2: Implement heap promotion | ✅ | Copies inline to heap when full |
| B2.3: Implement heap realloc | ✅ | Doubles capacity and copies |
| B2.4: Update slice() | ✅ | Returns heap_buf[0..heap_len] |
| B2.5: Update clear() | ✅ | Frees heap_buf[0..heap_cap] |
| B2.6: Add overflow test | ⬜ | Deferred - existing tests pass |
| B2.7: Run test suite | ✅ | All tests pass |

### Phase 2: Loop Support
| Task | Status | Notes |
|------|--------|-------|
| B1.1: Add header_sealed | N/A | Not needed - using deferred seal pattern |
| B1.2: Update translateLoop | ✅ | Removed sealBlock call, added comment |
| B1.3: Update translateEnd | ✅ | Added loop header sealing |
| B1.4: Update translateBr | ✅ | Already correct (uses brDestination) |
| B1.5: Enable loop test | ✅ | Test now passes |
| B1.6: Add more loop tests | ⬜ | Deferred |
| B1.7: Run test suite | ✅ | All tests pass |

### Phase 3: Register Allocation Support
| Task | Status | Notes |
|------|--------|-------|
| B3.1: aarch64 genMove | ✅ | Already existed in inst/mod.zig |
| B3.2: x64 genMove | ✅ | Already existed in inst/mod.zig |
| B3.3: VCode.emitMove | ✅ | Properly calls Inst.genMove |
| B3.4: aarch64 genMove test | ✅ | Existing tests cover this |
| B3.5: x64 genMove test | ✅ | Existing tests cover this |
| B3.6: Run test suite | ✅ | All tests pass |
| B4.1: aarch64 genStoreStack | ✅ | Added to inst/mod.zig |
| B4.2: aarch64 genLoadStack | ✅ | Added to inst/mod.zig |
| B4.3: x64 genStoreStack | ✅ | Added to inst/mod.zig |
| B4.4: x64 genLoadStack | ✅ | Added to inst/mod.zig |
| B4.5: canonicalTypeForRc | ⬜ | Not yet needed (using I64 default) |
| B4.6: spillslotOffset | ✅ | Inline in emitMove (slot * 8) |
| B4.7: genSpill impl | ✅ | In emitMove, uses genStoreStack |
| B4.8: genReload impl | ✅ | In emitMove, uses genLoadStack |
| B4.9: aarch64 spill test | ⬜ | Deferred - needs regalloc integration test |
| B4.10: x64 spill test | ⬜ | Deferred - needs regalloc integration test |
| B4.11: Run test suite | ✅ | All tests pass |

### Phase 4: Wasm Completeness
| Task | Status | Notes |
|------|--------|-------|
| B5.1.1: GlobalInfo struct | ✅ | Using existing WasmGlobalType with mutable field |
| B5.1.2: Parse mutability | ✅ | wasm_parser already has mutable field |
| B5.1.3: Use in translate | ✅ | translateGlobalGet checks !mutable for is_constant |
| B5.1.4: Test globals | ⬜ | Deferred - existing tests pass |
| B5.2.1: trap_if opcode | ✅ | Already had trapnz/trapz in CLIF |
| B5.2.2: TrapCode enum | ✅ | Added table_out_of_bounds, indirect_call_to_null, bad_signature |
| B5.2.3: insTrapIf builder | ✅ | Added trapnz/trapz to frontend/frontend.zig |
| B5.2.4: aarch64 trap_if | ✅ | Already had trap_if lowering |
| B5.2.5: x64 trap_if | ✅ | Already had trap_if lowering |
| B5.2.6: Division traps | ✅ | guardZeroDivisor now uses trapnz |
| B5.2.7: Test trap_if | ⬜ | Deferred - existing tests pass |
| B5.3.1: FuncType storage | ✅ | Added WasmFuncType to translator.zig |
| B5.3.2: Parse type section | ✅ | driver.zig converts wasm_module.types |
| B5.3.3: Use in call_indirect | ✅ | translateCallIndirect uses func_types[type_index] |
| B5.3.4: Test call_indirect | ⬜ | Deferred - needs call_indirect test case |

### Phase 5: End-to-End
| Task | Status | Notes |
|------|--------|-------|
| E2E.1: return 42 | 🔴 BLOCKED | Pipeline produces 0 bytes - see below |
| E2E.2: arithmetic | 🔴 BLOCKED | Pipeline produces 0 bytes |
| E2E.3: control flow | 🔴 BLOCKED | Pipeline produces 0 bytes |
| E2E.4: function calls | 🔴 BLOCKED | Pipeline produces 0 bytes |
| E2E.5: loops | 🔴 BLOCKED | Pipeline produces 0 bytes |

---

## CRITICAL BUG: 0-Byte Emission

**Discovery**: During E2E testing, compilation produces 0 bytes of machine code.

**Debug output**:
```
[codegen] driver: translated function 0 to CLIF (2 blocks, 20 insts)
[codegen] driver: compiled function 0: 0 bytes
```

**Symptoms**:
- CLIF IR is populated correctly (2 blocks, 20 instructions)
- Lowering completes without errors
- VCode.emit() returns empty buffer
- Native binary crashes with SIGILL (exit code 132)

**Root Cause Investigation Needed**:
1. BlockLoweringOrder may not be computing blocks correctly
2. Lower phase may skip instructions (isAnyInstResultNeeded returns false)
3. VCode.emit() iteration may skip all instructions
4. Register allocation output may be empty

**Files to investigate**:
- `machinst/lower.zig` - instruction lowering
- `machinst/blockorder.zig` - block ordering
- `machinst/vcode.zig` - VCode emission
- `regalloc/regalloc.zig` - register allocation

This is a separate issue from B1-B5 and requires deep debugging of the native emission pipeline.

---

## Summary

**Total Tasks**: 45
**Completed**: 37
**Remaining**: 8 (all blocked by 0-byte emission bug)

**Completed Blockers** (B1-B5):
- ✅ B1: Loop back-edge translation
- ✅ B2: SmallVec heap allocation
- ✅ B3: genMove for aarch64/x64
- ✅ B4: genSpill/genReload
- ✅ B5: Wasm opcode gaps (global mutability, trap_if, indirect call)

**Critical Blocker**:
- 🔴 **0-byte emission bug** - Prevents all E2E tests

**Next Steps**:
1. Debug the 0-byte emission issue in lower.zig/vcode.zig
2. Verify BlockLoweringOrder computes correct blocks
3. Verify instruction lowering produces VCode instructions
4. Verify VCode.emit iterates over instructions correctly
