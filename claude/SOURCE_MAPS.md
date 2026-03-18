# Source Maps: Instruction Address → Source Line Mapping

**Date:** 2026-03-18
**Status:** Implementation plan
**Goal:** Crash output shows `file.cot:42` instead of just `0x100004e8`

---

## Current Pipeline (Position Info Flow)

```
Scanner → Span(byte offsets)     ✅ preserved
Parser  → Span in AST nodes      ✅ preserved
Lowerer → Span in IR nodes       ✅ preserved (every emit* takes span)
SSA     → Pos(line,col,file)     ❌ defaulted to zeros
CLIF    → RelSourceLoc           ❌ never set
MachCode → srclocs array         ❌ empty (API exists, never called)
DWARF   → .debug_line section    ❌ dwarf.zig exists, not integrated
```

Every piece of infrastructure exists. The 4 gaps are just wiring.

---

## Implementation: 4 Gaps to Bridge

### Gap 1: SSA Builder — Populate Value.pos from IR Node.span

**File:** `compiler/frontend/ssa_builder.zig`

The SSA builder creates `Value` objects via `self.func.newValue()`. Each IR `Node` has a `span: Span` with byte offsets. The SSA `Value` has a `pos: Pos` field with `line: u32, col: u32, file: u16`.

**Change:** When converting IR nodes to SSA values, copy the span's start position:

```zig
// In SSA builder, after creating value:
const ir_node = self.getNode(node_idx);
value.pos = .{
    .line = ir_node.span.start.offset,  // byte offset (converted to line later)
    .col = 0,
    .file = self.file_idx,
};
```

Actually simpler: just store the byte offset in `pos.line` for now. The signal handler or DWARF builder can convert byte offset → line number using the Source's line_offsets table.

### Gap 2: SSA→CLIF Translator — Pass pos to RelSourceLoc

**File:** `compiler/codegen/native/ssa_to_clif.zig`

When translating SSA values to CLIF instructions, set the source location:

```zig
// Before emitting each CLIF instruction:
if (v.pos.line != 0) {
    self.mach_buffer.startSrcloc(.{ .offset = v.pos.line });
}
// ... emit instruction ...
if (v.pos.line != 0) {
    self.mach_buffer.endSrcloc();
}
```

Actually the source locations are tracked at the CLIF→MachInst level, not at SSA→CLIF. The CLIF function itself carries source positions through to machine code emission.

**Better approach:** Set `cur_srcloc` on the CLIF builder/emitter. The `ssa_to_clif.zig` translator has access to each SSA Value's pos. Pass it through to the CLIF instruction emission.

### Gap 3: MachInst Lower — Call startSrcloc/endSrcloc

**File:** `compiler/codegen/native/machinst/lower.zig`

The `lower.zig` file has a `finishIrInst` function called after each CLIF instruction is lowered to machine instructions. This is where `startSrcloc`/`endSrcloc` should be called.

The `MachBuffer` already has the API:
```zig
pub fn startSrcloc(self: *Self, loc: RelSourceLoc) void
pub fn endSrcloc(self: *Self) !void
```

And stores results in `self.srclocs: ArrayListUnmanaged(MachSrcLoc(RelSourceLoc))`.

### Gap 4: Emit Source Map Section

**File:** `compiler/codegen/native/compile.zig` or `compiler/codegen/native/elf.zig`

After machine code emission, `MachBuffer.srclocs` contains the mapping:
```
[(code_start, code_end, source_offset), ...]
```

Two options:
a) **DWARF .debug_line** — `dwarf.zig` already generates this. Feed srclocs → LineEntry → DWARF.
b) **Custom .cot_srcmap section** — simpler, read by signal handler at runtime.

For the signal handler to read source maps at runtime (without external tools), option (b) is better. DWARF can come later for lldb support.

**Custom format:**
```
Section: __DATA,__cot_srcmap
Header: magic(4) + entry_count(4) + source_file_count(2)
Source files: [offset_to_filename(4), filename_len(2)] × file_count
Entries (sorted by code_offset): [code_offset(4), source_offset(4), file_idx(2)] × entry_count
```

The signal handler binary-searches entries by PC to find the source location, then reads the source file table to print `filename:line`.

---

## Signal Handler Integration

Add to `__cot_signal_handler`:
1. Read `__cot_srcmap` section address (linker symbol)
2. Binary search for the faulting PC
3. Convert source byte offset → line number (precompute line offsets or embed them)
4. Print `filename:line` before the register dump

For V1, embed line numbers directly (not byte offsets) to avoid needing the source file at runtime:
```
Entries: [code_offset(4), line_number(4), file_idx(2)] × entry_count
```

---

## Effort Estimate

| Gap | Description | Effort |
|-----|-------------|--------|
| 1 | SSA builder: copy span→pos | Small |
| 2 | SSA→CLIF: pass source location | Small |
| 3 | MachInst: call startSrcloc/endSrcloc | Small |
| 4 | Emit __cot_srcmap section | Medium |
| 5 | Signal handler: read + binary search | Medium |
| **Total** | | ~3-4 hours |
