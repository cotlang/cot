# Debug Build Mode — `cot build --debug`

**Updated:** 2026-03-19
**Goal:** Production-quality debugging experience — step through Cot code in lldb/gdb, inspect variables, set breakpoints by file:line. Prerequisite for native self-hosting.

---

## Why This Matters

Without full DWARF debug info, debugging Cot programs means reading disassembly and register dumps. Every reference language (Zig, Go, Rust, Swift) ships debug builds that work with standard debuggers out of the box. This is table stakes for production use and essential before embarking on native self-hosting in Cot.

---

## Current State

### What Works
| Feature | Status | Location |
|---------|--------|----------|
| `.debug_line` (PC → file:line) | **Done** | dwarf.zig:194-264 |
| `.debug_abbrev` (1 abbreviation) | **Done** (stub) | dwarf.zig:136-159 |
| `.debug_info` (compile unit only) | **Done** (stub) | dwarf.zig:161-192 |
| Signal handler with backtrace | **Done** | signal_native.zig |
| Source location in crash output | **Done** | signal_native.zig (dladdr + srcmap) |
| DWARF .debug_line relocations | **Done** | macho.zig:509-532 |

### What's Missing for Interactive Debugging
| Feature | Enables | Status |
|---------|---------|--------|
| `.debug_info` subprograms | `b file.cot:42`, function names in `bt` | **Missing** |
| `.debug_info` variables | `p myvar`, `frame variable` | **Missing** |
| `.debug_info` types | Struct field inspection, type display | **Missing** |
| `.debug_str` string pool | Deduped names for all DIEs | **Missing** |
| `.debug_ranges` | Function address ranges | **Missing** |
| `.debug_frame` / `.eh_frame` | Stack unwinding for `bt` | **Missing** |
| DWARF location expressions | Variable → register/stack mapping | **Missing** |
| IR data threading to codegen | Variable names/types at emit time | **Partial** |
| ELF DWARF sections | Linux debugging support | **Missing** |
| `--debug` CLI flag | Gating mechanism | **Missing** |

### What's Already Built (Safety Checks)
These exist and work independently of the `--debug` flag:

| Check | Location |
|-------|----------|
| Integer overflow traps | x64/aarch64 inst/mod.zig |
| Division by zero traps | x64/aarch64 lower.zig |
| Bounds checks (array/slice) | lower.zig |
| Null pointer checks | lower.zig |
| Optional unwrap checks | lower.zig |
| ARC redzone guards (0xFA/0xFB) | arc_native.zig |
| ARC poison on free (0xDEADDEAD) | arc_native.zig |
| ARC double-free detection | arc_native.zig |
| ARC magic sentinel (0xC07A8C00) | arc_native.zig |
| Unreachable code traps | x64/aarch64 inst/mod.zig |
| Stack overflow detection | x64/aarch64 inst/mod.zig |

---

## Implementation Plan

### Phase 1: Subprograms — Function-Level Debugging (~6 hours)

**Goal:** `lldb ./binary` → `b file.cot:42` → `run` → stops at breakpoint → `bt` shows function names

**1a. Extend `.debug_abbrev` with subprogram abbreviation**

Add abbreviation code 2 for `DW_TAG_subprogram`:
- `DW_AT_name` (DW_FORM_strp) — function name
- `DW_AT_low_pc` (DW_FORM_addr) — function start address
- `DW_AT_high_pc` (DW_FORM_data8) — function size
- `DW_AT_decl_file` (DW_FORM_data1) — source file index
- `DW_AT_decl_line` (DW_FORM_data4) — source line number
- `DW_AT_external` (DW_FORM_flag_present) — visibility

**Reference:** Go `cmd/internal/dwarf/dwarf.go` lines 447-500 (subprogram abbreviation).

**File:** `compiler/codegen/native/dwarf.zig` — extend `generateDebugAbbrev` and `generateDebugInfo`.

**1b. Add `.debug_str` section**

String pool for all function names, file paths, type names. Change `DW_FORM_string` to `DW_FORM_strp` (4-byte offset into `.debug_str`).

**File:** `compiler/codegen/native/dwarf.zig` — new `generateDebugStr` function.

**1c. Add `.debug_ranges` section**

Map each function to its `[low_pc, high_pc)` range. Required for DWARF4 when functions are non-contiguous.

**Reference:** Go `cmd/internal/dwarf/dwarf.go` lines 1408-1414.

**1d. Thread function metadata to codegen**

Currently `driver.zig` has function names and code offsets but doesn't pass them to the DWARF builder. Add:
```zig
pub const DebugFuncInfo = struct {
    name: []const u8,
    code_offset: u32,
    code_size: u32,
    decl_line: u32,
};
```

Pass `[]DebugFuncInfo` to `DwarfBuilder` alongside line entries.

**Files:** `compiler/driver.zig` (collect), `compiler/codegen/native/object_module.zig` (thread), `compiler/codegen/native/dwarf.zig` (emit).

**Result:** `lldb` shows function names in backtraces, breakpoints work by file:line, stepping works.

### Phase 2: Variables — Local Variable Inspection (~8 hours)

**Goal:** `frame variable` and `p myvar` show local variable values in lldb

**2a. Thread IR function metadata to codegen**

The IR (`ir.zig`) stores locals with name, type, and offset. Currently this is discarded after SSA building. Thread it to codegen:

```zig
pub const DebugLocalInfo = struct {
    name: []const u8,
    type_name: []const u8,  // "i64", "*Scope", "?*Scope"
    frame_offset: i32,      // offset from frame pointer
    is_param: bool,
};
```

Add `debug_locals: []DebugLocalInfo` to `DebugFuncInfo`.

**Files:** `compiler/driver.zig` (collect from IR), `compiler/codegen/native/ssa_to_clif.zig` (compute frame offsets from regalloc), `compiler/codegen/native/dwarf.zig` (emit DIEs).

**2b. Add variable DIEs to `.debug_info`**

For each function, emit child DIEs:
- `DW_TAG_formal_parameter` for function parameters
- `DW_TAG_variable` for local variables

Attributes:
- `DW_AT_name` (DW_FORM_strp) — variable name
- `DW_AT_type` (DW_FORM_ref4) — reference to type DIE
- `DW_AT_location` (DW_FORM_exprloc) — DWARF location expression

**Reference:** Go `cmd/compile/internal/dwarfgen/dwarfgen.go` lines 200-300.

**2c. DWARF location expressions**

For stack variables: `DW_OP_fbreg <offset>` (frame base + offset).
For register variables: `DW_OP_reg<N>`.

Most Cot locals are on the stack (post-regalloc spills), so `DW_OP_fbreg` covers the common case.

**Reference:** Go `cmd/internal/dwarf/dwarf.go` lines 1108-1160 (location expression encoding).

**Result:** `p myvar` shows the value; `frame variable` lists all locals with types.

### Phase 3: Types — Struct/Enum Inspection (~8 hours)

**Goal:** `p myStruct` shows field names and values; `ptype MyStruct` shows the type definition

**3a. Base type DIEs**

Emit `DW_TAG_base_type` for: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `bool`, `void`.

Attributes: `DW_AT_name`, `DW_AT_byte_size`, `DW_AT_encoding` (signed/unsigned/float/boolean).

**3b. Pointer type DIEs**

`DW_TAG_pointer_type` with `DW_AT_type` referencing the pointee type DIE.

**3c. Struct type DIEs**

`DW_TAG_structure_type` with `DW_AT_name`, `DW_AT_byte_size`, children:
- `DW_TAG_member` for each field: `DW_AT_name`, `DW_AT_type`, `DW_AT_data_member_location`

**3d. Enum type DIEs**

`DW_TAG_enumeration_type` with `DW_AT_name`, `DW_AT_byte_size`, children:
- `DW_TAG_enumerator` for each variant: `DW_AT_name`, `DW_AT_const_value`

**3e. Optional type DIEs**

`DW_TAG_union_type` with tag discriminant + payload (mirrors Swift's optional DWARF layout).

**3f. Thread type registry to codegen**

The checker's type registry has all struct/enum definitions. Serialize the needed subset to a `DebugTypeInfo` array and pass to codegen.

**Reference:** Go `cmd/internal/dwarf/dwarf.go` lines 580-750 (type DIE emission).

**Result:** Full type inspection in debugger. `p myScope.parent` traverses struct fields.

### Phase 4: Stack Unwinding — `.debug_frame` (~4 hours)

**Goal:** `bt` works reliably even for deeply nested calls and signal handler frames

Emit CIE (Common Information Entry) + FDE (Frame Description Entry) per function. The CIE describes the default unwinding rules (ABI), the FDE describes per-function adjustments.

For ARM64: frame pointer chain (fp → prev_fp → ...) makes this straightforward — the CIE just says "fp is at [fp], return address is at [fp+8]".

**Reference:** Go `cmd/internal/dwarf/dwarf.go` lines 1415-1585.

**Note:** Backtrace already works via `backtrace()` libc call in the signal handler. `.debug_frame` improves reliability and enables proper unwinding in lldb.

### Phase 5: ELF + Linux Support (~4 hours)

Port the Mach-O DWARF section emission to ELF format. The DWARF content is identical — only the container format changes (section headers, relocations).

**File:** `compiler/codegen/native/elf.zig`

### Phase 6: CLI Flag + Gating (~1 hour)

Add `--debug` flag to `cot build`. In debug mode:
- Emit full DWARF (phases 1-5 above)
- Enable ARC diagnostic messages (print + abort on bad pointer/double-free/UAF)
- Enable fill-on-alloc (0xAA) and fill-on-free (0xDD)

In release mode (default or `--release`):
- Minimal DWARF (`.debug_line` only, for crash backtraces)
- ARC checks silently skip (no diagnostics)
- No fill patterns

---

## Data Pipeline

```
Source (.cot)
  → Scanner (spans: file, line, col)
  → Parser (AST nodes with spans)
  → Checker (type registry: struct/enum/func types with field info)
  → Lowerer (IR: functions with locals, params, spans)
  → SSA Builder (SSA values with Pos from IR spans)
  → Register Allocator (stack frame layout: local → frame offset)
  → Code Emitter (MachSrcLoc: instruction → source offset)
  → Driver (collects: func names, code offsets, line entries)
  → DWARF Builder (emits: .debug_info, .debug_line, .debug_str, ...)
  → Mach-O/ELF Writer (links debug sections into binary)
```

**Data that must be threaded through (currently lost):**
- IR `Func.locals[]` (name, type, offset) → codegen
- Checker type registry (struct/enum defs) → codegen
- Regalloc frame layout (final stack offsets) → DWARF location expressions

---

## Reference Implementations

| Reference | What to Copy | Lines |
|-----------|-------------|-------|
| Go `cmd/internal/dwarf/dwarf.go` | Full DWARF emission (types, vars, subprograms) | ~1,700 |
| Go `cmd/compile/internal/dwarfgen/dwarfgen.go` | Variable/scope collection from IR | ~672 |
| Go `cmd/link/internal/ld/dwarf.go` | Linker DWARF section writing | ~2,200 |
| Cranelift `ir/sourceloc.rs` | Source location threading through passes | ~100 |
| Cranelift `machinst/vcode.rs` | MachSrcLoc tracking (Cot already copies this) | ~50 |

## Cot Implementation Files

| File | Current | Needs |
|------|---------|-------|
| `compiler/codegen/native/dwarf.zig` (364 lines) | .debug_line + CU stub | Full DIE tree (subprograms, vars, types) |
| `compiler/codegen/native/macho.zig` | DWARF section linking | Additional sections (.debug_str, .debug_ranges) |
| `compiler/codegen/native/elf.zig` | No DWARF | Port from macho.zig |
| `compiler/driver.zig` | Func names + line entries | DebugFuncInfo + DebugLocalInfo collection |
| `compiler/codegen/native/ssa_to_clif.zig` | Source offsets | Frame offset tracking for locals |

---

## Effort Summary

| Phase | Feature | Effort | Enables |
|-------|---------|--------|---------|
| 1 | Subprograms + .debug_str + .debug_ranges | 6 hours | Breakpoints, stepping, function names in bt |
| 2 | Variables + location expressions | 8 hours | `p myvar`, `frame variable` |
| 3 | Types (struct/enum/pointer/optional) | 8 hours | `p myStruct.field`, `ptype MyType` |
| 4 | .debug_frame (stack unwinding) | 4 hours | Reliable bt in all contexts |
| 5 | ELF/Linux DWARF | 4 hours | Linux debugging |
| 6 | --debug CLI flag + gating | 1 hour | Debug vs release builds |
| **Total** | | **~31 hours** | Full lldb/gdb experience |

Phases 1-2 (~14 hours) give 80% of the debugging value. Phase 3 adds deep inspection. Phases 4-6 are polish.
