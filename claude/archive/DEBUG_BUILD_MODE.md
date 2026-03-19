# Debug Build Mode — `cot build --debug`

**Updated:** 2026-03-19
**Goal:** Production-quality debugging experience — step through Cot code in lldb/gdb, inspect variables, set breakpoints by file:line. Prerequisite for native self-hosting.

---

## Current State

### DWARF Debug Info — Working
| Feature | Status | Verified |
|---------|--------|----------|
| `.debug_line` (PC → file:line) | **Done** | lldb shows source at breakpoints |
| `.debug_abbrev` (8 abbreviation codes) | **Done** | dwarfdump validates |
| `.debug_info` compile unit | **Done** | DW_AT_language, DW_AT_producer |
| `.debug_info` subprogram DIEs | **Done** | `b file.cot:42` works, function names in `bt` |
| `.debug_info` variable + parameter DIEs | **Done** | `frame variable` shows names and types |
| `.debug_info` base type DIEs | **Done** | i8–i64, u8–u64, f32, f64, bool, void, ptr |
| `.debug_info` struct type + member DIEs | **Done** | `type lookup Point` → `struct Point { long x; long y; }` |
| Relocations for all debug sections | **Done** | Addresses resolve correctly |

### lldb Session Example (verified)
```
(lldb) b test_struct.cot:3
Breakpoint 1: where = test_struct`main + 12 at test_struct.cot:3
(lldb) run
-> 3   fn main() { var p = makePoint(10, 20); println(p.x + p.y) }
(lldb) frame variable
(Point) p = { x = ..., y = ... }
(lldb) type lookup Point
struct Point { long x; long y; }
(lldb) bt
  frame #0: test_struct`test_struct.add at test_struct.cot:1
  frame #1: test_struct`main at test_struct.cot:3
```

### Safety Checks — Always Active
| Check | Location |
|-------|----------|
| Integer overflow traps | x64/aarch64 inst/mod.zig |
| Division by zero traps | x64/aarch64 lower.zig |
| Bounds checks (array/slice) | lower.zig |
| Null pointer checks | lower.zig |
| ARC redzone guards (0xFA/0xFB) | arc_native.zig |
| ARC poison on free (0xDEADDEAD) | arc_native.zig |
| ARC double-free detection | arc_native.zig |
| Signal handler with register dump + backtrace | signal_native.zig |
| Source location in crash output | signal_native.zig |

---

## Remaining Work

### Phase 4: `.debug_frame` — Stack Unwinding (~4 hours)

Emit CIE (Common Information Entry) + FDE (Frame Description Entry) per function for reliable stack unwinding. Without this, `bt` works via frame pointer chain but may fail for leaf functions or optimized code.

**Reference:** Go `cmd/internal/dwarf/dwarf.go` lines 1442-1590.

### Phase 5: ELF/Linux DWARF (~4 hours)

Port Mach-O DWARF section emission to ELF format. Same DWARF content, different container (section headers, relocations).

### Phase 6: `--debug` CLI Flag + Gating (~1 hour)

Add `--debug` flag to `cot build`. Debug mode:
- Full DWARF (all phases above)
- ARC diagnostic messages print + abort
- Fill-on-alloc (0xAA) / fill-on-free (0xDD)

Release mode (default or `--release`):
- Minimal DWARF (`.debug_line` only for crash backtraces)
- ARC checks silently skip
- No fill patterns

### Known Limitation: Variable Values

Variable/parameter DIEs use IR frame offsets (pre-regalloc). The DWARF structure is correct (lldb shows names, types, struct fields), but displayed values may be wrong because regalloc can move variables to different stack slots. Fix requires threading regalloc's final frame layout back to DWARF location expressions.

### Not Planned (Post-1.0)

| Feature | Reference |
|---------|-----------|
| Race detection | Go `-race` |
| Full shadow memory ASan | ASAN_IMPLEMENTATION.md |
| Location lists (optimized code) | Go's createComplexVars |
| Inline subroutine DIEs | Go's DW_ABRV_INLINED_SUBROUTINE |

---

## Implementation Files

| File | Role |
|------|------|
| `compiler/codegen/native/dwarf.zig` | DWARF section generation (abbrev, info, line) |
| `compiler/codegen/native/macho.zig` | Mach-O writer with debug section linking |
| `compiler/codegen/native/object_module.zig` | Debug data threading |
| `compiler/driver.zig` | Collects DebugFuncInfo + DebugLocalInfo from IR |
| `compiler/frontend/types.zig` | TypeRegistry for struct type DIEs |

## Reference Implementations

| Reference | What Was Copied |
|-----------|----------------|
| Go `cmd/internal/dwarf/dwarf.go` | Abbreviation table, subprogram/variable/struct DIE structure |
| Go `cmd/compile/internal/dwarfgen/dwarfgen.go` | Variable collection (createSimpleVar pattern) |
| Go `cmd/link/internal/ld/dwarf.go` | Struct field emission (defgotype) |
