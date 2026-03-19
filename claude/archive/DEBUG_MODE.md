# Debug Mode & Crash Stack Traces — COMPLETE

**Date:** 2026-03-20
**Status:** All features implemented and shipped

---

## Summary

Cot has a two-tier crash trace system ported from Go and Zig:

| Mode | Flag | Data Source | Output | Reference |
|------|------|------------|--------|-----------|
| Release | (default) | Go-style pctab | `at file.cot:42` | Go `runtime/symtab.go` |
| Debug | `--debug` / `-g` | pctab + DWARF `.debug_line` | `at file.cot:42:13` | Zig `std/debug/Dwarf.zig` |
| Debugger | `lldb` | DWARF sections | Full: breakpoints, variables, types | Standard DWARF4 |

---

## Architecture

### Release: Go-style PC Tables (always embedded)

**Encoder:** `compiler/driver.zig` — Go `cmd/internal/obj/pcln.go:funcpctab()`
- Per-function varint-encoded (zigzag_value_delta, pc_delta) streams
- Functab: `{name_hash:u32, pctab_off:u32, file_idx:u16, pad:u16, pad2:u32}` per function
- Multi-file filetab with null-separated file paths
- FNV-1a hash for runtime function matching

**Decoder:** `compiler/codegen/native/signal_native.zig` — Go `runtime/symtab.go:step()/pcvalue()/readvarint()`
- dladdr for function name + base address (signal-safe)
- FNV-1a hash loop to match functab entries
- Two readvarint loops (value delta + PC delta) as CLIF IR blocks
- Zigzag decode: `-(raw & 1) ^ (raw >> 1)` (Go `symtab.go:1295`)

**Data sections:**
- `_cot_pctab` — varint streams
- `_cot_functab` — function entries (16 bytes each)
- `_cot_functab_count` — u32
- `_cot_filetab` — null-separated file paths
- `_cot_funcnames` — null-separated function names

### Debug: DWARF Runtime Reader (linked with `--debug`)

**Module:** `compiler/codegen/native/dwarf_reader.zig` — Zig `lib/std/debug/Dwarf.zig`
- Pre-compiled at `zig build` time for arm64-macos and x86_64-linux
- Embedded in compiler binary via `@embedFile`
- Written to temp .o and linked when `--debug` is set
- Exports `__cot_dwarf_resolve(pc)` — reads `.debug_line` from own binary

**Algorithm (ported from Zig):**
1. First call: mmap own binary (macOS: `_NSGetExecutablePath`, Linux: `/proc/self/exe`)
2. Parse Mach-O/ELF headers to find `.debug_line` section
3. Execute DWARF line number program state machine:
   - All standard opcodes (copy, advance_pc, advance_line, set_file, set_column, etc.)
   - Extended opcodes (end_sequence, set_address, define_file)
   - Special opcodes (combined address + line advance)
4. Binary search for closest address <= target PC
5. Print `"  at file:line:col\n"` to stderr

### DWARF Writer (for lldb)

**Module:** `compiler/codegen/native/dwarf.zig` — DWARF4 format
- `.debug_abbrev` — 8 DIE types
- `.debug_info` — compile units, subprograms, parameters, variables, types
- `.debug_line` — line number program with `DW_LNS_set_column` opcodes
- `.debug_frame` — CIE + per-function FDEs (ARM64 register tracking)

### Signal Handler

**Module:** `compiler/codegen/native/signal_native.zig`
- `__cot_signal_handler` — SA_SIGINFO handler for SIGSEGV/SIGABRT/SIGILL/SIGFPE/SIGBUS
- `__cot_print_source_loc(pc)` — pctab lookup (always called)
- `__cot_dwarf_resolve(pc)` — DWARF lookup (debug mode only)
- `__cot_print_backtrace` — per-frame loop with dladdr + source resolution
- `__cot_print_hex` — stack-based hex printer (signal-safe)
- `__cot_install_signals` — sigaction with SA_SIGINFO | SA_ONSTACK

### Per-Frame Backtrace

Each backtrace frame shows:
```
  symbol_name + 0xOFFSET  at file.cot:42
```

Uses `dladdr` per frame (signal-safe), then calls `__cot_print_source_loc` and optionally `__cot_dwarf_resolve`.

### Multi-File Support

- `func_file_indices` tracks which parsed file each IR function came from
- Encoder uses per-file source text for correct line number computation
- Filetab contains all file paths; functab entries carry correct `file_idx`
- Signal handler walks filetab to find filename for each function's `file_idx`

---

## Example Output

### Release build (`cot build file.cot`)
```
SIGSEGV: segmentation fault
PC=0x0000000104930694 addr=0x0000000000000000
  at /tmp/file.cot:4
  _sigtramp + 0x38
  _cot_main + 0x5c  at /tmp/file.cot:8
  main + 0x30  at /tmp/file.cot:8
  start + 0x1c10
```

### Debug build (`cot build file.cot --debug`)
```
SIGSEGV: segmentation fault
PC=0x0000000104930694 addr=0x0000000000000000
  at /tmp/file.cot:4
  at file.cot:7:13
  _sigtramp + 0x38
  _cot_main + 0x5c  at /tmp/file.cot:8
  main + 0x30  at /tmp/file.cot:8
  start + 0x1c10
```

---

## Files

| File | Purpose |
|------|---------|
| `compiler/codegen/native/signal_native.zig` | Signal handler + pctab decoder (CLIF IR) |
| `compiler/codegen/native/dwarf_reader.zig` | DWARF `.debug_line` runtime reader (Zig module) |
| `compiler/codegen/native/dwarf.zig` | DWARF writer (emits `.debug_*` sections) |
| `compiler/driver.zig` | Pctab encoder + multi-file tracking |
| `compiler/main.zig` | `--debug` flag wiring + DWARF .o linking |
| `build.zig` | Pre-compiles dwarf_reader.zig for arm64/x64 |

## Not Implemented (by design)

**DWARF frame unwinding (`.eh_frame`):** Go doesn't use it — uses OS-provided `backtrace()`. Zig does (`SelfUnwinder.zig`), but only needed for optimized code with omitted frame pointers. Cot's debug builds preserve frame pointers, so `backtrace()` is correct. Future enhancement if release builds omit frame pointers.

---

## Commits

```
3cc5061 Add column numbers to DWARF debug crash traces
da162a6 Add per-frame source resolution + multi-file pctab
c132d4a Add DWARF runtime reader for debug crash traces (Zig std.debug pattern)
4270ba4 Wire --debug/-g flag through to Driver.debug_mode
11fd09c Add Go-style PC→line tables for crash stack traces
```
