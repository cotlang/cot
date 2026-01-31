# Claude AI Instructions

## Project: Cot Programming Language

**Cot** is a Wasm-first programming language for full-stack web development.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

**This repository** contains the Cot compiler, written in Zig. Like Deno (Rust) for TypeScript, this is a permanent tool, not a temporary bootstrap.

---

## Current State (January 2026)

### Wasm Backend Progress

| Milestone | Status | Description |
|-----------|--------|-------------|
| M1-M3 | ✅ Done | Wasm SSA ops, lowering pass, code generator |
| M4-M5 | ✅ Done | E2E: return 42, add two numbers |
| M6-M7 | ✅ Done | Control flow (if/else, loops) |
| M8-M9 | ✅ Done | Function calls, CLI outputs .wasm |
| **M10** | 🔄 **Next** | Linear memory (load/store) |
| M11-M16 | TODO | Pointers, structs, arrays, strings, ARC |

### AOT Native Progress

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 1-3 | ✅ Done | ARM64/AMD64 backends ported & refactored (~20% reduction) |
| **Phase 4** | 🔄 **Next** | Wire into driver, enable native binary output |

### Test Status

**376/398 passed, 22 skipped (native tests)**

---

## Key Documents

| Document | Purpose |
|----------|---------|
| `WASM_BACKEND.md` | Wasm milestones M1-M16, implementation details |
| `AOT_EXECUTION_PLAN.md` | Native codegen phases, task checklist |
| `VISION.md` | Language vision, strategy, roadmap |
| `README.md` | Project overview and quick start |
| `../bootstrap-0.2/DESIGN.md` | Technical architecture specification |

---

## Architecture

```
Cot Compiler (Wasm path - PRIMARY):
  Cot Source → Scanner → Parser → Checker → Lowerer → IR → SSA → Wasm
                                                             ↓
                                                   compiler/ssa/passes/lower_wasm.zig
                                                             ↓
                                                   compiler/codegen/wasm_gen.zig
                                                             ↓
                                                        .wasm file

AOT Compiler (Native path - SECONDARY):
  .wasm → wasm_parser → wasm_to_ssa → SSA → regalloc → Native → ELF/Mach-O
                                              ↓
                                    compiler/codegen/native/arm64.zig
                                    compiler/codegen/native/amd64.zig
```

---

## Zig 0.15 API Changes

**ArrayList requires allocator for each operation:**
```zig
// Use ArrayListUnmanaged
var list: std.ArrayListUnmanaged(u8) = .{};
defer list.deinit(allocator);
try list.append(allocator, 42);
const w = list.writer(allocator);
```

---

## Debugging

**Use `compiler/pipeline_debug.zig`, NOT `std.debug.print`:**

```zig
const debug = @import("pipeline_debug.zig");
debug.log(.codegen, "emitting {s}", .{op_name});
```

Run with debug output:
```bash
COT_DEBUG=parse,lower,codegen zig build test
```

---

## Behavioral Guidelines

### DO

- Run tests after every change: `zig build test`
- Reference docs before implementing: check WASM_BACKEND.md or AOT_EXECUTION_PLAN.md
- Reference `bootstrap-0.2/` for working code examples
- Make incremental changes, verify each one
- Ask user for direction when uncertain

### DO NOT

- Modify bootstrap-0.2 (it's frozen)
- Skip testing
- Make large changes without verification
- Autonomously change direction

### When Stuck

1. Check the relevant planning doc (WASM_BACKEND.md or AOT_EXECUTION_PLAN.md)
2. Check if bootstrap-0.2 has working code for this
3. Reference DESIGN.md for intended architecture
4. Report the issue clearly and ask user how to proceed

---

## File Locations

| Need | Location |
|------|----------|
| Wasm backend code | `compiler/codegen/wasm*.zig` |
| Wasm lowering | `compiler/ssa/passes/lower_wasm.zig` |
| Native codegen | `compiler/codegen/native/` |
| SSA infrastructure | `compiler/ssa/` |
| Frontend | `compiler/frontend/` |
| Wasm milestones | `WASM_BACKEND.md` |
| AOT phases | `AOT_EXECUTION_PLAN.md` |
| Architecture spec | `../bootstrap-0.2/DESIGN.md` |
| Audit docs | `audit/` |

---

## Current Tasks

### Priority 1: Fix Failing Test
The wasm_gen.zig test is failing. Investigate and fix before proceeding.

### Priority 2: M10 Linear Memory
- Add memory section to Wasm module
- Implement load/store ops in wasm_gen.zig
- This unlocks pointers, structs, and most real programs

### Priority 3: AOT Phase 4
- Wire ARM64/AMD64 into driver.zig
- Un-skip 22 native tests
- Enable native binary compilation

---

## Memory Management: ARC

Cot uses Automatic Reference Counting:
- Compiler inserts retain/release automatically
- Same semantics for Wasm and native AOT targets
- See `bootstrap-0.2/DESIGN.md` section "Memory Management: ARC"

---

## Testing

```bash
# All tests
zig build test

# Specific file
zig test compiler/codegen/wasm_gen.zig

# With debug output
COT_DEBUG=codegen zig test compiler/codegen/wasm_gen.zig
```

---

## Language Design Summary

| Aspect | Choice |
|--------|--------|
| Syntax | Zig-inspired |
| Memory | ARC (Automatic Reference Counting) |
| Primary target | WebAssembly |
| Secondary target | Native via AOT |
| Niche | Full-stack apps (like Node.js, but compiled) |
| Philosophy | Simple, type-safe, runs everywhere |

---

## History

- **5 attempts** at self-hosting with native codegen all failed
- **Root cause**: Native codegen complexity (register allocation, two ISAs, ABI edge cases)
- **Solution**: Wasm as primary target (stack machine, single calling convention)
- **Frontend cleanup** completed: 54% code reduction
- **Wasm backend** M1-M9 complete: basic programs compile to .wasm
- **Native codegen** ported and refactored: ready for AOT wiring
