# Claude AI Instructions

## CRITICAL WARNING - READ THIS FIRST

### 1. Standard Approach: Copy Go Exactly

**Every language feature follows the same pattern:**

1. Find the equivalent Go code in `~/learning/go/src/cmd/`
2. Understand how Go implements it
3. Copy the pattern directly, translating Go syntax to Zig
4. Do NOT invent new approaches or "figure out" how things should work

**Example - Slice Decomposition:**
- Go file: `compile/internal/ssa/rewritedec.go`
- Go pattern: `SliceLen(SliceMake(ptr, len, cap))` → `len`
- Cot copy: `slice_len(slice_make(ptr, len))` → `copy(len)`

This is straightforward when you copy Go. It becomes complex when you try to invent.

### 2. Understand the Pipeline

**Cot compiles ALL code through Wasm first.** Native output (Mach-O, ELF) is AOT-compiled FROM Wasm.

- `cot file.cot` → produces native executable (goes through Wasm internally)
- `cot --target=wasm32 file.cot` → produces .wasm file directly

**Do NOT be surprised by Mach-O/ELF output.** This is the expected default behavior.

### 3. Key Go Reference Files

The Go compiler is at `~/learning/go/src/cmd/`. Key files:

| Feature | Go File | What to Copy |
|---------|---------|--------------|
| SSA → Wasm | `compile/internal/wasm/ssa.go` | Op handling patterns |
| Slice/String decompose | `compile/internal/ssa/rewritedec.go` | Compound type rewrites |
| Wasm assembly | `internal/obj/wasm/wasmobj.go` | Binary encoding |
| Wasm linking | `link/internal/wasm/asm.go` | Section layout |
| Generic rewrites | `compile/internal/ssa/rewritegeneric.go` | Algebraic simplification |

**DO NOT:**
- "Figure out" how something should work
- Write comments reasoning about logic
- Invent your own approach

**DO:**
- Find the equivalent Go code
- Copy it directly
- If you don't understand Go's code, read more of it until you do

---

## Project: Cot Programming Language

**Cot** is a Wasm-first programming language for full-stack web development.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

**This repository** contains the Cot compiler, written in Zig. Like Deno (Rust) for TypeScript, this is a permanent tool, not a temporary bootstrap.

---

## Current State (February 2026)

### Wasm Backend Progress

| Milestone | Status | Description |
|-----------|--------|-------------|
| M1-M3 | ✅ Done | Wasm SSA ops, lowering pass, code generator |
| M4-M5 | ✅ Done | E2E: return 42, add two numbers |
| M6-M7 | ✅ Done | Control flow (if/else, loops, break, continue) |
| M8-M9 | ✅ Done | Function calls (params, recursion), CLI outputs .wasm |
| M10 | ✅ Done | Linear memory (load/store, SP global, frame allocation) |
| M11 | ✅ Done | Pointers (off_ptr, add_ptr, sub_ptr) |
| M12 | ✅ Done | Structs (field read/write via off_ptr) |
| M13 | ✅ Done | Arrays/Slices (decomposition in lower_wasm, frame size fix) |
| M14 | ✅ Done | Strings (rewritegeneric + rewritedec passes, Go-matching structure) |
| **M15** | 🔄 **Next** | ARC (SSA ops exist, gen.zig needs implementation) |
| M16 | ⏳ TODO | Browser imports (JS interop) |

### Verified Test Coverage (58/58 passing)

| Category | Tests | Status |
|----------|-------|--------|
| Arithmetic | 10 | ✅ All pass |
| Control Flow | 14 | ✅ All pass |
| Functions | 16 | ✅ All pass |
| Memory | 5 | ✅ All pass |
| Structs | 5 | ✅ All pass |
| Arrays | 5 | ✅ All pass |
| Strings | 3 | ✅ All pass |

### Known Gaps

- **Struct-by-value params**: Not yet implemented (workaround: use field access directly)
- **ARC**: SSA ops defined but gen.zig doesn't emit wasm

### AOT Native Progress

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 1-3 | ✅ Done | ARM64/AMD64 backends ported & refactored |
| **Phase 4** | 🔄 Pending | Wire into driver, enable native binary output |

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

## Architecture - IMPORTANT: READ THIS

**Cot is a Wasm-first compiler. ALL code paths go through Wasm.**

```
                           Cot Source
                               ↓
                    Scanner → Parser → Checker → IR → SSA
                               ↓
                    lower_wasm.zig (SSA → Wasm SSA ops)
                               ↓
                    wasm/ package (Wasm bytecode)
                               ↓
              ┌────────────────┴────────────────┐
              ↓                                 ↓
        --target=wasm32                   --target=native (default)
              ↓                                 ↓
         .wasm file                    wasm_parser → wasm_to_ssa
                                               ↓
                                      regalloc → arm64/amd64
                                               ↓
                                        .o file (Mach-O/ELF)
                                               ↓
                                         linker (clang)
                                               ↓
                                        executable
```

**Key points for Claude:**
1. **Native output is Mach-O/ELF** - This is EXPECTED, not an error
2. **Native goes through Wasm** - The pipeline is: Cot → Wasm → Native (AOT)
3. **To get .wasm output**: Use `--target=wasm32`
4. **Default is native**: Running `cot file.cot` produces a native executable

**Testing Wasm output:**
```bash
# Compile to wasm
./zig-out/bin/cot --target=wasm32 test.cot -o test.wasm

# Run with Node.js
node -e 'const fs=require("fs"); const wasm=fs.readFileSync("test.wasm");
WebAssembly.instantiate(wasm).then(r=>console.log(r.instance.exports.main()));'
```

**Testing native output:**
```bash
# Compile to native (default)
./zig-out/bin/cot test.cot -o test
./test
echo $?  # Shows return value
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
3. **Study ~/learning/ reference implementations** (Go, Swift) to copy proven designs
4. Reference DESIGN.md for intended architecture
5. Report the issue clearly and ask user how to proceed

### Persistence Requirements

**NEVER give up on difficult code.** This project requires professional-grade implementation:

- **DO NOT** comment out failing tests - fix them
- **DO NOT** mark features as "needs work" and move on - complete them
- **DO NOT** stash code or restore from git to avoid problems - solve them
- **DO NOT** leave "TODO" comments for issues you encountered - resolve them now
- **DO NOT** create "known issues" sections - there should be no known issues

When code is difficult:
1. Study the reference implementation in `~/learning/go/` or `~/learning/swift/`
2. Understand WHY the reference design works
3. Apply the same pattern to Cot
4. Keep iterating until ALL tests pass
5. Never move on until the feature is complete

The project succeeds through persistence and copying proven designs, not shortcuts.

---

## File Locations

| Need | Location | Go Equivalent |
|------|----------|---------------|
| **Wasm codegen (core)** | `compiler/codegen/wasm/` | `wasm/ssa.go` |
| ConstString rewrite | `compiler/ssa/passes/rewritegeneric.zig` | `rewritegeneric.go` |
| Slice/String decompose | `compiler/ssa/passes/rewritedec.zig` | `rewritedec.go` |
| Op lowering (generic→wasm) | `compiler/ssa/passes/lower_wasm.zig` | `lower.go` |
| Native AOT codegen | `compiler/codegen/native/` | - |
| Wasm→Native converter | `compiler/codegen/native/wasm_to_ssa.zig` | - |
| SSA infrastructure | `compiler/ssa/` | `ssa/*.go` |
| Frontend | `compiler/frontend/` | - |
| Driver (orchestrates all) | `compiler/driver.zig` | `compile.go` |
| CLI entry point | `compiler/main.zig` | - |

| Documentation | Purpose |
|---------------|---------|
| `WASM_BACKEND.md` | Wasm implementation status and Go references |
| `AOT_EXECUTION_PLAN.md` | Native AOT compilation phases |
| `audit/SUMMARY.md` | Test results and component status |

---

## Current Tasks

### Option A: M16 Browser Imports
- Import section for JS interop
- console.log, DOM access
- Go reference: `cmd/link/internal/wasm/asm.go` writeImportSec

### Option B: AOT Phase 4
- Wire ARM64/AMD64 into driver.zig
- Un-skip 22 native tests
- Enable `cot build --native` for native binary output

### Option C: E2E Testing
- Create full end-to-end test programs in Cot
- Test strings, structs, arrays working together
- Verify wasm output runs correctly in wasmtime

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
