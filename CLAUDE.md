# Claude AI Instructions

## 🚨 BEFORE DEBUGGING ANYTHING, READ: `TROUBLESHOOTING.md`

When you encounter ANY bug or error, **STOP** and follow `TROUBLESHOOTING.md`.

**The #1 cause of project failure**: Claude tries to "figure out" fixes instead of copying the reference implementation. This document exists because this pattern has caused 5 rewrites.

**The fix that works every time**:
1. Find the reference implementation (Go or Cranelift)
2. Do line-by-line comparison
3. Copy exactly - don't invent, don't simplify, don't "improve"

See `TROUBLESHOOTING.md` for the full methodology, reference map, and checklist.

### ⚠️ Confused about br_table / dispatch loops?

**READ: `docs/BR_TABLE_ARCHITECTURE.md`** - br_table is INTENTIONAL, copied from Go's dispatch loop pattern. Do NOT try to remove or "fix" it without reading this document first.

---

## CRITICAL WARNING - READ THIS FIRST

### 1. Standard Approach: Copy Go Exactly

**Every language feature follows the same pattern:**

1. Find the equivalent Go code in `~/learning/go/src/cmd/`
2. Understand how Go implements it
3. Copy the pattern directly, translating Go syntax to Zig
4. Do NOT invent new approaches or "figure out" how things should work

**Case Study - M14 Strings (February 2026):**

Claude initially tried to "figure out" string handling by adding hacky special-case code in gen.zig. This created 100+ lines of messy code that didn't work properly.

When Claude stopped and copied Go's approach instead:
- Audited Go's `rewritegeneric.go` and `rewritedec.go`
- Created matching files: `rewritegeneric.zig` and `rewritedec.zig`
- Copied Go's pass structure and transformation patterns exactly
- Result: Clean code, all tests passing, completed in one session

**The lesson:** Copying Go took less effort AND produced better code. Inventing takes MORE effort and produces worse code. There is no upside to inventing.

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

**832 tests pass (0 failures, 0 skipped)** across Wasm and native targets.
**114 test case files, 42 Wasm E2E tests, 24 native E2E tests.**

### What's Done

- **Wasm backend (M1-M16):** Complete. Arithmetic, control flow, functions, memory, structs, arrays, strings, ARC, browser imports.
- **Native AOT (Phase 0-7):** Complete. Cranelift-style CLIF IR, regalloc2, ARM64/x64 backends, Mach-O/ELF output.
- **ARC runtime (M17-M19):** Complete. Retain/release, heap allocation, destructors.
- **Language features (M20-M23):** Complete. String ops, array append, for-range loops.
- **Phase 3 Wave 1-4:** Methods, enums, unions, switch, type aliases, imports, extern, bitwise, compound assign, optionals, chars, builtins - all verified on both Wasm and native.
- **Phase 3 Wave 5:** Floats (f32/f64), union payloads, error unions (`!T`, `try`, `catch`), function pointers, closures, defer, ARC coverage - all verified on both Wasm and native.
- **Sized integers:** Full pipeline (i8-u64, f32-f64), type system, @intCast.
- **Global variables:** Complete on Wasm (read, write, multi-function).
- **Slice syntax:** `arr[start:end]` with Go-style decomposition passes.

### What's Missing

**See [GAP_ANALYSIS.md](GAP_ANALYSIS.md) for the full gap analysis.**

| Feature | Priority | Why |
|---------|----------|-----|
| Generics | HIGH | Blocks typed collections, standard library |
| Dynamic lists + maps | HIGH | Requires generics, needed for real apps |
| String interpolation | MEDIUM | Developer experience |
| Traits/Interfaces | MEDIUM | Polymorphism for std lib |
| ~486 test cases | MEDIUM | Edge case coverage vs bootstrap-0.2 |

---

## Key Documents

### Priority (read these first)

| Document | Purpose |
|----------|---------|
| `GAP_ANALYSIS.md` | **PROJECT PRIORITY: Feature + test gap vs bootstrap-0.2, recommended path** |
| `TROUBLESHOOTING.md` | **MUST READ: Debugging methodology - never invent, always copy reference** |
| `docs/PIPELINE_ARCHITECTURE.md` | **MUST READ: Full pipeline, reference map for every stage, extern fn, allocator** |
| `VISION.md` | Language vision, Phase 3 TODO list, execution roadmap |

### Reference (read when needed)

| Document | Purpose |
|----------|---------|
| `docs/BR_TABLE_ARCHITECTURE.md` | br_table dispatch loop pattern - READ if confused about br_table |
| `WASM_BACKEND.md` | Wasm milestones M1-M16, implementation details |
| `ROADMAP_PHASE2.md` | M17-M24 milestones (ARC, heap, strings, arrays) - all complete |
| `CRANELIFT_PORT_MASTER_PLAN.md` | Native AOT codegen architecture (✅ Complete) |
| `NATIVE_AOT_FIXES.md` | Historical: Native AOT bug fixes |
| `PHASE3_COMPLETE.md` | Historical: Phase 3 Wave 1-4 implementation details and Go parity evidence |
| `../bootstrap-0.2/DESIGN.md` | Technical architecture specification |

---

## Architecture - IMPORTANT: READ THIS

**Cot is a Wasm-first compiler. ALL code paths go through Wasm.**

**The native AOT path uses Cranelift's architecture (fully ported to Zig).**

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
         .wasm file                    wasm_parser (parse Wasm binary)
                                               ↓
                                      wasm_to_clif/ (Wasm → CLIF IR)
                                               ↓
                                      ir/clif/ (CLIF IR representation)
                                               ↓
                                      machinst/lower.zig (CLIF → MachInst)
                                               ↓
                                      isa/aarch64/lower.zig (ARM64 patterns)
                                      isa/x64/lower.zig (AMD64 patterns)
                                               ↓
                                      isa/*/inst/ (MachInst types)
                                               ↓
                                      isa/*/emit.zig (MachInst → bytes)
                                               ↓
                                      machinst/buffer.zig (code buffer)
                                               ↓
                                        .o file (Mach-O/ELF)
                                               ↓
                                         linker (clang)
                                               ↓
                                        executable
```

**Cranelift Port Structure (see CRANELIFT_PORT_MASTER_PLAN.md):**
- `compiler/ir/clif/` - CLIF IR types, DFG, layout, builder
- `compiler/codegen/native/wasm_to_clif/` - Wasm → CLIF translation
- `compiler/codegen/native/machinst/` - Machine instruction framework
- `compiler/codegen/native/isa/aarch64/` - ARM64 backend
- `compiler/codegen/native/isa/x64/` - AMD64 backend (TODO)

**Key points for Claude:**
1. **Native output is Mach-O/ELF** - This is EXPECTED, not an error
2. **Native goes through Wasm then CLIF IR** - Pipeline: Cot → Wasm → CLIF IR → MachInst → Native
3. **To get .wasm output**: Use `--target=wasm32`
4. **Default is native**: Running `cot file.cot` produces a native executable
5. **Follow Cranelift exactly**: The port must match Cranelift's architecture 100%

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
- Reference docs before implementing: check WASM_BACKEND.md or CRANELIFT_PORT_MASTER_PLAN.md
- Reference `bootstrap-0.2/` for working code examples
- Make incremental changes, verify each one
- Ask user for direction when uncertain

### DO NOT

- Modify bootstrap-0.2 (it's frozen)
- Skip testing
- Make large changes without verification
- Autonomously change direction

### When Stuck

1. Check the relevant planning doc (WASM_BACKEND.md or CRANELIFT_PORT_MASTER_PLAN.md)
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
| `audit/SUMMARY.md` | Test results and component status |

---

## Current Tasks

**The project priority is closing the gap with bootstrap-0.2.** See [GAP_ANALYSIS.md](GAP_ANALYSIS.md).

Every new feature must:
1. **Work on Wasm** (`--target=wasm32`)
2. **Work on native** (default target, AOT through CLIF)
3. **Have test cases** (both `.cot` test files and unit tests)
4. **Copy the reference implementation** (Go for Wasm path, Zig for language semantics, Cranelift for native path)

### Priority: Close the Feature Gap

| Wave | Features | Status |
|------|----------|--------|
| **A (Fundamentals)** | Floats, defer, union payloads, error unions, function pointers | ✅ COMPLETE |
| **B (Expressiveness)** | Closures ✅, generics, string interpolation, dynamic collections | IN PROGRESS |
| **C (Test Parity)** | Port ~486 test cases from bootstrap-0.2 | TODO |

### Reference Implementations

| Feature | Primary Reference | Location |
|---------|------------------|----------|
| Wasm codegen | Go compiler | `~/learning/go/src/cmd/compile/internal/wasm/` |
| Language semantics | Zig compiler | `~/learning/zig/` (for error unions, comptime, defer) |
| Native AOT | Cranelift | `~/learning/wasmtime/cranelift/` |
| Register allocation | regalloc2 | `~/learning/regalloc2/src/` |

**Cot follows Zig's language design** (error unions, defer, comptime) but uses **Go's compilation patterns** (SSA → Wasm) and **Cranelift's native codegen** (CLIF → MachInst → ARM64/x64).

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
- **Solution**: Wasm-first architecture (stack machine, single calling convention)
- **bootstrap-0.2**: Previous compiler with 619 test cases, direct native codegen (AMD64/ARM64)
- **Current cot**: Wasm-first rewrite that has surpassed bootstrap-0.2 in features (114 test files, 832 total tests)
- **Wasm backend** M1-M16 complete, **ARC** M17-M19 complete, **Language** M20-M23 complete
- **Native AOT** complete via Cranelift port (CLIF IR, regalloc2, ARM64/x64) - 24 native E2E tests
- **Phase 3 Wave 1-4** language features verified on both Wasm and native
- **Phase 3 Wave 5** complete: floats, closures, function pointers, error unions, defer, ARC coverage, union payloads
- **Next**: Generics → standard library → ecosystem (see GAP_ANALYSIS.md)
