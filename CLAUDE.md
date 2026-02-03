# Claude AI Instructions

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
| M15 | ✅ Done | ARC runtime (retain/release in arc.zig, integrated with Linker) |
| M16 | ✅ Done | Browser imports (import section, import-aware exports in link.zig) |

### Verified Test Coverage (777/779 passing)

| Category | Tests | Status |
|----------|-------|--------|
| Wasm Codegen | 65+ | ✅ All pass |
| Native Codegen | 700+ | ✅ Most pass (2 skipped) |

### Known Gaps

- **Struct-by-value params**: Not yet implemented (workaround: use field access directly)
- **Native driver wiring**: Cranelift port ~80% complete, driver integration in progress

### AOT Native Progress

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 0-6 | ✅ Done | CLIF IR, Wasm translation, MachInst, ARM64/x64, regalloc |
| Phase 7 | 🔄 80% | Integration - block args, regalloc fixes done, driver wiring in progress |

**Recent Fixes:**
- Block call argument handling for control flow
- Register allocation mismatch in emitWithAllocs
- Args/Rets pseudo-instructions for function params/returns

**See `CRANELIFT_PORT_MASTER_PLAN.md` for full details.**

---

## Key Documents

| Document | Purpose |
|----------|---------|
| `CRANELIFT_PORT_MASTER_PLAN.md` | **Native AOT codegen - phases, status, and 5 critical blockers** |
| `WASM_BACKEND.md` | Wasm milestones M1-M16, implementation details |
| `ROADMAP_PHASE2.md` | M17-M24 detailed plan with Go/Swift research |
| `TESTING.md` | Testing strategy and test organization |
| `VISION.md` | Language vision, strategy, roadmap |
| `README.md` | Project overview and quick start |
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

**See `CRANELIFT_PORT_MASTER_PLAN.md` for native AOT status.**

### Priority Order (Recommended)

1. **Complete Phase 7** - Wire Cranelift port into driver.zig for native binary output
2. **M21-M22: Language features** - String ops, array append, for-range loops
3. **Browser imports** - Import section for JS interop (console.log, DOM access)
4. **E2E testing** - Full end-to-end test programs combining all features

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
- **Wasm backend** M1-M16 complete: full feature set compiles to .wasm
- **Cranelift port** ~80% complete: CLIF IR, regalloc, ARM64/x64 backends working
