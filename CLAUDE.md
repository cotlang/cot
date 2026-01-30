# Claude AI Instructions

## Zig 0.15 API Changes

**ArrayList requires allocator for each operation:**
```zig
// OLD (Zig 0.13)
var list = std.ArrayList(u8).init(allocator);
try list.append(42);
const w = list.writer();

// NEW (Zig 0.15) - use ArrayListUnmanaged
var list: std.ArrayListUnmanaged(u8) = .{};
defer list.deinit(allocator);
try list.append(allocator, 42);
const w = list.writer(allocator);
```

**Key patterns:**
- Use `std.ArrayListUnmanaged(T)` instead of `std.ArrayList(T)`
- Pass `allocator` to `append()`, `appendSlice()`, `writer()`, `deinit()`
- Initialize with `.{}` instead of `.init(allocator)`
- Store allocator in struct if needed for multiple operations

---

## Project: Cot Programming Language

**Cot** is a Wasm-first programming language for full-stack web development.

**The pitch:** Write like TypeScript, run like Rust, deploy anywhere, never think about memory.

**This repository** contains the Cot compiler, written in Zig. Like Deno (Rust) for TypeScript, this is a permanent tool, not a temporary bootstrap.

---

## Key Documents

| Document | Purpose |
|----------|---------|
| `VISION.md` | Language vision, strategy, and roadmap |
| `README.md` | Project status and structure |
| `../bootstrap-0.2/DESIGN.md` | Technical architecture specification |

---

## Strategy

The compiler stays in Zig. Self-hosting is a **future goal**, not an immediate one.

See `VISION.md` for full rationale.

---

## Current State (January 2026)

### Completed (80% of refactor)

| Component | Files | Status |
|-----------|-------|--------|
| Core | types, errors, target, testing | ✅ Done |
| Frontend | scanner, parser, checker, IR, lowerer | ✅ Done |
| SSA | op, value, block, func, liveness, regalloc, stackalloc | ✅ Done |
| SSA Passes | schedule, decompose, expand_calls | ✅ Done |
| Object Files | ELF, Mach-O, DWARF | ✅ Done |
| Pipeline | driver, main, pipeline_debug | ✅ Done |

### Next Phase: Wasm Backend

The compiler needs to emit Wasm bytecode instead of native ARM64/AMD64.

**Native codegen (Round 5) was intentionally skipped** - it will become the AOT compiler, not the main Cot compiler.

---

## Reference Repositories

### bootstrap-0.2 (FROZEN REFERENCE)

Location: `../bootstrap-0.2/`

**DO NOT MODIFY** - Use as reference only.

| File | Purpose |
|------|---------|
| `DESIGN.md` | Complete Wasm architecture specification |
| `src/codegen/arm64.zig` | Working ARM64 codegen (for AOT reference) |
| `src/codegen/amd64.zig` | Working AMD64 codegen (for AOT reference) |
| `src/cot1/` | Self-hosted compiler written in Cot |

**When to reference bootstrap-0.2:**
- Understanding how existing code works
- Debugging issues in 0.3
- Seeing working implementations

### cot (ACTIVE DEVELOPMENT)

This repository. The Cot 0.3 Wasm-first compiler.

---

## Architecture

```
Cot Compiler:
  Cot Source → Scanner → Parser → Checker → Lowerer → IR → Wasm
                                                           ↑
                                                     (NEXT PHASE)

AOT Compiler (future):
  Wasm → Parser → SSA → Regalloc → Native → ELF/Mach-O
                        ↑
                  (reuses existing SSA infrastructure)
```

**Key insight**: SSA and native codegen live in the AOT, not the main compiler. The Cot compiler emits Wasm directly from IR - no register allocation needed.

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

- **Write tests first** (TDD) - tests define the spec
- Run tests after every change: `zig build test`
- Reference `bootstrap-0.2/DESIGN.md` for architecture questions
- Reference `bootstrap-0.2/src/` for working code examples
- Make incremental changes, verify each one
- Ask user for direction when uncertain

### DO NOT

- Modify bootstrap-0.2 (it's frozen)
- Work on native codegen (that's for AOT, not main compiler)
- Skip testing
- Make large changes without verification
- Autonomously change direction

### When Stuck

1. Check if bootstrap-0.2 has working code for this
2. Reference DESIGN.md for intended architecture
3. Report the issue clearly and ask user how to proceed

---

## Memory Management: ARC

Cot uses Automatic Reference Counting:
- Compiler inserts retain/release automatically
- Same semantics for Wasm and native AOT targets
- See `bootstrap-0.2/DESIGN.md` section "Memory Management: ARC"

---

## Testing

**Test-Driven Development is mandatory.** Write tests first, then implement.

### Two Levels of Tests

**1. Zig Compiler Tests** (testing the compiler itself)
```zig
// In compiler/codegen/wasm.zig
test "emit i64.const" {
    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();
    try emitI64Const(&buf, 42);
    try testing.expectEqualSlices(u8, &[_]u8{ 0x42, 42 }, buf.items);
}
```

**2. Cot Language Tests** (testing the language, Zig-style inline syntax)
```cot
fn add(a: int, b: int) int {
    return a + b
}

test "add works" {
    assert(add(2, 3) == 5)
    assert(add(-1, 1) == 0)
}
```

### Running Tests

```bash
# All compiler tests
zig build test

# Specific file
zig test compiler/frontend/parser.zig

# With debug output
COT_DEBUG=parse zig test compiler/frontend/parser.zig
```

### Test Requirements

- Every new function needs tests
- Tests run before committing
- Failing tests block progress - fix them first

---

## File Locations

| Need | Location |
|------|----------|
| Language vision | `./VISION.md` |
| Wasm implementation plan | `./WASM_BACKEND.md` |
| Architecture spec | `../bootstrap-0.2/DESIGN.md` |
| Working native codegen | `../bootstrap-0.2/src/codegen/` |
| Cot compiler code | `../bootstrap-0.2/src/cot1/` |
| **Zig compiler** | `./compiler/` |
| **Cot stdlib** | `./stdlib/` |
| **Wasm runtime** | `./runtime/` |
| Audit documentation | `./audit/` |
| Progress tracking | `./REFACTOR_PLAN.md` |
| Project overview | `./README.md` |

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

## Audit Files

The `audit/` directory contains function-by-function verification for each refactored file:
- Line count changes (0.2 → 0.3)
- What was kept/removed/simplified
- Test coverage notes

Reference these when understanding what a file does or verifying correctness.

---

## History

- **5 attempts** at self-hosting with native codegen all failed
- **Root cause**: Native codegen complexity (register allocation, two ISAs, ABI edge cases)
- **Solution**: Wasm as primary target (stack machine, single calling convention)
- **Frontend cleanup** (Rounds 1-4) completed: 54% code reduction
- **Native codegen** (Round 5) intentionally skipped - becomes AOT instead
