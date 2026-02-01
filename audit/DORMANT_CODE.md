# Dormant Code Inventory

This document tracks code that exists but is not currently wired into the main compilation pipeline. Each item should be either integrated or removed.

## Status Legend
- **INTEGRATE**: Should be wired up in a future milestone
- **REMOVE**: No longer needed, can be deleted
- **KEEP**: Test infrastructure or intentionally separate

---

## 1. compile.zig - Wrapper Functions

**Location**: `compiler/ssa/compile.zig`

**Functions**:
- `compileExpression()` - Compiles a single expression to SSA
- `compileStatements()` - Compiles statement list to SSA

**Status**: KEEP (Test Infrastructure)

**Reason**: These are convenience wrappers used by unit tests to compile code snippets without going through the full driver pipeline. They're not dead code - they serve the test suite.

---

## 2. dom.zig - Dominance Frontiers

**Location**: `compiler/ssa/dom.zig`

**Purpose**: Computes dominance frontiers for SSA optimization passes (needed for proper phi node placement, dead code elimination, etc.)

**Status**: INTEGRATE (M15 or later)

**Plan**: When implementing advanced optimizations (constant propagation, dead code elimination, loop-invariant code motion), this infrastructure will be needed. Currently the compiler works without it because we don't do those optimizations yet.

**Go Reference**: `cmd/compile/internal/ssa/dom.go`

---

## 3. wasm_gen.zig - Legacy Wrapper

**Location**: `compiler/codegen/wasm_gen.zig`

**Status**: REMOVE (after extracting FuncIndexMap)

**Details**:
- `FuncIndexMap` type is still imported by `driver.zig`
- Rest of file is superseded by `wasm/gen.zig`

**Plan**: Move `FuncIndexMap` to `wasm/gen.zig` or a types file, then delete `wasm_gen.zig`.

---

## 4. generic.zig - Generic Pass Infrastructure

**Location**: `compiler/ssa/passes/generic.zig`

**Purpose**: Infrastructure for writing generic SSA transformation passes.

**Status**: INTEGRATE (when adding more passes)

**Plan**: This provides utilities that could simplify writing new passes. Review when adding next pass to see if it's useful.

---

## 5. dwarf.zig - Debug Info Generation

**Location**: `compiler/codegen/native/dwarf.zig`

**Purpose**: Generate DWARF debug information for native binaries.

**Status**: INTEGRATE (AOT Phase 5 or later)

**Plan**: After AOT Phase 4 wires up native codegen, Phase 5 should add debug info support so native binaries can be debugged with lldb/gdb.

---

## Action Items

| Item | Action | Milestone | Effort |
|------|--------|-----------|--------|
| wasm_gen.zig | Move FuncIndexMap, delete file | Next cleanup | Low |
| dom.zig | Wire up for optimizations | M17+ | Medium |
| generic.zig | Evaluate usefulness | M17+ | Low |
| dwarf.zig | Integrate for debug builds | AOT Phase 5 | Medium |
| compile.zig | Keep as-is | N/A | None |

---

## Review Schedule

This document should be reviewed:
- After each major milestone completion
- Before any "cleanup" sprints
- When test failures suggest orphaned code
