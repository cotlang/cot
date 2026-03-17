# Memory & ARC Audit — Proper Debugging Plan

**Date:** 2026-03-17
**Status:** RESOLVED — ir.cot now compiles in 17MB/0.5s. Memory explosion was from exponential generic re-checking (fixed by isDefined guard + depth limit). ARC use-after-free fixed by `load [copy]` retain in lower.zig:2448-2470. Remaining crash is stack overflow from native codegen frame bloat (see STACK_FRAME_ANALYSIS.md).

**Original problem:** ir.cot compilation either crashes (MallocScribble) or runs to 1GB+ (without). Zero measurements taken. All previous analysis was speculation.

---

## What We DON'T Know (Everything)

1. WHERE memory grows — which function allocates the most?
2. WHAT is allocated — ASTs? SSA values? checker data? Map backing arrays?
3. WHEN memory grows — during Phase 1 (parse)? Phase 2 (check)? Phase 3 (lower)?
4. WHY freed memory is accessed — which specific `*Ast` gets freed, by which release call?
5. HOW Go handles this — Go's compiler uses arenas per function, not ARC. What pattern?

## What We Need To Do

### Step 1: Measure memory at each compilation phase

Add RSS measurement between phases. Cot has `time()` for timestamps. For RSS, use a syscall or just print the allocation count.

Instrument `compileWithImports` in main.cot:
```
Phase 1 (parse): RSS before / after
Phase 2 (check): RSS before / after per file
Phase 3 (lower): RSS before / after per file
Codegen: RSS before / after
```

### Step 2: Count allocations per phase

Add a global allocation counter. Every `alloc()` increments it. Print the counter between phases to see WHERE allocations happen.

### Step 3: Trace the specific freed memory

With MallocScribble, the crash is `ast.cot:46: trap`. Add a print INSIDE `Ast.getNode` that checks `self.nodes.count` before the List.get:
```cot
fn getNode(idx: int) Node {
    if (self.nodes.count < 0 or self.nodes.count > 100000) {
        @panic("AST FREED: nodes.count=${self.nodes.count} idx=${idx} file=${self.filename}")
    }
    return self.nodes.get(idx)
}
```

This will show WHICH AST is freed and WHICH file it belongs to.

### Step 4: Audit Go's compiler memory pattern

Go's compiler uses per-function arenas. How does it handle ASTs?
- `references/go/src/cmd/compile/internal/syntax/` — parser creates AST nodes
- `references/go/src/cmd/compile/internal/types2/` — type checker references AST
- `references/go/src/cmd/compile/internal/ssagen/` — SSA gen references AST
- All in same process, GC handles lifetimes. No manual management.

### Step 5: Audit Swift's SILGen memory pattern

Swift's SILGen holds ASTs by strong reference. How are they retained?
- `references/swift/lib/SILGen/SILGenFunction.cpp` — function lowering
- `references/swift/lib/SILGen/SILGenModule.cpp` — module-level AST ownership
- The SILModule holds strong refs to all ASTs for the compilation unit.

## Execution

Do Step 1 and Step 3 FIRST — actual measurements, not speculation.
