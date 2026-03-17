# Selfcot Phase 3 Debug Report — 2026-03-17

## Status

| Metric | Before | After |
|--------|--------|-------|
| ir.cot RSS | 1GB+ (hung) | **18MB** (crashes) |
| Memory explosion | Yes | **Fixed** |
| Files compiling | 0 | **5** (token, scanner, source, errors, types) |
| Current blocker | Stack corruption | Zig compiler native codegen |

## Fixes Applied Today

### 1. ARC retain for field access through pointer deref (compiler/frontend/lower.zig:2448-2470)

`const saved = self.tree` now correctly retains the loaded managed pointer. Previously, when the field was later overwritten (`self.tree = other`), ARC released the old value, and the local `saved` pointed to freed memory.

### 2. Recursion depth limit for checkFnDeclBody (self/frontend/checker.cot)

`body_check_depth > 2` prevents unbounded recursion in the Phase 3 re-check path.

## ROOT CAUSE: Native Codegen Generates 2x Oversized Stack Frames

**Proven by ARM64 prologue disassembly (`sub sp, #N` instructions):**

| Function | Selfcot frame | Zig's own equivalent | Ratio |
|----------|--------------|---------------------|-------|
| checkFnDeclBody | **3,392 B** (0xD40) | 1,648 B (0x670) | 2.1x |
| instantiateGenericFunc | **2,736 B** (0xAB0) | — | — |
| lowerBlockNode | **2,992 B** (0xBB0) | — | — |
| lowerCall | **2,688 B** (0xA80) | — | — |
| lowerMethodCall | **2,480 B** (0x9B0) | — | — |
| lowerGenericFnInstanceInner | **2,896 B** (0xB50) | — | — |

**One call chain uses ~23KB of stack:**
`lowerToBuilder → lowerDecl → lowerStructDecl → lowerMethodWithName → lowerBlockNode → lowerStmt → lowerExprNode → lowerCall → lowerMethodCall → ensureGenericFnQueued`

**With ir.cot's FuncBuilder (86 nested methods), the lowerer processes each method body through this chain, and the stack overflows at 8MB.**

**Evidence of corruption:**
```
lowered_generics.count = 4355079168 (0x103C00000)  ← pointer value, not a count
lowered_generics.capacity = 9                       ← not a power of 2
```

**Confirmed NOT layout bug:** `@ptrToInt(&self.lowered_generics)` offsets match expected values (392, 416, 424).
**Confirmed NOT recursion bug:** Crashes even with `checkFnDeclBody` completely disabled.
**Confirmed NOT generic bug:** Crashes even with `lowerGenericFnInstanceInner` returning immediately.
**Confirmed stack corruption:** lldb shows `bt` returning only 1 frame (corrupted return addresses).

## Fix Required in Zig Compiler

The `overlap_group` mechanism exists in `ir.zig` (lines 201-205, 253-318) and is used by `lower.zig` for switch arms (lines 7112-7164). But:

1. **If-else chains don't use overlap groups** — the selfcot's checker has many if-else chains that don't share stack slots
2. **The selfcot's functions have far more locals than the Zig compiler's own** — each branch creates locals that are never shared

The fix: extend `lower.zig` to emit `beginOverlapGroup()`/`nextOverlapArm()` for if-else chains, and potentially optimize `ssa_builder.zig` stack slot allocation to better handle large functions.

## Files That Compile Successfully

| File | Imports | Generic count | Result |
|------|---------|--------------|--------|
| token.cot | none | 0 | **Success** |
| scanner.cot | list, string, token, source, errors | ~12 | **Success** |
| source.cot | list, string | ~4 | **Success** |
| errors.cot | list, string, source | ~6 | **Success** |
| types.cot | list, map, string, sys | ~11 | **Success** |
| ast.cot | list, string, source | ~12 | **Non-deterministic** (stack-dependent) |
| ir.cot | list, map, string, types, source | ~19 | **Crash** (stack overflow) |
