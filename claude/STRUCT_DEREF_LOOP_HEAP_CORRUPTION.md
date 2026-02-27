# Bug: *StructType Dereference in Loops — Heap Corruption

## Status: FIXED (Feb 27, 2026)

Fixed by: ARC overhaul Phase 1 (commit `73f4c0b`) — `managed` flag on `PointerType`.
`@intToPtr(*T, addr)` now creates `managed=false` pointers, and `couldBeARC()` returns `false` for them.
Verified: Cell-like 64-byte struct deref in loops (2000+ iterations) — no heap corruption.
Related: STRUCT_RETURN_CODEGEN_BUG.md (same root cause family — ARC releasing raw pointers)

## Summary

Any function that dereferences `*StructType` (pointer to a struct > 8 bytes) inside a loop corrupts the heap. The compiler incorrectly ARC-releases raw pointers obtained from `@intToPtr` or pointer arithmetic, treating them as ARC-managed objects. This destroys heap metadata (alloc_size, refcount), causing crashes on subsequent allocations.

## Impact — Blocks Cotty Terminal

This bug blocks ALL terminal grid operations in Cotty (the Cot dogfooding project at `~/cot-land/cotty`). Every function that modifies cells in a loop is affected:

| Function | File | Pattern |
|----------|------|---------|
| `clearLine(row)` | `cotty/src/terminal.cot:69` | `getCell(row, col).* = Cell.init()` in while loop |
| `eraseInLine(mode)` | `cotty/src/terminal.cot:336` | `getCell(row, col).* = Cell.init()` in while loop |
| `eraseInDisplay(mode)` | `cotty/src/terminal.cot:306` | `getCell(row, col).* = Cell.init()` in while loop |
| `insertBlanks(count)` | `cotty/src/terminal.cot:380` | `getCell(row, c).* = getCell(row, c-n).*` in while loop |
| `deleteChars(count)` | `cotty/src/terminal.cot:402` | `getCell(row, c).* = getCell(row, c+n).*` in while loop |
| `resize(rows, cols)` | `cotty/src/terminal.cot:455` | `new_grid.getCell(row, col).* = self.grid.getCell(row, col).*` in nested loop |

Cell is a struct with 8 × i64 fields = 64 bytes. `getCell()` returns `*Cell` via `@intToPtr(*Cell, items + idx * @sizeOf(Cell))`.

When compiled as a dylib (`cot build --lib`) and called from Swift, feeding ANY VT escape sequence that triggers these functions crashes the app.

## Symptom

Cotty macOS app crashes (SIGBUS or EXC_BAD_ACCESS) when shell output is fed through the VT parser. The crash occurs inside ARC's `release` or `dealloc` functions, triggered by cleanup of a loop-scoped local that holds a `*Cell` value.

Simpler reproduction: `cot test ~/cot-land/cotty/src/test_deref.cot` — test 4 fails with heap corruption.

## Root Cause

The bug is in the same family as STRUCT_RETURN_CODEGEN_BUG.md — the compiler's `couldBeARC()` heuristic cannot distinguish between:

1. **ARC-managed pointers** (from `new T { ... }`) — SHOULD be retained/released
2. **Raw pointers** (from `@intToPtr`, pointer arithmetic, `getCell()`) — MUST NOT be retained/released

When a `*Cell` local goes out of scope in a loop body, ARC emits a `release()` call. But the pointer points into the middle of a `List(Cell)` backing buffer — not to an ARC-managed heap object. ARC reads the "refcount" at `ptr - 24`, which is actually cell data from an adjacent cell, and corrupts it.

### Detailed Chain

1. `clearLine(row)` calls `self.grid.getCell(row, col)` → returns `*Cell` (raw pointer into List buffer)
2. The loop body does `ptr.* = Cell.init()` — stores a Cell struct through the pointer
3. At loop iteration end, ARC sees the `*Cell` local going out of scope
4. `couldBeARC(*Cell)` returns `true` (Cell is a struct, could be heap-allocated)
5. ARC emits `release(ptr)` → reads `ptr - 24` as heap header → reads/modifies cell data
6. The "refcount" (actually a codepoint or color value) gets decremented
7. If it reaches 0, ARC calls `dealloc()` which frees memory in the middle of the List buffer
8. Subsequent List operations (append, realloc) crash

### Why It Only Manifests in Loops

On the first iteration, the pointer might be to a cell with non-zero "refcount" (actually cell data), so release just decrements and returns. But across iterations, the same or adjacent cells get repeatedly "released", eventually driving the fake refcount to 0 and triggering dealloc of the list's backing buffer.

## Proof of Concept

```cot
// ~/cot-land/cotty/src/test_deref.cot — test 4
test "call store *Pair" {
    const data = alloc(0, 16)
    @intToPtr(*i64, data).* = 42
    var i: i64 = 0
    while (i < 2) {
        const p = returnPairPtr(data)      // returns *Pair (raw pointer)
        if (@ptrToInt(p) == 0) { @assert(false) }
        i += 1                              // ARC releases p here — BOOM
    }
    @assertEq(@intToPtr(*i64, data).*, 42)  // Fails: data was corrupted
}
```

## Cotty-Specific Evidence

When Cotty's terminal view feeds shell output through the VT parser:

1. Shell sends `ESC[K` (erase in line) → `eraseInLine(0)` called
2. `eraseInLine` loops: `while (col < self.grid.cols) { self.grid.getCell(row, col).* = Cell.init(); col += 1; }`
3. Each iteration: `getCell(row, col)` returns `*Cell` into local, ARC releases it at end of iteration
4. After ~2 iterations, the List(Cell) buffer's heap metadata is corrupted
5. Next operation crashes in `realloc` / `memmove` / `free`

lldb backtrace:
```
frame #0: _platform_memset + 112       ← macOS free zeroing memory
frame #1: mfm_free + 304
frame #2: dealloc + 52                 ← Cot ARC dealloc
frame #3: release + 204                ← Cot ARC release
frame #4: <terminal function> + ???     ← ARC cleanup of *Cell local
```

## Fix Location

### The Real Fix: ARC Overhaul Phase 1 (ARC_OVERHAUL.md)

The proper fix is the `managed` flag on pointer types from Phase 1 of the ARC overhaul:

1. `*T` from `@intToPtr` → `managed = false` → ARC skipped
2. `*T` from pointer arithmetic → `managed = false` → ARC skipped
3. `*T` from `new T { ... }` → `managed = true` → ARC applied

**File**: `compiler/frontend/types.zig` — add `managed` flag to pointer type
**File**: `compiler/frontend/checker.zig` — infer `managed` from expression origin
**File**: `compiler/frontend/lower.zig` — only emit retain/release for managed pointers

See ARC_OVERHAUL.md Phase 1 for full implementation plan.

### Interim Workaround (if needed before ARC overhaul)

**File**: `compiler/frontend/lower.zig` — in `lowerLocalVarDecl`, skip ARC cleanup for `*T` locals whose value originates from:
- `@intToPtr` expressions
- Field access on a non-ARC struct (pointer arithmetic)
- Function calls that return `*T` from pointer arithmetic (heuristic: check if callee body uses `@intToPtr`)

This is fragile and the ARC overhaul is the correct solution.

## Reproduction

```bash
# Simple repro (test 4 fails)
cot test ~/cot-land/cotty/src/test_deref.cot

# Cotty repro (app crashes when terminal receives any erase sequence)
cd ~/cot-land/cotty
cot build src/main.cot --lib -o macos/Sources/CCottyCore/lib/libcotty.dylib
cd macos && swift build && .build/debug/Cotty
# Terminal opens, shell spawns, first ESC[K triggers crash
```

## Relationship to Other Bugs

- **STRUCT_RETURN_CODEGEN_BUG.md**: Same root cause family. That bug is about ARC releasing a struct return value that's actually a pointer into a list buffer. This bug is about ARC releasing a `*StructType` local that's a raw pointer.
- **ARC_OVERHAUL.md Phase 1**: The definitive fix. Once `managed` flag exists, both bugs are resolved.
