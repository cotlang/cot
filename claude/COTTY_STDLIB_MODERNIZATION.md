# Cotty Stdlib Modernization — Eliminate Raw Builtins

**Date:** 2026-03-21
**Status:** In Progress
**Goal:** All low-level operations in stdlib wrappers. Cotty reads like TypeScript, not C.

---

## Current State: 347 raw builtin uses across 30 files

| Category | Count | Risk |
|----------|------:|------|
| `@intToPtr` / `@ptrToInt` | 138 | HIGH — raw pointer casts in app logic |
| `@sizeOf` | 71 | MEDIUM — coupled with @intToPtr arithmetic |
| `@intCast` | 37 | LOW — type narrowing (acceptable) |
| Process ops (fork/execve/pipe/dup2/setsid/ioctl) | 45 | HIGH — should use stdlib wrappers |
| `fd_read` / `fd_write` / `fd_close` | 22 | MEDIUM — should use std/io |
| `memcpy` | 19 | HIGH — raw memory copying |
| `@ptrOf` | 13 | MEDIUM — string internals |
| `@atomicStore` / `@atomicExchange` | 6 | CRITICAL — broken on native, blocks terminal |

**Target: ~80 remaining (FFI boundary only, in ffi.cot)**

---

## Phase 1a: `Atomic(T)` type — COMPLETE (already existed in stdlib)

**File:** `stdlib/thread.cot`

Add `Atomic(T)` struct with:
- `fn store(val: T) void` — wraps `@atomicStore`
- `fn load() T` — wraps `@atomicLoad`
- `fn exchange(val: T) T` — wraps `@atomicExchange`

Reference: Zig `std.atomic.Value`, Go `sync/atomic`

**Fixes:** Terminal rendering bug (broken `@atomicStore` on struct fields through `self` pointer)

---

## Phase 1b: `List(T).getPtr(idx)` — PENDING

**File:** `stdlib/list.cot`

Add method:
```cot
fn getPtr(index: i64) *T {
    if (index < 0 or index >= self.count) { @trap() }
    return @intToPtr(*T, self.items + index * @sizeOf(T))
}
```

**Eliminates:** ~80 `@intToPtr(*T, items + idx * @sizeOf(T))` pairs across Cotty

---

## Phase 1c: `mem.copy` / `mem.zero` — PENDING

**File:** `stdlib/mem.cot`

Add:
```cot
fn copy(dst: i64, src: i64, len: i64) void { memcpy(dst, src, len) }
fn zero(ptr: i64, len: i64) void { memset_zero(ptr, len) }
```

**Eliminates:** 19 raw `memcpy` calls in terminal.cot

---

## Phase 1d: `Pipe` type — PENDING

**File:** `stdlib/process.cot`

Add:
```cot
struct Pipe {
    read_fd: i64,
    write_fd: i64,
    static fn create() Pipe { ... }
    fn closeRead() void { fd_close(self.read_fd) }
    fn closeWrite() void { fd_close(self.write_fd) }
}
```

**Eliminates:** 6 raw `pipe()` + `pipeReadFd`/`pipeWriteFd` calls

---

## Phase 2: `std/pty.cot` module — PENDING

**New file:** `stdlib/pty.cot`

Extract PTY logic from `libcotty/src/pty.cot`:
```cot
struct Pty {
    master_fd: i64,
    child_pid: i64,
    static fn spawn(shell: string, rows: i64, cols: i64) Pty { ... }
    fn setWinsize(rows: i64, cols: i64) void { ... }
    fn write(data: string) i64 { ... }
    fn close() void { ... }
}
```

**Eliminates:** ~35 raw process builtins (fork, dup2, setsid, ioctl, execve) from Cotty

---

## Phase 3: Terminal cell access — PENDING

Replace in `libcotty/src/terminal.cot`:
```cot
// Before:
@intToPtr(*Cell, self.cells.items + idx * @sizeOf(Cell))
// After:
self.cells.getPtr(idx)
```

Also add:
- `fn copyRow(dst_row, src_row)` using `mem.copy`
- `fn clearRow(row)` using `mem.zero`

**Eliminates:** ~50 `@intToPtr`+`@sizeOf` pairs, 17 `memcpy` calls

---

## Phase 4: Surface cleanup — PENDING

In `libcotty/src/surface.cot`:
- Replace `@atomicStore(&self.render_dirty, 1)` → `self.render_dirty.store(1)`
- Replace `@atomicExchange(&self.render_dirty, 0)` → `self.render_dirty.exchange(0)`
- Replace `pipe()` → `Pipe.create()`
- Replace fork/dup2 → `std/pty` from Phase 2

**Eliminates:** ~25 raw builtins

---

## Phase 5: FileTree cleanup — PENDING

In `libcotty/src/file_tree.cot`:
```cot
// Before:
@intToPtr(*FileEntry, self.entries.items + idx * @sizeOf(FileEntry))
// After:
self.entries.getPtr(idx)
```

**Eliminates:** ~11 `@intToPtr` calls

---

## Phase 6: FFI layer audit — PENDING

`ffi.cot` is the **intentional unsafe boundary** — opaque pointer handles are correct (Ghostty pattern).
- Add null guards to all 45 exported workspace functions
- Document which builtins are intentional vs legacy
- Remove dead code

**Expected:** ~80 builtins remain in ffi.cot (acceptable)

---

## Phase 7: Fix terminal rendering — IN PROGRESS

Uses `Atomic(i64)` from Phase 1a:
```cot
// Before (broken — @atomicStore through self pointer chain):
render_dirty: i64,
fn markRenderDirty() void { @atomicStore(&self.render_dirty, 1) }

// After (correct — Atomic wraps the builtin safely):
render_dirty: Atomic(i64),
fn markRenderDirty() void { self.render_dirty.store(1) }
```

---

## Implementation Order

```
Phase 1a (Atomic)      ← FIRST — fixes terminal rendering
Phase 7 (fix render)   ← uses 1a, unblocks Cotty testing
Phase 1b (List.getPtr) ← biggest cleanup enabler
Phase 1c (mem.copy)    ← eliminates memcpy
Phase 1d (Pipe)        ← cleaner process setup
Phase 3 (terminal)     ← uses 1b + 1c, biggest file
Phase 2 (Pty module)   ← extract to stdlib
Phase 5 (FileTree)     ← uses 1b
Phase 4 (Surface)      ← uses all phases
Phase 6 (FFI audit)    ← final cleanup
```

## Impact

| Metric | Before | After |
|--------|--------|-------|
| Raw builtins in Cotty | 347 | ~80 (FFI only) |
| stdlib modules used | 8 | 14+ |
| New stdlib types | — | Atomic(T), Pipe, Pty, mem.copy/zero, List.getPtr |
| Terminal rendering | Broken | Fixed |
| Code style | Low-level C | TypeScript-like |

## Principle

Every builtin usage outside `ffi.cot` is a missing stdlib wrapper. If a Cot developer needs `@intToPtr` in their app code, the stdlib has failed them.
