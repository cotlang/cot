# ASan Implementation Plan for Cot

**Date:** 2026-03-18
**Goal:** Memory safety instrumentation for debug builds, catching heap buffer overflow, use-after-free, double-free, and uninitialized reads.

---

## Reference Language Audit

### Go: Compiler Instrumentation + LLVM ASan Runtime

Go (`go build -asan`) inserts `asanread(addr, sz)` / `asanwrite(addr, sz)` calls at every memory access. These forward through assembly trampolines (`runtime/asan_arm64.s`) to LLVM's `compiler-rt` shadow memory checks. Additionally, Go poisons freed GC spans with `asanpoison` and unpoisons on reallocation.

Key pattern: `if asanenabled { asanread(ptr, size) }` before every raw memory operation (growslice, makeslicecopy, map access, channel ops).

**Not suitable for Cot:** Requires LLVM's `compiler-rt` library. Cot doesn't use LLVM.

### Zig: Language-Level Safety + Debug Allocator

Zig implements safety at three levels:

1. **Compile-time checks** (`Sema.zig:addSafetyCheck`): In Debug/ReleaseSafe mode, inserts runtime bounds checks, null unwrap checks, overflow checks. Gated by `block.wantSafety()`.

2. **Undefined memory fill** (`codegen.zig:321`): Fills undefined/freed memory with `0xAA`. Makes use-after-free and uninitialized reads immediately produce recognizable values.

3. **GeneralPurposeAllocator (GPA)**: Debug allocator that tracks every allocation with metadata, detects double-free, use-after-free, leaks. Adds guard pages around allocations (optional).

**Best fit for Cot:** We already have Zig's pattern partially (bounds checks in lower.zig, poison on dealloc). Need to complete it.

### Rust/Cranelift: No Sanitizer Support

Cranelift (our codegen reference) has **zero sanitizer infrastructure**. No shadow memory, no instrumentation passes. Rust's `-Z sanitizer=address` works only with LLVM backend. This means Cot cannot inherit any sanitizer support from Cranelift.

---

## Cot's Current Infrastructure

| Feature | Status | Gap |
|---------|--------|-----|
| ARC magic sentinel (0xC07A8C00) | Done | Works |
| Pointer range check (< 4096) | Done | Works |
| Poison on dealloc (0xDEADDEAD) | Done | Silent — should report |
| Bounds checks in lower.zig | Done | Only for array/slice, not Map |
| alloc_raw (no ARC header) | Done | No redzones |
| Double-free detection (IsDeiniting) | Done | Works |

---

## Implementation Plan: Three Tiers (Zig Pattern)

### Tier 1: Redzone Allocator (CRITICAL — catches Map buffer overflow)

**What:** Add guard bytes (redzones) before and after every `alloc_raw` allocation. Validate redzones on `realloc_raw` and `dealloc_raw`. If corrupted, print exact corruption location and abort.

**Layout:**
```
[LEFT_REDZONE 16 bytes: 0xFAFAFAFA...] [user data: N bytes] [RIGHT_REDZONE 16 bytes: 0xFBFBFBFB...]
```

**Allocation:** `alloc_raw(size)` allocates `size + 32`, fills left redzone with 0xFA, right redzone with 0xFB, returns pointer to user data.

**Validation:** Before every `realloc_raw` and `dealloc_raw`:
1. Read 16 bytes before user data — check all are 0xFA
2. Read 16 bytes after user data — check all are 0xFB
3. If either is corrupted: print "heap buffer overflow detected at 0xNNNN" with the exact corrupted byte offset, then abort

**Where to implement:**
- `compiler/codegen/native/arc_native.zig`: `generateAllocRaw` — add 32 bytes to malloc size, fill redzones
- `compiler/codegen/native/arc_native.zig`: `generateReallocRaw` — validate redzones before free(old)
- `compiler/codegen/native/arc_native.zig`: `generateDeallocRaw` — validate redzones before free

**Gating:** Only in debug mode (`!release_mode`). Release mode allocations have no redzones.

**Reference:** Go's `growslice` validates buffers with `asanread` before copy. Zig's GPA adds guard pages. LLVM ASan uses 128-byte redzones.

**Overhead:** 32 extra bytes per raw allocation + one memcmp per realloc/dealloc. Negligible.

**Estimated effort:** ~100 lines of CLIF IR generation.

### Tier 2: Loud Use-After-Free Detection (HIGH)

**What:** Change poison detection from silent skip to loud abort in debug mode.

**Current behavior:** `retain`/`release` check magic at `obj-32`. If magic is 0xDEADDEAD (freed), they silently return (same as non-heap pointer).

**New behavior in debug mode:** If magic == 0xDEADDEAD, print:
```
panic: use-after-free detected: retain/release on freed object at 0xNNNN
```
Then call `__cot_print_backtrace()` and `exit(2)`.

**Where to implement:**
- `compiler/codegen/native/arc_native.zig`: `generateRetain` — after magic check, add poison check branch
- `compiler/codegen/native/arc_native.zig`: `generateRelease` — same

**The magic check flow becomes:**
```
if magic == ARC_HEAP_MAGIC → continue (valid ARC object)
if magic == POISON_MAGIC → abort with "use-after-free" (debug mode only)
else → skip (non-heap pointer, silent)
```

**Reference:** Zig fills freed memory with 0xAA and GPA reports on access. Go's `asanpoison` marks freed spans so ASan catches reads.

**Overhead:** One extra comparison per retain/release. Negligible.

**Estimated effort:** ~30 lines of CLIF IR in retain + release.

### Tier 3: Fill-on-Alloc Pattern (MEDIUM)

**What:** Fill newly allocated memory with recognizable patterns to detect uninitialized reads.

**Patterns (matching Zig):**
- `alloc` (ARC objects): Fill user data with 0xAA after header init
- `alloc_raw` (backing buffers): Fill with 0xAA after malloc
- `dealloc` (ARC objects): Fill entire user data with 0xDD before free
- `dealloc_raw` (backing buffers): Fill with 0xDD before free

**Where to implement:**
- `compiler/codegen/native/arc_native.zig`: All four alloc/dealloc functions
- Use `memset` libc call to fill (already imported for `memset_zero`)

**Reference:** Zig `codegen.zig:321` — fills undefined memory with 0xAA. macOS `MallocScribble` fills freed memory with 0x55.

**Overhead:** One memset per alloc/dealloc. Moderate for large allocations.

**Estimated effort:** ~40 lines of CLIF IR across 4 functions.

---

## Gating Mechanism

All three tiers gated by `release_mode` flag (already exists in `Lowerer`):

```
// In Lowerer: self.release_mode (set by --release flag)
// In native codegen: checked at function generation time
// Debug mode (default): all safety checks enabled
// Release mode (--release): no safety checks, no redzones, no fill
```

For the ARC runtime functions (retain/release/alloc/dealloc), gating is at **code generation time**: the Zig compiler generates DIFFERENT runtime function bodies depending on whether the target is debug or release. This is the same as Zig's `wantSafety()` — the safety checks are compiled into the binary, not toggled at runtime.

**Implementation:** Add a `debug_mode: bool` parameter to `arc_native.generate()` and `io_native.generate()`. When true, generate runtime functions with redzones, poison detection, and fill patterns. When false, generate minimal versions.

---

## Priority Order

| Tier | Feature | Catches | Effort | Priority |
|------|---------|---------|--------|----------|
| 1 | Redzones on alloc_raw | Heap buffer overflow (the Map bug) | 100 lines | IMPLEMENT NOW |
| 2 | Loud poison detection | Use-after-free (silent corruption) | 30 lines | IMPLEMENT NOW |
| 3 | Fill-on-alloc/dealloc | Uninitialized reads | 40 lines | NEXT |

Total: ~170 lines of CLIF IR generation code.

---

## Future: Full ASan (Post-1.0)

For full ASan coverage (stack buffer overflow, global overflow, use-after-return), Cot would need:
1. Shadow memory infrastructure (1 shadow byte per 8 app bytes)
2. Instrumentation pass in `ssa_to_clif.zig` that inserts shadow checks before every load/store
3. Runtime library for shadow memory management

This is a significant undertaking (~5000 lines) but would bring Cot to parity with Go/Rust sanitizer support. For now, the three-tier Zig-style approach catches the most common bugs with minimal effort.

---

## Reference Files

| Reference | File | Pattern |
|-----------|------|---------|
| Go ASan integration | `references/go/src/runtime/asan.go` | `asanread`/`asanwrite` before every memory op |
| Go growslice ASan | `references/go/src/runtime/slice.go:187-189` | validate old buffer before copy |
| Zig safety checks | `references/zig/src/Sema.zig:26482` | `addSafetyCheck` conditional branch |
| Zig undefined fill | `references/zig/src/codegen.zig:321` | 0xAA pattern for undefined memory |
| Cot ARC runtime | `compiler/codegen/native/arc_native.zig` | magic check, poison, range check |
| Cot bounds checks | `compiler/frontend/lower.zig:10565-10646` | Go-style panic on out-of-bounds |
