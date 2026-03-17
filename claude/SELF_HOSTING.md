# Cot Self-Hosting: Status, Blocker, and Path to 0.4

**Updated:** 2026-03-17
**Goal:** `selfcot build self/main.cot -o /tmp/selfcot.wasm` produces a working Wasm compiler.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status: 9 of 13 Frontend Files Compile

| File | Lines | Status | RSS | Time |
|------|-------|--------|-----|------|
| token.cot | 448 | **Pass** | ~10MB | <0.1s |
| scanner.cot | 774 | **Pass** | ~10MB | <0.1s |
| source.cot | 315 | **Pass** | ~10MB | <0.1s |
| errors.cot | 545 | **Pass** | ~10MB | <0.1s |
| types.cot | 1,575 | **Pass** | ~15MB | <0.2s |
| ast.cot | 1,541 | **Pass** | ~17MB | <0.3s |
| ir.cot | 1,451 | **Pass** | 17MB | 0.5s |
| parser.cot | 3,235 | **Pass** | ~20MB | ~0.5s |
| ssa.cot | 619 | **Pass** | ~15MB | <0.2s |
| checker.cot | 5,947 | **Fail** | — | stack overflow |
| lower.cot | 9,177 | **Fail** | — | stack overflow |
| ssa_builder.cot | 2,337 | **Fail** | — | stack overflow |
| arc_insertion.cot | 414 | **Fail** | — | stack overflow |

After frontend, codegen/ (16 files) and main.cot still need to compile.

---

## Blocker: Native Codegen Stack Frame Bloat

The Zig compiler's native backend generates stack frames ~2x larger than necessary. Deep call chains during Phase 3 lowering of complex files overflow the 8MB stack.

**Hard evidence (ARM64 prologue disassembly, `sub sp, #N`):**

| Function | Selfcot | Zig's own | Ratio |
|----------|---------|-----------|-------|
| checkFnDeclBody | 2,528B | 1,648B | 1.5x |
| instantiateGenericFunc | 2,112B | — | — |
| lowerBlockNode | 2,384B | — | — |
| lowerCall | 2,096B | — | — |

One call chain (`lowerToBuilder → ... → checkFnDeclBody`) uses ~18KB.

### Root Cause: SRET Double-Copy (52% of bloat)

Each `getNode()` call returns a 176-byte `Node` union via SRET. The lowerer creates TWO locals per call:
1. `__sret_tmp` — SRET return buffer (176B)
2. Named result local (e.g., `fn_node`) — value copied from SRET (176B)

**352 bytes per getNode call × 5 calls = 1,760 bytes** in `checkFnDeclBody` alone.

The shared SRET optimization (already implemented) eliminated the duplicate `__sret_tmp` allocations, reducing frames by 22%. But the named result locals + union captures still contribute ~1,000 bytes.

### What's Been Fixed

1. **Shared SRET local** — one reusable buffer per function instead of one per call. Frames reduced 20-25%.
2. **If-else/catch overlap groups** — stack slot sharing for branching constructs (was only switch arms).
3. **Infinite generic queue loop** — stub functions for generics that fail to lower.
4. **Recursion depth limit** — `checkFnDeclBody` bounded to prevent stack corruption.
5. **SSA fwd_ref tolerance** — silently accepts unresolved forward refs from depth-limited re-checking.
6. **ARC `load [copy]`** — retain managed pointers loaded from fields through pointer deref.

### What Remains

**Option A: Further reduce stack frames (more codegen work)**
- Eliminate the SRET-to-local copy entirely by using the named local AS the SRET buffer
- Liveness-based stack slot reuse — locals with non-overlapping lifetimes share slots
- Expected: another 30-40% frame reduction, should be enough for all 4 files

**Option B: Eliminate Phase 3 re-checking (architectural, Go pattern)**
- Extend `resolveTypeNode` fallbacks to resolve ALL expression types without `expr_types`
- Remove `checkFnDeclBody` from lowering entirely
- Eliminates the deepest call chains, removes the stack overflow trigger
- Also frees checkers and ASTs after Phase 2 (Go `freePackage` pattern)

**Option C: Both** — implement B for correctness, A for general native codegen quality.

---

## ARC Status

**Fixed:**
- `load [copy]` for field access through pointer deref (`lower.zig:2448-2470`)
- `@ptrToInt` ownership transfer (`disableForLocal`, commit `81f65b8`)
- `store [assign]` for managed pointer field assignment (line 3274)

**Not a bug (verified):**
- `couldBeARC(struct) = false` — struct copies don't need copy witnesses

**ARC audit checklist (Swift SIL patterns):**
- [x] `const x = self.field` where field is `*T` → `load [copy]` (retain + cleanup)
- [x] `self.field = x` through pointer deref → `store [assign]` (retain new, release old)
- [x] `@ptrToInt(managed_ptr)` → transfer ownership
- [ ] `@intToPtr(*T, raw_int)` → verify creates borrowed (+0) reference
- [ ] Function return of managed pointer → verify +1 (caller-owned)
- [ ] Function parameter of managed pointer → verify +0 (borrowed)

---

## Performance (once self-hosting works)

| Metric | Current (Zig-compiled cot) | Selfcot (estimated) |
|--------|---------------------------|---------------------|
| Compile self/ | 4.5s / 903MB | ~240s / 4.8GB |
| Per-line | 107μs | 5,700μs (53x slower) |

**Root causes of 53x gap:**
1. No optimization passes (10-20x) — no DCE, CSE, constant folding
2. No inlining (5-10x) — every List.get/set is a real call
3. Value-type copying (3-5x) — 144-byte SsaValue copied per access
4. ARC overhead (2-3x) — unnecessary retain/release on empty strings

**Performance fix priority (post self-hosting):**
1. Memory reduction → cache locality improvement (2-5x)
2. Pointer-based value access → eliminate memcpy overhead (3-5x)
3. Inlining hot functions → eliminate call overhead (5-10x)
4. SSA optimization passes → general code quality (2-3x)
5. Target: <15s self-compile (matching Go's trajectory at equivalent maturity)

---

## Path to 0.4

### Phase 1: Complete Self-Hosting (current)
- [x] 9/13 frontend files compile
- [ ] Remaining 4 files: checker, lower, ssa_builder, arc_insertion
- [ ] codegen/ files (16 files)
- [ ] main.cot compiles
- [ ] `selfcot build self/main.cot` produces working Wasm
- [ ] Validate: `wasmtime selfcot.wasm build self/test_tiny.cot` succeeds

### Phase 2: Release Polish (from RELEASE_PLAN.md)
- [ ] Homebrew tap + x86_64-macos binary
- [ ] VS Code marketplace extension
- [ ] `cot upgrade` self-update
- [ ] Shell completions (zsh, bash, fish)
- [ ] Error messages polish pass

### Phase 3: Performance (post-release)
- [ ] Memory reduction (target: <200MB for self-compile)
- [ ] Inlining pass (target: 10x improvement)
- [ ] SSA optimization passes (target: matching Go's self-compile speed)

---

## Key Files

| File | Purpose |
|------|---------|
| `self/main.cot` | Selfcot entry point + multi-file pipeline |
| `self/frontend/checker.cot` | Type checker (5,947 lines, largest file) |
| `self/frontend/lower.cot` | IR lowering (9,177 lines, most complex) |
| `compiler/frontend/lower.zig` | Zig compiler's lowerer (where codegen fixes go) |
| `compiler/frontend/ir.zig` | IR data structures (shared SRET local) |
| `compiler/frontend/ssa_builder.zig` | SSA builder (stack slot allocation) |
