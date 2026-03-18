# Phase 3-5 Selfcot Regression — RESOLVED

**Date:** 2026-03-19
**Status:** Fixed
**Context:** TypeLowering Phase 3-5 changes (commit 87c6179) regressed selfcot from 8/13 to 0/13 files

## Root Cause

**`?*T` (optional managed pointer) had a representation mismatch between struct layout and SSA.**

- `sizeOf(?*T)` returns 16 bytes (8-byte tag + 8-byte payload)
- The SSA builder treated `?*T` as a single 8-byte scalar (pointer-like, null=0)
- Loading a `?*T` from a struct field or local returned only the tag (0 or 1)
- The tag was then used as a pointer → SIGSEGV

## Fix: Go SSA Decomposition Pattern

Ported Go's multi-word type decomposition architecture (used for interfaces, slices, strings):

### New SSA ops (Go `IMake`/`ITab`/`IData` pattern)
- `opt_make(tag, payload)` — construct compound optional
- `opt_tag(value)` — extract tag component
- `opt_data(value)` — extract payload component

### Files modified
1. **`compiler/ssa/op.zig`** — Added `opt_make`, `opt_tag`, `opt_data` ops
2. **`compiler/frontend/ssa_builder.zig`** — Compound `?*T` handling in all 8 contexts: param init, load local, store local, load field (local+value), store field, ptr load, ptr store
3. **`compiler/frontend/lower.zig`** — `isPtrLikeOptional` returns false for managed pointers; call arg decomposition for `?*T` params; inline `?*T` field init in `new_expr`
4. **`compiler/ssa/passes/decompose.zig`** — Phi decomposition for `?*T` phis
5. **`compiler/ssa/passes/rewritedec.zig`** — Peephole: `opt_tag(opt_make(t,p))→t`, `opt_data(opt_make(t,p))→p`
6. **`compiler/codegen/native/ssa_to_clif.zig`** — CLIF emission using `compound_extra_map`
7. **`compiler/codegen/wasm/gen.zig`** — Wasm emission
8. **`compiler/driver.zig`** — Threading `type_reg` to decompose, Wasm signature updates

### Test results
- 83/83 test files pass (native + wasm)
- 0/13 selfcot files crash (was 13/13 crashing)
- All 13 reach selfcot checker errors (pre-existing type-check limitations)

## Architectural Lesson

The SSA representation and memory layout MUST agree. When `sizeOf` returns 16 bytes for a type, the SSA must use compound decomposition — not a single-word scalar. Go enforces this with mandatory decomposition passes for all multi-word types. Cot now follows this pattern for `?*T managed`.
