# ARC Parity: Cot vs Swift — Status & Remaining Work

**Updated:** 2026-03-19
**Goal:** Full ARC parity with Swift SILGen for production-ready memory management.
**Current state:** ~98% parity. All gaps resolved. Zero ad-hoc dispatch remaining.

---

## Architecture Summary

### Swift's Three Layers
1. **TypeLowering** (TypeLowering.cpp) — per-type copy/destroy dispatch. Each type category has a lowering class.
2. **ManagedValue** (ManagedValue.cpp) — per-value ownership (+1 owned / +0 borrowed) with cleanup handle.
3. **CleanupStack** (Cleanup.cpp) — scope-based LIFO destruction.

### Cot's Current Implementation
1. **Centralized dispatch:** `emitCopyValue`/`emitDestroyValue` in `lower.zig` — recursive type walk for all categories (pointer, optional, struct, error union).
2. **ManagedValue:** `arc_insertion.zig` defines ManagedValue + CleanupStack. Used via `lowerExprManaged`/`managedFromLowered` at all assignment sites.
3. **SSA decomposition:** `opt_make`/`opt_tag`/`opt_data` ops for `?*T` (Go IMake/ITab/IData pattern). Parallel to existing string/slice decomposition.
4. **Alloc separation:** `alloc()` for ARC objects (header + refcount), `alloc_raw()` for backing buffers (no header).

---

## Completed Work

### Done: alloc/alloc_raw Separation
- `alloc(metadata, size)` → ARC header (magic, refcount=1) for `new T` objects
- `alloc_raw(size)` → raw malloc for List/Map backing buffers
- Prevents retain/release from corrupting non-ARC memory

### Done: isTrivial for Managed Pointers
- `isTrivial(.pointer)` returns `!p.managed` — managed pointers are non-trivial
- Unified `needsARC` = `!isTrivial`

### Done: Optional ARC Registration
- Optionals with managed pointer elem are registered for ARC at init and assignment
- `?*T` uses compound layout with `opt_make`/`opt_tag`/`opt_data` SSA ops
- `isPtrLikeOptional` returns false for managed pointer optionals

### Done: ManagedValue Integration
- `lowerExprManaged` returns ManagedValue with ownership (+1/+0) and cleanup handle
- `managedFromLowered` wraps pre-lowered values with correct ownership
- All 6 assignment paths use `managedFromLowered` — zero double evaluation
- `forward()` transfers ownership on return, disabling cleanup

### Done: Auto-Synthesized Destructors
- `emitPendingAutoDeinits` generates `_deinit` for structs with ARC fields
- Per-field release: `.pointer.managed` → release, `?*T` → unwrap-then-release
- `emitFieldReleases` handles user-defined deinit functions

### Done: Parameter Ownership Convention
- All params are +0 (borrowed). No cleanup entry.
- Storing a param value into a field always retains (via ManagedValue +0 detection).

### Done: ?*T SSA Decomposition (Go Pattern)
- `opt_make(tag, payload)` — construct compound optional in SSA
- `opt_tag(value)` — extract tag component
- `opt_data(value)` — extract payload component
- Full pipeline: SSA builder, decompose pass, rewritedec peepholes, CLIF + Wasm codegen
- Call arg decomposition handles `*T → ?*T` wrapping (tag=1, payload=ptr)

---

## All Gaps Resolved

### Gap A: Struct Init → emitCopyValue ✅
All struct init paths (`lowerStructInit`, `lowerStructInitExpr`, `lowerNewExpr`, default values) now use `!isTrivial` + `emitCopyValue` instead of inline `.pointer.managed` checks. Handles `?*T`, nested structs, error unions automatically.

### Gap B: Deinit → emitDestroyValue ✅
`emitFieldReleases` and `emitPendingAutoDeinits` now use centralized `emitDestroyValue` with `!isTrivial` guard. Previously blocked by `?*T` SSA single-word bug — resolved by `opt_make`/`opt_tag`/`opt_data` decomposition.

### Gap C: Ad-Hoc Dispatch Eliminated ✅
All inline `retain`/`release` calls outside centralized functions replaced:
- `lowerAssign` local managed pointer → `emitCopyValue` + `emitDestroyValue`
- `lowerAssign` `?*T` local → `emitCopyValue` + `emitDestroyValue`
- `lowerLocalVarDecl` +0 init → `emitCopyValue`
- `lowerIndexAssign` element → `emitCopyValue` + `emitDestroyValue`
- `lowerReturn` +0 values → `emitCopyValue`
- `emitCleanupsImpl` release → `emitDestroyValue`

### List/Map/Future Handlers ✅
Added `retain`/`release` cases for `.list`, `.map`, `.future` in `emitCopyValue`/`emitDestroyValue`.

### Remaining inline retain/release (intentional)
Only inside centralized dispatch functions themselves, `emitOptionalFieldRetain` sub-dispatcher, `lowerNewExpr` `?*T` source-local extraction, and `@arc_retain`/`@arc_release` builtins.

---

## Swift Reference Files

| File | Lines | Purpose |
|------|-------|---------|
| `references/swift/lib/SIL/IR/TypeLowering.cpp` | 5,638 | emitCopyValue, emitDestroyValue, struct/enum lowering |
| `references/swift/lib/SILGen/ManagedValue.cpp` | 329 | copy(), forward(), ensurePlusOne() |
| `references/swift/lib/SILGen/Cleanup.cpp` | 560 | CleanupManager, scope-based destruction |
| `references/swift/lib/SILGen/SILGenLValue.cpp` | 5,993 | emitAssignToLValue, emitLoad, emitSemanticStore |

## Cot Implementation Files

| File | Purpose |
|------|---------|
| `compiler/frontend/lower.zig` | ARC insertion during lowering (emitCopyValue, emitDestroyValue, field assign, deinit) |
| `compiler/frontend/arc_insertion.zig` | CleanupStack, ManagedValue data structures |
| `compiler/frontend/ssa_builder.zig` | opt_make/opt_tag/opt_data compound decomposition |
| `compiler/frontend/types.zig` | Type classification (isTrivial, couldBeARC) |
| `compiler/codegen/native/arc_native.zig` | Native ARC runtime (alloc/retain/release/alloc_raw) |
| `compiler/codegen/wasm/arc.zig` | Wasm ARC runtime |
| `compiler/ssa/op.zig` | SSA op definitions (opt_make, opt_tag, opt_data) |
| `compiler/ssa/passes/decompose.zig` | Phi decomposition for compound types |
| `compiler/ssa/passes/rewritedec.zig` | Peephole rewrites for opt_tag/opt_data |

## Archived Documents

- `claude/archive/ARC_ARCHITECTURE_AUDIT.md` — 8-gap analysis (all resolved)
- `claude/archive/ARC_ALLOC_SEPARATION.md` — alloc/alloc_raw separation plan (completed)
- `claude/archive/PHASE3_REGRESSION.md` — ?*T SSA mismatch regression (fixed via opt_make/opt_tag/opt_data)
