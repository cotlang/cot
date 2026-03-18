# ARC Parity: Cot vs Swift — Status & Remaining Work

**Updated:** 2026-03-19
**Goal:** Full ARC parity with Swift SILGen for production-ready memory management.
**Current state:** ~70% parity. 3 gaps remain before full parity.

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

## Remaining Gaps (3)

### Gap A: Struct Init Paths Don't Use emitCopyValue (HIGH)

**Problem:** `emitCopyValue` correctly handles all type categories recursively, but two of three struct init paths don't call it. They use inline `.pointer.managed` checks that miss `?*T` fields.

| Init Path | File:Line | Uses emitCopyValue? | Handles ?*T? |
|-----------|-----------|--------------------:|:------------:|
| `lowerNewExpr` | lower.zig:6402 | No (inline) | Yes (custom) |
| `lowerStructInit` | lower.zig:2785 | No (inline) | **No** |
| `lowerStructInitExpr` | lower.zig:6079 | No (inline) | **No** |
| Default field values | lower.zig:2825, 6119 | No (inline) | **No** |

**Fix:** Replace inline `.pointer.managed` checks in `lowerStructInit`, `lowerStructInitExpr`, and default value paths with calls to `emitCopyValue`. This handles `?*T`, nested structs with managed fields, and error unions automatically.

**Complexity:** Small — the centralized function already exists and is correct.

**Swift reference:** `SILGenExpr.cpp` — struct literal init calls `emitCopyValue` for each non-trivial field.

### Gap B: emitFieldReleases/emitPendingAutoDeinits Missing Recursive Struct Destruction (MEDIUM)

**Problem:** `emitDestroyValue` correctly walks struct fields recursively in reverse order (LIFO). But `emitFieldReleases` and `emitPendingAutoDeinits` use inline per-field release that only handles `.pointer.managed` and `?*T`. Nested structs containing managed fields are not recursively destroyed.

| Destroy Path | File:Line | Uses emitDestroyValue? | Recursive? |
|-------------|-----------|:----------------------:|:----------:|
| `emitFieldReleases` | lower.zig:2066 | No (inline) | **No** |
| `emitPendingAutoDeinits` | lower.zig:2121 | No (inline) | **No** |
| Assignment old-value destroy | lower.zig:3324+ | **Yes** | Yes |

**Why not use emitDestroyValue directly:** The comment at lower.zig:2076 says "Can NOT use centralized emitDestroyValue here because the SSA builder treats ?*T as a single-word scalar." **This is now outdated** — the `opt_make`/`opt_tag`/`opt_data` SSA ops fix this. `emitDestroyValue` should now work correctly for `?*T` struct fields.

**Fix:** Replace inline per-field release in `emitFieldReleases` and `emitPendingAutoDeinits` with calls to `emitDestroyValue`. The centralized function handles all types recursively.

**Complexity:** Small — requires re-testing with selfcot after the change. The previous attempt failed because `?*T` SSA was broken; it should work now with the `opt_make` decomposition.

**Swift reference:** `LoadableStructTypeLowering::emitLoweredDestroyValue` — `forEachNonTrivialChild → emitDestroyValue`.

### Gap C: Ad-Hoc Type Dispatch (31+ Sites) (LOW — TECH DEBT)

**Problem:** 31+ locations in `lower.zig` manually check `.pointer.managed` or `.optional and couldBeARC` instead of using centralized `emitCopyValue`/`emitDestroyValue`. This creates inconsistency — some paths handle all type categories, others only handle managed pointers.

**Not blocking:** The centralized functions exist and are correct. The ad-hoc sites mostly work for the types selfcot uses. This is tech debt, not a correctness bug (except for Gaps A and B above).

**Fix:** Long-term refactor to replace ad-hoc checks with centralized dispatch. Lower priority than Gaps A and B.

**Swift reference:** `TypeExpansionKind::DirectChildren` — every operation dispatches through the type lowering hierarchy.

---

## Missing Type Handlers in emitCopyValue/emitDestroyValue

Both functions handle: pointer, optional, string, struct (recursive), error union.

Both functions are **missing** handlers for:
- `.list` — falls through to no-op (leak if List inside a struct is copied)
- `.map` — falls through to no-op (leak if Map inside a struct is copied)
- `.future` — falls through to no-op

**Impact:** Low for selfcot — List/Map/Future fields in structs are rare. The stdlib List/Map types themselves are stack-allocated and use `alloc_raw` for backing buffers. But any struct containing a `List(T)` field would leak if copied.

**Fix:** Add retain/release cases for `.list`, `.map`, `.future` in `emitCopyValue`/`emitDestroyValue`. These are heap-allocated objects with ARC headers, so the handler is the same as `.pointer.managed`: `retain(value)` / `release(value)`.

---

## Priority Order for Full Parity

| # | Gap | Effort | Impact | Unblocks |
|---|-----|--------|--------|----------|
| 1 | **Gap A:** Wire emitCopyValue into struct init | Small | Fixes ?*T field leaks in stack structs | Correctness |
| 2 | **Gap B:** Wire emitDestroyValue into deinit | Small | Fixes nested struct field leaks | Correctness |
| 3 | **List/Map/Future handlers** | Tiny | Fixes container field leaks | Edge cases |
| 4 | **Gap C:** Consolidate ad-hoc dispatch | Large | Tech debt, consistency | Maintainability |

After items 1-3, Cot reaches **~95% ARC parity** with Swift SILGen. Item 4 is polish.

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
