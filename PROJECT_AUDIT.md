# Project Audit - January 2026

## Executive Summary

The Cot compiler is at a crossroads. The Wasm backend is ~70% complete (M1-M9), but the native codegen code has been moved to `codegen/native/` and is now **dead code** - not compiled into any path, with tests skipped.

**Critical Decision Needed:** What to do with the native codegen code?

---

## Current State

### Test Status
- **376 passed** - Frontend, SSA, Wasm backend, native codegen unit tests
- **22 skipped** - Native codegen integration tests (pending Phase 4 wiring)

### Code Paths

```
WASM PATH (Working)                    NATIVE PATH (Dead Code)
═══════════════════                    ═══════════════════════
frontend/* ─┐
            │                          codegen/native/
            ▼                          ├── regalloc.zig      (NOT CALLED)
ssa/passes/schedule ──┐                ├── liveness.zig      (NOT CALLED)
ssa/passes/layout ────┼──► Wasm        ├── stackalloc.zig    (NOT CALLED)
ssa/passes/lower_wasm ┘    Backend     ├── abi.zig           (NOT CALLED)
            │                          ├── expand_calls.zig  (NOT CALLED)
            ▼                          ├── decompose.zig     (NOT CALLED)
codegen/wasm/* ──► .wasm               ├── elf.zig           (NOT CALLED)
                                       ├── macho.zig         (NOT CALLED)
                                       └── dwarf.zig         (NOT CALLED)
```

### Files in `codegen/native/` (3,644 lines of dead code)

| File | Lines | Status |
|------|-------|--------|
| regalloc.zig | 859 | Dead - tests skipped |
| liveness.zig | 947 | Dead - tests skipped |
| stackalloc.zig | 363 | Dead - tests skipped |
| abi.zig | 387 | Dead - tests skipped |
| expand_calls.zig | 256 | Dead - tests skipped |
| decompose.zig | 285 | Dead - tests skipped |
| elf.zig | 529 | Dead - tests skipped |
| macho.zig | 548 | Dead - tests skipped |
| dwarf.zig | 363 | Dead - tests skipped |
| **Total** | **3,644** | **0% exercised** |

---

## Documentation Inconsistencies

### README.md
- Says "Object Files: ✅ Done" - **misleading**, these are for AOT not Wasm
- Says "SSA Infrastructure: ✅ Done" - **misleading**, includes native-only files
- Repository structure shows `obj/` directory - **outdated**, files moved to `codegen/native/`

### CLAUDE.md
- Says "SSA: liveness, regalloc, stackalloc: ✅ Done" - **misleading**, these are native-only and untested
- Architecture diagram doesn't match current reality
- States "Native codegen (Round 5) was intentionally skipped" but code still exists

### VISION.md
- Roadmap Phase 4 "AOT Native Compiler" says ~1000 LOC needed - **accurate**
- But doesn't acknowledge existing code is dead

### AOT_STRATEGY.md
- Documents the plan well
- But says "Phase 2: Reorganize" status is incomplete - **outdated**, this was done

### WASM_BACKEND.md
- M1-M9 marked complete
- M10-M16 are the real remaining work
- Doesn't mention the failing wasm_gen test

---

## The Dead Code Problem

### Why It Matters

1. **Bit rot**: Code that isn't tested will break silently
2. **Maintenance burden**: Every change must consider code that may not work
3. **False confidence**: Documentation says "Done" but code may be broken
4. **Confusion**: Future sessions will trip over this

### How We Got Here

1. Native codegen was built for direct Cot → Native compilation
2. Project pivoted to Wasm-first architecture
3. Native code was moved aside "for AOT" but AOT wasn't implemented
4. Tests were skipped to make CI pass
5. Now we have 3,644 lines of orphaned code

---

## Options

### Option A: Delete Native Code (Recommended if timeline is > 6 months)

**Action:** Remove `codegen/native/` entirely. Re-port from `bootstrap-0.2` when AOT is actually needed.

**Pros:**
- No dead code
- No misleading documentation
- Clean codebase
- Can re-copy from bootstrap-0.2 when needed

**Cons:**
- Loses any improvements made during 0.3 refactor
- Re-work needed when AOT is implemented

### Option B: Implement Minimal AOT (Recommended if timeline is < 3 months)

**Action:** Wire up basic AOT so native code is exercised.

**Minimum work:**
1. `codegen/native/wasm_parser.zig` (~400 LOC) - parse Wasm binary
2. `codegen/native/wasm_to_ssa.zig` (~600 LOC) - convert to SSA
3. Copy `arm64.zig`, `amd64.zig` from bootstrap-0.2
4. Wire into driver.zig

**Pros:**
- Native code stays tested
- AOT becomes usable
- Validates the architecture

**Cons:**
- 1000+ LOC of new work
- May delay Wasm backend completion

### Option C: Keep Dead Code with Clear Marking (Not Recommended)

**Action:** Keep code but document it's not wired up.

**Pros:**
- Minimal effort

**Cons:**
- Code will rot
- Confusing for future sessions
- False sense of completeness

---

## Recommended Path Forward

### Immediate (This Session)

1. **Fix the failing wasm_gen test** - Wasm is the priority
2. **Update documentation** to reflect reality
3. **Decide on Option A vs B** based on timeline

### If Choosing Option A (Delete):

```bash
# Delete dead native code
rm -rf compiler/codegen/native/

# Remove imports from main.zig, driver.zig, compile.zig
# Update documentation
```

### If Choosing Option B (Implement AOT):

1. Create `wasm_parser.zig` (parse Wasm sections)
2. Create `wasm_to_ssa.zig` (stack → SSA conversion)
3. Copy arm64.zig, amd64.zig from bootstrap-0.2
4. Add `--target=native` path in driver.zig
5. Re-enable native tests

---

## Documentation Updates Needed

### README.md

Change:
```markdown
| Object Files | ✅ Done | ELF, Mach-O, DWARF debug info |
```

To:
```markdown
| Object Files | ⏸️ Paused | Moved to codegen/native/ for future AOT |
```

### CLAUDE.md

Change:
```markdown
| SSA | op, value, block, func, liveness, regalloc, stackalloc | ✅ Done |
```

To:
```markdown
| SSA | op, value, block, func | ✅ Done |
| Native Codegen | liveness, regalloc, stackalloc, abi, elf, macho, dwarf | ⏸️ AOT (not wired) |
```

### AOT_STRATEGY.md

Update Phase 2 status to ✅ DONE (files moved).

---

## Wasm Backend Remaining Work

The real priority. M10-M16 in WASM_BACKEND.md:

| Milestone | Description | Estimated LOC |
|-----------|-------------|---------------|
| M10 | Linear memory (load/store) | ~200 |
| M11 | Pointers | ~150 |
| M12 | Structs (field access) | ~300 |
| M13 | Arrays/Slices | ~300 |
| M14 | Strings (data section) | ~200 |
| M15 | ARC basics (retain/release) | ~400 |
| M16 | Browser imports | ~200 |
| **Total** | | **~1,750** |

This should be the focus. AOT can wait until Wasm backend is complete.

---

## Summary

**Current reality:** Cot has a working Wasm backend at 70% and 3,644 lines of dead native code.

**Recommendation:**
1. Fix the wasm_gen test
2. Delete `codegen/native/` (Option A)
3. Focus on M10-M16 to complete Wasm backend
4. Implement AOT later using bootstrap-0.2 as reference

The native code isn't wasted - it exists in bootstrap-0.2 and can be ported when needed. Keeping dead code in the repo creates more problems than it solves.
