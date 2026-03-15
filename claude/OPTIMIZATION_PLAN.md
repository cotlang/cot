# Cot Compiler Optimization Execution Plan

## Current State

| Metric | Zig `cot` | `selfcot` | Target |
|--------|-----------|-----------|--------|
| Self-compile time | 4.5s | 240s (53x) | <10s |
| Self-compile RSS | 187MB (wasm) / 910MB (native) | 7.2GB | <50MB |
| SSA passes | 6 | 6 | 20+ |
| Inlining | None | None | Budget-based |
| CSE | None | None | E-graph or hash-based |
| Dead code elim | None | None | Repeated |

**Root cause:** selfcot's binary is unoptimized (no inlining, no CSE, no DCE, no regalloc quality). This causes 53x slower execution and 38x more memory via excessive stack frames, struct copies, and heap fragmentation. Verified by memory profiling: SSA temporaries use only 98MB even when leaked; the 7.2GB comes from running unoptimized code that does 74GB of cumulative allocation for a 9K-line file.

## Reference Audit

### Go Compiler (57 SSA passes)
- **Inlining:** Budget-based, 80-unit threshold, PGO support for hot paths (2000 budget)
- **CSE:** Two phases (generic + lowered) — hash-based value numbering
- **Dead code:** Runs 8 times throughout pipeline, cheap O(n) pass
- **Prove:** Range analysis eliminates bounds checks and nil checks
- **Escape analysis:** Graph-based, decides stack vs heap allocation
- **Strength reduction:** `divisible` + `divmod` passes
- **Branch elimination:** Converts branches to conditional selects

### Cranelift (8-10 passes)
- **E-graph optimizer:** Equality saturation for CSE + algebraic simplification + constant propagation in one pass
- **Alias analysis:** Load forwarding, redundant load elimination
- **Simple legalization:** One-pass lowering of unsupported ops
- **Philosophy:** Fewer passes, each more powerful. E-graph does the work of Go's CSE + opt + phiopt + prove combined.

### Zig Compiler
- **Comptime evaluation:** Constant folding at language level
- **Semantic analysis:** Dead code via lazy evaluation (only analyzes reachable code)
- **LLVM backend:** Delegates heavy optimization to LLVM (-O2/-O3)
- **Self-hosted backend:** Basic block layout, register allocation, no inlining yet

### Rust (rustc + LLVM)
- **MIR optimizations:** Inlining, const prop, dead code, simplify branches
- **LLVM:** Full optimization suite (-O0 to -O3)
- **Cranelift backend:** Uses Cranelift for debug builds (faster compile, less optimization)

## Recommended Approach for Cot

**Philosophy: Go's pass-based pipeline, not Cranelift's e-graph.**

Rationale:
1. Go's passes are individually simple — each is 100-500 lines, easy to port
2. Go is our primary reference implementation — SSA structure already matches
3. E-graphs are complex to implement correctly (Cranelift's took years)
4. Go proves this approach works: compiles 2M+ lines in 15 seconds
5. Each pass gives measurable improvement — can ship incrementally

## Execution Plan (6 Phases)

### Phase 1: Dead Code Elimination (DCE)
**Go reference:** `ssa/deadcode.go` (~150 lines)
**Impact:** 10-15% compile time reduction, significant memory reduction
**Why first:** Removes garbage early, makes all subsequent passes faster

**Algorithm (Go pattern):**
1. Mark entry block as live
2. Walk successors of live blocks, mark reachable blocks
3. For each live block, mark values used by controls as live
4. Walk args of live values, mark those live
5. Remove dead values and unreachable blocks

**Implementation:**
- `compiler/ssa/passes/deadcode.zig` — Zig compiler
- `self/ssa/passes/deadcode.cot` — self-hosted compiler
- Insert BEFORE and AFTER `opt`, `lower`, and `schedule` passes (Go runs DCE 8 times)
- ~150 lines each

**Pass order after Phase 1:**
```
deadcode → rewritegeneric → deadcode → decompose → rewritedec →
deadcode → schedule → layout → lower → deadcode
```

### Phase 2: Copy Elimination + Phi Elimination
**Go reference:** `ssa/copyelim.go` (~50 lines), `ssa/phielim.go` (~30 lines)
**Impact:** 5-10% — removes redundant copies from SSA construction

**Copy elimination algorithm:**
- Walk all values. If `v.Op == Copy`, replace all uses of `v` with `v.Args[0]`
- Transitive: follow copy chains to find the original

**Phi elimination algorithm:**
- If all args to a phi are the same value (or the phi itself), replace phi with that value
- Common after inlining and other optimizations

**Implementation:**
- `compiler/ssa/passes/copyelim.zig` (~50 lines)
- `compiler/ssa/passes/phielim.zig` (~30 lines)
- `self/ssa/passes/copyelim.cot`
- `self/ssa/passes/phielim.cot`
- Insert after `rewritegeneric` and after `lower`

### Phase 3: Common Subexpression Elimination (CSE)
**Go reference:** `ssa/cse.go` (~100 lines)
**Impact:** 15-25% — eliminates redundant computations

**Algorithm (Go pattern — hash-based value numbering):**
1. For each block in dominator order:
2. For each value in block:
3. Compute hash: `hash(op, type, auxint, aux, args...)`
4. Look up hash in table — if match with same op/type/args, replace with existing
5. Otherwise insert into hash table

**Key insight:** Two values are equivalent if they have the same op, type, aux data, and their args are equivalent. This is checked AFTER dead code elimination.

**Implementation:**
- `compiler/ssa/passes/cse.zig` (~100 lines)
- `self/ssa/passes/cse.cot`
- Insert after first `deadcode`, before `schedule`
- Requires dominator tree (simple algorithm: ~50 lines)

### Phase 4: Inlining
**Go reference:** `inline/inl.go` (~800 lines for CanInline + InlineCalls)
**Impact:** 30-50% for self-compilation — THE critical optimization

**Why inlining matters most for selfcot:**
- `List.get()` / `List.set()` / `Map.get()` / `Map.set()` called millions of times
- Each call: ~20 cycles overhead (stack frame, arg passing, return)
- Inlined: 2-3 cycles (direct memory access)
- Self-compilation does ~10M+ generic method calls — inlining saves ~200M cycles

**Algorithm (Go budget-based):**
1. **CanInline pass:** Walk AST, compute cost for each function body
   - Each node costs 1 unit
   - Function calls cost 57 units (Go's `inlineExtraCallCost`)
   - If total cost ≤ 80 (`inlineMaxBudget`), mark as inlineable
2. **InlineCalls pass:** Walk call sites
   - If callee is inlineable AND caller isn't too big (5000 nodes): inline
   - Substitute params with args, rename locals, insert body at call site

**Cot-specific considerations:**
- `@safe` auto-ref means struct params are already pointers — inlining just removes the call overhead
- Generic monomorphization already creates separate function bodies per type — these are prime inline candidates
- `List(T).get/set/append` are the #1 targets — ~20 instructions each, called everywhere

**Implementation (2 parts):**
1. **IR-level inlining** (before SSA): Simpler, matches Go's approach
   - `compiler/frontend/inline.zig` — CanInline + InlineCalls on IR
   - `self/frontend/inline.cot` — same for self-hosted
   - Insert after lowering to IR, before SSA building
2. **SSA-level inlining** (alternative): More precise but harder
   - Would require SSA function merging
   - Defer to Phase 6

**Priority targets for inlining:**
| Function | Call count (est.) | Cost | Inline? |
|----------|-------------------|------|---------|
| `List(T).get` | 5M+ | ~15 | YES |
| `List(T).set` | 2M+ | ~20 | YES |
| `List(T).append` | 1M+ | ~30 | YES |
| `Map(K,V).get` | 500K+ | ~40 | YES |
| `Map(K,V).contains` | 500K+ | ~30 | YES |
| `SsaFunc.getValue` | 500K+ | ~5 | YES |
| `SsaFunc.setValue` | 300K+ | ~5 | YES |
| `SsaValue.getArg` | 1M+ | ~10 | YES |
| `SsaValue.addArg` | 500K+ | ~15 | YES |

### Phase 5: Constant Folding + Strength Reduction
**Go reference:** `ssa/rewrite.go` (generated rules), `ssa/prove.go` (~1000 lines)
**Impact:** 5-10%

**Constant folding:**
- `add(const(3), const(5))` → `const(8)`
- `mul(x, const(1))` → `x`
- `mul(x, const(0))` → `const(0)`
- `add(x, const(0))` → `x`
- Applied during the `opt` pass via rewrite rules

**Strength reduction (Go `divisible` pass):**
- `div(x, const(8))` → `shr(x, 3)`
- `mod(x, const(power_of_2))` → `and(x, power_of_2 - 1)`
- `mul(x, const(2))` → `shl(x, 1)`

**Implementation:**
- Add rewrite rules to `lower_wasm.zig` / `lower_native.zig`
- Or create new `opt.zig` pass that runs between DCE and CSE
- ~200 lines of pattern-matching rules

### Phase 6: Advanced Optimizations (Post-1.0)
These become important once the compiler is fast enough to iterate quickly:

**6a. Dead Store Elimination (DSE)**
- Go: `ssa/dse.go` — removes stores that are overwritten before being read
- Important for struct field assignment patterns

**6b. Bounds Check Elimination (BCE)**
- Go: `ssa/prove.go` — uses range analysis to prove array indices are in-bounds
- Critical for List-heavy code (every `.get()` does a bounds check)

**6c. Nil Check Elimination**
- Go: `ssa/nilcheckelim.go` — removes redundant nil checks
- Important for optional-heavy code

**6d. Branch Elimination**
- Go: `ssa/branchelim.go` — converts simple branches to conditional selects
- ARM64: `csel` instruction, Wasm: `select` instruction

**6e. Loop Invariant Code Motion (LICM)**
- Move computations that don't change inside loops to before the loop
- `while (i < list.count)` — hoist `list.count` load

**6f. Escape Analysis**
- Go-style: determine which values can be stack-allocated vs heap-allocated
- Would eliminate many `alloc`/`dealloc` calls for temporary structs

## Implementation Order & Expected Impact

| Phase | Pass | Lines | Time Impact | Memory Impact |
|-------|------|-------|-------------|---------------|
| 1 | DCE | 150 | -15% | -20% |
| 2 | CopyElim + PhiElim | 80 | -10% | -5% |
| 3 | CSE | 150 | -20% | -15% |
| 4 | Inlining | 800 | -50% | -60% |
| 5 | ConstFold + StrengthRed | 200 | -10% | -5% |
| **Total** | | **~1380** | **~70% faster** | **~70% less memory** |

**Expected results after all 5 phases:**
- Self-compile time: 240s → ~70s (still no register alloc improvements)
- Self-compile RSS: 7.2GB → ~2GB (still no escape analysis)
- With register alloc improvements: ~30s, ~500MB
- With escape analysis: ~15s, ~200MB

## Pass Pipeline After All Phases

```
[IR-level inlining]
  ↓
SSA Building
  ↓
earlyDeadcode → earlyPhiElim → earlyCopyElim
  ↓
rewritegeneric
  ↓
deadcode → opt (constant fold + strength reduction) → deadcode
  ↓
cse → deadcode
  ↓
decompose → rewritedec → deadcode
  ↓
schedule → layout
  ↓
lower_{wasm,native} → deadcode → loweredCse → deadcode
  ↓
[Codegen]
```

## Files to Create/Modify

| File | Purpose | Lines |
|------|---------|-------|
| `compiler/ssa/passes/deadcode.zig` | Dead code elimination | ~150 |
| `compiler/ssa/passes/copyelim.zig` | Copy elimination | ~50 |
| `compiler/ssa/passes/phielim.zig` | Phi elimination | ~30 |
| `compiler/ssa/passes/cse.zig` | Common subexpression elimination | ~100 |
| `compiler/ssa/passes/opt.zig` | Constant folding + strength reduction | ~200 |
| `compiler/frontend/inline.zig` | IR-level inlining | ~800 |
| `compiler/driver.zig` | Wire new passes into pipeline | ~50 |
| `self/ssa/passes/deadcode.cot` | Self-hosted DCE | ~150 |
| `self/ssa/passes/copyelim.cot` | Self-hosted copy elim | ~50 |
| `self/ssa/passes/phielim.cot` | Self-hosted phi elim | ~30 |
| `self/ssa/passes/cse.cot` | Self-hosted CSE | ~100 |
| `self/frontend/inline.cot` | Self-hosted inlining | ~800 |
| `self/codegen/wasm/driver.cot` | Wire new passes | ~50 |

## Verification Strategy

After each phase:
1. `zig build test` — compiler internals
2. `cot test test/e2e/features.cot` — all language features (native)
3. `cot test test/e2e/features.cot --target=wasm32` — all features (wasm)
4. `cot build self/main.cot -o /tmp/selfcot` — self-compilation succeeds
5. `/tmp/selfcot check self/main.cot` — selfcot can type-check itself
6. `/usr/bin/time -l` on above — measure time + RSS improvement

## Key Principle

**Port Go's passes line-by-line. Do not invent.** Each Go pass is battle-tested across millions of compilations. The SSA structure already matches. Copy the algorithm, adapt the data structures, verify with tests.
