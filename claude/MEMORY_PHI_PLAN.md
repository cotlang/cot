# Memory PHI Implementation Plan

**Date:** 2026-03-26
**Status:** PLAN — ready to implement
**Blocks:** Runtime SIGSEGV in List operations after memory threading (store/load reordering at block boundaries)
**Reference:** Go `cmd/compile/internal/ssagen/ssa.go` — memory as variable, not scalar

## Problem

After implementing Go-faithful explicit memory threading (stores/loads/calls take and produce memory args), the schedule pass correctly handles intra-block ordering. But at **block boundaries** (if-else join points, loop headers), `current_mem` carries stale state from the previously-processed block rather than the correct state from the CFG predecessor.

Example:
```
b1 (entry):
  store A → current_mem = storeA
  branch → b2, b3

b2: store B → current_mem = storeB
b3: store C → current_mem = storeC

b4 (join from b2 and b3):
  // BUG: current_mem = storeC (from b3, last processed)
  // SHOULD BE: phi(storeB, storeC)
  load X (uses storeC as memory arg — wrong if we came from b2!)
```

## Go's Solution (the proven approach)

Go treats memory as a **regular variable** stored in the same `vars` map as all other SSA variables. This is the complete mechanism:

### Go Reference (ssagen/ssa.go)

1. **`memVar`** (line 1163): A marker node used as the variable key for memory state:
   ```go
   memVar = ssaMarker("mem")
   ```

2. **Entry block** (lines 395, 438-439): Seeds `vars[memVar]` with `InitMem`:
   ```go
   s.startmem = s.entryNewValue0(ssa.OpInitMem, types.TypeMem)
   s.startBlock(s.f.Entry)
   s.vars[memVar] = s.startmem
   ```

3. **`startBlock`** (lines 1176-1186): Creates a **fresh empty** `vars` map. Memory is NOT carried forward:
   ```go
   func (s *state) startBlock(b *ssa.Block) {
       s.curBlock = b
       s.vars = map[ir.Node]*ssa.Value{}  // empty — no memory state!
   }
   ```

4. **`endBlock`** (lines 1191-1217): Saves ALL vars (including memory) to `defvars[block.ID]`:
   ```go
   s.defvars[b.ID] = s.vars  // includes memVar → latest memory value
   s.curBlock = nil
   s.vars = nil
   ```

5. **`s.mem()`** (line 6609): Gets current memory via `variable()`:
   ```go
   func (s *state) mem() *ssa.Value { return s.variable(memVar, types.TypeMem) }
   ```

6. **`variable()`** (lines 6585-6607): If not in current `vars`, creates `FwdRef`:
   ```go
   func (s *state) variable(n ir.Node, t *types.Type) *ssa.Value {
       if v := s.vars[n]; v != nil { return v }
       if v := s.fwdVars[n]; v != nil { return v }
       v = s.newValue0A(ssa.OpFwdRef, t, fwdRefAux{N: n})
       s.fwdVars[n] = v
       return v
   }
   ```

7. **`store()`** (line 1591): Updates memory variable:
   ```go
   func (s *state) store(t *types.Type, dst, val *ssa.Value) {
       s.vars[memVar] = s.newValue3A(OpStore, types.TypeMem, t, dst, val, s.mem())
   }
   ```

8. **`insertPhis()`** (phi.go): Resolves ALL `FwdRef` ops — memory FwdRefs are handled by the exact same algorithm as data variable FwdRefs. If predecessors disagree on the memory value, a Phi node is created.

### Why This Works

- Each block starts fresh (no carried-over memory state)
- First memory use in a block creates a `FwdRef` → resolved by `insertPhis`
- At join points, `insertPhis` creates `phi(mem_from_b2, mem_from_b3)` automatically
- At single-predecessor blocks, `insertPhis` resolves `FwdRef` → `Copy` of predecessor's memory
- No special cases, no "barrier" logic, no block-boundary hacks

## Current Cot Architecture

Our SSA builder already has the infrastructure Go uses:
- `vars: AutoHashMap(LocalIdx, *Value)` — variable map (same as Go's `s.vars`)
- `defvars: AutoHashMap(u32, AutoHashMap(LocalIdx, *Value))` — saved vars per block (same as Go's `s.defvars`)
- `fwd_vars: AutoHashMap(LocalIdx, *Value)` — forward references (same as Go's `s.fwdVars`)
- `startBlock()` — saves defvars, creates fresh vars (same as Go)
- `insertPhis()` — resolves FwdRef ops into Phi/Copy (same as Go)
- `variable()` — looks up in vars/fwd_vars, creates FwdRef if missing (same as Go)

The ONLY thing missing: `current_mem` is a **separate scalar field** instead of being a variable in `vars`.

## Implementation Plan

### Step 1: Define MEM_VAR constant

Add a sentinel `LocalIdx` value that represents the memory variable, analogous to Go's `memVar`:

```zig
// In ssa_builder.zig, near the struct definition:
/// Go: memVar = ssaMarker("mem") — sentinel local index for memory state variable.
/// Uses a value that can never collide with real IR local indices.
const MEM_VAR: ir.LocalIdx = std.math.maxInt(ir.LocalIdx);
```

### Step 2: Replace current_mem with vars-based memory

Remove the `current_mem: ?*Value` field. Instead, use:
```zig
// Get current memory state (creates FwdRef at block boundaries):
fn mem(self: *SSABuilder) !*Value {
    return self.variable(MEM_VAR, TypeRegistry.SSA_MEM);
}

// Set current memory state:
fn setMem(self: *SSABuilder, v: *Value) void {
    self.assign(MEM_VAR, v);
}
```

### Step 3: Update init()

In `init()`, after creating `init_mem`, store it as a variable:
```zig
// OLD: current_mem = init_mem;
// NEW:
try vars.put(MEM_VAR, init_mem);
```

Remove `current_mem` from the returned struct.

### Step 4: Update emitMemStore

```zig
fn emitMemStore(self: *SSABuilder, store_op: Op, addr: *Value, value: *Value, cur: *Block) !*Value {
    const store = try self.func.newValue(store_op, TypeRegistry.SSA_MEM, cur, self.cur_pos);
    store.addArg2(addr, value);
    const cur_mem = try self.mem();
    try store.addArgAlloc(cur_mem, self.allocator);
    try cur.addValue(self.allocator, store);
    self.setMem(store);
    return store;
}
```

### Step 5: Update emitMemLoad

```zig
fn emitMemLoad(self: *SSABuilder, load_op: Op, addr: *Value, type_idx: TypeIndex, cur: *Block) !*Value {
    const load = try self.func.newValue(load_op, type_idx, cur, self.cur_pos);
    load.addArg(addr);
    const cur_mem = try self.mem();
    try load.addArgAlloc(cur_mem, self.allocator);
    try cur.addValue(self.allocator, load);
    return load;
}
```

### Step 6: Update all call sites

Every place that does `if (self.current_mem) |mem| try v.addArgAlloc(mem, ...)` becomes:
```zig
const cur_mem = try self.mem();
try v.addArgAlloc(cur_mem, self.allocator);
```

And every `self.current_mem = v;` becomes:
```zig
self.setMem(v);
```

### Step 7: Update init() parameter stores

In `init()`, replace direct `current_mem` manipulation with `vars.put(MEM_VAR, ...)`:
```zig
// Instead of: current_mem = store;
// Do: try vars.put(MEM_VAR, store);  (local vars map, not self.vars)
```

### Step 8: Remove build() init_mem creation

Already done — init_mem is created in init(). Verify the `current_mem` field is fully removed.

### Step 9: Verify insertPhis handles SSA_MEM type

Check that the phi insertion pass doesn't filter out SSA_MEM-typed FwdRefs. It should process ALL FwdRefs uniformly (matching Go).

### Step 10: Verify schedule pass handles memory PHIs

The schedule pass already handles `.phi` ops (score = ScorePhi, skipped in nextMem). Memory PHIs will get ScorePhi score, ensuring they're scheduled first in the block.

## Files to Modify

1. **`compiler/frontend/ssa_builder.zig`** — Main changes:
   - Add `MEM_VAR` constant
   - Add `mem()` and `setMem()` methods
   - Remove `current_mem` field
   - Update `emitMemStore`, `emitMemLoad`
   - Update all call sites (convertCall, convertCallIndirect, convertClosureCall, memcpy call)
   - Update all move sites (5 locations)
   - Update `init()` to use `vars.put(MEM_VAR, ...)` instead of `current_mem`

2. **`compiler/frontend/ssa_builder.zig` `variable()` function** — Verify it handles MEM_VAR correctly (it should — LocalIdx is just a u16/u32).

3. **No changes needed to:**
   - `insertPhis()` — already handles all FwdRefs uniformly
   - `schedule.zig` — already handles phi ops
   - `ssa_to_clif.zig` — already skips init_mem and SSA_MEM values
   - `deadcode.zig` — already keeps used values alive

## Verification

After implementation:
```bash
# Must all pass:
zig build                                              # Compiler builds
zig build test                                         # Unit tests pass
./zig-out/bin/cot test test/e2e/features.cot           # Target: 368+/371
./zig-out/bin/cot test /tmp/test_inl4.cot              # @inlinable Map works
./zig-out/bin/cot test /tmp/test_bigstruct6.cot        # String struct in List works
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot  # selfcot builds
```

## Pipeline Debugging

Use these to verify correctness at each stage:
```bash
COT_SSA='funcName' ./zig-out/bin/cot build file.cot    # Inspect SSA — verify FwdRef → Phi conversion
COT_DEBUG=ssa ./zig-out/bin/cot build file.cot          # Verify memory variable tracking per block
COT_DEBUG=schedule ./zig-out/bin/cot build file.cot     # Verify no ScheduleIncomplete
```

## Risks

- **Low:** The `variable()` function might not handle `MEM_VAR` (maxInt) correctly if there are bounds checks. Verify.
- **Low:** The phi insertion might create excessive memory PHIs in simple functions. Go has this too — not a performance concern for correctness.
- **Medium:** The `init()` function uses local `vars` map before the SSABuilder is constructed. Need to ensure `MEM_VAR` is put in the local map (not `self.vars`).
