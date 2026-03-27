# Memory Threading Implementation Status

**Date:** 2026-03-26
**Status:** COMPLETE — Go-faithful memory threading working. One new bug to investigate.

## What Was Implemented

### Go-faithful explicit memory state threading (3 commits)

Ported Go's SSA memory model (`cmd/compile/internal/ssa/schedule.go`, `ssagen/ssa.go`, `phi.go`) to Cot's SSA builder, schedule pass, and codegen.

#### 1. Memory args on all memory operations
- **Stores** (`emitMemStore`): take `(addr, value, mem)`, produce `SSA_MEM` type
- **Loads** (`emitMemLoad`): take `(addr, mem)`, read memory but don't produce new state
- **Calls** (static_call, closure_call, inter_call): take `mem` as last arg, update memory state
- **Moves** (bulk copy): take `(dst, src, mem)`, produce `SSA_MEM` type
- **Parameter stores** in `init()`: threaded through `vars.put(MEM_VAR, store)`

#### 2. Memory as variable (Go's `memVar` pattern)
- `MEM_VAR = maxInt(u32)` — sentinel `LocalIdx` for memory state
- Memory tracked in `vars` map alongside all other SSA variables
- `mem()` method: calls `variable(MEM_VAR, SSA_MEM)` — creates FwdRef at block boundaries
- `setMem(v)` method: calls `assign(MEM_VAR, v)`
- `startBlock()` clears vars → memory creates FwdRef in new blocks
- `saveDefvars()` saves memory state per-block → `insertPhis()` creates memory PHIs automatically
- `saveDefvars()` called after IR block loop so last block's state is available

#### 3. Schedule pass — Go's nextMem
- Builds `nextMem[]` mapping memory input → next memory-producing value
- Uses `type_idx == SSA_MEM || writesMemory()` to identify memory producers (stores have SSA_MEM type, calls have writesMemory flag since Go uses tuples for calls but we don't)
- Adds load→next-store edges (Go schedule.go:266-278)
- No invented barrier/bridge/unthreaded logic — clean Go port
- Scoring: ScoreInitMem (2), ScoreMemory (5) type-based

#### 4. Codegen — skip memory args by position
- **CRITICAL RULE**: Skip memory arg by POSITION (last arg), NOT by value identity
- When a call's return value IS also the memory state (e.g., `memcpy(dest, elemPtr_result, size, elemPtr_result)`), identity-based skip removes the data arg too
- Position-based skip: `v.args[0 .. v.args.len - 1]` when `memoryArg() != null`
- Memory PHIs skipped in CLIF phi declaration and resolution
- Memory copies (from resolved FwdRefs) skipped in codegen
- `has_return` checks exclude SSA_MEM type

#### 5. lookupVarOutgoing — orphan block handling
- 0-predecessor blocks fall back to entry block's defvars
- Prevents unresolvable FwdRefs in orphan blocks

## Files Changed

| File | Changes |
|------|---------|
| `compiler/frontend/ssa_builder.zig` | MEM_VAR, mem()/setMem(), all stores/loads/calls/moves threaded, memory as variable, saveDefvars after loop |
| `compiler/ssa/value.zig` | `memoryArg()`: SSA_MEM type check + writesMemory fallback |
| `compiler/ssa/op.zig` | Calls: writes_memory+reads_memory. Store/move: SSA_MEM type, arg_len updated |
| `compiler/ssa/passes/schedule.zig` | Go's nextMem, type-based scoring, no barrier logic |
| `compiler/codegen/native/ssa_to_clif.zig` | Position-based memory arg skip, memory PHI/copy skip |
| `compiler/codegen/wasm/gen.zig` | Position-based memory arg skip for all call types |
| `compiler/ssa/passes/decompose.zig` | SSA_MEM excluded from slice type check |

## Test Results

```
features.cot:     368/371 (matches pre-threading baseline)
unit tests:       898/898
test/cases/*.cot: all pass
selfcot build:    not tested yet
```

The 2 pre-existing failures in features.cot are map iterator tests (`for key value in map`).
One new remaining bug: @inlinable Map_set runtime corruption (see below).

## REMAINING BUG: @inlinable Map_set runtime string corruption

### What works
- @inlinable Map_set **compiles** (ScheduleIncomplete is FIXED)
- The schedule pass produces valid output with no cycles
- All non-@inlinable tests pass

### What fails
```bash
./zig-out/bin/cot test /tmp/test_inl4.cot
# test "set and has" ... BUG: Map string hash: ptr=8612153840 len=6095171168
# panic: corrupt string in Map hash
```

The monomorphized `Map(string, int)_set` function produces garbage pointer/length values for the string key when computing the hash.

### Root cause hypothesis
The @inlinable monomorphization path (`lowerGenericFnInstance`) generates IR that decomposes string arguments into (ptr, len) stores to local variables. After memory threading + scheduling, these stores may be reordered relative to the loads that read them back for hash computation.

The specific issue: in the monomorphized body, there are ~24 blocks with stores to multiple independent locals (string ptr, string len, hash state, probe index, etc.). The memory chain threads through all of them, but the monomorphization may create stores/loads that reference locals from the ORIGINAL generic body's frame layout rather than the monomorphized copy's frame.

### Debugging approach
1. Use `COT_SSA='Map(17;5)_set'` to inspect the SSA at each pass stage
2. Look at the block containing string hash computation (block b5 with ~39 values)
3. Verify the string ptr/len stores at the start of the function use the correct local indices
4. Verify the loads for hash computation read from the same locals
5. Check if the schedule pass reorders stores PAST the loads in the hash computation block
6. Compare with a non-@inlinable version (regular Map_set via shared body) to see what's different

### Test file
```bash
cat > /tmp/test_inl4.cot << 'EOF'
import "std/map"
import "std/string"

test "set and has" {
    var m: Map(string, int) = .{}
    m.set("hello", 42)
    @assertEq(m.has("hello"), 1)
}
EOF
```

## Key Design Decisions

1. **Stores produce SSA_MEM, not VOID** — matches Go's `TypeMem`. Needed for type-based identification in schedule pass.

2. **Calls keep their return type** — Go uses tuples `(result, mem)` but Cot doesn't have tuple support. Instead, calls have `writes_memory = true` flag and the schedule pass checks both `type == SSA_MEM` and `writesMemory()`.

3. **memoryArg() checks both type AND flag** — pure type check works for stores/moves/init_mem (SSA_MEM type). writesMemory() fallback catches calls (which keep their return type).

4. **Position-based memory arg skip** — learned the hard way that identity-based skip breaks when a call result is both data and memory state. Always skip by position (last arg).

5. **Memory as variable, not scalar** — Go's `memVar` pattern enables automatic PHI insertion at block join points via existing `insertPhis()` infrastructure. No special memory-PHI code needed.
