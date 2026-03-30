# Specification: Explicit Memory Threading for Cot's SSA

**Date:** 2026-03-26
**Status:** SPEC — not yet implemented
**Blocks:** @inlinable monomorphization (schedule pass can't order memory ops)
**Reference:** Go `cmd/compile/internal/ssa/` — explicit memory state threading

## Problem

Cot's SSA has no explicit memory ordering. The schedule pass uses a linear chain
approximation that treats ALL stores as barriers for ALL loads. When @inlinable
monomorphized functions have multiple independent local stores (e.g., string
decomposition stores + hash computation loads), the linear chain creates false
dependencies, causing cycles in the topological sort (ScheduleIncomplete).

Go's SSA solves this with explicit memory threading: every memory operation takes
a `mem` input and produces a `mem` output. The schedule pass uses `nextMem[]` to
chain precisely — loads only order relative to stores they actually depend on.

## Go's Memory Threading Model

### Reference: `cmd/compile/internal/ssa/value.go`, `compile/internal/ssa/op.go`

In Go's SSA:
- There is a special **memory type** (`TypeMem`)
- Every function starts with an `InitMem` value (the initial memory state)
- **Stores** take `(addr, value, mem)` → produce `mem`
- **Loads** take `(addr, mem)` → produce `(value)`
- **Calls** take `(args..., mem)` → produce `(results..., mem)`
- Memory flows as a **chain** through the function
- Each block has ONE current memory state (the last memory-producing op)
- Phi nodes merge memory states at join points

### Example (Go SSA):
```
b0:
  v1 = InitMem
  v2 = LocalAddr {x}
  v3 = Store v2 42 v1        → mem (store x=42, after init)
  v4 = LocalAddr {y}
  v5 = Store v4 99 v3        → mem (store y=99, after x store)
  v6 = Load v2 v5            → i64 (load x, after y store)
```

The chain: `v1(InitMem) → v3(Store x) → v5(Store y) → v6(Load x)`

### Schedule uses nextMem (Go `schedule.go:257-278`):
```go
// Build nextMem: maps memory input to next memory output
for _, v := range b.Values {
    if v.Type.IsMemory() && v.Op != OpPhi && v.Op != OpInitMem {
        nextMem[v.MemoryArg().ID] = v
    }
}
// Add load→store edges: if a load's memory arg has a next store, order load first
for _, v := range b.Values {
    if !v.Type.IsMemory() && v.MemoryArg() != nil {
        if s := nextMem[v.MemoryArg().ID]; s != nil && s.Block == b {
            edges = append(edges, edge{v, s})
        }
    }
}
```

This is **precise**: load v6 only orders before stores that share its memory arg.
Independent locals (x, y) have separate store chains — loads from x don't block
on stores to y.

## Cot's Current Model

Cot's SSA has no memory type. Stores are `(addr, value)`. Loads are `(addr)`.
No memory arg, no memory output. The schedule pass uses a linear approximation:

```
last_mem = null
for v in block.values:
    if v.writesMemory():
        edge(last_mem, v)
        last_mem = v
    elif v.readsMemory():
        edge(last_mem, v)    ← ALL loads chain to last store
```

This creates false dependencies: a load from local X chains to a store to local Y
just because the store happened to be the most recent memory operation.

## Specification: Add Memory Threading

### 1. Add Memory Type

**File:** `compiler/ssa/value.zig`

Add `TypeRegistry.SSA_MEM` as the memory type (already exists as index 18).
This type represents "memory state" — it has no runtime representation, it's
purely for ordering.

### 2. Add `InitMem` Op

**File:** `compiler/ssa/op.zig`

Add `init_mem` op: produces the initial memory state at function entry.
- `arg_len = 0`
- `type = SSA_MEM`
- One per function, in the entry block

### 3. Modify Store Ops to Take and Produce Memory

**Current:** `store(addr, value)` → void
**New:** `store(addr, value, mem)` → mem

The store takes the current memory state as its last arg and produces a new
memory state. This creates the chain: each store depends on the previous one.

**Implementation:** Modify `op_info` for `store`, `store8`, `store16`, `store32`,
`store64`, `store_reg`:
```zig
table[@intFromEnum(Op.store)] = .{
    .name = "Store",
    .arg_len = 3,       // was 2: now (addr, value, mem)
    .writes_memory = true,
    .has_side_effects = true,
    .type = .SSA_MEM,   // NEW: produces memory state
};
```

### 4. Modify Load Ops to Take Memory

**Current:** `load(addr)` → value
**New:** `load(addr, mem)` → value

Loads take the current memory state to order them relative to stores.
They do NOT produce memory — they only read.

**Implementation:**
```zig
table[@intFromEnum(Op.load)] = .{
    .name = "Load",
    .arg_len = 2,       // was 1: now (addr, mem)
    .reads_memory = true,
};
```

### 5. Modify Call Ops to Take and Produce Memory

**Current:** `static_call(args...)` → result
**New:** `static_call(args..., mem)` → (result, mem)

Calls are memory barriers — they may read and write any memory.

For calls with tuple returns (result + mem), use `select0`/`select1` to
extract the result value and the new memory state:
```
v10 = static_call "foo" arg1 arg2 v_mem  → (i64, mem)
v11 = select0 v10                         → i64 (the result)
v12 = select1 v10                         → mem (the new memory state)
```

### 6. SSA Builder: Thread Memory Through Blocks

**File:** `compiler/frontend/ssa_builder.zig`

Track `current_mem: *Value` — the most recent memory state in the current block.

**Function entry:**
```zig
const init_mem = try func.newValue(.init_mem, TypeRegistry.SSA_MEM, entry, .{});
try entry.addValue(allocator, init_mem);
self.current_mem = init_mem;
```

**convertStoreLocal (for STRING decomposition):**
```zig
// OLD: just emit store(addr, ptr_val)
// NEW: emit store(addr, ptr_val, current_mem) → new_mem
const store = try func.newValue(.store, TypeRegistry.SSA_MEM, cur, self.cur_pos);
store.addArg(addr);
store.addArg(ptr_val);
store.addArg(self.current_mem);
try cur.addValue(self.allocator, store);
self.current_mem = store;
```

**convertLoadLocal:**
```zig
// OLD: just emit load(addr)
// NEW: emit load(addr, current_mem)
const load = try func.newValue(.load, type_idx, cur, self.cur_pos);
load.addArg(addr);
load.addArg(self.current_mem);
try cur.addValue(self.allocator, load);
```

**convertCall:**
```zig
// OLD: static_call(args...)
// NEW: static_call(args..., current_mem) → (result, mem)
call_val.addArg(self.current_mem);
// Extract memory state from call result
const new_mem = try func.newValue(.select1, TypeRegistry.SSA_MEM, cur, self.cur_pos);
new_mem.addArg(call_val);
try cur.addValue(self.allocator, new_mem);
self.current_mem = new_mem;
```

**Block transitions (branch/jump):**
Memory state flows through control flow. At join points (blocks with multiple
predecessors), a `phi` node merges the memory states:
```zig
// At block entry with multiple predecessors:
const mem_phi = try func.newValue(.phi, TypeRegistry.SSA_MEM, block, .{});
for (block.preds) |pred| {
    mem_phi.addArg(pred.current_mem);
}
self.current_mem = mem_phi;
```

### 7. Schedule Pass: Port Go's nextMem

**File:** `compiler/ssa/passes/schedule.zig`

Replace the linear chain with Go's `nextMem`:

```zig
fn scheduleBlock(allocator: Allocator, block: *Block, f: *Func) !void {
    const values = block.values.items;

    // ... (existing arg-based data dependency edges) ...

    // Go schedule.go:257-264: build nextMem chain
    var next_mem = try allocator.alloc(?*Value, f.vid.next_id);
    defer allocator.free(next_mem);
    @memset(next_mem, null);

    for (values) |v| {
        if (v.op == .phi or v.op == .init_mem) continue;
        if (v.type_idx == TypeRegistry.SSA_MEM) {
            // This value produces memory. Chain from its memory input.
            const mem_arg = v.memoryArg() orelse continue;
            if (mem_arg.block == block) {
                next_mem[mem_arg.id] = v;
            }
        }
    }

    // Go schedule.go:266-278: add load→store ordering edges
    for (values) |v| {
        if (v.op == .phi) continue;
        if (v.type_idx == TypeRegistry.SSA_MEM) continue; // skip stores
        const mem_arg = v.memoryArg() orelse continue;
        if (next_mem[mem_arg.id]) |store| {
            if (store.block == block) {
                try edges.append(allocator, .{ .x = v, .y = store });
            }
        }
    }

    // ... (existing topological sort) ...
}
```

### 8. Value.memoryArg() Helper

**File:** `compiler/ssa/value.zig`

Already exists (line 227). Verify it returns the last arg if it's memory-typed:
```zig
pub fn memoryArg(self: *const Value) ?*Value {
    if (!self.readsMemory() or self.args.len == 0) return null;
    const last = self.args[self.args.len - 1];
    if (last.type_idx == TypeRegistry.SSA_MEM) return last;
    return null;
}
```

### 9. SSA Passes: Handle Memory Type

**decompose.zig:** Decompose phi nodes on memory type (merge memory states).
Already handles strings/slices — add SSA_MEM.

**rewritedec.zig:** No change needed — memory values don't need rewriting.

**copyelim.zig:** Copy elimination may need to handle memory-typed copies.

**deadcode.zig:** Memory-typed values are live if they have uses (existing logic).
`InitMem` should be marked as live if any store/load exists.

**lower_native.zig / ssa_to_clif.zig:** Skip memory-typed values during codegen.
Memory threading is compile-time only — no runtime representation.

### 10. Backward Compatibility

The memory arg is the LAST arg of every memory operation. Existing passes that
iterate args (for data dependencies) will automatically create correct edges
from the memory arg to the value. This means the schedule pass gets BOTH:
1. Data dependency edges (from arg iteration)
2. Memory ordering edges (from nextMem)

No existing pass needs to change except schedule.zig (replace linear chain with
nextMem) and the SSA builder (thread memory).

## Implementation Order

1. **Add `init_mem` op** to op.zig — no behavioral change
2. **Modify SSA builder** to emit `init_mem` and thread `current_mem` through all
   stores, loads, and calls
3. **Update `memoryArg()`** to check last arg type
4. **Port Go's `nextMem`** to schedule.zig — replaces linear chain
5. **Update codegen** (ssa_to_clif.zig) to skip memory-typed values
6. **Test**: 370/370 features + @inlinable Map tests
7. **Verify**: no ScheduleIncomplete errors

## Verification

After implementation:
- `./zig-out/bin/cot test test/e2e/features.cot` → 370/370
- `./zig-out/bin/cot test /tmp/test_inl4.cot` → @inlinable Map works
- `./test/run_all.sh` → 82/82 (with @inlinable in stdlib/map.cot)
- `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` → selfcot builds
- `/tmp/selfcot test test/cases/arithmetic.cot` → selfcot can compile

## Risks

- **Medium:** Existing SSA passes may not handle the extra memory arg correctly.
  Mitigation: memory arg is always LAST, and existing arg iteration includes it.
- **Low:** Performance — memory threading adds one extra arg per memory op.
  This is the same cost Go pays and is negligible.
- **Low:** codegen changes — ssa_to_clif just needs to skip SSA_MEM values.

## Alternative (Not Recommended)

Instead of full memory threading, we could:
- Track which local each load/store accesses and only chain same-local ops
- This is more complex than Go's approach and doesn't generalize to heap ops

Go's explicit memory threading is the proven, general solution. Port it 1:1.
