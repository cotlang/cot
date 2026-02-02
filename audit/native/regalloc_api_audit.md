# Public API Module Audit

**Source**: `regalloc2/src/lib.rs` (run functions) + `ion/mod.rs`
**Target**: `compiler/codegen/native/regalloc/regalloc.zig`
**Status**: ✅ Complete (~400 LOC, 2 tests)

---

## Source File Analysis

### Key Functions

| Function | Lines | Description |
|----------|-------|-------------|
| run | ~20 | Main entry point with new Ctx |
| run_with_ctx | ~15 | Entry point with reusable Ctx |
| Ctx struct | ~100 | Reusable allocator context |

---

## Function-by-Function Audit

### 1. run (lib.rs lines 1656-1671)

**Rust:**
```rust
pub fn run<F: Function>(
    func: &F,
    env: &MachineEnv,
    options: &RegallocOptions,
) -> Result<Output, RegAllocError>
```

**Zig Port Status:** ✅ Done - `run()`

### 2. run_with_ctx (lib.rs lines 1676-1689)

**Rust:**
```rust
pub fn run_with_ctx<'a, F: Function>(
    func: &F,
    env: &MachineEnv,
    options: &RegallocOptions,
    ctx: &'a mut Ctx,
) -> Result<&'a Output, RegAllocError>
```

**Zig Port Status:** ✅ Done - `runWithCtx()`

### 3. Ctx struct

**Rust:**
```rust
pub struct Ctx {
    cfginfo: CFGInfo,
    ranges: LiveRanges,
    bundles: LiveBundles,
    spillsets: SpillSets,
    ...
}
```

**Zig Port Status:** ✅ Done - `Ctx` struct

| Field | Status | Notes |
|-------|--------|-------|
| cfginfo | ✅ | CFGInfo |
| ranges | ✅ | ArrayListUnmanaged(LiveRange) |
| bundles | ✅ | ArrayListUnmanaged(LiveBundle) |
| spillsets | ✅ | ArrayListUnmanaged(SpillSet) |
| spillslots | ✅ | ArrayListUnmanaged(SpillSlotData) |
| slots_by_class | ✅ | [3]SpillSlotList |
| vregs | ✅ | ArrayListUnmanaged(VRegData) |
| pregs | ✅ | ArrayListUnmanaged(PRegData) |
| allocation_queue | ✅ | PrioQueue |
| spilled_bundles | ✅ | ArrayListUnmanaged(LiveBundleIndex) |
| blockparam_ins | ✅ | ArrayListUnmanaged(BlockparamIn) |
| blockparam_outs | ✅ | ArrayListUnmanaged(BlockparamOut) |
| multi_fixed_reg_fixups | ✅ | ArrayListUnmanaged(MultiFixedRegFixup) |
| extra_spillslots_by_class | ✅ | [3]ArrayListUnmanaged(Allocation) |
| output | ✅ | Output |

---

## Implementation Plan

### Phase 1: Ctx struct
- [x] Ctx struct definition
- [x] init() method
- [x] deinit() method
- [x] clear() method for reuse

### Phase 2: Public API
- [x] run() function
- [x] runWithCtx() function

### Phase 3: Tests
- [x] Ctx init and deinit test
- [x] Ctx clear for reuse test

---

## Notes

1. **Reusable Context**: The Ctx struct is designed to be reused across multiple allocations to avoid repeated allocations.

2. **Clear for Reuse**: The clear() method efficiently resets all data structures while retaining capacity.

3. **Phase Orchestration**: runWithCtx() orchestrates all allocation phases:
   - CFG analysis
   - Liveness analysis (requires Function interface)
   - Live range building (requires Function interface)
   - Bundle merging (requires Function interface)
   - Bundle allocation (requires Function interface)
   - Spillslot allocation
   - Move insertion

4. **Function Interface**: The full implementation requires the Function trait callbacks for accessing instruction operands, block structure, etc.

