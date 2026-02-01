# AOT Native Codegen Execution Plan

## Goal

Complete the proof-of-concept AOT compiler pipeline:
```
Cot Source → Wasm → wasm_parser → wasm_to_ssa → SSA → regalloc → Native Code → ELF/Mach-O
```

After this is complete, the `codegen/native/` code will be **tested and exercised** on every build, preventing bit rot.

---

## Current Status: Phases 1-3 COMPLETE

**Test Results:** 376/398 passed, 22 skipped

All native codegen files have been copied and integrated.

---

## Files in `codegen/native/`

### AOT Pipeline (NEW)

| File | Lines | Status | Tests |
|------|-------|--------|-------|
| wasm_parser.zig | ~350 | ✅ Complete | 3 |
| wasm_to_ssa.zig | ~420 | ✅ Complete | 3 |

### Reference Implementation

| File | Lines | Status | Tests |
|------|-------|--------|-------|
| generic.zig | 308 | ✅ Complete | 1 |

### ARM64 Backend

| File | Lines | Status | Tests |
|------|-------|--------|-------|
| arm64_asm.zig | 989 | ✅ Complete | 29 |
| arm64.zig | 2,859 (was 3,589) | ✅ Refactored (20.3% reduction) | 1 |

### AMD64 Backend

| File | Lines | Status | Tests |
|------|-------|--------|-------|
| amd64_regs.zig | 218 | ✅ Complete | 3 |
| amd64_asm.zig | 1,628 | ✅ Complete | 13 |
| amd64.zig | 3,133 (was 3,946) | ✅ Refactored (20.6% reduction) | 1 |

### SSA Infrastructure (Previously Ported)

| File | Lines | Status | Tests |
|------|-------|--------|-------|
| liveness.zig | 947 | ✅ Ported | Skipped |
| regalloc.zig | 859 | ✅ Ported | Skipped |
| stackalloc.zig | 363 | ✅ Ported | Skipped |
| abi.zig | 387 | ✅ Ported | Skipped |
| expand_calls.zig | 256 | ✅ Ported | Skipped |
| decompose.zig | 285 | ✅ Ported | Skipped |

### Object File Output

| File | Lines | Status | Tests |
|------|-------|--------|-------|
| elf.zig | 529 | ✅ Ported | Skipped |
| macho.zig | 548 | ✅ Ported | Skipped |
| dwarf.zig | 363 | ✅ Ported | Skipped |

**Total: ~14,172 lines** (reduced from ~15,805 via refactoring)

---

## Audit Files Created

| File | Location |
|------|----------|
| generic.zig.md | audit/native/ |
| arm64_asm.zig.md | audit/native/ |
| arm64.zig.md | audit/native/ |
| amd64_regs.zig.md | audit/native/ |
| amd64_asm.zig.md | audit/native/ |
| amd64.zig.md | audit/native/ |

---

## Phase 4: Integration & Wiring (COMPLETE)

### Goal
Wire ARM64/AMD64 codegen into driver.zig and re-enable skipped tests.

### Status: Core Wiring COMPLETE

The AOT pipeline is wired into driver.zig and works for simple programs:
- `./zig-out/bin/cot test/cases/arithmetic/add.cot -o test && ./test` → exit code 42 ✓

### Tasks

#### 4.1: Wire ARM64 Codegen ✅
- [x] Import arm64.zig in driver.zig (line 22)
- [x] Add ARM64 code path for arm64-macos target (lines 276-279)
- [x] Generate Mach-O object file
- [x] Test: compile simple Cot program to native binary ✓

#### 4.2: Wire AMD64 Codegen ✅
- [x] Import amd64.zig in driver.zig (line 23)
- [x] Add AMD64 code path for amd64-linux target (lines 280-283)
- [x] Generate ELF object file (not tested on macos)

#### 4.3: Skipped Tests (5 remaining)
- liveness.zig: 3 tests skipped (computeLiveness variants)
- elf.zig: 1 test skipped
- macho.zig: 1 test skipped

Note: Original plan mentioned 22 skipped tests, but most were enabled during Phase 1-3.

#### 4.4: Known Issues
- Complex programs (e.g., fib_small) compile but hang on execution
- Requires debugging of the wasm_to_ssa conversion or native codegen

---

## Success Criteria

1. **All tests pass:** `zig build test` shows 0 failed, 0 skipped
2. **E2E works:** Can compile a simple Cot program to native binary and run it
3. **Audit complete:** Every ported file has an audit document
4. **No bootstrap-0.2 dependencies:** Code is self-contained

---

## Completed Phases

### Phase 1: Generic Codegen ✅

- [x] Copy generic.zig to codegen/native/
- [x] Update imports to new paths
- [x] Write audit file
- [x] Wire into driver.zig (uses generic for native path)
- [x] Tests pass (1/1)

### Phase 2: ARM64 Backend ✅

- [x] Copy arm64/asm.zig → arm64_asm.zig
- [x] Copy codegen/arm64.zig → arm64.zig
- [x] Update imports
- [x] Write audit files
- [x] Tests pass (30/30)

### Phase 3: AMD64 Backend ✅

- [x] Copy amd64/regs.zig → amd64_regs.zig
- [x] Copy amd64/asm.zig → amd64_asm.zig
- [x] Copy codegen/amd64.zig → amd64.zig
- [x] Update imports
- [x] Write audit files
- [x] Tests pass (17/17)

---

## Next Step

Begin Phase 4.1: Wire ARM64 codegen into driver.zig for actual native binary generation.
