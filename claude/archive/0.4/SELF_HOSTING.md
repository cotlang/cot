# Cot Self-Hosting: Status and Path to 0.4

**Updated:** 2026-03-25
**Version:** 0.3.8
**Goal:** selfcot2.wasm compiles selfcot3.wasm, and they are byte-identical.
**Milestone:** Self-hosting completion is the gate for **Cot 0.4** release.

---

## Current Status

| Step | Status | Command |
|------|--------|---------|
| Zig compiler builds selfcot (native) | **PASS** | `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot` |
| selfcot runs | **PASS** | `/tmp/selfcot version` → `cot 0.3.7 (self-hosted)` |
| selfcot passes 82/82 test files | **NEEDS VERIFICATION** | `/tmp/selfcot test test/cases/*.cot` |
| selfcot compiles selfcot2.wasm | **PASS** | `/tmp/selfcot build self/main.cot -o /tmp/selfcot2.wasm` (2.2MB) |
| selfcot2.wasm validates | **PASS** | `wasmtime compile /tmp/selfcot2.wasm` |
| selfcot2.wasm runs | **PASS** | `wasmtime run /tmp/selfcot2.wasm version` → `cot 0.3.7 (self-hosted)` |
| selfcot2.wasm compiles files | **PARTIAL** | Starts compiling, crashes at Map hash (corrupt string) |
| selfcot2.wasm passes test files | **NOT YET** | Blocked by runtime codegen bugs |
| selfcot2.wasm compiles selfcot3.wasm | **NOT YET** | Blocked by above |
| selfcot2.wasm == selfcot3.wasm | **NOT YET** | Final self-hosting gate |

---

## Path to 0.4 Release

### Phase 1: selfcot test parity (selfcot = native binary compiled by Zig)
Run ALL 82 test files through selfcot native binary. Fix any failures.
```bash
/tmp/selfcot test test/e2e/features.cot    # Must pass 370/370
./test/run_all.sh COT=/tmp/selfcot         # Must pass 82/82
```

### Phase 2: selfcot2 runtime bugs (selfcot2 = Wasm binary compiled by selfcot)
Fix runtime codegen bugs in selfcot2.wasm. Known:
- Map string hash corruption when compiling files (corrupt string ptr/len in Wasm)
- Likely 5-15 additional runtime bugs (every pipeline stage untested as Wasm code)
```bash
wasmtime run --dir=. /tmp/selfcot2.wasm build self/test_tiny.cot  # First bootstrap test
wasmtime run --dir=. /tmp/selfcot2.wasm test test/cases/*.cot     # All cases
```

### Phase 3: selfcot2 compiles selfcot3
```bash
wasmtime run --dir=. /tmp/selfcot2.wasm build self/main.cot -o /tmp/selfcot3.wasm
```

### Phase 4: Verify byte-identical output
```bash
diff /tmp/selfcot2.wasm /tmp/selfcot3.wasm  # Must be identical
# If different: compare function-by-function with wasm-objdump
```

### Phase 5: 0.4 Release
- Tag v0.4.0
- Update VERSION file
- Release binaries
- Announce: Cot is self-hosted

---

## Zig Compiler Status (v0.3.8)

- **82/82 test files pass** (370 features, 22 cases, all e2e)
- **CI green** on macOS + Linux
- **VWT Phase 8 complete** — address-only generics, shared function bodies
- **selfcot builds** in ~10s, 2,564 functions, 482 VWT types
- ~46,250 lines of Cot across 42 self/ files

---

## Key Milestones Achieved (2026-03-25)

1. **selfcot2.wasm validates** — zero Wasm validation errors (was blocked by f64 codegen, CSE use counts, unresolved functions)
2. **selfcot2.wasm runs** — `version` and `help` commands work
3. **selfcot2.wasm starts compiling** — checker runs, processes declarations, crashes at Map hash
4. **82/82 test files pass** — all stdlib updated for @safe, VWT, COW APIs
5. **VWT complete** — shared generic bodies with runtime @sizeOf, memcpy stores, f64↔i64 boundary handling

---

## Debugging Methodology

**Pipeline debugger** — `COT_DEBUG=phase ./zig-out/bin/cot build file.cot`:
- `COT_DEBUG=lower` — lowerer Phase 8 T-indirect, memcpy, SRET
- `COT_DEBUG=codegen` — SSA gen, unresolved functions, float locals, use counts
- `COT_DEBUG=deadcode` — edge removal, phi arg handling
- `COT_SSA=funcName` — interactive HTML SSA visualizer

**USE WATCH** — negative use count detection in `value.zig` (setArg, resetArgs, resetArgsFree)

**UNRESOLVED** — function resolution failures logged in `gen.zig` (emits `unreachable` trap instead of silent fallback)
