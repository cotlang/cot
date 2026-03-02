# Linux x86_64 Parity — Complete Investigation Notes

**Date:** 2026-03-02 (updated)
**Status:** Root cause found. Partial fix applied. More work needed.
**Goal:** Full test parity on Linux x86_64.

## Current State

- **Wasm→CLIF path** (current Linux default): 66/70 test files pass
- 4 failures: `arc.cot`, `spawn.cot`, `thread_basic.cot`, `threading.cot` (Wasm→CLIF translator stack underflow)
- **Direct native path** (macOS default, not yet enabled on Linux): 38/70 when enabled

## Critical Discovery This Session: `alu_rmi_r` dst==src1 Bug

### Why x64 has bugs that arm64 doesn't
- **arm64:** Native 3-operand ISA (`add x0, x1, x2`). dst is always a separate register. `gprEarlyDef(dst)` + `gprUse(src)` never creates same-vreg conflicts.
- **x64:** 2-operand ISA (`add src, dst` is destructive). The compiler models as 3-operand (`dst = src1 OP src2`) and emit code handles `mov src1→dst; OP src2, dst`. But the lowering sometimes creates `alu_rmi_r` with `dst == src1`, which breaks the regalloc.

---

## NEW: Bug — `alu_rmi_r` with dst == src1 (CONFIRMED FIX IN lower.zig)

### Root Cause
`lower.zig` `lowerIaddImm()` (line ~670) emits a redundant `mov lhs→dst` followed by `alu_rmi_r { src1: dst, src2: imm, dst: dst }`. Since `src1 == dst`, both operands resolve to the same vreg after alias resolution in `collectOperands`. In `get_operands.zig`, `alu_rmi_r` uses `gprEarlyDef(dst)` + `gprUse(src1)`, creating **Early DEF + Early USE of the same vreg at the same ProgPoint** (Before(inst)).

This violates regalloc2's invariant: `debug_assert!(range.to <= last_range.from)` in `add_liverange_to_vreg` (`liveranges.rs:183`). The liveness builder creates overlapping ranges for the vreg, causing the allocator to assign the same physical register to interfering live ranges → SIGSEGV.

### Debug Evidence
```
SAME-VREG EARLY DEF+USE: inst13 vreg v256 kind=use pos=early constraint=.{ .reg = void }
  operand[0]: vreg=v256 kind=def pos=early constraint=.{ .reg = void }
  operand[1]: vreg=v256 kind=use pos=early constraint=.{ .reg = void }
LIVERANGE OVERLAP: vreg v256, new range [0..27], last range [26..73]
```

### The Fix (applied in lower.zig)
Remove the redundant `mov lhs→dst` and pass `lhs_gpr` as `src1` instead of `dst_gpr`. The `alu_rmi_r` emit code (`emit.zig:1081`) already handles `mov src1→dst` when `src1 != dst`:

```zig
// BEFORE (broken):
ctx.emit(mov_r_r { src: lhs_gpr, dst: dst_gpr });  // redundant!
ctx.emit(alu_rmi_r { src1: dst_gpr.toReg(), src2: imm, dst: dst_gpr });  // dst == src1!

// AFTER (fixed):
ctx.emit(alu_rmi_r { src1: lhs_gpr, src2: imm, dst: dst_gpr });  // dst != src1
```

Two call sites fixed: line 681 (small imm) and line 706 (large imm).

### Why Cranelift doesn't hit this
Cranelift uses `reg_reuse_def(dst, src1_idx)` for x64 ALU ops — a **Late DEF** with Reuse constraint (not Early DEF). Late DEF at After ProgPoint vs Early USE at Before ProgPoint → different positions, no overlap.

Reference: `references/wasmtime/cranelift/codegen/src/isa/x64/inst/external.rs:271-274`

### Other alu_rmi_r call sites (verified safe)
- `lowerBinaryAlu` (line 2732): `src1 = lhs_gpr`, `dst = dst_gpr` ✓
- `materializeGlobalValue` iadd_imm (line 2540): `src1 = base_addr`, `dst = dst_gpr` ✓
- `materializeGlobalValue` iadd_imm (line 2637): `src1 = base_addr`, `dst = tmp_gpr` ✓
- `lowerIMul` (line 777): `src1 = lhs_gpr`, `dst = dst_gpr` ✓

---

## NEW: liveness.zig Changes (ACCIDENTALLY REVERTED — NEED RE-APPLICATION)

### What was changed in `addLiverangeToVreg` (liveness.zig:307)
Reference: `regalloc2/src/ion/liveranges.rs:148-213`

**Change 1:** Added defensive truncation matching reference's `debug_assert!(range.to <= last_range.from)`:
```zig
// After the allow_multiple_defs block, before contiguity check:
if (adjusted_range.to.bits > last_range.from.bits) {
    adjusted_range.to = last_range.from;
}
```

**Change 2:** Changed contiguity check from `==` to `>=` to match reference structure:
```zig
// ORIGINAL:
if (adjusted_range.to.bits == last_range.from.bits) { /* merge */ }

// REFERENCE pattern (liveranges.rs:191-212):
// if range.to < last_range.from { create new } else { merge }
// Which means: merge when range.to >= last_range.from

// CHANGED TO:
if (adjusted_range.to.bits >= last_range.from.bits) { /* merge */ }
```

**Change 3:** Added reference comments throughout.

### Status
These changes were accidentally reverted by `git checkout HEAD -- liveness.zig` before any clean testing could be done. The lower.zig fix eliminates the root cause (no more dst==src1), so these liveness changes are primarily defensive. They should be re-applied and tested together with the lower.zig fix.

---

## NEW: Systematic Audit Needed — All `gprEarlyDef` Instructions

Every x64 instruction using `gprEarlyDef` in `get_operands.zig` must be verified that no lowering path creates dst == any-use-operand of the same vreg:

| Instruction | Operands | Cranelift Pattern | Risk |
|-------------|----------|-------------------|------|
| `alu_rmi_r` | `gprEarlyDef(dst)` + `gprUse(src1)` | `reg_use` + `reg_reuse_def` (Late) | **FIXED** in lower.zig |
| `shift_r` | `gprEarlyDef(dst)` + `gprUse(value)` | Similar reuse | **CHECK** lowering |
| `cmove` | `gprEarlyDef(dst)` + `gprUse(alt)` | Similar reuse | **CHECK** lowering |
| `xmm_rm_r` | `xmmEarlyDef(dst)` + `xmmUse(src1)` | Similar reuse | **CHECK** lowering |

The CORRECT long-term fix: change all 2-operand x64 patterns to `regDefReuse` (Late DEF + Reuse constraint) matching Cranelift, instead of `gprEarlyDef`.

---

## Category 1: x64 Register Allocation Bug (CRITICAL — Blocks Everything)

### Symptom

String indexing (`s[0]`), struct field access, and any operation that combines pointer arithmetic with value computation crashes with SIGSEGV. The disassembly shows `rcx` assigned to both a stack address and a zero constant, causing a store to address 0.

### Root Cause: `shift_r` Operand Constraints

**File:** `compiler/codegen/native/isa/x64/inst/get_operands.zig:487-498`

```zig
.shift_r => |*p| {
    visitor.gprEarlyDef(&p.dst);
    visitor.gprUse(&p.value);       // ← BUG: p.value is Gpr (value type), not *Gpr
    switch (p.shift_by) {
        .cl => {
            visitor.regFixedUse(p.src.toReg(), regs.gprPreg(regs.GprEnc.RCX));
        },
        .imm => {},
    }
},
```

The `shift_r` instruction struct (`inst/mod.zig:466-476`) defines `value: Gpr` as a **value type**. When `visitor.gprUse(&p.value)` takes its address, the register allocator gets a temporary reference that doesn't properly track the virtual register as a live use. This means the register allocator doesn't know `value` is live, and may assign RCX to it — which then collides with the `regFixedUse` constraint that forces the shift amount into RCX.

### Why ARM64 Doesn't Have This Bug

ARM64 shifts can use **any register** for the shift amount — there's no fixed-register constraint like x64's CL requirement. The ARM64 backend never generates a `regFixedUse(RCX)` constraint, so the collision never happens.

### Cranelift Reference Pattern

**File:** `references/wasmtime/cranelift/codegen/src/isa/x64/inst.isle:608-610, 2018-2036`

Cranelift uses an `Imm8Gpr` tagged union for shift amounts:
- Compile-time constant → immediate shift (no CL needed)
- Runtime value → register shift (CL constraint via assembly rules)
- 3-tier masking strategy for small types (const-time, runtime AND, implicit)

Cranelift also separates shift instructions by type and source:
- `x64_shlq_mc` (register shift), `x64_shlq_mi` (immediate shift), `x64_shlq_m1` (shift-by-1)
- BMI2 variant `x64_shlx` avoids CL entirely on modern CPUs

### Fix Approach

**Option A (minimal):** Fix the operand constraint to properly track `value` as a use:
- Change `shift_r.value` from `Gpr` to something the visitor can track, OR
- Add a `gprUseValue` method that copies the register reference correctly

**Option B (complete, matches Cranelift):** Implement immediate shift support:
- Detect constant shift amounts in `lowerShift()` and emit `shift_by = .imm` instead of `.cl`
- This eliminates the CL constraint entirely for constant shifts (the common case in string indexing)
- The emit code already supports `.imm` — only `lower.zig` needs changes

**Option C (both):** Fix the constraint bug AND add immediate shifts. This is the correct approach.

### Affected Tests

Every test that does string indexing, struct field access through pointers, array operations, or any computed offset arithmetic. This is approximately 20 of the 32 failing files.

### Verification

After fixing, string indexing must work:
```cot
test "string index" {
    var s = "hello"
    @assertEq(s[0], 104)
}
```

---

## Category 2: `load_ext_name` Relocation Ordering

### Symptom

RIP-relative data references (string literals, global variables) resolve to wrong addresses. The displacement field in `lea rN, [rip+disp32]` is 4 bytes off, causing reads from garbage memory.

### Root Cause

**File:** `compiler/codegen/native/isa/x64/inst/emit.zig:1799-1816`

The original code emits `put4(0)` (displacement placeholder) BEFORE recording the relocation. Since `addRelocExternalName` uses `curOffset()`, the relocation points to the byte AFTER the displacement field instead of the start.

```
WRONG:  put4(0) → addReloc()  → relocation at disp+4
CORRECT: addReloc() → put4(0) → relocation at disp+0  (matches call_known pattern)
```

The `call_known` instruction (line 1523-1536) does it correctly: `addRelocExternalName` BEFORE `put4(0)`, with addend `-4`.

### Cranelift Reference

Cranelift's `MachBuffer::add_reloc()` records at `self.data.len()` — i.e., the current cursor position. Displacement bytes are emitted AFTER the relocation is recorded. The Cot `call_known` instruction follows this pattern; `load_ext_name` does not.

### Fix

Reorder to match `call_known`:
```zig
// Record relocation BEFORE emitting displacement
try sink.addRelocExternalName(buffer_mod.Reloc.X86PCRel4, buffer_ext_name, load.offset - 4);
try sink.put4(0); // Placeholder (linker patches)
```

### Status

**Fix already applied** in this session's working tree (`emit.zig` modified). Verified: `print("hello")` works, RIP-relative addresses resolve correctly.

---

## Category 3: Platform Symbol Remapping

### Symptom

Linker error: `undefined reference to '__open'` on Linux.

### Root Cause

**File:** `compiler/driver.zig:1716`

`io_native.zig:700` calls `__open` — a macOS-internal non-variadic wrapper for `open()`. This symbol doesn't exist on Linux. On macOS ARM64, variadic functions pass args on the stack (not registers), so `__open` is needed as a non-variadic entry point. On x86_64 Linux, variadic and non-variadic calling conventions are identical — calling `open()` directly works.

### Fix

Add to the platform name remapping chain in `driver.zig:1716`:
```zig
const platform_name = if (!is_macos and std.mem.eql(u8, actual_name, "__error"))
    "__errno_location"
else if (!is_macos and std.mem.eql(u8, actual_name, "__open"))
    "open"
else
    actual_name;
```

### Status

**Fix already applied** in this session's working tree.

---

## Category 4: Platform-Specific Constants

### Symptom

Socket operations, non-blocking I/O, and terminal ioctl produce wrong behavior or EINVAL on Linux because constants have different values in the macOS vs Linux kernel ABI.

### Root Cause

**File:** `compiler/codegen/native/io_native.zig`

5 constants are hardcoded to macOS values:

| Constant | Function | macOS | Linux | Line |
|----------|----------|-------|-------|------|
| `SOL_SOCKET` | `generateNetSetReuseAddr` | `0xFFFF` | `1` | 1784 |
| `SO_REUSEADDR` | `generateNetSetReuseAddr` | `0x0004` | `2` | 1785 |
| `O_NONBLOCK` | `generateSetNonblocking` | `0x0004` | `0x800` | 1842 |
| `TIOCSWINSZ` | `generateIoctlWinsize` | `0x80087467` | `0x5414` | 2370 |
| `TIOCSCTTY` | `generateIoctlSetCtty` | `0x20007461` | `0x540E` | 2430 |

### Fix

Each function needs `target_os` threaded through (the top-level `generate()` already has it) and a conditional:
```zig
const sol_socket = try ins.iconst(clif.Type.I64, if (target_os == .macos) 0xFFFF else 1);
```

### Status

**Fix already applied** in this session's working tree. All 5 constants are now conditional. The `target_os` parameter has been added to all 4 affected function signatures and call sites.

---

## Category 5: Linux Linker Flags

### Symptom

Link failures or runtime crashes due to missing libraries.

### Root Cause

**File:** `compiler/main.zig:1432-1436`

Linux link step only passes `-lc`. Missing:
- `-lpthread` — needed for `pthread_*` on glibc < 2.34 (harmless no-op on newer)
- `-lutil` — needed for `openpty()` (used by PTY runtime)

### Fix

```zig
} else if (compile_target.os == .linux) {
    link_args.appendSlice(allocator, &.{ "-lc", "-lpthread", "-lutil" }) catch { ... };
}
```

### Status

**Fix already applied** in this session's working tree.

---

## Category 6: x64 Dispatch — Missing Opcodes

### Audit Result: FULL PARITY

Both ARM64 (`isa/aarch64/lower.zig`) and x64 (`isa/x64/lower.zig`) handle the exact same set of CLIF opcodes. No missing dispatch entries. The one exception is `bitcast` which x64 has but ARM64 does not (x64 is ahead here).

Note: `fmin`, `fmax`, `fcopysign` implementations exist in x64 but were not wired in the dispatch switch.

### Status

**Fix already applied** in this session's working tree. `.fmin`, `.fmax`, `.fcopysign` are now wired in the x64 dispatch.

---

## Category 7: Compiler Crashes on Stdlib-Heavy Tests

### Symptom

~10 test files (crypto.cot, dotenv.cot, encoding.cot, fmt.cot, json.cot, etc.) crash the compiler itself during compilation, showing stack traces from `main.zig`.

### Root Cause (Suspected)

These tests import stdlib modules extensively. The crash likely occurs in `ssa_to_clif.zig` when:
1. A function from an imported module isn't found in `func_index_map` (returns null → "FATAL: func_index_map miss")
2. An unimplemented SSA opcode hits an `else => unreachable` branch

These tests need to be investigated individually. The Wasm→CLIF path handles them correctly because it goes through a different code path that resolves function references differently.

### Investigation Approach

1. Run each failing test individually with debug logging enabled
2. Check if the crash is in `ssa_to_clif.zig` or `driver.zig`
3. Identify the specific function or opcode that triggers the crash
4. Compare how the Wasm→CLIF path resolves the same reference

---

## Category 8: Missing Shift Optimizations

### Current State

The x64 backend always emits register-based shifts (forcing shift amount into CL/RCX), even when the shift amount is a compile-time constant. The emit code supports immediate shifts, but `lower.zig` never generates them.

### Cranelift Reference

Cranelift's lowering detects constant shift amounts via `put_masked_in_imm8_gpr`:
- **Priority 2:** Constant → `Imm8Gpr.Imm8` (no CL constraint needed)
- **Priority 1:** Small type → runtime mask via `x64_and` before shift
- **Default:** GPR → `Imm8Gpr.Gpr` (CL constraint)

Plus `shift-by-1` optimization using shorter opcodes (0xD0/0xD1 vs 0xD2/0xD3).

### Fix

In `lower.zig:lowerShift()`:
1. Check if shift amount input is an `iconst` instruction
2. If constant: emit `shift_by = .{ .imm = @intCast(amt & mask) }`
3. If variable: emit `shift_by = .cl` (current behavior)

This is both a correctness improvement (avoids CL collision for the common case) and a performance improvement (immediate shifts are shorter and avoid register pressure).

---

## Category 9: Event Loop (epoll) — Known Limitation

**Not a blocker for parity.** Documented in `io_native.zig:261-265`.

macOS uses kqueue; Linux stubs return -1 for kqueue functions. Real epoll implementation is a separate feature, not needed for test parity.

---

## Execution Plan

### Phase 1: Fix the Shift Register Bug (Unblocks ~20 tests)

This is the critical path. Everything else is already fixed or is a minor issue.

1. **Fix `get_operands.zig:487-498`** — Make `shift_r.value` properly tracked by the register allocator
2. **Add immediate shift support to `lower.zig:lowerShift()`** — Detect constant shift amounts and avoid CL constraint entirely
3. **Verify:** `s[0]`, struct field access, array indexing all work

**Reference:** `references/wasmtime/cranelift/codegen/src/isa/x64/inst.isle:608-610` (Imm8Gpr pattern)

### Phase 2: Investigate Compiler Crashes (~10 tests)

1. Run each crashing test with debug logging
2. Identify missing function references or unhandled opcodes
3. Fix `func_index_map` registration for imported stdlib functions

### Phase 3: Enable `force_direct` on Linux

**File:** `compiler/main.zig:1300`

Change:
```zig
const force_direct = if (compile_target.os == .linux) false else (compile_target.arch != .wasm32);
```
To:
```zig
const force_direct = compile_target.arch != .wasm32;
```

### Phase 4: Full Test Suite Verification

```bash
zig build
cot test test/e2e/features.cot              # Primary: 354 tests
./test/run_all.sh                            # Full: 70 files
cot test test/e2e/features.cot --target=wasm32  # Regression check
```

Target: 70/70 on Linux x86_64 (matching macOS ARM64).

---

## Already-Working Areas (No Changes Needed)

Confirmed by audit — these are correct and complete:

| Area | Status | Notes |
|------|--------|-------|
| `__error` → `__errno_location` | ✅ | driver.zig:1716 |
| stat struct layout | ✅ | Platform-aware in io_native.zig:2915 |
| kqueue → stubs on Linux | ✅ | io_native.zig:253-258 |
| x64 backend: 105+ instruction types | ✅ | Full dispatch parity with ARM64 |
| ELF writer in ObjectModule | ✅ | object_module.zig:559 |
| ioctl/fcntl variadic convention | ✅ | is_aarch64 guards at lines 2358, 2418, 1843 |
| All pthread/thread/scheduler functions | ✅ | POSIX APIs, work on both platforms |
| ARC runtime | ✅ | Completely platform-neutral |
| Print runtime | ✅ | Platform-neutral (calls write() directly) |
| Symbol prefixing (`_` on macOS, none on ELF) | ✅ | driver.zig:1720-1723 |
| Entry point naming (`_main` vs `main`) | ✅ | driver.zig:1736 |
| Data symbol naming | ✅ | driver.zig:1599-1667 |
| Cross-compilation triples | ✅ | main.zig:1396-1401 |
| x64 main wrapper machine code | ✅ | driver.zig:1803-1847 |

---

## Files Modified in This Session

These changes are in the working tree but uncommitted. They implement the fixes described in Categories 2-6:

| File | Changes |
|------|---------|
| `compiler/codegen/native/isa/x64/lower.zig` | Wire `.fmin`, `.fmax`, `.fcopysign` in dispatch |
| `compiler/codegen/native/isa/x64/inst/emit.zig` | Fix `load_ext_name` relocation ordering + addend |
| `compiler/codegen/native/io_native.zig` | 5 platform constants → conditional on `target_os` |
| `compiler/driver.zig` | Add `__open` → `open` symbol remapping |
| `compiler/main.zig` | Add `-lpthread -lutil` linker flags; enable `force_direct` |

---

## References

| Component | Reference Location |
|-----------|-------------------|
| x64 shift instructions | `references/wasmtime/cranelift/codegen/src/isa/x64/inst.isle:608-610, 2018-2036` |
| x64 operand constraints | `references/wasmtime/cranelift/codegen/src/isa/x64/inst/args.rs` |
| SSA → CLIF | `references/rust/compiler/rustc_codegen_cranelift/src/base.rs` |
| ELF relocations | `references/wasmtime/cranelift/codegen/src/isa/x64/inst/emit.rs` |
| x64 calling convention | System V ABI (AMD64), section 3.2.3 |
