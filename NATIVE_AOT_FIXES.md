# Native AOT Fixes Required

## Executive Summary

**Status: Native AOT is NOT production-ready.**

On February 4, 2026, E2E testing revealed that native AOT compilation only works for the most trivial cases. The documentation was prematurely updated to claim "Native AOT Done" when in reality:

| Feature | Status |
|---------|--------|
| Return constant (`return 42`) | ✅ Works |
| Return simple expression (`return 10 + 5`) | ✅ Works |
| Local variables (`let x = 10`) | ✅ **FIXED** (Feb 5, 2026) |
| Function calls (no params) | ✅ **FIXED** (Feb 4, 2026) |
| Nested function calls | ✅ **FIXED** (Feb 4, 2026) |
| Function calls (2+ params) | ✅ **FIXED** (Feb 5, 2026) |
| Function calls (1 param) | ❌ Compiler panic - index out of bounds |
| If/else control flow | ✅ **FIXED** (Feb 5, 2026) |
| While loops | ✅ **FIXED** (Feb 5, 2026) |
| Recursion | ❌ Returns base case immediately |
| Structs | ❌ Untested |
| Pointers | ❌ Untested |

**Feb 5 update:** vmctx wrapper fix resolved most SIGSEGV issues. New bugs found:
- Single-parameter functions crash compiler (index out of bounds in SSA)
- Recursion returns base case immediately (recursive call not working)

---

## CRITICAL: Methodology for ALL Fixes

> **READ THIS BEFORE EVERY TASK**

Every fix in this document MUST follow the process in `TROUBLESHOOTING.md`. The summary:

1. **NEVER invent logic** - If you're reasoning about what code "should" do, STOP
2. **ALWAYS find reference** - Every line of native codegen is ported from Cranelift
3. **ALWAYS copy exactly** - Translate Rust→Zig syntax, but preserve ALL logic
4. **NEVER simplify** - Even if reference code seems unnecessarily complex, copy it

**Reference locations for native AOT:**

| Our Code | Reference Code |
|----------|----------------|
| `compiler/codegen/native/wasm_to_clif/` | `~/learning/wasmtime/crates/cranelift/src/translate/` |
| `compiler/codegen/native/wasm_to_clif/translator.zig` | `~/learning/wasmtime/crates/cranelift/src/translate/code_translator.rs` |
| `compiler/codegen/native/wasm_to_clif/func_translator.zig` | `~/learning/wasmtime/crates/cranelift/src/translate/func_translator.rs` |
| `compiler/codegen/native/wasm_to_clif/stack.zig` | `~/learning/wasmtime/crates/cranelift/src/translate/state.rs` |
| `compiler/ir/clif/` | `~/learning/wasmtime/cranelift/codegen/src/ir/` |
| `compiler/codegen/native/machinst/` | `~/learning/wasmtime/cranelift/codegen/src/machinst/` |
| `compiler/codegen/native/isa/aarch64/` | `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/` |

---

## Task 0: Create Native E2E Test Infrastructure

**Priority: HIGHEST - Do this first**

Before fixing any bugs, we need a test harness that:
1. Compiles Cot source → native executable
2. Runs the executable
3. Checks the exit code matches expected value

**Location:** Create `compiler/codegen/native_e2e_test.zig`

**Model after:** `compiler/codegen/wasm_e2e_test.zig`

**Test cases to include (start with failing ones):**
```
test_return_42        -> expect 42
test_return_add       -> expect 15 (10 + 5)
test_local_var        -> expect 15 (let x=10; let y=5; return x+y)
test_func_no_params   -> expect 5 (fn get_five() -> return 5; fn main -> return get_five())
test_func_one_param   -> expect 20 (fn double(x) -> x+x; fn main -> return double(10))
test_func_two_params  -> expect 15 (fn add(a,b) -> a+b; fn main -> return add(10,5))
test_if_true          -> expect 1 (if 10 > 5 { return 1 } else { return 0 })
test_if_false         -> expect 0 (if 5 > 10 { return 1 } else { return 0 })
test_while_sum        -> expect 55 (sum 1 to 10)
```

**DO NOT invent the test harness design.** Look at how Cranelift's filetest infrastructure works:
- Reference: `~/learning/wasmtime/cranelift/filetests/`

---

## Task 1: Fix Stack Underflow in Function Calls ✅ FIXED

**Status:** FIXED on February 4, 2026

**Original Error:** Functions calling other functions would hang in infinite loop

**Root Cause Analysis:**

The issue was NOT a stack underflow in Wasm→CLIF translation. The actual issue was:

1. Functions that make calls need to save the link register (x30/LR) in the prologue
2. Without saving LR, after a `bl` instruction overwrites x30 with the return address
3. When main's `ret` executed, x30 still pointed to main's `ret` → infinite loop!

**Fix Applied (Following TROUBLESHOOTING.md methodology):**

1. **Found reference:** `cranelift/codegen/src/machinst/vcode.rs:687-745` - `compute_clobbers_and_function_calls()`
2. **Found reference:** `cranelift/codegen/src/isa/aarch64/abi.rs:1158` - checks `function_calls != .None`
3. **Copied pattern exactly:**

**Changes made:**

| File | Change |
|------|--------|
| `compiler/codegen/native/isa/aarch64/inst/mod.zig` | Added `callType()` method to classify call instructions |
| `compiler/codegen/native/isa/x64/inst/mod.zig` | Added `callType()` method to classify call instructions |
| `compiler/codegen/native/machinst/vcode.zig` | Added scanning for calls + prologue/epilogue emission |

**Prologue emitted when `function_calls != .None`:**
```asm
stp x29, x30, [sp, #-16]!   ; Save FP and LR
mov x29, sp                  ; Set up frame pointer
```

**Epilogue emitted before `ret` instructions:**
```asm
ldp x29, x30, [sp], #16     ; Restore FP and LR
```

**Test result:** `fn get_five() -> 5; fn main() -> get_five()` returns exit code 5 ✅

---

## Task 2: Fix Local Variables (SIGSEGV) ✅ FIXED

**Error observed:**
```
Exit: 139 (SIGSEGV)
```

**When:** Running native executable compiled from:
```cot
fn main() int {
    let x = 10;
    let y = 5;
    return x + y;
}
```

### Root Cause Analysis (February 4, 2026)

**The problem is NOT in Wasm→CLIF translation.** Locals are correctly translated to CLIF Variables using `builder.useVar()` and `builder.defVar()`.

**The problem is in Wasm codegen → native execution:**

1. Cot compiles to Wasm with **SP-based stack frames in linear memory**
   - `compiler/codegen/wasm/gen.zig` computes `frame_size`
   - Locals are stored at `SP + offset` in Wasm linear memory

2. The Wasm is then AOT compiled to native, but the native code still expects:
   - Global SP to exist at a known memory location
   - Linear memory to be allocated starting at some base address

3. **The stub in `lower.zig:2147`** uses hardcoded `0x10000` for vmctx_base:
   ```zig
   const vmctx_base: u64 = 0x10000;  // STUB - doesn't exist!
   ```

4. Generated code tries to load/store at addresses like `0x20000` → **SIGSEGV**

### Fix Approach (Following TROUBLESHOOTING.md)

**Option A: Add runtime memory initialization** (Wasmtime approach)
- Add BSS section with memory for Wasm linear memory
- Add startup code to initialize SP
- Update vmctx_base to point to actual memory

**Option B: Change codegen to avoid Wasm memory for locals**
- Would require major changes to Wasm codegen
- Not recommended - breaks Wasm semantics

**Reference:** Look at how Wasmtime initializes `VMContext` in `wasmtime/crates/runtime/src/vmcontext.rs`

### Our files to modify:
- `compiler/codegen/native/isa/aarch64/lower.zig` (vmctx_base)
- `compiler/codegen/native/object_module.zig` (add data section)
- `compiler/driver.zig` (coordinate memory setup)

### Architectural Challenge

The fix is complex because:
1. `lower.zig` generates hardcoded `mov x0, #0x10000` instructions
2. This needs to become a **relocation** to a symbol (e.g., `__wasm_memory`)
3. That symbol needs to be defined as a data section in the object file
4. The memory needs to be initialized (at least SP set to a valid offset)

This requires changes at multiple levels:
- CLIF IR generation (reference symbol instead of constant)
- Lowering (emit relocation instead of immediate)
- Object file (add BSS/data section)

**Complexity: HIGH** - Affects core memory model for native AOT

### Fix Applied (February 5, 2026)

**Solution:** Ported Cranelift vmctx pattern - generate _main wrapper that initializes static vmctx.

**Changes:**
| File | Change |
|------|--------|
| `compiler/driver.zig` | Generate _main wrapper with ADRP/ADD/BL, static vmctx buffer |
| `compiler/codegen/native/wasm_to_clif/func_translator.zig` | Add vmctx params to wasm function signatures |
| `compiler/codegen/native/wasm_to_clif/translator.zig` | Offset param indices by 2 for vmctx params |
| `compiler/codegen/native/isa/aarch64/lower.zig` | VMContext GlobalValue uses vmctx parameter |
| `compiler/codegen/native/macho.zig` | Fix extern bit for symbol-based relocations |

**Test result:** `let x = 10; return x` returns 10 ✅

---

## Task 3: Fix Function Calls with Parameters ✅ MOSTLY FIXED

**Status (Feb 5, 2026):** Works for 2+ parameters, but **1-parameter functions crash compiler**.

### What works:
```cot
fn add(a: i64, b: i64) i64 { return a + b }
fn main() i64 { return add(10, 5) }  // Returns 15 ✅
```

### What's broken:
```cot
fn identity(n: i64) i64 { return n }
fn main() i64 { return identity(42) }  // CRASH: index out of bounds
```

**Error:**
```
panic: index out of bounds: index 2863311530, len 47
compiler/ir/clif/dfg.zig:614:33: in valueType
compiler/codegen/native/frontend/ssa.zig:412:76: in sealOneBlock
```

### Investigation needed:
- The crash is in `sealBlock` when getting the type of a value with garbage index
- Likely an uninitialized Variable or Value being used
- Check how single-parameter functions differ from multi-parameter

**Reference files:**
- `~/learning/wasmtime/crates/cranelift/src/translate/code_translator.rs` (Operator::Call)
- `compiler/codegen/native/frontend/ssa.zig` (sealBlock)

### Original Investigation Steps (Follow TROUBLESHOOTING.md)
   - How does Cranelift push arguments?
   - How do we push arguments?
   - Are stack adjustments correct?

5. **DO NOT invent fixes.** Copy the reference exactly.

---

## Task 4: Fix If/Else Control Flow (SIGSEGV)

**Error observed:**
```
Exit: 139 (SIGSEGV)
```

**When:** Running native executable compiled from:
```cot
fn main() int {
    let x = 10;
    if x > 5 {
        return 1;
    } else {
        return 0;
    }
}
```

**Pipeline stage:** Control flow translation or branch emission

**Our files:**
- `compiler/codegen/native/wasm_to_clif/translator.zig` (br_if, if, else, end)
- `compiler/codegen/native/isa/aarch64/lower.zig` (branch lowering)

**Reference files:**
- `~/learning/wasmtime/crates/cranelift/src/translate/code_translator.rs`
- `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/lower.isle`

### Investigation Steps (Follow TROUBLESHOOTING.md)

1. **Note:** This might be the same root cause as Task 2 (local variables)
   - If local variables are broken, `let x = 10` will fail
   - Fix Task 2 first, then re-test this

2. **If still broken after Task 2:**
   - Disassemble and find crash point
   - Check branch instruction encoding
   - Compare with Cranelift's branch emission

3. **Check control flow translation:**
   - How does Cranelift translate Wasm `if`?
   - How does Cranelift handle block parameters?
   - How does Cranelift handle `end`?

4. **DO NOT guess.** Find reference and copy.

---

## Task 5: Test and Fix While Loops

**Not yet tested** - likely broken if control flow is broken.

**Test code:**
```cot
fn main() int {
    let sum = 0;
    let i = 1;
    while i <= 10 {
        sum = sum + i;
        i = i + 1;
    }
    return sum;  // expect 55
}
```

**Wait until Tasks 2-4 are fixed before testing.**

---

## Task 6: Test and Fix Recursion

**Not yet tested** - depends on function calls working.

**Test code:**
```cot
fn factorial(n: int) int {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}

fn main() int {
    return factorial(5);  // expect 120
}
```

**Wait until Tasks 1-4 are fixed before testing.**

---

## Task 7: Test and Fix Structs

**Not yet tested** - likely broken.

**Test code:**
```cot
struct Point {
    x: int,
    y: int,
}

fn main() int {
    let p = Point { x: 10, y: 20 };
    return p.x + p.y;  // expect 30
}
```

**Reference for struct handling:**
- `~/learning/wasmtime/crates/cranelift/src/translate/code_translator.rs` (memory operations)

---

## Task 8: Test and Fix Pointers

**Not yet tested** - likely broken.

**Test code:**
```cot
fn main() int {
    let x = 10;
    let p = &x;
    return *p;  // expect 10
}
```

---

## Order of Operations

1. **Task 0** - Create test infrastructure (required for all other tasks)
2. **Task 1** - Fix compiler panic (can't test anything if compiler crashes)
3. **Task 2** - Fix local variables (most basic feature after constants)
4. **Task 3** - Fix function calls (needed for interesting programs)
5. **Task 4** - Fix if/else (may be fixed by Task 2)
6. **Task 5** - Test loops (may be fixed by earlier tasks)
7. **Task 6** - Test recursion (depends on Tasks 3-4)
8. **Task 7** - Test structs (memory operations)
9. **Task 8** - Test pointers (memory operations)

---

## Success Criteria

Native AOT can only be called "done" when:

1. All tests in `native_e2e_test.zig` pass
2. Test coverage matches `wasm_e2e_test.zig` coverage
3. The same Cot programs produce the same results on both Wasm and native targets

---

## Reminder: The Process

Before making ANY change, verify:

- [ ] I identified which pipeline stage has the bug
- [ ] I found the exact reference file for this stage
- [ ] I found the exact function in the reference
- [ ] I did a line-by-line comparison
- [ ] I found a difference between our code and reference
- [ ] My change copies the reference pattern exactly
- [ ] I did NOT invent any new logic

**If you cannot check all boxes, STOP and find the reference.**

---

## History

- **Feb 4, 2026 (AM)**: Fixed value aliases, jump table relocs, operand order - `return 42` works
- **Feb 4, 2026 (PM)**: E2E testing revealed most features still broken
- **Feb 5, 2026**: Fixed vmctx wrapper - local variables, if/else, while loops now work
  - Single-param functions still crash (new bug)
  - Recursion returns base case immediately (new bug)
