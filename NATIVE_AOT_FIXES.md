# Native AOT Fixes Required

## Executive Summary

**Status: Native AOT is NOT production-ready.**

On February 4, 2026, E2E testing revealed that native AOT compilation only works for the most trivial cases. The documentation was prematurely updated to claim "Native AOT Done" when in reality:

| Feature | Status |
|---------|--------|
| Return constant (`return 42`) | ✅ Works |
| Return simple expression (`return 10 + 5`) | ✅ Works |
| Local variables (`let x = 10`) | ❌ SIGSEGV at runtime |
| Function calls (no params) | ❌ Compiler panic (stack underflow) |
| Function calls (with params) | ❌ SIGSEGV at runtime |
| If/else control flow | ❌ SIGSEGV at runtime |
| While loops | ❌ Untested (likely broken) |
| Structs | ❌ Untested (likely broken) |
| Pointers | ❌ Untested (likely broken) |

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

## Task 1: Fix Stack Underflow in Function Calls

**Error observed:**
```
thread panic: attempt to use null value
compiler/codegen/native/wasm_to_clif/stack.zig:284:32: in pop1
    return self.stack.pop().?;
```

**When:** Compiling `fn get_five() int { return 5; } fn main() int { return get_five(); }`

**Pipeline stage:** Wasm → CLIF translation

**Our file:** `compiler/codegen/native/wasm_to_clif/stack.zig` and `translator.zig`

**Reference file:** `~/learning/wasmtime/crates/cranelift/src/translate/state.rs`

### Investigation Steps (Follow TROUBLESHOOTING.md)

1. **Find where the error occurs:**
   - The panic is in `pop1()` which is called from `translateLocalSet`
   - This means when translating `local.set`, the stack is empty when it shouldn't be

2. **Find reference handling of local.set:**
   ```bash
   grep -n "local_set\|LocalSet" ~/learning/wasmtime/crates/cranelift/src/translate/code_translator.rs
   ```

3. **Compare stack management:**
   - How does Cranelift manage the value stack?
   - When are values pushed/popped?
   - What happens at function entry?

4. **Check function prologue:**
   - Does Cranelift push initial values onto the stack for locals?
   - Does our translator do the same?

5. **DO NOT guess.** Find exactly what Cranelift does and copy it.

### Likely Issue (Hypothesis Only - Verify Against Reference)

The Wasm `local.set` instruction expects a value on the stack. If the stack is empty:
- Either we're not pushing values correctly during earlier instructions
- Or we're handling function entry incorrectly
- Or we're mishandling the Wasm locals initialization

**Find the reference. Copy the reference. Do not invent.**

---

## Task 2: Fix Local Variables (SIGSEGV)

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

**Pipeline stage:** Either Wasm→CLIF translation OR code emission

**Our files:**
- `compiler/codegen/native/wasm_to_clif/translator.zig` (local.get/local.set)
- `compiler/codegen/native/isa/aarch64/lower.zig` (if it's a lowering issue)
- `compiler/codegen/native/isa/aarch64/inst/emit.zig` (if it's emission)

**Reference files:**
- `~/learning/wasmtime/crates/cranelift/src/translate/code_translator.rs`
- `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/lower.isle`
- `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/inst/emit.rs`

### Investigation Steps (Follow TROUBLESHOOTING.md)

1. **Determine if crash is in translation or emission:**
   ```bash
   # Compile to .o file and disassemble
   ./zig-out/bin/cot test.cot -o test
   objdump -d test > test.asm
   # OR
   lldb test
   run
   bt  # backtrace to see where crash occurs
   ```

2. **If crash is in generated code:**
   - Look at the disassembly
   - Find which instruction crashes
   - Trace back to what CLIF instruction generated it
   - Compare with Cranelift's emission for that instruction

3. **Check local variable handling:**
   - How does Cranelift translate `local.get`?
   - How does Cranelift translate `local.set`?
   - Are we using stack slots correctly?

4. **DO NOT guess the fix.** Find the reference pattern and copy it.

---

## Task 3: Fix Function Calls with Parameters (SIGSEGV)

**Error observed:**
```
Exit: 139 (SIGSEGV)
```

**When:** Running native executable compiled from:
```cot
fn add(a: int, b: int) int {
    return a + b;
}
fn main() int {
    return add(10, 5);
}
```

**Pipeline stage:** Likely ABI handling or call instruction emission

**Our files:**
- `compiler/codegen/native/wasm_to_clif/translator.zig` (call translation)
- `compiler/codegen/native/isa/aarch64/abi.zig` (ABI handling)
- `compiler/codegen/native/isa/aarch64/lower.zig` (call lowering)

**Reference files:**
- `~/learning/wasmtime/crates/cranelift/src/translate/code_translator.rs` (Operator::Call)
- `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/abi.rs`
- `~/learning/wasmtime/cranelift/codegen/src/isa/aarch64/lower.isle`

### Investigation Steps (Follow TROUBLESHOOTING.md)

1. **Disassemble and find crash point:**
   ```bash
   lldb ./test
   run
   bt
   disassemble
   ```

2. **Check calling convention:**
   - ARM64 uses x0-x7 for arguments
   - How does Cranelift set up the call?
   - How do we set up the call?

3. **Compare call instruction emission:**
   - Find `Operator::Call` in Cranelift's code_translator.rs
   - Compare with our `translateCall` function
   - Look for ANY difference

4. **Check argument passing:**
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
- Documentation was prematurely updated and needs correction
