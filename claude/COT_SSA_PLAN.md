# COT_SSA — Interactive SSA Visualizer Execution Plan

**Date:** 2026-03-23
**Status:** Planning
**Goal:** Port Go's GOSSAFUNC HTML visualizer to Cot, then to selfcot (self-hosted compiler)

---

## What We're Building

`COT_SSA=funcname cot build file.cot` generates an interactive HTML page showing the function's SSA at every compiler pass, side by side. Click a value to highlight it across all passes. See exactly where values are created, transformed, or eliminated.

This is a line-by-line port of Go's `GOSSAFUNC` from `references/go/src/cmd/compile/internal/ssa/html.go` (1315 lines).

---

## Reference Files

| What | Go Reference | Cot Target |
|------|-------------|------------|
| HTML generator | `references/go/src/cmd/compile/internal/ssa/html.go` | `compiler/ssa/html.zig` (new) |
| Pass loop integration | `references/go/src/cmd/compile/internal/ssa/compile.go` | `compiler/driver.zig` |
| Hash computation | `references/go/src/cmd/compile/internal/ssa/print.go` | `compiler/ssa/html.zig` (inline) |
| Func/Value/Block rendering | `references/go/src/cmd/compile/internal/ssa/html.go:1012-1254` | `compiler/ssa/html.zig` |
| Frontend integration | `references/go/src/cmd/compile/internal/ssagen/ssa.go:44-77` | `compiler/driver.zig` |

---

## Cot's SSA Pass Pipeline (What Gets Visualized)

### Wasm Target (10 passes)
```
start → copyelim → rewritegeneric → decompose → rewritedec → copyelim →
cse → deadcode → schedule → layout → lower_wasm
```

### Native Target (6 passes)
```
start → rewritegeneric → decompose → rewritedec → schedule → layout → lower_native
```

### Hook Points in driver.zig

**Wasm path** (`driver.zig:6227-6236`):
```zig
// Each of these lines gets a WritePhase call before and after
try copyelim.copyelim(ssa_func);
try rewritegeneric.rewrite(func_alloc, ssa_func, &string_offsets);
try decompose_builtin.decompose(func_alloc, ssa_func, type_reg);
try rewritedec.rewrite(func_alloc, ssa_func);
try copyelim.copyelim(ssa_func);
try cse_pass.cse(ssa_func);
try deadcode.deadcode(ssa_func);
try schedule.schedule(ssa_func);
try layout.layout(ssa_func);
try lower_wasm.lower(ssa_func);
```

**Native path** (`driver.zig:1549-1554`):
```zig
try rewritegeneric.rewrite(func_alloc, ssa_func, &string_offsets);
try decompose_builtin.decompose(func_alloc, ssa_func, type_reg);
try rewritedec.rewrite(func_alloc, ssa_func);
try schedule.schedule(ssa_func);
try layout.layout(ssa_func);
try lower_native.lower(ssa_func);
```

---

## Implementation Plan

### Phase 1: HTMLWriter Core (`compiler/ssa/html.zig`)
**Reference:** `references/go/src/cmd/compile/internal/ssa/html.go`
**Estimated size:** ~800 lines of Zig

- [ ] **1.1** Create `compiler/ssa/html.zig` with `HTMLWriter` struct
  - Fields: file writer, func pointer, prev_hash, pending_phases, pending_titles
  - Reference: Go `html.go` lines 21-29

- [ ] **1.2** Port `start()` — write HTML header, CSS, JavaScript
  - Embed the complete CSS stylesheet (Go lines 73-364, ~290 lines)
  - Embed the complete JavaScript (Go lines 366-740, ~370 lines)
  - Dark mode support, column collapse/expand, history state
  - Reference: Go `html.go` lines 66-777

- [ ] **1.3** Port `Close()` — close table, body, html tags, close file
  - Print "dumped SSA for {name} to {path}" message
  - Reference: Go `html.go` lines 779-791

- [ ] **1.4** Port `WritePhase()` — hash-based phase capture
  - Compute CRC32 hash of current SSA state (Go `print.go` lines 20-25)
  - If hash unchanged, batch with pending phases (skip rendering identical columns)
  - If hash changed, call `flushPhases()` to emit column
  - Reference: Go `html.go` lines 793-821

- [ ] **1.5** Port `flushPhases()` — emit HTML column
  - Combine batched phase names with " + "
  - Call `Func.HTML()` to render the SSA
  - Emit collapsed tab + expanded content
  - Reference: Go `html.go` lines 823-838

- [ ] **1.6** Port `WriteMultiTitleColumn()` — column generation
  - Collapsed column (vertical text, click to expand)
  - Expanded column (h2 titles + SSA content)
  - Phase ID for CSS targeting
  - Reference: Go `html.go` lines 928-946

- [ ] **1.7** Port `WriteSources()` — source code column
  - Line numbers on left, source code on right
  - Line number CSS classes for cross-referencing (`l42` etc.)
  - Reference: Go `html.go` lines 840-881

### Phase 2: SSA Rendering (`compiler/ssa/html.zig` continued)
**Reference:** `references/go/src/cmd/compile/internal/ssa/html.go:1012-1254`

- [ ] **2.1** Port `Value.HTML()` — short value rendering
  - Format: `<span class="v42 ssa-value">v42</span>`
  - CSS class = value ID for cross-pass highlighting
  - Reference: Go `html.go` lines 1012-1016

- [ ] **2.2** Port `Value.LongHTML()` — full value rendering
  - Format: `v42 (line:col) = add <i64> v10 v11 : R0`
  - Includes: op name, type, args (as clickable links), register allocation, aux data
  - Source position as line number
  - Reference: Go `html.go` lines 1018-1041

- [ ] **2.3** Port `Block.HTML()` — short block rendering
  - Format: `<span class="b5 ssa-block">b5</span>`
  - CSS class = block ID for cross-pass highlighting
  - Reference: Go `html.go` lines 1043-1047

- [ ] **2.4** Port `Block.LongHTML()` — full block rendering with successors
  - Format: `b5 plain → b6` or `b5 if v42 → b6 b7`
  - Includes: block kind, control values, successor blocks (as clickable links)
  - Reference: Go `html.go` lines 1049-1051

- [ ] **2.5** Port `htmlFuncPrinter` — full function HTML rendering
  - `startBlock()`: `<ul class="b5 ssa-print-func">` + predecessors + collapse button
  - `value()`: `<li class="ssa-long-value">` + `LongHTML()`
  - `endBlock()`: successor info + `</ul>`
  - `named()`: named variable → value mappings
  - Dead code marking: `dead-value` / `dead-block` CSS classes
  - Reference: Go `html.go` lines 1189-1254

- [ ] **2.6** Port `Func.HTML()` — top-level function rendering
  - Wrap in `<code>` tags
  - Call htmlFuncPrinter to emit all blocks and values
  - Reference: Go `html.go` lines 1053-1065

### Phase 3: Hash Computation
**Reference:** `references/go/src/cmd/compile/internal/ssa/print.go`

- [ ] **3.1** Port `hashFunc()` — CRC32 of SSA text representation
  - Use `std.hash.CRC32` to hash the string output of the function
  - Must include dead values (for detecting deadcode pass changes)
  - Reference: Go `print.go` lines 20-25

- [ ] **3.2** Port `fprintFunc()` — functional printer interface
  - Abstract printer that can output to string, HTML, or hash
  - Called by both text dump and HTML rendering
  - Reference: Go `print.go` lines 28-89

### Phase 4: Driver Integration
**Reference:** `references/go/src/cmd/compile/internal/ssagen/ssa.go:44-77`, `compile.go:58-145`

- [ ] **4.1** Add `COT_SSA` env var parsing in `compiler/driver.zig`
  - Read `COT_SSA` env var (function name filter)
  - Support package-qualified names: `COT_SSA=mymod.myFunc`
  - Store on Driver struct: `ssa_html_func: ?[]const u8`
  - Reference: Go `ssagen/ssa.go` lines 44-77

- [ ] **4.2** Create HTMLWriter when function name matches
  - In compilation loop, check if current function matches `ssa_html_func`
  - If match: create HTMLWriter, write header, write source column
  - Output file: `{funcname}.ssa.html` in CWD (or `COT_SSA_DIR` if set)
  - Reference: Go `ssagen/ssa.go` lines 379-390

- [ ] **4.3** Hook WritePhase after each SSA pass (Wasm path)
  - After each pass in `driver.zig:6227-6236`, call `html_writer.writePhase(pass_name)`
  - Capture timing: `std.time.nanoTimestamp()` before/after each pass
  - Include timing in phase title: `"copyelim <span class='stats'>[1234 ns]</span>"`
  - Reference: Go `compile.go` lines 58-145

- [ ] **4.4** Hook WritePhase after each SSA pass (Native path)
  - Same as 4.3 but for `driver.zig:1549-1554`

- [ ] **4.5** Close HTMLWriter after codegen
  - Call `html_writer.close()` after all passes complete
  - Print path to stderr: `"dumped SSA for {name} to {path}"`

- [ ] **4.6** Add `--ssa` CLI flag as alternative to env var
  - `cot build file.cot --ssa=funcname` equivalent to `COT_SSA=funcname`
  - Parse in `compiler/cli.zig`

### Phase 5: Cot-Specific Enhancements
**Beyond Go — features specific to Cot's architecture**

- [ ] **5.1** Type display — show Cot types, not just indices
  - Values display full type name: `v42 = add <i64>` not `v42 = add <type:5>`
  - String, slice, optional types shown with element types
  - Requires passing TypeRegistry to HTML renderer

- [ ] **5.2** Compound value tracking
  - String values show both ptr and len components
  - Highlight compound_len_locals alongside primary values
  - Visual indicator for decomposed compound types

- [ ] **5.3** Wasm-specific annotations
  - After lower_wasm: show Wasm opcode names (wasm_i64_add, wasm_f64_load)
  - Show function indices for call targets
  - Show memory offsets for load/store operations

- [ ] **5.4** Source line cross-referencing
  - Click a source line → highlight all SSA values from that line
  - Click an SSA value → highlight the source line it came from
  - Uses `Value.pos` (Pos struct with line/col)

- [ ] **5.5** ARC visualization (native target only)
  - Highlight retain/release pairs
  - Show reference count flow
  - Mark values that trigger ARC operations

### Phase 6: Testing & Validation

- [ ] **6.1** Manual test: `COT_SSA=main cot build test/cases/arithmetic.cot`
  - Verify HTML opens in browser
  - Verify all passes appear as columns
  - Verify value highlighting works across passes
  - Verify dark mode toggle works

- [ ] **6.2** Manual test: `COT_SSA=main cot build test/cases/strings.cot`
  - Verify compound type decomposition is visible
  - Verify string_make/string_len operations shown correctly

- [ ] **6.3** Manual test with Wasm target
  - `COT_SSA=main cot build test/cases/arithmetic.cot --target=wasm`
  - Verify Wasm-specific passes appear (lower_wasm)
  - Verify Wasm opcodes shown after lowering

- [ ] **6.4** Manual test with native target
  - `COT_SSA=main cot build test/cases/arithmetic.cot`
  - Verify native-specific passes appear (lower_native)

- [ ] **6.5** Edge cases
  - Function with no passes (trivial function)
  - Function with many blocks (large switch statement)
  - Generic function (rewritegeneric visible)
  - Async function (state machine visible)

---

## Phase 7: Port to Selfcot (self-hosted compiler)

**This is the critical phase.** The Zig compiler proves the visualizer works. Selfcot makes it self-sustaining — the self-hosted compiler can debug itself.

### Why This Matters

Debugging selfcot is currently the #1 bottleneck. When selfcot generates wrong Wasm:
1. The bug is in selfcot's SSA passes or codegen
2. You can't use a debugger (selfcot runs as a native binary compiling to Wasm)
3. You can't add print statements easily (they're in Cot, not Zig)
4. Comparing selfcot output vs Zig compiler output requires manual diffing

With COT_SSA in selfcot:
1. `COT_SSA=funcname /tmp/selfcot build file.cot` generates HTML
2. `COT_SSA=funcname ./zig-out/bin/cot build file.cot --target=wasm` generates HTML
3. Open both side by side — the divergence IS the bug
4. Click a value → see exactly which pass corrupted it

### Selfcot Pipeline (matches Zig compiler exactly)

**Files:** `self/optimize/*.cot`, `self/emit/wasm/*.cot`

```
start → copyelim → decompose → copyelim → cse → deadcode →
schedule → layout → lower_wasm
```

**Driver:** `self/emit/wasm/driver.cot:379-388`

### Port Plan

- [ ] **7.1** Create `self/emit/html.cot` — HTMLWriter in Cot
  - Port the Zig `compiler/ssa/html.zig` to Cot syntax
  - Same struct, same methods, same HTML/CSS/JS template
  - Uses `@safe` mode (all struct params are auto-pointers)
  - String building via `StringBuilder` from `std/string`

- [ ] **7.2** Port hash computation
  - CRC32 hash of SSA text representation
  - Must match Zig compiler's format for comparison

- [ ] **7.3** Port SSA rendering (Func/Block/Value HTML)
  - `fn valueHTML(v: Value) string` → `<span class="v42 ssa-value">v42</span>`
  - `fn valueLongHTML(v: Value) string` → full value with op, type, args
  - `fn blockHTML(b: Block) string` → `<span class="b5 ssa-block">b5</span>`
  - `fn funcHTML(f: Func) string` → full function rendering

- [ ] **7.4** Hook into selfcot driver
  - Read `COT_SSA` env var in `self/emit/wasm/driver.cot`
  - After each pass, call `htmlWriter.writePhase(name)`
  - Write HTML file on close

- [ ] **7.5** Port source column
  - Read source file text
  - Generate line-numbered source column
  - Cross-reference with Value.pos

- [ ] **7.6** Test: Compare Zig vs selfcot HTML output
  - Build same function with both compilers using COT_SSA
  - HTML should show identical SSA at each pass
  - Any difference reveals a selfcot bug

### Selfcot-Specific Features

- [ ] **7.7** Dual-compiler diff view
  - New mode: `COT_SSA_DIFF=funcname` runs BOTH compilers
  - Generates a single HTML with Zig output on left, selfcot on right
  - Values that differ are highlighted in red
  - This is the killer feature for self-hosting debugging

- [ ] **7.8** Wasm bytecode column
  - After all SSA passes, show the final Wasm bytecode
  - Disassembly format: `i64.const 42`, `i64.add`, `call $alloc`
  - Cross-reference Wasm instructions back to SSA values

---

## Phase 8: Documentation & Polish

- [ ] **8.1** Add `COT_SSA` to CLAUDE.md debugging section
- [ ] **8.2** Add `--ssa` flag to CLI help text
- [ ] **8.3** Update `compiler/pipeline_debug.zig` to reference COT_SSA for visual debugging
- [ ] **8.4** Add usage examples to TROUBLESHOOTING.md

---

## File Inventory (What Gets Created/Modified)

### New Files
| File | Size Est. | Purpose |
|------|-----------|---------|
| `compiler/ssa/html.zig` | ~800 lines | HTMLWriter + CSS + JS + rendering |
| `self/emit/html.cot` | ~600 lines | Selfcot HTMLWriter (Phase 7) |

### Modified Files
| File | Changes |
|------|---------|
| `compiler/driver.zig` | Add COT_SSA env var check, HTMLWriter creation, WritePhase hooks |
| `compiler/cli.zig` | Add `--ssa=funcname` flag |
| `compiler/ssa/func.zig` | Add `html_writer: ?*HTMLWriter` field |
| `self/emit/wasm/driver.cot` | Add COT_SSA hooks (Phase 7) |

---

## Implementation Order

```
Phase 1 (HTMLWriter core)     ← Foundation: HTML template, CSS, JS
Phase 2 (SSA rendering)       ← Make SSA beautiful: values, blocks, types
Phase 3 (Hash computation)    ← Smart: skip unchanged passes
Phase 4 (Driver integration)  ← Wire it up: env var, pass hooks, file output
Phase 5 (Cot enhancements)    ← Beyond Go: types, compounds, Wasm opcodes
Phase 6 (Testing)             ← Prove it works on real code
Phase 7 (Selfcot port)        ← THE CRITICAL PHASE: self-hosted debugging
Phase 8 (Documentation)       ← Polish and ship
```

**Phase 1-4 delivers a working visualizer.** Phase 5 makes it world-class. Phase 7 makes it self-sustaining.

---

## Success Criteria

1. `COT_SSA=main cot build file.cot` produces a valid HTML file
2. HTML shows all SSA passes as collapsible columns
3. Clicking a value highlights ALL occurrences across ALL passes
4. Dead code is visually faded
5. Dark mode works
6. Unchanged passes are batched into single columns
7. Per-pass timing is displayed
8. Source code column with line cross-referencing
9. Works for both Wasm and native targets
10. Selfcot produces identical output to Zig compiler for the same input
