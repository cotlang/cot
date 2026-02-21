# Wasm Upgrade Plan: Cot 0.4 Execution Plan

**Purpose:** Accurate status of Wasm feature adoption + concrete execution plan for remaining work toward Cot 0.4.

**Version:** Cot 0.3.1 | **Updated:** February 2026

---

## 1. Current State (Audited)

Cot emits Wasm 1.0 with cherry-picked 2.0 and 3.0 features. The following table reflects what's actually implemented and shipping, verified against the codebase.

| Feature | Status | Evidence |
|---------|--------|----------|
| `memory.copy` (0xFC 0x0A) | **DONE** | `gen.zig:1076-1087` — `.wasm_lowered_move` emits `memory_copy` directly (no loop) |
| `trunc_sat` (0xFC 0x00-07) | **DONE** | `constants.zig:393-401` — all 8 variants defined; `gen.zig:1054-1062` emits for float→int |
| Data count section (§12) | **DONE** | `link.zig:562-570` — conditional emission when data segments exist |
| `return_call` (0x12) tail calls | **DONE** | `lower_wasm.zig:353` maps `tail_call => .wasm_return_call`; peephole optimizer at lines 368-398 converts `call+return` patterns; `gen.zig:859-883` emits opcode 0x12 |
| Multi-value function types | **PARTIAL** | Types declared correctly in `link.zig`; compound returns (string ptr+len) use `compound_len_locals` workaround (`gen.zig:99-102`) with 5+ special cases |
| WasmGC struct ops | **~85% DONE** | `struct.new` (0xFB 0x00), `struct.get` (0xFB 0x02), `struct.set` (0xFB 0x05) all working; 12 tests in `test/e2e/wasmgc.cot`; missing: arrays, nested structs |
| `memory.fill` (0xFC 0x0B) | **DEFINED** | Opcode in `constants.zig:407`, decoded in `decoder.zig:827`, but never emitted by gen.zig |
| `call_indirect` (0x11) closures | **DONE** | `gen.zig:908-949` — closure calls with CTXT global + plain indirect calls; 4 E2E tests passing |

### What Changed Since the Original Plan

The original document (Feb 2026) listed Waves 1-2 as TODO. In fact:

- **Wave 1 (memory.copy, trunc_sat, data count):** All complete. `memory.copy` replaced the word-by-word loop. `trunc_sat` provides safe float→int. Data count section emits when needed.
- **Wave 2 (tail calls):** Complete with a peephole optimizer that automatically converts `call+return` to `return_call`. The original claim that `lower_wasm.zig:347` said "Wasm has no tail call (yet)" is outdated — it now maps directly to `wasm_return_call`.
- **WasmGC:** Not in the original plan at all, but substantially implemented (dual memory model decided and working).

---

## 2. Architecture Decision: Dual Memory Model

Cot uses a **dual memory model** — different memory management per target:

| Target | Flag | Memory Model | Struct Representation |
|--------|------|-------------|----------------------|
| **Wasm** | `--target=wasm32-gc` | **WasmGC** — browser GC manages struct lifetime | GC objects (`struct.new`, `struct.get`, `struct.set`) |
| **Native** | default | **ARC** — retain/release with unified cleanup stack | Linear memory with deterministic destruction |
| **Both** | — | **Linear memory** for raw allocations (`@alloc`), strings, slices | Pointer + length decomposition at ABI boundary |

**Rationale:** Browser engines already have a high-performance GC. Shipping ARC to the browser adds overhead for no benefit. Conversely, native executables benefit from deterministic destruction without GC pauses.

**Precedent:** Kotlin/Wasm uses the same dual approach — WasmGC for objects on browser, native GC (or manual) on other targets.

**Implementation:**
- `target.zig`: `gc` field, `isWasmGC()` method
- `link.zig`: GC struct types (0x5F) at indices 0..N-1, func types offset by N
- `gen.zig:1001-1037`: `wasm_gc_struct_new`, `wasm_gc_struct_get`, `wasm_gc_struct_set`
- GC ref locals initialized with `struct.new_default` (0xFB 0x01) at function entry, before dispatch loop

---

## 3. Remaining Execution Plan for 0.4

Five phases, ordered by complexity and dependency. Phases 1-2 are quick wins; Phase 3 eliminates tech debt; Phases 4-5 are advanced Wasm 3.0 adoption.

### Phase 1: WasmGC Completion (struct + array operations)

**Goal:** Complete WasmGC so all Cot types (not just flat structs) work on `--target=wasm32-gc`.

**Current gap:** `struct.new`/`get`/`set` work for flat structs. Missing: GC arrays for `List(T)` and `[]T`, nested structs (struct fields referencing other GC structs).

| Task | Files | Details |
|------|-------|---------|
| `array.new` / `array.get` / `array.set` / `array.len` | `constants.zig`, `gen.zig`, `link.zig` | GC arrays for List(T) backing storage. Opcodes: 0xFB 0x06 (`array.new`), 0xFB 0x0B (`array.get`), 0xFB 0x0E (`array.set`), 0xFB 0x0F (`array.len`) |
| Nested struct support | `gen.zig`, `link.zig` | Struct fields with `(ref null $typeidx)` — struct.get returns a ref, not i64 |
| GC-aware method dispatch | `lower.zig`, `gen.zig` | Method receivers: pass ref directly, don't `emitAddrLocal` (partially done) |

**Reference:** Kotlin/Wasm array lowering, wasmtime GC test suite (`tests/misc_testsuite/gc/`).

**Verification:** Extend `test/e2e/wasmgc.cot` with array creation, access, List(T) operations, nested struct tests. Run with `--target=wasm32-gc` via wasmtime.

---

### Phase 2: `memory.fill` Wiring (quick win)

**Goal:** Use `memory.fill` (0xFC 0x0B) for zero-initialization instead of manual loops.

**Current state:** Opcode defined in `constants.zig:407`, decoded in `decoder.zig:827`, but never emitted. Zero-init paths in `arc.zig` (allocator) and struct default init don't use it.

| Task | Files | Details |
|------|-------|---------|
| Wire `memory.fill` into zero-init | `gen.zig`, `arc.zig` | Where `@alloc` zeroes memory or structs are default-initialized, emit `memory.fill` (dest, 0x00, size) instead of loop |
| Add SSA→Wasm lowering | `lower_wasm.zig` | Map zero/fill ops to `wasm_lowered_fill` → emit `memory.fill` |

**Verification:** `./test/run_all.sh` — all allocation-heavy tests (list, map, set, json) must pass.

---

### Phase 3: Multi-Value Return Cleanup (eliminate compound workarounds)

**Goal:** Remove `compound_len_locals` workaround. Compound returns (string ptr+len) should use Wasm multi-value returns natively.

**Current state:** Functions returning `string` already declare multi-result types in `link.zig`. But `gen.zig` uses a `compound_len_locals` map (`gen.zig:99-102`) to store the second return value in a separate local, with 5+ special-case code paths:
- `gen.zig:1190-1206` — call sites: pop len to separate local
- `gen.zig:366-379` — `string_ptr`/`string_len` extraction ops
- `gen.zig:224-233` — return handler: push both from locals
- Return value materialization for nested calls

| Task | Files | Details |
|------|-------|---------|
| Remove `compound_len_locals` map | `gen.zig` | Both return values stay on Wasm stack; callers destructure with `local.set` pairs |
| Simplify `.ret` handler | `gen.zig:202-233` | Push N values for N-return function, emit `aret`. No special cases. |
| Remove extraction op special cases | `gen.zig:354-379` | `string_ptr`/`string_len` become standard stack operations |
| Update native pipeline | `driver.zig`, `ssa_builder.zig` | Verify multi-return decoding/translation handles clean multi-value |

**Risk:** HIGH — 5 special cases in gen.zig, each a potential regression. All stdlib modules (string, json, fs) exercise compound returns.

**Verification:** Full test suite — `cot test test/e2e/features.cot` (native + wasm32), all stdlib tests, `./test/run_all.sh`.

---

### Phase 4: `call_ref` for Closures (Wasm 3.0 typed function references)

**Goal:** Replace `call_indirect` + function table with `call_ref` (0x14) — direct typed call, no table overhead.

**Current state:** Closures use `call_indirect` (0x11) with a function table and CTXT global (`gen.zig:908-949`). This requires:
1. Function table in element section
2. Runtime type check on every indirect call
3. `i32_wrap_i64` to convert table index

`call_ref` eliminates the table entirely — closures carry a typed function reference.

| Task | Files | Details |
|------|-------|---------|
| Add `call_ref` opcode | `constants.zig` | `call_ref = 0x14`, `ref.func`, `ref_as_non_null` (0xD4) |
| Extend type section | `link.zig` | Emit `(ref $fn_type)` for each closure signature |
| Replace closure codegen | `gen.zig:908-931` | `call_indirect` → `call_ref`; closure creation: `ref.func` instead of table index |
| Parse/decode in native pipeline | `wasm_parser.zig`, `decoder.zig`, `translator.zig` | Decode 0x14, translate to CLIF indirect call |

**Reference:** wasmtime `cranelift/src/translate/` — `call_ref` handling in `translate_operator.rs`.

**Verification:** Closure tests in `test/e2e/features.cot` (4 tests: no-capture, single/multi-capture, passed-as-arg). Both native and wasm32 targets.

---

### Phase 5: Exception Handling (Wasm 3.0, highest complexity)

**Goal:** Use Wasm 3.0 exceptions (`throw`/`try_table`) for error propagation and reliable `defer` across call boundaries.

**Current state:** Error returns construct an error union struct (tag + value) and return it. Callers check the tag with `br_if`. `defer` runs at function exit via explicit calls before `return` — if a called function traps, defer doesn't run.

**Design:** Single generic `$cot_error` tag with `(error_set_id: i32, error_value: i32)` payload. Matches Go/Zig's "errors are values" philosophy. One tag is simpler than per-error-set tags and sufficient for Cot's error model.

| Task | Files | Details |
|------|-------|---------|
| Add exception opcodes | `constants.zig` | `throw` (0x08), `throw_ref` (0x0A), `try_table` (0x1F) |
| Add tag section | `link.zig` | Section 13 after global section, before export section |
| Error return → `throw` | `lower.zig`, `gen.zig` | On error path, emit `throw $cot_error` instead of struct construction + return |
| `try` expr → `try_table` | `lower.zig`, `gen.zig` | Wrap call in `try_table (catch $cot_error $handler)`, success value stays on stack |
| Defer-across-calls | `lower.zig`, `gen.zig` | Functions with `defer`: body wrapped in `try_table (catch_all_ref $cleanup)`, cleanup runs defers then `throw_ref` to re-propagate |
| Native pipeline | `wasm_parser.zig`, `decoder.zig`, `translator.zig` | Decode new opcodes, translate to CLIF exception handling |

**Impact:** Rewrites error return ABI. Every function returning `error!T` changes calling convention. Touches entire pipeline.

**Risk:** HIGHEST — ABI change affects all error-returning functions, stdlib error paths, and the native pipeline.

**Reference:** Go error handling → Wasm exception mapping. wasmtime EH tests (`tests/misc_testsuite/exception-handling/`).

**Verification:** Error handling tests, defer tests, stdlib error paths (fs, json). Both native and wasm32.

---

## 4. Post-0.4 Tracking

Features to track but not implement until their target version.

| Feature | Target | Wasm Status | Cot Use Case |
|---------|--------|-------------|--------------|
| Threads/atomics | 0.5 | Shipping all browsers (Phase 4) | Multi-threaded server, atomic ARC refcount |
| Stack switching | 0.5 | Phase 3, ~2026-2027 | `async fn` / `await` on Wasm target |
| Component model | 0.6 | Active dev (WASI 0.3) | Cross-language module interop, package system |
| Memory64 | 0.7+ | Shipping (no Safari) | >4GB server workloads |
| Branch hinting | 0.7+ | Standardized (Phase 5) | Cold-path optimization (`@branchHint`) |
| Wide arithmetic | 0.7+ | Phase 3 | BigInt, crypto 128-bit intermediates |

**Architecture note:** The current Wasm-as-IR pipeline (Cot → SSA → Wasm → {browser | AOT native via CLIF}) works for all features through 0.4. Only **stack switching** (async) may force an IR split (`lower_clif.zig`: SSA → CLIF directly), since stack switching is post-Wasm-3.0 and may not be expressible through Wasm bytecode. This split is anticipated for 0.5.

---

## 5. Reference Traceability

Every feature traced to its reference implementation.

| Feature | Reference | Location |
|---------|-----------|----------|
| `memory.copy` / `memory.fill` | Go `cmd/compile/internal/wasm` | `references/go/src/cmd/internal/obj/wasm/wasmobj.go` |
| `trunc_sat` | Go Wasm codegen | `references/go/src/cmd/compile/internal/wasm/` |
| `return_call` tail calls | Go SSA → Wasm lowering | `references/go/src/cmd/compile/internal/ssa/rewriteWasm.go` |
| `call_ref` typed refs | wasmtime Cranelift translate | `references/wasmtime/crates/cranelift/src/translate/` |
| WasmGC struct/array ops | Kotlin/Wasm + wasmtime GC tests | `references/wasmtime/tests/misc_testsuite/gc/` |
| Exception handling (`throw`/`try_table`) | wasmtime EH tests + Go error model | `references/wasmtime/tests/misc_testsuite/exception-handling/` |
| CLIF → ARM64 lowering | Cranelift aarch64 backend | `references/wasmtime/cranelift/codegen/src/isa/aarch64/` |
| CLIF → x64 lowering | Cranelift x64 backend | `references/wasmtime/cranelift/codegen/src/isa/x64/` |
| Error semantics | Zig error unions, `errdefer` | Zig language reference |
| Dual memory model | Kotlin/Wasm (WasmGC + linear) | Kotlin/Wasm documentation |

---

## Summary

| Phase | Features | Status | Risk | Effort |
|-------|----------|--------|------|--------|
| **1** | WasmGC arrays + nested structs | Remaining | Medium | 3-4 days |
| **2** | `memory.fill` wiring | Remaining | Low | Hours |
| **3** | Multi-value return cleanup | Remaining | High | 2-3 days |
| **4** | `call_ref` typed function refs | Remaining | Medium | 3-4 days |
| **5** | `throw` / `try_table` exceptions | Remaining | Highest | 1-2 weeks |

**Already complete:** `memory.copy`, `trunc_sat` (8 variants), data count section, `return_call` tail calls (with peephole optimizer), WasmGC struct ops (12 tests), closures via `call_indirect`.

**Phases 1-4 total:** ~9-11 days of focused work.
**Phase 5 additional:** ~1-2 weeks (ABI-breaking change, full pipeline impact).

---

## Reference Documents

- `claude/specs/WASM_3_0_REFERENCE.md` — Wasm 3.0 opcodes and adoption priorities
- `claude/specs/wasm-3.0-full.txt` — Full Wasm 3.0 specification (25K lines)
- `claude/ROADMAP_1_0.md` — Road to 1.0, version trajectory, feature waves
- `claude/PIPELINE_ARCHITECTURE.md` — Full compiler pipeline reference map
