# Cot Wasm Async Architecture — Coroutines, State Machines, and the JS Bridge

**Date:** 2026-03-22
**Purpose:** Permanent reference for async/coroutine implementation on Wasm. Documents the architectural decisions, compares with Go/Rust/Kotlin/Zig, and defines the path forward.

---

## The Three Industry Approaches

Every language targeting Wasm faces the same problem: **Wasm cannot partially unwind its call stack.** When a coroutine needs to suspend, there is no Wasm instruction to save the current call stack and resume it later. Three solutions exist:

### Approach 1: Runtime Stack Unwinding (Go)

**Every function has signature `(i32)->i32`.** All data passes through linear memory via a stack pointer.

When a goroutine suspends:
1. The suspending function sets `PAUSE=1` (global variable) and returns `1` (exit-immediately flag)
2. Every caller checks the return value — if `1`, immediately returns `1` itself
3. This propagates up the ENTIRE Wasm call stack until reaching the dispatch loop
4. The dispatch loop sees `PAUSE=1`, stops, and returns control to JS
5. Later, JS calls `_resume()`, the dispatch loop restarts, and the goroutine resumes at its saved block ID (via br_table)

**Why Go needs this:** Go's goroutines can suspend at ANY point (implicit suspension) — inside runtime code, inside library calls, anywhere. The language makes no distinction between sync and async code. Any function might trigger a goroutine switch.

**Cost:** All type information is lost. All data goes through memory. Every function call has dispatch overhead. Complex runtime (wasm_exec.js is 575 lines).

**Reference:** `references/go/src/cmd/compile/internal/wasm/ssa.go` lines 19-130, `references/go/lib/wasm/wasm_exec.js` lines 225-575.

### Approach 2: Compile-Time State Machines (Rust, Kotlin, Cot)

**Normal typed function signatures.** Async functions are transformed to state machine structs at compile time.

When a coroutine suspends:
1. The state machine stores its current state (which await point) and any live locals into a heap-allocated struct
2. Returns `Pending` / `COROUTINE_SUSPENDED` / `0` — a normal return value
3. No stack unwinding — just a regular function return
4. The caller (or event loop) calls `poll()` again later
5. The state machine switches on the saved state and resumes from that point

**Why this works:** Suspension only happens at explicit `await` sites. The compiler knows every possible suspension point at compile time and generates the state transitions. No runtime magic needed.

**Cost:** Code size grows (state machine dispatch per async function). Heap allocation for each future/coroutine. Cannot suspend in non-async code.

**Rust reference:** `references/rust/compiler/rustc_mir_transform/src/coroutine.rs`, `references/rust/library/core/src/future/future.rs` lines 37-114.

**Kotlin reference:** `references/kotlin/compiler/ir/backend.common/src/org/jetbrains/kotlin/backend/common/lower/AbstractSuspendFunctionsLowering.kt`, `references/kotlin/libraries/stdlib/wasm/src/kotlin/coroutines/CoroutineImpl.kt`.

### Approach 3: Don't Do It (Zig)

**Zig removed async/await entirely** in 0.12+. It was too complex for the benefit, and no backend (LLVM, Wasm) provided adequate primitives.

**Reference:** `references/zig/src/Zcu.zig:4427` — `async => return .{ .bad_backend = backend }`, `references/zig/src/Sema.zig:2491` — `"async has not been implemented in the self-hosted compiler yet"`.

### Future: Wasm 3.0 Stack Switching (Phase 3 Proposal)

Wasm has a stack switching proposal (`cont.new`, `cont.bind`, `resume`, `suspend`, `barrier`) that would enable native coroutines without state machines or stack unwinding. Chrome has JSPI (JS Promise Integration) as a stopgap. Wasmtime has experimental support.

**When this stabilizes (~2027+), Cot could simplify its state machine approach.** But the compile-time state machine works today on all runtimes.

**Reference:** `claude/specs/WASM_3_0_REFERENCE.md` lines 311-318.

---

## Why Cot Uses Approach 2 (Not Go's Approach)

### Go's `(i32)->i32` Is NOT Required for Coroutines

Go's calling convention is driven by its **implicit suspension model** — any Go function can trigger a goroutine switch at any time. This requires the ability to unwind from ANY point in the call stack, which forces the uniform `(i32)->i32` signature with memory-based parameter passing.

Cot, Rust, and Kotlin use **explicit suspension** — only `await` / `.await` / `suspend` sites can suspend. The compiler transforms these at compile time. No stack unwinding needed.

### Cot's Calling Convention Is Correct

Cot uses typed i64 parameters on the Wasm stack (not Go's memory-based passing). This is the same approach as Rust and Kotlin. It preserves type information, is more efficient, and works perfectly with compile-time state machines.

### Comparison Table

| Aspect | Go | Rust | Kotlin | Cot |
|--------|----|----|--------|-----|
| Function signature | `(i32)->i32` uniform | Normal typed | Normal typed | Normal typed (i64) |
| Suspension model | Implicit (anywhere) | Explicit (`.await`) | Explicit (`suspend`) | Explicit (`await`) |
| Transformation | Runtime stack unwind | Compile-time state machine | Compile-time CPS + state machine | Compile-time state machine |
| Parameter passing | Linear memory (SP) | Wasm stack | Wasm stack | Wasm stack |
| Stack unwinding | Yes (full unwind) | No | No | No |
| Type preservation | Lost (all i32) | Preserved | Preserved | Preserved (all i64) |
| JS bridge complexity | High (575 LOC) | Low (wasm-bindgen) | Low (CPS transform) | Low (typed imports) |
| Can suspend in non-async code | Yes | No | No | No |

---

## Cot's Current Async Implementation

### Syntax (Complete)

```cot
async fn fetch(url: string) !string {
    const response = await http_get(url)
    return response.body
}

// Await works in both async and sync contexts (no function coloring)
const result = await fetch("https://example.com")
```

**Parser:** `compiler/frontend/parser.zig` lines 220, 248-251, 1049-1053
**Tokens:** `kw_async`, `kw_await`, `kw_spawn`, `kw_select` — `compiler/frontend/token.zig` line 38
**AST:** `FnDecl.is_async`, `AwaitExpr`, `SpawnExpr`, `SelectExpr` — `compiler/frontend/ast.zig` lines 56, 102-104

### Type System (Complete)

```
async fn foo() T  →  returns Future(T)
await future      →  extracts T from Future(T)
```

**Future type:** `FutureType = { result_type: TypeIndex }` — `compiler/frontend/types.zig` line 75
**Size:** 8 bytes (pointer to heap-allocated state struct)
**Type checking:** `compiler/frontend/checker.zig` lines 3145-3156

**Design decision** (checker.zig line 3152): "await is allowed both in async and sync contexts. In sync context, it blocks until the future completes (like block_on). No function coloring."

### Native Target: Eager Evaluation (Complete, Working)

**File:** `compiler/frontend/lower.zig` lines 952-1082

The native path uses **eager execution** — the async function body runs to completion immediately when called. No state machine, no suspension.

```
async fn compute(x: i64) i64 { return x * 2 }

// Compiles to:
fn compute_body(x: i64) i64 { return x * 2 }  // normal function
fn compute(x: i64) *Future {
    state = alloc(Future)    // heap allocate
    result = compute_body(x) // call body EAGERLY (blocks)
    state.result = result    // store result
    state.state = 1          // mark DONE
    return state
}

// await just extracts the result:
await compute(5)  →  state = compute(5); return state.result
```

**19 tests pass** — `test/e2e/async.cot` (199 lines)

### Wasm Target: Poll-Based State Machine (Partial)

**File:** `compiler/frontend/lower.zig` lines 805-950

The Wasm path transforms async functions into two functions:

1. **Constructor** (`fn_name`): Allocates heap state struct, stores params, returns Future pointer
2. **Poll function** (`fn_name_poll`): Takes state pointer, runs body, returns 0 (PENDING) or 1 (READY)

**State struct layout:**
```
[0..7]:   state (i64) — 0=initial, 1=done
[8..8+N]: result (size depends on return type)
[8+N..]:  params (captured from constructor call)
```

**What works:**
- Single-await async functions compile and run correctly
- Poll function executes body and returns READY
- JS glue detects `_poll` exports and wraps as Promises

**What is incomplete:**
- **Multiple awaits in one function** — `countAwaitPoints()` counts them (line 808) but the state machine doesn't track which await we're at. Poll re-runs the entire body each time.
- **No state preservation between yields** — locals alive across await points are not saved to the state struct
- **No suspend/resume at Wasm level** — poll runs body to completion, doesn't actually yield mid-execution

### What Needs to Be Built (Rust/Kotlin Pattern)

To support multiple awaits:

```cot
async fn fetch_both() string {
    const a = await fetch("url1")  // await point 0
    const b = await fetch("url2")  // await point 1
    return a + b
}
```

The poll function should become:

```
fn fetch_both_poll(state: *State) i64 {
    switch (state.pc) {
        0 => {
            // Start fetch("url1"), get inner future
            state.inner_future = fetch("url1")
            state.pc = 1
            return 0  // PENDING
        }
        1 => {
            // Poll inner future
            if (fetch_poll(state.inner_future) == 1) {
                state.a = state.inner_future.result
                // Start fetch("url2")
                state.inner_future = fetch("url2")
                state.pc = 2
                return 0  // PENDING
            }
            return 0  // inner still pending
        }
        2 => {
            if (fetch_poll(state.inner_future) == 1) {
                state.b = state.inner_future.result
                state.result = state.a + state.b
                state.state = 1  // DONE
                return 1  // READY
            }
            return 0  // inner still pending
        }
    }
}
```

**Required changes:**
1. Track `pc` (program counter) in state struct — which await point we're at
2. Save locals alive across await points into state struct fields
3. Generate switch dispatch in poll function on `pc`
4. Each await point becomes a state transition: save state → return PENDING
5. On re-entry, restore state → check inner future → continue or stay PENDING

**Reference implementations:**
- Rust: `references/rust/compiler/rustc_mir_transform/src/coroutine.rs` — full state machine generation
- Kotlin: `references/kotlin/compiler/ir/backend.common/src/org/jetbrains/kotlin/backend/common/lower/AbstractSuspendFunctionsLowering.kt` — CPS + state machine

---

## Stdlib Event Loop

**File:** `stdlib/async.cot` (232 lines)

Platform-abstracted event loop using kqueue (macOS) / epoll (Linux):

```cot
fn eventLoopCreate() i64        // kqueue_create / epoll_create
fn watchRead(epfd, fd) i64      // kevent_add / epoll_add for read events
fn watchWrite(epfd, fd) i64     // kevent_add / epoll_add for write events
fn eventLoopWait(epfd, timeout) i64  // kevent_wait / epoll_wait

async fn asyncAccept(epfd, fd) IoError!i64   // non-blocking accept + poll loop
async fn asyncRead(epfd, fd, buf, len) IoError!i64
async fn asyncWrite(epfd, fd, buf, len) IoError!i64
async fn asyncConnect(epfd, fd, addr, port) IoError!i64
```

**Current limitation:** `eventLoopWait()` blocks the entire program. No concurrent task execution. Suitable for single-task I/O but not for multiplexing multiple coroutines.

---

## Unimplemented Features

| Feature | AST Node | Status | Notes |
|---------|----------|--------|-------|
| `spawn { body }` | `SpawnExpr` | Stub only | Fire-and-forget task dispatch |
| `select { cases }` | `SelectExpr` | Type checking only | Go-style select over channels |
| Channels | — | Not in type registry | Required for select |
| Multi-await state machine | — | Counted but not generated | Core missing piece |
| Native fiber context switch | — | Comment only | `lower.zig:952` mentions "Phase D" |

---

## JS Bridge Integration

### Current State

The JS glue (`compiler/codegen/js_glue.zig`) auto-detects async exports:
- If Wasm exports both `foo` and `foo_poll`, wraps as JS Promise
- Poll loop calls `foo_poll(state)` until it returns 1 (READY)
- Result extracted from Wasm memory

### For Canvas2D / Browser APIs

Cot's typed-parameter imports (module `"env"`) are the correct approach:
- `extern fn canvas_fill_rect(ctx: i64, x: i64, y: i64, w: i64, h: i64) void`
- Compiles to Wasm import: `("env", "canvas_fill_rect") : (i64, i64, i64, i64, i64) -> ()`
- JS provides the implementation, receives BigInt values

This matches Rust (wasm-bindgen) and Kotlin (external fun) — NOT Go's SP-based pattern.

### Async JS Interop (Future Work)

When Cot needs to await a JS Promise (e.g., `fetch()`):
1. Cot calls extern fn that starts the async JS operation
2. JS stores a callback that will signal completion
3. Cot's poll function checks a shared memory flag
4. When JS Promise resolves, JS writes result to shared memory and sets flag
5. Next poll detects flag, extracts result, returns READY

This is exactly how Rust's wasm-bindgen-futures works.

---

## Self-Hosted Compiler (selfcot)

**File:** `self/build/lower.cot`

- Async parsing and type checking: complete
- `lowerAsyncFiber()`: ported from Zig compiler (native eager evaluation)
- `lowerAwaitExpr()`: ported (native path only)
- Wasm state machine: NOT ported (selfcot only targets native currently)
- `countAwaitPoints()`: ported

---

## Architecture Decision Record

### Decision: Use Compile-Time State Machines (Approach 2)

**Rationale:**
1. Cot uses explicit suspension (`await`) — compiler knows every suspension point
2. Matches Rust and Kotlin, both proven at scale on Wasm
3. Preserves typed calling convention (i64 params on Wasm stack)
4. No runtime complexity (no wasm_exec.js equivalent needed)
5. Forward-compatible with Wasm 3.0 stack switching (can simplify later)

**Rejected: Go's `(i32)->i32` (Approach 1)**
- Would require rewriting entire Wasm codegen
- Loses type information
- Adds runtime complexity
- Only needed for implicit suspension, which Cot doesn't have

**Rejected: No Async (Approach 3)**
- Cot targets full-stack web development — async I/O is essential
- Browser APIs are inherently async (fetch, setTimeout, requestAnimationFrame)

### Decision: Await Without Function Coloring

**Rationale:** `await` works in both async and sync contexts. In sync context, it blocks (like Rust's `block_on`). This avoids the "function coloring" problem where adding `await` to one function forces all callers to become async.

**Trade-off:** Cannot detect at compile time whether a sync function will block on an incomplete future. Runtime behavior differs between targets (native blocks, Wasm polls).
