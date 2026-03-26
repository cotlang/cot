# Concurrency Implementation Execution Plan

**Date:** 2026-03-26
**Prerequisite:** 0.4 release (version bump + CI green)
**Design doc:** `claude/SWIFT_CONCURRENCY_PORT.md` (1,971 lines, audited)
**Status:** Phase 0 COMPLETE (6,156 lines deleted) + Phase 1 COMPLETE (async/await E2E on native)

---

## Current State of the Compiler

All Go-style concurrency code has been deleted. What remains:

| Item | Status | Location |
|------|--------|----------|
| `kw_async`, `kw_await` keywords | Reserved, parsed | token.zig, parser.zig |
| `FnDecl.is_async` field | Parsed, not processed | ast.zig:56 |
| `AwaitExpr` AST node | Parsed, checker rejects with error | ast.zig:400 |
| `parseAsyncFn()` | Routes to parseFnDecl with is_async=true | parser.zig:260 |
| `lowerAwaitExpr()` | Stub returning null_node | lower.zig:11869 |
| `Atomic(T)` | Preserved in stdlib/atomic.cot | stdlib/atomic.cot |
| Trait infrastructure | Fully operational (TraitDef, ImplTraitBlock) | checker.zig:145-157 |
| ExistentialType | Fully operational (any Trait) | types.zig:78-85 |
| ARC (retain/release) | Fully operational | arc_native.zig |
| VWT (value witnesses) | Fully operational, 475 types | vwt_gen.zig |

---

## Phase 1: Task Runtime (2-3 days) — COMPLETE

**Goal:** A `Task` is a heap-allocated ARC object that holds a function to execute and a result slot. A cooperative executor runs tasks in a priority queue. `async fn` returns a Task, `await` polls it.

**Status:** DONE. 3/3 tests pass on native. Also delivered block-scoped fn declarations (IR builder nesting). Wasm deferred to Phase 9.

### Step 1.1: Task Type in Type System

**File:** `compiler/frontend/types.zig`

Add after `ExistentialType` (line ~85):
```zig
pub const TaskType = struct { result_type: TypeIndex };
```

Add to `Type` union (after `.existential`):
```zig
    task: TaskType,
```

Add `sizeOf` case (task is a pointer — 8 bytes):
```zig
    .task => 8,
```

Add to all other switch exhaustiveness points: `alignOf`, `isTrivial` (false — ARC managed), `couldBeARC` (true), `equal`, format string.

Add `makeTask()` method to `TypeRegistry`:
```zig
pub fn makeTask(self: *TypeRegistry, result_type: TypeIndex) !TypeIndex {
    for (self.types.items, 0..) |t, i| {
        if (t == .task and t.task.result_type == result_type) return @intCast(i);
    }
    return self.add(.{ .task = .{ .result_type = result_type } });
}
```

**Swift reference:** Task is `struct Task<Success: Sendable, Failure: Error>` with `internal let _task: Builtin.NativeObject` (Task.swift:143-145). For Cot Phase 1, we model Task as a single pointer (like a managed heap object).

**Test:** `zig build test` passes.

### Step 1.2: Task Object Layout (Runtime)

The Task object lives on the heap, managed by ARC.

```
TaskObject layout (native, 48 bytes):
  Offset  0: refcount      (i64) — ARC reference count
  Offset  8: status         (i64) — 0=pending, 1=running, 2=completed, 3=cancelled
  Offset 16: fn_ptr         (i64) — pointer to the async function body
  Offset 24: context_ptr    (i64) — pointer to captured environment (or null)
  Offset 32: result         (i64) — result value (for simple types)
  Offset 40: error          (i64) — error value (for error unions)
```

For Wasm, same layout in linear memory.

**This is simpler than Swift's full TaskObject** (which has PrivateStorage, fragments, etc). We start minimal and add fields as needed in later phases. The key property: it's an ARC heap object, so retain/release just work.

### Step 1.3: Cooperative Executor

**File:** New file `compiler/codegen/native/executor_native.zig`

The executor is a priority queue of tasks. It runs on the current thread (cooperative, not preemptive).

**Runtime functions to generate as CLIF IR:**

| Function | Signature | Purpose |
|----------|-----------|---------|
| `task_create(fn_ptr, ctx_ptr) → task_ptr` | `(i64, i64) → i64` | Alloc TaskObject, set fn_ptr + context, status=pending |
| `task_poll(task_ptr) → status` | `(i64) → i64` | If pending: run fn_ptr(ctx_ptr), store result, set completed. Return status. |
| `task_get_result(task_ptr) → result` | `(i64) → i64` | Load result from offset 32 |
| `task_cancel(task_ptr)` | `(i64) → void` | Set status=cancelled |
| `task_is_cancelled(task_ptr) → bool` | `(i64) → i64` | Load status, return status==3 |

Phase 1 executor is trivially simple — `task_poll` just calls the function immediately (eager evaluation, like the old native async). The cooperative polling loop comes in Phase 2 when we have suspension points.

**Swift reference:** `CooperativeExecutor.swift` — PriorityQueue + run loop. We defer the full priority queue to Phase 2; Phase 1 just needs create/poll/get_result.

**Registration in driver.zig:** Add `"task_create", "task_poll", "task_get_result", "task_cancel", "task_is_cancelled"` to `runtime_func_names` array. Import and call `executor_native.generate()` alongside the existing runtime modules.

### Step 1.4: Wasm Runtime Equivalents

**File:** `compiler/codegen/wasi_runtime.zig`

Add the same 5 functions as Wasm module functions. Implementation:
- `task_create`: `alloc(48)`, store fields, return pointer
- `task_poll`: load fn_ptr from offset 16, `call_indirect`, store result at offset 32, set status=2
- `task_get_result`: `i64.load offset=32`
- `task_cancel`: `i64.store offset=8 value=3`
- `task_is_cancelled`: `i64.load offset=8`, `i64.eq` with 3

Register in `func_indices` map in driver.zig.

### Step 1.5: Checker — async fn Returns Task(T)

**File:** `compiler/frontend/checker.zig`

In `checkFnDeclWithName` (line ~807), after building the function type:

```zig
// If the function is async, wrap the return type in Task(T)
if (f.is_async) {
    const inner_ret = self.types.get(func_type).func.return_type;
    const task_ret = try self.types.makeTask(inner_ret);
    func_type = try self.types.makeFunc(
        self.types.get(func_type).func.params, task_ret
    );
}
```

In `checkAwaitExpr` (currently returns invalid_type), replace with:

```zig
fn checkAwaitExpr(self: *Checker, ae: ast.AwaitExpr) CheckError!TypeIndex {
    const operand_type = try self.checkExpr(ae.operand);
    const operand_info = self.types.get(operand_type);
    if (operand_info != .task) {
        self.err.errorWithCode(ae.span.start, .e300, "cannot await non-task type");
        return invalid_type;
    }
    return operand_info.task.result_type;
}
```

### Step 1.6: Lowerer — async fn and await

**File:** `compiler/frontend/lower.zig`

**async fn lowering** — In `lowerFnDecl` (line ~708), before the standard function lowering:

```zig
if (fn_decl.is_async) {
    // Generate two functions:
    // 1. The body function: fn_name_body(context_ptr) → result
    // 2. The wrapper: fn_name(params...) → Task(T) — creates task, returns it
    try self.lowerAsyncFnDecl(fn_decl);
    return;
}
```

New function `lowerAsyncFnDecl`:
1. Lower the original body as `{qualified_name}_body(ctx_ptr: i64) → result_type`
   - Load params from the context struct (like closure capture loading in `detectCaptures`)
2. Lower the wrapper as `{qualified_name}(params...) → i64` (returns task pointer)
   - Allocate context struct with space for all params
   - Store params into context
   - Call `task_create(fn_ptr=&body, ctx_ptr=context)`
   - Return task pointer

**await lowering** — Replace the stub in `lowerAwaitExpr`:

```zig
fn lowerAwaitExpr(self: *Lowerer, ae: ast.AwaitExpr) Error!ir.NodeIndex {
    const fb = self.current_func orelse return ir.null_node;
    const task_ptr = try self.lowerExprNode(ae.operand);

    // Poll the task (Phase 1: eager — runs immediately)
    var poll_args = [_]ir.NodeIndex{task_ptr};
    _ = try fb.emitCall("task_poll", &poll_args, false, TypeRegistry.I64, ae.span);

    // Get the result
    var result_args = [_]ir.NodeIndex{task_ptr};
    const result = try fb.emitCall("task_get_result", &result_args, false, TypeRegistry.I64, ae.span);

    // Release the task (ARC cleanup)
    var release_args = [_]ir.NodeIndex{task_ptr};
    _ = try fb.emitCall("release", &release_args, false, TypeRegistry.VOID, ae.span);

    return result;
}
```

### Step 1.7: First Test

**File:** New `test/e2e/concurrency.cot`

```cot
test "basic async await" {
    async fn add(a: int, b: int) int {
        return a + b
    }

    let result = await add(3, 4)
    @assertEq(result, 7)
}

test "async fn returns task" {
    async fn getValue() int {
        return 42
    }

    let val = await getValue()
    @assertEq(val, 42)
}
```

### Step 1.8: Selfcot Port

Mirror all changes to `self/check/types.cot`, `self/check/checker.cot`, `self/build/lower.cot`. The 1:1 rule applies.

### Step 1.9: Commit

One commit: "Phase 1: Task runtime — async fn returns Task(T), await polls task"

---

## Phase 2: async/await State Machine (3-4 days)

**Goal:** Multiple await points in a single async function. The function is split into a state machine at SSA level. Each await becomes a state transition.

### Step 2.1: State Machine Transformation

In the lowerer, when an async function body contains multiple `await` expressions:

1. **Count await points** in the AST (rewrite `countAwaitPoints`)
2. **Assign state IDs** — each await gets an incrementing integer (0, 1, 2, ...)
3. **Generate the state machine poll function:**
   ```
   fn_name_poll(state_ptr: i64) → i64 {
       switch (state_ptr.phase) {
           0 => { /* code before first await */
                  store awaited task ptr in state
                  state.phase = 1
                  return 0  // pending
                }
           1 => { /* first await completed, code before second await */
                  load result from awaited task
                  ... more code ...
                  state.phase = 2
                  return 0  // pending
                }
           N => { /* final code after last await */
                  store final result in state
                  return 1  // completed
                }
       }
   }
   ```

### Step 2.2: State Struct Layout

```
AsyncState layout:
  Offset  0: phase          (i64) — current state machine phase
  Offset  8: result         (i64) — final result (when completed)
  Offset 16: awaited_task   (i64) — task pointer being awaited (current phase)
  Offset 24: ...spilled locals...  — values alive across await points
```

The spilled locals are determined by liveness analysis: any local variable whose definition dominates an await point and whose use is after an await point must be spilled.

### Step 2.3: Executor Integration

Replace the eager `task_poll` with a real polling loop:
- `task_poll` checks if the awaited sub-task is complete
- If not, returns `pending` (0)
- If yes, continues to next state
- The executor calls `task_poll` in a loop on all pending tasks

**Reference:** Rust's `Future::poll()` pattern. Kotlin's CPS transform.

### Step 2.4: Test

```cot
test "multiple awaits" {
    async fn step1() int { return 10 }
    async fn step2() int { return 20 }

    async fn combined() int {
        let a = await step1()
        let b = await step2()
        return a + b
    }

    @assertEq(await combined(), 30)
}
```

---

## Phase 4: Sendable (2-3 days) — BEFORE actors

**Goal:** Compile-time checking that types crossing concurrency boundaries are safe.

### Step 4.1: Sendable Trait Definition

**File:** New `stdlib/sendable.cot`

```cot
/// Marker trait: types safe to transfer across concurrency boundaries.
/// Value types with all-Sendable fields conform implicitly.
/// Actors conform implicitly.
trait Sendable {}
```

### Step 4.2: Implicit Sendable Conformance in Checker

**File:** `compiler/frontend/checker.zig`

Add `isSendable(type_idx: TypeIndex) bool` method:

```
fn isSendable(self: *Checker, ty: TypeIndex) bool {
    const info = self.types.get(ty);
    return switch (info) {
        .basic => true,          // int, i32, i64, f64, bool — all Sendable
        .struct_type => |s| {    // Sendable if all fields Sendable
            for (s.fields) |f| {
                if (!self.isSendable(f.type_idx)) return false;
            }
            return true;
        },
        .enum_type => true,      // simple enums always Sendable
        .union_type => |u| {     // Sendable if all variants Sendable
            for (u.variants) |v| {
                if (v.type_idx != types.invalid_type and !self.isSendable(v.type_idx)) return false;
            }
            return true;
        },
        .optional => |o| self.isSendable(o.elem),
        .error_union => |eu| self.isSendable(eu.elem),
        .list => |l| self.isSendable(l.elem),           // COW — safe if element Sendable
        .map => |m| self.isSendable(m.key) and self.isSendable(m.value),
        .task => true,           // Tasks are always Sendable (ARC heap objects)
        .pointer => false,       // Raw pointers not Sendable
        .func => false,          // Closures not Sendable (may capture mutable state)
        .slice => false,         // Slices are borrowed references
        else => false,
    };
}
```

### Step 4.3: Checking Points

Insert Sendable checks at:
1. `Task {}` closure captures — all captured values must be Sendable
2. Cross-actor method arguments (Phase 3)
3. Cross-actor return values (Phase 3)

Phase 4 adds the checker infrastructure. Phase 3 activates it at actor boundaries.

---

## Phase 3: Actors (4-5 days)

**Goal:** `actor` keyword creates a reference type with compiler-enforced isolation. Cross-actor calls require `await`.

### Step 3.1: Parser — `actor` keyword

Add `kw_actor` to token.zig. Parse `actor Name { ... }` as a struct-like declaration with an `is_actor: bool` flag.

### Step 3.2: Type System — ActorType

Add `actor_type: ActorType` to the Type union. Actors are heap-allocated ARC objects (like structs but with an embedded serial executor).

### Step 3.3: Checker — Isolation Enforcement

The core algorithm from Swift's `ActorReferenceResult::forReference()`:

1. Each declaration has an **isolation domain** (actor-isolated, nonisolated)
2. Each access site has a **context isolation** (what actor we're currently in)
3. If declaration isolation ≠ context isolation → require `await`
4. Cross-actor argument/return types must be Sendable

### Step 3.4: Lowerer — Actor Method Dispatch

Actor method calls that cross isolation boundaries:
1. Create a task that calls the method on the actor's executor
2. Enqueue the task on the actor's serial queue
3. Return the task (caller awaits it)

### Step 3.5: Serial Executor

Each actor has a FIFO job queue. Non-reentrant by default: when an actor method hits `await`, the actor does NOT process other messages.

---

## Phases 5-10: Summary

| Phase | Key Deliverable | Depends On |
|-------|----------------|------------|
| 5. TaskGroup | `withTaskGroup { group.addTask {} }`, `async let` | Phase 2 |
| 6. Channel | `Channel(T, capacity:)` with `await send/recv`, `for await in` | Phase 4 |
| 7. Global Actors | `@globalActor`, `@MainActor` | Phase 3 |
| 8. Cancellation | `Task.isCancelled`, `Task.checkCancellation()`, handlers | Phase 1 |
| 9. Wasm Executor | Single-threaded event loop, browser integration | Phase 2 |
| 10. Continuations | `withCheckedContinuation` (callback bridge) | Phase 2 |

Each phase gets its own detailed execution plan when the prior phase is complete.

---

## Key Reference Files

| What | Where |
|------|-------|
| Swift Task ABI | `references/swift/include/swift/ABI/Task.h` |
| Swift Actor ABI | `references/swift/include/swift/ABI/Actor.h` |
| Swift Executor | `references/swift/include/swift/ABI/Executor.h` |
| Swift CooperativeExecutor | `references/swift/stdlib/public/Concurrency/CooperativeExecutor.swift` |
| Swift Sendable checking | `references/swift/lib/Sema/TypeCheckConcurrency.cpp` |
| Swift Actor isolation | `references/swift/lib/Sema/TypeCheckConcurrency.cpp:8253` (forReference) |
| Swift TaskGroup | `references/swift/stdlib/public/Concurrency/TaskGroup.swift` |
| Swift AsyncStream | `references/swift/stdlib/public/Concurrency/AsyncStream.swift` |
| Kotlin CPS transform | `references/kotlin/compiler/ir/backend.common/src/org/jetbrains/kotlin/backend/common/lower/AbstractSuspendFunctionsLowering.kt` |
| Cot's try/catch lowering | `compiler/frontend/lower.zig:11797` (reference for error union patterns) |
| Cot's closure captures | `compiler/frontend/lower.zig:7438` (reuse for Task {} state) |
| Cot's trait infrastructure | `compiler/frontend/checker.zig:145-157` (reuse for Sendable) |

---

## Success Criteria

Phase 1 is done when:
```bash
./zig-out/bin/cot test test/e2e/concurrency.cot           # async/await basic tests pass
./zig-out/bin/cot test test/e2e/concurrency.cot --target=wasm  # same on Wasm
./zig-out/bin/cot test test/e2e/features.cot               # 370/370 still pass
./zig-out/bin/cot build self/main.cot -o /tmp/selfcot      # selfcot still builds
```

The full concurrency port is done when:
```bash
./zig-out/bin/cot test test/e2e/concurrency.cot    # All concurrency tests pass (native + wasm)
# Tests cover: async/await, actors, Sendable, TaskGroup, Channel, cancellation
```
