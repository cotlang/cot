# Concurrency: Next Phases Execution Plan

**Date:** 2026-03-27
**Prerequisite:** All 10 concurrency phases complete (27/27 tests). Wasm store codegen fix committed.
**Goal:** Enable real suspension, closure-based TaskGroup, @Sendable closures, @TaskLocal.

---

## Priority Order

| # | Task | Effort | Impact | Swift Reference |
|---|------|--------|--------|-----------------|
| 1 | State Machine Transform (enable async_split) | 3-5 days | Unlocks real multi-await suspension | Rust coroutine.rs, Kotlin CPS |
| 2 | TaskGroup.addTask closure API | 2-3 days | Ergonomic structured concurrency | TaskGroup.swift SE-0304 |
| 3 | @Sendable closures (SE-0302) | 3-5 days | Type-safe concurrent closures | TypeCheckConcurrency.cpp:2971-3098 |
| 4 | @TaskLocal (SE-0311) | 3-4 days | Per-task context propagation | TaskLocal.swift, TaskLocal.cpp |
| 5 | @unchecked Sendable | 1 day | Escape hatch | TypeCheckConcurrency.cpp:7488 |

---

## Phase 1: Enable State Machine Transform

### What

Remove the `if (true or ...)` gate in `async_split.zig:189` by implementing the constructor/poll function split in the lowerer. Currently, `lowerAsyncFnEager` runs the async body to completion inline. For functions with 2+ await points, we need:

1. **Constructor function** — allocates the coroutine frame (state struct), stores params, returns Task pointer
2. **Poll function** — takes frame pointer, dispatches to resume point via state number, runs until next suspend or completion

### Architecture (Rust coroutine.rs pattern)

```
// Original:
async fn combined() i64 {
    const a = await getValue()    // suspend point 1
    const b = await getValue()    // suspend point 2
    return a + b
}

// After transform:
fn combined() i64 {                          // constructor
    var frame = alloc(0, frame_size)          // alloc state struct
    frame[0] = STATE_UNRESUMED               // state = 0
    // store params to frame if any
    combined_poll(frame)                      // initial poll
    return frame                              // return as Task ptr
}

fn combined_poll(frame: i64) void {           // poll function
    switch (frame[0]) {                       // dispatch on state
        STATE_UNRESUMED => {
            // run body until first await
            var task1 = getValue()            // call async fn
            frame[8] = task1                  // spill task ptr
            frame[0] = STATE_SUSPEND_0        // set next state
            return                            // yield (PENDING)
        },
        STATE_SUSPEND_0 => {
            var task1 = frame[8]              // restore task ptr
            var a = task1[0]                  // load result
            release(task1)                    // ARC release
            var task2 = getValue()            // second async call
            frame[16] = task2                 // spill
            frame[24] = a                     // spill live local
            frame[0] = STATE_SUSPEND_1
            return
        },
        STATE_SUSPEND_1 => {
            var task2 = frame[16]             // restore
            var a = frame[24]                 // restore
            var b = task2[0]                  // load result
            release(task2)
            frame[0] = STATE_RETURNED
            frame[8] = a + b                 // store final result
            return
        },
    }
}
```

### Implementation Steps

**Step 1.1: Split lowerAsyncFnEager into constructor + poll** (`lower.zig`)

When `countAwaits(fn_decl.body) >= 2`:

1. Emit **constructor function** (same name as original):
   - Allocate frame via `alloc(0, frame_size)` where frame_size comes from async_split analysis
   - Store each param to frame at known offsets
   - Store STATE_UNRESUMED (0) at frame[0]
   - Call poll function with frame pointer
   - Return frame pointer as Task(T)

2. Emit **poll function** (name = `{original}_poll`):
   - Takes single i64 param (frame pointer)
   - Lower the original body inside this function
   - Set `fb.async_frame_ptr` = param local (the frame pointer)
   - async_split SSA pass will transform the body into a state machine

**Step 1.2: Update lowerAwaitExpr for poll pattern** (`lower.zig`)

When inside a poll function (detected by `fb.async_frame_ptr != null`):

1. Evaluate operand (returns Task pointer from child async fn)
2. Check if child task is already complete (Phase 1: always true since eager)
3. If not complete: spill live locals to frame, set state, return PENDING
4. If complete: load result from child task, release child task, continue

For Phase 1 (eager), all child tasks complete immediately, so step 3 never triggers. This makes the transform safe to enable without changing runtime behavior.

**Step 1.3: Remove the gate** (`async_split.zig:189`)

Change `if (true or suspend_points.items.len < 2)` to `if (suspend_points.items.len < 2)`.

**Step 1.4: Wire poll function into executor** (`executor.cot`)

Update `Executor.run()` to:
1. For each task: call `poll_fn(frame)`
2. Check if state == STATE_RETURNED
3. If yes: mark completed, extract result
4. If no: re-enqueue for next poll iteration

**Step 1.5: Tests**

- Test `combined()` from concurrency.cot (2 await points) — already exists, should still pass
- Add test with 3+ await points (chain of 3 async calls)
- Add test with await inside loop (requires state machine loop dispatch)
- Verify all 27 existing tests still pass (eager path unchanged for 0-1 awaits)

### Swift/Rust References

| Component | Reference |
|-----------|-----------|
| State machine dispatch | Rust coroutine.rs:1085-1109 `insert_switch` |
| Frame layout | Rust coroutine.rs:969-1068 `compute_layout` |
| Liveness across suspend | Rust coroutine.rs:709-811 `locals_live_across_suspend_points` |
| Block splitting | Rust coroutine.rs:190-340 `TransformVisitor` |
| Constructor pattern | Kotlin AbstractSuspendFunctionsLowering.kt:66-92 |
| Poll pattern | Kotlin CoroutineTransformerLowering.kt |

---

## Phase 2: TaskGroup.addTask Closure API

### What

Change `TaskGroup.addTask(value: T)` to `TaskGroup.addTask(body: fn() -> T)` so tasks are created lazily from closures, matching Swift's `group.addTask { await someWork() }` pattern.

### Current API (Phase 1 stub)
```cot
group.addTask(42)           // pre-computed value, no async work
```

### Target API (Swift parity)
```cot
group.addTask(fn() i64 { return await computeValue() })
```

### Implementation Steps

**Step 2.1: Update TaskGroup struct** (`stdlib/task_group.cot`)

```cot
struct TaskGroup(T) {
    tasks: List(fn() -> T),    // list of closures (not results)
    results: List(T),          // completed results
    cursor: int,

    fn addTask(body: fn() -> T) void {
        self.tasks.append(body)
    }

    fn next() ?T {
        // Execute pending tasks (Phase 1: eager)
        while (self.results.len() <= self.cursor and self.tasks.len() > 0) {
            const task = self.tasks.get(0)  // dequeue first task
            // TODO: remove from front efficiently
            const result = task()            // call closure
            self.results.append(result)
        }
        if (self.cursor >= self.results.len()) { return null }
        const val = self.results.get(self.cursor)
        self.cursor = self.cursor + 1
        return val
    }
}
```

**Step 2.2: Update tests** (`test/e2e/task_group.cot`)

```cot
test "task group with closures" {
    var group = TaskGroup(i64).init()
    group.addTask(fn() i64 { return 10 })
    group.addTask(fn() i64 { return 20 })
    group.addTask(fn() i64 { return 30 })

    var sum: i64 = 0
    while (group.next()) |val| { sum = sum + val }
    @assertEq(sum, 60)
}
```

**Step 2.3: Closure type checking** (`checker.zig`)

Verify that the closure type `fn() -> T` matches `TaskGroup(T)`'s type parameter.

### Swift Reference

```swift
// Swift TaskGroup.swift
mutating func addTask(
    priority: TaskPriority? = nil,
    body: @Sendable @escaping () async throws -> ChildTaskResult
)
```

Key differences from Swift:
- Cot closures are not yet `@Sendable` (Phase 3 adds this)
- Cot closures are not `async` yet (Phase 1 state machine enables this)
- Priority parameter deferred (no priority system yet)

---

## Phase 3: @Sendable Closures (SE-0302)

### What

Add compile-time checking that closures passed to concurrent APIs only capture `Sendable` values. This prevents data races from shared mutable state.

### Swift Architecture (TypeCheckConcurrency.cpp:2971-3098)

1. **Closure is @Sendable if**: explicitly annotated OR passed to `sending` parameter OR used in `Task { }` / `TaskGroup.addTask { }`
2. **@Sendable implies nonisolated**: closure cannot access actor-isolated state from enclosing scope
3. **All captures must be Sendable**: recursive type check on every captured variable
4. **`nonisolated` locals bypass check**: variables explicitly marked `nonisolated(unsafe)` are exempt

### Implementation Steps

**Step 3.1: Track closure captures** (`checker.zig`)

Add capture analysis to the checker:

```zig
// For each closure literal, compute the set of captured variables
// by walking the closure body and finding references to variables
// defined in an enclosing scope.
fn computeClosureCaptures(self: *Checker, closure_body: NodeIndex) ![]CapturedVar {
    var captures = std.ArrayListUnmanaged(CapturedVar){};
    // Walk body, for each ident that resolves to an outer scope variable,
    // add to captures list with its type
    // ...
    return captures.toOwnedSlice(self.allocator);
}
```

**Step 3.2: Add @Sendable attribute** (`parser.zig`, `checker.zig`)

Parser: recognize `@Sendable` before closure expressions.
Checker: when a closure has `@Sendable` or is passed to a concurrent API:
1. Compute captures
2. For each capture, call `isSendable(capture.type_idx)`
3. If not Sendable, emit error: `"capture of non-Sendable type '{type}' in @Sendable closure"`

**Step 3.3: Mark concurrent API parameters** (`checker.zig`)

Functions that accept closures for concurrent execution should require @Sendable:
- `TaskGroup.addTask(body:)` — the body closure
- `Task { }` — the task body (future work)
- `Channel.send()` — the value (already checked by Sendable)

**Step 3.4: Enforce nonisolated** (`checker.zig`)

When inside a @Sendable closure, accessing actor-isolated state should error:
```
error: actor-isolated property 'count' can not be referenced from a @Sendable closure
```

**Step 3.5: Tests**

```cot
actor Counter { count: i64 }

test "sendable closure rejects non-sendable capture" {
    var c = new Counter { count: 0 }
    // This should be a compile error:
    // group.addTask(@Sendable fn() i64 { return c.count })
    //                                        ^ error: actor-isolated
}

test "sendable closure allows sendable capture" {
    const x: i64 = 42  // i64 is Sendable
    var group = TaskGroup(i64).init()
    group.addTask(@Sendable fn() i64 { return x })
    // Should compile fine
}
```

### Swift Reference

| Pattern | Swift | Cot |
|---------|-------|-----|
| Closure is Sendable | `@Sendable () -> T` | `@Sendable fn() -> T` |
| Capture check | TypeCheckConcurrency.cpp:2971 `checkLocalCaptures()` | checker.zig: new `checkSendableCaptures()` |
| Nonisolated enforcement | TypeCheckConcurrency.cpp:4963 | checker.zig: check `current_actor_type` inside @Sendable |
| Error diagnostic | `diag::non_sendable_capture` | `"non-Sendable type captured in @Sendable closure"` |

---

## Phase 4: @TaskLocal (SE-0311)

### What

Per-task storage that is inherited by child tasks and scoped via `withValue()`.

### Swift Architecture (TaskLocal.swift, TaskLocal.cpp)

```swift
@TaskLocal static var requestID: String = "none"

func handleRequest() async {
    $requestID.withValue("req-123") {
        // requestID == "req-123" here and in all child tasks
        await processRequest()  // child task inherits requestID
    }
    // requestID == "none" here (scope ended)
}
```

**Value storage**: Linked list per task. Each binding is a stack-pushed item. `withValue` pushes, scope exit pops. Child tasks copy the most recent binding per key.

### Implementation Steps

**Step 4.1: TaskLocal struct** (`stdlib/task_local.cot`)

```cot
struct TaskLocal(T) {
    default_value: T,
    key: i64,  // unique key for this task-local (address of struct)

    static fn init(default: T) TaskLocal(T) {
        return TaskLocal(T) { default_value: default, key: 0 }
    }

    fn get() T {
        // Look up in current task's local storage
        // If not found, return default_value
        const val = __task_local_get(self.key)
        if (val == 0) { return self.default_value }
        return @intToPtr(*T, val).*
    }

    fn withValue(T)(value: T, body: fn() -> void) void {
        __task_local_push(self.key, @ptrToInt(&value))
        defer __task_local_pop()
        body()
    }
}
```

**Step 4.2: Runtime functions** (`compiler/codegen/wasm/wasi.zig` or runtime)

```
__task_local_push(key: i64, value_ptr: i64) void
__task_local_pop() void
__task_local_get(key: i64) i64  // returns 0 if not found
__task_local_copy_to(target_task: i64) void  // for child task inheritance
```

Implementation: global HashMap(i64, List(TaskLocalItem)) keyed by task ID. Each task has a stack of bindings. Push/pop operate on the current task's stack.

**Step 4.3: Task structure update** (`lower.zig`)

Add task-local storage pointer to TaskObject:
```
Offset 0:  result (i64)
Offset 8:  cancelled (i64)
Offset 16: task_local_head (i64)  // pointer to linked list head
```

Update `alloc(0, 24)` in lowerAsyncFnEager.

**Step 4.4: Child task inheritance** (`lower.zig`)

When creating a child task (TaskGroup.addTask, Task { }), copy parent's task-local bindings:
```
// In lowerAsyncFnEager or task creation:
__task_local_copy_to(child_task_ptr)
```

**Step 4.5: @TaskLocal attribute** (`parser.zig`)

Parse `@TaskLocal` on static variable declarations. Generate the `$variable` projection for `withValue` access.

**Step 4.6: Tests**

```cot
import "std/task_local"

@TaskLocal
const requestID = TaskLocal(string).init("none")

test "task local basic" {
    @assertEq(requestID.get(), "none")
    requestID.withValue("req-123", fn() void {
        @assertEq(requestID.get(), "req-123")
    })
    @assertEq(requestID.get(), "none")  // restored
}

test "task local inherited by child" {
    requestID.withValue("parent", fn() void {
        var group = TaskGroup(string).init()
        group.addTask(fn() string { return requestID.get() })
        const val = group.next()
        @assertEq(val.?, "parent")  // inherited
    })
}
```

### Swift Reference

| Component | Swift | Cot |
|-----------|-------|-----|
| Storage | TaskLocal.cpp linked list per task | HashMap(task_id, List(Item)) |
| Push | `swift_task_localValuePush(key, value, type)` | `__task_local_push(key, value_ptr)` |
| Pop | `swift_task_localValuePop()` | `__task_local_pop()` |
| Get | `swift_task_localValueGet(key)` | `__task_local_get(key)` |
| Inheritance | `swift_task_localsCopyTo(target)` | `__task_local_copy_to(target_task)` |
| Scope | `withValue(_:operation:)` | `withValue(value, body)` |

---

## Phase 5: @unchecked Sendable

### What

Escape hatch for types that are logically Sendable but the compiler can't prove it. Used for types with internal synchronization (mutexes, atomics).

### Implementation

**Step 5.1: Parser** — recognize `@unchecked Sendable` in trait conformance syntax:
```cot
impl @unchecked Sendable for MyLockedType { }
```

**Step 5.2: Checker** — when `isSendable()` encounters a type with `@unchecked Sendable` conformance, return true without recursive checking.

**Step 5.3: Tests**
```cot
struct UnsafeContainer {
    data: i64,  // imagine this has internal mutex
}
impl @unchecked Sendable for UnsafeContainer { }

test "unchecked sendable" {
    const c = UnsafeContainer { .data = 42 }
    // Should be usable in concurrent contexts without error
}
```

---

## Selfcot Port Strategy

Each phase must be ported to `self/` after the Zig implementation is complete and tested:

| Phase | Files to Port |
|-------|---------------|
| 1 (State Machine) | `self/build/lower.cot` (constructor/poll split), `self/emit/wasm/gen.cot` (if SSA changes) |
| 2 (TaskGroup Closure) | `stdlib/task_group.cot` (stdlib change, auto-compiled) |
| 3 (@Sendable) | `self/check/checker.cot` (capture analysis), `self/parse/parser.cot` (@Sendable attr) |
| 4 (@TaskLocal) | `stdlib/task_local.cot` (new file), `self/build/lower.cot` (task struct update) |
| 5 (@unchecked) | `self/check/checker.cot` (unchecked conformance) |

---

## Validation Gates

After each phase:
1. All 27 existing concurrency tests pass (native)
2. All 15 concurrency tests pass (wasm) — new Wasm fix must hold
3. 370/370 features pass (no regression)
4. New phase-specific tests pass
5. `zig build test` passes
6. Selfcot builds: `./zig-out/bin/cot build self/main.cot -o /tmp/selfcot`
