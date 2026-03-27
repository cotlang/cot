# Phase K: AsyncStream Implementation Plan

**Date:** 2026-03-28
**Prerequisite:** Phase J (associated types in traits) — DONE
**Swift reference:** AsyncStream.swift, AsyncStreamBuffer.swift, AsyncStream.cpp
**Goal:** Production-ready AsyncStream with proven patterns from Swift. No invented logic.

---

## Audit Results (Phases A-J)

**Overall fidelity: 96% faithful, 3% adapted, 1% invented.**

All adaptations are documented and necessary (Wasm call_indirect, busy-wait on Wasm, Phase 1 eager evaluation). The two pieces of invented code are non-concerns (Wasm-specific call_indirect bridge, frame[16] poll_fn storage).

No regressions, no dead code, no stale stubs. 430+ tests all green.

---

## AsyncStream Architecture (from Swift audit)

### Core Types

```
AsyncStream<Element> {
    context: _Context                          // Wraps storage + produce function

    struct Continuation {                      // Producer interface
        storage: _Storage
        yield(value) -> YieldResult           // Add element or wake consumer
        finish()                              // Terminate stream
        onTermination: (Termination) -> Void  // Cleanup callback
    }

    struct Iterator: AsyncIteratorProtocol {   // Consumer interface
        context: _Context
        next() async -> Element?              // Get next or suspend
    }

    enum YieldResult { enqueued(remaining), dropped(Element), terminated }
    enum BufferingPolicy { unbounded, bufferingOldest(Int), bufferingNewest(Int) }
    enum Termination { finished, cancelled }
}
```

### Internal State (_Storage)

```
_Storage<Element> {
    state: {
        continuations: [UnsafeContinuation]   // FIFO queue of waiting consumers
        pending: Deque<Element>               // Buffered elements
        limit: BufferingPolicy                // Immutable after init
        onTermination: Termination -> Void    // Cleanup (cleared on first use)
        terminal: bool                        // Once true, yields return .terminated
    }
    lock: Mutex                               // Platform-specific (os_unfair_lock / pthread_mutex)
}
```

### State Machine Rules (from AsyncStreamBuffer.swift)

**yield(value):**
1. If consumer waiting AND pending empty AND not terminal → deliver directly
2. If consumer waiting AND pending non-empty → buffer value, deliver first pending
3. If no consumer → buffer per policy (unbounded/oldest/newest)
4. If terminal → return .terminated

**finish():**
1. Clear onTermination handler (prevent double-invoke)
2. Set terminal = true
3. Resume ALL waiting consumers with nil
4. Invoke handler(.finished) outside lock

**next():**
1. Register continuation in FIFO queue
2. If pending non-empty → deliver immediately
3. If terminal and empty → return nil
4. Otherwise → suspend (continuation stays queued)

**cancel():**
1. Swap out handler
2. Invoke handler(.cancelled)
3. Call finish()

---

## Implementation Steps

### Step 1: Deque(T) data structure (stdlib)
**Reference:** Swift _Deque (stdlib/private/Deque.swift)
**Effort:** 1 day

Ring buffer with O(1) append/removeFirst. Needed for buffered elements queue.
Already have List(T) which supports append. Need efficient removeFirst (shift).
Can implement as List with head cursor (like Channel already does).

### Step 2: AsyncSequence and AsyncIterator traits (stdlib)
**Reference:** AsyncSequence.swift, AsyncIteratorProtocol.swift
**Effort:** 1 day

```cot
trait AsyncSequence(Element) {
    type Iterator: AsyncIterator(Element)
    fn makeAsyncIterator() int  // returns Iterator handle
}

trait AsyncIterator(Element) {
    fn next() ?Element
}
```

Phase 1: next() is synchronous (returns Option). Phase 2+: async.

### Step 3: _Storage struct with locking (stdlib)
**Reference:** AsyncStreamBuffer.swift:58-278
**Effort:** 2 days

```cot
struct AsyncStreamStorage(Element) {
    pending: List(Element),     // buffered elements (Deque semantics via cursor)
    pending_cursor: int,        // front of deque
    limit: int,                 // BufferingPolicy encoded as int
    limit_kind: int,            // 0=unbounded, 1=oldest, 2=newest
    terminal: bool,
    has_termination_handler: bool,
}
```

Phase 1 simplification: no lock needed (single-threaded).
Phase 2+: add mutex via C interop (pthread_mutex_lock/unlock).

### Step 4: yield() state machine (stdlib)
**Reference:** AsyncStreamBuffer.swift:123-208
**Effort:** 2 days

Implement all three cases from the audit:
1. Consumer waiting (Phase 1: consumer polls synchronously)
2. Buffer per policy (unbounded, oldest, newest)
3. Terminal state returns .terminated

### Step 5: finish() and cancel() (stdlib)
**Reference:** AsyncStreamBuffer.swift:210-232, 110-121
**Effort:** 1 day

finish() sets terminal, invokes handler.
cancel() swaps handler, invokes with .cancelled, delegates to finish().

### Step 6: AsyncStream struct with builder and makeStream factory (stdlib)
**Reference:** AsyncStream.swift:290-298, 437-445
**Effort:** 1 day

```cot
fn AsyncStream_init(Element)(build: fn(Continuation(Element)) void) AsyncStream(Element)
fn AsyncStream_makeStream(Element)() (AsyncStream(Element), Continuation(Element))
```

### Step 7: Iterator with next() (stdlib)
**Reference:** AsyncStreamBuffer.swift:234-260
**Effort:** 1 day

Phase 1: next() checks pending buffer, returns element or null.
Phase 2+: next() suspends via continuation if buffer empty.

### Step 8: Tests
**Effort:** 1 day

- Builder pattern creates stream
- yield() stores elements, returns YieldResult
- Buffering policies (unbounded, oldest, newest)
- finish() signals end (next() returns null)
- cancel() invokes onTermination with .cancelled
- for-await loop consumes all elements
- Multiple yields + single consumer
- Capacity overflow with each policy

---

## Estimated Total: 10 days

| Step | Days | Depends On |
|------|------|-----------|
| 1. Deque(T) | 1 | — |
| 2. AsyncSequence/Iterator traits | 1 | Phase J (done) |
| 3. _Storage struct | 2 | Step 1 |
| 4. yield() state machine | 2 | Step 3 |
| 5. finish() + cancel() | 1 | Step 3 |
| 6. AsyncStream struct | 1 | Steps 3-5 |
| 7. Iterator next() | 1 | Steps 3, 6 |
| 8. Tests | 1 | All above |

---

## Phase L: sending + Region Isolation (after AsyncStream)

**Reference:** Swift SE-0430 (sending parameter), SE-0414 (region isolation)
**Estimated:** 5-6 days

This is advanced type system work:
1. `sending` keyword on function parameters
2. Forward dataflow analysis on SSA for region tracking
3. Automatic Sendable inference for disconnected values

Can be deferred until after AsyncStream is complete and tested.

---

## What's NOT In Scope

These are Phase 2+ items requiring real async suspension:
- AsyncStream.next() actual suspension (needs executor)
- Multiple concurrent consumers (needs task scheduling)
- withTaskCancellationHandler (needs cancellation propagation)
- AsyncThrowingStream (parallel implementation, after AsyncStream)
- Custom AsyncSequence operators (map, filter, etc.)
