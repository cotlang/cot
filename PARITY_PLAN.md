# Wasm Backend Parity Plan

Goal: Achieve 100% feature parity with bootstrap-0.2 native backend.

Reference: `~/learning/go/src/cmd/compile/internal/`

---

## Native AOT Status

**Status: ✅ WORKING** (as of 2026-01-31)

The AOT pipeline now produces working native binaries:

```bash
# Compile Cot → Wasm → SSA → Native
./zig-out/bin/cot hello.cot --target=arm64-macos -o hello
./hello
echo $?  # Exit code from main()
```

Verified working:
- Simple returns (`return 42` → exit code 42)
- Arithmetic expressions (`10 + 5 * 2` → exit code 20)
- Local variables
- Control flow (if/else, while)

---

## Phase 1: Control Flow Completeness

### P1.1: Break Statement
- [ ] Verify `break` parsed in frontend (scanner, parser)
- [ ] Verify `break` in IR (lower.zig)
- [ ] Verify `break` SSA op exists (op.zig)
- [ ] Implement break in lower_wasm.zig
- [ ] Implement break in wasm_gen.zig (br instruction)
- [ ] Add e2e test: simple break
- [ ] Add e2e test: nested loop break
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/branchelim.go`

### P1.2: Continue Statement
- [ ] Verify `continue` parsed in frontend
- [ ] Verify `continue` in IR
- [ ] Verify `continue` SSA op exists
- [ ] Implement continue in lower_wasm.zig
- [ ] Implement continue in wasm_gen.zig (br to loop header)
- [ ] Add e2e test: simple continue
- [ ] Add e2e test: nested loop continue
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/branchelim.go`

### P1.3: For-In Loops
- [ ] Verify `for` parsed in frontend
- [ ] Verify for-in lowered to while in IR
- [ ] Add e2e test: for i in range
- [ ] Add e2e test: for item in array
- Go ref: `~/learning/go/src/cmd/compile/internal/walk/range.go`

---

## Phase 2: Variables & Scope

### P2.1: Global Variables
- [ ] Check global var parsing in frontend
- [ ] Check global var in IR
- [ ] Add wasm global for each Cot global
- [ ] Implement global.get/global.set in wasm_gen.zig
- [ ] Add e2e test: read global
- [ ] Add e2e test: write global
- [ ] Add e2e test: global in function
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/writebarrier.go`

### P2.2: Constants
- [ ] Verify const evaluated at compile time
- [ ] Verify const inlined at use sites
- [ ] Add e2e test: const declaration
- [ ] Add e2e test: const in expression

---

## Phase 3: Type System

### P3.1: Integer Type Conversions
- [ ] Add wasm ops: i32.wrap_i64, i64.extend_i32_s, i64.extend_i32_u
- [ ] Add SSA ops for type casts
- [ ] Implement in lower_wasm.zig
- [ ] Implement in wasm_gen.zig
- [ ] Add e2e test: i64 to i32
- [ ] Add e2e test: i32 to i64 (signed)
- [ ] Add e2e test: i32 to i64 (unsigned)
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/rewriteWasm.go` (Cvt ops)

### P3.2: Smaller Integer Types (i8, i16, u8, u16)
- [ ] Verify frontend parses these types
- [ ] Implement load8/load16 with sign/zero extend
- [ ] Implement store8/store16 with truncation
- [ ] Add e2e test: u8 operations
- [ ] Add e2e test: i16 operations
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/rewriteWasm.go` (Load8/16/32)

### P3.3: Float Operations
- [ ] Verify f64 e2e works
- [ ] Add f32 support if needed
- [ ] Add e2e test: f64 arithmetic
- [ ] Add e2e test: f64 comparison
- [ ] Add e2e test: int to float conversion
- [ ] Add e2e test: float to int conversion
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/rewriteWasm.go` (Cvt64Fto*)

---

## Phase 4: Advanced Control Flow

### P4.1: Defer Statement
- [ ] Verify defer parsed in frontend
- [ ] Check defer implementation in IR
- [ ] Implement defer stack in wasm_gen.zig
- [ ] Add e2e test: simple defer
- [ ] Add e2e test: multiple defers (LIFO order)
- [ ] Add e2e test: defer with early return
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/writebarrier.go`

### P4.2: Switch/Match
- [ ] Verify switch parsed in frontend
- [ ] Check switch lowering to if-else chain or br_table
- [ ] Implement br_table for dense switches
- [ ] Add e2e test: simple switch
- [ ] Add e2e test: switch with default
- Go ref: `~/learning/go/src/cmd/compile/internal/ssa/rewritegeneric.go`

---

## Phase 5: Composite Types

### P5.1: Enums
- [ ] Verify enum parsed in frontend
- [ ] Check enum representation (integer tags)
- [ ] Add e2e test: enum declaration
- [ ] Add e2e test: enum comparison
- [ ] Add e2e test: enum in switch
- Go ref: Uses integer constants

### P5.2: Unions (Tagged)
- [ ] Check union parsing
- [ ] Check union layout (tag + max variant size)
- [ ] Add e2e test: union creation
- [ ] Add e2e test: union field access
- Go ref: `~/learning/go/src/cmd/compile/internal/types/size.go`

---

## Phase 6: Built-in Functions

### P6.1: len() Builtin
- [ ] Verify len() parsed as builtin
- [ ] Implement for strings (load length field)
- [ ] Implement for slices (load length field)
- [ ] Implement for arrays (compile-time constant)
- [ ] Add e2e test: len(string)
- [ ] Add e2e test: len(slice)
- [ ] Add e2e test: len(array)
- Go ref: `~/learning/go/src/cmd/compile/internal/walk/builtin.go`

### P6.2: print/println
- [ ] Requires import section (browser or WASI)
- [ ] Add import for fd_write or console.log
- [ ] Implement print builtin
- [ ] Add e2e test: print string
- Go ref: `~/learning/go/src/cmd/compile/internal/walk/print.go`

### P6.3: @sizeOf, @alignOf
- [ ] Verify these are compile-time evaluated
- [ ] Add e2e test: @sizeOf(struct)
- [ ] Add e2e test: @alignOf(type)

---

## Phase 7: External Functions

### P7.1: Import Section
- [ ] Add import section to wasm.Module
- [ ] Support (import "module" "name" (func ...))
- [ ] Add extern function declaration handling
- [ ] Add e2e test: call imported function
- Go ref: `~/learning/go/src/cmd/link/internal/wasm/asm.go`

### P7.2: Export Section (Enhanced)
- [ ] Export multiple functions
- [ ] Export memory
- [ ] Export globals
- Go ref: `~/learning/go/src/cmd/link/internal/wasm/asm.go`

---

## Progress Tracking

| Phase | Task | Status | Date |
|-------|------|--------|------|
| P1.1 | Break Statement | ✅ | 2026-01-31 |
| P1.2 | Continue Statement | ✅ | 2026-01-31 |
| P1.3 | For-In Loops | ⬜ | |
| P2.1 | Global Variables | ✅ | 2026-01-31 |
| P2.2 | Constants | ⬜ | |
| P3.1 | Int Conversions | ⬜ | |
| P3.2 | Small Int Types | ⬜ | |
| P3.3 | Float Operations | ✅ | 2026-01-31 |
| P4.1 | Defer Statement | ⬜ | |
| P4.2 | Switch/Match | ⬜ | |
| P5.1 | Enums | ⬜ | |
| P5.2 | Unions | ⬜ | |
| P6.1 | len() Builtin | ⬜ | |
| P6.2 | print/println | ⬜ | |
| P6.3 | @sizeOf/@alignOf | ⬜ | |
| P7.1 | Import Section | ⬜ | |
| P7.2 | Export Section | ⬜ | |

---

## Testing Strategy

Each feature requires:
1. Unit test in relevant file (e.g., wasm_gen.zig)
2. E2E test in wasm_e2e_test.zig
3. Verification against bootstrap-0.2 behavior
