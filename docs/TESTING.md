# Cot Testing System

## Overview

Cot uses inline test blocks (Zig syntax) with error-union-based test isolation. Tests are compiled and run as native executables via `cot test`.

## Syntax

```cot
test "description" {
    @assert(condition)
    @assert_eq(actual, expected)
}
```

## How It Works

### Pipeline

```
test "name" { body }
    → lowerTestDecl() creates test function returning !void
    → generateTestRunner() creates main() that calls each test
    → main() catches errors per-test, prints pass/fail, returns failure count
```

### Test Functions Return `!void`

Each `test` block compiles to a function with return type `!void` (error union). On `@assert` failure, the function returns an error (tag=1). On success, it returns ok (tag=0).

### Test Runner (Generated `main()`)

The generated `main()` function:
1. For each test: prints `test "name" ... `, calls the test function
2. Reads the error union tag from the return value
3. If error (tag != 0): prints `FAIL\n`, increments failure counter
4. If success (tag == 0): prints `ok\n`, increments pass counter
5. Prints summary: `N passed` (and `, M failed` if any failures)
6. Returns failure count as exit code (0 = all pass)

### Test Isolation

Tests are isolated via error unions (Zig pattern). A failing `@assert` in one test does NOT abort the process — the test runner catches the error and continues to the next test.

## Builtins

| Builtin | Args | Behavior |
|---------|------|----------|
| `@assert(cond)` | 1 | In test: return error. Outside test: print + trap |
| `@assert_eq(a, b)` | 2 | Same as `@assert(a == b)` |

### `@assert` Outside Tests

When `@assert` is used outside a test block (e.g., in a regular function), it prints "assertion failed\n" to stderr via `cot_write` and traps (`unreachable` in Wasm, `brk` on ARM64).

## Runtime Functions

Four Wasm module functions in `compiler/codegen/test_runtime.zig`:

| Function | Signature | Output |
|----------|-----------|--------|
| `__test_print_name` | `(ptr: i64, len: i64) -> void` | `test "name" ... ` to stderr |
| `__test_pass` | `() -> void` | `ok\n` to stderr |
| `__test_fail` | `() -> void` | `FAIL\n` to stderr |
| `__test_summary` | `(passed: i64, failed: i64) -> void` | `\nN passed[, M failed]\n` to stderr |

All functions write to stderr (fd=2) via `cot_write`.

## Output Format

```
test "basic arithmetic" ... ok
test "failing test" ... FAIL
test "another test" ... ok

3 passed, 1 failed
```

Exit code = number of failures (0 = all pass).

## Test Directory Structure

```
test/
  cases/              # Category unit tests (21 files, ~100 tests)
    arithmetic.cot      10 tests
    arrays.cot           6 tests
    arc.cot              5 tests
    bitwise.cot          6 tests
    builtins.cot         4 tests
    chars.cot            2 tests
    compound.cot         8 tests
    control_flow.cot    14 tests
    enum.cot             2 tests
    extern.cot           1 test
    float.cot            1 test
    functions.cot       16 tests
    loops.cot            3 tests
    memory.cot           5 tests
    methods.cot          1 test
    optional.cot         3 tests
    strings.cot         13 tests
    structs.cot          5 tests
    switch.cot           2 tests
    types.cot            2 tests
    union.cot            4 tests
  e2e/                # Comprehensive feature tests (12 files, ~614 tests)
    features.cot       107 tests (structs, generics, traits, enums, unions, etc.)
    expressions.cot    160 tests
    functions.cot      107 tests
    control_flow.cot    82 tests
    variables.cot       40 tests
    types.cot           46 tests
    memory.cot          17 tests
    stdlib.cot           5 tests (cross-file generic imports)
    map.cot             25 tests
    auto_free.cot        5 tests
    set.cot             10 tests
    string_interp.cot   10 tests
  test_inline.cot     # Manual smoke test
  browser/            # Pre-compiled Wasm for manual browser testing
```

## Adding New Tests

1. Add `test "name" { ... }` blocks to the appropriate file
2. Use `@assert(cond)` or `@assert_eq(actual, expected)`
3. Run `cot test <file>` to verify
4. Tests are automatically wired into `zig build test` via `native_e2e_test.zig`

## CLI

```bash
cot test file.cot              # Compile in test mode, run, print results
```

## CI Integration

`zig build test` runs all Zig-level tests including `native_e2e_test.zig`, which uses a batch architecture:

**Batch test** (1 test): All 33 `.cot` files from `test/e2e/` and `test/cases/` are concatenated into one combined source (imports deduplicated), compiled once, linked once, and run once. Verifies exit code 0 and that all ~720 tests pass.

**8 special tests** (remain isolated):
- 5 print tests (non-test-mode, verify specific stdout output)
- 3 inline test-mode tests (verify test runner output format, including failure isolation)

## File Map

| File | Purpose |
|------|---------|
| `compiler/codegen/test_runtime.zig` | `__test_print_name`, `__test_pass`, `__test_fail`, `__test_summary` |
| `compiler/driver.zig` | Wires test_runtime into linker + func_indices |
| `compiler/frontend/parser.zig` | Parses `@assert`, `@assert_eq` builtins |
| `compiler/frontend/checker.zig` | Type-checks `@assert`, `@assert_eq` builtins |
| `compiler/frontend/lower.zig` | `lowerTestDecl`, `generateTestRunner`, assert lowering |
| `compiler/codegen/native_e2e_test.zig` | E2E tests: compiles + runs all test files |

## Known Limitations

- f64 comparison in test mode triggers SIGILL (error-union wrapping issue)
- Nested struct field access with `undefined` init fails in test mode (4 tests skipped in types.cot)

## Future Improvements

- Source location in assert messages (`file:line:col`)
- Expected vs actual value display for `@assert_eq`
- `@expect(cond)` returning error (Zig pattern) for use with `try`
- Test filtering by name pattern
- Timing per test
