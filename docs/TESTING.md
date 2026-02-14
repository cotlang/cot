# Cot Testing System

## Overview

Cot uses inline test blocks (Zig syntax) with error-union-based test isolation. Tests are compiled and run as native executables via `cot test`.

## Two-Tier Strategy

| Tier | Command | What it runs | Speed |
|------|---------|-------------|-------|
| **Zig compiler tests** | `zig build test` | ~163 Zig-level tests with inline Cot snippets | Fast (<10s) |
| **Cot language tests** | `./test/run_all.sh` | ~1020 Cot tests across 46 `.cot` files | Slower (~60s) |
| **Single file** | `cot test file.cot` | Tests in one file | Fast |

### `zig build test` — Compiler internals

Runs Zig test blocks across the compiler:
- `native_e2e_test.zig` — 12 tests: print/println output, fd_write, fd_read, @exit, test-mode formatting
- `wasm_e2e_test.zig` — 106 tests: Wasm codegen pipeline with inline Cot
- `frontend/e2e_test.zig` — 34 tests: parser/checker/IR/SSA
- `frontend/integration_test.zig` — 10 tests: parser+checker integration
- `lsp/semantic_tokens.zig` — 1 test: LSP semantic token generation

These use small inline Cot snippets (3-5 lines) to test specific Zig codegen behavior.

### `./test/run_all.sh` — All Cot language tests

Discovers all `.cot` files in `test/e2e/` and `test/cases/`, runs each with `cot test`, reports per-file pass/fail. No hardcoded file lists — uses glob discovery.

```bash
./test/run_all.sh
# test/e2e/auto_free.cot                       ok  5 passed
# test/e2e/control_flow.cot                    ok  82 passed
# ...
# test/e2e/json.cot                            ok  38 passed
# ...
# test/cases/arithmetic.cot                    ok  10 passed
# ...
# 41/41 files passed
```

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
  run_all.sh            # Run all Cot tests (glob discovery, no hardcoded lists)
  cases/                # Category unit tests (21 files, ~106 tests)
    arithmetic.cot        10 tests
    arrays.cot             6 tests
    arc.cot                5 tests
    bitwise.cot            6 tests
    builtins.cot           4 tests
    chars.cot              2 tests
    compound.cot           8 tests
    control_flow.cot      14 tests
    enum.cot               2 tests
    extern.cot             1 test
    float.cot              1 test
    functions.cot         16 tests
    loops.cot              3 tests
    memory.cot             5 tests
    methods.cot            1 test
    optional.cot           3 tests
    strings.cot           13 tests
    structs.cot            5 tests
    switch.cot             2 tests
    types.cot              2 tests
    union.cot              4 tests
  e2e/                  # Comprehensive feature tests (25 files, ~904 tests)
    features.cot         127 tests
    expressions.cot      160 tests
    functions.cot        107 tests
    control_flow.cot      82 tests
    variables.cot         40 tests
    types.cot             46 tests
    memory.cot            17 tests
    stdlib.cot             8 tests
    map.cot               25 tests
    auto_free.cot          5 tests
    set.cot               10 tests
    string_interp.cot     10 tests
    wasi_io.cot           19 tests
    std_io.cot            34 tests
    string_methods.cot    34 tests
    math.cot               8 tests
    std_math.cot          16 tests
    sort.cot               7 tests
    json.cot              38 tests
    safe_mode.cot          8 tests
    io.cot                14 tests
    encoding.cot          26 tests
    url.cot               13 tests
    http.cot              11 tests
    wasmgc.cot            12 tests
  test_inline.cot       # Manual smoke test
  browser/              # Pre-compiled Wasm for manual browser testing
```

## Adding New Tests

1. Add `test "name" { ... }` blocks to the appropriate `.cot` file
2. Use `@assert(cond)` or `@assert_eq(actual, expected)`
3. Run `cot test <file>` to verify
4. Run `./test/run_all.sh` to verify all tests still pass

## File Map

| File | Purpose |
|------|---------|
| `test/run_all.sh` | Runs all Cot test files via `cot test` |
| `compiler/codegen/test_runtime.zig` | `__test_print_name`, `__test_pass`, `__test_fail`, `__test_summary` |
| `compiler/codegen/native_e2e_test.zig` | 12 Zig-level codegen tests (inline Cot snippets) |
| `compiler/driver.zig` | Wires test_runtime into linker + func_indices |
| `compiler/frontend/parser.zig` | Parses `@assert`, `@assert_eq` builtins |
| `compiler/frontend/checker.zig` | Type-checks `@assert`, `@assert_eq` builtins |
| `compiler/frontend/lower.zig` | `lowerTestDecl`, `generateTestRunner`, assert lowering |

## Known Limitations

- f64 comparison in test mode triggers SIGILL (error-union wrapping issue)
- Nested struct field access with `undefined` init fails in test mode (4 tests skipped in types.cot)

## Future Improvements

- Source location in assert messages (`file:line:col`)
- Expected vs actual value display for `@assert_eq`
- `@expect(cond)` returning error (Zig pattern) for use with `try`
- Test filtering by name pattern
- Timing per test
