# Cot Test Data

This directory contains test fixtures for the Cot compiler and runtime.

## Directory Structure

```
testdata/
├── cot/                    # Cot language (.cot) test fixtures
│   ├── valid/              # Valid programs that should compile and run
│   │   ├── arithmetic.cot
│   │   ├── control_flow.cot
│   │   ├── functions.cot
│   │   ├── loops.cot
│   │   └── structs.cot
│   ├── invalid/            # Programs that should fail to compile
│   │   ├── syntax_errors/  # Parse errors
│   │   ├── type_errors/    # Type checking errors
│   │   └── semantic_errors/# Semantic analysis errors
│   └── runtime/            # Programs for runtime testing
│       ├── assertions/     # Assertion tests
│       └── edge_cases/     # Edge case behavior
├── dbl/                    # DBL language (.dbl) test fixtures
│   ├── valid/
│   ├── invalid/
│   └── runtime/
├── bytecode/               # Pre-compiled bytecode modules
├── schema/                 # Database schema fixtures
└── golden/                 # Golden output files for snapshot tests
```

## Usage

### From Tests

Use the `fixtures` module to load test data:

```zig
const fixtures = @import("testing").fixtures;

test "compile valid fixtures" {
    const source = try fixtures.loadFixture(
        allocator,
        .cot_valid,
        "arithmetic.cot"
    );
    defer allocator.free(source);

    // Use source...
}
```

### Inline Fixtures

For quick tests, use inline fixtures:

```zig
const fixtures = @import("testing").fixtures;

test "simple test" {
    var module = try compile(fixtures.inline_fixtures.empty_main);
    defer module.deinit();
}
```

## Adding New Fixtures

1. Choose the appropriate directory based on the test type
2. Use descriptive filenames (e.g., `array_out_of_bounds.cot`)
3. Add a comment at the top explaining what the fixture tests
4. Keep fixtures minimal - test one thing at a time
