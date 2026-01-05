# Conformance Test Suite

This directory contains conformance tests that both the Zig and Rust runtimes must pass identically.

## Structure

Each subdirectory contains tests for a specific category:

- `arithmetic/` - Integer and decimal arithmetic operations
- `strings/` - String operations and manipulation
- `control_flow/` - Conditionals, loops, and branching
- `functions/` - Function calls, parameters, return values
- `records/` - Record types and field access
- `io/` - Input/output operations

## Test Format

Each test consists of:
- `<name>.cot` - Source file
- `<name>.expected` - Expected stdout output
- `<name>.meta.toml` (optional) - Test metadata

### meta.toml Format

```toml
[test]
description = "Test description"
skip_rust = false    # Skip for Rust runtime
skip_zig = false     # Skip for Zig runtime
exit_code = 0        # Expected exit code
timeout_ms = 5000    # Timeout in milliseconds
```

## Running Conformance Tests

From the project root:

```bash
# Run all conformance tests on Zig runtime
cot test conformance

# Run all conformance tests on Rust runtime
cot-rs test conformance

# Compare outputs between runtimes
./tools/compare-runtimes.sh
```
