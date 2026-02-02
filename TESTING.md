# Cot Testing Strategy

**Goal:** Build comprehensive test coverage to prove feature parity with bootstrap-0.2, then surpass it with the Wasm-first approach.

## Test Targets

| Target | Description | Test Count Goal |
|--------|-------------|-----------------|
| Wasm (browser) | Primary target - runs in all browsers | 400+ E2E tests |
| Wasm (runtime) | Node.js, Deno, wasmtime validation | Same tests as browser |
| Native (AOT) | ARM64/AMD64 via Wasm→SSA→Native | Same tests as Wasm |
| Inline | Unit tests embedded in Zig source | 200+ inline tests |

---

## Test Categories

### 1. Inline Tests (Zig `test` blocks)

Located in source files, run with `zig build test`.

| Module | File | Focus |
|--------|------|-------|
| Scanner | `frontend/scanner.zig` | Tokenization, keywords, operators |
| Parser | `frontend/parser.zig` | AST construction, error recovery |
| Checker | `frontend/checker.zig` | Type checking, scope resolution |
| Lowerer | `frontend/lower.zig` | IR generation, local allocation |
| SSA Builder | `frontend/ssa_builder.zig` | SSA construction, phi insertion |
| Wasm Gen | `codegen/wasm_gen.zig` | Wasm bytecode generation |
| ARM64 | `codegen/native/arm64.zig` | ARM64 instruction encoding |
| AMD64 | `codegen/native/amd64.zig` | AMD64 instruction encoding |

### 2. E2E Pipeline Tests (Full compilation)

| File | Coverage |
|------|----------|
| `frontend/e2e_test.zig` | Source → Parse → Check → Lower → IR |
| `codegen/wasm_e2e_test.zig` | Source → Wasm bytecode |
| `codegen/native_e2e_test.zig` | Source → Wasm → Native (planned) |

### 3. Runtime Tests (Execute compiled code)

**Wasm Runtime:**
```bash
# Using Node.js
node --experimental-wasm-modules test.mjs

# Using wasmtime
wasmtime test.wasm

# Using browser (via test harness)
open test/browser/runner.html
```

**Native Runtime:**
```bash
# Compile and run
./zig-out/bin/cot test.cot --target=arm64-macos -o test
./test
echo "Exit code: $?"
```

---

## Test File Organization

```
cot/
├── compiler/
│   ├── frontend/
│   │   ├── e2e_test.zig          # Frontend pipeline tests
│   │   └── integration_test.zig  # Integration tests
│   └── codegen/
│       ├── wasm_e2e_test.zig     # Wasm compilation tests
│       └── native_e2e_test.zig   # Native AOT tests (planned)
│
├── test/
│   ├── cases/                    # Test case files (.cot)
│   │   ├── arithmetic/           # Basic math operations
│   │   ├── control_flow/         # if/else, loops, break/continue
│   │   ├── functions/            # Calls, recursion, closures
│   │   ├── structs/              # Composite types
│   │   ├── memory/               # Pointers, slices, arrays
│   │   ├── strings/              # String operations
│   │   └── arc/                  # Reference counting
│   │
│   ├── browser/                  # Browser test harness
│   │   ├── runner.html           # Test runner page
│   │   ├── harness.js            # Test harness code
│   │   └── wasm/                 # Compiled test .wasm files
│   │
│   └── expected/                 # Expected outputs (golden files)
│
└── scripts/
    ├── run_wasm_tests.sh         # Run all Wasm tests
    ├── run_native_tests.sh       # Run all native tests
    └── run_browser_tests.sh      # Launch browser tests
```

---

## Test Case Format

Each test case is a `.cot` file with expected behavior documented:

```cot
// test/cases/arithmetic/add.cot
// EXPECT: exit_code=42
// EXPECT: stdout=
fn main() int {
    return 40 + 2;
}
```

```cot
// test/cases/control_flow/while_break.cot
// EXPECT: exit_code=5
fn main() int {
    let i: int = 0;
    while (true) {
        i = i + 1;
        if (i == 5) {
            break;
        }
    }
    return i;
}
```

---

## Browser Test Harness

### runner.html
```html
<!DOCTYPE html>
<html>
<head><title>Cot Wasm Tests</title></head>
<body>
  <h1>Cot Wasm Test Runner</h1>
  <div id="results"></div>
  <script type="module" src="harness.js"></script>
</body>
</html>
```

### harness.js
```javascript
const tests = [
  { name: 'arithmetic/add', expected: 42 },
  { name: 'arithmetic/mul', expected: 20 },
  { name: 'control_flow/while', expected: 10 },
  // ... more tests
];

async function runTests() {
  const results = document.getElementById('results');
  let passed = 0, failed = 0;

  for (const test of tests) {
    try {
      const wasm = await WebAssembly.instantiateStreaming(
        fetch(`wasm/${test.name}.wasm`)
      );
      const result = wasm.instance.exports.main();

      if (result === test.expected) {
        passed++;
        results.innerHTML += `<div style="color:green">✓ ${test.name}</div>`;
      } else {
        failed++;
        results.innerHTML += `<div style="color:red">✗ ${test.name}: got ${result}, expected ${test.expected}</div>`;
      }
    } catch (e) {
      failed++;
      results.innerHTML += `<div style="color:red">✗ ${test.name}: ${e.message}</div>`;
    }
  }

  results.innerHTML += `<h2>${passed} passed, ${failed} failed</h2>`;
}

runTests();
```

---

## Running Tests

### All Tests (Zig inline + E2E)
```bash
zig build test
```

### Wasm Compilation Tests
```bash
# Compile test cases to Wasm
for f in test/cases/**/*.cot; do
  ./zig-out/bin/cot "$f" -o "test/browser/wasm/$(basename "$f" .cot).wasm"
done
```

### Native AOT Tests
```bash
# Compile and run each test case
for f in test/cases/**/*.cot; do
  ./zig-out/bin/cot "$f" --target=arm64-macos -o /tmp/test_bin
  /tmp/test_bin
  echo "$f: exit code $?"
done
```

### Browser Tests
```bash
# Start local server and open browser
python3 -m http.server 8080 -d test/browser &
open http://localhost:8080/runner.html
```

---

## Parity Tracking

### Bootstrap-0.2 Reference

| Category | bootstrap-0.2 Tests | Cot 0.3 Tests | Parity |
|----------|---------------------|---------------|--------|
| Arithmetic | 50+ | 20 | 40% |
| Control Flow | 80+ | 30 | 37% |
| Functions | 100+ | 25 | 25% |
| Structs | 60+ | 15 | 25% |
| Pointers | 40+ | 10 | 25% |
| Arrays/Slices | 50+ | 10 | 20% |
| Strings | 60+ | 15 | 25% |
| Enums | 30+ | 0 | 0% |
| Defer | 20+ | 0 | 0% |
| ARC | 50+ | 5 | 10% |
| **Total** | **754** | **130** | **17%** |

### Milestones

| Milestone | Tests | Target Date |
|-----------|-------|-------------|
| M1: 200 tests | 200 | 2026-02-07 |
| M2: 400 tests | 400 | 2026-02-14 |
| M3: 600 tests | 600 | 2026-02-21 |
| M4: 754 tests (parity) | 754 | 2026-02-28 |
| M5: 1000 tests (surpass) | 1000 | 2026-03-15 |

---

## Debug Output

Enable debug logging with environment variables:
```bash
# All debug output
COT_DEBUG=all zig build test

# Specific phases
COT_DEBUG=parse,check,lower,codegen zig build test

# Wasm-specific
COT_DEBUG=wasm,layout zig build test

# Native-specific
COT_DEBUG=regalloc,arm64 zig build test
```

---

## CI Integration (Future)

```yaml
# .github/workflows/test.yml
name: Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: goto-bus-stop/setup-zig@v2
      - run: zig build test

  wasm-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: goto-bus-stop/setup-zig@v2
      - run: zig build
      - run: ./scripts/run_wasm_tests.sh

  native-test:
    runs-on: macos-latest
    steps:
      - uses: actions/checkout@v3
      - uses: goto-bus-stop/setup-zig@v2
      - run: zig build
      - run: ./scripts/run_native_tests.sh
```

---

## Contributing Tests

1. Create a new `.cot` file in the appropriate `test/cases/` subdirectory
2. Add `// EXPECT:` comments documenting expected behavior
3. Add the test to the browser harness if applicable
4. Run `zig build test` to verify inline tests pass
5. Run the test through Wasm and native pipelines
6. Submit PR with test file and any harness updates
