#!/bin/bash
# Compile all test cases to Wasm and run with Node.js

set -e

COT="./zig-out/bin/cot"
CASES_DIR="test/cases"
WASM_DIR="test/browser/wasm"

# Ensure compiler is built
if [ ! -f "$COT" ]; then
    echo "Building compiler..."
    zig build
fi

# Create output directory
mkdir -p "$WASM_DIR"

passed=0
failed=0
skipped=0

# Function to run wasm with Node.js and get main() return value
run_wasm() {
    local wasm_file="$1"
    node -e "
        const fs = require('fs');
        const wasm = fs.readFileSync('$wasm_file');
        WebAssembly.instantiate(wasm).then(r => {
            const result = r.instance.exports.main();
            // Convert BigInt to Number for comparison
            console.log(Number(result));
        }).catch(e => {
            console.error(e);
            process.exit(1);
        });
    " 2>/dev/null
}

# Find all .cot files
for cot_file in $(find "$CASES_DIR" -name "*.cot" | sort); do
    # Get relative path and compute output path
    rel_path="${cot_file#$CASES_DIR/}"
    name="${rel_path%.cot}"
    wasm_file="$WASM_DIR/${name}.wasm"

    # Create subdirectory if needed
    mkdir -p "$(dirname "$wasm_file")"

    # Extract expected exit code from file
    expected=$(grep -m1 "EXPECT: exit_code=" "$cot_file" | sed 's/.*exit_code=//')

    if [ -z "$expected" ]; then
        echo "⊘ $name: no expected value"
        ((skipped++)) || true
        continue
    fi

    # Compile to Wasm (use --target=wasm32 for wasm output)
    if ! "$COT" --target=wasm32 "$cot_file" -o "$wasm_file" 2>/dev/null; then
        echo "✗ $name: compilation failed"
        ((failed++)) || true
        continue
    fi

    # Run with Node.js
    result=$(run_wasm "$wasm_file")

    if [ "$result" = "$expected" ]; then
        echo "✓ $name"
        ((passed++)) || true
    else
        echo "✗ $name: got $result, expected $expected"
        ((failed++)) || true
    fi
done

echo ""
echo "================================"
echo "Passed: $passed"
echo "Failed: $failed"
echo "Skipped: $skipped"
echo "Total: $((passed + failed + skipped))"
