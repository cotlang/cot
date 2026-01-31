#!/bin/bash
# Compile all test cases to native and run

set -e

COT="./zig-out/bin/cot"
CASES_DIR="test/cases"
TMP_BIN="/tmp/cot_test_bin"

# Detect target
case "$(uname -s)-$(uname -m)" in
    Darwin-arm64) TARGET="arm64-macos" ;;
    Darwin-x86_64) TARGET="amd64-macos" ;;
    Linux-x86_64) TARGET="amd64-linux" ;;
    Linux-aarch64) TARGET="arm64-linux" ;;
    *) echo "Unsupported platform"; exit 1 ;;
esac

echo "Target: $TARGET"
echo ""

# Ensure compiler is built
if [ ! -f "$COT" ]; then
    echo "Building compiler..."
    zig build
fi

passed=0
failed=0
skipped=0

# Find all .cot files
for cot_file in $(find "$CASES_DIR" -name "*.cot" | sort); do
    rel_path="${cot_file#$CASES_DIR/}"
    name="${rel_path%.cot}"

    # Extract expected exit code from file
    expected=$(grep -m1 "EXPECT: exit_code=" "$cot_file" | sed 's/.*exit_code=//')

    if [ -z "$expected" ]; then
        echo "⊘ $name: no expected value"
        ((skipped++)) || true
        continue
    fi

    # Compile to object, then link
    OBJ_FILE="$TMP_BIN.o"
    if ! "$COT" "$cot_file" -o "$OBJ_FILE" 2>/dev/null; then
        echo "✗ $name: compilation failed"
        ((failed++)) || true
        continue
    fi

    # Link to executable
    if ! clang "$OBJ_FILE" -o "$TMP_BIN" -lSystem 2>/dev/null; then
        echo "✗ $name: linking failed"
        ((failed++)) || true
        continue
    fi

    # Run and capture exit code (don't fail on non-zero)
    set +e
    "$TMP_BIN" 2>/dev/null
    result=$?
    set -e

    if [ "$result" = "$expected" ]; then
        echo "✓ $name"
        ((passed++)) || true
    else
        echo "✗ $name: got $result, expected $expected"
        ((failed++)) || true
    fi
done

# Cleanup
rm -f "$TMP_BIN"

echo ""
echo "================================"
echo "Passed: $passed"
echo "Failed: $failed"
echo "Skipped: $skipped"
echo "Total: $((passed + failed + skipped))"
