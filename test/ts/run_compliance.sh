#!/bin/bash
# Cot TypeScript compliance test runner.
#
# Runs .ts test files from test/ts/cases/ and checks their output
# against expected output in corresponding .expected files.
#
# Usage:
#   ./test/ts/run_compliance.sh           # Run all tests
#   ./test/ts/run_compliance.sh basics    # Run specific category

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
COT="$REPO_ROOT/zig-out/bin/cot"
CASES_DIR="$SCRIPT_DIR/cases"

if [ ! -f "$COT" ]; then
    echo "Error: cot binary not found. Run: zig build"
    exit 1
fi

FILTER="${1:-}"
pass=0
fail=0
crash=0
total=0

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

for ts_file in "$CASES_DIR"/*.ts; do
    [ -f "$ts_file" ] || continue
    name=$(basename "$ts_file" .ts)

    # Filter by category if specified
    if [ -n "$FILTER" ] && [[ "$name" != *"$FILTER"* ]]; then
        continue
    fi

    expected_file="${ts_file%.ts}.expected"
    if [ ! -f "$expected_file" ]; then
        printf "${YELLOW}SKIP${NC} %s (no .expected file)\n" "$name"
        continue
    fi

    total=$((total + 1))
    expected=$(cat "$expected_file")

    # Run the test
    actual=$("$COT" run "$ts_file" 2>&1) || true
    exit_code=$?

    if [ $exit_code -gt 128 ]; then
        crash=$((crash + 1))
        printf "${RED}CRASH${NC} %s (signal %d)\n" "$name" "$((exit_code - 128))"
        continue
    fi

    # Compare output (ignore trailing whitespace/newlines)
    actual_trimmed=$(echo "$actual" | sed 's/^VWT:.*$//' | sed '/^$/d')
    expected_trimmed=$(echo "$expected" | sed '/^$/d')

    if [ "$actual_trimmed" = "$expected_trimmed" ]; then
        pass=$((pass + 1))
        printf "${GREEN}PASS${NC} %s\n" "$name"
    else
        fail=$((fail + 1))
        printf "${RED}FAIL${NC} %s\n" "$name"
        echo "  Expected: $(echo "$expected_trimmed" | head -3)"
        echo "  Actual:   $(echo "$actual_trimmed" | head -3)"
    fi
done

echo ""
echo "=== Results: $pass/$total passed, $fail failed, $crash crashed ==="

if [ $fail -gt 0 ] || [ $crash -gt 0 ]; then
    exit 1
fi
