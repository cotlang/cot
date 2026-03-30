#!/bin/bash
# Test262 compliance runner for Cot's TypeScript/JavaScript frontend.
#
# Usage:
#   ./test/ts/run_test262.sh              # Run all parser smoke tests
#   ./test/ts/run_test262.sh --category expressions  # Run specific category
#   ./test/ts/run_test262.sh --negative   # Run negative parse tests
#   ./test/ts/run_test262.sh --summary    # Just print counts
#
# What this tests:
#   - Scanner: Can we tokenize the file without crashing?
#   - Parser: Can we parse the file without crashing?
#   - Negative tests: Does the parser correctly reject invalid syntax?
#
# Test262 location: test/ts/test262/ (clone with: git clone --depth 1 https://github.com/nicolo-ribaudo/test262.git test/ts/test262)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
COT="$REPO_ROOT/zig-out/bin/cot"
TEST262_DIR="$REPO_ROOT/test/ts/test262/test/language"
RESULTS_DIR="$REPO_ROOT/test/ts/results"

mkdir -p "$RESULTS_DIR"

if [ ! -d "$TEST262_DIR" ]; then
    echo "Error: Test262 not found at $TEST262_DIR"
    echo "Clone it: git clone --depth 1 https://github.com/nicolo-ribaudo/test262.git test/ts/test262"
    exit 1
fi

if [ ! -f "$COT" ]; then
    echo "Error: cot binary not found. Run: zig build"
    exit 1
fi

CATEGORY="${2:-}"
MODE="${1:---smoke}"

# Check if a test file is a negative parse test (should fail at parse time)
is_negative_parse() {
    grep -q "phase: parse" "$1" 2>/dev/null && grep -q "negative:" "$1" 2>/dev/null
}

# Check if a test uses features we definitely can't handle yet
uses_unsupported() {
    local file="$1"
    # Skip tests requiring eval, generators, with, Proxy, class-fields-private
    head -5 "$file" | grep -q "flags:.*\(module\|async\|generated\)" && return 0
    grep -q '^\$DONOTEVALUATE' "$file" 2>/dev/null && return 1  # These are parse tests, run them
    return 1
}

run_smoke_tests() {
    local dir="$1"
    local category="$2"
    local pass=0
    local fail=0
    local skip=0
    local crash=0
    local total=0

    while IFS= read -r -d '' file; do
        total=$((total + 1))

        # Skip module tests and async tests for now
        if head -10 "$file" | grep -q "flags:.*module"; then
            skip=$((skip + 1))
            continue
        fi

        # Try to compile (check mode — parse + typecheck, no codegen)
        if "$COT" check "$file" >/dev/null 2>&1; then
            pass=$((pass + 1))
        else
            exit_code=$?
            if [ $exit_code -gt 128 ]; then
                crash=$((crash + 1))
                echo "CRASH: $file (signal $((exit_code - 128)))"
            else
                fail=$((fail + 1))
            fi
        fi

        # Progress indicator every 100 tests
        if [ $((total % 100)) -eq 0 ]; then
            printf "\r  %s: %d tested, %d pass, %d fail, %d crash, %d skip" \
                "$category" "$total" "$pass" "$fail" "$crash" "$skip"
        fi
    done < <(find "$dir" -name "*.js" -print0 | sort -z)

    printf "\r  %-35s total=%-6d pass=%-6d fail=%-6d crash=%-6d skip=%-6d\n" \
        "$category" "$total" "$pass" "$fail" "$crash" "$skip"

    echo "$category $total $pass $fail $crash $skip" >> "$RESULTS_DIR/smoke.txt"
}

run_negative_tests() {
    local dir="$1"
    local category="$2"
    local correct_reject=0
    local false_accept=0
    local crash=0
    local total=0

    while IFS= read -r -d '' file; do
        if ! is_negative_parse "$file"; then
            continue
        fi
        total=$((total + 1))

        if "$COT" check "$file" >/dev/null 2>&1; then
            false_accept=$((false_accept + 1))
        else
            exit_code=$?
            if [ $exit_code -gt 128 ]; then
                crash=$((crash + 1))
                echo "CRASH on negative test: $file"
            else
                correct_reject=$((correct_reject + 1))
            fi
        fi

        if [ $((total % 100)) -eq 0 ]; then
            printf "\r  %s: %d tested" "$category" "$total"
        fi
    done < <(find "$dir" -name "*.js" -print0 | sort -z)

    if [ $total -gt 0 ]; then
        printf "\r  %-35s total=%-6d reject=%-6d accept=%-6d crash=%-6d\n" \
            "$category" "$total" "$correct_reject" "$false_accept" "$crash"
        echo "$category $total $correct_reject $false_accept $crash" >> "$RESULTS_DIR/negative.txt"
    fi
}

echo "=== Test262 Compliance Tests ==="
echo "Binary: $COT"
echo "Test262: $TEST262_DIR"
echo ""

if [ "$MODE" = "--summary" ]; then
    if [ -f "$RESULTS_DIR/smoke.txt" ]; then
        echo "=== Last Smoke Test Results ==="
        cat "$RESULTS_DIR/smoke.txt"
    fi
    if [ -f "$RESULTS_DIR/negative.txt" ]; then
        echo "=== Last Negative Test Results ==="
        cat "$RESULTS_DIR/negative.txt"
    fi
    exit 0
fi

if [ "$MODE" = "--negative" ]; then
    echo "Running negative parse tests (should be rejected)..."
    echo "" > "$RESULTS_DIR/negative.txt"
    if [ -n "$CATEGORY" ]; then
        run_negative_tests "$TEST262_DIR/$CATEGORY" "$CATEGORY"
    else
        for dir in "$TEST262_DIR"/*/; do
            name=$(basename "$dir")
            run_negative_tests "$dir" "$name"
        done
    fi
    echo ""
    echo "Results saved to $RESULTS_DIR/negative.txt"
    exit 0
fi

# Default: smoke tests
echo "Running parse smoke tests (should not crash)..."
echo "" > "$RESULTS_DIR/smoke.txt"
if [ -n "$CATEGORY" ]; then
    run_smoke_tests "$TEST262_DIR/$CATEGORY" "$CATEGORY"
else
    for dir in "$TEST262_DIR"/*/; do
        name=$(basename "$dir")
        run_smoke_tests "$dir" "$name"
    done
fi

echo ""
echo "Results saved to $RESULTS_DIR/smoke.txt"

# Print totals
echo ""
echo "=== TOTALS ==="
awk '{ total+=$2; pass+=$3; fail+=$4; crash+=$5; skip+=$6 } END { printf "Total: %d, Pass: %d (%.1f%%), Fail: %d, Crash: %d, Skip: %d\n", total, pass, (pass/total)*100, fail, crash, skip }' "$RESULTS_DIR/smoke.txt"
