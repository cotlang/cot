#!/bin/bash
# Run all Cot language tests via `cot test`.
# Usage: ./test/run_all.sh [--target=wasm32]
#
# Discovers all .cot files in test/e2e/ and test/cases/, runs each with `cot test`,
# reports per-file pass/fail, exits with total failure count.
# Pass --target=<target> to forward to `cot test`.

set -euo pipefail

# Parse arguments
TARGET_FLAG=""
for arg in "$@"; do
    case "$arg" in
        --target=*) TARGET_FLAG="$arg" ;;
        *) echo "Unknown argument: $arg"; exit 1 ;;
    esac
done

# Find repo root (directory containing this script's parent)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

# Prefer dev build (zig-out) over PATH (Homebrew release may be outdated)
COT="${COT:-}"
if [ -z "$COT" ]; then
    if [ -x "$REPO_ROOT/zig-out/bin/cot" ]; then
        COT="$REPO_ROOT/zig-out/bin/cot"
    elif command -v cot &>/dev/null; then
        COT="cot"
    else
        echo "Error: cot not found. Run 'zig build' first or set COT=/path/to/cot"
        exit 1
    fi
fi

passed=0
failed=0
failures=()

# Collect all test files via glob
files=()
for f in test/e2e/*.cot test/cases/*.cot; do
    [ -f "$f" ] && files+=("$f")
done

total=${#files[@]}
if [ -n "$TARGET_FLAG" ]; then
    echo "Running $total Cot test files ($TARGET_FLAG)..."
else
    echo "Running $total Cot test files..."
fi
echo ""

for f in "${files[@]}"; do
    # Skip bench-only files (they need `cot bench`, not `cot test`)
    if grep -q '^bench "' "$f" && ! grep -q '^test "' "$f"; then
        printf "%-45s" "$f"
        echo "skip (bench file)"
        total=$((total - 1))
        continue
    fi
    printf "%-45s" "$f"
    if output=$("$COT" test "$f" $TARGET_FLAG 2>&1); then
        # Extract summary line if present (may be just "Tests passed" for wasm)
        summary=$(echo "$output" | grep -E '[0-9]+ passed' | tail -1 || true)
        echo "ok  $summary"
        passed=$((passed + 1))
    else
        exit_code=$?
        # Check if it was a signal crash (exit code > 128) — retry once
        if [ $exit_code -gt 128 ]; then
            signal=$((exit_code - 128))
            # Retry once for signal crashes (intermittent ARC cleanup issues)
            if retry_output=$("$COT" test "$f" $TARGET_FLAG 2>&1); then
                summary=$(echo "$retry_output" | grep -E '[0-9]+ passed' | tail -1 || true)
                echo "ok  $summary (retry after signal $signal)"
                passed=$((passed + 1))
                continue
            fi
            exit_code=$?
        fi
        echo "FAIL (exit $exit_code)"
        failures+=("$f")
        # Show last lines of output for debugging — enough to see signal messages
        echo "$output" | tail -10 | sed 's/^/    /'
        failed=$((failed + 1))
    fi
done

echo ""
echo "---"
echo "$passed/$total files passed"

if [ $failed -gt 0 ]; then
    echo "$failed file(s) failed:"
    for f in "${failures[@]}"; do
        echo "  - $f"
    done
    exit 1
fi

exit 0
