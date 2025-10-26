#!/bin/bash
# Automated test runner for FIT

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counters
TOTAL=0
PASSED=0
FAILED=0

# Get the FIT executable path
FIT_BIN="$(ls -t ../build/gfortran_*/app/fit 2>/dev/null | head -1)"
if [ -z "$FIT_BIN" ]; then
    echo -e "${RED}ERROR: FIT executable not found. Run 'fpm build' first.${NC}"
    exit 1
fi

echo "Using FIT: $FIT_BIN"
echo ""

# Test function
run_test() {
    local test_name=$1
    local input_file=$2
    local mode=$3
    local expected_file=$4

    TOTAL=$((TOTAL + 1))

    # Create temp file
    local temp_file="temp_${test_name}_${mode}.tmp"
    cp "$input_file" "$temp_file"

    # Run FIT
    echo -n "Testing ${test_name} with --${mode}... "

    if $FIT_BIN "$temp_file" "--${mode}" > /dev/null 2>&1; then
        # Compare output with expected
        if diff -q "$temp_file" "$expected_file" > /dev/null 2>&1; then
            echo -e "${GREEN}PASS${NC}"
            PASSED=$((PASSED + 1))
        else
            echo -e "${RED}FAIL${NC} (output mismatch)"
            echo "  Expected: $expected_file"
            echo "  Got:      $temp_file"
            echo "  Diff:"
            diff "$temp_file" "$expected_file" | head -20
            FAILED=$((FAILED + 1))
        fi
    else
        echo -e "${RED}FAIL${NC} (execution error)"
        FAILED=$((FAILED + 1))
    fi

    # Cleanup
    rm -f "$temp_file"
}

# Run tests
echo "========================================"
echo "  FIT Automated Test Suite"
echo "========================================"
echo ""

# Discover all test files
TEST_FILES=$(find . -maxdepth 1 -name "*.conflict" -type f | sort)

# Run all tests
for test_file in $TEST_FILES; do
    base=$(basename "$test_file" .conflict)

    # Check if expected files exist
    if [ -f "expected/${base}_incoming.expected" ]; then
        run_test "$base" "$test_file" "incoming" "expected/${base}_incoming.expected"
        run_test "$base" "$test_file" "local" "expected/${base}_local.expected"
        run_test "$base" "$test_file" "both" "expected/${base}_both.expected"
    else
        echo -e "${YELLOW}SKIP${NC}: Expected files not found for $base"
    fi
done

# Test short flags with one representative file
echo ""
echo "========================================"
echo "  Testing Short Flags (-i, -l, -b)"
echo "========================================"
echo ""

if [ -f "multi_conflict.f90.conflict" ] && [ -f "expected/multi_conflict.f90_incoming.expected" ]; then
    # Test short flag -i
    TOTAL=$((TOTAL + 1))
    temp_file="temp_short_i.tmp"
    cp "multi_conflict.f90.conflict" "$temp_file"
    echo -n "Testing short flag -i... "
    if $FIT_BIN "$temp_file" "-i" > /dev/null 2>&1 && \
       diff -q "$temp_file" "expected/multi_conflict.f90_incoming.expected" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        FAILED=$((FAILED + 1))
    fi
    rm -f "$temp_file"

    # Test short flag -l
    TOTAL=$((TOTAL + 1))
    temp_file="temp_short_l.tmp"
    cp "multi_conflict.f90.conflict" "$temp_file"
    echo -n "Testing short flag -l... "
    if $FIT_BIN "$temp_file" "-l" > /dev/null 2>&1 && \
       diff -q "$temp_file" "expected/multi_conflict.f90_local.expected" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        FAILED=$((FAILED + 1))
    fi
    rm -f "$temp_file"

    # Test short flag -b
    TOTAL=$((TOTAL + 1))
    temp_file="temp_short_b.tmp"
    cp "multi_conflict.f90.conflict" "$temp_file"
    echo -n "Testing short flag -b... "
    if $FIT_BIN "$temp_file" "-b" > /dev/null 2>&1 && \
       diff -q "$temp_file" "expected/multi_conflict.f90_both.expected" > /dev/null 2>&1; then
        echo -e "${GREEN}PASS${NC}"
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAIL${NC}"
        FAILED=$((FAILED + 1))
    fi
    rm -f "$temp_file"
fi

# Summary
echo ""
echo "========================================"
echo "  Test Summary"
echo "========================================"
echo "Total:  $TOTAL"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
