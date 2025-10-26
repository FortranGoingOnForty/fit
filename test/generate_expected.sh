#!/bin/bash
# Generate expected outputs for all conflict test files

set -e

cd "$(dirname "$0")"

FIT="../build/gfortran_*/app/fit"
FIT=$(ls -t $FIT 2>/dev/null | head -1)

if [ -z "$FIT" ]; then
    echo "ERROR: FIT executable not found"
    exit 1
fi

echo "Using FIT: $FIT"
echo ""

count=0

for conflict_file in *.conflict; do
    [ ! -f "$conflict_file" ] && continue

    base=$(basename "$conflict_file" .conflict)
    echo "Processing: $base"

    # Generate incoming
    cp "$conflict_file" "__temp_incoming__.tmp"
    $FIT "__temp_incoming__.tmp" --incoming > /dev/null 2>&1
    mv "__temp_incoming__.tmp" "expected/${base}_incoming.expected"
    count=$((count + 1))

    # Generate local
    cp "$conflict_file" "__temp_local__.tmp"
    $FIT "__temp_local__.tmp" --local > /dev/null 2>&1
    mv "__temp_local__.tmp" "expected/${base}_local.expected"
    count=$((count + 1))

    # Generate both
    cp "$conflict_file" "__temp_both__.tmp"
    $FIT "__temp_both__.tmp" --both > /dev/null 2>&1
    mv "__temp_both__.tmp" "expected/${base}_both.expected"
    count=$((count + 1))
done

echo ""
echo "Generated $count expected output files"
