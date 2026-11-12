#!/bin/bash
# run1.sh variant n m J B epochs samples seed early  -> prints "variant J B early RESULT..."
N=$(dirname "$(readlink -f "$0")")
v=$1; shift
d=$(mktemp -d "$N/runs/tmp.XXXXXX"); mkdir -p "$d/data"; cd "$d"
start=$(date +%s.%N)
out=$("$N/bin/$v" "$@" 2>&1 | grep RESULT)
t=$(echo "$(date +%s.%N) - $start" | bc)
echo "variant=$v J=$3 B=$4 early=$8 samples=$6 dtau=${9:-0.01} time=$t $out"
cd "$N"; rm -rf "$d"
