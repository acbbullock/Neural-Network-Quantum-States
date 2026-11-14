#!/bin/bash
# run1.sh variant n m J B epochs samples seed [dtau]  -> one line: settings, wall time, and the RESULT line
# (or FAIL and the error message when the program stops)
N=$(dirname "$(readlink -f "$0")")
v=$1; shift
mkdir -p "$N/runs"; d=$(mktemp -d "$N/runs/tmp.XXXXXX"); mkdir -p "$d/data"; cd "$d"
start=$(date +%s.%N)
all=$("$N/bin/$v" "$@" 2>&1)
t=$(echo "$(date +%s.%N) - $start" | bc)
out=$(echo "$all" | grep RESULT)
[ -z "$out" ] && out="FAIL n=$1 m=$2 seed=$7 $(echo "$all" | grep -m1 -i 'fatal\|error' | tr -s ' ')"
echo "variant=$v J=$3 B=$4 samples=$6 dtau=${8:-0.01} time=$t $out"
cd "$N"; rm -rf "$d"
