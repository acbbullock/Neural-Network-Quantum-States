#!/usr/bin/env bash
# Regenerates the data in this folder with the demo program.
#
# Usage: data/generate.sh PROGRAM [JOBS]
#
# PROGRAM is the built demo program, for example from
#   fpm build --compiler ifx --flag "-DNNQS_COARRAY -coarray=shared -coarray-num-images=4 -O2"
# JOBS is how many runs of the field sweep go at once (default 4).
#
# It trains three things:
#   - the antiferromagnetic chain of the README (1024 spins, J = -0.5, B = 0.1): energies_A.csv, correlations_A.csv.gz
#   - a ferromagnetic chain (1000 spins, J = 0.5, B = 0.1): energies_F.csv, correlations_F.csv.gz
#   - a sweep of the field B from 0.05 to 0.95 on a ferromagnetic chain of 64 spins with J = 0.5, which crosses the
#     quantum critical point at B = J: field_sweep/energies_B*.csv, and field_sweep/correlations.csv with each
#     site's correlation with the middle spin, averaged over the last 100 epochs
# The two long chains take a few hours. Their logs are collected in optimization_results.log.
set -euo pipefail

prog=$(readlink -f "$1")
jobs=${2:-4}
data=$(dirname "$(readlink -f "$0")")
work=$(mktemp -d)
echo "Working in $work"

run() {
  # run DIR ARGS...: trains in its own folder, since the program writes to ./data and ./optimization_results.log
  local dir=$1; shift
  mkdir -p "$dir/data"
  (cd "$dir" && "$prog" "$@" > stdout.txt)
}

run "$work/A" &
run "$work/F" 1000 64 0.5 0.1 &

fields=$(seq -f '%.2f' 0.05 0.05 0.95)
export -f run; export prog work
printf '%s\n' $fields | xargs -P "$jobs" -I{} bash -c 'run "$work/sweep/B{}" 64 16 0.5 {}'
wait

cp "$work"/A/data/energies_A.csv "$work"/F/data/energies_F.csv "$data"/
# The correlations hold every site at every epoch, about 15 MB each, and compress well.
gzip -9 -c "$work/A/data/correlations_A.csv" > "$data/correlations_A.csv.gz"
gzip -9 -c "$work/F/data/correlations_F.csv" > "$data/correlations_F.csv.gz"
cat "$work/A/optimization_results.log" "$work/F/optimization_results.log" > "$data/../optimization_results.log"

mkdir -p "$data/field_sweep"
echo 'B,site,correlation' > "$data/field_sweep/correlations.csv"
for B in $fields; do
  cp "$work/sweep/B$B/data/energies_F.csv" "$data/field_sweep/energies_B$B.csv"
  # Rows are sites and columns are epochs, after a header row: average each row over its last 100 columns.
  awk -F, -v B="$B" 'NR > 1 { s = 0; for (k = NF-99; k <= NF; k++) s += $k; printf "%s,%d,%.6f\n", B, NR-1, s/100 }' \
    "$work/sweep/B$B/data/correlations_F.csv" >> "$data/field_sweep/correlations.csv"
done
echo "Done. Raw output is in $work"
