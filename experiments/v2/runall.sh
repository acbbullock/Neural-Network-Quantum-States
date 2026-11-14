#!/bin/bash
# Runs every job file, 30 jobs at a time, the long chains first. Each line of results.txt is one run.
cd "$(dirname "$(readlink -f "$0")")"
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
cat jobs_long.txt jobs_fullsr64.txt jobs_ablation_small.txt | xargs -P 30 -L 1 ./run1.sh >> results.txt
echo DONE >> results.txt
