#!/usr/bin/env bash
# Builds the variants of the v2.0.0 experiment copy into ./bin, or only the variants named as arguments.
#
# Usage: ./build.sh [VARIANT...]
#
# src/nnqs.F90 is src/nnqs.f90 of v2.0.0 with run-setting hooks and preprocessor switches. With no switch it is the
# released algorithm. Each BREAK_* switch puts back one defect of 1.0.5; FULL_SR replaces the block-diagonal S by
# the full S. "v2_dp" is v2 in double precision.
set -euo pipefail
cd "$(dirname "$(readlink -f "$0")")"
mkdir -p bin

ALL="BREAK_SAMPLER BREAK_THETA BREAK_BCONJ BREAK_ELOC BREAK_STABLE"
declare -A V=(
  [v2]=""
  [fsr]="FULL_SR"
  [none]="$ALL"
  [nogauge]="NO_GAUGE"
)
for d in $ALL; do V[no_${d#BREAK_}]="$d"; done                                      # Leave-one-out ablation

BASE="-O2 -march=native -ffree-line-length-none"
build() {  # build NAME DIR FLAGS
  local name=$1 dir=$2 flags=$3
  (cd "$dir" && fpm-13 build --compiler gfortran --flag "$flags" >/dev/null)
  local exe; exe=$(cd "$dir" && fpm-13 run --compiler gfortran --flag "$flags" --runner echo 2>/dev/null | grep 'app/nnqs$')
  cp "$dir/$exe" "bin/$name" && echo "$name"
}

names=("$@"); [ ${#names[@]} -eq 0 ] && names=("${!V[@]}" v2_dp)
for v in "${names[@]}"; do
  case $v in
    v2_dp)
      dp=$(mktemp -d); cp -r src app fpm.toml "$dp"/
      sed -i 's/rk=>real32/rk=>real64/' "$dp/src/nnqs.F90" "$dp/src/nnqs_images.F90" "$dp/app/main.f90"
      sed -i 's/sp=>real32/sp=>real64/; s/sppsvx/dppsvx/g; s/cppsvx/zppsvx/g' "$dp/src/nnqs_lapack.f90"
      build v2_dp "$dp" "$BASE"; rm -rf "$dp" ;;
    *) [ -v "V[$v]" ] || { echo "unknown variant $v" >&2; exit 1; }
       flags="$BASE"; for d in ${V[$v]}; do flags="$flags -D$d"; done
       build "$v" . "$flags" ;;
  esac
done
