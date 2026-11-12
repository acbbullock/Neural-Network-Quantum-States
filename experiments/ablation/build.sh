#!/usr/bin/env bash
# Builds every variant of the experiment copy into ./bin, or only the variants named as arguments.
#
# Usage: ./build.sh [VARIANT...]
#
# Each variant is the 1.0.5 code in src/nnqs.F90 with some of its preprocessor switches defined. The fixed variant
# "clean" is built from ../fixed instead. "srnk_dp" is srnk in double precision, built from a copy with rk=real64.
set -euo pipefail
cd "$(dirname "$(readlink -f "$0")")"
mkdir -p bin

ALL="FIX_THETA FIX_ADAM FIX_ELOC FIX_BCONJ FIX_STABLE FIX_SAMPLER NO_ADAM NO_KICK"
declare -A V=(
  [base]=""
  [theta]="FIX_THETA"
  [all]="FIX_THETA FIX_ADAM FIX_ELOC FIX_BCONJ"
  [all5]="FIX_THETA FIX_ADAM FIX_ELOC FIX_BCONJ FIX_STABLE"
  [all6]="FIX_THETA FIX_ADAM FIX_ELOC FIX_BCONJ FIX_STABLE FIX_SAMPLER"
  [samp]="FIX_STABLE FIX_SAMPLER"
  [sr]="FIX_THETA FIX_ADAM FIX_ELOC FIX_BCONJ FIX_STABLE FIX_SAMPLER NO_ADAM"
  [srnk]="$ALL"
  [fsr]="$ALL FULL_SR"
  [nk_only]="NO_ADAM NO_KICK"
)
for drop in FIX_SAMPLER FIX_THETA FIX_BCONJ FIX_ELOC FIX_STABLE; do                       # Leave-one-out ablation
  V[no_${drop#FIX_}]=$(echo $ALL | tr ' ' '\n' | grep -vx $drop | tr '\n' ' ')
done

build() {  # build NAME DIR FLAGS: builds the app in DIR with FLAGS and copies it to bin/NAME
  local name=$1 dir=$2 flags=$3
  (cd "$dir" && fpm-13 build --compiler gfortran --flag "$flags" >/dev/null)
  local exe; exe=$(cd "$dir" && fpm-13 run --compiler gfortran --flag "$flags" --runner echo 2>/dev/null | grep 'app/nnqs$')
  cp "$dir/$exe" "bin/$name" && echo "$name"
}

names=("$@"); [ ${#names[@]} -eq 0 ] && names=("${!V[@]}" clean srnk_dp)
for v in "${names[@]}"; do
  case $v in
    clean) build clean ../fixed "-O2 -march=native" ;;
    srnk_dp)
      dp=$(mktemp -d); cp -r src app fpm.toml "$dp"/; rm "$dp/app/gradcheck.f90"                # single precision only
      sed -i 's/rk=>real32/rk=>real64/' "$dp/src/nnqs.F90" "$dp/src/nnqs_images.F90" "$dp/app/main.f90"
      sed -i 's/sp=>real32/sp=>real64/; s/sppsvx/dppsvx/g; s/cppsvx/zppsvx/g' "$dp/src/nnqs_lapack.f90"
      build srnk_dp "$dp" "-O2 -march=native $(printf -- '-D%s ' $ALL)" ;;
    *) [ -v "V[$v]" ] || { echo "unknown variant $v" >&2; exit 1; }
       flags="-O2 -march=native"; for d in ${V[$v]}; do flags="$flags -D$d"; done
       build "$v" . "$flags" ;;
  esac
done
