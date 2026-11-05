#!/usr/bin/env bash
# Usage: measure.sh BINARY [RUNS]
# Wall-clock medians (ms) over RUNS fresh processes (default 7, minimum 5):
#   cold_startup_ms        --load of an empty file
#   pcase_loop_ms          --load bench-pcase.el (6-pattern pcase, 10^5 calls; includes startup)
#   bytecomp_require_ms    --load bench-bytecomp.el ((require 'bytecomp); includes startup)
# Prints "<metric> <median_ms>" lines; also "<metric>_runs <ms ...>".
here=$(cd "$(dirname "$0")" && pwd) || exit 1
bin=${1:?usage: measure.sh BINARY [RUNS]}; runs=${2:-7}
[ -x "$bin" ] || { echo "not an executable: $bin" >&2; exit 2; }
[ "$runs" -ge 5 ] || { echo "RUNS must be >= 5" >&2; exit 2; }
one() {  # one MODULE: run once, print elapsed ms; fail on a missing marker
  local s e out
  s=$(date +%s%N)
  out=$( (ulimit -v 8388608; ulimit -c 0; timeout 300 "$bin" --load "$here/bench-$1.el" 2>&1) )
  e=$(date +%s%N)
  case "$1" in
    pcase) printf '%s\n' "$out" | grep -aq 'BENCH-PCASE-SUM 328566' || return 1 ;;
    bytecomp) printf '%s\n' "$out" | grep -aq 'BENCH-BYTECOMP t' || return 1 ;;
  esac
  echo $(( (e - s) / 1000000 ))
}
for pair in empty:cold_startup_ms pcase:pcase_loop_ms bytecomp:bytecomp_require_ms; do
  mod=${pair%%:*}; name=${pair##*:}; vals=()
  for _ in $(seq "$runs"); do
    v=$(one "$mod") || { echo "$name FAILED"; exit 1; }
    vals+=("$v")
  done
  med=$(printf '%s\n' "${vals[@]}" | sort -n | sed -n "$(( (runs + 1) / 2 ))p")
  echo "$name $med"
  echo "${name}_runs ${vals[*]}"
done
