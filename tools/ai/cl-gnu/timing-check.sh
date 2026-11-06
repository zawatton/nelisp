#!/usr/bin/env bash
# Usage: CLGNU_BIN=BINARY timing-check.sh [TOLERANCE_PERCENT] [BASELINE]
# Re-measure BINARY (measure.sh) and compare each median against the baseline
# medians (default baseline-build140.txt).  A metric passes when
#   median <= baseline * (1 + tol/100) + SLACK_MS
# (default tol 25, SLACK_MS 20, so millisecond-scale metrics are not flaky).
# Exit 0 iff every metric passes.
here=$(cd "$(dirname "$0")" && pwd) || exit 1
tol=${1:-25}; base=${2:-$here/baseline-build140.txt}; slack=${SLACK_MS:-20}
[ -x "${CLGNU_BIN:-}" ] || { echo "CLGNU_BIN is not an executable" >&2; exit 2; }
now=$(bash "$here/measure.sh" "$CLGNU_BIN" "${RUNS:-7}") || { echo "$now"; exit 1; }
rc=0
for m in cold_startup_ms pcase_loop_ms bytecomp_require_ms; do
  b=$(grep -E "^$m " "$base" | awk '{print $2}')
  n=$(printf '%s\n' "$now" | grep -E "^$m " | awk '{print $2}')
  [ -n "$b" ] && [ -n "$n" ] || { echo "$m missing (base=$b now=$n)"; rc=1; continue; }
  limit=$(( b + b * tol / 100 + slack ))
  if [ "$n" -le "$limit" ]; then echo "PASS $m now=$n base=$b limit=$limit"
  else echo "FAIL $m now=$n base=$b limit=$limit"; rc=1; fi
done
exit $rc
