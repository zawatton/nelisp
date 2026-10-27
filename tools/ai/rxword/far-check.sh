#!/bin/sh
# usage: far-check.sh BINARY   Runs the survey far string-match driver from directories of many path lengths
# (the old aref-cursor collision made the cost depend on heap layout); exit 0 iff the per-call cost
# (T50-T12)/38 stays under FAR_LIMIT_US (default 2000) in every layout.
here=$(cd "$(dirname "$0")" && pwd); bin=$1; lim=${FAR_LIMIT_US:-2000}; tmp=$(mktemp -d -p /tmp); rc=0
cpu=${RXWORD_CPU:-}
run() { # dir n
  ( ulimit -v 8388608; BENCH_N=$2 timeout 120 ${cpu:+taskset -c $cpu} "$bin" --load "$1/drv/mb-mbstr-string-match-far.el" 2>&1 < /dev/null | sed -n 's/.*elapsed=\([0-9.]*\).*/\1/p' )
}
base=$(printf '%s' "$tmp/" | wc -c)
# The baseline cost flipped 110 us -> 60 ms for driver path lengths of about 139..144 characters (heap layout dependent).
for want in $(seq ${FAR_FROM:-130} ${FAR_TO:-156}); do
  k=$((want - base - 33)); [ "$k" -ge 1 ] || continue
  d=$tmp/$(printf 'x%.0s' $(seq 1 $k)); mkdir -p "$d/drv"; cp "$here/far/pre3.el" "$d/"; cp "$here/far/drv/mb-mbstr-string-match-far.el" "$d/drv/"
  a=$(run "$d" 12); b=$(run "$d" 50)
  us=$(awk -v a="$a" -v b="$b" 'BEGIN{ if (a==""||b=="") print -1; else printf "%.0f", (b-a)/38*1e6 }')
  echo "driver-path-length=$want per-call-us=$us"
  [ "$us" -ge 0 ] && [ "$us" -lt "$lim" ] || rc=1
done
rm -rf "$tmp"; exit $rc
