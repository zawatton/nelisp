#!/bin/sh
# Usage: check-scaling.sh BINARY sm|buf [PRELOAD]
# Exit 0 iff the scaling probe reports no row whose cost grows with the subject size.
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
BIN=${1:?binary}; KIND=${2:?sm|buf}; PRE=${3:-}
OUT=${RX2_OUT:-$ROOT/target/rx2/scale}; mkdir -p "$OUT"
F="$ROOT/tools/ai/rx2/scale-$KIND.el"
if [ -n "$PRE" ]; then cat "$PRE" "$F" > "$OUT/run-$KIND.el"; F="$OUT/run-$KIND.el"; fi
( ulimit -v 8388608; timeout 280 "$BIN" --load "$F" > "$OUT/$KIND.txt" 2>&1 )
grep -q '^SCALING-FAILURES 0$' "$OUT/$KIND.txt"
