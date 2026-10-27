#!/bin/bash
# Usage: rowtime.sh BINARY|gnu ROW N  -> BENCH-CHECK line (elapsed seconds) for survey4 driver ROW.
# SURVEY4 (env) points at the survey4 directory holding drv/ and pre4.el.
B=$1; R=$2; N=$3
S=${SURVEY4:?set SURVEY4 to the survey4 directory}
cd "$S/drv" || exit 2
if [ "$B" = gnu ]; then
  BENCH_N=$N timeout 300 /usr/local/bin/emacs -Q --batch -l "$S/drv/$R.el" 2>&1 | grep BENCH-CHECK
else
  (ulimit -v 8388608; BENCH_N=$N timeout 300 "$B" --load "$S/drv/$R.el" -- 2>&1 | grep BENCH-CHECK)
fi
