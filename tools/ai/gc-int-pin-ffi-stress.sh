#!/bin/sh
# Run the ffi-loader stress probe RUNS times; pass only if no run saw a
# failed open or a failed openat probe.  Usage: gc-int-pin-ffi-stress.sh BIN [RUNS]
bin=${1:-./target/nelisp}; runs=${2:-3}; i=0
while [ $i -lt $runs ]; do
  out=$(STRESS_N=30 timeout 600 "$bin" --load tools/ai/gc-int-pin-ffi-stress.el 2>&1 | grep '^STRESS' | head -1)
  echo "$out" | cut -c1-160
  echo "$out" | grep -q 'open-errs=0 probe-fails=0' || exit 1
  i=$((i+1))
done
