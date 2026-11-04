#!/bin/bash
# Usage: campaign.sh IMAGE OUTDIR REPS -- sub-phased startup runs, fresh process each,
# integer-sum N=1000 (kernel negligible), pinned to CPU 1.
I=$(realpath "$1"); O=$2; R=${3:-7}; D=$(cd "$(dirname "$0")" && pwd)
SHA=$(sha256sum "$I" | cut -d' ' -f1); mkdir -p "$O/phase-out" "$O/scratch"; SRC="tree-sha256:$(cat "$(dirname "$I")/tree-sha256.txt")"
for k in $(seq 1 $R); do
  ( ulimit -v 8388608; ulimit -c 0; env NELISP_LANE=on NELISP_LANE_IMAGE="$I" NELISP_LANE_IMAGE_SHA256=$SHA \
    NELISP_SCRATCH_ROOT="$O/scratch" BENCH_ROW=integer-sum BENCH_N=1000 BENCH_HIGH=1000 BENCH_WARMUPS=0 NELISP_PHASE_SOURCE_ID="$SRC" \
    taskset -c 1 python3 -I "$D/lane_runner_phased.py" --load placeholder --phase-out "$O/phase-out" > "$O/run$k.out" 2> "$O/run$k.err"; echo "run$k rc=$?" >> "$O/rc.txt" )
done
