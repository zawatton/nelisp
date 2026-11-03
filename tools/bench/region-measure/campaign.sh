#!/bin/bash
# Usage: RM_IMAGE=... campaign.sh OUTDIR  -- runs protocol-frozen.json once (7 reps, fresh processes, CPU 1).
D=$(cd "$(dirname "$0")" && pwd); O=${1:?outdir}; I=${RM_IMAGE:?set RM_IMAGE}
mkdir -p "$O" || exit 1; : > "$O/raw.txt"
run() { (ulimit -v 8388608; ulimit -c 0; cd "$D" && env "$@" taskset -c 1 timeout 600 "$I" --load "$D/workload.el" < /dev/null 2>>"$O/stderr.txt" | grep '^RM ' | sed "s/^/rep=$rep /" >> "$O/raw.txt"); }
for rep in 0 1 2 3 4 5 6; do
  if [ $((rep % 2)) = 0 ]; then gv="none vector region"; wv="vector region"; else gv="region vector none"; wv="region vector"; fi
  for v in $gv; do run RM_ROW=gc-pause RM_VARIANT=$v; done
  for v in $wv; do for n in 262144 1048576; do run RM_ROW=word-access RM_VARIANT=$v RM_N=$n; done; done
done
sha256sum "$I" "$D/protocol-frozen.json" "$D/workload.el" > "$O/inputs.sha256"
wc -l < "$O/raw.txt"
