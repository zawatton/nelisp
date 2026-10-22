#!/usr/bin/env bash
# integ-wh-gcport.sh [BIN]: gc-port's light set (gc-stats, budget, vm-poll) on the merged dynamic binary,
# which fills both linkage slots.  Prints each script's verdict line (GC-PORT-<NAME> PASS|FAIL) and
# exits non-zero when one fails; target/final-verify/integ-wh-gcport.log is what `integ-wh-check.sh log
# gcport' reads.  The budget and vm-poll checks need a VM that polls at every Bcall (the callpath stage).
cd "$(dirname "$0")/../.." || exit 2
BIN=$(realpath "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
W=${GCP_WORK:-$PWD/target/integ-wh/gcport}; mkdir -p "$W"
fail=0
for x in gc-stats budget vm-poll; do
  work=(--work "$W/$x"); [ "$x" = vm-poll ] && work=()   # vm-poll's check has no --work
  (ulimit -v 8388608; timeout 3000 python3 tools/ai/gc-port-$x.py check --dyn "$BIN" --static "$BIN" "${work[@]}" > "$W/$x.out" 2>&1) || fail=1
  tail -n 1 "$W/$x.out"
done
exit $fail
