#!/usr/bin/env bash
# integ-wh-battery.sh [-b BIN] UNIT...: run the verification units of tools/ai/integ-wh-progress.org
# that make the logs its `log' criteria read, each as one heavy unit (bin/heavy-run.sh) under
# ulimit -v 8 GiB, units in parallel.  Prints one line per unit when it ends; logs go to
# target/final-verify/ (the criteria's inputs) and target/integ-wh/battery/.
#
#   checks     target/final-verify/checks.sh (11 groups) -> integ-wh-summary.log, checks-integ-wh/
#   fchecks    batch F's fixture checks -> integ-wh-fchecks.log
#   mcp        anvil-standalone smoke (40 tools) -> integ-wh-mcp.log
#   golden     two-generation symbol-name golden (S2.7) -> integ-wh-golden.log
#   scale      21,000-symbol at-scale image check (S2.8, about six minutes) -> integ-wh-scale.log
#   gcport     gc-port's gc-stats, budget and vm-poll -> integ-wh-gcport.log (the `log gcport' input),
#              then mxcache and alloc-golden -> integ-wh-gcport2.log
#   weakmark   fix 20's regression check (S2.15)
#   arity      callpath's builtin arity sweep -> integ-wh-arity/report.json
#   evalcall   callpath's evaluator-call fixture and the stash check
#   all        every unit above
# BIN defaults to target/integ/nelisp-dyn and fills both linkage slots.  A unit's exit status is its
# verdict; the exit status of this script is the number of failed units.
SELF=$(realpath "$0")
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
BIN=${INTEG_WH_DYN:-target/integ/nelisp-dyn}
if [ "${1:-}" = -b ]; then BIN=$2; shift 2; fi
BIN=$(realpath "$BIN") || exit 2
export INTEG_WH_DYN=$BIN
H=${HEAVY_RUN:-}
if [ -z "$H" ]; then
  d=$ROOT
  while [ "$d" != / ]; do [ -x "$d/bin/heavy-run.sh" ] && { H=$d/bin/heavy-run.sh; break; }; d=$(dirname "$d"); done
fi
[ -x "$H" ] || { echo "heavy-run.sh not found; set HEAVY_RUN"; exit 2; }
FV=$ROOT/target/final-verify; B=$ROOT/target/integ-wh/battery; mkdir -p "$FV" "$B"

body() {  # body UNIT: the command of a unit, run from the worktree root; its stdout is the unit's log
  case $1 in
    checks)   bash target/final-verify/checks.sh "$BIN" "$BIN" integ-wh ;;
    fchecks)  bash tools/ai/integ-wh-fchecks.sh ;;
    mcp)      bash tools/ai/integ-wh-mcp-smoke.sh "$BIN" ;;
    golden)   bash tools/ai/integ-wh-image-golden.sh "$BIN" ;;
    scale)    bash tools/ai/integ-wh-image-scale.sh "$BIN" ;;
    gcport)   bash tools/ai/integ-wh-gcport.sh "$BIN" ;;
    weakmark) bash tools/ai/integ-wh-check.sh weakmark ;;
    arity)    rm -rf "$B/arity"; mkdir -p "$B/arity"
              python3 tools/ai/callpath-perf-arity.py --binary "$BIN" --out "$B/arity" --linkage dynamic || return 1
              mkdir -p "$FV/integ-wh-arity"; cp "$B/arity/report.json" "$FV/integ-wh-arity/report.json" ;;
    evalcall) python3 tools/ai/callpath-perf-check.py evalcall --binary "$BIN" &&
              python3 tools/ai/callpath-perf-check.py stash ;;
    *) echo "unknown unit $1"; return 2 ;;
  esac
}
logof() {
  case $1 in
    checks)   echo "$FV/integ-wh-summary.log" ;;
    fchecks)  echo "$FV/integ-wh-fchecks.log" ;;
    mcp)      echo "$FV/integ-wh-mcp.log" ;;
    scale)    echo "$FV/integ-wh-scale.log" ;;
    gcport)   echo "$FV/integ-wh-gcport.log" ;;
    golden)   echo "$FV/integ-wh-golden.log" ;;
    *)        echo "$B/$1.log" ;;
  esac
}

if [ "${INTEG_WH_UNIT:-}" ]; then  # re-entered under heavy-run.sh for one unit
  u=$INTEG_WH_UNIT; log=$(logof "$u")
  ulimit -v 8388608
  s=$(date +%s); body "$u" > "$log" 2>&1; rc=$?
  if [ "$u" = gcport ]; then   # gc-port's two further checks, kept out of the log that holds exactly three verdicts
    : > "$FV/integ-wh-gcport2.log"
    for x in mxcache alloc-golden; do
      timeout 3000 python3 tools/ai/gc-port-$x.py check --dyn "$BIN" --static "$BIN" > "$B/gcport-$x.out" 2>&1 || rc=$((rc + 1000))
      tail -n 1 "$B/gcport-$x.out" >> "$FV/integ-wh-gcport2.log"
    done
  fi
  echo "UNIT $u rc=$rc $(( $(date +%s) - s ))s $log"
  exit $rc
fi

units=("$@"); [ "${units[*]:-}" = all ] && units=(checks fchecks mcp golden scale gcport weakmark arity evalcall)
[ ${#units[@]} -gt 0 ] || { sed -n '2,/^# verdict/p' "$SELF" | cut -c3-; exit 2; }
pids=()
for u in "${units[@]}"; do
  INTEG_WH_UNIT=$u "$H" bash "$SELF" &
  pids+=($!)
done
fails=0
for p in "${pids[@]}"; do wait "$p" || fails=$((fails + 1)); done
echo "BATTERY done: $fails of ${#units[@]} units failed"
exit $fails
