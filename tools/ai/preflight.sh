#!/usr/bin/env bash
# Run the gates CI runs, in CI's order, WITHOUT stopping at the first failure.
#
# Why this exists: CI is fail-fast, and a full round on this repo costs about
# an hour and a half.  When a change breaks N independent gates -- which is the
# normal case for ratchet drift, where an ns-inventory count, a fallback pin
# and a smoke's preload can all move together -- fail-fast turns that into N
# rounds, discovered one at a time.  Running the same list locally with the
# failures COLLECTED instead of fatal turns it back into one pass.  The Stage 5
# landing used exactly this shape and went green on its first CI round.
#
# This does not replace CI.  It front-runs the cheap, deterministic part of it.
#
# Usage:
#   tools/ai/preflight.sh            # the fast tier (inventories + parity)
#   tools/ai/preflight.sh --full     # everything below, including the slow tiers
#   tools/ai/preflight.sh --list     # print the tiers and exit
#
# Exit status is the number of failing gates (0 = all clear), capped at 125.
set -u

FULL=0
case "${1:-}" in
  --full) FULL=1 ;;
  --list) sed -n '/^FAST_GATES=/,/^)/p;/^SLOW_GATES=/,/^)/p' "$0"; exit 0 ;;
  "") ;;
  *) echo "unknown option: $1" >&2; exit 2 ;;
esac

# Ordered to match .github/workflows/ci.yml's own Linux lane, so the first
# thing that fails here is the first thing that would fail there.
FAST_GATES=(
  "compile|make compile"
  "unsafe-inventory|make unsafe-inventory"
  "ns-inventory|make ns-inventory"
  "parens-check|make parens-check"
  "reader-surface-audit|make reader-surface-audit"
  "pkg-graph|make pkg-graph"
  "pkg-load-lists|make pkg-load-lists"
  "doc-claims|make doc-claims"
  "bench-aot-tco|make bench-aot-tco"
  "emacs-parity|make emacs-parity"
)
SLOW_GATES=(
  "checked-alloc|make standalone-reader-checked"
  "shadow-smoke|make standalone-reader-shadow-smoke"
  "check-tier|bash tools/ai/nelisp-ai.sh check"
  "extras|bash tools/ai/nelisp-ai.sh extras"
  "perf|bash tools/ai/nelisp-ai.sh perf"
  "ert-full|bash tools/ai/nelisp-ai.sh test"
)

# Gates that are REAL in CI but cannot be judged from a local run, so a local
# failure here is not evidence of a defect.  They are still run -- the output is
# worth seeing -- but they do not count toward the failure total, because a
# preflight that cries wolf teaches people to ignore its red, which costs more
# than the gate was ever going to save.
#
# binary-size-ratchet: the `size' pin in tools/nelisp-binary-size-baseline.txt
# is calibrated against CI's toolchain.  A local build of the SAME commit
# measures larger -- 8,033,688 B locally vs 7,976,232 B in CI on efa22af65, a
# ~57KB gap that is environment, not source.  Comparing a local absolute size
# to that pin therefore reports a failure CI will not have, and "fixing" it by
# raising the pin would eat the real CI headroom (48KB there) the ratchet
# exists to guard.  Judge it either from CI's own
# `[binary-size-ratchet] PASS: ... is N bytes' line, or by building BASE and
# HEAD locally and comparing only the DELTA.
ADVISORY_GATES=(
  "binary-size-ratchet|make binary-size-ratchet"
)

OUT=${PREFLIGHT_LOGDIR:-target/preflight}
mkdir -p "$OUT"
declare -a NAMES STATUS COUNTS
declare -a ADV_NAMES ADV_STATUS ADV_COUNTS
# Assign, do not merely declare: under `set -u` a never-assigned array makes
# ${#ADV_NAMES[@]} in the summary an unbound-variable error, which is what
# the summary printed instead of the advisory table.
ADV_NAMES=(); ADV_STATUS=(); ADV_COUNTS=()

run_gate() {
  local name=${1%%|*} cmd=${1#*|}
  printf '\n########## %s ##########\n%s\n' "$name" "$cmd"
  local log="$OUT/$name.log"
  # Deliberately NOT `set -e' and deliberately not short-circuiting: the whole
  # point is to learn about every failure in one pass.
  eval "$cmd" > "$log" 2>&1
  local rc=$?
  local gc
  gc=$(grep -aoE 'GATE-COUNT checked=[0-9]+ findings=[0-9]+' "$log" | tail -1)
  [ -z "$gc" ] && gc="(no GATE-COUNT line)"
  NAMES+=("$name"); COUNTS+=("$gc")
  if [ $rc -eq 0 ]; then STATUS+=("pass"); else STATUS+=("FAIL rc=$rc"); fi
  printf 'rc=%s  %s\n' "$rc" "$gc"
  # A gate that reports checked=0 crashed rather than ran; that distinction has
  # been mistaken for a slowdown before, so call it out here.
  case "$gc" in *"checked=0 "*) printf '  !! checked=0 -- this gate DIED, it did not merely regress\n';; esac
}

for g in "${FAST_GATES[@]}"; do run_gate "$g"; done
if [ $FULL -eq 1 ]; then
  for g in "${SLOW_GATES[@]}"; do run_gate "$g"; done
  for g in "${ADVISORY_GATES[@]}"; do
    run_gate "$g"
    # Move the just-recorded row into the advisory bucket.
    last=$(( ${#NAMES[@]} - 1 ))
    ADV_NAMES+=("${NAMES[$last]}"); ADV_STATUS+=("${STATUS[$last]}")
    ADV_COUNTS+=("${COUNTS[$last]}")
    unset 'NAMES[last]' 'STATUS[last]' 'COUNTS[last]'
    NAMES=("${NAMES[@]}"); STATUS=("${STATUS[@]}"); COUNTS=("${COUNTS[@]}")
  done
fi

fails=0
printf '\n================ PREFLIGHT SUMMARY ================\n'
for i in "${!NAMES[@]}"; do
  printf '%-24s %-12s %s\n' "${NAMES[$i]}" "${STATUS[$i]}" "${COUNTS[$i]}"
  case "${STATUS[$i]}" in FAIL*) fails=$((fails+1));; esac
done
printf '==================================================\n'
if [ ${#ADV_NAMES[@]} -gt 0 ]; then
  printf 'ADVISORY (run, but NOT counted -- cannot be judged locally):\n'
  for i in "${!ADV_NAMES[@]}"; do
    printf '  %-22s %-12s %s\n' "${ADV_NAMES[$i]}" "${ADV_STATUS[$i]}" "${ADV_COUNTS[$i]}"
  done
  printf '  see this script'"'"'s ADVISORY_GATES comment for how to judge these\n'
  printf '==================================================\n'
fi
printf 'logs: %s\n' "$OUT"
if [ $fails -eq 0 ]; then
  printf 'PREFLIGHT: all %d gate(s) clear\n' "${#NAMES[@]}"
else
  printf 'PREFLIGHT: %d of %d gate(s) FAILED -- fix all of them before pushing\n' \
    "$fails" "${#NAMES[@]}"
fi
[ $fails -gt 125 ] && fails=125
exit $fails
