#!/usr/bin/env bash
# Replay prior ledgers in one heavy slot; replay the new S1.3 verbatim.
set -u
: "${NOTES_ROOT:?Set NOTES_ROOT to the Notes workspace}"
ulimit -v 8388608
out=target/wasm-intern-symbol/regressions
mkdir -p "$out"
failures=0
run() {
  local label=$1; shift
  "$@" > "$out/$label.log" 2>&1
  local rc=$?
  echo "$rc" > "$out/$label.rc"
  echo "$label rc=$rc"
  if (( rc != 0 )); then failures=$((failures+1)); fi
}
ledger() {
  local lane=$1 command
  command=$(sed -n '/^\*\* S1.3 /,$s/^cmd: //p' "tools/ai/wasm-$lane-progress.org")
  test -n "$command" || exit 2
  printf '%s\n' "$command" > "$out/$lane.command"
  if [[ $lane != intern-symbol ]]; then
    # The caller already holds a slot. Preserve every build/check/assertion,
    # removing only the inner launcher; node keeps that same parent slot.
    command=${command/'"$NOTES_ROOT/bin/heavy-run.sh" '/}
  fi
  printf '%s\n' "$command" > "$out/$lane.executed-command"
  mkdir -p "target/wasm-$lane"
  run "$lane" bash -c "$command"
  cp "target/wasm-$lane/current.log" "$out/$lane-build.log"
  cp "target/wasm-$lane/check.log" "$out/$lane-check.log"
  rg '^GATE-COUNT ' "$out/$lane-check.log" || true
}
case ${1:-all} in
  prior)
    # Invoke this mode only inside heavy-run; children inherit that slot.
    run genuine-accept tools/ai/wasm-genuine-alloc-accept.sh
    for lane in target-init target-alloc genuine-alloc; do ledger "$lane"; done
    ;;
  intern) ledger intern-symbol ;;
  all)
    run prior "$NOTES_ROOT/bin/heavy-run.sh" "$0" prior
    run intern "$0" intern
    ;;
  *) echo 'usage: wasm-intern-symbol-regression.sh [all|prior|intern]' >&2; exit 2 ;;
esac
(( failures == 0 ))
