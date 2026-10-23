#!/usr/bin/env bash
# symbols-name-word-checks.sh DYN STATIC TAG : the COMMON.md full lane check suite (11 groups), a tracked copy of the batch's
# target/final-verify/checks.sh, which is untracked.  Logs go to target/final-verify/checks-TAG/.
cd "$(dirname "$0")/../.." || exit 2
dyn=$(realpath "$1"); st=$(realpath "$2"); o=target/final-verify/checks-$3; mkdir -p "$o"
fail=0; count=0
r() { n=$1; shift; s=$(date +%s); "$@" >"$o/$n.log" 2>&1; code=$?; echo "$n rc=$code $(( $(date +%s)-s ))s"; count=$((count+1)); [ "$code" = 0 ] || fail=$((fail+1)); }
r standalone-checks env EVAL_BIN="$dyn" test/perf/standalone-checks.sh
for t in standalone-shallow-binding-check standalone-shallow-binding-dyn-check standalone-fx-call-shape-check standalone-bytecode-word-check; do
  r $t timeout 300 "$dyn" --load test/$t.el; done
r fx-spike timeout 300 "$dyn" --load test/perf/fx-spike-check.el
r fx-corpus env EVAL_BIN="$dyn" test/perf/fx-corpus-check.sh
r thread-standalone timeout 300 "$st" --load tools/nelisp-thread-standalone-smoke.el
r thread-allocating sh -c "timeout 300 '$st' --load tools/nelisp-thread-allocating-standalone-smoke.el && for i in 1 2 3 4 5; do ( ulimit -v 4000000; timeout 300 '$st' --load tools/nelisp-thread-allocating-standalone-smoke.el ) || exit 1; done"
r thread-percpu sh -c "timeout 300 '$st' --load tools/nelisp-thread-percpu-roots-smoke.el && for i in 1 2 3 4 5; do ( ulimit -v 4000000; timeout 300 '$st' --load tools/nelisp-thread-percpu-roots-smoke.el ) || exit 1; done"
r thread-mirror-guard timeout 300 "$st" --load tools/nelisp-thread-mirror-guard-smoke.el
grep -h "STANDALONE-CHECK" "$o/standalone-checks.log"
echo "GATE-COUNT: $count cases, $fail failures"
exit "$fail"
