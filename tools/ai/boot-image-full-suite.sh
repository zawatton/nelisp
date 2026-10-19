#!/usr/bin/env bash
# Adapted from the eval/core lane's target/final-verify/checks.sh.
# Usage: boot-image-full-suite.sh [--threads-only] DYNAMIC STATIC TAG
# The focused mode is for development; S5 accepts only all eleven checks.
set -u
cd "$(dirname "$0")/../.." || exit 2
export TMPDIR="$PWD/target/tmp"
export XDG_CACHE_HOME="$PWD/target/tmp/cache"
mkdir -p "$TMPDIR" "$XDG_CACHE_HOME" || exit 2
mode=full
if [[ ${1:-} == --threads-only ]]; then mode=threads; shift; fi
[[ $# == 3 && $3 =~ ^[A-Za-z0-9_-]+$ ]] || exit 2
dyn=$(realpath "$1") || exit 2
st=$(realpath "$2") || exit 2
[[ -x $dyn && -x $st ]] || exit 2
o=target/boot-image/full-suite-$3
mkdir -p "$o/standalone" || exit 2
: > "$o/results.tsv"
failed=0
checked=0
r() {
  local n=$1 s rc
  shift
  s=$SECONDS
  "$@" >"$o/$n.log" 2>&1
  rc=$?
  printf '%s\t%d\t%d\n' "$n" "$rc" "$((SECONDS-s))" >> "$o/results.tsv"
  echo "$n rc=$rc $((SECONDS-s))s"
  checked=$((checked+1))
  if [[ $rc != 0 ]]; then failed=$((failed+1)); fi
}
if [[ $mode == full ]]; then
  r standalone-checks env EVAL_BIN="$dyn" STANDALONE_CHECK_LOGDIR="$o/standalone" test/perf/standalone-checks.sh
  for t in standalone-shallow-binding-check standalone-shallow-binding-dyn-check standalone-fx-call-shape-check standalone-bytecode-word-check; do
    r "$t" timeout 300 "$dyn" --load "test/$t.el"
  done
  r fx-spike timeout 300 "$dyn" --load test/perf/fx-spike-check.el
  r fx-corpus env EVAL_BIN="$dyn" FX_CORPUS_LOGDIR="$o/fx-corpus" test/perf/fx-corpus-check.sh
fi
r thread-standalone timeout 300 "$st" --load tools/nelisp-thread-standalone-smoke.el
repeat_thread() {
  local f=$1 i
  timeout 300 "$st" --load "$f" || return $?
  for i in 1 2 3 4 5; do
    (ulimit -v 4000000 && timeout 300 "$st" --load "$f") || return $?
  done
}
r thread-allocating repeat_thread tools/nelisp-thread-allocating-standalone-smoke.el
r thread-percpu repeat_thread tools/nelisp-thread-percpu-roots-smoke.el
r thread-mirror-guard timeout 300 "$st" --load tools/nelisp-thread-mirror-guard-smoke.el
if [[ $mode == full ]]; then
  grep -h 'STANDALONE-CHECK' "$o/standalone-checks.log" || failed=$((failed+1))
fi
echo "BOOT-IMAGE-FULL-SUITE mode=$mode checked=$checked findings=$failed"
[[ $failed == 0 ]]
