#!/usr/bin/env bash
# word-heap-final-checks.sh DYN STATIC TAG : COMMON's eleven final groups.
# The census is a preflight; the existing runtime groups stay unchanged.
cd "$(dirname "$0")/../.." || exit 2
export TMPDIR="$PWD/target/tmp"
mkdir -p "$TMPDIR" || exit 2
[ "$#" = 3 ] || { echo "usage: $0 DYN STATIC TAG" >&2; exit 2; }
dyn=$(realpath "$1") || exit 2
st=$(realpath "$2") || exit 2
o=target/final-verify/checks-$3
mkdir -p "$o" || exit 2
fail=0; count=0
r() { n=$1; shift; s=$(date +%s); "$@" >"$o/$n.log" 2>&1; code=$?; echo "$n rc=$code $(( $(date +%s)-s ))s"; count=$((count+1)); [ "$code" = 0 ] || fail=$((fail+1)); }
# GNU bytecomp.el:3845-3865 warns on free variables during compilation.
# This post-link check catches NeLisp's unsafe hidden-slot lowering until
# the separate compiler fix supplies an equivalent compile-time diagnostic.
if ! { python3 tools/ai/native-free-symbol-census.py check "$dyn" --max 0 &&
       python3 tools/ai/native-free-symbol-census.py check "$st" --max 0;
     } >"$o/native-free-symbol.log" 2>&1; then
  cat "$o/native-free-symbol.log"
  echo "WORD-HEAP FINAL CHECKS FAIL: native free-symbol preflight"
  exit 1
fi
r standalone-checks env EVAL_BIN="$dyn" test/perf/standalone-checks.sh
for t in standalone-shallow-binding-check standalone-shallow-binding-dyn-check standalone-fx-call-shape-check standalone-bytecode-word-check; do
  r "$t" timeout 300 "$dyn" --load "test/$t.el"; done
r fx-spike timeout 300 "$dyn" --load test/perf/fx-spike-check.el
r fx-corpus env EVAL_BIN="$dyn" test/perf/fx-corpus-check.sh
r thread-standalone timeout 300 "$st" --load tools/nelisp-thread-standalone-smoke.el
r thread-allocating sh -c "timeout 300 '$st' --load tools/nelisp-thread-allocating-standalone-smoke.el && for i in 1 2 3 4 5; do ( ulimit -v 4000000; timeout 300 '$st' --load tools/nelisp-thread-allocating-standalone-smoke.el ) || exit 1; done"
r thread-percpu sh -c "timeout 300 '$st' --load tools/nelisp-thread-percpu-roots-smoke.el && for i in 1 2 3 4 5; do ( ulimit -v 4000000; timeout 300 '$st' --load tools/nelisp-thread-percpu-roots-smoke.el ) || exit 1; done"
r thread-mirror-guard timeout 300 "$st" --load tools/nelisp-thread-mirror-guard-smoke.el
grep -h "STANDALONE-CHECK" "$o/standalone-checks.log"
echo "GATE-COUNT: $count cases, $fail failures"
exit "$fail"
