#!/bin/bash
# tools/ai/eval-core-progress.org S2.2: the generated corpus (test/perf/fx-corpus.el)
# gives byte-identical output on the word VM and with NELISP_FX_DISABLE=1 (the
# interpreter), for two seeds, and the VM runs at least MIN_COVERED percent of it.
cd "$(dirname "$0")/../.." || exit 2
: "${EVAL_BIN:?set EVAL_BIN to a built nelisp binary}"
min=${MIN_COVERED:-60}
if [ -n "${FX_CORPUS_LOGDIR:-}" ]; then
  dir=$FX_CORPUS_LOGDIR
  mkdir -p "$dir" || exit 2
else
  dir=$(mktemp -d) || exit 2
  trap 'rm -rf "$dir"' EXIT
fi
fail=0
for seed in 1 7; do
  if FX_CORPUS_SEED=$seed timeout 600 "$EVAL_BIN" --load test/perf/fx-corpus.el > "$dir/vm$seed" 2>&1; then
    vm_rc=0
  else vm_rc=$?; fail=1; fi
  if NELISP_FX_DISABLE=1 FX_CORPUS_SEED=$seed timeout 600 "$EVAL_BIN" --load test/perf/fx-corpus.el > "$dir/in$seed" 2>&1; then
    in_rc=0
  else in_rc=$?; fail=1; fi
  echo "FX-CORPUS seed $seed: vm_rc=$vm_rc interpreter_rc=$in_rc"
  calls=$(grep -c ' => ' "$dir/vm$seed")
  cov=$(sed -n 's/^FX-CORPUS covered=\([0-9]*\)\/\([0-9]*\)$/\1 \2/p' "$dir/vm$seed")
  set -- $cov
  if [ -z "$cov" ] || [ "$calls" -lt 600 ]; then
    echo "FX-CORPUS seed $seed: incomplete run ($calls calls)"; tail -3 "$dir/vm$seed"; fail=1; continue
  fi
  if ! diff <(grep ' => ' "$dir/in$seed") <(grep ' => ' "$dir/vm$seed") > "$dir/diff$seed" 2>&1; then :; fi
  ndiff=$(grep -c '^[<>]' "$dir/diff$seed")
  echo "FX-CORPUS seed $seed: $calls calls, covered $1/$2, $ndiff differing lines"
  [ "$ndiff" = 0 ] || { head -6 "$dir/diff$seed"; fail=1; }
  [ $(( $1 * 100 )) -ge $(( $2 * min )) ] || { echo "FX-CORPUS coverage below $min%"; fail=1; }
done
[ $fail = 0 ] && echo "FX-CORPUS PASS"
exit $fail
