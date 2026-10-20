#!/bin/sh
# Run every test/standalone-*-check.el (or the explicit file arguments)
# on $EVAL_BIN, one at a time; exit
# non-zero if any fails that is not listed in
# test/perf/standalone-checks-known-red.txt.  The evaluator ledger's
# regression guard.
# Optional arguments select explicit check files for the same isolated runner.
cd "$(dirname "$0")/../.." || exit 2
: "${EVAL_BIN:?set EVAL_BIN to a built nelisp binary}"
fail=0
count=0
logdir=${STANDALONE_CHECK_LOGDIR:-target/standalone-checks}
mkdir -p "$logdir" || exit 2
# Optional exact paths keep a focused edit from running the entire corpus.
# Validate every path before starting any process; the default remains all.
if [ "$#" = 0 ]; then set -- test/standalone-*-check.el; fi
for f do
  case "$f" in
    test/standalone-*-check.el) ;;
    *) echo "STANDALONE-CHECK invalid check path: $f" >&2; exit 2 ;;
  esac
  [ -f "$f" ] || { echo "STANDALONE-CHECK missing check: $f" >&2; exit 2; }
done
for f do
  count=$((count+1))
  name=$(basename "$f" .el)
  code=0
  timeout 300 nice -n 10 "$EVAL_BIN" --load "$f" >"$logdir/$name.out" 2>"$logdir/$name.err" || code=$?
  # The reader may continue after an uncaught top-level error and exit 0.
  # A later PASS line cannot turn that failed invocation into a valid check.
  if [ "$code" = 0 ] && grep -q '^nelisp: uncaught error:' "$logdir/$name.err"; then code=1; fi
  if [ "$code" = 0 ]; then
    # A golden reference check must match all output, even when --load
    # returns zero after catching an error or silently skipping a case.
    expected=${f%.el}.expected
    if [ -f "$expected" ]; then
      if [ -s "$logdir/$name.err" ] || ! cmp -s "$expected" "$logdir/$name.out"; then
        echo "STANDALONE-CHECK FAIL $f (GNU output mismatch; $logdir/$name.out, $logdir/$name.err)"; fail=$((fail+1))
      fi
    fi
  else
    if grep -q "^$f " test/perf/standalone-checks-known-red.txt; then
      echo "STANDALONE-CHECK known-red $f (exit $code; $logdir/$name.err)"
    else
      echo "STANDALONE-CHECK FAIL $f (exit $code; $logdir/$name.out, $logdir/$name.err)"; fail=$((fail+1))
    fi
  fi
done
if [ "$count" = 0 ]; then echo "STANDALONE-CHECK FAIL: zero cases"; fail=$((fail+1)); fi
[ $fail = 0 ] && echo "STANDALONE-CHECKS PASS"
echo "GATE-COUNT: $count cases, $fail failures"
exit $fail
