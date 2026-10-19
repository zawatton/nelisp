#!/bin/sh
# Run every test/standalone-*-check.el on $EVAL_BIN, one at a time; exit
# non-zero if any fails that is not listed in
# test/perf/standalone-checks-known-red.txt.  The evaluator ledger's
# regression guard.
cd "$(dirname "$0")/../.." || exit 2
: "${EVAL_BIN:?set EVAL_BIN to a built nelisp binary}"
fail=0
logdir=${STANDALONE_CHECK_LOGDIR:-target/standalone-checks}
mkdir -p "$logdir" || exit 2
# Optional exact paths keep a focused edit from running the entire corpus.
# Validate every path before starting any process; the default remains all.
if [ "$#" = 0 ]; then set -- test/standalone-*-check.el; fi
for f do
  case "$f" in
    test/standalone-*-check.el) [ -f "$f" ] || exit 2 ;;
    *) echo "invalid standalone check: $f" >&2; exit 2 ;;
  esac
done
for f do
  name=$(basename "$f" .el)
  if timeout 300 nice -n 10 "$EVAL_BIN" --load "$f" >"$logdir/$name.out" 2>"$logdir/$name.err"; then :;
  else
    code=$?
    if grep -q "^$f " test/perf/standalone-checks-known-red.txt; then
      echo "STANDALONE-CHECK known-red $f (exit $code; $logdir/$name.err)"
    else
      echo "STANDALONE-CHECK FAIL $f (exit $code; $logdir/$name.out, $logdir/$name.err)"; fail=1
    fi
  fi
done
[ $fail = 0 ] && echo "STANDALONE-CHECKS PASS"
exit $fail
