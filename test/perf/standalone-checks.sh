#!/bin/sh
# Run every test/standalone-*-check.el on $EVAL_BIN, one at a time; exit
# non-zero if any fails that is not listed in
# test/perf/standalone-checks-known-red.txt.  The evaluator ledger's
# regression guard.
cd "$(dirname "$0")/../.." || exit 2
: "${EVAL_BIN:?set EVAL_BIN to a built nelisp binary}"
fail=0
for f in test/standalone-*-check.el; do
  if timeout 300 nice -n 10 "$EVAL_BIN" --load "$f" >/dev/null 2>&1; then :;
  elif grep -q "^$f " test/perf/standalone-checks-known-red.txt; then
    echo "STANDALONE-CHECK known-red $f"
  else
    echo "STANDALONE-CHECK FAIL $f"; fail=1
  fi
done
[ $fail = 0 ] && echo "STANDALONE-CHECKS PASS"
exit $fail
