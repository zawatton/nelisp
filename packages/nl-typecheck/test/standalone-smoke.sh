#!/bin/sh
# Doc 236 S3.5: load nl-typecheck on a standalone NeLisp binary and check
# the clash fixture.  Usage: standalone-smoke.sh /path/to/target/nelisp
# Needs a binary built elsewhere; this script never builds one.
BIN="$1"
test -n "$BIN" && test -x "$BIN" || { echo "standalone-smoke: NELISP_BIN not executable: $BIN"; exit 2; }
cd "$(dirname "$0")/../../.." || exit 1
SRC=packages/nl-typecheck/src
OUT=$("$BIN" --load scripts/nl-typecheck-standalone-smoke.el -- \
  "$SRC" packages/nl-typecheck/test/fixtures/clash.el 2>&1) || { echo "$OUT"; exit 1; }
echo "$OUT"
echo "$OUT" | grep -q 'clash.el:[0-9]*:[0-9]*: error: ' || exit 1
exit 0
