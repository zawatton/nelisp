#!/bin/sh
# Usage: parity.sh NELISP-BINARY [PRELOAD-FILE]
# Runs the corpus under GNU Emacs and NeLisp, diffs the output, and fails on any difference.
set -u
ROOT=$(cd "$(dirname "$0")/../../.." && pwd)
BIN=${1:?binary}
PRE=${2:-}
OUT=${RX2_OUT:-$ROOT/target/rx2/parity}
mkdir -p "$OUT"
( ulimit -v 8388608; timeout 300 /usr/local/bin/emacs -Q --batch -l "$ROOT/tools/ai/rx2/parity-corpus.el" > "$OUT/gnu.txt" 2> "$OUT/gnu.err" ) || { echo "GNU run failed"; exit 2; }
if [ -n "$PRE" ]; then
  ( ulimit -v 8388608; timeout 900 "$BIN" --load "$PRE" > "$OUT/nl.txt" 2> "$OUT/nl.err" ) || { echo "NeLisp run failed"; exit 2; }
else
  ( ulimit -v 8388608; timeout 1700 "$BIN" --load "$ROOT/tools/ai/rx2/parity-corpus.el" > "$OUT/nl.txt" 2> "$OUT/nl.err" ) || { echo "NeLisp run failed"; exit 2; }
fi
sed -i '/^nil$/d;/^"CORPUS-COUNT/,/^"$/d;${/^"$/d}' "$OUT/nl.txt"
if diff -q "$OUT/gnu.txt" "$OUT/nl.txt" > /dev/null; then echo "PARITY OK $(grep -c . "$OUT/gnu.txt") lines"; exit 0; fi
echo "PARITY DIFF: $(diff "$OUT/gnu.txt" "$OUT/nl.txt" | grep -c '^<') lines differ (see $OUT)"; exit 1
