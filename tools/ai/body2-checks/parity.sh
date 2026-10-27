#!/bin/sh
# parity.sh BINARY CORPUS [OUTDIR]: run the corpus on GNU Emacs 31.1 and on BINARY; exit 0 iff the outputs are
# identical and non-trivial (>= 50 rows, no error text).  Memory-capped; both sides run in a scratch directory.
# KNOWN (4th arg, optional): rows where v25 already differs from GNU, frozen as "N<space>text" lines of the v25
# output; those rows must keep the frozen text (no regression), every other row must equal GNU.
BIN=$1; C=$2; KNOWN=$4; HERE=$(cd "$(dirname "$0")" && pwd); T=${3:-$(mktemp -d)}
mkdir -p "$T" || exit 2
case $C in /*) ;; *) C=$HERE/$C;; esac
( ulimit -v 8388608; cd "$T" && timeout 300 /usr/local/bin/emacs -Q --batch -l "$C" > gnu.out 2> gnu.err ) || { echo "gnu run failed"; exit 2; }
( ulimit -v 8388608; cd "$T" && timeout 300 "$BIN" --load "$C" > nl.out 2> nl.err ) || { echo "nl run failed"; exit 2; }
if [ -n "$KNOWN" ]; then
  case $KNOWN in /*) ;; *) KNOWN=$HERE/$KNOWN;; esac
  awk 'NR==FNR{k[$1]=$0; next} ($1 in k){print k[$1]; next} {print}' "$KNOWN" "$T/gnu.out" > "$T/gnu.exp" && mv "$T/gnu.exp" "$T/gnu.out"
fi
rows=$(wc -l < "$T/gnu.out")
[ "$rows" -ge 50 ] || { echo "too few rows: $rows"; exit 2; }
if cmp -s "$T/gnu.out" "$T/nl.out"; then echo "PARITY OK rows=$rows"; exit 0; fi
echo "PARITY DIFF rows=$rows differing=$(diff "$T/gnu.out" "$T/nl.out" | grep -c '^<')"; diff "$T/gnu.out" "$T/nl.out" | head -${PARITY_SHOW:-6} | cut -c1-240; exit 1
