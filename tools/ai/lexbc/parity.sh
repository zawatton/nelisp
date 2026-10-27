#!/bin/sh
# lexbc parity: run tools/ai/lexbc/corpus.el on GNU Emacs 31.1 and on BIN; exit 0 iff equal.
# usage: parity.sh BIN [mutant-caught]
#   mutant-caught: compare BIN against GNU's output with one result altered; exit 0 iff
#   the runner reports that divergence (negative control).
# env LEXBC_OVERLAY (optional, absolute path) is loaded first on the NeLisp side.
D=$(cd "$(dirname "$0")" && pwd)
BIN=$(realpath "$1") || exit 2
OUT=$(mktemp -d) || exit 2
trap 'rm -rf "$OUT"' EXIT
( ulimit -v 8388608; cd "$OUT" && LEXBC_OVERLAY= TZ=UTC0 LC_ALL=C timeout 200 /usr/local/bin/emacs -Q --batch -l "$D/driver.el" > gnu.txt 2> gnu.err ) || { echo "GNU run failed"; exit 2; }
( ulimit -v 8388608; cd "$OUT" && TZ=UTC0 LC_ALL=C timeout 200 "$BIN" --load "$D/driver.el" > nl.txt 2> nl.err ) || { echo "NeLisp run failed rc=$?"; tail -3 "$OUT/nl.err"; }
n=$(wc -l < "$OUT/gnu.txt")
[ "$n" -gt 500 ] || { echo "GNU produced only $n cases"; exit 2; }
if [ "$2" = mutant-caught ]; then
  sed '7s/\t.*/\tMUTANT/' "$OUT/gnu.txt" > "$OUT/gnu.mut"
  if cmp -s "$OUT/gnu.mut" "$OUT/nl.txt"; then echo "MUTANT NOT CAUGHT"; exit 1; fi
  diff "$OUT/gnu.mut" "$OUT/nl.txt" | grep -q '^< 7	MUTANT' && { echo "MUTANT CAUGHT"; exit 0; }
  echo "MUTANT NOT CAUGHT"; exit 1
fi
if cmp -s "$OUT/gnu.txt" "$OUT/nl.txt"; then echo "PARITY-OK cases=$n"; exit 0; fi
echo "PARITY-DIFF cases=$n diverging=$(diff "$OUT/gnu.txt" "$OUT/nl.txt" | grep -c '^>')"
diff "$OUT/gnu.txt" "$OUT/nl.txt" | head -${LEXBC_DIFF_LINES:-20} | cut -c1-240
exit 1
