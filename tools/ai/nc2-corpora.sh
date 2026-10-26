#!/bin/sh
# nc2-corpora.sh BINARY OUTFILE   (run from the repository root)
# The bufedit / bufnative / textprop1 / timefmt lane corpora against GNU Emacs 31.1 on
# BINARY.  Writes one summary line per corpus to OUTFILE:
#   bufedit-edit gnu-diff=N   bufedit-corpus gnu-diff=N   bufnative-corpus gnu-diff=N
#   textprop1 corpus rc=R / fuzz rc=R   timefmt rc=R mismatches=N
# (gnu-diff = lines of GNU's transcript the binary does not reproduce).  Compare two
# binaries' OUTFILEs: a lane keeps every count at or below the baseline's.
set -u
BIN=${1:?binary}; OUT=${2:?outfile}
W=$(cd "$(dirname "$0")/../../.." && pwd)
BE=$W/bufedit-20261102/target/bufedit/corpus
BN=$W/bufnative-20261102/target/bufnative/corpus
TF=$W/timefmt-20261102/target/timefmt/corpus
T=${TMPDIR:-/tmp}/nc2corp.$$; mkdir -p $T
: > "$OUT"
gd() { # gnu-file engine-out -> lines of GNU not reproduced
  diff "$1" "$2" | grep -c '^<'; }
( ulimit -v 8388608
  cd $BE && timeout 600 "$BIN" --load edit.el > $T/edit.out 2>/dev/null
  cd $BE && timeout 600 "$BIN" --load corpus.el > $T/be.out 2>/dev/null
  cd $BN && timeout 600 "$BIN" --load corpus.el > $T/bn.out 2>/dev/null )
echo "bufedit-edit gnu-diff=$(gd $BE/edit-gnu.out $T/edit.out) lines=$(wc -l < $T/edit.out)" >> "$OUT"
echo "bufedit-corpus gnu-diff=$(gd $BE/gnu.out $T/be.out) lines=$(wc -l < $T/be.out)" >> "$OUT"
echo "bufnative-corpus gnu-diff=$(gd $BN/gnu.out $T/bn.out) lines=$(wc -l < $T/bn.out)" >> "$OUT"
sh tools/ai/textprop1-check.sh corpus "$BIN" > $T/tp.c 2>&1; echo "textprop1 corpus rc=$? $(head -1 $T/tp.c)" >> "$OUT"
sh tools/ai/textprop1-check.sh fuzz "$BIN" > $T/tp.f 2>&1; echo "textprop1 fuzz rc=$? $(head -1 $T/tp.f)" >> "$OUT"
MAXBAD=0 sh $TF/check.sh "$BIN" > $T/tf 2>&1; echo "timefmt rc=$? $(grep '^MATCH' $T/tf)" >> "$OUT"
rm -rf $T
cat "$OUT"
