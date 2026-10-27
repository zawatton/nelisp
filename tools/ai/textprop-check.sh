#!/bin/sh
# usage: textprop-check.sh corpus|fuzz|fuzz2|mutant|census BINARY   (run from the repository root)
# corpus: tools/ai/textprop1-corpus.el equal to GNU Emacs 31.1 (textprop1-check.sh has the allow-list)
# fuzz:   tools/ai/textprop1-fuzz.el, 3 seeds, equal to GNU
# fuzz2:  tools/ai/textprop-fuzz2.el (larger trees, LIMIT arguments, narrowing, edits), 4 seeds, equal to GNU
# mutant: the corpus and fuzz2 with tools/ai/textprop-mutant.el loaded must DIFFER from GNU (negative control)
# census: tools/ai/textprop-census.el: no stale row, and every tp function called is native except the
#         byte-code record-type helper behind the struct predicates
cd "$(dirname "$0")/../.." || exit 1
MODE=$1; BIN=$2
fuzz2() {
  OV=$1; bad=0; T=${TMPDIR:-/tmp}/textprop-fuzz2.$$; mkdir -p "$T"
  for s in 3 17 29 41; do
    TF_SEED=$s TF_RUNS=4 timeout 300 /usr/local/bin/emacs -Q --batch -l tools/ai/textprop-fuzz2.el > "$T/g" 2>/dev/null
    ( ulimit -v 8388608; if [ -n "$OV" ]; then TF_SEED=$s TF_RUNS=4 timeout 600 "$BIN" --load "$OV" --load tools/ai/textprop-fuzz2.el --; else TF_SEED=$s TF_RUNS=4 timeout 600 "$BIN" --load tools/ai/textprop-fuzz2.el --; fi ) > "$T/n" 2>/dev/null
    cmp -s "$T/g" "$T/n" || bad=$((bad+1))
    [ -s "$T/g" ] || bad=$((bad+1))
  done
  rm -rf "$T"; echo "fuzz2 seeds differing=$bad of 4"; return $bad
}
case $MODE in
corpus) sh tools/ai/textprop1-check.sh corpus "$BIN" ;;
fuzz) sh tools/ai/textprop1-check.sh fuzz "$BIN" ;;
fuzz2) fuzz2 "" ;;
mutant)
  # the controls only mean something when the unmutated run is green
  sh tools/ai/textprop1-check.sh corpus "$BIN" > /dev/null 2>&1 || { echo "corpus is red without the mutant"; exit 1; }
  fuzz2 "" > /dev/null 2>&1 || { echo "fuzz2 is red without the mutant"; exit 1; }
  sh tools/ai/textprop1-check.sh corpus "$BIN" tools/ai/textprop-mutant.el > /dev/null 2>&1 && { echo "mutant NOT caught by the corpus"; exit 1; }
  fuzz2 tools/ai/textprop-mutant.el > /dev/null 2>&1 && { echo "mutant NOT caught by fuzz2"; exit 1; }
  echo "mutant caught by corpus and fuzz2" ;;
census)
  out=$( ( ulimit -v 8388608; timeout 550 "$BIN" --load tools/ai/textprop-census.el 2>&1 ) )
  n=$(echo "$out" | grep -c '^SUMMARY'); echo "summaries=$n"
  bad=$(echo "$out" | awk '$1=="HOT" && $5!="native" && $3!="nelisp--record-type"')
  stale=$(echo "$out" | grep -c 'row-stale')
  [ -n "$bad" ] && echo "NOT NATIVE:" && echo "$bad" | head -20
  echo "stale=$stale"
  test -z "$bad" && test "$stale" = 0 && test "$n" -ge 24 ;;
esac
