#!/bin/sh
# usage: check.sh BINARY [fuzz|corpus|gnutests|print|bufsmoke]   (run from the repo root)
BIN=$1; MODE=${2:-fuzz}; D=tools/ai/overlay1; T=${TMPDIR:-/tmp}/overlay1-check.$$
ulimit -v 8388608; export LC_ALL=C.UTF-8
mkdir -p "$T"; trap 'rm -rf "$T"' EXIT
GNU=${GNU_EMACS:-emacs}
# The binary echoes the value of the last top-level form; GNU does not.
nl_run() { timeout 300 "$BIN" --load "$1" -- 2>&1 | sed '${/^\(t\|nil\)$/d}'; }
case $MODE in
fuzz)
  for f in fuzz fuzz2; do
    nl_run $D/$f.el > "$T/$f.nl"
    timeout 300 $GNU -Q --batch -l $D/$f.el > "$T/$f.gnu" 2>&1
    [ -s "$T/$f.gnu" ] || { echo "$f: empty GNU output"; exit 1; }
    cmp -s "$T/$f.nl" "$T/$f.gnu" || { echo "$f: output differs from GNU"; diff "$T/$f.gnu" "$T/$f.nl" | head -5; exit 1; }
    echo "$f: $(wc -l < "$T/$f.gnu") lines identical to GNU"
  done ;;
corpus)
  nl_run $D/corpus.el > "$T/c.nl"
  timeout 300 $GNU -Q --batch -l $D/corpus.el > "$T/c.gnu" 2>&1
  diff "$T/c.gnu" "$T/c.nl" > "$T/c.diff"
  # Two known, understood differences (see corpus.expected.diff): a marker printed
  # after its temp buffer died, and GNU's uninitialised sortvec for non-numeric priorities.
  diff "$T/c.diff" $D/corpus.expected.diff > /dev/null || { echo "corpus: unexpected differences"; cat "$T/c.diff" | head -20; exit 1; }
  echo "corpus: $(grep -c '=>' "$T/c.gnu") forms, only the 2 known differences" ;;
gnutests)
  python3 $D/gnutests.py "$BIN" src/buffer-tests.el --expect-pass $D/buffer-tests.pass.txt ;;
baseline) timeout 60 "$BIN" --load $D/baseline.el 2>&1 | grep -qx VOID ;;
print)
  nl_run $D/print.el > "$T/p.nl"
  timeout 100 $GNU -Q --batch -l $D/print.el > "$T/p.gnu" 2>&1
  [ -s "$T/p.gnu" ] || { echo "print: empty GNU output"; exit 1; }
  cmp -s "$T/p.nl" "$T/p.gnu" || { echo "print: differs from GNU"; diff "$T/p.gnu" "$T/p.nl" | head; exit 1; }
  echo "print: $(wc -l < "$T/p.gnu") lines identical to GNU" ;;
esac
