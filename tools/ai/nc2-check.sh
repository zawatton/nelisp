#!/bin/sh
# nc2-check.sh BINARY MODE   (run from the repository root)
#   off|vm|native : transcript of test/nelisp-nc2-corpus.el with the VM off, with every
#                   converted record back on the word VM (NC_MODE=vm), as built
#   same   : off == vm == native, with and without NC_GC=1 (forced collections), >= 20 cases
#   mutant-caught : the negative-control mutant (a wrong `<' in one row, NC_MODE=mutant)
#            changes the transcript
#   fxir   : the fxir reference evaluator over the hot set (target/nc2/hotset.txt minus the
#            names the evaluator itself calls) gives the VM's transcript (any binary)
#   gnu    : print the cases whose transcript differs from GNU Emacs (information only)
set -u
BIN=${1:?binary}; MODE=${2:?mode}
C=test/nelisp-nc2-corpus.el
T=${TMPDIR:-/tmp}/nc2chk.$$
ulimit -v 8388608
run() { # $1 mode
  case $1 in
    off) NELISP_FX_DISABLE=1 timeout 300 "$BIN" --load $C 2>&1 ;;
    native) timeout 300 "$BIN" --load $C 2>&1 ;;
    vm|mutant) NC_MODE=$1 timeout 300 "$BIN" --load $C 2>&1 | grep -v '^MODE ' ;;
  esac
}
case $MODE in
  off|vm|native|mutant) run $MODE ;;
  mutant-caught) run native > $T.nat; run mutant > $T.mut
        test -s $T.nat && ! cmp -s $T.nat $T.mut; rc=$?; diff $T.nat $T.mut | head -4 | cut -c1-200; rm -f $T.*; exit $rc ;;
  same) rc=0
        for g in "" 1; do
          NC_GC=$g run off > $T.off; NC_GC=$g run vm > $T.vm; NC_GC=$g run native > $T.nat
          test "$(grep -c ' => ' $T.off)" -ge 20 || rc=1
          cmp -s $T.off $T.vm || { echo "off/vm differ gc=$g"; diff $T.off $T.vm | head | cut -c1-300; rc=1; }
          cmp -s $T.off $T.nat || { echo "off/native differ gc=$g"; diff $T.off $T.nat | head | cut -c1-300; rc=1; }
        done; echo "cases=$(grep -c ' => ' $T.off)"; rm -f $T.*; exit $rc ;;
  fxir) grep -vx 'nreverse\|copy-sequence\|reverse\|sequencep\|nelisp--check-seq-list\|nelisp--fx-lambda\|error\|nelisp--record-set\|nelisp--record-length' \
          target/nc2/hotset.txt > $T.names
        run native | grep -v '^MODE ' > $T.on
        NC_FXIR=1 NC_FXIR_NAMES=$T.names timeout 300 "$BIN" --load $C > $T.ir 2>&1
        n=$(sed -n 's/^MODE fxir selected=//p' $T.ir); echo "fxir functions=$n"
        grep -v '^MODE ' $T.ir | cmp -s - $T.on && test "$(grep -c ' => ' $T.on)" -ge 20 && test "${n:-0}" -ge 100; rc=$?
        [ $rc = 0 ] || grep -v '^MODE ' $T.ir | diff - $T.on | head | cut -c1-300; rm -f $T.*; exit $rc ;;
  gnu) /usr/local/bin/emacs -Q --batch -l $C > $T.g 2>&1; run native > $T.n
        diff $T.g $T.n | grep '^[<>]' | cut -c1-200; rm -f $T.*; exit 0 ;;
  *) echo "unknown mode" >&2; exit 2 ;;
esac
