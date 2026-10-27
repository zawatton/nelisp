#!/bin/sh
# nc4-check.sh BINARY MODE   (run from the repository root)
#   off|vm|native|mutant|fxir : transcript of test/nelisp-nc4-corpus.el with the VM off
#        (interpreter), every converted record back on the word VM (NC_MODE=vm), as
#        built, with the negative-control mutant selected, or through the fxir
#        reference evaluator (NC_FXIR=1)
#   same   : off == vm == native == fxir, with and without NC_GC=1, >= 35 cases
#   gnu    : native == GNU Emacs 31.1 (-Q --batch)
#   mutant-caught : NC_HMUTANT=1 (a clause's condition spec replaced in the live
#        code vector) changes the native and the VM transcripts
#   converted : NC4NATIVE = number of nc4 corpus functions running native code
#   fx     : NC4FX = all nc4 corpus functions have fx records
set -u
BIN=${1:?binary}; MODE=${2:?mode}
C=test/nelisp-nc4-corpus.el
T=${TMPDIR:-/tmp}/nc4chk.$$
ulimit -v 8388608
run() { # $1 mode
  case $1 in
    off) NELISP_FX_DISABLE=1 timeout 300 "$BIN" --load $C 2>&1 | grep -v '^nil$' ;;
    native) timeout 300 "$BIN" --load $C 2>&1 | grep -v '^nil$' ;;
    vm) NC_MODE=vm timeout 300 "$BIN" --load $C 2>&1 | grep -v '^MODE \|^nil$' ;;
    fxir) NC_FXIR=1 timeout 300 "$BIN" --load $C 2>&1 | grep -v '^FXIR \|^nil$' ;;
  esac
}
case $MODE in
  off|vm|native|fxir) run $MODE ;;
  same) rc=0
        for g in "" 1; do
          NC_GC=$g run off > $T.off; NC_GC=$g run vm > $T.vm; NC_GC=$g run native > $T.nat; NC_GC=$g run fxir > $T.fx
          test "$(grep -c ' => ' $T.off)" -ge 35 || rc=1
          for m in vm nat fx; do cmp -s $T.off $T.$m || { echo "off/$m differ gc=$g"; diff $T.off $T.$m | head | cut -c1-300; rc=1; }; done
        done; echo "cases=$(grep -c ' => ' $T.off)"; rm -f $T.*; exit $rc ;;
  gnu) /usr/local/bin/emacs -Q --batch -l $C > $T.g 2>&1; run native > $T.n
       cmp -s $T.g $T.n; rc=$?; diff $T.g $T.n | head | cut -c1-300; rm -f $T.*; exit $rc ;;
  mutant-caught) rc=0
       run native > $T.n; NC_HMUTANT=1 run native | grep -v '^HMUTANT' > $T.m
       test -s $T.n && ! cmp -s $T.n $T.m || rc=1; diff $T.n $T.m | head -4 | cut -c1-200
       run vm > $T.v; NC_HMUTANT=1 run vm | grep -v '^HMUTANT' > $T.vm2
       test -s $T.v && ! cmp -s $T.v $T.vm2 || rc=1; diff $T.v $T.vm2 | head -4 | cut -c1-200
       rm -f $T.*; exit $rc ;;
  converted) n=$(NC_REPORT=1 timeout 300 "$BIN" --load $C 2>&1 | sed -n 's/^NC4NATIVE //p')
        echo "nc4 native=$n"; test "${n:-0}" -eq 36 ;;
  fx) n=$(NC_REPORT=1 timeout 300 "$BIN" --load $C 2>&1 | sed -n 's/^NC4FX //p')
        echo "nc4 fx=$n"; test "${n:-0}" -eq 36 ;;
  *) echo "unknown mode" >&2; exit 2 ;;
esac
