#!/bin/sh
# nc3-check.sh BINARY MODE   (run from the repository root)
#   off|vm|native|mutant : transcript of test/nelisp-nc3-corpus.el with the VM off, every
#                          converted record back on the word VM (NC_MODE=vm), as built, or
#                          with the negative-control mutant row selected
#   same   : off == vm == native, with and without NC_GC=1 (forced collections), >= 20 cases
#   mutant-caught : the mutant transcript differs from native
#   converted : all 22 nc3 corpus functions run native code
#   inline : (fset 'stringp ...) / (fset 'memq ...) are not seen by native callers
#            (GNU 31.1 byte-compiled: (t t))
#   gnu    : print the cases whose transcript differs from GNU Emacs (information only)
set -u
BIN=${1:?binary}; MODE=${2:?mode}
C=test/nelisp-nc3-corpus.el
T=${TMPDIR:-/tmp}/nc3chk.$$
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
          grep -q ' => ERR ' $T.off && { echo "a case errors as a whole gc=$g"; grep ' => ERR ' $T.off | cut -c1-200; rc=1; }
          cmp -s $T.off $T.vm || { echo "off/vm differ gc=$g"; diff $T.off $T.vm | head | cut -c1-300; rc=1; }
          cmp -s $T.off $T.nat || { echo "off/native differ gc=$g"; diff $T.off $T.nat | head | cut -c1-300; rc=1; }
        done; echo "cases=$(grep -c ' => ' $T.off)"; rm -f $T.*; exit $rc ;;
  converted) n=$(NC_REPORT=1 timeout 300 "$BIN" --load $C 2>&1 | sed -n 's/^NC3NATIVE //p')
        echo "nc3 native=$n"; test "${n:-0}" -eq 22 ;;
  inline) NC_INLINE=1 run native | grep -x 'inline-stringp-memq-fset => (t t)' ;;
  gnu) /usr/local/bin/emacs -Q --batch -l $C > $T.g 2>&1; run native > $T.n
        diff $T.g $T.n | grep '^[<>]' | cut -c1-200; rm -f $T.*; exit 0 ;;
  *) echo "unknown mode" >&2; exit 2 ;;
esac
