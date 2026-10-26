#!/bin/sh
# nc1-native-check.sh BINARY MODE   (run from the repository root)
#   off|vm|native|mutant : transcript of test/nelisp-nc-native-corpus.el with the VM off,
#                          converted records back on the VM, as built, or with the mutant
#   same   : off == vm == native, with and without NC_GC=1 (forced collections), >= 20 cases
#   mutant-caught : the mutant transcript differs from native
#   converted N : at least N distinct records were converted at build
#   inline-car : (fset 'car ...) does not reach native code (GNU compiled: t)
#   fx-same : the fx corpus (test/nelisp-fx-adopt-corpus.el) gives the same transcript
#             with the VM off, on the VM and on native code, and >= 70 corpus functions ran native
set -u
BIN=${1:?binary}; MODE=${2:?mode}
C=test/nelisp-nc-native-corpus.el
T=${TMPDIR:-/tmp}/nc1nat.$$
ulimit -v 8388608
run() { # $1 mode
  case $1 in
    off) NELISP_FX_DISABLE=1 timeout 300 "$BIN" --load $C 2>&1 ;;
    native) timeout 300 "$BIN" --load $C 2>&1 ;;
    *) NC_MODE=$1 timeout 300 "$BIN" --load $C 2>&1 | grep -v '^MODE ' ;;
  esac
}
case $MODE in
  off|vm|native|mutant) run $MODE ;;
  same) rc=0
        for g in "" 1; do
          NC_GC=$g run off > $T.off; NC_GC=$g run vm > $T.vm; NC_GC=$g run native > $T.nat
          test "$(grep -c ' => ' $T.off)" -ge 20 || rc=1
          cmp -s $T.off $T.vm || { echo "off/vm differ gc=$g"; diff $T.off $T.vm | head; rc=1; }
          cmp -s $T.off $T.nat || { echo "off/native differ gc=$g"; diff $T.off $T.nat | head; rc=1; }
        done; rm -f $T.*; exit $rc ;;
  mutant-caught) run native > $T.nat; run mutant > $T.mut
        test -s $T.nat && ! cmp -s $T.nat $T.mut; rc=$?; diff $T.nat $T.mut | head -4; rm -f $T.*; exit $rc ;;
  converted) n=$(NC_REPORT=1 timeout 300 "$BIN" --load $C 2>&1 | sed -n 's/^CONVERTED [0-9]* (\(.*\))$/\1/p' | tr ' ' '\n' | sort -u | grep -c .)
        echo "converted=$n"; test "${n:-0}" -ge "${3:?N}" ;;
  inline-car) NC_INLINE=1 run native | grep -x 'inline-car-fset => t' ;;
  fx-same) F="--load test/nelisp-nc-corpus-driver.el --load test/nelisp-fx-adopt-corpus.el"
        NELISP_FX_DISABLE=1 FXC_PRINT=1 timeout 300 "$BIN" --load test/nelisp-fx-adopt-corpus.el 2>&1 | grep -v '^t$\|^nil$' > $T.off
        NC_MODE=vm FXC_PRINT=1 FXC_FORCE=1 timeout 300 "$BIN" $F 2>&1 | grep -v '^t$\|^nil$' > $T.vm
        echo '(nc-corpus-report)' > $T.rep.el
        FXC_PRINT=1 FXC_FORCE=1 timeout 300 "$BIN" $F --load $T.rep.el 2>&1 | grep -v '^t$\|^nil$' > $T.nat
        n=$(sed -n 's/^NCCORPUS native=//p' $T.nat); grep -v '^NCCORPUS' $T.nat > $T.nat2; echo "native corpus functions=$n"
        test "$(grep -c ' => ' $T.off)" -ge 100 && cmp -s $T.off $T.vm && cmp -s $T.off $T.nat2 && test "${n:-0}" -ge 70; rc=$?
        [ $rc = 0 ] || { diff $T.off $T.vm | head -5; diff $T.off $T.nat2 | head -10; }; rm -f $T.*; exit $rc ;;
  *) echo "unknown mode" >&2; exit 2 ;;
esac
