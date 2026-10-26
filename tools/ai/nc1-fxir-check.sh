#!/bin/sh
# nc1-fxir-check.sh BINARY MODE   (run from the repository root)
#   vm-off | vm-on | ir : the fx corpus transcript (interpreter, word VM, fxir evaluator)
#   same   : fail unless the three transcripts are identical (t/nil lines dropped as in
#            tools/ai/fxadopt-check.sh) and non-empty
#   mutant : fail unless the deliberately wrong evaluator (NELISP_FXIR_MUTANT=1) differs
set -u
BIN=${1:?binary}; MODE=${2:?mode}
CORPUS=test/nelisp-fx-adopt-corpus.el
T=${TMPDIR:-/tmp}/nc1fxir.$$
ulimit -v 8388608
run() {
  case $1 in
    off) NELISP_FX_DISABLE=1 FXC_PRINT=1 timeout 300 "$BIN" --load $CORPUS 2>&1 ;;
    on) FXC_PRINT=1 FXC_FORCE=1 timeout 300 "$BIN" --load $CORPUS 2>&1 ;;
    ir) NELISP_FXIR_ROOT=$PWD FXC_PRINT=1 FXC_FORCE=1 timeout 300 "$BIN" --load test/nelisp-fxir-corpus-driver.el --load $CORPUS 2>&1 ;;
  esac
}
case $MODE in
  vm-off) run off ;;
  vm-on) run on ;;
  ir) run ir ;;
  same) run off | grep -v '^t$\|^nil$' > $T.off; run on | grep -v '^t$\|^nil$' > $T.on; run ir | grep -v '^t$\|^nil$' > $T.ir
        test "$(grep -c ' => ' $T.off)" -ge 100 && cmp -s $T.off $T.on && cmp -s $T.off $T.ir; rc=$?
        [ $rc = 0 ] || { diff $T.off $T.ir | head -20; }; rm -f $T.off $T.on $T.ir; exit $rc ;;
  mutant) run ir | grep -v '^t$\|^nil$' > $T.ir; NELISP_FXIR_MUTANT=1 run ir | grep -v '^t$\|^nil$' > $T.mut
        test -s $T.ir && ! cmp -s $T.ir $T.mut; rc=$?; rm -f $T.ir $T.mut; exit $rc ;;
  *) echo "unknown mode" >&2; exit 2 ;;
esac
