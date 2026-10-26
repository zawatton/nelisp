#!/bin/sh
# usage: check.sh BINARY MODE   (run from the repo root)
# MODE: seqcl | json | float | prn | jsonel  (corpus equals GNU except the known pre-existing cases;
#       jsonel loads the repo's packages/nelisp-emacs-foundation/src/json.el over the binary first)
#       scaling-seqcl | scaling-json | scaling-prn | speed
BIN=$1; MODE=$2; D=tools/ai/algo1
ulimit -v 8388608; export LC_ALL=C.UTF-8
T=${TMPDIR:-/tmp}/algo1-check.$$; mkdir -p $T
case $MODE in
  scaling-*) timeout 280 "$BIN" --load $D/${MODE}.el --; exit $? ;;
  speed) timeout 120 "$BIN" --load $D/speed.el --; exit $? ;;
esac
case $MODE in
  seqcl) F=corpus-seqcl.el; X='^$' ;;
  json) F=corpus-json.el; X='^(346|523|524|525|526|531|532|533|534|567|568|569|570):' ;;
  float) F=corpus-float.el; X='^$' ;;
  prn) F=corpus-prn.el; X='^(344|353|362|371|380|389|416|425|434|443|470|479|523|529|530|531|532|534|535|536|537|538|539|541|547|548|549|550|552|553|554|555|556|557):' ;;
  jsonel) F=corpus-jsonel.el; X='^$' ;;
  *) echo "unknown mode"; exit 2 ;;
esac
LOADF=$D/$F
if [ "$MODE" = jsonel ]; then LOADF=$T/wrap.el; printf '(load "%s" nil t)\n(load "%s" nil t)\n' "$PWD/packages/nelisp-emacs-foundation/src/json.el" "$PWD/$D/$F" > $LOADF; fi
timeout 250 "$BIN" --load $LOADF -- 2>&1 | grep -a -E '^[0-9]+:' | grep -a -v -E "$X" > $T/nl
timeout 250 /usr/local/bin/emacs -Q --batch -l $D/$F 2>&1 | grep -a -E '^[0-9]+:' | grep -a -v -E "$X" > $T/gnu
test -s $T/nl && diff $T/nl $T/gnu > $T/d; rc=$?
[ $rc -ne 0 ] && head -5 $T/d
rm -rf $T; exit $rc
