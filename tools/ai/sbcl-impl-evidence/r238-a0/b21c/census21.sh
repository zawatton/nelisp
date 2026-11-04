#!/bin/bash
# Doc 238 A0.2: census on the census build (21c) over the driver rows, reader probe, print diff, standalone checks.
N=$HOME/.cache/tmp/nelisp-sbcl-impl-builds; R=$N/run; T=$HOME/.cache/tmp/nelisp-sbcl-impl-f12-20261031
E=$T/tools/ai/sbcl-impl-evidence; I=$N/build-on21c/nelisp-dynamic; D=$R/b21c; O=$N/census21; mkdir -p $O
grep -q '^rc=0' $N/build-on21c/build.log && test -x $I || { echo BUILD-FAILED > $O/status; exit 1; }
SHA=$(sha256sum $I | cut -d' ' -f1); echo "$SHA" > $O/image.sha256
ulimit -v 8388608; ulimit -c 0
sh $R/repin.sh $I $N/build-on21c/src $D > $O/repin.log 2>&1 || { echo REPIN-FAILED > $O/status; exit 1; }
cp $D/benchmark-driver-native-on.el $D/drv.orig
A=$(grep -n "^(nelisp-phase-event 'APPLICATION_STATE_READY)" $D/drv.orig | cut -d: -f1); L=$(grep -n '^(kill-emacs' $D/drv.orig | cut -d: -f1)
{ head -$((A-1)) $D/drv.orig; cat $R/census-dump.el; echo '(r238-census-dump "startup")'; sed -n "${A},$((L-1))p" $D/drv.orig; echo '(r238-census-dump "workload")'; sed -n "${L},\$p" $D/drv.orig; } > $D/benchmark-driver-native-on.el
sh $R/rows.sh $I $D $D/rows > $O/rows.txt 2>&1
for f in $D/rows/*.out; do r=$(basename $f .out); grep '^CENSUS' $f | sed "s/^CENSUS /$r\t/"; done > $O/census-rows.tsv
cp $D/drv.orig $D/benchmark-driver-native-on.el
mk() { { cat $R/census-dump.el; echo "(nelisp--repr-census t)"; echo "(load \"$1\" nil t)"; echo "(r238-census-dump \"$2\")"; } > $O/$2-wrap.el; }
mk $E/s2-reader/reader-probe.el reader; ( cd $O && timeout 300 $I --load $O/reader-wrap.el < /dev/null > $O/reader.out 2>&1 )
mk $T/tools/bench/semantic-diff/print-cases.el print; ( cd $O && timeout 300 $I --load $O/print-wrap.el < /dev/null > $O/print.out 2>&1 )
G=$O/gates; mkdir -p $G; ( cd $T && EVAL_BIN=$I STANDALONE_CHECK_LOGDIR=$G sh test/perf/standalone-checks.sh < /dev/null > $G/all.txt 2>&1; echo "rc=$?" >> $G/all.txt )
echo CENSUS-DONE > $O/status
