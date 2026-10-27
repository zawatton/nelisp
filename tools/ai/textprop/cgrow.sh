#!/bin/sh
# cgrow.sh BIN TAG SURVEY ROW LO HI: callgrind Ir per trip by function of one resurvey24 driver -> $TEXTPROP_OUT/cg/TAG-ROW.txt
D=$(cd "$(dirname "$0")" && pwd); ROOTD=${TEXTPROP_OUT:-target/textprop}
R=${RESURVEY24:-$(cd "$D/../../../../resume-4943-20261024/target/resurvey24" && pwd)}
BIN=$1; TAG=$2; SV=$3; ROW=$4; LO=$5; HI=$6
mkdir -p $ROOTD/cg
for N in $LO $HI; do
  ( ulimit -v 8388608; BENCH_N=$N timeout 1500 valgrind --tool=callgrind --callgrind-out-file=$ROOTD/cg/$TAG-$ROW-$N.cg $BIN --load $R/survey$SV/drv/$ROW.el > $ROOTD/cg/$TAG-$ROW-$N.out 2>&1 )
done
python3 $D/cgdiff.py $ROOTD/cg/$TAG-$ROW-$LO.cg $ROOTD/cg/$TAG-$ROW-$HI.cg $((HI-LO)) $BIN > $ROOTD/cg/$TAG-$ROW.txt
