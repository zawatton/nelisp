#!/bin/sh
# usage: cgrun.sh BIN TAG OP  -> cg/TAG-OP-{lo,hi}.cg, cg/TAG-OP.txt (Ir per trip by function)
BIN=$1; TAG=$2; OP=$3; LO=${LO:-200}; HI=${HI:-1000}
D=$(cd "$(dirname "$0")" && pwd); ROOTD=${TEXTPROP_OUT:-target/textprop}
for N in $LO $HI; do
  ( ulimit -v 8388608; OP=$OP BENCH_N=$N timeout 900 valgrind --tool=callgrind --callgrind-out-file=$ROOTD/cg/$TAG-$OP-$N.cg $BIN --load $D/op-driver.el > $ROOTD/cg/$TAG-$OP-$N.out 2>&1 )
done
python3 $D/cgdiff.py $ROOTD/cg/$TAG-$OP-$LO.cg $ROOTD/cg/$TAG-$OP-$HI.cg $((HI-LO)) $BIN > $ROOTD/cg/$TAG-$OP.txt
