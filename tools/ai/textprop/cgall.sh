#!/bin/sh
# cgall.sh BIN TAG : callgrind Ir/call for the operations into $TEXTPROP_OUT/cg/TAG-all.txt (default target/textprop)
D=$(cd "$(dirname "$0")" && pwd); ROOTD=${TEXTPROP_OUT:-target/textprop}
BIN=$1; TAG=$2
mkdir -p $ROOTD/cg; rm -f $ROOTD/cg/$TAG-all.txt
for op in none gtp nspc pt atp rtp0 rtp tpa; do
  LO=100 HI=500 /home/madblack-21/Cowork/Notes/bin/heavy-run.sh sh $D/cgrun.sh $BIN $TAG $op
  echo "$op $(head -1 $ROOTD/cg/$TAG-$op.txt)" >> $ROOTD/cg/$TAG-all.txt
done
echo done > $ROOTD/cg/$TAG-all.done
