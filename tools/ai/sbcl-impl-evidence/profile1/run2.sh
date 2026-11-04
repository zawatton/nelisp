#!/bin/bash
# usage: run1.sh row n kernelsym
B=$HOME/.cache/tmp/nelisp-sbcl-impl-builds; R=$B/run; I=$B/build-on/nelisp-dynamic; P=$R/profile1
row=$1; n=$2; sym=$3
S=$(mktemp -d $P/scratch-$row-XXXX); mkdir $S/cache
cd $R
ulimit -v 8388608; ulimit -c 0
env BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256=09cbbb4f3274ed6f246605a0de35265a4719bf0393e35530a6379290d5cd1a79 BENCH_ROW=$row BENCH_N=$n BENCH_HIGH=$n BENCH_WARMUPS=0 TMPDIR=$S XDG_CACHE_HOME=$S/cache NELISP_OWNED_SCRATCH=$S \
 timeout 1500 valgrind --tool=callgrind --collect-atstart=yes --dump-instr=yes --separate-callers=2 --callgrind-out-file=$P/callgrind.$row.$n.out $I --load $R/benchmark-driver-native-on.el > $P/$row.$n.stdout 2> $P/$row.$n.stderr
echo rc=$? >> $P/$row.$n.stderr
