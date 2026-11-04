#!/bin/bash
N=$1; S=/home/madblack-21/.cache/tmp/alloc-scan-measure/s$N; mkdir -p $S/cache
ulimit -v 8388608; ulimit -c 0
env ALLOC_OUT=/home/madblack-21/.cache/tmp/alloc-scan-measure/ev.$N BENCH_ENGINE=nelisp NELISP_PRODUCER_IMAGE_SHA256=918f32f98ae881473f76ad85309ba306c3555caa0f70e5ff2e63cb2e68f77cc7 BENCH_ROW=vector-allocate64 BENCH_N=$N BENCH_HIGH=$N BENCH_WARMUPS=0 TMPDIR=$S XDG_CACHE_HOME=$S/cache NELISP_OWNED_SCRATCH=$S  timeout 1500 gdb -batch -nx -ex 'set pagination off' -ex 'set confirm off' -ex 'source /home/madblack-21/.cache/tmp/alloc-scan-measure/g.py' -ex 'quit' --args /home/madblack-21/.cache/tmp/nelisp-sbcl-impl-builds/build-on27/nelisp-dynamic --load /home/madblack-21/.cache/tmp/alloc-scan-measure/drv/benchmark-driver-native-on.el < /dev/null > /home/madblack-21/.cache/tmp/alloc-scan-measure/gdb.$N.out 2>&1
echo done $? >> /home/madblack-21/.cache/tmp/alloc-scan-measure/gdb.$N.out
