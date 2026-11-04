#!/bin/bash
# usage: run.sh NAME [extra env...]
R=$HOME/.cache/tmp/nelisp-sbcl-impl-builds/run; d=$R/s3-guard; name=$1; shift
env NELISP_LANE=on NELISP_LANE_IMAGE=$HOME/.cache/tmp/nelisp-sbcl-impl-builds/build-on/nelisp-dynamic NELISP_LANE_IMAGE_SHA256=09cbbb4f3274ed6f246605a0de35265a4719bf0393e35530a6379290d5cd1a79 NELISP_SCRATCH_ROOT=$R/scratch BENCH_ROW=function-call2 BENCH_N=1048576 BENCH_HIGH=1048576 BENCH_WARMUPS=2 NELISP_GUARD_OUT=$d/out/$name.guard "$@" timeout 300 python3 -I $R/lane_runner_guard.py --load placeholder >$d/$name.out 2>$d/$name.err; echo $? >$d/$name.rc
