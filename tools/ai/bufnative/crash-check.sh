#!/bin/bash
# usage: crash-check.sh BIN ; exit 0 iff the repro prints 12-style point (a number) and exits 0
D=$(dirname "$(readlink -f "$0")")
out=$( (ulimit -v 8388608; timeout 60 "$1" --load $D/crash-repro.el -- 2>&1) ); rc=$?
echo "$out"; echo "rc=$rc"; [ $rc -eq 0 ] && echo "$out" | grep -qE '^[0-9]+$'
