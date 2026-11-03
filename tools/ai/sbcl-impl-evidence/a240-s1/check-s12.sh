#!/bin/bash
# S1.2 E2E: full driver vs cold image + main, 9 rows x {LOW,HIGH}; compare BENCH-CHECK result=.
set -u
. "$(dirname "$0")/common.sh"
OUTTXT=${S12_OUT:-$ROOT/tools/ai/sbcl-impl-evidence/a240-s1/s12-b28.txt}
GNU=${GNU_EMACS_BIN:-/usr/local/bin/emacs-31.1}
FIX=${V8_FIXTURE_MODULE:-$HOME/.cache/tmp/nelisp-v8-native-20261029/out/qualification/fixtures/application.el}
rc=0; : >"$T/table"
printf '%-22s %8s %-8s %-18s %-18s %9s %9s %s\n' row N path full-result image-result full-ms image-ms match >"$T/table"
for ROW in integer-sum function-call2 nested-function-call2 vector-read vector-write vector-allocate64 string-slice16 string-concat16 hash-get1024; do
  case $ROW in
    integer-sum) LOW=4000000 HIGH=16000000 ;;
    vector-read|vector-write|function-call2|nested-function-call2) LOW=1048576 HIGH=4194304 ;;
    vector-allocate64|string-concat16|string-slice16) LOW=262144 HIGH=1048576 ;;
    hash-get1024) LOW=16384 HIGH=65536 ;;
  esac
  X=(); W=0; DRV=$RUNDIR/benchmark-driver-native-on.el
  if [ $ROW = nested-function-call2 ]; then DRV=$RUNDIR/nested-primary-on.el; W=2; X=(GNU_EMACS_BIN="$GNU" V8_FIXTURE_MODULE="$FIX"); fi
  IMG=$T/$ROW.img
  if ! GNU_EMACS_BIN=$GNU V8_FIXTURE_MODULE=$FIX "$PRODUCE" "$BIN" "$DRV" "$ROW" "$IMG" >"$T/p.log" 2>&1; then
    echo "produce failed for $ROW: $(tail -2 "$T/p.log")"; printf '%-22s produce-failed\n' $ROW >>"$T/table"; rc=1; continue; fi
  for N in $LOW $HIGH; do
    E=(BENCH_ROW=$ROW BENCH_N=$N BENCH_HIGH=$HIGH BENCH_WARMUPS=$W "${X[@]}")
    t0=$(date +%s%N); fo=$(run_nelisp "${E[@]}" -- --load "$DRV" 2>&1); frc=$?; t1=$(date +%s%N)
    io=$(run_nelisp "${E[@]}" -- --cold-load-from "$IMG" --load "$IMG.main.el" 2>&1); irc=$?; t2=$(date +%s%N)
    fr=$(echo "$fo" | sed -n 's/^BENCH-CHECK .* result=\([^ ]*\) .*/\1/p'); ir=$(echo "$io" | sed -n 's/^BENCH-CHECK .* result=\([^ ]*\) .*/\1/p')
    m=MATCH; { [ $frc -eq 0 ] && [ $irc -eq 0 ] && [ -n "$fr" ] && [ "$fr" = "$ir" ]; } || { m=MISMATCH; rc=1; }
    printf '%-22s %8s %-8s %-18s %-18s %9d %9d %s\n' $ROW $N full/img "${fr:--}" "${ir:--}" $(((t1-t0)/1000000)) $(((t2-t1)/1000000)) $m >>"$T/table"
  done
  rm -f "$IMG" "$IMG".*
done
{ echo "# S1.2 build28: one run per cell, warmups=0 (nested-function-call2 requires warmups=2)"; cat "$T/table"; } >"$OUTTXT"
cat "$T/table"
[ $rc -eq 0 ] && echo "S1.2 PASS" || echo "S1.2 FAIL"
exit $rc
