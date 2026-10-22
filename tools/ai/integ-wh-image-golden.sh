#!/usr/bin/env bash
# integ-wh-image-golden.sh [BIN]: dump and restore the symbol-name golden (integ-wh-image-golden.el)
# through two generations on the binary (default target/integ/nelisp-dyn).  Prints one line per
# run and "IMAGE-GOLDEN PASS" or "IMAGE-GOLDEN FAIL"; exit 0 on pass.
cd "$(dirname "$0")/../.." || exit 2
BIN=$(realpath "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
W=${IG_WORK:-$PWD/target/integ-wh/golden}; mkdir -p "$W"; rm -f "$W"/*.img
fail=0
run() {  # run MODE IMAGE_OUT [extra args...]
  local mode=$1 out=$2; shift 2
  (ulimit -v 8388608; IG_MODE=$mode IG_IMAGE=$out LC_ALL=C.UTF-8 timeout 300 "$BIN" "$@" --load tools/ai/integ-wh-image-golden.el > "$W/$mode.out" 2> "$W/$mode.err")
  local rc=$?
  local last; last=$(tail -n 1 "$W/$mode.out")
  echo "$mode rc=$rc stderr=$(wc -c < "$W/$mode.err")B: $last"
  [ $rc = 0 ] && [ ! -s "$W/$mode.err" ] && printf '%s' "$last" | grep -q ' PASS$' || fail=1
}
run dump "$W/g1.img"
run verify "" --cold-load-from "$W/g1.img"
run dump2 "$W/g2.img" --cold-load-from "$W/g1.img"
run verify2 "" --cold-load-from "$W/g2.img"
if [ $fail = 0 ]; then echo "IMAGE-GOLDEN PASS"; else echo "IMAGE-GOLDEN FAIL"; exit 1; fi
