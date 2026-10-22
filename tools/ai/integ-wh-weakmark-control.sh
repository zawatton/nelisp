#!/usr/bin/env bash
# integ-wh-weakmark-control.sh UNFIXED_BIN [FIXED_BIN]: the negative control of fix 20.
#
# test/standalone-weak-symbol-mark-check.el must fail on a binary built without the fix and pass on
# one built with it.  UNFIXED_BIN is a dynamic binary built from the revision before the fix
# (BUILD_OUT=target/integ/nelisp-dyn-unfixed bash tools/ai/integ-wh-build.sh <fix 20 commit>^);
# FIXED_BIN defaults to target/integ/nelisp-dyn.  Each binary runs the check as built (with its
# cold image) and as a copy without the image (a fresh-prelude boot), and the copy dumps an image
# in-process four times, twice under `setarch -R' (no address randomization).  Prints one line per
# run and ends with WEAKMARK-CONTROL OK iff the unfixed binary fails every check run and the fixed
# one passes every run.  The output of a run is recorded in tools/ai/integ-wh-weakmark-control.txt
# (S2.16); rerun it to refresh the record.
cd "$(dirname "$0")/../.." || exit 2
ROOT=$PWD
UNFIXED=${1:?usage: $0 UNFIXED_BIN [FIXED_BIN]}
FIXED=${2:-$ROOT/target/integ/nelisp-dyn}
OUT=$ROOT/target/integ-wh/weakmark-control; rm -rf "$OUT"; mkdir -p "$OUT"
ok=1

copy_without_cold() {  # copy_without_cold BIN DIR
  mkdir -p "$2"; cp -p "$1" "$2/nelisp" || return 1
  for e in .buffer-local-bytecode.tsv .native-startup.el .variable-alias-bytecode.tsv .version.json; do
    [ -f "$1$e" ] && cp -p "$1$e" "$2/nelisp$e"; done
}

run_check() {  # run_check LABEL EXPECT BIN: EXPECT is pass or fail
  local label=$1 expect=$2 bin=$3 rc line
  { (ulimit -v 8388608; timeout 300 "$bin" --load test/standalone-weak-symbol-mark-check.el > "$OUT/run.out" 2>&1); rc=$?; } 2>/dev/null
  line=$(grep '^WEAK-SYMBOL-MARK symbols' "$OUT/run.out")
  if [ -z "$line" ]; then
    [ "$rc" != 0 ] || rc=1
    line="no result line (exit status $rc$([ "$rc" -gt 128 ] && echo ", signal $((rc - 128))"))"
  else
    case $line in *PASS) rc=0 ;; *) rc=1 ;; esac
  fi
  printf '%-40s %-5s expected %s: %s\n' "$label" "$([ "$rc" = 0 ] && echo PASS || echo FAIL)" "$expect" "$line"
  if [ "$expect" = pass ]; then [ "$rc" = 0 ] || ok=0; else [ "$rc" != 0 ] || ok=0; fi
}

run_dump() {  # run_dump LABEL EXPECT BIN WRAPPER
  local label=$1 expect=$2 bin=$3 wrap=$4 got rc img=$OUT/dump.img
  rm -f "$img"
  { got=$( (ulimit -v 8388608; exec $wrap timeout 300 "$bin" --eval "(progn (princ (symbol-name 'eq)) (princ (nelisp--arena-dump-image-stream \"$img\")))") 2>&1 ); rc=$?; } 2>/dev/null
  case $got in eq[0-9]*) rc=0 ;; *) [ "$rc" != 0 ] || rc=1 ;; esac
  printf '%-40s %-5s expected %s: rc=%s, %s\n' "$label" "$([ "$rc" = 0 ] && echo PASS || echo FAIL)" "$expect" "$rc" \
    "$(printf %s "$got" | head -c 60 | tr '\n' ' ')"
  if [ "$expect" = pass ]; then [ "$rc" = 0 ] || ok=0; else [ "$rc" != 0 ] || ok=0; fi
  rm -f "$img"
}

for pair in "unfixed:$UNFIXED:fail" "fixed:$FIXED:pass"; do
  label=${pair%%:*}; rest=${pair#*:}; bin=${rest%:*}; expect=${rest##*:}
  [ -x "$bin" ] || { echo "missing $label binary $bin"; exit 2; }
  echo "$label binary: $("$bin" --version 2>/dev/null | head -1), sha256 $(sha256sum "$bin" | cut -c1-16)"
  copy_without_cold "$bin" "$OUT/$label" || exit 2
  run_check "$label, with the cold image" "$expect" "$bin"
  run_check "$label, without the cold image" "$expect" "$OUT/$label/nelisp"
  # A dump from a fresh boot dies or damages the image only for some environment sizes and
  # layouts, so an unfixed binary is not required to fail these; a fixed one must pass all.
  for i in 1 2 3 4; do
    if [ "$i" -gt 2 ] && command -v setarch > /dev/null; then w="setarch $(uname -m) -R"; else w=; fi
    if [ "$expect" = pass ]; then
      run_dump "$label, fresh boot and dump $i${w:+ (no ASLR)}" pass "$OUT/$label/nelisp" "$w"
    else
      keep=$ok; run_dump "$label, fresh boot and dump $i${w:+ (no ASLR)}" "any result" "$OUT/$label/nelisp" "$w"; ok=$keep
    fi
  done
done
[ $ok = 1 ] && echo "WEAKMARK-CONTROL OK" || echo "WEAKMARK-CONTROL FAIL"
[ $ok = 1 ]
