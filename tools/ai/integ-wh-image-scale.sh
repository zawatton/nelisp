#!/usr/bin/env bash
# integ-wh-image-scale.sh [BIN]: the symbols lane's at-scale image check (gc-check.el, copied as
# integ-wh-gc-check.el) on a generated image: 21,000 interned symbols with values, functions,
# plists and strings are dumped, restored, collected and churned, a second generation is dumped
# and restored.  The lane's own subject was a 24,516-symbol C-core library image; this one is
# generated, because that image's bundle revision is not in this repository.
# Prints one line per run and "IMAGE-SCALE PASS" or "IMAGE-SCALE FAIL"; exit 0 on pass.
cd "$(dirname "$0")/../.." || exit 2
BIN=$(realpath "${1:-${INTEG_WH_DYN:-target/integ/nelisp-dyn}}") || exit 2
W=${IG_WORK:-$PWD/target/integ-wh/scale}; mkdir -p "$W"; rm -f "$W"/*.img
fail=0
gen='(progn
  (dotimes (i 21000)
    (let ((s (intern (format "scale-sym-%d" i))))
      (cond ((= (% i 3) 0) (set s i))
            ((= (% i 3) 1) (set s (format "value-of-%d" i)))
            (t (set s (list i (make-string 5 ?q) (vector i i)))))
      (put s (quote scale-prop) (list i (format "plist-%d" i)))
      (when (= (% i 4) 0) (fset s (lambda (x) (+ x 1))))))
  (garbage-collect)
  (unless (> (nelisp--arena-dump-image-stream "'"$W"'/s1.img") 0) (error "dump failed"))
  (princ "SCALE-DUMPED\n"))'
run() {  # run LABEL FILE_FOR_STDOUT ARGS...
  local label=$1; shift
  local t0; t0=$(date +%s.%N)
  (ulimit -v 8388608; timeout 900 "$@" > "$W/$label.out" 2> "$W/$label.err")
  local rc=$?
  local secs; secs=$(LC_ALL=C awk -v a="$t0" -v b="$(date +%s.%N)" 'BEGIN { printf "%.2f", b - a }')
  # --load echoes the form's value after the script's own output: a lone quote closes it.
  local last; last=$(grep -v '^"*$' "$W/$label.out" | tail -n 1)
  echo "$label rc=$rc stderr=$(wc -c < "$W/$label.err")B ${secs}s: $last"
  [ $rc = 0 ] && [ ! -s "$W/$label.err" ] || fail=1
  printf '%s' "$last" | grep -q 'PASS\|SCALE-DUMPED' || fail=1
}
run generate "$BIN" --eval "$gen"
run check env ICHK_MODE=check "$BIN" --cold-load-from "$W/s1.img" --load tools/ai/integ-wh-gc-check.el
run dump2 env ICHK_MODE=dump ICHK_IMAGE="$W/s2.img" "$BIN" --cold-load-from "$W/s1.img" --load tools/ai/integ-wh-gc-check.el
run verify env ICHK_MODE=verify "$BIN" --cold-load-from "$W/s2.img" --load tools/ai/integ-wh-gc-check.el
if [ $fail = 0 ]; then echo "IMAGE-SCALE PASS"; else echo "IMAGE-SCALE FAIL"; exit 1; fi
