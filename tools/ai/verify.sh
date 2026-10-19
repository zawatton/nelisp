#!/bin/bash
# Final verification for the selected Doc 215 binaries.
cd "$(dirname "$0")/../.." || exit 2
O=target/abi-p2; D=${ABI_BIN:?}; S=${ABI_STATIC_BIN:?}; V=${ABI_VERIFY_DIR:-$O/resume-final}
BASE=${ABI_BASE_BIN:?}
mkdir -p $V; R=$V/summary.txt
echo "start $(date +%T) load: $(cut -d' ' -f1-3 /proc/loadavg)" > $R
# a
EVAL_BIN=$D sh test/perf/standalone-checks.sh > $V/a-standalone-checks.log 2>&1
echo "a rc=$? FAIL=$(grep -c 'STANDALONE-CHECK FAIL' $V/a-standalone-checks.log) known-red=$(grep -c 'known-red' $V/a-standalone-checks.log) last: $(tail -1 $V/a-standalone-checks.log) $(date +%T)" >> $R
grep 'STANDALONE-CHECK FAIL\|known-red' $V/a-standalone-checks.log | sed 's/^/a   /' >> $R
# b
for f in test/standalone-shallow-binding-check.el test/standalone-shallow-binding-dyn-check.el test/standalone-fx-forward-macro-check.el test/standalone-fx-late-special-check.el test/perf/fx-spike-check.el test/standalone-prn-string-escape-check.el test/standalone-cl-loop-gnu-check.el test/standalone-bytecode-word-check.el test/standalone-fx-call-shape-check.el test/standalone-abi-fx-result-word-check.el test/standalone-abi-char-table-word-check.el test/standalone-abi-reader-control-word-check.el; do
  lg=$V/b-$(basename $f .el).log
  timeout 600 nice -n 10 $D --load $f > $lg 2>&1; rc=$?
  echo "b $f rc=$rc last: $(tail -1 $lg | cut -c1-150)" >> $R
done
timeout 300 emacs --batch -l test/standalone-cl-loop-gnu-check.el > $V/b-cl-loop-gnu.emacs.out 2>/dev/null
if diff $V/b-standalone-cl-loop-gnu-check.log $V/b-cl-loop-gnu.emacs.out > $V/b-cl-loop-gnu.diff; then echo "b cl-loop-gnu diff vs GNU: identical" >> $R; else echo "b cl-loop-gnu diff vs GNU: $(wc -l < $V/b-cl-loop-gnu.diff) diff lines" >> $R; fi
python3 test/abi-n4-reference-test.py "$D" > "$V/b-n4-reference.log" 2>&1
echo "b n4-reference rc=$? last: $(tail -1 "$V/b-n4-reference.log")" >> "$R"
python3 test/abi-char-table-legacy-test.py "$D" > "$V/b-char-table-legacy.log" 2>&1
echo "b char-table-legacy rc=$? last: $(tail -1 "$V/b-char-table-legacy.log")" >> "$R"
python3 test/abi-char-table-image-test.py "$D" > "$V/b-char-table-image.log" 2>&1
echo "b char-table-image rc=$? last: $(tail -1 "$V/b-char-table-image.log")" >> "$R"
python3 test/abi-value-clone-test.py "$D" > "$V/b-value-clone.log" 2>&1
echo "b value-clone rc=$? last: $(tail -1 "$V/b-value-clone.log")" >> "$R"
# c
for n in standalone allocating-standalone percpu-roots mirror-guard; do
  f=tools/nelisp-thread-$n-smoke.el; lg=$V/c-$n.log
  timeout 600 nice -n 10 $S --load $f > $lg 2>&1; rc=$?
  echo "c $f rc=$rc pass=$(grep -c PASS $lg) fail=$(grep -c FAIL $lg)" >> $R
done
# d
python3 tools/ai/abi-anvil-smoke.py "$D" "$V/anvil" > $V/d-anvil.log 2>&1
echo "d rc=$?" >> $R; cut -c1-160 $V/d-anvil.log | sed 's/^/d   /' >> $R
# e: F1 prelink proof (build-time certificate of the dynamic build) + runtime check
P=$(ls -td target/standalone-compiler-f1-* | head -1)/prelink-closure.json
python3 -c "
import json;d=json.load(open('$P'));print('e F1 build proof', d['status'], 'records', len(d['records']), 'boundaries', [r['name'] for r in d['records'] if r.get('evaluator_boundary')])" >> $R 2>&1
for b in $D $S; do
  echo "e F1 runtime $b: $(timeout 120 $b --eval "(progn (nelisp-native-startup-ensure) (princ (list (featurep 'nelisp-native-compiler-f1-runtime-proof) (featurep 'nelisp-native-compiler-runtime-capability))))" 2>&1 | tail -1 | cut -c1-80)" >> $R
done
# comparison vs baseline, alternating on the same machine
echo "cmp load: $(cut -d' ' -f1-3 /proc/loadavg)" >> $R
for i in 1 2 3; do
  for b in $BASE $D; do
    s=$(date +%s.%N); $b --eval nil >/dev/null 2>&1; e=$(date +%s.%N)
    echo "cmp boot $i $(basename $b) $(echo "$e - $s" | bc)" >> $R
  done
done
for i in 1 2; do
  for b in $BASE $D; do
    timeout 900 nice -n 10 $b --load test/perf/shallow-binding-bench.el > $V/cmp-bench-$i-$(basename $b).log 2>&1
    echo "cmp bench $i $(basename $b) rc=$? $(grep -i regex $V/cmp-bench-$i-$(basename $b).log | head -1)" >> $R
  done
  for b in $BASE $D; do
    timeout 600 nice -n 10 $b --load test/perf/fx-spike-check.el > $V/cmp-fx-$i-$(basename $b).log 2>&1
    echo "cmp fx-spike $i $(basename $b) rc=$? $(tail -1 $V/cmp-fx-$i-$(basename $b).log | cut -c1-60)" >> $R
  done
done
echo "cmp load end: $(cut -d' ' -f1-3 /proc/loadavg)" >> $R
python3 tools/ai/box-census.py summary "${ABI_CENSUS:?}" > $V/e-census.txt 2>&1
echo "census: $(head -3 $V/e-census.txt | tr '\n' ' ' | cut -c1-200)" >> $R
echo "end $(date +%T)" >> $R

python3 tools/ai/abi-verify-summary.py "$V"
