#!/bin/sh
# S2.1: one dedicated build rc 0, prelink PASS inside the F1 policy (<= 384) and root evidence bound (98,304), and the Windows per-target no-data-blob tests.
cd "$(dirname "$0")/../../.." || exit 1
NB=${NOTES_BIN:-$(d=$PWD; while [ "$d" != / ]; do [ -x "$d/bin/heavy-run.sh" ] && { echo "$d/bin"; break; }; d=$(dirname "$d"); done)}
B=target/bcadopt-build/dynamic
tail -1 $B/build.log | grep -q '^BUILD rc=0' || { echo "build failed"; exit 1; }
python3 - <<'PY' || exit 1
import glob, json, re
j = json.load(open(glob.glob('target/bcadopt-build/dynamic/src/target/standalone-compiler-f1-*/prelink-closure.json')[0]))
assert j['status'] == 'PRELINK_F1_BOUNDARY_PASS' and len(j['records']) <= 384, (j['status'], len(j['records']))
nodes = [int(x) for x in re.findall(r'root-evidence\] nodes=(\d+)', open('target/bcadopt-build/dynamic/build.log', errors='replace').read())]
assert nodes and max(nodes) <= 98304, nodes
print('prelink', j['status'], len(j['records']), 'root evidence max', max(nodes))
PY
(ulimit -v 8388608; timeout 400 $NB/heavy-run.sh /usr/local/bin/emacs --batch -Q -L lisp -L src -L scripts -L test -l ert \
  -l test/nelisp-subr-object-test.el -l test/nelisp-aot-stackmap-test.el \
  --eval '(ert-run-tests-batch-and-exit "w2s-windows-forms-still-carry-no-data-blob\\|smap-table-unit-has-no-data-blob-on-any-target")') \
  > target/bcadopt/logs/rerun-s21-nodatablob.log 2>&1 || { echo "no-data-blob"; exit 1; }
grep -q 'Ran [1-9][0-9]* tests, [1-9][0-9]* results as expected, 0 unexpected' target/bcadopt/logs/rerun-s21-nodatablob.log || exit 1
echo "S2.1 ok"
