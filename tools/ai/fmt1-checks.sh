#!/bin/sh
# Lane fmt1 acceptance checks.  Usage: sh tools/ai/fmt1-checks.sh s11|s12|s21|s22
# FMT1_BIN overrides the candidate binary.  Run from anywhere; cwd becomes the repo root.
cd "$(dirname "$0")/../.." || exit 1
BIN=${FMT1_BIN:-target/fmt1-build-b2/dynamic/nelisp-dynamic}
case "$1" in
s11) # native engine answers width/flags/%S/aggregates; speed guard 100 us/op; red on the seed binary (v14)
  grep -q 'FMT1 OK n=19' target/fmt1/logs/green-b2-native-check.log || exit 1
  grep -q '^rc=1' target/fmt1/logs/red-v14-native-check.log || exit 1
  (ulimit -v 8388608; taskset -c 12 timeout 300 "$BIN" --load tools/ai/fmt1-native-check.el -- | grep -q 'FMT1 OK n=19') || exit 1 ;;
s12) # candidate corpus differences from GNU are exactly the seed's differences
  python3 - <<'PY' || exit 1
import re
def diffs(p):
    return re.findall(r'^DIFF .*\n\s+emacs .*\n\s+nelisp .*', open(p, errors='replace').read(), re.M)
a = diffs('target/fmt1/corpus/v14.out'); b = diffs('target/fmt1/corpus/b2.out')
assert a and a == b, (len(a), len(b))
print('corpus: 164 cases, %d pre-existing GNU differences, identical on seed and candidate' % len(b))
PY
  grep -q '^probe: 164 case' target/fmt1/corpus/b2.out || exit 1 ;;
s21) # one fresh full build, prelink inside F1 policy and root evidence bound, no data-blob (host ERT)
  tail -1 target/fmt1-build-b2/dynamic/build.log | grep -q '^BUILD rc=0' || exit 1
  python3 - <<'PY' || exit 1
import glob, json, re
j = json.load(open(glob.glob('target/fmt1-build-b2/dynamic/src/target/standalone-compiler-f1-*/prelink-closure.json')[0]))
assert j['status'] == 'PRELINK_F1_BOUNDARY_PASS' and len(j['records']) <= 384
for p in ('compiler-constructor', 'rooted-protocol'):
    k = json.load(open(glob.glob('target/fmt1-build-b2/dynamic/src/target/standalone-%s-*/prelink-closure.json' % p)[0]))
    assert k['status'] == 'PRELINK_DIRECT_CLOSURE_PASS', p
n = [int(x) for x in re.findall(r'root-evidence\] nodes=(\d+)', open('target/fmt1-build-b2/dynamic/build.log', errors='replace').read())]
assert n and max(n) <= 98304
print('prelink ok', len(j['records']), 'root evidence max', max(n))
PY
  grep -q 'Ran 2 tests, 2 results as expected, 0 unexpected' target/fmt1/logs/nodatablob-early.log || exit 1 ;;
s22)
  python3 - <<'PY' || exit 1
import json
d = json.load(open('target/fmt1/fx-b2/report.json')); c = d['checks']
assert d['status'] == 'EXPECTED_BASELINE_RED_REMAINS' and d['regression'] is False
assert c['weak24']['passed'] and c['weak24']['checks'] == 24
assert c['gnu56']['passed'] and c['gnu56']['checks'] == 56
assert c['afrt25']['expected_baseline_red'] and c['afrt25']['failures'] == ['exit-hook-result-gc']
assert c['buffer21']['passed'] and c['buffer21']['checks'] == 21
PY
  grep -q 'closurevm: PASS (24)' target/fmt1/logs/wincheck-linux.log && grep -q 'switch corpus: PASS' target/fmt1/logs/wincheck-linux.log && grep -q 'features: PASS' target/fmt1/logs/wincheck-linux.log && grep -q '^rc=0' target/fmt1/logs/wincheck-linux.log || exit 1 ;;
*) echo "usage: $0 s11|s12|s21|s22"; exit 2 ;;
esac
echo "$1 ok"
