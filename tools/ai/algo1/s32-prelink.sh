#!/bin/sh
# Prelink closure inside the F1 policy (<= 384 records) and root evidence bound (98,304) for the algo1 build.
cd "$(dirname "$0")/../../.." || exit 1
python3 - <<'PY' || exit 1
import glob, json, re
B = 'target/algo1-build/dynamic'
assert open(B + '/build.log', errors='replace').read().rstrip().splitlines()[-1].startswith('BUILD rc=0')
j = json.load(open(glob.glob(B + '/src/target/standalone-compiler-f1-*/prelink-closure.json')[0]))
assert j['status'] == 'PRELINK_F1_BOUNDARY_PASS' and len(j['records']) <= 384, (j['status'], len(j['records']))
nodes = [int(x) for x in re.findall(r'root-evidence\] nodes=(\d+)', open(B + '/build.log', errors='replace').read())]
assert nodes and max(nodes) <= 98304, nodes
print('prelink', j['status'], len(j['records']), 'root evidence max', max(nodes))
PY
