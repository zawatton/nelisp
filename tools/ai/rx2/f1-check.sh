#!/bin/sh
# Usage: f1-check.sh BUILD-DYNAMIC-DIR -- prelink closure inside the F1 policy (<= 384 records).
python3 - "$1" <<'PY'
import glob, json, sys
j = json.load(open(glob.glob(sys.argv[1] + '/src/target/standalone-compiler-f1-*/prelink-closure.json')[0]))
assert j['status'] == 'PRELINK_F1_BOUNDARY_PASS' and len(j['records']) <= 384, (j['status'], len(j['records']))
print('F1 records', len(j['records']))
PY
