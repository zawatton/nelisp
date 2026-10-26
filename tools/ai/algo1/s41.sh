#!/bin/sh
# S4 gates on target/algo1/timing/out/*/report.json: regression rows within 3 percent of v17,
# fixed rows at most 0.65 of v17.
cd "$(dirname "$0")/../../.." || exit 1
python3 - <<'PY' || exit 1
import json
def ratio(i):
    e = json.load(open(f'target/algo1/timing/out/{i}/report.json'))['engines']
    return e['algo1']['inner_ns_per_step']['median'] / e['v17']['inner_ns_per_step']['median']
bad = []
for i in ('reg-sum-bc', 'r3-mix'):
    if ratio(i) > 1.03: bad.append((i, ratio(i)))
for i in ('hl-seq-uniq-1k', 'hl-cl-union-500', 'enc-json-parse-nums', 'enc-json-parse-hash', 'enc-json-parse-alist',
          'enc-json-serialize-alist', 'enc-json-serialize-hash', 'enc-jsonel-read', 'enc-jsonel-encode', 'rp-prin1-strs-esc', 'rp-read-floats'):
    if ratio(i) > 0.65: bad.append((i, ratio(i)))
print('bad', bad)
raise SystemExit(1 if bad else 0)
PY
