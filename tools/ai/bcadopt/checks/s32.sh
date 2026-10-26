#!/bin/sh
# S3.2: reg-sum-bc, r3-mix, r3-call cand not slower than base beyond noise (cand ns/step <= base max of 5 paired reps); substrate-presence ratchet green.
cd "$(dirname "$0")/../../.." || exit 1
python3 - <<'PY' || exit 1
import re, sys
bad = []
for id_ in ('reg-sum-bc', 'r3-mix', 'r3-call'):
    t = open('target/bcadopt/bench/%s.log' % id_).read()
    rows = {m[0]: m[1:] for m in re.findall(r'^(base|cand|gnu)\s+([0-9.]+)\s+([0-9.]+)\s+([0-9.]+)', t, re.M)}
    if float(rows['cand'][0]) > float(rows['base'][2]): bad.append(id_)
    print(id_, 'base', rows['base'][0], 'cand', rows['cand'][0])
sys.exit(1 if bad else 0)
PY
grep -q 'substrate-presence-corpus-check\] PASS' target/codex/substrate-presence.log
